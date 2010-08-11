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

C **************** ARROW ****************
C ARROW draws an arrow between two points with a directional head.
C The arrow goes from P to Q, and the arrow point is centered round 
C coordinane A. al is arrow length, aw is arrow width. 
C If act = 'r' then arrow size relative to length of line, 
C if act = 'a' then absolute dimensions. 
C Returns the corner points of the arrow as pixel locations in 'ipoints'.
C IDRAW=0 don't draw; =1 shaft single line thickness; =2 shaft double line thickness.

      SUBROUTINE ARROW(P,Q,al,aw,ipoints,act,IDRAW)
#include "building.h"
#include "prj3dv.h"

C      COMMON/OUTIN/IUOUT,IUIN
      real P,Q,SNORM,PLANE,CORNER
      DIMENSION P(3),Q(3),SNORM(3),A(3),PLANE(4),CORNER(4,3)
      real VE,VECC,COG1,COG2,COG3
      integer ipoints
      DIMENSION VE(3),VECC(3),COG1(3),COG2(3),COG3(3),ipoints(6,2)
      character act*1
C     character outs*124
      logical unixok

C Local variables to pass to edline.
#ifdef OSI
      integer iixp,iiyp,iixq,iiyq,iixa,iiya
      integer iixc1,iiyc1,iixc2,iiyc2
      integer iixc3,iiyc3,iixc4,iiyc4
#else
      integer*8 iixp,iiyp,iixq,iiyq,iixa,iiya
      integer*8 iixc1,iiyc1,iixc2,iiyc2
      integer*8 iixc3,iiyc3,iixc4,iiyc4
#endif     

C Calculate the vector between P and Q.
      do 10 I=1,3
        SNORM(I)=Q(I)-P(I)
 10   continue

C Calculate the location of point A.  If relative, scale snorm.
C If absolute distance back from Q, find whole length and then
C work backwards using ratio. If P and Q are the same then
C DIST is going to be zero and in that case and the case of
C a very small DIST just return without attempting to draw it.
      DIST= CROW(P,Q)
      if(DIST.lt.0.0001) return   ! if zero length line.
      if(act(1:1).eq.'r')then
        do 20 I=1,3
          A(I)=P(I)+((1.0-al)*SNORM(I))
 20     continue
      else if(act(1:1).eq.'a')then
        r2 = DIST - al
        r1 = al
        a(1) = ((r2 * q(1)) + (r1 * p(1)))/DIST
        a(2) = ((r2 * q(2)) + (r1 * p(2)))/DIST
        a(3) = ((r2 * q(3)) + (r1 * p(3)))/DIST
      endif

C debug...
C      write(outs,'(a,4f10.4)')'Point is at X,Y,Z:',a(1),a(2),a(3),DIST
C      call edisp(iuout,outs)

C Generate the coefficients for the plane through A with normal NORM.
      PLANE(1)=SNORM(1)
      PLANE(2)=SNORM(2)
      PLANE(3)=SNORM(3)
      PLANE(4)=-1.*((SNORM(1)*A(1))+(SNORM(2)*A(2))+(SNORM(3)*A(3)))

C Find axis which is 'most perpendicular' to plane and the
C smallest coefficient in the vector NORM.
      AXLEN=0.
      IAXIS=0
      do 40 J=1,3
        XX=abs(snorm(j))
        if (XX.gt.AXLEN) then
          AXLEN=XX
          IAXIS=J
        endif
 40   continue
      if (IAXIS.eq.0) goto 9999

C Find first point on the plane, CORNER(1,*).  First set all coords to 
C zero then calculate where the chosen axis intersects.
      CORNER(1,1)=0.
      CORNER(1,2)=0.
      CORNER(1,3)=0.
      CORNER(1,IAXIS)=-1.*PLANE(4)/PLANE(IAXIS)

C Calculate the direction vector from A to the first corner.
      do 50 I=1,3
        VE(I)=CORNER(1,I)-A(I)
 50   continue

C Scale vector and move CORNER(1,*)
      DISTVEC=0.
      do 60 I=1,3
        DISTVEC=DISTVEC+VE(I)*VE(I)
 60   continue
      DISTVEC=SQRT(DISTVEC)
      if(DISTVEC.lt.0.0001) return   ! if zero length line.
      if(act(1:1).eq.'r')then
        SF=(DIST*aw)/DISTVEC
      elseif(act(1:1).eq.'a')then
        SF=(aw)/DISTVEC
      endif
      do 70 I=1,3
        CORNER(1,I)=A(I)+(VE(I)*SF)

C Corner 3 is a 180deg rotation.
        CORNER(3,I)=A(I)-(VE(I)*SF)
 70   continue

C Calculate cross product to find corners 2 and 4.
      VECC(1)=0.0
      VECC(2)=0.0
      VECC(3)=0.0
      VECC(1)=SNORM(2)*VE(3)-SNORM(3)*VE(2)
      VECC(2)=SNORM(3)*VE(1)-SNORM(1)*VE(3)
      VECC(3)=SNORM(1)*VE(2)-SNORM(2)*VE(1)
      DISTVEC=0.
      do 61 I=1,3
        DISTVEC=DISTVEC+VECC(I)*VECC(I)
 61   continue
      DISTVEC=SQRT(DISTVEC)
      if(DISTVEC.lt.0.0001) return   ! if zero length line.
      if(act(1:1).eq.'r')then
        SF=(DIST*aw)/DISTVEC
      elseif(act(1:1).eq.'a')then
        SF=(aw)/DISTVEC
      endif
      do 71 I=1,3
        CORNER(2,I)=A(I)+(VECC(I)*SF)

C Corner 3 is a 180deg rotation.
        CORNER(4,I)=A(I)-(VECC(I)*SF)
 71   continue

C Debug.
C      write(outs,*) 'Pln eq ',Plane
C      call edisp(iuout,outs)
C      write(outs,*) 'Axlen  & dist & SF iaxis ',AXLEN,DIST,SF,IAXIS
C      call edisp(iuout,outs)

C Draw the damn thing!
      CALL VECTRN(P,TEMAT,COG2,IER)
      CALL VECTRN(COG2,ETSMAT,COG3,IER)
      call u2pixel(COG3(1),COG3(2),iixp,iiyp)
      CALL VECTRN(Q,TEMAT,COG2,IER)
      CALL VECTRN(COG2,ETSMAT,COG3,IER)
      call u2pixel(COG3(1),COG3(2),iixq,iiyq)
      CALL VECTRN(A,TEMAT,COG2,IER)
      CALL VECTRN(COG2,ETSMAT,COG3,IER)
      call u2pixel(COG3(1),COG3(2),iixa,iiya)
      COG1(1)=corner(1,1)
      COG1(2)=corner(1,2)
      COG1(3)=corner(1,3)
      CALL VECTRN(COG1,TEMAT,COG2,IER)
      CALL VECTRN(COG2,ETSMAT,COG3,IER)
      call u2pixel(COG3(1),COG3(2),iixc1,iiyc1)
      COG1(1)=corner(2,1)
      COG1(2)=corner(2,2)
      COG1(3)=corner(2,3)
      CALL VECTRN(COG1,TEMAT,COG2,IER)
      CALL VECTRN(COG2,ETSMAT,COG3,IER)
      call u2pixel(COG3(1),COG3(2),iixc2,iiyc2)
      COG1(1)=corner(3,1)
      COG1(2)=corner(3,2)
      COG1(3)=corner(3,3)
      CALL VECTRN(COG1,TEMAT,COG2,IER)
      CALL VECTRN(COG2,ETSMAT,COG3,IER)
      call u2pixel(COG3(1),COG3(2),iixc3,iiyc3)
      COG1(1)=corner(4,1)
      COG1(2)=corner(4,2)
      COG1(3)=corner(4,3)
      CALL VECTRN(COG1,TEMAT,COG2,IER)
      CALL VECTRN(COG2,ETSMAT,COG3,IER)
      call u2pixel(COG3(1),COG3(2),iixc4,iiyc4)

C Draw the arrow. (IDRAW determines shaft width).
      if (IDRAW.gt.0) then
        if (IDRAW.eq.1) call eswline(iixp,iiyp,iixq,iiyq)
        if (IDRAW.eq.2) call edwline(iixp,iiyp,iixq,iiyq)
        call eswline(iixc1,iiyc1,iixc2,iiyc2)
        call eswline(iixc2,iiyc2,iixc3,iiyc3)
        call eswline(iixc3,iiyc3,iixc4,iiyc4)
        call eswline(iixc4,iiyc4,iixc1,iiyc1)
        call eswline(iixc1,iiyc1,iixq,iiyq)
        call eswline(iixc2,iiyc2,iixq,iiyq)
        call eswline(iixc3,iiyc3,iixq,iiyq)
        call eswline(iixc4,iiyc4,iixq,iiyq)
      endif
      call forceflush()

C Copy pixel locations to ipoints.
      ipoints(1,1)=int(iixp)
      ipoints(1,2)=int(iiyp)
      ipoints(2,1)=int(iixq)
      ipoints(2,2)=int(iiyq)
      ipoints(3,1)=int(iixc1)
      ipoints(3,2)=int(iiyc1)
      ipoints(4,1)=int(iixc2)
      ipoints(4,2)=int(iiyc2)
      ipoints(5,1)=int(iixc3)
      ipoints(5,2)=int(iiyc3)
      ipoints(6,1)=int(iixc4)
      ipoints(6,2)=int(iiyc4)

      RETURN

 9999 call isunix(unixok)
      if(unixok) write(6,*) 
     &  'Error: cannot find smallest coefficient in vector.'
      return
      end
