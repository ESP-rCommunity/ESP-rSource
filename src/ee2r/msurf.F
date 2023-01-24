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

C Contains routines to split surfaces into many polygons and grid 
C for daylightfactors etc.
C  CHECKREC   Taken from esrumrt/checkrec.F and reduced to only 
C             check if a surface is rectangular.  
C  
C  PARAMET    Generate parametric equations for a rectangular surface.
C  


C ********************* CHECKREC *********************
C  CHECKREC   Taken from esrumrt/checkrec.F and reduced to only 
C             check if a surface is rectangular.  
C IRECT: -1 not rectangular; 0 error; 1 rectangular.

      subroutine checkrec(ISURF,IRECT)
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

c  prod       inproduct of vectors (0 if perpendicular,
c             1 if parallel, -1 if anti-parallel)

      real prod,length,vec1(3),vec2(3)
      integer zeros
      character outs*124

c   Check if surface is rectangular.
c    nver(i)        Number of vertices in i
c    jvn(i,j)       vertex-index of surface i
c    x(jvn(i,j))
c    y(jvn(i,j))   coordinates of vertices
c    z(jvn(i,j))
c   We trace around the surface from vertex to vertex.
c   We take the inproduct of two subsequent normalised
c   direction vectors. If the surface is rectangular
c   the inproduct (prod) should be 4 times 0 and all other times
c   equal to 1. If this is not the case the surface is not
c   rectangular.
C Do only ISURF.
      IRECT=0
      zeros = 0
      vec1(1)=x(jvn(ISURF,1))-x(jvn(ISURF,nver(ISURF)))
      vec1(2)=y(jvn(ISURF,1))-y(jvn(ISURF,nver(ISURF)))
      vec1(3)=z(jvn(ISURF,1))-z(jvn(ISURF,nver(ISURF)))
      length=(vec1(1)**2+vec1(2)**2+vec1(3)**2)**0.5
      vec1(1)=vec1(1)/length
      vec1(2)=vec1(2)/length
      vec1(3)=vec1(3)/length
      do 50 j=1,nver(ISURF)
        jj=j+1
        if (jj.gt.nver(ISURF)) then
        jj=1
        endif
        vec2(1)=x(jvn(ISURF,jj))-x(jvn(ISURF,j))
        vec2(2)=y(jvn(ISURF,jj))-y(jvn(ISURF,j))
        vec2(3)=z(jvn(ISURF,jj))-z(jvn(ISURF,j))
        length=(vec2(1)**2+vec2(2)**2+vec2(3)**2)**0.5
        vec2(1)=vec2(1)/length
        vec2(2)=vec2(2)/length
        vec2(3)=vec2(3)/length
        prod=vec1(1)*vec2(1)+vec1(2)*vec2(2)+vec1(3)*vec2(3)
        vec1(1)=vec2(1)
        vec1(2)=vec2(2)
        vec1(3)=vec2(3)
        if (abs(prod).lt.0.00001) then
          zeros=zeros+1
        elseif (abs(prod-1.).gt.0.00001) then
          IRECT=-1
        endif
50    continue
      if (zeros.ne.4) then
        IRECT=-1
      else
        IRECT=1
      endif

C Check to see if IRECT has been set.
      if (IRECT.eq.0) then
        write (outs,'(a,i4)') 
     &                    'Could not determine shape of surface ',ISURF
        call edisp(iuout,outs)
      endif

      return
      end

C ********************* PARAMET *********************
C  PARAMET    Generate parametric equations for a rectangular surface.
C             surf = x(S,T) + y(S,T) + z(S,T)

      subroutine PARAMET(ISURF,SVALS,TVALS,TFORM,IR)
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      DIMENSION SVALS(3), TVALS(3), vec1(3), vec2(3), tform(3)

      character outs*124

      IR=0
      do 5 I=1,3
        SVALS(I)=0.
        TVALS(I)=0.
        TFORM(I)=0.
 5    continue

C Find minimum and maximum corners (opposites in the bounding box (plane)
      IMAX=1
      IMIN=1
      do 10 IVERT=2,NVER(ISURF)
        if (Z(jvn(ISURF,IVERT)).gt.Z(jvn(ISURF,IMAX))) then
          IMAX=IVERT
        elseif (Z(jvn(ISURF,IVERT)).lt.Z(jvn(ISURF,IMIN))) then
          IMIN=IVERT
        endif
 10   continue
      do 20 IVERT=2,NVER(ISURF)
        if (abs(Z(jvn(ISURF,IVERT))-Z(jvn(ISURF,IMAX))).lt.0.001) then
          if (Y(jvn(ISURF,IVERT)).gt.Y(jvn(ISURF,IMAX))) then
            IMAX=IVERT
          endif
        elseif(abs(Z(jvn(ISURF,IVERT))-Z(jvn(ISURF,IMIN))).lt.0.001)then
          if (Y(jvn(ISURF,IVERT)).lt.Y(jvn(ISURF,IMIN))) then
            IMIN=IVERT
          endif
        endif
 20   continue
      do 30 IVERT=2,NVER(ISURF)
        if (abs(Z(jvn(ISURF,IVERT))-Z(jvn(ISURF,IMAX))).lt.0.001) then
          if (abs(Y(jvn(ISURF,IVERT))-Y(jvn(ISURF,IMAX))).lt.0.001)then
            if (X(jvn(ISURF,IVERT)).gt.X(jvn(ISURF,IMAX))) then
              IMAX=IVERT
            endif
          endif
        elseif(abs(Z(jvn(ISURF,IVERT))-Z(jvn(ISURF,IMIN))).lt.0.001)then
          if (abs(Y(jvn(ISURF,IVERT))-Y(jvn(ISURF,IMIN))).lt.0.001)then
            if (X(jvn(ISURF,IVERT)).lt.X(jvn(ISURF,IMIN))) then
              IMIN=IVERT
            endif
          endif
        endif
 30   continue

C Debug.
C      write(6,*) 'min vert, max vert (no in surf) : ',IMIN,IMAX
C      write(6,*) 'Min vert coords: ',X(jvn(ISURF,IMIN)), 
C     &                            Y(jvn(ISURF,IMIN)), Z(jvn(ISURF,IMIN))
C      write(6,*) 'Max vert coords: ',X(jvn(ISURF,IMAX)), 
C     &                            Y(jvn(ISURF,IMAX)), Z(jvn(ISURF,IMAX))

C Check to see that IMAX and IMIN are not the same.
      if (IMAX.eq.IMIN) then
        write (outs,'(a,i4)') 
     &          'Could not find max and min vertices in surface ',ISURF
        call edisp(iuout,outs)
        IR=1
        return
      endif

C Find one of the two vertices which forms a right triangle with 
C vertices IMAX and IMIN.
      if (IMAX.gt.IMIN) then
        do 40 IVERT=IMIN+1,IMAX-1
          vec1(1)=x(jvn(ISURF,IVERT))-x(jvn(ISURF,IMIN))
          vec1(2)=y(jvn(ISURF,IVERT))-y(jvn(ISURF,IMIN))
          vec1(3)=z(jvn(ISURF,IVERT))-z(jvn(ISURF,IMIN))
          vec2(1)=x(jvn(ISURF,IMAX))-x(jvn(ISURF,IVERT))
          vec2(2)=y(jvn(ISURF,IMAX))-y(jvn(ISURF,IVERT))
          vec2(3)=z(jvn(ISURF,IMAX))-z(jvn(ISURF,IVERT))
          prod=vec1(1)*vec2(1)+vec1(2)*vec2(2)+vec1(3)*vec2(3)
          if (abs(prod).lt.0.00001) then

C Debug.
C      write(6,*) ' perp vert: ',IVERT

C Form parametric equations.
            SVALS(1)=vec2(1)
            SVALS(2)=vec2(2)
            SVALS(3)=vec2(3)
            TVALS(1)=vec1(1)
            TVALS(2)=vec1(2)
            TVALS(3)=vec1(3)
            TFORM(1)=x(jvn(ISURF,IMIN))
            TFORM(2)=y(jvn(ISURF,IMIN))
            TFORM(3)=z(jvn(ISURF,IMIN))
            return
          endif
 40     continue
      else
        do 50 IVERT=IMAX+1,IMIN-1
          vec1(1)=x(jvn(ISURF,IVERT))-x(jvn(ISURF,IMAX))
          vec1(2)=y(jvn(ISURF,IVERT))-y(jvn(ISURF,IMAX))
          vec1(3)=z(jvn(ISURF,IVERT))-z(jvn(ISURF,IMAX))
          vec2(1)=x(jvn(ISURF,IMIN))-x(jvn(ISURF,IVERT))
          vec2(2)=y(jvn(ISURF,IMIN))-y(jvn(ISURF,IVERT))
          vec2(3)=z(jvn(ISURF,IMIN))-z(jvn(ISURF,IVERT))
          prod=vec1(1)*vec2(1)+vec1(2)*vec2(2)+vec1(3)*vec2(3)
          if (abs(prod).lt.0.00001) then

C Debug.
C      write(6,*) ' perpend vert: ',IVERT

C Form parametric equations.
            SVALS(1)=vec2(1)
            SVALS(2)=vec2(2)
            SVALS(3)=vec2(3)
            TVALS(1)=vec1(1)
            TVALS(2)=vec1(2)
            TVALS(3)=vec1(3)
            TFORM(1)=x(jvn(ISURF,IMAX))
            TFORM(2)=y(jvn(ISURF,IMAX))
            TFORM(3)=z(jvn(ISURF,IMAX))
            return
          endif
 50     continue
      endif

      write (outs,'(a,i4)') 
     &            'Could not find right angle vertex in surface ',ISURF
      call edisp(iuout,outs)
      IR=1

      return
      end

