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

C+++++++++++++++++++++++
c  Check if surfaces of geometry are parallel or perpendicular
c  So that analytical equations can be used for the calculation
c  of viewfactors.

      subroutine checkrec

#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/G1M/XM(MTV),YM(MTV),ZM(MTV),NSURM,JVNM(MSM,MV),
     &           NVERM(MSM),NTVM
      COMMON /PLNEC/ PEQN(4,MSM), PCG(3,MSM), BOX(3,2,MSM),
     &               STMAT(4,4,MSM), SRMAT(4,4,MSM)
      COMMON /VISIB/ IVIS(MSM,MSM), IARECT(MSM,MSM)
      COMMON /TOTAL/ nsurmf, NGRID, NTRPOL

c  iarect(i,j) WITH i NOT EQUAL j
c             -will be  2 if the surfaces i and j are perpendicular
c              and totally visible to each other, with perpendicular
c              or parallel edges.
c             -will be  1 if the surfaces i and j are parallel and
c              totally visibile to each other, with perpendicular
c              or parallel edges.
c             -will be  0 if surfaces are not visible
c             -will be -1 if parallel and partly visible (shielded)
c             -will be -2 if perpendicular and partly visible
c             -will be -3 if neither perpendicular nor parallel
c             -will be -9 if surface j is visible but not rectangular
c             -will be -10 if surface i not rectangular
c  iarect(i,i)
c             -will be -20 if surface cannot see any other surface
c             -will be -19 if somewhere on the row is a -9
c             -will be -13 if somewhere on row is -3 (and no -9)
c             -will be -11 if somewhere on row is -1 (no -9 or -3)
c             -will be -12 if somewhere on row is -2 (no -9,-3,-1)
c              (can also be computed analytically)
c             -will be 0 if viewfactors ff(i,j) can be calculated
c              analytically.
c  inprod     inproduct of vectors (0 if perpendicular,
c             1 if parallel, -1 if anti-parallel)

        real inprod,length,vec1(3),vec2(3)
        integer i,j,k,zeros
        logical equal
        character outs*124

        do 12 i=1,nsurmf
          do 10 j=1,nsurmf
            iarect(i,j)=0
10        continue
12      continue

        do 25 i=1,nsurmf
          iarect(i,i)=-20
          do 20 j=1,nsurmf
            if (ivis(i,j).ne.-1) then
              iarect(i,i)=0
            endif
20        continue
25      continue

c   First check if surface i is rectangular.
c    nverm(i)        Number of vertices in i
c    jvnm(i,j)       vertex-index of surface i
c    xm(jvnm(i,j))
c    ym(jvnm(i,j))   coordinates of vertices
c    zm(jvnm(i,j))

c   We trace around the surface from vertex to vertex.
c   We take the inproduct of two subsequent normalised
c   direction vectors. If the surface is rectangular
c   the inproduct should be 4 times 0 and all other times
c   equal to 1. If this is not the case the surface is not
c   rectangular.
        do 90 i=1,nsurmf
          zeros = 0
          vec1(1)=xm(jvnm(i,1))-xm(jvnm(i,nverm(i)))
          vec1(2)=ym(jvnm(i,1))-ym(jvnm(i,nverm(i)))
          vec1(3)=zm(jvnm(i,1))-zm(jvnm(i,nverm(i)))
          length=(vec1(1)**2+vec1(2)**2+vec1(3)**2)**0.5
          vec1(1)=vec1(1)/length
          vec1(2)=vec1(2)/length
          vec1(3)=vec1(3)/length
        do 50 j=1,nverm(i)
          jj=j+1
          if (jj.gt.nverm(i)) then
            jj=1
          endif
          vec2(1)=xm(jvnm(i,jj))-xm(jvnm(i,j))
          vec2(2)=ym(jvnm(i,jj))-ym(jvnm(i,j))
          vec2(3)=zm(jvnm(i,jj))-zm(jvnm(i,j))
          length=(vec2(1)**2+vec2(2)**2+vec2(3)**2)**0.5
          vec2(1)=vec2(1)/length
          vec2(2)=vec2(2)/length
          vec2(3)=vec2(3)/length
          inprod=vec1(1)*vec2(1)+vec1(2)*vec2(2)+vec1(3)*vec2(3)
          vec1(1)=vec2(1)
          vec1(2)=vec2(2)
          vec1(3)=vec2(3)
          if (equal(inprod,0.0)) then
            zeros=zeros+1
          elseif (.not.equal(inprod,1.0)) then
            do 30 k=1,nsurmf
             iarect(i,k)=-10
             if (ivis(k,i).ne.-1) then
               iarect(k,i)=-9
               iarect(k,k)=-19
             else
               iarect(k,i)=0
             endif
30         continue
           iarect(i,i)=-10
         endif
50     continue
       if (zeros.ne.4) then
         do 70 k=1,nsurmf
           iarect(i,k)=-10
           if (ivis(k,i).ne.-1) then
             iarect(k,i)=-9
             iarect(k,k)=-19
           else
             iarect(k,i)=0
           endif
70       continue
         iarect(i,i)=-10
        endif
90      continue

        do 802 i=1,nsurmf-1
        do 800 j=i+1,nsurmf
        if (iarect(i,j).eq.-10) goto 800
        if (iarect(i,j).eq.-9) goto 800
        inprod=0.0
        do 110 k=1,3
          inprod=inprod + peqn(k,i)*peqn(k,j)
110      continue
c Here the inproduct of the normal vectors is taken.
         if (equal(-1.0,inprod)) goto 300
         if (equal(0.0,inprod)) goto 400
         if (equal(1.0,inprod)) goto 500

c  Angle between surfaces not 0 and not 90 degrees
         if (ivis(i,j).ne.-1) then
           iarect(i,j)=-3
           iarect(j,i)=-3
         endif

         goto 800

300      continue
c  Parallel and opposite orientation of surfaces.
         if (ivis(i,j).eq.1) then
           iarect(i,j)=1
           iarect(j,i)=1
         elseif (ivis(i,j).eq.-1) then
           iarect(i,j)=0
           iarect(j,i)=0
         elseif (ivis(i,j).eq.2) then
           iarect(i,j)=-1
           iarect(j,i)=-1
         elseif (ivis(i,j).eq.3) then
           iarect(i,j)=-2
           iarect(j,i)=-2
         endif
         goto 700

c  Perpendicular surfaces.
400      continue
         if (ivis(i,j).eq.1) then
           iarect(i,j)=2
           iarect(j,i)=2
         elseif (ivis(i,j).eq.-1) then
           iarect(i,j)=0
           iarect(j,i)=0
         elseif (ivis(i,j).eq.2) then
           iarect(i,j)=-1
           iarect(j,i)=-1
         elseif (ivis(i,j).eq.3) then
           iarect(i,j)=-2
           iarect(j,i)=-2
         endif
         goto 700

c  Parallel and same orientation of surfaces.
500      continue
         iarect(i,j)=0
         iarect(j,i)=0
         if (ivis(i,j).ne.-1) then
         write(outs,'(A,I3,A,I3,A)')' Fault in visiblity matrix: surf',
     &                              i,' to surf ',j,' should be -1!'
         call edisp(iuout,outs)
         endif
         goto 800

700      continue
         if (iarect(i,j).le.0) goto 800
c Surfaces are perpendicular or parallel and can see each other
c Are the edges also parallel or perpendicular ?
c If first edge is pararllel or perpendicular and the
c surface is rectangular then all edges are perp. or para.
         vec1(1)=xm(jvnm(i,1))-xm(jvnm(i,2))
         vec1(2)=ym(jvnm(i,1))-ym(jvnm(i,2))
         vec1(3)=zm(jvnm(i,1))-zm(jvnm(i,2))
         length=(vec1(1)**2+vec1(2)**2+vec1(3)**2)**0.5
         vec1(1)=vec1(1)/length
         vec1(2)=vec1(2)/length
         vec1(3)=vec1(3)/length

         vec2(1)=xm(jvnm(j,1))-xm(jvnm(j,2))
         vec2(2)=ym(jvnm(j,1))-ym(jvnm(j,2))
         vec2(3)=zm(jvnm(j,1))-zm(jvnm(j,2))
         length=(vec2(1)**2+vec2(2)**2+vec2(3)**2)**0.5
         vec2(1)=vec2(1)/length
         vec2(2)=vec2(2)/length
         vec2(3)=vec2(3)/length

         inprod=vec1(1)*vec2(1)+vec1(2)*vec2(2)+vec1(3)*vec2(3)
         if (equal(1.0,inprod)) goto 800
         if (equal(0.0,inprod)) goto 800
         if (equal(-1.0,inprod)) goto 800
         iarect(i,j)=iarect(i,j)*(-1)
         iarect(j,i)=iarect(j,i)*(-1)
800      continue
802      continue

c Check for negative values of iarect(i,j) and give iarect(i,i)
c the appropriate value.
         do 950 i=1,nsurmf
           if (iarect(i,i).eq.0) then
             do 900 j=1,nsurmf
               if (iarect(i,j).eq.-2) then
                 iarect(i,i)=-12
               endif
900          continue
             do 910 j=1,nsurmf
               if (iarect(i,j).eq.-1) then
                 iarect(i,i)=-11
               endif
910          continue
             do 920 j=1,nsurmf
               if (iarect(i,j).eq.-3) then
                 iarect(i,i)=-13
               endif
920          continue
         endif
950     continue
        call schryf
        return
        end


C+++++++++++++++++++++++ EQUAL
       logical function equal(x1,x2)
C          returns .true. if x1 and x2 are equal upto 0.00001
C                  .false. otherwise
       equal=.false.
       if (x1.le.(x2-0.00001)) goto 100
       if (x1.ge.(x2+0.00001)) goto 100
       equal=.true.
100    continue
       return
       end


C+++++++++++++++++++++++++
       subroutine schryf
#include "building.h"
       COMMON/OUTIN/IUOUT,IUIN,IEOUT
       COMMON /TOTAL/ nsurmf, NGRID, NTRPOL
       COMMON /VISIB/ IVIS(MSM,MSM), IARECT(MSM,MSM)
       character outs*124

       integer ansurfs(MS)
       integer k
       k=1
       do 100 i=1,nsurmf
         if ((iarect(i,i).eq.0).or.(iarect(i,i).eq.-12)) then
           ansurfs(k)=i
           k=k+1
         endif
100    continue

       if (k.gt.1) then
         ipos=1
         call edisp(iuout,' ')
         call edisp(iuout,' The viewfactors that will be calculated ')
         call edisp(iuout,' analytically are from the surface(s) :')
         call ailist(ipos,k-1,ansurfs,MS,'S',outs,loutln,itrunc)
         call edisp(iuout,outs)
       endif
       return
       end
