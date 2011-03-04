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

C GRID places a grid on a surface.
C IS - surface to grid
C igmaj,igmin - number of points in major and minor direction
C IVS,IVE - start and end verteg of edge defining major direction
C TFRM - size of transform along normal
C GPTS - x,y,z of grid points
C NGRID - number of grid points (after point containment)
C PEQN - equation of plane - does this need to be passed back???
C IER - error counter - used??
      SUBROUTINE GRID(IS,igmaj,igmin,IVS,IVE,TFRM,GPTS,NGRID,PEQN,ier)
#include "building.h"
#include "geometry.h"

      DIMENSION  GPTS(500,3)

      DIMENSION XSUR(MV),YSUR(MV),ZSUR(MV)
      DIMENSION PEQN(4),CG(3)
      REAL NVEC(3),MAJVEC(3),MINVEC(3)
      DIMENSION PA(3),PB(3),CA(3),CB(3)

C Initialize.
      NGRID=0
      ier=0

C Find equation of surface containing surface IS.
      do 10 I=1,NVER(IS)
        XSUR(I)=X(JVN(IS,I))
        YSUR(I)=Y(JVN(IS,I))
        ZSUR(I)=Z(JVN(IS,I))
 10   continue
      call PLEQN(XSUR,YSUR,ZSUR,NVER(IS),CG,PEQN,IERPLN)

C Find minimum and maximum extents of surface parallel to defined edge.
C For each vertex find intersection of line through test verts with 
C plane through point with normal parallel to test line.
      MAJVEC(1)=X(IVE)-X(IVS)
      MAJVEC(2)=Y(IVE)-Y(IVS)
      MAJVEC(3)=Z(IVE)-Z(IVS)

C Debug.
C      write(6,*) 'MAJvec ',(MAJVEC(I),I=1,3)

      Tmin=999999.9
      Tmax=-999999.9
      do 15 I=1,NVER(IS)
        T=(MAJVEC(1)*(X(JVN(IS,I)) - X(IVS)) + 
     &     MAJVEC(2)*(Y(JVN(IS,I)) - Y(IVS)) +
     &     MAJVEC(3)*(Z(JVN(IS,I)) - Z(IVS)))/
     &     (MAJVEC(1)*MAJVEC(1)+MAJVEC(2)*MAJVEC(2)+MAJVEC(3)*MAJVEC(3))
        Tmax=AMAX1(Tmax,T)
        Tmin=AMIN1(Tmin,T)
 15   continue

C Debug.
C      write(6,*) 'Tmax ',Tmax,'  Tmin',Tmin
C      write(6,*) 'Min vert ',(Tmin*MAJVEC(1))+X(IVS),
C     &                       (Tmin*MAJVEC(2))+Y(IVS),
C     &                        (Tmin*MAJVEC(3))+Z(IVS)
C      write(6,*) 'Max vert ',(Tmax*MAJVEC(1))+X(IVS),
C     &                       (Tmax*MAJVEC(2))+Y(IVS),
C     &                        (Tmax*MAJVEC(3))+Z(IVS)

C Now find vector in plane perp to defined edge.
      NVEC(1)=PEQN(1)
      NVEC(2)=PEQN(2)
      NVEC(3)=PEQN(3)

C Debug.
C      write(6,*) 'NVEC ',(NVEC(I),I=1,3)

      call CROSS(NVEC,MAJVEC,MINVEC)

C Debug.
C      write(6,*) 'MINvec ',(MINVEC(I),I=1,3)

C Now find minimum and maximum extents of surface perp to defined edge.
C For each vertex find intersection of line through test verts with 
C plane through point with normal parallel to test line.
      Smin=999999.9
      Smax=-999999.9
      do 20 I=1,NVER(IS)
        S=(MINVEC(1)*(X(JVN(IS,I)) - X(IVS)) + 
     &     MINVEC(2)*(Y(JVN(IS,I)) - Y(IVS)) +
     &     MINVEC(3)*(Z(JVN(IS,I)) - Z(IVS)))/
     &     (MINVEC(1)*MINVEC(1)+MINVEC(2)*MINVEC(2)+MINVEC(3)*MINVEC(3))
        Smax=AMAX1(Smax,S)
        Smin=AMIN1(Smin,S)
 20   continue

C Debug.
C      write(6,*) 'Smax ',Smax,'  Smin',Smin
C      write(6,*) 'Min vert ',(Smin*MINVEC(1))+X(IVS),
C     &                       (Smin*MINVEC(2))+Y(IVS),
C     &                       (Smin*MINVEC(3))+Z(IVS)
C      write(6,*) 'Max vert ',(Smax*MINVEC(1))+X(IVS),
C     &                       (Smax*MINVEC(2))+Y(IVS),
C     &                       (Smax*MINVEC(3))+Z(IVS)

C Find corner of bounding box.
      PA(1)=Tmin*MAJVEC(1)+X(IVS)
      PA(2)=Tmin*MAJVEC(2)+Y(IVS)
      PA(3)=Tmin*MAJVEC(3)+Z(IVS)
      PB(1)=Smin*MINVEC(1)+X(IVS)
      PB(2)=Smin*MINVEC(2)+Y(IVS)
      PB(3)=Smin*MINVEC(3)+Z(IVS)
      do 22 I=1,3
        CA(I)=0.
        CB(I)=0.
 22   continue
      call CLOSE3D(PA,MAJVEC,SA,PB,MINVEC,SB,DIST,CA,CB)

C Debug.
C      write(6,*) 'Intersect A ',(CA(I),I=1,3),' : ',SA,Tmin
C      write(6,*) 'Intersect B ',(CB(I),I=1,3),' : ',SB,Smin
C      write(6,*) 'Dist ',DIST

C The intersect will not be the minimum corner of the bounding box,
C but will be at a value of zero for SA or SB
      if (abs(SA-Tmin).lt.0.001) then
        XS=Smin*MINVEC(1)+X(IVS)
        YS=Smin*MINVEC(2)+Y(IVS)
        ZS=Smin*MINVEC(3)+Z(IVS)
      else
        XS=Tmin*MAJVEC(1)+X(IVS)
        YS=Tmin*MAJVEC(2)+Y(IVS)
        ZS=Tmin*MAJVEC(3)+Z(IVS)
      endif

C Debug.
C      write(6,*) 'Start corner ',XS,YS,ZS

C Scale direction vectors to equal length of bounding box edges divided 
C by 2*(number of grid points).
      do 25 I=1,3
        MAJVEC(I)=MAJVEC(I)*(Tmax-Tmin)/real(igmaj*2)
        MINVEC(I)=MINVEC(I)*(Smax-Smin)/real(igmin*2)
 25   continue

C Debug.
C      write(6,*) 'MAJvec ',(MAJVEC(I),I=1,3)
C      write(6,*) 'MINvec ',(MINVEC(I),I=1,3)

C Now make grid points.
      do 30 I=1,igmin
        do 40 J=1,igmaj
          GX=XS + (2.*J-1.)*MAJVEC(1) + (2.*I-1.)*MINVEC(1)
          GY=YS + (2.*J-1.)*MAJVEC(2) + (2.*I-1.)*MINVEC(2)
          GZ=ZS + (2.*J-1.)*MAJVEC(3) + (2.*I-1.)*MINVEC(3)
          call INSIDE3D(XSUR,YSUR,ZSUR,NVER(IS),GX,GY,GZ,INSURF)
          if (INSURF.eq.1) then
            NGRID = NGRID + 1
            GPTS(NGRID,1)=GX + PEQN(1)*TFRM
            GPTS(NGRID,2)=GY + PEQN(2)*TFRM
            GPTS(NGRID,3)=GZ + PEQN(3)*TFRM
          endif
 40     continue
 30   continue

      return
      end

C ******************** INSIDE3D ********************
C INSIDE3D returns 1 if a point is inside the given surface.
C Assumes that surface is bounded.
C Routine works by projecting line from the COG away from vertex 2
C and counts the number of intersections with edges.
C X,Y,Z are ordered coords of surface, in arrays of dimension MV.
C NV - number of vertices in surface
C XT,YT,ZT test point
C INSURF - if = 1 then inside surface.

      SUBROUTINE INSIDE3D(X,Y,Z,NV,XT,YT,ZT,INSURF)
#include "building.h"

      DIMENSION X(MV),Y(MV),Z(MV)
      DIMENSION COG(3),VC(3),TV(3),PA(3),PB(3)
      DIMENSION CA(3),CB(3),VPH(3)

      LOGICAL PREVHIT,SAMEDIR

      INSIDE=0
      INSURF=0

C Calculate bounding rectangle and check if point is inside it.
C This is a quick test to see if its worthwhile continuing.
      XMIN=X(1)
      XMAX=X(1)
      YMIN=Y(1)
      YMAX=Y(1)
      ZMIN=Z(1)
      ZMAX=Z(1)
      COG(1)=X(1)
      COG(2)=Y(1)
      COG(3)=Z(1)
      do 5 I=2,NV
        XMIN=AMIN1(XMIN,X(I))
        XMAX=AMAX1(XMAX,X(I))
        YMIN=AMIN1(YMIN,Y(I))
        YMAX=AMAX1(YMAX,Y(I))
        ZMIN=AMIN1(ZMIN,Z(I))
        ZMAX=AMAX1(ZMAX,Z(I))
        COG(1)=COG(1)+X(I)
        COG(2)=COG(2)+Y(I)
        COG(3)=COG(3)+Z(I)
 5    continue

C If outside BB then return.
      if (XT.lt.XMIN.or.XT.gt.XMAX.or.
     &    YT.lt.YMIN.or.YT.gt.YMAX.or.
     &    ZT.lt.ZMIN.or.ZT.gt.ZMAX) return

C Calculate COG.
      COG(1)=COG(1)/real(NV)
      COG(2)=COG(2)/real(NV)
      COG(3)=COG(3)/real(NV)

C Generate test vector parallel to line from point 2 to COG.
      IVECX=2
 6    TV(1)=COG(1)-X(IVECX)
      TV(2)=COG(2)-Y(IVECX)
      TV(3)=COG(3)-Z(IVECX)

C Check that this vector is not 0,0,0
      if (ABS(TV(1)).lt.0.001.and.ABS(TV(2)).lt.0.001.and.
     &    ABS(TV(3)).lt.0.001) then
        IVECX=IVECX+1
        if (IVECX.gt.NV) then

C This should never happen!
          call usrmsg('Could not create suitable test vector',
     &             'for point containment test, ignoring surface.','W')
          return
        endif
        goto 6
      endif

C Calculate TV.TV
      TVsq=0.0
      do 15 I=1,3
        TVsq=TVsq+TV(I)*TV(I)
 15   continue

C Check for intesection with each edge
      PREVHIT=.false.
      do 20 IV=1,NV
        if (IV.eq.NV) then
          IVN=1
          IVP=IV-1
        elseif (IV.eq.1) then
          IVN=IV+1
          IVP=NV
        else
          IVN=IV+1
          IVP=IV-1
        endif

C First check if previous edge was a hit, if so then ignore this
C edge if it continues in a similar direction.
        VC(1)=X(IVN)-X(IV)
        VC(2)=Y(IVN)-Y(IV)
        VC(3)=Z(IVN)-Z(IV)
        if (PREVHIT) then

C Find intersection of line joining two points and test vector.
          PA(1)=X(IV)
          PA(2)=Y(IV)
          PA(3)=Z(IV)
          VPH(1)=X(IVN)-X(IVP)
          VPH(2)=Y(IVN)-Y(IVP)
          VPH(3)=Z(IVN)-Z(IVP)
          PB(1)=XT
          PB(2)=YT
          PB(3)=ZT
          call CLOSE3D(PA,VPH,SA,PB,TV,SB,DIST,CA,CB)

C If the intersect is between vector IVN and IVP then SAMEDIR is true.
          if (SA.ge.0.0.and.SA.le.1.0) then
            SAMEDIR=.true.
          elseif (DIST.lt.0.0) then
            SAMEDIR=.true.
          else
            SAMEDIR=.false.
          endif
        endif
        if (PREVHIT.and.SAMEDIR) then

C Ignore this edge as its just a kink in the boundary.
          INSIDE=INSIDE+0
          PREVHIT=.false.
        else

C Find intersection.
          PA(1)=X(IV)
          PA(2)=Y(IV)
          PA(3)=Z(IV)
          PB(1)=XT
          PB(2)=YT
          PB(3)=ZT
          call CLOSE3D(PA,VC,SA,PB,TV,SB,DIST,CA,CB)

C Now check intersection.
          if (DIST.gt.0.001) then

C Lines miss in space - set SB to -99 to skip further checks.
            SB=-99.
          elseif (abs(DIST).lt.0.000) then

C Lines are parallel - set SB to -99 to skip further checks.
            SB=-99.
          endif
          if (SB.gt.0.0) then
            if (SA.ge.0.0.and.SA.le.1.0) then
              INSIDE=INSIDE+1
              if (abs(SA-1.0).lt.0.001) then
                PREVHIT=.true.
              else
                PREVHIT=.false.
              endif
            else
              PREVHIT=.false.
            endif
          else
            PREVHIT=.false.
          endif
        endif
 20   continue

C Check if INSIDE is odd.
      INSURF=INSIDE-((INSIDE/2)*2)

C Debug.
C      if (INSURF.eq.0) then
C        write(6,*) 'Point outside surface',INSURF
C      else
C        write(6,*) 'Point inside surface',INSURF
C      endif

      return
      end
