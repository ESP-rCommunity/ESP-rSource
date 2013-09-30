C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-2009.

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

C cfgrid.F contains the following routines:
C GRID:    Controling routine for all gridding.
C GRIDIST: Distribute grid cells according to power law equation in 1-D.
C GRIDCRV: Distribute grid cells according curvilinear data in 2-D.
C GRIDGEO: Calculate geometrical properties and interpolation factors 
C          of gridded domain.
C LINVOL:  Logical function: TRUE if inside nominated volume.

C ************************** GRID **************************
C GRID: Controling routine for all gridding.
C IER is returned as one if east/west grids do not match.
      SUBROUTINE GRID(ier)
#include "cfd.h"

C Passed parameter
      integer ier
      
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      common/GRIDFN/NCELX(MNREG,MNZ),NCELY(MNREG,MNZ),NCELZ(MNREG,MNZ),
     &  NCELZE(MNREG,MNZ),XREG(MNREG,MNZ),YREG(MNREG,MNZ),
     &  ZREG(MNREG,MNZ),ZREGE(MNREG,MNZ),Xplaw(MNREG,MNZ),
     &  Yplaw(MNREG,MNZ),Zplaw(MNREG,MNZ),Zplawe(MNREG,MNZ),NREG(4,MNZ)
      COMMON/ICFNOD/ICFD,ICP
      common/GRIDEFN/origin,xgrid,ygrid,zgrid,zegrid,ortho

C Data associated with Z axis curvilinear gridding.
      common/curvg1/ZHI(ntcelx),ZLO(ntcelx),ZWest(ntcelz),ZEast(ntcelz)

      logical origin,xgrid,ygrid,zgrid,zegrid,ortho

C Generating grid for X-axis.
      ICELx=2
      XU(ICELx)=0.0
      do 10 IREG=1,NREG(1,ICFD)
        call GRIDIST(IREG,ICFD,XPLAW,NCELX,XREG,XU,ICELx,ntcelx)
        ICELx=ICELx+ABS(NCELX(IREG,ICFD))
 10   continue
      XU(1)=-XU(3)

C Generating grid for Y-axis.
      ICELy=2
      YV(ICELy)=0.0
      do 20 IREG=1,NREG(2,ICFD)
        call GRIDIST(IREG,ICFD,YPLAW,NCELY,YREG,YV,ICELy,ntcely)
        ICELy=ICELy+ABS(NCELY(IREG,ICFD))
 20   continue
      YV(1)=-YV(3)

C Generating grid for Z-axis.
C Check for orthogonal gridding.
      if (ORTHO) then
        ICELz=2
        ZW(ICELz)=0.0
        do 30 IREG=1,NREG(3,ICFD)
          call GRIDIST(IREG,ICFD,ZPLAW,NCELZ,ZREG,ZW,ICELz,ntcelz)
          ICELz=ICELz+ABS(NCELZ(IREG,ICFD))
 30     continue
        ZW(1)=-ZW(3)
      else

C Generating grid for Z-axis on West side of domain.
        ICELw=2
        Zwest(ICELw)=0.0
        do 40 IREG=1,NREG(3,ICFD)
          call GRIDIST(IREG,ICFD,ZPLAW,NCELZ,ZREG,Zwest,ICELw,ntcelz)
          ICELw=ICELw+ABS(NCELZ(IREG,ICFD))
 40     continue
        Zwest(1)=-Zwest(3)

C Generating grid for Z-axis on East side of domain.
        ICELe=2
        Zeast(ICELe)=0.0
        do 50 IREG=1,NREG(4,ICFD)
          call GRIDIST(IREG,ICFD,ZPLAW,NCELZ,ZREG,Zeast,ICELe,ntcelz)
          ICELe=ICELe+ABS(NCELZ(IREG,ICFD))
 50     continue
        Zeast(1)=-Zeast(3)

C Check for equal grid points on opposite faces.
        if (ICELw.ne.ICELe) then
          call usrmsg('The number of grid cells in the east and ',
     &                    'west face are different. Please check! ','W')
          ier=1
          return
        endif

C Calculate intersection points.
        call GRIDCRV(ICELx,ICELe,ier)
      endif 

C Calculate geom properties of grid.
      call GRIDGEO

      return
      end


C ************************** GRIDIST **************************
C GRIDIST: Distribute grid cells according to power law equation.
C IR - current region.
C IC - current cfd domain.
C PLAW - power law coefficients.
C NCEL - number of cells.
C REG - linear size of regions.
C DIST - linear dimension of grid cells.
C ICELL - position within cell (??)
C NDIM - parameter for array size of DIST
      SUBROUTINE GRIDIST(IR,IC,PLAW,NCEL,REG,DIST,ICELL,NDIM)
#include "cfd.h"
      integer IR,IC,NCEL,ICELL,NDIM
      real PLAW,REG,DIST
      dimension PLAW(MNREG,MNZ),NCEL(MNREG,MNZ),REG(MNREG,MNZ)
      dimension DIST(NDIM)

C Generating grid for current region.
      if (PLAW(IR,IC).GT.0.0) then

C Generate power law grid type: I**PLAW
        if (NCEL(IR,IC).GT.0) then

C Grid size increasing from left to right.
          do 10 I=1,NCEL(IR,IC)-1
            DIST(I+ICELL)=REG(IR,IC)*(float(I)/float(NCEL(IR,IC)))**
     &                    PLAW(IR,IC)+DIST(ICELL)
 10       continue
          DIST(NCEL(IR,IC)+ICELL)=REG(IR,IC)+DIST(ICELL)

C Grid size increasing from left to middle of domain and decreasing
C from middle to right.
        else
          N=INT(ABS(NCEL(IR,IC))/2)
          NTC=ABS(NCEL(IR,IC))
          XCL=ABS(NCEL(IR,IC))/2.0
          do 20 I=1,N
            AUX=REG(IR,IC)/2.0*(I/XCL)**PLAW(IR,IC)
            DIST(I+ICELL)=AUX+DIST(ICELL)
            DIST(NTC-I+ICELL)=REG(IR,IC)-AUX+DIST(ICELL)
 20       continue
          DIST(NTC+ICELL)=REG(IR,IC)+DIST(ICELL)
        endif
      else
        if (NCEL(IR,IC).GT.0) then

C Grid size decreasing from left to right.
          do 50 I=1,NCEL(IR,IC)-1
            DIST(NCEL(IR,IC)-I+ICELL)=REG(IR,IC)*
     &          (1.0-(float(I)/float(NCEL(IR,IC)))**ABS(PLAW(IR,IC)))+
     &          DIST(ICELL)
 50       continue
          DIST(NCEL(IR,IC)+ICELL)=REG(IR,IC)+DIST(ICELL)

C Grid size decreasing from west to middle of domain and increasing
C from middle to east.
        else
          N=INT(ABS(NCEL(IR,IC))/2)
          IF(2*N.EQ.ABS(NCEL(IR,IC)))THEN
            N1=N
          ELSE
            N1=N+1
          ENDIF
          XCL=ABS(NCEL(IR,IC))/2.0
          NTC=ABS(NCEL(IR,IC))
          DO 60 I=1,N
            AUX=REG(IR,IC)/2.0*(1.0-(I/XCL)**ABS(PLAW(IR,IC)))
            DIST(N+1-I+ICELL)=AUX+DIST(ICELL)
            DIST(N1-1+I+ICELL)=REG(IR,IC)-AUX+DIST(ICELL)
 60       continue
          DIST(NTC+ICELL)=REG(IR,IC)+DIST(ICELL)
        endif
      endif

      return
      end


C ************************** GRIDGEO **************************
C GRIDGEO: Calculate geometrical properties and interpolation factors.

      SUBROUTINE GRIDGEO
#include "cfd.h"

      COMMON/CONST/GREAT,small,GRAV
      COMMON/UVEL/RESORU,NSWPU,URFU,FSDTU,DXEPU(ntcelx),
     1            DXPWU(ntcelx),SEWU(ntcelx)
      COMMON/VVEL/RESORV,NSWPV,URFV,FSDTV,DYNPV(ntcely),
     1            DYPSV(ntcely),SNSV(ntcely)
      COMMON/WVEL/RESORW,NSWPW,URFW,FSDTW,DZHPW(ntcelz),
     1            DZPLW(ntcelz),SHLW(ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/GEOM2/VolP(ntcelx,ntcely,ntcelz),
     &             VolU(ntcelx,ntcely,ntcelz),
     &             VolV(ntcelx,ntcely,ntcelz),
     &             VolW(ntcelx,ntcely,ntcelz)
      COMMON/GEOM3/AreaHLP(ntcelx,ntcely),AreaHLU(ntcelx,ntcely),
     &             AreaHLV(ntcelx,ntcely),AreaEWP(ntcely,ntcelz),
     &             AreaEWV(ntcely,ntcelz),AreaEWW(ntcely,ntcelz),
     &             AreaNSP(ntcelx,ntcelz),AreaNSU(ntcelx,ntcelz),
     &             AreaNSW(ntcelx,ntcelz)
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)


C Generate scalar grid in X direction. Scalar points located mid-way between
C X-velocity points.
      DO 201 I=1,NIM1
        XP(I)=0.5*(XU(I+1)+XU(I))
  201 continue
      XP(NI)=2.0*XU(NI)-XP(NIM1)

C Generate scalar grid in Y direction. Scalar points located mid-way between
C Y-velocity points.
      DO 203 J=1,NJM1
        YP(J)=0.5*(YV(J+1)+YV(J))
  203 continue
      YP(NJ)=2.0*YV(NJ)-YP(NJM1)

C Generate scalar grid in Z direction. Scalar points located mid-way between
C Z-velocity points.
      DO 205 K=1,NKM1
        ZP(K)=0.5*(ZW(K+1)+ZW(K))
  205 continue
      ZP(NK)=2.0*ZW(NK)-ZP(NKM1)
 
C When calculating differences make sure that these do not equal zero,
C as the values are used in the denominators at many places during solution.
C Calculate distances between scalar points in X-direction.
      DXPW(1)=small
      DXEP(NI)=small
      DO 101 I=1,NIM1
        DXEP(I)=ANOTZERO(XP(I+1)-XP(I))
        DXPW(I+1)=DXEP(I)
  101 continue

C Calculate distances between scalar points in Y-direction.
      DYPS(1)=small
      DYNP(NJ)=small
      DO 102 J=1,NJM1
        DYNP(J)=ANOTZERO(YP(J+1)-YP(J))
        DYPS(J+1)=DYNP(J)
  102 continue

C Calculate distances between scalar points in Z-direction.
      DZPL(1)=small
      DZHP(NK)=small
      DO 103 K=1,NKM1
        DZHP(K)=ANOTZERO(ZP(K+1)-ZP(K))
        DZPL(K+1)=DZHP(K)
  103 continue

C Calculate the widths of the scalar control volumes in the X-direction.
      SEW(1)=small
      SEW(NI)=small
      DO 104 I=2,NIM1
        SEW(I)=ANOTZERO(XU(I+1)-XU(I))
  104 continue

C Calculate the widths of the scalar control volumes in the Y-direction.
      SNS(1)=small
      SNS(NJ)=small
      DO 105 J=2,NJM1
        SNS(J)=ANOTZERO(YV(J+1)-YV(J))
  105 continue

C Calculate the heights of the scalar control volumes in the Z-direction.
      SHL(1)=small
      SHL(NK)=small
      DO 106 K=2,NKM1   
        SHL(K)=ANOTZERO(ZW(K+1)-ZW(K))
  106 continue

C DXEPU is the same as SEW. Why duplicate?
      DXPWU(1)=small
      DXEPU(NI)=small
      DO 107 I=1,NIM1
        DXEPU(I)=ANOTZERO(XU(I+1)-XU(I))
        DXPWU(I+1)=DXEPU(I)
  107 continue

C SEWU is the same as DXPW. Why duplicate?
      SEWU(1)=small
      DO 108 I=2,NI
        SEWU(I)=ANOTZERO(XP(I)-XP(I-1))
  108 continue

C DYNPV is the same as SNS. Why duplicate?
      DYPSV(1)=small
      DYNPV(NJ)=small
      DO 109 J=1,NJM1
        DYNPV(J)=ANOTZERO(YV(J+1)-YV(J))
        DYPSV(J+1)=DYNPV(J)
  109 continue

C SNSV is the same as DYPS. Why duplicate?
      SNSV(1)=small
      DO 110 J=2,NJ
        SNSV(J)=ANOTZERO(YP(J)-YP(J-1))
  110 continue

C DZHPW is the same as SHL. Why duplicate?
      DZPLW(1)=small
      DZHPW(NK)=small
      DO 111 K=1,NKM1
        DZHPW(K)=ANOTZERO(ZW(K+1)-ZW(K))
        DZPLW(K+1)=DZHPW(K)
  111 continue

C SHLW is the same as DZPL. Why duplicate?
      SHLW(1)=small
      DO 112 K=2,NK
        SHLW(K)=ANOTZERO(ZP(K)-ZP(K-1))
  112 continue

C Calculate interpolation factors for X-direction. SIFE and SIFW are used to
C evaluate properties at the faces of the scalar control volumes (ie. at the
C X-velocity points):
C   SIFE = 1 - {distance from P to e}/{distance from P to E}
C   SIFW = 1 - {distance from P to w}/{distance from P to W}
C If SIFE --> 1, then e face is very close to P.
C If SIFE --> 0, then e face is very far from P.
C Example usage, calculate density at e face:
C   DEN(e) = SIFE(I)*DEN(P) + SIFW(I+1)*DEN(E)
      DO 150 I=2,NIM1
        SIFE(I)=1.-0.5*SEW(I)/DXEP(I)
        SIFW(I)=1.-0.5*SEW(I)/DXPW(I)
  150 CONTINUE

C Calculate interpolation factors for Y-direction. Similar treatment as for X.
      DO 160 J=2,NJM1
        SIFN(J)=1.-0.5*SNS(J)/DYNP(J)
        SIFS(J)=1.-0.5*SNS(J)/DYPS(J)
  160 CONTINUE

C Calculate interpolation factors for Z-direction. Similar treatment as for X.
      DO 170 K=2,NKM1
        SIFH(K)=1.-0.5*SHL(K)/DZHP(K)
        SIFL(K)=1.-0.5*SHL(K)/DZPL(K)
  170 CONTINUE

C Calculate interpolation factors for fictitious cells.
      SIFE(1)=0.5
      SIFN(1)=0.5
      SIFH(1)=0.5
      SIFW(NI)=0.5
      SIFS(NJ)=0.5
      SIFL(NK)=0.5

C Calculate cell volumes and face areas.
      do 200 I=1,NI
        do 210 J=1,NJ

C Face areas of scalar and velocity control volumes.
C The final letter in names refers to the center of the stagered grid 
C control volume: P=scalar or pressure cv; U=U velocity stagered grid cv etc.
C High/Low areas.
          AreaHLP(I,J)=SEW(I)*SNS(J)
          AreaHLU(I,J)=SEWU(I)*SNS(J)
          AreaHLV(I,J)=SEW(I)*SNSV(J)
          do 220 K=1,NK

C VOLUME of scalar control volume, U, V and W staggered control volumes.
            VolP(I,J,K)=SEW(I)*SNS(J)*SHL(K)
            VolU(I,J,K)=SEWU(I)*SNS(J)*SHL(K)
            VolV(I,J,K)=SEW(I)*SNSV(J)*SHL(K)
            VolW(I,J,K)=SEW(I)*SNS(J)*SHLW(K)

C Face areas of scalar and velocity control volumes.
C East/West areas.
            AreaEWP(J,K)=SNS(J)*SHL(K)
            AreaEWV(J,K)=SNSV(J)*SHL(K)
            AreaEWW(J,K)=SNS(J)*SHLW(K)

C North/South areas.
            if (J.eq.1) then
              AreaNSP(I,K)=SEW(I)*SHL(K)
              AreaNSU(I,K)=SEWU(I)*SHL(K)
              AreaNSW(I,K)=SEW(I)*SHLW(K)
            endif
 220      continue
 210    continue
 200  continue

      return
      end


C ************************** GRIDCRV **************************
C GRIDCRV: Calculate intersection points for curvilinear gridding.
C IS - total number of grid points in x direction.
C KS - total number of grid points in z direction.
C ier is set to one if a vertical step is found.
      SUBROUTINE GRIDCRV(IS,KS,ier)
#include "building.h"
#include "geometry.h"
#include "cfd.h"

C Passed parameters.
      integer IS,KS,ier
      
      COMMON/GEOMCURV/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      common/CLVTXNO/ivxhigh(MV),ivxlow(MV),ihigh,ilow
      common/curvg1/ZHI(ntcelx),ZLO(ntcelx),ZWest(ntcelz),ZEast(ntcelz)

      dimension ZWcl(ntcelx,ntcelz)

C Calc top and bottom grid points.
      iold = 1
      do 333 loophi=1,ihigh-1

C Get the xyz of the current and the next point (ignore Z diferences here).
        x1=X(ivxhigh(loophi))
        y1=Y(ivxhigh(loophi))
        z1=Z(ivxhigh(loophi))
        x2=X(ivxhigh(loophi+1))
        y2=Y(ivxhigh(loophi+1))
        z2=Z(ivxhigh(loophi+1))
        xdist = crowxyz(x1,y1,z1,x2,y2,z1)
        if(xdist.gt.0.0)then
          xm = (z2-z1)/xdist
        else
          call usrmsg('found a vertical step! yuck!',' ','W')
          ier=1
          return
        endif

C Now loop through XU corrds within this bit of the upper curve.
C chkdist is againt the west facade to the end of the current line segment.
        chkdist1 = crowxyz(X(ivxhigh(1)),Y(ivxhigh(1)),z1,x1,y1,z1)
        chkdist2 = crowxyz(X(ivxhigh(1)),Y(ivxhigh(1)),z1,x2,y2,z1)

C Debug.
C        write(6,*) 'distances ',xdist,chkdist1,chkdist2,xm

        do 334 ixgp = iold,is
          if(XU(ixgp).le.chkdist2)then
            xst= XU(ixgp)-chkdist1
            ZHI(ixgp)= xm*xst - xm*x1 + z1
          else 
            goto 335
          endif
 334    continue
 335    iold=ixgp
 333  continue

      iold = 1
      do 433 looplo=1,ilow-1

C Get the xyz of the current and the next point (ignore Z diferences here).
        x1=X(ivxlow(looplo))
        y1=Y(ivxlow(looplo))
        z1=Z(ivxlow(looplo))
        x2=X(ivxlow(looplo+1))
        y2=Y(ivxlow(looplo+1))
        z2=Z(ivxlow(looplo+1))
        xdist = crowxyz(x1,y1,z1,x2,y2,z1)
        if(xdist.gt.0)then
          xm = (z2-z1)/xdist
        else
          call usrmsg('found a vertical step! yuck!',' ','W')
          ier=1
          return
        endif

C Now loop through XU corrds within this bit of the upper curve.
C chkdist is againt the west facade to the end of the current line segment.
        chkdist1 = crowxyz(X(ivxlow(1)),Y(ivxlow(1)),z1,x1,y1,z1)
        chkdist2 = crowxyz(X(ivxlow(1)),Y(ivxlow(1)),z1,x2,y2,z1)
        do 434 ixgp = iold,is
          if(XU(ixgp).le.chkdist2)then
            xst= XU(ixgp)-chkdist1
            ZLO(ixgp)= xm*xst - xm*x1 + z1
          else 
            goto 435
          endif
 434    continue
 435    iold=ixgp
 433  continue

C Debug.
C      write(6,*) 'ZWest ',(ZWest(II),II=2,ks)
C      write(6,*) 'ZEast ',(ZEast(II),II=2,ks)
C      write(6,*) 'ZHI ',(ZHI(II),II=2,is)
C      write(6,*) 'ZLO ',(ZLO(II),II=2,is)
C      write(6,*) 'IS ',is,'   KS ',ks

C Calculate the central grid points (we now know the boundary grid points).
      do 600 IX=2,IS
        do 610 IZ=2,KS
          tz=(float(IS-IX)*(ZWest(IZ)/ZWest(KS))+
     &        float(IX-1)*(ZEast(IZ)/ZEast(KS)))/float(IS-1)
          ZWcl(IX,IZ)=ZLO(IX) + tz*(ZHI(IX)-ZLO(IX))
 610    continue
 600  continue

C Debug.
C      write(6,*) 'Grid points'
C      write(6,'(a,10i6)') ' Z,   X=',(II,II=1,is)
      
C      do 700 IX=KS,1,-1
C        write(6,'(i10,10f6.2)') IX,(ZWcl(II,IX),II=1,is)
C 700  continue

      return
      end

C ********************* LINVOL *********************
C LINVOL - Logical function: TRUE if inside nominated volume.
C Itest,Jtest,Ktest - test point
C Ii,If start and end of I direction
C Ji,Jf start and end of J direction
C Ki,Kf start and end of J direction
      LOGICAL FUNCTION LINVOL(Itest,Jtest,Ktest,Ii,If,Ji,Jf,Ki,Kf)

      integer Itest,Jtest,Ktest,Ii,If,Ji,Jf,Ki,Kf
       
      LINVOL=.false.
      if (Itest.ge.Ii.and.Itest.le.If) then
        if (Jtest.ge.Ji.and.Jtest.le.Jf) then
          if (Ktest.ge.Ki.and.Ktest.le.Kf) then
            LINVOL=.true.
          endif
        endif
      endif
 
      RETURN
      END
