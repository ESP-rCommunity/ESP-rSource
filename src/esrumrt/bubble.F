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

C+++++++++++++++++++++++ BUBBLE
C Bubble projects through bubble from each grid cell.
      SUBROUTINE  BUBBLE


#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN

C PEQN  -  plane equation of surface
C PCG   -  centroid of surface
      COMMON /PLNEC/ PEQN(4,MSM), PCG(3,MSM), BOX(3,2,MSM),
     &               STMAT(4,4,MSM), SRMAT(4,4,MSM)
      COMMON /FORMF/ FF(MSM,MSM), FFS
      COMMON /VISIB/ IVIS(MSM,MSM), IARECT(MSM,MSM)
      COMMON /GRIDS/ IDG(MGC), XC(MGC), YC(MGC), ZC(MGC)
      COMMON /BUBXYZ2/ XBB(11000), YBB(11000), ZBB(11000)
      COMMON /BUBXYZ1/ NPATCH, NBUB
      COMMON /TOTAL/ nsurmf, NGRID, NTRPOL
      common /cube5/fstgrp,fstsur,lstsur,strtgr
      integer fstgrp,fstsur,lstsur,strtgr
      logical anal

C Initial value for IDS ??
      IDS=0
      CALL BUB(npatch)

c Set up counters to identify which stage the
c calculation has reached in current run.
      K1=strtgr+(NGRID-strtgr)/4
      K2=strtgr+(NGRID-strtgr)/2
      K3=strtgr+(NGRID-strtgr)*3/4
      K4=strtgr+(NGRID-strtgr)*95/100
      IF(NGRID.LT.5)THEN
        K1=100
        K2=100
        K3=100
        K4=100
      ENDIF

c Commence calculation.
c For tmrt calculations it is sufficient to take only cube-grid
c points.  With tmrt strtgr = first point on cube, else 1.
      DO 200  I = strtgr,NGRID

      IF(I.EQ.K1)THEN
        CALL USRMSG(' 1/4 complete','  ','-')
      ELSEIF(I.EQ.K2)THEN
        CALL USRMSG(' 1/2 complete','  ','-')
      ELSEIF(I.EQ.K3)THEN
        CALL USRMSG(' 3/4 complete','  ','-')
      ELSEIF(I.EQ.K4)THEN
        CALL USRMSG(' Almost complete','  ','-')
      ENDIF

      IF (IDS .NE. IDG(I))  THEN
        IDS = IDG(I)
        anal=.true.
      ENDIF

c anal = true if an analytical procedure will add new information.
       if ((iarect(ids,ids).eq.-12).or.(iarect(ids,ids).eq.0)) then
         if (anal) then
           do 100 jsur=1,nsurmf
             call analytic(ids,jsur)
100        continue
           anal=.false.
         endif
       endif

       if ((iarect(ids,ids).eq.-12).or.(iarect(ids,ids).eq.0)) goto 200

      X0 = XC(I)*STMAT(1,1,IDS) + YC(I)*STMAT(2,1,IDS) +
     &                ZC(I)*STMAT(3,1,IDS) + STMAT(4,1,IDS)
      Y0 = XC(I)*STMAT(1,2,IDS) + YC(I)*STMAT(2,2,IDS) +
     &                ZC(I)*STMAT(3,2,IDS) + STMAT(4,2,IDS)
      Z0 = XC(I)*STMAT(1,3,IDS) + YC(I)*STMAT(2,3,IDS) +
     &                ZC(I)*STMAT(3,3,IDS) + STMAT(4,3,IDS)
      T4 = XC(I)*STMAT(1,4,IDS) + YC(I)*STMAT(2,4,IDS) +
     &                ZC(I)*STMAT(3,4,IDS) + STMAT(4,4,IDS)

      X0 = X0/T4
      Y0 = Y0/T4
      Z0 = Z0/T4

      DO 300 J = 1,NBUB

      X1 = X0 + XBB(J)
      Y1 = Y0 + YBB(J)
      Z1 = Z0 - ZBB(J)

      X2 = X1*SRMAT(1,1,IDS) + Y1*SRMAT(2,1,IDS) +
     &             Z1*SRMAT(3,1,IDS) + SRMAT(4,1,IDS)
      Y2 = X1*SRMAT(1,2,IDS) + Y1*SRMAT(2,2,IDS) +
     &             Z1*SRMAT(3,2,IDS) + SRMAT(4,2,IDS)
      Z2 = X1*SRMAT(1,3,IDS) + Y1*SRMAT(2,3,IDS) +
     &             Z1*SRMAT(3,3,IDS) + SRMAT(4,3,IDS)
      T4 = X1*SRMAT(1,4,IDS) + Y1*SRMAT(2,4,IDS) +
     &             Z1*SRMAT(3,4,IDS) + SRMAT(4,4,IDS)

      X2 = X2/T4
      Y2 = Y2/T4
      Z2 = Z2/T4

      CALL  OBSTRT(IDS,XC(I),YC(I),ZC(I),0,X2,Y2,Z2,
     &                    ISRJ,X3,Y3,Z3)

      IF (ISRJ .EQ. 0)  GOTO  300

      FF(IDS,ISRJ) = FF(IDS,ISRJ) + ZBB(J)*FFS

  300 CONTINUE
  200 CONTINUE
      RETURN

      END


C+++++++++++++++++++++ ANALYTIC
C  Calculates the view factor from surface i1 to
C  surface j1 analytically.

      subroutine analytic(i1,j1)

C  i1,j1  indices of the two surfaces

#include "building.h"
      COMMON/G1M/XM(MTV),YM(MTV),ZM(MTV),NSURM,JVNM(MSM,MV),
     &           NVERM(MSM),NTVM
      COMMON /PLNEC/ PEQN(4,MSM), PCG(3,MSM), BOX(3,2,MSM),
     &               STMAT(4,4,MSM), SRMAT(4,4,MSM)
      COMMON /CELLS/ NCELL(MSM), NCPT(MSM), IGFACT
      COMMON /VISIB/ IVIS(MSM,MSM), IARECT(MSM,MSM)
      COMMON /FORMF/ FF(MSM,MSM), FFS

C  equal   is a function that is true if its two arguments are
C          equal within 0.00001
C  paralf ]
C  perpf  ] Are two functions that calculate the viewfactors
C         ] for parallel and perpendicular surfaces respectively

      logical equal
      integer i1,j1
      integer j,k
      real paralf,perpf
      real xyz1(3,mv),xyz2(3,mv)
      real nn1(3),nn2(3),x1,y1,z1,norm
      real PI,dy,a1,b1,c1,d1,a2,b2,c2,d2,g
      real xmin1(3),xmax1(3),xmin2(3),xmax2(3)
      PI = 4.0 * ATAN(1.0)

      if (i1.eq.j1) goto 1000

C  Read in vertices of surfaces first.
      do 10 j=1,nverm(i1)
        xyz1(1,j)=xm(jvnm(i1,j))
        xyz1(2,j)=ym(jvnm(i1,j))
        xyz1(3,j)=zm(jvnm(i1,j))
10    continue

      do 20 j=1,nverm(j1)
        xyz2(1,j)=xm(jvnm(j1,j))
        xyz2(2,j)=ym(jvnm(j1,j))
        xyz2(3,j)=zm(jvnm(j1,j))
20    continue

C Read in the normal-vectors to the two surfaces.
      nn1(1)=peqn(1,i1)
      nn1(2)=peqn(2,i1)
      nn1(3)=peqn(3,i1)
      nn2(1)=peqn(1,j1)
      nn2(2)=peqn(2,j1)
      nn2(3)=peqn(3,j1)

C Rotate both surfaces in standard position.
C This is: normal to face i1 along + z-axis and
C the edges along x or y.
C If surfaces are perpendicular then the normal to j1 will be
C along + x-axis.
C First rotate all vertices so that normal to surface i1
C parallel to + z-axis

C Rotate around z-axis.
      norm=(nn1(1)**2+nn1(2)**2)**0.5
      if (equal(0.0,norm)) goto 50
      do 30 j=1,nverm(i1)
        x1=(nn1(1)*xyz1(1,j)+nn1(2)*xyz1(2,j))/norm
        y1=(-nn1(2)*xyz1(1,j)+nn1(1)*xyz1(2,j))/norm
        xyz1(1,j)=x1
        xyz1(2,j)=y1
30    continue

      do 40 j=1,nverm(j1)
        x1=(nn1(1)*xyz2(1,j)+nn1(2)*xyz2(2,j))/norm
        y1=(-nn1(2)*xyz2(1,j)+nn1(1)*xyz2(2,j))/norm
        xyz2(1,j)=x1
        xyz2(2,j)=y1
40    continue

      x1=(nn1(1)*nn2(1)+nn1(2)*nn2(2))/norm
      y1=(-nn1(2)*nn2(1)+nn1(1)*nn2(2))/norm
      nn2(1)=x1
      nn2(2)=y1
      nn1(1)=norm**2
      nn1(2)=0.

50    continue

C Rotate around y-axis.
      do 60 j=1,nverm(i1)
        x1=(nn1(3)*xyz1(1,j)-nn1(1)*xyz1(3,j))
        z1=(nn1(1)*xyz1(1,j)+nn1(3)*xyz1(3,j))
        xyz1(1,j)=x1
        xyz1(3,j)=z1
60    continue

      do 70 j=1,nverm(j1)
        x1=(nn1(3)*xyz2(1,j)-nn1(1)*xyz2(3,j))
        z1=(nn1(1)*xyz2(1,j)+nn1(3)*xyz2(3,j))
        xyz2(1,j)=x1
        xyz2(3,j)=z1
70    continue

      x1=(nn1(3)*nn2(1)-nn1(1)*nn2(3))
      z1=(nn1(1)*nn2(1)+nn1(3)*nn2(3))
      nn2(1)=x1
      nn2(3)=z1
      nn1(3)=nn1(1)**2+nn1(3)**2
      nn1(1)=0

C Normal to i1 is in + z-direction.

C Rotate in x,y plane until edges are in right direction.
      if (iarect(i1,j1).eq.1) then

C  Surfaces are parallel, align first edge of i1 with + x axis.
        nn1(1)=xyz1(1,2)-xyz1(1,1)
        nn1(2)=xyz1(2,2)-xyz1(2,1)
      else

C  Surfaces are perpendicular, align normal to j1 with + x axis.
        nn1(1)=nn2(1)
        nn1(2)=nn2(2)
        nn1(3)=nn2(3)
      endif
      norm=(nn1(1)**2+nn1(2)**2)**0.5
      if (equal(0.0,norm)) goto 200
      do 110 j=1,nverm(i1)
        x1=(nn1(1)*xyz1(1,j)+nn1(2)*xyz1(2,j))/norm
        y1=(-nn1(2)*xyz1(1,j)+nn1(1)*xyz1(2,j))/norm
        xyz1(1,j)=x1
        xyz1(2,j)=y1
110   continue

      do 120 j=1,nverm(j1)
        x1=(nn1(1)*xyz2(1,j)+nn1(2)*xyz2(2,j))/norm
        y1=(-nn1(2)*xyz2(1,j)+nn1(1)*xyz2(2,j))/norm
        xyz2(1,j)=x1
        xyz2(2,j)=y1
120   continue

      x1=(nn1(1)*nn2(1)+nn1(2)*nn2(2))/norm
      y1=(-nn1(2)*nn2(1)+nn1(1)*nn2(2))/norm
      nn2(1)=x1
      nn2(2)=y1

C If surfaces parallel then normal to surface i1 in + z-direction.
C If surfaces perp. then surface j1 has normal in + x-direction.
200   continue

C  Now translate i1 to z=0 plane
      do 210 j=1,nverm(j1)
        xyz2(3,j)=xyz2(3,j)-xyz1(3,1)
210   continue
      do 220 j=1,nverm(i1)
        xyz1(3,j)=0.0
220   continue

C If planes are perpend. translate surface j1 to x=0 plane
C (iarect(i1,j1) is either 2 or -2).
      if (iarect(i1,j1).ne.1) then
        do 230 j=1,nverm(i1)
          xyz1(1,j)=xyz1(1,j)-xyz2(1,1)
230     continue
        do 240 j=1,nverm(j1)
          xyz2(1,j)=0.0
240     continue
      endif

C Determine the input values for analytical functions.
      do 310 k=1,3
        xmin1(k)=xyz1(k,1)
        xmax1(k)=xmin1(k)
310   continue
      do 330 j=1,nverm(i1)
        do 320 k=1,3
          if (xyz1(k,j).lt.xmin1(k)) then
            xmin1(k)=xyz1(k,j)
          endif
          if (xyz1(k,j).gt.xmax1(k)) then
            xmax1(k)=xyz1(k,j)
          endif
320     continue
330   continue

      do 340 k=1,3
        xmin2(k)=xyz2(k,1)
        xmax2(k)=xmin2(k)
340   continue
      do 360 j=1,nverm(j1)
        do 350 k=1,3
          if (xyz2(k,j).lt.xmin2(k)) then
            xmin2(k)=xyz2(k,j)
          endif
          if (xyz2(k,j).gt.xmax2(k)) then
            xmax2(k)=xyz2(k,j)
          endif
350     continue
360   continue

C No negative values allowed for paralf and Perpf. so translate
C both surfaces so that the smallest y-value is 0.0.
      dy=xmin1(2)
      if (xmin2(2).lt.dy) then
        dy=xmin2(2)
      endif

      a1=xmin1(2)-dy
      b1=xmax1(2)-dy
      c1=xmin1(1)
      d1=xmax1(1)

      if (iarect(i1,j1).eq.1) then
        a2=xmin2(2)-dy
        b2=xmax2(2)-dy
        c2=xmin2(1)
        d2=xmax2(1)
        g=xmin2(3)
        ff(i1,j1)=paralf(a1,b1,c1,d1,a2,b2,c2,d2,g)
      elseif (iarect(i1,j1).eq.2) then
        a2=xmin2(2)-dy
        b2=xmax2(2)-dy
        c2=xmin2(3)
        d2=xmax2(3)
        ff(i1,j1)=perpf(a1,b1,c1,d1,a2,b2,c2,d2)
      elseif (iarect(i1,j1).eq.-2) then
        a2=xmin2(2)-dy
        b2=xmax2(2)-dy
        c2=xmin2(3)
        d2=xmax2(3)
        if (c1.lt.0.0) then
          c1=0.0
        endif
        if (c2.lt.0.0) then
          c2=0.0
        endif
        ff(i1,j1)=perpf(a1,b1,c1,d1,a2,b2,c2,d2)
      endif

C In main program ff(i,j) is divided by  Pi* real(igfact*igfact).
C Therefore we multiply here with the same factor.
      ff(i1,j1)=ff(i1,j1)*PI* real(igfact*igfact)
1000  continue
      return
      end


C+++++++++++++++++++ BUB
c Calculate points on hemisphere.
c We work with spherical coordinates th1 (= angle with z-axis)
c and ang (= angle with x-axis in x-y-plane).
c The bubble is divided in m rings of equal angle 'th1'.
c These rings will NOT have equal area.
c The largest ring will be divided into n patches.
c The area of the patches is calculated and the other ring-areas
c are divided by this patch area to yield ni(i) (=number of patches
c in ring i). The number of patches is than scaled up to the same
c number as the procedure with rings of equal area had.
c Because of rounding errors the patches of the smaller rings
c can be too large or too small. To minimise this error we calculate
c 'patar' and re-calculate the number of patches per ring.
      SUBROUTINE  BUB(N)


#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON /BUBXYZ1/ NPATCH, NBUB
      COMMON /BUBXYZ2/ XBB(11000), YBB(11000), ZBB(11000)
      COMMON /FORMF/ FF(MSM,MSM), FFS
      REAL ni(100)
      real th1,th2,dth,ang,dang
      real patar,scale
      integer n,ntot
      PI = 4.0 * ATAN(1.0)
      
      IF(N.GT.MPATCH) GOTO 999
      nbub=0
      m=n/4
      dth=(pi/2)/m

c Calculate the number of points in each ring on the bubble.
c Each point should represent (about) the same area on the bubble.
      ni(1)=n
      ntot=n
      do 100 i=2,m
        th1=(pi/2)-(i-1)*dth
        th2=th1-dth
        ni(i)=ni(1)*(cos(th2)-cos(th1))/cos((pi/2)-dth)
        ntot=ntot+int(ni(i)+0.5)
100   continue

c Total number of points should be approx. the same as
c with method with rings of equal area.
      scale=(1.0*m*n/ntot)
      do 110 i=1,m
        ni(i)=1.0*int(ni(i)*scale+0.5)
110   continue

c Patar is the area of the patches in the smallest ring.
c To minimise the area error on the bubble, all the patch-areas
c are made as close to 'patar' as possible.
      patar=2*pi*(1.0-cos(dth))/ni(m)
      do 120 i=m,1,-1
        th1=(pi/2)-(i-1)*dth
        th2=th1-dth
        ni(i)=int(2*pi*(cos(th2)-cos(th1))/patar+0.5)
120   continue

c 'ang' and 'th' represent the point on the bubble with
c  radius=1.0, in spherical-coordinates.
c  The point (XBB,YBB,ZBB) is in the middle of a patch.
      do 210 i=1,m
        dang=2*pi/ni(i)
        th1=(pi/2)-(i-1)*dth-0.5*dth
        nn=int(ni(i))
        do 200 j=1,nn
          NBUB = NBUB + 1
          ang=(j-1)*dang+0.5*dang
          XBB(NBUB) = sin(th1)*cos(ang)
          YBB(NBUB) = sin(th1)*sin(ang)
          ZBB(NBUB) = cos(th1)
200     continue
210   continue

      IF (NBUB .GT. (mpatch**2+1000)) GOTO  999
      FFS = 2 * PI / NBUB

      RETURN

  999 CONTINUE
      CALL USRMSG(' Number of patches exceeds MPATCH',
     &            ' as defined in building.h ','W')
      RETURN
      END


C+++++++++++++++++++++++++ OBSTRT
C OBSTRT find which surface a vector hits.
      SUBROUTINE  OBSTRT(IDI,XI,YI,ZI,IDJ,XJ,YJ,ZJ,ISUR,XO,YO,ZO)

C     IDI  -  SURFACE (XI,YI,ZI) IS ON
C     (XI,YI,ZI)  -  3D CO-ORDINATES
C     IDJ  -  SURFACE (XJ,YJ,ZJ) IS ON (OR 0 IF NOT ON SURFACE)
C         IDJ = 0 THEN WE ARE CONSIDERING THE VECTOR PROJECTED
C         THRU THE POINT (XJ,YJ,ZJ) FROM (XI,YI,ZI)
C     (XJ,YJ,ZJ)  -  3D CO-ORDINATES
C     ISUR  -  RETURNED VALUE
C          = 0  IF NO OBSTRUCTION (IDJ#0) OR INTERSECTION (IDJ=0)
C          = K  IF SURFACE K OBSTRUCTS
C     (XO,YO,ZO) - POINT ON SURFACE WHERE PROJECTION HITS

#include "building.h"
      COMMON /PLNEC/ PEQN(4,MSM), PCG(3,MSM), BOX(3,2,MSM),
     &               STMAT(4,4,MSM), SRMAT(4,4,MSM)
      COMMON /SURPNT/ ISPNT(6,MSM), GAREA(MSM), EMISI(MS)
      COMMON /VISIB/ IVIS(MSM,MSM), IARECT(MSM,MSM)
      COMMON /ZZZ/  ZMIN(MSM,MSM), ZMAX(MSM,MSM)
      COMMON /TOTAL/ nsurmf, NGRID, NTRPOL

      DIMENSION  IAPNT(100),APOL(1000)

      LOGICAL  INSIDEP

      ISUR = 0
      D = 100000.0

      DO 100 K = 1,nsurmf

      IF (K .EQ. IDI)  GOTO  100
      IF (K .EQ. IDJ)  GOTO  100
      IF (IVIS(IDI,K) .EQ. -1)  GOTO  100
      IF (ZMIN(IDI,K) .GT. D)  GOTO  100

      F = XJ - XI
      G = YJ - YI
      H = ZJ - ZI

      DENOM = SQRT(F*F + G*G + H*H)

      F = F/DENOM
      G = G/DENOM
      H = H/DENOM

      DENOM = PEQN(1,K)*F + PEQN(2,K)*G + PEQN(3,K)*H

      IF (ABS(DENOM) .LT. 0.0001)  GOTO  100
      T = (PEQN(4,K) - PEQN(1,K)*XI - PEQN(2,K)*YI - PEQN(3,K)*ZI)/DENOM
      if (t.le.0.0) goto 100

      XK = XI + F*T
      YK = YI + G*T
      ZK = ZI + H*T

      A = XI - XK
      B = YI - YK
      C = ZI - ZK
      DK = A*A + B*B + C*C

      IF (DK .GT. D)  GOTO  100

      XT = XK*STMAT(1,1,K) + YK*STMAT(2,1,K) + ZK*STMAT(3,1,K) +
     &      STMAT(4,1,K)
      YT = XK*STMAT(1,2,K) + YK*STMAT(2,2,K) + ZK*STMAT(3,2,K) +
     &       STMAT(4,2,K)
      T4 = XK*STMAT(1,4,K) + YK*STMAT(2,4,K) + ZK*STMAT(3,4,K) +
     &      STMAT(4,4,K)
      XT = XT/T4
      YT = YT/T4

      IF (XT .LT. BOX(1,1,K))  GOTO  100
      IF (XT .GT. BOX(1,2,K))  GOTO  100
      IF (YT .LT. BOX(2,1,K))  GOTO  100
      IF (YT .GT. BOX(2,2,K))  GOTO  100

      INPK = ISPNT(2,K)
      CALL  GETPOL(INPK,NA,NAP,IAPNT,APOL)

      IF (.NOT.(INSIDEP(XT,YT,NA,IAPNT,APOL)))  GOTO  100
      D = DK
      ISUR = K
      XO = XK
      YO = YK
      ZO = ZK

  100 CONTINUE

      RETURN
      END


C+++++++++++++++++++++++ PARALF
c Function to calculate analytically the view factors
c of two parallel rectangular surfaces.

c g is the distance between the planes.
c The vetrex coordinates for surfaces 1 and 2 are:
c                 (a1,c1) (b1,c1) (b1,d1) (a1,d1)
c                 (a2,c2) (b2,c2) (b2,d2) (a2,d2)

      real function paralf(a1,b1,c1,d1,a2,b2,c2,d2,g)
      real a1,b1,c1,d1,a2,b2,c2,d2,g
      real s,fo
      real pi
      PI = 4.0 * ATAN(1.0)
      s=(b1-a1)*(d1-c1)
      if (s.gt.0.) then
        fo=f1(a1,c1,a2,c2,g)-f1(a1,c1,a2,d2,g)
        fo=fo-f1(a1,c1,b2,c2,g)+f1(a1,c1,b2,d2,g)
        fo=fo-f1(a1,d1,a2,c2,g)+f1(a1,d1,a2,d2,g)
        fo=fo+f1(a1,d1,b2,c2,g)-f1(a1,d1,b2,d2,g)
        fo=fo-f1(b1,c1,a2,c2,g)+f1(b1,c1,a2,d2,g)
        fo=fo+f1(b1,c1,b2,c2,g)-f1(b1,c1,b2,d2,g)
        fo=fo+f1(b1,d1,a2,c2,g)-f1(b1,d1,a2,d2,g)
        fo=fo-f1(b1,d1,b2,c2,g)+f1(b1,d1,b2,d2,g)
        paralf=fo/(pi*s)
      else
        paralf=0
      endif
      return
      end

C+++++++++++++++++++++++ F1
      real function f1(x1,y1,x2,y2,g)
      real x1,y1,x2,y2,g
      real u,uu,v,vv,ww,fi,uk,vk,gk
      u=x2-x1
      v=y2-y1
      uk=u**2
      vk=v**2
      gk=g**2
      uu=sqrt(uk+gk)
      vv=sqrt(vk+gk)
      ww=(gk+uk+vk)/(gk+vk)
      fi=u*vv*atan(u/vv)+v*uu*atan(v/uu)-gk*alog(ww)/2
      f1=fi/2
      return
      end


C+++++++++++++++++++++++++ PERPENDICULAR
c Function perpendicular.f

c  Calculates analytically the viewfactors of two rectangular
c  surfaces that are in perpendicular planes.
c  The vetrex coordinates for surfaces 1 and 2 are:
c                 (a1,c1,0)  (b1,c1,0)  (b1,d1,0)  (a1,d1,0)
c                 (a2,d1,c2) (b2,d1,c2) (b2,d1,d2) (a2,d1,d2)

      real function perpf(a1,b1,c1,d1,a2,b2,c2,d2)
      real a1,b1,c1,d1,a2,b2,c2,d2
      real s,fo
      real pi
      pi = 4.0 * ATAN(1.0)
      s=(b1-a1)*(d1-c1)
      if (s.gt.0.) then
        fo=f2(a1,c1,a2,c2)-f2(a1,c1,a2,d2)
        fo=fo-f2(a1,c1,b2,c2)+f2(a1,c1,b2,d2)
        fo=fo-f2(a1,d1,a2,c2)+f2(a1,d1,a2,d2)
        fo=fo+f2(a1,d1,b2,c2)-f2(a1,d1,b2,d2)
        fo=fo-f2(b1,c1,a2,c2)+f2(b1,c1,a2,d2)
        fo=fo+f2(b1,c1,b2,c2)-f2(b1,c1,b2,d2)
        fo=fo+f2(b1,d1,a2,c2)-f2(b1,d1,a2,d2)
        fo=fo-f2(b1,d1,b2,c2)+f2(b1,d1,b2,d2)
        perpf=fo/(pi*s)
      else
        perpf=0.
      endif
      return
      end

C+++++++++++++++++++++ F2
      real function f2(x1,y1,x2,y2)
      real x1,y1,x2,y2
      real u,fii,uk,vk
      u=x2-x1
      vk=y1**2+y2**2
      uk=u**2
      if ((uk+vk).le.0.0000001) then
        fii=0.
      elseif (vk.le.0.0000001) then
        fii=0.5*uk*alog(uk)
      else
        fii=0.5*(uk-vk)*alog(uk+vk)
        fii=fii+2*u*sqrt(vk)*atan(u/sqrt(vk))
      endif
      f2=fii/4
      return
      end
