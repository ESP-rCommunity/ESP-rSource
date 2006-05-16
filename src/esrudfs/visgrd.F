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

C This file contains the following routines:
C (related to image creation via gridding, point containment tests)

C POLY   Makes the flow vectors into polygons.
C PbyP     Iterates through each flow vector to determine which mesh points
C          it covers.
C MAKBIT   Creates a bitmap ASCII file of the flow vectors.
C MAKXPM   Creates a XPM (colour bitmap) ASCII file of the flow vectors.
C TRNSF1   Translates and rotates the site co-ordinate system to a local
C          face co-ordinate system.
C TRNSF2   Translates and rotates a flow vector from site co-ordinates
C          to the local co-ordinates of the site under consideration.
C MESH     Places a mesh on the surface under consideration - used for
C          point containment tests.
C PANGS    Computes the object body surface azimuth (PAZI), elevation
C          (PELV) and local X-Axis tilt (PTILT).
C POINT2   Decides if a specified point is within or outwith an
C          arrow polygon.

C ********************* POLY *********************
C POLY - creates a polygon for each flow vector.  The flow vectors are defined
C  by start and end points in cartesian co-ordinates.  It is necessary
C  to represent these by polygons to perform the point containment tests.
      SUBROUTINE POLY(i,j,k)
#include "cfd.h"

      COMMON/arpoly/XS(7),ZS(7),bXmin,bXmax,bZmin,bZmax
      common/TvecXYZ/TvecXbeg(ntcelx,ntcely,ntcelz),
     &               TvecXend(ntcelx,ntcely,ntcelz),
     &               TvecZbeg(ntcelx,ntcely,ntcelz),
     &               TvecZend(ntcelx,ntcely,ntcelz)
      COMMON/VISSET/imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal

C XS(i) and ZS(i) are the face co-ordinates of vertex i.  These are passed 
C back to the calling routine through common/arpoly/XS(4),ZS(4).
C This analagous ish' treatment of shading polygons.  In ish, the shading 
C polygon is calculated in DECOMP.

C Calculate the angle of the arrow.  We are working in face
C co-ordinates, so the y-direction is not important.
      PI = 4.0 * ATAN(1.0)
      SHORT = 1.0E-8
      deltaX = abs( TvecXend(i,j,k)-TvecXbeg(i,j,k) )
      deltaZ = abs( TvecZend(i,j,k)-TvecZbeg(i,j,k) )
      if(deltaX.lt.SHORT) then
        angle = PI/2.
      else
        angle = atan (deltaZ/deltaX)
      endif

C Make the size of the arrow head proportional to the length of the flow vector.
C vecL is the length of the vector.
C headL is the length of the arrow head.
C headW is the thickness of the arrow head.
C The length of arrow heads displayed can be altered by the user with the
C `arrow head length scaling factor', hLscal.
C The thickness of arrow heads displayed can be altered by the user 
C with the `arrow head thickness scaling factor', hWscal.
      vecL = sqrt(deltaX**2 + deltaZ**2)

C Check vector length and if really small then draw a blob.
      if (vecL.lt.0.02) then
        xcent= (TvecXend(i,j,k)+TvecXbeg(i,j,k))/2.
        zcent= (TvecZend(i,j,k)+TvecZbeg(i,j,k))/2.
        do 5 iv=1,7
          XS(iv) = xcent + (sin(iv*2.*PI/7.) * 0.005)
          ZS(iv) = zcent + (cos(iv*2.*PI/7.) * 0.005)
 5      continue
      else
        headL = (vecL/2.0)*hLscal
        headW = (vecL/5.0)*hWscal

C THICK is the thickness of the flow vector.
C The thickness of the vectors displayed can be altered by the user 
C with the `thickness scaling factor', TSCAL.
        THICK = (vecL/12.0)*TSCAL

C Determine the co-ordinates of the four corners of the rectangle.  The
C centre-line of the rectangle is along the line defined by the arrow start
C and end points.  The thickness of the rectangle is defined by THICK.

C First step: calculate X and Z distance between arrow start and end points
C and vertices of rectangle.
C ADDX = X-distance between arrow start and end points and rectangle vertices.
C ADDZ = Z-distance between arrow start and end points and rectangle vertices.
        ADDX = THICK * sin(angle)
        ADDZ = THICK * cos(angle)
        HLdx = headL * cos(angle)
        HLdz = headL * sin(angle)
        HWdx = headW * sin(angle)
        HWdz = headW * cos(angle)

C Second step: compute the co-ordinates for the four vertices that define
C the rectangle.  The computation depends of the direction of the flow,
C since the point-containment test requires that the first point be the 
C left-most, with the others arranged clockwise.

C For flow up and to the right:
        IF( (TvecXend(i,j,k) .ge. TvecXbeg(i,j,k)) .and.
     &         (TvecZend(i,j,k) .ge. TvecZbeg(i,j,k)) ) THEN
          XS(1) = TvecXbeg(i,j,k) - ADDX
          ZS(1) = TvecZbeg(i,j,k) + ADDZ
          XS(2) = TvecXend(i,j,k) - ADDX - HLdx
          ZS(2) = TvecZend(i,j,k) + ADDZ - HLdz
          XS(3) = TvecXend(i,j,k) - HLdx - HWdx
          ZS(3) = TvecZend(i,j,k) - HLdz + HWdz
          XS(4) = TvecXend(i,j,k)
          ZS(4) = TvecZend(i,j,k)
          XS(5) = TvecXend(i,j,k) - HLdx + HWdx
          ZS(5) = TvecZend(i,j,k) - HLdz - HWdz
          XS(6) = TvecXend(i,j,k) + ADDX - HLdx
          ZS(6) = TvecZend(i,j,k) - ADDZ - HLdz
          XS(7) = TvecXbeg(i,j,k) + ADDX
          ZS(7) = TvecZbeg(i,j,k) - ADDZ
        ENDIF

C For flow down and to the right:
        IF( (TvecXend(i,j,k) .ge. TvecXbeg(i,j,k)) .and.
     &         (TvecZend(i,j,k) .lt. TvecZbeg(i,j,k)) ) THEN
          XS(1) = TvecXbeg(i,j,k) - ADDX
          ZS(1) = TvecZbeg(i,j,k) - ADDZ
          XS(2) = TvecXbeg(i,j,k) + ADDX
          ZS(2) = TvecZbeg(i,j,k) + ADDZ
          XS(3) = TvecXend(i,j,k) + ADDX - HLdx
          ZS(3) = TvecZend(i,j,k) + ADDZ + HLdz
          XS(4) = TvecXend(i,j,k) - HLdx + HWdx
          ZS(4) = TvecZend(i,j,k) + HLdz + HWdz
          XS(5) = TvecXend(i,j,k)
          ZS(5) = TvecZend(i,j,k)
          XS(6) = TvecXend(i,j,k) - HLdx - HWdx
          ZS(6) = TvecZend(i,j,k) + HLdz - HWdz
          XS(7) = TvecXend(i,j,k) - ADDX - HLdx
          ZS(7) = TvecZend(i,j,k) - ADDZ + HLdz
        ENDIF

C For flow up and to the left:
        IF( (TvecXend(i,j,k) .lt. TvecXbeg(i,j,k)) .and.
     &      (TvecZend(i,j,k) .ge. TvecZbeg(i,j,k)) ) THEN
          XS(1) = TvecXend(i,j,k)
          ZS(1) = TvecZend(i,j,k)
          XS(2) = TvecXend(i,j,k) + HLdx + HWdx
          ZS(2) = TvecZend(i,j,k) - HLdz + HWdz
          XS(3) = TvecXend(i,j,k) + ADDX + HLdx
          ZS(3) = TvecZend(i,j,k) + ADDZ - HLdz
          XS(4) = TvecXbeg(i,j,k) + ADDX
          ZS(4) = TvecZbeg(i,j,k) + ADDZ
          XS(5) = TvecXbeg(i,j,k) - ADDX
          ZS(5) = TvecZbeg(i,j,k) - ADDZ
          XS(6) = TvecXend(i,j,k) - ADDX + HLdx
          ZS(6) = TvecZend(i,j,k) - ADDZ - HLdz
          XS(7) = TvecXend(i,j,k) + HLdx - HWdx
          ZS(7) = TvecZend(i,j,k) - HLdz - HWdz
        ENDIF

C For flow down and to the left:
        IF( (TvecXend(i,j,k) .lt. TvecXbeg(i,j,k)) .and.
     &       (TvecZend(i,j,k) .lt. TvecZbeg(i,j,k)) ) THEN
          XS(1) = TvecXend(i,j,k)
          ZS(1) = TvecZend(i,j,k)
          XS(2) = TvecXend(i,j,k) + HLdx - HWdx
          ZS(2) = TvecZend(i,j,k) + HLdz + HWdz
          XS(3) = TvecXend(i,j,k) - ADDX + HLdx
          ZS(3) = TvecZend(i,j,k) + ADDZ + HLdz
          XS(4) = TvecXbeg(i,j,k) - ADDX
          ZS(4) = TvecZbeg(i,j,k) + ADDZ
          XS(5) = TvecXbeg(i,j,k) + ADDX
          ZS(5) = TvecZbeg(i,j,k) - ADDZ
          XS(6) = TvecXend(i,j,k) + ADDX + HLdx
          ZS(6) = TvecZend(i,j,k) - ADDZ + HLdz
          XS(7) = TvecXend(i,j,k) + HLdx + HWdx
          ZS(7) = TvecZend(i,j,k) + HLdz - HWdz
        ENDIF
      ENDIF

C Determine the `box' which bounds the polygon.  The point containment
C tests will be restricted to mesh points within this box, in order
C to reduce computation time.
      bXmin = 1000.
      bXmax = -1000.
      bZmin = 1000.
      bZmax = -1000.
      do 10 ibox=1,7
        bXmin = AMIN1(XS(ibox),bXmin)
        bXmax = AMAX1(XS(ibox),bXmax)
        bZmin = AMIN1(ZS(ibox),bZmin)
        bZmax = AMAX1(ZS(ibox),bZmax)
   10 continue

      return
      end


C ********************* PbyP *********************
C PbyP - Examines each flow vector in turn to determine which
C  mesh points it covers.  Sets the contents of the IOSHD array.
      SUBROUTINE PbyP(ICOMP,IM,ISUR,ILAY,ISCALAR,IBR)
#include "building.h"
#include "cfd.h"
#include "flowvis.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/VISSET/imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal
      COMMON/arpoly/XS(7),ZS(7),bXmin,bXmax,bZmin,bZmax
      COMMON/TRNSF/A(5),B(5),C(3),XOR,YOR,ZOR
      COMMON/BIT1/TOSHD(MGRXZ,MGRXZ)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/DIVcf/DIVXcf,DIVZcf
      common/vecXYZ/vecXbeg(ntcelx,ntcely,ntcelz),
     1              vecXend(ntcelx,ntcely,ntcelz),
     2              vecYbeg(ntcelx,ntcely,ntcelz),
     3              vecYend(ntcelx,ntcely,ntcelz),
     4              vecZbeg(ntcelx,ntcely,ntcelz),
     5              vecZend(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/LOCAGE/AGEf(ntcelx,ntcely,ntcelz)
      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
     1              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)
      common/flvimg/imgtyp,IMOPTS
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)

      CHARACTER H*72,LBITrt*12,BITITL*5
      LOGICAL OK,LINVOL

C If contaminants are to be plotted remember which 
      IF(ISCALAR.GT.3)THEN
        ICTM=ISCALAR-3
        ISCALAR=3
      ENDIF

C Clear the image by setting all pixels blank in bit array.
      do 5 Jmesh=1,NOZ
        do 6 Imesh=1,NOX+60
          TOSHD(Imesh,Jmesh)=-1000.
    6   continue
    5 continue

C Set up the do loops according to which surface is being viewed.
C East or west view.
      if(ISUR.eq.2 .or. ISUR.eq.4) then
         L1max = NJM1
         L2max = NKM1

C North or south view.
      elseif(ISUR.eq.1 .or. ISUR.eq.3) then
         L1max = NIM1
         L2max = NKM1

C Top or bottom view.
      elseif(ISUR.eq.5 .or. ISUR.eq.6) then
         L1max = NIM1
         L2max = NJM1
      endif

C Iterate through each flow vector to determine which grids of the mesh 
C it covers.
C Scalar (temp) is for point then l=left, r=right, b=below, a=above.
C Need to interpolate back-to-front, set ilrm for this, if looking 
C from south, west or top.
      if (ISUR.eq.3.or.ISUR.eq.4.or.ISUR.eq.5) then
        ilrm=-1
      else
        ilrm=1
      endif
      do 15 L1=2,L1max
        do 25 L2=2,L2max
          if (L1.eq.2) then
            Il=L1+((1-ilrm)/2)
            Ir=L1+((1+ilrm)/2)
          elseif (L1.eq.L1max) then
            Il=L1-((1+ilrm)/2)
            Ir=L1-((1-ilrm)/2)
          else
            Il=L1-(1*ilrm)
            Ir=L1+(1*ilrm)
          endif
          if (L2.eq.2) then
            Jb=L2
            Ja=L2+1
          elseif (L2.eq.L2max) then
            Jb=L2-1
            Ja=L2
          else
            Jb=L2-1
            Ja=L2+1
          endif
          if(ISUR.eq.2 .or. ISUR.eq.4) then

C Check if cell is blocked.
            IBLK=0
            do 100 IV=1,NVOL(ICFD)
              if (IVTYPE(IV,ICFD).eq.30) then
                OK=LINVOL(ILAY,L1,L2,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                if (OK) IBLK=IBLK+1
              endif
 100        continue
            if (IBLK.gt.0) then
              temp=-992.0
              templ=-992.0
              tempr=-992.0
              tempb=-992.0
              tempa=-992.0
              templb=-992.0
              templa=-992.0
              temprb=-992.0
              tempra=-992.0
            else
            CALL POLY(ILAY,L1,L2)
            if (ISCALAR.eq.0) then
              temp=crowxyz(vecXbeg(ILAY,L1,L2),vecYbeg(ILAY,L1,L2),
     &                     vecZbeg(ILAY,L1,L2),vecXend(ILAY,L1,L2),
     &                     vecYend(ILAY,L1,L2),vecZend(ILAY,L1,L2))
              templ=crowxyz(vecXbeg(ILAY,Il,L2),vecYbeg(ILAY,Il,L2),
     &                      vecZbeg(ILAY,Il,L2),vecXend(ILAY,Il,L2),
     &                      vecYend(ILAY,Il,L2),vecZend(ILAY,Il,L2))
              tempr=crowxyz(vecXbeg(ILAY,Ir,L2),vecYbeg(ILAY,Ir,L2),
     &                      vecZbeg(ILAY,Ir,L2),vecXend(ILAY,Ir,L2),
     &                      vecYend(ILAY,Ir,L2),vecZend(ILAY,Ir,L2))
              tempb=crowxyz(vecXbeg(ILAY,L1,Jb),vecYbeg(ILAY,L1,Jb),
     &                      vecZbeg(ILAY,L1,Jb),vecXend(ILAY,L1,Jb),
     &                      vecYend(ILAY,L1,Jb),vecZend(ILAY,L1,Jb))
              tempa=crowxyz(vecXbeg(ILAY,L1,Ja),vecYbeg(ILAY,L1,Ja),
     &                      vecZbeg(ILAY,L1,Ja),vecXend(ILAY,L1,Ja),
     &                      vecYend(ILAY,L1,Ja),vecZend(ILAY,L1,Ja))
              templb=crowxyz(vecXbeg(ILAY,Il,Jb),vecYbeg(ILAY,Il,Jb),
     &                       vecZbeg(ILAY,Il,Jb),vecXend(ILAY,Il,Jb),
     &                       vecYend(ILAY,Il,Jb),vecZend(ILAY,Il,Jb))
              templa=crowxyz(vecXbeg(ILAY,Il,Ja),vecYbeg(ILAY,Il,Ja),
     &                       vecZbeg(ILAY,Il,Ja),vecXend(ILAY,Il,Ja),
     &                       vecYend(ILAY,Il,Ja),vecZend(ILAY,Il,Ja))
              temprb=crowxyz(vecXbeg(ILAY,Ir,Jb),vecYbeg(ILAY,Ir,Jb),
     &                       vecZbeg(ILAY,Ir,Jb),vecXend(ILAY,Ir,Jb),
     &                       vecYend(ILAY,Ir,Jb),vecZend(ILAY,Ir,Jb))
              tempra=crowxyz(vecXbeg(ILAY,Ir,Ja),vecYbeg(ILAY,Ir,Ja),
     &                       vecZbeg(ILAY,Ir,Ja),vecXend(ILAY,Ir,Ja),
     &                       vecYend(ILAY,Ir,Ja),vecZend(ILAY,Ir,Ja))
            elseif (ISCALAR.eq.1) then
              temp=Tf(ILAY,L1,L2)
              templ=Tf(ILAY,Il,L2)
              tempr=Tf(ILAY,Ir,L2)
              tempb=Tf(ILAY,L1,Jb)
              tempa=Tf(ILAY,L1,Ja)
              templb=Tf(ILAY,Il,Jb)
              templa=Tf(ILAY,Il,Ja)
              temprb=Tf(ILAY,Ir,Jb)
              tempra=Tf(ILAY,Ir,Ja)
            elseif (ISCALAR.eq.2) then
              temp=AGEf(ILAY,L1,L2)
              templ=AGEf(ILAY,Il,L2)
              tempr=AGEf(ILAY,Ir,L2)
              tempb=AGEf(ILAY,L1,Jb)
              tempa=AGEf(ILAY,L1,Ja)
              templb=AGEf(ILAY,Il,Jb)
              templa=AGEf(ILAY,Il,Ja)
              temprb=AGEf(ILAY,Ir,Jb)
              tempra=AGEf(ILAY,Ir,Ja)
            elseif (ISCALAR.eq.3) then
              temp=POLCONCf(ICTM,ILAY,L1,L2)
              templ=POLCONCf(ICTM,ILAY,Il,L2)
              tempr=POLCONCf(ICTM,ILAY,Ir,L2)
              tempb=POLCONCf(ICTM,ILAY,L1,Jb)
              tempa=POLCONCf(ICTM,ILAY,L1,Ja)
              templb=POLCONCf(ICTM,ILAY,Il,Jb)
              templa=POLCONCf(ICTM,ILAY,Il,Ja)
              temprb=POLCONCf(ICTM,ILAY,Ir,Jb)
              tempra=POLCONCf(ICTM,ILAY,Ir,Ja)
            endif
            Xmax=XU(ILAY+1)
            Xmin=XU(ILAY)
            Ymax=YV(L1+1)
            Ymin=YV(L1)
            Zmax=ZW(L2+1)
            Zmin=ZW(L2)
            endif
          elseif(ISUR.eq.1 .or. ISUR.eq.3) then

C Check if cell is blocked.
            IBLK=0
            do 101 IV=1,NVOL(ICFD)
              if (IVTYPE(IV,ICFD).eq.30) then
                OK=LINVOL(L1,ILAY,L2,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                if (OK) IBLK=IBLK+1
              endif
 101        continue
            if (IBLK.gt.0) then
              temp=-992.0
              templ=-992.0
              tempr=-992.0
              tempb=-992.0
              tempa=-992.0
              templb=-992.0
              templa=-992.0
              temprb=-992.0
              tempra=-992.0
            else
            CALL POLY(L1,ILAY,L2)
            if (ISCALAR.eq.0) then
              temp=crowxyz(vecXbeg(L1,ILAY,L2),vecYbeg(L1,ILAY,L2),
     &                     vecZbeg(L1,ILAY,L2),vecXend(L1,ILAY,L2),
     &                     vecYend(L1,ILAY,L2),vecZend(L1,ILAY,L2))
              templ=crowxyz(vecXbeg(Il,ILAY,L2),vecYbeg(Il,ILAY,L2),
     &                      vecZbeg(Il,ILAY,L2),vecXend(Il,ILAY,L2),
     &                      vecYend(Il,ILAY,L2),vecZend(Il,ILAY,L2))
              tempr=crowxyz(vecXbeg(Ir,ILAY,L2),vecYbeg(Ir,ILAY,L2),
     &                      vecZbeg(Ir,ILAY,L2),vecXend(Ir,ILAY,L2),
     &                      vecYend(Ir,ILAY,L2),vecZend(Ir,ILAY,L2))
              tempb=crowxyz(vecXbeg(L1,ILAY,Jb),vecYbeg(L1,ILAY,Jb),
     &                      vecZbeg(L1,ILAY,Jb),vecXend(L1,ILAY,Jb),
     &                      vecYend(L1,ILAY,Jb),vecZend(L1,ILAY,Jb))
              tempa=crowxyz(vecXbeg(L1,ILAY,Ja),vecYbeg(L1,ILAY,Ja),
     &                      vecZbeg(L1,ILAY,Ja),vecXend(L1,ILAY,Ja),
     &                      vecYend(L1,ILAY,Ja),vecZend(L1,ILAY,Ja))
              templb=crowxyz(vecXbeg(Il,ILAY,Jb),vecYbeg(Il,ILAY,Jb),
     &                       vecZbeg(Il,ILAY,Jb),vecXend(Il,ILAY,Jb),
     &                       vecYend(Il,ILAY,Jb),vecZend(Il,ILAY,Jb))
              templa=crowxyz(vecXbeg(Il,ILAY,Ja),vecYbeg(Il,ILAY,Ja),
     &                       vecZbeg(Il,ILAY,Ja),vecXend(Il,ILAY,Ja),
     &                       vecYend(Il,ILAY,Ja),vecZend(Il,ILAY,Ja))
              temprb=crowxyz(vecXbeg(Ir,ILAY,Jb),vecYbeg(Ir,ILAY,Jb),
     &                       vecZbeg(Ir,ILAY,Jb),vecXend(Ir,ILAY,Jb),
     &                       vecYend(Ir,ILAY,Jb),vecZend(Ir,ILAY,Jb))
              tempra=crowxyz(vecXbeg(Ir,ILAY,Ja),vecYbeg(Ir,ILAY,Ja),
     &                       vecZbeg(Ir,ILAY,Ja),vecXend(Ir,ILAY,Ja),
     &                       vecYend(Ir,ILAY,Ja),vecZend(Ir,ILAY,Ja))
            elseif (ISCALAR.eq.1) then
              temp=Tf(L1,ILAY,L2)
              templ=Tf(Il,ILAY,L2)
              tempr=Tf(Ir,ILAY,L2)
              tempb=Tf(L1,ILAY,Jb)
              tempa=Tf(L1,ILAY,Ja)
              templb=Tf(Il,ILAY,Jb)
              templa=Tf(Il,ILAY,Ja)
              temprb=Tf(Ir,ILAY,Jb)
              tempra=Tf(Ir,ILAY,Ja)
            elseif (ISCALAR.eq.2) then
              temp=AGEf(L1,ILAY,L2)
              templ=AGEf(Il,ILAY,L2)
              tempr=AGEf(Ir,ILAY,L2)
              tempb=AGEf(L1,ILAY,Jb)
              tempa=AGEf(L1,ILAY,Ja)
              templb=AGEf(Il,ILAY,Jb)
              templa=AGEf(Il,ILAY,Ja)
              temprb=AGEf(Ir,ILAY,Jb)
              tempra=AGEf(Ir,ILAY,Ja)
            elseif (ISCALAR.eq.3) then
              temp=POLCONCf(ICTM,L1,ILAY,L2)
              templ=POLCONCf(ICTM,Il,ILAY,L2)
              tempr=POLCONCf(ICTM,Ir,ILAY,L2)
              tempb=POLCONCf(ICTM,L1,ILAY,Jb)
              tempa=POLCONCf(ICTM,L1,ILAY,Ja)
              templb=POLCONCf(ICTM,Il,ILAY,Jb)
              templa=POLCONCf(ICTM,Il,ILAY,Ja)
              temprb=POLCONCf(ICTM,Ir,ILAY,Jb)
              tempra=POLCONCf(ICTM,Ir,ILAY,Ja)
            endif
            Xmax=XU(L1+1)
            Xmin=XU(L1)
            Ymax=YV(ILAY+1)
            Ymin=YV(ILAY)
            Zmax=ZW(L2+1)
            Zmin=ZW(L2)
            endif
          elseif(ISUR.eq.5 .or. ISUR.eq.6) then

C Check if cell is blocked.
            IBLK=0
            do 102 IV=1,NVOL(ICFD)
              if (IVTYPE(IV,ICFD).eq.30) then
                OK=LINVOL(L1,L2,ILAY,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                if (OK) IBLK=IBLK+1
              endif
 102        continue
            if (IBLK.gt.0) then
              temp=-992.0
              templ=-992.0
              tempr=-992.0
              tempb=-992.0
              tempa=-992.0
              templb=-992.0
              templa=-992.0
              temprb=-992.0
              tempra=-992.0
            else
            CALL POLY(L1,L2,ILAY)
            if (ISCALAR.eq.0) then
              temp=crowxyz(vecXbeg(L1,L2,ILAY),vecYbeg(L1,L2,ILAY),
     &                     vecZbeg(L1,L2,ILAY),vecXend(L1,L2,ILAY),
     &                     vecYend(L1,L2,ILAY),vecZend(L1,L2,ILAY))
              templ=crowxyz(vecXbeg(Il,L2,ILAY),vecYbeg(Il,L2,ILAY),
     &                      vecZbeg(Il,L2,ILAY),vecXend(Il,L2,ILAY),
     &                      vecYend(Il,L2,ILAY),vecZend(Il,L2,ILAY))
              tempr=crowxyz(vecXbeg(Ir,L2,ILAY),vecYbeg(Ir,L2,ILAY),
     &                      vecZbeg(Ir,L2,ILAY),vecXend(Ir,L2,ILAY),
     &                      vecYend(Ir,L2,ILAY),vecZend(Ir,L2,ILAY))
              tempb=crowxyz(vecXbeg(L1,Jb,ILAY),vecYbeg(L1,Jb,ILAY),
     &                      vecZbeg(L1,Jb,ILAY),vecXend(L1,Jb,ILAY),
     &                      vecYend(L1,Jb,ILAY),vecZend(L1,Jb,ILAY))
              tempa=crowxyz(vecXbeg(L1,Ja,ILAY),vecYbeg(L1,Ja,ILAY),
     &                      vecZbeg(L1,Ja,ILAY),vecXend(L1,Ja,ILAY),
     &                      vecYend(L1,Ja,ILAY),vecZend(L1,Ja,ILAY))
              templb=crowxyz(vecXbeg(Il,Jb,ILAY),vecYbeg(Il,Jb,ILAY),
     &                       vecZbeg(Il,Jb,ILAY),vecXend(Il,Jb,ILAY),
     &                       vecYend(Il,Jb,ILAY),vecZend(Il,Jb,ILAY))
              templa=crowxyz(vecXbeg(Il,Ja,ILAY),vecYbeg(Il,Ja,ILAY),
     &                       vecZbeg(Il,Ja,ILAY),vecXend(Il,Ja,ILAY),
     &                       vecYend(Il,Ja,ILAY),vecZend(Il,Ja,ILAY))
              temprb=crowxyz(vecXbeg(Ir,Jb,ILAY),vecYbeg(Ir,Jb,ILAY),
     &                       vecZbeg(Ir,Jb,ILAY),vecXend(Ir,Jb,ILAY),
     &                       vecYend(Ir,Jb,ILAY),vecZend(Ir,Jb,ILAY))
              tempra=crowxyz(vecXbeg(Ir,Ja,ILAY),vecYbeg(Ir,Ja,ILAY),
     &                       vecZbeg(Ir,Ja,ILAY),vecXend(Ir,Ja,ILAY),
     &                       vecYend(Ir,Ja,ILAY),vecZend(Ir,Ja,ILAY))
            elseif (ISCALAR.eq.1) then
              temp=Tf(L1,L2,ILAY)
              templ=Tf(Il,L2,ILAY)
              tempr=Tf(Ir,L2,ILAY)
              tempb=Tf(L1,Jb,ILAY)
              tempa=Tf(L1,Ja,ILAY)
              templb=Tf(Il,Jb,ILAY)
              templa=Tf(Il,Ja,ILAY)
              temprb=Tf(Ir,Jb,ILAY)
              tempra=Tf(Ir,Ja,ILAY)
            elseif (ISCALAR.eq.2) then
              temp=AGEf(L1,L2,ILAY)
              templ=AGEf(Il,L2,ILAY)
              tempr=AGEf(Ir,L2,ILAY)
              tempb=AGEf(L1,Jb,ILAY)
              tempa=AGEf(L1,Ja,ILAY)
              templb=AGEf(Il,Jb,ILAY)
              templa=AGEf(Il,Ja,ILAY)
              temprb=AGEf(Ir,Jb,ILAY)
              tempra=AGEf(Ir,Ja,ILAY)
            elseif (ISCALAR.eq.3) then
              temp=POLCONCf(ICTM,L1,L2,ILAY)
              templ=POLCONCf(ICTM,Il,L2,ILAY)
              tempr=POLCONCf(ICTM,Ir,L2,ILAY)
              tempb=POLCONCf(ICTM,L1,Jb,ILAY)
              tempa=POLCONCf(ICTM,L1,Ja,ILAY)
              templb=POLCONCf(ICTM,Il,Jb,ILAY)
              templa=POLCONCf(ICTM,Il,Ja,ILAY)
              temprb=POLCONCf(ICTM,Ir,Jb,ILAY)
              tempra=POLCONCf(ICTM,Ir,Ja,ILAY)
            endif
            Xmax=XU(L1+1)
            Xmin=XU(L1)
            Ymax=YV(L2+1)
            Ymin=YV(L2)
            Zmax=ZW(ILAY+1)
            Zmin=ZW(ILAY)
            endif
          endif

C Translate and rotate the X points (We do not need Y coords as we are 
C rotating/ transforming into the Y plane).
          if (imgtyp.eq.2) then
            Xmax=(Xmax-XOR)*A(1)+(Ymax-YOR)*A(2)-(Zmax-ZOR)*A(3)+
     &           (Ymax-YOR)*A(4)+(Zmax-ZOR)*A(5)
            Xmin=(Xmin-XOR)*A(1)+(Ymin-YOR)*A(2)-(Zmin-ZOR)*A(3)+
     &           (Ymin-YOR)*A(4)+(Zmin-ZOR)*A(5)

C Translate and rotate the Z points.
            Zmax=(Xmax-XOR)*C(1)-(Ymax-YOR)*C(2)+(Zmax-ZOR)*C(3)
            Zmin=(Xmin-XOR)*C(1)-(Ymin-YOR)*C(2)+(Zmin-ZOR)*C(3)
            XmaxT=amax1(Xmax,Xmin)
            XminT=amin1(Xmax,Xmin)
            ZmaxT=amax1(Zmax,Zmin)
            ZminT=amin1(Zmax,Zmin)

C Convert coords to pixels.
            IP1=NINT(XminT/DIVXcf)
            IP2=NINT(XmaxT/DIVXcf)
            IQ1=NINT(ZminT/DIVZcf)
            IQ2=NINT(ZmaxT/DIVZcf)

C Check not out of range.
            IP1=max0(1,IP1)
            IP2=min0(NOX,IP2)
            IQ1=max0(1,IQ1)
            IQ2=min0(NOZ,IQ2)

C Assign background colour, interpolating between values for surrounding 8 
C control volumes.
            IQmid=(IQ1+IQ2)/2
            IPmid=(IP1+IP2)/2
            do 31 Jmesh=IQ1,IQ2
              do 41 Imesh=IP1,IP2

C If the value of TOSHD is greater than -1000 then a vector is overlapping 
C this rectangle, therefore do not assign a colour to this point.
                if (TOSHD(Imesh,Jmesh).lt.-999.5)then

C Draw a light grey line representing the boundary of the ctl volume.
                  if (Jmesh.eq.IQ1.or.Imesh.eq.IP1.or.
     &                Jmesh.eq.IQ2.or.Imesh.eq.IP2.or.
     &                Jmesh.eq.1.or.Imesh.eq.1) then
                    TOSHD(Imesh,Jmesh)=-992.0

C Interpolate in one of four directions.
C Only if colouring in background.
                  elseif (IBR.eq.2) then
                    if (Jmesh.lt.IQmid.and.Imesh.lt.IPmid) then
                      Tp1=((temp+templ)/2.)
                      Tp2=((temp+tempb)/2.)
                      Tp3=((temp+templb+tempb+templ)/4.)
                      xfrac=abs(real(Imesh-IP1)/real(IPmid-IP1))
                      yfrac=abs(real(Jmesh-IQ1)/real(IQmid-IQ1))
                      Tp4=Tp1+(temp-Tp1)*xfrac
                      Tp5=Tp3+(Tp2-Tp3)*xfrac
                      TOSHD(Imesh,Jmesh)=Tp5+(Tp4-Tp5)*yfrac
                    elseif (Jmesh.ge.IQmid.and.Imesh.lt.IPmid) then
                      Tp1=((temp+templ)/2.)
                      Tp2=((temp+tempa)/2.)
                      Tp3=((temp+templa+tempa+templ)/4.)
                      xfrac=abs(real(Imesh-IP1)/real(IPmid-IP1))
                      yfrac=abs(real(IQ2-Jmesh)/real(IQ2-IQmid))
                      Tp4=Tp1+(temp-Tp1)*xfrac
                      Tp5=Tp3+(Tp2-Tp3)*xfrac
                      TOSHD(Imesh,Jmesh)=Tp5+(Tp4-Tp5)*yfrac
                    elseif (Jmesh.lt.IQmid.and.Imesh.ge.IPmid) then
                      Tp1=((temp+tempr)/2.)
                      Tp2=((temp+tempb)/2.)
                      Tp3=((temp+temprb+tempb+tempr)/4.)
                      xfrac=abs(real(IP2-Imesh)/real(IP2-IPmid))
                      yfrac=abs(real(Jmesh-IQ1)/real(IQmid-IQ1))
                      Tp4=Tp1+(temp-Tp1)*xfrac
                      Tp5=Tp3+(Tp2-Tp3)*xfrac
                      TOSHD(Imesh,Jmesh)=Tp5+(Tp4-Tp5)*yfrac
                    elseif (Jmesh.ge.IQmid.and.Imesh.ge.IPmid) then
                      Tp1=((temp+tempr)/2.)
                      Tp2=((temp+tempa)/2.)
                      Tp3=((temp+tempra+tempa+tempr)/4.)
                      xfrac=abs(real(IP2-Imesh)/real(IP2-IPmid))
                      yfrac=abs(real(IQ2-Jmesh)/real(IQ2-IQmid))
                      Tp4=Tp1+(temp-Tp1)*xfrac
                      Tp5=Tp3+(Tp2-Tp3)*xfrac
                      TOSHD(Imesh,Jmesh)=Tp5+(Tp4-Tp5)*yfrac
                    else
                      TOSHD(Imesh,Jmesh)=temp
                    endif
                  endif
                endif
 41           continue
 31         continue
          endif

C Convert coords of vector bounding box to pixels (rounding outwards).
          IP1=max(INT(bXmin/DIVXcf),1)
          IP2=min(INT(bXmax/DIVXcf)+1,NOX)
          IQ1=max(INT(bZmin/DIVZcf),1)
          IQ2=min(INT(bZmax/DIVZcf)+1,NOZ)

C Determine which mesh points are covered by this flow vector by
C performing a point containment test.
          do 30 Jmesh=IQ1,IQ2
            do 40 Imesh=IP1,IP2

C Calculate coordinates of mesh point.
              XOP=DIVXcf*(float(Imesh)-0.5)
              ZOP=DIVZcf*(float(Jmesh)-0.5)
              CALL POINT2(XOP,ZOP,IANS)
              IF(IANS.EQ.1) then
                if (imgtyp.eq.1) then
                  TOSHD(Imesh,Jmesh)=1.
                elseif (imgtyp.eq.2) then
                  if (IBR.eq.1) then
                    TOSHD(Imesh,Jmesh)=temp
                  else
                    TOSHD(Imesh,Jmesh)=-995.0
                  endif
                endif
              endif
 40         continue
 30       continue
 25     continue
 15   continue

      return
      end


C ********************* MAKBIT *********************
C MAKBIT - creates an X-bitmap ASCII file of the flow vectors based on
C  the contents of the IOSHD array.  Currently, only a black and
C  white image is created.  The format of the bitmap file was
C  deduced by examining files created by the `bitmap' program.
      SUBROUTINE MAKBIT
#include "flowvis.h"

      common/FILEP/IFIL

      COMMON/VISSET/imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal
      COMMON/VISFIL/LVEC,LBIT
      COMMON/BIT1/TOSHD(MGRXZ,MGRXZ)

      character*72 LVEC,LBIT

C Open file.
      IUNIT=IFIL+1
      call EFOPSEQ(IUNIT,LBIT,4,IER)

C Write the first two lines of the bitmap file.  These contain the width
C and height of the bitmap image, measured in pixels.
      write(IUNIT,1000) NOX,NOZ
 1000 format('#define flow_width ',i4,/,'#define flow_height ',i4)

C Write the third line of the bitmap file.  This is the start of the `c'
C static char array.
      write(IUNIT,'(a)') 'static char flow_bits[] = {'

C Calculate the value of each member of the static char array.
C Each member is expressed as four digits, the first two being `0x'.
C The `0x' probably indicates colour, but this can be investigated later.
C The final two digits are a hexidecimal number.  This number controls
C the display of eight pixels in a row, progressing from left to right.
C The first pixel in each grouping is worth 1 (in decimal), the second
C worth 2, the third worth 4, ...., the eighth worth 128.  The final
C two digits of the static char array is a summation of the value
C of the eight pixels, expressed in hexidecimal.
C Examples: `0xff' turns on all eight pixels in the grouping.
C           `0x01' turns on only the leftmost pixel in the grouping.
C           `0x03' turns on the two leftmost pixels in the grouping.

C Iterate through mesh domain, examining each 8-pixel grouping in turn.
C Start at the top left and progress to the right and down.  This direction
C is dictated by the required format of the bitmap file.
C NUMCHX is the number of 8-pixel groupings along the horizontal direction.
C ISUM is the running sum indicating which bits of the 8-pixel grouping are on.
      numchx = NOX/8
      do 10 Jmesh=NOZ,1,-1
        do 15 Ichar=1,numchx
          isum = 0
          do 20 ibit = 1,8

C Calculate the x-coordinate of the mesh point under consideration.
            Imesh = (Ichar-1)*8 + ibit

C Update the sum for this 8-pixel grouping.
            if (TOSHD(Imesh,Jmesh).gt.0.5) then
              iadd=1
            else
              iadd=0
            endif
            isum = isum + iadd*2**(ibit-1)
   20     continue

C Two write & format statements required, because f77's hexadecimal
C format (Z) drops leading zeros, and the leading zeros are important
C to the bitmap file, ie. write `0x01', not `0x 1'.
          if(isum.le.15) then
            write(IUNIT,'(a,z1,a)') '0x0',isum,','
          else
            write(IUNIT,'(a,z2,a)') '0x',isum,','
          endif
   15   continue
   10 continue

C Close the static char array.
      write(IUNIT,'(a)') '};'

C Close bitmap file.
      CALL ERPFREE(IUNIT,ISTAT)

      return
      end

C ********************* MAKXPM *********************
C MAKXPM - creates an X-picmap ASCII file of the flow vectors based on
C  the contents of the IOSHD array.  The format of the bitmap file was
C  deduced by examining files created by the `bitmap' program.
      SUBROUTINE MAKXPM(TMIN,TMAX)
#include "flowvis.h"

      common/FILEP/IFIL

      COMMON/VISSET/imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal
      COMMON/VISFIL/LVEC,LBIT
      COMMON/BIT1/TOSHD(MGRXZ,MGRXZ)
      COMMON/BIT2/BITITL

      character LVEC*72,LBIT*72,quote*1,comma*1,bitrow(MGRXZ)*1
      character HCTAB(84)*6, HTAB(84)*1,t1*1,t5*5,BITITL*5

      dimension LETTER(11,15)

      data HCTAB/'FFFFFF','000000','777777','0000FF','000DFF',
     &           '0019FF','0026FF','0033FF','0040FF','004CFF',
     &           '0059FF','0066FF','0073FF','0080FF','008CFF',
     &           '0099FF','00A6FF','00B3FF','00BFFF','00CCFF',
     &           '00D9FF','00E6FF','00F2FF','00FFFF','00FFF2',
     &           '00FFE6','00FFD9','00FFCC','00FFBF','00FFB3',
     &           '00FFA6','00FF99','00FF8C','00FF80','00FF73',
     &           '00FF66','00FF59','00FF4C','00FF40','00FF33',
     &           '00FF26','00FF19','00FF0D','00FF00','0DFF00',
     &           '1AFF00','26FF00','33FF00','40FF00','4CFF00',
     &           '59FF00','66FF00','73FF00','80FF00','8CFF00',
     &           '99FF00','A6FF00','B3FF00','BFFF00','CCFF00',
     &           'D9FF00','E6FF00','F2FF00','FFFF00','FFF200',
     &           'FFE600','FFD900','FFCC00','FFBF00','FFB300',
     &           'FFA600','FF9900','FF8C00','FF8000','FF7300',
     &           'FF6600','FF5900','FF4D00','FF4000','FF3300',
     &           'FF2600','FF1A00','FF0D00','FF0000'/
      data HTAB/'.','#','$','*','+',',','-','0','1','2','3','4','5','6',
     &          '7','8','9',':',';','<','=','>','?','@','A','B','C','D',
     &          'E','F','G','H','I','J','K','L','M','N','O','P','Q','R',
     &          'S','T','U','V','W','X','Y','Z','[',']','^','_','a','b',
     &          'c','d','e','f','g','h','i','j','k','l','m','n','o','p',
     &          'q','r','s','t','u','v','w','x','y','z','{','|','}','~'/
      quote=char(34)
      comma=char(44)


C Add a scale to the rhs of the image.
C Draw grey box (-992.0=grey) around interpolated colour bar.
      do 100 I=NOX+4,NOX+56
        do 110 J=40,NOZ-40
          TOSHD(NOX+3,J)=-992.0
          TOSHD(NOX+57,J)=-992.0
          TOSHD(I,J)=((TMAX-TMIN)*(J-50)/(NOZ-100)) + TMIN
 110    continue
        TOSHD(I,40)=-992.0
        TOSHD(I,NOZ-40)=-992.0
 100  continue

C Write maximum value.
      STmax=TMAX
      STmin=TMIN
      IEX=0
 99   if (abs(STmax).gt.10.0) then
        STmax=STmax/10.
        IEX=IEX-1
        goto 99
      elseif (abs(STmax).lt.0.000001) then
        continue
      elseif (abs(STmax).lt.1.0) then
        STmax=STmax*10.
        IEX=IEX+1
        goto 99
      endif

C Write maximum value.
      write (t5,'(f5.2)') STmax
      do 300 K=1,5
        write (t1,'(a)') t5(K:K)

C Write text (-995.0=black).
        call ANNOTATE(t1,LETTER)
        do 200 I=1,11
          do 210 J=1,15
            if (LETTER(I,J).ne.0) then
              IX=NOX+5+I+(K-1)*10
              IZ=NOZ-42-J
              TOSHD(IX,IZ)=-995.0
            endif
 210      continue
 200    continue
 300  continue

C Write minimum value.
      STmin=TMIN*(10.0**IEX)
      write (t5,'(f5.2)') STmin
      do 301 K=1,5
        write (t1,'(a)') t5(K:K)

C Write text (-1000.0=white).
        call ANNOTATE(t1,LETTER)
        do 201 I=1,11
          do 211 J=1,15
            if (LETTER(I,J).ne.0) then
              IX=NOX+5+I+(K-1)*10
              IZ=58-J
              TOSHD(IX,IZ)=-1000.0
            endif
 211      continue
 201    continue
 301  continue

C Write other values.
      do 333 KK=1,3
        write (t5,'(f5.2)') STmin+(STmax-STmin)*(real(KK)/4.)
        do 303 K=1,5
          write (t1,'(a)') t5(K:K)

C Write text (-995.0=black).
          call ANNOTATE(t1,LETTER)
          do 203 I=1,11
            do 213 J=1,15
              if (LETTER(I,J).ne.0) then
                IX=NOX+5+I+(K-1)*10
                IZ=int(58.0+(real(NOZ)-100.0)*(real(KK)/4.)-real(J))
                TOSHD(IX,IZ)=-995.0
              endif
 213        continue
 203      continue
 303    continue
 333  continue

C Write exponential if needed.
      if (IEX.ne.0) then
        if (abs(IEX).lt.10) then
          write (t5,'(1x,a,SP,I2)') 'E',IEX*(-1)
        else
          write (t5,'(1x,a,SP,I3)') 'E',IEX*(-1)
        endif
        do 302 K=1,5
          write (t1,'(a)') t5(K:K)

C Write text (-995.0=black).
          call ANNOTATE(t1,LETTER)
          do 202 I=1,11
            do 212 J=1,15
              if (LETTER(I,J).ne.0) then
                IX=NOX+5+I+(K-1)*10
                IZ=35-J
                TOSHD(IX,IZ)=-995.0
              endif
 212        continue
 202      continue
 302    continue
      endif

C Write title.
      do 304 K=1,5
        write (t1,'(a)') BITITL(K:K)

C Write text (-995.0=black).
        call ANNOTATE(t1,LETTER)
        do 204 I=1,11
          do 214 J=1,15
            if (LETTER(I,J).ne.0) then
              IX=NOX+5+I+(K-1)*11
              IZ=NOZ-22-J
              TOSHD(IX,IZ)=-995.0
            endif
 214      continue
 204    continue
 304  continue


C Open file.
      IUNIT=IFIL+1
      call EFOPSEQ(IUNIT,LBIT,4,IER)

C Write the first two lines of the bitmap file.  These contain the width
C and height of the bitmap image, measured in pixels.
      write(IUNIT,'(a)') '/* XPM */'
      write(IUNIT,'(a)') 'static char *test[] = {'
      write(IUNIT,'(a)') '/* width height num_colors chars_per_pixel */'
      write(IUNIT,'(a,4I5,2a)') quote,NOX+60,NOZ,84,1,quote,comma
      write(IUNIT,'(a)') '/* colours */'
      do 5 I=1,84
        write(IUNIT,'(6a)') quote,HTAB(I),' c #',HCTAB(I),quote,comma
 5    continue
      write(IUNIT,'(a)') '/* pixels */'

C Loop through each row from top of image.
      do 10 Jmesh=NOZ,1,-1
        do 15 Imesh=1,NOX+60
          TCHK=TOSHD(Imesh,Jmesh)
          if (TCHK.gt.-990.0) then
            if (TMAX.le.TMIN) then
              bitrow(Imesh)=HTAB(1)
            elseif (TCHK.lt.TMIN) then
              bitrow(Imesh)=HTAB(4)
            elseif (TCHK.gt.TMAX) then
              bitrow(Imesh)=HTAB(84)
            else
              IC=int(80.0*((TCHK-TMIN)/(TMAX-TMIN)))+4
              bitrow(Imesh)=HTAB(IC)
            endif
          elseif (TCHK.gt.-994.0) then
            bitrow(Imesh)=HTAB(3)
          elseif (TCHK.gt.-999.0) then
            bitrow(Imesh)=HTAB(2)
          else
            bitrow(Imesh)=HTAB(1)
          endif
 15     continue

C Format needs to be altered if MGRXZ changed in flowvis.h.
        write(IUNIT,'(1600a)')quote,(bitrow(II),II=1,NOX+60),quote,comma
 10   continue

C Close the static char array.
      write(IUNIT,'(a)') '};'

C Close bitmap file.
      CALL ERPFREE(IUNIT,ISTAT)

      return
      end

C ********************* TRNSF1 *********************
C TRNSF1 - sets the coefficients of the transformation equations
C which translate and rotate the site coordinate system to the 
C local surface under consideration.
C It then computes the local co-ordinates of the face under
C consideration.
C This is a pared-down version of TRNSF1 from esruish/trnsf.F
      SUBROUTINE TRNSF1(IS)
#include "building.h"

      COMMON/PANGL/PAZI,PELV,PTILT
      COMMON/TRNSF/A(5),B(5),C(3),XOR,YOR,ZOR
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G1T/XFT(MV),ZFT(MV)
      DIMENSION  XX(MV),YY(MV),ZZ(MV)

      PI = 4.0 * ATAN(1.0)
      R=PI/180.

C Compute the surface azimuth, elevation, and X-axis tilt angles.
      CALL PANGS(IS)

C Compute the transformation coefficients.
      IQ=INT(PAZI/90.)
      IF(IQ.EQ.0)goto  1
      goto (1,1,2,2),IQ
    1 AL=(180.-PAZI)*R
      goto 3
    2 AL=-(PAZI-180.)*R
    3 BE=-PELV*R
      GA=PTILT*R
      A(1)=COS(GA)*COS(AL)
      A(2)=SIN(BE)*SIN(GA)*COS(AL)
      A(3)=COS(BE)*SIN(GA)*COS(AL)
      A(4)=COS(BE)*SIN(AL)
      A(5)=SIN(BE)*SIN(AL)
      B(1)=COS(GA)*SIN(AL)
      B(2)=SIN(BE)*SIN(GA)*SIN(AL)
      B(3)=COS(BE)*SIN(GA)*SIN(AL)
      B(4)=COS(BE)*COS(AL)
      B(5)=SIN(BE)*COS(AL)
      C(1)=SIN(GA)
      C(2)=SIN(BE)*COS(GA)
      C(3)=COS(BE)*COS(GA)
      IP1=JVN(IS,1)
      XOR=X(IP1)
      YOR=Y(IP1)
      ZOR=Z(IP1)

C Transform object body face coordinates.
      DO 10 I=1,NVER(IS)
        XX(I)=X(JVN(IS,I))
        YY(I)=Y(JVN(IS,I))
        ZZ(I)=Z(JVN(IS,I))

C Transformed X.
        XFT(I)=(XX(I)-XOR)*A(1)+(YY(I)-YOR)*A(2)-(ZZ(I)-ZOR)*A(3)+
     &         (YY(I)-YOR)*A(4)+(ZZ(I)-ZOR)*A(5)

C Transformed Z.
        ZFT(I)=(XX(I)-XOR)*C(1)-(YY(I)-YOR)*C(2)+(ZZ(I)-ZOR)*C(3)

   10 CONTINUE

      RETURN
      END


C ********************* TRNSF2 *********************
C TRNSF2 - translates and rotates a flow vector from site co-ordinates
C to the local co-ordinates of the site under consideration.
C It is based on esruish's TRNSF2 routine, but the sun angle
C calculations have been removed and the transformed co-ordinates
C are returned to the calling routine.
      SUBROUTINE TRNSF2
#include "cfd.h"

      COMMON/TRNSF/A(5),B(5),C(3),XOR,YOR,ZOR
      COMMON/VISSET/imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      common/vecXYZ/vecXbeg(ntcelx,ntcely,ntcelz),
     1              vecXend(ntcelx,ntcely,ntcelz),
     2              vecYbeg(ntcelx,ntcely,ntcelz),
     3              vecYend(ntcelx,ntcely,ntcelz),
     4              vecZbeg(ntcelx,ntcely,ntcelz),
     5              vecZend(ntcelx,ntcely,ntcelz)
      common/TvecXYZ/TvecXbeg(ntcelx,ntcely,ntcelz),
     &               TvecXend(ntcelx,ntcely,ntcelz),
     &               TvecZbeg(ntcelx,ntcely,ntcelz),
     &               TvecZend(ntcelx,ntcely,ntcelz)

      dimension Vstart(3),Vend(3)

C Vstart is the start point of the flow vector in site co-ordinates.
C Vend is the end point of the flow vector in site co-ordinates.

C Process each flow vector of the CFD domain.
      do 20 I=2,NIM1
        do 201 J=2,NJM1
          do 202 K=2,NKM1

C Copy site co-ordinates of flow vector to temporary variables
C and scale vector length.
            Vstart(1)=vecXbeg(i,j,k)
            Vend(1)  =vecXend(i,j,k)
            Vstart(2)=vecYbeg(i,j,k)
            Vend(2)  =vecYend(i,j,k)
            Vstart(3)=vecZbeg(i,j,k)
            Vend(3)  =vecZend(i,j,k)

C Scale vector length.
            do 104 IV=1,3
              SUM=Vstart(IV)+Vend(IV)
              DIF=(Vend(IV)-Vstart(IV))*vLscal
              Vstart(IV)=(SUM-DIF)/2.
              Vend(IV)  =(SUM+DIF)/2.
 104        continue

C Translate and rotate to get the X points.
            TvecXbeg(i,j,k)=(Vstart(1)-XOR)*A(1)+(Vstart(2)-YOR)*A(2)-
     &                      (Vstart(3)-ZOR)*A(3)+(Vstart(2)-YOR)*A(4)+
     &                      (Vstart(3)-ZOR)*A(5)
            TvecXend(i,j,k)=(Vend(1)-XOR)*A(1)+(Vend(2)-YOR)*A(2)-
     &          (Vend(3)-ZOR)*A(3)+(Vend(2)-YOR)*A(4)+(Vend(3)-ZOR)*A(5)

C Translate and rotate to get the Z points.
            TvecZbeg(i,j,k)=(Vstart(1)-XOR)*C(1)-(Vstart(2)-YOR)*C(2)+
     &                      (Vstart(3)-ZOR)*C(3)
            TvecZend(i,j,k)=(Vend(1)-XOR)*C(1)-(Vend(2)-YOR)*C(2)+
     &                      (Vend(3)-ZOR)*C(3)
 202      continue
 201    continue
 20   continue

      RETURN
      END


C ********************* MESH *********************
C MESH -places a mesh on the face under consideration and then computes the
C centre point co-ordinate (local co-ordinates) of each grid rectangle.
C It is based largely on esruish's MESH routine, but glazed surfaces
C have been excluded.
      SUBROUTINE MESH(IS)
#include "building.h"

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G1T/XFT(MV),ZFT(MV)
      COMMON/VISSET/imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal
      COMMON/DIVcf/DIVXcf,DIVZcf

C Find max and min X & Z values for this surface.
      XMAX=-1.E+10
      ZMAX=-1.E+10
      XMIN=1.E+10
      ZMIN=1.E+10
      NV=NVER(IS)
      DO 10 I=1,NV

C NV is set to NVER(IS), which is the number of vertices in the surface.
C So, this checks the face co-ordinates of each vertex point.
        XMAX=AMAX1(XMAX,XFT(I))
        ZMAX=AMAX1(ZMAX,ZFT(I))
        XMIN=AMIN1(XMIN,XFT(I))
        ZMIN=AMIN1(ZMIN,ZFT(I))
   10 CONTINUE

C Mesh length and height is given by:
C DXMESH equals the width of the surface and DZMESH the height.
      DXMESH=XMAX-XMIN
      DZMESH=ZMAX-ZMIN

C Establish grid interval for this surface.
C NOX and NOZ are the number of grid divisions specified by
C the user. DIVXcf and DIVZcf are the width and height of the shading grids.
      DIVXcf=DXMESH/NOX
      DIVZcf=DZMESH/NOZ

      RETURN
      END


C ********************* PANGS *********************
C PANGS - computes the object body surface azimuth (PAZI), elevation
C (PELV) and local X-Axis tilt (PTILT) where:
C This is copied from esruish' PANGS.
C   PAZI  - is the angle measured from north to the face normal,
C           clockwise positive.
C   PELV  - is the angle measured from the horizontal to the
C           face normal (upward facing horizontal surface 90 deg.,
C           downward facing horizontal surface -90 deg., 
C           vertical surface 0 deg.
C   PTILT - is the angle between the face X-axis and the horizontal
C           where the X-axis is the line joining the 1st and 2nd
C           face vertices.
      SUBROUTINE PANGS(IS)
#include "building.h"

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/PANGL/PAZI,PELV,PTILT

      r=atan(1.0)/45.0
      XSUM=0.
      YSUM=0.
      ZSUM=0.

C Compute azimuth and elevation angles for face 'IS'.
      NV=NVER(IS)
      DO 10 I=1,NV
        K=I+1
        IF(I.EQ.NV)K=1
        IP1=JVN(IS,I)
        IP2=JVN(IS,K)
        XSUM=XSUM+Y(IP1)*Z(IP2)-Z(IP1)*Y(IP2)
        YSUM=YSUM+Z(IP1)*X(IP2)-X(IP1)*Z(IP2)
        ZSUM=ZSUM+X(IP1)*Y(IP2)-Y(IP1)*X(IP2)
   10 CONTINUE
      PAZI=90.0
      if(abs(xsum).lt.0.00001)pazi=0.0
      IF(XSUM.LT.0.0)PAZI=-90.0
      PELV=90.0
      if(abs(zsum).lt.0.00001)pelv=0.0
      IF(ZSUM.LT.0.0)PELV=-90.0
      if(abs(ysum).lt.0.00001)goto 11
      PAZI=ATAN2(XSUM,YSUM)/R
   11 SRX2Y2=SQRT(XSUM*XSUM+YSUM*YSUM)
      IF(PAZI.LT.0.)PAZI=PAZI+360.
      if(abs(srx2y2).lt.0.00001)goto 12
      PELV=ATAN2(ZSUM,SRX2Y2)/R
   12 CONTINUE

C Now compute local X-axis tilt angle.
      IV1=JVN(IS,1)
      IV2=JVN(IS,2)
      dz=z(iv2)-z(iv1)
      DIST=sqrt((X(IV2)-X(IV1))**2+(Y(IV2)-Y(IV1))**2+
     &          (Z(IV2)-Z(IV1))**2)
      ptilt=asin(dz/dist)/r

C For the special case of a horizontal surface, the
C azimuth calculated above is zero. It is now reset to
C the azimuth of the plane comprising the first two
C vertices of the face and the same two vertices but with
C an adjusted Z value.
      XSUM=0.
      YSUM=0.
      ZSUM=0.
      DO 20 I=1,4
        IF(I.EQ.1)THEN
           IP1=JVN(IS,1)
           IP2=JVN(IS,2)
           XX1=X(IP1)
           XX2=X(IP2)
           YY1=Y(IP1)
           YY2=Y(IP2)
           ZZ1=Z(IP1)
           ZZ2=Z(IP2)
        ELSEIF(I.EQ.2)THEN
           IP1=JVN(IS,2)
           IP2=JVN(IS,2)
           XX1=X(IP1)
           XX2=X(IP2)
           YY1=Y(IP1)
           YY2=Y(IP2)
           ZZ1=Z(IP1)
           ZZ2=Z(IP2)+0.1
        ELSEIF(I.EQ.3)THEN
           IP1=JVN(IS,2)
           IP2=JVN(IS,1)
           XX1=X(IP1)
           XX2=X(IP2)
           YY1=Y(IP1)
           YY2=Y(IP2)
           ZZ1=Z(IP1)+0.1
           ZZ2=Z(IP2)+0.1
        ELSEIF(I.EQ.4)THEN
           IP1=JVN(IS,1)
           IP2=JVN(IS,1)
           XX1=X(IP1)
           XX2=X(IP2)
           YY1=Y(IP1)
           YY2=Y(IP2)
           ZZ1=Z(IP1)+0.1
           ZZ2=Z(IP2)
        ENDIF

        XSUM=XSUM+YY1*ZZ2-ZZ1*YY2
        YSUM=YSUM+ZZ1*XX2-XX1*ZZ2
        ZSUM=ZSUM+XX1*YY2-YY1*XX2
   20 CONTINUE
      PAZI=90.0
      if(abs(xsum).lt.0.00001)pazi=0.0
      IF(XSUM.LT.0.0)PAZI=-90.0
      if(abs(ysum).lt.0.00001)goto 13
      PAZI=ATAN2(XSUM,YSUM)/R
   13 IF(PAZI.LT.0.)PAZI=PAZI+360.

      RETURN
      END


C ********************* POINT2 *********************
C POINT2 - decides if a specified point (XX,ZZ) is within or 
C outwith any arrow polygon, as defined in COMMON/arpoly.
      SUBROUTINE POINT2(XX,ZZ,IANS)

      COMMON/arpoly/XS(7),ZS(7),bXmin,bXmax,bZmin,bZmax

      DIMENSION C(16)

      IANS=0
      NP=7
      DO 10 I=1,NP
        II=2*I-1
        C(II)=XS(I)
        II=II+1
        C(II)=ZS(I)
   10 CONTINUE
      LAST1=2*NP+1
      LAST2=2*NP+2
      C(LAST1)=XS(1)
      C(LAST2)=ZS(1)
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


C ********************* ANNOTATE *********************
C ANNOTATE - returns pixmap data for given character.
C LETTERS (x,y,1-10) = '0' through '9'
C LETTERS (x,y,11) = '.'
C LETTERS (x,y,12-37) = 'A' through 'Z'
C LETTERS (x,y,38) = '='
C LETTERS (x,y,39) = '-'
C LETTERS (x,y,40) = '+'
      SUBROUTINE ANNOTATE(t1,LETTER)

      character t1*1
      DIMENSION LETTERS(11,15,40),LETTER(11,15)
      DATA ((LETTERS(I,J,1),I=1,11),J=1,15)/0,1,1,1,1,0,0,0,0,0,0,
     &                                      1,1,1,1,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      1,1,1,1,1,1,0,0,0,0,0,
     &                                      0,1,1,1,1,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,2),I=1,11),J=1,15)/0,0,0,0,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,1,1,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,3),I=1,11),J=1,15)/0,1,1,1,1,0,0,0,0,0,0,
     &                                      1,1,1,1,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      0,0,1,1,1,0,0,0,0,0,0,
     &                                      0,1,1,1,0,0,0,0,0,0,0,
     &                                      1,1,1,0,0,0,0,0,0,0,0,
     &                                      1,1,1,1,1,1,0,0,0,0,0,
     &                                      1,1,1,1,1,1,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,4),I=1,11),J=1,15)/0,1,1,1,1,0,0,0,0,0,0,
     &                                      1,1,1,1,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      0,0,1,1,1,0,0,0,0,0,0,
     &                                      0,0,0,0,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      1,1,1,1,1,1,0,0,0,0,0,
     &                                      0,1,1,1,1,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,5),I=1,11),J=1,15)/0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,1,1,1,0,0,0,0,0,0,
     &                                      0,0,1,1,1,0,0,0,0,0,0,
     &                                      0,1,0,1,1,0,0,0,0,0,0,
     &                                      1,0,0,1,1,0,0,0,0,0,0,
     &                                      1,1,1,1,1,1,0,0,0,0,0,
     &                                      1,1,1,1,1,1,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,6),I=1,11),J=1,15)/0,1,1,1,1,1,0,0,0,0,0,
     &                                      0,1,1,1,1,1,0,0,0,0,0,
     &                                      0,1,0,0,0,0,0,0,0,0,0,
     &                                      1,1,1,1,1,0,0,0,0,0,0,
     &                                      1,1,1,1,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      0,0,0,0,1,1,0,0,0,0,0,
     &                                      1,1,1,1,1,1,0,0,0,0,0,
     &                                      0,1,1,1,1,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,7),I=1,11),J=1,15)/0,0,1,1,1,0,0,0,0,0,0,
     &                                      0,1,1,1,1,1,0,0,0,0,0,
     &                                      1,1,0,0,0,0,0,0,0,0,0,
     &                                      1,1,1,1,1,0,0,0,0,0,0,
     &                                      1,1,1,1,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      0,1,1,1,1,1,0,0,0,0,0,
     &                                      0,1,1,1,1,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,8),I=1,11),J=1,15)/1,1,1,1,1,1,0,0,0,0,0,
     &                                      1,1,1,1,1,1,0,0,0,0,0,
     &                                      0,0,0,1,1,1,0,0,0,0,0,
     &                                      0,0,1,1,1,0,0,0,0,0,0,
     &                                      0,0,1,1,0,0,0,0,0,0,0,
     &                                      0,1,1,1,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,9),I=1,11),J=1,15)/0,1,1,1,1,0,0,0,0,0,0,
     &                                      1,1,1,1,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      0,1,1,1,1,0,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      1,1,1,1,1,1,0,0,0,0,0,
     &                                      0,1,1,1,1,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,10),I=1,11),J=1,15)/0,1,1,1,1,0,0,0,0,0,0,
     &                                      1,1,1,1,1,0,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      1,1,0,0,1,1,0,0,0,0,0,
     &                                      1,1,1,1,1,1,0,0,0,0,0,
     &                                      0,1,1,1,1,1,0,0,0,0,0,
     &                                      0,0,0,0,1,1,0,0,0,0,0,
     &                                      1,1,1,1,1,0,0,0,0,0,0,
     &                                      0,1,1,1,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,11),I=1,11),J=1,15)/0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,12),I=1,11),J=1,15)/0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,1,1,1,1,0,0,0,0,0,
     &                                      0,0,1,1,1,1,0,0,0,0,0,
     &                                      0,0,1,0,0,1,0,0,0,0,0,
     &                                      0,1,1,0,0,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      1,1,0,0,0,0,1,1,0,0,0,
     &                                      1,1,0,0,0,0,1,1,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,13),I=1,11),J=1,15)/0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,14),I=1,11),J=1,15)/0,0,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      1,1,1,0,0,0,1,1,0,0,0,
     &                                      1,1,0,0,0,0,0,0,0,0,0,
     &                                      1,1,0,0,0,0,0,0,0,0,0,
     &                                      1,1,0,0,0,0,1,1,0,0,0,
     &                                      1,1,1,0,0,1,1,1,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,0,1,1,1,1,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,15),I=1,11),J=1,15)/0,1,1,1,1,1,0,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,0,0,1,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,1,1,1,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,16),I=1,11),J=1,15)/0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,17),I=1,11),J=1,15)/0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,18),I=1,11),J=1,15)/0,0,0,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,1,1,0,0,
     &                                      1,1,1,0,0,0,0,1,1,0,0,
     &                                      1,1,0,0,0,0,0,0,0,0,0,
     &                                      1,1,0,0,0,1,1,1,1,0,0,
     &                                      1,1,0,0,0,1,1,1,1,0,0,
     &                                      1,1,1,0,0,0,0,1,1,0,0,
     &                                      0,1,1,1,1,1,1,1,1,0,0,
     &                                      0,0,1,1,1,1,0,1,1,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,19),I=1,11),J=1,15)/0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,20),I=1,11),J=1,15)/0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,21),I=1,11),J=1,15)/0,0,0,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,1,1,0,0,
     &                                      1,1,1,0,0,0,0,1,1,0,0,
     &                                      1,1,0,0,0,0,0,0,0,0,0,
     &                                      1,1,0,0,0,1,1,1,1,0,0,
     &                                      1,1,0,0,0,1,1,1,1,0,0,
     &                                      1,1,1,0,0,0,0,1,1,0,0,
     &                                      0,1,1,1,1,1,1,1,1,0,0,
     &                                      0,0,1,1,1,1,0,1,1,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,22),I=1,11),J=1,15)/0,1,1,0,0,0,1,1,1,0,0,
     &                                      0,1,1,0,0,1,1,1,0,0,0,
     &                                      0,1,1,0,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,0,0,0,0,0,
     &                                      0,1,1,1,1,1,0,0,0,0,0,
     &                                      0,1,1,0,1,1,0,0,0,0,0,
     &                                      0,1,1,0,0,1,1,0,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,1,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,23),I=1,11),J=1,15)/0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,24),I=1,11),J=1,15)/0,1,1,1,0,0,0,1,1,1,0,
     &                                      0,1,1,1,1,0,1,1,1,1,0,
     &                                      0,1,1,1,1,0,1,1,1,1,0,
     &                                      0,1,1,1,1,0,1,1,1,1,0,
     &                                      0,1,1,1,1,0,1,1,1,1,0,
     &                                      0,1,1,0,1,1,1,0,1,1,0,
     &                                      0,1,1,0,1,1,1,0,1,1,0,
     &                                      0,1,1,0,1,1,1,0,1,1,0,
     &                                      0,1,1,0,1,1,1,0,1,1,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,25),I=1,11),J=1,15)/0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,1,0,0,1,1,0,0,0,
     &                                      0,1,1,1,1,0,1,1,0,0,0,
     &                                      0,1,1,1,1,0,1,1,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,1,1,0,1,1,1,1,0,0,0,
     &                                      0,1,1,0,1,1,1,1,0,0,0,
     &                                      0,1,1,0,0,1,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,26),I=1,11),J=1,15)/0,0,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      1,1,1,0,0,0,1,1,1,0,0,
     &                                      1,1,0,0,0,0,0,1,1,0,0,
     &                                      1,1,0,0,0,0,0,1,1,0,0,
     &                                      1,1,0,0,0,0,0,1,1,0,0,
     &                                      1,1,1,0,0,0,1,1,1,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,0,1,1,1,1,1,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,27),I=1,11),J=1,15)/0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,1,1,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,28),I=1,11),J=1,15)/0,0,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      1,1,1,0,0,0,1,1,1,0,0,
     &                                      1,1,0,0,0,0,0,1,1,0,0,
     &                                      1,1,0,0,0,0,0,1,1,0,0,
     &                                      1,1,0,0,0,1,0,1,1,0,0,
     &                                      1,1,1,0,0,1,1,1,1,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,0,1,1,1,1,0,1,1,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,29),I=1,11),J=1,15)/0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,30),I=1,11),J=1,15)/0,1,1,1,1,1,0,0,0,0,0,
     &                                      1,1,1,1,1,1,1,0,0,0,0,
     &                                      1,1,0,0,0,1,1,0,0,0,0,
     &                                      1,1,0,0,0,0,0,0,0,0,0,
     &                                      0,1,1,1,1,1,0,0,0,0,0,
     &                                      0,0,0,0,0,1,1,0,0,0,0,
     &                                      1,1,0,0,0,1,1,0,0,0,0,
     &                                      1,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,31),I=1,11),J=1,15)/1,1,1,1,1,1,1,1,0,0,0,
     &                                      1,1,1,1,1,1,1,1,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,32),I=1,11),J=1,15)/0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,0,1,1,0,0,0,
     &                                      0,1,1,1,1,1,1,1,0,0,0,
     &                                      0,0,1,1,1,1,1,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,33),I=1,11),J=1,15)/1,1,0,0,0,0,1,1,0,0,0,
     &                                      1,1,0,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,1,1,0,0,0,0,
     &                                      0,1,1,0,0,1,1,0,0,0,0,
     &                                      0,1,1,0,0,1,1,0,0,0,0,
     &                                      0,0,1,1,1,1,0,0,0,0,0,
     &                                      0,0,1,1,1,1,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,34),I=1,11),J=1,15)/1,1,0,0,1,1,1,0,0,1,1,
     &                                      1,1,0,0,1,1,1,0,0,1,1,
     &                                      1,1,1,0,1,1,1,0,1,1,1,
     &                                      0,1,1,0,1,1,1,0,1,1,0,
     &                                      0,1,1,1,1,0,1,1,1,1,0,
     &                                      0,1,1,1,1,0,1,1,1,1,0,
     &                                      0,0,1,1,0,0,0,1,1,0,0,
     &                                      0,0,1,1,0,0,0,1,1,0,0,
     &                                      0,0,1,1,0,0,0,1,1,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,35),I=1,11),J=1,15)/1,1,1,0,0,1,1,1,0,0,0,
     &                                      0,1,1,0,0,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,0,0,0,0,0,
     &                                      0,0,1,1,1,1,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,1,1,1,1,0,0,0,0,0,
     &                                      0,1,1,1,1,1,0,0,0,0,0,
     &                                      0,1,1,0,0,1,1,0,0,0,0,
     &                                      1,1,1,0,0,1,1,1,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,36),I=1,11),J=1,15)/1,1,0,0,0,0,1,1,0,0,0,
     &                                      0,1,1,0,0,1,1,0,0,0,0,
     &                                      0,1,1,0,0,1,1,0,0,0,0,
     &                                      0,0,1,1,1,1,0,0,0,0,0,
     &                                      0,0,1,1,1,1,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,37),I=1,11),J=1,15)/1,1,1,1,1,1,1,0,0,0,0,
     &                                      1,1,1,1,1,1,1,0,0,0,0,
     &                                      0,0,0,0,1,1,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,1,1,1,0,0,0,0,0,0,
     &                                      0,1,1,1,0,0,0,0,0,0,0,
     &                                      1,1,1,0,0,0,0,0,0,0,0,
     &                                      1,1,1,1,1,1,1,0,0,0,0,
     &                                      1,1,1,1,1,1,1,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,38),I=1,11),J=1,15)/0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,39),I=1,11),J=1,15)/0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/
      DATA ((LETTERS(I,J,40),I=1,11),J=1,15)/0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,1,1,1,1,1,1,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,1,1,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0,
     &                                      0,0,0,0,0,0,0,0,0,0,0/

C Convert string to ascii code.
      ILET=ICHAR(t1)

C Convert this to letter order in LETTERS array.
      ILET=ILET-47
      if (ILET.gt.17) ILET=ILET-6
      if (ILET.eq.-1) ILET=11
      if (ILET.eq.-2) ILET=39
      if (ILET.eq.-4) ILET=40
C 'C' was printed as '='      if (ILET.eq.14) ILET=38

C Check for non printable chars.
      if (ILET.lt.1.or.ILET.gt.40) then
        do 50 I=1,11
          do 51 J=1,15
            LETTER(I,J)=0
 51       continue
 50     continue
        return
      endif

C Copy pixel data to LETTER array.
      do 10 I=1,11
        do 20 J=1,15
          LETTER(I,J)=LETTERS(I,J,ILET)
 20     continue
 10   continue

      RETURN
      END
