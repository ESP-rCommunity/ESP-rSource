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
C (related to image creation via vector commands)
C  MAKS2D - draws a 2 d silce of flow vectors on screen.  
C  MAKS3D - draws flow vectors on screen in 3D.  

C C ********************* MAKS2D *********************
C MAKS2D - draw 2d slices on screen.
      SUBROUTINE MAKS2D(ISUR,ilayer,name)
#include "cfd.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2

      COMMON/VISSET/imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal
      COMMON/arpoly/XS(7),ZS(7),bXmin,bXmax,bZmin,bZmax

      character souts*48,name*12,scale*10,title*30
#ifdef OSX
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

C Set default values.
      XZ=0.
      YZ=0.
      ZZ=0.

C Clear viewing area, set and store scaling factors.
      CALL startbuffer()
      CALL win3d(menuchw,6,6,3,4,igl,igr,igt,igb,igw,igwh)
      igw=igr-igl
      igwh=igb-igt

C Set up the do loops according to which surface is being viewed.
C East or west view.
      title='  '
      if(ISUR.eq.2 .or. ISUR.eq.4) then
        L1max = NJM1
        L2max = NKM1
        call VGRID2D(YZ,ZZ,YV,ZW,NJ,NK,title,'Y axis (m)','Z')

C North or south view.
      elseif(ISUR.eq.1 .or. ISUR.eq.3) then
        L1max = NIM1
        L2max = NKM1
        call VGRID2D(XZ,ZZ,XU,ZW,NI,NK,title,'X axis (m)','Z')

C Top or bottom view.
      elseif(ISUR.eq.5 .or. ISUR.eq.6) then
        L1max = NIM1
        L2max = NJM1
        call VGRID2D(XZ,YZ,XU,YV,NI,NJ,title,'X axis (m)','Y')
      endif

C Iterate through each flow vector to determine which grids of the mesh 
C it covers.
      do 10 L1=2,L1max
        do 20 L2=2,L2max

C Make each flow vector into a polygon.
          if(ISUR.eq.2 .or. ISUR.eq.4) then
            CALL POLY(ilayer,L1,L2)
C            temp=Tf(ilayer,L1,L2)
          endif
          if(ISUR.eq.1 .or. ISUR.eq.3) then
            CALL POLY(L1,ilayer,L2)
C            temp=Tf(L1,ilayer,L2)
          endif
          if(ISUR.eq.5 .or. ISUR.eq.6) then
            CALL POLY(L1,L2,ilayer)
C            temp=Tf(L1,L2,ilayer)
          endif

C Draw vector.
          call etplot(XS(1),ZS(1),0,0)
          do 30 I=2,7
            call etplot(XS(I),ZS(I),1,0)
 30       continue
          call etplot(XS(1),ZS(1),1,0)
 20     continue
 10   continue

C Write a title for the plot.
      write (souts,'(a,a)') ' Velocity vector plot for view ',name
      iside=1
      isize=0
      ifont=1
      call viewtext(souts,iside,isize,ifont)

C Draw a scale. First find distance between two points (iby and iysl)
C this represents 'sll' meters per second (initially 0.1).  If the 
C line takes up more than 1/4 of the width of the screen then try 
C again by dividing 'sll' by 10.
      call u2pixel(0.0,0.0,ibx,iby)
      icnt=1
      imax=igw/4
      div=1.0
 50   sll=0.1/div
      call u2pixel(0.0,sll,ixsl,iysl)
      isll= int((iby-iysl)*vLscal)
      if (icnt.gt.10) stop
      if (isll.gt.imax) then
        icnt=icnt+1
        div=div*10.0
        goto 50
      endif
      iright=igl+isll+10
      ileft=igl+10

C Draw scale line. e.g. |--------| = 0.1m/s
      call eswline(ileft,igt-5,ileft,igt-15)
      call eswline(iright,igt-5,iright,igt-15)
      call eswline(ileft,igt-10,iright,igt-10)
      write (scale,'(a,f5.3,a)') '= ',sll,'m/s'
      call wstxpt(iright+5,igt-5,scale)
      call forceflush()

      return
      end

C ********************* MAKS3D *********************
C MAKS3D - draw flows in 3d on screen using scaling factors of 
C first view.
      SUBROUTINE MAKS3D
#include "cfd.h"

      COMMON/VISSET/imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      common/vecXYZ/vecXbeg(ntcelx,ntcely,ntcelz),
     1              vecXend(ntcelx,ntcely,ntcelz),
     2              vecYbeg(ntcelx,ntcely,ntcelz),
     3              vecYend(ntcelx,ntcely,ntcelz),
     4              vecZbeg(ntcelx,ntcely,ntcelz),
     5              vecZend(ntcelx,ntcely,ntcelz)

      dimension Vstart(3),Vend(3),IC(6,2)

C Need to reset scale factors - do this by redrawing the domain.
      iz=0
      call VGRID3D('---','xyz',iz)

C << res cannot draw boundaries just now but calls useful in prj >>
C << TODO add boundary information to cfd results library to get round this >>
C      call VSB3D(0)
C      call VAO3D(0)

C Loop through all vectors.
      do 10 I=2,NIM1
        do 101 J=2,NJM1
          do 102 K=2,NKM1
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
            hsf=hWscal/10.
            call arrow(Vstart,Vend,0.5,hsf,IC,'r',2)
 102      continue
 101    continue
 10   continue
      call forceflush()

      return
      end
