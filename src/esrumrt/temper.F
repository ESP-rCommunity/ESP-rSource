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

c Compute Mean Radiant Temperature (TMRT)
c given as input:
c   Cube dimensions
c   viewfactors from cube to geometry        F(I,J)
c   Temperatures of all geometry surfaces    TEMP(j)

c                        4                                 4
c The model used is Tmrt(i) = (sum over j)   F(i,j) * TEMP(j)

C vectemp(k,i): The x,y,z values of the 'Vector Radiant Temp'.
C               Positive value means that the 'hot'side is
C               towards sensor face with the largest X,Y or Z
C               Z coordinate.

      subroutine temperature(icube)

#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON /FORMF/ FF(MSM,MSM), FFS
      integer ncub,ivfok
      real XOC,YOC,ZOC,DXC,DYC,DZC,CANG
      character CUBN*6
      COMMON/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)
      common /cube3/surarea(6)
      common/tmrt2/temp(MS),tempmrt
      common /cube5/fstgrp,fstsur,lstsur,strtgr
      integer fstgrp,fstsur,lstsur,strtgr
      common/tmrt1/tmrt,temperr
      logical tmrt,temperr

      dimension vectemp(3,MS)
      character outs*124
      real t(6),sum

      vectemp(1,icube)=0.0
      vectemp(2,icube)=0.0
      vectemp(3,icube)=0.0
      t(1)=0.0
      t(2)=0.0
      t(3)=0.0
      t(4)=0.0
      t(5)=0.0
      t(6)=0.0
      tempmrt=0.0

c Theoretically, the sum over a row in the view factor
c matrix should be 1.00.  Sometimes the program gives
c values that do not follow this rule.  Therefore the
c values of one row are scaled, in order to ensure that
c the sum is equal to one.
      temperr=.false.
      do 30 i=fstsur,fstsur+5
        sum =0.0
        do 10 j=1,lstsur
          sum= sum+ ff(i,j)
10      continue
        if (sum.gt.0.0) then
          do 20 j=1,lstsur
            ff(i,j)=ff(i,j)/sum
20        continue
        else
          ixxx=(i-fstsur+1)
          write(outs,22)ixxx
22        format(' Surface number',i3,' of cube cannot SEE other',
     &           ' Surfaces. TMRT incorrect !!')
          call edisp(iuout,outs)
          temperr=.true.
        endif
30    continue

c  For every surface of the cube inside the geometry the radiant
c  temperature is calculated. '  t(k)  '. Surface properties
C  are taken into account....look at code in MZCFGG
      do 200 i=fstsur,fstsur+5
        k=i-fstsur+1
        t(k)=0.0
        do 100 j=1,lstsur
          t(k)=t(k)+ff(i,j)*((temp(j)+273.19)**4)
100     continue
        t(k)=(t(k)**0.25)
200   continue

c  Here the tempmrt value is estimated as the mean radiant
c  temperature of all cube surfaces.
c  surarea(i) = the area of surface i divided by the total area.
      tempmrt=0.0
      do 300 i=1,6
        tempmrt=tempmrt+(t(i)*surarea(i))
        t(i)=(t(i)-273.19)
300   continue
      tempmrt=(tempmrt)-273.19

c  Here the Vector Radiant Temperature is calculated.
c  Cubefaces 4,1,6 have the larger x,y or z coordinates.
      vectemp(1,icube)=t(4)-t(2)
      vectemp(2,icube)=t(1)-t(3)
      vectemp(3,icube)=t(6)-t(5)

      call edisp(iuout,' ')
      WRITE(outs,'(A,I2)') ' Number of MRT sensors: ',NCUB
      call edisp(iuout,outs)
      call edisp(iuout,
     &  ' ___________________________________________')
      call edisp(iuout,
     &  ' Sensor|   origin (m)       |    size (m)       |rotation')
      call edisp(iuout,
     &  ' name  |X-cord|Y-cord|Z-cord|length|width|height|degree')
      L=icube
      WRITE(outs,303)CUBN(L),XOC(L),YOC(L),ZOC(L),
     &               DXC(L),DYC(L),DZC(L),CANG(L)
  303 FORMAT(1X,A6,3F7.2,1x,3F6.2,F6.1)
      call edisp(iuout,outs)
      call edisp(iuout,' ')

      write(outs,'(A,F8.2)') ' TMRT-value             [C]  :',tempmrt
      call edisp(iuout,outs)
      write(outs,'(A,3F8.2)')' VRT values(x,y and z)  [C]  :',
     &                        (vectemp(i,icube),i=1,3)
      call edisp(iuout,outs)
      return
      end

CC ======================================== CC
C Copy of res routine SENMRT ... adapted 
C slightly for use in h3k_report_data
C (c) ag@gae.ch / February 2011
C ******************** SENMRT ********************
C TSENMRT computes the resultant temperature(s) from opaque
C surface, window temperature information at MRT sensors.
C IOPT - specific temperature differences
C   IOPT = 0 is "standard mean radiant at sensor".

      SUBROUTINE TSENMRT(IZONE,IOPT,NSENS,TRET)
#include "building.h"
#include "model.h"
#include "geometry.h"
      integer NSENS   ! return value
      real TRET(MCUB) ! return value

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      REAL TFS, QFS

      COMMON/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)
      common/MRTF/FFL(MCUB,6,MSM),VEWF(MS,MS)


      DIMENSION XD(MZS),surarea(6),stemp(6)

      integer ISENS
      character outs*124,LMRT*72,CUBN*6

      NSENS=0

C Recover sensor viewfactors and fill commons.
      IUF=IFIL+1
      LMRT=LVIEW(IZONE)
      call ERMRT(0,IUOUT,IUF,LMRT,IZONE,IER)

C loop through each sensor of zone (if any)
      if (NCUB.gt.0) then
        NSENS=NCUB
        do 20 ISENS=1,NCUB
C Find surface areas of sensor cube.
          surarea(1)=DXC(ISENS)*DZC(ISENS)
          surarea(2)=DYC(ISENS)*DZC(ISENS)
          surarea(3)=surarea(1)
          surarea(4)=surarea(2)
          surarea(5)=DXC(ISENS)*DYC(ISENS)
          surarea(6)=surarea(5)
          totarea=(surarea(1)+surarea(2)+surarea(5))*2.0
          do 5 i=1,6
            surarea(i)=surarea(i)/totarea
5         continue

C Check view factors sum to one (if not then scale).
          do 10 I=1,6
            sum=0.000
            do 15 J=1,NZSUR(IZONE)
              sum=sum+FFL(ISENS,I,J)
 15         continue
            if (sum.gt.0.0) then
              do 25 J=1,NZSUR(IZONE)
                FFL(ISENS,I,J)=FFL(ISENS,I,J)/sum
 25           continue
            else
              call edisp(iuout,'error in sensor view factors.')
            endif
 10       continue

          TRET(ISENS)=0.

C Calculate MRT.
          do 30 K=1,6
            stemp(K)=0.0
            do 40 J=3,NZSUR(IZONE)+2
              stemp(K)=stemp(K)+
     &                 FFL(ISENS,K,(J-2))*((TFS(iZone,J-2)+273.19)**4)
 40         continue
            stemp(K)=stemp(K)**0.25
 30       continue

C Treat data according to IOPT.
          if (IOPT.eq.0) then

C Average surface radiant temperatures.
          XD(1)=0.0
          do 50 K=1,6
            XD(1)=XD(1)+(stemp(K)*surarea(K))
C            stemp(K)=stemp(K)-273.19
 50       continue
          TRET(ISENS)=XD(1)-273.19

C Calculate delta T between opposite surfaces.  The value of IOPT is the 
C surface representing the front surface.
          elseif (IOPT.eq.1) then
            TRET(ISENS)=stemp(1)-stemp(3)
          elseif (IOPT.eq.2) then
            TRET(ISENS)=stemp(2)-stemp(4)
          elseif (IOPT.eq.3) then
            TRET(ISENS)=stemp(3)-stemp(1)
          elseif (IOPT.eq.4) then
            TRET(ISENS)=stemp(4)-stemp(2)
          elseif (IOPT.eq.5) then
            TRET(ISENS)=stemp(5)-stemp(6)
          elseif (IOPT.eq.6) then
            TRET(ISENS)=stemp(6)-stemp(5)
          endif

C Debug.
C      write(6,*) 'TRET= ',TRET
 20     continue ! ISENS loop
      endif ! NCUB > 0
      return

 99   call edisp(iuout,' reading error in SENMRT')

      return
      end
