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

      INCLUDE 'building.h'
      COMMON/OUTIN/IUOUT,IUIN
      COMMON /FORMF/ FF(MSM,MSM), FFS
      COMMON/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)
      common /cube3/surarea(6)
      common/tmrt2/temp(MS),tempmrt
      common /cube5/fstgrp,fstsur,lstsur,strtgr
      integer fstgrp,fstsur,lstsur,strtgr
      common/tmrt1/tmrt,temperr
      logical tmrt,temperr

      dimension vectemp(3,MS)
      character outs*124,CUBN*6
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
      WRITE(outs,'(A,I2)') ' MRT sensor: ',NCUB
      call edisp(iuout,outs)
      call edisp(iuout,' ___________________________________________')
      call edisp(iuout,' Sensor|   origin (m)    |    size (m)  |rot')
      call edisp(iuout,' name  |Xcord|Ycord|Zcord|len |wid |ht  |deg')
      L=icube
      WRITE(outs,303)CUBN(L),XOC(L),YOC(L),ZOC(L),
     &               DXC(L),DYC(L),DZC(L),CANG(L)
  303 FORMAT(1X,A6,3F6.1,3F5.2,F5.0)
      call edisp(iuout,outs)
      call edisp(iuout,' ')

      write(outs,'(A,F8.2)') ' TMRT-value             [C]  :',tempmrt
      call edisp(iuout,outs)
      write(outs,'(A,3F8.2)')' VRT values(x,y and z)  [C]  :',
     &                        (vectemp(i,icube),i=1,3)
      call edisp(iuout,outs)
      return
      end
