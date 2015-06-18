C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2014.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C e2dxf takes an esp-r problem definition and creates a zip
C file in file unit ioout (assumed to already be opened).
C << TOBE DONE:
C << a) switch to new include files for geometry
C << b) include obstructions as gen bodies
C << c) include MRT blocks as gen bodies
C << d) figure out about marking transparent polygons
C <<    the key word is VIS=0 translucent, 1 wire-frame, 2 solid, -3 invis
C << figure out how to use ZON key word

      subroutine e2zip(itrc,ioout)
#include "building.h"
#include "model.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/C1/NCOMP,NCON
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)

C      common/io/ioin,ioout,ioblk,iosblk,iotobs
      dimension ndum(MS)

      character outs*124

C Setup standard assumptions.
      IFIL=11

C Assume configuration file is from IFIL+5, any leakage description
C is fom IFIL+6, revised config file on IFIL+3, geometry on
C IFILE+2 and ASCII viewing and geometry file reading on IFILE+1. 
      ITA1 = IFIL+8

C If zip output required.
      INPIC=NCOMP
      do 4 mz=1,inpic
        newfoc=mz

C Read in the zone geometry.
        WRITE(outs,'(a,a)')' Scanning : ',LGEOM(newfoc)
        CALL edisp(iuout,outs)

C Depending on version of geometry file scan it in.
        if(gversion(mz).lt.1.1) then
          CALL EGOMIN(ITA1,LGEOM(newfoc),newfoc,1,ITRC,IUOUT,IER)
          if(iobs(mz).eq.0)then
            continue   ! no obstructions
          elseif(iobs(mz).eq.1)then
            CALL ERPFREE(iafil,ISTAT)
            CALL EGOMST(ITA1I,newfoc,ZOBS(newfoc),0,ITRC,ITRU,IER)
          endif
        elseif(gversion(mz).ge.1.1) then
          call georead(ITA1,LGEOM(newfoc),newfoc,1,iuout,ier)
          if(iobs(mz).eq.0)then
            continue   ! no obstructions
          elseif(iobs(mz).eq.1)then
            CALL ERPFREE(iafil,ISTAT)
            CALL EGOMST(ITA1I,newfoc,ZOBS(newfoc),0,ITRC,ITRU,IER)
          elseif(iobs(mz).eq.2)then
            continue   ! obs within geo file
          endif
        endif

C Write surface information to the zip file. (Rotation set to 0 deg).
        WRITE(ioout,'(a,a12)')'GEN ',zname(newfoc)
        WRITE(ioout,'(2I4,a)')NTV,NSUR,' 0'
        DO 40 J=1,NTV
          WRITE(ioout,'(3F10.5)')X(J),Y(J),Z(J)
   40   continue

        DO 50 J=1,NSUR
          WRITE(ioout,'(2X,I3,1X,24I3)')NVER(J),(JVN(J,K),K=1,NVER(J))
          ndum(J)=0
   50   continue

        write(ioout,5611)(ndum(I),I=1,NSUR)
        write(ioout,5611)(ndum(I),I=1,NSUR)
5611    FORMAT(1X,32(I2))
        write(ioout,'(a)')' 3 0 0 0 '

C If there are obstructions convert them to general polygons
C and write them out.
        if(iobs(mz).eq.0)then
          continue
        elseif(iobs(mz).eq.1.or.iobs(mz).eq.2)then

          DO 301 IB=1,nbobs(mz)
            if(BLOCKTYP(mz,IB)(1:4).eq.'obs ')then
              CALL CNVBLK(XOB(mz,IB),YOB(mz,IB),ZOB(mz,IB),
     &          DXOB(mz,IB),DYOB(mz,IB),DZOB(mz,IB),
     &          BANGOB(mz,IB,1))
            elseif(BLOCKTYP(mz,IB)(1:4).eq.'obs3')then
              CALL CNVBLK3A(XOB(mz,IB),YOB(mz,IB),ZOB(mz,IB),
     &          DXOB(mz,IB),DYOB(mz,IB),DZOB(mz,IB),
     &          BANGOB(mz,IB,1),BANGOB(mz,IB,2),BANGOB(mz,IB,3))
            elseif(BLOCKTYP(mz,IB)(1:4).eq.'obsp')then
              call CNVBLKP(mz,IB) ! convert obsp type.
            endif
C << and then what... look in e2rform.F near line 1120
C << there are 8 vertices and 6 surfaces
            lnz=lnblnk(zname(mz))
            WRITE(ioout,'(4a)') 'GEN OBS:',zname(mz)(1:lnz),':',
     &        BLOCKNAME(mz,IB)(1:LNBLOCKNAME(mz,IB))
            WRITE(ioout,'(a)')' 8  6  0'
            DO 41 J=1,8
              if (BLOCKTYP(mz,ib).eq.'obs ') then
                WRITE(ioout,'(3F10.5)')XB(J),YB(J),ZB(J)
              elseif (BLOCKTYP(mz,ib).eq.'obs3') then
                WRITE(ioout,'(3F10.5)')XB(J),YB(J),ZB(J)
              elseif (BLOCKTYP(mz,ib).eq.'obsp') then
                WRITE(ioout,'(3F10.5)')XBP(mz,IB,J),YBP(mz,IB,J),
     &            ZBP(mz,IB,J)
              endif
   41       continue
            DO 51 J=1,6
              WRITE(ioout,'(a,4I3)') ' 4 ',(JVNB(J,K),K=1,4)
              ndum(J)=0
   51       continue

            write(ioout,5611)(ndum(I),I=1,6)
            write(ioout,5611)(ndum(I),I=1,6)
            write(ioout,'(a)')' 3 0 0 0 '

  301     CONTINUE
        endif

   4  continue

      return
      END

