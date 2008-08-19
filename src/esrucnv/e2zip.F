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

C e2dxf takes an esp-r problem definition and creates a zip
C file.
      subroutine e2zip(itrc,incobs,ichop)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      character CTYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

      common/io/ioin,ioout,ioblk,iosblk,iotobs
      dimension ndum(MS)

      character zname*12,zdesc*64,outs*124
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      logical newgeo  ! to use for testing if new/old geometry file.

C Setup standard assumptions.
      IFIL=11
      newgeo=.false.  ! assume older format geometry.

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
        call eclose(gversion(newfoc),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(ITA1,LGEOM(newfoc),newfoc,1,ITRC,IUOUT,IER)
        else
          call egomin(ITA1,LGEOM(newfoc),newfoc,1,ITRC,IUOUT,IER)
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
   4  continue

      return
      END

