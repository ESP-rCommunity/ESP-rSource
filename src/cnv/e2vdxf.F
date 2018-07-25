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

C e2dxf: takes an esp-r problem definition and creates either
C        an AutoCad DXF file or viewer file.
C e2thf: takes an esp-r problem definition and creates a
C        THF (THINGS) file for use in MicroGDS.
C lindir: finds the angles associate with a line in space.

      subroutine e2vdxf(incobs)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G1/G2/G5/precz/GS5.
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      COMMON/PRECT3/NTMC,NGLAZ(MTMC)

      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk

      integer indxzon
      character oformat*24,head*4
      common/dxfe/oformat,indxzon,head

      logical doobs

      character ZN*12,outs*124

C Setup standard assumptions.
      if(incobs.eq.1)doobs=.true.
      IFIL=11

C Assume configuration file is from IFIL+5, any leakage description
C is fom IFIL+6, revised config file on IFIL+3, geometry on
C IFILE+2 and ASCII viewing and geometry file reading on IFILE+1. 
      ITA1 = IFIL+8

C If viewer output required.
      if(oformat(1:4).eq.'view')then
        INPIC=NCOMP
        do 4 mz=1,inpic
          newfoc=mz

C Clear properties.
          NTMC=0
          nbobs(newfoc)=0

C Read in the zone geometry.
          WRITE(outs,'(a,a)')' Scanning : ',LGEOM(newfoc)
          CALL edisp(iuout,outs)
          call georead(ITA1,LGEOM(newfoc),newfoc,1,IUOUT,IER)
          call strup(zname(newfoc),ZN)  ! make upper case name

C Write surface information to the viewer file. First invert edges.
          DO 120 J=1,NSUR
            K=NVER(J)
            DO 130 L=1,K/2
              ITEMP=JVN(J,L)
              JVN(J,L)=JVN(J,K+1-L)
              JVN(J,K+1-L)=ITEMP
  130       continue
  120     continue

C Write to output file.
          WRITE(ioout,'(a,a15)')'GEN ',ZN
          WRITE(ioout,'(2I7)')NTV,NSUR
          DO 40 J=1,NTV
            WRITE(ioout,'(3F9.4)')X(J),Y(J),Z(J)
   40     continue
          DO 50 J=1,NSUR
            WRITE(ioout,'(2X,I3,1X,21I3)')NVER(J),(JVN(J,K),K=1,NVER(J))
   50     continue

C Deal with solar obstructions.
          if(nbobs(newfoc).ge.1)then
            if(incobs.eq.0)goto 4
            DO 301 IB=1,nbobs(newfoc)

C Use the GB1 common block via CNVBLK or CNVBLK3A.
              if(BLOCKTYP(newfoc,IB)(1:4).eq.'obs ')then
                CALL CNVBLK(XOB(newfoc,IB),YOB(newfoc,IB),
     &            ZOB(newfoc,IB),DXOB(newfoc,IB),DYOB(newfoc,IB),
     &            DZOB(newfoc,IB),BANGOB(newfoc,IB,1))
              elseif(BLOCKTYP(newfoc,IB)(1:4).eq.'obs3')then
                CALL CNVBLK3A(XOB(newfoc,IB),YOB(newfoc,IB),
     &            ZOB(newfoc,IB),DXOB(newfoc,IB),DYOB(newfoc,IB),
     &            DZOB(newfoc,IB),BANGOB(newfoc,IB,1),
     &            BANGOB(newfoc,IB,2),BANGOB(newfoc,IB,3))
              elseif(BLOCKTYP(newfoc,IB)(1:4).eq.'obsp')then
                call CNVBLKP(newfoc,IB) ! convert obsp type.
              endif

C Write obstruction information after inverting edges.
              DO 320 J=1,6
                K=4
                DO 330 L=1,K/2
                  ITEMP=JVNB(J,L)
                  JVNB(J,L)=JVNB(J,K+1-L)
                  JVNB(J,K+1-L)=ITEMP
  330           continue
  320         continue

C Write obstructions to output file.
              WRITE(ioout,'(a,a15,a)')'GEN ',ZN,' obstructions'
              WRITE(ioout,'(a)')'  8  6'
              DO 240 J=1,8  ! each of the vertices
                WRITE(ioout,'(3F9.4)')XB(J),YB(J),ZB(J)
  240         continue
              DO 250 J=1,6  ! each of the surfaces
                WRITE(ioout,'(a,21I3)') '  4 ',(JVNB(J,K),K=1,4)
  250         continue
  301       continue
          ENDIF

C Free the viewer file and reread the geometry file.
          call georead(ITA1,LGEOM(newfoc),newfoc,1,IUOUT,IER)
   4    continue

        CALL ERPFREE(ioout,ISTAT)
        call edisp(iuout,' End of conversion.')
        CALL EPAGEND
        STOP
      endif

C If dxf is output format. This style below is roughly R14.
C It can also be read by ?? and ??
      write(ioout,'(a)')'  0'
      write(ioout,'(a)')'SECTION'
      write(ioout,'(a)')'  2'
      write(ioout,'(a)')'HEADER'
      write(ioout,'(a)')'  0'
      write(ioout,'(a)')'ENDSEC'
      write(ioout,'(a)')'  0'
      write(ioout,'(a)')'SECTION'
      write(ioout,'(a)')'  2'
      write(ioout,'(a)')'TABLES'

C Std line type definition.
      write(ioout,'(a)')'  0'
      write(ioout,'(a)')'TABLE'
      write(ioout,'(a)')'  2'
      write(ioout,'(a)')'LTYPE'
      write(ioout,'(a)')' 70'
      write(ioout,'(a)')'     1'
      write(ioout,'(a)')'  0'
      write(ioout,'(a)')'LTYPE'
      write(ioout,'(a)')'  2'
      write(ioout,'(a)')'CONTINUOUS'
      write(ioout,'(a)')' 70'
      write(ioout,'(a)')'    64'
      write(ioout,'(a)')'  3'
      write(ioout,'(a)')'Solid line'
      write(ioout,'(a)')' 72'
      write(ioout,'(a)')'    65'
      write(ioout,'(a)')' 73'
      write(ioout,'(a)')'     0'
      write(ioout,'(a)')' 40'
      write(ioout,'(a)')'0.0'
      write(ioout,'(a)')'  0'
      write(ioout,'(a)')'ENDTAB'

C Std zero layer  << is this needed? >>.
      write(ioout,'(a)')'  0'
      write(ioout,'(a)')'TABLE'
      write(ioout,'(a)')'  2'
      write(ioout,'(a)')'LAYER'
      write(ioout,'(a)')' 70'
      write(ioout,'(a)')'     4'
      write(ioout,'(a)')'  0'
      write(ioout,'(a)')'LAYER'
      write(ioout,'(a)')'  2'
      write(ioout,'(a)')'0'
      write(ioout,'(a)')' 70'
      write(ioout,'(a)')'    64'
      write(ioout,'(a)')' 62'
      write(ioout,'(a)')'     7'
      write(ioout,'(a)')'  6'
      write(ioout,'(a)')'CONTINUOUS'
      write(ioout,'(a)')'  0'

C Std layer 1 << is this needed? >>.
      write(ioout,'(a)')'LAYER'
      write(ioout,'(a)')'  2'
      write(ioout,'(a)')'1'
      write(ioout,'(a)')' 70'
      write(ioout,'(a)')'    64'
      write(ioout,'(a)')' 62'
      write(ioout,'(a)')'     7'
      write(ioout,'(a)')'  6'
      write(ioout,'(a)')'CONTINUOUS'
      write(ioout,'(a)')'  0'

C Layer for each zone.
      INPIC=NCOMP
      do 14 mz=1,inpic
        call strup(zname(mz),ZN)
        write(ioout,'(a)')'LAYER'
        write(ioout,'(a)')'  2'
        write(ioout,'(a)')ZN(1:lnblnk(ZN))
        write(ioout,'(a)')' 70'
        write(ioout,'(a)')'    64'
        write(ioout,'(a)')' 62'
        write(ioout,'(a)')'     7'
        write(ioout,'(a)')'  6'
        write(ioout,'(a)')'CONTINUOUS'
        write(ioout,'(a)')'  0'

C If an obstruction for zone then make it the next layer.
        if(IOBS(mz).GT.0)then
          doobs=.TRUE.
          write(ioout,'(a)')'LAYER'
          write(ioout,'(a)')'  2'
          write(ioout,'(a4,a)') 'OBS_',ZN(1:lnblnk(ZN))
          write(ioout,'(a)')' 70'
          write(ioout,'(a)')'    64'
          write(ioout,'(a)')' 62'
          write(ioout,'(a)')'     7'
          write(ioout,'(a)')'  6'
          write(ioout,'(a)')'CONTINUOUS'
          write(ioout,'(a)')'  0'
        endif

C If there are transparent surfaces put them in another layer << .. >>
        if(ITW(mz).EQ.1)then
          write(ioout,'(a)')'LAYER'
          write(ioout,'(a)')'  2'
          write(ioout,'(a4,a)') 'TRN_',ZN(1:lnblnk(ZN))
          write(ioout,'(a)')' 70'
          write(ioout,'(a)')'    64'
          write(ioout,'(a)')' 62'
          write(ioout,'(a)')'     7'
          write(ioout,'(a)')'  6'
          write(ioout,'(a)')'CONTINUOUS'
          write(ioout,'(a)')'  0'
        endif
  14  continue
      write(ioout,'(a)')'ENDTAB'
      write(ioout,'(a)')'  0'
      write(ioout,'(a)')'ENDSEC'

C Write a standard obstruction block in the block section if
C there are any *.obs files or embedded blocks in geometry files.

C << what about six sided non-cube blocks? >>
C << could solids or 3DPOLY be used instead? >>

      write(ioout,'(a)')'  0'
      write(ioout,'(a)')'SECTION'
      write(ioout,'(a)')'  2'
      write(ioout,'(a)')'BLOCKS'
      write(ioout,'(a)')'  0'
      if(doobs)then
        write(ioout,'(a)')'BLOCK'
        write(ioout,'(a)')'  8'
        write(ioout,'(a)')'0'
        write(ioout,'(a)')'  2'
        write(ioout,'(a)')'OBSTRUCTION'
        write(ioout,'(a)')' 70'
        write(ioout,'(a)')'    64'
        write(ioout,'(a,/,a)')' 10','0.0'
        write(ioout,'(a,/,a)')' 20','0.0'
        write(ioout,'(a,/,a)')' 30','0.0'
        write(ioout,'(a)')'  3'
        write(ioout,'(a)')'OBSTRUCTION'
        write(ioout,'(a)')'  0'
        write(ioout,'(a)')'3DFACE'
        write(ioout,'(a)')'  8'
        write(ioout,'(a)')'0'
        write(ioout,'(a,/,a)')' 10','0.0'
        write(ioout,'(a,/,a)')' 20','1.0'
        write(ioout,'(a,/,a)')' 30','0.0'
        write(ioout,'(a,/,a)')' 11','0.0'
        write(ioout,'(a,/,a)')' 21','0.0'
        write(ioout,'(a,/,a)')' 31','0.0'
        write(ioout,'(a,/,a)')' 12','0.0'
        write(ioout,'(a,/,a)')' 22','0.0'
        write(ioout,'(a,/,a)')' 32','1.0'
        write(ioout,'(a,/,a)')' 13','0.0'
        write(ioout,'(a,/,a)')' 23','1.0'
        write(ioout,'(a,/,a)')' 33','1.0'
        write(ioout,'(a)')'  0'
        write(ioout,'(a)')'3DFACE'
        write(ioout,'(a)')'  8'
        write(ioout,'(a)')'0'
        write(ioout,'(a,/,a)')' 10','1.0'
        write(ioout,'(a,/,a)')' 20','1.0'
        write(ioout,'(a,/,a)')' 30','0.0'
        write(ioout,'(a,/,a)')' 11','0.0'
        write(ioout,'(a,/,a)')' 21','1.0'
        write(ioout,'(a,/,a)')' 31','0.0'
        write(ioout,'(a,/,a)')' 12','0.0'
        write(ioout,'(a,/,a)')' 22','1.0'
        write(ioout,'(a,/,a)')' 32','1.0'
        write(ioout,'(a,/,a)')' 13','1.0'
        write(ioout,'(a,/,a)')' 23','1.0'
        write(ioout,'(a,/,a)')' 33','1.0'
        write(ioout,'(a)')'  0'
        write(ioout,'(a)')'3DFACE'
        write(ioout,'(a)')'  8'
        write(ioout,'(a)')'0'
        write(ioout,'(a,/,a)')' 10','1.0'
        write(ioout,'(a,/,a)')' 20','0.0'
        write(ioout,'(a,/,a)')' 30','0.0'
        write(ioout,'(a,/,a)')' 11','1.0'
        write(ioout,'(a,/,a)')' 21','1.0'
        write(ioout,'(a,/,a)')' 31','0.0'
        write(ioout,'(a,/,a)')' 12','1.0'
        write(ioout,'(a,/,a)')' 22','1.0'
        write(ioout,'(a,/,a)')' 32','1.0'
        write(ioout,'(a,/,a)')' 13','1.0'
        write(ioout,'(a,/,a)')' 23','0.0'
        write(ioout,'(a,/,a)')' 33','1.0'
        write(ioout,'(a)')'  0'
        write(ioout,'(a)')'3DFACE'
        write(ioout,'(a)')'  8'
        write(ioout,'(a)')'0'
        write(ioout,'(a,/,a)')' 10','0.0'
        write(ioout,'(a,/,a)')' 20','0.0'
        write(ioout,'(a,/,a)')' 30','0.0'
        write(ioout,'(a,/,a)')' 11','1.0'
        write(ioout,'(a,/,a)')' 21','0.0'
        write(ioout,'(a,/,a)')' 31','0.0'
        write(ioout,'(a,/,a)')' 12','1.0'
        write(ioout,'(a,/,a)')' 22','0.0'
        write(ioout,'(a,/,a)')' 32','1.0'
        write(ioout,'(a,/,a)')' 13','0.0'
        write(ioout,'(a,/,a)')' 23','0.0'
        write(ioout,'(a,/,a)')' 33','1.0'
        write(ioout,'(a)')'  0'
        write(ioout,'(a)')'3DFACE'
        write(ioout,'(a)')'  8'
        write(ioout,'(a)')'0'
        write(ioout,'(a,/,a)')' 10','0.0'
        write(ioout,'(a,/,a)')' 20','0.0'
        write(ioout,'(a,/,a)')' 30','0.0'
        write(ioout,'(a,/,a)')' 11','0.0'
        write(ioout,'(a,/,a)')' 21','1.0'
        write(ioout,'(a,/,a)')' 31','0.0'
        write(ioout,'(a,/,a)')' 12','1.0'
        write(ioout,'(a,/,a)')' 22','1.0'
        write(ioout,'(a,/,a)')' 32','0.0'
        write(ioout,'(a,/,a)')' 13','1.0'
        write(ioout,'(a,/,a)')' 23','0.0'
        write(ioout,'(a,/,a)')' 33','0.0'
        write(ioout,'(a)')'  0'
        write(ioout,'(a)')'3DFACE'
        write(ioout,'(a)')'  8'
        write(ioout,'(a)')'0'
        write(ioout,'(a,/,a)')' 10','0.0'
        write(ioout,'(a,/,a)')' 20','0.0'
        write(ioout,'(a,/,a)')' 30','1.0'
        write(ioout,'(a,/,a)')' 11','1.0'
        write(ioout,'(a,/,a)')' 21','0.0'
        write(ioout,'(a,/,a)')' 31','1.0'
        write(ioout,'(a,/,a)')' 12','1.0'
        write(ioout,'(a,/,a)')' 22','1.0'
        write(ioout,'(a,/,a)')' 32','1.0'
        write(ioout,'(a,/,a)')' 13','0.0'
        write(ioout,'(a,/,a)')' 23','1.0'
        write(ioout,'(a,/,a)')' 33','1.0'
        write(ioout,'(a)')'  0'
        write(ioout,'(a)')'ENDBLK'
        write(ioout,'(a)')'  8'
        write(ioout,'(a)')'0'
        write(ioout,'(a)')'  0'
      endif
      write(ioout,'(a)')'ENDSEC'
      write(ioout,'(a)')'  0'
      write(ioout,'(a)')'SECTION'
      write(ioout,'(a)')'  2'
      write(ioout,'(a)')'ENTITIES'
      write(ioout,'(a)')'  0'

C Do conversion...
C For each zone in the list...
      INPIC=NCOMP
      do 54 mz=1,inpic

C Clear properties.
        NTMC=0
        nbobs(mz)=0

C Read in the zone geometry.
        WRITE(outs,'(a,a)')' Scanning : ',LGEOM(mz)
        CALL edisp(iuout,outs)
        call georead(ITA1,LGEOM(mz),mz,1,IUOUT,IER)
        call strup(zname(mz),ZN)

C Inclusion of obstructions.
        if(nbobs(mz).ge.1)then
          if(incobs.eq.0)goto 61
          do 43 ib=1,nbobs(mz)

C Convert block into an insertion and scaling for the block named
C obstruction (in the above BLOCK section).

C << what about 2 rotation obstruction? >>

C << what about 6 sided obstruction? >>

            write(ioout,'(a)')'INSERT'
            write(ioout,'(a)')'  8'
            write(ioout,'(a,a)') 'OBS_',ZN(:lnblnk(ZN))
            write(ioout,'(a)')'  2'
            write(ioout,'(a)')'OBSTRUCTION'
            write(ioout,'(a)')' 10'
            write(ioout,'(f9.1)')XOB(mz,IB)*1000.0
            write(ioout,'(a)')' 20'
            write(ioout,'(f9.1)')YOB(mz,IB)*1000.0
            write(ioout,'(a)')' 30'
            write(ioout,'(f9.1)')ZOB(mz,IB)*1000.0
            write(ioout,'(a)')' 41'
            write(ioout,'(f9.1)')DXOB(mz,IB)*1000.0
            write(ioout,'(a)')' 42'
            write(ioout,'(f9.1)')DYOB(mz,IB)*1000.0
            write(ioout,'(a)')' 43'
            write(ioout,'(f9.1)')DZOB(mz,IB)*1000.0
            write(ioout,'(a)')' 50'
            write(ioout,'(f9.1)')BANGOB(mz,IB,1)
            write(ioout,'(a)')'  0'
  43      continue

C Reread in the geometry file.
          call georead(ITA1,LGEOM(mz),mz,1,IUOUT,IER)
        endif
  61    continue

C Now surface information.  If there are tmc`s then put
C them in layer 'TRN_??' and put opaque in standard zone layer'

C To avoid mix of wire-frame lines and what looks like solid
C blocks use 3DPOLY for all surfaces in the model.
        do 75 isf=1,nsur
          iv=NVER(isf)
C          if(iv.eq.3.or.iv.eq.4)then
C            WRITE(ioout,'(A)')'3DFACE'
C            WRITE(ioout,'(A)')'  8'
C            if(SOTF(isf).eq.'TRAN')then
C              WRITE(ioout,'(A4,a)') 'TRN_',ZN(:lnblnk(ZN))
C            else
C              WRITE(ioout,'(A)') ZN(:lnblnk(ZN))
C            endif
C            inc = 0
C            DO 314 ibsv=1,iv
C              ii=JVN(isf,ibsv)
C              WRITE(ioout,'(I3)')10+inc
C              WRITE(ioout,'(f9.1)')X(II)*1000.0
C              WRITE(ioout,'(I3)')20+inc
C              WRITE(ioout,'(f9.1)')Y(II)*1000.0
C              WRITE(ioout,'(I3)')30+inc
C              WRITE(ioout,'(f9.1)')Z(II)*1000.0
C              inc = inc+1
C314         continue

C If a triangular surface duplicate the 4th corner.
C            if(iv.eq.3)then
C              ii=JVN(isf,3)
C              WRITE(ioout,'(a)') ' 13'
C              WRITE(ioout,'(f9.1)')X(ii)*1000.0
C              WRITE(ioout,'(a)') ' 23'
C              WRITE(ioout,'(f9.1)')Y(ii)*1000.0
C              WRITE(ioout,'(a)') ' 33'
C              WRITE(ioout,'(f9.1)')Z(ii)*1000.0
C            endif
C            write(ioout,'(a1)')'0'
C          else

C Write as a 3DPOLY with VERTEX.
            WRITE(ioout,'(A)')'POLYLINE'
            WRITE(ioout,'(A)')'  8'
            if(SOTF(isf).eq.'TRAN')then
              WRITE(ioout,'(A4,a)') 'TRN_',ZN(1:lnblnk(ZN))
            else
              WRITE(ioout,'(A)') ZN(1:lnblnk(ZN))
            endif
            WRITE(ioout,'(A)')' 66'
            WRITE(ioout,'(A)')'     1'
            write(ioout,'(a,/,a)')' 10','0.0'
            write(ioout,'(a,/,a)')' 20','0.0'
            write(ioout,'(a,/,a)')' 30','0.0'
            WRITE(ioout,'(A)')' 70'
            WRITE(ioout,'(A)')'     9'
            WRITE(ioout,'(A)')'  0'
            DO 311 ibsv=1,iv
              WRITE(ioout,'(A)')'VERTEX'
              WRITE(ioout,'(A)')'  8'
              WRITE(ioout,'(A)') ZN(1:lnblnk(ZN))
              ii=JVN(isf,ibsv)
              WRITE(ioout,'(a)') ' 10'
              WRITE(ioout,'(f10.1)')X(ii)*1000.0
              WRITE(ioout,'(a)') ' 20'
              WRITE(ioout,'(f10.1)')Y(ii)*1000.0
              WRITE(ioout,'(a)') ' 30'
              WRITE(ioout,'(f10.1)')Z(ii)*1000.0
              WRITE(ioout,'(A)')' 70'
              WRITE(ioout,'(A)')'    32'
              WRITE(ioout,'(A)')'  0'
 311        continue
            WRITE(ioout,'(A6)')'SEQEND'
            WRITE(ioout,'(A)')'  8'
            WRITE(ioout,'(A)')'0'
            WRITE(ioout,'(A)')'  0'
C          endif
 75     continue
  54  continue

C Close the dxf file.
      write(ioout,'(a6)')'ENDSEC'
      write(ioout,'(a)')'  0'
      write(ioout,'(a3)')'EOF'
      CALL ERPFREE(ioout,ISTAT)

      call edisp(iuout,' End of conversion.')
      CALL EPAGEND
      STOP

      END

C *************e2thf
C e2thf takes an esp-r problem definition and creates a
C THF (THINGS) file for use in MicroGDS.

      subroutine e2thf(incobs)
#include "building.h"
#include "model.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      COMMON/PRECT3/NTMC,NGLAZ(MTMC)

      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk

      integer indxzon
      character oformat*24,head*4
      common/dxfe/oformat,indxzon,head

      character ZN*12,outs*124
      integer llbm    ! length of block material

C Setup standard assumptions.
      IFIL=11

C Assume configuration file is from IFIL+5, any leakage description
C is fom IFIL+6, revised config file on IFIL+3, geometry on
C IFILE+2 and ASCII viewing and geometry file reading on IFILE+1. 
      ITA1 = IFIL+8

C If viewer output required.
      if(oformat(1:3).eq.'THF'.or.oformat(1:3).eq.'thf')then
        INPIC=NCOMP
        do 4 mz=1,inpic
          newfoc=mz

C Clear properties.
          NTMC=0
          nbobs(newfoc)=0

C Read in the zone geometry.
          WRITE(outs,'(a,a)')' Scanning : ',LGEOM(newfoc)
          CALL edisp(iuout,outs)
          call georead(ITA1,LGEOM(newfoc),newfoc,1,IUOUT,IER)
          call strup(zname(newfoc),ZN)

C Write surface information to the THF file.
          WRITE(ioout,'(a,i2)')'YIN ',NTV
          DO 40 J=1,NTV
            WRITE(ioout,'(3F9.4)')X(J),Y(J),Z(J)
   40     continue
          WRITE(ioout,'(a)')'0 '
          WRITE(ioout,'(I2)')NSUR
          DO 50 J=1,NSUR
            WRITE(ioout,'(a,a8)')'1 ',SMLCN(J)(1:8)
            WRITE(ioout,'(I3,1X,21I3)')NVER(J),(JVN(J,K),K=1,NVER(J))
   50     continue

          if(nbobs(newfoc).ge.1)then
            if(incobs.eq.0)goto 4
            DO 301 IB=1,nbobs(newfoc)

C Use the GB1 common block via CNVBLK
              if(BLOCKTYP(newfoc,IB)(1:4).eq.'obs ')then
                CALL CNVBLK(XOB(newfoc,IB),YOB(newfoc,IB),
     &            ZOB(newfoc,IB),DXOB(newfoc,IB),DYOB(newfoc,IB),
     &            DZOB(newfoc,IB),BANGOB(newfoc,IB,1))
              elseif(BLOCKTYP(newfoc,IB)(1:4).eq.'obs3')then
                CALL CNVBLK3A(XOB(newfoc,IB),YOB(newfoc,IB),
     &            ZOB(newfoc,IB),DXOB(newfoc,IB),DYOB(newfoc,IB),
     &            DZOB(newfoc,IB),BANGOB(newfoc,IB,1),
     &            BANGOB(newfoc,IB,2),BANGOB(newfoc,IB,3))
              elseif(BLOCKTYP(newfoc,IB)(1:4).eq.'obsp')then
                call CNVBLKP(newfoc,IB) ! convert obsp type.
              endif

C Write obstruction information as a clump of surfaces.
              WRITE(ioout,'(a)')'YIN  8'
              DO 240 J=1,8
                WRITE(ioout,'(3F9.4)')XB(J),YB(J),ZB(J)
  240         continue
              WRITE(ioout,'(a)')'0 '
              WRITE(ioout,'(a)')'6 '
              DO 250 J=1,6
                llbm=LNBLOCKMAT(newfoc,J)
                WRITE(ioout,'(2a)')'1 ',BLOCKMAT(newfoc,J)(1:llbm)
                WRITE(ioout,'(a,21I3)') '  4 ',(JVNB(J,K),K=1,4)
  250         continue
  301       continue
          ENDIF

C << Still to do - read and convert any ground topology. >>

C Free the THF file and reread in the geometry file.
          call georead(ITA1,LGEOM(newfoc),newfoc,1,IUOUT,IER)
   4    continue

        CALL ERPFREE(ioout,ISTAT)
        call edisp(iuout,' End of conversion.')
        CALL EPAGEND
        STOP
      endif
      END

C-----------------------------------------------------------------------
C lindir finds the angles associate with a line in space.
C The angles returned are the alpha and theta angles in degrees.
C Alpha lies in the XY plane and Theta measured from the XY plane.
C-----------------------------------------------------------------------
      SUBROUTINE LINDIR(X1,Y1,Z1,X2,Y2,Z2,ALPHA,THETA)

      logical close
      PI = 4.0 * ATAN(1.0)
      DIST=SQRT((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)
      THETA=ASIN((Z2-Z1)/DIST)
      call eclose(X1,X2,0.001,close)
      IF (close) THEN
         ALPHA=PI/2.
      ELSE
         ALPHA=ATAN((Y2-Y1)/(X2-X1))
      ENDIF
      THETA = THETA*180./PI
      ALPHA = ALPHA*180./PI

      RETURN
      END
