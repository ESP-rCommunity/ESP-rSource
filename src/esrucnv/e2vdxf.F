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

C e2dxf takes an esp-r problem definition and creates either
C an AutoCad DXF file or viewer file.
C e2thf takes an esp-r problem definition and creates a
C THF (THINGS) file for use in MicroGDS.

      subroutine e2vdxf(itrc,incobs,ichop)
#include "building.h"

C geometry.h provides commons G0/G2/precz/GS5.
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/PRECT3/NTMC,NGLAZ(MTMC)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

      common/io/ioin,ioout,ioblk,iosblk,iotobs
      common/dxfe/oformat,indxzon,head
      logical doobs
      logical newgeo  ! to use for testing if new/old geometry file.

      character oformat*24,head*4,ZN*12,outs*124
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12

C Setup standard assumptions.
      newgeo=.false.  ! assume older format geometry.
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
          NB=0

C Read in the zone geometry.
          WRITE(outs,'(a,a)')' Scanning : ',LGEOM(newfoc)
          CALL edisp(iuout,outs)
          call eclose(gversion(newfoc),1.1,0.01,newgeo)
          if(newgeo)then
            call georead(ITA1,LGEOM(newfoc),newfoc,1,IUOUT,IER)
          else
            call egomin(ITA1,LGEOM(newfoc),newfoc,1,ITRC,IUOUT,IER)
          endif
          call strup(zname(newfoc),ZN)

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
            WRITE(ioout,'(3F8.3)')X(J),Y(J),Z(J)
   40     continue
          DO 50 J=1,NSUR
            WRITE(ioout,'(2X,I3,1X,21I3)')NVER(J),(JVN(J,K),K=1,NVER(J))
   50     continue

C Read in util & obstruction file.
          IF(IOBS(newfoc).EQ.1)CALL EGOMST(ITA1,ZOBS(newfoc),0,0,
     &      iuout,IER)

          if(NB.ge.1)then
            if(incobs.eq.0)goto 4
            DO 301 IB=1,NB
              CALL ERECC(XO(IB),YO(IB),ZO(IB),DX(IB),DY(IB),DZ(IB),
     &                 BANG(IB))

C Write obstruction information after inverting edges.
              DO 320 J=1,NSUR
                K=NVER(J)
                DO 330 L=1,K/2
                  ITEMP=JVN(J,L)
                  JVN(J,L)=JVN(J,K+1-L)
                  JVN(J,K+1-L)=ITEMP
  330           continue
  320         continue

C Write obstructions to output file.
              WRITE(ioout,'(a,a15,a)')'GEN ',ZN,' obstructions'
              WRITE(ioout,'(2I7)')NTV,NSUR
              DO 240 J=1,NTV
                WRITE(ioout,'(3F8.3)')X(J),Y(J),Z(J)
  240         continue
              DO 250 J=1,NSUR
                WRITE(ioout,'(I5,21I3)')NVER(J),(JVN(J,K),K=1,NVER(J))
  250         continue
  301       continue
          ENDIF

C Free the viewer file and eeread in the geometry file.
          call eclose(gversion(newfoc),1.1,0.01,newgeo)
          if(newgeo)then
            call georead(ITA1,LGEOM(newfoc),newfoc,1,IUOUT,IER)
          else
            call egomin(ITA1,LGEOM(newfoc),newfoc,1,ITRC,IUOUT,IER)
          endif
   4    continue

        CALL ERPFREE(ioout,ISTAT)
        call edisp(iuout,' End of conversion.')
        CALL EPAGEND
        STOP
      endif

C If dxf is output format.
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
      write(ioout,"('TABLE',/,'  2',/,'LTYPE',/,' 70',/,'     1')")
      write(ioout,'(a)')'  0'
      write(ioout,"('LTYPE',/,'  2',/,'CONTINUOUS',/,' 70')")
      write(ioout,'(a)')'    64'
      write(ioout,'(a)')'  3'
      write(ioout,"('Solid line',/,' 72',/,'    65',/,' 73')")
      write(ioout,'(a)')'     0'
      write(ioout,'(a)')' 40'
      write(ioout,'(a)')'0.0'
      write(ioout,'(a)')'  0'
      write(ioout,'(a)')'ENDTAB'

C Std zero layer.
      write(ioout,"('  0',/,'TABLE',/,'  2')")
      write(ioout,"('LAYER',/,' 70',/,'     4')")
      write(ioout,'(a)')'  0'
      write(ioout,"('LAYER',/,'  2',/,'0',/,' 70',/,'    64')")
      write(ioout,"(' 62',/,'     7',/,'  6',/,'CONTINUOUS')")
      write(ioout,'(a)')'  0'
      write(ioout,"('LAYER',/,'  2',/,'1',/,' 70',/,'    64')")
      write(ioout,"(' 62',/,'     7',/,'  6',/,'CONTINUOUS')")
      write(ioout,'(a)')'  0'

C Layer for each zone.
      INPIC=NCOMP
      do 14 mz=1,inpic
        call strup(zname(mz),ZN)
        write(ioout,'(a)')'LAYER'
        write(ioout,'(a)')'  2'
        write(ioout,'(a)')ZN(1:lnblnk(ZN))
        write(ioout,"(' 70',/,'    64')")
        write(ioout,"(' 62',/,'     7',/,'  6',/,'CONTINUOUS')")
        write(ioout,'(a)')'  0'

C If an obstruction for zone then make it the next layer.
C If there are transparent surfaces put them in another layer << .. >>
        if(IOBS(mz).EQ.1)then
          doobs=.TRUE.
          write(ioout,"('LAYER',/,'  2')")
          write(ioout,'(a4,a)') 'OBS_',ZN(1:lnblnk(ZN))
          write(ioout,"(' 70',/,'    64')")
          write(ioout,"(' 62',/,'     7',/,'  6',/,'CONTINUOUS')")
          write(ioout,'(a)')'  0'
        endif
        if(ITW(mz).EQ.1)then
          write(ioout,"('LAYER',/,'  2')")
          write(ioout,'(a4,a)') 'TRN_',ZN(1:lnblnk(ZN))
          write(ioout,"(' 70',/,'    64')")
          write(ioout,"(' 62',/,'     7',/,'  6',/,'CONTINUOUS')")
          write(ioout,'(a)')'  0'
        endif
  14  continue
      write(ioout,'(a)')'ENDTAB'
      write(ioout,'(a)')'  0'
      write(ioout,'(a)')'ENDSEC'

C Write a standard obstruction block in the block section if
C there are any *.obs files.
      write(ioout,'(a)')'  0'
      write(ioout,'(a)')'SECTION'
      write(ioout,'(a)')'  2'
      write(ioout,'(a)')'BLOCKS'
      write(ioout,'(a)')'  0'
      if(doobs)then
        write(ioout,"('BLOCK',/,'  8',/,'0')")
        write(ioout,"('  2',/,'OBSTRUCTION',/,' 70',/,'    64')")
        write(ioout,"(' 10',/,'0.0',/,' 20',/,'0.0',/,' 30',/,'0.0')")
        write(ioout,'(a)')'  3'
        write(ioout,'(a)')'OBSTRUCTION'
        write(ioout,'(a)')'  0'
        write(ioout,"('3DFACE',/,'  8',/,'0')")
        write(ioout,"(' 10',/,'0.0',/,' 20',/,'1.0',/,' 30',/,'0.0')")
        write(ioout,"(' 11',/,'0.0',/,' 21',/,'0.0',/,' 31',/,'0.0')")
        write(ioout,"(' 12',/,'0.0',/,' 22',/,'0.0',/,' 32',/,'1.0')")
        write(ioout,"(' 13',/,'0.0',/,' 23',/,'1.0',/,' 33',/,'1.0')")
        write(ioout,'(a)')'  0'
        write(ioout,"('3DFACE',/,'  8',/,'0')")
        write(ioout,"(' 10',/,'1.0',/,' 20',/,'1.0',/,' 30',/,'0.0')")
        write(ioout,"(' 11',/,'0.0',/,' 21',/,'1.0',/,' 31',/,'0.0')")
        write(ioout,"(' 12',/,'0.0',/,' 22',/,'1.0',/,' 32',/,'1.0')")
        write(ioout,"(' 13',/,'1.0',/,' 23',/,'1.0',/,' 33',/,'1.0')")
        write(ioout,'(a)')'  0'
        write(ioout,"('3DFACE',/,'  8',/,'0')")
        write(ioout,"(' 10',/,'1.0',/,' 20',/,'0.0',/,' 30',/,'0.0')")
        write(ioout,"(' 11',/,'1.0',/,' 21',/,'1.0',/,' 31',/,'0.0')")
        write(ioout,"(' 12',/,'1.0',/,' 22',/,'1.0',/,' 32',/,'1.0')")
        write(ioout,"(' 13',/,'1.0',/,' 23',/,'0.0',/,' 33',/,'1.0')")
        write(ioout,'(a)')'  0'
        write(ioout,"('3DFACE',/,'  8',/,'0')")
        write(ioout,"(' 10',/,'0.0',/,' 20',/,'0.0',/,' 30',/,'0.0')")
        write(ioout,"(' 11',/,'1.0',/,' 21',/,'0.0',/,' 31',/,'0.0')")
        write(ioout,"(' 12',/,'1.0',/,' 22',/,'0.0',/,' 32',/,'1.0')")
        write(ioout,"(' 13',/,'0.0',/,' 23',/,'0.0',/,' 33',/,'1.0')")
        write(ioout,'(a)')'  0'
        write(ioout,"('3DFACE',/,'  8',/,'0')")
        write(ioout,"(' 10',/,'0.0',/,' 20',/,'0.0',/,' 30',/,'0.0')")
        write(ioout,"(' 11',/,'0.0',/,' 21',/,'1.0',/,' 31',/,'0.0')")
        write(ioout,"(' 12',/,'1.0',/,' 22',/,'1.0',/,' 32',/,'0.0')")
        write(ioout,"(' 13',/,'1.0',/,' 23',/,'0.0',/,' 33',/,'0.0')")
        write(ioout,'(a)')'  0'
        write(ioout,"('3DFACE',/,'  8',/,'0')")
        write(ioout,"(' 10',/,'0.0',/,' 20',/,'0.0',/,' 30',/,'1.0')")
        write(ioout,"(' 11',/,'1.0',/,' 21',/,'0.0',/,' 31',/,'1.0')")
        write(ioout,"(' 12',/,'1.0',/,' 22',/,'1.0',/,' 32',/,'1.0')")
        write(ioout,"(' 13',/,'0.0',/,' 23',/,'1.0',/,' 33',/,'1.0')")
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
        NB=0

C Read in the zone geometry.
        WRITE(outs,'(a,a)')' Scanning : ',LGEOM(mz)
        CALL edisp(iuout,outs)
        call eclose(gversion(mz),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(ITA1,LGEOM(mz),mz,1,IUOUT,IER)
        else
          call egomin(ITA1,LGEOM(mz),mz,1,ITRC,IUOUT,IER)
        endif
        call strup(zname(mz),ZN)

C Read in util & obstruction file.
        IF(IOBS(mz).EQ.1)THEN
          CALL EGOMST(ITA1,ZOBS(mz),0,0,iuout,IER)
        endif

C Inclusion of obstructions.
        if(NB.ge.1)then
          if(incobs.eq.0)goto 61
          do 43 ib=1,NB

C Convert block into insertion and scaling for a block named
C obstruction.
            write(ioout,'(a)')'INSERT'
            write(ioout,'(a)')'  8'
            write(ioout,'(a,a)') 'OBS_',ZN(:lnblnk(ZN))
            write(ioout,'(a)')'  2'
            write(ioout,'(a)')'OBSTRUCTION'
            write(ioout,'(a)')' 10'
            write(ioout,'(f9.1)')XO(IB)*1000.
            write(ioout,'(a)')' 20'
            write(ioout,'(f9.1)')YO(IB)*1000.
            write(ioout,'(a)')' 30'
            write(ioout,'(f9.1)')ZO(IB)*1000.
            write(ioout,'(a)')' 41'
            write(ioout,'(f9.1)')DX(IB)*1000.
            write(ioout,'(a)')' 42'
            write(ioout,'(f9.1)')DY(IB)*1000.
            write(ioout,'(a)')' 43'
            write(ioout,'(f9.1)')DZ(IB)*1000.
            write(ioout,'(a)')' 50'
            write(ioout,'(f9.1)')BANG(IB)
            write(ioout,'(a)')'  0'
  43      continue

C Reread in the geometry file.
          call eclose(gversion(mz),1.1,0.01,newgeo)
          if(newgeo)then
            call georead(ITA1,LGEOM(mz),mz,1,IUOUT,IER)
          else
            call egomin(ITA1,LGEOM(mz),mz,1,ITRC,IUOUT,IER)
          endif
        endif
  61    continue

C Now surface information.  If there are tmc`s then put
C them in layer 'TRN_??' and opaque in zone layer'
        do 75 isf=1,nsur
          iv=NVER(isf)
          if(iv.eq.3.or.iv.eq.4)then
            WRITE(ioout,'(A)')'3DFACE'
            WRITE(ioout,'(A)')'  8'
            if(SOTF(isf).eq.'TRAN')then
              WRITE(ioout,'(A4,a)') 'TRN_',ZN(:lnblnk(ZN))
            else
              WRITE(ioout,'(A)') ZN(:lnblnk(ZN))
            endif
            inc = 0
            DO 314 ibsv=1,iv
              ii=JVN(isf,ibsv)
              WRITE(ioout,'(I3)')10+inc
              WRITE(ioout,'(f9.1)')X(II)*1000.
              WRITE(ioout,'(I3)')20+inc
              WRITE(ioout,'(f9.1)')Y(II)*1000.
              WRITE(ioout,'(I3)')30+inc
              WRITE(ioout,'(f9.1)')Z(II)*1000.
              inc = inc+1
314         continue

C If a triangular surface duplicate the 4th corner.
            if(iv.eq.3)then
              ii=JVN(isf,3)
              WRITE(ioout,'(a)') ' 13'
              WRITE(ioout,'(f9.1)')X(ii)*1000.
              WRITE(ioout,'(a)') ' 23'
              WRITE(ioout,'(f9.1)')Y(ii)*1000.
              WRITE(ioout,'(a)') ' 33'
              WRITE(ioout,'(f9.1)')Z(ii)*1000.
            endif
            write(ioout,'(a1)')'0'
          else

C More complex shape - write as a 3DPOLY with VERTEX.
            WRITE(ioout,"('POLYLINE',/,'  8')")
            if(SOTF(isf).eq.'TRAN')then
              WRITE(ioout,'(A4,a)') 'TRN_',ZN(1:lnblnk(ZN))
            else
              WRITE(ioout,'(A)') ZN(1:lnblnk(ZN))
            endif
            WRITE(ioout,'(A)')' 66'
            WRITE(ioout,'(A)')'     1'
            WRITE(ioout,"(' 10',/,'0.0',/,' 20',/,'0.0',/,' 30')")
            WRITE(ioout,'(A)')'0.0'
            WRITE(ioout,"(' 70',/,'     9')")
            WRITE(ioout,'(A)')'  0'
            DO 311 ibsv=1,iv
              WRITE(ioout,'(A6)')'VERTEX'
              WRITE(ioout,'(A)')'  8'
              WRITE(ioout,'(A)') ZN(1:lnblnk(ZN))
              ii=JVN(isf,ibsv)
              WRITE(ioout,'(a)') ' 10'
              WRITE(ioout,'(f9.1)')X(ii)*1000.
              WRITE(ioout,'(a)') ' 20'
              WRITE(ioout,'(f9.1)')Y(ii)*1000.
              WRITE(ioout,'(a)') ' 30'
              WRITE(ioout,'(f9.1)')Z(ii)*1000.
              WRITE(ioout,'(A)')' 70'
              WRITE(ioout,'(A)')'    32'
              WRITE(ioout,'(A)')'  0'
 311        continue
            WRITE(ioout,'(A6)')'SEQEND'
            WRITE(ioout,'(A)')'  8'
            WRITE(ioout,'(A)')'0'
            WRITE(ioout,'(A)')'  0'
          endif
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

      subroutine e2thf(itrc,incobs,ichop)
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/PRECT3/NTMC,NGLAZ(MTMC)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

      common/io/ioin,ioout,ioblk,iosblk,iotobs
      common/dxfe/oformat,indxzon,head
      LOGICAL doobs

      character oformat*24,head*4,ZN*12,outs*124
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      logical newgeo  ! to use for testing if new/old geometry file.

C Setup standard assumptions.
      newgeo=.false.  ! assume older format geometry.
      if(incobs.eq.1)doobs=.true.
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
          NB=0

C Read in the zone geometry.
          WRITE(outs,'(a,a)')' Scanning : ',LGEOM(newfoc)
          CALL edisp(iuout,outs)
          call eclose(gversion(newfoc),1.1,0.01,newgeo)
          if(newgeo)then
            call georead(ITA1,LGEOM(newfoc),newfoc,1,IUOUT,IER)
          else
            call egomin(ITA1,LGEOM(newfoc),newfoc,1,ITRC,IUOUT,IER)
          endif
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

C Read in obstruction file.
          IF(IOBS(newfoc).EQ.1)CALL EGOMST(ITA1,ZOBS(newfoc),0,0,
     &      iuout,IER)

          if(NB.ge.1)then
            if(incobs.eq.0)goto 4
            DO 301 IB=1,NB
              CALL ERECC(XO(IB),YO(IB),ZO(IB),DX(IB),DY(IB),DZ(IB),
     &                 BANG(IB))

C Write obstruction information as a clump of surfaces.
              WRITE(ioout,'(a,i2)')'YIN ',NTV
              DO 240 J=1,NTV
                WRITE(ioout,'(3F9.4)')X(J),Y(J),Z(J)
  240         continue
              WRITE(ioout,'(a)')'0 '
              WRITE(ioout,'(I2)')NSUR
              DO 250 J=1,NSUR
                WRITE(ioout,'(a,a8)')'1 ',BLKMAT(J)(1:8)
                WRITE(ioout,'(I3,21I3)')NVER(J),(JVN(J,K),K=1,NVER(J))
  250         continue
  301       continue
          ENDIF

C << Still to do - read and convert any ground topology. >>

C Free the THF file and reread in the geometry file.
          call eclose(gversion(newfoc),1.1,0.01,newgeo)
          if(newgeo)then
            call georead(ITA1,LGEOM(newfoc),newfoc,1,IUOUT,IER)
          else
            call egomin(ITA1,LGEOM(newfoc),newfoc,1,ITRC,IUOUT,IER)
          endif
   4    continue

        CALL ERPFREE(ioout,ISTAT)
        call edisp(iuout,' End of conversion.')
        CALL EPAGEND
        STOP
      endif
      END

C-----------------------------------------------------------------------
C THIS SUBROUTINE FINDS THE ANGLES ASSOCIATED WITH A LINE IN
C SPACE. THE ANGLES RETURNED ARE THE ALPHA AND THETA ANGLES IN DEGREES.
C ALPHA LIES IN THE XY PLANE AND THETA MEASURED FROM THE XY PLANE.
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
