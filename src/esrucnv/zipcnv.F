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

C Read a `zip` geometry file and build an esp-r or __ product
C model.

C ******************** zipcnv
C EZIP reads `zip` geometry data and converts into esp-r
C product model.
C There are three levels of geometry data input corresponding
C to RECtangular (REC), REGular (REG) and GENeral (GEN) shaped zones.

C Window geometry is defined relative to a local coordinate
C system in the plane of the face in which the window is
C situated.
      SUBROUTINE zipcnv(upgcfg,LOUT,itrc,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/C3F/LCNN
      common/CFGV/icfgv
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C CTYPE (4 char) - zone shape type (REC, REG or GEN)
C gversion (real) version of the geometry file (1.0 legacy, 1.1 current).
      character CTYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

      COMMON/G4/NDP(MCOM),IDPN(MCOM,3)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/io/ioin,ioout,ioblk,iosblk,iotobs
      common/dxfe/oformat,indxzon,head

      DIMENSION XX(MS),YY(MS),ndum(MS),ddum(MS),ngld(MS)
      CHARACTER OUTSTR*124,WORD*20,zname*12,zdesc*64,phrase*64
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,upgcfg,LCFGF
      character GFILE*72,ZN*12,outs*124,tmp*124,LCNN*72,cfgroot*24
      character oformat*24,head*4,LOUT*72,SOTF*4,SN*12
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,dstmp*24
      logical okform,okant,zipok

      IER=0

      iotmp2=11        
      okform=.false.
      okant =.false.
      zipok =.false.

      if(NCOMP.eq.0)ICOMP=1
      IC=ICOMP

C First scan to see how many zones there will be and if
C it is a file we can read.
      nz=0
  42  read(ioin,'(A)',end=41,err=901) OUTSTR
      if(OUTSTR(1:13).eq.'COM data file')zipok=.true.
      if(OUTSTR(1:3).eq.'GEN')nz=nz+1
      if(OUTSTR(1:3).eq.'REC')nz=nz+1
      if(OUTSTR(1:3).eq.'REG')nz=nz+1
      if(OUTSTR(1:3).eq.'ESP')okform=.true.
      if(OUTSTR(1:3).eq.'ANT')okant =.true.
      goto 42
  41  continue
      rewind(ioin,ERR=999)
      write(outs,'(a,i3,a)')' found ',nz,' zones in zip file...'
      call edisp(iuout,outs)
      if(nz.gt.MCOM)then
        nz=MCOM
        write(outs,'(a,i3,a)')' will scan first ',nz,' zones...'
        call edisp(iuout,outs)
      endif
      
      if(zipok.and.okform.and.okant)then
      else
        call edisp(iuout,' not a usable zip file...')
        ier=1
        return
      endif

C We have a good file so open and generate the relevant part
C of the esp-r configuration file.

C If esp-r then generate initial portion of configuration file.
      if(oformat(1:3).eq.'esp')then
        LSNAM='Translation from XZIP'
        call mksyshd(LOUT,ier)
        if(upgcfg(1:7).eq.'upgrade')then
          write(ioout,'(I7,a)')nz,'  # no of zones'
          NCOMP=0
          NCON=0
        else
          write(ioout,'(I7,a)')nz,'  # no of zones'
        endif
      endif

C Loop some lines to get to ESP marker.  The word ANT might
C be on the next line (if so jump to 80, if not assume it
C might be zone information so jump to 81 avoiding re-read).
      do 15 ILL=1,10
        read(ioin,'(A)') OUTSTR
        if(OUTSTR(1:3).eq.'ESP')then
          read(ioin,'(A)') OUTSTR
          if(OUTSTR(1:3).eq.'ANT')then
            goto 80
          else
            goto 81
          endif
        else
          goto 15
        endif
        call edisp(iuout,' could not find ESP key word...')
        ier=1
        return
  15  continue


C Now begin the loop for zones. Zip uses the geometry type line
C to convey other information.  If two items on the line then the
C second is the zone name, if more items then not a proper name.
  80  CONTINUE
      read(ioin,'(A)',end=44,err=900) tmp
      OUTSTR=tmp(1:LNBLNK(tmp))
      CALL CHITMS(OUTSTR,ND)
      write(6,*)' ',outstr(1:40)
  81  continue
      K=0
      IF(ND.eq.1.or.ND.gt.2)THEN
        CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
        write(CTYPE(ic),'(a)')WORD(1:lnblnk(WORD))
        zname(IC)=' '
        zdesc(IC)=' '
      ELSEIF(ND.EQ.2)THEN
        CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
        write(CTYPE(ic),'(a)') WORD(1:lnblnk(WORD))
        CALL EGETW(OUTSTR,K,WORD,'W','Z name',IFLAG)
        ZN=WORD(1:12)
        call st2name(ZN,zname(IC))
        zdesc(IC)=' '
      ELSEIF(ND.gt.2)THEN
        CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
        write(CTYPE(IC),'(a)') WORD(1:lnblnk(WORD))
        CALL EGETW(OUTSTR,K,WORD,'W','Z name',IFLAG)
        ZN=WORD(1:12)
        call st2name(ZN,zname(IC))
        call egetrm(outstr,K,phrase,'W','Z description',IER)
        ZDESC(IC)=phrase
      ENDIF

C Fill in a default zone name if blank or UNKNOWN.
      IF(zname(IC)(1:2).EQ.'  '.OR.zname(IC)(1:7).EQ.'UNKNOWN')THEN
        IF(IC.LE.9)WRITE(zname(IC),'(A5,I1)')'Zone-',IC
        IF(IC.GT.9)WRITE(zname(IC),'(A5,I2)')'Zone-',IC
      ENDIF
      if(zdesc(IC)(1:1).EQ.' ')then
        write(zdesc(IC),'(a,a)') 
     &    zname(IC)(1:lnblnk(zname(IC))),' describes a...'
      endif

C Zone is of type REC, look for 7 items on one or two lines.
      IF(CTYPE(ic)(1:3).EQ.'REC')THEN
        read(ioin,*) XO,YO,ZO,DX,DY,DZ,AR

C Now convert to a gen description.
        CALL ERECC(XO,YO,ZO,DX,DY,DZ,AR)
        CTYPE(ic)='GEN '

C Zone is of type REG.
      ELSEIF(CTYPE(ic)(1:3).EQ.'REG')THEN
        read(ioin,*) NW,Z1,Z2,AR

C Read base vertex data.
        DO 60 IW=1,NW
          read(ioin,*) XX(IW),YY(IW)
   60   CONTINUE

C Now convert to Gen body type and place into in common block.
        CALL EREGC(NW,Z1,Z2,XX,YY)
        if(AR.LT.-.01.OR.AR.GT..01)then
          x1=X(1)
          y1=Y(1)
          CALL ESCROT(AR,x1,y1)
        endif
        CTYPE(ic)='GEN '

C Zone is of type GEN.
      ELSEIF(CTYPE(icomp)(1:3).EQ.'GEN')THEN
        read(ioin,*) NTV,NSUR,AR

C Read each vertex data line, strip any comments, see if 3 items and
C place in X(),Y(),Z().
        DO 62 I=1,NTV
          read(ioin,*) X(I),Y(I),Z(I)
   62   CONTINUE

C Read vertex list for each surface, strip comments, begin by finding
C the number of expected vertices (first item on list).
        DO 10 I=1,NSUR
          read(ioin,*) NVER(I),(JVN(I,KV),KV=1,NVER(I))
   10   CONTINUE

C Rotate if required.
        if(AR.LT.-.01.OR.AR.GT..01)then
          x1=X(1)
          y1=Y(1)
          CALL ESCROT(AR,x1,y1)
        endif
      ELSE
       write(outs,'(a,a)')' Shape type illegal in...',CTYPE(ic)
        call edisp(iuout,outs)
        goto 1001
      ENDIF

C Read windows in each surface data line(s).
      IRVA=NSUR
      read(ioin,*) (NGLD(KS),KS=1,IRVA)

C Skip over default window dimension details.
      DO 16 KS=1,NSUR
        IF(NGLD(KS).GT.0)THEN
          DO 18 KW=1,NGLD(KS)
            read(ioin,*) xx1,xx2,xx3,xx4
            call edisp(iuout,' WARNING: Default window skipped... ')
   18     CONTINUE
        ENDIF 
   16 CONTINUE

C Read doors in each surface data line(s), warn users to
C represent these as surfaces and ignore the data.
      IRVA=NSUR
      read(ioin,*) (ddum(KS),KS=1,IRVA)

C Read door dimension details and warn user to create a matching
C surface.
      DO 30 I=1,NSUR
        IF(ddum(I).gt.0)THEN
          read(ioin,*) xd,dxd,dzd
          call edisp(iuout,' WARNING: Default door skipped... ')
        ENDIF 
   30 CONTINUE

C Read default insolation surface numbers. NN = insolation defining 
C index:  1; one plane, 2; two planes, 3; all planes (diffuse).
C IDPN defines the default plane numbers.
      read(ioin,*) NDP(IC),(IDPN(IC,i),i=1,3)

      NZSUR(ic)=NSUR
      NZTV(ic)=NTV
      NCON=NCON+NSUR

C Data for zone complete, save this to a file before looping back
C to see if any other zones within this file.
C GFILE is the default file name for any geom file to be created,
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        IF(IC.LE.9)WRITE(GFILE,'(A5,I1,a4)')'Zone-',IC,'.geo'
        IF(IC.GT.9)WRITE(GFILE,'(A5,I2,a4)')'Zone-',IC,'.geo'
        LGEOM(IC)=GFILE
      else
        IF(IC.LE.9)WRITE(GFILE,'(a,a,A5,I1,a4)')
     &     zonepth(1:lnblnk(zonepth)),'/','Zone-',IC,'.geo'
        IF(IC.GT.9)WRITE(GFILE,'(a,a,A5,I2,a4)')
     &     zonepth(1:lnblnk(zonepth)),'/','Zone-',IC,'.geo'
        LGEOM(IC)=GFILE
      endif

C Write file names to cfg file. 
      if(oformat(1:3).eq.'esp')then
        WRITE(ioout,'(A,I3,A,A)',IOSTAT=IOS,ERR=2)
     &    '*zon ',NCCODE(IC),'   # reference for ',zname(IC)
        WRITE(ioout,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &    '*opr ',LPROJ(IC)(:LNBLNK(LPROJ(IC))),'  # schedules'
        WRITE(ioout,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &    '*geo ',GFILE(1:lnblnk(GFILE)),'  # geometry'
        WRITE(ioout,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &    '*con ',LTHRM(IC)(:LNBLNK(LTHRM(IC))),'  # construction'
        WRITE(ioout,'(A)',IOSTAT=IOS,ERR=2)'*zend '

C Create and write geometry file.
        ier=0
        CALL EFOPSEQ(iotmp2,LGEOM(IC),3,IER)
        write(iotmp2,'(a,a,a,a)')'# geometry of ',
     &       zname(ic)(1:lnblnk(zname(ic))),' defined in: ',
     &       LGEOM(IC)(1:lnblnk(LGEOM(IC)))
        write(iotmp2,'(a,a)')'GEN ',zname(ic)(1:lnblnk(zname(ic)))
        write(iotmp2,'(I8,I8,a)')NTV,NSUR,
     &      '   0.000    # vertices, surfaces, rotation angle'
        write(iotmp2,'(a)')'#  X co-ord, Y co-ord, Z co-ord '
        do 58 ivv=1,NTV
            write(iotmp2,'(3f10.3)')X(ivv),Y(ivv),Z(ivv)
  58    continue
        write(iotmp2,'(a)')'# no of vert & list of associated vert'
        do 59 is=1,NZSUR(ic)
          WRITE(iotmp2,5650)NVER(is),(JVN(is,J),J=1,NVER(is))
5650      FORMAT(1X,24(I3,','))
          ndum(is)=0
  59    continue
        write(iotmp2,'(a)')'# no of def windows within each surface'
        write(iotmp2,5611)(ndum(I),I=1,NSUR)
5611    FORMAT(1X,32I3)
        write(iotmp2,'(a)')'# surface indentation '
        write(iotmp2,5611)(ndum(I),I=1,NSUR)
        write(iotmp2,'(4i4,a)')NDP(IC),IDPN(IC,1),IDPN(IC,2),IDPN(IC,3),
     &    '   # def insolation distribution'

        write(iotmp2,'(a)')'# surface attributes follow: '
        write(iotmp2,'(a)')
     &      '# id  surface      geom  loc/  mlc db       environment'
        write(iotmp2,'(a)')
     &      '# no  name         type  posn  name         other side'
        do 65 is=1,NSUR
          IF(is.LE.9)WRITE(SN,'(a5,i1)')'Surf-',is
          IF(is.GT.9)WRITE(SN,'(a5,i2)')'Surf-',is
          SOTF='OPAQ'
          write(iotmp2,'(i3,a2,a12,a2,a4,a)')is,', ',SN,'  ',SOTF,
     &        '  UNKN  UNKNOWN      UNKNOWN'
  65    continue
        close(iotmp2)
      elseif(oformat(1:6).eq.'viewer')then

C Write surface information to the viewer file. First invert edges.
        DO 120 J=1,NSUR
          K=NVER(J)
          DO 130 L=1,K/2
            ITEMP=JVN(J,L)
            JVN(J,L)=JVN(J,K+1-L)
            JVN(J,K+1-L)=ITEMP
  130     CONTINUE
  120   CONTINUE

C Write to output file.
        WRITE(ioout,'(a3,1x,a12)')'GEN',zname(ic)
        WRITE(ioout,'(2I7)')NTV,NSUR
        DO 40 J=1,NTV
          WRITE(ioout,'(3F8.3)')X(J),Y(J),Z(J)
   40   CONTINUE
        DO 50 J=1,NSUR
          WRITE(ioout,'(2X,I3,1X,21I3)')NVER(J),(JVN(J,K),K=1,NVER(J))
   50   CONTINUE
      endif

C Increment the zone number and loop back to see if additional data.
      IC=IC+1
      if(IC.le.MCOM) goto 80


C Found end of file - Reset ICOMP.
   44 ier=0
      IC=IC-1
      ncomp=IC

C Write out the connections (assuming everything exterior).
      if(oformat(1:3).eq.'esp')then
        WRITE(ioout,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &    '*cnn  ',LCNN(1:lnblnk(LCNN)),'  # connections '
        CALL EFOPSEQ(iotmp2,LCNN,3,IER)
        WRITE(iotmp2,'(A,A)',IOSTAT=IOS,ERR=2) '*connections  for ',
     &    cfgroot(1:lnblnk(cfgroot))
        call dstamp(dstmp)
        WRITE(iotmp2,'(A,A)',IOSTAT=IOS,ERR=3) '*Date ',dstmp
        WRITE(iotmp2,'(I7,A)',IOSTAT=IOS,ERR=2)
     &    NCON,'   # number of connections'
        do 61 iv=1,ncomp
          do 66 is=1,NZSUR(iv)
            write(iotmp2,'(2i4,a)')iv,is,'   0   0   0'
  66      continue
  61    continue
        write(ioout,'(a)')'      0   # no mass flow analysis'
      endif
      close(iotmp2)
      write(outs,'(a,i3,a)')' Finished converting ',nz,' zones...'
      call edisp(iuout,outs)

    6 RETURN

 1001 write(outs,*)' End of file reached at...',WORD
      call edisp(iuout,outs)
      IER=1
      goto 6

 900  call edisp(iuout, 'Error reading input file')
      goto 6

 901  call edisp(iuout, 'Error in output file')
      goto 6

 999  call edisp(iuout, 'Error rewinding input file after scan...')
      goto 6

    2 CALL edisp(iuout,' Problem writing config data config...')
      goto 6
    3 CALL edisp(iuout,' Problem writing configuration name...')
      goto 6

      END


