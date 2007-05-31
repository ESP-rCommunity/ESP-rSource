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

C SILIST lists the contents of a shading/insolation file.
C SIEDIT edit the contents of a shading/insolation file.

C SILIST lists the contents of a shading/insolation file.
      SUBROUTINE SILIST(ICOMP)
#include "building.h"
      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/filep/ifil
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15,SN*12
      CHARACTER*3 month(12)
      character outs*124,H*72,tg*1,delim*1,xfile*144
      logical OK,DOK,close1,close2

C ntmc()=1 surface is source of insulation, ntnc()=0 not an insulation source.
C igc() is the offset to the insolation data.
C pso() is percentage shading for each hour (for the current surface)
C insst()=0, the whole surface is shaded; insst()=-1, the sun is not up,
C            otherwise the index of the surface being insolated.
C pinsst(i,j,k) proportion insolated from source i at hr j to insolated
C               surf k, for the receiving surface
      dimension ps(24),ishd(12),igc(ms),isadd(12),ntmc(ms)
      dimension insst(mgt,24,misur),pinsst(mgt,24,misur),
     &          pinwst(mgt,24,misur)

      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     &           'Aug','Sep','Oct','Nov','Dec'/

      ifilsi=ifil+3
      irec=1
      read(ifilsi,rec=irec,iostat=istat,err=1020)(ishd(i),i=1,12),
     &    (isadd(i),i=1,12)
      irec=2
      read(ifilsi,rec=irec,iostat=istat,err=900)irecx,nsurs,msurs
      if(nsur.ne.nsurs.or.msurs.ne.misur)goto 1010

C Assume no default windows so skip to record 4.
      irec=4
      read(ifilsi,rec=irec,iostat=istat,err=900)(ntmc(i),i=1,nsur)

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call edisp(icout,' Output being directed to file...')
      endif

C Display the contents of the shading/insolation file.
      WRITE(outs,'(A,A)')' Shading/Insolation file: ',LSHAD(ICOMP)
      call edisp(itru,outs)
      IMS=0
      DO 30 I=1,12
        if(ISHD(I).EQ.1)then
          WRITE(outs,'(2X,A,A)')MONTH(I),' - Shading'
          call edisp(itru,outs)
        elseif(ISHD(I).EQ.2)then
          WRITE(outs,'(2X,A,A)')MONTH(I),' -           Insolation'
          call edisp(itru,outs)
        elseif(ISHD(I).EQ.3)then
          WRITE(outs,'(2X,A,A)')MONTH(I),' - Shading   Insolation'
          call edisp(itru,outs)
        endif
        IF(ISHD(I).NE.0)IMS=IMS+1
        IF(ISHD(I).NE.0)MNTH=I
   30 CONTINUE
      IF(IMS.EQ.0)goto 2

C Ask if the output of month details required.
   31 dok=.true.
      h(1)='If you say yes you will be given further details '
      h(2)='about shading and insolation. '
      CALL ASKOK(' ',' Further details?',OK,dok,2)
      IF(.NOT.OK)goto  3
      IF(IMS.EQ.1)goto 50

   45 H(1)='Should be an integer Jan=1 etc. '
      CALL EASKI(MNTH,' ',' Month number ? ',
     &  1,'F',12,'F',1,'month number',IER,1)
      IF(IER.NE.0)goto  45
   50 IREC=ISADD(MNTH)
      IF(ISHD(MNTH).EQ.1.OR.ISHD(MNTH).EQ.3)goto 100
      IF(ISHD(MNTH).EQ.2)goto 200

C No data for this case.
      call edisp(iuout,' Sorry - no data for this month ')
      goto 31

C Output details of month shading.
  100 call edisp(itru,' ')
      WRITE(outs,'(A,A)')' Shading data for ',MONTH(MNTH)
      call edisp(itru,outs)
      call edisp(itru,'   Surface        24-hour shading time-series')
      IF(IREC.LT.5)goto 900
      DO 126 J=1,NSUR
        READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(PS(K),K=1,24)
        IREC=IREC+1

C Check to see if any of the values are > 0 and if so set a flag and
C print.
        LGTZ=0
        DO 226 L=1,24
          IF(PS(L).GT.0.0)LGTZ=1
  226   CONTINUE

        IF(LGTZ.GT.0)then
          WRITE(outs,124)SNAME(icomp,J),(PS(K),K=1,8)
  124     FORMAT(a,  ' 1- 8  ',8(F5.3,1X))
          call edisp(itru,outs)
          WRITE(outs,1125)(PS(K),K=9,16)
 1125     FORMAT(12X,'  9-16 ',8(F5.3,1X))
          call edisp(itru,outs)
          WRITE(outs,1126)(PS(k),K=17,24)
 1126     FORMAT(12X,' 17-24 ',8(F5.3,1X))
          call edisp(itru,outs)
        endif
  126 CONTINUE

C Window shading.  Calculate number of transparent surfaces.
      nwins=0
      do 199 i=1,nsur
        nwins=nwins+(ntmc(i))
  199 continue
      if(nwins.gt.0)then
        WRITE(outs,'(A,A)')' Shading data for ',MONTH(MNTH)
        call edisp(itru,outs)
        call edisp(itru,'   Glazings   24-hour shading time-series')
        call edisp(itru,' ')
      endif

      READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(IGC(J),J=1,NSUR)
      IRMS=IREC
      IREC=IREC+1
      DO 120 J=1,NSUR
        IF(IGC(J).EQ.0)goto 120
        IS=IGC(J)
        IREC=IRMS+IS
  120 CONTINUE

C Insolation case.
  200 IF(ISHD(MNTH).EQ.1)goto 300
      READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(IGC(J),J=1,NSUR)
      irec=irec+1
      do 201 i=1,nwins
        do 202 j=1,misur
        read(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(insst(i,n,j),n=1,24)
        IREC=IREC+1
  202   continue
        do 203 j=1,misur
        read(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(pinsst(i,n,j),n=1,24)
        IREC=IREC+1
  203   continue
        do 204 j=1,misur
        read(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(pinwst(i,n,j),n=1,24)
        IREC=IREC+1
  204   continue
  201 continue
  
C Calculate number of insolation sources (transparent) surfaces.
      iwin=0
      do 210 i=1,nsur
        if(igc(i).eq.0) goto 210
        do 211 k=1,ntmc(i)
          iwin=iwin+1
          SN=SNAME(icomp,i)
          call edisp(itru,'  ')
          write(outs,'(5a,i2,a)')' Insolation data for ',month(mnth),
     &      ' : Source surface ',SN(1:lnblnk(SN)),' (',i,')'
          call edisp(itru,outs)
          call edisp(itru,' Hour  Shading  Receiving       Percentage')
          call edisp(itru,'                Surface (index) Received')
          do 212 n=1,24
            if(insst(iwin,n,1).eq.-1)then
              write(outs,'(4X,A)')  '  Sun not up'
              if(n.eq.1.or.n.eq.24)call edisp(iuout,outs)
            elseif(insst(iwin,n,1).eq.0)then
              write(outs,'(I4,2x,2A)')n,SN(1:lnblnk(SN)),
     &          ' totally shaded'
              call edisp(itru,outs)
            else
              sumins=0
              do 213 j=1,misur
                sumins=sumins+pinsst(iwin,n,j)+pinwst(iwin,n,j)
                call eclose(pinsst(iwin,n,j),0.0,0.001,close1)
                call eclose(pinwst(iwin,n,j),0.0,0.001,close2)
                if((.NOT.close1).or.(.NOT.close2))then
                  if(j.eq.1)then
                    m=insst(iwin,n,j)
                    write(outs,1003)n,SNAME(icomp,m),m,
     &                pinsst(iwin,n,j)*100.0
 1003               format(i4,2x,a,' (',i2,')',f10.2)
                    call edisp(itru,outs)
                  else
                    m=insst(iwin,n,j)
                    write(outs,1004)SNAME(icomp,m),m,
     &                pinsst(iwin,n,j)*100.0
 1004               format(6x,a,' (',i2,')',f10.2)
                    call edisp(itru,outs)
                  endif
                endif
  213         continue
              if(abs(1.0-sumins).gt.0.02)then
                 write(outs,'(6x,2A,f5.2,a)') SN(1:lnblnk(SN)),
     &            ' partly shaded (',(1.0-sumins)*100.0,'%)'
                call edisp(itru,outs)
              endif
            endif
  212     continue
  211   continue
  210 continue

C Next month?
  300 goto 31

C Return.
    2 CALL EPAGEW
    3 RETURN

C Error handling.
  900 WRITE(outs,1009)IREC
 1009 FORMAT(' Shading/Insolation file error in SILIST - record',I6)
      call edisp(iuout,outs)
      goto 2

 1010 call edisp(iuout,' Geometry used in shading db is different to ')
      call edisp(iuout,' that specified for shading predictions.')
      goto 2

 1020 call edisp(iuout,' Empty file!')
      goto 2

      END

C***** SIEDIT edit the contents of a shading/insolation file.
C Also allow data to be imported and exported.
      SUBROUTINE SIEDIT(ICOMP)
#include "building.h"

      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/filep/ifil
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/DATA1/PSO(MS,24)

      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15
      CHARACTER*3 month(12)
      character outs*124,H*72,hold*74,expfil*72
      character outl*124,outld*124,OUTSTR*124,WORD*20
      logical OK,dok

      dimension ishd(12),igc(ms),isadd(12)

      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     &           'Aug','Sep','Oct','Nov','Dec'/

C Assume no default windows.
      ngldum=0
      ifilsi=ifil+3
      iuj=IFIL+10
      irec=1
      read(ifilsi,rec=irec,iostat=istat,err=1020)(ishd(i),i=1,12),
     &    (isadd(i),i=1,12)
      irec=2
      read(ifilsi,rec=irec,iostat=istat,err=900)irecx,nsurs,msurs
      if(nsur.ne.nsurs.or.msurs.ne.misur)goto 1010

C Assume no default windows so no more records to read.

C Display the contents of the shading/insolation file.
      WRITE(outs,'(A,A)')' Shading/Insolation file: ',LSHAD(ICOMP)
      IMS=0
      DO 30 I=1,12
        if(ISHD(I).EQ.1)then
          WRITE(outs,'(2X,A,A)')MONTH(I),' - Shading'
          call edisp(iuout,outs)
        elseif(ISHD(I).EQ.2)then
          WRITE(outs,'(2X,A,A)')MONTH(I),' -           Insolation'
          call edisp(iuout,outs)
        elseif(ISHD(I).EQ.3)then
          WRITE(outs,'(2X,A,A)')MONTH(I),' - Shading   Insolation'
          call edisp(iuout,outs)
        endif
        IF(ISHD(I).NE.0)IMS=IMS+1
        IF(ISHD(I).NE.0)MNTH=I
   30 CONTINUE
      IF(IMS.EQ.0)goto 2

C Ask if the output of month details required.
   31 h(1)='Edit option allows hourly data for one or more surfaces to'
      h(2)='be edited. Export dumps to text file, import reads from a'
      h(3)='similarly formatted file. '
      CALL EASKATOG(' Options:',' ','edit a month',
     &  'export','import','list','continue',' ',' ',ins1,3)
      if(ins1.eq.5)then
        goto  3
      elseif(ins1.eq.2)then

C Export the data and then start up an editor.
C << tell the user what to do >>
        CALL EASKS(expfil,'Export file name',' : ',72,'shad.data',
     &    'shding export',IER,1)
        CALL EFOPSEQ(iuj,expfil,3,IER)
        IF(IER.LT.0)THEN
          IER=1
          goto 31
        ENDIF
        WRITE(iuj,'(A)',IOSTAT=IOS,ERR=3) '*SHADING'
        WRITE(iuj,'(2i3,A)',IOSTAT=IOS,ERR=3) ICOMP,NSUR,' # zn & surfs'
      elseif(ins1.eq.3)then
        CALL EASKS(expfil,'Import file name',' : ',72,'shad.data',
     &    'shding import',IER,1)
        CALL EFOPSEQ(iuj,expfil,1,IER)
        IF(IER.LT.0)THEN
          IER=1
          goto 31
        ENDIF
        CALL STRIPC(iuj,OUTSTR,0,ND,1,'first line',IER)
        if(OUTSTR(1:8).ne.'*SHADING')then
          call usrmsg('This file is not an ascii shading file!',
     &     expfil,'W')
          CALL ERPFREE(iuj,istat)
          goto 31
        endif
        CALL STRIPC(iuj,OUTSTR,0,ND,1,'2nd line',IER)
        k=0
        CALL EGETWI(OUTSTR,K,iz,0,0,'-','zone index',IER)
        if(iz.ne.icomp)then
          call usrmsg('This data for a different zone!',expfil,'W')
          CALL ERPFREE(iuj,istat)
          goto 31
        endif
        CALL EGETWI(OUTSTR,K,is,0,0,'-','surface total',IER)
        if(is.ne.nsur)then
          call usrmsg('Wrong number of surfaces in file.',expfil,'W')
          CALL ERPFREE(iuj,istat)
          goto 31
        endif
      endif

C Display the current months data while reading database contents.
      IF(IMS.EQ.1)goto 50

   45 H(1)='Should be an integer Jan=1 etc. '
      CALL EASKI(MNTH,' ',' Month number ? ',
     &   1,'F',12,'F',1,'month number',IER,1)
      IF(IER.NE.0)goto 45
   50 IREC=ISADD(MNTH)
      IF(ISHD(MNTH).EQ.1.OR.ISHD(MNTH).EQ.3)goto 100
      IF(ISHD(MNTH).EQ.2)then
        call usrmsg('Sorry - only insolation data.',
     &              'Select again...','W')
        goto 45
      endif

C No data for this case.
      call edisp(iuout,' Sorry - no data for this month ')
      goto 31

C Output details of month shading.
  100 call edisp(iuout,' ')
      WRITE(outs,'(A,A)')' Current Shading data for ',MONTH(MNTH)
      call edisp(iuout,outs)
      call edisp(iuout,' Surface   24-hour shading time-series')
      IF(IREC.LT.5)goto 900
      DO 126 J=1,NSUR
        READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(PSO(J,K),K=1,24)
        IREC=IREC+1
        if(ins1.ne.3)then
          WRITE(outs,124)SNAME(icomp,J),(PSO(J,K),K=1,12)
  124     FORMAT(a,' 1- 12 ',12(F4.2,1X))
          call edisp(iuout,outs)
          WRITE(outs,1125)(PSO(J,K),K=13,24)
 1125     FORMAT(12X,' 13-24 ',12(F4.2,1X))
          call edisp(iuout,outs)
        endif
  126 CONTINUE

C Window shading, read but do not display (assumes no default windows).
      READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(IGC(J),J=1,NSUR)
      IRMS=IREC
      IREC=IREC+1
      DO 120 J=1,NSUR
        IF(IGC(J).EQ.0)goto 120
        IS=IGC(J)
        IREC=IRMS+IS
  120 CONTINUE
      if(ins1.eq.1)then
   47   H(1)='Should be index to surface. '
        CALL EASKI(ISE,' ',' Surface to edit ? ',
     &    1,'F',NSUR,'F',1,'surf number',IER,1)
        IF(IER.NE.0)goto 47
        write(outs,'(a,a,a)')'Shading: ',SNAME(icomp,ISE),' 1h00-12h00.'
        write(hold,'(12f6.3)') (PSO(ISE,K1),K1=1,12)
        h(1)='Shading data for hours 1-12. '
        CALL EASKS(hold,outs,' : ',74,' 0  0 0 0 0 0 0 0 0 0 0 0 ',
     &    '1-12 shding',IER,1)
        OK=.true.
        K=0
        do 42 ik=1,12
          CALL EGETWR(HOLD,K,PSO(ISE,ik),0.,1.,'W','1h00',IER)
          if(PSO(ISE,ik).lt.0.0.or.PSO(ISE,ik).gt.1.0)OK=.false.
  42    continue
        if(.NOT.OK)goto 47

       write(outs,'(a,a,a)')'Shading: ',SNAME(icomp,ISE),' 13h00-24h00.'
        write(hold,'(12f6.3)') (PSO(ISE,K1),K1=13,24)
        h(1)='Shading data for hours 13-24. '
        CALL EASKS(hold,outs,' : ',74,' 0 0 0 0 0 0 0 0 0 0 0 0 ',
     &    '13-24 shding',IER,1)
        OK=.true.
        K=0
        do 43 ik=13,24
          CALL EGETWR(HOLD,K,PSO(ISE,ik),0.,1.,'W','1h00',IER)
          if(PSO(ISE,ik).lt.0.0.or.PSO(ISE,ik).gt.1.0)OK=.false.
  43    continue
        if(.NOT.OK)goto 47

        dok=.false.
        h(1)='Say yes if you want to continue editing. '
        CALL ASKOK(' ',' Edit another surface?',OK,dok,1)
        if(OK)goto 47

        dok=.true.
        h(1)='Recent changes as held in memory and might be '
        h(2)='lost if you do not save to file. '
        CALL ASKOK(' ',' Update file ?',OK,dok,2)
        if(OK)then
          IREC=ISADD(MNTH)
          CALL SWRT(IREC)
        endif
      elseif(ins1.eq.2)then
        WRITE(iuj,'(A,i2)',IOSTAT=IOS,ERR=3) '*month ',MNTH
        do 44 is = 1,NSUR
          write(outl,'(12f7.4)') (PSO(IS,K1),K1=1,12)
          call SDELIM(outl,outld,'S',IW)
          WRITE(iuj,'(A)',IOSTAT=IOS,ERR=3) outld(1:lnblnk(outld))
          write(outl,'(12f7.4)') (PSO(IS,K1),K1=13,24)
          call SDELIM(outl,outld,'S',IW)
          WRITE(iuj,'(A)',IOSTAT=IOS,ERR=3) outld(1:lnblnk(outld))
  44    continue
        write(outs,'(a,a,a)') 'Data for ',MONTH(MNTH),' written.'
        call edisp(iuout,outs)
        dok=.true.
        h(1)='Unless this is December say yes. '
        CALL ASKOK('Export next month? (At last month say no',
     &    'and this will close the export file).',OK,dok,1)
        if(OK)then
          MNTH=MNTH+1
          if(MNTH.gt.12)then
            WRITE(iuj,'(A)',IOSTAT=IOS,ERR=3) '*end'
            CALL ERPFREE(iuj,istat)
            call edisp(iuout,'Export file close.')
            goto 31
          endif
          goto 50
        endif
        WRITE(iuj,'(A)',IOSTAT=IOS,ERR=3) '*end'
        CALL ERPFREE(iuj,istat)
        call edisp(iuout,'Export file close.')
        goto 31
      elseif(ins1.eq.3)then
        WRITE(outs,'(A,A)')' Imported Shading data for ',MONTH(MNTH)
        call edisp(iuout,outs)
  78    continue
        CALL STRIPC(iuj,OUTSTR,0,ND,1,'month line',IFLG)
        if(iflg.ne.0)goto 1030
        if(OUTSTR(1:4).eq.'*end')goto 1030
        if(OUTSTR(1:6).eq.'*month')then
          k=0
          CALL EGETW(OUTSTR,K,WORD,'-','month',IER)
          CALL EGETWI(OUTSTR,K,ivm,0,0,'-','month index',IER)
          if(ivm.eq.MNTH)then
            do 79 is=1,nsur
              CALL STRIPC(iuj,OUTSTR,0,ND,1,'data line 1',IFLG)
              k=0
              do 80 iss=1,12
                CALL EGETWR(OUTSTR,K,V,0.,1.,'W','data 1',IER)
                PSO(is,iss)=V
  80          continue
              WRITE(outs,124)SNAME(icomp,is),(PSO(is,K1),K1=1,12)
              call edisp(iuout,outs)
              CALL STRIPC(iuj,OUTSTR,0,ND,1,'data line 2',IFLG)
              k=0
              do 81 iss=13,24
                CALL EGETWR(OUTSTR,K,V,0.,1.,'W','data 2',IER)
                PSO(is,iss)=V
  81          continue
              WRITE(outs,1125)(PSO(is,K1),K1=13,24)
              call edisp(iuout,outs)
  79        continue
            CALL ERPFREE(iuj,istat)
            call edisp(iuout,'Import file closed.')
            dok=.true.
            h(1)='Imported data is held in memory. It will be lost '
            h(2)='unless you update the file. '
            CALL ASKOK(' ',' Update file ?',OK,dok,2)
            if(OK)then
              IREC=ISADD(MNTH)
              CALL SWRT(IREC)
            endif
            goto 31
          else
            goto 78
          endif
        else
          goto 78
        endif
      elseif(ins1.eq.3)then
        goto 31
      endif

C Next month?
      goto 31

C Return.
    2 CALL EPAGEW
    3 RETURN

C Error handling.
  900 WRITE(outs,1009)IREC
 1009 FORMAT(' Shading/Insolation file error in SILIST - record',I6)
      call edisp(iuout,outs)
      goto 2

 1010 call edisp(iuout,' Geometry used in shading db is different to ')
      call edisp(iuout,' that specified for shading predictions.')
      goto 2

 1020 call edisp(iuout,' Empty file!')
      goto 2

 1030 call edisp(iuout,' End of import file reached.')
      CALL ERPFREE(iuj,istat)
      goto 31
      
      END
