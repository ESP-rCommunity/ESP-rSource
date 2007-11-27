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
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)

      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER SN*12
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
      if(nzsur(icomp).ne.nsurs.or.msurs.ne.misur)goto 1010

C Assume no default windows so skip to record 4.
      irec=4
      read(ifilsi,rec=irec,iostat=istat,err=900)
     &  (ntmc(i),i=1,nzsur(icomp))

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call edisp(icout,'Output being directed to file.')
      endif

C Display the contents of the shading/insolation file.
      call edisp(itru,' ')
      WRITE(outs,'(3A)')'Content of zone shading & insolation file `',
     &                   LSHAD(ICOMP),'`.'
      call edisp(itru,outs)
      IMS=0
      MNTH=1
      IFIRST=0
      DO 30 I=1,12
        if(ISHD(I).EQ.1)then
          WRITE(outs,'(2X,A,A)')MONTH(I),' - Shading'
          call edisp(itru,outs)
        elseif(ISHD(I).EQ.2)then
          WRITE(outs,'(2X,A,A)')MONTH(I),' -           Insolation'
          call edisp(itru,outs)
        elseif(ISHD(I).EQ.3)then
          WRITE(outs,'(2X,A,A)')MONTH(I),' - Shading & Insolation'
          call edisp(itru,outs)
        endif
        if(ISHD(I).NE.0)then
           IMS=IMS+1
           if(IFIRST.EQ.0)then
              MNTH=I
              IFIRST=1
           endif
        endif
   30 CONTINUE
      IF(IMS.EQ.0)goto 2

C Ask if the output of month details are required.
   31 dok=.true.
      h(1)='Answer `yes` if you want details on the shading and/or'
      h(2)='insolation patterns for a specific month.'
      CALL ASKOK(' ','Month details?',OK,dok,2)
      IF(.NOT.OK)goto  3
      IF(IMS.EQ.1)goto 50

   45 H(1)='Specify the month as an integer where Jan=1, Feb=2 etc.'
      CALL EASKI(MNTH,' ','Month number?',
     &              1,'F',12,'F',1,'month number',IER,1)
      IF(IER.NE.0)goto  45
   50 IREC=ISADD(MNTH)
      IF(ISHD(MNTH).EQ.1.OR.ISHD(MNTH).EQ.3)goto 100
      IF(ISHD(MNTH).EQ.2)goto 200

C No data for this case.
      call edisp(iuout,'Sorry - no data available for this month!')
      goto 31

C Shading case.
  100 WRITE(outs,'(3A)')'External surface shading for ',MONTH(MNTH),
     &                  '.'
      call edisp(itru,outs)
      call edisp(itru,'Surface       Shading Factors')
      IF(IREC.LT.5)goto 900
      DO 126 J=1,NZSUR(icomp)
        ICN=IZSTOCN(icomp,j)
        READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(PS(K),K=1,24)
        IREC=IREC+1

C Check to see if any of the values are > 0 and if so set a flag and
C print.
        LGTZ=0
        DO 226 L=1,24
          IF(PS(L).GT.0.0)LGTZ=1
  226   CONTINUE

        IF(LGTZ.GT.0)then
          SN=SSNAME(icn)
          WRITE(outs,124)J,SN(1:7),(PS(K),K=1,12)
  124     FORMAT(I2,' (',a,')  01:00-12:00 ',12(F4.1))
          call edisp(itru,outs)
          WRITE(outs,1125)(PS(K),K=13,24)
 1125     FORMAT(14X,'13:00-24:00 ',12(F4.1))
          call edisp(itru,outs)
        endif
  126 CONTINUE

C Default window case.
      nwins=0
      do 199 i=1,nzsur(icomp)
        nwins=nwins+(ntmc(i))
  199 continue
C      if(nwins.gt.0)then
C        WRITE(outs,'(A,A)')'Shading for ',MONTH(MNTH)
C        call edisp(itru,outs)
C        call edisp(itru,'Glazings   Shading factors')
C      endif

      READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(
     &  IGC(J),J=1,NZSUR(icomp))
      IRMS=IREC
      IREC=IREC+1
      DO 120 J=1,NZSUR(icomp)
        IF(IGC(J).EQ.0)goto 120
        IS=IGC(J)
        IREC=IRMS+IS
  120 CONTINUE

C Insolation case.
  200 IF(ISHD(MNTH).EQ.1)goto 300
      READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)
     &  (IGC(J),J=1,NZSUR(icomp))
      irec=irec+1
      do 201 i=1,nwins
        do 202 j=1,misur
        read(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(insst(i,n,j),n=1,24)
        IREC=IREC+1
  202   continue
        do 203 j=1,misur
        read(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(pinsst(i,n,j),
     &                                             n=1,24)
        IREC=IREC+1
  203   continue
        do 204 j=1,misur
        read(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(pinwst(i,n,j),
     &                                             n=1,24)
        IREC=IREC+1
  204   continue
  201 continue
  
C Calculate number of insolation sources (transparent surfaces).
      iwin=0
      do 210 i=1,nzsur(icomp)
        ICN=IZSTOCN(icomp,i)
        if(igc(i).eq.0) goto 210
        do 211 k=1,ntmc(i)
          iwin=iwin+1
          write(outs,'(3A)')'Internal surface insolation for `',
     &                       month(mnth),'`.'
          call edisp(itru,outs)
          SN=SSNAME(icn)
          write(outs,'(A,I2,3A)')'Source surface is ',i,' (',
     &                            SN(1:lnblnk(SN)),').'
          call edisp(itru,outs)
          call edisp(itru,' Hour  Receiving       Proportion')
          call edisp(itru,'       Surface         Received')
          IPRINT=0
          do 212 n=1,24
            if(insst(iwin,n,1).eq.-1)then
              IF(IPRINT.EQ.2)then
                write(outs,'(I2,A)')n,':00  After sunset'
                call edisp(itru,outs)
C Don't write again.
                IPRINT=-1
              else
                IPRINT=1
              endif
            elseif(insst(iwin,n,1).eq.0)then
              IF(IPRINT.EQ.1)then
               write(outs,'(I2,A)')n-1,':00  Before sunrise'
               call edisp(itru,outs)
              ENDIF
              write(outs,'(I2,A)')n,':00   100.0%'
              call edisp(itru,outs)
              IPRINT=2
            else
              IF(IPRINT.EQ.1)then
                write(outs,'(I2,A)')n-1,':00  Before sunrise'
                call edisp(itru,outs)
              ENDIF
              sumins=0
              do 213 j=1,misur
                sumins=sumins+pinsst(iwin,n,j)+pinwst(iwin,n,j)
                call eclose(pinsst(iwin,n,j),0.0,0.001,close1)
                call eclose(pinwst(iwin,n,j),0.0,0.001,close2)
                if((.NOT.close1).or.(.NOT.close2))then
                  if(j.eq.1)then
                    m=insst(iwin,n,j)
                    ICM=IZSTOCN(icomp,m)
                    SN=SSNAME(icm)
                    write(outs,1003)n,m,SN(1:7),pinsst(iwin,n,j)*100.0
 1003               format(i2,':00  ',i2,' (',a,')',f10.1,'%')
                    call edisp(itru,outs)
                  else
                    m=insst(iwin,n,j)
                    ICM=IZSTOCN(icomp,m)
                    SN=SSNAME(icm)
                    write(outs,1004)m,SN(1:7),pinsst(iwin,n,j)*100.0
 1004               format(7x,i2,' (',a,')',f10.1,'%')
                    call edisp(itru,outs)
                  endif
                endif
  213         continue
              IPRINT=2
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
 1009 FORMAT('Shading/Insolation file error in SILIST - record',I6)
      call edisp(iuout,outs)
      goto 2

 1010 call edisp (iuout,' ')
      call edisp(iuout,'Geometry referenced in Zone Shading &')
      call edisp(iuout,'Insolation file is different to that')
      call edisp(iuout,'currently specified.')
      goto 2

 1020 call edisp(iuout,'Empty file!')
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
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/DATA1/PSO(MS,24)

      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER SN*12
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
      if(nzsur(icomp).ne.nsurs.or.msurs.ne.misur)goto 1010

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
   31 h(1)='Edit option allows the hourly shading factors for'
      h(2)='specified surfaces to be altered, the export option'
      h(3)='transfers the shading factors from the zone shading'
      h(4)='and insolation file to a text file (where they can'
      h(5)='be edited, while the import options executes the'
      h(6)='reverse transfer. The list option lists the contents'
      h(7)='of the zone shading and insolation file'
      CALL EASKATOG(' ','Options:','edit a month',
     &  'export','import','list','cancel',' ',' ',ins1,7)
      if(ins1.eq.5)then
        goto  3
      elseif(ins1.eq.2)then

C Export the data and then start up an editor.
        H(1)='A text file to which the shading factors will'
        H(2)='be written.'
        expfil='shd.txt'
        CALL EASKS(expfil,' ','Export file name?',72,'shad.txt',
     &    'shding export',IER,2)
        CALL EFOPSEQ(iuj,expfil,3,IER)
        IF(IER.LT.0)THEN
          IER=1
          goto 31
        ENDIF
        WRITE(iuj,'(A)',IOSTAT=IOS,ERR=3) '*SHADING'
        WRITE(iuj,'(2i3,A)',IOSTAT=IOS,ERR=3) ICOMP,NZSUR(icomp),
     &    ' # zn & surfs'
      elseif(ins1.eq.3)then
        CALL EASKS(expfil,' ','Import file name?',72,'shad.data',
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
        if(is.ne.nzsur(icomp))then
          call usrmsg('Wrong number of surfaces in file.',expfil,'W')
          CALL ERPFREE(iuj,istat)
          goto 31
        endif
      endif

C Display the current months data while reading database contents.
      IF(IMS.EQ.1)goto 50

   45 H(1)='The month is specified as an integer where Jan=1,'
      H(2)='Feb=2 etc.'
      CALL EASKI(MNTH,' ',' Month number?',
     &   1,'F',12,'F',1,'month number',IER,2)
      IF(IER.NE.0)goto 45
   50 IREC=ISADD(MNTH)
      IF(ISHD(MNTH).EQ.1.OR.ISHD(MNTH).EQ.3)goto 100
      IF(ISHD(MNTH).EQ.2)then
        call usrmsg('Sorry - only insolation data.',
     &              'Select again.','W')
        goto 45
      endif

C No data for this case.
      call edisp(iuout,' Sorry - no data for this month ')
      goto 31

C Output details of month shading.
  100 call edisp(iuout,' ')
      WRITE(outs,'(3A)')'External surface shading for ',MONTH(MNTH),
     &                    '.'
      call edisp(iuout,outs)
      call edisp(iuout,'Surface       Shading Factors')
      IF(IREC.LT.5)goto 900
      DO 126 J=1,NZSUR(icomp)
        ICN=IZSTOCN(icomp,j)
        READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(PSO(J,K),K=1,24)
        IREC=IREC+1
        if(ins1.ne.3)then
          SN=SSNAME(icn)
          WRITE(outs,124)J,SN(1:7),(PSO(J,K),K=1,12)
  124     FORMAT(I2,' (',a,')  01:00-12:00 ',12(F4.1))
          call edisp(iuout,outs)
          WRITE(outs,1125)(PSO(J,K),K=13,24)
 1125     FORMAT(14X,'13:00-24:00 ',12(F4.1))
          call edisp(iuout,outs)
        endif
  126 CONTINUE

C Window shading, read but do not display (assumes no default windows).
      READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)
     &  (IGC(J),J=1,NZSUR(icomp))
      IRMS=IREC
      IREC=IREC+1
      DO 120 J=1,NZSUR(icomp)
        IF(IGC(J).EQ.0)goto 120
        IS=IGC(J)
        IREC=IRMS+IS
  120 CONTINUE
      if(ins1.eq.1)then
   47   H(1)='Integer number of surface within model.'
        CALL EASKI(ISE,' ','Surface number?',
     &             1,'F',NZSUR(icomp),'F',1,'surf number',IER,1)
        IF(IER.NE.0)goto 47
        ICN=IZSTOCN(icomp,ise)
        SN=SSNAME(icn)
        write(outs,'(3A)')'Shading factors for surface `',
     &                        SN(1:lnblnk(SN)),'`,'
        write(hold,'(12f5.2)') (PSO(ISE,K1),K1=1,12)
        h(1)='Shading factors for hours 01:00-12:00.'
        CALL EASKS(hold,outs,'01:00-12:00; edit as required.',
     &     60,' 0  0 0 0 0 0 0 0 0 0 0 0 ','1-12 shding',IER,1)
        OK=.true.
        K=0
        do 42 ik=1,12
          CALL EGETWR(HOLD,K,PSO(ISE,ik),0.,1.,'W','1h00',IER)
          if(PSO(ISE,ik).lt.0.0.or.PSO(ISE,ik).gt.1.0)OK=.false.
  42    continue
        if(.NOT.OK)goto 47

        ICN=IZSTOCN(icomp,ise)
        SN=SSNAME(icn)
        write(outs,'(3A)')'Shading factors for surface `',
     &                       SN(1:lnblnk(SN)),'`,'
        write(hold,'(12f5.2)') (PSO(ISE,K1),K1=13,24)
        h(1)='Shading factors for hours 13:00-24:00.'
        CALL EASKS(hold,outs,'13:00-24:00; edit as required.',
     &     60,' 0 0 0 0 0 0 0 0 0 0 0 0 ','13-24 shding',IER,1)
        OK=.true.
        K=0
        do 43 ik=13,24
          CALL EGETWR(HOLD,K,PSO(ISE,ik),0.,1.,'W','1h00',IER)
          if(PSO(ISE,ik).lt.0.0.or.PSO(ISE,ik).gt.1.0)OK=.false.
  43    continue
        if(.NOT.OK)goto 47

        dok=.false.
        h(1)='Option to edit the shading factors for another surface.'
        CALL ASKOK(' ','Edit another surface?',OK,dok,1)
        if(OK)goto 47

        dok=.true.
        h(1)='Unless saved to the zone shading & insolation file, the'
        h(2)='edited shading factors will not be preserved.'
        CALL ASKOK(' ','Update shading/insolation file?',OK,dok,2)
        if(OK)then
          IREC=ISADD(MNTH)
          CALL SWRT(IREC)
        endif
      elseif(ins1.eq.2)then
        WRITE(iuj,'(A,i2)',IOSTAT=IOS,ERR=3) '*month ',MNTH
        do 44 is = 1,NZSUR(icomp)
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
        h(1)='Unless this is December say yes.'
        CALL ASKOK(' ','Continue with next month?.',OK,dok,1)
        if(OK)then
          MNTH=MNTH+1
          if(MNTH.gt.12)then
            WRITE(iuj,'(A)',IOSTAT=IOS,ERR=3) '*end'
            CALL ERPFREE(iuj,istat)
            call edisp(iuout,' ')
            call edisp(iuout,'Export file closed.')
            goto 31
          endif
          goto 50
        endif
        WRITE(iuj,'(A)',IOSTAT=IOS,ERR=3) '*end'
        CALL ERPFREE(iuj,istat)
        call edisp(iuout,'Export file closed.')
        goto 31
      elseif(ins1.eq.3)then
        WRITE(outs,'(A)')'Imported factors.'
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
            do 79 is=1,nzsur(icomp)
              ICN=IZSTOCN(icomp,is)
              CALL STRIPC(iuj,OUTSTR,0,ND,1,'data line 1',IFLG)
              k=0
              do 80 iss=1,12
                CALL EGETWR(OUTSTR,K,V,0.,1.,'W','data 1',IER)
                PSO(is,iss)=V
  80          continue
              SN=SSNAME(icn)
              WRITE(outs,124)is,SN(1:7),(PSO(is,K1),K1=1,12)
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
            h(1)='Imported data is held in memory and will be lost'
            h(2)='unless you update the zone shading & insolation'
            h(3)='file.'
            CALL ASKOK(' ','Update zone shading & insolation file?',
     &                  OK,dok,3)
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
