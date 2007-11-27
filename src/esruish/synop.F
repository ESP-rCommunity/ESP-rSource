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

C SYNOP.F of ESPshd contains the following routines:

C SYNOP:  Display table of % shading for opaque & transparent surfaces.
C INSYNP: Display synoptic information on the internal surfaces insolated.
C SILISTI: Lists the contents of a shading/insolation file.

C ******************* SYNOP 
C SYNOP displays a table showing the percentage opaque
C and transparent surface shading for each hour and surface
C held within the transitional shading file.
      SUBROUTINE SYNOP(ICOMP)
#include "building.h"
      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL
      COMMON/CONTR/MON,ISC(MS),IYD
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/HEADL/IRECS(MS)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)

      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      DIMENSION XFT(MV),ZFT(MV),IOSHD(MOX,MOZ)
      DIMENSION MTHNAM(12)

      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER h*72
      CHARACTER MTHNAM*3,BLKNAME*8,BLKMAT*12,outs*124,SN*12
      character tg*1,delim*1,xfile*144
      logical OK,dok

      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call edisp(icout,'Output being directed to file.')
      endif

      IUNIT=IFIL
      call edisp(itru,' ')
      WRITE(outs,'(3A)')'External surface shading for ',MTHNAM(MON),
     &                    '.'
      call edisp(itru,outs)

C Retrieve shading information and display on a surface-by-surface
C basis.
      DO 10 I=1,NZSUR(icomp)
        ICN=IZSTOCN(icomp,i)
        IF(ISC(I).EQ.0)goto 10
        IREC=IRECS(I)
        ISHDT=0
        ISHDH=0
        NV=NVER(I)  ! Is there a connection based variable for this?

C Retrieve surface transformed coordinates.
        READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(XFT(J),J=1,NV)
        IREC=IREC+1
        READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(ZFT(J),J=1,NV)
        IREC=IREC+1

C Compute gross surface area.
        CALL AREA(NV,XFT,ZFT,GSA)

C Write output header information.
        SN=SSNAME(ICN)
        WRITE(outs,'(A,I2,3A)') 'Surface ',I,
     &                      ' (',SN(1:lnblnk(SN)),')'
        call edisp(itru,outs)
        WRITE(outs,'(3a)') ' Hour   Shading'
        call edisp(itru,outs)

C Recover calculated shading for the current surface hour-by-hour.
        IPRINT=0
        DO 20 J=1,24
          READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)ISUNUP
          IREC=IREC+1
          IF(ISUNUP.EQ.0)goto 7
          goto 8

C Sun not up.
    7     IF(IPRINT.EQ.2)then
            write(outs,'(I2,A)')J,':00   After sunset'
            call edisp(itru,outs)
C Don't write again.
            IPRINT=-1
          else
            IPRINT=1
          endif
          goto 20

C Determine if surface is totally shaded or insolated.
    8     IF(IPRINT.EQ.1)then
             write(outs,'(I2,A)')J-1,':00   Before sunrise'
             call edisp(itru,outs)
          ENDIF
          READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)INDS
          IREC=IREC+1
          IF(INDS.EQ.0)goto 11
          IF(INDS.EQ.-1)goto 12

C No shading case.
          WRITE(outs,'(I2,A)')J,':00      0%'
          call edisp(itru,outs)
          ISHDH=ISHDH+1
          IPRINT=2
          goto 20

C Fully shaded case.
   12     WRITE(outs,'(I2,A)')J,':00    100%'
          call edisp(itru,outs)
          ISHDT=ISHDT+100
          ISHDH=ISHDH+1
          IPRINT=2
          goto 20

C Mixed shading/insolation case, fill IOSHD.
   11     IOPCNT=0
          DO 30 K=1,NOX
          READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(IOSHD(K,L),L=1,NOZ)
            IREC=IREC+1
            DO 40 L=1,NOZ
              IF(IOSHD(K,L).EQ.1)IOPCNT=IOPCNT+1
   40       CONTINUE
   30     CONTINUE

C Assuming no default windows, no need to read IGSHD.
          continue

C Compute gross surface shading area.
          XMAX=-1.E+10
          ZMAX=-1.E+10
          XMIN=1.E+10
          ZMIN=1.E+10
          DO 110 K=1,NV
            XMAX=AMAX1(XMAX,XFT(K))
            ZMAX=AMAX1(ZMAX,ZFT(K))
            XMIN=AMIN1(XMIN,XFT(K))
            ZMIN=AMIN1(ZMIN,ZFT(K))
  110     CONTINUE
          GDX=(XMAX-XMIN)/NOX
          GDZ=(ZMAX-ZMIN)/NOZ
          GSSA=IOPCNT*GDX*GDZ

          IOSP=INT(GSSA*100./GSA)
          ISHDT=ISHDT+IOSP
          ISHDH=ISHDH+1
          WRITE(outs,'(I2,A,I7,A)')J,':00',IOSP,'%'
          call edisp(itru,outs)
          IPRINT=2

   20   CONTINUE
        IMEAN=ISHDT/ISHDH
        WRITE(outs,'(A,I3,A)') 'Average for sun-up hours = ',
     &                        IMEAN,'%'
        call edisp(itru,outs)

        dok=.true.
        h(1)='A pause to allow data review.'
        CALL ASKOK(' ','Continue?',OK,dok,1)
        IF(.NOT.OK)goto 23
   10 CONTINUE
   23 RETURN

 1000 WRITE(outs,25)IREC
   25 FORMAT('Transitional shading file error in SYNOP - record',I6)
      call edisp(iuout,outs)
      goto 23
      END

C ********** INSYNP
C Subroutine 'insynp.f' outputs synoptic information on the internal
C surfaces insolated.
      SUBROUTINE insynp(icomp)

#include "building.h"

      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/CONTR/MON,ISC(MS),IYD
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/tmc/itmc(ms),itmcfg,nwins
      common/stins/insst(mgt,24,misur),pinsst(mgt,24,misur),
     &             pinwst(mgt,24,misur),shadst(mgt,24)

      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      character tg*1,delim*1,xfile*144

      character*3 month(12)
      character outs*124,SN*12
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      logical close1,close2

      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     &'Aug','Sep','Oct','Nov','Dec'/

C If output to file alter the edisp unit number.
      itru = iuout
      if(ixopen.eq.1)then
        itru = ixunit
        call edisp(iuout,'Output being directed to file.')
      endif

      iwin=0

      call edisp(itru,' ')
      WRITE(outs,'(3A)')'Internal surface insolation for ',month(mon),
     &                  '.'
      call edisp(itru,outs)

C Display on a surface-by-surface then window-by-window basis.
      DO 10 I=1,NZSUR(icomp)
        ICN=IZSTOCN(icomp,i)
        SN=SSNAME(ICN)
        IF(ISC(I).EQ.0)goto 10
        ng=itmc(i)

C Write header information.
      DO 20 J=1,NG
        kkk=iwin+j
        WRITE(outs,'(A,I2,3A)') 'Source surface is ',i,' (',
     &                           SN(1:lnblnk(SN)),').'
        call edisp(itru,outs)
        call edisp(itru,' Hour  Source   Receiving     Proportion')
        call edisp(itru,'       Shading  Surface       Received')

C Hour-by-hour.
        IPRINT=0
        do 30 k=1,24
          if(insst(kkk,k,1).eq.-1)then
             IF(IPRINT.EQ.2)then
                write(outs,'(I2,A)')k,':00  After sunset'
                call edisp(itru,outs)
C Don't write again.
                IPRINT=-1
             else
                IPRINT=1
             endif
          elseif(insst(kkk,k,1).eq.0.or.shadst(kkk,k).gt.0.999)then
             IF(IPRINT.EQ.1)then
               write(outs,'(I2,A)')k-1,':00  Before sunrise'
               call edisp(itru,outs)
             ENDIF
             write(outs,'(I2,A)')  k,':00   100.0%'
             call edisp(itru,outs)
             IPRINT=2
          else
            IF(IPRINT.EQ.1)then
              write(outs,'(I2,A)')k-1,':00  Before sunrise'
              call edisp(itru,outs)
            ENDIF
            do 40 m=1,misur
              call eclose(pinsst(kkk,k,m),0.0,0.001,close1)
              call eclose(pinwst(kkk,k,m),0.0,0.001,close2)
              if((.NOT.close1).or.(.NOT.close2))then
                if(m.eq.1)then
                  mm=insst(kkk,k,m)
                  ICM=IZSTOCN(icomp,mm)
                  SN=SSNAME(icm)
                  write(outs,1000)k,shadst(kkk,k)*100.0,
     &                mm,SN(1:7),pinsst(kkk,k,m)*100.0
 1000             format(i2,':00',f6.1,'%   ',i2,' (',a,')',f9.1,'%')
                  call edisp(itru,outs)
                else
                  mm=insst(kkk,k,m)
                  ICM=IZSTOCN(icomp,mm)
                  SN=SSNAME(icm)
                  write(outs,1001)mm,SN(1:7),pinsst(kkk,k,m)*100.0
 1001             format(15x,i2,' (',a,')',f9.1,'%')
                  call edisp(itru,outs)
                endif
              endif
              
   40       continue
            IPRINT=2
          endif
        
   30     CONTINUE
   20   CONTINUE
        iwin=iwin+ng
   10 CONTINUE
      RETURN
      END

C *********** SILISTI
C This routine lists the contents of a shading/insolation file
C from the point of view of an insolation analysis.
      SUBROUTINE SILISTI(ICOMP)

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
      common/tmc/itmc(ms),itmcfg,nwins

      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER SN*12
      character*3 month(12)
      character outs*124,H*72,tg*1,delim*1,xfile*144
      logical OK,dok,close1,close2

      dimension ps(24),ishd(12),igc(ms),isadd(12),ntmc(ms)
      dimension insst(mgt,24,misur),pinsst(mgt,24,misur),
     &          pinwst(mgt,24,misur)

      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     &           'Aug','Sep','Oct','Nov','Dec'/

C Assume no default windows.
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
      do 11 i=1,nzsur(icomp)
      if(itmc(i).ne.ntmc(i))goto 1010
   11 continue

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
          WRITE(outs,'(2A)')MONTH(I),' - Shading'
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

C Ask if the output of month details is required.
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
      call edisp(iuout,'No data available for this month!')
      goto 31

C Shading case.
  100 call edisp(itru,' ')
      WRITE(outs,'(3A)')'External surface shading for ',MONTH(MNTH),
     &                  '.'
      call edisp(itru,outs)
      call edisp(itru,'Surface       Shading Factors')
      IF(IREC.LT.5)goto 900
      DO 126 J=1,NZSUR(icomp)
        READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(PS(K),K=1,24)
        IREC=IREC+1

C Check to see if any of the values are > 0 and if so set a flag and
C print.
        LGTZ=0
        DO 226 L=1,24
          IF(PS(L).GT.0.0)LGTZ=1
  226   CONTINUE

        IF(LGTZ.GT.0)then
          icn=IZSTOCN(icomp,J)
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
C      WRITE(outs,'(A,A)')'Shading for ',MONTH(MNTH)
C      call edisp(itru,outs)
C      call edisp(itru,'Glazings   Shading factors')

      READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)
     &  (IGC(J),J=1,NZSUR(icomp))
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
        read(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(pinsst(i,n,j),n=1,24)
        IREC=IREC+1
  203   continue
        do 204 j=1,misur
        read(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(pinwst(i,n,j),n=1,24)
        IREC=IREC+1
  204   continue
  201 continue
  
C Calculate number of insolation sources (transparent surfaces).
      iwin=0
      do 210 i=1,nzsur(icomp)
        icn=IZSTOCN(icomp,i)
        if(igc(i).eq.0)goto 210
        do 211 k=1,itmc(i)
          iwin=iwin+1
          write(outs,'(3A)')'Internal surface insolation for ',
     &                       month(mnth),'.'
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
                    icm=IZSTOCN(icomp,m)
                    SN=SSNAME(icm)
                    write(outs,1003)n,m,SN(1:7),pinsst(iwin,n,j)*100.0
 1003               format(i2,':00  ',i2,' (',a,')',f10.1,'%')
                    call edisp(itru,outs)
                  else
                    m=insst(iwin,n,j)
                    icm=IZSTOCN(icomp,m)
                    SN=SSNAME(icm)
                    write(outs,1004)m,SN(1:7),pinsst(iwin,n,j)*100.0
 1004               format(7x,i2,' (',a,')',f10.1,'%')
                    call edisp(itru,outs)
                  endif
                endif
  213         continue
              IPRINT=2
              if(abs(1.0-sumins).gt.0.1)then
                write(outs,1005)(1.0-sumins)*100.0
 1005           format('Source is partly shaded:',f5.2,'%')
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
 1009 FORMAT('Shading/Insolation file error in SILISTI - record',I6)
      call edisp(iuout,outs)
      goto 2

 1010 call edisp(iuout,'Geometry used in shading db is different to ')
      call edisp(iuout,'that specified for shading predictions.')
      goto 2

 1020 call edisp(iuout,'Empty file!')
      goto 2

      END
