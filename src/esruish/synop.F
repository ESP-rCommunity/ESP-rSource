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
      INCLUDE 'building.h'
      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL
      COMMON/CONTR/MON,ISC(MS),IYD
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/HEADL/IRECS(MS)

      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      DIMENSION XFT(MV),ZFT(MV),IOSHD(MOX,MOZ)
      DIMENSION MTHNAM(12)

      CHARACTER SNAME*12,SOTF*4,SVFC*4,SOTHER*15,SMLCN*12,h*72
      CHARACTER MTHNAM*3,BLKNAME*8,BLKMAT*12,outs*124,SN*12
      character tg*1,delim*1,xfile*144
      logical OK,dok

      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call edisp(icout,' Output being directed to file...')
      endif

      IUNIT=IFIL
      WRITE(outs,'(A,A3)')' Shading information for ',MTHNAM(MON)
      call edisp(itru,outs)

C Retrieve shading information and display on a surface by
C surface basis.
      DO 10 I=1,NSUR
        IF(ISC(I).EQ.0)goto 10
        IREC=IRECS(I)
        ISHDT=0
        ISHDH=0
        NV=NVER(I)

C Retrieve surface transformed coordinates.
        READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(XFT(J),J=1,NV)
        IREC=IREC+1
        READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(ZFT(J),J=1,NV)
        IREC=IREC+1

C Compute gross surface area.
        CALL AREA(NV,XFT,ZFT,GSA)

C Write output header information.
        SN=SNAME(ICOMP,I)
        WRITE(outs,'(3a)') MTHNAM(MON),' hourly shading for ',
     &    SN(1:lnblnk(SN))
        call edisp(itru,outs)

C Hour-by-hour, during sunup hours check to see if and how much shading
C has been calculated for the current surface.
        DO 20 J=1,24
          READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)ISUNUP
          IREC=IREC+1
          write(6,*) 'isunup ',ISUNUP
          IF(ISUNUP.EQ.0)goto 7
          goto 8

C Sun not up.
    7     WRITE(outs,'(I4,A)')J,'  Sun not up'
          if(j.eq.1.or.j.eq.24)call edisp(itru,outs)
          goto 20

C Determine if surface is totally shaded or insolated.
    8     READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)INDS
          IREC=IREC+1
          IF(INDS.EQ.0)goto 11
          IF(INDS.EQ.-1)goto 12

C No shading case.
          WRITE(outs,'(I4,A)')J,'  Surface not shaded'
          call edisp(itru,outs)
          ISHDH=ISHDH+1
          goto 20

C Fully shaded case.
   12     WRITE(outs,'(I4,A)')J,'  Surface fully shaded'
          call edisp(itru,outs)
          ISHDT=ISHDT+100
          ISHDH=ISHDH+1
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

C Assuming no default windows no need to read IGSHD.
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

C No window case - output results.
          IOSP=INT(GSSA*100./GSA)
          ISHDT=ISHDT+IOSP
          ISHDH=ISHDH+1
          WRITE(outs,'(I4,I7)')J,IOSP
          call edisp(itru,outs)
   20   CONTINUE
        IMEAN=ISHDT/ISHDH
        WRITE(outs,'(4a,i3)')MTHNAM(MON),' overall shading for ',
     &    SN(1:lnblnk(SN)),' sun-up hours = ',IMEAN
        call edisp(itru,outs)

        dok=.true.
        h(1)='Pause to review data. '
        CALL ASKOK(' ',' Continue?',OK,dok,1)
        IF(.NOT.OK)goto 23
   10 CONTINUE
   23 RETURN

 1000 WRITE(outs,25)IREC
   25 FORMAT(' Transitional shading file error in SYNOP - record',I6)
      call edisp(iuout,outs)
      goto 23
      END

C ********** INSYNP
C Subroutine 'insynp.f' outputs synoptic information on the internal
C surfaces insolated.
      SUBROUTINE insynp(icomp)

      INCLUDE 'building.h'

      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/CONTR/MON,ISC(MS),IYD
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS), SMLCN(MS), SVFC(MS),SOTHER(MS)
      common/tmc/itmc(ms),itmcfg,nwins
      common/stins/insst(mgt,24,misur),pinsst(mgt,24,misur),
     &             pinwst(mgt,24,misur),shadst(mgt,24)

      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      character tg*1,delim*1,xfile*144

      character*3 month(12)
      character outs*124,SN*12
      CHARACTER SNAME*12,SOTF*4,SVFC*4,SOTHER*15,SMLCN*12
      logical close1,close2

      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     &'Aug','Sep','Oct','Nov','Dec'/

C If output to file alter the edisp unit number.
      itru = iuout
      if(ixopen.eq.1)then
        itru = ixunit
        call edisp(iuout,' Output being directed to file...')
      endif

      iwin=0

      WRITE(outs,'(A,A)')' Insolation information for ',month(mon)
      call edisp(itru,outs)

C Display on a surface by surface then window by window basis.
      DO 10 I=1,NSUR
        SN=SNAME(ICOMP,I)
        IF(ISC(I).EQ.0)goto 10
        ng=itmc(i)

C Write header information.
      DO 20 J=1,NG
        kkk=iwin+j
        write(outs,'(a,i2,2a)')' Source surface (',i,') ',
     &    SN(1:lnblnk(SN))
        call edisp(itru,outs)
        call edisp(itru,' Hour  Shading  Receiving        Percentage')
        call edisp(itru,'                Surface (index)  Received')

C Hour by hour.
        do 30 k=1,24
          if(insst(kkk,k,1).eq.-1)then
             write(outs,'(4X,A)')  '  Sun not up'
             if(k.eq.1.or.k.eq.24)call edisp(itru,outs)
          elseif(insst(kkk,k,1).eq.0.or.shadst(kkk,k).gt.0.999)then
            write(outs,'(i4,a)')k,'  Glazing totally shaded'
            call edisp(itru,outs)
          else
            do 40 m=1,misur
              call eclose(pinsst(kkk,k,m),0.0,0.001,close1)
              call eclose(pinwst(kkk,k,m),0.0,0.001,close2)
              if((.NOT.close1).or.(.NOT.close2))then
                if(m.eq.1)then
                  mm=insst(kkk,k,m)
                  write(outs,1000)k,shadst(kkk,k)*100.0,SNAME(icomp,mm),
     &              mm,pinsst(kkk,k,m)*100.0
 1000             format(i4,f7.2,2x,a,' (',i2,')',f10.2)
                  call edisp(itru,outs)
                else
                  mm=insst(kkk,k,m)
                  write(outs,1001)SNAME(icomp,mm),insst(kkk,k,m),
     &              pinsst(kkk,k,m)*100.0
 1001             format(13x,a,' (',i2,')',f10.2)
                  call edisp(itru,outs)
                endif
              endif
   40       continue
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

      INCLUDE 'building.h'

      COMMON/OUTPCH/ICOUT
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/filep/ifil
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/tmc/itmc(ms),itmcfg,nwins

      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15
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
      if(nsur.ne.nsurs.or.msurs.ne.misur)goto 1010

C Assume no default windows so skip to record 4.
      irec=4
      read(ifilsi,rec=irec,iostat=istat,err=900)(ntmc(i),i=1,nsur)
      do 11 i=1,nsur
      if(itmc(i).ne.ntmc(i))goto 1010
   11 continue

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
      h(1)='If you say yes you will be shown further details '
      h(2)='of shading and insolation patterns. '
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
      call edisp(itru,'   Surface   24-hour shading time-series')
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
          WRITE(outs,124)SNAME(icomp,J),(PS(K),K=1,12)
  124     FORMAT(a,' 1- 12 ',12(F4.2,1X))
          call edisp(itru,outs)
          WRITE(outs,1125)(PS(K),K=13,24)
 1125     FORMAT(12X,' 13-24 ',12(F4.2,1X))
          call edisp(itru,outs)
        endif
  126 CONTINUE

C Window shading.
      WRITE(outs,'(A,A)')' Shading data for ',MONTH(MNTH)
      call edisp(itru,outs)
      call edisp(itru,'   Glazings   24-hour shading time-series  ')
      call edisp(itru,' ')

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
  
      iwin=0
      do 210 i=1,nsur
        if(igc(i).eq.0)goto 210
        do 211 k=1,itmc(i)
          iwin=iwin+1
          write(outs,1000)month(mnth),SNAME(icomp,i),i
 1000     format(' Insolation data for ',a,' : Source surface ',a,'(',
     &      i2,')')
          call edisp(itru,outs)
          call edisp(itru,' Hour  Shading  Receiving       Percentage')
          call edisp(itru,'                Surface (index) Received')
          do 212 n=1,24
            if(insst(iwin,n,1).eq.-1)then
              write(outs,'(4X,A)')  '  Sun not up'
              if(n.eq.1.or.n.eq.24)call edisp(itru,outs)
            elseif(insst(iwin,n,1).eq.0)then
              write(outs,'(I4,A)')n,'  Surface totally shaded'
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
              if(abs(1.0-sumins).gt.0.1)then
                write(outs,1005)(1.0-sumins)*100.0
 1005           format(4x,'  Surface partly shaded (',f5.2,'%)')
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
 1009 FORMAT(' Shading/Insolation file error in SILISTI - record',I6)
      call edisp(iuout,outs)
      goto 2

 1010 call edisp(iuout,' Geometry used in shading db is different to ')
      call edisp(iuout,' that specified for shading predictions.')
      goto 2

 1020 call edisp(iuout,' Empty file!')
      goto 2

      END

