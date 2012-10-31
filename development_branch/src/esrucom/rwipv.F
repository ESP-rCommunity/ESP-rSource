C Subroutines to read and write IPV descriptions.

C *********** mkipvdat
C Write IPV data to an external file or within cfg file based
C on the parameter act (3 char) ='ipv' or ='cfg'
      subroutine mkipvdat(ifu,file,act)
#include "building.h"
#include "ipvdata.h"
      
      integer lnblnk  ! function definition

      character file*72,tab*1
      character outs*124,outsd*124,outsn*124,louts*248
      character dstmp*24
      character act*3   ! indicate which file destination
      integer list
      dimension list(72)  ! zone lists currently limited to 72 max.
                          ! edit if MCOM changes
      integer loutlen,itrunc,ipos  ! used with ailist call

C      tab=CHAR(9)
      tab=','

C Open any existing file by this name or create a new file.
      if(act(1:3).eq.'ipv')then
        CALL  ERPFREE(ifu,ISTAT)
        CALL EFOPSEQ(ifu,file,3,IER)
        IF(IER.LT.0)THEN
          IER=1
          RETURN
        ENDIF
        ipvversion=4
        WRITE(ifu,'(2A,i1,a,i1,3a)',IOSTAT=IOS,ERR=1003)'*IPV',tab,
     &    ipvversion,tab,ipvform,tab,ipvsimu,'  # IPV data follows'
      elseif(act(1:3).eq.'cfg')then
        ipvversion=4
        WRITE(ifu,'(A,i1,a,i1,3a)',IOSTAT=IOS,ERR=1003)'*ipv  ',
     &    ipvversion,tab,ipvform,tab,ipvsimu,'  # IPV data follows'
      endif
      WRITE(ifu,'(3A)',IOSTAT=IOS,ERR=1003)'*title',tab,ipvtitl
      WRITE(ifu,'(3A)',IOSTAT=IOS,ERR=1003)'*version',tab,ipvvers
      call dstamp(dstmp)
      write(ifu,'(3a)',IOSTAT=IOS,ERR=1003)'*date',tab,dstmp

C Note: ipvsynop might be a long line.
      WRITE(ifu,'(3A)',IOSTAT=IOS,ERR=1003)'*synop',tab,
     &  ipvsynop(1:lnblnk(ipvsynop))
      WRITE(outsn,'(A,I2)',IOSTAT=IOS,ERR=1003) '*images',nipvimg
      call SDELIM(outsn,outsd,'C',IW)
      WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003) outsd(1:lnblnk(outsd))
      if(nipvimg.gt.0)then
        do 42 i=1,nipvimg
          WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003)lipvimg(i)
  42    continue
      endif

C Loop through each assessment period.
      WRITE(outsn,'(A,i3)',IOSTAT=IOS,ERR=1003)'*simulations',
     &  nipvassmt
      call SDELIM(outsn,outsd,'C',IW)
      WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003) outsd(1:lnblnk(outsd))
      do 44 ij=1,nipvassmt

C Write comma separated for first two fields and append description.
        WRITE(outs,'(a,2i4)',IOSTAT=IOS,ERR=1003)'*period',
     &    ipvastjd(ij),ipvafnjd(ij)
        call SDELIM(outs,outsn,'C',IW)
        WRITE(ifu,'(3a)',IOSTAT=IOS,ERR=1003) outsn(1:lnblnk(outsn)),
     &    tab,ipvadesc(ij)
 44   continue
      WRITE(outsn,'(A,i3,10I4)',IOSTAT=IOS,ERR=1003)'*display_days',
     &   nipvdispjd,(ipvdispjd(L),L=1,nipvdispjd)
      call SDELIM(outsn,outsd,'C',IW)
      WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003) outsd(1:lnblnk(outsd))

C Write ratios for each climatic season.
      WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003)
     & 'season multipliers: heating|cooling|lights|fans|smallp|DWH'
      WRITE(outsn,'(A,6F8.3)',IOSTAT=IOS,ERR=1003)'*win1',ddmheat(1),
     &  ddmcool(1),ddmlight(1),ddmfan(1),ddmsmlpw(1),ddmdhw(1)
      call SDELIM(outsn,outsd,'C',IW)
      WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003) outsd(1:lnblnk(outsd))

      WRITE(outsn,'(A,6F8.3)',IOSTAT=IOS,ERR=1003)'*spr',ddmheat(2),
     &  ddmcool(2),ddmlight(2),ddmfan(2),ddmsmlpw(2),ddmdhw(2)
      call SDELIM(outsn,outsd,'C',IW)
      WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003) outsd(1:lnblnk(outsd))

      WRITE(outsn,'(A,6F8.3)',IOSTAT=IOS,ERR=1003)'*sum',ddmheat(3),
     &  ddmcool(3),ddmlight(3),ddmfan(3),ddmsmlpw(3),ddmdhw(3)
      call SDELIM(outsn,outsd,'C',IW)
      WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003) outsd(1:lnblnk(outsd))

      WRITE(outsn,'(A,6F8.3)',IOSTAT=IOS,ERR=1003)'*aut',ddmheat(4),
     &  ddmcool(4),ddmlight(4),ddmfan(4),ddmsmlpw(4),ddmdhw(4)
      call SDELIM(outsn,outsd,'C',IW)
      WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003) outsd(1:lnblnk(outsd))

      WRITE(outsn,'(A,6F8.3)',IOSTAT=IOS,ERR=1003)'*win2',ddmheat(5),
     &  ddmcool(5),ddmlight(5),ddmfan(5),ddmsmlpw(5),ddmdhw(5)
      call SDELIM(outsn,outsd,'C',IW)
      WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003) outsd(1:lnblnk(outsd))

C Write number of metrics followed for data for each.
      WRITE(outsn,'(a,i3)',IOSTAT=IOS,ERR=1003)'*metrics',nms
      call SDELIM(outsn,outsd,'C',IW)
      WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003) outsd(1:lnblnk(outsd))
      if(nms.gt.0)then
        WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003)
     &  '# metric data: metric index, nb zones, 2 miscel , floor area'
        do 28 ijj=1,nms
          lnd=lnblnk(msdoc(ijj))
          lnz=lnblnk(metgroup(ijj))
          lng=lnblnk(metrglbl(ijj))
          if(ijj.le.9)then
            WRITE(outsn,'(A,i1,4i4,F8.1,2F7.3,4a)',
     &       IOSTAT=IOS,ERR=1003)'*metric_',ijj,imetget(ijj),
     &       imetmsc(ijj,1),imetmsc(ijj,2),nzmg(ijj),emgflr(ijj),
     &       emgsca(ijj),emgwtg(ijj),' ',msdoc(ijj)(1:lnd),' ',
     &       metgroup(ijj)(1:lnz)
            call SDELIM(outsn,outsd,'C',IW)
            WRITE(ifu,'(4A)',IOSTAT=IOS,ERR=1003)
     &        outsd(1:lnblnk(outsd)),tab,metrglbl(ijj)(1:lng)
          else
            WRITE(outsn,'(A,i2,4i4,F8.1,2F7.3,4a)',
     &       IOSTAT=IOS,ERR=1003)'*metric_',ijj,imetget(ijj),
     &       imetmsc(ijj,1),imetmsc(ijj,2),nzmg(ijj),emgflr(ijj),
     &       emgsca(ijj),emgwtg(ijj),' ',msdoc(ijj)(1:lnd),' ',
     &       metgroup(ijj)(1:lnz)
            call SDELIM(outsn,outsd,'C',IW)
            WRITE(ifu,'(4A)',IOSTAT=IOS,ERR=1003)
     &        outsd(1:lnblnk(outsd)),tab,metrglbl(ijj)(1:lng)
          endif

C Copy izmg to single array list and then write via ailist call.
          if(nzmg(ijj).gt.0)then
            do 829 ijjj=1,nzmg(ijj)
              list(ijjj)=izmg(ijj,ijjj)
 829        continue
            itrunc=1
            ipos=1
            do while (itrunc.ne.0)
              call ailist(ipos,nzmg(ijj),list,72,'C',louts,loutlen,
     &          itrunc)
              write(ifu,'(1x,a)',IOSTAT=ios,ERR=1003) louts(1:loutlen)
              ipos=itrunc+1
            end do
          endif
  28    continue
      endif

C Loop through each demand set and write out associated data.
      if(neds.gt.0)then
        WRITE(ifu,'(2A,i2,a)',IOSTAT=IOS,ERR=1003)'*demand_sets',tab,
     &    neds,'  # number of energy demand sets'
        do 43 ij=1,neds

C Write set of energy (demand/capacity) associated data.
          WRITE(ifu,'(2a)',IOSTAT=IOS,ERR=1003) '# nb. zones,',
     &      ' 2 miscel, aggregate_toggle floor area, scaling descrip'
          if(ij.le.9)then
            WRITE(outsn,'(A,i1,4I4,F8.1,F7.3,2a)',IOSTAT=IOS,ERR=1003)
     &      '*energy_',ij,nzedg(ij),idgmsc(ij,1),idgmsc(ij,2),iaggr,
     &      edgflr(ij),edgsca(ij),' ',zedsdoc(ij)(1:lnblnk(zedsdoc(ij)))
            call SDELIM(outsn,outsd,'C',IW)
            WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003)outsd(1:lnblnk(outsd))
          else
            WRITE(outsn,'(A,i2,4I4,F8.1,F7.3,2a)',IOSTAT=IOS,ERR=1003)
     &      '*energy_',ij,nzedg(ij),idgmsc(ij,1),idgmsc(ij,2),iaggr,
     &      edgflr(ij),edgsca(ij),' ',zedsdoc(ij)(1:lnblnk(zedsdoc(ij)))
            call SDELIM(outsn,outsd,'C',IW)
            WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003)outsd(1:lnblnk(outsd))
          endif

C Copy izedg into list and the write out via ailist call.
          if(nzedg(ij).gt.0)then
            do 830 ijj=1,nzedg(ij)
              list(ijj)=izedg(ij,ijj)
 830        continue
            itrunc=1
            ipos=1
            do while (itrunc.ne.0)
              call ailist(ipos,nzedg(ij),list,72,'C',louts,loutlen,
     &          itrunc)
              write(ifu,'(1x,a)',IOSTAT=ios,ERR=1003) louts(1:loutlen)
              ipos=itrunc+1
            end do
          endif
  43    continue
      endif

C Write nonspecific demands data.
      WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003)
     &'# non-specific demands: occup/light/sm pwr/fans/pumps/lifts/dhw'
      WRITE(outsn,'(A,7I4)',IOSTAT=IOS,ERR=1003)'*dmds',idmdinc(1),
     & idmdinc(2),idmdinc(3),idmdinc(4),idmdinc(5),idmdinc(6),idmdinc(7)
      call SDELIM(outsn,outsd,'C',IW)
      WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003)outsd(1:lnblnk(outsd))

C If writing to external file use a different syntax and free the file.
C If writing to cfg file use *end_ipv and do not close file.
      if(act(1:3).eq.'ipv')then
        WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003)'*end'
        CALL  ERPFREE(ifu,ISTAT)
      elseif(act(1:3).eq.'cfg')then
        WRITE(ifu,'(A)',IOSTAT=IOS,ERR=1003)'*end_ipv'
      endif
      return

  999 CALL  ERPFREE(ifu,ISTAT)
      return

 1003 if(ios.eq.2)then
        CALL USRMSG('No premissopm to write IPV data.',' ','W')
        GOTO 999
      else
        CALL USRMSG('Problem writing IPV data.',' ','W')
        GOTO 999
      endif

      end

C *********** clearipvdat
C Clear IPV common blocks. If act = 'b' blank all the data, if act = 'i'
C then include initial data for energy demand sets and metrics.
      subroutine clearipvdat(act)
#include "building.h"
#include "model.h"
#include "ipvdata.h"
      
      integer lnblnk  ! function definition

C Comments about common blocks are in ripvdat.
      character act*1

C Days to display (default is three (early winter, spring, summer).
      nipvdispjd=3
      ipvdispjd(1)=11
      ipvdispjd(2)=67
      ipvdispjd(3)=186
      do 342 ij=1,MIPVA
        ddmheat(ij)=1.0
        ddmcool(ij)=1.0
        ddmlight(ij)=1.0
        ddmsmlpw(ij)=1.0
        ddmfan(ij)=1.0
        ddmdhw(ij)=1.0
        ipvadesc(ij)=' '
  342 continue

C Images.
      nipvimg=0
      lipvimg(1)='UNKNOWN'
      lipvimg(2)='UNKNOWN'
      lipvimg(3)='UNKNOWN'
      lipvimg(4)='UNKNOWN'

C Items to clear in any case.
      do 50 ij=1,MIPVM
        nzmg(ij)=0
        imetmsc(ij,1)=0
        imetmsc(ij,2)=0
        emgflr(ij)=1.0
        emgsca(ij)=1.0
        emgwtg(ij)=1.0
        izmg(ij,1)=0
        idgmsc(ij,1)=0
        idgmsc(ij,2)=0
        nzedg(ij)=0
        edgflr(ij)=1.0
        edgsca(ij)=1.0
        izedg(ij,1)=0
 50   continue

C Clear commons with blanking.
      if(act.eq.'b')then
        ipvsynop=' '
C        ipvsimu=' '
        nms=0
        do 65 ij=1,MIPVM
          imetget(ij)=0
          msdoc(ij)=' '
          metrglbl(ij)='- '
          metgroup(ij)='- '
  65    continue

C Energy demand sets.
        neds=0
        iaggr=0
        do 67 ij=1,MIPVM
          zedsdoc(ij)='- '
  67    continue
      elseif(act.eq.'i'.or.act.eq.'I')then
        ipvsimu=' '
        lr=lnblnk(cfgroot)
        write(ipvtitl,'(a)') modeltitle(1:40)
        write(ipvvers,'(2a)') 'Version ',cfgroot(1:lr)
        write(ipvsynop,'(2a)') ':The ',cfgroot(1:lr)

C Metric sets.
        nms=1
        do 165 ij=1,MIPVM
          if(ij.eq.1)then
            imetget(ij)=6
            msdoc(ij)='zones'
            metgroup(ij)='ocup_zones'
            metrglbl(ij)='Resultant T (degC)'
          else
            imetget(ij)=0
            msdoc(ij)=' '
            metgroup(ij)='- '
            metrglbl(ij)='- '
          endif
 165    continue

C Energy demand sets.
        neds=1
        do 167 ij=1,MIPVM
          if(ij.eq.1)then
            zedsdoc(ij)='zones'
          else
            zedsdoc(ij)='- '
          endif
 167    continue
      endif

      return
      end

C *********** ripvdat
C Read IPV data from an external file or within cfg file based
C on the parameter act (3 char) ='ipv' or ='cfg'.
      subroutine ripvdat(ifu,file,act,ier)
#include "building.h"
#include "model.h"
#include "espriou.h"
#include "ipvdata.h"
      
      integer lnblnk  ! function definition

      character file*72,word*20
      character OUTSTR*124,LOUTSTR*248
      character dstmp*24
      character act*3   ! indicate which file destination
      integer list
      dimension list(72)  ! zone lists currently limited to 72 max.
                          ! edit if MCOM changes
      character tipvadesc*40

C Comments about common blocks are in ripvdat.

C Open any existing file by this name.
      if(act(1:3).eq.'ipv')then
        CALL EFOPSEQ(ifu,file,1,IER)
        IF(IER.LT.0)THEN
          IER=1
          RETURN
        ENDIF
        write(currentfile,'(a)') file(1:lnblnk(file))

C Check identity and if that is successful then get the version
C number and the format indicator.
        CALL STRIPC(ifu,OUTSTR,99,ND,1,'line 1',IER)
        IF(IER.NE.0)RETURN
        if(OUTSTR(1:5).ne.'* IPV'.and.OUTSTR(1:4).ne.'*IPV')then
          call usrmsg('Is not an IPV description file.',' ','W')
          IER=1
          return
        endif
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','*IPV tag',IFLAG)
        if(ND.gt.2)then
          CALL EGETWI(OUTSTR,K,ipvversion,0,4,'W','version index',IER)
          CALL EGETWI(OUTSTR,K,ipvform,0,4,'W','ipv rep format',IER)
          if(ND.gt.3)then
            CALL EGETW(OUTSTR,K,ipvsimu,'W','*IPV assessment',IFLAG)
          endif
        else
          ipvversion=3
          ipvform=0
          ipvsimu='------'
        endif
      elseif(act(1:3).eq.'cfg')then
        continue  ! the header information has already been scanned.
      endif

C Clear (blank) IPV commons before reading.
      call clearipvdat('b')
      iper=0
      imset=0
      iset=0

C Scan lines.
  43  CALL STRIPC(ifu,OUTSTR,99,ND,1,'IPV data',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','tag',IFLAG)
      if(WORD(1:6).eq.'*title')then
        CALL EGETRM(OUTSTR,K,ipvtitl,'W','title string',IER)
      elseif(WORD(1:8).eq.'*version')then
        CALL EGETRM(OUTSTR,K,ipvvers,'W','version string',IER)
      elseif(WORD(1:5).eq.'*date')then

C If *date found get the rest of the line and then check to
C see if the next (long) line starts with *synop or *syn1
        CALL EGETRM(OUTSTR,K,dstmp,'W','date string',IER)
        CALL LSTRIPC(ifu,LOUTSTR,99,ND,1,'synopsis',IER)
        K=0
        CALL EGETW(LOUTSTR,K,WORD,'W','tag',IFLAG)
        if(WORD(1:5).eq.'*syn1')then
          write(ipvsynop,'(a)') LOUTSTR(7:lnblnk(LOUTSTR))

C Debug.
C          write(6,*) ipvsynop

          goto 43
        elseif(WORD(1:6).eq.'*synop')then
          write(ipvsynop,'(a)') LOUTSTR(8:lnblnk(LOUTSTR))
          goto 43
        endif
      elseif(WORD(1:5).eq.'*syn2')then

C Do not bother to read remainder of earlier multi-line synopsis.
        continue
      elseif(WORD(1:5).eq.'*syn3')then
        continue
      elseif(WORD(1:5).eq.'*syn4')then
        continue
      elseif(WORD(1:5).eq.'*syn5')then
        continue
      elseif(WORD(1:7).eq.'*images')then
        CALL EGETWI(OUTSTR,K,nipvimg,0,4,'W','no of images',IER)
        if(nipvimg.gt.0)then
          do 42 i=1,nipvimg
            CALL STRIPC(ifu,OUTSTR,0,ND,1,'image file',IER)
            lipvimg(i)=OUTSTR(1:72)
  42      continue
        endif
      elseif(WORD(1:12).eq.'*simulations')then
        CALL EGETWI(OUTSTR,K,nipvassmt,1,5,'W','simulations',IER)
        iper=0
        if(ipvsimu.eq.'------')then

C We have an older file which did not contain ipvsimu so make a guess
C at its value by looking at the number of assessments.
          if(nipvassmt.eq.1)then
            ipvsimu='ias   '
          elseif(nipvassmt.eq.3)then
            ipvsimu='i3t   '
          elseif(nipvassmt.eq.5)then
            ipvsimu='i5t   '
          else
            ipvsimu='i--   '
          endif
        endif
      elseif(WORD(1:7).eq.'*period')then

C If the initial portion of ipvadesc is the same as the model root
C name then discard the initial portion.
        iper=iper+1
        CALL EGETWI(OUTSTR,K,ipvastjd(iper),1,365,'W','assmt st',IER)
        CALL EGETWI(OUTSTR,K,ipvafnjd(iper),1,365,'W','assmt fn',IER)
        tipvadesc=' '
        CALL EGETRM(OUTSTR,K,tipvadesc,'W','assmt descr',IER)
        lncf=lnblnk(cfgroot)
        lnipva=lnblnk(tipvadesc)
        if(tipvadesc(1:lncf).eq.cfgroot(1:lncf))then
          write(ipvadesc(iper),'(a)') tipvadesc(lncf+1:lnipva)
        else
          write(ipvadesc(iper),'(a)') tipvadesc(1:lnipva)
        endif

      elseif(WORD(1:13).eq.'*display_days')then
        CALL EGETWI(OUTSTR,K,nipvdispjd,1,10,'W','displays',IER)
        do 343 ij=1,nipvdispjd
          CALL EGETWI(OUTSTR,K,ipvdispjd(ij),1,365,'W','display',IER)
  343   continue

      elseif(WORD(1:5).eq.'*win1')then
        CALL EGETWR(OUTSTR,K,ddmheat(1),0.,100.,'W','win h dd',IER)
        CALL EGETWR(OUTSTR,K,ddmcool(1),0.,100.,'W','win c dd',IER)
        CALL EGETWR(OUTSTR,K,ddmlight(1),0.,100.,'W','win lt dd',IER)
        CALL EGETWR(OUTSTR,K,ddmfan(1),0.,100.,'W','win fn dd',IER)
        CALL EGETWR(OUTSTR,K,ddmsmlpw(1),0.,100.,'W','win sp dd',IER)
        CALL EGETWR(OUTSTR,K,ddmdhw(1),0.,100.,'W','win hw dd',IER)
      elseif(WORD(1:4).eq.'*trn'.or.WORD(1:4).eq.'*spr')then
        CALL EGETWR(OUTSTR,K,ddmheat(2),0.,100.,'W','spr h dd',IER)
        CALL EGETWR(OUTSTR,K,ddmcool(2),0.,100.,'W','spr c dd',IER)
        CALL EGETWR(OUTSTR,K,ddmlight(2),0.,100.,'W','spr lt dd',IER)
        CALL EGETWR(OUTSTR,K,ddmfan(2),0.,100.,'W','spr fn dd',IER)
        CALL EGETWR(OUTSTR,K,ddmsmlpw(2),0.,100.,'W','spr sp dd',IER)
        CALL EGETWR(OUTSTR,K,ddmdhw(2),0.,100.,'W','spr hw dd',IER)
      elseif(WORD(1:4).eq.'*sum')then
        CALL EGETWR(OUTSTR,K,ddmheat(3),0.,100.,'W','sum h dd',IER)
        CALL EGETWR(OUTSTR,K,ddmcool(3),0.,100.,'W','sum c dd',IER)
        CALL EGETWR(OUTSTR,K,ddmlight(3),0.,100.,'W','sum lt dd',IER)
        CALL EGETWR(OUTSTR,K,ddmfan(3),0.,100.,'W','sum fn dd',IER)
        CALL EGETWR(OUTSTR,K,ddmsmlpw(3),0.,100.,'W','sum sp dd',IER)
        CALL EGETWR(OUTSTR,K,ddmdhw(3),0.,100.,'W','sum hw dd',IER)
      elseif(WORD(1:4).eq.'*aut')then
        CALL EGETWR(OUTSTR,K,ddmheat(4),0.,100.,'W','aut h dd',IER)
        CALL EGETWR(OUTSTR,K,ddmcool(4),0.,100.,'W','aut c dd',IER)
        CALL EGETWR(OUTSTR,K,ddmlight(4),0.,100.,'W','aut lt dd',IER)
        CALL EGETWR(OUTSTR,K,ddmfan(4),0.,100.,'W','aut fn dd',IER)
        CALL EGETWR(OUTSTR,K,ddmsmlpw(4),0.,100.,'W','aut sp dd',IER)
        CALL EGETWR(OUTSTR,K,ddmdhw(4),0.,100.,'W','aut hw dd',IER)
      elseif(WORD(1:5).eq.'*win2')then
        CALL EGETWR(OUTSTR,K,ddmheat(5),0.,100.,'W','win2 h dd',IER)
        CALL EGETWR(OUTSTR,K,ddmcool(5),0.,100.,'W','win2 c dd',IER)
        CALL EGETWR(OUTSTR,K,ddmlight(5),0.,100.,'W','win2 lt dd',IER)
        CALL EGETWR(OUTSTR,K,ddmfan(5),0.,100.,'W','win2 fn dd',IER)
        CALL EGETWR(OUTSTR,K,ddmsmlpw(5),0.,100.,'W','win2 sp dd',IER)
        CALL EGETWR(OUTSTR,K,ddmdhw(5),0.,100.,'W','win2 hw dd',IER)

C One of the sets of performance metrics and associated data.
C First read out how many metrics, then for each metric write
C out its zones, floor area, weighting and set description.

      elseif(WORD(1:8).eq.'*metrics')then
        CALL EGETWI(OUTSTR,K,nms,0,12,'W','nb of metric sets',IER)
        imset=0
      elseif(WORD(1:8).eq.'*metric_')then
        imset=imset+1
        CALL EGETWI(OUTSTR,K,imetget(imset),0,99,'W','metric iget',IER)
        CALL EGETWI(OUTSTR,K,imetmsc(imset,1),0,32,'W','metric msc 1',
     &    IER)
        CALL EGETWI(OUTSTR,K,imetmsc(imset,2),0,32,'W','metric msc 2',
     &    IER)
        CALL EGETWI(OUTSTR,K,nzmg(imset),0,MCOM,'W','nb of zones',IER)
        CALL EGETWR(OUTSTR,K,emgflr(imset),1.,9999.,'W','metric flr',
     &    IER)
        CALL EGETWR(OUTSTR,K,emgsca(imset),1.,9999.,'W','metric scale',
     &    IER)
        CALL EGETWR(OUTSTR,K,emgwtg(imset),0.,99.,'W','metric wtg',IER)

C Read the user defined set name and the short metric label. If version
C 4 also read in the group name for the zones associated with the metric.
        CALL EGETP(OUTSTR,K,msdoc(imset),'W','metric set descr',IER)
        if(ipvversion.eq.4)then
          CALL EGETP(OUTSTR,K,metgroup(imset),'W','metric group',IER)
        endif
        CALL EGETP(OUTSTR,K,metrglbl(imset),'W','metric 20 descr',IER)

C Read in the associated zones (on multiple lines is needed). Be sure
C to update warning range if model complexity changes in building.h
        if(nzmg(imset).gt.0)then
          CALL EGETWIA(ifu,list,nzmg(imset),1,82,'W',
     &      'IPV metrics associated zones',IER)
          DO 118 KV=1,nzmg(imset)
            izmg(imset,KV)=list(KV)
  118     CONTINUE
        endif

C One of the sets of energy (demand/capacity) associated data.
      elseif(WORD(1:12).eq.'*demand_sets')then
        CALL EGETWI(OUTSTR,K,neds,0,MIPVM,'W','no of demand sets',IER)
        iset=0
      elseif(WORD(1:8).eq.'*energy_')then
        iset=iset+1
        CALL EGETWI(OUTSTR,K,nzedg(iset),0,MCOM,'W','nb of zones',IER)
        CALL EGETWI(OUTSTR,K,idgmsc(iset,1),0,32,'W','demand msc',IER)
        CALL EGETWI(OUTSTR,K,idgmsc(iset,2),0,32,'W','demand msc',IER)
        CALL EGETWI(OUTSTR,K,iaggr,0,1,'W','aggregate toggle',IER)
        CALL EGETWR(OUTSTR,K,edgflr(iset),1.,9999.,'W','enrg flr',IER)
        CALL EGETWR(OUTSTR,K,edgsca(iset),0.,99.,'W','enrg wtg',IER)
        CALL EGETRM(OUTSTR,K,zedsdoc(iset),'W','enrg descr',IER)

C Fill izedg array via call to egetwia returning list.
        if(nzedg(iset).gt.0)then
          CALL EGETWIA(ifu,list,nzedg(iset),1,MCOM,'W',
     &      'IPV dmds associated zones',IER)
          DO 119 KV=1,nzedg(iset)
            izedg(iset,KV)=list(KV)
  119     CONTINUE
        endif

C Non-specific demands toggles for inclusion.
      elseif(WORD(1:5).eq.'*dmds')then
        CALL EGETWI(OUTSTR,K,idmdinc(1),0,1,'W','occup dmd',IER)
        CALL EGETWI(OUTSTR,K,idmdinc(2),0,1,'W','light dmd',IER)
        CALL EGETWI(OUTSTR,K,idmdinc(3),0,1,'W','smpwr dmd',IER)
        CALL EGETWI(OUTSTR,K,idmdinc(4),0,1,'W','fans dmd',IER)
        CALL EGETWI(OUTSTR,K,idmdinc(5),0,1,'W','pumps dmd',IER)
        CALL EGETWI(OUTSTR,K,idmdinc(6),0,1,'W','lifts dmd',IER)
        CALL EGETWI(OUTSTR,K,idmdinc(7),0,1,'W','DHW dmd',IER)
      elseif(WORD(1:8).eq.'*end_ipv')then
        return  ! for ipv data within cfg file
      elseif(WORD(1:5).eq.'*end ')then
        goto 999  ! for separate ipv file
      else
        goto 43
      endif
      goto 43

  999 CALL  ERPFREE(ifu,ISTAT)
      return

      end


C *********** listipvdat
C List current IPV data common blocks.
C Where act='a' list all, act='m' list metrics, act='d' list
C demand sets. Itru is the channel for listing.
      subroutine listipvdat(itru,act,ier)
#include "building.h"
#include "seasons.h"
C seasons.h provides typper and typsea
#include "ipvdata.h"
      
      integer lnblnk  ! function definition

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

C See ipvdata.h and ripvdat for IPV common block information.
      integer list
      dimension list(72)  ! zone lists currently limited to 72 max.
                          ! edit if MCOM changes

      character outs*124
      character act*1
      character descr*7,descrst*10,descrfn*10
      character zdescr*248   ! sized to hold 72 zone names
      character descra*7,descrb*7,descrc*7,descrd*7,descre*7

      if(act.eq.'a')then
        write(outs,'(2A)')     'Title : ',ipvtitl
        call edisp(itru,outs)
        write(outs,'(2A)')     'Version   : ',ipvvers
        call edisp(itru,outs)
        call edisp(itru,'synopsis:')
        call edisp248(itru,ipvsynop,80)
        write(outs,'(A,I3)')   'Images    : ',nipvimg
        call edisp(itru,outs)
        if(nipvimg.gt.0)then
          do 42 i=1,nipvimg
            call edisp(itru,lipvimg(i))
  42      continue
        endif
        call edisp(itru,'  ')
        call edisp(itru,'Simulations to be undertaken:')
        write(outs,'(a,i2,a)') '____simulations (',nipvassmt,
     &  ')____________days descrip___'
        call edisp(itru,outs)
        if(nipvassmt.eq.0)then
          call edisp(itru,'Simulations not yet defined.')
        elseif(nipvassmt.eq.1)then
          call stdate(iyear,ipvastjd(1),descr,descrst)
          call stdate(iyear,ipvafnjd(1),descr,descrfn)
          jjd1=(ipvafnjd(1)-ipvastjd(1))+1
          write(outs,'(4a,i4,2a)') 'annual ',descrst,' to ',descrfn,
     &      jjd1,' ',ipvadesc(1)
          call edisp(itru,outs)
        elseif(nipvassmt.eq.3)then
          call stdate(iyear,ipvastjd(1),descr,descrst)
          call stdate(iyear,ipvafnjd(1),descr,descrfn)
          jjd1=(ipvafnjd(1)-ipvastjd(1))+1
          write(outs,'(4a,i4,2a)') 'winter ',descrst,' to ',descrfn,
     &      jjd1,' ',ipvadesc(1)
          call edisp(itru,outs)
          call stdate(iyear,ipvastjd(2),descr,descrst)
          call stdate(iyear,ipvafnjd(2),descr,descrfn)
          jjd2=(ipvafnjd(2)-ipvastjd(2))+1
          write(outs,'(4a,i4,2a)') 'trans  ',descrst,' to ',descrfn,
     &      jjd2,' ',ipvadesc(2)
          call edisp(itru,outs)
          call stdate(iyear,ipvastjd(3),descr,descrst)
          call stdate(iyear,ipvafnjd(3),descr,descrfn)
          jjd3=(ipvafnjd(3)-ipvastjd(3))+1
          write(outs,'(4a,i4,2a)')  'summer ',descrst,' to ',descrfn,
     &      jjd3,' ',ipvadesc(3)
          call edisp(itru,outs)
        elseif(nipvassmt.eq.5)then
          call stdate(iyear,ipvastjd(1),descr,descrst)
          call stdate(iyear,ipvafnjd(1),descr,descrfn)
          jjd1=(ipvafnjd(1)-ipvastjd(1))+1
          write(outs,'(4a,i4,2a)')  '1st win ',descrst,' to ',descrfn,
     &      jjd1,' ',ipvadesc(1)
          call edisp(itru,outs)
          call stdate(iyear,ipvastjd(2),descr,descrst)
          call stdate(iyear,ipvafnjd(2),descr,descrfn)
          jjd2=(ipvafnjd(2)-ipvastjd(2))+1
          write(outs,'(4a,i4,2a)')  'spring  ',descrst,' to ',descrfn,
     &      jjd2,' ',ipvadesc(2)
          call edisp(itru,outs)
          call stdate(iyear,ipvastjd(3),descr,descrst)
          call stdate(iyear,ipvafnjd(3),descr,descrfn)
          jjd3=(ipvafnjd(3)-ipvastjd(3))+1
          write(outs,'(4a,i4,2a)')  'summer  ',descrst,' to ',descrfn,
     &      jjd3,' ',ipvadesc(3)
          call edisp(itru,outs)
          call stdate(iyear,ipvastjd(4),descr,descrst)
          call stdate(iyear,ipvafnjd(4),descr,descrfn)
          jjd4=(ipvafnjd(4)-ipvastjd(4))+1
          write(outs,'(4a,i4,2a)')  'autumn  ',descrst,' to ',descrfn,
     &      jjd4,' ',ipvadesc(4)
          call edisp(itru,outs)
          call stdate(iyear,ipvastjd(5),descr,descrst)
          call stdate(iyear,ipvafnjd(5),descr,descrfn)
          jjd5=(ipvafnjd(5)-ipvastjd(5))+1
          write(outs,'(4a,i4,2a)')  '2nd win ',descrst,' to ',descrfn,
     &      jjd5,' ',ipvadesc(5)
          call edisp(itru,outs)
        endif

C Display days.
        if(nipvdispjd.gt.0)then
          call edisp(itru,'  ')
          do 155 ix=1,nipvdispjd
            call stdate(iyear,ipvdispjd(ix),descr,descrst)
            write(outs,'(3a)')  'display day ',descrst,'.'
            call edisp(itru,outs)
  155     continue
        endif

        call edisp(itru,'  ')
        call edisp(itru,'Seasons (as found in climatelist file):')
        call edisp(itru,
     &    '         winter  spring  summer  autumn  winter')
        call stdate(iyear,is1wins,descra,descrst)
        call stdate(iyear,is1sprs,descrb,descrst)
        call stdate(iyear,is1sums,descrc,descrst)
        call stdate(iyear,is2sprs,descrd,descrst)
        call stdate(iyear,is2wins,descre,descrst)
        write(outs,'(10a)') '  start  ',descra,' ',descrb,' ',descrc,
     &    ' ',descrd,' ',descre
        call edisp(itru,outs)
        call stdate(iyear,is1winf,descra,descrfn)
        call stdate(iyear,is1sprf,descrb,descrfn)
        call stdate(iyear,is1sumf,descrc,descrfn)
        call stdate(iyear,is2sprf,descrd,descrfn)
        call stdate(iyear,is2winf,descre,descrfn)
        write(outs,'(10a)') '  finish ',descra,' ',descrb,' ',descrc,
     &    ' ',descrd,' ',descre
        call edisp(itru,outs)
        jd1=(is1winf-is1wins)+1
        jd2=(is1sprf-is1sprs)+1
        jd3=(is1sumf-is1sums)+1
        jd4=(is2sprf-is2sprs)+1
        jd5=(is2winf-is2wins)+1
        write(outs,'(a,i4,1x,4i8)')'  days   ',jd1,jd2,jd3,jd4,jd5
        call edisp(itru,outs)

        call edisp(itru,'  ')
        if(nipvassmt.eq.1)then
          call edisp(itru,'Ratios between assessments and all seasons:')
          WRITE(outs,'(a,f7.2)')  ' heating    ',ddmheat(1)
          call edisp(itru,outs)
          WRITE(outs,'(a,f7.2)')  ' cooling    ',ddmcool(1)
          call edisp(itru,outs)
          WRITE(outs,'(a,f7.2)')  ' lighting   ',ddmlight(1)
          call edisp(itru,outs)
          WRITE(outs,'(a,f7.2)')  ' small power',ddmsmlpw(1)
          call edisp(itru,outs)
          WRITE(outs,'(a,f7.2)')  ' fans&pumps ',ddmfan(1)
          call edisp(itru,outs)
          WRITE(outs,'(a,f7.2)')  ' DHW        ',ddmdhw(1)
          call edisp(itru,outs)
        elseif(nipvassmt.eq.3)then
          call edisp(itru,
     &      'Ratios between assessments and winter transition summer:')
          WRITE(outs,'(a,3f7.2)') ' heating    ',ddmheat(1),
     &      ddmheat(2),ddmheat(3)
          call edisp(itru,outs)
          WRITE(outs,'(a,3f7.2)') ' cooling    ',ddmcool(1),
     &      ddmcool(2),ddmcool(3)
          call edisp(itru,outs)
          WRITE(outs,'(a,3f7.2)') ' lighting   ',ddmlight(1),
     &      ddmlight(2),ddmlight(3)
          call edisp(itru,outs)
          WRITE(outs,'(a,3f7.2)') ' small power',ddmsmlpw(1),
     &      ddmsmlpw(2),ddmsmlpw(3)
          call edisp(itru,outs)
          WRITE(outs,'(a,3f7.2)') ' fans&pumps ',ddmfan(1),
     &      ddmfan(2),ddmfan(3)
          call edisp(itru,outs)
          WRITE(outs,'(a,3f7.2)') ' DHW        ',ddmdhw(1),
     &      ddmdhw(2),ddmdhw(3)
        elseif(nipvassmt.eq.5)then
          call edisp(itru,
     &      ' Ratios for:  winter spring summer autumn winter')
          WRITE(outs,'(a,5f7.2)') ' heating    ',ddmheat(1),
     &      ddmheat(2),ddmheat(3),ddmheat(4),ddmheat(5)
          call edisp(itru,outs)
          WRITE(outs,'(a,5f7.2)') ' cooling    ',ddmcool(1),
     &      ddmcool(2),ddmcool(3),ddmcool(4),ddmcool(5)
          call edisp(itru,outs)
          WRITE(outs,'(a,5f7.2)') ' lighting   ',ddmlight(1),
     &      ddmlight(2),ddmlight(3),ddmlight(4),ddmlight(5)
          call edisp(itru,outs)
          WRITE(outs,'(a,5f7.2)') ' small power',ddmsmlpw(1),
     &      ddmsmlpw(2),ddmsmlpw(3),ddmsmlpw(4),ddmsmlpw(5)
          call edisp(itru,outs)
          WRITE(outs,'(a,5f7.2)') ' fans&pumps ',ddmfan(1),
     &      ddmfan(2),ddmfan(3),ddmfan(4),ddmfan(5)
          call edisp(itru,outs)
          WRITE(outs,'(a,5f7.2)') ' DHW        ',ddmdhw(1),
     &      ddmdhw(2),ddmdhw(3),ddmdhw(4),ddmdhw(5)
          call edisp(itru,outs)
        endif
      endif
      if(act.eq.'a'.or.act.eq.'m')then

C List current metrics and demand sets.
        if(nms.ge.1)then
          call edisp(itru,' ')
          call edisp(itru,
     &    'Metric                zones area scaling weight  type name')
          do 269 ij=1,nms
            lnd=lnblnk(msdoc(ij))
            lnz=lnblnk(metgroup(ij))
            lng=lnblnk(metrglbl(ij))
            write(outs,'(i2,3a,i4,f8.2,2f6.3,4a)') ij,' ',
     &        metrglbl(ij)(1:lng),' ',nzmg(ij),emgflr(ij),emgsca(ij),
     &        emgwtg(ij),' ',msdoc(ij)(1:lnd),' ',metgroup(ij)(1:lnz)
     &        
            call edisp(itru,outs)
            inlist=nzmg(ij)
            do 829 ijj=1,nzmg(ij)
              list(ijj)=izmg(ij,ijj)
 829        continue
            call znarlist(inlist,list,72,zdescr,length,ier)
            call edisp248(itru,zdescr,100)
 269      continue
        else
          call edisp(itru,'No metrics have been defined.')
        endif
      endif
      if(act.eq.'a'.or.act.eq.'d')then

C List current demand sets..
        if(neds.ge.1)then
          call edisp(itru,' ')
          call edisp(itru,
     &    'Demand set,  nb zones, floor area, scaling factor')
          do 268 ij=1,neds
            write(outs,'(i2,2a,i4,f8.2,f6.3)') ij,' ',zedsdoc(ij),
     &        nzedg(ij),edgflr(ij),edgsca(ij)
            call edisp(itru,outs)
            inlist=nzedg(ij)
            do 828 ijj=1,nzedg(ij)
              list(ijj)=izedg(ij,ijj)
 828        continue
            call znarlist(inlist,list,72,zdescr,length,ier)
            call edisp248(itru,zdescr,100)
 268      continue
        else
          call edisp(itru,'No demand sets have been defined.')
        endif
        call edisp(itru,' ')
        if(idmdinc(1).eq.1)then
          call edisp(itru,'Dispersed occupant demands included.')
        endif
        if(idmdinc(2).eq.1)then
          call edisp(itru,'Dispersed lighting demands included.')
        endif
        if(idmdinc(3).eq.1)then
          call edisp(itru,'Dispersed small power demands included.')
        endif
        if(idmdinc(4).eq.1)then
          call edisp(itru,'Dispersed fan demands included.')
        endif
        if(idmdinc(5).eq.1)then
          call edisp(itru,'Dispersed pump demands included.')
        endif
        if(idmdinc(6).eq.1)then
          call edisp(itru,'Dispersed lift demands included.')
        endif
        if(idmdinc(7).eq.1)then
          call edisp(itru,'Dispersed DHW demands included.')
        endif
      endif
      return
      end
