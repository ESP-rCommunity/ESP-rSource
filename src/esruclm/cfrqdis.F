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

C HISCLM draws a histogram of the frequencies of occurance of values
C of the user-specified time-dependant variable.
      SUBROUTINE HISCLM(IP)

      PARAMETER (MF=100)

C      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/appw/iappw,iappx,iappy
      common/exporttgi/ixopen,ixloc,ixunit
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      COMMON/IFDFLG/IFLAGF

      COMMON/FREQS/NBINS,XBIN1,XBINN,DXBIN,IFREQ,IHITS,IMISS,INBIN(MF)
      COMMON/FREQS1/ICBIN(MF),ITOTAL

      dimension IVALS(3),FDC(3)
      character FDC*20,outs*124
      logical graph_ok,ok,dok

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero

      helpinsub='HISCLM'  ! set for cfiles

C Identify specific variable to examine
C and set up relevant label type for axis.
      graph_ok = .false.
      IGET=IP

C Call MODFRQ to set up bin sizes and fill bins with data.
      CALL HISFRQ (IGET)
      IF(IFLAGF.EQ.1)GOTO 9999
   75 helptopic='clm_frq_distr_choice'
      call gethelptext(helpinsub,helptopic,nbhelp)

      INPIC=1
      FDC(1)='frequency bins      '
      FDC(2)='cumulative bins     '
      FDC(3)='summary table       '
      CALL EPICKS(INPIC,IVALS,' ',' Presentation format: ',
     &  20,3,FDC,' frequency displays',IER,nbhelp)
      IF(INPIC.eq.0)goto 9999
      IG=IVALS(1)
      call usrmsg(' ',' ','-')

      IF(IG.EQ.3)then

C Set tabular output.
        if(graph_ok)then
          dok=.true.
          helptopic='clm_numb_below_graph'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call askok(' ','Add table below existing graph?',ok,dok,
     &      nbhelp)
          if(.NOT.ok)CALL CHEAD(30)
        else
          CALL CHEAD(30)
        endif

C If output to file alter the edisp unit number.
        itru = icout
        if(ixopen.eq.1)then
          itru = ixunit
          call usrmsg(' Output being directed to file...',' ','-')
        endif
        call edisp(itru,' ')
        call edisp(itru,' Frequency analysis - ')
        call edisp(itru,' ')
        call edisp(itru,
     &' Bin        range        No. of   freq.   cumul. no  cumulative')
        call edisp(itru,
     &'                          hits     (%)     of hits    freq (%)')
        call edisp(itru,' ')
        XBIN=XBIN1
        DO 10 I=1,NBINS
          XBINS=XBIN
          XBIN=XBIN+DXBIN
          CFREQ=(FLOAT(ICBIN(I))/FLOAT(ITOTAL))*100.0
          FREQ=(FLOAT(INBIN(I))/FLOAT(ITOTAL))*100.0
          WRITE(outs,78)I,XBINS,XBIN,INBIN(I),FREQ,ICBIN(I),CFREQ
   78     FORMAT(I4,1X,F8.2,' -',F8.2,I6,4X,F6.2,I9,F12.2)
          call eddisp(itru,outs)
   10   CONTINUE

C Invoke drawing routines.
      ELSEIF(IG.EQ.1.OR.IG.EQ.2)then

C In case the user has resized the display, re-establish the graphic
C bounds and redraw the image before presenting the command menu.

        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          if(iappw.gt.0.and.iappw.lt.100)then
            LIMTTY= MAX0(int(12*iappw*0.01),8)
            LIMIT= MAX0(int(12*iappw*0.01),8)
          else
            LIMTTY=12
            LIMIT=12
          endif
        elseif(iglib.eq.2)then

C GTK note: the call to win3d is a new requirement (until such time
C as the C automatically refreshes the coordinates).

C Setup and pass in parameters to win3d.
          iiw1=9; iiw2=8; iiw3=5; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
          call startbuffer()
        endif

        CALL CHEAD(11)
        iside=3
        isize=0
        ifont=1
        call viewtext(' Frequency analysis',iside,isize,ifont)

C Set axis label flag IL.
        IF(IGET.EQ.1)IL=2
        IF(IGET.EQ.2)IL=1
        IF(IGET.EQ.3)IL=2
        IF(IGET.EQ.4)IL=3
        IF(IGET.EQ.5)IL=4
        IF(IGET.EQ.6)IL=5

C Draw histogram.
        CALL HISDRW(IL,IG)
        graph_ok = .true.
      endif
      goto 75

 9999 RETURN
      END

C ---------- HISFRQ
C 'HISFRQ' divides the range of the selected variable into
C several bins and counts the frequency with which the variable
C falls inside each bin.

      SUBROUTINE HISFRQ (IGET)
      PARAMETER (MF=100)
      PARAMETER (MT=24)

      COMMON/FREQS/NBINS,XBIN1,XBINN,DXBIN,IFREQ,IHITS,IMISS,INBIN(MF)
      COMMON/FREQS1/ICBIN(MF),ITOTAL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/DAYSF/KDS,KDF

      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &CDIR(MT),CHUM(MT)

      COMMON/IFDFLG/IFLAGF

      character CHA*20
      logical ok,dok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero

      helpinsub='HISFRQ'  ! set for cfiles

C Reset the number of bins, the maximum hits in one bin etc.
      DXBIN=0.0
      NBINS=0
      IFREQ=0
      helptopic='clm_frq_distr_choice'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call easki(NBINS,' ',' Number of bins (0 for fixed width)?',
     &  0,'F',MF,'F',1,'no of bins',IER,nbhelp)
      IF (NBINS .GT. 0 .AND. NBINS .LE. MF) GOTO 110


      DXBIN=1.0
      CHA='  1   '
      helptopic='clm_frq_distr_bins'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKS(CHA,' ',' Bin width [d] ? ',20,
     &    '1','bin width',IER,5)
      IF(CHA(1:1).EQ.'d'.OR.CHA(1:1).EQ.'D')then
        dok=.true.
        call askok('  ','Default bins?',OK,dok,5)
        if(ok)then
          goto 300
        else
          goto 200
        endif
      endif
      K=0
      CALL EGETWR(CHA,K,DXBIN,0.,0.,'-','bin width',IER)

 110  helptopic='clm_frq_distr_bins'
      call gethelptext(helpinsub,helptopic,nbhelp)
      dok=.true.
      call  askok('  ','Default bins?',OK,dok,5)
      if(ok)then
        goto 300
      else
        goto 200
      endif

C Get bin sizes from user.
  200 IF(NBINS.EQ.0)goto 210
      helptopic='clm_frq_min_bin'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call easkr(XBIN1,' ','Minimum parameter value for bin ?',
     &  0.,'-',0.,'-',0.,'min bin parameter',IER,nbhelp)

      helptopic='clm_frq_max_bin'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call easkr(XBINN,' ','Maximum parameter value for bin ?',
     &  XBIN1,'F',0.,'-',0.,'max bin parameter',IER,nbhelp)

      DXBIN = (XBINN-XBIN1) / NBINS
  210 dok=.false.
      helptopic='clm_frq_max_hit'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call askok('  ','Fix maximum height of bins?',OK,dok,nbhelp)
      if(ok)then
        helptopic='clm_frq_max_hight'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easki(IFREQ,' ',' Maximim height ? ',
     &          0,'W',0,'-',1,'max bin height',IER,nbhelp)
        goto 500
      else
        goto 500
      endif

C Calculate bin sizes from max and min of data.
  300 INIT = 0
      DO 320 I=IDS,IDF
         ID = I

C Read values of the climatic parameters for this day.
         CALL CLMGET(ID)

c Set time range.
         JS=KDS
         JF=KDF
         IF(I.EQ.IDS)JS=MAX0(KDS,IT1)
         IF(I.EQ.IDF)JF=MIN0(KDF,IT2)

C Dayl is not referenced.
C         DAYL=JF-JS+1

C AND ANALYSE IT
         DO 310 J=JS,JF

C GET THE REQUIRED VALUE
           if(IGET.eq.1)then
             VAL=CDIF(J)
           elseif(IGET.eq.2)then
             VAL=CTMP(J)
           elseif(IGET.eq.3)then
             VAL=CDNR(J)
           elseif(IGET.eq.4)then
             VAL=CVEL(J)
           elseif(IGET.eq.5)then
             VAL=CDIR(J)
           elseif(IGET.eq.6)then
             VAL=CHUM(J)
           endif

           IF (INIT.NE.0)goto 904
           XMAX = VAL
           XMIN = XMAX
           INIT = 1
           GOTO 310
  904      IF (VAL .GT. XMAX) XMAX = VAL
           IF (VAL .LT. XMIN) XMIN = VAL
  310    CONTINUE
  320 CONTINUE

      XDIFF=(XMAX-XMIN)/100.0
      XBIN1 = XMIN-XDIFF
      XBINN = XMAX+XDIFF
      IF(NBINS.EQ.0)THEN

c Width of bin fixed.
        NB=int((XBINN-XBIN1)/DXBIN)
        NBINS=NB+1
        XDIFF=((NBINS*DXBIN)-(XBINN-XBIN1))/2.0
        XBIN1=XBIN1-XDIFF
        XBINN=XBINN+XDIFF
      ELSE IF(NBINS.NE.0)THEN
        DXBIN = (XBINN-XBIN1) / NBINS
      ENDIF
      IF(DXBIN.GT.0.0)goto 500
      call edisp(iuout,' Data range error: plot abandoned.')
      IFLAGF=1
      RETURN

C Fill bins with hits.
  500 DO 510 I=1,NBINS
         INBIN(I) = 0
         ICBIN(I) = 0
  510 CONTINUE
      IHITS = 0
      IMISS = 0

      DO 620 I = IDS,IDF
        ID = I

C Read values of the climatic parameters for this day.
        CALL CLMGET(ID)

C Set time range.
        JS=KDS
        JF=KDF
        IF(I.EQ.IDS)JS=MAX0(KDS,IT1)
        IF(I.EQ.IDF)JF=MIN0(KDF,IT2)
C        DAYL=JF-JS+1

C AND ANALYSE IT
        DO 610 J=JS,JF

C GET THE REQUIRED VALUE
          if(IGET.eq.1)then
            VAL=CDIF(J)
          elseif(IGET.eq.2)then
            VAL=CTMP(J)
          elseif(IGET.eq.3)then
            VAL=CDNR(J)
          elseif(IGET.eq.4)then
            VAL=CVEL(J)
          elseif(IGET.eq.5)then
            VAL=CDIR(J)
          elseif(IGET.eq.6)then
            VAL=CHUM(J)
          endif

          XDIFF=(XMAX-XMIN)/100.0
          IB = int(((VAL - XBIN1) / DXBIN)) + 1
          IF(IB.LE.0.OR.IB.GT.NBINS)goto 911
          INBIN(IB) = INBIN(IB) + 1
          IHITS = IHITS + 1
          goto 610
  911     CONTINUE
          IMISS = IMISS + 1
  610   CONTINUE
  620 CONTINUE

C Set up maximum number of hits in a bin, if not fixed.
      DO 700 I=1,NBINS
      IF(INBIN(I).GT.IFREQ)IFREQ=INBIN(I)
  700 CONTINUE

C Calculate cumulative frequency.
      ITOTAL=0
      DO 710 I=1,NBINS
      IF(I.EQ.1)ICBIN(I)=INBIN(I)
      IF(I.GT.1)ICBIN(I)=ICBIN(I-1)+INBIN(I)
      ITOTAL=ITOTAL+INBIN(I)
  710 CONTINUE

      RETURN
      END

C --------- HISDRW
C 'HISDRW' draws a histogram of the frequency distribution
      SUBROUTINE HISDRW(IL,IG)

      PARAMETER (MF=100)

      COMMON/FREQS/NBINS,XBIN1,XBINN,DXBIN,IFREQ,IHITS,IMISS,INBIN(MF)
      COMMON/FREQS1/ICBIN(MF),ITOTAL
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iigl,iigr,iigt,iigb,iigw,iigwh
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
#endif

      CHARACTER*10 LABELX,LABLSX(5)
      DATA LABLSX /'Temp. degC',
     &             'Srad W/m^2',
     &             'Wvel  m/s ',
     &             'Wdir degFN',
     &             'RelHum  % '/

      YMAX = IFREQ
      IF(IG.EQ.2)YMAX=ITOTAL
      YMIN = 0.0
      XMAX = XBINN
      XMIN = XBIN1

C Percentage (right) axis.
      PMAX=(YMAX/FLOAT(ITOTAL))*100.0
      PMIN=0.0

C Cast values for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,XMIN,XMAX,PMIN,PMAX,TSC,YSCP,
     &              sca,Xadd,YaddP)
      call vrtaxis(PMIN,PMAX,iigr,iigb,iigt,YaddP,YSCP,0,1,'Percent')

C Draw axis, begin with scaling factors for the X & Y axis.
C Stuff this into static memory.
      call axiscale(igwid,igheight,XMIN,XMAX,YMIN,YMAX,XSC,YSC,
     &              sca,Xadd,Yadd)
      call linescale(iigl,Xadd,XSC,iigb,Yadd,YSC)

C Pick up the data increment for the horizontal axis and then draw
C the horizontal axis.
      LABELX=LABLSX(IL)
      call horaxis(XMIN,XMAX,iigl,iigr,iigb,Xadd,XSC,1,LABELX)

C Draw the vertical axis.
      call vrtaxis(YMIN,YMAX,iigl,iigb,iigt,Yadd,YSC,1,0,
     &  'Distribution')
      call forceflush()

C Plot histogram.
      XBIN = XMIN
      igr=13
      DO 200 I = 1, NBINS
         YBIN=FLOAT(INBIN(I))
         IF(IG.EQ.2)YBIN=FLOAT(ICBIN(I))
         call egrbox(XBIN,YMIN,DXBIN,YBIN,igr)
         XBIN = XBIN + DXBIN
  200 CONTINUE
      call forceflush()

C Return.
      RETURN
      END
