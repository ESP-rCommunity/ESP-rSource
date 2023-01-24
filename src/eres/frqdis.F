C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file contains the following routines:
C      MODGRF - controls selection of frequency histogram
C      MODHED - header for graphic histogram
C      MODDRW - draws a frequency histogram of a variable

C ******************** MODGRF ********************
C MODGRF draws a histogram of the frequencies of
C occurrence of values of the user-specified
C time-dependant variable, or provides a summary 
C listing of the data.
C act='m' called from ipv metric : sets bin width uses current IGETS
C act='d' called from ipv demand : sets bin width uses current IGETS
C act='t' only text output allowed (called from stats.F).
C act='p' iget parameters already set.
C act='-' graphic output
C index is the goget index.

      SUBROUTINE MODGRF(act,index)
#include "building.h"
#include "ipvdata.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/OUTPCH/ICOUT

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)
      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      
      character SLABEL*32,GLABEL*24,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      COMMON/FREQS/NBINS,XBIN1,XBINN,DXBIN,IFREQ,IHITS,IMISS,INBIN(MF)
      integer ICBIN,IRCBIN
      COMMON/FREQS1/ICBIN(MF),IRCBIN(MF),ITOTAL
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

C Simulator parameters.
      COMMON/SPFL/spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok
      LOGICAL     spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok

C Selection menu for graphs and/or tables.
      dimension item(8)
      character item*24
      character outs*124,outsn*124,outsd*124,xfile*144,tg*1
      character delim*1,act*1,tranged*20,sq*1,trangf*12,trangefd*12
      character TLABEL*60,trange*20,tab*1
      character prompt*124,prompte*124

      logical zlist,ok
      logical HAVEIT
      logical isone,ishalf,isother
      integer NITMS,IG ! max items and current menu item
      integer order    ! for cumulative which order for graph
      integer hits     ! for graph distribution hits (0) or hours (1)
      real FNTS        ! real number of timesteps per hour
      real XONE        ! minimum of range if bin width one

#ifdef OSI
      integer iside,isize,ifont       ! passed to viewtext
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

      helpinsub='frqdis'  ! set for subroutine
      XDIFF=0.0           ! set initial value
      isone=.false.; ishalf=.false.; isother=.false.

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        if(NGET.ge.1)then
          write(prompt,'(3a)') 'Scanning ',
     &      SLABEL(1)(1:LNSLABEL(1)),' >> file...'
        else
          write(prompt,'(a)') ' Scanning data >> file...' 
        endif
      elseif(ixopen.eq.0)then
        if(NGET.ge.1)then
          write(prompt,'(3a)') 'Scanning ',
     &      SLABEL(1)(1:LNSLABEL(1)),'...'
        else
          write(prompt,'(a)') 'Scanning...' 
        endif
      endif
      write(prompte,'(2a)') prompt(1:lnblnk(prompt)),'done.'

C Make up a single quote for IPV
      sq = char(39)

C Call the menu of choices (this also sets some default options).
 1    zlist=.FALSE.
      if(act(1:1).eq.'-'.or.act(1:1).eq.'t')then

C Graphic action or text action requested.
        MENUTYPE=2
        call GOMSETUP
        call GOMENU
        if(ixopen.eq.1)then
          itru = ixunit
        else
          itru = icout
        endif
        if (MENUTYPE.eq.-1) return
      endif

C First recover bounds of selected data. If it can be done via
C getstats use that, otherwise call mmacal.
      XMAX=-1.E+10
      XMIN=1.E+10
      do 40 IX=1,NGET
        call GETSTATS(IX,HAVEIT,DMAX,DMIN,DAVE,DTOT,TXMAX,TXMIN)
        if(HAVEIT)then

C Debug.
C          write(6,*) 'get stats ',DMAX,DMIN,DAVE,DTOT,TXMAX,TXMIN
        else
          call MMACAL(IX,DMAX,DMIN,DAVE,DTOT,TXMAX,TXMIN)

C Debug.
C          write(6,*) 'get MMACAL ',DMAX,DMIN,DAVE,DTOT,TXMAX,TXMIN
        endif
        XMAX=MAX(XMAX,DMAX)
        XMIN=MIN(XMIN,DMIN)
 40   continue
      write(outs,'(3a,f11.2,a,f11.2)')'The ',
     &  SLABEL(NGET)(1:LNSLABEL(NGET)),' data range is',
     &  XMIN,' to',XMAX
      call edisp(iuout,outs)

C Reset number of bins and size of typcial bin.
      DXBIN=0.0
      NBINS=1

C Ask if user wants defaults, if not then ask for defining parameters.
      if(act(1:1).eq.'m'.or.act(1:1).eq.'d')then

C Depending on the metric being assessed set range and/or bin width.
        tab=','
        delim = 'C'
        if(index.eq.1)then

C Don't know the range for the *format line so figure it out.
C Try 1C initial bin, if that does not fit, try 2C and
C then 5C.
          DXBIN=1.0
          XBIN1=ANINT(XMIN/DXBIN)*DXBIN
          XBINN=ANINT(XMAX/DXBIN)*DXBIN
          NB=INT((XBINN-XBIN1)/DXBIN)

C Debug.
C          write(6,*) 'zone db bin ',xbin1,xbinn,nb

          if(NB.eq.0)then
            call edisp(iuout,'Range of db to small to binn.')
            call edisp(itru,'Range of db to small to binn.')
            return
          endif
          if(NB.ge.MF)then
            DXBIN=2.0
            XBIN1=ANINT(XMIN/DXBIN)*DXBIN
            XBINN=ANINT(XMAX/DXBIN)*DXBIN
            NB=INT((XBINN-XBIN1)/DXBIN)
            NBINS=NB+1
            XDIFF=((NBINS*DXBIN)-(XBINN-XBIN1))/2.0
            write(outsn,'(4a,i3,3a,F7.1,3a,F7.1)') '*format',tab,
     &        'frequency',tab,NBINS+2,tab,'5',tab,XBIN1,tab,'2.0',
     &        tab,XBINN
            if(NB.ge.MF)then
              DXBIN=5.0
              XBIN1=ANINT(XMIN/DXBIN)*DXBIN
              XBINN=ANINT(XMAX/DXBIN)*DXBIN
              NB=INT((XBINN-XBIN1)/DXBIN)
              NBINS=NB+1
              XDIFF=((NBINS*DXBIN)-(XBINN-XBIN1))/2.0
              write(outsn,'(4a,i3,3a,F7.1,3a,F7.1)') '*format',tab,
     &          'frequency',tab,NBINS+2,tab,'5',tab,XBIN1,tab,'5.0',
     &          tab,XBINN
            endif
          else
            NBINS=NB+1
            XDIFF=((NBINS*DXBIN)-(XBINN-XBIN1))/2.0
            write(outsn,'(4a,i3,3a,F7.1,3a,F7.1)') '*format',tab,
     &        'frequency',tab,NBINS+2,tab,'5',tab,XBIN1,tab,'1.0',
     &        tab,XBINN
          endif
          call SDELIM(outsn,outsd,'C',IW)
          call edisp(itru,outsd)
        elseif(index.eq.6)then
          XMAX=30.
          XMIN=12.
          DXBIN=2.0
          XBIN1=ANINT(XMIN/DXBIN)*DXBIN
          XBINN=ANINT(XMAX/DXBIN)*DXBIN
          NB=INT((XBINN-XBIN1)/DXBIN)
          write(outs,'(13a)') '*format',tab,'frequency',tab,'12',tab,
     &      '5',tab,'12.0',tab,'2.0',tab,'30.0'
          call edisp(itru,outs)
        elseif(index.eq.11.or.index.eq.12.or.index.eq.15.or.
     &         index.eq.38.or.index.eq.40)then

C Don't know the range for the *format line so figure it out.
C Try 100W initial bin, if that does not fit, try 200W and
C then 500W and finally 1000W.
          DXBIN=100.0
          XBIN1=ANINT(XMIN/DXBIN)*DXBIN
          XBINN=ANINT(XMAX/DXBIN)*DXBIN
          NB=INT((XBINN-XBIN1)/DXBIN)
          if(NB.ge.MF)then
            DXBIN=200.0
            XBIN1=ANINT(XMIN/DXBIN)*DXBIN
            XBINN=ANINT(XMAX/DXBIN)*DXBIN
            NB=INT((XBINN-XBIN1)/DXBIN)
            NBINS=NB+1
            XDIFF=((NBINS*DXBIN)-(XBINN-XBIN1))/2.0
            write(outsn,'(4a,i3,3a,F8.1,3a,F8.1)') '*format',tab,
     &        'frequency',tab,NBINS+2,tab,'5',tab,XBIN1,tab,'200.0',
     &        tab,XBINN
            if(NB.ge.MF)then
              DXBIN=500.0
              XBIN1=ANINT(XMIN/DXBIN)*DXBIN
              XBINN=ANINT(XMAX/DXBIN)*DXBIN
              NB=INT((XBINN-XBIN1)/DXBIN)
              NBINS=NB+1
              XDIFF=((NBINS*DXBIN)-(XBINN-XBIN1))/2.0
              write(outsn,'(4a,i3,3a,F8.1,3a,F8.1)') '*format',tab,
     &          'frequency',tab,NBINS+2,tab,'5',tab,XBIN1,tab,'500.0',
     &          tab,XBINN
              if(NB.ge.MF)then
                DXBIN=1000.0
                XBIN1=ANINT(XMIN/DXBIN)*DXBIN
                XBINN=ANINT(XMAX/DXBIN)*DXBIN
                NB=INT((XBINN-XBIN1)/DXBIN)
                NBINS=NB+1
                XDIFF=((NBINS*DXBIN)-(XBINN-XBIN1))/2.0
                write(outsn,'(4a,i3,3a,F9.1,3a,F9.1)') '*format',tab,
     &            'frequency',tab,NBINS+2,tab,'5',tab,XBIN1,tab,
     &            '1000.0',tab,XBINN
                if(NB.ge.MF)then
                  DXBIN=2000.0
                  XBIN1=ANINT(XMIN/DXBIN)*DXBIN
                  XBINN=ANINT(XMAX/DXBIN)*DXBIN
                  NB=INT((XBINN-XBIN1)/DXBIN)
                  NBINS=NB+1
                  XDIFF=((NBINS*DXBIN)-(XBINN-XBIN1))/2.0
                  write(outsn,'(4a,i3,3a,F9.1,3a,F9.1)') '*format',tab,
     &              'frequency',tab,NBINS+2,tab,'5',tab,XBIN1,tab,
     &              '2000.0',tab,XBINN
                  if(NB.ge.MF)then
                    DXBIN=5000.0
                    XBIN1=ANINT(XMIN/DXBIN)*DXBIN
                    XBINN=ANINT(XMAX/DXBIN)*DXBIN
                    NB=INT((XBINN-XBIN1)/DXBIN)
                    NBINS=NB+1
                    XDIFF=((NBINS*DXBIN)-(XBINN-XBIN1))/2.0
                    write(outsn,'(4a,i3,3a,F9.0,3a,F9.0)') '*format',
     &                tab,'frequency',tab,NBINS+2,tab,'5',tab,XBIN1,
     &                tab,'5000.0',tab,XBINN
                    if(NB.ge.MF)then
                      DXBIN=10000.0
                      XBIN1=ANINT(XMIN/DXBIN)*DXBIN
                      XBINN=ANINT(XMAX/DXBIN)*DXBIN
                      NB=INT((XBINN-XBIN1)/DXBIN)
                      NBINS=NB+1
                      XDIFF=((NBINS*DXBIN)-(XBINN-XBIN1))/2.0
                      write(outsn,'(4a,i3,3a,F9.0,3a,F9.0)') '*format',
     &                  tab,'frequency',tab,NBINS+2,tab,'5',tab,XBIN1,
     &                  tab,'10000.0',tab,XBINN
                    endif
                  endif
                endif
              endif
            endif
          else
            NBINS=NB+1
            XDIFF=((NBINS*DXBIN)-(XBINN-XBIN1))/2.0
            write(outsn,'(4a,i3,3a,F7.1,3a,F7.1)') '*format',tab,
     &        'frequency',tab,NBINS+2,tab,'5',tab,XBIN1,tab,'100.0',
     &        tab,XBINN
          endif
          call SDELIM(outsn,outsd,'C',IW)
          call edisp(itru,outsd)
        elseif(index.eq.13)then
          DXBIN=5.0
          XMAX=100.
          XMIN=0.
          XBIN1=ANINT(XMIN/DXBIN)*DXBIN
          XBINN=ANINT(XMAX/DXBIN)*DXBIN
          NB=INT((XBINN-XBIN1)/DXBIN)
          write(outs,'(13a)') '*format',tab,'frequency',tab,'21',tab,
     &      '5',tab,'0.0',tab,'5.0',tab,'100.0'
          call edisp(itru,outs)
        endif
        DELTA=XMAX-XMIN
        if(NB.ge.MF)then
          write(outs,'(a,3(i4,a))')'The required ',NB,'bins > ',MF,
     &                      '(max number of bins). Using ',MF-1,'bins.'
          call edisp(iuout,outs)
          NB=MF-1
        endif

C Been through all the variants, print *fields line and widen the
C range to be checked by XDIFF (as computed above)
        NBINS=NB+1
        XBIN1=XBIN1-XDIFF
        XBINN=XBINN+XDIFF

C Debug.
C        write(6,*) 'nbins xdiff xbin1 xbinn ',nbins,xdiff,xbin1,xbinn

        write(outs,'(2a)') 
     &   '*fields range distribution percent cumulative_distrib ',
     &   'cumulative_percent'
        call eddisp(itru,outs)
        call edisp(itru,'*data ')
      else

C If in pif mode the parameters will already have been set so
C use default bin setup.
        if (act(1:1).eq.'p'.and.autook)then
          ok=.TRUE.
        else
          helptopic='res_freq_default_bins'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call easkok(' ','Use default binsp?',OK,nbhelp)
        endif
        if(ok)then

C Check data range and pick a suitable value for DXBIN. No more than 
C MF bins for any given range. 
          DELTA=XMAX-XMIN
          if (DELTA.lt.1.) then
            DXBIN=0.05
          elseif (DELTA.lt.2.) then
            DXBIN=0.1
          elseif (DELTA.lt.5.) then
            DXBIN=0.25
          elseif (DELTA.lt.15.) then
            DXBIN=0.5   ! bin width one half
            ishalf=.true.
          elseif (DELTA.lt.30.) then
            DXBIN=1.0   ! bin width one
            isone=.true.
          elseif (DELTA.lt.60.) then
            DXBIN=2.0
          elseif (DELTA.lt.150.) then
            DXBIN=5.0
          elseif (DELTA.lt.300.) then
            DXBIN=10.0
          elseif (DELTA.lt.500.) then
            DXBIN=20.0
          elseif (DELTA.lt.1000.) then
            DXBIN=25.0
          elseif (DELTA.lt.2000.) then
            DXBIN=50.0
          elseif (DELTA.lt.5000.) then
            DXBIN=100.0
          elseif (DELTA.lt.10000.) then
            DXBIN=200.0
          elseif (DELTA.lt.20000.) then
            DXBIN=500.0
          elseif (DELTA.lt.50000.) then
            DXBIN=1000.0
          elseif (DELTA.lt.75000.) then
            DXBIN=1500.0
          elseif (DELTA.lt.100000.) then
            DXBIN=2000.0
          else
            DXBIN=100.0
          endif

C Bin sized from data max & min. This brings it to the nearest
C whole number. 
          XBIN1=ANINT(XMIN/DXBIN)*DXBIN
          XBINN=ANINT(XMAX/DXBIN)*DXBIN
          NB=INT((XBINN-XBIN1)/DXBIN)
          if(NB.ge.MF)then
            write(outs,'(a,3(i4,a))')'The required ',NB,'bins > ',MF,
     &                      '(max number of bins). Using ',MF-1,'bins.'
            call edisp(iuout,outs)
            NB=MF-1
          endif
          NBINS=NB+1
          if(isone)then
            continue  ! for bin width 1 do not offset
          else
            XDIFF=((NBINS*DXBIN)-(XBINN-XBIN1))/2.0  ! offset by half
            XBIN1=XBIN1-XDIFF
            XBINN=XBINN+XDIFF
          endif
        else
          XBIN1=XMIN
          XBINN=XMAX
          helptopic='res_frequency_bin_ranges'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call easkr(XBIN1,' ','Minimum parameter value for bin ?',
     &      0.,'-',0.,'-',0.,'min bin parameter',IER,nbhelp)
          call easkr(XBINN,' ','Maximum parameter value for bin ?',
     &      XBIN1,'F',0.,'-',0.,'max bin parameter',IER,nbhelp)
          call easki(NBINS,' ','Number of bins required ?',
     &      1,'F',MF,'F',0,'no of bins',IER,nbhelp)
          DXBIN=(XBINN-XBIN1)/NBINS
        endif
      endif

C Establish number of decimal places in the range text.
      call eclose(DXBIN,1.0,0.01,isone)
      call eclose(DXBIN,0.5,0.01,ishalf)
      if(isone)then

C Width is one so set the minimum to be at an integer point. If
C already at a whole number no change is made.
        isother=.false.
        if(XBIN1.ge.0.0)then
          XONE=real(int(XBIN1))  ! cast to number ie. 0.5 becomes 0.0
          XBIN1=XONE
          XONE=real(int(XBINN)+1)  ! cast to number ie. 5.5 becomes 6.0
          XBINN=XONE
        elseif(XBIN1.lt.0.0)then
          XONE=real(int(XBIN1)-1)! cast to number ie. -5.5 becomes -6.0
          XBIN1=XONE
          if(XBINN.lt.0.0)then
            XONE=real(int(XBINN))! cast to number ie. -0.5 becomes 0.0
            XBINN=XONE
          else
            XONE=real(int(XBINN)+1)! cast to number ie. 5.5 becomes 6.0
            XBINN=XONE
          endif
        endif

      elseif(ishalf)then
        isother=.false.
      else
        isother=.true.
      endif

      if(MMOD.eq.8)call usrmsg(' ',' ','-')

C Error check on bin width.
      IF(DXBIN.le.0.0)then
        call edisp(iuout,'All data zero: no frequency distribution.')
        call edisp(iuout,'Data range error: plot abandoned.')
        RETURN
      endif

C Empty bins of hits.
      DO 510 I=1,NBINS
         INBIN(I) = 0
         ICBIN(I) = 0
         IRCBIN(I)= 0
  510 CONTINUE
      IHITS = 0
      IMISS = 0
      iover = 0
      iunder = 0

C TSTART and TFINISH - start and finish times in hours from 0000 on the 
C first day of output.
      TSTART=FLOAT(IOH1)
      TFINSH=FLOAT(((IODF)*24+IOH2)-(IODS)*24)

C NDTS - the number of timesteps in a day.
      NDTS=24*NTS
      call usrmsg(prompt,'  ','-')
      do 10 IDAY=IODS,IODF
        call GOGET(IDAY)

C Add values in VAL2 to there correct bins. Loop through selected zones.
        do 400 IX=1,NGET
          IZONE=IGETNO(IX,2)
          if (IZONE.ne.0) then 
            zlist=.TRUE.
            NZNO(IX)=IZONE
          endif
          DO 421 J = 1,NDTS,NOUT

C Compute current time.
C IHRD - number of days since start of plotting period in hours.
C TIME - time in hours since start of first day plotted.
C Add half a timestep to TIME if data averaged.
            IHRD=(IDAY-IODS)*24
            call DATIME(J,ATIME)
            TIME=float(IHRD)+ATIME

C Within requested output period.
            IF(TIME.LT.(TSTART-1.0).or.TIME.GT.TFINSH)goto 421

C If there is occupancy filter and occupancy then include in check.
C Assume fully occupied.
            ih=int(ATIME+1.)
            ioc=1
            if(iocupf.eq.1) call getocup(IZONE,IDAY,J,ioc,iier)
            if(ioc.ne.0) then
              IB = INT(((VAL2(IX,J) - XBIN1) / DXBIN) + 1.)
              if(IB.LE.0)then
                iunder = iunder +1
                IMISS = IMISS + 1
              elseif(IB.GT.NBINS)then
                iover = iover +1
                IMISS = IMISS + 1
              else
                INBIN(IB) = INBIN(IB) + 1
                IHITS = IHITS + 1
              endif
            endif
 421      CONTINUE
 400    CONTINUE
 10   continue
      call usrmsg(prompte,'  ','P')
      if(MMOD.eq.8)call usrmsg('  ','  ','-')
      call edisp(iuout,' ')

C Save largest number of hits in a bin (IFREQ).
      IFREQ=0
      DO 700 I=1,NBINS
        IFREQ=MAX(IFREQ,INBIN(I))
  700 CONTINUE

C Calculate cumulative frequency.
      ITOTAL=0
      DO I=1,NBINS
        IF(I.EQ.1)ICBIN(I)=INBIN(I)
        IF(I.GT.1)ICBIN(I)=ICBIN(I-1)+INBIN(I)
        ITOTAL=ITOTAL+INBIN(I)
      ENDDO

C Calculate reverse cumulative frequency (loop downwards).
      DO I=NBINS,1,-1
        IF(I.EQ.NBINS)IRCBIN(I)=INBIN(I)
        IF(I.LT.NBINS)IRCBIN(I)=IRCBIN(I+1)+INBIN(I)
      ENDDO

C We now have the data in bins therefore ask how it should be displayed.
   75 if(act(1:1).eq.'m'.or.act(1:1).eq.'d')then
        IG=3    ! IPV metric or demand
      elseif(act(1:1).eq.'t')then
        IG=3    ! text output
      elseif(act(1:1).eq.'p')then
        IG=3    ! we already know parameters
      else

C << convert this block of code to use a standard menu
C << call so users can just exit and not have to click
C << on 'none of the above'.
    5   helptopic='res_freq_menu_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        item(1)='a frequency graph       '
        item(2)='b cumulative graph      '
        item(3)='c summary table         '
        item(4)='d frequency graph+table '
        item(5)='e cumulative graph+table'
        item(6)=' ______________________ '
        item(7)='? help                  '
        item(8)='- exit                  '
        nitms=8
        CALL EMENU(' Frequency analysis',ITEM,NITMS,IG)
        if(IG.eq.8)then
          goto 1
        elseif(IG.eq.7)then

C Help.
          helptopic='res_freq_menu_options'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('frequency help',nbhelp,'-',0,0,IER)
        elseif(IG.eq.6)then
          goto 5
        endif
      endif
      IF(IG.EQ.1.OR.IG.EQ.2.OR.IG.EQ.4.OR.IG.EQ.5)then

C Invoke drawing routines.  If echoing graphics to file, close it
C after image and advise user.
        if(MMOD.eq.8)then
          CALL MODHED('g',zlist,iocupf,ialstused)
          TLABEL='  '
          write (TLABEL,'(a,a13,f5.1,a)')
     &       SLABEL(NGET)(1:LNSLABEL(NGET)),', (Bin width =',DXBIN,')'
          iside=5; isize=1; ifont=1
          call viewtext(TLABEL,iside,isize,ifont)
          if(IG.eq.1.or.IG.eq.4)then
            order=0
            call easkab(' ','Scale options:','distribution (hits)',
     &        'distribution (hours)',ii,nbhelp)
            if(ii.eq.1) hits=0
            if(ii.eq.2) hits=1
            CALL MODDRW(1,TLABEL,order,hits)
          endif
          if(IG.eq.2.or.IG.eq.5)then

C Ask if reverse order.
            call easkab(' ','Graph options:','standard order',
     &        'reversed',ii,nbhelp)
            if(ii.eq.1) order=0
            if(ii.eq.2) order=1
            call easkab(' ','Scale options:','distribution (hits)',
     &        'distribution (hours)',ii,nbhelp)
            if(ii.eq.1) hits=0
            if(ii.eq.2) hits=1
            CALL MODDRW(2,TLABEL,order,hits)
          endif
        else
          call edisp(iuout,' Sorry, not in graphic mode...')
        endif

C Output to file - should sense automatically if graphical.
        if(ixopen.eq.1)then
          call ctlexp(xfile,ixopen,ixunit,ixpunit,'G','Freq dist',IER)
          if(ier.eq.-3)then
            continue
          else
           call edisp(iuout,'To echo more graphic commands, re-enable')
           call edisp(iuout,'in the main frquency distribution menu...')
          endif
        endif
      endif

      if(IG.EQ.3.OR.IG.EQ.4.OR.IG.EQ.5)then

C Text report. If output to file alter the edisp unit number and then
C write out tabular report.
        itru = icout
        if(ixopen.eq.1)then
          itru = ixunit
          call edisp(icout,' Output being directed to file... ')
        endif

C If frequency bin requested from IPV then do not print headings.
        if(act(1:1).eq.'m'.or.act(1:1).eq.'d')then
          continue
        else
          call edisp(itru,' ')
          CALL MODHED('t',zlist,iocupf,ialstused)
          call edisp(itru,SLABEL(NGET))
          if(ilflag.eq.0.or.ilflag.eq.2)then
            write(outs,'(2a)')' Bin   Data range       Distribution ',
     &      'frequency Cumulative frequency  Reverse Cumulative'
            call eddisp(itru,outs)
            write(outs,'(2a)')'                        timesteps hours',
     &      '    %  timesteps hours   %  timesteps hours    %'
            call eddisp(itru,outs)
          elseif(ilflag.eq.1)then
            write(outs,'(3a)')'Bin Range Timesteps Hours % ',
     &      'Cum_timesteps Cum_hours Cum_% ',
     &      'RCum_timesteps RCum_hours RCum_%'
            call eddisp(itru,outs)
          endif
        endif

C Provide initial range and then loop through each. Set XBINSJ as
C equal to the intial value for use in java IPV report.
        XBINS=XBIN1
        XBINSJ=XBINS
        XBIN=XBIN1+DXBIN
        FNTS=FLOAT(NTS)  ! remember NTS as real
        DO 101 I=1,NBINS
          if(act(1:1).eq.'d')then      ! IPV demand
            write(trange,'(F9.1,a,F9.1)') XBINS,'-',XBIN
          elseif(act(1:1).eq.'m')then  ! IPV metric

C For and IPV and in the case where the output format is java use
C integer bin label.
            if(index.eq.1.or.index.eq.6.or.index.eq.13.or.
     &         index.eq.11.or.index.eq.12.or.index.eq.15.or.
     &         index.eq.38.or.index.eq.40)then
              write(trange,'(F9.1,a,F9.1)') XBINS,'-',XBIN
            else
              write(trange,'(F9.1,a,F9.1)') XBINS,'-',XBIN
            endif
          else

C Standard text report. For bin of 1 (isone=true) no need for
C decimal place. If -0.5 bin the one decimal place otherwise 2. 
            if(isone)then
              write(trange,'(F9.0,a,F9.0)') XBINS,'-',XBIN
            elseif(ishalf)then
              write(trange,'(F9.1,a,F9.1)') XBINS,'-',XBIN
            else
              write(trange,'(F9.2,a,F9.2)') XBINS,'-',XBIN
            endif
          endif
          call SDELIM(trange,tranged,'N',IW)

C For IPV remember the last biggest bin.
          if(I.eq.NBINS) XBINJ=XBIN
          XBINS=XBIN
          XBIN=XBIN+DXBIN
          CFREQ=(FLOAT(ICBIN(I))/FLOAT(ITOTAL))*100.0
          CRFREQ=(FLOAT(IRCBIN(I))/FLOAT(ITOTAL))*100.0
          FREQ=(FLOAT(INBIN(I))/FLOAT(ITOTAL))*100.0
          if(I.eq.1)then

C If IPV and java output and RH print beginning at 0-5 and
C if IPV and java and something else put iunder on separate line.
            if(act(1:1).eq.'m')then

              if(isone)then
                write(trangf,'(a,F9.0)') '<',XBINSJ
              elseif(ishalf)then
                write(trangf,'(a,F9.1)') '<',XBINSJ
              else
                write(trangf,'(a,F9.1)') '<',XBINSJ
              endif
              call SDELIM(trangf,trangefd,'N',IW)
              if(isone)then
                WRITE(outs,'(i3,2x,a,16x,i6,a)') I-1,
     &          trangefd(1:lnblnk(trangefd)),iunder,
     &       '    0.0    0.0       0    0.0    0.0       0    0.0   0.0'
              else
                WRITE(outs,'(i3,2x,a,15x,i6,a)') I-1,
     &          trangefd(1:lnblnk(trangefd)),iunder,
     &       '    0.0    0.0       0    0.0    0.0       0    0.0   0.0'
              endif
                call eddisp(itru,outs)
              WRITE(outs,
     &          '(i3,2x,a,i6,f8.1,f6.1,i8,f8.1,f6.1,i8,f8.1,f6.1)')I,
     &          tranged,INBIN(I),FLOAT(INBIN(I))/FNTS,FREQ,
     &          ICBIN(I),FLOAT(ICBIN(I))/FNTS,CFREQ,
     &          IRCBIN(I),FLOAT(IRCBIN(I))/FNTS,CRFREQ
            else  ! act is not an IPV metric
              if(isone)then
                write(trangf,'(a,F9.0)') '<',XBINSJ
              elseif(ishalf)then
                write(trangf,'(a,F9.1)') '<',XBINSJ
              else
                write(trangf,'(a,F9.1)') '<',XBINSJ
              endif
              call SDELIM(trangf,trangefd,'N',IW)
              if(isone)then
                WRITE(outs,'(i3,2x,a,16x,i6,a)') I-1,
     &          trangefd(1:lnblnk(trangefd)),iunder,
     &       '    0.0    0.0       0    0.0    0.0       0    0.0   0.0'
              else
                WRITE(outs,'(i3,2x,a,15x,i6,a)') I-1,
     &          trangefd(1:lnblnk(trangefd)),iunder,
     &       '    0.0    0.0       0    0.0    0.0       0    0.0   0.0'
              endif
              call eddisp(itru,outs)
              WRITE(outs,
     &          '(i3,2x,a,i6,f8.1,f6.1,i8,f8.1,f6.1,i8,f8.1,f6.1)')I,
     &          tranged,INBIN(I),FLOAT(INBIN(I))/FNTS,FREQ,
     &          ICBIN(I),FLOAT(ICBIN(I))/FNTS,CFREQ,
     &          IRCBIN(I),FLOAT(IRCBIN(I))/FNTS,CRFREQ
            endif
          elseif(I.eq.NBINS)then

C If IPV output and RH print 95-100 and
            if(act(1:1).eq.'m')then

C If IPV include the `above` line after the last actual bin. 
              WRITE(outs,
     &          '(i3,2x,a,i6,f8.1,f6.1,i8,f8.1,f6.1,i8,f8.1,f6.1)')I,
     &          tranged,INBIN(I),FLOAT(INBIN(I))/FNTS,FREQ,
     &          ICBIN(I),FLOAT(ICBIN(I))/FNTS,CFREQ,
     &          IRCBIN(I),FLOAT(IRCBIN(I))/FNTS,CRFREQ
              call eddisp(itru,outs)
              if(isone)then
                write(trangf,'(a,F9.0)') '>',XBINJ
              elseif(ishalf)then
                write(trangf,'(a,F9.1)') '>',XBINJ
              else
                write(trangf,'(a,F9.1)') '>',XBINJ
              endif
              call SDELIM(trangf,trangefd,'N',IW)
              WRITE(outs,'(i3,2x,a,16x,i6,a)') I+1,
     &          trangefd(1:lnblnk(trangefd)),iover,
     &       '    0.0   0.0       0     0.0   0.0      0     0.0   0.0'
            else
              WRITE(outs,
     &          '(i3,2x,a,i6,f8.1,f6.1,i8,f8.1,f6.1,i8,f8.1,f6.1)')I,
     &          tranged,INBIN(I),FLOAT(INBIN(I))/FNTS,FREQ,
     &          ICBIN(I),FLOAT(ICBIN(I))/FNTS,CFREQ,
     &          IRCBIN(I),FLOAT(IRCBIN(I))/FNTS,CRFREQ
              call eddisp(itru,outs)
              if(isone)then
                write(trangf,'(a,F9.0)') '>',XBINJ
              elseif(ishalf)then
                write(trangf,'(a,F9.1)') '>',XBINJ
              else
                write(trangf,'(a,F9.1)') '>',XBINJ
              endif
              call SDELIM(trangf,trangefd,'N',IW)
              WRITE(outs,'(i3,2x,a,16x,i6,a)') I+1,
     &          trangefd(1:lnblnk(trangefd)),iover,
     &       '    0.0    0.0       0    0.0    0.0       0    0.0   0.0'
C             WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2,a,i6,a)') I,
C     &         tranged,INBIN(I),FREQ,ICBIN(I),CFREQ,'  ',iover,' above'
            endif
          else
            if(act(1:1).eq.'m')then
              WRITE(outs,
     &          '(i3,2x,a,i6,f8.1,f6.1,i8,f8.1,f6.1,i8,f8.1,f6.1)')I,
     &          tranged,INBIN(I),FLOAT(INBIN(I))/FNTS,FREQ,
     &          ICBIN(I),FLOAT(ICBIN(I))/FNTS,CFREQ,
     &          IRCBIN(I),FLOAT(IRCBIN(I))/FNTS,CRFREQ
            else
              WRITE(outs,
     &          '(i3,2x,a,i6,f8.1,f6.1,i8,f8.1,f6.1,i8,f8.1,f6.1)')I,
     &          tranged,INBIN(I),FLOAT(INBIN(I))/FNTS,FREQ,
     &          ICBIN(I),FLOAT(ICBIN(I))/FNTS,CFREQ,
     &          IRCBIN(I),FLOAT(IRCBIN(I))/FNTS,CRFREQ
            endif
          endif
          call eddisp(itru,outs)
  101   CONTINUE
        if(act(1:1).eq.'m'.or.act(1:1).eq.'d')then
          continue
        else
          call edisp(itru,' ')
        endif
      endif

C Allow other/ re- display?
      if(act(1:1).eq.'m'.or.act(1:1).eq.'d')then
        return
      elseif(act(1:1).eq.'t')then
        goto 1
      elseif(act(1:1).eq.'p')then
        return
      else
        goto 75
      endif

      END

C ******************** MODHED ********************
C MODHED outputs the appropriate header information
C corresponding to graphical result display.

      SUBROUTINE MODHED(act,zlist,iocupf,ialstused)
#include "building.h"
#include "model.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/OUTPCH/ICOUT
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/ZONPIK/NZ,NZNO(MCOM)

      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/SIMPKA/NSIM

      logical libheading ! have we printed report heading (library name etc).
      logical headingcontext ! has set or period changed so heading needs reprint
      common/libhead/libheading,headingcontext

      CHARACTER ETEXT*94,GTEXT*94,PDESCR*64,SDESCR*44,act*1
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      character outsd*124,outsd1*142,RSNAME*40
      logical zlist
      integer lsn1

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
      integer ilf,igfw,igfh,ild,igdw,igdh
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
      integer*8 ilf,igfw,igfh,ild,igdw,igdh
#endif

C Generate a descriptive string for the time period to be displayed
C and the simulation and output timesteps. Use opengdisp to get
C pixel references for calls to viewtext. Use winclr to erase the
C text dialogue box that would normally be displayed.
      if(act.eq.'g')then
        IF(MMOD.EQ.8)THEN

C Reduce the white space on the right so the label percent is not
C so separated from the axis.
          call winfnt(ifs)
          iiw1=8; iiw2=11; iiw3=7; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          ilf=2; ild=LIMTTY
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1)then
            call winclr
            call feedbox(iimenu,ilf,igfw,igfh)
            call opengdisp(iimenu,ild,ilf,igdw,igdh)
          endif
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
          call startbuffer()
        endif
      ENDIF

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)itru = ixunit

      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)

      lsn1=MIN0(lnblnk(RFILE),32)
      if(NSIM.gt.1)then
        WRITE(ETEXT,'(3A,I4,2A)')'Results library: ',
     &    RFILE(1:lsn1),'; results set:',ISIM,
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
      else
        WRITE(ETEXT,'(4A)')'Results library: ',RFILE(1:lsn1),
     &    '; ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
      endif
      GTEXT=ETEXT
      if(act.eq.'g')then
        iside=1; isize=0; ifont=1
        call viewtext(gtext,iside,isize,ifont)
      else
        if((.NOT.libheading).and.(.NOT.headingcontext))then
          call edisp(itru,GTEXT)
        endif
      endif

      WRITE(ETEXT,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &   SDESCR(12:lnblnk(SDESCR))
      GTEXT=ETEXT
      if(act.eq.'g')then
        iside=2; isize=0; ifont=1
        call viewtext(gtext,iside,isize,ifont)
      else
        if((.NOT.libheading).and.(.NOT.headingcontext))then
          call edisp(itru,GTEXT)
        endif
      endif

C If information is zone based, include zone list in header. Check to see
C if names of zones can be shown.
      if(zlist)then
        if(ialstused.ne.0)then
          WRITE(ETEXT,'(1x,2a)')'Using anchor list ',ALOCLBL(ialstused)
          GTEXT=ETEXT
          if(act.eq.'g')then
            iside=3; isize=0; ifont=1
            call viewtext(gtext,iside,isize,ifont)
          else
            call edisp(itru,GTEXT)
          endif
        else
C          if(nzonec.ge.NZ)then  ! Spading off, comment out option.
C            call colznlist(3,1,ierr)
C          else
            call znlist(outsd,len,ierr) 
            if(ierr.ne.1)then
              write(outsd1,'(2a)')'Frequency in ',outsd
              if(act.eq.'g')then
                iside=3; isize=0; ifont=1
                call viewtext(outsd1,iside,isize,ifont)
              else
                call edisp(itru,outsd1)
              endif
            endif
C          endif
        endif
        if(iocupf.eq.1)then
          GTEXT=' Filtered by occupancy'
          if(act.eq.'g')then
            iside=4; isize=0; ifont=1
            call viewtext(gtext,iside,isize,ifont)
          else
            call edisp(itru,GTEXT)
          endif
        else
          GTEXT=' Not filtered by occupancy'
        endif
      else
        if(iocupf.eq.1)then
          GTEXT=' Filtered by occupancy'
          if(act.eq.'g')then
            iside=3; isize=0; ifont=1
            call viewtext(gtext,iside,isize,ifont)
          else
            call edisp(itru,GTEXT)
          endif
        else
          GTEXT=' Not filtered by occupancy'
        endif
      endif
      if(act.eq.'g')then
        call forceflush()
      endif

      RETURN
      END

C ******************** MODDRW ********************
C MODDRW draws a histogram of the frequency distribution.
C If order=0 then cumulative in standard order order=1 
C cumulative in reverse order. Hits=0 distribution in
C number of hits =1 in hours.
      SUBROUTINE MODDRW(IG,alabel,order,hits)
#include "building.h"

C Parameters
      integer IG
      character alabel*60
      integer order  ! 0 standard 1 reversed
      integer hits   ! 0 distrib via hits 1 distrib via hours

      integer ncomp,ncon
      common/C1/NCOMP,NCON
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/FREQS/NBINS,XBIN1,XBINN,DXBIN,IFREQ,IHITS,IMISS,INBIN(MF)
      integer ICBIN,IRCBIN
      COMMON/FREQS1/ICBIN(MF),IRCBIN(MF),ITOTAL
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/ZONPIK/NZ,NZNO(MCOM)
      character LABEL*12
      logical colok

#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iicol,ibsize,ipixw,ipixh
      integer iid1,iid2
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iicol,ibsize,ipixw,ipixh
      integer*8 iid1,iid2
#endif


C Check if colour graph is possible.
      colok=.false.
      if(nzonec.ge.NCOMP)colok=.true.
      if(colok)then
        iicol=0; ibsize=0
        call winscl('-',iicol)  ! ensure axis drawn in black
      endif

C Set vertical scale.
      if(hits.eq.0)then
        YMAX = IFREQ
        IF(IG.EQ.2)YMAX=ITOTAL
        PMAX=(YMAX/FLOAT(ITOTAL))*100.0
      else
        YMAX = FLOAT(IFREQ)/FLOAT(NTS)
        IF(IG.EQ.2)YMAX=FLOAT(ITOTAL)/FLOAT(NTS)
        PMAX=(YMAX/(FLOAT(ITOTAL)/FLOAT(NTS)))*100.0
      endif
      YMIN = 0.0
      XMAX = XBINN
      XMIN = XBIN1

C Reset graphing area and cast values for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh

C Compute scales and offsets.
      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,XMIN,XMAX,YMIN,YMAX,xsc,ysc,sca,
     &              Xadd,Yadd)

C Stuff into static variables for later access.
      call linescale(iigl,Xadd,xsc,iigb,Yadd,ysc)

C Left axis.
      if(hits.eq.0)then
        LABEL='Distribution'
        call textpixwidth(LABEL,ipixw,ipixh)    ! get its width
        iid1=15; iid2=igt-(ipixh-2); iid3=igt-1  ! Do left justified.
        call dintervalf(Ymin,Ymax,ddy1,ny,0)
        call vrtaxisdd(YMIN,YMAX,iigl,iigb,iigt,Yadd,ysc,0,
     &    ddy1,ny,0,'  ')
        call textsizeatxy(iid1,iid2,LABEL,ibsize,'-',iicol)
      else
        LABEL='Hours'
        call textpixwidth(LABEL,ipixw,ipixh)  ! get its width
        iid1=15; iid2=igt-(ipixh-2); iid3=igt-1  ! Do left justified.
        call dintervalf(Ymin,Ymax,ddy1,ny,0)
        call vrtaxisdd(YMIN,YMAX,iigl,iigb,iigt,Yadd,ysc,0,
     &    ddy1,ny,0,'  ')
        call textsizeatxy(iid1,iid2,LABEL,ibsize,'-',iicol)
      endif

C Horizontal axis, label depending on data type.
C If moderate data range use mode 1, otherwise try 0.
      if((XMAX-XMIN).lt.5.0)then
        call dintervalf(Xmin,Xmax,ddy1,ny,0)
        call horaxisdd(XMIN,XMAX,iigl,iigr,iigb,Xadd,xsc,0,
     &    ddy1,ny,'  ')
      elseif((XMAX-XMIN).ge.5.0.and.(XMAX-XMIN).lt.1000.)then
        call dintervalf(Xmin,Xmax,ddy1,ny,1)
        call horaxisdd(XMIN,XMAX,iigl,iigr,iigb,Xadd,xsc,1,
     &    ddy1,ny,'  ')
      else
        call dintervalf(Xmin,Xmax,ddy1,ny,0)
        call horaxisdd(XMIN,XMAX,iigl,iigr,iigb,Xadd,xsc,0,
     &    ddy1,ny,'  ')
      endif

C Right axis.
C      PMAX=(YMAX/FLOAT(ITOTAL))*100.0
      PMIN=0.0
      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,XMIN,XMAX,PMIN,PMAX,xsc,psc,sca,
     &              Xadd,Padd)
      iid1=iigr; iid2=igt-(ipixh-2)
      ALABEL='Percent'
      call textpixwidth(ALABEL,ipixw,ipixh)  ! get its width
      call dintervalf(PMIN,PMAX,ddy1,ny,0)
      call vrtaxisdd(PMIN,PMAX,iigr,iigb,iigt,Padd,psc,0,
     &  ddy1,ny,1,'  ')
      call textsizeatxy(iid1,iid2,ALABEL,ibsize,'-',iicol)

C Plot histogram, draw grey box if YBIN > 0.
      XBIN = XMIN
      igr=13
      DO 200 I = 1, NBINS
        if(order.eq.0)then
          if(IG.EQ.2)then
            if(hits.eq.0)then
              YBIN=FLOAT(ICBIN(I))
            else
              YBIN=FLOAT(ICBIN(I))/FLOAT(NTS)
            endif
            if(ICBIN(I).gt.0)call egrbox(XBIN,YMIN,DXBIN,YBIN,igr)
          else
            if(hits.eq.0)then
              YBIN=FLOAT(INBIN(I))
            else
              YBIN=FLOAT(INBIN(I))/FLOAT(NTS)
            endif
            if(INBIN(I).gt.0)call egrbox(XBIN,YMIN,DXBIN,YBIN,igr)
          endif
          XBIN = XBIN + DXBIN
        else
          if(IG.EQ.2)then
            if(hits.eq.0)then
              YBIN=FLOAT(IRCBIN(I))
            else
              YBIN=FLOAT(IRCBIN(I))/FLOAT(NTS)
            endif
            if(IRCBIN(I).gt.0)call egrbox(XBIN,YMIN,DXBIN,YBIN,igr)
          else
            if(hits.eq.0)then
              YBIN=FLOAT(INBIN(I))
            else
              YBIN=FLOAT(INBIN(I))/FLOAT(NTS)
            endif
            if(INBIN(I).gt.0)call egrbox(XBIN,YMIN,DXBIN,YBIN,igr)
          endif
          XBIN = XBIN + DXBIN
        endif
  200 CONTINUE
      call forceflush()

      RETURN
      END

