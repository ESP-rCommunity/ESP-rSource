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

C This file contains the following routines:
C      MODGRF
C      MODHED
C      MODDRW

C ******************** MODGRF ********************
C MODGRF draws a histogram of the frequencies of
C occurrence of values of the user-specified
C time-dependant variable, or provides a summary 
C listing of the data.
C act='m' called from ipv metric : sets bin width uses current IGETS
C act='d' called from ipv demand : sets bin width uses current IGETS
C act='t' only text output allowed.
C act='p' iget parameters already set.
C index is the goget index.
      SUBROUTINE MODGRF(act,index)
#include "building.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/OUTPCH/ICOUT

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      COMMON/FREQS/NBINS,XBIN1,XBINN,DXBIN,IFREQ,IHITS,IMISS,INBIN(MF)
      COMMON/FREQS1/ICBIN(MF),ITOTAL
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

C Simulator parameters.
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok

C IPV info.
      common/IPVMS/nms,imetget(MIPVM),imetmsc(MIPVM,2),nzmg(MIPVM),
     &  emgflr(MIPVM),emgsca(MIPVM),emgwtg(MIPVM),izmg(MIPVM,MCOM)

C Format of the IPV report.
C  = 1 is human readable text, 2 is tab separated, 3 is java
      common/IPVFORMAT/ipvform,ipvversion

      character outs*124,outsn*124,outsd*124,H*72,xfile*144,tg*1
      character delim*1,act*1,tranged*20,sq*1,trangf*12,trangefd*12
      character SLABEL*32,GLABEL*20,TLABEL*60,trange*20,tab*1
      character prompt*124,prompte*124

      LOGICAL     spfileok,perok,tstepok,saveok,autook,exitok,startupok
      logical zlist,ok,dok

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        if(NGET.ge.1)then
          write(prompt,'(3a)') 'Scanning ',
     &      SLABEL(1)(1:lnblnk(SLABEL(1))),' >> file...'
        else
          write(prompt,'(a)') ' Scanning data >> file...' 
        endif
      elseif(ixopen.eq.0)then
        if(NGET.ge.1)then
          write(prompt,'(3a)') 'Scanning ',
     &      SLABEL(1)(1:lnblnk(SLABEL(1))),'...'
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

C First recover bounds of selected data.
      XMAX=-1.E+10
      XMIN=1.E+10
      do 40 IX=1,NGET
        call MMACHK(IX,DMAX,DMIN,DAVE,DTOT,TXMAX,TXMIN)
        XMAX=MAX(XMAX,DMAX)
        XMIN=MIN(XMIN,DMIN)
 40   continue
      write(outs,'(a,a,a,f11.2,a,f11.2)')'The ',
     &  SLABEL(NGET)(1:lnblnk(SLABEL(NGET))),' data range is',
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
          write(6,*) 'zone db bin ',xbin1,xbinn,nb
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
C Debug...
C        write(6,*) 'nbins xdiff xbin1 xbinn ',nbins,xdiff,xbin1,xbinn
        write(outs,'(2a)') 
     &   '*fields range distribution percent cumulative_distrib ',
     &   'cumulative_percent'
        call eddisp(itru,outs)
        call edisp(itru,'*data ')
      else

C If in pif mode use default bin setup.
        if (act(1:1).eq.'p'.and.autook)then
          ok=.TRUE.
        else
          dok=.true.
          h(1)='The default bins will tend to work for quick inital'
          h(2)='views of frequency bins. To customise the reports '
          h(3)='you should say no. '
          call askok('  ','Use default bin set-up?',OK,dok,3)
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
            DXBIN=0.5
          elseif (DELTA.lt.30.) then
            DXBIN=1.0
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

C Bin sized from data max & min. 
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
          XDIFF=((NBINS*DXBIN)-(XBINN-XBIN1))/2.0
          XBIN1=XBIN1-XDIFF
          XBINN=XBINN+XDIFF
        else
          XBIN1=XMIN
          XBINN=XMAX
          H(1)='Give minimum bin value for the current parameter.'
          call easkr(XBIN1,' ',' Minimum parameter value for bin ?',
     &                     0.,'-',0.,'-',0.,'min bin parameter',IER,1)
          H(1)='Give maximum bin value for the current parameter.'
          call easkr(XBINN,' ',' Maximum parameter value for bin ?',
     &                  XBIN1,'F',0.,'-',0.,'max bin parameter',IER,1)
          H(1)='The number of frequency bins is required.'
          call easki(NBINS,' ',' Number of bins required ? ',
     &                               1,'F',MF,'F',0,'no of bins',IER,0)
          DXBIN=(XBINN-XBIN1)/NBINS
        endif
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
            if(iocupf.eq.1) call getocup(IZONE,IDAY,ih,ioc,iier)
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
      DO 710 I=1,NBINS
        IF(I.EQ.1)ICBIN(I)=INBIN(I)
        IF(I.GT.1)ICBIN(I)=ICBIN(I-1)+INBIN(I)
        ITOTAL=ITOTAL+INBIN(I)
  710 CONTINUE

C We now have the data in bins therefore ask how it should be displayed.
   75 if(act(1:1).eq.'m'.or.act(1:1).eq.'d')then
        IG=3
      elseif(act(1:1).eq.'t')then
        IG=3
      elseif(act(1:1).eq.'p')then
        IG=3
      else
        H(1)='Frequency distribution choices. '
        IDG=1
        IG=1
        CALL MENUATOL('Frequency choices','Frequency reports:',
     &    'a frequency distribution','b cumulative freq. dist.',
     &    'c summary table ','d frequency dist. + table',
     &    'e cumulative dist. + table','f none of the above',
     &    ' ',' ',' ',' ',' ',' ',IG,IDG,1)
        if(IG.eq.6) goto 1
        CALL EASKABCD('Choices:',' ','frequency distribution',
     &     'cumulative freq. dist.','summary table ','continue',IG,1)
      endif
      IF(IG.EQ.1.OR.IG.EQ.2.OR.IG.EQ.4.OR.IG.EQ.5)then

C Invoke drawing routines.  If echoing graphics to file, close it
C after image and advise user.
        if(MMOD.eq.8)then
          CALL MODHED('g',zlist,iocupf,ialstused)
          TLABEL='  '
          write (TLABEL,'(a,a13,f5.1)')
     &       SLABEL(NGET)(1:lnblnk(SLABEL(NGET))),', Bin width =',DXBIN
          if(IG.eq.1.or.IG.eq.4)CALL MODDRW(1,TLABEL)
          if(IG.eq.2.or.IG.eq.5)CALL MODDRW(2,TLABEL)
        else
          call edisp(iuout,' Sorry, not in graphic mode...')
        endif

C Output to file - should sense automatically if graphical.
        if(ixopen.eq.1)then
          call ctlexp(xfile,ixopen,ixunit,ixpunit,'G','Freq dist',IER)
          call edisp(iuout,'To echo more graphic commands, re-enable')
          call edisp(iuout,'in the main frquency distribution menu...')
        endif
      endif
      if(IG.EQ.3.OR.IG.EQ.4.OR.IG.EQ.5)then

C If output to file alter the edisp unit number and then write out
C a tabular report.
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
          call edisp(itru,
     &' Bin   data range       Distri-  freq.   cumulative  cumulative')
          call edisp(itru,
     &'                        bution    (%)   distribution  freq (%)')
        endif

C Provide initial range and then loop through each. Set XBINSJ as
C equal to the intial value for use in java IPV report.
        XBINS=XBIN1
        XBINSJ=XBINS
        XBIN=XBIN1+DXBIN
        DO 101 I=1,NBINS
          if(act(1:1).eq.'d')then
            write(trange,'(F9.1,a,F9.1)') XBINS,'-',XBIN
          elseif(act(1:1).eq.'m')then

C For and IPV and in the case where the output format is java use
C integer bin label.
            if(index.eq.1.or.index.eq.6.or.index.eq.13.or.
     &         index.eq.11.or.index.eq.12.or.index.eq.15.or.
     &         index.eq.38.or.index.eq.40)then
              if(ipvform.eq.3)then
                write(trange,'(a,i6,a,i6,a)') sq,nint(XBINS),'-',
     &           nint(XBIN),sq
              else
                write(trange,'(F9.1,a,F9.1)') XBINS,'-',XBIN
              endif
            else
              if(ipvform.eq.3)then
                write(trange,'(a,F9.1,a,F9.1,a)') sq,XBINS,'-',XBIN,sq
              else
                write(trange,'(F9.1,a,F9.1)') XBINS,'-',XBIN
              endif
            endif
          else
            write(trange,'(F9.2,a,F9.2)') XBINS,'-',XBIN
          endif
          call SDELIM(trange,tranged,'N',IW)

C For IPV remember the last biggest bin.
          if(I.eq.NBINS) XBINJ=XBIN
          XBINS=XBIN
          XBIN=XBIN+DXBIN
          CFREQ=(FLOAT(ICBIN(I))/FLOAT(ITOTAL))*100.0
          FREQ=(FLOAT(INBIN(I))/FLOAT(ITOTAL))*100.0
          if(I.eq.1)then

C If IPV and java output and RH print beginning at 0-5 and
C if IPV and java and something else put iunder on separate line.
            if(act(1:1).eq.'m')then
              if(ipvform.eq.3)then
                if(index.eq.13)then
                  WRITE(outs,'(a,i5,2x,f6.2,i9,f12.2)') 
     &              tranged(1:lnblnk(tranged)),INBIN(I),FREQ,
     &              ICBIN(I),CFREQ
                else

C Make up string for `<XX` for java IPV report, make sure there are
C no spaces via call to sdelim.
                  if(index.eq.1.or.index.eq.6.or.index.eq.11.or.
     &               index.eq.12.or.index.eq.15.or.index.eq.38.or.
     &               index.eq.40)then
                    if(ipvform.eq.3)then
                      write(trangf,'(2a,i6,a)') sq,'<',nint(XBINSJ),sq
                    else
                      write(trangf,'(a,F9.1)') '<',XBINSJ
                    endif
                  else
                    if(ipvform.eq.3)then
                      write(trangf,'(2a,F9.1,a)') sq,'<',XBINSJ,sq
                    else
                      write(trangf,'(a,F9.1)') '<',XBINSJ
                    endif
                  endif
                  call SDELIM(trangf,trangefd,'N',IW)
                  WRITE(outs,'(a,i6,a)') 
     &              trangefd(1:lnblnk(trangefd)),iunder,' 0.0 0.0 0.0'
                  call eddisp(itru,outs)
                  WRITE(outs,'(a,i5,2x,f6.2,i9,f12.2)') 
     &              tranged(1:lnblnk(tranged)),INBIN(I),FREQ,
     &              ICBIN(I),CFREQ
                endif
              else

C If non-java IPV include the `below` line before the first actual bin. 
                write(trangf,'(a,F9.1)') '<',XBINSJ
                call SDELIM(trangf,trangefd,'N',IW)
                WRITE(outs,'(i3,2x,a,i6,a)') I-1,
     &            trangefd(1:lnblnk(trangefd)),iunder,' 0.0 0.0 0.0'
                call eddisp(itru,outs)
                WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2)') I,
     &            tranged,INBIN(I),FREQ,ICBIN(I),CFREQ
              endif
            else
              write(trangf,'(a,F9.1)') '<',XBINSJ
              call SDELIM(trangf,trangefd,'N',IW)
              WRITE(outs,'(i3,2x,a,i6,a)') I-1,
     &          trangefd(1:lnblnk(trangefd)),iunder,' 0.0 0.0 0.0'
              call eddisp(itru,outs)
              WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2)') I,
     &          tranged,INBIN(I),FREQ,ICBIN(I),CFREQ
            endif
          elseif(I.eq.NBINS)then

C If IPV and java output and RH print 95-100 and
C if IPV and java and something else put iover on separate line.
            if(act(1:1).eq.'m')then
              if(ipvform.eq.3)then
                if(index.eq.13)then
                  WRITE(outs,'(a,i5,2x,f6.2,i9,f12.2)') 
     &              tranged(1:lnblnk(tranged)),INBIN(I),FREQ,
     &              ICBIN(I),CFREQ
                else

C Make up string for `>XX` for java IPV report, make sure there are
C no spaces via call to sdelim.
                  WRITE(outs,'(a,i5,2x,f6.2,i9,f12.2)') 
     &              tranged(1:lnblnk(tranged)),INBIN(I),FREQ,
     &              ICBIN(I),CFREQ
                  call eddisp(itru,outs)
                  if(index.eq.1.or.index.eq.6.or.index.eq.11.or.
     &               index.eq.12.or.index.eq.15.or.index.eq.38.or.
     &               index.eq.40)then
                    if(ipvform.eq.3)then
                      write(trangf,'(2a,i6,a)') sq,'>',nint(XBINJ),sq
                    else
                      write(trangf,'(a,F9.1)') '>',XBINJ
                    endif
                  else
                    if(ipvform.eq.3)then
                      write(trangf,'(2a,F9.1,a)') sq,'>',XBINJ,sq
                    else
                      write(trangf,'(a,F9.1)') '>',XBINJ
                    endif
                  endif
                  call SDELIM(trangf,trangefd,'N',IW)
                  WRITE(outs,'(a,i6,a)') 
     &              trangefd(1:lnblnk(trangefd)),iover,' 0.0 0.0 0.0'
                endif
              else

C If non-java IPV include the `above` line after the last actual bin. 
                WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2)') I,
     &            tranged,INBIN(I),FREQ,ICBIN(I),CFREQ
                call eddisp(itru,outs)
                write(trangf,'(a,F9.1)') '>',XBINJ
                call SDELIM(trangf,trangefd,'N',IW)
                WRITE(outs,'(i3,2x,a,i6,a)') I+1,
     &            trangefd(1:lnblnk(trangefd)),iover,' 0.0 0.0 0.0'
              endif
            else
              WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2)') I,
     &          tranged,INBIN(I),FREQ,ICBIN(I),CFREQ
              call eddisp(itru,outs)
              write(trangf,'(a,F9.1)') '>',XBINJ
              call SDELIM(trangf,trangefd,'N',IW)
              WRITE(outs,'(i3,2x,a,i6,a)') I+1,
     &          trangefd(1:lnblnk(trangefd)),iover,' 0.0 0.0 0.0'
C             WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2,a,i6,a)') I,
C     &         tranged,INBIN(I),FREQ,ICBIN(I),CFREQ,'  ',iover,' above'
            endif
          else
            if(act(1:1).eq.'m')then
              if(ipvform.eq.3)then
                WRITE(outs,'(a,i5,2x,f6.2,i9,f12.2)') 
     &           tranged(1:lnblnk(tranged)),INBIN(I),FREQ,ICBIN(I),CFREQ
              else
                WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2)')I,tranged,
     &            INBIN(I),FREQ,ICBIN(I),CFREQ
              endif
            else
              WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2)')I,tranged,
     &          INBIN(I),FREQ,ICBIN(I),CFREQ
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

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)

      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/SIMPKA/NSIM

      CHARACTER ETEXT*94,GTEXT*94,PDESCR*60,SDESCR*44,act*1
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      character outsd*80,outsd1*81,RSNAME*40
      character ALOC*12,ALOCLBL*12,ALOCTYP*4
      logical zlist

C Generate a descriptive string for the time period to be displayed
C and the simulation and output timesteps. Use opengdisp to get
C pixel references for calls to viewtext. Use winclr to erase the
C text dialogue box that would normally be displayed.
      if(act.eq.'g')then
        IF(MMOD.EQ.8)THEN
#ifdef X11
          call winclr
          call feedbox(menuchw,2,igfw,igfh)
          call opengdisp(menuchw,LIMTTY,2,igdw,igdh)
#endif
          call win3d(menuchw,12,20,5,3,igl,igr,igt,igb,igw,igwh)
          call startbuffer()
        endif
      ENDIF

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)itru = ixunit

      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)

      lsn1=MIN0(lnblnk(RFILE),22)
      if(NSIM.gt.1)then
        WRITE(ETEXT,'(3A,I4,2A)')'Lib: ',RFILE(1:lsn1),' Set:',ISIM,
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
      else
        WRITE(ETEXT,'(4A)')'Lib: ',RFILE(1:lsn1),
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
      endif
      GTEXT=ETEXT
      if(act.eq.'g')then
        CALL viewtext(GTEXT,1,0,1)
      else
        call edisp(itru,GTEXT)
      endif

      WRITE(ETEXT,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &   SDESCR(11:lnblnk(SDESCR))
      GTEXT=ETEXT
      if(act.eq.'g')then
        CALL viewtext(GTEXT,2,0,1)
      else
        call edisp(itru,GTEXT)
      endif

C If information is zone based, include zone list in header. Check to see
C if names of zones can be shown.
      if(zlist)then
        if(ialstused.ne.0)then
          WRITE(ETEXT,'(1x,2a)')'Using anchor list ',ALOCLBL(ialstused)
          GTEXT=ETEXT
          if(act.eq.'g')then
            CALL viewtext(GTEXT,3,0,1)
          else
            call edisp(itru,GTEXT)
          endif
        else
          call znlist(outsd,len,ierr) 
          if(ierr.ne.1)then
            write(outsd1,'(1x,a)')outsd
            if(act.eq.'g')then
              CALL viewtext(outsd1,3,0,1)
            else
              call edisp(itru,outsd1)
            endif
          endif
        endif
        if(iocupf.eq.1)then
          GTEXT=' Filtered by occupancy'
        else
          GTEXT=' Not filtered by occupancy'
        endif
        if(act.eq.'g')then
          CALL viewtext(GTEXT,4,0,1)
        else
          call edisp(itru,GTEXT)
        endif
      else
        if(iocupf.eq.1)then
          GTEXT=' Filtered by occupancy'
        else
          GTEXT=' Not filtered by occupancy'
        endif
        if(act.eq.'g')then
          CALL viewtext(GTEXT,3,0,1)
        else
          call edisp(itru,GTEXT)
        endif
      endif
      if(act.eq.'g')then
        call forceflush()
      endif

      RETURN
      END

C ******************** MODDRW ********************
C MODDRW draws a histogram of the frequency distribution.

      SUBROUTINE MODDRW(IG,alabel)
#include "building.h"

      COMMON/FREQS/NBINS,XBIN1,XBINN,DXBIN,IFREQ,IHITS,IMISS,INBIN(MF)
      COMMON/FREQS1/ICBIN(MF),ITOTAL
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      character alabel*60

      YMAX = IFREQ
      IF(IG.EQ.2)YMAX=ITOTAL
      YMIN = 0.0
      XMAX = XBINN
      XMIN = XBIN1

C Reset graphing area.

C Compute scales and offsets.
      call axiscale(igw,igwh,XMIN,XMAX,YMIN,YMAX,xsc,ysc,sca,
     &              Xadd,Yadd)

C Stuff into static variables for later access.
      call linescale(igl,Xadd,xsc,igb,Yadd,ysc)

C Left axis.
      call vrtaxis(YMIN,YMAX,igl,igb,igt,Yadd,ysc,0,0,'Distribution')

C Horizontal axis, label depending on data type.
C If moderate data range use mode 1, otherwise try 0.
      if((XMAX-XMIN).lt.5.0)then
        call horaxis(XMIN,XMAX,igl,igr,igb,Xadd,xsc,0,alabel)
      elseif((XMAX-XMIN).ge.5.0.and.(XMAX-XMIN).lt.1000.)then
        call horaxis(XMIN,XMAX,igl,igr,igb,Xadd,xsc,1,alabel)
      else
        call horaxis(XMIN,XMAX,igl,igr,igb,Xadd,xsc,0,alabel)
      endif

C Right axis.
      PMAX=(YMAX/FLOAT(ITOTAL))*100.0
      PMIN=0.0
      call axiscale(igw,igwh,XMIN,XMAX,PMIN,PMAX,xsc,psc,sca,
     &              Xadd,Padd)
      call vrtaxis(PMIN,PMAX,igr,igb,igt,Padd,psc,0,1,'Percent')

C Plot histogram, draw grey box if YBIN > 0.
      XBIN = XMIN
      igr=13
      DO 200 I = 1, NBINS
         YBIN=FLOAT(INBIN(I))
         if(IG.EQ.2)then
           YBIN=FLOAT(ICBIN(I))
           if(ICBIN(I).gt.0)call egrbox(XBIN,YMIN,DXBIN,YBIN,igr)
         else
           if(INBIN(I).gt.0)call egrbox(XBIN,YMIN,DXBIN,YBIN,igr)
         endif
         XBIN = XBIN + DXBIN
  200 CONTINUE
      call forceflush()

      RETURN
      END

