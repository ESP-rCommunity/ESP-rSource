C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

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
C  MFOUTP - main controlling routine for mass flow line graphs, stats etc.
C ASKMFNOD- Asks for one or more mass flow nodes.
C ASKMFCON- Asks for one or more mass flow connections. 
C MOMFGHED- outputs header information for flow network graphical display.

C ******************** MFOUTP ********************
C MFOUTP is the main controlling routine for mass flow line graph
C output, stats, data above or below a setpoint, timestep listings and
C histograms. Occupancy filtering (based on user selected zone) is
C implemented for all but line graphs.
C This output mode allows the various causal relationships 
C to be displayed and so visually investigated.   
C act='p' iget parameters already set.

      SUBROUTINE MFOUTP(act)
#include "building.h"
#include "net_flow.h"
C act='p' iget parameters already set.
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)

      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN
      COMMON/GRAF2/YSC(5),Yadd(5),TSC,Xadd,IGX(5),ILR(5),DT
      common/grextras/graftitle,grlbl(5),ngrlbl,lblpx(5),lblpy(5)

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      common/getmenu/menutype,igetind(65),igetflux(65)
      common/exporttg/xfile,tg,delim
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/ABSRESLIB/RFILEABS,PFILEABS,MSTRFILEABS,LAFRESABS

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF

C ihflag = 0 write 13h30, ihflag = 1 write 0.5625
C idhflg = 0 no day demarcations, idhflg = 1 write demarcation
C          between tabular reporting days.
C ilflag = 0 tabular labels on multi-lines, ilflag = 1 on one line.
      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/LIBOK/IBLIB,IFLIB,ICLIB,IELIB,IPLIB

C Occupancy filter info.
      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      common/mfocup/iocpzone

      COMMON/MFPICK/NMFGET,IMFGETNO(MFRS,7)
      COMMON/MFLABEL/SMFLABEL(MFRS),GMFLABEL(MFRS),GLMFLABEL(MFRS)
      COMMON/MFGET/FLOWVALS(MFRS),MFRECVR(MCNN+2+MCONTM)

C Simulator parameters.
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      COMMON/FREQS/NBINS,XBIN1,XBINN,DXBIN,IFREQ,IHITS,IMISS,INBIN(MF)
      COMMON/FREQS1/ICBIN(MF),ITOTAL

      dimension Yold(MZS), TIMEold(MZS)
      character outs*124,louts*248,TLABEL*60,trange*20,tranged*20
      character ALABEL(5)*12,t20*20,act*1,H*72
      CHARACTER PDESCR*60,SDESCR*44,DESCR*7,DESCR1*10
      character SMFLABEL*32,GMFLABEL*20,GLMFLABEL*32
      character lltmpc*144
      CHARACTER DESCRH*5,DESCRD*5,NT*14,XT*14
      character graftitle*64,grlbl*24,t24*24,LTIME*5
      character LAPROB*72,RSNAME*40
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      character RFILEABS*144,PFILEABS*144,MSTRFILEABS*144,LAFRESABS*144
      character xfile*144,tg*1,delim*1,tab*1
      dimension FMAX(MFRS), FMIN(MFRS), FTOTSD(MFRS)
      dimension FAVE(MFRS), FSTD(MFRS), NF(MFRS)
      dimension TMAX(MFRS), TMIN(MFRS), IQHRS(MFRS)

      integer YAXSET, ADDLIN
      integer iglib

      logical colok,ok,dok
      LOGICAL spfileok,perok,tstepok,saveok,autook,exitok,startupok
#ifdef OSI
      integer iupdown,isym    ! passed to etplot
      integer iid1,iid2
#else
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iid1,iid2
#endif

C If the network flow file has not yet been opened request information.
      if(iflib.eq.1)then
        continue
      else

C Debug.
C        write(6,*) 'iairn,lafres ',iairn,lafres

        if(iairn.ge.1)then

C If there was a flow network defined ask for the flow results file.
C Assume standard naming convention (*.mfr for flow results), if the
C user specified a different flow results file then he will have to
C specify it here as well.
        lltmpc=LAFRESABS   ! based on the file name that was passed in the command line
        llt=lnblnk(lltmpc)

C Use ifdefs because the X11 version will be returning only the
C name of the file, while the GTK version will be returning the
C name with the full path.
   22   H(1)='A flow results library is created by the simulator. It'
        H(2)='will contain the time series results for flow'
        H(3)='simulations. '
        H(4)='On computers running UNIX, the file: '
        write(H(5),'(5x,a60)') DAFRES(1:60)
        H(6)='usually exists. This is a link to an example results'
        H(7)='library set up when ESP was first installed.'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(lltmpc,'Flow Library name ? ',' ',96,DAFRES,
     &      'flow library file',IER,7)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(lltmpc,'Flow Library name ? ',' ',124,DAFRES,
     &      'flow library file',IER,7)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(lltmpc,'Flow Library name ? ',' ',144,DAFRES,
     &      'flow library file',IER,7)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(lltmpc,'Flow Library name ? ',' ',144,DAFRES,
     &      'flow library file',IER,7)
        else
          CALL EASKF(lltmpc,'Flow Library name ? ',' ',96,DAFRES,
     &    'flow library file',IER,7)
        endif

C If user cancel.
        if(ier.eq.-3)then
          return
        endif

        if(lltmpc(1:2).ne.'  '.and.lltmpc(1:4).ne.'UNKN')then

          IFRES=IFIL+4
          call EFOPRAN(IFRES,LLTMPC,NMRL,1,IER)

C If there were no errors opening the file then remember its
C name to report in header of graphs and scan its header.
          if(ier.eq.0)then
            IFLIB=1
            LAFRES=LLTMPC(1:72)   ! remember some of the file name
            call MFREADH(IFRES,ITRC)
          else
            h(1)='There was a problem opening the flow results'
            h(2)='file. Check the name given and also check the'
            h(3)='folder to see if the file exists.'
            write(louts,'(3a)') 'Problem encountered opening ',
     &        lltmpc(1:lnblnk(lltmpc)),'!'
            call edisp248(iuout,louts,100)
            call easkab(' ','Options:','respecify','cancel',
     &        ii,3)
            if(ii.eq.1)goto 22
            return
          endif
        endif
        endif
      endif

C Define prompt and set an initial value for xquery.
      outs='  '
      XQUERY=0.0

C Check if can draw in colour.
      colok=.false.
      if(nzonec.ge.MFRS)colok=.true.
      
C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        if(NMFGET.ge.1)then
          write(outs,'(2a)')GLMFLABEL(1)(1:lnblnk(GLMFLABEL(1))),
     &      '>> file.'
        else
          write(outs,'(a)')' Output being directed to file.' 
        endif
        call edisp(iuout,outs)
      elseif(ixopen.eq.0)then
        if(MMOD.eq.8)call startbuffer
      endif

C Call the menu of choices (this also sets some default options).
C First clear current selections.
      call MOFLOWSU
 1    call MFMENU
      if(ixopen.eq.1)then
        itru = ixunit
      else
        itru = icout
      endif
      if (MENUTYPE.eq.-1)then

C User exited without selecting anything to display.
        return
      elseif (MENUTYPE.eq.9)then
        call usrmsg('Psychrometrics of flow not yet supported...',
     &    'please select another view.','W')
        goto 1
      elseif (MENUTYPE.eq.7.or.MENUTYPE.eq.8)then

C Ask for set point value.
        if (act(1:1).eq.'p') then
          continue
        else
          H(1)='The ..'
          CALL EASKR(XQUERY,' ','  Test point value ? ',
     &             0.0,'-',0.0,'-',20.0,'query value',IER,1)
        endif
      endif

C Set up recovery commons.
      call GOMFGETSU

C TSTART and TFINISH - start and finish times in hours from 0000 on the 
C first day of output.
      TSTART=FLOAT(IOH1)
      TFINSH=FLOAT(((IODF)*24+IOH2)-(IODS)*24)

C Generate a descriptive string for the time period to be displayed
C and the simulation and output timesteps.
      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)

C For line graphs, check if axis is currently drawn, if not then scale.
      if (MENUTYPE.eq.3) then
        do 5 I=1,NMFGET
          IAX=IMFGETNO(I,7)
          if (YAXSET(IAX).eq.0.or.ADDLIN.eq.0) then

C Setup parameters and call GOMFGET for each output to get required data.
C GOMFGET recovers the data in FLOWVAL2 (and averages output if required.)
C Set TIMEold to -ive number so as to identify first point to be plotted.
C The YMAX,MIN variables are for axis 1 through 5 max and min values.
            call AXISINT(2,ALABEL)
            do 6 J=1,MZS
              TIMEold(J)=-1.
 6          continue
          endif
 5      continue
      elseif (MENUTYPE.eq.4) then

C For timestep listings generate initial lines.
        lsn1=MIN0(lnblnk(LAFRES),22)
        if (IHFLAG.eq.1) then
          call edisp(itru,'# Timestep performance metrics.')
          WRITE(outs,'(4A)')'# Lib: ',LAFRES(1:lsn1),
     &      ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
          call edisp(itru,outs)
        else
          call edisp(itru,'Timestep performance metrics.')
          WRITE(outs,'(4A)')'Lib: ',LAFRES(1:lsn1),
     &      ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
          call edisp(itru,outs)
        endif
      endif
      call usrmsg('Scanning data for range of values...','  ','-')

C Reset stats values.Set query hours to zero.
      do 55 I=1,MFRS 
        IQHRS(I)=0
        FTOTSD(I)=0.0
        FSTD(I)=0.0
        FAVE(I)=0.0
        NF(I)=0
        FMAX(I)= -1.E+10
        FMIN(I)=  1.E+10
 55   continue

C Set histogram bounds.
      XMAX=-1.E+10
      XMIN=1.E+10

C First recover bounds of selected data.
      do 97 IDAY=IODS,IODF
        IHRD=(IDAY-IODS)*24
        JJ=0
        do 98 IHR=1,24
          do 99 ITS=1,NTS
            JJ=JJ+1
            IDAYR=IDAY-ISDS+1
            call GOMFGET(IDAYR,IHR,ITS)
            do 40 IX=1,NMFGET

C Calculate summary stats (part 1)
              J=IHR*NTS+(ITS-1)
              TIME=real(IHRD)+real(J)/real(NTS)

C If there is occupancy filter and occupancy then use pattern
C from zone iocpzone. Start with assumption of full occupancy.
              call DATIME(JJ,ATIME)
              ih=int(ATIME+1.)
              ioc=1
              if(iocupf.eq.1.and.iocpzone.ne.0)then
                call getocup(iocpzone,IDAY,ih,ioc,iier)
              endif
              if(ioc.ne.0) then
                if(FLOWVALS(IX).gt.FMAX(IX))then
                  FMAX(IX)=FLOWVALS(IX)
                  TMAX(IX)=TIME
                elseif(FLOWVALS(IX).lt.FMIN(IX))then
                  FMIN(IX)=FLOWVALS(IX)
                  TMIN(IX)=TIME
                endif
                FAVE(IX)=FAVE(IX)+FLOWVALS(IX)
                NF(IX)=NF(IX)+1
              endif

C Set max and min for graph axis.
              IAX=IMFGETNO(IX,7)
              if (YAXSET(IAX).lt.2) then
                YMAX(IAX)=MAX(FLOWVALS(IX),YMAX(IAX))
                YMIN(IAX)=MIN(FLOWVALS(IX),YMIN(IAX))
                YAXSET(IAX)=1
              endif

C Set max and min for any histogram, (this follows the pattern
C used in frqdis.F where initial range of data is not filtered
C by occupancy.
              XMAX=MAX(FLOWVALS(IX),XMAX)
              XMIN=MIN(FLOWVALS(IX),XMIN)
 40         continue
 99       continue
 98     continue
 97   continue

C Calculate average.
      do 56 I=1,NMFGET
        if(NF(I).ne.0) FAVE(I)=FAVE(I)/real(NF(I))
  56  continue

      call usrmsg('Scanning data for range of values...done.','  ','P')
      if (MENUTYPE.eq.2) then

C Inform user of the histo range and reset nb of bins and typcial bin size.
        write(outs,'(a,a,a,f9.2,a,f10.2)')'The ',
     &    GLMFLABEL(1)(1:lnblnk(GLMFLABEL(1))),' data range is',
     &    XMIN,' to',XMAX
        call edisp(itru,outs)
        if(iocupf.eq.1.and.iocpzone.ne.0)then
          call edisp(iuout,'The data will be filtered by occupancy.')
        endif
        DXBIN=0.0
        NBINS=1

C If in pif mode use default bin setup.
        if (act(1:1).eq.'p'.and.autook)then
          ok=.TRUE.
        else
          dok=.true.
          h(1)='If you need to customize the frequency reporting'
          h(2)='say no, otherwise the default set-up will probably '
          h(3)='work for initial interactive views. '
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
        call usrmsg(' ',' ','-')

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
  510   CONTINUE
        IHITS = 0
        IMISS = 0
        iover = 0
        iunder = 0
      elseif (MENUTYPE.eq.3) then

C Write graph head and set up axis positions and draw.
        call MOMFGHED
        call AXISPOS
        call AXISDRW(TSTART,TFINSH,ALABEL,'-')
      endif

C Recover and plot data. If listing to file or text feedback then
C write out labels for each column.
      do 10 IDAY=IODS,IODF
        if (MENUTYPE.eq.4) then
          CALL STDATE(IYEAR,IDAY,DESCR,DESCR1)
          if (IDHFLG.eq.1) then
            outs='# '
            call edisp(itru,outs)
            write(outs,'(3A)')'# ',DESCR1,' timestep listing'
            call edisp(itru,outs)
            outs='# '
            call edisp(itru,outs)
          else
            louts='  '
          endif
          if (IHFLAG.eq.1) then
            louts='#Time'
          else
            louts='Time '
          endif
          K=6

C For those dumping to a spreadsheet file this would be a good
C place to write out longer label on a single line.
          do 20 IL=1,NMFGET
            if(ILFLAG.eq.0)then
              KE=K+11
            else
              lke=lnblnk(GLMFLABEL(IL))
              KE=K+lke+1
            endif
            if(KE.le.248)then
              if(ILFLAG.eq.0)then
                write (louts(K:KE),'(a,a)') '|',GMFLABEL(IL)(1:10)
                K=K+11
              else

C Also use the current delimiter.
                if(delim.eq.'-')then
                  write (louts(K:KE),'(a,a)') '|',GLMFLABEL(IL)(1:lke)
                elseif(delim.eq.'T')then
                  tab=CHAR(9)
                  write (louts(K:KE),'(a,a)') tab,GLMFLABEL(IL)(1:lke)
                elseif(delim.eq.'C')then
                  write (louts(K:KE),'(a,a)') ',',GLMFLABEL(IL)(1:lke)
                elseif(delim.eq.'S')then
                  write (louts(K:KE),'(a,a)') ' ',GLMFLABEL(IL)(1:lke)
                elseif(delim.eq.'X')then
                  write (louts(K:KE),'(a,a)') ',',GLMFLABEL(IL)(1:lke)
                endif
                K=K+lke+1
              endif
            endif
 20       continue

C Print titles on first day and only on subsequent ones if day
C demarcations are omitted.
          if (IDAY.eq.IODS)then
            call edisp(itru,louts)
          else
            if (IDHFLG.eq.1)call edisp(itru,louts)
          endif

C If multi-line labels then write out the 2nd line.
          if(ILFLAG.eq.0)then
            if (IDHFLG.eq.1) then
              louts='#    '
            else
              louts='     '
            endif
            if (IHFLAG.eq.1) then
              louts='#    '
            else
              louts='     '
            endif
            K=6
            do 225 IL=1,NMFGET
              KE=K+11
              if(KE.le.248)then
                write (louts(K:KE),'(a,a)') '|',GMFLABEL(IL)(11:20)
                K=K+11
              endif
 225        continue
            if (IDAY.eq.IODS)then
              call edisp(itru,louts)
            else
              if (IDHFLG.eq.1)call edisp(itru,louts)
            endif
          endif

          if (IDHFLG.eq.1) then
            louts='#    '
          else
            louts='     '
          endif
          if (IHFLAG.eq.1) then
            louts='#    '
          else
            louts='     '
          endif
        endif

C Loop through each timstep in the day and generate output depending
C on the menu type. JJ is a counter for the timestep in the day which
C is equivalent to the J counter in table.F
        JJ=0
        do 10 IHR=1,24
          do 10 ITS=1,NTS
            JJ=JJ+1
            IDAYR=IDAY-ISDS+1
            call GOMFGET(IDAYR,IHR,ITS)

            if (MENUTYPE.eq.4) then

C If timestep listing format output for all items.
C Compute current time.
C IHRD - number of days since start of plotting period in hours.
C TIME - time in hours since start of first day plotted.
              IHRD=(IDAY-IODS)*24
              J=IHR*NTS+(ITS-1)
              TIME=real(IHRD)+real(J)/real(NTS)

C Within requested output period.
              IF(TIME.LT.(TSTART-1.0).or.TIME.GT.TFINSH)goto 10

C Tabular list. First, recalculate time as it is done in table.F 
C If there is occupancy filter and occupancy then use pattern
C from zone iocpzone. Start with assumption of full occupancy.
              call DATIME(JJ,ATIME)
              TIME=float(IHRD)+ATIME
              louts='  '
              if (IHFLAG.eq.0) then 

C Split flow time into hours and minutes, repeating every 24 hours.
                call STIME(JJ,LTIME)
                write (louts,'(a5)') LTIME
                K=6
              else
                RDOTY=FLOAT(IDAY)+(ATIME/24.)
                write (louts,'(f10.4)') RDOTY
                K=11
              endif
              do 410 IXY=1,NMFGET
                KE=K+11
                if(KE.le.248)then

C If there is occupancy filter and occupancy then include in check.
C Assume fully occupied.
                  ih=int(ATIME+1.)
                  ioc=1
                  if(iocupf.eq.1.and.iocpzone.ne.0)then
                    call getocup(iocpzone,IDAY,ih,ioc,iier)
                  endif
                  if(ioc.ne.0) then
                    write (louts(K:KE),'(f11.4)') FLOWVALS(IXY)
                  else
                    write (louts(K:KE),'(a)') '  not occ  '
                  endif
                  K=K+11
                endif
 410          continue
              call eddisp(itru,louts)

C Debug.
C             write(6,*)TIME,(FLOWVALS(IXY),IXY=1,NMFGET)

              goto 10
            endif

C Do the other menu types. If graph reset scaling factor for appropriate axis.
            do 400 IX=1,NMFGET
              if (MENUTYPE.eq.3) then
                Iax=IMFGETNO(IX,7)
                call linescale(igl,Xadd,TSC,igb,Yadd(Iax),YSC(Iax))
                if (TIMEold(IX).lt.0.0.or.ADDLIN.eq.1) then
                  TIMEold(IX)=0
                  Yold(IX)=FLOWVALS(IX)
                endif
                ioldpos=1
              endif

C Compute current time:
C IHRD - number of days since start of plotting period in hours.
C TIME - time in hours since start of first day plotted.
              IHRD=(IDAY-IODS)*24
              J=IHR*NTS+(ITS-1)
              TIME=real(IHRD)+real(J)/real(NTS)

C Within requested output period.
              IF(TIME.LT.(TSTART-1.0).or.TIME.GT.TFINSH)goto 421

              if (MENUTYPE.eq.2) then

C First, recalculate time as it is done in table.F 
C If there is occupancy filter and occupancy then use pattern
C from zone iocpzone. Start with assumption of full occupancy.
                call DATIME(JJ,ATIME)
                ih=int(ATIME+1.)
                ioc=1
                if(iocupf.eq.1.and.iocpzone.ne.0)then
                  call getocup(iocpzone,IDAY,ih,ioc,iier)
                endif
                if(ioc.ne.0) then
                  IB = INT(((FLOWVALS(IX) - XBIN1) / DXBIN) + 1.)
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
              elseif (MENUTYPE.eq.3) then

C Draw next section of line.
                iupdown=0
                isym=0
                if (ioldpos.eq.1)then
                  call etplot(TIMEold(IX),Yold(IX),iupdown,isym)
                endif
                ioldpos=0
                IPEN=1
                MARK=0
                RMK=AMOD((TIME-(0.5*float(NOUT)/float(NTS))),DT)
                if (RMK.lt.0.001) MARK=6
                TIMEold(IX)=TIME
                Yold(IX)=FLOWVALS(IX)

C If plotting colours for different zones then recall which
C item we are dealing with. If the index of the item is greater
C than that of the number of colours re-use an earlier colour.
                if(colok)then
                  if(IMFGETNO(IX,2).gt.0)then
                    ICM=IMFGETNO(IX,2)
                    if(ICM.gt.nzonec) ICM=IMFGETNO(IX,2)-nzonec
                    if(ICM.gt.nzonec) ICM=IMFGETNO(IX,2)-nzonec
                    call winscl('z',ICM)
                    call forceflush()
                  endif
                endif

C Plot the current value - this has already been copied to Yold and
C then reset to forground colour.
                iupdown=IPEN
                isym=MARK
                call etplot(TIME,Yold(IX),iupdown,isym)
                if(colok)call winscl('-',0)
                call forceflush()
              elseif (MENUTYPE.eq.5) then

C Stats, calculate standard deviation. First, recalculate time as it
C is done in table.F If there is occupancy filter and occupancy then
C use pattern from zone iocpzone. Start with assumption of full occupancy.
                call DATIME(JJ,ATIME)
                ih=int(ATIME+1.)
                ioc=1
                if(iocupf.eq.1.and.iocpzone.ne.0)then
                  call getocup(iocpzone,IDAY,ih,ioc,iier)
                endif
                if(ioc.ne.0) then
                  FTOTSD(IX)=FTOTSD(IX)+((FAVE(IX)-FLOWVALS(IX))**2)
                  NF(IX)=NF(IX)+1
                endif
              elseif (MENUTYPE.eq.7) then

C Hours above, calculate standard deviation. First, recalculate time as it
C is done in table.F If there is occupancy filter and occupancy then
C use pattern from zone iocpzone. Start with assumption of full occupancy.
                call DATIME(JJ,ATIME)
                ih=int(ATIME+1.)
                ioc=1
                if(iocupf.eq.1.and.iocpzone.ne.0)then
                  call getocup(iocpzone,IDAY,ih,ioc,iier)
                endif
                if(ioc.ne.0) then
                  if (FLOWVALS(IX).gt.XQUERY)then
                    IQHRS(ix)=IQHRS(IX)+1
                  endif
                endif
              elseif (MENUTYPE.eq.8) then

C Hours below.
                call DATIME(JJ,ATIME)
                ih=int(ATIME+1.)
                ioc=1
                if(iocupf.eq.1.and.iocpzone.ne.0)then
                  call getocup(iocpzone,IDAY,ih,ioc,iier)
                endif
                if(ioc.ne.0) then
                  if (FLOWVALS(IX).lt.XQUERY)then
                    IQHRS(ix)=IQHRS(ix)+1
                  endif
                endif
              endif
 421          CONTINUE
 400        CONTINUE

C Reset add profile.
            ADDLIN=0
 10   continue

C Calculate standard deviation and averages.
      if (MENUTYPE.eq.5) then
        do 25 I=1,NMFGET
          if (NF(I).lt.2) then
            FSTD(I)=0.
          else
            FSTD(I)=sqrt(FTOTSD(I)/real(NF(I)-1))
          endif
 25     continue
      endif

      if (MENUTYPE.eq.2) then

C For histogram, save largest number of hits in a bin (IFREQ).
        IFREQ=0
        DO 700 I=1,NBINS
          IFREQ=MAX(IFREQ,INBIN(I))
  700   CONTINUE

C Calculate cumulative frequency.
        ITOTAL=0
        DO 710 I=1,NBINS
          IF(I.EQ.1)ICBIN(I)=INBIN(I)
          IF(I.GT.1)ICBIN(I)=ICBIN(I-1)+INBIN(I)
          ITOTAL=ITOTAL+INBIN(I)
  710   CONTINUE

C Ask user how data should be presented.
        H(1)='Frequency distribution choices. '
        IDG=1
        IG=1
        CALL MENUATOL('Frequency choices','Frequency reports:',
     &    'a frequency distribution','b cumulative freq. dist.',
     &    'c summary table ','d frequency dist. + table',
     &    'e cumulative dist. + table','f none of the above',
     &    ' ',' ',' ',' ',' ',' ',IG,IDG,1)
        if(IG.eq.6) goto 1
        IF(IG.EQ.1.OR.IG.EQ.2.OR.IG.EQ.4.OR.IG.EQ.5)then

C Invoke drawing routines.  If echoing graphics to file, close it
C after image and advise user. If there is only one item then
C write heading with its identifier, otherwise include (...) to
C indicate that several flow items (e.g. nodes) are included.
          if(MMOD.eq.8)then
            call MOMFGHED
            TLABEL='  '
            if(NMFGET.eq.1)then
              if(iocupf.eq.1.and.iocpzone.ne.0)then
                write (TLABEL,'(2a,f6.2,a)')
     &            GLMFLABEL(1)(1:lnblnk(GLMFLABEL(1))),', Bin width =',
     &            DXBIN,' (filtered)'
              else
                write (TLABEL,'(2a,f6.2)')
     &            GLMFLABEL(1)(1:lnblnk(GLMFLABEL(1))),', Bin width =',
     &            DXBIN
              endif
            else
              if(iocupf.eq.1.and.iocpzone.ne.0)then
                write (TLABEL,'(2a,f6.2,a)')
     &            GLMFLABEL(1)(1:lnblnk(GLMFLABEL(1))),
     &            ' (...), Bin width =',DXBIN,' (filtered)'
              else
                write (TLABEL,'(2a,f6.2)')
     &            GLMFLABEL(1)(1:lnblnk(GLMFLABEL(1))),
     &            ' (...), Bin width =',DXBIN
              endif
            endif
            if(IG.eq.1.or.IG.eq.4)CALL MODDRW(1,TLABEL)
            if(IG.eq.2.or.IG.eq.5)CALL MODDRW(2,TLABEL)
          else
            call edisp(iuout,' Sorry, not in graphic mode...')
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

          call edisp(itru,' ')
          call edisp(itru,GLMFLABEL(1))
          call edisp(itru,
     &' Bin   data range       Distri-  freq.   cumulative  cumulative')
          call edisp(itru,
     &'                        bution    (%)   distribution  freq (%)')

C Provide initial range and then loop through each.
          XBINS=XBIN1
          XBIN=XBIN1+DXBIN
          DO 101 I=1,NBINS
            write(trange,'(F9.2,a,F9.2)') XBINS,'-',XBIN
            call SDELIM(trange,tranged,'N',IW)
            XBINS=XBIN
            XBIN=XBIN+DXBIN
            CFREQ=(FLOAT(ICBIN(I))/FLOAT(ITOTAL))*100.0
            FREQ=(FLOAT(INBIN(I))/FLOAT(ITOTAL))*100.0
            if(I.eq.1)then
              WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2,a,i6,a)') I,
     &        tranged,INBIN(I),FREQ,ICBIN(I),CFREQ,'  ',iunder,' below'
            elseif(I.eq.NBINS)then
              WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2,a,i6,a)') I,
     &        tranged,INBIN(I),FREQ,ICBIN(I),CFREQ,'  ',iover,' above'
            else
              WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2)')I,tranged,
     &        INBIN(I),FREQ,ICBIN(I),CFREQ
            endif
            call eddisp(itru,outs)
  101     CONTINUE
        endif

C Reset the number of requested items (NMFGET) so that if another
C histogram metric is selected it will be the first one.
        call usrmsg('Current selections have been cleared,',
     &    'you will need to re-select.','P')
        NMFGET=0
      elseif (MENUTYPE.eq.3) then

C Finally label lines, matching zone line colour if used.
        do 200 I=1,NMFGET
          Iax=IMFGETNO(I,7)
          call linescale(igl,Xadd,TSC,igb,Yadd(Iax),YSC(Iax))
          t20=GMFLABEL(I)
          if(colok)then
            if(IMFGETNO(I,2).gt.0)then
              ICM=IMFGETNO(I,2)
              if(ICM.gt.nzonec) ICM=IMFGETNO(I,2)-nzonec
              if(ICM.gt.nzonec) ICM=IMFGETNO(I,2)-nzonec
              call winscl('z',ICM)
            endif
          endif
          call u2pixel(TIMEold(I),Yold(I),iid1,iid2)
          call wstxpt(iid1,iid2,t20)
          if(colok)call winscl('-',0)
          call forceflush()
 200    continue

C If there are optional user defined labels draw them.
        if(ngrlbl.gt.0)then
          do 201 ijj=1,ngrlbl
            t24=grlbl(ijj)
            iid1=lblpx(ijj); iid2=lblpy(ijj);
            call wstxpt(iid1,iid2,t24)
 201      continue
        endif
      elseif (MENUTYPE.eq.5) then

C Display stats.
        write (outs,'(a,a)') 
     &    '                           Maximum                Minimum',
     &    '              Mean   Standard'
        call eddisp(itru,outs)
        write (outs,'(a,a)') 
     &    '                       value    occurrence    value    ',
     &    'occurrence     value  deviation'
        call eddisp(itru,outs)
        do 210 I=1,NMFGET

C Create string description for time of maximum occurrance.
          MAXDOY=INT(TMAX(I)/24.)+IODS
          TMAXHR=AMOD(TMAX(I),24.)
          call STDATE(IYEAR,MAXDOY,DESCR,DESCR1)
          call EDTIME(TMAXHR,DESCRH,DESCRD,TIMER)
          write (XT,'(a,a,a)')DESCR(1:6),'@',DESCRH

C Create string description for time of minimum occurrance.
          MINDOY=INT(TMIN(I)/24.)+IODS
          TMINHR=AMOD(TMIN(I),24.)
          call STDATE(IYEAR,MINDOY,DESCR,DESCR1)
          call EDTIME(TMINHR,DESCRH,DESCRD,TIMER)
          write (NT,'(a,a,a)')DESCR(1:6),'@',DESCRH
          t20=GMFLABEL(I)
          write (outs,'(a,1x,2(f7.2,2x,a12,2x),2(f7.2,3x))') t20,
     &      FMAX(I),XT,FMIN(I),NT,FAVE(I),FSTD(I)
          call eddisp(itru,outs)
 210    continue
        if(iocupf.eq.1.and.iocpzone.ne.0)then
          call eddisp(itru,'(above data filtered by occupancy)')
        endif
      elseif (MENUTYPE.eq.7) then

C Display stats + hrs above.
        write (outs,'(a,f7.2)')'Reporting number of hours above ',XQUERY
        call edisp(itru,outs)
        call edisp(itru,'  ')
        write (outs,'(a,a)') 
     &    '                          Maximum               Minimum',
     &    '              Mean    Hours'
        call eddisp(itru,outs)
        write (outs,'(a,a)')
     &    '                      value    occurrence   value    ',
     &    'occurrence      value   above'
        call eddisp(itru,outs)
        do 220 I=1,NMFGET

C Create string description for time of maximum occurrance.
          MAXDOY=INT(TMAX(I)/24.)+IODS
          TMAXHR=AMOD(TMAX(I),24.)
          call STDATE(IYEAR,MAXDOY,DESCR,DESCR1)
          call EDTIME(TMAXHR,DESCRH,DESCRD,TIMER)
          write (XT,'(a,a,a)')DESCR(1:6),'@',DESCRH

C Create string description for time of minimum occurrance.
          MINDOY=INT(TMIN(I)/24.)+IODS
          TMINHR=AMOD(TMIN(I),24.)
          call STDATE(IYEAR,MINDOY,DESCR,DESCR1)
          call EDTIME(TMINHR,DESCRH,DESCRD,TIMER)
          write (NT,'(a,a,a)')DESCR(1:6),'@',DESCRH
          AYAB=float(IQHRS(I))*float(NOUT)/float(NTS)
          t20=GMFLABEL(I)
          write (outs,'(a,1x,2(f7.2,2x,a12,1x),2f9.2)') t20,
     &      FMAX(I),XT,FMIN(I),NT,FAVE(I),AYAB
          call eddisp(itru,outs)

C Debug.
C          write(6,*) FMAX(I),FMIN(I),FAVE(I),FSTD(I),IQHRS(I)

 220    continue
        if(iocupf.eq.1.and.iocpzone.ne.0)then
          call eddisp(itru,'(above data filtered by occupancy)')
        endif
      elseif (MENUTYPE.eq.8) then

C Display stats + hrs below.
        write (outs,'(a,f7.2)')'Reporting number of hours below ',XQUERY
        call edisp(itru,outs)
        call edisp(itru,'  ')
        write (outs,'(a,a)') 
     &    '                          Maximum               Minimum',
     &    '              Mean    Hours'
        call eddisp(itru,outs)
        write (outs,'(a,a)') 
     &    '                      value    occurrence   value    ',
     &    'occurrence      value   below'
        call eddisp(itru,outs)
        do 230 I=1,NMFGET

C Create string description for time of maximum occurrance.
          MAXDOY=INT(TMAX(I)/24.)+IODS
          TMAXHR=AMOD(TMAX(I),24.)
          call STDATE(IYEAR,MAXDOY,DESCR,DESCR1)
          call EDTIME(TMAXHR,DESCRH,DESCRD,TIMER)
          write (XT,'(a,a,a)')DESCR(1:6),'@',DESCRH

C Create string description for time of minimum occurrance.
          MINDOY=INT(TMIN(I)/24.)+IODS
          TMINHR=AMOD(TMIN(I),24.)
          call STDATE(IYEAR,MINDOY,DESCR,DESCR1)
          call EDTIME(TMINHR,DESCRH,DESCRD,TIMER)
          write (NT,'(a,a,a)')DESCR(1:6),'@',DESCRH
          AYAB=float(IQHRS(I))*float(NOUT)/float(NTS)
          t20=GMFLABEL(I)
          write (outs,'(a,1x,2(f7.2,2x,a12,1x),2f9.2)') t20,
     &      FMAX(I),XT,FMIN(I),NT,FAVE(I),AYAB
          call eddisp(itru,outs)

C Debug.
C          write(6,*) FMAX(I),FMIN(I),FAVE(I),FSTD(I),IQHRS(I)

 230    continue
        if(iocupf.eq.1.and.iocpzone.ne.0)then
          call eddisp(itru,'(above data filtered by occupancy)')
        endif
      endif

      goto 1

      END


C ************************ ASKMFNOD ************************
C ASKMFNOD: Asks for one or more mass flow nodes.
C IOPT - restrict node types on display:
C  IOPT=0 - display all nodes
C  IOPT=1 - display internal nodes only.
C PROMPT1 and PROMPT2 are text prompt strings
C NH is number of help strings.

      SUBROUTINE ASKMFNOD(IOPT,NNPK,INPK,PROMPT1,PROMPT2,NH)
#include "net_flow.h"

      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)

      DIMENSION INPK(MNOD)

      CHARACTER VERT(MNOD)*12, NDNAM*12
      CHARACTER PROMPT1*72, PROMPT2*72

C Set up array of menu strings.
      do 10 I=1,NNOD
        VERT(I)=NDNAM(I)
 10   continue
      call EPICKS(NNPK,INPK,PROMPT1,PROMPT2,
     &                            12,NNOD,VERT,'Node select',IER,NH)

      return
      end

C ************************ ASKMFCTM ************************
C ASKMFCTM: Asks for one or more contaminants
C PROMPT1 and PROMPT2 are text prompt strings
C NH is number of help strings.

      SUBROUTINE ASKMFCTM(NCPK,ICPK,PROMPT1,PROMPT2,NH)
#include "net_flow.h"

      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)

      DIMENSION ICPK(MCONTM)

      CHARACTER VERTC(MCONTM)*12, CONTMNAM*12
      CHARACTER PROMPT1*72, PROMPT2*72

C Set up array of menu strings.
      do 10 I=1,NCONTM
        VERTC(I)=CONTMNAM(I)
 10   continue
      call EPICKS(NCPK,ICPK,PROMPT1,PROMPT2,
     &                 12,NCONTM,VERTC,'Contaminant select',IER,NH)

      return
      end

C ************************ ASKMFCON ************************
C ASKMFCON: Asks for one or more mass flow connections.  If INODE 
C is greater than zero then user specified list of connections
C associated with this node. If INODE = zero then allow display
C of all connections in the network. If INODE is less than zero
C then silently return all connections associated with the node
C (absolute value of INODE).

      SUBROUTINE ASKMFCON(INODE,NNPK,ICPK)
#include "net_flow.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)

      DIMENSION IPK(MCNN),ICPK(MCNN),ICID(MCNN)

      CHARACTER H*72,VRT(MCNN)*43
      CHARACTER LTPCMP*60,CMNAM*12,NDNAM*12
      character outs*124
      logical unixok

C Set up array of menu strings.
      NITMS=0
      do 10 I=1,NCNN
        IP=NODPS(I)
        IN=NODNE(I)
        IC=ITPCON(I)
        if (abs(INODE).gt.0) then
          if (abs(INODE).eq.IP.or.abs(INODE).eq.IN) then

C A node has been specified -> only list its connections.
            NITMS=NITMS+1
            ICID(NITMS)=I
            write(VRT(NITMS),'(5a)')
     &                      NDNAM(IP),'->',NDNAM(IN),' via ',CMNAM(IC)
          endif
        else

C List all connections.
          NITMS=NITMS+1
          ICID(NITMS)=I
          write(VRT(NITMS),'(5a)')
     &                      NDNAM(IP),'->',NDNAM(IN),' via ',CMNAM(IC)
        endif
 10   continue

      if (INODE.ge.0) then
        if (NITMS.lt.NNPK) NNPK=NITMS
        call EPICKS(NNPK,IPK,'xx','yy',
     &                     43,NITMS,VRT,'Connection selection',IER,0)

C Decode menu picks to connections.
        do 20 I=1,NNPK
          ICPK(I)=ICID(IPK(I))
 20     continue
      else

C Act silently.
        NNPK=NITMS
        do 40 I=1,NNPK
          ICPK(I)=ICID(I)
 40     continue
      endif

C Give feedback to user on selected entities.
      call isunix(unixok)
      call edisp(iuout,'Selected flow entities are:')
      call edisp(iuout,' node         node         component')
      do 30 I=1,NNPK
        IP=NODPS(ICPK(I))
        IN=NODNE(ICPK(I))
        IC=ITPCON(ICPK(I))
        if(unixok)then
	  write(outs,'(5a)') NDNAM(IP),'->',NDNAM(IN),' via ',
     &      CMNAM(IC)
          call edisp(iuout,outs)
        endif
 30   continue

      return
      end

C ******************** MOMFGHED ********************
C MOMFGHED outputs the appropriate header information corresponding
C to graphical result display of network flows.

      SUBROUTINE MOMFGHED
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/SIMPKA/NSIM
      common/grextras/graftitle,grlbl(5),ngrlbl,lblpx(5),lblpy(5)

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      CHARACTER ETEXT*124, RSNAME*40
      CHARACTER PDESCR*60,SDESCR*44
      character graftitle*64,grlbl*24,t64*64
      character LAPROB*72
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

C Generate a descriptive string for the time period to be displayed
C and the simulation and output timesteps. Use opengdisp to get
C pixel references for calls to viewtext.
      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)

      IF(MMOD.EQ.8)THEN
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call feedbox(menuchw,2,igfw,igfh)
          call opengdisp(menuchw,LIMTTY,2,igdw,igdh)
        endif
        call win3d(menuchw,9,30,4,3,igl,igr,igt,igb,igw,igwh)
        igw=igr-igl
        igwh=igb-igt
        call startbuffer()
      ENDIF

      lsn1=MIN0(lnblnk(LAFRES),22)
      if(NSIM.gt.1)then
        WRITE(ETEXT,'(3A,I4,2A)')'Lib: ',LAFRES(1:lsn1),' Set:',ISIM,
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
      else
        WRITE(ETEXT,'(4A)')'Lib: ',LAFRES(1:lsn1),
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
      endif
      iside=1
      isize=0
      ifont=1
      call viewtext(etext,iside,isize,ifont)

      WRITE(ETEXT,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &  SDESCR(11:lnblnk(SDESCR))
      iside=2
      call viewtext(etext,iside,isize,ifont)

      iancline=0
      if(graftitle(1:2).ne.'  ')then
        t64=graftitle
        if(iancline.gt.0)then
          iside=4
          isize=1
          ifont=2
          call viewtext(t64,iside,isize,ifont)
        endif
        if(iancline.eq.0)then
          iside=3
          isize=1
          ifont=2
          call viewtext(t64,iside,isize,ifont)
        endif
      endif

      RETURN
      END

