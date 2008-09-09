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
C      MOGRAF
C      MOWLIN
C      MOGHED
C      MOCOPT
C      MOCDAT
C      MOCGDW

C ******************** MOGRAF ********************

C MOGRAF is the main controlling routine for the graphical
C output facilities. This output mode allows the various
C causal relationships to be displayed and so visually
C investigated. All menu commands are defined in the ESP
C user's manual.

      SUBROUTINE MOGRAF
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG 
      common/getmenu/menutype,igetind(65),igetflux(65)

      CHARACTER*23 ITEM(13)
      character outs*124,h*72,LAPROB*72 

C Setup menu text.
      ITEM(1) ='2 Select result set   '
      ITEM(2) ='3 Define output period'
      ITEM(3) ='4 Select zones        '
      ITEM(4) ='  ------------------- '
      ITEM(5) ='a Time:var graph      '
      ITEM(6) ='b Intra-fabric        '
      ITEM(7) ='c 3D profile          '
      ITEM(8) ='d Frequency histogram '
      ITEM(9) ='e Var:Var graph       '
      ITEM(10)='f Network air/wtr flow'
      ITEM(11)='  ------------------- '
      ITEM(12)='? Help                '
      ITEM(13)='- Exit                '

C Graphical display possible.
    2 NITMS=13
      INO=-2

C Instanciate h() strings for the menu.
      H(1)='Graphing facilities for performance data include: '
      H(2)=' '
      H(3)=' Time:var graph -> performance data vs time.'
      H(4)=' Intra-fabric   -> condensation, temps in wall etc.'
      H(5)=' 3D profile     -> data plotted as time-of-day vs day.'
      H(6)=' Frequency histogram -> data as bar graphs.'
      H(7)=' Var:Var graph  -> performance data vs data.'
      H(8)=' Network flow   -> flow data vs time.'

    5 CALL EMENU('Graph facilities',ITEM,NITMS,INO)

C Test for illegal menu pick: because ISAVE = 1 or 2.
      IF(ISAVE.EQ.1.AND.INO.EQ.6)THEN
        CALL edisp(iuout,' The save option associated with the current')
        WRITE(outs,'(a,i2,a)')' assigned result set is',ISAVE,', so'
        CALL edisp(iuout,outs)
        CALL edisp(iuout,' this command is not available.')
        goto  2
      ENDIF

C Test for illegal menu pick.
      IF(INO.EQ.0.OR.INO.EQ.4.OR.INO.EQ.11)THEN
        INO=-1
        goto 5
      ELSEIF(INO.EQ.1)THEN
        CALL MORESS
      ELSEIF(INO.EQ.2)THEN
        CALL MOOPER
      ELSEIF(INO.EQ.3)THEN
        CALL MOZDFN
      ELSEIF(INO.EQ.5)THEN
        if(MMOD.LT.8.and.MMOD.ne.-6)then
          CALL USRMSG('Switch to graphic mode for this facility.',
     &      ' ','W')
          goto 2
        endif
        CALL MOWLIN('-')
      ELSEIF(INO.EQ.6)THEN
        if(MMOD.LT.8.and.MMOD.ne.-6)then
          CALL USRMSG('Switch to graphic mode for this facility.',
     &      ' ','W')
          goto 2
        endif
        CALL MOFCON
      ELSEIF(INO.EQ.7)THEN
        if(MMOD.LT.8.and.MMOD.ne.-6)then
          CALL USRMSG('Switch to graphic mode for this facility.',
     &      ' ','W')
          goto 2
        endif
        CALL MO3DPL
      ELSEIF(INO.EQ.8)THEN
        ivv=0
        CALL MODGRF('-',ivv)
      ELSEIF(INO.EQ.9)THEN
        if(MMOD.LT.8.and.MMOD.ne.-6)then
          CALL USRMSG('Switch to graphic mode for this facility.',
     &      ' ','W')
          goto 2
        endif
        CALL VWLIN
      ELSEIF(INO.EQ.10)THEN

C Network flow graphic output. Find and open the network flow description.
        if(iairn.eq.0)then
         call edisp(iuout,'No network flow associated with this model.')
         goto 2
        endif
        if(MMOD.LT.8.and.MMOD.ne.-6)then
          CALL USRMSG('Switch to graphic mode for this facility.',
     &      ' ','W')
          goto 2
        endif
        IRY=IYEAR
        MENUTYPE=3
        call MFOUTP('-')
      ELSEIF(INO.EQ.12)THEN
        CALL PHELPD('graphing',8,'-',0,0,IER)
      ELSEIF(INO.EQ.13)THEN
        RETURN
      ENDIF
      goto 5

      END

C ******************** MOWLIN 
C MOWLIN is the main controlling routine for line graph
C output. This output mode allows the various
C causal relationships to be displayed and so visually
C investigated.   
C act='p' iget parameters already set.

      SUBROUTINE MOWLIN(act)
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTPCH/ICOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      common/grextras/graftitle,grlbl(5),ngrlbl,lblpx(5),lblpy(5)
      COMMON/GFONT/IFS,ITFS,IMFS

      dimension Yold(MZS), TIMEold(MZS)
      dimension YSC(5), Yadd(5), igx(5), ilr(5)
      character SLABEL*32,prompt*124
      character GLABEL*20,ALABEL(5)*12,t20*20,act*1
      character graftitle*64,grlbl*24,t24*24

      integer YAXSET, ADDLIN
      integer iglib   ! if iglib is 2 then using GTK library otherwise X11
      integer oneonright  ! if only one axis on right.
      integer twoonrighta ! nearest if two axis on right
      integer twoonrightb ! farthest if two axis on right
      integer threerighta ! nearest if miscel plus two axis on right
      integer threerightb ! farthest if miscel plus two axis on right
      integer igwid,igheight  ! for use with axiscale

      logical kwatts,colok

C Define prompt
      prompt='  '

C Check if can draw in colour.
      colok=.false.
      if(nzonec.ge.NCOMP)colok=.true.
      
C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        if(NGET.ge.1)then
          write(prompt,'(a,a)') SLABEL(1)(1:lnblnk(SLABEL(1))),
     &      ' >> file.'
        else
          write(prompt,'(a)')' Output being directed to file. ' 
        endif
      elseif(ixopen.eq.0)then
        if(MMOD.eq.8)call startbuffer
      endif

C Call the menu of choices (this also sets some default options).
C First clear current selections.
      call GOMSETUP
 1    kwatts=.FALSE.
      MENUTYPE=3
      call GOMENU
      if(ixopen.eq.1)then
        itru = ixunit
      else
        itru = icout
      endif
      if (MENUTYPE.eq.-1) return

C TSTART and TFINISH - start and finish times in hours from 0000 on the 
C first day of output.
      TSTART=FLOAT(IOH1)
      TFINSH=FLOAT(((IODF)*24+IOH2)-(IODS)*24)

C NDTS - the number of timesteps in a day.
      NDTS=24*NTS

C Check if axis is currently drawn. This is for adding a profile.
C Basically if the y-axis for the chosen metric is already on the screen 
C then just draw the new profiles using the existing axis and scaling 
C factors.
      if (YAXSET(IGETNO(NGET,8)).ne.0.and.ADDLIN.eq.1) goto 99

C Check if heating/cooling plant information requested.
      do 3 I=1,NGET
        if (IGETNO(I,8).eq.2) then
          if (IGETNO(I,1).eq.8.or.IGETNO(I,1).eq.9.or.
     &        IGETNO(I,1).eq.10.or.IGETNO(I,1).eq.30.or.
     &        IGETNO(I,1).eq.31.or.IGETNO(I,1).eq.32.or.
     &        IGETNO(I,1).eq.33) then
            if (YAXSET(2).lt.2) YAXSET(2)=0
            kwatts=.TRUE.
          endif
        endif
 3    continue

C Setup parameters and call GOGET for each output day to get required data.
C GOGET recovers the data in VAL2 (and averages output if required.)
C Set TIMEold to -ive number so as to identify first point to be plotted.
C The YMAX,MIN variables are for axis 1 through 5 max and min values.
      do 4 I=1,5
        if (YAXSET(I).lt.2) then
          YMAX(I)=-1.E+10
          YMIN(I)=1.E+10
        endif
 4    continue
      do 6 I=1,MZS
        TIMEold(I)=-1.
 6    continue
      call usrmsg(prompt,'Scanning data for range of values...','-')

C First recover bounds of selected data.
      do 40 IX=1,NGET
        if (YAXSET(IGETNO(IX,8)).lt.2) then
          call MMACHK(IX,DMAX,DMIN,DAVE,DTOT,TXMAX,TXMIN)
          YAXSET(IGETNO(IX,8))=1

C Check for plant flux in kW, if so convert fluxes from W to kW
          if (IGETNO(IX,8).eq.2.and.kwatts) then
            if (IGETNO(IX,1).ne.8.and.IGETNO(IX,1).ne.9.and.
     &          IGETNO(IX,1).ne.10.and.IGETNO(IX,1).ne.30.and.
     &          IGETNO(IX,1).ne.31.and.IGETNO(IX,1).ne.32.and.
     &          IGETNO(IX,1).ne.33) then
              DMAX=DMAX/1000.
              DMIN=DMIN/1000.
            endif
          endif
          YMAX(IGETNO(IX,8))=MAX(DMAX,YMAX(IGETNO(IX,8)))
          YMIN(IGETNO(IX,8))=MIN(DMIN,YMIN(IGETNO(IX,8)))
        endif
 40   continue
      call usrmsg(prompt,'Scanning data for range of values...done.',
     &  'P')
      call usrmsg('  ','  ','-')

C Draw axis.where:
C  igx(1) is the left vertical axis.
C  igx(2) is the `Load W` vertical axis.
C  igx(3) is the `Int W/m^2' vertical axis
C  igx(4) is the `Miscel vertical axis which is placed in the centre.
C  igx(5) does not have a label and is not part of the do 100 loop.
C Set local values of offset from right end of horiz axis. Note: in 
C the GTK version the values probably should be adjusted to fit the
C current label font.
      oneonright=110
      twoonrighta=85
      twoonrightb=135
      threerighta=80
      threerightb=135

C moghed uses similar YAXSET logic to know how many right axis to draw.
      call MOGHED
      igx(1)=igl
      igx(2)=igr+threerightb
      igx(3)=igr+threerighta
      igx(4)=(igl+igr)/3
      igx(5)=2*(igl+igr)/3

C Optimise(!) axis positions.
      if (YAXSET(2).gt.0) then
        if (YAXSET(3).gt.0) then

C Axis 2,3 active, 4 either active or not still in center.
          igx(2)=igr+threerightb
          igx(3)=igr+threerighta
          igx(4)=(igl+igr)/2
        else

C Axis 2,4 active.
          if (YAXSET(4).gt.0) then
            igx(2)=igr+twoonrighta
            igx(4)=igr+twoonrightb
          else

C Axis 2 active.
            igx(2)=igr+oneonright
          endif
        endif
      else
        if (YAXSET(3).gt.0) then
          if (YAXSET(4).gt.0) then

C Axis 3, 4 active.
            igx(3)=igr+twoonrighta
            igx(4)=igr+twoonrightb
          else

C Axis 3 only active.
            igx(3)=igr+oneonright
          endif
        else

C Axis 4 only active or not. 
          if (YAXSET(4).gt.0) then
            igx(4)=igr+oneonright
          endif
        endif
      endif

C Debug.
C      write(6,*) ' igx values ',igx
C      write(6,*) ' yaxset ',YAXSET
C      write(6,*) ' igl igr igw igwh ',igl,igr,igw,igwh
C      write(6,*) ' fonts ifs itfs  & imfs & iglib ',IFS,ITFS,IMFS,iglib
C      write(6,*) ' onright ',oneonright,twoonrighta,twoonrightb
      if(IROC.eq.0)then
        ALABEL(1)='Temp: DegC'

C Could be kW for next units depends on scaling.
        ALABEL(2)='Load W'
        if (kwatts) ALABEL(2)='Load kW'
        ALABEL(3)='Int W/m^2'
        ALABEL(4)='Misc.'
      elseif(IROC.eq.1)then

C IROC, if rate of change of variable alter the axis label.
        ALABEL(1)='T DegC/hr'
        ALABEL(2)='Load W/hr'
        if (kwatts) ALABEL(2)='Load kW/hr'
        ALABEL(3)='Int W/m^2/hr'
        ALABEL(4)='Misc./hr'
      endif
      ILR(1)=0
      ILR(2)=1
      ILR(3)=1
      ILR(4)=1
      do 100 I=1,4
        if (YAXSET(I).gt.0) then

C This axis is required. Scale max and min values outwards.
          if (YAXSET(I).eq.1) then
            if (YMAX(I).lt.0) then
              YMAX(I)=YMAX(I)*0.9
            else
              YMAX(I)=YMAX(I)*1.1
            endif
            if (YMIN(I).lt.0) then
              YMIN(I)=YMIN(I)*1.1
            else
              YMIN(I)=YMIN(I)*0.9
            endif

C Round YMIN to a sensible value.
            if (YMIN(I).lt.0.0) then
              if (YMIN(I).gt.-1.0) then
                YMIN(I)=float(int(YMIN(I)*10.)-1)/10.
              elseif (YMIN(I).gt.-10.0) then
                YMIN(I)=float(INT(YMIN(I))-1)
              elseif (YMIN(I).gt.-100.0) then
                YMIN(I)=10.*float((INT(YMIN(I))/10)-1)
              elseif (YMIN(I).gt.-1000.0) then
                YMIN(I)=100.*float((INT(YMIN(I))/100)-1)
              else
                YMIN(I)=1000.*float((INT(YMIN(I))/1000)-1)
              endif
            else
              if (YMIN(I).lt.100.0) then
                YMIN(I)=10.*float(INT(YMIN(I))/10)
              elseif (YMIN(I).lt.1000.0) then
                YMIN(I)=100.*float(INT(YMIN(I))/100)
              else
                YMIN(I)=1000.*float(INT(YMIN(I))/1000)
              endif
            endif
          endif
          if (ABS(YMAX(I)-YMIN(I)).lt.0.00001) YMAX(I)=YMAX(I)+0.5

C Set-up frequency of labels (DT), number of decimal places (NDEC) and 
C draw vertical axis.
          call dinterval(TSTART-1.0,TFINSH,DT,NDEC,1)
          igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,TSTART-1.0,TFINSH,YMIN(I),
     &                  YMAX(I),TSC,YSC(I),sca,Xadd,Yadd(I))
          if (ILR(I).eq.0) then
            call vrtaxis(YMIN(I),YMAX(I),igx(I),igb,igt,Yadd(I),
     &                             YSC(I),0,ilr(I),ALABEL(I))
          else
            call vrtaxis(YMIN(I),YMAX(I),igx(I),igb,igt,Yadd(I),
     &                             YSC(I),0,ilr(I),'  ')
            call wstxpt(igx(I),igt-5,ALABEL(I))
          endif
        endif
 100  continue

C Draw horizontal (time) axis. But if there is no right axis then
C there should be logic to maximize the width of the plot.  << to be done >>
      call horaxis(TSTART-1.0,TFINSH,igl,igr,igb,Xadd,TSC,1,'Time Hrs')

C Recover and plot data.
 99   do 10 IDAY=IODS,IODF
        call GOGET(IDAY)

C Add values in VAL2 to there correct bins. Loop through selected zones.
        do 400 IX=1,NGET

C Reset scaling factor for appropriate axis.
          Iax=IGETNO(IX,8)
          call linescale(igl,Xadd,TSC,igb,Yadd(Iax),YSC(Iax))

C Loop through a day's worth of data for the current metric (IX) stepping 
C at defined output interval.
          if (TIMEold(IX).lt.0.0.or.ADDLIN.eq.1) then
            TIMEold(IX)=0

C Check for local comfort outside valididty of algorithms.
            Yold(IX)=VAL2(IX,1)
            if (IGETNO(IX,1).ge.50.AND.IGETNO(IX,1).le.59) then
              IH=1
 600          if (Yold(IX).gt.100.0) then
                Yold(IX)=VAL2(IX,(1+IH*NOUT))
                IH=IH+1
                goto 600
              endif
            endif

C Convert watts to kW if necessary.
            if (IGETNO(IX,8).eq.2.and.kwatts) then
              if (IGETNO(IX,1).ne.8.and.IGETNO(IX,1).ne.9.and.
     &            IGETNO(IX,1).ne.10.and.IGETNO(IX,1).ne.30.and.
     &            IGETNO(IX,1).ne.31.and.IGETNO(IX,1).ne.32.and.
     &            IGETNO(IX,1).ne.33) then

C The data is a load and not related to plant.  By default these loads 
C are in Watts but plant is being displayed so convert to kW.
                Yold(IX)=Yold(IX)/1000.
              endif
            endif
          endif
          ioldpos=1
          DO 421 J = 1,NDTS,NOUT

C Compute current time.
C IHRD - number of days since start of plotting period in hours.
C TIME - time in hours since start of first day plotted.
            IHRD=(IDAY-IODS)*24
            call DATIME(J,ATIME)
            TIME=float(IHRD)+ATIME

C Within requested output period.
            IF(TIME.LT.(TSTART-1.0).or.TIME.GT.TFINSH)goto 421

C Move pen to previous position on curve.  If first point then no previous 
C value therefore use current. 
C Convert watts to kW if necessary.
            if (IGETNO(IX,8).eq.2.and.kwatts) then
              if (IGETNO(IX,1).ne.8.and.IGETNO(IX,1).ne.9.and.
     &            IGETNO(IX,1).ne.10.and.IGETNO(IX,1).ne.30.and.
     &            IGETNO(IX,1).ne.31.and.IGETNO(IX,1).ne.32.and.
     &            IGETNO(IX,1).ne.33) then

C The data is a load and not related to plant.  By default these loads 
C are in Watts but plant is being displayed so convert to kW.
                VAL2(IX,J)=VAL2(IX,J)/1000.
              endif
            endif
            if (ioldpos.eq.1) call etplot(TIMEold(IX),Yold(IX),0,0)
            ioldpos=0
            IPEN=IGETNO(IX,6)
            MARK=0
            RMK=AMOD((TIME-(0.5*float(NOUT)/float(NTS))),DT)
            if (RMK.lt.0.001) MARK=IGETNO(IX,7)
            TIMEold(IX)=TIME
            ytmp=Yold(IX)
            Yold(IX)=VAL2(IX,J)

C In case of local comfort do not plot data outside bounds of algorithms
C This is passed as a PPD greater than 100%
C Setting old pen position to value at next timestep to make plot 
C look sensible.
            if (IGETNO(IX,1).ge.50.AND.IGETNO(IX,1).le.59) then
              if (VAL2(IX,J).gt.100.0) then
                IPEN=-105
                MARK=0
                ioldpos=1
                Yold(IX)=ytmp
                call usrmsg(prompt,
     &    'Cannot calculate comfort - outside range of algorithm.','-')
              endif
            endif

C If plotting colours for different zones then recall which
C zone we are dealing with.
            if(colok)then
              if(IGETNO(IX,2).gt.0)call winscl('z',IGETNO(IX,2))
            endif

C Plot the current value - this has already been copied to Yold and
C then reset to forground colour.
            call etplot(TIME,Yold(IX),IPEN,MARK)
            if(colok)call winscl('-',0)
            call forceflush()
 421      CONTINUE
 400    CONTINUE

C Reset add profile.
        ADDLIN=0
 10   continue

C Finally label lines, matching zone line colour if used.
C If IMFS is > zero then use IMFS-1 for line label.
      if(IMFS.gt.0.and.IMFS.lt.4)then
        call winfnt(imfs-1)
      endif
      do 200 I=1,NGET
        Iax=IGETNO(I,8)
        call linescale(igl,Xadd,TSC,igb,Yadd(Iax),YSC(Iax))
        t20=GLABEL(I)
        if(colok)then
          if(IGETNO(I,2).gt.0)call winscl('z',IGETNO(I,2))
        endif
        call u2pixel(TIMEold(I),Yold(I),ixs,iys)
        call wstxpt(ixs,iys,t20)
        if(colok)call winscl('-',0)
        call forceflush()
 200  continue
      if(IMFS.gt.0.and.IMFS.lt.4)then
        call winfnt(imfs)
      endif

C If there are optional user defined labels draw them.
      if(ngrlbl.gt.0)then
        do 201 ijj=1,ngrlbl
          t24=grlbl(ijj)
          call wstxpt(lblpx(ijj),lblpy(ijj),t24)
 201    continue
      endif
      goto 1

      END

C ******************** MOGHED ********************
C MOGHED outputs the appropriate header information corresponding
C to graphical result display.

      SUBROUTINE MOGHED
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/SIMPKA/NSIM
      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      common/grextras/graftitle,grlbl(5),ngrlbl,lblpx(5),lblpy(5)

      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GFONT/IFS,ITFS,IMFS

C To extend logic to maximize plotting area.
      integer YAXSET, ADDLIN
      integer iglib   ! if iglib is 2 then using GTK library otherwise X11
      integer onright  ! number of axis on right
      integer onleft   ! whether there is an axis on the left

      CHARACTER ETEXT*124, RSNAME*40
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      CHARACTER PDESCR*60,SDESCR*44,outsd*80,outsd1*81
      character ALOC*12,ALOCLBL*12,ALOCTYP*4
      character graftitle*64,grlbl*24,t64*64

C If GTK library then there are additional options << work in progress >>.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      onright = 0
      onleft = 0

C Generate a descriptive string for the time period to be displayed
C and the simulation and output timesteps. Use opengdisp to get
C pixel references for calls to viewtext.
      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)

      IF(MMOD.EQ.8)THEN
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call startbuffer
          call feedbox(menuchw,2,igfw,igfh)
          call opengdisp(menuchw,LIMTTY,2,igdw,igdh)
        endif

C Logic to alter the '30' for two different cases:
C  a) we are using GTK and a proportional font << to be done >>
C  b) there is none or one or two right axis 
C  c) there is none or one left axis

C Optimise(!) axis positions for each possible combination
C using similar logic tree to mowlin.
        if (YAXSET(1).gt.0) then
          onleft = 1
        endif
        if (YAXSET(2).gt.0) then
          if (YAXSET(3).gt.0) then
            onright=2
          else
            if (YAXSET(4).gt.0) then
              onright=2
            else
              onright=1
            endif
          endif
        else
          if (YAXSET(3).gt.0) then
            if (YAXSET(4).gt.0) then
              onright=2
            else
              onright=1
            endif
          else
            if (YAXSET(4).gt.0) then
              onright=1
            else
              onright=0
            endif
          endif
        endif
C Debug.
C        write(6,*) ' nb of right axis is currently ',onright

C Set standard fornt width so that space around graph is correct.
        call winfnt(imfs)
        if(onright.eq.0)then
          if(onleft.eq.0)then
            call win3d(menuchw,3,16,5,3,igl,igr,igt,igb,igw,igwh)
          else
            call win3d(menuchw,9,16,5,3,igl,igr,igt,igb,igw,igwh)
          endif
        elseif(onright.eq.1)then
          if(onleft.eq.0)then
            call win3d(menuchw,3,26,5,3,igl,igr,igt,igb,igw,igwh)
          else
            call win3d(menuchw,9,26,5,3,igl,igr,igt,igb,igw,igwh)
          endif
        elseif(onright.eq.2)then
          if(onleft.eq.0)then
            call win3d(menuchw,3,31,5,3,igl,igr,igt,igb,igw,igwh)
          else
            call win3d(menuchw,9,31,5,3,igl,igr,igt,igb,igw,igwh)
          endif
        endif
        call startbuffer()
      ENDIF

      lsn1=MIN0(lnblnk(RFILE),22)
      if(NSIM.gt.1)then
        WRITE(ETEXT,'(3A,I4,2A)')'Lib: ',RFILE(1:lsn1),' Set:',ISIM,
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
      else
        WRITE(ETEXT,'(4A)')'Lib: ',RFILE(1:lsn1),
     &    ': ',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM)))
      endif
      CALL viewtext(ETEXT,1,0,1)

      WRITE(ETEXT,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &  SDESCR(11:lnblnk(SDESCR))
      CALL viewtext(ETEXT,2,0,1)

      if(nzonec.ge.NZ)then
        call colznlist(3,1,ierr)
      else
        call znlist(outsd,len,ierr) 
        if(ierr.ne.1)then
          write(outsd1,'(1x,a)')outsd
          CALL viewtext(outsd1,3,0,1)
        endif
      endif

C Remember if a 4th line of text is being drawn.
      iancline=0
      if(IROC.eq.0.and.ialstused.ne.0)then
        WRITE(outsd1,'(1x,2a)')'Using anchor list ',ALOCLBL(ialstused)
        CALL viewtext(outsd1,4,0,1)
        iancline=iancline+1
      elseif(IROC.eq.1.and.ialstused.ne.0)then
        WRITE(outsd1,'(1x,3a)')'Using anchor list ',ALOCLBL(ialstused),
     &     '      (rate of change of variable plot)'
        CALL viewtext(outsd1,4,0,1)
        iancline=iancline+1
      elseif(IROC.eq.1.and.ialstused.eq.0)then
        WRITE(outsd1,'(a)') ' (rate of change of variable plot)'
        CALL viewtext(outsd1,4,0,1)
        iancline=iancline+1
      endif
      if(graftitle(1:2).ne.'  ')then
        t64=graftitle
        if(iancline.gt.0)CALL viewtext(t64,5,1,2)
        if(iancline.eq.0)CALL viewtext(t64,4,1,2)
      endif

      RETURN
      END

C ******************* MOCOPT *************************
C Returns control graph options selected by user.
C ****************************************************

              SUBROUTINE MOCOPT
              
      COMMON/OUTIN/IUOUT,IUIN              
      common/pophelp/h(60)
      COMMON/CGHOPT/ERPLOT,TRPLOT,SPPLOT,NCTLGH,ICTLGH,ISP,ITHR
      COMMON/CTLDAT/SPH,SPC,TRH,TRC,ICF,IDTYP,IPER,IBLW,PLTCTL,ILOOP
      LOGICAL ERPLOT,TRPLOT,SPPLOT,ITHR,ISP,PLTCTL,OK,dok
      character h*72
      
      write (6,*) ' Entering MOCOPT'

      CALL EDISP(IUOUT,' ') 
      h(1)='One or more of the following control parameters'     
      h(2)='may plotted with the control point temperature:'     
      h(3)='  - Error '     
      h(4)='  - Set-point'     
      h(5)='  - Throttling range.'     
      dok=.false.
      CALL ASKOK(' ',' Plot control error?',OK,dok,5)
      IF(OK)ERPLOT=.TRUE.
      dok=.false.
      CALL ASKOK(' ',' Plot control set-point?',OK,dok,5)
      IF(OK)SPPLOT=.TRUE. 
      dok=.false.
      CALL ASKOK(' ',' Plot throttling range?',OK,dok,5)
      IF(OK)TRPLOT=.TRUE.
      dok=.true.
      CALL ASKOK(' ',' Plot plant loop(s)? ',OK,dok,5)
      IF(OK)THEN
10      CALL EASKI(iloop,' ',' Which plant control loop? ',
     &             1,'F',20,'F',1,'plant loop',IER,0)
        IF(IER.NE.0)goto 10
      ENDIF

C Set control graph options counter,
      NCTLGH=1
      IF(ERPLOT)NCTLGH=NCTLGH+2
      IF(TRPLOT)NCTLGH=NCTLGH+4
      IF(SPPLOT)NCTLGH=NCTLGH+2     
C Set control graph options flag,
      IF(NCTLGH.EQ.1)ICTLGH=1
      IF(ERPLOT.AND..NOT.TRPLOT.AND..NOT.SPPLOT)ICTLGH=2
      IF(TRPLOT.AND..NOT.ERPLOT.AND..NOT.SPPLOT)ICTLGH=3
      IF(SPPLOT.AND..NOT.ERPLOT.AND..NOT.TRPLOT)ICTLGH=4
      IF(ERPLOT.AND.TRPLOT.AND..NOT.SPPLOT)ICTLGH=5
      IF(ERPLOT.AND.SPPLOT.AND..NOT.TRPLOT)ICTLGH=6
      IF(TRPLOT.AND.SPPLOT.AND..NOT.ERPLOT)ICTLGH=7
      IF(ERPLOT.AND.TRPLOT.AND.SPPLOT)ICTLGH=8
C Set control loop flag for labelling,
      PLTCTL=.FALSE.
      
      RETURN
      END

         
C ******************* MOCDAT *************************
C Returns control data for current time-step.
C ****************************************************

              SUBROUTINE MOCDAT(IZON,IDAY,TME)
         
#include "building.h"
#include "control.h"

      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/CGHOPT/ERPLOT,TRPLOT,SPPLOT,NCTLGH,ICTLGH,ISP,ITHR
      COMMON/CTLDAT/SPH,SPC,TRH,TRC,ICF,IDTYP,IPER,IBLW,PLTCTL,ILOOP
      COMMON/CCTL/ICASCF(MCOM)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      

      LOGICAL ERPLOT,TRPLOT,SPPLOT,ITHR,ISP,PLTCTL
      
      write (6,*) ' Entering MOCDAT'

C If routine called by mogdrw, time may be greater than 24.0;
      TIME=TME
      IF(TIME.GT.24.0)TIME=(TIME-(IDAY-IODS)*24.0)      

C Initialise set-point and throttling range plot options flags;
C graph only plotted if flag is set to 'true'.
      ISP=.TRUE.
      ITHR=.TRUE.

C Test to see if a control function is active;
      ICF=ICASCF(IZON)

C What day of week is present day.
      CALL EDAYR(IDAY,ID,IM)
      CALL EWEEKD(ID,IM,IYEAR,IDW)

C Read building control function data.
C If number of building control function day types are 0 then
C revert to weekday, saturday and sunday format.
      N=NBCDT(ICF)
      IF(N.EQ.0)GOTO 21
      DO 20 I=1,N
      IDTYP=I
      IF(IDAY.LE.IBCDV(ICF,I,2).AND.IDAY.GE.IBCDV(ICF,I,1))GOTO 4
20    CONTINUE
      GOTO 101     

C Weekday, saturday and sunday format.
21    IDTYP=1
      IF(IDW.EQ.6)IDTYP=2
      IF(IDW.EQ.7)IDTYP=3
      IF(IDAY.LE.IBCDV(ICF,IDTYP,2).AND.
     &        IDAY.GE.IBCDV(ICF,IDTYP,1)) GOTO 4
      GOTO 101

C Commence control data return,
4     N=NBCDP(ICF,IDTYP)
      DO 30 I=1,N
      IPER=I
      IF(I.EQ.N)GOTO 9
      IF(TIME.LE.TBCPS(ICF,IDTYP,I+1))GOTO 12
      GOTO 30
9     IF(TIME.GT.TBCPS(ICF,IDTYP,N))GOTO 12
30    CONTINUE
      GOTO 101

C Valid period established; now return control data,
12    IBLW=IBCLAW(ICF,IDTYP,IPER)
      IF(IBLW.EQ.1.OR.IBLW.EQ.3)THEN
         SPH=BMISCD(ICF,IDTYP,IPER,6)
         SPC=BMISCD(ICF,IDTYP,IPER,7)
         TRH=0.0
         TRC=0.0
         IF(IBLW.EQ.1)ISP=.FALSE.
         ITHR=.FALSE.
       ELSEIF(IBLW.EQ.5)THEN   
         SPH=BMISCD(ICF,IDTYP,IPER,6)
         SPC=BMISCD(ICF,IDTYP,IPER,7)
         TRH=BMISCD(ICF,IDTYP,IPER,8)
         TRC=BMISCD(ICF,IDTYP,IPER,9)           
         ISP=.TRUE.
         ITHR=.TRUE.
       ELSEIF(IBLW.EQ.7)THEN
         SPH=BMISCD(ICF,IDTYP,IPER,10)
         SPC=BMISCD(ICF,IDTYP,IPER,12)
         TRH=0.0
         TRC=0.0
         ISP=.TRUE.
         ITHR=.FALSE.
       ELSEIF(IBLW.EQ.8)THEN
         SPH=BMISCD(ICF,IDTYP,IPER,5)
         SPC=BMISCD(ICF,IDTYP,IPER,6)
         TRH=0.0
         TRC=0.0
         ISP=.TRUE.
         ITHR=.FALSE.
       ELSEIF(IBLW.EQ.10)THEN
         SPH=(BMISCD(ICF,IDTYP,IPER,4)+BMISCD(ICF,IDTYP,IPER,5))/2.0
         SPC=(BMISCD(ICF,IDTYP,IPER,6)+BMISCD(ICF,IDTYP,IPER,7))/2.0
         TRH=BMISCD(ICF,IDTYP,IPER,4)-BMISCD(ICF,IDTYP,IPER,5)
         TRC=BMISCD(ICF,IDTYP,IPER,6)-BMISCD(ICF,IDTYP,IPER,7)
         ISP=.TRUE.
         ITHR=.FALSE.
       ELSEIF(IBLW.EQ.2.OR.IBLW.EQ.4.OR.IBLW.EQ.9.OR.IBLW.EQ.11)THEN
         ISP=.FALSE.
         ITHR=.FALSE.
       ELSEIF(IBLW.EQ.6)THEN

C A plant control loop has been referenced.
C Loop reference is user-defined `ILOOP`.

C Set up day type and period pointers
         NDAYT=NPCDT(ILOOP)
            
C If NDAYT=0 set data to weekday, saturday and sunday format
         IF(NDAYT.EQ.0) THEN
           IDTYPP=1
           IF(IDW.EQ.6) IDTYPP=2
           IF(IDW.EQ.7) IDTYPP=3
         ELSE
           DO 10 IDTYPP=1,NDAYT
            IDS=IPCDV(ILOOP,IDTYPP,1)
            IDF=IPCDV(ILOOP,IDTYPP,2)
            IF(IDAY.GE.IDS.AND.IDAY.LE.IDF)GOTO 11
10         CONTINUE
         GOTO 101
         END IF

C Check number of periods in each day and the start and finish times,
11       NDAYP=NPCDP(ILOOP,IDTYPP)
         DO 110 IDAYP=1,NDAYP
          IPERP=IDAYP
          TPS=TPCPS(ILOOP,IDTYPP,IDAYP)
          IF(IDAYP.LT.NDAYP) THEN
           TPF=TPCPS(ILOOP,IDTYPP,IDAYP+1)
          ELSE
           TPF=24.
          END IF
          IF(TIME.GT.TPS.AND.TIME.LE.TPF) GOTO 191
110      CONTINUE
         GOTO 101

C Valid period established; now get control data        
C which depends on active plant control law, iclaw;
191      ICLAW=IPCLAW(ILOOP,IDTYP,IPERP)
         IF(ICLAW.EQ.1.OR.ICLAW.EQ.2)THEN
           SPH=PMISCD(ILOOP,IDTYP,IPERP,5)
           SPC=SPH
           TRH=PMISCD(ILOOP,IDTYP,IPERP,6)
           TRC=TRH
           ISP=.TRUE.
           ITHR=.TRUE.
         ELSEIF(ICLAW.EQ.3.OR.ICLAW.EQ.5)THEN
           SPH=(PMISCD(ILOOP,IDTYP,IPERP,4)+
     &          PMISCD(ILOOP,IDTYP,IPERP,5))/2.0
           SPC=SPH
           TRH=PMISCD(ILOOP,IDTYP,IPERP,4)-
     &         PMISCD(ILOOP,IDTYP,IPERP,5)
           TRC=TRH
           ISP=.TRUE.
           ITHR=.TRUE.
         ELSEIF(ICLAW.EQ.0.OR.ICLAW.EQ.4.OR.ICLAW.EQ.6)THEN 
           ISP=.FALSE.
           ITHR=.FALSE.
         ENDIF

C Set flag for changing labels,
         PLTCTL=.TRUE.
       ENDIF
101    RETURN
       END

C ********************** MOCGDW ***********************
C Draws the control graph(s).
      SUBROUTINE MOCGDW(CPVAL,IR,IZONE,J,II,TSTART,TFINSH,TIME,TIMEL)
#include "building.h"

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/ZONPIK/NZ,NZNO(MCOM)

      COMMON/CGHOPT/ERPLOT,TRPLOT,SPPLOT,NCTLGH,ICTLGH,ISP,ITHR
      COMMON/CTLDAT/SPH,SPC,TRH,TRC,ICF,IDTYP,IPER,IBLW,PLTCTL,ILOOP      
      COMMON/CCTL/ICASCF(MCOM)

      DIMENSION XVAL1(MS),XREM1(MS)
      character BLAB*13,label*8
      LOGICAL ERPLOT,TRPLOT,SPPLOT,ITHR,ISP,PLTCTL,NOPLOT

      XREM1(1)=0.0
      XREM1(2)=0.0
      write (6,*) ' Entering MOCGDW'

C Initialise the set-point/throttling-range plot flag,
      NOPLOT=.FALSE.
      
      ITS=NOUT/NTS
      N=24*NTS
      MARK=0
      IF(IR.EQ.0)MARK=9
      
      XVAL1(1)=CPVAL

C For each plot;
      DO 322 L=1,NCTLGH
      I=ICTLGH
      IF(ITS.GT.1)GOTO 321
      IF(TIME.LE.TSTART)GOTO 32 
      IF(L.EQ.1)THEN
        LABEL='Ctl Pt'
        IPEN=-304
      ENDIF
C If additional plot requested get control data;
      IF(L.GT.1)CALL MOCDAT(IZONE,II,TIME)
C Value plotted depends on the type of plot selected by user;
      IF(L.EQ.2.AND.I.EQ.2.OR.L.EQ.2.AND.I.EQ.5.OR.
     &   L.EQ.2.AND.I.EQ.6.OR.L.EQ.2.AND.I.EQ.8)THEN   
         XVAL1(L)=XVAL1(1)-SPH
         IPEN=-1
         LABEL='Htg Err'
         IF(PLTCTL)LABEL= 'Error'
       ELSEIF(L.EQ.3.AND.I.EQ.2.OR.L.EQ.3.AND.I.EQ.5.OR.
     &   L.EQ.3.AND.I.EQ.6.OR.L.EQ.3.AND.I.EQ.8)THEN
         IF(IBLW.EQ.6)GOTO 322
         XVAL1(L)=XVAL1(1)-SPC
         IPEN=-1
         LABEL='Clg Err'
         IF(PLTCTL)LABEL= 'Error'
       ELSEIF(L.EQ.2.AND.I.EQ.3.OR.L.EQ.4.AND.I.EQ.5.OR.
     &   L.EQ.2.AND.I.EQ.7.OR.L.EQ.4.AND.I.EQ.8)THEN
         XVAL1(L)=SPH+(TRH/2)
         IPEN=-104
         LABEL='HtgThUp'
         IF(PLTCTL)LABEL='Tht Hi'
         IF(.NOT.ITHR)NOPLOT=.TRUE.          
       ELSEIF(L.EQ.3.AND.I.EQ.3.OR.L.EQ.5.AND.I.EQ.5.OR.
     &   L.EQ.3.AND.I.EQ.7.OR.L.EQ.5.AND.I.EQ.8)THEN
         XVAL1(L)=SPH-(TRH/2)
         IPEN=-104
         LABEL='HtgThLo'
         IF(PLTCTL)LABEL='Tht Lo'
         IF(.NOT.ITHR)NOPLOT=.TRUE.
       ELSEIF(L.EQ.4.AND.I.EQ.3.OR.L.EQ.6.AND.I.EQ.5.OR.
     &   L.EQ.4.AND.I.EQ.7.OR.L.EQ.6.AND.I.EQ.8)THEN
         IF(IBLW.EQ.6)GOTO 322   
         XVAL1(L)=SPC+(TRC/2)
         IPEN=-104
         LABEL='ClgThUp'
         IF(PLTCTL)LABEL='Tht Hi'
         IF(.NOT.ITHR)NOPLOT=.TRUE.
       ELSEIF(L.EQ.5.AND.I.EQ.3.OR.L.EQ.7.AND.I.EQ.5.OR.
     &   L.EQ.5.AND.I.EQ.7.OR.L.EQ.7.AND.I.EQ.8)THEN
         IF(IBLW.EQ.6)GOTO 322
         XVAL1(L)=SPC-(TRC/2)
         IPEN=-104
         LABEL='ClgThLo'
         IF(PLTCTL)LABEL='Tht Lo'
         IF(.NOT.ITHR)NOPLOT=.TRUE.
       ELSEIF(L.EQ.2.AND.I.EQ.4.OR.L.EQ.4.AND.I.EQ.6.OR.
     &   L.EQ.6.AND.I.EQ.7.OR.L.EQ.8.AND.I.EQ.8)THEN
         XVAL1(L)=SPH
         IPEN=-9
         LABEL='HtgStPt'
         IF(PLTCTL)LABEL='Set Pt'
         IF(.NOT.ISP)NOPLOT=.TRUE.
       ELSEIF(L.EQ.3.AND.I.EQ.4.OR.L.EQ.5.AND.I.EQ.6.OR.
     &   L.EQ.7.AND.I.EQ.7.OR.L.EQ.9.AND.I.EQ.8)THEN
         IF(IBLW.EQ.6)GOTO 322
         XVAL1(L)=SPC
         IPEN=-9
         LABEL='ClgStPt'
         IF(PLTCTL)LABEL='Set Pt'
         IF(.NOT.ISP)NOPLOT=.TRUE.
       ENDIF
       
       Y=XREM1(L)
       XREM1(L)=XVAL1(L)

       IF(ICASCF(IZONE).EQ.0)GOTO 309

C Now plot graph(s),
       IF(L.EQ.1.OR.((L.EQ.2.OR.L.EQ.3).AND.
     &   (I.NE.3.AND.I.NE.4.AND.I.NE.7)))THEN

C A 'point-to-point plot' is used for control point and error graphs.
         call etplot(TIMEL,Y,0,0)
         IF(L.GT.1)THEN
            IF(.NOT.ISP.AND.IBLW.NE.1.AND.IBLW.NE.3)THEN

C no error plot,
              call etplot(TIME,XVAL1(L),0,0)
            ELSE
              call etplot(TIME,XVAL1(L),IPEN,MARK)
            ENDIF
         ELSE
            call etplot(TIME,XVAL1(L),IPEN,MARK)
         ENDIF
       ELSE
C A 'rectangular plot' is used for set-point & throttling range graphs.

         TIMEP=TIMEL+(0.5*NOUT)
         TIMEF=TIME+(0.5*NOUT)
         call etplot(TIMEP,Y,0,0)

C If first pass, then no vertical plot,
         IF(J.EQ.2)THEN
           call etplot(TIMEP,XVAL1(L),0,0)
         ELSE
           IF(NOPLOT)THEN
           call etplot(TIMEP,XVAL1(L),0,0)
           ELSE
           call etplot(TIMEP,XVAL1(L),IPEN,0)
           ENDIF
         ENDIF
         IF(NOPLOT)THEN

C no horizontal plot,
         call etplot(TIMEF,XVAL1(L),0,0)
         ELSE
         call etplot(TIMEF,XVAL1(L),IPEN,MARK)
         ENDIF
       ENDIF
              
       IF(II.EQ.IODF.AND.J.GT.N-NOUT)THEN
         IF(NZ.GT.1)THEN        
           write(BLAB,'(A2,I2,A8)')'zn',IZONE,LABEL
           call etlabel(BLAB,TFINSH,XVAL1(L),3,0)
         ELSE
           call etlabel(LABEL,TFINSH,XVAL1(L),3,0)
         ENDIF
       ENDIF
       GOTO 322

309    call etplot(TIME,XVAL1(L),0,0)
       IF(II.EQ.IODF.AND.J.GT.N-NOUT)THEN
       call usrmsg(' ', ' Uncontrolled zone(s) selected.','W')
       ENDIF
       GOTO 322

32     XREM1(L)=XVAL1(L)
       call etplot(TIME,XVAL1(L),0,0)
       goto 322
321    call etplot(TIME,XVAL1(L),0,9)
       goto 322
322    CONTINUE
      
       RETURN
       END
