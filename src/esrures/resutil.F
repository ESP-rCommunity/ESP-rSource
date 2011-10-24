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

C Utility routines for results analysis.
C Contains:
C  MOSTYLE - Change the output style.
C  AXISINT - initialise axis data.
C  AXISPOS - set positions of axes for all graph drawing routines.
C  AXISDRW - draw and label axis.

C ************ MOSTYLE ************
C MOSTYLE - set positions of axes for all graph drawing routines.
C MENUTYPE 1: not used
C MENUTYPE 2: Frequency histogram
C MENUTYPE 3: Time series plot
C MENUTYPE 4: Performance metrics
C MENUTYPE 5: Summary statistics
C MENUTYPE 6: Sensitivity
C MENUTYPE 7: Hours above query point
C MENUTYPE 8: Hours below query point
C MENUTYPE 9: Psychrometric chart

      SUBROUTINE MOSTYLE

      common/getmenu/menutype,igetind(65),igetflux(65)

      CHARACTER ITEM(15)*21
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MOSTYLE'  ! set for subroutine

C Create menu.
      ITEM(1) ='   Graphs:           '
      ITEM(2) ='a Time series plot   '
      ITEM(3) ='p Psychrometric chart'
      ITEM(4) ='  ------------------ '
      ITEM(5) ='   Statistics:       '
      ITEM(6) ='b Summary statistics '
      ITEM(7) ='c Hours > query point'
      ITEM(8) ='d Hours < query point'
      ITEM(9) ='e Frequency histogram'
      ITEM(10) ='  ------------------ '
      ITEM(11)='   Tabular lists:    '
      ITEM(12)='f Performance metrics'
      ITEM(13)='  ------------------ '
      ITEM(14)='? Help               '
      ITEM(15)='- Exit               '

      NITMS=15
      INO=-2

C Instanciate help strings for the menu.
      helptopic='res_style_selection_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

 1    CALL EMENU('Output styles',ITEM,NITMS,INO)

C Set MENUTYPE depending on choice made.
      if (INO.eq.2) then
        MENUTYPE=3
      elseif (INO.eq.3) then
        MENUTYPE=9
      elseif (INO.eq.6) then
        MENUTYPE=5
      elseif (INO.eq.7) then
        MENUTYPE=7
      elseif (INO.eq.8) then
        MENUTYPE=8
      elseif (INO.eq.9) then
        MENUTYPE=2
      elseif (INO.eq.12) then
        MENUTYPE=4
      elseif (INO.eq.(NITMS-1)) then
        helptopic='res_style_selection_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Output styles',nbhelp,'-',0,0,IER)
        goto 1
      elseif (INO.eq.NITMS) then
        return
      else
        goto 1
      endif

      RETURN
      END

C ************ AXISINT ************
C AXISINT - set positions of axes for all graph drawing routines.
C  IGRTP is graph type (defines axis labels).
C    1=std building information.
C    2=mass flow network.
C    3=plant systems
      SUBROUTINE AXISINT(IGRTP,ALABEL)
      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN,horaxisdiv
      COMMON/IGETFLG/IOCUPF,ialstused,IROC

      integer YAXSET,ADDLIN,horaxisdiv

      character ALABEL(5)*12

C Reset axis scaling.
      do 4 I=1,5
        if (YAXSET(I).lt.2.or.YAXSET(I).EQ.5) then
          YMAX(I)=-1.E+10
          YMIN(I)=1.E+10
        endif
 4    continue

C Define labels.
C IROC, if rate of change of variable alter the axis label.
      if (IGRTP.eq.1) then

C Standard building labels.
        if (IROC.eq.0) then
          ALABEL(1)='Temp: DegC'
          ALABEL(2)='Load: W'
          ALABEL(3)='Int: W/m^2'
          ALABEL(4)='Misc.'
        elseif (IROC.eq.1) then
          ALABEL(1)='T: DegC/hr'
          ALABEL(2)='Load: W/hr'
          ALABEL(3)='Int:W/m^2/hr'
          ALABEL(4)='Misc./hr'
        endif
      elseif (IGRTP.eq.2) then

C Mass flow labels.
        if (IROC.eq.0) then
          ALABEL(1)='Temp: DegC'
          ALABEL(2)='Pres: Pa'
          ALABEL(3)='Flow rate'
          ALABEL(4)='Misc.'
          ALABEL(5)='Conc: g/kg'
        elseif (IROC.eq.1) then
          ALABEL(1)='T: DegC/hr'
          ALABEL(2)='P: Pa/hr'
          ALABEL(3)='Flw rate/hr'
          ALABEL(4)='Misc./hr'
          ALABEL(5)='Conc e-3/hr'
        endif
      elseif (IGRTP.eq.3) then
        ALABEL(1)='Temp: DegC'
        ALABEL(2)='kg/s'
        ALABEL(3)='%'
        ALABEL(4)='KJ/Kg'
        ALABEL(5)='(-)'
      endif

      RETURN
      END

C ************ AXISPOS ************
C AXISPOS - set positions of axes for all graph drawing routines.

      SUBROUTINE AXISPOS

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN,horaxisdiv
      COMMON/GRAF2/YSC(5),Yadd(5),TSC,Xadd,IGX(5),ILR(5),DT

      integer YAXSET,ADDLIN,horaxisdiv

C Set default positions.
      igx(1)=igl
      igx(2)=igr+140
      igx(3)=igr+80
      igx(4)=2*(igl+igr)/3
      igx(5)=(igl+igr)/3
      ILR(1)=0
      ILR(2)=1
      ILR(3)=1
      ILR(4)=1
      ILR(5)=1

C Optimise axis positions.
      NAX=0
      do 10 I=1,5
        if (YAXSET(I).gt.0) NAX=NAX+1
 10   continue

C Position each axis in turn.
      IAXOK=0
      do 20 I=1,5
        if (YAXSET(I).gt.0) then
          IAXOK=IAXOK+1
          if (IAXOK.eq.1) then

C First axis to be placed therfore at left hand side.
            ILR(I)=0
            IGX(I)=igl
          else

C Place remaining axis to right hand side.
            ILR(I)=1
            if (NAX.eq.2) then
              IGX(I)=igr+120
            elseif (NAX.eq.3) then
              if (IAXOK.eq.2) then
                IGX(I)=igr+145
              else
                IGX(I)=igr+90
              endif
            else
              if (IAXOK.eq.2) then
                IGX(I)=igr+140
              elseif (IAXOK.eq.3) then
                IGX(I)=igr+80
              else
                IGX(I)=(igl+igr)/2
              endif
            endif
          endif
        endif
 20   continue

      RETURN
      END

C ************ AXISDRW ************
C AXISDRW - draw and label axes.

      SUBROUTINE AXISDRW(TSTART,TFINISH,ALABEL,act)

      COMMON/PPERS/IPSD1,IPSM1,IPSD2,IPSM2,IPSDS,IPSDF,NPTS,ipsav
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN,horaxisdiv
      COMMON/GRAF2/YSC(5),Yadd(5),TSC,Xadd,IGX(5),ILR(5),DT
      real TST,TFI,TSCR  ! for hourly version

      character ALABEL(5)*12,act*1

      integer YAXSET,ADDLIN,horaxisdiv
#ifdef OSI
      integer igwid,igheight,iid1,iid2  ! for use with axiscale
#else
      integer*8 igwid,igheight,iid1,iid2  ! for use with axiscale
#endif


C Debug.
C      write(6,*) 'AXISDRW',(YAXSET(I),I=1,5)

C Draw each axis in turn.
      do 10 I=1,5
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
C First set IADJ which rounds down always.
            if (YMIN(I).lt.0.0) then
              IADJ=-1
            else
              IADJ=0
            endif

C Use log(10) to get scaling factor.
            if (abs(YMIN(I)).gt.0.000000001) then
              ISC=int(log10(abs(YMIN(I))))
              if (ISC.lt.0) ISC=ISC-1
              FAC=10.0**ISC

C Debug.
C              write(6,*) 'Current ',YMIN(I),' fac ',FAC, ' IADJ ',IADJ

              YMIN(I)=FAC*float(int(YMIN(I)/FAC)+IADJ)

C Debug.
C              write(6,*) 'New    ',YMIN(I)

            endif
          endif

C Set-up frequency of labels (DT), number of decimal places (NDEC) and 
C draw vertical axis.
          call dinterval(TSTART,TFINISH,DT,NDEC,1)
          igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,TSTART,TFINISH,YMIN(I),YMAX(I),
     &                             TSC,YSC(I),sca,Xadd,Yadd(I))
          if (ILR(I).eq.0) then
            call vrtaxis(YMIN(I),YMAX(I),igx(I),igb,igt,Yadd(I),
     &                             YSC(I),0,ilr(I),ALABEL(I))
          else
            call vrtaxis(YMIN(I),YMAX(I),igx(I),igb,igt,Yadd(I),
     &                             YSC(I),0,ilr(I),'  ')
            iid1=igx(I); iid2=igt-5;
            call wstxpt(iid1,iid2,ALABEL(I))
          endif
        endif
 10   continue

C Draw horizontal (time) axis.
      if(act.eq.'P')then
        call horaxis(TSTART,TFINISH,igl,igr,igb,Xadd,TSC,1,'Timesteps')
      else

C- Hardwired to display hours instead of timesteps on x-axis
C- To revert back to original remove npts from following statement
        if(npts.eq.0) npts=1
        TST=TSTART/real(npts)
        TFI=TFINISH/real(npts)
	TSCR=TSC*real(npts)
C        call horaxis(TST,TFI,igl,igr,igb,Xadd,TSCR,1,'Time Hrs')
        if(horaxisdiv.eq.0.or.horaxisdiv.eq.1)then

C Default is the same as AXISDRW always provided.
          call horaxis(TST,TFI,igl,igr,igb,Xadd,TSCR,1,'Time Hrs')
        elseif(horaxisdiv.eq.2)then
          call horaxis(TST,TFI,igl,igr,igb,Xadd,TSCR,1,'Time Hrs')
        elseif(horaxisdiv.eq.3)then
          call horaxishdw(TST,TFI,igl,igr,igb,Xadd,TSCR,1,
     &      2,24,iods,'Time (days)')
        elseif(horaxisdiv.eq.4)then
          call horaxishdw(TST,TFI,igl,igr,igb,Xadd,TSCR,1,
     &      3,168,iods,'Time (weeks)')
        endif
      endif
      RETURN
      END
