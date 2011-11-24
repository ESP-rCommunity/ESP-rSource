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

C This subroutine draws a window with a simplified version of
C the standard  psychrometric chart. Similar to code in
C esrures/psychart.F - revised to for plotting climate data.
      SUBROUTINE PSYCHART
      PARAMETER (MT=24)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      common/grphlim/xmin,xmax,ytmin,ytmax
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/MXMNC/YMAX(6),YMIN(6),TMAX,TMIN,DT
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)
      common/pixhist/ipixhist(8760,2)
     
      CHARACTER*26 ITEM(7)
      CHARACTER puts*12,etext*72,gtext*72
      CHARACTER*30 CLMLOC
      character PERST1*14,PERST2*40,PERST3*40
      logical close,colok,cscaleok,ok,dok
      integer NITMS,INO  ! max items and current menu item

#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer iicol
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iicol
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='PSYCHART'  ! set for cfiles

      PATMOS=1013.25

C Clear remembered point position (so user might click on).
      do 96 i=1,8760
        ipixhist(i,1)=1
        ipixhist(i,2)=1
  96  continue

C Check if can draw in colour and with a range of colours.
C Colours used are nb 15 = grey40, nb 17 =grey60.
      colok=.false.
      if(nzonec.ge.24)colok=.true.
      cscaleok=.false.
      if(ncset.gt.24)cscaleok=.true.

C Scale the axes of the chart (initial values) - look at range
C of ambient temperature from current climate period.
      CALL CLMXMN
      Xmin=YMIN(2)
      Xmax=YMAX(2)
      Ytmin=0.0
      Ytmax=16.0
      enthmax=(enthp2(Xmax,Ytmax/1000.)) 
      enthminx= (enthp2(Xmin,Ytmax/1000.))    
      enthminy= (enthp2(Xmin,Ytmin/1000.)) 
      enthmaxy= (enthp2(Xmin,Ytmax/1000.)) 

C Debug.
C      write(6,*)'x x ',xmin,xmax,enthmax,enthminx,enthminy,enthmaxy

 10   call psychartbackground(Xmin,Xmax,Ytmin,Ytmax,enthmax,
     &  enthminx,enthminy,enthmaxy)

      ACLAT=ABS(CLAT)
      ACLONG=ABS(CLONG)
      IF(CLONG.LT.0.0.AND.CLAT.GE.0.0)then
        WRITE(ETEXT,102)CLMLOC,ACLAT,ACLONG,IYEAR
 102    FORMAT(' Climate analysis:',1X,A30,':',F6.2,'N',F6.2,'W :',I5)
      elseif(CLONG.GE.0.0.AND.CLAT.LT.0.0)then
        WRITE(ETEXT,103)CLMLOC,ACLAT,ACLONG,IYEAR
 103    FORMAT(' Climate analysis:',1X,A30,':',F6.2,'S',F6.2,'E :',I5)
      elseif(CLONG.LT.0.0.AND.CLAT.LT.0.0)then
        WRITE(ETEXT,104)CLMLOC,ACLAT,ACLONG,IYEAR
 104    FORMAT(' Climate analysis:',1X,A30,':',F6.2,'S',F6.2,'W :',I5)
      else
        WRITE(ETEXT,101)CLMLOC,ACLAT,ACLONG,IYEAR
 101    FORMAT(' Climate analysis:',1X,A30,':',F6.2,'N',F6.2,'E :',I5)
      endif
      GTEXT=ETEXT
      iside=1
      isize=0
      ifont=1
      call viewtext(gtext,iside,isize,ifont)

      NTSPH=1
      CALL PERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
      write(etext,'(2A)') ' P',PERST3(2:40)
      GTEXT=ETEXT
      iside=2
      isize=0
      ifont=1
      call viewtext(gtext,iside,isize,ifont)

C Setup menu text.
      ITEM(1) ='1 select plot data     '
      ITEM(2) ='2 change scales        '
      ITEM(3) ='3 re-draw chart        '
      ITEM(4) ='  ____________________ '
      ITEM(5) ='                       '
      ITEM(6) ='? help                 '
      ITEM(7) ='- exit                 '

C Display menu.
 2    NITMS=7
      INO=-2

C Instanciate h() strings for the menu.
      helptopic='psychrometric_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

 5    CALL EMENU('Ambient conditions',ITEM,NITMS,INO)
      IF(INO.EQ.1)THEN

C Ask whether user supplied points or from system components.
        helptopic='psychrometric_useful'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkabcd('Options:',' ','display data',
     &    'colour by temperature','colour by RH','continue',iwhich,2)

        if(iwhich.eq.4)goto 2

C For each day plot data on chart. ihrplt begins at the start day of
C the plotting. rhrange is the RH range, trange is the dbT range.
        ihrplt=(IDS-1)*24
        rhrange=YMAX(6)-YMIN(6)
        trange=YMAX(2)-YMIN(2)
        DO 31 J=IDS,IDF

C Read values of the climate for this day.
          ID=J
          CALL CLMGET(ID)
          KS=1
          KF=24
          IF(J.EQ.IDS)KS=IT1
          IF(J.EQ.IDF)KF=IT2
          DO 41 K=KS,KF

C For each hour...CTMP() is the horizontal azis value, 
C icolindex is scaled from the value of T or RH with the
C range of T or RH via colmult.
            ihrplt=ihrplt+1
            X=CTMP(K)
            call eclose(X,0.000,0.001,close)
            RH=CHUM(K)
            if(iwhich.eq.1)then
              iicol=0
              call winscl('-',iicol)
              icolindex=ncset
            elseif(iwhich.eq.2)then
              colmult=(real(ncset)/trange)
              icolindex=nint((CTMP(K)-YMIN(2))*colmult)
              if(icolindex.le.0) icolindex = 1
              if(icolindex.gt.ncset) icolindex = ncset
              if(cscaleok)then
                iicol=icolindex
                call winscl('c',iicol)
              else
                iicol=0
                call winscl('-',iicol)
              endif
            elseif(iwhich.eq.3)then
              colmult=(real(ncset)/rhrange)
              icolindex=nint((CHUM(K)-YMIN(6))*colmult)
              if(icolindex.le.0) icolindex = 1
              if(icolindex.gt.ncset) icolindex = ncset
              if(cscaleok)then
                iicol=icolindex
                call winscl('c',iicol)
              else
                iicol=0
                call winscl('-',iicol)
              endif
            endif
            GS=1000.*(HUMRT1(CTMP(k),CHUM(k),PATMOS,IOPT))

C If within temperature range, plot it.
            if(X.ge.Xmin.and.X.le.Xmax)then
              call u2pixel(X,GS,ix,iy)

C Debug.
C              write(6,*) ihrplt,colmult,icolindex,ix,iy,CTMP(K),CHUM(K)

              ipixhist(ihrplt,1)=ix
              ipixhist(ihrplt,2)=iy
              call esymbol(ix,iy,32,1)
            endif
 41       continue
 31     continue
        iicol=0
        call winscl('-',iicol)
        call forceflush()
      ELSEIF(INO.EQ.2)THEN

C Change the scales on the axis and re-calculated enthalph for the chart.
        helptopic='psychrometric_min_max'
        call gethelptext(helpinsub,helptopic,nbhelp)
        val=YMIN(2)
        call easkr(val,'Min temp C','for chart ',-10.0,'F',30.,'-',
     &    0.0,'min temp',IER,nbhelp)
        Xmin=val
        YMIN(2)=val
        val=YMAX(2)
        call easkr(val,'Max temp C','for chart ',0.0,'F',60.,'-',
     &    60.0,'max temp',IER,nbhelp)
        Xmax=val
        YMAX(2)=val
        call easkr(val,'Max moisture content g/kg','for chart ',
     &    0.0,'F',30.,'-',30.0,'max mcnt',IER,nbhelp)
        Ytmax=val
        enthmax=(enthp2(Xmax,Ytmax/1000.))
        enthminx= (enthp2(Xmin,Ytmax/1000.))    
        enthminy= (enthp2(Xmin,Ytmin/1000.)) 
        enthmaxy= (enthp2(Xmin,Ytmax/1000.))
        dok=.true.
        helptopic='psychrometric_redraw'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call askok('Redraw the chart to apply changes?',' ',
     &    ok,dok,nbhelp)
        if(ok)then
          goto 10
        else
          GOTO 2
        endif
      ELSEIF(INO.EQ.3)THEN
        call startbuffer()
        enthmax=(enthp2(Xmax,Ytmax/1000.))
        enthminx= (enthp2(Xmin,Ytmax/1000.))    
        enthminy= (enthp2(Xmin,Ytmin/1000.)) 
        enthmaxy= (enthp2(Xmin,Ytmax/1000.)) 
        GOTO 10       
      ELSEIF(INO.EQ.6)THEN
        helptopic='psychrometric_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Psychrometric chart',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.7)THEN

C Reset original screen size values on exit.
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then

C Setup and pass in parameters to win3d.
          iiw1=10; iiw2=20; iiw3=5; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
C          CALL win3d(menuchw,10,22,4,3,igl,igr,igt,igb,igw,igwh)
        elseif(iglib.eq.2)then
          iiw1=9; iiw2=20; iiw3=5; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
C          CALL win3d(menuchw,9,20,5,3,igl,igr,igt,igb,igw,igwh)
        endif
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        call startbuffer()
        RETURN        
      ELSE
        GOTO 5
      ENDIF
      GOTO 2
      
      END

C ***************psychartbackground****************
C psychartbackground draws a background psychrometric chart given the
C following parameters:
C Xmin db T minimum
C Xmax db T maximum
C Ymin moisture g/kg minimum
C Ymax moisture g/kg maximum
C enthmax enthalpy kJ/kg x axis maximum
C enthminx enthalpy x axis minimum
C enthminy enthalpy y axis minimum
C enthmaxy enthalpy y axis maximum
      subroutine psychartbackground(Xmin,Xmax,Ymin,Ymax,enthmax,
     &  enthminx,enthminy,enthmaxy)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      logical close,colok,cscaleok
      CHARACTER puts*16

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer ipos,isize      ! for use with etlabel
      integer iupdown,isym    ! passed to etplot
      integer iicol
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 ipos,isize      ! for use with etlabel
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iicol
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

      PATMOS=1013.25
      IF(MMOD.ne.8) return

C Check if can draw in colour and with a range of colours.
C Colours used are nb 15 = grey40, nb 17 =grey60.
      colok=.false.
      if(nzonec.ge.24)colok=.true.
      cscaleok=.false.
      if(ncset.gt.24)cscaleok=.true.

      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1)then
        LIMTTY=8
        LIMIT =8
        call feedbox(menuchw,2,igfw,igfh)      
        call opengdisp(menuchw,LIMTTY,2,igdw,igdh)

C Setup and pass in parameters to win3d.
        iiw1=10; iiw2=15; iiw3=5; iiw4=3; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
C        call win3d(menuchw,10,15,5,3,igl,igr,igt,igb,igw,igwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        igwid=igw
        igheight=igwh
      elseif(iglib.eq.2)then
        iiw1=10; iiw2=8; iiw3=4; iiw4=3; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
C        CALL win3d(menuchw,10,8,4,3,igl,igr,igt,igb,igw,igwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        igwid=igw
        igheight=igwh
      endif

      call startbuffer()
10    call axiscale(igwid,igheight,Xmin,Xmax,Ymin,Ymax,xsc,ysc,sca,
     &  xadd,yadd)

C Draw the axes of the chart.
      CALL horaxis(Xmin,Xmax,iigl,iigr,iigb,xadd,XSC,0,'db Temp C')
      CALL vrtaxis(0.0,0.01,igl,igb,igt,yadd,YSC,0,0,'Enthalpy kJ/kg')
      CALL vrtaxis(Ymin,Ymax,igr,igb,igt,yadd,YSC,0,1,'Moisture g/kg')

C Store the scaling parameters used in drawing the axes
      CALL linescale(iigl,xadd,XSC,iigb,yadd,YSC)
C Draw the dry-bulb temperature lines
      iupdown=0
      isym=0
      CALL etplot(0.,0.,iupdown,isym)

C Set the intervals for the chart.
      IF(xmax.gt.40.0) then
        TDINT=5.0
      ELSE
        TDINT=2.0
      ENDIF
      
      IF(ymax.gt.40.0) then
        GSINT=5.0
      ELSE
        GSINT=2.0
      ENDIF

      TD=0.
  20  iupdown=1
      isym=0
      CALL etplot(TD,ymax,iupdown,isym)
      TD=TD+TDINT
      iupdown=0
      isym=0
      CALL etplot(TD,0.,iupdown,isym)
      IF(TD.LE.xmax) goto 20

C Draw the moisture content lines
      iupdown=0
      isym=0
      CALL etplot(0.,0.,iupdown,isym)
      GS=0.
  30  iupdown=1
      isym=0
      CALL etplot(xmax,GS,iupdown,isym)
      GS=GS+GSINT
C      if(colok)then
C         iicol=15
C         call winscl('z',iicol)
C      endif
      iupdown=0
      isym=0
      CALL etplot(0.,GS,iupdown,isym)
      IF(GS.LE.ymax) goto 30 
      if(colok)then
        iicol=0
        call winscl('-',iicol)
      endif

C Draw on the RH curves.
      DO 40 IRH=100,10,-10
        RH=FLOAT(IRH)
        IPFLAG=0
        DO 50 ITD=0,int(xmax),1
          TD=FLOAT(ITD)
          GS=1000.*(HUMRT1(TD,RH,PATMOS,IOPT))
          IF(ITD.EQ.0)THEN
            iupdown=0
            isym=0
            CALL etplot(TD,GS,iupdown,isym)
          ELSE
            IF(GS.GT.0..AND.GS.LT.ymax.AND.IPFLAG.EQ.0) THEN
              if(colok)then
                iicol=17
                call winscl('z',iicol)
              endif
              iupdown=-102
              isym=0
              CALL etplot(TD,GS,iupdown,isym)
            ENDIF

C Case where TD<60 and GS>30
            IF(GS.GT.ymax.AND.IPFLAG.EQ.0)THEN
  60          TD=TD-0.05
              GS=1000.*(HUMRT1(TD,RH,PATMOS,IOPT))
              IF(GS.GT.ymax) GOTO 60
              if(colok)then
                iicol=17
                call winscl('z',iicol)
              endif
              iupdown=-102
              isym=0
              CALL etplot(TD,GS,iupdown,isym)
              IPFLAG=1 
            ENDIF                                       
          ENDIF

C Debug.
C          write(6,*) TD,xmax,xmax/2.

          call eclose(TD,real(int(xmax/2.)),0.001,close)
C          IF(TD.EQ.int(xmax/2.)) THEN
          IF(close) THEN
            write(puts,*) INT(RH),'%'
            if(GS.LE.ymax)then
              if(colok)then
                iicol=17
                call winscl('z',iicol)
              endif
              ipos=0
              isize=0
              CALL etlabel(puts,TD,GS,ipos,isize)
            endif
          ENDIF
          if(colok)then
            iicol=0
            call winscl('-',iicol)
          endif
  50    CONTINUE
  40  CONTINUE
                
C Draw on the lines of constant enthalpy.  
      DO 70 IENTH=0,int(enthmax),10
        ENTH=FLOAT(IENTH)
        IPFLAG=0
        DO 80 IGS=0,int(ymax),1
          GS=FLOAT(IGS)
          TD=TDB(ENTH,(GS/1000.))
          IF(IGS.EQ.0.AND.TD.LT.xmax)THEN
            iupdown=0
            isym=0
            CALL etplot(TD,GS,iupdown,isym)
          ELSEIF(IGS.EQ.0.AND.TD.GT.xmax)THEN
  110       GS=GS+0.05
            TD=TDB(ENTH,(GS/1000.))
            IF(TD.GT.xmax) GOTO 110
            if(colok)then
              iicol=24
              call winscl('z',iicol)
            endif
            iupdown=0
            isym=0
            CALL etplot(TD,GS,iupdown,isym)
          ELSE
            IF(TD.GT.0..AND.TD.LT.xmax.AND.IPFLAG.EQ.0) THEN
              if(colok)then
                iicol=24
                call winscl('z',iicol)
              endif
              iupdown=-102
              isym=0
              CALL etplot(TD,GS,iupdown,isym)
            ENDIF 

C Case where TD<60 and GS>30
            IF(TD.GT.xmax.AND.IPFLAG.EQ.0)THEN
  90          GS=GS+0.05
              TD=TDB(ENTH,(GS/1000.))
              IF(TD.GT.xmax) GOTO 90
              if(colok)then
                iicol=24
                call winscl('z',iicol)
              endif
              iupdown=1
              isym=0
              CALL etplot(TD,GS,iupdown,isym) 
            ENDIF
            IF(TD.LT.0..AND.IPFLAG.EQ.0) THEN
 100          GS=GS-0.05
              TD=TDB(ENTH,(GS/1000.))
              IF(TD.LT.0) GOTO 100
              if(colok)then
                iicol=24
                call winscl('z',iicol)
              endif
              iupdown=-102
              isym=0
              CALL etplot(TD,GS,iupdown,isym)
              IPFLAG=1
            ENDIF                                       
          ENDIF
          if(colok)then
            iicol=0
            call winscl('-',iicol)
          endif
 
 80     CONTINUE
  
 70   CONTINUE

C Draw on the enthalpy scales
      if(colok)then
        iicol=0
        call winscl('-',iicol)
      endif
      DO 200 IENTH=0,int(ENTHMAXY),10
        ENTH=FLOAT(IENTH)
        IF (ENTH.GT.ENTHMINY.AND.ENTH.LE.ENTHMAXY) THEN
            TD=Xmin
            GS=((ENTH-TD)/(1.82*TD+2500.))*1000.
            write(puts,*) IENTH
            ipos=0
            isize=0
            CALL etlabel(puts,Xmin-Xmax/XSC,GS,ipos,isize)
        ENDIF
  200 CONTINUE

      DO 210 IENTH=0,int(ENTHMAX),10
        ENTH=FLOAT(IENTH)
        IF (ENTH.GT.ENTHMINX.AND.ENTH.LE.ENTHMAX) THEN
          TD=TDB(ENTH,(YMAX)/1000.)
          write(puts,*) IENTH
          ipos=0
          isize=0
          CALL etlabel(puts,TD,YMAX+YMAX/YSC,ipos,isize)
        ENDIF
  210 CONTINUE
      call forceflush()

      return
      end
