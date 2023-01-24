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

C This file contains the following subroutines.
C  MOGRAF
C  MOWLIN
C  MOGHED
C  MOCOPT
C  MOCDAT
C  MOCGDW

C ******************** MOGRAF ********************
C The main controlling routine for the graphical
C output facilities. This output mode allows the various
C causal relationships to be visually inspected.

      SUBROUTINE MOGRAF
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "help.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      integer ncomp,ncon
      common/C1/NCOMP,NCON

      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG 
      common/getmenu/menutype,igetind(65),igetflux(65)

      CHARACTER*23 ITEM(13)
      character outs*124,LAPROB*72
      integer NITMS,INO ! max items and current menu item 
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

      helpinsub='tgraph'  ! set for subroutine

C Setup menu text.
      ITEM(1) ='2 result set          '
      ITEM(2) ='3 output period       '
      ITEM(3) ='4 zones               '
      ITEM(4) ='  ------------------- '
      ITEM(5) ='a parameter plot      '
      ITEM(6) ='b intra-constr. temp. '
      ITEM(7) ='c 3D profile          '
      ITEM(8) ='d frequency histogram '
      ITEM(9) ='e variable v. variable'
      ITEM(10)='f network flows       '
      ITEM(11)='  ------------------- '
      ITEM(12)='? help                '
      ITEM(13)='- exit menu           '

C Graphical display possible.
    2 NITMS=13
      if(MMOD.eq.8)then
        INO=-1
      else
        INO=-2
      endif

C Instantiate help strings for the menu.
      helptopic='res_graphs_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

    5 CALL EMENU('Graphing options',ITEM,NITMS,INO)

C Test for illegal menu pick: because ISAVE = 1 or 2.
      IF(ISAVE.EQ.1.AND.INO.EQ.6)THEN
        CALL edisp(iuout,' ')
        CALL edisp(iuout,'Command not available since the save level')
        WRITE(outs,'(a,i2,a)')'associated with the result set is',
     &                         ISAVE,'!'
        goto 2
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
         call edisp(IUOUT,' ')
         call edisp(iuout,'No network flow associated with model!')
         goto 2
        endif
        if(MMOD.LT.8.and.MMOD.ne.-6)then
          CALL USRMSG('Switch to graphic mode for this facility.',
     &      ' ','W')
          goto 2
        endif
        IRY=IYEAR

C If iairn=3 we could display the flow network in 3D at this point.
        if(iairn.eq.3)then

C Initialise coordinates for eye point, view point and angle of view.
          EYEM(1)=-100.; EYEM(2)=-100.; EYEM(3)=100.
          VIEWM(1)=10.; VIEWM(2)=10.; VIEWM(3)=10.
          ANG=40.

C General image option flags.
          ITDSP=0; ITBND=1; ITEPT=0
          ITZNM=0; ITSNM=1; ITVNO=1
          ITORG=0; ITSNR=1; ITOBS=0
          ITGRD=0; GRDIS=0.0
          ITPPSW=0

C Clear current viewing box and re-establish image bounds.
          CALL startbuffer

C Setup and pass in parameters to win3d.
          iiw1=4; iiw2=4; iiw3=3; iiw4=4; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
          igwid=igw; igheight=igwh

C Get bounds for image.
          nzg=NCOMP
          do 29 iz=1,nzg
            nznog(iz)=iz
  29      continue
          call bndobj(0,IER)

          HANG=ANG/2.0; HITH=1.0; YON=1300.0
          CALL LENS(IER)

C Determine scaling ratios for the drawing and axis.
C Determine the 2D coords of the site extremes.
          CALL SITE2D(SXMX,SXMN,SYMX,SYMN,ier)
          CALL axiscale(igwid,igheight,SXMN,SXMX,SYMN,SYMX,xsc,ysc,sca,
     &      Xadd,Yadd)
          call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
          call INICLP(ier)

C Read 3D flow network, report summary of contents and draw the network.
          IUM=IFIL+6
          CALL ERPFREE(IUM,ISTAT)
          CALL MFCDAT
          CALL EMF3DREAD(IUM,'S',IER)
          call edisp(iuout,' ')
          CALL MFLIST(iuout,'s')
          if(NNOD.gt.50.or.NCMP.gt.50)then
            call MFWFDRAW('r',0,0)  ! Draw flow network without names if complex
          else
            call MFWFDRAW('r',1,1)  ! Draw flow network with names
          endif 
          CALL ERPFREE(IUM,ISTAT)
          call pauses(1)
        endif

C Set standard menu type and then present the selection facaility.
        MENUTYPE=3
        call MFOUTP('-')
      ELSEIF(INO.EQ.12)THEN
        helptopic='res_graphs_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('graphing',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.13)THEN
        RETURN
      ENDIF
      goto 5

      END

C ******************** MOWLIN ********************
C The main controlling routine for line graph
C output. if act='p' then iget parameters are already set.

      SUBROUTINE MOWLIN(act)
#include "building.h"

      COMMON/OUTPCH/ICOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GRAF1/YMAX(6),YMIN(6),YAXSET(6),ADDLIN,horaxisdiv

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)
      COMMON/ZONPIK/NZ,NZNO(MCOM)

      character SLABEL*32,GLABEL*24,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      common/grextras/graftitle,grlbl(10),ngrlbl,lblpx(10),lblpy(10)
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      logical HAVEIT            ! to signal data access via GETSTATS

      integer ireportunit       ! zero is default one is W two is kW three Joules
      common/repunit/ireportunit
      real Yold,TIMEold         ! type for passing to C
      dimension Yold(MZS), TIMEold(MZS)
      dimension YSC(5), Yadd(5), igx(5), ilr(5)
      integer lbthusfar,lbdelta ! how many labels have been drawn
      integer lbypos(MS)        ! position of y of labels
      logical lbclose           ! are we going to obscure a label
      character prompt*124
      character ALABEL(6)*12,BLABEL(6)*12  ! 1st and 2nd graph labels.
      character t20*24,t36*36,act*1
      character graftitle*64,grlbl*24,t24*24

      integer YAXSET,ADDLIN,horaxisdiv
      integer iglib       ! if iglib is 2 then using GTK library otherwise X11
      integer overalldays ! number of days to graph (to set axis)
      integer oneonright  ! if only one axis on right.
      integer twoonrighta ! nearest if two axis on right
      integer twoonrightb ! farthest if two axis on right
      integer threerighta ! nearest if miscel plus two axis on right
      integer threerightb ! farthest if miscel plus two axis on right
#ifdef OSI
      integer igx,iigx  ! see array above
      integer igwid,igheight  ! for use with axiscale
      integer iupdown,isym,iid1,iid2,iid3   ! passed to etplot
      integer iicol,ibsize,ipixw,ipixh
      integer iigl,iigr,iigt,iigb,iigw,iigwh
#else
      integer*8 igx,iigx  ! see array above
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iupdown,isym,iid1,iid2,iid3   ! passed to etplot
      integer*8 iicol,ibsize,ipixw,ipixh
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
#endif

      logical kwatts,colok

C Define prompt
      prompt='  '

C Check if can draw in colour.
      colok=.false.
      if(nzonec.ge.NCOMP)colok=.true.

C Cast values for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
      
C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        if(NGET.ge.1)then
          write(prompt,'(a,a)') SLABEL(1)(1:LNSLABEL(1)),' >> file.'
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

      lbthusfar=0   ! reset drawn labels
      lbdelta=0
      do loop=1,MS
        lbypos(loop)=0
      enddo

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

C Check if heating/cooling plant information requested. If user has
C asked for normal units (kW) then set kwatts TRUE.
      do 3 I=1,NGET
        if (IGETNO(I,8).eq.2) then
          if (IGETNO(I,1).eq.8.or.IGETNO(I,1).eq.9.or.
     &        IGETNO(I,1).eq.10.or.IGETNO(I,1).eq.30.or.
     &        IGETNO(I,1).eq.31.or.IGETNO(I,1).eq.32.or.
     &        IGETNO(I,1).eq.33) then
            if (YAXSET(2).lt.2) YAXSET(2)=0
            if(ireportunit.eq.0.or.ireportunit.eq.2)then
              kwatts=.TRUE.
            else
              kwatts=.FALSE.
            endif
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
C      call usrmsg(prompt,'Scanning data for range of values...','-')

C First recover bounds of selected data. MMACAL returns kW for DMAX and
C DMIN for the specific cases of IGETNO(IX,1) = 10 or 32.
      do 40 IX=1,NGET
        if (YAXSET(IGETNO(IX,8)).lt.2) then
          call GETSTATS(IX,HAVEIT,DMAX,DMIN,DAVE,DTOT,TXMAX,TXMIN)
          if(HAVEIT)then
          else
            call MMACAL(IX,DMAX,DMIN,DAVE,DTOT,TXMAX,TXMIN)
          endif
          YAXSET(IGETNO(IX,8))=1

C Check for plant flux in kW, if so convert fluxes from W to kW
C based on value of ireportunit.
          if (kwatts.and.IGETNO(IX,8).eq.2) then
            if (IGETNO(IX,1).ne.8.and.IGETNO(IX,1).ne.9.and.
     &          IGETNO(IX,1).ne.10.and.IGETNO(IX,1).ne.30.and.
     &          IGETNO(IX,1).ne.31.and.IGETNO(IX,1).ne.32.and.
     &          IGETNO(IX,1).ne.33) then
              if(ireportunit.eq.0.or.ireportunit.eq.2)then
                DMAX=DMAX/1000.
                DMIN=DMIN/1000.
              endif
            endif
          elseif (.NOT.kwatts.and.IGETNO(IX,8).eq.2) then
            if (IGETNO(IX,1).eq.10.or.IGETNO(IX,1).eq.32) then
              DMAX=DMAX*1000.
              DMIN=DMIN*1000.
            endif
          endif
          YMAX(IGETNO(IX,8))=MAX(DMAX,YMAX(IGETNO(IX,8)))
          YMIN(IGETNO(IX,8))=MIN(DMIN,YMIN(IGETNO(IX,8)))
        endif
 40   continue
C      call usrmsg(prompt,'Scanning data for range of values...done.',
C     &  'P')
      call usrmsg('  ','  ','-')

C Draw axis.where:
C  igx(1) is the left vertical axis.
C  igx(2) is the `Load W` vertical axis.
C  igx(3) is the `Int W/m^2' vertical axis
C  igx(4) is the `Miscel vertical axis if placed in the centre.
C  igx(5) does not have a label and is not part of the do 100 loop.
C Set local values of vertical axis offset (in pixels) from right
C end of horiz (in pixels) axis to make up for the fact that glr
C is being shifted to the left as more axis are added. If iiw2 values
C near line ~960 change then the values below might need tweaking.
      oneonright=165
      twoonrighta=165
      twoonrightb=220
      threerighta=165
      threerightb=220

C moghed uses similar YAXSET logic to know how many right axis to draw.
C It works with current YASSET values.
      call MOGHED
      igx(1)=igl
      igx(2)=igr+threerightb
      igx(3)=igr+threerighta
      igx(4)=(igl+igr)/3
      igx(5)=2*(igl+igr)/3

C Optimise axis positions.
      if (YAXSET(2).gt.0) then
        if (YAXSET(3).gt.0) then

C Axis 2,3 active, 4 either active or not still in centre.
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

      if(IROC.eq.0)then
        ALABEL(1)='Temperature'
        BLABEL(1)='(C)'

C Could be KW for next units depends on scaling.
        ALABEL(2)='Load'
        BLABEL(2)='(W)'
        if (kwatts) BLABEL(2)='(KW)'
        ALABEL(3)='Intensity'
        BLABEL(3)='(W/m^2)'
        ALABEL(4)='Misc.'
        BLABEL(4)='(various)'
      elseif(IROC.eq.1)then

C IROC, if rate of change of variable alter the axis label.
        ALABEL(1)='Temperature'
        BLABEL(1)='(C/hr)'
        ALABEL(2)='Load'
        BLABEL(2)='(W/hr)'
        if (kwatts) BLABEL(2)='(KW/hr)'
        ALABEL(3)='Intensity'
        BLABEL(3)='(W/m^2/h)'
        ALABEL(4)='Misc.'
        BLABEL(4)='(various/hr)'
      endif

C Set which topics are on left or right.
      ILR(1)=0
      ILR(2)=1
      ILR(3)=1
      ILR(4)=1
      
C Cast values for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
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
          if(colok)then
            iicol=0; ibsize=0
            call winscl('-',iicol)  ! ensure axis drawn in black
          endif
          call dintervalf(TSTART-1.0,TFINSH,DT,NDEC,1)
          igwid=igw
          igheight=igwh
          iigx=int(igx(I))
          call axiscale(igwid,igheight,TSTART-1.0,TFINSH,YMIN(I),
     &                  YMAX(I),TSC,YSC(I),sca,Xadd,Yadd(I))

C Establish the width and height of the label.
          call textpixwidth(ALABEL(i),ipixw,ipixh)  ! get its width
          if (ILR(I).eq.0) then
            call dintervalf(YMIN(I),YMAX(I),ddy1,ny,0)
            call vrtaxisdd(YMIN(I),YMAX(I),iigx,iigb,iigt,Yadd(I),
     &        YSC(I),0,ddy1,ny,ilr(I),'  ')
            if(ilr(I).eq.0)then
              iid1=15; iid2=igt-(ipixh-2); iid3=igt-1    ! If on left do left justified.
            else
              iid1=igx(I); iid2=igt-(ipixh-2); iid3=igt-1
            endif
            call textsizeatxy(iid1,iid2,ALABEL(I),ibsize,'-',iicol)
            call textsizeatxy(iid1,iid3,BLABEL(I),ibsize,'-',iicol)
          else
            call dintervalf(YMIN(I),YMAX(I),ddy1,ny,0)
            call vrtaxisdd(YMIN(I),YMAX(I),iigx,iigb,iigt,Yadd(I),
     &        YSC(I),0,ddy1,ny,ilr(I),'  ')
            iid1=igx(I); iid2=igt-(ipixh-2); iid3=igt-1
            call textsizeatxy(iid1,iid2,ALABEL(I),ibsize,'-',iicol)
            call textsizeatxy(iid1,iid3,BLABEL(I),ibsize,'-',iicol)
          endif
        endif
 100  continue

C Draw horizontal (time) axis. But if there is no right axis then
C there should be logic to maximize the width of the plot.  << to be done >>
C Determine how many days to graph. If less than 2 months default to hourly,
C if 2-4 months default to days, if longer than 4 months use weeks as the
C default.
      if(horaxisdiv.eq.0.or.horaxisdiv.eq.1)then
        overalldays=IODF-IODS
        if(overalldays.le.60)then
          call dintervalf(TSTART-1.0,TFINSH,ddy1,ny,1)
          call horaxisdd(TSTART-1.0,TFINSH,iigl,iigr,iigb,Xadd,TSC,1,
     &      ddy1,ny,'Time Hrs')
        elseif(overalldays.gt.60.and.overalldays.le.120)then
          call dintervalf(TSTART-1.0,TFINSH,ddy1,ny,1)
          call horaxishdwdd(TSTART-1.0,TFINSH,iigl,iigr,iigb,Xadd,TSC,1,
     &      ddy1,ny,2,24,iods,'Time (days)')
        else
          call dintervalf(TSTART-1.0,TFINSH,ddy1,ny,1)
          call horaxishdwdd(TSTART-1.0,TFINSH,iigl,iigr,iigb,Xadd,TSC,1,
     &      ddy1,ny,3,168,iods,'Time (weeks)')
        endif
      elseif(horaxisdiv.eq.2)then
        call dintervalf(TSTART-1.0,TFINSH,ddy1,ny,1)
        call horaxisdd(TSTART-1.0,TFINSH,iigl,iigr,iigb,Xadd,TSC,1,
     &    ddy1,ny,'Time Hrs')
      elseif(horaxisdiv.eq.3)then
        call dintervalf(TSTART-1.0,TFINSH,ddy1,ny,1)
        call horaxishdwdd(TSTART-1.0,TFINSH,iigl,iigr,iigb,Xadd,TSC,1,
     &    ddy1,ny,2,24,iods,'Time (days)')
      elseif(horaxisdiv.eq.4)then
        call dintervalf(TSTART-1.0,TFINSH,ddy1,ny,1)
        call horaxishdwdd(TSTART-1.0,TFINSH,iigl,iigr,iigb,Xadd,TSC,1,
     &    ddy1,ny,3,168,iods,'Time (weeks)')
      endif
      
C Recover and plot data.
 99   do 10 IDAY=IODS,IODF
        call GOGET(IDAY)

C Add values in VAL2 to there correct bins. Loop through selected zones.
        do 400 IX=1,NGET

C Reset scaling factor for appropriate axis.
          Iax=IGETNO(IX,8)
          call linescale(iigl,Xadd,TSC,iigb,Yadd(Iax),YSC(Iax))

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

C The data is a load and not related to plant. By default these loads 
C are in W but plant is being displayed so convert to kW if directed
C to by ireportunit.
                if(ireportunit.eq.0.or.ireportunit.eq.2)then
                  Yold(IX)=Yold(IX)/1000.
                endif
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

C The data is a load and not related to plant. By default these loads 
C are in W but plant is being displayed so convert to kW if directed
C to by ireportunit.
                if(ireportunit.eq.0.or.ireportunit.eq.2)then
                  VAL2(IX,J)=VAL2(IX,J)/1000.
                endif
              endif
            endif
            if (ioldpos.eq.1)then
              iupdown=0
              isym=0
              call etplot(TIMEold(IX),Yold(IX),iupdown,isym)
            endif
            ioldpos=0
            IPEN=IGETNO(IX,6)
            MARK=0
            RMK=AMOD((TIME-(0.5*float(NOUT)/float(NTS))),DT)
            if (RMK.lt.0.001) MARK=IGETNO(IX,7)
            TIMEold(IX)=TIME
            ytmp=Yold(IX)
            Yold(IX)=VAL2(IX,J)

C In case of local comfort do not plot data outside bounds of algorithms
C This is passed as a PPD greater than 100%.
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
C zone we are dealing with. If zone not set then use black.
            if(colok)then
              if(IGETNO(IX,2).gt.0)then
                if(NZ.eq.1)then   ! only 1 zone so colour by surface
                  iicol=IGETNO(IX,3)
                  call winscl('z',iicol)
                else              ! multiple zones so colour by zone
                  iicol=IGETNO(IX,2)
                  call winscl('z',iicol)
                endif
              else
                iicol=0
                call winscl('-',iicol)
              endif
            endif

C Plot the current value - this has already been copied to Yold and
C then reset to foreground colour.
            iupdown=IPEN
            isym=MARK
            call etplot(TIME,Yold(IX),iupdown,isym)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
            call forceflush()
 421      CONTINUE
 400    CONTINUE

C Reset add profile.
        ADDLIN=0
 10   continue

C Label lines, matching zone line colour if used.
      do 200 I=1,NGET
        Iax=IGETNO(I,8)
        call linescale(iigl,Xadd,TSC,iigb,Yadd(Iax),YSC(Iax))
        t20=GLABEL(I)
        t36=TABLABEL(I)
        if(colok)then
          if(IGETNO(I,2).gt.0)then
            if(NZ.eq.1)then   ! only 1 zone so colour by surface
              iicol=IGETNO(I,3)
              call winscl('z',iicol)
            else              ! multiple zones so colour by zone
              iicol=IGETNO(I,2)
              call winscl('z',iicol)
            endif
          endif
        endif
        call u2pixel(TIMEold(I),Yold(I),iid1,iid2)

C Is this close to a previous label position? Try up to six
C variant positions to see if an un-obstructed Y can be found.
        if(lbthusfar.eq.0)then
          lbthusfar=1
          lbypos(lbthusfar)=int(iid2)
        else
          lbclose=.false.
          do loop=1,lbthusfar
            lbdelta=iabs(lbypos(loop)-int(iid2))
            if(lbdelta.lt.6) lbclose=.true.
          enddo
          if(lbclose)then        ! will obstruct try 7 pixels higher
            lbclose=.false.
            do loop=1,lbthusfar
              lbdelta=iabs(lbypos(loop)-(int(iid2)-7))
              if(lbdelta.lt.6) lbclose=.true.
            enddo
            if(.NOT.lbclose)then ! that will work so reset iid2
              iid2=iid2-6
              lbthusfar=lbthusfar+1
              lbypos(lbthusfar)=int(iid2)
            else                 ! will obstruct try 14 pixels higher
              lbclose=.false.
              do loop=1,lbthusfar
                lbdelta=iabs(lbypos(loop)-(int(iid2)-14))
                if(lbdelta.lt.6) lbclose=.true.
              enddo
              if(.NOT.lbclose)then ! that will work so reset iid2
                iid2=iid2-14
                lbthusfar=lbthusfar+1
                lbypos(lbthusfar)=int(iid2)
              else                 ! will obstruct try 21 pixels higher
                lbclose=.false.
                do loop=1,lbthusfar
                  lbdelta=iabs(lbypos(loop)-(int(iid2)-21))
                  if(lbdelta.lt.6) lbclose=.true.
                enddo
                if(.NOT.lbclose)then ! that will work so reset iid2
                  iid2=iid2-21
                  lbthusfar=lbthusfar+1
                  lbypos(lbthusfar)=int(iid2)
                else                 ! will obstruct try 28 pixels higher
                  lbclose=.false.
                  do loop=1,lbthusfar
                    lbdelta=iabs(lbypos(loop)-(int(iid2)-28))
                    if(lbdelta.lt.6) lbclose=.true.
                  enddo
                  if(.NOT.lbclose)then ! that will work so reset iid2
                    iid2=iid2-28
                    lbthusfar=lbthusfar+1
                    lbypos(lbthusfar)=int(iid2)
                  else               ! will obstruct try 7 pixels lower
                    lbclose=.false.
                    do loop=1,lbthusfar
                      lbdelta=iabs(lbypos(loop)-(int(iid2)+7))
                      if(lbdelta.lt.6) lbclose=.true.
                    enddo
                    if(.NOT.lbclose)then ! that will work so reset iid2
                      iid2=iid2+6
                      lbthusfar=lbthusfar+1
                      lbypos(lbthusfar)=int(iid2)
                    else                 ! will obstruct try 14 pixels lower
                      lbclose=.false.
                      do loop=1,lbthusfar
                        lbdelta=iabs(lbypos(loop)-(int(iid2)+14))
                        if(lbdelta.lt.6) lbclose=.true.
                      enddo
                      if(.NOT.lbclose)then ! that will work so reset iid2
                        iid2=iid2+14
                        lbthusfar=lbthusfar+1
                        lbypos(lbthusfar)=int(iid2)
                      else                 ! will obstruct try 21 pixels lower 
                        lbclose=.false.
                        do loop=1,lbthusfar
                          lbdelta=iabs(lbypos(loop)-(int(iid2)+21))
                          if(lbdelta.lt.6) lbclose=.true.
                        enddo
                        if(.NOT.lbclose)then ! that will work so reset iid2
                          iid2=iid2+21
                          lbthusfar=lbthusfar+1
                          lbypos(lbthusfar)=int(iid2)
                        else                 ! give up
                          lbthusfar=lbthusfar+1
                          lbypos(lbthusfar)=int(iid2)
                          continue  ! give up shifting
                        endif
                      endif
                    endif
                  endif
                endif
              endif
            endif
          else                   ! no obstruction remember and proceed
            lbthusfar=lbthusfar+1
            lbypos(lbthusfar)=int(iid2)
          endif
        endif
        ibsize=0
        call textsizeatxy(iid1,iid2,t36,ibsize,'z',iicol)
        if(colok)then
          iicol=0
          call winscl('-',iicol)
        endif
        call forceflush()
 200  continue
      call usrmsg(' ',' ','-')  ! refresh dialog 

C If there are optional user defined labels draw them.
      if(ngrlbl.gt.0)then
        do 201 ijj=1,ngrlbl
          t24=grlbl(ijj)
          iid1=lblpx(ijj); iid2=lblpy(ijj);
          iicol=0; ibsize=0
          call textsizeatxy(iid1,iid2,t24,ibsize,'-',iicol)
 201    continue
      endif
      goto 1

      END

C ******************** MOGHED ********************
C Outputs the appropriate header information corresponding
C to graphical result display.

      SUBROUTINE MOGHED
#include "building.h"
#include "model.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/SETNAM/RSNAME(MNRS)
      COMMON/SIMPKA/NSIM
      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      common/grextras/graftitle,grlbl(10),ngrlbl,lblpx(10),lblpy(10)

      COMMON/GRAF1/YMAX(6),YMIN(6),YAXSET(6),ADDLIN,horaxisdiv
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

C To extend logic to maximise plotting area.
      integer YAXSET,ADDLIN,horaxisdiv
      integer iglib   ! if iglib is 2 then using GTK library otherwise X11
      integer onright  ! number of axis on right
      integer onleft   ! whether there is an axis on the left

      CHARACTER ETEXT*136, RSNAME*40
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      CHARACTER PDESCR*64,SDESCR*44,outsd*124,outsd1*126
      character graftitle*64,grlbl*24,t64*64
#ifdef OSI
      integer iside,isize,ifont       ! passed to viewtext
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
      integer ilf,igfw,igfh,ild,igdw,igdh
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
      integer*8 ilf,igfw,igfh,ild,igdw,igdh
#endif

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
          iimenu=menuchw; ilf=2; ild=LIMTTY
          call startbuffer
          call feedbox(iimenu,ilf,igfw,igfh)
          call opengdisp(iimenu,ild,ilf,igdw,igdh)
        endif

C Logic to alter the '30' for two different cases:
C  a) we are using GTK and a proportional font << to be done >>
C  b) there is none or one or two right axis 
C  c) there is none or one left axis

C Optimise axis positions for each possible combination
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

C Set button font width so that space around graph is correct.
        call winfnt(ifs)
        if(onright.eq.0)then
          if(onleft.eq.0)then

C Setup and pass in parameters to win3d.
            iiw1=3; iiw2=34; iiw3=7; iiw4=3; iimenu=menuchw
            iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          else
            iiw1=7; iiw2=34; iiw3=7; iiw4=3; iimenu=menuchw
            iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          endif
        elseif(onright.eq.1)then
          if(onleft.eq.0)then
            iiw1=3; iiw2=42; iiw3=7; iiw4=3; iimenu=menuchw
            iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          else
            iiw1=7; iiw2=42; iiw3=7; iiw4=3; iimenu=menuchw
            iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          endif
        elseif(onright.eq.2)then

C Set the right termination of the horizontal axis to 43 char in so that
C there is room for two axis on the right.
          if(onleft.eq.0)then
            iiw1=3; iiw2=53; iiw3=7; iiw4=3; iimenu=menuchw
            iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          else
            iiw1=7; iiw2=53; iiw3=7; iiw4=3; iimenu=menuchw
            iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          endif
        endif

C Cast values back depending on machine type.
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        call startbuffer
        call winfnt(imfs)  ! back to menu font?
      ENDIF

      lsn1=MIN0(lnblnk(RFILE),42)
      if(NSIM.gt.1)then
        WRITE(ETEXT,'(3A,I4,3A)')'Results library: ',RFILE(1:lsn1),
     &    '; results set:',ISIM,
     &    ': (',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM))),')'
      else
        WRITE(ETEXT,'(5A)')'Results library: ',RFILE(1:lsn1),
     &    '; (',RSNAME(ISIM)(1:lnblnk(RSNAME(ISIM))),')'
      endif
      iside=1; isize=0; ifont=1
      if(mmod.eq.8)then
        call viewtext(etext,iside,isize,ifont)
      else
        call viewtextwwc(etext,iside,isize,ifont)
      endif

      WRITE(ETEXT,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &  SDESCR(12:lnblnk(SDESCR))
      iside=2
      if(mmod.eq.8)then
        call viewtext(etext,iside,isize,ifont)
      else
        call viewtextwwc(etext,iside,isize,ifont)
      endif

      if(nzonec.ge.NZ)then
        call colznlist(3,1,ierr)
      else
        call znlist(outsd,len,ierr) 
        if(ierr.ne.1)then
          write(outsd1,'(1x,a)')outsd
          iside=3
          if(mmod.eq.8)then
            call viewtext(outsd1,iside,isize,ifont)
          else
            call viewtextwwc(outsd1,iside,isize,ifont)
          endif
        endif
      endif

C Remember if a 4th line of text is being drawn.
      iancline=0
      if(IROC.eq.0.and.ialstused.ne.0)then
        WRITE(outsd1,'(1x,2a)')'Using anchor list ',ALOCLBL(ialstused)
        iside=4
        call viewtext(outsd1,iside,isize,ifont)
        iancline=iancline+1
      elseif(IROC.eq.1.and.ialstused.ne.0)then
        WRITE(outsd1,'(1x,3a)')'Using anchor list ',ALOCLBL(ialstused),
     &     '      (rate of change of variable plot)'
        iside=4
        call viewtext(outsd1,iside,isize,ifont)
        iancline=iancline+1
      elseif(IROC.eq.1.and.ialstused.eq.0)then
        WRITE(outsd1,'(a)') ' (rate of change of variable plot)'
        iside=4
        call viewtext(outsd1,iside,isize,ifont)
        iancline=iancline+1
      endif
      if(graftitle(1:2).ne.'  ')then
        t64=graftitle
        if(iancline.gt.0)then
          iside=5; isize=1; ifont=2
          call viewtext(t64,iside,isize,ifont)
        endif
        if(iancline.eq.0)then
          iside=4; isize=1; ifont=2
          call viewtext(t64,iside,isize,ifont)
        endif
      endif

      RETURN
      END

C ******************** MOCOPT ********************
C Returns control graph options selected by user.

      SUBROUTINE MOCOPT
#include "help.h"
              
      COMMON/OUTIN/IUOUT,IUIN,IEOUT              
      COMMON/CGHOPT/ERPLOT,TRPLOT,SPPLOT,NCTLGH,ICTLGH,ISP,ITHR
      COMMON/CTLDAT/SPH,SPC,TRH,TRC,ICF,IDTYP,IPER,IBLW,PLTCTL,ILOOP
      LOGICAL ERPLOT,TRPLOT,SPPLOT,ITHR,ISP,PLTCTL,OK

      helpinsub='tgraph'  ! set for subroutine

      CALL EDISP(IUOUT,' ') 
      helptopic='res_control_point_t'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKOK(' ','Plot control error?',OK,nbhelp)
      IF(OK)ERPLOT=.TRUE.
      CALL EASKOK(' ','Plot control set-point?',OK,nbhelp)
      IF(OK)SPPLOT=.TRUE. 
      CALL EASKOK(' ','Plot throttling range?',OK,nbhelp)
      IF(OK)TRPLOT=.TRUE.
      CALL EASKOK(' ','Plot plant loop(s)?',OK,nbhelp)
      IF(OK)THEN
10      CALL EASKI(iloop,' ','Which plant control loop?',
     &    1,'F',20,'F',1,'plant loop',IER,nbhelp)
        IF(IER.NE.0)goto 10
      ENDIF

C Set control graph options counter.
      NCTLGH=1
      IF(ERPLOT)NCTLGH=NCTLGH+2
      IF(TRPLOT)NCTLGH=NCTLGH+4
      IF(SPPLOT)NCTLGH=NCTLGH+2     
C Set control graph options flag.
      IF(NCTLGH.EQ.1)ICTLGH=1
      IF(ERPLOT.AND..NOT.TRPLOT.AND..NOT.SPPLOT)ICTLGH=2
      IF(TRPLOT.AND..NOT.ERPLOT.AND..NOT.SPPLOT)ICTLGH=3
      IF(SPPLOT.AND..NOT.ERPLOT.AND..NOT.TRPLOT)ICTLGH=4
      IF(ERPLOT.AND.TRPLOT.AND..NOT.SPPLOT)ICTLGH=5
      IF(ERPLOT.AND.SPPLOT.AND..NOT.TRPLOT)ICTLGH=6
      IF(TRPLOT.AND.SPPLOT.AND..NOT.ERPLOT)ICTLGH=7
      IF(ERPLOT.AND.TRPLOT.AND.SPPLOT)ICTLGH=8

C Set control loop flag for labelling.
      PLTCTL=.FALSE.
      
      RETURN
      END

         
C ******************** MOCDAT ********************
C Returns control data for current time-step.

      SUBROUTINE MOCDAT(IZON,IDAY,TME)  
#include "building.h"
#include "control.h"

      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/CGHOPT/ERPLOT,TRPLOT,SPPLOT,NCTLGH,ICTLGH,ISP,ITHR
      COMMON/CTLDAT/SPH,SPC,TRH,TRC,ICF,IDTYP,IPER,IBLW,PLTCTL,ILOOP
      integer icascf
      COMMON/CCTL/ICASCF(MCOM)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      

      LOGICAL ERPLOT,TRPLOT,SPPLOT,ITHR,ISP,PLTCTL
      
C If routine called by mogdrw, time may be greater than 24.0.
      TIME=TME
      IF(TIME.GT.24.0)TIME=(TIME-(IDAY-IODS)*24.0)      

C Initialise set-point and throttling range plot options flags.
C Graph only plotted if flag is set to 'true'.
      ISP=.TRUE.
      ITHR=.TRUE.

C Test to see if a control function is active.
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

C Commence control data return.
4     N=NBCDP(ICF,IDTYP)
      DO 30 I=1,N
      IPER=I
      IF(I.EQ.N)GOTO 9
      IF(TIME.LE.TBCPS(ICF,IDTYP,I+1))GOTO 12
      GOTO 30
9     IF(TIME.GT.TBCPS(ICF,IDTYP,N))GOTO 12
30    CONTINUE
      GOTO 101

C Valid period established; now return control data.
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

C Set up day type and period pointers.
         NDAYT=NPCDT(ILOOP)
            
C If NDAYT=0 set data to weekday, saturday and sunday format.
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

C Check number of periods in each day and the start and finish times.
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

C Valid period established, get control data        
C which depends on active plant control law, iclaw.
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
      integer icascf      
      COMMON/CCTL/ICASCF(MCOM)

      DIMENSION XVAL1(MS),XREM1(MS)
      character BLAB*13,label*8
      LOGICAL ERPLOT,TRPLOT,SPPLOT,ITHR,ISP,PLTCTL,NOPLOT
#ifdef OSI
      integer ipos,isize      ! for use with etlabel
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 ipos,isize    ! for use with etlabel
      integer*8 iupdown,isym  ! passed to etplot
#endif

      XREM1(1)=0.0
      XREM1(2)=0.0

C Initialise the set-point/throttling-range plot flag,
      NOPLOT=.FALSE.
      
      ITS=NOUT/NTS
      N=24*NTS
      MARK=0
      IF(IR.EQ.0)MARK=9
      
      XVAL1(1)=CPVAL

C For each plot.
      DO 322 L=1,NCTLGH
      I=ICTLGH
      IF(ITS.GT.1)GOTO 321
      IF(TIME.LE.TSTART)GOTO 32 
      IF(L.EQ.1)THEN
        LABEL='Ctl Pt'
        IPEN=-304
      ENDIF

C If additional plot requested get control data.
      IF(L.GT.1)CALL MOCDAT(IZONE,II,TIME)

C Value plotted depends on the type of plot selected by user.
      IF(L.EQ.2.AND.I.EQ.2.OR.L.EQ.2.AND.I.EQ.5.OR.
     &   L.EQ.2.AND.I.EQ.6.OR.L.EQ.2.AND.I.EQ.8)THEN   
         XVAL1(L)=XVAL1(1)-SPH
         IPEN=-1
         LABEL='Htg Err'
         IF(PLTCTL)LABEL= 'Error'
       ELSEIF(L.EQ.3.AND.I.EQ.2.OR.L.EQ.3.AND.I.EQ.5.OR.
     &        L.EQ.3.AND.I.EQ.6.OR.L.EQ.3.AND.I.EQ.8)THEN
         IF(IBLW.EQ.6)GOTO 322
         XVAL1(L)=XVAL1(1)-SPC
         IPEN=-1
         LABEL='Clg Err'
         IF(PLTCTL)LABEL= 'Error'
       ELSEIF(L.EQ.2.AND.I.EQ.3.OR.L.EQ.4.AND.I.EQ.5.OR.
     &        L.EQ.2.AND.I.EQ.7.OR.L.EQ.4.AND.I.EQ.8)THEN
         XVAL1(L)=SPH+(TRH/2)
         IPEN=-104
         LABEL='HtgThUp'
         IF(PLTCTL)LABEL='Tht Hi'
         IF(.NOT.ITHR)NOPLOT=.TRUE.          
       ELSEIF(L.EQ.3.AND.I.EQ.3.OR.L.EQ.5.AND.I.EQ.5.OR.
     &        L.EQ.3.AND.I.EQ.7.OR.L.EQ.5.AND.I.EQ.8)THEN
         XVAL1(L)=SPH-(TRH/2)
         IPEN=-104
         LABEL='HtgThLo'
         IF(PLTCTL)LABEL='Tht Lo'
         IF(.NOT.ITHR)NOPLOT=.TRUE.
       ELSEIF(L.EQ.4.AND.I.EQ.3.OR.L.EQ.6.AND.I.EQ.5.OR.
     &        L.EQ.4.AND.I.EQ.7.OR.L.EQ.6.AND.I.EQ.8)THEN
         IF(IBLW.EQ.6)GOTO 322   
         XVAL1(L)=SPC+(TRC/2)
         IPEN=-104
         LABEL='ClgThUp'
         IF(PLTCTL)LABEL='Tht Hi'
         IF(.NOT.ITHR)NOPLOT=.TRUE.
       ELSEIF(L.EQ.5.AND.I.EQ.3.OR.L.EQ.7.AND.I.EQ.5.OR.
     &        L.EQ.5.AND.I.EQ.7.OR.L.EQ.7.AND.I.EQ.8)THEN
         IF(IBLW.EQ.6)GOTO 322
         XVAL1(L)=SPC-(TRC/2)
         IPEN=-104
         LABEL='ClgThLo'
         IF(PLTCTL)LABEL='Tht Lo'
         IF(.NOT.ITHR)NOPLOT=.TRUE.
       ELSEIF(L.EQ.2.AND.I.EQ.4.OR.L.EQ.4.AND.I.EQ.6.OR.
     &        L.EQ.6.AND.I.EQ.7.OR.L.EQ.8.AND.I.EQ.8)THEN
         XVAL1(L)=SPH
         IPEN=-9
         LABEL='HtgStPt'
         IF(PLTCTL)LABEL='Set Pt'
         IF(.NOT.ISP)NOPLOT=.TRUE.
       ELSEIF(L.EQ.3.AND.I.EQ.4.OR.L.EQ.5.AND.I.EQ.6.OR.
     &        L.EQ.7.AND.I.EQ.7.OR.L.EQ.9.AND.I.EQ.8)THEN
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

C Now plot graph(s).
       IF(L.EQ.1.OR.((L.EQ.2.OR.L.EQ.3).AND.
     &   (I.NE.3.AND.I.NE.4.AND.I.NE.7)))THEN

C A 'point-to-point plot' is used for control point and error graphs.
         iupdown=0
         isym=0
         call etplot(TIMEL,Y,iupdown,isym)
         IF(L.GT.1)THEN
            IF(.NOT.ISP.AND.IBLW.NE.1.AND.IBLW.NE.3)THEN

C No error plot.
              iupdown=0
              isym=0
              call etplot(TIME,XVAL1(L),iupdown,isym)
            ELSE
              iupdown=IPEN
              isym=MARK
              call etplot(TIME,XVAL1(L),iupdown,isym)
            ENDIF
         ELSE
            iupdown=IPEN
            isym=MARK
            call etplot(TIME,XVAL1(L),iupdown,isym)
         ENDIF
       ELSE

C A 'rectangular plot' is used for set-point & throttling range graphs.
         TIMEP=TIMEL+(0.5*NOUT)
         TIMEF=TIME+(0.5*NOUT)
         iupdown=0
         isym=0
         call etplot(TIMEP,Y,iupdown,isym)

C If first pass, then no vertical plot.
         IF(J.EQ.2)THEN
           iupdown=0
           isym=0
           call etplot(TIMEP,XVAL1(L),iupdown,isym)
         ELSE
           IF(NOPLOT)THEN
             iupdown=0
             isym=0
             call etplot(TIMEP,XVAL1(L),iupdown,isym)
           ELSE
             iupdown=IPEN
             isym=0
             call etplot(TIMEP,XVAL1(L),iupdown,isym)
           ENDIF
         ENDIF
         IF(NOPLOT)THEN

C no horizontal plot.
           iupdown=0
           isym=0
           call etplot(TIMEF,XVAL1(L),iupdown,isym)
         ELSE
           iupdown=IPEN
           isym=MARK
           call etplot(TIMEF,XVAL1(L),iupdown,isym)
         ENDIF
       ENDIF
              
       IF(II.EQ.IODF.AND.J.GT.N-NOUT)THEN
         ipos=3
         isize=0
         IF(NZ.GT.1)THEN        
           write(BLAB,'(A2,I3.3,A8)')'zn',IZONE,LABEL
           call etlabel(BLAB,TFINSH,XVAL1(L),ipos,isize)
         ELSE
           call etlabel(LABEL,TFINSH,XVAL1(L),ipos,isize)
         ENDIF
       ENDIF
       GOTO 322

309    iupdown=0
       isym=0
       call etplot(TIME,XVAL1(L),iupdown,isym)
       IF(II.EQ.IODF.AND.J.GT.N-NOUT)THEN
       call usrmsg(' ', 'Uncontrolled zone(s) selected.','W')
       ENDIF
       GOTO 322

32     XREM1(L)=XVAL1(L)
       iupdown=0
       isym=0
       call etplot(TIME,XVAL1(L),iupdown,isym)
       goto 322
321    iupdown=0
       isym=9
       call etplot(TIME,XVAL1(L),iupdown,isym)
       goto 322
322    CONTINUE
      
       RETURN
       END
