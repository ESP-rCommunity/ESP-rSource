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
#include "building.h"
#include "model.h"
#include "prj3dv.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "help.h"
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      common/getmenu/menutype,igetind(65),igetflux(65)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      CHARACTER ITEM(16)*22
      character LAPROB*72
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

      helpinsub='resutil'  ! set for subroutine

C Create menu.
      ITEM(1) ='   Graphs:            '
      ITEM(2) ='a time series         '
      ITEM(3) ='p psychrometric chart '
      if(iairn.eq.3)then
        ITEM(4) ='3 3D flow network     '
      else
        ITEM(4) ='  3D flow network     '
      endif
      ITEM(5) ='  ------------------  '
      ITEM(6) ='   Statistics:        '
      ITEM(7) ='b summary             '
      ITEM(8) ='c hours > query point '
      ITEM(9) ='d hours < query point '
      ITEM(10)='e frequency histogram '
      ITEM(11)='  ------------------  '
      ITEM(12)='   Tabular lists:     '
      ITEM(13)='f performance metrics '
      ITEM(14)='  ------------------  '
      ITEM(15)='? help                '
      ITEM(16)='- exit menu'

      NITMS=16
      if(MMOD.eq.8)then
        INO=-1
      else
        INO=-2
      endif

C Instantiate help strings for the menu.
      helptopic='res_style_selection_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

 1    CALL EMENU('Output styles',ITEM,NITMS,INO)

C Set MENUTYPE depending on choice made.
      if (INO.eq.2) then
        MENUTYPE=3
      elseif (INO.eq.3) then
        MENUTYPE=9
      elseif (INO.eq.4) then

C Switch to 3D view. Might need to refresh scaling.
        if(iairn.eq.3)then
          MENUTYPE=15

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
          iiw1=4; iiw2=4; iiw3=3; iiw4=3; iimenu=menuchw
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

C Read 3D flow network, report contents and draw the network.
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
      elseif (INO.eq.7) then
        MENUTYPE=5
      elseif (INO.eq.8) then
        MENUTYPE=7
      elseif (INO.eq.9) then
        MENUTYPE=8
      elseif (INO.eq.10) then
        MENUTYPE=2
      elseif (INO.eq.13) then
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
      COMMON/GRAF1/YMAX(6),YMIN(6),YAXSET(6),ADDLIN,horaxisdiv
      COMMON/IGETFLG/IOCUPF,ialstused,IROC

      integer YAXSET,ADDLIN,horaxisdiv

      character ALABEL(6)*12

C Reset axis scaling.
      do 4 I=1,6
        if (YAXSET(I).lt.2.or.YAXSET(I).eq.5) then
          YMAX(I)=-1.E+10
          YMIN(I)=1.E+10
        endif
 4    continue

C Define labels.
C IROC, if rate of change of variable alter the axis label.
      if (IGRTP.eq.1) then

C Standard building labels.
        if (IROC.eq.0) then
          ALABEL(1)='Temp: C'
          ALABEL(2)='Load: W'
          ALABEL(3)='Int: W/m^2'
          ALABEL(4)='Misc.'
        elseif (IROC.eq.1) then
          ALABEL(1)='T: C/hr'
          ALABEL(2)='Load: W/hr'
          ALABEL(3)='Int:W/m^2/hr'
          ALABEL(4)='Misc./hr'
        endif
      elseif (IGRTP.eq.2) then

C Mass flow labels.
        if (IROC.eq.0) then
          ALABEL(1)='Temp: C'
          ALABEL(2)='Pres: Pa'
          ALABEL(3)='Flow rate'
          ALABEL(4)='Misc.'
          ALABEL(5)='Conc: g/kg'
          ALABEL(6)='Vel: m/s'
        elseif (IROC.eq.1) then
          ALABEL(1)='T: C/hr'
          ALABEL(2)='P: Pa/hr'
          ALABEL(3)='Flw rate/hr'
          ALABEL(4)='Misc./hr'
          ALABEL(5)='Conc e-3/hr'
          ALABEL(6)='V: m/s/hr'
        endif
      elseif (IGRTP.eq.3) then
        ALABEL(1)='Temp: C'
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
      COMMON/GRAF1/YMAX(6),YMIN(6),YAXSET(6),ADDLIN,horaxisdiv
      COMMON/GRAF2/YSC(6),Yadd(6),TSC,Xadd,IGX(6),ILR(6),DT

      integer YAXSET,ADDLIN,horaxisdiv

#ifdef OSI
      integer igx
#else
      integer*8 igx
#endif

C Set default positions.
      igx(1)=igl
      igx(2)=igr+140
      igx(3)=igr+80
      igx(4)=2*(igl+igr)/3
      igx(5)=(igl+igr)/3
      igx(6)=igl
      ILR(1)=0
      ILR(2)=1
      ILR(3)=1
      ILR(4)=1
      ILR(5)=1
      ILR(6)=0

C Optimise axis positions.
      NAX=0
      do 10 I=1,6
        if (YAXSET(I).gt.0) NAX=NAX+1
 10   continue

C Position each axis in turn.
      IAXOK=0
      do 20 I=1,6
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
      COMMON/GRAF1/YMAX(6),YMIN(6),YAXSET(6),ADDLIN,horaxisdiv
      COMMON/GRAF2/YSC(6),Yadd(6),TSC,Xadd,IGX(6),ILR(6),DT
      real TST,TFI,TSCR  ! for hourly version

      character ALABEL(6)*12,act*1

      integer YAXSET,ADDLIN,horaxisdiv
#ifdef OSI
      integer igx  ! see common graf2
      integer igwid,igheight,iid1,iid2  ! for use with axiscale
      integer iigl,iigr,iigt,iigb,iigw,iigwh,iicol,ibsize
#else
      integer*8 igx  ! see common graf2
      integer*8 igwid,igheight,iid1,iid2  ! for use with axiscale
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh,iicol,ibsize
#endif

C Cast values for horaxis.
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh

C Debug.
C      write(6,*) 'AXISDRW',(YAXSET(I),I=1,5)

C Draw each axis in turn.
      do 10 I=1,6
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
          call dintervalf(TSTART,TFINISH,DT,NDEC,1)
          igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,TSTART,TFINISH,YMIN(I),YMAX(I),
     &                             TSC,YSC(I),sca,Xadd,Yadd(I))
          if (ILR(I).eq.0) then
            call dintervalf(YMIN(I),YMAX(I),ddy1,ny,0)
            call vrtaxisdd(YMIN(I),YMAX(I),igx(I),iigb,iigt,Yadd(I),
     &        YSC(I),0,ddy1,ny,ilr(I),ALABEL(I))
          else
            call dintervalf(YMIN(I),YMAX(I),ddy1,ny,0)
            call vrtaxisdd(YMIN(I),YMAX(I),igx(I),iigb,iigt,Yadd(I),
     &        YSC(I),0,ddy1,ny,ilr(I),'  ')
            iid1=igx(I); iid2=igt-5;
            iicol=0; ibsize=0
            call textsizeatxy(iid1,iid2,ALABEL(I),ibsize,'-',iicol)
          endif
        endif
 10   continue

C Draw horizontal (time) axis.
      if(act.eq.'P')then
        call dintervalf(TSTART,TFINISH,ddy1,ny,1)
        call horaxisdd(TSTART,TFINISH,iigl,iigr,iigb,Xadd,TSC,1,
     &    ddy1,ny,'Timesteps')
      else

C- Hardwired to display hours instead of timesteps on x-axis
C- To revert back to original remove npts from following statement
        if(npts.eq.0) npts=1
        TST=TSTART/real(npts)
        TFI=TFINISH/real(npts)
        TSCR=TSC*real(npts)
C        call horaxis(TST,TFI,iigl,iigr,iigb,Xadd,TSCR,1,'Time Hrs')
        if(horaxisdiv.eq.0.or.horaxisdiv.eq.1)then

C Default is the same as AXISDRW always provided.
          call dintervalf(TST,TFI,ddy1,ny,1)
          call horaxisdd(TST,TFI,iigl,iigr,iigb,Xadd,TSCR,1,
     &      ddy1,ny,'Time Hrs')
        elseif(horaxisdiv.eq.2)then
          call dintervalf(TST,TFI,ddy1,ny,1)
          call horaxisdd(TST,TFI,iigl,iigr,iigb,Xadd,TSCR,1,
     &      ddy1,ny,'Time Hrs')
        elseif(horaxisdiv.eq.3)then
          call dintervalf(TST,TFI,ddy1,ny,1)
          call horaxishdwdd(TST,TFI,iigl,iigr,iigb,Xadd,TSCR,1,
     &      ddy1,ny,2,24,iods,'Time (days)')
        elseif(horaxisdiv.eq.4)then
          call dintervalf(TST,TFI,ddy1,ny,1)
          call horaxishdwdd(TST,TFI,iigl,iigr,iigb,Xadd,TSCR,1,
     &      ddy1,ny,3,168,iods,'Time (weeks)')
        endif
      endif
      RETURN
      END
