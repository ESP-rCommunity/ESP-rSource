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

C 'CLMGRF' ALLOWS THE CONSTRUCTION OF GRAPHS WHICH SHOW THE
C DISTRIBUTION OF ANY REQUESTED CLIMATIC PARAMETER OVER THE
C USER-SPECIFIED PERIOD.

      SUBROUTINE CLMGRF
      COMMON/OUTIN/IUOUT,IUIN
C      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/MXMNC/YMAX(6),YMIN(6),TMAX,TMIN,DT
      COMMON/RADTYP/IDNGH
      COMMON/CGSC/XSC,YSC(6),Xadd,Yadd(6)
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      CHARACTER*26 ITEM(16)
      character hold*72
      DIMENSION IPICK(6)
      LOGICAL mxmn
      integer onleft  ! number of left axis
      integer onright ! number of right axis
      integer NITMS,INO  ! max items and current menu item

#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CLMGRF'  ! set for cfiles

      onleft = 0      ! initial assumption of no axis
      onright = 0

      ITEM(1) =  'a set period            '
      ITEM(2) =  '  _____________________ '
      ITEM(3) =  'b dry bulb temperature  '
      if(IDNGH.eq.0)then
        ITEM(4) ='c direct normal solar   '
      else
        ITEM(4) ='c global horizon. solar '
      endif
      ITEM(5) =  'd diffuse solar         '
      ITEM(6) =  'e wind speed (m/s)      '
      ITEM(7) =  'f wind direction (deg)  '
      ITEM(8) =  'g relative humidity (%) '
      ITEM(9) =  '  _____________________ '
      ITEM(10)=  '! draw graph            '
      ITEM(11)=  'j reset axis scale      '
      ITEM(12)=  'k refresh current graph '
      ITEM(13)=  'l clear current items   '
      ITEM(14)=  '  _____________________ '
      ITEM(15)=  '? help                  '
      ITEM(16)=  '- exit                  '

C Clear the selection list.
      DO 10 I=1,6
        IPICK(I)=0
   10 CONTINUE
      IPP=0

C Display graphing options.
    7 INO=-2
      NITMS=16

      call usrmsg('  ','  ','-')
    6 CALL EMENU(' Graphical analysis',ITEM,NITMS,INO)
      
      if(ino.eq.nitms)then
        return
      elseif(ino.eq.1)then
        CALL selper(ier)
      elseif(ino.eq.3)then
        IPICK(2)=1
        IPP=1
      elseif(ino.eq.4)then
        IPICK(3)=1
        IPP=1
      elseif(ino.eq.5)then
        IPICK(1)=1
        IPP=1
      elseif(ino.eq.6)then
        IPICK(4)=1
        IPP=1
      elseif(ino.eq.7)then
        IPICK(5)=1
        IPP=1
      elseif(ino.eq.8)then
        IPICK(6)=1
        IPP=1
      elseif(ino.eq.10)then

C Draw the selected data (the non-zero ipick values). Begin by
C establishing the number of axis on left and then (more
C complicated logic) the right.
        IF(IPP.EQ.1)then
          if(ipick(2).eq.1.and.ipick(4).eq.1)then
            onleft=2
          elseif(ipick(2).eq.1.and.ipick(4).eq.0)then
            onleft=1
          elseif(ipick(2).eq.0.and.ipick(4).eq.1)then
            onleft=1
          elseif(ipick(2).eq.0.and.ipick(4).eq.0)then
            onleft=0
          endif

          if(ipick(1).eq.1.or.ipick(3).eq.1)then
            if(ipick(5).eq.1)then
              if(ipick(6).eq.1)then
                onright=3
              else
                onright=2
              endif
            else
              if(ipick(6).eq.1)then
                onright=2
              else
                onright=1
              endif
            endif
          else
            if(ipick(5).eq.1)then
              if(ipick(6).eq.1)then
                onright=2
              else
                onright=1
              endif
            else
              if(ipick(6).eq.1)then
                onright=1
              else
                onright=0
              endif
            endif
          endif

C Debug.
C          write(6,*) ' a number of right axis is ',onright

C Take into account which axis needs to be drawn as in esrures/tgraph.F.
          if(IFS.ne.IMFS)call winfnt(IMFS)
          if(onright.eq.3)then
            if(onleft.eq.2)then

C Setup and pass in parameters to win3d.
              iiw1=10; iiw2=20; iiw3=6; iiw4=3; iimenu=menuchw
              iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
              CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &          iigl,iigr,iigt,iigb,iigw,iigwh)
            elseif(onleft.eq.1)then
              iiw1=6; iiw2=20; iiw3=6; iiw4=3; iimenu=menuchw
              iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
              CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &          iigl,iigr,iigt,iigb,iigw,iigwh)
            elseif(onleft.eq.0)then
              iiw1=3; iiw2=20; iiw3=6; iiw4=3; iimenu=menuchw
              iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
              CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &          iigl,iigr,iigt,iigb,iigw,iigwh)
            endif
            igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
            igw=int(iigw); igwh=int(iigwh)
          elseif(onright.eq.2)then
            if(onleft.eq.2)then
              iiw1=10; iiw2=16; iiw3=6; iiw4=3; iimenu=menuchw
              iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
              CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &          iigl,iigr,iigt,iigb,iigw,iigwh)
            elseif(onleft.eq.1)then
              iiw1=6; iiw2=16; iiw3=6; iiw4=3; iimenu=menuchw
              iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
              CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &          iigl,iigr,iigt,iigb,iigw,iigwh)
            elseif(onleft.eq.0)then
              iiw1=3; iiw2=16; iiw3=6; iiw4=3; iimenu=menuchw
              iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
              CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &          iigl,iigr,iigt,iigb,iigw,iigwh)
            endif
            igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
            igw=int(iigw); igwh=int(iigwh)
          elseif(onright.eq.1)then
            if(onleft.eq.2)then
              iiw1=10; iiw2=12; iiw3=6; iiw4=3; iimenu=menuchw
              iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
              CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &          iigl,iigr,iigt,iigb,iigw,iigwh)
            elseif(onleft.eq.1)then
              iiw1=6; iiw2=12; iiw3=6; iiw4=3; iimenu=menuchw
              iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
              CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &          iigl,iigr,iigt,iigb,iigw,iigwh)
            elseif(onleft.eq.0)then
              iiw1=3; iiw2=12; iiw3=6; iiw4=3; iimenu=menuchw
              iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
              CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &          iigl,iigr,iigt,iigb,iigw,iigwh)
            endif
            igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
            igw=int(iigw); igwh=int(iigwh)
          elseif(onright.eq.0)then
            if(onleft.eq.2)then
              iiw1=10; iiw2=6; iiw3=6; iiw4=3; iimenu=menuchw
              iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
              CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &          iigl,iigr,iigt,iigb,iigw,iigwh)
            elseif(onleft.eq.1)then
              iiw1=6; iiw2=6; iiw3=6; iiw4=3; iimenu=menuchw
              iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
              CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &          iigl,iigr,iigt,iigb,iigw,iigwh)
            elseif(onleft.eq.0)then
              iiw1=3; iiw2=6; iiw3=6; iiw4=3; iimenu=menuchw
              iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
              CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &          iigl,iigr,iigt,iigb,iigw,iigwh)
            endif
            igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
            igw=int(iigw); igwh=int(iigwh)
          endif
          if(IFS.ne.IMFS)call winfnt(IFS)
          call startbuffer()

C Debug.
C          write(6,*)'clmgrf igl igr igt igb igw igwh',igl,igr,igt,
C     &      igb,igw,igwh

          mxmn=.TRUE.
          CALL CLMDRW(IPICK,mxmn,onleft,onright,'-')
          call usrmsg('  ','  ','-')
          goto 6
        else
          call edisp(iuout,' Nothing to draw yet ! ')
        endif
      elseif(ino.eq.11)then

C Scale axis (ipick).
C    1= diff solar (same as scale 3)
C    2= db temperature (at X pixel 50)
C    3= dir solar rad (at igr nearest at right)
C    4= wind speed (at igl nearest at left)
C    5= wind direction (at igr+50 middle at right)
C    6= rh (at igr+100 farthest at right)

C Present vertex coords for editing then parse data from HOLD.
        WRITE(HOLD,'(1x,4f7.1)')YMAX(1),YMIN(1),YMAX(2),YMIN(2)
        helptopic='clm_solar_graph'
        call gethelptext(helpinsub,helptopic,nbhelp)
  42    CALL EASKS(HOLD,' Max & Min for solar radiation & db temp:',
     &     ' : ',72,' 800. 0. 40. 0. ','1st axis mx mn',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,YMAX(1),0.,0.,'-','dif sol mx',IER)
        CALL EGETWR(HOLD,K,YMIN(1),0.,0.,'-','dif sol mn',IER)
        CALL EGETWR(HOLD,K,YMAX(2),0.,0.,'-','db temp mx',IER)
        CALL EGETWR(HOLD,K,YMIN(2),0.,0.,'-','db temp mn',IER)
        if(ier.ne.0)goto 42
        YMAX(3)=YMAX(1)
        YMIN(3)=YMIN(1)

C Second set of axis.
        WRITE(HOLD,'(1x,6f7.1)')YMAX(4),YMIN(4),YMAX(5),YMIN(5),
     &    YMAX(6),YMIN(6)
        helptopic='clm_wind_graph'
        call gethelptext(helpinsub,helptopic,nbhelp)
  43    CALL EASKS(HOLD,' Max & Min for wind speed, wind dir, rh:',' :',
     &     72,' 10. 0. 360. 0. 100. 0.','2nd axis mx mn',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,YMAX(4),0.,0.,'-','wind spd mx',IER)
        CALL EGETWR(HOLD,K,YMIN(4),0.,0.,'-','wind spd mn',IER)
        CALL EGETWR(HOLD,K,YMAX(5),0.,360.,'-','wind dir mx',IER)
        CALL EGETWR(HOLD,K,YMIN(5),0.,360.,'-','wind dir mn',IER)
        CALL EGETWR(HOLD,K,YMAX(6),0.,0.,'-','rh mx',IER)
        CALL EGETWR(HOLD,K,YMIN(6),0.,0.,'-','rh mn',IER)
        if(ier.ne.0)goto 43
        DO 70 I=1,6
          igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,TMIN,TMAX,YMIN(I),YMAX(I),
     &      XSC,bysc,sca,Xadd,addY)
          YSC(I)=bysc
          Yadd(I)=addY
  70    CONTINUE
        mxmn=.FALSE.
        CALL CLMDRW(IPICK,mxmn,onleft,onright,'-')
        goto 6
      elseif(ino.eq.12)then

C In case the user has resized the display, re-establish the graphic
C bounds and redraw the image before presenting the command menu.
        if(ipick(1).eq.1.or.ipick(3).eq.1)then
          if(ipick(5).eq.1)then
            if(ipick(6).eq.1)then
              onright=3
            else
              onright=2
            endif
          else
            if(ipick(6).eq.1)then
              onright=2
            else
              onright=1
            endif
          endif
        else
          if(ipick(5).eq.1)then
            if(ipick(6).eq.1)then
              onright=2
            else
              onright=1
            endif
          else
            if(ipick(6).eq.1)then
              onright=1
            else
              onright=0
            endif
          endif
        endif
C Debug.
C        write(6,*) ' b number of right axis is ',onright

C Take into account which axis needed to be drawn as in esrures/tgraph.F
        if(IFS.ne.IMFS)call winfnt(IMFS)
        if(onright.eq.3)then
          if(onleft.eq.2)then
            iiw1=10; iiw2=20; iiw3=6; iiw4=3; iimenu=menuchw
            iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          elseif(onleft.eq.1)then
            iiw1=6; iiw2=20; iiw3=6; iiw4=3; iimenu=menuchw
            iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          elseif(onleft.eq.0)then
            iiw1=3; iiw2=20; iiw3=6; iiw4=3; iimenu=menuchw
            iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          endif
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
        elseif(onright.eq.2)then
          if(onleft.eq.2)then
            iiw1=10; iiw2=16; iiw3=6; iiw4=3; iimenu=menuchw
            iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          elseif(onleft.eq.1)then
            iiw1=6; iiw2=16; iiw3=6; iiw4=3; iimenu=menuchw
            iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          elseif(onleft.eq.0)then
            iiw1=3; iiw2=16; iiw3=6; iiw4=3; iimenu=menuchw
            iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          endif
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
        elseif(onright.eq.1)then
          if(onleft.eq.2)then
            iiw1=10; iiw2=12; iiw3=6; iiw4=3; iimenu=menuchw
            iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          elseif(onleft.eq.1)then
            iiw1=6; iiw2=12; iiw3=6; iiw4=3; iimenu=menuchw
            iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          elseif(onleft.eq.0)then
            iiw1=3; iiw2=12; iiw3=6; iiw4=3; iimenu=menuchw
            iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          endif
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
        elseif(onright.eq.0)then
          if(onleft.eq.2)then
            iiw1=10; iiw2=6; iiw3=6; iiw4=3; iimenu=menuchw
            iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          elseif(onleft.eq.1)then
            iiw1=6; iiw2=6; iiw3=6; iiw4=3; iimenu=menuchw
            iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          elseif(onleft.eq.0)then
            iiw1=3; iiw2=6; iiw3=6; iiw4=3; iimenu=menuchw
            iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
            CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &        iigl,iigr,iigt,iigb,iigw,iigwh)
          endif
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
        endif
        if(IFS.ne.IMFS)call winfnt(IFS)
        call startbuffer()

C Debug.
C        write(6,*)'igl igr igt igb igw igwh ',igl,igr,igt,igb,igw,igwh

        mxmn=.FALSE.
        CALL CLMDRW(IPICK,mxmn,onleft,onright,'-')
        goto 6
      elseif(ino.eq.13)then

C Clear selections.
        DO 101 I=1,6
          IPICK(I)=0
          YMAX(I)=-10000.
          YMIN(I)=10000.
  101   CONTINUE
        IPP=0
        INEW=1
      elseif(ino.eq.15)then
C Help.
        helptopic='clm_graph_facilities'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('clm graphics',nbhelp,'-',0,0,IER)
      else
        ino=-1
        goto 6
      endif
      goto 7

      END


C ************ CLMDRW
C 'CLMDRW' displays the axis and draws the selected weather data
C over the specified period. onleft and onright are passed in to
C indicate how much space is available for the graph (i.e. what
C axis that do not need to be drawn)
C season (1 char) '-' do not bother 's' indicate season.
      SUBROUTINE CLMDRW(IPICK,mxmn,onleft,onright,season)

C Manual inclusion of a parameter from building.h
      parameter (MSPS=10)
      parameter (MIPVA=MSPS)

#include "seasons.h"
C seasons.h provides typper and typsea

      PARAMETER (MT=24)
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

C THE FOLLOWING COMMON BLOCK CONTAINS THE SCALE AND pixel offsets
C OF THE X-AXIS AND Y-AXIS AS COMPUTED IN 'CLMXMN' - THIS INFORMATION
C IS USED TO SET THE SCALE FOR DRAWING THE CHOSEN DISTRIBUTIONS.
      COMMON/CGSC/XSC,YSC(6),Xadd,Yadd(6)
      COMMON/MXMNC/YMAX(6),YMIN(6),TMAX,TMIN,DT
      COMMON/DAT24/PT24(6)

      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)

      character season*1
      logical mxmn,colok
      integer onleft  ! number of left axis
      integer onright ! number of right axis
      integer months
#ifdef OSI
      integer iupdown,isym,iicol    ! passed to etplot
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iix,iiy
#else
      integer*8 iupdown,isym,iicol   ! passed to etplot
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iix,iiy
#endif

C The 6 dimensions are as follows:
C  1= diff solar, 2= db temperature, 3 = dir solar rad,
C  4= wind speed, 5= wind direction, 6= rh
      DIMENSION IPICK(6)
      DIMENSION IPEN(6),MARK(6)
      DATA IPEN/-5,1,-5,-105,-105,-204/
      DATA MARK/8,6,1,4,5,3/

C Check if can draw in colour.
      colok=.false.
      if(nzonec.ge.6)colok=.true.

C Cast variables for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh

C HEADER INFORMATION
      imh=menuchw
      CALL GHEAD(imh,onleft,onright)

C Initiate display of axes (where dx is the X-axis tick interval).
C If we are displaying more than 4 months use day ticks.
      if(mxmn)CALL CLMXMN
      months=im2-im1
      if(months.ge.5)then
        CALL DCAXES(IPICK,onleft,onright,'w')
      elseif(months.ge.2.and.months.lt.5)then
        CALL DCAXES(IPICK,onleft,onright,'d')
      else
        CALL DCAXES(IPICK,onleft,onright,'-')
      endif

C NOW DRAW DISTRIBUTIONS.
      DO 30 J=IDS,IDF

C READ VALUES OF THE CLIMATIC PARAMETERS FOR THIS DAY.
        ID=J
        CALL CLMGET(ID)
        KS=0
        KF=24
        IF(J.EQ.IDS)KS=IT1
        IF(J.EQ.IDF)KF=IT2

C Process each of the axis where ipck is non-zero.
        DO 20 I=1,6
          IF(IPICK(I).EQ.0)GO TO 20
          if(colok)then
            iicol=I-1
            call winscl('z',iicol)
          endif
          JPEN=IPEN(I)
          MMK=MARK(I)

C Reset the scale for each type of line.
          call linescale(iigl,Xadd,XSC,iigb,Yadd(I),YSC(I))
          
C DRAW ONE DAY FOR EACH DISTRIBUTION
          DO 40 K=KS,KF

C GET APPROPRIATE VALUES
            if(K.eq.0)then
              Y=PT24(I)
            else
              if(I.eq.1)then
                Y=CDIF(K)
              elseif(I.eq.2)then
                Y=CTMP(K)
              elseif(I.eq.3)then
                Y=CDNR(K)
              elseif(I.eq.4)then
                Y=CVEL(K)
              elseif(I.eq.5)then
                Y=CDIR(K)
              elseif(I.eq.6)then
                Y=CHUM(K)
              endif
            endif

            TIME=real(J-IDS)*24.+ real(K)
            IM=INT(AMOD(TIME,DT)+0.01)

C SHOULD DISTRIBUTION HAVE A LINE MARKING AT THIS TIME.
            MK=0
            IF(IM.EQ.0)MK=MMK

C NOW PLOT DISRTIBUTION.
            IF(K.EQ.KS)then
              iupdown=0
              isym=0
              call etplot(TIME,Y,iupdown,isym)
            else
              iupdown=JPEN
              isym=MK
              call etplot(TIME,Y,iupdown,isym)
            endif
            if(K.eq.KF)PT24(I)=Y
   40     CONTINUE
   20   CONTINUE

C If user interesed in the seasons provide some symbols along
C the top of the graph.
        if(season.eq.'s')then
          if(ID.ge.is1wins.and.ID.lt.is1winf)then
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=1
              call winscl('z',iicol) ! symbol using line colour
            endif
            call u2pixel(TIME,0.0,iix,iiy)
            if(ID.eq.is1wins+2)then
              call wstxpt(iix,igt-10,'Winter')
            endif
            call esymbol(iix,igt-5,8,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
          elseif(ID.ge.is1sprs.and.ID.lt.is1sprf)then
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=2
              call winscl('z',iicol) ! symbol using line colour
            endif
            call u2pixel(TIME,0.0,iix,iiy)
            if(ID.eq.is1sprs+2)then
              call wstxpt(iix,igt-10,'Spring')
            endif
            call esymbol(iix,igt-5,3,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
          elseif(ID.ge.is1sums.and.ID.lt.is1sumf)then
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=3
              call winscl('z',iicol) ! symbol using line colour
            endif
            call u2pixel(TIME,0.0,iix,iiy)
            if(ID.eq.is1sums+2)then
              call wstxpt(iix,igt-10,'Summer')
            endif
            call esymbol(iix,igt-5,32,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
          elseif(ID.ge.is2sprs.and.ID.lt.is2sprf)then
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=4
              call winscl('z',iicol) ! symbol using line colour
            endif
            call u2pixel(TIME,0.0,iix,iiy)
            if(ID.eq.is2sprs+2)then
              call wstxpt(iix,igt-10,'Autumn')
            endif
            call esymbol(iix,igt-5,3,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
          elseif(ID.ge.is2wins.and.ID.lt.is2winf)then
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=6
              call winscl('z',iicol) ! symbol using line colour
            endif
            call u2pixel(TIME,0.0,iix,iiy)
            if(ID.eq.is2wins+2)then
              call wstxpt(iix,igt-10,'Winter')
            endif
            call esymbol(iix,igt-5,17,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
          endif

C Typical week in each season.
          if(ID.ge.ia1wins.and.ID.lt.ia1winf)then
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=1
              call winscl('z',iicol) ! symbol using line colour
            endif
            call u2pixel(TIME,0.0,iix,iiy)
            call esymbol(iix,igt,8,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
          elseif(ID.ge.ia1sprs.and.ID.lt.ia1sprf)then
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=2
              call winscl('z',iicol) ! symbol using line colour
            endif
            call u2pixel(TIME,0.0,iix,iiy)
            call esymbol(iix,igt,8,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
          elseif(ID.ge.iasums.and.ID.lt.iasumf)then
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=3
              call winscl('z',iicol) ! symbol using line colour
            endif
            call u2pixel(TIME,0.0,iix,iiy)
            call esymbol(iix,igt,8,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
          elseif(ID.ge.ia2sprs.and.ID.lt.ia2sprf)then
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=4
              call winscl('z',iicol) ! symbol using line colour
            endif
            call u2pixel(TIME,0.0,iix,iiy)
            call esymbol(iix,igt,8,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
          elseif(ID.ge.ia2wins.and.ID.lt.ia2winf)then
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=6
              call winscl('z',iicol) ! symbol using line colour
            endif
            call u2pixel(TIME,0.0,iix,iiy)
            call esymbol(iix,igt,8,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
          endif
        endif
   30 CONTINUE
      if(colok)then
        iicol=0
        call winscl('-',iicol)
      endif
      call forceflush()

      RETURN
      END

C ************ CLMDRWD
C 'CLMDRWD' Draws one days weather data (complements data editing).
C onleft and onright passed in to indicate which axes to draw.
      SUBROUTINE CLMDRWD(IDAY,onleft,onright)
      PARAMETER (MT=24)
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

C THE FOLLOWING COMMON BLOCK CONTAINS THE SCALE AND pixel offsets
C OF THE X-AXIS AND Y-AXIS AS COMPUTED IN 'CLMXMN' - THIS INFORMATION
C IS USED TO SET THE SCALE FOR DRAWING THE CHOSEN DISTRIBUTIONS.
      COMMON/CGSC/XSC,YSC(6),Xadd,Yadd(6)
      COMMON/MXMNC/YMAX(6),YMIN(6),TMAX,TMIN,DT
      COMMON/DAT24/PT24(6)

      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)
      DIMENSION IPICK(6)
      DIMENSION IPEN(6),MARK(6)
      logical colok
      integer onleft  ! number of left axis
      integer onright ! number of right axis
#ifdef OSI
      integer iupdown,isym,iicol   ! passed to etplot
      integer iigl,iigr,iigt,iigb,iigw,iigwh
#else
      integer*8 iupdown,isym,iicol    ! passed to etplot
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
#endif

      DATA IPEN/-5,1,-5,-105,-105,-205/
      DATA MARK/8,6,1,4,5,3/

C Check if can draw in colour.
      colok=.false.
      if(nzonec.ge.6)colok=.true.

C Cast values for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh

C HEADER INFORMATION
      INEW=1
      IDS=IDAY
      IDF=IDAY
      CALL EDAYR(IDAY,ID1,IM1)
      CALL EDAYR(IDAY,ID2,IM2)
      IT1 = 1
      IT2 = 24
      IPICK(1)=1
      IPICK(2)=1
      IPICK(3)=1
      IPICK(4)=1
      IPICK(5)=1
      IPICK(6)=1
      onleft = 2      ! assume all axes to be drawn
      onright = 3

C The menu character width will have been reset before CLMDRWD
C was called.  Pass onleft and onright into ghead.
      imh=menuchw
      CALL GHEAD(imh,onleft,onright)

C NOW INITIATE DISPLAY OF AXES (DX IS TICK INTERVAL ON X-AXIS).
      CALL CLMXMN
      CALL DCAXES(IPICK,onleft,onright,'-')

C READ VALUES OF THE CLIMATIC PARAMETERS FOR THIS DAY.
      KS=0
      KF=24
      J=IDS

C FOR EACH DISTRIBUTION
      DO 20 I=1,6
        IF(IPICK(I).EQ.0)GO TO 20
        if(colok)then
          iicol=I-1
          call winscl('z',iicol)
        endif
        JPEN=IPEN(I)
        MMK=MARK(I)

C Reset the scale for each type of line.
        call linescale(iigl,Xadd,XSC,iigb,Yadd(I),YSC(I))
          
C DRAW ONE DAY FOR EACH DISTRIBUTION
        DO 40 K=KS,KF

C GET APPROPRIATE VALUES
          if(K.eq.0)then
            Y=PT24(I)
          else
            if(I.eq.1)then
              Y=CDIF(K)
            elseif(I.eq.2)then
              Y=CTMP(K)
            elseif(I.eq.3)then
              Y=CDNR(K)
            elseif(I.eq.4)then
              Y=CVEL(K)
            elseif(I.eq.5)then
              Y=CDIR(K)
            elseif(I.eq.6)then
              Y=CHUM(K)
            endif
          endif

          TIME=real(J-IDS)*24.+ real(K)
          IM=INT(AMOD(TIME,DT)+0.01)

C SHOULD DISTRIBUTION HAVE A LINE MARKING AT THIS TIME.
          MK=0
          IF(IM.EQ.0)MK=MMK

C NOW PLOT DISRTIBUTION.
          IF(K.EQ.KS)then
            iupdown=0
            isym=0
            call etplot(TIME,Y,iupdown,isym)
          else
            iupdown=JPEN
            isym=MK
            call etplot(TIME,Y,iupdown,isym)
          endif
          if(K.eq.KF)PT24(I)=Y
   40   CONTINUE
   20 CONTINUE
      if(colok)then
        iicol=0
        call winscl('-',iicol)
      endif
      call forceflush()

      RETURN
      END

C ************** DCAXES
C 'DCAXES' Draws the time axis as well as the vertical axis based on the
C current values of he array ipick and the state of onleft and onright.
C acthor (1 char) '-' standard, 'd' day, 'w' week.
      SUBROUTINE DCAXES(IPICK,onleft,onright,acthor)

      COMMON/MXMNC/YMAX(6),YMIN(6),TMAX,TMIN,DT
      COMMON/CGSC/XSC,YSC(6),Xadd,Yadd(6)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW

      DIMENSION IPICK(6),IPAX(6)
      integer onleft  ! number of left axis
      integer onright ! number of right axis
      logical colok
      character acthor*1

C Local variables to pass to edline and axiscale.
#ifdef OSI
      integer iigr35,iigr50,iigr85,iigr100,iigr135,iigt40
      integer iicol
      integer iiw1,iiw2,iiw3,iiw4
      integer iigl,iigr,iigt,iigb,iigw,iigwh
#else
      integer*8 iigr35,iigr50,iigr85,iigr100,iigr135,iigt40
      integer*8 iicol
      integer*8 iiw1,iiw2,iiw3,iiw4
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
#endif

C Check if can draw in colour.
      colok=.false.
      if(nzonec.ge.6)colok=.true.

C Cast values for horaxis.
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh

C DRAW TIME AXIS
C Pick up the data increment for the horizontal axis.

C Debug.
C      write(6,*)'dcaxes igl igr igt igb igw igwh onleft onright ',
C     &  igl,igr,igt,igb,igw,igwh,onleft,onright,' ',acthor

      call dintervalf(TMIN,TMAX,DT,NDEC,1)
      if(acthor.eq.'-')then
        call horaxis(TMIN,TMAX,iigl,iigr,iigb,Xadd,XSC,1,
     &    'Time (hours)')
      elseif(acthor.eq.'d')then
        CALL EDAY(id1,im1,isjday)
        call horaxishdw(TMIN,TMAX,iigl,iigr,iigb,Xadd,XSC,1,2,24,
     &    isjday,'Time (days)')
      elseif(acthor.eq.'w')then
        CALL EDAY(id1,im1,isjday)
        call horaxishdw(TMIN,TMAX,iigl,iigr,iigb,Xadd,XSC,1,3,168,
     &    isjday,'Time (weeks)')
      endif
      call forceflush()

C DRAW Y AXES
C SET AXES REQUIRED
      DO 30 I=2,6
        IPAX(I)=0
        IF(IPICK(I).EQ.1)IPAX(I)=1
  30  CONTINUE

C For diffuse solar use the solar axis along with direct solar.
      IF(IPICK(1).EQ.1)IPAX(3)=1
      DO 50 I=2,6
        IF(IPAX(I).EQ.0)GO TO 50

C Above each axis title draw a short section of line to match that
C of the graph to follow plus a symbol which is equivalent to
C that which is plotted with the etplot.
        if(I.eq.2)then

C Temperature axis, include solid line and large dot symbol. If there
C are two axis on the left put the label at the top. Otherwise put
C Label at the side vertically. Use pixel position 50 if there are
C two axis on left, otherwise use igl.
          if(onleft.eq.2)then
            iiw1=50
            call vrtaxis(YMIN(I),YMAX(I),iiw1,iigb,iigt,Yadd(I),
     &        YSC(I),1,0,':')
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=I-1 
              call winscl('z',iicol) ! symbol using line colour
            endif
            call wstxpt(25,igt-22,'Temp')
            call wstxpt(25,igt-10,'deg.C')
            call eswline(25,igt-40,60,igt-40)
            call esymbol(46,igt-40,8,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
            call forceflush()
          elseif(onleft.eq.1)then
            call vrtaxis(YMIN(I),YMAX(I),iigl,iigb,iigt,Yadd(I),
     &        YSC(I),1,0,'Temp degC')
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=I-1 
              call winscl('z',iicol) ! symbol using line colour
            endif
            call wstxpt(25,igt-22,'Temp')
            call eswline(25,igt-40,60,igt-40)
            call esymbol(46,igt-40,8,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
            call forceflush()
          endif
        elseif(I.eq.3)then

C Solar radiation axis, include dashed line and symbol.
          call vrtaxis(YMIN(I),YMAX(I),iigr,iigb,iigt,Yadd(I),
     &      YSC(I),1,1,':')
          if(IFS.ne.IMFS)call winfnt(IFS)
          if(colok)then
            iicol=I-1 
            call winscl('z',iicol) ! symbol using line colour
          endif
          call wstxpt(igr,igt-22,'S.Rad')
          call wstxpt(igr,igt-10,'W/m^2')
          iigr=igr; iiw1=igt-40; iis2=igr+35; iiw3=igt-40;iiw4=5
          call edash(iigr,iiw1,iis2,iiw3,iiw4)
          call esymbol(igr+22,igt-40,9,0)
          if(IFS.ne.IMFS)call winfnt(IMFS)
          if(colok)then
            iicol=0
            call winscl('-',iicol)
          endif
          call forceflush()
        elseif(I.eq.4)then

C Wind speed axis, include chained line and symbol. If two axes on
C left put label at top, otherwise put along the left.
          if(onleft.eq.2)then
            call vrtaxis(YMIN(I),YMAX(I),iigl,iigb,iigt,Yadd(I),
     &        YSC(I),1,0,':')
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=I-1 
              call winscl('z',iicol) ! symbol using line colour
            endif
            call wstxpt(igl,igt-22,'Wnd sp')
            call wstxpt(igl,igt-10,'m/s')
            call echain(igl,igt-40,igl+35,igt-40,5)
            call esymbol(igl+22,igt-40,0,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
            call forceflush()
          elseif(onleft.eq.1)then
            call vrtaxis(YMIN(I),YMAX(I),iigl,iigb,iigt,Yadd(I),
     &        YSC(I),1,0,'Wind speed m/s')
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=I-1 
              call winscl('z',iicol) ! symbol using line colour
            endif
            call wstxpt(igl,igt-22,'Wnd sp')
            call echain(igl,igt-40,igl+35,igt-40,5)
            call esymbol(igl+22,igt-40,0,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
            call forceflush()
          endif
        elseif(I.eq.5)then

C Wind direction axis, include chained line and symbol.
          if(onright.eq.1)then
            call vrtaxis(YMIN(I),YMAX(I),iigr,iigb,iigt,Yadd(I),
     &        YSC(I),1,1,':')
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=I-1 
              call winscl('z',iicol) ! symbol using line colour
            endif
            call wstxpt(igr,igt-22,'W dir')
            call wstxpt(igr,igt-10,'deg ')
            call echain(igr,igt-40,igr+35,igt-40,5)
            call esymbol(igr+22,igt-40,1,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
            call forceflush()
          elseif(onright.eq.2)then
            if(ipick(6).eq.1)then

C If the other is RH then use igr for this one.
              call vrtaxis(YMIN(I),YMAX(I),iigr,iigb,iigt,Yadd(I),
     &          YSC(I),1,1,':')
              if(IFS.ne.IMFS)call winfnt(IFS)
              if(colok)then
                iicol=I-1 
                call winscl('z',iicol) ! symbol using line colour
              endif
              call wstxpt(igr,igt-22,'W dir')
              call wstxpt(igr,igt-10,'deg ')
              call echain(igr,igt-40,igr+35,igt-40,5)
              call esymbol(igr+22,igt-40,1,0)
              if(IFS.ne.IMFS)call winfnt(IMFS)
              if(colok)then
                iicol=0
                call winscl('-',iicol)
              endif
              call forceflush()
            elseif(ipick(1).eq.1.or.ipick(3).eq.1)then

C If the other is solar then use igr_50 for this one.
              iiw1=iigr+50
              call vrtaxis(YMIN(I),YMAX(I),iiw1,iigb,iigt,Yadd(I),
     &          YSC(I),1,1,':')
              if(IFS.ne.IMFS)call winfnt(IFS)
              if(colok)then
                iicol=I-1 
                call winscl('z',iicol) ! symbol using line colour
              endif
              call wstxpt(igr+50,igt-22,'W dir')
              call wstxpt(igr+50,igt-10,'deg ')
              call echain(igr+50,igt-40,igr+85,igt-40,5)
              call esymbol(igr+72,igt-40,1,0)
              if(IFS.ne.IMFS)call winfnt(IMFS)
              if(colok)then
                iicol=0
                call winscl('-',iicol)
              endif
              call forceflush()
            endif
          elseif(onright.eq.3)then
            iiw1=iigr+50
            call vrtaxis(YMIN(I),YMAX(I),iiw1,iigb,iigt,Yadd(I),
     &        YSC(I),1,1,':')
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=I-1
              call winscl('z',iicol) ! symbol using line colour
            endif
            call wstxpt(igr+50,igt-22,'W dir')
            call wstxpt(igr+50,igt-10,'deg ')
            call echain(igr+50,igt-40,igr+85,igt-40,5)
            call esymbol(igr+72,igt-40,1,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
            call forceflush()
          endif
        elseif(I.eq.6)then

C RH axis, include dotted line and symbol. If there are 3 axis on the
C right then place far to right (+100), if there is one on the right
C use igr. If there are two axis then set right-most position.
          if(onright.eq.3)then 
            iiw1=iigr+100
            call vrtaxis(YMIN(I),YMAX(I),iiw1,iigb,iigt,Yadd(I),
     &        YSC(I),1,1,':')
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=I-1 
              call winscl('z',iicol) ! symbol using line colour
            endif
            call wstxpt(igr+100,igt-22,'RH %')
            iigr100=igr+100
            iigr135=igr+135
            iigt40=igt-40
            call edline(iigr100,iigt40,iigr135,iigt40,5)
            call esymbol(igr+120,igt-40,12,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
            call forceflush()
          elseif(onright.eq.2)then

C If solar or wind direction is the other then use the +50 slot.
            iiw1=iigr+50
            call vrtaxis(YMIN(I),YMAX(I),iiw1,iigb,iigt,Yadd(I),
     &        YSC(I),1,1,':')
            if(IFS.ne.IMFS)call winfnt(IFS)
            if(colok)then
              iicol=I-1 
              call winscl('z',iicol) ! symbol using line colour
            endif
            call wstxpt(igr+50,igt-22,'RH %')
            iigr50=igr+50
            iigr85=igr+85
            iigt40=igt-40
            call edline(iigr50,iigt40,iigr85,iigt40,5)
            call esymbol(igr+70,igt-40,12,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
            call forceflush()
          elseif(onright.eq.1)then
            call vrtaxis(YMIN(I),YMAX(I),iigr,iigb,iigt,Yadd(I),
     &        YSC(I),1,1,'RH %')
            if(colok)then
              iicol=I-1 
              call winscl('z',iicol) ! symbol using line colour
            endif
            call wstxpt(igr,igt-22,'RH ')
            iigr=igr
            iigt40=igt-40
            iigr35=igr+35
            call edline(iigr,iigt40,iigr35,iigt40,5)
            call esymbol(igr+20,igt-40,12,0)
            if(IFS.ne.IMFS)call winfnt(IMFS)
            if(colok)then
              iicol=0
              call winscl('-',iicol)
            endif
            call forceflush()
          endif
        endif
  50  CONTINUE
      RETURN
      END

C *********** PCGRF  *************
C  Subroutine PCGRF  will draw the predicted climatic data (for
C  data type icv) and optionally for previous data values (icl)
C  for the time period IDS to IDF as defined in subroutine PSAVE.
C ICL is zero instructs the graphing to omit the previous
C   value (7th index of CLMV) of the selected data. If ICL is
C   positive then draw the current and previous values. If ICL 
C   is negative draw only the past value.

      SUBROUTINE PCGRF(ICV,ICL,IDS,IDF)
      PARAMETER (MD=365,MT=24)
      COMMON/CLIM1/CLMV(7,MD,MT),ICMAX,ICMAX2,ICMIN
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iupdown,isym    ! passed to etplot
      integer iicol
      integer iigl,iigr,iigt,iigb,iigw,iigwh
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iicol
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
#endif
      CHARACTER*10 CLMVAR(4),CVLAB
      logical close,colok
      DATA CLMVAR/'S rad W/m2','Temp Deg.C','Rel hum pc',
     &            'Wind s m/s'/

C Check if can draw in colour.
      colok=.false.
      if(nzonec.ge.6)colok=.true.

C Cast values for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh

      ICVS = ICV
      CVLAB=CLMVAR(ICVS)

C  Draw the time axis. First establish how many hours are to be defined.
      THOUR=(IDF-IDS+1)*24.0
      TMIN=0.0
      TMAX=THOUR

C  Draw the axis for the climatic variable and then draw the graph.
      A=ICMAX-ICMIN
      call eclose(A,0.0,0.001,close)
      IF(close)A=20.0
      CMAX=ICMAX+A/20.0
      CMIN=ICMIN-A/20.0
      IF(ICVS.EQ.1)CMIN=0.0

C Draw axis, begin with scaling factors for the X & Y axis.
C Stuff this into static memory.
      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,TMIN,TMAX,CMIN,CMAX,XSC,YSC,sca,
     &              Xaddd,Yaddd)
      call linescale(iigl,Xaddd,XSC,iigb,Yaddd,YSC)

      call dintervalf(TMIN,TMAX,DX,NDEC,1)
      call horaxis(TMIN,TMAX,iigl,iigr,iigb,Xaddd,XSC,0,'Time (hrs)')


C Draw the vertical axis.
      call vrtaxis(CMIN,CMAX,iigl,iigb,iigt,Yaddd,YSC,0,0,CVLAB)

C If the previous data is to be drawn. Loop.
      if(icl.ne.0)then
        if(colok)then
          iicol=2
          call winscl('z',iicol)
        endif
        START=TMIN+1.0
        iupdown=0
        isym=0
        call etplot(START,CLMV(7,IDS,1),iupdown,isym)
        IHOUR=0
        iupdown=1
        DO 100 I=IDS,IDF
          JJ=IHOUR
          DO 200 J=1,24
            IHOUR=JJ+J
            call etplot(real(IHOUR),CLMV(7,I,J),iupdown,isym)
  200     CONTINUE
  100   CONTINUE
        if(colok)then
          iicol=0
          call winscl('-',iicol)
        endif
        call forceflush()
        if(icl.lt.0)return
      endif

C  Draw graphs of either solar radiation,temperature,
C  realtive humidity or wind velocity and direction.
      START=TMIN+1.0
    7 iupdown=0
      isym=0
      call etplot(START,CLMV(ICVS,IDS,1),iupdown,isym)
      IHOUR=0
      iupdown=1
      DO 10 I=IDS,IDF
        JJ=IHOUR
        DO 20 J=1,24
          IHOUR=JJ+J
          call etplot(real(IHOUR),CLMV(ICVS,I,J),iupdown,isym)
   20   CONTINUE
   10 CONTINUE
      GOTO (3,1111,1111,1111,5,6),ICVS
    3 CONTINUE
      ICVS = 5
      GOTO 7
    5 ICVS=1
      GOTO 1111
    6 ICVS=4
 1111 call forceflush()
      RETURN
      END
