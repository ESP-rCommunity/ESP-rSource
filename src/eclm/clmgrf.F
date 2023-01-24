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
#include "climate.h"
#include "epara.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
C      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/MXMNC/YMAX(MCM),YMIN(MCM),TMAX,TMIN,DT
      COMMON/RADTYP/IDNGH
      COMMON/CGSC/XSC,YSC(MCM),Xadd,Yadd(MCM)
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      CHARACTER*26 ITEM(30)
      character hold*72
      DIMENSION IPICK(MCM)
      LOGICAL mxmn
      integer onleft  ! number of left axis
      integer onright ! number of right axis
      integer NITMS,INO  ! max items and current menu item

      integer menumetric(MCM)
      character KEY*1

#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

      helpinsub='clmgrf'  ! set for cfiles

      onleft = 0      ! initial assumption of no axis
      onright = 0
      mxmn=.true.
      labelfont=4  ! small proportional font

      MHEAD=2
      MCTL=7
      IPACT=CREATE
      ILEN=NCM
      CALL EKPAGE(IPACT)

      ITEM(1) =  '1 set period              '
      ITEM(2) =  '  _______________________ '

      M=MHEAD
      I=0
      do imet=1,MCM
        if (CMXST(imet)) then
          I=I+1
          menumetric(I)=imet
          IF(I.GE.IST.AND.(I.LE.(IST+MIFULL)))THEN
            M=M+1
            call EMKEY(I,KEY,IER)
            write (ITEM(M),'(a,1x,a)') KEY,CMNAMF(imet)
          ENDIF
        endif
      enddo

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        ITEM(M+1)='  _______________________ '
      ELSE
        WRITE(ITEM(M+1),'(a8,i2,a4,i2,a)')'0 Page: ',IPM,' of ',MPM,
     &    ' ________ '
      ENDIF

      ITEM(M+2)=  '! draw graph              '
      ITEM(M+3)=  '^ reset axis scale        '
      ITEM(M+4)=  '* clear current items     '
      ITEM(M+5)=  '  _______________________ '
      ITEM(M+6)=  '? help                    '
      ITEM(M+7)=  '- exit                    '
  
C Clear the selection list.
      DO 10 I=1,MCM
        IPICK(I)=0
   10 CONTINUE
      IPP=0

C Number of actual items displayed.
      NITMS=M+MCTL

C Display graphing options.
    7 INO=-2

      call usrmsg('  ','  ','-')
    6 CALL EMENU(' Graphical analysis',ITEM,NITMS,INO)
      
      if(ino.eq.nitms)then
        return
      elseif(ino.eq.1)then
        CALL selper(ier)
        mxmn=.true.
      ELSEIF(INO.gt.MHEAD.and.INO.le.(NITMS-MCTL))THEN

C Select a weather metric.
        CALL KEYIND(NITMS,INO,imenu,IO)
        IP=menumetric(imenu)
        IPICK(IP)=1
        IPP=1
        mxmn=.true.
      elseif (INO.eq.(NITMS-6)) then
 
C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF

      elseif(ino.eq.NITMS-5)then

C Draw the selected data (the non-zero ipick values). Begin by
C establishing the number of axis on left and then the right.
C Currently, temperature, wind speed, and pressure go on the left,
C others go on the right.
        IF(IPP.EQ.1)then

          onleft=0
          if (IPICK(1).eq.1) onleft=onleft+1
          if (IPICK(5).eq.1) onleft=onleft+1
          if (IPICK(10).eq.1) onleft=onleft+1

          onright=0
          if (IPICK(2).eq.1 .or. IPICK(3).eq.1 .or. IPICK(4).eq.1) 
     &      onright=onright+1
          if (IPICK(6).eq.1) onright=onright+1
          if (IPICK(7).eq.1) onright=onright+1
          if (IPICK(8).eq.1 .or. IPICK(9).eq.1) onright=onright+1

C Take into account which axis needs to be drawn as in esrures/tgraph.F.
          lastbuttonfont=IFS
          call winfnt(labelfont)
          iiw1=1; iiw2=4; iiw3=6; iiw4=3; iimenu=menuchw
          iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &          iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)

          call winfnt(lastbuttonfont)
          call startbuffer()

C Debug.
C          write(6,'(a,6i5)')'clmgrf igl igr igt igb igw igwh',
C     &      igl,igr,igt,igb,igw,igwh

          CALL CLMDRW(IPICK,mxmn,onleft,onright,'-')
          mxmn=.false.
          call usrmsg('  ','  ','-')
          goto 6
        else
          call edisp(iuout,' Nothing to draw yet ! ')
        endif
      elseif(ino.eq.NITMS-4)then

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
  42    CALL EASKS(HOLD,' Max & Min for db temp & solar radiation:',
     &     ' : ',72,' 800. 0. 40. 0. ','1st axis mx mn',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,YMAX(1),0.,0.,'-','db temp mx',IER)
        CALL EGETWR(HOLD,K,YMIN(1),0.,0.,'-','db temp mn',IER)
        CALL EGETWR(HOLD,K,YMAX(2),0.,0.,'-','solar mx',IER)
        CALL EGETWR(HOLD,K,YMIN(2),0.,0.,'-','solar mn',IER)
        if(ier.ne.0)goto 42
        YMAX(3)=YMAX(2); YMAX(4)=YMAX(2)
        YMIN(3)=YMIN(2); YMIN(4)=YMIN(2)

C Second set of axes.
        WRITE(HOLD,'(1x,6f7.1)')YMAX(5),YMIN(5),YMAX(6),YMIN(6),
     &    YMAX(7),YMIN(7)
        helptopic='clm_wind_graph'
        call gethelptext(helpinsub,helptopic,nbhelp)
  43    CALL EASKS(HOLD,' Max & Min for wind speed, wind dir, rh:',' :',
     &     72,' 10. 0. 360. 0. 100. 0.','2nd axis mx mn',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,YMAX(5),0.,0.,'-','wind spd mx',IER)
        CALL EGETWR(HOLD,K,YMIN(5),0.,0.,'-','wind spd mn',IER)
        CALL EGETWR(HOLD,K,YMAX(6),0.,360.,'W','wind dir mx',IER)
        CALL EGETWR(HOLD,K,YMIN(6),0.,360.,'W','wind dir mn',IER)
        CALL EGETWR(HOLD,K,YMAX(7),0.,100.,'W','rh mx',IER)
        CALL EGETWR(HOLD,K,YMIN(7),0.,100.,'W','rh mn',IER)
        if(ier.ne.0)goto 43

C Third set of axes.
        if (CFVER.gt.1) then
          WRITE(HOLD,'(1x,4f10.1)')YMAX(8),YMIN(8),YMAX(10),YMIN(10)
          helptopic='clm_wind_graph'
          call gethelptext(helpinsub,helptopic,nbhelp)
  44      CALL EASKS(HOLD,' Max & Min for cloud cover & pressure:',' :',
     &       72,' 10. 0. 100000. 50000.','3rd axis mx mn',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,YMAX(8),0.,10.,'W','cld cvr mx',IER)
          CALL EGETWR(HOLD,K,YMIN(8),0.,10.,'W','cld cvr mn',IER)
          CALL EGETWR(HOLD,K,YMAX(10),0.,0.,'-','pressure mx',IER)
          CALL EGETWR(HOLD,K,YMIN(10),0.,0.,'-','pressure mn',IER)
          if(ier.ne.0)goto 44
          YMAX(9)=YMAX(8)
          YMIN(9)=YMIN(8)

C << Add axes for more variables here >>

C           itmp=MCM
        else
C           itmp=7
        endif

        mxmn=.FALSE.
        CALL CLMDRW(IPICK,mxmn,onleft,onright,'-')
        goto 6

      elseif(ino.eq.NITMS-3)then

C Clear selections.
        DO 101 I=1,MCM
          IPICK(I)=0
          YMAX(I)=-10000.
          YMIN(I)=10000.
  101   CONTINUE
        IPP=0
        INEW=1
      elseif(ino.eq.NITMS-1)then
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

#include "building.h"
#include "seasons.h"
C seasons.h provides typper and typsea
#include "climate.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

C THE FOLLOWING COMMON BLOCK CONTAINS THE SCALE AND pixel offsets
C OF THE X-AXIS AND Y-AXIS AS COMPUTED IN 'CLMXMN' - THIS INFORMATION
C IS USED TO SET THE SCALE FOR DRAWING THE CHOSEN DISTRIBUTIONS.
      COMMON/CGSC/XSC,YSC(MCM),Xadd,Yadd(MCM)
      COMMON/MXMNC/YMAX(MCM),YMIN(MCM),TMAX,TMIN,DT
      COMMON/DAT24/PT24(MCM)

      character season*1
      logical mxmn,colok
      integer onleft  ! number of left axis
      integer onright ! number of right axis
      integer months
#ifdef OSI
      integer iupdown,isym,iicol    ! passed to etplot
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iix,iiy,iiz
#else
      integer*8 iupdown,isym,iicol   ! passed to etplot
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iix,iiy,iiz
#endif

C For indices mapping see climate.h
      DIMENSION IPICK(MCM)
      DIMENSION IPEN(MCM),MARK(MCM)
      DATA IPEN/1,-5,-5,-5,-105,-105,-205,1,1,-205/
      DATA MARK/1,7,8,9,4,5,6,12,13,14/

C Check if can draw in colour.
      colok=.false.
      if(nzonec.ge.6)colok=.true.

C Cast variables for linescale.
C Add 50 pixels for each axis on left, and take away 50 pixels for each
C axis on right.
      iigl=igl+50*onleft; iigr=igr-50*onright
      iigw=igw-50*(onleft+onright)
      iigt=igt; iigb=igb; iigwh=igwh

C HEADER INFORMATION
      imh=menuchw
      CALL GHEAD(imh,onleft,onright)

C Initiate display of axes (where dx is the X-axis tick interval).
C If we are displaying more than 4 months use day ticks.
      if(mxmn)CALL CLMXMN

      do i=1,MCM
        if (IPICK(i).eq.1) then
          call axiscale(iigw,iigwh,TMIN,TMAX,YMIN(I),YMAX(I),
     &      XSC,bysc,sca,Xadd,addY)
          YSC(I)=bysc
          Yadd(I)=addY
        endif
      enddo

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
        CALL CLMGET(ID,IER)
        KS=0
        KF=24
        IF(J.EQ.IDS)KS=IT1
        IF(J.EQ.IDF)KF=IT2

C Process each of the axis where ipck is non-zero.
        DO 20 I=1,MCM
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
              Y=CMRVAL(I,K)
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
              iiz=igt-10
              call textatxy(iix,iiz,'Winter','z',iicol)
            endif
            iiz=igt-5; isym=8
            if(mmod.eq.8)then
              call esymbol(iix,iiz,isym,1)
            else
              call esymbolwwc(iix,iiz,isym,1)
            endif
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
              iiz=igt-10
              call textatxy(iix,iiz,'Spring','z',iicol)
            endif
            iiz=igt-5; isym=3
            if(mmod.eq.8)then
              call esymbol(iix,iiz,isym,0)
            else
              call esymbolwwc(iix,iiz,isym,0)
            endif
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
              iiz=igt-10
              call textatxy(iix,iiz,'Summer','z',iicol)
            endif
            iiz=igt-5; isym=8
            if(mmod.eq.8)then
C             call esymbol(iix,iiz,32,0)
              call esymbol(iix,iiz,isym,0)
            else
              call esymbolwwc(iix,iiz,isym,0)
            endif
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
              iiz=igt-10
              call textatxy(iix,iiz,'Autumn','z',iicol)
            endif
            iiz=igt-5; isym=3
            if(mmod.eq.8)then
              call esymbol(iix,iiz,isym,0)
            else
              call esymbolwwc(iix,iiz,isym,0)
            endif
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
              iiz=igt-10
              call textatxy(iix,iiz,'Winter','z',iicol)
            endif
            iiz=igt-5; isym=17
            if(mmod.eq.8)then
              call esymbol(iix,iiz,isym,0)
            else
              call esymbolwwc(iix,iiz,isym,0)
            endif
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
            iiz=igt; isym=8
            if(mmod.eq.8)then
              call esymbol(iix,iiz,isym,1)
            else
              call esymbolwwc(iix,iiz,isym,1)
            endif
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
            iiz=igt; isym=8
            if(mmod.eq.8)then
              call esymbol(iix,iiz,isym,1)
            else
              call esymbolwwc(iix,iiz,isym,1)
            endif
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
            iiz=igt; isym=8
            if(mmod.eq.8)then
              call esymbol(iix,iiz,isym,1)
            else
              call esymbolwwc(iix,iiz,isym,1)
            endif
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
            iiz=igt; isym=8
            if(mmod.eq.8)then
              call esymbol(iix,iiz,isym,1)
            else
              call esymbolwwc(iix,iiz,isym,1)
            endif
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
            iiz=igt; isym=8
            if(mmod.eq.8)then
              call esymbol(iix,iiz,isym,1)
            else
              call esymbolwwc(iix,iiz,isym,1)
            endif
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
      if(mmod.eq.8) call forceflush()

      RETURN
      END

C ************** DCAXES
C 'DCAXES' Draws the time axis as well as the vertical axis based on the
C current values of he array ipick and the state of onleft and onright.
C acthor (1 char) '-' standard, 'd' day, 'w' week.
      SUBROUTINE DCAXES(IPICK,onleft,onright,acthor)
#include "climate.h"

      COMMON/MXMNC/YMAX(MCM),YMIN(MCM),TMAX,TMIN,DT
      COMMON/CGSC/XSC,YSC(MCM),Xadd,Yadd(MCM)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW

      DIMENSION IPICK(MCM),IPAX(MCM),MARK(MCM)
      integer onleft  ! number of left axis
      integer onright ! number of right axis
      logical colok
      character acthor*1
      integer labelfont

C These do not match values in CLMDRW because those values are filtered
C through etplot, which has its own conventions and maps values to
C esymbol convections.
C These values are passed straight to esymbol.
      DATA MARK/9,4,5,6,0,1,8,14,15,3/

C Local variables to pass to edline and axiscale.
#ifdef OSI
      integer iigr35,iigr50,iigr85,iigr100,iigr135,iigt40
      integer iicol,isize,isym
      integer iiw1,iiw2,iiw3,iiw4
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iix,iiy,iiz
#else
      integer*8 iigr35,iigr50,iigr85,iigr100,iigr135,iigt40
      integer*8 iicol,isize,isym
      integer*8 iiw1,iiw2,iiw3,iiw4
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iix,iiy,iiz
#endif

C Check if can draw in colour.
      colok=.false.
      if(nzonec.ge.6)colok=.true.
      labelfont=4  ! small proportional font
      isize=0

C Cast values for horaxis.
C Add 50 pixels for each axis on left, and take away 50 pixels for each
C axis on right.
      iigl=igl+50*onleft; iigr=igr-50*onright
      iigw=igw-50*(onleft+onright)
      iigt=igt; iigb=igb; iigwh=igwh      

C DRAW TIME AXIS
C Pick up the data increment for the horizontal axis.

C Debug.
C      write(6,'(a,8i5,2a)')
C     &  'dcaxes igl igr igt igb igw igwh onleft onright ',
C     &  igl,igr,igt,igb,igw,igwh,onleft,onright,' ',acthor

      call dintervalf(TMIN,TMAX,DT,NDEC,1)
      if(acthor.eq.'-')then
        CALL dintervalf(TMIN,TMAX,ddy1,ny,1)
        CALL horaxisdd(TMIN,TMAX,iigl,iigr,iigb,xadd,XSC,1,ddy1,ny,
     &  'Time (hours)')
      elseif(acthor.eq.'d')then
        CALL EDAY(id1,im1,isjday)
        CALL dintervalf(TMIN,TMAX,ddy1,ny,1)
        call horaxishdwdd(TMIN,TMAX,iigl,iigr,iigb,Xadd,XSC,1,
     &    ddy1,ny,2,24,isjday,'Time (days)')
      elseif(acthor.eq.'w')then
        CALL EDAY(id1,im1,isjday)
        CALL dintervalf(TMIN,TMAX,ddy1,ny,1)
        call horaxishdwdd(TMIN,TMAX,iigl,iigr,iigb,Xadd,XSC,1,
     &    ddy1,ny,3,168,isjday,'Time (weeks)')
      endif
      call forceflush()

C DRAW Y AXES
C SET AXES REQUIRED
      DO 30 I=1,MCM
        IPAX(I)=IPICK(I)
  30  CONTINUE

C Ensure only 1 entry of IPAX is 1 for solar metrics.
      if (IPAX(2).eq.1 .or. IPAX(3).eq.1 .or. IPAX(4).eq.1) then
        IPAX(2)=1; IPAX(3)=0; IPAX(4)=0
      endif

C Ensure only 1 entry of IPAX is 1 for cloud metrics.
      if (IPAX(8).eq.1 .or. IPAX(9).eq.1) then
        IPAX(8)=1; IPAX(9)=0
      endif

      DO 50 I=1,MCM
        IF(IPAX(I).EQ.0)GO TO 50

C Above each axis title draw a short section of line to match that
C of the graph to follow plus a symbol which is equivalent to
C that which is plotted with the etplot.
C On the left, if there are more than one axes put label at top,
C otherwise put vertically along the axis.
C On the right always put label at top.

C Temperature axis, takes first position on left.
        if(I.eq.1)then
          iiw1=iigl
          call dintervalf(YMIN(I),YMAX(I),DY,NDEC,1)
          if (onleft.gt.1) then
            call vrtaxisdd(YMIN(I),YMAX(I),iiw1,iigb,iigt,Yadd(I),
     &        YSC(I),1,DY,NDEC,0,' ')
          else
            call vrtaxisdd(YMIN(I),YMAX(I),iiw1,iigb,iigt,Yadd(I),
     &        YSC(I),1,DY,NDEC,0,'Temp degC')
          endif
          if(colok)then
            iicol=I-1 
            call winscl('z',iicol) ! symbol using line colour
          endif
          iiw1=iiw1-20
          iix=iiw1; iiz=igt-22
          lastbuttonfont=IFS
          call textsizeatxy(iix,iiz,'Temp',isize,'z',iicol)
          if (onleft.gt.1) then
            iiz=igt-10
            call textsizeatxy(iix,iiz,'deg.C',isize,'z',iicol)
          endif
          iiw2=igt-40; iiw3=iiw1+35
          if(mmod.eq.8)then
            call eswline(iiw1,iiw2,iiw3,iiw2)
          else
            call eswlinewwc(iiw1,iiw2,iiw3,iiw2)
          endif
          iix=iiw1+15; iiz=iiw2
          isym=MARK(I)
          if(mmod.eq.8)then
            call esymbol(iix,iiz,isym,1)
          else
            call esymbolwwc(iix,iiz,isym,1)
          endif
          call winfnt(lastbuttonfont)
          if(colok)then
            iicol=0
            call winscl('-',iicol)
          endif
          if(mmod.eq.8) call forceflush()

C Solar radiation axis, first position on right.
        elseif(I.eq.2)then
          iiw1=iigr
          call dintervalf(YMIN(I),YMAX(I),DY,NDEC,1)
          call vrtaxisdd(YMIN(I),YMAX(I),iiw1,iigb,iigt,Yadd(I),
     &      YSC(I),1,DY,NDEC,1,' ')
          if(colok)then
            iicol=I-1 
            call winscl('z',iicol) ! symbol using line colour
          endif
          iix=iiw1; iiz=igt-22
          lastbuttonfont=IFS
          call textsizeatxy(iix,iiz,'S.Rad',isize,'z',iicol)
          iiz=igt-10
          call textsizeatxy(iix,iiz,'W/m^2',isize,'z',iicol)
          iiw2=igt-40; iiw3=iiw1+35
          call edash(iiw1,iiw2,iiw3,iiw2,5)
          iix=iiw1+15; iiz=iiw2
          isym=MARK(I)
          if(mmod.eq.8)then
            call esymbol(iix,iiz,isym,1)
          else
            call esymbolwwc(iix,iiz,isym,1)
          endif
          call winfnt(lastbuttonfont)
          if(colok)then
            iicol=0
            call winscl('-',iicol)
          endif
          if(mmod.eq.8) call forceflush()

C Wind speed axis, after temperature on left.
        elseif(I.eq.5)then
          iiw1=iigl
          if (IPAX(1).eq.1) iiw1=iigl-50
          call dintervalf(YMIN(I),YMAX(I),DY,NDEC,1)
          if (onleft.gt.1) then
            call vrtaxisdd(YMIN(I),YMAX(I),iiw1,iigb,iigt,Yadd(I),
     &        YSC(I),1,DY,NDEC,0,' ')
          else
            call vrtaxisdd(YMIN(I),YMAX(I),iiw1,iigb,iigt,Yadd(I),
     &        YSC(I),1,DY,NDEC,0,'Wind speed m/s')
          endif
          if(colok)then
            iicol=I-1 
            call winscl('z',iicol) ! symbol using line colour
          endif
          iiw1=iiw1-20
          iix=iiw1; iiz=igt-22
          lastbuttonfont=IFS
          call textsizeatxy(iix,iiz,'Wnd sp',isize,'z',iicol)
          if (onleft.gt.1) then
            iiz=igt-10
            call textsizeatxy(iix,iiz,'m/s',isize,'z',iicol)
          endif
          iiw2=igt-40; iiw3=iiw1+35
          call echain(iiw1,iiw2,iiw3,iiw2,5)
          iix=iiw1+15; iiz=iiw2
          isym=MARK(I)
          if(mmod.eq.8)then
            call esymbol(iix,iiz,isym,1)
          else
            call esymbolwwc(iix,iiz,isym,1)
          endif
          call winfnt(lastbuttonfont)
          if(colok)then
            iicol=0
            call winscl('-',iicol)
          endif
          if(mmod.eq.8) call forceflush()

C Wind direction axis, after solar on right.          
        elseif(I.eq.6)then
          iiw1=iigr
          if (IPAX(2).eq.1) iiw1=iiw1+50
          call dintervalf(YMIN(I),YMAX(I),DY,NDEC,1)
          call vrtaxisdd(YMIN(I),YMAX(I),iiw1,iigb,iigt,Yadd(I),
     &      YSC(I),1,DY,NDEC,1,' ')
          if(colok)then
            iicol=I-1 
            call winscl('z',iicol) ! symbol using line colour
          endif
          iix=iiw1; iiz=igt-22
          lastbuttonfont=IFS
          call textsizeatxy(iix,iiz,'W dir',isize,'z',iicol)
          iiz=igt-10
          call textsizeatxy(iix,iiz,'deg CW',isize,'z',iicol)
          iiw2=igt-40; iiw3=iiw1+35
          call echain(iiw1,iiw2,iiw3,iiw2,5)
          iix=iiw1+15; iiz=iiw2
          isym=MARK(I)
          if(mmod.eq.8)then
            call esymbol(iix,iiz,isym,1)
          else
            call esymbolwwc(iix,iiz,isym,1)
          endif
          call winfnt(lastbuttonfont)
          if(colok)then
            iicol=0
            call winscl('-',iicol)
          endif
          if(mmod.eq.8) call forceflush()

C RH axis, after solar and wind direction on right.
        elseif(I.eq.7)then
          iiw1=iigr
          if (IPAX(2).eq.1) iiw1=iiw1+50
          if (IPAX(6).eq.1) iiw1=iiw1+50
          call dintervalf(YMIN(I),YMAX(I),DY,NDEC,1)
          call vrtaxisdd(YMIN(I),YMAX(I),iiw1,iigb,iigt,Yadd(I),
     &      YSC(I),1,DY,NDEC,1,' ')
          if(colok)then
            iicol=I-1 
            call winscl('z',iicol) ! symbol using line colour
          endif
          iix=iiw1; iiz=igt-22
          lastbuttonfont=IFS
          call textsizeatxy(iix,iiz,'RH %',isize,'z',iicol)
          iiw2=igt-40; iiw3=iiw1+35
          if(mmod.eq.8)then
            call edline(iiw1,iiw2,iiw3,iiw2,5)
          else
            call edlinewwc(iiw1,iiw2,iiw3,iiw2,5)
          endif
          iix=iiw1+15; iiz=iiw2
          isym=MARK(I)
          if(mmod.eq.8)then
            call esymbol(iix,iiz,isym,1)
          else
            call esymbolwwc(iix,iiz,isym,1)
          endif
          call winfnt(lastbuttonfont)
          if(colok)then
            iicol=0
            call winscl('-',iicol)
          endif
          if(mmod.eq.8) call forceflush()

C Cloud cover axis, last position on right.
        elseif (I.eq.8) then
          iiw1=iigr+(onright-1)*50
          call dintervalf(YMIN(I),YMAX(I),DY,NDEC,1)
          call vrtaxisdd(YMIN(I),YMAX(I),iiw1,iigb,iigt,Yadd(I),
     &      YSC(I),1,DY,NDEC,1,' ')
          if(colok)then
            iicol=I-1 
            call winscl('z',iicol) ! symbol using line colour
          endif
          iix=iiw1; iiz=igt-22
          lastbuttonfont=IFS
          call textsizeatxy(iix,iiz,'Cloud',isize,'z',iicol)
          iiw2=igt-40; iiw3=iiw1+35
          if(mmod.eq.8)then
            call eswline(iiw1,iiw2,iiw3,iiw2)
          else
            call eswlinewwc(iiw1,iiw2,iiw3,iiw2)
          endif
          iix=iiw1+15; iiz=iiw2
          isym=MARK(I)
          if(mmod.eq.8)then
            call esymbol(iix,iiz,isym,1)
          else
            call esymbolwwc(iix,iiz,isym,1)
          endif
          call winfnt(lastbuttonfont)
          if(colok)then
            iicol=0
            call winscl('-',iicol)
          endif
          if(mmod.eq.8) call forceflush()

C Pressure axis, last position on left.
        elseif (I.eq.10) then
          iiw1=iigl-(onleft-1)*50
          call dintervalf(YMIN(I),YMAX(I),DY,NDEC,1)
          if (onleft.gt.1) then
            call vrtaxisdd(YMIN(I),YMAX(I),iiw1,iigb,iigt,Yadd(I),
     &        YSC(I),1,DY,NDEC,0,' ')
          else
            call vrtaxisdd(YMIN(I),YMAX(I),iiw1,iigb,iigt,Yadd(I),
     &        YSC(I),1,DY,NDEC,0,'Pressure Pa')
          endif
          if(colok)then
            iicol=I-1 
            call winscl('z',iicol) ! symbol using line colour
          endif
          iiw1=iiw1-20
          iix=iiw1; iiz=igt-22
          lastbuttonfont=IFS
          call textsizeatxy(iix,iiz,'Pres',isize,'z',iicol)
          if (onleft.gt.1) then
            iiz=igt-10
            call wstxpt(iix,iiz,'Pa')
          endif
          iiw2=igt-40; iiw3=iiw1+35
          if(mmod.eq.8)then
            call edline(iiw1,iiw2,iiw3,iiw2,5)
          else
            call edlinewwc(iiw1,iiw2,iiw3,iiw2,5)
          endif
          iix=iiw1+15; iiz=iiw2
          isym=MARK(I)
          if(mmod.eq.8)then
            call esymbol(iix,iiz,isym,1)
          else
            call esymbolwwc(iix,iiz,isym,1)
          endif
          call winfnt(lastbuttonfont)
          if(colok)then
            iicol=0
            call winscl('-',iicol)
          endif
          if(mmod.eq.8) call forceflush()
        endif
  50  CONTINUE
      call usrmsg(' ',' ','-') ! force redraw of the dialog box
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
      CHARACTER*10 CLMVAR(6),CVLAB
      logical close,colok
      DATA CLMVAR/'S rad W/m2','Temp Deg.C','Rel hum pc',
     &            'Wind s m/s','Cloud 10th','Prssure Pa'/

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

      call dintervalf(TMIN,TMAX,DX,NDEC,0)
      CALL horaxisdd(TMIN,TMAX,iigl,iigr,iigb,xadd,XSC,0,DX,NDEC,
     &  'Time (hrs)')


C Draw the vertical axis.
      call dintervalf(CMIN,CMAX,DY,NDEC,0)
      call vrtaxisdd(CMIN,CMAX,iigl,iigb,iigt,Yaddd,YSC,0,DY,NDEC,0,
     &  CVLAB)

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
