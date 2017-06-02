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

C This file contains the following subroutines:

C   minptb
C   zngrph

C ******************** minptb ********************

C 'minptb' prompts the user to enter data associated
C with the zone(s) temperature to be monitored during the
C simulation.

C Common block '/montrb/' consists of the following variables:

c mnzn   : Number of zones considered.
c mzone  : Array holding number of each zone.
c montrb : Switch to activate monitoring of zone temperatures.
c          (0 means switch is off).
c yminb  : Expected minimum value of temperature along the y axis.
c ymaxb  : Expected maximum value of temperature along the y axis.
c ibplot : If =0 then no plotiing takes place. Used when in
c          start-up period.
c irecpb : Holds data position in building result library. Number
c          of positions held = no. of zones * no. time-steps/hour.
c mbstv  : Building state variables to be plotted:
c               1: zone temperature.
c               2: zone plant flux.
c               3: zone temperature and plant flux.

      subroutine minptb
#include "building.h"
#include "geometry.h"
#include "monitor.h"

      common/outin/iuout,iuin
      
      integer ncomp,ncon
      common/c1/ncomp,ncon
      common /bmontr/mnzn,mzone(mcom),montrb,yminb(2),ymaxb(2),
     &               ibplot,irecpb(MDPH),mbstv

      dimension IVALS(MCOM)
      character hold*24

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='minptb'  ! set for subroutine

c Initialize array.
      do 5 i=1,MDPH
         irecpb(i)=0
    5 continue
      yminb(1)=0.
      ymaxb(1)=30.
      yminb(2)=-6000.
      ymaxb(2)=6000.

c Get number of zones.
      if(ncomp.eq.1) then
         mzone(1)=1
         mnzn=1
      else
   10   mnzn=NCOMP
        helptopic='monitor_facility_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EPICKS(mnzn,IVALS,' ',' Which zones to include: ',
     &    12,NCOMP,zname,'monitor list',IER,nbhelp)
         if(ier.ne.0) goto 10
         if(mnzn.eq.0)return

c Get number of each zone.
         do 55 i=1,mnzn
           mzone(i)=IVALS(I)
   55    continue
      endif

c Get desired state variable.
      CALL EASKABCD( 'State variables:',' ','temperature (C)',
     &  'plant flux (W)','both','cancel',mbstv,nbhelp)
      if(mbstv.eq.4)then
        mbstv=0
        mnzn=0
        return
      endif

c Get expected minimum and maximum values for state variable.
c For option 1 and 2, read min and max data.
      if(mbstv.eq.1.or.mbstv.eq.2) then
   30   WRITE(hold,'(2F9.2)')yminb(1),ymaxb(1)
        CALL EASKS(hold,' Minimum and maximum values:',' ',24,' ',
     &      'min & max values',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,yminb(1),0.,0.,'-','mntr min',IER)
        CALL EGETWR(HOLD,K,ymaxb(1),0.,0.,'-','mntr max',IER)
        if(ier.ne.0) goto 30

c For option 3, read min and max data for temperature
c and plant flux.
      elseif(mbstv.eq.3) then
   33   WRITE(hold,'(2F9.2)')yminb(1),ymaxb(1)
        CALL EASKS(hold,' Minimum and maximum temperature:',' ',24,' ',
     &      'min & max values',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,yminb(1),0.,0.,'-','mntr min',IER)
        CALL EGETWR(HOLD,K,ymaxb(1),0.,0.,'-','mntr max',IER)
        if(ier.ne.0) goto 33

   36   WRITE(hold,'(2F9.1)')yminb(2),ymaxb(2)
        CALL EASKS(hold,' Minimum and maximum plant (W):',' ',24,' ',
     &      'min & max values',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,yminb(2),0.,0.,'-','mntr min',IER)
        CALL EGETWR(HOLD,K,ymaxb(2),0.,0.,'-','mntr max',IER)
        if(ier.ne.0) goto 36
      endif

      return
      end

c ******************** zngrph ********************

c 'mgraph' sets up graphics variables and draws axes
c for subsequent plotting.

c If 'mode=1' then set up graph window and draw axes.
c If 'mode=2' then plot data points.

c Plotting commences after start-up period.
c This is because the routine in which
c values are assigned to zone temperatures is called
c after the start-up period.

      subroutine zngrph(iss,isf,instep,mode)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "monitor.h"
      
      integer lnblnk  ! function definition

C Parameters for eline calls.
      integer LNMVAB, LNDRAB, LNMVRL, LNDRRL
      parameter (LNMVAB=1, LNDRAB=0, LNMVRL=3, LNDRRL=2)

      COMMON/OUTIN/IUOUT,IUIN
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      
      integer ncomp,ncon
      common/c1/ncomp,ncon
      common /bmontr/mnzn,mzone(mcom),montrb,yminb(2),ymaxb(2),
     &               ibplot,irecpb(MDPH),mbstv
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/CLIM/IDIF(MT),ITMP(MT),IDNR(MT),IVEL(MT),
     &IDIR(MT),IHUM(MT),IDIFF,ITMPF,IDNRF,IVELF,IDIRF,
     &IHUMF

c Graph commons.
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/spad/mmod,limit,limtty
      COMMON/FILEP/IFIL
      COMMON/SAVE/ISAVE

C Need to remember the following variables (i.e those are
C static variables). Although by default all variables are remembered
C it is good practice to save only those that need saving
C since not all compilers use the same default.
      save xsc,ysc,ysc1,xadd,yadd,yadd1,icount,ncoord,ncoord1,
     &     ncoord2,iyofst
      dimension ncoord(mcom,2,2), ncoord1(mcom,2,2),
     &          ncoord2(1,2,2)

      CHARACTER DS*7,DS1*10,DE*7,DE1*10
      character etext*82, clabel*20,nterm*1,tab*1
      logical colok

#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer iix,iiy,iix2,iix5,iix35,iiy2  ! passed to edline
      integer iicol
      integer iigl,iigr,iigt,iigb,iigw,iigwh
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iix,iiy,iix2,iix5,iix35,iiy2  ! passed to edline
      integer*8 iicol
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
#endif

C Test save option selected.
C [for NRCan]      tab=','
      tab=' '

C In order to pass the value across to C it is necessary to first
C do an internal write placing an end marker which C can understand.
      nterm=CHAR(0)

C Check if can draw in colour.
      colok=.false.
      if(nzonec.ge.NCOMP)colok=.true.

C Cast values for horaxis.
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh

      goto (1000,2000) mode

 1000 continue

C Open the text display box equal to LIMTTY if MODEL = 8.
      IF(MMOD.EQ.8)THEN
        call startbuffer()
        iyofst=20
      ENDIF

C Find minimum and maximum values for x-axis. Add one day
C to ensure last 24 hours is plotted.
      xmin=0.
      nhours=((isf-iss)+1)*24
      xmax=float(nhours)

c Display header information.
      write(etext,'(2A)')'Project: ',modeltitle(1:lnblnk(modeltitle))
      iside=1
      isize=1
      ifont=1
      call viewtext(etext,iside,isize,ifont)

C Generate view period string.
      CALL EDAY(ISD1,ISM1,IODS)
      CALL STDATE(IYEAR,IODS,DS,DS1)
      CALL EDAY(ISD2,ISM2,IODF)
      CALL STDATE(IYEAR,IODF,DE,DE1)
      write(etext,'(7a,i4)') 'Period',tab,DS1,' to ',DE1,' Year',tab,
     &  IYEAR
      iside=2
      isize=1
      ifont=1
      call viewtext(etext,iside,isize,ifont)

C If Plant flux then divide by 1000.0
      if(mbstv.eq.2) then
         yminb(1)=yminb(1)/1000.
         ymaxb(1)=ymaxb(1)/1000.
      elseif(mbstv.eq.3) then
         yminb(2)=yminb(2)/1000.
         ymaxb(2)=ymaxb(2)/1000.
      endif

c Determine scaling ratios for the drawing and axis.
      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,xmin,xmax,yminb(1),ymaxb(1),
     &   xsc,ysc,sca,xadd,yadd)

c Determine scaling ratios for other y axis.
      if(mbstv.eq.3)
     &   call axiscale(igwid,igheight,xmin,xmax,yminb(2),ymaxb(2),
     &      xsc,ysc1,sca1,xadd,yadd1)

c Draw a scale horizontal axis, move to beginning point.
      call dintervalf(Xmin,Xmax,ddy1,ny,1)
      call horaxisdd(xmin,xmax,iigl,iigr,iigb,xadd,xsc,1,
     &  ddy1,ny,'Time (h)')

c Draw a scale vertical axis.
      call dintervalf(yminb(1),ymaxb(1),DY,NDEC,1)
      call vrtaxisdd(yminb(1),ymaxb(1),iigl,iigb,iigt,yadd,ysc,1,
     &  DY,NDEC,0,':')
      if(mbstv.eq.1.or.mbstv.eq.3) then
         call wstxpt(igl-5,igt-17,'Temp')
         call wstxpt(igl-5,igt-5,'deg.C')
      elseif(mbstv.eq.2) then
         call wstxpt(igl-5,igt-17,'Plant')
         call wstxpt(igl-5,igt-5,'  KW')
         yminb(1)=yminb(1)*1000.
         ymaxb(1)=ymaxb(1)*1000.
      endif

      if(mbstv.eq.3) then
         call dintervalf(yminb(2),ymaxb(2),DY,NDEC,1)
         call vrtaxisdd(yminb(2),ymaxb(2),
     &     iigr,iigb,iigt,yadd1,ysc1,1,DY,NDEC,1,':')
         call wstxpt(igr,igt-17,'Plant')
         call wstxpt(igr,igt-5,'  KW')
         yminb(2)=yminb(2)*1000.
         ymaxb(2)=ymaxb(2)*1000.
       endif

c initialise data items count 'icount'.
      call forceflush()
      icount=0
      return

c Plot graph for zone(s) state variables.
C If save option 0 then exit.
 2000 if((ISAVE.eq.0).or.(ISAVE.eq.6)) goto 999
      IF(IFS.GT.0)CALL winfnt(1)
      iunit=ifil+2
      do 20 j=1, instep

C Draw External dry bulb temperature only if
C zone temperatures or zone temperatures and plant flux
C options were invoked.
         if(mbstv.ne.2) then
            dbpv=float(itmp(ihrp))/10.
            dbfv=float(itmp(ihrf))/10.
            if(ihrf.eq.1) dbfv=float(itmpf)/10.
            ratio=float(j-1)/float(instep)

C Evaluate dry bulb temperature using linear interpolation.
            dbval=dbpv+ratio*(dbfv-dbpv)
            if(dbval.lt.yminb(1)) dbval=yminb(1)
            if(dbval.gt.ymaxb(1)) dbval=ymaxb(1)

C If this is first point then save it and identify
C dry bulb temperature as 'EXT' with a solid line
C with no symbols.
            if(icount.eq.0) then
               ncoord2(1,1,1)=igl+int((0+xadd)*xsc)
               ncoord2(1,1,2)=igb-int((dbval+yadd)*ysc)
               iy= igt+iyofst
               ix= 25
               clabel(1:3)='EXT'
               call wstxpt(ix,iy,clabel)
               iiy=iy
               iix5=ix+5
               iix35=ix+35
               call edline(iix5,iiy,iix35,iiy,1)

C Otherwise plot external temperature.
            else
               ti=float(icount)/float(instep)
               ncoord2(1,2,1)=igl+int((ti+xadd)*xsc)
               ncoord2(1,2,2)=igb-int((dbval+yadd)*ysc)
               call eswline(ncoord2(1,1,1),ncoord2(1,1,2),
     &           ncoord2(1,2,1),ncoord2(1,2,2))
               call forceflush
               ncoord2(1,1,1)=ncoord2(1,2,1)
               ncoord2(1,1,2)=ncoord2(1,2,2)
            endif
         endif
         do 10 i=1, mnzn
            irec=irecpb(mzone(i)+ncomp*(j-1))
            if(irec.eq.0) goto 999
            if(isave.eq.1) then
                read(iunit,rec=irec,iostat=istat,err=1100) tma,dumm,qm
            else
                read(iunit,rec=irec,iostat=istat,err=1100) tma,qm
            endif
            if(mbstv.eq.1) value=tma
            if(mbstv.eq.2) value=qm/1000.
            if(value.gt.ymaxb(1)) value=ymaxb(1)
            if(value.lt.yminb(1)) value=yminb(1)

C Case when both variables are plotted.
            if(mbstv.eq.3) then
               value=tma
               value1=qm/1000.
               if(value.gt.ymaxb(1))  value=ymaxb(1)
               if(value.lt.yminb(1))  value=yminb(1)
               if(value1.gt.ymaxb(2)) value1=ymaxb(2)
               if(value1.lt.yminb(2)) value1=yminb(2)
            endif

c If first point then store its coordinates and identify
c each curve as 'Z??' where 'Z' stands for zone.
            if(icount.eq.0) then
               if(colok)then
                 if(mzone(i).gt.0)then
                   iicol=mzone(i)
                   call winscl('z',iicol)
                 endif
               endif
               clabel='                    '
C               write(clabel,'(a1,i3,a1)')'Z',mzone(i),nterm
               ln=lnzname(mzone(i))
               write(clabel,'(2a)') zname(mzone(i))(1:ln),nterm
               ncoord(i,1,1)=igl+int((0+xadd)*xsc)
               ncoord(i,1,2)=igb-int((value+yadd)*ysc)
               if(mbstv.eq.3) then
                  ncoord1(i,1,1)=igl+int((0+xadd)*xsc)
                  ncoord1(i,1,2)=igb-int((value1+yadd1)*ysc1)
                  iy= igt+iyofst+(i*18)
                  ix= igr+25
                  call wstxpt(ix,iy,clabel)
                  iiy2=iy+2
                  iix5=ix+5
                  iix35=ix+35
                  call edline(iix5,iiy2,iix35,iiy2,2)
                  isr=i+mnzn
                  call esymbol(ix+20,iy+2,isr,0)
               endif
               iy= igt+iyofst+(i*18)
               ix= 25
               call wstxpt(ix,iy,clabel)
               call eswline(ix+5,iy+2,ix+35,iy+2)
               call esymbol(ix+20,iy+2,i,0)
               if(colok)then
                 iicol=0
                 call winscl('-',iicol)
               endif
               call forceflush

c else start plotting.
            else
               if(colok)then
                 if(mzone(i).gt.0)then
                   iicol=mzone(i)
                   call winscl('z',iicol)
                 endif
               endif
               ti=float(icount)/float(instep)
               ncoord(i,2,1)=igl+int((ti+xadd)*xsc)
               ncoord(i,2,2)=igb-int((value+yadd)*ysc)
               call eline (ncoord(i,1,1),ncoord(i,1,2), lnmvab)
               call eline (ncoord(i,2,1),ncoord(i,2,2), lndrab)
               if(j.eq.instep) call esymbol(ncoord(i,2,1),
     &                   ncoord(i,2,2),i,0)
               ncoord(i,1,1)=ncoord(i,2,1)
               ncoord(i,1,2)=ncoord(i,2,2)
               if(mbstv.eq.3) then
                  ncoord1(i,2,1)=igl+int((ti+xadd)*xsc)
                  ncoord1(i,2,2)=igb-int((value1+yadd1)*ysc1)
                  iix=ncoord1(i,1,1)
                  iiy=ncoord1(i,1,2)
                  iix2=ncoord1(i,2,1)
                  iiy2=ncoord1(i,2,2)
                  call edline (iix,iiy,iix2,iiy2,2)
                  isr=i+mnzn
                  if(j.eq.instep) call esymbol(ncoord1(i,2,1),
     &                   ncoord1(i,2,2),isr,0)
                  ncoord1(i,1,1)=ncoord1(i,2,1)
                  ncoord1(i,1,2)=ncoord1(i,2,2)
               endif
               if(colok)then
                 iicol=0
                 call winscl('-',iicol)
               endif
               call forceflush
            endif
   10    continue
         icount=icount+1
   20 continue
      IF(IFS.GT.0)CALL winfnt(IFS)
      call forceflush()
      goto 999

 1100 write(etext,'(A,i5,a)') ' zngrh: error at record',irec,'.'
      call edisp(iuout,etext)

  999 return
      end

