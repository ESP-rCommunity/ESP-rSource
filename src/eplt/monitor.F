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

C This file contains the following subroutines:
C   minput
C   mgraph

C ******************** minput ********************

c 'minput' prompts the user to enter data associated
c with the plant state variable(s) to be monitored during
c the simulation.

c Common block '/montrv/' consists of the following variables:

c mnpc   : Number of plant components considered.
c mcomp  : Array holding number of each component.
c mcnod  : Array holding node number for each component.
c mstate : State variable to be monitored
c          (i.e (1) temperature (C)
c               (2) first phase mass flow rate  (kg/s)
c               (3) second phase mass flow rate (kg/s) )
c monitr : Switch to activate monitoring of variables.
c          (0 means switch is off).
c ymin   : Expected minimum value of variable along the y axis.
c ymax   : Expected maximum value of variable along the y axis.

      subroutine minput
#include "plant.h"
#include "help.h"

      common/outin/iuout,iuin,ieout
      common/c9/npcomp,nci(mpcom),cdata(mpcom,mmiscd)
      common/c12ps/npcdat(mpcom,9),ipofs1(mcoefg),ipofs2(mcoefg,mpvar)
      common/c13ps/npmcoe,npnod,npmtyp
      common /montrv/mnpc,mcomp(mpcom),mcnod(mpcom),mstate,monitr,
     &               ymin,ymax

      common/pcnam/pcname(mpcom)

      dimension IVALS(mpcom)
      character pcname*15, outs*124

      helpinsub='monitor'  ! set for subroutine

c Get number of components.
      if(npcomp.eq.1) then
         mnpc=1
         mcomp(1)=1
      else

C Initialise minimum and maximum y-axis values.
        ymin=-5.0
        ymax=30.0

c Get number of each component.
   10   mnpc=npcomp
        helptopic='plt_graphic_monitor'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EPICKS(mnpc,IVALS,' ',' Which component to include: ',
     &    15,npcomp,pcname,' monitor list',IER,nbhelp)
     &    
         if(ier.ne.0) goto 10
         if(mnpc.eq.0)return

c Get number of each zone.
         do 55 i=1,mnpc
           mcomp(i)=IVALS(I)
   55    continue
      endif

c Find number of nodes for this component and initialise
c desired node number.
      do 60 i=1,mnpc
        nnodes=npcdat(mcomp(i),8)
        mcnod(i)=1

c If more than one node exists then get desired node.
        if(nnodes.gt.1) then
          write(outs,*) ' Node number for component',mcomp(i),'?'
          CALL EASKI(nnc,outs,' ',
     &      1,'F',nnodes,'F',1,'node number',IER,nbhelp)
          mcnod(i)=nnc
        endif
   60 continue

c Desired state variable?. The number of available state
c variables is determined from the value of the matrix
c type index.
   20 ier=0
      if(npmtyp.eq.3) then
        CALL EASKI(mstate,' State variable:',
     &    ' 1) temperature,  2) 1st phase or  3) 2nd phase ? ',
     &    1,'F',3,'F',1,'state variable',IER,nbhelp)
      elseif(npmtyp.eq.2) then
        CALL EASKI(mstate,' State variable:',
     &    ' 1) temperature,  2) 1st phase ? ',
     &    1,'F',2,'F',1,'state variable',IER,nbhelp)
      elseif(npmtyp.eq.1) then
         mstate=1
      endif
      if(ier.ne.0) goto 20

c Get expected minimum and maximum values for satate variable.
   30 CALL EASKR(ymin,' ',' Minimum value for state variable ?',
     &  0.,'-',0.,'-',0.8,'min state var',IER,nbhelp)
      CALL EASKR(ymax,' ',' Maximum value for state variable ?',
     &  0.,'-',0.,'-',0.8,'max state var',IER,nbhelp)
      if(ier.ne.0) goto 30
      return

      end

c ******************** mgraph ********************

c 'mgraph' sets up graphics variables and draws axes
c for subsequent plotting.

c If 'mode=1' then set up graph window and draw axes.
c If 'mode=2' then plot data points.

c Plotting commences after start-up period.
c This is because the routine in which
c values are assigned to 'csv1m', 'csv2m' and 'csv3m' is called
c after the start-up period.

      subroutine mgraph(iss,isf,instpp,mode)
#include "plant.h"

C Parameters for eline calls.
      integer LNMVAB, LNDRAB, LNMVRL, LNDRRL
      parameter (LNMVAB=1, LNDRAB=0, LNMVRL=3, LNDRRL=2)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/pctstp/ntstpp
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      common /montrv/mnpc,mcomp(mpcom),mcnod(mpcom),mstate,monitr,
     &               ymin,ymax
      common/csv123/csv1m(mpnode),csv2m(mpnode),csv3m(mpnode)
      common/c12ps/npcdat(mpcom,9),ipofs1(mcoefg),ipofs2(mcoefg,mpvar)

c Graph commons.
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

C Need to remember the following variables (i.e those are
C static variables). Although by default all variables are remembered
C it is good practice to save only those that need saving
C since not all compilers use the same default.
      save xsc,ysc,xadd,yadd,icount,ncoord
      dimension ncoord(mpcom,2,2)

      character label1*10,etext*72,clabel*20,nulter*1,tab*1
      CHARACTER DS*7,DS1*10,DE*7,DE1*10,DS2*8,DE2*8
      character*12 statev(3)
      logical colok
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer iicol,iix,iiy,iix1,iiy1
      integer iigl,iigr,iigt,iigb,iigw,iigwh
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont     ! passed to etplot
      integer*8 iicol,iix,iiy,iix1,iiy1
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
#endif

      tab=' '

C This is string terminator for C.
      nulter=char(0)

C Cast values for horaxis.
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh

C Jump to required routine.
      goto (1000,2000) mode

c Choose correct label.
 1000 if(mstate.eq.1) then
        label1='degrees(C)'
        statev(1)='temperature '
      else if(mstate.eq.2) then
        label1='10^-3 kg/s'
        statev(2)='first phase '
      else if(mstate.eq.3) then
        label1='10^-3 kg/s'
        statev(3)='second phase'
      endif

C Check if can draw in colour.
      colok=.false.
      if(nzonec.ge.mnpc)colok=.true.

C Open the text display box equal to LIMTTY if MMOD = 8.
      if(mmod.eq.8)call startbuffer()

c Display header information.
      write(etext,'(3a)') ' Variations of ',statev(mstate),
     &  ' state variable with time'
      iside=1; isize=1; ifont=1
      if(mmod.eq.8)then
        call viewtext(etext,iside,isize,ifont)
      else
        call viewtextwwc(etext,iside,isize,ifont)
      endif

C Generate view period string.
      CALL EDAY(ISD1,ISM1,IODS)
      CALL STDATE(IYEAR,IODS,DS,DS1,DS2)
      CALL EDAY(ISD2,ISM2,IODF)
      CALL STDATE(IYEAR,IODF,DE,DE1,DE2)
      write(etext,'(7a,i4)') 'Period',tab,DS1,' to ',DE1,' Year',tab,
     &  IYEAR
      iside=2
      if(mmod.eq.8)then
        call viewtext(etext,iside,isize,ifont)
      else
        call viewtextwwc(etext,iside,isize,ifont)
      endif

C Find minimum and maximum values for x-axis based on the simulation
C period.
      xmin=0.0
      nhours=((iodf-iods)+1)*24
      xmax=float(nhours)

c Case for 1st or 2nd phase.
      if(mstate.eq.2.or.mstate.eq.3) then
         ymin=ymin*1000.
         ymax=ymax*1000.
      endif

c Determine scaling ratios for the drawing and axis.
      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,xmin,xmax,ymin,ymax,xsc,ysc,sca,
     &      xadd,yadd)

c Draw a scale horizontal axis, move to beginning point.
      call dintervalf(xmin,xmax,DY,NDEC,1)
      call horaxisdd(xmin,xmax,iigl,iigr,iigb,xadd,xsc,1,
     &  DY,NDEC,'Time (hours)')

c Draw a scale vertical axis.
      call dintervalf(ymin,ymax,DY,NDEC,1)
      call vrtaxisdd(ymin,ymax,iigl,iigb,iigt,yadd,ysc,1,
     &  DY,NDEC,0,label1)

c Restore original values for next call.
      if(mstate.eq.2.or.mstate.eq.3) then
         ymin=ymin/1000.
         ymax=ymax/1000.
      endif

c initialise data items count 'icount'.
      icount=0
      return

c Plot graph for chosen state variable.
 2000 continue
      do 10 i=1, mnpc
        IF(IFS.GT.0)CALL winfnt(1)
        ii=npcdat(mcomp(i),9)-1

c Temperature.
        if(mstate.eq.1) value=csv1m(ii+mcnod(i))

c First phase.
        if(mstate.eq.2) value=csv2m(ii+mcnod(i))

c Second phase.
        if(mstate.eq.3) value=csv3m(ii+mcnod(i))

c Make sure graph window bounds are not exceeded.
        if(value.gt.ymax) value=ymax
        if(value.lt.ymin) value=ymin

c For 1st and 2nd phase mass flow rate, multiply by 1000.
        if(mstate.eq.2.or.mstate.eq.3) value=value*1000.

c If first point then store its coordinates and identify
c each curve as 'C?N??' where 'C' stands for component
c and 'N' stands for node in this component.
        if(icount.eq.0) then
          write(clabel,2010)mcomp(i),mcnod(i),nulter
 2010     format('C',i2,',n',i2,a1)
          ncoord(i,1,1)=igl+int((0+xadd)*xsc)
          ncoord(i,1,2)=igb-int((value+yadd)*ysc)
          iiy= ncoord(i,1,2)
          iix= ncoord(i,1,1)+3
          if(colok)then
            iicol=i
            call winscl('z',iicol)
          endif
          call textatxy(iix,iiy,clabel,'z',iicol)
          if(colok)then
            iicol=0
            if(mmod.EQ.8) call winscl('-',iicol)
          endif

c else start plotting.
        else
          if(colok)then
            iicol=i
            if(mmod.EQ.8) call winscl('z',iicol)
          endif
          ti=float(icount)/float(instpp)
          ncoord(i,2,1)=igl+int((ti+xadd)*xsc)
          ncoord(i,2,2)=igb-int((value+yadd)*ysc)
          iix1=ncoord(i,1,1); iiy1=ncoord(i,1,2)
          if(mmod.EQ.8)then
            call eline (iix1,iiy1, lnmvab)
          else
            call elinewwc (iix1,iiy1, lnmvab)
          endif
          iix1=ncoord(i,2,1); iiy1=ncoord(i,2,2)
          if(mmod.EQ.8)then
            call eline (iix1,iiy1, lndrab)
          else
            call elinewwc (iix1,iiy1, lndrab)
          endif
          ncoord(i,1,1)=ncoord(i,2,1)
          ncoord(i,1,2)=ncoord(i,2,2)
          if(colok)then
            iicol=0
            call winscl('-',iicol)
          endif
          if(mmod.EQ.8) call forceflush
        endif
   10 continue

C Debug.
C      write(6,*) icount,instpp,ti,xmax

C Increment count.
      icount=icount+1
      IF(IFS.GT.0)CALL winfnt(IFS)
      return
      end

