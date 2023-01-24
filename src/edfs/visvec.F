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

C This file contains the following routines:
C (related to image creation via vector commands)
C  MAKS2D - draws a 2 d silce of flow vectors on screen.  
C  MAKS3D - draws flow vectors on screen in 3D.  

C C ********************* MAKS2D *********************
C MAKS2D - draw 2d slices on screen.  IFRAME is the timestep
C offset from the beginning of the display period.
      SUBROUTINE MAKS2D(ISUR,ilayer,name,IFRAME)
#include "building.h"
#include "cfd.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/cfdsmper/ICDYS,ICDYF,CFTS,CFTF
      COMMON/cfdotper/ICDYOS,ICDYOF,CFTOS,CFTOF
      common/flvcol/ISOPT(6),IBGOPT(6),SCmin,SCmax

      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2

      COMMON/VISSET/imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal
      COMMON/arpoly/XS(7),ZS(7),bXmin,bXmax,bZmin,bZmax
      integer icolindex

      character souts*48,name*12,scale*32,title*30
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer iicol
      integer iupdown,isym,ibx,iby,ixsl,iysl    ! passed to etplot
      integer iid1,iid2,iid3,iid4
      integer iigt,ileft,iright      ! passed to eswline
      integer iigl,iigr,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iicol
      integer*8 iupdown,isym,ibx,iby,ixsl,iysl    ! passed to etplot
      integer*8 iid1,iid2,iid3,iid4
      integer*8 iigt,ileft,iright      ! passed to eswline
      integer*8 iigl,iigr,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif
      integer iiright   !passed to wstxpt
      logical colok,cscaleok
      real tempmin,tempmax,trange,colmult,NOW,FRACH
      CHARACTER MTHNAM(12)*4,PDESCR*96

      DATA MTHNAM/' Jan',' Feb',' Mar',' Apr',' May',' Jun',' Jul',
     &            ' Aug',' Sep',' Oct',' Nov',' Dec'/

C Set default values.
      XZ=0.0; YZ=0.0; ZZ=0.0

C Check if can draw in colour and with a range of colours.
      colok=.false.
      if(nzonec.ge.24)colok=.true.
      cscaleok=.false.
      if(ncset.gt.24)cscaleok=.true.
      
C << set a range of temps for testing logic similar to clmpsychart.F
C      write(6,*) 'within MAKS2D SCmin,SCmax',SCmin,SCmax
      tempmin=SCmin; tempmax=SCmax; trange=tempmax-tempmin
C      write(6,*) 'within MAKS2D ',tempmin,tempmax,trange
      colmult=(real(ncset)/trange)
      
C Clear viewing area, set and store scaling factors.
      CALL startbuffer()

C Setup and pass in parameters to win3d.
      iiw1=6; iiw2=4; iiw3=4; iiw4=2; iimenu=menuchw
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)

C Set up the do loops according to which surface is being viewed.
C East or west view.
      title='  '
      if(ISUR.eq.2 .or. ISUR.eq.4) then
        L1max = NJM1
        L2max = NKM1
        call VGRID2D(YZ,ZZ,YV,ZW,NJ,NK,title,'Y axis (m)','Z')

C North or south view.
      elseif(ISUR.eq.1 .or. ISUR.eq.3) then
        L1max = NIM1
        L2max = NKM1
        call VGRID2D(XZ,ZZ,XU,ZW,NI,NK,title,'X axis (m)','Z')

C Top or bottom view.
      elseif(ISUR.eq.5 .or. ISUR.eq.6) then
        L1max = NIM1
        L2max = NJM1
        call VGRID2D(XZ,YZ,XU,YV,NI,NJ,title,'X axis (m)','Y')
      endif

C Iterate through each flow vector to determine which grids of the mesh 
C it covers.
      do 10 L1=2,L1max
        do 20 L2=2,L2max

C Make each flow vector into a polygon.
          if(ISUR.eq.2 .or. ISUR.eq.4) then
            CALL POLY(ilayer,L1,L2)
            temp=Tf(ilayer,L1,L2)
          endif
          if(ISUR.eq.1 .or. ISUR.eq.3) then
            CALL POLY(L1,ilayer,L2)
            temp=Tf(L1,ilayer,L2)
          endif
          if(ISUR.eq.5 .or. ISUR.eq.6) then
            CALL POLY(L1,L2,ilayer)
            temp=Tf(L1,L2,ilayer)
          endif

C Set current colour.
          icolindex=nint((tempmax-temp)*colmult)
          if(icolindex.le.0) icolindex = 1
          if(icolindex.gt.ncset) icolindex = ncset
          if(cscaleok)then
            iicol=icolindex
            call winscl('c',iicol)
          else
            iicol=0
            call winscl('-',iicol)
          endif

C Draw vector.
          iupdown=0
          isym=0
          call etplot(XS(1),ZS(1),iupdown,isym)
          iupdown=1
          do 30 I=2,7
            call etplot(XS(I),ZS(I),iupdown,isym)
 30       continue
          call etplot(XS(1),ZS(1),iupdown,isym)
 20     continue
        iicol=0  ! set back to black
        call winscl('-',iicol)
 10   continue

C Write a title for the plot.
      CALL EDAYR(ICDYOS,IDs,IMs)
      CALL EDAYR(ICDYOF,IDf,IMf)
      FRACH=1.0/float(NTS)  ! what part of hour
      NOW=CFTOS+((IFRAME-1)*FRACH)
      if(NOW.gt.24.1) NOW=0.0
      write(PDESCR,'(3a,i3,2a,f5.2,a,i3,2a,f5.2,a,f5.2)')
     & 'Velocity vector plot for ',name(1:lnblnk(name)),' During',
     & IDs,MTHNAM(IMs),', hr ',CFTOS,' to',IDf,MTHNAM(IMf),', hr ',
     & CFTOF,' @',NOW
      iside=1; isize=0; ifont=1
      call viewtext(PDESCR,iside,isize,ifont)

C Draw a scale. First find distance between two points (iby and iysl)
C this represents 'sll' meters per second (initially 0.1).  If the 
C line takes up more than 1/4 of the width of the screen then try 
C again by dividing 'sll' by 10.
      call u2pixel(0.0,0.0,ibx,iby)
      icnt=1
      imax=igw/4
      div=1.0
 50   sll=0.1/div
      call u2pixel(0.0,sll,ixsl,iysl)
      isll= int((iby-iysl)*vLscal)
      if (icnt.gt.10) stop
      if (isll.gt.imax) then
        icnt=icnt+1
        div=div*10.0
        goto 50
      endif
      iright=igl+isll+10
      ileft=igl+10

C Draw scale line. e.g. |--------| = 0.1m/s
      iid1=ileft; iid2=igt-5; iid3=ileft; iid4=igt-15;
      call eswline(iid1,iid2,iid3,iid4)
      iid1=iright; iid2=igt-5; iid3=iright; iid4=igt-15;
      call eswline(iid1,iid2,iid3,iid4)
      iid1=ileft; iid2=igt-10; iid3=iright; iid4=igt-10;
      call eswline(iid1,iid2,iid3,iid4)
      write (scale,'(a,f5.3,a,F5.1,a,F5.1)') 
     &  '= ',sll,'m/s T',tempmin,'-T',tempmax
      iid1=iright+5; iid2=igt-5;
      call textatxy(iid1,iid2,scale,'-',iicol)

C Draw 10 steps along colour range.
      iid2=igt-10           ! mid of |----|
      iright=igl+isll+165   ! to right of text
      ra=trange/10.0
      temp=tempmin
      do loop=1,11
        if(loop.eq.1)then
          icolindex=nint((tempmax-temp)*colmult)
        else
          temp=temp+ra
          icolindex=nint((tempmax-temp)*colmult)
        endif
        if(icolindex.le.0) icolindex = 1
        if(icolindex.gt.ncset) icolindex = ncset
C        write(6,*) 'temp ',temp,icolindex
        if(cscaleok)then
          iicol=icolindex
          call winscl('c',iicol)
        else
          iicol=0
          call winscl('-',iicol)
        endif
        iright=iright+10
        call ecirc(iright,iid2,4,0)
        call forceflush()
      enddo
      
      return
      end

C ********************* MAKS3D *********************
C MAKS3D - draw flows in 3d on screen using scaling factors of 
C first view.
      SUBROUTINE MAKS3D(IFRAME)
#include "building.h"
#include "cfd.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/VISSET/imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/cfdsmper/ICDYS,ICDYF,CFTS,CFTF
      COMMON/cfdotper/ICDYOS,ICDYOF,CFTOS,CFTOF
      common/flvcol/ISOPT(6),IBGOPT(6),SCmin,SCmax
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      common/vecXYZ/vecXbeg(ntcelx,ntcely,ntcelz),
     1              vecXend(ntcelx,ntcely,ntcelz),
     2              vecYbeg(ntcelx,ntcely,ntcelz),
     3              vecYend(ntcelx,ntcely,ntcelz),
     4              vecZbeg(ntcelx,ntcely,ntcelz),
     5              vecZend(ntcelx,ntcely,ntcelz)

C To support stand-alone display via DFV.
      integer ISIMDAY ! Julian day within a cfd3dasci file
      real SIMTIMEF   ! time associated with a cfd3dasci file
      COMMON/DFVtime/ISIMDAY,SIMTIMEF

      dimension Vstart(3),Vend(3),IC(6,2)
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer iicol
      integer iupdown,isym,ibx,iby,ixsl,iysl    ! passed to etplot
      integer iid1,iid2,iid3,iid4
      integer iigt,ileft,iright      ! passed to eswline
      integer iigl,iigr,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iicol
      integer*8 iupdown,isym,ibx,iby,ixsl,iysl    ! passed to etplot
      integer*8 iid1,iid2,iid3,iid4
      integer*8 iigt,ileft,iright      ! passed to eswline
      integer*8 iigl,iigr,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif
      integer iiright   !passed to wstxpt
      logical colok,cscaleok
      real tempmin,tempmax,trange,colmult,NOW,FRACH
      character souts*48
      CHARACTER MTHNAM(12)*4,PDESCR*70

      DATA MTHNAM/' Jan',' Feb',' Mar',' Apr',' May',' Jun',' Jul',
     &            ' Aug',' Sep',' Oct',' Nov',' Dec'/
      
C Check if can draw in colour and with a range of colours.
      colok=.false.
      if(nzonec.ge.24)colok=.true.
      cscaleok=.false.
      if(ncset.gt.24)cscaleok=.true.
      
C Set a range of temps and colours similar to clmpsychart.F
C      write(6,*) 'within MAKS3D SCmin,SCmax',SCmin,SCmax
      tempmin=SCmin; tempmax=SCmax; trange=tempmax-tempmin
      colmult=(real(ncset)/trange)

C Need to reset scale factors - do this by redrawing the domain.
      call VGRID3D(0,1)

C Loop through all vectors.
      do 10 I=2,NIM1
        do 101 J=2,NJM1
          do 102 K=2,NKM1
            Vstart(1)=vecXbeg(i,j,k)
            Vend(1)  =vecXend(i,j,k)
            Vstart(2)=vecYbeg(i,j,k)
            Vend(2)  =vecYend(i,j,k)
            Vstart(3)=vecZbeg(i,j,k)
            Vend(3)  =vecZend(i,j,k)
            temp=Tf(i,j,k) ! current cell temperature

C Scale vector length.
            do 104 IV=1,3
              SUM=Vstart(IV)+Vend(IV)
              DIF=(Vend(IV)-Vstart(IV))*vLscal
              Vstart(IV)=(SUM-DIF)/2.
              Vend(IV)  =(SUM+DIF)/2.
 104        continue
            hsf=hWscal/10.

C Setup colour for current cell arrow.
            icolindex=nint((tempmax-temp)*colmult)
C            write(6,*) 
C     &        'Vstart,Vend,temp,hsf,icolindex ',
C     &        i,j,k,Vstart,Vend,temp,hsf
            if(icolindex.le.0) icolindex = 1
            if(icolindex.gt.ncset) icolindex = ncset
            if(cscaleok)then
              iicol=icolindex
              call winscl('c',iicol)
            else
              iicol=0
              call winscl('-',iicol)
            endif

C Draw the arrow.
            call arrow(Vstart,Vend,0.5,hsf,IC,'r',2)
            call forceflush()
 102      continue
 101    continue
        iicol=0  ! set back to black
        call winscl('-',iicol)
 10   continue
      call forceflush()

C Draw 10 steps along colour range.
C Write a title for the plot.
C Figure out the current time!  If IFRAME is zero then subroutine
C called within DFV so use alternative variables.
      if(IFRAME.eq.0)then
        CALL EDAYR(ISIMDAY,IDs,IMs)
        NOW=SIMTIMEF
        write(PDESCR,'(a,i3,2a,f5.2)')
     &    ' On',IDs,MTHNAM(IMs),' @',NOW
      else
        CALL EDAYR(ICDYOS,IDs,IMs)
        CALL EDAYR(ICDYOF,IDf,IMf)
        FRACH=1.0/float(NTS)  ! what part of hour
        NOW=CFTOS+((IFRAME-1)*FRACH)
        if(NOW.gt.24.1) NOW=0.0
        write(PDESCR,'(a,i3,2a,f5.2,a,i3,2a,f5.2,a,f5.2,a,f5.2)')
     &    ' During',IDs,MTHNAM(IMs),', hr ',CFTOS,' to',IDf,MTHNAM(IMf),
     &    ', hr ',CFTOF,' @',NOW
      endif
      iside=1 ! 1st line
      isize=0
      ifont=1
      call viewtext(PDESCR,iside,isize,ifont)
      write (souts,'(a,F5.1,a,F5.1)') ' Velocity vectors T ',
     &  tempmin,'-T',tempmax
      iside=2 ! 2nd line
      isize=0
      ifont=1
      call viewtext(souts,iside,isize,ifont)
      iid2=igt-15      ! near middle height of text 
      iright=igl+165   ! to right of text
      ra=trange/10.0
      temp=tempmin
      do loop=1,11
        if(loop.eq.1)then
          icolindex=nint((tempmax-temp)*colmult)
        else
          temp=temp+ra
          icolindex=nint((tempmax-temp)*colmult)
        endif
        if(icolindex.le.0) icolindex = 1
        if(icolindex.gt.ncset) icolindex = ncset
C        write(6,*) 'temp ',temp,icolindex
        if(cscaleok)then
          iicol=icolindex
          call winscl('c',iicol)
        else
          iicol=0
          call winscl('-',iicol)
        endif
        iright=iright+10
        call ecirc(iright,iid2,4,0)
        call forceflush()
      enddo

      return
      end
