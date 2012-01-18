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

C GRAPH.F contains the following routines.
C GRAPH:  Uses FACDRW to display the target surface
C         and SHDDRW to superimpose any direct shading.
C FACDRW: Draws the target surface elevation.
C SHDDRW: Plots point on a surface to represent the
C         directly shaded shaded portion.
C SITPLN: Scales and draws the site plan showing zone and
C         obstructions.
C SUNPLT: Draws a Mercator sun-path diagram.

C ********** GRAPH **********
C Uses FACDRW to display the target surface and SHDDRW
C to superimpose any direct shading.

      subroutine graph(icomp)
#include "building.h"
#include "geometry.h"

C Parameter
      integer icomp  ! the focus zone

      common/outin/iuout,iuin
      common/filep/ifil
      common/spad/mmod,limit,limtty
      common/g1t/xft(mv),zft(mv)
      common/headl/irecs(ms)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/viewpx/menuchw,igl,igr,igt,igb,igw,igwh
      integer izstocn
      common/c24/izstocn(mcom,ms)

      character outs*124
      dimension ioshd(mox,moz),iva(ms)
      logical ok,dok
      integer icc,is,nv   ! local variables for connextion and vertices
      integer ih ! hour counter
      
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
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='graph'  ! set for subroutine

      iunit=ifil

C Ask user for the surface to focus on.
    2 call pickssur(icomp,np,'L',iva,ier)
      if(np.eq.0)then
        goto 1
      else
        is=iva(1)
      endif

C Find connection and number of edges and then retrieve the
C surface transformed coordinates.
      icc=izstocn(icomp,is)
      nv=nznver(icc)
      irec=irecs(is)
      read(iunit,rec=irec,iostat=istat,err=1000)(xft(i),i=1,nv)
      irec=irec+1
      read(iunit,rec=irec,iostat=istat,err=1000)(zft(i),i=1,nv)
      irec=irec+1

C Set up mesh centre points.
      call mesh(icomp,is)

C Make the right offset for the graphic box is the character
C width of the menu.
      ier=0
      if(mmod.lt.8)then
        call usrmsg('Not in graphic mode!',' ','W')
        return
      endif

C Clear the viewing box, draw its border and return the offsets
C that define it. The win3d parameters match those in facdrw
C and sitpln.
      call startbuffer()

C Setup and pass in parameters to win3d.
      iiw1=7; iiw2=4; iiw3=4; iiw4=3; iimenu=menuchw
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
C      call win3d(menuchw,7,4,4,3,igl,igr,igt,igb,igw,igwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      igwid=igw
      igheight=igwh

C Update the plane equation.
      call trnsf1(icomp,is)

C Find maximum & minimum surface coordinates for this surface.
      xmax=-1.E+8
      zmax=-1.E+8
      xmin=1.E+8
      zmin=1.E+8
      do 10 i=1,nznver(icc)
        xmax=amax1(xmax,xft(i))
        zmax=amax1(zmax,zft(i))
        xmin=amin1(xmin,xft(i))
        zmin=amin1(zmin,zft(i))
   10 continue

C Reopen the dialogue box.
      if(mmod.eq.8)call usrmsg(' ',' ','-')

C Determine scaling ratios.
      call axiscale(igwid,igheight,xmin,xmax,zmin,zmax,xsc,ysc,sca,
     &  Xadd,Zadd)

C Place scaling in static C variables for use by line drawing routines.
      call linescale(iigl,Xadd,sca,iigb,Zadd,sca)

C Output start hour.
      ih=1
      helptopic='ish_graphing_hour'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call easki(ih,' ','Output start hour?',
     &  1,'F',24,'F',1,'output start',ier,nbhelp)

C Hour-by-hour.
      do 20 j=1,24
         read(iunit,rec=irec,iostat=istat,err=1000)isunup
         irec=irec+1
         if(isunup.eq.0)goto 20

C inds dictates whether direct shading is total, partial or zero.
         read(iunit,rec=irec,iostat=istat,err=1000)inds
         irec=irec+1

C No direct shading.
         if(inds.eq.-2)then
            irec=irec+1
            if(j.lt.ih)goto 20
            call facdrw(j,icomp,is,nv,xmin,xmax,zmin,zmax,
     &                                               Xadd,Zadd,sca)

C Total direct shading.
         elseif(inds.eq.-1)then
            irec=irec+1
            if(j.lt.ih)goto 20
            call facdrw(j,icomp,is,nv,xmin,xmax,zmin,zmax,
     &                                               Xadd,Zadd,sca)
            call shddrw(icomp,is,1,ioshd)

C Partial direct shading.
         elseif(inds.eq.0)then
            if(j.lt.ih)then
               irec=irec+nox(icomp)+1
               goto 20
            endif
            do 30 k=1,nox(icomp)
               read(iunit,rec=irec,iostat=istat,err=1000)
     &           (ioshd(k,l),l=1,noz(icomp))
               irec=irec+1
   30       continue
            irec=irec+1
            call facdrw(j,icomp,is,nv,xmin,xmax,zmin,zmax,
     &                                               Xadd,Zadd,sca)
            call shddrw(icomp,is,2,ioshd)
         endif

         dok=.true.
         call askok(' ','Next hour?',ok,dok,nbhelp)
         if(.not.ok)goto 1
   20 continue

    1 dok=.true.
      call askok(' ','Consider another surface?',ok,dok,nbhelp)
      if(.not.ok)return
      goto 2

 1000 write(outs,1001)irec
 1001 format('GRAPH: Zone transitional shading file error, record',i4)
      call edisp(iuout,outs)
      return
      end

C ********** FACDRW **********
C Draws the elevation of the target surface.

      subroutine facdrw(ihour,icomp,isur,nv,xmin,xmax,zmin,zmax,
     &                                                Xadd,Zadd,sca)
#include "building.h"
#include "geometry.h"

      integer mon,isc,iyd
      common/contr/mon,isc(ms),iyd
      common/g1T/xft(mv),zft(mv)
      integer izstocn
      common/c24/izstocn(mcom,ms)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/viewpx/menuchw,igl,igr,igt,igb,igw,igwh

      dimension month(12)
      character*72 etext*72,month*3
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer ipos,iupdown,isym     ! passed to etplot
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 ipos,iupdown,isym     ! passed to etplot
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

C Display header information. Calculate graphic boundaries.
      iiw1=7; iiw2=4; iiw3=3; iiw4=3; iimenu=menuchw
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
C      call win3d(menuchw,7,4,3,3,igl,igr,igt,igb,igw,igwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      icn=izstocn(icomp,isur)

      write(etext,1)month(mon),ihour,isur,ssname(icn)
    1 format('Direct shading: ',a3,', @ ',i2,':00, Surface ',i2,
     &       ' (',a,').')
      iside=1
      isize=1
      ifont=2
      call viewtext(etext,iside,isize,ifont)

C Draw horizontal axis and move to start point.
      call horaxis(xmin,xmax,iigl,iigr,iigb,Xadd,sca,0,'Horizontal')

C Draw vertical axis.
      call vrtaxis(zmin,zmax,iigl,iigb,iigt,Zadd,sca,0,0,'Vertical')

C Draw target surface.
      iupdown=0
      isym=0
      call etplot(xft(1),zft(1),iupdown,isym)
      iupdown=1
      do 10 i=2,nv
         call etplot(xft(i),zft(i),iupdown,isym)
   10 continue
      call etplot(xft(1),zft(1),iupdown,isym)
      call forceflush()
      return
      end

C ********** SHDDRW **********
C Plots points on target surface representing direct shaded portion.

      subroutine shddrw(izone,is,ishad,ioshd)
#include "building.h"
#include "geometry.h"

C Parameters
      integer izone  ! index of zone
      integer is     ! target surface
      integer ishad  ! denotes partial or full shading - see below
      integer ioshd  ! array of points
      dimension ioshd(mox,moz)

      common/grid11/xop(mox,moz),zop(mox,moz)

#ifdef OSI
      integer iix,iiy,iid1,iid2
#else
      integer*8 iix,iiy,iid1,iid2
#endif

      do 10 i=1,nox(izone)
         do 20 j=1,noz(izone)

C ishad=2 corresponds to the partial shading case, while ishad=1
C corresponds to 100% shading for which ioshd points falling outwith
C the surface polygon will not have been set to -1.
            if(ishad.eq.1)then
               call point1(izone,is,xop(i,j),zop(i,j),ians)
               if(ians.eq.0)goto 20
               goto 1
            endif
            if(ioshd(i,j).eq.1)goto 1
            goto 20
    1       call u2pixel(xop(i,j),zop(i,j),iix,iiy)
            iid1=iix-1; iid2=iix+1;
            call edwline(iid1,iiy,iid2,iiy)
   20    continue
   10 continue
      call forceflush()
      return
      end

C ********** SITPLN **********
C Scales and draws the site plan showing the object boundary
C and the surrounding obstruction blocks based on common block
C data structures.

      subroutine sitpln(icomp)
#include "building.h"

      common/spad/mmod,limit,limtty

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      character direc*1

C Return if not in graphic mode.
      ier=0
      if(mmod.lt.8)then
        call usrmsg('Not in graphic mode!',' ','W')
        return
      endif

C Use plelv to draw the site plan for the current zone.
      nzg=1
      nznog(1)=icomp
      izgfoc=icomp
      direc='P'
      call PLELEV(direc)
      
      return
      end

C ********** SUNPLT **********
C Draws a Mercator sun path diagram.

      subroutine sunplt(ijday)

      common/spad/mmod,limit,limtty
      common/prec8/slat,slon
      integer ifs,itfs,imfs
      common/gfont/ifs,itfs,imfs
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/viewpx/menuchw,igl,igr,igt,igb,igw,igwh

      character etext*72,blab*3,month*3
      dimension month(12)
      logical above
#ifdef OSI
      integer igwid,igheight  ! passed to axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer ipos,iupdown,isym    ! passed to etplot
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! passed to axiscale
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 ipos,iupdown,isym    ! passed to etplot
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif
      real xsc,ysc,sca,Xadd,Yadd  ! returned from axiscale

      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &           'Sep','Oct','Nov','Dec'/

C Clear the viewing box, draw its border and return the offsets
C that define it. Call to axiscale works better if
C local integer variables are used.
C Setup and pass in parameters to win3d.
      iiw1=8; iiw2=8; iiw3=4; iiw4=3; iimenu=menuchw
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
C      call win3d(menuchw,8,8,4,3,igl,igr,igt,igb,igw,igwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      igwid=igw
      igheight=igwh

C Reopen the dialogue box.
      if(mmod.eq.8)call usrmsg(' ',' ','-')

C Determine scaling ratios.
      call axiscale(igwid,igheight,0.,360.,0.,90.,xsc,ysc,sca,Xadd,Yadd)

C Place scaling in static C variables for use by line drawing routines.
      call linescale(iigl,Xadd,xsc,iigb,Yadd,ysc)

C Draw horizontal axis.
      call horaxis(0.,360.,iigl,iigr,iigb,Xadd,xsc,0,
     &  'Azimuth deg.')

C Draw vertical axes on left and right.
      call vrtaxis(0.,90.,iigl,iigb,iigt,Yadd,ysc,0,0,'Altitude deg.')
      call vrtaxis(0.,90.,iigr,iigb,iigt,Yadd,ysc,0,1,' ')

C Output a heading for the view.
      call edayr(ijday,ido,imo)
      iside=2
      isize=1
      ifont=ifs
      call viewtext('Mercator Sun Plot',iside,isize,ifont)
      write(etext,'(i2,1x,a3,a,f6.1,a,f5.1)')ido,month(imo),
     &                ' at Latitude',slat,', Longitude',slon
      iside=3
      isize=1
      call viewtext(etext,iside,isize,ifont)

C Plot sun path, for each hour of the day get the azimuth and
C elevation and plot that position on the graph with a symbol and
C time indication.
      above=.false.
      do 10 ihr=1,24
         stime=float(ihr)
         sazi=0.0
         salt=0.0
         call eazalts(stime,ijday,slat,slon,isunup,sazi,salt)
         if(salt.gt.0)then
            if(.not.above)then
               iupdown=0
               isym=0
               call etplot(sazi,salt,iupdown,isym)
               above=.true.
            endif
            iclkhr=ihr
            if(ihr.gt.12)iclkhr=ihr-12
            if(iclkhr.gt.9)then
               write(blab,'(i2)')iclkhr
               ipos=4
               isize=itfs
               call etlabel(blab,sazi,salt,ipos,isize)
            else
               write(blab,'(i1)')iclkhr
               ipos=4
               isize=itfs
               call etlabel(blab,sazi,salt,ipos,isize)
            endif

C Plot a symbol at the sun position.
            iupdown=1
            isym=6
            call etplot(sazi,salt,iupdown,isym)
         endif
   10 continue
      call forceflush()
      return
      end
