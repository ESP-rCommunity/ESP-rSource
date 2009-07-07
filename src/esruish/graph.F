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
      common/pophelp/h(60)
      common/spad/mmod,limit,limtty
      common/g1t/xft(mv),zft(mv)
      common/headl/irecs(ms)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/viewpx/menuchw,igl,igr,igt,igb,igw,igwh
      common/c24/izstocn(mcom,ms)

      character outs*124,h*72
      dimension ioshd(mox,moz),iva(ms)
      logical ok,dok
      integer icc,is,nv   ! local variables for connextion and vertices
      
#ifdef OSX
      integer igwid,igheight  ! for use with axiscale
#else
      integer*8 igwid,igheight  ! for use with axiscale
#endif

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
      call win3d(menuchw,7,4,4,3,igl,igr,igt,igb,igw,igwh)
      igw=igr-igl
      igwh=igb-igt
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
      call linescale(igl,Xadd,sca,igb,Zadd,sca)

C Output start hour.
      ih=1
      h(1)='Give the hour as an integer between 1 and 24.'
      call easki(ih,' ','Output start hour?',
     &     1,'F',24,'F',1,'output start',ier,0)

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
         call askok(' ','Next hour?',ok,dok,0)
         if(.not.ok)goto 1
   20 continue

    1 dok=.true.
      call askok(' ','Consider another surface?',ok,dok,0)
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

      common/contr/mon,isc(ms),iyd
      common/g1T/xft(mv),zft(mv)
      common/c24/izstocn(mcom,ms)
      common/g6/ssname(mcon),ssotf(mcon),ssmlcn(mcon),ssvfc(mcon),
     &          ssother(mcon,3),ssparent(mcon),ssuse(mcon,2)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/viewpx/menuchw,igl,igr,igt,igb,igw,igwh

      dimension month(12)
      character*72 etext*72,month*3
      character ssname*12,ssotf*32,ssmlcn*32,ssvfc*4,ssother*24,
     &          ssparent*12,ssuse*8
#ifdef OSX
      integer iside,isize,ifont     ! passed to viewtext
      integer ipos,iupdown,isym     ! passed to etplot
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 ipos,iupdown,isym     ! passed to etplot
#endif

      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

C Display header information. Calculate graphic boundaries.
      call win3d(menuchw,7,4,3,3,igl,igr,igt,igb,igw,igwh)
      igw=igr-igl
      igwh=igb-igt
      icn=izstocn(icomp,isur)

      write(etext,1)month(mon),ihour,isur,ssname(icn)
    1 format('Direct shading: ',a3,', @ ',i2,':00, Surface ',i2,
     &       ' (',a,').')
      iside=1
      isize=1
      ifont=2
      call viewtext(etext,iside,isize,ifont)

C Draw horizontal axis and move to start point.
      call horaxis(xmin,xmax,igl,igr,igb,Xadd,sca,0,'Horizontal')

C Draw vertical axis.
      call vrtaxis(zmin,zmax,igl,igb,igt,Zadd,sca,0,0,'Vertical')

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

#ifdef OSX
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
#include "geometry.h"

      common/spad/mmod,limit,limtty

      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)

      integer izstocn
      common/c24/izstocn(mcom,ms)
      common/prec8/slat,slon
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/viewpx/menuchw,igl,igr,igt,igb,igw,igwh
      integer ifs,itfs,imfs
      common/gfont/ifs,itfs,imfs

      character sstr*72,blabel*4
      character tz*13
#ifdef OSX
      integer igwid,igheight
      integer iside,isize     ! passed to viewtext
      integer iupdown,isym,ifont    ! passed to etplot
      integer iid1,iid2,iix,iiy
#else
      integer*8 igwid,igheight
      integer*8 iside,isize     ! passed to viewtext
      integer*8 iupdown,isym,ifont    ! passed to etplot
      integer*8 iid1,iid2,iix,iiy
#endif

C The right offset for the graphic box is the character width
C of the menu. Also allow 2 text lines at the bottom for future
C dialogue. Axiscale is in wwlib.c.
      ier=0
      if(mmod.lt.8)then
        call usrmsg('Not in graphic mode!',' ','W')
        return
      endif

C Define a viewing box, draw its border after clearing it and
C return the offsets that define it and set local variables.
      call win3d(menuchw,7,4,4,3,igl,igr,igt,igb,igw,igwh)
      igw=igr-igl
      igwh=igb-igt
      igwid=igw
      igheight=igwh

      call startbuffer()

C Reopen the dialogue box.
      call usrmsg(' ',' ','-')

C Establish the maximum and minimum vertix values.
      xmx=-1.E+8
      ymx=-1.E+8
      xmn=1.E+8
      ymn=1.E+8
      do 10 i=1,nztv(icomp)
        xmn=amin1(xmn,VCOORD(icomp,i,1))
        ymn=amin1(ymn,VCOORD(icomp,i,2))
        xmx=amax1(xmx,VCOORD(icomp,i,1))
        ymx=amax1(ymx,VCOORD(icomp,i,2))
   10 continue

C Convert obstruction block geometry to (/gb1/) and
C establish maximum and minimum site coordinates.
      do 20 i=1,nbobs(icomp)
        call cnvblk(xob(icomp,i),yob(icomp,i),zob(icomp,i),
     &    dxob(icomp,i),dyob(icomp,i),dzob(icomp,i),bangob(icomp,i))
        do 30 j=1,8
          xmn=amin1(xmn,xb(j))
          ymn=amin1(ymn,yb(j))
          xmx=amax1(xmx,xb(j))
          ymx=amax1(ymx,yb(j))
   30   continue
   20 continue

C Determine scaling ratios for the drawing and axis.
      call axiscale(igwid,igheight,xmn,xmx,ymn,ymx,xsc,ysc,sca,
     &  Xadd,Yadd)

C Place scaling in static C variables for use by line drawing routines.
      call linescale(igl,Xadd,sca,igb,Yadd,sca)

C Draw a scale horizontal axis, move to beginning point.
      call horaxis(xmn,xmx,igl,igr,igb,Xadd,sca,1,'East ->')

C Draw a scale vertical axis.
      call vrtaxis(ymn,ymx,igl,igb,igt,Yadd,sca,1,0,'^ North')

C Plot the zone and the zone name.
      call u2pixel(VCOORD(icomp,1,1),VCOORD(icomp,1,2),iix,iiy)
      iupdown=0
      isym=0
      call etplot(VCOORD(icomp,1,1),VCOORD(icomp,1,2),iupdown,isym)
      if(lnzname(icomp).gt.1)then
         write(tz,'(a)')zname(icomp)
         call winfnt(itfs)
         iid1=iix+2; iid2=iiy-2;
         call wstxpt(iid1,iid2,tz)
      else
         call winfnt(itfs)
         iid1=iix+2; iid2=iiy-2;
         call wstxpt(iid1,iid2,'Target')
      endif

C For each corner recover coordinates for the connection and plot.
      iupdown=0
      isym=0
      do 40 i=1,nzsur(icomp)
        icc=IZSTOCN(icomp,i)
        j=NZJVN(icc,1)
        call etplot(VCOORD(icomp,j,1),VCOORD(icomp,j,2),iupdown,isym)
        k=NZNVER(icc)
        iupdown=-302
        do 50 l=2,k
          m=NZJVN(icc,l)
          call etplot(VCOORD(icomp,m,1),VCOORD(icomp,m,2),iupdown,isym)
   50   continue
        call etplot(VCOORD(icomp,j,1),VCOORD(icomp,j,2),iupdown,isym)
   40 continue

C Convert obstruction block geometry to (/gb1/) and plot.
      if(nbobs(icomp).eq.0)goto 1
      do 60 ib= 1,nbobs(icomp)
        call cnvblk(xob(icomp,ib),yob(icomp,ib),zob(icomp,ib),
     &    dxob(icomp,ib),dyob(icomp,ib),dzob(icomp,ib),
     &    bangob(icomp,i))
        iupdown=0
        isym=0
        do 70 i=1,6
          j=jvnb(i,1)
          call etplot(xb(j),yb(j),iupdown,isym)
          k=4
          iupdown=1
          do 80 l=2,k
            m=jvnb(i,l)
            call etplot(xb(m),yb(m),iupdown,isym)
   80     continue
          call etplot(xb(j),yb(j),iupdown,isym)
   70    continue

         call u2pixel(xb(J),yb(J),iix,iiy)
         write(blabel,2)ib
    2    format('B',i2)
         iid1=iix+2; iid2=iiy-2;
         call wstxpt(iid1,iid2,blabel)
   60 continue

C Print labels and return to default font.
    1 write(sstr,'(a,f5.1,a,f4.1)')'Site Plan, Latitude ',slat,
     &  ' Longitude ',slon
      iside=1
      isize=1
      ifont=2
      call viewtext(sstr,iside,isize,ifont)
      call forceflush()
      call winfnt(ifs)

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
#ifdef OSX
      integer igwid,igheight  ! passed to axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer ipos,iupdown,isym    ! passed to etplot
#else
      integer*8 igwid,igheight  ! passed to axiscale
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 ipos,iupdown,isym    ! passed to etplot
#endif
      real xsc,ysc,sca,Xadd,Yadd  ! returned from axiscale

      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &           'Sep','Oct','Nov','Dec'/

C Clear the viewing box, draw its border and return the offsets
C that define it. On 64 bit machines igw and igwh can be incorrectly
C returned so re-calculate. Call to axiscale works better if
C local integer variables are used.
      call win3d(menuchw,8,8,4,3,igl,igr,igt,igb,igw,igwh)
      igw=igr-igl
      igwh=igb-igt
      igwid=igw
      igheight=igwh

C Reopen the dialogue box.
      if(mmod.eq.8)call usrmsg(' ',' ','-')

C Determine scaling ratios.
      call axiscale(igwid,igheight,0.,360.,0.,90.,xsc,ysc,sca,Xadd,Yadd)

C Place scaling in static C variables for use by line drawing routines.
      call linescale(igl,Xadd,xsc,igb,Yadd,ysc)

C Draw horizontal axis.
      call horaxis(0.,360.,igl,igr,igb,Xadd,xsc,0,
     &             'Azimuth deg.')

C Draw vertical axes on left and right.
      call vrtaxis(0.,90.,igl,igb,igt,Yadd,ysc,0,0,'Altitude deg.')
      call vrtaxis(0.,90.,igr,igb,igt,Yadd,ysc,0,1,' ')

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
