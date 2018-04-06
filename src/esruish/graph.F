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

C Ask user for the surface to focus on.If nothing selected
C ask user what to do.
    2 call pickssur(icomp,np,'L',iva,ier)
      if(np.eq.0)then
        goto 1    ! ask user whether to continue
      else
        is=iva(1)
        if(is.eq.0) goto 1   ! ask user whether to continue
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
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      icn=izstocn(icomp,isur)

      write(etext,1)month(mon),ihour,isur,ssname(icn)
    1 format('Direct shading: ',a3,', @ ',i2,':00, Surface ',i3,
     &       ' (',a,').')
      iside=1
      isize=1
      ifont=2
      call viewtext(etext,iside,isize,ifont)

C Draw horizontal axis and move to start point.
      call dintervalf(xmin,xmax,DY,NDEC,0)
      call horaxisdd(xmin,xmax,iigl,iigr,iigb,Xadd,sca,0,
     &  DY,NDEC,'Horizontal')

C Draw vertical axis.
      call dintervalf(zmin,zmax,DY,NDEC,0)
      call vrtaxisdd(zmin,zmax,iigl,iigb,iigt,Zadd,sca,0,
     &  DY,NDEC,0,'Vertical')

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
      real xxop,zzop  ! to pass to point1

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
               xxop=xop(i,j); zzop=zop(i,j)  ! to pass to point1
               call point1(izone,is,xxop,zzop,i,j,ians)
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
      call dintervalf(0.,360.,DY,NDEC,0)
      call horaxisdd(0.,360.,iigl,iigr,iigb,Xadd,xsc,0,
     &  DY,NDEC,'Azimuth deg.')

C Draw vertical axes on left and right.
      call dintervalf(0.,90.,DY,NDEC,0)
      call vrtaxisdd(0.,90.,iigl,iigb,iigt,Yadd,ysc,0,
     &  DY,NDEC,0,'Altitude deg.')
      call vrtaxisdd(0.,90.,iigr,iigb,iigt,Yadd,ysc,0,
     &  DY,NDEC,1,' ')

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

C ********** shadeview3d
      subroutine shadeview3d(izone,imon,igday,vtime,its,ier)
C Assumes that MESH3D has already been called.
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"

C Parameters
      integer izone   ! focus zone
      integer imon    ! focus month
      integer igday   ! day of the month
      real vtime      ! visualisation time
      integer its     ! surface to place mesh on
      integer ier     ! returns 1 if sun not up

      common/tracech/icout
      common/tc/itc,icnt
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN

C Mesh for surface as points in 3D.
      common/grid3/ogrida(ms)
      real x3op,y3op,z3op
      common/grid31/x3op(mox,moz),y3op(mox,moz),z3op(mox,moz)
      integer izstocn
      common/c24/izstocn(mcom,ms)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      common/cai/caii(ms),caie(ms)
      common/sangl1/sazi1,salt1
      common/prec8/slat,slon

C Common blocks for expanded obstructions.
      real SHXB  ! X coord of an obstruction polygon
      real SHYB  ! Y coord of an obstruction polygon
      real SHZB  ! Z coord of an obstruction polygon
      integer JVNBSH ! ordered list of edges of obstruction polygons
      real SHEQN ! equation of each obstruction polygon
      real shpazi ! azimuth of each obstruction polygon
      real shpelv ! elevation of each obstruction polygon
      real BBO    ! bounding box LL & UR for each obstruction
      COMMON/SHDGB1/SHXB(MB,12),SHYB(MB,12),SHZB(MB,12),
     &  JVNBSH(MB,6,4),SHEQN(MB,6,4),shpazi(MB,6),shpelv(MB,6),
     &  BBO(MB,2,3)
      real caiiob,caieob  ! similar to caii caie
      common/caiob/caiiob(MB,6),caieob(MB,6)

      common/shad3/ioshd(mox,moz),foshd(mox,moz),gssa,ipexcl
      common/shad4/oshd(mox,moz),ihit(mox,moz)

      character OFBC*1   ! obstruction front back cross
      common/ofbcarray/ofbc(ms,mb)
      character FBC*1    ! surface front back cross
      common/fbcarray/fbc(ms,mcon)

      logical tok,ok   ! for trace
      character mode*3
      character etext*72
      
C 3D Points on the grid which are shaded (for the current hour for each of
C the focus surface grid points.
      real x3intrs,y3intrs,z3intrs
      common/grid32/x3intrs(mox,moz),y3intrs(mox,moz),z3intrs(mox,moz)
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

      character month*3
      dimension month(12)
      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &           'Sep','Oct','Nov','Dec'/

      DIMENSION VP(3),EQN(4)
      integer ipoints
      dimension p(3),q(3),ipoints(6,2)  ! for arrow
      real XXuv,YYuv,ZZuv  ! 1 unit inwards
      character outs*124
      integer n,j
      logical inside ! true if inside
      logical proceed  ! true if can see face
      logical repeat ! true to switch time or surface
      real tol  ! slight offset
      real op   ! obstruction optical

C Get the current connection and number of surfaces in zone.
  43  mode='dir'  ! direct shading in this view
      icc=izstocn(izone,its)
      iccf=izstocn(izone,its)
      gsa=ssna(iccf)
      tol=0.001 ! for surfaces along a cardinal axis will have one

C If trace is on and verbose set tok, if not so verbose set ok.
      tok=.false.; ok=.false.
      if(icout.eq.33.and.itc.eq.2)tok=.true.
      if(icout.eq.33.and.itc.eq.1)ok=.true.

C Find bounding boxes of surfaces in this model.
      call findsurbox()

C Establish, for each surface in the current zone which other
C surfaces in the model are in front or behind or crossing it.
      call frontsurbox(izone)

C Instanciate derived properties of obstructions e.g. bounding
C boxes and the angles for the obstruction faces.
      call precobs(izone)

C Establish whether obstr are in front of or behind each 
C surface in the zone.
      call frontobsbox(izone)

C TEST: Ask hour for the solar azimuth and altitude angles relative to the
C original coordinate system.
  42  call eday(igday,imon,julian)
      stime=vtime
      call eazalts(stime,julian,slat,slon,isunup,sazi1,salt1)

C Abandon calculation for this hour if before sunrise or after sunset.
      if(isunup.eq.0)then
        call usrmsg('The sun is not up.','Please respecify','W')
        ier=1
        return
      endif

C Reset.
      gssa=0.0     ! related to obstructions
      gselfsa=0.0  ! related to self-shading
      itothits=0  ! reset counter for nb of grid hits for surface 

C This section graphically displays shading from obstructions.
C Redraw the wireframe and the grid.
      if(mmod.eq.8)then
        MODIFYVIEW=.true.
        modlen=.true.
        itsnm=1
        CALL INLNST(1)
        nzg=1; nznog(1)=izone; izgfoc=izone
        CALL ADJVIEW(IER)
      endif

C Draw the grid on the surface.
      do i=1,nox(izone)
        do j=1,noz(izone)
          XXW=x3op(i,j)
          YYW=y3op(i,j)
          ZZW=z3op(i,j)
          call i3pippa(izone,its,xxw,yyw,zzw,i,j,i3pip)
          if(i3pip.eq.0)then
            continue  ! grid point not within focus surface
          else
            if(mmod.eq.8) call wiresymbol(xxw,yyw,zzw,1,32)
          endif
        enddo ! of j
      enddo   ! of i

C Heading on graph.
      write(etext,'(3a,f6.2,3a,f7.2,a,f7.2)')'Direct shading: ',
     &  month(imon),' @ ',vtime,' ',ssname(iccf),' azim',sazi1,
     &  ' elev',salt1
      iside=1
      isize=1
      ifont=2
      call viewtext(etext,iside,isize,ifont)

C For current solar azimuth and altitude get unit vector and then
C create a unit vector pointing to the sun.
      call  AZ2UV(sazi1,salt1,vdx,vdy,vdz)
      uvdx=1.0*vdx; uvdy=1.0*vdy; uvdz=1.0*vdz

C Draw an arrow to the centre grid point along the sun ray. 
      auvdx=1.0*vdx; auvdy=1.0*vdy; auvdz=1.0*vdz
      P(1)=x3op(10,10); P(2)=y3op(10,10); P(3)=z3op(10,10)
      Q(1)=x3op(10,10)+auvdx; Q(2)=y3op(10,10)+auvdy 
      Q(3)=z3op(10,10)+auvdz
      shaft=2.0; al=0.3; aw=0.15
      call arrow(P,Q,al,aw,ipoints,'a',2)
      call pausems(100)

C Clear arrays for direct shading.
      do m=1,nox(izone)
        do n=1,noz(izone)
          XXW=x3op(m,n)
          YYW=y3op(m,n)
          ZZW=z3op(m,n)
          call i3pippa(izone,its,xxw,yyw,zzw,m,n,i3pip)
          ihit(m,n)=0
          oshd(m,n)=0.0
          ioshd(m,n)=0
          if(i3pip.eq.0)ioshd(m,n)=-1

C Clear xzy of shaded point on the target surface.
          x3intrs(m,n)=0.0; y3intrs(m,n)=0.0; z3intrs(m,n)=0.0
        enddo  ! of n
      enddo    ! of m

C Establish the visibility of the solar angle (sazi1 salt1)
C for obstructions at the current time (julian ihr) 
      va=0.0; vb=0.0
      ihr=nint(vtime)
      call incaiobs(izone,julian,ihr,va,vb)
      write(6,*)'current azi alt',ihr,sazi1,salt1

C Establish visibility of the solar angle with the surfaces
C in the zone for the current zone and hour.
      call incai(izone,ihr)

C If the focus surface faces away from the sun then just set
C ioshd to 1 for grid points within the surface.
      if(caie(its).lt.0.0)then
        if(mode.eq.'dir')then
          do i=1,nox(izone)
            do j=1,noz(izone)
              if(ioshd(i,j).eq.-1)then
                continue
              else

C Facing away so remember this and save all the grid points to draw.
                ioshd(i,j)=1
                x3intrs(i,j)=x3op(i,j); y3intrs(i,j)=y3op(i,j)
                z3intrs(i,j)=z3op(i,j)
              endif
            enddo  ! of j
          enddo    ! of i
        elseif(mode.eq.'dif')then

C Increment the number of sky patches that are behind the target
C surface.
          ipexcl=ipexcl+1
        endif

C Now draw the shaded points. irpt is 1 for test
        irpt=1
        call drawshadedgrid(izone,its,irpt,2)
      endif
      
C Loop through the obstructions that are in front of or cross
C the current surface.
      if(nbobs(izone).gt.0)then
        do 66 ib=1,nbobs(izone)
          if(ofbc(ITS,ib).eq.'B') goto 66
C Obstruction faces: front v1 v2 v6 v5
C right v2 v3 v7 v6, back v3 v4 v8 v7
C left v4 v1 v5 v8, top v5 v6 v7 v8, base  v1 2 3 4.
          do iface=1,6

C We already know which of the faces faces away so
C can exclude them from intersection test.
            if(caieob(ib,iface).lt.0.0)then
C              write(6,*)'consider ib iface',ib,iface
              proceed=.true.
            else
C              write(6,*)'face away ib iface',ib,iface
              proceed=.false.
            endif
            if(proceed)then

C Recover equation of plane of obstruction face.
              EQN(1)=SHEQN(ib,iface,1)
              EQN(2)=SHEQN(ib,iface,2)
              EQN(3)=SHEQN(ib,iface,3)
              EQN(4)=SHEQN(ib,iface,4)

C Loop through all the grid points of the surface testing first
C if the grid point within the surface?
              do 100 m=1,nox(izone)
                do 110 n=1,noz(izone)

                  XXuv=x3op(m,n)+uvdx; YYuv=y3op(m,n)+uvdy
                  ZZuv=z3op(m,n)+uvdz
                  xxw=x3op(m,n); yyw=y3op(m,n); zzw=z3op(m,n)
                  call i3pippa(izone,its,xxw,yyw,zzw,m,n,i3pip)
                  if(i3pip.eq.0)then
                    goto 110  ! grid point not within focus surface
                  else
                    if(mmod.eq.8) call wiresymbol(xxw,yyw,zzw,1,32)
                  endif

C Project from grid point to the obstruction polygon face and find
C point of intersection x3 y3 z3
                  CALL VECPLN(x3op(m,n),y3op(m,n),z3op(m,n),
     &              XXuv,YYuv,ZZuv,EQN, x3,y3,z3, IERR)
C                  call wiresymbol(x3,y3,z3,2,32)

C Debug.
C                  write(6,*) 'caieob is ',caieob(ib,iface)
C                  write(6,*) 'caiiob is ',caiiob(ib,iface)
C                  write(6,*) 'solar is ',sazi1,salt1
C                  write(6,*) 'obs face azi elev ',
C     &              shpazi(ib,iface),shpelv(ib,iface)
              
C Check if intersection x3 y3 z3 is within obstruction BBO
                  inside=.true.  ! assume inside
                  if(x3.lt.(BBO(ib,1,1)-tol))then
                    inside=.false.
                  elseif(y3.lt.(BBO(ib,1,2)-tol))then
                    inside=.false.
                  elseif(z3.lt.(BBO(ib,1,3)-tol))then
                    inside=.false.
                  elseif(x3.gt.(BBO(ib,2,1)+tol))then
                    inside=.false.
                  elseif(y3.gt.(BBO(ib,2,2)+tol))then
                    inside=.false.
                  elseif(z3.gt.(BBO(ib,2,3)+tol))then
                    inside=.false.
                  endif
C                  write(6,*)'is it inside ',ib,iface,inside
                  if(inside)then

C Within bounding box so test containment in the obstruction polygon.
                    xxw=x3; yyw=y3; zzw=z3
                    call i3piobs(izone,ib,iface,xxw,yyw,zzw,m,n,i3pio)
                    if(i3pio.ne.0)then
C                      write(6,*)'i3piobs for ',izone,ib,iface,xxw,yyw,
C     &                  zzw,i3pio
                      if(mmod.eq.8)then

C Remember where the shaded point is.
                        call wiresymbol(x3,y3,z3,3,32)
                        xxw=x3op(m,n); yyw=y3op(m,n); zzw=z3op(m,n)
                        x3intrs(m,n)=xxw; y3intrs(m,n)=yyw
                        z3intrs(m,n)=zzw
C                        call wiresymbol(xxw,yyw,zzw,2,33)
C                        call pausems(50)
                      endif

C Point obscured by current obstruction block face. Point shaded
C if any face obscures so only increment oshd for first hit, ignoring
C other hits.
                      ihit(m,n)=ihit(m,n)+1
                      ioshd(m,n)=1
                      itothits=itothits+1  !remember for the whole surface

C If more than one obstruction block shaded then opacities need to be
C compounded.
                      op=1.0  ! opacity guess
                      if(ihit(m,n).eq.1)oshd(m,n)=
     &                   1.0-(1.0-oshd(m,n))*(1.0-op)
                      if(oshd(m,n).gt.1.0)oshd(m,n)=1.0
                    endif
                  endif
  110           continue
  100         continue
            endif  ! of proceed
          enddo    ! of iface
   66   continue
      endif        ! of any obstructions

C Dump intermediate calculations.
      if(tok.or.ok)then
        if(itothits.eq.0)then
          write(icout,*) 'no obstruction hits '
        else
          if(tok)write(icout,*) 'obstructions dumping ihit '
          if(tok)call dumpnoxnozi(izone,ihr,ihit)
          write(icout,*) 'obstructions dumping ioshd '
          call dumpnoxnozi(izone,ihr,ioshd)
          write(icout,*) 'obstructions dumping oshd '
          call dumpnoxnozr(izone,ihr,oshd)
        endif
      endif

C This section graphically displays self-shading.
C Now draw the shaded points. irpt is 1 for test
      irpt=1
      call drawshadedgrid(izone,its,irpt,2)

C Now do the loop looking at surfaces in the zone.
C Filter: not the focus surface, must face outside, should not
C face away from the ray.

C For each surface do initial filtering.
      do 111 loop=1,nzsur(izone)
        icc=izstocn(izone,loop)
        if(loop.eq.its)then
          goto 111   !  skip self
        elseif(caie(loop).lt.0.0)then
          goto 111  ! cannot be seen by solar vector
        elseif(fbc(its,icc).eq.'B')then
          goto 111  ! other surface is behind
        elseif(ssother(icc,1)(1:7).eq.'SIMILAR')then
          goto 111  ! do not face outside
        endif
        if(ssother(icc,1)(1:8).eq.'EXTERIOR')then
             
          EQN(1)=ssureqn(icc,1)
          EQN(2)=ssureqn(icc,2)
          EQN(3)=ssureqn(icc,3)
          EQN(4)=ssureqn(icc,4)

          do 200 m=1,nox(izone)
            do 210 n=1,noz(izone)

C Add unit vector to the sun path to the current mesh cell.
              XXuv=x3op(m,n)+uvdx; YYuv=y3op(m,n)+uvdy
              ZZuv=z3op(m,n)+uvdz
              xxw=x3op(m,n); yyw=y3op(m,n); zzw=z3op(m,n)
              call i3pippa(izone,its,xxw,yyw,zzw,m,n,i3pip)
              if(i3pip.eq.0)then
                goto 210  ! grid point not within focus surface
              else
                if(mmod.eq.8) call wiresymbol(xxw,yyw,zzw,1,32)
              endif

              CALL VECPLN(x3op(m,n),y3op(m,n),z3op(m,n),
     &          XXuv,YYuv,ZZuv,EQN, x3,y3,z3, IERR)

C Is this intersection within the bounding box of the receiving surface?
              call insurbox(x3,y3,z3,izone,loop,inside)
              if(inside)then
                call i3pippa(izone,loop,x3,y3,z3,m,n,i3pip)
C                write(6,*) 'i3pippa for ',izone,loop,x3,y3,z3,i3pip
                if(i3pip.ne.0)then

C Point fully obscured (self shading surfaces are assumed to be opaque.
                  ihit(m,n)=ihit(m,n)+1
                  itothits=itothits+1  !remember for the whole surface
                  ioshd(m,n)=1
                  oshd(m,n)=1.0  ! assume opaque

                  if(mmod.eq.8)then

C Remember where the shaded point is.
                    xxw=x3op(m,n); yyw=y3op(m,n); zzw=z3op(m,n)
                    x3intrs(m,n)=xxw; y3intrs(m,n)=yyw
                    z3intrs(m,n)=zzw
                        
C Draw a bigger blob on the focus grid point to show it is shaded.
C                    call wiresymbol(xxw,yyw,zzw,2,33)
C                    call pausems(10)
                  endif
                endif
              endif
  210       continue
  200     continue
        endif
 111  continue  ! of loop

C Dump intermediate calculations.
      if(tok.or.ok)then
        if(itothits.eq.0)then
          write(icout,*) 'no self-shade hits '
        else
          if(tok)write(icout,*) 'surfaces dumping ihit '
          if(tok)call dumpnoxnozi(izone,ihr,ihit)
          write(icout,*) 'surfaces dumping ioshd '
          call dumpnoxnozi(izone,ihr,ioshd)
          if(tok)write(icout,*) 'surfaces dumping oshd '
          if(tok)call dumpnoxnozr(izone,ihr,oshd)
        endif
      endif

C Now draw the shaded points. irpt is 1 for test
      irpt=1
      call drawshadedgrid(izone,its,irpt,2)
      call pauses(1)

C Sum the shading
      do k=1,nox(izone)
        do l=1,noz(izone)
          if(ioshd(k,l).eq.-1)then
            continue
          else
            if(mode.eq.'dir') gssa=gssa+oshd(k,l)*ogrida(its)
            if(mode.eq.'dif') gssa=gssa+foshd(k,l)*ogrida(its)
            if(mode.eq.'dir') gselfsa=gselfsa+oshd(k,l)*ogrida(its)
            if(mode.eq.'dif') gselfsa=gselfsa+foshd(k,l)*ogrida(its)
          endif
        enddo  ! of l
      enddo    ! of k
      write(outs,'(a,f8.4,a,f8.4,a,f8.4,a,i3)')
     &  'Shading area = ',gssa,' Surface area = ',gsa,
     &  ' Grid area =',ogrida(its),' total hits ',itothits
      call edisp(iuout,outs)
      write(outs,'(a,f7.3)') 'direct shading is ',gssa/gsa
      call edisp(iuout,outs)

C Adjust diffuse shading to relate only to the sky portion that is
C visible to the target surface. This is required because the calculation
C of the diffuse irradiance in bps does not include the obscured
C sky portion.

C Ask for a different time or exit.
      repeat=.false.
      iopt=0
      call easkatog('Display options:',' ','-60m','-30m','+30m','+60m',
     &              'another surface','cancel',' ',iopt,nbhelp)
      if(iopt.eq.1)then
        vtime=vtime-1.0
        repeat=.true.
      elseif(iopt.eq.2)then
        vtime=vtime-0.5
        repeat=.true.
      elseif(iopt.eq.3)then
        vtime=vtime+0.5
        repeat=.true.
      elseif(iopt.eq.4)then
        vtime=vtime+1.0
        repeat=.true.
      elseif(iopt.eq.5)then

C Confirm other surface, create grid, clear arrays.
        call easki(its,' ','Index of surface to display?',
     &    1,'F',36,'F',1,'surf index',ier,nbhelp)
        nox(izone)=20; noz(izone)=20
        call mesh3d(izone,its,'-')
        gssa=0.0     ! related to obstructions
        gselfsa=0.0  ! related to self-shading
        itothits=0  ! reset counter for nb of grid hits for surface 
        do m=1,nox(izone)
          do n=1,noz(izone)
            ihit(m,n)=0
            ioshd(m,n)=0
            oshd(m,n)=0.0
            x3intrs(m,n)=0.0; y3intrs(m,n)=0.0; z3intrs(m,n)=0.0
          enddo  ! of n
        enddo    ! of m
        goto 43
      elseif(iopt.eq.6)then
        return
      endif
      if(repeat)then

C Clear arrays.
        gssa=0.0     ! related to obstructions
        gselfsa=0.0  ! related to self-shading
        itothits=0  ! reset counter for nb of grid hits for surface 
        do m=1,nox(izone)
          do n=1,noz(izone)
            ihit(m,n)=0
            ioshd(m,n)=0
            oshd(m,n)=0.0
            x3intrs(m,n)=0.0; y3intrs(m,n)=0.0; z3intrs(m,n)=0.0
          enddo  ! of n
        enddo    ! of m
        goto 42
      endif

      end

C ***** drawshadedgrid
C drawshadedgrid displays the currently shaded grid points on the
C wire-frame view. 
C << generalize this >>
      subroutine drawshadedgrid(izone,its,irpt,icolour)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
      integer izone  ! focus zone
      integer its    ! focus surface
      integer irpt   ! implied delay
      integer icolour ! 1 is black 2 is russet
      
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      
C 3D Points on the grid which are shaded (for the current hour for each of
C the focus surface grid points.
      real x3intrs,y3intrs,z3intrs
      common/grid32/x3intrs(mox,moz),y3intrs(mox,moz),z3intrs(mox,moz)
      common/shad3/ioshd(mox,moz),foshd(mox,moz),gssa,ipexcl
      integer m,n
      real x3,y3,z3
      
      if(mmod.eq.8)then
        do m=1,nox(izone)
          do n=1,noz(izone)
            if(ioshd(m,n).eq.1)then   ! if shaded
              x3=x3intrs(m,n); y3=y3intrs(m,n); z3=z3intrs(m,n)
              call wiresymbol(x3,y3,z3,icolour,33)
              if(irpt.eq.2)call pausems(4)   ! medium pause
              if(irpt.eq.3)call pausems(6)   ! longer pause
            endif
          enddo ! of n
          if(irpt.eq.1)call pausems(3)     ! slight pause for column
        enddo   ! of m
        call forceflush()
        if(irpt.eq.1)call pausems(100)      ! slight pause before diffuse
      else
        return
      endif

      return
      end
