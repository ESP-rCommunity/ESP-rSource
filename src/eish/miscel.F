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


C MISCEL.F contains the following routines used by ish.

C MESH:    computes centre point X&Z coord of grid squares.
C MESH3D:  computes 3D centre point of source grid.
C precobs: pre-computes obstruction polygons for the current zone
C incaiobs: for obstructions calculate eqivalent of caii and caie.
C SUR3DLEHI: 3D method to return overall length and height of a surface.
C ETRANSGRID: 3D method to create source grid point.
C PICKSSUR: selects surfaces for shading & insolation analysis.
C ASKDAY: returns 'imo' (month), 'ido' (day of month) and 'ijday' (day of year).
C POINT1   determine if a point is within the target surface
C POINT2   determine if a point is within a shadow polygon.
C ipippa: winding method containment checks - point in polygon (2D)
C i3pippa: winding method containment checks - point in polygon (3D)
C i3piobs: winding method containment checks - point in obstr polygon (3D)
C findsurbox: for each surface in the model find its length width and BB.
C insurbox: test if the 3D coordinate X Y Z is within surface BB.

C ******************** MESH ********************
C Computes the centre point 'x & y' coordinates for each rectangular
C grid cell as superimposed on the target surface. Note that some of
C these points will lie outside a non-rectangular target surface; such
C points are later excluded.

      subroutine mesh(izone,its)
#include "building.h"
#include "geometry.h"

C Parameters
      integer izone  ! focus zone
      integer its    ! surface to place mesh on

      common/g1t/xft(mv),zft(mv)
      common/grid3/ogrida(ms)
      common/grid11/xop(mox,moz),zop(mox,moz)
      integer izstocn
      common/c24/izstocn(mcom,ms)
      integer i

C Get the current connection and number of surfaces in zone.
      icc=izstocn(izone,its)

C Derive bounding box (in 2d) for surface 'its'. First find max and min
C x & y values for this surface by scanning each vertex.
      xmax=-1.E+10
      zmax=-1.E+10
      xmin=1.E+10
      zmin=1.E+10
      nv=isznver(izone,its)
      do i=1,nv
         xmax=amax1(xmax,xft(i))
         zmax=amax1(zmax,zft(i))
         xmin=amin1(xmin,xft(i))
         zmin=amin1(zmin,zft(i))
      enddo  ! of i

C Mesh length and height.
      dxmesh=xmax-xmin
      dzmesh=zmax-zmin

C Establish grid interval for this surface. Ogrida is the area of
C one grid cell.
      divx=dxmesh/nox(izone)
      divz=dzmesh/noz(izone)
      ogrida(its)=divx*divz

C Compute x & y values for centre point of each grid cell.
      do i=1,nox(izone)
         do j=1,noz(izone)
            xop(i,j)=divx/2.+(i-1)*divx
            zop(i,j)=divz/2.+(j-1)*divz
         enddo   ! of j
      enddo      ! of i
      return
      end

C ******************** MESH3D ********************
C Computes the centre point 'x & y' coordinates for each rectangular
C grid cell as superimposed on the target surface. Note that some of
C these points will lie outside a non-rectangular target surface; such
C points are later excluded.

      subroutine mesh3d(izone,its,act)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"

C Parameters
      integer izone   ! focus zone
      integer its     ! surface to place mesh on
      character act*1 ! if 't' enable testing

      common/tracech/icout
      common/tc/itc,icnt
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Mesh for surface as points in 3D.
      common/grid3/ogrida(ms)
      real x3op,y3op,z3op
      common/grid31/x3op(mox,moz),y3op(mox,moz),z3op(mox,moz)
      integer izstocn
      common/c24/izstocn(mcom,ms)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      common/cai/caii(ms),caie(ms)
      common/sangl1/sazi1,salt1
      integer mon,isc,iyd
      common/contr/mon,isc(ms),iyd
      common/prec8/slat,slon
      real BB(2,3)   ! bounding box in site coordinates

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

C ical = 0 indicates that no insolation calculations have been
C performed and is set to 1 when they have.
      common/data1/ical,idifc,init
      common/shad3/ioshd(mox,moz),foshd(mox,moz),gssa,ipexcl
      common/shad4/oshd(mox,moz),ihit(mox,moz)

      character OFBC*1   ! obstruction front back cross
      common/ofbcarray/ofbc(ms,mb)
      character FBC*1    ! surface front back cross
      common/fbcarray/fbc(ms,mcon)

      dimension npz(8),alt(8),azi(8)
      logical tok,ok   ! for trace
      character mode*3

C 3D Points on the grid which are shaded (for the current hour for each of
C the focus surface grid points.
      real x3intrs,y3intrs,z3intrs
      common/grid32/x3intrs(mox,moz),y3intrs(mox,moz),z3intrs(mox,moz)

      character month*3
      dimension month(12)
      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &           'Sep','Oct','Nov','Dec'/

C Sky vault discretisation.
      data npz/30,60,84,108,126,138,144,145/
      data alt/6.,18.,30.,42.,54.,66.,78.,90./
      data azi/12.,12.,15.,15.,20.,30.,60.,0./

      DIMENSION XX(mv),YY(mv),ZZ(mv)
      DIMENSION VP(3),EQN(4)
      integer ipoints
      dimension p(3),q(3),ipoints(6,2)  ! for arrow
      real XXuv,YYuv,ZZuv  ! 1 unit inwards
      character outs*124
      integer n,j
      logical inside ! true if inside
      logical proceed  ! true if can see face
      real tol  ! slight offset
      real op   ! obstruction optical

C Get the current connection and number of surfaces in zone.
      icc=izstocn(izone,its)
      gsa=sna(izone,its)
      tol=0.001 ! for surfaces along a cardinal axis will have one

C If trace is on and verbose set tok, if not so verbose set ok.
      tok=.false.; ok=.false.
      if(icout.eq.33.and.itc.eq.2)tok=.true.
      if(icout.eq.33.and.itc.eq.1)ok=.true.

C Find bounding boxes of surfaces.
      call findsurbox()

C Derive bounding box (in 3d) for surface 'its'. First find max and min
C x & y & z values for this surface by scanning each vertex.
      call SUR3DLEHI(izone,ITS,WID,HIGH,BB)
      if(tok)then
        call edisp(icout,' ')
        write(outs,'(a,i3,a,i3,a,f7.3,a,f7.3)')'*** MESH3D: zone',
     &    izone,' surf',its,' width',WID,' height',HIGH
        call edisp(icout,outs)
      endif

C Mesh length and height.
      dxmesh=WID
      dzmesh=HIGH

C Establish grid interval for this surface. Ogrida is the area of
C one grid cell.
      divx=dxmesh/nox(izone)
      divz=dzmesh/noz(izone)
      ogrida(its)=divx*divz
      if(tok)then
        write(icout,*)'dxmesh dzmesh divx divz ogrida',
     &    dxmesh,dzmesh,divx,divz,ogrida(its)
      endif

C Compute x & y & z values for centre point of each grid cell.
      NV = isznver(izone,its)
      DO J = 1,NV
        XX(J) = szcoords(izone,iszjvn(izone,its,J),1)
        YY(J) = szcoords(izone,iszjvn(izone,its,J),2)
        ZZ(J) = szcoords(izone,iszjvn(izone,its,J),3)
      ENDDO  ! of J

C For each grid point calculate offset from lower left and compute its
C site XYZ via etransgrid. Draw a dot depending if inside/outside.
      do i=1,nox(izone)
        do j=1,noz(izone)
           xoffset=divx/2.+(i-1)*divx
           zoffset=divz/2.+(j-1)*divz
           call ETRANSGRID(NV,XX,YY,ZZ,xoffset,zoffset,XXW,YYW,ZZW)
           x3op(i,j)=XXW
           y3op(i,j)=YYW
           z3op(i,j)=ZZW
           call i3pippa(izone,its,xxw,yyw,zzw,i,j,i3pip)
C           write(6,*) 'i3pippa for ',izone,its,xxw,yyw,zzw,i3pip
           if(i3pip.eq.0)then
             if(mmod.eq.8) call wiresymbol(xxw,yyw,zzw,2,32)
           else
             if(mmod.eq.8) call wiresymbol(xxw,yyw,zzw,1,32)
           endif
        enddo ! of j
      enddo   ! of i

      if(act.ne.'t') return

C Testing:
C Find bounding boxes of surfaces in this model.
      call findsurbox()

C Establish, for each surface in the current zone which other
C surfaces in the model are in front or behind or crossing it.
      call frontsurbox(izone)

C Instantiate derived properties of obstructions e.g. bounding
C boxes and the angles for the obstruction faces.
      call precobs(izone)

C Establish whether obstr are in front of or behind each 
C surface in the zone.
      call frontobsbox(izone)

C TEST: Ask hour for the solar azimuth and altitude angles relative to the
C original coordinate system.
  42  call eday(17,3,iyd)
      ihr=8
      call easki(ihr,' ','hour of the day?',
     &   0,'F',24,'F',1,'hour index',ier,nbhelp)
      if(ihr.eq.0) return
      stime=float(ihr)
      call eazalts(stime,iyd,slat,slon,isunup,sazi1,salt1)

C Abandon calculation for this hour if before sunrise or after sunset.
      if(isunup.eq.0)then
        call usrmsg('The sun is not up.','Please respecify','W')
        goto 42
      endif

C This section graphically displays shading from obstructions.
C Redraw the wireframe.
      if(mmod.eq.8)then
        MODIFYVIEW=.true.
        modlen=.true.
        itsnm=1
        CALL INLNST(1)
        nzg=1; nznog(1)=izone; izgfoc=izone
        CALL redraw(IER)
      endif

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
      call pausems(200)

C Clear arrays for direct shading and draw viable grid points.
      do m=1,nox(izone)
        do n=1,noz(izone)
          XXW=x3op(m,n)
          YYW=y3op(m,n)
          ZZW=z3op(m,n)
          call i3pippa(izone,its,xxw,yyw,zzw,m,n,i3pip)
          oshd(m,n)=0.0
          ioshd(m,n)=0
          if(i3pip.eq.0)then
            ioshd(m,n)=-1  ! grid point not within focus surface
          else
            if(mmod.eq.8) call wiresymbol(xxw,yyw,zzw,1,32)
          endif

C Clear xzy of shaded point on the target surface.
          x3intrs(m,n)=0.0; y3intrs(m,n)=0.0; z3intrs(m,n)=0.0
        enddo  ! of n
      enddo    ! of m


C Next loop looks at diffuse shading from obstructions and then
C from surfaces.
C Setup counters
C The outer loop is looking at each sky patch (2,146) This
C sets a solar vector.
C   if surface can see the patch continue
C     loop through the obstruction faces
C       loop across grid of the focus surface
C         if grid point on surface then
C           if an obstr face to be considered and
C           if an intersection increment ihit(m,n)
C             and ioshd(m,n) and ? for multiple obstructions which
C             have transparency & shdtst might be called
C           loop through all the applicable surfaces
C             if an intersection increment ihit(m,n)
C             and ioshd(m,n) for self shading

C Zero the shading calcs indicators.
      ical=0
      indcom=0
      kind=0
      knt=0
      ksu=0

C ksu is the number of sun-up hours
      do i=1,24
        stime=float(i)
        call eazalts(stime,iyd,slat,slon,isunup,sazi,salt)
        if(isunup.eq.1)ksu=ksu+1
      enddo ! of i

C knt is the total number of target surfaces in zone.
C      do i=1,nzsur(icomp)
C         if(isc(i).eq.1)knt=knt+1
C      enddo ! of i

C Estimate simulaton extent (surface-hours) to support the
C progress report.         
C      kt=ksu*knt

C Commence direct (mode=dir) and diffuse (mode=dif) shading computation
C for each target surface in turn.
      init=0
      
      ihour=ihr
      stime=float(ihour)

C Determine the solar azimuth and altitude angles relative to the
C original coordinate system.
      call eazalts(stime,iyd,slat,slon,isunup,sazi1,salt1)
      
      mode='dir'  ! for initial pass

C Remember sun position for this hour.
      sazir=sazi1
      saltr=salt1

C Abandon calculation for this hour if before sunrise or after sunset.
      if(isunup.eq.0)then

C Trace output.
        if(tok)then
          call edisp(icout,' ')
          write(outs,9997)ihour
 9997     format('*** SHADC: Sun not up at hour ',I2)
          call edisp(icout,outs)
        endif
        goto 42
      endif

C Initialise ipexcl, the number of sky patched 'behind' the
C target surface.
      ipexcl=0

C Compute direct (ip=1) then diffuse (ip=2,146, i.e. 145 sky
C patches) shading. Where the latter is not time dependent
C (e.g. geometry is time invariant and sky irradiance distribution
C is handled by bps), initiate only for the first sun-up time
C step.
      do 40 ip=1,146

        itothits=0  ! reset counter for nb of grid hits for surface 

C Clear oshd & ioshd arrays as done at 475 in shdcon.F
C Clear the foshd array as in shdcon.F line 430. And draw the
C grid on the surface.
        do i=1,nox(izone)
          do j=1,noz(izone)
            XXW=x3op(i,j); YYW=y3op(i,j); ZZW=z3op(i,j)
            call i3pippa(izone,its,xxw,yyw,zzw,i,j,i3pip)

C Reset foshd at start, setting any grid point not within surface
C to -1.0.
            if(ip.eq.2)foshd(i,j)=0.0
            oshd(i,j)=0.0
            ioshd(i,j)=0
            if(i3pip.eq.0)then
              foshd(i,j)=-1.0  ! grid point not within focus surface
              ioshd(i,j)=-1
            endif
          enddo ! of j
        enddo   ! of i

C Direct case.
        if(ip.eq.1)then
          mode='dir'
          gssa=0.0

C Trace output.
          if(tok)then
            call edisp(icout,' ')
            write(outs,'(a,i2)')
     &       '*** SHADC: calculating direct shading at hour ',
     &       ihour
            call edisp(icout,outs)
          endif
          va=sazi1; vb=salt1

C Diffuse case (only initilise gssa at start of sky vault processing).
        else
          mode='dif'
          if(ip.eq.2)then
            gssa=0.0     ! related to obstructions
            gselfsa=0.0  ! related to self-shading
          endif

C Trace output.
          if(tok)then
            call edisp(icout,' ')
            write(outs,'(a,i2,a,i3)')
     &        '*** SHADC: calculating diffuse shading at hour ',
     &        ihour,' for patch ',ip-1
            call edisp(icout,outs)
          endif

C Test whether to calculate diffuse shading again after first sun-up hour.
          if(idifc.eq.0.and.init.eq.1)then
            mode='dup'
C            call tfile3(icomp,itsur,mode)
            goto 40
          endif

          iptch=ip-1  ! set diffuse patch counter
          if(iptch.ge.1.and.iptch.le.30) nzone = 1
          if(iptch.ge.31.and.iptch.le.60) nzone = 2
          if(iptch.ge.61.and.iptch.le.84) nzone = 3
          if(iptch.ge.85.and.iptch.le.108) nzone = 4
          if(iptch.ge.109.and.iptch.le.126) nzone = 5
          if(iptch.ge.127.and.iptch.le.138) nzone = 6
          if(iptch.ge.139.and.iptch.le.144) nzone = 7
          if(iptch.eq.145) nzone = 8
          if(nzone.eq.8) then
            ptazi = 0.0
            ptalt = 90.0
          elseif(nzone.eq.1) then
            ptazi = azi(nzone)*(iptch - 1)
            ptalt = alt(nzone)
          else
            ptazi = azi(nzone)*((iptch - npz(nzone-1)) - 1)
            ptalt = alt(nzone)
          endif

C If current sky patch is also sun position then exclude from
C calculation.
          diff1=abs(sazir-ptazi)
          diff2=abs(saltr-ptalt)
          if(diff1.le.6.0.and.diff2.le.6.0)then
            if(tok.or.ok)then
              write(icout,*) '  '
              write(outs,'(a,i3,a,2f7.2)') 'Diffuse patch ',iptch,
     &          ' angles ',ptazi,ptalt
              write(icout,'(a,2f8.4,a)') 
     &          'Angles ',diff1,diff2,' less than 6 degrees'
            endif
            goto 40
          endif
          sazi1=ptazi; salt1=ptalt
          va=sazi1; vb=salt1
        endif

C Generate a fresh directional vector.
        call  AZ2UV(va,vb,vdx,vdy,vdz)
        uvdx=1.0*vdx; uvdy=1.0*vdy; uvdz=1.0*vdz

C Compute angles to figure out what obstruction faces can see the patch.
        ihh=0  ! override with current patch vector or sun vector
        call incaiobs(izone,iyd,ihh,va,vb)

C Re-compute angles for surfaces seeing the patch.
        call incai3(izone,ihh,va,vb)

C Debug.
        if(mode(1:3).eq.'dif')then
          if(tok.or.ok)then
            write(icout,*) '  '
            write(outs,'(a,i3,a,2f7.2)') 'Diffuse patch ',iptch,
     &        ' angles ',va,vb
            write(icout,*) outs
            write(icout,*) 'and caii is',caii(1:12)
            write(icout,*) 'and caie is',caie(1:12)
          endif
        endif

C The target surface cannot see the sun/or sky patch.
        if(caie(its).lt.0.0)then
          if(mode.eq.'dir')then
            do i=1,nox(izone)
              do j=1,noz(izone)
                if(ioshd(i,j).eq.-1)then
                  continue
                else
                  ioshd(i,j)=1
                endif
              enddo ! of j
            enddo   ! of i
          elseif(mode.eq.'dif')then

C Ipexcl counts the number of sky patches that are behind the target
C surface.
            ipexcl=ipexcl+1
            write(icout,*) 'Cannot see sky patch @ ',iptch
          endif
          goto 40          ! jump to next patch
        else
          continue  ! can be seen by solar vector
        endif

C Redraw the wireframe and the grid.
        if(mmod.eq.8)then
          MODIFYVIEW=.true.
          modlen=.true.
          itsnm=1
          CALL INLNST(1)
          nzg=1; nznog(1)=izone; izgfoc=izone
          CALL redraw(IER)
        endif

C Draw an arrow from the centre grid point along the patch ray.
C Use different variables for drawing arrow. 
        call  AZ2UV(va,vb,avdx,avdy,avdz)
        auvdx=1.0*avdx; auvdy=1.0*avdy; auvdz=1.0*avdz
        P(1)=x3op(10,10); P(2)=y3op(10,10); P(3)=z3op(10,10)
        Q(1)=x3op(10,10)+auvdx; Q(2)=y3op(10,10)+auvdy 
        Q(3)=z3op(10,10)+auvdz
        shaft=2.0; al=0.3; aw=0.15
        call arrow(P,Q,al,aw,ipoints,'a',2)
        call pausems(20)

C Loop through the obstructions that are in front of or cross
C the current surface.
        if(nbobs(izone).gt.0)then
          do ib=1,nbobs(izone)

C Clear ihit for this obstruction block.
            do 501 i=1,nox(izone)
              do 502 j=1,noz(izone)
                ihit(i,j)=0

C Clear xzy of shaded point on the target surface.
                x3intrs(i,j)=0.0; y3intrs(i,j)=0.0
                z3intrs(i,j)=0.0
  502         continue
  501       continue
            do iface=1,6
              if(caieob(ib,iface).lt.0.0)then
                proceed=.true.
              else
                proceed=.false.
              endif
              if(proceed)then

C Recover equation of plane of obstruction face.
                EQN(1)=SHEQN(ib,iface,1)
                EQN(2)=SHEQN(ib,iface,2)
                EQN(3)=SHEQN(ib,iface,3)
                EQN(4)=SHEQN(ib,iface,4)

                do 300 m=1,nox(izone)
                  do 310 n=1,noz(izone)

C Update the normal vector from the current grid point. Then
C call i3pippa to test containment within focus surface.
                    XXuv=x3op(m,n)+uvdx; YYuv=y3op(m,n)+uvdy
                    ZZuv=z3op(m,n)+uvdz
                    XXW=x3op(m,n); YYW=y3op(m,n); ZZW=z3op(m,n)
                    call i3pippa(izone,its,xxw,yyw,zzw,m,n,i3pip)
                    if(i3pip.eq.0)then
                      goto 310  ! grid point not within focus surface
                    else
C                      if(mmod.eq.8) call wiresymbol(xxw,yyw,zzw,1,32)
                    endif

C Project from grid point to the obstruction polygon face and find
C point of intersection x3 y3 z3
                    CALL VECPLN(x3op(m,n),y3op(m,n),z3op(m,n),
     &                XXuv,YYuv,ZZuv,EQN, x3,y3,z3, IERR)

C Check if x3 y3 z3 is within BBO.
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
                    if(inside)then
                      call i3piobs(izone,ib,iface,x3,y3,z3,m,n,i3pio)
                      if(i3pio.ne.0)then
                        if(mmod.eq.8)then

C Remember where the shaded point is.
                          xxw=x3op(m,n); yyw=y3op(m,n); zzw=z3op(m,n)
                          x3intrs(m,n)=xxw; y3intrs(m,n)=yyw
                          z3intrs(m,n)=zzw
                        endif

C Point obscured by current obstruction block face. Point shaded
C if any face obscures so only increment oshd for first hit, ignoring
C other hits.
                        ihit(m,n)=ihit(m,n)+1
                        itothits=itothits+1  !remember for the whole surface
                        ioshd(m,n)=1

C If more than one obstruction block shaded then opacities need to be
C compounded.
                        op=opob(izone,ib)
                        if(ihit(m,n).eq.1)oshd(m,n)=
     &                     1.0-(1.0-oshd(m,n))*(1.0-op)
                        if(oshd(m,n).gt.1.0)oshd(m,n)=1.0
                      endif
                    endif
  310             continue ! of j
  300           continue   ! of i
              endif  ! of proceed
            enddo    ! of iface
          enddo      ! of ib
        endif        ! have blocks

C Have processed all the obstructions. Calculate foshd similar to
C as done in shdcon.F near line 594
C << Comment out so that foshd not double counted when self-shading
C << is added below
C        if(mode.eq.'dif')then
C          if(itothits.eq.0)then
C            continue  ! no hits for patch do not update foshd
C          else
C            do 70 i=1,nox(izone)
C              do 80 j=1,noz(izone)
C                if(ioshd(i,j).eq.-1)goto 80
C                foshd(i,j)=foshd(i,j)+oshd(i,j)/145.0
C   80         continue
C   70       continue
C          endif
C        endif

C Now draw the shaded points.
C << need to set irpt in the interface >>
        irpt=1
        call drawshadedgrid(izone,its,irpt,1)

C Dump intermediate calculations.
        if(tok.or.ok)then
          if(itothits.eq.0)then
            write(icout,*) 'no hits in patch loop ',iptch,va,vb
          else
            write(icout,*) 'patch loop ',iptch,va,vb
            if(tok)write(icout,*) 'obstructions dumping diffuse ihit'
            if(tok)call dumpnoxnozi(izone,ihr,ihit)
            write(icout,*) 'obstructions dumping diffuse ioshd '
            call dumpnoxnozi(izone,ihr,ioshd)
            write(icout,*) 'obstructions dumping diffuse oshd '
            call dumpnoxnozr(izone,ihr,oshd)
            write(icout,*) 'obstructions dumping diffuse foshd '
            call dumpnoxnozr(izone,ihr,foshd)
          endif
        endif

C Look at surfaces for diffuse self-shading. This adds to the
C number of hits from the obstruction pass.
C << temporary comment out so can see details.
C        itothits=0  ! reset counter for nb of grid hits for surface 
        do 112 loop=1,nzsur(izone)
          icc=izstocn(izone,loop)
          if(loop.eq.its)then
            goto 112   !  skip self
          elseif(caie(loop).lt.0.0)then
            goto 112  ! cannot be seen by solar vector
          elseif(fbc(its,icc).eq.'B')then
            goto 112  ! other surface is behind
          elseif(sother(izone,loop,1)(1:7).eq.'SIMILAR')then
            goto 112  ! do not face outside
          endif
          if(sother(izone,loop,1)(1:8).eq.'EXTERIOR')then
             
            EQN(1)=sureqn(izone,loop,1)
            EQN(2)=sureqn(izone,loop,2)
            EQN(3)=sureqn(izone,loop,3)
            EQN(4)=sureqn(izone,loop,4)

            do 201 m=1,nox(izone)
              do 211 n=1,noz(izone)

C Add unit vector to the sun path to the current mesh cell.
                XXuv=x3op(m,n)+uvdx; YYuv=y3op(m,n)+uvdy
                ZZuv=z3op(m,n)+uvdz
                xxw=x3op(m,n); yyw=y3op(m,n); zzw=z3op(m,n)
                call i3pippa(izone,its,xxw,yyw,zzw,m,n,i3pip)
                if(i3pip.eq.0)then
                  goto 211  ! grid point not within focus surface
                else
C                  if(mmod.eq.8) call wiresymbol(xxw,yyw,zzw,1,32)
                endif

                CALL VECPLN(x3op(m,n),y3op(m,n),z3op(m,n),
     &            XXuv,YYuv,ZZuv,EQN, x3,y3,z3, IERR)

C Is this intersection within the bounding box of the receiving surface?
                call insurbox(x3,y3,z3,izone,loop,inside)
                if(inside)then
                  call i3pippa(izone,loop,x3,y3,z3,m,n,i3pip)
C                  write(6,*) 'i3pippa for ',izone,loop,x3,y3,z3,i3pip
                  if(i3pip.ne.0)then
                    ihit(m,n)=ihit(m,n)+1
                    itothits=itothits+1  !remember for the whole surface
                    ioshd(m,n)=1
                    oshd(m,n)=1.0

                    if(mmod.eq.8)then

C Remember where the shaded point is.
                      x3intrs(m,n)=xxw; y3intrs(m,n)=yyw
                      z3intrs(m,n)=zzw
                        
C Draw a bigger blob on the focus grid point to show it is shaded.
C                      call wiresymbol(xxw,yyw,zzw,2,33)
C                      call pausems(10)
                    endif
                  endif
                endif
  211         continue
  201       continue
          endif
  112   continue

C Draw the self-shaded points. irpt is 1 for test
        irpt=1
        call drawshadedgrid(izone,its,irpt,2)

C Have processed all the surfaces. Calculate foshd similar to
C as done in shdcon.F near line 594
        if(mode.eq.'dif')then
          if(itothits.eq.0)then
            continue  ! no hits for patch do not update foshd
          else
            do 71 i=1,nox(izone)
              do 81 j=1,noz(izone)
                if(ioshd(i,j).eq.-1)goto 81
                foshd(i,j)=foshd(i,j)+oshd(i,j)/145.0
   81         continue
   71       continue
          endif
        endif

C Dump intermediate calculations.
        if(tok.or.ok)then
          if(itothits.eq.0)then
            write(icout,*) 'no surf hits in patch loop ',iptch,va,vb
          else
            write(icout,*) 'patch loop surf',iptch,va,vb
            write(icout,*) 'surface dumping diffuse ihit '
            call dumpnoxnozi(izone,ihr,ihit)
            write(icout,*) 'surface dumping diffuse ioshd '
            call dumpnoxnozi(izone,ihr,ioshd)
            write(icout,*) 'surface dumping diffuse oshd '
            call dumpnoxnozr(izone,ihr,oshd)
            write(icout,*) 'surface dumping diffuse foshd '
            call dumpnoxnozr(izone,ihr,foshd)
          endif
        endif

C Calculate overall self-shading on target surface, gselfsa (is
C initialised in shadc before direct and diffuse processing).
        if(iptch.eq.145.or.ip.eq.1.or.ip.eq.146)then
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
     &      'Surface obstr & self-shading area = ',
     &      gssa,' Surface area = ',gsa,' Grid area =',ogrida(its),
     &      ' excluded patches ',ipexcl
          if(tok.or.ok)then
            call edisp(icout,outs)
            call edisp(iuout,outs)
          else
            call edisp(iuout,outs)
          endif
          if(mode.eq.'dir')then
            if(tok.or.ok)then
              write(icout,'(a,f7.3)') 'direct shading is ',gssa/gsa
              write(outs,'(a,f7.3)') 'direct shading is ',gssa/gsa
              call edisp(iuout,outs)
            else
              write(outs,'(a,f7.3)') 'direct shading is ',gssa/gsa
              call edisp(iuout,outs)
            endif

C Adjust diffuse shading to relate only to the sky portion that is
C visible to the target surface. This is required because the calculation
C of the diffuse irradiance in bps does not include the obscured
C sky portion.
          elseif(mode.eq.'dif')then
            adj=145.0/(145.0-float(ipexcl))
            if(tok.or.ok)then
              write(icout,*) 'diffuse obstr & self-shaing is',
     &          gssa*adj/gsa
              write(outs,*) 'diffuse obstr & self-shaing is',
     &          gssa*adj/gsa
              call edisp(iuout,outs)
            else
              write(outs,*) 'diffuse obstr & self-shaing is',
     &          gssa*adj/gsa
              call edisp(iuout,outs)
            endif
          endif
        endif
  
 40   continue

C Indicate that sky vault has been processes once in case idifc = 0
C (i.e. time invariant geometry and sky irradiance distribution).
      init=1
      goto 42
      
      return
      end

C ******************** PRECOBS ********************
C precobs pre-computes obstruction polygons for the current zone
C obstructio blocks and places in common block SHDGB1.

      subroutine precobs(izone)
#include "building.h"
#include "geometry.h"

C Parameters
      integer izone   ! focus zone

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)

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


      DIMENSION XX(mv),YY(mv),ZZ(mv)
      DIMENSION VP(3),EQN(4)
      integer j,ip1
      logical close  ! for azi testing
      integer loop
      real Xmin,Xmax,Ymin,Ymax,Zmin,Zmax ! 3D bounds
      real XSUM(6),YSUM(6),ZSUM(6)  ! for azi & elev calcs
      real azi,elv

      PI = 4.0 * ATAN(1.0)
      R=PI/180.

C Transform an obstruction into polygons in common block GB1.
      if(nbobs(izone).gt.0)then
        call edisp(iuout,'Instandiating SHDGB1')
        do ib=1,nbobs(izone)
          if(BLOCKTYP(izone,ib)(1:4).eq.'obs ')then
            CALL CNVBLK(XOB(izone,IB),YOB(izone,IB),ZOB(izone,IB),
     &        DXOB(izone,IB),DYOB(izone,IB),DZOB(izone,IB),
     &        BANGOB(izone,IB,1))
          elseif(BLOCKTYP(izone,IB)(1:4).eq.'obs3')then
            CALL CNVBLK3A(XOB(izone,IB),YOB(izone,IB),ZOB(izone,IB),
     &        DXOB(izone,IB),DYOB(izone,IB),DZOB(izone,IB),
     &        BANGOB(izone,IB,1),BANGOB(izone,IB,2),BANGOB(izone,IB,3))
          endif

C Obstruction faces: front v1 v2 v6 v5
C right v2 v3 v7 v6, back v3 v4 v8 v7
C left v4 v1 v5 v8, top v5 v6 v7 v8, base  v1 2 3 4.

C Set initial bounding box where BBO(ib,1,*) is lower left and
C BBO(ib,2,*) is upper right.
          Xmin=100.0; Xmax=-100.0; Ymin=100.0; Ymax=-100.0
          Zmin=100.0; Zmax=-100.0

C Instantiate SHXB SHYB SHZB.
          do loop=1,12
            SHXB(ib,loop)=XB(loop)
            SHYB(ib,loop)=YB(loop)
            SHZB(ib,loop)=ZB(loop)
          enddo  ! of loop
          
C Get bounds for the whole obstruction as well as azimuth
C and elevation for each
          NV = 4       ! assume 4 corners for an obs polygon
          DO loop=1,6  ! and 6 faces to the obstruction
            XS=0.0; YS=0.0; ZS=0.0  ! for orientation calcs
            DO J = 1,NV
              JVNBSH(ib,loop,J)=JVNB(loop,J)
              XX(J) = XB(JVNB(loop,J))  ! for equation
              YY(J) = YB(JVNB(loop,J))
              ZZ(J) = ZB(JVNB(loop,J))
              
C Use logic similar to egeometry.F ~line 4162
              K=J+1
              IF(J.EQ.NV)K=1
              IP1=JVNB(loop,J)
              IP2=JVNB(loop,K)
              if(IP1.gt.0.and.IP2.gt.0)then
                XS=XS+YB(IP1)*ZB(IP2)-ZB(IP1)*YB(IP2)
                YS=YS+ZB(IP1)*XB(IP2)-XB(IP1)*ZB(IP2)
                ZS=ZS+XB(IP1)*YB(IP2)-YB(IP1)*XB(IP2)
              endif
              Xmin=AMIN1(Xmin,XB(JVNB(loop,J)))
              Xmax=AMAX1(Xmax,XB(JVNB(loop,J)))
              Ymin=AMIN1(Ymin,YB(JVNB(loop,J)))
              Ymax=AMAX1(Ymax,YB(JVNB(loop,J)))
              Zmin=AMIN1(Zmin,ZB(JVNB(loop,J)))
              Zmax=AMAX1(Zmax,ZB(JVNB(loop,J)))
            ENDDO  ! of J
            XSUM(loop)=XS
            YSUM(loop)=YS
            ZSUM(loop)=ZS
            call PLEQN(XX,YY,ZZ,NV,VP,EQN,IERR)
            SHEQN(ib,loop,1)=EQN(1)
            SHEQN(ib,loop,2)=EQN(2)
            SHEQN(ib,loop,3)=EQN(3)
            SHEQN(ib,loop,4)=EQN(4)
          ENDDO    ! of loop

C Calculate azimuth and elevation of the obstruction 
C polygon using method similar to egeometry.F ~4217
          DO k=1,6
            shpazi(ib,k)=90.0
            IF(XSUM(K).LT.0.0)shpazi(ib,k)=-90.0
            CALL ECLOSE(XSUM(K),0.0,0.0001,CLOSE)
            IF(CLOSE)shpazi(ib,k)=0.0
            shpelv(ib,k)=90.0
            IF(ZSUM(K).LT.0.0)shpelv(ib,k)=-90.0
            CALL ECLOSE(ZSUM(K),0.0,0.0001,CLOSE)
            IF(CLOSE)shpelv(ib,k)=0.0
            CALL ECLOSE(YSUM(K),0.0,0.0001,CLOSE)
            IF(.NOT.CLOSE)then
              AZI=ATAN2(XSUM(K),YSUM(K))
              shpazi(ib,k)=AZI/R
            ENDIF
            SRX2Y2=SQRT(XSUM(K)*XSUM(K)+YSUM(K)*YSUM(K))
            IF(shpazi(ib,k).LT.0.)shpazi(ib,k)=shpazi(ib,k)+360.
            CALL ECLOSE(SRX2Y2,0.0,0.0001,CLOSE)
            IF(.NOT.CLOSE)then
              ELV=ATAN2(ZSUM(K),SRX2Y2)
              shpelv(ib,k)=ELV/R
            ENDIF
C            write(6,*) 'ib k azi elev',ib,k,shpazi(ib,k),shpelv(ib,k)
          ENDDO  ! of k

C Assign BBO at the lower left and upper right extents.
          BBO(ib,1,1)=Xmin; BBO(ib,1,2)=Ymin; BBO(ib,1,3)=Zmin
          BBO(ib,2,1)=Xmax; BBO(ib,2,2)=Ymax; BBO(ib,2,3)=Zmax
          write(6,*) 'ib bbo ll',BBO(ib,1,1),BBO(ib,1,2),
     &      BBO(ib,1,3)
        
        enddo  ! of ib
      endif
      return
      end

C ******************** INCAIOBS ********************
C Calculate the eqivalent of caii and caie for obstructions.
C If ihour>0 compute sazi1 & salt1 otherwise pass in sazi & salt.

      subroutine incaiobs(izone,iyd,ihour,sazi,salt)
#include "building.h"
#include "geometry.h"

C Parameters.
      integer izone  ! the focus zone
      integer iyd    ! day of the year
      integer ihour  ! the focus hour
      real sazi,salt ! passed in if ihour = 0 (for diffuse solar)

      common/traceout/icout
      common/tc/itc,icnt
      real caiiob,caieob  ! similar to caii caie
      common/caiob/caiiob(MB,6),caieob(MB,6)

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

      character outs*124
      logical ok,tok

C If trace is on set tok.
      tok=.false.
      if(icout.eq.33)tok=.true.

      pi = 4.0 * atan(1.0)
      r=pi/180.
      if(ihour.eq.0)then
        sazi1=sazi; salt1=salt
        stime=float(ihour)
      else
        stime=float(ihour)
        call eazalts(stime,iyd,slat,slon,isunup,sazi1,salt1)
      endif

C  Cosine of the angle of incidence and surface-solar azimuth angle.
      if(nbobs(izone).gt.0)then
        do ib=1,nbobs(izone)
          do iface=1,6  ! and 6 faces to the obstruction
            if(salt1.gt.0.0)then
              psazi=abs(shpazi(ib,iface)-sazi1)
              x1=cos(salt1*r)
              x2=sin((90.-shpelv(ib,iface))*r)
              x3=cos(psazi*r)
              x4=sin(salt1*r)
              x5=cos((90.-shpelv(ib,iface))*r)
              caieob(ib,iface)=x1*x2*x3+x4*x5
              paz=shpazi(ib,iface)+180.0
              if(shpazi(ib,iface).gt.180.0)paz=shpazi(ib,iface)-180.0
              psazi=abs(paz-sazi1)
              x3=cos(psazi*r)
              x1=cos(salt1*r)
              x2=sin((90.+shpelv(ib,iface))*r)
              x3=cos(psazi*r)
              x4=sin(salt1*r)
              x5=cos((90.+shpelv(ib,iface))*r)
              caiiob(ib,iface)=x1*x2*x3+x4*x5
            else
              caiiob(ib,iface)=0.0
              caieob(ib,iface)=0.0
            endif
          enddo ! of iface
        enddo   ! of ib
      endif

C Trace output, check also that user has asked for detailed trace.
      ok=.false.
      if(itc.eq.2)ok=.true.
      if(tok.and.ok)then
        call edisp(icout,' ')
        write(outs,'(a,i3,a,i3)')'**INCAIOBS: Hour',ihour,' zone',izone
        call edisp(icout,outs)
        call edisp(icout,
     &  ' Obs face   Internal   External   Azimuth   Elevation')

        do ib=1,nbobs(izone)
          do iface=1,6  ! and 6 faces to the obstruction
            write(outs,'(2i3,f6.4,6x,f6.4,7x,f5.1,6x,f5.1)')
     &        ib,iface,caiiob(ib,iface),caieob(ib,iface),
     &        shpazi(ib,iface),shpelv(ib,iface)
            call edisp(icout,outs)
          enddo ! of iface
        enddo   ! of ib
      endif
      return
      end

C ******************** SUR3DLEHI ********************
C Determines the overall length and height of a surface
C (bounding box) and passes the vaules back as WID and HIGH.
C Uses a temporary transform into 2D to get these data.

      SUBROUTINE SUR3DLEHI(izone,IS,WID,HIGH,BB)
#include "building.h"
#include "geometry.h"

C Parameters.
      integer izone  ! the focus zone
      integer is     ! the focus surface
      real wid,high  ! pass back width and height of bounding box
      real BB(2,3)   ! bounding box in site coordinates

      integer izstocn
      common/c24/izstocn(mcom,ms)

      DIMENSION  XX(MV),YY(MV),ZZ(MV)
      DIMENSION  VP(3),EP(3),EQN(4)
      DIMENSION  TMAT(4,4),RMAT(4,4)

      real XYMAX,ZMAX
      real XMIN,YMIN,XMAX,YMAX     ! 2D bounds
      real XMN,XMX,YMN,YMX,ZMN,ZMX ! 3D bounds
      integer n,j,ip1

C Set initial bounding box where BB(1,*) is lower left and
C BB(2,*) is upper right.
      XMN=100.0; XMX=-100.0; YMN=100.0; YMX=-100.0
      ZMN=100.0; ZMX=-100.0

C Get the current connection and number of surfaces in zone.
      icc=izstocn(izone,is)

C Transform surface into into site coordinates in the
C surface of the plane. Make up XX,YY,ZZ to pass across to the
C transform routine.
      N = isznver(izone,is)
      DO J = 1,N
        ip1=iszjvn(izone,is,J)
        XX(J) = szcoords(izone,ip1,1)
        XMN=AMIN1(XMN,XX(J))
        XMX=AMAX1(XMX,XX(J))
        YY(J) = szcoords(izone,ip1,2)
        YMN=AMIN1(YMN,YY(J))
        YMX=AMAX1(YMX,YY(J))
        ZZ(J) = szcoords(izone,ip1,3)
        ZMN=AMIN1(ZMN,ZZ(J))
        ZMX=AMAX1(ZMX,ZZ(J))
      ENDDO  ! of J

C Assign BB at the lower left and upper right extents.
      BB(1,1)=XMN; BB(1,2)=YMN; BB(1,3)=ZMN
      BB(2,1)=XMX; BB(2,2)=YMX; BB(2,3)=ZMX

C Find transformation matrices that normalise face.
      call PLEQN(XX,YY,ZZ,N,VP,EQN,IERR)
      IF (IERR .LT. 0) return
      DO J = 1,3
        EP(J) = VP(J) + EQN(J)
      ENDDO  ! of J
      CALL  EYEMAT(EP,VP,1.0,TMAT,RMAT)

C Transform all points in surface and find lower left corner
C and the upper right corner.
      XMIN=100.0; YMIN=100.0; ZMIN=100.0
      XMAX=0.0; YMAX=0.0; ZMAX=0.0; WID=0.0; HIGH=0.0
      DO I=1,N
        CALL ORTTRN(XX(I),YY(I),ZZ(I),TMAT,X1,Y1,ZZZ,IERR)
        IF(X1.LT.XMIN)XMIN=X1
        IF(Y1.LT.YMIN)YMIN=Y1
        IF(ZZZ.LT.ZMIN)ZMIN=ZZZ
        IF(X1.GT.XMAX)XMAX=X1
        IF(Y1.GT.YMAX)YMAX=Y1
        IF(ZZZ.GT.ZMAX)ZMAX=ZZZ
      ENDDO  ! of I

C Determine the height difference between the vertices. If the height
C difference is greater than the previous values then define as new
C maximum.
      ZVAL = ABS(YMAX - YMIN)
      If (ZVAL.gt.HIGH) HIGH = ZVAL

C Determine the distance between the vertices on the x plane. If the
C distance is greater than the previous values then define as new
C maximum.
      XYVAL= ABS(XMAX - XMIN)
      if (XYVAL.gt.WID) WID = XYVAL

C Debug.
C      write(6,'(a,8F7.3)') 'x mm and y mm and z mm',XMIN,XMAX,YMIN,
C     &  YMAX,ZMIN,ZMAX,ZVAL,XYVAL
C      write(6,*) 'iz is wid high',izone,IS,WID,HIGH

      return
      END


C ******************** ETRANSGRID ********************
C Used when creating a grid point in 3D equivalent to that
C created by 2D calculations.

C STEP 1 - Find equation of surface (EQN(4)) via PLEQN and get centre
C          Centre of Gravity (VP(3))
C STEP 2 - Set up Eye Point normal to plane at C. of G. (EP(3))
C STEP 3 - Find matrix and reverse matrix via EYEMAT to transform grid
C          point to normal view.
C STEP 4 - Find co-ordinates of 'origin' as transformed via ORTTRN to
C          to normal view ('origin' - first vertex of surface)
C STEP 5 - Apply reverse transformation to co-ordinates via
C          ORTTRN to give vertices in global co-ords.
C N is the number of surface vertices, X,Y,Z are the surface vertex arrays,
C DX,DZ are the mesh offsets, XXW,YYW,ZZW are the transformed wcoords.

      SUBROUTINE ETRANSGRID(N,XA,YA,ZA,DX,DZ,XXW,YYW,ZZW)
#include "building.h"

C Parameters.
      integer n     ! number of vertices to process
      real XA,YA,ZA ! array of surface coordinates
      DIMENSION  XA(MV),YA(MV),ZA(MV)
      real DX,DZ    ! grid X Z offsets
      real XXW,YYW,ZZW

      common/traceout/icout
      common/tc/itc,icnt

      DIMENSION  TMAT(4,4),RMAT(4,4)
      DIMENSION  VP(3),EP(3),EQN(4)
      CHARACTER OUTSTR*124
      real offset  ! offset away from the plane
      integer j,i
      logical tok

C If trace is on set tok.
      tok=.false.
      if(icout.eq.33.and.itc.eq.2)tok=.true.

      offset=0.0

C Find transformation matrices that normalise face.
      call PLEQN(XA,YA,ZA,N,VP,EQN,IERR)

      IF (IERR .LT. 0)  return
      DO J = 1,3
        EP(J) = VP(J) + EQN(J)
      ENDDO  ! of J
      IF(tok)THEN
        CALL EDISP(icout,' ETRANSGRID Plane equation data: ')
        WRITE(OUTSTR,'(a,3F8.3)')' Center of grav:',(VP(I),I=1,3)
        CALL EDISP(icout,OUTSTR)
        WRITE(OUTSTR,'(a,4F8.3)')' Equation:',(EQN(I),I=1,4)
        CALL EDISP(icout,OUTSTR)
        WRITE(OUTSTR,'(a,3F8.3)')' Eye Point:',(EP(I),I=1,3)
        CALL EDISP(icout,OUTSTR)
      ENDIF
      CALL  EYEMAT(EP,VP,1.0,TMAT,RMAT)

C Transform all points in surface and find lower left corner.
      XMIN=0.0
      YMIN=0.0
      DO I=1,N
        CALL ORTTRN(XA(I),YA(I),ZA(I),TMAT,X1,Y1,ZZZ,IERR)
        IF(X1.LT.XMIN)XMIN=X1
        IF(Y1.LT.YMIN)YMIN=Y1
      ENDDO  ! of I
      XXW=XMIN+DX
      YYW=YMIN+DZ

C Apply transformation first shifting the Z point by OFFSET.
      ZZZ=ZZZ-OFFSET
      CALL  ORTTRN(XXW,YYW,ZZZ,RMAT,XX,YY,ZZ,IERR)
      XXW = XX
      YYW = YY
      ZZW = ZZ
      IF(tok)THEN
        CALL EDISP(icout,' Transformed  coords: ')
        WRITE(OUTSTR,'(a,3F8.3)') ' X Y Z:',XXW,YYW,ZZW
      ENDIF

      RETURN
      END

C ******************** PICKSSUR ********************
C Selects surfaces for shading & insolation analysis.
C 'izone' is the target zone, 'np' the number of surfaces selected,
C 'act' = 's or S' for shading, 'act' = 'i or I' for insolation,
C 'act' = 'l or L' for view shading surfaces and 'act' = '-' for
C no selection.  'iva' is the array of selections.

      subroutine pickssur(izone,np,act,iva,ier)
#include "building.h"
#include "geometry.h"
#include "help.h"
      
      integer ncomp,ncon
      common/c1/ncomp,ncon
      integer izstocn
      common/c24/izstocn(mcom,ms)
      integer mon,isc,iyd
      common/contr/mon,isc(ms),iyd

C iaplic(1) toggle for shading; iaplic(2) toggle for insolation:
C                      1 is 'all_applicable' and
C                      0 is manual selection of surfaces.
C nsurfcalc nb of shaded surfaces, lstsfcalc() list of applicable surfaces.
C nsurfinso nb of insolation sources, isurfinso() list of insolation sources.
      common/ishdirec/iaplic(mcom,2),nsurfcalc(mcom),lstsfcalc(mcom,ms),
     &       nsurfinso(mcom),isurfinso(mcom,ms)

      dimension stmp(ms),iva(ms),ivals(ms)
      character stmp*33,act*1

      integer ichoice ! temporary local variable for a call to easkab
      integer iexcludediff_flag
      common/excludediff/iexcludediff_flag
      integer i
      logical newgeo  ! Used to test for new/old geometry file.

      helpinsub='miscel'  ! set for subroutine

      ier=0
      if(izone.gt.ncomp.or.izone.eq.0)then
         call usrmsg(' ',' Zone number out of range! ','W')
         ier=1
         return
      endif
      newgeo=.false.  ! Assume older format geometry.
      call eclose(gversion(izone),1.1,0.01,newgeo)
      
      helptopic='diffuse_shading_request'
      call gethelptext(helpinsub,helptopic,nbhelp)
      iexcludediff_flag=0     
      call easkab(' ','Diffuse shading options:',
     &  'Exclude diffuse shading',
     &  'Include diffuse shading (default)',ichoice,nbhelp)
      if(ichoice.eq.1)then

C Exclude diffuse shading.
         iexcludediff_flag=1
      else
         iexcludediff_flag=0      
      endif

C Generate help text for the sequence of dialogs.
 43   helptopic='surface_selection_notes'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(act.eq.'s'.or.act.eq.'S')then
         call easkabc(' ','Shading options:',
     &     'all applicable surfaces','select surface(s)',
     &     'cancel',iic,nbhelp)
         if(iic.eq.1)iaplic(izone,1)=1
         if(iic.eq.2)iaplic(izone,1)=0
      elseif(act.eq.'i'.or.act.eq.'I')then
         if(newgeo)then
           if(iaplic(izone,2).eq.1)then
             iic=1   ! user nominated in the geometry file
           elseif(iaplic(izone,2).eq.0)then
             iic=2   ! user nominated in the geometry file
           endif
         else
           call easkabc(' ','Insolation options:',
     &       'all applicable sources','select source(s)',
     &       'cancel',iic,nbhelp)
           if(iic.eq.1)iaplic(izone,2)=1
           if(iic.eq.2)iaplic(izone,2)=0
         endif
      elseif(act.eq.'l'.or.act.eq.'L')then
         iic=4
      else
         iic=2
      endif

C Clear return array.
      do i=1,nzsur(izone)
         iva(i)=0
      enddo  ! of i

C Process initial selection.
      if(iic.eq.3)then
         return
      elseif(iic.eq.1)then
         if(act.eq.'s'.or.act.eq.'S')then
            np=0
            do i=1,nzsur(izone)
               icn=izstocn(izone,i)
               if(sother(izone,i,1)(1:5).eq.'EXTER')then
                  np=np+1
                 iva(np)=i
               endif
            enddo  ! if i
            if(np.eq.0)then
               call usrmsg('No applicable surfaces found!',' ','W')
               goto 43
            endif
            return
         elseif(act.eq.'i'.or.act.eq.'I')then
            np=0
            do i=1,nzsur(izone)
               icn=izstocn(izone,i)
               if(sother(izone,i,1)(1:5).eq.'EXTER'.and.
     &            sotf(izone,i)(1:4).ne.'OPAQ')then
                  np=np+1
                  iva(np)=i
               endif
            enddo  ! of i
            if(np.eq.0)then
               call usrmsg('No applicable surfaces found!',' ','W')
               goto 43
            endif
            return
         endif
      elseif(iic.eq.2)then
         inpic=nzsur(izone)
         do i=1,inpic
            icn=izstocn(izone,i)
            if(act.eq.'s'.or.act.eq.'S')then
               if(sother(izone,i,1)(1:5).eq.'EXTER')then
                  write(stmp(I),'(5a)')sname(izone,i),'|',
     &              sotf(izone,i)(1:4),'|',sother(izone,i,1)(1:15)
               else
                  write(stmp(I),'(2a)')sname(izone,i),' not applicable'
               endif
            elseif(act.eq.'-')then
               write(stmp(i),'(5a)')sname(izone,i),'|',
     &           sotf(izone,i)(1:4),'|',sother(izone,i,1)(1:15)
            elseif(act.eq.'i'.or.act.eq.'I')then
               if(sother(izone,i,1)(1:5).eq.'EXTER'.and.
     &            sotf(izone,i)(1:4).ne.'OPAQ')then
                  write(stmp(i),'(5a)')sname(izone,i),'|',
     &              sotf(izone,i)(1:4),'|',sother(izone,i,1)(1:15)
               else
                  write(stmp(i),'(2a)')sname(izone,i),' not applicable'
               endif
            endif
            iva(i)=0
         enddo  ! of i

         call epicks(inpic,ivals,' ','Surface(s) to include',
     &     33,nzsur(izone),stmp,'Surface(s) to include?',ier,nbhelp)
         if(inpic.eq.0)then
            return
         else
            np=inpic
            do i=1,np
               iva(i)=ivals(i)
            enddo  ! of i
         endif

      elseif(iic.eq.4)then
         inpic=nzsur(izone)
         do i=1,inpic
            icn=izstocn(izone,i)
            if(isc(i).eq.1)then
               write(stmp(i),'(5a)')sname(izone,i),'|',
     &           sotf(izone,i)(1:4),'|',sother(izone,i,1)(1:15)
            else
               write(stmp(i),'(2a)')sname(izone,i),' not assessed'
            endif
            iva(i)=0
         enddo  ! of i
         inpic=1
         call epicks(inpic,ivals,' ','Surface to View',
     &     33,nzsur(izone),stmp,'Surface(s) to view?',ier,nbhelp)
         if(inpic.eq.0)then
            return
         else
            np=inpic
            iva(1)=ivals(1)
         endif
      endif
      return
      end

C ******************** ASKDAY ********************
C Returns 'imo' (month), 'ido' (day of month) and 'ijday' (day of year).
C 'ifday' is a toggle controlling the display and input of periods:
C 0 = Julian day, 1 or 2 = day of month.

      subroutine askday(ifday,imo,ido,ijday,ier)
#include "help.h"

      common/outin/iuout,iuin,ieout
      dimension id(12)

      data id/31,28,31,30,31,30,31,31,30,31,30,31/

      helpinsub='miscel'  ! set for subroutine
      ier=0
      helptopic='specify_day_of_year'
      call gethelptext(helpinsub,helptopic,nbhelp)
    1 if(ifday.eq.0)then
         call easki(ijday,' ','Year day?',
     &     1,'F',365,'F',1,'start day',ier,nbhelp)
         call edayr(ijday,ido,imo)
         if(ier.ne.0)goto 1
      else
         call easki(imo,' ', 'Month number?',
     &     1,'F',12,'F',1,'end day',ier,nbhelp)
         call easki(ido,' ', 'Day of the month?',
     &     1,'F',31,'F',1,'start day',ier,nbhelp)
         call eday(ido,imo,ijday)
      endif

C Check range.
      if(ido.gt.id(imo))then
         call edisp(iuout,'Day past end of month! Respecify.')
         goto 1
      endif
      call eday(ido,imo,ijday)
      return
      end

C ******************** POINT1 ********************
C Determines if a specified point is within or outwith the
C target surface.

      subroutine point1(icomp,is,xx,zz,igm,ign,ians)
#include "building.h"
#include "geometry.h"

C Input parameters.
      integer icomp   ! the zone
      integer is      ! the target surface
      real xx,zz      ! point
      integer igm,ign ! grid index currently being evaluated
      integer ians    ! what has been determined

      common/g1t/xft(mv),zft(mv)
      integer izstocn
      common/c24/izstocn(mcom,ms)

      real c
      dimension c(2*mv+2)
      integer icc,ii,last1,last2,ln,ip,n,i

      ians=0

C Get the current connection and number of surfaces in zone.
      icc=izstocn(icomp,is)

C Determine if point xx,zz is within surface 'is'.
      np=isznver(icomp,is)
      do i=1,np
         ii=2*i-1
         c(ii)=xft(i)
         ii=ii+1
         c(ii)=zft(i)
      enddo  ! of i
      last1=2*np+1
      last2=2*np+2
      c(last1)=xft(1)
      c(last2)=zft(1)
      ln=0
      ip=np+1
      do 20 i=2,ip
         n=2*i
         if((c(n-2)-zz)*(zz-c(n)))20,1,2
    2    if((zz-c(n-2))*(c(n-1)-c(n-3))/(c(n)-c(n-2))+c(n-3)-xx)20,3,4
    1    if(c(n-2)-c(n))5,6,7
    6    if((c(n-3)-xx)*(xx-c(n-1)))20,3,3
    5    if((zz-c(n-2))*(c(n-1)-c(n-3))/(c(n)-c(n-2))+c(n-3)-xx)20,3,8
    7    if((zz-c(n-2))*(c(n-1)-c(n-3))/(c(n)-c(n-2))+c(n-3)-xx)20,3,9
    9    ln=ln-2
    8    ln=ln-1
    4    ln=ln+2
   20 continue

C << document this logic >>
      if((ln/4)*4.ne.ln)goto 3
      goto 11
    3 ians=1
   11 continue

      return
      end

C ******************** POINT2 ********************
C Determines if point xx,zz is within or outwith the shadow polygon
C xs,zs.

      subroutine point2(xx,zz,ians)

C Input parameters.
      real xx,zz     ! point
      integer ians   ! what has been determined

      common/shad2/ishd,npoint,xs(5),zs(5)
      real c
      dimension c(12)
      integer last1,last2,ln,ip,n,i

C Logic of if(statement)j1 j2 j3 is
C if (result of statement is) <0  =0 >0
      ians=0
      np=npoint
      do 10 i=1,np
         ii=2*i-1
         c(ii)=xs(i)
         ii=ii+1
         c(ii)=zs(i)
   10 continue
      last1=2*np+1
      last2=2*np+2
      c(last1)=xs(1)
      c(last2)=zs(1)
      ln=0
      ip=np+1
      do 20 i=2,ip
         n=2*i
         if((c(n-2)-zz)*(zz-c(n)))20,1,2   ! jump if (result of statement is) <0  =0 >0
    2    if((zz-c(n-2))*(c(n-1)-c(n-3))/(c(n)-c(n-2))+c(n-3)-xx)20,3,4
    1    if(c(n-2)-c(n))5,6,7
    6    if((c(n-3)-xx)*(xx-c(n-1)))20,3,3
    5    if((zz-c(n-2))*(c(n-1)-c(n-3))/(c(n)-c(n-2))+c(n-3)-xx)20,3,8
    7    if((zz-c(n-2))*(c(n-1)-c(n-3))/(c(n)-c(n-2))+c(n-3)-xx)20,3,9
    9    ln=ln-2
    8    ln=ln-1
    4    ln=ln+2
   20 continue
      if((ln/4)*4.ne.ln)goto 3
      goto 11
    3 ians=1
   11 return
      end
      
C ******************** ipippa ********************
C Checks if a point is within a 2D polygon using the winding method.

      subroutine ipippa(icomp,is,xp,yp,igm,ign,ipip)
#include "building.h"
#include "geometry.h"

C Input parameters.
      integer icomp   ! the zone
      integer is      ! the target surface
      real xp,yp        ! point
      integer igm,ign ! grid index currently being evaluated
      integer ipip    ! what has been determined

C This routine and iquad below use the winding number concept, but instead
C of calculating the angle for each point on the polygon, it
C looks to see when the polygon crosses from one quadrant to
C another, and which way it goes. It also tests when a polygon segment 
C goes from the (+,+) quadrant to (-,-) or (+,-) to (-,+).
C
C Assumes that xarray(nc+1)==xarray(1) and yarray(nc+1)==yarray(1)
c returns ipippa=0 if point x,y not in the bound polygon.
C
C Code based on C functions by Ken McElvain by Barry Rowlingson in
C http://www.realtimerendering.com/resources/RTNews/html/rtnv3n4.html

      common/g1t/xft(mv),zft(mv)
      integer izstocn
      common/c24/izstocn(mcom,ms)

      real xarray,yarray  ! 2D array
      dimension xarray(MV+1),yarray(MV+1)
      integer nc     ! number of points in polygon

      real xlastp,ylastp
      real xthisp,ythisp
      real a,b
      integer ioldq
      integer i

C Get the current connection and number of surfaces in zone.
      icc=izstocn(icomp,is)

C Determine if point xx,zz is within surface 'is'. And setup
C the local arrays.
      np=isznver(icomp,is)
      nc=np+1
      xarray(nc)=xft(1)
      yarray(nc)=zft(1)
      do loop=1,np
        xarray(loop)=xft(loop)
        yarray(loop)=zft(loop)
      enddo
 
      iwind = 0
      xlastp = xarray(nc)
      ylastp = yarray(nc)
      ioldq = iquad(xlastp,ylastp,xp,yp)
      do i=1,nc 

C For each point in the polygon. 
         xthisp=xarray(i)
         ythisp=yarray(i)
         inewq = iquad(xthisp,ythisp,xp,yp)

C See if this line segment starts/finishes in a diff quadrant
         if(ioldq.ne.inewq) then

C See if it is an adjacent quadrant.
           if(mod(ioldq+1,4).eq.inewq) then
              iwind=iwind + 1
           else if(mod(inewq+1,4).eq.ioldq) then
              iwind = iwind - 1
           else 

C If it jumps two quadrants, see which way round
C the origin it went
              a = (ylastp-ythisp)*(xp-xlastp)
              b = xlastp-xthisp
              a = a + ylastp * b
              b=b*yp
              if (a.gt.b) then
                iwind=iwind+2
              else
                iwind=iwind-2
              end if
            end if
          end if
          xlastp=xthisp
          ylastp=ythisp
          ioldq=inewq
      end do
 
C Quadrant winding is either -4,0,+4 so divide down and take abs.
      ipip = abs(iwind/4)

      end

C iquad determines which quadrant xp,yp is in relative to xo,yo as origin.
      function iquad(xp,yp,xo,yo)
      real xp,yp,xo,yo

      if(xp.lt.xo)then
        if(yp.lt.yo) then
          iquad=2
        else 
          iquad=1
        end if
      else
        if(yp.lt.yo)then
          iquad = 3
        else 
          iquad = 0
        end if
      end if

      return
      end
   
      
C ******************** i3pippa ********************
C Point in polygon containment check (3D).

      subroutine i3pippa(icomp,is,xp,yp,zp,igm,ign,ipip)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"

C Input parameters.
      integer icomp   ! the zone
      integer is      ! the target surface
      real xp,yp,zp   ! point
      integer igm,ign ! grid index currently being evaluated
      integer ipip    ! what has been determined

C This routine and iquad below use the winding number concept, but instead
C of calculating the angle for each point on the polygon, it
C looks to see when the polygon crosses from one quadrant to
C another, and which way it goes. It also tests when a polygon segment 
C goes from the (+,+) quadrant to (-,-) or (+,-) to (-,+).

C Assumes that xarray(nc+1)==xarray(1) and yarray(nc+1)==yarray(1)
c returns ipippa=0 if point x,y not in the bound polygon.
c
C Code based on C functions by Ken McElvain by Barry Rowlingson in
C http://www.realtimerendering.com/resources/RTNews/html/rtnv3n4.html

      integer izstocn
      common/c24/izstocn(mcom,ms)

      real xarray,yarray  ! 2D array
      dimension xarray(MV+1),yarray(MV+1)
      integer nc     ! number of points in polygon

      real xlastp,ylastp
      real xthisp,ythisp
      real a,b
      integer ioldq
      integer i
      character drop*1

C Get the current connection and number of surfaces in zone.
      icc=izstocn(icomp,is)

C Check the surface normal and figure out which is the largest.
      biggest=0.0
      drop='-'
      if(abs(sureqn(icomp,is,1)).gt.biggest) drop='X'
      if(abs(sureqn(icomp,is,2)).gt.biggest) drop='Y'
      if(abs(sureqn(icomp,is,3)).gt.biggest) drop='Z'

C Determine if point xx,zz is within surface 'is'. And setup
C the local arrays.
      np=isznver(icomp,is)
      nc=np+1
      if(drop.eq.'X')then
        xx=yp; yy=zp
        xarray(nc)=szcoords(icomp,iszjvn(icomp,is,1),2)
        yarray(nc)=szcoords(icomp,iszjvn(icomp,is,1),3)
        do loop=1,np
          xarray(loop)=szcoords(icomp,iszjvn(icomp,is,loop),2)
          yarray(loop)=szcoords(icomp,iszjvn(icomp,is,loop),3)
        enddo
      elseif(drop.eq.'Y')then
        xx=xp; yy=zp
        xarray(nc)=szcoords(icomp,iszjvn(icomp,is,1),1)
        yarray(nc)=szcoords(icomp,iszjvn(icomp,is,1),3)
        do loop=1,np
          xarray(loop)=szcoords(icomp,iszjvn(icomp,is,loop),1)
          yarray(loop)=szcoords(icomp,iszjvn(icomp,is,loop),3)
        enddo
      elseif(drop.eq.'Z')then
        xx=xp; yy=yp
        xarray(nc)=szcoords(icomp,iszjvn(icomp,is,1),1)
        yarray(nc)=szcoords(icomp,iszjvn(icomp,is,1),2)
        do loop=1,np
          xarray(loop)=szcoords(icomp,iszjvn(icomp,is,loop),1)
          yarray(loop)=szcoords(icomp,iszjvn(icomp,is,loop),2)
        enddo
      endif
      
      iwind = 0
      xlastp = xarray(nc)
      ylastp = yarray(nc)
      ioldq = iquad(xlastp,ylastp,xx,yy)
      do i=1,nc 

C For each point in the polygon. 
         xthisp=xarray(i)
         ythisp=yarray(i)
         inewq = iquad(xthisp,ythisp,xx,yy)

C See if this line segment starts/finishes in a diff quadrant
         if(ioldq.ne.inewq) then
c if so, see if its an adjacent quadrant
           if(mod(ioldq+1,4).eq.inewq) then
              iwind=iwind + 1
           else if(mod(inewq+1,4).eq.ioldq) then
              iwind = iwind - 1
           else 
c If it jumps two quadrants - see which way round
c the origin it went
              a = (ylastp-ythisp)*(xx-xlastp)
              b = xlastp-xthisp
              a = a + ylastp * b
              b=b*yy
              if (a.gt.b) then
                iwind=iwind+2
              else
                iwind=iwind-2
              end if
            end if
          end if
          xlastp=xthisp
          ylastp=ythisp
          ioldq=inewq
      end do
 
C Quadrant winding is either -4,0,+4 so divide down and take abs.
      ipip = abs(iwind/4)

      end
      
C ******************** i3piobs ********************
C Point in obstruction polygon (3D) containment check
C using the winding method.

      subroutine i3piobs(icomp,ib,iface,xp,yp,zp,igm,ign,ipio)
#include "building.h"

C Input parameters.
      integer icomp   ! the zone
      integer ib      ! the target obstruction
      integer iface   ! face of obstruction
      real xp,yp,zp   ! point
      integer igm,ign ! grid index currently being evaluated
      integer ipio    ! what has been determined

C This routine and iquad below use the winding number concept, but instead
C of calculating the angle for each point on the polygon, it
C looks to see when the polygon crosses from one quadrant to
C another, and which way it goes. It also tests when a polygon segment 
C goes from the (+,+) quadrant to (-,-) or (+,-) to (-,+).

C Assumes that xarray(nc+1)==xarray(1) and yarray(nc+1)==yarray(1)
c returns ipippa=0 if point x,y not in the bound polygon.
c
C Code based on C functions by Ken McElvain by Barry Rowlingson in
C http://www.realtimerendering.com/resources/RTNews/html/rtnv3n4.html

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
      DIMENSION VP(3),EQN(4)

      real xarray,yarray  ! 2D array
      dimension xarray(MV+1),yarray(MV+1)
      integer nc     ! number of points in polygon

      real xlastp,ylastp
      real xthisp,ythisp
      real a,b
      integer ioldq
      integer i
      character drop*1

C Equation of plane of the current obstruction face.
      EQN(1)=SHEQN(ib,iface,1)
      EQN(2)=SHEQN(ib,iface,2)
      EQN(3)=SHEQN(ib,iface,3)
      EQN(4)=SHEQN(ib,iface,4)

C Check the surface normal and figure out which is the largest.
      biggest=0.0
      drop='-'
      if(abs(eqn(1)).gt.biggest) drop='X'
      if(abs(eqn(2)).gt.biggest) drop='Y'
      if(abs(eqn(3)).gt.biggest) drop='Z'

C Determine if point xx,zz is within obstr face 'iface'. And setup
C the local arrays.
      np=4
      nc=np+1
      if(drop.eq.'X')then
        xx=yp; yy=zp
        xarray(nc)=SHYB(ib,JVNBSH(ib,iface,1))
        yarray(nc)=SHZB(ib,JVNBSH(ib,iface,1))
        do loop=1,np
          xarray(loop)=SHYB(ib,JVNBSH(ib,iface,loop))
          yarray(loop)=SHZB(ib,JVNBSH(ib,iface,loop))
        enddo
      elseif(drop.eq.'Y')then
        xx=xp; yy=zp
        xarray(nc)=SHXB(ib,JVNBSH(ib,iface,1))
        yarray(nc)=SHZB(ib,JVNBSH(ib,iface,1))
        do loop=1,np
          xarray(loop)=SHXB(ib,JVNBSH(ib,iface,loop))
          yarray(loop)=SHZB(ib,JVNBSH(ib,iface,loop))
        enddo
      elseif(drop.eq.'Z')then
        xx=xp; yy=yp
        xarray(nc)=SHXB(ib,JVNBSH(ib,iface,1))
        yarray(nc)=SHYB(ib,JVNBSH(ib,iface,1))
        do loop=1,np
          xarray(loop)=SHXB(ib,JVNBSH(ib,iface,loop))
          yarray(loop)=SHYB(ib,JVNBSH(ib,iface,loop))
        enddo
      endif
      
      iwind = 0
      xlastp = xarray(nc)
      ylastp = yarray(nc)
      ioldq = iquad(xlastp,ylastp,xx,yy)
      do i=1,nc 

C For each point in the polygon. 
         xthisp=xarray(i)
         ythisp=yarray(i)
         inewq = iquad(xthisp,ythisp,xx,yy)

C See if this line segment starts/finishes in a diff quadrant
         if(ioldq.ne.inewq) then
c if so, see if its an adjacent quadrant
           if(mod(ioldq+1,4).eq.inewq) then
              iwind=iwind + 1
           else if(mod(inewq+1,4).eq.ioldq) then
              iwind = iwind - 1
           else 
c If it jumps two quadrants - see which way round
c the origin it went
              a = (ylastp-ythisp)*(xx-xlastp)
              b = xlastp-xthisp
              a = a + ylastp * b
              b=b*yy
              if (a.gt.b) then
                iwind=iwind+2
              else
                iwind=iwind-2
              end if
            end if
          end if
          xlastp=xthisp
          ylastp=ythisp
          ioldq=inewq
      end do
 
C Quadrant winding is either -4,0,+4 so divide down and take abs.
      ipio = abs(iwind/4)
C      write(6,*)'found i3piobs ',
C     &  icomp,ib,iface,xp,yp,zp,igm,ign,ipio,iwind

      end
      
C ******************** findsurbox ********************
C For each surface in the model find its length width and bounding box.

      subroutine findsurbox()
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"

      integer ncomp,ncon
      common/c1/ncomp,ncon
      integer izstocn
      common/c24/izstocn(mcom,ms)
      real surboxwid,surboxhigh,surbb
      common/surbx/surboxwid(mcon),surboxhigh(mcon),surbb(mcon,2,3)
      integer loop,loop2
      real BB(2,3)   ! bounding box in site coordinates
      real WID,HIGH

      do loop=1,ncomp
        do loop2=1,nzsur(loop)
          icc=izstocn(loop,loop2)
          call SUR3DLEHI(loop,loop2,WID,HIGH,BB)
          surboxwid(icc)=WID
          surboxhigh(icc)=HIGH
          surbb(icc,1,1)=BB(1,1)
          surbb(icc,1,2)=BB(1,2)
          surbb(icc,1,3)=BB(1,3)
          surbb(icc,2,1)=BB(2,1)
          surbb(icc,2,2)=BB(2,2)
          surbb(icc,2,3)=BB(2,3)
        enddo  ! of loop2
      enddo    ! of loop
      return
      end
      
C ******************** insurbox ********************
C Test if the 3D coordinate X Y Z is within surface bounding box.

      subroutine insurbox(XP,YP,ZP,icomp,its,inside)
#include "building.h"
#include "geometry.h"

C Input parameters.
      real XP,YP,ZP
      integer icomp ! focus zone
      integer its   ! focus surface
      logical inside ! true if inside
      
      integer izstocn
      common/c24/izstocn(mcom,ms)
      real surboxwid,surboxhigh,surbb
      common/surbx/surboxwid(mcon),surboxhigh(mcon),surbb(mcon,2,3)
      real tol  ! slight offset
      
C For surfaces along a cardinal axis will have one dimension of
C effectively zero thickness so to avoid risk of equality check
C introduce a 1mm offset to slightly grow the bounding box.
      tol=0.001 ! for surfaces along a cardinal axis will have one
      
      inside=.true.  ! assume inside
      icc=izstocn(icomp,its)
      if(XP.lt.(surbb(icc,1,1)-tol))then
       inside=.false.
      elseif(YP.lt.(surbb(icc,1,2)-tol))then
       inside=.false.
      elseif(ZP.lt.(surbb(icc,1,3)-tol))then
       inside=.false.
      elseif(XP.gt.(surbb(icc,2,1)+tol))then
       inside=.false.
      elseif(YP.gt.(surbb(icc,2,2)+tol))then
       inside=.false.
      elseif(ZP.gt.(surbb(icc,2,3)+tol))then
       inside=.false.
      endif

      return
      end
      
C ******************** frontsurbox ********************
C For each model surface determine if its bounding box is in-front of,
C behind or crossing the bounding box of other surfaces in a zone.

      subroutine frontsurbox(icomp)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"

      integer icomp ! focus zone
      integer ncomp,ncon
      common/c1/ncomp,ncon
      integer izstocn
      common/c24/izstocn(mcom,ms)
      real surboxwid,surboxhigh,surbb
      common/surbx/surboxwid(mcon),surboxhigh(mcon),surbb(mcon,2,3)
      character FBC*1
      common/fbcarray/fbc(ms,mcon)
      integer loop,loop2,loop3
      logical positive

C Clear array.
      do loop=1,MS      ! for each surface in zone
        do loop2=1,ncon ! vs all connections
          fbc(loop,loop2)='-'
        enddo  ! of loop2
      enddo    ! of loop
      do loop=1,nzsur(icomp)    ! for each surface in zone

C We know the surface equation (sureqn) and we know that
C survn is a unit vector away from the surface cog. Capture
C whether the dot product of the surface against the point
C at survn (val) is positive or negative.
        icc=izstocn(icomp,loop)
        val=(sureqn(icomp,loop,1)*survn(icomp,loop,1))+
     &      (sureqn(icomp,loop,2)*survn(icomp,loop,2))+
     &      (sureqn(icomp,loop,3)*survn(icomp,loop,3))-
     &       sureqn(icomp,loop,4)
        if(val.gt.0.0)then
          positive=.true.
        else
          positive=.false.
        endif

C Loop through each of the other surfaces in the model and
C check whether the surface bounding box corners (lower left
C and upper right) yield positive or negative (or a mix).
        do loop2=1,ncomp
          do loop3=1,nzsur(loop2)
            icco=izstocn(loop2,loop3)
            valll=(sureqn(icomp,loop,1)*surbb(icco,1,1))+
     &      (sureqn(icomp,loop,2)*surbb(icco,1,2))+
     &      (sureqn(icomp,loop,3)*surbb(icco,1,3))-sureqn(icomp,loop,4)

            valur=(sureqn(icomp,loop,1)*surbb(icco,2,1))+
     &      (sureqn(icomp,loop,2)*surbb(icco,2,2))+
     &      (sureqn(icomp,loop,3)*surbb(icco,2,3))-sureqn(icomp,loop,4)

C If both are same sign then in front.
            if(positive)then
              if(valll.gt.0.0.and.valur.gt.0.0)then
                fbc(loop,icco)='F'
              elseif(valll.le.0.0.and.valur.le.0.0)then
                fbc(loop,icco)='B'
              else
                fbc(loop,icco)='C'
              endif
            else
              if(valll.gt.0.0.and.valur.gt.0.0)then
                fbc(loop,icco)='B'
              elseif(valll.le.0.0.and.valur.le.0.0)then
                fbc(loop,icco)='F'
              else
                fbc(loop,icco)='C'
              endif
            endif

C Ignore self.
            if(icc.eq.icco) fbc(loop,icco)='-'
          enddo  ! of loop3
        enddo  ! of loop2
        write(6,'(22a)') SNAME(icomp,loop),' ',(fbc(loop,j),j=1,20)
      enddo    ! of loop
      return
      end  ! of frontsurbox
      
C ******************** frontobsbox ********************
C For each obstruction model determine if its bounding box is in-front of,
C behind or crossing the bounding boxes of other urfaces in a zone.

      subroutine frontobsbox(icomp)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"

      integer icomp ! focus zone
      integer izstocn
      common/c24/izstocn(mcom,ms)
      character OFBC*1
      common/ofbcarray/ofbc(ms,mb)
      integer loop,loop2
      logical positive

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

C Clear array.
      do loop=1,MS    ! for each surface in zone
        do loop2=1,mb ! vs all obstructions
          ofbc(loop,loop2)='-'
        enddo  ! of loop2
      enddo    ! of loop
      do loop=1,nzsur(icomp)    ! for each surface in zone

C We know the surface equation (sureqn) and we know that
C survn is a unit vector away from the surface cog. So capture
C whether val is positive or negative.
        icc=izstocn(icomp,loop)
        val=(sureqn(icomp,loop,1)*survn(icomp,loop,1))+
     &      (sureqn(icomp,loop,2)*survn(icomp,loop,2))+
     &      (sureqn(icomp,loop,3)*survn(icomp,loop,3))-
     &       sureqn(icomp,loop,4)
        if(val.gt.0.0)then
          positive=.true.
        else
          positive=.false.
        endif

C Loop through each of the obstructions and check whether
C its bounding box corners (lower left and upper right yield
C positive or negative.
        if(nbobs(icomp).gt.0)then
          do loop2=1,nbobs(icomp)
            valll=(sureqn(icomp,loop,1)*BBO(loop2,1,1))+
     &      (sureqn(icomp,loop,2)*BBO(loop2,1,2))+
     &      (sureqn(icomp,loop,3)*BBO(loop2,1,3))-sureqn(icomp,loop,4)

            valur=(sureqn(icomp,loop,1)*BBO(loop2,2,1))+
     &      (sureqn(icomp,loop,2)*BBO(loop2,2,2))+
     &      (sureqn(icomp,loop,3)*BBO(loop2,2,3))-sureqn(icomp,loop,4)

C If both are same sign then in front.
            if(positive)then
              if(valll.gt.0.0.and.valur.gt.0.0)then
                ofbc(loop,loop2)='F'
              elseif(valll.le.0.0.and.valur.le.0.0)then
                ofbc(loop,loop2)='B'
              else
                ofbc(loop,loop2)='C'
              endif
            else
              if(valll.gt.0.0.and.valur.gt.0.0)then
                ofbc(loop,loop2)='B'
              elseif(valll.le.0.0.and.valur.le.0.0)then
                ofbc(loop,loop2)='F'
              else
                ofbc(loop,loop2)='C'
              endif
            endif
          enddo  ! of loop2
        endif    ! of nbobs
        write(6,'(22a)') SNAME(icomp,loop),' ',(ofbc(loop,j),j=1,20)
      enddo    ! of loop
      return
      end


C ******************** INCAI3 ********************
C Similar to INCAI but if ihour is zero then the ray to
C the sky patch angles are passed in (for diffuse sky patch purposes).

      subroutine incai3(icomp,ihour,sazi,salt)
#include "building.h"
#include "geometry.h"

C Parameters.
      integer icomp  ! the focus zone
      integer ihour  ! the focus hour
      real sazi,salt ! use if ihour is zero

      common/traceout/icout
      common/tc/itc,icnt
      common/cai/caii(ms),caie(ms)
      common/sangl1/sazi1,salt1
      integer izstocn
      common/c24/izstocn(mcom,ms)

      character outs*124
      logical ok,tok
      character ZSDES*28,ZSDESC*20,ZSDESS*16
      integer lnzsn,icc

C If trace is on set tok.
      tok=.false.
      if(icout.eq.33)tok=.true.

C If ihour is > zero then use common block values.
      if(ihour.gt.0)then
       sazi=sazi1; salt=salt1
      endif

      pi = 4.0 * atan(1.0)
      r=pi/180.

C  Cosine of the angle of incidence and surface-solar azimuth angle.
      do i=1,nzsur(icomp)
        icc=izstocn(icomp,i)
        if(salt.gt.0.0)then
          psazi=abs(spazi(icomp,i)-sazi)
          x1=cos(salt*r)
          x2=sin((90.-spelv(icomp,i))*r)
          x3=cos(psazi*r)
          x4=sin(salt*r)
          x5=cos((90.-spelv(icomp,i))*r)
          caie(i)=x1*x2*x3+x4*x5
          paz=spazi(icomp,i)+180.0
          if(spazi(icomp,i).gt.180.0)paz=spazi(icomp,i)-180.0
          psazi=abs(paz-sazi)
          x3=cos(psazi*r)
          x1=cos(salt*r)
          x2=sin((90.+spelv(icomp,i))*r)
          x3=cos(psazi*r)
          x4=sin(salt*r)
          x5=cos((90.+spelv(icomp,i))*r)
          caii(i)=x1*x2*x3+x4*x5
        else
          caii(i)=0.0
          caie(i)=0.0
        endif
      enddo   ! of i

C Trace output, check also that user has asked for detailed trace.
      ok=.false.
      if(itc.eq.2)ok=.true.
      if(tok.and.ok)then
        call edisp(icout,' ')
        write(outs,'(a,i3,a,i3)')'*** INCAI3: Hour',ihour,' zone',icomp
        call edisp(icout,outs)
        call edisp(icout,
     &  ' Surface    Internal    External    Azimuth    Elevation')

        do 50 i=1,nzsur(icomp)
          icc=izstocn(icomp,i)
          call zsid(icomp,i,ZSDES,ZSDESC,ZSDESS)
          lnzsn=lnblnk(ZSDES)
          write(outs,'(a,f6.4,6x,f6.4,7x,f5.1,6x,f5.1)')
     &     ZSDES(1:lnzsn),caii(i),caie(i),spazi(icomp,i),spelv(icomp,i)
          call edisp(icout,outs)
   50   continue
      endif
      return
      end  ! of incai3
