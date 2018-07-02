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

C This file contains:
C  clickonbitmap: Control interface to define vertices or floor plan
C      extrusion via clicking on a bitmap.
C  dogrid:       draw grid on bitmap using current parameters.
C  shifoverlays: update the position parameters and pixel arrays for
C      lines and vertices over a bitmap.
C  refrshcur:    Refresh current bitmap and current overlayed information.
C  ctlbmpan:     control panning of the bitmap within the graphics feedback
C      area of a esp-r module.

C *************
C Window jamb points SHOULD not be at existing wall corners.
C there should be a separate structure to hold these points.
C << TODO >>
C Door jamb points SHOULD not be at existing wall corners.
C there should be a separate structure to hold these points.
C << TODO >>

C ************* clickonbitmap
C Control interface for defining vertices or a floor plan extrusion
C via clicking on a bitmap.
      subroutine clickonbitmap(icomp,ier)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G1/G2/G4/G5/G7/prec17/precz/c20/GS5/GS6.
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

C Where ESP-r was installed (as recorded when it was compiled).
      common/deflt4/dinstpath
      character dinstpath*60

      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GT/GTNAME
      COMMON/GTFIL/GTGEOM
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)
      common/cctlnm/ctldoc,lctlf
      integer icascf
      common/cctl/icascf(mcom)

C XYZ for up to 40 furniture points.
      integer nboffurnpts
      real furx,fury,furz
      integer ifurx,ifury  ! pixels for furniture origins
      common/furncord/nboffurnpts,furx(40),fury(40),furz(40),
     & ifurx(40),ifury(40)

      DIMENSION ITEMS(23)
      DIMENSION XX(MS),YY(MS)  ! get ready to depreciate

      integer INVERT  ! number of coordinates in list
      real px,py,pz   ! user coordinates from clicks
      integer ipxx,ipyy,ipzz ! pixel coordinates from clicks
      common/clicklist/INVERT,px(MTV),py(MTV),pz(MTV),
     &  ipxx(MTV),ipyy(MTV),ipzz(MTV)

      real pxbase,pybase       ! to hold floor plan base points
      integer ipxbase,ipybase  ! to hold floor plan base pixels
      common/clickbase/pxbase(MTV),pybase(MTV),ipxbase(MTV),ipybase(MTV)  ! to hold the initial base verts

      integer nbofdoorpts
      logical wasdoorunique    ! jamb points separate from floor plan base points
      integer ibedgeofdoor     ! for each door jamb the associated base edge
      real pxdoor,pydoor       ! to hold door jambs
      integer ipxdoor,ipydoor  ! to hold door jamb pixels
      common/clickdoor/nbofdoorpts,wasdoorunique(40),ibedgeofdoor(40),
     &  pxdoor(40),pydoor(40),ipxdoor(40),ipydoor(40)  ! to hold door jambs

      integer nbofwindowpts   ! remember how many window points added
      logical waswindowunique ! for each window point was it unique
      integer ibedgeofwin     ! for each window the associated clicked edge
      real pxwin,pywin        ! to hold window jambs
      integer ipxwin,ipywin   ! to hold window jamb pixels
      common/clickwin/nbofwindowpts,waswindowunique(40),ibedgeofwin(40),
     &  pxwin(40),pywin(40),ipxwin(40),ipywin(40)  ! to hold window jambs

      integer nipwhis
      integer ipxwhis,ipywhis ! previous pixel points for floor plan
      integer iphisznst,iphisznfn ! start & end for each zone
      common/clickhis/nipwhis,ipxwhis(MGTV),ipywhis(MGTV),
     &  iphisznst(MCOM),iphisznfn(MCOM)

C Pixel (upper left and lower right corners) of where the bitmap is draw.
      integer ixbul,iybul,ixblr,iyblr ! x-up-left y-up-left x-lower-right y-lower-right
      common/bmlimits/ixbul,iybul,ixblr,iyblr

      logical xbmnorth,xbmorigin  ! logical toggles for north origin
      logical xbmgrid,xbmscale    ! logical toggles for gridding and scale set
      logical trimmed ! if only part of bitmap is displayed
      common/disptoggle/xbmnorth,xbmorigin,xbmgrid,xbmscale,trimmed
      
C << what about another array marking the type of the coordinate >>

      dimension VIEWLIM(6),GRSPC(3)

      CHARACTER ITEMS*33,outs*124,temp6*6,temp3*3,louts*496
      character phrase*124,WORD*48,msg*72,ltmp*72,fs*1
      character zn*12,dn*12
      character ctldoc*248,LCTLF*72,zd*64,DFILE*72,CFILE*72,OFILE*72
      CHARACTER GMLCN*32,GSNAME*6,GTNAME*15,GTGEOM*72,t15*15
      character longtfile*144,fname*96
      integer nfile  ! nb of files in image folder
      character sfile*72,snpfile*72
      logical xbmload,xbmsnap
      logical completed,unique,CLOSEX,CLOSEY,addinganother
      logical unixok
      logical dodoor  ! true if adding door head points
      logical dowindow ! true if adding window sill & head points
      logical dofurn  ! true if adding furniture origins

C Signal first entry to subroutine with justentered as true, once
C an object has been saved the set justentered to false.
      logical justentered
      logical firsteclick  ! so base coords only checked once.
      LOGICAL OKC,XST,dok,ok
      integer IWM,IWMG  ! for radio buttons
      integer MITEM,INO ! max items and current menu item
      integer NW  ! number of walls (points along floor plan)
      
      integer lastwalllist ! remember when we stopped asking for plan corners
      logical odd          ! for left or right jamb
      integer idjvn(40,4)  ! edge list for up to 20 doors
      integer iwjvn(40,4)  ! edge list for up to 20 windows
      
      real Z1,Z2    ! base and ceiling of floor extrusion
      real Z3,Z4,Z5 ! door head, window sill, window head

#ifdef OSI
      integer iixs1,iiys1,iixs2,iiys2,iixtn1,iiytn1,iixtn2,iiytn2
      integer iid1,iid2,iid3,iid4
      integer iix,iiy
      integer iixx,iiyy,iizz,iik
      integer iicol
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu,iixo,iiyo
      integer iixoffset,iiyoffset,iiwidth,iihight
      integer iixbul,iiybul,iixblr,iiyblr,iilix,iiliy
#else
      integer*8 iixs1,iiys1,iixs2,iiys2,iixtn1,iiytn1,iixtn2,iiytn2
      integer*8 iid1,iid2,iid3,iid4
      integer*8 iix,iiy
      integer*8 iixx,iiyy,iizz,iik
      integer*8 iicol
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu,iixo,iiyo
      integer*8 iixoffset,iiyoffset,iiwidth,iihight
      integer*8 iixbul,iiybul,iixblr,iiyblr,iilix,iiliy
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='clickonbitmap'  ! set for subroutine

C Logical toggles: xbmorigin (required origin defined) xbmnorth (optional
C north defined), xbmscale (required scaling factor defined), xbmgrid
C (optional gridding defined), xbmload (has bitmap been loaded), trimmed
C has only part of bitmap been displayed, xbmsnap (snap to grid if xbmgrid
C is true).
      itru=iuout
      xbmorigin=.false.; xbmnorth=.false.; xbmscale=.false.
      xbmgrid=.false.; xbmload=.false.; trimmed=.false.
      xbmsnap=.false.; dodoor=.false.; dowindow=.false.
      firsteclick=.false.; dofurn=.false.

C Set true to signal that user wants to add another zone.
      addinganother=.false.
      justentered=.true.

C fname (nname of the bitmap file, assumed to be in the current folder or
C specified by absolute address.
      fname=' '

C Input mode 
C   inpmode: 1 = new vertices with common Z,
C   inpmode: 2 = new vertices with different Z,
C   inpmode: 3 = floor plan extrusion with option for doors & windows,
C   inpmode: 4 = south elevation (looking north)
C   inpmode: 5 = east elevation (looking west)
C   inpmode: 6 = ground topology,
C   inpmode: 7 = obstructions,
C   inpmode: 8 = nothing (not enough toggles set to do this yet) 
      inpmode=8

C Assumed initial number of walls for extruded floor
C plan case(NW), initial base Z (z1) initial top (z2).
      NW=0; Z1=0.0; Z2=2.4; Z3=2.1; Z4=0.7; Z5=2.1
      Y1=0.0; X1=0.0; other=0.1

C Reset index when last extrusion point entered.
      lastwalllist=0; nbofdoorpts=0; nbofwindowpts=0
      nbofobspts=0; nboffurnpts=0
      do loop=1,40
        wasdoorunique(loop)=.false.
        waswindowunique(loop)=.false.
        ibedgeofdoor(loop)=0; ibedgeofwin(loop)=0
        furx(loop)=0.0; fury(loop)=0.0; furz(loop)=0.0
        ifurx(loop)=0; ifury(loop)=0
        pxwin(loop)=0.0; pywin(loop)=0.0
        ipxwin(loop)=0; ipywin(loop)=0

      enddo

C nbobs() is the number of obstruction blocks. NBL is number of
C historical points for prior blocks (NBL = nbobs*3 because each
C block is defined with an origin point, a point at end of
C the front face and a point at back right corner).
      NBFREL=0

C Pixel (upper left and lower right corners) of where the bitmap was draw.
      ixbul=0; iybul=0; ixblr=0; iyblr=0

C Defined width and height of bitmap and trimmed width and height.
      ibmwidth=0; ibmhight=0; iwidth=0; ihight=0

C User requested pixel offsets (when panning).
      ixoffset=0; iyoffset=0

C Two points defining north and angle from north.
      ixtn1=0; iytn1=0; ixtn2=0; iytn2=0; ang=0.0

C Scaling factor (pixels/m), bituwidth is the user units in the width
C of the bitmap, bituheight is the user units in the height of the
C bitmap. Uxoffset is the user coord at the left of the bitmap,
C uyoffset is the user coord at the bottom of the bitmap.
      factor=0.0; bituwidth=0.0; bituheight=0.0
      uxoffset=0.0; uyoffset=0.0

C viewlim(1) and viewlim(2) are the user unit left and right boundaries and
C viewlim(3) and viewlim(4) are the lower and upper boundaries.
      viewlim(1)=0.0; viewlim(2)=0.0; viewlim(3)=0.0; viewlim(4)=0.0
      
C VIEWLIM(6)     - current view limits (x1,x2,y1,y2,z1,z2)

C Gridding flag indicating the density of the grid.
      iwmg=0
      GRSPC(1)=0.0; GRSPC(2)=0.0; GRSPC(3)=0.0

C Vertices and pixels (clear list of pixels for vertices and corners).
C MTV arrays should hold the master list of points.
      INVERT=0
      do 7 i=1,MTV
        px(i)=0.0; py(i)=0.0; pz(i)=0.0
        ipxx(i)=0; ipyy(i)=0; ipzz(i)=0
        pxbase(i)=0.0; pybase(i)=0.0
        ipxbase(i)=0; ipybase(i)=0
  7   continue

C Clear history of pixel base points. nipwhis is the cummulative number
C of historical base points. Should not include the door heads and window
C sill and heads in this count.
      nipwhis=0
      do 9 i=1,MGTV
        ipxwhis(i)=0
        ipywhis(i)=0
        if(i.le.MCOM)then
          iphisznst(i)=0
          iphisznfn(i)=0
        endif
  9   continue

C Warn user if icomp is zero, this implies that the clickonbitmap
C has been called from ground topology menu or obstructions. If 
C we have a zone then fill ZN and ZD so we know we can proceed 
C with entering points.  Otherwise clear ZN and ZD.
      if(icomp.eq.0)then
        call usrmsg('Currently no focus zone so assuming focus on',
     &              'ground topology or shading obstructions.','W')
        ZN='  '
        zd='  '
      else
        ZN=zname(icomp)
        zd=zdesc(icomp)
      endif

C Give a bit more space in graphic feedback area for the bitmap.
      CALL startbuffer()

C Setup and pass in parameters to win3d.
      iiw1=1; iiw2=3; iiw3=1; iiw4=1; iimenu=menuchw
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)

  142 continue
    3 INO=-4
      m=0
      n=0

C Point and click on a bitmap.
      WRITE(ITEMS(1),'(A,1X,A20)')  'a XBM file:',fname(1:20)

C I part of the bitmap shows then allow user to pan left or right or
C up and down (by 50 pixel increments).
      if(trimmed)then
        m=m+1
        WRITE(ITEMS(1+m),'(A)')     'b  pan bitmap '
      endif
      if(xbmload)then
        n=n+1
        WRITE(ITEMS(1+m+n),'(A)')   'c  refresh bitmap '
      endif
      if(xbmorigin)then
        ITEMS(2+m+n)='d  origin: defined'
      else
        ITEMS(2+m+n)='d  origin: not defined'
      endif
      if(xbmnorth)then
        write(ITEMS(3+m+n),'(a,F7.2,a)') 'e  north: ',ang,' deg'
      else
        ITEMS(3+m+n)='e  north: not defined'
      endif
      if(xbmscale)then
        write(ITEMS(4+m+n),'(a,F7.3,a)') 'f  scaling: ',factor,
     &    ' pixels/metre'
      else
        ITEMS(4+m+n)='f  scaling: not defined'
      endif
      if(xbmgrid)then
        if(iwmg.eq.1)then
          ITEMS(5+m+n)='g  gridding: defined as none'
        elseif(iwmg.eq.2)then
          ITEMS(5+m+n)='g  gridding: @ 2.0m'
        elseif(iwmg.eq.3)then
          ITEMS(5+m+n)='g  gridding: @ 1.0m'
        elseif(iwmg.eq.4)then
          ITEMS(5+m+n)='g  gridding: @ 0.5m'
        elseif(iwmg.eq.5)then
          ITEMS(5+m+n)='g  gridding: @ 0.25m'
        elseif(iwmg.eq.6)then
          ITEMS(5+m+n)='g  gridding: @ 0.1m'
        elseif(iwmg.eq.7)then
          write(ITEMS(5+m+n),'(a,f5.3)') 'g  gridding: @ ',other
        else
          ITEMS(5+m+n)='g  gridding: defined'
        endif
      else
        ITEMS(5+m+n)='g  gridding: not defined'
      endif
      if(xbmsnap)then
        ITEMS(6+m+n)='h  snap-to >> on           '
      else
        ITEMS(6+m+n)='h  snap-to >> off          '
      endif
      ITEMS(7+m+n)='  _________________________  '

      if(inpmode.eq.1)then
        ITEMS(8+m+n)='i mode >> pick vertices at one Z'
        WRITE(ITEMS(9+m+n),'(A,1X,F8.4)')'j  Z coordinate @',Z1
        WRITE(ITEMS(10+m+n),'(A,1X,i4)') 'k  nb. of vertices ',INVERT
        ITEMS(11+m+n)='                             '
        ITEMS(12+m+n)='                             '
        ITEMS(13+m+n)='                             '
        if(trimmed)then
          ITEMS(14+m+n)='m  start (type `e` end `p` pan)'
        else
          ITEMS(14+m+n)='m  start (type `e` to end)   '
        endif
      elseif(inpmode.eq.2)then
        ITEMS(8+m+n)='i mode >> pick vertices @ diff Z'
        ITEMS(9+m+n)='                             '
        WRITE(ITEMS(10+m),'(A,1X,i4)')  'k  nb. of vertices ',INVERT
        ITEMS(11+m+n)='                             '
        ITEMS(12+m+n)='                             '
        ITEMS(13+m+n)='                             '
        if(trimmed)then
          ITEMS(14+m+n)='m  start (type `e` end `p` pan)'
        else
          ITEMS(14+m+n)='m  start (type `e` to end)   '
        endif
      elseif(inpmode.eq.3)then

C Include extras for door heights and glazing sill & heads during
C postprocessing.
        ITEMS(8+m+n)='i mode >> pick floor plan'
        WRITE(ITEMS(9+m+n),'(A,1X,F8.4)')  'j  floor @  :',Z1
        WRITE(ITEMS(10+m+n),'(A,1X,F8.4)') 'k  ceiling @:',Z2
        WRITE(ITEMS(11+m+n),'(A,1X,F8.4)') 'l  door head @:',Z3
        WRITE(ITEMS(12+m+n),'(A,1X,2F6.3)')'m  wind sill head @:',
     &    Z4,Z5
        WRITE(ITEMS(13+m+n),'(A,1X,i3)')   'n  nb. of walls ',NW
        if(trimmed)then
          ITEMS(14+m+n)='m  start (type `e` end `p` pan)'
        else
          ITEMS(14+m+n)='m  start (type `e` to end)   '
        endif
      elseif(inpmode.eq.4)then
        ITEMS(8+m+n)='i mode >> elevation (south)'
        WRITE(ITEMS(9+m+n),'(A,1X,F8.4)')'j  Y coordinate @',Y1
        WRITE(ITEMS(10+m+n),'(A,1X,i4)') 'k  nb. of vertices ',INVERT
        ITEMS(11+m+n)='                             '
        ITEMS(12+m+n)='                             '
        ITEMS(13+m+n)='                             '
        if(trimmed)then
          ITEMS(14+m+n)='m  start (type `e` end `p` pan)'
        else
          ITEMS(14+m+n)='m  start (type `e` to end)   '
        endif
      elseif(inpmode.eq.5)then
        ITEMS(8+m+n)='i mode >> elevation (east)'
        WRITE(ITEMS(9+m+n),'(A,1X,F8.4)')'j  X coordinate @',X1
        WRITE(ITEMS(10+m+n),'(A,1X,i4)') 'k  nb. of vertices ',INVERT
        ITEMS(11+m+n)='                             '
        ITEMS(12+m+n)='                             '
        ITEMS(13+m+n)='                             '
        if(trimmed)then
          ITEMS(14+m+n)='m  start (type `e` end `p` pan)'
        else
          ITEMS(14+m+n)='m  start (type `e` to end)   '
        endif
      elseif(inpmode.eq.6)then
        ITEMS(8+m+n)='i mode >> ground topology'
        ITEMS(9+m+n)='                             '
        WRITE(ITEMS(10+m+n),'(A,1X,i4)') 'k  nb. of vertices ',INVERT
        ITEMS(11+m+n)='                             '
        ITEMS(12+m+n)='                             '
        ITEMS(13+m+n)='                             '
        if(trimmed)then
          ITEMS(14+m+n)='m  start (type `e` end `p` pan)'
        else
          ITEMS(14+m+n)='m  start (type `e` to end)   '
        endif
      elseif(inpmode.eq.7)then
        ITEMS(8+m+n)='i mode >> Future facility'
        ITEMS(9+m+n) ='  . . .                         '
        ITEMS(10+m+n)='  . . .                         '
        ITEMS(11+m+n)='  . . .                         '
        ITEMS(12+m+n)='  . . .                         '
        ITEMS(13+m+n)='  . . .                         '
        ITEMS(14+m+n)='  . . .                         '
      elseif(inpmode.eq.8)then
        ITEMS(8+m+n)='i mode >>                        '
        ITEMS(9+m+n) ='  . . .                         '
        ITEMS(10+m+n)='  . . .                         '
        ITEMS(11+m+n)='  . . .                         '
        ITEMS(12+m+n)='  . . .                         '
        ITEMS(13+m+n)='  . . .                         '
        ITEMS(14+m+n)='  . . .                         '
      endif
      ITEMS(15+m+n)=  '  _________________________     '

      ITEMS(16+m+n)=  '! list coordinates              '
      if(justentered)then
        ITEMS(17+m+n)=  '  . . .                       '
      else
        if(inpmode.eq.7)then
          ITEMS(17+m+n)='< not applicable              '
        elseif(inpmode.eq.6)then
          ITEMS(17+m+n)='< not applicable              '
        else
          ITEMS(17+m+n)='< create another zone         '
        endif
      endif
      if(inpmode.eq.6)then
        ITEMS(18+m+n)='> save ground topology          '
      else
        ITEMS(18+m+n)='  . . .                         '
      endif
      ITEMS(19+m+n)=  '? help                          '
      ITEMS(20+m+n)=  '- exit                          '
      MITEM=20+m+n

C Help text for this menu.
      helptopic='clickon_bitmap_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('  Point definition',ITEMS,MITEM,INO)
  29  IF(INO.EQ.MITEM)THEN

C On exit, check save data depending on input mode.
        if(inpmode.eq.6)then

C Save any ground topology.
          dok=.false.
          call askok('Have you saved the ground topology?',' ',ok,
     &      dok,nbhelp)
          if(ok)then
            return
          else
            goto 142
          endif
        elseif(inpmode.eq.7)then
          return
        elseif(inpmode.eq.8)then
          return
        else
          return
        endif

      ELSEIF(INO.EQ.MITEM-1)THEN

C Help.
        helptopic='clickon_bitmap_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('bitmap opening',nbhelp,'-',0,0,IER)

      elseif(INO.EQ.MITEM-2)then

C Write data to file depending on the input mode.
        if(inpmode.eq.6)then

C Save any ground topology.
          if(NGT.ge.1)then
            write(outs,'(3a)')'Updating ',gtname(1:lnblnk(gtname)),
     &        '...'
            call edisp(iuout,outs)
            call egrout(IFIL+2,gtgeom,iuout,ier)
            CALL EMKCFG('-',IER)
            write(outs,'(3a)')'Updating ',gtname(1:lnblnk(gtname)),
     &        '...done.'
            call edisp(iuout,outs)
          endif
        else

          continue
        endif

      elseif(INO.EQ.MITEM-3)then

C Offer to start another object.
        call easkab('Options for adding objects (see help):',' ',
     &    'start clicking on points','continue',iwclear,nbhelp)
        if(iwclear.eq.2)then
          goto 142
        endif

C Clear from any prior zone additions.
        lastwalllist=0; nbofdoorpts=0; nbofwindowpts=0
        nbofobspts=0; nboffurnpts=0
        dodoor=.false.; dowindow=.false.; dofurn=.false.
        do loop=1,40
          wasdoorunique(loop)=.false.
          waswindowunique(loop)=.false.
          ibedgeofdoor(loop)=0; ibedgeofwin(loop)=0
          furx(loop)=0.0; fury(loop)=0.0; furz(loop)=0.0
          ifurx(loop)=0; ifury(loop)=0
          pxwin(loop)=0.0; pywin(loop)=0.0
          ipxwin(loop)=0; ipywin(loop)=0
        enddo

C Clear and start new zone or ground topology or obstruction.
        if(inpmode.eq.6)then
          NGT=0; NGTV=0
          if(gtgeom(1:4).eq.'UNKN'.or.gtgeom(1:2).eq.'  ')then
            write(ltmp,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.grnd'
          else
            ltmp=gtgeom
          endif
 56       CALL EASKS(ltmp,' Ground topology/geometry file name ? ',
     &      ' ',72,'grnd.geo','ground geom file',IER,nbhelp)
          if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
            gtgeom=ltmp
          else
            goto 56
          endif

C Get the name of the ground. Also set ZN to this to allow
C entry to point clicking.
          GTNAME='from_bitmap'
  49      t15=GTNAME
          CALL EASKS(t15,' Description of the ground? (<12 char)',
     &      ' ',15,'flat','not flat',IER,nbhelp)
          if(t15(1:2).ne.'  '.and.t15(1:4).ne.'UNKN')then
            GTNAME=t15
            ZN=t15(1:12)
          else
            goto 49
          endif
        elseif(inpmode.eq.7)then
          continue
        else

C Before clearing current zone data save configuration.
          CALL EMKCFG('-',IER)

C After updating configuration, setup for another zone. Remember
C the last historical index if inpmode=3.
          if(inpmode.eq.1.or.inpmode.eq.2.or.inpmode.eq.4.or.
     &       inpmode.eq.5)then
            INVERT=0

C << is this a place to clear the clicklist arrays? >>

          else if(inpmode.eq.3)then
            NW=0

C << TODO Clear doors & windows bookkeepping >>

          endif

C Creation of another new zone, first ask its name (ZN), trapping out
C illegal characters. Setup default file names (DFILE/CFILE/OFILE).
C Mark for increment ICOMP counter if the user decides to save the
C collected points as a zone.
          addinganother=.true.
          INCOMP=NCOMP+1
          call tstamp('>','PRJ: new zone')
 42       ZN=' '
          CALL EASKS(ZN,'What do you want to call this new zone ',
     &      ' ( <12 char, no blanks) ?',12,'new_zone','zone name',
     &      IER,nbhelp)
          IF(ZN.eq.' '.or.ier.ne.0)goto 42

C << Unix vs NT needs to be updated >>
          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(DFILE,'(2a)') zn(1:lnblnk(zn)),'.geo'
            WRITE(CFILE,'(2a)') zn(1:lnblnk(zn)),'.con'
            WRITE(OFILE,'(2a)') zn(1:lnblnk(zn)),'.opr'
          else
            WRITE(DFILE,'(4a)') zonepth(1:lnblnk(zonepth)),'/',
     &        zn(1:lnblnk(zn)),'.geo'
            WRITE(CFILE,'(4a)') zonepth(1:lnblnk(zonepth)),'/',
     &        zn(1:lnblnk(zn)),'.con'
            WRITE(OFILE,'(4a)') zonepth(1:lnblnk(zonepth)),'/',
     &        zn(1:lnblnk(zn)),'.opr'
          endif

          write(zd,'(2a)') zn(1:lnblnk(zn)),' describes a...'
 43       CALL EASKS(zd,' What does it represent ',
     &      ' ( <64 char) ?',64,'no description entered','zone descr',
     &      IER,nbhelp)
          IF(zd.eq.' '.or.ier.ne.0)goto 43
        endif
        call edisp(iuout,'BEGIN ENTERING POINTS NOW...')
        INO=14+m+n
        goto 29

      elseif(INO.EQ.MITEM-4)then

C List the current vertices and corners.
        if(INVERT.gt.0)then
          if(inpmode.eq.6)then
            do 119 j=1,INVERT
              write(outs,'(a,i3,a,f8.3,a,f8.3,a,2i5)') 'Vertex ',j,
     &        ' @ X',px(j),' Y',py(j),' and pixel: ',ipxx(j),ipyy(j)
              call edisp(iuout,outs)
  119       continue
          elseif(inpmode.eq.7)then
            continue
          elseif(inpmode.eq.8)then
            continue
          elseif(inpmode.eq.1.or.inpmode.eq.2)then
            do 120 j=1,INVERT
              write(outs,'(a,i3,a,f8.3,a,f8.3,a,2i5)') 'Vertex ',j,
     &        ' @ X',px(j),' Y',py(j),' and pixel: ',ipxx(j),ipyy(j)
              call edisp(iuout,outs)
  120       continue
          elseif(inpmode.eq.4)then
            do 121 j=1,INVERT
              write(outs,'(a,i3,a,f8.3,a,f8.3,a,2i5)') 'Vertex ',j,
     &        ' @ X',px(j),' Z',pz(j),' and pixel: ',ipxx(j),ipzz(j)
              call edisp(iuout,outs)
  121       continue
          elseif(inpmode.eq.5)then
            do 122 j=1,INVERT
              write(outs,'(a,i3,a,f8.3,a,f8.3,a,2i5)') 'Vertex ',j,
     &        ' @ Y',py(j),' Z',pz(j),' and pixel: ',ipyy(j),ipzz(j)
              call edisp(iuout,outs)
  122       continue
          endif
        endif
        if(NW.gt.0)then
          do 123 j=1,NW
            if(j.eq.1)then
              write(outs,'(a,i3,a,f8.3,a,f8.3)') 'Corner ',j,
     &          ' @ X',pxbase(j),' Y',pybase(j)
            else
              tdis= crowxyz(pxbase(j-1),pybase(j-1),0.0,
     &              pxbase(j),pybase(j),0.0)
              call ln2az(pxbase(j-1),pybase(j-1),0.0,
     &          pxbase(j),pybase(j),0.0,az,el)
              write(outs,'(a,i3,a,f8.3,a,f8.3,a,f7.3,a,f8.3,a,f7.3)') 
     &         'Corner ',j,' @ X',pxbase(j),' Y',pybase(j),' distance',
     &          tdis,'m and @ aimuth ',az,' & elev ',el
            endif
            call edisp(iuout,outs)
  123     continue
        endif

      elseif(INO.EQ.1)then

C Load an X11 bitmap file (user required to convert to the format).
        helptopic='x11_bitmap_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        idno=7
        call MENUATOL('Bitmap options: (see help)',' ',
     &    'a small blank bitmap','b small with ticks for gridding',
     &    'c medium blank bitmap','d medium with ticks for gridding',
     &    'e large blank bitmap','f large with ticks for gridding',
     &    'g user defined bitmap','h bitmap in images folder',' ',
     &    ' ',' ',' ',inptb,idno,nbhelp)

C Locate the xbm files via the installed path.
        call isunix(unixok)
        if(unixok)then
          fs = char(47)
        else
          fs = char(92)
        endif
        if(inptb.eq.1)then
          write(fname,'(5a)') dinstpath(1:lnblnk(dinstpath)),fs,'lib',
     &      fs,'40x40_notick.xbm'
        elseif(inptb.eq.2)then
          write(fname,'(5a)') dinstpath(1:lnblnk(dinstpath)),fs,'lib',
     &      fs,'40x40.xbm'
        elseif(inptb.eq.3)then
          write(fname,'(5a)') dinstpath(1:lnblnk(dinstpath)),fs,'lib',
     &      fs,'50x50_notick.xbm'
        elseif(inptb.eq.4)then
          write(fname,'(5a)') dinstpath(1:lnblnk(dinstpath)),fs,'lib',
     &      fs,'50x50.xbm'
        elseif(inptb.eq.5)then
          write(fname,'(5a)') dinstpath(1:lnblnk(dinstpath)),fs,'lib',
     &      fs,'60x60_notick.xbm'
        elseif(inptb.eq.6)then
          write(fname,'(5a)') dinstpath(1:lnblnk(dinstpath)),fs,'lib',
     &      fs,'60x60.xbm'
        elseif(inptb.eq.7)then
          write(fname,'(2a)') '.',fs
        elseif(inptb.eq.8)then

C Use code similar to that in edzone.F subroutine newzone and in
C folders.F pfolders to get files in ../images folder and recover
C the name of the xbm file.
          sfile=' '
          snpfile=' '
          call edisp(iuout,' ')
          call browsefilelist('?','img','fil',sfile,snpfile,nfile,iier)
          if(nfile.gt.0)then
            sfile=' '
            snpfile=' '
            call browsefilelist('b','img','fil',sfile,snpfile,nfile,
     &        iier)
            if(snpfile(1:2).ne.'  ')then
              write(fname,'(3a)')imgpth(1:lnblnk(imgpth)),fs,
     &          snpfile(1:lnblnk(snpfile))

C Debug.
C              write(6,*) fname

            else

C User did not select a file. Take them back to the question.
              call usrmsg('You did not select any existing file.',
     &          'Please select or cancel.','W')
              goto 142 ! ask for choice again
            endif
          else
            call usrmsg('You did not select any existing file.',
     &       'Please select or cancel.','W')
            goto 142 ! ask for choice again
          endif
        endif
        longtfile=' '    ! reset return string
        CALL EASKXORGTKF(fname,'X11 bitmap file name?',' ',
     &    'xxx.xbm',longtfile,'bitmap file',IER,nbhelp)
        write(fname,'(a)') longtfile(1:lnblnk(longtfile))
     
C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 142  ! redisplay menu.
        endif
        if(fname(1:2).ne.'  ')then

C See what size the bitmap is and if it is an Xll bitmap.
C Read in the first two lines of the bitmap file and if they begin
C with #define they are assumed to be X11 bitmaps. The pixel width
C and pixel height are also read.
          iin=IFIL+6
          CALL ERPFREE(iin,ISTAT)
          call FPOPEN(iin,ISTAT,1,1,fname)
          if(ISTAT.eq.0)then
            phrase=' '
            read(iin,'(a)',iostat=ios,err=99) phrase
            if(phrase(1:8).eq.'#define ')then
              K=8
              CALL EGETW(phrase,K,WORD,'W','bitmap name',IFLAG)
              CALL EGETWI(phrase,K,ibmwidth,1,3000,'W','bitmap w',IER)
              read(iin,'(a)',iostat=ios,err=99) phrase
              if(phrase(1:8).eq.'#define ')then
                K=8
                CALL EGETW(phrase,K,WORD,'W','bitmap name',IFLAG)
                CALL EGETWI(phrase,K,ibmhight,1,3000,'W','bitmap h',
     &            IER)
              endif

C Feedback.
              write(outs,'(a,i4,a,i4,a)') 
     &          'The bitmap file is ',ibmwidth,' by ',
     &          ibmhight,' pixels.'
              call edisp(iuout,outs)
              CALL ERPFREE(iin,ISTAT)

C If we reached this point then clear the display and deal with whether
C the bitmap has to be trimmed. Set local variables iwidth and ihight
C for passing to winlodpart to the smaller of the display w&h or the
C bitmap w&h. Note ixoffset and iyoffset indicate how far from the
C upper left corner of the bitmap data should be read from (to support
C panning). Note: the parameters ixbul,iybul,ixblr,iyblr are the pixel
C positions of the bitmap as drawn on the display.
              call startbuffer()
              if(igw.lt.ibmwidth.or.igwh.lt.ibmhight)then
                trimmed=.true.
                ixoffset=0

C If bitmap is taller, adjust so we start with lower left corner.
                if(igwh.lt.ibmhight)then
                  iyoffset=(ibmhight-igwh)
                  write(outs,*) 'reset to lower left corner ',iyoffset
                  call edisp(iuout,outs)
                else
                  iyoffset=0
                endif
                iwidth=ibmwidth
                if(igw.lt.ibmwidth) iwidth=igw
                ihight=ibmhight
                if(igwh.lt.ibmhight) ihight=igwh
                write(outs,'(a,i4,a,i4,a,i4,a,i4)')
     &          ' The bitmap was trimmed to ',iwidth,
     &            ' x',ihight,' pixels from ',ibmwidth,' x',ibmhight
                call edisp(iuout,outs)

C Cast data types for C code.
                iixoffset=ixoffset; iiyoffset=iyoffset
                iiwidth=iwidth; iihight=ihight
                iixbul=ixbul; iiybul=iybul; iixblr=ixblr; iiyblr=iyblr
                iilix=15; iiliy=15
                call winlodpart(fname,iixoffset,iiyoffset,iiwidth,
     &            iihight,iilix,iiliy,iixbul,iiybul,iixblr,iiyblr)
                ixbul=int(iixbul); iybul=int(iiybul)
                ixblr=int(iixblr); iyblr=int(iiyblr)
              else
                trimmed=.false.
                ixoffset=0; iyoffset=0
                iwidth=ibmwidth; ihight=ibmhight

C Cast data types for C code.
                iixoffset=ixoffset; iiyoffset=iyoffset
                iiwidth=iwidth; iihight=ihight
                iixbul=ixbul; iiybul=iybul; iixblr=ixblr; iiyblr=iyblr
                iilix=10; iiliy=10
                call winlodpart(fname,iixoffset,iiyoffset,iiwidth,
     &            iihight,iilix,iiliy,iixbul,iiybul,iixblr,iiyblr)
                ixbul=int(iixbul); iybul=int(iiybul)
                ixblr=int(iixblr); iyblr=int(iiyblr)
              endif

C After loading the bitmap populate the display with corners, north,
C origin if these have been defined.

C Debug.
C              write(6,*) 'bitmap drawn at ',ixbul,iybul,ixblr,iyblr
              iicol=0
              call winscl('z',iicol)
              iid1=ixbul; iid2=iybul;
              call esymbol(iid1,iid2,6,1)
              iid1=ixblr; iid2=iyblr;
              call esymbol(iid1,iid2,4,1)
              call winscl('-',iicol)
              xbmload=.true.
            else
              call usrmsg('This file is not an X11 bitmap file.',
     &                    'Sorry.... ','W')
              CALL ERPFREE(iin,ISTAT)
              goto 142
            endif
          endif
        endif
        call edisp(iuout,'What to do next... ')
        if(xbmorigin)then
          call edisp(iuout,' a) define an origin - DONE')
        else
          call edisp(iuout,' a) define an origin')
        endif
        if(xbmnorth)then
          call edisp(iuout,' b) define north - DONE')
        else
          call edisp(iuout,' b) define north (optional)')
        endif
        if(xbmscale)then
          call edisp(iuout,' c) define scale - DONE')
        else
          call edisp(iuout,' c) define scale')
        endif
        call edisp(iuout,' d) toggle on grid (optional)')
        call edisp(iuout,' e) toggle on grid-snap (optional)')
        call edisp(iuout,' f) set the input mode')
        goto 142

      elseif(INO.EQ.2.and.m.eq.1)then

C Support bitmap panning - left, right, up, down.
        if(.NOT.trimmed) goto 142
        helptopic='click_pan_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
  199   write(msg,'(a,i4,a,i4,a)') 
     &    'Panning options: (current offset x',ixoffset,' y ',
     &    iyoffset,' pixels)'
        IWM=1
        CALL EASKATOG(msg,' ','done','left','right','up','down',' ',
     &    ' ',IWM,nbhelp)
        if(iwm.eq.1.or.iwm.gt.5)then
          goto 142
        else
          inpm=inpmode
          call ctlbmpan(iwm,fname,ixoffset,iyoffset,ibmwidth,ibmhight,
     &      iwidth,ihight,inpm,nw,ixtn1,ixtn2,
     &      iytn1,iytn2,ixo,iyo)

C Refresh current bitmap and overlayed information.
          completed=.true.
          inpm=inpmode
          lastwalllist=nw
          call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,inpm,
     &      iwmg,factor,viewlim,nw,ixtn1,ixtn2,iytn1,iytn2,
     &      ixo,iyo,completed,other,lastwalllist)
          goto 199
        endif

      elseif((INO.EQ.2.and.m.eq.0.and.n.eq.1).or.
     &       (INO.EQ.3.and.m.eq.1.and.n.eq.1))then

C Refresh current bitmap and overlayed information.
        completed=.true.
        inpm=inpmode
        lastwalllist=nw
        call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,inpm,
     &    iwmg,factor,viewlim,nw,ixtn1,ixtn2,iytn1,iytn2,
     &    ixo,iyo,completed,other,lastwalllist)

      elseif(INO.EQ.2+m+n)then

C Origin within the bitmap.
        if(xbmorigin)then
          dok=.false.
          CALL ASKOK(' ',' Redefine origin on the bitmap? ',OK,
     &      dok,nbhelp)
          if(OK) xbmorigin = .false.
        endif
        if(.NOT.xbmorigin)then
          call edisp(iuout,' ')
          call usrmsg('Select origin via cursor (click left button)',
     &      ' ','-')
          iicol=0
          call winscl('z',iicol)
          CALL trackview(iik,iixo,iiyo)

C Debug
C          write(6,*) 'origin track click',iik,iixo,iiyo
          iid1=int(iixo); iid2=int(iiyo);
          ixo=int(iixo); iyo=int(iiyo);
          call esymbol(iid1,iid2,24,1)
          write(temp6,'(A)')'Origin'
          iix=iixo+5; iiy=iiyo-5;
          CALL wstxpt(iix,iiy,temp6)
          call winscl('-',iicol)
          write(outs,'(a,2i6)')' origin X Y pixels =',iixo,iiyo
          call edisp(iuout,outs)
          call usrmsg(' ',' ','-')
          xbmorigin=.true.
        endif
        goto 142

      elseif(INO.EQ.3+m+n)then

C North.
        if(xbmnorth)then
          dok=.false.
          CALL ASKOK(' ','Redefine north on the bitmap? ',
     &      OK,dok,nbhelp)
          if(OK) xbmnorth = .false.
        endif
        if(.NOT.xbmnorth)then
          call edisp(iuout,' ')
          call usrmsg('Select two points defining north axis... ',
     &      ' ','-')
          iicol=0
          call winscl('z',iicol)
          CALL trackview(iik,iixtn1,iiytn1)
          ixtn1=int(iixtn1); iytn1=int(iiytn1)
          iid1=int(iixtn1); iid2=int(iiytn1)
          call esymbol(iid1,iid2,11,1)
          CALL trackview(iik,iixtn2,iiytn2)
          ixtn2=int(iixtn2); iytn2=int(iiytn2);
          iid1=int(iixtn2); iid2=int(iiytn2)
          call esymbol(iid1,iid2,18,1)
          iid1=ixtn1; iid2=iytn1; iid3=ixtn2; iid4=iytn2;
          call edwline(iid1,iid2,iid3,iid4)
          write(temp6,'(A)')'North'
          iid1=ixtn2+5; iid2=iytn2-5;
          CALL wstxpt(iid1,iid2,temp6)
          call winscl('-',iicol)
          call usrmsg(' ',' ','-')
          xbmnorth=.true.
        endif
        goto 142

      elseif(INO.EQ.4+m+n)then

C Scaling of a known line. Ask for two points and distance.
        if(xbmscale)then
          dok=.false.
          CALL ASKOK(' ','Redefine scale on the bitmap?',
     &      OK,dok,nbhelp)
          if(OK) xbmscale = .false.
        endif
        if(.NOT.xbmscale)then
          call edisp(iuout,' ')
          call usrmsg('Define the bitmap scale by selecting ',
     &      'two points along a known line... ','-')
          iicol=0
          call winscl('z',iicol)

C Get the pixel locations iixs1,iiys1 and iixs2,iiys2
          CALL trackview(iik,iixs1,iiys1)
          call esymbol(iixs1,iiys1,11,1)
          CALL trackview(iik,iixs2,iiys2)
          call esymbol(iixs2,iiys2,11,2)
          call winscl('-',iicol)
          call usrmsg(' ',' ','-')
          disn=1.0
          CALL EASKR(disn,' ','Distance (metres) between points?',
     &      0.0001,'W',0.,'-',0.1,'dist',IER,nbhelp)
          vdis= crowxyz(float(iixs1),float(iiys1),0.0,float(iixs2),
     &      float(iiys2),0.0)
          write(outs,*) 'pixels between points ',vdis
          call edisp(iuout,outs)
          factor=vdis/disn
          write(outs,*)' There are ',factor,' pixels per metre.'
          call edisp(iuout,outs)
          iicol=0
          call winscl('z',iicol)
          call eswline(iixs1,iiys1,iixs2,iiys2)

C Try test point along screen Y and check for angle between it and the
C point nominated. No transform is yet applied.
          call ang3vtx(float(ixtn1),float(iytn1-50),0.,float(ixtn1),
     &      float(iytn1),0.,float(ixtn2),float(iytn2),0.0,ang)
          if(ang.gt.1.5)then
            iixtn1=ixtn1; iiytn1=iytn1; iixtn2=ixtn2; iiytn2=iytn2;
            call eswline(iixtn1,iiytn1,iixtn2,iiytn2)
            if(ixtn2.gt.ixtn1)then
              write(outs,'(a,f9.4)') 
     &          'Clockwise angle bitmap north line and vertical is',ang
              call edisp(iuout,outs)
            elseif(ixtn2.lt.ixtn1)then
              write(outs,'(a,f9.4)') 
     &      'Anticlockwise angle bitmap north line and vertical is',ang
              call edisp(iuout,outs)
            endif
          else
            ang = 0.0
          endif

C Report where the two scaling points were.
          pxs=float(iixs1-iixo)/factor
          pys=float(iiyo-iiys1)/factor
          write(outs,'(a,f8.3,a,f8.3)') 'First scale point at X',pxs,
     &      ' Y ',pys
          call edisp(iuout,outs)
          pxs=float(iixs2-iixo)/factor
          pys=float(iiyo-iiys2)/factor
          write(outs,'(a,f8.3,a,f8.3)') '2nd scale point at X',pxs,
     &      ' Y ',pys
          call edisp(iuout,outs)

C Using the upper left ixbul,iybul and lower right ixblr,iyblr calculate
C the extents of the bitmap in user units. bituwidth is the user units
C in the width of the bitmap, Uxoffset is the user coord at the left of the bitmap,
C uyoffset is the user coord at the bottom of the bitmap.
          bituwidth=float(ixblr-ixbul)/factor
          bituheight=float(iyblr-iybul)/factor
          uxoffset=float(ixbul-iixo)/factor
          uyoffset=float(iyo-iyblr)/factor

C For purposes of drawing a grid find the nearest metre in the X and Y
C where viewlim(1) and viewlim(2) are the left and right boundaries and
C where viewlim(3) and viewlim(4) are the lower and upper boundaries.
          viewlim(1)=float(int(uxoffset))
          viewlim(2)=real(anint(viewlim(1) + bituwidth))
          viewlim(3)=float(int(uyoffset))
          viewlim(4)=real(anint(viewlim(3) + bituheight))
          xbmscale=.true.
          call winscl('z',iicol)
        endif
        goto 142

      elseif(INO.EQ.5+m+n)then

C Gridding options. Check if user wants a grid.
        if(.NOT.xbmorigin)then
          call usrmsg('Please define origin first...',' ','W')
          goto 142
        endif
        if(.NOT.xbmscale)then
          call usrmsg('Please define scaling factors first...',' ','W')
          goto 142
        endif
        if(xbmgrid)then
          dok=.false.
          CALL ASKOK(' ','Redefine grid on the bitmap?',OK,dok,nbhelp)
          if(OK) xbmgrid = .false.
        endif
        if(.NOT.xbmgrid)then
          IWMG=1
          CALL EASKATOG('Gridding options:',' ',
     &      'none','2.0m','1.0m','0.5m','0.25m','0.1m','other',
     &      IWMG,nbhelp)
          if(iwmg.eq.1)then
            GRSPC(1)=0.0; GRSPC(2)=0.0; GRSPC(3)=0.0; xbmgrid=.false.
          elseif(iwmg.eq.2)then
            GRSPC(1)=2.0; GRSPC(2)=2.0; GRSPC(3)=2.0; xbmgrid=.true.
          elseif(iwmg.eq.3)then
            GRSPC(1)=1.0; GRSPC(2)=1.0; GRSPC(3)=1.0; xbmgrid=.true.
          elseif(iwmg.eq.4)then
            GRSPC(1)=0.5; GRSPC(2)=0.5; GRSPC(3)=0.5; xbmgrid=.true.
          elseif(iwmg.eq.5)then
            GRSPC(1)=0.25; GRSPC(2)=0.25; GRSPC(3)=0.25; xbmgrid=.true.
          elseif(iwmg.eq.6)then
            GRSPC(1)=0.1; GRSPC(2)=0.1; GRSPC(3)=0.1; xbmgrid=.true.
          elseif(iwmg.eq.7)then
            ZDI=0.1
            CALL EASKR(ZDI,' ',
     &       'Distance between grid points (0.1 - 1.0m)?',
     &        0.100,'W',1.0,'W',1.0,'grid point distance',IER,nbhelp)
            GRSPC(1)=ZDI; GRSPC(2)=ZDI; GRSPC(3)=ZDI
            other=ZDI
            xbmgrid=.true.
          endif
          completed=.true.
          inpm=inpmode
          lastwalllist=nw
          call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,inpm,
     &      iwmg,factor,viewlim,nw,ixtn1,ixtn2,iytn1,iytn2,
     &      ixo,iyo,completed,other,lastwalllist)
        endif
        goto 142
      elseif(INO.EQ.6+m+n)then

C Snap-to options. Check if user wants snap-to grid.
        if(.NOT.xbmorigin)then
          call usrmsg('Please define origin first...',' ','W')
          goto 142
        endif
        if(.NOT.xbmscale)then
          call usrmsg('Please define scaling factors first...',' ','W')
          goto 142
        endif
        if(.NOT.xbmgrid)then
          call usrmsg('Please define gridding first...',' ','W')
          goto 142
        endif
        if(xbmsnap)then
          xbmsnap = .false.
        else
          xbmsnap = .true.
        endif

      elseif(INO.EQ.8+m+n)then

C Toggle the input mode. Ask user if they want to do this.
        if(.NOT.xbmorigin)then
          call usrmsg('Please define origin first...',' ','W')
          inpmode = 8
          goto 142
        endif
        if(.NOT.xbmscale)then
          call usrmsg('Please define scaling factors first...',' ','W')
          inpmode = 8
          goto 142
        endif
        helptopic='click_on_input_modes'
        call gethelptext(helpinsub,helptopic,nbhelp)
        inpt=inpmode
        if(inpt.eq.1) msg='Options (currently vertices @ one Z)'
        if(inpt.eq.2) msg='Options (currently vertices @ various Z)'
        if(inpt.eq.3) msg='Options (currently floor plan extrusion)'
        if(inpt.eq.4) msg='Options (currently south elevation)'
        if(inpt.eq.5) msg='Options (currently east elevation)'
        if(inpt.eq.6) msg='Options (currently ground topology)'
        if(inpt.eq.7) msg='Options (future facility)'
        if(inpt.eq.8) msg='Options (currently awaiting data)'
        ilno=inpmode
        idno=3
        call MENUATOL(msg,' Coordinate input options',
     &    'a vertices @ one Z','b vertices @ various Z',
     &    'c floor plan extrusion','d south elevation',
     &    'e east elevation','f ground topology',
     &    'g N/A (future option)','h awaiting data',' ',' ',' ',
     &    ' ',inpt,idno,nbhelp)

C If changing the input mode confirm with user what to do with prior
C data (if moving from vertices to floor plan extrusion or from floor
C plan extrusion to vertices). Note: before clickonbitmap was called
C to create a zone the user will have nominated an initial zone name
C and description and so the user is not prompted for this. Obstructions
C and ground topology do require intial information.
        if(inpt.ne.inpmode)then
          if(ilno.eq.1.or.ilno.eq.2.or.ilno.eq.3)then

C User was working on zone data. Switching to another mode we
C need to start new stuff via the add another facility in the
C main menu.
            CALL EMKCFG('-',IER)  ! update cfg file first
            justentered=.false.
          else
            justentered=.true.  ! no need to show add-another
          endif
          if(nipwhis.gt.0)then
            dok=.false.
            CALL ASKOK(
     &        'Clear history of floor plans before defining new',
     &        'vertices? (previous zones will not show on bitmap)',
     &        OK,dok,nbhelp)
              if(OK)nipwhis=0
          endif

C << is this a place to clear the clicklist arrays? >>
          if(inpt.eq.1)then
            INVERT=0; inpmode = inpt
          elseif(inpt.eq.2)then
            INVERT=0; inpmode = inpt
          elseif(inpt.eq.3)then
            INVERT=0; NW=0; inpmode = inpt
C << TODO clear doors and windows as well >>
          elseif(inpt.eq.4)then
            INVERT=0; inpmode = inpt
          elseif(inpt.eq.5)then
            INVERT=0; inpmode = inpt
          elseif(inpt.eq.6)then
            NW=0; INVERT=0; NGT=0; NGTV=0
            if(gtgeom(1:4).eq.'UNKN'.or.gtgeom(1:2).eq.'  ')then
              write(ltmp,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.grnd'
            else
              ltmp=gtgeom
            endif
 46         CALL EASKS(ltmp,' Ground topology/geometry file name ? ',
     &        ' ',72,'grnd.geo','ground geom file',IER,nbhelp)
            if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
              gtgeom=ltmp
            else
              goto 46
            endif
            GTNAME='from_bitmap'
  39        t15=GTNAME
            CALL EASKS(t15,'Short <12 char description of the ground?',
     &        ' ',15,'flat','not flat',IER,nbhelp)
            if(t15(1:2).ne.'  '.and.t15(1:4).ne.'UNKN')then
              GTNAME=t15
            else
              goto 39
            endif
            inpmode = inpt
          elseif(inpt.eq.7)then
            continue
          elseif(inpt.eq.8)then
            continue
          endif
        endif
        goto 142

      elseif(INO.EQ.9+m+n)then

        if(inpmode.eq.1)then

C Z coordinate to be associated with all points.
          Z1=0.
          CALL EASKR(Z1,' ',
     &      'Elevation (m) of vertices to be selected?',
     &      0.000,'W',99.9,'W',0.0,'elevation of verts',IER,nbhelp)
        elseif(inpmode.eq.3)then

C Floor height.  User begins with an extruded shape.
          Z1=0.
          CALL EASKR(Z1,' ','Elevation (m) of the base surface? ',
     &      0.000,'W',99.9,'W',0.0,'elevation of base',IER,nbhelp)
        elseif(inpmode.eq.4)then

C Y coordinate to be associated with all points.
          Y1=0.
          CALL EASKR(Y1,' ','Y value for south elevation?',
     &      -99.000,'W',99.9,'W',0.0,'Y value for elevation',
     &      IER,nbhelp)
        elseif(inpmode.eq.5)then

C X coordinate to be associated with all points.
          X1=0.
          CALL EASKR(X1,' ','X value for east elevation?',
     &      -99.000,'W',99.9,'W',0.0,'X value for elevation',
     &      IER,nbhelp)
        elseif(inpmode.eq.7)then
          continue
        endif
        goto 142

      elseif(INO.EQ.10+m+n)then
        if(inpmode.eq.3)then

C Ceiling height.
          Z2=2.4
          CALL EASKR(Z2,' ','Elevation (m) of the top surface?',
     &      0.000,'W',99.9,'W',2.7,'elevation of top',IER,nbhelp)
        elseif(inpmode.eq.7)then
          continue
        elseif(inpmode.eq.8)then
          continue
        endif
        goto 142

      elseif(INO.EQ.11+m+n)then
        if(inpmode.eq.3)then

C Door head height.
          Z3=2.1
          CALL EASKR(Z3,' ','Elevation (m) at door head?',
     &      0.000,'W',99.9,'W',2.7,'door head',IER,nbhelp)
        endif
        goto 142
      elseif(INO.EQ.12+m+n)then
        if(inpmode.eq.3)then

C Window sill & head height.
          Z4=0.9
          CALL EASKR(Z4,' ','Elevation (m) at window sill?',
     &      0.000,'W',99.9,'W',2.7,'door head',IER,nbhelp)
          Z5=2.1
          CALL EASKR(Z5,' ','Elevation (m) at window head?',
     &      0.000,'W',99.9,'W',2.7,'door head',IER,nbhelp)
        endif
        goto 142

      elseif(INO.EQ.14+m+n)then

C First check to see if there is a non-blank ZN.
        if(ZN(1:2).eq.'  ')then
          dok=.false.
          helptopic='click_option_mistake'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call askok(
     &      'No zone name found. Are you sure you want to proceed?',
     &      '(you might not have picked `add another` first)',
     &      ok,dok,nbhelp)
          if(.NOT.ok) goto 142
        endif

C *******************************************
C Start identifying vertices for the input modes which gather
C points at fixed Z or ground vertices.
        if(inpmode.eq.1.or.inpmode.eq.2.or.inpmode.eq.4.or.
     &     inpmode.eq.5.or.inpmode.eq.6)then

C Adding vertices to the zone, if colour monitor use green colour.
          call edisp(iuout,
     &      'Select vertices via cursor...type `e` to finish.')
          call edisp(iuout,'type `d` to delete the latest vertex.')
          if(xbmgrid)then
            if(xbmsnap)then
              call edisp(iuout,'type `s` to turn off snap-to grid.')
            else
              call edisp(iuout,'type `s` to turn on snap-to grid.')
            endif
          endif
          if(trimmed)call edisp(iuout,'and type `p` to pan bitmap.')
 146      if(inpmode.eq.1.or.inpmode.eq.2.or.inpmode.eq.6)then

C Get next user action for common Z different Z or ground topology
C (e.g. if user had been panning or some other task).
            CALL trackview(iik,iixx,iiyy)
            ixx=int(iixx); iyy=int(iiyy)
          elseif(inpmode.eq.4)then

C Get next user action for south elevation.
            CALL trackview(iik,iixx,iizz)
            ixx=int(iixx); izz=int(iizz)
          elseif(inpmode.eq.5)then

C Get next user action for east elevation.
            CALL trackview(iik,iiyy,iizz)
            iyy=int(iiyy); izz=int(iizz)
          endif

C If user typed `e` then complete the process.
          if(iik.eq.69.or.iik.eq.101)then
            goto 147
          endif
  
C Toggle on/off the snap-to grid.
          if(iik.eq.83.or.iik.eq.115)then
            if(xbmgrid)then
              if(xbmsnap)then
                xbmsnap=.false.
                call edisp(iuout,'snap-to grid is off...')
              else
                xbmsnap=.true.
                call edisp(iuout,'snap-to grid is on...')
              endif
            else
              call edisp(iuout,'no grid so ingnore `s` keypress...')
            endif
            goto 146
          endif

C If user typed `p` or `P` then do panning until user says done and then continue.
          if(iik.eq.80.or.iik.eq.112)then
            if(trimmed)then
              helptopic='click_pan_options'
              call gethelptext(helpinsub,helptopic,nbhelp)
 148          write(msg,'(a,i4,a,i4,a)') 
     &          'Panning options: (current offset x',ixoffset,' y ',
     &          iyoffset,' pixels)'
              IWM=1
              CALL EASKATOG(msg,' ','done','left','right','up','down',
     &          ' ',' ',IWM,nbhelp)
              if(iwm.eq.1.or.iwm.gt.5)then
                goto 146  ! check for more user input
              else
                inpm=inpmode
                call ctlbmpan(iwm,fname,ixoffset,iyoffset,ibmwidth,
     &            ibmhight,iwidth,ihight,inpm,
     &            nw,ixtn1,ixtn2,iytn1,
     &            iytn2,ixo,iyo)

C Refresh current bitmap and overlayed information.
                completed=.false.
                inpm=inpmode; lastwalllist=nw
                call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,
     &            inpm,iwmg,factor,viewlim,nw,ixtn1,ixtn2,iytn1,iytn2,
     &            ixo,iyo,completed,other,lastwalllist)
              endif
              goto 148
            endif
          endif

C If user typed `d` or `D` then delete the latest coordinate. Then
C refresh current bitmap and overlayed information.

C << TODO add check to see if door or window clicking enabled >>.

          if(iik.eq.67.or.iik.eq.100)then
            if(INVERT.gt.1)then
              INVERT=INVERT-1
              call edisp(iuout,'Last vertex deleted. Refreshing...')
              completed=.false.
              inpm=inpmode; lastwalllist=nw
              call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,
     &          inpm,iwmg,factor,viewlim,nw,ixtn1,ixtn2,iytn1,iytn2,
     &          ixo,iyo,completed,other,lastwalllist)
              goto 146
            endif
          endif

C If snap-to is true then find nearest user unit and cast back to pixels.
          if(inpmode.eq.1.or.inpmode.eq.2.or.inpmode.eq.6)then
            tpx=float(ixx-ixo)/factor; tpy=float(iyo-iyy)/factor
          elseif(inpmode.eq.4)then
            tpx=float(ixx-iixo)/factor; tpy=float(iyo-izz)/factor
          elseif(inpmode.eq.5)then
            tpx=float(iyy-iixo)/factor; tpy=float(iyo-izz)/factor
          endif

C If snap-to is true then find nearest user unit and cast back to pixels.
          if(xbmsnap)then
            REMX=AMOD(tpx,GRSPC(1))
            REMY=AMOD(tpy,GRSPC(2))
            IF(ABS(REMX).GT.GRSPC(1)/2.)THEN
              if(tpx.gt.0.0)then
                tpx=tpx+(GRSPC(1)-REMX)
              elseif(tpx.lt.0.0)then
                tpx=tpx-(GRSPC(1)-ABS(REMX))
              else
                tpx=tpx+(GRSPC(1)-REMX)
              endif
            ELSE
              tpx=tpx-REMX
            ENDIF
            IF(ABS(REMY).GT.GRSPC(2)/2.)THEN
              if(tpy.gt.0.0)then
                tpy=tpy+(GRSPC(2)-REMY)
              elseif(tpy.lt.0.0)then
                tpy=tpy-(GRSPC(2)-ABS(REMY))
              else
                tpy=tpy+(GRSPC(2)-ABS(REMY))
              endif
            ELSE
              tpy=tpy-REMY
            ENDIF
            if(inpmode.eq.1.or.inpmode.eq.2.or.inpmode.eq.6)then
              ixx = int(tpx*factor) + ixo; iyy = iyo - int(tpy*factor)
            elseif(inpmode.eq.4)then
              ixx = int(tpx*factor) + ixo; izz = iyo - int(tpy*factor)
            elseif(inpmode.eq.5)then
              iyy = int(tpx*factor) + ixo; izz = iyo - int(tpy*factor)
            endif
          endif

C Find if this vertex is unique, if it is not then jump back and select
C another pixel. It is only after users type an 'e' that they get asked
C whether they are adding points for doors and/or windows where duplicate
C vertices might be used.
          if(INVERT.gt.1)then
            unique=.true.
            do 76,ij=1,INVERT
              CLOSEX=.false.; CLOSEY=.false.
              if(inpmode.eq.1.or.inpmode.eq.2.or.inpmode.eq.6)then
                CALL ECLOSE(px(ij),tpx,0.03,CLOSEX)
                CALL ECLOSE(py(ij),tpy,0.03,CLOSEY)
              elseif(inpmode.eq.4)then
                CALL ECLOSE(px(ij),tpx,0.03,CLOSEX)
                CALL ECLOSE(pz(ij),tpy,0.03,CLOSEY)
              elseif(inpmode.eq.5)then
                CALL ECLOSE(py(ij),tpx,0.03,CLOSEX)
                CALL ECLOSE(pz(ij),tpy,0.03,CLOSEY)
              endif
              if(CLOSEX.and.CLOSEY)unique=.false.
  76        continue
            if(.NOT.unique)then
              call edisp(iuout,'duplicate vertex...try again.')
              goto 146
            endif
          endif

C Increment number of vertices, add to clicklist arrays, change colour
C to green draw the symbol and index of vertex and do Z editing if requested.
          INVERT=INVERT+1
          if(inpmode.eq.1.or.inpmode.eq.2.or.inpmode.eq.6)then
            ipxx(INVERT)=ixx; ipyy(INVERT)=iyy
            px(INVERT)=tpx; py(INVERT)=tpy; iicol=3
            call winscl('z',iicol)
            iid1=ipxx(INVERT); iid2=ipyy(INVERT);
            call esymbol(iid1,iid2,24,1)
          elseif(inpmode.eq.4)then
            ipxx(INVERT)=ixx; ipzz(INVERT)=izz
            px(INVERT)=tpx; pz(INVERT)=tpy; iicol=3
            call winscl('z',iicol)
            iid1=ipxx(INVERT); iid2=ipzz(INVERT);
            call esymbol(iid1,iid2,24,1)
          elseif(inpmode.eq.5)then
            ipyy(INVERT)=iyy; ipzz(INVERT)=izz
            py(INVERT)=tpx; pz(INVERT)=tpy; iicol=3
            call winscl('z',iicol)
            iid1=ipyy(INVERT); iid2=ipzz(INVERT);
            call esymbol(iid1,iid2,24,1)
          endif

          if(INVERT.gt.0.and.INVERT.le.9)write(temp3,'(i1)') INVERT
          if(INVERT.gt.9.and.INVERT.le.99)write(temp3,'(i2)') INVERT
          if(INVERT.gt.99)write(temp3,'(i3)') INVERT
          if(inpmode.eq.1.or.inpmode.eq.2)then
            call textatxy(ipxx(INVERT)+4,ipyy(INVERT)-1,temp3,'z',3)
            iicol=0
            call winscl('-',iicol)
            X(INVERT)=px(INVERT); Y(INVERT)=py(INVERT)
            if(inpmode.eq.1)then
              Z(INVERT)=Z1
            elseif(inpmode.eq.2)then
              if(INVERT.gt.1)then
                Z3=Z(INVERT-1)
              else
                Z3=0.0
              endif
              CALL EASKR(Z3,' ',' Elevation (Z metres)? ',
     &          0.000,'W',99.9,'W',2.7,'elevation',IER,nbhelp)
              Z(INVERT)=Z3
            endif
            if(INVERT.eq.1)then
              write(outs,'(a,i3,a,f8.3,a,f8.3)') 'Vertex ',INVERT,
     &          ' @ X',px(INVERT),' Y',py(INVERT)
            else
              tdis=crowxyz(px(INVERT-1),py(INVERT-1),Z(INVERT-1),
     &          px(INVERT),py(INVERT),Z(INVERT))
              call ln2az(px(INVERT-1),py(INVERT-1),Z(INVERT-1),
     &          px(INVERT),py(INVERT),Z(INVERT),az,el)
              write(outs,'(a,i3,a,f8.3,a,f8.3,a,f7.3,a,f8.3,a,f7.3)') 
     &          'Vertex ',INVERT,' @ X',px(INVERT),' Y',py(INVERT),
     &          ' distance ',tdis,'m and @ aimuth ',az,' & elev ',el
            endif
            call edisp(iuout,outs)
          elseif(inpmode.eq.4)then
            call textatxy(ipxx(INVERT)+4,ipzz(INVERT)-1,temp3,'z',3)
            iicol=0
            call winscl('-',iicol)
            X(INVERT)=px(INVERT); Z(INVERT)=pz(INVERT)
            Y(INVERT)=Y1
            if(INVERT.eq.1)then
              write(outs,'(a,i3,a,f8.3,a,f8.3)') 'Vertex ',INVERT,
     &          ' @ X',px(INVERT),' Z',pz(INVERT)
            else
              tdis=crowxyz(px(INVERT-1),y(INVERT-1),pz(INVERT-1),
     &          px(INVERT),y(INVERT),pz(INVERT))
              write(outs,'(a,i3,a,f8.3,a,f8.3,a,f7.3)') 
     &          'Vertex ',INVERT,' @ X',px(INVERT),' Z',pz(INVERT),
     &          ' distance ',tdis
            endif
            call edisp(iuout,outs)
          elseif(inpmode.eq.5)then
            call textatxy(ipyy(INVERT)+4,ipzz(INVERT)-1,temp3,'z',3)
            iicol=0
            call winscl('-',iicol)
            Y(INVERT)=py(INVERT); Z(INVERT)=pz(INVERT)
            X(INVERT)=X1
            if(INVERT.eq.1)then
              write(outs,'(a,i3,a,f8.3,a,f8.3)') 'Vertex ',INVERT,
     &          ' @ Y',py(INVERT),' Z',pz(INVERT)
            else
              tdis=crowxyz(x(INVERT-1),py(INVERT-1),pz(INVERT-1),
     &          x(INVERT),py(INVERT),pz(INVERT))
              write(outs,'(a,i3,a,f8.3,a,f8.3,a,f7.3)') 
     &          'Vertex ',INVERT,' @ Y',py(INVERT),' Z',pz(INVERT),
     &          ' distance ',tdis
            endif
            call edisp(iuout,outs)
          elseif(inpmode.eq.6)then

C Add to ground topology and confirm Z.
            call textatxy(ipxx(INVERT)+4,ipyy(INVERT)-1,temp3,'z',3)
            iicol=0
            call winscl('-',iicol)
            XGT(INVERT)=px(INVERT); YGT(INVERT)=py(INVERT)
            if(INVERT.gt.1)then
              Z3=ZGT(INVERT-1)
            else
              Z3=0.0
            endif
            CALL EASKR(Z3,' ',' Elevation (Z metres)? ',
     &        -99.000,'W',99.9,'W',0.0,'elevation',IER,nbhelp)
            ZGT(INVERT)=Z3
            if(INVERT.eq.1)then
              write(outs,'(a,i3,a,f8.3,a,f8.3)') 'Vertex ',INVERT,
     &          ' @ X',px(INVERT),' Y',py(INVERT)
            else
              tdis=crowxyz(px(INVERT-1),py(INVERT-1),Z(INVERT-1),
     &          px(INVERT),py(INVERT),Z(INVERT))
              call ln2az(px(INVERT-1),py(INVERT-1),Z(INVERT-1),
     &          px(INVERT),py(INVERT),Z(INVERT),az,el)
              write(outs,'(a,i3,a,f8.3,a,f8.3,a,f7.3,a,f8.3,a,f7.3)') 
     &          'Vertex ',INVERT,' @ X',px(INVERT),' Y',py(INVERT),
     &          ' distance ',tdis,'m and @ aimuth ',az,' & elev ',el
            endif
            call edisp(iuout,outs)
          endif
          goto 146  ! loop back for more user input

C *********************************
C Selection terminated with `e`, ask user if they want to save the
C information. If so fill zone common blocks and make a
C dummy surface with the first three vertices.
 147      continue
          helptopic='user_typed_e'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call easkabc('Options:',' ','clear points & redefine zone',
     &      'clear points & continue','save zone data',izclear,nbhelp)
          if(izclear.eq.1.or.izclear.eq.2)then
            if(inpmode.eq.1.or.inpmode.eq.2.or.inpmode.eq.4.or.
     &         inpmode.eq.5)then
              INVERT=0
            endif
            if(izclear.eq.2)then
              goto 142
            elseif(izclear.eq.1)then

C Refresh the bitmap.
              completed=.true.
              inpm=inpmode; lastwalllist=nw
              call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,inpm,
     &         iwmg,factor,viewlim,nw,ixtn1,ixtn2,iytn1,iytn2,
     &         ixo,iyo,completed,other,lastwalllist)

C Reset to enter the clicking.
              call edisp(iuout,'BEGIN RE-ENTERING POINTS NOW...')
              INO=14+m+n
              goto 29
            endif
          elseif(izclear.eq.3)then

C If adding another then use the previously defined names and
C instanciate the common blocks.
            if(addinganother)then
              icomp=incomp
              call st2name(ZN,zname(ICOMP))
              lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update the length of this string.
              zdesc(ICOMP)=zd
              lnzdesc(ICOMP)=lnblnk(zd)  ! update the length of this string.
              LGEOM(ICOMP)=DFILE
              LTHRM(ICOMP)=CFILE
              LPROJ(ICOMP)=OFILE
            endif
            iicol=0
            call winscl('-',iicol)
            call edisp(iuout,'Instanciating new objects...')
            if(inpmode.eq.6)then

C Create single ground surface.
              NGTV=INVERT; NGT=1; NGVER(1)=3
              JGVN(1,1)=1; JGVN(1,2)=2; JGVN(1,3)=3
              GSNAME(1)='first'
              GMLCN(1)='UNKN'
            else

C Create a single surface out of first 3 vertices so that a
C zone geometry file can be created. 
              NTV=INVERT; NSUR=1; NZSUR(ICOMP)=NSUR
              NVER(1)=3
              JVN(1,1)=1; JVN(1,2)=2; JVN(1,3)=3
              NZTV(icomp)=NTV
              SNAME(ICOMP,1)='first'
              SOTF(1)='OPAQUE'; SMLCN(1)='UNKN'; SVFC(1)='FLOR'
              SOTHER(1,1)='UNKNOWN'; SOTHER(1,2)='0'; SOTHER(1,3)='0'
              SUSE(1,1)='-'; SUSE(1,2)='-'; SPARENT(1)='-'

C Update the connection list for an initial surface.
              ICCC=NCON
              ICCC=ICCC+1
              IC1(ICCC)=ICOMP; IE1(ICCC)=1; ICT(ICCC)=0
              IC2(ICCC)=0; IE2(ICCC)=0
              SSNAME(iccc)=SNAME(ICOMP,1)
              SSOTF(iccc)=SOTF(1)
              SSMLCN(iccc)=SMLCN(1)
              ssmlcindex(iccc)=0  ! no matching MLC
              SSVFC(iccc)=SVFC(1)
              SSOTHER(iccc,1)=SOTHER(1,1)
              SSOTHER(iccc,2)=SOTHER(1,2)
              SSOTHER(iccc,3)=SOTHER(1,3)
              SSUSE(iccc,1)=SUSE(1,1)
              SSUSE(iccc,2)=SUSE(1,2)
              SSPARENT(iccc)=SPARENT(1)
              IZSTOCN(icomp,1)=iccc
              NCON=ICCC

C Complete zone data before exiting use a similar pattern to that
C of the floor plan extrusion block of code below.
              IF(zname(ICOMP)(1:2).EQ.'  ')THEN
               IF(ICOMP.LE.9)WRITE(zname(ICOMP),'(A5,I1)')'Zone-',ICOMP
               IF(ICOMP.GT.9)WRITE(zname(ICOMP),'(A5,I2)')'Zone-',ICOMP
               IF(ICOMP.GT.99)WRITE(zname(ICOMP),'(A5,I3)')'Zone-',ICOMP
               lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update length of this string.
              ENDIF
              if(zdesc(ICOMP)(1:2).EQ.'  ')then
               write(zdesc(ICOMP),'(2a)') 
     &           zname(ICOMP)(1:lnzname(ICOMP)),' describes a...'
               lnzdesc(ICOMP)=lnblnk(zdesc(ICOMP))  ! update the length of this string.
              endif

C Set type type and version based on current value of igupgrade.
C << convert to always using V 1.1 >>
              CTYPE(icomp)='GEN '
              gversion(icomp) =1.1
              NDP(ICOMP)=3
              IDPN(ICOMP,1)=0; IDPN(ICOMP,2)=0; IDPN(ICOMP,3)=0

C Update the G7 common blocks and then assign ZBASEA.
              call zgupdate(1,ICOMP,ier)
              IUZBASEA(icomp)=0
              IBASES(ICOMP,1)=NSUR
              IZBASELIST(ICOMP)=1
              ibcs=IZSTOCN(icomp,NSUR)
              ZBASEA(icomp)= SSNA(ibcs)

C Update the global coordinates for this zones surfaces so that
C the subsequent wireframe image can be drawn and the bounds of
C the zone can be calculated.
              DO 41 J=1,NZTV(ICOMP)
                VCOORD(ICOMP,J,1)=X(J)
                VCOORD(ICOMP,J,2)=Y(J)
                VCOORD(ICOMP,J,3)=Z(J)
   41         CONTINUE
 
C Save this to file before passing into the geometry editing facility.
              write(outs,'(3a)') 'Updating ',
     &          zdesc(ICOMP)(1:lnzdesc(ICOMP)),'...'
              call edisp(iuout,outs)
              gversion(icomp) =1.1
              call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
              IF(IER.EQ.1)THEN
               CALL usrmsg(' ','Problem creating geometry file...','W')
              ENDIF

              write(outs,'(3a)') 'Updating ',
     &          zdesc(ICOMP)(1:lnzdesc(ICOMP)),'...done.'
              call edisp(iuout,outs)

C Read in control file if defined, setup additional file names and then
C write out the ctl file with additional zone lined to no control and
C lastly, update the configuration file.
              OKC=.false.
              if(LCTLF(1:2).ne.'  '.and.LCTLF(1:4).ne.'UNKN')OKC=.true.
              if(OKC)then
                ICTLF=IFIL+1
                CALL ERPFREE(ICTLF,ISTAT)
                call FINDFIL(LCTLF,XST)
                IF(XST) CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
              endif

              NCOMP=NCOMP+1
              NCCODE(ICOMP)=NCOMP
              if(OKC)then
                icascf(NCOMP)=0
                call usrmsg(' updating control for additional zone...',
     &            ' ','P')
                call CTLWRT(ICTLF,IER)
                call usrmsg(' ',' ','-')
              endif
              CALL EMKCFG('-',IER)
              call usrmsg(' updating model for additional zone...done.',
     &          ' ','P')

C Save the extrusion points to the history list. Set iphisznst
C to the first point in this zone and iphisznfn to the closing
C point of the zone.
              if(NW.gt.0)then
                do 218 j=1,NW
                  if(nipwhis+1.le.MGTV)then
                    nipwhis=nipwhis+1
                    if(j.eq.1)iphisznst(icomp)=nipwhis
                    ipxwhis(nipwhis)=ipxbase(j)
                    ipywhis(nipwhis)=ipybase(j)
                  endif
  218           continue
                if(nipwhis+1.le.MGTV)then
                  nipwhis=nipwhis+1
                  ipxwhis(nipwhis)=ipxbase(1)
                  ipywhis(nipwhis)=ipybase(1)
                endif
                iphisznfn(icomp)=nipwhis
              endif

C << to here >>
            endif
            justentered=.false.
          endif

        elseif(inpmode.eq.3)then

C ******************************
C FLOOR PLAN extrusion. click on corners until list terminated, allowing
C bitmap to be panned if necessary during the process.
          call edisp(iuout,
     & 'Input points around the base ANTI-clockwise looking from the')
          call edisp(iuout,
     & 'top, preferably start near the lower left corner and...')
          call edisp(iuout,
     & 'type `d` to delete last point, `e` to finish. If you mess')
          call edisp(iuout,
     & 'up type `e` and then select option to re-start the zone.')
          if(xbmgrid)then
            if(xbmsnap)then
              call edisp(iuout,'type `s` to turn off snap-to grid.')
            else
              call edisp(iuout,'type `s` to turn on snap-to grid.')
            endif
          endif
          if(trimmed)call edisp(iuout,
     &      'type `p` to pan bitmap (if a large zone).')
          NW=0  ! reset number of corners to zero
 246      CALL trackview(iik,iixx,iiyy)
          ixx=int(iixx); iyy=int(iiyy)

C If user typed `e` or `E` then ask if doors and/or window points
C should be added.
          if(iik.eq.69.or.iik.eq.101)then

C Z1 base, Z2 ceiling, Z3 door head, Z4 window sill, Z5 window head.
C Report of lines & vertices near Z1.

C Keep track of initial base points for the first 'e' click.
C Save the initial pxbase() pybase() arrays for use in establishing
C parent surface of windows and doors.
            if(.NOT.firsteclick)then
              firsteclick=.true.
              nbbase=NW
              do loop=1,nbbase
                pxbase(loop)=pxbase(loop)
                pybase(loop)=pybase(loop)
              enddo
              write(louts,'(a,124f6.2)') 'X base points: ',
     &          (pxbase(j),J=1,nbbase)
              CALL EDISP248(iuout,louts,100)
              write(louts,'(a,124f6.2)') 'Y base points: ',
     &          (pybase(j),J=1,nbbase)
              CALL EDISP248(iuout,louts,100)
            else

C Subsequent [e] so check if we were working with doors and
C if so whether nbofdoorpts is even or odd. If odd remove
C one so there are proper pairs of jambs. We do not know
C which one of the master list was associated with that
C door though.
              if(dodoor)then
                im=MOD(nbofdoorpts,2)
                odd=.false.
                if(im.eq.1) odd=.true.
                if(odd)then
                  nbofdoorpts=nbofdoorpts-1
                endif
              endif

C Subsequent [e] so check if we were working with windows and
C if so whether nbofwindowpts is even or odd. We do not know
C which one of the master list was associated with that
C window though.
              if(dowindow)then
                im=MOD(nbofwindowpts,2)
                odd=.false.
                if(im.eq.1) odd=.true.
                if(odd)then
                  nbofwindowpts=nbofwindowpts-1
                endif
              endif
            endif

C Offer user options after an [e] click.
            call easkatog(
     &      'Options (if adding both door & windows do doors first):',
     &        ' ','door jambs','glazing jambs','furniture origins',
     &        'obstructions','continue',' ',' ',izdoor,nbhelp)

            if(izdoor.eq.1)then
              dodoor=.true.; nbofdoorpts=0
              lastwalllist=NW  ! remember last extrusion corner
              call edisp(iuout,
     &        'Click new points for door jambs (up to 20 doors).')
              call edisp(iuout,
     &        'Proceed anti-clockwise, `e` to end. Later you can also')
              call edisp(iuout,'add up to 20 windows via clicks.')
              goto 246  ! ask for another point
            elseif(izdoor.eq.2)then
              dowindow=.true.; nbofwindowpts=0
              if(dodoor)then
                continue  ! previously did doors so already know lastwalllist
              else
                lastwalllist=NW  ! remember last extrusion corner
              endif
              call edisp(iuout,
     &          'Click new points for window jambs (up to 20 windows).')
              call edisp(iuout,
     &          'Proceed anti-clockwise, `e` to end. You can then add')
              call edisp(iuout,'add solar obstructions if you want.')
              goto 246  ! ask for another point
            elseif(izdoor.eq.3)then
              call edisp(iuout,
     &        'Select origin points for furniture (with Z value)')
              dofurn=.true. ! set true & keep true until after doors & win processed
              Z8=Z1+0.01    ! initial assumption just off of floor
              call clickfurn(icomp,fname,ixoffset,iyoffset,
     &          ibmwidth,ibmhight,iwidth,ihight,inpm,iwmg,factor,
     &          viewlim,nw,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo,completed,
     &          other,lastwalllist,xbmsnap,GRSPC,ier)

C Offer to start another object.
              call easkab('Options:',' ',
     &          'add obstructions','continue',iopt,nbhelp)
              if(iopt.eq.2)then
                goto 247  ! process
              elseif(iopt.eq.1)then

C Use logic for creating obstructions (duplicate of izdoor.eq.4)
                call edisp(iuout,
     &        'Select three corners (origin, end of front edge & back')
                call edisp(iuout,
     &        'corner for solar obstructions...type `e` to finish.')
                if(xbmgrid)then
                  if(xbmsnap)then
                    call edisp(iuout,
     &   'type `s` to turn off snap-to grid (before starting block).')
                  else
                    call edisp(iuout,
     &   'type `s` to turn on snap-to grid (before starting block).')
                  endif
                endif
                if(trimmed)then
                  call edisp(iuout,
     &           'And type `p` to pan bitmap (before starting block).')
                endif

C Use clickobstructions to gather locations & form of obstructions.
                call clickobstructions(icomp,fname,ixoffset,iyoffset,
     &            iwidth,ihight,inpm,iwmg,factor,viewlim,nw,ixtn1,ixtn2,
     &            iytn1,iytn2,ixo,iyo,completed,other,
     &            lastwalllist,xbmsnap,GRSPC,NBFREL,ier)
                goto 247  ! process
              endif
            elseif(izdoor.eq.4)then
              call edisp(iuout,
     &        'Select three corners (origin, end of front edge & back')
              call edisp(iuout,
     &        'corner for solar obstructions...type `e` to finish.')
              if(xbmgrid)then
                if(xbmsnap)then
                 call edisp(iuout,
     &   'type `s` to turn off snap-to grid (before starting block).')
                else
              call edisp(iuout,
     &   'type `s` to turn on snap-to grid (before starting block).')
                endif
              endif
              if(trimmed)then
                call edisp(iuout,
     &        'And type `p` to pan bitmap (before starting block).')
              endif

C Use clickobstructions to gather locations and form of
C obstructions.
              call  clickobstructions(icomp,fname,ixoffset,iyoffset,
     &          iwidth,ihight,inpm,iwmg,factor,viewlim,nw,ixtn1,ixtn2,
     &          iytn1,iytn2,ixo,iyo,completed,other,
     &          lastwalllist,xbmsnap,GRSPC,NBFREL,ier)
              goto 247  ! process
            else

C If user did not add any window or door points then set lastwalllist
              if(.NOT.dodoor.and..NOT.dowindow)then
                lastwalllist=NW  ! remember last extrusion corner
              endif      
              goto 247  ! process
            endif
          endif

C Toggle on/off the snap-to grid.
          if(iik.eq.83.or.iik.eq.115)then
            if(xbmgrid)then
              if(xbmsnap)then
                xbmsnap=.false.
                call edisp(iuout,'snap-to grid is off...')
              else
                xbmsnap=.true.
                call edisp(iuout,'snap-to grid is on...')
              endif
            else
              call edisp(iuout,'no grid so ingnore `s` keypress...')
            endif
            goto 246
          endif

C If user typed `p` or `P` then allow for panning and then continue.
          if(iik.eq.80.or.iik.eq.112)then
            if(trimmed)then
              helptopic='click_pan_options'
              call gethelptext(helpinsub,helptopic,nbhelp)
              write(msg,'(a,i4,a,i4,a)') 
     &          'Panning options: (current offset x',ixoffset,' y ',
     &          iyoffset,' pixels)'
              IWM=1
  248          CALL EASKATOG(msg,' ','done','left','right','up','down',
     &          ' ',' ',IWM,2)
              if(iwm.gt.1.and.iwm.le.5)then
                inpm=inpmode
                call ctlbmpan(iwm,fname,ixoffset,iyoffset,ibmwidth,
     &            ibmhight,iwidth,ihight,inpm,
     &            nw,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo)

C Refresh current bitmap and overlayed information.
                completed=.false.
                inpm=inpmode
                call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,
     &            inpm,iwmg,factor,viewlim,nw,ixtn1,ixtn2,iytn1,iytn2,
     &            ixo,iyo,completed,other,lastwalllist)
                goto 248  ! any more panning?
              else
                goto 246  ! get next user action
              endif
            endif
          endif

C If user typed `d` or `D` then delete the previous coordinate.
          if(iik.eq.67.or.iik.eq.100)then
            if(NW.gt.1)then

C If working with doors then decrement nbofdoorpts.
              if(.NOT.dowindow.and.dodoor)then
                INVERT=INVERT-1
                nbofdoorpts=nbofdoorpts-1
              endif

C If working with windows then decrement nbofwindowpts.
              if(dowindow)then
                INVERT=INVERT-1
                nbofwindowpts=nbofwindowpts-1
              endif

C If we are not focused on windows and doors then delete
C a base edge.
              if(.NOT.dowindow.and..NOT.dodoor)then
                NW=NW-1
                call edisp(iuout,'Last corner deleted. Refreshing...')
                completed=.false.
              endif

C Refresh the display.
              inpm=inpmode
              call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,
     &          inpm,iwmg,factor,viewlim,nw,ixtn1,ixtn2,iytn1,iytn2,
     &          ixo,iyo,completed,other,lastwalllist)
              goto 246
            endif
          endif

C Check if the new point is close to one of the historical points.
          if(nipwhis.gt.0)then
            do 317 j=1,nipwhis
              idx=iabs(ipxwhis(j)-ixx)
              idy=iabs(ipywhis(j)-iyy)
              if(idx.le.4.and.idy.le.4)then
                write(outs,'(a,i3,a,i3,a,i3,a,2i4,a)') 
     &            'Point @ pixel X',ixx,' Y',iyy,
     &            ' is close to point (',j,')',ipxwhis(j),
     &            ipywhis(j),'. That data'
                call edisp(iuout,outs)
                call edisp(iuout,
     &            'will be used UNLESS you TYPE `d` now.') 
                ixx=ipxwhis(j)
                iyy=ipywhis(j)
                goto 318
              endif
  317       continue
          endif

C If snap-to is true then find nearest user unit and cast back to pixels.
  318     continue
          tpxw=float(ixx-ixo)/factor; tpyw=float(iyo-iyy)/factor
          if(xbmsnap)then
            REMX=AMOD(tpxw,GRSPC(1)); REMY=AMOD(tpyw,GRSPC(2))
            IF(ABS(REMX).GT.GRSPC(1)/2.)THEN
              if(tpxw.gt.0.0)then
                tpxw=tpxw+(GRSPC(1)-REMX)
              elseif(tpxw.lt.0.0)then
                tpxw=tpxw-(GRSPC(1)-ABS(REMX))
              else
                tpxw=tpxw+(GRSPC(1)-REMX)
              endif
            ELSE
              tpxw=tpxw-REMX
            ENDIF
            IF(ABS(REMY).GT.GRSPC(2)/2.)THEN
              if(tpyw.gt.0.0)then
                tpyw=tpyw+(GRSPC(2)-REMY)
              elseif(tpyw.lt.0.0)then
                tpyw=tpyw-(GRSPC(2)-ABS(REMY))
              else
                tpyw=tpyw+(GRSPC(2)-ABS(REMY))
              endif
            ELSE
             tpyw=tpyw-REMY
            ENDIF
            ixx = int(tpxw*factor) + ixo
            iyy = iyo - int(tpyw*factor)
          endif

C If adding door points if the click matches a previous zone corner
C then ask user to try again. If dowindow also happens to be
C true then skip the door loop.
          if(.NOT.dowindow.and.dodoor)then
            if(NW.gt.1)then
              unique=.true.
              do ij=1,NW
                CLOSEX=.false.; CLOSEY=.false.
                CALL ECLOSE(pxbase(ij),tpxw,0.03,CLOSEX)
                CALL ECLOSE(pybase(ij),tpyw,0.03,CLOSEY)
                if(CLOSEX.and.CLOSEY)then
                  unique=.false.; iv1=ij  ! remember which clicked edge
                endif
              enddo
              if(.NOT.unique)then

C Advise user to clik on another point which is not the same
C as an existing base point.
                call usrmsg('Door jambs should not coincide with an',
     &            'existing floor plan point. Select again.','W')
                goto 246  ! ask for another point
              else

C If we are at limits of door list jump.
                if(nbofdoorpts+1.gt.40)then
                  call usrmsg('Cannot add more door jambs to the zone',
     &              'please type an `e`.','W')
                  goto 246  ! jump for user input
                endif

C We have a unique point for the door.
                nbofdoorpts=nbofdoorpts+1
                wasdoorunique(nbofdoorpts)=.true.
                im=MOD(nbofdoorpts,2)
                odd=.false.
                if(im.eq.1) odd=.true.

C Draw it and index.
                ipxdoor(nbofdoorpts)=ixx; ipydoor(nbofdoorpts)=iyy
                iid1=ixx; iid2=iyy
                pxdoor(nbofdoorpts)=tpxw; pydoor(nbofdoorpts)=tpyw

                call winscl('z',iicol)
                call esymbol(iid1,iid2,24,1)
                if(nbofdoorpts.gt.0.and.nbofdoorpts.le.9)
     &            write(temp3,'(a,i1)') 'd',nbofdoorpts
                if(nbofdoorpts.gt.9.and.nbofdoorpts.le.99)
     &            write(temp3,'(a,i2)') 'd',nbofdoorpts
                call textatxy(ipxdoor(nbofdoorpts)+4,
     &            ipydoor(nbofdoorpts)-1,temp3,'z',3)
                if(odd)then
                  write(outs,'(a,i3,a,f8.3,a,f8.3)') 'L Door ',
     &              nbofdoorpts,' @ X',
     &              pxdoor(nbofdoorpts),' Y',pydoor(nbofdoorpts)
                else
                  write(outs,'(a,i3,a,f8.3,a,f8.3)') 'R Door ',
     &              nbofdoorpts,' @ X',
     &              pxdoor(nbofdoorpts),' Y',pydoor(nbofdoorpts)
                endif
                call edisp(iuout,outs)

C Loop through known base points and align the click point
C with a base line if it is close to it. This ensures that even
C if there is no snap on that we reduce risk of warped surfaces.
                call pointmergewithinclick(lastwalllist,pxbase,pybase,
     &            pxdoor(nbofdoorpts),pydoor(nbofdoorpts),
     &            X1,rx,ry,rz,iv1,iv2,iok)
                if(iok.eq.1)then
                  write(outs,'(a,i3,a,f8.3,a,f8.3,3i3)') 'Aligned Door',
     &              nbofdoorpts,' @ X',rx,' Y',ry,iok,iv1,iv2
                  pxdoor(nbofdoorpts)=rx; pydoor(nbofdoorpts)=ry  ! update
                  ibedgeofdoor(nbofdoorpts)=iv1 ! remember which clicked edge
                else
                  ibedgeofdoor(nbofdoorpts)=iv1 ! remember which clicked edge
                endif
                goto 246
              endif
            endif
          endif

C If adding window jambs if the click matches a previous zone corner
C then ask user to try again. The logic assumes that the user
C selects doors first.
          if(dowindow.and..NOT.dofurn)then
            if(NW.gt.1)then
              unique=.true.
              do ij=1,NW
                CLOSEX=.false.; CLOSEY=.false.
                CALL ECLOSE(pxbase(ij),tpxw,0.03,CLOSEX)
                CALL ECLOSE(pybase(ij),tpyw,0.03,CLOSEY)
                if(CLOSEX.and.CLOSEY)then
                  unique=.false.; iv1=ij  ! remember which clicked edge
                endif
              enddo
              if(.NOT.unique)then

C Advise user to clik on another point which is not the same
C as an existing base point.
                call usrmsg('Window jambs should not coincide with an',
     &            'existing floor plan point. Select again.','W')
                goto 246  ! ask for another point
              else

C If we are at limits of door list jump.
                if(nbofwindowpts+1.gt.40)then
                  call usrmsg('Cannot add more window jambs to zone',
     &              'please type an `e`.','W')
                  goto 246  ! jump for user input
                endif
                nbofwindowpts=nbofwindowpts+1
                waswindowunique(nbofwindowpts)=.false.
                im=MOD(nbofwindowpts,2)
                odd=.false.
                if(im.eq.1) odd=.true.

C Draw it and index.
                ipxwin(nbofwindowpts)=ixx; ipywin(nbofwindowpts)=iyy
                iid1=ixx; iid2=iyy
                pxwin(nbofwindowpts)=tpxw; pywin(nbofwindowpts)=tpyw
                call winscl('z',iicol)
                call esymbol(iid1,iid2,24,1)
                if(nbofwindowpts.gt.0.and.nbofwindowpts.le.9)
     &            write(temp3,'(a,i1)') 'w',nbofwindowpts
                if(nbofwindowpts.gt.9.and.nbofwindowpts.le.99)
     &            write(temp3,'(a,i2)') 'w',nbofwindowpts
                call textatxy(ipxwin(nbofwindowpts)+4,
     &            ipywin(nbofwindowpts)-1,temp3,'z',3)
                write(outs,'(a,i3,a,f8.3,a,f8.3)') 'Window ',
     &            nbofwindowpts,' @ X',
     &            pxwin(nbofwindowpts),' Y',pywin(nbofwindowpts)
                call edisp(iuout,outs)

C Loop through known base points and align the click point
C with a line if it is close to it. This ensures that even
C if there is no snap on that we reduce risk of warped surfaces.
                call pointmergewithinclick(lastwalllist,pxbase,pybase,
     &            pxwin(nbofwindowpts),pywin(nbofwindowpts),
     &            X1,rx,ry,rz,iv1,iv2,iok)
                if(odd)then
                  write(outs,'(a,i3,a,f8.3,a,f8.3,a,2i3)') 
     &              'L Aligned Window',
     &            nbofwindowpts,' @ X',rx,' Y',ry,' between',iv1,iv2
                else
                  write(outs,'(a,i3,a,f8.3,a,f8.3,a,2i3)') 
     &              'R Aligned Window',
     &            nbofwindowpts,' @ X',rx,' Y',ry,' between',iv1,iv2
                endif
                call edisp(iuout,outs)
                pxwin(nbofwindowpts)=rx; pywin(nbofwindowpts)=ry
                ibedgeofwin(nbofwindowpts)=iv1 ! remember which clicked edge
                goto 246
              endif
            endif
          endif

C Find if this corner is unique, if it is not unique then jump back and
C slect another pixel. If adding door or window points it might or 
C might not match a previous zone corner.
          if(NW.gt.1)then
            unique=.true.
            do ij=1,NW
              CLOSEX=.false.; CLOSEY=.false.
              CALL ECLOSE(pxbase(ij),tpxw,0.03,CLOSEX)
              CALL ECLOSE(pybase(ij),tpyw,0.03,CLOSEY)
              if(CLOSEX.and.CLOSEY)unique=.false.
            enddo
            if(.NOT.unique)then
              call edisp(iuout,'duplicate corner...try again.')
              goto 246
            endif
          endif

C Corner added draw it and index and a line from the previous to current.
          NW=NW+1
          ipxbase(NW)=ixx; ipybase(NW)=iyy
          pxbase(NW)=tpxw; pybase(NW)=tpyw; iicol=3
          call winscl('z',iicol)
          iid1=ipxbase(NW); iid2=ipybase(NW);
          call esymbol(iid1,iid2,24,1)
          if(NW.gt.0.and.NW.le.9)write(temp3,'(i1)') NW
          if(NW.gt.9.and.NW.le.99)write(temp3,'(i2)') NW
          if(NW.gt.99)write(temp3,'(i3)') NW
          call textatxy(ipxbase(NW)+4,ipybase(NW)-1,temp3,'z',3)
          if(NW.gt.1)then
            iicol=3
            call winscl('z',iicol)
            iid1=ipxbase(NW-1); iid2=ipybase(NW-1)
            iid3=ipxbase(NW); iid4=ipybase(NW)
            call edwline(iid1,iid2,iid3,iid4)
          endif
          if(NW.eq.1)then
            write(outs,'(a,i3,a,f8.3,a,f8.3)') 'Point ',NW,
     &        ' @ X',pxbase(NW),' Y',pybase(NW)
          else
            tdis=crowxyz(pxbase(NW-1),pybase(NW-1),0.0,
     &           pxbase(NW),pybase(NW),0.0)
            call ln2az(pxbase(NW-1),pybase(NW-1),0.0,
     &        pxbase(NW),pybase(NW),0.0,az,el)
            write(outs,'(a,i3,a,f8.3,a,f8.3,a,f7.3,a,f8.3,a,f7.3)') 
     &        'Point ',NW,' @ X',pxbase(NW),' Y',pybase(NW),' distance',
     &        tdis,'m and @ aimuth ',az,' & elev ',el
          endif
          call edisp(iuout,outs)
          XX(NW)=pxbase(NW); YY(NW)=pybase(NW)
          goto 246

C Selection terminated with `e`, ask user if they want to save the
C information. If so fill zone common blocks. Draw line from last 
C corner to first corner and return forground colour to black.
 247      continue
          iicol=3
          call winscl('z',iicol)
          if(dodoor.or.dowindow.or.dofurn)then
            iid1=ipxbase(lastwalllist); iid2=ipybase(lastwalllist)  ! use lastwalllist
          else
            iid1=ipxbase(NW); iid2=ipybase(NW)  ! std close of boundary lines
          endif
          iid3=ipxbase(1); iid4=ipybase(1)
          call edwline(iid1,iid2,iid3,iid4)
          iicol=0
          call winscl('-',iicol)
          helptopic='user_typed_e'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call easkabc('Options:',' ','clear points & redefine zone',
     &      'clear points & continue','save zone data',izclear,nbhelp)
          if(izclear.eq.1.or.izclear.eq.2)then

C Clear history of extrusion points and number of walls. 
            if(NW.gt.0.and.nipwhis.le.NW)then
              nipwhis=0
            elseif(NW.gt.0.and.nipwhis.gt.NW)then
              if(addinganother)then
                nipwhis=iphisznfn(incomp-1)
              else
                nipwhis=nipwhis - (NW+1)
              endif
            endif
            NW=0
            if(izclear.eq.2)then
              goto 142
            elseif(izclear.eq.1)then

C Refresh the bitmap.
              completed=.true.
              inpm=inpmode
              call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,inpm,
     &         iwmg,factor,viewlim,nw,ixtn1,ixtn2,iytn1,iytn2,
     &         ixo,iyo,completed,other,lastwalllist)

C Reset to enter the clicking.
              call edisp(iuout,'BEGIN RE-ENTERING POINTS NOW...')
              INO=14+m+n
              goto 29
            endif
          elseif(izclear.eq.3)then

C If adding another then use the previously defined names and
C instanciate the common blocks.
            if(addinganother)then
              icomp=incomp
              call st2name(ZN,zname(ICOMP))
              lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update the length of this string.
              zdesc(ICOMP)=zd
              lnzdesc(ICOMP)=lnblnk(zd)  ! update the length of this string.
              LGEOM(ICOMP)=DFILE
              LTHRM(ICOMP)=CFILE
              LPROJ(ICOMP)=OFILE
            endif

C Convert corners into a gen body and update nzsur and nztv
C << could we use pxbase pybase instead of xx yy? >>
C            CALL EREGC(lastwalllist,Z1,Z2,XX,YY)
            CALL EREGC(lastwalllist,Z1,Z2,pxbase,pybase)
            NSUR=lastwalllist+2

C Debug.
            write(6,'(a,6i4)') 'i nw is ',NW,lastwalllist,NSUR,NTV,
     &        nbofdoorpts,nbofwindowpts
            write(6,'(a,42F7.3)') 'xx is ',(pxbase(j),J=1,lastwalllist)
            write(6,'(a,42F7.3)') 'yy is ',(pybase(j),J=1,lastwalllist)

C Set door idjvn logic as a while loop for up to 20 doors.
            if(nbofdoorpts.gt.0)then
              iloc=1
              do while (iloc.lt.nbofdoorpts)
                if(iloc.eq.1)then
                  idoor=1; idstart=NTV+1
                elseif(iloc.eq.3)then
                  idoor=2; idstart=NTV+5
                elseif(iloc.eq.5)then
                  idoor=3; idstart=NTV+9
                elseif(iloc.eq.7)then
                  idoor=4; idstart=NTV+13
                elseif(iloc.eq.9)then
                  idoor=5; idstart=NTV+17
                elseif(iloc.eq.11)then
                  idoor=6; idstart=NTV+21
                elseif(iloc.eq.13)then
                  idoor=7; idstart=NTV+25
                elseif(iloc.eq.15)then
                  idoor=8; idstart=NTV+29
                elseif(iloc.eq.17)then
                  idoor=9; idstart=NTV+33
                elseif(iloc.eq.19)then
                  idoor=10; idstart=NTV+37
                elseif(iloc.eq.21)then
                  idoor=11; idstart=NTV+41
                elseif(iloc.eq.23)then
                  idoor=12; idstart=NTV+45
                elseif(iloc.eq.25)then
                  idoor=13; idstart=NTV+49
                elseif(iloc.eq.27)then
                  idoor=14; idstart=NTV+53
                elseif(iloc.eq.29)then
                  idoor=15; idstart=NTV+57
                elseif(iloc.eq.31)then
                  idoor=16; idstart=NTV+61
                elseif(iloc.eq.33)then
                  idoor=17; idstart=NTV+65
                elseif(iloc.eq.35)then
                  idoor=18; idstart=NTV+69
                elseif(iloc.eq.37)then
                  idoor=19; idstart=NTV+73
                elseif(iloc.eq.39)then
                  idoor=20; idstart=NTV+77
                endif
C                write(6,'(a,i4,a,4i4)') 'Door verts start at',
C     &          idstart,' list',idstart,idstart+2,idstart+3,idstart+1
                idjvn(idoor,1)=idstart; idjvn(idoor,2)=idstart+2
                idjvn(idoor,3)=idstart+3; idjvn(idoor,4)=idstart+1
                iloc=iloc+2
              end do
            endif

C If there were door jamb points add them. Assume
C click was on a unique point so add both head and base. 
            if(nbofdoorpts.gt.0)then
              INVERT=NTV  ! pick up nb of verts from EREGC call

C Merging points into existing lines and zone structure.
              do loop2=1,nbofdoorpts
                INVERT=INVERT+1
                call CKADDVERTINSURF(ITRU,ICOMP,'-',
     &            pxdoor(loop2),pydoor(loop2),Z1,IER)
                INVERT=INVERT+1
                call CKADDVERTINSURF(ITRU,ICOMP,'i',
     &            pxdoor(loop2),pydoor(loop2),Z3,IER)
                write(outs,'(a,3i4,a,2f8.3)') 'added door jambs ',
     &            loop,ntv,invert,' @ ',pxdoor(loop),pydoor(loop)
                call edisp(iuout,outs)
              enddo
              dodoor=.false.  ! reset to show doors completed.
            endif

C If there were window jamb points add them. Note the door
C vertices will already have been merged so start at NTV+1.
C The logic below works for up to 20 windows.
            if(nbofdoorpts.gt.0.and.nbofwindowpts.gt.0)then
              iloc=1
              do while (iloc.lt.nbofwindowpts)
                if(iloc.eq.1)then
                  iwin=1; idstart=NTV+1
                elseif(iloc.eq.3)then
                  iwin=2; idstart=NTV+5
                elseif(iloc.eq.5)then
                  iwin=3; idstart=NTV+9
                elseif(iloc.eq.7)then
                  iwin=4; idstart=NTV+13
                elseif(iloc.eq.9)then
                  iwin=5; idstart=NTV+17
                elseif(iloc.eq.11)then
                  iwin=6; idstart=NTV+21
                elseif(iloc.eq.13)then
                  iwin=7; idstart=NTV+25
                elseif(iloc.eq.15)then
                  iwin=8; idstart=NTV+29
                elseif(iloc.eq.17)then
                  iwin=9; idstart=NTV+33
                elseif(iloc.eq.19)then
                  iwin=10; idstart=NTV+37
                elseif(iloc.eq.21)then
                  iwin=11; idstart=NTV+41
                elseif(iloc.eq.23)then
                  iwin=12; idstart=NTV+45
                elseif(iloc.eq.25)then
                  iwin=13; idstart=NTV+49
                elseif(iloc.eq.27)then
                  iwin=14; idstart=NTV+53
                elseif(iloc.eq.29)then
                  iwin=15; idstart=NTV+57
                elseif(iloc.eq.31)then
                  iwin=16; idstart=NTV+61
                elseif(iloc.eq.33)then
                  iwin=17; idstart=NTV+65
                elseif(iloc.eq.35)then
                  iwin=18; idstart=NTV+69
                elseif(iloc.eq.37)then
                  iwin=19; idstart=NTV+73
                elseif(iloc.eq.39)then
                  iwin=20; idstart=NTV+77
                endif
C                write(6,'(a,i4,a,4i4)') 'Window verts start @',
C     &           idstart,' list',idstart,idstart+2,idstart+3,idstart+1
                iwjvn(iwin,1)=idstart; iwjvn(iwin,2)=idstart+2
                iwjvn(iwin,3)=idstart+3; iwjvn(iwin,4)=idstart+1
                iloc=iloc+2
              end do

              INVERT=NTV  ! pick up nb of verts from EREGC call
              do loop=1,nbofwindowpts
                INVERT=INVERT+1 

C Merging point into zone structure.
                call CKADDVERTINSURF(ITRU,ICOMP,'i',
     &            pxwin(loop),pywin(loop),Z4,IER)
                INVERT=INVERT+1
                call CKADDVERTINSURF(ITRU,ICOMP,'i',
     &            pxwin(loop),pywin(loop),Z5,IER)
                write(outs,'(a,3i4,a,2f8.3)') 'added window jambs ',
     &            loop,ntv,invert,' @ ',pxwin(loop),pywin(loop)
                call edisp(iuout,outs)
              enddo
              dowindow=.false.
            endif
            if(nbofdoorpts.eq.0.and.nbofwindowpts.gt.0)then

C Setup start vertex index for each inserted window.
              iloc=1
              do while (iloc.lt.nbofwindowpts)
                if(iloc.eq.1)then
                  iwin=1; idstart=NTV+1
                elseif(iloc.eq.3)then
                  iwin=2; idstart=NTV+5
                elseif(iloc.eq.5)then
                  iwin=3; idstart=NTV+9
                elseif(iloc.eq.7)then
                  iwin=4; idstart=NTV+13
                elseif(iloc.eq.9)then
                  iwin=5; idstart=NTV+17
                elseif(iloc.eq.11)then
                  iwin=6; idstart=NTV+21
                elseif(iloc.eq.13)then
                  iwin=7; idstart=NTV+25
                elseif(iloc.eq.15)then
                  iwin=8; idstart=NTV+29
                elseif(iloc.eq.17)then
                  iwin=9; idstart=NTV+33
                elseif(iloc.eq.19)then
                  iwin=10; idstart=NTV+37
                elseif(iloc.eq.21)then
                  iwin=11; idstart=NTV+41
                elseif(iloc.eq.23)then
                  iwin=12; idstart=NTV+45
                elseif(iloc.eq.25)then
                  iwin=13; idstart=NTV+49
                elseif(iloc.eq.27)then
                  iwin=14; idstart=NTV+53
                elseif(iloc.eq.29)then
                  iwin=15; idstart=NTV+57
                elseif(iloc.eq.31)then
                  iwin=16; idstart=NTV+61
                elseif(iloc.eq.33)then
                  iwin=17; idstart=NTV+65
                elseif(iloc.eq.35)then
                  iwin=18; idstart=NTV+69
                elseif(iloc.eq.37)then
                  iwin=19; idstart=NTV+73
                elseif(iloc.eq.39)then
                  iwin=20; idstart=NTV+77
                endif
C                write(6,'(a,i4,a,4i4)') 'Window verts start at',
C     &          idstart,' list',idstart,idstart+2,idstart+3,idstart+1
                iwjvn(iwin,1)=idstart; iwjvn(iwin,2)=idstart+2
                iwjvn(iwin,3)=idstart+3; iwjvn(iwin,4)=idstart+1
                iloc=iloc+2
              end do

              INVERT=NTV  ! pick up nb of verts from EREGC call
              do loop=1,nbofwindowpts
                INVERT=INVERT+1 

C Merging point into zone structure.
                call CKADDVERTINSURF(ITRU,ICOMP,'i',
     &            pxwin(loop),pywin(loop),Z4,IER)
                INVERT=INVERT+1
                call CKADDVERTINSURF(ITRU,ICOMP,'i',
     &            pxwin(loop),pywin(loop),Z5,IER)
                write(outs,'(a,3i4,a,2f8.3)') 'added window jambs ',
     &            loop,ntv,invert,' @ ',pxwin(loop),pywin(loop)
                call edisp(iuout,outs)
              enddo
              dowindow=.false.
            endif

C After processing doors and windows do furniture origins.
            if(dofurn)then
              if(nboffurnpts.gt.0)then
                do loop=1,nboffurnpts
                  INVERT=INVERT+1
                  call CKADDVERTINSURF(ITRU,ICOMP,'-',
     &              furx(loop),fury(loop),furz(loop),IER)
                  write(outs,'(a,3i4,a,3f8.3)') 'added furniture ',
     &              loop,ntv,invert,' @ ',
     &              furx(loop),fury(loop),furz(loop)
                  call edisp(iuout,outs)
                enddo
              endif
              dofurn=.false.
            endif
    
C Begin with default assumptions for each surface then overwrite
C this if user supplied information exists.
            CALL FILSUR(ICOMP,0)

C Update the connection list.
            ICCC=NCON
            DO 432 ICC=1,NSUR
              ICCC=ICCC+1; IC1(ICCC)=ICOMP; IE1(ICCC)=ICC
              ICT(ICCC)=0; IC2(ICCC)=0; IE2(ICCC)=0
              SSNAME(iccc)=SNAME(icomp,icc)
              SSOTF(iccc)=SOTF(icc)
              SSMLCN(iccc)=SMLCN(icc)
              ssmlcindex(iccc)=0  ! no matching MLC
              SSVFC(iccc)=SVFC(icc)
              SSOTHER(iccc,1)=SOTHER(icc,1)
              SSOTHER(iccc,2)=SOTHER(icc,2)
              SSOTHER(iccc,3)=SOTHER(icc,3)
              SSUSE(iccc,1)=SUSE(icc,1)
              SSUSE(iccc,2)=SUSE(icc,2)
              SSPARENT(iccc)=SPARENT(icc)
              IZSTOCN(icomp,icc)=iccc
  432       CONTINUE
            NCON=ICCC

C Complete zone data before exiting.
            IF(zname(ICOMP)(1:2).EQ.'  ')THEN
              IF(ICOMP.LE.9)WRITE(zname(ICOMP),'(A5,I1)')'Zone-',ICOMP
              IF(ICOMP.GT.9)WRITE(zname(ICOMP),'(A5,I2)')'Zone-',ICOMP
              IF(ICOMP.GT.99)WRITE(zname(ICOMP),'(A5,I3)')'Zone-',ICOMP
              lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update length of this string.
            ENDIF
            if(zdesc(ICOMP)(1:2).EQ.'  ')then
              write(zdesc(ICOMP),'(2a)') 
     &          zname(ICOMP)(1:lnzname(ICOMP)),' describes a...'
              lnzdesc(ICOMP)=lnblnk(zdesc(ICOMP))  ! update the length of this string.
            endif

C Set geometry type.
            CTYPE(icomp)='GEN '
            gversion(icomp) =1.1
            NDP(ICOMP)=3
            IDPN(ICOMP,1)=0; IDPN(ICOMP,2)=0; IDPN(ICOMP,3)=0

            NZSUR(ICOMP)=NSUR  ! update nzsur() it is needed by zgupdate.
            NZTV(ICOMP)=NTV

C Update the G7 common blocks and then assign ZBASEA.
            call zgupdate(1,ICOMP,ier)
            IUZBASEA(icomp)=0
            IBASES(ICOMP,1)=NSUR
            IZBASELIST(ICOMP)=1
            ibcs=IZSTOCN(icomp,NSUR)
            ZBASEA(icomp)= SSNA(ibcs)

C Update the global coordinates for this zones surfaces so that
C the subsequent wireframe image can be drawn and the bounds of
C the zone can be calculated.
            DO J=1,NZTV(ICOMP)
              VCOORD(ICOMP,J,1)=X(J)
              VCOORD(ICOMP,J,2)=Y(J)
              VCOORD(ICOMP,J,3)=Z(J)
            ENDDO
 
C Save this to file before passing into the geometry editing facility.
            write(outs,'(3a)') 'Updating ',
     &        zdesc(ICOMP)(1:lnzdesc(ICOMP)),'...'
            call edisp(iuout,outs)
            gversion(icomp) =1.1
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            IF(IER.EQ.1)THEN
              CALL usrmsg(' ',' Problem creating geometry file...','W')
            ENDIF

            write(outs,'(3a)') 'Updating ',
     &        zdesc(ICOMP)(1:lnzdesc(ICOMP)),'...done.'
            call edisp(iuout,outs)

C Add doors as surfaces (for up to 20).
            if(nbofdoorpts.gt.0)then
              iloc=1
              do while (iloc.lt.nbofdoorpts)
                if(iloc.eq.1)then
                  idoor=1; dn='door_a'
                elseif(iloc.eq.3)then
                  idoor=2; dn='door_b'
                elseif(iloc.eq.5)then
                  idoor=3; dn='door_c'
                elseif(iloc.eq.7)then
                  idoor=4; dn='door_d'
                elseif(iloc.eq.9)then
                  idoor=5; dn='door_e'
                elseif(iloc.eq.11)then
                  idoor=6; dn='door_f'
                elseif(iloc.eq.13)then
                  idoor=7; dn='door_g'
                elseif(iloc.eq.15)then
                  idoor=8; dn='door_h'
                elseif(iloc.eq.17)then
                  idoor=9; dn='door_i'
                elseif(iloc.eq.19)then
                  idoor=10; dn='door_j'
                elseif(iloc.eq.21)then
                  idoor=11; dn='door_k'
                elseif(iloc.eq.23)then
                  idoor=12; dn='door_l'
                elseif(iloc.eq.25)then
                  idoor=13; dn='door_m'
                elseif(iloc.eq.27)then
                  idoor=14; dn='door_n'
                elseif(iloc.eq.29)then
                  idoor=15; dn='door_o'
                elseif(iloc.eq.31)then
                  idoor=16; dn='door_p'
                elseif(iloc.eq.33)then
                  idoor=17; dn='door_q'
                elseif(iloc.eq.35)then
                  idoor=18; dn='door_r'
                elseif(iloc.eq.37)then
                  idoor=19; dn='door_s'
                elseif(iloc.eq.39)then
                  idoor=20; dn='door_t'
                endif
                ICON=IZSTOCN(icomp,nsur)+1
                NSUR=NSUR+1
                NZSUR(ICOMP)=NSUR
                NZTV(ICOMP)=NTV
                JVN(NSUR,1)=idjvn(idoor,1); JVN(NSUR,2)=idjvn(idoor,2)
                JVN(NSUR,3)=idjvn(idoor,3); JVN(NSUR,4)=idjvn(idoor,4)
                NVER(NSUR)=4
                CALL FILSUR(ICOMP,NSUR)
                write(SNAME(ICOMP,NSUR),'(a)') dn(1:lnblnk(dn))
                ltrace=1
                call addedsurf(icomp,icon,ltrace,ier)

C The base vertex associated is ibedgeofdoor() assume that is the
C index of the parent surface for use in mergedoorinparent. Note
C there will be two door clicks for each door so adjust index
C of ibedgeofdoor to account for this.
                IPS=ibedgeofdoor(iloc)  ! edge at the door click (iloc)
                ICS=NSUR    ! point to new surface
                call mergedoorinparent(1,IPS,ICS,icwhich1,icwhich2,ier)
                iloc=iloc+2
              end do

              write(outs,'(3a)') 'Updating ',
     &          zdesc(ICOMP)(1:lnzdesc(ICOMP)),'...for doors'
              call edisp(iuout,outs)
              gversion(icomp) =1.1
              call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
              IF(IER.EQ.1)THEN
                CALL usrmsg(' ','Problem creating geometry file...','W')
              ENDIF
              nbofdoorpts=0
            endif

C Now do update for windows. Logic for up to 20 windows.
            if(nbofwindowpts.gt.0)then
              iloc=1
              do while (iloc.lt.nbofwindowpts)
                if(iloc.eq.1)then
                  iwin=1; dn='glaze_a'
                elseif(iloc.eq.3)then
                  iwin=2; dn='glaze_b'
                elseif(iloc.eq.5)then
                  iwin=3; dn='glaze_c'
                elseif(iloc.eq.7)then
                  iwin=4; dn='glaze_d'
                elseif(iloc.eq.9)then
                  iwin=5; dn='glaze_e'
                elseif(iloc.eq.11)then
                  iwin=6; dn='glaze_f'
                elseif(iloc.eq.13)then
                  iwin=7; dn='glaze_g'
                elseif(iloc.eq.15)then
                  iwin=8; dn='glaze_h'
                elseif(iloc.eq.17)then
                  iwin=9; dn='glaze_i'
                elseif(iloc.eq.19)then
                  iwin=10; dn='glaze_j'
                elseif(iloc.eq.21)then
                  iwin=11; dn='glaze_k'
                elseif(iloc.eq.23)then
                  iwin=12; dn='glaze_l'
                elseif(iloc.eq.25)then
                  iwin=13; dn='glaze_m'
                elseif(iloc.eq.27)then
                  iwin=14; dn='glaze_n'
                elseif(iloc.eq.29)then
                  iwin=15; dn='glaze_o'
                elseif(iloc.eq.31)then
                  iwin=16; dn='glaze_p'
                elseif(iloc.eq.33)then
                  iwin=17; dn='glaze_q'
                elseif(iloc.eq.35)then
                  iwin=18; dn='glaze_r'
                elseif(iloc.eq.37)then
                  iwin=19; dn='glaze_s'
                elseif(iloc.eq.39)then
                  iwin=20; dn='glaze_t'
                endif
                ICON=IZSTOCN(icomp,nsur)+1; NSUR=NSUR+1
                NZSUR(ICOMP)=NSUR; NZTV(ICOMP)=NTV
                JVN(NSUR,1)=iwjvn(iwin,1); JVN(NSUR,2)=iwjvn(iwin,2)
                JVN(NSUR,3)=iwjvn(iwin,3); JVN(NSUR,4)=iwjvn(iwin,4)
                NVER(NSUR)=4
                CALL FILSUR(ICOMP,NSUR)
                write(SNAME(ICOMP,NSUR),'(a)') dn(1:lnblnk(dn))
                ltrace=1
                call addedsurf(icomp,icon,ltrace,ier)
                IPS=ibedgeofwin(iloc)  ! edge at child click (iloc)
                ICS=NSUR    ! point to new surface
                IRT=1       ! as a simple rectangular child within
                call mergechildinparent(0,IPS,ICS,IRT,ier)
                iloc=iloc+2
              end do
              write(outs,'(3a)') 'Updating ',
     &          zdesc(ICOMP)(1:lnzdesc(ICOMP)),'... for glazing'
              call edisp(iuout,outs)

              gversion(icomp) =1.1
              call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
              IF(IER.EQ.1)THEN
                CALL usrmsg(' ','Problem creating geometry file...','W')
              ENDIF
              nbofwindowpts=0
            endif

C Read in control file if defined, setup additional file names and then
C write out the ctl file with additional zone lined to no control and
C lastly, update the configuration file.
            OKC=.false.
            if(LCTLF(1:2).ne.'  '.and.LCTLF(1:4).ne.'UNKN')OKC=.true.
            if(OKC)then
              ICTLF=IFIL+1
              CALL ERPFREE(ICTLF,ISTAT)
              call FINDFIL(LCTLF,XST)
              IF(XST) CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
            endif

            NCOMP=NCOMP+1
            NCCODE(ICOMP)=NCOMP
            if(OKC)then
              icascf(NCOMP)=0
              call usrmsg(' updating control for additional zone...',
     &        ' ','P')
              call CTLWRT(ICTLF,IER)
              call usrmsg(' ',' ','-')
            endif
            CALL EMKCFG('-',IER)
            call usrmsg(' updating model for additional zone...done.',
     &        ' ','P')

C Save the floor plan base points to the history list. Set iphisznst
C to the first point in this zone and iphisznfn to the closing
C point of the zone and ignore any points related to door heads,
C window sills & heads.
            if(NW.gt.0)then
              do 217 j=1,lastwalllist
                if(nipwhis+1.le.MGTV)then
                  nipwhis=nipwhis+1
                  if(j.eq.1)iphisznst(icomp)=nipwhis
                  ipxwhis(nipwhis)=ipxbase(j)
                  ipywhis(nipwhis)=ipybase(j)
                endif
  217         continue
              if(nipwhis+1.le.MGTV)then
                nipwhis=nipwhis+1
                ipxwhis(nipwhis)=ipxbase(1)
                ipywhis(nipwhis)=ipybase(1)
              endif
              iphisznfn(icomp)=nipwhis

C Debug.
C              write(6,*) 'history ',icomp,iphisznst(icomp),
C     &          iphisznfn(icomp),lastwalllist
            endif
            justentered=.false.

C Refresh current bitmap to recognise zone just saved.
            completed=.true.
            inpm=inpmode
            call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,inpm,
     &       iwmg,factor,viewlim,nw,ixtn1,ixtn2,iytn1,iytn2,
     &       ixo,iyo,completed,other,lastwalllist)
          endif

C Reset the temporary string variables used to name and describe zone.
          ZN=' '
          zd=' '

        elseif(inpmode.eq.7)then

C Currently unused option.
          goto 142

        elseif(inpmode.eq.8)then
          goto 142
        endif
      else
        goto 3
      endif
      goto 3

C Error.
  99  if(IOS.eq.2)then
        call edisp(iuout,
     &    'permissions problem determining bitmap file type.')
      else
        call edisp(iuout,'problem determining bitmap file type.')
      endif
      goto 3

      end

C ********* dogrid
C Draw the grid using current parameters (notes on parameters found
C at top of code file). Other is the distance between grid points if
C the user edited the value (iwmg=7).
      subroutine dogrid(iwmg,factor,ixo,iyo,viewlim,other)
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

C Pixel (upper left and lower right corners) of where the bitmap is draw.
      integer ixbul,iybul,ixblr,iyblr ! x-up-left y-up-left x-lower-right y-lower-right
      common/bmlimits/ixbul,iybul,ixblr,iyblr

      dimension viewlim(6)
      logical greyok,closelabel
      character temp6*6
      real other   ! the other grid distance defined by user
      real xp,yp   ! current position in X and Y
      real xpl,ypl ! most recent metre interval.

C Local variables to pass to edline.
#ifdef OSI
      integer iix,iiy,iily,iilx,iiuy,iirx
      integer iicol
#else
      integer*8 iix,iiy,iily,iilx,iiuy,iirx
      integer*8 iicol
#endif       

C      write(6,*) 'dogrid called ',iwmg,factor,ixo,iyo,ixbul,iybul,
C     &  ixblr,iyblr,viewlim,other
      if(iwmg.eq.1)then
         return
      elseif(iwmg.gt.1)then

C If libraries and monitor support greyscale then reset forground
C to 36% grey when drawing dots.
        greyok=.false.
        if(nifgrey.gt.4)then
          iicol=nifgrey-4
          call winscl('i',iicol)
          greyok=.true.
        endif
        IF(IFS.GT.0)CALL winfnt(0)
        do 342 i=int(viewlim(1)),int(viewlim(2))
          ily=iyblr
          iuy=iybul
          if(iwmg.eq.7)then

C If user defined interval only set on the first pass.
            if(i.eq.int(viewlim(1)))then
              xp=float(i)
              xpl=float(i)
            else
              xpl=float(i)
            endif
          else
            xp=float(i)
            xpl=float(i)
          endif
          ix=ixo+int(xp*factor)

C Remember position of first and last line for use in horizontal lines.
          if(i.eq.int(viewlim(1)))ixaxl=ix

C Debug.
C          write(6,'(a,f8.4,3i6)') 'x grid info',xp,ix,ily,iuy

          if(ix.gt.ixbul.and.ix.lt.ixblr)then

C If within display range draw vertical line. If spacing less than 1m
C put label at 1m invervals and draw solid grey line.
            if(iwmg.eq.3.or.iwmg.eq.4.or.iwmg.eq.5.or.iwmg.eq.6)then
              if(greyok)then
                iicol=nifgrey-4
                call winscl('i',iicol)
              endif

C Draw the dotted line, first take calculated pixels and
C cast to local variable to pass to edline.
              iix=ix; iily=ily; iiuy=iuy
              call edline(iix,iily,iix,iiuy,2)
              if(i.gt.-10.and.i.lt.99)then
                write(temp6,'(i2)') i
                call textatxy(ix-8,iyblr+15,temp6,'i',nifgrey-4)
              else
                write(temp6,'(i3)') i
                call textatxy(ix-12,iyblr+15,temp6,'i',nifgrey-4)
              endif
              if(ix.gt.ixaxr)ixaxr=ix
            elseif(iwmg.eq.2)then

C Do every 2m.
              if(mod(i,2).eq.0)then
                if(greyok)then
                  iicol=nifgrey-4
                  call winscl('i',iicol)
                endif
                iix=ix; iily=ily; iiuy=iuy
                call edline(iix,iily,iix,iiuy,2)
                if(i.gt.-10.and.i.lt.99)then
                  write(temp6,'(i2)') i
                  call textatxy(ix-8,iyblr+15,temp6,'i',nifgrey-4)
                else
                  write(temp6,'(i3)') i
                  call textatxy(ix-12,iyblr+15,temp6,'i',nifgrey-4)
                endif
                if(ix.gt.ixaxr)ixaxr=ix
              endif
            elseif(iwmg.eq.7)then

C Check if we are at a metre mark and if so write label.
              call eclose(xp,xpl,0.001,closelabel)
              if(closelabel)then
                if(greyok)then
                  iicol=nifgrey-3
                  call winscl('i',iicol)
                endif
                iix=ix; iily=ily; iiuy=iuy
                call edline(iix,iily,iix,iiuy,3)
                if(i.gt.-10.and.i.lt.99)then
                  write(temp6,'(i2)') i
                  call textatxy(ix-8,iyblr+15,temp6,'i',nifgrey-4)
                else
                  write(temp6,'(i3)') i
                  call textatxy(ix-12,iyblr+15,temp6,'i',nifgrey-4)
                endif
              endif
  444         xp=xp+other
              ix=ixo+int(xp*factor)
              if(ix.gt.ixbul.and.ix.lt.ixblr)then
                if(greyok)then
                  iicol=nifgrey-3
                  call winscl('i',iicol)
                endif
                iix=ix; iily=ily; iiuy=iuy
                call edline(iix,iily,iix,iiuy,3)
                if(ix.gt.ixaxr)ixaxr=ix
                if(xp.lt.float(i+1)) goto 444  ! can any more be drawn within this metre?
              endif
            endif
          endif
          if(iwmg.eq.4)then
            xp=float(i)+0.5
            ix=ixo+int(xp*factor)
            if(ix.gt.ixbul.and.ix.lt.ixblr)then
              if(greyok)then
                iicol=nifgrey-3
                call winscl('i',iicol)
              endif
              iix=ix; iily=ily; iiuy=iuy
              call edline(iix,iily,iix,iiuy,3)
              if(ix.gt.ixaxr)ixaxr=ix
            endif
          elseif(iwmg.eq.5)then
            do 345 ij=1,3
              xp=xp+0.25
              ix=ixo+int(xp*factor)
              if(ix.gt.ixbul.and.ix.lt.ixblr)then
                if(greyok)then
                  iicol=nifgrey-4
                  call winscl('i',iicol)
                endif
                iix=ix; iily=ily; iiuy=iuy
                if(ij.eq.1)call edline(iix,iily,iix,iiuy,3)
                if(ij.eq.2)call edline(iix,iily,iix,iiuy,2)
                if(ij.eq.3)call edline(iix,iily,iix,iiuy,3)
                if(ix.gt.ixaxr)ixaxr=ix
              endif
 345        continue
          elseif(iwmg.eq.6)then
            do 344 ij=1,9
              xp=xp+0.1
              ix=ixo+int(xp*factor)
              if(ix.gt.ixbul.and.ix.lt.ixblr)then
                if(greyok)then
                  iicol=nifgrey-4
                  call winscl('i',iicol)
                endif
                iix=ix; iily=ily; iiuy=iuy
                if(ij.eq.5)then
                  call edline(iix,iily,iix,iiuy,2)
                else
                  call edline(iix,iily,iix,iiuy,3)
                endif
                if(ix.gt.ixaxr)ixaxr=ix
              endif
 344        continue
          endif
  342   continue

C Now loop through vertical axis.
        do 343 i=int(viewlim(3)),int(viewlim(4))
          ilx=ixaxl; irx=ixaxr
          if(iwmg.eq.7)then

C If user defined interval only set on the first pass.
            if(i.eq.int(viewlim(3)))then
              yp=float(i); ypl=float(i)
            else
              ypl=float(i)
            endif
          else
            yp=float(i); ypl=float(i)
          endif
          iy=iyo-int(yp*factor)

C Debug.
C          write(6,'(a,f8.4,3i6)') 'y grid info',yp,ilx,irx,iy

          if(iy.gt.iybul.and.iy.lt.iyblr)then
            if(iwmg.eq.3.or.iwmg.eq.4.or.iwmg.eq.5.or.iwmg.eq.6)then
              if(greyok)then
                iicol=nifgrey-4
                call winscl('i',iicol)
              endif
              iilx=ilx; iiy=iy; iirx=irx
              call edline(iilx,iiy,iirx,iiy,2)
              write(temp6,'(i3)') i
              call textatxy(irx+3,iy-1,temp6,'i',nifgrey-4)
            elseif(iwmg.eq.2)then

C Do every other metre.

C Debug.
C              write(6,*) 'i mod ',i,mod(i,2)

              if(mod(i,2).eq.0)then
                if(greyok)then
                  iicol=nifgrey-4
                  call winscl('i',iicol)
                endif
                iilx=ilx; iiy=iy; iirx=irx
                call edline(iilx,iiy,iirx,iiy,2)
                write(temp6,'(i3)') i
                call textatxy(irx+3,iy-1,temp6,'i',nifgrey-4)
              endif
            elseif(iwmg.eq.7)then
              call eclose(yp,ypl,0.001,closelabel)
              if(closelabel)then
                if(greyok)then
                  iicol=nifgrey-3
                  call winscl('i',iicol)
                endif
                iilx=ilx; iiy=iy; iirx=irx
                call edline(iilx,iiy,iirx,iiy,3)
                write(temp6,'(i3)') i
                call textatxy(irx+3,iy-1,temp6,'i',nifgrey-4)
              endif
  445         yp=yp+other
              iy=iyo-int(yp*factor)
              if(iy.gt.iybul.and.iy.lt.iyblr)then
                if(greyok)then
                  iicol=nifgrey-3
                  call winscl('i',iicol)
                endif
                iilx=ilx; iiy=iy; iirx=irx
                call edline(iilx,iiy,iirx,iiy,3)
                if(yp.lt.float(i+1)) goto 445  ! can any more be drawn within this metre?
              endif
            endif
          endif
          if(iwmg.eq.4)then
            yp=yp+0.5
            iy=iyo-int(yp*factor)
            if(greyok)then
              iicol=nifgrey-4
              call winscl('i',iicol)
            endif
            if(iy.gt.iybul.and.iy.lt.iyblr)then
              iilx=ilx; iiy=iy; iirx=irx
              call edline(iilx,iiy,iirx,iiy,3)
            endif
          elseif(iwmg.eq.5)then

C Logic for 0.25m.
            yp=yp+0.25
            iy=iyo-int(yp*factor)
            if(greyok)then
              iicol=nifgrey-4
              call winscl('i',iicol)
            endif
            if(iy.gt.iybul.and.iy.lt.iyblr)then
              iilx=ilx; iiy=iy; iirx=irx
              call edline(iilx,iiy,iirx,iiy,3)
            endif
            yp=yp+0.25
            iy=iyo-int(yp*factor)
            if(greyok)then
              iicol=nifgrey-4
              call winscl('i',iicol)
            endif
            if(iy.gt.iybul.and.iy.lt.iyblr)then
              iilx=ilx; iiy=iy; iirx=irx
              call edline(iilx,iiy,iirx,iiy,2)
            endif
            yp=yp+0.25
            iy=iyo-int(yp*factor)
            if(greyok)then
              iicol=nifgrey-4
              call winscl('i',iicol)
            endif
            if(iy.gt.iybul.and.iy.lt.iyblr)then
              iilx=ilx; iiy=iy; iirx=irx
              call edline(iilx,iiy,iirx,iiy,3)
            endif
          elseif(iwmg.eq.6)then

C Logic for 0.1m.
            do 346 ij=1,9
              yp=yp+0.1
              iy=iyo-int(yp*factor)
              if(iy.gt.iybul.and.iy.lt.iyblr)then
                if(greyok)then
                  iicol=nifgrey-4
                  call winscl('i',iicol)
                endif
                iilx=ilx; iiy=iy; iirx=irx
                if(ij.eq.5)then
                  call edline(iilx,iiy,iirx,iiy,2)
                else
                  call edline(iilx,iiy,iirx,iiy,3)
                endif
              endif
  346       continue
          endif
  343   continue
      endif
      IF(IFS.GT.0)CALL winfnt(IFS)
      iicol=0
      call winscl('-',iicol)
      return
      end

C ***** shifoverlays
C Based on logical variables xbmnorth,xbmorigin and sizes of invert,nw
C update the position parameters and pixel arrays. Paramter definintion
C noted at top of source code.
C inpm is the input mode.
      subroutine shifoverlays(nw,idfxoffset,idfyoffset,
     &  inpm,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo)
#include "building.h"

      integer INVERT  ! number of coordinates in list
      real px,py,pz   ! user coordinates from clicks
      integer ipxx,ipyy,ipzz ! pixel coordinates from clicks
      common/clicklist/INVERT,px(MTV),py(MTV),pz(MTV),
     &  ipxx(MTV),ipyy(MTV),ipzz(MTV)

      logical xbmnorth,xbmorigin  ! logical toggles for north origin
      logical xbmgrid,xbmscale    ! logical toggles for gridding and scale set
      logical trimmed ! if only part of bitmap is displayed
      common/disptoggle/xbmnorth,xbmorigin,xbmgrid,xbmscale,trimmed

      real pxbase,pybase       ! to hold floor plan base points
      integer ipxbase,ipybase  ! to hold floor plan base pixels
      common/clickbase/pxbase(MTV),pybase(MTV),ipxbase(MTV),ipybase(MTV)  ! to hold the initial base verts

      integer nbofdoorpts
      logical wasdoorunique    ! jamb points separate from floor plan base points
      integer ibedgeofdoor     ! for each door jamb the associated base edge
      real pxdoor,pydoor       ! to hold door jambs
      integer ipxdoor,ipydoor  ! to hold door jamb pixels
      common/clickdoor/nbofdoorpts,wasdoorunique(40),ibedgeofdoor(40),
     &  pxdoor(40),pydoor(40),ipxdoor(40),ipydoor(40)  ! to hold door jambs

      integer nbofwindowpts   ! remember how many window points added
      logical waswindowunique ! for each window point was it unique
      integer ibedgeofwin     ! for each window the associated clicked edge
      real pxwin,pywin        ! to hold window jambs
      integer ipxwin,ipywin   ! to hold window jamb pixels
      common/clickwin/nbofwindowpts,waswindowunique(40),ibedgeofwin(40),
     &  pxwin(40),pywin(40),ipxwin(40),ipywin(40)  ! to hold window jambs

C XYZ for up to 40 furniture points.
      integer nboffurnpts
      real furx,fury,furz  ! furniture origins
      integer ifurx,ifury  ! pixels for furniture origins
      common/furncord/nboffurnpts,furx(40),fury(40),furz(40),
     & ifurx(40),ifury(40)

      integer nipwhis
      integer ipxwhis,ipywhis ! previous pixel points for floor plan
      integer iphisznst,iphisznfn ! start & end for each zone
      common/clickhis/nipwhis,ipxwhis(MGTV),ipywhis(MGTV),
     &  iphisznst(MCOM),iphisznfn(MCOM)

      if(xbmnorth)then
        ixtn1=ixtn1+idfxoffset
        ixtn2=ixtn2+idfxoffset
        iytn1=iytn1+idfyoffset
        iytn2=iytn2+idfyoffset
      endif
      if(xbmorigin)then
        ixo=ixo+idfxoffset
        iyo=iyo+idfyoffset
      endif

C Shift the master pixel list. 
      if(INVERT.gt.0)then
        do j=1,INVERT
          if(inpm.eq.1.or.inpm.eq.2.or.inpm.eq.3.or.inpm.eq.6.or.
     &       inpm.eq.7)then

C For all plan views.
            ipxx(j)=ipxx(j)+idfxoffset
            ipyy(j)=ipyy(j)+idfyoffset
          elseif(inpm.eq.4)then

C For south elevation.
            ipxx(j)=ipxx(j)+idfxoffset
            ipzz(j)=ipzz(j)+idfyoffset
          elseif(inpm.eq.5)then

C For east elevation.
            ipyy(j)=ipyy(j)+idfxoffset
            ipzz(j)=ipzz(j)+idfyoffset
          endif
        enddo  ! of j
      endif

C If in floor plan extrusion mode shift current base points and historical
C base points.
      if(inpm.eq.3)then
        if(NW.gt.0)then
          do j=1,NW
            ipxbase(j)=ipxbase(j)+idfxoffset
            ipybase(j)=ipybase(j)+idfyoffset
          enddo
        endif
        if(nipwhis.gt.0)then
          do j=1,nipwhis
            ipxwhis(j)=ipxwhis(j)+idfxoffset
            ipywhis(j)=ipywhis(j)+idfyoffset
          enddo
        endif
        if(nbofdoorpts.gt.0)then
          do j=1,nbofdoorpts
            ipxdoor(j)=ipxdoor(j)+idfxoffset
            ipydoor(j)=ipydoor(j)+idfyoffset
          enddo
        endif
        if(nbofwindowpts.gt.0)then
          do j=1,nbofwindowpts
            ipxwin(j)=ipxwin(j)+idfxoffset
            ipywin(j)=ipywin(j)+idfyoffset
          enddo
        endif
        if(nboffurnpts.gt.0)then
          do j=1,nboffurnpts
            ifurx(j)=ifurx(j)+idfxoffset
            ifury(j)=ifury(j)+idfyoffset
          enddo
        endif

      endif
      return
      end

C ****** Refresh current bitmap and current overlayed information.
C Parameter definitions noted at top of source code.
C inpm is the input mode.
C Other is the grid distance if user supplied (iwmg=7).
      subroutine refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,
     &  inpm,iwmg,factor,viewlim,nw,ixtn1,ixtn2,iytn1,iytn2,
     &  ixo,iyo,completed,other,lastwalllist)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN

      integer INVERT  ! number of coordinates in list
      real px,py,pz   ! user coordinates from clicks
      integer ipxx,ipyy,ipzz ! pixel coordinates from clicks
      common/clicklist/INVERT,px(MTV),py(MTV),pz(MTV),
     &  ipxx(MTV),ipyy(MTV),ipzz(MTV)

      logical xbmnorth,xbmorigin  ! logical toggles for north origin
      logical xbmgrid,xbmscale    ! logical toggles for gridding and scale set
      logical trimmed ! if only part of bitmap is displayed
      common/disptoggle/xbmnorth,xbmorigin,xbmgrid,xbmscale,trimmed

      real pxbase,pybase       ! to hold floor plan base points
      integer ipxbase,ipybase  ! to hold floor plan base pixels
      common/clickbase/pxbase(MTV),pybase(MTV),ipxbase(MTV),ipybase(MTV)  ! to hold the initial base verts

      integer nbofdoorpts
      logical wasdoorunique    ! jamb points separate from floor plan base points
      integer ibedgeofdoor     ! for each door jamb the associated base edge
      real pxdoor,pydoor       ! to hold door jambs
      integer ipxdoor,ipydoor  ! to hold door jambs pixels
      common/clickdoor/nbofdoorpts,wasdoorunique(40),ibedgeofdoor(40),
     &  pxdoor(40),pydoor(40),ipxdoor(40),ipydoor(40)  ! to hold door jambs

      integer nbofwindowpts   ! remember how many window points added
      logical waswindowunique ! for each window point was it unique
      integer ibedgeofwin     ! for each window the associated clicked edge
      real pxwin,pywin        ! to hold window jambs
      integer ipxwin,ipywin   ! to hold window jamb pixels
      common/clickwin/nbofwindowpts,waswindowunique(40),ibedgeofwin(40),
     &  pxwin(40),pywin(40),ipxwin(40),ipywin(40)  ! to hold window jambs

C XYZ for up to 40 furniture points.
      integer nboffurnpts
      real furx,fury,furz
      integer ifurx,ifury  ! pixels for furniture origins
      common/furncord/nboffurnpts,furx(40),fury(40),furz(40),
     & ifurx(40),ifury(40)

      integer nipwhis
      integer ipxwhis,ipywhis ! previous pixel points for floor plan
      integer iphisznst,iphisznfn ! start & end for each zone
      common/clickhis/nipwhis,ipxwhis(MGTV),ipywhis(MGTV),
     &  iphisznst(MCOM),iphisznfn(MCOM)

C Pixel (upper left and lower right corners) of where the bitmap is draw.
      integer ixbul,iybul,ixblr,iyblr ! x-up-left y-up-left x-lower-right y-lower-right
      common/bmlimits/ixbul,iybul,ixblr,iyblr

      logical completed,within

      dimension viewlim(6)
      character fname*72,temp6*6,temp3*3
#ifdef OSI
      integer iixtn1,iiytn1,iixtn2,iiytn2,iid1,iid2,iid3,iid4
      integer iicol
      integer iixoffset,iiyoffset,iiwidth,iihight
      integer iixbul,iiybul,iixblr,iiyblr,iilix,iiliy
#else
      integer*8 iixtn1,iiytn1,iixtn2,iiytn2,iid1,iid2,iid3,iid4
      integer*8 iicol
      integer*8 iixoffset,iiyoffset,iiwidth,iihight
      integer*8 iixbul,iiybul,iixblr,iiyblr,iilix,iiliy
#endif

C Debug.
C      write(6,*) 'refresh bitmap'

      call startbuffer()
      iixoffset=ixoffset; iiyoffset=iyoffset
      iiwidth=iwidth; iihight=ihight
      iixbul=ixbul; iiybul=iybul; iixblr=ixblr; iiyblr=iyblr
      iilix=15; iiliy=15
      call winlodpart(fname,iixoffset,iiyoffset,iiwidth,iihight,
     &  iilix,iiliy,iixbul,iiybul,iixblr,iiyblr)
      ixbul=int(iixbul); iybul=int(iiybul)
      ixblr=int(iixblr); iyblr=int(iiyblr)
      call edisp(iuout,'Refreshing bitmap...')
      iicol=0
      call winscl('z',iicol)
      iid1=ixbul; iid2=iybul;
      call esymbol(iid1,iid2,6,1)
      iid1=ixblr; iid2=iyblr;
      call esymbol(iid1,iid2,4,1)
      call winscl('-',iicol)

C If north known and within the displayed area then draw it.
      if(inpm.eq.4.or.inpm.eq.5)then
        continue
      else
        if(xbmnorth)then
          if(((ixtn1.ge.ixbul).and.(ixtn1.le.ixblr)).and.
     &       ((iytn1.ge.iybul).and.(iytn1.le.iyblr)))then
            iicol=0
            call winscl('z',iicol)
            iid1=ixtn1; iid2=iytn1;
            call esymbol(iid1,iid2,11,1)
            iid1=ixtn2; iid2=iytn2;
            call esymbol(iid1,iid2,18,1)
            iixtn1=ixtn1; iiytn1=iytn1; iixtn2=ixtn2; iiytn2=iytn2;
            call edwline(iixtn1,iiytn1,iixtn2,iiytn2)
            write(temp6,'(A)')'North'
            iid1=ixtn2+5; iid2=iytn2-5;
            CALL wstxpt(iid1,iid2,temp6)
            call winscl('-',iicol)
          endif
        endif
      endif

C If origin known and within the displayed area then draw it.
      if(xbmorigin)then
        if(((ixo.ge.ixbul).and.(ixo.le.ixblr)).and.
     &     ((iyo.ge.iybul).and.(iyo.le.iyblr)))then
          iicol=0
          call winscl('z',iicol)
          iid1=ixo; iid2=iyo;
          call esymbol(iid1,iid2,24,1)
          write(temp6,'(A)')'Origin'
          iid1=ixo+5; iid2=iyo-5;
          CALL wstxpt(iid1,iid2,temp6)
          call winscl('-',iicol)
        endif
      endif
      if(xbmgrid)then

C If bitmap has been panned then the extents of the grid need to be
C assessed again.
        if(xbmscale)then
          bituwidth=float(ixblr-ixbul)/factor
          bituheight=float(iyblr-iybul)/factor
          uxoffset=float(ixbul-ixo)/factor
          uyoffset=float(iyo-iyblr)/factor
          viewlim(1)=float(int(uxoffset))
          viewlim(2)=real(anint(viewlim(1) + bituwidth))
          viewlim(3)=float(int(uyoffset))
          viewlim(4)=real(anint(viewlim(3) + bituheight))
        endif
        call dogrid(iwmg,factor,ixo,iyo,viewlim,other)
      endif

C If there are vertices then draw them. Note: check if within the
C limits of ixbul,iybul,ixblr,iyblr (the pixel positions of the
C bitmap as drawn on the display). Also include vertices for
C windows doors furniture origins that might have been included 
C in a floor plan extrusion.
      if(INVERT.gt.0)then
        do 122 j=1,INVERT
          if(inpm.eq.1.or.inpm.eq.2.or.inpm.eq.3.or.inpm.eq.6)then
            if(((ipxx(j).ge.ixbul).and.(ipxx(j).le.ixblr)).and.
     &         ((ipyy(j).ge.iybul).and.(ipyy(j).le.iyblr)))then
              iicol=3
              call winscl('z',iicol)
              iid1=ipxx(j); iid2=ipyy(j);
              call esymbol(iid1,iid2,24,1)
              if(j.gt.0.and.j.le.9)write(temp3,'(i1)') j
              if(j.gt.9.and.j.le.99)write(temp3,'(i2)') j
              if(j.gt.99)write(temp3,'(i3)') j
              call textatxy(ipxx(j)+4,ipyy(j)-1,temp3,'z',3)
              iicol=0
              call winscl('-',iicol)
            endif
          elseif(inpm.eq.4)then
            if(((ipxx(j).ge.ixbul).and.(ipxx(j).le.ixblr)).and.
     &         ((ipzz(j).ge.iybul).and.(ipzz(j).le.iyblr)))then
              iicol=3
              call winscl('z',iicol)
              iid1=ipxx(j); iid2=ipzz(j);
              call esymbol(iid1,iid2,24,1)
              if(j.gt.0.and.j.le.9)write(temp3,'(i1)') j
              if(j.gt.9.and.j.le.99)write(temp3,'(i2)') j
              if(j.gt.99)write(temp3,'(i3)') j
              call textatxy(ipxx(j)+4,ipzz(j)-1,temp3,'z',3)
              iicol=0
              call winscl('-',iicol)
            endif
          elseif(inpm.eq.5)then
            if(((ipyy(j).ge.ixbul).and.(ipyy(j).le.ixblr)).and.
     &         ((ipzz(j).ge.iybul).and.(ipzz(j).le.iyblr)))then
              iicol=3
              call winscl('z',iicol)
              iid1=ipyy(j); iid2=ipzz(j);
              call esymbol(iid1,iid2,24,1)
              if(j.gt.0.and.j.le.9)write(temp3,'(i1)') j
              if(j.gt.9.and.j.le.99)write(temp3,'(i2)') j
              if(j.gt.99)write(temp3,'(i3)') j
              call textatxy(ipyy(j)+4,ipzz(j)-1,temp3,'z',3)
              iicol=0
              call winscl('-',iicol)
            endif
          endif
  122   continue
      endif

C If there are historical points from earlier extruded floor plans
C draw them. If outside of the displayed bitmap do not draw it
C or the line of the wall.
      if(nipwhis.gt.0)then
        do 124 j=1,nipwhis
          if(((ipxwhis(j).ge.ixbul).and.(ipxwhis(j).le.ixblr)).and.
     &       ((ipywhis(j).ge.iybul).and.(ipywhis(j).le.iyblr)))then
            iicol=2
            call winscl('z',iicol)
            iid1=ipxwhis(j); iid2=ipywhis(j);
            call esymbol(iid1,iid2,24,1)
            if(j.gt.0.and.j.le.9)write(temp3,'(i1)') j
            if(j.gt.9.and.j.le.99)write(temp3,'(i2)') j
            if(j.gt.99)write(temp3,'(i3)') j
            call textatxy(ipxwhis(j)+4,ipywhis(j)-1,temp3,'z',2)
            if(j.gt.1)then
              iicol=2
              call winscl('z',iicol)

C See which zone this point is in. If past the first point in
C the zone draw back to the starting point.
              within=.false.
              do 42 iz=1,MCOM
                if(iphisznst(iz).ne.0.and.iphisznfn(iz).ne.0)then
                  if(j.gt.iphisznst(iz).and.
     &               j.le.iphisznfn(iz))within=.true.
                endif
  42          continue
              if(within)then
                iid1=ipxwhis(j-1); iid2=ipywhis(j-1)
                iid3=ipxwhis(j); iid4=ipywhis(j)
                call edwline(iid1,iid2,iid3,iid4)
              endif
            endif
          endif
  124   continue
      endif

C If there is an extruded floor plan draw them. If of outside
C of the displayed bitmap do not draw it or the line of the wall.
C Debug.
C      write(6,*) 'refrshcur nw lw nd nw nf ',NW,lastwalllist,
C     &  nbofdoorpts,nbofwindowpts,nboffurnpts
      if(NW.gt.0.and.inpm.eq.3)then
        do 123 j=1,NW
          if(((ipxbase(j).ge.ixbul).and.(ipxbase(j).le.ixblr)).and.
     &       ((ipybase(j).ge.iybul).and.(ipybase(j).le.iyblr)))then
            iicol=3
            call winscl('z',iicol)
            iid1=ipxbase(j); iid2=ipybase(j);
            call esymbol(iid1,iid2,24,1)
            if(j.gt.0.and.j.le.9)write(temp3,'(i1)') j
            if(j.gt.9.and.j.le.99)write(temp3,'(i2)') j
            if(j.gt.99)write(temp3,'(i3)') j
            call textatxy(ipxbase(j)+4,ipybase(j)-1,temp3,'z',3)
            if(j.gt.1)then
              iicol=3
              call winscl('z',iicol)
              iid1=ipxbase(j-1); iid2=ipybase(j-1)
              iid3=ipxbase(j); iid4=ipybase(j)
              call edwline(iid1,iid2,iid3,iid4)
            endif
          endif
  123   continue

C If user has completed selection, draw the closing line if it is
C within the extents of the displayed bitmap. Use lastwalllist
C to match logic elsewhere.
        if(completed)then
          if(((ipxbase(1).ge.ixbul).and.(ipxbase(1).le.ixblr)).and.
     &       ((ipybase(1).ge.iybul).and.(ipybase(1).le.iyblr)).and.
     &       ((ipxbase(NW).ge.ixbul).and.(ipxbase(NW).le.ixblr)).and.
     &       ((ipybase(NW).ge.iybul).and.(ipybase(NW).le.iyblr)))then
            iicol=3
            call winscl('z',iicol)
            iid1=ipxbase(lastwalllist); iid2=ipybase(lastwalllist)
            iid3=ipxbase(1); iid4=ipybase(1)
            call edwline(iid1,iid2,iid3,iid4)
          endif
        endif
        iicol=0
        call winscl('-',iicol)

C If there are door jambs draw them.
        if(nbofdoorpts.gt.0)then
          do j=1,nbofdoorpts
            if(((ipxdoor(j).ge.ixbul).and.(ipxdoor(j).le.ixblr)).and.
     &        ((ipydoor(j).ge.iybul).and.(ipydoor(j).le.iyblr)))then
              iicol=3
              call winscl('z',iicol)
              iid1=ipxdoor(j); iid2=ipydoor(j);
              call esymbol(iid1,iid2,24,1)
              if(j.gt.0.and.j.le.9)write(temp3,'(a,i1)') 'd',j
              if(j.gt.9.and.j.le.99)write(temp3,'(i2)') 'd',j
              call textatxy(ipxdoor(j)+4,ipydoor(j)-1,temp3,'z',4)
              iicol=0
              call winscl('-',iicol)
            endif
          enddo
        endif

C If there are window jambs draw them.
        if(nbofwindowpts.gt.0)then
          do j=1,nbofwindowpts
            if(((ipxwin(j).ge.ixbul).and.(ipxwin(j).le.ixblr)).and.
     &        ((ipywin(j).ge.iybul).and.(ipywin(j).le.iyblr)))then
              iicol=3
              call winscl('z',iicol)
              iid1=ipxwin(j); iid2=ipywin(j);
              call esymbol(iid1,iid2,24,1)
              if(j.gt.0.and.j.le.9)write(temp3,'(a,i1)') 'w',j
              if(j.gt.9.and.j.le.99)write(temp3,'(a,i2)') 'w',j
              call textatxy(ipxwin(j)+4,ipywin(j)-1,temp3,'z',5)
              iicol=0
              call winscl('-',iicol)
            endif
          enddo
        endif

C If furniture origins.
        if(nboffurnpts.gt.0)then
          do j=1,nboffurnpts
            if(((ifurx(j).ge.ixbul).and.(ifurx(j).le.ixblr)).and.
     &        ((ifury(j).ge.iybul).and.(ifury(j).le.iyblr)))then
              iicol=3
              call winscl('z',iicol)
              iid1=ifurx(j); iid2=ifury(j);
              call esymbol(iid1,iid2,24,1)
              if(j.gt.0.and.j.le.9)write(temp3,'(a,i1)') 'f',j
              if(j.gt.9.and.j.le.99)write(temp3,'(a,i2)') 'f',j
              call textatxy(ifurx(j)+4,ifury(j)-1,temp3,'z',6)
              iicol=0
              call winscl('-',iicol)
            endif
          enddo
        endif

      endif
      return
      end

C **** control panning
C Interface control for panning of the bitmap within the graphics feedback
C area of a esp-r module.
C inpm is the input mode.
      subroutine ctlbmpan(iwm,fname,ixoffset,iyoffset,ibmwidth,ibmhight,
     &  iwidth,ihight,inpm,nw,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

C Pixel (upper left and lower right corners) of where the bitmap is draw.
      integer ixbul,iybul,ixblr,iyblr ! x-up-left y-up-left x-lower-right y-lower-right
      common/bmlimits/ixbul,iybul,ixblr,iyblr

      logical xbmnorth,xbmorigin  ! logical toggles for north origin
      logical xbmgrid,xbmscale    ! logical toggles for gridding and scale set
      logical trimmed ! if only part of bitmap is displayed
      common/disptoggle/xbmnorth,xbmorigin,xbmgrid,xbmscale,trimmed

      character fname*72

#ifdef OSI
      integer iixoffset,iiyoffset,iiwidth,iihight
      integer iixbul,iiybul,iixblr,iiyblr
#else
      integer*8 iixoffset,iiyoffset,iiwidth,iihight
      integer*8 iixbul,iiybul,iixblr,iiyblr
#endif

C Debug.
C      write(6,*) 'offsets ',ixoffset,iyoffset

      if(IWM.eq.1)then
        return
      elseif(IWM.eq.2)then

C It at the left edge do nothing. If near (within 50 pixels) of the left
C edge then make it the left edge. Otherwise recalculate positions and
C load a sub-section of the bitmap into the current display.
        if(ixoffset.eq.0)then
          return
        elseif(ixoffset.gt.0.and.ixoffset.le.50)then
          call startbuffer()
          idfxoffset=ixoffset; idfyoffset=0; ixoffset=0
          iwidth=ibmwidth
          if(igw.lt.ibmwidth) iwidth=igw
          ihight=ibmhight
          if(igwh.lt.ibmhight) ihight=igwh

C Cast for calling of C code.
          iixoffset=ixoffset; iiyoffset=iyoffset
          iiwidth=iwidth; iihight=ihight
          iixbul=ixbul; iiybul=iybul; iixblr=ixblr; iiyblr=iyblr
          call checklodpart(fname,iixoffset,iiyoffset,iiwidth,iihight,
     &      15,15,iixbul,iiybul,iixblr,iiyblr)
          call edisp(iuout,'Back to left edge of bitmap.')
        else
          call startbuffer()
          idfxoffset=50; idfyoffset=0; ixoffset=ixoffset-50
          iwidth=ibmwidth
          if(igw.lt.ibmwidth) iwidth=igw
          ihight=ibmhight
          if(igwh.lt.ibmhight) ihight=igwh

C Cast for calling of C code.
          iixoffset=ixoffset; iiyoffset=iyoffset
          iiwidth=iwidth; iihight=ihight
          iixbul=ixbul; iiybul=iybul; iixblr=ixblr; iiyblr=iyblr
          call checklodpart(fname,iixoffset,iiyoffset,iiwidth,iihight,
     &      15,15,iixbul,iiybul,iixblr,iiyblr)
          call edisp(iuout,'Panned 50 pixels to left...')
        endif
        call shifoverlays(nw,idfxoffset,
     &   idfyoffset,inpm,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo)
      elseif(IWM.eq.3)then
        call startbuffer()
        idfxoffset=-50; idfyoffset=0; ixoffset=ixoffset+50
        iwidth=ibmwidth-50
        if(igw.lt.iwidth) iwidth=igw
        ihight=ibmhight
        if(igwh.lt.ibmhight) ihight=igwh

C Cast for calling of C code.
        iixoffset=ixoffset; iiyoffset=iyoffset
        iiwidth=iwidth; iihight=ihight
        iixbul=ixbul; iiybul=iybul; iixblr=ixblr; iiyblr=iyblr
        call checklodpart(fname,iixoffset,iiyoffset,iiwidth,iihight,
     &    15,15,iixbul,iiybul,iixblr,iiyblr)
        call edisp(iuout,'Panned 50 pixels to right...')

        call shifoverlays(nw,idfxoffset,
     &   idfyoffset,inpm,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo)
      elseif(IWM.eq.4)then
        if(iyoffset.eq.0)then
          return
        elseif(iyoffset.gt.0.and.iyoffset.le.50)then
          call startbuffer()
          idfyoffset=iyoffset; idfxoffset=0; iyoffset=0
          iwidth=ibmwidth
          if(igw.lt.ibmwidth) iwidth=igw
          ihight=ibmhight
          if(igwh.lt.ibmhight) ihight=igwh

C Cast for calling of C code.
          iixoffset=ixoffset; iiyoffset=iyoffset
          iiwidth=iwidth; iihight=ihight
          iixbul=ixbul; iiybul=iybul; iixblr=ixblr; iiyblr=iyblr
          call checklodpart(fname,iixoffset,iiyoffset,iiwidth,iihight,
     &      15,15,iixbul,iiybul,iixblr,iiyblr)
          call edisp(iuout,'Panned to top of bitmap...')

          call shifoverlays(nw,idfxoffset,
     &     idfyoffset,inpm,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo)
        else
          call startbuffer()
          idfyoffset=50; idfxoffset=0; iyoffset=iyoffset-50
          iwidth=ibmwidth
          if(igw.lt.ibmwidth) iwidth=igw
          ihight=ibmhight
          if(igwh.lt.ibmhight) ihight=igwh

C Cast for calling of C code.
          iixoffset=ixoffset; iiyoffset=iyoffset
          iiwidth=iwidth; iihight=ihight
          iixbul=ixbul; iiybul=iybul; iixblr=ixblr; iiyblr=iyblr
          call checklodpart(fname,iixoffset,iiyoffset,iiwidth,iihight,
     &      15,15,iixbul,iiybul,iixblr,iiyblr)
          call edisp(iuout,'Panned 50 pixels up in bitmap...')

          call shifoverlays(nw,idfxoffset,
     &     idfyoffset,inpm,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo)
        endif
      elseif(IWM.eq.5)then
        call startbuffer()
        idfxoffset=0; idfyoffset=-50; iyoffset=iyoffset+50
        iwidth=ibmwidth
        if(igw.lt.ibmwidth) iwidth=igw
        ihight=ibmhight-50
        if(igwh.lt.ihight) ihight=igwh

C Cast for calling of C code.
        iixoffset=ixoffset; iiyoffset=iyoffset
        iiwidth=iwidth; iihight=ihight
        iixbul=ixbul; iiybul=iybul; iixblr=ixblr; iiyblr=iyblr
        call checklodpart(fname,iixoffset,iiyoffset,iiwidth,iihight,
     &    15,15,iixbul,iiybul,iixblr,iiyblr)
        call edisp(iuout,'Panned 50 pixels down in bitmap...')

        call shifoverlays(nw,idfxoffset,
     &   idfyoffset,inpm,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo)
      endif
      return
      end


C ************* CKADDVERTINSURF
C CKADDVERTINSURF confirm if a potential vertex (XQ,YQ,ZQ) is unique
C (not really close to another) add to zone. If act='i' and along 
C the edge of an existing surface update the zone topology to include it.
C ITRU = unit number for user output, IER=0 OK, IER=1 problem.
      SUBROUTINE CKADDVERTINSURF(ITRU,ICOMP,act,XQ,YQ,ZQ,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
      COMMON/FILEP/IFIL
      LOGICAL ok,dok,matchver
      character act*1

C Tollerance for surface matching.
      real ANGCC  ! angle between surfaces tolerance
      real CACC   ! tolerance between vertices
      real DACC   ! tolerance along line
      real COGCC  ! tolerance between surface COG
      real SNACC  ! tolerance between surface areas
      integer IACC ! number of matching corners outside dist tolerance
      common/matching/ANGCC,CACC,DACC,COGCC,SNACC,IACC

      character outs*124
      integer iwhich1,iwhich2,iwhich3  ! for user selections
      logical found,closex,closey,closez

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CKADDVERTINSURF'  ! set for subroutine

      iwhich3=0; iwhich1=0; iwhich2=0

C Use gversion 1.1.
      gversion(icomp) =1.1

C Similar logic to ADDVERT. Consider found if within CACC.
      if(NZTV(icomp)+1.le.MTV)then
        found=.false.
        do ixx = 1,NZTV(icomp)
          CALL ECLOSE(XQ,X(ixx),CACC,closex)
          CALL ECLOSE(YQ,Y(ixx),CACC,closey)
          CALL ECLOSE(ZQ,Z(ixx),CACC,closez)
          if(closex.and.closey.and.closez)then
            found=.true.
            goto 145
          endif
          CALL ECLOSE(XQ,X(ixx),CACC*2,closex)
          CALL ECLOSE(YQ,Y(ixx),CACC*2,closey)
          CALL ECLOSE(ZQ,Z(ixx),CACC*2,closez)
          dist=crowxyz(QX,QY,QZ,X(ixx),Y(ixx),Z(ixx))
          if(closex.and.closey.and.closez)then
            write(outs,'(a,3f8.3,a,f6.3,a,i3,a,3f8.3)')
     &        'New vertex @',QX,QY,QZ,' is close (',
     &        dist,') to existing vertex',ixx,' @',
     &        X(ixx),Y(ixx),Z(ixx)
            call edisp(itru,outs)
            dok=.true.
            call askok('Copied point close to existing vertex.',
     &        'Skip copying it?',found,dok,nbhelp)
            goto 145
          endif
        enddo  ! of ixx

C If an existing vertex is close then skip to next, otherwise
C add a new vertex to the end of the zone list and update vcoord.
  145   if(found)then
          continue
        else
          NTV=NTV+1
          NZTV(icomp)=NTV
          X(NTV)=XQ; Y(NTV)=YQ; Z(NTV)=ZQ
          VCOORD(ICOMP,ntv,1)=XQ
          VCOORD(ICOMP,ntv,2)=YQ
          VCOORD(ICOMP,ntv,3)=ZQ
          XMN=AMIN1(XMN,X(NTV))
          YMN=AMIN1(YMN,Y(NTV))
          ZMN=AMIN1(ZMN,Z(NTV))
          XMX=AMAX1(XMX,X(NTV))
          YMX=AMAX1(YMX,Y(NTV))
          ZMX=AMAX1(ZMX,Z(NTV))

C Update the geometry file to remember the new vertex.
          call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,itru,3,IER)

C For a unique vertex also test to see if it is close (+-CACC) to
C one of the existing lines. Also check the last edge in the surface.
          if(act(1:1).eq.'i')then
            continue
          else
            return
          endif
          iwhich3=NTV
          do 2246 ivj=1,NSUR
            ivjlimit=NVER(ivj)
            do 2247 ivjj=1,ivjlimit
              if(ivjj.eq.ivjlimit)then
                iwhich1=JVN(ivj,ivjj)
                iwhich2=JVN(ivj,1)
              else
                iwhich1=JVN(ivj,ivjj)
                iwhich2=JVN(ivj,ivjj+1)
              endif

C Report length of line. Use method of Ward/Radiance in fvect.c
C Consider close if less than 3mm.
              call pointtoline(iwhich3,iwhich1,iwhich2,offset,
     &          matchver)
              if(.NOT.matchver) goto 2247
              if(offset.lt.CACC)then
                write(outs,'(a,i3,a,3f9.4,a,f6.4,a,i3,a,i3,2a)')
     &            'New vertex ',iwhich3,' @',
     &            X(NTV),Y(NTV),Z(NTV),' is close (',offset,
     &            ') to edge ',IWHICH1,' &',IWHICH2,' of ',
     &            sname(icomp,ivj)
                call edisp(itru,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                if(NVER(ivj)+1.le.MV)then
                  if(offset.le.CACC)then
                    ok=.true.
                  else
                    dok=.true.
                    helptopic='vertex_near_edge'
                    call gethelptext(helpinsub,helptopic,nbhelp)
                    call askok(
     &                'Insert this vertex into the adjacent surface?',
     &                '(see help)',ok,dok,nbhelp)
                  endif
                else
                  ok=.false.
                endif
                if(ok)then
                  NVER(ivj)=NVER(ivj)+1
                  IXV=NVER(ivj)+1
 1148             continue
                  IXV=IXV-1
                  JVN(ivj,IXV)=JVN(ivj,IXV-1)
                  IF(IXV.GT.ivjj+1)GOTO 1148
                  JVN(ivj,ivjj+1)=iwhich3
                  call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,itru,3,IER)

C Debug.
C                   write(6,'(a,124i4)') 'now jvn is ',
C     &               (JVN(ivj,ii),ii=1,NVER(ivj))

C Surface (ivj) vertex list has been updated. Go on to next surface.
                  goto 2246 
                endif
              endif
 2247       continue
 2246     continue  ! end of loop of surfs in zone
        endif
      endif
      end


C ************* MERGEVERTINSURF
C MERGEVERTINSURF merge a vertex (QX,QY,QZ) into a zone and if it is along 
C the edge of an existing surface update the zone topology to include it.
C IER=0 OK, IER=1 problem.
      SUBROUTINE MERGEVERTINSURF(ICOMP,QX,QY,QZ,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"

      integer icomp
      real QX,QY,QZ
      integer ier

      COMMON/FILEP/IFIL
      LOGICAL ok,dok,matchver
      COMMON/OUTIN/IUOUT,IUIN

C Tollerance for surface matching.
      real ANGCC  ! angle between surfaces tolerance
      real CACC   ! tolerance between vertices
      real DACC   ! tolerance along line
      real COGCC  ! tolerance between surface COG
      real SNACC  ! tolerance between surface areas
      integer IACC ! number of matching corners outside dist tolerance
      common/matching/ANGCC,CACC,DACC,COGCC,SNACC,IACC

      character outs*124
      integer iwhich1,iwhich2,iwhich3  ! for user selections
      logical found

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MERGEVERTINSURF'  ! set for subroutine

      iwhich3=0; iwhich1=0; iwhich2=0

C Set new version for geometry file.
      gversion(icomp) =1.1

C Assumes that QX,QY,QZ already checked to see if they duplicate
C anything in the zone. This code does the merge and save.
      NTV=NTV+1
      NZTV(icomp)=NTV
      X(NTV)=QX; Y(NTV)=QY; Z(NTV)=QZ
      VCOORD(ICOMP,ntv,1)=QX
      VCOORD(ICOMP,ntv,2)=QY
      VCOORD(ICOMP,ntv,3)=QZ
      XMN=AMIN1(XMN,X(NTV))
      YMN=AMIN1(YMN,Y(NTV))
      ZMN=AMIN1(ZMN,Z(NTV))
      XMX=AMAX1(XMX,X(NTV))
      YMX=AMAX1(YMX,Y(NTV))
      ZMX=AMAX1(ZMX,Z(NTV))

C Update the geometry file to remember the new vertex.
      call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)

C For a unique vertex also test to see if it is close (+-CACC) to
C one of the existing lines. nsur-1 because the new surface does
C not need to be checked. Also check the last edge in the surface.
      iwhich3=ntv
      do 246 ivj=1,NSUR-1
        ivjlimit=NVER(ivj)
        do 247 ivjj=1,ivjlimit
          if(ivjj.eq.ivjlimit)then
            iwhich1=JVN(ivj,ivjj)
            iwhich2=JVN(ivj,1)
          else
            iwhich1=JVN(ivj,ivjj)
            iwhich2=JVN(ivj,ivjj+1)
          endif

C Report length of line. Use method of Ward/Radiance in fvect.c
          call pointtoline(iwhich3,iwhich1,iwhich2,offset,matchver)
          if(.NOT.matchver) goto 247
          if(offset.lt.CACC)then
            write(outs,'(a,i3,a,3f8.3,a,f6.4,a,i3,a,i3,2a)')
     &        'New vertex ',iwhich3,' @',
     &        X(NTV),Y(NTV),Z(NTV),' is close (',offset,
     &        ') to edge ',IWHICH1,' &',IWHICH2,' of surface ',
     &        SNAME(ICOMP,ivj)
            call edisp(iuout,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
            if(NVER(ivj)+1.le.MV)then
              if(offset.le.CACC)then
                ok=.true.
              else
                dok=.true.
                helptopic='vertex_close_edge'
                call gethelptext(helpinsub,helptopic,nbhelp)
                call askok(
     &            'Insert this vertex into the adjacent surface?',
     &            '(see help)',ok,dok,nbhelp)
              endif
            else
              ok=.false.
            endif
            if(ok)then
              NVER(ivj)=NVER(ivj)+1
              IXV=NVER(ivj)+1
  148         continue
              IXV=IXV-1
              JVN(ivj,IXV)=JVN(ivj,IXV-1)
              IF(IXV.GT.ivjj+1)GOTO 148
              JVN(ivj,ivjj+1)=iwhich3

C Debug.
C                write(6,'(a,124i4)') 'now jvn is ',
C     &            (JVN(ivj,ii),ii=1,NVER(ivj))

C Surface (ivj) vertex list has been updated. Go on to next surface.
              goto 246 
            endif
          endif
  247   continue
  246 continue
      return
      end

C pointmergewithinclick ****************
C For use in click-on-bitmap to find if a point (px py pz) is close to
C one of the base edges in XX() YY()and calculate the coord (rx,ry,rz)
C if merged. Set ok 1 if px py pz was within ?mm of one of the
C edges.
      subroutine pointmergewithinclick(lastwalllist,xx,yy,px,py,pz,
     &  rx,ry,rz,iv1,iv2,ok)
#include "building.h"
C #include "model.h"
C #include "geometry.h"

      integer lastwalllist  ! how many historical points to look at
      real XX(MS),YY(MS)
      real px,py,pz,rx,ry,rz
      integer ok
      integer iv1,iv2 ! the position in the XX array which matched
      COMMON/OUTIN/IUOUT,IUIN

      character outs*248
      integer loop2
      logical match,tok,dok
      real toll

      toll=0.06  ! 60mm tollerance
 42   continue
 
C For the current list of XX YY and from last to first.
      DO loop2 = 1,lastwalllist-1
        xp1=XX(loop2); yp1=YY(loop2)
        zp1=0.0
        xp2=XX(loop2+1); yp2=YY(loop2+1)
        zp2=0.0
        call pointtoline3d(px,py,pz,xp1,yp1,zp1,xp2,yp2,zp2,
     &    offset,match)
        if(offset.lt.toll.and.match)then
          iv1=loop2; iv2=loop2+1
C          write(6,'(a,3f7.3,a,2i3,7f7.3,2i4)') 'might be',px,py,pz,' &',
C     &      iv1,iv2,xp1,yp1,zp1,xp2,yp2,zp2,offset
          write(outs,'(a,3f7.3,a,2i3,a,f6.3,a)') 'point ',px,py,pz,
     &      ' is between base v ',iv1,iv2,' with offset ',offset,'m'
          call edisp(iuout,outs)

C If the distance is greater than 1mm get the distance between the
C start of the line (iwhich1) and the test point (iwhich3) and do this
C for the end point on the line (iwhich2) and the test point.
          tdis= crowxyz(xp1,yp1,zp1,xp2,yp2,zp2)
          vdislsp=crowxyz(xp1,yp1,zp1,px,py,pz)

C Use square root of (vdislsp^2 - offset^2) 
          aligndis = SQRT((vdislsp * vdislsp) - (offset * offset))

C Use ratio calculation to make an aligned point vdislsp along the line.
          r2 = tdis - aligndis 
          r1 = aligndis
          rx = ((r2 * xp1) + (r1 * xp2))/tdis
          ry = ((r2 * yp1) + (r1 * yp2))/tdis
          rz = ((r2 * zp1) + (r1 * zp2))/tdis
          write(outs,'(a,3f10.5)')'New aligned vertex @ X,Y,Z:',
     &      rx,ry,rz
          call edisp(iuout,outs)
          ok=1    ! close enough so return
          return
        else
C          call edisp(iuout,
C     &      'Point not within 60mm of line so not re-aligned.')
          ok=0
        endif
      enddo  ! loop2
      return
      end

C clickobstructions *******************
      subroutine clickobstructions(icomp,fname,ixoffset,iyoffset,
     &  iwidth,ihight,inpm,iwmg,factor,viewlim,nw,ixtn1,ixtn2,
     &  iytn1,iytn2,ixo,iyo,completed,other,
     &  lastwalllist,xbmsnap,GRSPC,NBFREL,ier)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G1/G2/G4/G5/G7/prec17/precz/c20/GS5/GS6.
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      logical xbmnorth,xbmorigin  ! logical toggles for north origin
      logical xbmgrid,xbmscale    ! logical toggles for gridding and scale set
      logical trimmed ! if only part of bitmap is displayed
      common/disptoggle/xbmnorth,xbmorigin,xbmgrid,xbmscale,trimmed

      integer INVERT  ! number of coordinates in list
      real px,py,pz   ! user coordinates from clicks
      integer ipxx,ipyy,ipzz ! pixel coordinates from clicks
      common/clicklist/INVERT,px(MTV),py(MTV),pz(MTV),
     &  ipxx(MTV),ipyy(MTV),ipzz(MTV)

C Pixel (upper left and lower right corners) of where the bitmap is draw.
      integer ixbul,iybul,ixblr,iyblr ! x-up-left y-up-left x-lower-right y-lower-right
      common/bmlimits/ixbul,iybul,ixblr,iyblr

      logical xbmsnap,completed
      dimension viewlim(6)
      dimension GRSPC(3)
      logical clx0,cly0,clx1,cly1,clxm1,clym1
      logical modmlc  ! for selecting MLC
      LOGICAL dok,ok
      character outs*124,S12*12,temp6*6,fname*96,msg*72
      integer nbo
      real Z6,Z7    ! obs base elevation and height

#ifdef OSI
      integer iixx1,iiyy1,iid1,iid2
      integer iixx2,iiyy2,iixx3,iiyy3,iix,iiy
      integer iik
      integer iicol
#else
      integer*8 iixx1,iiyy1,iid1,iid2
      integer*8 iixx2,iiyy2,iixx3,iiyy3,iix,iiy
      integer*8 iik
      integer*8 iicol
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='clickonbitmap'  ! set for subroutine

      INPMODE=inpm

C Pick up the origin of the block and/or instruction to end the
C input or pan the bitmap.
 346  CALL trackview(iik,iixx1,iiyy1)
      ixx1=int(iixx1); iyy1=int(iiyy1)

C If user typed `e` then complete the process.
      if(iik.eq.69.or.iik.eq.101)goto 347

C Toggle on/off the snap-to grid.
      if(iik.eq.83.or.iik.eq.115)then
        if(xbmgrid)then
          if(xbmsnap)then
            xbmsnap=.false.
            call edisp(iuout,'snap-to grid is off...')
          else
            xbmsnap=.true.
            call edisp(iuout,'snap-to grid is on...')
          endif
        else
          call edisp(iuout,'no grid so ingnore `s` keypress...')
        endif
        goto 346
      endif

C If user typed `p` or `P` then do panning until user says done and then continue.
      if(iik.eq.80.or.iik.eq.112)then
        if(trimmed)then
 348      helptopic='click_pan_options'
          call gethelptext(helpinsub,helptopic,nbhelp)
          write(msg,'(a,i4,a,i4,a)') 
     &      'Panning options: (current offset x',ixoffset,' y ',
     &      iyoffset,' pixels)'
          IWM=1
          CALL EASKATOG(msg,' ','done','left','right','up','down',
     &      ' ',' ',IWM,nbhelp)
          if(iwm.eq.1.or.iwm.gt.5)then
            goto 346
          else
            inpm=inpmode
c            write(6,*) 'clickobstructions ctlbmpan',iwm,fname,ixoffset,
c     &        iyoffset,ibmwidth,ibmhight,iwidth,ihight,inpm,
c     &        nw,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo
            call ctlbmpan(iwm,fname,ixoffset,iyoffset,ibmwidth,
     &        ibmhight,iwidth,ihight,inpm,
     &        nw,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo)

C Refresh current bitmap and overlayed information.
            completed=.false.
            inpm=inpmode; lastwalllist=nw
            call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,
     &        inpm,iwmg,factor,viewlim,nw,ixtn1,ixtn2,iytn1,iytn2,
     &        ixo,iyo,completed,other,lastwalllist)
          endif
          goto 348
        endif
      endif

C Draw first point and pick up the next two points
      call esymbol(iixx1,iiyy1,24,1)
      CALL trackview(iik,iixx2,iiyy2)
      ixx2=int(iixx2); iyy2=int(iiyy2)
      call esymbol(iixx2,iiyy2,24,1)
      CALL trackview(iik,iixx3,iiyy3)
      ixx3=int(iixx3); iyy3=int(iiyy3)
      call esymbol(iixx3,iiyy3,24,1)

C Convert three points.
      tpx1=float(ixx1-ixo)/factor; tpy1=float(iyo-iyy1)/factor
      tpx2=float(ixx2-ixo)/factor; tpy2=float(iyo-iyy2)/factor
      tpx3=float(ixx3-ixo)/factor; tpy3=float(iyo-iyy3)/factor

C If snap-to is true then find nearest user unit and cast back to pixels.
      if(xbmsnap)then
        REMX1=AMOD(tpx1,GRSPC(1))
        REMY1=AMOD(tpy1,GRSPC(2))
        IF(ABS(REMX1).GT.GRSPC(1)/2.0)THEN
          if(tpx1.gt.0.0)then
            tpx1=tpx1+(GRSPC(1)-REMX1)
          elseif(tpx1.lt.0.0)then
            tpx1=tpx1-(GRSPC(1)-ABS(REMX1))
          else
            tpx1=tpx1+(GRSPC(1)-REMX1)
          endif
        ELSE
          tpx1=tpx1-REMX1
        ENDIF
        REMX2=AMOD(tpx2,GRSPC(1)); REMY2=AMOD(tpy2,GRSPC(2))
        IF(ABS(REMX2).GT.GRSPC(1)/2.0)THEN
          if(tpx2.gt.0.0)then
            tpx2=tpx2+(GRSPC(1)-REMX2)
          elseif(tpx2.lt.0.0)then
            tpx2=tpx2-(GRSPC(1)-ABS(REMX2))
          else
            tpx2=tpx2+(GRSPC(1)-REMX2)
          endif
        ELSE
          tpx2=tpx2-REMX2
        ENDIF
        REMX3=AMOD(tpx3,GRSPC(1)); REMY3=AMOD(tpy3,GRSPC(2))
        IF(ABS(REMX3).GT.GRSPC(1)/2.0)THEN
          if(tpx3.gt.0.0)then
            tpx3=tpx3+(GRSPC(1)-REMX3)
          elseif(tpx3.lt.0.0)then
            tpx3=tpx3-(GRSPC(1)-ABS(REMX3))
          else
            tpx3=tpx3+(GRSPC(1)-REMX3)
          endif
        ELSE
          tpx3=tpx3-REMX3
        ENDIF
        IF(ABS(REMY1).GT.GRSPC(2)/2.0)THEN
          if(tpy1.gt.0.0)then
            tpy1=tpy1+(GRSPC(2)-REMY1)
          elseif(tpy1.lt.0.0)then
            tpy1=tpy1-(GRSPC(2)-ABS(REMY1))
          else
            tpy1=tpy1+(GRSPC(2)-ABS(REMY1))
          endif
        ELSE
          tpy1=tpy1-REMY1
        ENDIF
        IF(ABS(REMY2).GT.GRSPC(2)/2.0)THEN
          if(tpy2.gt.0.0)then
            tpy2=tpy2+(GRSPC(2)-REMY2)
          elseif(tpy2.lt.0.0)then
            tpy2=tpy2-(GRSPC(2)-ABS(REMY2))
          else
            tpy2=tpy2+(GRSPC(2)-ABS(REMY2))
          endif
        ELSE
          tpy2=tpy2-REMY2
        ENDIF
        IF(ABS(REMY3).GT.GRSPC(2)/2.0)THEN
          if(tpy3.gt.0.0)then
            tpy3=tpy3+(GRSPC(2)-REMY3)
          elseif(tpy3.lt.0.0)then
            tpy3=tpy3-(GRSPC(2)-ABS(REMY3))
          else
            tpy3=tpy3+(GRSPC(2)-ABS(REMY3))
          endif
        ELSE
          tpy3=tpy3-REMY3
        ENDIF

        ixx1 = int(tpx1*factor) + ixo
        iyy1 = iyo - int(tpy1*factor)
        ixx2 = int(tpx2*factor) + ixo
        iyy2 = iyo - int(tpy2*factor)
        ixx3 = int(tpx3*factor) + ixo
        iyy3 = iyo - int(tpy3*factor)
      endif

C Remember the block origin.
      INVERT=INVERT+1
      ipxx(INVERT)=ixx1; ipyy(INVERT)=iyy1
      px(INVERT)=tpx1; py(INVERT)=tpy1; iicol=3
      call winscl('z',iicol)
      iid1=ipxx(INVERT); iid2=ipyy(INVERT);
      call esymbol(iid1,iid2,24,1)
      write(temp6,'(A)')'Org'
      iid1=ixx1+5; iid2=iyy1-5;
      CALL wstxpt(iid1,iid2,temp6)

C Remember the point at the end of the front edge.
      INVERT=INVERT+1
      ipxx(INVERT)=ixx2; ipyy(INVERT)=iyy2
      px(INVERT)=tpx2; py(INVERT)=tpy2; iicol=3
      call winscl('z',iicol)
      iid1=ipxx(INVERT); iid2=ipyy(INVERT);
      call esymbol(iid1,iid2,24,1)
      twid=crowxyz(tpx1,tpy1,Z1,tpx2,tpy2,Z1)

C Find orientation of line and from that the orientation of the blocks.
C Find which quadrant. Use logic from EREVEAL
      call ln2az(tpx1,tpy1,Z1,tpx2,tpy2,Z1,az,el)
      if(az.ge.0.0.and.az.le.270.0)then
        azim=az+90.0
      else
        azim=az-270.0
      endif
      elev=0.0
      call AZ2UV(azim,elev,vdx,vdy,vdz)

C Debug.
C      write(6,*) 'az azim vdx vdy vdz ',az,azim,vdx,vdy,vdz

C Check if tollerably close to an axis.
      CALL ECLOSE(vdx,0.0,0.001,clx0)
      CALL ECLOSE(vdy,0.0,0.001,cly0)
      CALL ECLOSE(vdx,1.0,0.001,clx1)
      CALL ECLOSE(vdy,1.0,0.001,cly1)
      CALL ECLOSE(vdx,-1.0,0.001,clxm1)
      CALL ECLOSE(vdy,-1.0,0.001,clym1)
      if(clx0.and.cly1)then
        RO=180.0
      elseif(clx1.and.cly0)then
        RO=90.0
      elseif(clx0.and.clym1)then
        RO= 0.0
      elseif(clxm1.and.cly0)then
        RO= (-90.0)
      elseif(vdx.gt.0.0.and.vdy.gt.0.0)then
        RO= 180.0 - azim
      elseif(vdx.gt.0.0.and.vdy.lt.0.0)then
        RO= 180.0 - azim
      elseif(vdx.lt.0.0.and.vdy.lt.0.0)then
        RO= 180.0 - azim
      elseif(vdx.lt.0.0.and.vdy.gt.0.0)then
        RO = (azim - 180.0) * (-1.)
      endif

      INVERT=INVERT+1
      ipxx(INVERT)=ixx3; ipyy(INVERT)=iyy3
      px(INVERT)=tpx3; py(INVERT)=tpy3; iicol=3
      call winscl('z',iicol)
      iid1=ipxx(INVERT); iid2=ipyy(INVERT);
      call esymbol(iid1,iid2,24,1)
      tdepth=crowxyz(tpx2,tpy2,Z1,tpx3,tpy3,Z1)
      iixx1=ixx1; iiyy1=iyy1; iixx2=ixx2; iiyy2=iyy2;
      iixx3=ixx3; iiyy3=iyy3;

      call edwline(iixx1,iiyy1,iixx2,iiyy2)
      call edwline(iixx2,iiyy2,iixx3,iiyy3)

C Try and complete the obstruction block by making a similar point
C offset from the origin. Depending on the direction of the vector
C between point 2 & 3 draw the two extra lines that complete the box.
      ioffx=iabs(ixx3-ixx2); ioffy=iabs(iyy3-iyy2)
      if(ixx3.eq.ixx2)then
        ioffx=0
        if(iyy3.eq.iyy2)then
          ioffy=0
        elseif(iyy3.lt.iyy2)then
          iid1=iyy1-ioffy
          call eswline(iixx1,iiyy1,iixx1,iid1)
          iid2=iyy1-ioffy
          call eswline(iixx1,iid2,iixx3,iiyy3)
        elseif(iyy3.gt.iyy2)then
          iid1=iyy1+ioffy
          call eswline(iixx1,iiyy1,iixx1,iid1)
          iid2=iyy1+ioffy
          call eswline(iixx1,iid2,iixx3,iiyy3)
        endif
      elseif(ixx3.lt.ixx2)then
        if(iyy3.eq.iyy2)then
          ioffy=0
          iid1=ixx1-ioffx
          call eswline(iixx1,iiyy1,iid1,iiyy1)
          iid2=ixx1-ioffx
          call eswline(iid2,iiyy1,iixx3,iiyy3)
        elseif(iyy3.lt.iyy2)then
          iid1=ixx1-ioffx; iid2=iyy1-ioffy
          call eswline(iixx1,iiyy1,iid1,iid2)
          iid1=ixx1-ioffx; iid2=iyy1-ioffy
          call eswline(iid1,iid2,iixx3,iiyy3)
        elseif(iyy3.gt.iyy2)then
          iid1=ixx1-ioffx; iid2=iyy1+ioffy
          call eswline(iixx1,iiyy1,iid1,iid2)
          iid1=ixx1-ioffx; iid2=iyy1+ioffy
          call eswline(iid1,iid2,iixx3,iiyy3)
        endif
      elseif(ixx3.gt.ixx2)then
        if(iyy3.eq.iyy2)then
          ioffy=0
          iid1=ixx1+ioffx
          call eswline(iixx1,iiyy1,iid1,iiyy1)
          iid1=ixx1+ioffx
          call eswline(iid1,iiyy1,iixx3,iiyy3)
        elseif(iyy3.lt.iyy2)then
          iid1=ixx1+ioffx; iid2=iyy1-ioffy
          call eswline(iixx1,iiyy1,iid1,iid2)
          iid1=ixx1+ioffx; iid2=iyy1-ioffy
          call eswline(iid1,iid2,iixx3,iiyy3)
        elseif(iyy3.gt.iyy2)then
          iid1=ixx1+ioffx; iid2=iyy1+ioffy
          call eswline(iixx1,iiyy1,iid1,iid2)
          iid1=ixx1+ioffx; iid2=iyy1+ioffy
          call eswline(iid1,iid2,iixx3,iiyy3)
        endif
      endif

C Debug.
C      write(6,*) ixx3,ixx2,ixx1,ioffx,ixx1+ioffx
C      write(6,*) iyy3,iyy2,iyy1,ioffy,iyy1+ioffy

      write(outs,'(a,i3,a,f8.3,a,f8.3,a,f7.3,a,f7.3,a,f7.3)') 
     &  'Block ',nbobs(icomp),' @ X',tpx1,' Y',tpy1,' width ',
     &  twid,'m and depth ',tdepth,'m @ aimuth ',RO
      call edisp(iuout,outs)
      call forceflush()
      helptopic='clickon_obstructions'
      call gethelptext(helpinsub,helptopic,nbhelp)
      dok=.true.
      CALL ASKOK(' ',
     &  'Is the block position ok (so we can continue)?',
     &  OK,dok,nbhelp)
      if(.NOT.ok)then
        INVERT=INVERT-3
        completed=.false.
        inpm=inpmode; lastwalllist=nw
        call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,inpm,
     &    iwmg,factor,viewlim,nw,ixtn1,ixtn2,
     &    iytn1,iytn2,ixo,iyo,completed,other,lastwalllist)
        goto 346
      endif
      nbobs(icomp)=nbobs(icomp)+1
      nbo=nbobs(icomp)   ! local counter
      NBFREL=NBFREL+3
      NOX(icomp)=20; NOZ(icomp)=20
      
C Ask for name based on the initial name given by the user.
      S12='blk_'
      CALL EASKS(S12,' ',' Name of obstruction? ',
     &  12,'obstruction','Block name',IER,nbhelp)
      IF(S12(1:2).NE.'  ')then
        BLOCKNAME(icomp,nbo)=S12
        LNBLOCKNAME(icomp,nbo)=lnblnk(S12)
      endif
      Z6=2.4; Z7=0.2; blkopaq=1.0 ! initial guesses
      CALL EASKR(Z6,' ','Elevation of the base of obstruction?',
     &  0.000,'W',99.9,'W',0.0,'elevation of obs base',IER,nbhelp)
      CALL EASKR(Z7,' ','Height of the obstruction?',
     &  0.000,'W',99.9,'W',0.0,'elevation of obs base',IER,nbhelp)
      CALL EASKR(blkopaq,' ',
     & 'Opacity of the obstruction 0=tran 1=opaque?',
     &  0.000,'W',1.0,'W',1.0,'opacity of obs base',IER,nbhelp)
      if(mlcver.eq.0)then
        CALL EPKMLC(ISEL,
     &    'Select an OPAQUE construction from the list to',
     &    'associate with the block for visualisation purposes.',IER)
      else
        call edisp(iuout,'Select an OPAQUE construction for block')
        CALL EDMLDB2(modmlc,'-',ISEL,IER)
      endif
      IF(ISEL.GT.0)then
        WRITE(BLOCKMAT(icomp,nbo),'(A)') mlcname(ISEL)
        LNBLOCKMAT(icomp,nbo)=lnblnk(BLOCKMAT(icomp,nbo))
      else
        WRITE(BLOCKMAT(icomp,nbo),'(A)') 'UNKNOWN'
        LNBLOCKMAT(icomp,nbo)=7
      endif
      XOB(icomp,nbo)=tpx1; YOB(icomp,nbo)=tpy1; ZOB(icomp,nbo)=Z6
      DXOB(icomp,nbo)=twid; DYOB(icomp,nbo)=tdepth; DZOB(icomp,nbo)=Z7
      BANGOB(icomp,nbo,1)=RO; BANGOB(icomp,nbo,2)=0.0
      BANGOB(icomp,nbo,3)=0.0
      OPOB(icomp,nbo)=blkopaq
      BLOCKTYP(icomp,nbo)='obs '
      call usrmsg(' ',' ','-')
      call edisp(iuout,' ')
      call edisp(iuout,
     &  'You can now pick points for next obstruction...')
      goto 346

  347 continue

C Since this will be saved with the zone geometry file only
C update IOBS.
      if(nbobs(icomp).gt.0)then
        IOBS(icomp)=2
        return
      endif
      return
      end
    

C clickfurn *******************
      subroutine clickfurn(icomp,fname,ixoffset,iyoffset,
     &  ibmwidth,ibmhight,iwidth,ihight,inpm,iwmg,factor,
     &  viewlim,nw,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo,completed,other,
     &  lastwalllist,xbmsnap,GRSPC,ier)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G1/G2/G4/G5/G7/prec17/precz/c20/GS5/GS6.
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN

C XYZ for up to 40 furniture points.
      integer nboffurnpts
      real furx,fury,furz  ! furniture origins
      integer ifurx,ifury  ! pixels for furniture origins
      common/furncord/nboffurnpts,furx(40),fury(40),furz(40),
     & ifurx(40),ifury(40)

      integer INVERT  ! number of coordinates in list
      real px,py,pz   ! user coordinates from clicks
      integer ipxx,ipyy,ipzz ! pixel coordinates from clicks
      common/clicklist/INVERT,px(MTV),py(MTV),pz(MTV),
     &  ipxx(MTV),ipyy(MTV),ipzz(MTV)

C Pixel (upper left and lower right corners) of where the bitmap is draw.
      integer ixbul,iybul,ixblr,iyblr ! x-up-left y-up-left x-lower-right y-lower-right
      common/bmlimits/ixbul,iybul,ixblr,iyblr

      logical xbmnorth,xbmorigin  ! logical toggles for north origin
      logical xbmgrid,xbmscale    ! logical toggles for gridding and scale set
      logical trimmed ! if only part of bitmap is displayed
      common/disptoggle/xbmnorth,xbmorigin,xbmgrid,xbmscale,trimmed

      logical xbmsnap,completed

      dimension viewlim(6)
      dimension GRSPC(3)
      LOGICAL dok,ok
      character outs*124,temp6*6,fname*96,msg*72
      real Z8    ! origin elevation

#ifdef OSI
      integer iixx1,iiyy1,iid1,iid2
      integer iik
      integer iicol
#else
      integer*8 iixx1,iiyy1,iid1,iid2
      integer*8 iik
      integer*8 iicol
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='clickonbitmap'  ! set for subroutine

      INPMODE=inpm

C Panning while inputting furniture is fragile so warn users.
      call edisp(iuout,
     &  'Avoid panning bitmap while defining furniture origins.')

C Pick up the furniture origin and/or instruction to end the
C input or pan the bitmap.
 346  CALL trackview(iik,iixx1,iiyy1)
      ixx1=int(iixx1); iyy1=int(iiyy1)

C If user typed `e` then complete the process.
      if(iik.eq.69.or.iik.eq.101)goto 347

C If user typed `d` or `D` then delete the previous coordinate.
      if(iik.eq.67.or.iik.eq.100)then
        INVERT=INVERT-1
        nboffurnpts=nboffurnpts-1
      endif

C Toggle on/off the snap-to grid.
      if(iik.eq.83.or.iik.eq.115)then
        if(xbmgrid)then
          if(xbmsnap)then
            xbmsnap=.false.
            call edisp(iuout,'snap-to grid is off...')
          else
            xbmsnap=.true.
            call edisp(iuout,'snap-to grid is on...')
          endif
        else
          call edisp(iuout,'no grid so ingnore `s` keypress...')
        endif
        goto 346
      endif

C If user typed `p` or `P` then do panning until user says done and then continue.
      if(iik.eq.80.or.iik.eq.112)then
        if(trimmed)then
 348      helptopic='click_pan_options'
          call gethelptext(helpinsub,helptopic,nbhelp)
          write(msg,'(a,i4,a,i4,a)') 
     &      'Panning options: (current offset x',ixoffset,' y ',
     &      iyoffset,' pixels)'
          IWM=1
          CALL EASKATOG(msg,' ','done','left','right','up','down',
     &      ' ',' ',IWM,nbhelp)
          if(iwm.eq.1.or.iwm.gt.5)then
            goto 346
          else
            inpm=inpmode
            call ctlbmpan(iwm,fname,ixoffset,iyoffset,ibmwidth,
     &        ibmhight,iwidth,ihight,inpm,
     &        nw,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo)

C Refresh current bitmap and overlayed information.
            completed=.false.
            inpm=inpmode; lastwalllist=nw
            call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,
     &        inpm,iwmg,factor,viewlim,nw,ixtn1,ixtn2,iytn1,iytn2,
     &        ixo,iyo,completed,other,lastwalllist)
          endif
          goto 348
        endif
      endif

C Draw the point.
      call esymbol(iixx1,iiyy1,26,1)

C Convert point.
      tpx1=float(ixx1-ixo)/factor; tpy1=float(iyo-iyy1)/factor

C If snap-to is true then find nearest user unit and cast back to pixels.
      if(xbmsnap)then
        REMX1=AMOD(tpx1,GRSPC(1))
        REMY1=AMOD(tpy1,GRSPC(2))
        IF(ABS(REMX1).GT.GRSPC(1)/2.0)THEN
          if(tpx1.gt.0.0)then
            tpx1=tpx1+(GRSPC(1)-REMX1)
          elseif(tpx1.lt.0.0)then
            tpx1=tpx1-(GRSPC(1)-ABS(REMX1))
          else
            tpx1=tpx1+(GRSPC(1)-REMX1)
          endif
        ELSE
          tpx1=tpx1-REMX1
        ENDIF
        IF(ABS(REMY1).GT.GRSPC(2)/2.0)THEN
          if(tpy1.gt.0.0)then
            tpy1=tpy1+(GRSPC(2)-REMY1)
          elseif(tpy1.lt.0.0)then
            tpy1=tpy1-(GRSPC(2)-ABS(REMY1))
          else
            tpy1=tpy1+(GRSPC(2)-ABS(REMY1))
          endif
        ELSE
          tpy1=tpy1-REMY1
        ENDIF

        ixx1 = int(tpx1*factor) + ixo
        iyy1 = iyo - int(tpy1*factor)
      endif

C Remember the furniture origin so it can be drawn on refreshes.
      INVERT=INVERT+1
      ipxx(INVERT)=ixx1; ipyy(INVERT)=iyy1
      px(INVERT)=tpx1; py(INVERT)=tpy1; iicol=3
      call winscl('z',iicol)
      iid1=ipxx(INVERT); iid2=ipyy(INVERT);
      call esymbol(iid1,iid2,24,1)
      write(temp6,'(A)')'FOrg'
      iid1=ixx1+5; iid2=iyy1-5;
      CALL wstxpt(iid1,iid2,temp6)

      if(nboffurnpts+1.gt.40)then
        call usrmsg('At limit of funiture origins.',
     &    'type `e` to end.','W')
        goto 346
      endif
      nboffurnpts=nboffurnpts+1
      furx(nboffurnpts)=tpx1; fury(nboffurnpts)=tpy1; iicol=3
      ifurx(nboffurnpts)=ixx1; ifury(nboffurnpts)=iyy1
      write(msg,'(a,2i3,a,f8.3,a,f8.3)') 'Furn origin',
     &  INVERT,nboffurnpts,' @ X',px(INVERT),' Y',py(INVERT)
      call edisp(iuout,msg)

      call forceflush()
      helptopic='clickon_furn'
      call gethelptext(helpinsub,helptopic,nbhelp)
      dok=.true.
      CALL ASKOK(' ',
     &  'Is the furniture position ok (so we can continue)?',
     &  OK,dok,nbhelp)
      if(.NOT.ok)then
        INVERT=INVERT-1
        completed=.false.
        inpm=inpmode; lastwalllist=nw
        call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,inpm,
     &    iwmg,factor,viewlim,nw,ixtn1,ixtn2,iytn1,iytn2,
     &    ixo,iyo,completed,other,lastwalllist)
        goto 346
      endif
      
C Ask for Z and then add vertex to zone structures.
      CALL EASKR(Z8,msg,'Elevation?',0.000,'W',
     &  99.9,'W',0.0,'elevation of furn origin',IER,nbhelp)
      furz(nboffurnpts)=Z8
      call usrmsg(' ',' ','-')
      call edisp(iuout,' ')
      call edisp(iuout,'Pick points for furniture or `e`...')
      goto 346

  347 continue
      return
      end
    

