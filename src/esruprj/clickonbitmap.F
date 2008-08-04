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

C ************* clickonbitmap
C Control interface for defining vertices or a floor plan extrusion
C via clicking on a bitmap.
      subroutine clickonbitmap(icomp,ier)
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)

C CTYPE (4 char) - zone shape type (REC, REG or GEN)
C gversion (real) version of the geometry file (1.0 legacy, 1.1 current).
      character CTYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G2/NGL(MS)
      COMMON/G4/NDP(MCOM),IDPN(MCOM,3)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),BANG(MB)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,12),IUZBASEA(MCOM),
     &  IZBASELIST(MCOM)
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C Where ESP-r was installed (as recorded when it was compiled).
      common/deflt4/dinstpath
      character dinstpath*48

      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GT/GTNAME
      COMMON/GTFIL/GTGEOM
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)
      common/cctlnm/ctldoc,lctlf
      common/cctl/icascf(mcom)

      DIMENSION ITEMS(21)
      DIMENSION XX(MS),YY(MS)

C px and py are the user coordinates for vertices (zone and/or ground),
C ipxx and ipyy are the pixel coordinates for each vertex.
C pxw and pyw are the user coordinates for floor plan extrusion,
C ipxw and ipyw are associated the pixel coordinates.
      dimension px(MGTV),py(MGTV),pz(MGTV)
      dimension ipxx(MGTV),ipyy(MGTV),ipzz(MGTV)
      dimension pxw(MTV),pyw(MTV),ipxw(MTV),ipyw(MTV)

C ipxwhis & ipywhis are previous pixel points for floor plan.
      dimension ipxwhis(300),ipywhis(300)

C iphisznst & iphisznfn are the historical points start and
C end indices for each zone.
      dimension iphisznst(MCOM),iphisznfn(MCOM)

      dimension VIEWLIM(6),GRSPC(3),IVALS(MCOM)

      CHARACTER*72 H
      CHARACTER ITEMS*33,outs*124,temp6*6,temp3*3,DESC*48
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      CHARACTER SSPARENT*12
      character phrase*124,WORD*48,msg*72,ltmp*72,fs*1
      character zname*12,zdesc*64,zn*12,BLKNAME*8,BLKMAT*12
      character ctldoc*248,LCTLF*72,zd*64,DFILE*72,CFILE*72,OFILE*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,cfgroot*24,LCFGF*72
      CHARACTER GMLCN*12,GSNAME*6,GTNAME*15,GTGEOM*72,t15*15,S8*8
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character longtfile*144,fname*96

      logical xbmorigin,xbmnorth,xbmscale,xbmgrid,xbmload,xbmsnap
      logical trimmed,completed,unique,CLOSEX,CLOSEY,addinganother
      logical clx0,cly0,clx1,cly1,clxm1,clym1
      logical unixok

C Signal first entry to subroutine with justentered as true, once
C an object has been saved the set justentered to false.
      logical justentered
      LOGICAL OKC,XST,dok,ok
      logical newgeo  ! to use for testing if new/old geometry file.

C Logical toggles: xbmorigin (required origin defined) xbmnorth (optional
C north defined), xbmscale (required scaling factor defined), xbmgrid
C (optional gridding defined), xbmload (has bitmap been loaded), trimmed
C has only part of bitmap been displayed, xbmsnap (snap to grid if xbmgrid
C is true).
      newgeo=.false.  ! assume older format geometry.
      xbmorigin=.false.
      xbmnorth=.false.
      xbmscale=.false.
      xbmgrid=.false.
      xbmload=.false.
      trimmed=.false.
      xbmsnap=.false.

C Set true to signal that user wants to add another zone.
      addinganother=.false.
      justentered=.true.

C fname (nname of the bitmap file, assumed to be in the current folder or
C specified by absolute address.
      fname=' '

C Input mode 
C   inpmode: 1 = new vertices with common Z,
C   inpmode: 2 = new vertices with different Z,
C   inpmode: 3 = corners for floor plan extrusion,
C   inpmode: 4 = south elevation (looking north)
C   inpmode: 5 = east elevation (looking west)
C   inpmode: 6 = ground topology,
C   inpmode: 7 = obstructions,
C   inpmode: 8 = nothing (not enough toggles set to do this yet) 
      inpmode=8

C Assumed initial number of walls for extruded floor
C plan case(NW), initial base Z (z1) initial top (z2).
      NW=0
      Z1=0.
      Z2=2.4
      Y1=0.0
      X1=0.0
      other=0.1

C NB is the number of obstruction blocks. NBL is number of
C historical points for prior blocks (NBL = NB*3 because each
C block is defined with an origin point, a point at end of
C the front face and a point at back right corner). 
      NBFREL=0
      NB=0

C Pixel (upper left and lower right corners) of where the bitmap was draw.
      ixbul=0
      iybul=0
      ixblr=0
      iyblr=0

C Defined width and height of bitmap and trimmed width and height.
      ibmwidth=0
      ibmhight=0
      iwidth=0
      ihight=0

C User requested pixel offsets (when panning).
      ixoffset=0
      iyoffset=0

C Two points defining north and angle from north.
      ixtn1=0
      iytn1=0
      ixtn2=0
      iytn2=0
      ang=0.0

C Scaling factor (pixels/m), bituwidth is the user units in the width
C of the bitmap, bituheight is the user units in the height of the
C bitmap. Uxoffset is the user coord at the left of the bitmap,
C uyoffset is the user coord at the bottom of the bitmap.
      factor=0.0
      bituwidth=0.0
      bituheight=0.0
      uxoffset=0.0
      uyoffset=0.0

C viewlim(1) and viewlim(2) are the user unit left and right boundaries and
C viewlim(3) and viewlim(4) are the lower and upper boundaries.
      viewlim(1)=0.0
      viewlim(2)=0.0
      viewlim(3)=0.0
      viewlim(4)=0.0
C VIEWLIM(6)     - current view limits (x1,x2,y1,y2,z1,z2)

C Gridding flag indicating the density of the grid.
      iwmg=0
      GRSPC(1)=0.0
      GRSPC(2)=0.0
      GRSPC(3)=0.0

C Vertices and pixels (clear list of pixels for vertices and corners).
      INVERT=0
      do 7 i=1,MGTV
        px(i)=0.0
        py(i)=0.0
        pz(i)=0.0
        ipxx(i)=0
        ipyy(i)=0
        ipzz(i)=0
  7   continue
      do 8 i=1,MTV
        pxw(i)=0.0
        pyw(i)=0.0
        ipxw(i)=0
        ipyw(i)=0
  8   continue

C Clear history of pixel points. nipwhis is the cummulative number
C of historical points.
      nipwhis=0
      do 9 i=1,300
        ipxwhis(i)=0
        ipywhis(i)=0
        if(i.le.MCOM)then
          iphisznst(i)=0
          iphisznfn(i)=0
        endif
  9   continue

C Warn user if icomp is zero, this implies that the clickonbitmap
C has been called from ground topology menu. If we have a zone then
C fill ZN and ZD so we know we can proceed with entering points. 
C Otherwise clear ZN and ZD.
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
      CALL win3d(menuchw,1,3,1,1,igl,igr,igt,igb,igw,igwh)

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
        WRITE(ITEMS(10+m+n),'(A,1X,i4)')  'k  nb. of vertices ',INVERT
        ITEMS(11+m+n)='                             '
        if(trimmed)then
          ITEMS(12+m+n)='m  start (type `e` end `p` pan)'
        else
          ITEMS(12+m+n)='m  start (type `e` to end)   '
        endif
      elseif(inpmode.eq.2)then
        ITEMS(8+m+n)='i mode >> pick vertices @ diff Z'
        ITEMS(9+m+n)='                             '
        WRITE(ITEMS(10+m),'(A,1X,i4)')  'k  nb. of vertices ',INVERT
        ITEMS(11+m+n)='                             '
        if(trimmed)then
          ITEMS(12+m+n)='m  start (type `e` end `p` pan)'
        else
          ITEMS(12+m+n)='m  start (type `e` to end)   '
        endif
      elseif(inpmode.eq.3)then
        ITEMS(8+m+n)='i mode >> pick floor plan'
        WRITE(ITEMS(9+m+n),'(A,1X,F8.4)')  'j  floor @  :',Z1
        WRITE(ITEMS(10+m+n),'(A,1X,F8.4)')  'k  ceiling @:',Z2
        WRITE(ITEMS(11+m+n),'(A,1X,i2)')    'l  nb. of walls ',NW
        if(trimmed)then
          ITEMS(12+m+n)='m  start (type `e` end `p` pan)'
        else
          ITEMS(12+m+n)='m  start (type `e` to end)   '
        endif
      elseif(inpmode.eq.4)then
        ITEMS(8+m+n)='i mode >> elevation (south)'
        WRITE(ITEMS(9+m+n),'(A,1X,F8.4)')'j  Y coordinate @',Y1
        WRITE(ITEMS(10+m+n),'(A,1X,i4)')  'k  nb. of vertices ',INVERT
        ITEMS(11+m+n)='                             '
        if(trimmed)then
          ITEMS(12+m+n)='m  start (type `e` end `p` pan)'
        else
          ITEMS(12+m+n)='m  start (type `e` to end)   '
        endif
      elseif(inpmode.eq.5)then
        ITEMS(8+m+n)='i mode >> elevation (east)'
        WRITE(ITEMS(9+m+n),'(A,1X,F8.4)')'j  X coordinate @',X1
        WRITE(ITEMS(10+m+n),'(A,1X,i4)')  'k  nb. of vertices ',INVERT
        ITEMS(11+m+n)='                             '
        if(trimmed)then
          ITEMS(12+m+n)='m  start (type `e` end `p` pan)'
        else
          ITEMS(12+m+n)='m  start (type `e` to end)   '
        endif
      elseif(inpmode.eq.6)then
        ITEMS(8+m+n)='i mode >> ground topology'
        ITEMS(9+m+n)='                             '
        WRITE(ITEMS(10+m+n),'(A,1X,i4)')  'k  nb. of vertices ',INVERT
        ITEMS(11+m+n)='                             '
        if(trimmed)then
          ITEMS(12+m+n)='m  start (type `e` end `p` pan)'
        else
          ITEMS(12+m+n)='m  start (type `e` to end)   '
        endif
      elseif(inpmode.eq.7)then
        ITEMS(8+m+n)='i mode >> obstructions'
        WRITE(ITEMS(9+m+n),'(A,1X,F8.4)')  'j  base @  :',Z1
        WRITE(ITEMS(10+m+n),'(A,1X,F8.4)')  'k  height:',Z2
        WRITE(ITEMS(11+m+n),'(A,1X,i2)')    'l  nb. of obs ',NB
        if(trimmed)then
          ITEMS(12+m+n)='m  start (type `e` end `p` pan)'
        else
          ITEMS(12+m+n)='m  start (type `e` to end)   '
        endif
      elseif(inpmode.eq.8)then
        ITEMS(8+m+n)='i mode >>                        '
        ITEMS(9+m+n) ='  . . .                         '
        ITEMS(10+m+n)='  . . .                         '
        ITEMS(11+m+n)='  . . .                         '
        ITEMS(12+m+n)='  . . .                         '
      endif
      ITEMS(13+m+n)=  '  _________________________     '

      ITEMS(14+m+n)=  '! list coordinates              '
      if(justentered)then
        ITEMS(15+m+n)=  '  . . .                       '
      else
        if(inpmode.eq.7)then
          ITEMS(15+m+n)='< create more obstructions    '
        elseif(inpmode.eq.6)then
          ITEMS(15+m+n)='< not applicable              '
        else
          ITEMS(15+m+n)='< create another zone         '
        endif
      endif
      if(inpmode.eq.6)then
        ITEMS(16+m+n)='> save ground topology          '
      else
        ITEMS(16+m+n)='  . . .                         '
      endif
      ITEMS(17+m+n)=  '? help                          '
      ITEMS(18+m+n)=  '- exit                          '
      MITEM=18+m+n

C Help text for this menu.
      H(1)='You can describe a thermal zone as an rectangle, a floor'
      H(2)='plan extrusion or a general polyhedra. Use only as much'
      H(3)='details as required to simplify attribution tasks, QA'
      H(4)='and interpretation tasks.'
      H(5)=' '
      H(6)='Points from a bitmap:                  '
      H(7)=' Zones, ground topology and obstructions can be defined'
      H(8)=' via points from a scanned image of a site plan or floor'
      H(9)=' plan or elevation or one of several predefined grids.'
      H(10)='                  '
      H(11)=' First scan the relevant source and convert it to an'
      H(12)=' XBM (X11 monochrome bitmap) file and place this in'
      H(13)=' the project `cfg` folder.'
      H(14)=' '
      h(15)=' Ensure that the bitmap is large enough to allow for'
      h(16)=' accurate positioning of the mouse. The bitmap can be'
      h(17)=' larger than the graphic display area (you can pan left'
      H(18)=' and right and up and down within it).'
      H(19)=' '
      H(20)=' The source image should indicate an origin (X=0.0'
      H(21)=' Y=0.0), the direction north (optional), and a'
      H(22)=' line of known length (to get scaling from).'
      H(23)='                  '
      H(24)='Menu options:     '
      H(25)=' Pan: if bitmap larger then display then you can show'
      H(26)='      a portion of the bitmap and pan around'
      H(27)=' Origin: identifies X=0.0 and Y=0.0 on the bitmap  '
      H(28)=' North: identifies angle between North and Y axis'
      H(29)='        (no translation/rotation is currently done!)'
      H(30)=' Scaling: select two points a known distance apart '
      H(31)='          (for best results pick a longer line).'
      H(32)=' Gridding: impose a grid and a `snap-to-grid` function.'
      H(33)='           '
      H(34)='Input modes:'
      H(35)='  Adding vertices (with same Z value or different Z) is'
      H(36)='  useful if you want to gather points and later link'
      H(37)='  the vertices to form surfaces.'
      H(38)='                  '
      H(39)='  Adding walls is the same as a floor plan extrusion.'
      H(40)='  You can input many zones in sequence. Note:'
      H(41)='  the snap to grid option is useful here.'
      H(42)='                  '
      H(43)='  Adding vertices for a south or east elevation is'
      H(44)='  supported. Later you can link them together to form'
      H(45)='  surfaces.'
      H(46)='                  '
      H(47)='  Adding vertices to the project ground topology is'
      H(48)='  supported. Later you can link them together to form'
      H(49)='  surfaces.'
      H(50)=' '
      H(51)='  Adding obstructions for the zones you have created'
      H(52)='  can save time. You will probably want to set the'
      H(53)='  gridding small before you start this.'
      H(54)=' '
      H(55)='Zones and obstructions can be saved or discarded after'
      H(56)='you type `e` but you must manually save ground topol.'
      H(57)=' '
      H(58)='HINT: this facility is designed for doing zones then'
      H(59)='obstructions in one session. '

      CALL EMENU('  Point definition',ITEMS,MITEM,INO)
  29  IF(INO.EQ.MITEM)THEN

C On exit, check save data depending on input mode.
        if(inpmode.eq.6)then

C Save any ground topology.
          dok=.false.
          h(1)='You were defining ground topology. This information'
          h(2)='might be lost if you exit without having saved first.'
          call askok('Have you saved the ground topology?',' ',ok,
     &      dok,2)
          if(ok)then
            return
          else
            goto 142
          endif
        elseif(inpmode.eq.8)then
          return
        else
          return
        endif
      ELSEIF(INO.EQ.MITEM-1)THEN

C Help.
        CALL PHELPD('bitmap opening',59,'-',0,0,IER)
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
        h(1)='Assuming the basic parameters (zone base and top height,'
        h(2)='gridding and shap options have been set you can proceed '
        h(3)='with adding points. Otherwise select continue and get'
        h(4)='the parameters sorted first. '
        call easkab('Options for adding objects (see help):',' ',
     &    'start clicking on points','continue',iwclear,4)
        if(iwclear.eq.2)then
          goto 142
        endif

C Clear and start new zone or ground topology or obstruction.
        if(inpmode.eq.6)then
          NGT=0
          NGTV=0
          H(1)='This file contains a description of a ground form'
          H(2)='for use by Radiance and viewing (later by esp). '
          if(gtgeom(1:4).eq.'UNKN'.or.gtgeom(1:2).eq.'  ')then
            write(ltmp,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.grnd'
          else
            ltmp=gtgeom
          endif
 56       CALL EASKS(ltmp,' Ground topology/geometry file name ? ',
     &      ' ',72,'grnd.geo','ground geom file',IER,2)
          if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
            gtgeom=ltmp
          else
            goto 56
          endif

C Get the name of the ground. Also set ZN to this to allow
C entry to point clicking.
          H(1)='This is a reminder of the ground type.'
          GTNAME='from_bitmap'
  49      t15=GTNAME
          CALL EASKS(t15,' Description of the ground?',
     &      ' ',15,'flat','not flat',IER,1)
          if(t15(1:2).ne.'  '.and.t15(1:4).ne.'UNKN')then
            GTNAME=t15
            ZN=t15(1:12)
          else
            goto 49
          endif
        elseif(inpmode.eq.7)then

C Clear current obstructions and setup for another zones obs.
          INVERT=0
          NBFREL=0
          NB=0
          NOX=MOX
          NOZ=MOZ
          NGX=10
          NGZ=10
          ZN='obstructions'
        else

C Before clearing current zone data save configuration.
          CALL EMKCFG('-',IER)

C After updating configuration, setup for another zone. Remember
C the last historical index if inpmode=3.
          if(inpmode.eq.1.or.inpmode.eq.2.or.inpmode.eq.4.or.
     &       inpmode.eq.5)then
            INVERT=0
          else if(inpmode.eq.3)then
            NW=0
          endif

C Creation of another new zone, first ask its name (ZN), trapping out
C illegal characters. Setup default file names (DFILE/CFILE/OFILE).
C Mark for increment ICOMP counter if the user decides to save the
C collected points as a zone.
          addinganother=.true.
          INCOMP=NCOMP+1
          call tstamp('>','PRJ: new zone')
          H(1)='The name of the zone is used both for descriptive'
          H(2)='and bookkeeping purposes.  Each name should be'
          H(3)='unique and should not contain blanks. '
 42       ZN=' '
          CALL EASKS(ZN,' What do you want to call this new zone ',
     &      ' ( <12 char, no blanks) ?',12,'new_zone','zone name',
     &      IER,3)
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

          H(1)='The zone description allows further information about'
          H(2)='the zone form and composition to be recorded.  '
          write(zd,'(2a)') zn(1:lnblnk(zn)),' describes a...'
 43       CALL EASKS(zd,' What does it represent ',
     &  ' ( <64 char) ?',64,'no description entered','zone descr',IER,2)
          IF(zd.eq.' '.or.ier.ne.0)goto 43
        endif
        call edisp(iuout,'BEGIN ENTERING POINTS NOW...')
        INO=12+m+n
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
            jj=0
            do 118 j=1,NBFREL
              jj=jj+1
              if(jj.eq.1)then
                write(outs,'(a,f8.3,a,f8.3,a,2i5)') 'blk org @X',
     &            px(j),' Y',py(j),' and pixel: ',ipxx(j),ipyy(j)
              elseif(jj.eq.2)then
                write(outs,'(a,f8.3,a,f8.3,a,2i5)') 'fr edge @X',
     &            px(j),' Y',py(j),' and pixel: ',ipxx(j),ipyy(j)
              elseif(jj.eq.3)then
                write(outs,'(a,f8.3,a,f8.3,a,2i5)') 'bk edge @X',
     &            px(j),' Y',py(j),' and pixel: ',ipxx(j),ipyy(j)
                jj=0
              endif
              call edisp(iuout,outs)
  118       continue
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
     &          ' @ X',pxw(j),' Y',pyw(j)
            else
              tdis= crowxyz(pxw(j-1),pyw(j-1),0.0,pxw(j),pyw(j),0.0)
              call ln2az(pxw(j-1),pyw(j-1),0.0,pxw(j),pyw(j),0.0,az,el)
              write(outs,'(a,i3,a,f8.3,a,f8.3,a,f7.3,a,f8.3,a,f7.3)') 
     &          'Corner ',j,' @ X',pxw(j),' Y',pyw(j),' distance ',
     &          tdis,'m and @ aimuth ',az,' & elev ',el
            endif
            call edisp(iuout,outs)
  123     continue
        endif
      elseif(INO.EQ.1)then

C Load an X11 bitmap file (user required to convert to the format).
        h(1)='Please convert any bitmap file to XBM format '
        h(2)='and ensure that the graphic feedback area is large '
        h(3)='enough for it to display properly and that the bitmap'
        h(4)='is large enough to allow for accurate positioning of '
        h(5)='the mouse. '
        h(6)=' '
        h(7)='If you just want to use a blank bitmap or a bitmap with'
        h(8)='tickmarks to quickly input zones and/or obstructions'
        h(9)='then these are also available. '
        idno=7
        call MENUATOL('Bitmap options: (see help)',' ',
     &    'a small for gridding','b small with ticks for gridding',
     &    'c medium for gridding','d medium with ticks for gridding',
     &    'e large for gridding','f large with ticks for gridding',
     &    'g user defined bitmap',' ',' ',' ',' ',' ',inptb,idno,9)

C << the xbm files should be located via the installed path >>
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
        else
          goto 142
        endif
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        write(longtfile,'(a)') fname(1:lnblnk(fname))
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(fname,'X11 bitmap file name?',
     &      ' ',96,'xxx.xbm','bitmap file',IER,3)
          write(longtfile,'(a)') fname(1:lnblnk(fname))
        elseif(iglib.eq.2)then
          CALL EASKF(longtfile,'X11 bitmap file name?',
     &      ' ',144,'xxx.xbm','bitmap file',IER,3)
          write(fname,'(a)') longtfile(1:95)
        else
          CALL EASKF(fname,'X11 bitmap file name?',
     &      ' ',96,'xxx.xbm','bitmap file',IER,3)
        endif

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
              CALL EGETWI(phrase,K,ibmwidth,1,1200,'W','bitmap w',IER)
              read(iin,'(a)',iostat=ios,err=99) phrase
              if(phrase(1:8).eq.'#define ')then
                K=8
                CALL EGETW(phrase,K,WORD,'W','bitmap name',IFLAG)
                CALL EGETWI(phrase,K,ibmhight,1,1200,'W','bitmap h',
     &            IER)
              endif

C Debug.
              write(outs,*) 'The bitmap file is ',ibmwidth,' by ',
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
                write(outs,*) ' The bitmap was trimmed to ',iwidth,
     &            ' x',ihight,' pixels from ',ibmwidth,' x',ibmhight
                call edisp(iuout,outs)
                call winlodpart(fname,ixoffset,iyoffset,iwidth,ihight,
     &            15,15,ixbul,iybul,ixblr,iyblr)
              else
                trimmed=.false.
                ixoffset=0
                iyoffset=0
                iwidth=ibmwidth
                ihight=ibmhight
                call winlodpart(fname,ixoffset,iyoffset,iwidth,ihight,
     &            10,10,ixbul,iybul,ixblr,iyblr)
              endif

C After loading the bitmap populate the display with corners, north,
C origin if these have been defined.
C Debug.
C              write(6,*) 'bitmap drawn at ',ixbul,iybul,ixblr,iyblr
              call winscl('z',0)
              call esymbol(ixbul,iybul,6,1)
              call esymbol(ixblr,iyblr,4,1)
              call winscl('-',0)
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
        call edisp(iuout,' a) define an origin')
        call edisp(iuout,' b) define north (optional)')
        call edisp(iuout,' c) define scale')
        call edisp(iuout,' d) toggle on grid (optional)')
        call edisp(iuout,' e) toggle on grid-snap (optional)')
        call edisp(iuout,' f) set the input mode')
        goto 142
      elseif(INO.EQ.2.and.m.eq.1)then

C Support bitmap panning - left, right, up, down.
        if(.NOT.trimmed) goto 142
  199   h(1)='The view of a truncated bitmap can be panned left '
        h(2)='and right and up and down (by increments of 50 pixels)'
        write(msg,'(a,i4,a,i4,a)') 
     &    'Panning options: (current offset x',ixoffset,' y ',
     &    iyoffset,' pixels)'
        CALL EASKATOG(msg,' ','done','left','right','up','down',' ',
     &    ' ',IWM,2)
        if(iwm.eq.1.or.iwm.gt.5)then
          goto 142
        else
          inpm=inpmode
          call ctlbmpan(iwm,fname,ixoffset,iyoffset,ibmwidth,ibmhight,
     &      iwidth,ihight,inpm,ixbul,iybul,ixblr,iyblr,xbmnorth,
     &      xbmorigin,invert,nw,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo,
     &      ipxx,ipyy,ipzz,ipxw,ipyw,nipwhis,ipxwhis,ipywhis)

C Refresh current bitmap and overlayed information.
          completed=.true.
          inpm=inpmode
          call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,inpm,
     &      ixbul,iybul,ixblr,iyblr,xbmnorth,xbmorigin,xbmgrid,
     &      xbmscale,iwmg,factor,viewlim,invert,nw,ixtn1,ixtn2,
     &      iytn1,iytn2,ixo,iyo,ipxx,ipyy,ipzz,ipxw,ipyw,nipwhis,
     &      ipxwhis,ipywhis,iphisznst,iphisznfn,completed,other)
          goto 199
        endif
      elseif((INO.EQ.2.and.m.eq.0.and.n.eq.1).or.
     &       (INO.EQ.3.and.m.eq.1.and.n.eq.1))then

C Refresh current bitmap and overlayed information.
        completed=.true.
        inpm=inpmode
        call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,inpm,
     &    ixbul,iybul,ixblr,iyblr,xbmnorth,xbmorigin,xbmgrid,
     &    xbmscale,iwmg,factor,viewlim,invert,nw,ixtn1,ixtn2,iytn1,
     &    iytn2,ixo,iyo,ipxx,ipyy,ipzz,ipxw,ipyw,nipwhis,ipxwhis,
     &    ipywhis,iphisznst,iphisznfn,completed,other)
      elseif(INO.EQ.2+m+n)then

C Origin within the bitmap.
        if(xbmorigin)then
          dok=.false.
          h(1)='You previously defined an orgin on the bitmap.'
          h(2)='If you want to change it say ok. '
          CALL ASKOK(' ',' Redefine origin? ',OK,dok,2)
          if(OK) xbmorigin = .false.
        endif
        if(.NOT.xbmorigin)then
          call edisp(iuout,' ')
          call usrmsg('Select origin via cursor (click left button)',
     &      ' ','-')
          call winscl('z',0)
          CALL trackview(ii,ixo,iyo)
          call esymbol(ixo,iyo,24,1)
          write(temp6,'(A)')'Origin'
          CALL wstxpt(ixo+5,iyo-5,temp6)
          call winscl('-',0)
          write(outs,'(a,2i6)')' origin X Y pixels =',ixo,iyo
          call edisp(iuout,outs)
          call usrmsg(' ',' ','-')
          xbmorigin=.true.
        endif
        goto 142
      elseif(INO.EQ.3+m+n)then

C North.
        if(xbmnorth)then
          dok=.false.
          h(1)='You previously defined north on the bitmap.'
          h(2)='If you want to change it say ok. '
          CALL ASKOK(' ',' Redefine north? ',OK,dok,2)
          if(OK) xbmnorth = .false.
        endif
        if(.NOT.xbmnorth)then
          call edisp(iuout,' ')
          call usrmsg('Select two points defining north axis... ',
     &      ' ','-')
          call winscl('z',0)
          CALL trackview(ii,ixtn1,iytn1)
          call esymbol(ixtn1,iytn1,11,1)
          CALL trackview(ii,ixtn2,iytn2)
          call esymbol(ixtn2,iytn2,18,1)
          call edwline(ixtn1,iytn1,ixtn2,iytn2)
          write(temp6,'(A)')'North'
          CALL wstxpt(ixtn2+5,iytn2-5,temp6)
          call winscl('-',0)
          call usrmsg(' ',' ','-')
          xbmnorth=.true.
        endif
        goto 142
      elseif(INO.EQ.4+m+n)then

C Scaling of a known line. Ask for two points and distance.
        if(xbmscale)then
          dok=.false.
          h(1)='You previously defined scaling points on the bitmap.'
          h(2)='If you want to change this say ok. '
          CALL ASKOK(' ',' Redefine scale? ',OK,dok,2)
          if(OK) xbmscale = .false.
        endif
        if(.NOT.xbmscale)then
          call edisp(iuout,' ')
          call usrmsg('Define the bitmap scale by selecting ',
     &      'two points along a known line... ','-')
          call winscl('z',0)
          CALL trackview(ii,ixs1,iys1)
          call esymbol(ixs1,iys1,11,1)
          CALL trackview(ii,ixs2,iys2)
          call esymbol(ixs1,iys2,11,2)
          call winscl('-',0)
          call usrmsg(' ',' ','-')
          disn=1.
          CALL EASKR(disn,' ','Distance (metres) between points?',
     &      0.0001,'W',0.,'-',0.1,'dist',IER,1)
          vdis= crowxyz(float(ixs1),float(iys1),0.0,float(ixs2),
     &      float(iys2),0.0)
          factor=vdis/disn
          write(outs,*)' There are ',factor,' pixels per metre.'
          call edisp(iuout,outs)
          call winscl('z',0)
          call eswline(ixs1,iys1,ixs2,iys2)

C Try test point along screen Y and check for angle between it and the
C point nominated. No transform is yet applied.
          call ang3vtx(float(ixtn1),float(iytn1-50),0.,float(ixtn1),
     &      float(iytn1),0.,float(ixtn2),float(iytn2),0.0,ang)
          if(ang.gt.1.5)then
            call eswline(ixtn1,iytn1,ixtn2,iytn2)
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
          pxs=float(ixs1-ixo)/factor
          pys=float(iyo-iys1)/factor
          write(outs,'(a,f8.3,a,f8.3)') 'First scale point at X',pxs,
     &      ' Y ',pys
          call edisp(iuout,outs)
          pxs=float(ixs2-ixo)/factor
          pys=float(iyo-iys2)/factor
          write(outs,'(a,f8.3,a,f8.3)') '2nd scale point at X',pxs,
     &      ' Y ',pys
          call edisp(iuout,outs)

C Using the upper left ixbul,iybul and lower right ixblr,iyblr calculate
C the extents of the bitmap in user units. bituwidth is the user units
C in the width of the bitmap, Uxoffset is the user coord at the left of the bitmap,
C uyoffset is the user coord at the bottom of the bitmap.
          bituwidth=float(ixblr-ixbul)/factor
          bituheight=float(iyblr-iybul)/factor
          uxoffset=float(ixbul-ixo)/factor
          uyoffset=float(iyo-iyblr)/factor

C For purposes of drawing a grid find the nearest metre in the X and Y
C where viewlim(1) and viewlim(2) are the left and right boundaries and
C where viewlim(3) and viewlim(4) are the lower and upper boundaries.
          viewlim(1)=float(int(uxoffset))
          viewlim(2)=real(anint(viewlim(1) + bituwidth))
          viewlim(3)=float(int(uyoffset))
          viewlim(4)=real(anint(viewlim(3) + bituheight))
          xbmscale=.true.
          call winscl('z',0)
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
          h(1)='You previously defined gridding for the bitmap.'
          h(2)='If you want to change this say ok. '
          CALL ASKOK(' ',' Redefine grid? ',OK,dok,2)
          if(OK) xbmgrid = .false.
        endif
        if(.NOT.xbmgrid)then
          h(1)='A grey grid will overlay the bitmap from corner to '
          h(2)='corner. '
          CALL EASKATOG('Gridding options:',' ',
     &      'none','2.0m','1.0m','0.5m','0.25m','0.1m','other',IWMG,2)
          if(iwmg.eq.1)then
            GRSPC(1)=0.0
            GRSPC(2)=0.0
            GRSPC(3)=0.0
            xbmgrid=.false.
          elseif(iwmg.eq.2)then
            GRSPC(1)=2.0
            GRSPC(2)=2.0
            GRSPC(3)=2.0
            xbmgrid=.true.
          elseif(iwmg.eq.3)then
            GRSPC(1)=1.0
            GRSPC(2)=1.0
            GRSPC(3)=1.0
            xbmgrid=.true.
          elseif(iwmg.eq.4)then
            GRSPC(1)=0.5
            GRSPC(2)=0.5
            GRSPC(3)=0.5
            xbmgrid=.true.
          elseif(iwmg.eq.5)then
            GRSPC(1)=0.25
            GRSPC(2)=0.25
            GRSPC(3)=0.25
            xbmgrid=.true.
          elseif(iwmg.eq.6)then
            GRSPC(1)=0.1
            GRSPC(2)=0.1
            GRSPC(3)=0.1
            xbmgrid=.true.
          elseif(iwmg.eq.7)then
            ZDI=0.1
            H(1)='Enter a value between 0.1 and 1.0.'
            CALL EASKR(ZDI,' ',' Distance between grid points?',
     &      0.100,'W',1.0,'W',1.0,'grid point distance',IER,1)
            GRSPC(1)=ZDI
            GRSPC(2)=ZDI
            GRSPC(3)=ZDI
            other=ZDI
            xbmgrid=.true.
          endif
          completed=.true.
          inpm=inpmode
          call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,inpm,
     &      ixbul,iybul,ixblr,iyblr,xbmnorth,xbmorigin,xbmgrid,
     &      xbmscale,iwmg,factor,viewlim,invert,nw,ixtn1,ixtn2,iytn1,
     &      iytn2,ixo,iyo,ipxx,ipyy,ipzz,ipxw,ipyw,nipwhis,ipxwhis,
     &      ipywhis,iphisznst,iphisznfn,completed,other)
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
        H(1)='Input modes:'
        H(2)='  Adding vertices (with same Z value) is useful if you'
        H(3)='  want to gather information from the bitmap and later'
        H(4)='  link the vertices to form surfaces.'
        H(5)='                  '
        H(6)='  Adding vertices (with different Z values) is useful'
        H(7)='  if you want to capture information at several levels'
        H(8)='  (e.g. contours on a site plan)'
        H(9)='  '
        H(10)='  Adding walls via a floor plan extrusion. Remember to'
        H(11)='  select the `corners` anti-clockwise.'
        H(12)='  '
        H(13)='  South elevation fills X & Z coords @ one Y.'
        H(14)='  East elevation  fills Y & Z coords @ one X.'
        H(15)='  '
        H(16)='  Ground toplogy will ask for Z value for each point.'
        H(17)='  '
        H(18)='  Obstructions asks for 2 points defining front and'
        H(19)='  one point perpendicular as well as base and top.'
        H(20)='  '
        H(21)='If gridding is on and snap-to is on then the selected'
        H(22)='coordinate may be relocated to the nearest point. '
        inpt=inpmode
        if(inpt.eq.1) msg='Options (currently vertices @ one Z)'
        if(inpt.eq.2) msg='Options (currently vertices @ various Z)'
        if(inpt.eq.3) msg='Options (currently floor plan extrusion)'
        if(inpt.eq.4) msg='Options (currently south elevation)'
        if(inpt.eq.5) msg='Options (currently east elevation)'
        if(inpt.eq.6) msg='Options (currently ground topology)'
        if(inpt.eq.7) msg='Options (currently shading obstructions)'
        if(inpt.eq.8) msg='Options (currently awaiting data)'
        ilno=inpmode
        idno=3
        call MENUATOL(msg,' Coordinate input options',
     &    'a vertices @ one Z','b vertices @ various Z',
     &    'c floor plan extrusion','d south elevation',
     &    'e east elevation','f ground topology',
     &    'g shading obstructions','h awaiting data',' ',' ',' ',
     &    ' ',inpt,idno,22)

C If changing the input mode confirm with user what to do with prior
C data (if moving from vertices to floor plan extrusion or from floor
C plan extrusion to vertices). Note: before clickonbitmap was called
C to create a zone the user will have nominated an initial zone name
C and description and so the user is not prompted for this. Obstructions
C and ground topology do require intial information.
        if(inpt.ne.inpmode)then
          justentered=.true.
          if(nipwhis.gt.0)then
            dok=.false.
            h(1)='If you previously were defining points via floor '
            h(2)='plans the history of these points can be cleared '
            h(3)='if you say ok. '
            CALL ASKOK(
     &        'Clear history of floor plans before defining new',
     &        'vertices? (previous zones will not show on bitmap)',
     &        OK,dok,3)
              if(OK)nipwhis=0
          endif
          if(inpt.eq.1)then
            INVERT=0
            inpmode = inpt
          elseif(inpt.eq.2)then
            INVERT=0
            inpmode = inpt
          elseif(inpt.eq.3)then
            INVERT=0
            NW=0
            inpmode = inpt
          elseif(inpt.eq.4)then
            INVERT=0
            inpmode = inpt
          elseif(inpt.eq.5)then
            INVERT=0
            inpmode = inpt
          elseif(inpt.eq.6)then
            NW=0
            INVERT=0
            NGT=0
            NGTV=0
            H(1)='This file contains a description of a ground form'
            H(2)='for use by Radiance and viewing (later by esp). '
            if(gtgeom(1:4).eq.'UNKN'.or.gtgeom(1:2).eq.'  ')then
              write(ltmp,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.grnd'
            else
              ltmp=gtgeom
            endif
 46         CALL EASKS(ltmp,' Ground topology/geometry file name ? ',
     &        ' ',72,'grnd.geo','ground geom file',IER,2)
            if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
              gtgeom=ltmp
            else
              goto 46
            endif
            H(1)='This is a reminder of the ground type.'
            GTNAME='from_bitmap'
  39        t15=GTNAME
            CALL EASKS(t15,' Description of the ground?',
     &        ' ',15,'flat','not flat',IER,1)
            if(t15(1:2).ne.'  '.and.t15(1:4).ne.'UNKN')then
              GTNAME=t15
            else
              goto 39
            endif
            inpmode = inpt
          elseif(inpt.eq.7)then
            NW=0
            INVERT=0
            NBFREL=0
            NB=0
            NOX=MOX
            NOZ=MOZ
            NGX=10
            NGZ=10
            inpmode = inpt
          elseif(inpt.eq.8)then
            continue
          endif
        endif
        goto 142
      elseif(INO.EQ.9+m+n)then
        if(inpmode.eq.1)then

C Z coordinate to be associated with all points.
          Z1=0.
          H(1)='The elevation is in metres.'
          CALL EASKR(Z1,' ',' Elevation of vertices to be selected?',
     &      0.000,'W',99.9,'W',0.0,'elevation of verts',IER,1)
        elseif(inpmode.eq.3)then

C Floor height.  User begins with an extruded shape.
          Z1=0.
          H(1)='The base (floor) elevation is in metres.'
          CALL EASKR(Z1,' ',' Elevation of the base surface? ',
     &      0.000,'W',99.9,'W',0.0,'elevation of base',IER,1)
        elseif(inpmode.eq.4)then

C Y coordinate to be associated with all points.
          Y1=0.
          H(1)='The south elevation has all the same'
          H(2)='Y values (m).'
          CALL EASKR(Y1,' ','Y value for south elevation?',
     &      -99.000,'W',99.9,'W',0.0,'Y value for elevation',IER,2)
        elseif(inpmode.eq.5)then

C X coordinate to be associated with all points.
          X1=0.
          H(1)='The east elevation has all the same'
          H(2)='X values (m).'
          CALL EASKR(X1,' ','X value for east elevation?',
     &      -99.000,'W',99.9,'W',0.0,'X value for elevation',IER,2)
        elseif(inpmode.eq.7)then

C Base of obstruction..
          Z1=0.
          H(1)='The base elevation of obstruction is in metres.'
          CALL EASKR(Z1,' ',' Elevation of the base of obstruction?',
     &      0.000,'W',99.9,'W',0.0,'elevation of obs base',IER,1)
        endif
        goto 142
      elseif(INO.EQ.10+m+n)then
        if(inpmode.eq.3)then

C Ceiling height.
          Z2=2.4
          H(1)='The top surface height is along the Z axis (metres).'
          H(2)='Note: this is not the base-top distance!'
          CALL EASKR(Z2,' ',' Elevation of the top surface? ',
     &      0.000,'W',99.9,'W',2.7,'elevation of top',IER,2)
        elseif(inpmode.eq.7)then

C Top of obstruction..
          Z2=1.
          H(1)='The height of obstruction is in metres.'
          CALL EASKR(Z2,' ',' Height (top-bottom) of obstruction?',
     &      0.000,'W',99.9,'W',0.0,'height of obs top',IER,1)
        elseif(inpmode.eq.8)then
          continue
        endif
        goto 142
      elseif(INO.EQ.11+m+n)then
        goto 142
      elseif(INO.EQ.12+m+n)then

C First check to see if there is a non-blank ZN.
        if(ZN(1:2).eq.'  ')then
          dok=.false.
          h(1)='You might have selected this option by mistake. ESP-r'
          h(2)='is expecting to have found a name for a zone at this'
          h(3)='point. Probably you ment to select the `add another` '
          h(4)='option first. '
          call askok(
     &      'No zone name found. Are you sure you want to proceed?',
     &      '(you might not have picked `add another` first)',ok,dok,4)
          if(.NOT.ok) goto 142
        endif

C Start identifying vertices for the input modes which gather
C zone or ground vertices.
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
            CALL trackview(ii,ixx,iyy)
          elseif(inpmode.eq.4)then
            CALL trackview(ii,ixx,izz)
          elseif(inpmode.eq.5)then
            CALL trackview(ii,iyy,izz)
          endif

C If user typed `e` then complete the process.
          if(ii.eq.69.or.ii.eq.101)goto 147

C Toggle on/off the snap-to grid.
          if(ii.eq.83.or.ii.eq.115)then
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
          if(ii.eq.80.or.ii.eq.112)then
            if(trimmed)then
 148          h(1)='The view of a truncated bitmap can be panned left'
              h(2)='and right and up and down (by 50 pixels)'
              write(msg,'(a,i4,a,i4,a)') 
     &          'Panning options: (current offset x',ixoffset,' y ',
     &          iyoffset,' pixels)'
              CALL EASKATOG(msg,' ','done','left','right','up','down',
     &          ' ',' ',IWM,2)
              if(iwm.eq.1.or.iwm.gt.5)then
                goto 146
              else
                inpm=inpmode
                call ctlbmpan(iwm,fname,ixoffset,iyoffset,ibmwidth,
     &            ibmhight,iwidth,ihight,inpm,ixbul,iybul,ixblr,iyblr,
     &            xbmnorth,xbmorigin,invert,nw,ixtn1,ixtn2,iytn1,
     &            iytn2,ixo,iyo,ipxx,ipyy,ipzz,ipxw,ipyw,nipwhis,
     &            ipxwhis,ipywhis)

C Refresh current bitmap and overlayed information.
                completed=.false.
                inpm=inpmode
                call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,
     &            inpm,ixbul,iybul,ixblr,iyblr,xbmnorth,xbmorigin,
     &            xbmgrid,xbmscale,iwmg,factor,viewlim,invert,nw,
     &            ixtn1,ixtn2,iytn1,iytn2,ixo,iyo,ipxx,ipyy,ipzz,
     &            ipxw,ipyw,nipwhis,ipxwhis,ipywhis,iphisznst,
     &            iphisznfn,completed,other)
              endif
              goto 148
            endif
          endif

C If user typed `d` or `D` then delete the latest coordinate. Then
C refresh current bitmap and overlayed information.
          if(ii.eq.67.or.ii.eq.100)then
            if(INVERT.gt.1)then
              INVERT=INVERT-1
              call edisp(iuout,'Last vertex deleted. Refreshing...')
              completed=.false.
              inpm=inpmode
              call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,
     &          inpm,ixbul,iybul,ixblr,iyblr,xbmnorth,xbmorigin,
     &          xbmgrid,xbmscale,iwmg,factor,viewlim,invert,nw,
     &          ixtn1,ixtn2,iytn1,iytn2,ixo,iyo,ipxx,ipyy,ipzz,ipxw,
     &          ipyw,nipwhis,ipxwhis,ipywhis,iphisznst,iphisznfn,
     &          completed,other)
              goto 146
            endif
          endif

C If snap-to is true then find nearest user unit and cast back to pixels.
          if(inpmode.eq.1.or.inpmode.eq.2.or.inpmode.eq.6)then
            tpx=float(ixx-ixo)/factor
            tpy=float(iyo-iyy)/factor
          elseif(inpmode.eq.4)then
            tpx=float(ixx-ixo)/factor
            tpy=float(iyo-izz)/factor
          elseif(inpmode.eq.5)then
            tpx=float(iyy-ixo)/factor
            tpy=float(iyo-izz)/factor
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
              ixx = int(tpx*factor) + ixo
              iyy = iyo - int(tpy*factor)
            elseif(inpmode.eq.4)then
              ixx = int(tpx*factor) + ixo
              izz = iyo - int(tpy*factor)
            elseif(inpmode.eq.5)then
              iyy = int(tpx*factor) + ixo
              izz = iyo - int(tpy*factor)
            endif
          endif

C Find if this vertex is unique, if it is not then jump back and select
C another pixel.
          if(INVERT.gt.1)then
            unique=.true.
            do 76,ij=1,INVERT
              CLOSEX=.false.
              CLOSEY=.false.
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

C Increment number of vertices, add to ipxx and ipyy arrays, change colour
C to green draw the symbol and index of vertex and do Z editing if requested.
          INVERT=INVERT+1
          if(inpmode.eq.1.or.inpmode.eq.2.or.inpmode.eq.6)then
            ipxx(INVERT)=ixx
            ipyy(INVERT)=iyy
            px(INVERT)=tpx
            py(INVERT)=tpy
            call winscl('z',3)
            call esymbol(ipxx(INVERT),ipyy(INVERT),24,1)
          elseif(inpmode.eq.4)then
            ipxx(INVERT)=ixx
            ipzz(INVERT)=izz
            px(INVERT)=tpx
            pz(INVERT)=tpy
            call winscl('z',3)
            call esymbol(ipxx(INVERT),ipzz(INVERT),24,1)
          elseif(inpmode.eq.5)then
            ipyy(INVERT)=iyy
            ipzz(INVERT)=izz
            py(INVERT)=tpx
            pz(INVERT)=tpy
            call winscl('z',3)
            call esymbol(ipyy(INVERT),ipzz(INVERT),24,1)
          endif

          if(INVERT.gt.0.and.INVERT.le.9)write(temp3,'(i1)') INVERT
          if(INVERT.gt.9.and.INVERT.le.99)write(temp3,'(i2)') INVERT
          if(INVERT.gt.99)write(temp3,'(i3)') INVERT
          if(inpmode.eq.1.or.inpmode.eq.2)then
            call textatxy(ipxx(INVERT)+4,ipyy(INVERT)-1,temp3,'z',3)
            call winscl('-',0)
            X(INVERT)=px(INVERT)
            Y(INVERT)=py(INVERT)
            if(inpmode.eq.1)then
              Z(INVERT)=Z1
            elseif(inpmode.eq.2)then
              if(INVERT.gt.1)then
                Z3=Z(INVERT-1)
              else
                Z3=0.0
              endif
              CALL EASKR(Z3,' ',' Elevation (Z metres)? ',
     &          0.000,'W',99.9,'W',2.7,'elevation',IER,2)
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
            call winscl('-',0)
            X(INVERT)=px(INVERT)
            Z(INVERT)=pz(INVERT)
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
            call winscl('-',0)
            Y(INVERT)=py(INVERT)
            Z(INVERT)=pz(INVERT)
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
            call winscl('-',0)
            XGT(INVERT)=px(INVERT)
            YGT(INVERT)=py(INVERT)
            if(INVERT.gt.1)then
              Z3=ZGT(INVERT-1)
            else
              Z3=0.0
            endif
            CALL EASKR(Z3,' ',' Elevation (Z metres)? ',
     &        -99.000,'W',99.9,'W',0.0,'elevation',IER,2)
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
          goto 146

C Selection terminated with `e`, ask user if they want to save the
C information. If so fill zone common blocks and make a
C dummy surface with the first three vertices.
 147      continue
          h(1)='Having stopped adding points, if you made a mistakes'
          h(2)='you can clear and start again or you can clear data'
          h(3)='and return to the menu or save the data.'
          call easkabc('Options:',' ','clear points & redefine zone',
     &      'clear points & continue','save zone data',izclear,3)
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
              inpm=inpmode
              call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,inpm,
     &         ixbul,iybul,ixblr,iyblr,xbmnorth,xbmorigin,xbmgrid,
     &         xbmscale,iwmg,factor,viewlim,invert,nw,ixtn1,ixtn2,iytn1,
     &         iytn2,ixo,iyo,ipxx,ipyy,ipzz,ipxw,ipyw,nipwhis,ipxwhis,
     &         ipywhis,iphisznst,iphisznfn,completed,other)

C Reset to enter the clicking.
              call edisp(iuout,'BEGIN RE-ENTERING POINTS NOW...')
              INO=12+m+n
              goto 29
            endif
          elseif(izclear.eq.3)then

C If adding another then use the previously defined names and
C instanciate the common blocks.
            if(addinganother)then
              icomp=incomp
              call st2name(ZN,zname(ICOMP))
              zdesc(ICOMP)=zd
              LGEOM(ICOMP)=DFILE
              LTHRM(ICOMP)=CFILE
              LPROJ(ICOMP)=OFILE
            endif
            call winscl('-',0)
            call edisp(iuout,'Instanciating new objects...')
            if(inpmode.eq.6)then
              NGTV=INVERT
              NGT=1
              NGVER(1)=3
              JGVN(1,1)=1
              JGVN(1,2)=2
              JGVN(1,3)=3
              GSNAME(1)='first'
              GMLCN(1)='UNKN'
            else
              NTV=INVERT
              NSUR=1
              NVER(1)=3
              JVN(1,1)=1
              JVN(1,2)=2
              JVN(1,3)=3
              NZTV(icomp)=NTV
              SNAME(ICOMP,1)='first'
              SOTF(1)='OPAQ'
              SMLCN(1)='UNKN'
              SVFC(1)='FLOR'
              SOTHER(1,1)='UNKNOWN'
              SOTHER(1,2)='0'
              SOTHER(1,3)='0'
              SUSE(1,1)='-'
              SUSE(1,2)='-'
              SPARENT(1)='-'
              NGL(1)=0

C Update the connection list for an initial surface.
              ICCC=NCON
              ICCC=ICCC+1
              IC1(ICCC)=ICOMP
              IE1(ICCC)=1
              ICT(ICCC)=0
              IC2(ICCC)=0
              IE2(ICCC)=0
              SSNAME(iccc)=SNAME(ICOMP,1)
              SSOTF(iccc)=SOTF(1)
              SSMLCN(iccc)=SMLCN(1)
              SSVFC(iccc)=SVFC(1)
              SSOTHER(iccc,1)=SOTHER(1,1)
              SSOTHER(iccc,2)=SOTHER(1,2)
              SSOTHER(iccc,3)=SOTHER(1,3)
              SSUSE(iccc,1)=SUSE(1,1)
              SSUSE(iccc,2)=SUSE(1,2)
              SSPARENT(iccc)=SPARENT(1)
              IZSTOCN(icomp,1)=iccc
              NCON=ICCC
            endif
            justentered=.false.
         endif
        elseif(inpmode.eq.3)then

C Floor plan extrusion. click on corners until list terminated, allowing
C bitmap to be panned if necessary during the process.
          call edisp(iuout,
     & 'Input points around the base ANTI-clockwise looking from the')
          call edisp(iuout,
     & 'top, preferably start from the lower left corner and...')
          call edisp(iuout,
     & 'type `d` to delete last point, `e` to finish.')
          if(xbmgrid)then
            if(xbmsnap)then
              call edisp(iuout,'type `s` to turn off snap-to grid.')
            else
              call edisp(iuout,'type `s` to turn on snap-to grid.')
            endif
          endif
          if(trimmed)call edisp(iuout,'type `p` to pan bitmap.')
          NW=0
 246      CALL trackview(ii,ixx,iyy)

C If user typed `e` or `E` then complete the process.
          if(ii.eq.69.or.ii.eq.101)goto 247

C Toggle on/off the snap-to grid.
          if(ii.eq.83.or.ii.eq.115)then
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
          if(ii.eq.80.or.ii.eq.112)then
            if(trimmed)then
              h(1)='The view of a truncated bitmap can be panned left'
              h(2)='and right and up and down (by 50 pixels)'
              write(msg,'(a,i4,a,i4,a)') 
     &          'Panning options: (current offset x',ixoffset,' y ',
     &          iyoffset,' pixels)'
              CALL EASKATOG(msg,' ','done','left','right','up','down',
     &          ' ',' ',IWM,2)
              if(iwm.gt.1.and.iwm.le.5)then
                inpm=inpmode
                call ctlbmpan(iwm,fname,ixoffset,iyoffset,ibmwidth,
     &            ibmhight,iwidth,ihight,inpm,ixbul,iybul,ixblr,iyblr,
     &            xbmnorth,xbmorigin,invert,nw,ixtn1,ixtn2,iytn1,
     &            iytn2,ixo,iyo,ipxx,ipyy,ipzz,ipxw,ipyw,nipwhis,
     &            ipxwhis,ipywhis)

C Refresh current bitmap and overlayed information.
                completed=.false.
                inpm=inpmode
                call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,
     &            inpm,ixbul,iybul,ixblr,iyblr,xbmnorth,xbmorigin,
     &            xbmgrid,xbmscale,iwmg,factor,viewlim,invert,nw,
     &            ixtn1,ixtn2,iytn1,iytn2,ixo,iyo,ipxx,ipyy,ipzz,ipxw,
     &            ipyw,nipwhis,ipxwhis,ipywhis,iphisznst,iphisznfn,
     &            completed,other)
              endif
            endif
            goto 246
          endif

C If user typed `d` or `D` then delete the previous coordinate.
          if(ii.eq.67.or.ii.eq.100)then
            if(NW.gt.1)then
              NW=NW-1
              call edisp(iuout,'Last corner deleted. Refreshing...')
              completed=.false.
              inpm=inpmode
              call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,
     &          inpm,ixbul,iybul,ixblr,iyblr,xbmnorth,xbmorigin,
     &          xbmgrid,xbmscale,iwmg,factor,viewlim,invert,nw,
     &          ixtn1,ixtn2,iytn1,iytn2,ixo,iyo,ipxx,ipyy,ipzz,ipxw,
     &          ipyw,nipwhis,ipxwhis,ipywhis,iphisznst,iphisznfn,
     &          completed,other)
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
  318     tpxw=float(ixx-ixo)/factor
          tpyw=float(iyo-iyy)/factor
          if(xbmsnap)then
            REMX=AMOD(tpxw,GRSPC(1))
            REMY=AMOD(tpyw,GRSPC(2))
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

C Find if this corner is unique, if it is not unique then jump back and
C slect another pixel.
          if(NW.gt.1)then
            unique=.true.
            do 66,ij=1,NW
              CLOSEX=.false.
              CLOSEY=.false.
              CALL ECLOSE(pxw(ij),tpxw,0.03,CLOSEX)
              CALL ECLOSE(pyw(ij),tpyw,0.03,CLOSEY)
              if(CLOSEX.and.CLOSEY)unique=.false.
  66        continue
            if(.NOT.unique)then
              call edisp(iuout,'duplicate corner...try again.')
              goto 246
            endif
          endif

C Corner added draw it and index and a line from the previous to current.
          NW=NW+1
          ipxw(NW)=ixx
          ipyw(NW)=iyy
          pxw(NW)=tpxw
          pyw(NW)=tpyw

          call winscl('z',3)
          call esymbol(ipxw(NW),ipyw(NW),24,1)
          if(NW.gt.0.and.NW.le.9)write(temp3,'(i1)') NW
          if(NW.gt.9.and.NW.le.99)write(temp3,'(i2)') NW
          if(NW.gt.99)write(temp3,'(i3)') NW
          call textatxy(ipxw(NW)+4,ipyw(NW)-1,temp3,'z',3)
          if(NW.gt.1)then
            call winscl('z',3)
            call edwline(ipxw(NW-1),ipyw(NW-1),ipxw(NW),ipyw(NW))
          endif
          if(NW.eq.1)then
            write(outs,'(a,i3,a,f8.3,a,f8.3)') 'Corner ',NW,
     &        ' @ X',pxw(NW),' Y',pyw(NW)
          else
            tdis=crowxyz(pxw(NW-1),pyw(NW-1),0.0,pxw(NW),pyw(NW),0.0)
            call ln2az(pxw(NW-1),pyw(NW-1),0.0,pxw(NW),pyw(NW),0.0,
     &          az,el)
            write(outs,'(a,i3,a,f8.3,a,f8.3,a,f7.3,a,f8.3,a,f7.3)') 
     &        'Corner ',NW,' @ X',pxw(NW),' Y',pyw(NW),' distance ',
     &        tdis,'m and @ aimuth ',az,' & elev ',el
          endif
          call edisp(iuout,outs)
          XX(NW)=pxw(NW)
          YY(NW)=pyw(NW)
          goto 246

C Selection terminated with `e`, ask user if they want to save the
C information. If so fill zone common blocks. Draw line from last 
C corner to first corner and return forground colour to black.
 247      continue
          call winscl('z',3)
          call edwline(ipxw(NW),ipyw(NW),ipxw(1),ipyw(1))
          call winscl('-',0)
          h(1)='Having stopped adding points, if you made a mistakes'
          h(2)='you can clear and start again or you can clear data'
          h(3)='and return to the menu or save the data.'
          call easkabc('Options:',' ','clear points & redefine zone',
     &      'clear points & continue','save zone data',izclear,3)
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
     &         ixbul,iybul,ixblr,iyblr,xbmnorth,xbmorigin,xbmgrid,
     &         xbmscale,iwmg,factor,viewlim,invert,nw,ixtn1,ixtn2,iytn1,
     &         iytn2,ixo,iyo,ipxx,ipyy,ipzz,ipxw,ipyw,nipwhis,ipxwhis,
     &         ipywhis,iphisznst,iphisznfn,completed,other)

C Reset to enter the clicking.
              call edisp(iuout,'BEGIN RE-ENTERING POINTS NOW...')
              INO=12+m+n
              goto 29
            endif
          elseif(izclear.eq.3)then

C If adding another then use the previously defined names and
C instanciate the common blocks.
            if(addinganother)then
              icomp=incomp
              call st2name(ZN,zname(ICOMP))
              zdesc(ICOMP)=zd
              LGEOM(ICOMP)=DFILE
              LTHRM(ICOMP)=CFILE
              LPROJ(ICOMP)=OFILE
            endif

C Convert into a gen body and update nzsur and nztv
            CALL EREGC(NW,Z1,Z2,XX,YY)
            NSUR=NW+2

C Begin with default assumptions for each surface then overwrite
C this if user supplied information exists.
            CALL FILSUR(iuout,ICOMP,0)
            DO 444 I=1,NSUR
              NGL(I)=0
  444       CONTINUE

C Update the connection list.
            ICCC=NCON
            DO 432 ICC=1,NSUR
              ICCC=ICCC+1
              IC1(ICCC)=ICOMP
              IE1(ICCC)=ICC
              ICT(ICCC)=0
              IC2(ICCC)=0
              IE2(ICCC)=0
              SSNAME(iccc)=SNAME(icomp,icc)
              SSOTF(iccc)=SOTF(icc)
              SSMLCN(iccc)=SMLCN(icc)
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
            ENDIF
            if(zdesc(ICOMP)(1:2).EQ.'  ')then
              write(zdesc(ICOMP),'(a,a)') 
     &          zname(ICOMP)(1:lnblnk(zname(ICOMP))),' describes a...'
            endif

C Set type type and version based on current value of igupgrade.
            CTYPE(icomp)='GEN '
            if(igupgrade.eq.2)then
              gversion(icomp)=1.1
            else
              gversion(icomp)=1.0
            endif
            NDP(ICOMP)=3
            IDPN(ICOMP,1)=0
            IDPN(ICOMP,2)=0
            IDPN(ICOMP,3)=0

            NZSUR(ICOMP)=NSUR  ! update nzsur() it is needed by zgupdate.
            NZTV(ICOMP)=NTV

C Update the G7 common blocks and then assign ZBASEA.
            call zgupdate(2,ICOMP,ier)
            IUZBASEA(icomp)=0
            IBASES(ICOMP,1)=NSUR
            IZBASELIST(ICOMP)=1
            ibcs=IZSTOCN(icomp,NSUR)
            ZBASEA(icomp)= SSNA(ibcs)
 
C Save this to file before passing into the geometry editing facility.
            write(outs,'(3a)') 'Updating ',
     &        zdesc(ICOMP)(1:lnblnk(zdesc(ICOMP))),'...'
            call edisp(iuout,outs)
            call eclose(gversion(ICOMP),1.1,0.01,newgeo)
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            else
              call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            endif
            IF(IER.EQ.1)THEN
              CALL usrmsg(' ',' Problem creating geometry file...','W')
            ENDIF

            write(outs,'(3a)') 'Updating ',
     &        zdesc(ICOMP)(1:lnblnk(zdesc(ICOMP))),'...done.'
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
     &        ' ','P')
              call CTLWRT(ICTLF,IER)
              call usrmsg(' ',' ','-')
            endif
            CALL EMKCFG('-',IER)
            call usrmsg(' updating model for additional zone...done.',
     &        ' ','P')

C Save the extrusion points to the history list. Set iphisznst
C to the first point in this zone and iphisznfn to the closing
C point of the zone.
            if(NW.gt.0)then
              do 217 j=1,NW
                if(nipwhis+1.le.300)then
                  nipwhis=nipwhis+1
                  if(j.eq.1)iphisznst(icomp)=nipwhis
                  ipxwhis(nipwhis)=ipxw(j)
                  ipywhis(nipwhis)=ipyw(j)
                endif
  217         continue
              if(nipwhis+1.le.300)then
                nipwhis=nipwhis+1
                ipxwhis(nipwhis)=ipxw(1)
                ipywhis(nipwhis)=ipyw(1)
              endif
              iphisznfn(icomp)=nipwhis
            endif
            justentered=.false.

C Refresh current bitmap to recognise zone just saved.
        completed=.true.
        inpm=inpmode
        call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,inpm,
     &    ixbul,iybul,ixblr,iyblr,xbmnorth,xbmorigin,xbmgrid,
     &    xbmscale,iwmg,factor,viewlim,invert,nw,ixtn1,ixtn2,iytn1,
     &    iytn2,ixo,iyo,ipxx,ipyy,ipzz,ipxw,ipyw,nipwhis,ipxwhis,
     &    ipywhis,iphisznst,iphisznfn,completed,other)
          endif

C Reset the temporary string variables used to name and describe zone.
          ZN=' '
          zd=' '
        elseif(inpmode.eq.7)then

C Input mode for obstructions. Each obstruction is formed by a
C point at origin, a 2nd point at end of front edge and a point
C at the back corner from the front edge.
          call edisp(iuout,
     &      'Select three corners (origin, end of front edge and')
          call edisp(iuout,
     &      '  back corner...type `e` to finish.')
          if(xbmgrid)then
            if(xbmsnap)then
              call edisp(iuout,
     & 'type `s` to turn off snap-to grid (before starting block).')
            else
              call edisp(iuout,
     & 'type `s` to turn on snap-to grid (before starting block).')
            endif
          endif
          if(trimmed)then
            call edisp(iuout,
     &        'And type `p` to pan bitmap (before starting block).')
          endif

C Pick up the origin of the block and/or instruction to end the
C input or pan the bitmap.
 346      CALL trackview(ii,ixx1,iyy1)

C If user typed `e` then complete the process.
          if(ii.eq.69.or.ii.eq.101)goto 347

C Toggle on/off the snap-to grid.
          if(ii.eq.83.or.ii.eq.115)then
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
          if(ii.eq.80.or.ii.eq.112)then
            if(trimmed)then
 348          h(1)='The view of a truncated bitmap can be panned left'
              h(2)='and right and up and down (by 50 pixels)'
              write(msg,'(a,i4,a,i4,a)') 
     &          'Panning options: (current offset x',ixoffset,' y ',
     &          iyoffset,' pixels)'
              CALL EASKATOG(msg,' ','done','left','right','up','down',
     &          ' ',' ',IWM,2)
              if(iwm.eq.1.or.iwm.gt.5)then
                goto 346
              else
                inpm=inpmode
                call ctlbmpan(iwm,fname,ixoffset,iyoffset,ibmwidth,
     &            ibmhight,iwidth,ihight,inpm,ixbul,iybul,ixblr,iyblr,
     &            xbmnorth,xbmorigin,invert,nw,ixtn1,ixtn2,iytn1,
     &            iytn2,ixo,iyo,ipxx,ipyy,ipzz,ipxw,ipyw,nipwhis,
     &            ipxwhis,ipywhis)

C Refresh current bitmap and overlayed information.
                completed=.false.
                inpm=inpmode
                call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,
     &            inpm,ixbul,iybul,ixblr,iyblr,xbmnorth,xbmorigin,
     &            xbmgrid,xbmscale,iwmg,factor,viewlim,invert,nw,
     &            ixtn1,ixtn2,iytn1,iytn2,ixo,iyo,ipxx,ipyy,ipzz,
     &            ipxw,ipyw,nipwhis,ipxwhis,ipywhis,iphisznst,
     &            iphisznfn,completed,other)
              endif
              goto 348
            endif
          endif

C Pick up the next two points
          CALL trackview(ii,ixx2,iyy2)
          CALL trackview(ii,ixx3,iyy3)

C Convert three points.
          tpx1=float(ixx1-ixo)/factor
          tpy1=float(iyo-iyy1)/factor
          tpx2=float(ixx2-ixo)/factor
          tpy2=float(iyo-iyy2)/factor
          tpx3=float(ixx3-ixo)/factor
          tpy3=float(iyo-iyy3)/factor

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
            REMX2=AMOD(tpx2,GRSPC(1))
            REMY2=AMOD(tpy2,GRSPC(2))
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
            REMX3=AMOD(tpx3,GRSPC(1))
            REMY3=AMOD(tpy3,GRSPC(2))
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
          ipxx(INVERT)=ixx1
          ipyy(INVERT)=iyy1
          px(INVERT)=tpx1
          py(INVERT)=tpy1
          call winscl('z',3)
          call esymbol(ipxx(INVERT),ipyy(INVERT),24,1)
          write(temp6,'(A)')'Org'
          CALL wstxpt(ixx1+5,iyy1-5,temp6)

C Remember the point at the end of the front edge.
          INVERT=INVERT+1
          ipxx(INVERT)=ixx2
          ipyy(INVERT)=iyy2
          px(INVERT)=tpx2
          py(INVERT)=tpy2
          call winscl('z',3)
          call esymbol(ipxx(INVERT),ipyy(INVERT),24,1)
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
C Debug...
C          write(6,*) 'az azim vdx vdy vdz ',az,azim,vdx,vdy,vdz

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
          ipxx(INVERT)=ixx3
          ipyy(INVERT)=iyy3
          px(INVERT)=tpx3
          py(INVERT)=tpy3
          call winscl('z',3)
          call esymbol(ipxx(INVERT),ipyy(INVERT),24,1)
          tdepth=crowxyz(tpx2,tpy2,Z1,tpx3,tpy3,Z1)
          call edwline(ixx1,iyy1,ixx2,iyy2)
          call edwline(ixx2,iyy2,ixx3,iyy3)

C Try and complete the obstruction block by making a similar point
C offset from the origin. Depending on the direction of the vector
C between point 2 & 3 draw the two extra lines that complete the box.
          ioffx=iabs(ixx3-ixx2)
          ioffy=iabs(iyy3-iyy2)
          if(ixx3.eq.ixx2)then
            ioffx=0
            if(iyy3.eq.iyy2)then
              ioffy=0
            elseif(iyy3.lt.iyy2)then
              call eswline(ixx1,iyy1,ixx1,iyy1-ioffy)
              call eswline(ixx1,iyy1-ioffy,ixx3,iyy3)
            elseif(iyy3.gt.iyy2)then
              call eswline(ixx1,iyy1,ixx1,iyy1+ioffy)
              call eswline(ixx1,iyy1+ioffy,ixx3,iyy3)
            endif
          elseif(ixx3.lt.ixx2)then
            if(iyy3.eq.iyy2)then
              ioffy=0
              call eswline(ixx1,iyy1,ixx1-ioffx,iyy1)
              call eswline(ixx1-ioffx,iyy1,ixx3,iyy3)
            elseif(iyy3.lt.iyy2)then
              call eswline(ixx1,iyy1,ixx1-ioffx,iyy1-ioffy)
              call eswline(ixx1-ioffx,iyy1-ioffy,ixx3,iyy3)
            elseif(iyy3.gt.iyy2)then
              call eswline(ixx1,iyy1,ixx1-ioffx,iyy1+ioffy)
              call eswline(ixx1-ioffx,iyy1+ioffy,ixx3,iyy3)
            endif
          elseif(ixx3.gt.ixx2)then
            if(iyy3.eq.iyy2)then
              ioffy=0
              call eswline(ixx1,iyy1,ixx1+ioffx,iyy1)
              call eswline(ixx1+ioffx,iyy1,ixx3,iyy3)
            elseif(iyy3.lt.iyy2)then
              call eswline(ixx1,iyy1,ixx1+ioffx,iyy1-ioffy)
              call eswline(ixx1+ioffx,iyy1-ioffy,ixx3,iyy3)
            elseif(iyy3.gt.iyy2)then
              call eswline(ixx1,iyy1,ixx1+ioffx,iyy1+ioffy)
              call eswline(ixx1+ioffx,iyy1+ioffy,ixx3,iyy3)
            endif
          endif
C Debug..
C          write(6,*) ixx3,ixx2,ixx1,ioffx,ixx1+ioffx
C          write(6,*) iyy3,iyy2,iyy1,ioffy,iyy1+ioffy

          write(outs,'(a,i3,a,f8.3,a,f8.3,a,f7.3,a,f7.3,a,f7.3)') 
     &      'Block ',NB,' @ X',tpx1,' Y',tpy1,' width ',twid,
     &      'm and depth ',tdepth,'m @ aimuth ',RO
          call edisp(iuout,outs)
          call forceflush()
          dok=.true.
          h(1)='Assuming the last block is where you want it proceed.'
          CALL ASKOK(' ','Block position ok? ',OK,dok,1)
          if(.NOT.ok)then
            INVERT=INVERT-3
            completed=.false.
            inpm=inpmode
            call refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,inpm,
     &        ixbul,iybul,ixblr,iyblr,xbmnorth,xbmorigin,xbmgrid,
     &        xbmscale,iwmg,factor,viewlim,invert,nw,ixtn1,ixtn2,
     &        iytn1,iytn2,ixo,iyo,ipxx,ipyy,ipzz,ipxw,ipyw,nipwhis,
     &        ipxwhis,ipywhis,iphisznst,iphisznfn,completed,other)
            goto 346
          endif
          NB=NB+1
          NBFREL=NBFREL+3

C Ask for name.
          H(1)='A descriptive name (up to 8 char) is recommended.'
          S8='blk_'
          CALL EASKS(S8,' ',' Name of obstruction? ',
     &      8,'obstruction','Block name',IER,1)
          IF(S8.NE.' ')BLKNAME(NB)=S8
          H(1)='The base elevation of obstruction is in metres.'
          CALL EASKR(Z1,' ',' Elevation of the base of obstruction?',
     &      0.000,'W',99.9,'W',0.0,'elevation of obs base',IER,1)
          H(1)='The height of obstruction is in metres (dimension.'
          H(2)='from base to top). '
          CALL EASKR(Z2,' ',' Height of the obstruction?',
     &      0.000,'W',99.9,'W',0.0,'elevation of obs base',IER,2)
          CALL EPKMLC(ISEL,
     &      'Select an OPAQUE construction from the list to',
     &      'associate with the block for visualisation purposes.',IER)
          IF(ISEL.GT.0)then
            WRITE(BLKMAT(NB),'(A12)')DESC(ISEL)(1:12)
          endif
          XO(NB)=tpx1
          YO(NB)=tpy1
          ZO(NB)=Z1
          DX(NB)=twid
          DY(NB)=tdepth
          DZ(NB)=Z2
          BANG(NB)=RO
          call usrmsg(' ',' ','-')
          call edisp(iuout,' ')
          call edisp(iuout,
     &      'You can now pick points for next obstruction...')
          goto 346

  347     continue

C Save current batch of obstructions, first ask which zone it is related to.
          H(1)=' Pick a zone for these obstructions.'
          INPIC=1
          CALL EPICKS(INPIC,IVALS,' ',
     &      ' Associate which zone with these obstructions? ',
     &      12,NCOMP,zname,' zone list',IER,1)
          IC=IVALS(1)
          if(IC.gt.0.and.NB.gt.0)then
            if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
              WRITE(OFILE,'(A,A4)')zname(IC)(1:lnblnk(zname(IC))),
     &          '.obs'
            else
              WRITE(OFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &          zname(IC)(1:lnblnk(zname(IC))),'.obs'
            endif
            H(1)='In order to account for external shading devices,'
            H(2)='window reveals, trees, outside walls, etc. in a '
            H(3)='detailed shading analysis the user should'
            H(4)='approximate such objects as one or more prisms' 
            H(5)='which are described in a site obstruction file.'
            H(6)='The browse option presents a list of files in the'
            H(7)='project `zones` folder.'
            if(ZOBS(IC)(1:2).eq.'  '.or.ZOBS(IC)(1:4).eq.'UNKN')then
              LTMP=OFILE
            else
              LTMP=ZOBS(IC)
            endif
            CALL EASKS(LTMP,' Zone obstructions file? ',
     &        ' ',72,OFILE,'obstructions file',IER,5)
            IF(LTMP.NE.' ')ZOBS(IC)=LTMP
            IOBS(ic)=1
            IUF=IFIL+1
            CALL MKGOMST(IUF,ZOBS(IC),IC,IER)
            CALL EMKCFG('-',IER)

C Reset justentered so the `create more` option is shown.
            justentered=.false.
          endif
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
      subroutine dogrid(iwmg,factor,ixo,iyo,ixbul,iybul,ixblr,iyblr,
     &  viewlim,other)
      COMMON/GFONT/IFS,ITFS,IMFS
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

      dimension viewlim(6)
      logical greyok,closelabel
      character temp6*6
      real other   ! the other grid distance defined by user
      real xp,yp   ! current position in X and Y
      real xpl,ypl ! most recent metre interval.

      if(iwmg.eq.1)then
         return
      elseif(iwmg.gt.1)then

C If libraries and monitor support greyscale then reset forground
C to 36% grey when drawing dots.
        greyok=.false.
        if(nifgrey.gt.4)then
          call winscl('i',nifgrey-4)
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
C Debug...
C          write(6,'(a,f8.4,3i6)') 'grid info',xp,ix,ily,iuy
          if(ix.gt.ixbul.and.ix.lt.ixblr)then

C If within display range draw vertical line. If spacing less than 1m
C put label at 1m invervals and draw solid grey line.
            if(iwmg.eq.3.or.iwmg.eq.4.or.iwmg.eq.5.or.iwmg.eq.6)then
              if(greyok)call winscl('i',nifgrey-4)
C              call eswline(ix,ily,ix,iuy)
              call edline(ix,ily,ix,iuy,2)
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
                if(greyok)call winscl('i',nifgrey-4)
C                call eswline(ix,ily,ix,iuy)
                call edline(ix,ily,ix,iuy,2)
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
                if(greyok)call winscl('i',nifgrey-3)
                call edline(ix,ily,ix,iuy,3)
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
                if(greyok)call winscl('i',nifgrey-3)
                call edline(ix,ily,ix,iuy,3)
                if(ix.gt.ixaxr)ixaxr=ix
                if(xp.lt.float(i+1)) goto 444  ! can any more be drawn within this metre?
              endif
            endif
          endif
          if(iwmg.eq.4)then
            xp=float(i)+0.5
            ix=ixo+int(xp*factor)
            if(ix.gt.ixbul.and.ix.lt.ixblr)then
              if(greyok)call winscl('i',nifgrey-3)
              call edline(ix,ily,ix,iuy,3)
              if(ix.gt.ixaxr)ixaxr=ix
            endif
          elseif(iwmg.eq.5)then
            do 345 ij=1,3
              xp=xp+0.25
              ix=ixo+int(xp*factor)
              if(ix.gt.ixbul.and.ix.lt.ixblr)then
                if(greyok)call winscl('i',nifgrey-4)
                if(ij.eq.1)call edline(ix,ily,ix,iuy,3)
                if(ij.eq.2)call edline(ix,ily,ix,iuy,2)
                if(ij.eq.3)call edline(ix,ily,ix,iuy,3)
                if(ix.gt.ixaxr)ixaxr=ix
              endif
 345        continue
          elseif(iwmg.eq.6)then
            do 344 ij=1,9
              xp=xp+0.1
              ix=ixo+int(xp*factor)
              if(ix.gt.ixbul.and.ix.lt.ixblr)then
                if(greyok)call winscl('i',nifgrey-4)
                if(ij.eq.5)then
                  call edline(ix,ily,ix,iuy,2)
                else
                  call edline(ix,ily,ix,iuy,3)
                endif
                if(ix.gt.ixaxr)ixaxr=ix
              endif
 344        continue
          endif
  342   continue

C Now loop through vertical axis.
        do 343 i=int(viewlim(3)),int(viewlim(4))
          ilx=ixaxl
          irx=ixaxr
          if(iwmg.eq.7)then

C If user defined interval only set on the first pass.
            if(i.eq.int(viewlim(3)))then
              yp=float(i)
              ypl=float(i)
            else
              ypl=float(i)
            endif
          else
            yp=float(i)
            ypl=float(i)
          endif
          iy=iyo-int(yp*factor)
C Debug...
C          write(6,'(a,f8.4,3i6)') 'grid info',yp,ilx,irx,iy
          if(iy.gt.iybul.and.iy.lt.iyblr)then
            if(iwmg.eq.3.or.iwmg.eq.4.or.iwmg.eq.5.or.iwmg.eq.6)then
              if(greyok)call winscl('i',nifgrey-4)
C              call eswline(ilx,iy,irx,iy)
              call edline(ilx,iy,irx,iy,2)
              write(temp6,'(i3)') i
              call textatxy(irx+3,iy-1,temp6,'i',nifgrey-4)
            elseif(iwmg.eq.2)then

C Do every other metre.
C              write(6,*) 'i mod ',i,mod(i,2)
              if(mod(i,2).eq.0)then
                if(greyok)call winscl('i',nifgrey-4)
C                call eswline(ilx,iy,irx,iy)
                call edline(ilx,iy,irx,iy,2)
                write(temp6,'(i3)') i
                call textatxy(irx+3,iy-1,temp6,'i',nifgrey-4)
              endif
            elseif(iwmg.eq.7)then
              call eclose(yp,ypl,0.001,closelabel)
              if(closelabel)then
                if(greyok)call winscl('i',nifgrey-3)
                call edline(ilx,iy,irx,iy,3)
                write(temp6,'(i3)') i
                call textatxy(irx+3,iy-1,temp6,'i',nifgrey-4)
              endif
  445         yp=yp+other
              iy=iyo-int(yp*factor)
              if(iy.gt.iybul.and.iy.lt.iyblr)then
                if(greyok)call winscl('i',nifgrey-3)
                call edline(ilx,iy,irx,iy,3)
                if(yp.lt.float(i+1)) goto 445  ! can any more be drawn within this metre?
              endif
            endif
          endif
          if(iwmg.eq.4)then
            yp=yp+0.5
            iy=iyo-int(yp*factor)
            if(greyok)call winscl('i',nifgrey-4)
            if(iy.gt.iybul.and.iy.lt.iyblr)call edline(ilx,iy,irx,iy,3)
          elseif(iwmg.eq.5)then

C Logic for 0.25m.
            yp=yp+0.25
            iy=iyo-int(yp*factor)
            if(greyok)call winscl('i',nifgrey-4)
            if(iy.gt.iybul.and.iy.lt.iyblr)call edline(ilx,iy,irx,iy,3)
            yp=yp+0.25
            iy=iyo-int(yp*factor)
            if(greyok)call winscl('i',nifgrey-4)
            if(iy.gt.iybul.and.iy.lt.iyblr)call edline(ilx,iy,irx,iy,2)
            yp=yp+0.25
            iy=iyo-int(yp*factor)
            if(greyok)call winscl('i',nifgrey-4)
            if(iy.gt.iybul.and.iy.lt.iyblr)call edline(ilx,iy,irx,iy,3)
          elseif(iwmg.eq.6)then

C Logic for 0.1m.
            do 346 ij=1,9
              yp=yp+0.1
              iy=iyo-int(yp*factor)
              if(iy.gt.iybul.and.iy.lt.iyblr)then
                if(greyok)call winscl('i',nifgrey-4)
                if(ij.eq.5)then
                  call edline(ilx,iy,irx,iy,2)
                else
                  call edline(ilx,iy,irx,iy,3)
                endif
              endif
  346       continue
          endif
  343   continue
      endif
      IF(IFS.GT.0)CALL winfnt(IFS)
      call winscl('-',0)
      return
      end

C ***** shifoverlays
C Based on logical variables xbmnorth,xbmorigin and sizes of invert,nw
C update the position parameters and pixel arrays. Paramter definintion
C noted at top of source code.
C inpm is the input mode.
      subroutine shifoverlays(xbmnorth,xbmorigin,invert,nw,idfxoffset,
     &  idfyoffset,inpm,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo,ipxx,ipyy,ipzz,
     &  ipxw,ipyw,nipwhis,ipxwhis,ipywhis)
#include "building.h"

      logical xbmnorth,xbmorigin
      dimension ipxx(MGTV),ipyy(MGTV),ipzz(MGTV)
      dimension ipxw(MTV),ipyw(MTV)
      dimension ipxwhis(300),ipywhis(300)

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

C If there are vertices or walls then shift them. 
      if(INVERT.gt.0)then
        do 116 j=1,INVERT
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
  116   continue
      endif

C If in floor plan extrusion mode.
      if(inpm.eq.3)then
        if(NW.gt.0)then
          do 216 j=1,NW
            ipxw(j)=ipxw(j)+idfxoffset
            ipyw(j)=ipyw(j)+idfyoffset
  216     continue
        endif
        if(nipwhis.gt.0)then
          do 316 j=1,nipwhis
            ipxwhis(j)=ipxwhis(j)+idfxoffset
            ipywhis(j)=ipywhis(j)+idfyoffset
  316     continue
        endif
      endif
      return
      end

C ****** Refresh current bitmap and current overlayed information.
C Parameter definitions noted at top of source code.
C inpm is the input mode << >>.
C Other is the grid distance if user supplied (iwmg=7).
      subroutine refrshcur(fname,ixoffset,iyoffset,iwidth,ihight,
     &  inpm,ixbul,iybul,ixblr,iyblr,xbmnorth,xbmorigin,xbmgrid,
     &  xbmscale,iwmg,factor,viewlim,invert,nw,ixtn1,ixtn2,iytn1,iytn2,
     &  ixo,iyo,ipxx,ipyy,ipzz,ipxw,ipyw,nipwhis,ipxwhis,ipywhis,
     &  iphisznst,iphisznfn,completed,other)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN

      logical xbmnorth,xbmorigin,xbmgrid,xbmscale,completed,within
      dimension ipxx(MGTV),ipyy(MGTV),ipzz(MGTV)
      dimension ipxw(MTV),ipyw(MTV)
      dimension ipxwhis(300),ipywhis(300),viewlim(6)
      dimension iphisznst(MCOM),iphisznfn(MCOM)
      character fname*72,temp6*6,temp3*3

C Debug.
C      write(6,*) 'refresh bitmap'
      call startbuffer()
      call winlodpart(fname,ixoffset,iyoffset,iwidth,ihight,
     &  15,15,ixbul,iybul,ixblr,iyblr)
      call edisp(iuout,'Refreshing bitmap...')
      call winscl('z',0)
      call esymbol(ixbul,iybul,6,1)
      call esymbol(ixblr,iyblr,4,1)
      call winscl('-',0)

C If north known and within the displayed area then draw it.
      if(inpm.eq.4.or.inpm.eq.5)then
        continue
      else
        if(xbmnorth)then
          if(((ixtn1.ge.ixbul).and.(ixtn1.le.ixblr)).and.
     &       ((iytn1.ge.iybul).and.(iytn1.le.iyblr)))then
            call winscl('z',0)
            call esymbol(ixtn1,iytn1,11,1)
            call esymbol(ixtn2,iytn2,18,1)
            call edwline(ixtn1,iytn1,ixtn2,iytn2)
            write(temp6,'(A)')'North'
            CALL wstxpt(ixtn2+5,iytn2-5,temp6)
            call winscl('-',0)
          endif
        endif
      endif

C If origin known and within the displayed area then draw it.
      if(xbmorigin)then
        if(((ixo.ge.ixbul).and.(ixo.le.ixblr)).and.
     &     ((iyo.ge.iybul).and.(iyo.le.iyblr)))then
          call winscl('z',0)
          call esymbol(ixo,iyo,24,1)
          write(temp6,'(A)')'Origin'
          CALL wstxpt(ixo+5,iyo-5,temp6)
          call winscl('-',0)
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
        call dogrid(iwmg,factor,ixo,iyo,ixbul,iybul,ixblr,iyblr,
     &    viewlim,other)
      endif

C If there are vertices then draw them. Note: check if within the
C limits of ixbul,iybul,ixblr,iyblr (the pixel positions of the
C bitmap as drawn on the display).
      if(INVERT.gt.0)then
        do 122 j=1,INVERT
          if(inpm.eq.1.or.inpm.eq.2.or.inpm.eq.6.or.inpm.eq.7)then
            if(((ipxx(j).ge.ixbul).and.(ipxx(j).le.ixblr)).and.
     &         ((ipyy(j).ge.iybul).and.(ipyy(j).le.iyblr)))then
              call winscl('z',3)
              call esymbol(ipxx(j),ipyy(j),24,1)
              if(j.gt.0.and.j.le.9)write(temp3,'(i1)') j
              if(j.gt.9.and.j.le.99)write(temp3,'(i2)') j
              if(j.gt.99)write(temp3,'(i3)') j
              call textatxy(ipxx(j)+4,ipyy(j)-1,temp3,'z',3)
              call winscl('-',0)
            endif
          elseif(inpm.eq.4)then
            if(((ipxx(j).ge.ixbul).and.(ipxx(j).le.ixblr)).and.
     &         ((ipzz(j).ge.iybul).and.(ipzz(j).le.iyblr)))then
              call winscl('z',3)
              call esymbol(ipxx(j),ipzz(j),24,1)
              if(j.gt.0.and.j.le.9)write(temp3,'(i1)') j
              if(j.gt.9.and.j.le.99)write(temp3,'(i2)') j
              if(j.gt.99)write(temp3,'(i3)') j
              call textatxy(ipxx(j)+4,ipzz(j)-1,temp3,'z',3)
              call winscl('-',0)
            endif
          elseif(inpm.eq.5)then
            if(((ipyy(j).ge.ixbul).and.(ipyy(j).le.ixblr)).and.
     &         ((ipzz(j).ge.iybul).and.(ipzz(j).le.iyblr)))then
              call winscl('z',3)
              call esymbol(ipyy(j),ipzz(j),24,1)
              if(j.gt.0.and.j.le.9)write(temp3,'(i1)') j
              if(j.gt.9.and.j.le.99)write(temp3,'(i2)') j
              if(j.gt.99)write(temp3,'(i3)') j
              call textatxy(ipyy(j)+4,ipzz(j)-1,temp3,'z',3)
              call winscl('-',0)
            endif
          endif
  122   continue
      endif

C If there are historical points from earlier extruded floor plans
C draw them. If outside of the displayed bitmap do not draw it
C or the line of the wall.
C      if(nipwhis.gt.0.and.inpm.eq.3)then
      if(nipwhis.gt.0)then
        do 124 j=1,nipwhis
          if(((ipxwhis(j).ge.ixbul).and.(ipxwhis(j).le.ixblr)).and.
     &       ((ipywhis(j).ge.iybul).and.(ipywhis(j).le.iyblr)))then
            call winscl('z',2)
            call esymbol(ipxwhis(j),ipywhis(j),24,1)
            if(j.gt.0.and.j.le.9)write(temp3,'(i1)') j
            if(j.gt.9.and.j.le.99)write(temp3,'(i2)') j
            if(j.gt.99)write(temp3,'(i3)') j
            call textatxy(ipxwhis(j)+4,ipywhis(j)-1,temp3,'z',2)
            if(j.gt.1)then
              call winscl('z',2)

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
                call edwline(ipxwhis(j-1),ipywhis(j-1),ipxwhis(j),
     &            ipywhis(j))
              endif
            endif
          endif
  124   continue
      endif

C If there is an extruded floor plan draw them. If of outside
C of the displayed bitmap do not draw it or the line of the wall.
C << code needs to be updated to allow for clipping >>
      if(NW.gt.0.and.inpm.eq.3)then
        do 123 j=1,NW
          if(((ipxw(j).ge.ixbul).and.(ipxw(j).le.ixblr)).and.
     &       ((ipyw(j).ge.iybul).and.(ipyw(j).le.iyblr)))then
            call winscl('z',3)
            call esymbol(ipxw(j),ipyw(j),24,1)
            if(j.gt.0.and.j.le.9)write(temp3,'(i1)') j
            if(j.gt.9.and.j.le.99)write(temp3,'(i2)') j
            if(j.gt.99)write(temp3,'(i3)') j
            call textatxy(ipxw(j)+4,ipyw(j)-1,temp3,'z',3)
            if(j.gt.1)then
              call winscl('z',3)
              call edwline(ipxw(j-1),ipyw(j-1),ipxw(j),ipyw(j))
            endif
          endif
  123   continue

C If user has completed selection, draw the closing line if it is
C within the extents of the displayed bitmap.
        if(completed)then
          if(((ipxw(1).ge.ixbul).and.(ipxw(1).le.ixblr)).and.
     &       ((ipyw(1).ge.iybul).and.(ipyw(1).le.iyblr)).and.
     &       ((ipxw(NW).ge.ixbul).and.(ipxw(NW).le.ixblr)).and.
     &       ((ipyw(NW).ge.iybul).and.(ipyw(NW).le.iyblr)))then
            call winscl('z',3)
            call edwline(ipxw(NW),ipyw(NW),ipxw(1),ipyw(1))
          endif
        endif
        call winscl('-',0)
      endif
      return
      end

C **** control panning
C Interface control for panning of the bitmap within the graphics feedback
C area of a esp-r module.
C inpm is the input mode.
      subroutine ctlbmpan(iwm,fname,ixoffset,iyoffset,ibmwidth,ibmhight,
     &  iwidth,ihight,inpm,ixbul,iybul,ixblr,iyblr,xbmnorth,xbmorigin,
     &  invert,nw,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo,ipxx,ipyy,ipzz,ipxw,
     &  ipyw,nipwhis,ipxwhis,ipywhis)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      logical xbmnorth,xbmorigin
      dimension ipxx(MGTV),ipyy(MGTV),ipzz(MGTV)
      dimension ipxw(MTV),ipyw(MTV)
      dimension ipxwhis(300),ipywhis(300)
      character fname*72

C Debug...
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
          idfxoffset=ixoffset
          idfyoffset=0
          ixoffset=0
          iwidth=ibmwidth
          if(igw.lt.ibmwidth) iwidth=igw
          ihight=ibmhight
          if(igwh.lt.ibmhight) ihight=igwh
          call checklodpart(fname,ixoffset,iyoffset,iwidth,ihight,
     &      15,15,ixbul,iybul,ixblr,iyblr)
          call edisp(iuout,'Back to left edge of bitmap.')
        else
          call startbuffer()
          idfxoffset=50
          idfyoffset=0
          ixoffset=ixoffset-50
          iwidth=ibmwidth
          if(igw.lt.ibmwidth) iwidth=igw
          ihight=ibmhight
          if(igwh.lt.ibmhight) ihight=igwh
          call checklodpart(fname,ixoffset,iyoffset,iwidth,ihight,
     &      15,15,ixbul,iybul,ixblr,iyblr)
          call edisp(iuout,'Panned 50 pixels to left...')
        endif
        call shifoverlays(xbmnorth,xbmorigin,invert,nw,idfxoffset,
     &   idfyoffset,inpm,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo,ipxx,ipyy,ipzz,
     &   ipxw,ipyw,nipwhis,ipxwhis,ipywhis)
      elseif(IWM.eq.3)then
        call startbuffer()
        idfxoffset=-50
        idfyoffset=0
        ixoffset=ixoffset+50
        iwidth=ibmwidth-50
        if(igw.lt.iwidth) iwidth=igw
        ihight=ibmhight
        if(igwh.lt.ibmhight) ihight=igwh
        call checklodpart(fname,ixoffset,iyoffset,iwidth,ihight,
     &    15,15,ixbul,iybul,ixblr,iyblr)
        call edisp(iuout,'Panned 50 pixels to right...')
        call shifoverlays(xbmnorth,xbmorigin,invert,nw,idfxoffset,
     &   idfyoffset,inpm,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo,ipxx,ipyy,ipzz,
     &   ipxw,ipyw,nipwhis,ipxwhis,ipywhis)
      elseif(IWM.eq.4)then
        if(iyoffset.eq.0)then
          return
        elseif(iyoffset.gt.0.and.iyoffset.le.50)then
          call startbuffer()
          idfyoffset=iyoffset
          idfxoffset=0
          iyoffset=0
          iwidth=ibmwidth
          if(igw.lt.ibmwidth) iwidth=igw
          ihight=ibmhight
          if(igwh.lt.ibmhight) ihight=igwh
          call checklodpart(fname,ixoffset,iyoffset,iwidth,ihight,
     &      15,15,ixbul,iybul,ixblr,iyblr)
          call edisp(iuout,'Panned to top of bitmap...')
          call shifoverlays(xbmnorth,xbmorigin,invert,nw,idfxoffset,
     &     idfyoffset,inpm,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo,ipxx,ipyy,
     &     ipzz,ipxw,ipyw,nipwhis,ipxwhis,ipywhis)
        else
          call startbuffer()
          idfyoffset=50
          idfxoffset=0
          iyoffset=iyoffset-50
          iwidth=ibmwidth
          if(igw.lt.ibmwidth) iwidth=igw
          ihight=ibmhight
          if(igwh.lt.ibmhight) ihight=igwh
          call checklodpart(fname,ixoffset,iyoffset,iwidth,ihight,
     &      15,15,ixbul,iybul,ixblr,iyblr)
          call edisp(iuout,'Panned 50 pixels up in bitmap...')
          call shifoverlays(xbmnorth,xbmorigin,invert,nw,idfxoffset,
     &     idfyoffset,inpm,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo,ipxx,ipyy,
     &     ipzz,ipxw,ipyw,nipwhis,ipxwhis,ipywhis)
        endif
      elseif(IWM.eq.5)then
        call startbuffer()
        idfxoffset=0
        idfyoffset=-50
        iyoffset=iyoffset+50
        iwidth=ibmwidth
        if(igw.lt.ibmwidth) iwidth=igw
        ihight=ibmhight-50
        if(igwh.lt.ihight) ihight=igwh
        call checklodpart(fname,ixoffset,iyoffset,iwidth,ihight,
     &    15,15,ixbul,iybul,ixblr,iyblr)
        call edisp(iuout,'Panned 50 pixels down in bitmap...')
        call shifoverlays(xbmnorth,xbmorigin,invert,nw,idfxoffset,
     &   idfyoffset,inpm,ixtn1,ixtn2,iytn1,iytn2,ixo,iyo,ipxx,ipyy,
     &   ipzz,ipxw,ipyw,nipwhis,ipxwhis,ipywhis)
      endif
      return
      end
