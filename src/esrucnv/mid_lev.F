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

C Mid level routines for dxf translator.
c  BLOCK: processes the block definitions in .dxf file
C  insert: processes the 'INSERT' command in .dxf file and
C  getlay: looks through the current layer names for the

C ******** block
c BLOCK processes the block definitions in .dxf file
c of Autocad and create a /tmp/blockdxf2e file for later
c 'INSERT' command use.

C << NOTE: for some computer types there will not be >>
C << a /tmp folder so work out a different approach >>
      subroutine block(itrc)
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      common/trn/xscale,yscale,zscale,rotangl,basex,lineno,minseg,
     & basey, xincr,yincr,zincr,basez,elev,thick,dfltelev,dfltthick

      integer gcode,innum
      character instring*72
      real realin,coorx,coory,coorz,blkxorg,blkyorg,blkzorg
      common/rd/gcode,instring,innum,realin,coorx,coory,coorz,
     &           blkxorg,blkyorg,blkzorg

      real flag3d,radians,dxfconv
      integer flagins
      common/fg/flag3d,flagins,radians,dxfconv

      integer ioin,ioout,ioblk,iosblk,iotobs
      common/io/ioin,ioout,ioblk,iosblk,iotobs

      real elevlast,thicklast
      integer polylinflag
      common/vex/elevlast,thicklast,polylinflag

      integer currlay,currcol,oldlay,oldcol
      common/attrs/currlay,currcol,oldlay,oldcol

      integer indxzon
      character oformat*24,head*4
      common/dxfe/oformat,indxzon,head

      character bname*72,pbname*72,chr*1
      character RZNAME*24,outs*124
      logical skip,reset,match,close

      reset = .false.

C Place a temporary file in the users home folder with name based
C on the current process number. << >>
      if(itrc.gt.0)call edisp(iuout,' Processing BLOCK section...')
      write(pbname,'(a)') '/tmp/blockdxf2e'
      open(ioblk,file=pbname,status='UNKNOWN')

C The temporary block file (blockdxf2e) will be read in again,
C if the mm>>metre conversion is on then disable it while writing.
      call eclose(dxfconv,1.0,0.001,close)
      if(.NOT.close)then
        reset = .true.
        dxflast = dxfconv 
        dxfconv = 1.0  
      endif
  
C Set that this is a block entity and any putcoord will have the
C block base offset removed.    
      flagins=1
      elevlast = dfltelev
      thicklast = dfltthick
 10   call readgc(itrc)
      if (instring(1:6).eq.'ENDSEC') goto 100

      if (instring(1:5).ne.'BLOCK') goto 10
      dfltelev = elevlast
      dfltthick = thicklast
      call readgc(itrc)                
 9001 if (gcode.ne.0)then
        if (gcode.eq.2) then
          leng=lnblnk(instring)
          if (leng.gt.76)leng=76
 
C Shift to lower case and remove any $ in the name.
          do 15 i=1,leng
            chr=instring(i:i)
            if (chr.eq.'$')then
              instring(i:i)='_'
            elseif (chr.le.'Z'.and.chr.ge.'A') then
              chr=char(ichar(chr)+32)
              instring(i:i)=chr
            endif
 15       continue

C Remove possible leading * from block name.
          bname=' '
          write(bname(1:leng),'(a)') instring(1:leng)
          if (bname(1:1).eq.'*') then
            bname(1:(leng-1))=bname(2:leng)
            leng=leng-1
          endif
             
C There are certain block types that can be ignored:
C aecid.vew, border.vew, ddi.vew, for these look until
C `ENDBLK` found.
          skip=.false.
          if(bname(1:9).eq.'aecid.vew')then
            skip=.true.
          elseif(bname(1:10).eq.'border.vew')then
            skip=.true.
          elseif(bname(1:7).eq.'ddi.vew')then
            skip=.true.
          elseif(bname(1:4).eq.'igv_')then
            skip=.true.
          elseif(bname(1:9).eq.'cwwid.vew')then
            skip=.true.
          elseif(bname(1:3).eq.'a11')then
            skip=.true.
          elseif(bname(1:6).eq.'sqrmid')then
            skip=.true.
          endif
          if(skip)then
            if(itrc.gt.0)then
              write(outs,'(a,a)') ' skipping ',bname(1:lnblnk(bname))
              call edisp(iuout,outs)
            endif
 91         call readgc(itrc)                
            if (gcode.eq.0.and.instring(1:6).eq.'ENDBLK')goto 10
            goto 91
          endif

C Being a block definition place it in the local folder.
C Note the gcode=10 reads in the x & y block origin
          write(ioblk,'(a,a)') '*blk ',bname(1:leng)
        elseif (gcode.eq.10) then
          blkxorg=coorx
          blkyorg=coory
          blkzorg=elev
        elseif (gcode.eq.30) then
          blkzorg=realin
        elseif (gcode.eq.38) then
          dfltelev=realin
          blkzorg = realin
        elseif (gcode.eq.39) then
          dfltthick=realin
        endif
        call readgc(itrc)
        goto  9001
      endif
      write(ioblk,'(a)') 'BLOCK BASE '
      write(ioblk,*) blkxorg,blkyorg,blkzorg
      
C If the input string is something other then ENDBLK then
C parse the set of commands that can be used. Skip those
C that we cannot use.
 9003 if (instring(1:6).ne.'ENDBLK')then
        elev=dfltelev
        thick = dfltthick
        if (instring(1:5).eq.'TRACE') then
          if(itrc.gt.0)then
            write(outs,'(a,a)') ' skipping TRACE:',bname(1:leng)
            call edisp(iuout,outs)
          endif
        elseif (instring(1:4).eq.'LINE') then
          call genlin(itrc)
        elseif (instring(1:3).eq.'ARC') then
          call genarc(itrc)
        elseif (instring(1:5).eq.'SOLID') then
          if(itrc.gt.0)then
            write(outs,'(a,a)') ' skipping SOLID:',bname(1:leng)
            call edisp(iuout,outs)
          endif
          call readgc(itrc)
          goto  9003
        elseif (instring(1:6).eq.'CIRCLE') then

C Circles are not delt with, but need to skip lines.
          if(itrc.gt.0)then
            write(outs,'(a,a)')' skipping CIRCLE:',bname(1:leng)
            call edisp(iuout,outs)
          endif
          call gencrl(itrc)
        elseif (instring(1:6).eq.'INSERT') then
          call insert(itrc)
        elseif (instring(1:5).eq.'POINT') then
          if(itrc.gt.0)then
            write(outs,'(a,a)') ' skipping POINT:',bname(1:leng)
            call edisp(iuout,outs)
          endif
          call readgc(itrc)
          goto  9003
        elseif (instring(1:5).eq.'POLYL') then
          call genplin(itrc)
        elseif (instring(1:5).eq.'3DFAC') then
          call face3d(itrc)
        elseif (instring(1:5).eq.'3DLIN') then 
          if(itrc.gt.0)then
            write(outs,'(a,a)') ' skipping line:',bname(1:leng)
            call edisp(iuout,outs)
          endif
        elseif (instring(1:4).eq.'TEXT') then

C Text is not delt with, but need to skip lines.
          if(itrc.gt.0)then
            write(outs,'(a,a)') ' skipping text:',bname(1:leng)
            call edisp(iuout,outs)
          endif
          call gentext(itrc)
        elseif (gcode.eq.8) then
          call getlay(currlay,itrc)
          call matchl(currlay,head,RZNAME,match,im)
          if(match)indxzon=im
          call readgc(itrc)
        elseif (gcode.eq.62) then
          currcol = innum
          call readgc(itrc)
        else
          call readgc(itrc)
        endif
        goto  9003
      endif
      write(ioblk,'(a,a)') '*end ',bname(1:leng)
      goto 10

C Finished the block file, close it.
 100  if(itrc.gt.0)call edisp(iuout,' Block section processed...')
      close(ioblk)

      flagins=0
      dfltelev = elevlast
      dfltthick = thicklast

C Reset conversion if necessary.
      if(reset)then
        dxfconv =dxflast
      endif

      return
      end

C ******** insert
C Subroutine insert processes the 'INSERT' command in .dxf file and
C translates it into 'FIL' command for .vew file of viewer.
C Note: all the insert files are assumed to be in local folder.
      subroutine insert(itrc) 
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      common/trn/xscale,yscale,zscale,rotangl,basex,lineno,minseg,
     & basey, xincr,yincr,zincr,basez,elev,thick,dfltelev,dfltthick

      integer gcode,innum
      character instring*72
      real realin,coorx,coory,coorz,blkxorg,blkyorg,blkzorg
      common/rd/gcode,instring,innum,realin,coorx,coory,coorz,
     &           blkxorg,blkyorg,blkzorg

      real flag3d,radians,dxfconv
      integer flagins
      common/fg/flag3d,flagins,radians,dxfconv

      integer ioin,ioout,ioblk,iosblk,iotobs
      common/io/ioin,ioout,ioblk,iosblk,iotobs

      integer currlay,currcol,oldlay,oldcol
      common/attrs/currlay,currcol,oldlay,oldcol
      common/matrix/ amat(3,3), xnorm, ynorm, znorm

      integer indxzon
      character oformat*24,head*4
      common/dxfe/oformat,indxzon,head
               
      character bname*72,pbname*72,chr*1,scratch*72
      character RZNAME*24,outs*124
      logical skip,match

      xscale=1.0; yscale=1.0; zscale=1.0; rotangl=0.0
      nrow=1; ncolumn=1
      rdisp=0.0
      cdisp=0
      zrot=0.0
      iucs = 0
      xucs = 0.0; yucs = 0.0; zucs = 0.0
      xrot = 0.0; yrot = 0.0
      delta = 0.015625

      xnorm = 0.0; ynorm = 0.0; znorm = 1.0
      call initmatrix

C Read an entity from DXF file. Depending on the value of gcode
C process the entity type until gcode is zero.
      call readgc(itrc)
 9001 if (gcode.ne.0)then
        if (gcode.eq.8) then
           call getlay(currlay,itrc)
           call matchl(currlay,head,RZNAME,match,im)
           if(match)indxzon=im
        elseif (gcode.eq.2) then

C Found the block name, read `instance` information.
          leng=lnblnk(instring)
          if (leng.gt.76)leng=76

          do 15 i=1,leng
            chr=instring(i:i)
            if (chr.eq.'$')then
              instring(i:i)='_'
            elseif (chr.le.'Z'.and.chr.ge.'A') then
              chr=char(ichar(chr)+32)
              instring(i:i)=chr
            endif
 15       continue

          bname=' '
          write(bname(1:leng),'(a)') instring(1:leng)
          if (bname(1:1).eq.'*') then
            bname(1:(leng-1))=bname(2:leng)
            leng=leng-1
          endif                
        elseif (gcode.eq.10) then
          dx=coorx
          dy=coory
          dz=elev
        elseif (gcode.eq.30) then
          dz=realin
        elseif (gcode.eq.38) then
          elev=realin
        elseif (gcode.eq.39) then
          thick=realin
        elseif (gcode.eq.41) then
          xscale=realin
        elseif (gcode.eq.42) then
          yscale=realin
        elseif (gcode.eq.43) then
          zscale=realin
        elseif (gcode.eq.50) then
          rotangl=realin
        elseif (gcode.eq.70) then
          ncolumn=innum
        elseif (gcode.eq.71) then
          nrow=innum
        elseif (gcode.eq.44) then
          cdisp=realin
        elseif (gcode.eq.45) then
          rdisp=realin
 
C These 3 commands put in for the 'UCS' type of info
        elseif (gcode .eq. 210) then
          xucs = realin
          if (abs(xucs) .gt. delta) iucs = 1
        elseif (gcode .eq. 220) then
          yucs = realin
          if (abs(yucs) .gt. delta) iucs = 1
        elseif (gcode .eq. 230) then
          zucs = realin
        endif
        call readgc(itrc)
        goto  9001
      endif

C If one of the blocks we know we can skip then do so.
      skip=.false.
      if(bname(1:9).eq.'aecid.vew')then
        skip=.true.
      elseif(bname(1:10).eq.'border.vew')then
        skip=.true.
      elseif(bname(1:7).eq.'ddi.vew')then
        skip=.true.
      elseif(bname(1:4).eq.'igv_')then
        skip=.true.
      elseif(bname(1:9).eq.'cwwid.vew')then
        skip=.true.
      elseif(bname(1:3).eq.'a11')then
        skip=.true.
      elseif(bname(1:3).eq.'sqr')then
        skip=.true.
      elseif(bname(1:7).eq.'scratch')then
        skip=.true.
      endif
      if(skip)return

      x=dx
      coorz=dz

C All imported block definition files come from (blockdxf2e) and a
C scratch file (block_scratch) for reporting translations.

C << NOTE: to be done >>
C << there will not be a /tmp folder on Native Windows >>
C << find another place to put this temporary file >>

      write(pbname,'(a)') '/tmp/blockdxf2e'
      write(scratch,'(a)') '/tmp/block_scratch'
      open(ioblk,file=pbname,status='UNKNOWN')
      if(itrc.gt.0)then
        open(iosblk,file=scratch,status='UNKNOWN')
      endif

      if (iucs .eq. 1) then
        if (abs(xucs) .lt. delta) then
          xrot = 90.0
          if (yucs .gt. 0) xrot = 270.0
        elseif (abs(yucs) .lt. delta) then
          yrot = 90.0
          if (xucs .gt. 0) yrot = 270.0
        endif
        do 7005 i=1,ncolumn
          y = dy
          do 7006 j=1,nrow
            if(oformat(1:6).eq.'viewer')then
              write(ioout,'(a)') 'TRA'
              write(ioout,'(a)') bname(1:lnblnk(bname))
              write(ioout,'(a)') '2'
              write(ioout,'(a,3f18.6)') 'MOV',x,y,coorz
C << Note: zrot not defined >>
              write(ioout,'(a,3f18.6)') 'ROT',xrot,yrot,zrot
            elseif(oformat(1:3).eq.'esp')then
              if(bname(1:11).eq.'obstruction')then
              endif
            endif
            y=y+rdisp
 7006     continue
          x=x+cdisp
 7005   continue
      else

C At this point the stuff from the block file should be read
C in and converted.
        do 7001 i=1,ncolumn
          y=dy
          do 7002 j=1,nrow
            if(oformat(1:6).eq.'viewer')then
              write(ioout,'(a)') 'COMMENT'
              write(ioout,'(a)') bname(1:lnblnk(bname))
            endif
            if(itrc.gt.0)then
              write(outs,'(a,a)') ' importing ',bname(1:lnblnk(bname))
              call edisp(iuout,outs)
            endif
            coorx=x
            coory=y
            if(oformat(1:3).eq.'esp')then
              if(bname(1:11).eq.'obstruction')then
                call extrblk(itrc,bname,coorx,coory,coorz,
     &                     xscale,yscale,zscale,rotangl,ier)
                write(iotobs,'(a12,2x,7f9.4)')RZNAME(1:12),
     &            coorx*dxfconv,coory*dxfconv,coorz*dxfconv,
     &            xscale*dxfconv,yscale*dxfconv,zscale*dxfconv,rotangl
              else
              endif
            elseif(oformat(1:6).eq.'viewer')then
              call extrblk(itrc,bname,coorx,coory,coorz,
     &                     xscale,yscale,zscale,rotangl,ier)
            endif
            y=y+rdisp
 7002     continue
          x=x+cdisp
 7001   continue
      endif
      return
      end

C ********* getlay
C  getlay looks through the current layer names for the
C  string in instring, and returns the equivalent layer number
C  if the string is not in the current layer names list, then
C  the name is added.
      subroutine getlay(ilay,itrc)

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      parameter (MAXLAY = 80)
      integer gcode,innum
      character instring*72
      real realin,coorx,coory,coorz,blkxorg,blkyorg,blkzorg
      common/rd/gcode,instring,innum,realin,coorx,coory,coorz,
     &           blkxorg,blkyorg,blkzorg

      integer currlay,currcol,oldlay,oldcol
      common/attrs/currlay,currcol,oldlay,oldcol
      common/layers/ layname(MAXLAY), numlay, laycolour(MAXLAY),
     &       layflag(MAXLAY)

      common/laytoo/ layuse(MAXLAY)
      character layname*72,outs*124

      do 100 i = 1, numlay
        if (layname(i) .eq. instring) then
          ilay = i
          if (laycolour(i).gt.0) currcol = laycolour(i)
          goto 999
        endif
 100  continue

C If this line is reached then name was not found so add it.
      numlay = numlay + 1
      layname(numlay) = instring
      laycolour(numlay) = 0
      layflag(numlay) = 0
      ilay = numlay
      layuse(numlay) = 0
      if(itrc.gt.0)then
         write(outs,'(a,i3,1x,a)') 'new layer ',numlay,
     &     instring(1:lnblnk(instring))
         call edisp(iuout,outs)
      endif
 999  return
      end

C ******** WRITELC
C Write the contents of a viewer layer file.
      subroutine writelc

      parameter (MAXLAY = 80)
      integer ioin,ioout,ioblk,iosblk,iotobs
      common/io/ioin,ioout,ioblk,iosblk,iotobs

      integer currlay,currcol,oldlay,oldcol
      common/attrs/currlay,currcol,oldlay,oldcol

      common/laytoo/ layuse(MAXLAY)

      integer indxzon
      character oformat*24,head*4
      common/dxfe/oformat,indxzon,head

      if (oldlay.ne.currlay) then
        if(oformat(1:6).eq.'viewer')then
          write(ioout,'(a)') 'LAY'
          write(ioout,'(i3)') currlay
        endif
        oldlay = currlay
        layuse(oldlay) = layuse(oldlay) + 1
      endif
      if (currcol .lt. 1) currcol = 1
      if (oldcol.ne.currcol) then
        if(oformat(1:6).eq.'viewer')then
          write(ioout,'(a)') 'COL'
          write(ioout,'(i3)') currcol
        endif
        oldcol = currcol
      endif
      return
      end

C ******* genplin
C genplin - parse a 2 or 3D polyline from information in DXF file.
C itrc indicator so that debug can be done from command line.

      subroutine genplin(itrc)
#include "building.h"
#include "geometry.h"
      parameter (MAXPNT = 100)

      COMMON/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C24/IZSTOCN(MCOM,MS)

      integer polylinflag
      common/trn/xscale,yscale,zscale,rotangl,basex,lineno,minseg,
     & basey, xincr,yincr,zincr,basez,elev,thick,dfltelev,dfltthick

      integer gcode,innum
      character instring*72
      real realin,coorx,coory,coorz,blkxorg,blkyorg,blkzorg
      common/rd/gcode,instring,innum,realin,coorx,coory,coorz,
     &           blkxorg,blkyorg,blkzorg

      real flag3d,radians,dxfconv
      integer flagins
      common/fg/flag3d,flagins,radians,dxfconv

      integer ioin,ioout,ioblk,iosblk,iotobs
      common/io/ioin,ioout,ioblk,iosblk,iotobs
      common/vex/elevlast,thicklast,polylinflag
      common/arrays/xarray(MAXPNT), yarray(MAXPNT), zarray(MAXPNT),
     &       numpl

      integer currlay,currcol,oldlay,oldcol
      common/attrs/currlay,currcol,oldlay,oldcol
      common/matrix/ amat(3,3), xnorm, ynorm, znorm

      integer indxzon
      character oformat*24,head*4
      common/dxfe/oformat,indxzon,head

      character RZNAME*24
      logical match,close

C << Note, if this is being called within a block definition >>
C << and the layer name is 0 (as it might be in microgds) then >>
C << can ignore this and continue with the current layer info. >>
      numpl = 0
      xnorm = 0.0; ynorm = 0.0; znorm = 1.0
      polylinflag = 0
      mesh = 0
      mclosed = 0
      nclosed = 0
      flag3d = 0.0
      call readgc(itrc)
 9001 if (gcode.ne.0)then
        if (gcode.eq.8) then

C Find current layer and then matching zone.
          call getlay(currlay,itrc)
          call matchl(currlay,head,RZNAME,match,im)
          if(match)indxzon=im
        elseif (gcode.eq.38) then
          elev=realin
        elseif (gcode.eq.39) then
          flag3d=1.0
          thick=realin
        elseif (gcode.eq.40) then
        elseif (gcode.eq.41) then
        elseif (gcode.eq.62) then
          currcol = innum
        elseif (gcode.eq.70) then
          polylinflag = innum
c  check the bits to see which flags are set
c            ibset = iand(innum,1)
c            if (ibset .gt. 0) mclosed = 1
c            ibset = iand(innum,32)
c            if (ibset .gt. 0) nclosed = 1

C << btest a function, a vax extension. >>
C             if (btest(innum,1) .eq. .TRUE.) mclosed = 1
C             if (btest(innum,6) .eq. .TRUE.) nclosed = 1
             if (btest(innum,1)) mclosed = 1
             if (btest(innum,6)) nclosed = 1
        elseif (gcode.eq.71) then
          mesh = 1
        elseif (gcode.eq.72) then
          mesh = 1
        elseif (gcode.eq.210) then
          xnorm = realin
        elseif (gcode.eq.220) then
          ynorm = realin
        elseif (gcode.eq.230) then
          znorm = realin
        endif
        call readgc(itrc)
        goto  9001
      endif

 9002 if (instring(1:6).eq.'SEQEND')then
        call eclose(flag3d,0.0,0.001,close)

C Write out the line in chunks of 255 verticies.
        call writelc
        call initmatrix
        if (mclosed.eq.1 .and. mesh.ne.1) then

C This is a closed poly so copy first point to last.
          numpl = numpl + 1
          xarray(numpl) = xarray(1)
          yarray(numpl) = yarray(1)
          zarray(numpl) = zarray(1)
        endif
        istart = 1
        numtod = numpl
 9005   continue
        if (numtod .lt. 255) then
          numd = numtod
        else
          numd = 255
        endif
        if (mesh.eq.1)then

C This is a mesh surface.
          numd = numtod
          call edisp(iuout,' skipping MESH ')
        elseif (close) then

C << Polylines can probably be treated as surfaces >>
          if(oformat(1:6).eq.'viewer')then
            write(ioout,'(a)') 'PLA (POLY)'
            write(ioout,'(i4)') numd
          elseif(oformat(1:3).eq.'esp')then

C Add a surface to zone and update connection.
            if(head.eq.'OBS_')then
            elseif(head.eq.'BLK_')then
              continue
            else
              NZSUR(indxzon) = NZSUR(indxzon) + 1
              NCON=NCON+1
              IZSTOCN(indxzon,NZSUR(indxzon)) = NCON
              if(itrc.gt.1)then
                write(iuout,*) 
     &          'genplin increment esp surf list ',NCON,NZSUR(indxzon)
              endif
            endif
          endif
          do 9006 i = istart, istart+numd-1
            coorx = xarray(i)
            coory = yarray(i)
            coorz = zarray(i)
            call putcoods(itrc)
 9006     continue
        else

C Dump out the sides as planes.  If esp update connections.
          do 9007 i = istart, istart+numd-2
            if(oformat(1:6).eq.'viewer')then
              write(ioout,'(a)') 'PLA (2D extrud)'
              write(ioout,'(a)') '  4'
            elseif(oformat(1:3).eq.'esp')then
              if(head.eq.'OBS_')then
              elseif(head.eq.'BLK_')then
              else
                NZSUR(indxzon) = NZSUR(indxzon) + 1
                NCON=NCON+1
                IZSTOCN(indxzon,NZSUR(indxzon)) = NCON
                if(itrc.gt.1)then
                  write(iuout,*) 
     &            'genplin increment esp surf list ',NCON,NZSUR(indxzon)
                endif
              endif
            endif
            coorx = xarray(i); coory = yarray(i); coorz = zarray(i)
            call putcoods(itrc)
            coorz = coorz + thick
            call putcoods(itrc)
            coorx = xarray(i+1)
            coory = yarray(i+1)
            call putcoods(itrc)
            coorz = zarray(i+1)
            call putcoods(itrc)
 9007     continue
        endif
        istart = istart+numd-1
        numtod = numtod - numd + 1
        if (numtod .gt. 1) goto 9005
      else
        if (instring(1:6).eq.'VERTEX') then
          call vertex(itrc)
        endif
        goto  9002
      endif
 9003 if (gcode.ne.0)then
        if (gcode.eq.38) then
          elev=realin
        elseif (gcode.eq.39) then
          thick=realin
        endif
        call readgc(itrc)
        goto  9003
      endif
      thick = 0.0

      return
      end


C ****** vertex
C vertex - compose XYZ from data in a DXF file.
      subroutine vertex(itrc)

      parameter (MAXPNT = 100)

      common/trn/xscale,yscale,zscale,rotangl,basex,lineno,minseg,
     &basey,  xincr,yincr,zincr,basez,elev,thick,dfltelev,dfltthick

      integer gcode,innum
      character instring*72
      real realin,coorx,coory,coorz,blkxorg,blkyorg,blkzorg
      common/rd/gcode,instring,innum,realin,coorx,coory,coorz,
     &           blkxorg,blkyorg,blkzorg

      real flag3d,radians,dxfconv
      integer flagins
      common/fg/flag3d,flagins,radians,dxfconv

      common/arrays/xarray(MAXPNT), yarray(MAXPNT), zarray(MAXPNT),
     &       numpl

      ivertflag = 0
      call readgc(itrc) 
 9001 if (gcode.ne.0)then
        if (gcode.eq.38) then 
          elev=realin 
        elseif (gcode.eq.39) then
          flag3d=1.0
          thick=realin
        elseif (gcode.eq.70) then
          ivertflag=innum
        elseif (gcode.eq.10) then
          numpl = numpl + 1
          xarray(numpl)=coorx
          yarray(numpl)=coory
          zarray(numpl)=elev 
        elseif (gcode.eq.30) then
          zarray(numpl)=realin
          elev = realin
        elseif (gcode.eq.50) then 
        endif
        call readgc(itrc)
        goto  9001
      endif

C If this is a spline control point then delete it
      if (ivertflag .eq. 16) then
         numpl = numpl - 1
         return
      endif

      return
      end
