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

C Set of low level routines.

C ***** face3d
C face3d are triangles or quads which are general surfaces in autocad.
      subroutine face3d(itrc)
#include "building.h"
#include "geometry.h"
#include "dxfdata.h"
      
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk

      common/matrix/ amat(3,3), xnorm, ynorm, znorm

      integer indxzon
      character oformat*24,head*4
      common/dxfe/oformat,indxzon,head

      character RZNAME*24
      logical match

      real xx(10),yy(10),zz(10)
      integer count
      integer iestart,iend

      iestart=0; iend=0   ! clear
      xnorm = 0.0; ynorm = 0.0; znorm = 1.0 ! initial assumptions
      call readgc(itrc)  
      count=0

C Loop until gcode is zero.   
 9001 if (gcode.ne.0)then
        if(gcode.eq.8) then
          call getlay(currlay,itrc)
          call matchl(currlay,head,RZNAME,match,im)
          if(match)indxzon=im
        elseif (gcode.ge.10 .and. gcode.le.16) then
          xx(gcode-9)=coorx        
          yy(gcode-9)=coory
          zz(gcode-9)=entelev
          count = gcode - 9
        elseif (gcode.ge.30 .and. gcode.le.36) then
          zz(gcode-29)=realin
          count = gcode -29
        elseif (gcode.eq.62) then
          currcol = innum
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

C Test for triangular patch.
C << use close3 instead >>
      if (xx(count).eq.xx(count-1).and.yy(count).eq.yy(count-1)
     &     .and.zz(count).eq.zz(count-1)) count = count - 1

      if (xx(count).eq.xx(1).and.yy(count).eq.yy(1)
     &     .and.zz(count).eq.zz(1)) count = count - 1

      if (count .lt. 3) return
      call writelc
      call initmatrix
      if(oformat(1:6).eq.'viewer')then

C If working on a block write to chartblk.array.
        if(head.eq.'BLK_')then
          ichartblk=ichartblk+1
          write(chartblk(ichartblk),'(a)') 'PLA (3DFACE)'
          ichartblk=ichartblk+1
          write(chartblk(ichartblk),'(i3)') count
        else 
          write(ioout,'(a)') 'PLA (3DFACE)'
          write(ioout,'(i3)') count 
        endif
      elseif(oformat(1:3).eq.'esp')then
        if(head.eq.'OBS_')then
          continue
        elseif(head.eq.'BLK_')then

C Writing to temporary block file, do it in viewer format.
          ichartblk=ichartblk+1
          write(chartblk(ichartblk),'(a)') 'PLA (3DFACE)'
          ichartblk=ichartblk+1
          write(chartblk(ichartblk),'(i3)') count 
        else

C Here we are updating the number of surfaces in the zone
C Update the izstocn array!
          NZSUR(indxzon) = NZSUR(indxzon) + 1
          NCON=NCON+1
          IZSTOCN(indxzon,NZSUR(indxzon)) = NCON
          if(itrc.gt.1)then
            write(iuout,*) 
     &        'face3d increment esp surf list ',NCON,NZSUR(indxzon)
          endif
        endif
      elseif(oformat(1:5).eq.'click')then
        iestart=nbdxflines+1  ! start of edge list
      endif

      do 10 i=1,count
        coorx=xx(i); coory=yy(i); coorz=zz(i)
        call putcoods(itrc)
        if(oformat(1:5).eq.'click')then

C Each call to putcoords incremented nbdxfcoords so make up the edge.
          nbdxflines=nbdxflines+1
          idxfedge(nbdxflines,1)=nbdxfcoords-1
          idxfedge(nbdxflines,2)=nbdxfcoords
          idxfedgecol(nbdxflines)=currcol
          idxfedgelay(nbdxflines)=currlay
        endif
  10  continue
      iend=nbdxflines   ! end of edge list
      if(oformat(1:5).eq.'click')then
        write(ioout,'(a)') '*startline  # from 3DFACE'
        do 9017 j = iestart,iend
          write(ioout,'(a,i5,i5,2i4,a,i5)') '*edge',idxfedge(j,1),
     &      idxfedge(j,2),currcol,currlay,' # ',j
 9017   continue
      endif

      return
      end


C ***** solid
C solid has 3-4 corners and an extrusion distance or a vector.

C << this is work in progress currently it only does the >>
C << base of the solid body. transforms are needed to do the >>
C << upper 3-4 corners >>

      subroutine solid(itrc)
#include "building.h"
#include "geometry.h"
#include "dxfdata.h"
      
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk

      common/matrix/ amat(3,3), xnorm, ynorm, znorm

      integer indxzon
      character oformat*24,head*4
      common/dxfe/oformat,indxzon,head

      character RZNAME*24
      logical match

      real xx(10),yy(10),zz(10)
      integer count
      integer iestart,iend

      iestart=0; iend=0   ! clear
      xnorm = 0.0; ynorm = 0.0; znorm = 1.0 ! initial assumptions
      flag3d=0.
      call readgc(itrc)  
      count=4   ! there are typically 4 corners in a solid

C Loop until gcode is zero.   
 9001 if (gcode.ne.0)then
        if(gcode.eq.8) then
          call getlay(currlay,itrc)
          call matchl(currlay,head,RZNAME,match,im)
          if(match)indxzon=im
        elseif (gcode.ge.10 .and. gcode.le.16) then
          xx(gcode-9)=coorx        
          yy(gcode-9)=coory
          zz(gcode-9)=entelev
        elseif (gcode.ge.30 .and. gcode.le.36) then
          zz(gcode-29)=realin
        elseif (gcode.eq.39) then
          entthick=realin        
          if (entthick.gt.0.) flag3d = 1.
        elseif (gcode.eq.62) then
          currcol = innum
        elseif (gcode.eq.210) then
          xnorm = realin   ! x vector
        elseif (gcode.eq.220) then
          ynorm = realin   ! y vector
        elseif (gcode.eq.230) then
          znorm = realin   ! z vector
        endif
        call readgc(itrc)
        goto  9001
      endif

C Test for triangular solid.
C << use close3 instead >>
      if (xx(count).eq.xx(count-1).and.yy(count).eq.yy(count-1)
     &     .and.zz(count).eq.zz(count-1)) count = count - 1

      if (xx(count).eq.xx(1).and.yy(count).eq.yy(1)
     &     .and.zz(count).eq.zz(1)) count = count - 1

      if (count .lt. 3) return
      call writelc
      call initmatrix
      call xformp(0.,0.,entthick, dx1,dy1,dz1)
      if(oformat(1:5).eq.'click')then
        iestart=nbdxflines+1  ! start of edge list

        do 10 i=1,count
          coorx=xx(i); coory=yy(i); coorz=zz(i)
          call putcoods(itrc)

C Each call to putcoords incremented nbdxfcoords so make up the edge.
          nbdxflines=nbdxflines+1
          idxfedge(nbdxflines,1)=nbdxfcoords-1
          idxfedge(nbdxflines,2)=nbdxfcoords
          idxfedgecol(nbdxflines)=currcol
          idxfedgelay(nbdxflines)=currlay
  10    continue
        iend=nbdxflines   ! end of edge list
        write(ioout,'(a)') '*startline  # from SOLID'
        do 9017 j = iestart,iend
          write(ioout,'(a,i5,i5,2i4,a,i5)') '*edge',idxfedge(j,1),
     &      idxfedge(j,2),currcol,currlay,' # ',j
 9017   continue
      endif

      return
      end


****** genlin
C this subroutine processes the 'LINE' command in .dxf file of
C autocad and translates it if it is extruded into a surface,
C otherwise if it is 2D it might be useful for click-on-bitmap.
      subroutine genlin(itrc)
#include "building.h"
#include "geometry.h"
#include "dxfdata.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk

      common/matrix/ amat(3,3), xnorm, ynorm, znorm

      integer indxzon
      character oformat*24,head*4
      common/dxfe/oformat,indxzon,head

      character RZNAME*24
      logical match,close
      real dx1,dy1,dz1    ! offset in each axis
      integer iestart,iend

      iestart=0; iend=0   ! clear
      sx = 0.0; sy = 0.0  ! start coord
      ex = 0.0; ey = 0.0  ! end coord       
      xnorm = 0.0; ynorm = 0.0; znorm = 1.0 ! vector
      thickx = 0.0; thicky = 0.0; thickz = 1.0 
      if (entthick.gt.0.0) then
         flag3d=1.
      else
         flag3d=0.
      endif
      z1 = dfltelev
      z2 = dfltelev

      call readgc(itrc)        
 9001 if(gcode.ne.0)then
        if(gcode.eq.8) then
          call getlay(currlay,itrc)
          call matchl(currlay,head,RZNAME,match,im)
          if(match)indxzon=im   ! set zone index
        elseif (gcode.eq.10) then
          sx=coorx; sy=coory  ! set start
        elseif (gcode.eq.11) then
          ex=coorx; ey=coory  ! set end        
        elseif (gcode.eq.30) then
          z1 = realin         ! Z at start
        elseif (gcode.eq.31) then
          z2 = realin         ! Z at end
        elseif (gcode.eq.38) then        
          z1=realin           ! entity elevation v11 or earlier   
          z2 = z1
        elseif (gcode.eq.39) then        
          entthick=realin        
          if (entthick.gt.0.) flag3d = 1.
        elseif (gcode.eq.62) then
          currcol = innum     ! colour number
        elseif (gcode.eq.210) then
          thickx = realin     ! X extrusion direction
        elseif (gcode.eq.220) then
          thicky = realin     ! Y extrusion direction
        elseif (gcode.eq.230) then
          thickz = realin     ! Z ectrusion direction
        endif
        call readgc(itrc)    ! scan next line
        goto  9001
      endif

C A zero code so have passed end of the entity.
      call writelc
      call initmatrix
      call eclose(flag3d,0.0,0.001,close)
      if (close) then
        if(oformat(1:5).eq.'click')then

C Click output can take a 2D line.
          iestart=nbdxflines+1  ! start of edge list
          coorx = sx; coory = sy; coorz = z1  ! pass start
          call putcoods(itrc)      
          coorx = ex; coory = ey; coorz = z2  ! pass end
          call putcoods(itrc)        

C Each call to putcoords incremented nbdxfcoords so make up the edge.
          nbdxflines=nbdxflines+1
          idxfedge(nbdxflines,1)=nbdxfcoords-1
          idxfedge(nbdxflines,2)=nbdxfcoords
          idxfedgecol(nbdxflines)=currcol
          idxfedgelay(nbdxflines)=currlay
          write(ioout,'(a)') '*startline  # from 2D line'
          write(ioout,'(a,i5,i5,2i4,a,i5)') '*edge',
     &      idxfedge(nbdxflines,1),
     &      idxfedge(nbdxflines,2),currcol,currlay,' # ',nbdxflines

        else

C Other output formats cannot deal with 2D stuff.     
          call edisp(iuout, ' skipping single LINE ')
        endif
      else        
        xnorm = thickx; ynorm = thicky; znorm = thickz
        call initmatrix
        call xformp(0.,0.,entthick, dx1,dy1,dz1)
        flag3d=0.       
        xnorm = 0.0; ynorm = 0.0; znorm = 1.0
        call initmatrix
        if(oformat(1:6).eq.'viewer')then
          if(head.eq.'BLK_')then
            ichartblk=ichartblk+1
            write(chartblk(ichartblk),'(a)') 'PLA  (2D ext line)'
            ichartblk=ichartblk+1
            write(chartblk(ichartblk),'(a)') '  4' 
          else 
            write(ioout,'(a)') 'PLA  (2D ext line)'
            write(ioout,'(a)') '  4' 
          endif
        elseif(oformat(1:3).eq.'esp')then

C Add a surface to zone.
          if(head.eq.'OBS_')then
            continue
          elseif(head.eq.'BLK_')then
            continue
          else
  
C Incrementing NZSUR for this zone but also to
C update IZSTOCN to reflect this change.
            NZSUR(indxzon) = NZSUR(indxzon) + 1
            NCON=NCON+1
            IZSTOCN(indxzon,NZSUR(indxzon)) = NCON
            if(itrc.gt.1)then
              write(iuout,*) 
     &        'genlin increment esp surf list ',NCON,NZSUR(indxzon)
            endif
          endif
        elseif(oformat(1:5).eq.'click')then
          iestart=nbdxflines+1  ! start of edge list
        endif
      
        coorx = sx; coory = sy; coorz = z1  ! pass start
        call putcoods(itrc)      
        coorx = ex; coory = ey; coorz = z2  ! pass end
        call putcoods(itrc)        
        if(oformat(1:5).eq.'click')then

C Each call to putcoords incremented nbdxfcoords so make up the edge.
          nbdxflines=nbdxflines+1
          idxfedge(nbdxflines,1)=nbdxfcoords-1
          idxfedge(nbdxflines,2)=nbdxfcoords
          idxfedgecol(nbdxflines)=currcol
          idxfedgelay(nbdxflines)=currlay
        endif
        coorx = ex + dx1
        coory = ey + dy1
        coorz = z2 + dz1
        call putcoods(itrc)        
        if(oformat(1:5).eq.'click')then

C Each call to putcoords incremented nbdxfcoords so make up the edge.
          nbdxflines=nbdxflines+1
          idxfedge(nbdxflines,1)=nbdxfcoords-1
          idxfedge(nbdxflines,2)=nbdxfcoords
          idxfedgecol(nbdxflines)=currcol
          idxfedgelay(nbdxflines)=currlay
        endif
        coorx = sx + dx1
        coory = sy + dy1
        coorz = z1 + dz1
        call putcoods(itrc)        
        if(oformat(1:5).eq.'click')then

C Each call to putcoords incremented nbdxfcoords so make up the edge.
          nbdxflines=nbdxflines+1
          idxfedge(nbdxflines,1)=nbdxfcoords-1
          idxfedge(nbdxflines,2)=nbdxfcoords
          idxfedgecol(nbdxflines)=currcol
          idxfedgelay(nbdxflines)=currlay
        endif
        iend=nbdxflines   ! end of edge list
        if(oformat(1:5).eq.'click')then
          write(ioout,'(a)') '*startline  # from 2D ext line'
          do 9017 j = iestart,iend
            write(ioout,'(a,i5,i5,2i4,a,i5)') '*edge',idxfedge(j,1),
     &        idxfedge(j,2),currcol,currlay,' # ',j
 9017     continue
        endif
      endif        
      return
      end

****** gencrl
C this subroutine processes the 'CIRLCE' command in .dxf file of
C autocad and skip over it.
      subroutine gencrl(itrc)
#include "dxfdata.h"

      common/matrix/ amat(3,3), xnorm, ynorm, znorm

      xnorm = 0.0; ynorm = 0.0; znorm = 1.0 

      call readgc(itrc)     
 9001 if (gcode.ne.0)then
         if (gcode.eq.8) then
            call getlay(currlay,itrc)
         endif
         call readgc(itrc)
         goto  9001
      endif

      return
      end

****** gentext
C this subroutine processes the 'TEXT' command in .dxf file of
C autocad and skip over it.
      subroutine gentext(itrc)
#include "dxfdata.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      common/matrix/ amat(3,3), xnorm, ynorm, znorm
      character tstr*80

      xnorm = 0.0; ynorm = 0.0; znorm = 1.0

      call readgc(itrc)        
 9001 if (gcode.ne.0)then
         if (gcode.eq.1) then
           tstr = instring
           if(itrc.gt.1)then
             write(iuout,*) 
     &         'gentext string is ',tstr(1:lnblnk(tstr))
           endif
         elseif (gcode.eq.8) then
            call getlay(currlay,itrc)
         endif
         call readgc(itrc)
         goto  9001
      endif

      return
      end

C ****** genarc
      subroutine genarc(itrc) 
#include "building.h"
#include "geometry.h"
#include "dxfdata.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk

      common/matrix/ amat(3,3), xnorm, ynorm, znorm

      integer indxzon
      character oformat*24,head*4
      common/dxfe/oformat,indxzon,head

C Use ichartblk and chartblk as a scratch area for blocks.
      character RZNAME*24
      logical match,close
      real PI,radians
      integer iestart,iend

C Clear initial values for local variables.
      iestart=0; iend=0   ! clear
      PI = 4.0 * ATAN(1.0)
      radians = PI/180.
      if (entthick .gt. 0.) then
        flag3d = 1.
      else
        flag3d = 0.
      endif
      xnorm = 0.0; ynorm = 0.0; znorm = 1.0  ! initial assumptions
      radiu = 0.
      cx = 0.      
      cy = 0.
                     
      call readgc(itrc)                
 9001 if (gcode.ne.0)then
        if (gcode.eq.8) then

C Find current layer and then matching zone.
          call getlay(currlay,itrc)
          call matchl(currlay,head,RZNAME,match,im)
          if(match)indxzon=im
        elseif (gcode.eq.10) then
          cx=coorx                
          cy=coory                
        elseif (gcode.eq.30) then
          entelev=realin                
        elseif (gcode.eq.38) then                
          entelev=realin                
        elseif (gcode.eq.39) then                
          entthick=realin                
          flag3d=1.                
        elseif (gcode.eq.40) then                
          radiu=realin                
        elseif (gcode.eq.50) then                
          startangl=realin                
        elseif (gcode.eq.51) then                
          endangl=realin                
        elseif (gcode.eq.62) then
          currcol = innum
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
      if (startangl.gt.endangl) endangl=endangl+360.0
      divs = 5.
      angle = abs(endangl-startangl)
      numsec=int(angle/divs)+1 
      if (numsec.lt.minseg) then
        numsec = minseg
        divs = angle/(numsec-1)
      elseif (numsec .gt. 37) then
        numsec = 37
        divs = angle/36
      endif

      call writelc
      call initmatrix
      call eclose(flag3d,0.0,0.001,close)
      if (close) then                
        call edisp(iuout, ' LIN (ARC) skipped')              
      else                
 9003   if (startangl.lt.endangl)then
          if(oformat(1:6).eq.'viewer')then
            if(head.eq.'BLK_')then
              ichartblk=ichartblk+1
              write(chartblk(ichartblk),'(a)') 'PLANE (ARC: 2D extr)'
              ichartblk=ichartblk+1
              write(chartblk(ichartblk),'(a)') '  4' 
            else 
              write(ioout,'(a)') 'PLANE (ARC: 2D extr)'              
              write(ioout,'(a)') '  4' 
            endif  
          elseif(oformat(1:3).eq.'esp')then
            if(head.eq.'OBS_')then
              continue
            elseif(head.eq.'BLK_')then
              continue
            else
    
C Updating NZSUR for this zone but also to
C update IZSTOCN for this surface.
              NZSUR(indxzon) = NZSUR(indxzon) + 1
              NCON=NCON+1
              IZSTOCN(indxzon,NZSUR(indxzon)) = NCON
              if(itrc.gt.1)then
                write(iuout,*) 
     &          'genarc increment esp surf list ',NCON,NZSUR(indxzon)
              endif
            endif
          elseif(oformat(1:5).eq.'click')then
            iestart=nbdxflines+1  ! start of edge list
          endif
            
          coorx=radiu*cos(startangl*radians)+cx                
          coory=radiu*sin(startangl*radians)+cy                
          coorz=entelev                
          call putcoods(itrc)                
          coorz=entelev+entthick                
          call putcoods(itrc)                
          if(oformat(1:5).eq.'click')then

C Each call to putcoords incremented nbdxfcoords so make up the edge.
            nbdxflines=nbdxflines+1
            idxfedge(nbdxflines,1)=nbdxfcoords-1
            idxfedge(nbdxflines,2)=nbdxfcoords
            idxfedgecol(nbdxflines)=currcol
            idxfedgelay(nbdxflines)=currlay
          endif
          startangl=startangl+divs                
          if (startangl.ge.endangl) goto 110                
          coorx=radiu*cos(startangl*radians)+cx                
          coory=radiu*sin(startangl*radians)+cy                
          coorz=entelev+entthick                
          call putcoods(itrc)                
          if(oformat(1:5).eq.'click')then

C Each call to putcoords incremented nbdxfcoords so make up the edge.
            nbdxflines=nbdxflines+1
            idxfedge(nbdxflines,1)=nbdxfcoords-1
            idxfedge(nbdxflines,2)=nbdxfcoords
            idxfedgecol(nbdxflines)=currcol
            idxfedgelay(nbdxflines)=currlay
          endif
          coorz=entelev                
          call putcoods(itrc)                
          if(oformat(1:5).eq.'click')then

C Each call to putcoords incremented nbdxfcoords so make up the edge.
            nbdxflines=nbdxflines+1
            idxfedge(nbdxflines,1)=nbdxfcoords-1
            idxfedge(nbdxflines,2)=nbdxfcoords
            idxfedgecol(nbdxflines)=currcol
            idxfedgelay(nbdxflines)=currlay
          endif
          goto  9003
        endif
 110    coorx=radiu*cos(endangl*radians)+cx
        coory=radiu*sin(endangl*radians)+cy                
        coorz=entelev+entthick                
        call putcoods(itrc)                
        if(oformat(1:5).eq.'click')then

C Each call to putcoords incremented nbdxfcoords so make up the edge.
          nbdxflines=nbdxflines+1
          idxfedge(nbdxflines,1)=nbdxfcoords-1
          idxfedge(nbdxflines,2)=nbdxfcoords
          idxfedgecol(nbdxflines)=currcol
          idxfedgelay(nbdxflines)=currlay
        endif
        coorz=entelev                
        call putcoods(itrc)                
        if(oformat(1:5).eq.'click')then

C Each call to putcoords incremented nbdxfcoords so make up the edge.
          nbdxflines=nbdxflines+1
          idxfedge(nbdxflines,1)=nbdxfcoords-1
          idxfedge(nbdxflines,2)=nbdxfcoords
          idxfedgecol(nbdxflines)=currcol
          idxfedgelay(nbdxflines)=currlay
        endif
        iend=nbdxflines   ! end of edge list
        if(oformat(1:5).eq.'click')then
          write(ioout,'(a)') '*startline  # from genarc'
          do 9017 j = iestart,iend
            write(ioout,'(a,i5,i5,2i4,a,i5)') '*edge',idxfedge(j,1),
     &        idxfedge(j,2),currcol,currlay,' # ',j
 9017     continue
        endif
      endif                
      coorz=0                
      return                
      end       


C ******** matrix
      subroutine initmatrix
c  this initialises the transformation matrix for the
c  final vertex output

      common/matrix/ amat(3,3), xnorm, ynorm, znorm

      parameter (SMALL = 0.015625)

      if (abs(xnorm).lt.SMALL .and. abs(ynorm).lt.SMALL) then
        wx = 0.0; wy = 1.0; wz = 0.0
      else
        wx = 0.0; wy = 0.0; wz = 1.0
      endif

c get the X-axis vector
      call cross2(wx,wy,wz, xnorm,ynorm,znorm,
     &           amat(1,1),amat(2,1),amat(3,1))
c get the Y-axis vector
      call cross2(xnorm,ynorm,znorm, amat(1,1),amat(2,1),amat(3,1),
     &           amat(1,2),amat(2,2),amat(3,2))

c get the Z-axis vector
      amat(1,3) = xnorm; amat(2,3) = ynorm; amat(3,3) = znorm
      return
      end

C ******* xformp
      subroutine xformp(xo,yo,zo, xm,ym,zm)
C  routine to multiply a point by the current matrix to get a
C  transformed point.
      common/matrix/ amat(3,3), xnorm, ynorm, znorm

      xm = xo*amat(1,1) + xo*amat(1,2) + zo*amat(1,3)
      ym = xo*amat(2,1) + xo*amat(2,2) + zo*amat(2,3)
      zm = xo*amat(3,1) + xo*amat(3,2) + zo*amat(3,3)
      return
      end

