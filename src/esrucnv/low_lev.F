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

C Set of low level routines.

C ***** face3d
C face3d are triangles or quads which are general surfaces in autocad.
      subroutine face3d
#include "building.h"
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)

      integer gcode,innum,ioin,ioout,ioblk
      integer currlay, currcol, oldlay, oldcol
      common/trn/xscale,yscale,zscale,rotangl,basex,lineno,minseg,
     &basey,  xincr,yincr,zincr,basez,elev,thick,dfltelev,dfltthick
      common/rd/gcode,instring,innum,realin,coorx,coory,coorz,
     &           blkxorg,blkyorg,blkzorg
      common/io/ioin,ioout,ioblk,iosblk,iotobs
      common/attrs/ currlay, currcol, oldlay, oldcol
      common/matrix/ amat(3,3), xnorm, ynorm, znorm
      common/dxfe/oformat,indxzon,head

      character instring*72,oformat*24,head*4,RZNAME*24
      logical match

      real x(10),y(10),z(10)
      integer count

      xnorm = 0.
      ynorm = 0.
      znorm = 1.
      call readgc  
      count=0      
 9001 if (gcode.ne.0)then
         if (gcode.eq.8) then
            call getlay(currlay)
            call matchl(currlay,head,RZNAME,match,im)
            if(match)indxzon=im
         elseif (gcode.ge.10 .and. gcode.le.16) then
            x(gcode-9)=coorx        
            y(gcode-9)=coory
            z(gcode-9)=elev
            count = gcode - 9
         elseif (gcode.ge.30 .and. gcode.le.36) then
            z(gcode-29)=realin
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
         call readgc
         goto  9001
      endif

c Test for triangular patch.
      if (x(count).eq.x(count-1).and.y(count).eq.y(count-1)
     &     .and.z(count).eq.z(count-1)) count = count - 1

      if (x(count).eq.x(1).and.y(count).eq.y(1)
     &     .and.z(count).eq.z(1)) count = count - 1

      if (count .lt. 3) return
      call writelc
      call initmatrix
      if(oformat(1:6).eq.'viewer')then

C If working on a block write to temporary block file.
        if(head.eq.'BLK_')then
          write(ioblk,'(a)') 'PLA (3DFACE)'
          write(ioblk,'(i3)') count
        else 
          write(ioout,'(a)') 'PLA (3DFACE)'
          write(ioout,'(i3)') count 
        endif
      elseif(oformat(1:3).eq.'esp')then
        if(head.eq.'OBS_')then
        elseif(head.eq.'BLK_')then

C Writing to temporary block file, do it in viewer format.
          write(ioblk,'(a)') 'PLA (3DFACE)'
          write(ioblk,'(i3)') count 
        else

C Here we are updating the number of surfaces in the zone
C Update the izstocn array!
          NZSUR(indxzon) = NZSUR(indxzon) + 1
          NCON=NCON+1
          IZSTOCN(indxzon,NZSUR(indxzon)) = NCON
        endif
      endif

      do 10 i=1,count
         coorx=x(i)
         coory=y(i)
         coorz=z(i)
         call putcoods
 10   continue

      return
      end


****** genlin
C this subroutine processes the 'LINE' command in .dxf file of
C autocad and translates it if it is extruded.
      subroutine genlin
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)

      integer gcode,innum,ioin,ioout,ioblk,flagins
      integer currlay, currcol, oldlay, oldcol
      common/trn/xscale,yscale,zscale,rotangl,basex,lineno,minseg,
     &basey,  xincr,yincr,zincr,basez,elev,thick,dfltelev,dfltthick
      common/rd/gcode,instring,innum,realin,coorx,coory,coorz,
     &           blkxorg,blkyorg,blkzorg
      common/fg/flag3d,flagins,radians,dxfconv
      common/io/ioin,ioout,ioblk,iosblk,iotobs
      common/attrs/ currlay, currcol, oldlay, oldcol
      common/matrix/ amat(3,3), xnorm, ynorm, znorm
      common/dxfe/oformat,indxzon,head
      character instring*72,oformat*24,head*4,RZNAME*24
      logical match

      xnorm = 0.
      ynorm = 0.
      znorm = 1.
      thickx = 0.
      thicky = 0.
      thickz = 1.
      if (thick.gt.0.) then
         flag3d=1.
      else
         flag3d=0.
      endif
      z1 = dfltelev
      z2 = dfltelev

      call readgc        
 9001 if (gcode.ne.0)then
         if (gcode.eq.8) then
            call getlay(currlay)
            call matchl(currlay,head,RZNAME,match,im)
            if(match)indxzon=im
         elseif (gcode.eq.10) then
            sx=coorx        
            sy=coory
         elseif (gcode.eq.11) then
            ex=coorx        
            ey=coory        
         elseif (gcode.eq.30) then
            z1 = realin
         elseif (gcode.eq.31) then
            z2 = realin
         elseif (gcode.eq.38) then        
            z1=realin        
            z2 = z1
         elseif (gcode.eq.39) then        
            thick=realin        
            if (thick.gt.0.) flag3d = 1.
         elseif (gcode.eq.62) then
            currcol = innum
         elseif (gcode.eq.210) then
            thickx = realin
         elseif (gcode.eq.220) then
            thicky = realin
         elseif (gcode.eq.230) then
            thickz = realin
         endif
         call readgc
         goto  9001
      endif

      call writelc
      call initmatrix
      if (flag3d.eq.0.) then        
          call edisp(iuout, ' skipping single LINE ')
      else        
         xnorm = thickx
         ynorm = thicky
         znorm = thickz
         call initmatrix
         call xformp(0.,0.,thick, dx,dy,dz)
         flag3d=0.       
         xnorm = 0.
         ynorm = 0.
         znorm = 1.
         call initmatrix
         if(oformat(1:6).eq.'viewer')then
           if(head.eq.'BLK_')then
             write(ioblk,'(a)') 'PLA  (2D ext line)'
             write(ioblk,'(a)') '  4' 
           else 
             write(ioout,'(a)') 'PLA  (2D ext line)'
             write(ioout,'(a)') '  4' 
           endif
         elseif(oformat(1:3).eq.'esp')then

C Add a surface to zone.
           if(head.eq.'OBS_')then
           elseif(head.eq.'BLK_')then
             call edisp(iuout,'block (2D ext line)..')
           else
  
C Incrementing NZSUR for this zone but also to
C update IZSTOCN to reflect this change.
             NZSUR(indxzon) = NZSUR(indxzon) + 1
             NCON=NCON+1
             IZSTOCN(indxzon,NZSUR(indxzon)) = NCON
           endif
         endif
      
         coorx = sx
         coory = sy
         coorz = z1
         call putcoods        
         coorx = ex
         coory = ey
         coorz = z2
         call putcoods        
         coorx = ex + dx
         coory = ey + dy
         coorz = z2 + dz
         call putcoods        
         coorx = sx + dx
         coory = sy + dy
         coorz = z1 + dz
         call putcoods        
      endif        
      return
      end

****** gencrl
C this subroutine processes the 'CIRLCE' command in .dxf file of
C autocad and skip over it.
      subroutine gencrl

      integer gcode,innum
      integer currlay, currcol, oldlay, oldcol
      common/rd/gcode,instring,innum,realin,coorx,coory,coorz,
     &           blkxorg,blkyorg,blkzorg
      common/attrs/ currlay, currcol, oldlay, oldcol
      common/matrix/ amat(3,3), xnorm, ynorm, znorm
      character instring*72

      xnorm = 0.
      ynorm = 0.
      znorm = 1.

      call readgc        
 9001 if (gcode.ne.0)then
         if (gcode.eq.8) then
            call getlay(currlay)
         endif
         call readgc
         goto  9001
      endif

      return
      end

****** gentext
C this subroutine processes the 'TEXT' command in .dxf file of
C autocad and skip over it.
      subroutine gentext

      integer gcode,innum
      integer currlay, currcol, oldlay, oldcol
      common/rd/gcode,instring,innum,realin,coorx,coory,coorz,
     &           blkxorg,blkyorg,blkzorg
      common/attrs/ currlay, currcol, oldlay, oldcol
      common/matrix/ amat(3,3), xnorm, ynorm, znorm
      character instring*72,tstr*80

      xnorm = 0.
      ynorm = 0.
      znorm = 1.

      call readgc        
 9001 if (gcode.ne.0)then
         if (gcode.eq.1) then
           tstr = instring
         elseif (gcode.eq.8) then
            call getlay(currlay)
         endif
         call readgc
         goto  9001
      endif

      return
      end

C ****** genarc
      subroutine genarc 
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)

      integer gcode,innum,ioin,ioout,ioblk,flagins
      integer currlay, currcol, oldlay, oldcol
      common/trn/xscale,yscale,zscale,rotangl,basex,lineno,minseg,
     &basey,  xincr,yincr,zincr,basez,elev,thick,dfltelev,dfltthick
      common/rd/gcode,instring,innum,realin,coorx,coory,coorz,
     &           blkxorg,blkyorg,blkzorg
      common/fg/flag3d,flagins,radians,dxfconv
      common/io/ioin,ioout,ioblk,iosblk,iotobs
      common/attrs/ currlay, currcol, oldlay, oldcol
      common/matrix/ amat(3,3), xnorm, ynorm, znorm
      common/dxfe/oformat,indxzon,head
      character instring*72,oformat*24,head*4,RZNAME*24
      logical match

      if (thick .gt. 0.) then
        flag3d = 1.
      else
        flag3d = 0.
      endif
      xnorm = 0.
      ynorm = 0.
      znorm = 1.
      call readgc                
 9001 if (gcode.ne.0)then
        if (gcode.eq.8) then

C Find current layer and then matching zone.
          call getlay(currlay)
          call matchl(currlay,head,RZNAME,match,im)
          if(match)indxzon=im
        elseif (gcode.eq.10) then
          cx=coorx                
          cy=coory                
        elseif (gcode.eq.30) then
          elev=realin                
        elseif (gcode.eq.38) then                
          elev=realin                
        elseif (gcode.eq.39) then                
          thick=realin                
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
        call readgc 
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
      if (flag3d.eq.0.) then                
        call edisp(iuout, ' LIN (ARC) skipped')              
      else                
 9003   if (startangl.lt.endangl)then
          if(oformat(1:6).eq.'viewer')then
            if(head.eq.'BLK_')then
              write(ioblk,'(a)') 'PLANE (ARC: 2D extr)'
              write(ioblk,'(a)') '  4' 
            else 
              write(ioout,'(a)') 'PLANE (ARC: 2D extr)'              
              write(ioout,'(a)') '  4' 
            endif  
          elseif(oformat(1:3).eq.'esp')then
            if(head.eq.'OBS_')then
            elseif(head.eq.'BLK_')then
            else
    
C Updating NZSUR for this zone but also to
C update IZSTOCN for this surface.
              NZSUR(indxzon) = NZSUR(indxzon) + 1
              NCON=NCON+1
              IZSTOCN(indxzon,NZSUR(indxzon)) = NCON
            endif
          endif
            
          coorx=radiu*cos(startangl*radians)+cx                
          coory=radiu*sin(startangl*radians)+cy                
          coorz=elev                
          call putcoods                
          coorz=elev+thick                
          call putcoods                
          startangl=startangl+divs                
          if (startangl.ge.endangl) goto 110                
          coorx=radiu*cos(startangl*radians)+cx                
          coory=radiu*sin(startangl*radians)+cy                
          coorz=elev+thick                
          call putcoods                
          coorz=elev                
          call putcoods                
          goto  9003
        endif
 110    coorx=radiu*cos(endangl*radians)+cx
        coory=radiu*sin(endangl*radians)+cy                
        coorz=elev+thick                
        call putcoods                
        coorz=elev                
        call putcoods                
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
        wx = 0.0
        wy = 1.0
        wz = 0.0
      else
        wx = 0.0
        wy = 0.0
        wz = 1.0
      endif

c get the X-axis vector
      call cross2(wx,wy,wz, xnorm,ynorm,znorm,
     &           amat(1,1),amat(2,1),amat(3,1))
c get the Y-axis vector
      call cross2(xnorm,ynorm,znorm, amat(1,1),amat(2,1),amat(3,1),
     &           amat(1,2),amat(2,2),amat(3,2))

c get the Z-axis vector
      amat(1,3) = xnorm
      amat(2,3) = ynorm
      amat(3,3) = znorm
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

