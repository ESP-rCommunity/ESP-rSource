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

C ********** extrblk
C Subroutine to extract block information related to an
C INCLUDE command in a dxf file. bn is the block name
C being searched for, ioblk is the scratch block information
C file. xp,yp,zp are where to apply the block,
C xsca,ysc,zsc are the scaling factors, rot is the rotation angle.
      subroutine extrblk(itrc,bn,xp,yp,zp,xsc,ysc,zsc,rot,ier)
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk
      integer gcode,innum
      character instring*72
      real realin,coorx,coory,coorz,blkxorg,blkyorg,blkzorg
      common/rd/gcode,instring,innum,realin,coorx,coory,coorz,
     &           blkxorg,blkyorg,blkzorg

      real flag3d,dxfconv
      integer flagins
      common/fg/flag3d,flagins,dxfconv

      integer indxzon
      character oformat*24,head*4
      common/dxfe/oformat,indxzon,head

      character bn*72,instr*72,ckstr*85,endstr*85
      character outs*124
      
      PI = 4.0 * ATAN(1.0)
      RAD = PI/180.

C Scan for the block name:  "*blk ????" start and end, make up
C local strings for comparison.
      write(ckstr,'(a,a)')  '*blk ',bn(1:lnblnk(bn))
      write(endstr,'(a,a)') '*end ',bn(1:lnblnk(bn))
      lc=lnblnk(instr)+5

 10   read(ioblk,'(a)',end=500,err=600) instr
      l=lnblnk(instr)
      if(instr(1:l).eq.ckstr(1:lc))then

C Found start of block.
        if(itrc.gt.0)then
          write(outs,'(a,a)') 'found ',bn(1:lnblnk(bn))
          call edisp(iuout,outs)
        endif
 12     read(ioblk,'(a)',end=500,err=600) instr
        l=lnblnk(instr)
        if(instr(1:10).eq.'BLOCK BASE')then

C Read the block base origin.
          read(ioblk,*) blkxorg,blkyorg,blkzorg
          if(itrc.gt.0)then
            write(outs,*) 'b base = ',blkxorg,blkyorg,blkzorg,
     &               ' place at ',xp,yp,zp
            call edisp(iuout,outs)
            write(outs,*) 'scale by ',xsc,ysc,zsc,' rotate ',rot
            call edisp(iuout,outs)
          endif
          goto 12
        elseif(instr(1:3).eq.'LAY')then

C Skip past the layer index and read again.
          read(ioblk,*,end=500,err=600)ilayer
          if(oformat(1:6).eq.'viewer')then
            write(ioout,'(a)') 'LAY'
            write(ioout,'(i3)') ilayer
          endif
          goto 12
        elseif(instr(1:3).eq.'COL')then

C Skip past the colour and read again.
          read(ioblk,*,end=500,err=600)icolour
          if(oformat(1:6).eq.'viewer')then
            write(ioout,'(a)') 'COL'
            write(ioout,'(i3)') icolour
          endif
          goto 12
        elseif(instr(1:3).eq.'PLA')then

C Found a plane, see how many vertices.
          read(ioblk,*,end=500,err=600) ivert
          if(oformat(1:6).eq.'viewer')then
            write(ioout,'(a,2x,a)') instr(1:l),bn(1:lnblnk(bn))
            write(ioout,'(i3)') ivert
          endif
          if(ivert.gt.0)then
            do 42 iv=1,ivert

C Get coords.
              read(ioblk,*,end=500,err=600) X0,Y0,Z0
              if(itrc.gt.0)then
                write(outs,'(a,3f12.5)') 'vert ',X0,Y0,Z0
                call edisp(iuout,outs)
              endif

C Scale & (block) Origin displacement.
              X0=(X0-blkxorg)*xsc
              Y0=(Y0-blkyorg)*ysc
              Z0=(Z0-blkzorg)*zsc
              if(itrc.gt.0)then
                write(outs,'(a,3f12.5)') 'scald',X0,Y0,Z0
                call edisp(iuout,outs)
              endif

C Rotate about Z axis.
              if(rot.gt.0.0.or.rot.lt.0.0)then
                cosZ=cos(RAD*rot)
                sinZ=sin(RAD*rot)
                Xn=(X0-blkxorg)*cosZ-(Y0-blkyorg)*sinZ
                Yn=(X0-blkxorg)*sinZ+(Y0-blkyorg)*cosZ
                X0=Xn
                Y0=Yn
              endif
C Translate by
              X0=X0+xp
              Y0=Y0+yp
              Z0=Z0+zp
              if(itrc.gt.0)then
                write(outs,'(a,3f12.5)')'trand ',X0,Y0,Z0
                call edisp(iuout,outs)
              endif

C Convert from mm to metres if requrired, write to file and tell user.
              X0=X0*dxfconv
              Y0=Y0*dxfconv
              Z0=Z0*dxfconv
              if(itrc.gt.0)then
                write(outs,'(a,3f12.4)') ' new  ',X0,Y0,Z0
                call edisp(iuout,outs)
                call edisp(iuout,'  ')
              endif
              if(oformat(1:6).eq.'viewer')then
                write(ioout,'(3f12.4)') X0,Y0,Z0
              endif
  42        continue
          endif
          goto 12
        elseif(instr(1:l).eq.endstr(1:lc))then
          rewind(ioblk,ERR=999)
          return
        endif
      elseif(instr(1:l).eq.endstr(1:lc))then

C End of block information, see about processing vertices.
C Make sure to rewind the file.
        rewind(ioblk,ERR=999)
        return
      else
        goto 10
      endif

  22  return

 500  if(itrc.gt.0)then
        write(outs,'(a)') 'end of block file found... '
        call edisp(iuout,outs)
        call edisp(iuout,instr)
      endif
      rewind(ioblk,ERR=999)
      goto 22

 600  call edisp(iuout,'error reading block file... ')
      ier=1
      goto 22

 999  call edisp(iuout,'error rewinding file ')
      ier=1
      goto 22
      end
