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

C**** convert from ww commands to xfig description.
      subroutine wwxfig(filname1,IER)
      
      integer lnblnk  ! function definition

      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      character filname1*72,OUTSTR*124,slash*1
      logical etstart 

      dimension  ietx(1440),iety(1440),ietud(1440),ietsym(1440)


C Write xfig header.
      iet=0
      slash=CHAR(92)
      igfont=0
      write(ioout,'(a)') '#FIG 3.1'
      write(ioout,'(a)') 'Portrait'
      write(ioout,'(a)') 'Center'
      write(ioout,'(a)') 'Metric'
      write(ioout,'(a)') '1200 2'

C Read ww commands file. Firts scan for axiscale and linescale.
   42 CALL STRIPC(ioin,outstr,0,ND,1,'ww cmds',IER)
      if(ier.ne.0)goto 901
      if(outstr(1:10).eq.'*start_set')then
        goto 42
      elseif(outstr(1:8).eq.'*end_set')then
        goto 901
      elseif(outstr(1:8).eq.'*esymbol')then
        goto 42
      elseif(outstr(1:9).eq.'*axiscale')then
        CALL STRIPC(ioin,outstr,0,ND,1,'axiscale cmds',IER)
        K=0
        CALL EGETWI(outstr,K,igw,0,1000,'-','ax igw',IER)
        CALL EGETWI(outstr,K,igwh,0,1000,'-','ax igwh',IER)
        CALL EGETWR(outstr,K,SXMN,0.,0.,'-','site xmin',IER)
        CALL EGETWR(outstr,K,SXMX,0.,0.,'-','site xmax',IER)
        CALL EGETWR(outstr,K,SYMN,0.,0.,'-','site ymin',IER)
        CALL EGETWR(outstr,K,SYMX,0.,0.,'-','site ymax',IER)
        CALL EGETWR(outstr,K,xsc,0.,0.,'-','xsc',IER)
        CALL EGETWR(outstr,K,ysc,0.,0.,'-','ysc',IER)
        CALL EGETWR(outstr,K,sca,0.,0.,'-','sca',IER)
        CALL EGETWR(outstr,K,Xadd,0.,0.,'-','Xadd',IER)
        CALL EGETWR(outstr,K,Yadd,0.,0.,'-','Yadd',IER)
        goto 42
      elseif(outstr(1:10).eq.'*linescale')then
        CALL STRIPC(ioin,outstr,0,ND,1,'linescale cmds',IER)
        K=0
        CALL EGETWI(outstr,K,igl,0,1000,'-','lnsc loff',IER)
        CALL EGETWR(outstr,K,Xadd,0.,0.,'-','Xadd',IER)
        CALL EGETWR(outstr,K,sca,0.,0.,'-','sca',IER)
        CALL EGETWI(outstr,K,igb,0,1000,'-','lnsc iboff',IER)
        CALL EGETWR(outstr,K,Yadd,0.,0.,'-','Yadd',IER)
        CALL EGETWR(outstr,K,sca,0.,0.,'-','sca',IER)
        goto 42
      elseif(outstr(1:6).eq.'*win3d')then
        CALL STRIPC(ioin,outstr,0,ND,1,'win3d cmds',IER)
        K=0
        CALL EGETWI(outstr,K,menuchw,0,100,'-','win3d menu',IER)
        CALL EGETWI(outstr,K,icl,0,100,'-','win3d icl',IER)
        CALL EGETWI(outstr,K,icr,0,100,'-','win3d icr',IER)
        CALL EGETWI(outstr,K,ict,0,100,'-','win3d ict',IER)
        CALL EGETWI(outstr,K,icb,0,100,'-','win3d icb',IER)
        CALL EGETWI(outstr,K,igl,0,1000,'-','win3d igl',IER)
        CALL EGETWI(outstr,K,igr,0,1000,'-','win3d igr',IER)
        CALL EGETWI(outstr,K,igt,0,1000,'-','win3d igt',IER)
        CALL EGETWI(outstr,K,ign,0,1000,'-','win3d igb',IER)
        CALL EGETWI(outstr,K,igw,0,1000,'-','win3d igw',IER)
        CALL EGETWI(outstr,K,igwh,0,1000,'-','win3d igwh',IER)
        goto 42
      else
        goto 42
      endif
 901  close(ioin)

C Account for the scaling from the win3d igw igwh to the xfig units.
C one pixel is 16 xfig units.

C Scan looking for various commands.
      etstart=.false.
      open(ioin ,file=filname1,status='OLD', err=902)
   43 CALL STRIPC(ioin,outstr,0,ND,1,'ww cmds',IER)
      if(ier.ne.0)goto 902
      if(outstr(1:10).eq.'*start_set')then
        goto 43
      elseif(outstr(1:8).eq.'*end_set')then

C If reached end_set there might be lines to plot.
        if(iet.gt.1)then
          call outet(iet,ietx,iety,ietud,ietsym)
          iet=0
          etstart=.false.
        endif
        goto 902
      elseif(outstr(1:7).eq.'*winfnt')then
        CALL STRIPC(ioin,outstr,0,ND,1,'etplot cmds',IER)
        K=0
        CALL EGETWI(outstr,K,igfont,0,4,'-','gfx font',IER)
        goto 43
      elseif(outstr(1:7).eq.'*etplot')then
        CALL STRIPC(ioin,outstr,0,ND,1,'etplot cmds',IER)
        K=0
        CALL EGETWR(outstr,K,X,0.,0.,'-','Xadd',IER)
        CALL EGETWR(outstr,K,Y,0.,0.,'-','Xadd',IER)
        CALL EGETWI(outstr,K,iud,0,1000,'-','etplot iud',IER)
        CALL EGETWI(outstr,K,isym,0,1000,'-','etplot isym',IER)
        if(iud.eq.0.and.(.NOT.etstart))then

C We have a new series of lines to plot. Start the array of points.
C To convert into xfig units multiply by 16.
          etstart=.true.
          iet=1
          ietx(iet)= (int(( X + Xadd) * sca ) + igl) * 16
          iety(iet)= (igb - int(( Y + Yadd) * sca )) * 16 
          ietud(iet)=iud
          ietsym(iet)=isym
        elseif(iud.eq.0.and.etstart)then

C Arrived at a pen-up (meaning start new polygon) as an extension of
C a current sequence. Write out existing arrays and begin a new one.
          call outet(iet,ietx,iety,ietud,ietsym)
          etstart=.true.
          iet=1
          ietx(iet)= (int(( X + Xadd) * sca ) + igl) * 16
          iety(iet)= (igb - int(( Y + Yadd) * sca )) * 16 
          ietud(iet)=iud
          ietsym(iet)=isym
          goto 43
        else
          iet=iet+1
          ietx(iet)= (int(( X + Xadd) * sca ) + igl) * 16
          iety(iet)= (igb - int(( Y + Yadd) * sca )) * 16 
          ietud(iet)=iud
          ietsym(iet)=isym
        endif
        goto 43
      elseif(outstr(1:7).eq.'*edline')then  ! dotted line
        if(iet.gt.1)then
          call outet(iet,ietx,iety,ietud,ietsym)
          iet=0
          etstart=.false.
        endif
        CALL STRIPC(ioin,outstr,0,ND,1,'edline cmds',IER)
        K=0
        CALL EGETWI(outstr,K,ix1,0,1000,'-','edline ix1',IER)
        CALL EGETWI(outstr,K,iy1,0,1000,'-','edline iy1',IER)
        CALL EGETWI(outstr,K,ix2,0,1000,'-','edline ix2',IER)
        CALL EGETWI(outstr,K,iy2,0,1000,'-','edline iy2',IER)
        CALL EGETWI(outstr,K,ipd,0,1000,'-','edline ipd',IER)
        dl=float(ipd)
        write(ioout,'(a,f6.3,a)')'2 1 2 1 -1 7 0 0 -1',dl,
     &        ' 0 0 -1 0 0 2'
        write(ioout,'(4i6)')ix1*16,iy1*16,ix2*16,iy2*16
        goto 43
      elseif(outstr(1:8).eq.'*edwline')then
        if(iet.gt.1)then
          call outet(iet,ietx,iety,ietud,ietsym)
          iet=0
          etstart=.false.
        endif
        CALL STRIPC(ioin,outstr,0,ND,1,'edwline cmds',IER)
        K=0
        CALL EGETWI(outstr,K,ix1,0,1000,'-','edline ix1',IER)
        CALL EGETWI(outstr,K,iy1,0,1000,'-','edline iy1',IER)
        CALL EGETWI(outstr,K,ix2,0,1000,'-','edline ix2',IER)
        CALL EGETWI(outstr,K,iy2,0,1000,'-','edline iy2',IER)
        write(ioout,'(a)')'2 1 0 2 -1 7 0 0 -1 0.000 0 0 -1 0 0 2'
        write(ioout,'(4i6)')ix1*16,iy1*16,ix2*16,iy2*16
        goto 43
      elseif(outstr(1:8).eq.'*eswline')then
        if(iet.gt.1)then
          call outet(iet,ietx,iety,ietud,ietsym)
          iet=0
          etstart=.false.
        endif
        CALL STRIPC(ioin,outstr,0,ND,1,'eswline cmds',IER)
        K=0
        CALL EGETWI(outstr,K,ix1,0,1000,'-','edline ix1',IER)
        CALL EGETWI(outstr,K,iy1,0,1000,'-','edline iy1',IER)
        CALL EGETWI(outstr,K,ix2,0,1000,'-','edline ix2',IER)
        CALL EGETWI(outstr,K,iy2,0,1000,'-','edline iy2',IER)
        write(ioout,'(a)')'2 1 0 1 -1 7 0 0 -1 0.000 0 0 -1 0 0 2'
        write(ioout,'(4i6)')ix1*16,iy1*16,ix2*16,iy2*16
        goto 43
      elseif(outstr(1:7).eq.'*echain')then
        if(iet.gt.1)then
          call outet(iet,ietx,iety,ietud,ietsym)
          iet=0
          etstart=.false.
        endif
        CALL STRIPC(ioin,outstr,0,ND,1,'echain cmds',IER)
        K=0
        CALL EGETWI(outstr,K,ix1,0,1000,'-','edline ix1',IER)
        CALL EGETWI(outstr,K,iy1,0,1000,'-','edline iy1',IER)
        CALL EGETWI(outstr,K,ix2,0,1000,'-','edline ix2',IER)
        CALL EGETWI(outstr,K,iy2,0,1000,'-','edline iy2',IER)
        CALL EGETWI(outstr,K,ipd,0,1000,'-','edline ipd',IER)
        dl=float(ipd)+1.
        write(ioout,'(a,f6.3,a)')'2 1 1 1 -1 7 0 0 -1',dl,
     &        ' 0 0 -1 0 0 2'
        write(ioout,'(4i6)')ix1*16,iy1*16,ix2*16,iy2*16
        goto 43
      elseif(outstr(1:9).eq.'*viewtext')then
        if(iet.gt.1)then
          call outet(iet,ietx,iety,ietud,ietsym)
          iet=0
          etstart=.false.
        endif
        CALL STRIPC(ioin,outstr,0,ND,1,'viewtext cmds',IER)
        K=0
        CALL EGETWI(outstr,K,linep,0,100,'-','vtxt linep',IER)
        CALL EGETWI(outstr,K,lside,0,1,'-','vtxt lside',IER)
        CALL EGETWI(outstr,K,lsize,0,3,'-','vtxt lsize',IER)
        CALL STRIPC(ioin,outstr,0,ND,1,'viewtext phrase',IER)
        lphw = lnblnk(outstr)
        if(lsize.eq.0)then
          lfs=10
        elseif(lsize.eq.1)then
          lfs=12
        elseif(lsize.eq.2)then
          lfs=14
        elseif(lsize.eq.3)then
          lfs=18
        endif
        lfh = lfs*16
        lfw = lphw*lfs*8
        write(ioout,'(a,i3,a,4i6,2x,a,a,a)')'4 0 -1 0 0 0 ',lfs,
     &    ' 0.000 4 ',lfh,lfw,igl,linep*lfh,outstr(1:lphw),slash,'001'
        goto 43
      elseif(outstr(1:7).eq.'*wstxpt')then
        if(iet.gt.1)then
          call outet(iet,ietx,iety,ietud,ietsym)
          iet=0
          etstart=.false.
        endif
        CALL STRIPC(ioin,outstr,0,ND,1,'wstxpt cmds',IER)
        K=0
        CALL EGETWI(outstr,K,ixp,0,100,'-','wstxt xp',IER)
        CALL EGETWI(outstr,K,iyp,0,1,'-','wstxt yp',IER)
        CALL STRIPC(ioin,outstr,0,ND,1,'wstxpt phrase',IER)
        lphw = lnblnk(outstr)
        if(igfont.eq.0)then
          lfs=10
        elseif(igfont.eq.1)then
          lfs=12
        elseif(igfont.eq.2)then
          lfs=14
        elseif(igfont.eq.3)then
          lfs=18
        endif
        lfh = lfs*16
        lfw = lphw*lfs*8
        write(ioout,'(a,i3,a,4i6,2x,a,a,a)')'4 0 -1 0 0 0 ',lfs,
     &    ' 0.000 4 ',lfh,lfw,ixp*16,iyp*16,outstr(1:lphw),slash,'001'
        goto 43
      else
        goto 43
      endif
      goto 43

 902  close(ioin)

      return
      end

C ***** write out etplot line.
      subroutine outet(iet,ietx,iety,ietud,ietsym)
C If iud > 0 then solid line, if 0 > iud > -100 then dashed,
C if -101 > iud > -200 then chain, if -201 > iud > -300 dotted,
C if -301 > iud > -400 then thick.

      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      dimension  ietx(1440),iety(1440),ietud(1440),ietsym(1440)

      dl=iabs(ietud(2))
      if(ietud(2).gt.0)then
        write(ioout,'(a,i5)')'2 1 0 1 -1 7 0 0 -1 0.000 0 0 -1 0 0',iet
      elseif(ietud(2).lt.0.and.ietud(2).ge.-100)then
        write(ioout,'(a,f6.3,a,i5)')'2 1 1 1 -1 7 0 0 -1',dl,
     &        ' 0 0 -1 0 0',iet
      elseif(ietud(2).lt.-100.and.ietud(2).ge.-200)then
        write(ioout,'(a,f6.3,a,i5)')'2 1 1 1 -1 7 0 0 -1',dl-100.,
     &        ' 0 0 -1 0 0',iet
      elseif(ietud(2).lt.-200.and.ietud(2).ge.-300)then
        write(ioout,'(a,f6.3,a,i5)')'2 1 2 1 -1 7 0 0 -1',dl-200.,
     &        ' 0 0 -1 0 0',iet
      elseif(ietud(2).lt.-300.and.ietud(2).ge.-400)then
        write(ioout,'(a,i5)')'2 1 0 2 -1 7 0 0 -1 0.000 0 0 -1 0 0',iet
      endif
      X=FLOAT(iet)/6.
      N=INT(X)+1
      do 142 i=1,N
        J1 = I*6-5
        J2 = I*6
        IF (I.EQ. N) J2 = iet
        WRITE(ioout,'(12i6)')(ietx(j),iety(j),J=J1,J2)
  142 continue
      return
      end
