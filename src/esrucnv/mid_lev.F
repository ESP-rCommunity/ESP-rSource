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

C Hight & Mid level routines for dxf translator.
c  BLOCK: processes the block definitions in .dxf file
C  insert: processes the 'INSERT' command in .dxf file and
C  getlay: looks through the current layer names for the

C ******** scandxf
C Scandxf does a high level scan of a DXF file and takes
C the information gathered and writes to various output
C file formats.
      subroutine scandxf(filname,filname1,filname2,itmprm,itrc)
#include "building.h"
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)

      character cfgroot*24,LCFGF*72
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)

      parameter (MAXLAY = 80)
      integer gcode,innum
      character instring*72
      real realin,coorx,coory,coorz,blkxorg,blkyorg,blkzorg
      common/rd/gcode,instring,innum,realin,coorx,coory,coorz,
     &           blkxorg,blkyorg,blkzorg

      integer lineno
      common/trn/xscale,yscale,zscale,rotangl,basex,lineno,minseg,
     & basey, xincr,yincr,zincr,basez,elev,thick,dfltelev,dfltthick

      integer currlay,currcol,oldlay,oldcol
      common/attrs/currlay,currcol,oldlay,oldcol

      character layname*72
      integer numlay,laycolour,layflag
      common/layers/ layname(MAXLAY), numlay, laycolour(MAXLAY),
     &       layflag(MAXLAY)

      integer indxzon
      character oformat*24,head*4
      common/dxfe/oformat,indxzon,head

      integer ilay
      integer nbo  ! local variable for nbobs
      character ZN*12
      character RZNAME*24

      character chartobs*124  ! string buffer for obstructions
      character chartblk*124  ! string buffer for blocks
      common/dxfc/chartobs(500),chartblk(500)
      integer ichartobs       ! how many obstructions found
      integer ichartblk       ! how many blocks found
      common/dxfci/ichartobs,ichartblk 
      character outs*248,fs*1
      character number*3,dstmp*24
      logical OPND,match,unixok

C File names.
      character filname*144,filname2*144,filname1*144,lf*72

      integer itrc,itmprm

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C System parameter initializing. Use ioin for the input file,
C ioout for primary output file, ioblk for block storage,
C iotmp2 for miscel output files.
      ioin=1        
      ioout=2        
      ioblk=3
      iotmp2=11        

      lineno = 0
      elev=0.
      dfltelev=elev
      thick=0.
      dfltthick=thick
      minseg = 6

C Clear text buffers for obstructions.
      ichartobs = 0
      do 466 iloop=1,500
        chartobs(iloop)='  '
        chartblk(iloop)='  '
  466 continue

C The high level scan of a DXF file starts here. It works by
C calling readgc to recover information from the DXF file until
C and EOF is located. It looks for 'HEAD' and 'TABL' and 'ENDSEC'.
      if(itrc.gt.1)then
        call edisp(iuout,' ')
        call edisp(iuout,'Start of debug of DXF file contents...')
      endif
      instring = 'start'
 9007 if (instring(1:3).ne.'EOF')then
        call readgc(itrc)
        if (instring(1:4).eq.'HEAD') then

C HEAD section. Note if header section has a number of entries then
C keep reading until ENDSEC is read.
          if(itrc.gt.1) call edisp(iuout,' ')
          if(itrc.gt.0)call edisp(iuout,' Processing dxf head...')
 9002     if (instring(1:6).ne.'ENDSEC')then
            if (instring(2:5).eq.'SURF') then
              call readgc(itrc)
              minseg=innum
            else
              read(ioin,'(A)') instring
              lineno = lineno + 1
              goto  9002
            endif
            call readgc(itrc)
            goto  9002
          endif
          if(itrc.gt.0)call edisp(iuout, ' Head processed...')
        elseif (instring(1:4).eq.'TABL') then

C TABLE section. Read until we find ENDSEC. We are interested in the
C number of layers and layer colours and layer names.
          if(itrc.gt.1) call edisp(iuout,' ')
          if(itrc.gt.0)call edisp(iuout, ' Tables being processed....')
 9008     if (instring(1:6).ne.'ENDSEC')then
            if (gcode.eq.2 .and. instring(:5).eq.'LAYER') then
              ilay = 0
              call readgc(itrc)
 9009         if (gcode.ne.0 .or.
     &           (gcode.eq.0 .and. instring(1:6).ne.'ENDTAB')) then
                if (gcode.eq.2) then
                  call getlay(ilay,itrc)
                  if (ilay .gt. 0) laycolour(ilay) = 1
                elseif (gcode.eq.62) then
                  if (ilay .gt. 0) laycolour(ilay) = abs(innum)
                elseif (gcode .eq. 70) then
                  if (ilay .gt. 0) layflag(ilay) = innum
                endif
                call readgc(itrc)
                goto 9009
              endif
            else
              read(ioin,'(A)') instring
              lineno = lineno + 1
              goto  9008
            endif
            call readgc(itrc)
            goto  9008
          endif
          if(itrc.gt.0)call edisp(iuout, ' Tables processed....')

C Transform information into esp-r common blocks. Begin by looping
C though known layers, skipping layer `0` and `1`.  But if there are
C only two layers and they are '0' and '1' then we have no named
C layers so warn the user and advise that layer zero data will be
C placed in a zone named First.
           if(oformat(1:3).eq.'esp')then
             if(numlay.eq.1)then
               if(layname(1)(1:1).eq.'0')then
                 call edisp(iuout,
     & 'DXF file has no named layers. Dumping layer 0 into zone First')

C Assuming a single esp-r zone so create file names etc.
                 NCOMP=NCOMP+1
                 write(ZN,'(a)') 'First'
                 ln =lnblnk(ZN)
                 if(itrc.gt.1)then
                   write(outs,'(a,i2,2a)') 'Creating new zone ',
     &               NCOMP,' named ',zn
                   call edisp(iuout,outs)
                 endif
                 NCCODE(NCOMP)=NCOMP
                 write(zname(NCOMP),'(a)') ZN
                 lnzname(NCOMP)=lnblnk(ZN)
                 if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                   write(lf,'(a,a4)') ZN(1:ln),'.geo'
                 else
                   write(lf,'(a,a1,a,a4)') zonepth(1:lnblnk(zonepth)),
     &               fs,ZN(1:ln),'.geo'
                 endif
                 LGEOM(NCOMP)=lf
                 goto 46    ! jump to next step
               endif
             endif
             do 45 i=1,numlay
               ll=lnblnk(layname(i))
               if(layname(i)(1:ll).ne.'0'.and.
     &            layname(i)(1:ll).ne.'1')then
                 call matchl(i,head,RZNAME,match,im)
                 if(itrc.gt.1)then
                   write(outs,*) 'cl match ',i,head,' ',RZNAME,match,im
                   call edisp(iuout,outs)
                 endif
                 if(.NOT.match)then

C Found a new zone so create file names etc.
                   NCOMP=NCOMP+1
                   ln = min0(12,lnblnk(RZNAME))
                   write(ZN,'(a)') RZNAME(1:ln)
                   if(itrc.gt.1)then
                     write(outs,'(a,i2,2a)') 'Creating new zone ',
     &                 NCOMP,' named ',zn
                     call edisp(iuout,outs)
                   endif
                   NCCODE(NCOMP)=NCOMP
                   write(zname(NCOMP),'(a)') ZN
                   lnzname(NCOMP)=lnblnk(ZN)
                   if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                     write(lf,'(a,a4)') ZN(1:ln),'.geo'
                   else
                     write(lf,'(a,a1,a,a4)') zonepth(1:lnblnk(zonepth)),
     &                 fs,ZN(1:ln),'.geo'
                   endif
                   LGEOM(NCOMP)=lf
                 else

C Found subset of an existing zone.  If an obstructions
C layer (starts with OBS_) then create a zone obs file. Add path
C for zone files.

C << need to update to work with v1.1 geometry files with embedded
C << obstructions
                   if(head.eq.'OBS_')then
                     if(IOBS(im).eq.0)then
                       IOBS(im)=1
                       if(zonepth(1:2).eq.'  '.or.
     &                    zonepth(1:2).eq.'./')then
                         write(lf,'(a,a4)') 
     &                     zname(im)(1:lnzname(im)),'.obs'
                       else
                         write(lf,'(a,a1,a,a4)') 
     &                     zonepth(1:lnblnk(zonepth)),fs,
     &                     zname(im)(1:lnzname(im)),'.obs'
                       endif
                       ZOBS(im)=lf
                       if(itrc.gt.1)then
                         write(outs,'(2a)')'Creating obstructions for ',
     &                     zname(im)(1:lnzname(im))
                         call edisp(iuout,outs)
                       endif
                     endif
                   endif
                 endif
               endif
  45         continue

C If obstruction layers (names beginning with "OBS_" use
C ichartobs and chartobs to hold data. Reset ichartobs.

C Continue with configuration file.
  46         write(ioout,'(I7,a)')ncomp,'  # no of zones'

C Write zone information to configuration file depending on format.
C Knowing zones, create other zone files.
             do 44 k2=1,ncomp
               if(icfgv.lt.3)then
                 call usrmsg('Configuration file is an old format. ',
     &             'skipping read... ','W')
                 stop 'ecnv aborted.'
               elseif(icfgv.eq.3.or.icfgv.eq.4)then
                 WRITE(ioout,'(A,I3,A,A)',IOSTAT=IOS,ERR=2)
     &             '*zon ',NCCODE(k2),'   # reference for ',zname(k2)
                 WRITE(ioout,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &             '*opr ',LPROJ(k2)(:LNBLNK(LPROJ(k2))),'  # schedules'
                 WRITE(ioout,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &             '*geo ',LGEOM(k2)(:lnblnk(LGEOM(k2))),'  # geometry'
                 WRITE(ioout,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &             '*con ',LTHRM(k2)(:LNBLNK(LTHRM(k2))),
     &             '  # construction'
                 if(IOBS(k2).eq.1)then
                   WRITE(ioout,'(a,a)',IOSTAT=IOS,ERR=2)'*obs ',
     &               ZOBS(k2)(1:lnblnk(ZOBS(k2)))
                 endif
                 WRITE(ioout,'(A)',IOSTAT=IOS,ERR=2)'*zend '
               endif
  44         continue
           endif
         elseif (instring(1:4).eq.'BLOC') then

C << ? block treatment with esp-r? >>
            call block(itrc)
         elseif (instring(1:4).eq.'ENTI') then

C ENTITY section.
C << ? entity treatment with esp-r? >>
            elev=dfltelev        
            thick=dfltthick        
            xscale=1.      
            yscale=1.       
            zscale=1.    
            rotangl=0.
      
            if(itrc.gt.1) call edisp(iuout,' ')
            if(itrc.gt.0)call edisp(iuout, ' Entities processing...')
            blkxorg=0.0
            blkyorg=0.0
            blkzorg=0.0
            currlay = 0
            currcol = 1
            call readgc(itrc)
 91         if (instring(1:6).ne.'ENDSEC'.and. instring(1:6).ne.
     &                           'ENDBLK') then
              elev=dfltelev        
              thick=dfltthick        
              if (gcode.eq.0.and.instring(1:4).eq.'LINE') then

C Take in 2D lines, if extruded then make into surfaces.
                call genlin(itrc)
              elseif (gcode.eq.0.and.instring(1:3).eq.'ARC') then
                call genarc(itrc)
              elseif (gcode.eq.0.and.instring(1:5).eq.'POINT') then
                if(itrc.gt.0)call edisp(iuout, ' skipping POINT ')
              elseif (instring(1:5).eq.'TRACE') then        
                if(itrc.gt.0)call edisp(iuout, ' skipping TRACE ')
              elseif (instring(1:5).eq.'SOLID') then        
                if(itrc.gt.0)call edisp(iuout, ' skipping SOLID ')
              elseif(instring(1:6).eq.'INSERT') then        
                call insert(itrc)        
              elseif (instring(1:5).eq.'CIRCL') then        
                if(itrc.gt.0)call edisp(iuout, ' skipping CIRCLE ')
                call gencrl(itrc)
              elseif (instring(1:5).eq.'POLYL') then
                call genplin(itrc)
              elseif (instring(1:6).eq.'3DFACE') then
                call face3d(itrc)
              elseif (instring(1:6).eq.'3DLINE') then
                if(itrc.gt.0)call edisp(iuout,' skipping single line ')
              elseif (instring(1:4).eq.'TEXT') then 
                if(itrc.gt.0)call edisp(iuout, ' skipping text ')
                call gentext(itrc)
              elseif (gcode.eq.8) then
                call getlay(currlay,itrc)
                call readgc(itrc)
              elseif (gcode.eq.39) then
                thick = realin
                call readgc(itrc)
              elseif (gcode.eq.62) then
               currcol = innum
               call readgc(itrc)
              else
               call readgc(itrc)
              endif
             goto  91
           endif
           if(itrc.gt.0)call edisp(iuout, ' Entities processed....')
         endif
         goto  9007
      endif

C  Write the picture name in .vew file
      if(oformat(1:6).eq.'viewer')then
        leng=lnblnk(filname2)                
        write(ioout,'(a)') 'NAM'
        write(ioout,'(a,a1,a)') 'AutoCAD',fs,filname2(1:(leng))        
        write(ioout,'(a)') 'END'
        call edisp(iuout, ' ')
        call edisp(iuout, 'ecnv conversion is complete.')
        write(outs,'(2a)') 'dxf2v_finished ',filname2(1:leng)
        call edisp248(iuout,outs,100)

C Close the files, remove temporary files if not to be kept.
        close(ioout)
        if(itmprm.eq.1)then
          INQUIRE(ioblk,OPENED=OPND)
          IF(OPND)CLOSE(ioblk,STATUS='DELETE')
        else
          INQUIRE(ioblk,OPENED=OPND)
          IF(OPND)close(ioblk)
        endif
      elseif(oformat(1:3).eq.'esp')then

C Generate the zone geometry and any obstruction files. These
C facilities require that IC1 IE1 ICT IC2 IE2 and IZSTOCN are
C up to date. Start over and assume everything faces an
C unknown boundary.  Actually in the low and mid level code
C scanning the DXF file the values of izstocn and ncon have 
C been updated. This start-again block of code is for safety.
        NCON=0
        icc=0
        do 57 iv=1,ncomp
          do 58 icz=1,nzsur(iv)
            icc=icc+1
            ncon=ncon+1
            IC1(icc)=iv
            IE1(icc)=icz
            ICT(icc)=-1
            IC2(icc)=0
            IE2(icc)=0
            IZSTOCN(IC1(icc),IE1(icc))=icc
   58     continue
   
C Use the local wegeom to write out a old style of esp-r
C geometry file based on the information available. After
C writing close iotmp2 so that it can be used for the
C connections file.

C << need an alternative to wegeom for version 1.1 >>
          if(itrc.gt.0)then
            WRITE(outs,'(A,I3,A,A)',IOSTAT=IOS,ERR=2)
     &        'Writing zone ',NCCODE(iv),' named ',zname(iv)
            call edisp248(iuout,outs,100)
	  endif
          lf=LGEOM(iv)
          ivv=iv
          call wegeom(iotmp2,ivv,lf,ier)
          close(iotmp2)

C Create a dummy obstruction file for zone. Loop through the
C character arry chartobs and scan for blocks matching the
C curret zone name.
C << what about newer obstruction forms ? >>
          if(IOBS(ivv).eq.1)then
            nbobs(ivv)=0
            do 142 iloop=1,ichartobs
              read(chartobs(iloop),*)ZN,TXO,TYO,TZO,TDX,TDY,TDZ,TBANG
              if(ZN(1:lnblnk(ZN)).eq.zname(ivv)(1:lnzname(ivv)))then
                nbobs(ivv)=nbobs(ivv)+1
                nbo=nbobs(ivv)
                XOB(ivv,nbo)=TXO
                YOB(ivv,nbo)=TYO
                ZOB(ivv,nbo)=TZO
                DXOB(ivv,nbo)=TDX
                DYOB(ivv,nbo)=TDY
                DZOB(ivv,nbo)=TDZ
                BANGOB(ivv,nbo,1)=TBANG
                BANGOB(ivv,nbo,2)=0.0
                BANGOB(ivv,nbo,3)=0.0
                BLOCKTYP(ivv,nbo)='obs '
                write(BLOCKNAME(ivv,nbo),'(a,i2.2)') 'dxf',iloop
              endif
 142        continue
 
C << NOTE: if we switch over to the new geometry format there is no
C << need for a separate obstructions file
 
            CALL EFOPSEQ(iotmp2,ZOBS(ivv),3,IER)
            write(iotmp2,'(a,a)')
     &        '# DUMMY site obstruction file defined in ',
     &        ZOBS(ivv)(1:lnblnk(ZOBS(ivv)))
            write(iotmp2,'(a,a)')'# associated with zone geom file ',
     &        LGEOM(ivv)(1:lnblnk(LGEOM(ivv)))
            write(iotmp2,'(a)')' 0.0  0.0  # dummy valuse'
            write(iotmp2,'(I4,a)') nbobs(ivv),
     &        '     # no obstruction blocks'
            write(iotmp2,'(a)')
     &       '# origin  X   Y   Z   width  depth  height  angle  descr'
            do 144 ibx=1,nbobs(ivv)
              write(iotmp2,'(7f9.4,3a)')XOB(ivv,ibx),YOB(ivv,ibx),
     &          ZOB(ivv,ibx),DXOB(ivv,ibx),DYOB(ivv,ibx),
     &          DZOB(ivv,ibx),BANGOB(ivv,ibx,1),' ',
     &          BLOCKNAME(ivv,ibx),' # block '
 144        continue
            write(iotmp2,'(a)')'# grid opq X opq Z win X win Z'
            write(iotmp2,'(a)')'    20    20     5     5'
            close(iotmp2)
          endif
  57    continue

C Write out the connections (assuming everything exterior). Check to see
C that LCNN is something other than UNKNOWN.
        if(LCNN(1:7).eq.'UNKNOWN')then
          write(LCNN,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.cnn'
        endif
        if(icfgv.eq.3.or.icfgv.eq.4)then
          WRITE(ioout,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &        '*cnn  ',LCNN(1:lnblnk(LCNN)),'  # connections '
          CALL EFOPSEQ(iotmp2,LCNN,3,IER)
          WRITE(iotmp2,'(A,A)',IOSTAT=IOS,ERR=2) '*connections  for ',
     &      cfgroot(1:lnblnk(cfgroot))
          call dstamp(dstmp)
          WRITE(iotmp2,'(A,A)',IOSTAT=IOS,ERR=3) '*Date ',dstmp
          WRITE(iotmp2,'(I7,A)',IOSTAT=IOS,ERR=2)
     &        NCON,'   # number of connections'
        endif
        do 61 iv=1,ncomp
          do 66 is=1,NZSUR(iv)
            write(iotmp2,'(2i4,a)')iv,is,'  -1   0   0'
  66      continue
  61    continue
        write(ioout,'(a)')'      0   # no mass flow analysis'
        if(icfgv.eq.3.or.icfgv.eq.4)close(iotmp2)
        close(ioout)

        if(itmprm.eq.1)then
          INQUIRE(ioblk,OPENED=OPND)
          IF(OPND)CLOSE(ioblk,STATUS='DELETE')
        else
          INQUIRE(ioblk,OPENED=OPND)
          IF(OPND)close(ioblk)
        endif
      endif

C Dump out layer names and equivalent number.
      leng=lnblnk(filname2)                
      write(filname2,'(a,a4)')filname(1:leng),'.lay'        
      open(ioout,file=filname2,status='UNKNOWN', err=901)
      write(ioout,'(a,a)') 'Layer names used in the file : ',
     &      filname1(1:lnblnk(filname1))
      write(ioout,'(a)') 'Number  Name       Default Colour'
      do 10 i = 1,numlay
        if (laycolour(i) .gt. 0) then
           write(number,'(i3)') laycolour(i)
        else
           number = ' - '
        endif
        write(ioout,1000) i, layname(i)(1:15), number
 1000   format('  ',i3,'    : ',a,'  ',a)
 10   continue
      close(ioout)
      call edisp(iuout,' End of conversion.')

      return

 901  write(outs,'(2a)') 'Error opening "NEW" file :',
     &     filname2(:lnblnk(filname2))
      call edisp248(iuout,outs,100)
      goto 999

    2 CALL edisp(iuout,' Problem writing config data config...')
      close(ioout)
      goto 999

    3 CALL edisp(iuout,' Problem writing configuration name...')
      close(ioout)
      goto 999

 999  stop 'ecnv aborted.'
      end

C ******** block
c BLOCK processes the block definitions in .dxf file
c of Autocad and create a /tmp/blockdxf2e file for later
c 'INSERT' command use.

C << NOTE: for some computer types there will not be >>
C << a /tmp folder so work out a different approach >>
C << possibly using memory >>
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

      real flag3d,dxfconv
      integer flagins
      common/fg/flag3d,flagins,dxfconv

      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk

      real elevlast,thicklast
      integer polylinflag
      common/vex/elevlast,thicklast,polylinflag

      integer currlay,currcol,oldlay,oldcol
      common/attrs/currlay,currcol,oldlay,oldcol

      integer indxzon
      character oformat*24,head*4
      common/dxfe/oformat,indxzon,head

      character chartobs*124  ! string buffer for obstructions
      character chartblk*124  ! string buffer for blocks
      common/dxfc/chartobs(500),chartblk(500)
      integer ichartobs       ! how many obstructions found
      integer ichartblk       ! how many blocks found
      common/dxfci/ichartobs,ichartblk 

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
C << consider adding to this list >>
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
      
C If the input string is something other than ENDBLK then
C parse the set of commands that can be used. Skip those
C that we cannot use (SOLID, CIRCLE, POINT, TEXT).
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

C << can we do something with 3DLIN? >>
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
          call getlay(currlay,itrc)   ! layer name
          call matchl(currlay,head,RZNAME,match,im)
          if(match)indxzon=im
          call readgc(itrc)
        elseif (gcode.eq.62) then
          currcol = innum   ! the colour index
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

      real flag3d,dxfconv
      integer flagins
      common/fg/flag3d,flagins,dxfconv

      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk

      integer currlay,currcol,oldlay,oldcol
      common/attrs/currlay,currcol,oldlay,oldcol
      common/matrix/ amat(3,3), xnorm, ynorm, znorm

      integer indxzon
      character oformat*24,head*4
      common/dxfe/oformat,indxzon,head

      character chartobs*124  ! string buffer for obstructions
      character chartblk*124  ! string buffer for blocks
      common/dxfc/chartobs(500),chartblk(500)
      integer ichartobs       ! how many obstructions found
      integer ichartblk       ! how many blocks found
      common/dxfci/ichartobs,ichartblk 

      character bname*72,pbname*72,chr*1
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

C All imported block definition files come from (blockdxf2e).

C << NOTE: to be done >>
C << there will not be a /tmp folder on Native Windows >>
C << find another place to put this temporary file >>
C << or consider using memory rather than scratch files >>

      write(pbname,'(a)') '/tmp/blockdxf2e'
      open(ioblk,file=pbname,status='UNKNOWN')

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

C This assumes that obstructions are in blocks named obstruction
C and the data is written to an array of strings for later scanning.

C << extend to include additional rotations and
C << tilts and opacity and ??

              if(bname(1:11).eq.'obstruction')then
                call extrblk(itrc,bname,coorx,coory,coorz,
     &                     xscale,yscale,zscale,rotangl,ier)
                ichartobs=ichartobs+1
                write(chartobs(ichartobs),'(a12,2x,7f9.4)')
     &            RZNAME(1:12),
     &            coorx*dxfconv,coory*dxfconv,coorz*dxfconv,
     &            xscale*dxfconv,yscale*dxfconv,zscale*dxfconv,rotangl

C Debug.
                write(6,*) 'chartobs=',ichartobs,' ',
     &            chartobs(ichartobs)
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

      character layname*72
      integer numlay,laycolour,layflag
      common/layers/ layname(MAXLAY), numlay, laycolour(MAXLAY),
     &       layflag(MAXLAY)

      common/laytoo/ layuse(MAXLAY)
      character outs*124

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
      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk

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

      real flag3d,dxfconv
      integer flagins
      common/fg/flag3d,flagins,dxfconv

      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk
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

      real flag3d,dxfconv
      integer flagins
      common/fg/flag3d,flagins,dxfconv

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
