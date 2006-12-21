C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C Filter between various design tools.
C AutoCAD Notes:
C   it traps some AEC and AutoCAD constructs  which are
C   not in keeping with energy analysis. It assumes CAD units
C   a mm (unless otherwise instructed).  AutoCAD block definitions
C   are held in a file `blockdxf2e` which is deleted unless otherwise
C   instructed.

C Portions of the DXF translation can be traced back (eventually) 
C to code from ABACUS, Department of Architecture & Building Science.
      program ecnv
#include "building.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure database)
C LOPTDB,IOPTDB (for optical database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)

      parameter (MAXLAY = 80)
      integer gcode,innum,ioin,ioout,ioblk,flagins
      integer currlay, currcol, oldlay, oldcol
      common/trn/xscale,yscale,zscale,rotangl,basex,lineno,minseg,
     & basey, xincr,yincr,zincr,basez,elev,thick,dfltelev,dfltthick
      common/rd/gcode,instring,innum,realin,coorx,coory,coorz,
     &           blkxorg,blkyorg,blkzorg
      common/fg/flag3d,flagins,radians,dxfconv
      common/io/ioin,ioout,ioblk,iosblk,iotobs
      common/rp/repfile,repelev,repthick,nrep
      common/attrs/ currlay, currcol, oldlay, oldcol
      common/layers/ layname(MAXLAY), numlay, laycolour(MAXLAY),
     &       layflag(MAXLAY)
      common/laytoo/ layuse(MAXLAY)
      common/dxfe/oformat,indxzon,head

C ESP-r commons.
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/appw/iappw,iappx,iappy
      common/rpath/path
      common/uhome/upath
      common/udot/esprc

C Where default db list are kept.
      common/defdb/dfdblbl,defdbfl
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      common/C3F/LCNN
      common/CFGV/icfgv
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)

      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),BANG(MB)
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      
C Path to SHOCC input files
C bSHOCCed     - logical flag whether project has SHOCC input
C bZoneSHOCCed - logical array whether zones have SHOCC input
C SHOCCshlFile - path to SHOCC .shl file for SHOCC'd project library
C SHOCCshzFile - path to SHOCC .shz file for each SHOCC'd zone
      common/SHOCCcfg/bSHOCCed,SHOCCshlFile,bZoneSHOCCed(mcom),
     &                SHOCCshzFile(mcom)
      logical bSHOCCed,bZoneSHOCCed
      character SHOCCshlFile*72,SHOCCshzFile*72

      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      character filname*72,filname2*72,filname1*72,repfile*40
      character inf*72,ouf*72,layname*72,instring*72,lf*72
      character iformat*24,oformat*24,number*3,fs*1
      character zname*12,zdesc*64,RZNAME*24,head*4,ZN*12,LTMP*72
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character path*72,upath*72,esprc*72,LCNN*72
      character dfdblbl*20,defdbfl*72,upgcfg*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      character cfgroot*24,dstmp*24

      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,LCFGF
      character htmlfile*72
      character ltobs*72,outs*124
      logical OPND,match,doobs,unixok

C System parameter initializing. Use ioin for the input file,
C ioout for primary output file, ioblk for block storage,
C iotmp2 for miscel output files. Iotobs is obstruction block
C storage.
      call ezero
      call curmodule('ecnv')
      lineno = 0
      nrep=0        
      ioin=1        
      ioout=2        
      ioblk=3
      iotmp2=11        
      iotobs=13        
      iosblk=14
      IUOUT=6
      IUIN=5
      LIMTTY=20
      LIMIT=20
      itrc=1
      NSIGFIG=3

C Get command line parameters.
      call parcnv(itrc,conv,incobs,ichop,itmprm,iformat,oformat,inf,ouf,
     &  upgcfg)
      xincr=0        
      yincr=0        
      zincr=0        
      rotangl=0.        
      xscale=1.        
      yscale=1.        
      zscale=1.        
      elev=0.
      dfltelev=elev
      thick=0.
      dfltthick=thick
      flagins=0        
      basex=0
      basey=0
      basez=0
      oldlay = 0
      oldcol = 1
      currlay = 0
      currcol = 1
      PI = 4.0 * ATAN(1.0)
      radians = PI/180.
      minseg = 6
      dxfconv = 1.0

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

C Assume construction input from 15 and materials from 16.
      IFMUL=15
      IFMAT=16
      IOPTDB=17

C Assume there are no obstructions.
      doobs=.FALSE.

      call edisp(IUOUT,' ')
      write(outs,'(2a)')
     &  ' ESP-r Data Conversion: Version 2.5a of January 2006.',
     &  ' Copyright 2001-6 Energy'
      call edisp(IUOUT,outs)
      write(outs,'(2a)')
     & ' Systems Research Unit, University of',
     & ' Strathclyde, Glasgow, Scotland.'
      call edisp(IUOUT,outs)

C Find the user's home folder then get user's custom settings.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(a,a,a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(a,a,a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,iotmp2,0,IIER)

C Convert from "c" strings to fortran strings, if unknown then exit.

C Get input file and format.
      filname=inf
      write(outs,'(a,a)')'Input file is: ',filname(1:lnblnk(filname))
      call edisp(iuout,outs)
      if(iformat(1:3).eq.'dxf')then

C Determine root name of dxf file (to make layer file from).
        leng=lnblnk(filname)                
        if (filname(leng-3:leng).eq.'.dxf'.or.
     &      filname(leng-3:leng).eq.'.DXF')then
          leng = leng - 4
          filname1=filname
        else
          write(filname1,'(2a)') filname(1:leng),'.dxf'
        endif       
      elseif(iformat(1:3).eq.'zip')then
        filname1=filname
      elseif(iformat(1:3).eq.'esp')then
        filname1=filname
        call fdroot(filname1,path,LCFGF)
      elseif(iformat(1:2).eq.'ww')then
        filname1=filname
      else
        call edisp(iuout,'ecnv_abort unknown_input_format')
        stop 'ecnv aborted.'
      endif     

      if(filname.eq.' '.or.filname.eq.'UNKNOWN')then
        call edisp(iuout,'ecnv_error unknown_input_file')
        stop 'ecnv aborted.'
      endif     

C Get output format. If viewer then use a single file, if
C ESP-r then a series of files will be opened.
      filname2=ouf
      if(filname2.eq.' '.or.filname2.eq.'UNKNOWN')then
        call edisp(iuout,'ecnv_error unknown_output_file')
        stop 'ecnv aborted.'
      endif
      if(oformat(1:6).eq.'viewer')then
        write(outs,'(a,a)')'The viewer output file will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp(iuout,outs)
      elseif(oformat(1:5).eq.'tsbi3')then
        write(outs,'(a,a)')'The tsbi3 output file will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp(iuout,outs)
      elseif(oformat(1:4).eq.'vrml')then
        write(outs,'(a,a)')'The vrml world will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp(iuout,outs)
      elseif(oformat(1:3).eq.'esp')then

C If esp then also find its path so zone files can be
C placed in the same folder.
        write(outs,'(a,a)')'The esp-r configuration file will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp(iuout,outs)
        call fdroot(filname2,path,LCFGF)
        if(upgcfg(1:7).ne.'UNKNOWN')then

C Instanciate and clear commons, scan the supplied problem
C to update commons for configuration.
          call blnksys
          call edisp(iuout,' Scanning the problem before updating...')
          call escdef(dfdblbl,defdbfl,iotmp2,IER)
          call ESCSYS(LCFGF,ioout,iotmp2,IER)
          if(IER.eq.0)then
            CFGOK=.TRUE.
          else
            call edisp(iuout,'ecnv_error corrupt_update_file')
            stop 'ecnv aborted.'
          endif
        else
          call escdef(dfdblbl,defdbfl,iotmp2,IER)
          call blnksys
        endif
      elseif(oformat(1:3).eq.'dxf')then

C If AutoCAD then also find its path so that support files can be
C placed in the same folder.
        write(outs,'(a,a)')'The AutoCAD DXF file will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp(iuout,outs)
      elseif(oformat(1:6).eq.'zip')then
        write(outs,'(a,a)')'The zip output file will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp(iuout,outs)
      elseif(oformat(1:4).eq.'xfig')then
        write(outs,'(a,a)')'The xfig output file will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp(iuout,outs)
      elseif(oformat(1:3).eq.'THF'.or.oformat(1:3).eq.'thf')then
        write(outs,'(a,a)')'The THF output file will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp(iuout,outs)
      else
        call edisp(iuout,'ecnv_error unknown_output_format')
        stop 'ecnv aborted.'
      endif
    

C If input format is zip and output is esp go and do this.
      if(iformat(1:3).eq.'zip')then
        if(oformat(1:3).eq.'esp'.or.oformat(1:6).eq.'viewer')then
          open(ioin ,file=filname1,status='OLD', err=900)
          open(ioout,file=filname2,status='UNKNOWN', err=901)
          call zipcnv(upgcfg,filname2,itrc,IER)
          close(ioout)
          close(ioin)
          stop
        endif
      endif

C If input format is ww and output is xfig go and do this.
      if(iformat(1:2).eq.'ww'.and.oformat(1:4).eq.'xfig')then
        open(ioin ,file=filname1,status='OLD', err=900)
        open(ioout,file=filname2,status='UNKNOWN', err=901)
        call wwxfig(filname1,IER)
        close(ioout)
        close(ioin)
        stop
      endif

C If input format is esp and output is viewer, tsbi3, thf, dxf or
C vrml do this.
      if(iformat(1:3).eq.'esp')then

C Assume that the configuration, mlc db, primitive db, control file
C have not been read in.
        CFGOK=.FALSE.
        MLDBOK=.FALSE.
        MATDBOK=.FALSE.
        CTLOK=.FALSE.

C Scan the defaults file for default configuration.
C Make temporary use of file unit iotmp2.
        call escdef(dfdblbl,defdbfl,iotmp2,IER)
        LTMP=filname

C Find the path and local file name.
        call fdroot(LTMP,path,LCFGF)
        call edisp(iuout,' Scanning the model description...')
        call ESCSYS(LCFGF,ioin,iotmp2,IER)
        if(IER.eq.0)then
          CFGOK=.TRUE.
        else
          call edisp(iuout,'ecnv_error corrupt_input_file')
          stop 'ecnv aborted.'
        endif

        if(oformat(1:6).eq.'viewer'.or.oformat(1:3).eq.'dxf')then
          open(ioout,file=filname2,status='UNKNOWN', err=901)
          call e2vdxf(itrc,incobs,ichop)
          call edisp(iuout,' End of conversion.')
          CALL EPAGEND
          STOP
        endif

C Create THF (THINGS) file.
        if(oformat(1:3).eq.'THF'.or.oformat(1:3).eq.'thf')then
          open(ioout,file=filname2,status='UNKNOWN', err=901)
          call e2thf(itrc,incobs,ichop)
          call edisp(iuout,' End of conversion.')
          CALL EPAGEND
          STOP
        endif

        if(oformat(1:6).eq.'zip')then
          open(ioout,file=filname2,status='UNKNOWN', err=901)
          WRITE(ioout,'(a)') 'COM data file converted from `esp`'
          WRITE(ioout,'(a)') 'NAME'
          WRITE(ioout,'(a,a,a)') '"',filname2(1:lnblnk(filname2)),'"'
          WRITE(ioout,'(a)') 'COM DATE ....'
          WRITE(ioout,'(a)') ' '
          WRITE(ioout,'(a)') 'ESP'
          WRITE(ioout,'(a)') 'ANT'
          call e2zip(itrc,incobs,ichop)
          CALL ERPFREE(ioout,ISTAT)
          call edisp(iuout,' End of conversion.')
          CALL EPAGEND
          STOP
        endif
        if(oformat(1:5).eq.'tsbi3')then
          open(ioout,file=filname2,status='UNKNOWN', err=901)
          WRITE(ioout,'(a)') '#TSBI3-B 11'
          WRITE(ioout,'(a)') '#comment'
          WRITE(ioout,'(a)') 'Data converted from esp config. file'
          WRITE(ioout,'(a)')  filname(:lnblnk(filname))
          WRITE(ioout,'(a)') ' '
          call e2tsbi3(itrc,incobs,ichop)
          CALL ERPFREE(ioout,ISTAT)
          call edisp(iuout,' End of conversion.')
          CALL EPAGEND
          STOP
        endif
        if(oformat(1:4).eq.'vrml')then
          write(htmlfile,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.html'
          open(ioblk,file=htmlfile,status='UNKNOWN', err=901)
          write(ioblk,'(a)')  '<html>'
          write(ioblk,'(a)')  '<head>'
          write(ioblk,'(3a)') '<title>',cfgroot,'</title>'
          write(ioblk,'(a)')  '</head>'
          write(ioblk,'(a)')  '<body>'

          open(ioout,file=filname2,status='UNKNOWN', err=901)
          WRITE(ioout,'(a)') '#VRML V2.0 utf8'
          WRITE(ioout,'(a)') ' '
          call e2vrml(itrc,ichop)
          CALL ERPFREE(ioout,ISTAT)
          CALL ERPFREE(ioblk,ISTAT)
          call edisp(iuout,' End of conversion.')
          CALL EPAGEND
          STOP
        endif
      endif

C Input is from AutoCAD.
      open(ioin ,file=filname1,status='OLD', err=900)
      open(ioout,file=filname2,status='UNKNOWN', err=901)

C Confirm conversion of scale.
      dxfconv=conv
      if(dxfconv.gt.2.0.or.dxfconv.lt.0.0001)then
        call edisp(iuout,'dxf2v_error range_of_conversion')
        stop 'ecnv aborted.'
      endif

C If esp-r then generate initial portion of configuration file.
      if(oformat(1:3).eq.'esp')then
        LSNAM='Translation from AutoCAD'
        call mksyshd(filname2,ier)
      endif

C The general control part of the program
      instring = 'start'
 9007 if (instring(1:3).ne.'EOF')then
        call readgc
        if (instring(1:4).eq.'HEAD') then

C HEAD section. Note if header section has a number of entries then
C keep reading until ENDSEC is read.
          if(itrc.gt.0)call edisp(iuout,' Processing dxf head...')
 9002     if (instring(1:6).ne.'ENDSEC')then
            if (instring(2:5).eq.'SURF') then
              call readgc
              minseg=innum
            else
              read(ioin,'(A)') instring
              lineno = lineno + 1
              goto  9002
            endif
            call readgc
            goto  9002
          endif
          if(itrc.gt.0)call edisp(iuout, ' Head processed...')
        elseif (instring(1:4).eq.'TABL') then

C TABLE section.
          if(itrc.gt.0)call edisp(iuout, ' Tables being processed....')
 9008     if (instring(1:6).ne.'ENDSEC')then
            if (gcode.eq.2 .and. instring(:5).eq.'LAYER') then
              ilay = 0
              call readgc
 9009         if (gcode.ne.0 .or.
     &           (gcode.eq.0 .and. instring(1:6).ne.'ENDTAB')) then
                if (gcode.eq.2) then
                  call getlay(ilay)
                  if (ilay .gt. 0) laycolour(ilay) = 1
                elseif (gcode.eq.62) then
                  if (ilay .gt. 0) laycolour(ilay) = abs(innum)
                elseif (gcode .eq. 70) then
                  if (ilay .gt. 0) layflag(ilay) = innum
                endif
                call readgc
                goto 9009
              endif
            else
              read(ioin,'(A)') instring
              lineno = lineno + 1
              goto  9008
            endif
            call readgc
            goto  9008
          endif
          if(itrc.gt.0)call edisp(iuout, ' Tables processed....')

C Transform information into esp-r common blocks. Begin by looping
C though known layers, skipping layer `0` and `1`.
           if(oformat(1:3).eq.'esp')then
             do 45 i=1,numlay
               ll=lnblnk(layname(i))
               if(layname(i)(1:ll).ne.'0'.and.
     &            layname(i)(1:ll).ne.'1')then
                 call matchl(i,head,RZNAME,match,im)
                 if(.NOT.match)then

C Found a new zone so create file names etc.
                   NCOMP=NCOMP+1
                   ln = min0(12,lnblnk(RZNAME))
                   write(ZN,'(a)') RZNAME(1:ln)
                   NCCODE(NCOMP)=NCOMP
                   write(zname(NCOMP),'(a)') ZN
                   if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                     write(lf,'(a,a4)') ZN(1:ln),'.geo'
                   else
                     write(lf,'(a,a1,a,a4)') zonepth(1:lnblnk(zonepth)),
     &                 fs,ZN(1:ln),'.geo'
                   endif
                   LGEOM(NCOMP)=lf
                 else

C Found subset of an existing zone.  If obstructions
C layer then we know about some utilities.  Add path so
C that zone files live in same folder as the configuration file.
                   if(head.eq.'OBS_')then
                     if(IOBS(im).eq.0)then
                       IOBS(im)=1
                       doobs=.TRUE.
                       if(zonepth(1:2).eq.'  '.or.
     &                    zonepth(1:2).eq.'./')then
                         write(lf,'(a,a4)') 
     &                     zname(im)(1:lnblnk(zname(im))),'.obs'
                       else
                         write(lf,'(a,a1,a,a4)') 
     &                     zonepth(1:lnblnk(zonepth)),fs,
     &                     zname(im)(1:lnblnk(zname(im))),'.obs'
                       endif
                       ZOBS(im)=lf
                     endif
                   endif
                 endif
               endif
  45         continue

C If obstruction layers (names beginning with "OBS_" make
C a scratch file to collect data.
             ltobs='/tmp/tobs'
             open(iotobs,file=ltobs(1:lnblnk(ltobs)),status='UNKNOWN',
     &            err=903)

C Continue with configuration file.
             write(ioout,'(I7,a)')ncomp,'  # no of zones'

C Write zone information to configuration file depending on format.
C Knowing zones, create utility files.
             do 44 k2=1,ncomp
               if(icfgv.lt.3)then
                 call usrmsg('Configuration file is an old format. ',
     &             'skipping read... ','W')
                 stop 'ecnv aborted.'
               elseif(icfgv.eq.3)then
                 WRITE(ioout,'(A,I3,A,A)',IOSTAT=IOS,ERR=2)
     &             '*zon ',NCCODE(k2),'   # reference for ',zname(k2)
                 WRITE(ioout,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &             '*opr ',LPROJ(k2)(:LNBLNK(LPROJ(k2))),'  # schedules'
                 WRITE(ioout,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &             '*geo ',LGEOM(k2)(:lnblnk(LGEOM(k2))),'  # geometry'
                 WRITE(ioout,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &             '*con ',LTHRM(k2)(:LNBLNK(LTHRM(k2))),
     &             '  # construction'
                 WRITE(ioout,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &             '*shz ',SHOCCshzFile(k2)(:LNBLNK(SHOCCshzFile(k2))),
     &             '  # SHOCC'
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
      
            if(itrc.gt.0)call edisp(iuout, ' Entities processing...')
            blkxorg=0.0
            blkyorg=0.0
            blkzorg=0.0
            currlay = 0
            currcol = 1
            call readgc
 91         if (instring(1:6).ne.'ENDSEC'.and. instring(1:6).ne.
     &                           'ENDBLK') then
              elev=dfltelev        
              thick=dfltthick        
              if (gcode.eq.0.and.instring(1:4).eq.'LINE') then

C Take in 2D lines, if extruded then make into surfaces.
                call genlin
              elseif (gcode.eq.0.and.instring(1:3).eq.'ARC') then
                call genarc
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
                call gencrl
              elseif (instring(1:5).eq.'POLYL') then
                call genplin
              elseif (instring(1:6).eq.'3DFACE') then
                call face3d
              elseif (instring(1:6).eq.'3DLINE') then
                if(itrc.gt.0)call edisp(iuout,' skipping single line ')
              elseif (instring(1:4).eq.'TEXT') then 
                if(itrc.gt.0)call edisp(iuout, ' skipping text ')
                call gentext
              elseif (gcode.eq.8) then
                call getlay(currlay)
                call readgc
              elseif (gcode.eq.39) then
                thick = realin
                call readgc
              elseif (gcode.eq.62) then
               currcol = innum
               call readgc
              else
               call readgc
              endif
             goto  91
           endif
           if(itrc.gt.0)call edisp(iuout, ' Entities processed....')
         endif
         goto  9007
      endif

C  Write the picture name in .vew file
      if(oformat(1:6).eq.'viewer')then
        write(ioout,'(a)') 'NAM'
        write(ioout,'(a,a1,a)') 'AutoCAD',fs,filname2(1:(leng))        
        write(ioout,'(a)') 'END'
        call edisp(iuout, ' ')
        call edisp(iuout, 'ecnv conversion is complete.')
        write(outs,'(a,a)') 'dxf2v_finished ',
     &                    filname2(1:lnblnk(filname2))
        call edisp(iuout,outs)

C Close the files, remove temporary files if not to be kept.
        close(ioout)
        if(itmprm.eq.1)then
          INQUIRE(ioblk,OPENED=OPND)
          IF(OPND)CLOSE(ioblk,STATUS='DELETE')
          if(itrc.gt.0)then
            INQUIRE(iosblk,OPENED=OPND)
            close(iosblk,STATUS='DELETE')
          endif
        else
          INQUIRE(ioblk,OPENED=OPND)
          IF(OPND)close(ioblk)
          if(itrc.gt.0)close(iosblk)
        endif
      elseif(oformat(1:3).eq.'esp')then

C Generate the zone geometry and any obstruction files.
        NCON=0
        do 57 iv=1,ncomp
          ncon=ncon+NZSUR(iv)
          lf=LGEOM(iv)
          ivv=iv
          call wegeom(iotmp2,ivv,lf,ier)

C Create a dummy obstruction file for zone. Rewind the temporary
C block store file and scann for blocks matching the current
C zone.
          if(IOBS(ivv).eq.1)then
            NB=0
            INQUIRE(iotobs,OPENED=OPND)
            if(OPND)then
              rewind(iotobs,ERR=999)
            else
              ltobs='/tmp/tobs'
              open(iotobs,file=ltobs(1:lnblnk(ltobs)),status='UNKNOWN',
     &            err=903)
            endif
  142       read(iotobs,*,end=143)ZN,TXO,TYO,TZO,TDX,TDY,TDZ,TBANG
            if(ZN(:lnblnk(ZN)).eq.zname(ivv)(:lnblnk(zname(ivv))))then
              NB=NB+1
              XO(NB)=TXO
              YO(NB)=TYO
              ZO(NB)=TZO
              DX(NB)=TDX
              DY(NB)=TDY
              DZ(NB)=TDZ
              BANG(NB)=TBANG
            endif
            goto 142
 143        continue
            CALL EFOPSEQ(iotmp2,ZOBS(ivv),3,IER)
            write(iotmp2,'(a,a)')
     &        '# DUMMY site obstruction file defined in ',
     &        ZOBS(ivv)(1:lnblnk(ZOBS(ivv)))
            write(iotmp2,'(a,a)')'# associated with zone geom file ',
     &        LGEOM(ivv)(1:lnblnk(LGEOM(ivv)))
            write(iotmp2,'(a)')' 0.0  0.0  # dummy valuse'
            write(iotmp2,'(I4,a)') NB,'     # no obstruction blocks'
            write(iotmp2,'(a)')
     &       '# origin  X   Y   Z   width  depth  height  angle  descr'
            do 144 ibx=1,NB
              write(iotmp2,'(7f9.4,a)')XO(ibx),YO(ibx),ZO(ibx),
     &           DX(ibx),DY(ibx),DZ(ibx),BANG(ibx),' dxf # block '
 144        continue
            write(iotmp2,'(a)')'# grid opq X opq Z win X win Z'
            write(iotmp2,'(a)')'    20    20     5     5'
            close(iotmp2)
          endif
  57    continue

C Write out the connections (assuming everything exterior).
        if(icfgv.eq.3)then
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
            write(iotmp2,'(2i4,a)')iv,is,'   0   0   0'
  66      continue
  61    continue
        write(ioout,'(a)')'      0   # no mass flow analysis'
        if(icfgv.eq.3)close(iotmp2)
        close(ioout)

        if(itmprm.eq.1)then
          INQUIRE(ioblk,OPENED=OPND)
          IF(OPND)CLOSE(ioblk,STATUS='DELETE')
          if(itrc.gt.0)then
            INQUIRE(iosblk,OPENED=OPND)
            close(iosblk,STATUS='DELETE')
          endif
          if(doobs)then
            INQUIRE(iotobs,OPENED=OPND)
            IF(OPND)close(iotobs,STATUS='DELETE')
          endif
        else
          INQUIRE(ioblk,OPENED=OPND)
          IF(OPND)close(ioblk)
          if(itrc.gt.0)close(iosblk)
          if(doobs)close(iotobs)
        endif
      endif

C Dump out layer names and equivalent number.
      write(filname2,'(a,a4)')filname(1:(leng)),'.lay'        
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
        write(ioout,1000) i, layname(i)(:15), number
 1000   format('  ',i3,'    : ',a,'  ',a)
 10   continue
      close(ioout)
      call edisp(iuout,' End of conversion.')
      CALL EPAGEND
      STOP

C Error messages.
 900  write(outs,'(a,a)') 'Error opening "OLD" file :',
     &     filname1(:lnblnk(filname1))
      call edisp(iuout,outs)
      goto 999
 901  write(outs,'(a,a)') 'Error opening "NEW" file :',
     &     filname2(:lnblnk(filname2))
      call edisp(iuout,outs)
      goto 999

 903  call edisp(iuout,'Error opening or writing temp obstr file.')
      close(iotobs)
      stop

    2 CALL edisp(iuout,' Problem writing config data config...')
      close(ioout)
      goto 999

    3 CALL edisp(iuout,' Problem writing configuration name...')
      close(ioout)
      goto 999

 999  stop 'ecnv aborted.'
      end

C **** wegeom
C write esp geometry file.
      subroutine wegeom(iotmp2,iv,lf,ier)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/ZNDATA/VCOORD(MCOM,MTV,3),NZNVER(MCON),NZJVN(MCON,MV)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)

C Use YOS to hold dummy values for surface indentation.
      dimension ndum(MS),Y0S(MS)
      character lf*72,zname*12,zdesc*64,SN*12,SOTF*4,louts*248
       
      ier=0
      CALL EFOPSEQ(iotmp2,lf,3,IER)
      if(ier.ne.0)goto 903
      write(iotmp2,'(a,a,a,a)')'# geometry of ',
     &       zname(iv)(1:lnblnk(zname(iv))),' defined in: ',
     &       lf(1:lnblnk(lf))
      write(iotmp2,'(a,a)')'GEN ',zname(iv)(1:lnblnk(zname(iv)))
      write(iotmp2,'(I8,I8,a)')NZTV(iv),NZSUR(iv),
     &      '   0.000    # vertices, surfaces, rotation angle'
      write(iotmp2,'(a)')'#  X co-ord, Y co-ord, Z co-ord '
      do 58 ivv=1,NZTV(iv)
            write(iotmp2,'(3f10.3)')VCOORD(iv,ivv,1),VCOORD(iv,ivv,2),
     &         VCOORD(iv,ivv,3)
  58  continue
      write(iotmp2,'(a)')'# no of vert & list of associated vert'
      do 59 is=1,NZSUR(iv)
        icc=IZSTOCN(iv,is)
        WRITE(iotmp2,5650)NZNVER(icc),
     &          (NZJVN(icc,J),J=1,NZNVER(icc))
5650    FORMAT(1X,19(I3,','))
        ndum(is)=0
        Y0S(is)=0.0
  59  continue

C Write out sets of packed strings, for unused index and indentation.
      WRITE(iotmp2,'(a)',IOSTAT=ios,ERR=13) '# unused index'
      itrunc=1
      ipos=1
      do while (itrunc.ne.0)
        call ailist(ipos,NZSUR(iv),ndum,MS,'C',louts,loutln,itrunc)
        write(iotmp2,'(1x,a)',IOSTAT=ios,ERR=14) louts(1:loutln)
        ipos=itrunc+1
      end do
      WRITE(iotmp2,'(a)',IOSTAT=ios,ERR=13) '# surfaces indentation (m)'
      itrunc=1
      ipos=1
      do while (itrunc.ne.0)
        call arlist(ipos,nsur,Y0S,MS,'C',louts,loutln,itrunc)
        write(iotmp2,'(1x,a)',IOSTAT=ios,ERR=14) louts(1:loutln)
        ipos=itrunc+1
      end do
      write(iotmp2,'(a)')'  3 0 0 0  # def insolation distribution'

      write(iotmp2,'(a)')'# surface attributes follow: '
      write(iotmp2,'(a)')
     &      '# id  surface      geom  loc/  mlc db       environment'
      write(iotmp2,'(a)')
     &      '# no  name         type  posn  name         other side'
      do 60 is=1,NZSUR(iv)
        IF(is.LE.9)WRITE(SN,'(a5,i1)')'Surf-',is
        IF(is.GT.9)WRITE(SN,'(a5,i2)')'Surf-',is
        SOTF='OPAQ'
        if(itmcfl(iv,is).eq.1)SOTF='TRAN'
        write(iotmp2,'(i3,a2,a12,a2,a4,a)')is,', ',SN,'  ',SOTF,
     &        '  UNKN  UNKNOWN      UNKNOWN'
  60  continue
      return

 903  call edisp(iuout,'ERROR opening or writing esp-r zone geom file.')
      close(iotmp2)
      ier=1
      return

   13 if(IOS.eq.2)then
        CALL USRMSG(' No permission to write ',lf,'W')
      else
        CALL USRMSG(' File write error in ',lf,'W')
      endif
      close(iotmp2)
      IER=1
      return

   14 if(IOS.eq.2)then
        CALL USRMSG(' No prmission to write array in ',lf,'W')
      else
        CALL USRMSG(' Long array write error in ',lf,'W')
      endif
      close(iotmp2)
      IER=1
      return

      end

C ****** putcoods
C  Putcoods writes co-ordinates into .vew file or esp geometry commons.
      subroutine putcoods
#include "building.h"
      parameter (MAXLAY = 80)

      integer currlay, currcol, oldlay, oldcol
      integer gcode,innum,ioin,ioout,ioblk,flagins
      common/rd/gcode,instring,innum,realin,coorx,coory,
     &          coorz,blkxorg,blkyorg,blkzorg
      common/attrs/ currlay, currcol, oldlay, oldcol
      common/fg/flag3d,flagins,radians,dxfconv
      common/io/ioin,ioout,ioblk,iosblk,iotobs
      common/matrix/ amat(3,3), xnorm, ynorm, znorm
      common/layers/ layname(MAXLAY), numlay, laycolour(MAXLAY),
     &       layflag(MAXLAY)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      common/dxfe/oformat,indxzon,head

      character instring*72,oformat*24,head*4
      character layname*72
      logical match

C Multiply current x,y,z by transformation matrix
      x = coorx*amat(1,1) + coory*amat(1,2) + coorz*amat(1,3)
      y = coorx*amat(2,1) + coory*amat(2,2) + coorz*amat(2,3)
      z = coorx*amat(3,1) + coory*amat(3,2) + coorz*amat(3,3)

      llay=lnblnk(layname(currlay))

C If viewer format write coordinates either absolute or reset
C from blk?org.  If one of the 'BLK_' layers then write to
C the block storage file, otherwise to the normal output file.
C For both file formats convert from mm to metres.  
      if(oformat(1:6).eq.'viewer')then
        if (flagins.eq.0) then      
          if(head.eq.'BLK_')then
            write(ioblk,'(3f12.4)') x*dxfconv, y*dxfconv, z*dxfconv
          else
            write(ioout,'(3f12.4)') x*dxfconv, y*dxfconv, z*dxfconv
          endif
        else        
          x = x-blkxorg
          y = y-blkyorg        
          z = z-blkzorg      
          if(head.eq.'BLK_')then
            write(ioblk,'(3f12.4)')  x*dxfconv, y*dxfconv, z*dxfconv        
          else
            write(ioout,'(3f12.4)')  x*dxfconv, y*dxfconv, z*dxfconv        
          endif
        endif  
      elseif(oformat(1:3).eq.'esp')then
        if(head.eq.'OBS_')then
        elseif(head.eq.'BLK_')then

C Currently writing to temporary block file so do this in viewer
C format (for later recovery as an obstruction block).
          if (flagins.eq.0) then      
            write(ioblk,'(3f12.4)') x*dxfconv, y*dxfconv, z*dxfconv
          else        
            x = x-blkxorg
            y = y-blkyorg        
            z = z-blkzorg      
            write(ioblk,'(3f12.4)')  x*dxfconv, y*dxfconv, z*dxfconv  
          endif      
        else
          x1=x*dxfconv
          y1=y*dxfconv
          z1=z*dxfconv

C If on layer "0" or "1" do not bother to match vertex.
          if(llay.eq.1)then
            if(layname(currlay)(1:1).eq.'0')return
            if(layname(currlay)(1:1).eq.'1')return
          endif

          if(head.eq.'TRN_')ITMCFL(indxzon,NZSUR(indxzon))=1
          call matchv(indxzon,NZSUR(indxzon),match,x1,y1,z1)
        endif
      endif     
      return
      end

C ********** readgc
C Subroutine to read group code from dxf file.
      subroutine readgc

      COMMON/OUTIN/IUOUT,IUIN
      integer gcode,innum,ioin,ioout,ioblk
      common/trn/xscale,yscale,zscale,rotangl,basex,lineno,minseg,
     &basey,  xincr,yincr,zincr,basez,elev,thick,dfltelev,dfltthick
      common/rd/gcode,instring,innum,realin,coorx,coory,
     & coorz,blkxorg,blkyorg,blkzorg
      common/io/ioin,ioout,ioblk,iosblk,iotobs
      character instring*72,outs*124

      read(ioin,'(I6)',end=500,err=600) gcode
      if (gcode.lt.10) then
         read(ioin,'(A)') instring
      elseif (gcode.ge.60.and.gcode.le.80) then
         read(ioin,*) innum
      elseif (gcode.ge.30.and.gcode.lt.60) then
         read(ioin,*) realin
      elseif (gcode.ge.210.and.gcode.lt.240) then
         read(ioin,*) realin
      elseif (gcode.ge.10.and.gcode.lt.20) then
         read(ioin,*) coorx
         read(ioin,'(i6)') icode
C debug...
C          write(6,*) 'gcode is ',gcode,coorx,icode
         if (icode.ne.(gcode+10)) then
            call edisp(iuout,'Y co-ordinate missing')
            goto 600
         endif
         read(ioin,*) coory
         lineno = lineno + 2
      else
         read(ioin,'(A)') instring
      endif
      lineno = lineno + 2
      return

 500  call edisp(iuout, 'ABNORMAL END OF INPUT FILE')
      stop 'ecnv aborted.'

 600  write(outs,'(a,I4)') 'ERROR reading dxf file at line : ',lineno
      call edisp(iuout,outs)
      stop 'ecnv aborted.'
      end

C ****** matchl 
C Given a layer, return related zone (im), name (RZNAME), and 
C head of name (head).
      subroutine matchl(il,head,RZNAME,match,im)
#include "building.h"

      parameter (MAXLAY = 80)
      COMMON/C1/NCOMP,NCON
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/layers/layname(MAXLAY), numlay, laycolour(MAXLAY),
     &       layflag(MAXLAY)
      character layname*72,zname*12,zdesc*64,TZNAME*24,RZNAME*24,head*4
      logical match

      head=' '

C Copy and clean layer name, check to see if it begins with 'OBS_' or
C 'OPQ_' or 'TRN_', find the characters after this and check if this
C matches a known zone name.
      call st2name(layname(il),TZNAME)
      ltz = lnblnk(TZNAME)

C Check to see that we are not dealing with the special layer "0" or
C "1".
      if(ltz.eq.1)then
        if(TZNAME(1:1).eq.'0'.or.TZNAME(1:1).eq.'1')then
          RZNAME='____'
          head='BLK_'
          match=.false.
          im=0
          return
        endif
      endif
      if(TZNAME(1:4).eq.'OBS_')then
        write(RZNAME,'(a)') TZNAME(5:ltz)
        head='OBS_'
      elseif(TZNAME(1:4).eq.'OPQ_')then
        write(RZNAME,'(a)') TZNAME(5:ltz)
        head='OPQ_'
      elseif(TZNAME(1:4).eq.'TRN_')then
        write(RZNAME,'(a)') TZNAME(5:ltz)
        head='TRN_'
      else
        write(RZNAME,'(a)') TZNAME(1:ltz)
        head='____'
      endif

      if(ncomp.eq.0)then
        match=.false.
        im=0
      else

C Loop and see if root of layer name matches an existing
C zone name.
        match=.false.
        im=0
        do 46 k1=1,ncomp
          if(RZNAME(1:lnblnk(RZNAME)).eq.
     &       zname(k1)(1:lnblnk(zname(k1))))then
             match=.true.
             im=k1
          endif
  46    continue
      endif

      return
      end

C **** matchv
C Matches or adds a vertex to a zone.
      subroutine matchv(iz,is,match,x1,y1,z1)
#include "building.h"
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/ZNDATA/VCOORD(MCOM,MTV,3),NZNVER(MCON),NZJVN(MCON,MV)

      logical clx,cly,clz,match

C NZTV(IZ)= NVT, NZSUR(IZ)=NSUR,  VCOORD(IZ,K,3)= x,y,z
C NZNVER(icc) = NVER(J), NZJVN(icc,K) = JVN(J,K)
C Check and see if the current point is close to an existing
C vertex. If so point to it, otherwise add to vertex list.
C Update the surface topology.
      match=.false.
      if(NZTV(iz).gt.0)then
        do 40 iv=1,NZTV(iz)
          call eclose(x1,vcoord(iz,iv,1),0.001,clx)
          call eclose(y1,vcoord(iz,iv,2),0.001,cly)
          call eclose(z1,vcoord(iz,iv,3),0.001,clz)
          if(clx.and.cly.and.clz)then
            match=.true.
            ivmatch = iv
          endif
  40    continue
      endif
      icc=IZSTOCN(iz,is)
      if(match)then
        NZNVER(icc) = NZNVER(icc)+1
        NZJVN(icc,NZNVER(icc))=ivmatch
      else
        NZTV(iz) = NZTV(iz)+1
        NZNVER(icc) = NZNVER(icc)+1
        NZJVN(icc,NZNVER(icc)) = NZTV(iz)
        vcoord(iz,NZTV(iz),1) = x1
        vcoord(iz,NZTV(iz),2) = y1
        vcoord(iz,NZTV(iz),3) = z1
      endif

      return
      end


C **** Make a string uppercase.
      subroutine strup(str,ustr)
      character*(*) str,ustr
      character*1 chr

      leng=LEN(str)
      ustr=' '        
      do 15 i=1,leng
        chr=str(i:i)
        if (chr.eq.'$')then
          ustr(i:i)='_'
        elseif (chr.le.'z'.and.chr.ge.'a') then
          chr=char(ichar(chr)-32)
          ustr(i:i)=chr
        else
          ustr(i:i)=chr
        endif
  15  continue
      return
      end

C ********* blnksys
C Fill common blocks for a minimal standard systems configuration
C file so that output file can be written from commons.
      subroutine blnksys
#include "building.h"
#include "esprdbfile.h"
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C4/XLAT,XLON
      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM
      CHARACTER*72 SNFNAM
      COMMON/C6/INDCFG
      COMMON/C23/IFPNF,LPNF
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)

      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/AFNZN/zmfn1,zmfn2
      common/cctlnm/ctldoc,lctlf
      COMMON/LOG/LPRJLG
      COMMON/IMAGF/imgfmt(10),imgfoc(10),limgfil(10),noimg,iton
      COMMON/GTFIL/GTGEOM
      COMMON/GT/GTNAME
      common/radcfg/LRADCF
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2

      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER LAPROB*72,LPNF*72,LPRJLG*72
      CHARACTER imgfmt*4,imgfoc*4,limgfil*72
      character zmfn1*124,zmfn2*124
      character lradcf*72,ctldoc*248,LCTLF*72
      character GTGEOM*72,GTNAME*15
      character*10 wkd1, wkd2

      LCLIM=DCLIM
      LAPRES=DAPRES
      LFMAT=DFCON
      LFMUL=DFMUL
      LOPTDB=DOPTDB
      LPRFDB=DPRFDB
      LPCDB =DPCDB
      LPRJLG='job.notes'
      LPNF='UNKNOWN'
      zmfn1=' '
      zmfn2=' '
      lradcf='UNKNOWN'
      LCTLF=' '
      CTLDOC='no overall project control specified (yet)'
      GTGEOM='UNKNOWN'
      GTNAME = 'flat_booring'
      idwe1=6
      idwe2=7
      wkd1='Saturday'
      wkd2='Sunday'

C Set number of images read to 0.
      noimg=0
      NCOMP=0
      NCON=0
      XLAT=50.0
      XLON=0.0
      IXPOS=1
      GREF=0.2
      ITGREF=1
      SREF=0.4
      DO 60 I=1,12
        GREF12(I)=GREF
        NSNOW(I)=0
   60 CONTINUE
      SNFNAM=' '
      INDCFG=1

C Clear esp-r commons.
      NCOMP=0
      NCON=0
      do 76 iz=1,MCOM
        write(LPROJ(iz),'(a)') 'UNKNOWN'
        write(LTHRM(iz),'(a)') 'UNKNOWN'
        NCCODE(iz)=iz
        LUTIL(iz)=' '
        INDUTL(iz)=0
        IVF(iz)=0
        ISI(iz)=0
        IHC(iz)=0
        ITW(iz)=0
        ICGC(iz)=0
        IOBS(iz)=0
  76  continue

      return
      end

C ********* mksyshd
C mksyshd - write out the initial portion of the system configuration
C file (up to where the zone information starts).
      subroutine mksyshd(LOUT,ier)
#include "building.h"
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/C3F/LCNN
      COMMON/C4/XLAT,XLON
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM
      CHARACTER*72 SNFNAM
      COMMON/C5R/SKYR,GRDR,BLDR
      COMMON/C6/INDCFG
      common/C21/IFCFG,cfgroot,LCFGF
      common/CFGV/icfgv
      COMMON/LOG/LPRJLG
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      common/cctlnm/ctldoc,lctlf
      common/io/ioin,ioout,ioblk,iosblk,iotobs
      COMMON/IMAGF/imgfmt(10),imgfoc(10),limgfil(10),noimg,iton

C Primary energy and emissions conversions.
      common/PCONV/ipconv,pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
      common/CONVEM/phtco2,phtnox,phtsox,pclco2,pclnox,pclsox,
     &              pltco2,pltnox,pltsox,pfnco2,pfnnox,pfnsox,
     &              pspco2,pspnox,pspsox,phwco2,phwnox,phwsox

C Path to SHOCC input files
C bSHOCCed     - logical flag whether project has SHOCC input
C bZoneSHOCCed - logical array whether zones have SHOCC input
C SHOCCshlFile - path to SHOCC .shl file for SHOCC'd project library
C SHOCCshzFile - path to SHOCC .shz file for each SHOCC'd zone
      common/SHOCCcfg/bSHOCCed,SHOCCshlFile,bZoneSHOCCed(mcom),
     &                SHOCCshzFile(mcom)
      logical bSHOCCed,bZoneSHOCCed
      character SHOCCshlFile*72,SHOCCshzFile*72

      CHARACTER*72 LCFGF,LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER LCNN*72,LAPROB*72,LPRJLG*72,LOUT*72
      CHARACTER CTLDOC*248,LCTLF*72,cfgroot*24
      character imgfmt*4,imgfoc*4,limgfil*72,indxcmt*24
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      character dstmp*24

      IER=0

C Comment for configuration index (upgrade registration level).
      if(INDCFG.EQ.0) indxcmt=' # Building only'
      if(INDCFG.EQ.1)  indxcmt=' # Building only'
      IF(INDCFG.EQ.2)  indxcmt=' # Plant only'
      IF(INDCFG.EQ.3) indxcmt=' # Building & Plant'
      if(INDCFG.EQ.0)then
        INDCFG=1
        write(LCNN,'(a,a)') cfgroot(1:lnblnk(cfgroot)),'.cnn'
      endif
      WRITE(ioout,'(A)',IOSTAT=IOS,ERR=3) '* CONFIGURATION3.0'
      WRITE(ioout,30,IOSTAT=IOS,ERR=3) LOUT(1:lnblnk(LOUT))
      call dstamp(dstmp)
      WRITE(ioout,'(A,A,a)',IOSTAT=IOS,ERR=3) '*date ',dstmp,
     &  '  # latest file modification '
      WRITE(ioout,'(A,A)',IOSTAT=IOS,ERR=3) '*root ',
     &  cfgroot(1:lnblnk(cfgroot))
      WRITE(ioout,'(A,A,a)',IOSTAT=IOS,ERR=3) '*zonpth ',zonepth,
     &  '  # path to zones'
      WRITE(ioout,'(A,A,a)',IOSTAT=IOS,ERR=3) '*netpth ',netpth,
     &  '  # path to networks'
      WRITE(ioout,'(A,A,a)',IOSTAT=IOS,ERR=3) '*ctlpth ',ctlpth,
     &  '  # path to controls'
      WRITE(ioout,'(A,A,a)',IOSTAT=IOS,ERR=3) '*imgpth ',imgpth,
     &  '  # path to project images'
      WRITE(ioout,'(A,A,a)',IOSTAT=IOS,ERR=3) '*radpth ',radpth,
     &  '  # path to radiance files'
      WRITE(ioout,'(A,A,a)',IOSTAT=IOS,ERR=3) '*dbspth ',dbspth,
     &  '  # path to local databases'
      WRITE(ioout,'(A,i4,a)',IOSTAT=IOS,ERR=3) '*indx ',INDCFG,
     &  indxcmt(1:lnblnk(indxcmt))
  30  FORMAT('# ESRU system configuration defined by file ',/,'# ',A)


      WRITE(ioout,'(F7.3,2X,F7.3,a)')XLAT,XLON,
     &'   # Latitude & Longitude '
      WRITE(ioout,'(I7,2X,F6.3,a)') IXPOS,GREF,
     &'   # Site exposure & ground refl'
      IF(IXPOS.EQ.8)WRITE(ioout,'(3F8.3,A)')
     &           SKYR,GRDR,BLDR,'   # views to sky ground buildings'
      write(ioout,'(a)')'* DATABASES'
      WRITE(ioout,'(A,A)') '*prm  ',LFMAT(1:lnblnk(LFMAT))
      WRITE(ioout,'(A,A)') '*mlc  ',LFMUL(1:lnblnk(LFMUL))
      WRITE(ioout,'(A,A)') '*opt  ',LOPTDB(1:lnblnk(LOPTDB))
      WRITE(ioout,'(A,A)') '*prs  ',LAPRES(1:lnblnk(LAPRES))
      WRITE(ioout,'(A,A)') '*evn  ',LPRFDB(1:lnblnk(LPRFDB))
      WRITE(ioout,'(A,A)') '*clm  ',LCLIM(1:lnblnk(LCLIM))
      WRITE(ioout,'(A,A)') '*pdb  ',LPCDB(1:lnblnk(LPCDB))

C << add test for whether model has SHOCC file >>
      WRITE(ioout,'(A,A)') '*shl  ',
     &      SHOCCshlFile(1:lnblnk(SHOCCshlFile))
      if(lnblnk(lctlf).eq.0.or.lctlf(1:7).eq.'UNKNOWN')then
        continue
      else
        WRITE(ioout,'(a,a)') '*ctl  ',LCTLF(1:lnblnk(LCTLF))
      endif

C Ground reflectivity model, no-snow monthly albedos, snow albedo
C (if ground reflectivity model 2 or 3)
      IF(ITGREF.NE.1) WRITE(ioout,803) '*gref ',ITGREF,
     &      (GREF12(I),I=1,12),SREF
  803 FORMAT(A,I1,13(1X,F5.3))

C Number of days with snow on the ground 
C (if ground reflectivity model 2)
      IF(ITGREF.EQ.2) WRITE(ioout,805) '*snow',(NSNOW(I),I=1,12)
  805 FORMAT(A,12(1X,I2))

C Snow depth file
C (if ground reflectivity model 3)
      IF(ITGREF.EQ.3) WRITE(ioout,'(A,A)',IOSTAT=IOS,ERR=2) '*sndf ',
     &      SNFNAM(1:lnblnk(SNFNAM))

      WRITE(ioout,'(a,I4,a)',IOSTAT=IOS,ERR=2) '*year  ',
     &  IYEAR,' # assessment year'

C Write images if any.
      if(noimg.gt.0)then
        do 142 img=1,noimg
          WRITE(ioout,'(a,a4,2x,a4,2x,a)')'*img ',imgfmt(img),
     &      imgfoc(img),limgfil(img)(1:lnblnk(limgfil(img)))
 142    continue
      endif

C Write primary energy conversions if these have been set.
      if(ipconv.eq.1)then
        WRITE(ioout,'(a)',IOSTAT=IOS,ERR=2) 
     &   '# prim energy conv (heat,cool,lights,fan,sml pwr,hot water)'
        WRITE(ioout,'(a,6F6.3)',IOSTAT=IOS,ERR=2) '*pecnv ',
     &    pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
        WRITE(ioout,'(a,3F9.3,a)',IOSTAT=IOS,ERR=2) '*htemis ',
     &    phtco2,phtnox,phtsox,' # heating emissions CO2,NOX,SOX'
        WRITE(ioout,'(a,3F9.3,a)',IOSTAT=IOS,ERR=2) '*clemis ',
     &    pclco2,pclnox,pclsox,' # cooling emissions CO2,NOX,SOX'
        WRITE(ioout,'(a,3F9.3,a)',IOSTAT=IOS,ERR=2) '*ltemis ',
     &    pltco2,pltnox,pltsox,' # lighting emissions CO2,NOX,SOX'
        WRITE(ioout,'(a,3F9.3,a)',IOSTAT=IOS,ERR=2) '*fnemis ',
     &    pfnco2,pfnnox,pfnsox,' # fan/pump emissions CO2,NOX,SOX'
        WRITE(ioout,'(a,3F9.3,a)',IOSTAT=IOS,ERR=2) '*spemis ',
     &    pspco2,pspnox,pspsox,' # small power emissions CO2,NOX,SOX'
        WRITE(ioout,'(a,3F9.3,a)',IOSTAT=IOS,ERR=2) '*hwemis ',
     &    phwco2,phwnox,phwsox,' # dhw emissions CO2,NOX,SOX'
      endif
      write(ioout,'(a)')'* PROJ LOG'
      WRITE(ioout,'(A)')LPRJLG(1:lnblnk(LPRJLG))
      write(ioout,'(a)')'* Building'
      WRITE(ioout,'(A)')LSNAM(1:lnblnk(LSNAM))
      return

    2 CALL edisp(iuout,' Problem writing config data config...')
      stop 'ecnv aborted.'
    3 CALL edisp(iuout,' Problem writing configuration name...')
      stop 'ecnv aborted.'
      end

C Dummy routines from common3dv.F needed to match implied
C calls in esru_lib.F
      SUBROUTINE ADJVIEW(ITRC,IER)
      ier=0
      return
      end

      subroutine chgazi(icazi)
      return
      end

      subroutine chgelev(icelev)
      return
      end

      SUBROUTINE EMKVIEW(ITRC,IUO,CFGOK,IER)
      logical cfgok
      ier=0
      return
      end

      SUBROUTINE INLNST(ITYP)
      return
      end

      SUBROUTINE PLELEV(IUF,direc,ier)
      CHARACTER direc*1
      ier=0
      return
      end

      subroutine imgdisp(iforce,focus,ier)
      character focus*4
      return
      end

