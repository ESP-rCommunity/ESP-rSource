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
      USE START_UP
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure database)
C LOPTDB,IOPTDB (for optical database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
#include "material.h"
#include "espriou.h"
#include "dxfdata.h"
      
      integer lnblnk  ! function definition

      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk
      common/rp/repfile,repelev,repthick,nrep

      integer indxzon
      character oformat*24,head*4
      common/dxfe/oformat,indxzon,head

C ESP-r commons.
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/rpath/path
      common/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C6/INDCFG
      common/C21/IFCFG,cfgroot,LCFGF

C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Path to SHOCC input files
C bSHOCCed     - logical flag whether project has SHOCC input
C bZoneSHOCCed - logical array whether zones have SHOCC input
C SHOCCshlFile - path to SHOCC .shl file for SHOCC'd project library
C SHOCCshzFile - path to SHOCC .shz file for each SHOCC'd zone
C      common/SHOCCcfg/bSHOCCed,SHOCCshlFile,bZoneSHOCCed(mcom),
C     &                SHOCCshzFile(mcom)
C      logical bSHOCCed,bZoneSHOCCed
C      character SHOCCshlFile*72,SHOCCshzFile*72
     
C Name of current application
      common/APPNAME/cAppName
      character cAppName*12     

      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      character filname*144,filname2*144,filname1*144,repfile*40
      character inf*144,ouf*144
      character iformat*24,fs*1
      character LTMP*144
      character path*72
      character upgcfg*72

      character cfgroot*24,MODE*4
      character buffer*144

      CHARACTER LCFGF*72
      character htmlfile*72
      character outs*248
      logical unixok

C Data types for parcnv call.
      integer itrc
      integer iverb  ! 2 is trace active 0 is silent
      integer incobs ! zero default 1 to include obstructions
      integer ichop  ! zero default 1 if -chop in cmd line but
                     ! is not yet used
      integer itmprm ! 1 is delete tmp files 0 is keep them
      real conv      ! passed as 0.001 if coords in mm
 
C System parameter initializing. Use ioin for the input file,
C ioout for primary output file, ioblk for block storage,
C iotmp2 for miscel output files.
      call ezero
      call curmodule('ecnv')
      cAppName = 'ecnv'

      lineno = 0
      nrep=0        
      ioin=1        
      ioout=2        
      ioblk=3
      iotmp2=11               
      IFIL=15   ! set this to a higher number so no clash
      IUOUT=6
      IUIN=5
      LIMTTY=20
      LIMIT=20
      NSIGFIG=3
      matver=0.0   ! initial assumption of binary materials database


C Get command line parameters. 
      call parcnv(iverb,conv,incobs,ichop,itmprm,iformat,oformat,inf,
     &  ouf,upgcfg)
      itrc=iverb    ! set trace after call to parcnv
      rotangl=0.        
      xscale=1.        
      yscale=1.        
      zscale=1.        
      entelev=0.
      dfltelev=entelev
      entthick=0.
      dfltthick=entthick
      flagins=0        
      oldlay = 0
      oldcol = 1
      currlay = 0
      currcol = 1
      minseg = 6
      dxfconv = 1.0
      layuse(1)=0

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

      call edisp(IUOUT,' ')
      CALL ESPrVersion("summary",cAppName,IUOUT)

C Find the user's home folder then get user's custom settings.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,iotmp2,0,IIER)

C Convert from "c" strings to fortran strings, if unknown then exit.

C Get input file and format.
      filname=inf
      write(outs,'(2a)')'Input file is: ',filname(1:lnblnk(filname))
      call edisp248(iuout,outs,100)
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

      if(filname(1:2).eq.'  '.or.filname(1:7).eq.'UNKNOWN')then
        call edisp(iuout,'ecnv_error unknown_input_file')
        stop 'ecnv aborted.'
      endif     

C Get output format. If viewer then use a single file, if
C ESP-r then a series of files will be opened.
      filname2=ouf
      if(filname2(1:2).eq.'  '.or.filname2(1:7).eq.'UNKNOWN')then
        call edisp(iuout,'ecnv_error unknown_output_file')
        stop 'ecnv aborted.'
      endif
      if(oformat(1:6).eq.'viewer')then
        write(outs,'(2a)')'The viewer output file will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp248(iuout,outs,100)
      elseif(oformat(1:4).eq.'vrml')then
        write(outs,'(2a)')'The vrml world will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp248(iuout,outs,100)
      elseif(oformat(1:3).eq.'esp')then

C If esp then also find its path so zone files can be
C placed in the same folder.
        write(outs,'(2a)')'The esp-r configuration file will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp248(iuout,outs,100)
        call fdroot(filname2,path,LCFGF)
        if(upgcfg(1:7).ne.'UNKNOWN')then

C Instanciate and clear commons, scan the supplied problem
C to update commons for configuration.
          call clrprb
          call edisp(iuout,' Scanning the problem before updating...')
          call escdef(iotmp2,IER)
          MODE='ALL '
          call ERSYS(LCFGF,ioout,iotmp2,MODE,itrc,IER)
          if(IER.eq.0)then
          
C We have successfully scanned in the configuration file. If we
C assume that the user will be adding surface to a model with
C no existing zones then we can set the number of connections
C to zero. If there are exsting zones then warn the user there
C might be problems.
            CFGOK=.TRUE.
            if(NCOMP.gt.0)then
              call edisp(iuout,
     &          'Model already has zones. Check results carefully.')
            endif
            if(NCON.gt.0)then
              call edisp(iuout,
     &          'Model already has surfaces. Check results carefully.')
            endif
            
C If this is a registration only model it should be upgraded first.
C Set ifcfg to the unit number that deals with the configuration file.
C Set indcfg to one to signal a building model, set the number of
C zones and connections to zero and set the connections file name
C based on the root name of the model. Write the model cfg and cnn
C files and close the cfg file.
            if(INDCFG.EQ.0)then
              ifcfg=ioout
              INDCFG=1
              NCOMP=0
              NCON=0
              write(LCNN,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.cnn'
              CALL EMKCFG('-',ier)
              close(ifcfg)
            endif
          else
            call edisp(iuout,'ecnv_error corrupt_update_file')
            stop 'ecnv aborted.'
          endif
        else
          call escdef(iotmp2,IER)
          call clrprb
        endif

      elseif(oformat(1:3).eq.'dxf')then

C If AutoCAD then also find its path so that support files can be
C placed in the same folder.
        write(outs,'(2a)')'The AutoCAD DXF file will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp248(iuout,outs,100)

      elseif(oformat(1:6).eq.'zip')then

        write(outs,'(2a)')'The zip output file will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp248(iuout,outs,100)

      elseif(oformat(1:4).eq.'xfig')then

        write(outs,'(2a)')'The xfig output file will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp248(iuout,outs,100)

      elseif(oformat(1:3).eq.'THF'.or.oformat(1:3).eq.'thf')then

        write(outs,'(2a)')'The THF output file will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp248(iuout,outs,100)

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

C If input format is esp and output is viewer, thf, dxf or
C vrml do this.
      if(iformat(1:3).eq.'esp')then

C Assume that the configuration, mlc db, material db, control file
C have not been read in.
        CFGOK=.FALSE.
        MLDBOK=.FALSE.
        MATDBOK=.FALSE.
        CTLOK=.FALSE.

C Scan the defaults file for default configuration.
C Make temporary use of file unit iotmp2.
        call escdef(iotmp2,IER)
        write(LTMP,'(a)') filname(1:lnblnk(filname))

C Find the path and local file name.
        call fdroot(LTMP,path,LCFGF)
        call edisp(iuout,' Scanning the model description...')
        MODE='ALL '
        call ERSYS(LCFGF,ioin,iotmp2,MODE,itrc,IER)
        if(IER.eq.0)then
          CFGOK=.TRUE.
        else
          call edisp(iuout,'ecnv_error corrupt_input_file')
          stop 'ecnv aborted.'
        endif

C If output format is viewer or dxf.

C << consider more passed parameters to fine-tune entity types >>

        if(oformat(1:6).eq.'viewer'.or.oformat(1:3).eq.'dxf')then
          open(ioout,file=filname2,status='UNKNOWN', err=901)
          call e2vdxf(incobs)
          call edisp(iuout,' End of conversion.')
          CALL EPAGEND
          STOP
        endif

C Create THF (THINGS) file.
        if(oformat(1:3).eq.'THF'.or.oformat(1:3).eq.'thf')then
          open(ioout,file=filname2,status='UNKNOWN', err=901)
          call e2thf(incobs)
          call edisp(iuout,' End of conversion.')
          CALL EPAGEND
          STOP
        endif

C Create an Abacus zip file format.

C << note this format is no longer used and might be
C << considered for removal

        if(oformat(1:6).eq.'zip')then
          open(ioout,file=filname2,status='UNKNOWN', err=901)
          WRITE(ioout,'(a)') 'COM data file converted from `esp`'
          WRITE(ioout,'(a)') 'NAME'
          WRITE(ioout,'(a,a,a)') '"',filname2(1:lnblnk(filname2)),'"'
          WRITE(ioout,'(a)') 'COM DATE ....'
          WRITE(ioout,'(a)') ' '
          WRITE(ioout,'(a)') 'ESP'
          WRITE(ioout,'(a)') 'ANT'
          call e2zip(incobs)
          CALL ERPFREE(ioout,ISTAT)
          call edisp(iuout,' End of conversion.')
          CALL EPAGEND
          STOP
        endif

C Export to VRML which also includes an html file (which
C makes use of ioblk file channel).

C << consider updating to a newer format such as SVG >>
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
          CALL ERPFREE(ioblk,ISTAT)  ! free up the html file
          call edisp(iuout,' End of conversion.')
          CALL EPAGEND
          STOP
        endif
      endif

C If we got to this point the conversion is from AutoCAD.
      open(ioin ,file=filname1,status='OLD', err=900)
      open(ioout,file=filname2,status='UNKNOWN', err=901)

C Confirm conversion of scale.
      dxfconv=conv
      if(dxfconv.gt.2.0.or.dxfconv.lt.0.0001)then
        call edisp(iuout,'dxf2v_error range_of_conversion')
        stop 'ecnv aborted.'
      endif

C If esp-r then generate initial portion of configuration file.
C If we started by scanning an existing esp-r configuration file
C there is not much need to call mksyshd.  Just loop until
C you get to the line * Building.
      if(oformat(1:3).eq.'esp')then
        modeltitle='Translation from AutoCAD'
        if(upgcfg(1:7).eq.'upgrade')then
        
C Read ioout until * Building then write out the new value
C of modeltitle and then jump to 28 to allow data from DXF file
C to be scanned.
          do 27 i=1,100
            read(ioout,'(A)') buffer
            if(itrc.gt.1)then
              write(outs,*) buffer(1:lnblnk(buffer))
              call edisp(iuout,outs)
            endif
            if(buffer(1:10).eq.'* Building') then
              write(ioout,'(A)',IOSTAT=IOS,ERR=2)
     &          modeltitle(1:lnblnk(modeltitle))
              goto 28
            endif
  27      continue
          call edisp(iuout,'end of cfg file without finding *building')
	else
          call mksyshd(filname2,ier)
        endif
      endif
  28  continue   ! we found the * Building line.

C The high level scan of a DXF file starts here. It works by
C calling readgc to recover information from the DXF file until
C and EOF is located. It looks for 'HEAD' and 'TABL' and 'ENDSEC'.
      call scandxf(filname,filname1,filname2,itmprm,itrc) 

      CALL EPAGEND
      STOP

C Error messages.
 900  write(outs,'(2a)') 'Error opening "OLD" file :',
     &     filname1(:lnblnk(filname1))
      call edisp248(iuout,outs,100)
      goto 999
 901  write(outs,'(2a)') 'Error opening "NEW" file :',
     &     filname2(:lnblnk(filname2))
      call edisp248(iuout,outs,100)
      goto 999

    2 CALL edisp(iuout,' Problem writing config data config...')
      close(ioout)
      goto 999

 999  stop 'ecnv aborted.'
      end

C **** wegeom
C write esp-r (older style) geometry file.
      subroutine wegeom(iotmp2,iv,lf,ier)
#include "building.h"
#include "geometry.h"

C << update this to generate v1.1 geometry file >>
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)

C common prec17 is described in geometry.h.

C Use YOS to hold dummy values for surface indentation.
      dimension ndum(MS),Y0S(MS)
      character lf*72,SN*12,OTF*4,louts*248
      integer lnzn
       
      ier=0
      CALL EFOPSEQ(iotmp2,lf,3,IER)
      if(ier.ne.0)goto 903
      lnzn=lnzname(iv)
      write(iotmp2,'(4a)')'# geometry of ',
     &  zname(iv)(1:lnzn),' defined in: ',lf(1:lnblnk(lf))
      write(iotmp2,'(5a)')'GEN ',zname(iv)(1:lnzn),' ',
     &  zname(iv)(1:lnzn),' was imported from DXF  # type, name, descr'
      write(iotmp2,'(I8,I8,a)')NZTV(iv),NZSUR(iv),
     &      '   0.000    # vertices, surfaces, rotation angle'
      write(iotmp2,'(a)')'#  X co-ord, Y co-ord, Z co-ord '
      do 58 ivv=1,NZTV(iv)
         write(iotmp2,'(3f11.5)')VCOORD(iv,ivv,1),VCOORD(iv,ivv,2),
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
        call arlist(ipos,NZSUR(iv),Y0S,MS,'C',louts,loutln,itrunc)
        write(iotmp2,'(1x,a)',IOSTAT=ios,ERR=14) louts(1:loutln)
        ipos=itrunc+1
      end do
      write(iotmp2,'(a)')'  3 0 0 0  # def insolation distribution'

      write(iotmp2,'(a)')'# surface attributes follow: '
      write(iotmp2,'(a)')
     &      '# id  surface      geom  loc/  mlc db       environment'
      write(iotmp2,'(a)')
     &      '# no  name         type  posn  name         other side'

C << logic below does not yet account for whether a surface is opaque
C << or transparent, it also does not check the orientation of the
C << surfaces.

      do 60 is=1,NZSUR(iv)
        IF(is.LE.9)WRITE(SN,'(a5,i1)')'Surf-',is
        IF(is.GT.9)WRITE(SN,'(a5,i2)')'Surf-',is
        OTF='OPAQ'
        if(itmcfl(iv,is).eq.1)OTF='TRAN'
        write(iotmp2,'(i3,a2,a12,a2,a4,a)')is,', ',SN,'  ',OTF,
     &        '  UNKN  UNKNOWN      UNKNOWN'
  60  continue
  
C Set assumption that the user has edited base area to 1m2.
      IUZBASEA(iv)=1
      IZBASELIST(iv)=0
      ZBASEA(iv)=1.0

C Surfaces associated with base.
      WRITE(iotmp2,'(a)')'# base'
      WRITE(iotmp2,'(a,F9.2,i2)') '  0  0  0  0  0  0 ',
     &  ZBASEA(iv),IUZBASEA(iv)
      
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

      subroutine putcoods(itrc)
#include "building.h"
#include "geometry.h"
#include "dxfdata.h"
      
      integer lnblnk  ! function definition
      
      integer IUOUT,IUIN
      COMMON/OUTIN/IUOUT,IUIN

      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk
      common/matrix/ amat(3,3), xnorm, ynorm, znorm

      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)

      integer indxzon
      character oformat*24,head*4
      common/dxfe/oformat,indxzon,head

      character outs*124
      logical match
      integer nbofsurf  ! to pass to matchv

C Multiply current xx,yy,zz by transformation matrix
      xx = coorx*amat(1,1) + coory*amat(1,2) + coorz*amat(1,3)
      yy = coorx*amat(2,1) + coory*amat(2,2) + coorz*amat(2,3)
      zz = coorx*amat(3,1) + coory*amat(3,2) + coorz*amat(3,3)

      llay=lnblnk(dxflayname(currlay))

C If viewer format write coordinates either absolute or reset
C from blk?org.  If one of the 'BLK_' layers then write to
C the block storage file, otherwise to the normal output file.
C For both file formats convert from mm to metres.  
      if(oformat(1:6).eq.'viewer')then
        if (flagins.eq.0) then      
          if(head.eq.'BLK_')then

C Increment chartblk array.
            ichartblk=ichartblk+1
            write(chartblk(ichartblk),'(3f12.4)') xx*dxfconv, 
     &        yy*dxfconv, zz*dxfconv
          else
            write(ioout,'(3f12.4)') xx*dxfconv, yy*dxfconv, zz*dxfconv
          endif
        else        
          xx = xx-blkxorg
          yy = yy-blkyorg        
          zz = zz-blkzorg      
          if(head.eq.'BLK_')then

C Write chartblk array.
            ichartblk=ichartblk+1
            write(chartblk(ichartblk),'(3f12.4)')  xx*dxfconv,
     &        yy*dxfconv, zz*dxfconv        
          else
            write(ioout,'(3f12.4)')  xx*dxfconv, yy*dxfconv, zz*dxfconv        
          endif
        endif  
      elseif(oformat(1:3).eq.'esp')then
        if(head.eq.'OBS_')then
          continue
        elseif(head.eq.'BLK_')then

C Currently writing to temporary block file so do this in viewer
C format (for later recovery as an obstruction block).
          if (flagins.eq.0) then      

C Write chartblk array.
            ichartblk=ichartblk+1
            write(chartblk(ichartblk),'(3f12.4)') xx*dxfconv,
     &        yy*dxfconv, zz*dxfconv
          else        
            xx = xx-blkxorg
            yy = yy-blkyorg        
            zz = zz-blkzorg      

C Write chartblk array.
            ichartblk=ichartblk+1
            write(chartblk(ichartblk),'(3f12.4)')  xx*dxfconv,
     &        yy*dxfconv, zz*dxfconv  
          endif      
        else
          x1=xx*dxfconv
          y1=yy*dxfconv
          z1=zz*dxfconv

C If on layer "0" or "1" do not bother to match vertex if there are
C named layers. If there are no named layers then accept '0'. 
          if(numlay.eq.1)then
            if(dxflayname(1)(1:1).eq.'0')then
              continue   ! this is a known case.
            endif
          else
            if(llay.eq.1)then
              if(dxflayname(currlay)(1:1).eq.'0')return
              if(dxflayname(currlay)(1:1).eq.'1')return
            endif
          endif
          nbofsurf=NZSUR(indxzon)
          if(head.eq.'TRN_')ITMCFL(indxzon,nbofsurf)=1
          call matchv(indxzon,nbofsurf,match,x1,y1,z1)

C Debug.
          if(itrc.gt.1)then
            if(match)then
              write(outs,*) 'match for ',indxzon,nbofsurf,x1,y1,z1
            else
              write(outs,*) 'added to  ',indxzon,nbofsurf,x1,y1,z1
            endif
            call edisp(iuout,outs)
          endif
        endif
      endif     
      return
      end

C ********** readgc
C Subroutine to read group code from dxf file. The parameter
C itrc sets debug mode if greater than one.
      subroutine readgc(itrc)
#include "dxfdata.h"

      COMMON/OUTIN/IUOUT,IUIN

      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk
      character outs*124

      read(ioin,'(I6)',end=500,err=600) gcode
      if (gcode.lt.10) then
         read(ioin,'(A)') instring

C Debug...
         if(itrc.gt.1)then
           write(iuout,*) 'gcode linr nb & string ',gcode,lineno,
     &       ' ',instring(1:lnblnk(instring))
         endif
      elseif (gcode.ge.60.and.gcode.le.80) then
         read(ioin,*,end=500,err=600) innum
      elseif (gcode.ge.30.and.gcode.lt.60) then
         read(ioin,*,end=500,err=600) realin
      elseif (gcode.ge.210.and.gcode.lt.240) then
         read(ioin,*,end=500,err=600) realin
      elseif (gcode.ge.10.and.gcode.lt.20) then
         read(ioin,*,end=500,err=600) coorx
         read(ioin,'(i6)') icode

C Debug...
         if(itrc.gt.1)then
           write(iuout,*) 'gcode is ',gcode,coorx,lineno
         endif
         if (icode.ne.(gcode+10)) then
            call edisp(iuout,'Y co-ordinate missing')
            goto 600
         endif
         read(ioin,*,end=500,err=600) coory
         lineno = lineno + 2
      else
         read(ioin,'(A)') instring

C Debug...
         if(itrc.gt.1)then
           write(iuout,*) 'gcode linr nb & string ',gcode,lineno,
     &       ' ',instring(1:lnblnk(instring))
         endif
      endif
      lineno = lineno + 2
      return

 500  call edisp(iuout, 'readgc ABNORMAL END OF INPUT FILE')
      stop 'ecnv aborted.'

 600  write(outs,'(a,I4)') 'readgc ERROR reading dxf file at line : ',
     &  lineno
      call edisp(iuout,outs)
      stop 'ecnv aborted.'
      end

C ****** matchl 
C Given a layer, return related zone (im), name (RZNAME), and 
C head of name (head).  If there were no named layers and the
C conversion is stuffing layer 1 named zero into a zone named
C first then accept match.
      subroutine matchl(il,head,RZNAME,match,im)
#include "building.h"
#include "geometry.h"
#include "dxfdata.h"
      
      integer lnblnk  ! function definition
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      character TZNAME*24,RZNAME*24,head*4
      logical match

      head=' '

C Copy and clean layer name, check to see if it begins with 'OBS_' or
C 'OPQ_' or 'TRN_', find the characters after this and check if this
C matches a known zone name.
      call st2name(dxflayname(il),TZNAME)
      ltz = lnblnk(TZNAME)

C If there were no named layers and the conversion is stuffing
C layer 1 named zero into a zone named first then accept match.
      if(numlay.eq.1.and.ltz.eq.1)then
        if(TZNAME(1:1).eq.'0')then
          RZNAME='First'
          head='____'
          match=.true.
          im=1
          return
        endif
      endif

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
#include "geometry.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C24/IZSTOCN(MCOM,MS)

      logical clx,cly,clz,match,unixok
      character outs*124

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
      call isunix(unixok)
      if(icc.eq.0.and.unixok)then
        write(outs,*) 'connection index zero for ',iz,is
        call edisp(iuout,outs)
      endif
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

C ********* mksyshd
C mksyshd - write out the initial portion of the system configuration
C file (up to where the zone information starts).

C << to be done - this code should be identical to that included 
C << in emkcfg up to the point of writing out the zone information.
C << one option would be to pass a parameter to emkcfg to allow it
C << to write only the header portion.
C << MUST be updated to reflect ipath and longer database names >>

      subroutine mksyshd(LOUT,ier)
#include "building.h"
#include "model.h"
#include "site.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/C6/INDCFG
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/LLOG/LPRJLG

      common/cctlnm/ctldoc,lctlf

      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton

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

      CHARACTER LCFGF*72
      CHARACTER LPRJLG*72,LOUT*72
      CHARACTER CTLDOC*248,LCTLF*72,cfgroot*24
      character indxcmt*24

      character dstmp*24

      IER=0

C Comment for configuration index (upgrade registration level).
      if(INDCFG.EQ.0) indxcmt=' # Building only'
      if(INDCFG.EQ.1) indxcmt=' # Building only'
      IF(INDCFG.EQ.2) indxcmt=' # Plant only'
      IF(INDCFG.EQ.3) indxcmt=' # Building & Plant'
      if(INDCFG.EQ.0)then
        INDCFG=1
        write(LCNN,'(a,a)') cfgroot(1:lnblnk(cfgroot)),'.cnn'
      endif
      WRITE(ioout,'(A)',IOSTAT=IOS,ERR=3) '* CONFIGURATION3.0'
      WRITE(ioout,30,IOSTAT=IOS,ERR=3) LOUT(1:lnblnk(LOUT))
      call dstamp(dstmp)
      WRITE(ioout,'(3a)',IOSTAT=IOS,ERR=3) '*date ',dstmp,
     &  '  # latest file modification '
      WRITE(ioout,'(2A)',IOSTAT=IOS,ERR=3) '*root ',
     &  cfgroot(1:lnblnk(cfgroot))
      WRITE(ioout,'(3a)',IOSTAT=IOS,ERR=3) '*zonpth ',zonepth,
     &  '  # path to zones'
      WRITE(ioout,'(3a)',IOSTAT=IOS,ERR=3) '*netpth ',netpth,
     &  '  # path to networks'
      WRITE(ioout,'(3a)',IOSTAT=IOS,ERR=3) '*ctlpth ',ctlpth,
     &  '  # path to controls'
      WRITE(ioout,'(3a)',IOSTAT=IOS,ERR=3) '*aimpth ',aimpth,
     &  '  # path to aim2'
      WRITE(ioout,'(3a)',IOSTAT=IOS,ERR=3) '*radpth ',radpth,
     &  '  # path to radiance files'
      WRITE(ioout,'(3a)',IOSTAT=IOS,ERR=3) '*imgpth ',imgpth,
     &  '  # path to project images'
      WRITE(ioout,'(3a)',IOSTAT=IOS,ERR=3) '*tmppth ',tmppth,
     &  '  # path to project scratch folder'
      WRITE(ioout,'(3a)',IOSTAT=IOS,ERR=3) '*docpth ',docpth,
     &  '  # path to project documents'
      WRITE(ioout,'(3a)',IOSTAT=IOS,ERR=3) '*dbspth ',dbspth,
     &  '  # path to local databases'
      WRITE(ioout,'(3a)',IOSTAT=IOS,ERR=3) '*bsmpth ',bsmpth,
     &  '  # path to BASESIMP files'
      WRITE(ioout,'(A,i4,a)',IOSTAT=IOS,ERR=3) '*indx ',INDCFG,
     &  indxcmt(1:lnblnk(indxcmt))
  30  FORMAT('# ESRU system configuration defined by file ',/,'# ',A)

      WRITE(ioout,'(F7.3,2X,F7.3,a)')sitelat,sitelongdif,
     &'   # Latitude & Longitude diff'
      WRITE(ioout,'(I7,2X,F6.3,a)') siteexposureindex,groundrefl,
     &'   # Site exposure & ground refl'
      IF(siteexposureindex.EQ.8)WRITE(ioout,'(3F8.3,A)')
     &  skyview,groundview,buildingview,
     &  '   # views to sky ground buildings'
      write(ioout,'(a)')'* DATABASES'

C Write based on current ipath* state.
      if(ipathmat.eq.0.or.ipathmat.eq.1)then
        WRITE(ioout,'(2A)',IOSTAT=IOS,ERR=3) '*mat  ',
     &      LFMAT(1:lnblnk(LFMAT))
      elseif(ipathmat.eq.2)then
        WRITE(ioout,'(2A)',IOSTAT=IOS,ERR=3) '*stdmat  ',
     &      LFMAT(1:lnblnk(LFMAT))
      endif
      if(ipathmul.eq.0.or.ipathmul.eq.1)then
        WRITE(ioout,'(2A)',IOSTAT=IOS,ERR=3) '*mlc  ',
     &    LFMUL(1:lnblnk(LFMUL))
      elseif(ipathmul.eq.2)then
        WRITE(ioout,'(2A)',IOSTAT=IOS,ERR=3) '*stdmlc  ',
     &    LFMUL(1:lnblnk(LFMUL))
      endif
      if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
        WRITE(ioout,'(2A)',IOSTAT=IOS,ERR=3) '*opt  ',
     &      LOPTDB(1:lnblnk(LOPTDB))
      elseif(ipathoptdb.eq.2)then
        WRITE(ioout,'(2A)',IOSTAT=IOS,ERR=3) '*stdopt  ',
     &      LOPTDB(1:lnblnk(LOPTDB))
      endif
      if(ipathapres.eq.0.or.ipathapres.eq.1)then
        WRITE(ioout,'(2A)',IOSTAT=IOS,ERR=3) '*prs  ',
     &      LAPRES(1:lnblnk(LAPRES))
      elseif(ipathapres.eq.2)then
        WRITE(ioout,'(2A)',IOSTAT=IOS,ERR=3) '*stdprs  ',
     &      LAPRES(1:lnblnk(LAPRES))
      endif
      if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
        WRITE(ioout,'(2A)',IOSTAT=IOS,ERR=3) '*evn  ',
     &      LPRFDB(1:lnblnk(LPRFDB))
      elseif(ipathprodb.eq.2)then
        WRITE(ioout,'(2A)',IOSTAT=IOS,ERR=3) '*stdevn  ',
     &      LPRFDB(1:lnblnk(LPRFDB))
      endif
      WRITE(ioout,'(A,A)') '*clm  ',LCLIM(1:lnblnk(LCLIM))
      if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
        WRITE(ioout,'(2A)',IOSTAT=IOS,ERR=3) '*pdb  ',
     &      LPCDB(1:lnblnk(LPCDB))
      elseif(ipathpcdb.eq.2)then
        WRITE(ioout,'(2A)',IOSTAT=IOS,ERR=3) '*stdpdb  ',
     &      LPCDB(1:lnblnk(LPCDB))
      endif

C << add test for whether model has SHOCC file >>
C      WRITE(ioout,'(A,A)') '*shl  ',
C     &      SHOCCshlFile(1:lnblnk(SHOCCshlFile))

C Ground reflectivity model, no-snow monthly albedos, snow albedo
C (if ground reflectivity model 2 or 3)
      IF(groundreflmodel.NE.1) 
     &  WRITE(ioout,803) '*gref ',groundreflmodel,
     &       (groundreflmonth(I),I=1,12),snowgroundrefl
  803 FORMAT(A,I1,13(1X,F5.3))

C Number of days with snow on the ground 
C (if ground reflectivity model 2)
      IF(groundreflmodel.EQ.2) 
     &  WRITE(ioout,805) '*snow',(dayswithsnow(I),I=1,12)
  805 FORMAT(A,12(1X,I2))

C Snow depth file
C (if ground reflectivity model 3)
      IF(groundreflmodel.EQ.3) WRITE(ioout,'(A,A)',IOSTAT=IOS,ERR=2) 
     &  '*sndf ',SNFNAM(1:lnblnk(SNFNAM))

      if(lnblnk(lctlf).eq.0.or.lctlf(1:7).eq.'UNKNOWN')then
        continue
      else
        WRITE(ioout,'(a,a)') '*ctl  ',LCTLF(1:lnblnk(LCTLF))
      endif

      WRITE(ioout,'(a,I4,a)',IOSTAT=IOS,ERR=2) '*year  ',
     &  IYEAR,' # assessment year'

C Write images if any.
      if(noimg.gt.0)then
        do 142 img=1,noimg
          WRITE(ioout,'(a,a4,2x,a4,2x,a)')'*img ',imgfmt(img),
     &      imgfoc(img),limgfil(img)(1:lnblnk(limgfil(img)))
          if(icfgv.gt.3)then
            WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2)'*imdoc ',
     &        imgdoc(img)(1:lnblnk(imgdoc(img)))
          endif
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
      WRITE(ioout,'(A)')modeltitle(1:lnblnk(modeltitle))
      return

    2 CALL edisp(iuout,' Problem writing config data config...')
      stop 'ecnv aborted.'
    3 CALL edisp(iuout,' Problem writing configuration name...')
      stop 'ecnv aborted.'
      end

C Dummy routines from common3dv.F needed to match implied
C calls in esru_lib.F
      SUBROUTINE ADJVIEW(IER)
      ier=0
      return
      end

      subroutine chgazi(icazi)
      return
      end

      subroutine chgelev(icelev)
      return
      end

      subroutine chgsun(isunhour)
      return
      end

      subroutine chgeye(EVX,EVY,EVZ,VX,VY,VZ,EAN,JITZNM,JITSNM,JITVNO,
     &   JITOBS,JITSNR,JITGRD,JITORG,DIS,JITBND,JITDSP,JITHLS,JITHLZ,
     &   JITPPSW)

C Passed paramters.
      real EVX,EVY,EVZ,VX,VY,VZ,EAN,DIS

C Depending on computer type set integer size of passed parameters.
#ifdef OSI
      integer JITZNM,JITSNM,JITVNO,JITOBS,JITSNR,JITGRD,JITORG
      integer JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW
#else
      integer*8 JITZNM,JITSNM,JITVNO,JITOBS,JITSNR,JITGRD,JITORG
      integer*8 JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW
#endif     
      return
      end

      subroutine chgzonpik(jizgfoc,jnzg)
#ifdef OSI
      integer jizgfoc,jnzg  ! for use with viewtext
#else
      integer*8 jizgfoc,jnzg  ! for use with viewtext
#endif
      return
      end

      subroutine chgzonpikarray(jnznog,jnznogv)
#ifdef OSI
      integer jnznog,jnznogv
#else
      integer*8 jnznog,jnznogv
#endif
      return
      end

      SUBROUTINE EMKVIEW(IUO,CFGOK,IER)
      logical cfgok
      ier=0
      return
      end

      SUBROUTINE INLNST(ITYP)
      return
      end

      subroutine imgdisp(iforce,focus,ier)
      character focus*4
      return
      end

C Dummy subroutine needed to compile (called from library code).
      SUBROUTINE PLELEV(direc)
      CHARACTER direc*1
      return
      end

