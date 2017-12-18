C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-2011.

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
C   a mm (unless otherwise instructed).

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
C UA1 in uncertainty.h
#include "uncertainty.h"
#include "material.h"
#include "espriou.h"
#include "control.h"
#include "dxfdata.h"
      PARAMETER (MSTMC=20)
      
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
      character bidirfile*72,bidirname*12
      COMMON/BIDIRFL/bidirfile,bidirname(MSTMC)

C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

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

      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL XST

      character filname*144,filname2*144,filname1*144,repfile*40
      character inf*144,ouf*144
      character longtfile*144
      character theprimedirective*8
      character iformat*24,fs*1
      character LTMP*144
      character path*72,mpath*72
      character upgcfg*72

      character MODE*4
      character buffer*144

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

C For help messages
      common/pophelp/h(60)   ! needed for dynamic popup
      character H*72
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found
 
C System parameter initializing. Use ioin for the input file,
C ioout for primary output file, ioblk for block storage,
C iotmp2 for miscel output files.
      call ezero
      call curmodule('ecnv')
      cAppName = 'ecnv'
      helpinapp='ecnv'  ! set once for the application
      helpinsub='ecnv'  ! set for MAIN
      modeltitle='UNKNOWN'

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
      ipathapres=0; ipathoptdb=0; ipathpcdb=0; ipathprodb=0
      ipathmat=0; ipathmul=0; ipathclim=0; ipathsbem=0; ipathmsc=0
      ipathmould=0

C Get command line parameters. 
C input formats are: [esp|dxf|ww] where:
C   dxf = AutoCAD V11-14 ASCII files.
C   ww = ww graphic vector command set (output by ESP-r graphic modules).

C output formats are: [esp|dxf|viewer|xfig|vrml] where
C   esp is primarily the geometry of an ESP-r model with cfg and cnn files.
C   dxf is V12-V14 DXF file which includes surfaces and some types of
C      solar obstructions.
C   viewer is a hidden line wire-frame viewing tool included with ESP-r.
C   xfig is a vector drawing package.
C   vrml includes virtual world file and matching html file holding the
C      attributes of the geometric entities.
C   click are dxf points and lines to be used with clickonbitmap facility.

      call parecnv(iverb,conv,incobs,ichop,itmprm,iformat,oformat,inf,
     &  ouf,upgcfg)
      itrc=iverb    ! set trace after call to parcnv
      rotangl=0.0; xscale=1.0; yscale=1.0; zscale=1.        
      entelev=0.; dfltelev=entelev
      entthick=0.0; dfltthick=entthick
      flagins=0; oldlay = 0; oldcol = 1
      currlay = 0; currcol = 1
      minseg = 6

C Confirm conversion of scale.
      dxfconv=conv
C      dxfconv = 1.0
      layuse(1)=0

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs
      write(upath,'(a1,a1)')'.',fs
      write(zonepth,'(a1,a1)')'.',fs
      write(netpth,'(a1,a1)')'.',fs
      write(ctlpth,'(a1,a1)')'.',fs
      write(aimpth,'(a1,a1)')'.',fs
      write(imgpth,'(a1,a1)')'.',fs
      write(radpth,'(a1,a1)')'.',fs
      write(docpth,'(a1,a1)')'.',fs
      write(dbspth,'(a1,a1)')'.',fs
      write(bsmpth,'(a1,a1)')'.',fs
      write(hvacpth,'(a1,a1)')'.',fs

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

C Scan the defaults file for default system configuration file.
C Make temporary use of file unit IFIL+5.  Note: escdef must come
C after scan of .esprc file.
      call escdef(IFIL+5,'-',IER)

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
      elseif(iformat(1:3).eq.'esp')then
        filname1=filname
        call fdroot(filname1,path,LCFGF)
      elseif(iformat(1:2).eq.'ww')then
        filname1=filname
      elseif(iformat(1:5).eq.'gbxml')then
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

C Depending on the output format advise user or do some pre-processing.
      if(oformat(1:6).eq.'viewer')then

        write(outs,'(2a)')'The viewer output file will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp248(iuout,outs,100)

      elseif(oformat(1:4).eq.'vrml')then

        write(outs,'(2a)')'The vrml world will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp248(iuout,outs,100)

      elseif(oformat(1:5).eq.'click')then

C User filename2 for debug data for click output.
        write(outs,'(2a)')'The click data will be: ',
     &                  filname2(1:lnblnk(filname2))
        call edisp248(iuout,outs,100)
        open(ioout,file=filname2,status='UNKNOWN', err=901)

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
          call escdef(iotmp2,'s',IER)
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
          call escdef(iotmp2,'s',IER)
          call clrprb
        endif

      elseif(oformat(1:3).eq.'dxf')then

C If AutoCAD then also find its path so that support files can be
C placed in the same folder.
        write(outs,'(2a)')'The AutoCAD DXF file will be: ',
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
    
C If input format is ww and output is xfig go and do this.
      if(iformat(1:2).eq.'ww'.and.oformat(1:4).eq.'xfig')then
        open(ioin ,file=filname1,status='OLD', err=900)
        open(ioout,file=filname2,status='UNKNOWN', err=901)
        call wwxfig(filname1,IER)
        close(ioout)
        close(ioin)
        stop
      endif
    
C If input format is gbxml and output is esp go and do this.
      if(iformat(1:5).eq.'gbxml'.and.oformat(1:3).eq.'esp')then
        INQUIRE (FILE=filname1,EXIST=XST)
        if(XST)then

C Variables which were set at start of prj.F
C Climate db is on channel IFIL (NB when not in use other binary
C databases may make use of this unit temporarily).
          ICLIM=IFIL; ipathclim=0
          LCLIM=DCLIM
          write(LAPRES,'(a)') DAPRES(1:lnblnk(DAPRES))
          IFMUL=IFIL+3; ipathmul=0
          write(LFMUL,'(a)') DFMUL(1:lnblnk(DFMUL))
          IFMAT=IFIL+4; ipathmat=0
          write(LFMAT,'(a)') DFCON(1:lnblnk(DFCON))
          IFCFG=IFIL+5

          call clrprb  ! clear the model common blocks prior to scan
          write(longtfile,'(a)') filname1(1:lnblnk(filname1)) ! gbxml file
          write(mpath,'(a)') filname2(1:lnblnk(filname2))     ! folder for model
          IUNIT=IFIL+1
          IOPTDB=IFIL+6
          IPRODB=IFIL+6
          write(LOPTDB,'(a)') DOPTDB(1:lnblnk(DOPTDB))
          ipathoptdb=0
          write(lfmould,'(a)') dmdbnam(1:lnblnk(dmdbnam))
          ipathmould=0
          write(LPRFDB,'(a)') DPRFDB(1:lnblnk(DPRFDB))
          ipathprodb=0
          write(LPCDB,'(a)') DPCDB(1:lnblnk(DPCDB))
          ipathpcdb=0
          write(MCMPDBFL,'(a)') DMCMPDBFL(1:lnblnk(DMCMPDBFL))
          ipathmsc=0
          write(LSBEM,'(a)')  DSBEM(1:lnblnk(DSBEM))  ! assign default SBEM db
          ipathsbem=0
          write(LPREDEF,'(a)')  DPREDEF(1:lnblnk(DPREDEF))  ! assign default SBEM db
          ipathpredef=0
          IUTDF=IFIL+7
          IUTDFA=IFIL+8
          write(LTDF,'(a)') 'UNKNOWN'
          write(LTDFA,'(a)') 'UNKNOWN'
          ITDFLG=0
          icfcdb = ifil + 10
          write(LCFCDB,'(a)') DCFCDB(1:lnblnk(DCFCDB))
          lmodellog='project.notes'
          lmodelqa='UNKNOWN'
          bidirfile='UNKNOWN'
          LUALF='UNKNOWN'

C If iverb passed as non-zero then change directive.
          if(iverb.eq.0)then
            call silentxmlread(iunit,longtfile,'- ',mpath,ier)
          elseif(iverb.eq.1)then
            call silentxmlread(iunit,longtfile,'v ',mpath,ier)
          elseif(iverb.eq.2)then
            call silentxmlread(iunit,longtfile,'vv',mpath,ier)
          endif
          call edisp(iuout,mpath)
          call edisp(iuout,modeltitle)
          call edisp248(iuout,modeldocblock,80)
          call usrmsg('Processing of silent file complete.',
     &      'Exiting from project manager','P')

C Clear allocatable arrays
          CALL DeallocateAllArrays
          CALL EPAGEND
          STOP
        endif
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
        call escdef(iotmp2,'s',IER)
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
      elseif(oformat(1:5).eq.'click')then

C Initial code for working with click coordinates and lines.
C First clear the arrays.
        nbdxfcoords=0; nbdxflines=0
        do 29 loop=1,15000
          dxfx(loop)=0.0; dxfy(loop)=0.0; dxfz(loop)=0.0
          idxfedge(loop,1)=0; idxfedge(loop,2)=0
          idxfedgelay(loop)=0; idxfedgecol(loop)=0
  29    continue
        dxfminx=1000.0; dxfminy=1000.0; dxfminz=1000.0
        dxfmaxx= -1000.0; dxfmaxy= -1000.0; dxfmaxz= -1000.0 

C Use scandxf.
        call scandxf(filname,filname1,filname2,itmprm,itrc)

C Write the bounds that were found.
        write(ioout,'(a,6f12.4)') '*bounds',dxfminx,dxfminy,dxfminz,
     &    dxfmaxx,dxfmaxy,dxfmaxz

C Write the number of coordinates and lines.
        write(ioout,'(a,i5)') '*nbcoords',nbdxfcoords
        write(ioout,'(a,i5)') '*nblines',nbdxflines
        do 10 i = 1,numlay
          if (laycolour(i) .gt. 0) then
            write(ioout,'(a,i3,3a,i3)') '*layer',i,' ',
     &        dxflayname(i)(1:lnblnk(dxflayname(i))),' ',laycolour(i)
          else
            write(ioout,'(a,i3,3a)') '*layer',i,' ',
     &        dxflayname(i)(1:lnblnk(dxflayname(i))),' - '
          endif
   10   continue
        close(ioout)

        CALL EPAGEND
        STOP

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
C wegeom writes esp-r (older style) geometry file.
      subroutine wegeom(iotmp2,iv,lf,ier)
#include "building.h"
#include "geometry.h"

C << update this to generate v1.1 geometry file >>
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      integer IZSTOCN
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
5650    FORMAT(i4,124(I4,','))
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
C          if(itrc.gt.1)then
C            if(match)then
C              write(outs,*) 'match for ',indxzon,nbofsurf,x1,y1,z1
C            else
C              write(outs,*) 'added to  ',indxzon,nbofsurf,x1,y1,z1
C            endif
C            call edisp(iuout,outs)
C          endif
        endif
      elseif(oformat(1:5).eq.'click')then
        if (flagins.eq.0) then      
          if(head.eq.'BLK_')then

C Increment chartblk array.
            ichartblk=ichartblk+1
            write(chartblk(ichartblk),'(3f12.4)') xx*dxfconv, 
     &        yy*dxfconv, zz*dxfconv
            nbdxfcoords=nbdxfcoords +1      
            dxfx(nbdxfcoords) = xx*dxfconv
            dxfy(nbdxfcoords) = yy*dxfconv       
            dxfz(nbdxfcoords) = zz*dxfconv       
            write(ioout,'(a,3f12.4,a,i4)') '*coord',xx*dxfconv,
     &        yy*dxfconv,zz*dxfconv,' # ',nbdxfcoords 
          else
            nbdxfcoords=nbdxfcoords +1      
            dxfx(nbdxfcoords) = xx*dxfconv
            dxfy(nbdxfcoords) = yy*dxfconv       
            dxfz(nbdxfcoords) = zz*dxfconv       
            write(ioout,'(a,3f12.4,a,i4)') '*coord',xx*dxfconv,
     &        yy*dxfconv,zz*dxfconv,' # ',nbdxfcoords 
          endif
        else
          nbdxfcoords=nbdxfcoords +1      
          xx = xx-blkxorg
          yy = yy-blkyorg        
          zz = zz-blkzorg      
          if(head.eq.'BLK_')then

C Write chartblk array.
            ichartblk=ichartblk+1
            write(chartblk(ichartblk),'(3f12.4)')  xx*dxfconv,
     &        yy*dxfconv, zz*dxfconv        
            dxfx(nbdxfcoords) = xx*dxfconv
            dxfy(nbdxfcoords) = yy*dxfconv       
            dxfz(nbdxfcoords) = zz*dxfconv       
            write(ioout,'(a,3f12.4,a,i4)') '*coord',xx*dxfconv, 
     &        yy*dxfconv,zz*dxfconv,' # ',nbdxfcoords        
          else
            dxfx(nbdxfcoords) = xx*dxfconv
            dxfy(nbdxfcoords) = yy*dxfconv
            dxfz(nbdxfcoords) = zz*dxfconv
            write(ioout,'(a,3f12.4,a,i4)') '*coord',xx*dxfconv, 
     &        yy*dxfconv,zz*dxfconv,' # ',nbdxfcoords        
          endif
        endif

C Update bounds.
        if(dxfx(nbdxfcoords).lt.dxfminx) dxfminx=dxfx(nbdxfcoords)
        if(dxfy(nbdxfcoords).lt.dxfminy) dxfminy=dxfy(nbdxfcoords)
        if(dxfz(nbdxfcoords).lt.dxfminz) dxfminz=dxfz(nbdxfcoords)
        if(dxfx(nbdxfcoords).gt.dxfmaxx) dxfmaxx=dxfx(nbdxfcoords)
        if(dxfy(nbdxfcoords).gt.dxfmaxy) dxfmaxy=dxfy(nbdxfcoords)
        if(dxfz(nbdxfcoords).gt.dxfmaxz) dxfmaxz=dxfz(nbdxfcoords)
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
      elseif (gcode.eq.90) then
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
            call edisp(iuout,'Warning: Y co-ordinate missing')
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
      integer IZSTOCN
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

      CHARACTER LOUT*72
      CHARACTER CTLDOC*248,LCTLF*72
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
      if(ipathmould.eq.0.or.ipathmould.eq.1)then
        WRITE(ioout,'(3A)',IOSTAT=IOS,ERR=3) '*mould  ',
     &    lfmould(1:lnblnk(lfmould)),' # mould isopleths'
      elseif(ipathmould.eq.2)then
        WRITE(ioout,'(3A)',IOSTAT=IOS,ERR=3) '*stdmould  ',
     &    lfmould(1:lnblnk(lfmould)),' # mould isopleths'
      endif

      if(ipathclim.eq.0.or.ipathclim.eq.1)then
        WRITE(ioout,'(2A)',IOSTAT=IOS,ERR=2) '*clm  ',
     &      LCLIM(1:lnblnk(LCLIM))
      elseif(ipathclim.eq.2)then
        WRITE(ioout,'(2A)',IOSTAT=IOS,ERR=2) '*stdclm  ',
     &      LCLIM(1:lnblnk(LCLIM))
      endif
C      WRITE(ioout,'(A,A)') '*clm  ',LCLIM(1:lnblnk(LCLIM))
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

C Write control file if known.
      if(lnblnk(lctlf).eq.0)then
        continue
      elseif(lctlf(1:7).eq.'UNKNOWN')then
        continue
      elseif(lctlf(1:2).eq.'  ')then
        continue
      else
        WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*ctl  ',
     &    LCTLF(1:lnblnk(LCTLF))
      endif

C Write model contents file if known.
      if(lnblnk(lmodelqa).eq.0)then
        continue
      elseif(lmodelqa(1:7).eq.'UNKNOWN')then
        continue
      elseif(lmodelqa(1:2).eq.'  ')then
        continue
      else
        WRITE(IFCFG,'(a,a)',IOSTAT=IOS,ERR=2) '*contents ',
     &    lmodelqa(1:lnblnk(lmodelqa))
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
      WRITE(ioout,'(A)')lmodellog(1:lnblnk(lmodellog))
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
      integer JITZNM,JITSNM,JITVNO,JITOBS,JITVIS,JITVOBJ,JITSNR,JITGRD
      integer JITORG,JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW
#else
      integer*8 JITZNM,JITSNM,JITVNO,JITOBS,JITVIS,JITVOBJ,JITSNR
      integer*8 JITGRD,JITORG,JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW
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
      
      SUBROUTINE EDMLDB2(chgdb,ACTION,isel,IER)
      logical chgdb
      character*1 ACTION
      integer isel,ier
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

      subroutine Find_season_parameters()
      return
      end

      SUBROUTINE BASESIMP_INPUTS(ICOMP,IER)
      integer icomp,ier
      return
      end

C Local variant of command line parsing.
C parcnv parse command line paramters for ecnv.
      subroutine parecnv(iverb,conv,iobs,ichop,itmprm,iform,oform,
     &                   inf,ouf,ucfg)
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      integer iargc,m
      integer termtype
      integer i,iverb,itmprm,iobs,ichop
      real conv
      character argument*72,prog*72,inf*144,outs*248,appn*24
      character iform*24,oform*24,ouf*144,ucfg*72,mode*8

      inf  = 'UNKNOWN'
      iform = 'UNKNOWN'
      oform = 'UNKNOWN'
      ouf  = 'UNKNOWN'
      ucfg  = 'UNKNOWN'
      conv = 0.001
      iverb = 0
      itmprm = 1
      iobs  = 0
      ichop = 0

C Get number of arguments and command name.
      termtype = -1
      mode = 'text'
      m = iargc()
      i = 0
      call getarg(i,prog)
      call findapp(prog,appn)

C If one parameter.
      if(m.eq.0)then
        stop
      elseif(m.ge.1)then
  41    i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)
        if(argument(1:5).eq.'-help')then
      call edisp(iuout,'The product model filter. ')
      call edisp(iuout,' Use: [-v] [{-mm|-metre}] ')
      call edisp(iuout,'  -if {esp|dxf|ww|gbxml} -in <file> ')
      call edisp(iuout,
     &  '  -of {esp|dxf|viewer|xfig|vrml} -out <file>')
      call edisp(iuout,' ')
      call edisp(iuout,'Where options are:')
      call edisp(iuout,' -if [esp|dxf|ww] = input format')
      call edisp(iuout,'      (dxf = AutoCAD V11-12)')
      call edisp(iuout,'      (ww = ww graphic commands)')
      call edisp(iuout,'      (gbxml = gbXML file)')
      call edisp(iuout,' -in [file]  = input file')
      call edisp(iuout,
     &  ' -of [esp|dxf|viewer|xfig|vrml] = dest format')
      call edisp(iuout,' -out [file] = output file')
      call edisp(iuout,' -v          = verbose mode')
      call edisp(iuout,' -mm -metre  = dxf coords in mm (default)')
      call edisp(iuout,' -keep       = retain scratch files')
      call edisp(iuout,' -obs        = include esp-r obstructions')
      call edisp(iuout,' ')
      call edisp(iuout,'The following combinations are supported:')
      call edisp(iuout,'  input format  output format')
      call edisp(iuout,'  esp-r         dxf, viewer, vrml')
      call edisp(iuout,'  dxf           esp-r, viewer')
      call edisp(iuout,'  gbxml         esp-r')
      call edisp(iuout,' ')
      call edisp(iuout,'If converting to esp-r then an additional') 
      call edisp(iuout,'parameter is allowed:')
      call edisp(iuout,' -u [upgrade] when updating a model from ')
      call edisp(iuout,'    registration stage.')
      call edisp(iuout,' ')
      call edisp(iuout,' -help :this help message.')
      call edisp(iuout,' ')
          stop
        elseif(argument(1:2).eq.'-v')then
          iverb = 2   ! tell application to run with debug on
        elseif(argument(1:3).eq.'-mm')then
          conv = 0.001
        elseif(argument(1:6).eq.'-metre')then
          conv = 1.00
        elseif(argument(1:4).eq.'-obs')then
          iobs = 1
        elseif(argument(1:5).eq.'-chop')then
          ichop = 1
        elseif(argument(1:5).eq.'-keep')then
          itmprm = 0
        elseif(argument(1:3).eq.'-in')then
          i=i+1
          call getarg(i,inf)
        elseif(argument(1:4).eq.'-out')then
          i=i+1
          call getarg(i,ouf)
        elseif(argument(1:2).eq.'-u')then
          i=i+1
          call getarg(i,ucfg)
          call edisp(iuout,'Will attempt to update configuration')
        elseif(argument(1:3).eq.'-if')then
          i=i+1
          call getarg(i,iform)
        elseif(argument(1:3).eq.'-of')then
          i=i+1
          call getarg(i,oform)
        endif
        goto 41

  42    continue
        write(outs,'(4a)') 'Starting ecnv in ',
     &    oform(1:lnblnk(oform)),' mode with in file ',
     &    inf(1:lnblnk(inf))
        call edisp248(iuout,outs,100)
        write(outs,'(3a)') 'and output file ',ouf(1:lnblnk(ouf)),'.'
        call edisp248(iuout,outs,100)
        return
      endif
      end

C Local copies of subroutines supporting gbxml parsing.
C  CKVERT : checks vertex lists & returns if zone is fully bounded.
C initcaspattern - sets up standard room uses with a bit of diversity.
C  pregist: register a new project.
C  opendb: Standard opening of thermophysical and optical data.
C  POINTTOLINE: determines distance from a 3D point to a 3D line.
C  Calenmanage: sets up and manages a calendar for ESP-r.
C  Calenprint:  displays a calendar for year iyear (or a few months)
C updatectlfornewdaytype adds an additional day type to each of the
C model control domains. 
C ADDCTLD  Add or delete a building/plant/flow/global/optical control day type.
C BNDOBJ: Does range checking on objects to be displayed.
C  makeCFCfile: Creates a *.cfc file from imported GSLedit data.
C LKMENU is called by INITPF (bps.F). It displays.

C ************* CKVERT 
C CKVERT: Checks vertex lists and returns whether zone is fully 
C bounded (bound=.true.) and which surfaces/vertex links are unmatched.
C Number of unbounded edges in iub, number inverted surfaces inve.
C IVB = 0 is silent, IVB=1 feedback on errors. act '-' report only,
C act 'r' attempt repair.

C << could this be done with G6 and with ??

      SUBROUTINE CKVERT(ivb,ICOMP,bounded,iub,inve,act,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
      integer ivb   ! verbosity
      integer icomp ! the current zone
      logical bounded ! true if fully bounded
      integer iub     ! number of unbounded edges
      integer inve    ! number of inverted edges
      character act*1 ! - report r repair
      integer ier     ! error state

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

C Tollerance for surface matching.
      real ANGCC  ! angle between surfaces tolerance
      real CACC   ! tolerance between vertices
      real DACC   ! tolerance along line
      real COGCC  ! tolerance between surface COG
      real SNACC  ! tolerance between surface areas
      integer IACC ! number of matching corners outside dist tolerance
      common/matching/ANGCC,CACC,DACC,COGCC,SNACC,IACC

      DIMENSION COG1(3),COG2(3),COG3(3)
      DIMENSION itv1(MS*MV) ! 1st of every edge in every surface
      DIMENSION itv2(MS*MV) ! 2nd of every edge in every surface
      DIMENSION irelsur(MS*MV) ! which surface assoc with every edge
      DIMENSION invesur(MS),jvn1(MV)
      CHARACTER outs*124,louts*496,msg*96
      logical hilight
      logical greyok,matchver,notin,OK,dok,adjusted
      logical newgeo  ! version of geom file
      integer icursur,ipair
      integer iii

#ifdef OSI
      integer iupdown,isym,iix,iiy,iicol    ! passed to etplot
#else
      integer*8 iupdown,isym,iix,iiy,iicol    ! passed to etplot
#endif

      greyok=.false.      ! see if colour available
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

C New or older geometry file?
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      adjusted=.false.  ! set true later if change requires file update

C Jump back point in case the edge lists have been updated
C (the itv1 & itv2 need to be re-formed.
  42  continue
  
C Take a line pair and then look and see if it is referenced in
C reverse order just once. itv1 & itv2 are the vertices at the
C start and end of each edge, its1 is the reference surface. 
      if(ivb.ne.0)call tstamp('>','PRJ: checking vertex match')
      ipair=0; inve=0; iub=0
      DO IS=1,NSUR
        invesur(IS)=0
        if(NVER(IS).eq.0)then
          continue   ! what about itv2 value for this?
        else
          DO IV=1,NVER(IS)  ! need to trap for NVER zero
            ipair=ipair+1
            itv1(ipair)=JVN(IS,IV)

C Also check the last vertex back to the first.
            if(IV.eq.NVER(IS))then
              itv2(ipair)=JVN(IS,1)
            else
              itv2(ipair)=JVN(IS,IV+1)
            endif
            irelsur(ipair)=IS
          ENDDO  ! of NVER
        endif
      ENDDO      ! of NSUR

C Count the number of times the edge mentioned (in both directions).
      bounded=.true.
      hilight=.false.
      do 144 ip=1,ipair
        matchab = 0; matchba = 0
        do ipck=1,ipair

C First see if reverse match has been found.
          if(itv1(ip).eq.itv2(ipck).and.itv2(ip).eq.itv1(ipck))then
            matchba = matchba +1
          elseif(itv1(ip).eq.itv1(ipck).and.itv2(ip).eq.itv2(ipck))then
            matchab = matchab +1
          endif
        enddo
        if(matchab.ne.matchba)then

C The imbalance between matchab and matchba is the number of problem
C edges.  If ivimb=2 then probably two edges in same direction, if
C ivimb=1 then we have an unmatched edge.
          bounded=.false.
          ivimb=(abs((matchab+10)-(matchba+10)))
          if(ivimb.eq.2)then
            write(outs,'(a,i3,a,i3,3a)')
     &        'The edge between vertices ',itv1(ip),' &',
     &        itv2(ip),' of ',SNAME(icomp,irelsur(ip)),
     &        ' may be adjacent to a reversed surface.'
            if(ivb.ne.0)then
              call edisp(iuout,outs)
              if(MMOD.lt.8)then
                continue
              else
                hilight=.true.
              endif
            endif

C Incrememt hits for zone as well as for this surface.
            inve=inve+1
            invesur(irelsur(ip))=invesur(irelsur(ip))+1
    
          elseif(ivimb.eq.1)then
            icursur=irelsur(ip)  ! remember the current surface
            write(outs,'(a,i3,a,i3,3a)')
     &        'The edge between vertices ',itv1(ip),' &',itv2(ip),
     &        ' of ',SNAME(icomp,icursur),' may be unbounded.'
            if(ivb.ne.0)then
              call edisp(iuout,outs)
              if(MMOD.lt.8)then
                continue
              else
                hilight=.true.
              endif
            endif
            iub=iub+1

            if(act(1:1).eq.'-')return   ! report only

C This is a great place to look for vertices that are not
C part of this surface's edges (notin true) that could be
C added. If we have a corrupt JVN list (a zero entry) ignore.
            do iwhich1=1,NTV
              notin=.true.
              if(NVER(icursur).eq.0)then
                notin=.false.
              else
                do iwhich2=1,NVER(icursur)
                  if(iwhich1.eq.JVN(icursur,iwhich2)) notin=.false.
                  if(JVN(icursur,iwhich2).eq.0) notin=.false.
                enddo
              endif
              if(notin)then
                call pointtoline(iwhich1,itv1(ip),itv2(ip),offset,
     &            matchver)
                if(itv1(ip).eq.itv2(ip))then
                  matchver=.false.  ! cannot deal with this
                endif
                if(matchver)then
                  if(offset.lt.CACC)then
                    write(outs,'(a,i3,a,3f9.4,a,f6.4,a,i3,a,i3,2a)')
     &                'Vertex ',iwhich1,' @',X(iwhich1),Y(iwhich1),
     &                Z(iwhich1),' is close (',offset,') to edge ',
     &                itv1(ip),' &',itv2(ip),' of surface ',
     &                SNAME(icomp,icursur)
                    if(ivb.ne.0) call edisp(iuout,outs)

C Loop through the current surface and find the position in the
C list for itv1(ip) so insertion can come just after this point.
                    itsposition=0
                    do iwhich2=1,NVER(icursur)
                      if(itv1(ip).eq.JVN(icursur,iwhich2))then
                        itsposition=iwhich2
                      endif
                    enddo
                    ivtoadd=iwhich1  ! remember it
                    write(louts,'(a,124i4)') 'cur list: ',
     &                (JVN(icursur,ii),ii=1,NVER(icursur))
                    if(ivb.ne.0) call edisp248(iuout,louts,100)

C If current surface (icursur) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge and then
C inserting the new vertex index.
                    if(NVER(icursur)+1.le.MV)then
                      if(offset.lt.0.004)then
                        ok=.true.  ! If within 4mm just do it
                        write(outs,'(a,f6.4,3a)') 'Inserting a (',
     &                    offset,'m) to edge vertex within ',
     &                    SNAME(icomp,icursur),'.'
                        if(ivb.ne.0) call edisp(iuout,outs)
                      else
                        dok=.true.
                        call edisp248(iuout,louts,100)
                        write(msg,'(a,f6.4,3a)') 'Insert this (',offset,
     &                    'm) to edge vertex in ',
     &                    SNAME(icomp,icursur),'?'
                        call askok(' ',msg,ok,dok,nbhelp)
                      endif
                    else
                      ok=.false.
                    endif
                    if(ok)then
                      adjusted=.true.
                      NVER(icursur)=NVER(icursur)+1

C If its location in the list is actually the last in the list?
                      if(itsposition+1.eq.NVER(icursur))then
                        JVN(icursur,itsposition+1)=ivtoadd
                      else
                        iii=NVER(icursur)+1
  348                   continue
                        iii=iii-1
                        JVN(icursur,iii)=JVN(icursur,iii-1)
                        IF(iii.GT.itsposition+1)GOTO 348
                        JVN(icursur,itsposition+1)=ivtoadd
                      endif
C Debug.
                      write(louts,'(a,124i4)') 'adj list: ',
     &                  (JVN(icursur,ii),ii=1,NVER(icursur))
                      if(ivb.ne.0) call edisp248(iuout,louts,100)
                      iub=iub-1   ! decrement counter of unbounded
                      goto 42     ! jump back and reform itv1 & itv2
                    endif
                  endif
                endif
              endif
            enddo
          endif

          if(hilight)then

C The hilight block of code removed from ecnv version.
          endif
        endif
 144  continue
      if(ivb.ne.0)then

C If all edges of a surface are likely to be inverted offer
C the user the option to invert.
C Debug.
C        write(6,*) 'reversed hits per surface ',invesur
C        write(6,*) 'edges per surface ',NVER
        DO IS=1,NSUR
          if(NVER(IS).gt.0.and.(invesur(IS).eq.NVER(IS)))then
            write(louts,'(a,124i4)') 'current list: ',
     &        (JVN(is,ii),ii=1,NVER(is))
            call edisp248(iuout,louts,100)
            write(outs,'(3a)')
     &        'Checked the edges of surface ',SNAME(icomp,is),
     &        ' looks like surface needs to be inverted.'
            dok=.true.
            call askok(outs,'Invert its edges?',ok,dok,nbhelp)
            if(ok)then
              adjusted=.true.  ! ensure option to save is given
              do 146, iyy = 1,NVER(is)
                jvn1(iyy)=JVN(is,iyy)
  146         CONTINUE
              JVN(is,1)=jvn1(2)
              JVN(is,2)=jvn1(1)
              do 147, iyy = 3,NVER(is)
                izz=NVER(is)+3-iyy
                JVN(is,iyy)=jvn1(izz)
  147         CONTINUE
              write(louts,'(a,124i4)') 'inverted list is ',
     &          (JVN(is,ii),ii=1,NVER(is))
              call edisp248(iuout,louts,100)
            endif
          endif
        ENDDO
      endif

C If any of the edge lists were altered give option to update.
      if(adjusted)then
        dok=.true.
        if(ivb.eq.0)then
          ok=.true.
        else
          call askok(' ','Update geometry file?',ok,dok,nbhelp)
        endif

C Update the geometry file.
        if(ok)then
          if(newgeo)then
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,
     &        iuout,3,IER)
          else
            call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
          endif
        endif
      endif
      if(.not.bounded)then
        if(ivb.ne.0)then
          call edisp(iuout,'The enclosure is improperly bounded and')
          write(outs,'(a,i3,a)')'there are ',inve+iub,
     &      ' problem edges (hilighted in wireframe).'
          call edisp(iuout,outs)
        endif
      elseif(bounded)then
        if(ivb.ne.0)call edisp(iuout,' Enclosure is fully bounded.')
      endif

      RETURN
      END

C ******* initcaspattern
C initcaspattern populates the casual gain common blocks for
C a number of room use patterns with some diversity across
C day types and during days. Called from PRJFMK
      subroutine initcaspattern(icomp,igu,ier)

#include "building.h"
#include "schedule.h"
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found 

      integer loop

      helpinsub='initcaspattern'  ! set for subroutine

C For each of the current day tpes.
      DO 399 I=1,NBDAYTYPE
        if(igu.eq.8)then

C Setup nothing happens here e.g. one period for each casual gain slot.
          ctlstr(icomp,I)='no control of air flow  '
          NAC(I)=1; IACS(I,1)=0; IACF(I,1)=24
          ACI(I,1)=0.0; ACV(I,1)=0.0; IPT(I,1)=0; TA(I,1)=0.0
          NCAS(I)=3
          ICGS(I,1)=0; ICGF(I,1)=24; ICGUnit(I,1)=0
          CMGS(I,1)=0.0; CMGL(I,1)=0.0
          RADC(I,1)=0.6; CONC(I,1)=0.4  ! CIBSE Guide A 6.3
          ICGT(I,1)=1
          ICGS(I,2)=0; ICGF(I,2)=24; ICGUnit(I,2)=0
          CMGS(I,2)=0.0; CMGL(I,2)=0.0
          RADC(I,2)=0.3; CONC(I,2)=0.7  ! CIBSE Table A 6.5
          ICGT(I,2)=2 
          ICGS(I,3)=0; ICGF(I,3)=24; ICGUnit(I,3)=0
          CMGS(I,3)=0.0; CMGL(I,3)=0.0
          RADC(I,3)=0.4; CONC(I,3)=0.6
          ICGT(I,3)=3
     
        elseif(igu.eq.9)then

C A cellular office with some diversity. Uses CIBSE Guide A 6.3
C for radiant-convective splits. Saturday morning use. Sunday &
C holidays essentially unoccupied.
          ctlstr(icomp,I)='no control of air flow  '
          NAC(I)=1; IACS(I,1)=0; IACF(I,1)=24
          ACI(I,1)=0.5; ACV(I,1)=0.0; IPT(I,1)=0; TA(I,1)=0.0
          if(I.eq.1)then
            NCAS(I)=15
            do loop=1,15  ! set common information
              if(loop.ge.1.and.loop.le.7)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.ge.8.and.loop.le.12)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2  ! for lights
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              elseif(loop.ge.13.and.loop.le.15)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -3  ! for equip
                RADC(I,loop)=0.4; CONC(I,loop)=0.6
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=8; CMGS(I,1)=0.0; CMGL(I,1)=0.0  ! occupants bit of diversity
            ICGS(I,2)=8; ICGF(I,2)=9; CMGS(I,2)=30.0; CMGL(I,2)=15.0
            ICGS(I,3)=9; ICGF(I,3)=12; CMGS(I,3)=100.0; CMGL(I,3)=50.0
            ICGS(I,4)=12; ICGF(I,4)=13; CMGS(I,4)=60.0; CMGL(I,4)=30.0
            ICGS(I,5)=13; ICGF(I,5)=16; CMGS(I,5)=100.0; CMGL(I,5)=50.0
            ICGS(I,6)=16; ICGF(I,6)=18; CMGS(I,6)=80.0; CMGL(I,6)=40.0
            ICGS(I,7)=18; ICGF(I,7)=24; CMGS(I,7)=0.0; CMGL(I,7)=0.0
              
            ICGS(I,8)=0; ICGF(I,8)=8; CMGS(I,8)=0.0; CMGL(I,8)=0.0 ! lights W/m2
            ICGS(I,9)=8; ICGF(I,9)=12; CMGS(I,9)=7.0; CMGL(I,8)=0.0
            ICGS(I,10)=12; ICGF(I,10)=14; CMGS(I,10)=5.0; CMGL(I,10)=0.0
            ICGS(I,11)=14; ICGF(I,11)=18; CMGS(I,11)=7.0; CMGL(I,11)=0.0
            ICGS(I,12)=18; ICGF(I,12)=24; CMGS(I,12)=1.0; CMGL(I,12)=0.0

            ICGS(I,13)=0; ICGF(I,13)=7; CMGS(I,13)=2.0; CMGL(I,13)=0.0   ! equipment W/m2
            ICGS(I,14)=7; ICGF(I,14)=17; CMGS(I,14)=11.7; CMGL(I,14)=0.0 ! equipment W/m2
            ICGS(I,15)=17; ICGF(I,15)=24; CMGS(I,15)=3.0; CMGL(I,15)=0.0 ! sbem equipment W/m2
            
          elseif(I.eq.2)then
            NCAS(I)=10  ! saturdays only mornings
            do loop=1,10
              if(loop.ge.1.and.loop.le.4)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.ge.5.and.loop.le.7)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              elseif(loop.ge.8.and.loop.le.10)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -3
                RADC(I,loop)=0.4; CONC(I,loop)=0.6
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=8; CMGS(I,1)=0.0; CMGL(I,1)=0.0  ! occupants only in morning
            ICGS(I,2)=8; ICGF(I,2)=9; CMGS(I,2)=30.0; CMGL(I,2)=15.0
            ICGS(I,3)=9; ICGF(I,3)=12; CMGS(I,3)=100.0; CMGL(I,3)=50.0
            ICGS(I,4)=12; ICGF(I,4)=24; CMGS(I,4)=0.0; CMGL(I,4)=0.0
            ICGS(I,5)=0; ICGF(I,5)=8; CMGS(I,5)=0.0; CMGL(I,5)=0.0 ! lights W/m2
            ICGS(I,6)=8; ICGF(I,6)=12; CMGS(I,6)=7.0; CMGL(I,6)=0.0
            ICGS(I,7)=12; ICGF(I,7)=24; CMGS(I,7)=5.0; CMGL(I,7)=0.0
            ICGS(I,8)=0; ICGF(I,8)=7; CMGS(I,8)=2.0; CMGL(I,8)=0.0  ! equipment W/m2
            ICGS(I,9)=7; ICGF(I,9)=14; CMGS(I,9)=11.7; CMGL(I,9)=0.0
            ICGS(I,10)=14; ICGF(I,10)=24; CMGS(I,10)=2.0; CMGL(I,10)=0.0
          elseif(I.eq.3)then
            NCAS(I)=3  ! sunday minimal
            do loop=1,3
              if(loop.eq.1)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.eq.2)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              elseif(loop.eq.3)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -3
                RADC(I,loop)=0.4; CONC(I,loop)=0.6
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=24; CMGS(I,1)=0.0; CMGL(I,1)=0.0  ! occupants only in morning
            ICGS(I,2)=0; ICGF(I,2)=24; CMGS(I,2)=1.0; CMGL(I,2)=0.0  ! emergency lights W/m2
            ICGS(I,3)=0; ICGF(I,3)=24; CMGS(I,3)=2.0; CMGL(I,3)=0.0
          else
            NCAS(I)=3  ! holiday & other minimal
            do loop=1,3
              if(loop.eq.1)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.eq.2)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              elseif(loop.eq.3)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -3
                RADC(I,loop)=0.4; CONC(I,loop)=0.6
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=24; CMGS(I,1)=0.0; CMGL(I,1)=0.0  ! occupants only in morning
            ICGS(I,2)=0; ICGF(I,2)=24; CMGS(I,2)=1.0; CMGL(I,2)=0.0  ! emergency lights W/m2
            ICGS(I,3)=0; ICGF(I,3)=24; CMGS(I,3)=2.0; CMGL(I,3)=0.0
          endif
        elseif(igu.eq.10)then

C An open plan cellular office m2/person with some diversity.
C Uses CIBSE Guide A 6.3 for radiant-convective splits. Saturday
C morning use. Sunday & holidays essentially unoccupied. Assume
C m2/person with 9 as 100% occupied.
          ctlstr(icomp,I)='no control of air flow  '
          NAC(I)=1; IACS(I,1)=0; IACF(I,1)=24
          ACI(I,1)=0.5; ACV(I,1)=0.0; IPT(I,1)=0; TA(I,1)=0.0
          if(I.eq.1)then
            NCAS(I)=15
            do loop=1,15  ! set common information
              if(loop.ge.1.and.loop.le.7)then
                ICGUnit(I,loop)=2; ICGT(I,loop)= -1  ! m2/occup
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.ge.8.and.loop.le.12)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2  ! for lights
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              elseif(loop.ge.13.and.loop.le.15)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -3  ! for equip
                RADC(I,loop)=0.4; CONC(I,loop)=0.6
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=8; CMGS(I,1)=0.0; CMGL(I,1)=0.0  ! occupants bit of diversity
            ICGS(I,2)=8; ICGF(I,2)=9; CMGS(I,2)=30.0; CMGL(I,2)=30.0
            ICGS(I,3)=9; ICGF(I,3)=12; CMGS(I,3)=9.0; CMGL(I,3)=7.0
            ICGS(I,4)=12; ICGF(I,4)=13; CMGS(I,4)=15.0; CMGL(I,4)=15.0
            ICGS(I,5)=13; ICGF(I,5)=16; CMGS(I,5)=9.0; CMGL(I,5)=9.0
            ICGS(I,6)=16; ICGF(I,6)=18;CMGS(I,6)=11.25;CMGL(I,6)=11.25
            ICGS(I,7)=18; ICGF(I,7)=24; CMGS(I,7)=0.0; CMGL(I,7)=0.0
              
            ICGS(I,8)=0; ICGF(I,8)=8; CMGS(I,8)=0.0; CMGL(I,8)=0.0 ! lights W/m2
            ICGS(I,9)=8; ICGF(I,9)=12; CMGS(I,9)=7.0; CMGL(I,8)=0.0
            ICGS(I,10)=12; ICGF(I,10)=14; CMGS(I,10)=5.0; CMGL(I,10)=0.0
            ICGS(I,11)=14; ICGF(I,11)=18; CMGS(I,11)=7.0; CMGL(I,11)=0.0
            ICGS(I,12)=18; ICGF(I,12)=24; CMGS(I,12)=1.0; CMGL(I,12)=0.0

            ICGS(I,13)=0; ICGF(I,13)=7; CMGS(I,13)=2.0; CMGL(I,13)=0.0  ! equipment W/m2
            ICGS(I,14)=7; ICGF(I,14)=17; CMGS(I,14)=11.7; CMGL(I,14)=0.0 ! equipment W/m2
            ICGS(I,15)=17; ICGF(I,15)=24; CMGS(I,15)=3.0; CMGL(I,15)=0.0 ! sbem equipment W/m2
          elseif(I.eq.2)then
            NCAS(I)=10  ! saturdays only mornings
            do loop=1,10
              if(loop.ge.1.and.loop.le.4)then
                ICGUnit(I,loop)=2; ICGT(I,loop)= -1  ! m2/occup
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.ge.5.and.loop.le.7)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              elseif(loop.ge.8.and.loop.le.10)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -3
                RADC(I,loop)=0.4; CONC(I,loop)=0.6
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=8; CMGS(I,1)=0.0; CMGL(I,1)=0.0  ! occupants only in morning
            ICGS(I,2)=8; ICGF(I,2)=9; CMGS(I,2)=30.0; CMGL(I,2)=30.0
            ICGS(I,3)=9; ICGF(I,3)=12; CMGS(I,3)=9.0; CMGL(I,3)=9.0
            ICGS(I,4)=12; ICGF(I,4)=24; CMGS(I,4)=0.0; CMGL(I,4)=0.0
            ICGS(I,5)=0; ICGF(I,5)=8; CMGS(I,5)=0.0; CMGL(I,5)=0.0 ! lights W/m2
            ICGS(I,6)=8; ICGF(I,6)=12; CMGS(I,6)=7.0; CMGL(I,6)=0.0
            ICGS(I,7)=12; ICGF(I,7)=24; CMGS(I,7)=5.0; CMGL(I,7)=0.0
            ICGS(I,8)=0; ICGF(I,8)=7; CMGS(I,8)=2.0; CMGL(I,8)=0.0  ! equipment W/m2
            ICGS(I,9)=7; ICGF(I,9)=14; CMGS(I,9)=11.7; CMGL(I,9)=0.0
            ICGS(I,10)=14; ICGF(I,10)=24; CMGS(I,10)=2.0; CMGL(I,10)=0.0
          elseif(I.eq.3)then
            NCAS(I)=3  ! sunday minimal
            do loop=1,3
              if(loop.eq.1)then
                ICGUnit(I,loop)=2; ICGT(I,loop)= -1  ! m2/occup
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.eq.2)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              elseif(loop.eq.3)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -3
                RADC(I,loop)=0.4; CONC(I,loop)=0.6
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=24; CMGS(I,1)=0.0; CMGL(I,1)=0.0  ! occupants only in morning
            ICGS(I,2)=0; ICGF(I,2)=24; CMGS(I,2)=1.0; CMGL(I,2)=0.0  ! emergency lights W/m2
            ICGS(I,3)=0; ICGF(I,3)=24; CMGS(I,3)=2.0; CMGL(I,3)=0.0
          else
            NCAS(I)=3  ! holiday & other minimal
            do loop=1,3
              if(loop.eq.1)then
                ICGUnit(I,loop)=2; ICGT(I,loop)= -1  ! m2/occup
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.eq.2)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              elseif(loop.eq.3)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -3
                RADC(I,loop)=0.4; CONC(I,loop)=0.6
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=24; CMGS(I,1)=0.0; CMGL(I,1)=0.0  ! occupants only in morning
            ICGS(I,2)=0; ICGF(I,2)=24; CMGS(I,2)=1.0; CMGL(I,2)=0.  ! emergency lights W/m2
            ICGS(I,3)=0; ICGF(I,3)=24; CMGS(I,3)=2.0; CMGL(I,3)=0.0
          endif
        elseif(igu.eq.11)then

C Setup an office corridor with peaks at start and end of office
C hours as well as around lunch. Brief cleaning each morning.
C Uses CIBSE Guide A 6.3 for radiant-convective splits. Saturday 
C morning use. Sunday & holidays essentially unoccupied.
          ctlstr(icomp,I)='no control of air flow  '
          NAC(I)=1; IACS(I,1)=0; IACF(I,1)=24
          ACI(I,1)=0.5; ACV(I,1)=0.0; IPT(I,1)=0; TA(I,1)=0.0
          if(I.eq.1)then
            NCAS(I)=15
            do loop=1,15  ! set common information
              if(loop.ge.1.and.loop.le.9)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.ge.10.and.loop.le.12)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2  ! for lights
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              elseif(loop.ge.13.and.loop.le.15)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -3  ! for equip
                RADC(I,loop)=0.4; CONC(I,loop)=0.6
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=7; CMGS(I,1)=30.0; CMGL(I,1)=15.0  ! occupants at shift changes
            ICGS(I,2)=7; ICGF(I,2)=8; CMGS(I,2)=200.0; CMGL(I,2)=100.0
            ICGS(I,3)=8; ICGF(I,3)=9; CMGS(I,3)=300.0; CMGL(I,3)=150.0
            ICGS(I,4)=9; ICGF(I,4)=12; CMGS(I,4)=100.0; CMGL(I,4)=50.0
            ICGS(I,5)=12; ICGF(I,5)=14; CMGS(I,5)=200.0; CMGL(I,5)=100.0
            ICGS(I,6)=14; ICGF(I,6)=17; CMGS(I,6)=150.0; CMGL(I,6)=80.0
            ICGS(I,7)=17; ICGF(I,7)=18; CMGS(I,7)=350.0; CMGL(I,7)=170.0
            ICGS(I,8)=18; ICGF(I,8)=21; CMGS(I,8)=70.0; CMGL(I,8)=35.0
            ICGS(I,9)=21; ICGF(I,9)=24; CMGS(I,9)=30.0; CMGL(I,9)=15.0
              
            ICGS(I,10)=0; ICGF(I,10)=7; CMGS(I,10)=1.0; CMGL(I,10)=0.0 ! lights W/m2
            ICGS(I,11)=7; ICGF(I,11)=21; CMGS(I,11)=8.0; CMGL(I,11)=0.0
            ICGS(I,12)=21; ICGF(I,12)=24; CMGS(I,12)=1.0; CMGL(I,12)=0.0

            ICGS(I,13)=0; ICGF(I,13)=7; CMGS(I,13)=0.0; CMGL(I,13)=0.0  ! equipment W/m2
            ICGS(I,14)=7; ICGF(I,14)=8; CMGS(I,14)=3.0; CMGL(I,14)=0.0  ! equipment W/m2 cleaning
            ICGS(I,15)=8; ICGF(I,15)=24; CMGS(I,15)=0.0; CMGL(I,15)=0.0   ! sbem equipment W/m2
          elseif(I.eq.2)then
            NCAS(I)=12  ! saturdays only mornings
            do loop=1,12
              if(loop.ge.1.and.loop.le.6)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.ge.7.and.loop.le.9)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              elseif(loop.ge.10.and.loop.le.12)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -3
                RADC(I,loop)=0.4; CONC(I,loop)=0.6
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=7; CMGS(I,1)=30.0; CMGL(I,1)=15.0  ! occupants only in morning
            ICGS(I,2)=7; ICGF(I,2)=8; CMGS(I,2)=100.0; CMGL(I,2)=50.0
            ICGS(I,3)=8; ICGF(I,3)=9; CMGS(I,3)=200.0; CMGL(I,3)=100.0
            ICGS(I,4)=9; ICGF(I,4)=12; CMGS(I,4)=80.0; CMGL(I,4)=40.0
            ICGS(I,5)=12; ICGF(I,5)=14; CMGS(I,5)=100.0; CMGL(I,5)=50.0
            ICGS(I,6)=14; ICGF(I,6)=24; CMGS(I,6)=30.0; CMGL(I,6)=15.0
            ICGS(I,7)=0; ICGF(I,7)=8; CMGS(I,7)=1.0; CMGL(I,7)=0.0 ! lights W/m2
            ICGS(I,8)=8; ICGF(I,8)=12; CMGS(I,8)=7.0; CMGL(I,8)=0.0
            ICGS(I,9)=12; ICGF(I,9)=24; CMGS(I,9)=5.0; CMGL(I,9)=0.0
            ICGS(I,10)=0; ICGF(I,10)=7; CMGS(I,10)=0.0; CMGL(I,10)=0.0  ! equipment W/m2
            ICGS(I,11)=7; ICGF(I,11)=14; CMGS(I,11)=1.0; CMGL(I,11)=0.0
            ICGS(I,12)=14; ICGF(I,12)=24; CMGS(I,12)=0.0; CMGL(I,12)=0.0
          else
            NCAS(I)=3  ! Sunday holiday & other minimal
            do loop=1,3
              if(loop.eq.1)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.eq.2)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              elseif(loop.eq.3)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -3
                RADC(I,loop)=0.4; CONC(I,loop)=0.6
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=24; CMGS(I,1)=6.0; CMGL(I,1)=3.0  ! security only
            ICGS(I,2)=0; ICGF(I,2)=24; CMGS(I,2)=1.0; CMGL(I,2)=0.0  ! emergency lights W/m2
            ICGS(I,3)=0; ICGF(I,3)=24; CMGS(I,3)=1.0; CMGL(I,3)=0.0
          endif
        elseif(igu.eq.12)then

C Office meeting room for up to 6 with some diversity. Uses CIBSE Guide A 6.3
C for radiant-convective splits. Saturday, Sunday & holidays unoccupied.
          ctlstr(icomp,I)='no control of air flow  '
          NAC(I)=1; IACS(I,1)=0; IACF(I,1)=24
          ACI(I,1)=0.5; ACV(I,1)=0.0; IPT(I,1)=0; TA(I,1)=0.0
          if(I.eq.1)then
            NCAS(I)=22
            do loop=1,22  ! set common information
              if(loop.ge.1.and.loop.le.9)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.ge.10.and.loop.le.15)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2  ! for lights
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              elseif(loop.ge.16.and.loop.le.22)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -3  ! for equip
                RADC(I,loop)=0.4; CONC(I,loop)=0.6
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=7; CMGS(I,1)=0.0; CMGL(I,1)=0.0  ! occupants bit of diversity
            ICGS(I,2)=7; ICGF(I,2)=8; CMGS(I,2)=50.0; CMGL(I,2)=25.0 ! cleaners
            ICGS(I,3)=8; ICGF(I,3)=10; CMGS(I,3)=500.0; CMGL(I,3)=250.0
            ICGS(I,4)=10; ICGF(I,4)=11; CMGS(I,4)=100.0; CMGL(I,4)=50.0
            ICGS(I,5)=11; ICGF(I,5)=13; CMGS(I,5)=600.0; CMGL(I,5)=300.0
            ICGS(I,6)=13; ICGF(I,6)=14; CMGS(I,6)=0.0; CMGL(I,6)=0.0
            ICGS(I,7)=14; ICGF(I,7)=16; CMGS(I,7)=500.0; CMGL(I,7)=250.0
            ICGS(I,8)=16; ICGF(I,8)=18; CMGS(I,8)=300.0; CMGL(I,8)=150.0
            ICGS(I,9)=18; ICGF(I,9)=24; CMGS(I,9)=0.0; CMGL(I,9)=0.0
              
            ICGS(I,10)=0; ICGF(I,10)=8; CMGS(I,10)=0.0; CMGL(I,10)=0.0 ! lights W/m2
            ICGS(I,11)=8; ICGF(I,11)=10; CMGS(I,11)=7.0; CMGL(I,11)=0.0
            ICGS(I,12)=10; ICGF(I,12)=11; CMGS(I,12)=2.0; CMGL(I,12)=0.0
            ICGS(I,13)=11; ICGF(I,13)=13; CMGS(I,13)=7.0; CMGL(I,13)=0.0
            ICGS(I,14)=13; ICGF(I,14)=18; CMGS(I,14)=6.0; CMGL(I,14)=0.0
            ICGS(I,15)=18; ICGF(I,15)=24; CMGS(I,15)=1.0; CMGL(I,15)=0.0

            ICGS(I,16)=0; ICGF(I,16)=8; CMGS(I,16)=1.0; CMGL(I,16)=0.0   ! equipment W/m2
            ICGS(I,17)=8; ICGF(I,17)=10; CMGS(I,17)=11.7; CMGL(I,17)=0.0 ! meeting
            ICGS(I,18)=10; ICGF(I,18)=11; CMGS(I,18)=0; CMGL(I,18)=0.0   ! no projector
            ICGS(I,19)=11; ICGF(I,19)=12; CMGS(I,19)=11.7; CMGL(I,19)=0.0   ! no projector
            ICGS(I,20)=12; ICGF(I,20)=15; CMGS(I,20)=0; CMGL(I,20)=0.0   ! no projector
            ICGS(I,21)=15; ICGF(I,21)=17; CMGS(I,21)=11.7; CMGL(I,21)=0.0   ! projector
            ICGS(I,22)=17; ICGF(I,22)=24; CMGS(I,22)=3.0; CMGL(I,22)=0.0 ! sbem equipment W/m2
            
         else
            NCAS(I)=3  ! Saturday Sunday holiday & other minimal
            do loop=1,3
              if(loop.eq.1)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.eq.2)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              elseif(loop.eq.3)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -3
                RADC(I,loop)=0.4; CONC(I,loop)=0.6
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=24; CMGS(I,1)=0.0; CMGL(I,1)=0.0  ! occupants only in morning
            ICGS(I,2)=0; ICGF(I,2)=24; CMGS(I,2)=1.0; CMGL(I,2)=0.0  ! emergency lights W/m2
            ICGS(I,3)=0; ICGF(I,3)=24; CMGS(I,3)=0.0; CMGL(I,3)=0.0
          endif
        elseif(igu.eq.13)then

C Setup an residential dining room - two people for breakfast
C and 4 in the evening with diversity for different day types.
C No equipment gains in this room. Lights at 8W/m2.
          ctlstr(icomp,I)='no control of air flow  '
          NAC(I)=1; IACS(I,1)=0; IACF(I,1)=24
          ACI(I,1)=0.5; ACV(I,1)=0.0; IPT(I,1)=0; TA(I,1)=0.0
          if(I.eq.1)then
            NCAS(I)=10
            do loop=1,10  ! set common information
              if(loop.ge.1.and.loop.le.5)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.ge.6.and.loop.le.10)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2  ! for lights
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=7; CMGS(I,1)=0.0; CMGL(I,1)=0.0
            ICGS(I,2)=7; ICGF(I,2)=8; CMGS(I,2)=180.0; CMGL(I,2)=150.0  ! breakfast
            ICGS(I,3)=8; ICGF(I,3)=18; CMGS(I,3)=0.0; CMGL(I,3)=0.0
            ICGS(I,4)=18; ICGF(I,4)=19; CMGS(I,4)=280.0; CMGL(I,4)=200.0
            ICGS(I,5)=19; ICGF(I,5)=24; CMGS(I,5)=0.0; CMGL(I,5)=0.0
              
            ICGS(I,6)=0; ICGF(I,6)=7; CMGS(I,6)=0.0; CMGL(I,6)=0.0 ! lights W/m2
            ICGS(I,7)=7; ICGF(I,7)=8; CMGS(I,7)=8.0; CMGL(I,7)=0.0
            ICGS(I,8)=8; ICGF(I,8)=18; CMGS(I,8)=0.0; CMGL(I,8)=0.0
            ICGS(I,9)=18; ICGF(I,9)=20; CMGS(I,9)=8.0; CMGL(I,9)=0.0
            ICGS(I,10)=20; ICGF(I,10)=24; CMGS(I,10)=1.0; CMGL(I,10)=0.0
            
          elseif(I.eq.2)then  ! later on Saturday and 3 for breakfast
            NCAS(I)=10
            do loop=1,10  ! set common information
              if(loop.ge.1.and.loop.le.5)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.ge.6.and.loop.le.10)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2  ! for lights
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=8; CMGS(I,1)=0.0; CMGL(I,1)=0.0
            ICGS(I,2)=8; ICGF(I,2)=9; CMGS(I,2)=230.0; CMGL(I,2)=250.0  ! breakfast
            ICGS(I,3)=9; ICGF(I,3)=19; CMGS(I,3)=0.0; CMGL(I,3)=0.0
            ICGS(I,4)=19; ICGF(I,4)=20; CMGS(I,4)=280.0; CMGL(I,4)=300.0
            ICGS(I,5)=20; ICGF(I,5)=24; CMGS(I,5)=0.0; CMGL(I,5)=0.0
              
            ICGS(I,6)=0; ICGF(I,6)=8; CMGS(I,6)=0.0; CMGL(I,6)=0.0 ! lights W/m2
            ICGS(I,7)=8; ICGF(I,7)=10; CMGS(I,7)=8.0; CMGL(I,7)=0.0
            ICGS(I,8)=10; ICGF(I,8)=18; CMGS(I,8)=0.0; CMGL(I,8)=0.0
            ICGS(I,9)=18; ICGF(I,9)=20; CMGS(I,9)=8.0; CMGL(I,9)=0.0
            ICGS(I,10)=20; ICGF(I,10)=24; CMGS(I,10)=1.0; CMGL(I,10)=0.0

          elseif(I.eq.3)then  ! everyone on Sunday and later
            NCAS(I)=10
            do loop=1,10  ! set common information
              if(loop.ge.1.and.loop.le.5)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.ge.6.and.loop.le.10)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2  ! for lights
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=8; CMGS(I,1)=0.0; CMGL(I,1)=0.0
            ICGS(I,2)=8; ICGF(I,2)=10; CMGS(I,2)=270.0; CMGL(I,2)=250.0  ! breakfast
            ICGS(I,3)=10; ICGF(I,3)=19; CMGS(I,3)=0.0; CMGL(I,3)=0.0
            ICGS(I,4)=19; ICGF(I,4)=21; CMGS(I,4)=300.0; CMGL(I,4)=280.0
            ICGS(I,5)=21; ICGF(I,5)=24; CMGS(I,5)=0.0; CMGL(I,5)=0.0
              
            ICGS(I,6)=0; ICGF(I,6)=8; CMGS(I,6)=0.0; CMGL(I,6)=0.0 ! lights W/m2
            ICGS(I,7)=8; ICGF(I,7)=10; CMGS(I,7)=8.0; CMGL(I,7)=0.0
            ICGS(I,8)=10; ICGF(I,8)=19; CMGS(I,8)=0.0; CMGL(I,8)=0.0
            ICGS(I,9)=19; ICGF(I,9)=21; CMGS(I,9)=8.0; CMGL(I,9)=0.0
            ICGS(I,10)=21; ICGF(I,10)=24; CMGS(I,10)=1.0; CMGL(I,10)=0.0

          else
            NCAS(I)=2  ! holiday & other minimal
            do loop=1,2
              if(loop.eq.1)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.eq.2)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=24; CMGS(I,1)=0.0; CMGL(I,1)=0.0  ! occupants only in morning
            ICGS(I,2)=0; ICGF(I,2)=24; CMGS(I,2)=1.0; CMGL(I,2)=0.0  ! lights W/m2
          endif
        elseif(igu.eq.14)then

C Setup an residential lounge - used primarily in the evning with
C diversity of occupancy and equipment use. Lights at 8W/m2.
          ctlstr(icomp,I)='no control of air flow  '
          NAC(I)=1; IACS(I,1)=0; IACF(I,1)=24
          ACI(I,1)=0.5; ACV(I,1)=0.0; IPT(I,1)=0; TA(I,1)=0.0
          if(I.eq.1)then
            NCAS(I)=14
            do loop=1,14  ! set common information
              if(loop.ge.1.and.loop.le.5)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.ge.6.and.loop.le.10)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2  ! for lights
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              elseif(loop.ge.11.and.loop.le.14)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -3  ! for equip
                RADC(I,loop)=0.4; CONC(I,loop)=0.6
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=6; CMGS(I,1)=0.0; CMGL(I,1)=0.0
            ICGS(I,2)=6; ICGF(I,2)=8; CMGS(I,2)=40.0; CMGL(I,2)=40.0  ! breakfast
            ICGS(I,3)=8; ICGF(I,3)=17; CMGS(I,3)=70.0; CMGL(I,3)=40.0 ! occassional use
            ICGS(I,4)=17; ICGF(I,4)=21; CMGS(I,4)=360.0; CMGL(I,4)=200.0 ! evening
            ICGS(I,5)=21; ICGF(I,5)=24; CMGS(I,5)=240.0; CMGL(I,5)=130.0
              
            ICGS(I,6)=0; ICGF(I,6)=7; CMGS(I,6)=1.0; CMGL(I,6)=0.0 ! lights W/m2
            ICGS(I,7)=7; ICGF(I,7)=9; CMGS(I,7)=8.0; CMGL(I,7)=0.0
            ICGS(I,8)=9; ICGF(I,8)=16; CMGS(I,8)=3.0; CMGL(I,8)=0.0
            ICGS(I,9)=16; ICGF(I,9)=21; CMGS(I,9)=8.0; CMGL(I,9)=0.0
            ICGS(I,10)=21; ICGF(I,10)=24; CMGS(I,10)=6.0; CMGL(I,10)=0.0

            ICGS(I,11)=0; ICGF(I,11)=6; CMGS(I,11)=2.0; CMGL(I,11)=0.0   ! equipment W/m2
            ICGS(I,12)=6; ICGF(I,12)=9; CMGS(I,12)=4.0; CMGL(I,12)=0.0 ! radio etc
            ICGS(I,13)=9; ICGF(I,13)=18; CMGS(I,13)=2.0; CMGL(I,13)=0.0 ! 
            ICGS(I,14)=18; ICGF(I,14)=24; CMGS(I,14)=8.0; CMGL(I,14)=0.0 ! TV etc
            
          elseif(I.eq.2)then  ! alt pattern on Saturday
            NCAS(I)=14
            do loop=1,14  ! set common information
              if(loop.ge.1.and.loop.le.5)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.ge.6.and.loop.le.10)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2  ! for lights
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              elseif(loop.ge.11.and.loop.le.14)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -3  ! for equip
                RADC(I,loop)=0.4; CONC(I,loop)=0.6
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=8; CMGS(I,1)=0.0; CMGL(I,1)=0.0
            ICGS(I,2)=8; ICGF(I,2)=9; CMGS(I,2)=140.0; CMGL(I,2)=100.0  ! breakfast
            ICGS(I,3)=9; ICGF(I,3)=18; CMGS(I,3)=70.0; CMGL(I,3)=40.0 ! occassional use
            ICGS(I,4)=18; ICGF(I,4)=21; CMGS(I,4)=300.0; CMGL(I,4)=200.0 ! evening
            ICGS(I,5)=21; ICGF(I,5)=24; CMGS(I,5)=200.0; CMGL(I,5)=130.0
              
            ICGS(I,6)=0; ICGF(I,6)=8; CMGS(I,6)=1.0; CMGL(I,6)=0.0 ! lights W/m2
            ICGS(I,7)=8; ICGF(I,7)=10; CMGS(I,7)=8.0; CMGL(I,7)=0.0
            ICGS(I,8)=10; ICGF(I,8)=17; CMGS(I,8)=3.0; CMGL(I,8)=0.0
            ICGS(I,9)=17; ICGF(I,9)=21; CMGS(I,9)=8.0; CMGL(I,9)=0.0
            ICGS(I,10)=21; ICGF(I,10)=24; CMGS(I,10)=6.0; CMGL(I,10)=0.0

            ICGS(I,11)=0; ICGF(I,11)=6; CMGS(I,11)=2.0; CMGL(I,11)=0.0   ! equipment W/m2
            ICGS(I,12)=6; ICGF(I,12)=9; CMGS(I,12)=4.0; CMGL(I,12)=0.0 ! radio etc
            ICGS(I,13)=9; ICGF(I,13)=18; CMGS(I,13)=2.0; CMGL(I,13)=0.0 ! 
            ICGS(I,14)=18; ICGF(I,14)=24; CMGS(I,14)=8.0; CMGL(I,14)=0.0 ! TV etc

          elseif(I.eq.3)then  ! everyone on Sunday and later
            NCAS(I)=14
            do loop=1,14  ! set common information
              if(loop.ge.1.and.loop.le.5)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.ge.6.and.loop.le.10)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2  ! for lights
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              elseif(loop.ge.11.and.loop.le.14)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -3  ! for equip
                RADC(I,loop)=0.4; CONC(I,loop)=0.6
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=9; CMGS(I,1)=0.0; CMGL(I,1)=0.0
            ICGS(I,2)=9; ICGF(I,2)=10; CMGS(I,2)=180.0; CMGL(I,2)=100.0  ! breakfast
            ICGS(I,3)=10; ICGF(I,3)=18; CMGS(I,3)=80.0; CMGL(I,3)=40.0 ! occassional use
            ICGS(I,4)=18; ICGF(I,4)=21; CMGS(I,4)=200.0; CMGL(I,4)=100.0 ! evening
            ICGS(I,5)=21; ICGF(I,5)=24; CMGS(I,5)=100.0; CMGL(I,5)=50.0
              
            ICGS(I,6)=0; ICGF(I,6)=8; CMGS(I,6)=1.0; CMGL(I,6)=0.0 ! lights W/m2
            ICGS(I,7)=8; ICGF(I,7)=10; CMGS(I,7)=7.0; CMGL(I,7)=0.0
            ICGS(I,8)=10; ICGF(I,8)=18; CMGS(I,8)=3.0; CMGL(I,8)=0.0
            ICGS(I,9)=18; ICGF(I,9)=21; CMGS(I,9)=8.0; CMGL(I,9)=0.0
            ICGS(I,10)=21; ICGF(I,10)=24; CMGS(I,10)=6.0; CMGL(I,10)=0.0

            ICGS(I,11)=0; ICGF(I,11)=6; CMGS(I,11)=2.0; CMGL(I,11)=0.0   ! equipment W/m2
            ICGS(I,12)=6; ICGF(I,12)=9; CMGS(I,12)=5.0; CMGL(I,12)=0.0 ! radio etc
            ICGS(I,13)=9; ICGF(I,13)=18; CMGS(I,13)=3.0; CMGL(I,13)=0.0 ! 
            ICGS(I,14)=18; ICGF(I,14)=24; CMGS(I,14)=7.0; CMGL(I,14)=0.0 ! TV etc

          else
            NCAS(I)=2  ! holiday & other minimal
            do loop=1,2
              if(loop.eq.1)then
                ICGUnit(I,loop)=0; ICGT(I,loop)=1  ! ocup Watts
                RADC(I,loop)=0.6; CONC(I,loop)=0.4
              elseif(loop.eq.2)then
                ICGUnit(I,loop)=1; ICGT(I,loop)= -2
                RADC(I,loop)=0.3; CONC(I,loop)=0.7
              endif
            enddo
            ICGS(I,1)=0; ICGF(I,1)=24; CMGS(I,1)=0.0; CMGL(I,1)=0.0  ! occupants only in morning
            ICGS(I,2)=0; ICGF(I,2)=24; CMGS(I,2)=1.0; CMGL(I,2)=0.0  ! lights W/m2
          endif
        endif
 399  CONTINUE

C Instanciate initial documentation for air schedules and casual gains.
      if(igu.eq.8)then
        write(oprdesc(icomp),'(2a)')
     &  'Nothing happens in this zone i.e. no occupants lights',
     &  ' and small power. Initial period of 0-24 hour for each.'
        write(ventdesc(icomp),'(2a)')
     &  'It has no infiltration or ventilation from other ',
     &  'zones and no control imposed on air movement.'
      elseif(igu.eq.9)then
        write(oprdesc(icomp),'(2a)')
     &   'Cellular office some diversity single occupant lights ',
     &   '7W/m2 and small power 6W/m2. Reduced hours weekends.'
        write(ventdesc(icomp),'(2a)')
     &    'Assumes 0.5 ach infiltration. No ventilation from ',
     &    'other zones and no control imposed on air movement.'
      elseif(igu.eq.10)then
        write(oprdesc(icomp),'(2a)')
     &   'Open plan office some diversity for occupant lights ',
     &   '7W/m2 and small power 6W/m2. Reduced hours weekends.'
        write(ventdesc(icomp),'(2a)')
     &    'Assumes 0.5 ach infiltration. No ventilation from ',
     &    'other zones and no control imposed on air movement.'
      elseif(igu.eq.11)then
        write(oprdesc(icomp),'(2a)')
     &   'Office corridor diversity 0-3 people, lights emergency & ',
     &   '7W/m2 and brief small power 2W/m2. Reduced hours weekends.'
        write(ventdesc(icomp),'(2a)')
     &    'Assumes 0.5 ach infiltration. No ventilation from ',
     &    'other zones and no control imposed on air movement.'
      elseif(igu.eq.12)then
        write(oprdesc(icomp),'(2a)')
     &   'Meeting room max 6 with diversity, lights emergency & ',
     &   '7W/m2 with occassional projector use. Weekdays only.'
        write(ventdesc(icomp),'(2a)')
     &    'Assumes 0.5 ach infiltration. No ventilation from ',
     &    'other zones and no control imposed on air movement.'
      elseif(igu.eq.13)then
        write(oprdesc(icomp),'(2a)')
     &   'Dining room (max 4) with diversity used for breakfast',
     &   'and dinner & with occassional lighting. Extra latent.'
        write(ventdesc(icomp),'(2a)')
     &    'Assumes 0.5 ach infiltration. No ventilation from ',
     &    'other zones and no control imposed on air movement.'
      elseif(igu.eq.14)then
        write(oprdesc(icomp),'(2a)')
     &   'Lounge (max 3) with diversity used mostly in evening',
     &   'with occassional lighting radio TV etc.'
        write(ventdesc(icomp),'(2a)')
     &    'Assumes 0.5 ach infiltration. No ventilation from ',
     &    'other zones and no control imposed on air movement.'
      endif

      return
      end

C ********** pregist
C Register a new project. Parameter act = 'i ' then initial pass,
C if 'e ' then provide editing menu, if 'sn' use passed parameters
C for the root and path to create a new model, if 'sc' silent continue,
C if 'sw' then pased root and model is assumed to be created silently 
C within the current folder.
C root (32 char) root name (for 'sn' case)
C mpath (72 char) path to model folder (for 'sn' case)
C menu (72 char) brief title of model
      subroutine pregist(act,root,mpath,menu,ier)
#include "building.h"
#include "model.h"
#include "site.h"
#include "esprdbfile.h"
#include "espriou.h"
C espriou.h provides currentfile and MFFOLD.
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      common/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/C6/INDCFG
      common/PREC8/SLAT,SLON
      common/rpath/path
      common/rcmd/LCMDFL

C Plant network.
      COMMON/C23/IFPNF,LPNF

C Images.
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton

C External text editor.
      common/texted/tedlbl,teditor

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

C Calendar.
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

C Passed in parameters.
      character root*32,mpath*72,menu*72

      logical XST,OK,dok,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical OKI,concat,unixok,clkok

      character dstmp*24,LCFTMP*32
      character*72 LTMP
      character path*72,LCMDFL*144,LPNF*72
      character odir*84,pwd*84
      character doit*248,longtfile*144,longtfiledos*144
      character tedlbl*20,teditor*20
      character uname*24,tmode*8
      character limg*72,act*2
      character iformat*4,ifocus*4
      character fs*1
      character subpath*72,action*3,outs*124
      character lltmp*144,lguess*144,lldef*144  ! for working with EASKXORGTKF
      character lpath*72,fname*72 ! for use with fdroot

      integer iyeart    ! for local editing
      integer iglib  ! for detecting GTK or X11
      integer lnmp,lncr,lnod ! for with of mpath, cfgroot, odir strings
      integer itrcc   ! for silently rescanning new cfg file
      character tcname*248   ! for editing image notes
      character WORDS(12)*32,testw*32
      character MODE*4

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='pregist'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C If act = 'i' then initial pass, if 'e' then provide editing menu.

C Ask for source file, brief description and log file. Offer
C editing of log file, images, hypertext link and results summary.
      IER=0

  289 if(act(1:2).eq.'sc')then

C Silent continue, there is nothing to do (currently).
        return
      elseif(act(1:2).eq.'sn')then

C Silent new within the pre-registration process.

C Debug.
        write(6,*)  ' pregist ',root
        write(6,*)  ' pregist ',mpath

        write(LCFTMP,'(a)') root(1:lnblnk(root))
      elseif(act(1:2).eq.'sw')then

C Silent within the pre-registration process.

C Debug.
        write(6,*)  ' pregist ',root
        write(6,*)  ' pregist ',mpath

        write(LCFTMP,'(a)') root(1:lnblnk(root))

      elseif(act(1:1).eq.'i')then

        helptopic='cfg_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        LCFTMP='  '
        CALL EASKSCMD(LCFTMP,' ','Model root name?',
     &    'cancel',clkok,32,'new_model','model root name',IER,nbhelp)
        call usrmsg(' ',' ','-')
        if(clkok) return         ! User selected cancel.

C Trap for blank root name.
        IF(LCFTMP(1:2).EQ.'  '.or.LCFTMP(1:4).eq.'UNKN')GOTO 289

C If we got this far in the 'i' mode clear common blocks. Note this
C clear is not done when called in the 'sn' or 'sw' mode because
C the meta file facility will have instanciated some common blocks
C which should not be cleared.
        call clrprb
      endif

C User did not cancel so instantiate data for new model.
C Current default version is v4.
      icfgv=4   ! alter this to 4 when working correctly
      INDCFG=0
      if(act(1:2).eq.'sn'.or.act(1:2).eq.'sw')then
        continue  ! META or gbXML file will already have set sitelat etc.
      else
        sitelat=55.9; SLAT=55.9; 
        sitelongdif=-4.1; SLON=-4.1
        siteexposureindex=1
        groundrefl=0.2
      endif
      if(act(1:2).eq.'sn'.or.act(1:2).eq.'sw')then
        continue  ! META or gbXML file will already have set zones etc.
      else
        NCOMP=0
        noimg=0
        LPNF='UNKNOWN'
        lmodellog='UNKNOWN'
        nbdaytype=0   ! reset to zero day types
      endif
      call st2file(LCFTMP,LCMDFL)  ! remove unprintable characters

C Derive the model root name from this.
C If sting > 4 char it might have a .cfg attached, otherwise add.
      call fdroot(LCMDFL,path,LCFGF)
      lcfgr=lnblnk(LCFGF)
      lcfgl=lcfgr-3
      if(lcfgr.gt.4)then
        if(LCFGF(lcfgl:lcfgr).eq.'.cfg')then
          if(lcfgl-1.le.32)then
            write(cfgroot,'(a)') LCFGF(1:lcfgl-1)
          else
            write(cfgroot,'(a)') LCFGF(1:32)
          endif
        else
          if(lcfgr.le.32)then
            write(cfgroot,'(a)') LCFGF(1:lcfgr)
          else
            write(cfgroot,'(a)') LCFGF(1:32)
          endif
          write(LCFGF,'(a,a)')LCFGF(1:lcfgr),'.cfg'
        endif
      else
        if(lcfgr.le.32)then
          write(cfgroot,'(a)') LCFGF(1:lcfgr)
        else
          write(cfgroot,'(a)') LCFGF(1:32)
        endif
        write(LCFGF,'(a,a)')LCFGF(1:lcfgr),'.cfg'
      endif

C Check existance of this file (should not overwrite an existing file).
      CALL ERPFREE(IFCFG,ISTAT)
      call FINDFIL(LCFGF,XST)
      if(XST)then
        dok=.true.
        helptopic='cfg_overwrite_warning'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(act(1:2).eq.'sn')then
          OK=.true.
        elseif(act(1:2).eq.'sw')then
          OK=.true.
        elseif(act(1:1).eq.'i')then
          call askok('There is an existing configuration with',
     &    'this name. Do you want to overwrite it?',OK,dok,nbhelp)
        endif
        if(.not.OK)goto 289
      endif

C Ask where to put it.
C Get the current folder and display options to the user.
      odir=' '
      call usrdir(odir)
      lnod=lnblnk(odir)
      if(act(1:2).eq.'sw')then

        continue ! silent within the pre-registration process.

      elseif(act(1:2).eq.'sn')then

C Silent new within the pre-registration process.

C Create the folder mpath and inside that the standard model folders.
        lnmp=lnblnk(mpath)
        write(doit,'(2a)') 'mkdir ',mpath(1:lnmp)
        call usrmsg('Creating model folders...','  ','-')
        call runit(doit,'-')
        call pausems(100)
        call usrmsg('  ','  ','-')
        write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'cfg'
        call runit(doit,'-')
        call pausems(100)

C Check that the control folder path is standard.
        call GETTOKENS(ctlpth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'ctl'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'ctl'
          write(ctlpth,'(3a)')'..',fs,'ctl'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
        endif
        call runit(doit,'-')
        call pausems(100)

C Check that the aim2 folder path is standard.
        call GETTOKENS(aimpth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'aim2'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'aim2'
          write(aimpth,'(3a)')'..',fs,'aim2'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
        endif
        call runit(doit,'-')
        call pausems(100)

C Check that the zones folder path is standard or was not yet defined.
        call GETTOKENS(zonepth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'zones'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'zones'
          write(zonepth,'(3a)')'..',fs,'zones'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
        endif
        call runit(doit,'-')
        call pausems(100)

C Check that the nets folder path is standard.
        call GETTOKENS(netpth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'nets'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'nets'
          write(netpth,'(3a)')'..',fs,'nets'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
        endif
        call runit(doit,'-')
        call pausems(100)

C Check that the images folder path is standard.
        call GETTOKENS(imgpth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'images'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'images'
          write(imgpth,'(3a)')'..',fs,'images'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
        endif
        call runit(doit,'-')
        call pausems(100)

C Check that the doc folder path is standard.
        call GETTOKENS(docpth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'doc'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'doc'
          write(docpth,'(3a)')'..',fs,'doc'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
        endif
        call runit(doit,'-')
        call pausems(100)
        write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'rad'
        call runit(doit,'-')
        call pausems(100)

C Check that the hvac folder path is standard.
        call GETTOKENS(hvacpth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'hvac'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'hvac'
          write(hvacpth,'(3a)')'..',fs,'hvac'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
        endif
        call runit(doit,'-')
        call pausems(100)

C Check that the dbs folder path is standard.
        call GETTOKENS(dbspth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'dbs'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'dbs'
          write(dbspth,'(3a)')'..',fs,'dbs'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
        endif
        call runit(doit,'-')
        call pausems(100)

        write(radpth,'(3a)')'..',fs,'rad'

C Check that the bsm (BASESIMP) folder path is standard.
        call GETTOKENS(bsmpth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'bsm'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'bsm'
          write(dbspth,'(3a)')'..',fs,'bsm'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
        endif
        call runit(doit,'-')
        call pausems(100)

C Re-establish pwd and then project folders.
        write(path,'(4a)')mpath(1:lnblnk(mpath)),fs,'cfg',fs
        call edisp(iuout,
     &      'System configuration file is located in folder:')
        call edisp(iuout,path)

C Write out the model log file within the model doc folder.
        uname=' '
        call usrname(uname)
        call usrdir(pwd)
        write(modeltitle,'(a)') menu(1:lnblnk(menu))
        lr=lnblnk(cfgroot)
        if(docpth(1:2).eq.'  '.or.docpth(1:2).eq.'./')then
          write(lmodellog,'(2a)') cfgroot(1:lr),'.log'
        elseif(docpth(1:3).eq.'../')then
          write(lmodellog,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &      cfgroot(1:lr),'.log'
        else
          write(lmodellog,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &      cfgroot(1:lr),'.log'
        endif
C        write(lmodellog,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.log'
        write(currentfile,'(a)') lmodellog(1:lnblnk(lmodellog))
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,lmodellog,3,IER)
        write(iunit,'(2a)')'Project notes for ',LCFGF(1:lnblnk(LCFGF))
        write(iunit,'(2a)')'Title: ',modeltitle(1:lnblnk(modeltitle))
        write(iunit,'(2a)')'In folder: ',pwd(1:lnblnk(pwd))
        write(iunit,'(2a)')'By: ',uname(1:lnblnk(uname))
        call dstamp(dstmp)
        write(iunit,'(2a)')'Date: ',dstmp
        write(iunit,'(a)')'Client: '
        write(iunit,'(a)')'Project reference: '
        write(iunit,'(a)')'_____________________________'
        write(iunit,'(a)')'Notes: '
        write(iunit,'(a)')'  '
        write(iunit,'(a)')'_____________________________'
        write(iunit,'(a)')'  '
        CALL ERPFREE(IUNIT,ISTAT)
        goto 77

      elseif(act(1:1).eq.'i')then

C Assume a standard set of folders to hold the model description.
        IW=2
      endif

C Setup the various paths to distributed folders and then create.
C Check longer paths cause the path variable to have a string buffer overflow.
      odir=' '
      call usrdir(odir)
      lnod=lnblnk(odir)
      lncr=lnblnk(cfgroot)
      if(lnod+lncr+6.lt.72)then
        continue
      else
        write(outs,'(6a)')odir(1:lnod),fs,cfgroot(1:lncr),fs,'cfg',fs
        call usrmsg(
     &    'The derived path (below) is > 72 char give shorter root',
     &    outs,'W')
        goto 289
      endif

C Instanciate standard folder names for use within model configuration file.
      write(zonepth,'(3a)')'..',fs,'zones'
      write(netpth,'(3a)')'..',fs,'nets'
      write(ctlpth,'(3a)')'..',fs,'ctl'
      write(aimpth,'(3a)')'..',fs,'aim2'
      write(imgpth,'(3a)')'..',fs,'images'
      write(radpth,'(3a)')'..',fs,'rad'
      write(docpth,'(3a)')'..',fs,'doc'
      write(dbspth,'(3a)')'..',fs,'dbs'
      write(bsmpth,'(3a)')'..',fs,'bsm'
      write(hvacpth,'(3a)')'..',fs,'hvac'
      lncr=lnblnk(cfgroot)
      write(odir,'(a)')cfgroot(1:lncr)
      lnod=lncr
      write(doit,'(2a)') 'mkdir ',cfgroot(1:lncr)
      call usrmsg('Creating folders...','  ','-')
      call runit(doit,'-')
      call pausems(100)
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'cfg'
      call runit(doit,'-')
      call pausems(100)
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'ctl'
      call runit(doit,'-')
      call pausems(100)
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'aim2'
      call runit(doit,'-')
      call pausems(100)
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'zones'
      call runit(doit,'-')
      call pausems(100)
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'nets'
      call runit(doit,'-')
      call pausems(100)
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'images'
      call runit(doit,'-')
      call pausems(100)
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'doc'
      call runit(doit,'-')
      call pausems(100)
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'rad'
      call runit(doit,'-')
      call pausems(100)
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'dbs'
      call runit(doit,'-')
      call pausems(100)
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'hvac'
      call runit(doit,'-')
      call pausems(100)
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'bsm'
      call runit(doit,'-')
      call pausems(100)
      call usrmsg('Creating folders...done.','  ','P')

C Re-establish pwd and then project folders.  Note longer paths
C can cause the path variable to have a string buffer overflow.
      odir=' '
      call usrdir(odir)
      lnod=lnblnk(odir)
      lncr=lnblnk(cfgroot)
      if(lnod+lncr+6.lt.72)then
        call edisp(iuout,'  ')
        write(path,'(6a)')odir(1:lnod),fs,cfgroot(1:lncr),fs,'cfg',fs
        call edisp(iuout,
     &    'New model configuration file is in folder:')
        call edisp(iuout,path)
      else
        write(outs,'(6a)')odir(1:lnod),fs,cfgroot(1:lncr),fs,'cfg',fs
        call usrmsg(
     &    'The derived path (below) is > 72 char give shorter root',
     &    outs,'W')
        goto 289
      endif

C Having tested the explicit path reset.
      write(path,'(4a)') cfgroot(1:lncr),fs,'cfg',fs
      write(outs,'(2a)') 'Resetting model path to: ',path
      call edisp(iuout,outs)

C General description of the model.
  52  helptopic='cfg_model_descriptor'
      call gethelptext(helpinsub,helptopic,nbhelp)
      modeltitle='  '
      ltmp=modeltitle
      CALL EASKS(ltmp,' ','Model description?',
     &  72,'Base case model','model description',IER,nbehlp)
      if(ltmp(1:2).ne.'  '.or.lnblnk(ltmp).gt.3)then
        modeltitle=ltmp
      else
        call usrmsg('Sorry, a blank line is not acceptable.',
     &    'Enter a meaningful description (<7 characters)','W')
        goto 52
      endif

C Documentation.
  53  helptopic='cfg_model_log_file'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(lmodellog(1:2).eq.'  '.or.lmodellog(1:4).eq.'UNKN')then
        lr=lnblnk(cfgroot)
        if(docpth(1:2).eq.'  '.or.docpth(1:2).eq.'./')then
          write(lmodellog,'(2a)') cfgroot(1:lr),'.log'
        elseif(docpth(1:3).eq.'../')then
          write(lmodellog,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &      cfgroot(1:lr),'.log'
        else
          write(lmodellog,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &      cfgroot(1:lr),'.log'
        endif
      endif

C Detect if working on X11 or GTK. The file name returned (lltmp).
C If X11 lltmp will be the file name and if GTK it will include the
C full path to the file.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      lltmp='  '
      lguess='  '
      lldef='project.log'
      if(iglib.eq.2)then
        call addpath(lmodellog,longtfile,concat)  ! expand path for GTK browser
        write(lguess,'(a)') longtfile(1:lnblnk(longtfile))
      else
        write(lguess,'(a)') lmodellog(1:lnblnk(lmodellog))
      endif
      call edisp(iuout,'  ')  ! echo blank line
      CALL EASKXORGTKF(lguess,
     &  'Project log file:',' ',lldef,lltmp,'log file',IER,nbhelp)

C If user request jump back and re-display the menu.
      if(ier.eq.-3)then
        goto 53  ! cancel detected, restore name and redisplay menu.
      endif

C Ask user whether file is in ../doc and if so use fdroot if GTK.
      if(iglib.eq.2)then
        dok=.true.
        CALL ASKOK('Is this file in the model ../doc folder?',
     &    ' ',OK,dok,3)
        if(OK)then
          call fdroot(lltmp,lpath,fname)
          write(lmodellog,'(3a)') docpth(1:lnblnk(docpth)),fs,
     &      fname(1:lnblnk(fname))
        endif
      else
        write(lmodellog,'(a)') lltmp(1:lnblnk(lltmp))
      endif

C See if file exists, otherwise create it and write basic
C information.
      uname=' '
      call usrname(uname)
      call usrdir(pwd)
      call FINDFIL(lmodellog,XST)
      IUNIT=IFIL+1
      if(XST)then
        CALL LISTAS(IUNIT,lmodellog,IER)
        IF(IER.ne.0)THEN
          dok=.true.
          CALL ASKOK('Problem detected with log file!',
     &               'Try again?',OK,dok,nbhelp)
          IF(OK)GOTO 53
        ENDIF
      else
        write(currentfile,'(a)') lmodellog(1:lnblnk(lmodellog))
        CALL EFOPSEQ(IUNIT,lmodellog,3,IER)
        write(iunit,'(a,a)')'Project notes for ',LCFGF(1:lnblnk(LCFGF))
        write(iunit,'(a,a)')'Title: ',modeltitle(1:lnblnk(modeltitle))
        write(iunit,'(a,a)')'In folder: ',pwd(1:lnblnk(pwd))
        write(iunit,'(a,a)')'By: ',uname(1:lnblnk(uname))
        call dstamp(dstmp)
        write(iunit,'(a,a)')'Date: ',dstmp
        write(iunit,'(a)')'Client: '
        write(iunit,'(a)')'Project reference: '
        write(iunit,'(a)')'_____________________________'
        write(iunit,'(a)')'Notes: '
        write(iunit,'(a)')'  '
        write(iunit,'(a)')'_____________________________'
        write(iunit,'(a)')'  '
        CALL ERPFREE(IUNIT,ISTAT)
      endif

C Allow user to edit the log file. Append to path if
C necessary.  If vi then spawn a new window to do the editing.

C << This would be a good place to offer an internal editing popup
C << which uses GTK facilities.
      dok=.false.
      helptopic='cfg_model_log_file'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ASKOK(' ','Edit log file?',OK,dok,nbhelp)
      IF(OK)then

C Check if Unix-based or DOS based.
        call isunix(unixok)
        if(unixok)then
          call addpath(lmodellog,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(lmodellog,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        tmode='graph'
        if(teditor(1:2).eq.'vi')tmode='text'
        write(doit,'(a,2x,a,a)') teditor(1:lnblnk(teditor)),
     &    longtfile(1:lnblnk(longtfile)),' &'
        call runit(doit,tmode)
      endif

C Ask about images and a results summary.
C << this section removed from ecnv version. >>

      if(act(1:1).eq.'i')then

C Ask for site position.
        helptopic='cfg_latitude_longitude'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(SLAT,' ','Site latitude?',
     &    -89.9,'W',89.9,'W',55.9,'site latitude',IER,nbhelp)
        IF(IER.EQ.0)sitelat=SLAT

        CALL EASKR(SLON,
     &   'Longitude difference from local time meridian (east +ve)?',
     &   ':',-15.0,'W',15.0,'W',-4.1,'longitude difference',IER,nbhelp)
        IF(IER.EQ.0)sitelongdif=SLON

C Ask for year, initialise and then print out the calendar if the
C user does not cancel the operation. Because there have been no
C day types created use a call to calenmanage to instanciate an
C initial set of days.
        helptopic='cfg_model_year'
        call gethelptext(helpinsub,helptopic,nbhelp)
        iyeart=iyear
        CALL EASKI(IYEART,' ','Assessment year?',
     &    1900,'W',2051,'W',2007,'assessment year',IERI,nbhelp)
        if(ieri.eq.-3) then
          call calenmanage('i',ier)
        else
          iyear=iyeart
          call calenmanage('i',ier)

C Comment out printing of initial calendar as it can cause a crash
C on some 64bit computers.
C          loop=8
C          loopst=1
C          call calenprint(iuout,'t',iyear,loopst,loop)
C          call edisp(iuout,' ')
C          call calenprint(iuout,'g',iyear,loopst,loop)
C          call edisp(iuout,' ')
        endif

        helptopic='cfg_model_other'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('new site',nbhelp,'-',0,0,IER)
      endif

C Create a system configuration file based on the registration
C information.
   77 call tstamp('>','PRJ: create configuration')
      call tstamp('>',LCFGF)
      CALL EMKCFG('-',IER)
      if(ier.eq.0)then
        CFGOK=.TRUE.

C Open core of databases if not already done so.
        if(MLDBOK.and.MATDBOK.and.OPTKOK)then
          continue
        else
          call opendb(ier)
          if(ier.ne.0)then
            call usrmsg('Possible problem with the Constructions',
     &               'or Optical Properties db. Please check.','W')
            ier = 0
          endif
        endif

C If initial registration re-read the newly created model cfg
C file so that any explicit file paths for databases can get
C recognised as *std items and then written out in *std format.
        if(act(1:1).eq.'i')then
          itrcc=0
          MODE='ALL '
          IUF=IPRODB   ! assign second file unit to the events db unit
          CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrcc,IER)
          call pausems(100)
          CALL EMKCFG('-',IER)
        endif
        return
      endif
      return
      end

C ********** opendb
C NOTE: if this code change - be sure to update the copy of this
C subroutine embedded within esruish/ish.F and esrueco/ecoesp.F

C Open materials, constructions and optical properties. In the 
C case of materials, first assume it is a binary file, check its
C contents and if a problem then scan new ascii format.
C Uf that does not work try the older ascii materials file to
C fill the materials data arrays.
C If sucessful the material common blocks will be filled and
C closemat1 or closemat2 will be set.

      subroutine opendb(ier)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      logical XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical closemat1,closemat2
      character LASCI*144,DFILE*144,fs*1
      character SOPT*12,outs248*248,GDESCR*36
      character t144*144   ! for use with erprcdb
      character lworking*144 
      integer lndbp   ! for length of standard path
      logical unixok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='opendb'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Scan the binary file data into materials commons and if this was
C sucessful and matver was set to 1.1 in matformbin then we can
C carry on using the materials common blocks for subsequent access.
      call scananymat(ier)
      call eclose(matver,1.1,0.001,closemat1)
      call eclose(matver,1.2,0.001,closemat2)

C Set help text for this subroutine.
      helptopic='many_mat_not_found'
      call gethelptext(helpinsub,helptopic,nbhelp)
        
C Read multilayer db information into common depending on its location.
      CALL ERPFREE(IFMUL,ISTAT)
      if(ipathmul.eq.0.or.ipathmul.eq.1)then
        lworking=lfmul  ! use as is
      elseif(ipathmul.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    lfmul(1:lnblnk(lfmul))  ! prepend db folder path
      endif
      call FINDFIL(lworking,XST)
      if(XST)then
        CALL ERMLDB(0,IUOUT,IER)
        IF(IER.eq.1)then
          write(outs248,'(3a)') 
     &    ' Problems with materials used by Constructions db',
     &     LFMAT(1:lnblnk(LFMAT)),'!'
          call edisp248(iuout,outs248,100)
          call edisp(iuout,' ')
          MLDBOK=.FALSE.
        ELSEIF(IER.eq.2)then

C There were lots of undefined materials so likely the Materials
C binary file is for the wrong machine type. Guess the name of
C the ascii version and if it exists, offer to convert it or use it.
          IAF=IFIL+1
          LASCI=' '
          if(ipathmat.eq.0.or.ipathmat.eq.1)then
            write(LASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
          elseif(ipathmat.eq.2)then
            lndbp=lnblnk(standarddbpath)
            write(LASCI,'(4a)') standarddbpath(1:lndbp),fs,
     &        LFMAT(1:lnblnk(LFMAT)),'.a'
          endif

C Label 155 is a jump back point for the case of users supplying a name.
  155     CALL EFOPSEQ(IAF,LASCI,1,IER)
          if(ier.eq.0)then

C If the ascii version exists there is no need to convert it just
C scan it into common blocks.
            call rascimat(IAF,LASCI,IER)  ! try current ascii format
            if(ier.eq.-2)then
              ier=0
              call rlegacymat(IAF,LASCI,ier)  ! try older ascii format
              if(ier.ne.0)then
                call usrmsg('No readable Materials were found or',
     &                      'or the file was corrupt.','W')
                MATDBOK=.FALSE.
              else
                call eclose(matver,1.1,0.001,closemat1)
                call eclose(matver,1.1,0.001,closemat2)
                if(closemat1.or.closemat2)then

C Materials data in place, set this as the new file name and then
C try and re-scan the Constructions.
                  call usrmsg(
     &              'An ASCII Materials db was found and used.',
     &              'Re-scanning constructions...','P')
                  MATDBOK=.TRUE.
                  write(LFMAT,'(a)') LASCI(1:lnblnk(LASCI))
                  CALL ERPFREE(IFMUL,ISTAT)

C Search again for lworking.
                  call FINDFIL(lworking,XST)
                  if(XST)then
                    CALL ERMLDB(0,IUOUT,IER)
                    if(ier.eq.0)then
                      MLDBOK=.TRUE.
                      call usrmsg(
     &                  'An ASCII Materials db was found and used.',
     &                  'Re-scanning constructions...done.','-')
                    else
                      MLDBOK=.FALSE.
                      call usrmsg(
     &                  'An ASCII Materials db was found and used.',
     &                  'Re-scanning constructions...failed.','W')
                    endif
                  endif
                else
                  call usrmsg(
     &              'No readable Material db was found.',
     &              'Check other warnings for advice.','W')
                  MATDBOK=.FALSE.
                endif
              endif
            elseif(ier.eq.0)then

C Materials data in place, set this as the new file name and then
C try and re-scan the constructions.
              call usrmsg(
     &          'An ASCII Materials db was found and used.',
     &          'Re-scanning constructions...','P')
              MATDBOK=.TRUE.
              write(LFMAT,'(a)') LASCI(1:lnblnk(LASCI))
              CALL ERPFREE(IFMUL,ISTAT)

C Search again for lworking.
              call FINDFIL(lworking,XST)
              if(XST)then
                CALL ERMLDB(0,IUOUT,IER)
                if(ier.eq.0)then
                  MLDBOK=.TRUE.
                  call usrmsg(
     &              'An ASCII Materials db was found and used.',
     &              'Re-scanning constructions...done.','-')
                else
                  MLDBOK=.FALSE.
                  call usrmsg(
     &              'An ASCII Materials db was found and used.',
     &              'Re-scanning constructions...failed.','W')
                endif
              endif
            endif
          else

C Ask user for ascii Materials file to convert.
            IAF=IFIL+1
            DFILE=' '
            CALL EASKS(LASCI,'Materials file (ASCII)','Confirm:',
     &        144,DFILE,'materials db (ascii)',IER,nbhelp)
            goto 155
          endif
        elseif(IER.eq.3)then
          write(outs248,'(3a)') 
     &    ' Not enough constructions found in Constructions db',
     &     lworking(1:lnblnk(lworking)),'!'
          call edisp248(iuout,outs248,100)
          call edisp(iuout,' ')
          MLDBOK=.FALSE.
        ELSEIF(IER.eq.4)THEN
          CALL ERMLDB2(0,iuout,IER)
          if(IER.eq.0)then
            MLDBOK=.TRUE.
          endif
         else

C Scan was ok so set mldbok to true.
          MLDBOK=.TRUE.
        endif
      else

C Could not find Constructions db at this time, report to user.
        write(outs248,'(3a)') 'Constructions db ',
     &     LFMUL(1:lnblnk(LFMUL)),' not found!'
        call edisp248(iuout,outs248,100)
        call edisp(iuout,' ')
        MLDBOK=.FALSE.
      endif

C Open Ooptical Properties db and read into common.
      SOPT='ALL'
      CALL EROPTDB(0,iuout,SOPT,GDESCR,IER)
      if(ier.ne.0)then
        call usrmsg('Optical Properties db not found or',
     &              'there was a problem reading it!','W')
        OPTKOK=.FALSE.
      else
        OPTKOK=.TRUE.
      endif

C << Commented out for ecnv version >>
C Open the current Pressure Coefficients db (signal by passing
C a blank string to erprcdb).
C      t144='  '
C      CALL ERPRCDB(t144,0,3,IER)
C      if(ier.ne.0)then
C        call usrmsg('Pressure Coefficients db not found',
C     &              'or there was a problem reading it!','W')
C      endif

      return
      end

C POINTTOLINE: determines distance from a 3D point to a 3D line.
C where ipoint is the index of the test vertex, iwhich1 is the index
C of the vertex at the start of the line, iwhich2 is the index of the
C index at the end of the line, offset is the distance (m), match is
C a logical set to true if close enough.
C Only returns match=true if point was found along the line between
C the two vertices (i.e. it discards matches beyond the end points.
C It assumes that calling code will decide whether the distance
C can be used. 
      subroutine pointtoline(ipoint,iwhich1,iwhich2,offset,match)
#include "building.h"
#include "geometry.h"
      dimension vd(3),vd1(3),vd2(3)
      logical match

C If any of the indices is zero then return with match=false.
      match=.false.
      iwhich3=ipoint
      if(iwhich1.eq.0.or.iwhich2.eq.0.or.iwhich3.eq.0)then
        match=.false.
        return
      endif

C Report length of line. Use method of Ward/Radiance in fvect.c
      vd(1)= X(IWHICH2)-X(IWHICH1)
      vd(2)= Y(IWHICH2)-Y(IWHICH1)
      vd(3)= Z(IWHICH2)-Z(IWHICH1)
      call dot3(vd,vd,vdis)
      vd1(1)= X(IWHICH3)-X(IWHICH1)
      vd1(2)= Y(IWHICH3)-Y(IWHICH1)
      vd1(3)= Z(IWHICH3)-Z(IWHICH1)
      call dot3(vd1,vd1,vdis1)
      vd2(1)= X(IWHICH3)-X(IWHICH2)
      vd2(2)= Y(IWHICH3)-Y(IWHICH2)
      vd2(3)= Z(IWHICH3)-Z(IWHICH2)
      call dot3(vd2,vd2,vdis2)
      if(vdis2.gt.vdis1)then
        if((vdis2 - vdis1).gt.vdis)then
          match=.false.
          offset=0.0
          return
        endif
      else
        if((vdis1 - vdis2).gt.vdis)then
          match=.false.
          offset=0.0
          return
        endif
      endif
      if(vdis.ne.0.0)then

C The original C code returned the square of distance
C so unpack via sqrt call.
        d2l=(vdis1-(vdis+vdis1-vdis2)*
     &      (vdis+vdis1-vdis2)/vdis/4.0)
        if(abs(d2l).lt.0.0003)then
          offset=d2l   ! if really small d2l
        else
          offset=SQRT(d2l)
        endif
        match=.true.
      else
        offset=0.0   ! vdis was zero so assume a match
        match=.true.
      endif
      return
      end


C ********** calenmanage **************
C Calenmanage sets up and manages a calendar for ESP-r. Currently
C up to 15 day types (MDTY) can be defined and one may be assigned to
C each of the 365 days of the year.
C calename (char*32) is the overall name of this calendar (e.g. `UK standard`)
C calentag (char*12) is a tag for each day type (e.g. `autumn_wkd`)
C calendayname (char*32) menu phrase for each day type (e.g.
C   `autumn weekdays`
C nbdaytype (int) is the number of day types
C nbcaldays (int) is the number of days associated with each type
C icalendar (int*365) for each day, the associated day type
C act (char*1) action to take 'i' initialise, '-' interactive

C Note that there is no specific logic to remind a user to update the
C model cfg file if a change is made.
      subroutine calenmanage(act,ier)
      implicit none
#include "epara.h"
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "espriou.h"  
C espriou.h for climatelist file name.
C #include "control.h"
#include "schedule.h"

#include "seasons.h"
C seasons.h provides typper and typsea

      integer lnblnk  ! function definition

C Parameters passed
      character act*1 ! action to take
      integer ier     ! error state

      integer iuout,iuin
      common/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      integer ifil
      integer iyear,ibdoy,iedoy,ifday,iftime
      common/set1/iyear,ibdoy,iedoy,ifday,iftime
      common/user/browse
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      character calename*32,calentag*12,calendayname*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      integer nbdaytype,nbcaldays,icalender
      common/cctlnm/ctldoc,lctlf
      character ctldoc*248,lctlf*72
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

C Local variables.
      integer MVERT,IVERT,mcvert,icvert ! max items and current menu item
      integer ib,ld,l,idayloop,loop ! index for looping
      integer ijd,idayn,idwk,idt,idtt,imthn,ixd   ! for counting days
      integer idno,istjandwk,ISDS,ISDF   ! for counting days
      integer ifoc,ifrq,ihdt,m,mj,ij,mm,loopst,ix,io,isw,ipact,irt ! for position
      integer ictlf  ! for control domain and file unit
      integer ieri,istat   ! for error states
      integer iuf,iuo      ! additional file units
      integer itrc,itru    ! feedback verbosity
      integer icomp
      integer ipatday1,ipatday2,ipatday3,ipatday4,ipatday5,ipatday6  ! remember what the new day types are
      integer idol,idaynum,idtyy,idwknum,imthnum  ! for determining day of week
      logical ok,dok,xst

C Variables for handling climate file.
      character llclmdb*144
      integer lndbp
      logical unixok
      character fs*1

C m1slots and m2slots are for the two possible months to be
C displayed. 42 is 7 days over maximum of 6 periods e.g.
C                   2000
C         Jan                     Feb             
C Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su  
C                 1  2        1  2  3  4  5  6         
C  3  4  5  6  7  8  9     7  8  9 10 11 12 13     
C 10 11 12 13 14 15 16    14 15 16 17 18 19 20    
C 17 18 19 20 21 22 23    21 22 23 24 25 26 27    
C 24 25 26 27 28 29 30    28 29                   
C 31                    
      DIMENSION VERT(35),citem(21),IDVALS(10),clist(365)
      dimension ixdvals(365)
      integer IDVALS,ixdvals

      character vert*33,DS*7,DS1*10,key*1
      character outs*124,head*18,tcalename*32,citem*52,clist*52
      character tcalentag*12,tcalendayn*32
      logical modify,browse
      logical havectl ! to remember if control file exists
      logical usecalendar  ! signal so easier to detect if other domains
                           ! have a different number of day types

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='calenmanage'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      havectl=.false.          ! assume no control
      usecalendar=.false. 
      ICTLF=IFIL+1

C If user requested initial calendar. Setup initial 4 day types (wk sat sun holiday).
      if(act.eq.'i'.and.(.NOT.browse))then
        if(nbdaytype.le.3)then
          calename='standard weekday Sat Sun hol'
          nbdaytype=4
          nbcaldays(1)=0; calentag(1)='weekdays'
          calendayname(1)='weekdays (all year)'
          nbcaldays(2)=0; calentag(2)='saturday'
          calendayname(2)='Saturdays (all year)'
          nbcaldays(3)=0; calentag(3)='sunday'
          calendayname(3)='Sundays (all year)'
          calentag(4)='holiday'
          nbcaldays(4)=0; calendayname(4)='holiday'
          nbcaldays(5)=0; calentag(5)='-'; calendayname(5)='-'
          nbcaldays(6)=0; calentag(6)='-'; calendayname(6)='-'
          nbcaldays(7)=0; calentag(7)='-'; calendayname(7)='-'
          nbcaldays(8)=0; calentag(8)='-'; calendayname(8)='-'
          nbcaldays(9)=0; calentag(9)='-'; calendayname(9)='-'
          nbcaldays(10)=0; calentag(10)='-'; calendayname(10)='-'
          do 42 ijd=1,365

C Assume 1 Jan is a holiday (users can change this later).
            if(ijd.eq.1)then
              icalender(ijd)=4
              nbcaldays(4)=nbcaldays(4)+1
            else

C For julian day ijd find month and day of month and day of week.
              call edayr(ijd,idayn,imthn)
              call eweekd(idayn,imthn,iyear,idwk)
              if(idwk.ge.1.and.idwk.le.5)then
                icalender(ijd)=1
                nbcaldays(1)=nbcaldays(1)+1
              elseif(idwk.eq.6)then
                icalender(ijd)=2
                nbcaldays(2)=nbcaldays(2)+1
              elseif(idwk.eq.7)then
                icalender(ijd)=3
                nbcaldays(3)=nbcaldays(3)+1
              endif
            endif
  42      continue
          return
        else
          call usrmsg('day types exist, not initialised.',' ','W')
        endif
      endif

      MHEAD=4
      MCTL=5
      ILEN=365
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      modify=.false.

C Initial menu entry setup.
   92 IER=0
      ILEN=365
      IVERT=-3

C Set usecalendar.
      if(nbdaytype.ge.3) usecalendar=.true. 

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      write(VERT(1),'(2a)')     '1 calendar: ',calename(1:20)
      write(VERT(2),'(a,i2,a)') '2 manage day types (',nbdaytype,')'
      VERT(3)                  =' __________________________'
      VERT(4)                  ='  date         day type    '
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          call stdate(iyear,l,DS,DS1)
          WRITE(VERT(M),'(a1,1x,3a)') KEY,DS1,'  ',
     &      calentag(icalender(L))
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        VERT(M+1)='  ______________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      VERT(M+2)  ='+ apply day type to many days    '
      VERT(M+3)  ='! list the calendar              '
      VERT(M+4)  ='? help                           '
      VERT(M+5)  ='- exit                           '

C Help text for this menu.
      helptopic='calendar_day_types'
      call gethelptext(helpinsub,helptopic,nbhelp)

      write(head,'(a,i4)') ' Calendar for ',iyear
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.eq.1)THEN

C Edit calendar name
        tcalename=calename
        CALL EASKS(tcalename,' Description of calendar: ',
     &    '  ',32,'weekday sat sun all year','calen descr',
     &    IER,nbhelp)
        if(tcalename(1:2).ne.'  ')then
          if(tcalename(1:lnblnk(tcalename)).ne.
     &       calename(1:lnblnk(calename)))then
            calename=tcalename
            modify=.true.
          endif
        endif
      ELSEIF(IVERT.EQ.2)THEN

C Manage day types.
        CALL EPMENSV
  73    write(citem(1),'(2a)') '  calendar: ',calename(1:32)
        write(citem(2),'(a)')  ' __tag________description______________'
        mj=2
        do 77 ij=1,nbdaytype
          CALL EMKEY(ij,KEY,IER)
          write(citem(ij+mj),'(5a)') KEY,' ',calentag(ij),' ',
     &      calendayname(ij)
  77    continue
        mm=mj+nbdaytype
        write(citem(mm+1),'(a)') ' ____________________________________'
        write(citem(mm+2),'(a)') '+ add a daytype or set of daytypes   '
        write(citem(mm+3),'(a)') '? help                               '
        write(citem(mm+4),'(a)') '- exit                               '
        mcvert=mj+nbdaytype+4
        icvert=-1
        CALL EMENU('  Day types',citem,mcvert,icvert)
        if(icvert.eq.mcvert)then
          CALL EPMENRC
          goto 3
        elseif(icvert.eq.mcvert-1)then

C Help.
          CALL PHELPD('calendar day section',nbhelp,'-',0,0,IER)
          goto 73
        elseif(icvert.eq.mcvert-2)then

C Add a single day type or a set of day types (e.g. seasonal patterns) if
C there are seasonal periods defined for the current weather. 
C Note: the delete a day type is currently not implemented.

C Rescan the `climatelist` file. Check if this climate is
C in the list. If not instanciate season and typical start and end dates.
          if(ihaveseason.gt.0)then
            continue
          else

C Setup string buffer with distribution weather folder name.
            lndbp=lnblnk(standardclmpath)
            if(ipathclim.eq.0.or.ipathclim.eq.1)then
              llclmdb=LCLIM
            elseif(ipathclim.eq.2)then
              write(llclmdb,'(3a)') standardclmpath(1:lndbp),fs,
     &          LCLIM(1:lnblnk(LCLIM))
            endif
            INQUIRE (FILE=cdblfil,EXIST=XST)
            if(XST)then
              IUF=IFIL+2
              call scancdblist(IUF,llclmdb,ok,ier)
              if(ok)then
                continue
              else

C Set default early winter, spring, summer, autumn, late winter periods.
                CALL EDAY(9,1,ia1wins); CALL EDAY(15,1,ia1winf)
                CALL EDAY(6,3,ia1sprs); CALL EDAY(12,3,ia1sprf)
                CALL EDAY(11,7,iasums); CALL EDAY(17,7,iasumf)
                CALL EDAY(2,10,ia2sprs); CALL EDAY(8,10,ia2sprf)
                CALL EDAY(20,11,ia2wins); CALL EDAY(26,11,ia2winf)

C Default season definitions.
                CALL EDAY(1,1,is1wins); CALL EDAY(28,2,is1winf)
                CALL EDAY(1,11,is2wins); CALL EDAY(31,12,is2winf)
                CALL EDAY(1,3,is1sprs); CALL EDAY(30,4,is1sprf)
                CALL EDAY(1,9,is2sprs); CALL EDAY(31,10,is2sprf)
                CALL EDAY(1,5,is1sums); CALL EDAY(31,8,is1sumf)
              endif
            endif
          endif

C Dialog based on whether or not seasons have been found.

          if(nbdaytype.ge.3.and.nbdaytype.le.MDTY)then
            CALL EASKABCD(' Options:',' ','add day type',
     &        'add set of day types','delete (not available)',
     &        'continue',IRT,nbhelp)
            if(irt.eq.1)then

C If there is a control file, scan it and then loop through each of the
C existing control loops to add in what-will-become the new day type.
C havectl is returned as true if there is a control file.
              call  updatectlfornewdaytype(havectl,usecalendar,ier)

C Now increment nbdaytype and get its name and description.
              nbdaytype=nbdaytype+1
              nbcaldays(nbdaytype)=0
              calentag(nbdaytype)='new'
              calendayname(nbdaytype)='new day type'

C Edit calendar day tag and name.
              tcalentag=calentag(nbdaytype)
              CALL EASKS(tcalentag,' Calendar day type tag: ',
     &          '  ',12,'holiday','day tag',IER,nbhelp)
              if(tcalentag(1:2).ne.'  ')then
                if(tcalentag(1:lnblnk(tcalentag)).ne.
     &         calentag(nbdaytype)(1:lnblnk(calentag(nbdaytype))))then
                  calentag(nbdaytype)=tcalentag
                endif
              endif
              tcalendayn=calendayname(nbdaytype)
              CALL EASKS(tcalendayn,' Calendar day type descripton: ',
     &          '  ',32,'holiday','day descr',IER,nbhelp)
              if(tcalendayn(1:2).ne.'  ')then
                if(tcalendayn(1:lnblnk(tcalendayn)).ne.
     & calendayname(nbdaytype)(1:lnblnk(calendayname(nbdaytype))))then
                  calendayname(nbdaytype)=tcalendayn
                endif
              endif

C Update control data for all zones to include this new day type (now
C that its name is known).
              if(havectl)then
                CALL CTLWRT(ICTLF,IER)
              endif

C Deal with operation files. User will be asked if they want to
C overwrite or save to new file names.
              CALL UPDOPR
              modify=.true.
              goto 73

            elseif(irt.eq.2)then

C Display a list of day type patterns.
              idno=1
              isw=0
              call MENUATOL('  ','Daytype patterns',
     &          'a retail open Monday-Saturday closed Sunday',
     &          'b winter trans summer weekdays & weekends',
     &          'c xxx','d xxx','e xxx','f xxx',
     &          ' ',' ',' ',' ',' ',' ',
     &          isw,idno,nbhelp)
              if(isw.eq.1)then

C A retail profile is open Monday thru Saturday with reduced operating
C hours on the 2nd weekend day. Call the 1st day type retail_open and
C the 2nd day  retail_close. Later offer the user an option to reset
C the calendar to match this.
                if(nbdaytype+2.lt.MDTY)then

C For the 1st retail day revise the controls and then the operations.
                  call edisp(iuout,' ')
                  call edisp(iuout,' Processing retail_open day...')
                  call  updatectlfornewdaytype(havectl,usecalendar,ier)

C Now increment nbdaytype and get its name and description. Update
C the control file if it exists and then process zone operation files.
                  nbdaytype=nbdaytype+1
                  nbcaldays(nbdaytype)=0
                  ipatday1 = nbdaytype  ! remember this
                  calentag(nbdaytype)='retail_open'
                  calendayname(nbdaytype)='retail open Mon-Sat'
                  if(havectl)then
                    CALL CTLWRT(ICTLF,IER)
                  endif
                  CALL UPDOPR

C For the 2nd retail day revise the controls and then the operations.
                  call edisp(iuout,' Processing retail_close day...')
                  call  updatectlfornewdaytype(havectl,usecalendar,ier)

C Now increment nbdaytype and get its name and description. Update
C the control file if it exists and then process zone operation files.
                  nbdaytype=nbdaytype+1
                  nbcaldays(nbdaytype)=0
                  ipatday2 = nbdaytype  ! remember this
                  calentag(nbdaytype)='retail_close'
                  calendayname(nbdaytype)='retail closed Sunday'
                  if(havectl)then
                    CALL CTLWRT(ICTLF,IER)
                  endif
                  CALL UPDOPR

C If user agrees apply these two new day types to the current calendar.
C Loop through all days, if one of the original 3 day types revise them
C to use the new Mon-Sat & Sunday types.
                  DOK=.true.
                  CALL ASKOK('Apply these new day types to the current',
     &              'calendar of the model?',OK,DOK,nbhelp)
                  if(OK)then
                    DO 123 IDOL=1,365
                      IF(ICALENDER(IDOL).LE.3)THEN
                        CALL EDAYR(IDOL,IDAYNUM,IMTHNUM)
                        CALL EWEEKD(IDAYNUM,IMTHNUM,IYEAR,IDWKNUM)
                        IF(IDWKNUM.LE.6)THEN
                          IDTYY=ipatday1 ! WEEKDAY
                        ELSEIF(IDWKNUM.EQ.7)THEN
                          IDTYY=ipatday2 ! SUNDAY
                        ENDIF
                        ICALENDER(IDOL)=IDTYY  ! update the model calendar
                      ENDIF
 123                CONTINUE
                  endif

                  modify=.true.
                  goto 73

                else
                  call usrmsg('Not able to add two more day types to',
     &              'the current model calendar.','W')
                  goto 73
                endif
              elseif(isw.eq.2)then

C Create a separate weekday and weekend day type for winter transition
C and summer for the model seasons. The day type names are win_wkday,
C win_wkend, trn_wkday, trn_wkend, sum_wkday, sum_wkend.
                if(nbdaytype+6.lt.MDTY)then

C For the win_wkday revise the controls and then the operations.
                  call edisp(iuout,' ')
                  call edisp(iuout,' Processing win_wkday day...')
                  call  updatectlfornewdaytype(havectl,usecalendar,ier)

C Now increment nbdaytype and get its name and description. Update
C the control file if it exists and then process zone operation files.
                  nbdaytype=nbdaytype+1
                  nbcaldays(nbdaytype)=0
                  ipatday1 = nbdaytype  ! remember this
                  calentag(nbdaytype)='win_wkday'
                  calendayname(nbdaytype)='winter seasons weekdays'
                  if(havectl)then
                    CALL CTLWRT(ICTLF,IER)
                  endif
                  CALL UPDOPR

C For the win_wkend revise the controls and then the operations.
                  call edisp(iuout,' Processing win_wkend day...')
                  call  updatectlfornewdaytype(havectl,usecalendar,ier)
                  nbdaytype=nbdaytype+1
                  nbcaldays(nbdaytype)=0
                  ipatday2 = nbdaytype  ! remember this
                  calentag(nbdaytype)='win_wkend'
                  calendayname(nbdaytype)='winter seasons weekends'
                  if(havectl)then
                    CALL CTLWRT(ICTLF,IER)
                  endif
                  CALL UPDOPR

C For the trn_wkday revise the controls and then the operations.
                  call edisp(iuout,' Processing trn_wkday day...')
                  call  updatectlfornewdaytype(havectl,usecalendar,ier)
                  nbdaytype=nbdaytype+1
                  nbcaldays(nbdaytype)=0
                  ipatday3 = nbdaytype  ! remember this
                  calentag(nbdaytype)='trn_wkday'
                  calendayname(nbdaytype)='transition seasons weekdays'
                  if(havectl)then
                    CALL CTLWRT(ICTLF,IER)
                  endif
                  CALL UPDOPR

C For the trn_wkend revise the controls and then the operations.
                  call edisp(iuout,' Processing trn_wkend day...')
                  call  updatectlfornewdaytype(havectl,usecalendar,ier)
                  nbdaytype=nbdaytype+1
                  nbcaldays(nbdaytype)=0
                  ipatday4 = nbdaytype  ! remember this
                  calentag(nbdaytype)='trn_wkend'
                  calendayname(nbdaytype)='transition seasons weekends'
                  if(havectl)then
                    CALL CTLWRT(ICTLF,IER)
                  endif
                  CALL UPDOPR

C For the sum_wkday revise the controls and then the operations.
                  call edisp(iuout,' Processing sum_wkday day...')
                  call  updatectlfornewdaytype(havectl,usecalendar,ier)
                  nbdaytype=nbdaytype+1
                  nbcaldays(nbdaytype)=0
                  ipatday5 = nbdaytype  ! remember this
                  calentag(nbdaytype)='sum_wkday'
                  calendayname(nbdaytype)='summer season weekdays'
                  if(havectl)then
                    CALL CTLWRT(ICTLF,IER)
                  endif
                  CALL UPDOPR

C For the sum_wkend revise the controls and then the operations.
                  call edisp(iuout,' Processing sum_wkend day...')
                  call  updatectlfornewdaytype(havectl,usecalendar,ier)
                  nbdaytype=nbdaytype+1
                  nbcaldays(nbdaytype)=0
                  ipatday6 = nbdaytype  ! remember this
                  calentag(nbdaytype)='sum_wkend'
                  calendayname(nbdaytype)='summer season weekends'
                  if(havectl)then
                    CALL CTLWRT(ICTLF,IER)
                  endif
                  CALL UPDOPR

C If user agrees apply these six new day types to the current calendar.
C Loop through all days, if one of the original 3 day types revise them
C to use the new seasonal day types. The 5 sesaons in the weather data
C are applied so that win1 and win2 both get the winter day types and
C spring and autumn both get the transition day type.
                  DOK=.true.
                  CALL ASKOK('Apply these new day types to the current',
     &              'calendar of the model?',OK,DOK,nbhelp)
                  if(OK)then
C Debug.
C                    write(6,*) nbdaytype,ipatday1,ipatday2,ipatday3,
C     &ipatday4,ipatday5,ipatday6
C                    write(6,*) is1wins,is1winf,is2wins,is2winf,
C     &is1sprs,is1sprf,is2sprs,is2sprf,is1sums,is1sumf
                    DO 124 IDOL=1,365
                      IF(ICALENDER(IDOL).LE.3)THEN
                        CALL EDAYR(IDOL,IDAYNUM,IMTHNUM)
                        CALL EWEEKD(IDAYNUM,IMTHNUM,IYEAR,IDWKNUM)
                        if(IDOL.ge.is1wins.and.IDOL.le.is1winf)then
                          IF(IDWKNUM.LT.6)THEN
                            IDTYY=ipatday1 ! Win weekday
                          ELSEIF(IDWKNUM.EQ.6.or.IDWKNUM.EQ.7)THEN
                            IDTYY=ipatday2 ! Win weekend
                          ENDIF
                        elseif(IDOL.ge.is1sprs.and.IDOL.le.is1sprf)then
                          IF(IDWKNUM.LT.6)THEN
                            IDTYY=ipatday3 ! transition weekday
                          ELSEIF(IDWKNUM.EQ.6.or.IDWKNUM.EQ.7)THEN
                            IDTYY=ipatday4 ! transition weekend
                          ENDIF
                        elseif(IDOL.ge.is1sums.and.IDOL.le.is1sumf)then
                          IF(IDWKNUM.LT.6)THEN
                            IDTYY=ipatday5 ! summer WEEKDAY
                          ELSEIF(IDWKNUM.EQ.6.or.IDWKNUM.EQ.7)THEN
                            IDTYY=ipatday6 ! summer weekend
                          ENDIF
                        elseif(IDOL.ge.is2sprs.and.IDOL.le.is2sprf)then
                          IF(IDWKNUM.LT.6)THEN
                            IDTYY=ipatday3 ! autumn weekday
                          ELSEIF(IDWKNUM.EQ.6.or.IDWKNUM.EQ.7)THEN
                            IDTYY=ipatday4 ! autumn weekend
                          ENDIF
                        elseif(IDOL.ge.is2wins.and.IDOL.le.is2winf)then
                          IF(IDWKNUM.LT.6)THEN
                            IDTYY=ipatday1 ! winter WEEKDAY
                          ELSEIF(IDWKNUM.EQ.6.or.IDWKNUM.EQ.7)THEN
                            IDTYY=ipatday2 ! winter weekend
                          ENDIF
                        endif

C If the julian day did not match then fall back on day of the week.
                        if(IDTYY.eq.0)then
                          IF(IDWKNUM.LT.6)THEN
                            IDTYY=1 ! WEEKDAY
                          ELSEIF(IDWKNUM.EQ.6)THEN
                            IDTYY=2 ! SATURDAY
                          ELSEIF(IDWKNUM.EQ.7)THEN
                            IDTYY=3 ! SUNDAY
                          ENDIF
                        endif
                        ICALENDER(IDOL)=IDTYY  ! update the model calendar
                      ENDIF
 124                CONTINUE
                  endif

                  modify=.true.
                  goto 73

                endif
              else
                goto 73
              endif
            elseif(irt.eq.3)then
              goto 73
            elseif(irt.eq.4)then
              goto 73
            endif
          endif
        elseif(icvert.gt.2.and.icvert.lt.mcvert-3)then

C Edit calendar day tag and name.
          ifoc=icvert-2
          tcalentag=calentag(ifoc)
          CALL EASKS(tcalentag,' Calendar day type tag: ',
     &      '  ',12,'holiday','day tag',IER,nbhelp)
          if(tcalentag(1:2).ne.'  ')then
            if(tcalentag(1:lnblnk(tcalentag)).ne.
     &         calentag(ifoc)(1:lnblnk(calentag(ifoc))))then
              calentag(ifoc)=tcalentag
              modify=.true.
            endif
          endif
          tcalendayn=calendayname(ifoc)
          CALL EASKS(tcalendayn,' Calendar day type descripton: ',
     &      '  ',32,'holiday','day descr',IER,nbhelp)
          if(tcalendayn(1:2).ne.'  ')then
            if(tcalendayn(1:lnblnk(tcalendayn)).ne.
     &         calendayname(ifoc)(1:lnblnk(calendayname(ifoc))))then
              calendayname(ifoc)=tcalendayn
              modify=.true.
            endif
          endif

C Write out the control file with the current calendar names and
C also re-write the zone operation files to reflect the new
C calendar names.
          if(modify)then
            ICTLF=IFIL+1; itrc=0     ! set to silent read
            CALL ERPFREE(ICTLF,ISTAT)
            call FINDFIL(LCTLF,XST)
            if(XST)then
              call edisp(iuout,
     &          ' Updating control to reflect new daytype name...')
              CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
              CALL CTLWRT(ICTLF,IER)
            endif
            call edisp(iuout,
     &      ' Updating zone schedules to reflect new daytype name...')

C For every zone in the model do the following...
            DO 100 ICOMP=1,NCOMP

C Check if operations file exists and read it.
              INQUIRE (FILE=LPROJ(ICOMP),EXIST=XST)
              IF(XST)THEN
                IUO=IFIL+1
                CALL ERPFREE(IUO,ISTAT)
                itru=iuout
                CALL EROPER(ITRC,ITRU,IUO,ICOMP,IER)

C Write zone operations file and update to current format.
                ip3ver(icomp)=21
                CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,IER)
              ENDIF
 100        CONTINUE
            call usrmsg('  ','  ','-')
          endif
          goto 73
        else
          goto 73
        endif
      ELSEIF(IVERT.EQ.MVERT)THEN
        if(modify.and.(.NOT.browse))then

C Update calendar day types
          DO 21 IB=1,NBDAYTYPE
            NBCALDAYS(IB)=0
 21       CONTINUE
          DO 22 IB=1,365
            NBCALDAYS(ICALENDER(IB))=NBCALDAYS(ICALENDER(IB))+1
 22       CONTINUE

C Update the model. 
          CALL EMKCFG('-',IER)
        endif
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Produce help text for the vertex menu.
        CALL PHELPD('calendar section',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C List current calendar.
C Begin by finding out the day of the week of 1 Jan.
        ijd=1
        call edayr(ijd,idayn,imthn)
        call eweekd(idayn,imthn,iyear,istjandwk)
        idno=1
        isw=0
        call MENUATOL(outs,'Display options:',
     &    'a january - march','b april - june',
     &    'c july - september','d october - december ',
     &    'e all year',' ',' ',' ',' ',' ',' ',' ',isw,idno,nbhelp)

C loop is the number of months to display
C loopst is the month number to start with
        if(isw.eq.1)then
          loop=3
          loopst=1
        elseif(isw.eq.2)then
          loop=3
          loopst=4
        elseif(isw.eq.3)then
          loop=3
          loopst=7
        elseif(isw.eq.4)then
          loop=3
          loopst=10
        elseif(isw.eq.5)then
          loop=12
          loopst=1
        endif

C Print the calendar.
        call calenprint(iuout,'t',iyear,loopst,loop)
        call edisp(iuout,' ')
        call calenprint(iuout,'g',iyear,loopst,loop)
        call edisp(iuout,' ')
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Apply day type to several days of the year. First ask for the
C day type and then present a list of all of the days so that the
C user can select one or more.
        CALL EPMENSV
        do 67 ij=1,nbdaytype
          write(citem(ij),'(3a)') calentag(ij),' ',calendayname(ij)
  67    continue
        IX=1
        CALL EPICKS(IX,IDVALS,' ',' Available day types:',
     &    52,nbdaytype,citem,'available day types',IER,nbhelp)
       if(ix.ne.0)then

C << potential place to support seasonal day type allocations >>

          IHDT=0
          CALL EASKABC('How to add day types (see help)?',' ',
     &    'One by one','Follow pattern','cancel',IHDT,nbhelp)
          IF(IHDT.EQ.1)THEN
            idt=idvals(1)
            do 78 ld=1,365
              call stdate(iyear,ld,DS,DS1)
              WRITE(clist(ld),'(3a)') DS1,'  ',
     &          calentag(icalender(ld))
  78        continue
            CALL EPMENSV
            ixd=365
            CALL EPICKS(ixd,ixdvals,' ',' Days in the year:',
     &        52,365,clist,'available days',IER,nbhelp)
            CALL EPMENRC
            if(ixd.ne.0)then
              do 79 idayloop=1,ixd
                ifoc=ixdvals(idayloop)
                call stdate(iyear,ifoc,DS,DS1)
                icalender(ifoc)=idt
                nbcaldays(idt)=nbcaldays(idt) + 1
                write(outs,*) 'Revised day: ',ifoc,' ',DS1,' ',
     &            icalender(ifoc),' ',calendayname(icalender(ifoc))
                call edisp(iuout,outs)
  79          continue
              modify=.true.
            endif
          ELSEIF(IHDT.EQ.2)THEN

C Pattern addition of day types.
            ISDS=1
            ISDF=365
            IFRQ=1
            call eAskPer('Dates during which to add new day type',
     &      isds,isdf,ifday,ier)

C Regenerate help string that is clobbered by call to easkper
            CALL EASKI(IFRQ,
     &      ' Enter number of days to repeat pattern after ',
     &      ' E.g. 7=weekly, 30=monthly, 1=daily ',
     &      1,'F',30,'F',1,'frequency ',IERI,nbhelp)
            DO 532 IDTT=ISDS,ISDF,IFRQ
              ICALENDER(IDTT)=idvals(1)
 532        CONTINUE
          ENDIF
        endif
        CALL EPMENRC
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit day identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        call stdate(iyear,ifoc,DS,DS1)
        write(outs,*) 'For day: ',ifoc,' ',DS1,' ',
     &    icalender(ifoc),' ',calendayname(icalender(ifoc))
        call edisp(iuout,outs)

C Select from current range of day types. Assign icalendar value
C for the focus day to the selected day type and increment nbcaldays.
        CALL EPMENSV
        do 76 ij=1,nbdaytype
          write(citem(ij),'(3a)') calentag(ij),' ',calendayname(ij)
  76    continue
        IX=1
        CALL EPICKS(IX,IDVALS,' ',' Available day types:',
     &    52,nbdaytype,citem,'avail day types',IER,nbhelp)
        if(ix.ne.0)then
          idt=idvals(1)
          icalender(ifoc)=idt
          nbcaldays(idt)=nbcaldays(idt) + 1
          write(outs,*) 'Revised day: ',ifoc,' ',DS1,' ',
     &      icalender(ifoc),' ',calendayname(icalender(ifoc))
          call edisp(iuout,outs)
          modify=.true.
        endif
        CALL EPMENRC
      ELSE
C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

C Errors.
C   14 if(IOS.eq.2)then
C        CALL USRMSG(' No prmission to write array in ',' ','W')
C      else
C        CALL USRMSG(' Long arrary write error in ',' ','W')
C      endif
C      IER=1
C      return
      end

C ********** calenprint **************
C Calenprint displays a calendar for year iyear beginning
C at loopst for loop months. 
C Currently it prints to text feedback or file only. Future
C option is to display in graphic feedback with day types
C as defined by calenmanage.
      subroutine calenprint(itru,act,iyear,loopst,loop)
#include "building.h"

C Parameters
      integer itru    ! reporting unit
      character act*1 ! action to take 't' text feedback, 'g' graphic feedback
      integer iyear   ! year to use for day of week calculations
      integer loopst  ! month to start list/display
      integer loop    ! number of months to list/display

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)

C m1slots and m2slots are for the two possible months to be
C displayed. 42 is 7 days over maximum of 6 periods e.g.
C                   2000
C         Jan                     Feb             
C Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su  
C                 1  2        1  2  3  4  5  6         
C  3  4  5  6  7  8  9     7  8  9 10 11 12 13     
C 10 11 12 13 14 15 16    14 15 16 17 18 19 20    
C 17 18 19 20 21 22 23    21 22 23 24 25 26 27    
C 24 25 26 27 28 29 30    28 29                   
C 31                    
C         Jan                     Feb             
C Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su  
C  x  x  x  x  x  x  x     x  x  x  x  x  x  x
C  x  x  x  x  x  x  x     x  x  x  x  x  x  x
C  x  x  x  x  x  x  x     x  x  x  x  x  x  x
C  x  x  x  x  x  x  x     x  x  x  x  x  x  x
C  x  x  x  x  x  x  x     x  x  x  x  x  x  x
C  x  x  x  x  x  x  x     x  x  x  x  x  x  x
      DIMENSION m1slots(42,2),m2slots(42,2)
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer iicol
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iicol
#endif

C Text for each slot.
      character*3 am1slots(42),am2slots(42)
      character calename*32,calentag*12,calendayname*32,word3*3
      character mohead*44,outs*124,etext*72
      CHARACTER*9 RAMONTH(12)
      DIMENSION ID(12),MDAYS(12)

C ID is the number of days in each month, MDAYS is the julian
C start day of each month.
      DATA ID/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA MDAYS/1,32,60,91,121,152,182,213,244,274,305,335/
      DATA RAMONTH/'January  ','February ','March    ','April    ',
     &             'May      ','June     ','July     ','August   ',
     &             'September','October  ','November ','December '/

C Set common text strings and then if in test mode print months
C side by side.
      mohead = ' Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su '
      if(act.eq.'t')then
        write(outs,'(20x,i4)') iyear
        call edisp(itru,outs)

C If number of months to display is a multiple of 2 display side by side.
C imon1 and imon2 are possible months to dispalay (if zero then do not).
C iloopfn is the end month in the do 44
        imon1=0
        imon2=0
        iloopfn=(loopst+loop)-1
        do 44 im=loopst,iloopfn,2
          if(mod(loop,2).eq.0)then

C Set two consecutive months, write their names and the full
C heading and then find the start day of the week for each.
            imon1=im
            imon2=im+1
            write(outs,'(8x,a,17x,a)') ramonth(imon1),ramonth(imon2)
            call edisp(itru,outs)
            write(outs,'(a)') mohead
            call edisp(itru,outs)
            call edayr(mdays(imon1),idayn,imthn)
            call eweekd(idayn,imthn,iyear,im1dwk)
            call edayr(mdays(imon2),idayn,imthn)
            call eweekd(idayn,imthn,iyear,im2dwk)
          else
            if(im.eq.iloopfn)then
              imon1=im
              imon2=0
              write(outs,'(8x,a)') ramonth(imon1)
              call edisp(itru,outs)
              write(outs,'(a)') mohead(1:22)
              call edisp(itru,outs)
              call edayr(mdays(imon1),idayn,imthn)
              call eweekd(idayn,imthn,iyear,im1dwk)
              im2dwk=0
            else
              imon1=im
              imon2=im+1
              write(outs,'(8x,a,17x,a)') ramonth(imon1),ramonth(imon2)
              call edisp(itru,outs)
              write(outs,'(a)') mohead
              call edisp(itru,outs)
              call edayr(mdays(imon1),idayn,imthn)
              call eweekd(idayn,imthn,iyear,im1dwk)
              call edayr(mdays(imon2),idayn,imthn)
              call eweekd(idayn,imthn,iyear,im2dwk)
            endif
          endif

C For one or two current months fill the m1slots and m2slots.
C icntm1 & icntm2 increment as days fill the slots.
C If the slot is before the start of the month fill with 0,
C if the slot is the first day of the month set slot date and day type,
C if a subsequent day of the month the set slot date and day type.
C am1slots(42),am2slots(42)
          icntm1=0
          icntm2=0
          do 46 ims = 1,42
            if(ims.lt.im1dwk)then
              m1slots(ims,1)=0
              m1slots(ims,2)=0
              am1slots(ims)='   '
            elseif(ims.ge.im1dwk)then
              icntm1=icntm1+1
              if(icntm1.le.id(imon1))then
                m1slots(ims,1)=icntm1
                CALL EDAY(icntm1,imon1,icurday)
                m1slots(ims,2)=icalender(icurday)
                write(am1slots(ims),'(i3)') icntm1 
              else
                m1slots(ims,1)=0
                m1slots(ims,2)=0
                am1slots(ims)='   '
              endif
            endif

C Fill second month with real data or blanks.
            if(im2dwk.eq.0)then
              m2slots(ims,1)=0
              m2slots(ims,2)=0
              am2slots(ims)='   '
            else
              if(ims.lt.im2dwk)then
                m2slots(ims,1)=0
                m2slots(ims,2)=0
                am2slots(ims)='   '
              elseif(ims.ge.im2dwk)then
                icntm2=icntm2+1
                if(icntm2.le.id(imon2))then
                  m2slots(ims,1)=icntm2
                  CALL EDAY(icntm2,imon2,icurday)
                  m2slots(ims,2)=icalender(icurday)
                  write(am2slots(ims),'(i3)') icntm2 
                else
                  m2slots(ims,1)=0
                  m2slots(ims,2)=0
                  am2slots(ims)='   '
                endif
              endif
            endif
  46      continue
          write(outs,'(15a)') (am1slots(J),j=1,7),' ',
     &      (am2slots(J),j=1,7)
          call edisp(itru,outs)
          write(outs,'(15a)') (am1slots(J),j=8,14),' ',
     &      (am2slots(J),j=8,14)
          call edisp(itru,outs)
          write(outs,'(15a)') (am1slots(J),j=15,21),' ',
     &      (am2slots(J),j=15,21)
          call edisp(itru,outs)
          write(outs,'(15a)') (am1slots(J),j=22,28),' ',
     &      (am2slots(J),j=22,28)
          call edisp(itru,outs)
          write(outs,'(15a)') (am1slots(J),j=29,35),' ',
     &      (am2slots(J),j=29,35)
          call edisp(itru,outs)
          write(outs,'(15a)') (am1slots(J),j=36,42),' ',
     &      (am2slots(J),j=36,42)
          call edisp(itru,outs)
  44    continue 
      elseif(act.eq.'g')then
        if(MMOD.eq.8)call startbuffer()
        line=3
        write(etext,'(20x,i4)') iyear
        iside=line
        isize=0
        ifont=2
        call viewtext(etext,iside,isize,ifont)

C If number of months to display is a multiple of 2 display side by side.
C imon1 and imon2 are possible months to dispalay (if zero then do not).
C iloopfn is the end month in the do 44
        imon1=0
        imon2=0
        iloopfn=(loopst+loop)-1
        do 144 im=loopst,iloopfn,2
          if(mod(loop,2).eq.0)then

C Set two consecutive months, write their names and the full
C heading and then find the start day of the week for each.
            imon1=im
            imon2=im+1
            line=line+1
            write(etext,'(8x,a,17x,a)') ramonth(imon1),ramonth(imon2)
            iside=line
            isize=0
            ifont=2
            call viewtext(etext,iside,isize,ifont)
            line=line+1
            write(etext,'(a)') mohead
            iside=line
            call viewtext(etext,iside,isize,ifont)
            call edayr(mdays(imon1),idayn,imthn)
            call eweekd(idayn,imthn,iyear,im1dwk)
            call edayr(mdays(imon2),idayn,imthn)
            call eweekd(idayn,imthn,iyear,im2dwk)
          else
            if(im.eq.iloopfn)then
              imon1=im
              imon2=0
              line=line+1
              write(etext,'(8x,a)') ramonth(imon1)
              iside=line
              isize=0
              ifont=2
              call viewtext(etext,iside,isize,ifont)
              line=line+1
              write(etext,'(a)') mohead(1:22)
              iside=line
              call viewtext(etext,iside,isize,ifont)
              call edayr(mdays(imon1),idayn,imthn)
              call eweekd(idayn,imthn,iyear,im1dwk)
              im2dwk=0
            else
              imon1=im
              imon2=im+1
              line=line+1
              write(etext,'(8x,a,17x,a)') ramonth(imon1),ramonth(imon2)
              iside=line
              isize=0
              ifont=2
              call viewtext(etext,iside,isize,ifont)
              line=line+1
              write(etext,'(a)') mohead
              iside=line
              call viewtext(etext,iside,isize,ifont)
              call edayr(mdays(imon1),idayn,imthn)
              call eweekd(idayn,imthn,iyear,im1dwk)
              call edayr(mdays(imon2),idayn,imthn)
              call eweekd(idayn,imthn,iyear,im2dwk)
            endif
            call forceflush()
          endif

C For one or two current months fill the m1slots and m2slots.
C icntm1 & icntm2 increment as days fill the slots.
C If the slot is before the start of the month fill with 0,
C if the slot is the first day of the month set slot date and day type,
C if a subsequent day of the month the set slot date and day type.
C am1slots(42),am2slots(42)
          icntm1=0
          icntm2=0
          do 146 ims = 1,42
            if(ims.lt.im1dwk)then
              m1slots(ims,1)=0
              m1slots(ims,2)=0
              am1slots(ims)='   '
            elseif(ims.ge.im1dwk)then
              icntm1=icntm1+1
              if(icntm1.le.id(imon1))then
                m1slots(ims,1)=icntm1
                CALL EDAY(icntm1,imon1,icurday)
                m1slots(ims,2)=icalender(icurday)
                write(am1slots(ims),'(i3)') icntm1 
              else
                m1slots(ims,1)=0
                m1slots(ims,2)=0
                am1slots(ims)='   '
              endif
            endif

C Fill second month with real data or blanks.
            if(im2dwk.eq.0)then
              m2slots(ims,1)=0
              m2slots(ims,2)=0
              am2slots(ims)='   '
            else
              if(ims.lt.im2dwk)then
                m2slots(ims,1)=0
                m2slots(ims,2)=0
                am2slots(ims)='   '
              elseif(ims.ge.im2dwk)then
                icntm2=icntm2+1
                if(icntm2.le.id(imon2))then
                  m2slots(ims,1)=icntm2
                  CALL EDAY(icntm2,imon2,icurday)
                  m2slots(ims,2)=icalender(icurday)
                  write(am2slots(ims),'(i3)') icntm2 
                else
                  m2slots(ims,1)=0
                  m2slots(ims,2)=0
                  am2slots(ims)='   '
                endif
              endif
            endif
 146      continue

C Repeat this 6 times (there can be this many lines of 7 days)
          do 149 lj=1,6
            if(lj.eq.1)then
              lstart=1
              lfinish=7
            elseif(lj.eq.2)then
              lstart=8
              lfinish=14
            elseif(lj.eq.3)then
              lstart=15
              lfinish=21
            elseif(lj.eq.4)then
              lstart=22
              lfinish=28
            elseif(lj.eq.5)then
              lstart=29
              lfinish=35
            elseif(lj.eq.6)then
              lstart=36
              lfinish=42
            endif

C Plot out the first 7 days.
            isize=2
            line=line+1
            icx=1
            do 147 j = lstart,lfinish
              write(word3(1:3),'(a)') am1slots(J)(1:3)
              idcol=m1slots(j,2)
              iicol=0
              if(idcol.eq.0)call winscl('-',iicol)
              call findviewtext(icx,line,isize,ix,iy)
              if(idcol.eq.0)then
                call textatxy(ix,iy,word3,'-',idcol)
              else
                call textatxy(ix,iy,word3,'z',idcol)
              endif
              icx=icx+3
  147       continue

C Shift over another character and print out the 2nd months line.
            icx=icx+1
            do 148 j = lstart,lfinish
              write(word3(1:3),'(a)') am2slots(J)(1:3)
              idcol=m2slots(j,2)
              iicol=0
              if(idcol.eq.0)call winscl('-',iicol)
              call findviewtext(icx,line,isize,ix,iy)
              if(idcol.eq.0)then
                call textatxy(ix,iy,word3,'-',idcol)
              else
                call textatxy(ix,iy,word3,'z',idcol)
              endif
              icx=icx+3
  148       continue
 149      continue
 144    continue

C Also include list of day types.
        line=line+2
        iicol=0
        call winscl('-',iicol)
        write(etext,*) 'Calendar: ',calename
        iside=line
        isize=0
        ifont=2
        call viewtext(etext,iside,isize,ifont)
        do 151 icd = 1,nbdaytype
          line=line+1
          write(etext,*) calentag(icd),' ',calendayname(icd)
          idcol=icd
          iicol=0
          if(idcol.eq.0)call winscl('-',iicol)
          call findviewtext(3,line,isize,ix,iy)
          if(idcol.eq.0)then
            call textatxy(ix,iy,etext,'-',idcol)
          else
            call textatxy(ix,iy,etext,'z',idcol)
          endif
 151    continue
        iicol=0
        call winscl('-',iicol)
        call forceflush()
      endif
      return

C Errors for debug printing.
C   14 if(IOS.eq.2)then
C        CALL USRMSG(' No prmission to write array in ',' ','W')
C      else
C        CALL USRMSG(' Long array write error in ',' ','W')
C      endif
C      IER=1
C      return
      end


C ********** updatectlfornewdaytype
C updatectlfornewdaytype adds an additional day type to each of the
C model control domains. 
      subroutine updatectlfornewdaytype(havectl,usecalendar,ier)
#include "building.h"
#include "control.h"

C Parameters passed
      logical havectl ! to remember if control file exists
      logical usecalendar  ! signal so easier to detect if other domains
                           ! have a different number of day types
      integer ier          ! error state

      integer iuout,iuin
      common/OUTIN/IUOUT,IUIN
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      integer nbdaytype,nbcaldays,icalender
      COMMON/FILEP/IFIL
      integer ifil
      common/cctlnm/ctldoc,lctlf
      character ctldoc*248,LCTLF*72
      integer iloop ! index for looping
      integer idt   ! for counting days
      integer icfoc,ictlf  ! for control domain and file unit
      integer istat   ! for error states
      integer ITRC         ! for verbosity

      character dayact*2   ! action to take in managing day types
      logical XST
      integer IHDTP   ! for users preference to add or copy

      havectl=.false.          ! reset to assume no control

      ICTLF=IFIL+1; itrc=0     ! set to silent read
      CALL ERPFREE(ICTLF,ISTAT)
      call FINDFIL(LCTLF,XST)
      if(XST)then
        CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
        havectl=.true.          ! yes we have control

C Ask user whether to create a minimal control regime or to
C duplicate the last control day type in each loop.
C The treatment of day types within addctld is based on an assumption
C that nbdaytpe is not incremented until after the addctld call.
        IDT=nbdaytype     ! set to the not-yet-incremented value
        IHDTP=1
        CALL EASKABC('Control pattern for new day type (see help)?',
     &    ' ','One period free floating',
     &    'Use pattern of last day type','cancel',IHDTP,nbhelp)
        if(IHDTP.eq.1)then
          dayact='SA'       ! assume silent addition
        elseif(IHDTP.eq.2)then
          dayact='SC'       ! assume silent copy
        elseif(IHDTP.eq.3)then
          havectl=.false.   ! ignore control
          return            ! do not bother processing control data
        endif
        if(NCF.gt.0)then  ! zone loops
          icfoc=0
          do 80 iloop=1,NCF
            if(nbcdt(iloop).ne.0.AND.usecalendar)then
              continue   ! calendar and ideal day types do not match
            else
              call ADDCTLD(icfoc,iloop,IDT,dayact)
            endif
  80      continue
        endif
        if(NCC.gt.0)then  ! flow loops
          icfoc=2
          do 81 iloop=1,NCC
            if(nfcdt(iloop).ne.0.AND.usecalendar)then
              continue   ! calendar and flow day types do not match
            else
              call ADDCTLD(icfoc,iloop,IDT,dayact)
            endif
  81      continue
        endif
        if(NCL.gt.0)then  ! plant loops
          icfoc=1
          do 82 iloop=1,NCL
            if(npcdt(iloop).ne.0.AND.usecalendar)then
              continue   ! calendar and plant day types do not match
            else
              call ADDCTLD(icfoc,iloop,IDT,dayact)
            endif
  82      continue
        endif
        if(NGF.gt.0)then  ! global loops
          icfoc=3
          do 83 iloop=1,NGF
            call ADDCTLD(icfoc,iloop,IDT,dayact)
  83      continue
        endif
        if(NOF.gt.0)then  ! optical loops
          icfoc=5
          do 84 iloop=1,NOF
            call ADDCTLD(icfoc,iloop,IDT,dayact)
  84      continue
        endif
        if(nCFCctlloops.gt.0)then ! CFC loops
          icfoc=6
          do 86 iloop=1,nCFCctlloops
            call ADDCTLD(icfoc,iloop,IDT,dayact)
  86      continue
        endif
      endif

      return
      end
      
C *********************** SUBROUTINE FZYFNC (dummy)
C This subroutine calculates the slopes and intercepts for
C all the fuzzy membership functions read in by 'FZDATA'.
      SUBROUTINE FZYFNC
      return
      end


C ******** ADDCTLD
C Add or delete a building/plant/flow/global/optical control day type.
      SUBROUTINE ADDCTLD(icfoc,II,IDT,ACT)
#include "building.h"
#include "net_flow.h"
#include "control.h"

C Parameters
      integer icfoc   ! control domain to work with 
      integer II      ! index of the control
      integer IDT     ! index of the day type to delete or add
C ACT is SA or sa is silent appends a single day type with a single period IDT
C ACT is SD or sd is silent delete the day type IDT
C ACT is SC or sc is silent copy of day type IDT
C ACT is -- interactive use of the subroutine
      character ACT*2

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER nbdaytype,nbcaldays,icalender

      logical usecalendar  ! signal to prevent update of nbcdt

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='ADDCTLD'  ! set for subroutine
      usecalendar=.false.  ! initial assumption

C The nn value represents the day types that existed prior to the intervention.
C Typically a zero means to follow the calendar day types.
  77  if(icfoc.eq.0)then
        nn=nbcdt(ii)
      elseif(icfoc.eq.1)then
        nn=npcdt(ii)
      elseif(icfoc.eq.2)then
        nn=nfcdt(ii)
      elseif(icfoc.eq.3)then
        nn=ngcdt(ii)
      elseif(icfoc.eq.5)then
        nn=nocdt(ii)
      elseif(icfoc.eq.6)then
        nn=nCFCctldaytypes(ii)
      endif
      if(nn.eq.0)then
        nn=nbdaytype  ! set equal to calendar
        usecalendar=.true.
      endif

C Acquire help messages.
      helptopic='ctl_add_day_type'
      call gethelptext(helpinsub,helptopic,nbhelp)

C If interactive ask what to do, otherwise take passed directives.
      if(ACT(1:2).eq.'--')then
        CALL EASKABCD('Options: ',' ','delete day type',
     &    'add day type','copy existing day type','continue',
     &    IW,nbhelp)
      elseif(ACT(1:2).eq.'SD'.or.ACT(1:2).eq.'sd')then
        IW=1
      elseif(ACT(1:2).eq.'SA'.or.ACT(1:2).eq.'sa')then
        IW=2
      elseif(ACT(1:2).eq.'SC'.or.ACT(1:2).eq.'sc')then
        IW=3
      endif

      IF(IW.EQ.1)THEN

C If interactive ask, otherwise take IDT as the day type to act on.
        if(ACT(1:2).eq.'--')then
          ID=1
          CALL EASKI(ID,' ',' Which day type (index)? ',
     &      1,'F',MB,'F',1,'delete day type',IERI,nbhelp)
          if(ieri.eq.-3) return
        else
          ID=IDT
        endif
        if(icfoc.eq.0.and.NN.ge.2)then
          do 793 IDT=ID,NN-1
            ibcdv(II,IDT,1)=ibcdv(II,IDT+1,1)
            ibcdv(II,IDT,2)=ibcdv(II,IDT+1,2)
            nbcdp(II,IDT)=nbcdp(II,IDT+1)
            do 794 IDP=1,nbcdp(II,IDT)
              tbcps(II,IDT,IDP)=tbcps(II,IDT+1,IDP)
              ibctyp(II,IDT,IDP)=ibctyp(II,IDT+1,IDP)
              ibclaw(II,IDT,IDP)=ibclaw(II,IDT+1,IDP)
              bmiscd(II,IDT,IDP,1)=bmiscd(II,IDT+1,IDP,1)
              imis=INT(bmiscd(II,IDT,IDP,1))
              do 795 IPM=2,imis+1
                bmiscd(II,IDT,IDP,IPM)=bmiscd(II,IDT+1,IDP,IPM)
  795         continue
  794       continue
  793     continue
          NN=NN-1        ! decrement local counter
          if(.NOT.usecalendar) nbcdt(II)=NN
        elseif(icfoc.eq.1.and.NN.ge.2)then
          do 693 IDT=ID,NN-1
            ipcdv(II,IDT,1)=ipcdv(II,IDT+1,1)
            ipcdv(II,IDT,2)=ipcdv(II,IDT+1,2)
            npcdp(II,IDT)=npcdp(II,IDT+1)
            do 694 IDP=1,npcdp(II,IDT)
              tpcps(II,IDT,IDP)=tpcps(II,IDT+1,IDP)
              ipctyp(II,IDT,IDP)=ipctyp(II,IDT+1,IDP)
              ipclaw(II,IDT,IDP)=ipclaw(II,IDT+1,IDP)
              pmiscd(II,IDT,IDP,1)=pmiscd(II,IDT+1,IDP,1)
              imis=INT(pmiscd(II,IDT,IDP,1))
              do 695 IPM=2,imis+1
                pmiscd(II,IDT,IDP,IPM)=pmiscd(II,IDT+1,IDP,IPM)
  695         continue
  694       continue
  693     continue
          NN=NN-1        ! decrement local counter
          if(.NOT.usecalendar) npcdt(II)=NN
        elseif(icfoc.eq.2.and.NN.ge.2)then
          do 593 IDT=ID,NN-1
            ifcdv(II,IDT,1)=ifcdv(II,IDT+1,1)
            ifcdv(II,IDT,2)=ifcdv(II,IDT+1,2)
            nfcdp(II,IDT)=nfcdp(II,IDT+1)
            do 594 IDP=1,nfcdp(II,IDT)
              tfcps(II,IDT,IDP)=tfcps(II,IDT+1,IDP)
              ifctyp(II,IDT,IDP)=ifctyp(II,IDT+1,IDP)
              ifclaw(II,IDT,IDP)=ifclaw(II,IDT+1,IDP)
              fmiscd(II,IDT,IDP,1)=fmiscd(II,IDT+1,IDP,1)
              imis=INT(fmiscd(II,IDT,IDP,1))
              do 595 IPM=2,imis+1
                fmiscd(II,IDT,IDP,IPM)=fmiscd(II,IDT+1,IDP,IPM)
  595         continue
  594       continue
  593     continue
          NN=NN-1        ! decrement local counter
          if(.NOT.usecalendar) nfcdt(II)=NN
        elseif(icfoc.eq.3.and.NN.ge.2)then
          do 893 IDT=ID,NN-1
            igcdv(II,IDT,1)=igcdv(II,IDT+1,1)
            igcdv(II,IDT,2)=igcdv(II,IDT+1,2)
            ngcdp(II,IDT)=ngcdp(II,IDT+1)
            do 894 IDP=1,ngcdp(II,IDT)
              tgcps(II,IDT,IDP)=tgcps(II,IDT+1,IDP)
              igctyp(II,IDT,IDP)=igctyp(II,IDT+1,IDP)
              igclaw(II,IDT,IDP)=igclaw(II,IDT+1,IDP)
              gmiscd(II,IDT,IDP,1)=gmiscd(II,IDT+1,IDP,1)
              imis=INT(gmiscd(II,IDT,IDP,1))
              do 895 IPM=2,imis+1
                gmiscd(II,IDT,IDP,IPM)=gmiscd(II,IDT+1,IDP,IPM)
  895         continue
  894       continue
  893     continue
          NN=NN-1        ! decrement local counter
          if(.NOT.usecalendar) ngcdt(II)=NN
        elseif(icfoc.eq.5.and.NN.ge.2)then
          do 1 IDT=ID,NN-1
            iocdv(II,IDT,1)=iocdv(II,IDT+1,1)
            iocdv(II,IDT,2)=iocdv(II,IDT+1,2)
            nocdp(II,IDT)=nocdp(II,IDT+1)
            do 2 IDP=1,nocdp(II,IDT)
              tocps(II,IDT,IDP)=tocps(II,IDT+1,IDP)
              ioctyp(II,IDT,IDP)=ioctyp(II,IDT+1,IDP)
              ioclaw(II,IDT,IDP)=ioclaw(II,IDT+1,IDP)
              omiscd(II,IDT,IDP,1)=omiscd(II,IDT+1,IDP,1)
              imis=INT(omiscd(II,IDT,IDP,1))
              do 3 IPM=2,imis+1
                omiscd(II,IDT,IDP,IPM)=omiscd(II,IDT+1,IDP,IPM)
  3           continue
  2         continue
  1       continue
          NN=NN-1        ! decrement local counter
          if(.NOT.usecalendar) nocdt(II)=NN
        elseif(icfoc.eq.6.and.NN.ge.2)then
          do 6793 IDT=ID,NN-1
            iCFCctldatevalid(II,IDT,1)=iCFCctldatevalid(II,IDT+1,1)
            iCFCctldatevalid(II,IDT,2)=iCFCctldatevalid(II,IDT+1,2)
            nCFCdayctlperiods(II,IDT)=nCFCdayctlperiods(II,IDT+1)
            do 6794 IDP=1,nCFCdayctlperiods(II,IDT)
              CFCctlperiodstart(II,IDT,IDP)=
     &        CFCctlperiodstart(II,IDT+1,IDP)
              iCFCctltype(II,IDT,IDP)=iCFCctltype(II,IDT+1,IDP)
              iCFCctllaw(II,IDT,IDP)=iCFCctllaw(II,IDT+1,IDP)
              CFCmiscdata(II,IDT,IDP,1)=CFCmiscdata(II,IDT+1,IDP,1)
              imis=INT(CFCmiscdata(II,IDT,IDP,1))
              do 6795 IPM=2,imis+1
                CFCmiscdata(II,IDT,IDP,IPM)=
     &          CFCmiscdata(II,IDT+1,IDP,IPM)
 6795         continue
 6794       continue
 6793     continue
          NN=NN-1        ! decrement local counter
          if(.NOT.usecalendar) nCFCctldaytypes(II)=NN
        endif
      ELSEIF(IW.EQ.2)THEN

C Create a new control day type. If interactive ask for periods, if
C in silent mode assume one period. If using calendar days do not
C update nbcdt - let it remain at zero.
        if(icfoc.eq.0.and.NN+1.LE.MCDT)then
          if(ACT(1:2).eq.'--')then
            IP=1
            CALL EASKI(IP,' How many periods in this day type? ',
     &        '(default is 1 period free floating)',
     &        1,'F',MCDP,'F',1,'ctl periods',IERI,nbhelp)
            if(ieri.eq.-3) goto 77
          else
            IP=1
          endif
          NN=NN+1      ! increment local counter
          if(.NOT.usecalendar) nbcdt(II)=NN
          nb=NN
          ibcdv(II,nb,1)=1
          ibcdv(II,nb,2)=365
          nbcdp(II,nb)=IP
          do 784 IDP=1,IP
            if(IDP.eq.1)tbcps(II,nb,IDP)=0.
            if(IDP.gt.1)tbcps(II,nb,IDP)=float(IDP)
            ibctyp(II,nb,IDP)=0
            ibclaw(II,nb,IDP)=2
            bmiscd(II,nb,IDP,1)=0.
  784     continue
        elseif(icfoc.eq.1.and.NN+1.LE.mcdt)then
          if(ACT(1:2).eq.'--')then
            IP=1
            CALL EASKI(IP,' How many periods in this day type? ',
     &        '(default is 1 period free floating)',
     &        1,'F',mcdp,'F',1,'ctl periods',IERI,nbhelp)
            if(ieri.eq.-3) goto 77
          else
            IP=1
          endif
          NN=NN+1      ! increment local counter
          if(.NOT.usecalendar) npcdt(II)=NN
          np=NN
          ipcdv(II,np,1)=1
          ipcdv(II,np,2)=365
          npcdp(II,np)=IP
          do 684 IDP=1,IP
            if(IDP.eq.1)tpcps(II,np,IDP)=0.
            if(IDP.gt.1)tpcps(II,np,IDP)=float(IDP)
            ipctyp(II,np,IDP)=0
            ipclaw(II,np,IDP)=2
            pmiscd(II,np,IDP,1)=0.
  684     continue
        elseif(icfoc.eq.2.and.NN+1.LE.mcdt)then
          if(ACT(1:2).eq.'--')then
            IP=1
            CALL EASKI(IP,' How many periods in this day type? ',
     &        '(default is 1 period free floating)',
     &        1,'F',mcdp,'F',1,'ctl periods',IERI,nbhelp)
            if(ieri.eq.-3) goto 77
          else
            IP=1
          endif
          NN=NN+1      ! increment local counter
          if(.NOT.usecalendar) nfcdt(II)=NN
          nf=NN
          ifcdv(II,nf,1)=1
          ifcdv(II,nf,2)=365
          nfcdp(II,nf)=IP
          do 584 IDP=1,IP
            if(IDP.eq.1)tfcps(II,nf,IDP)=0.
            if(IDP.gt.1)tfcps(II,nf,IDP)=float(IDP)
            ifctyp(II,nf,IDP)=1
            ifclaw(II,nf,IDP)=0
            fmiscd(II,nf,IDP,1)=2.
            fmiscd(II,nf,IDP,2)=0.
            fmiscd(II,nf,IDP,3)=1.
  584     continue
        elseif(icfoc.eq.3.and.NN+1.LE.mcdt)then
          NN=NN+1      ! increment local counter
          if(.NOT.usecalendar) ngcdt(II)=NN
          ng=NN
          igcdv(II,ng,1)=1
          igcdv(II,ng,2)=365
          ngcdp(II,ng)=1
          tgcps(II,ng,1)=0.
          igctyp(II,ng,1)=1
          igclaw(II,ng,1)=0
          gmiscd(II,ng,1,1)=2.
          gmiscd(II,ng,1,2)=0.
          gmiscd(II,ng,1,3)=1.
        elseif(icfoc.eq.5.and.NN+1.LE.mcdt)then
          if(ACT(1:2).eq.'--')then
            IP=1
            CALL EASKI(IP,' How many periods in this day type? ',
     &        '(default is 1 period free floating)',
     &        1,'F',mcdp,'F',1,'ctl periods',IERI,nbhelp)
            if(ieri.eq.-3) goto 77
          else
            IP=1
          endif
          NN=NN+1      ! increment local counter
          if(.NOT.usecalendar) nocdt(II)=NN
          no=NN
          iocdv(II,no,1)=1
          iocdv(II,no,2)=365
          nocdp(II,no)=IP
          do 4 IDP=1,IP
            if(IDP.eq.1)tocps(II,no,IDP)=0.
            if(IDP.gt.1)tocps(II,no,IDP)=float(IDP)
            ioctyp(II,no,IDP)=0
            ioclaw(II,no,IDP)=0
            omiscd(II,no,IDP,1)=0.
  4       continue
        elseif(icfoc.eq.6.and.NN+1.LE.MCDT)then
          if(ACT(1:2).eq.'--')then
            IP=1
            CALL EASKI(IP,' How many periods in this day type? ',
     &        '(default is 1 period free floating)',
     &        1,'F',MCDP,'F',1,'ctl periods',IERI,nbhelp)
            if(ieri.eq.-3) goto 77
          else
            IP=1
          endif
          NN=NN+1      ! increment local counter
          if(.NOT.usecalendar) nCFCctldaytypes(II)=NN
          nb=NN
          iCFCctldatevalid(II,nb,1)=1
          iCFCctldatevalid(II,nb,2)=365
          nCFCdayctlperiods(II,nb)=IP
          do 6784 IDP=1,IP
            if(IDP.eq.1)CFCctlperiodstart(II,nb,IDP)=0.
            if(IDP.gt.1)CFCctlperiodstart(II,nb,IDP)=float(IDP)
            iCFCctltype(II,nb,IDP)=1
            iCFCctllaw(II,nb,IDP)=1
            CFCmiscdata(II,nb,IDP,1)=2.
 6784     continue
        endif
      elseif(IW.EQ.3)then

C Copy and existing control loop day type. If interactive ask, if
C in silent mode assume passed value of IDT. If using calendar days do not
C update nbcdt - let it remain at zero.
        if(ACT(1:2).eq.'--')then
          IDC=1
          CALL EASKI(IDC,' ',' Which day type to copy ? ',
     &      1,'F',MB,'F',1,'copy day type',IERI,nbhelp)
          if(ieri.eq.-3) goto 77
        else
          IDC=IDT
        endif
        if(icfoc.eq.0.and.NN+1.LE.MCDT)then
          NN=NN+1      ! increment local counter
          if(.NOT.usecalendar) nbcdt(II)=NN
          nb=NN
          ibcdv(II,nb,1)=ibcdv(II,IDC,1)
          ibcdv(II,nb,2)=ibcdv(II,IDC,2)
          nbcdp(II,nb)=nbcdp(II,IDC)
          do 494 IDP=1,nbcdp(II,IDC)
            tbcps(II,nb,IDP)=tbcps(II,IDC,IDP)
            ibctyp(II,nb,IDP)=ibctyp(II,IDC,IDP)
            ibclaw(II,nb,IDP)=ibclaw(II,IDC,IDP)
            bmiscd(II,nb,IDP,1)=bmiscd(II,IDC,IDP,1)
            imis=INT(bmiscd(II,IDC,IDP,1))
            do 495 IPM=2,imis+1
              bmiscd(II,nb,IDP,IPM)=bmiscd(II,IDC,IDP,IPM)
  495       continue
  494     continue
        elseif(icfoc.eq.1.and.NN+1.LE.mcdt)then
          NN=NN+1      ! increment local counter
          if(.NOT.usecalendar) npcdt(II)=NN
          np=NN
          ipcdv(II,np,1)=ipcdv(II,IDC,1)
          ipcdv(II,np,2)=ipcdv(II,IDC,2)
          npcdp(II,np)=npcdp(II,IDC)
          do 394 IDP=1,npcdp(II,IDC)
            tpcps(II,np,IDP)=tpcps(II,IDC,IDP)
            ipctyp(II,np,IDP)=ipctyp(II,IDC,IDP)
            ipclaw(II,np,IDP)=ipclaw(II,IDC,IDP)
            pmiscd(II,np,IDP,1)=pmiscd(II,IDC,IDP,1)
            imis=INT(pmiscd(II,IDC,IDP,1))
            do 395 IPM=2,imis+1
              pmiscd(II,np,IDP,IPM)=pmiscd(II,IDC,IDP,IPM)
  395       continue
  394     continue
        elseif(icfoc.eq.2.and.NN+1.LE.mcdt)then
          NN=NN+1      ! increment local counter
          if(.NOT.usecalendar) nfcdt(II)=NN
          nf=NN
          ifcdv(II,nf,1)=ifcdv(II,IDC,1)
          ifcdv(II,nf,2)=ifcdv(II,IDC,2)
          nfcdp(II,nf)=nfcdp(II,IDC)
          do 392 IDP=1,nfcdp(II,IDC)
            tfcps(II,nf,IDP)=tfcps(II,IDC,IDP)
            ifctyp(II,nf,IDP)=ifctyp(II,IDC,IDP)
            ifclaw(II,nf,IDP)=ifclaw(II,IDC,IDP)
            fmiscd(II,nf,IDP,1)=fmiscd(II,IDC,IDP,1)
            imis=INT(fmiscd(II,IDC,IDP,1))
            do 393 IPM=2,imis+1
              fmiscd(II,nf,IDP,IPM)=fmiscd(II,IDC,IDP,IPM)
  393       continue
  392     continue
        elseif(icfoc.eq.3.and.NN+1.LE.mcdt)then
          NN=NN+1      ! increment local counter
          if(.NOT.usecalendar) ngcdt(II)=NN
          ng=NN
          igcdv(II,ng,1)=igcdv(II,IDC,1)
          igcdv(II,ng,2)=igcdv(II,IDC,2)
          ngcdp(II,ng)=ngcdp(II,IDC)
          do 390 IDP=1,ngcdp(II,IDC)
            tgcps(II,ng,IDP)=tgcps(II,IDC,IDP)
            igctyp(II,ng,IDP)=igctyp(II,IDC,IDP)
            igclaw(II,ng,IDP)=igclaw(II,IDC,IDP)
            gmiscd(II,ng,IDP,1)=gmiscd(II,IDC,IDP,1)
            imis=INT(gmiscd(II,IDC,IDP,1))
            do 391 IPM=2,imis+1
              gmiscd(II,ng,IDP,IPM)=gmiscd(II,IDC,IDP,IPM)
  391       continue
  390     continue
        elseif(icfoc.eq.5.and.NN+1.LE.mcdt)then
          NN=NN+1      ! increment local counter
          if(.NOT.usecalendar) nocdt(II)=NN
          no=NN
          iocdv(II,no,1)=iocdv(II,IDC,1)
          iocdv(II,no,2)=iocdv(II,IDC,2)
          nocdp(II,no)=nocdp(II,IDC)
          do 5 IDP=1,nocdp(II,IDC)
            tocps(II,no,IDP)=tocps(II,IDC,IDP)
            ioctyp(II,no,IDP)=ioctyp(II,IDC,IDP)
            ioclaw(II,no,IDP)=ioclaw(II,IDC,IDP)
            omiscd(II,no,IDP,1)=omiscd(II,IDC,IDP,1)
            imis=INT(omiscd(II,IDC,IDP,1))
            do 6 IPM=2,imis+1
              omiscd(II,no,IDP,IPM)=omiscd(II,IDC,IDP,IPM)
  6         continue
  5       continue
        elseif(icfoc.eq.6.and.NN+1.LE.MCDT)then
          NN=NN+1      ! increment local counter
          if(.NOT.usecalendar) nCFCctldaytypes(II)=NN
          nb=NN
          iCFCctldatevalid(II,nb,1)=iCFCctldatevalid(II,IDC,1)
          iCFCctldatevalid(II,nb,2)=iCFCctldatevalid(II,IDC,2)
          nCFCdayctlperiods(II,nb)=nCFCdayctlperiods(II,IDC)
          do 6494 IDP=1,nCFCdayctlperiods(II,IDC)
            CFCctlperiodstart(II,nb,IDP)=CFCctlperiodstart(II,IDC,IDP)
            iCFCctltype(II,nb,IDP)=iCFCctltype(II,IDC,IDP)
            iCFCctllaw(II,nb,IDP)=iCFCctllaw(II,IDC,IDP)
            CFCmiscdata(II,nb,IDP,1)=CFCmiscdata(II,IDC,IDP,1)
            imis=INT(CFCmiscdata(II,IDC,IDP,1))
            do 6495 IPM=2,imis+1
              CFCmiscdata(II,nb,IDP,IPM)=CFCmiscdata(II,IDC,IDP,IPM)
 6495       continue
 6494     continue
        endif
      elseif(IW.EQ.4)then
        return
      endif
      return
      end
      
C ***************** BNDOBJ (dummy)
C BNDOBJ: Does range checking on objects to be displayed.
      SUBROUTINE BNDOBJ(ITRC,IER)
      integer ITRC,IER
      return
      end
      
C ************************************** (dummy)
      subroutine makeCFCfile(icomp,ier)
      integer icomp,ier
      return
      end

C ************************************** (dummy)
      SUBROUTINE LKMENU(ICOMP)
      integer icomp
      return
      end

