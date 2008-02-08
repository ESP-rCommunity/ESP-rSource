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

C ish.F comprising - ISH:     Main program; ets up defaults and
C                             offers main menu.
C                  - SHDPRB:  Scans the system configuration file for
C                             the files that define the problem or
C                             allows user input.
C                  - IMGDISP: Dummy routine needed for call- back
C                             from c.
C The shading & insolation program predicts, for a target zone:
C   1. direct and diffuse shading on opaque and transparent surfaces;
C   2. insolation of internal surfaces.

C The routines comprising ish are as follows.
C shdcon.F - SHDCON: Sets up the parameters relating to shading computation.
C          - SHADC:  Implements the shading computation.
C          - SHDTST: tests grid centre points to see if within surface.
C          - DECOMP: reduces 8 pt shadow 'box' to equivalent outline.
C inscon.F - INSCON: readies the insolation computation and calls INSOLC.
C          - IGEOMS: establishes the surface grid.
C          - INWSHD: reads the zone transitional shading file to extract
C                    insolation source shading information.
C          - INSOLC: controls the insolation computation.
C          - INCAI:  computes areas and angles.
C          - INSORT: sets up mesh points for transparent surfaces.
C          - TRANSP: transforms a point on a surface to 3d coordinate space.
C sifile.F - SFILE:  Open a new transitional shading file.
C          - SIFILE: Opens a zone shading & insolation file.
C          - SWRT:   Writes the shading data to the shading file.
C          - SSAVE:  Checks and writes a zone shading and insolationd b.
C          - RETRV1: Retrives info from header block of the trans file.
C trnfile.F- TFILE1: Writes to transitional shading file.
C          - TFILE2: Transfers transformed coords of surfaces to trns shd file.
C          - TFILE3: Saves hourly grid shading index in the trns shd file.
C trnsfs.F - TRNSF1: Sets of the coefficients of the shading transformation equations.
C          - TRNSF2: Convert solar angles to psuedo angles in new coordinate system.
C          - TRNSF3: Project 8 corner point of the obstruction block.
C trnsfi.F - TRNSF1I:Sets of the coefficients of the insolationtransformation equations.
C          - TRNSF2I:Convert solar angles to psuedo angles in new coordinate system.
C          - TRNSF3I:Project TMC grid to new coordinate system.
C sifops.F - SIFOPS: Allows editing/importing/exporting of zone shading
C                    & insolation db contents.
C sifrd.F  - reads the contents of a zone shading & insolation db for
C            a given month.
C sifwrt.F - writes shading/insolation data to a zone shading &
C            insolation db for a given month.
C synop.F  - SHSYNP: Display table of direct and diffuse shading factors for
C                    analysed surfaces.
C            INSYNP: Display internal surface insolation proportions for
C                    analysed sources.
C graph.F  - GRAPH:  Controls display of surface & shaded areas.
C          - FACDRW: Draws the surface normal elevation.
C          - SHDDRW: Plots points on a surface the shaded portion.
C          - SITPLN: Draws the site plan showing zone & obstructions.
C miscel.F - AREA:   Returns area of a polygon.
C          - MESH:   Computes centre point X & Z coordinates of grid
C                    squares.
C          - POINT1: Decides if a point is within a surface boundary.
C          - POINT2: Decides if a point is within a shadow boundary.
C ashrea.F - WINSHD: ASHRAE single window shading prediction method.
C          - WSHAD:  Executes the ASHRAE algorithm.

C ********** ISH **********
C Sets up defaults and offers main menu.

      program ish
#include "building.h"
#include "esprdbfile.h"

C IEEE callback (Solaris)
      external SIGFPE_bps      
      integer SIGFPE_bps      

      common/outin/iuout,iuin
      common/tracech/icout
      common/filech/ixopen,ixunit,ixpunit
      common/filep/ifil
      common/tc/itc,icnt
      common/spad/mmod,limit,limtty
      common/image/imt,eyem(3),viewm(3),hith,yon,ang,hang,wide
      common/pophelp/h(60)
      common/viewpx/menuchw,igl,igr,igt,igb,igw,igwh
      common/appw/iappw,iappx,iappy
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      common/set1/iyear,ibdoy,iedoy,ifday,iftime
      common/gfont/ifs,itfs,imfs
      common/gzonpik/izgfoc,nzg,nznog(mcom)
      common/ray2/itdsp,itbnd,itept,itznm,itsnm,itvno,itorg,itsnr,
     &            itobs,ithls,ithlz,itgrd,grdis,itppsw
      common/ray3/modify,modlen,modbnd
      common/fopened/cfgok,mldbok,matdbok,ctlok,optkok
      common/contr/mon,isc(ms),iyd
      common/data1/ical
      common/mtfile/ltrns,multic,mons,monf

      character ctype*4
      real gversion
      integer igupgrade
      common/g0/ctype(mcom),gversion(mcom),igupgrade
      common/c1/ncomp,ncon
      common/c2/lsnam,nccode(mcom),lproj(mcom),lgeom(mcom),
     &          lshad(mcom),lthrm(mcom),indutl(mcom),lutil(mcom)
      common/c20/nzsur(mcom),nztv(mcom)
      common/c21/ifcfg,cfgroot,lcfgf
      common/indics/ivf(mcom),isi(mcom),ihc(mcom),
     &              itw(mcom),icgc(mcom),iobs(mcom)
      common/udesc/lview(mcom),lhcco(mcom),
     &             ltwin(mcom),lcgcin(mcom),zobs(mcom)

C Path to model and command line file (if any).
      common/rpath/path
      common/uhome/upath
      common/udot/esprc
      common/rcmd/lcmdfl

C Indicator of possible focus zone and action to take.
      common/rzone/inzone
      common/ract/paction

C Where default db lists are kept.
      common/defdb/dfdblbl,defdbfl
      common/precz/zname(mcom),zdesc(mcom)
      common/prec8/slat,slon

C Project journal on/off, unit number, cmd, file name.
      common/journopt/journio,iuj,journcmd,jfile

C Defaults.
      common/deflt2/dfcfg,dfctl,defrlb,dafres,daprob,dpnf,dsbem
      
C Significant figure reporting limit.
      common/sfig/nsigfig
      common/exporttg/xfile,tg,delim

C Name of current application.
      common/appname/cAppName

C Material data arrays.
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(mgcl),
     &  matdbdate,matdbdoc,matcatname(mgcl),matcatdoc(mgcl)

      real matver
      dimension month(12),items(22),isadd(12),ishd(12)
      character*72 lname,ltrns,tbase,h
      character*72 lcfgf,lsnam,lproj,lgeom,lshad,lthrm,lutil
      character*72 lview,lhcco,ltwin,lcgcin,zobs
      character items*26,cAppName*12
      character month*3,outs*124,outs248*248,etext*60,ltmp*144
      character descrh*5,descrd*5,zname*12,zdesc*64,inf*144,lcmdfl*144
      character path*72,upath*72,esprc*72,cfgroot*24
      character dfdblbl*20,defdbfl*72
      character inz*16,inzone*16,zn*12,pact*16,paction*16,outstr*124
      character*72 dfcfg,dfctl,defrlb,daprob,dafres,dpnf,dsbem
      character journcmd*20,jfile*72,uname*24,fs*1,cjfile*72
      character xfile*144,tg*1,delim*1,mode*4
      logical silent,unixok,there,context,existing
      logical newgeo  ! to use for testing if new/old geometry file.
      logical modify,modlen,modbnd,xst
      logical ok,dok,cfgok,mldbok,matdbok,ctlok,optkok

      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &           'Sep','Oct','Nov','Dec'/

C Initialize global common data.
      call ezero
      call curmodule('ish')
      cAppName = 'ish'
      iuout=6
      iuin=5
      limtty=24
      limit =24
      nsigfig=3
      matver=0.0   ! Initial assumption of binary materials database.

C iunit  - zone transitional shading file
C ifilsi - zone shading & insolation db
C iuf    - miscellaneous other files
C ifcfg  - system configuration file
      ifil=11
      iunit=ifil
      iuf=ifil+1
      ifcfg=ifil+2
      ifilsi=ifil+3
      iairp=ifil+6
      iuj=ifil+10
      cfgok=.false.
      lname='SHDTRN'
      zn='UNKNOWN'
      call parsfza(modl,iappw,iappx,iappy,inf,inz,pact)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

      ifday=2
      iftime=0
      iyear=2008
      ibdoy=1
      iedoy=1
      itc=0
      icout=iuout
      newgeo=.false.  	! Assume old format geometry.

C Initial coordinates for eye point, viewing point and angle of view.
      eyem(1)=-100.
      eyem(2)=-100.
      eyem(3)=100.
      viewm(1)=10.
      viewm(2)=10.
      viewm(3)=10.
      ang=40.

C Image option flags.
      itdsp=0
      itbnd=1
      itept=0
      itznm=0
      itsnm=0
      itvno=1
      itorg=1
      itsnr=0
      itgrd=1
      grdis=0.0
      itppsw=0
      ifs=1
      itfs=1
      imfs=1

C If your compiler does not support floating-point arithmetic
C according to ANSI/IEEE Std 754-1985 comment out the following code.

C Initialize mode, status, and signal handling for IEEE arithmetic.
C See also: f77_ieee_environment(3F) f77_floatingpoint(3F)
C           Note that in case of Sun FORTRAN, IEEEE exceptions arise
C           only if: Sun-3 f77 with -f68881 or -ffpa option
C                or: Sun-4 f77.
      ieeer=ieee_handler('set','common',SIGFPE_ABORT)
      ieeer=ieee_handler('set','common',SIGFPE_bps)
      if(ieeer.ne.0) write(iuout,*) ' IEEE_handler not set !'

C Initialise terminal and flags.
      mmod=modl
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
         iappw=530
         iappx=75
         iappy=170
      else
         if(iappx.le.0)iappx=75
         if(iappy.le.0)iappy=170
         if(iappw.le.200)then
            iappwi=int(530*iappw*0.01)
            iappw=iappwi
         elseif(iappw.gt.200)then
            continue
         endif
      endif

C Set pixels height to iappw and pixels width to factor in
C monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      if(iappw.gt.0.and.iappw.lt.100)then
         menuchw = MAX0(int(28*iappw*0.01),16)
         limtty=10
         limit =10
      else
         menuchw = 28
         limtty=10
         limit =10
      endif
      if(mmod.EQ.8)then

C Set initial font sizes (imfs is for graphs, ifs for dialog & text
C feedback).
         imfs=1
         ifs=1
         itfs=1
         call userfonts(ifs,itfs,imfs)
      else
         limtty=16
         limit =16
      endif
      call epages(mmod,iuin,iuout,iappwi,iapphi,iappx,iappy,menuchw,
     &'ESP-r Shading & Insolation: enquiries to esru@strath.ac.uk')

C Open the text display box equal to limtty if mmod = 8.
      if(mmod.eq.8)then
         call win3d(menuchw,2,1,1,3,igl,igr,igt,igb,igw,igwh)
#ifdef X11
         call opencpw
         call opensetup
         call updwire(0)
         call updcapt(1)
         call updazi(1)
#endif
         call setzscale()
         mdispl=0
         nifgrey=0
         ncset=0
         ngset=0
         nzonec=0
         call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
         call startbuffer()
      endif
      if(mmod.ne.-6)icout=iuout

C Find the user's home folder then get user's custom settings.
      call usrhome(upath)
      if(unixok)then
         write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
         write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,iuf,0,ier)

C If there is a journal active, set up to add to itl. First get
C the journal file from the user's home folder. All subsequent
C tstamp messages will be appended to the journal.
      if(journio.eq.1)then
         uname=' '
         call usrname(uname)
         cjfile=' '
         write(cjfile,'(a,a1,3a)')upath(1:lnblnk(upath)),fs,'.',
     &                            uname(1:lnblnk(uname)),'cur_j'
         there=.false.
         inquire(file=cjfile,exist=there)
         if(there)then
            close(iuj)
            open(iuj,file=cjfile,status='UNKNOWN',err=901)
            call stripc(iuj,outstr,0,ND,1,'header',ier)
            if(outstr(1:15).eq.'Current_Journal')then
               k=16
               call egetrm(outstr,K,jfile,'W','root journ',ier)
            endif
            close(iuj)
         endif

         write(outs,'(2a)')'Journal (shading/insolation) for:',
     &                     uname(1:lnblnk(uname))
         call tstamp('>',outs)
      endif
  902 continue

C Scan the defaults file for default configuration.
      call escdef(iuf,ier)

C Take command line file name as initial configuration file.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
         lcmdfl=inf
         write(outs248,'(2a)')'Input file is: ',lcmdfl
         if(pact(1:13).ne.'update_silent')then
            call edisp248(iuout,outs248,80)
         endif
      else
         lcmdfl=' '
      endif

C Take command line zone name and determine if focus should be
C on a single zone.
      if(inz(1:1).ne.' ')then
         inzone=inz
         write(outs,'(2a)')'Input zone is: ',inzone
         if(pact(1:13).ne.'update_silent')then
            call edisp(iuout,outs)
         endif
         write(outs,'(2a)')'Ish: the input zone is: ',inzone
         call tstamp('>',outs)
      else
         inzone='ALL'
         call tstamp('>','Ish: no focus zone')
      endif

C Take command line action parameter. If paction = 'recalculate' then
C after the configuration is read in, do a standard annual calculation
C and pause so user can view results. If paction = 'update_silent' then
C do standard annual calculation and exit when completed.
      if(pact(1:1).ne.' ')then
         paction=pact
         write(outs,'(2a)')'Ish: the input action is: ',paction
         call tstamp('>',outs)
         if(paction(1:11).eq.'recalculate')then
            write(outs,'(2a)')' the action is: ',paction
            call edisp(iuout,outs)
            silent=.true.
         elseif(paction(1:13).eq.'update_silent')then
            silent=.true.
         else
            paction='INTERACTIVE'
            call tstamp('>','Ish: interactive mode')
            silent=.false.
         endif
      else
         paction='INTERACTIVE'
         silent=.false.
      endif

      ical=0
      multic=0
      icomp=1
      mons=1
      monf=12

C Export (grtool format) file information. Temporarily use 
C unit ifil+6 for output file.
      xfile='shading.txt'
      ixopen=0
      ixunit=ifil+9
      delim='-'

C If an input file has been specified then load it.
      lcfgf=' '
      xst=.false.
      if(lcmdfl(1:2).ne.'  '.and.lcmdfl(1:4).ne.'UNKN')then
         inquire(file=lcmdfl,exist=xst)
         if(xst)then
            call fdroot(lcmdfl,path,lcfgf)
            if(paction(1:13).ne.'update_silent')then
               call usrmsg('Scanning the model...',' ','-')
            endif
            mode='NONE'
            call ersys(lcfgf,ifcfg,iairp,mode,0,ier)
            if(paction(1:13).ne.'update_silent')then
               call usrmsg('Scanning the model...done.',' ','-')
            endif
            if(ier.eq.0)then
               cfgok=.true.
               nzones=ncomp
               call zdata (itrc,ier,nzones,isfsum)
               if(mmod.EQ.8)call updwire(ncomp)

C If inzone = 'All' display an image of the model. If inzone is
C either a string which matches a zone name or is an index, then
C focus on that zone. If inzone = UNKNOWN or the index 0 present
C a list.
               call zindex(inzone,index)
               if(index.le.0)goto 201
               icomp = index
               zn=zname(icomp)

C Set up zone transitional shading file based on zone name.
               write(lname,'(a)')zn(1:lnblnk(zn))
               ok=.true.
               ltrns=lname
               itc=0
               call shdprb(itc,silent,icomp,ier)
            endif
         endif
      endif

C If in recalculation mode, proceed with minimal interface. If
C an existing db is found, removed it and open a new one before
C starting calculations.
      if(silent)then
         if(isi(icomp).EQ.1)then
            maxrec=24
            if(nzsur(icomp).gt.24)maxrec=nzsur(icomp)
            call findfil(lshad(icomp),xst)
            if(xst)then
               call erpfree(ifilsi,istst)
               call efopran(ifilsi,lshad(icomp),maxrec,1,ier)
               write(outs,'(2a)') 'Deleting previous ',lshad(icomp)
               if(paction(1:13).ne.'update_silent')then
                  call edisp(iuout,' ')
                  call edisp(iuout,outs)
               endif
               call efdelet(ifilsi,istat)
               call sifile(icomp)
            endif
          
C Proceed with shading and then insolation calculations.
            if(iobs(icomp).eq.1)then
               call egomst(iuf,zobs(icomp),0,0,iuout,ier)
               zn=zname(icomp)
               write(outs,'(3a)')'Shading calculations for ',
     &                           zn(1:lnblnk(zn)),'...'
               call edisp(iuout,outs)
               call tstamp('>','Ish: re-calculate shading')
               call shdcon(icomp,ier,'s')
               write(outs,'(3a)')'Shading calculations for ',
     &                           zn(1:lnblnk(zn)),'...done.'
               call edisp(iuout,outs)
            else
               call edisp(iuout,
     &           'No obstructions, calculating insolation.')
            endif
            icstat=0
            zn=zname(icomp)
            write(outs,'(3a)')'Insolation calculations for ',
     &                        zn(1:lnblnk(zn)),'...'
            call edisp(iuout,outs)
            call tstamp('>','Ish: start insolation computation')
            call inscon(icomp,icstat,'s')
            write(outs,'(3a)')'Insolation calculations for ',
     &                        zn(1:lnblnk(zn)),'...done.'
            call edisp(iuout,outs)
         else
            call usrmsg('Unknown Zone Shading & Insolation file.',
     &                  'Cannot recalculate!','W')
         endif
         if(paction(1:11).eq.'recalculate')then
            call usrmsg('Recalculation complete. You may exit.',
     &                  ' ','-')
         elseif(paction(1:13).eq.'update_silent')then

C Finished recalculation so exit program, delete the transitional
C shading files. If a single month calculation has been done then
C offer to save or delete. If a multi-month calculation has been
C done, ask user to delete the transitional files.
            call usrmsg('Recalculation complete...exiting.',' ','-')
            iunit=ifil
            if(multic.eq.0)then
               call efdelet(iunit,istat)
            elseif(multic.eq.2)then
               irecw=ms+5
               do 155 ix=1,ncomp
                  do 156 im=1,12
                     write(lname,'(a,a3)')
     &                    zname(ix)(1:lnblnk(zname(ix))),month(im)
                     call efopran(iunit,lname,irecw,0,ier)
                     if(ier.eq.0)call efdelet(iunit,istat)
 156              continue
 155           continue
               call pauses(1)
            endif
         endif
         call epagend
         stop
      endif

C Not in silent recalculation mode so present application version
C information.
      call ESPrVersion("summary")
      
C Display opening menu.
  201 ier=0
      inos=-3
      write(items(1),'(a,a17)') 'a model: ',lcfgf(1:17)
      write(items(2),'(a,a18)') '  path: ',path(1:18)
      write(items(3),'(a,a12)') 'b zone: ',zn(1:12)
      items(4) =                '  _______________________ '
      items(5)=                 'c ASHRAE window shading   '
      items(6)=                 'd Mercator sun path       '
      items(7)=                 'e view from sun/site plan '
      items(8) =                '  _______________________ '
      items(9)=                 'f calculate shading       '
      items(10)=                'g shading synopsis        '
      items(11)=                'h shadow image            '
      items(12) =               '  _______________________ '
      items(13)=                'i calculate insolation    '
      items(14)=                'j insolation synopsis     '
      items(15) =               '  _______________________ '
      items(16)=                'k shd/ins db contents     '
      items(17) =               '  _______________________ '
      if(itc.eq.0)then
         items(18)=              'l trace output >> none    '
      elseif(itc.eq.1)then
         items(18)=              'l trace output >> fort.33 '
      endif
      if(ixopen.eq.1)then
         items(19)='> outputs >> file       '
      elseif(ixopen.eq.0)then
         items(19)='> outputs >> screen     '
      endif
      items(20)=                '! browse zone surfaces    '
      items(21)=                '? help                    '
      items(22)=                '- exit Shading/Insolation '

C If zone has not been selected disallow calculations and views.
      if(zn(1:4).eq.'UNKN')then
         items(9) =              'f  ...                    '
         items(10)=              'g  ...                    '
         items(11)=              'h  ...                    '
         items(13)=              'i  ...                    '
         items(14)=              'j  ...                    '
         items(16)=              'k  ...                    '
      endif
      nitems = 22

C Use askabout to instantiate the initial help messages
C (2nd parameter is 1).
      call askabout('ish',1)
      call emenu('Shading & Insolation',items,nitems,inos)

C Request definition of model.
      if(inos.eq.1)then
         h(1)='The Model Configuration File holds the definition'
         h(2)='of the building/plant to be simulated, including the'
         h(3)='names of all constituent files. If the name given'
         h(4)='matches an existing file it will be read and the'
         h(5)='data therein made available for use.'
         ltmp=lcmdfl

C Call easkf depending on the current file name length. The X11
C version will be returning only the name of the file, while the
C GTK version will be returning the name with the full path.
  289    llt=lnblnk(ltmp)
         iglib = igraphiclib()  ! find out if X11, GTK or text only.
         if(iglib.eq.1.or.iglib.eq.3)then
            if(llt.lt.96)then
               call easkf(ltmp,'Model Configuration file?',' ',
     &                        96,dfcfg,'config file name',ier,5)
            elseif(llt.ge.96.and.llt.lt.124)then
               call easkf(ltmp,'Model configuration file?',' ',
     &                       124,dfcfg,'config file name',ier,5)
            elseif(llt.ge.124.and.llt.le.144)then
               call easkf(ltmp,'Model configuration file?',' ',
     &                       144,dfcfg,'config file name',ier,5)
            endif
         elseif(iglib.eq.2)then
            call easkf(ltmp,'Model configuration file?',' ',
     &                       144,dfcfg,'config file name',ier,5)
         else
            call easkf(ltmp,'Model configuration file?',' ',
     &                        96,dfcfg,'config file name',ier,5)
         endif

         if(ltmp(1:2).eq.'  '.or.ltmp(1:4).eq.'UNKN')goto 289
         call st2file(ltmp,lcmdfl)
         inquire (file=lcmdfl,exist=xst)
         if(xst)then
            call fdroot(lcmdfl,path,lcfgf)
            call tstamp('>','Ish: scanning new model')
            call tstamp('>',lcfgf)
            mode='NONE'
            call ersys(lcfgf,ifcfg,iairp,mode,0,ier)
            if(ier.eq.0)then
               cfgok=.true.
               nzones=ncomp
               call zdata (itrc,ier,nzones,isfsum)
            endif
         endif

C Read geometry file for either calculation purpose.
      elseif(inos.eq.3)then
         if(.not.cfgok)then
            call usrmsg(' ','Please define the model first!','W')
            goto 201
         endif
         ic=-1
         call easkgeof('Zone to assess:',cfgok,ic,'-',ier)
         if(ic.eq.0.or.ic.eq.-1)goto 201
         ier=0
         icomp=ic
         zn=zname(icomp)
         write(outs,'(2a)')'Ish: shift zone focus to: ',zn
         call tstamp('>',outs)

C If there is an existing transitional shading file with 
C the zone's name as root, ask whether to use it.
         write(lname,'(a)')zname(icomp)(1:lnblnk(zname(icomp)))
         ltrns=lname
         call shdprb(itc,silent,icomp,ier)

C Calculate window shading using the ASHRAE algorithm.
      elseif(inos.eq.5)then
         call tstamp('>','Ish: enter ASHRAE algorithm')
         call winshd

C Mercator sun plot.
      elseif(inos.eq.6)then
         call tstamp('>','Ish: enter sunplot')
         imo=1
         ido=1
         call askday(ifday,imo,ido,ijday,ier)
         call sunplt(ijday)

C Draw the site plan or sun view.
      elseif(inos.eq.7)then
         if(mmod.lt.8)goto 201
         h(1)='The site plan shows zones (marked `Target`) and'
         h(2)='obstructions (marked `Bn` where n is the block'
         h(3)='number). The view from the sun places the viewer'
         h(4)='at a distance of 1 km from `Target`.'
         call easkab(' ','View:','plan','from sun',iw,4)

C After drawing the site plan, the zone geometry file is reread 
C to reconstruct /G1/,/G2/ and /G5/.
         if(iw.eq.1)then
            call tstamp('>','Ish: enter site plan')
            call sitpln(icomp)
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(newgeo)then
               call georead(iuf,lgeom(icomp),icomp,0,0,iuout,ier)
            else
               call egomin(iuf,lgeom(icomp),icomp,0,0,iuout,ier)
            endif
         else

C Draw the zone and any obstructions associated with it based on
C the sun position. Confirm the latitude & longitude, request
C the day and time, compute the positon and update the view.
            call tstamp('>' ,'Ish: look at sun view')
            nts=1
            imo=1
            ido=9
            stime=12.
  67        call asktim(ifday,nts,imo,ido,ijday,stime,it,ier)
  68        call eazalts(stime,ijday,slat,slon,isunup,sazi,salt)
            if(salt.gt.0.0)then
               call angxyz(sazi,salt,x1,y1,z1)
               eyem(1)=x1
               eyem(2)=y1
               eyem(3)=z1
               itsnm=1
               modify=.true.
               modlen=.true.
               nzg=1
               nznog(1)=icomp
               izgfoc=icomp
               call adjview(itc,ier)

C Generate a heading for the view.
               call edtime(stime,descrh,descrd,timer)
               write(etext,'(a,i2,1x,a3,a,a5,a,f6.1,a,f5.1)')
     &            'View @ ',ido,month(imo),', ',descrh,
     &            ' Sun position - azimuth ',sazi,', altitude ',salt
               call viewtext(etext,2,1,ifs)
               call easkab(' ','View:','next hour','exit',iw,0)
               if(iw.eq.1)then
                  stime=stime+((60.0/float(nts))/60.0)
                  goto 68
               else
                  itsnm=0
                  goto 201
               endif
            else
               call easkab('View point below horizon!',' ',
     &                     'Specify another time?','exit',iw,0)
               if(iw.eq.1)goto 67
               itsnm=0
            endif
         endif

C Calculate direct and diffuse shading on specified external surfaces.
      elseif(inos.eq.9)then
         if(zn(1:4).eq.'UNKN') then
            call edisp(iuout,'Select a zone first!')
            goto 201
         endif
         if(isi(icomp).eq.1)then

C If there is already data in the file ask user what to do.
            existing=.false.
            irec=1
            read(ifilsi,rec=irec,iostat=istat,err=30)(ishd(i),i=1,12),
     &                                               (isadd(i),i=1,12)
            do 10 i=1,12
               if(ishd(i).ne.0.or.isadd(i).ne.0)existing=.true.
   10       continue
            if(existing)then
               h(1)='The Zone Shading & Insolation file already'
               h(2)='contains data. New computations can overwrite'
               h(3)='these data or another file may be created.'
               call easkabc('Zone Shading & Insolation db has data!',
     &           'Options:','overwrite','create new db','cancel',iw,3)
               if(iw.eq.1)then
                  continue
               elseif(iw.eq.2)then
                  maxrec=24
                  if(nzsur(icomp).gt.24)maxrec=nzsur(icomp)
                  call erpfree(ifilsi,istst)
                  call efopran(ifilsi,lshad(icomp),maxrec,1,ier)
                  write(outs,'(2a)') 'Deleting previous ',lshad(icomp)
                  if(paction(1:13).ne.'update_silent')then
                     call edisp(iuout,' ')
                     call edisp(iuout,outs)
                  endif
                  call efdelet(ifilsi,istat)
                  call sifile(icomp)
               elseif(iw.eq.3)then
                  goto 201
               endif
            endif
            call tstamp('>','Ish: start shading computation')
            call shdcon(icomp,ier,'i')
         else
            call usrmsg(
     &       'No Zone Shading & Insolation db! Use Project Manager',
     &       'to specify one for this zone or choose another zone.',
     &       'W')
         endif

C Synopsis of surface shading calculation results (uses information held
C in the zone transitional shading file).
      elseif(inos.eq.10)then
         if(zn(1:4).eq.'UNKN')then
            call edisp(iuout,'Select a zone first!')
            goto 201
         else
            call tstamp('>','Ish: shading synopsis')
   65       h(1)='The month is specified as an integer with'
            h(2)='January = 1, February = 2 and so on.'
            mon=mons
  202       call easki(mon,' ','Computation month number?',
     &           1,'F',12,'F',1,'month number',ier,2)
            if(ier.ne.0)goto 65

C Allow for number of surfaces within the zone transitional shading
C file.
            irecw=ms+5
            write(tbase,'(a,a3)')ltrns(:lnblnk(ltrns)),month(mon)
            call efopran(iunit,tbase,irecw,1,ier)
            if(ier.eq.0)then
               call edisp(iuout,' ')
               write(outs,'(3a)')'Zone transitional shading file ',
     &                              tbase(:lnblnk(tbase)),' used.'
               call edisp(iuout,outs)
            else
               call edisp(itru,' ')
               write(outs,'(3a)')'No insolation data for ',month(mon),
     &                                                             '!'
               call edisp(itru,outs)
               goto 201
            endif

C Shading synopsis is based on information in the zone transitional
C shading file.
            call retrv1(icomp)
            call shsynp(icomp)
            if(mon.lt.12)then
               call easkab(' ','Consider another month?','yes',
     &                     'no',iw,0)
               if(iw.eq.1)then
                  mon=mon+1
                  goto 202
               endif
            endif
         endif

C Plot surface shading patterns using information in zone transitional
C shading file.
      elseif(inos.eq.11)then
         if(zn(1:4).eq.'UNKN')then
            call edisp(iuout,'Select a zone first!')
            goto 201
         else
   66       h(1)='The month is specified as an integer with'
            h(2)='January = 1, February = 2 and so on.'
            mon=mons
  203       call easki(mon,' ','Computation month number?',
     &                 1,'F',12,'F',1,'month number',ier,2)
            if(ier.ne.0)goto 66

C Allow for number of surfaces within the zone transitional shading
C file.
            irecw=ms+5
            write(tbase,'(a,a3)')ltrns(:lnblnk(ltrns)),month(mon)
            call efopran(iunit,tbase,irecw,1,ier)
            if(ier.eq.0)then
               call edisp(iuout,' ')
               write(outs,'(3a)')'Zone transitional shading file ',
     &               tbase(:lnblnk(tbase)),' used.'
               call edisp(iuout,outs)
            else
               call usrmsg('A zone transitional shading file does',
     &                     'not exist for this month.','W')
               goto 201
            endif
            call retrv1(icomp)
            call tstamp('>','Ish: view shading patterns')
            call graph(icomp)
            if(mon.lt.12)then
               call easkab(' ','Consider another month?','yes',
     &                     'no',iw,0)
               if(iw.eq.1)then
                  mon=mon+1
                  goto 203
               endif
            endif
         endif

C Calculate insolation of internal surfaces due to specified external 
C source surfaces.
      elseif(inos.eq.13)then
         if(zn(1:4).eq.'UNKN')then
            call edisp(iuout,'Select a zone first!')
            goto 201
         endif
         if(isi(icomp).eq.1)then
            existing=.false.
            irec=1
            read(ifilsi,rec=irec,iostat=istat,err=30)(ishd(i),i=1,12),
     &                                               (isadd(i),i=1,12)

            do 20 i=1,12
               if(ishd(i).ne.0.or.isadd(i).ne.0)existing=.true.
   20       continue
            if(existing)then
               h(1)='The Zone Shading & Insolation file already'
               h(2)='contains data. New computations can overwrite'
               h(3)='these data or another file may be created.'
               call easkabc('Zone Shading & Insolation db has data!',
     &           'Options:','overwrite','create new db','cancel',iw,3)
               if(iw.eq.1)then
                  continue
               elseif(iw.eq.2)then
                  maxrec=24
                  if(nzsur(icomp).gt.24)maxrec=nzsur(icomp)
                  call erpfree(ifilsi,istst)
                  call efopran(ifilsi,lshad(icomp),maxrec,1,ier)
                  write(outs,'(2a)') 'Deleting previous ',lshad(icomp)
                  if(paction(1:13).ne.'update_silent')then
                     call edisp(iuout,' ')
                     call edisp(iuout,outs)
                  endif
                  call efdelet(ifilsi,istat)
                  call sifile(icomp)
               elseif(iw.eq.3)then
                  goto 201
               endif
            endif
            call tstamp('>','Ish: start insolation computation')
            icstat=0
            call inscon(icomp,icstat,'-')
         else
            call usrmsg(
     &       'No Zone Shading & Insolation db! Use Project Manager',
     &       'to specify one for this zone or choose another zone.',
     &       'W')
         endif

C Synopsis of insolation calculation results (based on information
C held in the zone shading & insolation db.
      elseif(inos.eq.14)then
         if(zn(1:4).eq.'UNKN')then
            call edisp(iuout,' ')
            call edisp(iuout,'Select a zone first!')
            goto 201
         else
            call tstamp('>','Ish: list insolation synopsis')
            h(1)='The month is specified as an integer with'
            h(2)='January = 1, February = 2 and so on.'
            mon=mons
  204       call easki(mon,' ','Month number?',
     &                 1,'F',12,'F',1,'ins comp month',ier,2)
            if(ier.ne.0)goto 204
            call sifrd(icomp,mon,ier)
            call insynp(icomp)
            if(mon.lt.12)then
               call easkab(' ','Consider another month?','yes',
     &                     'no',iw,0)
               if(iw.eq.1)then
                  mon=mon+1
                  goto 204
               endif
            endif
         endif

C Edit/import/export/list.
      elseif(inos.eq.16)then
         if(zn(1:4).eq.'UNKN')then
            call edisp(iuout,'Select a zone first!')
            goto 201
         else
            call sifops(icomp)
         endif

C Set trace level displayed in opening menu and main menu.
      elseif(inos.eq.18)then
         itc=itc+1
         if(itc.gt.1)then
            itc=0
            icout=iuout
         elseif(itc.eq.1)then
            icout=33
         endif

C Redirect synoptic outputs and result of ASHRAE method to a
C user specified file.
      elseif(inos.eq.19)then
         if(ixopen.eq.0)then
            write(xfile,'(2a)')zname(icomp)(1:lnblnk(zname(icomp))),
     &                         '.txt'  ! initial file name
         endif
         call edisp(iuout,' ')
         call ctlexp(xfile,ixopen,ixloc,ixunit,'T','Report',ier)

C Surface summary: print header, followed by surface information.
      elseif(inos.eq.20)then
         call tstamp('>','Ish: list surface attributes')
         context=.false.
         call surinfo(icomp,iuout,context)

C Help via the askabout facility in esru_ask.F - 0 requests display.
      elseif(inos.eq.21)then
         call askabout('ish ',0)

C Exit program: if nothing has been done, delete the transitional
C shading file; if a single or multi-month calculation has been done
C then offer to save or delete.
      elseif(inos.eq.22)then
         iunit=ifil
         if(multic.eq.0)then
            call efdelet(iunit,istat)
         elseif(multic.eq.2)then
            dok=.true.
            h(1)='Zone transitional shading files were used to'
            h(2)='store intermediate shading results for use in'
            h(3)='any subsequent insolation analysis. Normally,'
            h(4)='these do not need to be retained and you may'
            h(5)='delete them.'
            call askok(' ','Delete zone transitional shading file(s)?',
     &                 ok,dok,5)
            irecw=ms+5
            if(ok)then
               do 55 ix=1,ncomp
                  do 56 im=1,12
                     write(lname,'(A,A3)')
     &                      zname(ix)(1:lnblnk(zname(ix))),month(im)
                     call efopran(iunit,lname,irecw,0,ier)
                     if(ier.eq.0)then
                        write(outs,'(2a)') 'Deleting ',lname
                        call usrmsg(outs,' ','-')
                        call efdelet(iunit,istat)
                     endif
   56             continue
   55          continue
            endif
         endif
         if(journio.eq.1)then
            call tstamp('>','Finished shading & insolation analysis.')
            close(iuj)
         endif
         call epagend
         stop

C Not a legitimate menu choice.
      else
         inos=-1
         goto 201
      endif
      goto 201

  901 call edisp(iuout,'Error opening journal file, continuing.')
      goto 902
   30 call edisp(iuout,'Error checking if file contained data.')
      goto 201

      end
      
C ********** SHDPRB **********
C Scans the system configuration file for the files that define
C the problem or allows user input.

      subroutine shdprb(itc,silent,icomp,ier)
#include "building.h"

      common/spad/mmod,limit,limtty
      common/outin/iuout,iuin
      common/pophelp/h(60)
      common/filep/ifil
      common/g0/ctype(mcom),gversion(mcom),igupgrade
      common/g1/x(mtv),y(mtv),z(mtv),nsur,jvn(ms,mv),nver(ms),ntv
      common/c2/lsnam,nccode(mcom),lproj(mcom),lgeom(mcom),
     &          lshad(mcom),lthrm(mcom),indutl(mcom),lutil(mcom)
      common/c20/nzsur(mcom),nztv(mcom)
      common/indics/ivf(mcom),isi(mcom),ihc(mcom),
     &              itw(mcom),icgc(mcom),iobs(mcom)
      common/udesc/lview(mcom),lhcco(mcom),
     &             ltwin(mcom),lcgcin(mcom),zobs(mcom)
      common/gzonpik/izgfoc,nzg,nznog(mcom)
      common/ray3/modify,modlen,modbnd

      character*72 lsnam,lproj,lgeom,lshad,lthrm,lutil
      character*72 lview,lhcco,ltwin,lcgcin,zobs
      character h*72,ctype*4
      logical modify,modlen,modbnd,silent
      logical newgeo  ! Used to test for new/old geometry file.

      iuf=ifil+1
      newgeo=.false.  ! Assume old format geometry.

      call eclose(gversion(icomp),1.1,0.01,newgeo)
      if(silent)then
         if(newgeo)then
            call georead(iuf,lgeom(icomp),icomp,1,0,iuout,ier)
         else
            call egomin(iuf,lgeom(icomp),icomp,1,0,iuout,ier)
         endif
      else
         if(newgeo)then
            call georead(iuf,lgeom(icomp),icomp,1,1,iuout,ier)
         else
            call egomin(iuf,lgeom(icomp),icomp,1,1,iuout,ier)
         endif
      endif
      if(ier.ne.0)then
         call edisp(iuout,'Problem detected while trying to open/read')
         call edisp(iuout,'zone geometry file. Respecify via menu.')
         return
      endif
      nzsur(icomp)=nsur
      nztv(icomp)=ntv

C Get name of obstruction file.
      if(iobs(icomp).eq.1)then
         if(silent)then
            call egomst(iuf,zobs(icomp),0,0,iuout,ier)
         else
            call egomst(iuf,zobs(icomp),0,1,iuout,ier)
         endif
      elseif(iobs(icomp).ne.1)then
         if(silent)then
            call edisp(iuout,
     &             'No obstructions so only compute insolation.')
         else
            h(1)='Where obstructions are not associated with a zone,'
            h(2)='external surface shading cannot be calculated. Note'
            h(3)='that it is still possible to calculate internal'
            h(4)='surface insolation.'
            call easkab('No Zone Obstructions file found!','Options:',
     &                  'cancel','compute insolation only',iw,4)
            if(iw.EQ.1)then
               call epagend
               stop
            endif
         endif
      endif

C Get name of the zone shading & insolation db.
      if(isi(icomp).eq.1)then
         call sifile(icomp)
      elseif(isi(icomp).ne.1)then
         call usrmsg('No Zone Shading & Insolation file found!',
     &               'Establish one via the Project Manager.','W')
         call usrmsg('The Shading & Insolation module',
     &               'is now terminating.','-')
         call pauses(1)
         call epagend
         stop
      endif

C Determine it's bounds for future comparison. Force it to be drawn.
      if(mmod.eq.8)then
         nzg=1
         nznog(1)=icomp
         izgfoc=icomp
         call esczone(icomp)
         call bndobj(0,ier)
         call erczone(icomp)
         modify=.true.
         modlen=.true.
         modbnd=.true.
         nzg=1
         nznog(1)=icomp
         izgfoc=icomp
         call adjview(itc,ier)
      endif
      return
      end

C ********** IMGDISP **********
C Dummy routine (needed for call back from c.

      subroutine imgdisp(iforce,focus,ier)

      character focus*4
     
      return
      end

C In support of floating point exceptions.
      integer function SIGFPE_bps(sig,code,context)
      integer sig,code,context(5)
      character label*16
      if(loc(code).eq.208) label = 'invalid'
      if(loc(code).eq.200) label = 'division by zero'
      if(loc(code).eq.212) label = 'overflow'
      if(loc(code).eq.204) label = 'underflow'
      if(loc(code).eq.196) label = 'inexact'
      write(6,*) 'Exception code ',loc(code),label,'at ',context(4)
      SIGFPE_bps = loc(code)
      end
