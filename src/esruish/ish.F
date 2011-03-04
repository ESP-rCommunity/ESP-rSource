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
#include "model.h"

C geometry.h provides commons G0/G2/c20/precz.
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
C espriou.h provides currentfile, defdbfl exemfl.
#include "prj3dv.h"

C IEEE callback (Solaris)
#ifdef SUN
#ifdef F90
      external SIGFPE_bps      
      integer SIGFPE_bps      
#endif
#endif
      
      integer lnblnk  ! function definition

      common/outin/iuout,iuin
      common/tracech/icout
      common/filech/ixopen,ixunit,ixpunit
      common/filep/ifil
      common/tc/itc,icnt
      common/spad/mmod,limit,limtty
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/viewpx/menuchw,igl,igr,igt,igb,igw,igwh
      common/appw/iappw,iappx,iappy
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      common/set1/iyear,ibdoy,iedoy,ifday,iftime
      integer ifs,itfs,imfs
      common/gfont/ifs,itfs,imfs
      common/gzonpik/izgfoc,nzg,nznog(mcom)
      common/ray2/itdsp,itbnd,itept,itznm,itsnm,itvno,itorg,itsnr,
     &            itobs,ithls,ithlz,itgrd,grdis,itppsw
      common/ray3/modify,modlen,modbnd
      common/fopened/cfgok,mldbok,matdbok,ctlok,optkok
      common/contr/mon,isc(ms),iyd
      common/data1/ical,idifc,init
      common/mtfile/ltrns,multic,mons,monf
      
      integer ncomp,ncon
      common/c1/ncomp,ncon
      common/c21/ifcfg,cfgroot,lcfgf

C Path to model and command line file (if any).
      common/rpath/path
      common/rcmd/lcmdfl

C Indicator of possible focus zone and action to take.
      common/rzone/inzone
      common/ract/paction

      common/prec8/slat,slon

C Project journal on/off, unit number, cmd, file name.
      common/journopt/journio,iuj,journcmd,jfile

C Defaults.
      common/deflt2/dfcfg,dfctl,defrlb,dafres,daprob,dpnf
      
C Significant figure reporting limit.
      common/sfig/nsigfig
      common/exporttg/xfile,tg,delim

C Name of current application.
      common/appname/cAppName

      dimension month(12),items(24),isadd(12),ishd(12),iavdec(12)
      character*72 lname,ltrns,tbase
      character*72 lcfgf
      character items*26,cAppName*12
      character month*3,outs*124,outs248*248,etext*70,ltmp*144
      character descrh*5,descrd*5,inf*144,lcmdfl*144
      character path*72,cfgroot*24,outstr*124
      character inz*16,inzone*16,zn*12,pact*16,paction*16
      character ascif*72,pascif*72
      character*72 dfcfg,dfctl,defrlb,daprob,dafres,dpnf
      character journcmd*20,jfile*72,uname*24,fs*1,cjfile*72
      character xfile*144,tg*1,delim*1,mode*4
      character exportfile*72 ! for ascii shading file
      logical silent,unixok,there,context,existing
      logical modify,modlen,modbnd,xst
      logical ok,dok,cfgok,mldbok,matdbok,ctlok,optkok
      logical user_recalc  ! user requested recalculation
      logical loaded_existing  ! to signal working with existing shd file
      logical changed_existing  ! to signal existing shd file updated

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer lnshd   ! length of current shading file string.
      integer numberofzones ! to pass to updwire rather than ncomp
      integer ier
      integer nitems,inos ! max items and current menu item

#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      data iavdec/17,15,16,15,15,11,17,16,16,16,15,11/
      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &           'Sep','Oct','Nov','Dec'/

C Initialize global common data.
      call ezero
      call curmodule('ish')
      cAppName = 'ish'
      helpinapp='ish'  ! set once for the application
      helpinsub='ish'  ! set for MAIN
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
      IFMUL=IFIL+4    ! set unit numbers for contstruction db.
      IFMAT=IFIL+5    ! set unit numbers for material db.
      iairp=ifil+6    ! for alt files during cfg read
      IOPTDB=IFIL+7   ! set unit numbers for optical db.
      iuj=ifil+10
      cfgok=.false.
      lname='SHDTRN'
      zn='UNKNOWN'
      user_recalc=.false.
      changed_existing=.false.
      
      call parsish(modl,iappw,iappx,iappy,inf,inz,pact,pascif)

C Set sky type to isotropic as default (idifc = 0): diffuse shading
C is then calculated only once per day rather than at each time step
C if the sky is anisotropic (idifc = 1).
      idifc=0

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
#ifdef SUN
#ifdef F90
      ieeer=ieee_handler('set','common',SIGFPE_ABORT)
      ieeer=ieee_handler('set','common',SIGFPE_bps)
      if(ieeer.ne.0) write(iuout,*) ' IEEE_handler not set !'
#endif
#endif

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
        igw=igr-igl
        igwh=igb-igt
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call opencpw
          call opensetup
          numberofzones=0
          call updwire(numberofzones)  ! pass local integer
          call updcapt(1)
          call updazi(1)
        endif
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
         if(pact(1:13).eq.'update_silent'.or.
     &      pact(1:16).eq.'useupdate_silent')then
           continue
         else
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
         if(pact(1:13).eq.'update_silent'.or.
     &      pact(1:16).eq.'useupdate_silent')then
           continue
         else
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
C If paction= useupdate_silent then import existing ascii file if
C available otherwise do standard annual calculation. If paction
C is 'asci2bin' then create an empty binary and then import ascii
C data and then exit. If paction is 'bin2asci' then scan the binary
C and then call the ascii export facility and exit.
      if(pact(1:1).ne.' ')then
         paction=pact
         write(outs,'(2a)')'Ish: the input action is: ',paction
         call tstamp('>',outs)
         if(paction(1:11).eq.'recalculate')then
            write(outs,'(2a)')' the action is: ',paction
            call edisp(iuout,outs)
            user_recalc=.true.
         elseif(paction(1:13).eq.'update_silent')then
            silent=.true.
         elseif(paction(1:16).eq.'useupdate_silent')then
            silent=.true.
         elseif(paction(1:8).eq.'asci2bin')then
	    write(ascif,'(a)') pascif(1:lnblnk(pascif))
            if(ascif(1:4).eq.'UNKN')then
              call edisp(iuout,'ish: the action file is unknown!')
              call pauses(1)
              call epagend
              stop
            endif
            silent=.true.
         elseif(paction(1:8).eq.'bin2asci')then
	    write(ascif,'(a)') pascif(1:lnblnk(pascif))
            if(ascif(1:4).eq.'UNKN')then
              call edisp(iuout,'ish: the action file is unknown!')
              call pauses(1)
              call epagend
              stop
            endif
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
      
      if(.NOT.silent)then
      
C Not in silent recalculation mode so present application version
C information.
        CALL ESPrVersion("summary",cAppName,IUOUT)
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

C If an input file has been specified then load it and force it
C to also fill data structures for obstructions.
      lcfgf=' '
      xst=.false.
      if(lcmdfl(1:2).ne.'  '.and.lcmdfl(1:4).ne.'UNKN')then
         inquire(file=lcmdfl,exist=xst)
         if(xst)then
            call fdroot(lcmdfl,path,lcfgf)
            if(paction(1:13).eq.'update_silent'.or.
     &         paction(1:16).eq.'useupdate_silent')then
              continue
            else
              call usrmsg('Scanning the model...',' ','-')
            endif
            iuout=6     ! reset in case corrupted
            mode='ALL '
            call ersys_mmode(lcfgf,ifcfg,iairp,mode,0,ier,silent)

C Open core of databases if not already done so.
            if(MLDBOK.and.MATDBOK.and.OPTKOK)then
              continue
            else
              call opendb(iier) ! return different error state for db.
              if(iier.ne.0)then
                call usrmsg('Possible problem with the Constructions',
     &               'or Optical Properties db.','W')
              endif
            endif
	    
            if(paction(1:13).eq.'update_silent'.or.
     &         paction(1:16).eq.'useupdate_silent')then
              continue
            else
              call usrmsg('Scanning the model...done.',' ','-')
            endif
            if(ier.eq.0)then
               cfgok=.true.
               numberofzones=ncomp
               call zdata (itrc,ier,numberofzones,isfsum)
               if(mmod.EQ.8)then
                 numberofzones=ncomp   ! use local integer for updwire.
                 call updwire(numberofzones)
               endif

C If inzone = 'All' display an image of the model. If inzone is
C either a string which matches a zone name or is an index, then
C focus on that zone. If inzone = UNKNOWN or the index 0 present
C a list.
               call zindex(inzone,index)
               if(index.le.0)goto 201
               icomp = index
               zn=zname(icomp)

C Set up zone transitional shading file based on zone name.
C Set logical status flags based on returned value of istat.
               write(lname,'(a)')zname(icomp)(1:lnzname(icomp))
               ok=.true.
               ltrns=lname
               itc=0
               istat=0

C Read in the model files taking into account the current value
C of paction (so that we do not do a recursive asci->bin->ascii.
               call shdprb(silent,icomp,istat)
               if(istat.eq.0)then
                 loaded_existing=.true.
               elseif(istat.eq.1)then
                 loaded_existing=.false.
               elseif(istat.eq.8)then

C A new binary file was created from an existing ascii so
C the requested useupdate_silent task has been completed.
                 loaded_existing=.true.
                 if(paction(1:16).eq.'useupdate_silent')then
                   call pauses(1)
                   call epagend
                   stop
                 endif
               endif
            endif
         endif
      else

C There was no input file, at least clear the model common blocks.
        call clrprb
      endif

C If useupdate_silent and reached this point then there was either
C an existing binary shd file which was opened via shdprb and needs
C to be cleared and re-filled from an ascii shading file or there
C was a black binary file created. Guess the name of the ascii shading
C file for the current zone. If it exists then proceed as in
C asci2bin.
      if(silent.and.paction(1:16).eq.'useupdate_silent')then
        if(zn(1:4).eq.'UNKN')then
          call edisp(iuout,'A zone is required to convert file.')
          ier = 1
          call epagend
          stop 
        else
          lnshd=lnblnk(lshad(icomp))
          write(ascif,'(2a)') lshad(icomp)(1:lnshd),'a'
          call findfil(ascif,xst)
          if(xst)then
            if(isi(icomp).EQ.1)then
              maxrec=24
              if(nzsur(icomp).gt.24)maxrec=nzsur(icomp)
              call findfil(lshad(icomp),xst)
              if(xst)then
                call erpfree(ifilsi,istst)
                ier=0
                call efopran(ifilsi,lshad(icomp),maxrec,1,ier)
                write(outs,'(2a)') 'Deleting previous ',lshad(icomp)
                call edisp(iuout,outs)
                call efdelet(ifilsi,istat)
                call sifile(icomp,istat)
                if(istat.eq.0)then
                  continue
                elseif(istat.eq.1)then
                  continue
                else
                  call edisp(iuout,'problem setting up new file.')
                  ier = 1
                  call epagend
                  stop 
                endif
                write(outs,'(3a)') ' Reading ASCII file ',
     &            ascif(1:lnblnk(ascif)),'...'
                call edisp(iuout,outs)

C There are no files to cleanup so just convert and exit.
                call sifimport(icomp,ascif,ier)
                call pauses(1)
                call epagend
                stop     
              endif
            endif
          else

C The was no existing ascii file so signal need to recalculate shading.
            user_recalc=.true.
          endif
        endif
      endif

C If in conversion mode then call the relevant subroutine.
      if(silent.and.paction(1:8).eq.'asci2bin')then

C Passed an ascii file and expect a binary file to be created. If
C an existing db is found, removed it and open a new one before
C starting conversion.
         if(zn(1:4).eq.'UNKN')then
           call edisp(iuout,'A zone is required to convert file.')
           ier = 1
           call epagend
           stop 
         else
           if(isi(icomp).EQ.1)then
             maxrec=24
             if(nzsur(icomp).gt.24)maxrec=nzsur(icomp)
             call findfil(lshad(icomp),xst)
             if(xst)then
               call erpfree(ifilsi,istst)
               ier=0
               call efopran(ifilsi,lshad(icomp),maxrec,1,ier)
               write(outs,'(2a)') 'Deleting previous ',lshad(icomp)
               call edisp(iuout,outs)
               call efdelet(ifilsi,istat)
               call sifile(icomp,istat)
               if(istat.eq.0)then
                 continue
               elseif(istat.eq.1)then
                 continue
               else
                 call edisp(iuout,'problem setting up new file.')
                 ier = 1
                 call epagend
                 stop 
               endif
               write(outs,'(3a)') ' Reading ASCII file ',
     &           ascif(1:lnblnk(ascif)),'...'
               call edisp(iuout,outs)

C There are no files to cleanup so just convert and exit.
               call sifimport(icomp,ascif,ier)
               call pauses(1)
               call epagend
               stop     
             endif
           endif
         endif
      endif
      
      if(silent.and.paction(1:8).eq.'bin2asci')then
      
C Assumes that the binary file is scanned and the common blocks are
C filled prior to calling sifexp.
         if(zn(1:4).eq.'UNKN')then
           call edisp(iuout,'A zone is required to convert file.')
           ier = 1
           call epagend
           stop 
         else

C If loaded_existing has been set to true we probably can do the
C conversion. Note, when starting ish for this command line option
C any existing ascii file will be overwritten.
           write(outs,'(2a)') ' Creating ASCII file ',
     &       ascif(1:lnblnk(ascif))
           call edisp(iuout,outs)
           call sifexp(icomp,ascif,ier)
           call pauses(1)
           call epagend
           stop 
         endif
      endif
      
C If in recalculation mode, proceed with minimal interface. If
C an existing db is found, removed it and open a new one before
C starting calculations.
      if(silent.or.user_recalc)then
         if(isi(icomp).EQ.1)then
            maxrec=24
            if(nzsur(icomp).gt.24)maxrec=nzsur(icomp)
            call findfil(lshad(icomp),xst)
            if(xst)then
               call erpfree(ifilsi,istst)
               ier=0
               call efopran(ifilsi,lshad(icomp),maxrec,1,ier)
               write(outs,'(2a)') 'Deleting previous ',lshad(icomp)
               if(paction(1:13).eq.'update_silent'.or.
     &            paction(1:16).eq.'useupdate_silent')then
                 continue
               else
                 call edisp(iuout,' ')
                 call edisp(iuout,outs)
               endif
               call efdelet(ifilsi,istat)
               call sifile(icomp,istat)
               if(istat.eq.0)then
                 continue
               elseif(istat.eq.1)then
                 continue
               else
                 call edisp(iuout,'problem with setting up new file.')
               endif
            endif
          
C Proceed with shading and then insolation calculations.
            if(iobs(icomp).gt.0)then
               write(outs,'(3a)')'Shading calculations for ',
     &           zname(icomp)(1:lnzname(icomp)),'...'
               if(paction(1:13).eq.'update_silent'.or.
     &            paction(1:16).eq.'useupdate_silent')then
                 continue
               else
                 call edisp(iuout,outs)
               endif
               call tstamp('>','Ish: re-calculate shading')
               call shdcon(icomp,ier,'s')
               changed_existing=.true.  ! so we know to update ASCII on exit
               write(outs,'(3a)')'Shading calculations for ',
     &           zname(icomp)(1:lnzname(icomp)),'...done.'
               if(paction(1:13).eq.'update_silent'.or.
     &            paction(1:16).eq.'useupdate_silent')then
                 continue
               else
                 call edisp(iuout,outs)
               endif
            else
               call edisp(iuout,
     &           'No obstructions, calculating insolation.')
            endif
            icstat=0
            write(outs,'(3a)')'Insolation calculations for ',
     &        zname(icomp)(1:lnzname(icomp)),'...'
            if(paction(1:13).eq.'update_silent'.or.
     &         paction(1:16).eq.'useupdate_silent')then
              continue
            else
              call edisp(iuout,outs)
            endif
            call tstamp('>','Ish: start insolation computation')
            call inscon(icomp,icstat,'s')
            changed_existing=.true.  ! so we know to update ASCII on exit
            write(outs,'(3a)')'Insolation calculations for ',
     &        zname(icomp)(1:lnzname(icomp)),'...done.'
            if(paction(1:13).eq.'update_silent'.or.
     &         paction(1:16).eq.'useupdate_silent')then
              continue
            else
              call edisp(iuout,outs)
            endif
         else
            call usrmsg('Unknown Zone Shading & Insolation file.',
     &                  'Cannot recalculate!','W')
         endif
         if(paction(1:13).eq.'update_silent'.or.
     &      paction(1:16).eq.'useupdate_silent')then

C Finished silent recalculation so exit program, delete the transitional
C shading files. If a single month calculation has been done then
C offer to save or delete. If a multi-month calculation has been
C done, ask user to delete the transitional files. Note that the
C logic loops through all of the zones, some of which may not
C have transitional files. Those that do not will return a -301
C ier state from efopran call and thus non-existant files will
C not be deleted.
            iunit=ifil
            if(multic.eq.0)then
               call efdelet(iunit,istat)
            elseif(multic.eq.2)then
               irecw=ms+5
               do 155 ix=1,ncomp
                  do 156 im=1,12
                     write(lname,'(a,a3)')
     &                    zname(ix)(1:lnzname(ix)),month(im)
                     ier=0  ! reset ier in case prev file did not exist
                     call efopran(iunit,lname,irecw,0,ier)
                     if(ier.eq.0)call efdelet(iunit,istat)
 156              continue
 155           continue
               call pauses(1)
            endif
         endif

C Computations finished, write out the ascii version of the database.
         lnshd=lnblnk(lshad(icomp))
         write(exportfile,'(2a)') lshad(icomp)(1:lnshd),'a'
         if(paction(1:13).eq.'update_silent'.or.
     &      paction(1:16).eq.'useupdate_silent')then
           continue
         else
           write(outs,'(2a)') ' Creating ASCII file ',
     &       exportfile(1:lnblnk(exportfile))
           call edisp(iuout,outs)
         endif
         call sifexp(icomp,exportfile,ier)
         call pauses(1)
 
C Allow user to quit manually for a recalculate directive but
C auto exit for update_silent.
         if(paction(1:13).eq.'update_silent'.or.
     &      paction(1:16).eq.'useupdate_silent')then
           call epagend
           stop
         endif
      endif
      
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
      if(idifc.eq.0)then
         items(9)=              'f sky type << isotropic   '
      elseif(idifc.eq.1)then
         items(9)=              'f sky type << anisotropic '
      endif
      items(10)=                'g calculate shading       '
      items(11)=                'h shading synopsis        '
      items(12)=                'i shadow image            '
      items(13) =               '  _______________________ '
      items(14)=                'j calculate insolation    '
      items(15)=                'k insolation synopsis     '
      items(16) =               '  _______________________ '
      items(17)=                'l shd/ins db contents     '
      items(18)=                'm shd/ins export & import '
      items(19) =               '  _______________________ '

C If user toggles trace a 2nd time then we are in detailed mode
      if(itc.eq.0)then
         items(20)=             'm trace output >> none    '
      elseif(itc.eq.1)then
         items(20)=             'm trace output >> fort.33 '
      elseif(itc.eq.2)then
         items(20)=             'm trace output+ >> fort.33'
      endif
      if(ixopen.eq.1)then
         items(21)='> outputs >> file       '
      elseif(ixopen.eq.0)then
         items(21)='> outputs >> screen     '
      endif
      items(22)=                '! browse zone surfaces    '
      items(23)=                '? help                    '
      items(24)=                '- exit Shading/Insolation '

C If zone has not been selected disallow calculations and views.
      if(zn(1:4).eq.'UNKN')then
         items(10) =             'g  ...                    '
         items(11)=              'h  ...                    '
         items(12)=              'i  ...                    '
         items(14)=              'j  ...                    '
         items(15)=              'k  ...                    '
         items(17)=              'l  ...                    '
         items(18)=              'm  ...                    '
      endif
      nitems = 24

C Use askabout to instantiate the initial help messages
C (2nd parameter is 1).
      call askabout('ish ',1)
      call emenu('Shading & Insolation',items,nitems,inos)

C Request definition of model.
      if(inos.eq.1)then
         helptopic='ish_model_cfg_file'
         call gethelptext(helpinsub,helptopic,nbhelp)
         ltmp=lcmdfl

C Call easkf depending on the current file name length. The X11
C version will be returning only the name of the file, while the
C GTK version will be returning the name with the full path.
  289    llt=lnblnk(ltmp)
         iglib = igraphiclib()  ! find out if X11, GTK or text only.
         if(iglib.eq.1.or.iglib.eq.3)then
            if(llt.lt.96)then
               call easkf(ltmp,'Model Configuration file?',' ',
     &           96,dfcfg,'config file name',ier,nbhelp)
            elseif(llt.ge.96.and.llt.lt.124)then
               call easkf(ltmp,'Model configuration file?',' ',
     &           124,dfcfg,'config file name',ier,nbhelp)
            elseif(llt.ge.124.and.llt.le.144)then
               call easkf(ltmp,'Model configuration file?',' ',
     &           144,dfcfg,'config file name',ier,nbhelp)
            endif
         elseif(iglib.eq.2)then
            call easkf(ltmp,'Model configuration file?',' ',
     &        144,dfcfg,'config file name',ier,nbhelp)
         else
            call easkf(ltmp,'Model configuration file?',' ',
     &        96,dfcfg,'config file name',ier,nbhelp)
         endif

         if(ltmp(1:2).eq.'  '.or.ltmp(1:4).eq.'UNKN')goto 289
         call st2file(ltmp,lcmdfl)
         inquire (file=lcmdfl,exist=xst)
         if(xst)then
            call fdroot(lcmdfl,path,lcfgf)
            call tstamp('>','Ish: scanning new model')
            call tstamp('>',lcfgf)
            mode='ALL '
            call ersys_mmode(lcfgf,ifcfg,iairp,mode,0,ier,silent)
            if(MLDBOK.and.MATDBOK.and.OPTKOK)then
              continue
            else
              call opendb(iier) ! return different error state for db.
              if(iier.ne.0)then
                call usrmsg('Possible problem with the Constructions',
     &               'or Optical Properties db.','W')
              endif
            endif
            if(ier.eq.0)then
               cfgok=.true.
               numberofzones=ncomp
               call zdata (itrc,ier,numberofzones,isfsum)
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
         write(outs,'(2a)')'Ish: shift zone focus to: ',zname(icomp)
         call tstamp('>',outs)

C If there is an existing transitional shading file with 
C the zone's name as root, ask whether to use it.
         write(lname,'(a)')zname(icomp)(1:lnzname(icomp))
         ltrns=lname
         istat=0
         call shdprb(silent,icomp,istat)
         if(istat.eq.0)then
           loaded_existing=.true.
         elseif(istat.eq.1)then
           loaded_existing=.false.
         elseif(istat.eq.8)then
           loaded_existing=.true.
         endif

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
         helptopic='view_from_sun_or_plan'
         call gethelptext(helpinsub,helptopic,nbhelp)
         call easkab(' ','View:','plan','from sun',iw,nbhelp)

C After drawing the site plan, the zone geometry file is reread 
C to reconstruct /G1/,/G2/ and /G5/.
         if(iw.eq.1)then
            call tstamp('>','Ish: enter site plan')
            call sitpln(icomp)
               call georead(iuf,lgeom(icomp),icomp,0,iuout,ier)
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
               call adjview(ier)

C Generate a heading for the view.
               call edtime(stime,descrh,descrd,timer)
               write(etext,'(a,i2,1x,a3,a,a5,a,f6.1,a,f5.1)')
     &            'View @ ',ido,month(imo),', ',descrh,
     &            ' Sun position - azimuth ',sazi,', altitude ',salt
               iside=1
               isize=1
               ifont=ifs
               call viewtext(etext,iside,isize,ifont)
               call easkab(' ','View:','next hour','exit',iw,nbhelp)
               if(iw.eq.1)then
                  stime=stime+((60.0/float(nts))/60.0)
                  goto 68
               else
                  itsnm=0
                  goto 201
               endif
            else
               call easkab('View point below horizon!',' ',
     &                     'Specify another time?','exit',iw,nbhelp)
               if(iw.eq.1)goto 67
               itsnm=0
            endif
         endif

C Toggle sky type.
      elseif(inos.eq.9)then
         helptopic='what_kind_of_sky'
         call gethelptext(helpinsub,helptopic,nbhelp)
         call easkab(' ','Sky type:','isotropic','anisotropic',
     &     iw,nbhelp)
C Isotropic sky.
         if(iw.eq.1)then
            idifc=0
C Anisotropic sky.
         elseif(iw.eq.2)then
            idifc=1
         endif

C Calculate direct and diffuse shading on specified external surfaces.
      elseif(inos.eq.10)then
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
               helptopic='request_to_overwrite'
               call gethelptext(helpinsub,helptopic,nbhelp)
               call easkabc('Zone Shading & Insolation db has data!',
     &           'Options:','update','create new db','cancel',
     &           iw,nbhelp)
               if(iw.eq.1)then
                  continue
               elseif(iw.eq.2)then
                  maxrec=24
                  if(nzsur(icomp).gt.24)maxrec=nzsur(icomp)
                  call erpfree(ifilsi,istst)
                  ier=0
                  call efopran(ifilsi,lshad(icomp),maxrec,1,ier)
                  write(outs,'(2a)') 'Deleting previous ',lshad(icomp)
                  if(paction(1:13).eq.'update_silent'.or.
     &               paction(1:16).eq.'useupdate_silent')then
                    continue
                  else
                    call edisp(iuout,' ')
                    call edisp(iuout,outs)
                  endif
                  call efdelet(ifilsi,istat)
                  call sifile(icomp,istat)
                  if(istat.eq.0)then
                    continue
                  elseif(istat.eq.1)then
                    continue    ! probably not the time to look for an ascii version
                  else
                    call edisp(iuout,
     &                'problem with setting up new file.')
                  endif
               elseif(iw.eq.3)then
                  goto 201
               endif
            endif
            call tstamp('>','Ish: start shading computation')
            call shdcon(icomp,ier,'i')
            changed_existing=.true.  ! so we know to update ASCII on exit
         else
            call usrmsg(
     &       'No Zone Shading & Insolation db! Use Project Manager',
     &       'to specify one for this zone or choose another zone.',
     &       'W')
         endif
      elseif(inos.eq.11)then

C Synopsis of surface shading calculation results (uses information held
C in the zone transitional shading file or from common blocks).
         if(zn(1:4).eq.'UNKN')then
            call edisp(iuout,'Select a zone first!')
            goto 201
         else
            helptopic='synopsis_period_options'
            call gethelptext(helpinsub,helptopic,nbhelp)
            call tstamp('>','Ish: shading synopsis')
            mon=mons
  202       call easki(mon,' ','Computation month number?',
     &        1,'F',12,'F',1,'month number',ier,nbhelp)
            if(ier.ne.0)goto 201
            call eday(iavdec(mon),mon,iyd)
	    
C If there is a transitional shading file then use it when composing
C the synopsis. If not found (i.e. if binary file is based on reading
C an ascii shading file) then use an alternative approach.

C Allow for number of surfaces within the zone transitional shading
C file.
            irecw=ms+5
            write(tbase,'(a,a3)')ltrns(:lnblnk(ltrns)),month(mon)
            ier=0
            call efopran(iunit,tbase,irecw,1,ier)
            if(ier.eq.0)then

C Shading synopsis is based on information in the zone transitional
C shading file.
              call edisp(iuout,' ')
              write(outs,'(3a)')'Zone transitional shading file ',
     &          tbase(:lnblnk(tbase)),' used.'
              call edisp(iuout,outs)
              call retrv1(icomp)
              call shsynp(icomp)
	       
            else

C Shading synopsis is based on common block information.
              call sifrd(icomp,mon,ier)
              call shadsynp(icomp)
            endif

            if(mon.lt.12)then
               call easkab(' ','Consider another month?','yes',
     &                     'no',iw,nbhelp)
               if(iw.eq.1)then
                  mon=mon+1
                  goto 202
               endif
            endif
         endif

C Plot surface shading patterns using information in zone transitional
C shading file.
      elseif(inos.eq.12)then
         if(zn(1:4).eq.'UNKN')then
            call edisp(iuout,'Select a zone first!')
            goto 201
         else
            helptopic='graph_period_options'
            call gethelptext(helpinsub,helptopic,nbhelp)
   66       mon=mons
  203       call easki(mon,' ','Computation month number?',
     &        1,'F',12,'F',1,'month number',ier,nbhelp)
            if(ier.ne.0)goto 66
            call eday(iavdec(mon),mon,iyd)

C Allow for number of surfaces within the zone transitional shading
C file.
            irecw=ms+5
            write(tbase,'(a,a3)')ltrns(:lnblnk(ltrns)),month(mon)
            ier=0
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
     &                     'no',iw,nbhelp)
               if(iw.eq.1)then
                  mon=mon+1
                  goto 203
               endif
            endif
         endif

C Calculate insolation of internal surfaces due to specified external 
C source surfaces.
      elseif(inos.eq.14)then
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
               helptopic='request_to_overwrite'
               call gethelptext(helpinsub,helptopic,nbhelp)
               call easkabc('Zone Shading & Insolation db has data!',
     &           'Options:','update','create new db','cancel',
     &           iw,nbehlp)
               if(iw.eq.1)then
                  continue
               elseif(iw.eq.2)then
                  maxrec=24
                  if(nzsur(icomp).gt.24)maxrec=nzsur(icomp)
                  call erpfree(ifilsi,istst)
                  ier=0
                  call efopran(ifilsi,lshad(icomp),maxrec,1,ier)
                  write(outs,'(2a)') 'Deleting previous ',lshad(icomp)
                  if(paction(1:13).eq.'update_silent'.or.
     &               paction(1:16).eq.'useupdate_silent')then
                    continue
                  else
                    call edisp(iuout,' ')
                    call edisp(iuout,outs)
                  endif
                  call efdelet(ifilsi,istat)
                  call sifile(icomp,istat)
                  if(istat.eq.0)then
                    continue
                  elseif(istat.eq.1)then
                    continue
                  else
                    call edisp(iuout,
     &                'problem with setting up new file.')
                  endif
               elseif(iw.eq.3)then
                  goto 201
               endif
            endif
            call tstamp('>','Ish: start insolation computation')
            icstat=0
            call inscon(icomp,icstat,'-')
            changed_existing=.true.  ! so we know to update ASCII on exit
         else
            call usrmsg(
     &       'No Zone Shading & Insolation db! Use Project Manager',
     &       'to specify one for this zone or choose another zone.',
     &       'W')
         endif

C Synopsis of insolation calculation results (based on information
C held in the zone shading & insolation db.)
      elseif(inos.eq.15)then
         if(zn(1:4).eq.'UNKN')then
            call edisp(iuout,' ')
            call edisp(iuout,'Select a zone first!')
            goto 201
         else
            call tstamp('>','Ish: list insolation synopsis')
            mon=mons
            helptopic='synopsis_period_options'
            call gethelptext(helpinsub,helptopic,nbhelp)
  204       call easki(mon,' ','Month number?',
     &        1,'F',12,'F',1,'ins comp month',ier,nbhelp)
            if(ier.ne.0)goto 201
            call eday(iavdec(mon),mon,iyd)

            call sifrd(icomp,mon,ier)
            call insynp(icomp,'i')   ! allow user to control surfaces seen
            if(mon.lt.12)then
               call easkab(' ','Consider another month?','yes',
     &                     'no',iw,nbhelp)
               if(iw.eq.1)then
                  mon=mon+1
                  goto 204
               endif
            endif
         endif

C Edit/import (interactive) /export (interactive) /list.
      elseif(inos.eq.17)then
         if(zn(1:4).eq.'UNKN')then
            call edisp(iuout,'Select a zone first!')
            goto 201
         else
            call sifops(icomp)
         endif

C Export or import ASCII (silent).
      elseif(inos.eq.18)then
         if(zn(1:4).eq.'UNKN')then
            call edisp(iuout,'Select a zone first!')
            goto 201
         else
           helptopic='export_import_choice'
           call gethelptext(helpinsub,helptopic,nbhelp)
           call easkabc('Zone Shading & Insolation db has data!',
     &       'Options:','export ascii','import ascii','cancel',
     &       iw,nbhelp)
           lnshd=lnblnk(lshad(icomp))
           if(iw.eq.1)then
             write(exportfile,'(2a)') lshad(icomp)(1:lnshd),'a'
             call easks(exportfile,' ','Export file name?',72,
     &         'siexp.txt','shdins export',ier,nbhelp)
             write(outs,'(2a)') ' Creating ASCII file ',
     &         exportfile(1:lnblnk(exportfile))
             call edisp(iuout,outs)
             call sifexp(icomp,exportfile,ier)
             call pauses(1)
           elseif(iw.eq.2)then
             write(exportfile,'(2a)') lshad(icomp)(1:lnshd),'a'
             call easks(exportfile,' ','Import file name?',72,
     &         'siexp.txt','shdins import',ier,nbhelp)
             write(outs,'(3a)') ' Reading ASCII file ',
     &         exportfile(1:lnblnk(exportfile)),'...'
             call edisp(iuout,outs)
             call sifimport(icomp,exportfile,ier)
             call pauses(1)
           elseif(iw.eq.3)then
             continue
           endif
         endif

C Set trace level displayed in opening menu and main menu.
      elseif(inos.eq.20)then
         itc=itc+1
         if(itc.gt.2)then
           itc=0
           icout=iuout
         elseif(itc.eq.1.or.itc.eq.2)then
           icout=33
         endif

C Redirect synoptic outputs and result of ASHRAE method to a
C user specified file.
      elseif(inos.eq.21)then
         if(ixopen.eq.0)then
            write(xfile,'(2a)')zname(icomp)(1:lnzname(icomp)),'.txt'  ! initial file name
         endif
         call edisp(iuout,' ')
         call ctlexp(xfile,ixopen,ixloc,ixunit,'T','Report',ier)

C Surface summary: print header, followed by surface information.
      elseif(inos.eq.22)then
         call tstamp('>','Ish: list surface attributes')
         context=.false.
         call surinfo(icomp,iuout,context)

C Help via the askabout facility in esru_ask.F - 0 requests display.
      elseif(inos.eq.23)then
         call askabout('ish ',0)

C Exit program: if nothing has been done, delete the transitional
C shading file; if a single or multi-month calculation has been done
C then offer to save or delete.
      elseif(inos.eq.24)then
         iunit=ifil
         if(multic.eq.0)then
            call efdelet(iunit,istat)
         elseif(multic.eq.2)then
            dok=.true.
            helptopic='request_to_delete_trans'
            call gethelptext(helpinsub,helptopic,nbhelp)
            call askok(' ','Delete zone transitional shading file(s)?',
     &        ok,dok,nbhelp)
            irecw=ms+5
            if(ok)then
               do 55 ix=1,ncomp
                  do 56 im=1,12
                     write(lname,'(A,A3)')
     &                 zname(ix)(1:lnzname(ix)),month(im)
                     ier=0  ! reset ier in case prev file did not exist
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

C When exiting write out the ascii version of the database.
         lnshd=lnblnk(lshad(icomp))
         if(lshad(icomp)(1:4).eq.'UNKN'.or.
     &      lshad(icomp)(1:2).eq.'  '.or.
     &      lnshd.le.1.or.lnshd.ge.72)then
            continue
         else
         
C If user is browsing we may not want to write ascii file
C If the user has just opened an existing database but not
C changed it then we do not need to write it out.
           if(loaded_existing)then
             if(changed_existing)then
               write(exportfile,'(2a)') lshad(icomp)(1:lnshd),'a'
               write(outs,'(2a)') ' Creating ASCII file ',
     &          exportfile(1:lnblnk(exportfile))
               call edisp(iuout,outs)
               call sifexp(icomp,exportfile,ier)
               call pauses(1)
             else
               continue
             endif
           else
             write(exportfile,'(2a)') lshad(icomp)(1:lnshd),'a'
             write(outs,'(2a)') ' Creating ASCII file ',
     &          exportfile(1:lnblnk(exportfile))
             call edisp(iuout,outs)
             call sifexp(icomp,exportfile,ier)
             call pauses(1)           
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
C the problem or allows user input. Depending on what happend
C with the file pass back an indicator:
C  ifstat = 0  no errors and existing file was opened correctly
C  ifstat = 1  new file was opened correctly
C  ifstat = 2  no shading file name so exited
C  ifstat = 3  error opening the shading file
C  ifstat = 4  error reading header of file
C  ifstat = 5  error writing record 4 of file
C  ifstat = 6  user cancel
C  ifstat = 7  error opening geometry file
C  ifstat = 8  new file filled via import of ascii file

      subroutine shdprb(silent,icomp,ifstat)
#include "building.h"
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      common/spad/mmod,limit,limtty
      common/outin/iuout,iuin
      common/filep/ifil

      common/gzonpik/izgfoc,nzg,nznog(mcom)
      common/ray3/modify,modlen,modbnd
      common/ract/paction
      integer iobstrtrace  ! set to non-zero if already written
      common/obstrc/iobstrtrace(mcom,mb)

      character exportfile*72,paction*16,outs*124

      logical modify,modlen,modbnd,silent,xst

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='shdprb'  ! set for subroutine

      iuf=ifil+1

      if(silent)then
            call georead(iuf,lgeom(icomp),icomp,1,iuout,ier)
         else
            call georead(iuf,lgeom(icomp),icomp,1,iuout,ier)
         endif
      if(ier.ne.0)then
         call edisp(iuout,'Problem detected while trying to open/read')
         call edisp(iuout,'zone geometry file. Respecify via menu.')
         ifstat=7
         return
      endif

C Get name of obstruction file.
      if(iobs(icomp).eq.1)then
         continue   ! any obstructions have already been scanned.
      elseif(iobs(icomp).eq.2)then
         continue   ! any obstructions are in the geometry file.
      elseif(iobs(icomp).eq.0)then
         if(silent)then
            call edisp(iuout,
     &             'No obstructions so only compute insolation.')
         else
            helptopic='obs_not_associated'
            call gethelptext(helpinsub,helptopic,nbhelp)
            call easkab('No Zone Obstructions file found!','Options:',
     &                  'cancel','compute insolation only',iw,nbhlep)
            if(iw.EQ.1)then
               ifstat=6
               call epagend
               stop
            endif
         endif
      endif

C Clear trace information for obs block coordinates.
      if(nbobs(icomp).gt.0)then
        do 42 ib=1,nbobs(icomp)
          iobstrtrace(icomp,ib)=0
  42    continue
      endif

C Because they will be used many times derive the azimuth elevation
C and tilt of all surfaces in the zone (fill common pangl).
      do 20 is=1,nzsur(icomp)
        call pangs(icomp,is)
  20  continue

C Get name of the zone shading & insolation db.
      if(isi(icomp).eq.1)then
         call sifile(icomp,istat)
         if(istat.eq.0)then
           ifstat=0
         elseif(istat.eq.1)then

C A new binary file was created and filled with zeros. Check and
C see if an ascii version of the file exists with the same name
C as the shading file but ending with shda. If so try and scan it
C as long as the user request is not recalculate or update_silent
C or if the user requested a file conversion.
           ifstat=1
           if(paction(1:11).eq.'recalculate')then
             continue
           elseif(paction(1:13).eq.'update_silent')then
             continue
           elseif(paction(1:8).eq.'bin2asci')then
             continue
           elseif(paction(1:8).eq.'asci2bin')then
             continue
           else
	     write(exportfile,'(2a)') 
     &         lshad(icomp)(1:lnblnk(lshad(icomp))),'a'
             call findfil(exportfile,xst)
             if(xst)then
               write(outs,'(3a)') ' Reading ASCII file ',
     &           exportfile(1:lnblnk(exportfile)),'...'
               call edisp(iuout,outs)
               call sifimport(icomp,exportfile,ier)
               call pauses(1)
               ifstat=8   ! signal import was done
             endif
           endif
         else
           ifstat=istat
           call edisp(iuout,'problem with setting up new file.')
         endif
      elseif(isi(icomp).ne.1)then
         call usrmsg('No Zone Shading & Insolation file found!',
     &               'Establish one via the Project Manager.','W')
         call usrmsg('The Shading & Insolation module',
     &               'is now terminating.','-')
         ifstat=2
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
         call adjview(ier)
      endif
      return
      end

C ********** IMGDISP **********
C Dummy routine (needed for call back from c.

      subroutine imgdisp(iforce,focus,ier)

      character focus*4
     
      return
      end

C ********** opendb
C NOTE: This is based on code in esruprj/eddb.F but takes into account
C the value of paction when ish was initially called.

C Open materials constructions, multi-layer constructions and
C optical properties databases. In the case of materials, first
C assume it is a binary file, check its contents and if a problem
C then scan new ascii format and if that does not work try the
C older ascii materials file to fill the materials data arrays.
C If sucessful the material common blocks will be filled and
C closemat1 or closemat2 will be set.

C << update this to match changes in lfmat >>

      subroutine opendb(ier)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      common/ract/paction
      character paction*16

      logical XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical closemat1,closemat2
      character LASCI*144,DFILE*144,fs*1
      character SOPT*12,outs*124,outs248*248,GDESCR*36
      character lworking*144
      integer lndbp   ! for length of standard database path
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
        
C Read multilayer db information into common. If in update_silent mode
C ish might be called during install installation and as there are no
C materials database but a local constructions database there will be
C warnings which require an interaction and so treat update_silent as
C a special case with no interaction.
      CALL ERPFREE(IFMUL,ISTAT)

C Depending on location check existance.
      if(ipathmul.eq.0.or.ipathmul.eq.1)then
        lworking=lfmul  ! use as is
      elseif(ipathmul.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    lfmul(1:lnblnk(lfmul))  ! prepend db folder path
      endif
      call FINDFIL(lworking,XST)
      if(XST)then
        if(iuout.eq.0) iuout=6  ! reset in case lost
        CALL ERMLDB(0,IUOUT,IER)
        if(paction(1:13).eq.'update_silent'.or.
     &     paction(1:16).eq.'useupdate_silent')then
          if(IER.eq.1)then
            MLDBOK=.FALSE.
          elseif(IER.eq.2)then
            MLDBOK=.FALSE.
          endif
	  goto 138    ! if silent and no materials jump to optics
	endif
        IF(IER.eq.1)then
          write(outs248,'(3a)') 
     &    ' Problems with materials used by Constructions db',
     &     LFMAT(1:lnblnk(LFMAT)),'!'
          call edisp248(iuout,outs248,100)
          call edisp(iuout,' ')
          MLDBOK=.FALSE.
        ELSEIF(IER.eq.2)then

C There were lots of undefined materials so likely the Materials
C database is for the wrong machine type. Guess the name of
C the ascii version and if it exists, offer to convert it or use it.
          helptopic='warning_about_bin_mat'
          call gethelptext(helpinsub,helptopic,nbhelp)
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

C If the ascii version exists there is no need to convert it.
C Ask whether to scan it into the materials arrays.
            call rascimat(IAF,LASCI,IER)  ! try current ascii format
            if(ier.eq.-2)then
              ier=0
              call rlegacymat(IAF,LASCI,ier)  ! try older ascii format
              if(ier.ne.0)then
                call usrmsg('No readable Materials database was',
     &                      'found or the file was corrupt.','W')
                MATDBOK=.FALSE.
              else
                call eclose(matver,1.1,0.001,closemat1)
                call eclose(matver,1.2,0.001,closemat2)
                if(closemat1.or.closemat2)then

C Materials data in place, set this as the new file name and then
C try and re-scan the Constructions database (using lworking).
                  call usrmsg(
     &              'An ASCII Materials db was found and used.',
     &              'Re-scanning constructions...','P')
                  MATDBOK=.TRUE.
                  write(LFMAT,'(a)') LASCI(1:lnblnk(LASCI))
                  CALL ERPFREE(IFMUL,ISTAT)

C Open the expanded location of LFMUL.
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
C try and re-scan the constructions database (via lworking).
              call usrmsg(
     &          'An ASCII Materials db was found and used.',
     &          'Re-scanning constructions...','P')
              MATDBOK=.TRUE.
              write(LFMAT,'(a)') LASCI(1:lnblnk(LASCI))
              CALL ERPFREE(IFMUL,ISTAT)

C Again use the expanded path.
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

C Ask user for ascii Materials database to convert.
            IAF=IFIL+1
            DFILE=' '
            CALL EASKS(LASCI,' ','Materials database (ASCII)?',
     &        144,DFILE,'materials db (ascii)',IER,nbhelp)
            goto 155
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

C Open optical Properties db and read into common.
 138  SOPT='ALL'
      CALL EROPTDB(0,iuout,SOPT,GDESCR,IER)
      if(ier.ne.0)then
        call usrmsg('Optical Properties db not found or',
     &              'there was a problem reading it!','W')
        OPTKOK=.FALSE.
      else
        OPTKOK=.TRUE.
      endif

      return
      end



C In support of floating point exceptions.
#ifdef SUN
#ifdef F90
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
#endif
#endif
