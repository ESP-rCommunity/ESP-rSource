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

C Project Manager: file prj.F comprising
C   MAIN: does what it says on the box.
C   simula: commissions technical assessments and analysis.
C   checkmodel: checks model prior to commissioning a simulation.
C   imgdisp: displays images associated with start-up or at
C   specific points.
C   CFGVER: supports the creation of model variants.
C   VERMAN: copies various ESP-r files and names them uniquely
C          in order to build multiple variants of a base case model
C   FNCNGR: changes the name of file ORIGNAM by appending APP

C ********** MAIN
      program prj
#include "building.h"
#include "model.h"

C geometry.h provides commons G0 and G2.
#include "geometry.h"
#include "uncertainty.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure databases)
C LOPTDB,IOPTDB (for optical database)
C LPCDB/IPCDB (for plant template database)
C LPRFDB,IPRODB (for event profile database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
C ICLIM,LCLIM (for climate file)
C LSBEM (SBEM database)
C default file names for databases
#include "material.h"
#include "espriou.h"
C espriou.h provides currentfile, defdbfl exemfl.
#include "control.h"
#include "net_flow.h"
#include "prj3dv.h"
#include "ipvdata.h"
#include "sbem.h"
#include "schedule.h"
      PARAMETER (MSTMC=20)
      
      integer lnblnk  ! function definition

C IEEE callback (Solaris)
#ifdef SUN
#ifdef F90
      external SIGFPE_bps      
      integer SIGFPE_bps      
#endif
#endif

      common/initv/initvt,EYEMI(3),VIEWMI(3),ANGI
      common/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      integer ifs,itfs,imfs
      common/GFONT/IFS,ITFS,IMFS
      common/FILEP/IFIL
      common/appw/iappw,iappx,iappy
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      common/C6/INDCFG
      common/C21/IFCFG,cfgroot,LCFGF
      common/CFGV/icfgv
      common/cctlnm/ctldoc,lctlf

C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

C Project log file, journal on/off, unit number, cmd, file name.
      common/LOG/LPRJLG
      common/journopt/journio,iuj,journcmd,jfile

C Ground topology.
      common/GTFIL/GTGEOM

C Path to model and command line file (if any). Browse
C is a logical flag, .true. restricts update/save options.
      common/rpath/path
      common/rcmd/LCMDFL

      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

C Indicator of possible focus zone.
      common/rzone/inzone
      common/user/browse

C Redirected text/graphics parameters.
      common/exporttgi/ixopen,ixloc,ixunit

C Where ESP-r was installed (as recorded when it was compiled).
      common/deflt4/dinstpath

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton

C Radiance RIF file.
      common/radcfg/LRADCF

C IPV description via ipvdata.h.
      common/IPVF/lipvdatf

      common/pophelp/h(60)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2
      common/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/gzonpik/izgfoc,nzg,nznog(mcom)
      common/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      common/RAY3/MODIFY,MODLEN,MODBND
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Uncertainty.
      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)

C IDST tool commands
      common/idsta/CMDCFG,CMDCLM,CMDCLMN,CMDNOTE,CMDRES,CMDLBL(10),
     &             CMDLNAM(10)
      common/idstb/nidst
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)

      COMMON/BIDIRFL/bidirfile,bidirname(MSTMC)
      COMMON/Vld20/Vldtng

C Name of current application
      common/APPNAME/cAppName
      character cAppName*12

      logical OK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,MODSIT
      logical MODIFY,MODLEN,MODBND,browse,confirm,XST,ckpath
      logical unixok,isdok,Vldtng,changedit
      logical itisanexemplar  ! passed to newprb to ensure browse mode.

      character dstmp*24,uname*24
      character LUALF*72,LCNG*15,LLOC*15
      character*72 LCFGF,H,LPRJLG
      character LTMP*72
      character inf*144,inz*16,inzone*16,act*16,inact*16,LCMDFL*144
      character outs*124,outstr*124
C      character outs248*248
      character path*72,cfgroot*24,exemplar*144
      character LAPROB*72
      character ITEMS(27)*34
      character ltcmdfl*144
      character ctldoc*248,lctlf*72,ETEXT*82,lradcf*72
      character journcmd*20,jfile*72,tfile*72,cjfile*72
      character GTGEOM*72,lipvdatf*72,longtfile*144
      character*10 wkd1, wkd2
      character CMDCFG*72,CMDCLM*72,CMDCLMN*72,CMDNOTE*64,CMDRES*72
      character ALOC*12,ALOCLBL*12,ALOCTYP*4,CMDLBL*12,CMDLNAM*80
      CHARACTER cpth*48,cmdn*48,topt*72
      character t24*24,DFILE*72,DCNN*72,bidirfile*72,bidirname*12
      CHARACTER dinstpath*48,dirpath*72
      character useraction*8  ! returned from exemplar access call.
      character menu*72       ! blank model title for pregist call
      character fs*1

C Passed parameters for pregist.
      character root*24,mpath*72,folder*12

C Returned parameters from silentread.
      character theprimedirective*8

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      real d1,d2,d3   ! data used with transfor or rotate command line
      integer ivals   ! holds list of zones to be rotated or transformed.
      integer numberofzones ! to pass to updwire rather than ncomp
      integer MITEM,INO ! max items and current menu item
      dimension ivals(MCOM)
      logical silent  ! for dependency checking on transform or rotate

#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

C Initialize variables and recover command line arguments.
      call ezero
      call curmodule('prj ')
      cAppName = 'prj'
      ITRC=0
      IUOUT=6
      IUIN=5
      LIMTTY=24
      LIMIT =24
      IFIL=11
      iuj=IFIL+10

C Recover command line parameters (terminal, size, config file, zone
C focus and actions arguments).
      call parsprj(MODL,iappw,iappx,iappy,inf,inz,act,d1,d2,d3)
      IFDAY=2
      IFTIME=0
      nsset=0
      isset=0
      isstup=0
      isbnstep=1
      ispnstep=2
      issave=2
      isavgh=0
      NSIGFIG=3

C Initial assumptions about databases (prior to configuration file scan
C that the default database files will be defined explicitly).
      matver=0.0   ! initial assumption of binary materials database
      ipathapres=0; ipathoptdb=0; ipathpcdb=0; ipathprodb=0
      ipathmat=0; ipathmul=0; ipathclim=0; ipathsbem=0; ipathmsc=0
      
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
      write(tmppth,'(a1,a1)')'.',fs
      write(dbspth,'(a1,a1)')'.',fs
      write(hvacpth,'(a1,a1)')'.',fs
      IYEAR=2007
      IBDOY=1
      IEDOY=365
      cfgroot=' '
      LCTLF=' '
      LCNN=' '
      LAPROB='  '
      lradcf='UNKNOWN'
      GTGEOM='UNKNOWN'
      LUALF='UNKNOWN'
      lipvdatf='UNKNOWN'
      LASBEM='UNKNOWN'  ! assume no UK NCM description
      nipvassmt=0  ! to signal initially no IPV defined
      dmdsdesc='no dispersed demands notes (yet)'
      bdmds='UNKNOWN'
      bidirfile='UNKNOWN'

C Assume weekends are Saturday/ Sunday unless re-defined.
      idwe1=6
      idwe2=7
      wkd1='Saturday'
      wkd2='Sunday'

C Set upgrade files flag to zero (no opinion yet)
      igupgrade=0

C Clear number of images and allow image browsing.
      noimg=0
      iton=0
      NALOC= 0
      nidst= 0

C Initialise coordinates for eye point, view point and angle of view.
      EYEM(1)=-100.
      EYEM(2)=-100.
      EYEM(3)=100.
      VIEWM(1)=10.
      VIEWM(2)=10.
      VIEWM(3)=10.
      ANG=40.

C General image option flags.
      ITDSP=0
      ITBND=1
      ITEPT=0
      ITZNM=0
      ITSNM=1
      ITVNO=1
      ITORG=0
      ITSNR=1
      ITOBS=0
      ITGRD=0
      GRDIS=0.0
      ITPPSW=0
      IFS=1
      ITFS=1
      IMFS=1

C Assume that the system configuration, multi-layered db, material db
C and control files have not been read in and that any files saved will
C be atrributed.
      CFGOK=.FALSE.
      MLDBOK=.FALSE.
      MATDBOK=.FALSE.
      CTLOK=.FALSE.
      OPTKOK=.FALSE.
      browse=.false.
      Vldtng=.false.
      changedit=.false.
      MODSIT=.false.  ! assume model context has not changed.

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

C Initialise output device, assume minimal trace and a standard
C display.  If passed zero size and offsets use default, if size
C is <200 then take % of default, otherwise use passed width.
C If left & top offsets are 0 then use defaults.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=630
        iappx=40
        iappy=40
      else
        if(iappx.lt.0)iappx=40
        if(iappy.lt.0)iappy=40
        if(iappw.le.200)then
          iappwi=int(630*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(36*iappw*0.01),20)
        LIMTTY= MAX0(int(10*iappw*0.01),6)
        LIMIT = MAX0(int(10*iappw*0.01),6)
      else
        menuchw = 36
        LIMTTY=10
        LIMIT =10
      endif
      IF(MMOD.EQ.8)THEN

C Set initial font sizes (IMFS is for graphs, IFS is for dialog & text feedback).
        IMFS=1
        IFS=1
        ITFS=1
        call userfonts(IFS,ITFS,IMFS)
      ELSE
        LIMTTY=16
        LIMIT =16
      ENDIF
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &'ESP-r Project Manager: enquiries to esru@strath.ac.uk')

C Open the text display box equal to LIMTTY if MMOD = 8.  Model
C not yet know so pass 0 to the wireframe control routine (updwire).
C If starting as a reduced percentage of default then widen the
C graphic display to compensate.
      IF(MMOD.EQ.8)THEN
        CALL win3d(menuchw,4,1,1,3,igl,igr,igt,igb,igw,igwh)
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
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      ENDIF

C Generate the version report.
      CALL ESPrVersion("summary",cAppName,IUOUT)

C Find the user's home folder then get user's custom settings
C if defined. If there is no .esprc file in the users home folder
C then the esp-r install folders will be checked by scesprc and
C esprc may be updated if the alternate version is found.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IFIL+5,0,IIER)

C Debug.
C      write(6,*) 'upath',upath
C      write(6,*) 'esprc',esprc

C If there is a journal active set this up (put journal file in the
C users home folder as a 'dot' file in the form .user_name_pid_number.
C Use st2file to remove any blanks and wild cards. Save the journal
C file name in a scratch dot file (to be removed when prj exits).
      if(journio.eq.1)then
        uname=' '
        call usrname(uname)
        call esppid(ipid)
        write(tfile,'(a,a1,a,a,i7)')upath(1:lnblnk(upath)),fs,'.',
     &    uname(1:lnblnk(uname)),ipid
        call st2file(tfile,jfile)

C Open a file which holds info on the parent journal file, make
C tmp use of iuj unit number.
        write(cjfile,'(5a)')upath(1:lnblnk(upath)),fs,'.',
     &    uname(1:lnblnk(uname)),'cur_j'
        open(iuj,file=cjfile,status='UNKNOWN',err=900)
        write(iuj,'(a,a)')'Current_Journal,',jfile(1:lnblnk(jfile))
        call dstamp(dstmp)
        write(iuj,'(2a)')'Date,',dstmp
        write(iuj,'(2a)')'User,',uname(1:lnblnk(uname))
        close(iuj)

C Write header of the journal file and then close.
        open(iuj,file=jfile,status='UNKNOWN',err=901)
        write(iuj,'(a,a)')'Journal for:',uname(1:lnblnk(uname))
        call dstamp(dstmp)
        write(iuj,'(a,a)')'Date: ',dstmp
        close(iuj)
      endif
      call tstamp('>','Started Project Manager')
  902 continue

C Scan the defaults file for default system configuration file.
C Make temporary use of file unit IFIL+5.  Note: escdef must come
C after scan of .esprc file.
      call escdef(IFIL+5,IER)

C Take command line file name as initial configuration file.
      if(inf(1:2).eq.'  '.or.inf(1:4).eq.'UNKN')then
        LCMDFL='UNKNOWN'
      else
        LCMDFL=inf
      endif

C Take command line zone name, convert to 'inzone'
C and determine if focus is a single zone.
      if(inz(1:2).ne.'  ')then
        inzone=inz
      else
        inzone='ALL'
      endif

C If command line also included a key word, file name or data after -act
C then set inact.
      if(act(1:2).ne.'  ')then
        inact=act
      else
        inact='NONE'
      endif

C Climate db is on channel IFIL (NB when not in use other binary
C databases may make use of this unit temporarily).
      ICLIM=IFIL
      LCLIM=DCLIM

      write(LAPRES,'(a)') DAPRES(1:lnblnk(DAPRES))

C Multi-layer constructions db on channel IFIL+3.
      IFMUL=IFIL+3
      write(LFMUL,'(a)') DFMUL(1:lnblnk(DFMUL))

C Materials db on channel IFIL+4.
      IFMAT=IFIL+4
      write(LFMAT,'(a)') DFCON(1:lnblnk(DFCON))

C System configuration file on channel IFIL+5. 
      IFCFG=IFIL+5
      LCFGF='UNKNOWN'
      modeltitle='UNKNOWN'

C Mass flow network, profiles and optical dbs on channel IFIL+6. 
      IOPTDB=IFIL+6
      write(LOPTDB,'(a)') DOPTDB(1:lnblnk(DOPTDB))

      IPRODB=IFIL+6

C Temporal information on channel IFIL+7.
      IUTDF=IFIL+7
      IUTDFA=IFIL+8
      write(LTDF,'(a)') 'UNKNOWN'
      write(LTDFA,'(a)') 'UNKNOWN'
      ITDFLG=0

      write(LPRFDB,'(a)') DPRFDB(1:lnblnk(DPRFDB))

      write(LPCDB,'(a)') DPCDB(1:lnblnk(DPCDB))

C Special materials/miscel component database.
      write(MCMPDBFL,'(a)') DMCMPDBFL(1:lnblnk(DMCMPDBFL))

C Text/graphic feedback redirection on channel ifil +9 
      ixunit = ifil + 9

C Project log.
      LPRJLG='job.notes'

C If -actf was included in the command line then see if it
C is a known command that can be acted on.
      XST=.false.
      if(inact(1:10).eq.'silentread')then

C Check to see if the -file parameter is a high level silent input file
C and if so do what it says.
        if(LCMDFL(1:2).ne.'  '.and.LCMDFL(1:4).ne.'UNKN')then
          INQUIRE (FILE=LCMDFL,EXIST=XST)
          if(XST)then
            call clrprb  ! clear the model common blocks prior to scan
            write(longtfile,'(a)') LCMDFL(1:lnblnk(LCMDFL))
            IUNIT=IFIL+1
            call silentread(iunit,longtfile,'-',theprimedirective,ier)
            call edisp(iuout,theprimedirective)
            call edisp(iuout,modeltitle)
            call edisp248(iuout,modeldocblock,80)
            if(theprimedirective(1:3).eq.'new')then
              call usrmsg('Processing of silent file complete.',
     &          'Exiting from project manager','P')

C Clear allocatable arrays
              CALL DeallocateAllArrays
              CALL EPAGEND
              STOP
            else
              goto 3
            endif
          endif
        endif
      elseif(inact(1:9).eq.'transform'.or.inact(1:6).eq.'rotate')then
        XST=.false.
        if(LCMDFL(1:2).ne.'  '.and.LCMDFL(1:4).ne.'UNKN')then
          INQUIRE (FILE=LCMDFL,EXIST=XST)
          if(XST)then

C Call newprb without the need to confirm model but to check its path.
C We assume that the user owns the model to be transformed.
            itisanexemplar=.false.
            confirm=.false.
            ckpath=.true.
            call NEWPRB(ITRC,confirm,ckpath,itisanexemplar,IER)
            if(ier.eq.-3)then
              call usrmsg('Configuration file could not be loaded.',
     &          'Exiting from project manager','W')
C Clear allocatable arrays
              CALL DeallocateAllArrays
              CALL EPAGEND
              STOP
            endif

C Open core of databases if not already done so.
            if(MLDBOK.and.MATDBOK.and.OPTKOK)then
              continue
            else
              call opendb(ier)
              if(ier.ne.0)then
                call usrmsg('Possible problem with the Constructions',
     &               'or Optical Properties db. Exiting.','W')
C Clear allocatable arrays
              CALL DeallocateAllArrays
                CALL EPAGEND
                STOP
              endif
            endif

C Scan control file if one exists.
            ICTLF=IFIL+1
            CALL ERPFREE(ICTLF,ISTAT)
            call FINDFIL(LCTLF,XST)
            if(XST)then
              CALL EZCTLR(ICTLF,0,IUOUT,IER)
            endif
	    
C Now implement the transform based on data passed in command line
C for all the zones in the model. NOTE: in the next line set the
C value of silent = .false if you want to interact with the transform
C or rotation.
C            silent= .false.
            silent= .true.
	    inpic=NCOMP
	    do 29 iz=1,inpic
	      ivals(iz)=iz
  29        continue
  
C Perform either the transform or rotation.
            if(inact(1:9).eq.'transform')then
              call globaltransform(d1,d2,d3,inpic,ivals,
     &          silent,itru,ier)
            elseif(inact(1:6).eq.'rotate')then
              call globalrotate(d1,d2,d3,inpic,ivals,silent,itru,ier)
	    endif
            if(silent)then 
              call usrmsg(
     &          'Processing of transform or rotate complete.',
     &          'Exiting from project manager','W')

C Clear allocatable arrays
              CALL DeallocateAllArrays
              CALL EPAGEND
              STOP
            else
              goto 3
            endif
          endif
        endif      
      endif

C If an input file has been specified then load it.
      XST=.false.
      if(LCMDFL(1:2).ne.'  '.and.LCMDFL(1:4).ne.'UNKN')then
        INQUIRE (FILE=LCMDFL,EXIST=XST)
        if(XST)then

C Check to see if the command line file is an IDST command file.
C If it is, extract configuration file name and command data.
          isdok=.false.
          CALL ERPFREE(IFCFG,ISTAT)
          call FPOPEN(IFCFG,ISTAT,1,1,LCMDFL)
          CALL STRIPC(IFCFG,OUTSTR,0,ND,1,'IDST header',IER)
          if(OUTSTR(1:14).eq.'*IDST COMMANDS')then
            isdok=.true.
            cpth=' '
            cmdn=' '
 22         CALL STRIPC(IFCFG,OUTSTR,0,ND,0,'idst tags',IIER)
            if(iier.ne.0)then

C Debug.
C              write(6,*)'idst naloc =',nidst,naloc

              goto 23
            endif
            if(OUTSTR(1:9).eq.'*abs_path')then
              k=10
              CALL EGETRM(OUTSTR,K,cpth,'W','idst cfg path',IER)
              goto 22
            elseif(OUTSTR(1:8).eq.'*win_cfg'.or.
     &             OUTSTR(1:8).eq.'*trn_cfg'.or.
     &             OUTSTR(1:8).eq.'*sum_cfg')then
              k=9
              CALL EGETRM(OUTSTR,K,cmdn,'W','config',IER)
              if(cpth(1:2).ne.'  ')then
                write(CMDCFG,'(a,a)') cpth(1:lnblnk(cpth)),
     &            cmdn(1:lnblnk(cmdn))
              else
                write(CMDCFG,'(a)') cmdn(1:lnblnk(cmdn))
              endif
              write(LCMDFL,'(a)') CMDCFG(1:lnblnk(CMDCFG))
              goto 23
            endif
            goto 22
          endif
  23      CALL ERPFREE(IFCFG,ISTAT)

C Call newprb without the need to confirm model but to check its path.
C Assume that the file passed is not an exemplar.
          itisanexemplar=.false.
          confirm=.false.
          ckpath=.true.
          call NEWPRB(ITRC,confirm,ckpath,itisanexemplar,IER)
          if(ier.eq.-3)then
            ier=0
            goto 3  ! cancel detected, redisplay menu
          endif

C          call edisp(iuout,'* Loading supplied model description.')
          call tstamp('>','PRJ: loading supplied model')
          call tstamp('>',LCFGF)
        else

C The model file might not exist. Request the path is checked.
C Assume that the file name passed is not an exemplar.
          itisanexemplar=.false.
          confirm=.false.
          ckpath=.true.
          call NEWPRB(ITRC,confirm,ckpath,itisanexemplar,IER)
          if(ier.eq.-3)then
            ier=0
            goto 3  ! cancel detected, redisplay menu
          endif

C          call edisp(iuout,'* Loading supplied model description.')
          call tstamp('>','PRJ: loading supplied model')
          call tstamp('>',LCFGF)
        endif
      endif

C Provide site information, update wireframe control with
C current number of zones if a model has been loaded.
      if(LCMDFL(1:2).ne.'  '.and.LCMDFL(1:4).ne.'UNKN')then
        if(ier.eq.0)then
          IF(MMOD.EQ.8)then
            numberofzones=ncomp
            call updwire(numberofzones)  ! pass local integer
            call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
            call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
            WRITE(etext,'(2A)')'Model: ',
     &        modeltitle(1:lnblnk(modeltitle))
            iside=1
            isize=1
            ifont=1
            call viewtext(etext,iside,isize,ifont)
          endif
          CFGOK=.TRUE.
          call siteinfo(iuout)

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

C Scan control file if one exists.
          ICTLF=IFIL+1
          CALL ERPFREE(ICTLF,ISTAT)
          call FINDFIL(LCTLF,XST)
          if(XST)then
            CALL EZCTLR(ICTLF,0,IUOUT,IER)
          endif

C If there are associated images and the image browser
C has not been invoked do this now.
          call imgdisp(0,'****',ier)

C If 'inzone' = 'All' display an image of the model. If 'inzone'
C is either a string which matches a zone name or is an index then
C focus on that zone.  If 'inzone' = 'UNKNOWN' or 0 present a list.
          call zindex(inzone,index)
          if(index.le.0)then
            if(indcfg.ne.2.and.indcfg.ne.0)then
              MODIFY=.TRUE.
              MODBND=.TRUE.
              MODLEN=.TRUE.
              nzg=NCOMP
              DO 44 I=1,nzg
                nznog(I)=I
  44          CONTINUE

C (Re)Set all surfaces to standard line width and if an initial view
C has been specified update view info to this.
              CALL INLNST(1)
              izgfoc=0
              if(initvt.eq.1)then
                EYEM(1)=EYEMI(1)
                EYEM(2)=EYEMI(2)
                EYEM(3)=EYEMI(3)
                VIEWM(1)=VIEWMI(1)
                VIEWM(2)=VIEWMI(2)
                VIEWM(3)=VIEWMI(3)
                ANG=ANGI
              endif

C Re-read the command line file as an IDST command file and extract data.
C Package code within isdok block into a separate subroutine
C to clarify code and allow for future evolution.
              if(isdok)then
                 call IPVCMD('-',inf)
              endif

C Pass information to C library code if running in graphic mode.
              CALL ADJVIEW(IER)
              if(MMOD.EQ.8)then
                call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
                call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
                WRITE(etext,'(2A)')'Model: ',
     &            modeltitle(1:lnblnk(modeltitle))
                iside=1
                isize=1
                ifont=1
                call viewtext(etext,iside,isize,ifont)
              endif
            endif
            if(MMOD.EQ.8)then
              call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
              call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
              WRITE(etext,'(2A)')'Model: ',
     &          modeltitle(1:lnblnk(modeltitle))
              iside=1
              isize=1
              ifont=1
              call viewtext(etext,iside,isize,ifont)
            endif
          else
            itru=iuout
            call georead(IFIL+1,LGEOM(index),index,1,iuout,IER)
            IF(IER.NE.0)goto 3

            CALL EDZONE(ITRC,index,IER)
            MODIFY=.TRUE.
          endif
        endif
      endif

      ITRC=1
    3 INO=-4
      ITEMS(1) =  'a introduction to ESP-r'
      ITEMS(2) =  'b database maintenance'
      ITEMS(3) =  'c validation testing'
      ITEMS(4) =  ' .... Model selection ........'
      ITEMS(5) =  'd open existing'
      ITEMS(6) =  'e create new'

C Debug.
C      write(6,*) ' model management browse status ',browse

      if(CFGOK)then
        if(browse)then
          ITEMS(7) =  ' .... Current model (browse) ....'
          ITEMS(13)=  'm browse/ simulate'
        else
          ITEMS(7) =  ' .... Current model ..........'
          ITEMS(13)=  'm browse/ edit/ simulate'
        endif
        WRITE(ITEMS(8),'(2A)') '  cfg  : ',LCFGF(1:22)
        WRITE(ITEMS(9),'(2A)') '  path : ',path(1:22)
        WRITE(ITEMS(10),'(2A)')'g root : ',cfgroot(1:22)
        WRITE(ITEMS(11),'(2A)')'h title: ',modeltitle(1:22)
        WRITE(ITEMS(12),'(A)') 'j variants'
        ITEMS(14)=  '                            '
      else
        ITEMS(7) =    ' .... Current model (none) ....'
        WRITE(ITEMS(8),'(A)')  '  cfg  :         '
        WRITE(ITEMS(9),'(A)')  '  path :         '
        WRITE(ITEMS(10),'(A)') '  root :         '
        WRITE(ITEMS(11),'(A)') '  title:         '
        WRITE(ITEMS(12),'(A)') '  variants       '
        ITEMS(13)=  '                              '
        ITEMS(14)=  '                              '
      endif
      ITEMS(15)   = ' .... Import & export ........'
      ITEMS(16)   = 'n import CAD file                 '
      if(CFGOK)then
        ITEMS(17) = 'o export current model            '
        ITEMS(18) = 'p archive current model           '
      else
        ITEMS(17) = '  export                          '
        ITEMS(18) = '  archive                         '
      endif
      ITEMS(19)   = ' .... Model location ........'
      if(CFGOK)then
        ITEMS(20) = 't folders & files         '
      else
        ITEMS(20) = '  folders & files         '
      endif
      ITEMS(21)  =  ' .... Miscellaneous ........'
      if(CFGOK)then
        ITEMS(22) = 'r save model                      '
        ITEMS(23) = 's save model as                   '
      else
        ITEMS(22) = '  save model                      '
        ITEMS(23) = '  save model as                   '
      endif
      IF(ITRC.EQ.0)THEN
        ITEMS(24) = 'v feedback >> silent              '
      ELSEIF(ITRC.EQ.1)THEN
        ITEMS(24) = 'v feedback >> summary             '
      ELSEIF(ITRC.EQ.2)THEN
        ITEMS(24) = 'v feedback >> detailed            '
      ENDIF
      ITEMS(25) =   '* preferences                     '
      ITEMS(26) =   '? help                            '
      ITEMS(27) =   '- exit Project Manager            '
      MITEM=27

C If user has defined model and perhaps resized the display then
C redraw the model image. If in registration mode do not attempt
C to draw the model.
      if(indcfg.eq.2)then
        CALL USRMSG(' ',' ','-')
      elseif(indcfg.eq.0)then
        CALL USRMSG(' ',' ','-')
      else
        if(CFGOK.AND.MODIFY)then
          MODBND=.TRUE.
          MODLEN=.TRUE.
          ITSNM=1
          ITVNO=1
          nzg=NCOMP
          if(nzg.gt.0)then
            DO 444 I=1,nzg
              nznog(I)=I
  444       CONTINUE

C (Re)Set all surfaces to standard line width.
            CALL INLNST(1)
            izgfoc=0
            CALL ADJVIEW(IER)
          endif

C Re-draw the configuration buttons.
          if(MMOD.EQ.8)then
            call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
            call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
            WRITE(etext,'(2A)')'Model: ',
     &        modeltitle(1:lnblnk(modeltitle))
            iside=1
            isize=1
            ifont=1
            call viewtext(etext,iside,isize,ifont)
          endif
        endif
      endif

C Use askabout to instantiate the initial help messages (2nd parameter is one).
      call askabout('prj ',1)

C Present opening menu.
      CALL EMENU('Model Management',ITEMS,MITEM,INO)
      IF(INO.EQ.MITEM)THEN
        if(changedit)then
          h(1)='If you exit without saving, recent changes could be'
          h(2)='lost. If you selected this option in error select' 
          h(3)='cancel.'
          CALL EASKABC('There are unsaved changes in the model.',
     &      'Options:','exit (no save)','save & exit','cancel',IW,3)
          if(iw.eq.3)then
            goto 3
          elseif(iw.eq.2)then
            call tstamp('>','PRJ: update configuration')
            call tstamp('>',LCFGF)
            CALL EMKCFG('-',IER)
            OK=.true.
          elseif(iw.eq.1)then
            OK=.true.
            continue
          endif
        else
          call usrmsg('Closing Project Manager.',' ','P')
          ok=.true.
        endif
        IF(.NOT.OK)GOTO 3
        if(journio.eq.1)then
          call tstamp('>','Finish Project Manager')
          close(iuj)
          write(cjfile,'(5a)')upath(1:lnblnk(upath)),fs,'.',
     &      uname(1:lnblnk(uname)),'cur_j'
          open(iuj,file=cjfile,status='UNKNOWN',err=899)
          CLOSE(iuj,STATUS='DELETE')
        endif

C If there is a temporary optical file remove it.
        XST=.false.
        topt='tmpopt'
        call FINDFIL(topt,XST)
        if(XST)then
          IUF=IFIL+1
          CALL ERPFREE(IUF,ISTAT)
          CALL EFOPSEQ(IUF,topt,1,IER)
          CALL EFDELET(IUF,ISTAT)
        endif

C Clear allocatable arrays
        CALL DeallocateAllArrays
        CALL EPAGEND
        STOP
      ELSEIF(INO.EQ.1)THEN

C Introduction.
        H(1)='Welcome to the ESP-r Project Manager, a tool to'
        H(2)='help you specify, appraise and evolve your energy'
        H(3)='system designs.'
        H(4)=' '
        H(5)='If you are new to ESP-r, you might like to start'
        H(6)='by selecting the tutorial button and browsing the'
        H(7)='information available at www.esru.strath.ac.uk. There'
        H(8)='is also a paper manual included with the source code'
        H(9)='distribution.'
        H(10)=' '
        H(11)='Please contact ESRU at the email address shown in the'
        H(12)='header should you require further information or wish'
        H(13)='to suggest tool enhancements.'
        H(14)=' '
        H(15)='To list the invocation options supported by this tool'
        H(16)='enter `esp-r -help` at the command line prompt.'
        H(17)=' '
        H(18)='The version of prj supports models of the following'
        H(19)='(maximum) complexity: '
        write(H(20),'(a,i3)') 'Number of zones ',MCOM
        write(H(21),'(a,i3)') 'Surfaces per zone ',MS
        write(H(22),'(a,i3)') 'Edges per surface ',MC
        write(H(23),'(a,i4)') 'Surfaces in model ',MCON
        write(H(24),'(a,i3)') 'Zone control loops ',MCF
        write(H(25),'(a,i3)') 'Control day types ',MCDT
        write(H(26),'(a,i3)') 'Mass flow nodes  ',MNOD
        write(H(27),'(a,i3)') 'Mass flow components ',MCMP
        write(H(28),'(a,i3)') 'Mass flow connections ',MCNN
        write(H(29),'(a,i3)') 'Control day types ',MCDT
        write(H(30),'(a,i3)') 'Constructions in model ',MMLC
        H(31)=' '
        H(32)='If you need to work with more complex models then '
        H(33)='you will need to update the parameters in the '
        H(34)='source include folder, re-compile ESP-r and '
        H(35)='then test it on increasingly complex models. '
        CALL PHELPD('Introduction',35,'gen_esp_intr',0,0,IER)
        call tstamp('>','PRJ: intro help')
      ELSEIF(INO.EQ.2)THEN

C Manage databases.
        call tstamp('>','PRJ: db management enter')
        CALL EDDB(IER)
        call tstamp('>','PRJ: db management exit')
      ELSEIF(INO.EQ.3)THEN

C Manage validation. Does not need to have an existing model
C loaded << Check that this is true. >>
        call MValid
      ELSEIF(INO.EQ.5)THEN

C Open an exemplar or existing model.
        h(1)='The ESP-r system includes a number of example models'
        h(2)='that demonstrate different modelling capabilities.'
        h(3)='Alternatively, you can select another existing model,'
        h(4)='perhaps one you created earlier or have received from'
        h(5)='a colleague.'
        CALL EASKABC(' ','Model selection options:',
     &    'exemplar','other','cancel',IW,5)
        if(iw.eq.3)then
          continue
        elseif(iw.eq.1)then

C Scan exemplars file specified in the user's .esprc file (or from
C the standard file dinstpath'/training/exemplars'. If call to
C rexmpl returns an ier=2 then the user asked to continue.
          IUF=IFIL+1
          XST=.false.
          if(exemfl(1:2).ne.'  '.and.exemfl(1:4).ne.'UNKN')then
            INQUIRE (FILE=exemfl,EXIST=XST)
            if(XST)then
              call tstamp('>','PRJ: scanning exemplars enter')

C << An idea - rexmpl could return in addition the path and the cfg file  >>
C << name parts of exemplar (because it already knows this so it need not >>
C << be resolved again. >>

C Call rexmpl to get the exemplar. If ier is set to 2 then the
C user didn't pick anything. If useraction is set to 'browse' then
C ensure the toggle is set accordingly.
              useraction = ' '
              call rexmpl(IUF,exemplar,useraction,ier)
              call tstamp('>','PRJ: scanning exemplars return')
              if(ier.eq.2)then
                ier=0
                goto 3
              endif
              if(ier.ne.0)goto 3
              if(exemplar(1:7).ne.'UNKNOWN'.and.
     &           exemplar(1:2).ne.'  ')then

C Set initial browse toggle to match useraction.
                if(useraction(1:8).eq.'browse  ')then
                  browse=.true.
                elseif(useraction(1:8).eq.'ownit   ')then
                  browse=.false.
                endif

C Debug.
C                write(6,*) 'after exemplar selection browse ',browse

C Copy the exemplar configuration file (with full path) to LCMDFL.
                write(LCMDFL,'(a)') exemplar(1:lnblnk(exemplar))
                ltcmdfl=exemplar
                call fdroot(ltcmdfl,path,LCFGF)

C Debug...
C                write(outs248,'(2a)') '* LCMDFL is ',LCMDFL
C                call edisp248(iuout,outs248,110)
C                write(outs248,'(2a)') 'Model path is ',path
C                call edisp248(iuout,outs248,110)
C                write(outs248,'(2a)') 'Model cfg is ',LCFGF
C                call edisp248(iuout,outs248,110)
C                call edisp(iuout,' ')

C If the path of the model indicates it is one of the standard
C exemplars then flag browse and itisanexemplar to true.
                write(dirpath,'(3a)') dinstpath(1:lnblnk(dinstpath)),
     &            fs,'training'

                ldirpath=lnblnk(dirpath)
                if(path(1:ldirpath).eq.dirpath(1:ldirpath))then

C For OSX/Linux/Unix comparison of paths is usually successful so
C if the model path is to the distribution training folder believe it.
                  browse=.true.
                  itisanexemplar=.true.
                else

C A non match may happen in Native windows so fall back to user
C action if there is no match in the paths.
                  if(useraction(1:8).eq.'browse  ')then
                    browse=.true.
                    itisanexemplar=.true.
                  elseif(useraction(1:8).eq.'ownit   ')then
                    browse=.false.
                    itisanexemplar=.false.
                  endif
                endif

C Debug.
                write(outs,*) '* Browse status ',browse,itisanexemplar
                call edisp(iuout,outs)

C If running on a Windows machine the the file browser would have been
C used and there is no need to check the path.
                call isunix(unixok)
                confirm=.false.
                if(unixok)then
                  ckpath=.true.
                else
                  ckpath=.false.
                endif
                call NEWPRB(ITRC,confirm,ckpath,itisanexemplar,IER)
                if(ier.eq.-3)then
                  ier=0
                  goto 3  ! cancel detected, redisplay menu
                endif

                call tstamp('>','PRJ: continuing with exemplar')
              endif
            endif
          else
            call edisp(iuout,'No exemplars list available!')
            call edisp(iuout,' ')
            goto 3
          endif
        elseif(iw.eq.2)then

C Select an existing model. If Project Manager was started with
C 'UNKNOWN' then ask for actual name to create or read.
          if(LCMDFL(1:7).eq.'UNKNOWN')then
            itisanexemplar=.false.
            confirm=.true.
            ckpath=.true.
            call NEWPRB(ITRC,confirm,ckpath,itisanexemplar,IER)
            if(ier.eq.-3)then
              ier=0
              goto 3  ! cancel detected, redisplay menu
            endif
          else

C Something was passed in via the command line so offer choice.
            H(1)='The model configuration file holds the definition'
            H(2)='of the model to be simulated, including the names'
            H(3)='of the composing files.  A configuration file'
            H(4)='has been passed to the Project Manager as a command'
            H(5)='line argument.'
            H(6)=' '
            H(7)='If you selected this option in error click cancel. '
            write(outs,'(a,a)') ' The command line included: ',
     &      LCMDFL(1:lnblnk(LCMDFL))
            CALL EASKABC(outs,' ','use it as the model',
     &        'specify another','cancel',IW,7)

C User confirmed model so find the path and local file name. The
C variable dirpath is where esp-r training models are located.
            if(IW.eq.1)then
              ltcmdfl=LCMDFL
              call fdroot(ltcmdfl,path,LCFGF)
              write(dirpath,'(3a)') dinstpath(1:lnblnk(dinstpath)),
     &          fs,'training'
              ldirpath=lnblnk(dirpath)
              if(path(1:ldirpath).eq.dirpath(1:ldirpath))then
                browse=.true.
                itisanexemplar=.true.
              else
                itisanexemplar=.false.
              endif
              confirm=.false.
              ckpath=.true.
              call NEWPRB(ITRC,confirm,ckpath,itisanexemplar,IER)
              if(ier.eq.-3)then
                ier=0
                goto 3  ! cancel detected, redisplay menu
              endif
            elseif(IW.eq.2)then
              itisanexemplar=.false.
              confirm=.true.
              ckpath=.true.
              call NEWPRB(ITRC,confirm,ckpath,itisanexemplar,IER)
              if(ier.eq.-3)then
                ier=0
                goto 3  ! cancel detected, redisplay menu
              endif
            elseif(IW.eq.3)then
              INO=-2
              GOTO 3
            endif
          endif

C For any loaded model, if user is not browsing check to see if
C the model should be updated.
          if(browse)then
            continue
          else
            call mupdate(ier)
          endif
        endif
      ELSEIF(INO.EQ.6)THEN

C Start new project, begin with registration.
        call tstamp('>','PRJ: beginning new model')
        root=' '
        mpath=' '
        folder=' '
        menu=' '
        call pregist('i ',root,mpath,folder,menu,ier)
      ELSEIF(INO.EQ.10)THEN

C Root name of the model.
        if(CFGOK)then
          h(1)='This `root name` will be used to create the'
          h(2)='ESP-r model folder structure that holds key'
          h(3)='files such as the system configuration file.'
          t24=cfgroot
          CALL EASKS(t24,' ','Project root name (<24 chars)?',
     &                24,'project','root name',IER,3)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')cfgroot=t24
          changedit=.true.
        endif
      ELSEIF(INO.EQ.11)THEN

C Title of the model.
        if(CFGOK)then
 246      H(1)='Each model has an associated descriptor (<72 chars),'
          H(2)='which should be suitably informative, e.g.:'
          H(3)=' '
          H(4)=' `PV-facade office, daylight controlled, base case.`'
          ltmp=modeltitle
          CALL EASKS(ltmp,' ','Model description?',
     &         72,'base case simulation','model description',IER,4)
          if(ltmp(1:2).eq.'  '.or.ltmp(1:4).eq.'UNKN')then
            call usrmsg('Blank description not allowed!',' ','W')
            goto 246
          else
            modeltitle=ltmp
          endif
          changedit=.true.
        endif
      ELSEIF(INO.EQ.12)THEN

C Version.
        CALL CFGVER
      ELSEIF(INO.EQ.13)THEN

C Browse the current model.
        if(CFGOK)then
          itru=iuout
          CALL EDCFG(ITRC,itru,IER)
        endif
      ELSEIF(INO.EQ.16)THEN

C Import from 3rd party tools.
        call cadin(itrc,ichoice,ier)
      ELSEIF(INO.EQ.17)THEN

C Export, if something other than registration or plant only.
        if(INDCFG.NE.0.and.INDCFG.NE.2)then
          call tstamp('>','PRJ: enter export controller')
          call exportcad(itrc,IER)
        else
          CALL USRMSG(' ','Insufficient data to export!','W')
        endif
      ELSEIF(INO.EQ.18)THEN

C Archive the model (make a tar file containing all the files referenced
C in the cfg file).
        call archiveit(ier)
      ELSEIF(INO.EQ.20)THEN

C Project folders and files.
        call pfolders('-',iier)
      ELSEIF(INO.EQ.22.or.INO.EQ.23)THEN

C Save model (22 without asking for file name & 23 asking for file name).
        if(browse)then
          call usrmsg('Cannot update the model while in browse',
     &                'mode: you must `own` the model!','W')
          goto 3
        endif

        if(INO.EQ.23)then

C Ask for file name.
          H(1)='The current model description will be copied'
          H(2)='into this file.'
          DFILE=' '
          ltmp=LCFGF
   89     CALL EASKS(ltmp,' Updated configuration file?',
     &      ' ',72,DFILE,'updated configuration file',IER,2)
          if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
            LCFGF=ltmp
          else
            call usrmsg('Re-enter file name.',' ','W')
            goto 89
          endif

C Also give user option to alter the root name.
          h(1)='This `root name` will be used to help create'
          h(2)='system level file names (control, connections).'
          h(3)='If you have altered the name of the configuration'
          h(4)='file you may want to modify the root name as'
          h(5)='well.'
          t24=cfgroot
          CALL EASKS(t24,' Root name for the project? (see help',
     &     'about whether to modify)',24,'project','root name',IER,5)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')cfgroot=t24
        endif

C If configuration file format is version 3 then check to see if there
C are any utility files. If found then ask is the user wants to save
C in older format.
        if(icfgv.ge.3)then

C Also check about connections file.
          if(ncon.gt.1)then
            if(INO.EQ.23)then
  289         H(1)='The surface topology of the model is held'
              H(2)='in a connections file.'
              write(DCNN,'(a,a)')cfgroot(1:lnblnk(cfgroot)),'.cnn'
              if(LCNN(1:1).eq.' ')LCNN=DCNN
              ltmp=LCNN
              CALL EASKS(ltmp,' Surface connections file name?',' ',
     &          72,DCNN,'system connx file name',IER,2)
              if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
                LCNN=ltmp
              else
                call usrmsg('Re-enter file name.',' ','W')
                goto 289
              endif
            endif
          endif
        endif

        call tstamp('>','PRJ: update configuration')
        call tstamp('>',LCFGF)
        CALL EMKCFG('-',IER)
        IF(IER.EQ.1)THEN
          call usrmsg('Problem creating new file. Check disk space',
     &                'and/or file permissions.','W')
        ENDIF
        goto 3
      ELSEIF(INO.EQ.MITEM-3)THEN

C Toggle trace level.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0
        INO=-4
        GOTO 3
      ELSEIF(INO.EQ.MITEM-2)THEN

C Allow user to change preferences for dates, time, wireframe view
C and reporting/trace level and version of the model files.
        MODSIT=.false.
        call setup(MODSIT,ITRC,IER)
        if(MODSIT)then
          call usrmsg('Saving new default view...',' ','-')
          CALL EMKCFG('-',IER)
          call usrmsg('Saving new default view...done.',' ','-')
          MODSIT=.false.
        endif
      ELSEIF(INO.EQ.MITEM-1)THEN

C Help via askabout: 0 requests display.
        call askabout('prj ',0)
      ELSE
        INO=-4
        GOTO 3
      ENDIF
      INO=-2
      GOTO 3

 901  call edisp(iuout,'Error opening Journal.')
      call edisp(iuout,' ')
      goto 902
 900  call edisp(iuout,'Error opening current Journal ... continuing.')
      call edisp(iuout,' ')
      goto 902
 899  call edisp(iuout,'Error closing current Journal ... continuing.')
      call edisp(iuout,' ')
      goto 902

      END
     
C ********** simula
C simula commissions technical assessments and analysis.
      subroutine simula(ier)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "sbem.h"
#include "ipvdata.h"
#include "tdf2.h"
C Get logical name of terminal type, expand model name
C to include the path and create a string to drive the modules.
      
      integer lnblnk  ! function definition

      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      common/appw/iappw,iappx,iappy
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/PREC7/ITCNST
      COMMON/C6/INDCFG
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      common/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

C Temporal header (to get its time frequency)
      common/CFGV/icfgv
      common/IPVF/lipvdatf

C Typical seasons and default simulation periods. Isset is the
C current seasons set, nsset number of sets, Isauto (0=use info as
C defaults 1=autoexec mode), istcnst (startup days), isbnstep (building
C timesteps/hr), ispnstep (plant timesteps/hr), issave (results save
C level).
      common/spmfxst/ispmxist,spflnam
      common/spfldes/spfdescr(MSPS)
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton
      COMMON/ENTFILE/ENTFLNAM,IENTXIST
      common/user/browse

      dimension ITEMS(25)

      character doit*248,tmode*8,ITEMS*34,H*72,key*1
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,spflnam*72,longtfile*144,longtfiledos*144
      CHARACTER*72 LAPROB,LCFGF,ENTFLNAM,lipvdatf
      character cfgroot*24,cr*24,spfdescr*8,ipvaction*3

C dd is character array for selecting simulation parameter sets.
      character dd(MSPS+3)*33,de*8,aut*12
      character t72*72

      character brw*11,outs*124
      character descra*7,descrb*7,descrst*10,descrfn*10

      logical DOK,OK,MSTROK,MSTRZN,XST,modparms
      logical browse,concat,hri,defok,unixok
      logical mlcok,bndryok,bndrysxc,confok,geofok,oprfok,prob
      logical tmcok,shdok

      integer isstupt,isbnstept,ispnstept,issavet  ! for local editing
      integer MITEM,INO,ic,IRT ! max items and current menu item

#ifdef OSI
      integer ICPMOD     ! passed from tchild
#else
      integer*8 ICPMOD     ! passed from tchild
#endif

C Set initial values of local variables.
      modparms=.false.
      lr=lnblnk(cfgroot)
      cr=cfgroot
      if(nsset.gt.0)isset=1
      t72='  '
      itrc=0   ! set initial value.
      aut=' interactive'
      call isunix(unixok)

C Do an initial check to see if the model is attributed and
C constructions are known.
      if(INDCFG.eq.2)then
        continue
      else
        call checkmodel(mlcok,bndryok,bndrysxc,confok,geofok,oprfok,
     &    prob,tmcok,shdok)
        if(.NOT.mlcok)then
          call edisp(iuout,'* Surface construction attributes not ok.')
        endif
        if(.NOT.bndryok)then
          call edisp(iuout,'* Surface connection attributes not ok.')
        endif
        if(.NOT.bndrysxc)then
          call edisp(iuout,'* Surface topology (1st check) not ok.')
       endif
        if(prob)then
          call edisp(iuout,'* Surface topology (2nd check) not ok.')
        endif
        if(.NOT.(geofok.AND.confok.AND.oprfok))then
          call edisp(iuout,'* Mandatory zone files do not exist!')
        endif
      endif

      if(noimg.gt.0)call imgdisp(1,'FPER',ier)
      
    3 INO=-4
      if(nsset.eq.0)then
        ITEMS(1) ='a simulation presets >> none  '
        ITEMS(2)='  ___________________________ '
        M=2
      else
        write(ITEMS(1),'(a,i2,a,i2,a)')'a simulation presets (',isset,
     &    ' of',nsset,')'
        write(ITEMS(2),'(a,a8)')      'b set name: ',spfdescr(isset)
        write(ITEMS(3),'(a,i4)')      'c start-up days:',isstup
        if(isbnstep.le.1)then
          write(ITEMS(4),'(a,i2)')    'd zone timestep/h:',isbnstep
        else
          if(isavgh.eq.0)then
            write(ITEMS(4),'(a,i2,a)')'d zone timestep/h:',isbnstep,
     &        ' each ts saved'
          else
            write(ITEMS(4),'(a,i2,a)')'d zone timestep/h:',isbnstep,
     &        ' saved 1 ts/hr'
          endif
        endif
        if(INDCFG.eq.2.or.INDCFG.eq.3)then
          write(ITEMS(5),'(a,i2)')    'e plant timestep/(bldg ts):',
     &      ispnstep
        else
          write(ITEMS(5),'(a)')       'e plant timestep/(bldg ts): N/A'
        endif
        write(ITEMS(6),'(a,i2)')      'f result save level:',issave

C Display the simulation period.
        CALL EDAY(isstday(isset),isstmon(isset),ijdstart)
        call stdate(iyear,ijdstart,descra,descrst)
        CALL EDAY(isfnday(isset),isfnmon(isset),ijdfinish)
        call stdate(iyear,ijdfinish,descrb,descrfn)
        write(ITEMS(7),'(4a)')        'g from: ',descrst,' - ',descrfn
        if(INDCFG.ne.2)then
          write(ITEMS(8),'(a,a)')     'h zone results: ',
     &      sblres(isset)(1:16)
        else
          write(ITEMS(8),'(a)')       '  zone results: N/A'
        endif
        if(IAIRN.ge.1)then
          write(ITEMS(9),'(a,a)')     'i flow results: ',
     &      sflres(isset)(1:16)
        else
          write(ITEMS(9),'(a)')       '  flow results: N/A'
        endif
        if(INDCFG.eq.2.or.INDCFG.eq.3)then
          write(ITEMS(10),'(a,a)')    'j plant results: ',
     &      splres(isset)(1:15)
        else
          write(ITEMS(10),'(a)')      '  plant results: N/A'
        endif
        if(ispmxist.gt.0)then
          write(ITEMS(11),'(a)')      'k : '
        else
          write(ITEMS(11),'(a)')      '  : N/A'
        endif
        if(MSTROK)then
          write(ITEMS(12),'(a,a)')    'l moist. results: ',
     &      smstres(isset)(1:15)
        else
          write(ITEMS(12),'(a)')      '  moisture results: N/A'
        endif
        if(ientxist.gt.0)then
          write(ITEMS(13),'(a,a)')    'm elect. results: ',
     &      selres(isset)(1:15)
        else
          write(ITEMS(13),'(a)')      '  electrical results: N/A'
        endif
        if(lnblnk(lipvdatf).eq.0)then
          write(ITEMS(14),'(a)')      '  IPV report   : N/A'
        elseif(lipvdatf(1:7).eq.'UNKNOWN')then
          write(ITEMS(14),'(a)')      '  IPV report   : N/A'
        else
          write(ITEMS(14),'(a,a15)')  'n IPV report: ',sipvres(1:15)
        endif    
        ITEMS(15)  ='o save or dereference parameters '
        ITEMS(16)  ='  ______________________________ '
        M=16
      endif
      if(INDCFG.eq.2)then
        ITEMS(M+1) ='p plant only simulation          '
      else
        ITEMS(M+1) ='p integrated simulation          '
      endif
      ITEMS(M+2)   ='r fluid flow only                '
      ITEMS(M+3)   ='s visual impact                  '
      ITEMS(M+4)   ='t integrated performance view    '
      ITEMS(M+5)   ='u NCM compliance checking        '
      ITEMS(M+6)   ='  ______________________________ '
      IF(ITRC.EQ.0)THEN
        ITEMS(m+7) = 'v feedback >> silent              '
      ELSEIF(ITRC.EQ.1)THEN
        ITEMS(m+7) = 'v feedback >> summary             '
      ELSEIF(ITRC.EQ.2)THEN
        ITEMS(m+7) = 'v feedback >> detailed            '
      ENDIF
      ITEMS(M+8)   ='? help                           '
      ITEMS(M+9)   ='- exit this menu                 '
      MITEM=M+9

C At each pass update for recent parameter changes.
      if(modparms)then
        call tstamp('>','PRJ: update configuration for sim-setup')
        call tstamp('>',LCFGF)
        CALL EMKCFG('-',IER)
        modparms=.false.
      endif

C Help text for this menu.
      H(1)='Normally an integrated simulation is invoked by which'
      H(2)='the heat, fluid, electrical power and light flows within'
      H(3)='combined building/plant models may be evaluated.'
      H(4)=' '
      H(5)='The other options are useful when only one of the above'
      H(6)='technical sub-systems is to be appraised, e.g. a CFD only'
      H(7)='analysis with fixed boundary temperatures. Note however'
      H(8)='that such a simulation can still be commissioned via the'
      H(9)='`Integrated` option.'
      H(10)=' '
      H(11)='You can define simulation options such as the assessment'
      H(12)='period prior to commissioning simulations. Several sets'
      H(13)='can be defined (e.g. standard seasonal assessments).'
      H(14)='A default set is always available.'

      CALL EMENU('Simulation Controller',ITEMS,MITEM,INO)
      if(INO.EQ.MITEM)then
        if(modparms)then
          CALL EMKCFG('-',IER)
          modparms=.false.
        endif
        RETURN
      elseif(INO.EQ.MITEM-1)then

C List help text for the simulations menu.
        CALL PHELPD('Simulation',14,'-',0,0,IER)
      elseif(INO.EQ.MITEM-2)then

C Toggle trace level.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0
      elseif(INO.EQ.MITEM-8)then

C Invoke simulator for the case of a simulation parameter set.
        if(nsset.gt.0)then
          h(1)='Interactive mode uses simulation parameters as'
          h(2)='defaults, which can be overwritten. The silent'
          h(3)='running mode uses the parameters to run in automatic'
          h(4)='mode.'
          CALL EASKABC(' ','Simulation interaction options:',
     &      'run interactively','run silently','cancel',
     &      isauto,4)
          if(isauto.eq.1)then
            aut=' interactive'
          elseif(isauto.eq.2)then
            aut=' silent'
          else
            goto 3
          endif

C Setup chile process for the type of terminal.
          call tstamp('>','PRJ: beginning simulation with param set.')
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          call isunix(unixok)
          if(unixok)then
            call addpath(LCFGF,longtfile,concat)
          else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
            call addpath(LCFGF,longtfile,concat)
            call cmdfiledos(longtfile,longtfiledos,ier)

C Debug the patched file name.
            write(outs,'(2a)') '* Corrected file ',
     &        longtfiledos(1:lnblnk(longtfiledos))
            call edisp248(iuout,outs,100)
            longtfile=' '
            longtfile=longtfiledos
          endif

C Debug.
C          write(6,*) ' simula browse status ',browse

C If browsing pass this information to bps and if the user
C has set the trace level above one add a -v optin to the
C command line.
          if(browse)then
	    if(itrc.gt.1)then
              brw = ' -v -b yes'
            else
              brw = ' -b yes'
            endif
          else
	    if(itrc.gt.1)then
              brw = ' -v -b no '
            else
              brw = ' -b no '
            endif
          endif

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,7a)') 'bps -mode ',tmode,
     &        ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &        longtfile(1:lnblnk(longtfile)),brw,' -p ',
     &        spfdescr(isset),aut,' &'
          else
            write(doit,'(9a)') 'bps -mode ',tmode,
     &        ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),
     &        brw,' -p ',spfdescr(isset),aut,' &'
          endif

C Debug.
          call usrmsg('Begining simulation via',doit,'-')
          call runit(doit,tmode)
          return
        else

C Invoke simulator for the case without a simulation parameter set.
          call tstamp('>','PRJ: beginning simulation')
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          call isunix(unixok)
          if(unixok)then
            call addpath(LCFGF,longtfile,concat)
          else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
            call addpath(LCFGF,longtfile,concat)
            call cmdfiledos(longtfile,longtfiledos,ier)

C Debug the patched file name.
            write(outs,'(2a)') '* Corrected file ',
     &        longtfiledos(1:lnblnk(longtfiledos))
            call edisp(iuout,outs)
            longtfile=' '
            longtfile=longtfiledos
          endif

C If browsing pass this information to bps.
          if(browse)then
	    if(itrc.gt.1)then
              brw = ' -v -b yes'
            else
              brw = ' -b yes'
            endif
          else
	    if(itrc.gt.1)then
              brw = ' -v -b no '
            else
              brw = ' -b no '
            endif
          endif

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,4a)') 'bps -mode ',tmode,
     &        ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &        longtfile(1:lnblnk(longtfile)),brw,' &'
          else
            write(doit,'(6a)') 'bps -mode ',tmode,
     &        ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),
     &        brw,' &'
          endif

C Debug.
          call usrmsg('Begining simulation via',doit,'-')
          call runit(doit,tmode)
          return
        endif
      elseif(INO.EQ.MITEM-7)then

        H(1)='The first option computes the pressure induced'
        H(2)='fluid flow within a network of connected flow'
        H(3)='components representing the building and/or plant'
        H(4)='system in whole or part.  The second option permits'
        H(5)='an n-dimensional steady state or transient flow'
        H(6)='analysis of a single zone by the technique of'
        H(7)='computational fluid dynamics.'
        H(8)=' '
        H(9)='In both cases, because the temperature distribution'
        H(10)='throughout the whole building/plant model is'
        H(11)='not computed, buoyancy effects and boundary conditions'
        H(12)='are time invariant. For models where this assumption'
        H(13)='is inappropriate, the `Integrated` simulation option'
        H(14)='should be used.'
        CALL EASKABC('Fluid flow analysis via:',' ','Nodal Network',
     &      'Computational Fluid Dynamics','cancel',IW,146)
        IF(IW.EQ.1)THEN

C Execute mfs.
          call tstamp('>','PRJ: beginning mfs')
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,a)') 'mfs -mode ',tmode,
     &        ' -s ',iappw,iappx+15,iappy+50,' &'
          else
            write(doit,'(3a)') 'mfs -mode ',tmode,' -s 0 0 0 &'
          endif

C Debug.
C          call usrmsg('Beginning flow only assessment via',doit,'-')
          call runit(doit,tmode)
        ELSEIF(IW.EQ.2)THEN

C Execute dfs.
          call tstamp('>','PRJ: beginning dfs')
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,a)') 'dfs -mode ',tmode,
     &        ' -s ',iappw,iappx+15,iappy+50,' &'
          else
            write(doit,'(3a)') 'dfs -mode ',tmode,' -s 0 0 0 &'
          endif

C Debug.
C          call usrmsg('Beginning CFD only assessment via',doit,'-')
          call runit(doit,tmode)
        ENDIF

      elseif(INO.EQ.MITEM-6)then

C Visual simulation.  Starts e2r with configuration file. Depending on
C if Unix or Windows setup the appropriate path.

C If browsing pass this information to e2r.
        if(browse)then
	  if(itrc.gt.1)then
            brw = ' -v -b yes'
          else
            brw = ' -b yes'
          endif
        else
	  if(itrc.gt.1)then
            brw = ' -v -b no '
          else
            brw = ' -b no '
          endif
        endif

        call tstamp('>','PRJ: beginning e2r')
        doit = ' '
        call tchild(ICPMOD)
        call termode(ICPMOD,tmode)
        call isunix(unixok)
        if(unixok)then
          call addpath(LCFGF,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(LCFGF,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        if(iappw.gt.0.and.iappw.le.200)then
          write(doit,'(3a,3i4,4a)') 'e2r -mode ',tmode,
     &      ' -s ',iappw,iappx+35,iappy+40,' -file ',
     &      longtfile(1:lnblnk(longtfile)),brw,' &'
        else
          write(doit,'(6a)') 'e2r -mode ',tmode,
     &      ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),brw,' &'
        endif

C Debug.
        call usrmsg('Beginning visual assessment via',doit,'-')
        call runit(doit,tmode)
      elseif(INO.EQ.MITEM-5)then

C IPV for the actual number of simulations required.
        call ipvactions(ier)

      elseif(INO.EQ.MITEM-4)then

C Generate UK NCM specific models, run simulations and generate results.
C On return reset the value of isset.
          call MMGFUN
          if(nsset.gt.0)isset=1

      elseif(MITEM.eq.11.and.INO.EQ.1)then

C Case of no initial sets, offer default or user defined sets. If the
C data is internal to the file then it will already have been scanned.
        if(icfgv.lt.4)then
          if(lnblnk(lipvdatf).eq.0.or.lipvdatf(1:7).eq.'UNKNOWN')then
            h(1)='Simulation options (timesteps, start & finish dates,'
            h(2)='startup periods, result file names, etc.) can be '
            h(3)='defined in advance. You can define a several sets'
            h(4)='of parameters, e.g. to support standard assessments'
            h(5)='for a project or seasonal assessments (see also IPV'
            h(6)='definitions in project context). '
            DOK=.false.
            CALL ASKOK('  ',' Proceed with parameter definitions?',
     &      OK,DOK,6)
            if(.NOT.OK)then
              INO=-4
              GOTO 3
            endif
            ID=1
          else

C Scan the IPV description before asking user choice.
            call FINDFIL(lipvdatf,XST)
            IF(XST)then
	      ipvaction='ipv'
              call ripvdat(ifil+1,lipvdatf,ipvaction,ier)
            else
              call usrmsg('IPV definition not found.',lipvdatf,'W')
              INO=-4
              GOTO 3
            endif
            h(1)='Simulation options (timesteps, start & finish dates,'
            h(2)='startup periods, results file names, etc.) can be '
            h(3)='defined in advance. You can define several sets'
            h(4)='of parameters, e.g. to support standard assessments'
            h(5)='for a project or seasonal assessments. '
            h(6)='An IPV description has is associated with this model'
            h(7)='and matching parameter sets will be created. '
            DOK=.true.
            CALL ASKOK('An IPV description has been found. Proceed to',
     &        'create matching parameter sets?',OK,DOK,7)
            if(.NOT.OK)then
              INO=-4
              GOTO 3
            endif
            ID=3
          endif
        else
	
C There may be ipv data held in the configuration file.
          if(nipvassmt.eq.0)then
            h(1)='Simulation options (timesteps, start & finish dates,'
            h(2)='startup periods, result file names, etc.) can be '
            h(3)='defined in advance. You can define a several sets'
            h(4)='of parameters, e.g. to support standard assessments'
            h(5)='for a project or seasonal assessments (see also IPV'
            h(6)='definitions in project context). '
            DOK=.false.
            CALL ASKOK('  ',' Proceed with parameter definitions?',
     &      OK,DOK,6)
            if(.NOT.OK)then
              INO=-4
              GOTO 3
            endif
            ID=1
          else
            h(1)='Simulation options (timesteps, start & finish dates,'
            h(2)='startup periods, results file names, etc.) can be '
            h(3)='defined in advance. You can define several sets'
            h(4)='of parameters, e.g. to support standard assessments'
            h(5)='for a project or seasonal assessments. '
            h(6)='An IPV description has is associated with this model'
            h(7)='and matching parameter sets will be created. '
            DOK=.true.
            CALL ASKOK('An IPV description has been found. Proceed to',
     &        'create matching parameter sets?',OK,DOK,7)
            if(.NOT.OK)then
              INO=-4
              GOTO 3
            endif
            ID=3	   
          endif
        endif

C Setup one parameter set or several sets to match IPV description.
        if(ID.eq.1.or.ID.eq.3)then
          if(INDCFG.eq.2)then
            nsset=1
            isset=1
            isstup=0
            isbnstep=1
            ispnstep=10
            issave=4
            isavgh=0
          else
            call scntcnst(TDM,istd,TCM,ISTC)
            nsset=1
            isset=1
            if(isstup.eq.0)isstup=ITCNST
            isbnstep=1
            ispnstep=10
            issave=4
            isavgh=0
          endif
          if(ID.eq.1)then
            h(1)='Typical names are `win`, `spr` & `sum`.'
            de='default'
            CALL EASKS(de,'Name for set?',' ',8,'win','set name',IER,1)
            write(spfdescr(isset),'(a8)')de(1:8)
            lde=lnblnk(spfdescr(isset))
            spfdescr(2)='-'
            spfdescr(3)='-'
            spfdescr(4)='-'
            spfdescr(5)='-'
            isstday(isset)=9
            isstmon(isset)=1
            isfnday(isset)=15
            isfnmon(isset)=1
            cr=cfgroot
            lr=lnblnk(cfgroot)
            if(INDCFG.ne.2)then
              WRITE(sblres(isset),'(A,A4)')cr(1:lr),'.res'
            else
              sblres(isset)=' '
            endif
            if(IAIRN.ge.1)then
              WRITE(sflres(isset),'(A,A4)')cr(1:lr),'.mfr'
            else
             sflres(isset)=' '
            endif
            if(INDCFG.eq.2.or.INDCFG.eq.3)then
              WRITE(splres(isset),'(A,A4)')cr(1:lr),'.plr'
            else
             splres(isset)=' '
            endif
            if(MSTROK)then
              WRITE(smstres(isset),'(A,A4)')cr(1:lr),'.msr'
            else
             smstres(isset)=' '
            endif
            if(lnblnk(lipvdatf).eq.0)then
              sipvres=' '
            elseif(lipvdatf(1:7).eq.'UNKNOWN')then
              sipvres=' '
            else
              WRITE(sipvres,'(A,A7)')cr(1:lr),'ipv.rep'
            endif
          elseif(ID.eq.3)then

C Set parameter sets for current number of IPV assessments.
            call ipv2simpar(ipvsimu)
          endif
          modparms=.true.
        endif
      elseif(MITEM.eq.25.and.INO.EQ.1)then

C Select an existing set, create a new one or cancel.
 170     do 60 ii=1, nsset
           CALL EDAY(isstday(ii),isstmon(ii),ijdstart)
           call stdate(iyear,ijdstart,descra,descrst)
           CALL EDAY(isfnday(ii),isfnmon(ii),ijdfinish)
           call stdate(iyear,ijdfinish,descrb,descrfn)
           CALL EMKEY(ii,KEY,IER)
           write(dd(ii),'(7a)') key,' ',spfdescr(ii),' ',descrst,'-',
     &       descrfn
 60     continue
        if (nsset.lt.MSPS) then
          dd(nsset+1)='+ make a new set'
        else
          dd(nsset+1)='                '
        endif
        dd(nsset+2)='? help            '
        dd(nsset+3)='- end             '
        ic=nsset+3
        if(ic.lt.MSPS+3)then
          do 61 ii=ic+1,MSPS+3
            dd(ii)=' '
 61       continue
        endif

C Help text for this menu.
        h(1)='Simulation options can be defined in advance with this'
        h(2)='facility. Named sets (e.g. winter|trans|summer)'
        h(3)='allow specific assessments to be invoked.'
        h(4)=' '
        h(5)='Definitions of integrated performance views (see the'
        h(6)='model context menu) might have already set up one,'
        h(7)='three or five parameter sets.'
        h(8)=' '
        h(9)='Selecting one of the existing sets will load the'
        h(10)='associated parameters and any simulation commissioned'
        h(11)='will use this information.'
        h(12)=' '
        h(13)='After running an assessment, your next step is to '
        h(14)='explore the predictions via the results analysis'
        h(15)='module the correct results file should be identified'
        h(16)='automatically. '
        h(17)=' '
        h(18)='NOTE: if you have run several assessments, you should'
        h(19)='select the correct set before going on to results '
        h(20)='analysis. '

        CALL EMENU('Parameter sets',dd,ic,IRT)
        if(IRT.ge.ic) then
          INO=-4
          GOTO 3
        elseif(IRT.eq.ic-1) then
          CALL PHELPD('Simulation sets',20,'-',0,0,IER)
          goto 170
        elseif(IRT.eq.ic-2.and.nsset.lt.5)then

C It is possible to add another set so ask user and set file names.
C << note this probably should check to see if there is an IPV >>
C << description first >>
          h(1)='Typical names are win spr sum. '
          de='win'
          CALL EASKS(de,' ','Name for set?',8,'win','set name',IER,1)
          nsset=nsset+1
          isset=nsset
          write(spfdescr(isset),'(a8)')de(1:8)
          lde=lnblnk(spfdescr(isset))
          isstday(isset)=9
          isstmon(isset)=1
          isfnday(isset)=15
          isfnmon(isset)=1
          if(INDCFG.ne.2)then
            WRITE(sblres(isset),'(3a)')cr(1:lr),de(1:lde),'.res'
          else
            sblres(isset)=' '
          endif
          if(IAIRN.ge.1)then
            WRITE(sflres(isset),'(3a)')cr(1:lr),de(1:lde),'.mfr'
          else
           sflres(isset)=' '
          endif
          if(INDCFG.eq.2.or.INDCFG.eq.3)then
            WRITE(splres(isset),'(3a)')cr(1:lr),de(1:lde),'.plr'
          else
           splres(isset)=' '
          endif
          if(MSTROK)then
            WRITE(smstres(isset),'(3a)')cr(1:lr),de(1:lde),'.msr'
          else
           smstres(isset)=' '
          endif
          modparms=.true.
          goto 170
        elseif(IRT.ge.1.and.IRT.le.nsset)then
          isset=IRT
        endif
        call scntcnst(TDM,istd,TCM,ISTC)
      elseif(MITEM.eq.25.and.INO.EQ.2)then

C If there is an IPV descripton and this set is one,
C do not allow this set name to be changed.
        if(nipvassmt.eq.nsset)then
          call usrmsg('IPV set name is pre-defined,','.','W')
        else
          h(1)='Typical names are `win`, `spr` and `sum`.'
          de=spfdescr(isset)
          CALL EASKS(de,' ','Name for set?',8,spfdescr(isset),
     &                  'set name',IER,1)
          write(spfdescr(isset),'(a8)')de(1:8)
          modparms=.true.
        endif
      elseif(MITEM.eq.25.and.INO.EQ.3)then
        if(INDCFG.eq.2)then
          call edisp(iuout,'* Start-up days not applicable to plant.')
        else
          call scntcnst(TDM,istd,TCM,ISTC)
          H(1)='The pre-simulation start-up period is required to'
          H(2)='isolate the effects of initialisation assumptions from'
          H(3)='the simulation results. The suggested value, which is'
          H(4)='computed automatically, may be overridden.'
          isstupt=isstup
          CALL EASKI(isstupt,' ',
     &       'Pre-simulation start-up period (days)?',
     &       0,'F',100,'W',ITCNST,'presimulation days',IERI,2)
          if(ieri.eq.-3)then
            continue
          else
            isstup=isstupt
            modparms=.true.
          endif 
        endif
      elseif(MITEM.eq.25.and.INO.EQ.4)then

C The simulation timestep. Edit local variable and if not canceling
C instantiate it and also ask about results integration if more than
C one timestep in the hour.
        if(itdflg.ne.0)then
          write(outs,'(2a,i3,a)') '* The model includes a temporal',
     &     'file with ',ntsph,' per hour frequency. Use this value.'
          call edisp(iuout,outs)
        endif
        H(1)='Building-side timesteps range from 1 to 60 per hour.'
        H(2)='If the model includes temporal definitions data then'
        H(3)='the building-side time step must be matched to the'
        H(4)='value used for the temporal definitions.'
        isbnstept=isbnstep
        CALL EASKI(isbnstept,' ','Zone-side time steps per hour?',
     &       1,'F',60,'W',2,'zone steps per hour',IERI,4)
        if(ieri.eq.-3)then
          continue
        else
          isbnstep=isbnstept
          if(isbnstep.gt.1)then
            if(itdflg.ne.0)then

C There is temporal data so must save at each timestep.
C              write(6,*) 'temporal data found so save each timestep'

              isavgh=0
            else
              H(1)='The ESP-r Simulator calculates the value of each'
              H(2)='state variable at each computational time step.'
              H(3)='The default action is to transfer each value at'
              H(4)='each computational time step to the results'
              H(5)='file(s). This option allows to average results'
              H(6)='over one hour before transfer to the results'
              H(7)='database(s), thus decreasing the size of results'
              H(8)='database(s).'
              defok=.false.
              hri=.false.
              call askok(' Hourly results integration:',' ',hri,
     &          defok,8)
              if(hri)then
                isavgh=1
              else
                isavgh=0
              endif
            endif
          else
            isavgh=0
          endif
          modparms=.true.
        endif
      elseif(MITEM.eq.25.and.INO.EQ.5)then
        if(INDCFG.eq.2.or.INDCFG.eq.3)then
          H(1)='Plant-side time step is an integer subdivision'
          H(2)='of the active building side time steps per hour.'
          ispnstept=ispnstep
          CALL EASKI(ispnstept,' ',
     &      'Plant-side time steps per zone step?',
     &      1,'F',100,'W',10,'plt steps per hour',IERI,2)
          if(ieri.eq.-3)then
            continue
          else
            ispnstep=ispnstept
            modparms=.true.
          endif
        endif
      elseif(MITEM.eq.25.and.INO.EQ.6)then
        H(1)='Level 0 - summary table'
        H(2)='Level 1 - zone temperatures and plant demand'
        H(3)='Level 2 - as 1 + zone surface temperatures'
        H(4)='Level 3 - as 2 + zone construction node temperatures'
        H(5)='Level 4 - as 3 + zone and surface energy balances'
        H(6)='Level 6 - summary database friendly and csv file'
        issavet=issave
        CALL EASKI(issavet,' ','Results save level (0-4 & 6)?',
     &      0,'F',6,'F',6,'save level',IERI,6)
        if(ieri.eq.-3)then
          continue
        else
          issave=issavet
          modparms=.true.
        endif
      elseif(MITEM.eq.25.and.INO.EQ.7)then 
        CALL EDAY(isstday(isset),isstmon(isset),ISDS)
        CALL EDAY(isfnday(isset),isfnmon(isset),ISDF)
        call EASKPER('Assessment period:',ISDS,ISDF,IFDAY,IER)
        call EDAYR(ISDS,isstday(isset),isstmon(isset))
        call EDAYR(ISDF,isfnday(isset),isfnmon(isset))
        modparms=.true.
      elseif(MITEM.eq.25.and.INO.EQ.8)then
        h(1)='This library holds the predictions associated'
        h(2)='with a building simulation.'
        t72=sblres(isset)
        CALL EASKS(t72,' ','Building results library?',72,
     &    'zones.res','bld library',IER,2)
        sblres(isset)=t72
        modparms=.true.
      elseif(MITEM.eq.25.and.INO.EQ.9)then
        if(IAIRN.ge.1)then
          h(1)='This library holds the predictions associated'
          h(2)='with a network flow simulation.'
          t72=sflres(isset)
          CALL EASKS(t72,' ','Network flow results library?',72,
     &    'network.mfr','flow results library',IER,2)
          sflres(isset)=t72
          modparms=.true.
        endif
      elseif(MITEM.eq.25.and.INO.EQ.10)then
        if(INDCFG.eq.2.or.INDCFG.eq.3)then
          h(1)='This library holds the predictions associated'
          h(2)='with a plant simulation.'
          t72=splres(isset)
          CALL EASKS(t72,' ','Plant results library?',72,
     &    'plant.res','plant results library',IER,2)
          splres(isset)=t72
          modparms=.true.
        endif
      elseif(MITEM.eq.25.and.INO.EQ.11)then
        if(ispmxist.gt.0)then
          continue
        endif
      elseif(MITEM.eq.25.and.INO.EQ.12)then
        if(MSTROK)then
          h(1)='This library holds the predictions associated'
          h(2)='with a moisture simulation.'
          t72=smstres(isset)
          CALL EASKS(t72,' ','Moisture results library?',72,
     &    'moist.res','moisture results library',IER,2)
          smstres(isset)=t72
          modparms=.true.
        endif
      elseif(MITEM.eq.25.and.INO.EQ.13)then
        if(ientxist.gt.0)then
          h(1)='This library holds the predictions associated'
          h(2)='with an electrical simulation.'
          t72=selres(isset)
          CALL EASKS(t72,' ','Electrical results library?',72,
     &    'elect.res','electrical results library',IER,2)
          selres(isset)=t72
          modparms=.true.
        endif
      elseif(MITEM.eq.25.and.INO.EQ.14)then
        if(lnblnk(lipvdatf).eq.0)then
        elseif(lipvdatf(1:7).eq.'UNKNOWN')then
        else
          h(1)='This is the name of the file to be used to transfer'
          h(2)='simulation results to the IPV generation tool.'
          t72=sipvres
          CALL EASKS(t72,' ','IPV transfer file name?',72,
     &    'IPV.rep','IPV report file',IER,1)
          sipvres=t72
          modparms=.true.
        endif
      elseif(MITEM.eq.25.and.INO.EQ.15)then
        h(1)='You can save changes to simulation parameter sets or'
        h(2)='dereference the current sets (e.g. to import new '
        h(3)='IPV assessment periods).'
        CALL EASKABC(' ','Options:',
     &    'save current sets','clear sets','cancel',IDO,3)
        if(IDO.eq.3)then
          goto 3
        elseif(IDO.eq.1.or.IDO.eq.2)then
          if(IDO.eq.2)then
            nsset=0
            modparms=.true.
          endif
          if(modparms)then
            call tstamp('>','PRJ: update configuration for sim-setup')
            call tstamp('>',LCFGF)
            CALL EMKCFG('-',IER)
            modparms=.false.
          else
C            call usrmsg('Sets are current, nothing to update.',
C     &                  ' ','-')
          endif
          goto 3
        endif
      else
        INO=-4
        GOTO 3
      ENDIF
      INO=-2
      GOTO 3

      END

C ********** ipvactions
C ipvactions commissions ipv assessments and recovery and comparisons.

C << noticed that the results file passed to res is 72 char rather than
C << the longer string used when commissioning a simulation.

      subroutine ipvactions(ier)
#include "building.h"
#include "model.h"
#include "ipvdata.h"

      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive the modules.
      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      common/appw/iappw,iappx,iappy
      common/C21/IFCFG,cfgroot,LCFGF
      common/IPVF/lipvdatf
      common/exporttg/xfile,tg,delim
      common/CFGV/icfgv
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

C Typical seasons and default simulation periods. Isset is the
C current seasons set, nsset number of sets, Isauto (0=use info as
C defaults 1=autoexec mode), istcnst (startup days), isbnstep (building
C timesteps/hr), ispnstep (plant timesteps/hr), issave (results save
C level), isavgh = 0 save every timestep = 1 save hourly zone data.
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres
      common/user/browse

      character doit*248,tmode*8,tfile*72,H*72
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,longtfile*144,longtfiledos*144
      CHARACTER LCFGF*72,lipvdatf*72
      character cfgroot*24,aut*12

      dimension assmttag(MSPS),runtag(MSPS)
      character assmttag*8,runtag*16,brw*8
      character ltpath*72,filen*72,ltmp*72
      character basefile*144,variantfile*144,outs*124
      character word1*20,word2*40,word3*20,outstr*124
      character xfile*144,tg*1,delim*1,tab*1,fs*1
      character ipvaction*3

      logical XST,browse,remote,concat,unixok
      logical atreports,im2
      integer IWM   ! for radio button

C   anu_ht_m2 - annual heat kWh/m2/a
C   anu_ht_bld - annual heat for building kWh/a
C   anu_ht_thrm_m2 - annual heating therms/m2/a
C   anu_ht_thrm_bld - annual heating therms/a
C   anu_cl_m2 - annual cooling kWh/m2/a
C   anu_cl_bld - annual heat for buidling kWh/a
C   anu_tot_m2 - annual total heating/cooling/lights etc.
C   anu_ht_cap_m2 - annual heating capacity kW/m2/a
C   anu_ht_cap_bld - annual buidling heating capacity kW/a
C   anu_cl_cap_m2 - annual cooling capacity kW/m2/a
C   anu_cl_cap_bld - annual buidling cooling capacity kW/a
C the first array index is base and 2nd is variant, 3rd diff
      dimension anu_ht_m2(3),anu_ht_bld(3)
      dimension anu_ht_thrm_m2(3),anu_ht_thrm_bld(3)
      dimension anu_cl_m2(3),anu_cl_bld(3)
      dimension anu_tot_m2(3),anu_tot_bld(3)
      dimension anu_ht_cap_m2(3),anu_ht_cap_bld(3)
      dimension anu_cl_cap_m2(3),anu_cl_cap_bld(3)
#ifdef OSI
      integer ICPMOD     ! passed from tchild
#else
      integer*8 ICPMOD     ! passed from tchild
#endif


      ibasefile=11
      ivarfile=12
      NSIGFIG=3
      tab=CHAR(9)

      if(nsset.gt.0)isset=1
      write(basefile,'(a)') sipvres(1:lnblnk(sipvres))
      write(variantfile,'(a)') sipvres(1:lnblnk(sipvres))
      aut=' default'

C Set file separator
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Clear values.
      anu_ht_m2(1)=0.; anu_ht_m2(2)=0.; anu_ht_m2(3)=0.
      anu_ht_bld(1)=0.; anu_ht_bld(2)=0.; anu_ht_bld(3)=0.
      anu_ht_thrm_m2(1)=0.; anu_ht_thrm_m2(2)=0.; anu_ht_thrm_m2(3)=0.
      anu_ht_thrm_bld(1)=0.;anu_ht_thrm_bld(2)=0.;anu_ht_thrm_bld(3)=0.
      anu_cl_m2(1)=0.; anu_cl_m2(2)=0.; anu_cl_m2(3)=0.;  
      anu_cl_bld(1)=0.; anu_cl_bld(2)=0.; anu_cl_bld(3)=0.;
      anu_tot_m2(1)=0.; anu_tot_m2(2)=0.; anu_tot_m2(3)=0.;   
      anu_tot_bld(1)=0.; anu_tot_bld(2)=0.; anu_tot_bld(3)=0.;   
      anu_ht_cap_m2(1)=0.; anu_ht_cap_m2(2)=0.; anu_ht_cap_m2(3)=0.
      anu_ht_cap_bld(1)=0.; anu_ht_cap_bld(2)=0.; anu_ht_cap_bld(3)=0.
      anu_cl_cap_m2(1)=0.; anu_cl_cap_m2(2)=0.; anu_cl_cap_m2(3)=0. 
      anu_cl_cap_bld(1)=0.; anu_cl_cap_bld(2)=0.; anu_cl_cap_bld(3)=0. 
 
C IPV for the actual number of simulations required.
      if(icfgv.lt.4)then
        H(1)='An Integrated Performance View (IPV) is a collection'
        H(2)='of standard performance indicators that have been'
        H(3)='pre-defined. Some of the underlying assumptions may'
        H(4)='be changed via the Model Context menu option.'
        if(lnblnk(lipvdatf).eq.0)then
          CALL PHELPD('IPV message',4,'-',0,0,IER)
          return
        elseif(lipvdatf(1:7).eq.'UNKNOWN')then
          CALL PHELPD('IPV message',4,'-',0,0,IER)
          return
        else
          call FINDFIL(lipvdatf,XST)
          IF(XST)then
	    ipvaction='ipv'
            call ripvdat(ifil+1,lipvdatf,ipvaction,ier)
            if(ier.ne.0)then
              CALL PHELPD('IPV message',4,'-',0,0,IER)
              return
            endif
          else
            CALL PHELPD('IPV message',4,'-',0,0,IER)
            return
          endif
        endif
      else
      
C IPV data could be internal to cfg file.
        if(nipvassmt.eq.0)then
          CALL PHELPD('IPV message',4,'-',0,0,IER)
          return	
        else
          continue
        endif
      endif

  39  h(1)='Sufficient information seems available to undertake '
      h(2)='the assessments needed for an IPV. '
      h(3)=' '
      h(4)='The option to run required simulations will invoke '
      h(5)='as many simulations as required, saving the results '
      h(6)='to the files specified in the simulation parameter '
      h(7)='sets. The assessments can be run silently or in an '
      h(8)='interactive mode. '
      h(9)=' '
      h(10)='Extracting IPV reports is done by invoking the res '
      h(11)='module with specific command line parameters. Results'
      h(12)='recovery is usually done in a silent mode as there is'
      h(13)='minimal user interaction required. '
      h(14)=' '
      h(15)='You can compare two IPV reports, extracting and '
      h(16)='reporting on the differences found. The report'
      h(17)='includes both nominal and whole building values '
      h(18)='for the following performance metrics: '
      h(19)=' '
      h(20)=' *for each variant it reports heating demand kWh/m^2/a'
      h(21)='  and for the whole model (kWh/a), heating capacity kW/m^2'
      h(22)='  and for the whole model (kW/a diversified).'
      h(23)=' *for each variant it reports heating therms (converted'
      h(24)='  as kWh * 0.0341296)'
      h(25)=' *for each variant it reports cooling demand kWh/m2/a'
      h(26)='  and for the whole model (kWh/a), cooling capacity kW/m^2'
      h(27)='  and for the whole model (kW/a)'
      h(28)=' *difference in heating, therms and cooling by area and'
      h(29)='  for the whole building.'
      CALL EASKABCD(' ','IPV options:',
     &  'run required simulations','extract reports',
     &  'compare reports','cancel',irs,29)
      if(irs.eq.1)then
        h(1)='Interactive mode uses simulation parameters as '
        h(2)='defaults. The silent running mode uses the'
        h(3)='parameters to run in automatic mode.'
        CALL EASKABC(' ','Simulation options:',
     &    'run interactively','run silently',
     &    'cancel',isauto,3)
        call tstamp('>','PRJ: beginning IPV simulations.')
        doit = ' '
        call tchild(ICPMOD)
        call termode(ICPMOD,tmode)
        if(isauto.eq.1)then
          aut=' default'
        elseif(isauto.eq.2)then
          aut=' silent '
          tmode='text'
        elseif(isauto.eq.3)then
          return
        endif

C Temporarily assume no browsing if in ipv mode.
        if(browse)then
          brw = ' -b yes'
        else
          brw = ' -b no '
        endif

        call isunix(unixok)
        if(unixok)then
          call addpath(LCFGF,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(LCFGF,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif

C Run only the number of assessments required.
        if(nipvassmt.eq.1)then
          assmttag(1)=' -p ann'
        elseif(nipvassmt.eq.3)then
          assmttag(1)=' -p win'
          assmttag(2)=' -p trn'
          assmttag(3)=' -p sum'
        elseif(nipvassmt.eq.5)then
          assmttag(1)=' -p win1'
          assmttag(2)=' -p spr'
          assmttag(3)=' -p sum'
          assmttag(4)=' -p aut'
          assmttag(5)=' -p win2'
        endif
        do 667 ij=1,nipvassmt
              
C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,5a)') 'bps -mode ',tmode,
     &        ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &        longtfile(1:lnblnk(longtfile)),brw,assmttag(ij),aut
          else
            write(doit,'(7a)') 'bps -mode ',tmode,
     &        ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),
     &        brw,assmttag(ij),aut
          endif
C          call usrmsg('Beginning assessment via',doit,'-')
          if(isauto.eq.2)then
            call runit(doit,'-')
          else
            call runit(doit,tmode)
          endif
C          call edisp(iuout,'* IPV run complete.')
  667   continue
      elseif(irs.eq.2)then
        h(1)='Interactive mode uses simulation parameters as '
        h(2)='defaults. The silent running mode uses the'
        h(3)='parameters to run in automatic mode. '
        CALL EASKABC(' ','IPV extraction options:',
     &    'run interactively','run silently','cancel',
     &    isauto,3)
        call tstamp('>','PRJ: beginning IPV recovery.')
        doit = ' '
        call tchild(ICPMOD)
        call termode(ICPMOD,tmode)
        if(isauto.eq.1)then
          aut=' default'
        elseif(isauto.eq.2)then
          aut=' silent '
          tmode='text'
        elseif(isauto.eq.3)then
          return
        endif
        h(1)='IPV recovery assumes that the relevant results'
        h(2)='libraries for each simulation are available.'
        h(3)='The `all` option does each in sequence.'
        call easkabcd('IPV recovery options:',' ','intermediate',
     &    'scan intermediate ->annual','all','cancel',IRT,3)
        if(IRT.eq.4) goto 39  ! redisplay menu.

C Extract only for the number of assessments required. Note: the strings
C ipv_win ipv_win1 ipv_aut etc are set in the IPV descriptive process and
C should not be altered in the simulation parameter sets.
C << add code to disable editing if an IPV has been defined >>
        if(nipvassmt.eq.1)then
          runtag(1)=' -act ipv_ann '
        elseif(nipvassmt.eq.3)then
          runtag(1)=' -act ipv_win '
          runtag(2)=' -act ipv_trn '
          runtag(3)=' -act ipv_sum '
        elseif(nipvassmt.eq.5)then
          runtag(1)=' -act ipv_win1 '
          runtag(2)=' -act ipv_spr '
          runtag(3)=' -act ipv_sum '
          runtag(4)=' -act ipv_aut '
          runtag(5)=' -act ipv_win2 '
        endif

C Logic to react when browsing a model.
        remote=.false.
        if(browse)then
          remote=.true.
        else
          ltmp = LCFGF
          call addpath(ltmp,longtfile,concat)
          if(concat)then
            remote=.true.
          endif
        endif

C Debug.
C        write(6,*) 'status of browse and remote ',browse,remote

        if(IRT.eq.1.or.IRT.eq.3)then
          do 668 ij=1,nipvassmt
            tfile=sblres(ij)
            if(remote)then
              call fdroot(tfile,ltpath,filen)
              call isunix(unixok)
              if(unixok)then
                if (ICHAR(ltpath(1:1)).ne.47) then
                  write(tfile,'(3a)') upath(1:lnblnk(upath)),fs,
     &              filen(1:lnblnk(filen))
                endif
              else
                if (ltpath(2:2).ne.':') then
                  write(tfile,'(3a)') upath(1:lnblnk(upath)),fs,
     &              filen(1:lnblnk(filen))
                endif
              endif

C Debug.
C              write(6,*) 'reset tfile to ',tfile

            endif

            h(1)='IPV recovery assumes that the relevant results'
            h(2)='libraries for each simulation are available.'
            h(3)='The all option does each in sequence. '
            if(iappw.gt.0.and.iappw.le.200)then
              write(doit,'(3a,3i4,4a)') 'res -mode ',tmode,
     &          ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &          tfile(1:lnblnk(tfile)),runtag(ij),aut
            else
              write(doit,'(6a)') 'res -mode ',tmode,
     &           ' -file ',tfile(1:lnblnk(tfile)),runtag(ij),aut
            endif
C            call usrmsg('Beginning performance recovery via',doit,'-')
            if(isauto.eq.2)then
              call runit(doit,'-')
            else
              call runit(doit,tmode)
            endif
            write(outs,'(3a)') '* IPV extract ',
     &        runtag(ij)(7:lnblnk(runtag(ij))),' is complete.'
            call edisp(iuout,outs)
 668      continue
          if(IRT.eq.3)then
            tfile=sblres(nipvassmt)
            if(remote)then
              call fdroot(tfile,ltpath,filen)
              call isunix(unixok)
              if(unixok)then
                if (ICHAR(ltpath(1:1)).ne.47) then
                  write(tfile,'(3a)') upath(1:lnblnk(upath)),fs,
     &              filen(1:lnblnk(filen))
                endif
              else
                if (ltpath(2:2).ne.':') then
                  write(tfile,'(3a)') upath(1:lnblnk(upath)),fs,
     &              filen(1:lnblnk(filen))
                endif
              endif

C Debug.
C              write(6,*) 'itr3 reset tfile to ',tfile

            endif
            if(iappw.gt.0.and.iappw.le.200)then
              write(doit,'(3a,3i4,4a)') 'res -mode ',tmode,
     &          ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &          tfile(1:lnblnk(tfile)),' -act ipv_annual ',aut
            else
              write(doit,'(6a)') 'res -mode ',tmode,' -file ',
     &          tfile(1:lnblnk(tfile)),' -act ipv_annual ',aut
            endif
C            call usrmsg('Beginning performance recovery via',doit,'-')
            if(isauto.eq.2)then
              call runit(doit,'-')
            else
              call runit(doit,tmode)
            endif
C            call edisp(iuout,'* IPV report complete.')
          endif
        elseif(IRT.eq.2)then
          tfile=sblres(nipvassmt)
          if(remote)then
            call fdroot(tfile,ltpath,filen)
            call isunix(unixok)
            if(unixok)then
              if (ICHAR(ltpath(1:1)).ne.47) then
                write(tfile,'(3a)') upath(1:lnblnk(upath)),fs,
     &            filen(1:lnblnk(filen))
              endif
            else
              if (ltpath(2:2).ne.':') then
                write(tfile,'(3a)') upath(1:lnblnk(upath)),fs,
     &            filen(1:lnblnk(filen))
              endif
            endif

C Debug.
C            write(6,*) ' irt2 reset tfile to ',tfile

          endif
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,4a)') 'res -mode ',tmode,
     &        ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &        tfile(1:lnblnk(tfile)),' -act ipv_annual ',aut
          else
            write(doit,'(6a)') 'res -mode ',tmode,' -file ',
     &        tfile(1:lnblnk(tfile)),' -act ipv_annual ',aut
          endif
C          call usrmsg('Beginning performance recovery via',doit,'-')
          if(isauto.eq.2)then
            call runit(doit,'-')
          else
            call runit(doit,tmode)
          endif
        endif
      elseif(irs.eq.3)then

C Compare two IPV reports.  First ask for separator.

C Toggle delimeter.
        H(1) ='Tabular data can be sent to file with various'
        H(2) ='delimiters:'
        H(3) ='  - spaces (use spaces to lineup columns);'
        H(4) ='  - single space between columns;'
        H(5) ='  - comma separator (e.g. for excel);'
        H(6) ='  - tab separator (e.g. for excel);'
        H(7) ='  - tagged (marked up data).'
        IWM=1
        CALL EASKATOG('Column delimeter:',' ',
     &    'spaces','single space','tab','comma','tagged',
     &    'cancel',' ',IWM,7)
        if(iwm.eq.1)then
          delim = '-'
        elseif(iwm.eq.2)then
          delim = 'S'
        elseif(iwm.eq.3)then
          delim = 'T'
        elseif(iwm.eq.4)then
          delim = 'C'
        elseif(iwm.eq.5)then
          delim = 'X'
        elseif(iwm.eq.6)then
          goto 39  ! redisplay menu.
        endif

 41     llt=lnblnk(basefile)
        h(1)='This is the name of the IPV report for the base case'
        h(2)='model (or the model you want to reference against).'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(basefile,'IPV base case report file ?',
     &        ' ',96,'xxx.rep','IPV base report',IER,2)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(basefile,'IPV base case report file ?',
     &        ' ',124,'xxx.rep','IPV base report',IER,2)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(basefile,'IPV base case report file ?',
     &        ' ',144,'xxx.rep','IPV base report',IER,2)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(basefile,'IPV base case report file ?',
     &      ' ',144,'xxx.rep','IPV base report',IER,2)
        else
          CALL EASKF(basefile,'IPV base case report file ?',
     &      ' ',96,'xxx.rep','IPV base report',IER,2)
        endif

C At user's request re-display the menu.
        if(ier.eq.-3)then
          goto 39  ! redisplay menu.
        endif

        lnf=lnblnk(basefile)
        inquire (file=basefile(1:lnf),exist=xst)
        if(xst)then
          call erpfree(ibasefile,istat)
          OPEN (ibasefile,FILE=basefile(1:lnf),ACCESS='SEQUENTIAL',
     &      STATUS='OLD',IOSTAT=ISTAT)
          if(istat.eq.0)then
            call edisp(iuout,'* Base model is')
            call edisp(iuout,basefile)
          else
            call edisp(iuout,'* Problem with base case file!')
            goto 41
          endif
        endif

C Ask the name of the design variant file.
 40     llt=lnblnk(variantfile)
        h(1)='This is the name of the IPV report for the other'
        h(2)='model.'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(variantfile,'IPV variant report file ?',
     &        ' ',96,'xxx.rep','IPV variant report',IER,2)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(variantfile,'IPV variant report file ?',
     &        ' ',124,'xxx.rep','IPV variant report',IER,2)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(variantfile,'IPV variant report file ?',
     &        ' ',144,'xxx.rep','IPV variant report',IER,2)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(variantfile,'IPV variant report file ?',
     &      ' ',144,'xxx.rep','IPV variant report',IER,2)
        else
          CALL EASKF(variantfile,'IPV variant report file ?',
     &      ' ',96,'xxx.rep','IPV variant report',IER,2)
        endif

C At user's request re-display the menu.
        if(ier.eq.-3)then
          goto 39  ! redisplay menu.
        endif

        lnf=lnblnk(variantfile)
        inquire (file=variantfile(1:lnf),exist=xst)
        if(xst)then
          call erpfree(ivarfile,istat)
          OPEN (ivarfile,FILE=variantfile(1:lnf),ACCESS='SEQUENTIAL',
     &      STATUS='OLD',IOSTAT=ISTAT)
          if(istat.eq.0)then
            call edisp(iuout,'* Variant model is')
            call edisp(iuout,variantfile)
          else
            call edisp(iuout,'* Problem with variant file!')
            goto 40
          endif
        endif

C Recover data from the base case file.
C Set test logic to false.
        ipass=1
  42    if(ipass.eq.1)ifu=ibasefile
        if(ipass.eq.2)ifu=ivarfile
        if(ipass.eq.3)then

C We have done both passes, report differences and exit.
          call edisp(iuout,' ')
          call edisp(iuout,'Summary of performance and differences ')
          call edisp(iuout,' ')
          call edisp(iuout,'Demand is reported in kWh/m2/a and kWh/a')
          call edisp(iuout,'Capacity is reported in kW/m2/a and kW/a')
          call edisp(iuout,
     &      'Differences reported between the base case model and the')
          call edisp(iuout,
     &      'variant: positive = savings, negative = not as good as.')
          call edisp(iuout,' ')
          call edisp(iuout,'* For the base case model:')
          if(delim.eq.'-')then
            call edisp(iuout,
     &            '        Demand              Capacity')
            call edisp(iuout,
     &            '        m2     building     m2     building')
          elseif(delim.eq.'T')then
            write(outs,'(7a)') 'Demand m2',tab,'Demand building',tab,
     &        'Capacity m2',tab,'Capacity building'
          elseif(delim.eq.'C')then
            write(outs,'(2a)') 'Demand m2,Demand building,',
     &        'Capacity m2,Capacity building'
          elseif(delim.eq.'S')then
            write(outs,'(2a)') 'Demand m2 Demand building ',
     &        'Capacity m2 Capacity building'
          endif
          call edisp(iuout,outs)
          write(outs,'(a,F8.3,F10.2,F9.3,F10.2)') 'Heat  ',
     &      anu_ht_m2(1),
     &      anu_ht_bld(1),anu_ht_cap_m2(1),anu_ht_cap_bld(1)
          call eddisp(iuout,outs)
          write(outs,'(a,F8.3,F10.2)') 'Therm ',anu_ht_thrm_m2(1),
     &      anu_ht_thrm_bld(1)
          call eddisp(iuout,outs)
          write(outs,'(a,F8.3,F10.2,F9.3,F10.2)') 'Cool  ',
     &      anu_cl_m2(1),
     &      anu_cl_bld(1),anu_cl_cap_m2(1),anu_cl_cap_bld(1)
          call eddisp(iuout,outs)
          call edisp(iuout,'* For the variant model:')
          write(outs,'(a,F8.3,F10.2,F9.3,F10.2)') 'Heat  ',
     &      anu_ht_m2(2),
     &      anu_ht_bld(2),anu_ht_cap_m2(2),anu_ht_cap_bld(2)
          call eddisp(iuout,outs)
          write(outs,'(a,F8.3,F10.2)') 'Therm ',anu_ht_thrm_m2(2),
     &      anu_ht_thrm_bld(2)
          call eddisp(iuout,outs)
          write(outs,'(a,F8.3,F10.2,F9.3,F10.2)') 'Cool  ',
     &      anu_cl_m2(2),
     &      anu_cl_bld(2),anu_cl_cap_m2(2),anu_cl_cap_bld(2)
          call eddisp(iuout,outs)
          write(outs,'(4a)')'Diff between ',
     &      basefile(1:lnblnk(basefile)),
     &      ' & ',variantfile(1:lnblnk(variantfile))
          call edisp(iuout,outs)
          if(delim.eq.'-')then
            call edisp(iuout,
     &            '        Demand              Capacity')
            call edisp(iuout,
     &            '        m2     building     m2     building')
          elseif(delim.eq.'T')then
            write(outs,'(7a)') 'Demand m2',tab,'Demand building',tab,
     &        'Capacity m2',tab,'Capacity building'
          elseif(delim.eq.'C')then
            write(outs,'(2a)') 'Demand m2,Demand building,',
     &        'Capacity m2,Capacity building'
          elseif(delim.eq.'S')then
            write(outs,'(2a)') 'Demand m2 Demand building ',
     &        'Capacity m2 Capacity building'
          endif
          call edisp(iuout,outs)
          anu_ht_m2(3)=anu_ht_m2(1)-anu_ht_m2(2)
          anu_ht_bld(3)=anu_ht_bld(1)-anu_ht_bld(2)
          anu_ht_thrm_m2(3)=anu_ht_thrm_m2(1)-anu_ht_thrm_m2(2)
          anu_ht_thrm_bld(3)=anu_ht_thrm_bld(1)-anu_ht_thrm_bld(2)
          anu_cl_m2(3)=anu_cl_m2(1)-anu_cl_m2(2)
          anu_cl_bld(3)=anu_cl_bld(1)-anu_cl_bld(2)
          anu_tot_bld(3)=anu_tot_bld(1)-anu_tot_bld(2)
          anu_ht_cap_m2(3)=anu_ht_cap_m2(1)-anu_ht_cap_m2(2)
          anu_ht_cap_bld(3)=anu_ht_cap_bld(1)-anu_ht_cap_bld(2)
          anu_cl_cap_m2(3)=anu_cl_cap_m2(1)-anu_cl_cap_m2(2)
          anu_cl_cap_bld(3)=anu_cl_cap_bld(1)-anu_cl_cap_bld(2)
          write(outs,'(a,F8.3,F10.2,F9.3,F10.2)') 'Heat  ',
     &      anu_ht_m2(3),
     &      anu_ht_bld(3),anu_ht_cap_m2(3),anu_ht_cap_bld(3)
          call eddisp(iuout,outs)
          write(outs,'(a,F8.2,F10.2)') 'Therm ',anu_ht_thrm_m2(3),
     &      anu_ht_thrm_bld(3)
          call eddisp(iuout,outs)
          write(outs,'(a,F8.3,F10.2,F9.3,F10.2)') 'Cool  ',
     &      anu_cl_m2(3),
     &      anu_cl_bld(3),anu_cl_cap_m2(3),anu_cl_cap_bld(3)
          call eddisp(iuout,outs)

C Patch for IEA_Annex 46: if comma separated delimeter then also
C write out a single line summary.
          if(delim.eq.'C')then
            call edisp(iuout,
     & 'Location,building heating kWh/a,heating therms,cooling kWh/a')
            write(outs,'(2a,F11.2,a,F11.2,a,F11.2)')
     &        variantfile(1:lnblnk(variantfile)),' ',anu_ht_bld(3),
     &        ' ',anu_ht_thrm_bld(3),' ',anu_cl_bld(3)
            call eddisp(iuout,outs)
          endif

          CALL EASKAB('Comparison options:',' ','return to menu',
     &      'another report',IW,1)
          if(iw.eq.1)then
            return
          else
            call erpfree(ibasefile,istat)
            call erpfree(ivarfile,istat)
            goto 41
          endif
        endif
        atreports=.false.
 142    CALL STRIPC(ifu,OUTSTR,0,ND,1,'*Summary',IER)
        IF(IER.NE.0)goto 2
        if(OUTSTR(1:8).eq.'*Summary')then
          write(outs,'(a,i1,a)') 'pass ',ipass,' found summary'
          call edisp(iuout,outs)
 143      CALL STRIPC(ifu,OUTSTR,0,ND,1,'*report',IER)
          IF(IER.NE.0)goto 1
          if(OUTSTR(1:7).eq.'*report')then
            K=7
            CALL EGETWI(OUTSTR,K,id1,1,98,'W','iget',IIER)
            if(id1.eq.98)then

C We have the performance aggregate
              CALL EGETW(OUTSTR,K,WORD1,'W','third',IFLAG)
              CALL EGETW(OUTSTR,K,WORD2,'W','metric clarification',
     &          IFLAG)
              if(word2(1:11).eq.'performance')then
                atreports=.true.
                im2=.true.
C                call edisp(iuout,'* Found performance m^2.')
              elseif(word2(1:20).eq.'building_performance')then
                atreports=.true.
                im2=.false.
C                call edisp(iuout,'* Found performance building.')
              else
                atreports=.false.
                goto 143
              endif
              if(atreports)then

C found the correct report, skip title, format, fields lines
                CALL STRIPC(ifu,OUTSTR,0,ND,1,'*title',IER)
                CALL STRIPC(ifu,OUTSTR,0,ND,1,'*format',IER)
                CALL STRIPC(ifu,OUTSTR,0,ND,1,'*fields',IER)
                CALL STRIPC(ifu,OUTSTR,0,ND,1,'*data',IER)
                k=5
                if(im2)then
                  CALL EGETWR(OUTSTR,K,anu_ht_m2(ipass),0.,0.,'-',
     &              'heat dmd',IER)
                  anu_ht_thrm_m2(ipass)=anu_ht_m2(ipass)*0.0341296
                  CALL EGETWR(OUTSTR,K,anu_cl_m2(ipass),0.,0.,'-',
     &              'cool dmd',IER)
                else
                  CALL EGETWR(OUTSTR,K,anu_ht_bld(ipass),0.,0.,'-',
     &              'heat dmd',IER)
                  anu_ht_thrm_bld(ipass)=anu_ht_bld(ipass)*0.0341296
                  CALL EGETWR(OUTSTR,K,anu_cl_bld(ipass),0.,0.,'-',
     &              'cool dmd',IER)
                endif

                goto 143
              endif
            elseif(id1.eq.74)then

C We have capacity aggregate
              CALL EGETW(OUTSTR,K,WORD1,'W','third',IFLAG)
              CALL EGETW(OUTSTR,K,WORD2,'W','metric clarification',
     &          IFLAG)
              if(word2(1:8).eq.'capacity')then
                atreports=.true.
                im2=.true.
C                call edisp(iuout,'* Found capacity.')
              elseif(word2(1:17).eq.'building_capacity')then
                atreports=.true.
                im2=.false.
C                call edisp(iuout,'* Found building capacity.')
              else
                atreports=.false.
                goto 143
              endif
              if(atreports)then

C Found the correct report, skip title, format, fields lines
                CALL STRIPC(ifu,OUTSTR,0,ND,1,'*title',IER)
                k=0
                CALL EGETW(OUTSTR,K,WORD1,'W','first',IFLAG)
                CALL EGETP(OUTSTR,K,WORD2,'W','title',IFLAG)
                CALL EGETW(OUTSTR,K,WORD3,'W','unit',IFLAG)
                if(word3(1:5).eq.'W/m^2')then
                  im2=.true.
C                  call edisp(iuout,'* Confirm m^2 capacity.')
                elseif(word3(1:2).eq.'kW')then
                  im2=.false.
C                  call edisp(iuout,'* Confirm building capacity.')
                endif
                CALL STRIPC(ifu,OUTSTR,0,ND,1,'*format',IER)
                CALL STRIPC(ifu,OUTSTR,0,ND,1,'*fields',IER)
                CALL STRIPC(ifu,OUTSTR,0,ND,1,'*data',IER)
                k=5
                if(im2)then
                  CALL EGETWR(OUTSTR,K,anu_ht_cap_m2(ipass),0.,0.,'-',
     &              'heat cap',IER)
                  CALL EGETWR(OUTSTR,K,anu_cl_cap_m2(ipass),0.,0.,'-',
     &              'cool cap',IER)

C Still need to find building capacity report so look further.
                  goto 143
                else
                  CALL EGETWR(OUTSTR,K,anu_ht_cap_bld(ipass),0.,0.,
     &              '-','heat cap',IER)
                  CALL EGETWR(OUTSTR,K,anu_cl_cap_bld(ipass),0.,0.,
     &              '-','cool cap',IER)

C We now have both demand and capacity, increment ipass.
                  ipass=ipass+1
                  goto 42
                endif
              endif
            else

C Not one we are intrested in at the moment.
              goto 143
            endif
          else
            goto 143
          endif
        else
          goto 142
        endif
      elseif(irs.eq.4)then
        return
      endif 

C error state.
  1   call edisp(iuout,'* Reached end of file looking for report!')
      goto 39
  2   call edisp(iuout,'* Reached end of file looking for summary!')
      goto 39
   
      end
        
C ********** checkmodel
C checkmodel check model prior to commissioning a simulation. Return logical
C variables for various topics for possible use by calling routines.
C  mlcok looks for unattributed surface constructions
C  bndryok looks for unattributed surface connections
C  bndrysxc looks for surface connection attribute different from cnn list.
C  confok looks for missing construction files.
C  geofok looks for missing geometry files.
C  oprfok looks for missing/old/unsorted operations files. 
C  prob looks for contiguity cross-references
C  tmcok looks for matching transparent partitions.
C  shdok checks if shading files are readable.
      subroutine checkmodel(mlcok,bndryok,bndrysxc,confok,geofok,
     &                      oprfok,prob,tmcok,shdok)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "CFC_common.h"

      integer lnblnk  ! function definition

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive the modules.
      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)

      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)

      character H*72,CXSTR*78
      CHARACTER*72 LS
      character ZSDES*28,ZSDESC*20,ZSDESS*16,zn*12
      CHARACTER*72 LAPROB

      logical XST,asked,dok,sorted,problem
      logical mlcok,bndryok,bndrysxc,confok,geofok,oprfok,prob,dup
      logical tmcok,shdok
      integer icple,iscple  ! coupled zone and surface for tmc check.
      integer itmc1,itmc2   ! non-zero if surface a tmc

      asked=.false.

      IUO=IFIL+1
      if(nsset.gt.0)isset=1

C Do an initial check to see if the model is attributed and
C constructions are known.
      mlcok=.true.
      lh=0
      do 40 i=1,ncon
        if(SSMLCN(i)(1:7).eq.'UNKNOWN')then
          CALL ZSID(IC1(I),IE1(I),ZSDES,ZSDESC,ZSDESS)
          mlcok=.false.
          if(lh.lt.60)then
            lh=lh+1
            write(h(lh),'(2a)') 'MLC unknown in ',ZSDES
          endif
        endif
 40   continue
      if(.NOT.mlcok)then
        call tstamp('>','PRJ: found unattributed surfaces')
        dok=.true.
        h(1)='A model with UNKNOWN surface construction attributes'
        h(2)='cannot be simulated. You can see where by clicking ok.'
        call askok(
     &    'Some surfaces not fully attributed for constructions. View',
     &    'report of suspect surfaces?',asked,dok,2)
        if(asked)then
          CALL PHELPD('lacking construction attrib',lh,'-',0,0,IER)
        endif
      endif
      lh=0
      bndryok=.true.
      do 41 i=1,ncon
        if(SSOTHER(i,1)(1:7).eq.'UNKNOWN')then
          CALL ZSID(IC1(I),IE1(I),ZSDES,ZSDESC,ZSDESS)
          bndryok=.false.
          if(lh.lt.60)then
            lh=lh+1
            write(h(lh),'(2a)') 'Other side boundary unknown in ',ZSDES
          endif
        endif
 41   continue
      if(.NOT.bndryok)then
        call tstamp('>','PRJ: found some surfaces unattributed')
        dok=.true.
        h(1)='A model with UNKNOWN surface topology attributes'
        h(2)='cannot be simulated. You can see where by clicking ok.'
        call askok(
     &    'Some surfaces not fully attributed for boundary. View',
     &    'list of surfaces with missing attributes?',asked,dok,2)
        if(asked)then
          CALL PHELPD('questioned boundary attrib',lh,'-',0,0,IER)
        endif
      endif
      lh=0
      bndrysxc=.true.
      tmcok=.true.
      do 42 i=1,ncon
        CALL ZSID(IC1(I),IE1(I),ZSDES,ZSDESC,ZSDESS)
        CALL CONXINFO(1,i,CXSTR)
        if(ICT(i).eq.0.and.SSOTHER(i,1)(1:5).ne.'EXTER')then
          bndrysxc=.false.
          if(lh.lt.58)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary looking for EXTER in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection is... ',CXSTR(1:50)
            lh=lh+1
            write(h(lh),'(2a)') 'found surface attribute... ',
     &        SSOTHER(i,1)
          endif
        endif
        if(ICT(i).eq.1.and.SSOTHER(i,1)(1:5).ne.'SIMIL')then
          bndrysxc=.false.
          if(lh.lt.58)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary looking for SIML in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection is... ',CXSTR(1:50)
            lh=lh+1
            write(h(lh),'(2a)') 'found surface attribute... ',
     &        SSOTHER(i,1)
          endif
        endif
        if(ICT(i).eq.2.and.SSOTHER(i,1)(1:5).ne.'CONST')then
          bndrysxc=.false.
          if(lh.lt.58)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary looking for CONST in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection is... ',CXSTR(1:50)
            lh=lh+1
            write(h(lh),'(2a)') 'found surface attribute... ',
     &        SSOTHER(i,1)
          endif
        endif
        if(ICT(i).eq.3.and.SSOTHER(i,1)(1:5).eq.'CONST')then
          bndrysxc=.false.
          if(lh.lt.58)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary unexpected CONST in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection... ',CXSTR(1:50)
            lh=lh+1
            write(h(lh),'(2a)') 'found surface attribute... ',
     &        SSOTHER(i,1)
          endif
        endif
        if(ICT(i).eq.3.and.SSOTHER(i,1)(1:5).eq.'SIMIL')then
          bndrysxc=.false.
          if(lh.lt.58)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary unexpected SIMIL in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection... ',CXSTR(1:50)
            lh=lh+1
            write(h(lh),'(2a)') 'found surface attribute... ',
     &        SSOTHER(i,1)
          endif
        endif
        if(ICT(i).eq.3.and.SSOTHER(i,1)(1:5).eq.'EXTER')then
          bndrysxc=.false.
          if(lh.lt.59)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary unexpected EXTER in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection... ',CXSTR(1:50)
          endif
        endif
        if(ICT(i).eq.3.and.SSOTHER(i,1)(1:5).eq.'GROUN')then
          bndrysxc=.false.
          if(lh.lt.59)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary unexpected GROUN in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection... ',CXSTR(1:50)
          endif
        endif

C Do same check as in solar.F
        if(ICT(i).eq.3)then
          ICPLE=IC2(i)
          ISCPLE=IE2(i)
          ITMC1=ITMCFL(IC1(i),IE1(i))
          ITMC2=ITMCFL(ICPLE,ISCPLE)
          if(ITMC1.NE.0)then
            if(ITMC2.EQ.0)then
              tmcok=.false.
              if(lh.lt.58)then
                lh=lh+1
                write(h(lh),'(2a)') 'Opaque/Transp mismatch in ',ZSDES
                lh=lh+1
                write(h(lh),'(2a)') 'connection is... ',CXSTR(1:50)
                lh=lh+1
                write(h(lh),'(2a)') 'for surface attribute...',SSMLCN(i)
              endif
            endif
          endif
        endif

        if(ICT(i).eq.4.and.SSOTHER(i,1)(1:5).ne.'GROUN')then
          bndrysxc=.false.
          if(lh.lt.58)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary looking for GROUN in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection... ',CXSTR(1:50)
            lh=lh+1
            write(h(lh),'(2a)') 'found surface attribute... ',
     &        SSOTHER(i,1)
          endif
        endif
        if(ICT(i).eq.5.and.SSOTHER(i,1)(1:5).ne.'ADIAB')then
          bndrysxc=.false.
          if(lh.lt.58)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary looking for ADIAB in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection... ',CXSTR(1:50)
            lh=lh+1
            write(h(lh),'(2a)') 'found surface attribute... ',
     &        SSOTHER(i,1)
          endif
        endif
        if(ICT(i).eq.6.and.SSOTHER(i,1)(1:5).ne.'BASES')then

C HOT3000: BASESIMP.
          bndrysxc=.false.
          if(lh.lt.58)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary looking for BASES in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection... ',CXSTR(1:50)
            lh=lh+1
            write(h(lh),'(2a)') 'found surface attribute... ',
     &        SSOTHER(i,1)
          endif
        endif
C HOT3000: BASESIMP.
 42   continue
      if(.NOT.bndrysxc)then
        call tstamp('>','PRJ: found surface attributions inconsistent')
        dok=.true.
        h(1)='A model with inconsistent surface topology attributes'
        h(2)='might fail when simulated. This will list where.'
        call askok(
     &    'Some surface boundary attributions may not be correct. View',
     &    'list of suspect connections?',asked,dok,2)
        if(asked)then
          CALL PHELPD('questioned boundary attrib',lh,'-',0,0,IER)
        endif
      endif

C Check contiguity.
      lh=0
      ICC=0
      prob=.false.
      do 287 IZ=1,NCOMP
        call georead(IFIL+1,LGEOM(IZ),IZ,0,iuout,IER)
        do 289 IS=1,NSUR
          ICC=ICC+1
          CALL ZSID(IZ,IS,ZSDES,ZSDESC,ZSDESS)
          if(IC1(ICC).ne.IZ)prob=.true.
          if(IE1(ICC).ne.IS)prob=.true.
          if(lh.lt.60.and.prob)then
            lh=lh+1
            write(h(lh),'(2a)') 'Zone surface <> master list ',ZSDES
          endif
  289   continue
  287 continue

C Check to see if partitions match. See if other zone/surface
C exists and what it points to.
      do 290 i=1,NCON
        CALL CONXINFO(1,i,CXSTR)
        if(ICT(i).eq.3)then
          if(IC2(i).gt.0.and.IC2(i).le.NCOMP.and.
     &      IE2(i).gt.0.and.IE2(i).le.NZSUR(IC2(i)))then
            ioc=IZSTOCN(IC2(i),IE2(i))
            if(ioc.ne.0)then
              if(ICT(ioc).ne.3)then
                prob=.true.
              elseif(IC2(ioc).ne.IC1(i))then
                prob=.true.
              elseif(IE2(ioc).ne.IE1(i))then
                prob=.true.
              endif
              if(lh.lt.60.and.prob)then
                lh=lh+1
                write(h(lh),'(2a)') 'Zone surface <> master list ',
     &            CXSTR(1:40)
              endif

C Check to see if this connection only pointed to once in the whole list.
              dup=.false.
              do 291 i2=1,NCON
                if(IC2(i2).eq.IC1(i).and.IE2(i2).eq.IE1(i))then
                  if(.NOT.dup)then
                    if(ICT(i2).eq.3)dup=.true.
                  else
                    prob=.true.
                  endif
                endif
  291         continue
              if(lh.lt.60.and.dup)then
                lh=lh+1
                write(h(lh),'(2a)') 
     &            'Duplicates surface <> master list ',CXSTR(1:36)
              endif
            endif
          else
            prob=.true.
            if(lh.lt.60)then
              lh=lh+1
              write(h(lh),'(2a)') 'Zone surface <> master list ',
     &          CXSTR(1:40)
            endif
          endif
        endif
  290 continue
      if(prob)then
        call tstamp('>','PRJ: found topology inconsistent')
        call usrmsg('Topology found to be inconsistent. This',
     &  'should be resolved before running a simulation.','W')
        CALL PHELPD('boundary checks',lh,'-',0,0,IER)
      endif

C Now look for unknown files.
      lh=0
      geofok=.true.
      do 46 i=1,ncomp
        if(LGEOM(i)(1:7).eq.'UNKNOWN'.or.LGEOM(i)(1:2).eq.'  ')then
          geofok=.false.
          if(lh.lt.60)then
            lh=lh+1
            write(h(lh),'(3a)') 'Zone ',zname(i)(1:lnzname(i)),
     &        ' geometry undefined.'
          endif
        else
          XST=.false.
          call FINDFIL(LGEOM(i),XST)
          if(.NOT.XST)then
            if(lh.lt.60)then
              lh=lh+1
              write(h(lh),'(3a)') 'Zone ',zn(1:lnblnk(zn)),
     &        ' geometry not found.'
            endif
          endif
        endif
 46   continue
      if(.NOT.geofok)then
        call usrmsg(
     &    'Some zones do not reference a geometry file. The',
     &    'model will not allow thermal simulation.','W')
        CALL PHELPD('geometry checks',lh,'-',0,0,IER)
      endif
      lh=0
      oprfok=.true.
      do 43 i=1,ncomp
        if(LPROJ(i)(1:7).eq.'UNKNOWN'.or.LPROJ(i)(1:2).eq.'  ')then
          oprfok=.false.
          if(lh.lt.60)then
            lh=lh+1
            write(h(lh),'(3a)') 'Zone ',zname(i)(1:lnzname(i)),
     &        ' operations undefined.'
          endif
        else
          XST=.false.
          call FINDFIL(LPROJ(i),XST)
          if(.NOT.XST)then
            if(lh.lt.60)then
              lh=lh+1
              write(h(lh),'(3a)') 'Zone ',zn(1:lnblnk(zn)),
     &        ' operations not found.'
              oprfok=.false.
            endif
          else
            CALL ERPFREE(IUO,ISTAT)
            CALL EROPER(ITRC,ITRU,IUO,I,IER)

C Do cursory check to see if the file is sorted.
            problem=.false.
            call checksort(i,1,problem,ier)
            if(problem) sorted=.false.
            problem=.false.
            call checksort(i,2,problem,ier)
            if(problem) sorted=.false.
            problem=.false.
            call checksort(i,3,problem,ier)
            if(problem)then
              oprfok=.false.
            endif
          endif
        endif
  43  continue
      if(.NOT.oprfok)then
        call usrmsg(
     &  'Some zones lack operation details or have outdated files.',
     &  'The model will not support thermal simulation.','W')
        CALL PHELPD('operations checks',lh,'-',0,0,IER)
      endif
      lh=0
      confok=.true.
      do 45 i=1,ncomp
        if(LTHRM(i)(1:7).eq.'UNKNOWN'.or.LTHRM(i)(1:2).eq.'  ')then
          confok=.false.
          if(lh.lt.60)then
            lh=lh+1
            write(h(lh),'(3a)') 'Zone ',zname(i)(1:lnzname(i)),
     &        ' constructions undefined.'
          endif
        else
          XST=.false.
          call FINDFIL(LTHRM(i),XST)
          if(.NOT.XST)then
            if(lh.lt.60)then
              lh=lh+1
              write(h(lh),'(3a)') 'Zone ',zn(1:lnblnk(zn)),
     &        ' constructions not found.'
            endif
          else

C Zone construction file exists so attempt to scan it.
            call georead(IFIL+1,LGEOM(I),I,0,iuout,IER)
            CALL ECONST(LTHRM(I),IFIL+1,I,0,IUOUT,IER)
            if(ier.ne.0)then
              if(lh.lt.60)then
                lh=lh+1
                write(h(lh),'(3a)') 'Zone ',zn(1:lnblnk(zn)),
     &          ' constructions did not read cleanly.'
              endif
            endif
            if(ITW(I).eq.1)then
              CALL ERTWIN(0,IUOUT,IFIL+1,LTWIN(I),I,IER)
              if(ier.ne.0)then
                if(lh.lt.60)then
                  lh=lh+1
                  write(h(lh),'(3a)') 'Zone ',zn(1:lnblnk(zn)),
     &            ' tmc file did not read cleanly.'
                endif
              endif
            endif
            if(icfc(I).eq.1)then
              CALL read_in_cfc_file(0,IUOUT,IFIL+1,lcfcin(I),I,IER)
              if(ier.ne.0)then
                if(lh.lt.60)then
                  lh=lh+1
                  write(h(lh),'(3a)') 'Zone ',zn(1:lnblnk(zn)),
     &            ' cfc file did not read cleanly.'
                endif
              endif
            endif
          endif
        endif
 45   continue
      if(.NOT.confok)then
        call usrmsg(
     &   'Some zones do not reference a construction file. The',
     &   'model will not allow thermal simulation.','W')
        CALL PHELPD('constructions checks',lh,'-',0,0,IER)
      endif
      lh=0
      shdok=.true.
      IUO=IFIL+1
      do 52 i=1,ncomp
        if(isi(i).eq.1)then
          LS=LSHAD(I)
          if(LS(1:7).eq.'UNKNOWN'.or.LS(1:2).eq.'  ')then
            shdok=.false.
            if(lh.lt.60)then
              lh=lh+1
              write(h(lh),'(3a)') 'Zone ',zname(i)(1:lnzname(i)),
     &        ' shading undefined.'
            endif
          else
            XST=.false.
            call FINDFIL(LS,XST)
            if(.NOT.XST)then
              if(lh.lt.60)then
                lh=lh+1
                write(h(lh),'(3a)') 'Zone ',zn(1:lnblnk(zn)),
     &        ' shading not found.'
              endif
            else

C Attempt to read the shading file as done in esrubld/util.F subroutine
C mzshin.
              maxrec=24
              if(nzsur(i).gt.24) maxrec=nzsur(i)
              CALL ERPFREE(IFIL+1,ISTAT)
              call EFOPRAN(IFIL+1,LS,maxrec,1,IER)
              IF(ier.ne.0)goto 1000

C Check file has been written to. If a problem the try again
C at maxrec=24 for older files. Remember maxrec for use in solar.F.
              IREC=2
              READ(IFIL+1,REC=IREC,IOSTAT=ISTAT,ERR=1001)INEXT,NS
              if(INEXT.LE.3.OR.NS.NE.nzsur(i))then
                CALL ERPFREE(IFIL+1,ISTAT)
                maxrec=24
                call EFOPRAN(IFIL+1,LS,maxrec,1,IER)
                IF(ier.ne.0)goto 1000
                IREC=2
                READ(IFIL+1,REC=IREC,IOSTAT=ISTAT,ERR=1001)INEXT,NS
                if(INEXT.LE.3.OR.NS.NE.nzsur(i))then
                  CALL ERPFREE(IFIL+1,ISTAT)
                  maxrec=MS
                  call EFOPRAN(IFIL+1,LS,maxrec,1,IER)
                  IF(ier.ne.0)goto 1000
                  IREC=2
                  READ(IFIL+1,REC=IREC,IOSTAT=ISTAT,ERR=1001)INEXT,NS
                  if(INEXT.LE.3.OR.NS.NE.nzsur(i))goto 1003
                endif
              endif
            endif
          endif
        endif
        goto 52

 1000   continue
 1001   IER=1
        if(lh.lt.59)then
          lh=lh+1
          write(h(lh),'(2A)') zname(I),' shading file error in...'
          lh=lh+1
          write(h(lh),'(A)') LS(1:lnblnk(LS))
        endif
        shdok=.false.
        goto 52

 1003   if(lh.lt.59)then
          lh=lh+1
          write(h(lh),'(a,i2,a,i2,a,i2)') 'Model nsur=',NSUR,' db ns=',
     &      ns,' inext=',INEXT
          lh=lh+1
          write(h(lh),'(A)') 'Mismatch between model and shading file.'
        endif
        shdok=.false.
 52   continue
      if(.NOT.shdok)then
        call usrmsg(
     &    'Some zones have missing or corrupt shading files. The',
     &    'model not yet complete vis-a-vis thermal simulation.','W')
        CALL PHELPD('shading checks',lh,'-',0,0,IER)
      endif

      return
      end

C ********** imgdisp
C imgdisp displays images associated with start-up or at specific points
C and the image browser has not been invoked do this now. In the case
C of the GTK version use an in-built GTK function rather than an
C external tool.
      subroutine imgdisp(iforce,focus,ier)
#include "building.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      common/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      common/CFGV/icfgv

C A set of possible image formats which can be accepted within
C a model configuration file. These definitions are held in the
C .esprc file. To add additional image types and/or 3rd
C party display applications edit the ESP-r Install script which
C creates the .esprc file.
C   imgtyp is the number of different image formats supported
C   fmttag (4 char) is a tag for each image formt (e.g. GIF, XBMP)
C   fmtexe (20 char) is the application name used to display
C          images of type fmttag.
      common/showimg/imgtyp,fmttag(5),fmtexe(5)

C Images in the model.
C  imgfmt (4 char) gives the format of each image associated with
C         the model, it must match one of the known fmttag.
C  imgfoc (4 char) associates an image with a specific topic:
C         'FZON' is related to zone composition
C         'FNET' is related to network composition
C         'FCTL' is related to control composition
C         'FDFS' is related to CFD domains
C         'FPER' is related to predicted performance
C         '****' is a general image displayed at startup
C  limgfil (72 char) name of the image file << ?? longer string >>
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton

      character ilist*200,dolist*254
      character fmttag*4,fmtexe*20,tfile*72,focus*4
      character longtfile*144,longtfiledos*144
      character topic*248,head*136,act*1,h*72
      integer iglib  ! if one then X11 if 2 then GTK if 3 text only.
      integer idl  ! length of image documentation

      logical concat,show,unixok

C Check if Unix-based or DOS based.
      call isunix(unixok)

C Loop through the images and display those which match the 'focus'
C and can be displayed with a known utility. Iforce, if set to 1,
C forces display.
      if(noimg.eq.0)return
      if(imgtyp.eq.0)then
        call edisp(iuout,'* No image display application available.')
        call edisp(iuout,'Ask your administrator to define one via')
        call edisp(iuout,'your .esprc file.')
        return
      endif

      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1)then

C If X11 version then request an external agent to display the image.
        if((iton.eq.0.or.iforce.eq.1))then
          do 498 imgv=1,imgtyp
            ilist=' '
            ix=1
            do 49 img=1,noimg
              if(imgfmt(img)(1:3).eq.fmttag(imgv)(1:3))then

C If focus is `****` then show images marked with `****` , otherwise
C attempt to match the focus with the image. 
                show=.false.
                if(imgfoc(img)(1:4).eq.focus(1:4))show=.true.
                if(show)then
                  longtfile=' '
                  if(unixok)then
                    call addpath(limgfil(img),longtfile,concat)
                  else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.  If X11 then an external application is used
C an double quoting is useful. If GTK then internal function used and
C quotes are not required.
                    call addpath(limgfil(img),longtfile,concat)
                    if(iglib.eq.1)then
                      call cmdfiledos(longtfile,longtfiledos,ier)
                    elseif(iglib.eq.2)then
                      call imgfiledos(longtfile,longtfiledos,ier)
                    endif
                    longtfile=' '
                    longtfile=longtfiledos
                  endif
                  ixl=lnblnk(longtfile)
                  ixe=ix+ixl
                  if(ixe.le.200)then
                    WRITE(ilist(ix:ixe),'(a,2x)')longtfile(1:ixl)
                  endif
                  ix=ixe+1
                endif
              endif
 49         continue
            if(ix.gt.1)then
              dolist=' '
              if(fmtexe(imgv)(1:2).eq.'  ')then
                call usrmsg(
     &    'The image display tool has not be defined. Ask your',
     &    'administrator to update the .esprc file definition.','W')
              else

C If recent version then display image documentation as well.
                if(icfgv.gt.3)then
                  call edisp(iuout,'  ')
                  call edisp248(iuout,imgdoc(imgv),100)
                endif
                write(dolist,'(a,2x,a,a)')
     &            fmtexe(imgv)(1:lnblnk(fmtexe(imgv))),
     &            ilist(1:lnblnk(ilist)),' &'
                call runit(dolist,'graph')
                iton=1
              endif
            endif
 498      continue
        endif
      elseif(iglib.eq.2)then

C If GTK version then use GTK calls to display images in popup.
        if((iton.eq.0.or.iforce.eq.1))then

C Setup help text for the popup.
          h(1)='Images can be associated with a model for various'
          h(2)='purposes: '
          h(3)=' * document conditions at the physical site'
          h(4)=' * document assumptions in the model'
          h(5)=' * indicate what performance is expected'
          h(6)='   '
          h(7)='Images are added to a model via the context->images'
          write(h(8),'(a,i2,a)')
     &      'menu item. There can be up to ',MIMG,' images associated'
          h(9)='with a model. '
          h(10)='Images of type GIF and XBM and JPG are supported.'
          CALL PHELPW(10,IHW,IER)

          do 99 img=1,noimg
            if(imgfmt(img)(1:3).eq.'GIF'.or.
     &         imgfmt(img)(1:4).eq.'XBMP'.or.
     &         imgfmt(img)(1:3).eq.'TIF'.or.
     &         imgfmt(img)(1:3).eq.'JPG')then

C If focus is `****` then show images marked with `****` , otherwise
C attempt to match the focus with the image. Create a string topic
C to pass to the display routine along with the image file name.
C << tag for editing ?? >>
              show=.false.
              if(imgfoc(img)(1:4).eq.focus(1:4))show=.true.
              if(show)then
                longtfile=' '
                if(unixok)then
                  call addpath(limgfil(img),longtfile,concat)
                else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
                  call addpath(limgfil(img),longtfile,concat)
                  call cmdfiledos(longtfile,longtfiledos,ier)
                  longtfile=' '
                  longtfile=longtfiledos
                endif

C If version 4 or newer then display the text otherwise make
C up a general block of text for the image.
                if(icfgv.gt.3)then
                  idl=lnblnk(imgdoc(img))
                  write(topic,'(a)') imgdoc(img)(1:idl)
                else
                  if(focus(1:4).eq.'****')then
                    write(topic,'(3a)') ' Image ',longtfile(1:ixl),
     &                ' is a general topic.'
                  elseif(focus(1:4).eq.'FZON')then
                    write(topic,'(3a)') ' Image ',longtfile(1:ixl),
     &                ' provides details of zone composition.'
                  elseif(focus(1:4).eq.'FNET')then
                    write(topic,'(3a)') ' Image ',longtfile(1:ixl),
     &                ' provides details of network composition.'
                  elseif(focus(1:4).eq.'FCTL')then
                    write(topic,'(3a)') ' Image ',longtfile(1:ixl),
     &                ' provides details of control composition.'
                  elseif(focus(1:4).eq.'FDFS')then
                    write(topic,'(3a)') ' Image ',longtfile(1:ixl),
     &                ' provides details of the CFD domain.'
                  elseif(focus(1:4).eq.'FPER')then
                    write(topic,'(3a)') ' Image ',longtfile(1:ixl),
     &                ' relates to model performance.'
                  endif
                endif

C Provide some feedback, set the title of the popup and then call C
C function in lib/esp_draw.c
                call edisp(iuout,topic)
                write(head,'(2a)') 'Image: ',longtfile(1:ixl)
                act='-'
                if(MMOD.eq.8)then
                  call popupimage(head,topic,act,longtfile)
                  iton=1
                else
                  call edisp(iuout,'Not in graphics mode.')
                endif
              endif
            endif
  99      continue
        endif
      endif

      return
      end

C ********************* CFGVER 
C CFGVER supports the creation of model variants.
C It copies a project and gives it a new name   
C thereby facilitating creatiion of multiple versions
C of the same project that can later be modified.

      SUBROUTINE CFGVER
#include "building.h"
#include "model.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/user/browse
      common/C21/IFCFG,cfgroot,LCFGF

      dimension items(7),items2(33)
      CHARACTER*72 H,LCFGF
      CHARACTER ITEMS*36,ITEMS2*36,APP*6
      CHARACTER NBSTR*6,CFGROOT*24,OUTS*124
      LOGICAL BROWSE
      integer MITEM,INO,MITEMS2,KNO ! max items and current menu item

      IF(BROWSE)THEN
        CALL EDISP(IUOUT,
     &      '* Variant Manager does not operate in browse mode!')
        RETURN
      ENDIF

C Help message for version manager menu
      H(1) = 'This menu supports the creation of design variants.'
      H(2) = 'The assumption is that a working model exists from'
      H(3) = 'which other permutations can be derived.'
      H(4) = ' '
      H(5) = 'Variants may be created in the following ways.'
      H(6) = ' '
      H(7) = 'Project Variant: asks specific questions about the '
      H(8) = 'changes the user wishes to make to the model. The'
      H(9) = 'changes once implemented are saved to new model'
      H(10)= 'files, i.e. the original model is preserved.'
      H(11)= ' '
      H(12)= 'For example, a variant corresponding to a different'
      H(13)= 'location will require a different climate file,'
      H(14)= 'modified ground temperatures and recalculated shading'
      H(15)= 'factors.'
      H(16)= ' '
      H(17)= 'As another example, a variant with alternative'
      H(18)= 'constructions may requires new geometry, construction'
      H(19)= 'and optical property files.'
      H(20)= ' '
      H(21)= 'Several changes may be applied within a single'
      H(22)= 'variant.'
      H(23)= ' '
      H(24)= 'A five character postfix is requested from the'
      H(25)= 'user and this is appended to the names of new files.'
      H(26)= ' '
      H(27)= ' '
      H(28)= '*****SBEM Notional Model*****'
      H(29)= ' ESP-r creates the SBEM notional model in compliance with'
      H(30)= ' CIBSE TM33 ??? This creates a model with the postfix'
      H(31)= ' _not [i.e. filname.cfg is copied to filename_not.cfg]'
      H(32)= ' '
      H(33)= ' Details of how the notonal building relates to the '
      H(34)= ' original building and plant model is defined in relevant'
      H(35)= ' SBEM documentation'
      H(36)= ' '

C Ask for changes to be made
 200  INO=-3
      ITEMS(1) = 'a make a project variant'
      ITEMS(2) = 'b other 0 (under developmnet)'
      ITEMS(3) = 'c other 1 (under development)'
      ITEMS(4) = 'd other 2 (under development)'
      ITEMS(5) = ' --------------------------'
      ITEMS(6) = '? help'
      ITEMS(7) = '- exit'
      MITEM=7
      CALL EMENU('Variants Manager',ITEMS,MITEM,INO)
      IF(INO.EQ.1)THEN
        APP='_xyz  '
        CALL EASKS(APP,' ',
     &     'Specify up to 6 characters to append to project name?',
     &     6,'project','root name modifier',IER,36)
        K=0
        CALL EGETRM(APP,K,NBSTR,'W','cfg modifier ',IER)
        APP=NBSTR
        WRITE(OUTS,'(4A)')'* New project name will be `',
     &        CFGROOT(1:LNBLNK(CFGROOT)),APP,'`.'
        CALL EDISP(IUOUT,OUTS)

C Change string holding cfg description
        CALL EASKS(modeltitle,' ','Description for this variant?',
     &     72,'project','description modifier',IER,35)
C        CALL EDISP(IUOUT,'* New project description will be ')
C        CALL EDISP(IUOUT,modeltitle)

C Set up menu for different variants
        LRCR=0
 201    KNO=-3
        ITEMS2(1) = ' ___ Site and location ___'
        ITEMS2(2) = 'a global tasks and site exposure'
        ITEMS2(3) = 'b site exposure & ground reflectance'
        ITEMS2(4) = 'c climate, latitude and longitude'
        ITEMS2(5) = 'd pressure distribution'
        ITEMS2(6) = 'e ground temperature profiles'
        ITEMS2(7) = ' ___ Form and fabric ___'
        ITEMS2(8) = 'f geometry and attribution'
        ITEMS2(9) = 'g shading and insolation'
        ITEMS2(10)= 'h view factors and radiant sensors'
        ITEMS2(11)= 'i glazed area'
        ITEMS2(12)= 'j zone constructions'
        ITEMS2(13)= 'k zone materials properties'
        ITEMS2(14)= 'l computational fluid dynamics'
        ITEMS2(15)= 'm convection regimes'
        ITEMS2(16)= 'n active materials/advanced optics'
        ITEMS2(17)= 'o adaptive gridding and moisture'
        ITEMS2(18)= 'p building integrated renewables'
        ITEMS2(19)= 'q integrated performance view'
        ITEMS2(20)= '___ Networks ___'
        ITEMS2(21)= 'r fluid flow'
        ITEMS2(22)= 's electrical'
        ITEMS2(23)= 't plant'
        ITEMS2(24)= 'u contaminants'
        ITEMS2(25)= '___ Usage ___'
        ITEMS2(26)= 'v zone operations'
        ITEMS2(27)= 'w controls'
        ITEMS2(28)= 'x casual gains control'
        ITEMS2(29)= 'y event profiles'
        ITEMS2(30)= 'z mould and mycotoxins'
        ITEMS2(31)=' --------------------------------'
        ITEMS2(32)='? help'
        ITEMS2(33)='- exit this menu'
        MITEMS2=33

C Help.
        h(1) ='Change options:'
        h(2) =' global tasks - work in progress - use with caution.'
        h(3) =' '
        h(4) =' site exposure & ground reflectivity - modifies these'
        h(5) ='   values within revised configuration file.'
        h(6) =' '
        h(7) =' geometry & attribution - copies zone files so that'
        h(8) ='   subsequent changes are saved to new files. If there'
        h(9) ='   are viewfactor, shading, obstruction, hc regime or'
        h(10)='   or casual gain ctl files these are copied as'
        h(11)='   well as revised configuration and connections files.'
        h(12)=' '
        h(13)=' shading - if selected will duplicate and then update'
        h(14)='   existing shading files e.g. a change in site.'
        h(15)=' '
        h(16)=' climate data & latitude/longitude - finds dependencies'
        h(17)='   associated with such changes (ground temps, shading,'
        h(18)='   IPV) and updates the model accordingly.'
        h(19)=' '
        h(20)=' pressure distributions - either alters cp definitions'
        h(21)='   (so-called cpcalc method) or copies and then edits'
        h(22)='   pressure distributions database.'
        h(23)=' '
        h(24)=' glazed area - not yet operational'
        h(25)=' '
        h(26)=' zone constructions - copies geometry, construction '
        h(27)='   and tmc files to hold subsequent changes by the'
        h(28)='   user. Also creates variant connections and model'
        h(29)='   configuration file. If number of surfaces will'
        h(30)='   be altered select `geometry & attribution` first.'
        h(31)=' '
        h(32)=' materials database - makes copy of current database'
        h(33)='   to hold subsequent changes by the user.'
        h(34)=' '
        h(35)=' cfd domain - makes copy of current domains to hold'
        h(36)='   subsequent changes by the user.'
        h(37)=' '
        h(38)=' ground temperature profiles - select this if site'
        h(39)='   details are being altered.'
        h(40)=' '
        h(41)=' convection regimes - make copy of current regimes'
        h(42)='   to hold subsequent changes by the user'
        h(43)=' '
        h(44)=' active materials - not yet implemented'
        h(45)=' adaptive gridding - not yet implemented'
        h(46)=' '
        h(47)=' building integrated reneuables - not yet implemented'
        h(48)=' '
        h(49)=' integrated performance view - select this to update'
        h(50)='   documentation and title (probably should be done'
        h(51)='   in conjunction with any other variant task).'
        h(52)=' '
        h(53)=' flow network - copy current network to hold'
        h(54)='   subsequent user changes.'
        h(55)=' '
        h(56)=' schedules - copy current zone operations to hold'
        h(57)='   subseuent user changes.'
        h(58)=' '
        CALL EMENU('Model Variants',ITEMS2,MITEMS2,KNO)

        IF(KNO.EQ.2)THEN

C Site and location
C global tasks and site exposure 
          CALL VERMAN(APP,1,LRCR)
        ELSEIF(KNO.EQ.3)THEN

C site exposure and ground reflectance
          CALL VERMAN(APP,3,LRCR)
        ELSEIF(KNO.EQ.4)THEN

C climate, latitiude and longitude
          CALL VERMAN(APP,6,LRCR)
        ELSEIF(KNO.EQ.5)THEN

C pressure distributions
          CALL VERMAN(APP,7,LRCR)
        ELSEIF(KNO.EQ.6)THEN

C ground temperature profiles
          CALL VERMAN(APP,12,LRCR)
        ELSEIF(KNO.EQ.8)THEN

C geometry and attribution
          CALL VERMAN(APP,2,LRCR)
        ELSEIF(KNO.EQ.9)THEN

C shading and insolation
          CALL VERMAN(APP,4,LRCR)
        ELSEIF(KNO.EQ.10)THEN

C view factors and radiant sensors
          CALL VERMAN(APP,5,LRCR)
        ELSEIF(KNO.EQ.11)THEN

C Form and fabric information
C glazed area
          CALL VERMAN(APP,8,LRCR)
        ELSEIF(KNO.EQ.12)THEN

C variant zone constructions
          CALL VERMAN(APP,9,LRCR)
        ELSEIF(KNO.EQ.13)THEN

C variant material database
          CALL VERMAN(APP,10,LRCR)
        ELSEIF(KNO.EQ.14)THEN

C CFD domain
          CALL VERMAN(APP,11,LRCR)
        ELSEIF(KNO.EQ.15)THEN

C convection calculations
          CALL VERMAN(APP,13,LRCR)
        ELSEIF(KNO.EQ.16)THEN

C active materials and advanced optics
          CALL VERMAN(APP,14,LRCR)
        ELSEIF(KNO.EQ.17)THEN

C adaptive gridding and moisture
          CALL VERMAN(APP,15,LRCR)
        ELSEIF(KNO.EQ.18)THEN

C building integrated renewables
          CALL VERMAN(APP,16,LRCR)
        ELSEIF(KNO.EQ.19)THEN

C integrated performance view
          CALL VERMAN(APP,17,LRCR)
        ELSEIF(KNO.EQ.21)THEN
         
C fluid flow network
          CALL VERMAN(APP,18,LRCR)
        ELSEIF(KNO.EQ.22)THEN

C electrical network
          CALL VERMAN(APP,19,LRCR)
        ELSEIF(KNO.EQ.23)THEN

C plant components / plant network
          CALL VERMAN(APP,20,LRCR)
        ELSEIF(KNO.EQ.24)THEN

C contaminants network
          CALL VERMAN(APP,21,LRCR)
        ELSEIF(KNO.EQ.26)THEN

C Usage information
C zone operations
          CALL VERMAN(APP,22,LRCR)
        ELSEIF(KNO.EQ.27)THEN

C controls
          CALL VERMAN(APP,23,LRCR)
        ELSEIF(KNO.EQ.28)THEN

C casual gains control
          CALL VERMAN(APP,24,LRCR)
        ELSEIF(KNO.EQ.29)THEN

C event profiles
          CALL VERMAN(APP,25,LRCR)
        ELSEIF(KNO.EQ.30)THEN

C mould and mycotoxins
          CALL VERMAN(APP,26,LRCR)
        ELSEIF(KNO.EQ.32)THEN
          CALL PHELPD('version tasks',58,'-',0,0,IER)
          GOTO 201
        ELSEIF(KNO.EQ.33)THEN
          GOTO 200
        ENDIF
        IF(LRCR.EQ.2)RETURN
        GOTO 201
      ELSEIF(INO.EQ.6)THEN
        CALL PHELPD('version intro',36,'-',0,0,IER)
        GOTO 200
      ELSEIF(INO.EQ.7)THEN
        RETURN
      ELSE
        GOTO 200
      ENDIF
      RETURN
      END

C ********************* VERMAN 
C VERMAN copies various ESP-r files and names them uniquely
C in order to build multiple variants of a base case model
C The root name is modified with the three character string APE appended
C to it and the configuration file is always written out.
C For the various other files in the project model, data is first read
C into common blocks and the file name is changed using APE and the same
C data is written out to the new file.
C<< It is intended that the data be modified interactively before
C<< writing out to new files in the future and this has been implemented
C<< for some of the options included in this subroutine.
C For the following values of ILM the respective file will be copied and
C renamed with APE character string appendage
C ILM = 1 means global tasks and site exposure
C ILM = 2 means geometry and attribution
C ILM = 3 means site exposure & ground reflectance
C ILM = 4 means shading and insolation and obstructions
C ILM = 5 means view factors and radiant sensors
C ILM = 6 means climate, latitude and longitude
C ILM = 7 means pressure distribution
C ILM = 8 means glazed area
C ILM = 9 means variant zone constructions
C ILM = 10 means variant material database
C ILM = 11 means computational fluid dynamics
C ILM = 12 means ground temperature profiles
C ILM = 13 means convection calculations
C ILM = 14 means active materials and advanced optics
C ILM = 15 means adaptive gridding and moisture
C ILM = 16 means building integrated renewables
C ILM = 17 means integrated performance view
C ILM = 18 means fluid flow network
C ILM = 19 means electrical network
C ILM = 20 means plant components / plant network
C ILM = 21 means contaminants network
C ILM = 22 means zone operations
C ILM = 23 means controls
C ILM = 24 means casual gains control
C ILM = 25 means event profiles
C ILM = 26 means mould and mycotoxins (currently not implemented)

C << Check existence of all files if non-existant create filenames with 
C << message to user that such a file does not exist

      SUBROUTINE VERMAN(APE,ILM,LRCR)
#include "building.h"
#include "model.h"
#include "site.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "ipvdata.h"
      
      integer lnblnk  ! function definition

C Path to model and command line file (if any). 
      common/OUTIN/IUOUT,IUIN
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/C21/IFCFG,cfgroot,LCFGF
      common/FILEP/IFIL
      common/IPVF/lipvdatf
      common/CFGV/icfgv
      common/pophelp/h(60)

      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres

      common/cctlnm/ctldoc,lctlf
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)

      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/CONTM/CNTMFIL,CNTMDESC,NTSTEPC

      DIMENSION IVALS(MCOM),IVALSS(8),SALT(8)

C ivalsv keeps track of new viewfactor files which need analysys
C ivalsg keeps track of whether a geometry file has alread been copied.
      dimension ivalsv(MCOM),ivalsg(MCOM)

      character*72 sblres,sflres,splres,smstres,sipvres,selres
      character CNTMFIL*72
      CHARACTER*72 LIPVDATF,NNAME,H,LCFGF,msg1,MSG2,LCFD
      character ctldoc*248,lctlf*72
      CHARACTER*72 LAPROB
      CHARACTER*24 cfgroot
      CHARACTER OUTSTR*124
      CHARACTER APE*6,EXT*4,OUTS*124,doit*248,tmode*8
      CHARACTER SALT*31,CNTMDESC*124
      character ipvaction*3  ! to signal external ipv file
      character lworking*144,fs*1
      integer irootlen,iapelin  ! string buffer lengths
      LOGICAL XST,OK,DOK,modsit,moddb,QUIET,docnn,silent,donegeo
      logical unixok
      logical newgeo  ! to use for testing if new/old geometry file.
      logical updatetitle ! if true the edit IPV titles.
      integer IW  ! for radio button
#ifdef OSI
      integer ICPMOD     ! passed from tchild
#else
      integer*8 ICPMOD     ! passed from tchild
#endif

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      M=ILM
      call isunix(unixok)

C Assume that a new connections file does not need to be created.
      docnn=.false.
      newgeo=.false.  ! assume older format geometry.

C Change name of configuration file
      IF(LRCR.eq.0)THEN
        EXT='.cfg'
        CALL FNCNGR(LCFGF,APE,EXT,NNAME)
        LCFGF=NNAME
      ENDIF

      IF(M.EQ.1)THEN

C Site and location:
C global tasks (work in progress APE needs sorting).
C        ITRC=1
C        ITRU=6
C        CALL EDZCOMP(ITRC,ITRU,APE,IER)
        h(1)='global tasks currently not supported' 
        CALL PHELPD('global tasks not supported',1,'-',0,0,IER)
        msg1='updating model for global tasks...'
      ELSEIF(M.EQ.2)THEN

C geometry and attribution
        inpic=0
        call askmultizone(inpic,ivals,
     &    ' Select zones for geometric variant?',
     &    'Zone geometric variant','-',ier)
        if(inpic.gt.0)then
          DO 17 IC=1,inpic
            IUF=IFIL+2
            ICOMP=ivals(IC)
            EXT='.geo'
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            call georead(IUF,LGEOM(ICOMP),ICOMP,1,iuout,IER)
            CALL FNCNGR(LGEOM(ICOMP),APE,EXT,NNAME)
            LGEOM(ICOMP)=NNAME
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IUF,LGEOM(ICOMP),IComp,IUOUT,3,IER)
            else
              call emkgeo(IUF,LGEOM(ICOMP),ICOMP,3,IER)
            endif

C Remember this geometry file has been done so that a later selection
C of constructions does not re-do it.
            ivalsg(ic)=ivals(IC)

C If there is an existing shading file copy it to new name
C so that subsequent changes are applied to the new file.

C << what this logic does not yet do is check and see if there is an
C << ascii version of the shading file and copy that as well.

            if(ISI(icomp).eq.1)then
              EXT='.shd'
              CALL FNCNGR(LSHAD(ICOMP),APE,EXT,NNAME)
              if(unixok)then
                WRITE(doit,'(4A)')'cp ',
     &            LSHAD(ICOMP)(1:LNBLNK(LSHAD(ICOMP))),' ',
     &            NNAME(1:LNBLNK(NNAME))
              else
                WRITE(doit,'(4A)')'copy /y ',
     &            LSHAD(ICOMP)(1:LNBLNK(LSHAD(ICOMP))),' ',
     &            NNAME(1:LNBLNK(NNAME))
              endif
C              CALL USRMSG('Copying shading file via:',doit,'-')
              CALL RUNIT(doit,'-')
              LSHAD(ICOMP)=NNAME
            endif

C If there is an existing viewfactor file copy it to new name
C so that subsequent changes are applied to the new file.
            if(IVF(icomp).eq.1)then
              EXT='.vwf'
              CALL FNCNGR(LVIEW(ICOMP),APE,EXT,NNAME)
              if(unixok)then
                WRITE(doit,'(4A)')'cp ',
     &            LVIEW(ICOMP)(1:LNBLNK(LVIEW(ICOMP))),' ',
     &            NNAME(1:LNBLNK(NNAME))
              else
                WRITE(doit,'(4A)')'copy /y ',
     &            LVIEW(ICOMP)(1:LNBLNK(LVIEW(ICOMP))),' ',
     &            NNAME(1:LNBLNK(NNAME))
              endif
C              CALL USRMSG('Copying view factor file via:',doit,'-')
              CALL RUNIT(doit,'-')
              LVIEW(ICOMP)=NNAME

C ? after copy it is probably not necessary to re-run the viewfactor analysis.
C              ivalsv(ic)=ivals(IC)
            endif

C If there is a heat transfer regime copy the exiting file
C so that subsequent changes are applied to the new file.
            if(IHC(icomp).eq.1)then
              EXT='.htc'
              CALL FNCNGR(LHCCO(ICOMP),APE,EXT,NNAME)
              if(unixok)then
                WRITE(doit,'(4A)')'cp ',
     &            LHCCO(ICOMP)(1:LNBLNK(LHCCO(ICOMP))),' ',
     &            NNAME(1:LNBLNK(NNAME))
              else
                WRITE(doit,'(4A)')'copy /y ',
     &            LHCCO(ICOMP)(1:LNBLNK(LHCCO(ICOMP))),' ',
     &            NNAME(1:LNBLNK(NNAME))
              endif
C              CALL USRMSG('Copying hc regime file via:',doit,'-')
              CALL RUNIT(doit,'-')
              LHCCO(ICOMP)=NNAME
            endif

C If there is a casual gain control file copy the exiting file
C so that subsequent changes are applied to the new file.
            if(ICGC(icomp).eq.1)then
              EXT='.cgc'
              CALL FNCNGR(LHCCO(ICOMP),APE,EXT,NNAME)
              if(unixok)then
                WRITE(doit,'(4A)')'cp ',
     &            LCGCIN(ICOMP)(1:LNBLNK(LCGCIN(ICOMP))),' ',
     &            NNAME(1:LNBLNK(NNAME))
              else
                WRITE(doit,'(4A)')'copy /y ',
     &            LCGCIN(ICOMP)(1:LNBLNK(LCGCIN(ICOMP))),' ',
     &            NNAME(1:LNBLNK(NNAME))
              endif
C              CALL USRMSG('Copying casual gain ctl file via:',
C     &                    doit,'-')
              CALL RUNIT(doit,'-')
              LCGCIN(ICOMP)=NNAME
            endif

C If there is an obstruction file copy the exiting file
C so that subsequent changes are applied to the new file.
            if(IOBS(icomp).eq.1)then
              EXT='.obs'
              CALL FNCNGR(ZOBS(ICOMP),APE,EXT,NNAME)
              if(unixok)then
                WRITE(doit,'(4A)')'cp ',
     &            ZOBS(ICOMP)(1:LNBLNK(ZOBS(ICOMP))),' ',
     &            NNAME(1:LNBLNK(NNAME))
              else
                WRITE(doit,'(4A)')'copy /y ',
     &            ZOBS(ICOMP)(1:LNBLNK(ZOBS(ICOMP))),' ',
     &            NNAME(1:LNBLNK(NNAME))
              endif
C              CALL USRMSG('Copying obstruction file via:',doit,'-')
              CALL RUNIT(doit,'-')
              ZOBS(ICOMP)=NNAME
            elseif(IOBS(icomp).eq.2)then
              continue
            endif
 17       CONTINUE
          docnn=.true.
          msg1='updating model for geometry changes...'
          call edisp(iuout,
     &      '* New geometry files available for subsequent changes.')
        endif
        ishdq=-1
        ivfwq=-1
        do 43 iz=1,ncomp
          if(ISI(iz).eq.1)ishdq=1
          if(IVF(iz).eq.1)ivfwq=1
  43    continue
        if(ishdq.eq.1)then
          call usrmsg(
     &      'Shading files found that will be dependant on future',
     &      'geometry changes: files copied.','W')
          ishd=1
        endif
        if(ivfwq.eq.1)then
          call usrmsg(
     &      'View factor files found that will be dependant on',
     &      'future geometry changes: files copied.','W')
        endif
      ELSEIF(M.EQ.3)THEN

C site exposure and ground reflectance
C Selection strings for site exposure.
        SALT(1)='typical city centre            '
        SALT(2)='typical urban site             '
        SALT(3)='typical rural site             '
        SALT(4)='city: = sky, grnd, bldgs       '
        SALT(5)='city: below surroundings       '
        SALT(6)='isolated rural site            '
        SALT(7)='totally enclosed (no sky)      '
        WRITE(SALT(8),53)skyview,groundview,buildingview
  53    FORMAT('sky=',F4.2,' grnd=',F4.2,' bld=',F4.2)
        H(1)='The site exposure defines the relative view factors'
        H(2)='between a vertical surface and the sky, ground and'
        H(3)='surrounding buildings.  These data are used in the'
        H(4)='calculation of external longwave radiation exchanges.'
 563    IX=1
        CALL EPICKS(IX,IVALSS,' ',' Site exposure:',
     &         31,8,SALT,'site exposure',IER,4)
        IF(IX.EQ.0)GOTO 563
        siteexposureindex=IVALSS(1)
        IF(siteexposureindex.EQ.8)THEN
          H(1)='0 <= Sky view factor <= .99'
  290     CALL EASKR(skyview,' ',' Sky view factor?',
     &         0.0,'W',0.99,'W',0.33,'sky view factor',IER,1)
          H(1)='0 <= Ground view factor <= .99'
          CALL EASKR(groundview,' ',' Ground view factor?',
     &         0.0,'W',0.99,'W',0.33,'ground view factor',IER,1)
          H(1)='0 <= Surroundings view factor <= .99'
          CALL EASKR(buildingview,' ',' Surroundings view factor?',
     &         0.0,'W',0.99,'W',0.33,'surroundings view factor',IER,1)
          IF(ABS(skyview+groundview+buildingview-1.).GT..001)THEN
            CALL USRMSG(' Error: view factor summation > 1.0',
     &                  ' ','W')
            GOTO 290
          ENDIF
        ENDIF
        H(1)='0 <= ground reflectance <= .99'
        CALL EASKR(groundrefl,' ',' Ground reflectance?',
     &       0.0,'W',0.99,'W',0.2,'ground reflectance',IER,1)
        MODSIT=.true.
        msg1='updating model for site exposure changes...'
C        call edisp(iuout,'* New cfg file will hold new site data.')
      ELSEIF(M.EQ.4)THEN

C Shading and insolation choices. First find out if any of the
C zones curently include shading or obstruction files. If none do then
C aloow the user to select one or more zones.

C << it would be convenient if the number of obstructions was in
C << an array so files did not need to be scanned.

        ishd=-1
        do 142 iz=1,ncomp
          if(ISI(iz).eq.1)ishd=1
          if(IOBS(iz).eq.1)ishd=1
          if(IOBS(iz).eq.2)ishd=1
 142    continue
        if(ishd.eq.-1)then
          h(1)='There are no current instructions to evaluate detailed'
          h(2)='shading and/or insolation patterns. '
          h(3)=' '
          h(4)='You can nominate zones which you want to include an'
          h(5)='insolation analysis (i.e. track direct solar radiation'
          h(6)='within zones). '
          h(7)=' '
          h(8)='You can nominate zones which you want to create solar'
          h(9)='obstructions in so that shading and insolation can'
          h(10)='be assessed.'
          h(11)=' '
          h(12)='Selecting continue at this point you will be asked'
          h(13)='if you want to make any other changes to the model'
          h(14)='before the information is written out. '
          CALL EASKABC(
     &    'The model does not include detailed shading/insolation.',
     &    ' ','select zones for insolation',
     &    'select zones for shading/insolation','continue',IW,14)
          if(iw.eq.1)then
            inpic=0
            call askmultizone(inpic,ivals,
     &        'Select zones for insolation?',
     &        'Zone insolation variant','s',ier)
          elseif(iw.eq.2)then
            inpic=0
            call askmultizone(inpic,ivals,
     &        'Select zones for shading/insolation?',
     &        'Zone shading/insolation variant','s',ier)
          elseif(iw.eq.3)then
            inpic=0
            continue
          endif
          if(iw.eq.1.or.iw.eq.2)then
            if(inpic.gt.0)then

C << need to deal with file separator more generically >>
              DO 151 IC=1,inpic
                ICOMP=ivals(IC)
                ISI(ICOMP)=1
                if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                  WRITE(LSHAD(icomp),'(A,A4)')
     &              zname(icomp)(1:lnzname(icomp)),'.shd'
                else
                  WRITE(LSHAD(icomp),'(3A,A4)') 
     &              zonepth(1:lnblnk(zonepth)),'/',
     &              zname(icomp)(1:lnzname(icomp)),'.shd'
                endif
                EXT='.shd'
                CALL FNCNGR(LSHAD(ICOMP),APE,EXT,NNAME)
                LSHAD(ICOMP)=NNAME

C Create a small place-holder file that subsequent calculation
C procedure can overwrite. Mark for performing shading calculatons.
                IUF=IFIL+2
                CALL EFOPSEQ(IUF,LSHAD(ICOMP),3,IER)
                WRITE(IUF,'(2a)') 'Placeholder shading file for ',
     &            zname(icomp)(1:lnzname(icomp))
                CALL ERPFREE(IUF,ios)
                ishad=1
                ishd=1
 151          continue
            endif
          endif
          if(iw.eq.2)then

C Create an obstructions file for each of the selected zones if
C it is an older format geometry file.
C First create initial name, then mangle it and write block.
            IUF=12
            if(inpic.gt.0)then
              DO 152 IC=1,inpic
                ICOMP=ivals(IC)
                call eclose(gversion(icomp),1.1,0.01,newgeo)
                if(newgeo)then

C If v1.1 geometry file, set iobs=2 and add an initial block via ecrobs.
                  IOBS(icomp)=2
                  silent=.true.
                  CALL ECROBS(ITRC,ITRU,IUF,ICOMP,silent,IER)
                  call geowrite(IUF,LGEOM(ICOMP),IComp,IUOUT,3,IER)
                else

C Older format geometry file so use separate zone obstructions file.
                  if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                    WRITE(ZOBS(icomp),'(A,A4)')
     &                zname(icomp)(1:lnzname(icomp)),'.obs'
                  else
                    WRITE(ZOBS(icomp),'(3A,A4)') 
     &                zonepth(1:lnblnk(zonepth)),'/',
     &                zname(icomp)(1:lnzname(icomp)),'.obs'
                  endif
                  EXT='.obs'
                  CALL FNCNGR(ZOBS(ICOMP),APE,EXT,NNAME)
                  ZOBS(ICOMP)=NNAME
                  silent=.true.
                  CALL ECROBS(ITRC,ITRU,IUF,ICOMP,silent,IER)
                  IF(IOBS(ICOMP).eq.0)then
                    ZOBS(ICOMP)=' '
                  endif
                endif
 152          continue
            endif
          endif
          if(iw.ne.3)then
            call edisp(iuout,
     &        '* Shading files being copied. If site details')
            call edisp(iuout,
     &        'change shading/insolation will be recalculated.')
          endif
        elseif(ishd.eq.1)then

C There is shading and/or insulation check if user wants to dereference.

C << the logic below does not check to see if model uses newer file
C << formats e.g. to remove obstructions from within geometry file.

          h(1)='The model includes detailed evaluations of'
          h(2)='shading and/or insolation patterns. '
          h(3)=' '
          h(4)='You can nominate zones which you want to dereference'
          h(5)='the insolation analysis (select from provided list)'
          h(6)=' '
          h(7)='You can nominate zones which you want to dereference'
          h(8)='obstruction(s). Note: does not remove the file only'
          h(9)='the reference to the obstruction file. '
          h(10)=' '
          h(11)='You can nominate zones which you want to dereference'
          h(12)='both obstructions and shading files. Note: does not'
          h(13)='remove the files only the reference.'
          h(14)=' '
          h(15)='You can either modify existing obstruction files '
          h(16)='or create obstructions for zones which do not'
          h(17)='have any. In both cases place-holder shading'
          h(18)='files will also be created.'
          h(19)=' '
          h(20)='Note: modifications to obstructions files does not'
          h(21)='have an impact on zone geometry files. '
          h(22)=' '
          h(23)='Selecting continue at this point you will be asked'
          h(24)='if you want to make any other changes to the model'
          h(25)='before the information is written out. '
          IW=1
          CALL EASKATOG(
     &      'The model includes detailed shading/insolation.',' ',
     &      'dereference shading','dereference obstructions',
     &      'dereference obstr & shading','modify obstructions',
     &      'continue',' ',' ',IW,25)
          if(iw.eq.1)then
            inpic=0
            call askmultizone(inpic,ivals,
     &        'Select zones do dereference insolation?',
     &        'Zone insolation dereference','s',ier)
            if(inpic.gt.0)then
              DO 153 IC=1,inpic
                ICOMP=ivals(IC)
                ISI(ICOMP)=0
 153          continue
              msg1='updating model for shading changes...'
            endif
          elseif(iw.eq.2)then

C Note: only dereference zones which have separate zone obstruction
C files (iobs=1). Ignore zones with obstructions within geometry file.
            inpic=0
            call askmultizone(inpic,ivals,
     &        'Select zones do dereference obstructions?',
     &        'Zone obstructions dereference','b',ier)
            if(inpic.gt.0)then
              DO 154 IC=1,inpic
                ICOMP=ivals(IC)
                if(iobs(icomp).eq.1) IOBS(ICOMP)=0
 154          continue
              msg1='updating model for shading changes...'
            endif
          elseif(iw.eq.3)then
            inpic=0
            call askmultizone(inpic,ivals,
     &        'Select obs & shading/insolation dereference variant',
     &        'obs & shading/insolation variant','s',ier)
            if(inpic.gt.0)then
              DO 155 IC=1,inpic
                ICOMP=ivals(IC)
                IOBS(ICOMP)=0
                ISI(ICOMP)=0
 155          continue
            endif
            msg1='updating model for shading changes...'
          elseif(iw.eq.4)then

C Create an obstructions file for each of the selected zones
C which does not already have one. If there is an obstructions
C file (iobs(icomp) is non-zero) then read current obstructions
C update the file name and write back out to NNAME file.

C << if newer format geometry then alternative logic needed >>

            IUF=12
            inpic=0
            call askmultizone(inpic,ivals,
     &        'Select obstruction files to modify/add for this variant',
     &        'List of zones/obstructions','b',ier)
            if(inpic.gt.0)then
              DO 156 IC=1,inpic
                ICOMP=ivals(IC)
                if(IOBS(ICOMP).eq.1)then
                  silent=.true.
                  EXT='.obs'
                  CALL EGOMST(IUF,icomp,ZOBS(ICOMP),0,ITRC,ITRU,IER)
                  CALL FNCNGR(ZOBS(ICOMP),APE,EXT,NNAME)
                  ZOBS(ICOMP)=NNAME
                  CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)
                elseif(IOBS(ICOMP).eq.2)then
                  continue
                elseif(IOBS(ICOMP).eq.0)then

C Create an obstruction file name, mangle the name and write
C out default block.

C << and check if newer geometry file and see about alternative
C << logic that does not bother with obs file copying.

                  IOBS(ICOMP)=1
                  if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                    WRITE(ZOBS(icomp),'(A,A4)')
     &                zname(icomp)(1:lnzname(icomp)),'.obs'
                  else
                    WRITE(ZOBS(icomp),'(3A,A4)') 
     &                zonepth(1:lnblnk(zonepth)),'/',
     &                zname(icomp)(1:lnzname(icomp)),'.obs'
                  endif
                  EXT='.obs'
                  CALL FNCNGR(ZOBS(ICOMP),APE,EXT,NNAME)
                  ZOBS(ICOMP)=NNAME
                  silent=.true.
                  CALL ECROBS(ITRC,ITRU,IUF,ICOMP,silent,IER)
                endif

C There will need to be a shading file for this modified obstruction
C fileis so create a base name for shading file and then tweak the name via fncngr.
                ISI(ICOMP)=1
                if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                  WRITE(LSHAD(icomp),'(A,A4)')
     &              zname(icomp)(1:lnzname(icomp)),'.shd'
                else
                  WRITE(LSHAD(icomp),'(3A,A4)') 
     &              zonepth(1:lnblnk(zonepth)),'/',
     &              zname(icomp)(1:lnzname(icomp)),'.shd'
                endif
                EXT='.shd'
                CALL FNCNGR(LSHAD(ICOMP),APE,EXT,NNAME)
                LSHAD(ICOMP)=NNAME

C Create a small place-holder file that subsequent calculation
C procedure can overwrite. Mark to update shading files.
                IUF=IFIL+2
                CALL EFOPSEQ(IUF,LSHAD(ICOMP),3,IER)
                WRITE(IUF,'(2a)') 'Placeholder shading file for ',
     &            zname(icomp)(1:lnzname(icomp))
                CALL ERPFREE(IUF,ios)
                ishad=1
                ishd=1

C << what the logic does not yet do is loot to see if there is an ascii
C << version of the zone shading file and copy it to the new name.

 156          continue
              msg1='updating model for obstruction changes...'
            endif
          elseif(iw.eq.5)then
            msg1='shading changes not selected'
          endif
        endif
      ELSEIF(M.EQ.5)THEN

C View factors and radiant sensors
        inpic=0
        call askmultizone(inpic,ivals,
     &    ' Select zones for viewfactor variant?',
     &    'Zone viewfactor variant','v',ier)
        if(inpic.gt.0)then
          nbvalsv=0
          h(1)='Area weighted is a simplified techniques that is'
          h(2)='appropriate where geometry has been approximated.'
          h(3)='Otherwise choose ray traced, which is accurate'
          h(4)='(but time consuming).'
          CALL EASKABC('View factor options:',' ',
     &      'area weighted','ray traced','cancel',IW,4)

C Updating viewfactors can require configuration file to be updated
C before calling mrt.
          if(iw.eq.1.or.iw.eq.2)then
            DO 127 IC=1,inpic
              IUF=IFIL+2
              ITRC=1
              ITRU=6
              ICOMP=ivals(IC)
              if(IVF(icomp).eq.1)then
                EXT='.vwf'
                CALL ERMRT(ITRC,ITRU,IUF,LVIEW(ICOMP),ICOMP,IER)
                CALL FNCNGR(LVIEW(ICOMP),APE,EXT,NNAME)
                LVIEW(ICOMP)=NNAME
                CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),
     &            NZSUR(ICOMP),IUF,ICOMP,'v',IER)
              elseif(IVF(icomp).eq.0)then
                if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                  WRITE(LVIEW(icomp),'(A,A4)')
     &              zname(icomp)(1:lnzname(icomp)),'.vwf'
                else
                  WRITE(LVIEW(icomp),'(3A,A4)') 
     &              zonepth(1:lnblnk(zonepth)),'/',
     &              zname(icomp)(1:lnzname(icomp)),'.vwf'
                endif
                EXT='.vwf'
                CALL FNCNGR(LVIEW(ICOMP),APE,EXT,NNAME)
                LVIEW(ICOMP)=NNAME

C In both cases create an initial viewfactor file. If ray tracing
C required, save the list of zones to be done after the
C configuration file has been updated.
                silent=.true.
                call EDMRT(ITRC,ITRU,IUF,ICOMP,silent,'a',IER)
                if(iw.eq.2)then
                  ivalsv(ic)=ivals(IC)
                endif
              endif
 127        CONTINUE
            if(iw.eq.2)then
              nbvalsv=inpic
            endif
            msg1='updating model for viewfactor changes...'
            call edisp(iuout,
     &          '* View factor files being copied to allow updates')
            call edisp(iuout,'should geometry subsequent change.')
          endif
        endif
      ELSEIF(M.EQ.6)THEN

C Climate, latitiude and longitude. First check if the model
C includes shading files.
        ishd=-1
        do 42 iz=1,ncomp
          if(ISI(iz).eq.1)ishd=1
  42    continue

        CALL EDDBCLM(moddb,ape,ishad)

        IF(NGRDP.NE.0)THEN
          call edisp(IUOUT,
     &     '* User defined ground temperatures need to be updated.')
          CALL EDDGTP(modsit) 
        ENDIF

C If there is an IPV file also update it.
        if(icfgv.lt.4)then
          call FINDFIL(lipvdatf,XST)
          call isunix(unixok)
          IF(XST)THEN
            call edisp(IUOUT,
     &        '* IPV file has been updated to match climate.')
            call edisp(IUOUT,'Please update the IPV titles.')
	    ipvaction='ipv'
            CALL RIPVDAT(IFIL+1,LIPVDATF,ipvaction,IER)
            EXT='.ipv'
            CALL FNCNGR(LIPVDATF,APE,EXT,NNAME)
            if(unixok)then
              WRITE(doit,'(4A)')'cp ',LIPVDATF(1:LNBLNK(LIPVDATF)),' ',
     &          NNAME(1:LNBLNK(NNAME))
            else
              WRITE(doit,'(4A)')'copy /y ',
     &          LIPVDATF(1:LNBLNK(LIPVDATF)),' ',
     &          NNAME(1:LNBLNK(NNAME))
            endif
C          CALL USRMSG('Copying IPV file via:',doit,'-')
            CALL RUNIT(doit,'-')
            LIPVDATF=NNAME
            updatetitle=.true.
          else
	    updatetitle=.false.   ! nothing to do
	  endif
        else
          if(nipvassmt.eq.0)then
	    updatetitle=.false.   ! nothing to do
          else
            updatetitle=.true.
          endif
	endif

C Update the titles.
        if(updatetitle)then
          CALL EASKS(ipvtitl,'Enter project title for this variant?',
     &    ' ',40,'project','description modifier',IER,0)
          CALL EASKS(ipvvers,'Enter project version for this variant?',
     &    ' ',40,'project','description modifier',IER,0)
          CALL EASKS248(ipvsynop,'Synopsis',' ',72,
     &    'Enter Project sysnopsis for this variant','synopsis',IER,5)
          CALL IPVDAT('-')
        endif
        msg1='updating model for climate changes...'
      ELSEIF(M.EQ.7)THEN

C Pressure distribution
        H(1) = 'Pressure coefficients are used to calculate pressure'
        H(2) = 'distribution. These are defined at two levels.'
        H(3) = 'Building parameters include overall dimensions, roof'
        H(4) = 'detail, plan area etc. Invoking this facility will'
        H(5) = 'allow these values to be changed.'
        H(6) = 'The database option defines pressure coefficients that'
        H(7) = 'are used where an air flow network exists.'
        msg1='updating model for pressure db changes...'
        CALL EASKABC('Change to be made to:',
     &  ' ','building parameters','database','cancel',IW,7)
        IF(IW.EQ.3)THEN
          RETURN
        ELSEIF(IW.EQ.2)THEN
          EXT='.db1'
          CALL FNCNGR(LAPRES,APE,EXT,NNAME)
          CALL fdroot(NNAME,msg1,MSG2)
          WRITE(LAPRES,'(3a)')DBSPTH(1:LNBLNK(DBSPTH)),'/',MSG2
          DOK=.TRUE.
          CALL ASKOK('  ','Edit database now?',OK,DOK,5)
          IF(OK)THEN
            CALL EDPCDB(IER)          
          ELSE
            CALL EMKAPCDB(LAPRES,IER)
          ENDIF
        ELSEIF(IW.EQ.1)THEN
          CALL CPCDAT
        ENDIF
        msg1='updating model for pressure db changes...'
      ELSEIF(M.EQ.8)THEN

C Form and fabric information
C glazed area
        h(1)='glazing area currently not supported' 
        CALL PHELPD('glazing area not supported',1,'-',0,0,IER)
        msg1='updating model for glazing area changes...'
      ELSEIF(M.EQ.9)THEN

C Variant zone constructions (this includes geometry, construction
C and tmc file variants for each zone selected.
        inpic=0
        call askmultizone(inpic,ivals,
     &    ' Select zones for construction variant?',
     &    'Zone construction variant','c',ier)
        if(inpic.gt.0)then
          DO 147 IC=1,inpic
            IUNIT=12
            QUIET=.FALSE.
            ICOMP=ivals(IC)
            IF(ICOMP.EQ.0)GOTO 147
            call FINDFIL(LTHRM(ICOMP),XST)
            IF(.NOT.XST)THEN
              CALL EDISP(IUOUT,
     &          '* Construction file does not exist...skipping.')
              goto 147
            ENDIF

C Check if geometry file has already been updated.
            donegeo=.false.
            DO 149 icg=1,inpic
              if(icomp.eq.ivalsg(icg))donegeo=.true.
 149        continue
            if(.NOT.donegeo)then
              EXT='.geo'
              call eclose(gversion(icomp),1.1,0.01,newgeo)
              call georead(IUNIT,LGEOM(ICOMP),ICOMP,IR,ITRU,IER)
              CALL FNCNGR(LGEOM(ICOMP),APE,EXT,NNAME)
              LGEOM(ICOMP)=NNAME
              if(igupgrade.eq.2.and.(.NOT.newgeo))then
                gversion(icomp) =1.1
                newgeo = .true.
              endif
              if(newgeo)then
                call geowrite(IUNIT,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
              else
                call emkgeo(IUNIT,LGEOM(ICOMP),ICOMP,3,IER)
              endif
              WRITE(MSG1,'(2A)')
     &        '* New geometry file written out for zone ',ZNAME(ICOMP)
              CALL EDISP(IUOUT,MSG1)
            endif
            ITRU=6
            CALL ECONST(LTHRM(ICOMP),IUNIT,ICOMP,ITRC,ITRU,IER)
            EXT='.con'
            CALL FNCNGR(LTHRM(ICOMP),APE,EXT,NNAME)
            LTHRM(ICOMP)=NNAME
            CALL EMKCON(LTHRM(ICOMP),IUNIT,ICOMP,QUIET,IER)
            WRITE(MSG1,'(2A)')
     &              '* New constructions file written out for zone ',
     &              ZNAME(ICOMP)
            CALL EDISP(IUOUT,MSG1)
            call FINDFIL(LTWIN(ICOMP),XST)
            IF(.NOT.XST)GOTO 147
            IFU=13
            CALL ERTWIN(ITRC,ITRU,IFU,LTWIN(ICOMP),ICOMP,IER)
            EXT='.tmc'
            CALL FNCNGR(LTWIN(ICOMP),APE,EXT,NNAME)
            LTWIN(ICOMP)=NNAME
            CALL MKTWIN(IFU,ICOMP,QUIET,IER)
            WRITE(MSG1,'(2A)')
     &      'New  transparent constructions file written out for zone '
     &      ,ZNAME(ICOMP)
            CALL EDISP(IUOUT,MSG1)
 147      CONTINUE
          docnn=.true.
          msg1='updating model for construction changes...'       
          call edisp(iuout,
     &      '* Subsequent changes will be written to the new files.')
        endif
      ELSEIF(M.EQ.10)THEN

C Construction database variant. Depending on location expand path to file.
C << probably should only allow a variant if a local database >>
        if(ipathmul.eq.0.or.ipathmul.eq.1)then
          lworking=LFMUL
        elseif(ipathmul.eq.2)then
          lndbp=lnblnk(standarddbpath)
          write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &      LFMUL(1:lnblnk(LFMUL))
        endif
        LFMULEN=LNBLNK(lworking)
        IF(LFMUL(LFMULEN:LFMULEN).EQ.'1')THEN
          EXT='.db1'
        ELSEIF(LFMUL(LFMULEN:LFMULEN).EQ.'2')THEN
          EXT='.db2'
        ELSEIF(LFMUL(LFMULEN:LFMULEN).EQ.'3')THEN
          EXT='.db3'
        ELSEIF(LFMUL(LFMULEN:LFMULEN).EQ.'a')THEN
          EXT='.asc'
        else
          EXT='.dbs'
        endif

C Find last token in lworking and return in msg2.
        call isunix(unixok)
        call fdroot(lworking,msg1,MSG2)
        WRITE(NNAME,'(2a)')DBSPTH(1:LNBLNK(DBSPTH)),MSG2
        CALL FNCNGR(NNAME,APE,EXT,NNAME)
        if(unixok)then
          WRITE(DOIT,'(4A)')'cp ',lworking(1:LNBLNK(lworking)),' ',
     &       NNAME(1:LNBLNK(NNAME))
        else
          WRITE(DOIT,'(4A)')'copy /y ',lworking(1:LNBLNK(lworking)),' ',
     &       NNAME(1:LNBLNK(NNAME))
        endif
        write(LFMUL,'(a)') NNAME(1:lnblnk(NNAME))  ! truncation risk
        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,'Copying database via ...')
        CALL EDISP(IUOUT,DOIT)
        CALL RUNIT(DOIT,'-')
        call edisp(iuout,' Updating model for construction changes...')
        CALL EDISP(IUOUT,' ')

C Also make automatically materials database variant
C << probably should only allow a variant if a local database >>
        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          lworking=LFMAT
        elseif(ipathmat.eq.2)then
          lndbp=lnblnk(standarddbpath)
          write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &      LFMAT(1:lnblnk(LFMAT))
        endif
        LFMATEN=LNBLNK(lworking)
        IF(LFMAT(LFMATEN:LFMATEN).EQ.'1')THEN
          EXT='.db1'
        ELSEIF(LFMAT(LFMATEN:LFMATEN).EQ.'2')THEN
          EXT='.db2'
        ELSEIF(LFMAT(LFMATEN:LFMATEN).EQ.'3')THEN
          EXT='.db3'
        ELSEIF(LFMAT(LFMATEN:LFMATEN).EQ.'a')THEN
          EXT='.asc'
        else
          EXT='.dbs'
        endif

C Find last token in lworking and return in msg2.
        call isunix(unixok)
        call fdroot(lworking,msg1,MSG2)
        WRITE(NNAME,'(2a)')DBSPTH(1:LNBLNK(DBSPTH)),MSG2
        CALL FNCNGR(NNAME,APE,EXT,NNAME)  ! ?? why call this
        if(unixok)then
          WRITE(DOIT,'(4A)')'cp ',LFMAT(1:LNBLNK(LFMAT)),' ',
     &       NNAME(1:LNBLNK(NNAME))
        else
          WRITE(DOIT,'(4A)')'copy /y ',LFMAT(1:LNBLNK(LFMAT)),' ',
     &       NNAME(1:LNBLNK(NNAME))
        endif
        write(LFMAT,'(a)') NNAME(1:lnblnk(NNAME))
        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,'Copying database via ...')
        CALL EDISP(IUOUT,DOIT)
        CALL RUNIT(DOIT,'-')
        call edisp(iuout,' Updating model for constructions changes...')
        CALL EDISP(IUOUT,' ')
      ELSEIF(M.EQ.11)THEN

C CFD domain
        inpic=0
        call askmultizone(inpic,ivals,
     &    ' Select zones for CFD variant?',
     &    'Zone CFD variant','d',ier)
        ICFD=1
        if(inpic.gt.0)then
          EXT='.dfd'
          DO 178 IC=1,inpic
            ICOMP=ivals(IC)
            ITRC=2
            ITU=6
            CALL DFDREAD(ICOMP,itrc,itu,IER)
            CALL FNCNGR(LCFD(ICOMP),APE,EXT,NNAME)
            LCFD(ICOMP)=NNAME
            IUF=12
            CALL DFDSV(IUF,ICOMP,IER)
 178      CONTINUE
          DOK=.FALSE.
          CALL ASKOK('  ','Edit CFD data now?',OK,DOK,5)
          IF(OK)CALL CFDCOMP(ICOMP,iuf,IER)
          msg1='updating model for CFD domain changes...'
          call edisp(iuout,' ')
        endif
      ELSEIF(M.EQ.12)THEN

C ground temperature profiles
        CALL EDDGTP(modsit) 
        msg1='updating model for ground temperature changes...'
        call edisp(iuout,
     &    '* Configuration file holds revised greound temperatures.')
      ELSEIF(M.EQ.13)THEN

C variant convection calculations
C Get list of zones
        inpic=0
        call askmultizone(inpic,ivals,
     &    ' Select zones for hc regime variant?',
     &    'Zone hc regime variant','h',ier)
        if(inpic.gt.0)then
          EXT='.htc'
          DO 188 IC=1,inpic
            ICOMP=ivals(IC)
            ITRU=6
            IUF=12
            CALL ehtcff(LHCCO(ICOMP),IUF,ITRU,IER)
            CALL FNCNGR(LHCCO(ICOMP),APE,EXT,NNAME)
            LHCCO(ICOMP)=NNAME
            CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,ITRU,IER)
 188      CONTINUE
          msg1='updating model for convective regime changes...'
          call edisp(iuout,
     &      '* Subsequent changes will be applied to the new files.')
        endif
      ELSEIF(M.EQ.14)THEN

C active materials and advanced optics
        h(1)='active materials currently not supported' 
        CALL PHELPD('active mat not supported',1,'-',0,0,IER)
        msg1='updating model for active materials changes...'
      ELSEIF(M.EQ.15)THEN

C adaptive gridding and moisture
        h(1)='adaptive gridding currently not supported' 
        CALL PHELPD('adaptive gridding not supported',1,'-',0,0,IER)
        msg1='updating model for gridding changes...'
      ELSEIF(M.EQ.16)THEN

C building integrated renewables
        h(1)='renewables currently not supported' 
        CALL PHELPD('renewables not supported',1,'-',0,0,IER)
        msg1='updating model for integrated renewables changes...'
      ELSEIF(M.EQ.17)THEN

C variant integrated performance view
        if(icfgv.lt.4)then
          EXT='.ipv'
          call FINDFIL(lipvdatf,XST)
          call isunix(unixok)
          IF(XST)THEN
	    ipvaction='ipv'
            CALL RIPVDAT(IFIL+1,LIPVDATF,ipvaction,IER)
            CALL FNCNGR(LIPVDATF,APE,EXT,NNAME)
            if(unixok)then
              WRITE(doit,'(4A)')'cp ',LIPVDATF(1:LNBLNK(LIPVDATF)),' ',
     &          NNAME(1:LNBLNK(NNAME))
            else
              WRITE(doit,'(4A)')'copy /y ',
     &          LIPVDATF(1:LNBLNK(LIPVDATF)),' ',
     &          NNAME(1:LNBLNK(NNAME))
            endif
C          CALL USRMSG('Copying file via:',doit,'-')
            CALL RUNIT(doit,'-')
            LIPVDATF=NNAME
            updatetitle=.true.
          else
	    updatetitle=.false.   ! nothing to do
	  endif
        else
          if(nipvassmt.eq.0)then
	    updatetitle=.false.   ! nothing to do
          else
            updatetitle=.true.
          endif
	endif

C Update the titles.
        if(updatetitle)then
          CALL EASKS(ipvtitl,' Enter project title for this variant?',
     &     ' ',40,'project','description modifier',IER,0)
          CALL EASKS(ipvvers,' Enter project version for this variant?'
     &     ,' ',40,'project','description modifier',IER,0)
          CALL EASKS248(ipvsynop,'Synopsis',' ',72,
     &    'Enter Project sysnopsis for this variant','synopsis',IER,5)
          CALL IPVDAT('-')
        ELSE
          if(icfgv.lt.4)then
            CALL EDISP(IUOUT,
     &        '* IPV file does not exist, creating one!')        
            WRITE(LIPVDATF,'(2A)')cfgroot(1:lnblnk(cfgroot)),'.ipv'
            CALL FNCNGR(LIPVDATF,APE,EXT,NNAME)
            LIPVDATF=NNAME
          endif
          CALL IPVDAT('I')
          CALL EASKS(ipvtitl,' Enter project title for this variant?',
     &     ' ',40,'project','description modifier',IER,0)
          CALL EASKS(ipvvers,' Enter project version for this variant?'
     &     ,' ',40,'project','description modifier',IER,0)
          CALL EASKS248(ipvsynop,'Synopsis',' ',72,
     &    'Enter Project sysnopsis for this variant','synopsis',IER,5)
          CALL IPVDAT('-') 
        ENDIF
        if(icfgv.lt.4)then
          ipvaction='ipv'
          CALL MKIPVDAT(IFIL+1,LIPVDATF,ipvaction)
          msg1='updating model for IPV changes...'
          call edisp(iuout,
     &      '* IPV file being copied and edited text included.')
        endif
      ELSEIF(M.EQ.18)THEN
         
C variant fluid flow network
        IF(IAIRN.EQ.1)THEN
          EXT='.afn'
          CALL FNCNGR(LAPROB,APE,EXT,NNAME)
          call isunix(unixok)

C Because subroutine to write the file is called with file number and
C not by name invoke system calls to make a copy but this file is read
C into commons and then written out again
          if(unixok)then
            write(doit,'(4A)')'cp -f ',LAPROB(1:lnblnk(LAPROB)),
     &        ' ',NNAME
          else
            write(doit,'(4A)')'copy /y ',LAPROB(1:lnblnk(LAPROB)),
     &        ' ',NNAME
          endif
          LAPROB=NNAME
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
C          CALL USRMSG('Copying flow network via:',doit,'-')
          call runit(doit,tmode)
          CALL EFOPSEQ(IUM,LAPROB,1,IER)

C Read the file header and check for first-line tag. If 4 items
C then an older file so rewind the file and call emfread.
          CALL STRIPC(IUM,OUTSTR,99,ND,0,'1st line of file',IER)
          IF(ND.GE.4)THEN
            REWIND(IUM,ERR=999)
            CALL EMFREAD(IUM,IIER)
          elseif((ND.eq.1.or.ND.eq.2).and.
     &          OUTSTR(1:18).EQ.'*Graphical_network')then

C Found a graphic network file, advise user and exit.
            call usrmsg(
     &      'This file is a graphic network file rather than a',
     &      'flow network file!','W')
          endif
          CALL MFWRIT(IUM)
        ENDIF
        msg1='updating model for air flow network changes...'
        call edisp(iuout,'* Mass flow file being copied.')
      ELSEIF(M.EQ.19)THEN

C electrical network
        h(1)='electrical network currently not supported' 
        CALL PHELPD('electrical net not supported',1,'-',0,0,IER)
        msg1='updating model for electrical network changes...'
      ELSEIF(M.EQ.20)THEN

C plant components / plant network
        h(1)='plants currently not supported' 
        CALL PHELPD('plant not supported',1,'-',0,0,IER)
        msg1='updating model for plant network changes...'
      ELSEIF(M.EQ.21)THEN

C contaminants network
C << There is some problem with this section because the ctm file is
C << written out as fort.83 in ../cfg and not as ../nets/.ctm
        EXT='.ctm'
        CALL CTREAD(iier)
        if(iier.ne.0)then
          outs='* Problem scanning contaminants file.'
          call edisp(iuout,outs)
        endif
        CALL FNCNGR(CNTMFIL,APE,EXT,NNAME)
        CNTMFIL=NNAME
        IUNIT=72
        CALL CTWRIT(iunit)
        msg1='updating model for contaminants network changes...'
        call edisp(iuout,'* Contaminants network being copied.')
      ELSEIF(M.EQ.22)THEN

C Versioning of zone operations. First ask which zones that
C will have different zone operations. Loop through each of
C these, read the current operations file, derive a new file
C name from fncngr call and then call prjfmk with the index
C of the zone (to write out the common block data to the
C alternative file name.
        inpic=0
        call askmultizone(inpic,ivals,
     &    ' Select zones for schedules variant?',
     &    'Zone schedules variant','o',ier)
        if(inpic.gt.0)then
          EXT='.opr'
          IUO=12
          DO 148 IC=1,inpic
            ICOMP=ivals(IC)
            IF(ICOMP.EQ.0)GOTO 148
            call FINDFIL(LPROJ(ICOMP),XST)
            IF(.NOT.XST)THEN
              CALL EDISP(IUOUT,'* Operations file does not exist!')
              RETURN
            ENDIF
            CALL EROPER(ITRC,ITRU,IUO,ICOMP,IER)
            CALL FNCNGR(LPROJ(ICOMP),APE,EXT,NNAME)
            LPROJ(ICOMP)=NNAME
            CALL PRJFMK(ITRC,ITRU,IUO,ICOMP,IER,ICOMP)
            WRITE(MSG1,'(2A)')'* New operations file written out for ',
     &      ZNAME(ICOMP)
            CALL EDISP(IUOUT,MSG1)
 148      CONTINUE
          msg1='updating model for zone operation changes...'
          call edisp(iuout,
     &      '* Subsequent changes will be applied to the new files.')
        endif
      ELSEIF(M.EQ.23)THEN

C A version with different control file. If there is a current
C control file << assume that common block data is current >>
C derive an alternative file name and call ctlwrt to write out the
C common block data.
        EXT='.ctl'
        IUNIT=12
        call FINDFIL(LCTLF,XST)
        IF(.NOT.XST)THEN
          CALL EDISP(IUOUT,'* Control file does not exist!')
          RETURN
        ENDIF
        CALL FNCNGR(LCTLF,APE,EXT,NNAME)
        LCTLF=NNAME
        CALL CTLWRT(IUNIT,IER)
        msg1='Updating model for zone controls changes...'
        call edisp(iuout,'* Control file being copied.')
      ELSEIF(M.EQ.24)THEN

C casual gains control
        h(1)='casual gains currently not supported' 
        CALL PHELPD('Not supported',1,'func not supported',0,0,IER)
        msg1='Updating model for zone casual gain control changes...'
      ELSEIF(M.EQ.25)THEN

C event profiles
        h(1)='event profiles currently not supported' 
        CALL PHELPD('Not supported',1,'func not supported',0,0,IER)
        msg1='Updating model for event profile changes...'
      ELSEIF(M.EQ.26)THEN

C mould and mycotoxins
        h(1)='Moulds and mycotoxins not currently supported.' 
        CALL PHELPD('Not supported',1,'func not supported',0,0,IER)
        msg1='Updating model for mould and mycotoxins changes...'
      ENDIF

C if notional model generation then return now
      IF(LRCR.EQ.3)RETURN

C Reached this point after user has selected a topic whether or
C not any zones were selected.
      h(1)='You may wish to make a number of changes to the'
      h(2)='model variant. If so, select another option.'
      h(3)=' '
      h(4)='Remember all the affected files will have the variant'
      h(5)='extension characters in their file name.'
      dok=.false.
      CALL ASKOK(' ','Make more changes?',OK,DOK,5)
      IF(OK)THEN
        LRCR=1
        RETURN
      ELSE

C Write out configuration file
C But first change names of results libraries zones, flow, plant,
C moisture, electrical and IPV
        DO 123 ISPS=1,NSSET
          EXT='.res'
          CALL FNCNGR(SBLRES(ISPS),APE,EXT,NNAME)
          if(NNAME(1:4).ne.'not_')SBLRES(ISPS)=NNAME
          EXT='.mfr'
          CALL FNCNGR(SFLRES(ISPS),APE,EXT,NNAME)
          if(NNAME(1:4).ne.'not_')SFLRES(ISPS)=NNAME
          EXT='.plr'
          CALL FNCNGR(SPLRES(ISPS),APE,EXT,NNAME)
          if(NNAME(1:4).ne.'not_')SPLRES(ISPS)=NNAME
          EXT='.msr'
          CALL FNCNGR(SMSTRES(ISPS),APE,EXT,NNAME)
          if(NNAME(1:4).ne.'not_')SMSTRES(ISPS)=NNAME
          EXT='.res'
          CALL FNCNGR(SELRES(ISPS),APE,EXT,NNAME)
          if(NNAME(1:4).ne.'not_')SELRES(ISPS)=NNAME
 123    CONTINUE
        EXT='.rep'
        CALL FNCNGR(SIPVRES,APE,EXT,NNAME)
        SIPVRES=NNAME
        LRCR=2

C Check length of cfgroot string. Figure out how to include the
C whole of ape in the root name, even if this requires removing
C some characters from the middle of the string buffer.
        irootlen=LNBLNK(CFGROOT)
	iapelin=lnblnk(ape)
        IF(irootlen.GT.(24-iapelin))THEN
          irootlen=24-iapelin
        ENDIF
        write(cfgroot,'(2a)')cfgroot(1:irootlen),ape(1:iapelin)

C If a variant connections file also required set this up.
        if(docnn)then
          EXT='.cnn'
          CALL FNCNGR(LCNN,APE,EXT,NNAME)
          LCNN=NNAME
        endif
        call usrmsg(msg1,' ','-')
        CALL EMKCFG('-',IER)
        call usrmsg(msg1,'done.','P')
C        WRITE (OUTS,'(2A)')'* Writing new configuration file ',LCFGF
C        CALL EDISP(IUOUT,OUTS)
      ENDIF

C If shading needs updating do this now.
      IF(ISHAD.EQ.1)CALL EDDSHD

C If calculated viewfactors need run do this now.
      if(nbvalsv.gt.0)then
        DO 128 IC=1,nbvalsv
          ICOMP=ivalsv(IC)
          silent=.true.
          call EDMRT(ITRC,ITRU,IUF,ICOMP,silent,'v',IER)
 128   continue
      endif

      RETURN

C File rewind errors.
  999 CALL USRMSG('Error rewinding flow network file:',
     &  LAPROB,'W')
      IER=1
      return

      END

***************** FNCNGR ************************
C FNCNGR changes the name of file ORIGNAM by appending APP
C before an extension EXT, returning NEWNAM as the new file name
      SUBROUTINE FNCNGR(ORIGNAM,APP,EXT,NEWNAM)

      CHARACTER*(*) ORIGNAM,NEWNAM
      CHARACTER APP*6,EXT*4,LEXT*6

      LCFIL=LNBLNK(ORIGNAM)
      LA=LNBLNK(APP)
      LX=LNBLNK(EXT)
      if(lcfil.gt.1)then
        IF(ORIGNAM(LCFIL-3:LCFIL).EQ.EXT(1:4))THEN
          IF(LCFIL.GT.(72-3))LCFIL=69
          WRITE(NEWNAM,'(3A)')ORIGNAM(1:LCFIL-4),APP(1:la),EXT(1:lx)
        ELSE

C For ascii files with extension .asc write name assuming a full
C extension of 6 characters (e.g. materials.db3.a has extension of
C ".db3.a"
          IF(EXT.EQ.'.asc')THEN
            IF(LCFIL.GT.(72-5))LCFIL=65
            IF(LCFIL.GT.5)THEN
              LEXT=ORIGNAM(LCFIL-5:LCFIL)
            ELSE
              LEXT='small'
            endif
            WRITE(NEWNAM,'(3A)')ORIGNAM(1:LCFIL-6),APP(1:LA),LEXT
          ELSE
            IF(LCFIL.GT.(72-3))LCFIL=69
            WRITE(NEWNAM,'(2A)')ORIGNAM(1:LCFIL-4),APP(1:la)
          ENDIF
        ENDIF
      else
        WRITE(NEWNAM,'(3A)') 'not_yet_defined',APP(1:la),EXT(1:lx)
      endif

      RETURN
      END

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
