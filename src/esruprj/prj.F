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
C   MAIN: it does what it says on the box.
C   simula: commissions technical assessments and analysis.
C   checkmodel: check model prior to commissioning a simulation.
C   imgdisp: displays images associated with start-up or at specific points
C   CTLEXP: Control feedback to export wireframe or text feedback area.
C   CFGVER: supports the creation of model variants.
C   VERMAN: copies various esp-r files and names them uniquely
C          in order to build multiple variants of a base case model
C   FNCNGR: changes the name of file ORIGNAM by appending APP

C ********** MAIN
      program prj
#include "building.h"
#include "uncertainty.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IFPRE (for window pressure databases)
C LOPTDB,IOPTDB (for optical database)
C LPCDB/IPCDB (for plant template database)
C LPRFDB,IPRODB (for event profile database)
C LFCON,IFCON,LFMUL,IFMUL (for materials and constructions database)
C ICLIM,LCLIM (for climate file)
C default file names for databases
      PARAMETER (MSTMC=1)

      common/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE
      common/initv/initvt,EYEMI(3),VIEWMI(3),ANGI
      common/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      common/GFONT/IFS,ITFS,IMFS
      common/FILEP/IFIL
      common/appw/iappw,iappx,iappy
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      common/C1/NCOMP,NCON
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/C3F/LCNN
      common/C6/INDCFG
      common/C21/IFCFG,cfgroot,LCFGF
      common/CFGV/icfgv
      common/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
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
      common/uhome/upath
      common/udot/esprc
      common/rcmd/LCMDFL
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

C Indicator of possible focus zone.
      common/rzone/inzone
      common/user/browse

C Redirected text/graphics parameters.
      common/exporttgi/ixopen,ixloc,ixunit

C Where exemplars list and default db list are kept.
      common/exemlst/exemlbl,exemfl
      common/defdb/dfdblbl,defdbfl

C Defaults.
      common/deflt4/dinstpath

C Images.
      common/IMAGF/imgfmt(10),imgfoc(10),limgfil(10),noimg,iton

C Radiance RIF file.
      common/radcfg/LRADCF

C Temporal definitions.
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFT/LTDF,LTDFA

C IPV description.
      common/IPVF/lipvdatf

      common/pophelp/h(60)
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

C Dispersed demands notes and file name.
      COMMON/BL1/dmdsdesc,bdmds
      COMMON/BIDIRFL/bidirfile,bidirname(MSTMC)
      COMMON/Vld20/Vldtng

      logical OK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical MODIFY,MODLEN,MODBND,browse,confirm,XST,ckpath
      logical unixok,isdok,Vldtng,changedit,foundutl
      logical itisanexemplar  ! passed to newprb to ensure browse mode.

      character dstmp*24,uname*24
      character LUALF*72,LCNG*15,LLOC*15
      character*72 LCFGF,H,LPRJLG
      character*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,LCNN,LTMP
      character*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character inf*144,inz*16,inzone*16,LCMDFL*144
      character outs*124,outstr*124,outs248*248
      character path*72,upath*72,esprc*72,cfgroot*24,exemplar*144
      character LAPROB*72
      character ITEMS(27)*33
      character imgfmt*4,imgfoc*4,limgfil*72,ltcmdfl*144
      character exemlbl*20,exemfl*72,dfdblbl*20,defdbfl*72
      character ctldoc*248,LCTLF*72,ETEXT*82,lradcf*72,LTDFA*72
      character journcmd*20,jfile*72,tfile*72,cjfile*72,LTDF*72 
      character GTGEOM*72,lipvdatf*72,longtfile*144
      character*10 wkd1, wkd2
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24,fs*1
      character docpth*24,tmppth*24,dbspth*24
      character CMDCFG*72,CMDCLM*72,CMDCLMN*72,CMDNOTE*64,CMDRES*72
      character ALOC*12,ALOCLBL*12,ALOCTYP*4,CMDLBL*12,CMDLNAM*80
      CHARACTER dmdsdesc*248,bdmds*72,cpth*48,cmdn*48,topt*72
      character t24*24,DFILE*72,DCNN*72,bidirfile*72,bidirname*12
      CHARACTER dinstpath*48,dirpath*48
      character useraction*8  ! returned from exemplar access call.

C Initialize variables and recover command line arguments.
      call ezero
      call curmodule('prj ')
      ITRC=0
      IUOUT=6
      IUIN=5
      LIMTTY=24
      LIMIT =24
      IFIL=11
      iuj=IFIL+10
      call parsfz(MODL,iappw,iappx,iappy,inf,inz)
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
      write(imgpth,'(a1,a1)')'.',fs
      write(radpth,'(a1,a1)')'.',fs
      write(docpth,'(a1,a1)')'.',fs
      write(tmppth,'(a1,a1)')'.',fs
      write(dbspth,'(a1,a1)')'.',fs
      IYEAR=2000
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
      dmdsdesc='no dispersed demands notes (yet)'
      bdmds='UNKNOWN'
      bidirfile='UNKNOWN'

C Assume weekends are Saturday/ Sunday unless re-defined.
      idwe1=6
      idwe2=7
      wkd1='Saturday'
      wkd2='Sunday'

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

C If your compiler does not support floating-point arithmetic
C according to ANSI/IEEE Std 754-1985 comment out the following code.

C Initialize mode, status, and signal handling for IEEE arithmetic.
C See also: f77_ieee_environment(3F) f77_floatingpoint(3F)
C           Note that in case of Sun FORTRAN, IEEEE exceptions arise
C           only if: Sun-3 f77 with -f68881 or -ffpa option
C                or: Sun-4 f77.
C      ieeer=ieee_handler('set','common',SIGFPE_ABORT)
C      ieeer=ieee_handler('set','common',SIGFPE_bps)
C      if(ieeer.ne.0) write(iuout,*) ' IEEE_handler not set !'

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
     &'Project Manager: enquiries to esru@strath.ac.uk')

C Open the text display box equal to LIMTTY if MMOD = 8.  Model
C not yet know so pass 0 to the wireframe control routine (updwire).
C If starting as a reduced percentage of default then widen the
C graphic display to compensate.
      IF(MMOD.EQ.8)THEN
        CALL win3d(menuchw,4,1,1,3,igl,igr,igt,igb,igw,igwh)
#ifdef X11
        call opencpw
        call opentutorial
        call opensetup
        call updwire(0)
        call updcapt(1)
        call updazi(1)
#endif
        call setzscale()
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      ENDIF

      call edisp(IUOUT,' Welcome to the ESP-r System, Version 11.2')
      call edisp(IUOUT,' ')
      call edisp(IUOUT,' (ESP-r Project Manager Version 5.8 of')
      call edisp(IUOUT,' November 2006, Copyright 2001-6 Energy ')
      call edisp(IUOUT,' Systems Research Unit, University of ')
      call edisp(IUOUT,' Strathclyde, Glasgow, Scotland.)')
      call edisp(IUOUT,' ')

C Find the user's home folder then get user's custom settings
C if defined.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IFIL+5,0,IIER)
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
      call tstamp('>','Started project manager')
  902 continue

C Scan the defaults file for default system configuration file.
C Make temporary use of file unit IFIL+5.  Note: escdef must come
C after scan of .esprc file.
      call escdef(dfdblbl,defdbfl,IFIL+5,IER)

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

C Climate db is on channel IFIL (NB when not in use other binary
C databases may make use of this unit temporarily).
      ICLIM=IFIL
      LCLIM=DCLIM

      LAPRES=DAPRES

C Multi-layer constructions db on channel IFIL+3.
      IFMUL=IFIL+3
      LFMUL=DFMUL

C Materials db on channel IFIL+4.
      IFCON=IFIL+4
      LFCON=DFCON

C System configuration file on channel IFIL+5. 
      IFCFG=IFIL+5
      LCFGF='UNKNOWN'
      LSNAM='UNKNOWN'

C Mass flow network, profiles and optical dbs on channel IFIL+6. 
      IOPTDB=IFIL+6
      LOPTDB=DOPTDB

      IPRODB=IFIL+6

C Temporal information on channel IFIL+7.
      IUTDF=IFIL+7
      IUTDFA=IFIL+8
      LTDF='UNKNOWN'
      LTDFA='UNKNOWN'
      ITDFLG=0

      LPRFDB=DPRFDB

      LPCDB=DPCDB

C Text/graphic feedback redirection on channel ifil +9 
      ixunit = ifil + 9

C Project log.
      LPRJLG='job.notes'

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
 22         CALL STRIPC(IFCFG,OUTSTR,0,ND,0,'tags',IIER)
            if(iier.ne.0)then
C Debug.
C              write(6,*)'idst naloc =',nidst,naloc
              goto 23
            endif
            if(OUTSTR(1:9).eq.'*abs_path')then
              k=10
              CALL EGETRM(OUTSTR,K,cpth,'W','cfg path',IER)
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
C << for now assume that the file passed is not an exemplar >>
          itisanexemplar=.false.
          confirm=.false.
          ckpath=.true.
          call NEWPRB(ITRC,confirm,ckpath,itisanexemplar,IER)
          if(ier.eq.-1)goto 3

          call edisp(iuout,' Loading supplied model description.')
          call tstamp('>','PRJ: loading supplied model')
          call tstamp('>',LCFGF)
        else

C The model file might not exist.
C << for now assume that the file passed is not an exemplar >>
          itisanexemplar=.false.
          confirm=.false.
          ckpath=.true.
          call NEWPRB(ITRC,confirm,ckpath,itisanexemplar,IER)
          if(ier.eq.-1)goto 3

          call edisp(iuout,' Loading supplied model description.')
          call tstamp('>','PRJ: loading supplied model')
          call tstamp('>',LCFGF)
        endif
      endif

C Provide site information, update wireframe control with
C current number of zones if a model has been loaded.
      if(LCMDFL(1:2).ne.'  '.and.LCMDFL(1:4).ne.'UNKN')then
        if(ier.eq.0)then
          IF(MMOD.EQ.8)then
            call updwire(ncomp)
            call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
            call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
            WRITE(ETEXT,'(2A)')'Project: ',LSNAM(1:lnblnk(LSNAM))
            CALL viewtext(ETEXT,1,1,1)
          endif
          CFGOK=.TRUE.
          call siteinfo(iuout)
          call edisp(iuout,' ')

C Open core of databases if not already done so.
          if(MLDBOK.and.MATDBOK.and.OPTKOK)then
            continue
          else
            call opendb(ier)
            if(ier.ne.0)then
              call usrmsg('Possible problems with one or more of the',
     &          'construction and optical databases, please check.','W')
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
              CALL ADJVIEW(ITRC,IER)
              if(MMOD.EQ.8)then
                call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
                call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
                WRITE(ETEXT,'(2A)')'Project: ',LSNAM(1:lnblnk(LSNAM))
                CALL viewtext(ETEXT,1,1,1)
              endif
            endif
            if(MMOD.EQ.8)then
              call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
              call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
              WRITE(ETEXT,'(2A)')'Project: ',LSNAM(1:lnblnk(LSNAM))
              CALL viewtext(ETEXT,1,1,1)
            endif
          else
            call edisp(iuout,' ')
            call edisp(iuout,'Changing focus to specified zone.')
            itru=iuout
            CALL EGOMIN(IFIL+1,LGEOM(index),index,1,ITRC,iuout,IER)
            IF(IER.NE.0)goto 3

C If there is an obstructions file read it.
            IUF=IFIL+1
            IF(IOBS(index).EQ.1)CALL EGOMST(IUF,ZOBS(index),IR,ITRC,
     &         ITRU,IER)
            CALL EDZONE(ITRC,index,IER)
            MODIFY=.TRUE.
          endif
        endif
      endif

      ITRC=1
    3 INO=-4
      ITEMS(1) =  'a introduction to ESP-r        '
      ITEMS(2) =  'b database maintenance         '
      ITEMS(3) =  'c validation                   '
      ITEMS(4) =  ' Select_______________________ '
      ITEMS(5) =  'd   open                       '
      ITEMS(6) =  'e   new model                  '
      if(CFGOK)then
        if(browse)then
          ITEMS(7) =  ' Current__(browse only)_______ '
          ITEMS(13)=  'm   browse/simulate            '
        else
          ITEMS(7) =  ' Current______________________ '
          ITEMS(13)=  'm   browse/edit/simulate       '
        endif
        WRITE(ITEMS(8),'(2A)') '    cfg  : ',LCFGF(1:22)
        WRITE(ITEMS(9),'(2A)') '    path : ',path(1:22)
        WRITE(ITEMS(10),'(2A)')'g   root : ',cfgroot(1:22)
        WRITE(ITEMS(11),'(2A)')'h   title: ',LSNAM(1:22)
        WRITE(ITEMS(12),'(A)') 'j   version:       '
        ITEMS(14)=  '                              '
      else
        ITEMS(7) =  ' Current__(no model yet)______'
        WRITE(ITEMS(8),'(A)')  '    cfg  :         '
        WRITE(ITEMS(9),'(A)')  '    path :         '
        WRITE(ITEMS(10),'(A)') '    root :         '
        WRITE(ITEMS(11),'(A)') '    title:         '
        WRITE(ITEMS(12),'(A)') '    version:       '
        ITEMS(13)=  '                              '
        ITEMS(14)=  '                              '
      endif
      ITEMS(15)   = ' Import & export______________'
      ITEMS(16)   = 'n   import from 3rd party     '
      if(CFGOK)then
        ITEMS(17) = 'o   export current model      '
        ITEMS(18) = 'p   archive current model     '
      else
        ITEMS(17) = '    export                    '
        ITEMS(18) = '    archive                   '
      endif
      ITEMS(19)   = ' Folders & files______________'
      if(CFGOK)then
        ITEMS(20) = 't   project folders & files   '
      else
        ITEMS(20) = '    project folders & files   '
      endif
      ITEMS(21)  =  ' _____________________________'
      if(CFGOK)then
        ITEMS(22) = 'r save model                  '
        ITEMS(23) = 's save model as               '
      else
        ITEMS(22) = '  save model                  '
        ITEMS(23) = '  save model as               '
      endif
      IF(ITRC.EQ.0)THEN
        ITEMS(24) = 'v feedback >> silent          '
      ELSEIF(ITRC.EQ.1)THEN
        ITEMS(24) = 'v feedback >> summary         '
      ELSEIF(ITRC.EQ.2)THEN
        ITEMS(24) = 'v feedback >> detailed        '
      ENDIF
      ITEMS(25) =   '* preferences                 '
      ITEMS(26) =   '? help                        '
      ITEMS(27) =   '- exit Project Manager        '
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
            CALL ADJVIEW(ITRC,IER)
          endif

C Re-draw the configuration buttons.
          if(MMOD.EQ.8)then
            call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
            call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
            WRITE(ETEXT,'(2A)')'Project: ',LSNAM(1:lnblnk(LSNAM))
            CALL viewtext(ETEXT,1,1,1)
          endif
        endif
      endif

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('prj ',1)

C Present opening menu.
      CALL EMENU('Project manager',ITEMS,MITEM,INO)
      IF(INO.EQ.MITEM)THEN
        if(changedit)then
          h(1)='If you exit without saving, recent changes could be'
          h(2)='lost. If you select continue then you will be returned'
          h(3)='to the interface.'
          CALL EASKABC('There are unsaved changes in the model.',
     &      'options:','exit w/o saving','save & exit','continue',IW,3)
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
          call usrmsg(' Closing Project Manager...',' ','P')
          ok=.true.
        endif
        IF(.NOT.OK)GOTO 3
        if(journio.eq.1)then
          call tstamp('>','Finish project manager')
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
        CALL EPAGEND
        STOP
      ELSEIF(INO.EQ.1)THEN

C Introduction.
        H(1)='Welcome to the Project Manager, a tool to help you'
        H(2)='specify, appraise and evolve your designs.'
        H(3)=' '
        H(4)='If you are new to the system, you might like to start'
        H(5)='by selecting the tutorial button and reading'
        H(6)='through the WWW tutorials provided.  There is also a '
        H(7)='paper manual.  The World-Wide Web address is'
        H(8)=' '
        H(9)='     http://www.esru.strath.ac.uk'
        H(10)=' '
        H(11)=' '
        H(12)='Please contact ESRU at the email address shown above'
        H(13)='should you require further information or wish to'
        H(14)='suggest tool enhancements.'
        H(15)=' '
        H(16)='To list the invocation options supported by this tool'
        H(17)='enter `esp-r -help` at the command line prompt.'
        CALL PHELPD('Introduction',17,'gen_esp_intr',0,0,IER)
        call tstamp('>','PRJ: intro help')
      ELSEIF(INO.EQ.2)THEN

C Manage databases.
        call tstamp('>','PRJ: db management enter')
        CALL EDDB(ITRC,IUOUT,IER)
        call tstamp('>','PRJ: db management exit')
      ELSEIF(INO.EQ.3)THEN

C Manage validation. Does not need to have an existing model
C loaded << double check that this is true >>
        call MValid
      ELSEIF(INO.EQ.5)THEN

C Open an exemplar or existing model.
        h(1)='A list of example (exemplar) models is available or'
        h(2)='you can specify another existing model.'
        CALL EASKABC(' Model opening options:','  ',
     &    'open an exemplar','open another model','continue',IW,2)
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
              call rexmpl(IUF,exemfl,exemplar,useraction,ier)
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

C Copy the exemplar configuration file (with full path) to LCMDFL.
                write(LCMDFL,'(a)') exemplar(1:lnblnk(exemplar))
                ltcmdfl=exemplar
                call fdroot(ltcmdfl,path,LCFGF)

C Debug...
                write(outs248,'(2a)') 'LCMDFL is ',LCMDFL
                call edisp248(iuout,outs248,110)
                write(outs248,'(2a)') 'model path is ',path
                call edisp248(iuout,outs248,110)
                write(outs248,'(2a)') 'model cfg is ',LCFGF
                call edisp248(iuout,outs248,110)
                call edisp(iuout,' ')

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

C A non match may happen in Native windows so fall back ont useracton
C if there is no match in the paths.
                  if(useraction(1:8).eq.'browse  ')then
                    browse=.true.
                    itisanexemplar=.true.
                  elseif(useraction(1:8).eq.'ownit   ')then
                    browse=.false.
                    itisanexemplar=.false.
                  endif
                endif
                write(outs,*) ' browse status is ',browse
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
                call tstamp('>','PRJ: continuing with exemplar')
              endif
            endif
          else
            call edisp(iuout,' No `exemplars` list available')
            call edisp(iuout,' returning to main menu.')
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
          else

C Something was passed in via the command line so offer choice.
            H(1)='The model configuration file holds the definition'
            H(2)='of the model to be simulated, including the names'
            H(3)='of the composing files.  Such a file has been'
            H(4)='passed to the Project Manager as a command line'
            H(5)='argument.'
            write(outs,'(a,a)') ' The command line included: ',
     &      LCMDFL(1:lnblnk(LCMDFL))
            CALL EASKAB(outs,' ','use it as the model',
     &        'specify another',IW,5)

C Find the path and local file name.
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
            else
              itisanexemplar=.false.
              confirm=.true.
              ckpath=.true.
              call NEWPRB(ITRC,confirm,ckpath,itisanexemplar,IER)
            endif
C Debug.
C            write(6,*) ' browse status is ',browse
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
        call pregist('i',ier)
      ELSEIF(INO.EQ.10)THEN

C Root name of the model.
        if(CFGOK)then
          h(1)='This `root` word will be used to help create'
          h(2)='system level file names (control, connections).'
          h(3)='It is derived from the configuration file name.'
          t24=cfgroot
          CALL EASKS(t24,' Root name for the project?',
     &     '(single word <24 char)',24,'project','root name',IER,3)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')cfgroot=t24
          changedit=.true.
        endif
      ELSEIF(INO.EQ.11)THEN

C Title of the model.
        if(CFGOK)then
 246      H(1)='The model has an associated 72 character descriptor'
          H(2)='which should be informative, e.g.:'
          H(3)=' '
          H(4)='  `PV-facade office, daylight controlled, base case`'
          ltmp=LSNAM
          CALL EASKS(ltmp,' Model description?',
     &      ' ',72,'base case simulation','model description',IER,4)
          if(ltmp(1:2).eq.'  '.or.ltmp(1:4).eq.'UNKN')then
            call usrmsg('A blank description is not allowed.',' ','W')
            goto 246
          else
            LSNAM=ltmp
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
          CALL USRMSG(' ','Insufficient data to export.','W')
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
     &                'mode: you must own the model!','W')
          goto 3
        endif

        if(INO.EQ.23)then

C Ask for file name.
          H(1)='The current model description will be placed'
          H(2)='into this file.'
          DFILE=' '
          ltmp=LCFGF
   89     CALL EASKS(ltmp,' Updated configuration file?',
     &      ' ',72,DFILE,'updated configuration file',IER,2)
          if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
            LCFGF=ltmp
          else
            call usrmsg('Please re-enter the file name. ',' ','W')
            goto 89
          endif

C Also give user option to alter the root name.
          h(1)='This `root` word will be used to help create'
          h(2)='system level file names (control, connections).'
          h(3)='If you have altered the name of the configuration'
          h(4)='fine you might want to modify the root name as'
          h(5)='well.'
          t24=cfgroot
          CALL EASKS(t24,' Root name for the project? (see help',
     &     'about whether to modify)',24,'project','root name',IER,5)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')cfgroot=t24
        endif

C If configuration file format is version 3 then check to see if there
C are any utility files. If found then ask is the user wants to save
C in older format.
        if(icfgv.eq.3)then
          foundutl=.false.
          DO 45 I=1,ncomp
            if(INDUTL(I).ne.0)foundutl=.true.
  45      CONTINUE
          if(foundutl)then
            CALL EASKAB(
     &      ' Configuration file loaded includes legacy utility file',
     &      ' names. Options:','Update to current format','continue',
     &      IW,3)
            if(IW.EQ.1)then
              icfgv=3

C Get root name and ensure that configuration file ends in .cfg.
              lcfgr=lnblnk(LCFGF)
              lcfgl=lcfgr-3
              if(lcfgr.gt.4)then
                if(LCFGF(lcfgl:lcfgr).eq.'.cfg')then
                  write(cfgroot,'(a)') LCFGF(1:lcfgl-1)
                else
                  write(cfgroot,'(a)') LCFGF(1:lcfgr)
                  write(LCFGF,'(a,a)')LCFGF(1:lcfgr),'.cfg'
                endif
              else
                write(cfgroot,'(a)') LCFGF(1:lcfgr)
                write(LCFGF,'(a,a)')LCFGF(1:lcfgr),'.cfg'
              endif
              h(1)='This `root` word will be used to help create'
              h(2)='system level file names (control, connections).'
              h(3)='It is derived from the configuration file name.'
              t24=cfgroot
              CALL EASKS(t24,' Root name for the project?',
     &         '(single word <24 char)',24,'project','root name',IER,3)
              if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')cfgroot=t24
            elseif(IW.eq.2)then
              goto 3
            endif
          endif

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
                call usrmsg('Please re-enter the file name. ',' ','W')
                goto 289
              endif
            endif
          endif
        endif

        call tstamp('>','PRJ: update configuration')
        call tstamp('>',LCFGF)
        CALL EMKCFG('-',IER)
        IF(IER.EQ.1)THEN
          call usrmsg('Problem creating new file. Check disk space and',
     &                'or file permissions ','W')
        ENDIF
        goto 3
      ELSEIF(INO.EQ.MITEM-3)THEN

C Toggle trace level.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0
        INO=-4
        GOTO 3
      ELSEIF(INO.EQ.MITEM-2)THEN
        call usrmsg('Preferences functions (under construction)',' ',
     &    'P')
      ELSEIF(INO.EQ.MITEM-1)THEN

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('prj ',0)
      ELSE
        INO=-4
        GOTO 3
      ENDIF
      INO=-2
      GOTO 3

 901  call edisp(iuout,'Error opening journal file, continuing.')
      goto 902
 900  call edisp(iuout,'Error opening cur journal file, continuing.')
      goto 902
 899  call edisp(iuout,'Error closing cur journal file.')
      goto 902

      END
     
C ********** simula
C simula commissions technical assessments and analysis.
      subroutine simula(ier)
#include "building.h"
#include "esprdbfile.h"

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive the modules.
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
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFH/NWPR,NITDF,NTSPH,NEXTCL,NEXTRC,LASTHD,
     &             LSTREC,NDBSTP,NUWPR
      common/IPVF/lipvdatf

      common/IPVSEA/nipvassmt,nipvdispjd,ipvastjd(MIPVA),
     &              ipvafnjd(MIPVA),ipvdispjd(10)

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
      common/IMAGF/imgfmt(10),imgfoc(10),limgfil(10),noimg,iton
      COMMON/ENTFILE/ENTFLNAM,IENTXIST
      common/user/browse

      dimension ITEMS(25)

      character doit*248,tmode*8,tfile*72,ITEMS*33,H*72,key*1
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,spflnam*72,longtfile*144,longtfiledos*144
      CHARACTER*72 LAPROB,LCFGF,ENTFLNAM,lipvdatf
      character cfgroot*24,cr*24,spfdescr*8

C dd is character array for selecting simulation parameter sets.
      character dd(MSPS+3)*33,de*8,aut*12
      character imgfmt*4,imgfoc*4,limgfil*72,t72*72

      character brw*8,outs*124,dq*1,fs*1
      character descra*7,descrb*7,descrst*10,descrfn*10

      logical DOK,OK,MSTROK,MSTRZN,XST,modparms
      logical browse,concat,hri,defok,unixok
      logical mlcok,bndryok,bndrysxc,confok,geofok,oprfok,prob

      modparms=.false.
      lr=lnblnk(cfgroot)
      cr=cfgroot
      if(nsset.gt.0)isset=1
      t72='  '

C When invoking doit commands we need to have a file separator
C and a double quote character available in case of spaces in name.
      call isunix(unixok)
      if(unixok)then
        dq = char(34)
        fs = char(47)
      else
        dq = char(34)
        fs = char(92)
      endif

C Do an initial check to see if the model is attributed and
C constructions are known.
      if(INDCFG.eq.2)then
        continue
      else
        call checkmodel(mlcok,bndryok,bndrysxc,confok,geofok,oprfok,
     &    prob)
        if(mlcok)then
          call edisp(iuout,'Surface construction attributes ok.')
        endif
        if(bndryok)then
          call edisp(iuout,'Surface connection attributes ok.')
        endif
        if(bndrysxc)then
          call edisp(iuout,'Surface topology (first check) ok.')
        endif
        if(.NOT.prob)then
          call edisp(iuout,'Surface topology (second check) ok.')
        endif
        if(geofok.AND.confok.AND.oprfok)then
          call edisp(iuout,'Mandatory zone files exist.')
        endif
      endif

      if(noimg.gt.0)call imgdisp(1,'FPER',ier)
      
    3 INO=-4
      if(nsset.eq.0)then
        ITEMS(1) ='a simulation presets >> none  '
        ITEMS(2)='  ___________________________ '
        M=2
      else
        write(ITEMS(1),'(a,i2,a,i2,a)') 'a simulation presets (',isset,
     &    ' of',nsset,')'
        write(ITEMS(2),'(a,a8)')  'b  set name: ',spfdescr(isset)
        write(ITEMS(3),'(a,i4)')  'c  startup days:',isstup
        if(isbnstep.le.1)then
          write(ITEMS(4),'(a,i3)')'d  zone ts/hr:',isbnstep
        else
          if(isavgh.eq.0)then
            write(ITEMS(4),'(a,i3,a)')  'd  zone ts/hr:',isbnstep,
     &        ' each ts saved'
          else
            write(ITEMS(4),'(a,i3,a)')  'd  zone ts/hr:',isbnstep,
     &        ' saved 1 ts/hr'
          endif
        endif
        if(INDCFG.eq.2.or.INDCFG.eq.3)then
          write(ITEMS(5),'(a,i3)')'e  plant ts (per bldg ts):',ispnstep
        else
          write(ITEMS(5),'(a)')   'e  plant timesteps/hr: N/A'
        endif
        write(ITEMS(6),'(a,i3)') 'f  result save level:',issave

C Display the simulation period.
        CALL EDAY(isstday(isset),isstmon(isset),ijdstart)
        call stdate(iyear,ijdstart,descra,descrst)
        CALL EDAY(isfnday(isset),isfnmon(isset),ijdfinish)
        call stdate(iyear,ijdfinish,descrb,descrfn)
        write(ITEMS(7),'(4a)')'g  from: ',descrst,' - ',descrfn
        if(INDCFG.ne.2)then
          write(ITEMS(8),'(a,a16)')'h  zone results: ',
     &      sblres(isset)(1:16)
        else
          write(ITEMS(8),'(a)')  '   zone results: N/A'
        endif
        if(IAIRN.ge.1)then
          write(ITEMS(9),'(a,a16)')'i  flow results: ',
     &      sflres(isset)(1:16)
        else
          write(ITEMS(9),'(a)')  '   flow results: N/A'
        endif
        if(INDCFG.eq.2.or.INDCFG.eq.3)then
          write(ITEMS(10),'(a,a15)')'j  plant results: ',
     &      splres(isset)(1:15)
        else
          write(ITEMS(10),'(a)')  '   plant results: N/A'
        endif
        if(ispmxist.gt.0)then
          write(ITEMS(11),'(a)')  'k  : '
        else
          write(ITEMS(11),'(a)')  '   : N/A'
        endif
        if(MSTROK)then
          write(ITEMS(12),'(a,a15)')'l  moist results: ',
     &      smstres(isset)(1:15)
        else
          write(ITEMS(12),'(a)')  '   moisture results: N/A'
        endif
        if(ientxist.gt.0)then
          write(ITEMS(13),'(a,a15)')'m  elect results: ',
     &      selres(isset)(1:15)
        else
          write(ITEMS(13),'(a)')  '   electrical results: N/A'
        endif
        if(lnblnk(lipvdatf).eq.0)then
          write(ITEMS(14),'(a)')  '   ipv report   : N/A'
        elseif(lipvdatf(1:7).eq.'UNKNOWN')then
          write(ITEMS(14),'(a)')  '   ipv report   : N/A'
        else
          write(ITEMS(14),'(a,a15)')'n  ipv report: ',sipvres(1:15)
        endif    
        ITEMS(15)='o save or dereference parameters '
        ITEMS(16)='  ______________________________ '
        M=16
      endif
      if(INDCFG.eq.2)then
        ITEMS(M+1) ='p system only simulation         '
      else
        ITEMS(M+1) ='p integrated simulation          '
      endif
      ITEMS(M+2) ='r   fluid flow (stand-alone)     '
      ITEMS(M+3) ='s   visual impact                '
      ITEMS(M+4) ='t   integrated performance view  '
      ITEMS(M+5) ='  ______________________________ '
      ITEMS(M+6) ='? help                           '
      ITEMS(M+7) ='- exit this menu                 '
      MITEM=M+7

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
      H(3)='combined building/ plant models may be evaluated.'
      H(4)=' '
      H(5)='The other options are useful when only one of the above'
      H(6)='technical sub-systems is to be appraised, e.g. a CFD only'
      H(7)='analysis with fixed buoyancy.  Note however that such a'
      H(8)='simulation can still be commissioned via the'
      H(9)='`Integrated` option.'
      H(10)=' '
      H(11)='You can define simulation options such as the assessment'
      H(12)='period prior to commissioning simulations. Several sets'
      H(13)='can be defined (e.g. standard seasonal assessments). '
      H(14)='A default set is always available. '

      CALL EMENU('Simulation controller',ITEMS,MITEM,INO)
      if(INO.EQ.MITEM)then
        if(modparms)then
          CALL EMKCFG('-',IER)
          modparms=.false.
        endif
        RETURN
      elseif(INO.EQ.MITEM-1)then

C List help text for the simulations menu.
        CALL PHELPD('Simulation',14,'-',0,0,IER)
      elseif(INO.EQ.MITEM-6)then

C << Add logic to determine whether bps/mfs/dfs should be run. >> 
C Invoke simulator.
        if(nsset.gt.0)then
          h(1)='Interactive mode uses simulation parameters as defaults'
          h(2)='which can be overwritten. The silent running mode '
          h(3)='uses the parameters to run in automatic mode. '
          CALL EASKAB('Simulation interaction options: (see help)',' ',
     &       'run interactively','silent running mode',isauto,3)
          if(isauto.eq.1)aut=' interactive'
          if(isauto.eq.2)aut=' silent'
          call tstamp('>','PRJ: beginning simulation with param set')
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          if(unixok)then
            call addpath(LCFGF,longtfile,concat)
          else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
            call addpath(LCFGF,longtfile,concat)
            call cmdfiledos(longtfile,longtfiledos,ier)

C Debug the patched file name.
            write(outs,'(2a)') 'corrected file ',
     &        longtfiledos(1:lnblnk(longtfiledos))
            call edisp248(iuout,outs,100)
            longtfile=' '
            longtfile=longtfiledos
          endif

C If browsing pass this information to bps.
          if(browse)then
            brw = ' -b yes'
          else
            brw = ' -b no '
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
          call usrmsg('Begining simulation via',doit,'-')
          call runit(doit,tmode)
          return
        else
          call tstamp('>','PRJ: beginning simulation')
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          if(unixok)then
            call addpath(LCFGF,longtfile,concat)
          else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
            call addpath(LCFGF,longtfile,concat)
            call cmdfiledos(longtfile,longtfiledos,ier)

C Debug the patched file name.
            write(outs,'(2a)') 'corrected file ',
     &        longtfiledos(1:lnblnk(longtfiledos))
            call edisp(iuout,outs)
            longtfile=' '
            longtfile=longtfiledos
          endif

C If browsing pass this information to bps.
          if(browse)then
            brw = ' -b yes'
          else
            brw = ' -b no '
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
          call usrmsg('Begining simulation via',doit,'-')
          call runit(doit,tmode)
          return
        endif
      elseif(INO.EQ.MITEM-5)then
        H(1)='The first option computes the pressure induced'
        H(2)='fluid flow within a network of connected flow'
        H(3)='components representing the building and/or plant'
        H(4)='system in whole or part.  The second option permits'
        H(5)='a steady state or transient, n-dimensional flow'
        H(6)='analysis of a single zone by the technique of'
        H(7)='computational fluid dynamics.'
        H(8)=' '
        H(9)='In both cases, because the temperature distribution'
        H(10)='throughout the whole building/ plant model is'
        H(11)='not computed, buoyancy effects are time invariant.'
        H(12)='For models where this assumption is inappropriate'
        H(13)='the `Integrated` simulation option should be used.'
        CALL EASKABC('Fluid flow analysis via:',' ','Nodal Network',
     &      'Computational Fluid Dynamics','cancel',IW,13)
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
          call usrmsg('beginning flow only assessment via',doit,'-')
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
          call usrmsg('beginning CFD only assessment via',doit,'-')
          call runit(doit,tmode)
        ENDIF
      elseif(INO.EQ.MITEM-4)then

C Visual simulation.  Starts e2r with configuration file. Depending on
C if Unix or Windows setup the appropriate path.
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
          write(doit,'(3a,3i4,3a)') 'e2r -mode ',tmode,
     &      ' -s ',iappw,iappx+35,iappy+40,' -file ',
     &      longtfile(1:lnblnk(longtfile)),' &'
        else
          write(doit,'(5a)') 'e2r -mode ',tmode,
     &      ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),' &'
        endif
        call usrmsg('beginning visual assessment via',doit,'-')
        call runit(doit,tmode)
      elseif(INO.EQ.MITEM-3)then

C IPV for the actual number of simulations required.
        call ipvactions(ier)
      elseif(MITEM.eq.9.and.INO.EQ.1)then

C Case of no initial sets, offer default or user defined sets.
        if(lnblnk(lipvdatf).eq.0.or.lipvdatf(1:7).eq.'UNKNOWN')then
          h(1)='Simulation options (timesteps, start and finish dates,'
          h(2)='startup periods, names of results files, etc.) can be '
          h(3)='defined in advance. You can define a one or more sets'
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
            call ripvdat(ifil+1,lipvdatf,ier)
          else
            call usrmsg('IPV definition not found.',lipvdatf,'W')
            INO=-4
            GOTO 3
          endif
          h(1)='Simulation options (timesteps, start and finish dates,'
          h(2)='startup periods, names of results files, etc.) can be '
          h(3)='defined in advance. You can define a one or more sets'
          h(4)='of parameters, e.g. to support standard assessments'
          h(5)='for a project or seasonal assessments. '
          h(6)='An IPV description has is associated with this model'
          h(7)='and matching parameter sets will be created. '
          DOK=.true.
          CALL ASKOK('An IPV description has been found. Proceed to',
     &      'create matching parameter sets?',OK,DOK,7)
          if(.NOT.OK)then
            INO=-4
            GOTO 3
          endif
          ID=3
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
            call edisp(iuout,'Scanning for startup days')
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
            h(1)='Typical names are win spr sum. '
            de='default'
            CALL EASKS(de,'Name for set ?',' ',8,'win','set name',IER,1)
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
            call ipv2simpar('-')
          endif
          modparms=.true.
        endif
      elseif(MITEM.eq.23.and.INO.EQ.1)then

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
        h(7)='three or five parameter sets. '
        h(8)=' '
        h(9)='Selecting one of the existing sets will load the'
        h(10)='associated parameters and any simulation commissioned'
        h(11)='will use this information.'
        h(12)=' '
        h(13)='NOTE: if you have run several assessments, you should'
        h(14)='select the correct set before going on to results '
        h(15)='analysis. '

        CALL EMENU('Parameter sets',dd,ic,IRT)
        if(IRT.ge.ic) then
          INO=-4
          GOTO 3
        elseif(IRT.eq.ic-1) then
          CALL PHELPD('Simulation sets',15,'-',0,0,IER)
          goto 170
        elseif(IRT.eq.ic-2.and.nsset.lt.5)then

C It is possible to add another set so ask user and set file names.
C << note this probably should check to see if there is an IPV >>
C << description first >>
          h(1)='Typical names are win spr sum. '
          de='win'
          CALL EASKS(de,' Name for set ?',' ',8,'win','set name',IER,1)
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
        call edisp(iuout,'Rechecking startup days')
        call scntcnst(TDM,istd,TCM,ISTC)
      elseif(MITEM.eq.23.and.INO.EQ.2)then

C If there is an IPV descripton and this isset is one,
C do not allow this set name to be changed.
        if(nipvassmt.eq.nsset)then
          call usrmsg('Sorry, and IPV is being used and the set',
     &                'names are pre-defined','W')
        else
          h(1)='Typical names are win spr sum. '
          de=spfdescr(isset)
          CALL EASKS(de,' Name for set ?',' ',8,spfdescr(isset),
     &                  'set name',IER,1)
          write(spfdescr(isset),'(a8)')de(1:8)
          modparms=.true.
        endif
      elseif(MITEM.eq.23.and.INO.EQ.3)then
        if(INDCFG.eq.2)then
          call edisp(iuout,'Startup days not applicable to plant.')
        else
          call edisp(iuout,'Rechecking startup days')
          call scntcnst(TDM,istd,TCM,ISTC)
          H(1)='Presimulation days to allow model to reach equlabrium.'
          H(2)='The default is the suggested value for this model.'
          CALL EASKI(isstup,'Presimulation period (days): ',' ',
     &       0,'F',100,'W',ITCNST,'presimulation days',IER,2)
          modparms=.true.
        endif
      elseif(MITEM.eq.23.and.INO.EQ.4)then
        if(itdflg.ne.0)then
          write(outs,'(2a,i3,a)') 'The model includes a temporal file',
     &      ' with ',ntsph,' per hour frequency. Please use the same.'
          call edisp(iuout,outs)
        endif
        H(1)='Zone-side timesteps range from 1 to 60 per hour.'
        H(2)='Note: if the model includes temporal data then you must'
        H(3)='use the same zone-side teimstep as in the temporal file.'
        CALL EASKI(isbnstep,'Zone-side timesteps per hour: ',' ',
     &       1,'F',60,'W',2,'zone steps per hour',IER,1)
        if(isbnstep.gt.1)then
          if(itdflg.ne.0)then

C There is temporal data so must save at each timestep.
            write(6,*) 'temporal data found so save each timestep'
            isavgh=0
          else
            H(1) ='Bps calculates the value of each state variable '
            H(2) ='at each computational time step. The default action'
            H(3) ='is to transfer each value at each computational'
            H(4) ='time step to the results database(s).'
            H(5) ='This option allows to average results over one'
            H(6) ='hour before transfer to the results database(s),'
            H(7) ='thus decreasing the size of results database(s).'
            defok=.false.
            hri=.false.
            call askok(' Hourly results integration: ',' ',hri,defok,7)
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
      elseif(MITEM.eq.23.and.INO.EQ.5)then
        if(INDCFG.eq.2.or.INDCFG.eq.3)then
          H(1)='Plant-side timesteps range from 1 to 60 per hour.'
          CALL EASKI(ispnstep,'Plant-side timesteps per zone step: ',
     &      ' ',1,'F',100,'W',10,'plt steps per hour',IER,1)
          modparms=.true.
        endif
      elseif(MITEM.eq.23.and.INO.EQ.6)then
        H(1)='Save >> 0 summary table '
        H(2)='Save >> 1 zone temps & plant injection '
        H(3)='Save >> 2 (1+) surface temps '
        H(4)='Save >> 3 (2+) node temps '
        H(5)='Save >> 4 (2+) zone and surface energy balances '
        CALL EASKI(issave,'Save level for predictions (0-4)?: ',
     &      '(see help)',0,'F',4,'F',4,'save level',IER,5)
        modparms=.true.
      elseif(MITEM.eq.23.and.INO.EQ.7)then 
        CALL EDAY(isstday(isset),isstmon(isset),ISDS)
        CALL EDAY(isfnday(isset),isfnmon(isset),ISDF)
        call EASKPER('Assessment period:',ISDS,ISDF,IFDAY,IER)
        call EDAYR(ISDS,isstday(isset),isstmon(isset))
        call EDAYR(ISDF,isfnday(isset),isfnmon(isset))
        modparms=.true.
      elseif(MITEM.eq.23.and.INO.EQ.8)then
        h(1)='This library holds all the zone-side performance indices.'
        t72=sblres(isset)
        CALL EASKS(t72,'Zone-side results library?',' ',72,
     &    'zones.res','bld library',IER,1)
        sblres(isset)=t72
        modparms=.true.
      elseif(MITEM.eq.23.and.INO.EQ.9)then
        if(IAIRN.ge.1)then
          h(1)='This library holds the network flow predictions.'
          t72=sflres(isset)
          CALL EASKS(t72,'Network flow results library?',' ',72,
     &    'network.mfr','flow results library',IER,1)
          sflres(isset)=t72
          modparms=.true.
        endif
      elseif(MITEM.eq.23.and.INO.EQ.10)then
        if(INDCFG.eq.2.or.INDCFG.eq.3)then
          h(1)='This library holds the plant system predictions.'
          t72=splres(isset)
          CALL EASKS(t72,'Plant results library?',' ',72,
     &    'plant.res','plant results library',IER,1)
          splres(isset)=t72
          modparms=.true.
        endif
      elseif(MITEM.eq.23.and.INO.EQ.11)then
        if(ispmxist.gt.0)then
          continue
        endif
      elseif(MITEM.eq.23.and.INO.EQ.12)then
        if(MSTROK)then
          h(1)='This library holds the moisture predictions.'
          t72=smstres(isset)
          CALL EASKS(t72,'Moisture results library?',' ',72,
     &    'moist.res','moisture results library',IER,1)
          smstres(isset)=t72
          modparms=.true.
        endif
      elseif(MITEM.eq.23.and.INO.EQ.13)then
        if(ientxist.gt.0)then
          h(1)='This library holds the electrical predictions.'
          t72=selres(isset)
          CALL EASKS(t72,'Electrical results library?',' ',72,
     &    'elect.res','electrical results library',IER,1)
          selres(isset)=t72
          modparms=.true.
        endif
      elseif(MITEM.eq.23.and.INO.EQ.14)then
        if(lnblnk(lipvdatf).eq.0)then
        elseif(lipvdatf(1:7).eq.'UNKNOWN')then
        else
          h(1)='This is the name of the (intermediate) IPV report.'
          t72=sipvres
          CALL EASKS(t72,'IPV (intermediate) report?',' ',72,
     &    'IPV.rep','IPV report file',IER,1)
          sipvres=t72
          modparms=.true.
        endif
      elseif(MITEM.eq.23.and.INO.EQ.15)then
        h(1)='You can save changes to simulation parameter sets or'
        h(2)='dereference the current sets (for example to import new'
        h(3)='IPV assessment periods. '
        CALL EASKABC('Options: ',' ',
     &    'save current sets','clear sets','continue',IDO,3)
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
            call usrmsg('Sets are current, nothing to update.',' ','-')
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

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive the modules.
      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      common/appw/iappw,iappx,iappy
      common/C21/IFCFG,cfgroot,LCFGF
      common/IPVF/lipvdatf
      common/uhome/upath
      common/exporttg/xfile,tg,delim
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

      common/IPVSEA/nipvassmt,nipvdispjd,ipvastjd(MIPVA),
     &              ipvafnjd(MIPVA),ipvdispjd(10)

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
      character ltpath*72,filen*72,ltmp*72,upath*72
      character basefile*144,variantfile*144,outs*124,outsd*124
      character word1*20,word2*40,word3*20,outstr*124
      character xfile*72,tg*1,delim*1,tab*1,fs*1

      logical XST,browse,remote,concat,unixok
      logical atreports,im2

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

      ibasefile=11
      ivarfile=12
      NSIGFIG=3
      tab=CHAR(9)

      if(nsset.gt.0)isset=1
      write(basefile,'(a)') sipvres(1:lnblnk(sipvres))
      write(variantfile,'(a)') sipvres(1:lnblnk(sipvres))

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
      H(1)='An Integrated Performance View is a collection of '
      H(2)='performance indicators which have been formally defined'
      H(3)='(see Context menu).  Please see your system '
      H(4)='administrator for details of its implementation. '
      if(lnblnk(lipvdatf).eq.0)then
        CALL PHELPD('IPV message',4,'-',0,0,IER)
        return
      elseif(lipvdatf(1:7).eq.'UNKNOWN')then
        CALL PHELPD('IPV message',4,'-',0,0,IER)
        return
      else
        call FINDFIL(lipvdatf,XST)
        IF(XST)then
          call ripvdat(ifil+1,lipvdatf,ier)
          if(ier.ne.0)then
            CALL PHELPD('IPV message',4,'-',0,0,IER)
            return
          endif
        else
          CALL PHELPD('IPV message',4,'-',0,0,IER)
          return
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
      h(20)=' *for each variant it reports heating demand kWh/m2/a'
      h(21)='  and for the whole model (kWh/a), heating capacity kW/m2'
      h(22)='  and for the whole model (kW/a diversified).'
      h(23)=' *for each variant it reports heating therms (converted'
      h(24)='  as kWh * 0.0341296)'
      h(25)=' *for each variant it reports cooling demand kWh/m2/a'
      h(26)='  and for the whole model (kWh/a), cooling capacity kW/m2'
      h(27)='  and for the whole model (kW/a)'
      h(28)=' *difference in heating, therms and cooling by area and'
      h(29)='  for the whole building.'
      CALL EASKABCD('IPV options: (see help)',' ',
     &  'run required simulations','extract reports',
     &  'compare reports','continue',irs,29)
      if(irs.eq.1)then
        h(1)='Interactive mode uses simulation parameters as '
        h(2)='defaults. The silent running mode uses the'
        h(3)='parameters to run in automatic mode. '
        CALL EASKAB('Simulation interaction options: (see help)',
     &    ' ','run interactively','silent running mode',isauto,3)
        call tstamp('>','PRJ: beginning IPV simulations.')
        doit = ' '
        call tchild(ICPMOD)
        call termode(ICPMOD,tmode)
        if(isauto.eq.1)aut=' default'
        if(isauto.eq.2)then
          aut=' silent '
          tmode='text'
        endif

C Temporarily assume no browsing if in ipv mode.
        if(browse)then
          brw = ' -b yes'
        else
          brw = ' -b no '
        endif

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
          call usrmsg('beginning assessment via',doit,'-')
          if(isauto.eq.2)then
            call runit(doit,'-')
          else
            call runit(doit,tmode)
          endif
          call edisp(iuout,'IPV run complete.')
  667   continue
      elseif(irs.eq.2)then
        h(1)='Interactive mode uses simulation parameters as '
        h(2)='defaults. The silent running mode uses the'
        h(3)='parameters to run in automatic mode. '
        CALL EASKAB('IPV extraction options: (see help)',
     &    ' ','run interactively','silent running mode',isauto,3)
        call tstamp('>','PRJ: beginning IPV recovery.')
        doit = ' '
        call tchild(ICPMOD)
        call termode(ICPMOD,tmode)
        if(isauto.eq.1)aut=' default'
        if(isauto.eq.2)then
          aut=' silent '
          tmode='text'
        endif
        h(1)='IPV recovery assumes that the relevant results '
        h(2)='libraries for each simulation are available. '
        h(3)='The all option does each in sequence. '
        call easkabcd('IPV recovery options:',' ','intermediate',
     &    'scan intermediate ->annual','all','continue',IRT,3)

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

            h(1)='IPV recovery assumes that the relevant results '
            h(2)='libraries for each simulation are available. '
            h(3)='The all option does each in sequence. '
            if(iappw.gt.0.and.iappw.le.200)then
              write(doit,'(3a,3i4,4a)') 'res -mode ',tmode,
     &          ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &          tfile(1:lnblnk(tfile)),runtag(ij),aut
            else
              write(doit,'(6a)') 'res -mode ',tmode,
     &           ' -file ',tfile(1:lnblnk(tfile)),runtag(ij),aut
            endif
            call usrmsg('beginning performance recovery via',doit,'-')
            if(isauto.eq.2)then
              call runit(doit,'-')
            else
              call runit(doit,tmode)
            endif
            write(outs,'(3a)') 'IPV extract ',
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
            call usrmsg('beginning performance recovery via',doit,'-')
            if(isauto.eq.2)then
              call runit(doit,'-')
            else
              call runit(doit,tmode)
            endif
            call edisp(iuout,'IPV report complete.')
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
          call usrmsg('beginning performance recovery via',doit,'-')
          if(isauto.eq.2)then
            call runit(doit,'-')
          else
            call runit(doit,tmode)
          endif
        endif
      elseif(irs.eq.3)then

C Compare two IPV reports.  First ask for separator.

C Toggle delimeter.
        H(1) ='Tabular data can be sent to file with various '
        H(2) ='delimiters: '
        H(3) ='  spaces (format using spaces to lineup columns)'
        H(4) ='  single space between columns'
        H(5) ='  comma separator (for excel)'
        H(6) ='  tab separator (for excel)'
        H(7) ='  tagged - marked up data'
        CALL EASKATOG('Delimeter to use between columns of data:',' ',
     &    'normal spaces','single space','tab','comma','tagged',
     &    'continue',' ',IWM,7)
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
        endif

 41     llt=lnblnk(basefile)
        h(1)='This is the name of the IPV report for the base case'
        h(2)='model (or the model you want to reference against).'
#ifdef X11
        if(llt.lt.96)then
          CALL EASKF(basefile,'IPV base case report file ?',
     &      ' ',96,'xxx.rep','IPV base report',IER,2)
        elseif(llt.ge.96.and.llt.lt.124)then
          CALL EASKF(basefile,'IPV base case report file ?',
     &      ' ',124,'xxx.rep','IPV base report',IER,2)
        elseif(llt.ge.124.and.llt.le.144)then
          CALL EASKF(basefile,'IPV base case report file ?',
     &      ' ',144,'xxx.rep','IPV base report',IER,2)
        endif
#else
        CALL EASKF(basefile,'IPV base case report file ?',
     &    ' ',144,'xxx.rep','IPV base report',IER,2)
#endif
        lnf=lnblnk(basefile)
        inquire (file=basefile(1:lnf),exist=xst)
        if(xst)then
          call erpfree(ibasefile,istat)
          OPEN (ibasefile,FILE=basefile(1:lnf),ACCESS='SEQUENTIAL',
     &      STATUS='OLD',IOSTAT=ISTAT)
          if(istat.eq.0)then
            call edisp(iuout,'base model is')
            call edisp(iuout,basefile)
          else
            call edisp(iuout,'problem with base case file...')
            goto 41
          endif
        endif

C Ask the name of the design variant file.
 40     llt=lnblnk(variantfile)
        h(1)='This is the name of the IPV report for the other'
        h(2)='model.'
#ifdef X11
        if(llt.lt.96)then
          CALL EASKF(variantfile,'IPV variant report file ?',
     &      ' ',96,'xxx.rep','IPV variant report',IER,2)
        elseif(llt.ge.96.and.llt.lt.124)then
          CALL EASKF(variantfile,'IPV variant report file ?',
     &      ' ',124,'xxx.rep','IPV variant report',IER,2)
        elseif(llt.ge.124.and.llt.le.144)then
          CALL EASKF(variantfile,'IPV variant report file ?',
     &      ' ',144,'xxx.rep','IPV variant report',IER,2)
        endif
#else
        CALL EASKF(variantfile,'IPV variant report file ?',
     &    ' ',144,'xxx.rep','IPV variant report',IER,2)
#endif
        lnf=lnblnk(variantfile)
        inquire (file=variantfile(1:lnf),exist=xst)
        if(xst)then
          call erpfree(ivarfile,istat)
          OPEN (ivarfile,FILE=variantfile(1:lnf),ACCESS='SEQUENTIAL',
     &      STATUS='OLD',IOSTAT=ISTAT)
          if(istat.eq.0)then
            call edisp(iuout,'variant model is')
            call edisp(iuout,variantfile)
          else
            call edisp(iuout,'problem with variant file...')
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
          call edisp(iuout,'For the base case model:')
          if(delim.eq.'-')then
            call edisp(iuout,
     &            '       Demand              Capacity')
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
          write(outs,'(a,F7.3,F10.2,F9.3,F10.2)') 'Heat  ',
     &      anu_ht_m2(1),
     &      anu_ht_bld(1),anu_ht_cap_m2(1),anu_ht_cap_bld(1)
          call eddisp(iuout,outs)
          write(outs,'(a,F7.3,F10.2)') 'Therm ',anu_ht_thrm_m2(1),
     &      anu_ht_thrm_bld(1)
          call eddisp(iuout,outs)
          write(outs,'(a,F7.3,F10.2,F9.3,F10.2)') 'Cool  ',
     &      anu_cl_m2(1),
     &      anu_cl_bld(1),anu_cl_cap_m2(1),anu_cl_cap_bld(1)
          call eddisp(iuout,outs)
          write(outs,'(a,F7.2,F10.1)') 'Total ',anu_tot_m2(1),
     &      anu_tot_bld(1)
          call eddisp(iuout,outs)
          call edisp(iuout,'For the variant model:')
          write(outs,'(a,F7.3,F10.2,F9.3,F10.2)') 'Heat  ',
     &      anu_ht_m2(2),
     &      anu_ht_bld(2),anu_ht_cap_m2(2),anu_ht_cap_bld(2)
          call eddisp(iuout,outs)
          write(outs,'(a,F7.3,F10.2)') 'Therm ',anu_ht_thrm_m2(2),
     &      anu_ht_thrm_bld(2)
          call eddisp(iuout,outs)
          write(outs,'(a,F7.3,F10.2,F9.3,F10.2)') 'Cool  ',
     &      anu_cl_m2(2),
     &      anu_cl_bld(2),anu_cl_cap_m2(2),anu_cl_cap_bld(2)
          call eddisp(iuout,outs)
          write(outs,'(a,F7.2,F10.1)') 'Total ',anu_tot_m2(2),
     &      anu_tot_bld(2)
          call eddisp(iuout,outs)
          call edisp(iuout,' ')
          write(outs,'(4a)')'Diff between ',
     &      basefile(1:lnblnk(basefile)),
     &      ' & ',variantfile(1:lnblnk(variantfile))
          call edisp(iuout,outs)
          if(delim.eq.'-')then
            call edisp(iuout,
     &            '       Demand              Capacity')
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
          anu_tot_m2(3)=anu_tot_m2(1)-anu_tot_m2(2)
          anu_tot_bld(3)=anu_tot_bld(1)-anu_tot_bld(2)
          anu_ht_cap_m2(3)=anu_ht_cap_m2(1)-anu_ht_cap_m2(2)
          anu_ht_cap_bld(3)=anu_ht_cap_bld(1)-anu_ht_cap_bld(2)
          anu_cl_cap_m2(3)=anu_cl_cap_m2(1)-anu_cl_cap_m2(2)
          anu_cl_cap_bld(3)=anu_cl_cap_bld(1)-anu_cl_cap_bld(2)
          write(outs,'(a,F7.3,F10.2,F9.3,F10.2)') 'Heat  ',
     &      anu_ht_m2(3),
     &      anu_ht_bld(3),anu_ht_cap_m2(3),anu_ht_cap_bld(3)
          call eddisp(iuout,outs)
          write(outs,'(a,F8.2,F10.2)') 'Therm ',anu_ht_thrm_m2(3),
     &      anu_ht_thrm_bld(3)
          call eddisp(iuout,outs)
          write(outs,'(a,F7.3,F10.2,F9.3,F10.2)') 'Cool  ',
     &      anu_cl_m2(3),
     &      anu_cl_bld(3),anu_cl_cap_m2(3),anu_cl_cap_bld(3)
          call eddisp(iuout,outs)
          write(outs,'(a,F8.2,F10.0)') 'Total ',anu_tot_m2(3),
     &      anu_tot_bld(3)
          call eddisp(iuout,outs)

C If comma separated delimeter then also write out a single line
C summary (Patch for IEA_Annex 46).
          if(delim.eq.'C')then
            call edisp(iuout,
     & 'Location,building heating kWh/a,heating therms,cooling kWh/a')
            write(outs,'(2a,F11.2,a,F11.2,a,F11.2)')
     &        variantfile(1:lnblnk(variantfile)),' ',anu_ht_bld(3),
     &        ' ',anu_ht_thrm_bld(3),' ',anu_cl_bld(3)
            call eddisp(iuout,outs)
          endif

          CALL EASKAB('comparison options:',' ','exit',
     &      'do another report',IW,1)
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
C            write(6,*) outstr(1:lnblnk(outstr))
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
C                call edisp(iuout,' found performance m^2')
              elseif(word2(1:20).eq.'building_performance')then
                atreports=.true.
                im2=.false.
C                call edisp(iuout,' found performance building')
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
C                write(6,*) outstr(1:lnblnk(outstr))
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

C Get the total data line, 2nd is the value.
                CALL STRIPC(ifu,OUTSTR,0,ND,1,'*data',IER)
                k=5
                CALL EGETW(OUTSTR,K,WORD1,'W','total',IFLAG)
C                write(6,*) outstr(1:lnblnk(outstr))
                if(im2)then
                  CALL EGETWR(OUTSTR,K,anu_tot_m2(ipass),0.,0.,'-',
     &              'total m2',IER)
                else
                  CALL EGETWR(OUTSTR,K,anu_tot_bld(ipass),0.,0.,'-',
     &              'total bld',IER)
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
C                call edisp(iuout,' found capacity')
              elseif(word2(1:17).eq.'building_capacity')then
                atreports=.true.
                im2=.false.
C                call edisp(iuout,' found building capacity')
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
C                  call edisp(iuout,' confirm m^2 capacity')
                elseif(word3(1:2).eq.'kW')then
                  im2=.false.
C                  call edisp(iuout,' confirm building capacity')
                endif
                CALL STRIPC(ifu,OUTSTR,0,ND,1,'*format',IER)
                CALL STRIPC(ifu,OUTSTR,0,ND,1,'*fields',IER)
                CALL STRIPC(ifu,OUTSTR,0,ND,1,'*data',IER)
                k=5
C                write(6,*) outstr(1:lnblnk(outstr))
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
  1   call edisp(iuout,'reached end of file looking for report')
      goto 39
  2   call edisp(iuout,'reached end of file looking for summary')
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
      subroutine checkmodel(mlcok,bndryok,bndrysxc,confok,geofok,
     &                      oprfok,prob)
#include "building.h"

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive the modules.
      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

      character H*72,CXSTR*78
      character SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character SSPARENT*12
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      character ZSDES*28,ZSDESC*20,ZSDESS*16,zname*12,zdesc*64,zn*12

      logical XST,asked,dok,sorted,problem
      logical mlcok,bndryok,bndrysxc,confok,geofok,oprfok,prob,dup

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
        if(SSOTHER(i)(1:7).eq.'UNKNOWN')then
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
      do 42 i=1,ncon
        CALL ZSID(IC1(I),IE1(I),ZSDES,ZSDESC,ZSDESS)
        CALL CONXINFO(1,i,CXSTR)
        if(ICT(i).eq.0.and.SSOTHER(i)(1:5).ne.'EXTER')then
          bndrysxc=.false.
          if(lh.lt.58)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary looking for EXTER in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection is... ',CXSTR(1:50)
            lh=lh+1
            write(h(lh),'(2a)') 'found surface attribute... ',SSOTHER(i)
          endif
        endif
        if(ICT(i).eq.1.and.SSOTHER(i)(1:5).ne.'SIMIL')then
          bndrysxc=.false.
          if(lh.lt.58)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary looking for SIML in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection is... ',CXSTR(1:50)
            lh=lh+1
            write(h(lh),'(2a)') 'found surface attribute... ',SSOTHER(i)
          endif
        endif
        if(ICT(i).eq.2.and.SSOTHER(i)(1:5).ne.'CONST')then
          bndrysxc=.false.
          if(lh.lt.58)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary looking for CONST in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection is... ',CXSTR(1:50)
            lh=lh+1
            write(h(lh),'(2a)') 'found surface attribute... ',SSOTHER(i)
          endif
        endif
        if(ICT(i).eq.3.and.SSOTHER(i)(1:5).eq.'CONST')then
          bndrysxc=.false.
          if(lh.lt.58)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary unexpected CONST in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection... ',CXSTR(1:50)
            lh=lh+1
            write(h(lh),'(2a)') 'found surface attribute... ',SSOTHER(i)
          endif
        endif
        if(ICT(i).eq.3.and.SSOTHER(i)(1:5).eq.'SIMIL')then
          bndrysxc=.false.
          if(lh.lt.58)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary unexpected SIMIL in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection... ',CXSTR(1:50)
            lh=lh+1
            write(h(lh),'(2a)') 'found surface attribute... ',SSOTHER(i)
          endif
        endif
        if(ICT(i).eq.3.and.SSOTHER(i)(1:5).eq.'EXTER')then
          bndrysxc=.false.
          if(lh.lt.59)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary unexpected EXTER in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection... ',CXSTR(1:50)
          endif
        endif
        if(ICT(i).eq.3.and.SSOTHER(i)(1:5).eq.'GROUN')then
          bndrysxc=.false.
          if(lh.lt.59)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary unexpected GROUN in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection... ',CXSTR(1:50)
          endif
        endif
        if(ICT(i).eq.4.and.SSOTHER(i)(1:5).ne.'GROUN')then
          bndrysxc=.false.
          if(lh.lt.58)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary looking for GROUN in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection... ',CXSTR(1:50)
            lh=lh+1
            write(h(lh),'(2a)') 'found surface attribute... ',SSOTHER(i)
          endif
        endif
        if(ICT(i).eq.5.and.SSOTHER(i)(1:5).ne.'ADIAB')then
          bndrysxc=.false.
          if(lh.lt.58)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary looking for ADIAB in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection... ',CXSTR(1:50)
            lh=lh+1
            write(h(lh),'(2a)') 'found surface attribute... ',SSOTHER(i)
          endif
        endif
        if(ICT(i).eq.6.and.SSOTHER(i)(1:5).ne.'BASES')then

C HOT3000: BASESIMP.
          bndrysxc=.false.
          if(lh.lt.58)then
            lh=lh+1
            write(h(lh),'(2a)') 'Boundary looking for BASES in ',ZSDES
            lh=lh+1
            write(h(lh),'(2a)') 'connection... ',CXSTR(1:50)
            lh=lh+1
            write(h(lh),'(2a)') 'found surface attribute... ',SSOTHER(i)
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
        CALL EGOMIN(IFIL+1,LGEOM(IZ),IZ,0,0,iuout,IER)
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
        call usrmsg('Topology found to be inconsistent and this should',
     &  'be resolved before running a simulation.','W')
        CALL PHELPD('boundary checks',lh,'-',0,0,IER)
      endif

C Now look for unknown files.
      lh=0
      geofok=.true.
      do 46 i=1,ncomp
        if(LGEOM(i)(1:7).eq.'UNKNOWN'.or.LGEOM(i)(1:2).eq.'  ')then
          write(zn,'(a)') zname(i)
          geofok=.false.
          if(lh.lt.60)then
            lh=lh+1
            write(h(lh),'(3a)') 'Zone ',zn(1:lnblnk(zn)),
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
     &    'Some zones do not reference any geometry (file). The',
     &    'model not yet complete vis-a-vis thermal simulation.','W')
        CALL PHELPD('geometry checks',lh,'-',0,0,IER)
      endif
      lh=0
      oprfok=.true.
      do 43 i=1,ncomp
        if(LPROJ(i)(1:7).eq.'UNKNOWN'.or.LPROJ(i)(1:2).eq.'  ')then
          write(zn,'(a)') zname(i)
          oprfok=.false.
          if(lh.lt.60)then
            lh=lh+1
            write(h(lh),'(3a)') 'Zone ',zn(1:lnblnk(zn)),
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
     &  'Some zones lack operations or have older/unsorted files.',
     &  'The model not yet complete vis-a-vis thermal simulation.','W')
        CALL PHELPD('operations checks',lh,'-',0,0,IER)
      endif
      lh=0
      confok=.true.
      do 45 i=1,ncomp
        if(LTHRM(i)(1:7).eq.'UNKNOWN'.or.LTHRM(i)(1:2).eq.'  ')then
          write(zn,'(a)') zname(i)
          confok=.false.
          if(lh.lt.60)then
            lh=lh+1
            write(h(lh),'(3a)') 'Zone ',zn(1:lnblnk(zn)),
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
          endif
        endif
 45   continue
      if(.NOT.confok)then
        call usrmsg(
     &    'Some zones do not reference any construction (file). The',
     &    'model not yet complete vis-a-vis thermal simulation.','W')
        CALL PHELPD('constructions checks',lh,'-',0,0,IER)
      endif
      return
      end

C ********** imgdisp
C imgdisp displays images associated with start-up or at specific points
C and the image browser has not been invoked do this now. In the case
C of the GTK version use an in-built GTK function rather than an
C external tool.
      subroutine imgdisp(iforce,focus,ier)

      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

C A set of possible image formats which can be accepted within
C a model configuration file. These definitions are held in the
C so-called esprc file. To add additional image types and/or 3rd
C party display applications edit the ESP-r Install script which
C creates the esprc file.
C   imgtyp is the number of different image formats supported
C   fmttag (4 char) is a tag for each image formt (e.g. GIF XBMP)
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
C  noimg is the number of images associated with the model.
C  iton is a toggle iton=0 initial state where images have not been scanned
C    and iton=1 images have been displayed.
      common/IMAGF/imgfmt(10),imgfoc(10),limgfil(10),noimg,iton

      character imgfmt*4,imgfoc*4,limgfil*72,ilist*200,dolist*254
      character fmttag*4,fmtexe*20,tfile*72,focus*4
      character longtfile*144,longtfiledos*144
      character topic*248,head*136,act*1,h*72

      logical concat,show,unixok

C Check if Unix-based or DOS based.
      call isunix(unixok)

C Loop through the images and display those which match the 'focus'
C and can be displayed with a known utility. Iforce, if set to 1,
C forces display.
      if(noimg.eq.0)return
      if(imgtyp.eq.0)then
        call edisp(iuout,' ')
        call edisp(iuout,'No image display application available.')
        call edisp(iuout,'Consult tutorial tool for instructions on')
        call edisp(iuout,'how to set up your .esprc file.')
        return
      endif

#ifdef X11

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
#ifdef X11
                  call cmdfiledos(longtfile,longtfiledos,ier)
#endif
#ifdef GTK
                  call imgfiledos(longtfile,longtfiledos,ier)
#endif
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
 49       continue
          if(ix.gt.1)then
            dolist=' '
            if(fmtexe(imgv)(1:2).eq.'  ')then
              call usrmsg(
     &    'The image display tool has not be defined. See your',
     &    'administrator about updating the esprc definition.','W')
            else
              write(dolist,'(a,2x,a,a)')
     &          fmtexe(imgv)(1:lnblnk(fmtexe(imgv))),
     &          ilist(1:lnblnk(ilist)),' &'
              call runit(dolist,'graph')
              iton=1
            endif
          endif
 498    continue
      endif
#else
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
        h(8)='menu item. There can be up to 10 images associate with'
        h(9)='a model. '
        h(10)='Images of type GIF and XBM and ?? are supported.'
        CALL PHELPW(10,IHW,IER)
        call dupphelp(10)

        do 99 img=1,noimg
          if(imgfmt(img)(1:3).eq.'GIF'.or.
     &       imgfmt(img)(1:4).eq.'XBMP'.or.
     &       imgfmt(img)(1:3).eq.'TIF'.or.
     &       imgfmt(img)(1:3).eq.'JPG')then

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
              ixl=lnblnk(longtfile)
              if(focus(1:4).eq.'****')then
                write(topic,'(3a)') ' Image ',longtfile(1:ixl),
     &            ' is a general topic.'
              elseif(focus(1:4).eq.'FZON')then
                write(topic,'(3a)') ' Image ',longtfile(1:ixl),
     &            ' provides details of zone composition.'
              elseif(focus(1:4).eq.'FNET')then
                write(topic,'(3a)') ' Image ',longtfile(1:ixl),
     &            ' provides details of network composition.'
              elseif(focus(1:4).eq.'FCTL')then
                write(topic,'(3a)') ' Image ',longtfile(1:ixl),
     &            ' provides details of control composition.'
              elseif(focus(1:4).eq.'FDFS')then
                write(topic,'(3a)') ' Image ',longtfile(1:ixl),
     &            ' provides details of the CFD domain.'
              elseif(focus(1:4).eq.'FPER')then
                write(topic,'(3a)') ' Image ',longtfile(1:ixl),
     &            ' relates to model performance.'
              endif

C Provide some feedback, set the title of the popup and then call C function
C in lib/esp_draw.c
              call edisp(iuout,topic)
              write(head,'(2a)') 'Image: ',longtfile(1:ixl)
              act='-'
              call popupimage(head,topic,act,longtfile)
              iton=1
            endif
          endif
  99    continue
      endif

#endif
      return
      end

C CTLEXP ***********
C CTLEXP: Control feedback to export wireframe or text feedback area.
C Assign user-specified export file - checking if local or remote.
C tg is a character T or G specifying text or graphic 
C info being saved.
      subroutine ctlexp(xfile,ixopen,ixloc,ixunit,tg,msg,IER)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/rpath/path

      character*(*) msg,xfile
      character H*72,path*72,outs*124,tg*1,tfile*72
      character longtfile*144,longtfiledos*144
      logical concat,unixok

C Check if Unix-based or DOS based.
      call isunix(unixok)

C Each call is a toggle. If open then close text or graphics command
C file. For the graphics ask if file should be further converted.
      ixopen=ixopen+1
      if(ixopen.GT.1)ixopen=0
      if(ixopen.eq.0)then
        write(outs,'(a,a)') 'closing export file: ',xfile
        call usrmsg(outs,' ','-')
        if(tg.eq.'T')then
          CALL ERPFREE(ixunit,ISTAT)
        elseif(tg.eq.'G')then
          if(ixloc.eq.1)then
            longtfile=' '
            if(unixok)then
              call addpath(xfile,longtfile,concat)
            else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
              call addpath(xfile,longtfile,concat)
              call cmdfiledos(longtfile,longtfiledos,ier)
              longtfile=' '
              longtfile=longtfiledos
            endif
          else
            write(longtfile,'(a)') xfile(1:lnblnk(xfile))
          endif
          call wwcsetend
          call wwcclose(longtfile)
        endif
      elseif(ixopen.eq.1)then
        ixloc=0
        if(path.ne.'./'.and.path.ne.' ')then
           write(outs,'(A,A)') ' The current path is: ',path
           call edisp(iuout,outs)
         h(1)='If using path then the export file will be placed in'
         h(2)='the model folder. Otherwise nominate a folder and file'
         h(3)='name for the export file (i.e. /tmp/junk.exp or'
         h(4)='/home/ralph/reports/junk.exp) '
           write(outs,'(A,A)') ' The model is in folder ',path
           CALL EASKAB(outs,' place export file:','in the model folder',
     &      'user defined folder',ixloc,4)
        endif
        h(1)='This file is a text file which can be used in'
        h(2)='reports or for third party tools. '
        call easks(xfile,' export file name: ',' ',72,
     &    ' ','export file name',IER,2)

        write(outs,'(3A)')' opened ',xfile(1:lnblnk(xfile)),
     &                       ' for export.'
        call usrmsg(outs,' ','p')
        if(ixloc.eq.1)then
          if(tg.eq.'T')then
            call efopseq(ixunit,xfile,4,IER)
            if(ier.ne.0)return
          elseif(tg.eq.'G')then

C Writing remotely, add the path to the given file name before
C passing request to wwlib.c
            longtfile=' '
            if(unixok)then
              call addpath(xfile,longtfile,concat)
            else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
              call addpath(xfile,longtfile,concat)
              call cmdfiledos(longtfile,longtfiledos,ier)
              longtfile=' '
              longtfile=longtfiledos
            endif
            call edisp248(iuout,longtfile,100)
            call edisp248(iuout,xfile,100)
            call wwcopen(longtfile)
            call wwcsetstart
          endif
        else
          if(tg.eq.'T')then
            CALL ERPFREE(ixunit,ISTAT)
            call FPOPEN(ixunit,ISTAT,1,3,xfile)
            if(ISTAT.lt.0)return
          elseif(tg.eq.'G')then
            tfile=xfile
            call wwcopen(tfile)
            call wwcsetstart
          endif
        endif
        write(outs,'(1x,3A)') msg(:lnblnk(msg)),
     &    ' >> to ',xfile(1:lnblnk(xfile))
        call usrmsg(outs,' ','p')
      endif
      RETURN
      END

C ********************* CFGVER 
C CFGVER supports the creation of model variants.
C It copies a project and gives it a new name   
C Thereby facilitating creatiion of multiple versions of the same
C project that can be later modified.

      SUBROUTINE CFGVER
#include "building.h"

      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/user/browse
      common/C21/IFCFG,cfgroot,LCFGF
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

      dimension items(7),items2(33)
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,H,LCFGF
      CHARACTER ITEMS*36,ITEMS2*36,APP*4
      CHARACTER NBSTR*4,CFGROOT*24,OUTS*124
      LOGICAL BROWSE

      IF(BROWSE)THEN
        CALL EDISP(IUOUT,
     &  ' Version manager does not work in browse mode! ')
        RETURN
      ENDIF

C Help message for version manager menu
      H(1) = 'This menu supports the creation of design variants.'
      H(2) = 'It assumes that one version of a working model exists '
      H(3) = '(usually the base case or the most fundamental model'
      H(4) = 'from which all other permutations can be derived).'
      H(5) = ' '
      H(6) = 'Design variants can be created in the following ways'
      H(7) = ' '
      H(8) = 'Project Variant: asks specific questions about the '
      H(9) = ' changes the user wishes to make in the model and then'
      H(10)= ' it either makes the changes and saves them to new'
      H(11)= ' files or makes copies of relevant model files in '
      H(12) = ' preparation for further actions by the user. Thus'
      H(13)= ' the original model is preserved. '
      H(14)= ' '
      H(15)= ' For example - a variant with a different climate file'
      H(16)= ' might imply a change of latitude and longitude as'
      H(17)= ' well as ground temperatures and require recalculation'
      H(18)= ' of shading. The facility manages the process and a new'
      H(19)= ' configuration file is generated.'
      H(20)= ' '
      H(21)= ' For example - a variant with different constructions'
      H(22)= ' requires new geometry, construction and zone optical'
      H(23)= ' files. The facility makes new copies of these files'
      H(24)= ' so that any subsequent changes made by the user are'
      H(25)= ' saved to the new files rather than the original model'
      H(26)= ' files.'
      H(27)= ' '
      H(28)= ' You will be presented with a list of topics to select'
      H(29)= ' from. You can apply several changes within one variant'
      H(30)= ' model if you wish.'
      H(31)= ' '
      H(32)= 'The other place holders are currently not implemented'
      H(33)= ' '
      H(34)= 'A three character postfix is given by the user and this'
      H(35)= 'is appended to names of copied files'

C Ask for changes to be made
 200  INO=-3
      ITEMS(1) = 'a make a project variant'
      ITEMS(2) = 'b make nominal (under development)'
      ITEMS(3) = 'c other (under development)'
      ITEMS(4) = 'd other (under development)'
      ITEMS(5) = ' --------------------------'
      ITEMS(6) = '? help'
      ITEMS(7) = '- exit'
      MITEM=7
      CALL EMENU('Version manager',ITEMS,MITEM,INO)
      IF(INO.EQ.1)THEN
        APP='_xyz'
        CALL EASKS(APP,' Up to 4 letters to append to project name?',
     &     '(see help) ',4,'project','root name modifier',IER,35)
        K=0
        CALL EGETRM(APP,K,NBSTR,'W','cfg modifier ',IER)
        APP=NBSTR
        WRITE(OUTS,'(3A)')'New project name will be ',
     &        CFGROOT(1:LNBLNK(CFGROOT)),APP
        CALL EDISP(IUOUT,OUTS)

C Change string holding cfg description
        CALL EASKS(LSNAM,' Project description for this variant?',
     &     '(see help) ',72,'project','description modifier',IER,35)
        CALL EDISP(IUOUT,'New project description will be')
        CALL EDISP(IUOUT,LSNAM)

C Set up menu for different variants
        LRCR=0
 201    KNO=-3
        ITEMS2(1) = ' Site and location:'
        ITEMS2(2) = 'a global tasks and site exposure'
        ITEMS2(3) = 'b site exposure & ground reflectance'
        ITEMS2(4) = 'c climate,latitude and longitude'
        ITEMS2(5) = 'd pressure distribution'
        ITEMS2(6) = 'e ground temperature profiles'
        ITEMS2(7) = ' Form and fabric:'
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
        ITEMS2(20)= ' networks:'
        ITEMS2(21)= 'r fluid flow network'
        ITEMS2(22)= 's electrical network'
        ITEMS2(23)= 't plant components / plant network'
        ITEMS2(24)= 'u contaminants network'
        ITEMS2(25)= ' usage:'
        ITEMS2(26)= 'v zone operations'
        ITEMS2(27)= 'w controls'
        ITEMS2(28)= 'x casual gains control'
        ITEMS2(29)= 'y event profiles'
        ITEMS2(30)= 'z mould and microtoxins'
        ITEMS2(31)=' --------------------------------'
        ITEMS2(32)='? help'
        ITEMS2(33)='- exit'
        MITEMS2=33

C Help.
        h(1) ='Change options: '
        h(2) =' global tasks - work in progress use with caution.'
        h(3) =' '
        h(4) =' site exposure & ground reflectivity - modifies these '
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
        CALL EMENU('Changes to this model',ITEMS2,MITEMS2,KNO)

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

C mould and microtoxins
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
        CALL PHELPD('version intro',35,'-',0,0,IER)
        GOTO 200
      ELSEIF(INO.EQ.7)THEN
        RETURN
      ELSE
        GOTO 200
      ENDIF
      RETURN
      END

C ********************* VERMAN 
C VERMAN copies various esp-r files and names them uniquely
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
C renamed with APE appendage
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
C ILM = 26 means mould and microtoxins (currently not implemented)

C << Check existence of all files if non-existant create filenames with 
C << message to user that such a file does not exist

      SUBROUTINE VERMAN(APE,ILM,LRCR)
#include "building.h"
#include "esprdbfile.h"

C Path to model and command line file (if any). 
      common/OUTIN/IUOUT,IUIN
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      COMMON/C1/NCOMP,NCON
      common/C3F/LCNN
      common/C21/IFCFG,cfgroot,LCFGF
      common/FILEP/IFIL
      common/IPVF/lipvdatf
      common/pophelp/h(60)
      common/IPVA/ipvtitl,ipvvers,ipvsynop
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/cctlnm/ctldoc,lctlf
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM
      CHARACTER*72 SNFNAM
      common/C5R/SKYR,GRDR,BLDR
      COMMON/C14/UGRDTP(12,MGRDP),NGRDP
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/CONTM/CNTMFIL,CNTMDESC,NTSTEPC

      DIMENSION IVALS(MCOM),IVALSS(8),SALT(8)

C ivalsv keeps track of new viewfactor files which need analysys
C ivalsg keeps track of whether a geometry file has alread been copied.
      dimension ivalsv(MCOM),ivalsg(MCOM)

      character*72 sblres,sflres,splres,smstres,sipvres,selres
      character CNTMFIL*72,LCNN*72
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER*72 LIPVDATF,NNAME,H,LCFGF,msg1,MSG2,LCTLF,LCFD
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS,LAPROB
      CHARACTER*24 zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth
      CHARACTER*24 dbspth,cfgroot,oldcfgroot
      CHARACTER ipvtitl*40,ipvvers*40,ipvsynop*248,CTLDOC*248,OUTSTR*124
      CHARACTER APE*4,EXT*4,OUTS*124,doit*248,zname*12,zdesc*64,tmode*8
      CHARACTER SALT*31,CNTMDESC*124
      LOGICAL XST,OK,DOK,modsit,moddb,QUIET,docnn,silent,donegeo
      logical unixok

      M=ILM
      call isunix(unixok)

C Assume that a new connections file does not need to be created.
      docnn=.false.

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
            CALL EGOMIN(IUF,LGEOM(ICOMP),ICOMP,1,0,iuout,IER)
            CALL FNCNGR(LGEOM(ICOMP),APE,EXT,NNAME)
            LGEOM(ICOMP)=NNAME
            CALL EMKGEO(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)

C Remember this geometry file has been done so that a later selection
C of constructions does not re-do it.
            ivalsg(ic)=ivals(IC)

C If there is an existing shading file copy it to new name
C so that subsequent changes are applied to the new file.
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
              CALL USRMSG('copying shading file via:',doit,'-')
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
              CALL USRMSG('copying viewfactor file via:',doit,'-')
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
              CALL USRMSG('copying hc regime file via:',doit,'-')
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
              CALL USRMSG('copying casual gain ctl file via:',doit,'-')
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
              CALL USRMSG('copying obstruction file via:',doit,'-')
              CALL RUNIT(doit,'-')
              ZOBS(ICOMP)=NNAME
            endif
 17       CONTINUE
          docnn=.true.
          msg1='updating model for geometry changes...'
          call edisp(iuout,
     &      'New geometry files available for subsequent changes.')
        endif
        ishdq=-1
        ivfwq=-1
        do 43 iz=1,ncomp
          if(ISI(iz).eq.1)ishdq=1
          if(IVF(iz).eq.1)ivfwq=1
  43    continue
        if(ishdq.eq.1)then
          call usrmsg(
     &      'shading files found which will be dependant on future',
     &      'geometry changes. Shading files copied.','W')
          ishd=1
        endif
        if(ivfwq.eq.1)then
          call usrmsg(
     &      'viewfactor files found which will be dependant on future',
     &      'geometry changes. Viewfactor files copied.','W')
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
        WRITE(SALT(8),53)SKYR,GRDR,BLDR
  53    FORMAT('sky=',F4.2,' grnd=',F4.2,' bld=',F4.2)
        H(1)='The site exposure defines the relative view factors'
        H(2)='between a vertical surface and the sky, ground and'
        H(3)='surrounding buildings.  These data are used in the'
        H(4)='calculation of external longwave radiation exchanges.'
 563    IX=1
        CALL EPICKS(IX,IVALSS,' ',' Site exposure:',
     &         31,8,SALT,'site exposure',IER,4)
        IF(IX.EQ.0)GOTO 563
        IXPOS=IVALSS(1)
        IF(IXPOS.EQ.8)THEN
          H(1)='0 <= Sky view factor <= .99'
  290     CALL EASKR(SKYR,' ',' Sky view factor?',
     &         0.0,'W',0.99,'W',0.33,'sky view factor',IER,1)
          H(1)='0 <= Ground view factor <= .99'
          CALL EASKR(GRDR,' ',' Ground view factor?',
     &         0.0,'W',0.99,'W',0.33,'ground view factor',IER,1)
          H(1)='0 <= Surroundings view factor <= .99'
          CALL EASKR(BLDR,' ',' Surroundings view factor?',
     &         0.0,'W',0.99,'W',0.33,'surroundings view factor',IER,1)
          IF(ABS(SKYR+GRDR+BLDR-1.).GT..001)THEN
            CALL USRMSG(' Total view factor > 1.0 ',' ','W')
            GOTO 290
          ENDIF
        ENDIF
        H(1)='0 <= ground reflectance <= .99'
        CALL EASKR(GREF,' ',' Ground reflectance?',
     &       0.0,'W',0.99,'W',0.2,'ground reflectance',IER,1)
        MODSIT=.true.
        msg1='updating model for site exposure changes...'
        call edisp(iuout,'New configuration will hold new site data.')
      ELSEIF(M.EQ.4)THEN

C Shading and insolation choices. First find out if any of the
C zones curently include shading or obstruction files. If none do then
C aloow the user to select one or more zones.
        ishd=-1
        do 142 iz=1,ncomp
          if(ISI(iz).eq.1)ishd=1
          if(IOBS(iz).eq.1)ishd=1
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
     &              zname(icomp)(1:lnblnk(zname(icomp))),'.shd'
                else
                  WRITE(LSHAD(icomp),'(3A,A4)') 
     &              zonepth(1:lnblnk(zonepth)),'/',
     &              zname(icomp)(1:lnblnk(zname(icomp))),'.shd'
                endif
                EXT='.shd'
                CALL FNCNGR(LSHAD(ICOMP),APE,EXT,NNAME)
                LSHAD(ICOMP)=NNAME

C Create a small place-holder file that subsequent calculation
C procedure can overwrite. Mark for performing shading calculatons.
                IUF=IFIL+2
                CALL EFOPSEQ(IUF,LSHAD(ICOMP),3,IER)
                WRITE(IUF,'(2a)') 'Placeholder shading file for ',
     &            zname(icomp)(1:lnblnk(zname(icomp)))
                CALL ERPFREE(IUF,ios)
                ishad=1
                ishd=1
 151          continue
            endif
          endif
          if(iw.eq.2)then

C Create an obstructions file for each of the selected zones.
C First create initial name, then mangle it and write block.
            IUF=12
            if(inpic.gt.0)then
              DO 152 IC=1,inpic
                ICOMP=ivals(IC)
                if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                  WRITE(ZOBS(icomp),'(A,A4)')
     &              zname(icomp)(1:lnblnk(zname(icomp))),'.obs'
                else
                  WRITE(ZOBS(icomp),'(3A,A4)') 
     &              zonepth(1:lnblnk(zonepth)),'/',
     &              zname(icomp)(1:lnblnk(zname(icomp))),'.obs'
                endif
                EXT='.obs'
                CALL FNCNGR(ZOBS(ICOMP),APE,EXT,NNAME)
                ZOBS(ICOMP)=NNAME
                silent=.true.
                CALL ECROBS(ITRC,ITRU,IUF,ICOMP,silent,IER)
                IF(IOBS(ICOMP).eq.0)then
                  ZOBS(ICOMP)=' '
                endif
 152          continue
            endif
          endif
          if(iw.ne.3)then
            call edisp(iuout,
     &        'Shading files being copied. If site details have')
            call edisp(iuout,
     &        'changed shading/insolation will be recalculated.')
          endif
        elseif(ishd.eq.1)then

C There is shading and/or insulation check if user wants to dereference.
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
              call edisp(iuout,'Shading files being dereferenced.')
              msg1='updating model for shading changes...'
            endif
          elseif(iw.eq.2)then
            inpic=0
            call askmultizone(inpic,ivals,
     &        'Select zones do dereference obstructions?',
     &        'Zone obstructions dereference','b',ier)
            if(inpic.gt.0)then
              DO 154 IC=1,inpic
                ICOMP=ivals(IC)
                IOBS(ICOMP)=0
 154          continue
              call edisp(iuout,'Obstruction files being dereferenced.')
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
            call edisp(iuout,
     &        'Shading & obstruction files being dereferenced.')
            msg1='updating model for shading changes...'
          elseif(iw.eq.4)then

C Create an obstructions file for each of the selected zones
C which does not already have one. If there is an obstructions
C file (iobs(icomp) is non-zero) then read current obstructions
C update the file name and write back out to NNAME file.
            IUF=12
            inpic=0
            call askmultizone(inpic,ivals,
     &        'Select obstruction files to modify/add for this variant',
     &        'List of zones/obstructions','b',ier)
            if(inpic.gt.0)then
              DO 156 IC=1,inpic
                ICOMP=ivals(IC)
                IF(IOBS(ICOMP).eq.1)then
                  silent=.true.
                  EXT='.obs'
                  CALL EGOMST(IUF,ZOBS(ICOMP),0,ITRC,ITRU,IER)
                  CALL FNCNGR(ZOBS(ICOMP),APE,EXT,NNAME)
                  ZOBS(ICOMP)=NNAME
                  CALL MKGOMST(IUF,ZOBS(ICOMP),ICOMP,IER)
                else

C Create an obstruction file name, mangle the name and write
C out default block.
                  IOBS(ICOMP)=1
                  if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                    WRITE(ZOBS(icomp),'(A,A4)')
     &                zname(icomp)(1:lnblnk(zname(icomp))),'.obs'
                  else
                    WRITE(ZOBS(icomp),'(3A,A4)') 
     &                zonepth(1:lnblnk(zonepth)),'/',
     &                zname(icomp)(1:lnblnk(zname(icomp))),'.obs'
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
     &              zname(icomp)(1:lnblnk(zname(icomp))),'.shd'
                else
                  WRITE(LSHAD(icomp),'(3A,A4)') 
     &              zonepth(1:lnblnk(zonepth)),'/',
     &              zname(icomp)(1:lnblnk(zname(icomp))),'.shd'
                endif
                EXT='.shd'
                CALL FNCNGR(LSHAD(ICOMP),APE,EXT,NNAME)
                LSHAD(ICOMP)=NNAME

C Create a small place-holder file that subsequent calculation
C procedure can overwrite. Mark to update shading files.
                IUF=IFIL+2
                CALL EFOPSEQ(IUF,LSHAD(ICOMP),3,IER)
                WRITE(IUF,'(2a)') 'Placeholder shading file for ',
     &            zname(icomp)(1:lnblnk(zname(icomp)))
                CALL ERPFREE(IUF,ios)
                ishad=1
                ishd=1
 156          continue
              call edisp(iuout,
     &          'Obstruction files being created/copied.')
              call edisp(iuout,
     &          'Placeholder shading files being created.')
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
          h(1)='Area weighted calculations are the default treatment.'
          h(2)='Ray tracing is more accurate and time consuming.'
          CALL EASKABC('Viewfactor options:',' ',
     &      'area weighted (quick)','ray traced','continue',IW,2)

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
     &              zname(icomp)(1:lnblnk(zname(icomp))),'.vwf'
                else
                  WRITE(LVIEW(icomp),'(3A,A4)') 
     &              zonepth(1:lnblnk(zonepth)),'/',
     &              zname(icomp)(1:lnblnk(zname(icomp))),'.vwf'
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
     &    'View factor files being copied so that subsequent changes')
            call edisp(iuout,'in geomergy will be captured safely.')
          endif
        endif
      ELSEIF(M.EQ.6)THEN

C climate, latitiude and longitude. First check if the model includes
C any shading files.
        ishd=-1
        do 42 iz=1,ncomp
          if(ISI(iz).eq.1)ishd=1
  42    continue

        call edisp(IUOUT,'  ')
        CALL EDDBCLM(moddb,ape,ishad)

        IF(NGRDP.NE.0)THEN
          call edisp(IUOUT,'  ')
          call edisp(IUOUT,
     &     'User defined ground temperatures need to be updated')
          call edisp(IUOUT,
     &     'so supply monthly temperatures for the new site...')
          CALL EDDGTP(modsit) 
        ENDIF

C If there is an IPV file also update it.
        call FINDFIL(lipvdatf,XST)
        call isunix(unixok)
        IF(XST)THEN
          call edisp(IUOUT,'  ')
          call edisp(IUOUT,
     &     'The current IPV file should be updated to match climate.')
          call edisp(IUOUT,'Please at lest update the IPV titles...')
          CALL RIPVDAT(IFIL+1,LIPVDATF,IER)
          EXT='.ipv'
          CALL FNCNGR(LIPVDATF,APE,EXT,NNAME)
          if(unixok)then
            WRITE(doit,'(4A)')'cp ',LIPVDATF(1:LNBLNK(LIPVDATF)),' ',
     &        NNAME(1:LNBLNK(NNAME))
          else
            WRITE(doit,'(4A)')'copy /y ',
     &        LIPVDATF(1:LNBLNK(LIPVDATF)),' ',
     &        NNAME(1:LNBLNK(NNAME))
          endif
          CALL USRMSG('copying IPV file via:',doit,'-')
          CALL RUNIT(doit,'-')
          LIPVDATF=NNAME
          CALL EASKS(ipvtitl,' Enter project title for this variant?',
     &     ' ',40,'project','description modifier',IER,0)
          CALL EASKS(ipvvers,' Enter project version for this variant?'
     &     ,' ',40,'project','description modifier',IER,0)
          CALL EASKS248(ipvsynop,'Synopsis',' ',72,
     &    'Enter Project sysnopsis for this variant','synopsis',IER,5)
          CALL IPVDAT('-')
        endif
        msg1='updating model for climate changes...'
      ELSEIF(M.EQ.7)THEN

C pressure distributions
        H(1) = 'Pressure coefficients are used for calculating pressure'
        H(2) = 'distributions around a building. These are defined at '
        H(3) = 'two levels '
        H(4) = 'Building parameters include overall dimensions, roof'
        H(5) = 'detail, plan area etc. Invoking this facility will'
        H(6) = 'allow these values to be changed.'
        H(7) = 'Database involves detailed pressure coefficients that '
        H(8) = 'are used in pressure prediction when using an air'
        H(9) = 'flow network description. Invoking this facility '
        H(10)= 'copies the database, renames it and optionally allows'
        H(11)= 'you to edit it.'
        msg1='updating model for pressure db changes...'
        CALL EASKABC('Change to be made to ... (see help) ',
     &  ' ','building parameters','database','abort',IW,11)
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
     &          'Construction file does not exist...skipping.')
              goto 147
            ENDIF

C Check if geometry file has already been updated.
            donegeo=.false.
            DO 149 icg=1,inpic
              if(icomp.eq.ivalsg(icg))donegeo=.true.
 149        continue
            if(.NOT.donegeo)then
              EXT='.geo'
              CALL EGOMIN(IUNIT,LGEOM(ICOMP),ICOMP,IR,ITRC,ITRU,IER)
              CALL FNCNGR(LGEOM(ICOMP),APE,EXT,NNAME)
              LGEOM(ICOMP)=NNAME
              CALL EMKGEO(IUNIT,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
              WRITE(MSG1,'(2A)')
     &        'New geometry file written out for zone ',ZNAME(ICOMP)
              CALL EDISP(IUOUT,MSG1)
            endif
            ITRU=6
            CALL ECONST(LTHRM(ICOMP),IUNIT,ICOMP,ITRC,ITRU,IER)
            EXT='.con'
            CALL FNCNGR(LTHRM(ICOMP),APE,EXT,NNAME)
            LTHRM(ICOMP)=NNAME
            CALL EMKCON(LTHRM(ICOMP),IUNIT,ICOMP,ITRU,QUIET,IER)
            WRITE(MSG1,'(2A)')
     &      'New constructions file written out for zone ',ZNAME(ICOMP)
            CALL EDISP(IUOUT,MSG1)
            call FINDFIL(LTWIN(ICOMP),XST)
            IF(.NOT.XST)GOTO 147
            IFU=13
            CALL ERTWIN(ITRC,ITRU,IFU,LTWIN(ICOMP),ICOMP,IER)
            EXT='.tmc'
            CALL FNCNGR(LTWIN(ICOMP),APE,EXT,NNAME)
            LTWIN(ICOMP)=NNAME
            CALL MKTWIN(ITRU,IFU,ICOMP,QUIET,IER)
            WRITE(MSG1,'(2A)')
     &      'New  transparent constructions file written out for zone '
     &      ,ZNAME(ICOMP)
            CALL EDISP(IUOUT,MSG1)
 147      CONTINUE
          docnn=.true.
          msg1='updating model for construction changes...'       
          call edisp(iuout,
     &      'subsequent changes will be written in the new files')
        endif
      ELSEIF(M.EQ.10)THEN

C material database variant
        LFMULEN=LNBLNK(LFMUL)
        IF(LFMUL(LFMULEN:LFMULEN).EQ.'1')EXT='.db1'
        IF(LFMUL(LFMULEN:LFMULEN).EQ.'2')EXT='.db2'
        IF(LFMUL(LFMULEN:LFMULEN).EQ.'3')EXT='.db3'
        CALL FNCNGR(LFMUL,APE,EXT,NNAME)

C << probably should be limited to project databases >>
        call isunix(unixok)
        call fdroot(LFMUL,msg1,MSG2)
        WRITE(LFMUL,'(3a)')DBSPTH(1:LNBLNK(DBSPTH)),'/',MSG2
        if(unixok)then
          WRITE(DOIT,'(4A)')'cp ',LFMUL(1:LNBLNK(LFMUL)),' ',
     &       NNAME(1:LNBLNK(NNAME))
        else
          WRITE(DOIT,'(4A)')'copy /y ',LFMUL(1:LNBLNK(LFMUL)),' ',
     &       NNAME(1:LNBLNK(NNAME))
        endif
        LFMUL=NNAME
        CALL USRMSG('copying database via:',doit,'-')
        CALL RUNIT(DOIT,'-')
        msg1='updating model for materials changes...'        
        call edisp(iuout,
     &    'A copy of the materials database is being made.')
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
     &    'Configuration file will hold revised greound temperatures')
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
     &      'Subsequent changes will be applied to the new files.')
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
        EXT='.ipv'
        call FINDFIL(lipvdatf,XST)
        call isunix(unixok)
        IF(XST)THEN
          CALL RIPVDAT(IFIL+1,LIPVDATF,IER)
          CALL FNCNGR(LIPVDATF,APE,EXT,NNAME)
          if(unixok)then
            WRITE(doit,'(4A)')'cp ',LIPVDATF(1:LNBLNK(LIPVDATF)),' ',
     &        NNAME(1:LNBLNK(NNAME))
          else
            WRITE(doit,'(4A)')'copy /y ',
     &        LIPVDATF(1:LNBLNK(LIPVDATF)),' ',
     &        NNAME(1:LNBLNK(NNAME))
          endif
          CALL USRMSG('copying file via:',doit,'-')
          CALL RUNIT(doit,'-')
          LIPVDATF=NNAME
          CALL EASKS(ipvtitl,' Enter project title for this variant?',
     &     ' ',40,'project','description modifier',IER,0)
          CALL EASKS(ipvvers,' Enter project version for this variant?'
     &     ,' ',40,'project','description modifier',IER,0)
          CALL EASKS248(ipvsynop,'Synopsis',' ',72,
     &    'Enter Project sysnopsis for this variant','synopsis',IER,5)
          CALL IPVDAT('-')
        ELSE
          CALL EDISP(IUOUT,'ipv file does not exist creating one')        
          WRITE(LIPVDATF,'(2A)')cfgroot(1:lnblnk(cfgroot)),'.ipv'
          CALL FNCNGR(LIPVDATF,APE,EXT,NNAME)
          LIPVDATF=NNAME
          CALL IPVDAT('I')
          CALL EASKS(ipvtitl,' Enter project title for this variant?',
     &     ' ',40,'project','description modifier',IER,0)
          CALL EASKS(ipvvers,' Enter project version for this variant?'
     &     ,' ',40,'project','description modifier',IER,0)
          CALL EASKS248(ipvsynop,'Synopsis',' ',72,
     &    'Enter Project sysnopsis for this variant','synopsis',IER,5)
          CALL IPVDAT('-') 
        ENDIF
        CALL MKIPVDAT(IFIL+1,LIPVDATF)
        msg1='updating model for IPV changes...'
        call edisp(iuout,
     &    'IPV file being copied and edited text included.')
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
          CALL USRMSG('copying flow network via:',doit,'-')
          call runit(doit,tmode)
          CALL EFOPSEQ(IUM,LAPROB,1,IER)

C Read the file header and check for first-line tag. If 4 items
C then an older file so rewind the file and call emfread.
          CALL STRIPC(IUM,OUTSTR,99,ND,0,'1st line of file',IER)
          IF(ND.GE.4)THEN
            REWIND(IUM,ERR=999)
            CALL EMFREAD(IUM,0,IIER)
          elseif((ND.eq.1.or.ND.eq.2).and.
     &          OUTSTR(1:18).EQ.'*Graphical_network')then

C Found a graphic network file, advise user and exit.
            call usrmsg(
     &      'This file is a graphic network file rather than a flow',
     &      'network file.','W')
          endif
          CALL MFWRIT(IUM)
        ENDIF
        msg1='updating model for air flow network changes...'
        call edisp(iuout,'Mass flow file being copied. ')
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
          outs='There was a problem scanning contaminates file.'
          call edisp(iuout,outs)
        endif
        CALL FNCNGR(CNTMFIL,APE,EXT,NNAME)
        CNTMFIL=NNAME
        IUNIT=72
        CALL CTWRIT(iunit)
        msg1='updating model for contaminates network changes...'
        call edisp(iuout,'Contaminates network being copied.')
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
              CALL EDISP(IUOUT,'Operations file does not exist')
              RETURN
            ENDIF
            CALL EROPER(ITRC,ITRU,IUO,ICOMP,IER)
            CALL FNCNGR(LPROJ(ICOMP),APE,EXT,NNAME)
            LPROJ(ICOMP)=NNAME
            CALL PRJFMK(ITRC,ITRU,IUO,ICOMP,IER,ICOMP)
            WRITE(MSG1,'(2A)')'New operations file written out for ',
     &      ZNAME(ICOMP)
            CALL EDISP(IUOUT,MSG1)
 148      CONTINUE
          msg1='updating model for zone operation changes...'
          call edisp(iuout,
     &      'Subsequent changes will be applied to the new files.')
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
          CALL EDISP(IUOUT,'Control file does not exist')
          RETURN
        ENDIF
        CALL FNCNGR(LCTLF,APE,EXT,NNAME)
        LCTLF=NNAME
        CALL CTLWRT(IUNIT,IER)
        msg1='updating model for zone controls changes...'
        call edisp(iuout,'Control file being copied.')
      ELSEIF(M.EQ.24)THEN

C casual gains control
        h(1)='casual gains currently not supported' 
        CALL PHELPD('Not supported',1,'func not supported',0,0,IER)
        msg1='updating model for zone casual gain control changes...'
      ELSEIF(M.EQ.25)THEN

C event profiles
        h(1)='event profiles currently not supported' 
        CALL PHELPD('Not supported',1,'func not supported',0,0,IER)
        msg1='updating model for event profile changes...'
      ELSEIF(M.EQ.26)THEN

C mould and microtoxins
        h(1)='moulds and microtoxins currently not supported' 
        CALL PHELPD('Not supported',1,'func not supported',0,0,IER)
        msg1='updating model for mould and microtoxins changes...'
      ENDIF

C Reached this point after user has selected a topic whether or
C not any zones were selected.
      h(1)='You may wish to make a number of changes to the model'
      h(2)='variant. If so, select another option.'
      h(3)=' '
      h(4)='Remember all the affected files will have the variant'
      h(5)='extension characters in their file name.'
      dok=.false.
      CALL ASKOK('  ',' Make more changes (see help)?',OK,DOK,5)
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

C Check length of cfgroot string.
        IROOTLEN=LNBLNK(CFGROOT)
        IF(IROOTLEN.GT.(24-3))THEN
          IROOTLEN=21
        ENDIF
        write(cfgroot,'(2a)')cfgroot(1:irootlen),ape

C If a variant connections file also required set this up.
        if(docnn)then
          EXT='.cnn'
          CALL FNCNGR(LCNN,APE,EXT,NNAME)
          LCNN=NNAME
        endif
        call usrmsg(msg1,' ','-')
        CALL EMKCFG('-',IER)
        call usrmsg(msg1,'done.','P')
        WRITE (OUTS,'(2A)')'Writing new configuration file ',LCFGF
        CALL EDISP(IUOUT,OUTS)
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
      CHARACTER APP*4,EXT*4

      LCFIL=LNBLNK(ORIGNAM)
      LA=LNBLNK(APP)
      LX=LNBLNK(EXT)
      if(lcfil.gt.1)then
        IF(ORIGNAM(LCFIL-3:LCFIL).EQ.EXT(1:4))THEN
          IF(LCFIL.GT.(72-3))LCFIL=69
          WRITE(NEWNAM,'(3A)')ORIGNAM(1:LCFIL-4),APP(1:la),EXT(1:lx)
        ELSE
          IF(LCFIL.GT.(72-3))LCFIL=69
          WRITE(NEWNAM,'(2A)')ORIGNAM(1:LCFIL-4),APP(1:la)
        ENDIF
      else
        WRITE(NEWNAM,'(3A)') 'not_yet_defined',APP(1:la),EXT(1:lx)
      endif

      RETURN
      END

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
