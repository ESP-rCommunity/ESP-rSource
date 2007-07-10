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

C ******************** bps
C The main program initialises the user terminal and the
C user-specified climate database and displays the main menu.

C Common block variables are:

C ier1    - input error index which ensures that those menu picks
C           which are dependent on input data cannot be selected
C           if any error was detected during the input sequence

C iuout & - write and read unit numbers set via the following data
C iuin      statement

C icout   - write unit for rule scripts.

C ifil    - basic file unit number. Actual file unit numbers are:
C           ifil+0  climate file
C           ifil+1  any temporarily opened file
C           ifil+2  building results library
C           ifil+3  plant results library
C           ifil+4  mass flow results file
C           ifil+5  primitive construction db.
C           ifil+6  temnporal file (ascii)
C           ifil+7  temporal scratch file
C           ifil+8  
C           ifil+9  plant control supplementary file
C           ifil+10 3D control volumes file
C           ifil+11 3D connections file
C           ifil+12 3D nodes distribution file
C           ifil+13 3D nodes temperature and energy file
C           ifil+14 3D nodes total longwave file
C           ifil+15 3D nodes longwave file
C           ifil+16 3D temporarily file
C           ifil+17 moisture flow results file
C           ifil+18 CFD results file
C           ifil+19 CFD monitoring file
C           ifil+20 CFD temporarily file for use during simulation
C           ifil+21 Power flow results library
C           ifil+22 any supplementery temporarily opened file
C           ifil+23 first nochan (occupied utility file channel)
C                   NB nochan+x is used in several places

C           ifil+31 Read in vertical solar data
C           ifil+32 (fort.42) Vertical solar output channel
C           ifil+40 snow depth ASCII file
C           ifil+41 snow depth binary file
C           ifil+45 (fort.55) PV output channel
C           ifil+50 (fort.60) BEMS output dump
C           ifil+60 Structured mesh temperarture file.
C           ifil+70 H3KOutput File
C << file unit clash, consider ifil + 62
C           ifil+72 Contaminant description file
C
C           ifil+71 CETC .fcts1 output file  )   These files are written
C           ifil+72 CETC .fcts2 output file  )__ sequentially, and could all
C           ifil+73 CETC .fcts3 output file  )   use the same channel to
C           ifil+74 CETC .fcts4 output file  )   free up some file numbers
C
C           ifil+76 Runtime boundry condition definition file (BCD.tmp)
C           ifil+77 Temporary binary file used when converting
C                   hourly profiles 'Hrly_Load.tmp'
C           ifil+80 CETC month status IO channel
C           ifil+81 Runtime lookup table data file 
C           ifil+82 File containing wind turbine power curve data

C MMOD   -  index returned from the 'pages' routine
C           and determines the device type attached to current job.

C For details on bps's theoretical basis see publications listed
C at the following Web site:

C http://www.strath.ac.uk/Departments/ESRU

C AIM-2.
C#include "../cetc/aim2_module.F"
C HVAC.
C#include "../cetc/hvac_module.F"
C Results data storage
C#include "../cetc/h3kstore_module.F"
C Mechanical ventilation.
C#include "../cetc/Vnt_data.F"
C GSHP.
C#include "../cetc/gshp_module.F"
C AIMS
C#include "../cetc/AIMS_module.F"
C GCEP.
C#include "../cetc/GCEP_module.F"

      program bps

C======hot3000========================start
C
C  This bps main is here for convience. It 
C  allows the same version of this file to
C  be used to compile the following forms
C  of bps: a UNIX executable, a Windows
C  EXE and a Windows DLL and bpsh3k.
C 
C==========================================
      common/dllflg/dll
      character dllsubr*12,dllmesg*124
      logical dll

      dll = .false.
      call SubBps

      end

C======SubBps=========================
C SubBps can be used where bps needs to be called as
C a subroutine rather than as an application.
      subroutine SubBps

#include "building.h"
#include "plant.h"
#include "uncertainty.h"
#include "net_flow.h"
#include "MultiYear_simulations.h"
#include "cfd.h"
#include "esprdbfile.h"

C For dll mode dllsubr is the subroutine name where an error occurs
C and dllmesg tells the details of the error.
      common/dllerr/dllsubr,dllmesg
      common/dllflg/dll
      common/BPSDLL/iLogFile, cBpsVersion, cPrjPath, cPrjName,
     &            cCfgPath, cCfgFile, cLogFile, lBpsDebug

      logical lBpsDebug
      character*72 cBpsVersion, cPrjPath, cPrjName, cCfgPath,
     &           cCfgFile, cLogFile

C IEEE callback (Solaris)
C      external SIGFPE_bps      
C      integer SIGFPE_bps      

      common/outin/iuout,iuin
      common/shout/icout
      common/filep/ifil
      common/tc/itc,icnt
      common/trc/itrc
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      common/appw/iappw,iappx,iappy

      common/er1/ier1
      common/wrnoff/iwmoff
      common/help/ihelpm
      common/avrage/idaver

C Record lengths for zone (nzrl), plant (nprl), electrical(nerl) results libraries.
      common/reclen/nzrl,nprl,nerl
      common/recver/izver,ipver,iever
      common/hcthry/ihct,icorexhct
      COMMON/DNORGH/IRTYPE
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      common/rdcgen/irdact
      common/rdcmfs/irdcm,sFLWTIM,sDRYB,sWDIR,sWSPD,sITER,sIOK,
     &              sTNOD(MNOD)

      COMMON/VTHP18/LNLTHP
      COMMON/VTHP31/INTHPS,INTHPZ(MCOM)
      COMMON/GR1D05/LGRD1D
      COMMON/GR1D06/IGR1D
      COMMON/GR3D100/BLDG3D,ZONE3D(MCOM)
      COMMON/GR3D109/L3DNDD(MCOM),L3DTLW(MCOM),L3DLWV(MCOM)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GRND100/GRND3D
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      COMMON/MOIST09/IMSTR
      COMMON/GRSD100/IndxSt
      COMMON/GRSD122/InSvST,ISTemp

C Path to model and command line file (if any).
      common/rpath/path
      common/uhome/upath
      common/udot/esprc
      common/rcmd/LCMDFL
      common/CFGV/icfgv
      common/user/browse

C CFD output channels.
      COMMON/ICFCHN/ICFMON,ICFTMP,ICFLIB
      common/CFDres/photon,flwvis

C CFD results library.
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)

C CFD visualisation.
      common/flvpar/Lflpar(MCOM)
      COMMON/ACT/ACTCNN(MCNN)

C CFD small openings.
      common/SSOinit/areaSSO

C Where default db list are kept.
      common/defdb/dfdblbl,defdbfl

C Project journal on/off, unit number, cmd, file name.
      common/journopt/journio,iuj,journcmd,jfile

      COMMON/C1/NCOMP,NCON
      COMMON/CHANNL/NOCHAN
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFT/LTDF,LTDFA

C Simulator parameters.
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      common/SPFLN/spsname

C BEMS interaction time for results dump. 
      COMMON/BEMSTIM/bemstime,ibems,bresfil

C Uncertainty.
      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)

C Illuminance database.
      common/ILDB/NDBENT,DBILLA(200,5),DBILLS(200,MLCOM,MDF)

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Active domain common (expand to other domains?).
      common/ACTDOM/CFDOK
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG
      
C Timeout information
      common/TIMEOUT/sTimeout,iTimeoutFlag

C Material data array.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)
      
C Name of current application
      common/APPNAME/cAppName
      
      character cAppName*12
      character sTimeout*72
      integer iTimeoutFlag      

      LOGICAL OK,dok,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,OK3D
      LOGICAL IGR1D,BLDG3D,ZONE3D,GRND3D,INPOK,XST,CFDOK,browse
      LOGICAL INTHPS,INTHPZ,MSTROK,MSTRZN
      LOGICAL spfileok,perok,tstepok,saveok,autook,exitok,startupok
      logical unixok,there,photon,flwvis,gopher,dll,ACTCNN,areaSSO

      character LUALF*72,LCNG*15,LLOC*15,cfgroot*24,LCFGF*72
      character*29 item(15)
      character h*72,LTMP*72,OUTS*124,spsname*8
      character LCMDFL*144,inf*144,sps*8,aut*12,fs*1,actf*72
      character browsing*4
      CHARACTER*72 L3DNDD,L3DTLW,L3DLWV,LNLTHP,LGRD1D
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD,Lflpar
      character path*72,upath*72,esprc*72,bresfil*72
      character dfdblbl*20,defdbfl*72,RFILE*72,PFILE*72,MSTRFILE*72
      character LAPROB*72,LAFRES*72
      character journcmd*20,jfile*72,uname*24,cjfile*72,OUTSTR*124
      character dllsubr*12,dllmesg*124,LTDF*72,LTDFA*72
      character t16*16,outs248*248
      
C Flags for multi-year simulation menus
      logical bOptOut, bDone, bMY_clm_err, bExists, bCreate
      logical bautook

C Variables used to configure h3kreports      
      logical bH3K_Rep_Enabled
      logical rep_xmlstatus

C Active domain.  Assume not active, check if running in dll mode
C and clear dll subroutine name and error details.
      CFDOK=.false.

C mark in log file, start of simulation
      if( dll ) then
#ifdef F90
        open(UNIT=iLogFile, FILE=cLogFile,position='APPEND' )
#else
        OPEN( UNIT=iLogFile, FILE=cLogFile, ACCESS='APPEND' )     
#endif
        write( iLogFile, * ) "subroutine: bps() - dll run"
        write( iLogFile, * ) "checking input"
        close( iLogFile )
#ifdef F90
        open(UNIT=iLogFile, FILE=cLogFile,position='APPEND' )
#else
        OPEN(unit=iLogFile,FILE=cLogFile, ACCESS = 'APPEND')
#endif
      endif

      dllsubr=' '
      dllmesg=' '
      do 1 I=1,MCNN
        ACTCNN(I)=.TRUE.
 1    continue

C Assume that the configuration, mlc db, primitive db, control file
C have not been read in.
      CFGOK=.FALSE.
      MLDBOK=.FALSE.
      MATDBOK=.FALSE.
      CTLOK=.FALSE.
      spfileok=.false.
      LUALF='UNKNOWN'
      RFILE='libb'
      PFILE='libp'
      LAFRES='libf'
      MSTRFILE='libmstr'
      do 5 I=1,MCOM
        Lflpar(I)='UNKNOWN'
 5    continue

C CFD results output. `photon' indicates whether PHOTON phi files will
C be created; `flwvis' indicates whether ESP-r's in-built flow visualization
C facility will be used to generated images of the flow. `gopher' is a flag
C used to generate images for the preliminary flow field when the adaptive
C conflation controller is active.
      photon=.FALSE.
      flwvis=.FALSE.
      gopher = .false.

C CFD results library: IRECPC=current record; ICFDSV=results version
      IRECPC=0
      ICFDSV=2

C Flag indicating that CFD `small opening' areas have not yet been calculated.
      areaSSO = .FALSE.

C Illuminance values held in database.
      NDBENT=0

C Font sizes for graphic environment.
      IFS=1
      ITFS=1
      IMFS=1

C Some initial values, which may be changed later:
      call ezero
      call curmodule('bps ')
      iuout = 6
      iuin = 5
      icout=0
      itc=0
      itrc=0
      icnt=1
      itcf=0
      itu=iuout
      iwmoff=1
      ihelpm=0
      NSIGFIG=3
      matver=0.0   ! initial assumption of binary materials database

      cAppName = 'bps'

C Set parameters for DLL run.
C all console write is rerouted to the log file
      if( dll ) then
        itu = iLogFile
        iuout = iLogFile
        icout = iLogFile
#ifdef F90
        open(UNIT=iLogFile, FILE=cLogFile,position='APPEND' )
#else
        OPEN(UNIT=iLogFile, FILE=cLogFile, ACCESS='APPEND' )     
#endif
        call SetDllParms(MODL,iappw,iappx,iappy,inf,sps,aut,actf)
        browsing='no'
      else
        call parsim(MODL,iappw,iappx,iappy,inf,browsing,sps,aut,actf)
      endif

C Detect if the simulator was started up from the project manager
C in browsing mode.
      if(browsing(1:2).eq.'no'.or.browsing(1:2).eq.'NO'.or.
     &   browsing(1:2).eq.'No')then
        browse=.false.
      elseif(browsing(1:3).eq.'yes'.or.browsing(1:3).eq.'YES'.or.
     &   browsing(1:3).eq.'Yes')then 
        browse=.true.
      endif

C create new child thread to timeout simulation if required.
C << Note this is temporarily commented out and should be
C << re-established for CETC use.
C      if ( itimeoutflag .eq. 1)then
C        call timeout_espr(sTimeout)
C      endif

C Get the bems time from the actf parameter
      if(actf(1:4).eq.'bems') then
        bresfil=actf(4:72)
        ibems=1
      endif

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

C Initialise the unit numbers for the various files used. See comments 
C at begining for details. These must not be changed elsewhere. 
      ifil=10
      IUNIT=IFIL+1
      ifcfg=IFIL+1
      IUTDF=IFIL+6
      IUTDFA=IFIL+7
      ICVS=IFIL+10
      ICNC=IFIL+11
      INDD=IFIL+12
      ITAQ=IFIL+13
      ITLW=IFIL+14
      ILWV=IFIL+15
      ITF3=IFIL+16
      IMSTR=IFIL+17
      ICFLIB=IFIL+18
      ICFMON=IFIL+19
      ICFTMP=IFIL+20
      nochan=ifil+23
      ISTemp=IFIL+60

C Use process ID as seed for random number generator (for use in 
C uncertainty analysis routines)
      call esppid(ipid)
      x=RSTART(ipid)

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

C Initialise output device: -10 forces user selection.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=560
        iappx=75
        iappy=170
      else
        if(iappx.le.0)iappx=75
        if(iappy.le.0)iappy=170
        if(iappw.le.200)then
          iappwi=int(560*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(32*iappw*0.01),16)
        LIMTTY=13
        LIMIT =13
      else
        menuchw = 32
        LIMTTY=13
        LIMIT =13
      endif
      IF(MMOD.EQ.8)THEN

C Set initial font sizes (IMFS is for graphs, IFS is for dialog & text feedback).
        IMFS=0
        IFS=0
        ITFS=0
        call userfonts(IFS,ITFS,IMFS)
      ELSE
        LIMTTY=20
        LIMIT =20
      ENDIF
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &'ESP-r Integrated Simulator: enquiries to esru@strath.ac.uk')
      IF(MMOD.EQ.-6)then
        ICOUT=0
      else
        ICOUT=IUOUT
      endif

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN
#ifdef X11
        CALL win3d(menuchw,16,16,4,3,igl,igr,igt,igb,igw,igwh)
        call opencpw
        call opentutorial
        call opensetup
        call updcapt(1)
#else
        CALL win3d(menuchw,16,16,5,4,igl,igr,igt,igb,igw,igwh)
#endif
        call setzscale()
        mdispl=0
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      ENDIF
      CALL EDISP(IUOUT,' ')

C Write version.
      CALL ESPrVersion("summary")
      CALL EDISP(IUOUT,' ')

C Find the user's home folder then get users custom settings.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IFIL+1,0,IIER)

C If there is a journal active set up to add to the journal. First
C get the root journal file (setup by prj) from the users home folder.
C All subsequent tstamp messages will be appended to root journal.
      iuj=IFIL+1
      if(journio.eq.1)then
        uname=' '
        call usrname(uname)
        cjfile=' '
        write(cjfile,'(a,a1,a,a,a)')upath(1:lnblnk(upath)),fs,'.',
     &    uname(1:lnblnk(uname)),'cur_j'
        there=.false.
        INQUIRE(FILE=cjfile,EXIST=there)
        if(there)then
          close(iuj)
          open(iuj,file=cjfile,status='UNKNOWN',err=901)
          CALL STRIPC(iuj,OUTSTR,0,ND,1,'header',IER)
          if(OUTSTR(1:15).eq.'Current_Journal')then
            k=16
            CALL EGETRM(OUTSTR,K,jfile,'W','root journ',IER)
          endif
          close(iuj)
        endif

        write(outs,'(a,a)')'Journal (simulator) for:',
     &    uname(1:lnblnk(uname))
        call tstamp('>',outs)
      endif

C Scan the defaults file for default configuration.
C Make temporary use of file unit IFIL+1.  Note: escdef must come
C after scan of .esprc file.
C      dfdblbl='NT default'
 902  call escdef(dfdblbl,defdbfl,IFIL+1,IER)

C Take command line file name as initial configuration file.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
        write(outs248,'(a,a)')' the input file is: ',LCMDFL
        call edisp(iuout,outs248)
        call tstamp('>',outs248)
      else
        LCMDFL='UNKNOWN'
      endif

C Inital climate assumption.
      LCLIM=DCLIM
      ICLIM=IFIL

C Day & Time format preference.
      IFDAY=2
      IFTIME=0

C Initialize once per hour storage of results counter.
      irdact=0
      irdcm=1

C Record length of binary results file.
      nzrl = MZRL
      izver = 3

C Record length of plant results file.
C Parameter 'nprecl' as defined in 'plant.h'.
      nprl = nprecl

C Unit for primitives constr db in case control -99 is invoked.
      ifmat=ifil+5

C Initialise results averaging flag: 0 means
C results will be averaged.
      idaver=0

C Default surface convective heat transfer coefficient model.
C ihct (for internal convenction) icorexhct (for external).
      ihct=1
      icorexhct=1

C No input achieved as yet.
      INPOK=.FALSE.

C So far no input errors.
      ier1=0

C Default setting of IREP, reporting and range checking on.
      IREP=0

C If an input file has been specified then load it and then
C return to the main menu.
      XST=.false.
      if(LCMDFL(1:2).ne.'  '.and.LCMDFL(1:4).ne.'UNKN')then
        INQUIRE (FILE=LCMDFL,EXIST=XST)
        IF(XST)THEN

C Check for a simulator parameters - if requested then use the 
C info in after reading the .cfg file.
          if(sps(1:2).ne.'  '.and.sps(1:4).ne.'UNKN')then
            write(spsname,'(a8)')sps(1:8)
            if(aut(1:6).eq.'silent')then
              autook=.true.
              call edisp(iuout,'Silent-running mode.')
              call tstamp('>','SIM: Silent-running mode')
            else
              autook=.false.
            endif
          else
            spsname='  '
          endif
          call tstamp('>','SIM: enter input checks')
          CALL MZINPT(IREP,ier)

C If running dll note the failure mode.
          if(ier.eq.2.and.dll)then
#ifdef F90
            open(UNIT=iLogFile, FILE=cLogFile,position='APPEND' )
#else
            OPEN( UNIT=iLogFile, FILE=cLogFile, ACCESS='APPEND' )     
#endif
            write(iLogFile,*) 'bps failed in ',dllsubr
            write(iLogFile,*) 'details: ',dllmesg
            return
          endif
          call tstamp('>','SIM: exit input checks')

C Increment input flag if no input data errors detected.
          IF(IER.EQ.0)THEN
            INPOK=.TRUE.
          ELSE
            if ( autook ) then
              call edisp(iuout, ' ')
              call edisp(iuout,
     &              'Fatal error: Model was not scanned correctly')
              stop 
            endif 
            if(dll)then
              dllsubr='bps MAIN'
              dllmesg='IER1 flag set while scanning the model.'
              ier=2
              return
            endif
            h(1)='A warning was flagged while scanning the model. You'
            h(2)='are advised to check and then re-read the model. If'
            h(3)='the warning is minor proceed with caution.'
            call easkabc('While scanning model a problem was found.',
     &        'Options: ','read again','use it','abort',IW,3)
            if(iw.eq.1)then
              INPOK=.FALSE.
            elseif(iw.eq.2)then
              call tstamp('>','SIM: use model with init scan warning')
              INPOK=.TRUE.
            elseif(iw.eq.3)then
              call tstamp('>','SIM: abort after error scan problem')
              call epagend
              if( dll ) then
                dllsubr='bps MAIN'
                dllmesg='IER1 flag set while scanning the model.'
                ier=2
                return
              endif
              STOP
            endif
          endif
        else

C No confiburation file passed, if in dll mode return otherwise continue.
          if( dll ) then
            return
          else
            continue
          endif
        endif

C Initialize the thermal and gridding variables.
        IF(INPOK.AND.INTHPS)THEN
          CALL THZERO
          CALL THERMR(LNLTHP,IUNIT,IUOUT,ITRC,IER)
          IF(IER.NE.0)INPOK=.FALSE.
        ENDIF
        IF(INPOK.AND.IGR1D)CALL READ1D(LGRD1D,IUNIT,IUOUT,ITRC,IER)
        IF(IER.NE.0)INPOK=.FALSE.

C Check legallity of moisture node distribution with respect to heat
C node distribution.
        IF(INPOK.AND.MSTROK)CALL CHKMST(IER)
        IF(IER.NE.0)INPOK=.FALSE.
        IF(INPOK.AND.IGR1D)THEN
          DO 10 ICOMP=1,NCOMP
            CALL MZCOE1(ICOMP)
   10     CONTINUE
        ENDIF

C Initiate the moisture matrix solution parameters.
        IF(INPOK)CALL INITMS

C Initialize the structured gridding variables.
        IF(IndxSt.GT.0)THEN
          CALL READST(IndxSt,ITRC,IER)
          IF(IER.EQ.0)CALL INITST
          IF(IER.NE.0)INPOK=.FALSE.
        ENDIF
      ENDIF

C Set up initial menu.
   20 ITEM(1)   ='a Input model description    '
      ITEM(2)   ='b Assign climate file        '
      ITEM(3)   ='  _________________________  '
      ITEM(4)   ='c Initiate simulation        '
      ITEM(5)   ='  _________________________  '
      ITEM(6)   ='t Invoke trace facilities    '
      if ( bMY_sim_enabled ) then
         ITEM(7)   ='y Multi-year sim >> ON    '
      else
         ITEM(7)   ='y Multi-year sim >> OFF   '
      endif 
      IF(IWMOFF.EQ.1)THEN
        ITEM(8) ='w Warnings >> OFF            '
      ELSEIF(IWMOFF.EQ.0)THEN
        ITEM(8) ='w Warnings >> ON             '
      ENDIF
      IF(ITRC.EQ.0)THEN
        ITEM(9) ='r Reporting >> silent        '
      ELSEIF(ITRC.EQ.1)THEN
        ITEM(9) ='r Reporting >> summary       '
      ELSEIF(ITRC.EQ.2)THEN
        ITEM(9) ='r Reporting >> detailed      '
      ENDIF
      ITEM(10)   ='  _________________________  '
      if ( rep_xmlstatus() ) then 
         ITEM(11)  ='s Configure H3K Reports      '
      else
         ITEM(11)  ='s H3K Reports unavailable    ' 
      endif 
      ITEM(12)  ='  _________________________  '
      ITEM(13)  ='? Help                       '
      ITEM(14)  ='- Quit                       '
      nitms=14

C And menu is to be displayed.
      ino=-3
   30 CALL USRMSG(' ',' ','-')

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('bps ',1)

C Do not display the menu if in autook mode as defined in the 
C simulator parameter file.
C NOTE: if the menu is changed then the values assigned to ino 
C       will have to be updated.
      if(autook) then
        if(exitok) then
          ino=NITMS
        else
          ino=4
        endif
      else
        CALL EMENU('ESP-r integrated simulator',ITEM,NITMS,INO)
      endif

C Enter configuration file       
      IF(INO.EQ.1)THEN
        call tstamp('>','SIM: enter read model')
        CALL MZINPT(IREP,ier)
        if(ier.eq.2.and.dll)then
#ifdef F90
          open(UNIT=iLogFile, FILE=cLogFile,position='APPEND' )
#else
          OPEN(UNIT=iLogFile, FILE=cLogFile, ACCESS='APPEND' )     
#endif
          write(iLogFile,*) 'bps failed in ',dllsubr
          write(iLogFile,*) 'details: ',dllmesg
          ier=2
          return
        endif

C Now increment input flag - but only if no input data errors
C have been detected.
        IF(IER1.EQ.0)THEN
          INPOK=.TRUE.
        ELSE
          INPOK=.FALSE.
        ENDIF

C initialize the thermal and gridding variables variables.
        IF(INPOK.AND.INTHPS)THEN
          CALL THZERO
          CALL THERMR(LNLTHP,IUNIT,IUOUT,ITRC,IER)
          IF(IER.NE.0)INPOK=.FALSE.
        ENDIF
        IF(INPOK.AND.IGR1D)CALL READ1D(LGRD1D,IUNIT,IUOUT,ITRC,IER)
        IF(IER.NE.0)INPOK=.FALSE.

C Check legallity of moisture node distribution with respect to heat
C node distribution.
        IF(INPOK.AND.MSTROK)CALL CHKMST(IER)
        IF(IER.NE.0)INPOK=.FALSE.
        IF(INPOK.AND.IGR1D)THEN
          DO 40 ICOMP=1,NCOMP
            CALL MZCOE1(ICOMP)
   40     CONTINUE
        ENDIF
        IF(INPOK)CALL INITMS

C Initialize the structured gridding variables.
        IF(IndxSt.GT.0)THEN
          CALL READST(IndxSt,ITRC,IER)
          IF(IER.EQ.0)CALL INITST
          IF(IER.NE.0)INPOK=.FALSE.
        ENDIF
        call tstamp('>','SIM: exit read model')
      ELSEIF(INO.EQ.2 .and. .not. bMY_sim_enabled )THEN

C Single-year climate file definition
C Set climate data file (file must exist - unit
C number = ifil). If found, attempt to open and test that it is
C a climate file by reading records 366 and 367.
        H(1)=' A climate file defines the model boundary'
        H(2)=' condition. Usually a number of files will'
        H(3)=' exist and these can be analysed using ESRU: clm.'
        H(4)=' '
        H(5)=' Any climate file can be entered at this point. '
        H(6)=' For example, on computers running UNIX, the file '
        H(7)=' [~esru/esp-r/default] holds the name of a '
        H(8)=' default climate file. This might define a TRY or '
        H(9)=' example year. '
   50   iclim=ifil
        call erpfree(iclim,istat)
        LTMP=LCLIM
        CALL EASKS(LTMP,' Climate file name ?',
     &    ' ',72,DCLIM,'climate file name',IER,9)
        IF(LTMP.EQ.' ')GOTO 50
        LCLIM=LTMP
        call tstamp('>','SIM: change climate')
        call tstamp('>',LCLIM)
        call EFOPRAN(ICLIM,LCLIM,144,0,IER)
        IF(ier.eq.0)THEN
          IREC=366
          READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=103)ICYEAR,IRT
          IREC=367
          READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=103)CLMLOC
          WRITE(OUTS,1000)CLMLOC,ICYEAR
 1000     FORMAT(' Climate data: ',A30,':',I5)
          CALL EDISP(IUOUT,' ')
          CALL EDISP(IUOUT,OUTS)
          IRTYPE=0
          IF(IRT.EQ.123)IRTYPE=1
          if(icfgv.lt.3)then
            h(1)='Older configuration files do not have a year record.'
            h(2)='If you want the year to be taken from the climate'
            h(3)='database use the default.'
            IYEAR=ICYEAR
            CALL EASKI(IYEAR,
     &        'Year for the simulation? (The configuration does not',
     &        'specify one and the climate db year is shown) ',
     &          1900,'W',2000,'@',ICYEAR,'year from clim',IER,3)
          endif
C Set flag: climate data ok
          bSY_climate_defined = .true.

        ELSE
          bSY_climate_defined = .false.

          dok = .false.
          h(1)='At simulation time there should not be errors in'
          h(2)='the associated climate file. Check you model to see'
          h(3)='if it has been corrupted. '
          CALL ASKOK(' Problem encountered reading climate db,,',
     &               ' try again? (no exits simulator)',OK,dok,3)
          IF(OK)THEN
            goto 20
          ELSE
            call tstamp('>','SIM: abort after error scanning climate')
            call epagend
            CALL ERPFREE(ICLIM,ISTAT)
            if( dll ) return
C There's no reason to stop the simulator here - 
C Let's just return to the menu and let the user decide what to do!
          ENDIF
        ENDIF

C Close & free file (it will be opened again before the start
C of the simulation         
        CALL EPAGEND
        CALL ERPFREE(ICLIM,ISTAT)

      ELSEIF(INO.EQ.2 .and. bMY_sim_enabled ) then

C Get multi-year climate file name - 'Fail': Do not allow user
C to create new file if not found. 
        t16='Fail'
        call spec_MY_clm_db (t16,bExists,bCreate)

        if ( bExists ) then

C Parse file and check for errors. If no errors set status flag.        
          call parse_MY_clm_db (ICLIM, bautook, bMY_clm_err)
          if ( .not. bMY_clm_err ) then
             t16='fix'
             call check_MY_clm_db ( bautook,t16, bMY_clm_err )
          endif
          if ( .not. bMY_clm_err ) then
            bMY_climates_defined = .true.
          else
            bMY_climates_defined = .false.
          endif
        endif 

C Initiate simulation.
      ELSEIF(INO.EQ.4)THEN

C Check that climate & building configurations files are ok
        IF (.NOT. INPOK ) THEN
          CALL USRMSG(' ','Configuration file not yet entered.','W')
        ELSEIF ( .NOT. bSY_climate_defined .and.
     &           .NOT. bMY_sim_enabled ) THEN
          CALL USRMSG(' ',
     &     'Single-year climate database not yet entered.','W')
        ELSEIF ( .NOT. bMY_climates_defined .and.
     &           bMY_sim_enabled ) THEN
          CALL USRMSG(' ',
     &     'Multi-year climate database not yet entered.','W')
     
        ELSE

          call tstamp('>','SIM: enter initiate simulation')
          IF(BLDG3D)THEN

C For the multi-D model, check that the 1-D grid distribution is default.
            CALL CHKGRD(OK3D)
            IF(OK3D)THEN
              if( dll ) then
                close( iLogFile )
#ifdef F90
                open(UNIT=iLogFile,FILE=cLogFile,position='APPEND' )
#else
                OPEN(UNIT=iLogFile,FILE=cLogFile,ACCESS = 'APPEND')     
#endif
                write( iLogFile, * ) "Simulation started"
                close( iLogFile )
#ifdef F90
                open(UNIT=iLogFile,FILE=cLogFile,position='APPEND' )
#else
                OPEN(UNIT=iLogFile,FILE=cLogFile,ACCESS = 'APPEND')     
#endif
              endif          
              CALL MZSIML
            ELSE
              CALL USRMSG(' FATAL ERROR:',
     &          ' 3D zones should have default 1D gridding.','W')
            ENDIF
          ELSE
            CALL MZSIML
          ENDIF
          call tstamp('>','SIM: exit initiate simulation')
        ENDIF
      elseif(ino.eq.6)then

C Trace output. Begin by checking if configuration file exists.      
        IF (.NOT. INPOK ) THEN
          CALL USRMSG(' ','Configuration file not yet entered.','W')
        ELSE

C Setup trace.
          call mztrac
        ENDIF 
      elseif ( ino .eq. 7 ) then

C Enable / disable multi-year simulations. First
C toggle status flag, and warn user if required climate data
C is not yet specified. 
        if ( bMY_sim_enabled ) then
          bMY_sim_enabled = .false.

          if ( .not. bSY_climate_defined )
     &      CALL USRMSG (' ',
     &        ' Single-year climate database must be defined','W')
           
        else

          bMY_sim_enabled = .true.
          if ( .not. bMY_climates_defined )
     &      CALL USRMSG (' ',
     &        ' Multi-year climate database must be defined','W')
           
        endif 
      elseif(ino.eq.(NITMS-6))then

C Warning messages switch.
        dok=.true.
        h(1)='Warning messages are surpressed unless you say '
        h(2)='yes to this question. '
        h(3)=' '
        h(4)='If you are experiencing odd performance predictions'
        h(5)='you might turn warnings on as an extra check. '
        CALL ASKOK('Warning messages required?','(see help)',OK,dok,5)
        IF(OK)THEN
          IWMOFF=0
          IF(MMOD.NE.-6) ICOUT=99
        ELSE
          IWMOFF=1
        ENDIF
      elseif(ino.eq.(NITMS-5))then

C Toggle trace level.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0
      elseif(ino.eq.(NITMS-3))then

C Configure H3K reports.
        if ( rep_xmlstatus() ) then
          call Config_H3k_reports_menu()
        else
          CALL USRMSG(' ',
     &      ' H3K Reports has not been compiled into bps','W')
        endif
      elseif(ino.eq.(NITMS-1))then

C Help via the askabout facility in esru_ask.F The zero requests display
        ihelpm=1
        call askabout('bps ',0)
        if(ino.eq.1)then
          dok=.true.
          h(1)='Just in case there are additional messages. '
          CALL ASKOK(' ','Subsequent help messages required?',OK,dok,1)
          IF(OK)THEN
            IHELPM=1
          ELSE
            IHELPM=0
          ENDIF
        endif
      elseif(ino.eq.NITMS)then

C Clean up and exit.
C Delete any temporal scratch file if temporal data associated with
C the model.
        if(ITDFLG.ne.0)then
          CALL ERPFREE(IUTDF,ISTAT)
          lnts=lnblnk(LTDF)
          if(LTDF(lnts-15:lnts).eq.'.temporalscratch')then
            call usrmsg('Deleting scratch file...',LTDF,'P')
            call EFOPRAN(iutdf,LTDF,ITWPR,1,IER)
            CALL EFDELET(iutdf,ISTAT)
            call usrmsg('Deleting scratch file...done.',LTDF,'P')
          endif
        endif

C Delete all the 3D temporary files.
        IF(BLDG3D)THEN
          DO 60 ICOMP=1,NCOMP
            IF(ZONE3D(ICOMP))THEN
              CALL EFOPRAN(ITF3,L3DNDD(ICOMP),4,3,IER)
              CALL EFDELET(ITF3,ISTAT)
              CALL EFOPRAN(ITF3,L3DTLW(ICOMP),5,3,IER)
              CALL EFDELET(ITF3,ISTAT)
              CALL EFOPRAN(ITF3,L3DLWV(ICOMP),4,3,IER)
              CALL EFDELET(ITF3,ISTAT)
            ENDIF
   60     CONTINUE
        ENDIF
        IF(GRND3D)THEN
          CALL EFOPRAN(ITF3,LGDNDD,3,3,IER)
          CALL EFDELET(ITF3,ISTAT)
        ENDIF

C Close moisture result library.
        IF(MSTROK)CALL ERPFREE(IMSTR,ISTAT)
        call tstamp('>','Finish simulator')
        CALL ERPFREE(ICLIM,ISTAT)
        CALL EPAGEND
        if( dll ) return

        STOP
      else

C Trap unused menu picks.
        INO=-1
        GOTO 30
      endif

C Fall through from normal picks, re-display the menu.
      goto 20

C Error.
  103 if(autook) then
        ok=.false.
      else
        dok=.false.
        h(1)='There should not be a problem with the climate'
        h(2)='data at simulation time. Check your model! '
        CALL ASKOK(' Problem in climate file, try again? ',
     &             ' (no exits program) ',OK,dok,2)
      endif
      IF(OK)THEN
        goto 20
      ELSE
        CALL EPAGEND
        CALL ERPFREE(ICLIM,ISTAT)
        if( dll ) return
        STOP
      ENDIF

 901  call edisp(iuout,'Error opening journal file, continuing.')
      goto 902

      end

C Dummy routines (needed for call back from c).
      subroutine imgdisp(iforce,focus,ier)
      character focus*4
     
      return
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

      subroutine SVDSOPT
      return
      end

      subroutine RCDSOPT
      return
      end

      SUBROUTINE DRAWOBS(IFOC,ier)
      return
      end

      SUBROUTINE DRWSEN(ier)
      return
      end

      SUBROUTINE EGRNDR(IER)
      return
      end

      SUBROUTINE DSGRID(RH,GD,LD,IER)
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

C----------------------------------------------------
C  Subroutine: SetDllParms
C  Set parameters for esp-r engine as if running
C  in silent mode and text mode
C      subroutine SetDllParms(termtype,iappw,iappx,iappy,inf,sps,aut,
C     & actf)
C      character inf*72,sps*8,aut*12,fs*1,actf*72
C      integer termtype,iappw,iappx,iappy

C Place holder (actual code omitted from this version of bps).
C      return
C      end

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

