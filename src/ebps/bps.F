C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C ******************** bps ********************
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
C           ifil+8  for external help text
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
C           ifil+19+(ICFD-1)*100 CFD monitoring file
C           ifil+20 CFD temporarily file for use during simulation
C           ifil+21 Power flow results library
C           ifil+22 any supplementery temporarily opened file
C           ifil+23 first nochan (occupied utility file channel)
C                   NB nochan+x is used in several places
C           ifil+24+(ICFD-1)*100 CFD boundary condition export
C           ifil+25 Used by acoustics module
C           ifil+26 Used by acoustics module
C           ifil+27 Used by acoustics module
C           ifil+28 User-specified fuel-cell electrical loads
C           ifil+29+(ICFD-1)*100 CFD ACC actions log
C           ifil+30 Fuel cell electrical load scratch file
C           ifil+31 Read in vertical solar data
C           ifil+32 (fort.42) Vertical solar output channel
C           ifil+33 Agent log file
C           ifil+34 Gremlin log file
C           ifil+40 snow depth ASCII file
C           ifil+41 snow depth binary file
C           ifil+45 (fort.55) PV output channel
C           ifil+50 (fort.60) BEMS output dump
C           ifil+60 Structured mesh temperarture file.
C           ifil+70 H3KOutput File
C << file unit clash, consider ifil + 62
C           ifil+72 Contaminant description file
C           ifil+71 CETC .fcts1 output file  )   These files are written
C           ifil+72 CETC .fcts2 output file  )__ sequentially, and could all
C           ifil+73 CETC .fcts3 output file  )   use the same channel to
C           ifil+74 CETC .fcts4 output file  )   free up some file numbers
C           ifil+76 Runtime boundry condition definition file (BCD.tmp)
C           ifil+77 Temporary binary file used when converting
C                   hourly profiles 'Hrly_Load.tmp'
C           ifil+80 CETC month status IO channel
C           ifil+81 Runtime lookup table data file
C           ifil+82 File containing wind turbine power curve data
C           ifil+83 Temporary file access for deleting "console_out.txt" prior to simulation
C MMOD   -  index returned from the 'pages' routine
C           and determines the device type attached to current job.

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

C Initialise variables in stratified_tank and NCHE commons.
#include "BlockData.h"   

      program bps
      USE START_UP, ONLY: UsePassedCommandLine

C======hot3000========================start
C
C  This bps main is here for convience. It
C  allows the same version of this file to
C  be used to compile the following forms
C  of bps: a UNIX executable, a Windows
C  EXE and a Windows DLL.
C
C==========================================
      common/dllflg/dll
      character dllsubr*12,dllmesg*124
      logical dll

      dll = .false.
      ! This ensures the command line is used and not a string passed in
      UsePassedCommandLine = .false.
      call SubBps

      end program
c*******************************************************************************

      subroutine PassInCommandLine(nParameters, CommandLineString,
     &            length)
      USE START_UP
      USE DLL_Functions
      IMPLICIT NONE

C Declare  calling parameter types
      character(300), intent(in)  :: CommandLineString
      integer, intent(in) :: length
      integer, intent(in) :: nParameters
      character(300)  :: Buffer
      Buffer = ''
      Buffer(1:length) = CommandLineString(1:length)
C      write(*,*)'In PassInCommandLine'
C      write(*,*)'CommandLineString = ',CommandLineString
C      write(*,*)'length = ',length
C      write(*,*)'nParameters= ',nParameters
      call SetPassedCommandLine(Buffer, nParameters)
C Since this subroutine is called by the harmonizer to initialize ESP-r,
C this is a good place to load in the harmonizer dll.
      call LoadDll
      end subroutine PassInCommandLine
c*******************************************************************************


C ******************** SubBps ********************
C For use where bps needs to be called as
C a subroutine rather than as an application.

      subroutine SubBps
      USE START_UP
      use h3kmodule
      use TCC, ONLY:COSIM
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "plant.h"
#include "uncertainty.h"
#include "net_flow.h"
#include "MultiYear_simulations.h"
#include "cfd.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
#include "climate.h"
#include "help.h"

      integer lnblnk  ! function definition

C For dll mode dllsubr is the subroutine name where an error occurs
C and dllmesg tells the details of the error.
      common/dllerr/dllsubr,dllmesg
      common/dllflg/dll
      common/BPSDLL/iLogFile, cBpsVersion, cPrjPath, cPrjName,
     &            cCfgPath, cCfgFile, cLogFile, lBpsDebug

      logical lBpsDebug
      character*72 cBpsVersion, cPrjPath, cPrjName, cCfgPath,
     &           cCfgFile, cLogFile

      common/outin/iuout,iuin,ieout
      common/warncounter/iecount
      common/shout/icout
      common/filep/ifil
      common/tc/itc,icnt
      common/trc/itrc
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/CLMDT1/CLMLOC
      CHARACTER CLMLOC*42
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      common/appw/iappw,iappx,iappy
      common/er1/ier1
      common/wrnoff/iwmoff
      common/help/ihelpm
      common/avrage/idaver

C Record lengths for zone (nzrl), plant (nprl) and electrical(nerl)
C results files.
      common/reclen/nzrl,nprl,nerl
      common/recver/izver,ipver,iever
      common/hcthry/ihct,icorexhct
      COMMON/DNORGH/IRTYPE
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES,CFRESF
      character RFILE*72,PFILE*72,MSTRFILE*72,LAFRES*72,CFRESF*72
      common/rdcgen/irdact
      common/rdcmfs/irdcm,sFLWTIM,sDRYB,sWDIR,sWSPD,sITER,sIOK,
     &              sTNOD(MNOD)
      COMMON/VTHP18/LNLTHP
      CHARACTER LNLTHP*72
      COMMON/VTHP31/INTHPS,INTHPZ(MCOM)
      LOGICAL INTHPS,INTHPZ
      COMMON/GR1D05/LGRD1D
      CHARACTER LGRD1D*72
      COMMON/GR1D06/IGR1D
      LOGICAL IGR1D
      COMMON/GR3D100/BLDG3D,ZONE3D(MCOM)
      LOGICAL BLDG3D,ZONE3D
      COMMON/GR3D109/L3DNDD(MCOM),L3DTLW(MCOM),L3DLWV(MCOM)
      CHARACTER*72 L3DNDD,L3DTLW,L3DLWV
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GRND100/GRND3D
      LOGICAL GRND3D
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      LOGICAL MSTROK,MSTRZN
      COMMON/MOIST09/IMSTR
      COMMON/GRSD100/IndxSt
      COMMON/GRSD122/InSvST,ISTemp

C Path to model and command line file (if any).
      common/rpath/path
      character path*72
      common/rcmd/LCMDFL
      character LCMDFL*144
      common/user/browse
      logical browse

C CFD output channels.
      COMMON/ICFCHN/ICFMON(MNZ),ICFDBC(MNZ),ICFTMP,ICFLIB
      common/CFDres/photon,flwvis

C CFD results library.
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)

C CFD visualisation.
      common/flvpar/Lflpar(MCOM)
      CHARACTER*72 Lflpar
      COMMON/ACT/ACTCNN(MCNN)

C CFD coupling toggles
      common/CFDTOGS/ACCON,CFD2BLD,CFD2MFS

C CFD grid refinement.
      COMMON/RFNGRD/DOFLT,DORFN,IFNDWT(MNZ),NRFN,IRFNCLS(MNZ,MRFN,3),
     &  NRFND,IRFND(MNZ,MRFNT,3)
      logical DOFLT,DORFN

C Project journal on/off, unit number, cmd, file name.
      common/journopt/journio,iuj,journcmd,jfile

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/CHANNL/NOCHAN

C Simulator parameters.
      COMMON/SPFL/spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok
      LOGICAL spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok
      common/SPFLN/spsname
      character spsname*30

C BEMS interaction time for results dump.
      COMMON/BEMSTIM/bemstime,ibems,bresfil

C Illuminance database.
      common/ILDB/NDBENT,DBILLA(200,5),DBILLS(200,MLCOM,MDF)

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

C Active domain common (expand to other domains?).
      common/ACTDOM/CFDOK
      LOGICAL CFDOK

C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

C Timeout information (not currently used).
C      common/TIMEOUT/sTimeout,iTimeoutFlag
C      character sTimeout*72
C      integer iTimeoutFlag

C Name of current application
      common/APPNAME/cAppName
      character cAppName*12

C Session log file (for errors etc.)
      character iefile*72
      common/logs/iefile

C Flag for enabling h3k customizations 

      common/H3KCustom/bEnableH3kExtensions
      logical bEnableH3kExtensions

      real UNMETHRS  ! for each zone counter for unmet hours.
      COMMON/UNMET/UNMETHRS(MCOM)

C Toggle for extreme silent running (endless) mode.
      logical endless,issilient
      common/endlessmode/endless,issilient

C Logical flag for uncertainty header has been printed.
      logical uncertheadprinted
      common/uncerthead/uncertheadprinted

      integer iverb     ! verbosity passed on command line

      LOGICAL OK,dok,OK3D
      LOGICAL INPOK,XST
      logical unixok,there,photon,flwvis,dll,ACTCNN
      logical concat

      character*29 item(15)
      character OUTS*124
      character inf*144,sps*30,aut*12,fs*1,actf*96
      character llclmdb*144
      character browsing*4
      character bresfil*72
      character journcmd*20,jfile*72,cjfile*72,OUTSTR*124
      character dllsubr*12,dllmesg*124
      character t16*16,outs248*248
      character cVnum*38      ! returned from ESPrVersionNum
      character pagestitle*68 ! for banner title via epages call
      character dstmp*24,uname*24,tfile*72

      character mca*12        ! to force Monte Carlo assessments
      integer nbsets          ! how many sets to run
      COMMON/UAMET/IUNCMET,nbsets
      COMMON/UA5/IMET,ISIM,NRSIM

C Ask to overwrite flag.
      COMMON/OVRWT/AUTOVR
      logical AUTOVR

C Flags for multi-year simulation menus.
      logical bOptOut, bDone, bMY_clm_err, bExists, bCreate
      logical bautook

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer ier
      integer NITMS,INO  ! max items and current menu item
      integer ACCON,CFD2BLD,CFD2MFS
      integer ISTRW

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
      integer iicapture   ! to pass to updcapt
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
      integer*8 iicapture   ! to pass to updcapt
#endif

C Active domain.  Assume not active, check if running in dll mode
C and clear dll subroutine name and error details.
      CFDOK=.false.

C Co-simulation between ESP-r and TRNSYS is possible.  By default assume
C there is no co-simulation.  This flag will be set to true if a TRNSYS
C coupling component is detected by the plant domain.
      COSIM = .false.

      IGR1D = .false. ! assume no adaptive 1D gridding

C Prior to commencing simulation, delete "console_out.txt", if found.
      INQUIRE (FILE="console_out.txt",EXIST=XST)
      if(XST)then
            OPEN( UNIT=ifil+83, FILE="console_out.txt" )
            CLOSE ( UNIT=ifil+83, STATUS='DELETE' )
      endif

C Remove Modish lock file.
      call system("rm -f ../tmp/Z*.lock.tmp")

C Mark in log file, start of simulation.
      if( dll ) then
        open(UNIT=iLogFile, FILE=cLogFile,position='APPEND' )
        write( iLogFile, * ) "subroutine: bps() - dll run"
        write( iLogFile, * ) "checking input"
        close( iLogFile )
        open(UNIT=iLogFile, FILE=cLogFile,position='APPEND' )
      endif

      dllsubr=' '
      dllmesg=' '
      do 1 I=1,MCNN
        ACTCNN(I)=.TRUE.
 1    continue

C Disable h3k customisations by default.
      bEnableH3kExtensions = .false.

C Assume that the configuration, mlc db, primitive db, control file
C have not been read in.
      CFGOK=.FALSE.
      MLDBOK=.FALSE.
      MATDBOK=.FALSE.
      CTLOK=.FALSE.
      spfileok=.false.
      LUALF='UNKNOWN'
      RFILE='BLDres'
      PFILE='PWRres'
      LAFRES='FLWres'
      CFRESF='CFDres'
      MSTRFILE='MSTres'
      do 5 I=1,MCOM
        Lflpar(I)='UNKNOWN'
        UNMETHRS(I)=0  ! reset counter of unmet hours
 5    continue
      sps='  '

C CFD results output. `photon' indicates whether PHOTON phi files will
C be created; `flwvis' indicates whether ESP-r's in-built flow visualization
C facility will be used to generated images of the flow. `gopher' is a flag
C used to generate images for the preliminary flow field when the adaptive
C conflation controller is active (not referenced).
      photon=.FALSE.
      flwvis=.FALSE.

C CFD results library: IRECPC=current record; ICFDSV=results version.
      IRECPC=0
      ICFDSV=3

C Default CFD coupling toggles.
      ACCON=1        ! Adaptive Conflation Controller active
      CFD2BLD=1      ! 1-way CFD-building conflation
      CFD2MFS=2      ! 2-way CFD-mass flow network conflation
      DOFLT=.True.   ! Residual flatline detection on
      DORFN=.False.  ! Automatic grid refinement off

C Illuminance values held in database.
      NDBENT=0

C Assume interactive or silent mode.
      endless=.false.; issilient=.false.

C Font sizes for graphic environment.
      IFS=1; ITFS=1; IMFS=1

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
      matver=0.0     ! initial assumption of binary materials database
      AUTOVR=.false. ! by default, ask to overwrite files

      cAppName = 'bps'
      helpinapp='bps'  ! set once for the application
      helpinsub='bps'  ! set for MAIN

C Set parameters for DLL run.
C all console write is rerouted to the log file
      if( dll ) then
        itu = iLogFile
        iuout = iLogFile
        icout = iLogFile
        open(UNIT=iLogFile, FILE=cLogFile,position='APPEND' )
        call SetDllParms(MODL,iappw,iappx,iappy,inf,sps,aut,actf)
        browsing='no'
      else

C Get command line parameters from local subroutine parbps.
        call parbps(iverb,MODL,iappw,iappx,iappy,inf,browsing,sps,aut,
     &    actf,mca,nbsets)
        itrc=iverb    ! set trace after call to parsim
        if(mca(1:3).eq.'MCA')then
          IUNCMET=3
          IMET=3
          if(nbsets.gt.0) NRSIM=nbsets
        else
          IUNCMET=0
          IMET=0
          NRSIM=0
        endif
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

C Check for endless running mode.
      if(aut(1:7).eq.'endless')then
        autook=.true.
        endless=.true.
        call edisp(iuout,'Reality-Emulator endless-running mode.')
        call tstamp('>','bps: Endless-running mode')
      endif

C Create new child thread to timeout simulation if required.
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
      ICFTMP=IFIL+20
      nochan=ifil+23
      ISTemp=IFIL+60

C Use process ID as seed for random number generator (for use in
C uncertainty analysis routines)
      call esppid(ipid)
      x=RSTART(ipid)
      uncertheadprinted=.false.

C Initialise output device: -10 forces user selection.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=590
        iappx=75
        iappy=170
      else
        if(iappx.le.0)iappx=75
        if(iappy.le.0)iappy=170
        if(iappw.le.200)then
          iappwi=int(590*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/725.0))

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

C Set initial font sizes (IMFS is for menus, IFS is for dialog & ITFS text feedback).
C Fonts 4-7 are proportional and 0-3 are fixed width. Use proportional for menus
C and dialog.
        IMFS=4
        IFS=4
        ITFS=4
        call userfonts(IFS,ITFS,IMFS)
        call defaultfonts(IFS,ITFS,IMFS)  ! and remember these as defaults
      ELSE
        IMFS=4   ! Text mode set LIMTTY larger for paging menus.
        IFS=4
        ITFS=4
        LIMTTY=30
        LIMIT =30
      ENDIF

C Find the current ESP-r version number and add it to application title.
      call ESPrVersionNum(cVnum)
      write(pagestitle,'(2a)') 'Integrated Simulator of ESP-r ',
     &  cVnum(1:lnblnk(cVnum))

      lntitle=lnblnk(pagestitle)
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &  pagestitle,lntitle)

      IF(MMOD.EQ.-6)then
        ICOUT=0
      else
        ICOUT=IUOUT
      endif

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then

C Setup and pass in parameters to win3d.
          iiw1=15; iiw2=15; iiw3=6; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
          call opencpw
          call opensetup
          iicapture=1
          call updcapt(iicapture)
        elseif(iglib.eq.2)then
          iiw1=15; iiw2=15; iiw3=5; iiw4=4; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
        endif
        call setzscale()
        mdispl=0; nifgrey=0
        ncset=0; ngset=0; nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      ELSE

C Set reasonable defaults for text mode.
        igw=431; igwh=187; igl=92; igb=273
        iiw1=15; iiw2=15; iiw3=6; iiw4=3; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3dwwc(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
      ENDIF
      CALL EDISP(IUOUT,' ')

C Write version (but only title if silent or endless).
      if(aut(1:6).eq.'silent')then
        CALL ESPrVersion("silent",cAppName,IUOUT)
      elseif(aut(1:7).eq.'endless')then
        CALL ESPrVersion("silent",cAppName,IUOUT)
      else
        CALL ESPrVersion("summary",cAppName,IUOUT)
      endif

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
C after scan of .esprc file. Do it silently.
 902  call escdef(IFIL+1,'s',IER)

C Create and open a session log file based on user name, PID etc.
      uname=' '; tfile=' '
      call usrname(uname)
      call esppid(ipid)
        write(tfile,'(a,a1,a,a,i7,a)')upath(1:lnblnk(upath)),fs,'.',
     &    uname(1:lnblnk(uname)),ipid,'.log'
      call st2file(tfile,iefile)

C Open the session file.
      ieout=ifil+932          ! set to unused index 942
      iecount=0               ! clear count of errors
      open(ieout,file=iefile,status='UNKNOWN',err=903)
      write(ieout,'(a)')'Session log for bps'
      call dstamp(dstmp) ! get curret time
      write(ieout,'(2a)')'Date,',dstmp
      write(ieout,'(2a)')'User,',uname(1:lnblnk(uname))
  903 continue

C Take command line file name as initial configuration file.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
C        call edisp(iuout,'  ')  ! echo blank line
        write(outs248,'(a,a)') 'The input file is: ',LCMDFL
C        call edisp248(iuout,outs248,90)
        call tstamp('>',outs248)
      else
        LCMDFL='UNKNOWN'
      endif

C Inital weather assumption.
      LCLIM=DCLIM
      ICLIM=IFIL

C Day & time format preference.
      IFDAY=2
      IFTIME=0

C Initialise once per hour storage of results counter.
      irdact=0
      irdcm=1

C Record length of binary results file (nzrl). izver is the
C version of the zone results file. Since Feb 2008 izver
C has been set at 4 and signals explicit inclusion of separate
C conv/rad/latent gain fields for each casual gain type.
      nzrl = MZRL
      izver = 4
      izver = 5  ! use more of record width

C Record length of plant results file.
C Parameter 'nprecl' as defined in 'plant.h'.
      nprl = nprecl

C Unit for materials db in case control -99 is invoked.
      ifmat=ifil+5

C Unit for CFClayers db in case control -99 is invoked.
      icfcdb=ifil+6
      write(LCFCDB,'(a)') DCFCDB(1:lnblnk(DCFCDB))

C Initialise results averaging flag: 0 means
C results will be averaged.
      idaver=0

C Default surface convective heat transfer coefficient model.
C ihct (for internal convenction) icorexhct (for external).
C These are overwritten by directives in cfg file.
      ihct=1        ! assume A&H
      icorexhct=1   ! assume McAdams

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
            write(spsname,'(a30)')sps(1:30)
            if(aut(1:6).eq.'silent')then
              autook=.true.; issilient=.true.
              call tstamp('>','bps: Silent-running mode')
            elseif(aut(1:7).eq.'endless')then
              autook=.true.
              endless=.true.
              call edisp(iuout,'Endless-running mode.')
              call tstamp('>','bps: Endless-running mode')
            else
              autook=.false.
            endif
          else
            spsname='  '
          endif
          call tstamp('>','bps: enter input checks')

C Note: call to MZINPT corrupts IGR1D on some compilers.
          CALL MZINPT(IREP,ier)

C If running dll note the failure mode.
          if(ier.eq.2.and.dll)then
            open(UNIT=iLogFile, FILE=cLogFile,position='APPEND' )
            write(iLogFile,*) 'bps failed in ',dllsubr
            write(iLogFile,*) 'details: ',dllmesg
            return
          endif
          call tstamp('>','bps: exit input checks')

C Increment input flag if no input data errors detected.
          IF(IER.EQ.0)THEN
            INPOK=.TRUE.
          ELSE
            if ( autook ) then
              call edisp(iuout,' ')
              call edisp(iuout,
     &              'Fatal error: Model was not scanned correctly')
              iecount=iecount+1  ! retain the log file
              call pauses(3)     ! pause for 2 seconds then abort program.
              close(ieout)
              CALL ERPFREE(ieout,ISTAT)
              call epagend
              stop
            endif
            if(dll)then
              dllsubr='bps MAIN'
              dllmesg='IER1 flag set while scanning the model.'
              ier=2
              return
            endif
            helptopic='model_cfg_scan_warning'
            call gethelptext(helpinsub,helptopic,nbhelp)
            call easkabc('Problem scanning model!',
     &        'Options:','read again','use it','abort',IW,nbhelp)
            if(iw.eq.1)then
              INPOK=.FALSE.
            elseif(iw.eq.2)then
              call tstamp('>','bps: use model with init scan warning')
              INPOK=.TRUE.
            elseif(iw.eq.3)then
              call tstamp('>','bps: abort after error scan problem')
              call pauses(2)   ! pause for 2 seconds then abort program.
              if( dll ) then
                dllsubr='bps MAIN'
                dllmesg='IER1 flag set while scanning the model.'
                ier=2
                return
              else
                call usrmsg('IER1 flag set while scanning the model.',
     &            ' ','W')
                iecount=iecount+1  ! retain the log file
                close(ieout)
                CALL ERPFREE(ieout,ISTAT)
                call pauses(2)   ! pause for 2 seconds then abort program.
                CALL EPAGEND
                STOP
              endif
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
          CALL THERMR(LNLTHP,IUNIT,ITRC,IER)
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
   20 ITEM(1)    ='a define model               '
      ITEM(2)    ='b assign weather file        '
      ITEM(3)    ='  _________________________  '
      ITEM(4)    ='c initiate simulation        '
      ITEM(5)    ='  _________________________  '
      ITEM(6)    ='t trace facilities           '
      if ( bMY_sim_enabled ) then
         ITEM(7) ='y multi-year sim >> ON       '
      else
         ITEM(7) ='y multi-year sim >> OFF      '
      endif
      IF(IWMOFF.EQ.1)THEN
        ITEM(8)  ='w warnings >> OFF            '
      ELSEIF(IWMOFF.EQ.0)THEN
        ITEM(8)  ='w warnings >> ON             '
      ENDIF
      IF(ITRC.EQ.0)THEN
        ITEM(9)  ='r reporting >> silent        '
      ELSEIF(ITRC.EQ.1)THEN
        ITEM(9)  ='r reporting >> summary       '
      ELSEIF(ITRC.EQ.2)THEN
        ITEM(9)  ='r reporting >> detailed      '
      ENDIF
      ITEM(10)   ='  _______________________    '
      if ( isReportingInstalled() ) then
         ITEM(11)='s configure H3K Reports      '
      else
         ITEM(11)='s H3K Reports unavailable    '
      endif
      ITEM(12)   ='  _________________________  '
      ITEM(13)   ='? help                       '
      ITEM(14)   ='- quit module                '
      nitms=14

C And menu is to be displayed.
      ino=-3
   30 CALL USRMSG(' ',' ','-')

C Use askabout to instantiate the initial help messages (2nd parameter is one).
      call askabout('bps ',1)

C Do not display the menu if in autook mode as defined in the
C simulator parameter file.
      if(autook) then
        if(exitok) then
          ino=NITMS
        else
          ino=4   ! sensitive to menu layout
        endif
      else
        CALL EMENU('Integrated simulator',ITEM,NITMS,INO)
      endif

C Enter configuration file.
      IF(INO.EQ.1)THEN
        call tstamp('>','bps: enter read model')
        CALL MZINPT(IREP,ier)
        if(ier.eq.2.and.dll)then
          open(UNIT=iLogFile, FILE=cLogFile,position='APPEND' )
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

C Initialize the thermal and gridding variables.
        IF(INPOK.AND.INTHPS)THEN
          CALL THZERO
          CALL THERMR(LNLTHP,IUNIT,ITRC,IER)
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

C Initialise the structured gridding variables.
        IF(IndxSt.GT.0)THEN
          CALL READST(IndxSt,ITRC,IER)
          IF(IER.EQ.0)CALL INITST
          IF(IER.NE.0)INPOK=.FALSE.
        ENDIF
        call tstamp('>','bps: exit read model')
      ELSEIF(INO.EQ.2 .and. .not. bMY_sim_enabled )THEN

C Single-year climate file definition
C Set climate data file (file must exist - unit
C number = ifil). If found, attempt to open and test that it is
C a climate file by reading records 366 and 367.
        helptopic='ask_for_climate_file'
        call gethelptext(helpinsub,helptopic,nbhelp)
        iclim=ifil
        call erpfree(iclim,istat)

        llt=lnblnk(LCLIM)
        write(llclmdb,'(a)') LCLIM(1:llt)

 291    llt=lnblnk(llclmdb)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            ISTRW=96
          elseif(llt.ge.96.and.llt.lt.124)then
            ISTRW=124
          elseif(llt.ge.124.and.llt.le.144)then
            ISTRW=144
          endif
        elseif(iglib.eq.2)then
          ISTRW=144
        else
          ISTRW=96
        endif
        CALL EASKF(llclmdb,' ','Weather file?',
     &    ISTRW,DCLIM,'weather file name',IER,nbhelp)
        IF(llclmdb(1:2).EQ.'  ')GOTO 291
        IER=0
        ipathclim=1

C Cast expanded weather file name back to LCLIM.
        llt=lnblnk(llclmdb)
        write(LCLIM,'(a)') llclmdb(1:llt)
        call tstamp('>','bps: change weather')
        call tstamp('>',LCLIM)
        ier=0
        call CLMOPB(0,IER)
        IF(ier.eq.0)THEN
          CALL CLMRDBMD(IER)
          WRITE(OUTS,1000)CLMLOC(1:lnblnk(CLMLOC)),ICYEAR
 1000     FORMAT(' Climate data: ',A,':',I5)
          if(endless.or.issilient)then
            continue
          else
            CALL EDISP(IUOUT,' ')
            CALL EDISP(IUOUT,OUTS)
          endif
          IRTYPE=0
          IF(ICDNGH.EQ.123)IRTYPE=1
          bSY_climate_defined = .true.  ! Set flag: climate data ok

        ELSE
          bSY_climate_defined = .false.

          helptopic='not_have_errors_clm'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK('Problem reading weather file!',
     &                'Retry',OK,nbhelp)
          IF(OK)THEN
            goto 20
          ELSE
            call tstamp('>','bps: abort after error scanning climate')
            call pauses(2)   ! pause for 2 seconds then abort program.
            CALL ERPFREE(ICLIM,ISTAT)
            if( dll ) return
          ENDIF
        ENDIF

C Close & free file (it will be opened again before the start
C of the simulation
C        CALL EPAGEND
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

          call tstamp('>','bps: enter initiate simulation')
          IF(BLDG3D)THEN

C For the multi-D model, check that the 1-D grid distribution is default.
            CALL CHKGRD(OK3D)
            IF(OK3D)THEN
              if( dll ) then
                close( iLogFile )
                open(UNIT=iLogFile,FILE=cLogFile,position='APPEND' )
                write( iLogFile, * ) "Simulation started"
                close( iLogFile )
                open(UNIT=iLogFile,FILE=cLogFile,position='APPEND' )
              endif
              CALL MZSIML
            ELSE
              CALL USRMSG(' FATAL ERROR:',
     &          ' 3D zones should have default 1D gridding.','W')
            ENDIF
          ELSE
            CALL MZSIML
          ENDIF
          call tstamp('>','bps: exit initiate simulation')
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
     &        ' Multi-year weather database must be defined','W')

        endif
      elseif(ino.eq.(NITMS-6))then

C Warning messages switch.
        helptopic='supress_warnings'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(' ','Warning messages required?',
     &    OK,nbhelp)
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
        if ( isReportingInstalled() ) then
          call Config_H3k_reports_menu()
        else
          CALL USRMSG(' ',
     &      ' H3K Reports has not been compiled into bps','W')
        endif
      elseif(ino.eq.(NITMS-1))then

C Help via the askabout facility in esru_ask.F The zero requests display.
        ihelpm=1
        call askabout('bps ',0)
        if(ino.eq.1)then
          CALL EASKOK(' ','Help messages required?',
     &      OK,nbhelp)
          IF(OK)THEN
            IHELPM=1
          ELSE
            IHELPM=0
          ENDIF
        endif
      elseif(ino.eq.NITMS)then

C Clean up and exit.
        if(endless.or.issilient)then
          continue
        else

C Write unmet hours to channel 27 before closing bps.
C << Need to enable unmet hours recovery in res. >>
          write(27,'(a)')
     &      'Unmet set-point hours during simulation.'
          do I=1,NCOMP
            write(27,'(a,i2,1x,a,f8.1)') 'Zone ',I,zname(I),UNMETHRS(I)
          enddo
        endif

C Delete temporal scratch file.
        if(ITDFLG.ne.0)then
          CALL ERPFREE(IUTDF,ISTAT)
          lnts=lnblnk(LTDF)
          if(LTDF(lnts-15:lnts).eq.'.temporalscratch')then
            ier=0
            call EFOPRAN(iutdf,LTDF,1,1,IER)
            CALL EFDELET(iutdf,ISTAT)
          endif
        endif

C Delete 3D temporary files.
        IF(BLDG3D)THEN
          DO 60 ICOMP=1,NCOMP
            IF(ZONE3D(ICOMP))THEN
              ier=0
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
          ier=0
          CALL EFOPRAN(ITF3,LGDNDD,3,3,IER)
          CALL EFDELET(ITF3,ISTAT)
        ENDIF

C Close moisture result file.
        IF(MSTROK)CALL ERPFREE(IMSTR,ISTAT)
        call tstamp('>','Finish simulator')
        CALL ERPFREE(ICLIM,ISTAT)
        if( dll ) return

C Clear allocatable arrays.
        close(ieout)
        if(iecount.eq.0)then     ! if there were no warnings delete
          open(ieout,file=iefile,status='UNKNOWN')
          call efdelet(ieout,istat)
        endif
        CALL DeallocateAllArrays
        call epagend

C Remove modish lock and temporary files.
        call system("rm -f ../tmp/Z*.lock.tmp") !
        call system("rm -f ../tmp/*.store.tmp") !
        call system("rm -f ../tmp/*.temp.*")
        call system("rm -f ../tmp/*.temp")
        return
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
        helptopic='not_have_errors_clm'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK('Problem with weather file!',
     &       'Retry?',OK,nbhelp)
      endif
      IF(OK)THEN
        goto 20
      ELSE
        CALL ERPFREE(ICLIM,ISTAT)
        iecount=iecount+1  ! retain the log file
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        if( dll ) return
        call epagend
        STOP
      ENDIF

 901  call edisp(iuout,'Error opening journal file, continuing.')
      goto 902

      end subroutine SubBps


C ******************** parbps ********************
C Command line parser for bps.

      subroutine parbps(iverb,termtype,iappw,iappx,iappy,inf,browse,
     &  sps,aut,actf,mca,nbsets)

      integer lnblnk  ! function definition
      integer, intent(inout)  ::  iverb, termtype,iappw,iappx,iappy
      character(144), intent(inout)  :: inf
      character(96), intent(inout) :: actf
      character(12), intent(inout) :: aut
      character(4), intent(inout) :: browse
      character(30), intent(inout) :: sps
      character(72)  :: argument,sTimeout
      character(12), intent(inout) :: mca  ! is 'mca' or 'bc' or '-'
      integer, intent(inout)  ::  nbsets   ! number of sets to run

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/TIMEOUT/sTimeout,iTimeoutFlag
      integer :: iTimeoutFlag
      integer :: err

      integer iargc,m
      integer i
      real conv
      character prog*72,outs*248,appn*24
      character mode*8
      logical unixok

      inf  = 'UNKNOWN'
      mca  = '-'  ! assume not doing monte-carlo
      iverb = 0   ! assume not verbose
      nbsets = 0  ! assume no monte-carlo sets

C Get number of arguments and command name.
      termtype = 8      ! assume graphic mode
      mode = 'graphic'
      aut  = 'default'
      browse = '    '
      m = iargc()
      i = 0
      call getarg(i,prog)
      call findapp(prog,appn)
      call isunix(unixok)

C If one parameter.
      if(m.eq.0)then
        goto 42
      elseif(m.ge.1)then
  41    i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)
        if(argument(1:5).eq.'-help')then
         call edisp(iuout,' ')
         call edisp(iuout,'This is the ESP-r Simulator; use:')
         call edisp(iuout,' [-mode {text|graphic/script}]')
         call edisp(iuout,' [-file <model configuration file>]')
         call edisp(iuout,' [-v|-vv]')
         call edisp(iuout,'    verbose or very verbose')
         call edisp(iuout,' [-p <par_set_name> {interactive|silent}]')
         call edisp(iuout,' [-actf <BEMS commands file>')
         call edisp(iuout,' [-s <width, offset left, offset top>]')
         call edisp(iuout,'    size of start-up window.')
         call edisp(iuout,' [-b {no|yes} browse mode]')
         call edisp(iuout,'    if yes, results are placed in the user')
         call edisp(iuout,'    home folder, not the model folder.')
          stop
        elseif(argument(1:3).eq.'-vv')then
          iverb = 2   ! tell application to run with debug verbose
        elseif(argument(1:2).eq.'-v')then
          iverb = 1   ! tell application to run with debug moderate
                      ! Note: this option MUST follow -version for both to be supported
        elseif(argument(1:5).eq.'-mode')then

          i=i+1
          call getarg(i,argument)
          if(argument(1:4).eq.'text')then
            termtype = -1
            mode = 'text'
          elseif(argument(1:4).eq.'page')then
            termtype = -2
            mode = 'page'
          elseif(argument(1:5).eq.'graph')then
            termtype = 8
            mode = 'graphic'
          elseif(argument(1:6).eq.'script')then
            termtype = -6
            mode = 'script'
          endif
        elseif(argument(1:5).eq.'-file')then
          i=i+1
          call getarg(i,inf)
        elseif(argument(1:5).eq.'-actf')then
          i=i+1
          call getarg(i,actf)
        elseif(argument(1:4).eq.'-mca')then
          mca  = 'MCA'  ! set monti-carlo assessment
          i=i+1
          call getarg(i,argument)  ! number of Monti-Carlo sets
          read(argument,*,IOSTAT=IOS,ERR=2)nbsets
        elseif(argument(1:2).eq.'-b')then
          i=i+1
          call getarg(i,browse)
        elseif(argument(1:2).eq.'-p')then
          i=i+1
          call getarg(i,sps)
          i=i+1
          call getarg(i,aut)
        elseif(argument(1:8).eq.'-timeout')then
          i=i+1
          call getarg(i,sTimeout)
          iTimeoutFlag = 1
        endif
        goto 41
      endif
  42  continue

      if ( iverb == 2 ) then
        write(outs,'(6a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf))
        if(unixok)call edisp248(iuout,outs,90)
        outs=' '
        write(outs,'(7a)') 'and parameters ',sps(1:lnblnk(sps)),
     &    ' in ',aut,' mode and ',browse,' browsing.'
        if(unixok)call edisp248(iuout,outs,90)
      endif
      return

C Errors.
   2  call isunix(unixok)
      if(unixok)then
        if(IOS.eq.2)then
          write(6,'(a)')
     &      'bps: permission error getting parameters.'
        else
          write(6,'(a)')'bps: error extracting parameters.'
        endif
        return
      endif
      end

C Dummy routines (needed for call back from c).
      subroutine imgdisp(iforce,focus,ier)
      character focus*4

      return
      end

C Dummy routines from common3dv.F needed to match implied
C calls in esru_lib.F
      SUBROUTINE ADJVIEW(IER)
      ier=0
      return
      end

      subroutine redraw(IER)
      ier=0
      return
      end

      subroutine chgazi(icazi,ifrlk)
      return
      end

      subroutine chgelev(icelev,ifrlk)
      return
      end

      subroutine chgpan(ix,iy)
      return
      end

      subroutine chgzoom(imode)
      return
      end

      subroutine optview
      return
      end

      subroutine chgsun(isunhour)
      return
      end

      subroutine chgeye(EVX,EVY,EVZ,VX,VY,VZ,EAN,JITZNM,JITSNM,JITVNO,
     &   JITOBS,JITSNR,JITGRD,JITORG,DIS,JITBND,JITDSP,JITHLS,JITHLZ,
     &   JITPPSW)

C Passed parameters.
      real EVX,EVY,EVZ,VX,VY,VZ,EAN,DIS

C Depending on computer type, set integer size of passed parameters.
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

      SUBROUTINE PLELEV(direc)
      CHARACTER direc*1
      return
      end

C  ******************** SetDllParms ********************
C  Sets parameters as if running in silent mode or text mode.
C
C      subroutine SetDllParms(termtype,iappw,iappx,iappy,inf,sps,aut,
C     & actf)
C
C      character inf*144,sps*30,aut*12,fs*1,actf*96
C      integer termtype,iappw,iappx,iappy
C
C Place holder (actual code omitted from this version of bps).
C      return
C      end

