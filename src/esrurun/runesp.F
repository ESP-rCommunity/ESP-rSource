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

C runesp     - the main controlling routine.
C parse_oc   - example of parsing output from a 3rd party
C              tool and using it to create a silentread file
C setschedule - generate a zone operation file matching a
C               fixed pattern but with occupant/lighting/sp
C               densitites passed in from 3rd party file.

C *************************************************************
C runesp is a command line driven tool which takes a file
C from a 3rd party GUI and does three tasks:
C a) it parses the command file and creates a 'silentread' file
C    to pass to the project manager. The project manger uses
C    the information in the silentread file to create a new
C    single folder model.
C b) it invokes the project manager to further process an
C    existing model (this is work-in-progress).
C c) it invokes the simulator to run an annual simulation based
C    on the newly created model.
C d) on exit it prints the name of the configuration file it
C    created.

C The common blocks and variable instanciations below are based
C on the startup sequence in prj.F that preceed the calls which
C deal with reading a silentread input file.
C ************************************************************

      program runesp
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "uncertainty.h"
#include "espriou.h"
#include "esprdbfile.h"
#include "material.h"
#include "schedule.h"
      PARAMETER (MSTMC=20)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/trc/itrc
      COMMON/INPER/INIT
      COMMON/OUTPCH/ICOUT
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/DAYSF/KDS,KDF
      common/cctlnm/ctldoc,lctlf
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)

      common/C21/IFCFG,cfgroot,LCFGF

      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth,aimpth
      common/radcfg/LRADCF

      common/IPVF/lipvdatf

C Path to problem and command line file (if any).
      common/rpath/path
      common/uhome/upath
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG
      common/LOG/LPRJLG
      common/GTFIL/GTGEOM

C Name of current application
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2
      common/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Uncertainty.
      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

      common/idstb/nidst
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton
      COMMON/BIDIRFL/bidirfile,bidirname(MSTMC)
      
      CHARACTER INTER*144,INF*144
      character path*72,upath*72,cfgroot*24
      character outs*124,outs248*248
      character doit*248,longtfile*144,longtfiledos*144
      character fs*1
      character act*16
      character LUALF*72,LCNG*15,LLOC*15
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,aimpth*24
      character ALOC*12,ALOCLBL*12,ALOCTYP*4
      character GTGEOM*72,lipvdatf*72
      character*10 wkd1, wkd2
      character*72 LCFGF,LPRJLG
      character LCTLF*72,ctldoc*248,lradcf*72
      character LAPROB*72 
      character bidirfile*72,bidirname*12
      LOGICAL unixok,concat
      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Initial assumptions.
      call ezero
      ITRC=1
      IUOUT=6
      IUIN=5
      LIMTTY=24
      LIMIT =24
      IFIL=11
      ID1=1
      IM1=1
      IT1=1
      ID2=31
      IM2=12
      IT2=24
      IDS=1
      IDF=365
      KDS=1
      KDF=24
      IUTDF=IFIL+7
      NSIGFIG=3

C Get command line parameters. The file inf is assumed to have
C been created by a 3rd party tool and includes directives as
C to the actons to be taken (this is NOT a so-called silentread
C file). The parameter act will be:
C   'NONE' if no parameter was included in the command line and
C          all steps will be taken
C   'create_silent' to parse the input and create the silentread file
C   'create_model' to parse the input file, create silentread file
C          and then generate the model
C   'create_run_model' does the above but initiates a simulation.
      call partfa(MODL,inf,act)
      IFDAY=2
      IFTIME=0
      nsset=0
      isset=0
      isstup=0
      isbnstep=1
      ispnstep=2
      issave=2
      isavgh=0
      matver=0.0   ! initial assumption of binary materials database

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

C Assume that the system configuration, multi-layered db, material db
C and control files have not been read in and that any files saved will
C be atrributed.
      CFGOK=.FALSE.
      MLDBOK=.FALSE.
      MATDBOK=.FALSE.
      CTLOK=.FALSE.
      OPTKOK=.FALSE.

c This is a console application.
      MMOD=-1

      ICOUT=IUOUT

      call edisp(IUOUT,'3rd party conversion starting...')

C Find the user's home folder then get user's custom settings.
C Make temporary use of file unit IAF=IFIL+1.
      IAF=IFIL+1
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IAF,0,IIER)

C Scan the defaults file for default configuration.
      call escdef(IAF,IER)

C Climate db is on channel IFIL (NB when not in use other binary
C databases may make use of this unit temporarily).
      ICLIM=IFIL
      LCLIM=DCLIM

      LAPRES=DAPRES

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

C Project log.
      LPRJLG='job.notes'

      INIT =0

C Take command line file name to LCMDF and use as initial climate file.
      if(inf(1:2).eq.'  '.or.inf(1:4).eq.'UNKN')then
        call edisp(iuout,'ERROR: runesp no input file specified')
        call epwait
        STOP
      else
        INTER=inf
        write(OUTS248,'(A,A)')' the input file is: ',INTER
        CALL EDISP248(IUOUT,OUTS248,80)
      endif

C Parse the input file and return an error condition.
      if(act(1:4).eq.'NONE')then

C If no action passed assume it is the same as create_run_model.
        write(act,'(a)') 'create_model'
        call parse_oc(INTER,act,ier)

C Debug.
        write(outs,'(2a)') 'runesp generated model ',LCFGF
        call edisp(iuout,outs)

C Invoke bps with -p directive.
        doit = ' '
        call isunix(unixok)
        if(unixok)then
          call addpath(LCFGF,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(LCFGF,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)

C Debug the patched file name.
          write(outs248,'(2a)') '* Corrected file ',
     &      longtfiledos(1:lnblnk(longtfiledos))
          call edisp248(iuout,outs248,100)
          longtfile=' '
          longtfile=longtfiledos
        endif
        write(doit,'(9a)') 'bps -mode text -file ',
     &    longtfile(1:lnblnk(longtfile)),
     &    ' -b no -p ann silent'

C Debug.
        call usrmsg('Begining simulation via',doit,'-')
        call runit(doit,'text')
        call usrmsg('Finished simulation.',' ','-')

        call epwait
        STOP

      elseif(act(1:13).eq.'create_silent')then
        call parse_oc(INTER,act,ier)
        call edisp(iuout,'runesp generated silentread file.')

c << create silent read file to be done. >>
C << start up prj with the silent input file...>>
        call epwait
        STOP
      elseif(act(1:12).eq.'create_model')then
        call parse_oc(INTER,act,ier)
        call edisp(iuout,'runesp generated model.')
        call epwait
        STOP
      elseif(act(1:16).eq.'create_run_model')then
        call parse_oc(INTER,act,ier)
        call edisp(iuout,'runesp generated model & annual assessment.')

C << start up bps with -p directive...>>

        call epwait
        STOP
      endif

      END

C **************** parse_oc
C parse_oc reads the contents of the input file (LFILE) and generates
C silentread data structures.
C parent action: 'create_silent' or 'create_model' or 'create_run_model'
      subroutine parse_oc(LFILE,parentaction,ier)
      integer MSZ   ! number of META zones array sizes, edit to
                    ! match MCOM in building.h
      PARAMETER (MSZ=72) 
#include "building.h"
#include "model.h"
#include "espriou.h"
#include "esprdbfile.h"
#include "geometry.h"
C espriou.h provides currentfile.

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

C Where ESP-r was installed (as recorded when it was compiled).
      common/deflt4/dinstpath
      character dinstpath*48

      common/dllerr/dllsubr,dllmesg
      character dllsubr*12,dllmesg*124  ! messages if errors in dll

C Simulation parameter presets.
      common/spfldes/spfdescr(MSPS)
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,spfdescr*8
              
      character*72 odir    ! where are folders
      CHARACTER OUTSTR*248,WORD*32,outs*124,outs2*124,outs248*248

      character LFILE*144,thefile*72,theresult*72
      integer ier
      logical unixok,dll,concat

C root   (24 char) the root name of the model
C mpath  (72 char) the path to the folder with cfg file.
C folder (12 char) model folder layout: 'single' or 'distributed'
C weather (32 char) the weather file name (no path included)
      character root*24,mpath*72,mnpath*72,folder*12
      character fs*1           ! file separator
      character weather*32     ! file name for the location (no path)
      character weathergr*36   ! file name for weather related ground temps (no path)
      character groundfile*72  ! file name for weather related gorund temps (with path)
      character subpath*72
      character parentaction*16 ! action passed from calling code
      character action*8       ! to pass to silentmodel
      character simact*6       ! to pass to silentmodel
      character longtfile*144,longtfiledos*144

C Local string values.
      character wallconstr*32,roofconstr*32,floorconstr*32
      character windowconstr*32,skylightconstr*32

C Documentation (modeltitle) comes from model.h

C Paremeters passed to:silentzone
      character use*8,szuse*8,usefile*32  ! usage pattern directives
      dimension use(3),szuse(MSZ,3),usefile(MSZ)

      character hasconstr*32,szhasconstr*32
      dimension hasconstr(MS),szhasconstr(MSZ,MS)  ! construction to use for each surface
      character hasoptic*32,szhasoptic*32
      dimension hasoptic(MS),szhasoptic(MSZ,MS)  ! optics to use for each surface

C Array of percent window area in the parent surfaces
      real glzpercent(5)

C Array of occupant, lighting, sp density.
      real densityof(3)

C Heating and cooling setpoints
      real setpoints(2)

C Run-type 0=free float control, 1=controlled
      integer runtype

C Logicals if true then there is glass in a parent surface
      logical glzin1,glzin2,glzin3,glzin4,glzin5

C Data related to locating a ground temperature file.
      logical haveground ! set true if ground temp file has been found
      integer lnw   ! length of weather string.
      integer lnwg   ! length of weather ground string.
      integer igrfile ! unit number of ground file
      integer lsn    ! length of currentfile

      integer metaver  ! one for 1.1 and two for 1.2

      IER=0

C Get the current folder and display options to the user.
C first list any files with .cfg in name.
      odir=' '
      call usrdir(odir)

C Determine operating system
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Debug.
C      write(6,*) 'usrdir is ',odir

C Set logicals for locations of windows.
      metaver =2  ! initial assumption
      glzin1 = .false.
      glzin2 = .false.
      glzin3 = .false.
      glzin4 = .false.
      glzin5 = .false.
      glzpercent(1)=0.0
      glzpercent(2)=0.0
      glzpercent(3)=0.0
      glzpercent(4)=0.0
      glzpercent(5)=0.0

      setpoints(1)=0.0
      setpoints(2)=100.0
      subpath=' '
      haveground=.false.

C For each of the possible ESP-r zones, clear values.
      do 41 iz=1,MCOM
        nztv(iz) = 0
        zname(iz) = ' '
        shape(iz) = ' '
        nbwalls(iz) = 0
        rotateit(iz,1) = 0.0
        rotateit(iz,2) = 0.0
        rotateit(iz,3) = 0.0
        rotateit(iz,4) = 0.0
        rotateit(iz,5) = 0.0
        rotateit(iz,6) = 0.0
        zorigin(iz,1) = 0.0
        zorigin(iz,2) = 0.0
        zorigin(iz,3) = 0.0
        zsize(iz,1) = 0.0
        zsize(iz,2) = 0.0
        zsize(iz,3) = 0.0
        znbglz(iz) = 0
        znbdoor(iz) = 0
        do 39 isu=1,MS
          sname(iz,isu)=' '
          zboundarytype(iz,isu,1)=0
          zboundarytype(iz,isu,2)=0
          zboundarytype(iz,isu,3)=0
          zhasglaze(iz,isu)=0.0
          zhasdoor(iz,isu)=0.0
          isznbedges(iz,isu)=0
          do 54 ivu=1,MV
            iszlist(iz,isu,ivu)=0
  54      continue
  39    continue

C Clear data for internal mass (not currently used).
        do 158 imu=1,4
          zdatamass(iz,imu,1)=0.0
          zdatamass(iz,imu,2)=0.0
          zdatamass(iz,imu,3)=0.0
          zdatamass(iz,imu,4)=0.0
          zdatamass(iz,imu,5)=0.0
          zdatamass(iz,imu,6)=0.0
          zdatamass(iz,imu,7)=0.0
          ztextmass(iz,imu,1)=' '
          ztextmass(iz,imu,2)=' '
          ztextmass(iz,imu,3)=' '
 158    continue
  41  continue

C Clear the msz array structures.
      weather=' '
      do 40 iz=1,MSZ
        szuse(iz,1) = ' '
        szuse(iz,2) = ' '
        szuse(iz,3) = ' '
        usefile(iz) = ' '
        do 52 isu=1,MS
          szhasconstr(iz,isu)=' '
          szhasoptic(iz,isu)=' '
  52    continue
  40  continue

      NS=0     ! temporary array for counting surfaces.
      nsz=1
      weather='none'

C Check if running in dll mode.
      call isadll(dll)

C Initialise data file. and set currentfile.
      IUNIT= IFIL
      CALL EFOPSEQ(IUNIT,LFILE,1,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'OC file ',LFILE(1:lnblnk(LFILE)),
     &      ' could not be opened.'
        if(dll)then
          dllsubr='parse_oc'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(iuout,outs)
          IER=1
          RETURN
        endif
      ENDIF
      write(currentfile,'(a)') LFILE(1:lnblnk(LFILE))

C Guess about a root name and folder from lfile (until
C there is more information available?) Need to do this
C in order to have a path for optional ground temperature
C file.
      call fdroot(LFILE,mnpath,thefile)
      lcfgr=lnblnk(thefile)
      lcfgl=lcfgr-3
      if(lcfgr.gt.4)then
        if(thefile(lcfgl:lcfgr).eq.'.txt')then
          write(root,'(a)') thefile(1:lcfgl-1)
        else
          write(root,'(a)') thefile(1:lcfgr)
        endif
      else
        write(root,'(a)') thefile(1:lcfgr)
      endif
      write(mpath,'(2a)') mnpath(1:lnblnk(mnpath)),
     &  root(1:lnblnk(root))

C Read lines from file in tag - data format.  Use EGETP for the
C tag in case it has spaces.
 43   CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'silent line 1',IER)
      IF(IER.NE.0)goto 1001
      K=0
      CALL EGETP(OUTSTR,K,WORD,'W','tag in oc file',IER)
      if(WORD(1:8).eq.'*climate')then
      
C The order of the logic requires that we test for the existence
C of a ground temperature file which matches the name of the
C weather file. The following logic is similar to that used in
C subroutine 
        CALL EGETRM(OUTSTR,K,weather,'W','weather name',IER)
        igrfile=IFIL+2  ! hopefully not used
        lnw=lnblnk(weather)
        write(weathergr,'(2a)') weather(1:lnw),'.gnd'
        lnwg=lnblnk(weathergr)
        write(subpath,'(4a)')
     &    dinstpath(1:lnblnk(dinstpath)),fs,'climate',fs
        write(groundfile,'(2a)') subpath(1:lnblnk(subpath)),
     &    weathergr(1:lnwg)
        write(currentfile,'(a)') groundfile(1:lnblnk(groundfile))
        call FPOPEN(igrfile,ISTAT,1,0,groundfile)
        if(ISTAT.ge.0)then
          haveground=.true.
          CALL ERPFREE(igrfile,ISTAT)
        else

C Try with the root name prepended.
          write(groundfile,'(3a)') mpath(1:lnblnk(mpath)),fs,
     &      weathergr(1:lnwg)
          write(currentfile,'(a)') groundfile(1:lnblnk(groundfile))
          call FPOPEN(igrfile,ISTAT,1,0,groundfile)
          if(ISTAT.ge.0)then
            haveground=.true.
            CALL ERPFREE(igrfile,ISTAT)
          else
  
C Try where-the-input-file is.
            write(groundfile,'(2a)') mnpath(1:lnblnk(mnpath)),
     &        weathergr(1:lnwg)
            write(currentfile,'(a)') groundfile(1:lnblnk(groundfile))
            call FPOPEN(igrfile,ISTAT,1,0,groundfile)
            if(ISTAT.ge.0)then
              haveground=.true.
              CALL ERPFREE(igrfile,ISTAT)
            endif
          endif
        endif
        goto 43
      elseif(WORD(1:7).eq.'*length')then
        CALL EGETWR(OUTSTR,K,val1,0.,0.,'-','box length',IER)
        zsize(nsz,1)=val1
        goto 43
      elseif(WORD(1:6).eq.'*width')then
        CALL EGETWR(OUTSTR,K,val1,0.,0.,'-','box width',IER)
        zsize(nsz,2)=val1
        goto 43
      elseif(WORD(1:7).eq.'*heigth'.or.WORD(1:7).eq.'*height')then
        CALL EGETWR(OUTSTR,K,val1,0.,0.,'-','box height',IER)
        zsize(nsz,3)=val1
        goto 43
      elseif(WORD(1:9).eq.'*rotate')then
        CALL EGETWR(OUTSTR,K,rot,0.,0.,'-','rotation angle',IER)
        rotateit(nsz,1)=rot
        CALL EGETWR(OUTSTR,K,rot,0.,0.,'-','rotation X',IER)
        rotateit(nsz,2)=rot
        CALL EGETWR(OUTSTR,K,rot,0.,0.,'-','rotation Y',IER)
        rotateit(nsz,3)=rot
        goto 43
      elseif(WORD(1:16).eq.'*previous_rotate')then
        CALL EGETWR(OUTSTR,K,rot,0.,0.,'-','prior rotation angle',IER)
        rotateit(nsz,4)=rot
        CALL EGETWR(OUTSTR,K,rot,0.,0.,'-','prior rotation X',IER)
        rotateit(nsz,5)=rot
        CALL EGETWR(OUTSTR,K,rot,0.,0.,'-','prior rotation Y',IER)
        rotateit(nsz,6)=rot
        goto 43
      elseif(WORD(1:10).eq.'*Roof type'.or.
     &       WORD(1:10).eq.'*roof type')then
        goto 43
      elseif(WORD(1:10).eq.'*Window(N)'.or.
     &       WORD(1:10).eq.'*window(n)')then

C A window on the North is the 3rd surface in the zone
        CALL EGETWR(OUTSTR,K,glzpercent(3),0.,0.,'-','glz surf 1',IER)
        glzin3 = .true.
        zhasglaze(nsz,3)=glzpercent(3)
        goto 43
      elseif(WORD(1:10).eq.'*Window(S)'.or.
     &       WORD(1:10).eq.'*window(s)')then

C A window on the (South 1st surface) of the box.
        CALL EGETWR(OUTSTR,K,glzpercent(1),0.,0.,'-','glz surf 1',IER)
        glzin1 = .true.
        zhasglaze(nsz,1)=glzpercent(1)
        goto 43
      elseif(WORD(1:10).eq.'*Window(E)'.or.
     &       WORD(1:10).eq.'*window(e)')then

C A window on the (East 2nd surface) of the box.
        CALL EGETWR(OUTSTR,K,glzpercent(2),0.,0.,'-','glz surf 2',IER)
        glzin2 = .true.
        zhasglaze(nsz,2)=glzpercent(2)
        goto 43
      elseif(WORD(1:10).eq.'*Window(W)'.or.
     &       WORD(1:10).eq.'*window(w)')then

C A window on the (West 4th surface) of the box.
        CALL EGETWR(OUTSTR,K,glzpercent(4),0.,0.,'-','glz surf 2',IER)
        glzin4 = .true.
        zhasglaze(nsz,4)=glzpercent(4)
        goto 43
      elseif(WORD(1:10).eq.'*Window(R)'.or.
     &       WORD(1:10).eq.'*window(r)')then

C A window on the (Roof 5th surface) of the box.
        CALL EGETWR(OUTSTR,K,glzpercent(5),0.,0.,'-','glz surf 2',IER)
        glzin5 = .true.
        zhasglaze(nsz,5)=glzpercent(5)
        goto 43
      elseif(WORD(1:5).eq.'*Roof'.or.WORD(1:5).eq.'*roof')then
        CALL EGETP(OUTSTR,K,roofconstr,'W','roof construction',IER)
        goto 43
      elseif(WORD(1:5).eq.'*Wall'.or.WORD(1:5).eq.'*wall')then
        CALL EGETP(OUTSTR,K,wallconstr,'W','wall construction',IER)
        goto 43
      elseif(WORD(1:6).eq.'*Floor'.or.WORD(1:6).eq.'*floor')then
        CALL EGETP(OUTSTR,K,floorconstr,'W','floor construction',IER)
        goto 43
      elseif(WORD(1:7).eq.'*Window'.or.WORD(1:7).eq.'*window')then
        CALL EGETP(OUTSTR,K,windowconstr,'W','window construction',IER)
        goto 43
      elseif(WORD(1:9).eq.'*Skylight'.or.WORD(1:9).eq.'*skylight')then
        CALL EGETP(OUTSTR,K,skylightconstr,'W','skyl construction',IER)
        goto 43
      elseif(WORD(1:5).eq.'*Hvac'.or.WORD(1:5).eq.'*hvac')then
        goto 43
      elseif(WORD(1:9).eq.'*Hotwater'.or.WORD(1:9).eq.'*hotwater')then
        goto 43
      elseif(WORD(1:17).eq.'*Lighting_density'.or.
     &       WORD(1:17).eq.'*lighting_density')then
        CALL EGETWR(OUTSTR,K,densityof(2),0.,0.,'-','lt density',IER)

C Debug.
C        write(6,*) 'lt density ',densityof(2)

        goto 43
      elseif(WORD(1:18).eq.'*Equipment_density'.or.
     &       WORD(1:18).eq.'*equipment_density')then
        CALL EGETWR(OUTSTR,K,densityof(3),0.,0.,'-','sp density',IER)

C Debug.
C        write(6,*) 'sp density ',densityof(3)

        goto 43
      elseif(WORD(1:17).eq.'*Occupant_density'.or.
     &       WORD(1:17).eq.'*occupant_density')then
        CALL EGETWR(OUTSTR,K,densityof(1),0.,0.,'-','ocup density',IER)

C Debug.
C        write(6,*) 'occupant density ',densityof(1)

        goto 43
      elseif(WORD(1:9).eq.'*Lighting'.or.
     &       WORD(1:9).eq.'*lighting')then
        goto 43
      elseif(WORD(1:17).eq.'*Summer_set_point'.or.
     &       WORD(1:17).eq.'*summer_set_point')then
        CALL EGETWR(OUTSTR,K,setpoints(2),0.,0.,'-','cooling setpt',IER)

C Debug.
C        write(6,*) 'cooling setpoint ',setpoints(2)

        goto 43
      elseif(WORD(1:17).eq.'*Winter_set_point'.or.
     &       WORD(1:17).eq.'*winter_set_point')then
        CALL EGETWR(OUTSTR,K,setpoints(1),0.,0.,'-','heating setpt',IER)

C Debug.
C        write(6,*) 'heating setpoint ',setpoints(1)

        goto 43
      elseif(WORD(1:9).eq.'*run_type')then

C Run type is the last item in the file. Close file and jump to processing.
        CALL EGETWI(OUTSTR,K,runtype,0,2,'W','run type',IER)

        CALL ERPFREE(IUNIT,ios)
        goto 42
      else
        goto 43
      endif

C Process the information just gathered and setup the
C rest of the common blocks.
  42  continue

C Guess about a root name and folder from lfile (until
C there is more information available?)
      call fdroot(LFILE,mnpath,thefile)
      lcfgr=lnblnk(thefile)
      lcfgl=lcfgr-3
      if(lcfgr.gt.4)then
        if(thefile(lcfgl:lcfgr).eq.'.txt')then
          write(root,'(a)') thefile(1:lcfgl-1)
        else
          write(root,'(a)') thefile(1:lcfgr)
        endif
      else
        write(root,'(a)') thefile(1:lcfgr)
      endif
      write(mpath,'(2a)') mnpath(1:lnblnk(mnpath)),
     &  root(1:lnblnk(root))

C Always create a single folder. Fill in default menu and doc.
      folder='single'
      write(modeltitle,'(a)') 'auto generation of warehouse'

C Place the origin of the model at the site origin.
      zorigin(nsz,1)=0.0
      zorigin(nsz,2)=0.0
      zorigin(nsz,3)=0.0
      znbdoor(nsz)=0

C If a standard pattern of operations can be used assign standard
C uses parameters. In this case file is written manually so lease
C szuse as blanks.
C      write(szuse(nsz,1),'(a)') 'pattern'
C      write(szuse(nsz,2),'(a)') 'all'
C      write(szuse(nsz,3),'(a)') 'allcas'
C      write(usefile(nsz),'(a)') 'warehouse.opr'

C Set the name of the new zone.
      write(zname(nsz),'(a)') 'the_space'

C Assign the intial box surface name and construction attributes.
      write(sname(nsz,1),'(a)') 'front'
      write(szhasconstr(nsz,1),'(a)') wallconstr
      write(szhasoptic(nsz,1),'(a)') 'OPAQUE'
      write(sname(nsz,2),'(a)') 'right'
      write(szhasconstr(nsz,2),'(a)') wallconstr
      write(szhasoptic(nsz,2),'(a)') 'OPAQUE'
      write(sname(nsz,3),'(a)') 'back'
      write(szhasconstr(nsz,3),'(a)') wallconstr
      write(szhasoptic(nsz,3),'(a)') 'OPAQUE'
      write(sname(nsz,4),'(a)') 'left'
      write(szhasconstr(nsz,4),'(a)') wallconstr
      write(szhasoptic(nsz,4),'(a)') 'OPAQUE'
      write(sname(nsz,5),'(a)') 'roof'
      write(szhasconstr(nsz,5),'(a)') roofconstr
      write(szhasoptic(nsz,5),'(a)') 'OPAQUE'
      write(sname(nsz,6),'(a)') 'floor'
      write(szhasconstr(nsz,6),'(a)') floorconstr
      write(szhasoptic(nsz,6),'(a)') 'OPAQUE'

C Determine how many surfaces in the model.
      write(shape(nsz),'(a)') 'box '
      nbwalls(nsz)=4
      NS=6
      if(glzin1)then
        znbglz(nsz)=znbglz(nsz)+1
        NS = NS + 1
        write(sname(nsz,NS),'(a)') 'glz_front'
        write(szhasconstr(nsz,NS),'(a)') windowconstr
        write(szhasoptic(nsz,NS),'(a)') 'TRAN'
      endif
      if(glzin2)then
        NS = NS + 1
        write(sname(nsz,NS),'(a)') 'glz_right'
        write(szhasconstr(nsz,NS),'(a)') windowconstr
        write(szhasoptic(nsz,NS),'(a)') 'TRAN'
      endif
      if(glzin3)then
        NS = NS + 1
        write(sname(nsz,NS),'(a)') 'glz_back'
        write(szhasconstr(nsz,NS),'(a)') windowconstr
        write(szhasoptic(nsz,NS),'(a)') 'TRAN'
      endif
      if(glzin4)then
        NS = NS + 1
        write(sname(nsz,NS),'(a)') 'glz_left'
        write(szhasconstr(nsz,NS),'(a)') windowconstr
        write(szhasoptic(nsz,NS),'(a)') 'TRAN'
      endif
      if(glzin5)then
        NS = NS + 1
        write(sname(nsz,NS),'(a)') 'skylight'
        write(szhasconstr(nsz,NS),'(a)') skylightconstr
        write(szhasoptic(nsz,NS),'(a)') 'TRAN'
      endif

C For the surfaces other than ground set to outside. If there was
C a ground temperature file the assume we can point to the profile
C in that file.
      do 70 isurf=1,NS
        if(isurf.eq.6)then
          if(.NOT.haveground)then
            zboundarytype(nsz,isurf,1)=4
            zboundarytype(nsz,isurf,2)=1
            zboundarytype(nsz,isurf,3)=0
          else
            zboundarytype(nsz,isurf,1)=4
            zboundarytype(nsz,isurf,2)=0
            zboundarytype(nsz,isurf,3)=1
          endif
        else
          zboundarytype(nsz,isurf,1)=0
          zboundarytype(nsz,isurf,2)=0
          zboundarytype(nsz,isurf,3)=0
        endif
  70  continue
  
C Prior to setting up the model define default calendar
      call calenmanage('i',ier)

C Test creation of new model. Note for a single folder option the
C configuration will be in /tmp/box if mpath is /tmp/box. For the
C distributed folder option the onfiguration file will be in the
C folder /tmp/box/cfg if mpath is /tmp/box. If 'within' and
C single folder then no folder is created.
      if(parentaction(1:13).eq.'create_silent')then
        action='new'
        simact='------'
        call silentmodel(action,root,mpath,folder,weather,simact,ier)
      elseif(parentaction(1:12).eq.'create_model')then
        action='within'
        simact='------'
        call silentmodel(action,root,mnpath,folder,weather,simact,ier)
      elseif(parentaction(1:16).eq.'create_run_model')then
        action='new'
        simact='------'
        call silentmodel(action,root,mpath,folder,weather,simact,ier)
      endif

C Generate the zone and zone files.
      icomp=1
      isz=1
      use(1)= szuse(isz,1)
      use(2)= szuse(isz,2)
      use(3)= szuse(isz,3)
      znbmass(isz) = 0   ! current mass not supported

C Fill surface arrays.
      do 49 isu=1,MS
        hasconstr(isu)=szhasconstr(isz,isu)
        hasoptic(isu)=szhasoptic(isz,isu)
  49  continue

      write(outs,'(2a)') 'Processing ',zname(isz)
      call edisp(iuout,outs)
      call silentzone(ICOMP,metaver,hasconstr,hasoptic,use,
     &  usefile(isz),IER)

C Overwrite the initial zone operation file with the correct
C density information.
      call setschedule(isz,zname(isz),densityof)

C Create a standard control file if runtype is 1.
      if(runtype.eq.1)then
        call  setcontrol(setpoints)
      endif
      write(outs,'(3a)') 'Processing ',zname(isz),' done.'
      call edisp(iuout,outs)

C Set values of simulation parameter set for an annual simulation.
      nsset=1    ! one set
      isstup=1   ! one day starup
      isbnstep=1 ! one building timestep per hour
      ispnstep=1  ! one system timestep per hour
      issave=4   ! save full energy balance (might reduce to 2 later)
      isavgh=0   ! save every timestep
      spfdescr(1)='ann'  ! set named ann
      isstday(1)=1
      isstmon(1)=1
      isfnday(1)=31
      isfnmon(1)=12

C Setup the name and path to the standard results file based on
C the root name of the model.
      write(theresult,'(2a)') root(1:lnblnk(root)),'.res'
      call isunix(unixok)
      if(unixok)then
        call addpath(theresult,longtfile,concat)
      else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(theresult,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)

C Debug the patched file name.
          write(outs248,'(2a)') '* Corrected file ',
     &      longtfiledos(1:lnblnk(longtfiledos))
          call edisp248(iuout,outs248,100)
          longtfile=' '
          longtfile=longtfiledos
        endif
        write(sblres(1),'(a)') longtfile(1:72)

C      write(sblres(1),'(2a)') mnpath(1:lnblnk(mnpath)),
C     &  theresult(1:lnblnk(theresult))

C Update configuration file to know about simulation parameters.
      CALL EMKCFG('-',IER)

      return

C Generate message about the line contents and the file name.
 1001 write(outs,'(3a)') 'parse_oc: conversion error in...',
     &  OUTSTR(1:50),'...'
      lsn=MIN0(lnblnk(currentfile),110)
      write(outs2,'(2a)') 'in: ',currentfile(1:lsn)
      if(dll)then
        dllsubr='SILENTREAD'
        dllmesg=outs
        ier=2
        CALL ERPFREE(IUNIT,ios)
        return
      else
        call edisp(iuout,outs)
        call edisp(iuout,outs2)
        IER=1
        CALL ERPFREE(IUNIT,ios)
        RETURN
      endif

      end

C ************** setschedule
C setschedule generates a zone operation file matching a
C fixed pattern but with occupant/lighting/sp
C densitites passed in from 3rd party file.

C The current implemented pattern is three periods in each
C day with occupents as m2 per person, lights in W/m2 and
C small power in W/m2. Occupied times are 9-21h each day.
      subroutine setschedule(ICOMP,name,densityof)
#include "building.h"
#include "model.h"
#include "espriou.h"
      COMMON/FILEP/IFIL
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth,aimpth

C Array of occupant, lighting, sp density.
      real densityof(3)
      character name*12   ! name given to the zone
      character opfil*72
      character dstmp*24
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,aimpth*24
      character fs*1
      logical unixok

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Establish the name of the zone operations file.
      iuo=ifil+2
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(OPFIL,'(A,A4)') name(1:lnblnk(name)),'.opr'
      else
        WRITE(OPFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &    name(1:lnblnk(name)),'.opr'
      endif
      CALL EFOPSEQ(IUO,OPFIL,3,IER)
      write(currentfile,'(a)') OPFIL(1:lnblnk(OPFIL))
      write(LPROJ(icomp),'(a)') OPFIL(1:lnblnk(OPFIL))

      call dstamp(dstmp)
        
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1) '*Operations 2.0'
      WRITE(IUO,'(3a)',IOSTAT=IOS,ERR=1) '*date ',dstmp,
     &  '  # latest file modification '
      WRITE(IUO,30,IOSTAT=IOS,ERR=1)
     &  name(1:lnblnk(name)),OPFIL(1:lnblnk(OPFIL))
  30  FORMAT('# operations of ',a,' defined in: ',/,'# ',a)

      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  'standard lighting/ocup/sp density assumptions'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '# control(no control of air flow  ), low mid & high setpoints'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '   0     0.000     0.000     0.000'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '     1   # no Weekday flow periods'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '# Wkd: start, stop, infil, ventil, source, data'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '   0, 24,    1.000    0.000    0    0.000'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '     1   # no Saturday flow periods'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '# Sat: start, stop, infil, ventil, source, data'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '   0, 24,    1.000    0.000    0    0.000'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '     1   # no Sunday flow periods'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '# Sun: start, stop, infil, ventil, source, data'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '   0, 24,    1.000    0.000    0    0.000'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '     9   # no Weekday casual gains'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '# Wkd: type, start, stop, sens, latent, rad_frac, conv_frac'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -1,   0,   9,',densityof(1),'       0.0, 0.200, 0.800'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -1,   9,  21,',densityof(1),'       0.0, 0.200, 0.800'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -1,  21,  24,',densityof(1),'       0.0, 0.200, 0.800'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -2,   0,   9,',densityof(2),'       0.0, 0.600, 0.400'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -2,   9,  19,',densityof(2)*0.1,'       0.0, 0.600, 0.400'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -2,  19,  24,',densityof(2),'       0.0, 0.600, 0.400'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -3,   0,   9,',densityof(3),'       0.0, 0.500, 0.500'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -3,   9,  21,',densityof(3),'       0.0, 0.500, 0.500'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -3,  21,  24,',densityof(3),'       0.0, 0.500, 0.500'

      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '     9   # no Saturday casual gains'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '# Sat: type, start, stop, sens, latent, rad_frac, conv_frac'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -1,   0,   9,',densityof(1),'       0.0, 0.200, 0.800'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -1,   9,  21,',densityof(1),'       0.0, 0.200, 0.800'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -1,  21,  24,',densityof(1),'       0.0, 0.200, 0.800'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -2,   0,   7,',densityof(2),'       0.0, 0.600, 0.400'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -2,   7,  19,',densityof(2)*0.1,'       0.0, 0.600, 0.400'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -2,  19,  24,',densityof(2),'       0.0, 0.600, 0.400'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -3,   0,   9,',densityof(3),'       0.0, 0.500, 0.500'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -3,   9,  21,',densityof(3),'       0.0, 0.500, 0.500'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -3,  21,  24,',densityof(3),'       0.0, 0.500, 0.500'

      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '     9   # no Sunday casual gains'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '# Sun: type, start, stop, sens, latent, rad_frac, conv_frac'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -1,   0,   9,',densityof(1),'       0.0, 0.200, 0.800'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -1,   9,  21,',densityof(1),'       0.0, 0.200, 0.800'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -1,  21,  24,',densityof(1),'       0.0, 0.200, 0.800'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -2,   0,   7,',densityof(2),'       0.0, 0.600, 0.400'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -2,   7,  19,',densityof(2)*0.1,'       0.0, 0.600, 0.400'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -2,  19,  24,',densityof(2),'       0.0, 0.600, 0.400'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -3,   0,   9,',densityof(3),'       0.0, 0.500, 0.500'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -3,   9,  21,',densityof(3),'       0.0, 0.500, 0.500'
      WRITE(IUO,'(A,F9.1,A)',IOSTAT=IOS,ERR=1)
     &  '   -3,  21,  24,',densityof(3),'       0.0, 0.500, 0.500'

      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  '# Labels for gain types'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &  ' Occupt Lights Equipt'
      CALL ERPFREE(IUO,ISTAT)

C Update configuration file to know about the operation file.
      CALL EMKCFG('-',IER)

      return

C Error messages.
 1    if(IOS.eq.2)then
        call usrmsg(' No permission to write operations file!',
     &            ' returning to menu...','W')
      else
        call usrmsg(' Operations file transfer error !',
     &            ' returning to menu...','W')
      endif
      end


C Run the simulation template and analyse results
C      CALL EDISP(IUOUT,'runesp running simulation(s) ...')
C      CALL RUNSIM

C Dummy routines from common3dv.F
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

      subroutine imgdisp(iforce,focus,ier)
      character focus*4
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

      SUBROUTINE PLELEV(direc)
      CHARACTER direc*1
      return
      end

***************** FNCNGR (copy block from prj.F) ************************
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

C ************** setcontrol
C setcontrol generates a zone control file matching a
C fixed pattern but with setpoints as passed in from
C a 3rd party file.

C The current implemented pattern is three periods in a
C typical day with free float outside of occupied period.
      subroutine setcontrol(setpoints)
#include "building.h"
#include "geometry.h"
#include "espriou.h"
      COMMON/FILEP/IFIL
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth,aimpth
      common/C21/IFCFG,cfgroot,LCFGF
      common/cctl/icascf(mcom)
      common/cctlnm/ctldoc,lctlf

      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,aimpth*24
      character cfgroot*24,LCFGF*72
      character LCTLF*72,ctldoc*248
      character fs*1
      logical unixok
      dimension setpoints(2)
      integer izone   ! index of the assumed zone.
      real heatcap,coolcap ! heating and cooling capacities.

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Assume control appied to 1st zone in model. 
      izone = 1
      if(ZBASEA(izone).gt.1.0)then
        heatcap= 140.0 * ZBASEA(izone)
        coolcap= 60.0 * ZBASEA(izone)
      else
        heatcap= 100000.0
        coolcap= 100000.0
      endif

C Establish the name of the control file.
      iuo=ifil+2
      LN=max(1,LNBLNK(cfgroot))
      if(ctlpth(1:2).eq.'  '.or.ctlpth(1:2).eq.'./')then
        WRITE(LCTLF,'(2a)')cfgroot(1:ln),'.ctl'
      else
        WRITE(LCTLF,'(4a)') ctlpth(1:lnblnk(ctlpth)),fs,
     &       cfgroot(1:ln),'.ctl'
      endif
      write(currentfile,'(a)') LCTLF(1:lnblnk(LCTLF))

      CALL EFOPSEQ(IUO,LCTLF,3,IER)
      WRITE(IUO,'(2A)',IOSTAT=IOS,ERR=1)
     & 'Heating to 18C and cooling to 26C during occupied period all',
     & ' days of the year.'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     & '* Building'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     & 'Heating and cooling are assumed to be fully convective.'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     & '   1  # No. of functions'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     & '* Control function'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     & '# senses the temperature of the current zone.'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     & '    0    0    0    0  # sensor data'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     & '# actuates air point of the current zone'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     & '    0    0    0  # actuator data'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     & '    1 # No. day types'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     & '    1  365  # valid Tue-01-Jan - Tue-31-Dec'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     & '     1  # No. of periods in day'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     & '    0    1   0.000  # ctl type, law (basic control), start @'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     & '      7.  # No. of data items'

C In the next line we set the heating and cooling capacity based
C on 30W/m2 of floor area.
      WRITE(IUO,'(F11.3,A,F11.3,a,2F8.3,a)',IOSTAT=IOS,ERR=1)
     &  heatcap,' 0.000 ',coolcap,' 0.000 ',setpoints(1),setpoints(2),
     &  ' 0.000'

      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     & '# Function:Zone links'
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     & ' 1'
      CALL ERPFREE(IUO,ISTAT)

c Match single zone with single control function.
      ICASCF(1)=1

C Update configuration file to know about the operation file.
      CALL EMKCFG('-',IER)

      return

C Error messages.
 1    if(IOS.eq.2)then
        call usrmsg(' No permission to write control file!',
     &            ' returning...','W')
      else
        call usrmsg(' Control file transfer error !',
     &            ' returning...','W')
      endif
      end

