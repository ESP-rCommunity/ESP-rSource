C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file contains the following subroutines.
C mzinpt - controls input of data that defines a building
C          and/or plant configuration for simulation.
C mzprec - computes time-independent zone properties.
C mzutil - reads additional zone information where active.
C usesps - passes simulation definition data in configuration file to
C          simulation controller.
C ckcurmatch - scan and report mis-matches.
 

C ******************** mzinpt ********************
C Controls input of data that defines a building and/or plant configuration
C for simulation.
C IREP sets the level of reporting/checking for use in uncertainty analyses
C where data has to be re-read silently. IREP=1 implies silent operation
C and no checking.
 
C indcfg      - configuration file index where 1 = building only, 2 = plant only 
C               and 3 = building and plant.
C sitelat     - site latitude. 
C sitelongdif - site longitudinal difference with the local reference meridian (East +ve).
C sitealt     - site altitude.
C atmpres     - atmospheric pressure, dependent on site altitude if avalable in weather file.
C patmos      - atmospheric pressure.
C extairden   - external air density, dependent on atmospheric pressure, temperature and RH.
C siteexposureindex - 1 = city centre, normal case; 2 = urban, normal case
C                     3 = rural, normal case; 4 = city centre, equal sky, ground & building views
C                     5 = city centre, building below mean height of surroundings
C                     6 = rural, isolated; 7 = totally enclosed; 8 = user defined.
C gref        - ground reflectivity.
C modeltitle  - model name.
C ncomp       - number of building zones.
C nccode      - zone code.
C lproj       - zone operations file.
C lgeom       - zone geometry file.      
C lthrm       - zone constructions file.
C ncon        - number of intra-zone connections.
C ic1 & ie1   - zone and construction number defining the start point of a connection.
C ict         - connection type:
C                     0 = to external conditions;
C                     1 = to conditions identical to the source zone;
C                     2 = to constant environmental conditions;
C                     3 = to another zone (defined by ic2 & ie2);
C                     4 = to ground;
C                     5 = adiabatic;
C                     7 = to a CEN 13791 partition.
C ic2 & ie2  - zone and construction number defining the end point of a connection:
C                     - unused if corresponding ict = 0;
C                     - temperature and incident radiation differential to apply if 
C                       corresponding ict = 1;
C                     - temperature and incident radiation to apply if corresponding ict = 2;
C                     - zone and construction numbers if corresponding ict = 3;
C                     - pointer to ground temperatureprofile if corresponding ict = 4.
C iairn      - building fluid flow simulation index (0 = none; 1 = text file;
C              2 = graphic file; 3 =3 D file) .
C icaas      - fluid (= air) flow network node associated with each building zone. Fluid
C              flow network nodes and building zones may be partially matched.
C iflwn      - plant fluid flow simulation index (o = none; 1 = on).
C icffs      - fluid flow network node associated with each plant component. Fluid
C              flow network nodes and plant components may be partially matched.
C laprob     - fluid flow description file.
C lpnam      - name of the plant system.
C npcomp     - number of plant components.
C npcdat     - plant component data.
C npmtyp     - plant system type:
C                     1 = energy balance only
C                     2 = mass balance, single phase
C                     3 = mass balance, two phases.
C npref      - plant components database pointer
C npsta      - start address for each component in the database
C nci        - number of  control variables. (For variables to be controlled, a
C              corresponding control loop must be defined.)
C cdata      - initial value for each control variable.
C npcon      - number of plant inter-component connections.
C ipc1 & ipn1 - component and node number defining receiving component (i.e. the inlet node).
C ipct       - plant component inter-connection type:
C                     1 = to identical conditions as the source component;
C                     2 = to constant and environmental conditions defined by ipc2 and ipn2;
C                     3 = to another component as defined by ipc2 and ipn2;
C                     4 = to a building zone as defined by ipc2;
C                     5 = to ambient conditions.
C ipc2 & ipn2 - component and node number defining the connected component.
C ncont      - number of plant component containment conditions. If a component has no
C              defined containment then it will have no exchange with its environment.
C ipcc       - plant component for containment.
C indcp      - index dictating the containment type (as ict above but read 'component' for 'zone').
C cntdat     - surroundings data for a plant component:
C                     if indcp = 0 - outside air used and cntdat(1) indicates any differential
C                                    to apply;
C                     if indcp = 1 - temperature of a specified plant component node; cntdat(1)
C                                    identifies the component, cntdat(2) the node and cntdat(3)
C                                    any temperature differential;
C                     if indcp = 2 - constant temperature as given by cntdat(1);
C                     if indcp = 3 - temperature of a building zone; cntdat(1) identifies the zone,
C                                    cntdat(2) the construction and cntdat(3) the node number from
C                                    'outside' (cntdat(2) and cntdat(3) both zero implies use zone
C                                    air temperature, while cntdat(3) only zero implies use of the
C                                    inside surface temperature of the specified construction.

      subroutine mzinpt(IREP,IER)
      USE AIM2_InputData, ONLY: iAIM2, LAIM2
      USE AIM2, ONLY: AIM2_READIN
      use CFC_Module , ONLY: cfcver
      IMPLICIT NONE
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "site.h"
#include "espriou.h"
#include "esprdbfile.h"
#include "material.h"
#include "plant.h"
#include "power.h"
#include "net_flow.h"
#include "dhw_common.h"
#include "MultiYear_simulations.h"
#include "UserSimulationToggles.h"
#include "climate.h"
#include "help.h"

      INTEGER, PARAMETER :: MSTMC=20
      
      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition

      common/er1/ier1
      INTEGER :: ier1
      common/trc/itrc
      INTEGER :: itrc
      common/filep/ifil
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      INTEGER :: IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      INTEGER :: icyear,icdngh
      REAL :: clat,clong
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      integer :: isd1,ism1,isd2,ism2,isds,isdf,ntstep
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

C Path to model and command line file name.
      common/rpath/path
      common/rcmd/LCMDFL
      common/c6/indcfg
      INTEGER :: indcfg

      common/afn/iairn,laprob,icaas(mcom)
      INTEGER :: iairn,icaas
      common/ffn/iflwn,icffs(mpcon)
      INTEGER :: iflwn,icffs
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      CHARACTER*72 LCFD
      integer :: IFCFD
      common/ACTDOM/CFDOK
      logical CFDOK

      real CFTIMS,CFTIMF    ! start and finish times for CFD assessment
      integer ICFDYS,ICFDYF ! julian day-of-year at start and end of CFD
      integer ICFAUX  ! helps to keep track of IFCFD()
      integer ICFVIEW ! toggle for preview via dfv during assessment
      integer ICFVIEWMINT,ICFVIEWMAXT  ! range of temperatures for dfv
      logical DFVinvoked
      COMMON/CFSEUP/CFTIMS,CFTIMF,ICFDYS,ICFDYF,ICFAUX(MCOM),ICFVIEW,
     &  ICFVIEWMINT,ICFVIEWMAXT,DFVinvoked

      common/prec7/itcnst
      INTEGER :: itcnst
      COMMON/DNORGH/IRTYPE
      INTEGER :: irtype
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

C Temporal definition commons.
C For each timestep (assuming 10tsph frequency over a year) iknowrec
C will be zero if not yet set and a positive number representing the record
C to read from the temporal file if already known.
      integer iknowrec,itdflastrec,itdfprevrec
      common/tdfhash/iknowrec(87600,4),itdflastrec,itdfprevrec

      INTEGER :: ispmxist

C Defaults.
      character*96 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF
      COMMON/GR1D04/GAM,RGAM
      REAL :: GAM, RGAM
      COMMON/GR3D100/BLDG3D,ZONE3D(MCOM)
      LOGICAL :: BLDG3D,ZONE3D
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      INTEGER :: icvs,icnc,indc,indd,itaq,itlw,ilwv,itf3
      COMMON/GRND100/GRND3D
      LOGICAL GRND3D
      COMMON/GRND101/NNODTG,ITCNSTG,TEMP3G(MCOM),AREAGD(MCOM)
      INTEGER :: NNODTG,ITCNSTG
      REAL :: TEMP3G,AREAGD
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      LOGICAL MSTROK,MSTRZN
      COMMON/GRSD100/IndxSt
      INTEGER :: IndxSt
      COMMON/GRSD101/LGrdSt

C Uncertainty common.
      COMMON/UA5/IMET,ISIM,NRSIM
      INTEGER :: imet,isim,nrsim

C Simulator parameter.
      COMMON/SPFL/spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok
      LOGICAL spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok
      common/SPFLN/spsname
      COMMON/Vld30/Tinitial
      REAL :: Tinitial
      common/dllerr/dllsubr,dllmesg

C Flag indicating presence of HVAC models:
C ihvacflag=1 indicates HVAC models are active; 0 indicates no HVAC models.
      common/hvacinfo/ihvacflag,hvacfile
      INTEGER :: ihvacflag

C Mechanical Ventilation systems.
      Integer iMech
      Character LMech*72
      common/mechvdef/iMech,LMech

C For reading in of bi-directional data.
      COMMON/BIDIRFL/bidirfile,bidirname(MSTMC)

C Toggle for extreme silent running (endless) mode.
      logical endless,issilient
      common/endlessmode/endless,issilient

      CHARACTER*72 LGrdSt

      CHARACTER OUTSTR*124,MODE*4
      CHARACTER outs*124,louts*248,spsname*30,L144*144
      CHARACTER outs248*248,LCMDFL*144
      character*72 laprob,path
      character dllsubr*12,dllmesg*124,bidirfile*72,bidirname*12

C HVAC AIM-2
      character hvacfile*72

      LOGICAL OK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,XST,CFCDBOK
      logical dll,closemat1,closemat2,unixok,concat
      logical closecfc1,closecfc2

C GSHP
C Common holding flag indicating presence of gshp data input file:
C igshp = 1 there is a gshp file; igshp = 0 no gshp file.
      common/gshpinfo/igshp
      INTEGER :: igshp

C GCEP
C Common holding flag indicating presence of gcep data input file:
C igcep = 1 there is a gcep file; igcep = 0 no gcep file.
      common/gcepinfo/igcep
      INTEGER :: igcep

C Function determining if HOT3000 features enabled.
      logical bH3KExtentionsActive
      
C Flag to enable inter-domain iteration 
      common / Plt_Elec_Net / bInter_domain_iteration
      logical bInter_domain_iteration

C Status of multi-year weather database.
      logical bMY_clm_err 
      integer lndbp   ! for length of standard database path
      character lltmp*144  ! to re-create plant database file name with path
      character lworking*144,fs*1,llclmdb*144

      integer IClmErr  ! the error state for climate file opening
      integer ier, irep, j
      
      REAL :: clmloc
      INTEGER :: i,iaprob,iglib,irec,istat,itru,iua,llt,nclass
      integer :: loop
      real :: rtmp
      integer ISTRW

C Atmospheric pressure and density.
      REAL PRESSURE_ALT,AIR_DENSITY

      helpinsub='input'  ! set for cfiles

C Reset error indicator and check if running as a dll.
      ier1=0
      call isadll(dll)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Initialize the system variables.
      GAM=0.5
      fUserGAM = GAM
      BLDG3D=.FALSE.
      GRND3D=.FALSE.
      MSTROK=.FALSE.
      IndxSt=0
      LGrdSt=' '
      Tinitial=15.0

C If an input file has been specified then load it after confirmation
C and return to the main menu.
      helptopic='model_cfg_file_name'
      call gethelptext(helpinsub,helptopic,nbhelp)
 42   if (.NOT.(ISIM.gt.1.or.autook)) then
        write(L144,'(a)') LCMDFL(1:lnblnk(LCMDFL))
        llt=lnblnk(LCMDFL)

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
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
        CALL EASKF(L144,' ','Model configuration file?',ISTRW,DFCFG,
     &    'config file name',IER,nbhelp)

        if(L144(1:2).ne.'  '.and.L144(1:4).ne.'UNKN')then
          write(LCMDFL,'(a)') L144(1:lnblnk(L144))
        else
          goto 42
        endif
      endif
      INQUIRE (FILE=LCMDFL,EXIST=XST)
      if(XST)then

C Find the path and local file name.
        call fdroot(LCMDFL,path,LCFGF)

C Use supplementary temporary unit number for IAPROB.
        IAPROB=IFIL+22
        MODE='ALL '
        if(endless.or.issilient)then
          continue
        else
C          call usrmsg(' ','Scanning model description ...','-')
        endif
        CALL ERSYS_mmode(LCFGF,ifcfg,IAPROB,MODE,ITRC,IER,autook)
        if(endless.or.issilient)then
          continue
        else
C          call usrmsg(' ','Scanning model description ... done.','-')
        endif

C Building and plant description now complete.
        if(ier.ne.0)then
          if(dll)then
            if(ier.eq.2)then
              return
            else
              dllsubr='mzinput'
              dllmesg='Configuration file read problem!'
              ier=2
              return
            endif
          else
            helptopic='model_cfg_warning'
            call gethelptext(helpinsub,helptopic,nbhelp)
            ! Don't ask to rescan model if running in HOT3000 mode
            if ( .not. bH3KExtentionsActive() )then
              CALL EASKOK('Model load problem!',
     &        'rescan?',OK,nbhelp)
              if(OK)goto 42
            endif
          endif
        endif
      else
        if(dll)then
          dllsubr='mzinput'
          dllmesg='Model configuration file does not exist!'
          ier=2
          return
        else
          call usrmsg('Model configuration file does not exist!',
     &     'Please retry. ','W')
          goto 42
        endif
      endif
      CFGOK=.TRUE.

C Use any simulation parameters in the configuration file.
      if(spsname(1:2).ne.'  '.and.spsname(1:4).ne.'UNKN')then
        call usesps(spsname,ier)
        if(ier.eq.2)then
          return
        endif
      else

C Loop through zones to see if there is a cfd domain description.
C If so set default CFD assessment timing assumptions. These
C will need to be updated once the user specifies the building
C simulation period manually. Signal this by setting both ICFDYS
C and ICFDYF to zero so building simulation period is suggested
C in simcon.F
        do loop=1,NCOMP
          IF(ABS(IFCFD(loop)).GT.0) CFDOK=.true.
        enddo
        if(CFDOK)then
          ICFDYS=0
          ICFDYF=0
          CFTIMS=1.0
          CFTIMF=23.99  ! start and finish times for CFD assessment
          DFVinvoked=.false.
        endif
      endif

C Time-independent zone data (unless plant only).
      if(indcfg.ne.2) then
        call mzprec(IREP,IER)
        if(ier.eq.2)then
          return
        endif
        if(iflwn.ge.1.and.iairn.eq.0) call mfcdat
      else
        if(iflwn.eq.1) call mfcdat
      endif

C HVAC.
      if(ihvacflag.eq.1) then
        call HVACINPUT(hvacfile)
      endif

C AIM-2.
      if(iAIM2.eq.1)then
        call AIM2_READIN
      endif

c DHW.
       if( iDHW_Flag.eq.1)then
        call DHW_READIN
       endif

C Mechanicanical ventilation.
      if(iMech.eq.1)then
        call MECH_VENT_READIN ( )
      endif

C GSHP.
      if(igshp .eq. 1)then
        call GSHPINPUT
      endif

C GCEP.
      if(igcep .eq. 1)then
        call GCEPINPUT
      endif

C Fluid flow simulation parameters if fluid flow network defined.
      if(iairn.ge.1.or.iflwn.eq.1) call mflwsu

C Unless building only, plant matrix template setup & checks.
C If plant component db file unit not set use iaprob. Work
C with lltmp because the standard database path might be pre-pended.
      if(indcfg.ne.1) then
        if(ipcdb.eq.0)ipcdb=IAPROB
        lndbp=lnblnk(standarddbpath)
        if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
          lltmp=LPCDB
        elseif(ipathpcdb.eq.2)then
          write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &      lpcdb(1:lnblnk(lpcdb))  ! prepend db folder path
        endif
        write(currentfile,'(a)') lltmp(1:lnblnk(lltmp))
        ier=0
        call EFOPRAN(ipcdb,lltmp,20,1,IER)
        call mzpmxt(ipcdb)

C Default flag for plant<->electrical network interdomain iteration
C to false. Plant coponents requiring this facility will set flag
C to true. Note: this code is not required in the stand-alone 
C plt simulator since, by definition, it doesn't support the
C electrical network.
        bInter_domain_iteration = .false.

C Plant manufacturer's and/or static data & plant template checks.
        call mzsdat
      endif

C If the power flow simulation flag is set then set up the electrical network.
      IF(IENTXIST.GT.0) CALL ENETSETU

C Advanced albedo model: pre-process snow depth file. The ascii file is
C read and converted to a binary file
      IF (groundreflmodel.EQ.3) THEN
        CALL PPGREF(IER)
        IF (IER.NE.0) RETURN
      ENDIF

C Report specified start-up period ITRC in verbose mode.
      write(outs,'(a,I3,a)')
     &     'A systems and building simulation start-up period of ',
     &     ITCNST,' days has been specified.'
      if(endless.or.issilient)then
        continue
      else
        if(itrc.gt.1)call edisp(iuout,outs)
      endif

      IF(GRND3D)THEN

C Specify ground simulation start-up period.
        ier=0
        CALL EFOPRAN(ICNC,LGDCNC,7,1,IER)
        READ(ICNC,REC=1,IOSTAT=ISTAT,ERR=7)ITCNSTG
        CALL ERPFREE(ICNC,ISTAT)
        helptopic='model_startup_period'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if (IREP.eq.1) then
          write(OUTS,'(a,I4,a)')
     &      ' Ground simulation start-up period is',ITCNSTG,' days.'
          call EDISP(IUOUT,OUTS)
        else
          WRITE(OUTS,'(2A)')'Number of days for ground',
     &                      'simulation start-up?'
          CALL EASKI(ITCNSTG,' ',OUTS,0,'F',99999,'W',1,
     &      'startup',IER,nbhelp)
        endif
      ENDIF

C Free plant components database if opened.
      if(ipcdb.ne.0) call erpfree(ipcdb,istat)

C If materials db not yet defined has not yet been opened do so.
C Use the unit IFMAT=IFIL+5 for this via common condb.
C << this logic could be updated to use MATFROMBIN as in ascii_mat.F >>
      IF(.NOT.MATDBOK)THEN
  55    write(currentfile,'(a)') LFMAT(1:lnblnk(LFMAT))

        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          ier=0
          call EFOPRAN(IFMAT,LFMAT,40,1,IER)
        elseif(ipathmat.eq.2)then
          ier=0
          lndbp=lnblnk(standarddbpath)
          write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &      LFMAT(1:lnblnk(LFMAT))
          call EFOPRAN(IFMAT,lworking,40,1,IER)
        endif

        if(ier.ne.0)then
C Database was not found, or file could not be read.
          if(dll)then
            dllsubr='mzinpt'
            dllmesg='Problem opening materials database!'
            ier=2
            return

C In interactive mode, prompt user for new database.
          elseif ( .not. autook ) then

            IER=0
            helptopic='materials_db_not_found'
            call gethelptext(helpinsub,helptopic,nbhelp)
            write(lltmp,'(a)') LFMAT(1:lnblnk(LFMAT))
  56        CALL EASKS(lltmp,' ','Material database?',
     &              144,DFCON,'materials db file name',IER,nbhelp)
            if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then
              write(LFMAT,'(a)') lltmp(1:lnblnk(lltmp))
              goto 55
            else
              goto 56
            endif
          else

C In silent running mode, notify user and terminate simulation.
            call UsrMsg('Materials database not found!',
     &                  ' ', 'F')
          endif ! <- matches 'if(ier.ne.0)then'
        else

C Also check to see if the file has correct info (using pattern of
C code from esruprj/eddb.F subroutine opendb.
          IREC=601
          READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1006)NCLASS
          if(NCLASS.LE.0)then
            goto 1006   ! no classifications so try alternatives
          else
            matver = 0.0
            MATDBOK=.TRUE.
            goto 139  !  a binary file found so carry on.
          endif
 1006     CALL ERPFREE(IFMAT,ISTAT)  ! if a read error look at alternatives.
          write(currentfile,'(a)') LFMAT(1:lnblnk(LFMAT))

          if(ipathmat.eq.0.or.ipathmat.eq.1)then
            call rascimat(IFMAT,LFMAT,IER)  ! try current ascii format
          elseif(ipathmat.eq.2)then
            lndbp=lnblnk(standarddbpath)
            write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &        LFMAT(1:lnblnk(LFMAT))
            call rascimat(IFMAT,lworking,IER)  ! try current ascii format
          endif

          if(ier.eq.-2)then
            ier=0
            if(ipathmat.eq.0.or.ipathmat.eq.1)then
              call rlegacymat(IFMAT,LFMAT,ier)  ! try older ascii format
            elseif(ipathmat.eq.2)then
              lndbp=lnblnk(standarddbpath)
              write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &          LFMAT(1:lnblnk(LFMAT))
              call rlegacymat(IFMAT,lworking,ier)  ! try older ascii format
            endif

            if(ier.ne.0)then
              if(dll)then
                dllsubr='mzinpt'
                dllmesg='Problem opening materials database!'
                ier=2
                return
              else
                call usrmsg('Materials database not',
     &            'found or file was corrupt!','W')
              endif
            else
              call eclose(matver,1.1,0.001,closemat1)
              call eclose(matver,1.2,0.001,closemat2)
              if(closemat1.or.closemat2) MATDBOK=.TRUE.
            endif
          elseif(ier.eq.0)then
            call eclose(matver,1.1,0.001,closemat1)
            call eclose(matver,1.2,0.001,closemat2)
            if(closemat1.or.closemat2) MATDBOK=.TRUE.
          endif
        endif
      ENDIF

      IF(.NOT.CFCDBOK)THEN
  57    write(currentfile,'(a)') LCFCDB(1:lnblnk(LCFCDB))

        if(ipathcfc.eq.0.or.ipathcfc.eq.1)then
          ier=0
          call EFOPRAN(ICFCDB,LCFCDB,40,1,IER)
        elseif(ipathcfc.eq.2)then
          ier=0
          lndbp=lnblnk(standarddbpath)
          write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &      LCFCDB(1:lnblnk(LCFCDB))
          call EFOPRAN(ICFCDB,lworking,40,1,IER)
        endif

        if(ier.ne.0)then
C Database was not found, or file could not be read.
          if(dll)then
            dllsubr='mzinpt'
            dllmesg='Problem opening CFClayers database!'
            ier=2
            return

C In interactive mode, prompt user for new database.
          elseif ( .not. autook ) then

            IER=0
            helptopic='CFClayers_db_not_found'
            call gethelptext(helpinsub,helptopic,nbhelp)
            write(lltmp,'(a)') LCFCDB(1:lnblnk(LCFCDB))
  58        CALL EASKS(lltmp,' ','CFClayers database?',
     &             144,DCFCDB,'CFClayers db file name',IER,nbhelp)
            if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then
              write(LCFCDB,'(a)') lltmp(1:lnblnk(lltmp))
              goto 57
            else
              goto 58
            endif
          else

C In silent running mode, notify user and terminate simulation.
            call UsrMsg('CFClayers database not found!',
     &                  ' ', 'F')
          endif ! <- matches 'if(ier.ne.0)then'
        else

          CALL ERPFREE(ICFCDB,ISTAT)  ! if a read error look at alternatives.
          write(currentfile,'(a)') LCFCDB(1:lnblnk(LCFCDB))

          if(ipathcfc.eq.0.or.ipathcfc.eq.1)then
            call rascicfc(ICFCDB,LCFCDB,IER)  ! try current ascii format
          elseif(ipathcfc.eq.2)then
            lndbp=lnblnk(standarddbpath)
            write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &        LCFCDB(1:lnblnk(LCFCDB))
            call rascicfc(ICFCDB,lworking,IER)  ! try current ascii format
          endif

          if(ier.eq.0)then
            call eclose(cfcver,1.1,0.001,closecfc1)
            call eclose(cfcver,1.2,0.001,closecfc2)
            if(closecfc1.or.closecfc2) CFCDBOK=.TRUE.
          endif
        endif      
      ENDIF

C Read the multilayer construction database so that it's
C data may be accessed by control and reporting facilities.
C Use file unit IAPROB to read db.
 139  IFMUL=IAPROB
      CALL ERPFREE(IFMUL,ISTAT)
      if(ipathmul.eq.0.or.ipathmul.eq.1)then
        lworking=lfmul  ! use as is
      elseif(ipathmul.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    lfmul(1:lnblnk(lfmul))  ! prepend db folder path
      endif
      call FINDFIL(lworking,XST)
      if(.NOT.XST)then
        write(outs248,'(2A)') 'Could not find construction database!',
     &    LFMUL(1:lnblnk(LFMUL))
        call isadll(dll)
        if(dll)then
          dllsubr='mzinpt'
          dllmesg=outs
          ier=2
          return
        else
          call lusrmsg(outs248,' ','W')
        endif
      else
        write(currentfile,'(a)') lfmul(1:lnblnk(lfmul))
        CALL ERMLDB(0,iuout,IER)
        IF(IER.eq.4)THEN
          CALL ERMLDB2(0,iuout,IER)
          if(IER.eq.0)then
            MLDBOK=.TRUE.
          else

C Note: the databases are not actually referenced by bps. This is a non-
C       critical error --- warn user and clear error flag
            call usrmsg(outs,' ','W')
            ier = 0 
          endif
        ELSEIF(IER.EQ.1.or.IER.eq.2.or.IER.eq.3)THEN
          outs='Problem reading construction database!'
          if(dll)then
            dllsubr='mzinpt'
            dllmesg=outs
            ier=2
            return
          else

C Note: the databases are not actually referenced by bps. This is a non-
C       critical error --- warn user and clear error flag
            call usrmsg(outs,' ','W')
            ier = 0 
          endif
        else
          MLDBOK=.TRUE.
        endif
      endif

C If a temporal file has been referenced in the configuration file
C then process it, creating a scratch file for use during the
C simulation and filling the TDFFLG2 commons.
      IF(ITDFLG.ne.0)THEN
        call supplyandcheck(ltdfa,'S',ier)
        if(IER.NE.0)then
          outs='Problem opening temporal file, aborting!'
          if(dll)then
            dllsubr='mzinpt'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,' ')
            call edisp(iuout,outs)
            ier=2
            return
          endif
        endif

C Set iknowrec array to signal temporal has not been scanned.
C Documentation of values found in subroutine RCTDFB.
        do 2 I=1,87600
          iknowrec(I,1)=0
          iknowrec(I,2)=0
          iknowrec(I,3)=0
          iknowrec(I,4)=0
          itdflastrec=0
          itdfprevrec=0
 2      continue
      ENDIF
  
 35   if ( bSY_climate_defined ) then
 
C Check if the weather file in the configuration file
C is correct. If not, warn user .

C Collect & process single-year weather file.
C Note that this file uses a different error flag than
C the global model definition as it can be specified separately.
         CALL CLMOPB(0,IClmErr)
         IF(iClmErr.eq.0)THEN
            CALL CLMRDBMD(IClmErr)           
         ELSE

C Set weather definition flag to false .
            bSY_climate_defined = .false.
            if(dll)then

C Kill simulation if in DLL mode.           
               dllsubr='mzinpt'
               dllmesg='Problem opening weather file!'
               ier=2
               return
            else

C Warn the user that the weather file definition has failed. 
               write(louts,'(A,A,A)')
     &           'Warning: specified weather file (',
     &           LCLIM(1:LNBLNK(LCLIM)), ') not found!'   
               call edisp248 (iuout,louts,100)
               if ( autook ) then

C If running silently, stop.
                  call edisp248(iuout,louts,100)               
                  close(ieout)
                  CALL ERPFREE(ieout,ISTAT)
                  call pauses(3)   ! pause for 3 seconds then abort program.
                  call epagend
                  stop
               else
C If running interactively, warn user and free file channel.
                  call usrmsg(outs,' ','W')
               endif
            endif
         ENDIF

C Close & free file (it will be opened again before the start
C of the simulation         
         CALL ERPFREE(ICLIM,ISTAT)
      endif 

      if ( bMY_climates_defined ) then

C Check status of multi-year climate file: parse file.
         call parse_MY_clm_db ( ICLIM, autook, bMY_clm_err)

C Check validity of records.
         if ( .not. bMY_clm_err )
     &           call check_MY_clm_db ( autook, 'fix', bMY_clm_err )
         if ( bMY_clm_err ) then
            bMY_climates_defined = .false.
         else
            bMY_climates_defined = .true.
         endif 
      
      endif      ! <- matches " if ( bSY_climate_defined ) ..."

C Open and scan the bi-directional data file if it exists.
      if(lnblnk(bidirfile).eq.0)then
        continue
      elseif(bidirfile(1:7).eq.'UNKNOWN')then
        continue
      else
        iua=IAPROB
        call erbiwin(iuout,iua,bidirfile,ier)
        if(ier.eq.0)then
          call edisp(itru,'Bi-directional data sucessfully scanned')
        else
          call edisp(itru,'Problem reading the bi-directional data.')
        endif
      ENDIF

C Call routine to establish priority between overlapping airflow 
C computation methods (e.g. AIM-2/airflow network) 
      call AirFlowInitializePriority()      

C Calculate default external air pressure and air density based on site altitude
C for case where atmospheric pressure is not available in weather file
C (atmpres is in Pa, patmos is in mbar).
      atmpres = PRESSURE_ALT(sitealt)
      patmos=atmpres/100.

C For default air density, assume 0C and 50% RH.
C This will be recalculated as needed at prevailing conditions.
      extairden=AIR_DENSITY(patmos,0.0,50.0)

      return

C Error.
  103 if(dll)then
        dllsubr='mzinpt'
        dllmesg='Problem opening weather file.'
        ier=2
        return
      else
        helptopic='problem_opening_climate'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK('Problem opening weather file!',
     &              'retry?',OK,nbhelp)
        IF(OK)THEN
          goto 35
        ELSE
          call pauses(2)   ! pause for 2 seconds then abort program
          close(ieout)
          CALL ERPFREE(ieout,ISTAT)
          CALL ERPFREE(ICLIM,ISTAT)
          CALL EPAGEND
          call epagend
          STOP
        ENDIF
      endif

    7 if(dll)then
        dllsubr='mzinpt'
        dllmesg='Problem at first record of ground connections file.'
        ier=2
        return
      else
        CALL USRMSG('Error occured at the first record ',
     &              'in the ground connections file.','W')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call pauses(2)   ! pause for 2 seconds then abort program
        CALL EPAGEND
        STOP
      endif

      write(outs,'(A,I3)') ' mzinpt: climate file error @ rec ',irec
      call edisp(iuout,outs)
      call epwait
      return
      end

C ******************** mzprec ********************
C Initiates computation of time-independent zone
C properties, checks the legality of all type 3 connections and
C checks if user wants to re-specify simulation start-up period.
C IREP=1 implies no reporting or data checking.

      SUBROUTINE MZPREC(IREP,IER)
#include "building.h"
#include "model.h"
#include "site.h"
#include "schedule.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/WARN/IWARN
      COMMON/WRNOFF/IWMOFF
      COMMON/FILEP/IFIL

      common/trc/itrc
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      INTEGER :: ic1,ie1,ict,ic2,ie2
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      INTEGER :: IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

C During setup scan for inbuilt and user defined ground temperatures.
C igttype: if > 1 then index points to most often used standard profile
C         if < 1 then index points to most often used user profile
C         if = 0 then use compiled pattern.
      common/C25/igttype,GTMP(12)

      COMMON/PREC7/ITCNST
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
      COMMON/PREC13/C(MCOM,MS,MN,2),QC(MCOM,MS,MN)
      COMMON/PREC14/emarea(MCOM)

      COMMON/VTHP30/ILTHPS,ILTHPZ(MCOM)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR3D100/BLDG3D,ZONE3D(MCOM)
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      LOGICAL MSTROK,MSTRZN
      COMMON/MOIST02/LMOIST(MCOM)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)

C Uncertainty common.
      COMMON/UA5/IMET,ISIM,NRSIM
      common/spmfxst/ispmxist,spflnam 

C Thermal bridge common block is described in esruprj/edgeo.F.
      integer nbrdg, ibrdg
      real psi,lngth,losspercent,totheatloss,thbrpercent
      common/THRBRDG/nbrdg(MCOM),psi(MCOM,16),lngth(MCOM,16),
     &               ibrdg(MCOM,16),losspercent(MCOM),totheatloss(MCOM),
     &               thbrpercent,uavtotal(MCOM)
      real uavtotal   

C Dll mode error subroutine and error details.
      common/dllerr/dllsubr,dllmesg

C Simulator parameters.
      COMMON/SPFL/spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok
      LOGICAL     spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok

C Toggle for extreme silent running (endless) mode.
      logical endless,issilient
      common/endlessmode/endless,issilient

C Uncertainties
      character USETNAME*96   ! the synopsis for each MC-run
      character uheader*1000  ! header including name of each uncertainty changed
      character urunvals*1000 ! attribute changed-to value of each uncertainty
                              ! change for the run.
      COMMON/USETN/USETNAME(MNRS),uheader,urunvals

C Logical flag for uncertainty header has been printed.
      logical uncertheadprinted
      common/uncerthead/uncertheadprinted
      logical headerdone
      logical itemunique  ! should we include in fort.37
      integer itemsinheader,nbpastitems,nbdatahits
      character pastphrase*42
      common/uhead/headerdone,itemunique(400),pastphrase(400),
     &  itemsinheader,nbpastitems,nbdatahits

      CHARACTER*72 LMOIST,LMSTR
      character outs*124,CXSTR*78,spflnam*72
      character dllsubr*12,dllmesg*124
      LOGICAL BLDG3D,ZONE3D
      LOGICAL ILTHPS,ILTHPZ,CLOSE
      logical dll,prob
      dimension igarray(mgrdp),iguarray(12)

      if (.not.startupok) ITCNST=0
      IWARN=0
      call isadll(dll)

C Zone-by-zone. Use IUNIT for geometry file.
      IUNIT=IFIL+1
      NNDS=0
      MNDS=0

C Clear igarray() for inbuilt ground temps and iguarray()
C for user defined ground temps. 
      igttype=0
      do 9 ij=1,mgrdp
        igarray(ij)=0
        if(ij.le.12) iguarray(ij)=0
  9   continue
      if(endless.or.issilient)then
        continue
      else
C        call usrmsg(' ','Checking zone descriptions.','-')
      endif
      ILTHPS=.FALSE.
      BLDG3D=.FALSE.
      MSTROK=.FALSE.
      DO 10 I=1,NCOMP
        IZ=I
        MNDZ(IZ)=0

C Read contents of the I'th zone data files.
        CALL EROPER(ITRC,ITU,IUNIT,IZ,IER)
        if(ier.eq.1)then
          write(outs,'(a,i2,a)') 
     &        'Error reading operation file for zone ',IZ,'.'
          if(dll)then
            dllsubr='MZPREC'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            close(ieout)
            CALL ERPFREE(ieout,ISTAT)
            call pauses(3)   ! pause for 3 seconds then abort program
            CALL EPAGEND
            STOP
          endif
        endif

C If a non-sorted operations file has been read warn user and
C translate the casual gains.
        if(ip3ver(iz).eq.0)then
          write(outs,'(a,i2,a)') 
     &      'Operation file for zone ',IZ,' periods being sorted!'
          call edisp(iuout,outs)
          call PROCESSOLDCAS(IZ,itrc,itu)
        endif

C Zeroise thermal bridge heat loss before (possibly) reading from geometry file.
        totheatloss(IZ)=0.0

        call georead(IUNIT,LGEOM(IZ),IZ,1,ITU,IER)
        if(ier.eq.1)then
          write(outs,'(a,i2,a)') 
     &        'Error reading geometry file for zone ',IZ,'.'
          if(dll)then
            dllsubr='MZPREC'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            close(ieout)
            CALL ERPFREE(ieout,ISTAT)
            call pauses(3)   ! pause for 3 seconds then abort program
            CALL EPAGEND
            STOP
          endif
        endif

        CALL ECONST(LTHRM(IZ),IUNIT,IZ,ITRC,ITU,IER)
        if(ier.eq.1)then
          write(outs,'(a,i2,a)') 
     &        'Error reading construction file for zone ',IZ,'.'
          if(dll)then
            dllsubr='MZPREC'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            close(ieout)
            CALL ERPFREE(ieout,ISTAT)
            call pauses(3)   ! pause for 3 seconds then abort program.
            CALL EPAGEND
            STOP
          endif
        endif

C Open any timestep based utility files.
        CALL MZUTIL(IZ,IER)
        if(ier.eq.2.and.dll)then
          return
        endif
        IF(ZONE3D(IZ))BLDG3D=.TRUE.

C Alter properties for uncertainty analysis. 
        if (ISIM.gt.1) then
          if (IMET.eq.1) then
            call DSA(IZ)
          elseif (IMET.eq.2) then
            call FCTA(IZ)
          elseif (IMET.ge.3) then
            call MCSA(IZ)
          endif
        endif

C Initiate computation of all time-independent zone properties.
        if (ISIM.gt.1) then
          continue
        else
          CALL MZAREA(IZ)
        endif

        CALL MZVOLM(IZ)
        CALL MZPAZI(IZ)
        IF(IVF(IZ).EQ.0)CALL MZVFCL(IZ)
        CALL MZVENC(IZ)
        CALL MZCASG(IZ)
        
C If the start-up period has been specified in a simulator parameter 
C file then do not recalculate it.
        if(.not.startupok) CALL MZTCON(IZ)
        CALL MZMISC(IZ)
        IF(MSTRZN(IZ))THEN
          MSTROK=.TRUE.
          LMSTR=LMOIST(IZ)

C Read moisture file.
          CALL EMSTRR(LMSTR,IUNIT,IZ,ITRC,ITU,IER)
        ENDIF

C Keep track of the total surface area/emisivity for use
C in apportioning casual and plant radiation to surfaces.
C Also instantiate izsfloor and izsceil.
        emarea(iz)=0.0
        izsfloor(iz)=0
        izsceil(iz)=0
        do 80 kk=1,nconst(iz)
          emarea(iz)=emarea(iz)+sna(iz,kk)*ei(iz,kk)
          if(SPELV(iz,kk).GE.89.98.AND.SPELV(iz,kk).LE.90.02)then
            izsceil(iz)=kk
          endif
          if(SPELV(iz,kk).GE.-90.02.AND.SPELV(iz,kk).LE.-89.98)then
            izsfloor(iz)=kk
          endif
  80    continue

C Assign two nodes per layer and setup the coefficients. This should
C catch any thermophysical changes made via MCSA.
        CALL NODTWO(IZ)
        NNDS=NNDS+NNDZ(IZ)
        CALL MZCOE1(IZ)
        IF(ICGC(IZ).EQ.1)CALL DAYFAC(IZ)
   10 CONTINUE
      if(endless.or.issilient)then
        continue
      else
C        call usrmsg(' ','Zone descriptions check completed.','-')
      endif
      if (IREP.eq.1) return

C Check connections between type 3 surfaces to ensure
C no mismatch.
      IW=IWARN
      IWARN=0
      if(endless.or.issilient)then
        continue
      else
C        call usrmsg(' ','Checking model topology.','-')
      endif
      call ckcurmatch(prob,iprob)
      if(prob.and.iprob.gt.1)then
        outs='More than one problem with model contiguity.'
        if(dll)then
          dllsubr='MZPREC'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(iuout,outs)
          IWARN=IWARN+iprob
        endif
      endif
      DO 20 I=1,NCON
        NZ1=IC1(I)
        NS1=IE1(I)
        NZ2=IC2(I)
        NS2=IE2(I)

C Check for ground temperature profiles and keep track of
C which ones are most often used.
        if(ICT(I).eq.4)then
          if(NZ2.gt.0)then
            igarray(NZ2)=igarray(NZ2)+1
          elseif(NZ2.eq.-3)then
            continue
          else
            iguarray(NS2)=iguarray(NS2)+1
          endif
        endif
        IF(ICT(I).NE.3)goto 20

C Make sure the other side is an allowable surface.
        if(NZ2.gt.NCOMP.or.NS2.gt.NZSUR(NZ2))then
          write(outs,'(3a,I3,A,i3,A,I3,a,i3,a)')' Zone ',zname(NZ1),
     &      ' (',NZ1,') surface',NS1,
     &      ' connected to non-existent zone (',
     &      NZ2,') or surface (',NS2,').'
          if(dll)then
            dllsubr='MZPREC'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            close(ieout)
            CALL ERPFREE(ieout,ISTAT)
            call pauses(3)   ! pause for 3 seconds then abort program.
            CALL EPAGEND
            STOP
          endif
        endif

C Label for the connection being checked.
        CALL CONXINFO(1,I,CXSTR)

C For the `i` connection find its matching connection and see that it
C has reciprocal information.
        ioc=IZSTOCN(NZ2,NS2)
        if(ioc.eq.0)then
          write(outs,'(3a,I3,A,i3,A)')' Zone ',zname(NZ2),
     &      ' (',NZ2,') surface',NS2,' connection cannot be resolved.'
          if(dll)then
            dllsubr='MZPREC'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            close(ieout)
            CALL ERPFREE(ieout,ISTAT)
            call pauses(3)   ! pause for 3 seconds then abort program.
            CALL EPAGEND
            STOP
          endif
        endif

C Check back to see if the other side is also a type 3 connection.
        if(ICT(ioc).ne.3)then
          IF(IWMOFF.ne.1)then
            write(outs,'(A,A12,A2,I3,A,I3,A)') ' Zone ',zname(NZ2),
     &        ' (',NZ2,') Surface',NS2,' is not'
            call edisp(iuout,outs)
            call edisp(iuout,' connected to a Type 3 surface.')
          endif
          IWARN=IWARN+1
        else
          if(NZ2.EQ.IC1(ioc).AND.NS2.EQ.IE1(ioc))then

C We have a zone connection that matches so carry out other tests.
C to ensure proper matching. The first test involves surface azimuth
C and elevation angles to ensure surfaces face each other.
            SELV=SPELV(NZ1,NS1)+SPELV(NZ2,NS2)
            if(SELV.LT.-0.9.OR.SELV.GT.0.9)then
              write(outs,'(A,A)')' For: ',CXSTR
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              write(outs,'(A,2f8.3,A)')' the surface elevations (',
     &          SPELV(NZ1,NS1),SPELV(NZ2,NS2),') are beyond tolerance.'
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              IF(IWMOFF.ne.1)call edisp(iuout,' (should be + & - 1deg')
              IWARN=IWARN+1
            endif
            SELV=ABS(SPELV(NZ1,NS1))
            IF(SELV.GT.89.1.AND.SELV.LT.90.9)goto 71
            DAZI=SPAZI(NZ1,NS1)-SPAZI(NZ2,NS2)
            DAZI=ABS(DAZI)
            if(DAZI.LT.179.1.OR.DAZI.GT.180.9)then
              write(outs,'(A,A)')' For: ',CXSTR
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              write(outs,'(A,2f8.3,A)')' the surface azimuths (',
     &          SPAZI(NZ1,NS1),SPAZI(NZ2,NS2),') are beyond tolerance.'
              IF(IWMOFF.ne.1) call edisp(iuout,outs)
              IF(IWMOFF.ne.1) call edisp(iuout,
     &            '(should be ~180 deg dif')
              IWARN=IWARN+1
            endif

C Check physical properties where possible.
   71       if(NELTS(NZ1,NS1).NE.NELTS(NZ2,NS2))then
              write(outs,'(A,A)')' For: ',CXSTR
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              write(outs,'(A,A,i3,a,i3,A)')
     &         ' the construction layers (',
     &          SMLCN(NZ1,NS1),NELTS(NZ1,NS1),' & ',NELTS(NZ2,NS2),
     &          ') do not match.'
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              IWARN=IWARN+1
              goto 20
            endif
            if(NGAPS(NZ1,NS1).NE.NGAPS(NZ2,NS2))then
              write(outs,'(A,A)')' For: ',CXSTR
              if(IWMOFF.ne.1)call edisp(iuout,outs)
              write(outs,'(A,A,2i3,A)')' the number of air gaps (',
     &         SMLCN(NZ1,NS1),NGAPS(NZ1,NS1),NGAPS(NZ2,NS2),
     &         ') do not match.'
              if(IWMOFF.ne.1)call edisp(iuout,outs)
              IWARN=IWARN+1
              goto 20
            endif

C Check surface radiative properties.
            CALL ECLOSE(EE(NZ1,NS1),EI(NZ2,NS2),0.02,CLOSE)
            IF(.NOT.CLOSE)then
              write(outs,'(A,A)')' For: ',CXSTR
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              write(outs,'(A,2f5.2,A)')
     &          ' the outside face emissivities (',
     &          EE(NZ1,NS1),EI(NZ2,NS2),') do not match.'
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              IWARN=IWARN+1
              goto 20
            endif
            CALL ECLOSE(EI(NZ1,NS1),EE(NZ2,NS2),0.02,CLOSE)
            IF(.NOT.CLOSE)then
              write(outs,'(A,A)')' For: ',CXSTR
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              write(outs,'(A,2f5.2,A)')
     &         ' the inside face emissivities (',
     &          EI(NZ1,NS1),EE(NZ2,NS2),') do not match.'
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              IWARN=IWARN+1
              goto 20
            endif
            CALL ECLOSE(AE(NZ1,NS1),AI(NZ2,NS2),0.02,CLOSE)
            IF(.NOT.CLOSE)then
              write(outs,'(A,A)')' For: ',CXSTR
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
            write(outs,'(A,2f5.2,A)')
     &          ' the outside face absorptivities (',
     &          AE(NZ1,NS1),AI(NZ2,NS2),') do not match.'
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              IWARN=IWARN+1
              goto 20
            endif
            CALL ECLOSE(AI(NZ1,NS1),AE(NZ2,NS2),0.02,CLOSE)
            IF(.NOT.CLOSE)then
              write(outs,'(A,A)')' For: ',CXSTR
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
             write(outs,'(A,2f5.2,A)')
     &         ' the inside face absorptivities (',
     &          AI(NZ1,NS1),AE(NZ2,NS2),') do not match.'
              IF(IWMOFF.ne.1)call edisp(iuout,outs)
              IWARN=IWARN+1
              goto 20
            endif

C Check construction thermophysical properties.
            NN=NNDC(NZ1,NS1)-1
            KK=NN
            DO 40 K=2,NN
              D1=C(NZ1,NS1,K,1)-C(NZ2,NS2,KK,2)
              DD1=ABS(C(NZ1,NS1,K,1)/100000.0)
              IF(D1.LT.-DD1.OR.D1.GT.DD1) then
                write(outs,'(A,A)')' For: ',CXSTR
                IF(IWMOFF.ne.1)call edisp(iuout,outs)
               write(outs,'(A,i3,A)')
     &            ' the thermophysical property of layer',K,
     &            ' does not match.'
                IF(IWMOFF.ne.1)call edisp(iuout,outs)
                IWARN=IWARN+1
                goto 20
              endif
              D2=C(NZ1,NS1,K,2)-C(NZ2,NS2,KK,1)
              DD2=ABS(C(NZ1,NS1,K,2)/100000.0)
              IF(D2.LT.-DD2.OR.D2.GT.DD2) then
                write(outs,'(A,A)')' For: ',CXSTR
                IF(IWMOFF.ne.1)call edisp(iuout,outs)
              write(outs,'(A,i3,A)')' the thermophysical prop of layer',
     &             K,' does not match.'
                IF(IWMOFF.ne.1)call edisp(iuout,outs)
                IWARN=IWARN+1
                goto 20
              endif
              KK=KK-1
   40       CONTINUE
          else

C We have a mismatch in zone connections (connection which should have
C matched does not).
            if(IWMOFF.ne.1)then
              write(outs,'(A,I4,A,I4,A)')
     &          ' Mismatch between connections',I,' and',ioc,'.'
              call edisp(iuout,outs)
              IWARN=IWARN+1
              goto 20
            endif
          endif
        endif
   20 continue
      IWARN=IWARN+IW
      if(endless.or.issilient)then
        continue
      else
C        call usrmsg(' ','Model topology check successful.','-')
      endif

C Find out which was most often used in terms of ground temperatures.
      igmax=0
      igmaxindex=0
      igumax=0
      igumaxindex=0
      do 8 ij=1,12
        if(igarray(ij).gt.igmax)then
          igmax=igarray(ij)
          igmaxindex=ij
        endif
        if(iguarray(ij).gt.igumax)then
          igumax=iguarray(ij)
          igumaxindex=ij
        endif
  8   continue

      if(igmaxindex.eq.0.and.igumaxindex.eq.0)then

C Nothing specified as a connection so use UK monthly mean
C ground temperatures at 1.2 m depth as given by Lacy.
        GTMP(1)=7.28; GTMP(2)=6.5; GTMP(3)=6.56; GTMP(4)=7.33
        GTMP(5)=9.06; GTMP(6)=11.39; GTMP(7)=13.5; GTMP(8)=14.33
        GTMP(9)=14.11; GTMP(10)=11.72; GTMP(11)=10.72
        GTMP(12)=8.67
      elseif(igmax.gt.igumax)then

C More standard profiles used so get from grdtmp.
        igttype=igmaxindex 
        do 6 ij = 1,12
          GTMP(ij)=grdtmp(ij,igmaxindex)
  6     continue
      elseif(igmax.le.igumax)then

C More user defined profiles used so get from UGRDTP.
        igttype= -1 * igumaxindex
        do 5 ij = 1,12
          GTMP(ij)=UGRDTP(ij,igumaxindex)
  5     continue
      endif

C Identify special materials file & call routine to read it.
      if(ispmxist.gt.0)then
        call spminit
      endif

      RETURN
      END

C ******************** MZUTIL ********************
C Reads additional zone descriptions if active.

      SUBROUTINE MZUTIL(ICOMP,IER)
#include "building.h"
#include "model.h"
#include "cfd.h"
#include "CFC_common.h"
#include "espriou.h"

      common/trc/itrc
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      COMMON/FILEP/IFIL
      COMMON/ER1/IER1
      INTEGER :: ier1

      COMMON/ICFNOD/ICFD,ICP
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      CHARACTER*72 LCFD
      common/ndcfd/NCFDND,ICFDND(MNZ),NCONF
      common/bsmtdef/iBSIMP(MCOM),LBSIMP(MCOM)
      COMMON/cfcall/icfcal(MNZ)
      COMMON/ACCrec/IACC(MNZ)
      COMMON/ICFCHN/ICFMON(MNZ),ICFDBC(MNZ),ICFTMP,ICFLIB
      common/param2/TITLE(MNZ),CFTRFL(MNZ),LPHI(MNZ)
      common/shad0/ISIcalc,icalcD,icalcM

C Active domain common.
      common/ACTDOM/CFDOK
      logical CFDOK
      
C Experimental hydrogen controller.
C Residential hydrogen cogen: control file
      common/resh2ctl/iresh2ctl,resh2flnam
      integer iresh2ctl         !- flag indicating that file required by residential hydrogen
                                !- cogen controller is specified
      character*72 resh2flnam   !- name of file containing residential hydrogen cogen
                                !- controller inputs

C Experimental renewable energy system (electrical) controller
C input file information.
      common/reselecctl/ireselecctl,reselecflnam
      integer ireselecctl       !- flag indicating that file required by renewable energy
                                !- system controller is specified
      character*72 reselecflnam !- name of file containing renewable energy system
                                !- controller inputs
                                
      CHARACTER OUTSTR*124,outs*72
      CHARACTER*72 lfil,CFTRFL,TITLE,LPHI
      CHARACTER*72 LBSIMP

      logical dll

C Reset warning flag and check and see if running in dll mode.
      IER1=0
      IUNIT=IFIL+1
      call isadll(dll)
      
C Open zone viewfactor, pre-calculated S/I and convection files.
C Report the contents of the S/I file if the user has requested 
C a trace and then re-scan the model.
      IF(IVF(ICOMP).EQ.1)CALL MZVFIN(ICOMP)
      IF(ISI(ICOMP).EQ.1.and.ISIcalc.eq.2)CALL MZSHIN(ICOMP)
      IF(IHC(ICOMP).EQ.1)CALL MZHCFV2(ICOMP)

C If BASESIMP foundation defined, read it in.
      if(iBSIMP(icomp).eq.1)then
        call BASEREAD(icomp,ier)
        if(dll.and.ier.eq.2)then
          return
        endif
      endif

C Residential hydrogen cogeneration control file. Experimental,
C use with caution.
      if ( iresh2ctl .eq. 1 ) then
         call RESH2_Ctl_Read
      endif

C Renewable energy system electrical controller. Experimental, 
C use with caution.
      if ( (ireselecctl .eq. 1) .and. (ICOMP.eq.1) ) then
         call RES_elec_ctl_Read  ! Read control file once.
      endif

C Read zone TMC file.
      IF(ITW(ICOMP).EQ.1)then
        CALL ERTWIN(itrc,itu,iunit,LTWIN(ICOMP),icomp,ier)
        if(dll.and.ier.eq.2)then
          return
        endif
      endif

C Read zone CFC file.
      if (icfc(icomp).eq.1)then
        call read_in_cfc_file(itrc,itu,iunit,lcfcin(icomp),icomp,ier)
        if(dll.and.ier.eq.2)then
          return
        end if
      end if

C Read zone casual gain control file.
      IF(ICGC(ICOMP).EQ.1)then
        CALL ercgcf(itrc,itu,LCGCIN(ICOMP),ICOMP,ier)
        if(dll.and.ier.eq.2)then
          return
        endif
        if (ier.eq.-101) call usrmsg(' ',
     &           'Daylight coefficients not yet calculated!','F')
      endif

C CFD domain.
      IF(ABS(IFCFD(ICOMP)).GT.0 .and. CFDOK)THEN
        ICP=ICOMP

C Set flags that indicate whether there is thermal and/or
C network air flow conflation. These flags affect how
C the .dfd file is read in. Five conflation mechanisms
C are supported.
C IBLD=1 - thermal conflation active and mapping between
C          dfs and bps surfaces is required.
C IMFN=1 - air flow conflation active and mapping between
C          dfs and bps connections is required.
        IBLD=0
        IMFN=0
        if(IFCFD(ICOMP).eq.1) IBLD=1
        if(IFCFD(ICOMP).eq.2) IBLD=1
        if(IFCFD(ICOMP).eq.3) IMFN=1
        if(IFCFD(ICOMP).eq.4) IBLD=1
        if(IFCFD(ICOMP).eq.5) IBLD=1
        ICFD=0
 98     ICFD=ICFD+1

C Read CFD configuration file and set length scales based on ESP-r standard
C geometry model.
        if(ICFDND(ICFD).EQ.ICOMP) then
          IFL=IFIL+1

C Set currentfile.
          write(currentfile,'(a)')LCFD(ICOMP)(1:LNBLNK(LCFD(ICOMP)))
          CALL EFOPSEQ(IFL,LCFD(ICOMP),1,IER)
          CALL STRIPC(IFL,OUTSTR,0,ND,1,'dfd line 1',IER)
          if(OUTSTR(1:15).eq.'DFS DESCRIPTION')then
            rewind(IFL)
            CALL CFDDTA(ITRC,ITU,ICOMP,IBLD,IMFN,IER)
          else
            CALL DFDREAD(ICOMP,itrc,itu,IER)
          endif
          CALL CFDLENS
          CALL DEFINESSO

C Open CFD monitoring file.
          ICFMON(ICFD) = IFIL + 19 + (ICFD-1)*100
          call efopseq(ICFMON(ICFD),CFTRFL(ICFD),3,ier)
          if(ier.ne.0)then
            write(outs,'(a,i2,a)') 
     &        'Error opening CFD monitoring file for domain ',ICFD,'.'
            call edisp(iuout,outs)
          endif

C Open CFD boundary condition export file.
          ICFDBC(ICFD) = IFIL+24+(ICFD-1)*100
          write(lfil,'(a14,i2.2,a4)')'../tmp/CFD-BC_',icfd,'.txt'
          call efopseq(ICFDBC(ICFD),lfil,3,ier)
          if(ier.ne.0)then
            write(outs,'(a,i2,a)') 
     &   'Error opening CFD boundary condition export file for domain ',
     &      ICFD,'.'
            call edisp(iuout,outs)
          endif

C If adaptive conflation control is active then open a file to record
C the actions of the controller.
          if (ABS(IFCFD(ICOMP)).eq.4.or.ABS(IFCFD(ICOMP)).eq.5) then
            write(lfil,'(a19,i2.2,a4)')'../tmp/ACC-actions_',icfd,
     &        '.txt'
            IACC(ICFD) = IFIL + 29 + (ICFD-1)*100
            call efopseq(IACC(ICFD),lfil,3,ier)
            if(ier.ne.0)then
              write(outs,'(a,i2,a)') 
     &          'Error opening ACC-actions file for domain ',ICFD,'.'
              call edisp(iuout,outs)
            endif  
          endif

C Initialise counter to indicate that CFD has not yet been invoked. This
C will be incremented each time CFD is called for this zone.
          icfcal(ICFD) = 0
        endif
        if(ICFD.LT.NCONF)goto 98
      ENDIF

      RETURN
      END

C ********************** USESPSC **********************
C Passes pre-simulation data in configuration file to
C simulation control. Parameters that can be set are as follows.
C *St_up - startup period
C *Period - the simulation start day and month, end day and month
C *Time step - the number of time steps per hour
C *Hourly - whether each timestep is written to the file.
C *Save option - 1,2,3,4
C *Results - building results file
C *Flow_results - air flow results file
C *Plant_results - plant results file
C *CFD results file
C Weather and control are as specified in the configuration file.

      SUBROUTINE USESPS(name,IER)
#include "building.h"
#include "plant.h"
#include "power.h"
#include "MultiYear_simulations.h"
      
      integer lnblnk  ! function definition

      common/outin/iuout,iuin,ieout
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      COMMON/PCTIME/TIMSEC
      COMMON/PCTSTP/NTSTPP
      common/save/isave
      common/prec7/itcnst
      common/spfldes/spfdescr(MSPS)
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      INTEGER :: nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      CHARACTER*72 LCFD
      integer :: ifcfd

C Extended simulation parameters for each set.
      common/spfldats/isstupex(MSPS),isbnstepex(MSPS),ispnstepex(MSPS),
     &  issaveex(MSPS),isavghex(MSPS),iscfdactivate(MSPS),
     &  isicfdys(MSPS),isicfdyf(MSPS),
     &  scftims(MSPS),scftimf(MSPS)
      INTEGER :: isstupex,isbnstepex,ispnstepex,issaveex,isavghex
      INTEGER :: iscfdactivate      ! zero ignore domains
      INTEGER :: isicfdys,isicfdyf  ! CFD simulation start & finish days
      REAL :: scftims,scftimf       ! CFD simulation start & finish time

C Once per hour storage of results, averaged over the hour.
      common/rdcgen/irdact
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),scfdres(MSPS),sipvres
      character sblres*72,sflres*72,splres*72,smstres*72,
     &  selres*72,scfdres*72,sipvres*72

C lafres  - fluid flow network results file.
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES,CFRESF
      character RFILE*72,PFILE*72,MSTRFILE*72,LAFRES*72,
     &  CFRESF*72
      COMMON/ENETRES/ERESLIB
      character ERESLIB*72
      common/dllerr/dllsubr,dllmesg

      COMMON/SPFL/spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok
      LOGICAL     spfileok,perok,cfdperok,tstepok,saveok,autook,exitok,
     &  startupok
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/ACTDOM/CFDOK
      logical CFDOK

      real CFTIMS,CFTIMF    ! start and finish times for CFD assessment
      integer ICFDYS,ICFDYF ! julian day-of-year at start and end of CFD
      integer ICFAUX  ! helps to keep track of IFCFD()
      integer ICFVIEW ! toggle for preview via dfv during assessment
      integer ICFVIEWMINT,ICFVIEWMAXT  ! range of temperatures for dfv
      logical DFVinvoked
      COMMON/CFSEUP/CFTIMS,CFTIMF,ICFDYS,ICFDYF,ICFAUX(MCOM),ICFVIEW,
     &  ICFVIEWMINT,ICFVIEWMAXT,DFVinvoked

      logical dll,match

      character outs*124, outs2*124
      character spfdescr*30,name*30
      character dllsubr*12,dllmesg*124

C Check if running in dll mode.
      call isadll(dll)
      IER=0

C Assign global data.
      if(isstup.ne.0)then
        ITCNST=isstup
        startupok=.true.
      endif
      if(isbnstep.ne.0)then
        ntstep=isbnstep

C Check whether to save every timestep to file.
        if(isavgh.eq.1)then
          irdact=1
        else
          irdact=0
        endif
        tstepok=.true.
      endif

C Plant time steps and time increment.
      if(ispnstep.ne.0)then
        NTSTPP=ispnstep
        TIMSEC=3600.0/(FLOAT(NTSTEP)*FLOAT(NTSTPP))
        tstepok=.true.
      else
        NTSTPP=1  ! Prevent ntstpp from being zero.
      endif
      if(issave.ge.0)then
        isave=issave
        saveok=.true.
      endif

C Match period name in command line.
      ln=lnblnk(name)
      match=.false.

      do 42 i=1, nsset
        if(name(1:ln).eq.spfdescr(i)(1:lnblnk(spfdescr(i))))then
          match=.true.

C Determine if period supports multi-year simulations.
          bMY_sim_enabled = bMY_period_support (i)

C Update initial assumptions with data from this set.
          if(isstupex(i).ne.0)then
            ITCNST=isstupex(i)
            startupok=.true.
          endif
          if(isbnstepex(i).ne.0)then
            ntstep=isbnstepex(i)

C Check whether to save every timestep to the file.
            if(isavghex(i).eq.1)then
              irdact=1
            else
              irdact=0
            endif
            tstepok=.true.
          endif

          if(ispnstepex(i).ne.0)then
            NTSTPP=ispnstepex(i)
            TIMSEC=3600.0/(FLOAT(NTSTEP)*FLOAT(NTSTPP))
            tstepok=.true.
          else
            NTSTPP=1  ! Prevent ntstpp from being zero.
          endif
          if(issaveex(i).ge.0)then
            isave=issaveex(i)
            saveok=.true.
          endif

          isd1=isstday(i)
          ism1=isstmon(i)
          CALL EDAY(isd1,ism1,isds)
          isd2=isfnday(i)
          ism2=isfnmon(i)
          CALL EDAY(isd2,ism2,isdf)

C Loop through zones to see if there is a cfd domain description.
          do loop=1,NCOMP
            IF(ABS(IFCFD(loop)).GT.0) CFDOK=.true.
          enddo

C If there is a CFD domain set its attributes from sps.
C If there is a directive to ignore reset CFDOK.
C The CFSEUP attributes will be used in askaboutcfd.
          cfdperok=.false.
          if(CFDOK)then
            if (iscfdactivate(i).gt.-1) then
              CFTIMS=scftims(i)
              CFTIMF=scftimf(i)  ! start and finish times for CFD assessment
              ICFDYS=isicfdys(i) ! day-of-year at start ...
              ICFDYF=isicfdyf(i) ! ... and end of CFD
              if(iscfdactivate(i).eq.0) CFDOK=.false.
              cfdperok=.true.
              DFVinvoked=.false.
            endif
          endif
          
          if ( bMY_sim_enabled ) then

C Handle start/end year.
            iMY_start_year = iMY_period_start_years(i)
            iMY_end_year = iMY_period_end_years(i)
            isdf = isdf + 365 * ( iMY_end_year - iMY_start_year )

C Assign start/end year to simulation year. These data will not be
C used in single simulation mode but assigning them now enables
C the interface to initialise the start/end year to relevant values
C when the user interactively switches to multi-year mode.
          else
            iMY_start_year = iYear
            iMY_end_year   = iYear
          endif

          perok = .true.
          if ( isdf .lt. isds ) then
            perok = .false. 
            if ( bMY_sim_enabled ) then
              write(outs,'(A,I2,A,I2,A,I4,A)')
     &          ' Error: Period start day (',
     &          isd1,'/',ism1,'/',iMY_start_year,')'
              write(outs2,'(A,I2,A,I2,A,I4,A)')
     &          ' is after period finish day (',
     &          isd2,'/',ism2,'/',iMY_end_year,')'

            else
              write(outs,'(A,I2,A,I2,A)')
     &          ' Error: Period start day (',
     &          isd1,'/',ism1,')'
              write(outs2,'(A,I2,A,I2,A)')
     &          ' is after period finish day (',
     &          isd2,'/',ism2,')'
            endif

            call edisp(iuout, outs )
            call edisp(iuout, outs2)

            if ( autook ) then
              call pauses(3)   ! pause for 3 seconds then abort program
              stop
            else
              call UsrMsg(outs,outs2,'W')
            endif
          endif 

C Assign results file names based on names recorded in the simulation
C parameter sets. If the user is browsing then code in reslib.F will ensure 
C that it is placed in the user's home folder (if there is a relative path
C included such as ../res then this will be ignored). 
C If the user is not browsing and the simulation was started 
C in the model cfg folder then the supplied name will be used as is (i.e. if
C it has a relative path then it will be written using that path).
C If the file name includes an absolute path (e.g. /tmp) then this is respected
C for interactive or silent runs, for invocations from within the cfg folder
C or from another location.
          if(sblres(i)(1:2).ne.'  ')then
            RFILE=sblres(i)
          endif
          if(sflres(i)(1:2).ne.'  ')then
            LAFRES=sflres(i)
          endif
          if(splres(i)(1:2).ne.'  ')then
            pfile=splres(i)
          endif
          if(smstres(i)(1:2).ne.'  ')then
            MSTRFILE=smstres(i)
          endif
          if(ientxist.gt.0.and.(selres(i)(1:2).ne.'  '))then
            ERESLIB=selres(i)
          endif
          if(scfdres(i)(1:2).ne.'  ')then
            CFRESF=scfdres(i)  ! set cfd results file name
          endif
          if(match)goto 43
        endif
   42 continue

C Check that there is enough information to proceed.
   43 if(autook) then
        if(perok.AND.tstepok.AND.saveok)then
          spfileok=.true.
        else
          write(outs,'(2a)') 
     &      'insufficient parameter information for automatic use ',
     &      'of the simulator (e.g. missing periods, save level etc.).'
          if(dll)then
            dllsubr='mzinpt'
            dllmesg=outs
            ier=2
            return
          else
            call usrmsg(outs,' ','W')
            close(ieout)
            CALL ERPFREE(ieout,ISTAT)
            call pauses(3)   ! pause for 3 seconds then abort program
            CALL EPAGEND
            STOP
          endif
        endif
      else
        spfileok=.true.
      endif

      return
      end

C ******************** ckcurmatch ********************
C Scan and report current mis-matches.

      subroutine ckcurmatch(prob,iprob)
#include "building.h"
#include "model.h"
#include "geometry.h"

C Parameters
      logical prob  ! true if there is a problem
      integer iprob ! number of mis-matches detected

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      logical dup,first

      CHARACTER CXSTR*78

      IPROB=0
      ICC=0
      prob=.false.
      first=.false.
      do 287 IZ=1,NCOMP
        call georead(IFIL+1,LGEOM(IZ),IZ,0,itru,IER)
        nzsur(iz)=nsur
        do 288 IS=1,NSUR
          ICC=ICC+1
          if(IC1(ICC).ne.IZ)then

C If this is the first warning, print out heading.
            if(.NOT.first)then
              CALL CONXINFO(1,0,CXSTR)
              CALL EDISP(iuout,CXSTR)
              first=.true.
            endif
            call edisp(iuout,'Current zone does not match contiguity')
            CALL CONXINFO(1,ICC,CXSTR)
            CALL EDISP(iuout,CXSTR)
            prob=.true.
            IPROB=IPROB+1
          endif
          if(IE1(ICC).ne.IS)then
            if(.NOT.first)then
              CALL CONXINFO(1,0,CXSTR)
              CALL EDISP(iuout,CXSTR)
              first=.true.
            endif
            call edisp(iuout,
     &          'Current surface does not match contiguity')
            CALL CONXINFO(1,ICC,CXSTR)
            CALL EDISP(iuout,CXSTR)
            prob=.true.
            IPROB=IPROB+1
          endif
  288   continue
  287 continue

C Check to see if partitions match. See if other zone/surface
C exists and what it points to.
      do 290 i=1,NCON
        if(ICT(i).eq.3)then
          if(IC2(i).gt.0.and.IC2(i).le.NCOMP.and.
     &      IE2(i).gt.0.and.IE2(i).le.NZSUR(IC2(i)))then
            ioc=IZSTOCN(IC2(i),IE2(i))
            if(ioc.ne.0)then
              if(ICT(ioc).ne.3)then
                if(.NOT.first)then
                  CALL CONXINFO(1,0,CXSTR)
                  CALL EDISP(iuout,CXSTR)
                  first=.true.
                endif
                call edisp(iuout,' ')
                call edisp(iuout,'One side is not linked!')
                CALL CONXINFO(1,i,CXSTR)
                CALL EDISP(iuout,CXSTR)
                CALL CONXINFO(1,ioc,CXSTR)
                CALL EDISP(iuout,CXSTR)
                prob=.true.
                IPROB=IPROB+1
               elseif(IC2(ioc).ne.IC1(i))then
                if(.NOT.first)then
                  CALL CONXINFO(1,0,CXSTR)
                  CALL EDISP(iuout,CXSTR)
                  first=.true.
                endif
                call edisp(iuout,' ')
                call edisp(iuout,'Zone contiguity link incorrect!')
                prob=.true.
                IPROB=IPROB+1
                CALL CONXINFO(1,i,CXSTR)
                CALL EDISP(iuout,CXSTR)
                CALL CONXINFO(1,ioc,CXSTR)
                CALL EDISP(iuout,CXSTR)
              elseif(IE2(ioc).ne.IE1(i))then
                if(.NOT.first)then
                  CALL CONXINFO(1,0,CXSTR)
                  CALL EDISP(iuout,CXSTR)
                  first=.true.
                endif
                call edisp(iuout,' ')
                call edisp(iuout,
     &              'Surface contiguity link incorrect!')
                prob=.true.
                IPROB=IPROB+1
                CALL CONXINFO(1,i,CXSTR)
                CALL EDISP(iuout,CXSTR)
                CALL CONXINFO(1,ioc,CXSTR)
                CALL EDISP(iuout,CXSTR)
              endif

C Check to see if this connection is only pointed to once by an
C another zone connection in the list.
              dup=.false.
              do 291 i2=1,NCON
                if(IC2(i2).eq.IC1(i).and.IE2(i2).eq.IE1(i))then
                  if(.NOT.dup)then
                    if(ICT(i2).eq.3)dup=.true.
                  else
                    if(.NOT.first)then
                      CALL CONXINFO(1,0,CXSTR)
                      CALL EDISP(iuout,CXSTR)
                      first=.true.
                    endif
                    call edisp(iuout,' ')
                    call edisp(iuout,'Contiguity 2nd reference found!')
                    CALL CONXINFO(1,i,CXSTR)
                    CALL EDISP(iuout,CXSTR)
                    CALL CONXINFO(1,ioc,CXSTR)
                    CALL EDISP(iuout,CXSTR)
                    CALL CONXINFO(1,i2,CXSTR)
                    CALL EDISP(iuout,CXSTR)
                    prob=.true.
                  endif
                endif
  291         continue
            endif
          else
            if(.NOT.first)then
              CALL CONXINFO(1,0,CXSTR)
              CALL EDISP(iuout,CXSTR)
              first=.true.
            endif
            call edisp(iuout,' ')
            call edisp(iuout,
     &          'Contiguity other side link out of range!')
            CALL CONXINFO(1,i,CXSTR)
            CALL EDISP(iuout,CXSTR)
            prob=.true.
            IPROB=IPROB+1
          endif
        endif
  290 continue
      return
      end
