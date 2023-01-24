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


C This file contains the following subroutines.
C  ERSYS       - legacy interface that calls ERSYS_mmode in verbose mode.
C  ERSYS_mmode - reads a commented model configuration file in silent
C                or verbose mode.
C  scan_deprecated - deals with ancient tags.
C  scan_ground - scan ground related entities in model cfg file.
C  scan_database_names - scan database file names in model cfg file.
C  scan_day_types - scan project day types in model cfg file.
C  scan_sps    - scan simulation parameter sets in model cfg file.
C  scan_ies    - scan IES optics for use with Radiance.
C  scan_season - scan season entries in model cfg file.
C  SITEINFO    - provides site information (cfg file).
C  CONXINFO    - provides a description of inter-connections. 
C  HZSTOCON    - scans the current connections list and builds array
C                IZSTOCN(MCOMP,MS) holding the connection index.
C findwhichdbpath - takes a database file name and checks to see if
C                   the path is local, absolute or to the standard folder.

C ******************** ERSYS ********************
C A legacy interface to ERSYS_mmode allowing existing code to
C ERSYS_mmode in its traditional verbose mode.
C IER=3 signals model is too complex for current executables.

      SUBROUTINE ERSYS(FILE,IUC,IUF,MODE,ITRC,IER)
      IMPLICIT NONE
      integer  IUC, IUF, ITRC, IER
      CHARACTER*72 FILE
      CHARACTER*4 MODE
      logical bSilent

C Call multi-mode interface in traditional verboase mode.

      bSilent = .false.

      call ERSYS_mmode(FILE,IUC,IUF,MODE,ITRC,IER,bSilent)

      return
      end


C ******************** ERSYS_mmode ********************
C Reads a system configuration file which defines the building
C and/or plant system for simulation with or without comments
C and with several levels of verbosity in reporting.  
C If MODE='NONE' ERSYS fills the common block with file names only. 
C IF MODE='ALL ' then all geom files scanned. Depending on the value
C of bVerbose, it may provide interactive prompts to the screen.

C Common block variables are:
C INDCFG - is the configuration file index where: 0= registration, 
C          1= building only, 2= plant only, 3= building and plant.
C ICFGV  - version of the model configuration file. Version 4 was
C          introduced in May 2008 and V5 in April 2021.

C Data from site.h:
C siteexposureindex - site exposure index: 1 = city centre, normal case;
C                     2 = urban site, normal case; 3 = rural site, normal case;
C                     4 = city centre, equal sky, ground & building view factors;
C                     5 = city centre, building below mean height of surroundings;
C                     6 = rural site, isolated; 7= totally enclosed building;
C                     8 = user defined.
C groundrefl        - external ground reflectivity.
C groundreflmonth   - monthly values of ground reflectivity.
C groundreflmodel   - type of ground reflectivity model: 1 = constant albedo;
C                     2 = simple model (monthly albedo, number of days with snow on ground);
C                     3 = advanced model (monthly albedo, snow depth read from file).
C snowgroundrefl    - snow reflectivity.
C dayswithsnow      - monthly values of days with snow on ground.
C SNFNAM            - name of file containing hourly snow depths.
C modeltitle        - model title.
C NCOMP   - number of building zones.
C NCCODE  - zone code (for database reference).
C LPROJ   - zone operations (scheduling) file.
C LGEOM   - zone geometry file.
C LTHRM   - zone construction file.
C NCON    - number of intra-zone connections.
C IC1 & IE1 - zone and surface numbers defining the start point of each connection.
C ICT     - connection type where:
C            -1 = unknown at this time;
C             0 = connection to external conditions;
C             1 = connection to identical environmental conditions
C                 as the source zone (OR, relative temp and absolute 
C                 radiation can be specified, see below);
C             2 = connection to constant and known environmental conditions;
C             3 = connection to another zone (as defined by IC2 & IE2; 
C             4 = connected to ground;
c             5 = surface is adiabatic;
c             7 = surface partition according to the CEN 13791.
C IC2 & IE2  - connection description where IC2 & IE2 are unused if corresponding ICT=0
C            - contain relative temp and absolute incident radiation 
C              intensity values respectively if corresponding ICT=1
C              (If IC2=IE2=0, then identical to source zone)
C            - contain constant temperature and incident radiation 
C              intensity values respecitvely if corresponding ICT=2
C            - contain the zone and surface numbers at which each
C              connection terminates (if corresponding ICT=3)
C            - contain pointers to ground temps (if corresponding ICT=4)
C IAIRN   - building air flow simulation index (1=legacy on, 2=graphic)
C ICAAS   - fluid (= air) flow network node index associated with each building 
C           zone. NB fluid flow network and building zones need not be 
C           matched.
C IFLWN   - plant fluid flow simulation index (1 = on).
C ICFFS   - fluid flow network node associated with each plant component. 
C           NB fluid flow network & plant energy network need not be matched.
C LAPROB  - fluid flow model description file.

C ICFFS   - fluid flow network node associated with each plant
C           component. NB fluid flow network and plant energy
C           network need not be matched.
C ISIcalc - shading/insolation handling: 0 default, 1 embedded calculation;
C           2 pre-constructed file. 

C BDMDS,dbdsdesc - name file project demands file and related description.
C IER=3 signals model is overly complex for current executables.

      SUBROUTINE ERSYS_mmode(FILE,IUC,IUF,MODE,ITRC,IER,bSilent)
      USE AIM2_InputData, ONLY: iAIM2,LAIM2
      IMPLICIT NONE
#include "building.h"
#include "model.h"
#include "site.h"
#include "plant.h"
#include "power.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "tdf2.h"
#include "cfd.h"
#include "uncertainty.h"
#include "gnetwk.h"
#include "dhw_common.h"
#include "MultiYear_simulations.h"
#include "bc_data.h"
#include "espriou.h"
#include "lookup_data.h"
#include "sbem.h"
#include "roam.h"
#include "CFC_common.h"
#include "ipvdata.h"
#include "seasons.h"
#include "schedule.h"
#include "UserSimulationToggles.h"
#include "help.h"

      INTEGER, PARAMETER :: MSTMC=20

      integer lnblnk     ! function definition
      character FILE*72  ! model configuration file name
      integer IUC        ! channel to read configuration file
      integer IUF        ! channel for other required model files
      character MODE*4   ! if ALL then causes additional data structure to be read
      integer ITRC       ! level of reporting during the file scan
      integer IER        ! return zero if ok, 2 if cfg file could not be opened
                         ! 3 if excess model complexity
      logical bSilent    ! flag indicating verbosity.

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      INTEGER :: mmod,limit,limtty
      COMMON/FILEP/IFIL  ! base file unit number
      INTEGER :: ifil
      COMMON/ER1/IER1
      INTEGER :: ier1
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout
      character doit*248
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      INTEGER :: ic1,ie1,ict,ic2,ie2

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      INTEGER :: IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

C skyview viewfactor from external surfaces to the sky (site.h)
C groundview viewfactor from external surfaces to the ground
C buildingview viewfactor from external surfaces to other buildings & obstructions
      COMMON/C6/INDCFG
      INTEGER :: INDCFG

C IAIM2 set to 1 if AIM-2 infiltration model active.
C LAIM2 (72 char) file holding AIM-2 infiltration model data.
C      common/aim2def/iAIM2,LAIM2

      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      common/clmlnum/nbclmlhelp
      character clmlnam*42,clmlaid*72,clmldbfile*144,clmlavail*10
      character clmlhelp*72
      INTEGER :: nbclmlhelp

C NZSUR is the number of surfaces in each zone (from geometry.h).
C NZTV is the number of vertices in each zone (from geometry.h).
C szcoords, iszjvn (from geometry.h)

C IFPNF is the unit for the plant network file.
C LPNF (72 char) plant network file name.
      COMMON/C23/IFPNF,LPNF
      INTEGER :: IFPNF
      COMMON/C23a/BPF

C IZSTOCN - for each zone:surface a pointer to connection index.
      INTEGER :: IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/PREC8/SLAT,SLON
      REAL :: SLAT,SLON

      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: IAIRN,ICAAS

C Instruction to pause assessment to allow rescan of a zone operation file.
      integer ipausecas   ! zero no pause non-zero pause 
      common/pausecas/ipausecas(MCOM)

C Configuration control.
C ctldoc (248 char) - Overall control notes. If equal to 'none'
C                   then no control is imposed.
C lctlf (72 char) - model control file name.
      common/cctlnm/ctldoc,lctlf

C Images in the model.
C  imgfmt (4 char) gives the format of each image associated with
C         the model, it must match one of the known fmttag.
C  imgfoc (4 char) associates an image with a specific topic:
C         'FZON' is related to zone composition
C         'FNET' is related to network composition
C         'FCTL' is related to control composition
C         'FDFS' is related to CFD domains
C         'FPLN' is related to plant networks
C         'FPER' is related to predicted performance
C         '****' is a general image displayed at startup
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS FPLN
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton

C Initial view.
      common/initv/initvt,EYEMI(3),VIEWMI(3),ANGI
      REAL :: angi,EYEMI,VIEWMI
      INTEGER :: initvt

C Special materials file.
      common/spmfxst/ispmxist,spflnam
      INTEGER :: ispmxist

C Non-linear thermophysical properties.
      COMMON/VTHP18/LNLTHP
      CHARACTER LNLTHP*72
      COMMON/VTHP31/INTHPS,INTHPZ(MCOM)
      LOGICAL INTHPS,INTHPZ

C 1D node distribution.
      COMMON/GR1D05/LGRD1D
      CHARACTER LGRD1D*72
      COMMON/GR1D06/IGR1D
      LOGICAL IGR1D

C 3D ground.
      COMMON/GRND100/GRND3D
      LOGICAL GRND3D

C 3D zones.
      common/GR3D100/BLDG3D,ZONE3D(MCOM)
      LOGICAL BLDG3D,ZONE3D
      common/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)

C Moisture.
      common/MOIST01/MSTROK,MSTRZN(MCOM)
      LOGICAL MSTROK,MSTRZN
      common/MOIST02/LMOIST(MCOM)

C Structured mesh.
      COMMON/GRSD100/IndxSt
      INTEGER :: IndxSt
      COMMON/GRSD101/LGrdSt

C Primary energy conversions and emissions.
      common/PCONV/ipconv,pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
      INTEGER :: ipconv
      REAL :: pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
      common/CONVEM/phtco2,phtnox,phtsox,pclco2,pclnox,pclsox,
     &              pltco2,pltnox,pltsox,pfnco2,pfnnox,pfnsox,
     &              pspco2,pspnox,pspsox,phwco2,phwnox,phwsox

      REAL :: phtco2,phtnox,phtsox,pclco2,pclnox,pclsox
      REAL :: pltco2,pltnox,pltsox,pfnco2,pfnnox,pfnsox
      REAL :: pspco2,pspnox,pspsox,phwco2,phwnox,phwsox

C CPCALC: ICPCON set to 1 if there is a definition.
      common/CPCALC/icpcon,ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,
     &              sbh,pad,wvpe
      REAL :: ble,bwi,bhi,blox,bloy,bloz,orient,ra,sbh,pad,wvpe
      INTEGER :: icpcon,irt

C CFD
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      INTEGER :: icfdnd,NCONF,IFCFD,ncfdnd

C Variables for weekdays, and weekends.
C Assume: Mon=1, Tue=2, Wed=3, Thu=4, Fri=5, Sat=6, Sun=7
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2
      INTEGER :: idwe1,idwe2
      CHARACTER wkd1*10,wkd2*10

C IPV data is from ipvdata.h. If within cfg file then the file
C name is replaced with 'internal'.
      common/IPVF/lipvdatf

C Contaminant flow model commons
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM/CNTMFIL,CNTMDESC,NTSTEPC
      INTEGER :: NCONTM,NOCNTM,NTSTEPC

C HVAC
C Flag indicating presence of HVAC models:
C ihvacflag=1 indicates HVAC models are active; 0 indicates no HVAC models.
      common/hvacinfo/ihvacflag,hvacfile
      INTEGER :: ihvacflag

      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),scfdres(MSPS),sipvres
      character sblres*72,sflres*72,splres*72,smstres*72,
     &  selres*72,scfdres*72,sipvres*72

C BASESIMP.
      common/bsmtdef/iBSIMP(MCOM),LBSIMP(MCOM)
      INTEGER :: ibsimp

C Fuel cell.
      common/Fcell/iFClds,LFClds
      INTEGER :: iFClds

C Mechanical Ventilation systems
      Integer iMech
      Character LMech*72
      common/mechvdef/iMech,LMech

C GSHP
C Flag indicating presence of GSHP models:
C igshp=1 indicates GSHP models are active; 0 indicates no GSHP models.
      common/gshpinfo/igshp
      INTEGER :: igshp
      common/gshpfile/gshpfile

C Wind-generated electricity
      common/H2wind/LWndSupp,iWndSupp
      INTEGER :: iWndSupp

C GCEP
C Flag indicating presence of GCEP models:
C igcepflag=1 indicates GCEP models are active; 0 indicates no GCEP models.
      common/gcepinfo/igcep
      INTEGER :: igcep
      common/gcepfile/gcepfile

C Bidirectional raw optical data file.
      COMMON/BIDIR/IFLAGBI,INTVALBI,NSTMCFL(MCON)
      COMMON/BIDIRFL/bidirfile,bidirname(MSTMC)
      INTEGER :: IFLAGBI,INTVALBI,NSTMCFL
C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER :: icalender,nbcaldays,nbdaytype

C CPCALC pressure coef.
      common/cpcaco/nuco,naco(MPCP),xcoab(MPCP),ycoab(MPCP),
     &              zcoab(MPCP),azim(MPCP),TYPE(MPCP),elev(MPCP)
      REAL :: azim,elev,zcoab,ycoab,xcoab
      INTEGER :: nuco

C Path to model and command line file (if any). Browse
C is a logical flag, .true. restricts update/save options.
      common/rpath/path
      common/user/browse

C Path to SHOCC input files
C bSHOCCed     - logical flag whether project has SHOCC input
C bZoneSHOCCed - logical array whether zones have SHOCC input
C SHOCCshlFile - path to SHOCC .shl file for SHOCC'd project library
C SHOCCshzFile - path to SHOCC .shz file for each SHOCC'd zone
      common/SHOCCcfg/bSHOCCed,SHOCCshlFile,bZoneSHOCCed(mcom),
     &                SHOCCshzFile(mcom)
      logical bSHOCCed,bZoneSHOCCed
      character SHOCCshlFile*72,SHOCCshzFile*72

C Residential hydrogen cogeneration control file
      common/resh2ctl/iresh2ctl,resh2flnam
      integer iresh2ctl         !- flag indicating that file required by residential hydrogen
                                !- cogen controller is specified
      character*72 resh2flnam   !- name of file containing residential hydrogen cogen
                                !- controller inputs

C Renewable energy system electrical control file
      common/reselecctl/ireselecctl,reselecflnam
      integer ireselecctl       !- flag indicating that file required by renewable energy
                                !- system controller is specified
      character*72 reselecflnam !- name of file containing renewable energy system
                                !- controller inputs

C Treatment of solar radiation data in weather file.
C ESP-r operates on hourly-centered data. That is, the amount of solar
C irradiance at the recorded hour is the instantaneous irradiance at the
C hour. In Canadian Weather for Energy Calculations (CWEC) files, solar
C radiation is integrated over the previous hour. To account for 
C the half hour shift, a flag has been implemented to indicate
C whether solar radiation data in the weather file is hour-centered
C (default) or half-hour centered.
C The flag (iSlr_half_hr_flg) can be set in the .cfg file or within bps in the 
C 'simulation toggles' menu.
C iSlr_half_hr_flg value = 0 :hour-centered; =1 : half-hour centered.
      common/CWEC_SOL/iSlr_half_hr_flg
      integer iSlr_half_hr_flg,idty

C Shading/insolation calculation mod.
      common/shad0/ISIcalc,icalcD,icalcM
      integer ISIcalc,icalcD,icalcM

C The flag iflgevapC(mcom) can be set in the .cfg file or within bps in the 
C 'simulation toggles' menu.
      common/EVCOOL/iflgevapC(mcom) ! modifies the indoor air humidity ratio
      integer iflgevapC             ! if 1, sets indoor equal to outdoor wet bulb temp
      integer iZntempFlg,itempFlg
      
C Radiance processor cores.
      integer radcores
      common/radcor/radcores

C Zone fluid.
C  zfldK - conductivity;
C  zfldD - density;
C  zfldC - specific heat capacity;
C  zflsA - total shortwave absorptivity;
C  zSWA  - shortwave absorption (W).
      COMMON/zfluid/znotair(mcom),zfldK,zfldD,zfldC,zfldA,
     &              zSWAp(mcom),zSWAf(mcom)
      real zfldK,zfldD,zfldC,zfldA,zSWAp,zSWAf
      LOGICAL znotair
      
      integer IHCT,ICOREXHCT
      COMMON/HCTHRY/IHCT,ICOREXHCT  ! Default for internal & external hc method.

      INTEGER IRNCMD,IRNCMF
      INTEGER ICREPORT,IDOL,IDAYNUM,IMTHNUM,IDWKNUM,IDTYY

      integer iexcludediff_flag
      common/excludediff/iexcludediff_flag

      LOGICAL browse,context
      LOGICAL XST,OK,DOK,IGDCVS,IGDCNC,IGDNDC,IGDTAQ,cnnok
      LOGICAL OK3DCVS,OK3DCNC,OK3DNDC,OK3DTAQ
      logical foundmould,foundsbem
      CHARACTER loutstr*248,OUTSTR*124,OUTS*124,text*124,t117*117
      CHARACTER LCFD*72
      CHARACTER*72 LAPROB,LPNF,BPF
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,LMOIST,LGrdSt
      CHARACTER WORD*20,WORD2*20,NODID*12,CXSTR*78
      character msgl2*48
      CHARACTER CTLDOC*248,LCTLF*72
      CHARACTER spflnam*72,lipvdatf*72
      CHARACTER ectime*24,naco*40,type*4
      character*10 wkday(7)
      character hvacfile*72,LBSIMP*72,LFClds*72
      character gshpfile*72
      character gcepfile*72
      character limgpth*24,ldocpth*24,lcfgroot*32,lpath*72
      character path*72,lupath*72
      character namez*12,namen*12
      character bidirfile*72,bidirname*12
      CHARACTER CONTMNAM*12,CNTMDESC*124,cntmfil*72
      character calename*32,calentag*12,calendayname*32
      character LWndSupp*72
      character ipvact*3   ! to signal internal or external IPV file
      character pstring*40 ! for season days and months
      logical newgeo       ! to use for testing if new/old geometry file
      logical unixok       ! to check for database path file separators
      logical havetitile   ! set true if modeltitle scanned
      logical did142       ! processed zones.
      logical deprecated   ! trap ancient tags
      integer itru         ! unit for output
      integer iflag        ! for error traps
      integer loop         ! for looping     
      INTEGER :: id1,id2,id3,id4,icon,icomp,icc,i,ia,ibrowse,ierv
      INTEGER :: im1,im2,im3,im4,iier,ipos,ios,inod,inisz,igrdp
      INTEGER :: irva,is,iset_start,k,kv,j,iv,iva,iunit,iunit1,lc
      INTEGER :: itrunc,isva,istat,n,m,items
      INTEGER :: lcfgl,lcfgr,louts,ncn,nd
      real    :: vp1,vp2,vp3,vp4,vp5  ! for ies control points

#ifdef OSI
      integer iincomp,iincon    ! to pass number of zones connections to c code
#else
      integer*8 iincomp,iincon  ! to pass nb zones connections to c code
#endif

      REAL :: gva
      DIMENSION GVA(12),IVA(99),namez(mcom),namen(mcom)
      INTEGER, dimension(mcon) :: IRVC
      data wkday/'Monday', 'Tuesday','Wednesday', 'Thursday',
     &           'Friday','Saturday', 'Sunday'/

C Flags & storage for parsing *sim-toggle tags.
      logical bError
      integer iWordCount
      integer iEGetArrW
      character*248 cLnWords(124)
C Function references used to parse *sim-toggle tags.      
      real fCtoR_err 
      integer iCtoI_err
      logical bStringsMatch
      integer iucbk
     
      newgeo=.false.  ! assume older format geometry.

      call isunix(unixok)
      itru=iuout   ! set trace unit to use in subroutine calls
      iflag=0      ! initial value
      foundmould=.false.; foundsbem=.false.
      havetitile=.false.; did142=.false.

C Initalising starting point for parsing simulation presets (*sps).
      iSet_start = 1

C Initialise values for plant convergence criteria.
      call InitPltSolnParams() 
      
C Initialise flag for boundary condition data.
      bBC_data_defined = .false.

C Scan the defaults file and then set file common blocks.
C Make temporary use of file unit IUF.
C Note: escdef must come after scan of .esprc file.
      call escdef(IUF,'s',IER)
      write(LCLIM,'(a)') DCLIM(1:lnblnk(DCLIM))
      write(LAPRES,'(a)') DAPRES(1:lnblnk(DAPRES))
      write(LFMAT,'(a)') DFCON(1:lnblnk(DFCON))
      write(LFMUL,'(a)') DFMUL(1:lnblnk(DFMUL))
      write(LOPTDB,'(a)') DOPTDB(1:lnblnk(DOPTDB))
      write(LPRFDB,'(a)') DPRFDB(1:lnblnk(DPRFDB))
      write(LPCDB,'(a)')  DPCDB(1:lnblnk(DPCDB))
      write(LSBEM,'(a)')  DSBEM(1:lnblnk(DSBEM))  ! assign default SBEM db
      write(MCMPDBFL,'(a)') DMCMPDBFL(1:lnblnk(DMCMPDBFL))
      write(lfmould,'(a)') dmdbnam(1:lnblnk(dmdbnam))
      write(LCFCDB,'(a)') DCFCDB(1:lnblnk(DCFCDB))
      write(LPREDEF,'(a)') DPREDEF(1:lnblnk(DPREDEF))

C Clear the model common blocks.
      call clrprb

C Initialise simulation toggle flags .
      bUserGAM = .false.       
      bUserSolar = .false. 
      bUserPltSolvConfig = .false. 
      
      LUALF='UNKNOWN'
      bidirfile='UNKNOWN'
      CALENAME='UNKNOWN'
      if(iutdf.eq.0)IUTDF=IFIL+7  ! set if not already done so.

C Set flag indicating that solar radiation data is hour-centered (default).
C Variable will be reset if 'slr_half_hr' included in .cfg.
      iSlr_half_hr_flg = 0

C Set site altitude to 0m. This will be overwritten if it is included in .cfg.
      sitealt=0.0

C Set flag for indoor wet bulb temperature to default (no evaporative cooling)
C Variable will be reset if 'iflgevapC(icomp)' included in .cfg.
      do 88 icomp = 1,ncomp
        iflgevapC(icomp) = 0
 88   continue

C Initialise NCM data read flags.
      IRNCMD=0
      IRNCMF=0
      LASBEM='UNKNOWN'  ! assume no UK NCM description

C Initialise HVAC flag to indicate no HVAC models active. This
C will be reset if `*hvac' included in .cfg.
      ihvacflag=0
      hvacfile=' '

C AIM-2.
      LAIM2=' '
      iAIM2 = 0

C Fuel cell.
      iFClds=0
      LFClds=' '

c Mechanical ventilation.
      LMech = ' '
      iMech = 0

C GSHP
C Initialise flag to indicate no GSHP models active. This
C will be reset if `*gshp' is included in the .cfg file.
      gshpfile = ' '
      igshp = 0

C GCEP: initialise flag to indicate no models active. This
C will be reset if `*gcep' is included in the .cfg file.
      gcepfile = ' '
      igcep = 0

C Initialise day types.
      idty=0

C Wind-generated electricity.
      iWndSupp = 0
      LWndSupp =' '

C Water properties for water filled zones (set std values).
      zfldK=0.6; zfldD=998.2; zfldC=4190.0; zfldA=0.0

C Clear zone related files and flags.
      do 43 i=1,MCOM
        ipausecas(i)=0  ! unmark pause to re-scan zone operation file
   43 continue

C Assume standard version of configuration file and that user has
C set no preference for upgrading.
      icfgv=4
      usecurcfg=0
      nuco=0

C Initialise the indices.
      IGDCVS=.FALSE.
      IGDCNC=.FALSE.
      IGDNDC=.FALSE.
      IGDTAQ=.FALSE.
      EYEMI(1)=-100.
      EYEMI(2)=-100.
      EYEMI(3)=100.
      VIEWMI(1)=10.
      VIEWMI(2)=10.
      VIEWMI(3)=10.
      ANGI=40.
      initvt=0

C Older files may not have a *list section for the calendar and
C to support compatibility with legacy assumption of 3 day types
C set nbdaytype = 3 but instantiate the calendar variables for
C weekday/saturday/sunday/holiday and calendar day type tags. Newer
C cfg files will have a *list section and thus update nbdaytype.
C The initial day types cannot (?) be deleted and more day types if 
C defined will be stored in CALENTAG(5) onwards.
      IF(NBDAYTYPE.LT.3)THEN
        nbdaytype=3
      ENDIF
      CALENTAG(1)='weekday'
      CALENTAG(2)='saturday'
      CALENTAG(3)='sunday'
      CALENTAG(4)='holiday'
      CALENDAYNAME(1)='generic weekday'
      CALENDAYNAME(2)='generic saturday'
      CALENDAYNAME(3)='generic sunday'
      CALENDAYNAME(4)='generic holiday'

C Open the configuration file, return if not found otherwise set
C currentfile to the configuration file.
      IER=0
      INDCFG=1
      IUCBK=IUC
      CALL EFOPSEQ(IUC,FILE,1,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF
      write(currentfile,'(a)') FILE(1:lnblnk(FILE))

C Derive the model root name from this in case none is specified.
C If FILE > 4 char it might have a .cfg attached, otherwise add.
      lcfgr=lnblnk(FILE)
      lcfgl=lcfgr-3
      if(lcfgr.gt.4)then
        if(FILE(lcfgl:lcfgr).eq.'.cfg')then
          if(lcfgl.gt.32)then
            write(cfgroot,'(a)',iostat=ios,err=99) FILE(1:32)
          else
            write(cfgroot,'(a)',iostat=ios,err=99) FILE(1:lcfgl-1)
          endif
        else
          if(lcfgr.gt.32)then
            write(cfgroot,'(a)',iostat=ios,err=99) FILE(1:32)
          else
            write(cfgroot,'(a)',iostat=ios,err=99) FILE(1:lcfgr)
          endif
        endif
      else
        if(lcfgr.gt.32)then
          write(cfgroot,'(a)',iostat=ios,err=99) FILE(1:32)
        else
          write(cfgroot,'(a)',iostat=ios,err=99) FILE(1:lcfgr)
        endif
      endif

C Read '*CONFIGURATION' and flag if a new format file otherwise
C treat as an old format.  If '*CONFIGURATION3.0' read zone info
C via tags. If '* CONFIGURATION4.0' or '* CONFIGURATION4.2' then
C IPV data might be embedded in the configuration file.
      CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'config index',IER)
      IF(IER.NE.0)goto 1
      if(LOUTSTR(1:17).eq.'* CONFIGURATION3.'.or.
     &   LOUTSTR(1:17).eq.'* CONFIGURATION4.'.or.
     &   LOUTSTR(1:14).eq.'*configuration')then
        if(LOUTSTR(1:17).eq.'* CONFIGURATION3.')icfgv=3
        if(LOUTSTR(1:18).eq.'* CONFIGURATION4.0')icfgv=4
        if(LOUTSTR(1:18).eq.'* CONFIGURATION4.2')icfgv=5
        if(LOUTSTR(1:18).eq.'*configuration 4.2')icfgv=5
      else
        call usrmsg('Does not appear to be a config file: ',FILE,'W')
        ier=1
        return
      endif
      
      if(icfgv.lt.5)then
        IHCT=1; ICOREXHCT=1  ! assume A&H inside and McAdams outside heat transfer.
      else
        IHCT=1; ICOREXHCT=2  ! assume A&H inside and MoWiTT outside heat transfer.
      endif

C Point of return after tokens have been read and decoded.
  34  CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'header tags',IER)
      IF(IER.NE.0)goto 1
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','tokens',IFLAG)
      IF(IFLAG.NE.0)goto 1
      deprecated=.false.
      call scan_deprecated(loutstr,word,deprecated)  ! Check if a deprecated tag.
      if(deprecated) goto 34

      if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
        CALL EGETRM(LOUTSTR,K,ectime,'W','date stamp',IER)  ! missing date stamp not fatal
      elseif(WORD(1:5).eq.'*root'.or.WORD(1:10).eq.'*base_name')then
        CALL EGETRM(LOUTSTR,K,cfgroot,'W','model base name',IER)
        IF(IER.NE.0)goto 1
      elseif(WORD(1:7).eq.'*zonpth')then
        CALL EGETRM(LOUTSTR,K,zonepth,'W','path to zones',IER)
        IF(IER.NE.0)goto 1
      elseif(WORD(1:7).eq.'*netpth')then
        CALL EGETRM(LOUTSTR,K,netpth,'W','path to nets',IER)
        IF(IER.NE.0)goto 1
      elseif(WORD(1:7).eq.'*ctlpth')then
        CALL EGETRM(LOUTSTR,K,ctlpth,'W','path to ctl',IER)
        IF(IER.NE.0)goto 1
      elseif(WORD(1:7).eq.'*mscpth')then
        CALL EGETRM(LOUTSTR,K,mscpth,'W','path to miscel. files',IER)
        IF(IER.NE.0)goto 1
      elseif(WORD(1:7).eq.'*imgpth')then
        CALL EGETRM(LOUTSTR,K,imgpth,'W','path to img',IER)
        IF(IER.NE.0)goto 1
      elseif(WORD(1:7).eq.'*tmppth')then  ! do not bother with this folder name
         goto 34
      elseif(WORD(1:7).eq.'*docpth')then
        CALL EGETRM(LOUTSTR,K,docpth,'W','path to documents',IER)
        IF(IER.NE.0)goto 1
      elseif(WORD(1:7).eq.'*radpth')then
        CALL EGETRM(LOUTSTR,K,radpth,'W','path to radnce',IER)
        IF(IER.NE.0)goto 1
      elseif(WORD(1:7).eq.'*dbspth')then
        CALL EGETRM(LOUTSTR,K,dbspth,'W','path to loc databases',IER)
        IF(IER.NE.0)goto 1
      elseif(WORD(1:8).eq.'*tmppth')then
        CALL EGETRM(LOUTSTR,K,tmppth,'W','path to temp files',IER)
        IF(IER.NE.0)goto 1
      elseif(WORD(1:8).eq.'*radcore')then
        CALL EGETWI(LOUTSTR,K,radcores,1,4,'W','radiance cores',IER)
        IF(IER.NE.0)goto 1
      elseif(WORD(1:4).eq.'*ies')then

C IES data to pass to radiance. Currently 10 entities are supported.
        call scan_ies(IUC,ier)
        goto 34   ! read another tag
      elseif(WORD(1:8).eq.'*latlong')then
        CALL EGETWR(LOUTSTR,K,sitelat,-90.,90.,'W','Latitude',IER)
        SLAT=sitelat
        CALL EGETWR(LOUTSTR,K,sitelongdif,-15.,15.,'W',
     &    'Longitude dif from time meridian',IER)
        SLON=sitelongdif
      elseif(WORD(1:8).eq.'*sitealt')then
        call egetwr(loutstr,k,sitealt,-100.,3000.,'W',
     &    'site altitude',IER)
      elseif(WORD(1:6).eq.'*site ')then
        CALL EGETWI(loutstr,k,siteexposureindex,1,8,'W',
     &    'Site exposure index',IER)
        CALL EGETWR(LOUTSTR,K,groundrefl,0.,1.,'W',
     &    'Ground reflect',IER)
      elseif(WORD(1:12).eq.'*slr_half_hr')then ! Solar radiation (half-hour data) flag.
        call egetwi(loutstr,k,iSlr_half_hr_flg,0,1,'W',
     &    'solar half-hour flag',IER)
      elseif(WORD(1:9).eq.'*contents')then
        CALL EGETRM(LOUTSTR,K,lmodelqa,'W','model contents',IER)
      elseif(loutstr(1:6).eq.'*notes')then
        CALL EGETRM(LOUTSTR,K,lmodellog,'W','notes file',IER)
      elseif(WORD(1:5).eq.'*img ')then

C An image file. If this is a version 4 file then there will be
C a subsequent (long) line with documentation for the images.
        noimg=noimg+1
        CALL EGETW(LOUTSTR,K,WORD2,'W','img format',IFLAG)
        write(imgfmt(noimg),'(a4)') WORD2(1:4)
        CALL EGETW(LOUTSTR,K,WORD2,'W','focus',IFLAG)
        write(imgfoc(noimg),'(a4)') WORD2(1:4)
        CALL EGETRM(LOUTSTR,K,limgfil(noimg),'W','image file',IER)
      elseif(WORD(1:6).eq.'*imdoc')then
        if(noimg.gt.0)then
          CALL EGETRM(LOUTSTR,K,imgdoc(noimg),'W','image docum',IER)
        endif

C Start of databases via general or specific tag file tag.
      elseif(LOUTSTR(1:7).eq.'* DATAB'.or.WORD(1:6).eq.'*datab')then
        call scan_database_names(IUC,foundmould,foundsbem,IRNCMD,ier)
        if(ier.eq.0) goto 34
      elseif(LOUTSTR(1:4).eq.'*mat'.or.LOUTSTR(1:7).eq.'*stdmat'.or.
     &       WORD(1:4).eq.'*mat'.or.WORD(1:7).eq.'*stdmat')then
        call scan_database_names(IUC,foundmould,foundsbem,IRNCMD,ier)
        if(ier.eq.0) goto 34
      elseif(LOUTSTR(1:8).eq.'* Ground'.or.  ! V5: Scan the ground entries.
     &       LOUTSTR(1:8).eq.'* GROUND'.or.
     &       WORD(1:8).eq.'*ground ')then
        call scan_ground(IUC,ITRC,IGDCVS,IGDCNC,IGDNDC,IGDTAQ,ier)
        if(ier.eq.0) goto 34
      elseif(LOUTSTR(1:19).eq.'*surf_heat_transfer')then
        call egetwi(loutstr,k,ihct,1,8,'W','inside hc method',IER)
        call egetwi(loutstr,k,icorexhct,1,12,'W','ext hc method',IER)
        if(ier.eq.0) goto 34
      elseif(WORD(1:9).eq.'*calename'.or.WORD(1:9).eq.'*calentag'.or.
     &       WORD(1:7).eq.'*daytag')then
        call scan_day_types(IUC,idty,ier)
        if(ier.eq.0) goto 34
      elseif(WORD(1:4).eq.'*tdf')then
        CALL EGETRM(LOUTSTR,K,LTDFA,'W','TDF file name',IER)
        CALL ERPFREE(IUTDF,ISTAT)  ! check if it exists.
        call FINDFIL(LTDFA,XST)
        call scan_temporal_links(IUC,XST,ITRC,ier)
        goto 34
      elseif(WORD(1:5).eq.'*indx')then

C In V3 V4 cfg files *index is followed by some site information which
C may not be tagged. V5 will have tagged site information which can
C be picked up via the usual scan.
        CALL EGETWI(LOUTSTR,K,INDCFG,0,3,'F','config index',IER)
        IF(IER.NE.0)goto 1
        if(ITRC.GE.1.AND.ITRC.LT.4)then  ! Reporting.
          CALL EDISP(iuout,' ')
          IF(INDCFG.EQ.0)THEN
            CALL EDISP(iuout,' Project registration configuration')
          ELSEIF(INDCFG.EQ.1)THEN
            CALL EDISP(iuout,' Zone only configuration')
          ELSEIF(INDCFG.EQ.2)THEN
            CALL EDISP(iuout,' Plant only configuration')
          ELSEIF(INDCFG.EQ.3)THEN
            CALL EDISP(iuout,'  Zone/plant configuration')
          ELSE
            msgl2=' Unknown configuration type'
            CALL USRMSG(' ',msgl2,'W')
            IER=1
            RETURN
          ENDIF
          CALL EDISP(iuout,' ')
        endif
        if(icfgv.ge.5) goto 34

C Latitude & longitude difference.
        CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'latitude & long dif',IER)
        IF(IER.NE.0)goto 1
        if(LOUTSTR(1:8).eq.'*latlong')then
          K=9
          CALL EGETWR(LOUTSTR,K,sitelat,-90.,90.,'W','Latitude',IER)
          SLAT=sitelat
          CALL EGETWR(LOUTSTR,K,sitelongdif,-15.,15.,'W',
     &      'Longitude dif from time meridian',IER)
          SLON=sitelongdif
        else
          K=0
          CALL EGETWR(LOUTSTR,K,sitelat,-90.,90.,'W','Latitude',IER)
          SLAT=sitelat
          CALL EGETWR(LOUTSTR,K,sitelongdif,-15.,15.,'W',
     &      'Longitude dif from time meridian',IER)
          SLON=sitelongdif
        endif
        IF(IER.NE.0) GOTO 1

C Site exposure and ground reflectance.
        CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'ground reflectance',IER)
        IF(IER.NE.0)goto 1
        if(LOUTSTR(1:6).eq.'*site ')then
          K=6
          CALL EGETWI(LOUTSTR,K,siteexposureindex,1,8,'W',
     &      'Site exposure index',IER)
          CALL EGETWR(LOUTSTR,K,groundrefl,0.,1.,'W','Ground refl',IER)
        else
          K=0
          CALL EGETWI(LOUTSTR,K,siteexposureindex,1,8,'W',
     &      'Site exposure index',IER)
          CALL EGETWR(LOUTSTR,K,groundrefl,0.,1.,'W','Ground refl',IER)
        endif
        IF(IER.NE.0) GOTO 1

C Index of exposure and related data.
        IF(siteexposureindex.EQ.8) THEN
          CALL LSTRIPC(IUC,LOUTSTR,3,ND,1,'user defined site exposure',
     &      IER)
          IF(IER.NE.0)goto 1
          K=0
          CALL EGETWR(LOUTSTR,K,skyview,0.,1.,'W','Sky view',IER)
          CALL EGETWR(LOUTSTR,K,groundview,0.,1.,'W','Ground view',IER)
          CALL EGETWR(LOUTSTR,K,buildingview,0.,1.,'W','Building vew',
     &      IER)
          IF(IER.NE.0) GOTO 1

C Range checking.
          IF(ABS(skyview+groundview+buildingview-1.).GT..001) 
     &      CALL USRMSG(
     &      ' Total exposure out of range in',LOUTSTR,'W')
        endif

C Reporting.
        IF(ITRC.GE.1.AND.ITRC.LT.4)CALL SITEINFO(itru)


C V3 & V4 files assume that * DATABASES or *databases immedately follow the
C *indx and latitude & site exposure lines.
        CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'* DATABASES',IER)
        if(IER.NE.0)return
        if(LOUTSTR(1:7).eq.'* DATAB'.or.LOUTSTR(1:6).eq.'*datab')then
          call scan_database_names(IUC,foundmould,foundsbem,IRNCMD,ier)
          if(ier.eq.0) goto 34
        else
          call edisp(itru,' Did not find `* DATABASES` ')
          ier=1
          return
        endif
        goto 34

      elseif(LOUTSTR(1:10).eq.'* Building'.or.
     &       LOUTSTR(1:10).eq.'* BUILDING'.or.
     &       LOUTSTR(1:9).eq.'*building')then

C If an older file then jump to process the title and zone or plant information.
C Newer files might have the title following *building.
        if(icfgv.ge.5)then
          if(ND.gt.2)then
             CALL EGETRM(LOUTSTR,K,modeltitle,'W','title',IER)
             havetitile=.true.
          else
            CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'model name',IER)
            IF(IER.NE.0)goto 1
            modeltitle=LOUTSTR(1:72)
            havetitile=.true.
          endif
        endif
        goto 451            ! jump to process zones or plant.
      elseif(LOUTSTR(1:7).eq.'* Plant'.or.  ! V5 sections revised so check.
     &       LOUTSTR(1:6).eq.'*plant')then
        if(icfgv.ge.5)then
          if(LOUTSTR(1:11).eq.'*plant none')then
             goto 34        ! There is no plant.
          else
            if(ND.gt.2)then ! the model title follows *plant.
              k=7
              CALL EGETRM(LOUTSTR,K,modeltitle,'W','title',IER)
              havetitile=.true.
            endif
          endif
        endif
        CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'* plant',IER)
        LPNF=LOUTSTR(1:72)
        backspace(IUC) ! backspace one line before processing.
C        if(INDCFG.eq.2) goto 1001          ! jump to process plant definitions.
        if(INDCFG.eq.2) goto 888            ! jump to process plant definitions.
      elseif(loutstr(1:6).eq.'*title')then  ! for registration only model
        CALL EGETRM(LOUTSTR,K,modeltitle,'W',
     &    'model title registration',IER)
        goto 451                            ! should be last line of file.
      elseif(WORD(1:4).eq.'*ctl')then
        CALL EGETRM(LOUTSTR,K,LCTLF,'W','config control',IER)
      elseif(LOUTSTR(1:8).eq.'* Ground'.or.
     &       LOUTSTR(1:8).eq.'* GROUND'.or.
     &       WORD(1:8).eq.'*ground ')then
        call scan_ground(IUC,ITRC,IGDCVS,IGDCNC,IGDNDC,IGDTAQ,ier)
      elseif(WORD(1:9).eq.'*contents')then
        CALL EGETRM(LOUTSTR,K,lmodelqa,'W','model contents',IER)
      elseif(WORD(1:5).eq.'*gref')then

C Ground reflectivity model, no-snow monthly albedos, snow albedo
        CALL EGETWI(LOUTSTR,K,groundreflmodel,1,3,'F',
     &    'ground reflection model',IER)
        DO 441 I=1,12
          CALL EGETWR(LOUTSTR,K,groundreflmonth(I),0.,1.,'W',
     &      'monthly gr. refl.',IER)
  441   CONTINUE
        CALL EGETWR(LOUTSTR,K,snowgroundrefl,0.,1.,'W',
     &    'snow ground reflection',IER)  
      elseif(WORD(1:5).eq.'*snow')then

C Number of days with snow on the ground (if ground reflection model is type 2)
        DO 442 I=1,12
          CALL EGETWI(LOUTSTR,K,dayswithsnow(I),0,31,'W',
     &      'days with snow each month',IER)
  442   CONTINUE
      elseif(WORD(1:5).eq.'*sndf')then

C Snow depth file (if ground reflection model is type 3)
        CALL EGETRM(LOUTSTR,K,SNFNAM,'W','snow depth file',IER)
      elseif(WORD(1:4).eq.'*bcd')then

C Flexible boundary conditions data definition.  
        call EGETRM(LOUTSTR,K,cTemp,'W','BC data file', IER)
        cBC_data_file_name = cTemp(1:lnblnk(cTemp))
        bBC_data_defined = .true.
      elseif(WORD(1:4).eq.'*shl')then

C There is a SHOCC library associated with the model so set
c bSHOCCed true and then check if the file exists.
        CALL EGETRM(LOUTSTR,K,SHOCCshlFile,'W','SHOCC',IER)
        XST=.FALSE.
        call FINDFIL(SHOCCshlFile,XST)
        if(XST) then
          bSHOCCed=.true.
        else
          write(outs,'(3a)') 'SHOCC project library file ',
     &      SHOCCshlFile(1:lnblnk(SHOCCshlFile)),
     &      ' was referenced but not found.'
          call edisp(iuout,outs)
        endif
      elseif(WORD(1:5).eq.'*hvac')then

C HVAC file, test if it exists.
        CALL EGETRM(LOUTSTR,K,hvacfile,'W','config hvac',IER)
        XST=.FALSE.
        call FINDFIL(hvacfile,XST)
        if(XST)then
          ihvacflag=1
        else
          write(outs,'(3a)') 'HVAC file ',hvacfile(1:lnblnk(hvacfile)),
     &      ' was referenced but not found.'
          call edisp(iuout,outs)
        endif
      elseif(WORD(1:4).eq.'*aim')then

C AIM-2, test if it exists.
        CALL EGETRM(LOUTSTR,K,LAIM2,'W','AIM-2 infil data',IER)
        XST=.FALSE.
        call FINDFIL(LAIM2,XST)
        if(XST)then
          iAIM2=1
        else
          write(outs,'(3a)') 'AIM-2 file ',LAIM2(1:lnblnk(LAIM2)),
     &      ' was referenced but not found.'
          call edisp(iuout,outs)
        endif
C        goto 34
      elseif(WORD(1:8).eq.'*roaming')then

C Change orientation definition while roaming.
        IROAM=1
        call EGETRM(LOUTSTR,K,LROAM,'-','Roaming file name', IER)
        XST=.FALSE.
        call FINDFIL(LROAM,XST)
        if(.NOT.XST)then
          call usrmsg('Roaming file not found; File name: ',LROAM,'W')
        endif
      elseif(WORD(1:4).eq.'*tab')then

C Lookup table definition.     
        call EGETRM(LOUTSTR,K,cTemp,'W','Lookup table data file', IER)
        cLookup_data_file_name = cTemp(1:lnblnk(cTemp))
        bLookup_data_defined = .true.
      elseif(WORD(1:4).eq.'*ctm')then

C Contaminant model.
        CALL EGETRM(LOUTSTR,K,TEXT,'F','contaminant model',IER)
        CNTMFIL=TEXT(1:72)
        NOCNTM=1
        IF(IER.NE.0)GOTO 1
        IUNIT=IFIL+72
        CALL EFOPSEQ(IUNIT,CNTMFIL,1,IER)
        IF(IER.LT.0)then
          outs='Problem opening contaminant file...aborting cfg scan.'
          call edisp(iuout,outs)
          goto 6
        endif

C Reporting.
        IF(ITRC.GE.1.AND.ITRC.LT.4)THEN
          CALL EDISP(iuout,' ')
          CALL EDISP(iuout,' A contaminant network has been defined:')
          WRITE(OUTS,9986)CNTMFIL(1:LNBLNK(CNTMFIL))
 9986     FORMAT(' Contaminant file              : ',a)
          CALL EDISP(iuout,OUTS)
        ENDIF

C Read the file header and check for second-line tag. 
        CALL LSTRIPC(IUNIT,LOUTSTR,0,ITEMS,1,' ',IER)
        CALL LSTRIPC(IUNIT,LOUTSTR,0,ITEMS,1,' ',IER)
        J=0
        IF(LOUTSTR(:9).NE.'SECTION_1'.OR.IER.NE.0)THEN
          outs='Problem scanning contaminant file...aborting cfg scan'
          call edisp(iuout,outs)
          goto 6
        ENDIF
        REWIND(IUNIT,ERR=999)
        NOCNTM=1
        CALL ERPFREE(IUNIT,ISTAT)

      elseif(WORD(1:4).eq.'*fcl')then

C Fuel cell.
C If a file holding the time history of the electrical loads placed on the
C fuel cell exists, then get the name of the file and test for its existence.
        CALL EGETRM(LOUTSTR,K,LFClds,'W','Fuel cell elec loads data',
     &    IER)
        XST=.FALSE.
        call FINDFIL(LFClds,XST)
        if(XST)then
          iFClds=1
        else
          write(outs,'(3a)') 'FC loads file ',LFClds(1:lnblnk(LFClds)),
     &      ' was referenced but not found.'
          call edisp(iuout,outs)
          STOP ' Need elec loads data: unresolvable error.'
        endif
      elseif(WORD(1:4).eq.'*dhw')then

C DHW, test and see if it exists.
C This routine puts the file name into the DHW Modules Variable 
C and sets the flag into the DHW Modules Variable iDHW_Flag 
        CALL EGETRM(LOUTSTR,K,sDHW_InputFileName,'W','DHW data',IER)
        XST=.FALSE.
        call FINDFIL(sDHW_InputFileName,XST)
        if(XST)then
          iDHW_Flag=1
        else
          write(outs,'(3a)') 'DHW file ',
     &      sDHW_InputFileName(1:lnblnk(sDHW_InputFileName)),
     &      ' was referenced but not found.'
          call edisp(iuout,outs)
        endif
      elseif(WORD(1:5).eq.'*mvnt')then

C Mechanical ventilation, test and see if it exists.
        CALL EGETRM(LOUTSTR,K,LMech,'W','Mechanical ventilation data',
     &    IER )
        XST=.FALSE.
        call FINDFIL(LMech,XST)
        if(XST)then
         iMech=1
        else
          write(outs,'(3a)') 'Mechanical ventilation file ',
     &      LMech(1:lnblnk(LMech)),' not found!'
          call edisp(iuout,outs)
        endif
      elseif(WORD(1:5).eq.'*gshp')then

C GSHP: Test to ensure that .gshp file exists and set igshp
C to one if it does.
        CALL EGETRM(LOUTSTR,K,gshpfile,'W','gshp input',IER )
        XST=.FALSE.
        call FINDFIL(gshpfile,XST)
        if(XST)then
         igshp=1
        else
          write(outs,'(3a)') 'GSHP file ',
     &       gshpfile(1:lnblnk(gshpfile)),
     &       ' referenced but not found!'
          call edisp(iuout,outs)
        endif
      elseif(WORD(1:5).eq.'*gcep')then

C GCEP: Test to ensure that .gcep file exists and set
C igcep to one if it does.
        CALL EGETRM(LOUTSTR,K,gcepfile,'W','gcep input',IER )
        XST=.FALSE.
        call FINDFIL(gcepfile,XST)
        if(XST)then
         igcep=1
        else
          write(outs,'(3a)') 'GCEP file ',gcepfile(1:lnblnk(gcepfile)),
     &      ' referenced but not found!'
          call edisp(iuout,outs)
        endif
      elseif(WORD(1:4).eq.'*wnd')then

C Wind generated electricity.
C If a file holding the time history of wind generated electricity exists,
C get the name of the file and test for its existence.
        CALL EGETRM(LOUTSTR,K,LWndSupp,'W',
     &    'Wind-gen elec supply file',IER )
        XST=.FALSE.
        call FINDFIL(LWndSupp,XST)
        if(XST)then
          iWndSupp=1
        else
          write(outs,'(3a)') 'Wind-gen elec supply file ',
     &     LWndSupp(1:lnblnk(LWndSupp)),' was referenced but not found.'
          call edisp(iuout,outs)
          STOP ' Need wind elec supply data: unresolvable error.'
        endif
      elseif(WORD(1:11).eq.'*Twb_evCool')then

C Get flag integer (should be 1) and then the zone number
        call egetwi(loutstr,k,itempFlg,0,1,'W',
     &    'Twet bulb flag',IER)
        call egetwi(loutstr,k,iZntempFlg,0,999,'W',
     &    'zone numb flag',IER)
        iflgevapC(iZntempFlg)= itempFlg
      elseif(WORD(1:18).eq.'*exclude_diff_shad')then

C Exclude diffuse solar radiation flag.
        call egetwi(loutstr,k,iexcludediff_flag,0,1,'W',
     &    'exclude diff shad flag',IER)
      elseif(WORD(1:4).eq.'*vew')then
        CALL EGETWR(LOUTSTR,K,EYEMI(1),0.,0.,'-','X cord',IER)
        CALL EGETWR(LOUTSTR,K,EYEMI(2),0.,0.,'-','Y cord',IER)
        CALL EGETWR(LOUTSTR,K,EYEMI(3),0.,0.,'-','Z cord',IER)
        CALL EGETWR(LOUTSTR,K,VIEWMI(1),0.,0.,'-','X cord',IER)
        CALL EGETWR(LOUTSTR,K,VIEWMI(2),0.,0.,'-','Y cord',IER)
        CALL EGETWR(LOUTSTR,K,VIEWMI(3),0.,0.,'-','Z cord',IER)
        CALL EGETWR(LOUTSTR,K,ANGI,0.,0.,'-','Z cord',IER)
        initvt=1
      elseif(WORD(1:4).eq.'*ual')then
        CALL EGETRM(LOUTSTR,K,LUALF,'W','uncertainty definitions',IER)
      elseif(WORD(1:8).eq.'*weekend')then
        CALL EGETWI(LOUTSTR,K,idwe1,1,7,'W','weekend day',IER)
        CALL EGETWI(LOUTSTR,K,idwe2,1,7,'W','weekend day',IER)
        wkd1=wkday(idwe1)
        wkd2=wkday(idwe2)

C Seasonal data (similar to climatelist file scan in rcdblist.F).
      elseif(WORD(1:8).eq.'*seasons')then

        call scan_season(IUC,ier)  ! Parse seasons attributes.
        goto 34

      elseif(WORD(1:5).eq.'*year')then
        CALL EGETWI(LOUTSTR,K,IYEAR,1900,2051,'W','year',IER)

C Perform check that day types correspond with simulation year.
C The logic below assumes that the first three day types are still weekday
C saturday sunday. It loops through each Julian day and if the model
C day type is one of the standard 3 it checks that the model weekday
C is the same as a Julian weekday. Any day types beyond 3 are 
C retained. If no day types are defined in the model then it should do the
C work silently. Howevere, if there are more than 4 day types then the
C user might have setup that should not be overwritten.
        ICREPORT=0
        if(nbdaytype.gt.4)then
          continue
        else
          DO 123 IDOL=1,365
            IF(ICALENDER(IDOL).LE.3)THEN
              CALL EDAYR(IDOL,IDAYNUM,IMTHNUM)
              CALL EWEEKD(IDAYNUM,IMTHNUM,IYEAR,IDWKNUM)
              IF(IDWKNUM.LT.6)THEN
                IDTYY=1 ! WEEKDAY
              ELSEIF(IDWKNUM.EQ.6)THEN
                IDTYY=2 ! SATURDAY
              ELSEIF(IDWKNUM.EQ.7)THEN
                IDTYY=3 ! SUNDAY
              ENDIF
              IF(ICALENDER(IDOL).EQ.0)THEN
                ICALENDER(IDOL)=IDTYY  ! update the model day types
                ICREPORT=2             ! do it silently
              ELSEIF(ICALENDER(IDOL).NE.IDTYY)THEN
                ICALENDER(IDOL)=IDTYY  ! update the model day types
                ICREPORT=1
              ENDIF
            ENDIF
 123      CONTINUE
        endif
        if(ICREPORT.EQ.1)then
          CALL EDISP(IUOUT,'  ')
          CALL EDISP(IUOUT,
     &      'Simulation year and day type mismatch rectified')
        endif

      elseif (WORD(1:7).eq.'*agent') then    ! Agent.
        call AGT_CHK(LOUTSTR,K,IUF,iier)
        if (iier.ne.0) then
          CALL edisp(iuout,' Error getting number of agents,')
          CALL edisp(iuout,' agents will not be activated.')
        endif
        
      elseif (WORD(1:8).eq.'*gremlin') then  ! Gremlins.
        call GRM_CHK(LOUTSTR,K,IUF,iier)
        if (iier.ne.0) then
          CALL edisp(iuout,' Error getting number of gremlins,')
          CALL edisp(iuout,' gremlins will not be activated.')
        endif

C Water filled zones - get general parameters.
      elseif(WORD(1:15).eq.'*water_in_zones')then
        K=16
        CALL EGETWR(LOUTSTR,K,zfldK,0.,0.,'-','wtr cond',IER)
        CALL EGETWR(LOUTSTR,K,zfldD,0.,0.,'-','wtr dens',IER)
        CALL EGETWR(LOUTSTR,K,zfldC,0.,0.,'-','wtr spec ht',IER)
        CALL EGETWR(LOUTSTR,K,zfldA,0.,0.,'-','wtr sw abs',IER)

C Functional Mock-up Interface (FMI).
      elseif (WORD(1:4).eq.'*FMI') then
        call FMI_CHK(LOUTSTR,K,IUF,iier)
        if (iier.ne.0) then
          CALL edisp(iuout,' Error getting number of FMU directives,')
          CALL edisp(iuout,' FMI will not be activated.')
        endif

C System level primary energy conversion data, set flag.
      elseif(WORD(1:6).eq.'*pecnv')then
        ipconv=1
        CALL EGETWR(LOUTSTR,K,pcnvht,0.,0.,'-','heat p cnv',IER)
        CALL EGETWR(LOUTSTR,K,pcnvcl,0.,0.,'-','cool p cnv',IER)
        CALL EGETWR(LOUTSTR,K,pcnvlt,0.,0.,'-','light p cnv',IER)
        CALL EGETWR(LOUTSTR,K,pcnvfn,0.,0.,'-','fan p cnv',IER)
        CALL EGETWR(LOUTSTR,K,pcnvsp,0.,0.,'-','sml p cnv',IER)
        CALL EGETWR(LOUTSTR,K,pcnvhw,0.,0.,'-','hw p cnv',IER)
      elseif(WORD(1:5).eq.'*ecnv')then
        continue   ! efficiencies data line currently unused 
      elseif(WORD(1:7).eq.'*htemis')then
        CALL EGETWR(LOUTSTR,K,phtco2,0.,0.,'-','ht co2',IER)
        CALL EGETWR(LOUTSTR,K,phtnox,0.,0.,'-','ht nox',IER)
        CALL EGETWR(LOUTSTR,K,phtsox,0.,0.,'-','ht sox',IER)
      elseif(WORD(1:7).eq.'*clemis')then
        CALL EGETWR(LOUTSTR,K,pclco2,0.,0.,'-','cl co2',IER)
        CALL EGETWR(LOUTSTR,K,pclnox,0.,0.,'-','cl nox',IER)
        CALL EGETWR(LOUTSTR,K,pclsox,0.,0.,'-','cl sox',IER)
      elseif(WORD(1:7).eq.'*ltemis')then
        CALL EGETWR(LOUTSTR,K,pltco2,0.,0.,'-','lt co2',IER)
        CALL EGETWR(LOUTSTR,K,pltnox,0.,0.,'-','lt nox',IER)
        CALL EGETWR(LOUTSTR,K,pltsox,0.,0.,'-','lt sox',IER)
      elseif(WORD(1:7).eq.'*fnemis')then
        CALL EGETWR(LOUTSTR,K,pfnco2,0.,0.,'-','fan co2',IER)
        CALL EGETWR(LOUTSTR,K,pfnnox,0.,0.,'-','fan nox',IER)
        CALL EGETWR(LOUTSTR,K,pfnsox,0.,0.,'-','fan sox',IER)
      elseif(WORD(1:7).eq.'*spemis')then
        CALL EGETWR(LOUTSTR,K,pspco2,0.,0.,'-','sml co2',IER)
        CALL EGETWR(LOUTSTR,K,pspnox,0.,0.,'-','sml nox',IER)
        CALL EGETWR(LOUTSTR,K,pspsox,0.,0.,'-','sml sox',IER)
      elseif(WORD(1:7).eq.'*hwemis')then
        CALL EGETWR(LOUTSTR,K,phwco2,0.,0.,'-','hw co2',IER)
        CALL EGETWR(LOUTSTR,K,phwnox,0.,0.,'-','hw nox',IER)
        CALL EGETWR(LOUTSTR,K,phwsox,0.,0.,'-','hw sox',IER)
      elseif(WORD(1:4).eq.'*ipv')then

C IPV data file if there were two items on the line so call
C ripvdat with the external file name. If there are 4 items
C call ripvdat for data within the cfg file.
        if(ND.eq.2)then
          CALL EGETRM(LOUTSTR,K,lipvdatf,'W','ipv file',IER)
          CALL ERPFREE(IUTDF,ISTAT)
          call FINDFIL(lipvdatf,XST)
          if(XST)then
            ipvact='ipv'
            call ripvdat(IUTDF,lipvdatf,ipvact,ier)
            CALL ERPFREE(IUTDF,ISTAT)
          endif
        elseif(ND.ge.3)then

C The IPV data line needs to be scanned prior to calling ripvdat
C for the rest of the data.
          K=0
          CALL EGETW(LOUTSTR,K,WORD,'W','*IPV tag',IFLAG)
          CALL EGETWI(LOUTSTR,K,ipvversion,0,4,'W','version index',IER)
          CALL EGETWI(LOUTSTR,K,ipvform,0,2,'W','ipv rep format',IER)
          if(ND.gt.3)then
            CALL EGETW(LOUTSTR,K,ipvsimu,'W','*IPV assessment',IFLAG)
          else
            ipvsimu='------'
          endif
          lipvdatf='internal'  ! reset file name to the keyword internal
          ipvact='cfg'
          call ripvdat(iuc,FILE,ipvact,ier)
        endif
      elseif(WORD(1:5).eq.'*dmds')then

C Building fans/pumps/DHW demands file.
        CALL EGETRM(LOUTSTR,K,bdmds,'W','bdmds file',IER)
      elseif(WORD(1:7).eq.'*cpcalc')then

C CPCALC global information.
        CALL EGETWR(LOUTSTR,K,ble,0.,0.,'-','cpccfg ble',IER)
        CALL EGETWR(LOUTSTR,K,bwi,0.,0.,'-','cpccfg bwi',IER)
        CALL EGETWR(LOUTSTR,K,bhi,0.,0.,'-','cpccfg bhi',IER)
        CALL EGETWR(LOUTSTR,K,blox,0.,0.,'-','cpccfg blox',IER)
        CALL EGETWR(LOUTSTR,K,bloy,0.,0.,'-','cpccfg bloy',IER)
        CALL EGETWR(LOUTSTR,K,bloz,0.,0.,'-','cpccfg bloz',IER)
        CALL EGETWR(LOUTSTR,K,orient,0.,0.,'-','cpccfg orient',IER)
        CALL EGETWI(LOUTSTR,K,irt,0,0,'-','cpccfg irt',IER)
        CALL EGETWR(LOUTSTR,K,ra,0.,0.,'-','cpccfg ra',IER)
        CALL EGETWR(LOUTSTR,K,sbh,0.,0.,'-','cpccfg sbh',IER)
        CALL EGETWR(LOUTSTR,K,pad,0.,0.,'-','cpccfg pad',IER)
        CALL EGETWR(LOUTSTR,K,wvpe,0.,0.,'-','cpccfg wvpe',IER)
        icpcon=1
      elseif(WORD(1:5).eq.'*rif ')then

C Radiance scene file.
        CALL EGETRM(LOUTSTR,K,lradcf,'W','radiance scene file',IER)
      elseif(WORD(1:4).eq.'*ncm')then

C SBEM project specific NCM description file.
        CALL EGETW(LOUTSTR,K,LASBEM,'W','SBEM file',IER)
        ISBEM=2  ! to signal we have NCM description
        IRNCMF=1
      elseif(WORD(1:5).eq.'*not ')then

C Flag to check if UK NCM notional model
        INOTI=1
      elseif(WORD(1:4).eq.'*ref')then

C Flag to check if UK NCM reference model
        INOTI=2
      elseif(WORD(1:4).eq.'*typ')then

C Flag to check if UK NCM typical model
        INOTI=3
      elseif(WORD(1:4).eq.'*str')then

C Flag to check if UK NCM stripped model
        INOTI=4
      elseif(WORD(1:4).eq.'*nlt')then

C Non-linear thermophysical properties configuration file.
        INTHPS=.TRUE.
        CALL EGETRM(LOUTSTR,K,LNLTHP,'W','non-linear prop',IER)

C Simulation presets: Optionally parameters normally hidden in the password-
C protected simulation toggles directory.
      elseif ( loutstr(1:11).eq. '*sim-toggle' ) then
 
C Count number of words on this line. Should be three:         
C         *sim-toggle KEYWORD VALUE    
        iWordCount = iEGetArrW ( loutstr, cLnWords )
        
        bError = .false. 
        
        RightWordCount: if ( iWordCount == 3 ) then 

C Match keyword against known list        
          MatchKeywords: 
     &    if ( bStringsMatch(cLnWords(2),'bld-soln-implicitness')) then

C Building equations implicitness-explicitness degree.             
            bUserGAM = .true. 
            fUserGAM = fCtoR_err( cLnWords(3),
     &                           'Building equations implicitness',
     &                           bError ) 
            if ( fUserGAM < 0. .or. fUserGAM > 1. ) bError = .true. 
      
C Flag for solar processing.
          elseif ( 
     &       bStringsMatch(cLnWords(2),'bld-solar-processing')) then 
             bUserSolar = .true. 
             iUserSolar = -1 
             ParseSolarTag: 
     &       if ( bStringsMatch(cLnWords(3),'enabled' )) then 
               iUserSolar = 0
             elseif ( bStringsMatch(cLnWords(3),'disabled')) then
               iUserSolar = 1
             else 
               bError = .true.
             endif ParseSolarTag      
          elseif ( 
     &       bStringsMatch(cLnWords(2),'plt-max-iterations')) then 
             bUserPltSolvConfig = .true. 
             iUserMAXITP = iCtoI_err( cLnWords(3),
     &                           'Max plant iterations',
     &                           bError ) 
             if ( iUserMAXITP  < 0. ) bError = .true.   
          elseif ( 
     &       bStringsMatch(cLnWords(2),'plt-clt-max-iterations')) then 
             bUserPltSolvConfig = .true.    
             iUserITRCLP = iCtoI_err( cLnWords(3),
     &                                'Max plant control iterations',
     &                                bError ) 
             if ( iUserITRCLP  < 0. ) bError = .true.      
          elseif ( 
     &       bStringsMatch(cLnWords(2),'plt-max-error-relative')) then 
             bUserPltSolvConfig = .true.               
             fUserPERREL = fCtoR_err( cLnWords(3),
     &                           'Plant soln max relative error (-) ',
     &                           bError ) 
             if ( fUserPERREL  < 0. ) bError = .true.               
          elseif ( 
     &       bStringsMatch(cLnWords(2),'plt-max-error-temp')) then 
             bUserPltSolvConfig = .true.             
             fUserPERTMP = fCtoR_err( cLnWords(3),
     &                         'Plant soln max temperature error (oC) ',
     &                           bError ) 
             if ( fUserPERTMP < 0. ) bError = .true. 
          elseif ( 
     &       bStringsMatch(cLnWords(2),'plt-max-error-flux')) then 
             bUserPltSolvConfig = .true. 
             fUserPERFLX = fCtoR_err( cLnWords(3),
     &                         'Plant soln max heat flux error (W) ',
     &                           bError ) 
             if ( fUserPERFLX < 0. ) bError = .true.              
          elseif ( 
     &       bStringsMatch(cLnWords(2),'plt-max-error-flow')) then 
             bUserPltSolvConfig = .true.  
             fUserPERMFL = fCtoR_err( cLnWords(3),
     &                         'Plant soln max mass flow error (kg/s) ',
     &                         bError ) 
             if ( fUserPERTMP < 0. ) bError = .true.              
          elseif ( 
     &       bStringsMatch(cLnWords(2),'plt-max-error-H2-flow')) then  
             bUserPltSolvConfig = .true.             
             fUserH2iterTol= fCtoR_err( cLnWords(3),
     &                     'Plant soln max H2 mass flow error (kg/s) ',
     &                         bError ) 
             if ( fUserH2iterTol < 0. ) bError = .true.                  
          endif MatchKeywords
        endif RightWordCount
        SimToggleError: if ( bError ) then        
          if ( .not. bSilent ) then 
            call usrmsg(
     &           ' Could not parse simulation toggle ',loutstr,'W')
          else 
            ier=1 
            return
          endif  
        endif SimToggleError   
      elseif(WORD(1:4).eq.'*g1d')then
  
C Building 1D constuction 
        IGR1D=.TRUE.
        CALL EGETRM(LOUTSTR,K,LGRD1D,'W','bldng 1D gridding',IER)
      elseif(WORD(1:6).eq.'*biopt')then

C Bi-directional optical data. Read how many data to read followed
C by the raw-datafile name and then scan for
C a connection-based list on the next line(s).
        ncn=0
        CALL EGETWI(LOUTSTR,K,ncn,0,0,'-','nb items/connections',IER)
        CALL EGETRM(LOUTSTR,K,bidirfile,'W','bioptical file',IER)
        IF(NCN.GT.0) THEN
          CALL EGETWIA(IUC,IRVC,NCN,0,1,'W','bi-optical list',IER)
          DO 114 KV=1,NCN
            NSTMCFL(KV)=IRVC(KV)
  114     CONTINUE
        endif
      elseif(WORD(1:4).eq.'*spf')then

C Special materials and electrical bus description files.
        CALL EGETRM(LOUTSTR,K,spflnam,'W','special mats',IER)
        ispmxist=1
      elseif(WORD(1:4).eq.'*pnt')then
        CALL EGETRM(LOUTSTR,K,entflnam,'W','power netwk',IER)
        ientxist=2
      elseif(WORD(1:6).eq.'*resh2')then

C H2-dep_controller:
C Residential hydrogen cogen: control file
C The hydrogen system controller code is experimental. 
C We'll parse the file name, and write the *resh2 tag 
C back out when when the cfg file is saved, but 
C we won't do anything else at present.
        CALL EGETRM(LOUTSTR,K,resh2flnam,'W','res h2 ctl',IER)
        iresh2ctl = 1
      elseif(WORD(1:6).eq.'*resel')then

C Renewable energy system (elec) controller control file
C The RES (elec) system controller code is experimental. 
        CALL EGETRM(LOUTSTR,K,reselecflnam,'W','res elec ctl',IER)
        ireselecctl = 1
      elseif(WORD(1:4).eq.'*sps' .or. WORD(1:6).eq. '*mysps')then
        call scan_sps(IUC,ier)     ! Process simulation paramater sets.
      elseif(WORD(1:8).eq.'*end_sps' .or.
     &       WORD(1:12).eq.'*end_my_sets' )then
        goto 34
      elseif(WORD(1:7).eq.'*intipv')then
        CALL EGETRM(LOUTSTR,K,sipvres,'W','ipv report',IER)
      elseif(loutstr(1:10).eq.'* PROJ LOG')then

C Project log file, read and then go on to fabric/plant description.
        CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'notes file',IER)
        write(lmodellog,'(a)') LOUTSTR(1:LNBLNK(LOUTSTR))
        goto 45

      elseif(loutstr(1:6).eq.'*notes')then

C Project notes file, read and then go on to fabric/plant description.
        CALL EGETRM(LOUTSTR,K,lmodellog,'W','notes file',IER)
        goto 45
      
      elseif ( .not. bSilent ) then

C Ish has a file unit clash with help text scan so manually set
C the h() arrays.
        call usrmsg(' Unknown entry format... ',loutstr,'W')
        dok=.true.
        h(1)='An unknown tag or line was found in the configuration'
        h(2)='file. The file might be corrupt or for a different'
        h(3)='version of ESP-r. '
        h(4)=' '
        h(5)='Depending on the error, it might be possible to read'
        h(6)='the rest of the file - exercise caution! '
        nbhelp=6
        CALL EASKOK(' ','Continue reading configuration file?',OK,
     &     nbhelp)
        if(.NOT.ok)then
          ier=1
          return
        endif
      else
        call edisp(iuout,'Unknown entry in .cfg file:')
        call edisp(iuout, WORD)
        call edisp248(iuout,loutstr,90)
        call edisp(iuout,'or possibly')
        call edisp(iuout,outstr)
        ier=1
        return
      endif

C Loop back for next database file.
      goto 34
      
C End of initial portion of cfg scanning.  The next section deals
C with zone or plant details.

 45   continue
 451  continue

      IF(INDCFG.EQ.2)goto 888  ! If plant only jump

C If V3 or V4 building or building & plant then read in the building info.
C At this point there may be ground information, if abscent then
C this is the `* Building` section.
      if(icfgv.lt.5)then
        CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'building name',IER)
        IF(IER.NE.0)goto 1
        if(LOUTSTR(1:8).eq.'* Ground'.or.LOUTSTR(1:8).eq.'* GROUND'.or.
     &     LOUTSTR(1:8).eq.'*ground ')then

C Scan the ground entries and if no errors jump to continue reading tokens.
          call scan_ground(IUC,ITRC,IGDCVS,IGDCNC,IGDNDC,IGDTAQ,ier)
          if(ier.eq.0) goto 45
        elseif(LOUTSTR(1:10).eq.'* Building'.or.
     &         LOUTSTR(1:10).eq.'* BUILDING'.or.
     &         LOUTSTR(1:9).eq.'*building')then
          CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'model name',IER)
          IF(IER.NE.0)goto 1
          modeltitle=LOUTSTR(1:72)
          havetitile=.true.
        else
          modeltitle=LOUTSTR(1:72)
          havetitile=.true.
        endif
      endif

C If ground information was read in but still registration level then exit.
      if(INDCFG.EQ.0)then
        CALL ERPFREE(IUC,ISTAT)
        RETURN
      endif

C If mould database tag was not found instantiate it.
      if(foundmould)then
        continue
      else
        call findwhichdbpath('mld',dmdbnam,ier)
      endif

C If SBEM database tag was not found instantiate it.
      if(foundsbem)then
        continue
      else
        call findwhichdbpath('sbm',DSBEM,ier)
        ISBEM=1
        IRNCMD=1
      endif

C Number of building zones.
      CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'no of zones',IER)
      IF(IER.NE.0)goto 1
      if(LOUTSTR(1:7).eq.'*zones ')then
        k=7
      else
        K=0
      endif
      CALL EGETWI(LOUTSTR,K,NCOMP,0,MCOM,'W','zones',IER)

C If greater complexity than executables can deal with
C set ier=3 and generate a message.
      if(ncomp.gt.MCOM)then
        write(outs,'(a,i3,a,i3,a)') 'Model complexity ',ncomp,
     &    ' zones is GREATER THAN ',MCOM,'!'
        call edisp(iuout,outs)
        call edisp(iuout,'Either re-compile or simplify model.')
        IER=3
        RETURN
      endif
      IF(IER.NE.0) GOTO 1

C Reporting.
      IF(ITRC.GE.1)THEN
        WRITE(OUTS,'(2a)')' The model title is ',
     &    modeltitle(1:LNBLNK(modeltitle))
        CALL EDISP(iuout,OUTS)
        CALL EDISP(iuout,' ')
        if(NCOMP.gt.10)then
          call edisp(iuout,' Please wait.')
        endif
        WRITE(OUTS,9994)NCOMP
 9994     FORMAT(' It is composed of ',I2,' thermal zones which are',
     &    ' described in the following files:')
        CALL EDISP(iuout,OUTS)
        CALL EDISP(iuout,' ')
      ENDIF

C Zone file names.
      icomp=0
   46 if(icfgv.lt.3)then
        msgl2='File format is pre-1996 and no longer supported.'
        call usrmsg('Detected an ancient model cfg file.',
     &     'File format is pre-1996 and no longer supported.','W')
        CALL ERPFREE(IUC,ISTAT)
        RETURN
      endif
      CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'zone data',IER)
      IF(IER.NE.0)goto 1
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','tags',IFLAG)
      if(WORD(1:5).ne.'*zend')then
        CALL EGETRM(LOUTSTR,K,text,'W','remaining text',IFLAG)
      endif
      IF(IFLAG.NE.0)goto 1
      if(WORD(1:4).eq.'*zon')then
        OK3DCVS=.FALSE.
        OK3DCNC=.FALSE.
        OK3DNDC=.FALSE.
        OK3DTAQ=.FALSE.
        icomp=icomp+1
        ZONE3D(icomp)=.FALSE.
        K=5
        CALL EGETWI(LOUTSTR,K,NCCODE(ICOMP),1,100,'W','zone index',IER)
        IF(IER.NE.0) GOTO 1

C If the line includes 'water' or 'Water' then set to true and also
C set the fluid K D C A. This should reduce need to go into simulation
C toggles to set water filled zones.
        if(ND.gt.2)then
          CALL EGETW(LOUTSTR,K,WORD2,'W','air or water',IFLAG)
          if(WORD2(1:5).eq.'water'.or.WORD2(1:5).eq.'Water')then
            znotair(icomp)=.TRUE.
            zSWAp(icomp)=0.0; zSWAf(icomp)=0.0
          else
            znotair(icomp)=.FALSE.  ! usual assumption of air filled
          endif
        else
          znotair(icomp)=.FALSE.  ! usual assumption of air filled
        endif
      elseif(WORD(1:4).eq.'*opr')then
        LPROJ(icomp)=text(1:72)
      elseif(WORD(1:9).eq.'*pausecas')then

C Flag to pause assessment at each timestep to allow for
C a zone casual gain to be adapted. No impact if tag missing.
        ipausecas(icomp)=1
      elseif(WORD(1:4).eq.'*geo')then
        LGEOM(icomp)=text(1:72)
      elseif(WORD(1:4).eq.'*con')then
        LTHRM(icomp)=text(1:72)
      elseif(WORD(1:4).eq.'*utl')then
        msgl2='This is no longer supported. Skipping.'
        call usrmsg('Detected a legacy zone utility file.',
     &    msgl2,'W')
      elseif(WORD(1:4).eq.'*obs')then
        iobs(icomp)=1
        ZOBS(icomp)=text(1:72)
      elseif(WORD(1:4).eq.'*tmc')then
        itw(icomp)=1
        LTWIN(icomp)=text(1:72)
      elseif(WORD(1:4).eq.'*cfc')then
        icfc(icomp)=1
        lcfcin(icomp)=text(1:72)
      elseif(WORD(1:4).eq.'*ivf')then
        ivf(icomp)=1
        LVIEW(icomp)=text(1:72)
      elseif(WORD(1:4).eq.'*cgc')then
        icgc(icomp)=1
        LCGCIN(icomp)=text(1:72)
      elseif(WORD(1:4).eq.'*ise')then  ! embedded S/I calculation
        isi(icomp)=1
        ISIcalc=1
        LSHAD(icomp)=text(1:72)

C Ensure ../tmp exists and that an empty file with the name of the S/I file
C referenced in the cfg file is placed there.
        write(doit,'(4a)') 'mkdir -p ../tmp; rm -f ',
     &        LSHAD(icomp)(1:lnblnk(LSHAD(icomp))),'; echo " " > ',
     &        LSHAD(icomp)(1:lnblnk(LSHAD(icomp)))
        call runit(doit,'-')
      elseif(WORD(1:4).eq.'*isi')then  ! existing S/I file
        isi(icomp)=1
        ISIcalc=2
        LSHAD(icomp)=text(1:72)
      elseif(WORD(1:4).eq.'*ihc')then
        ihc(icomp)=1
        LHCCO(icomp)=text(1:72)
      elseif(WORD(1:4).eq.'*ibc')then
        msgl2='This is no longer supported. Skipping.'
        call usrmsg('Detected a default window bind control descr.',
     &    msgl2,'W')
      elseif(WORD(1:4).eq.'*cfd')then
        LCFD(icomp)=text(1:72)   ! conflation type
        call FINDFIL(LCFD(ICOMP),XST)
        if(XST)then
          write(currentfile,'(a)') LCFD(ICOMP)(1:lnblnk(LCFD(ICOMP)))
          CALL EFOPSEQ(IUF,LCFD(ICOMP),1,IER)
          IF(IER.NE.0)goto 1
          CALL LSTRIPC(IUF,LOUTSTR,0,ND,1,'dfd line 1',IER)
          if(LOUTSTR(1:15).eq.'DFS DESCRIPTION')then
            CALL LSTRIPC(IUF,LOUTSTR,0,ND,1,'dfd line 2',IER)
            K=0
            CALL EGETW(LOUTSTR,K,WORD,'W','tag',IFLAG)
            if(WORD(1:11).eq.'*conflation')then
              CALL EGETWI(LOUTSTR,K,iv,0,7,'F','confla type',IER)
              IFCFD(icomp)=iv
            else
              call usrmsg('DFD (ver 1) conflation tag missing in:',
     &          LCFD(ICOMP),'W')
            endif
          elseif(LOUTSTR(1:7).eq.'*DFS V2')then
            CALL LSTRIPC(IUF,LOUTSTR,0,ND,1,'dfd line 2',IER)
            K=0
            CALL EGETW(LOUTSTR,K,WORD,'W','tag',IFLAG)
            if(WORD(1:11).eq.'*conflation')then
              CALL EGETWI(LOUTSTR,K,iv,0,7,'F','confla type',IER)
              IFCFD(icomp)=iv
            else
              call usrmsg('DFD (ver 2) conflation tag missing in:',
     &        LCFD(ICOMP),'W')
            endif
          else
            call usrmsg('Referenced file not a DFD description:',
     &        LCFD(ICOMP),'W')
          endif
          CALL ERPFREE(IUF,ISTAT)
          NCONF=NCONF+1
          icfdnd(NCONF)=ICOMP
          write(currentfile,'(a)') FILE(1:lnblnk(FILE))
        else
          call usrmsg('DFD file not found:',LCFD(ICOMP),'W')
        endif
      elseif(WORD(1:4).eq.'*cvs')then
        OK3DCVS=.true.
        L3DCVS(icomp)=text(1:72)
        if(OK3DCVS.and.OK3DCNC.and.OK3DNDC.and.OK3DTAQ)then
          ZONE3D(icomp)=.TRUE.
        endif
      elseif(WORD(1:4).eq.'*cnc')then
        OK3DCNC=.true.
        L3DCNC(icomp)=text(1:72)
        if(OK3DCVS.and.OK3DCNC.and.OK3DNDC.and.OK3DTAQ)then
          ZONE3D(icomp)=.TRUE.
        endif
      elseif(WORD(1:4).eq.'*ndc')then
        OK3DNDC=.true.
        L3DNDC(icomp)=text(1:72)
        if(OK3DCVS.and.OK3DCNC.and.OK3DNDC.and.OK3DTAQ)then
          ZONE3D(icomp)=.TRUE.
        endif
      elseif(WORD(1:4).eq.'*3dt')then
        OK3DTAQ=.true.
        L3DTAQ(icomp)=text(1:72)
        if(OK3DCVS.and.OK3DCNC.and.OK3DNDC.and.OK3DTAQ)then
          ZONE3D(icomp)=.TRUE.
        endif
      elseif(WORD(1:4).eq.'*mst')then
        MSTRZN(icomp)=.true.
        LMOIST(icomp)=text(1:72)
        MSTROK=.TRUE.
      elseif(WORD(1:4).eq.'*shz')then  ! SHOCC zone file
        SHOCCshzFile(icomp)=text(1:72)
        XST=.FALSE.
        call FINDFIL(SHOCCshzFile(icomp),XST)
        if(XST) then
          bZoneSHOCCed(icomp)=.true.
        else
          write(outs,'(3a)') 'SHOCC zone file ',
     &      SHOCCshzFile(icomp)(1:lnblnk(SHOCCshzFile(icomp))),
     &      ' not found!'
        endif
      elseif(WORD(1:4).eq.'*bsm')then  ! BASESIMP description
        LBSIMP(icomp)=text(1:72)
        XST=.FALSE.
        call FINDFIL(LBSIMP(icomp),XST)
        if(XST)then
          iBSIMP(icomp)=1
        else
          write(outs,'(3a)') 'BASESIMP file ',
     &      LBSIMP(icomp)(1:lnblnk(LBSIMP(icomp))),' not found!'
            call edisp(iuout,outs)
          iBSIMP(icomp)=0
        endif
      elseif(WORD(1:4).eq.'*csm')then
        if(IndxSt.eq.0)then
          IndxSt=icomp
          LGrdSt=text(1:72)
        else
          msgl2='Only the 1st structured zone considered.'
          CALL USRMSG('Multiple structured zones!',msgl2,'W')
        endif
      elseif(WORD(1:5).eq.'*zend')then
C        if(OK3DCVS.and.OK3DCNC.and.OK3DNDC.and.OK3DTAQ)then
C          ZONE3D(icomp)=.TRUE.
C        else
C          ZONE3D(icomp)=.FALSE.
C        endif
        if(icomp.lt.ncomp)then  ! loop back for next zone
          goto 46
        else
          goto 142        ! scan geometry files.
        endif
      elseif(WORD(1:13).eq.'*building_end')then
          goto 142
      elseif(WORD(1:4).eq.'*cnn')then
        if(did142)then      ! if *zend was found
          goto 23           ! files already scanned
        else
          if(icfgv.ge.5)then
            backspace(IUC)  ! rewind two lines
            backspace(IUC)
            goto 142        ! scan geometry files.
          else
            backspace(IUC)  ! rewind one line
            goto 142        ! scan geometry files.
          endif
        endif
      else
        call usrmsg('Unknown configuration file tag:',WORD,'W')
      endif
      goto 46

C Geometry file, get or derive the zone name via reading the file.
C Make temporary use of file unit IUF. 
 142  do 22 ICOMP=1,NCOMP
        zname(ICOMP)='UNKNOWN'
        call FINDFIL(LGEOM(ICOMP),XST)

        IF(XST)THEN
          write(currentfile,'(a)') LGEOM(ICOMP)(1:lnblnk(LGEOM(ICOMP)))
          CALL ERPFREE(IUF,ios)
          call georead(iuf,LGEOM(ICOMP),ICOMP,1,itru,ier)
          write(currentfile,'(a)') FILE(1:lnblnk(FILE))
        ELSE
          ZNAME(ICOMP)='UNKNOWN'
          write(outs,'(a)')LGEOM(ICOMP)
          CALL USRMSG(' geometry file not found ',outs,'W')
          NZSUR(ICOMP)=0
          NZTV(ICOMP)=0
          VOL(ICOMP)=0.0
          lnzname(ICOMP)=0
        ENDIF

C Fill in a default zone name if blank or UNKNOWN.
        IF(zname(ICOMP)(1:2).EQ.'  '.OR.
     &     zname(ICOMP)(1:7).EQ.'UNKNOWN')THEN
          IF(ICOMP.LE.9)WRITE(zname(ICOMP),'(A5,I1)')'Zone-',ICOMP
          IF(ICOMP.GT.9.and.ICOMP.LE.99)then
            WRITE(zname(ICOMP),'(A5,I2)')'Zone-',ICOMP
          ENDIF
          IF(ICOMP.GT.99)WRITE(zname(ICOMP),'(A5,I3)')'Zone-',ICOMP
          lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update string length
        ENDIF
        if(zdesc(ICOMP)(1:1).EQ.' ')then
          write(zdesc(ICOMP),'(2a)') 
     &      zname(ICOMP)(1:lnzname(ICOMP)),' describes a'
          lnzdesc(ICOMP)=lnblnk(zdesc(ICOMP))  ! update string length
        endif

C Reporting.
C << This could be in a separate subroutine.
        IF(ITRC.GE.1.AND.ITRC.LT.4)THEN
          WRITE(OUTS,'(a,i3,a,i3,2a)',IOSTAT=ISTAT,ERR=1) ' Zone',
     &        ICOMP,' index',NCCODE(ICOMP),'Named: ',zname(ICOMP)
          CALL EDISP(iuout,OUTS)
          WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Operations: ',
     &        LPROJ(ICOMP)(1:LNBLNK(LPROJ(ICOMP)))
          CALL EDISP(iuout,OUTS)
          WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Geometry: ',
     &        LGEOM(ICOMP)(1:LNBLNK(LGEOM(ICOMP)))
          CALL EDISP(iuout,OUTS)
          WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Constructions: ',
     &        LTHRM(ICOMP)(1:LNBLNK(LTHRM(ICOMP)))
          CALL EDISP(iuout,OUTS)
          IF(IOBS(ICOMP).EQ.1)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Obstructions: ',
     &          ZOBS(ICOMP)(1:LNBLNK(ZOBS(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ELSEIF(IOBS(ICOMP).EQ.2)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Obstructions: ',
     &          ' in zone geometry.'
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(ITW(ICOMP).EQ.1)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Transp. constr: ',
     &          LTWIN(ICOMP)(1:LNBLNK(LTWIN(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(icfc(ICOMP).EQ.1)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' CFC constr: ',
     &          lcfcin(ICOMP)(1:LNBLNK(lcfcin(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(IVF(ICOMP).EQ.1)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' View factors: ',
     &          LVIEW(ICOMP)(1:LNBLNK(LVIEW(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(ICGC(ICOMP).EQ.1)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Casual contrl: ',
     &          LCGCIN(ICOMP)(1:LNBLNK(LCGCIN(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(ISI(ICOMP).EQ.1)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Shading/Ins: ',
     &          LSHAD(ICOMP)(1:LNBLNK(LSHAD(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(IHC(ICOMP).EQ.1)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Convect. coeff: ',
     &          LHCCO(ICOMP)(1:LNBLNK(LHCCO(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(bZoneSHOCCed(ICOMP))THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' SHOCC inputs: ',
     &          SHOCCshzFile(ICOMP)(1:LNBLNK(SHOCCshzFile(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(iBSIMP(ICOMP).EQ.1)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' BASESIMP inputs: ',
     &          LBSIMP(ICOMP)(1:LNBLNK(LBSIMP(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(IFCFD(ICOMP).GT.0)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' CFD domain: ',
     &          LCFD(ICOMP)(1:LNBLNK(LCFD(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(MSTRZN(ICOMP))THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Moisture data: ',
     &          LMOIST(ICOMP)(1:LNBLNK(LMOIST(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          if(ZONE3D(ICOMP))then
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' 3D volumes: ',
     &          L3DCVS(ICOMP)(1:LNBLNK(L3DCVS(ICOMP)))
            CALL EDISP(iuout,OUTS)
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' 3D connections: ',
     &          L3DCNC(ICOMP)(1:LNBLNK(L3DCNC(ICOMP)))
            CALL EDISP(iuout,OUTS)
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' 3D coordinates: ',
     &          L3DNDC(ICOMP)(1:LNBLNK(L3DNDC(ICOMP)))
            CALL EDISP(iuout,OUTS)
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' 3D temps: ',
     &          L3DTAQ(ICOMP)(1:LNBLNK(L3DTAQ(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          if(IndxSt.eq.icomp)then
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Structured mesh: ',
     &          LGrdSt(1:LNBLNK(LGrdSt))
            CALL EDISP(iuout,OUTS)
          endif
        endif
  22  continue
      did142=.true.

C Read in the building connectivity information.
      CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'connections file',IER)
      IF(IER.NE.0)goto 1

C Jump to 23 if the building type model does not have any zones
C so the subsequent line might be *cnn.
  23  if(LOUTSTR(1:4).eq.'*cnn')then
        cnnok=.true.
        K=0
        CALL EGETW(LOUTSTR,K,WORD,'W','connections tag',IER)
        CALL EGETRM(LOUTSTR,K,LCNN,'W','connections file',IER)
        call FINDFIL(LCNN,XST)
        if(XST)then
          write(currentfile,'(a)') LCNN(1:lnblnk(LCNN))
          CALL EFOPSEQ(IUF,LCNN,1,IER)
          IF(IER.NE.0)goto 1
          CALL LSTRIPC(IUF,LOUTSTR,0,ND,1,'connex file header',IER)
          if(LOUTSTR(1:12).ne.'*connections')then
            CALL USRMSG(' not a connections file ',LCNN,'W')
            goto 1
          endif
          CALL LSTRIPC(IUF,LOUTSTR,0,ND,1,'connex file date',IER)
          CALL LSTRIPC(IUF,LOUTSTR,0,ND,1,'no of connex',IER)
          K=0
          CALL EGETWI(LOUTSTR,K,NCON,3,MCON,'W','no connex',IER)
          IF(IER.NE.0) GOTO 1
        else
          if(NCOMP.eq.0)then
            goto 6     ! if there are no zones then no cnn file
          else
            CALL USRMSG('Connections file not found!',LCNN,'W')
            IER=1
            RETURN
          endif
        endif
      else
        cnnok=.false.
        K=0
        CALL EGETWI(LOUTSTR,K,NCON,3,MCON,'W','no connection',IER)
        IF(IER.NE.0) GOTO 1
      endif

C Reporting: generate header for connections.
      IF(ITRC.GE.1.AND.ITRC.LT.4)THEN
        CALL EDISP(iuout,' ')
        WRITE(OUTS,'(a,i4)')' Number of surfaces in the model =',NCON
        CALL EDISP(iuout,OUTS)
      ENDIF

      DO 21 ICON=1,NCON

C Read from either the connections or configuration file.

C << Connections are always in an separate file.
        if(cnnok)then
          write(currentfile,'(a)') LCNN(1:lnblnk(LCNN))
          CALL LSTRIPC(IUF,LOUTSTR,0,ND,1,'connections',IER)
        else
          write(currentfile,'(a)') FILE(1:lnblnk(FILE))
          CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'connections',IER)
        endif
        IF(IER.NE.0)goto 1
        K=0
        CALL EGETWI(LOUTSTR,K,IC1(ICON),1,MCOM,'F','conn src zone',IER)
        IF(IER.NE.0) GOTO 1
        CALL EGETWI(LOUTSTR,K,IE1(ICON),1,MS,'F','conn src surf',IER)
        IF(IER.NE.0) GOTO 1
        CALL EGETWI(LOUTSTR,K,ICT(ICON),-1,7,'F','conn type',IER)
        IF(IER.NE.0) GOTO 1
        CALL EGETWI(LOUTSTR,K,IC2(ICON),0,0,'-','conn other zone',IER)
        IF(IER.NE.0) GOTO 1
        CALL EGETWI(LOUTSTR,K,IE2(ICON),0,0,'-','conn other surf',IER)
        IF(IER.NE.0) GOTO 1

C If ground connection IC2=0 then IE2 is the profile reference.
        IF(ICT(ICON).EQ.4)THEN
          IF(IC2(ICON).EQ.0.AND.IE2(ICON).GT.NGRDP)THEN
            CALL USRMSG(
     &      'Ground connection referenced an unknown monthly profile!',
     &        LOUTSTR,'W')
          ENDIF
          IF(IC2(ICON).EQ.-3.AND..NOT.GRND3D)CALL USRMSG(
     &    '3D ground model not available but referenced in',LOUTSTR,'W')
        ENDIF

C Range checking.
        IF(ICT(ICON).EQ.0)THEN
          IF(IC2(ICON).NE.0.OR.IE2(ICON).NE.0) CALL USRMSG(
     &      'Ext conn (4th & 5th items) should be 0 in',LOUTSTR,'W')
        ENDIF
        IF(ICT(ICON).EQ.3)THEN
          IF(IC2(ICON).LT.1.OR.IC2(ICON).GT.MCOM)CALL USRMSG(
     &      'Int conn other zone (4th item) unknown in',LOUTSTR,'W')
          IF(IE2(ICON).LT.1.OR.IE2(ICON).GT.MS)CALL USRMSG(
     &      'Int conn other surf (5th item) unknown in',LOUTSTR,'W')
        ENDIF
   21 CONTINUE

C Close connections file and build zone:surface connections hash array.
      if(cnnok)CALL ERPFREE(IUF,ISTAT)
      do 42 icc = 1, NCON
        IZSTOCN(IC1(icc),IE1(icc))=icc
 42   continue

C With hash known, re-scan geometry files (connection based
C surface attributes will be instantiated during geometry read).
C Warn if izstocn returns zero. Recover zone volume and surface
C derived data. Use georead or egomin depending on version.
      do 242 ICOMP=1,NCOMP
        call FINDFIL(LGEOM(ICOMP),XST)
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(XST)then
          if(newgeo)then

C If MODE='ALL ' fill obstruction data structures
C but there is a case where a version 1.1 geometry file might
C not yet include obstruction blocks that were in legacy zone
C obstructions files. If this is the case then scan them.
            write(currentfile,'(a)') 
     &        LGEOM(icomp)(1:lnblnk(LGEOM(icomp)))
            call georead(iuf,LGEOM(ICOMP),ICOMP,1,itru,ier)
            if(MODE(1:3).eq.'ALL'.or.MODE(1:3).eq.'all')then
              if(iobs(icomp).eq.1)then
                call FINDFIL(ZOBS(ICOMP),XST)
                if(XST)then
                  CALL ERPFREE(IUF,ISTAT)
                  CALL EGOMST(iuf,ICOMP,ZOBS(ICOMP),0,ITRC,itru,IER)
                endif
              elseif(iobs(icomp).eq.2)then
                continue
              endif
            endif
          else
    
C For MODE='ALL ' also scan obstruction blocks from zone
C obstruction file if older model.
            call egomin(iuf,LGEOM(ICOMP),ICOMP,1,0,itru,ier)
            if(MODE(1:3).eq.'ALL'.or.MODE(1:3).eq.'all')then
              if(iobs(icomp).eq.1)then
                call FINDFIL(ZOBS(ICOMP),XST)
                if(XST)then
                  CALL ERPFREE(IUF,ISTAT)
                  CALL EGOMST(iuf,ICOMP,ZOBS(ICOMP),0,ITRC,itru,IER)
                endif
              elseif(iobs(icomp).eq.2)then
                continue
              endif
            endif
          endif
          DO 243 IS=1,NZSUR(ICOMP)
            icc=IZSTOCN(icomp,is)
            if(icc.eq.0)then
              write(outs,'(a,i3,a,i3)')
     &          'zero icc associated with zone ',
     &          icomp,' & surface',is
              call edisp(itru,outs)
              goto 243
            endif
 243      continue

C Call to zgupdate is after connection lists established.
          call zgupdate(0,icomp,ier)
    
C For MODE='ALL ' also find co-planer surfaces & edges of similar materials.
          if(MODE(1:3).eq.'ALL'.or.MODE(1:3).eq.'all')then
            call suredgeadj(itrc,'-',icomp,ier)

C Instantiate the szcoords and iszjvn arrays.
            DO 40 J=1, NZTV(ICOMP)
              szcoords(ICOMP,J,1)=X(J)
              szcoords(ICOMP,J,2)=Y(J)
              szcoords(ICOMP,J,3)=Z(J)
   40       CONTINUE
            DO 50 J=1, nzsur(ICOMP)
              icc=IZSTOCN(icomp,j)
              if(icc.ne.0)then
                isznver(icomp,J)=NVER(J)
                N = isznver(icomp,J)
                DO 60 K=1,N
                  iszjvn(icomp,J,K)=JVN(J,K)
   60           CONTINUE
              endif
   50       CONTINUE
          endif

C Trace geometry related information. Because not all zones will have
c beeen read in yet do not include the extended surface `context`.
          if(ITRC.ne.0.and.ITRC.lt.4)then
            call ZINFOREP(itru,icomp)
            context=.false.
            CALL SURINFO(ICOMP,itru,context)
            IF(ITRC.GT.1.AND.ITRC.LT.4)CALL VERINFO(icomp,itru)
            CALL INSINFO(ICOMP,itru)
          endif
        endif
 242  continue

C Reporting. Connection zone:surface associated data (zone geometry
C will have been scanned in by this point and surface names known).
      IF(ITRC.GE.1.AND.ITRC.LT.4)THEN
        CALL CONXINFO(1,0,CXSTR)
        CALL EDISP(iuout,CXSTR)
        do 41 icc = 1, NCON
          CALL CONXINFO(1,ICC,CXSTR)
          CALL EDISP(iuout,CXSTR)
 41     continue
      ENDIF

C Read in anchors or group-of-zones if available. For each, get 
C its name, number of links and then the links. If the string 
C is not *Anchor or *Group then it is probably the index for flow.
      write(currentfile,'(a)') FILE(1:lnblnk(FILE))
      if(IUC.ne.IUCBK) IUC=IUCBK
      CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'anchors or flow',IER)
      if(IER.eq.2) then
        IER=0  ! In ish this can be a fault point but we have enough
        goto 6
      endif
      IF(IER.NE.0)goto 1
      
      if(LOUTSTR(1:7).eq.'*Anchor')then
        K=8
        CALL EGETWI(LOUTSTR,K,NALOC,0,20,'F','NALOC',IER)
        IF(IER.NE.0) GOTO 1
        do ia=1,NALOC
          CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'anchor name',IER)
          K=0
          CALL EGETW(LOUTSTR,K,WORD,'W','anchor name',IER)
          write(ALOCLBL(ia),'(a12)') WORD(1:12)
          if(ND.gt.2)then
            CALL EGETW(LOUTSTR,K,WORD,'W','anchor type',IER)
            write(ALOCTYP(ia),'(a4)') WORD(1:4)
          else
            write(ALOCTYP(ia),'(a4)') 'UNKN'
          endif
          CALL EGETWI(LOUTSTR,K,IALOC(ia),0,99,'F','anchor link',IER)
          IRVA=IALOC(ia)
          CALL EGETWIA(IUC,IVA,IRVA,0,0,'-','anchor assoc links',IER)
          DO LC=1,IALOC(ia)
            lstanchr(ia,LC)=IVA(LC)
          ENDDO
        enddo
        CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'anchor end',IER)
      elseif(LOUTSTR(1:6).eq.'*Group')then
        K=7
        CALL EGETWI(LOUTSTR,K,nzgroup,0,32,'F','nzgroup',IER)
        IF(IER.NE.0) GOTO 1
        do ia=1,nzgroup
          CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'group name',IER)
          K=0
          CALL EGETP(LOUTSTR,K,WORD,'W','group name',IER)
          write(zglbl(ia),'(a)') WORD(1:16)
          CALL EGETWI(LOUTSTR,K,izgnumber(ia),0,99,'F','how many',IER)
          IRVA=izgnumber(ia)
          CALL EGETWIA(IUC,IVA,IRVA,0,0,'-','group assoc links',IER)
          DO LC=1,izgnumber(ia)
            izglist(ia,LC)=IVA(LC)
          ENDDO
        enddo
        CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'anchor end',IER)
      else

C No anchors or groups so this might be a flow network.
        goto 343
      endif

C Check if there is an fluid (1=text, 2=graphic) air flow network
C available (0 = no).
      CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'flow network index',IER)
      IF(IER.NE.0)goto 1

C A number 0 1 2 3 or *flow legacy   *flow graphic  *flow network  *flow none
 343  if(loutstr(1:10).eq.'*flow none')then
        IAIRN = 0
      elseif(loutstr(1:12).eq.'*flow legacy')then
        IAIRN = 1
      elseif(loutstr(1:13).eq.'*flow graphic')then
        IAIRN = 2
      elseif(loutstr(1:13).eq.'*flow network')then
        IAIRN = 3
      else
        K=0
        CALL EGETWI(LOUTSTR,K,IAIRN,0,3,'F','IAIRN',IER)
        IF(IER.NE.0) GOTO 1
      endif

C Process a fluid (=air) flow network.
      IF(IAIRN.ge.1)THEN

C Read name of flow network file (text or graphic type).
        CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'leakage file',IER)
        LAPROB=LOUTSTR(1:72)
        IF(IER.NE.0)goto 1

C Determine building zone to fluid (=air) network node mapping. Flow nodes
C may be referenced by either name or number.To be able to check whether
C node names are valid, first read the fluid flow model file, which
C is temporarily opened on IUF.
        write(currentfile,'(a)') LAPROB(1:lnblnk(LAPROB))
        CALL EFOPSEQ(IUF,LAPROB,1,IER)
        IF(IER.LT.0)then
          outs='Problem opening flow network file, aborting cfg scan!'
          call edisp(iuout,outs)
          goto 6
        endif

C Reporting.
        IF(ITRC.GE.1.AND.ITRC.LT.4)THEN
          CALL EDISP(iuout,' ')
          CALL EDISP(iuout,'An air flow network file exists:')
          WRITE(OUTS,'(a,a)')LAPROB(1:LNBLNK(LAPROB)),'.'
          CALL EDISP(iuout,OUTS)
        ENDIF

C Fill valid component arrays.
        CALL MFCDAT

C Read the file header and check for first-line tag. If 4 items
C then it is a legacy file so rewind the file and call emfread.
C If LOUTSTR is *Graphical_network, call netread; if LOUTSTR is
C *Flow_network, call EMF3DREAD.
        CALL LSTRIPC(IUF,LOUTSTR,99,ND,0,'1st line of flow file',IER)
        if(IER.NE.0)then
          outs='Problem scanning flow network file, aborting cfg scan!'
          call edisp(iuout,outs)
          goto 6
        endif

C Graphic network flow file, scan it (silently) and transfer data
C to network flow common blocks.
        if(LOUTSTR(1:18).EQ.'*Graphical_network')then
          IAIRN = 2
          CALL ERPFREE(IUF,ISTAT)
          call NETREAD(IUF,'S',IER)
          CALL NETTOFLW(ier)

C Text network flow file, scan it (silently) and transfer data
C to network flow common blocks.
        elseif(LOUTSTR(1:13).EQ.'*Flow_network')then
          IAIRN = 3
          CALL ERPFREE(IUF,ISTAT)
          CALL MFCDAT
          CALL EMF3DREAD(IUF,'S',IER)
        else

C Found legacy text fluid flow file.
          REWIND(IUF,ERR=999)
          CALL EMFREAD(IUF,IER)
          CALL ERPFREE(IUF,ISTAT)
          IAIRN = 1
        endif
        if(IER.NE.0)then
          outs='Problem scanning flow network file, aborting cfg scan!'
          call edisp(iuout,outs)
          goto 6
        endif

C Proceed with building zone to flow network node connections list.
C Read one record into a string. If there is only one word
C on the line then it is probably an older version of the file; try
C reading another line or two to get to list.
        write(currentfile,'(a)') FILE(1:lnblnk(FILE))
        CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'node-zone list',IER)
        if(ND.eq.1.and.NCOMP.ne.1)then
          CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'old pressc.db1',IER)
          if(ND.eq.1)then
            CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'old mass resl',IER)
          endif
        endif
        IF(IER.NE.0)then
          outs='Problem with zone to flow node links in cfg file!'
          dok=.true.
          h(1)='The section of the system configuration file that'
          h(2)='defines the links between building zones and flow'
          h(3)='network nodes has an error.'
          call easkok('Problem with flow node to zone linking!',
     &      'Continue?',OK,3)
          if(.NOT.OK)RETURN
          ier=0
        endif

C Zone-by-zone, split this string into separate names or numbers
C reading more lines if necessary.
        K=0
        isva=ncomp
        inisz=MCOM
        call egetagwsa(loutstr,k,IUC,isva,namen,inisz,'W',
     &    'node name or node index',ierv)

C For each zone take namen() and check it against the node names in
C the flow network (recognise 0 as a zone with no matched flow node).
        do 35 icomp=1,ncomp
          NODID=namen(icomp)(1:12)

C Test for valid fluid network node name.
          INOD=0
   31     INOD=INOD+1
          IF(NODID(1:12).EQ.NDNAM(INOD)(1:12)) GOTO 32
          IF(INOD.LT.NNOD) GOTO 31
          if(NODID(1:2).eq.'0 ')then
            continue  ! A zero signals there is no associated zone.
          else
            write(outs,'(3a)') 'Did not find matching node name ',NODID,
     &        ' in the cfg file zone-node list'
            call edisp(iuout,outs)
          endif
          GOTO 33
   32     ICAAS(ICOMP)=INOD  ! name is valid
          GOTO 35

C Name not valid, tranform string to a number.
   33     read(namen(icomp),*,ERR=1)ICAAS(ICOMP)
   35   CONTINUE

C Reporting. Make temporary arrays for zone and node names which
C are then written out in packed format (the first 117 chars) on
C as many pairs of lines as needed.
        IF(ITRC.GE.1.AND.ITRC.LT.4)THEN
          do 777 j=1,ncomp
            namez(j)=zname(j)
            if(ICAAS(J).ne.0)then
              namen(j)=NDNAM(ICAAS(J))
            else
              namen(j)='---'
            endif
  777     continue
          itrunc=1
          ipos=1
          do while (itrunc.ne.0)
            louts=1
            CALL EDISP(iuout,'Building zone/flow node association:')
            call aslist(ipos,NCOMP,namez,MCOM,'S',t117,louts,itrunc)
            WRITE(OUTS,'(2a)') ' Zone >',t117(1:louts)
            CALL EDISP(iuout,OUTS)
            call aslist(ipos,NCOMP,namen,MCOM,'S',t117,louts,itrunc)
            WRITE(OUTS,'(2a)')' Node >',t117(1:louts)
            CALL EDISP(iuout,OUTS)
            ipos=itrunc+1
          end do
        ENDIF
      ENDIF

C If a flow network and contaminant file have been defined then call
C subroutine to fill contaminant common block data.
      if(IAIRN.GT.0.AND.NOCNTM.EQ.1)then
        CALL CTREAD(iier)
        if(iier.ne.0)then
          outs='Problem scanning contaminants file!'
          call edisp(iuout,' ')
          call edisp(iuout,outs)
        endif
      endif
      if(ier.ne.0)then
        call edisp(iuout,' ')
        call edisp(iuout,
     &      'System configuration file scanned - but has problems!')
      else
        IF(ITRC.GE.1)CALL edisp(iuout,
     &      'System configuration file scanned successfully.')
      endif

C Read UK National Calculation Method (NCM) data.
      IF(IRNCMF.EQ.1)CALL RSBEM
      IF(IRNCMD.EQ.1)CALL SBEMPR(IER)

C Building description now complete, read in plant network file if it
C exists. IFPNF is the file unit number and iunit1 the plant
C components db; pltcfg assumes that ifpnf exists and is open.
C There may be a * Ground line for either a plant only or building
C + plant model.
 888  continue
      IF(INDCFG.EQ.2.OR.INDCFG.EQ.3)THEN
        CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'building name',IER)
        IF(IER.NE.0)goto 1
        if(LOUTSTR(1:8).eq.'* Ground'.or.LOUTSTR(1:8).eq.'* GROUND'.or.
     &     LOUTSTR(1:8).eq.'*ground ')then
3440      CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'ground info',IER)
          K=0
          CALL EGETW(LOUTSTR,K,WORD,'W','ground tags',IFLAG)

C Ground monthly temperature profiles (2nd item is the number of profiles).
          IF(WORD(1:4).EQ.'*mgp')THEN
            CALL EGETWI(LOUTSTR,K,NGRDP,1,MGRDP,'F','grn prfls',IER)
            IF(NGRDP.eq.0) goto 3440
            DO 280 IGRDP=1,NGRDP
              CALL EGETWRA(IUC,GVA,12,-15.,99.,'W','Gr tmp',IER)
              DO 290 J=1,12
                UGRDTP(J,IGRDP)=GVA(J)
  290         CONTINUE

C Reporting.
              IF(ITRC.GE.1.AND.ITRC.LT.4)THEN
                CALL EDISP(iuout,' ')
                CALL EDISP(iuout,'Ground temperature profile Jan-Dec:')
                WRITE(OUTS,'(12F7.2)')(UGRDTP(J,IGRDP),J=1,12)
                CALL EDISP(iuout,OUTS)
              ENDIF
 280        CONTINUE
          ENDIF
        else
          backspace(IUC) ! no * Ground found so backspace one line.
        endif

C Identify plant file; skip if this is a '* Plant' record.
1001    CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'* plant',IER)
        IF(IER.NE.0)then
          outs='Problem reading * PLANT line.'
          call edisp(iuout,outs)
          goto 1
        endif
        LPNF=LOUTSTR(1:72)
        IF(LPNF(1:4).EQ.'*end') THEN
          goto 1001
        ENDIF
        if(LPNF(1:7).EQ.'* Plant'.OR.LPNF(1:7).EQ.'* PLANT'.or.
     &     LPNF(1:6).EQ.'*plant'  ) then
          if(icfgv.ge.5)then
            if(LPNF(1:11).eq.'*plant none')then
              goto 344     ! There is no plant this should be last line of file.
            else
              if(ND.gt.2)then ! the model title follows *plant.
                k=7
                CALL EGETRM(LOUTSTR,K,modeltitle,'W','title',IER)
                havetitile=.true.
              endif
            endif
          endif
          CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'plant network',IER)
          if(IER.NE.0)then
            call edisp(iuout,'Problem reading plant network file!')
            goto 1
          endif
          LPNF=LOUTSTR(1:72)

C Read boundary file name. If IIER returns end of file, fall through. 
          write(currentfile,'(a)') FILE(1:lnblnk(FILE))
          CALL LSTRIPC(IUC,LOUTSTR,0,ND,0,'boundary/model name',IIER)
          if (IIER.eq.2) then
            IIER=0
            BPF='  '
          else
            BPF=LOUTSTR(1:72)
          endif
          IF(BPF(1:10).EQ.'* Boundary'.OR.
     &       BPF(1:10).EQ.'* BOUNDARY') THEN
            CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'* boundary',IER)
            BPF=LOUTSTR(1:72)
            if(INDCFG.eq.2.and.BPF(1:3).eq.'../') then
              CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'model name',IIER)
              IF(IIER.eq.0)then
                if(havetitile)then
                  continue
                else
                  modeltitle=LOUTSTR(1:72)
                  havetitile=.true.
                endif
              elseif(IIER.eq.2)then
                if(havetitile)then
                  continue
                else
                  modeltitle=LOUTSTR(1:72)
                  havetitile=.true.
                  iier=0
                endif
              else
                write(modeltitle,'(2a)') 'Systems model ',cfgroot
                havetitile=.true.
              endif
            else
              IF(INDCFG.EQ.2)then
                IF(IIER.eq.0)then
                  if(havetitile)then
                    continue
                  else
                    modeltitle=LOUTSTR(1:72)
                   havetitile=.true.
                  endif
                elseif(IIER.eq.2)then
                  if(havetitile)then
                    continue
                  else
                    modeltitle=LOUTSTR(1:72)
                    havetitile=.true.
                    iier=0
                  endif
                else
                  write(modeltitle,'(2a)') 'Systems model ',cfgroot
                  havetitile=.true.
                endif
              endif
            endif
          ELSE
            IF(INDCFG.EQ.2)then  ! no * Boundary found so is modeltitle
              IF(IIER.eq.0)then
                if(havetitile)then
                  continue
                else
                  modeltitle=LOUTSTR(1:72)
                  havetitile=.true.
                endif
              elseif(IIER.eq.2)then
                if(havetitile)then
                  continue
                else
                  modeltitle=LOUTSTR(1:72)
                  havetitile=.true.
                  iier=0
                endif
              else
                write(modeltitle,'(2a)') 'Systems model ',cfgroot
                havetitile=.true.
              endif
            endif
          ENDIF
        ENDIF

 1002   IFPNF=IFIL+1
        iunit1=IUF
        write(currentfile,'(a)') LPNF(1:lnblnk(LPNF))
        CALL EFOPSEQ(ifpnf,LPNF,1,IER)
        IF(IER.LT.0)THEN
          IER=1
          CALL edisp(iuout,'Problem opening plant network file.')
          RETURN
        ENDIF
        call pltcfg(ifpnf,iunit1,iuout,itrc)
        if(ier1.ne.0)then
          CALL edisp(iuout,'Plant network scanned, but with problems!')
        else
          IF(ITRC.GE.1)CALL edisp(iuout,
     &        'Plant network file successfully scanned.')
        endif
        CALL ERPFREE(ifpnf,ISTAT)
        write(currentfile,'(a)') FILE(1:lnblnk(FILE))

C If plant only, read another line for the model name if not already done.
        if(.NOT.havetitile)then
          CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'model name',IER)
          IF(IER.NE.0)goto 1
          modeltitle=LOUTSTR(1:72)
        endif

C Free plant components database and/or fluid flow network file.
        IF(IUNIT1.NE.0) CALL ERPFREE(IUNIT1,ISTAT)
      endif

C Check for optional final tags. These might be:
C FMI
C agents
C gremlins

C Initilise commons assuming no definitions.
 344  CALL FMI_CLEARALL
      CALL AGT_INIT
      CALL GRM_INIT

C Try to read another line.
   61 CALL LSTRIPC(IUC,LOUTSTR,0,ND,0,'cfg line',iier)
      if (iier.eq.1 .or. iier.eq.2) then ! end of file
        iier=0
        goto 6
      elseif (lnblnk(loutstr).le.1) then ! end of file
        goto 6
      elseif (iier.ne.0) then ! error
        IER=iier
        goto 1
      endif

C Get tag.
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','cfg tag',IER)

C Functional Mock-up Interface (FMI).
      if (LOUTSTR(1:4).eq.'*FMI') then
        call FMI_CHK(LOUTSTR,K,IUF,iier)
        if (iier.ne.0) then
          CALL edisp(iuout,' Error getting number of FMU directives,')
          CALL edisp(iuout,' FMI will not be activated.')
        endif

C Agent.
      elseif (LOUTSTR(1:7).eq.'*agent') then
        call AGT_CHK(LOUTSTR,K,IUF,iier)
        if (iier.ne.0) then
          CALL edisp(iuout,' Error getting number of agents,')
          CALL edisp(iuout,' agents will not be activated.')
        endif
        
C Gremlins.
      elseif (LOUTSTR(1:8).eq.'*gremlin') then
        call GRM_CHK(LOUTSTR,K,IUF,iier)
        if (iier.ne.0) then
          CALL edisp(iuout,' Error getting number of gremlins,')
          CALL edisp(iuout,' gremlins will not be activated.')
        endif

C Placeholder for non plant network.
      elseif (LOUTSTR(1:11).eq.'*plant none') then
        goto 6  ! This should be the last line in the file.

C Unexpected content.
      else
        goto 1
      endif

      goto 61

C Building and plant descriptions now complete.
    6 CALL ERPFREE(IUC,ISTAT)

C Pass information about the current model to the C interface.
      lcfgroot=cfgroot
      lpath=path
      lupath=upath
      limgpth=imgpth
      ldocpth=docpth
      iincomp=ncomp
      iincon=ncon
      if(browse)then
        ibrowse=1
        call curproject(lcfgroot,lpath,lupath,limgpth,ldocpth,
     &    ibrowse,iincomp,iincon)
      else
        ibrowse=0
        call curproject(lcfgroot,lpath,lupath,limgpth,ldocpth,
     &    ibrowse,iincomp,iincon)
      endif
      RETURN

C Error handling.
  1   CALL USRMSG('Problem with configuration line:',LOUTSTR,'W')
      call edisp(iuout,'Check the system  configuration file.')
      IER=1
      GOTO 6

  2   msgl2='  '
      CALL USRMSG('Problem associating tdf item with contrl.',msgl2,'W')
      IER=1
      GOTO 6

  99  CALL USRMSG(
     &  'Problem creating cfgroot string from config file name.',
     &   cfgroot,'W')
      IER=1
      GOTO 6

C File rewind errors.
  999 CALL USRMSG('Error rewinding flow network file ',
     &  LAPROB,'W')
      IER=1
      GOTO 6

      END


C *************** scan_deprecated ***********
C Scans ancient tags no longer used (so main loop not cluttered).

      subroutine scan_deprecated(loutstr,word,deprecated)
      
      character loutstr*248,word*20  ! Passed from calling code.
      logical deprecated             ! True if unused.
      
      if(loutstr(1:7).eq.'*aimpth')then
        deprecated=.true. 
      elseif(loutstr(1:8).eq.'*hvacpth')then
        deprecated=.true. 
      elseif(loutstr(1:7).eq.'*bsmpth')then
        deprecated=.true. 
      elseif(loutstr(1:7).eq.'*annipv')then
        deprecated=.true. 
      elseif(loutstr(1:7).eq.'*B-NAME')then
        deprecated=.true. 
      elseif(loutstr(1:10).eq.'*B-ADDRESS')then
        deprecated=.true. 
      elseif(loutstr(1:7).eq.'*B-CITY')then
        deprecated=.true. 
      elseif(loutstr(1:11).eq.'*B-POSTCODE')then
        deprecated=.true. 
      elseif(loutstr(1:7).eq.'*O-NAME')then
        deprecated=.true. 
      elseif(loutstr(1:10).eq.'*O-ADDRESS')then
        deprecated=.true. 
      elseif(loutstr(1:7).eq.'*O-CITY')then
        deprecated=.true. 
      elseif(loutstr(1:11).eq.'*O-POSTCODE')then
        deprecated=.true. 
      elseif(loutstr(1:12).eq.'*O-TELEPHONE')then
        deprecated=.true. 
      elseif(loutstr(1:7).eq.'*C-NAME')then
        deprecated=.true. 
      elseif(loutstr(1:12).eq.'*C-TELEPHONE')then
        deprecated=.true. 
      elseif(loutstr(1:10).eq.'*C-ADDRESS')then
        deprecated=.true. 
      elseif(loutstr(1:7).eq.'*C-CITY')then
        deprecated=.true. 
      elseif(loutstr(1:11).eq.'*C-POSTCODE')then
        deprecated=.true. 
      elseif(loutstr(1:10).eq.'*quick_run')then
        deprecated=.true. 
      endif
      return
      end

C *************** scan_ground ***********
C Scan ground related entities in model cfg file.

      subroutine scan_ground(IUC,ITRC,IGDCVS,IGDCNC,IGDNDC,IGDTAQ,ier)
#include "building.h"
C #include "model.h"
#include "site.h"

      LOGICAL IGDCVS,IGDCNC,IGDNDC,IGDTAQ

C 3D ground.
      COMMON/GRND100/GRND3D
      LOGICAL GRND3D
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD

C External longwave radiation (used by HOT3000).
      COMMON/LongRad/iExtLgRadFlag,eGrdTp
      INTEGER :: iExtLgRadFlag
      REAL :: eGrdTp(12)
      REAL :: getVal(12)
      INTEGER jMon

C Ground topology.
      COMMON/GTFIL/GTGEOM
      character GTGEOM*72
      REAL :: gva
      DIMENSION GVA(12)      
      character loutstr*248,WORD*20,OUTS*124

C Calling code will have already noticed * Ground or *ground.
      ier=0
 344  CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'ground info',IER)
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','ground tags',IFLAG)

C External longwave radiation: if gtmp tag present in cfg file, get profile.
      if(WORD(1:5).EQ.'*gtmp')then
        CALL EGETWRA(IUC,getVal,12,-15.0,99.0,'W','Ex lg rad',IER)
        iExtLgRadFlag = 1
C Store profile in array.
        DO jMon=1,12
          eGrdTp(jMon)=getVal(jMon)
        ENDDO

      elseif(WORD(1:4).eq.'*cvs')then

C 3D ground control volumes file.
        IGDCVS=.TRUE.
        CALL EGETRM(LOUTSTR,K,LGDCVS,'W','3d ground vols',IER)

C 3D ground connections file.
      elseif(WORD(1:4).eq.'*cnc')then
        IGDCNC=.TRUE.
        CALL EGETRM(LOUTSTR,K,LGDCNC,'W','3d ground conns',IER)

C 3D ground nodes coordinates file.
      elseif(WORD(1:4).eq.'*ndc')then
        IGDNDC=.TRUE.
        CALL EGETRM(LOUTSTR,K,LGDNDC,'W','3d ground cords',IER)

C 3D ground nodes coordinates file.
      elseif(WORD(1:4).eq.'*taq')then
        IGDTAQ=.TRUE.
        CALL EGETRM(LOUTSTR,K,LGDTAQ,'W','3d ground temps',IER)

C Ground topology.
      elseif(WORD(1:4).eq.'*gtp')then
        CALL EGETRM(LOUTSTR,K,GTGEOM,'W','ground topology',IER)

C Ground monthly temperature profiles. 2nd item is the number of profiles.
      elseif(WORD(1:4).eq.'*mgp')then
        CALL EGETWI(LOUTSTR,K,NGRDP,1,MGRDP,'F','grn prfls',IER)
        IF(NGRDP.eq.0) goto 344
        DO 28 IGRDP=1,NGRDP
          CALL EGETWRA(IUC,GVA,12,-15.,99.,'W','Gr tmp',IER)
          DO J=1,12
            UGRDTP(J,IGRDP)=GVA(J)
          ENDDO

C Reporting.
          IF(ITRC.GE.1.AND.ITRC.LT.4)THEN
            CALL EDISP(iuout,' ')
            CALL EDISP(iuout,' Ground temperature profile Jan-Dec:')
            WRITE(OUTS,'(12F7.2)')(UGRDTP(J,IGRDP),J=1,12)
            CALL EDISP(iuout,OUTS)
          ENDIF
  28    CONTINUE

C Ground monthly humidity profiles. 2nd item is the number of profiles.
      elseif(WORD(1:4).eq.'*mgh')then
        CALL EGETWI(LOUTSTR,K,NGRDPH,1,MGRDP,'F','grn prfls',IER)
        IF(NGRDPH.eq.0) goto 344
        IF(NGRDPH.ne.NGRDP)then
          WRITE(OUTS,'(A,I2,A)')
     &      'FATAL: Number of ground temperature profiles ',NGRDP,
     &      ' is not equal '
          CALL EDISP(iuout,OUTS)
          WRITE(OUTS,'(A,I2,A)')
     &      'to the number of ground humidity profiles ',NGRDPH,
     &      '! Programm stopped.'
          CALL EDISP(iuout,OUTS)
          STOP
        ENDIF
        DO 36 IGRDP=1,NGRDPH
          CALL EGETWRA(IUC,GVA,12,300.,3000.,'W','Gr hum',IER)
          DO J=1,12
            UGRDHUM(J,IGRDP)=GVA(J)
          ENDDO

C Reporting.
          IF(ITRC.GE.1.AND.ITRC.LT.4)THEN
            CALL EDISP(iuout,' ')
            CALL EDISP(iuout,' Ground humidity profile Jan-Dec:')
            WRITE(OUTS,'(12F6.0)')(UGRDHUM(J,IGRDP),J=1,12)
            CALL EDISP(iuout,OUTS)
          ENDIF
  36    CONTINUE

        goto 344
      elseif(WORD(1:4).eq.'*end'.or.WORD(1:11).eq.'*ground_end')then

C Set 3D ground flag if 3D files found and return to calling code.
        IF(IGDCVS.AND.IGDCNC.AND.IGDNDC.AND.IGDTAQ)GRND3D=.TRUE.
        return
      endif
      goto 344   ! scan another ground data line
      end

C *************** scan_database_names ***********
C Scan database file names in model cfg file.

      subroutine scan_database_names(IUC,foundmould,foundsbem,IRNCMD,
     &  ier)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "MultiYear_simulations.h"
#include "sbem.h"

      character loutstr*248,WORD*20
      character lworking*144
      logical unixok
      logical foundmould,foundsbem

C Since some file names could be long strings, LSTRIPC and LOUTSTR are used.
C The trigger tag might be '* DATAB' or '*datab' or '*mat' or '*stdmat'
      call isunix(unixok)
      backspace(IUC)
  44  CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'db names',IER)
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','db names',IFLAG)
      if(LOUTSTR(1:7).eq.'* DATAB'.or.LOUTSTR(1:6).eq.'*datab')then
        CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'db names',IER)
        K=0
        CALL EGETW(LOUTSTR,K,WORD,'W','db names',IFLAG)
      elseif(LOUTSTR(1:4).eq.'*mat'.or.LOUTSTR(1:7).eq.'*stdmat')then
        continue
      endif

C Deal with each of the possible database names until a tag does not match.
      if(WORD(1:4).eq.'*prm')then
        CALL EGETRM(LOUTSTR,K,lworking,'W','material db',IER)

C Process the file name following *prm and depending on its path
C set the LFMAT common block string via call to findwhichdbpath.
        call findwhichdbpath('mat',lworking,ier)

      elseif(WORD(1:4).eq.'*mat')then
        CALL EGETRM(LOUTSTR,K,lworking,'W','material db',IER)

C Process the file name following *mat and depending on its path
C set the LFMAT common block string via call to findwhichdbpath.
        call findwhichdbpath('mat',lworking,ier)
      elseif(WORD(1:7).eq.'*stdmat')then
        CALL EGETRM(LOUTSTR,K,LFMAT,'W','material db',IER)
        ipathmat=2  ! standard folder for material database

      elseif(WORD(1:4).eq.'*mlc')then
        CALL EGETRM(LOUTSTR,K,lworking,'W','multilayer db',IER)

C Process the file name following *mlc and depending on its path
C set the LFMUL common block string via call to findwhichdbpath.
        call findwhichdbpath('mul',lworking,ier)
      elseif(WORD(1:7).eq.'*stdmlc')then
        CALL EGETRM(LOUTSTR,K,LFMUL,'W','multilayer db',IER)
        ipathmul=2  ! standard folder for MLC database

      elseif(WORD(1:4).eq.'*opt')then
        CALL EGETRM(LOUTSTR,K,lworking,'W','optical db',IER)

C Process the file name following *opt and depending on its path
C set the LOPTDB common block string via call to findwhichdbpath.
        call findwhichdbpath('opt',lworking,ier)
      elseif(WORD(1:7).eq.'*stdopt')then
        ipathoptdb=2  ! standard folder for optical database
        CALL EGETRM(LOUTSTR,K,LOPTDB,'W','optical db',IER)

      elseif(WORD(1:4).eq.'*prs')then
        CALL EGETRM(LOUTSTR,K,lworking,'W','pressure db',IER)

C Process the file name following *prs and depending on its path
C set the lapres common block string via call to findwhichdbpath.
        call findwhichdbpath('prs',lworking,ier)
      elseif(WORD(1:7).eq.'*stdprs')then
        ipathapres=2  ! standard folder for pressure coef database
        CALL EGETRM(LOUTSTR,K,LAPRES,'W','pressure db',IER)

      elseif(WORD(1:6).eq.'*cfcdb')then
        CALL EGETRM(LOUTSTR,K,lworking,'W','CFClayers db',IER)

C Process the file name following *cfcdb and depending on its path
C set the LCFCDB common block string via call to findwhichdbpath.
        call findwhichdbpath('cfc',lworking,ier)
      elseif(WORD(1:9).eq.'*stdcfcdb')then
        CALL EGETRM(LOUTSTR,K,LCFCDB,'W','CFClayers db',IER)
        ipathcfc=2  ! standard folder for material database

      elseif(WORD(1:4).eq.'*evn')then
        CALL EGETRM(LOUTSTR,K,lworking,'W','profiles db',IER)

C Process the file name following *evn and depending on its path
C set the LPRFDB common block string via call to findwhichdbpath.
        call findwhichdbpath('evn',lworking,ier)
      elseif(WORD(1:7).eq.'*stdevn')then
        CALL EGETRM(LOUTSTR,K,LPRFDB,'W','profile db',IER)
        ipathprodb=2  ! standard folder for profiles database

      elseif(WORD(1:6).eq.'*mould')then
        CALL EGETRM(LOUTSTR,K,lworking,'W','mould isopleths',IER)

C Process the file name following *mld and depending on its path
C set the lfmould common block string via call to findwhichdbpath.
        call findwhichdbpath('mld',lworking,ier)
        foundmould=.true.
      elseif(WORD(1:9).eq.'*stdmould')then
        ipathmould=2  ! standard folder for mould isopleths
        CALL EGETRM(LOUTSTR,K,lfmould,'W','mould isopleths',IER)
        foundmould=.true.
      elseif(WORD(1:4).eq.'*clm')then
        CALL EGETRM(LOUTSTR,K,lworking,'W','weather file',IER)

C Read name of standard weather file and set a flag indicating
C single-year weather.
        call findwhichdbpath('clm',lworking,ier)
        bSY_climate_defined = .true.        
      elseif(WORD(1:7).eq.'*stdclm')then
        ipathclim=2  ! standard folder for weather
        CALL EGETRM(LOUTSTR,K,lclim,'W','weather file',IER)
        bSY_climate_defined = .true.
        
      elseif(WORD(1:6).eq.'*myclm') then

C Read multi-year weather file and set flag indicating muli-year
C weather.     
        CALL EGETRM(LOUTSTR,K,cTemp,
     &    'W','Multi-year climate db',IER)
        cMY_climate_db_name = cTemp(1:lnblnk(cTemp))
        bMY_climates_defined = .true.

C Special materials and miscellaneous component database.
      elseif(WORD(1:7).eq.'*mscldb')then
        CALL EGETRM(LOUTSTR,K,lworking,'W','misc comp db',IER)

C Process the file name following *mscldb and depending on its path
C set the MCMPDBFL common block string.
        call findwhichdbpath('msc',lworking,ier)

C Standard folder for miscellaneous component database.
      elseif(WORD(1:10).eq.'*stdmscldb')then
        CALL EGETRM(LOUTSTR,K,MCMPDBFL,'W','misc comp db',IER)
        ipathmsc=2  

C Process the file name following *pdb and depending on its path
C set the LPCDB common block string.
      elseif(WORD(1:4).eq.'*pdb')then
        CALL EGETRM(LOUTSTR,K,lworking,'W','plant comp db',IER)
        call findwhichdbpath('pdb',lworking,ier)

      elseif(WORD(1:7).eq.'*stdpdb')then
        ipathpcdb=2  ! standard folder for plant template database
        CALL EGETRM(LOUTSTR,K,LPCDB,'W','plant template db',IER)
      elseif(WORD(1:7).eq.'*predef')then
        CALL EGETRM(LOUTSTR,K,lworking,'W','predefined obj db',IER)

C Process the file name following *predef and depending on its path
C set the LPREDEF common block string.
        call findwhichdbpath('pre',lworking,ier)
        if(icfgv.lt.5)then
          return        ! predefined db is last in list in older cfg files.
        endif

      elseif(WORD(1:10).eq.'*stdpredef')then
        ipathpredef=2  ! standard folder for predefined objects database
        CALL EGETRM(LOUTSTR,K,LPREDEF,'W','predefined db',IER)
        if(icfgv.lt.5)then
          return        ! predefined db is last in list in older cfg files.
        endif
      elseif(WORD(1:5).eq.'*sbem')then

C Process the file name following *sbem and depending on its path
C set the LSBEM common block string.
        CALL EGETRM(LOUTSTR,K,lworking,'W','SBEM db',IER)
        call findwhichdbpath('sbm',lworking,ier)
        ISBEM=1
        IRNCMD=1
        foundsbem=.true.

      elseif(WORD(1:8).eq.'*stdsbem')then

C SBEM database file in standard location.
        ipathsbem=2
        CALL EGETRM(LOUTSTR,K,LSBEM,'W','standard SBEM db',IER)
        ISBEM=1
        IRNCMD=1
        foundsbem=.true.

      elseif(WORD(1:13).eq.'*database_end')then
        return
C      elseif(WORD(1:4).eq.'*ctl')then           ! past db section backspace and return.
C         backspace(IUC)
C         return
C      elseif(WORD(1:12).eq.'*slr_half_hr')then  ! past db section backspace and return.
C         backspace(IUC)
C         return
C      elseif(WORD(1:7).eq.'*sitealt')then       ! past db section backspace and return.
C         backspace(IUC)
C         return
      else                                      ! not a known tag backspace and return.
         backspace(IUC)
         return
      endif
      goto 44                                    ! scan another line.
      end

C *************** scan_day_types ***********
C Scan project day types in model cfg file.

      subroutine scan_day_types(IUC,idty,ier)
#include "building.h"

C Day types.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      character calename*32,calentag*12,calendayname*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER :: icalender,nbcaldays,nbdaytype

      integer idty
      character loutstr*248,WORD*20
      INTEGER, dimension(365) :: IRCC

C '*calename' or '*calentag' or '*daytag' detected, backspace and rescan the line.
      backspace(IUC)
 443  CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'calendar name',IER)
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','calendar tag',IFLAG)
      if(WORD(1:9).eq.'*calename')then
        CALL EGETRM(LOUTSTR,K,calename,'1W','calendar name',IER)
      elseif(WORD(1:9).eq.'*calentag'.or.WORD(1:7).eq.'*daytag')then
        idty=idty+1
        CALL EGETP(LOUTSTR,K,calentag(idty),'W','cal tag',IER)
        CALL EGETP(LOUTSTR,K,calendayname(idty),'W',
     &    'cal day name',IER)
        CALL EGETWI(LOUTSTR,K,nbcaldays(idty),0,365,'W',
     &    'days assoc with day type',IER)
        goto 443
      elseif(WORD(1:5).eq.'*list'.or.WORD(1:10).eq.'*calendar '.or.
     &       WORD(1:10).eq.'*day_types')then
        if(WORD(1:5).eq.'*list') K=6
        if(WORD(1:10).eq.'*calendar ') K=10
        if(WORD(1:10).eq.'*day_types') K=10

C For each day of the year, read from the 365 indicies into the
C icalender array.
        CALL EGETWI(LOUTSTR,K,NBDAYTYPE,3,MDTY,'W','day types',IER)
        CALL EGETWIA(IUC,IRCC,365,0,MDTY,'W','calendar day types',IER)
        DO 118 KV=1,365
          icalender(KV)=IRCC(KV)
  118   CONTINUE
        goto 443                 ! Read next token.
      elseif(WORD(1:9).eq.'*end_list'.or.
     &       WORD(1:13).eq.'*end_calendar'.or.
     &       WORD(1:13).eq.'*calendar_end'.or.
     &       WORD(1:14).eq.'*day_types_end')then
        if(calename(1:7).eq.'UNKNOWN') calename='Typical'
        return
      else
        backspace(IUC)  ! Not one of the known tags backspace and return.
        if(calename(1:7).eq.'UNKNOWN') calename='Typical'
        return
      endif

      end

C *************** scan_sps ***********
C Scan simulation parameter sets in model cfg file.
C << update logic to take better tokens.
C << update logic to work with V4 and V5 versions.

      subroutine scan_sps(IUC,ier)
#include "building.h"
#include "MultiYear_simulations.h"

C Simulation parameter presets.
      COMMON/C6/INDCFG
      INTEGER :: INDCFG
      common/spfldes/spfdescr(MSPS)
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      INTEGER :: isstday,isstmon,isfnday,isfnmon

      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      INTEGER :: nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),scfdres(MSPS),sipvres
      character sblres*72,sflres*72,splres*72,smstres*72,
     &  selres*72,scfdres*72,sipvres*72
      character :: spfdescr*30

C Extended simulation parameters for each set.
      common/spfldats/isstupex(MSPS),isbnstepex(MSPS),ispnstepex(MSPS),
     &  issaveex(MSPS),isavghex(MSPS),iscfdactivate(MSPS),
     &  isicfdys(MSPS),isicfdyf(MSPS),
     &  scftims(MSPS),scftimf(MSPS)
      INTEGER :: isstupex,isbnstepex,ispnstepex,issaveex,isavghex
      INTEGER :: iscfdactivate      ! zero ignore domains
      INTEGER :: isicfdys,isicfdyf  ! CFD simulation start & finish days
      REAL :: scftims,scftimf       ! CFD simulation start & finish time
      CHARACTER WORD*20,loutstr*248
      logical bMY_sim_sps

      backspace(IUC)
      CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'sps tokens',IER)
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','tokens',IFLAG)
      if(WORD(1:4).eq.'*sps' .or. WORD(1:6).eq. '*mysps')then
      
C Check for initial assumptions about simulation setup data and save level.
        CALL EGETWI(LOUTSTR,K,nsset,1,MSPS,'W','no. of sets',IER)
        CALL EGETWI(LOUTSTR,K,isstup,0,300,'W','startup',IER)    
        CALL EGETWI(LOUTSTR,K,isbnstep,1,60,'W','zone_ts',IER)
        CALL EGETWI(LOUTSTR,K,ispnstep,0,100,'W','plant_ts',IER)
        CALL EGETWI(LOUTSTR,K,issave,0,6,'W','save_lv',IER)

C If 6 items on line also read whether to save only 1stph.
        if(ND.eq.7)then
          CALL EGETWI(LOUTSTR,K,isavgh,0,1,'W','save_hourly',IER)
        else
          isavgh=0
        endif
        bMY_sim_sps = .false. 
        do 333 iss=1,nsset

C The following code ensures thea the sXXres varaibles are initialzied
C to empty strings even if they are not defind in the input file.
          write(sblres(iss), '(A)')  '  '
          write(sflres(iss), '(A)')  '  '
          write(smstres(iss), '(A)') '  '
          write(splres(iss), '(A)')  '  '
          write(selres(iss), '(A)')  '  '
          write(scfdres(iss), '(A)')  '  '
          
C Count number of items to see if legacy set or extended set.
          CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'sim set',IER)
          K=0

          if (LOUTSTR(1:14) .eq. '*start_my_sets' ) then
            bMY_sim_sps = .true.
            CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'sim set',IER)
          elseif ( LOUTSTR(1:12) .eq. '*end_my_sets') then
            bMY_sim_sps = .false.
            CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'sim set',IER)
          elseif ( LOUTSTR(1:4) .eq. '*set') then
            bMY_sim_sps = .false.      ! expect data to follow on same line
            K=0
            CALL EGETW(LOUTSTR,K,WORD,'W','*set',IFLAG)
          endif
          
C Set flag indicating if set describes a multi-year period
          bMY_period_support (iss) = bMY_sim_sps

C The following logic allows tester models with only 4 items on the
C line (day month day month) to function by always using the global values.
          if(ND.le.6)then
            isstupex(iss)=isstup; isbnstepex(iss)=isbnstep
            ispnstepex(iss)=ispnstep
            issaveex(iss)=issave; isavghex(iss)=isavgh
          else
            CALL EGETWI(LOUTSTR,K,isstupex(iss),0,300,'W','startup',IER)
            CALL EGETWI(LOUTSTR,K,isbnstepex(iss),1,60,'W','bld ts',IER)
            CALL EGETWI(LOUTSTR,K,ispnstepex(iss),1,100,'W','pl ts',IER)
            CALL EGETWI(LOUTSTR,K,issaveex(iss),0,5,'W','save lv',IER)
            CALL EGETWI(LOUTSTR,K,isavghex(iss),0,1,'W','save hr',IER)
          endif
          CALL EGETWI(LOUTSTR,K,isstday(iss),1,31,'W','start day',IER)
          CALL EGETWI(LOUTSTR,K,isstmon(iss),1,12,'W','start month',IER)
          if ( bMY_sim_sps )
     &      CALL EGETWI(LOUTSTR,K,iMY_Period_Start_years(iss),
     &                 1900,2100,'W','start year',IER)
          CALL EGETWI(LOUTSTR,K,isfnday(iss),1,31,'W','end day',IER)
          CALL EGETWI(LOUTSTR,K,isfnmon(iss),1,12,'W','end month',IER)          
          if ( bMY_sim_sps )
     &      CALL EGETWI(LOUTSTR,K,iMY_Period_end_years(iss),
     &                 1900,2100,'W','end year',IER)
          
          CALL EGETRM(LOUTSTR,K,spfdescr(iss),'W','set name',IER)
          iscfdactivate(iss)=-1

C Gather additional tokens about file names or *set
 334      CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'res names',IER)
          K=0
          CALL EGETW(LOUTSTR,K,WORD,'W','res tags',IFLAG)

C Might have a simulation preset without CFD information, so initialise
C iscfdactivate to -1. This can then be used as a test for such cases.
          if(WORD(1:5).eq.'*sblr')then
            CALL EGETRM(LOUTSTR,K,sblres(iss),'W','zone res',IER)
          elseif(WORD(1:5).eq.'*sflr')then
            CALL EGETRM(LOUTSTR,K,sflres(iss),'W','netw res',IER)
          elseif(WORD(1:5).eq.'*splr')then
            CALL EGETRM(LOUTSTR,K,splres(iss),'W','plnt res',IER)
          elseif(WORD(1:6).eq.'*smstr')then
            CALL EGETRM(LOUTSTR,K,smstres(iss),'W','moist res',IER)
          elseif(WORD(1:5).eq.'*selr')then
            CALL EGETRM(LOUTSTR,K,selres(iss),'W','elect res',IER)
          elseif(WORD(1:6).eq.'*scfdr')then

C Read cfd assessment attributes and then the cfd results file name.
C isicfdys(iss),isicfdyf(iss),scftims(iss),scftimf(iss). If 
C iscfdactivate(iss) is -1 then ignore CFD domain and do not
C check for valid calendar periods or bother to scan file name.
            CALL EGETWI(LOUTSTR,K,iscfdactivate(iss),-1,1,'W',
     &        'activate',IER)
            if(iscfdactivate(iss).eq.-1)then
              CALL EGETWI(LOUTSTR,K,isicfdys(iss),1,365,'-',
     &        'cfd start',IER)
              CALL EGETWI(LOUTSTR,K,isicfdyf(iss),1,365,'-',
     &        'cfd finish',IER)
              CALL EGETWR(LOUTSTR,K,scftims(iss),1.0,25.0,'-',
     &        'cfd start time',IER)
              CALL EGETWR(LOUTSTR,K,scftimf(iss),1.0,25.0,'-',
     &        'cfd finish time',IER)
              scfdres(iss)='UNKNOWN'
            else
              CALL EGETWI(LOUTSTR,K,isicfdys(iss),1,365,'W',
     &        'cfd start',IER)
              CALL EGETWI(LOUTSTR,K,isicfdyf(iss),1,365,'W',
     &        'cfd finish',IER)
              CALL EGETWR(LOUTSTR,K,scftims(iss),1.0,25.0,'W',
     &        'cfd start time',IER)
              CALL EGETWR(LOUTSTR,K,scftimf(iss),1.0,25.0,'W',
     &        'cfd finish time',IER)
              CALL EGETRM(LOUTSTR,K,scfdres(iss),'W','cfd res',IER)
            endif
          elseif(WORD(1:8).eq.'*end_set')then
            goto 333   
          elseif(WORD(1:8).eq.'*end_sps')then
            return
          elseif(WORD(1:8).eq.'*set')then
            backspace(IUC)     ! *set so rewind and process next set.
            goto 333
          else
            backspace(IUC)     ! not related tag, rewind and return.
            return
          endif
          goto 334
 333    continue
 
      elseif(WORD(1:8).eq.'*end_sps' .or.
     &       WORD(1:12).eq.'*end_my_sets' )then
        return
      endif
      end

C *************** scan_ies ***********
C Scan IES optical data in model cfg file.
C All associated lines have key words. Allowable key words:
C *ies, *nameies, *iesraw, *fixture, *ctlies *endies
C Any other entries signals a return to calling code
C (backspace when returning unless *endies).

      subroutine scan_ies(IUC,ier)
#include "building.h"
#include "model.h"

      CHARACTER WORD*20,loutstr*248

      backspace(IUC)   ! backup and re-read the line.
      CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'ies tokens',IER)
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','ies tokens',IFLAG)
      if(WORD(1:4).eq.'*ies')then
        CALL EGETWI(LOUTSTR,K,nbofies,1,10,'F','IES entries',IER)
        IF(IER.NE.0)return
        loop=0
  335   CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'IES tag data',IER)
        K=0
        CALL EGETW(LOUTSTR,K,WORD,'W','IES header tags',IFLAG)
        if(WORD(1:8).eq.'*nameies')then
          loop=loop+1    ! increment loop
          CALL EGETW(LOUTSTR,K,iesname(loop),'W','IES name',IFLAG)
          CALL EGETRM(LOUTSTR,K,iesmenu(loop),'W','IES menu',IER)
        elseif(WORD(1:7).eq.'*iesraw')then
          CALL EGETRM(LOUTSTR,K,iesfile(loop),'W','IES file',IER)
        elseif(WORD(1:8).eq.'*fixture')then
          CALL EGETWR(LOUTSTR,K,ieslen(loop),0.,0.,'-','IESlen',IER)
          CALL EGETWR(LOUTSTR,K,ieswid(loop),0.,0.,'-','IESwid',IER)
          CALL EGETWR(LOUTSTR,K,iesht(loop),0.,0.,'-','IESht',IER)
          CALL EGETW(LOUTSTR,K,iesalong(loop),'W','along axs',IFLAG)
        elseif(WORD(1:7).eq.'*ctlies')then
          CALL EGETWI(LOUTSTR,K,iespercents(loop),1,5,'F',
     &      'nb ies pc',IER)
          vp1=0.0; vp2=0.0; vp3=0.0; vp4=0.0; vp5=0.0
          if(iespercents(loop).eq.1)then
            CALL EGETWR(LOUTSTR,K,vp1,0.,1.,'-','IES step 1',IER)
          elseif(iespercents(loop).eq.2)then
            CALL EGETWR(LOUTSTR,K,vp1,0.,1.,'-','IES step 1',IER)
            CALL EGETWR(LOUTSTR,K,vp2,0.,1.,'-','IES step 2',IER)
          elseif(iespercents(loop).eq.3)then
            CALL EGETWR(LOUTSTR,K,vp1,0.,1.,'-','IES step 1',IER)
            CALL EGETWR(LOUTSTR,K,vp2,0.,1.,'-','IES step 2',IER)
            CALL EGETWR(LOUTSTR,K,vp3,0.,1.,'-','IES step 3',IER)
          elseif(iespercents(loop).eq.4)then
            CALL EGETWR(LOUTSTR,K,vp1,0.,1.,'-','IES step 1',IER)
            CALL EGETWR(LOUTSTR,K,vp2,0.,1.,'-','IES step 2',IER)
            CALL EGETWR(LOUTSTR,K,vp3,0.,1.,'-','IES step 3',IER)
            CALL EGETWR(LOUTSTR,K,vp4,0.,1.,'-','IES step 4',IER)
          elseif(iespercents(loop).eq.5)then
            CALL EGETWR(LOUTSTR,K,vp1,0.,1.,'-','IES step 1',IER)
            CALL EGETWR(LOUTSTR,K,vp2,0.,1.,'-','IES step 2',IER)
            CALL EGETWR(LOUTSTR,K,vp3,0.,1.,'-','IES step 3',IER)
            CALL EGETWR(LOUTSTR,K,vp4,0.,1.,'-','IES step 4',IER)
            CALL EGETWR(LOUTSTR,K,vp5,0.,1.,'-','IES step 5',IER)
          endif
          iessteps(loop,1)=vp1; iessteps(loop,2)=vp2
          iessteps(loop,3)=vp3; iessteps(loop,4)=vp4
          iessteps(loop,5)=vp5
        elseif(WORD(1:7).eq.'*endies')then
          return
        else
          backspace(IUC)   ! backup and re-read the line.
          return  ! Tag not in associated list.
        endif
        goto 335    ! read more ies data lines
      else
      endif

      return
      end

C *************** scan_season ***********
C Scan season entries in legacy and current model cfg files.

      subroutine scan_season(IUC,ier)
#include "building.h"
#include "model.h"
#include "seasons.h"

      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      common/clmlnum/nbclmlhelp
      character clmlnam*42,clmlaid*72,clmldbfile*144,clmlavail*10
      character clmlhelp*72
      INTEGER :: nbclmlhelp

      character WORD*20,loutstr*248
      character pstring*40 ! for season days and months

      backspace(IUC)   ! backup and re-read the line.
      CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'season tokens',IER)
      K=0
      if(LOUTSTR(1:8).eq.'*seasons')then
        ihaveseason=1   ! signal a local definition exists
      else
        return
      endif

 444  CALL LSTRIPC(IUC,LOUTSTR,0,ND,1,'seasons data',IER)
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','seasons tag',IFLAG)
      if(WORD(1:5).eq.'*aide')then
        call egetrm(loutstr,K,clmlaid,'W','brief description',IER)
        goto 444
      elseif(WORD(1:5).eq.'*name')then
        call egetrm(loutstr,K,clmlnam,'W','menu entry',IER)
        goto 444
      elseif(WORD(1:9).eq.'*winter_t')then

C Instantiate typper common block variables ia1wins,ia1winf,ia2wins,ia2winf.
        call egetrm(loutstr,K,pstring,'W','winter typ',IER)

C Start and end day of both winters.
        K=0
        CALL EGETWI(pstring,K,ID1,1,31,'F','s day',IER)
        CALL EGETWI(pstring,K,IM1,1,12,'F','s month',IER)
        CALL EGETWI(pstring,K,ID2,1,31,'F','f day',IER)
        CALL EGETWI(pstring,K,IM2,1,12,'F','f month',IER)
        CALL EDAY(ID1,IM1,ia1wins)
        CALL EDAY(ID2,IM2,ia1winf)
        CALL EGETWI(pstring,K,ID1,1,31,'F','s day',IER)
        CALL EGETWI(pstring,K,IM1,1,12,'F','s month',IER)
        CALL EGETWI(pstring,K,ID2,1,31,'F','f day',IER)
        CALL EGETWI(pstring,K,IM2,1,12,'F','f month',IER)
        CALL EDAY(ID1,IM1,ia2wins)
        CALL EDAY(ID2,IM2,ia2winf)
        goto 444
      elseif(WORD(1:9).eq.'*spring_t')then

C Instantiate typper common block variables ia1sprs,ia1sprf,ia2sprs,ia2sprf .
        call egetrm(loutstr,K,pstring,'W','spring typ',IER)

C Start and end day and typical day of spring and autumn.
        K=0
        CALL EGETWI(pstring,K,ID1,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM1,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID2,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM2,1,12,'F','month',IER)
        CALL EDAY(ID1,IM1,ia1sprs)
        CALL EDAY(ID2,IM2,ia1sprf)
        CALL EGETWI(pstring,K,ID1,1,31,'F','s day',IER)
        CALL EGETWI(pstring,K,IM1,1,12,'F','s month',IER)
        CALL EGETWI(pstring,K,ID2,1,31,'F','f day',IER)
        CALL EGETWI(pstring,K,IM2,1,12,'F','f month',IER)
        CALL EDAY(ID1,IM1,ia2sprs)
        CALL EDAY(ID2,IM2,ia2sprf)
        goto 444
      elseif(WORD(1:9).eq.'*summer_t')then

C Instantiate typper common block variables iasums & iasumf.
        call egetrm(loutstr,K,pstring,'W','summer typ',IER)
        K=0
        CALL EGETWI(pstring,K,ID1,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM1,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID2,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM2,1,12,'F','month',IER)
        CALL EDAY(ID1,IM1,iasums)
        CALL EDAY(ID2,IM2,iasumf)
        if(icfgv.lt.5)then
          goto 444      ! Older files may have more entries.
        else
          CALL LSTRIPC(IUC,LOUTSTR,0,ND,0,'season end',IER)
          if(LOUTSTR(1:11).eq.'*season_end')then
            return
          else
            backspace(IUC) ! not associated so backspace twwo lines.
            backspace(IUC)
            return
          endif
          goto 444
        endif
      elseif(WORD(1:9).eq.'*winter_s')then

C Read winter season - two periods, e.g. nov-dec & jan-feb
C into common block typsea (is1wins,is1winf,is2wins,is2winf).
        call egetrm(loutstr,K,pstring,'W','winter sea',IER)
        K=0
        CALL EGETWI(pstring,K,ID1,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM1,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID2,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM2,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID3,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM3,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID4,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM4,1,12,'F','month',IER)
        CALL EDAY(ID1,IM1,is1wins)
        CALL EDAY(ID2,IM2,is1winf)
        CALL EDAY(ID3,IM3,is2wins)
        CALL EDAY(ID4,IM4,is2winf)
        goto 444
      elseif(WORD(1:9).eq.'*spring_s')then

C Read transition season - two periods, e.g. mar-may & sep-oct
C cinto ommon block typsea (is1sprs,is1sprf,is2sprs,is2sprf).
        call egetrm(loutstr,K,pstring,'W','trans sea',IER)
        K=0
        CALL EGETWI(pstring,K,ID1,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM1,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID2,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM2,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID3,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM3,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID4,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM4,1,12,'F','month',IER)
        CALL EDAY(ID1,IM1,is1sprs)
        CALL EDAY(ID2,IM2,is1sprf)
        CALL EDAY(ID3,IM3,is2sprs)
        CALL EDAY(ID4,IM4,is2sprf)
        goto 444
      elseif(WORD(1:9).eq.'*summer_s')then

C Read summer season, e.g. jun-aug into common block typsea (is1sums,is1sumf).
        call egetrm(loutstr,K,pstring,'W','summer sea',IER)
        K=0
        CALL EGETWI(pstring,K,ID1,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM1,1,12,'F','month',IER)
        CALL EGETWI(pstring,K,ID2,1,31,'F','day',IER)
        CALL EGETWI(pstring,K,IM2,1,12,'F','month',IER)
        CALL EDAY(ID1,IM1,is1sums)
        CALL EDAY(ID2,IM2,is1sumf)
        goto 444
      elseif(WORD(1:11).eq.'*help_start')then

C Read and store comments into the HELP array for later display.
        nbclmlhelp=0
 445    CALL LSTRIPC(IUC,LOUTSTR,0,ND,0,'help line',IER)
        if(LOUTSTR(1:11).eq.'*season_end')then
          continue
        else
          goto 445
        endif
      elseif(WORD(1:11).eq.'*season_end')then
        return
      endif

      return
      end

C *************** SITEINFO ***************
C Provides a description of the site information in
C common blocks C4, C5, C5R, PREC8, which are filled by reading the
C system configuration file.

      SUBROUTINE SITEINFO(ITRU)
#include "building.h"
#include "site.h"
      
      integer lnblnk  ! function definition

      CHARACTER POS*124,OUTSTR*124,sdesc*16

      IF(siteexposureindex.EQ.1)THEN
        POS='typical city centre.'
      ELSEIF(siteexposureindex.EQ.2)THEN
        POS='typical urban.'
      ELSEIF(siteexposureindex.EQ.3)THEN
        POS='typical rural.'
      ELSEIF(siteexposureindex.EQ.4)THEN
        POS='city center, equal views to sky, ground and buildings.'
      ELSEIF(siteexposureindex.EQ.5)THEN
        POS='city center below mean height of surrounding buildings.'
      ELSEIF(siteexposureindex.EQ.6)THEN
        POS='isolated rural.'
      ELSEIF(siteexposureindex.EQ.7)THEN
        POS='within a totally enclosed building.'
      ELSEIF(siteexposureindex.EQ.8)THEN
        POS='user defined exposure as follows: '
      ENDIF

C Get lat:long description.
      call sitell2s(sitelat,sitelongdif,sdesc)
      CALL EDISP(ITRU,'  ')  ! echo blank line before site info
      WRITE(OUTSTR,'(3a)')' Site location: ',sdesc,
     &  ' of local meridian.'
      CALL EDISP(ITRU,OUTSTR)

      IF(groundreflmodel.EQ.1)THEN
        WRITE(OUTSTR,'(a,F4.2,a)')
     &    ' Ground reflectivity: constant = ',groundrefl,'.'
      ELSEIF(groundreflmodel.EQ.2)THEN
        WRITE(OUTSTR,'(a)')
     &    ' Ground reflectivity: variable (simple model).'
      ELSEIF(groundreflmodel.EQ.3)THEN
        WRITE(OUTSTR,'(a)')
     &    ' Ground reflectivity: variable (advanced model).'
      ENDIF
      CALL EDISP(ITRU,OUTSTR)

      WRITE(OUTSTR,'(2a)')' Site exposure ',POS(1:LNBLNK(POS))
      CALL EDISP(ITRU,OUTSTR)

      IF(siteexposureindex.EQ.8)THEN
        WRITE(OUTSTR,98911)skyview
98911   FORMAT(' User defined sky ratio =',F6.4)
        CALL EDISP(ITRU,OUTSTR)
        WRITE(OUTSTR,98912)groundview
98912   FORMAT('           ground ratio =',F6.4)
        CALL EDISP(ITRU,OUTSTR)
        WRITE(OUTSTR,98913)buildingview
98913   FORMAT('         building ratio =',F6.4)
        CALL EDISP(ITRU,OUTSTR)
      ENDIF

      RETURN
      END

C *************** scan_temporal_links ***********
C Scan links to temporal data in model cfg file.

      subroutine scan_temporal_links(IUC,TXST,ITRC,ier)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "tdf2.h"

      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)
      CHARACTER DBTAG*12,DBTASK*12,DBZN*15,DBSN*15

      character loutstr*248,WORD*20,OUTS*124
      character HDR*12
      character msgl2*48
      logical TXST

C Add XST as a parameter and deal with case where it does not exist.
      IF(TXST)then
C Try to open as ascii file first. If that fails try as a binary file.
        CALL EFOPSEQ(IUTDF,LTDFA,1,IER)
        if(ier.eq.0)then
          CALL LSTRIPC(IUTDF,loutstr,99,ND,1,'header',IER)
          if(LOUTSTR(1:9).eq.'ASCIITDF3')THEN
            ITDFLG=3
          elseif(LOUTSTR(1:9).eq.'ASCIITDF2')THEN
            ITDFLG=2
          elseif(LOUTSTR(1:12).eq.'TDFdatabase2')THEN
            ITDFLG=1
          else

C Might have reached this point because it was a binary file.
C Scan the first record of this file.
            CALL ERPFREE(IUTDF,ISTAT)
            NWPR=MTABC
            ITWPR=NWPR+1
            ier=0
            call EFOPRAN(iutdf,LTDFA,ITWPR,1,IER)
            IREC=1
            READ(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=103)HDR,NWPR
            if(HDR(1:12).eq.'TDFdatabase2')THEN
              ITDFLG= -2
            elseif(HDR(1:12).eq.'TDFdatabase3')THEN
              ITDFLG= -3
            else
              ITDFLG=0
            endif
            CALL ERPFREE(IUTDF,ISTAT)
            goto 104
 103        msgl2='in the temporal file'
            CALL USRMSG(' could not read header record 1',msgl2,'W')
            CALL ERPFREE(IUTDF,ISTAT)
            ITDFLG=0
          endif
        else
          CALL ERPFREE(IUTDF,ISTAT)
          NWPR=MTABC
          ITWPR=NWPR+1
          ier=0
          call EFOPRAN(iutdf,LTDFA,ITWPR,1,IER)
          IREC=1
          READ(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=102)HDR,NWPR
          if(HDR(1:12).eq.'TDFdatabase2')THEN
            ITDFLG= -2
          elseif(HDR(1:12).eq.'TDFdatabase3')THEN
            ITDFLG= -3
          else
            ITDFLG=0
          endif
          CALL ERPFREE(IUTDF,ISTAT)
          goto 104
 102      msgl2='in the temporal file'
          CALL USRMSG(' could not read header record 1',msgl2,'W')
          CALL ERPFREE(IUTDF,ISTAT)
          ITDFLG=0
        endif
      else

C Temporal file was not found, set ITDFLG to 1 & keep reading until * end tdf found.
        ITDFLG=1
      endif

C Look for beginning of instance names and loop until '* end tdf' found.
C One instance name per line is the format. 

C Reset simulation flags if itdflg not zero.
  104 if(itdflg.ne.0)then

C Read references to TDF id's and then the related tasks and zones.
        M=0
   59   CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'instance names',IER)
        IF(IER.NE.0)GOTO 58
        IF(LOUTSTR(1:9).EQ.'* end tdf')GOTO 58
        M=M+1
        DBTAG(M)=' '
        DBTASK(M)=' '
        DBZN(M)=' '
        DBSN(M)=' '
        K=0
        CALL EGETW(LOUTSTR,K,WORD,'W','tdf instance name',IER)
        DBTAG(M)=WORD(1:12)
        if(ND.gt.1)then
          CALL EGETW(LOUTSTR,K,WORD,'W','related task/type',IER)
          DBTASK(M)=WORD(1:12)
        endif
        if(ND.gt.2)then
          CALL EGETW(LOUTSTR,K,WORD,'W','related zone',IER)
          DBZN(M)=WORD(1:15)
        endif
        if(ND.gt.3)then
          CALL EGETW(LOUTSTR,K,WORD,'W','related surface',IER)
          DBSN(M)=WORD(1:15)
        endif            
        WRITE(OUTS,'(A,I2,8A)') ' Tdf item:',M,':',DBTAG(M),
     &    ' of type: ',DBTASK(M),' associated with ',DBZN(M),
     &    'and surface',DBSN(M)
        IF(ITRC.GE.1)CALL EDISP(iuout,OUTS)
        GOTO 59
   58   CONTINUE
      endif
      ITEMSTD=M
      CALL ERPFREE(IUTDF,ISTAT)

      return
      end

C **************** CONXINFO ****************
C Returns a description in CXSTR of the inter-connection 
C information in common blocks, which are filled by reading the
C system configuration file. If ICON=0 then generate a text heading.
C Note: ICF not currently used.

      SUBROUTINE CONXINFO(ICF,ICON,CXSTR)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

C Passed parameters
      integer icf     ! unused at this time
      integer icon    ! connection index
      character CXSTR*78 ! returned string

      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      CHARACTER SST1*12,ZST1*12,SST2*12,ZST2*12
      integer ICM  ! connection for current surface
      integer ictype  ! connection type for current surface
      integer lsn1   ! surface name length
      integer lzn1   ! zone name length
      integer lsn2   ! other surface name length
      integer lzn2   ! other zone name length

      if(ICON.EQ.0)then
        WRITE(CXSTR,8986)  
 8986   FORMAT('Con  Origin surface              Other side')    
        RETURN
      endif

C Trap out of range IC1 and IE1 variables.
      if(IC1(ICON).EQ.0)then
        CXSTR='  '
        RETURN
      endif
      if(IE1(ICON).EQ.0)then
        CXSTR='  '
        RETURN
      endif

      ictype = ICT(ICON)  ! assign local variable
      SST1=SNAME(IC1(ICON),IE1(ICON))
      lsn1=lnblnk(SST1)
      ZST1=zname(IC1(ICON))
      lzn1=lnblnk(ZST1)
      IF(ictype.EQ.-1)THEN
        WRITE(CXSTR,7985)ICON,SST1(1:lsn1),ZST1(1:lzn1)
 7985   FORMAT(I4,1X,a,' in ',a,' not yet defined')
      ELSEIF(ictype.EQ.0)THEN
        WRITE(CXSTR,8985)ICON,SST1(1:lsn1),ZST1(1:lzn1)
 8985   FORMAT(I4,1X,a,' in ',a,' is External')
      ELSEIF(ictype.EQ.1)THEN
        if(IC2(ICON).eq.0.and.IE2(ICON).eq.0)then
          WRITE(CXSTR,8984)ICON,SST1(1:lsn1),ZST1(1:lzn1)
 8984     FORMAT(I4,1X,a,' in ',a,' >|< Identical environment ')
        else
          WRITE(CXSTR,8987)ICON,SST1(1:lsn1),
     &      ZST1(1:lzn1),IC2(ICON),IE2(ICON)
 8987     FORMAT(I4,1X,a,' in ',a,' >|< Similar +-',i3,'dC & ',
     &      I4,' W rad')
        endif
      ELSEIF(ictype.EQ.2)THEN
        WRITE(CXSTR,8983)ICON,SST1(1:lsn1),ZST1(1:lzn1),
     &                   IC2(ICON),IE2(ICON)
 8983   FORMAT(I4,1X,a,' in ',a,' >|< Constant @',I3,' dC & ',
     &    I4,' W rad')
      ELSEIF(ictype.EQ.3)THEN
        ICM=IZSTOCN(IC2(ICON),IE2(ICON))
        SST2=SNAME(IC2(ICON),IE2(ICON))
        lsn2=lnblnk(SST2)
        ZST2=zname(IC2(ICON))
        lzn2=lnblnk(ZST2)
        WRITE(CXSTR,8982)ICON,SST1(1:lsn1),ZST1(1:lzn1),
     &                        SST2(1:lsn2),ZST2(1:lzn2)
 8982   FORMAT(I4,1X,a,' in ',a,' >|< ',a,' in ',a)
      ELSEIF(ictype.EQ.4)THEN
        IF(IC2(ICON).GT.0)THEN
          WRITE(CXSTR,8981)ICON,SST1(1:lsn1),
     &                     ZST1(1:lzn1),IC2(ICON)
 8981     FORMAT(I4,1X,a,' in ',a,' >|< ground profile',I2)
        ELSEIF(IC2(ICON).EQ.-3)THEN
          WRITE(CXSTR,8998)ICON,SST1(1:lsn1),ZST1(1:lzn1)
 8998     FORMAT(I4,1X,a,' in ',a,' >|< 3D ground model')
        ELSE
          WRITE(CXSTR,8980)ICON,SST1(1:lsn1),
     &                     ZST1(1:lzn1),IE2(ICON)
 8980     FORMAT(I4,1X,a,' in ',a,
     &         ' >|< user def ground profile',I2)
        ENDIF
      ELSEIF(ictype.EQ.5)THEN
        WRITE(CXSTR,7984)ICON,SST1(1:lsn1),ZST1(1:lzn1)
 7984   FORMAT(I4,1X,a,' in ',a,' is adiabatic')
      ELSEIF(ictype.EQ.6)THEN

C BASESIMP connection.
        WRITE(CXSTR,7986)ICON,SST1(1:lsn1),ZST1(1:lzn1)
 7986   FORMAT(I4,1X,a,' in ',a,' is BASESIMP fndtn')
      ELSEIF(ictype.EQ.7)THEN

C CEN 13791 boundary condition.
        if(IC2(ICON).eq.0.and.IE2(ICON).eq.0)then
          WRITE(CXSTR,9984)ICON,SST1(1:lsn1),ZST1(1:lzn1)
 9984     FORMAT(I4,1X,a,' in ',a,' >|< Identical CEN 13791 ')
        else
          WRITE(CXSTR,9987)ICON,SST1(1:lsn1),
     &      ZST1(1:lzn1),IC2(ICON),IE2(ICON)
 9987     FORMAT(I4,1X,a,' in ',a,' >|< CEN13791+-',i3,'dC & ',
     &      I4,' W rad')
        endif
      ENDIF
      RETURN
      END

C ******************** HZSTOCN ********************

C HZSTOCN scans the current connections list and builds an
C array IZSTOCN(MCOM,MS) which holds the connection index.

      SUBROUTINE HZSTOCN
#include "building.h"
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      do 42 icc = 1, NCON
        IZSTOCN(IC1(icc),IE1(icc))=icc
 42   continue
      return
      end

C **************** conninanchor 
C conninanchor returns true if a connection is included within
C a particular anchor list and matches a valid anchor use.
      subroutine conninanchor(icnn,ianc,type,ok,ier)
#include "building.h"
#include "model.h"

      character outs*124,type*4,msgl2*48
      logical found,ok

C Check if ianc is a known anchor and then if there is a list.
      found=.false.
      if(ianc.gt.0.and.ianc.le.NALOC)then
        if(ALOCTYP(ianc)(1:4).eq.type(1:4))then
          if(IALOC(ianc).gt.0)then
            do 45 jj = 1,IALOC(ianc)
              if(lstanchr(ianc,jj).eq.icnn)then
                found=.true.
                goto 46
              endif
  45        continue
  46        continue
            ok=found
            return
          else
            write(outs,'(6a)') 'Anchor ',ALOCLBL(ianc),' type ',
     &        ALOCTYP(ianc),' does not match requested ',type
            msgl2='and so will not be used.'
            call usrmsg(outs,msgl2,'W')
            ier=0
            return
          endif
        else
          write(outs,'(3a)') 'Anchor ',ALOCLBL(ianc),' has zero list.'
          msgl2='and so will not be used.'
          call usrmsg(outs,msgl2,'W')
          ier=1
          return
        endif
      else
        write(outs,'(a,i2,a)') 'Anchor ',ianc,' is an unknown anchor.'
        msgl2='and so will not be used.'
        call usrmsg(outs,msgl2,'W')
        ier=1
        return
      endif
      end

C ******************** findwhichdbpath ********************
C Takes a database file name and checks to
C see if the path is local or absolute or standard database folder.
C Depending on which database (the parameter topic) the
C common block file string and whichdbpath variable is updated.
C topic =  'opt' for optics, 'pdb' for plant component templates
C       =  'prs' for pressure coef, 'sbm' for UK SBEM database,
C       =  'mat' for common materials, 'evn' for event profiles
C       =  'mul' for common constructions, 'msc' for active components
C       =  'mld' for mould isopleths, 'clm' for weather
C       =  'cfc' for cfc layers

      subroutine findwhichdbpath(topic,file,ier)
      implicit none
#include "esprdbfile.h"

      integer lnblnk  ! function definition
      integer ier  ! typd for passed parameter

C Currently works with optical and plant template database.
      character*(*) file
      character topic*3 ! identify which type of database
      integer lndbp,lnclmp ! length of standard database weather paths
      integer lnwkg   ! for length of working file name
      logical unixok  ! to check for database path file separators

C Set OS and check that standarddbpath standardweatherpath have been set.
      lndbp=lnblnk(standarddbpath)
      lnclmp=lnblnk(standardclmpath)
      lnwkg=lnblnk(file)
      if(lndbp.lt.1.or.lnwkg.lt.1)then
        ier=2
        return
      endif
      call isunix(unixok)
      
      if(unixok)then
        if(file(1:7).eq.'../dbs/')then

C Local model dbs folder.
          if(topic(1:3).eq.'opt')then
            ipathoptdb=1  ! local folder for common optics
            write(LOPTDB,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=1  ! local folder for plant database
            write(LPCDB,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'pre')then
            ipathpredef=1  ! local folder for predefined objects
            write(lpredef,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'prs')then
            ipathapres=1  ! local folder for pressure distributions
            write(lapres,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'sbm')then
            ipathsbem=1  ! local folder for UK SBEM database
            write(lsbem,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mat')then
            ipathmat=1  ! local folder for common materials
            write(LFMAT,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'evn')then
            ipathprodb=1  ! local folder for event profiles
            write(LPRFDB,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mul')then
            ipathmul=1  ! local folder for common constructions
            write(LFMUL,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'msc')then
            ipathmsc=1  ! local folder for active components
            write(MCMPDBFL,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mld')then
            ipathmould=1  ! local folder for mould isopleths
            write(lfmould,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'clm')then
            ipathclim=1  ! local folder for weather
            write(LCLIM,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'cfc')then
            ipathcfc=1  ! local folder for CFC
            write(LCFCDB,'(a)') file(1:lnwkg) ! assign directly
          endif
        elseif(file(1:lndbp).eq.standarddbpath(1:lndbp))then

C The initial part of database path matches so save the file name w/o path.
          if(topic(1:3).eq.'opt')then
            ipathoptdb=2
            write(LOPTDB,'(a)') file(lndbp+2:lnwkg)  ! without path
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=2
            write(LPCDB,'(a)')  file(lndbp+2:lnwkg)  ! without path
          elseif(topic(1:3).eq.'pre')then
            ipathpredef=2
            write(lpredef,'(a)') file(lndbp+2:lnwkg)  ! without path
          elseif(topic(1:3).eq.'prs')then
            ipathapres=2
            write(lapres,'(a)') file(lndbp+2:lnwkg)  ! without path
          elseif(topic(1:3).eq.'sbm')then
            ipathsbem=2
            write(lsbem,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'mat')then
            ipathmat=2
            write(LFMAT,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'evn')then
            ipathprodb=2
            write(LPRFDB,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'mul')then
            ipathmul=2
            write(LFMUL,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'msc')then
            ipathmsc=2
            write(MCMPDBFL,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'mld')then
            ipathmould=2
            write(lfmould,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'clm')then
            ipathclim=2
            write(LCLIM,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'cfc')then
            ipathcfc=2
            write(LCFCDB,'(a)') file(lndbp+2:lnwkg) ! without path
          endif
        elseif(file(1:lnclmp).eq.standardclmpath(1:lnclmp))then

C The initial part of the weather path matches save the file name w/o path.
          if(topic(1:3).eq.'clm')then
            ipathclim=2
            write(LCLIM,'(a)') file(lnclmp+2:lnwkg) ! without path
          endif
        else
          if(topic(1:3).eq.'opt')then
            ipathoptdb=0
            write(LOPTDB,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=0
            write(LPCDB,'(a)')  file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'pre')then
            ipathpredef=0
            write(lpredef,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'prs')then
            ipathapres=0
            write(lapres,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'sbm')then
            ipathsbem=0
            write(lsbem,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mat')then
            ipathmat=0
            write(LFMAT,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'evn')then
            ipathprodb=0
            write(LPRFDB,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mul')then
            ipathmul=0
            write(LFMUL,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'msc')then
            ipathmsc=0
            write(MCMPDBFL,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mld')then
            ipathmould=0  ! local folder for mould isopleths
            write(lfmould,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'clm')then
            ipathclim=0
            write(LCLIM,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'cfc')then
            ipathcfc=0
            write(LCFCDB,'(a)') file(1:lnwkg) ! assign directly
          endif
        endif
      else
        if(file(1:7).eq.'..\\dbs\\')then
          if(topic(1:3).eq.'opt')then
            ipathoptdb=1  ! local folder for optical database
            write(LOPTDB,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=1  ! local folder for plant database
            write(LPCDB,'(a)') file(1:lnwkg)   ! assign directly
          elseif(topic(1:3).eq.'pre')then
            ipathpredef=1  ! local folder for predefined obj database
            write(lpredef,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'prs')then
            ipathapres=1  ! local folder for pressure database
            write(lapres,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'sbm')then
            ipathsbem=1
            write(lsbem,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mat')then
            ipathmat=1  ! local folder for materials database
            write(LFMAT,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'evn')then
            ipathprodb=1  ! local folder for profiles database
            write(LPRFDB,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mul')then
            ipathmul=1  ! local folder for MLC database
            write(LFMUL,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'msc')then
            ipathmsc=1  ! local folder for active components
            write(MCMPDBFL,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mld')then
            ipathmould=1  ! local folder for mould isopleths
            write(lfmould,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'clm')then
            ipathclim=1  ! local folder for weather
            write(LCLIM,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'cfc')then
            ipathcfc=1  ! local folder for CFC
            write(LCFCDB,'(a)') file(1:lnwkg) ! assign directly
          endif
        elseif(file(1:lndbp).eq.standarddbpath(1:lndbp))then
          if(topic(1:3).eq.'opt')then
            ipathoptdb=2
            write(LOPTDB,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=2
            write(LPCDB,'(a)')  file(lndbp+2:lnwkg)  ! without path
          elseif(topic(1:3).eq.'pre')then
            ipathpredef=2
            write(LPREDEF,'(a)')  file(lndbp+2:lnwkg)  ! without path
          elseif(topic(1:3).eq.'prs')then
            ipathapres=2
            write(lapres,'(a)') file(lndbp+2:lnwkg)  ! without path
          elseif(topic(1:3).eq.'sbm')then
            ipathsbem=2
            write(lsbem,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'mat')then
            ipathmat=2
            write(LFMAT,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'evn')then
            ipathprodb=2
            write(LPRFDB,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'mul')then
            ipathmul=2
            write(LFMUL,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'msc')then
            ipathmsc=2
            write(MCMPDBFL,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'mld')then
            ipathmould=2
            write(lfmould,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'clm')then
            ipathclim=2
            write(LCLIM,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'cfc')then
            ipathcfc=2
            write(LCFCDB,'(a)') file(lndbp+2:lnwkg) ! without path
          endif
        elseif(file(1:lnclmp).eq.standardclmpath(1:lnclmp))then
          if(topic(1:3).eq.'clm')then
            ipathclim=2
            write(LCLIM,'(a)') file(lnclmp+2:lnwkg) ! without path
          endif
        else
          if(topic(1:3).eq.'opt')then
            ipathoptdb=0
            write(LOPTDB,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=0
            write(LPCDB,'(a)')  file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'pre')then
            ipathpredef=0
            write(lpredef,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'prs')then
            ipathapres=0
            write(lapres,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'sbm')then
            ipathsbem=0
            write(lsbem,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mat')then
            ipathmat=0
            write(LFMAT,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'evn')then
            ipathprodb=0
            write(LPRFDB,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mul')then
            ipathmul=0
            write(LFMUL,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'msc')then
            ipathmsc=0
            write(MCMPDBFL,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mld')then
            ipathmould=0
            write(lfmould,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'clm')then
            ipathclim=0
            write(LCLIM,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'cfc')then
            ipathcfc=0
            write(LCFCDB,'(a)') file(1:lnwkg) ! assign directly
          endif
        endif
      endif

      return
      end

C Intialise plant solution convergence criteria.
      subroutine InitPltSolnParams()
      implicit none 
      include "plant.h" 
      include "UserSimulationToggles.h"
      
      
C Plant matrix solver convergence criteria.      
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      integer MAXITP, itrclp, ICSV
      real PERREL, PERTMP, PERFLX, PERMFL, CSVI
     
C Tolerance for iteration of hydrogen flow matrix.
      common/PITER_H2/fH2_iteration_tolerance
      real fH2_iteration_tolerance
      
C Initialise plant side iteration parameters.
      MAXITP=100
      PERREL=.01
      PERTMP=1.
      PERFLX=10.
      PERMFL=.0005  
      itrclp=maxitp

C Hydrogen flow iteration tolerance.
      fH2_iteration_tolerance = PERMFL * 1.0E-03 
      iUserMAXITP    = MAXITP 
      iUserITRCLP    = itrclp
      fUserPERREL    = PERREL 
      fUserPERTMP    = PERTMP 
      fUserPERFLX    = PERFLX 
      fUserPERMFL    = PERMFL       
      fUserH2iterTol = fH2_iteration_tolerance    
      
      return 
      end 
