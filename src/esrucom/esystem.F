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

C esystem.f provides the following facilities:
C  ERSYS :      Legacy interface that calls ERSYS_mmode in verbose
C               mode.
C  ERSYS_mmode: Reads a commented system configuration file, in silent
C               or verbose modes
C  SITEINFO: English description of the site information (cfg file).
C  CONXINFO: returns an English description of the inter-connections 
C            in the configuration file. 
C  HZSTOCON: Scans the current connections list and builds array
C            IZSTOCN(MCOMP,MS) holding the connection index.
C findwhichdbpath: takes a database file name and checks to see if
C            the path is local or absolute or standard database folder.

C ******************** ERSYS_mmode
C ERSYS_mmode is a legacy interface to ERSYS_mmode, below. It allows
C existing code to call ERSYS_mmode in its traditional verbose mode.

      SUBROUTINE ERSYS(FILE,IUC,IUF,MODE,ITRC,IER)
      IMPLICIT NONE
      integer  iUC, iUF, iTRC, iER
      CHARACTER*72 FILE
      CHARACTER*4 MODE
      logical bSilent

C Call multi-mode interface in traditional verboase mode.

      bSilent = .false.

      call ERSYS_mmode(FILE,IUC,IUF,MODE,ITRC,IER,bSilent)

      return
      end


C ******************** ERSYS_mmode 
C ERSYS_mmode reads a system configuration file which defines the building
C and/or plant definition for simulation with or without comments
C and with several levels of verbosity in reporting.  
C If MODE='NONE' ERSYS fills the common block with file names only. 
C IF MODE='ALL ' then all geom files scanned. Depending on the value
C of bVerbose, it may provide interactive prompts to the screen.

C Common block variables are:
C INDCFG  - is the configuration file index where: 0= registration, 
C           1= building only, 2= plant only, 3= building and plant.

C ICFGV   - version of the model configuration file. Version 4 was
C           introduced in May 2008 to explicitly hold data e.g. IPV
C           data previously held in separate files.

C Data from site.h:
C siteexposureindex   - Site exposure index :
C           1= city centre, normal case; 2= urban site, normal case
C           3= rural site, normal case;
C           4= city centre, equal sky, ground & building view factors
C           5= city centre, building below mean height of surrounding
C              buildings
C           6= rural site, isolated; 7= totally enclosed building
C           8= user defined

C groundrefl    - External ground reflectivity.
C groundreflmonth  - (12) Monthly values of ground reflectivity
C groundreflmodel  - Type of ground reflectivity model
C           1= constant albedo
C           2= simple model (monthly albedo, number of days with snow on ground
C           3= advanced model (monthly albedo, snow depth read from file)
C snowgroundrefl - Snow reflectivity
C dayswithsnow - (12) Monthly values of number of days with snow on ground
C SNFNAM  - *72 Name of the file containing hourly snow depth information

C modeltitle - is the name of the entire system.

C NCOMP   - number of zones comprising the model to be simulated.
C NCCODE  - zone code (for eventual database reference).

C LPROJ   - zone scheduling (operation file).
C LGEOM   - zone geometry file.
C LTHRM   - zone construction file.

C NCON    - total number of intra-zone connections.
C IC1 &   - zone and surface numbers defining the start
C IE1       point of each connection.

C ICT     - connection type where:
C          -1 ; unknown at this time.
C           0 ; connection to external conditions
C           1 ; connection to identical environmental conditions
C               as the source zone (OR, relative temp and absolute 
C               radiation can be specified, see below)
C           2 ; connection to constant and known environmental conditions
C           3 ; connection to another zone (as defined by IC2 & IE2 
C           4 ; connected to ground
c           5 ; surface is adiabatic
c           7 ; surface partition according to the CEN 13791
C IC2 &   - connection description where IC2 & IE2:
C IE2       - are unused if corresponding ICT=0
C           - contain relative temp and absolute incident radiation 
C             intensity values respectively if corresponding ICT=1
C             (If IC2=IE2=0, then identical to source zone)
C           - contain constant temperature and incident radiation 
C             intensity values respecitvely if corresponding ICT=2
C           - contain the zone and surface numbers at which each
C             connection terminates (if corresponding ICT=3)
C           - contain pointers to ground temps (if corresponding ICT=4)
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

C BDMDS,dbdsdesc - name file project demands file and related description.

      SUBROUTINE ERSYS_mmode(FILE,IUC,IUF,MODE,ITRC,IER,bSilent)
#include "building.h"
#include "model.h"
#include "site.h"

C geometry.h provides commons G0/G2/prec2/precz/c20.
#include "geometry.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure databases)
C LOPTDB,IOPTDB (for optical database)
C LPCDB/IPCDB (for plant template database)
C LPRFDB,IPRODB (for event profile database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
C ICLIM,LCLIM (for climate file)
C LSBEM (for SBEM database)
C MCMPDBFL (Misc components)
C default file names for databases

#include "net_flow.h"
#include "tdf2.h"
#include "cfd.h"
#include "uncertainty.h"
#include "gnetwk.h"
#include "dhw_common.h"
#include "MultiYear_simulations.h"
#include "bc_data.h"
#include "espriou.h"
C espriou.h provides currentfile and defdbfl.
#include "lookup_data.h"
#include "sbem.h"
#include "roam.h"
#include "CFC_common.h"
#include "ipvdata.h"
#include "seasons.h"
      PARAMETER (MSTMC=20)
      
      integer lnblnk  ! function definition

C Parameters
      character FILE*72  ! model configuration file name
      integer IUC        ! channel to read configuration file
      integer IUF        ! channel for other required model files
      character MODE*4   ! if ALL then causes additional data structure to be read
      integer ITRC       ! level of reporting during the file scan
      integer IER        ! return zero if ok, 2 if cfg file could not be opened
      logical bSilent    ! Flag indicating verbosity.

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/POPHELP/H(60)

C IFIL base file unit number (set in MAIN of each application).
      COMMON/FILEP/IFIL
      COMMON/ER1/IER1
      COMMON/OUTIN/IUOUT,IUIN

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

C skyview viewfactor from external surfaces to the sky (site.h)
C groundview viewfactor from external surfaces to the ground
C buildingview viewfactor from external surfaces to other buildings & obstructions
      COMMON/C6/INDCFG

C Seasons.
      character clmlnam*32,clmlaid*72,clmldbfile*144,clmlavail*10
      character clmlhelp*72
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      common/clmlnum/nbclmlhelp

C NZSUR is the number of surfaces in each zone (from geometry.h).
C NZTV is the number of vertices in each zone (from geometry.h).
C VCOORD, NZNVER & NZJVN (from geometry.h)

C For use with MODE=ALL to setup ZNDATA arrays.
C X,Y & Z   - coordinates of the vertices defining the zone
C NSUR      - Number of faces comprising zone
C JVN       - contains an ordered description (anticlockwise) of the
C             vertices in each face
C NTV       - total number of vertices in zone
C NVER      - contains the number of vertices in each face
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

C IFCFG is the file unit for the model configuration file.
C cfgroot (24 char) root name for the model (used when generating
C   system files.
C LCFGF (72 char) model configuration file name.
      common/C21/IFCFG,cfgroot,LCFGF

C IFPNF is the unit for the plant network file.
C LPNF (72 char) plant network file name.
      COMMON/C23/IFPNF,LPNF

C IZSTOCN - for each zone:surface a pointer to connection index.
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/CFGV/icfgv
      COMMON/PREC8/SLAT,SLON

      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKFLNAM,NWKTYPSTR(MNWKTYP)

C Defaults for some of the obscure files.
      character*72 DFCFD,DECMPDBFL,DICONDBFL
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DICONDBFL

C Temporal definition commons.
C itdflg=0 none, 1 older file, 2 asci v2, 3 asci v3, -2 bin v2, -3 bin v3
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFT/LTDF,LTDFA
      COMMON/TDFFH/NWPR,NITDF,NTSPH,NEXTCL,NEXTRC,LASTHD,
     &             LSTREC,NDBSTP,NUWPR

C ITEMSTD: Number of temporal items associated/used in this model.
      COMMON/TDFFLG/ITEMSTD

C DBTAG (12 char) tag of temporal entity associated with model.
C DBTASK (12 char) the type of temporal entity (which determines what
C   parts of the model it can be associated with
C DBZN (15 char) name of the zone or control etc it is associated with.
C DBSN (15 char) name of the surface within the associated zone.
       COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)

C Configuration control.
C ctldoc (248 char) - Overall control notes. If equal to 'none'
C                   then no control is imposed.
C lctlf (72 char) - model control file name.
      common/cctlnm/ctldoc,lctlf

C LPRJLG (72 char) model log file.
      COMMON/LOG/LPRJLG

C ZONEPTH (24 char) relative path to the model zones folder.
C NETPTH (24 char) relative path to the model networks folder
C CTLPTH (24 char) relative path to the model control folder
C IMGPTH (24 char) relative path to the model images folder
C RADPTH (24 char) relative path to the model Radiance folder
C DOCPTH (24 char) relative path to the model documents folder
C TMPPTH (24 char) relative path to scratch folder
C dbspth (24 char) relative path to model specific databases folder
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

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
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton

C Initial view.
      common/initv/initvt,EYEMI(3),VIEWMI(3),ANGI

C Special materials & electrical network files.
      common/spmfxst/ispmxist,spflnam
      common/entfile/entflnam, ientxist

C Non-linear thermophysical properties.
      COMMON/VTHP18/LNLTHP
      COMMON/VTHP31/INTHPS,INTHPZ(MCOM)

C 1D node distribution.
      COMMON/GR1D05/LGRD1D
      COMMON/GR1D06/IGR1D

C 3D ground.
      COMMON/GRND100/GRND3D
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD

C 3D zones.
      common/GR3D100/BLDG3D,ZONE3D(MCOM)
      common/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)

C Moisture.
      common/MOIST01/MSTROK,MSTRZN(MCOM)
      common/MOIST02/LMOIST(MCOM)

C Structured mesh.
      COMMON/GRSD100/IndxSt
      COMMON/GRSD101/LGrdSt

C Ground topology.
      COMMON/GTFIL/GTGEOM

C Primary energy conversions and emissions.
      common/PCONV/ipconv,pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
      common/CONVEM/phtco2,phtnox,phtsox,pclco2,pclnox,pclsox,
     &              pltco2,pltnox,pltsox,pfnco2,pfnnox,pfnsox,
     &              pspco2,pspnox,pspsox,phwco2,phwnox,phwsox

C CPCALC description
C ICPCON set to one if there is a CPCALC definition.
      common/CPCALC/icpcon,ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,
     &              sbh,pad,wvpe

C Radiance scenes file.
      common/radcfg/LRADCF

C CFD
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF

C Variables for weekdays, and weekends.
C Assume: Mon=1, Tue=2, Wed=3, Thu=4, Fri=5, Sat=6, Sun=7
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2

C Uncertainty analysis common.
      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)

C IPV data is from ipvdata.h.
      common/IPVF/lipvdatf

C Dispersed demands (lifts/fans/DHW).
      COMMON/BL1/dmdsdesc,bdmds

C Anchor data.
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      common/anchorb/IALOC(20),lstanchr(20,60)

C Contaminant flow model commons
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM/CNTMFIL,CNTMDESC,NTSTEPC

C HVAC
C Flag indicating presence of HVAC models:
C ihvacflag=1 indicates HVAC models are active; 0 indicates no HVAC models.
      common/hvacinfo/ihvacflag,hvacfile

C Simulation parameter presets.
      common/spfldes/spfdescr(MSPS)
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres

C BASESIMP.
      common/bsmtdef/iBSIMP(MCOM),LBSIMP(MCOM)

C AIM-2.
C IAIM2 set to 1 if AIM-2 infiltration model active.
C LAIM2 (72 char) file holding AIM-2 infiltration model data.
      common/aim2def/iAIM2,LAIM2

C Fuel cell.
      common/Fcell/iFClds,LFClds

C Mechanical Ventilation systems
      Integer iMech
      Character LMech*72
      common/mechvdef/iMech,LMech

C External Longwave Radiation 
      common/LongRad/iExtLgRadFlag,eGrdTp
      REAL      eGrdTp(12)
      REAL      getVal(12)
      INTEGER   iExtLgRadFlag
      INTEGER   jMon

C GSHP
C Flag indicating presence of GSHP models:
C igshp=1 indicates GSHP models are active; 0 indicates no GSHP models.
      common/gshpinfo/igshp
      common/gshpfile/gshpfile


C Wind-generated electricity
      common/H2wind/LWndSupp,iWndSupp

C GCEP
C Flag indicating presence of GCEP models:
C igcepflag=1 indicates GCEP models are active; 0 indicates no GCEP models.
      common/gcepinfo/igcep
      common/gcepfile/gcepfile

C Bidirectional raw optical data file.
      COMMON/BIDIR/IFLAGBI,INTVALBI,NSTMCFL(MCON)
      COMMON/BIDIRFL/bidirfile,bidirname(MSTMC)

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)

C DLLSUBR (12 char) name of current subroutine, used in dll silent reporting
C dllmesg (124 char) tells the details of the DLLSUBR error.
      common/dllerr/dllsubr,dllmesg

C CPCALC pressure coef.
      common/cpcaco/nuco,naco(MPCP),xcoab(MPCP),ycoab(MPCP),
     &              zcoab(MPCP),azim(MPCP),TYPE(MPCP),elev(MPCP)

C Path to model and command line file (if any). Browse
C is a logical flag, .true. restricts update/save options.
      common/rpath/path
      common/uhome/upath
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

C H2-dep_controller: start
C Residential hydrogen cogen: control file
      common/resh2ctl/iresh2ctl,resh2flnam
      integer iresh2ctl         !- flag indicating that file required by residential hydrogen
                                !- cogen controller is specified
      character*72 resh2flnam   !- name of file containing residential hydrogen cogen
                                !- controller inputs
C H2-dep_controller: end

C Flag for multi-year simulation definition
      logical bMY_sim_sps

C Treatment of solar radiation data in weather file
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
      INTEGER IRNCMD,IRNCMF
      INTEGER ICREPORT,IDOL,IDAYNUM,IMTHNUM,IDWKNUM,IDTYY

      integer iexcludediff_flag
      common/excludediff/iexcludediff_flag

C Simulation parameters
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      LOGICAL spfileok,perok,tstepok,saveok,autook,exitok,startupok

      LOGICAL BLDG3D,ZONE3D,MSTROK,MSTRZN,dll,browse,context
      LOGICAL INTHPS,INTHPZ,IGR1D,GRND3D
      LOGICAL XST,OK,DOK,IGDCVS,IGDCNC,IGDNDC,IGDTAQ,cnnok
      LOGICAL OK3DCVS,OK3DCNC,OK3DNDC,OK3DTAQ
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDNDD,LGDTAQ,LGRD1D,LNLTHP
      CHARACTER loutstr*248,OUTSTR*124,OUTS*124,text*124,t117*117,h*72
      CHARACTER LCFD*72
      CHARACTER*72 LAPROB,LPNF,LTDF,LTDFA
      character LPRJLG*72
      character lworking*144
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,LMOIST,LGrdSt
      CHARACTER WORD*20,NODID*12,CXSTR*78,ZN*12
      CHARACTER DBTAG*12,DBTASK*12,DBZN*15,phrase*64,DBSN*15
      character WORD2*20,msgl2*48
      character lradcf*72
      CHARACTER CTLDOC*248,LCTLF*72
      CHARACTER spflnam*72,entflnam*72,lipvdatf*72
      character GTGEOM*72
      CHARACTER ectime*24,wkd1*10,wkd2*10,naco*40,type*4
      character*10 wkday(7)
      character LUALF*72,LCNG*15,LLOC*15,cfgroot*24,LCFGF*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      CHARACTER dmdsdesc*248,bdmds*72
      CHARACTER NDNAM*12,ALOC*12,ALOCLBL*12,ALOCTYP*4
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,spfdescr*8
      character hvacfile*72,LAIM2*72,LBSIMP*72,LFClds*72
      character gshpfile*72
      character gcepfile*72
      character dllsubr*12,dllmesg*124
      character limgpth*24,ldocpth*24,ltmppth*24,lcfgroot*24,lpath*72
      character path*72,upath*72,lupath*72
      character namez*12,namen*12
      CHARACTER*72 NWKNAM,NWKDSC,NWKFLNAM
      character NWKTYPSTR*12,bidirfile*72,bidirname*12
      CHARACTER CONTMNAM*12,CNTMDESC*124,cntmfil*72
      character calename*32,calentag*12,calendayname*32
      character LWndSupp*72
      character HDR*12
      character ipvact*3   ! to singnal internal or external IPV file
      character pstring*40 ! for season days and months
      integer INDU    ! to detect old zone utility files
      logical newgeo  ! to use for testing if new/old geometry file.
      integer lndbp   ! for length of standard database path
      integer lnwkg   ! for length of working file name
      logical unixok  ! to check for database path file separators

      DIMENSION GVA(12),IVA(60),namez(mcom),namen(mcom)
      dimension IRVC(mcon),IRCC(365)
      data wkday/'Monday', 'Tuesday','Wednesday', 'Thursday',
     &           'Friday','Saturday', 'Sunday'/

      newgeo=.false.  ! assume older format geometry.

C Check if running in dll mode.
      call isadll(dll)

C Initalizing starting point for parsing simulation presets (*sps)
      iSet_start = 1

C Initialize flag for boundary condition data 
      bBC_data_defined = .false.

C Scan the defaults file and then set file common blocks.
C Make temporary use of file unit IUF.
C Note: escdef must come after scan of .esprc file.
      call escdef(IUF,IER)
      write(LCLIM,'(a)') DCLIM(1:lnblnk(DCLIM))
      write(LAPRES,'(a)') DAPRES(1:lnblnk(DAPRES))
      write(LFMAT,'(a)') DFCON(1:lnblnk(DFCON))
      write(LFMUL,'(a)') DFMUL(1:lnblnk(DFMUL))
      write(LOPTDB,'(a)') DOPTDB(1:lnblnk(DOPTDB))
      write(LPRFDB,'(a)') DPRFDB(1:lnblnk(DPRFDB))
      write(LPCDB,'(a)')  DPCDB(1:lnblnk(DPCDB))
      write(MCMPDBFL,'(a)') DMCMPDBFL(1:lnblnk(DMCMPDBFL))

C Clear the model common blocks.
      call clrprb

      LUALF='UNKNOWN'
      bidirfile='UNKNOWN'
      CALENAME='UNKNOWN'
      if(iutdf.eq.0)IUTDF=IFIL+7  ! set if not already done so.

C Set flag indicating that solar radiation data is hour-centered (default)
C Variable will be reset if 'slr_half_hr' included in .cfg
      iSlr_half_hr_flg = 0

C Initialise NCM data read flags
      IRNCMD=0
      IRNCMF=0
      LASBEM='UNKNOWN'  ! assume no UK NCM description

C Initialize HVAC flag to indicate no HVAC models active. This
C will be reset if `*hvac' included in .cfg.
      ihvacflag=0
      hvacfile=' '

C AIM-2.
      LAIM2=' '
      iAIM2 = 0

C Fuel cell.
      iFClds=0
      LFClds=' '

c Mechanical ventilation
      LMech = ' '
      iMech = 0

C GSHP
C Initialize GSHP flag to indicate no GSHP models active. This
C will be reset if `*gshp' is included in the .cfg file.
      gshpfile = ' '
      igshp = 0

C GCEP (begin)
C Initialize GCEP flag to indicate no GCEP models active. This
C will be reset if `*gcep' is included in the .cfg file.
      gcepfile = ' '
      igcep = 0
C GCEP (end)

C Initialise day types
      idty=0

C Wind-generated electricity
      iWndSupp = 0
      LWndSupp =' '

C Clear zone related files.
      do 43 i=1,MCOM
        continue
   43 continue

C Assume older version of configuration file.
      icfgv=2

      nuco=0

C Initialize the indices.
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

C Set day types to default of 3 (weekday/saturday/sunday) and calendar
C day type tags to default as well. These 3 day types cannot be deleted
C and more day types if defined will be stored in CALENTAG(4) onwards.
C this can be modified later. [The IF clause may not be necessary]
      IF(NBDAYTYPE.LT.3)THEN
        nbdaytype=3
      ENDIF
      CALENTAG(1)='weekday'
      CALENTAG(2)='saturday'
      CALENTAG(3)='sunday'
      CALENDAYNAME(1)='generic weekday'
      CALENDAYNAME(2)='generic saturday'
      CALENDAYNAME(3)='generic sunday'

C Open the configuration file, return if not found otherwise set
C currentfile to the configuration file.
      IER=0
      INDCFG=1
      CALL EFOPSEQ(IUC,FILE,1,IER)
      IF(IER.LT.0)THEN
        if(dll)then
          dllsubr='ersys'
          dllmesg='The configuration file could not be opened.'
          ier=2
          return
        else
          IER=1
          RETURN
        endif
      ENDIF
      write(currentfile,'(a)') FILE(1:lnblnk(FILE))

C Derive the model root name from this in case none is specified.
C If FILE > 4 char it might have a .cfg attached, otherwise add.
      lcfgr=lnblnk(FILE)
      lcfgl=lcfgr-3
      if(lcfgr.gt.4)then
        if(FILE(lcfgl:lcfgr).eq.'.cfg')then
          if(lcfgl.gt.24)then
            write(cfgroot,'(a)',iostat=ios,err=99) FILE(1:24)
          else
            write(cfgroot,'(a)',iostat=ios,err=99) FILE(1:lcfgl-1)
          endif
        else
          if(lcfgr.gt.24)then
            write(cfgroot,'(a)',iostat=ios,err=99) FILE(1:24)
          else
            write(cfgroot,'(a)',iostat=ios,err=99) FILE(1:lcfgr)
          endif
        endif
      else
        if(lcfgr.gt.24)then
          write(cfgroot,'(a)',iostat=ios,err=99) FILE(1:24)
        else
          write(cfgroot,'(a)',iostat=ios,err=99) FILE(1:lcfgr)
        endif
      endif

C Read '*CONFIGURATION' and flag if a new format file otherwise
C treat as an old format.  If '*CONFIGURATION3.0' read zone info
C via tags. If '* CONFIGURATION4.' then expect to find IPV data
C embedded in the configuration file.
      CALL STRIPC(IUC,OUTSTR,99,ND,1,'config index',IER)
      IF(IER.NE.0)goto 1
      if(OUTSTR(1:17).eq.'* CONFIGURATION3.'.or.
     &   OUTSTR(1:17).eq.'* CONFIGURATION4.')then
        if(OUTSTR(1:17).eq.'* CONFIGURATION3.')icfgv=3
        if(OUTSTR(1:17).eq.'* CONFIGURATION4.')icfgv=4
  34    CALL STRIPC(IUC,OUTSTR,99,ND,1,'date stamp',IER)
        IF(IER.NE.0)goto 1
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','header tags',IFLAG)
        IF(IFLAG.NE.0)goto 1
        if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
          CALL EGETRM(OUTSTR,K,ectime,'W','date stamp',IER)
          IF(IER.NE.0)goto 1
          goto 34
        elseif(WORD(1:5).eq.'*root')then
          CALL EGETRM(OUTSTR,K,cfgroot,'W','root name',IER)
          IF(IER.NE.0)goto 1
          goto 34
        elseif(WORD(1:7).eq.'*zonpth')then
          CALL EGETRM(OUTSTR,K,zonepth,'W','path to zones',IER)
          IF(IER.NE.0)goto 1
          goto 34
        elseif(WORD(1:7).eq.'*netpth')then
          CALL EGETRM(OUTSTR,K,netpth,'W','path to nets',IER)
          IF(IER.NE.0)goto 1
          goto 34
        elseif(WORD(1:7).eq.'*ctlpth')then
          CALL EGETRM(OUTSTR,K,ctlpth,'W','path to ctl',IER)
          IF(IER.NE.0)goto 1
          goto 34
        elseif(WORD(1:7).eq.'*imgpth')then
          CALL EGETRM(OUTSTR,K,imgpth,'W','path to img',IER)
          IF(IER.NE.0)goto 1
          goto 34
        elseif(WORD(1:7).eq.'*tmppth')then
          CALL EGETRM(OUTSTR,K,tmppth,'W','path to scratch',IER)
          IF(IER.NE.0)goto 1
          goto 34
        elseif(WORD(1:7).eq.'*docpth')then
          CALL EGETRM(OUTSTR,K,docpth,'W','path to documents',IER)
          IF(IER.NE.0)goto 1
          goto 34
        elseif(WORD(1:7).eq.'*radpth')then
          CALL EGETRM(OUTSTR,K,radpth,'W','path to radnce',IER)
          IF(IER.NE.0)goto 1
          goto 34
        elseif(WORD(1:7).eq.'*dbspth')then
          CALL EGETRM(OUTSTR,K,dbspth,'W','path to loc databases',IER)
          IF(IER.NE.0)goto 1
          goto 34
        elseif(WORD(1:5).eq.'*indx')then
          CALL EGETWI(OUTSTR,K,INDCFG,0,3,'F','config index',IER)
          IF(IER.NE.0)goto 1
        endif
      elseif(OUTSTR(1:7).eq.'* CONFI')then
        icfgv=2  ! if we got here then it was not v3 or v4
        CALL STRIPC(IUC,OUTSTR,99,ND,1,'config index',IER)
        IF(ND.EQ.1)THEN
          K=0
          CALL EGETWI(OUTSTR,K,INDCFG,0,3,'F','config index',IER)
          IF(IER.NE.0)GOTO 1
        ELSE
          CALL USRMSG(' configuration index mismatch',OUTSTR,'W')
        ENDIF
      else
        call usrmsg('Does not appear to be a config file: ',FILE,'W')
        ier=1
        return
      endif

C Reporting.
      IF(ITRC.GE.1.AND.ITRC.LT.4)THEN
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
      ENDIF

C Now read all configuration data from file; start with site data.
C Latitude & Longitude.
      CALL STRIPC(IUC,OUTSTR,0,ND,1,'latitude & long dif',IER)
      IF(IER.NE.0)goto 1
      K=0
      CALL EGETWR(OUTSTR,K,sitelat,-90.,90.,'W','Latitude',IER)
      SLAT=sitelat
      CALL EGETWR(OUTSTR,K,sitelongdif,-15.,15.,'W',
     &  'Longitude dif from time meridian',IER)
      SLON=sitelongdif
      IF(IER.NE.0) GOTO 1

C Get site exposure and ground reflectance.
      CALL STRIPC(IUC,OUTSTR,99,ND,1,'ground reflectance',IER)
      IF(IER.NE.0)goto 1
      K=0
      CALL EGETWI(OUTSTR,K,siteexposureindex,1,8,'W',
     &  'Site exposure index',IER)
      CALL EGETWR(OUTSTR,K,groundrefl,0.,1.,'W','Ground reflect',IER)
      IF(IER.NE.0) GOTO 1

C Index of exposure has user defined data associated with it.
      IF(siteexposureindex.EQ.8) THEN
        CALL STRIPC(IUC,OUTSTR,3,ND,1,'user defined site exposure',IER)
        IF(IER.NE.0)goto 1
        K=0
        CALL EGETWR(OUTSTR,K,skyview,0.,1.,'W','Sky view',IER)
        CALL EGETWR(OUTSTR,K,groundview,0.,1.,'W','Ground view',IER)
        CALL EGETWR(OUTSTR,K,buildingview,0.,1.,'W','Building vew',IER)
        IF(IER.NE.0) GOTO 1

C Range checking.
        IF(ABS(skyview+groundview+buildingview-1.).GT..001) 
     &    CALL USRMSG(
     &    ' Total exposure out of range in',OUTSTR,'W')
      ENDIF

C Reporting.
      IF(ITRC.GE.1.AND.ITRC.LT.4)CALL SITEINFO(iuout)

C If initial configuration format (without database sections then
C skip to building/plant section.
      if(icfgv.eq.1) goto 45
      if(icfgv.gt.1)then

C If '* CONFIGURATION' found then read in the databases.
        CALL STRIPC(IUC,OUTSTR,0,ND,1,'* DATABASES',IER)
        if(IER.NE.0)return
        if(OUTSTR(1:7).ne.'* DATAB')then
          call edisp(iuout,' Did not find `* DATABASES` ')
          ier=1
          return
        endif
      endif

C Read databases until '* PROJ LOG' found.  If starts with *std
C then set the whichdbpath variable to signal that it is in
C the standard location. If tag does not start with *std then
C check the intial portion of the file name to see if it is
C local or absolute and if absolute whether it matches the
C standard path.

C Note that some file names could be long strings
C so LSTRIPC and LOUTSTR are used.
  44  continue
      lndbp=lnblnk(standarddbpath)
      call isunix(unixok)

      CALL LSTRIPC(IUC,LOUTSTR,99,ND,1,'db names',IER)
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','db names',IFLAG)
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

C Process the file name folloing *prs and depending on its path
C set the lapres common block string via call to findwhichdbpath.
        call findwhichdbpath('prs',lworking,ier)
      elseif(WORD(1:7).eq.'*stdprs')then
        ipathapres=2  ! standard folder for pressure coef database
        CALL EGETRM(LOUTSTR,K,LAPRES,'W','pressure db',IER)
      elseif(WORD(1:4).eq.'*evn')then
        CALL EGETRM(LOUTSTR,K,lworking,'W','profiles db',IER)

C Process the file name following *evn and depending on its path
C set the LPRFDB common block string via call to findwhichdbpath.
        call findwhichdbpath('evn',lworking,ier)
      elseif(WORD(1:7).eq.'*stdevn')then
        CALL EGETRM(LOUTSTR,K,LPRFDB,'W','profile db',IER)
        ipathprodb=2  ! standard folder for profiles database

      elseif(WORD(1:4).eq.'*clm')then

C << to be done - introduce *stdclm to signal standard location >>

C Read name of standard climate file and set a flag indicating
C that a single-year climate database has been defined
        CALL EGETRM(LOUTSTR,K,LCLIM,'W','climate db',IER)
        bSY_climate_defined = .true.        
      elseif(WORD(1:6).eq.'*myclm') then

C Read multi-year climate database and set flag indicating hat a muli-year
C climate database has been defined.     
        CALL EGETRM(LOUTSTR,K,cTemp,
     &    'W','Multi-year climate db',IER)
        cMY_climate_db_name = cTemp(1:lnblnk(cTemp))
        bMY_climates_defined = .true.
      elseif(WORD(1:7).eq.'*mscldb')then

C special materials and misc component database
        CALL EGETRM(LOUTSTR,K,lworking,'W','misc comp db',IER)

C Process the file name following *mscldb and depending on its path
C set the MCMPDBFL common block string via call to findwhichdbpath.
        call findwhichdbpath('msc',lworking,ier)
      elseif(WORD(1:10).eq.'*stdmscldb')then

C special materials and misc component database
        CALL EGETRM(LOUTSTR,K,MCMPDBFL,'W','misc comp db',IER)
        ipathmsc=2  ! standard folder for misc comp db
      elseif(WORD(1:4).eq.'*pdb')then
        CALL EGETRM(LOUTSTR,K,lworking,'W','plant comp db',IER)

C Process the file name folloing *pdb and depending on its path
C set the LPCDB common block string via call to findwhichdbpath.
        call findwhichdbpath('pdb',lworking,ier)

C Debug.
C        write(6,*) lworking
C        write(6,*) lpcdb
C        write(6,*) ' ersys ipathpcdb is ',ipathpcdb

      elseif(WORD(1:7).eq.'*stdpdb')then
        ipathpcdb=2  ! standard folder for plant template database
        CALL EGETRM(LOUTSTR,K,LPCDB,'W','plant template db',IER)
      elseif(WORD(1:5).eq.'*sbem')then

C Process the file name following *sbem and depending on its path
C set the LSBEM common block string via call to findwhichdbpath.
        CALL EGETRM(LOUTSTR,K,lworking,'W','SBEM db',IER)
        call findwhichdbpath('sbm',lworking,ier)
        ISBEM=1
        IRNCMD=1

      elseif(WORD(1:8).eq.'*stdsbem')then

C SBEM database file in standard location.
        ipathsbem=2  ! standard folder for SBEM database
        CALL EGETRM(LOUTSTR,K,LSBEM,'W','standard SBEM db',IER)
        ISBEM=1
        IRNCMD=1

      elseif(WORD(1:4).eq.'*ctl')then
        CALL EGETRM(LOUTSTR,K,LCTLF,'W','config control',IER)

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

C Number of days with snow on the ground (if ground refl model 2)
        DO 442 I=1,12
          CALL EGETWI(LOUTSTR,K,dayswithsnow(I),0,31,'W',
     &      'days with snow each month',IER)
  442   CONTINUE

      elseif(WORD(1:5).eq.'*sndf')then

C Snow depth file (if ground refl. model 3)
        CALL EGETRM(LOUTSTR,K,SNFNAM,'W','snow depth file',IER)

      elseif(WORD(1:4).eq.'*bcd')then

C Flexible boundry condition data definition.  
        call EGETRM(LOUTSTR,K,cTemp,'W','BC data file', IER)
        cBC_data_file_name = cTemp(1:lnblnk(cTemp))
        bBC_data_defined = .true.
      elseif(WORD(1:4).eq.'*shl')then

C There is a SHOCC library associated with the model so set
c bSHOCCed true and then check and see if the file exists.
        CALL EGETRM(LOUTSTR,K,SHOCCshlFile,'W','SHOCC',IER)
        XST=.FALSE.
        call FINDFIL(SHOCCshlFile,XST)
        if(XST) then
          bSHOCCed=.true.
        else
          write(outs,'(3a)') 'SHOCC project library file ',
     &      SHOCCshlFile(1:lnblnk(SHOCCshlFile)),
     &      ' was referenced but not found.'
        endif

      elseif(WORD(1:5).eq.'*hvac')then

C hvac file, test and see if it exists.
        CALL EGETRM(LOUTSTR,K,hvacfile,'W','config control',IER)
        XST=.FALSE.
        call FINDFIL(hvacfile,XST)
        if(XST)then
          ihvacflag=1
        else
          write(outs,'(3a)') 'Hvac file ',hvacfile(1:lnblnk(hvacfile)),
     &      ' was referenced but not found.'
          if(dll)then
            dllsubr='ersys'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
          endif
        endif
      elseif(WORD(1:4).eq.'*aim')then

C AIM-2, test and see if it exists.
        CALL EGETRM(LOUTSTR,K,LAIM2,'W','AIM-2 infil data',IER)
        XST=.FALSE.
        call FINDFIL(LAIM2,XST)
        if(XST)then
          iAIM2=1
        else
          write(outs,'(3a)') 'AIM-2 file ',LAIM2(1:lnblnk(LAIM2)),
     &      ' was referenced but not found.'
          if(dll)then
            dllsubr='ersys'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
          endif
        endif
      elseif(WORD(1:8).eq.'*roaming')then

C Changing orientation definition
        IROAM=1
        call EGETRM(LOUTSTR,K,LROAM,'-','Roaming file name', IER)
        XST=.FALSE.
        call FINDFIL(LROAM,XST)
        if(.NOT.XST)then
          call usrmsg('Roaming file not found; File name: ',LROAM,'W')
        endif
      elseif(WORD(1:4).eq.'*tab')then

C Lookup table definition      
        call EGETRM(LOUTSTR,K,cTemp,'W','Lookup table data file', IER)
        cLookup_data_file_name = cTemp(1:lnblnk(cTemp))
        bLookup_data_defined = .true.
      elseif(WORD(1:4).eq.'*ctm')then

C Contaminant model 
        CALL EGETRM(LOUTSTR,K,TEXT,'F','contaminant model',IER)
        CNTMFIL=TEXT(1:72)
        NOCNTM=1
        IF(IER.NE.0)GOTO 1

C Open the contaminant file and fill in common block variables
        IUNIT=IFIL+72
        CALL EFOPSEQ(IUNIT,CNTMFIL,1,IER)
        IF(IER.LT.0)then
          outs='Problem opening contaminant file...aborting cfg scan.'
          if(dll)then
            dllsubr='mzinpt'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            goto 6
          endif
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
        CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
        CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,' ',IER)
        J=0
        IF(OUTSTR(:9).NE.'SECTION_1'.OR.IER.NE.0)THEN
          outs='Problem scanning contaminant file...aborting cfg scan'
          if(dll)then
            dllsubr='mzinpt'
           dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            goto 6
          endif
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
          if(dll)then
            dllsubr='ersys'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            STOP ' Need elec loads data: unresolvable error.'
          endif
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
          if(dll)then
            dllsubr='ersys'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)

          endif
        endif

      elseif(WORD(1:5).eq.'*mvnt')then

C Mechanical Ventilation, test and see if it exists.
        CALL EGETRM(LOUTSTR,K,LMech,'W','Mechanical Ventilation data',
     &    IER )
        XST=.FALSE.
        call FINDFIL(LMech,XST)
        if(XST)then
         iMech=1
        else
          write(outs,'(3a)') 'Mechanical Ventilation file ',
     &      LMech(1:lnblnk(LMech)),' was referenced but not found.'
          if(dll)then
            dllsubr='ersys'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
          endif
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
     &       ' was referenced but not found.'
          if(dll)then
            dllsubr='ersys'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
          endif
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
     &      ' was referenced but not found.'
          if(dll)then
            dllsubr='ersys'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
          endif
        endif
      elseif(WORD(1:4).eq.'*wnd')then

C Wind-generated electricity
C If a file holding the time history of wind-generated electricity exists,
C then get the name of the file and test for its existence.
        CALL EGETRM(LOUTSTR,K,LWndSupp,'W',
     &    'Wind-gen elec supply file',IER )
        XST=.FALSE.
        call FINDFIL(LWndSupp,XST)
        if(XST)then
          iWndSupp=1
        else
          write(outs,'(3a)') 'Wind-gen elec supply file ',
     &     LWndSupp(1:lnblnk(LWndSupp)),' was referenced but not found.'
          if(dll)then
            dllsubr='ersys'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            STOP ' Need wind elec supply data: unresolvable error.'
          endif
        endif
      elseif(WORD(1:12).eq.'*slr_half_hr')then

C Solar radiation (half hour data) flag.
        call egetwi(loutstr,k,iSlr_half_hr_flg,0,1,'W',
     &    'solar half-hour flag',IER)

      elseif(WORD(1:17).eq.'*exlude_diff_shad')then

C Solar radiation (half hour data) flag.
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
      elseif(WORD(1:9).eq.'*calename')then

C There is a calendar of day types defined read its name.
        CALL EGETRM(LOUTSTR,K,calename,'W','calendar name',IER)
 443    CALL STRIPC(IUC,OUTSTR,0,ND,1,'calendar data',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','calendar tag',IFLAG)
        if(WORD(1:9).eq.'*calentag')then
          idty=idty+1
          CALL EGETP(OUTSTR,K,calentag(idty),'W','cal tag',IER)
          CALL EGETP(OUTSTR,K,calendayname(idty),'W',
     &      'cal day name',IER)
          CALL EGETWI(OUTSTR,K,nbcaldays(idty),0,365,'W',
     &      'days assoc with day type',IER)
          goto 443
        elseif(WORD(1:5).eq.'*list')then
          K=6
          CALL EGETWI(OUTSTR,K,NBDAYTYPE,3,MDTY,'W','day types',IER)
          CALL EGETWIA(IUC,IRCC,365,0,10,'W','calendar day types',IER)
          DO 118 KV=1,365
            icalender(KV)=IRCC(KV)
  118     CONTINUE
          goto 443
        elseif(WORD(1:9).eq.'*end_list')then
          continue
        endif
      elseif(WORD(1:4).eq.'*ual')then
        CALL EGETRM(LOUTSTR,K,LUALF,'W','uncertainty lib',IER)
      elseif(WORD(1:8).eq.'*weekend')then
        CALL EGETWI(LOUTSTR,K,idwe1,1,7,'W','weekend day',IER)
        CALL EGETWI(LOUTSTR,K,idwe2,1,7,'W','weekend day',IER)
        wkd1=wkday(idwe1)
        wkd2=wkday(idwe2)

C Seasonal data (similar to climatelist file scan in rcdblist.F)
      elseif(WORD(1:8).eq.'*seasons')then
        ihaveseason=1   ! signal a local definition exists
 444    CALL STRIPC(IUC,OUTSTR,0,ND,1,'seasons data',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','seasons tag',IFLAG)
        if(WORD(1:5).eq.'*aide')then
          call egetrm(outstr,K,clmlaid,'W','brief description',IER)
          goto 444
        elseif(WORD(1:5).eq.'*name')then
          call egetrm(outstr,K,clmlnam,'W','menu entry',IER)
          goto 444
        elseif(WORD(1:9).eq.'*winter_t')then

C Instanciate typper common block variables ia1wins,ia1winf,ia2wins,ia2winf 
          call egetrm(outstr,K,pstring,'W','winter typ',IER)

C Has start and end day of both winters.
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

C Instanciate typper common block variables ia1sprs,ia1sprf,ia2sprs,ia2sprf 
          call egetrm(outstr,K,pstring,'W','spring typ',IER)

C Has start and end day and typical day of spring and autumn.
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

C Instanciate typper common block variables iasums & iasumf
          call egetrm(outstr,K,pstring,'W','summer typ',IER)
          K=0
          CALL EGETWI(pstring,K,ID1,1,31,'F','day',IER)
          CALL EGETWI(pstring,K,IM1,1,12,'F','month',IER)
          CALL EGETWI(pstring,K,ID2,1,31,'F','day',IER)
          CALL EGETWI(pstring,K,IM2,1,12,'F','month',IER)
          CALL EDAY(ID1,IM1,iasums)
          CALL EDAY(ID2,IM2,iasumf)
          goto 444
        elseif(WORD(1:9).eq.'*winter_s')then

C Read winter season - two periods e.g. nov-dec & jan-feb
C common block typsea is1wins,is1winf,is2wins,is2winf
          call egetrm(outstr,K,pstring,'W','winter sea',IER)
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

C Read transition season - two periods e.g. mar-may & sep-oct
C common block typsea is1sprs,is1sprf,is2sprs,is2sprf
          call egetrm(outstr,K,pstring,'W','trans sea',IER)
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

C Read summer season - e.g. jun-aug (common block typsea is1sums,is1sumf).
          call egetrm(outstr,K,pstring,'W','summer sea',IER)
          K=0
          CALL EGETWI(pstring,K,ID1,1,31,'F','day',IER)
          CALL EGETWI(pstring,K,IM1,1,12,'F','month',IER)
          CALL EGETWI(pstring,K,ID2,1,31,'F','day',IER)
          CALL EGETWI(pstring,K,IM2,1,12,'F','month',IER)
          CALL EDAY(ID1,IM1,is1sums)
          CALL EDAY(ID2,IM2,is1sumf)
          goto 444
        elseif(WORD(1:11).eq.'*help_start')then

C Read and store the comments into the HELP array for later display.
          nbclmlhelp=0
 445      CALL STRIPC(IUC,OUTSTR,0,ND,0,'help line',IER)
          if(OUTSTR(1:11).eq.'*season_end')then
            continue
          else
            nbclmlhelp=nbclmlhelp+1
            lastcc = MIN0(lnblnk(OUTSTR),72)
            if(nbclmlhelp.lt.60)then
              write(clmlhelp(nbclmlhelp),'(a)')OUTSTR(1:lastcc)
            endif
            goto 445
          endif
        endif

      elseif(WORD(1:5).eq.'*year')then
        CALL EGETWI(LOUTSTR,K,IYEAR,1900,2020,'W','year',IER)

C Perform check that calendar day types correspond with simulation year
C If more than three day types are declared then retain those day types
C otherwise overwrite 
        ICREPORT=0
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
            IF(ICALENDER(IDOL).NE.IDTYY)THEN
              ICALENDER(IDOL)=IDTYY
              ICREPORT=1
            ENDIF
          ENDIF
 123    CONTINUE
        IF(ICREPORT.EQ.1)CALL EDISP(IUOUT,
     &    'Simulation year and calendar mismatch rectified')
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
      elseif(WORD(1:6).eq.'*pecnv')then

C System level primary energy conversion data, set flag.
        ipconv=1
        CALL EGETWR(LOUTSTR,K,pcnvht,0.,0.,'-','heat p cnv',IER)
        CALL EGETWR(LOUTSTR,K,pcnvcl,0.,0.,'-','cool p cnv',IER)
        CALL EGETWR(LOUTSTR,K,pcnvlt,0.,0.,'-','light p cnv',IER)
        CALL EGETWR(LOUTSTR,K,pcnvfn,0.,0.,'-','fan p cnv',IER)
        CALL EGETWR(LOUTSTR,K,pcnvsp,0.,0.,'-','sml p cnv',IER)
        CALL EGETWR(LOUTSTR,K,pcnvhw,0.,0.,'-','hw p cnv',IER)
      elseif(WORD(1:5).eq.'*ecnv')then

C Effeciencies data line currently unused so skip.
        continue
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
          CALL EGETWI(LOUTSTR,K,ipvform,0,4,'W','ipv rep format',IER)
          if(ND.gt.3)then
            CALL EGETW(LOUTSTR,K,ipvsimu,'W','*IPV assessment',IFLAG)
          else
            ipvsimu='------'
          endif
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
        ISBEM=1
        IRNCMF=1
      elseif(WORD(1:4).eq.'*not')then

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
      elseif(WORD(1:4).eq.'*g1d')then

C Building 1D node distribution.
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

      elseif(WORD(1:4).eq.'*tdf')then

C If marked for Temporal file get the name and if it exists
C scan first line/record and set ITDFLG appropriately.
C If itdflg non-zero read any association included in the configuration
C file. Save detailed checking of the temporal file to the
C context code in the project manager or the input code of
C the simulator or the results analyis module.
        CALL EGETRM(LOUTSTR,K,LTDFA,'W','TDF file name',IER)

C Read the name of the tdf file and check if it exists. 
        CALL ERPFREE(IUTDF,ISTAT)
        call FINDFIL(LTDFA,XST)
        if(XST)then

C Try to open as ascii file first. If that fails try as a binary file.
          CALL EFOPSEQ(IUTDF,LTDFA,1,IER)
          if(ier.eq.0)then
            CALL STRIPC(IUTDF,outstr,99,ND,1,'header',IER)
            if(OUTSTR(1:9).eq.'ASCIITDF3')THEN
              ITDFLG=3
            elseif(OUTSTR(1:9).eq.'ASCIITDF2')THEN
              ITDFLG=2
            elseif(OUTSTR(1:12).eq.'TDFdatabase2')THEN
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
 103          msgl2='in the temporal file'
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
 102        msgl2='in the temporal file'
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
  104   if(itdflg.ne.0)then

C Read references to TDF id's, and then the related tasks and zones.
          M=0
   59     CALL STRIPC(IUC,OUTSTR,99,ND,1,'instance names',IER)
          IF(IER.NE.0)GOTO 58
          IF(OUTSTR(1:9).EQ.'* end tdf')GOTO 58
          M=M+1
          DBTAG(M)=' '
          DBTASK(M)=' '
          DBZN(M)=' '
          DBSN(M)=' '
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','tdf instance name',IER)
          DBTAG(M)=WORD(1:12)
          if(ND.gt.1)then
            CALL EGETW(OUTSTR,K,WORD,'W','related task/type',IER)
            DBTASK(M)=WORD(1:12)
          endif
          if(ND.gt.2)then
            CALL EGETW(OUTSTR,K,WORD,'W','related zone',IER)
            DBZN(M)=WORD(1:15)
          endif
          if(ND.gt.3)then
            CALL EGETW(OUTSTR,K,WORD,'W','related surface',IER)
            DBSN(M)=WORD(1:15)
          endif            
          WRITE(OUTS,'(A,I2,8A)') ' Tdf item:',M,':',DBTAG(M),
     &      ' of type: ',DBTASK(M),' associated with ',DBZN(M),
     &      'and surface',DBSN(M)
          IF(ITRC.GE.1)CALL EDISP(iuout,OUTS)
        GOTO 59
   58   CONTINUE
        endif
        ITEMSTD=M
        CALL ERPFREE(IUTDF,ISTAT)
        
      elseif(WORD(1:4).eq.'*sps' .or. WORD(1:6).eq. '*mysps')then
      
C Check for simulation setup data and save level.
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
          
          CALL STRIPC(IUC,OUTSTR,0,ND,1,'sim set',IER)

          if (OUTSTR(1:14) .eq. '*start_my_sets' ) then
            bMY_sim_sps = .true.
            CALL STRIPC(IUC,OUTSTR,0,ND,1,'sim set',IER)
          elseif ( OUTSTR(1:12) .eq. '*end_my_sets') then
            bMY_sim_sps = .false.
            CALL STRIPC(IUC,OUTSTR,0,ND,1,'sim set',IER)
          endif
          
C Set flag indicating if set describes a multi-year period
          bMY_period_support (iss) = bMY_sim_sps
          
          K=0
          CALL EGETWI(OUTSTR,K,isstday(iss),1,31,'W','start day',IER)
          CALL EGETWI(OUTSTR,K,isstmon(iss),1,12,'W','start month',IER)
          if ( bMY_sim_sps )
     &      CALL EGETWI(OUTSTR,K,iMY_Period_Start_years(iss),
     &                 1800,2100,'W','start year',IER)
          CALL EGETWI(OUTSTR,K,isfnday(iss),1,31,'W','end day',IER)
          CALL EGETWI(OUTSTR,K,isfnmon(iss),1,12,'W','end month',IER)          
          if ( bMY_sim_sps )
     &      CALL EGETWI(OUTSTR,K,iMY_Period_end_years(iss),
     &                 1800,2100,'W','end year',IER)

C Opportunity for improvenemt: Should error trap duplicate
C (ie ambigious) set names
          CALL EGETRM(OUTSTR,K,spfdescr(iss),'W','set name',IER)

 334      CALL STRIPC(IUC,OUTSTR,0,ND,1,'res names',IER)
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','res tags',IFLAG)
          if(WORD(1:5).eq.'*sblr')then
            CALL EGETRM(OUTSTR,K,sblres(iss),'W','zone res',IER)
          elseif(WORD(1:5).eq.'*sflr')then
            CALL EGETRM(OUTSTR,K,sflres(iss),'W','netw res',IER)
          elseif(WORD(1:5).eq.'*splr')then
            CALL EGETRM(OUTSTR,K,splres(iss),'W','plnt res',IER)
          elseif(WORD(1:6).eq.'*smstr')then
            CALL EGETRM(OUTSTR,K,smstres(iss),'W','moist res',IER)
          elseif(WORD(1:5).eq.'*selr')then
            CALL EGETRM(OUTSTR,K,selres(iss),'W','elect res',IER)
          elseif(WORD(1:5).eq.'*spvr')then
            continue
          elseif(WORD(1:8).eq.'*end_set')then
            goto 333   
          elseif(WORD(1:8).eq.'*end_sps')then
            goto 44
          endif
          goto 334
 333    continue

      elseif(WORD(1:8).eq.'*end_sps' .or.
     &       WORD(1:12).eq.'*end_my_sets' )then
        goto 44
      elseif(WORD(1:7).eq.'*intipv')then
        CALL EGETRM(LOUTSTR,K,sipvres,'W','ipv report',IER)
        goto 44
      elseif(WORD(1:7).eq.'*annipv')then

C Annual IPV report depreciated in favour of sipvres.
        continue
        goto 44
      elseif(WORD(1:7).eq.'*B-NAME')then

C Next tokens relate to building address.
        CALL EGETRM(LOUTSTR,K,pjname,'W','project complience name',IER)
        goto 44
      elseif(WORD(1:10).eq.'*B-ADDRESS')then
        CALL EGETRM(LOUTSTR,K,baddress,'W','building address',IER)
        goto 44
      elseif(WORD(1:7).eq.'*B-CITY')then
        CALL EGETRM(LOUTSTR,K,bcity,'W','building city',IER)
        goto 44
      elseif(WORD(1:11).eq.'*B-POSTCODE')then
        CALL EGETRM(LOUTSTR,K,buildingPC,'W','building post code',IER)
        goto 44
      elseif(WORD(1:7).eq.'*O-NAME')then

C Next tokens relate to owner/client address.
        CALL EGETRM(LOUTSTR,K,ownername,'W','owner name',IER)
        goto 44
      elseif(WORD(1:10).eq.'*O-ADDRESS')then
        CALL EGETRM(LOUTSTR,K,owneraddr,'W','owner address',IER)
        goto 44
      elseif(WORD(1:7).eq.'*O-CITY')then
        CALL EGETRM(LOUTSTR,K,ownercity,'W','owner city',IER)
        goto 44
      elseif(WORD(1:11).eq.'*O-POSTCODE')then
        CALL EGETRM(LOUTSTR,K,ownerPC,'W','owner post code',IER)
        goto 44
      elseif(WORD(1:12).eq.'*O-TELEPHONE')then
        CALL EGETRM(LOUTSTR,K,ownertel,'W','owner phone',IER)
        goto 44

C Next tokens relate to simulationist address.
      elseif(WORD(1:7).eq.'*C-NAME')then
        CALL EGETRM(LOUTSTR,K,certifname,'W','simu staff name',IER)
        goto 44
      elseif(WORD(1:12).eq.'*C-TELEPHONE')then
        CALL EGETRM(LOUTSTR,K,certTel,'W','simu staff phone',IER)
        goto 44
      elseif(WORD(1:10).eq.'*C-ADDRESS')then
        CALL EGETRM(LOUTSTR,K,certaddress,'W','simu staff address',IER)
        goto 44
      elseif(WORD(1:7).eq.'*C-CITY')then
        CALL EGETRM(LOUTSTR,K,certcity,'W','simu staff city',IER)
        goto 44
      elseif(WORD(1:11).eq.'*C-POSTCODE')then
        CALL EGETRM(LOUTSTR,K,certPC,'W','simu staff post code',IER)
        goto 44

      elseif(loutstr(1:10).eq.'* PROJ LOG')then

C Project log file, read and then go on to fabric/plant description.
        CALL STRIPC(IUC,OUTSTR,0,ND,1,'project log file',IER)
        LPRJLG=OUTSTR(1:LNBLNK(OUTSTR))
        goto 45
      elseif ( .not. bSilent ) then

        call usrmsg(' Unknown entry format... ',outstr,'W')
        dok=.true.
        h(1)='An unknown tag or line was found in the configuration'
        h(2)='file. The file might be corrupt or for a different'
        h(3)='version of ESP-r. '
        h(4)=' '
        h(5)='Depending on the error, it might be possible to read'
        h(6)='the rest of the file - exercise caution! '
        CALL ASKOK('  ','Continue reading configuration?',OK,dok,6)
        if(.NOT.ok)then
          ier=1
          return
        endif

      else
          ier=1
          return
      endif

C Loop back for next database file.
      goto 44
      
C --- end of LSTRIPC and LOUTSTRT scanning ---

C Warn user if notional or stripped model. These are generated
C automatically and should not be modified through project manager
 45   H(1)='Additional models are built during the calculation '
      H(2)='process for the UK NCM. These should not be modified '
      H(3)='manually i.e. using the project manager (or any other)'
      H(4)='interface. These models are '
      H(5)=' *_str.cfg stripped model'
      H(6)=' *_not.cfg notional model'
      H(7)=' *_typ.cfg typical model'
      H(8)=' *_ref.cfg reference model'
      H(9)=' '
      h(10)='Only proceed if you are sure of what you are doing'
      IWARN=0
      IF(INOTI.EQ.1.AND.ITRC.NE.4)THEN
        IWARN=1
        OUTS=
     &  'This is a UK NCM notional model and should not be modified'
      ELSEIF(INOTI.EQ.2.AND.ITRC.NE.4)THEN
        IWARN=1
        OUTS=
     &  'This is a UK NCM reference model and should not be modified'
      ELSEIF(INOTI.EQ.3.AND.ITRC.NE.4)THEN
        IWARN=1
        OUTS=
     &  'This is a UK NCM typical model and should not be modified'
      ELSEIF(INOTI.EQ.4.AND.ITRC.NE.4)THEN
        IWARN=1
        OUTS=
     &  'This is a UK NCM stripped model and should not be modified'
      ELSEIF(ISBEM.EQ.1.AND.INOTI.EQ.0.AND.ITRC.NE.4)THEN
        continue  ! this model is original which includes NCM file.
      ENDIF
      IF(IWARN.EQ.1.AND..NOT.AUTOOK)THEN
         call askok(OUTS,
     &   'using project manager are you sure you want to continue',
     &   OK,dok,10)
         IF(.NOT.OK)STOP
      ENDIF

C If building or building & plant then read in the building info.
      IF(INDCFG.EQ.2)goto 888

C At this point there may be ground information, if abscent then
C this is the `* Building` section.
      CALL STRIPC(IUC,OUTSTR,0,ND,1,'building name',IER)
      IF(IER.NE.0)goto 1
      if(OUTSTR(1:8).eq.'* Ground'.or.OUTSTR(1:8).eq.'* GROUND')then
 344    CALL STRIPC(IUC,OUTSTR,0,ND,1,'ground info',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','ground tags',IFLAG)

C External longwave radiation: if gtmp tag present in cfg file, get profile.
        IF(WORD(1:5).EQ.'*gtmp')THEN
          CALL EGETWRA(IUC,getVal,12,-9.0,99.0,'W','Ex lg rad',IER)
          iExtLgRadFlag = 1
C Store profile in array.
          DO 27 jMon=1,12
            eGrdTp(jMon)=getVal(jMon)
  27      CONTINUE
        elseif(WORD(1:4).eq.'*cvs')then

C 3D ground control volumes file.
          IGDCVS=.TRUE.
          CALL EGETRM(OUTSTR,K,LGDCVS,'W','3d ground vols',IER)

C 3D ground connections file.
        elseif(WORD(1:4).eq.'*cnc')then
          IGDCNC=.TRUE.
          CALL EGETRM(OUTSTR,K,LGDCNC,'W','3d ground conns',IER)

C 3D ground nodes coordinates file.
        elseif(WORD(1:4).eq.'*ndc')then
          IGDNDC=.TRUE.
          CALL EGETRM(OUTSTR,K,LGDNDC,'W','3d ground cords',IER)

C 3D ground nodes coordinates file.
        elseif(WORD(1:4).eq.'*taq')then
          IGDTAQ=.TRUE.
          CALL EGETRM(OUTSTR,K,LGDTAQ,'W','3d ground temps',IER)

C Ground topology.
        elseif(WORD(1:4).eq.'*gtp')then
          CALL EGETRM(OUTSTR,K,GTGEOM,'W','ground topology',IER)

C Ground monthly temperature profiles. 2nd item is the number of profiles.
        elseif(WORD(1:4).eq.'*mgp')then
          CALL EGETWI(OUTSTR,K,NGRDP,1,MGRDP,'F','grn prfls',IER)
          IF(NGRDP.eq.0) goto 344
          DO 28 IGRDP=1,NGRDP
            CALL EGETWRA(IUC,GVA,12,-9.,99.,'W','Gr tmp',IER)
            DO 29 J=1,12
              UGRDTP(J,IGRDP)=GVA(J)
   29       CONTINUE

C Reporting.
            IF(ITRC.GE.1.AND.ITRC.LT.4)THEN
              CALL EDISP(iuout,' ')
              CALL EDISP(iuout,' Ground temperature profile Jan-Dec:')
              WRITE(OUTS,'(12F6.1)')(UGRDTP(J,IGRDP),J=1,12)
              CALL EDISP(iuout,OUTS)
            ENDIF
  28      CONTINUE
          goto 344
        elseif(WORD(1:4).eq.'*end')then

C Set 3D ground flag if 3D files found and read next section.
          IF(IGDCVS.AND.IGDCNC.AND.IGDNDC.AND.IGDTAQ)GRND3D=.TRUE.
          goto 45
        endif

C Read more ground related information.
        goto 344
      elseif(OUTSTR(1:10).eq.'* Building'.or.
     &       OUTSTR(1:10).eq.'* BUILDING')then
        CALL STRIPC(IUC,OUTSTR,0,ND,1,'model name',IER)
        IF(IER.NE.0)goto 1
        modeltitle=OUTSTR(1:72)
      else
        modeltitle=OUTSTR(1:72)
      endif

C If ground information was read in but still registration level then exit.
      if(INDCFG.EQ.0)then
        CALL ERPFREE(IUC,ISTAT)
        RETURN
      endif

C Number of building zones.
      CALL STRIPC(IUC,OUTSTR,0,ND,1,'no of zones',IER)
      IF(IER.NE.0)goto 1
      K=0
      CALL EGETWI(OUTSTR,K,NCOMP,0,MCOM,'W','zones',IER)
      IF(IER.NE.0) GOTO 1

C Reporting.
      IF(ITRC.GE.1)THEN
        WRITE(OUTS,'(2a)')' The model title is ',
     &    modeltitle(1:LNBLNK(modeltitle))
        CALL EDISP(iuout,OUTS)
        CALL EDISP(iuout,' ')
        if(NCOMP.gt.10)then
          call edisp(iuout,' Please wait...')
        endif
        WRITE(OUTS,9994)NCOMP
 9994     FORMAT(' It is composed of ',I2,' thermal zones which are',
     &    ' described in the following files:')
        CALL EDISP(iuout,OUTS)
        CALL EDISP(iuout,' ')
      ENDIF

C Zone file names.
      icomp=0
   46 if(icfgv.eq.3.or.icfgv.eq.4)then
        CALL STRIPC(IUC,OUTSTR,0,ND,1,'zone data',IER)
        IF(IER.NE.0)goto 1
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','tags',IFLAG)
        if(WORD(1:5).ne.'*zend')then
          CALL EGETRM(OUTSTR,K,text,'W','remaining text',IFLAG)
        endif
        IF(IFLAG.NE.0)goto 1
        if(WORD(1:4).eq.'*zon')then
          OK3DCVS=.FALSE.
          OK3DCNC=.FALSE.
          OK3DNDC=.FALSE.
          OK3DTAQ=.FALSE.
          icomp=icomp+1
          K=0
          CALL EGETWI(text,K,NCCODE(ICOMP),1,100,'W','zone index',IER)
          IF(IER.NE.0) GOTO 1
        elseif(WORD(1:4).eq.'*opr')then
          LPROJ(icomp)=text(1:72)
        elseif(WORD(1:4).eq.'*geo')then
          LGEOM(icomp)=text(1:72)
        elseif(WORD(1:4).eq.'*con')then
          LTHRM(icomp)=text(1:72)
        elseif(WORD(1:4).eq.'*utl')then
          msgl2='This is no longer supported. Skipping...'
          call usrmsg('Detected a ANCIENT zone utility file.',
     &      msgl2,'W')
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
        elseif(WORD(1:4).eq.'*isi')then
          isi(icomp)=1
          LSHAD(icomp)=text(1:72)
        elseif(WORD(1:4).eq.'*ihc')then
          ihc(icomp)=1
          LHCCO(icomp)=text(1:72)
        elseif(WORD(1:4).eq.'*ibc')then
          msgl2='This is no longer supported. Skipping...'
          call usrmsg('Detected a default window bind control descr. ',
     &     msgl2,'W')
        elseif(WORD(1:4).eq.'*iaf')then
          msgl2='No longer supported (use temporal). Skipping...'
          call usrmsg('Detected ascii timestep airflows. ',
     &     msgl2,'W')
        elseif(WORD(1:4).eq.'*icg')then
          msgl2='No longer supported (use temporal). Skipping...'
          call usrmsg('Detected ascii timestep casual gains.',
     &     msgl2,'W')
        elseif(WORD(1:4).eq.'*cfd')then

C If a DFD file exists, open it and get the conflation type.
          LCFD(icomp)=text(1:72)
          call FINDFIL(LCFD(ICOMP),XST)
          if(XST)then
            write(currentfile,'(a)') LCFD(ICOMP)(1:lnblnk(LCFD(ICOMP)))
            CALL EFOPSEQ(IUF,LCFD(ICOMP),1,IER)
            IF(IER.NE.0)goto 1
            CALL STRIPC(IUF,OUTSTR,0,ND,1,'dfd line 1',IER)
            if(OUTSTR(1:15).eq.'DFS DESCRIPTION')then
              CALL STRIPC(IUF,OUTSTR,0,ND,1,'dfd line 2',IER)
              K=0
              CALL EGETW(OUTSTR,K,WORD,'W','tag',IFLAG)
              if(WORD(1:11).eq.'*conflation')then
                CALL EGETWI(OUTSTR,K,iv,0,7,'F','confla type',IER)
                IFCFD(icomp)=iv
              else
                call usrmsg('DFD (ver 1) conflation tag missing in:',
     &          LCFD(ICOMP),'W')
              endif
            elseif(OUTSTR(1:7).eq.'*DFS V2')then
              CALL STRIPC(IUF,OUTSTR,0,ND,1,'dfd line 2',IER)
              K=0
              CALL EGETW(OUTSTR,K,WORD,'W','tag',IFLAG)
              if(WORD(1:11).eq.'*conflation')then
                CALL EGETWI(OUTSTR,K,iv,0,7,'F','confla type',IER)
                IFCFD(icomp)=iv
              else
                call usrmsg('DFD (ver 2) conflation tag missing in:',
     &          LCFD(ICOMP),'W')
              endif
            else
              call usrmsg('Referenced file not DFD description',
     &          LCFD(ICOMP),'W')
            endif
            CALL ERPFREE(IUF,ISTAT)
            NCONF=NCONF+1
            icfdnd(NCONF)=ICOMP
            write(currentfile,'(a)') FILE(1:lnblnk(FILE))
          else
            call usrmsg('DFD file not found',LCFD(ICOMP),'W')
          endif
        elseif(WORD(1:4).eq.'*cvs')then
          OK3DCVS=.true.
          L3DCVS(icomp)=text(1:72)
        elseif(WORD(1:4).eq.'*cnc')then
          OK3DCNC=.true.
          L3DCNC(icomp)=text(1:72)
        elseif(WORD(1:4).eq.'*ndc')then
          OK3DNDC=.true.
          L3DNDC(icomp)=text(1:72)
        elseif(WORD(1:4).eq.'*3dt')then
          OK3DTAQ=.true.
          L3DTAQ(icomp)=text(1:72)
        elseif(WORD(1:4).eq.'*mst')then
          MSTRZN(icomp)=.true.
          LMOIST(icomp)=text(1:72)
          MSTROK=.TRUE.

        elseif(WORD(1:4).eq.'*shz')then

C A SHOCC zone file is included, check and see if it exists
C and if it does so set bZoneSHOCCed(icomp) to true.
          SHOCCshzFile(icomp)=text(1:72)
          XST=.FALSE.
          call FINDFIL(SHOCCshzFile(icomp),XST)
          if(XST) then
            bZoneSHOCCed(icomp)=.true.
          else
            write(outs,'(3a)') 'SHOCC zone file ',
     &        SHOCCshzFile(icomp)(1:lnblnk(SHOCCshzFile(icomp))),
     &        ' was referenced but not found.'
          endif

        elseif(WORD(1:4).eq.'*bsm')then

C A BASESIMP description is included in the model. Check if it
C exists and set iBSIMP(icomp) to one if it does.
          LBSIMP(icomp)=text(1:72)
          XST=.FALSE.
          call FINDFIL(LBSIMP(icomp),XST)
          if(XST)then
            iBSIMP(icomp)=1
          else
            write(outs,'(3a)') 'BASESIMP file ',
     &        LBSIMP(icomp)(1:lnblnk(LBSIMP(icomp))),
     &        ' was referenced but not found.'
            if(dll)then
              dllsubr='ersys'
              dllmesg=outs
              ier=2
              return
            else
              call edisp(iuout,outs)
              iBSIMP(icomp)=0
            endif
          endif
        elseif(WORD(1:4).eq.'*csm')then
          if(IndxSt.eq.0)then
            IndxSt=icomp
            LGrdSt=text(1:72)
          else
            msgl2=' Only the 1st structured zone considered.'
            CALL USRMSG(' Multiple structured zones!',msgl2,'W')
          endif
        elseif(WORD(1:5).eq.'*zend')then
          if(OK3DCVS.and.OK3DCNC.and.OK3DNDC.and.OK3DTAQ)then
            ZONE3D(icomp)=.TRUE.
          else
            ZONE3D(icomp)=.FALSE.
          endif

C If not all the expected zones read in, loop back, if ncomp read
C in then proceed to check zone names.
          if(icomp.lt.ncomp)then
            goto 46
          else
            goto 142
          endif
        elseif(WORD(1:4).eq.'*cnn')then

C If there were zero zones then the next line might be *cnn.
          goto 23  ! jump to code block that processes *cnn
        else
          call usrmsg('Unknown config tag ',WORD,'W')
        endif
        goto 46
      elseif(icfgv.lt.3)then
        DO 20 ICOMP=1,NCOMP
          CALL STRIPC(IUC,OUTSTR,0,ND,1,'zone code',IER)
          IF(IER.NE.0)goto 1
          K=0
          CALL EGETWI(OUTSTR,K,NCCODE(ICOMP),1,100,'W','zone code',IER)
          IF(IER.NE.0) GOTO 1

          CALL STRIPC(IUC,OUTSTR,0,ND,1,'operation file',IER)
          IF(IER.NE.0)goto 1
          LPROJ(ICOMP)=OUTSTR(1:72)
          CALL STRIPC(IUC,OUTSTR,0,ND,1,'geometry file',IER)
          IF(IER.NE.0)goto 1
          LGEOM(ICOMP)=OUTSTR(1:72)
          CALL STRIPC(IUC,OUTSTR,0,ND,1,'constr file',IER)
          IF(IER.NE.0)goto 1
          LTHRM(ICOMP)=OUTSTR(1:72)
          CALL STRIPC(IUC,OUTSTR,0,ND,1,'utility flag',IER)
          IF(IER.NE.0)goto 1
          K=0
          CALL EGETWI(OUTSTR,K,INDU,0,1,'F','utility flg',IER)
          IF(IER.NE.0) GOTO 1

C Warn user about old utility file.
          if(INDU.EQ.1)then
            call edisp(iuout,'Manual update: edit the entries in the')
            call edisp(iuout,'zone utility file and add them to the')
            call edisp(iuout,'model cfg file via a text editor.')
            msgl2='This is no longer supported. Skipping...'
            call usrmsg('Detected a ANCIENT zone utility file.',
     &        msgl2,'W')
          endif
   20   CONTINUE
      endif

C Geometry file, get or derive the zone name via reading the first
C line. Make temporary use of file unit IUF. 
 142  do 22 ICOMP=1,NCOMP
        zname(ICOMP)='UNKNOWN'
        call FINDFIL(LGEOM(ICOMP),XST)

C Quick read, if version 1.1 or newer then the file must be
C fully read to find the number of surfaces in the zone.
        IF(XST)THEN
          write(currentfile,'(a)') LGEOM(ICOMP)(1:lnblnk(LGEOM(ICOMP)))
          CALL ERPFREE(IUF,ios)
          CALL EFOPSEQ(IUF,LGEOM(ICOMP),1,IER)
          IF(IER.NE.0)goto 1
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'geom 1st',IER)
          if(OUTSTR(1:13).eq.'*Geometry 1.1')then
            K=13
            CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
            write(CTYPE(icomp),'(a)') WORD(1:lnblnk(WORD))
            zname(ICOMP)=' '
            CALL EGETW(OUTSTR,K,WORD,'W','Z name',IFLAG)
            ZN=WORD(1:12)
            call st2name(ZN,zname(ICOMP))
            lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update length of this string.
            CALL ERPFREE(IUF,ios)
            call georead(iuf,LGEOM(ICOMP),ICOMP,1,iuout,ier)
            gversion(icomp) = 1.1   ! set the version number.
            write(currentfile,'(a)') FILE(1:lnblnk(FILE))
          else

C Quick read, get the zone name and the number of surfaces if the
C file version is prior to version 1.1.
            K=0
            IF(ND.EQ.1)THEN
              CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
              write(CTYPE(icomp),'(a)') WORD(1:lnblnk(WORD))
              zname(ICOMP)=' '
              lnzname(ICOMP)=0
              zdesc(ICOMP)=' '
              lnzdesc(ICOMP)=0
            ELSEIF(ND.EQ.2)THEN
              CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
              write(CTYPE(icomp),'(a)') WORD(1:lnblnk(WORD))
              CALL EGETW(OUTSTR,K,WORD,'W','Z name',IFLAG)
              ZN=WORD(1:12)
              call st2name(ZN,zname(ICOMP))
              lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update this string length
              zdesc(ICOMP)=' '
              lnzdesc(ICOMP)=0
            ELSEIF(ND.gt.2)THEN
              CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
              write(CTYPE(icomp),'(a)') WORD(1:lnblnk(WORD))
              CALL EGETW(OUTSTR,K,WORD,'W','Z name',IFLAG)
              ZN=WORD(1:12)
              call st2name(ZN,zname(ICOMP))
              lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update this string length
              call egetrm(outstr,K,phrase,'W','Z description',IER)
              ZDESC(ICOMP)=phrase
              lnzdesc(ICOMP)=lnblnk(phrase)  ! update the length of this string.
            ENDIF
            CALL STRIPC(IUF,OUTSTR,99,ND,1,'NTV NSUR AR',IER)
            K=0
            CALL EGETWI(OUTSTR,K,NTV,4,MTV,'F','no verts',IER)
            CALL EGETWI(OUTSTR,K,NSUR,3,MS,'F','no surfs',IER)
            NZSUR(ICOMP)=NSUR
            NZTV(ICOMP)=NTV
            IF(IFLAG.NE.0)GOTO 1

C Close geometry file and set version number.
            gversion(icomp) = 1.0   ! set the version number.
            CALL ERPFREE(IUF,ISTAT)
            write(currentfile,'(a)') FILE(1:lnblnk(FILE))
          endif
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
          IF(ICOMP.GT.9)WRITE(zname(ICOMP),'(A5,I2)')'Zone-',ICOMP
          lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update the length of this string.
        ENDIF
        if(zdesc(ICOMP)(1:1).EQ.' ')then
          write(zdesc(ICOMP),'(2a)') 
     &      zname(ICOMP)(1:lnzname(ICOMP)),' describes a'
          lnzdesc(ICOMP)=lnblnk(zdesc(ICOMP))  ! update the length of this string.
        endif

C Reporting.
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
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Shading: ',
     &          LSHAD(ICOMP)(1:LNBLNK(LSHAD(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(IHC(ICOMP).EQ.1)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Convect. coeff: ',
     &          LHCCO(ICOMP)(1:LNBLNK(LHCCO(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF

C Include SHOCC zone file name in the report.
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

C Read in the building connectivity information.
      CALL STRIPC(IUC,OUTSTR,0,ND,1,'no of connections',IER)
      IF(IER.NE.0)goto 1

C Jump to 23 if the building type model does no have any zones
C so the subsequent line might be *cnn.
  23  if(OUTSTR(1:4).eq.'*cnn')then
        cnnok=.true.
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','connections tag',IER)
        CALL EGETRM(OUTSTR,K,LCNN,'W','connections file',IER)
        call FINDFIL(LCNN,XST)
        if(XST)then
          write(currentfile,'(a)') LCNN(1:lnblnk(LCNN))
          CALL EFOPSEQ(IUF,LCNN,1,IER)
          IF(IER.NE.0)goto 1
          CALL STRIPC(IUF,OUTSTR,0,ND,1,'connex file header',IER)
          if(OUTSTR(1:12).ne.'*connections')then
            CALL USRMSG(' not a connections file ',LCNN,'W')
            goto 1
          endif
          CALL STRIPC(IUF,OUTSTR,0,ND,1,'connex file date',IER)
          CALL STRIPC(IUF,OUTSTR,0,ND,1,'no of connex',IER)
          K=0
          CALL EGETWI(OUTSTR,K,NCON,3,MCON,'W','no connex',IER)
          IF(IER.NE.0) GOTO 1
        else
          if(NCOMP.eq.0)then
            continue   ! if there are no zones then cnn file does not need to exist
            goto 6     ! close the cfg file
          else
            CALL USRMSG('Connections file not found!',LCNN,'W')
            if(dll)then
              dllsubr='ersys'
              dllmesg='Connections file does not exist!'
              ier=2
              return
            else
              IER=1
              RETURN
            endif
          endif
        endif
      else
        cnnok=.false.
        K=0
        CALL EGETWI(OUTSTR,K,NCON,3,MCON,'W','no connection',IER)
        IF(IER.NE.0) GOTO 1
      endif

C Reporting.
C Generate header for connections. Find a variable for -1.
      IF(ITRC.GE.1.AND.ITRC.LT.4)THEN
        CALL EDISP(iuout,' ')
        WRITE(OUTS,'(a,i4)')' Number of surfaces in the model =',NCON
        CALL EDISP(iuout,OUTS)
      ENDIF

      DO 21 ICON=1,NCON

C Read from either the connections file or configuration file.
        if(cnnok)then
          write(currentfile,'(a)') LCNN(1:lnblnk(LCNN))
          CALL STRIPC(IUF,OUTSTR,0,ND,1,'connections',IER)
        else
          write(currentfile,'(a)') FILE(1:lnblnk(FILE))
          CALL STRIPC(IUC,OUTSTR,0,ND,1,'connections',IER)
        endif
        IF(IER.NE.0)goto 1
        K=0
        CALL EGETWI(OUTSTR,K,IC1(ICON),1,MCOM,'F','conn src zone',IER)
        IF(IER.NE.0) GOTO 1
        CALL EGETWI(OUTSTR,K,IE1(ICON),1,MS,'F','conn src surf',IER)
        IF(IER.NE.0) GOTO 1
        CALL EGETWI(OUTSTR,K,ICT(ICON),-1,7,'F','conn type',IER)
        IF(IER.NE.0) GOTO 1
        CALL EGETWI(OUTSTR,K,IC2(ICON),0,0,'-','conn other zone',IER)
        IF(IER.NE.0) GOTO 1
        CALL EGETWI(OUTSTR,K,IE2(ICON),0,0,'-','conn other surf',IER)
        IF(IER.NE.0) GOTO 1

C If ground connection IC2=0 then IE2 will be the profile.
        IF(ICT(ICON).EQ.4)THEN
          IF(IC2(ICON).EQ.0.AND.IE2(ICON).GT.NGRDP)THEN
            CALL USRMSG(
     &      'Ground connection referenced an unknown monthly profile!',
     &        OUTSTR,'W')
          ENDIF
          IF(IC2(ICON).EQ.-3.AND..NOT.GRND3D)CALL USRMSG(
     &    '3D ground model not available but referenced in',OUTSTR,'W')
        ENDIF

C Range checking.
        IF(ICT(ICON).EQ.0)THEN
          IF(IC2(ICON).NE.0.OR.IE2(ICON).NE.0) CALL USRMSG(
     &      'Ext conn (4th & 5th items) should be 0 in',OUTSTR,'W')
        ENDIF
        IF(ICT(ICON).EQ.3)THEN
          IF(IC2(ICON).LT.1.OR.IC2(ICON).GT.MCOM)CALL USRMSG(
     &      'Int conn other zone (4th item) unknown in',OUTSTR,'W')
          IF(IE2(ICON).LT.1.OR.IE2(ICON).GT.MS)CALL USRMSG(
     &      'Int conn other surf (5th item) unknown in',OUTSTR,'W')
        ENDIF
   21 CONTINUE

C Close connections file and build zone:surface to connections hash array.
      if(cnnok)CALL ERPFREE(IUF,ISTAT)
      do 42 icc = 1, NCON
        IZSTOCN(IC1(icc),IE1(icc))=icc
 42   continue

C With hash known, re-scan geometry files (connection based
C surface attributes will be instanciated during geometry read).
C Warn if izstocn returns zero. Recover zone volume and surface
C derived data. Use georead or egomin depending on version.
      do 242 ICOMP=1,NCOMP
        call FINDFIL(LGEOM(ICOMP),XST)
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(XST)then
          if(newgeo)then

C If MODE='ALL ' we also want to fill obstruction data structures
C but there is a case where a version 1.1 geometry file might
C not yet include obstruction blocks that were in legacy zone
C obstructions files. If this is the case then scan them.
            call georead(iuf,LGEOM(ICOMP),ICOMP,1,iuout,ier)
            if(MODE(1:3).eq.'ALL'.or.MODE(1:3).eq.'all')then
              if(iobs(icomp).eq.1)then
                call FINDFIL(ZOBS(ICOMP),XST)
                if(XST)then
                  CALL ERPFREE(IUF,ISTAT)
                  CALL EGOMST(iuf,ICOMP,ZOBS(ICOMP),0,ITRC,iuout,IER)
                endif
              elseif(iobs(icomp).eq.2)then
                continue
              endif
            endif
          else
	    
C If MODE='ALL ' also scan obstruction blocks from zone
C obstruction file if older model.
            call egomin(iuf,LGEOM(ICOMP),ICOMP,1,0,iuout,ier)
            if(MODE(1:3).eq.'ALL'.or.MODE(1:3).eq.'all')then
              if(iobs(icomp).eq.1)then
                call FINDFIL(ZOBS(ICOMP),XST)
                if(XST)then
                  CALL ERPFREE(IUF,ISTAT)
                  CALL EGOMST(iuf,ICOMP,ZOBS(ICOMP),0,ITRC,iuout,IER)
                endif
              elseif(iobs(icomp).eq.2)then
                continue
              endif
            endif
          endif
          DO 243 IS=1,NZSUR(ICOMP)
            icc=IZSTOCN(icomp,is)
            if(icc.eq.0)then
              write(iuout,*)'zero icc associated with zn ',icomp,
     &          ' & sur',is
              goto 243
            endif
 243      continue

C Call to zgupdate is after connection lists established.
          call zgupdate(0,icomp,ier)
	    
C If MODE='ALL ' also find co-planer surfaces & edges of similar materials. 
          if(MODE(1:3).eq.'ALL'.or.MODE(1:3).eq.'all')then
            call suredgeadj(itrc,'-',icomp,ier)

C Instanciate the vcoord and nzjvn nzvner arrays.
            DO 40 J=1, NZTV(ICOMP)
              VCOORD(ICOMP,J,1)=X(J)
              VCOORD(ICOMP,J,2)=Y(J)
              VCOORD(ICOMP,J,3)=Z(J)
   40       CONTINUE
            DO 50 J=1, nzsur(ICOMP)
              icc=IZSTOCN(icomp,j)
              if(icc.ne.0)then
                NZNVER(icc)=NVER(J)
                N = NVER(J)
                DO 60 K=1,N
                  NZJVN(icc,K)=JVN(J,K)
   60           CONTINUE
              endif
   50       CONTINUE
          endif

C Trace geometry related information. Because not all zones will have
c beeen read in do not include the extended surface `context`.
          if(ITRC.ne.0.and.ITRC.lt.4)then
            call ZINFOREP(iuout,icomp)
            context=.false.
            CALL SURINFO(ICOMP,iuout,context)
            IF(ITRC.GT.1.AND.ITRC.LT.4)CALL VERINFO(iuout)
            CALL INSINFO(ICOMP,iuout)
          endif
        endif
 242  continue

C Reporting. Connection Zone Surface Type Associated data (zone geometry
C will have been scanned in by this point and surface names known.
      IF(ITRC.GE.1.AND.ITRC.LT.4)THEN
        CALL CONXINFO(1,0,CXSTR)
        CALL EDISP(iuout,CXSTR)
        do 41 icc = 1, NCON
          CALL CONXINFO(1,ICC,CXSTR)
          CALL EDISP(iuout,CXSTR)
 41     continue
      ENDIF

C Read in anchors if available. For each anchor, get its name, number
C of links and then the links. If the string is not *Anchor then
C it is probably the index indicating flow.
      write(currentfile,'(a)') FILE(1:lnblnk(FILE))
      CALL STRIPC(IUC,OUTSTR,0,ND,1,'anchors or flow',IER)
      IF(IER.NE.0)goto 1
      if(OUTSTR(1:7).eq.'*Anchor')then
        K=8
        CALL EGETWI(OUTSTR,K,NALOC,0,20,'F','NALOC',IER)
        IF(IER.NE.0) GOTO 1
        do 346 ia=1,NALOC
          CALL STRIPC(IUC,OUTSTR,99,ND,1,'anchor name',IER)
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','anchor name',IER)
          write(ALOCLBL(ia),'(a12)') WORD(1:12)
          if(ND.gt.2)then
            CALL EGETW(OUTSTR,K,WORD,'W','anchor type',IER)
            write(ALOCTYP(ia),'(a4)') WORD(1:4)
          else
            write(ALOCTYP(ia),'(a4)') 'UNKN'
          endif
          CALL EGETWI(OUTSTR,K,IALOC(ia),0,60,'F','anc link',IER)
          IRVA=IALOC(ia)

C Scan next line for links.
          CALL EGETWIA(IUC,IVA,IRVA,0,0,'-','assoc links',IER)
          DO 345 LC=1,IALOC(ia)
            lstanchr(ia,LC)=IVA(LC)
 345      continue
 346    continue
        CALL STRIPC(IUC,OUTSTR,0,ND,1,'anchor end',IER)
      else

C No anchors so this is the flow network index.
        goto 343
      endif

C Check if there is an fluid (1=text, 2=grphic) air flow network available: 0 = no.
      CALL STRIPC(IUC,OUTSTR,0,ND,1,'flow network index',IER)
      IF(IER.NE.0)goto 1
 343  K=0
      CALL EGETWI(OUTSTR,K,IAIRN,0,2,'F','IAIRN',IER)
      IF(IER.NE.0) GOTO 1

C If there is an fluid (=air) flow network available then:
      IF(IAIRN.ge.1)THEN

C Read name of flow network file or graphic network flow file.
        CALL STRIPC(IUC,OUTSTR,0,ND,1,'leakage file',IER)
        LAPROB=OUTSTR(1:72)
        IF(IER.NE.0)goto 1

C Determine energy zone/fluid(=air) flow network node mapping. Flow nodes
C may be referenced by either name or number; to be able to check whether
C node names are valid, first read the fluid flow model file, which
C is temporarily opened on IUF.
        write(currentfile,'(a)') LAPROB(1:lnblnk(LAPROB))
        CALL EFOPSEQ(IUF,LAPROB,1,IER)
        IF(IER.LT.0)then
          outs='Problem opening flow network file...aborting cfg scan.'
          if(dll)then
            dllsubr='mzinpt'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            goto 6
          endif
        endif

C Reporting.
        IF(ITRC.GE.1.AND.ITRC.LT.4)THEN
          CALL EDISP(iuout,' ')
          CALL EDISP(iuout,' A mass flow network has been defined:')
          WRITE(OUTS,9886)LAPROB(1:LNBLNK(LAPROB))
 9886     FORMAT(' Leakage file              : ',a)
          CALL EDISP(iuout,OUTS)
        ENDIF

C Fill valid component type arrays
        CALL MFCDAT

C Read the file header and check for first-line tag. If 4 items
C then an older file so rewind the file and then call emfread.
        CALL STRIPC(IUF,OUTSTR,99,ND,0,'1st line of file',IER)
        if(IER.NE.0)then
          outs='Problem scanning flow network file...aborting cfg scan'
          if(dll)then
            dllsubr='mzinpt'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            goto 6
          endif
        endif
        IF(ND.GE.4)THEN
          REWIND(IUF,ERR=999)
          CALL EMFREAD(IUF,IER)
          CALL ERPFREE(IUF,ISTAT)
          IAIRN = 1
        elseif((ND.eq.1.or.ND.eq.2).and.
     &          OUTSTR(1:18).EQ.'*Graphical_network')then

C Found a graphic network file, scan it (silently) and then convert it
C into network flow common blocks.
          IAIRN = 2
          NWKFLNAM=LAPROB
          CALL ERPFREE(IUF,ISTAT)
          call NETREAD(IUF,'S',IER)
          CALL NETTOFLW(ier)
        endif
        if(IER.NE.0)then
          outs='Problem scanning flow network file...aborting cfg scan'
          if(dll)then
            dllsubr='mzinpt'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            goto 6
          endif
        endif

C Proceed with mfs/zone conct list; read one whole record into a string.
C If there is only one word on the line then it is probably an
C older file. Try reading another line or two to get to list.
        write(currentfile,'(a)') FILE(1:lnblnk(FILE))
        CALL STRIPC(IUC,OUTSTR,99,ND,1,'node-zone list',IER)
        if(ND.eq.1.and.NCOMP.ne.1)then
          CALL STRIPC(IUC,OUTSTR,99,ND,1,'old pressc.db1',IER)
          if(ND.eq.1)then
            CALL STRIPC(IUC,OUTSTR,99,ND,1,'old mass resl',IER)
          endif
        endif
        IF(IER.NE.0)then
          outs='Problem with flow node-zone links in cfg file.'
          if(dll)then
            dllsubr='ersys'
            dllmesg=outs
            ier=2
            return
          else
            dok=.true.
            h(1)='The section of the configuration file that defines'
            h(2)='links between zones and flow network nodes gave a '
            h(3)='problem. The file might be corrupted or truncated. '
            call askok('There appears to be a problem with flow node-',
     &        'zone links in the configuration file. Continue?',
     &        OK,dok,3)
            if(.NOT.OK)RETURN
            ier=0
          endif
        endif

C Then, zone by zone, split this string into seperate names or numbers
C reading more lines if necessary. Substitute call to egetagwpa if/when
C list is written as comma separated phrases (if and when zone and
C flow node names are longer phrases)
        K=0
        isva=ncomp
        inisz=MCOM
        call egetagwsa(outstr,k,IUC,isva,namen,inisz,'W',
     &    'node name or node index',ierv)

C For each zone take namen() and check it against the node
C names in the flow network as well as recognise zero as
C a zone with no flow node.
        do 35 icomp=1,ncomp
          NODID=namen(icomp)(1:12)

C See if what we have is a valid fluid network node name.
          INOD=0
   31     INOD=INOD+1
          IF(NODID(1:12).EQ.NDNAM(INOD)(1:12)) GOTO 32
          IF(INOD.LT.NNOD) GOTO 31
          GOTO 33

C Right, it is a valid fluid network node name.
   32     ICAAS(ICOMP)=INOD
          GOTO 35

C No, it is not. Now tranform what we have into a number.
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
            CALL EDISP(iuout,' Thermal zone/flow node association:')
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

C If a flow network and contaminant file have been defined then call the
C subroutine to fill in contaminant common blocks
      if(IAIRN.GT.0.AND.NOCNTM.EQ.1)then
        CALL CTREAD(iier)
        if(iier.ne.0)then
          outs='There was a problem with scanning contaminants file.'
          call edisp(iuout,outs)
        endif
      endif

C Associations between temporal file and configuration file are
C no longer checked in ersys, instead they are part of the routine
C supplyandcheck.

C If at this point ier remains set at something other than zero then
C warn the user.
      if(ier.ne.0)then
        CALL edisp(iuout,
     &      ' model system configuration scanned - but not cleanly.')
      else
        IF(ITRC.GE.1)CALL edisp(iuout,
     &      ' model system configuration successfully scanned')
      endif

C Read UK NCM specific data
      IF(IRNCMF.EQ.1)CALL RSBEM
      IF(IRNCMD.EQ.1)CALL SBEMPR(IER)

C Building description now complete, begin to read in plant if it
C exists. IFPNF is the unit for the plant network file (same as for
C temporary opening of geometry files etc., iunit1 is
C the plant components db, pltcfg assumes that ifpnf exists and is open. 
 888  continue
      IF(INDCFG.EQ.2.OR.INDCFG.EQ.3)THEN

C Identify plant file; skip if this is '* Plant' record.
        CALL STRIPC(IUC,OUTSTR,0,ND,1,'* plant',IER)
        IF(IER.NE.0)then
          outs='Problem reading * PLANT line'
          if(dll)then
            dllsubr='ersys'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(iuout,outs)
            goto 1
          endif
        endif
        LPNF=OUTSTR(1:72)
        IF(LPNF(1:7).EQ.'* Plant'.OR.LPNF(1:7).EQ.'* PLANT') THEN
          CALL STRIPC(IUC,OUTSTR,0,ND,1,'* plant',IER)
          if(IER.NE.0)then
            call edisp(iuout,'Problem reading * PLANT line')
            goto 1
          endif
          LPNF=OUTSTR(1:72)

C If a plant only model, attempt to read the model name.
C This could be the last line in the file so ier could
C be set to 2.
          IF(INDCFG.EQ.2)then
            CALL STRIPC(IUC,OUTSTR,0,ND,1,'model name',IIER)
            IF(IIER.eq.0)then
              modeltitle=OUTSTR(1:72)
            elseif(IIER.eq.2)then
              modeltitle=OUTSTR(1:72)
              iier=0
            else
              write(modeltitle,'(2a)') 'Systems model ',cfgroot
            endif
          endif
        ENDIF

        IFPNF=IFIL+1
        iunit1=IUF
        write(currentfile,'(a)') LPNF(1:lnblnk(LPNF))
        CALL EFOPSEQ(ifpnf,LPNF,1,IER)
        IF(IER.LT.0)THEN
          IER=1
          CALL edisp(iuout,' problem opening plant network file...')
          RETURN
        ENDIF
        call pltcfg(ifpnf,iunit1,iuout,itrc)
        if(ier1.ne.0)then
          CALL edisp(iuout,' plant network scanned - but not cleanly.')
        else
          IF(ITRC.GE.1)CALL edisp(iuout,
     &        ' plant network successfully scanned')
        endif
        CALL ERPFREE(ifpnf,ISTAT)
        write(currentfile,'(a)') FILE(1:lnblnk(FILE))

C Free plant components database and/or fluid flow model file).
        IF(IUNIT1.NE.0) CALL ERPFREE(IUNIT1,ISTAT)
      endif


C Both building and plant description are now complete
C Close cfg file before exiting.
    6 CALL ERPFREE(IUC,ISTAT)

C Pass information about the current model to the C interface.
      lcfgroot=cfgroot
      lpath=path
      lupath=upath
      limgpth=imgpth
      ldocpth=docpth
      ltmppth=tmppth
      if(browse)then
        ibrowse=1
        call curproject(lcfgroot,lpath,lupath,limgpth,ldocpth,ltmppth,
     &    ibrowse)
      else
        ibrowse=0
        call curproject(lcfgroot,lpath,lupath,limgpth,ldocpth,ltmppth,
     &    ibrowse)
      endif
      RETURN

C Error handling
  1   if(dll)then
        dllsubr='ersys'
        write(dllmesg,'(2a)')'Probelm scanning cfg line: ',OUTSTR(1:40)
        ier=2
        return
      else
        CALL USRMSG('Problem with configuration line:',OUTSTR,'W')
        call edisp(iuout,' Plese check your configuration file.')
        IER=1
        GOTO 6
      endif

  2   msgl2='  '
      CALL USRMSG('Problem associating tdf item with contrl.',msgl2,'W')
      IER=1
      GOTO 6

  99  if(dll)then
        dllsubr='ersys'
        dllmesg='Problem creating cfgroot string from config file name.'
        ier=2
        return
      else
        CALL USRMSG(
     &    'Problem creating cfgroot string from config file name.',
     &     cfgroot,'W')
        IER=1
        GOTO 6
      endif 

C File rewind errors.
  999 if(dll)then
        dllsubr='ersys'
        WRITE(dllmesg,'(2a)') 
     &  ' Error rewinding flow network file ',LAPROB(1:32)
        ier=2
        return
      else
        CALL USRMSG('Error rewinding flow network file:',
     &    LAPROB,'W')
        IER=1
        GOTO 6
      endif 

      END

C *************** SITEINFO 
C SITEINFO provides an English description of the site information in
C common blocks C4, C5, C5R, PREC8 which are filled by reading the
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

C **************** CONXINFO 
C CONXINFO returns an English description in CXSTR of the inter-connection 
C information in common blocks  which are filled by reading the
C system configuration file. If ICF=1 then display with zone and surface names 
C if possible. If ICON=0 then generate a text heading.

      SUBROUTINE CONXINFO(ICF,ICON,CXSTR)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/C24/IZSTOCN(MCOM,MS)
      CHARACTER CXSTR*78,SST1*12,ZST1*12,SST2*12,ZST2*12
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      integer ICM  ! connection for current surface.

      if(ICON.EQ.0.AND.ICF.GT.0)then
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

      SST1=SSNAME(ICON)
      lsn1=lnblnk(SST1)
      ZST1=zname(IC1(ICON))
      lzn1=lnblnk(ZST1)
      IF(ICT(ICON).EQ.-1.AND.ICF.GT.0)THEN
        WRITE(CXSTR,7985)ICON,SST1(1:lsn1),ZST1(1:lzn1)
 7985   FORMAT(I4,1X,a,' in ',a,' not yet defined')
      ELSEIF(ICT(ICON).EQ.0.AND.ICF.GT.0)THEN
        WRITE(CXSTR,8985)ICON,SST1(1:lsn1),ZST1(1:lzn1)
 8985   FORMAT(I4,1X,a,' in ',a,' is External')
      ELSEIF(ICT(ICON).EQ.1.AND.ICF.GT.0)THEN
        if(IC2(ICON).eq.0.and.IE2(ICON).eq.0)then
          WRITE(CXSTR,8984)ICON,SST1(1:lsn1),ZST1(1:lzn1)
 8984     FORMAT(I4,1X,a,' in ',a,' >|< Identical environment ')
        else
          WRITE(CXSTR,8987)ICON,SST1(1:lsn1),
     &      ZST1(1:lzn1),IC2(ICON),IE2(ICON)
 8987     FORMAT(I4,1X,a,' in ',a,' >|< Similar +-',i3,'dC & ',
     &      I4,' W rad')
        endif
      ELSEIF(ICT(ICON).EQ.2.AND.ICF.GT.0)THEN
        WRITE(CXSTR,8983)ICON,SST1(1:lsn1),ZST1(1:lzn1),
     &                   IC2(ICON),IE2(ICON)
 8983   FORMAT(I4,1X,a,' in ',a,' >|< Constant @',I3,' dC &',
     &    I4,' W rad')
      ELSEIF(ICT(ICON).EQ.3.AND.ICF.GT.0)THEN
        ICM=IZSTOCN(IC2(ICON),IE2(ICON))
        SST2=SSNAME(ICM)
        lsn2=lnblnk(SST2)
        ZST2=zname(IC2(ICON))
        lzn2=lnblnk(ZST2)
        WRITE(CXSTR,8982)ICON,SST1(1:lsn1),ZST1(1:lzn1),
     &                        SST2(1:lsn2),ZST2(1:lzn2)
 8982   FORMAT(I4,1X,a,' in ',a,' >|< ',a,' in ',a)
      ELSEIF(ICT(ICON).EQ.4.AND.ICF.GT.0)THEN
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
      ELSEIF(ICT(ICON).EQ.5.AND.ICF.GT.0)THEN
        WRITE(CXSTR,7984)ICON,SST1(1:lsn1),ZST1(1:lzn1)
 7984   FORMAT(I4,1X,a,' in ',a,' is adiabatic')
      ELSEIF(ICT(ICON).EQ.6.AND.ICF.GT.0)THEN

C BASESIMP connection.
        WRITE(CXSTR,7986)ICON,SST1(1:lsn1),ZST1(1:lzn1)
 7986   FORMAT(I4,1X,a,' in ',a,' is BASESIMP fndtn')
      ELSEIF(ICT(ICON).EQ.7.AND.ICF.GT.0)THEN

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

      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      common/anchorb/IALOC(20),lstanchr(20,60)

      character ALOC*12,ALOCLBL*12,ALOCTYP*4,outs*124,type*4,msgl2*48
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

C ******* findwhichdbpath
C findwhichdbpath takes a database file name and checks to
C see if the path is local or absolute or standard database folder.
C Depending on which database (the parameter topic) the
C common block file string and whichdbpath variable is updated.
C topic =  'opt' for optics, 'pdb' for plant component templates
C       =  'prs' for pressure coef, 'sbm' for UK SBEM database,
C       =  'mat' for materials database, 'evn' for profiles database
C       =  'mul' for MLC database, 'msc' for miscellaneous components
      subroutine findwhichdbpath(topic,file,ier)
      implicit none
#include "esprdbfile.h"

      integer lnblnk  ! function definition
      integer ier  ! typd for passed parameter

C Currently works with optical and plant template database.
      character*(*) file
      character topic*3 ! identify which type of database
      integer lndbp   ! for length of standard database path
      integer lnwkg   ! for length of working file name
      logical unixok  ! to check for database path file separators

C Set OS and check that standarddbpath has been set.
      lndbp=lnblnk(standarddbpath)
      lnwkg=lnblnk(file)
      if(lndbp.lt.1.or.lnwkg.lt.1)then
        ier=2
        return
      endif
      call isunix(unixok)
      
      if(unixok)then
        if(file(1:7).eq.'../dbs/')then
          if(topic(1:3).eq.'opt')then
            ipathoptdb=1  ! local folder for optical database
            write(LOPTDB,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=1  ! local folder for plant database
            write(LPCDB,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'prs')then
            ipathapres=1  ! local folder for pressure database
            write(lapres,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'sbm')then
            ipathsbem=1  ! local folder for UK SBEM database
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
            ipathmsc=1  ! local folder for miscel comp database
            write(MCMPDBFL,'(a)') file(1:lnwkg) ! assign directly
          endif
        elseif(file(1:lndbp).eq.standarddbpath(1:lndbp))then

C The initial part of the path matches so only save the file name w/o path.
          if(topic(1:3).eq.'opt')then
            ipathoptdb=2
            write(LOPTDB,'(a)') file(lndbp+2:lnwkg)  ! without path
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=2
            write(LPCDB,'(a)')  file(lndbp+2:lnwkg)  ! without path
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
          endif
        else
          if(topic(1:3).eq.'opt')then
            ipathoptdb=0
            write(LOPTDB,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=0
            write(LPCDB,'(a)')  file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'prs')then
            ipathapres=0
            write(lapres,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'sbm')then
            ipathsbem=1  ! local folder for UK SBEM database
            write(lsbem,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'prs')then
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
          elseif(topic(1:3).eq.'prs')then
            ipathapres=1  ! local folder for pressure database
            write(lapres,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'sbm')then
            ipathsbem=1  ! local folder for UK SBEM database
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
            ipathmsc=1  ! local folder for miscel comp database
            write(MCMPDBFL,'(a)') file(1:lnwkg) ! assign directly
          endif
        elseif(file(1:lndbp).eq.standarddbpath(1:lndbp))then
          if(topic(1:3).eq.'opt')then
            ipathoptdb=2
            write(LOPTDB,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=2
            write(LPCDB,'(a)')  file(lndbp+2:lnwkg)  ! without path
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
          endif
        else
          if(topic(1:3).eq.'opt')then
            ipathoptdb=0
            write(LOPTDB,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=0
            write(LPCDB,'(a)')  file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'prs')then
            ipathapres=0
            write(lapres,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'sbm')then
            ipathsbem=1  ! local folder for UK SBEM database
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
          endif
        endif
      endif

C Debug.
C      write(6,*) file
C      if(topic(1:3).eq.'opt')then
C        write(6,*) loptdb
C        write(6,*) 'findwhichdbpath ipath is ',ipathoptdb
C      elseif(topic(1:3).eq.'pdb')then
C        write(6,*) LPCDB
C        write(6,*) 'findwhichdbpath ipath is ',ipathpcdb
C      elseif(topic(1:3).eq.'prs')then
C        write(6,*) lapres
C        write(6,*) 'findwhichdbpath ipath is ',ipathapres
C      elseif(topic(1:3).eq.'sbm')then
C        write(6,*) lsbem
C        write(6,*) 'findwhichdbpath ipath is ',ipathsbem
C      elseif(topic(1:3).eq.'mat')then
C        write(6,*) LFMAT
C        write(6,*) 'findwhichdbpath ipath is ',ipathmat
C      elseif(topic(1:3).eq.'evn')then
C        write(6,*) LPRFDB
C        write(6,*) 'findwhichdbpath ipath is ',ipathprodb
C      elseif(topic(1:3).eq.'mul')then
C        write(6,*) LFMUL
C        write(6,*) 'findwhichdbpath ipath is ',ipathmul
C      elseif(topic(1:3).eq.'msc')then
C        write(6,*) MCMPDBFL
C        write(6,*) 'findwhichdbpath ipath is ',ipathmsc
C      endif

      return
      end
