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
C  ERSYSG :  Reads a commented system configuration file (with generic db).
C  SITEINFO: English description of the site information (cfg file).
C  CONXINFO: returns an English description of the inter-connections 
C            in the configuration file. 
C  HZSTOCON: Scans the current connections list and builds array
C            IZSTOCN(MCOMP,MS) holding the connection index.


C ******************** ERSYSG 
C ERSYSG reads a system configuration file which defines the building
C and/or plant definition for simulation with or without comments
C and with several levels of verbosity in reporting.  
C If MODE='NONE' ERSYS fills the common block with file names only. 
C IF MODE='ALL ' then all geom files scanned.

C Common block variables are:
C INDCFG  - is the configuration file index where: 0= registration, 
C           1= building only, 2= plant only, 3= building and plant.

C ICFGV   - version of the model configuration file.

C XLAT,   - Site latitude and the longitudinal difference between the 
C XLON      site and the reference time zone (east +ve).

C IXPOS   - Site exposure index :
C           1= city centre, normal case; 2= urban site, normal case
C           3= rural site, normal case;
C           4= city centre, equal sky, ground & building view factors
C           5= city centre, building below mean height of surrounding
C              buildings
C           6= rural site, isolated; 7= totally enclosed building
C           8= user defined

C GREF    - External ground reflectivity.

C LSNAM   - is the name of the entire system.

C NCOMP   - number of zones comprising the model to be simulated.
C NCCODE  - zone code (for eventual database reference).

C LPROJ   - zone scheduling (operation file).
C LGEOM   - zone geometry file.
C LTHRM   - zone construction file.
C INDUTL  - Zone utilities file index: 0= none 1= zone utility file supplied
C LUTIL   - Zone utilities file name (legacy file).

C NCON    - total number of intra-zone connections.
C IC1 &   - zone and surface numbers defining the start
C IE1       point of each connection.

C ICT     - connection type where:
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
C ICAAS   - fluid (= air) flow network node associated with each building 
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

      SUBROUTINE ERSYSG(FILE,IUC,IUF,MODE,ITRC,IER)
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"
#include "cfd.h"
#include "uncertainty.h"
#include "gnetwk.h"
#include "dhw_common.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure databases)
C LOPTDB,IOPTDB (for optical database)
C LPCDB/IPCDB (for plant template database)
C LPRFDB,IPRODB (for event profile database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
C ICLIM,LCLIM (for climate file)
C default file names for databases

#include "espriou.h"
C espriou.h provides currentfile.

      PARAMETER (MSTMC=1)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/POPHELP/H(60)

C IFIL base file unit number (set in MAIN of each application).
      COMMON/FILEP/IFIL
      COMMON/ER1/IER1
      COMMON/OUTIN/IUOUT,IUIN
C      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)

C LCNN (72 char) model connections file name.
      common/C3F/LCNN

C xlat,   - Site latitude and the longitudinal difference between the 
C xlon      site and the reference time zone (east +ve).
      COMMON/C4/XLAT,XLON
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM
      CHARACTER*72 SNFNAM

C SKYR viewfactor from external surfaces to the sky
C GRDR viewfactor from external surfaces to the ground
C BLDR viewfactor from external surfaces to other buildings & obstructions
      COMMON/C5R/SKYR,GRDR,BLDR
      COMMON/C6/INDCFG
      COMMON/C14/UGRDTP(12,MGRDP),NGRDP

C NZSUR is the number of surfaces in each zone.
C NZTV is the number of vertices in each zone.
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)

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
      COMMON/PREC2/VOL(MCOM)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/PREC8/SLAT,SLON
      COMMON/G0/CTYPE

      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

C ZMFN1 (124 char) holds first line of flow node->zone links
C ZMFN2 (124 char) holds second line of flow node->zone links
      COMMON/AFNZN/zmfn1,zmfn2
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKFLNAM,NWKTYPSTR(MNWKTYP)
      common/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

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
       COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT)

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

C Images.
      COMMON/IMAGF/imgfmt(10),imgfoc(10),limgfil(10),noimg,iton

C Initial view.
      common/initv/initvt,EYEMI(3),VIEWMI(3),ANGI

C Where default db list is kept.
C DFDBLBL (20 char) label for list of default databases.
C DEFDBFL (72 char) file name for list of default databases.
      common/defdb/dfdblbl,defdbfl

C Special materials & electrical network files.
      common/spmfxst/ispmxist,spflnam
      common/entfile/entflnam, ientxist

C Misc. components database
      COMMON/MCMPDBNAM/MCMPDBFL

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
      COMMON/GT/GTNAME

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

C IPV description.
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
C igshpflag=1 indicates GSHP models are active; 0 indicates no GSHP models.
      common/gshpinfo/igshp
      common/gshpfile/gshpfile

C GCEP (begin)
C Flag indicating presence of GCEP models:
C igcepflag=1 indicates GCEP models are active; 0 indicates no GCEP models.
      common/gcepinfo/igcep
      common/gcepfile/gcepfile

C Bidirectional raw optical data file.
      COMMON/BIDIR/IFLAGBI,INTVALBI,NSTMCFL(MCON)
      COMMON/BIDIRFL/bidirfile,bidirname(MSTMC)

C Calendar commons.
      common/calena/calename,calentag(10),calendayname(10)
      common/caleni/nbdaytype,nbcaldays(10),icalender(365)

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

      LOGICAL BLDG3D,ZONE3D,MSTROK,MSTRZN,dll,browse,context
      LOGICAL INTHPS,INTHPZ,IGR1D,GRND3D
      LOGICAL XST,OK,DOK,IGDCVS,IGDCNC,IGDNDC,IGDTAQ,cnnok
      LOGICAL OK3DCVS,OK3DCNC,OK3DNDC,OK3DTAQ
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDNDD,LGDTAQ,LGRD1D,LNLTHP
      CHARACTER OUTSTR*124,OUTS*124,text*124,t117*117,h*72
      CHARACTER*72 FILE,LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,LCFD,LCNN
      CHARACTER*72 LAPROB,LPNF,LTDF,LTDFA
      character LPRJLG*72
      character*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,LMOIST,LGrdSt
      CHARACTER WORD*20,NODID*12,MODE*4,CXSTR*78,CTYPE*3,ZN*12
      character imgfmt*4,imgfoc*4,limgfil*72,zname*12,zdesc*64
      CHARACTER DBTAG*12,DBTASK*12,DBZN*15,phrase*64
      character zmfn1*124,zmfn2*124,WORD2*20,msgl2*48
      character lradcf*72
      CHARACTER CTLDOC*248,LCTLF*72,LOCUTIL*72
      CHARACTER spflnam*72,entflnam*72,MCMPDBFL*72,lipvdatf*72
      character dfdblbl*20,defdbfl*72,GTGEOM*72,GTNAME*15
      CHARACTER ectime*24,wkd1*10,wkd2*10,naco*40,type*4
      character*10 wkday(7)
      character LUALF*72,LCNG*15,LLOC*15,cfgroot*24,LCFGF*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      CHARACTER dmdsdesc*248,bdmds*72
      CHARACTER NDNAM*12,ALOC*12,ALOCLBL*12,ALOCTYP*4
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15,SSPARENT*12
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
      character HDR*12

      DIMENSION GVA(12),IVA(60),namez(mcom),namen(mcom)
      dimension IRVC(mcon),IRCC(365)
      data wkday/'Monday', 'Tuesday','Wednesday', 'Thursday',
     &           'Friday','Saturday', 'Sunday'/

C Check if running in dll mode.
      call isadll(dll)

C Scan the defaults file and then set file common blocks.
C Make temporary use of file unit IUF.
C Note: escdef must come after scan of .esprc file.

C Patch for dbm use. Scan for default generic databases and the
C set gendbfn before reading model.
      defdbfl='./gdefaults'
      call escgdef(dfdblbl,defdbfl,IUF,IER)
      do 43 i=1,MGDB
        gendbfn(i)=dgendbfn(i)
  43  continue
      LCLIM=DCLIM


C Clear the model common blocks.
      call clrprb

      LUALF='UNKNOWN'
      MCMPDBFL='UNKNOWN'
      bidirfile='UNKNOWN'

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

C Assume older version of configuration file.
      icfgv=1

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
C via tags.
      CALL STRIPC(IUC,OUTSTR,99,ND,1,'config index',IER)
      IF(IER.NE.0)goto 1
      if(OUTSTR(1:17).eq.'* CONFIGURATION3.')then
        if(OUTSTR(1:18).eq.'* CONFIGURATION3.0')icfgv=3
        if(OUTSTR(1:18).eq.'* CONFIGURATION3.1')icfgv=4
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
        icfgv=2
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
      IF(ITRC.GE.1)THEN
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
      CALL EGETWR(OUTSTR,K,XLAT,-90.,90.,'W','Latitude',IER)
      SLAT=XLAT
      CALL EGETWR(OUTSTR,K,XLON,-15.,15.,'W','Longitude dif',IER)
      SLON=XLON
      IF(IER.NE.0) GOTO 1

C Get site exposure and ground reflectance.
      CALL STRIPC(IUC,OUTSTR,0,ND,1,'ground reflectance',IER)
      IF(IER.NE.0)goto 1
      K=0
      CALL EGETWI(OUTSTR,K,IXPOS,1,8,'W','Site exposure',IER)
      CALL EGETWR(OUTSTR,K,GREF,0.,1.,'W','Ground reflect',IER)
      IF(IER.NE.0) GOTO 1

C Index of exposure has user defined data associated with it.
      IF(IXPOS.EQ.8) THEN
        CALL STRIPC(IUC,OUTSTR,3,ND,1,'user defined site exposure',IER)
        IF(IER.NE.0)goto 1
        K=0
        CALL EGETWR(OUTSTR,K,SKYR,0.,1.,'W','Sky expos',IER)
        CALL EGETWR(OUTSTR,K,GRDR,0.,1.,'W','Ground expos',IER)
        CALL EGETWR(OUTSTR,K,BLDR,0.,1.,'W','Building expos',IER)
        IF(IER.NE.0) GOTO 1

C Range checking.
        IF(ABS(SKYR+GRDR+BLDR-1.).GT..001)CALL USRMSG(
     &    ' Total exposure out of range in',OUTSTR,'W')
      ENDIF

C Reporting.
      IF(ITRC.GE.1)CALL SITEINFO(iuout)

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

C Read databases until '* PROJ LOG' found.
  44  continue
      CALL STRIPC(IUC,OUTSTR,99,ND,1,'db names',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','db names',IFLAG)
      if(WORD(1:4).eq.'*mlc'.or.WORD(1:4).eq.'*cns')then
        CALL EGETRM(OUTSTR,K,gendbfn(1),'W','construction db',IER)
      elseif(WORD(1:4).eq.'*prm'.or.WORD(1:4).eq.'*mat')then
        CALL EGETRM(OUTSTR,K,gendbfn(2),'W','material db',IER)
      elseif(WORD(1:4).eq.'*opt')then
        CALL EGETRM(OUTSTR,K,gendbfn(3),'W','optical db',IER)
      elseif(WORD(1:4).eq.'*hyg')then
        continue
      elseif(WORD(1:4).eq.'*aco')then
        CALL EGETRM(OUTSTR,K,gendbfn(5),'W','acoustic db',IER)
      elseif(WORD(1:4).eq.'*col')then
        CALL EGETRM(OUTSTR,K,gendbfn(6),'W','colour db',IER)
      elseif(WORD(1:5).eq.'*mico')then
        CALL EGETRM(OUTSTR,K,gendbfn(7),'W','micotoxin db',IER)
      elseif(WORD(1:4).eq.'*env')then
        CALL EGETRM(OUTSTR,K,gendbfn(8),'W','env impt db',IER)
      elseif(WORD(1:4).eq.'*trn')then
        CALL EGETRM(OUTSTR,K,gendbfn(9),'W','transport db',IER)
      elseif(WORD(1:4).eq.'*prs')then
        CALL EGETRM(OUTSTR,K,gendbfn(10),'W','pressure db',IER)
      elseif(WORD(1:4).eq.'*pro'.or.WORD(1:4).eq.'*evn')then
        CALL EGETRM(OUTSTR,K,gendbfn(11),'W','profiles db',IER)
      elseif(WORD(1:4).eq.'*pdb')then
        CALL EGETRM(OUTSTR,K,gendbfn(12),'W','plant comp db',IER)
      elseif(WORD(1:4).eq.'*cmp')then
        CALL EGETRM(OUTSTR,K,gendbfn(13),'W','components db',IER)
      elseif(WORD(1:4).eq.'*clm')then
        CALL EGETRM(OUTSTR,K,LCLIM,'W','climate db',IER)
      elseif(WORD(1:4).eq.'*ctl')then
        CALL EGETRM(OUTSTR,K,LCTLF,'W','config control',IER)
      elseif(WORD(1:5).eq.'*hvac')then

C hvac file, test and see if it exists.
        CALL EGETRM(OUTSTR,K,hvacfile,'W','config control',IER)
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
        CALL EGETRM(OUTSTR,K,LAIM2,'W','AIM-2 infil data',IER)
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
      elseif(WORD(1:4).eq.'*ctm')then

C Contaminant model 
        CALL EGETRM(OUTSTR,K,TEXT,'F','contaminant model',IER)
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
        IF(ITRC.GE.1)THEN
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
        CALL EGETRM(OUTSTR,K,LFClds,'W','Fuel cell elec loads data',IER)
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
        CALL EGETRM(OUTSTR,K,sDHW_InputFileName,'W','DHW data',IER)
C This routine puts the file name into the DHW Modules Variable 
        XST=.FALSE.
        call FINDFIL(sDHW_InputFileName,XST)
        if(XST)then
          iDHW_Flag=1
C This routine set the flag into the DHW Modules Variable iDHW_Flag 
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
        CALL EGETRM(OUTSTR,K,LMech,'W','Mechanical Ventilation data',
     & IER )
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

C GSHP
c Test to ensure that .gshp file exists.
        CALL EGETRM(OUTSTR,K,gshpfile,'W','gshp input',IER )
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
C GCEP (begin)
c Test to ensure that .gcep file exists.
        CALL EGETRM(OUTSTR,K,gcepfile,'W','gcep input',IER )
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
C GCEP (end)

      elseif(WORD(1:4).eq.'*vew')then
        CALL EGETWR(OUTSTR,K,EYEMI(1),0.,0.,'-','X cord',IER)
        CALL EGETWR(OUTSTR,K,EYEMI(2),0.,0.,'-','Y cord',IER)
        CALL EGETWR(OUTSTR,K,EYEMI(3),0.,0.,'-','Z cord',IER)
        CALL EGETWR(OUTSTR,K,VIEWMI(1),0.,0.,'-','X cord',IER)
        CALL EGETWR(OUTSTR,K,VIEWMI(2),0.,0.,'-','Y cord',IER)
        CALL EGETWR(OUTSTR,K,VIEWMI(3),0.,0.,'-','Z cord',IER)
        CALL EGETWR(OUTSTR,K,ANGI,0.,0.,'-','Z cord',IER)
        initvt=1
      elseif(WORD(1:9).eq.'*calename')then

C There is a calendar of day types defined read its name.
        CALL EGETRM(OUTSTR,K,calename,'W','calendar name',IER)
        nbdaytype=0
 443    CALL STRIPC(IUC,OUTSTR,0,ND,1,'calendar data',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','calendar tag',IFLAG)
        if(WORD(1:9).eq.'*calentag')then
          nbdaytype=nbdaytype+1
          CALL EGETP(OUTSTR,K,calentag(nbdaytype),'W','cal tag',IER)
          CALL EGETP(OUTSTR,K,calendayname(nbdaytype),'W',
     &      'cal day name',IER)
          CALL EGETWI(OUTSTR,K,nbcaldays(nbdaytype),1,300,'W',
     &      'days assoc with day type',IER)
          goto 443
        elseif(WORD(1:5).eq.'*list')then
          CALL EGETWIA(IUC,IRCC,365,0,10,'W','calendar day types',IER)
          DO 118 KV=1,365
            icalender(KV)=IRCC(KV)
  118     CONTINUE
          goto 443
        elseif(WORD(1:9).eq.'*end_list')then
          continue
        endif
      elseif(WORD(1:4).eq.'*ual')then
        CALL EGETRM(OUTSTR,K,LUALF,'W','uncertainty lib',IER)
      elseif(WORD(1:8).eq.'*weekend')then
        CALL EGETWI(OUTSTR,K,idwe1,1,7,'W','weekend day',IER)
        CALL EGETWI(OUTSTR,K,idwe2,1,7,'W','weekend day',IER)
        wkd1=wkday(idwe1)
        wkd2=wkday(idwe2)
      elseif(WORD(1:5).eq.'*year')then
        CALL EGETWI(OUTSTR,K,IYEAR,1900,2020,'W','year',IER)
      elseif(WORD(1:4).eq.'*img')then

C An image file.
        noimg=noimg+1
        CALL EGETW(OUTSTR,K,WORD2,'W','img format',IFLAG)
        write(imgfmt(noimg),'(a4)') WORD2(1:4)
        CALL EGETW(OUTSTR,K,WORD2,'W','focus',IFLAG)
        write(imgfoc(noimg),'(a4)') WORD2(1:4)
        CALL EGETRM(OUTSTR,K,limgfil(noimg),'W','image file',IER)
      elseif(WORD(1:6).eq.'*pecnv')then

C System level primary energy conversion data, set flag.
        ipconv=1
        CALL EGETWR(OUTSTR,K,pcnvht,0.,0.,'-','heat p cnv',IER)
        CALL EGETWR(OUTSTR,K,pcnvcl,0.,0.,'-','cool p cnv',IER)
        CALL EGETWR(OUTSTR,K,pcnvlt,0.,0.,'-','light p cnv',IER)
        CALL EGETWR(OUTSTR,K,pcnvfn,0.,0.,'-','fan p cnv',IER)
        CALL EGETWR(OUTSTR,K,pcnvsp,0.,0.,'-','sml p cnv',IER)
        CALL EGETWR(OUTSTR,K,pcnvhw,0.,0.,'-','hw p cnv',IER)
      elseif(WORD(1:5).eq.'*ecnv')then

C Effeciencies data line currently unused so skip.
        continue
      elseif(WORD(1:7).eq.'*htemis')then
        CALL EGETWR(OUTSTR,K,phtco2,0.,0.,'-','ht co2',IER)
        CALL EGETWR(OUTSTR,K,phtnox,0.,0.,'-','ht nox',IER)
        CALL EGETWR(OUTSTR,K,phtsox,0.,0.,'-','ht sox',IER)
      elseif(WORD(1:7).eq.'*clemis')then
        CALL EGETWR(OUTSTR,K,pclco2,0.,0.,'-','cl co2',IER)
        CALL EGETWR(OUTSTR,K,pclnox,0.,0.,'-','cl nox',IER)
        CALL EGETWR(OUTSTR,K,pclsox,0.,0.,'-','cl sox',IER)
      elseif(WORD(1:7).eq.'*ltemis')then
        CALL EGETWR(OUTSTR,K,pltco2,0.,0.,'-','lt co2',IER)
        CALL EGETWR(OUTSTR,K,pltnox,0.,0.,'-','lt nox',IER)
        CALL EGETWR(OUTSTR,K,pltsox,0.,0.,'-','lt sox',IER)
      elseif(WORD(1:7).eq.'*fnemis')then
        CALL EGETWR(OUTSTR,K,pfnco2,0.,0.,'-','fan co2',IER)
        CALL EGETWR(OUTSTR,K,pfnnox,0.,0.,'-','fan nox',IER)
        CALL EGETWR(OUTSTR,K,pfnsox,0.,0.,'-','fan sox',IER)
      elseif(WORD(1:7).eq.'*spemis')then
        CALL EGETWR(OUTSTR,K,pspco2,0.,0.,'-','sml co2',IER)
        CALL EGETWR(OUTSTR,K,pspnox,0.,0.,'-','sml nox',IER)
        CALL EGETWR(OUTSTR,K,pspsox,0.,0.,'-','sml sox',IER)
      elseif(WORD(1:7).eq.'*hwemis')then
        CALL EGETWR(OUTSTR,K,phwco2,0.,0.,'-','hw co2',IER)
        CALL EGETWR(OUTSTR,K,phwnox,0.,0.,'-','hw nox',IER)
        CALL EGETWR(OUTSTR,K,phwsox,0.,0.,'-','hw sox',IER)
      elseif(WORD(1:4).eq.'*ipv')then

C IPV data file.
        CALL EGETRM(OUTSTR,K,lipvdatf,'W','ipv file',IER)
      elseif(WORD(1:5).eq.'*dmds')then

C Building fans/pumps/DHW demands file.
        CALL EGETRM(OUTSTR,K,bdmds,'W','bdmds file',IER)
      elseif(WORD(1:7).eq.'*cpcalc')then

C CPCALC global information.
        CALL EGETWR(OUTSTR,K,ble,0.,0.,'-','cpccfg ble',IER)
        CALL EGETWR(OUTSTR,K,bwi,0.,0.,'-','cpccfg bwi',IER)
        CALL EGETWR(OUTSTR,K,bhi,0.,0.,'-','cpccfg bhi',IER)
        CALL EGETWR(OUTSTR,K,blox,0.,0.,'-','cpccfg blox',IER)
        CALL EGETWR(OUTSTR,K,bloy,0.,0.,'-','cpccfg bloy',IER)
        CALL EGETWR(OUTSTR,K,bloz,0.,0.,'-','cpccfg bloz',IER)
        CALL EGETWR(OUTSTR,K,orient,0.,0.,'-','cpccfg orient',IER)
        CALL EGETWI(OUTSTR,K,irt,0,0,'-','cpccfg irt',IER)
        CALL EGETWR(OUTSTR,K,ra,0.,0.,'-','cpccfg ra',IER)
        CALL EGETWR(OUTSTR,K,sbh,0.,0.,'-','cpccfg sbh',IER)
        CALL EGETWR(OUTSTR,K,pad,0.,0.,'-','cpccfg pad',IER)
        CALL EGETWR(OUTSTR,K,wvpe,0.,0.,'-','cpccfg wvpe',IER)
        icpcon=1
      elseif(WORD(1:5).eq.'*rif ')then

C Radiance scene file.
        CALL EGETRM(OUTSTR,K,lradcf,'W','radiance scene file',IER)
      elseif(WORD(1:4).eq.'*nlt')then

C Non-linear thermophysical properties configuration file.
        INTHPS=.TRUE.
        CALL EGETRM(OUTSTR,K,LNLTHP,'W','non-linear prop',IER)
      elseif(WORD(1:4).eq.'*g1d')then

C Building 1D node distribution.
        IGR1D=.TRUE.
        CALL EGETRM(OUTSTR,K,LGRD1D,'W','bldng 1D gridding',IER)
      elseif(WORD(1:6).eq.'*biopt')then

C Bi-directional optical data. Read how many data to read followed
C by the raw-datafile name and then scan for
C a connection-based list on the next line(s).
        ncn=0
        CALL EGETWI(OUTSTR,K,ncn,0,0,'-','nb items/connections',IER)
        CALL EGETRM(OUTSTR,K,bidirfile,'W','bioptical file',IER)
        IF(NCN.GT.0) THEN
          CALL EGETWIA(IUC,IRVC,NCN,0,1,'W','bi-optical list',IER)
          DO 114 KV=1,NCN
            NSTMCFL(KV)=IRVC(KV)
  114     CONTINUE
        endif
      elseif(WORD(1:4).eq.'*spf')then

C Special materials and electrical bus description files.
        CALL EGETRM(OUTSTR,K,spflnam,'W','special mats',IER)
        ispmxist=1
      elseif(WORD(1:4).eq.'*pnt')then
        CALL EGETRM(OUTSTR,K,entflnam,'W','power netwk',IER)
        ientxist=2
      elseif(WORD(1:4).eq.'*tdf')then

C If marked for Temporal file get the name and if it exists
C scan first line/record and set ITDFLG appropriately.
C If itdflg non-zero read any association included in the configuration
C file. Save detailed checking of the temporal file to the
C context code in the project manager or the input code of
C the simulator or the results analyis module.
        CALL EGETRM(OUTSTR,K,LTDFA,'W','TDF file name',IER)

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
          WRITE(OUTS,'(A,I2,7A)') ' Tdf item:',M,':',DBTAG(M),
     &      ' of type: ',DBTASK(M),' associated with ',DBZN(M),'.'
          IF(ITRC.GE.1)CALL EDISP(iuout,OUTS)
        GOTO 59
   58   CONTINUE
        endif
        ITEMSTD=M
        CALL ERPFREE(IUTDF,ISTAT)
      elseif(WORD(1:4).eq.'*sps')then

C Check for simulation setup data and save level.
        CALL EGETWI(OUTSTR,K,nsset,1,5,'W','no. of sets',IER)
        CALL EGETWI(OUTSTR,K,isstup,0,300,'W','startup',IER)
        CALL EGETWI(OUTSTR,K,isbnstep,1,60,'W','zone_ts',IER)
        CALL EGETWI(OUTSTR,K,ispnstep,0,100,'W','plant_ts',IER)
        CALL EGETWI(OUTSTR,K,issave,0,5,'W','save_lv',IER)

C If 6 items on line also read whether to save only 1stph.
        if(ND.eq.6)then
          CALL EGETWI(OUTSTR,K,isavgh,0,1,'W','save_hourly',IER)
        else
          isavgh=0
        endif
        do 333 iss=1,nsset
          CALL STRIPC(IUC,OUTSTR,0,ND,1,'sim set',IER)
          K=0
          CALL EGETWI(OUTSTR,K,isstday(iss),1,31,'W','start',IER)
          CALL EGETWI(OUTSTR,K,isstmon(iss),1,12,'W','start',IER)
          CALL EGETWI(OUTSTR,K,isfnday(iss),1,31,'W','end',IER)
          CALL EGETWI(OUTSTR,K,isfnmon(iss),1,12,'W','end',IER)
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
      elseif(WORD(1:8).eq.'*end_sps')then
        goto 44
      elseif(WORD(1:7).eq.'*intipv')then
        CALL EGETRM(OUTSTR,K,sipvres,'W','ipv report',IER)
        goto 44
      elseif(WORD(1:7).eq.'*annipv')then

C Annual IPV report depreciated in favour of sipvres.
        continue
        goto 44
      elseif(outstr(1:10).eq.'* PROJ LOG')then

C Project log file, read and then go on to fabric/plant description.
        CALL STRIPC(IUC,OUTSTR,0,ND,1,'project log file',IER)
        LPRJLG=OUTSTR(1:LNBLNK(OUTSTR))
        goto 45
      else
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
      endif

C Loop back for next database file.
      goto 44

C If building or building & plant then read in the building info.
   45 IF(INDCFG.EQ.2)goto 888

C At this point there may be ground information, if abscent then
C this is the `* Building` section.
      CALL STRIPC(IUC,OUTSTR,0,ND,1,'model name',IER)
      IF(IER.NE.0)goto 1
      if(OUTSTR(1:8).eq.'* Ground'.or.OUTSTR(1:8).eq.'* GROUND')then
 344    CALL STRIPC(IUC,OUTSTR,0,ND,1,'ground info',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','ground tags',IFLAG)

C External longwave radiation (begin)
C if gtmp tag present in cfg file, get profile.
        IF(WORD(1:5).EQ.'*gtmp')THEN
            CALL EGETWRA(IUC,getVal,12,-9.0,99.0,'W','Ex lg rad',IER)
            iExtLgRadFlag = 1
C Store profile in array.
            DO 27 jMon=1,12
              eGrdTp(jMon)=getVal(jMon)
  27        CONTINUE
C External Longwave Radiation (end)

C 3D ground control volumes file.
        elseif(WORD(1:4).eq.'*cvs')then
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
            IF(ITRC.GE.1)THEN
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
        CALL STRIPC(IUC,OUTSTR,0,ND,1,'building name',IER)
        IF(IER.NE.0)goto 1
        LSNAM=OUTSTR(1:72)
      else
        LSNAM=OUTSTR(1:72)
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
      CALL EGETWI(OUTSTR,K,NCOMP,1,MCOM,'W','zones',IER)
      IF(IER.NE.0) GOTO 1

C Reporting.
      WRITE(OUTS,'(2a)')' The system name is ',LSNAM(1:LNBLNK(LSNAM))
      if(MMOD.ne.8)CALL EDISP(iuout,OUTS)
      IF(ITRC.GE.1)THEN
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
          indutl(icomp)=1
        elseif(WORD(1:4).eq.'*obs')then
          iobs(icomp)=1
          ZOBS(icomp)=text(1:72)
        elseif(WORD(1:4).eq.'*tmc')then
          itw(icomp)=1
          LTWIN(icomp)=text(1:72)
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
        elseif(WORD(1:4).eq.'*bsm')then
C BASESIMP.
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
          CALL EGETWI(OUTSTR,K,INDUTL(ICOMP),0,1,'F','utility flg',IER)
          IF(IER.NE.0) GOTO 1

C Read utility file name and scan files into common.
          LUTIL(ICOMP)=' '
          if(INDUTL(ICOMP).EQ.1)then
            CALL STRIPC(IUC,OUTSTR,0,ND,1,'utility file',IER)
            IF(IER.NE.0)goto 1
            LOCUTIL=OUTSTR(1:72)
            call FINDFIL(LOCUTIL,XST)
            if(XST)then
              CALL ERUTIL(IUF,LOCUTIL,ICOMP,IER)
            else
              msgl2='  '
              CALL USRMSG(' utility file not found ',msgl2,'W')
            endif
            write(currentfile,'(a)') FILE(1:lnblnk(FILE))
          endif
   20   CONTINUE
      endif

C Geometry file, get or derive the zone name via reading the first
C line. Make temporary use of file unit IUF. 
 142  do 22 ICOMP=1,NCOMP
        zname(ICOMP)='UNKNOWN'
        call FINDFIL(LGEOM(ICOMP),XST)

C Quick read, get the zone name and the number of surfaces.
        IF(XST)THEN
          write(currentfile,'(a)') LGEOM(ICOMP)(1:lnblnk(LGEOM(ICOMP)))
          CALL EFOPSEQ(IUF,LGEOM(ICOMP),1,IER)
          IF(IER.NE.0)goto 1
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'geom 1st',IER)
          K=0
          IF(ND.EQ.1)THEN
            CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
            CTYPE=WORD(1:3)
            zname(ICOMP)=' '
            zdesc(ICOMP)=' '
          ELSEIF(ND.EQ.2)THEN
            CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
            CTYPE=WORD(1:3)
            CALL EGETW(OUTSTR,K,WORD,'W','Z name',IFLAG)
            ZN=WORD(1:12)
            call st2name(ZN,zname(ICOMP))
            zdesc(ICOMP)=' '
          ELSEIF(ND.gt.2)THEN
            CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
            CTYPE=WORD(1:3)
            CALL EGETW(OUTSTR,K,WORD,'W','Z name',IFLAG)
            ZN=WORD(1:12)
            call st2name(ZN,zname(ICOMP))
            call egetrm(outstr,K,phrase,'W','Z description',IER)
            ZDESC(ICOMP)=phrase
          ENDIF
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'NTV NSUR AR',IER)
          K=0
          CALL EGETWI(OUTSTR,K,NTV,4,MTV,'F','no verts',IER)
          CALL EGETWI(OUTSTR,K,NSUR,3,MS,'F','no surfs',IER)
          NZSUR(ICOMP)=NSUR
          NZTV(ICOMP)=NTV
          IF(IFLAG.NE.0)GOTO 1

C Close geometry file.
          CALL ERPFREE(IUF,ISTAT)
          write(currentfile,'(a)') FILE(1:lnblnk(FILE))
        ELSE
          ZNAME(ICOMP)='UNKNOWN'
          write(outs,'(a)')LGEOM(ICOMP)
          CALL USRMSG(' geometry file not found ',outs,'W')
          NZSUR(ICOMP)=0
          NZTV(ICOMP)=0
          VOL(ICOMP)=0.0
        ENDIF

C Fill in a default zone name if blank or UNKNOWN.
        IF(zname(ICOMP)(1:2).EQ.'  '.OR.
     &     zname(ICOMP)(1:7).EQ.'UNKNOWN')THEN
            IF(ICOMP.LE.9)WRITE(zname(ICOMP),'(A5,I1)')'Zone-',ICOMP
            IF(ICOMP.GT.9)WRITE(zname(ICOMP),'(A5,I2)')'Zone-',ICOMP
        ENDIF
        if(zdesc(ICOMP)(1:1).EQ.' ')then
          write(zdesc(ICOMP),'(2a)') 
     &      zname(ICOMP)(1:lnblnk(zname(ICOMP))),' describes a...'
        endif

C Reporting.
        IF(ITRC.GE.1)THEN
          WRITE(OUTS,'(a,i3,a,i3,2a)',IOSTAT=ISTAT,ERR=1) ' Zone',
     &        ICOMP,' index',NCCODE(ICOMP),'Named: ',zname(ICOMP)
          CALL EDISP(iuout,OUTS)
          WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Operations   : ',
     &        LPROJ(ICOMP)(1:LNBLNK(LPROJ(ICOMP)))
          CALL EDISP(iuout,OUTS)
          WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Geometry     : ',
     &        LGEOM(ICOMP)(1:LNBLNK(LGEOM(ICOMP)))
          CALL EDISP(iuout,OUTS)
          WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Constructions: ',
     &        LTHRM(ICOMP)(1:LNBLNK(LTHRM(ICOMP)))
          CALL EDISP(iuout,OUTS)
          IF(IOBS(ICOMP).EQ.1)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Obstructions : ',
     &          ZOBS(ICOMP)(1:LNBLNK(ZOBS(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(ITW(ICOMP).EQ.1)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Transp constr : ',
     &          LTWIN(ICOMP)(1:LNBLNK(LTWIN(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(IVF(ICOMP).EQ.1)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Viewfactors: ',
     &          LVIEW(ICOMP)(1:LNBLNK(LVIEW(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(ICGC(ICOMP).EQ.1)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Casual contrl : ',
     &          LCGCIN(ICOMP)(1:LNBLNK(LCGCIN(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(ISI(ICOMP).EQ.1)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Shading: ',
     &          LSHAD(ICOMP)(1:LNBLNK(LSHAD(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(IHC(ICOMP).EQ.1)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Convect htc : ',
     &          LHCCO(ICOMP)(1:LNBLNK(LHCCO(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(iBSIMP(ICOMP).EQ.1)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' BASESIMP inputs : ',
     &          LBSIMP(ICOMP)(1:LNBLNK(LBSIMP(ICOMP)))
            CALL EDISP(iuout,OUTS)
          ENDIF
          IF(IFCFD(ICOMP).GT.0)THEN
            WRITE(OUTS,'(2a)',IOSTAT=ISTAT,ERR=1)' Cfd domain: ',
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
      if(OUTSTR(1:4).eq.'*cnn')then
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
          CALL USRMSG(' connections file not found ',LCNN,'W')
          if(dll)then
            dllsubr='ersys'
            dllmesg='The connections file does not exist.'
            ier=2
            return
          else
            IER=1
            RETURN
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
      IF(ITRC.GE.1)THEN
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
        CALL EGETWI(OUTSTR,K,ICT(ICON),0,6,'F','conn type',IER)
        IF(IER.NE.0) GOTO 1
        CALL EGETWI(OUTSTR,K,IC2(ICON),0,0,'-','conn other zone',IER)
        IF(IER.NE.0) GOTO 1
        CALL EGETWI(OUTSTR,K,IE2(ICON),0,0,'-','conn other surf',IER)
        IF(IER.NE.0) GOTO 1

C If ground connection IC2=0 then IE2 will be the profile.
        IF(ICT(ICON).EQ.4)THEN
          IF(IC2(ICON).EQ.0.AND.IE2(ICON).GT.NGRDP)THEN
            CALL USRMSG(
     &       'Ground connection referenced an unknown monthly profile!',
     &        OUTSTR,'W')
          ENDIF
          IF(IC2(ICON).EQ.-3.AND..NOT.GRND3D)CALL USRMSG(
     &    ' 3D ground model not available but referenced in',OUTSTR,'W')
        ENDIF

C Range checking.
        IF(ICT(ICON).EQ.0)THEN
          IF(IC2(ICON).NE.0.OR.IE2(ICON).NE.0) CALL USRMSG(
     &      ' Ext conn (4th & 5th items) should be 0 in',OUTSTR,'W')
        ENDIF
        IF(ICT(ICON).EQ.3)THEN
          IF(IC2(ICON).LT.1.OR.IC2(ICON).GT.MCOM)CALL USRMSG(
     &      ' Int conn other zone (4th item) unknown in',OUTSTR,'W')
          IF(IE2(ICON).LT.1.OR.IE2(ICON).GT.MS)CALL USRMSG(
     &      ' Int conn other surf (5th item) unknown in',OUTSTR,'W')
        ENDIF
   21 CONTINUE

C Close connections file and build zone:surface to connections hash array.
      if(cnnok)CALL ERPFREE(IUF,ISTAT)
      do 42 icc = 1, NCON
        IZSTOCN(IC1(icc),IE1(icc))=icc
 42   continue

C With hash known, re-scan geometry files and build connection based
C surface attributes and recover zone volume and surface derived data.
C NOTE: call to zgupdate is after connection lists established.
      do 242 ICOMP=1,NCOMP
        call FINDFIL(LGEOM(ICOMP),XST)
        if(XST)then
          CALL EGOMIN(IUF,LGEOM(ICOMP),ICOMP,1,0,iuout,IER)
          DO 243 IS=1,NZSUR(ICOMP)
            icc=IZSTOCN(icomp,is)
            if(icc.eq.0)then
              write(iuout,*)'zero icc associated with zn ',icomp,
     &          ' & sur',is
              goto 243
            endif
            SSNAME(icc)=SNAME(icomp,is)
            SSOTF(icc)=SOTF(is)
            SSMLCN(icc)=SMLCN(is)
            SSVFC(icc)=SVFC(is)
            SSOTHER(icc)=SOTHER(is)
 243      continue
          call zgupdate(0,icomp,ier)

C Trace geometry related information. Because not all zones will have
c beeen read in do not include the extended surface `context`.
          if(ITRC.ne.0)then
            call ZINFOREP(iuout,icomp)
            context=.false.
            CALL SURINFO(ICOMP,iuout,context)
            IF(ITRC.GT.1)CALL VERINFO(iuout)
            CALL INSINFO(ICOMP,iuout)
          endif
        endif
 242  continue

C Reporting. Connection Zone Surface Type Associated data (zone geometry
C will have been scanned in by this point and surface names known.
      IF(ITRC.GE.1)THEN
        CALL CONXINFO(1,0,CXSTR)
        CALL EDISP(iuout,CXSTR)
        do 41 icc = 1, NCON
          CALL CONXINFO(1,ICC,CXSTR)
          CALL EDISP(iuout,CXSTR)
 41     continue
      ENDIF

C Read in anchors if available. For each anchor, get its name, number
C of links and then the links.
      write(currentfile,'(a)') FILE(1:lnblnk(FILE))
      CALL STRIPC(IUC,OUTSTR,0,ND,1,'anchors',IER)
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
        IF(ITRC.GE.1)THEN
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
          CALL EMFREAD(IUF,ITRC,IER)
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
C reading a second line if necessary.
        if(ND.le.NCOMP)zmfn1=OUTSTR
        K=0
        DO 35 ICOMP=1,NCOMP
          CALL EGETW(OUTSTR,K,WORD,'W','node name',IERV)
          IF(IERV.NE.0) THEN
            K=0
            CALL EDISP(iuout,' reading continuation line...')
            CALL STRIPC(IUC,OUTSTR,0,ND,0,'2nd node-zone',IER)
            IF(IER.NE.0)goto 1
            zmfn2=OUTSTR
            CALL EGETW(OUTSTR,K,WORD,'W','node name',IERV)
          ENDIF
          IF(IERV.NE.0) GOTO 1
          NODID=WORD(1:12)

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
   33     read(WORD,*,ERR=1)ICAAS(ICOMP)
   35   CONTINUE

C Reporting. Make temporary arrays for zone and node names which
C are then written out in packed format (the first 117 chars).
        IF(ITRC.GE.1)THEN
          do 777 j=1,ncomp
            namez(j)=zname(j)
            if(ICAAS(J).ne.0)then
              namen(j)=NDNAM(ICAAS(J))
            else
              namen(j)='---'
            endif
  777     continue

C << this may need to be in a do while loop >>
          CALL EDISP(iuout,' Thermal zone/flow node association:')
          ipos=1
          call aslist(ipos,NCOMP,namez,MCOM,'S',t117,louts,itrunc)
          WRITE(OUTS,'(2a)') ' Zone >',t117(1:louts)
          CALL EDISP(iuout,OUTS)
          call aslist(ipos,NCOMP,namen,MCOM,'S',t117,louts,itrunc)
          WRITE(OUTS,'(2a)')' Node >',t117(1:louts)
          CALL EDISP(iuout,OUTS)
        ENDIF
      ENDIF

C If a flow network and contaminant file have been defined then call the
C subroutine to fill in contaminant common blocks
      IF(IAIRN.GT.0.AND.NOCNTM.EQ.1)then
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
              LSNAM=OUTSTR(1:72)
            elseif(IIER.eq.2)then
              LSNAM=OUTSTR(1:72)
              iier=0
            else
              write(lsnam,'(2a)') 'Systems model ',cfgroot
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
      COMMON/C4/XLAT,XLON
      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM
      CHARACTER*72 SNFNAM
      COMMON/C5R/SKYR,GRDR,BLDR
      CHARACTER POS*124,OUTSTR*124,sdesc*16

      IF(IXPOS.EQ.1)THEN
        POS='typical city centre.'
      ELSEIF(IXPOS.EQ.2)THEN
        POS='typical urban.'
      ELSEIF(IXPOS.EQ.3)THEN
        POS='typical rural.'
      ELSEIF(IXPOS.EQ.4)THEN
        POS='city center, equal views to sky, ground and buildings.'
      ELSEIF(IXPOS.EQ.5)THEN
        POS='city center below mean height of surrounding buildings.'
      ELSEIF(IXPOS.EQ.6)THEN
        POS='isolated rural.'
      ELSEIF(IXPOS.EQ.7)THEN
        POS='within a totally enclosed building.'
      ELSEIF(IXPOS.EQ.8)THEN
        POS='user defined exposure as follows: '
      ENDIF

C Get lat:long description.
      call sitell2s(XLAT,XLON,sdesc)
      WRITE(OUTSTR,'(3a)')' Site location: ',sdesc,
     &  ' of local meridian.'
      CALL EDISP(ITRU,OUTSTR)

      WRITE(OUTSTR,'(a,F4.2,a)')' Ground reflectivity ',GREF,'.'
      CALL EDISP(ITRU,OUTSTR)
      WRITE(OUTSTR,'(2a)')' Site exposure ',POS(1:LNBLNK(POS))
      CALL EDISP(ITRU,OUTSTR)

      IF(IXPOS.EQ.8)THEN
        WRITE(OUTSTR,98911)SKYR
98911   FORMAT(' User defined sky ratio =',F6.4)
        CALL EDISP(ITRU,OUTSTR)
        WRITE(OUTSTR,98912)GRDR
98912   FORMAT('           ground ratio =',F6.4)
        CALL EDISP(ITRU,OUTSTR)
        WRITE(OUTSTR,98913)BLDR
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
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      CHARACTER CXSTR*78,SST1*12,ZST1*12,SST2*12,ZST2*12,zdesc*64
      CHARACTER zname*12,SOTHER*15,SNAME*12,SMLCN*12,SVFC*4,SOTF*4

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

      SST1=SNAME(IC1(ICON),IE1(ICON))
      ZST1=zname(IC1(ICON))
      IF(ICT(ICON).EQ.0.AND.ICF.GT.0)THEN
        WRITE(CXSTR,8985)ICON,SST1(1:LNBLNK(SST1)),ZST1(1:LNBLNK(ZST1))
 8985   FORMAT(I3,2X,a,' in ',a,' is External')
      ELSEIF(ICT(ICON).EQ.1.AND.ICF.GT.0)THEN
        if(IC2(ICON).eq.0.and.IE2(ICON).eq.0)then
          WRITE(CXSTR,8984)ICON,SST1(1:LNBLNK(SST1)),
     &      ZST1(1:LNBLNK(ZST1))
 8984     FORMAT(I3,2X,a,' in ',a,' >|< Identical environment ')
        else
          WRITE(CXSTR,8987)ICON,SST1(1:LNBLNK(SST1)),
     &      ZST1(1:LNBLNK(ZST1)),IC2(ICON),IE2(ICON)
 8987     FORMAT(I3,2X,a,' in ',a,' >|< Similar +-',i3,'dC & ',
     &      I4,' W rad')
        endif
      ELSEIF(ICT(ICON).EQ.2.AND.ICF.GT.0)THEN
        WRITE(CXSTR,8983)ICON,SST1(1:LNBLNK(SST1)),ZST1(1:LNBLNK(ZST1)),
     &                   IC2(ICON),IE2(ICON)
 8983   FORMAT(I3,2X,a,' in ',a,' >|< Constant @',I3,' dC &',
     &    I4,' W rad')
      ELSEIF(ICT(ICON).EQ.3.AND.ICF.GT.0)THEN
        SST2=SNAME(IC2(ICON),IE2(ICON))
        ZST2=zname(IC2(ICON))
        WRITE(CXSTR,8982)ICON,SST1(1:LNBLNK(SST1)),ZST1(1:LNBLNK(ZST1)),
     &                        SST2(1:LNBLNK(SST2)),ZST2(1:LNBLNK(ZST2))
 8982   FORMAT(I3,2X,a,' in ',a,' >|< ',a,' in ',a)
      ELSEIF(ICT(ICON).EQ.4.AND.ICF.GT.0)THEN
        IF(IC2(ICON).GT.0)THEN
          WRITE(CXSTR,8981)ICON,SST1(1:LNBLNK(SST1)),
     &                     ZST1(1:LNBLNK(ZST1)),IC2(ICON)
 8981     FORMAT(I3,2X,a,' in ',a,' >|< ground profile',I2)
        ELSEIF(IC2(ICON).EQ.-3)THEN
          WRITE(CXSTR,8998)ICON,SST1(1:LNBLNK(SST1)),
     &                     ZST1(1:LNBLNK(ZST1))
 8998     FORMAT(I3,2X,a,' in ',a,' >|< 3D ground model')
        ELSE
          WRITE(CXSTR,8980)ICON,SST1(1:LNBLNK(SST1)),
     &                     ZST1(1:LNBLNK(ZST1)),IE2(ICON)
 8980     FORMAT(I3,2X,a,' in ',a,
     &         ' >|< user def ground profile',I2)
        ENDIF
      ELSEIF(ICT(ICON).EQ.5.AND.ICF.GT.0)THEN
        WRITE(CXSTR,7984)ICON,SST1(1:LNBLNK(SST1)),ZST1(1:LNBLNK(ZST1))
 7984   FORMAT(I3,2X,a,' in ',a,' is adiabatic')
C BASESIMP start.
      ELSEIF(ICT(ICON).EQ.6.AND.ICF.GT.0)THEN
        WRITE(CXSTR,7986)ICON,SST1(1:LNBLNK(SST1)),ZST1(1:LNBLNK(ZST1))
 7986   FORMAT(I3,2X,a,' in ',a,' is BASESIMP fndtn')
C BASESIMP end.

C CEN 13791 boundary condition start.
      ELSEIF(ICT(ICON).EQ.7.AND.ICF.GT.0)THEN
        if(IC2(ICON).eq.0.and.IE2(ICON).eq.0)then
          WRITE(CXSTR,9984)ICON,SST1(1:LNBLNK(SST1)),
     &      ZST1(1:LNBLNK(ZST1))
 9984     FORMAT(I3,2X,a,' in ',a,' >|< Identical CEN 13791   ')
        else
          WRITE(CXSTR,9987)ICON,SST1(1:LNBLNK(SST1)),
     &      ZST1(1:LNBLNK(ZST1)),IC2(ICON),IE2(ICON)
 9987     FORMAT(I3,2X,a,' in ',a,' >|< CEN13791+-',i3,'dC & ',
     &      I4,' W rad')
        endif
C CEN 13791 boundary condition end.
      ENDIF

      RETURN
      END

C ******************** HZSTOCN ********************

C HZSTOCN scans the current connections list and builds an
C array IZSTOCN(MCOM,MS) which holds the connection index.

      SUBROUTINE HZSTOCN
#include "building.h"
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
      COMMON/OUTIN/IUOUT,IUIN
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
