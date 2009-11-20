C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C ******************** ESCSYS 
C ESCSYS scans a system configuration file for fabric related
C file names. No range checking is done, ie assumed to have
C been checked elsewhere.  
      SUBROUTINE ESCSYS(FILE,IUC,IUF,IER)
#include "building.h"
#include "model.h"
#include "site.h"
#include "geometry.h"
#include "net_flow.h"
#include "tdf2.h"
#include "cfd.h"
#include "uncertainty.h"
#include "dhw_common.h"
#include "MultiYear_simulations.h"
#include "bc_data.h"
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
#include "lookup_data.h"
#include "CFC_common.h"

      PARAMETER (MSTMC=20)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/C6/INDCFG
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C23/IFPNF,LPNF
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/CFGV/icfgv
      COMMON/PREC8/SLAT,SLON

C Common blocks as in esystem.F (some omitted).
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/AFNZN/zmfn1,zmfn2

      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFT/LTDF,LTDFA
      COMMON/TDFFLG/ITEMSTD
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT)
      common/cctlnm/ctldoc,lctlf
      COMMON/LOG/LPRJLG
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C Images.
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton
      common/initv/initvt,EYEMI(3),VIEWMI(3),ANGI
      common/defdb/dfdblbl,defdbfl
      common/spmfxst/ispmxist,spflnam
      common/entfile/entflnam, ientxist
      COMMON/VTHP18/LNLTHP
      COMMON/VTHP31/INTHPS,INTHPZ(MCOM)
      COMMON/GR1D05/LGRD1D
      COMMON/GR1D06/IGR1D
      COMMON/GRND100/GRND3D
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      common/GR3D100/BLDG3D,ZONE3D(MCOM)
      common/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)
      common/MOIST01/MSTROK,MSTRZN(MCOM)
      common/MOIST02/LMOIST(MCOM)
      COMMON/GRSD100/IndxSt
      COMMON/GRSD101/LGrdSt
      COMMON/GTFIL/GTGEOM
      common/PCONV/ipconv,pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
      common/CONVEM/phtco2,phtnox,phtsox,pclco2,pclnox,pclsox,
     &              pltco2,pltnox,pltsox,pfnco2,pfnnox,pfnsox,
     &              pspco2,pspnox,pspsox,phwco2,phwnox,phwsox
      common/radcfg/LRADCF
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF

C Variables for weekdays, and weekends.
C Assume: Mon=1, Tue=2, Wed=3, Thu=4, Fri=5, Sat=6, Sun=7
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2

C Uncertainty analysis common.
      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      common/IPVF/lipvdatf
      COMMON/BL1/dmdsdesc,bdmds
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      common/anchorb/IALOC(20),lstanchr(20,60)

C Contaminant flow model commons
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM/CNTMFIL,CNTMDESC,NTSTEPC

C HVAC
      common/hvacinfo/ihvacflag,hvacfile
      common/spfldes/spfdescr(MSPS)
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres

C BASESIMP.
      common/bsmtdef/iBSIMP(MCOM),LBSIMP(MCOM)

C AIM-2.
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
      common/gshpinfo/igshp
      common/gshpfile/gshpfile

C Wind-generated electricity
      common/H2wind/LWndSupp,iWndSupp

C GCEP
      common/gcepinfo/igcep
      common/gcepfile/gcepfile

C Bidirectional raw optical data file.
      COMMON/BIDIR/IFLAGBI,INTVALBI,NSTMCFL(MCON)
      COMMON/BIDIRFL/bidirfile,bidirname(MSTMC)

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)

C Error subroutine and error details for dll mode.
      common/dllerr/dllsubr,dllmesg

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

C iSlr_half_hr_flg value = 0 :hour-centered; =1 : half-hour centered.
      common/CWEC_SOL/iSlr_half_hr_flg
      integer iSlr_half_hr_flg     

      LOGICAL BLDG3D,ZONE3D,MSTROK,MSTRZN,dll,browse
      LOGICAL INTHPS,INTHPZ,IGR1D,GRND3D
      LOGICAL XST,OK,DOK,IGDCVS,IGDCNC,IGDNDC,IGDTAQ,cnnok
      LOGICAL OK3DCVS,OK3DCNC,OK3DNDC,OK3DTAQ
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDNDD,LGDTAQ,LGRD1D,LNLTHP
      CHARACTER OUTSTR*124,OUTS*124,OUTS2*124,text*124,h*72
      CHARACTER*72 FILE,LCFD
      character*72 LAPROB,LPNF,LTDF,LTDFA
      character LPRJLG*72
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,LMOIST,LGrdSt
      CHARACTER WORD*20,ZN*12
      character zmfn1*124,zmfn2*124,WORD2*20,msgl2*48
      CHARACTER DBTAG*12,DBTASK*12,DBZN*15,phrase*64
      character lradcf*72
      CHARACTER ctldoc*248,LCTLF*72,LOCUTIL*72
      CHARACTER spflnam*72,entflnam*72,lipvdatf*72
      character dfdblbl*20,defdbfl*72,GTGEOM*72
      character ectime*24,wkd1*10,wkd2*10
      character*10 wkday(7)
      character LUALF*72,LCNG*15,LLOC*15,cfgroot*24,LCFGF*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      CHARACTER dmdsdesc*248,bdmds*72
      character ALOC*12,ALOCLBL*12,ALOCTYP*4
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,spfdescr*8
      character hvacfile*72,LAIM2*72,LBSIMP*72,LFClds*72
      character gshpfile*72,gcepfile*72
      character dllsubr*12,dllmesg*124
      character limgpth*24,ldocpth*24,ltmppth*24,lcfgroot*24,lpath*72
      character path*72,upath*72,lupath*72,bidirfile*72,bidirname*12
      CHARACTER CONTMNAM*12,CNTMDESC*124,cntmfil*72
      character calename*32,calentag*12,calendayname*32,HDR*12
      character LWndSupp*72
      integer lsn  ! length for currentfile

C Flag for multi-year simulation definition
      logical bMY_sim_sps

      DIMENSION GVA(12),IVA(60)
      dimension IRVC(mcon),IRCC(365)
      data wkday/'Monday', 'Tuesday','Wednesday', 'Thursday',
     &           'Friday','Saturday', 'Sunday'/

C Check if running in dll mode.
      call isadll(dll)

      iSet_start = 1
      bBC_data_defined = .false.
      bLookup_data_defined = .false.

C Scan the defaults file and then set file common blocks.
C Make temporary use of file unit IUF.
C Note: escdef must come after scan of .esprc file.
      call escdef(dfdblbl,defdbfl,IUF,IER)
      LCLIM=DCLIM
      LAPRES=DAPRES
      LFMAT=DFCON
      LFMUL=DFMUL
      LOPTDB=DOPTDB
      LPRFDB=DPRFDB
      LPCDB =DPCDB

C Clear the model common blocks.
      call clrprb

      LUALF='UNKNOWN'
      MCMPDBFL='UNKNOWN'
      bidirfile='UNKNOWN'

      iSlr_half_hr_flg = 0

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

C Initialize GSHP flag to indicate no gshp models active. This
C will be reset if `*gshp' included in .cfg.
      gshpfile = ' '
      igshp=0

C Initialize GCEP flag to indicate no gcep models active. This
C will be reset if `*gcep' included in .cfg.
      gcepfile = ' '
      igcep=0

      iWndSupp = 0
      LWndSupp =' '

C Assume older version of configuration file.
      icfgv=1

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

C Open the configuration file, return if not found.
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

C Read '*CONFIGURATION' and flag if a new format file otherwise
C treat as an old format.  If '*CONFIGURATION3.0' read zone info
C via tags.
      CALL STRIPC(IUC,OUTSTR,99,ND,1,'config index',IER)
      IF(IER.NE.0)goto 1
      if(OUTSTR(1:17).eq.'* CONFIGURATION3.')then
        icfgv=3
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
        call usrmsg('Does not appear to be a config file:',FILE,'W')
        ier=1
        return
      endif

C Now read all configuration data from file; start with site data.
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
        CALL EGETWR(OUTSTR,K,buildingview,0.,1.,'W','Building view',
     &    IER)
        IF(IER.NE.0) GOTO 1
      ENDIF

C If '* CONFIGURATION' found then read db until '* PROJ LOG' found.
      if(icfgv.eq.1) goto 45
      if(icfgv.gt.1)then
        CALL STRIPC(IUC,OUTSTR,0,ND,1,'* DATABASES',IER)
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
      if(WORD(1:4).eq.'*prm')then
        CALL EGETRM(OUTSTR,K,LFMAT,'W','material db',IER)
      elseif(WORD(1:4).eq.'*mlc')then
        CALL EGETRM(OUTSTR,K,LFMUL,'W','multilayer db',IER)
      elseif(WORD(1:4).eq.'*opt')then
        CALL EGETRM(OUTSTR,K,LOPTDB,'W','optical db',IER)
      elseif(WORD(1:4).eq.'*prs')then
        CALL EGETRM(OUTSTR,K,LAPRES,'W','pressure db',IER)
      elseif(WORD(1:4).eq.'*evn')then
        CALL EGETRM(OUTSTR,K,LPRFDB,'W','profiles db',IER)
      elseif(WORD(1:4).eq.'*clm')then
        CALL EGETRM(OUTSTR,K,LCLIM,'W','climate db',IER)
        bSY_climate_defined = .true.        
      elseif(WORD(1:6).eq.'*myclm') then
        CALL EGETRM(OUTSTR,K,cTemp,
     &    'W','Multi-year climate db',IER)
        cMY_climate_db_name = cTemp(1:lnblnk(cTemp))
        bMY_climates_defined = .true.
      elseif(WORD(1:7).eq.'*mscldb')then
        CALL EGETRM(LOUTSTR,K,MCMPDBFL,'W','misc comp db',IER)
      elseif(WORD(1:4).eq.'*pdb')then
        CALL EGETRM(OUTSTR,K,LPCDB,'W','plant comp db',IER)
      elseif(WORD(1:4).eq.'*ctl')then
        CALL EGETRM(OUTSTR,K,LCTLF,'W','config control',IER)

      elseif(WORD(1:5).eq.'*gref')then

C Ground reflectivity model, no-snow monthly albedos, snow albedo
        CALL EGETWI(OUTSTR,K,groundreflmodel,1,3,'F',
     &    'ground reflection model',IER)
        DO 441 I=1,12
          CALL EGETWR(OUTSTR,K,groundreflmonth(I),0.,1.,'W',
     &      'monthly gr. refl.',IER)
  441   CONTINUE
        CALL EGETWR(OUTSTR,K,snowgroundrefl,0.,1.,'W','snow refl.',IER)  

      elseif(WORD(1:5).eq.'*snow')then

C Number of days with snow on the ground (if ground refl model 2)
        DO 442 I=1,12
          CALL EGETWI(OUTSTR,K,dayswithsnow(I),0,31,'W',
     &      'days with snow each month',IER)
  442   CONTINUE

      elseif(WORD(1:5).eq.'*sndf')then

C Snow depth file (if ground refl. model 3)
        CALL EGETRM(OUTSTR,K,SNFNAM,'W','snow depth file',IER)
      elseif(WORD(1:4).eq.'*bcd')then

C Flexible boundry condition data definition      
        call EGETRM(OUTSTR,K,cTemp,'W','BC data file', IER)
        cBC_data_file_name = cTemp(1:lnblnk(cTemp))
        bBC_data_defined = .true.
      elseif(WORD(1:4).eq.'*shl')then

C SHOCC library is associated with this model.
        CALL EGETRM(OUTSTR,K,SHOCCshlFile,'W','SHOCC',IER)

      elseif(WORD(1:5).eq.'*hvac')then

C hvac file.
        CALL EGETRM(OUTSTR,K,hvacfile,'W','config control',IER)
        ihvacflag=1
      elseif(WORD(1:5).eq.'*dhw')then

C dhw file.
        CALL EGETRM(OUTSTR,K,sDHW_InputFileName,'W','dhw data',IER)
        iDHW_FLAG=1
      elseif(WORD(1:4).eq.'*aim')then

C AIM-2.
        CALL EGETRM(OUTSTR,K,LAIM2,'W','AIM-2 infil data',IER)
      elseif(WORD(1:4).eq.'*ctm')then

C Contaminant model 
        CALL EGETRM(OUTSTR,K,TEXT,'F','contaminant model',IER)
        CNTMFIL=TEXT(1:72)
        NOCNTM=1
      elseif(WORD(1:4).eq.'*fcl')then

C Fuel cell.
        iAIM2=1
        CALL EGETRM(OUTSTR,K,LFClds,'W','Fuel cell elec loads',IER)
        iFClds=1
      elseif(WORD(1:5).eq.'*gshp')then

c GSHP
        CALL EGETRM(OUTSTR,K,gshpfile,'W','GSHP config data',IER)
        igshp=1
      elseif(WORD(1:5).eq.'*gcep')then

c GCEP
        CALL EGETRM(OUTSTR,K,gcepfile,'W','GCEP config data',IER)
        igcep=1
      elseif(WORD(1:4).eq.'*wnd')then

C Wind-generated electricity
C If a file holding the time history of wind-generated electricity exists,
C then get the name of the file and test for its existence.
        CALL EGETRM(OUTSTR,K,LWndSupp,'W',
     &    'Wind-gen elec supply file',IER )
        iWndSupp=1
      elseif(WORD(1:12).eq.'*slr_half_hr')then

C Solar radiation (half hour data) flag.
        call egetwi(outstr,k,iSlr_half_hr_flg,0,1,'W',
     &    'solar half-hour flag',IER)
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

C CPCALC global information (do not bother to read).
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
      elseif(WORD(1:6).eq.'*resh2')then

C H2-dep_controller:
        CALL EGETRM(OUTSTR,K,resh2flnam,'W','res h2 ctl',IER)
        iresh2ctl = 1
      elseif(WORD(1:4).eq.'*tdf')then

C If marked for Temporal Database read associations until *end tdf
        CALL EGETRM(OUTSTR,K,LTDFA,'W','TDF db name',IER)
        CALL ERPFREE(IUTDF,ISTAT)
        call FINDFIL(LTDFA,XST)
        if(XST)then
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
              ITDFLG=0
            endif
            CALL ERPFREE(IUTDF,ISTAT)
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
 102        msgl2='  '
            CALL USRMSG(' could not read header record 1',msgl2,'W')
            CALL ERPFREE(IUTDF,ISTAT)
          endif
        else

C The temporal file was not found, but keep reading until * end tdf found.
          ITDFLG=0
          M=0
  159     CALL STRIPC(IUC,OUTSTR,99,ND,1,'instance names',IER)
          IF(IER.NE.0)GOTO 158
          IF(OUTSTR(1:9).EQ.'* end tdf')GOTO 158
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
          goto 159
  158     CONTINUE
          ITEMSTD=M
        endif
        if(itdflg.ne.0)then
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
          IF(OUTSTR(1:9).EQ.'* end tdf')GOTO 58
          GOTO 59
   58     CONTINUE
        endif
        ITEMSTD=M
        CALL ERPFREE(IUTDF,ISTAT)
      elseif(WORD(1:4).eq.'*sps' .or. WORD(1:6).eq. '*mysps')then

C Check for simulation setup data and save level.
        CALL EGETWI(OUTSTR,K,nsset,1,MSPS,'W','no. of sets',IER)
        CALL EGETWI(OUTSTR,K,isstup,0,300,'W','startup',IER)
        CALL EGETWI(OUTSTR,K,isbnstep,1,60,'W','zone_ts',IER)
        CALL EGETWI(OUTSTR,K,ispnstep,0,100,'W','plant_ts',IER)
        CALL EGETWI(OUTSTR,K,issave,0,5,'W','save_lv',IER)

C If 6 items on line also read whether to save only 1stph.
        if(ND.eq.7)then
          CALL EGETWI(OUTSTR,K,isavgh,0,1,'W','save_hourly',IER)
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

          CALL EGETRM(OUTSTR,K,spfdescr(iss),'W','set name',IER)

 334      CALL STRIPC(IUC,OUTSTR,0,ND,1,'res names',IER)
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','res tags',IFLAG)

C The following code ensures the sXXres varaibles are initialized
C to empty strings even if they are not defind in the input file.
          write(sblres(iss), '(A)') '  '
          write(sflres(iss), '(A)') '  '
          write(smstres(iss),'(A)') '  '
          write(splres(iss), '(A)') '  '
          write(selres(iss), '(A)') '  '
          
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
        CALL EGETRM(OUTSTR,K,sipvres,'W','ipv report',IER)
        goto 44
      elseif(WORD(1:7).eq.'*annipv')then
        continue
        goto 44
      elseif(outstr(1:10).eq.'* PROJ LOG')then

C Project log file, read and then go on to fabric/plant description.
        CALL STRIPC(IUC,OUTSTR,0,ND,1,'project log file',IER)
        LPRJLG=OUTSTR(1:LNBLNK(OUTSTR))
        goto 45
      else
        call usrmsg(' Unknown entry format... ',outstr,'W')
        ier=1
        return
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
        IF(WORD(1:5).EQ.'*gtmp')THEN
          CALL EGETWRA(IUC,getVal,12,-9.0,99.0,'W','Ex lg rad',IER)
          iExtLgRadFlag = 1
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
  28      CONTINUE
          goto 344
        elseif(WORD(1:4).eq.'*end')then
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
      CALL EGETWI(OUTSTR,K,NCOMP,1,MCOM,'W','zones',IER)
      IF(IER.NE.0) GOTO 1

C Zone file names.
      icomp=0
   46 if(icfgv.eq.3)then
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
          IFCFD(icomp)=1
          LCFD(icomp)=text(1:72)
          NCONF=NCONF+1
          icfdnd(NCONF)=ICOMP
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

C SHOCC zone file has been included.
          SHOCCshzFile(icomp)=text(1:72)
        elseif(WORD(1:4).eq.'*bsm')then

C BASESIMP description for this zone so set iBSIMP().
          iBSIMP(icomp)=1
          LBSIMP(icomp)=text(1:72)
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
          CALL EGETWI(OUTSTR,K,INDU,0,1,'F','utility flg',IER)
          IF(IER.NE.0) GOTO 1

C Warn user about old utility file name.
          if(INDU.EQ.1)then
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
            lnzname(ICOMP)=0
            zdesc(ICOMP)=' '
            lnzdesc(ICOMP)=0
          ELSEIF(ND.EQ.2)THEN
            CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
            CTYPE=WORD(1:3)
            CALL EGETW(OUTSTR,K,WORD,'W','Z name',IFLAG)
            ZN=WORD(1:12)
            call st2name(ZN,zname(ICOMP))
            lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update length of this string.
            zdesc(ICOMP)=' '
            lnzdesc(ICOMP)=0
          ELSEIF(ND.gt.2)THEN
            CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
            CTYPE=WORD(1:3)
            CALL EGETW(OUTSTR,K,WORD,'W','Z name',IFLAG)
            ZN=WORD(1:12)
            call st2name(ZN,zname(ICOMP))
            lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update length of this string.
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
          lnzname(ICOMP)=0
        ENDIF

C Fill in a default zone name if blank or UNKNOWN.
        IF(zname(ICOMP)(1:2).EQ.'  '.OR.
     &     zname(ICOMP)(1:7).EQ.'UNKNOWN')THEN
          IF(ICOMP.LE.9)WRITE(zname(ICOMP),'(A5,I1)')'Zone-',ICOMP
          IF(ICOMP.GT.9)WRITE(zname(ICOMP),'(A5,I2)')'Zone-',ICOMP
          lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update this string length
        ENDIF
        if(zdesc(ICOMP)(1:1).EQ.' ')then
          write(zdesc(ICOMP),'(2a)') 
     &      zname(ICOMP)(1:lnzname(ICOMP)),' describes a...'
          lnzdesc(ICOMP)=lnblnk(zdesc(ICOMP))  ! update the length of this string.
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
          CALL STRIPC(IUF,OUTSTR,0,ND,1,'connex file date',IER)
          CALL STRIPC(IUF,OUTSTR,0,ND,1,'no of connex',IER)
          K=0
          CALL EGETWI(OUTSTR,K,NCON,3,MCON,'W','no connex',IER)
          IF(IER.NE.0) GOTO 1
        else
          CALL USRMSG(' connections file not found ',LCNN,'W')
        endif
      else
        cnnok=.false.
        K=0
        CALL EGETWI(OUTSTR,K,NCON,3,MCON,'W','no connection',IER)
        IF(IER.NE.0) GOTO 1
      endif

      DO 21 ICON=1,NCON
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
     &       'Ground connection referenced an unknown monthly profile!',
     &        OUTSTR,'W')
          ENDIF
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

C If this is an older model only sother(is,1) will have a value
C so do some tests before instanciating.
            IF(SOTHER(is,1)(1:2).EQ.'  ')then
              SSOTHER(icc,1)='UNKNOWN'
              SSOTHER(icc,2)='0'
              SSOTHER(icc,3)='0'
            ELSEIF(SOTHER(is,1)(1:7).EQ.'UNKNOWN')then
              SSOTHER(icc,1)=SOTHER(is,1)
              SSOTHER(icc,2)=SOTHER(is,2)
              SSOTHER(icc,3)=SOTHER(is,3)
            ELSEIF(SOTHER(is,1)(1:8).EQ.'EXTERIOR')then
              SSOTHER(icc,1)=SOTHER(is,1)
              SSOTHER(icc,2)=SOTHER(is,2)
              SSOTHER(icc,3)=SOTHER(is,3)
            ELSEIF(SOTHER(is,1)(1:9).EQ.'ADIABATIC')then
              SSOTHER(icc,1)=SOTHER(is,1)
              SSOTHER(icc,2)=SOTHER(is,2)
              SSOTHER(icc,3)=SOTHER(is,3)
            ELSEIF(SOTHER(is,1)(1:9).EQ.'IDENT_CEN')then
              SSOTHER(icc,1)=SOTHER(is,1)
              SSOTHER(icc,2)=SOTHER(is,2)
              SSOTHER(icc,3)=SOTHER(is,3)
            ELSEIF(SOTHER(is,1)(1:7).EQ.'SIMILAR'.OR.
     &             SOTHER(is,1)(1:8).EQ.'CONSTANT'.OR.
     &             SOTHER(is,1)(1:8).EQ.'BASESIMP')then

C If similar check if 2nd item is a '-', if so use the values
C from IC2 and IE2, otherwise use sother data.
              if(SOTHER(is,2)(1:1).EQ.'-')then
                SSOTHER(icc,1)=SOTHER(is,1)
                write(SSOTHER(icc,2),'(i2.2)') IC2(icc)
                write(SSOTHER(icc,3),'(i2.2)') IE2(icc)
              else
                SSOTHER(icc,1)=SOTHER(is,1)
                SSOTHER(icc,2)=SOTHER(is,2)
                SSOTHER(icc,3)=SOTHER(is,3)
              endif
            ELSEIF(SOTHER(is,1)(1:6).EQ.'GROUND')then

C If constant check if 2nd item is a '-', if so use the values
C from IC2 and IE2, otherwise use sother data.
              if(SOTHER(is,2)(1:1).EQ.'-')then
                SSOTHER(icc,1)=SOTHER(is,1)
                write(SSOTHER(icc,2),'(i2.2)') IC2(icc)
                write(SSOTHER(icc,3),'(i2.2)') IE2(icc)
              else
                SSOTHER(icc,1)=SOTHER(is,1)
                SSOTHER(icc,2)=SOTHER(is,2)
                SSOTHER(icc,3)=SOTHER(is,3)
              endif
            ELSEIF(SOTHER(is,1)(1:7).EQ.'ANOTHER')then

C ANOTHER is a key word from new format geometry file so the 2nd
C and 3rd slots might have useful data.

C If partition check if 2nd item is a '-', if so we are missing
C the surface info so get from the connection variables.
C from IC2 (zone) and IE2 (surf), otherwise use sother data.
C Note ssother(icc,2) is a slot for surface and ssother(icc,3) is zone.
              if(SOTHER(is,2)(1:1).EQ.'-')then
                SSOTHER(icc,1)=SOTHER(is,1)
                write(SSOTHER(icc,2),'(i2.2)') IC2(icc)
                write(SSOTHER(icc,3),'(i2.2)') IE2(icc)
              else
                SSOTHER(icc,1)=SOTHER(is,1)
                SSOTHER(icc,2)=SOTHER(is,2)
                SSOTHER(icc,3)=SOTHER(is,3)
              endif
            else

C We have probably been passed a zone name from a legacy file so
C fill SSOTHER from IC2 & IE2.
              SSOTHER(icc,1)(1:7)='ANOTHER'
              write(SSOTHER(icc,2),'(i2.2)') IC2(icc)
              write(SSOTHER(icc,3),'(i2.2)') IE2(icc)
            endif
            SSUSE(icc,1)=SUSE(is,1)
            SSUSE(icc,2)=SUSE(is,2)
            SSPARENT(icc)=SPARENT(is)
 243      continue
          call zgupdate(0,icomp,ier)
        endif
 242  continue

C Read in anchors if available.
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
        goto 343
      endif

C Check if there is an fluid (1=text, 2=grphic) air flow network available: 0 = no.
      CALL STRIPC(IUC,OUTSTR,0,ND,1,'flow network index',IER)
      IF(IER.NE.0)goto 1
 343  K=0
      CALL EGETWI(OUTSTR,K,IAIRN,0,2,'F','IAIRN',IER)

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

C Read the file header and check for first-line tag. If 4 items
C then an older file so rewind the file and then call emfread.
        CALL STRIPC(IUF,OUTSTR,99,ND,0,'1st line of file',IER)
        IF(ND.GE.4)THEN
          REWIND(IUF,ERR=999)
          CALL ERPFREE(IUF,ISTAT)
          IAIRN = 1
        elseif((ND.eq.1.or.ND.eq.2).and.
     &          OUTSTR(1:18).EQ.'*Graphical_network')then

C Found a graphic network file, scan it (silently) and then convert it
C into network flow common blocks.
          IAIRN = 2
          CALL ERPFREE(IUF,ISTAT)
        endif

C Next line (or two) in cfg file will be energy zone/fluid(=air) flow network 
C node mapping. There will be one for each zone so read that
C many items and save each line intact (so can write out later).

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
            dllsubr='scsys'
            dllmesg=outs
            ier=2
            return
          else
            dok=.true.
            h(1)='At the end of the configuration file there are'
            h(2)='data linking zones and mass flow nodes. If you'
            h(3)='continue the rest of the file will be scanned '
            h(4)='otherwise if you say no some of the model data '
            h(5)='will be missed. Please check your model. '
            call askok('There might be a problem with flow node-zone',
     &        'links in the configuration file, continue?',OK,dok,5)
            if(.NOT.OK)RETURN
            ier=0
          endif
        endif

C If all on one line remember whole string otherwise read next line.
        if(ND.eq.NCOMP)then
          zmfn1=OUTSTR
        elseif(ND.lt.NCOMP)then
          zmfn1=OUTSTR
          CALL STRIPC(IUC,zmfn2,0,ND,0,'2nd node-zone',IER)
          IF(IER.NE.0)then
            outs='Problem with flow node-zone links in cfg file.'
            if(dll)then
              dllsubr='scsys'
              dllmesg=outs
              ier=2
              return
            else
              dok=.true.
              h(1)='At the end of the configuration file there are'
              h(2)='data linking zones and mass flow nodes. If you'
              h(3)='continue the rest of the file will be scanned '
              h(4)='otherwise if you say no some of the model data '
              h(5)='will be missed. Please check your model. '
              call askok('There might be a problem with flow node-zone',
     &        'links in the configuration file, continue?',OK,dok,5)
              if(.NOT.OK)RETURN
              ier=0
            endif
          endif
        endif
      ENDIF

C Building description now complete, begin to read in plant network
C file name. 
 888  continue
      IF(INDCFG.EQ.2.OR.INDCFG.EQ.3)THEN

C Identify plant file; skip if this is '* Plant' record.
        CALL STRIPC(IUC,OUTSTR,0,ND,1,'* plant',IER)
        IF(IER.NE.0)goto 1
        LPNF=OUTSTR(1:72)
        IF(LPNF(1:7).EQ.'* Plant'.OR.LPNF(1:7).EQ.'* PLANT') THEN
          CALL STRIPC(IUC,OUTSTR,0,ND,1,'* plant',IER)
          IF(IER.NE.0)goto 1
          LPNF=OUTSTR(1:72)
        ENDIF
      endif

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

 1    CALL USRMSG('ESCSYS Conversion error in',OUTSTR,'W')
      lsn=MIN0(lnblnk(currentfile),110)
      write(outs2,'(2a)') 'in: ',currentfile(1:lsn)
      call edisp(iuout,' Please check your file:')
      call edisp(iuout,outs2)
      IER=1
      GOTO 6

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
