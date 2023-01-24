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


C This file includes the following subroutines.
C  EMKCFG     - rrites the model data to the system configuration file.
C  CONXINF    - returns inter-connection description in CXSTR.
C  ckcurmatch - scans and report connection mis-matches.

C ******************** EMKCFG ********************
C Writes the model data to the system configuration file.

      SUBROUTINE EMKCFG(act,IER)
      USE AIM2_InputData, ONLY:LAIM2,iAIM2
      IMPLICIT NONE
#include "building.h"
#include "model.h"
#include "site.h"
#include "geometry.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "tdf2.h"
#include "uncertainty.h"
#include "MultiYear_simulations.h"
#include "bc_data.h"
#include "esprdbfile.h"
#include "lookup_data.h"
#include "sbem.h"
#include "roam.h"
#include "CFC_common.h"
#include "ipvdata.h"
#include "seasons.h"
#include "UserSimulationToggles.h"
#include "schedule.h"
#include "dhw_common.h"
#include "qrun.h"
#include "plant.h"
#include "power.h"
#include "FMI.h"
#include "help.h"
#include "agent.h"
#include "gremlin.h"

      INTEGER, PARAMETER :: MSTMC=20
      integer lnblnk  ! function definition
      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      INTEGER :: ic1,ie1,ict,ic2,ie2
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      INTEGER :: IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/C6/INDCFG
      INTEGER :: INDCFG
      INTEGER :: IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

C Plant network.
      COMMON/C23/IFPNF,LPNF
      INTEGER :: ifpnf
      COMMON/C23a/BPF

C Configuration control.
      common/cctlnm/ctldoc,lctlf

C Images.
      character imgfmt*4    ! GIF XBMP TIF JPG
      character imgfoc*4    ! FZON FNET FCTL FDFS FPLN
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! 0 if images not yet shown, 1 if yes
      common/imagfi/noimg,iton

C Initial view.
      common/initv/initvt,EYEMI(3),VIEWMI(3),ANGI
      REAL :: angi,EYEMI,VIEWMI
      INTEGER :: initvt

C Special materials file.
      common/spmfxst/ispmxist,spflnam
      INTEGER :: ispmxist

C Fluid flow network.
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas

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
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD

C Ground topology.
      COMMON/GTFIL/GTGEOM

C CFD.
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      INTEGER :: ifcfd

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

C Temporal definitions.
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)

C Primary energy and emissions conversions.
      common/PCONV/ipconv,pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
      INTEGER :: ipconv
      REAL :: pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
      common/CONVEM/phtco2,phtnox,phtsox,pclco2,pclnox,pclsox,
     &              pltco2,pltnox,pltsox,pfnco2,pfnnox,pfnsox,
     &              pspco2,pspnox,pspsox,phwco2,phwnox,phwsox
      REAL :: phtco2,phtnox,phtsox,pclco2,pclnox,pclsox
      REAL :: pltco2,pltnox,pltsox,pfnco2,pfnnox,pfnsox
      REAL :: pspco2,pspnox,pspsox,phwco2,phwnox,phwsox
 
C IPV description via ipvdata.h. If within cfg file then the file
C name is replaced with 'internal'.
      character lipvdatf*72
      common/IPVF/lipvdatf

C CPCALC.
      common/CPCALC/icpcon,ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,
     &              sbh,pad,wvpe
      REAL :: ble,bwi,bhi,blox,bloy,bloz,orient,ra,sbh,pad,wvpe
      INTEGER :: icpcon,irt

C Variables for weekdays and weekends (Monday=1 through Sunday=7).
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2
      INTEGER :: idwe1,idwe2

C Strings associated with one climatelist item.
C  clmlnam    - selection list string
C  clmlaid    - aide memoire for list item
C  clmldbfile - weather file name
C  clmlavail  - either ONLINE or OFFLINE
C  clmlhelp   - array of help strings
      character clmlnam*42,clmlaid*72,clmldbfile*144,clmlavail*10
      character clmlhelp*72
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      common/clmlnum/nbclmlhelp
      INTEGER :: nbclmlhelp
      common/ACTDOM/CFDOK
      logical CFDOK

C Simulation parameter presets.
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

C Extended simulation parameters for each set.
      common/spfldats/isstupex(MSPS),isbnstepex(MSPS),ispnstepex(MSPS),
     &  issaveex(MSPS),isavghex(MSPS),iscfdactivate(MSPS),
     &  isicfdys(MSPS),isicfdyf(MSPS),
     &  scftims(MSPS),scftimf(MSPS)
      INTEGER :: isstupex,isbnstepex,ispnstepex,issaveex,isavghex
      INTEGER :: iscfdactivate      ! 0, ignore domains
      INTEGER :: isicfdys,isicfdyf  ! CFD simulation start & finish days
      REAL :: scftims,scftimf       ! CFD simulation start & finish time

C BASESIMP.
      common/bsmtdef/iBSIMP(MCOM),LBSIMP(MCOM)
      INTEGER :: ibsimp

C HVAC presence flag (ihvacflag=1 active; 0 not active).
      common/hvacinfo/ihvacflag,hvacfile
      INTEGER :: ihvacflag

C HRV inputs.
      Integer iMech
      Character *72 LMech
      Common /mechvdef/ iMech, LMech

C Contaminant flow.
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM/CNTMFIL,CNTMDESC,NTSTEPC
      INTEGER :: NCONTM,NOCNTM,NTSTEPC

C SHOCC.
C  bSHOCCed     - logical flag whether project has SHOCC input
C  bZoneSHOCCed - logical array whether zones have SHOCC input
C  SHOCCshlFile - path to .shl file for SHOCC'd project library
C  SHOCCshzFile - path to .shz file for each SHOCC'd zone
      common/SHOCCcfg/bSHOCCed,SHOCCshlFile,bZoneSHOCCed(mcom),
     &                SHOCCshzFile(mcom)
      logical bSHOCCed,bZoneSHOCCed
      character SHOCCshlFile*72,SHOCCshzFile*72

C Renewable energy system (electrical).
      common/reselecctl/ireselecctl,reselecflnam
      integer ireselecctl       ! flag indicating file availability
      character*72 reselecflnam ! name of file with controller inputs

C Fuel cell.
      common/Fcell/iFClds,LFClds
      INTEGER :: iFClds

C External longwave radiation (used by HOT3000).
      COMMON/LongRad/iExtLgRadFlag,eGrdTp
      INTEGER :: iExtLgRadFlag
      REAL :: eGrdTp(12)
      INTEGER jMon

C GSHP.
       common/gshpinfo/igshp
       common/gshpfile/gshpfile
       INTEGER igshp  
       character gshpfile*72 

C Wind-generated electricity.
      common/H2wind/LWndSupp,iWndSupp
      INTEGER :: iWndSupp

C GCEP.
       common/gcepinfo/igcep
       common/gcepfile/gcepfile
       INTEGER igcep  
       character gcepfile*72 

C Bi-directional optical data file.
      COMMON/BIDIR/IFLAGBI,INTVALBI,NSTMCFL(MCON)
      INTEGER :: IFLAGBI,INTVALBI,NSTMCFL
      COMMON/BIDIRFL/bidirfile,bidirname(MSTMC)

C Calendar.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER :: icalender,nbcaldays,nbdaytype

C Solar data directive.
      common/CWEC_SOL/iSlr_half_hr_flg
      integer iSlr_half_hr_flg      ! 0 hour-centered; 1 half-hour centered
      
C Indoor air humidity ratio handling.
      common/EVCOOL/iflgevapC(mcom) 
      integer iflgevapC             ! if 1, indoor = outdoor wet bulb temperature
      integer ii
      
C Shading directive.      
      integer iexcludediff_flag     ! if 1, exclude diffuse shading
      common/excludediff/iexcludediff_flag

C Shading/insolation calculation mode.
      common/shad0/ISIcalc,icalcD,icalcM
      integer ISIcalc               ! 0 default, 1 embedded, 2 file
      integer icalcD,icalcM 

C Radiance processor cores.
      integer radcores
      common/radcor/radcores

C Zone fluid.
      COMMON/zfluid/znotair(mcom),zfldK,zfldD,zfldC,zfldA,
     &              zSWAp(mcom),zSWAf(mcom)
      real zfldK,zfldD,zfldC,zfldA,zSWAp,zSWAf
      LOGICAL znotair              ! zone can be air or water filled

      integer izconstv,iztmcv
      common/znconstrv/izconstv(MCOM),iztmcv(MCOM)

      CHARACTER*72 LBSIMP,hvacfile,LFClds
      CHARACTER LAPROB*72
      CHARACTER CXSTR*78
      CHARACTER LPNF*72
      CHARACTER*72 BPF
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,LMOIST,LGrdSt
      CHARACTER ctldoc*248,LCTLF*72
      CHARACTER spflnam*72
      character GTGEOM*72,LCFD*72,DCNN*72
      CHARACTER DBTAG*12,DBTASK*12,DBZN*15,indxcmt*24,DBSN*15
      character*10 wkd1, wkd2
      CHARACTER outs*124
      character spfdescr*30,ltmp*72,louts*144
      character bidirfile*72,bidirname*12
      character calename*32,calentag*12,calendayname*32,loutc*96
      character LWndSupp*72
      CHARACTER CNTMFIL*72,CONTMNAM*12,CNTMDESC*124
      character descr*7,descrst*10,descrfn*10  ! for season dates
      character descrst1*10,descrfn1*10        ! for 2nd season dates
      character descr2st*8,descr2fn*8,descr2st1*8,descr2fn1*8
      
C Array of mass flow nodes for writing.
      character namen*12
      dimension namen(mcom)

      character act*1        ! '-' normal use, 's' for silent
      character ipvaction*3  ! to instruct mkipv to write to cfg file
      character tab*1        ! literal tab character
      character fs*1         ! separator

      dimension iav(99)

      logical dogrnd,unixok
      character dstmp*24
      character ALOC*12
      character tokens*156,comment*76,aligned_str*156
      character TempString*248         ! temporary hvac folder path to check if folder exists
      character doit*248      ! temporary hvac folder path if folder does not exist

C Multi-weather simulations preset
      logical bMY_set_open 

      integer itrunc  ! indicates a continuation line
      integer ipos    ! position in flow node string buffer
      integer lnouts  ! location at end of flow node string buffer
      integer lnipv   ! location at end of the external IPV file
      integer loop    ! controls looping
      logical lexist  ! does ipv directives file exit
      
      INTEGER :: ier,i,ia,iav,icc,icd,id1,id2,id3,id4,im1,im2,im3,im4
      INTEGER :: ij,img,ios,iss,iuf,iw,j,l,lcfgl,lcfgr,lct,lctn,loutlen
      INTEGER :: loutln,ncn,lnzglbl,lr
      logical cmtopr,cmtgeo,cmtcon,cmttmc,cmtivf,cmtisi  ! comment on file types
      
      IUF=IFIL+1
      IER=0

C Open any existing file by this name or create a new file.
      CALL EFOPSEQ(IFCFG,LCFGF,3,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF
      helpinsub='emkcfg'  ! set for subroutine
      helptopic='cfg_version_reminder'
      call gethelptext(helpinsub,helptopic,nbhelp)

      cmtopr=.false.; cmtgeo=.false.; cmtcon=.false.
      cmttmc=.false.; cmtivf=.false.; cmtisi=.false. 

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Remind user that a newer version of the file can be written.
      if(icfgv.lt.4)then
        CALL EASKAB('Configuration file loaded was in old format.',
     &    'Save in:','latest format','cancel',IW,nbhelp)
        if(IW.EQ.2)then
          call edisp(iuout,'System configuration file not updated.')
          return

C Upgrade file format.
        elseif(IW.EQ.1)then

C If no model root name has been set, make from configuration file.
C If filename > 4 char it might have a .cfg attached.
          icfgv=4; usecurcfg=1     ! use version 4 format
          lcfgr=lnblnk(LCFGF)
          lcfgl=lcfgr-3
          if(lcfgr.gt.4)then
            if(LCFGF(lcfgl:lcfgr).eq.'.cfg')then
              if(lcfgl.gt.32)then
                write(cfgroot,'(a)',iostat=ios,err=3) LCFGF(1:32)
              else
                write(cfgroot,'(a)',iostat=ios,err=3) LCFGF(1:lcfgl-1)
              endif
            else
              if(lcfgr.gt.32)then
                write(cfgroot,'(a)',iostat=ios,err=3) LCFGF(1:32)
              else
                write(cfgroot,'(a)',iostat=ios,err=3) LCFGF(1:lcfgr)
              endif
            endif
          else
            if(lcfgr.gt.32)then
              write(cfgroot,'(a)',iostat=ios,err=3) LCFGF(1:32)
            else
              write(cfgroot,'(a)',iostat=ios,err=3) LCFGF(1:lcfgr)
            endif
          endif

C Check connections file.
          if(ncon.gt.1)then
  290       write(LCNN,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.cnn'
            DCNN=LCNN
            ltmp=LCNN
            CALL EASKS(ltmp,' ','Connections file name?',
     &        72,DCNN,'system connx file name',IER,nbhelp)
            if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
              LCNN=ltmp
            else
              GOTO 290
            endif
          elseif(ncon.eq.0.and.indcfg.gt.0)then

C If emkcfg was called from ecnv with an initial registration model
C that had no zones but where indcfg had already been reset to 1
C then get the name of the connections file.
            write(LCNN,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.cnn'
          endif
        endif
      else

C Also check connections file if in interactive mode.
        if(ncon.gt.1)then
          if(act.eq.'-'.and.LCNN(1:4).eq.'UNKN')then
  292       write(LCNN,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.cnn'
            DCNN=LCNN
            ltmp=LCNN
            CALL EASKS(ltmp,' ','Connections file name?',
     &        72,DCNN,'system connx file name',IER,nbhelp)
            if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
              LCNN=ltmp
            else
              GOTO 292
            endif
          endif
        elseif(ncon.eq.0.and.indcfg.gt.0)then

C If emkcfg was called from ecnv with an initial registration model
C that had no zones but where indcfg had already been reset to 1
C then get the name of the connections file.
          write(LCNN,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.cnn'
        endif
      endif

C Test to see if model needs to be updated from registration
C to some other condition. If act is '-' ask; if act is 's'
C take the default name for the connections file.
      if(INDCFG.EQ.0)then
        if(NCOMP.eq.1.and.LGEOM(1)(1:4).ne.'dumm')then
          INDCFG=1
          write(LCNN,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.cnn'
          if(act(1:1).eq.'-')then
  289       DCNN='model.cnn'
            ltmp=LCNN
            CALL EASKS(ltmp,' ','Connections file name?',72,
     &        DCNN,'system connx file name',IER,nbhelp)
            if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
              LCNN=ltmp
            else
              GOTO 289
            endif
          endif
          if(LPNF(1:4).ne.'UNKN'.and.LPNF(1:2).ne.'  ')then
            INDCFG=3
          endif
        elseif(ncomp.gt.1)then
          INDCFG=1
          if(LPNF(1:4).ne.'UNKN'.and.LPNF(1:2).ne.'  ')then
            INDCFG=3
          endif
        endif
      elseif(INDCFG.EQ.1)then
        if(LPNF(1:4).ne.'UNKN'.and.LPNF(1:2).ne.'  ')then
          INDCFG=3
        endif
      endif

C Comment for configuration index.
      IF(INDCFG.EQ.0) indxcmt=' # Project registration'
      IF(INDCFG.EQ.1) indxcmt=' # Building only'
      IF(INDCFG.EQ.2) indxcmt=' # Plant only'
      IF(INDCFG.EQ.3) indxcmt=' # Building & Plant'

C Begin writing the file with version number in the header.
      if(icfgv.eq.3)then
        write(tokens,'(a)') '* CONFIGURATION3.0'
        write(comment,'(a)') 'ESP-r model configuration V3'
        call align_comment(32,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      elseif(icfgv.eq.4)then
        write(tokens,'(a)') '* CONFIGURATION4.0'
        write(comment,'(a)') 'ESP-r model configuration V4'
        call align_comment(32,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      endif

      call dstamp(dstmp)
      write(tokens,'(2a)') '*date ',dstmp
      write(comment,'(a)') 'latest file modification'
      call align_comment(32,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=3) '*root ',
     &  cfgroot(1:lnblnk(cfgroot))
      write(tokens,'(2a)') '*zonpth ',zonepth
      write(comment,'(a)') 'path to zone files'
      call align_comment(32,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      write(tokens,'(2a)') '*netpth ',netpth
      write(comment,'(a)') 'path to network files'
      call align_comment(32,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      write(tokens,'(2a)') '*ctlpth ',ctlpth
      write(comment,'(a)') 'path to control files'
      call align_comment(32,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      write(tokens,'(2a)') '*mscpth ',mscpth
      write(comment,'(a)') 'path to miscel. files'
      call align_comment(32,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      write(tokens,'(2a)') '*radpth ',radpth
      write(comment,'(a)') 'path to radiance files'
      call align_comment(32,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      write(tokens,'(2a)') '*imgpth ',imgpth
      write(comment,'(a)') 'path to project images'
      call align_comment(32,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      write(tokens,'(2a)') '*docpth ',docpth
      write(comment,'(a)') 'path to project documents'
      call align_comment(32,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      write(tokens,'(2a)') '*dbspth ',dbspth
      write(comment,'(a)') 'path to local databases'
      call align_comment(32,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      write(tokens,'(2a)') '*tmppth ',tmppth
      write(comment,'(a)') 'path to temporary files'
      call align_comment(32,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      write(tokens,'(a,i2)') '*radcore ',radcores
      write(comment,'(a)') 'cores available to Radiance'
      call align_comment(32,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

C If there are IES files and entities associated with the model:
      if(nbofies.gt.0)then
        write(tokens,'(a,i2)') '*ies ',nbofies
        write(comment,'(a)') 'number of IES entities'
        call align_comment(32,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        do loop=1,nbofies
          write(IFCFG,'(4a)',IOSTAT=IOS,ERR=3) '*nameies ',
     &      iesname(loop)(1:lnblnk(iesname(loop))),', ',iesmenu(loop)
           write(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*iesraw ',
     &       iesfile(loop)(1:lnblnk(iesfile(loop))),'  # raw data'
          write(IFCFG,'(a,3F7.3,3a)') '*fixture ',ieslen(loop),
     &      ieswid(loop),iesht(loop),' ',iesalong(loop),
     &      '  # length, width, height'
          write(IFCFG,'(a,i2,5f5.2)',IOSTAT=IOS,ERR=3) '*ctlies ',
     &      iespercents(loop),(iessteps(loop,I),I=1,iespercents(loop))
        enddo
        write(IFCFG,'(a)',IOSTAT=IOS,ERR=3) '*endies '
      endif

      write(tokens,'(A,i4)') '*indx ',INDCFG
      IF(INDCFG.EQ.0) comment='project registration'
      IF(INDCFG.EQ.1) comment='building only'
      IF(INDCFG.EQ.2) comment='plant only'
      IF(INDCFG.EQ.3) comment='building & plant'
      call align_comment(32,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

      write(tokens,'(F7.3,2X,F7.3)') sitelat,sitelongdif
      write(comment,'(a)') 
     &  'latitude & longitude difference (from reference meridian)'
      call align_comment(32,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      write(tokens,'(I7,2X,F6.3)') siteexposureindex,groundrefl
      write(comment,'(a)') 'site exposure & ground reflectivity'
      call align_comment(32,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      if(siteexposureindex.EQ.8)then
        write(tokens,'(3F8.3)') skyview,groundview,buildingview
        write(comment,'(a)') 
     &    'views to sky, ground & surrounding obstructions'
        call align_comment(32,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      endif

C Write databases related to this model. Databases are located
C in the ESP-r standard location or the local model folder.
      WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) '* DATABASES'

      if(ipathmat.eq.0.or.ipathmat.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*mat ',
     &      LFMAT(1:lnblnk(LFMAT))
      elseif(ipathmat.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdmat ',
     &      LFMAT(1:lnblnk(LFMAT))
      endif

      if(ipathcfc.eq.0.or.ipathcfc.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*cfcdb ',
     &      LCFCDB(1:lnblnk(LCFCDB))
      elseif(ipathcfc.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdcfcdb ',
     &      LCFCDB(1:lnblnk(LCFCDB))
      endif

      if(ipathmul.eq.0.or.ipathmul.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*mlc ',
     &      LFMUL(1:lnblnk(LFMUL))
      elseif(ipathmul.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdmlc ',
     &      LFMUL(1:lnblnk(LFMUL))
      endif

      if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*opt ',
     &      LOPTDB(1:lnblnk(LOPTDB))
      elseif(ipathoptdb.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdopt ',
     &      LOPTDB(1:lnblnk(LOPTDB))
      endif

      if(ipathapres.eq.0.or.ipathapres.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*prs ',
     &      LAPRES(1:lnblnk(LAPRES))
      elseif(ipathapres.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdprs ',
     &      LAPRES(1:lnblnk(LAPRES))
      endif

      if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*evn ',
     &      LPRFDB(1:lnblnk(LPRFDB))
      elseif(ipathprodb.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdevn ',
     &      LPRFDB(1:lnblnk(LPRFDB))
      endif

      if(ipathclim.eq.0.or.ipathclim.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*clm ',
     &      LCLIM(1:lnblnk(LCLIM))
      elseif(ipathclim.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdclm ',
     &      LCLIM(1:lnblnk(LCLIM))
      endif

      if(ipathmsc.eq.0.or.ipathmsc.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*mscldb ',
     &      MCMPDBFL(1:lnblnk(MCMPDBFL))
      elseif(ipathmsc.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdmscldb ',
     &      MCMPDBFL(1:lnblnk(MCMPDBFL))
      endif
      if(ipathmould.eq.0.or.ipathmould.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*mould ',
     &    lfmould(1:lnblnk(lfmould))
      elseif(ipathmould.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdmould  ',
     &    lfmould(1:lnblnk(lfmould))
      endif

C Ground reflectivity model (no-snow monthly albedos & snow albedo
C if ground reflectivity model 2 or 3).
      IF(groundreflmodel.GT.1) WRITE(IFCFG,803) '*gref ',
     &   groundreflmodel,(groundreflmonth(I),I=1,12),snowgroundrefl
  803 FORMAT(A,I1,13(1X,F5.3))

C Number of days with snow on the ground (if ground reflectivity model 2).
      IF(groundreflmodel.EQ.2)  
     &  WRITE(IFCFG,805) '*snow',(dayswithsnow(I),I=1,12)
  805 FORMAT(A,12(1X,I2))

C Snow depth file (if ground reflectivity model 3).
      IF(groundreflmodel.EQ.3) WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) 
     &  '*sndf ',SNFNAM(1:lnblnk(SNFNAM))

C Multi-year weather database.
      if (bMY_climates_defined) then 
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*myclm ',
     &    cMY_climate_db_name(1:lnblnk(cMY_climate_db_name))
      endif

C Plant components database.
      if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*pdb ',
     &      LPCDB(1:lnblnk(LPCDB))
      elseif(ipathpcdb.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdpdb ',
     &      LPCDB(1:lnblnk(LPCDB))
      endif

C UK SBEM database.
      if(ipathsbem.eq.0.or.ipathsbem.eq.1)then
        WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*sbem ',
     &    LSBEM(1:lnblnk(LSBEM))
      elseif(ipathsbem.eq.2)then
        WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*stdsbem ',
     &    LSBEM(1:lnblnk(LSBEM))
      endif

C Predefined objects database.
      if(ipathpredef.eq.0.or.ipathpredef.eq.1)then
        WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*predef ',
     &    LPREDEF(1:lnblnk(LPREDEF))
      elseif(ipathpredef.eq.2)then
        WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*stdpredef ',
     &    LPREDEF(1:lnblnk(LPREDEF))
      endif

C HVAC.
      IF (ihvacflag.EQ.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*hvac ',
     &    hvacfile(1:lnblnk(hvacfile))
      endif

C DHW.
      IF (iDHW_FLAG.EQ.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*dhw ',
     &    sDHW_InputFileName(1:lnblnk(sDHW_InputFileName))
      endif

C AIM-2.
      IF (iAIM2.EQ.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*aim ',
     &    LAIM2(1:lnblnk(LAIM2))
      endif

C MVNT.
      IF (iMECH.EQ.1)then
        WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*mvnt ',
     &    LMECH(1:lnblnk(LMECH))
      endif

C Fuel cell.
      IF (iFClds.EQ.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*fcl ',
     &    LFClds(1:lnblnk(LFClds))
      endif

C GSHP
      IF (igshp.EQ.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*gshp ',
     &    gshpfile(1:lnblnk(gshpfile))
      endif

C GCEP
      IF (igcep.EQ.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*gcep  ',
     &    gcepfile(1:lnblnk(gcepfile))
      endif

C Wind-generated electricity.
      if (iWndSupp.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*wnd ',
     &    LWndSupp(1:lnblnk(LWndSupp))
      endif

C Solar radiation handling.
      if(iSlr_half_hr_flg.eq.0)then
        write(tokens,'(a,i2)') '*slr_half_hr ',iSlr_half_hr_flg
        write(comment,'(a)') 'solar data hour centred'
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      else
        write(tokens,'(a,i2)') '*slr_half_hr ',iSlr_half_hr_flg
        write(comment,'(a)') 'solar data half-hour centred'
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      endif

C Site altitude.
      WRITE(IFCFG,'(A,F6.1)',IOSTAT=IOS,ERR=2) 
     &  '*sitealt ',sitealt

C Direct evaporative cooling.
      do 888 ii = 1,ncomp      
        if(iflgevapC(ii).eq.1)then
          WRITE(IFCFG,'(A,i2,i3,A)',IOSTAT=IOS,ERR=2) '*Twb_evCool ',
     &      iflgevapC(ii),ii,
     &      '  # indoor = outdoor wet bulb temperature'
        endif
 888  continue

C Diffuse shading flag.
      if(iexcludediff_flag.eq.1)then
        WRITE(IFCFG,'(A,i2,A)',IOSTAT=IOS,ERR=2) '*exclude_diff_shad  ',
     &    iexcludediff_flag,'  # diffuse shading is excluded'
      endif      
      
C Quick run method  
      if(iQuick_run.eq.0)then
        write(tokens,'(a,i2)') '*quick_run ',iQuick_run
        write(comment,'(a)') 'no'
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      else
        write(tokens,'(a,i2)') '*quick_run ',iQuick_run
        write(comment,'(a)') 'yes'
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      endif  
  
C SHOCC.
      IF ( bSHOCCed ) then
        write(tokens,'(2a)') '*shl ',
     &    SHOCCshlFile(1:lnblnk(SHOCCshlFile))
        write(comment,'(a)') 'SHOCC project library'
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      endif

C BCD (boundary condition definiton file).
      if ( bBC_data_defined ) then
        write(tokens,'(2a)') '*bcd ',
     &    cBC_data_file_name(1:lnblnk(cBC_data_file_name))
        write(comment,'(a)') 'BCD file name'
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      endif 

C Lookup table.
      if ( bLookup_data_defined ) then
        write(tokens,'(2a)') '*tab ',cLookup_data_file_name
     &    (1:lnblnk(cLookup_data_file_name))
        write(comment,'(a)') 'Lookup table file'
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      endif 

C Write control file if known.
      if(lnblnk(lctlf).eq.0)then
        continue
      elseif(lctlf(1:7).eq.'UNKNOWN')then
        continue
      elseif(lctlf(1:2).eq.'  ')then
        continue
      else
        write(tokens,'(2a)') '*ctl ',LCTLF(1:lnblnk(LCTLF))
        write(comment,'(a)') 'building/system control file'
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      endif

C Renewable energy system.
      if(ireselecctl.gt.0.and.reselecflnam(1:2).ne.'  ')then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*resel ',
     &    reselecflnam(1:lnblnk(reselecflnam))
      endif

C Model contents.
      if(lnblnk(lmodelqa).eq.0)then
        continue
      elseif(lmodelqa(1:7).eq.'UNKNOWN')then
        continue
      elseif(lmodelqa(1:2).eq.'  ')then
        continue
      else
        WRITE(IFCFG,'(a,a)',IOSTAT=IOS,ERR=2) '*contents ',
     &    lmodelqa(1:lnblnk(lmodelqa))
      endif

C Contaminants.
      IF(NOCNTM.GT.0)WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     & '*ctm ',CNTMFIL(1:lnblnk(CNTMFIL))

C Initial view information if other than default.
      if(initvt.eq.1)then
        WRITE(IFCFG,'(a,6f8.1,f5.1)',IOSTAT=IOS,ERR=2)'*vew ',
     &    EYEMI(1),EYEMI(2),EYEMI(3),
     &    VIEWMI(1),VIEWMI(2),VIEWMI(3),ANGI
      endif

C Simulation toggles. 
      UserGAM: if ( bUserGAM ) then 
        WRITE(IFCFG,'(A,A,f8.4)',IOSTAT=IOS,ERR=2)
     &     '*sim-toggle ', 
     &     'bld-soln-implicitness ',
     &     fUserGAM
      endif UserGAM
      
      UserSolar: if ( bUserSolar ) then 
        if ( iUserSolar == 0 ) then
          WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &     '*sim-toggle ', 
     &     'bld-solar-processing ',
     &     'enabled '
        else 
          WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &     '*sim-toggle ', 
     &     'bld-solar-processing ',
     &     'disabled '
        endif 
      endif UserSolar
      
      IF(CALENAME(1:7).NE.'UNKNOWN')THEN
        write(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*calename ',
     &    calename(1:lnblnk(calename))
        do 443 icd = 1,nbdaytype
          lct=lnblnk(calentag(icd))
          lctn=lnblnk(calendayname(icd))
          write(tokens,'(5a,i3)') '*calentag ',
     &      calentag(icd)(1:lct),', ',calendayname(icd)(1:lctn),', ',
     &      nbcaldays(icd)
          write(comment,'(a)') 'day type name, menu entry, instances'
          call align_comment(48,tokens,comment,aligned_str)
          if(icd.eq.1)then
            write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))  ! Comment on first day type
          else
            write(IFCFG,'(a)') tokens(1:lnblnk(tokens))
          endif
 443    continue
        write(tokens,'(a,i4)') '*list ',nbdaytype
        write(comment,'(a)') 'day types & type for each day of year'
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        itrunc=1
        ipos=1
        do while (itrunc.ne.0)
          call ailist(ipos,365,icalender,365,'C',loutc,loutlen,itrunc)
          write(IFCFG,'(1x,a)',IOSTAT=ios,ERR=2) loutc(1:loutlen)
          ipos=itrunc+1
        end do
        write(IFCFG,'(a)',IOSTAT=IOS,ERR=2) '*end_list'
      endif

C Roaming.
      IF(IROAM.EQ.1)THEN
         WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*roaming ',LROAM
      ENDIF

C Uncertainty analysis.
      if(lnblnk(LUALF).eq.0)then
        continue
      elseif(LUALF(1:7).eq.'UNKNOWN')then
        continue
      else
        WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*ual ',
     &    LUALF(1:lnblnk(LUALF))
      endif

C Alternative weekends definition.
      if(idwe1.ne.6.or.idwe2.ne.7)then
        write(tokens,'(a,2I3)') '*weekend ',idwe1,idwe2
        write(comment,'(a)') 'alternative weekend definition'
        call align_comment(32,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      endif

C Seasonal definitions associated with the weather file.
      if(ihaveseason.gt.0)then
        tab=CHAR(9) 
        write(IFCFG,'(a)')'*seasons '
        write(IFCFG,'(3a)')'*name ',tab,clmlnam(1:lnblnk(clmlnam))
        write(IFCFG,'(3a)')'*aide ',tab,clmlaid(1:lnblnk(clmlaid))
        call edayr(is1wins,id1,im1)
        call edayr(is1winf,id2,im2)
        call edayr(is2wins,id3,im3)
        call edayr(is2winf,id4,im4)
        call stdate(iyear,is1wins,descr,descrst,descr2st)
        call stdate(iyear,is1winf,descr,descrfn,descr2fn)
        call stdate(iyear,is2wins,descr,descrst1,descr2st1)
        call stdate(iyear,is2winf,descr,descrfn1,descr2fn1)
        write(tokens,'(a,8i3)') '*winter_s ',id1,im1,id2,im2,id3,im3,
     &    id4,im4
        write(comment,'(8a)') 'winter season ',descrst,'--',descrfn,
     &    ' & ',descrst1,'--',descrfn1
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

        call edayr(is1sprs,id1,im1)
        call edayr(is1sprf,id2,im2)
        call edayr(is2sprs,id3,im3)
        call edayr(is2sprf,id4,im4)
        call stdate(iyear,is1sprs,descr,descrst,descr2st)
        call stdate(iyear,is1sprf,descr,descrfn,descr2fn)
        call stdate(iyear,is2sprs,descr,descrst1,descr2st1)
        call stdate(iyear,is2sprf,descr,descrfn1,descr2fn1)
        write(tokens,'(a,8i3)') '*spring_s ',id1,im1,id2,im2,id3,im3,
     &    id4,im4
        write(comment,'(8a)') 'spring season ',descrst,'--',descrfn,
     &    ' & ',descrst1,'--',descrfn1
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

        call edayr(is1sums,id1,im1)
        call edayr(is1sumf,id2,im2)
        call stdate(iyear,is1sums,descr,descrst,descr2st)
        call stdate(iyear,is1sumf,descr,descrfn,descr2fn)
        write(tokens,'(a,4i3)') '*summer_s ',id1,im1,id2,im2
        write(comment,'(4a)') 'summer season ',descrst,'--',descrfn
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

        call edayr(ia1wins,id1,im1)
        call edayr(ia1winf,id2,im2)
        call edayr(ia2wins,id3,im3)
        call edayr(ia2winf,id4,im4)
        call stdate(iyear,ia1wins,descr,descrst,descr2st)
        call stdate(iyear,ia1winf,descr,descrfn,descr2fn)
        call stdate(iyear,ia2wins,descr,descrst1,descr2st1)
        call stdate(iyear,ia2winf,descr,descrfn1,descr2fn1)

        write(tokens,'(a,8i3)') '*winter_t ',id1,im1,id2,im2,id3,im3,
     &    id4,im4
        write(comment,'(8a)') 'winter typical ',descrst,'--',descrfn,
     &    ' & ',descrst1,'--',descrfn1
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

        call edayr(ia1sprs,id1,im1)
        call edayr(ia1sprf,id2,im2)
        call edayr(ia2sprs,id3,im3)
        call edayr(ia2sprf,id4,im4)
        call stdate(iyear,ia1sprs,descr,descrst,descr2st)
        call stdate(iyear,ia1sprf,descr,descrfn,descr2fn)
        call stdate(iyear,ia2sprs,descr,descrst1,descr2st1)
        call stdate(iyear,ia2sprf,descr,descrfn1,descr2fn1)

        write(tokens,'(a,8i3)') '*spring_t ',id1,im1,id2,im2,id3,im3,
     &    id4,im4
        write(comment,'(8a)') 'spring typical ',descrst,'--',descrfn,
     &    ' & ',descrst1,'--',descrfn1
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

        call edayr(iasums,id1,im1)
        call edayr(iasumf,id2,im2)
        call stdate(iyear,iasums,descr,descrst,descr2st)
        call stdate(iyear,iasumf,descr,descrfn,descr2fn)
        write(tokens,'(a,4i3)') '*summer_t ',id1,im1,id2,im2
        write(comment,'(4a)') 'summer typical ',descrst,'--',descrfn
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        write(IFCFG,'(a)')'*season_end'
      endif

      write(tokens,'(a,i4)') '*year ',IYEAR
      write(comment,'(a)') 'assessment year'
      call align_comment(48,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

C Water filled zone data.
      write(tokens,'(a,f4.1,3f7.1)') '*water_in_zones ',
     &  zfldK,zfldD,zfldC,zfldA
      write(comment,'(a)') 
     &  'water cond, density, spec heat, shortwave abs'
      call align_comment(48,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

C NCM.
      IF(ISBEM.GE.1)THEN
        if(LASBEM(1:4).eq.'UNKN')then
          continue
        else
          WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*ncm ',
     &      LASBEM(1:lnblnk(LASBEM))
        endif
      ENDIF
      IF(INOTI.EQ.1)THEN
        write(tokens,'(a)') '*not'
        write(comment,'(a)')'UK NCM notional model (10% thermal bridges'
        call align_comment(32,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      ELSEIF(INOTI.EQ.2)THEN
        write(tokens,'(a)') '*ref'
        write(comment,'(a)') 'UK NCM reference model'
        call align_comment(32,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2)OUTS(1:LNBLNK(OUTS))
      ELSEIF(INOTI.EQ.3)THEN
        write(tokens,'(a)') '*typ'
        write(comment,'(a)') 'UK NCM typical model'
        call align_comment(32,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2)OUTS(1:LNBLNK(OUTS))
      ELSEIF(INOTI.EQ.4)THEN
        write(tokens,'(a)') '*str'
        write(comment,'(a)') 'UK NCM stripped model'
        call align_comment(32,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2)OUTS(1:LNBLNK(OUTS))
      ENDIF

C Images and documentation.
      if(noimg.gt.0)then
        do 42 img=1,noimg
          WRITE(IFCFG,'(a,a4,2x,a4,2x,a)',IOSTAT=IOS,ERR=2)'*img ',
     &     imgfmt(img),imgfoc(img),limgfil(img)(1:lnblnk(limgfil(img)))
          if(icfgv.gt.3)then
            WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2)'*imdoc ',
     &        imgdoc(img)(1:lnblnk(imgdoc(img)))
          endif
 42     continue
      endif

C Primary energy conversions.
      if(ipconv.eq.1)then
        WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2) 
     &   '# prim energy conv (heat,cool,lights,fan,sml pwr,hot water)'
        WRITE(IFCFG,'(a,6F6.3)',IOSTAT=IOS,ERR=2) '*pecnv ',
     &    pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
        write(tokens,'(a,3F9.3)') '*htemis ',phtco2,phtnox,phtsox
        write(comment,'(a)') 'heating emissions CO2,NOX,SOX'
        call align_comment(32,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

        write(tokens,'(a,3F9.3)') '*clemis ',pclco2,pclnox,pclsox
        write(comment,'(a)') 'cooling emissions CO2,NOX,SOX'
        call align_comment(32,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

        write(tokens,'(a,3F9.3)') '*ltemis ',pltco2,pltnox,pltsox
        write(comment,'(a)') 'lighting emissions CO2,NOX,SOX'
        call align_comment(32,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

        write(tokens,'(a,3F9.3)') '*fnemis ',pfnco2,pfnnox,pfnsox
        write(comment,'(a)') 'fan/pump emissions CO2,NOX,SOX'
        call align_comment(32,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

        write(tokens,'(a,3F9.3)') '*spemis ',pspco2,pspnox,pspsox
        write(comment,'(a)') 'small power emissions CO2,NOX,SOX'
        call align_comment(32,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

        write(tokens,'(a,3F9.3)') '*hwemis ',phwco2,phwnox,phwsox
        write(comment,'(a)') 'DHW emissions CO2,NOX,SOX'
        call align_comment(32,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      endif

C IPV. If directives exist then save them to a file in msc folder.
      lnipv=lnblnk(lipvdatf)
      lr=lnblnk(cfgroot)
      if(nms.gt.0.or.neds.gt.0)then

C Check to see if msc folder exists, if not create on and set mscpth 
C of the folder as it is used in creating the hvac file name
        WRITE(TempString,'(4a)')'..',fs,'msc',fs
        INQUIRE(FILE=TempString,EXIST=lexist)
        if (.not.lexist) then
          write(doit,'(4a)') 'mkdir ','..',fs,'msc'
          call usrmsg('Creating folder:',doit,'-')
          call runit(doit,'-')
          call pausems(400)
          write(mscpth,'(3a)')'..',fs,'msc'
        endif
        if(ipvform.eq.0) ipvform=2               ! if format not defined, use tab separated
        WRITE(ltmp,'(4a)') mscpth(1:lnblnk(mscpth)),fs,
     &    cfgroot(1:lr),'.ipv'
        if(lipvdatf(1:7).eq.'UNKNOWN')then       ! create default name and write
          lipvdatf=ltmp
          lnipv=lnblnk(lipvdatf)
          ipvaction='ipv'
          call mkipvdat(iuf,ltmp,ipvaction)
          WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*ipv ',
     &      lipvdatf(1:lnipv)
        elseif(lipvdatf(1:8).eq.'internal')then  ! convert from internal to external file
          lipvdatf=ltmp
          lnipv=lnblnk(lipvdatf)
          ipvaction='ipv'
          call mkipvdat(iuf,ltmp,ipvaction)
          WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*ipv ',
     &      lipvdatf(1:lnipv)
        else
          lnipv=lnblnk(lipvdatf)
          WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*ipv ', ! save the current ipv file name
     &      lipvdatf(1:lnipv)
        endif
      endif

C Building demands file.
      if(lnblnk(bdmds).eq.0)then
        continue
      elseif(bdmds(1:7).eq.'UNKNOWN')then
        continue
      else
        WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*dmds ',
     &    bdmds(1:lnblnk(bdmds))
      endif

C CPCALC.
      if(icpcon.eq.1)then
        WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2) '# CPCALC data'
        WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) 
     &    '# building lenght/width/hight,origin (x,y,z),',
     &    'orientation,roof type,roof angle,SBH,PAD,Vexp'
        WRITE(IFCFG,'(a,6F7.2,F6.1,I2,4F7.2)',IOSTAT=IOS,ERR=2)
     &    '*cpcalc ',ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,sbh,pad,
     &    wvpe
      ENDIF

C Radiance.
      if(lnblnk(lradcf).eq.0)then
        continue
      elseif(lradcf(1:7).eq.'UNKNOWN')then
        continue
      else
        WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*rif ',
     &    lradcf(1:lnblnk(lradcf))
      endif

C Non-linear thermophysical properties.
      IF(INTHPS)WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*nlt ',
     &  LNLTHP(1:lnblnk(LNLTHP))

C Building 1D node distribution.
      IF(IGR1D)WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*g1d ',
     &  LGRD1D(1:lnblnk(LGRD1D))

C Bi-directional optical data.
      if(lnblnk(bidirfile).eq.0)then
        continue
      elseif(bidirfile(1:7).eq.'UNKNOWN')then
        continue
      else
        ncn=ncon
        WRITE(IFCFG,'(a,i3,1x,a)',IOSTAT=IOS,ERR=2) '*biopt ',
     &    NCN,bidirfile(1:lnblnk(bidirfile))
        itrunc=1
        ipos=1
        do while (itrunc.ne.0)
          call ailist(ipos,ncn,NSTMCFL,MCON,'C',louts,loutln,itrunc)
          write(IFCFG,'(1x,a)',IOSTAT=ios,ERR=146) louts(1:loutln)
          ipos=itrunc+1
        end do
      endif

C Special materials and electrical files.
      if(ispmxist.gt.0.and.spflnam(1:2).ne.'  ')then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*spf ',
     &    spflnam(1:lnblnk(spflnam))
      endif
      if(ientxist.gt.0)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*pnt ',
     &    entflnam(1:lnblnk(entflnam))
      endif

C Temporal definitions.
      if(ITDFLG.ne.0)then
        if(itdflg.gt.1)then
          WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*tdf ',
     &      LTDFA(1:lnblnk(LTDFA))
        elseif(itdflg.lt.0.or.itdflg.eq.1)then
          WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*tdf ',
     &      LTDF(1:lnblnk(LTDF))
        endif
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) 
     &    '# tdf item id, item type, <related zone>, <related surface>'
        if(ITEMSTD.gt.0)then
          do 27 L=1,ITEMSTD
            WRITE(IFCFG,29)DBTAG(L),DBTASK(L),DBZN(L),DBSN(L)
  29        FORMAT(A12,1X,A8,1X,2A16)
  27      continue
        endif
        WRITE(IFCFG,'(A)')'* end tdf'
      endif

C Simulation parameter presets.
      if(nsset.gt.0)then
        write(tokens,'(A,6i4)') '*sps ',nsset,isstup,isbnstep,ispnstep,
     &    issave,isavgh
        if(isavgh.eq.0)then
          write(comment,'(2a)') 'parameter sets, default startup, ',
     &      'zone & plant ts, save level & frequency'
        else
          write(comment,'(2a)') 'parameter sets, default startup, ',
     &      'zone & plant ts, save level @ 1 ts/hr'
        endif
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

C Multi-year weather data (only works with save level 5).
        bMY_set_open = .false.
        do 333 iss=1,nsset

C Flag indicating start of multi-year climate record.
          if (bMY_period_support(iss) .and. .not. bMY_set_open) then
            write(IFCFG,'(A)') '*start_my_sets'
            bMY_set_open = .true.
          endif 

C Extended simulation parameter sets.
          if ( bMY_period_support(iss) ) then
            write(tokens,'(5I4,2I3,I5,2I3,I5,2x,A)') 
     &        isstupex(iss),isbnstepex(iss),ispnstepex(iss),
     &        issaveex(iss),isavghex(iss),isstday(iss),isstmon(iss),
     &        iMY_period_start_years(iss),isfnday(iss),isfnmon(iss),
     &        iMY_period_end_years(iss),
     &        spfdescr(iss)(1:lnblnk(spfdescr(iss)))
            if(iss.eq.1)then
              write(comment,'(2a)') 'startup, zone & plant ts, ',
     &          'save level, @ ts, period start DM & end DM'
              call align_comment(48,tokens,comment,aligned_str)
              write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
            else
              write(IFCFG,'(a)') tokens(1:lnblnk( tokens))
            endif
          else
            write(tokens,'(9I4,2x,A)') 
     &        isstupex(iss),isbnstepex(iss),ispnstepex(iss),
     &        issaveex(iss),isavghex(iss),isstday(iss),isstmon(iss),
     &        isfnday(iss),isfnmon(iss),
     &        spfdescr(iss)(1:lnblnk(spfdescr(iss)))
            if(iss.eq.1)then
              write(comment,'(2a)') 'startup, zone & plant ts, ',
     &          'save level, @ ts, period start DM & end DM'
              call align_comment(48,tokens,comment,aligned_str)
              write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
            else
              write(IFCFG,'(a)') tokens(1:lnblnk( tokens))
            endif
          endif 
          if(INDCFG.ne.2)WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*sblr ',
     &      sblres(iss)(1:lnblnk(sblres(iss)))
          if(IAIRN.ge.1)WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*sflr ',
     &      sflres(iss)(1:lnblnk(sflres(iss)))
          if(INDCFG.eq.2.or.INDCFG.eq.3)then
            WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*splr ',
     &        splres(iss)(1:lnblnk(splres(iss)))
          endif
          if(MSTROK)WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*smstr ',
     &      smstres(iss)(1:lnblnk(smstres(iss)))
          if(ientxist.gt.0)WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)
     &      '*selr ',selres(iss)(1:lnblnk(selres(iss)))

C CFD.
          if(iscfdactivate(iss).gt.-1)then
            WRITE(IFCFG,'(A,3I4,2F7.3,2A)',IOSTAT=IOS,ERR=2)
     &       '*scfdr ',iscfdactivate(iss),isicfdys(iss),isicfdyf(iss),
     &       scftims(iss),scftimf(iss),' ',
     &       scfdres(iss)(1:lnblnk(scfdres(iss)))
          endif
          WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2)'*end_set'

C End of multi-simulation sets.
          if ( iss .eq. nsset ) then
            if ( bMY_period_support(iss) )
     &        write(IFCFG, '(A)') '*end_my_sets'
            bMY_set_open = .false.
          elseif ( bMY_set_open .and.
     &            .not. bMY_period_support (iss+1) ) then         
            write(IFCFG, '(A)') '*end_my_sets'
            bMY_set_open = .false.
          endif

 333    continue
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2)'*end_sps'
      endif

C IPV annual report.
      if(icfgv.lt.4)then
        if(lnipv.eq.0)then
          continue
        elseif(lipvdatf(1:7).eq.'UNKNOWN')then
          continue
        else
          if(sipvres(1:2).ne.'  '.and.sipvres(1:7).ne.'UNKNOWN')then
            WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*intipv ',
     &        sipvres(1:lnblnk(sipvres))
          endif
        endif
      else
        if(nipvassmt.gt.0)then
          if(sipvres(1:2).ne.'  '.and.sipvres(1:7).ne.'UNKNOWN')then
            WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*intipv ',
     &        sipvres(1:lnblnk(sipvres))
          endif
        endif
      endif


      WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) '* PROJ LOG'
      WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2)lmodellog(1:lnblnk(lmodellog))

C Ground information.
      dogrnd = .false.
      IF(NGRDP.gt.0.or.GRND3D)dogrnd=.true.
      IF(GTGEOM(1:7).ne.'UNKNOWN'.and.GTGEOM(1:2).ne.'  ')dogrnd=.true.

C Ground temperature.
      IF (iExtLgRadFlag .EQ. 1) dogrnd = .true.
      if(dogrnd)then
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) '* Ground '
      IF(iExtLgRadFlag.EQ.1) THEN
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2)'*gtmp '
        WRITE(IFCFG,933,IOSTAT=IOS,ERR=2)(eGrdTp(jMon),jMon=1,12)
      ENDIF

C 3D ground files.
        IF(GRND3D)THEN
          WRITE(IFCFG,'(2A)')'*cvs  ',LGDCVS(1:lnblnk(LGDCVS))
          WRITE(IFCFG,'(2A)')'*cnc  ',LGDCNC(1:lnblnk(LGDCNC))
          WRITE(IFCFG,'(2A)')'*ndc  ',LGDNDC(1:lnblnk(LGDNDC))
          WRITE(IFCFG,'(2A)')'*taq  ',LGDTAQ(1:lnblnk(LGDTAQ))
        ENDIF
        if(GTGEOM(1:7).ne.'UNKNOWN'.and.GTGEOM(1:2).ne.'  ')then
          WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*gtp ',
     &      GTGEOM(1:lnblnk(GTGEOM))
        endif
        if(ngrdp.gt.0)then
          WRITE(IFCFG,'(A,i3)',IOSTAT=IOS,ERR=2)'*mgp ',NGRDP
          DO 932 I=1,NGRDP
            WRITE(IFCFG,933,IOSTAT=IOS,ERR=2)(UGRDTP(J,I),J=1,12)
  933       FORMAT(12F7.2)
  932     CONTINUE
        endif
        if(ngrdph.gt.0)then
          WRITE(IFCFG,'(A,i3)',IOSTAT=IOS,ERR=2)'*mgh ',NGRDPH
          DO 942 I=1,NGRDPH
            WRITE(IFCFG,943,IOSTAT=IOS,ERR=2)(UGRDHUM(J,I),J=1,12)
  943       FORMAT(12F6.0)
  942     CONTINUE
        endif

        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) '*end'
      endif

C Registration.
      if(INDCFG.EQ.0)then
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) 
     &    modeltitle(1:lnblnk(modeltitle))
        goto 9028
      endif

C If plant only jump.
      IF(INDCFG.EQ.2)GOTO 9021

C Fabric portion of description.
      WRITE(IFCFG,5006,IOSTAT=IOS,ERR=2) 
     &  modeltitle(1:lnblnk(modeltitle))
 5006 FORMAT('* Building',/,A)

      write(tokens,'(i7)') NCOMP
      write(comment,'(a)') 'number of zones'
      call align_comment(48,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

      DO 920 I = 1,NCOMP
        if(znotair(I))then  ! if water filled
          write(tokens,'(a,i3,a)') '*zon ',NCCODE(I),' Water'
          write(comment,'(2a)') 'reference for ',zname(I)
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        else
          write(tokens,'(a,i3)') '*zon ',NCCODE(I)
          write(comment,'(2a)') 'reference for ',zname(I)
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        endif
        write(tokens,'(2a)') '*opr ',LPROJ(I)(1:LNBLNK(LPROJ(I)))
        write(comment,'(a)') 'schedules'
        if(.NOT.cmtopr)then
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
          cmtopr=.true.      ! only show 1st instance of comment
        else
          write(IFCFG,'(a)') tokens(1:lnblnk(tokens))
        endif
        write(tokens,'(2a)') '*geo ',LGEOM(I)(1:LNBLNK(LGEOM(I)))
        write(comment,'(a)') 'geometry'
        if(.NOT.cmtgeo)then
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
          cmtgeo=.true.      ! only show 1st instance of comment
        else
          write(IFCFG,'(a)') tokens(1:lnblnk(tokens))
        endif
        write(tokens,'(2a)') '*con ',LTHRM(I)(1:LNBLNK(LTHRM(I)))
        write(comment,'(a)') 'constructions'
        if(.NOT.cmtcon)then
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
          cmtcon=.true.      ! only show 1st instance of comment
        else
          write(IFCFG,'(a)') tokens(1:lnblnk(tokens))
        endif
        if(IOBS(I).EQ.1)then
          write(tokens,'(2a)') '*obs ',ZOBS(I)(1:lnblnk(ZOBS(I)))
          write(comment,'(a)') 'obstructions'
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        endif

C Newer format zone construction files embed tmc attributes. If
C so dereference the legacy tmc file.
        if(izconstv(i).ge.21)then
          continue
        else
          if(ITW(I).EQ.1)then
            write(tokens,'(2a)') '*tmc ',LTWIN(I)(1:lnblnk(LTWIN(I)))
            write(comment,'(a)') 'transparent constructions'
            if(.NOT.cmttmc)then
              call align_comment(48,tokens,comment,aligned_str)
              write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
              cmttmc=.true.      ! only show 1st instance of comment
            else
              write(IFCFG,'(a)') tokens(1:lnblnk(tokens))
            endif
          endif
        endif
        if(icfc(I).EQ.1)then
          write(tokens,'(2a)') '*cfc ',lcfcin(I)(1:lnblnk(lcfcin(I)))
          write(comment,'(a)') 'complex fenestration constructions'
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        endif
        if(IVF(I).EQ.1)then
          write(tokens,'(2a)') '*ivf ',LVIEW(I)(1:LNBLNK(LVIEW(I)))
          write(comment,'(a)') 'view factors'
          if(.NOT.cmtivf)then
            call align_comment(48,tokens,comment,aligned_str)
            write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
            cmtivf=.true.      ! only show 1st instance of comment
          else
            write(IFCFG,'(a)') tokens(1:lnblnk(tokens))
          endif
        endif
        if(ICGC(I).EQ.1)then
          write(tokens,'(2a)') '*cgc ',LCGCIN(I)(1:lnblnk(LCGCIN(I)))
          write(comment,'(a)') 'casual gain control'
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        endif
        if(ISI(I).EQ.1)then
          if(ISIcalc.eq.1)then
            write(LSHAD(I),'(A,I0,A)') '../tmp/Z',I,'.shd'
            write(tokens,'(2a)') '*ise ',LSHAD(I)(1:lnblnk(LSHAD(I)))
            write(comment,'(a)') 'embedded S/I calculation'
            if(.NOT.cmtisi)then
              call align_comment(48,tokens,comment,aligned_str)
              write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
              cmtisi=.true.      ! only show 1st instance of comment
            else
              write(IFCFG,'(a)') tokens(1:lnblnk(tokens))
            endif
          elseif(ISIcalc.eq.2)then
            write(tokens,'(2a)') '*isi ',LSHAD(I)(1:lnblnk(LSHAD(I)))
            write(comment,'(a)') 'pre-constructed S/I file'
            if(.NOT.cmtisi)then
              call align_comment(48,tokens,comment,aligned_str)
              write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
              cmtisi=.true.      ! only show 1st instance of comment
            else
              write(IFCFG,'(a)') tokens(1:lnblnk(tokens))
            endif
          endif
        endif

C Zone convection coefficient file.
        if(IHC(I).EQ.1)then
          write(tokens,'(2a)') '*ihc ',LHCCO(I)(1:lnblnk(LHCCO(I)))
          write(comment,'(a)')
     &      'convective heat transfer coefficient file'
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        endif

C Zone CFD file.
        if(lnblnk(LCFD(I)).eq.0)then
          continue
        elseif(LCFD(I)(1:7).eq.'UNKNOWN'.or.LCFD(I)(1:2).eq.'  ')then
          continue
        else
          write(tokens,'(2a)') '*cfd ',LCFD(I)(1:lnblnk(LCFD(I)))
          write(comment,'(a)') 'cfd domain file'
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        endif

C 3D zone files.
        if(ZONE3D(I))then
          write(tokens,'(2a)') '*cvs ',L3DCVS(I)(1:lnblnk(L3DCVS(I)))
          write(comment,'(a)') '3D volumes file'
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
          write(tokens,'(2a)') '*cnc ',L3DCNC(I)(1:lnblnk(L3DCNC(I)))
          write(comment,'(a)') '3D connections file'
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
          write(tokens,'(2a)') '*ndc ',L3DNDC(I)(1:lnblnk(L3DNDC(I)))
          write(comment,'(a)') '3D coordinates file'
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
          write(tokens,'(2a)') '*3dt ',L3DTAQ(I)(1:lnblnk(L3DTAQ(I)))
          write(comment,'(a)') '3D temperatures file'
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        endif

C Zone construction moisture file.
        if(MSTRZN(I))then
          write(tokens,'(2a)') '*mst ',LMOIST(I)(1:lnblnk(LMOIST(I)))
          write(comment,'(a)') 'construction moisture file'
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        endif

C Construction structured mesh file.
        if(IndxSt.EQ.I)then
          write(tokens,'(2a)') '*csm ',LGrdSt(1:lnblnk(LGrdSt))
          write(comment,'(a)') 'construction structured mesh file'
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        endif

C SHOCC (sub-hour occupancy file).
        if(bZoneSHOCCed(I))then
          write(tokens,'(2a)') 
     &      '*shz ',SHOCCshzFile(I)(1:lnblnk(SHOCCshzFile(I)))
          write(comment,'(a)') 'SHOCC file'
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        endif

C BASEIMP file.
        if(iBSIMP(I).EQ.1)then
          write(tokens,'(2a)') '*bsm ',LBSIMP(I)(1:lnblnk(LBSIMP(I)))
          write(comment,'(a)') 'BASESIMP file'
          call align_comment(48,tokens,comment,aligned_str)
          write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        endif

C Zone files end marker.
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2)'*zend '
  920 CONTINUE

C Zone surface connections file.
      write(tokens,'(2a)') '*cnn ',LCNN(1:lnblnk(LCNN))
      write(comment,'(a)') 'surface connections file'
      call align_comment(48,tokens,comment,aligned_str)
      write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))

C Re-build zone surface-to-connection hash array.
      do 43 icc = 1, NCON
        IZSTOCN(IC1(icc),IE1(icc))=icc
 43   continue

C Zone anchor points.
      if(NALOC.gt.0)then
        WRITE(IFCFG,'(A,i2)',IOSTAT=IOS,ERR=3) '*Anchor ',NALOC
        do 44 ia=1,NALOC
          WRITE(IFCFG,'(3A,i3)',IOSTAT=IOS,ERR=3) ALOCLBL(ia),' ',
     &       ALOCTYP(ia),IALOC(ia)

C Packed string of items associated with each anchor.
          do 45 L=1,IALOC(ia)
            iav(l)=lstanchr(ia,l)
  45      continue
          itrunc=1
          ipos=1
          do while (itrunc.ne.0)
            call ailist(ipos,IALOC(ia),iav,99,'S',outs,loutlen,itrunc)
            write(IFCFG,'(1x,a)',IOSTAT=ios,ERR=3) outs(1:loutlen)
            ipos=itrunc+1
          end do
 44     continue
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=3) '*End'
      endif

C Zone groups.
      if(nzgroup.gt.0)then
        WRITE(IFCFG,'(A,i2)',IOSTAT=IOS,ERR=3) '*Group ',nzgroup
        do ia=1,nzgroup
          lnzglbl=lnblnk(zglbl(ia))
          WRITE(IFCFG,'(2A,i3)',IOSTAT=IOS,ERR=3) zglbl(ia)(1:lnzglbl),
     &      ', ',izgnumber(ia)
          do L=1,izgnumber(ia)
            iav(l)=izglist(ia,l)
          enddo
          itrunc=1
          ipos=1
          do while (itrunc.ne.0)
            call ailist(ipos,izgnumber(ia),iav,40,'S',outs,loutlen,
     &        itrunc)
            write(IFCFG,'(1x,a)',IOSTAT=ios,ERR=3) outs(1:loutlen)
            ipos=itrunc+1
          end do
        enddo
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=3) '*End'
      endif

C Fluid flow network file.
      if(IAIRN.EQ.0)then
        write(tokens,'(i4)') IAIRN
        write(comment,'(a)') 'no fluid flow network'
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      elseif(IAIRN.ge.1)then
        write(tokens,'(i4)') IAIRN
        write(comment,'(a)') 'fluid flow network'
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
      endif
      IF(IAIRN.ge.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)
     &    LAPROB(1:LNBLNK(LAPROB)),' # flow file network file'
        do 777 j=1,ncomp
          if(ICAAS(J).ne.0)then
            namen(j)=NDNAM(ICAAS(J)) ! 0 means no association exists
          else
            namen(j)='0'
          endif
  777   continue
        itrunc=1
        ipos=1
        do while (itrunc.ne.0)
          lnouts=1
          call aslist(ipos,ncomp,namen,MCOM,'C',outs,lnouts,itrunc)
          write(IFCFG,'(1x,a)',IOSTAT=ios,ERR=2) outs(1:lnouts)
          ipos=itrunc+1
        end do

      endif

C Plant network.
 9021 if(INDCFG.EQ.2.or.INDCFG.EQ.3)then
        WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2) '* Plant'
        write(tokens,'(a)') LPNF(1:lnblnk(LPNF))
        write(comment,'(a)') 'plant network file'
        call align_comment(48,tokens,comment,aligned_str)
        write(IFCFG,'(a)') aligned_str(1:lnblnk(aligned_str))
        if(BPF(1:3).EQ.'../')then
          WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2) '* Boundary'
          WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2)BPF
        endif
        if(INDCFG.EQ.2)then
          WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2) 
     &      modeltitle(1:lnblnk(modeltitle))
        endif
      endif

C FMI.
      if (is_FMU.and.FMISFL(1:7).ne.'UNKNOWN') then
        write(IFCFG,'(a,a,a)',IOSTAT=IOS,ERR=2)
     &    '*FMI ',FMISFL(1:lnblnk(FMISFL)),'  # FMI specification file'
      endif
      
C Agents.
      if (IS_AGT.and.AGTFIL(1:7).ne.'UNKNOWN') then
        write(IFCFG,'(a,a,a)',IOSTAT=IOS,ERR=2)
     &    '*agent ',AGTFIL(1:lnblnk(AGTFIL)),'  # agent file'
      endif

C Gremlins.
      if (IS_GRM.and.GRMFIL(1:7).ne.'UNKNOWN') then
        write(IFCFG,'(a,a,a)',IOSTAT=IOS,ERR=2)
     &    '*gremlin ',GRMFIL(1:lnblnk(GRMFIL)),'  # gremlin file'
      endif

C Close the system configration file.
 9028 CALL ERPFREE(IFCFG,IOS)

C Write the zone connections file.
      if(ncon.gt.0)then
        CALL EFOPSEQ(IUF,LCNN,3,IER)
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=2) '*connections for ',
     &    cfgroot(1:lnblnk(cfgroot))
        call dstamp(dstmp)
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3) '*Date ',dstmp
        WRITE(IUF,'(I7,A)',IOSTAT=IOS,ERR=2)
     &    NCON,'  # number of connections'
        DO 930 I = 1,NCON
          CALL CONXINF(1,I,CXSTR)
          WRITE(IUF,'(5I4,2A)',IOSTAT=IOS,ERR=2) IC1(I),IE1(I),
     &      ICT(I),IC2(I),IE2(I),'  # ',CXSTR(1:lnblnk(CXSTR))
          IF(ICT(I).NE.4.OR.IC2(I).GT.0)GOTO 930
          if(IE2(I).GT.NGRDP)then
            CALL edisp(iuout,' ')
            CALL edisp(iuout,
     &      'Ground connection referenced an unknown monthly profile!')
          endif
  930   CONTINUE
        CALL ERPFREE(IUF,IOS)   ! free the connections file
      endif

      RETURN

C Error messages.
    2 CALL USRMSG(' ','Problem writing data!','W')
      GOTO 9028
    3 if(IOS.eq.2)then
        CALL USRMSG(' ',
     &    'Not permitted to write configuration file.','W')
        GOTO 9028
      else
        CALL USRMSG(' ','Problem writing configuration name','W')
        GOTO 9028
      endif
  146 if(IOS.eq.2)then
        CALL USRMSG('Not permitted to write bi-directional optics',
     &    'list to configuration file.','W')
        GOTO 9028
      else
        CALL USRMSG('Problem writing bi-directional optics list',
     &    'to configuration file.','W')
        GOTO 9028
      endif

      END


C ******************** CONXINF ********************
C Returns a description in CXSTR of the inter-connection 
C information after reading the system configuration file.
C If ICF=1 then display with zone and surface names. If
C ICON=0 then generate a text heading.

      SUBROUTINE CONXINF(ICF,ICON,CXSTR)
#include "building.h"
#include "geometry.h"
      
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      INTEGER :: IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      CHARACTER CXSTR*78,SST1*12,ZST1*12,SST2*12,ZST2*12
      integer lnblnk  ! function definition

      IF(ICON.EQ.0.AND.ICF.GT.0)THEN
        WRITE(CXSTR,8986)  
 8986   FORMAT('Con   Origin surface               Other side')    
        RETURN
      ENDIF

C Trap out of range IC1 and IE1 variables.
      if(IC1(ICON).EQ.0)then
        CXSTR='  '
        RETURN
      endif
      if(IE1(ICON).EQ.0)then
        CXSTR='  '
        RETURN
      endif

      SST1=SNAME(IC1(icon),IE1(icon))
      lsn1=lnblnk(SST1)
      ZST1=ZNAME(IC1(ICON))
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
 8983   FORMAT(I4,1X,a,' in ',a,' >|< Constant @',I3,' dC & ',
     &         I4,' W rad')
      ELSEIF(ICT(ICON).EQ.3.AND.ICF.GT.0)THEN
        ICM=IZSTOCN(IC2(ICON),IE2(ICON))
        if(ICM.gt.0)then
          SST2=SNAME(IC2(ICON),IE2(ICON))
          lsn2=lnblnk(SST2)
          ZST2=zname(IC2(ICON))
          lzn2=lnblnk(ZST2)
        else
          SST2='not_known'
          lsn2=lnblnk(SST2)
          ZST2='not_known'
          lzn2=lnblnk(ZST2)
        endif
        WRITE(CXSTR,8982)ICON,SST1(1:lsn1),ZST1(1:lzn1),
     &                        SST2(1:lsn2),ZST2(1:lzn2)
 8982   FORMAT(I4,1X,a,' in ',a,' >|< ',a,' in ',a)
      ELSEIF(ICT(ICON).EQ.4.AND.ICF.GT.0)THEN
        IF(IC2(ICON).GT.0)THEN
          WRITE(CXSTR,8981)ICON,SST1(1:lsn1),
     &                     ZST1(1:lzn1),IC2(ICON)
 8981     FORMAT(I4,1X,a,' in ',a,' >|< ground profile',I2)
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
        WRITE(CXSTR,7986)ICON,SST1(1:lsn1),ZST1(1:lzn1)
 7986   FORMAT(I4,1X,a,' in ',a,' is BASESIMP fndtn')

      ELSEIF(ICT(ICON).EQ.7.AND.ICF.GT.0)THEN

        if(IC2(ICON).eq.0.and.IE2(ICON).eq.0)then
          WRITE(CXSTR,9984)ICON,SST1(1:lsn1),ZST1(1:lzn1)
 9984     FORMAT(I4,1X,a,' in ',a,' >|< Identical CEN 13791   ')
        else
          WRITE(CXSTR,9987)ICON,SST1(1:lsn1),
     &      ZST1(1:lzn1),IC2(ICON),IE2(ICON)
 9987     FORMAT(I4,1X,a,' in ',a,' >|< CEN13791+-',i3,'dC & ',
     &      I4,' W rad')
        endif
      ENDIF

      RETURN
      END


C ******************** ckcurmatch ********************
C Scan and report mis-matches.

      subroutine ckcurmatch(prob,iprob)
#include "building.h"
#include "geometry.h"

C Parameters
      logical prob  ! true if there is a problem
      integer iprob ! number of mis-matches detected

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      logical dup,first

      CHARACTER CXSTR*78
      character msg*124

      IPROB=0
      ICC=0
      prob=.false.
      first=.false.
      do 287 IZ=1,NCOMP
        do 288 IS=1,nzsur(iz)
          ICC=ICC+1
          if(IC1(ICC).ne.IZ)then

C If this is the first warning, print out heading.
            if(.NOT.first)then
              CALL CONXINF(1,0,CXSTR)
              CALL EDISP(iuout,CXSTR)
              first=.true.
            endif
            call edisp(iuout,'Current zone does not match contiguity.')
            CALL CONXINF(1,ICC,CXSTR)
            CALL EDISP(iuout,CXSTR)
            prob=.true.
            IPROB=IPROB+1
          endif
          if(IE1(ICC).ne.IS)then
            if(.NOT.first)then
              CALL CONXINF(1,0,CXSTR)
              CALL EDISP(iuout,CXSTR)
              first=.true.
            endif
            write(msg,'(a,2i3,a,i3,a,i5)') 'Current surface ',IZ,IS,
     &        ' does not match connection surface',IE1(ICC),
     &        ' for connection ',ICC
            call edisp(iuout,msg)
            CALL CONXINF(1,ICC,CXSTR)
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
                  CALL CONXINF(1,0,CXSTR)
                  CALL EDISP(iuout,CXSTR)
                  first=.true.
                endif
                call edisp(iuout,
     &            'One side of construction is not linked!')
                CALL CONXINF(1,i,CXSTR)
                CALL EDISP(iuout,CXSTR)
                CALL CONXINF(1,ioc,CXSTR)
                CALL EDISP(iuout,CXSTR)
                prob=.true.
                IPROB=IPROB+1
               elseif(IC2(ioc).ne.IC1(i))then
                if(.NOT.first)then
                  CALL CONXINF(1,0,CXSTR)
                  CALL EDISP(iuout,CXSTR)
                  first=.true.
                endif
                call edisp(iuout,'Contiguous zone link >|< incorrect!')
                prob=.true.
                IPROB=IPROB+1
                CALL CONXINF(1,i,CXSTR)
                CALL EDISP(iuout,CXSTR)
                CALL CONXINF(1,ioc,CXSTR)
                CALL EDISP(iuout,CXSTR)
              elseif(IE2(ioc).ne.IE1(i))then
                if(.NOT.first)then
                  CALL CONXINF(1,0,CXSTR)
                  CALL EDISP(iuout,CXSTR)
                  first=.true.
                endif
               call edisp(iuout,
     &            'Contiguous surface link >|< incorrect!')
                prob=.true.
                IPROB=IPROB+1
                CALL CONXINF(1,i,CXSTR)
                CALL EDISP(iuout,CXSTR)
                CALL CONXINF(1,ioc,CXSTR)
                CALL EDISP(iuout,CXSTR)
              endif

C Check to see if this connection is only pointed to once by an
C 'another zone' connection in the list.
              dup=.false.
              do 291 i2=1,NCON
                if(IC2(i2).eq.IC1(i).and.IE2(i2).eq.IE1(i))then
                  if(.NOT.dup)then
                    if(ICT(i2).eq.3)dup=.true.
                  else
                    if(.NOT.first)then
                      CALL CONXINF(1,0,CXSTR)
                      CALL EDISP(iuout,CXSTR)
                      first=.true.
                    endif
                    call edisp(iuout,'Contiguity 2nd reference found!')
                    CALL CONXINF(1,i,CXSTR)
                    CALL EDISP(iuout,CXSTR)
                    CALL CONXINF(1,ioc,CXSTR)
                    CALL EDISP(iuout,CXSTR)
                    CALL CONXINF(1,i2,CXSTR)
                    CALL EDISP(iuout,CXSTR)
                    prob=.true.
                    IPROB=IPROB+1
                  endif
                endif
  291         continue
            endif
          else
            if(.NOT.first)then
              CALL CONXINF(1,0,CXSTR)
              CALL EDISP(iuout,CXSTR)
              first=.true.
            endif
            call edisp(iuout,
     &          'Contiguity other side link out of range!')
            CALL CONXINF(1,i,CXSTR)
            CALL EDISP(iuout,CXSTR)
            prob=.true.
            IPROB=IPROB+1
          endif
        endif
  290 continue
      return
      end
