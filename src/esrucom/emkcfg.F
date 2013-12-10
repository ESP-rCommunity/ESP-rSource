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

C emkcfg.F includes the following:
C  EMKCFG  Creates a system configuration file.
C  CONXINF Returns inter-connection descr in CXSTR (copy)
C  ckcurmatch Scan and report current connection mis-matches.

C **************** EMKCFG 
C EMKCFG writes out the current contents of system configuration
C common blocks into the file and unit specified in COMMON/C21.
      SUBROUTINE EMKCFG(act,IER)
      
      USE AIM2_InputData, ONLY:LAIM2,iAIM2
      
      IMPLICIT NONE

#include "building.h"
#include "model.h"
#include "site.h"
#include "geometry.h"
#include "net_flow.h"
#include "tdf2.h"
#include "uncertainty.h"
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

      INTEGER, PARAMETER :: MSTMC=20
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: iuout,iuin
      common/pophelp/h(60)
      
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

C Configuration control.
      common/cctlnm/ctldoc,lctlf

C Images.
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
      REAL :: angi,EYEMI,VIEWMI
      INTEGER :: initvt

C Special materials file flag
      common/spmfxst/ispmxist,spflnam
      common/entfile/entflnam,ientxist
      INTEGER :: ispmxist,ientxist

      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      COMMON/MFLOW2/NDNAM(0:MNOD)

C Non-linear thermophysical properties.
      COMMON/VTHP18/LNLTHP
      COMMON/VTHP31/INTHPS,INTHPZ(MCOM)

C 1D node distribution.
      COMMON/GR1D05/LGRD1D
      COMMON/GR1D06/IGR1D

C 3D ground.
      COMMON/GRND100/GRND3D
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD

C Ground topology.
      COMMON/GTFIL/GTGEOM

C CFD
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      INTEGER :: ifcfd

C 3D zones.
      common/GR3D100/BLDG3D,ZONE3D(MCOM)
      common/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)

C Moisture.
      common/MOIST01/MSTROK,MSTRZN(MCOM)
      common/MOIST02/LMOIST(MCOM)

C Structured mesh.
      COMMON/GRSD100/IndxSt
      INTEGER :: IndxSt
      COMMON/GRSD101/LGrdSt

C Temporal definition commons.
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
      common/IPVF/lipvdatf

C CPCALC description
      common/CPCALC/icpcon,ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,
     &              sbh,pad,wvpe
      REAL :: ble,bwi,bhi,blox,bloy,bloz,orient,ra,sbh,pad,wvpe
      INTEGER :: icpcon,irt

C Variables for weekdays, and weekends.
C Assume: Mon=1, Tue=2, Wed=3, Thu=4, Fri=5, Sat=6, Sun=7
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2
      INTEGER :: idwe1,idwe2

C Strings associated with one climatelist item.
C clmlnam (char*32) is the selection list string
C clmlaid is aide (char*72) memoire for list item
C clmldbfile (char*144) is the climate file (with path info)
C clmlavail (char*10) is either ONLINE or OFFLINE
C clmlhelp (char*72) is array of help strings
      character clmlnam*32,clmlaid*72,clmldbfile*144,clmlavail*10
      character clmlhelp*72
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)

C Numbers associated with one climatelist item.
      common/clmlnum/nbclmlhelp
      INTEGER :: nbclmlhelp

C Uncertainty analysis common.
      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)

C Anchor data in model.h.

C Simulation parameter presets.
      common/spfldes/spfdescr(MSPS)
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      INTEGER :: isstday,isstmon,isfnday,isfnmon
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      INTEGER :: nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh      
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres

C BASESIMP.
      common/bsmtdef/iBSIMP(MCOM),LBSIMP(MCOM)
      INTEGER :: ibsimp
C HVAC
C Flag indicating presence of HVAC models:
C ihvacflag=1 indicates HVAC models are active; 0 indicates no HVAC models.
      common/hvacinfo/ihvacflag,hvacfile
      INTEGER :: ihvacflag

C HRV Inputs
      Integer iMech
      Character *72 LMech
      Common /mechvdef/ iMech, LMech

C Contaminant flow model commons
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM/CNTMFIL,CNTMDESC,NTSTEPC
      INTEGER :: NCONTM,NOCNTM,NTSTEPC

C Path to SHOCC input files
C bSHOCCed     - logical flag whether project has SHOCC input
C bZoneSHOCCed - logical array whether zones have SHOCC input
C SHOCCshlFile - path to SHOCC .shl file for SHOCC'd project library
C SHOCCshzFile - path to SHOCC .shz file for each SHOCC'd zone
      common/SHOCCcfg/bSHOCCed,SHOCCshlFile,bZoneSHOCCed(mcom),
     &                SHOCCshzFile(mcom)
      logical bSHOCCed,bZoneSHOCCed
      character SHOCCshlFile*72,SHOCCshzFile*72
      

C Fuel cell.
      common/Fcell/iFClds,LFClds
      INTEGER :: iFClds

C External longwave radiation used by HOT3000.
      COMMON/LongRad/iExtLgRadFlag,eGrdTp
      INTEGER :: iExtLgRadFlag
      REAL :: eGrdTp(12)
      INTEGER jMon

C GSHP
       common/gshpinfo/igshp
       common/gshpfile/gshpfile
       INTEGER igshp  
       character gshpfile*72 

C Wind-generated electricity
      common/H2wind/LWndSupp,iWndSupp
      INTEGER :: iWndSupp

C GCEP
       common/gcepinfo/igcep
       common/gcepfile/gcepfile
       INTEGER igcep  
       character gcepfile*72 

C Bidirectional raw optical data file.
      COMMON/BIDIR/IFLAGBI,INTVALBI,NSTMCFL(MCON)
      INTEGER :: IFLAGBI,INTVALBI,NSTMCFL
      COMMON/BIDIRFL/bidirfile,bidirname(MSTMC)

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER :: icalender,nbcaldays,nbdaytype

C iSlr_half_hr_flg value = 0 :hour-centered; =1 : half-hour centered.
      common/CWEC_SOL/iSlr_half_hr_flg
      integer iSlr_half_hr_flg     

C When iexcludediff_flag is 1, diffuse shading is excluded. 
C If 0 then the calculation includes diffuse shading.      
      integer iexcludediff_flag
      common/excludediff/iexcludediff_flag

      CHARACTER*72 LBSIMP,hvacfile,LFClds

      CHARACTER LAPROB*72
      CHARACTER CXSTR*78
      CHARACTER LPNF*72
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDNDD,LGDTAQ,LGRD1D,LNLTHP
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,LMOIST,LGrdSt
      CHARACTER ctldoc*248,LCTLF*72,H*72
      CHARACTER spflnam*72,entflnam*72
      character GTGEOM*72,LCFD*72,DCNN*72
      CHARACTER DBTAG*12,DBTASK*12,DBZN*15,indxcmt*24,DBSN*15
      character*10 wkd1, wkd2
      character LUALF*72,LCNG*15,LLOC*15,lipvdatf*72
      CHARACTER outs*124
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,spfdescr*8,ltmp*72,louts*144
      character bidirfile*72,bidirname*12
      character calename*32,calentag*12,calendayname*32,loutc*96
      character LWndSupp*72
      CHARACTER CNTMFIL*72,CONTMNAM*12,CNTMDESC*124
      character descr*7,descrst*10,descrfn*10  ! for season dates
      character descrst1*10,descrfn1*10        ! for 2nd season dates
      
C An array of mass flow nodes for writing.
      CHARACTER NDNAM*12
      character namen*12
      dimension namen(mcom)

      character act*1  ! a '-' is normal use, a 's' is silent
      character ipvaction*3  ! to instruct mkipv to do it within cfg file
      character tab*1  ! a literal tab character

      dimension iav(99)

      logical dogrnd
      LOGICAL INTHPS,INTHPZ,IGR1D,GRND3D
      LOGICAL BLDG3D,ZONE3D,MSTROK,MSTRZN
      character dstmp*24
      character ALOC*12

C Control for writing out multi-climate sim presets
      logical bMY_set_open 

      integer itrunc  ! to indicate there is a continuation line.
      integer ipos    ! position in flow node string buffer
      integer lnouts   ! location at end of flow node string buffer
      integer lnipv   ! location at end of the external IPV file
      
      INTEGER :: ier,i,ia,iav,icc,icd,id1,id2,id3,id4,im1,im2,im3,im4
      INTEGER :: ij,img,ios,iss,iuf,iw,j,l,lcfgl,lcfgr,lct,lctn,loutlen
      INTEGER :: loutln,ncn
      
      IUF=IFIL+1
      IER=0

C Open any existing file by this name or create a new file.
      CALL EFOPSEQ(IFCFG,LCFGF,3,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

C Remind that newer version of the file can be written. (duplicae of
C code in edcfg to trap updates following on from other modifications).
      if(icfgv.lt.3)then
        h(1)='The version 3 format merges the contents of the zone '
        h(2)='utility files into the configuration file. Models '
        h(3)='created or updated after 1996 should use the V3. '
        h(4)=' '
        h(5)='The version 4 format includes within the cfg file '
        h(6)='some data held in separate files and uses newer '
        h(7)='(free format) version of zone geometry files.'
        h(8)=' '
        h(9)='Both V3 and V4 files can support newer format '
        h(10)='geometry and obstruction data. '
        CALL EASKAB(' Configuration file loaded was in old format.',
     &    ' save in:','current format','cancel',IW,10)
        if(IW.EQ.2)then
          call edisp(iuout,'Configuration not being updated.')
          return
        elseif(IW.EQ.1)then

C If no model root name has been set, make from configuration file.
C If filename > 4 char it might have a .cfg attached.
          icfgv=4     ! if upgrading use version 4
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
              write(cfgroot,'(a)') LCFGF(1:lcfgr)
            endif
          endif

C Also check about connections file.
          if(ncon.gt.1)then
  290       H(1)='The surface topology of the model is held'
            H(2)='in a connections file.'
            write(LCNN,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.cnn'
            DCNN=LCNN
            ltmp=LCNN
            CALL EASKS(ltmp,' Connections file name?',' ',
     &        72,DCNN,'system connx file name',IER,2)
            if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
              LCNN=ltmp
            else
              GOTO 290
            endif
          elseif(ncon.eq.0.and.indcfg.gt.0)then

C If emkcfg was called from ecnv with an initial registration model
C that had no zones but where indcfg had already been reset to one
C then we need to know the name of the connections file.
            write(LCNN,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.cnn'
          endif
        endif
      elseif(icfgv.ge.3)then

C Also check about connections file if in interactive mode.
        if(ncon.gt.1)then
          if(act.eq.'-'.and.LCNN(1:4).eq.'UNKN')then
  292       H(1)='The surface topology of the model is held'
            H(2)='in a connections file.'
            write(LCNN,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.cnn'
            DCNN=LCNN
            ltmp=LCNN
            CALL EASKS(ltmp,' Connections file name?',' ',
     &        72,DCNN,'system connx file name',IER,2)
            if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
              LCNN=ltmp
            else
              GOTO 292
            endif
          endif
        elseif(ncon.eq.0.and.indcfg.gt.0)then

C If emkcfg was called from ecnv with an initial registration model
C that had no zones but where indcfg had already been reset to one
C then we need to know the name of the connections file.
          write(LCNN,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.cnn'
        endif
      endif

C Test to see if model needs to be updated from registration
C to some other condition.  If first zones added the check where to
C put the connections. If act is '-' then ask, if act is 's' then
C take the default name for the connections file.
      if(INDCFG.EQ.0)then
        if(NCOMP.eq.1.and.LGEOM(1)(1:4).ne.'dumm')then
          INDCFG=1
C          call edisp(iuout,'Upgrading from registration to zone.')
          write(LCNN,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.cnn'
          if(act(1:1).eq.'-')then
  289       H(1)='The surface topology of the model is held in'
            H(2)='a connections file.'
            DCNN='model.cnn'
            ltmp=LCNN
            CALL EASKS(ltmp,' Connections file name?',' ',72,
     &        DCNN,'system connx file name',IER,2)
            if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
              LCNN=ltmp
            else
              GOTO 289
            endif
          endif
          if(LPNF(1:4).ne.'UNKN'.and.LPNF(1:2).ne.'  ')then
            INDCFG=3
C            call edisp(iuout,'Upgrading from zone to zone + plant.')
          endif
        elseif(ncomp.gt.1)then
          INDCFG=1
C          call edisp(iuout,'Upgrading from registration to zone.')
          if(LPNF(1:4).ne.'UNKN'.and.LPNF(1:2).ne.'  ')then
            INDCFG=3
C            call edisp(iuout,'Upgrading from zone to zone + plant.')
          endif
        endif
      elseif(INDCFG.EQ.1)then
        if(LPNF(1:4).ne.'UNKN'.and.LPNF(1:2).ne.'  ')then
          INDCFG=3
C          call edisp(iuout,'Upgrading from zone to zone + plant.')
        endif
      endif

C Comment for configuration index
      IF(INDCFG.EQ.0) indxcmt=' # Project registration'
      IF(INDCFG.EQ.1)  indxcmt=' # Building only'
      IF(INDCFG.EQ.2)  indxcmt=' # Plant only'
      IF(INDCFG.EQ.3) indxcmt=' # Building & Plant'

C Begin writing the file with version number in the header.
      if(icfgv.eq.3)then
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=3) '* CONFIGURATION3.0'
      elseif(icfgv.eq.4)then
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=3) '* CONFIGURATION4.0'
      endif

      WRITE(IFCFG,30,IOSTAT=IOS,ERR=3) LCFGF(1:lnblnk(LCFGF))
      call dstamp(dstmp)
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*date ',dstmp,
     &  '  # latest file modification '
      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=3) '*root ',
     &  cfgroot(1:lnblnk(cfgroot))
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*zonpth  ',zonepth,
     &  '  # path to zones'
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*netpth  ',netpth,
     &  '  # path to networks'
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*ctlpth  ',ctlpth,
     &  '  # path to controls'
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*aimpth ',aimpth,
     &  '  # path to aim2 files'
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*radpth ',radpth,
     &  '  # path to radiance files'
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*imgpth  ',imgpth,
     &  '  # path to project images'
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*docpth  ',docpth,
     &  '  # path to project documents'
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*dbspth  ',dbspth,
     &  '  # path to local databases'
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*hvacpth ',hvacpth,
     &  '  # path to hvac files'
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*bsmpth ',bsmpth,
     &  '  # path to BASESIMP files'
      WRITE(IFCFG,'(A,i4,a)',IOSTAT=IOS,ERR=3) '*indx ',INDCFG,
     &  indxcmt(1:lnblnk(indxcmt))
  30  FORMAT('# ESRU system configuration defined by file ',/,'# ',A)

      WRITE(IFCFG,800,IOSTAT=IOS,ERR=2) sitelat,sitelongdif
 800  FORMAT(F7.3,2X,F7.3,
     &  '   # Latitude & Longitude (diff from time meridian)')

      WRITE(IFCFG,8001,IOSTAT=IOS,ERR=2) siteexposureindex,groundrefl
8001  FORMAT(I7,2X,F6.3,'   # Site exposure & ground reflectivity')
      IF(siteexposureindex.EQ.8)
     &  WRITE(IFCFG,'(3F8.3,A)',IOSTAT=IOS,ERR=2)
     &    skyview,groundview,buildingview,
     &    '   # views to sky ground buildings'

C Write databases related to this model.
      WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) '* DATABASES'

C Material database has the option of two different syntax to
C reflect whether the standard database folder is being used.
      if(ipathmat.eq.0.or.ipathmat.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*mat  ',
     &      LFMAT(1:lnblnk(LFMAT))
      elseif(ipathmat.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdmat  ',
     &      LFMAT(1:lnblnk(LFMAT))
      endif

C CFClayers database has the option of two different syntax to
C reflect whether the standard database folder is being used.
      if(ipathcfc.eq.0.or.ipathcfc.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*cfcdb  ',
     &      LCFCDB(1:lnblnk(LCFCDB))
      elseif(ipathcfc.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdcfcdb  ',
     &      LCFCDB(1:lnblnk(LCFCDB))
      endif

C MLC database has the option of two different syntax.
      if(ipathmul.eq.0.or.ipathmul.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*mlc  ',
     &      LFMUL(1:lnblnk(LFMUL))
      elseif(ipathmul.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdmlc  ',
     &      LFMUL(1:lnblnk(LFMUL))
      endif

C Optical database has the option of two different syntax to
C reflect whether the standard database folder is being used.
      if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*opt  ',
     &      LOPTDB(1:lnblnk(LOPTDB))
      elseif(ipathoptdb.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdopt  ',
     &      LOPTDB(1:lnblnk(LOPTDB))
      endif

C Pressure coef database has the option of two different syntax to
C reflect whether the standard database folder is being used.
      if(ipathapres.eq.0.or.ipathapres.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*prs  ',
     &      LAPRES(1:lnblnk(LAPRES))
      elseif(ipathapres.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdprs  ',
     &      LAPRES(1:lnblnk(LAPRES))
      endif
      if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*evn  ',
     &      LPRFDB(1:lnblnk(LPRFDB))
      elseif(ipathprodb.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdevn  ',
     &      LPRFDB(1:lnblnk(LPRFDB))
      endif

      if(ipathclim.eq.0.or.ipathclim.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*clm  ',
     &      LCLIM(1:lnblnk(LCLIM))
      elseif(ipathclim.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdclm  ',
     &      LCLIM(1:lnblnk(LCLIM))
      endif

C      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*clm  ',
C     &      LCLIM(1:lnblnk(LCLIM))

      if(ipathmsc.eq.0.or.ipathmsc.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*mscldb  ',
     &      MCMPDBFL(1:lnblnk(MCMPDBFL))
      elseif(ipathmsc.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdmscldb  ',
     &      MCMPDBFL(1:lnblnk(MCMPDBFL))
      endif
      if(ipathmould.eq.0.or.ipathmould.eq.1)then
        WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2) '*mould  ',
     &    lfmould(1:lnblnk(lfmould)),' # mould isopleths'
      elseif(ipathmould.eq.2)then
        WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2) '*stdmould  ',
     &    lfmould(1:lnblnk(lfmould)),' # mould isopleths'
      endif

C Ground reflectivity model, no-snow monthly albedos, snow albedo
C (if ground reflectivity model 2 or 3)
      IF(groundreflmodel.GT.1) WRITE(IFCFG,803) '*gref ',
     &   groundreflmodel,(groundreflmonth(I),I=1,12),snowgroundrefl
  803 FORMAT(A,I1,13(1X,F5.3))

C Number of days with snow on the ground 
C (if ground reflectivity model 2)
      IF(groundreflmodel.EQ.2)  
     &  WRITE(IFCFG,805) '*snow',(dayswithsnow(I),I=1,12)
  805 FORMAT(A,12(1X,I2))

C Snow depth file
C (if ground reflectivity model 3)
      IF(groundreflmodel.EQ.3) WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) 
     &  '*sndf ',SNFNAM(1:lnblnk(SNFNAM))

C Multiyear climate database
      if ( bMY_climates_defined ) then 
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*myclm ',
     &    cMY_climate_db_name(1:lnblnk(cMY_climate_db_name))
      endif

C Plant database has the option of two different syntax to
C reflect whether the standard database folder is being used.
      if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*pdb  ',
     &      LPCDB(1:lnblnk(LPCDB))
      elseif(ipathpcdb.eq.2)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*stdpdb  ',
     &      LPCDB(1:lnblnk(LPCDB))
      endif

C UK SBEM database has the option of two different syntax to
C reflect whether the standard database folder is being used.
C      IF(ISBEM.GE.1)THEN
        if(ipathsbem.eq.0.or.ipathsbem.eq.1)then
          WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*sbem  ',
     &      LSBEM(1:lnblnk(LSBEM))
        elseif(ipathsbem.eq.2)then
          WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*stdsbem  ',
     &      LSBEM(1:lnblnk(LSBEM))
        endif
C      ENDIF

C HVAC
      IF (ihvacflag.EQ.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*hvac  ',
     &    hvacfile(1:lnblnk(hvacfile))
      endif

C DHW
      IF (iDHW_FLAG.EQ.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*dhw  ',
     &    sDHW_InputFileName(1:lnblnk(sDHW_InputFileName))
      endif

C AIM-2.
      IF (iAIM2.EQ.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*aim   ',
     &    LAIM2(1:lnblnk(LAIM2))
      endif

C MVNT
      IF (iMECH.EQ.1)then
        WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*mvnt   ',
     &    LMECH(1:lnblnk(LMECH))
      endif

C Fuel cell.
      IF (iFClds.EQ.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*fcl   ',
     &    LFClds(1:lnblnk(LFClds))
      endif

C GSHP
      IF (igshp.EQ.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*gshp  ',
     &    gshpfile(1:lnblnk(gshpfile))
      endif

C GCEP
      IF (igcep.EQ.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*gcep  ',
     &    gcepfile(1:lnblnk(gcepfile))
      endif

C Wind-generated electricity.
      if (iWndSupp.eq.1)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*wnd  ',
     &    LWndSupp(1:lnblnk(LWndSupp))
      endif

C Solar radiation (half hour data) flag.
      if(iSlr_half_hr_flg.eq.0)then
        WRITE(IFCFG,'(A,i2,A)',IOSTAT=IOS,ERR=2) '*slr_half_hr  ',
     &    iSlr_half_hr_flg,'  # solar timing hour centred'
      else
        WRITE(IFCFG,'(A,i2,A)',IOSTAT=IOS,ERR=2) '*slr_half_hr  ',
     &    iSlr_half_hr_flg,'  # solar timing half-hour centred'
      endif

C Diffuse shading flag.
      if(iexcludediff_flag.eq.1)then
        WRITE(IFCFG,'(A,i2,A)',IOSTAT=IOS,ERR=2) '*exclude_diff_shad  ',
     &    iexcludediff_flag,'  # diffuse shading is excluded'
      endif      
      
C Flag for quick run method	  
      if(iQuick_run.eq.0)then
        WRITE(IFCFG,'(A,i2,A)',IOSTAT=IOS,ERR=2) '*quick_run  ',
     &    iQuick_run,'  # no'
      else
        WRITE(IFCFG,'(A,i2,A)',IOSTAT=IOS,ERR=2) '*quick_run  ',
     &    iQuick_run,'  # yes'
      endif	  
	  
C SHOCC: library file.
      IF ( bSHOCCed ) then
         WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2) '*shl  ',
     &     SHOCCshlFile(1:lnblnk(SHOCCshlFile)),
     &     '  # SHOCC project library'
      endif

C BCD (boundary condition definiton file)
      if ( bBC_data_defined ) then
         WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2) '*bcd  ',
     &     cBC_data_file_name(1:lnblnk(cBC_data_file_name)),
     &     '  # BCD file name'
      endif 

C Lookup table data file
      if ( bLookup_data_defined ) then
         WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2) '*tab  ',
     &     cLookup_data_file_name
     &     (1:lnblnk(cLookup_data_file_name)),
     &     '  # Lookup table file name'
      endif 

C Write control file if known.
      if(lnblnk(lctlf).eq.0)then
        continue
      elseif(lctlf(1:7).eq.'UNKNOWN')then
        continue
      elseif(lctlf(1:2).eq.'  ')then
        continue
      else
        WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*ctl  ',
     &    LCTLF(1:lnblnk(LCTLF))
      endif

C Write model contents file if known.
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

C Write contaminant model description
      IF(NOCNTM.GT.0)WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     & '*ctm  ',CNTMFIL(1:lnblnk(CNTMFIL))

C Write initial view info if other than default.
      if(initvt.eq.1)then
        WRITE(IFCFG,'(a,6f8.1,f5.1)',IOSTAT=IOS,ERR=2)'*vew ',
     &    EYEMI(1),EYEMI(2),EYEMI(3),
     &    VIEWMI(1),VIEWMI(2),VIEWMI(3),ANGI
      endif

C User defined simulation toggles. 
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
      
C Write out calendar if defined
      IF(CALENAME(1:7).NE.'UNKNOWN')THEN
        write(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*calename ',
     &    calename(1:lnblnk(calename))
        do 443 icd = 1,nbdaytype
          lct=lnblnk(calentag(icd))
          lctn=lnblnk(calendayname(icd))
          write(IFCFG,'(5a,i3)',IOSTAT=IOS,ERR=2) '*calentag ',
     &      calentag(icd)(1:lct),', ',calendayname(icd)(1:lctn),', ',
     &      nbcaldays(icd)
 443    continue
        write(IFCFG,'(a,1I4)',IOSTAT=IOS,ERR=2) '*list ',nbdaytype
        itrunc=1
        ipos=1
        do while (itrunc.ne.0)
          call ailist(ipos,365,icalender,365,'C',loutc,loutlen,itrunc)
          write(IFCFG,'(1x,a)',IOSTAT=ios,ERR=2) loutc(1:loutlen)
          ipos=itrunc+1
        end do
        write(IFCFG,'(a)',IOSTAT=IOS,ERR=2) '*end_list'
      endif

C Changing orientation enabled if present
      IF(IROAM.EQ.1)THEN
         WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*roaming ',LROAM
      ENDIF

C Write uncertainty analysis library file if known.
      if(lnblnk(LUALF).eq.0)then
        continue
      elseif(LUALF(1:7).eq.'UNKNOWN')then
        continue
      else
        WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*ual  ',
     &    LUALF(1:lnblnk(LUALF))
      endif

C Write alternative definition of weekends if not default.
      if(idwe1.ne.6.or.idwe2.ne.7)then
        WRITE(IFCFG,'(a,2I3,a)',IOSTAT=IOS,ERR=2) '*weekend  ',
     &    idwe1,idwe2,' # alt weekend def'
      endif

C If there is a seasonal definition associated with the climate
C file (e.g. user defined or via climate list).  This block of
C code might be a << good one to place in a separate subroutine >>
C and used by various facilities.
      if(ihaveseason.gt.0)then
        tab=CHAR(9) 
        write(IFCFG,'(a)')'*seasons'
        write(IFCFG,'(3a)')'*name',tab,clmlnam(1:lnblnk(clmlnam))
        write(IFCFG,'(3a)')'*aide',tab,clmlaid(1:lnblnk(clmlaid))
        call edayr(is1wins,id1,im1)
        call edayr(is1winf,id2,im2)
        call edayr(is2wins,id3,im3)
        call edayr(is2winf,id4,im4)
        call stdate(iyear,is1wins,descr,descrst)
        call stdate(iyear,is1winf,descr,descrfn)
        call stdate(iyear,is2wins,descr,descrst1)
        call stdate(iyear,is2winf,descr,descrfn1)
        write(IFCFG,'(a,8i3,8a)')'*winter_s',id1,im1,id2,im2,id3,im3,
     &    id4,im4,' # seasons ',descrst,'--',descrfn,' & ',descrst1,
     &    '--',descrfn1
        call edayr(is1sprs,id1,im1)
        call edayr(is1sprf,id2,im2)
        call edayr(is2sprs,id3,im3)
        call edayr(is2sprf,id4,im4)
        call stdate(iyear,is1sprs,descr,descrst)
        call stdate(iyear,is1sprf,descr,descrfn)
        call stdate(iyear,is2sprs,descr,descrst1)
        call stdate(iyear,is2sprf,descr,descrfn1)
        write(IFCFG,'(a,8i3,8a)')'*spring_s',id1,im1,id2,im2,id3,im3,
     &    id4,im4,' # seasons ',descrst,'--',descrfn,' & ',descrst1,
     &    '--',descrfn1
        call edayr(is1sums,id1,im1)
        call edayr(is1sumf,id2,im2)
        call stdate(iyear,is1sums,descr,descrst)
        call stdate(iyear,is1sumf,descr,descrfn)
        write(IFCFG,'(a,4i3,4a)')'*summer_s',id1,im1,id2,im2,
     &    ' # summer season ',descrst,'--',descrfn
        call edayr(ia1wins,id1,im1)
        call edayr(ia1winf,id2,im2)
        call edayr(ia2wins,id3,im3)
        call edayr(ia2winf,id4,im4)
        call stdate(iyear,ia1wins,descr,descrst)
        call stdate(iyear,ia1winf,descr,descrfn)
        call stdate(iyear,ia2wins,descr,descrst1)
        call stdate(iyear,ia2winf,descr,descrfn1)
        write(IFCFG,'(a,8i3,8a)')'*winter_t',id1,im1,id2,im2,id3,im3,
     &    id4,im4,' # typical ',descrst,'--',descrfn,' & ',descrst1,
     &    '--',descrfn1
        call edayr(ia1sprs,id1,im1)
        call edayr(ia1sprf,id2,im2)
        call edayr(ia2sprs,id3,im3)
        call edayr(ia2sprf,id4,im4)
        call stdate(iyear,ia1sprs,descr,descrst)
        call stdate(iyear,ia1sprf,descr,descrfn)
        call stdate(iyear,ia2sprs,descr,descrst1)
        call stdate(iyear,ia2sprf,descr,descrfn1)
        write(IFCFG,'(a,8i3,8a)')'*spring_t',id1,im1,id2,im2,id3,im3,
     &    id4,im4,' # typical ',descrst,'--',descrfn,' & ',descrst1,
     &    '--',descrfn1
        call edayr(iasums,id1,im1)
        call edayr(iasumf,id2,im2)
        call stdate(iyear,iasums,descr,descrst)
        call stdate(iyear,iasumf,descr,descrfn)
        write(IFCFG,'(a,4i3,4a)')'*summer_t',id1,im1,id2,im2,
     &    ' # typical summer ',descrst,'--',descrfn
        write(IFCFG,'(a)')'*help_start'
        if(nbclmlhelp.eq.0)then
          write(IFCFG,'(a)')' Supporting text has yet to be entered.'
        else
          do 442 ij=1,nbclmlhelp
            write(IFCFG,'(a)') clmlhelp(ij)(1:lnblnk(clmlhelp(ij)))
 442      continue
        endif
        write(IFCFG,'(a)')'*season_end'
      endif

      WRITE(IFCFG,'(a,I4,a)',IOSTAT=IOS,ERR=2) '*year  ',
     &  IYEAR,' # assessment year'

C If SBEM compliance checking is active write name of NCM file if
C it is something other than UNKNOWN.
      IF(ISBEM.GE.1)THEN
        if(LASBEM(1:4).eq.'UNKN')then
          continue
        else
          WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*ncm  ',
     &      LASBEM(1:lnblnk(LASBEM))
        endif
      ENDIF
      IF(INOTI.EQ.1)THEN
        WRITE(OUTS,'(2A)')'*not # UK NCM notional model (10% thermal',
     &  ' bridges included in all exterior surfaces)'
        WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2)OUTS(1:LNBLNK(OUTS))
      ELSEIF(INOTI.EQ.2)THEN
        WRITE(OUTS,'(A)')'*ref # UK NCM reference model'
        WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2)OUTS(1:LNBLNK(OUTS))
      ELSEIF(INOTI.EQ.3)THEN
        WRITE(OUTS,'(A)')'*typ # UK NCM typical model'
        WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2)OUTS(1:LNBLNK(OUTS))
      ELSEIF(INOTI.EQ.4)THEN
        WRITE(OUTS,'(A)')'*str # UK NCM stripped model'
        WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2)OUTS(1:LNBLNK(OUTS))
      ENDIF

C Write images if any. If the version of the configuration file
C is greater than 3 then also write out documentation.
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

C Write primary energy conversions if these have been set.
      if(ipconv.eq.1)then
        WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2) 
     &   '# prim energy conv (heat,cool,lights,fan,sml pwr,hot water)'
        WRITE(IFCFG,'(a,6F6.3)',IOSTAT=IOS,ERR=2) '*pecnv ',
     &    pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
        WRITE(IFCFG,'(a,3F9.3,a)',IOSTAT=IOS,ERR=2) '*htemis ',
     &    phtco2,phtnox,phtsox,' # heating emissions CO2,NOX,SOX'
        WRITE(IFCFG,'(a,3F9.3,a)',IOSTAT=IOS,ERR=2) '*clemis ',
     &    pclco2,pclnox,pclsox,' # cooling emissions CO2,NOX,SOX'
        WRITE(IFCFG,'(a,3F9.3,a)',IOSTAT=IOS,ERR=2) '*ltemis ',
     &    pltco2,pltnox,pltsox,' # lighting emissions CO2,NOX,SOX'
        WRITE(IFCFG,'(a,3F9.3,a)',IOSTAT=IOS,ERR=2) '*fnemis ',
     &    pfnco2,pfnnox,pfnsox,' # fan/pump emissions CO2,NOX,SOX'
        WRITE(IFCFG,'(a,3F9.3,a)',IOSTAT=IOS,ERR=2) '*spemis ',
     &    pspco2,pspnox,pspsox,' # small power emissions CO2,NOX,SOX'
        WRITE(IFCFG,'(a,3F9.3,a)',IOSTAT=IOS,ERR=2) '*hwemis ',
     &    phwco2,phwnox,phwsox,' # dhw emissions CO2,NOX,SOX'
      endif

C Depending on the version of the configuration file write out the
C name of the IPV file or write the data internal to the cfg file.
      lnipv=lnblnk(lipvdatf)
      if(icfgv.lt.4)then
        if(lnipv.eq.0)then
          continue
        elseif(lipvdatf(1:7).eq.'UNKNOWN')then
          continue
        elseif(lipvdatf(1:8).eq.'internal')then

C If the number of IPV metrics or demand sets is greater than
C zero. If so take common block information and include
C it in the configuration file.
          if(nms.gt.0.or.neds.gt.0)then

C Use call mkipvdat to write into the cfg file and then clear name of
C the external IPV file (also notify the user they can remove it).
            ipvaction='cfg'
            if(ipvform.eq.0) ipvform=2  ! if format not defined set to tab separated
            call mkipvdat(ifcfg,LCFGF,ipvaction)
          endif
        else
          WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*ipv  ',
     &      lipvdatf(1:lnipv)
        endif
      else

C If the version of configuration file is 4 or greater
C then there could be an embedded IPV description. Check
C first if the number of IPV metrics or demand sets is greater than
C zero. If so take common block information and include
C it in the configuration file.
        if(nms.gt.0.or.neds.gt.0)then

C Use call mkipvdat to write into the cfg file and then clear name of
C the external IPV file (also notify the user they can remove it).
          ipvaction='cfg'
          if(ipvform.eq.0) ipvform=2  ! if format not defined set to tab separated
          call mkipvdat(ifcfg,LCFGF,ipvaction)
          lipvdatf='internal'  ! ensure old file name is no longer referenced
        endif
      endif

C Building demands file, if known.
      if(lnblnk(bdmds).eq.0)then
        continue
      elseif(bdmds(1:7).eq.'UNKNOWN')then
        continue
      else
        WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*dmds  ',
     &    bdmds(1:lnblnk(bdmds))
      endif

C Write CPCALC data, if known
      if(icpcon.eq.1)then
        WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2) '# CPCALC data'
        WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) 
     &    '# building lenght/width/hight,origin (x,y,z),',
     &    'orientation,roof type,roof angle,SBH,PAD,Vexp'
        WRITE(IFCFG,'(a,6F7.2,F6.1,I2,4F7.2)',IOSTAT=IOS,ERR=2)
     &    '*cpcalc',ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,sbh,pad,
     &    wvpe
      ENDIF

C Write radiance RIF file if known.
      if(lnblnk(lradcf).eq.0)then
        continue
      elseif(lradcf(1:7).eq.'UNKNOWN')then
        continue
      else
        WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*rif  ',
     &    lradcf(1:lnblnk(lradcf))
      endif

C Non-linear thermophysical properties configuration file.
      IF(INTHPS)WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*nlt  ',
     &  LNLTHP(1:lnblnk(LNLTHP))

C Building 1D node distribution.
      IF(IGR1D)WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*g1d  ',
     &  LGRD1D(1:lnblnk(LGRD1D))

C Bi-directional optical data. If bidirfile is something other
C than blank or 'UNKNOWN'.
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

C Special materials and electrical bus description files.
      if(ispmxist.gt.0.and.spflnam(1:2).ne.'  ')then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*spf  ',
     &    spflnam(1:lnblnk(spflnam))
      endif
      if(ientxist.gt.0)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*pnt  ',
     &    entflnam(1:lnblnk(entflnam))
      endif

C Write temporal file information. Check to see if an
C ascii version is available (itdflg > 1) and if so
C write out ltdfa instead.
      if(ITDFLG.ne.0)then
        if(itdflg.gt.1)then
          WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*tdf  ',
     &      LTDFA(1:lnblnk(LTDFA))
        elseif(itdflg.lt.0.or.itdflg.eq.1)then
          WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*tdf  ',
     &      LTDF(1:lnblnk(LTDF))
        endif
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) 
     &    '# tdf item id, item type, <related zone>, <related surface> '
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
        if(isavgh.eq.0)then
          WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2)
     &'# sim setup: no. sets startup zone_ts plant_ts save_lv @ each ts'
        else
          WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2)
     &'# sim setup: no. sets startup zone_ts plant_ts save_lv @ 1 ts/hr'
        endif
        WRITE(IFCFG,'(A,6i4)',IOSTAT=IOS,ERR=2) '*sps ',nsset,isstup,
     &    isbnstep,ispnstep,issave,isavgh

C Deal with multi-year climate data (currently only works with
C save level 5).
        bMY_set_open = .false.
     
        do 333 iss=1,nsset

C Flag indicating start of multi-year climate record.
          if ( bMY_period_support(iss) .and. .not. bMY_set_open ) then
            write(IFCFG, '(A)') '*start_my_sets'
            bMY_set_open = .true.
          endif 

          if ( bMY_period_support(iss) ) then
            WRITE(IFCFG,'(2I3,1I5,2I3,1I5,2x,2A)',IOSTAT=IOS,ERR=2)
     &        isstday(iss),isstmon(iss),iMY_period_start_years(iss),
     &        isfnday(iss),isfnmon(iss),iMY_period_end_years(iss),
     &        spfdescr(iss),' # period & name'
          else
            WRITE(IFCFG,'(4I4,2x,2A)',IOSTAT=IOS,ERR=2)isstday(iss),
     &        isstmon(iss),isfnday(iss),isfnmon(iss),spfdescr(iss),
     &        ' # period & name'
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

          WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2)'*end_set'

C Flag for end of multi-simuation sets.
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

C Annual ipv report (if description included within cfg or external file).
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

C Building and client address tokens (as in UK national calculation method).
      WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) 
     &  '# Name and address of building'
      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*B-NAME ',
     &  pjname(1:lnblnk(pjname))
      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*B-ADDRESS ',
     &  baddress(1:lnblnk(baddress))
      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*B-CITY ',
     &  bcity(1:lnblnk(bcity))
      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*B-POSTCODE ',
     &  buildingPC(1:lnblnk(buildingPC))
      WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) 
     &  '# Contact information for building owner'
      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*O-NAME ',
     &  ownername(1:lnblnk(ownername))
      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*O-ADDRESS ',
     &  owneraddr(1:lnblnk(owneraddr))
      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*O-CITY ',
     &  ownercity(1:lnblnk(ownercity))
      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*O-POSTCODE ',
     &  ownerPC(1:lnblnk(ownerPC))
      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*O-TELEPHONE ',
     &  ownertel(1:lnblnk(ownertel))

C Write out contact details of simulation team.
      WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) 
     &  '# Contact information for simulation team'
      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*C-NAME ',
     &  certifname(1:lnblnk(certifname))
      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*C-ADDRESS ',
     &  certaddress(1:lnblnk(certaddress))
      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*C-CITY ',
     &  certcity(1:lnblnk(certcity))
      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*C-POSTCODE ',
     &  certPC(1:lnblnk(certPC))
      WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)'*C-TELEPHONE ',
     &  certTel(1:lnblnk(certTel))


C Write project log file related to this model.
      WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) '* PROJ LOG'
      WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2)lmodellog(1:lnblnk(lmodellog))

C Ground information.
      dogrnd = .false.
      IF(NGRDP.gt.0.or.GRND3D)dogrnd=.true.
      IF(GTGEOM(1:7).ne.'UNKNOWN'.and.GTGEOM(1:2).ne.'  ')dogrnd=.true.

C External Longwave Radiation
C Add condition - if profile entered for external longwave radiation calc, then
C dogrnd = true.
      IF (iExtLgRadFlag .EQ. 1) dogrnd = .true.

      if(dogrnd)then
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) '* Ground'

C If ground profile for external longwave radiation calculation is
C entered (flag=1) then write *gtmp tag to cfg file followed
C by profile. 
 
      IF(iExtLgRadFlag.EQ.1) THEN
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2)'*gtmp'
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
          WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*gtp  ',
     &      GTGEOM(1:lnblnk(GTGEOM))
        endif
        if(ngrdp.gt.0)then
          WRITE(IFCFG,'(A,i3)',IOSTAT=IOS,ERR=2)'*mgp  ',NGRDP
          DO 932 I=1,NGRDP
            WRITE(IFCFG,933,IOSTAT=IOS,ERR=2)(UGRDTP(J,I),J=1,12)
  933       FORMAT(12F6.2)
  932     CONTINUE
        endif
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) '*end'
      endif

C If registration.
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

      WRITE(IFCFG,'(I7,A)',IOSTAT=IOS,ERR=2) NCOMP,'  # no of zones'

      DO 920 I = 1,NCOMP
        WRITE(IFCFG,'(A,I3,2A)',IOSTAT=IOS,ERR=2)
     &    '*zon ',NCCODE(I),'   # reference for ',zname(I)
        WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &    '*opr ',LPROJ(I)(1:LNBLNK(LPROJ(I))),'  # schedules'
        WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &    '*geo ',LGEOM(I)(1:LNBLNK(LGEOM(I))),'  # geometry'
        WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &    '*con ',LTHRM(I)(1:LNBLNK(LTHRM(I))),'  # construction'
        if(IOBS(I).EQ.1)then
          WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &      '*obs ',ZOBS(I)(1:lnblnk(ZOBS(I))),'  # obstructions'
        endif
        if(ITW(I).EQ.1)then
          WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &    '*tmc ',LTWIN(I)(1:lnblnk(LTWIN(I))),'  # transparent constr'
        endif
        if(icfc(I).EQ.1)then
          WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &    '*cfc ',lcfcin(I)(1:lnblnk(lcfcin(I))),
     &    '  # complex fenestration constr'
        endif
        if(IVF(I).EQ.1)then
          WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &      '*ivf ',LVIEW(I)(1:LNBLNK(LVIEW(I))),' # viewfactors'
        endif
        if(ICGC(I).EQ.1)then
          WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &    '*cgc ',LCGCIN(I)(1:lnblnk(LCGCIN(I))),' # casual gn control'
        endif
        if(ISI(I).EQ.1)then
          WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &      '*isi ',LSHAD(I)(1:lnblnk(LSHAD(I))),'  # shading db '
        endif
        if(IHC(I).EQ.1)then
          WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &    '*ihc ',LHCCO(I)(1:LNBLNK(LHCCO(I))),'  # convective hc coef'
        endif

C Write zone CFD file if known.
        if(lnblnk(LCFD(I)).eq.0)then
          continue
        elseif(LCFD(I)(1:7).eq.'UNKNOWN'.or.LCFD(I)(1:2).eq.'  ')then
          continue
        else
          WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=2)
     &      '*cfd ',LCFD(I)(1:lnblnk(LCFD(I))),'  # cfd domain'
        endif
        if(ZONE3D(I))then
          WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &      '*cvs ',L3DCVS(I)(1:lnblnk(L3DCVS(I))),'  # 3D volumes'
          WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &      '*cnc ',L3DCNC(I)(1:lnblnk(L3DCNC(I))),'  # 3D connections'
          WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &      '*ndc ',L3DNDC(I)(1:lnblnk(L3DNDC(I))),'  # 3D coordiantes'
          WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &      '*3dt ',L3DTAQ(I)(1:lnblnk(L3DTAQ(I))),'  # 3D temperatures'
        endif
        if(MSTRZN(I))then
          WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &      '*mst ',LMOIST(I)(1:lnblnk(LMOIST(I))),'  # moisture'
        endif
        if(IndxSt.EQ.I)then
          WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &      '*csm ',LGrdSt(1:lnblnk(LGrdSt)),'  # structured mesh'
        endif

C SHOCC zone files.
        if(bZoneSHOCCed(I))then
          WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &      '*shz ',SHOCCshzFile(I)(1:lnblnk(SHOCCshzFile(I))),
     &      '  # SHOCC'
        endif

C BSIMP
        if(iBSIMP(I).EQ.1)then
          WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &      '*bsm ',LBSIMP(I)(1:lnblnk(LBSIMP(I))),'  # BASESIMP input '
        endif

        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2)'*zend '
  920 CONTINUE

C Write connection information.
      WRITE(IFCFG,'(3A)',IOSTAT=IOS,ERR=2)
     &  '*cnn ',LCNN(1:lnblnk(LCNN)),'  # connections'

C Re-build zone:surface to connection hash array.
      do 43 icc = 1, NCON
        IZSTOCN(IC1(icc),IE1(icc))=icc
 43   continue

C If there is more than one anchor concept write them out.
      if(NALOC.gt.0)then
        WRITE(IFCFG,'(A,i2)',IOSTAT=IOS,ERR=3) '*Anchor ',NALOC
        do 44 ia=1,NALOC
          WRITE(IFCFG,'(3A,i3)',IOSTAT=IOS,ERR=3) ALOCLBL(ia),' ',
     &       ALOCTYP(ia),IALOC(ia)

C Write packed string of the items associated with each anchor.
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

C Write airflow model indes and then its description.
      if(IAIRN.EQ.0)then
        WRITE(IFCFG,'(i4,a)',IOSTAT=IOS,ERR=2)IAIRN,
     &    '   # no fluid flow network'
      elseif(IAIRN.ge.1)then
        WRITE(IFCFG,'(i4,a)',IOSTAT=IOS,ERR=2)IAIRN,
     &    '   # fluid flow network:'
      endif

      IF(IAIRN.ge.1)then

C Make up an array of node names to write out substituting a
C zero where there is no association.
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2)
     &    LAPROB(1:LNBLNK(LAPROB)),'    # leakage description'
        do 777 j=1,ncomp
          if(ICAAS(J).ne.0)then
            namen(j)=NDNAM(ICAAS(J))
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

 9021 if(INDCFG.EQ.2.or.INDCFG.EQ.3)then

C Plant network information - plant file and model name (if plant only).
        WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2) '* Plant'
        WRITE(IFCFG,'(2a)',IOSTAT=IOS,ERR=2)LPNF(1:lnblnk(LPNF)),
     &    '  # plant network description'
        if(INDCFG.EQ.2)then
          WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2) 
     &      modeltitle(1:lnblnk(modeltitle))
        endif
      endif

C This is all there is to write so close the files.
 9028 CALL ERPFREE(IFCFG,IOS) ! free the cfg file unit

C Now write the connections file if there are any.
      if(ncon.gt.0)then
        CALL EFOPSEQ(IUF,LCNN,3,IER)
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=2) '*connections for ',
     &    cfgroot(1:lnblnk(cfgroot))
        call dstamp(dstmp)
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3) '*Date ',dstmp
        WRITE(IUF,'(I7,A)',IOSTAT=IOS,ERR=2)
     &    NCON,'   # number of connections'

        DO 930 I = 1,NCON
          CALL CONXINF(1,I,CXSTR)
          WRITE(IUF,'(5I4,2A)',IOSTAT=IOS,ERR=2) IC1(I),IE1(I),
     &      ICT(I),IC2(I),IE2(I),'  # ',CXSTR(1:lnblnk(CXSTR))

          IF(ICT(I).NE.4.OR.IC2(I).GT.0)GOTO 930
          if(IE2(I).GT.NGRDP)then
            CALL edisp(iuout,
     &      'Ground connection referenced an unknown monthly profile!')
          endif
  930   CONTINUE
        CALL ERPFREE(IUF,IOS)   ! free the cnn file unit
      endif

      RETURN

C Error messages.
    2 CALL USRMSG(' Problem writing data',' ','W')
      GOTO 9028
    3 if(IOS.eq.2)then
        CALL USRMSG(' Not permitted to write configuration file.',
     &    ' ','W')
        GOTO 9028
      else
        CALL USRMSG(' Problem writing configuration name',' ','W')
        GOTO 9028
      endif
  146 if(IOS.eq.2)then
        CALL USRMSG(' Not permitted to write bi-directional optics',
     &    'list to configuration file.','W')
        GOTO 9028
      else
        CALL USRMSG(' Problem writing bi-directional optics list',
     &    'to configuration file. ','W')
        GOTO 9028
      endif

      END

C ********* copy of code from esrucom/esystem.f
C ********* for applications which only scan/write configuration
C CONXINF returns an English description in CXSTR of the inter-connection 
C information in common blocks  which are filled by reading the
C system configuration file. If ICF=1 then display with zone and surface names 
C if possible. If ICON=0 then generate a text heading.
      SUBROUTINE CONXINF(ICF,ICON,CXSTR)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      INTEGER :: IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      CHARACTER CXSTR*78,SST1*12,ZST1*12,SST2*12,ZST2*12

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

      SST1=SSNAME(ICON)
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
 8983   FORMAT(I4,1X,a,' in ',a,' >|< Constant @',I3,' dC &',
     &         I4,' W rad')
      ELSEIF(ICT(ICON).EQ.3.AND.ICF.GT.0)THEN
        ICM=IZSTOCN(IC2(ICON),IE2(ICON))
        if(ICM.gt.0)then
          SST2=SSNAME(ICM)
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

C BASESIMP start.
      ELSEIF(ICT(ICON).EQ.6.AND.ICF.GT.0)THEN
        WRITE(CXSTR,7986)ICON,SST1(1:lsn1),ZST1(1:lzn1)
 7986   FORMAT(I4,1X,a,' in ',a,' is BASESIMP fndtn')
C BASESIMP end.
      ELSEIF(ICT(ICON).EQ.7.AND.ICF.GT.0)THEN

C CEN 13791 boundary condition.
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

C ****** Scan and report current mis-matches.
      subroutine ckcurmatch(prob,iprob)
#include "building.h"
#include "geometry.h"

C Parameters
      logical prob  ! true if there is a problem
      integer iprob ! number of mis-matches detected

      COMMON/OUTIN/IUOUT,IUIN

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
        do 288 IS=1,nzsur(iz)
          ICC=ICC+1
          if(IC1(ICC).ne.IZ)then

C If this is the first warning print out heading.
            if(.NOT.first)then
              CALL CONXINF(1,0,CXSTR)
              CALL EDISP(iuout,CXSTR)
              first=.true.
            endif
            call edisp(iuout,'Current zone does not match contig.')
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
            call edisp(iuout,'Current surface does not match contig.')
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
                call edisp(iuout,'One side is not linked...')
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
                call edisp(iuout,'Contig. zone link >|< incorrect...')
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
               call edisp(iuout,'Contig. surface link >|< incorrect...')
                prob=.true.
                IPROB=IPROB+1
                CALL CONXINF(1,i,CXSTR)
                CALL EDISP(iuout,CXSTR)
                CALL CONXINF(1,ioc,CXSTR)
                CALL EDISP(iuout,CXSTR)
              endif

C Check to see if this connection is only pointed to once by an
C "another zone" connection in the whole list.
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
                    call edisp(iuout,'Contig. 2nd reference found...')
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
            call edisp(iuout,'Contig. other side links out of range...')
            CALL CONXINF(1,i,CXSTR)
            CALL EDISP(iuout,CXSTR)
            prob=.true.
            IPROB=IPROB+1
          endif
        endif
  290 continue
      return
      end
