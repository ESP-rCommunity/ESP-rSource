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

C emkcfg.F includes the following:
C  EMKCFG  Creates a system configuration file.
C  CONXINF Returns inter-connection descr in CXSTR (copy)
C  ckcurmatch Scan and report current connection mis-matches.

C **************** EMKCFGG 
C EMKCFGG writes out the current contents of system configuration
C common blocks into the file and unit specified in COMMON/C21.
C This version uses generic databases.
      SUBROUTINE EMKCFGG(act,IER)

#include "building.h"
#include "geometry.h"
#include "tdf2.h"
#include "uncertainty.h"
#include "net_flow.h"
#include "esprdbfile.h"
#include "CFC_common.h"
C espprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure databases)
C LOPTDB,IOPTDB (for optical database)
C LPCDB/IPCDB (for plant template database)
C LPRFDB,IPRODB (for event profile database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
C ICLIM,LCLIM (for climate file)
      PARAMETER (MSTMC=20,MSGAL=40)
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      common/C3F/LCNN
      COMMON/C4/XLAT,XLON

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM
      CHARACTER*72 SNFNAM
      COMMON/C5R/SKYR,GRDR,BLDR
      COMMON/C6/INDCFG
      COMMON/C14/UGRDTP(12,MGRDP),NGRDP
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)

C Plant network.
      COMMON/C23/IFPNF,LPNF
      common/CFGV/icfgv

C Project log file, paths to standard file locations.
      COMMON/LOG/LPRJLG
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

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

C Radiance scene file.
      common/radcfg/LRADCF

C Special materials file flag
      common/spmfxst/ispmxist,spflnam
      common/entfile/entflnam,ientxist

      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
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

C 3D zones.
      common/GR3D100/BLDG3D,ZONE3D(MCOM)
      common/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)

C Moisture.
      common/MOIST01/MSTROK,MSTRZN(MCOM)
      common/MOIST02/LMOIST(MCOM)

C Structured mesh.
      COMMON/GRSD100/IndxSt
      COMMON/GRSD101/LGrdSt

C Temporal definition commons.
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFT/LTDF,LTDFA
      COMMON/TDFFLG/ITEMSTD
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)

C Primary energy and emissions conversions.
      common/PCONV/ipconv,pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
      common/ECONV/ieconv
      common/CONVEM/phtco2,phtnox,phtsox,pclco2,pclnox,pclsox,
     &              pltco2,pltnox,pltsox,pfnco2,pfnnox,pfnsox,
     &              pspco2,pspnox,pspsox,phwco2,phwnox,phwsox

C IPV description.
      common/IPVF/lipvdatf

C Project demands (lifts/fans/DHW).
      COMMON/BL1/dmdsdesc,bdmds

C CPCALC description
      common/CPCALC/icpcon,ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,
     &              sbh,pad,wvpe

C Variables for weekdays, and weekends.
C Assume: Mon=1, Tue=2, Wed=3, Thu=4, Fri=5, Sat=6, Sun=7
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2

C Uncertainty analysis common.
      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)

C Anchor data.
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      common/anchorb/IALOC(20),lstanchr(20,60)

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
      common/aim2def/iAIM2,LAIM2
C HVAC
C Flag indicating presence of HVAC models:
C ihvacflag=1 indicates HVAC models are active; 0 indicates no HVAC models.
      common/hvacinfo/ihvacflag,hvacfile

C Contaminant flow model commons
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM/CNTMFIL,CNTMDESC,NTSTEPC

C DHW.
      common/dhw/sDHW_InputFileName,iDHW_FLAG
      character sDHW_InputFileName*72
      INTEGER   iDHW_FLAG

C Fuel cell.
      common/Fcell/iFClds,LFClds
C External longwave radiation (begin)
      COMMON/LongRad/iExtLgRadFlag,eGrdTp(12)
      INTEGER jMon
C GSHP
       common/gshpinfo/igshp
       common/gshpfile/gshpfile
       INTEGER igshp  
       character gshpfile*72 
C GCEP(begin)
       common/gcepinfo/igcep
       common/gcepfile/gcepfile
       INTEGER igcep  
       character gcepfile*72 
C GCEP(end)

C Bidirectional raw optical data file.
      COMMON/BIDIR/IFLAGBI,INTVALBI,NSTMCFL(MCON)
      COMMON/BIDIRFL/bidirfile,bidirname(MSTMC)

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)

C Generic databases from esprdbfile.h.

      CHARACTER*72 LBSIMP,LAIM2,hvacfile,LFClds

      CHARACTER*72 LCFGF,LCNN
      CHARACTER LAPROB*72,LPRJLG*72
      CHARACTER CXSTR*78,cfgroot*24
      CHARACTER LPNF*72,LTDF*72,LTDFA*72
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDNDD,LGDTAQ,LGRD1D,LNLTHP
      character*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,LMOIST,LGrdSt
      character lradcf*72
      CHARACTER ctldoc*248,LCTLF*72,H*72
      CHARACTER spflnam*72,entflnam*72
      character GTGEOM*72,LCFD*72,DCNN*72
      CHARACTER DBTAG*12,DBTASK*12,DBZN*15,indxcmt*24,DBSN*15
      character*10 wkd1, wkd2
      character LUALF*72,LCNG*15,LLOC*15,lipvdatf*72,act*1
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      CHARACTER dmdsdesc*248,bdmds*72,outs*124
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,spfdescr*8,ltmp*72,louts*144
      character bidirfile*72,bidirname*12
      character calename*32,calentag*12,calendayname*32,loutc*96
      CHARACTER CNTMFIL*72,CONTMNAM*12,CNTMDESC*124

C An array of mass flow nodes for writing.
      CHARACTER NDNAM*12
      character namen*12
      dimension namen(mcom)

      dimension iav(60)

      logical KNOWN,dogrnd
      LOGICAL INTHPS,INTHPZ,IGR1D,GRND3D
      LOGICAL BLDG3D,ZONE3D,MSTROK,MSTRZN
      character dstmp*24
      character ALOC*12,ALOCLBL*12,ALOCTYP*4

C Control for writing out multi-climate sim presets
      logical bMY_set_open 

      integer itrunc  ! to indicate there is a continuation line.
      integer ipos    ! position in flow node string buffer
      integer lnouts   ! location at end of flow node string buffer

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
        CALL EASKAB(' Configuration file loaded was in old format.',
     &    ' save in:','current format','cancel',IW,3)
        if(IW.EQ.2)then
          call edisp(iuout,'Configuration not being updated.')
          return
        elseif(IW.EQ.1)then

C If no model root name has been set, make from configuration file.
C If filename > 4 char it might have a .cfg attached.
          icfgv=3
          lcfgr=lnblnk(LCFGF)
          lcfgl=lcfgr-3
          if(lcfgr.gt.4)then
            if(LCFGF(lcfgl:lcfgr).eq.'.cfg')then
              if(lcfgl.gt.24)then
                write(cfgroot,'(a)',iostat=ios,err=3) LCFGF(1:24)
              else
                write(cfgroot,'(a)',iostat=ios,err=3) LCFGF(1:lcfgl-1)
              endif
            else
              if(lcfgr.gt.24)then
                write(cfgroot,'(a)',iostat=ios,err=3) LCFGF(1:24)
              else
                write(cfgroot,'(a)',iostat=ios,err=3) LCFGF(1:lcfgr)
              endif
            endif
          else
            if(lcfgr.gt.24)then
              write(cfgroot,'(a)',iostat=ios,err=3) LCFGF(1:24)
            else
              write(cfgroot,'(a)') LCFGF(1:lcfgr)
            endif
          endif

C Also check about connections file.
          if(ncon.gt.1)then
  290       H(1)='The surface topology of the model is held'
            H(2)='in a connections file.'
            write(LCNN,'(a,a)')cfgroot(1:lnblnk(cfgroot)),'.cnn'
            DCNN=LCNN
            ltmp=LCNN
            CALL EASKS(ltmp,' Connections file name?',' ',
     &        72,DCNN,'system connx file name',IER,2)
            if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
              LCNN=ltmp
            else
              GOTO 290
            endif
          endif
        endif
      endif

C Test to see if model needs to be updated from registration
C to some other condition.  If first zones added the check where to
C put the connections.
      if(INDCFG.EQ.0)then
        if(NCOMP.eq.1.and.LGEOM(1)(1:4).ne.'dumm')then
          INDCFG=1
          call edisp(iuout,'Upgrading from registration to zone...')
          write(LCNN,'(a,a)') cfgroot(1:lnblnk(cfgroot)),'.cnn'
  289     H(1)='The surface topology of the model is held in'
          H(2)='a connections file.'
          DCNN='model.cnn'
          ltmp=LCNN
          CALL EASKS(ltmp,' Connections file name?',' ',72,
     &      DCNN,'system connx file name',IER,2)
          if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
            LCNN=ltmp
          else
            GOTO 289
          endif
          if(LPNF(1:4).ne.'UNKN'.and.LPNF(1:2).ne.'  ')then
            INDCFG=3
            call edisp(iuout,'Upgrading from zone to zone + plant...')
          endif
        elseif(ncomp.gt.1)then
          INDCFG=1
          call edisp(iuout,'Upgrading from registration to zone...')
          if(LPNF(1:4).ne.'UNKN'.and.LPNF(1:2).ne.'  ')then
            INDCFG=3
            call edisp(iuout,'Upgrading from zone to zone + plant...')
          endif
        endif
      elseif(INDCFG.EQ.1)then
        if(LPNF(1:4).ne.'UNKN'.and.LPNF(1:2).ne.'  ')then
          INDCFG=3
          call edisp(iuout,'Upgrading from zone to zone + plant...')
        endif
      endif

C Comment for configuration index
      IF(INDCFG.EQ.0) indxcmt=' # Project registration'
      IF(INDCFG.EQ.1)  indxcmt=' # Building only'
      IF(INDCFG.EQ.2)  indxcmt=' # Plant only'
      IF(INDCFG.EQ.3) indxcmt=' # Building & Plant'

C Begin writing the file.
      WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=3) '* CONFIGURATION3.1'
      WRITE(IFCFG,30,IOSTAT=IOS,ERR=3) LCFGF(1:lnblnk(LCFGF))
      call dstamp(dstmp)
      WRITE(IFCFG,'(A,A,a)',IOSTAT=IOS,ERR=3) '*date ',dstmp,
     &  '  # latest file modification '
      WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=3) '*root ',
     &  cfgroot(1:lnblnk(cfgroot))
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*zonpth ',zonepth,
     &  '  # path to zones'
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*netpth ',netpth,
     &  '  # path to networks'
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*ctlpth ',ctlpth,
     &  '  # path to controls'
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*radpth ',radpth,
     &  '  # path to radiance files'
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*imgpth ',imgpth,
     &  '  # path to project images'
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*tmppth ',tmppth,
     &  '  # path to project scratch folder'
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*docpth ',docpth,
     &  '  # path to project documents'
      WRITE(IFCFG,'(3a)',IOSTAT=IOS,ERR=3) '*dbspth ',dbspth,
     &  '  # path to local databases'
      WRITE(IFCFG,'(A,i4,a)',IOSTAT=IOS,ERR=3) '*indx ',INDCFG,
     &  indxcmt(1:lnblnk(indxcmt))
  30  FORMAT('# ESRU system configuration defined by file ',/,'# ',A)

      WRITE(IFCFG,800,IOSTAT=IOS,ERR=2) XLAT,XLON
 800  FORMAT(F7.3,2X,F7.3,
     &  '   # Latitude & Longitude (diff from meridian)')

      WRITE(IFCFG,8001,IOSTAT=IOS,ERR=2) IXPOS,GREF
8001  FORMAT(I7,2X,F6.3,'   # Site exposure & ground reflectivity')
      IF(IXPOS.EQ.8)WRITE(IFCFG,'(3F8.3,A)',IOSTAT=IOS,ERR=2)
     &           SKYR,GRDR,BLDR,'   # views to sky ground buildings'

C Write databases related to this model.
      WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) '* DATABASES'
      WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*cns  ',
     &      gendbfn(1)(1:lnblnk(gendbfn(1)))
      WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*mat  ',
     &      gendbfn(2)(1:lnblnk(gendbfn(2)))
C      WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*opt  ',
C     &      LOPTDB(1:lnblnk(LOPTDB))
      WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*aco  ',
     &      gendbfn(5)(1:lnblnk(gendbfn(5)))
      WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*col  ',
     &      gendbfn(6)(1:lnblnk(gendbfn(6)))
      WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*env  ',
     &      gendbfn(8)(1:lnblnk(gendbfn(8)))
      WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*trn  ',
     &      gendbfn(9)(1:lnblnk(gendbfn(9)))
      WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*prs  ',
     &      gendbfn(10)(1:lnblnk(gendbfn(10)))
      WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*pro  ',
     &      gendbfn(11)(1:lnblnk(gendbfn(11)))
      WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*clm  ',
     &      LCLIM(1:lnblnk(LCLIM))
      WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*pdb  ',
     &      gendbfn(12)(1:lnblnk(gendbfn(12)))

C HVAC
      IF (ihvacflag.EQ.1)then
        WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*hvac  ',
     &    hvacfile(1:lnblnk(hvacfile))
      endif

C DHW
      IF (iDHW_FLAG.EQ.1)then
        WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*dhw  ',
     &    sDHW_InputFileName(1:lnblnk(sDHW_InputFileName))
      endif

C AIM-2.
      IF (iAIM2.EQ.1)then
        WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*aim   ',
     &    LAIM2(1:lnblnk(LAIM2))
      endif

C Fuel cell.
      IF (iFClds.EQ.1)then
        WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*fcl   ',
     &    LFClds(1:lnblnk(LFClds))
      endif

C GSHP
      IF (igshp.EQ.1)then
        WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*gshp  ',
     &    gshpfile(1:lnblnk(gshpfile))
      endif

C GCEP (begin)
      IF (igcep.EQ.1)then
        WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2) '*gcep  ',
     &    gcepfile(1:lnblnk(gcepfile))
      endif
C  GCEP (end)

C Write control file if known.
      if(lnblnk(lctlf).eq.0)then
        continue
      elseif(lctlf(1:7).eq.'UNKNOWN')then
        continue
      elseif(lctlf(1:2).eq.'  ')then
        continue
      else
        WRITE(IFCFG,'(a,a)',IOSTAT=IOS,ERR=2) '*ctl  ',
     &    LCTLF(1:lnblnk(LCTLF))
      endif

C Write contaminant model description
      IF(NOCNTM.GT.0)WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     & '*ctm  ',CNTMFIL(1:lnblnk(CNTMFIL))

C Write initial view info if other than default.
      if(initvt.eq.1)then
        WRITE(IFCFG,'(a,6f8.1,f5.1)',IOSTAT=IOS,ERR=2)'*vew ',
     &    EYEMI(1),EYEMI(2),EYEMI(3),
     &    VIEWMI(1),VIEWMI(2),VIEWMI(3),ANGI
      endif

C Write out calendar if nbdaytype is non-zero.
      if(nbdaytype.gt.0)then
        write(IFCFG,'(2a)',IOSTAT=IOS,ERR=2) '*calename ',
     &    calename(1:lnblnk(calename))
        do 443 icd = 1,nbdaytype
          lct=lnblnk(calentag(icd))
          lctn=lnblnk(calendayname(icd))
          write(IFCFG,'(5a,i3)',IOSTAT=IOS,ERR=2) '*calentag ',
     &      calentag(icd)(1:lct),',',calendayname(icd)(1:lctn),',',
     &      nbcaldays(icd)
 443    continue
        write(IFCFG,'(a)',IOSTAT=IOS,ERR=2) '*list'
        itrunc=1
        ipos=1
        do while (itrunc.ne.0)
          call ailist(ipos,365,icalender,365,'C',loutc,loutlen,itrunc)
          write(IFCFG,'(1x,a)',IOSTAT=ios,ERR=2) loutc(1:loutlen)
          ipos=itrunc+1
        end do
        write(IFCFG,'(a)',IOSTAT=IOS,ERR=2) '*end_list'
      endif

C Write uncertainty analysis library file if known.
      if(lnblnk(LUALF).eq.0)then
        continue
      elseif(LUALF(1:7).eq.'UNKNOWN')then
        continue
      else
        WRITE(IFCFG,'(a,a)',IOSTAT=IOS,ERR=2) '*ual  ',
     &    LUALF(1:lnblnk(LUALF))
      endif

C Write alternative definition of weekends if not default.
      if(idwe1.ne.6.or.idwe2.ne.7)then
        WRITE(IFCFG,'(a,2I3,a)',IOSTAT=IOS,ERR=2) '*weekend  ',
     &    idwe1,idwe2,' # alt weekend def'
      endif
      WRITE(IFCFG,'(a,I4,a)',IOSTAT=IOS,ERR=2) '*year  ',
     &  IYEAR,' # assessment year'

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

C IPV file, if known.
      if(lnblnk(lipvdatf).eq.0)then
        continue
      elseif(lipvdatf(1:7).eq.'UNKNOWN')then
        continue
      else
        WRITE(IFCFG,'(a,a)',IOSTAT=IOS,ERR=2) '*ipv  ',
     &    lipvdatf(1:lnblnk(lipvdatf))
      endif

C Building demands file, if known.
      if(lnblnk(bdmds).eq.0)then
        continue
      elseif(bdmds(1:7).eq.'UNKNOWN')then
        continue
      else
        WRITE(IFCFG,'(a,a)',IOSTAT=IOS,ERR=2) '*dmds  ',
     &    bdmds(1:lnblnk(bdmds))
      endif

C Write CPCALC data, if known
      if(icpcon.eq.1)then
        WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2) 
     &   '# CPCALC data'
        WRITE(IFCFG,'(a,a)',IOSTAT=IOS,ERR=2) 
     &   '# building lenght/width/hight,origin (x,y,z),',
     &   'orientation,roof type,roof angle,SBH,PAD,Vexp'
        WRITE(IFCFG,'(a,6F7.2,F6.1,I2,4F7.2)',IOSTAT=IOS,ERR=2)
     &   '*cpcalc',ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,sbh,pad,
     &     wvpe
      ENDIF

C Write radiance RIF file if known.
      if(lnblnk(lradcf).eq.0)then
        continue
      elseif(lradcf(1:7).eq.'UNKNOWN')then
        continue
      else
        WRITE(IFCFG,'(a,a)',IOSTAT=IOS,ERR=2) '*rif  ',
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

C Write temporal db information.
      if(ITDFLG.gt.0)then
        WRITE(IFCFG,'(2A)',IOSTAT=IOS,ERR=2) '*tdf  ',
     &    LTDF(1:lnblnk(LTDF))
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
        WRITE(IFCFG,'(A,5i5)',IOSTAT=IOS,ERR=2) '*sps ',nsset,isstup,
     &     isbnstep,ispnstep,issave,isavgh
        do 333 iss=1,nsset
          WRITE(IFCFG,'(4I4,2x,A,A)',IOSTAT=IOS,ERR=2)isstday(iss),
     &      isstmon(iss),isfnday(iss),isfnmon(iss),spfdescr(iss),
     &      ' # period & name'
          if(INDCFG.ne.2)WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2)'*sblr ',
     &       sblres(iss)(1:lnblnk(sblres(iss)))
          if(IAIRN.ge.1)WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2)'*sflr ',
     &       sflres(iss)(1:lnblnk(sflres(iss)))
          if(INDCFG.eq.2.or.INDCFG.eq.3)then
            WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2)'*splr ',
     &        splres(iss)(1:lnblnk(splres(iss)))
          endif
          if(MSTROK)WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2)'*smstr ',
     &       smstres(iss)(1:lnblnk(smstres(iss)))
          if(ientxist.gt.0)WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2)
     &      '*selr ',selres(iss)(1:lnblnk(selres(iss)))
          WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2)'*end_set'
 333    continue
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2)'*end_sps'
      endif

C Annual ipv report (if included).
      if(lnblnk(lipvdatf).eq.0)then
        continue
      elseif(lipvdatf(1:7).eq.'UNKNOWN')then
        continue
      else
        if(sipvres(1:2).ne.'  '.and.sipvres(1:7).ne.'UNKNOWN')then
          WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2)'*intipv ',
     &      sipvres(1:lnblnk(sipvres))
        endif
      endif

C Write project log file related to this model.
      WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) '* PROJ LOG'
      WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) LPRJLG(1:lnblnk(LPRJLG))

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
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2) LSNAM(1:lnblnk(LSNAM))
        goto 9028
      endif

C If plant only jump.
      IF(INDCFG.EQ.2)GOTO 9021

C Fabric portion of description.
      WRITE(IFCFG,5006,IOSTAT=IOS,ERR=2) LSNAM(1:lnblnk(LSNAM))
 5006 FORMAT('* Building',/,A)

      WRITE(IFCFG,'(I7,A)',IOSTAT=IOS,ERR=2) NCOMP,'  # no of zones'

      DO 920 I = 1,NCOMP
        WRITE(IFCFG,'(A,I3,A,A)',IOSTAT=IOS,ERR=2)
     &    '*zon ',NCCODE(I),'   # reference for ',zname(I)
        WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &    '*opr ',LPROJ(I)(:LNBLNK(LPROJ(I))),'  # schedules'
        WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &    '*geo ',LGEOM(I)(:LNBLNK(LGEOM(I))),'  # geometry'
        WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &    '*con ',LTHRM(I)(:LNBLNK(LTHRM(I))),'  # construction'
        if(IOBS(I).EQ.1)then
          WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &      '*obs ',ZOBS(I)(:lnblnk(ZOBS(I))),'  # obstructions'
        endif
        if(ITW(I).EQ.1)then
          WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &      '*tmc ',LTWIN(I)(:lnblnk(LTWIN(I))),'  # transparent constr'
        endif
        if(icfc(I).EQ.1)then
          WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &    '*cfc ',lcfcin(I)(1:lnblnk(lcfcin(I))),
     &    '  # complex fenestration constr'
        endif
        if(IVF(I).EQ.1)then
          WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &      '*ivf ',LVIEW(I)(:LNBLNK(LVIEW(I))),' # viewfactors'
        endif
        if(ICGC(I).EQ.1)then
          WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &      '*cgc ',LCGCIN(I)(:lnblnk(LCGCIN(I))),' # casual gn control'
        endif
        if(ISI(I).EQ.1)then
          WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &      '*isi ',LSHAD(I)(:lnblnk(LSHAD(I))),'  # shading db '
        endif
        if(IHC(I).EQ.1)then
          WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &      '*ihc ',LHCCO(I)(:LNBLNK(LHCCO(I))),'  # convective hc coef'
        endif

C Write zone CFD file if known.
        if(lnblnk(LCFD(I)).eq.0)then
          continue
        elseif(LCFD(I)(1:7).eq.'UNKNOWN'.or.LCFD(I)(1:2).eq.'  ')then
          continue
        else
          WRITE(IFCFG,'(a,a,a)',IOSTAT=IOS,ERR=2)
     &      '*cfd ',LCFD(I)(:lnblnk(LCFD(I))),'  # cfd domain'
        endif
        if(ZONE3D(I))then
          WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &      '*cvs ',L3DCVS(I)(:lnblnk(L3DCVS(I))),'  # 3D volumes'
          WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &      '*cnc ',L3DCNC(I)(:lnblnk(L3DCNC(I))),'  # 3D connections'
          WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &      '*ndc ',L3DNDC(I)(:lnblnk(L3DNDC(I))),'  # 3D coordiantes'
          WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &      '*3dt ',L3DTAQ(I)(:lnblnk(L3DTAQ(I))),'  # 3D temperatures'
        endif
        if(MSTRZN(I))then
          WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &      '*mst ',LMOIST(I)(:lnblnk(LMOIST(I))),'  # moisture'
        endif
        if(IndxSt.EQ.I)then
          WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &      '*csm ',LGrdSt(:lnblnk(LGrdSt)),'  # structured mesh'
        endif

C BSIMP
        if(iBSIMP(I).EQ.1)then
          WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &      '*bsm ',LBSIMP(I)(:lnblnk(LBSIMP(I))),'  # BASESIMP input '
        endif
                  
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=2)'*zend '
  920 CONTINUE

C Write connection information.
      WRITE(IFCFG,'(A,A,A)',IOSTAT=IOS,ERR=2)
     &  '*cnn  ',LCNN(1:lnblnk(LCNN)),'  # connections '
      CALL EFOPSEQ(IUF,LCNN,3,IER)
      WRITE(IUF,'(A,A)',IOSTAT=IOS,ERR=2) '*connections  for ',
     &  cfgroot(1:lnblnk(cfgroot))
      call dstamp(dstmp)
      WRITE(IUF,'(A,A)',IOSTAT=IOS,ERR=3) '*Date ',dstmp
      WRITE(IUF,'(I7,A)',IOSTAT=IOS,ERR=2)
     &  NCON,'   # number of connections'

      DO 930 I = 1,NCON
        CALL CONXINF(1,I,CXSTR)
        WRITE(IUF,'(5I4,A,A)',IOSTAT=IOS,ERR=2) IC1(I),IE1(I),
     &    ICT(I),IC2(I),IE2(I),'  # ',CXSTR(:lnblnk(CXSTR))

        IF(ICT(I).NE.4.OR.IC2(I).GT.0)GOTO 930
        if(IE2(I).GT.NGRDP)then
           CALL edisp(iuout,
     &      'Ground connection referenced an unknown monthly profile!')
        endif
  930 CONTINUE

C Re-build zone:surface to connection hash array.
      CALL ERPFREE(IUF,IOS)
      do 43 icc = 1, NCON
        IZSTOCN(IC1(icc),IE1(icc))=icc
 43   continue

      if(NALOC.gt.0)then
        WRITE(IFCFG,'(A,i3)',IOSTAT=IOS,ERR=3) '*Anchor ',NALOC
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
            call ailist(ipos,IALOC(ia),iav,60,'S',outs,loutlen,itrunc)
            write(IFCFG,'(1x,a)',IOSTAT=ios,ERR=3) outs(1:loutlen)
            ipos=itrunc+1
          end do
 44     continue
        WRITE(IFCFG,'(A)',IOSTAT=IOS,ERR=3) '*End'
      endif

C Write airflow model description
      IF(IAIRN.EQ.0)WRITE(IFCFG,807,IOSTAT=IOS,ERR=2)IAIRN
      IF(IAIRN.ge.1)WRITE(IFCFG,808,IOSTAT=IOS,ERR=2)IAIRN
  807 FORMAT(I7,'   # no fluid flow network')
  808 FORMAT(I7,'   # fluid flow network:  ')
      IF(IAIRN.ge.1)then

C Make up an array of node names to write out substituting a
C zero where there is no association.
        WRITE(IFCFG,'(A,A)',IOSTAT=IOS,ERR=2)
     &    LAPROB(:LNBLNK(LAPROB)),'    # leakage description'
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

C Plant network information.
        WRITE(IFCFG,'(a)',IOSTAT=IOS,ERR=2) '* Plant'
        WRITE(IFCFG,5007,IOSTAT=IOS,ERR=2)LPNF(:lnblnk(LPNF))
 5007   FORMAT(A,'       # plant network description')
      endif

 9028 CALL  ERPFREE(IFCFG,IOS)
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
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/C24/IZSTOCN(MCOM,MS)

      CHARACTER CXSTR*78,SST1*12,ZST1*12,SST2*12,ZST2*12
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12

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
      subroutine ckcurmatch(prob,iprob,ier)
#include "building.h"

C geometry.h provides commons G0/G2/c20.
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

      logical prob,dup,first
      logical newgeo  ! to use for testing if new/old geometry file.

      CHARACTER CXSTR*78

      newgeo=.false.  ! assume older format geometry.
      IPROB=0
      ICC=0
      prob=.false.
      first=.false.
      do 287 IZ=1,NCOMP
        call eclose(gversion(iz),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(IFIL+1,LGEOM(IZ),IZ,0,0,itru,IER)
        else
          CALL EGOMIN(IFIL+1,LGEOM(IZ),IZ,0,0,itru,IER)
        endif
        nzsur(iz)=nsur
        do 288 IS=1,NSUR
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
