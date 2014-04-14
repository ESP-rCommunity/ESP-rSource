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

C The file esru_misc.F is a collection of support facilities for
C reading and reporting ESP-r compatible files.

C esru_misc.F contains the following:
C  CLRPRB : clears the zone based memory in preparation for reading
C           a new model file.
C  cfgpk:  Reacts to one of several configuration buttons and presents
C          related images.
C  EGOMST: Reads site obstruction data as ASCII strings, with or without
C          range checking and printed summary.
C  MKGOMST:Creates an annotated site obstruction file based
C          on passed parameters and the contents of common GS5.
C  ERMRT:  Read the viewfactor/ MRT sensor definition file.
C  EMKMRT: Write viewfactor/MRT sensor definition file based on
C          information in common blocks MRTC, MRTF.

C  PLEQN:  Finds the equation EQN to a plane containing a polygon
C          defined as a set of X() Y() Z() verticies tracing the edges.
C  ETRANSW: used when creating an insert rectangular shape into a surface.
C ETRANFRAME: used when creating an fixed width frame into a surface.
C  TRANSUR: Transforms a surface along its normal.

C  UV2AZ:  Recover azimuth & elevation from unit vector.
C  LN2AZ:  Recover azimuth & elevation vector from two coordinates in space.
C  AZ2UV:  Recover unit vector from azimuth & elevation.
C  CHECKWARP: transforms surface into 2D and checks if points are
C          are not in the plane of the surface.
C  ESZONE  Saves the current contents of common blocks G1 G2 G4 G5
C          into SG1 SG2 SG4 SG5.
C  ERZONE  Recovers the saved contents of common blocks G1 G2 G4 G5
C          from SG1 SG2 SG4 SG5.
C  EASKGEOF: Asks for zone number & geometry file name. If a configuration
C            file has been read present a list of zone names.
C  ASKZONE: presents  a list of zones to select one from depending on topic.
C  ASKMULTIZONE: presents a list of zones to select one or more from.
C  EASKSUR: Presents a list of surfaces in a zone IZONE.
C  asksnode: Presents a list of layers/nodes within a surface.
C  FLNAME: Provides a name for a defined file type.
C  ZSID:   Returns a compact description in ZSDESC of the zone:surface.
C ZNARLIST takes an array (list) of zone indices and returns descr string.

C ************* CLRPRB
C CLRPRB clears the zone based memory in preparation for reading
C in a new model file.
      SUBROUTINE CLRPRB

      
      USE AIM2_InputData, ONLY: iAIM2,LAIM2
      USE AIM2, ONLY: AIM2_ClearAll

      IMPLICIT NONE
      
#include "building.h"
#include "model.h"
#include "site.h"

C geometry.h provides commons G0/G2/G4/G5/c20/precz.
#include "geometry.h"
#include "cfd.h"
#include "sbem.h"
#include "ipvdata.h"
#include "seasons.h"
#include "schedule.h"
C derived.h are derived areas and U values for zones and the model
#include "derived.h"

      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas

C << G7 and G8 need to be cleared >>

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

C iaplic(1) toggle for shading; iaplic(2) toggle for insolation.
C   toggle for assessment where:
C   1 is 'all_applicable', 0 is manual selection of surfaces.
C nsurfcalc nb of shaded surfaces, lstsfcalc() list of applicable surfaces.
C nsurfinso nb of insolation sources, isurfinso() list of insolation sources.
      common/ishdirec/iaplic(MCOM,2),nsurfcalc(MCOM),lstsfcalc(MCOM,MS),
     &     nsurfinso(MCOM),isurfinso(MCOM,MS)
      INTEGER :: iaplic,nsurfcalc,nsurfinso,lstsfcalc,isurfinso
      
C HOT3000:HVAC
C Flag indicating presence of HVAC models:
C ihvacflag=1 indicates HVAC models are active; 0 indicates no HVAC models.
      common/hvacinfo/ihvacflag,hvacfile
      INTEGER :: ihvacflag

C Simulation parameter presets.
      common/spfldes/spfdescr(MSPS)
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
      INTEGER :: isstday,isstmon,isfnday,isfnmon
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      INTEGER :: nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres


C HOT3000: BASESIMP.
      common/bsmtdef/iBSIMP(MCOM),LBSIMP(MCOM)
      INTEGER :: ibsimp


      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      INTEGER :: ic1,ie1,ict,ic2,ie2
      COMMON/C23/IFPNF,LPNF
      INTEGER :: ifpnf
      common/cctl/icascf(mcom)
      INTEGER :: icascf
      COMMON/PREC8/SLAT,SLON
      REAL :: slat,slon

C CFD
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      INTEGER :: ifcfd
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      INTEGER :: ncfdnd,icfdnd,nconf
C Variables for weekdays, and weekends.
C Assume: Mon=1, Tue=2, Wed=3, Thu=4, Fri=5, Sat=6, Sun=7
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2
      INTEGER :: idwe1,idwe2

C IPV description.
      common/IPVF/lipvdatf

C Anchor data in model.h.

C Non-linear thermophysical properties.
      COMMON/VTHP18/LNLTHP
      COMMON/VTHP31/INTHPS,INTHPZ(MCOM)
C 1D node distribution.
      COMMON/GR1D05/LGRD1D
      COMMON/GR1D06/IGR1D
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD

C 3D ground.
      COMMON/GRND100/GRND3D

C 3D zones.
      common/GR3D100/BLDG3D,ZONE3D(MCOM)
      common/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)

C Moisture.
      common/MOIST01/MSTROK,MSTRZN(MCOM)
      common/MOIST02/LMOIST(MCOM)

C Ground topology.
      COMMON/GTFIL/GTGEOM
      COMMON/GT/GTNAME

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
      

C CPCALC description
      common/CPCALC/icpcon,ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,
     &              sbh,pad,wvpe
      REAL :: ble,bwi,bhi,blox,bloy,bloz,orient,ra,sbh,pad,wvpe
      INTEGER :: icpcon,irt

C Images.
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image

      common/imagfi/noimg,iton
      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes

C Configuration control.
      common/cctlnm/ctldoc,lctlf

C Special materials & electrical network files.
      common/spmfxst/ispmxist,spflnam
      common/entfile/entflnam, ientxist
      INTEGER :: ispmxist,ientxist
C Path to SHOCC input files
C bSHOCCed     - logical flag whether project has SHOCC input
C bZoneSHOCCed - logical array whether zones have SHOCC input
C SHOCCshlFile - path to SHOCC .shl file for SHOCC'd project library
C SHOCCshzFile - path to SHOCC .shz file for each SHOCC'd zone
      common/SHOCCcfg/bSHOCCed,SHOCCshlFile,bZoneSHOCCed(mcom),
     &                SHOCCshzFile(mcom)
      logical bSHOCCed,bZoneSHOCCed
      character SHOCCshlFile*72,SHOCCshzFile*72

C Thermal bridge common block:
C nbrdg is the number of thermal bridge types in the zone
C psi is the linear thermal transmittance value,
C lngth is the length of the thermal bridge and
C ibrdg is an index defining the type of thermal bridge as follows:
C   1 to 8 (NOT metal cladding)
C   1 is "roof-wall", 2 is "wall-ground floor"
C   3 is "wall-wall (convex corner)", 4 is "wall-wall (concave corner)"
C   5 is "wall-floor (not ground floor)", 6 is "lintel above window or door"
C   7 is "Sill below window", 8 is "jamb at window or door"
C   9 to 15 (metal cladding)
C   9 is "roof-wall-mc", 10 is "wall-ground floor-mc"
C  11 is "wall-wall (corner)-mc", 12 is "wall-floor (not ground floor)-mc"
C  13 is "lintel above window or door-mc", 14 is "Sill below window-mc"
C  15 is "jamb at window or door-mc"
C  16 is for any other categories of thermal bridges

C losspercent is W/K for an alternative method  where the calculated heat flow
C   is augmented by a fixed percentage of the UA (fabric loss)
C totheatloss is W/K for all length*psi plus losspercent for the zone
C thbrpercent is the user defined fraction for thermal bridges used by
C   losspercent and is assumed to apply to all zones in the model.
      integer nbrdg, ibrdg
      real psi,lngth,losspercent,totheatloss,thbrpercent
      real uavtotal
      common/THRBRDG/nbrdg(MCOM),psi(MCOM,16),lngth(MCOM,16),
     &               ibrdg(MCOM,16),losspercent(MCOM),totheatloss(MCOM),
     &               thbrpercent,uavtotal(MCOM)

C IPV data comes via ipvdata.h

C Assumption about inserted window surface percentage and assumed
C door width and height.
      real defwininsert,defdoorwidth,defdoorheight
      common/ginsert/defwininsert,defdoorwidth,defdoorheight

      CHARACTER LCFD*72
      CHARACTER*72 LAPROB,LPNF

      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDNDD,LGDTAQ,LGRD1D,LNLTHP
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,LMOIST
      character hvacfile*72,LBSIMP*72
      character ctldoc*248,lctlf*72
      character GTGEOM*72,GTNAME*15,spflnam*72,entflnam*72,lipvdatf*72
      character*10 wkd1, wkd2
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,spfdescr*8
      LOGICAL INTHPS,INTHPZ,IGR1D,GRND3D
      LOGICAL BLDG3D,ZONE3D,MSTROK,MSTRZN
     
      INTEGER :: i,icc,ibe,ibu,j,ixs,ix,iivu,isystcurrent,imu
 
      call tstamp('>','PRJ: clearing model commons')
      
C Clear any allocated arrays      
      CALL DeallocateAllArrays

C Project level common blocks.

C Initial assumption is no SHOCC files.
      bSHOCCed=.false.
      SHOCCshlFile=' '

      lmodellog='job.notes'
      LPNF='UNKNOWN'
      LGDCVS='UNKNOWN'
      LGDCNC='UNKNOWN'
      LGDNDC='UNKNOWN'
      LGDTAQ='UNKNOWN'
      LGRD1D='UNKNOWN'
      LNLTHP='UNKNOWN'
      zonepth='./'
      netpth='./'
      ctlpth='./'
      aimpth='./'
      imgpth='./'
      radpth='./'
      dbspth='./'
      bsmpth='./'
      hvacpth='./'
      lradcf='UNKNOWN'
      cfgroot=' '
      LCTLF='UNKNOWN'
      LCNN='UNKNOWN'
      CTLDOC='no overall project control notes (yet)'
      GTGEOM='UNKNOWN'
      GTNAME='flat_booring'
      lipvdatf='UNKNOWN'
      bdmds='UNKNOWN'
      dmdsdesc='no dispersed demands notes (yet)'

C << Consider clearing the calendar plus dependencies in
C << control and operations which rely on daytypes.
      idwe1=6
      idwe2=7
      wkd1='Saturday'
      wkd2='Sunday'
      spflnam='UNKNOWN'
      ispmxist=0
      entflnam='UNKNOWN'
      ientxist=0
      igupgrade=0   ! no opinion about upgrading model files

C Initialize HVAC flag to indicate no HVAC models active. This
C will be reset if `*hvac' included in .cfg.
      ihvacflag=0
      hvacfile='UNKNOWN'

C HOT3000: AIM-2.
      iAIM2=0
      LAIM2='UNKNOWN'
      CALL AIM2_ClearAll

C Clear pre-simulation sets.
      nsset= 0
      isstup= 0
      isbnstep= 0
      ispnstep= 0
      isavgh=0
      issave= -1
      sipvres=' '
      do 39 i=1,MSPS
        spfdescr(i)=' '
        sblres(i)=' '
        sflres(i)=' '
        splres(i)=' '
        smstres(i)=' '
        selres(i)=' '
        isstday(i)=0
        isstmon(i)=0
        isfnday(i)=0
        isfnmon(i)=0
        seahddwk(i)=0.0
        seacddwk(i)=0.0
        seahddtot(i)=0.0
        seacddtot(i)=0.0
        wkdiff(i)=0.0
        iwkbest(i)=0
        iwkbstrt(i)=0
        wkheatdd(i)=0.0
        wkcooldd(i)=0.0
  39  continue

C Clear IPV strings and number of assessmments.
      ipvtitl=' '
      ipvvers=' '
      ipvsynop=' '
      ipvsimu=' '
      nipvassmt=0
      nipvdispjd=0
      ipvastjd(1)=0
      ipvafnjd(1)=0
      ipvdispjd(1)=0

C Clear anchors.
      NALOC=0
      do 40 i=1,20
        ALOCLBL(i)='undefined'
        ALOCTYP(i)='unkn'
        IALOC(i)=0
        do 400 j=1,99
          lstanchr(i,j)=0
  400   continue
  40  continue

C Set initial primary conversions and emissions.
      ipconv=0
      pcnvht=1.0
      pcnvcl=1.0
      pcnvlt=1.0
      pcnvfn=1.0
      pcnvhw=1.0
      phtco2=0.0
      phtnox=0.0
      phtsox=0.0
      pclco2=0.0
      pclnox=0.0
      pclsox=0.0
      pltco2=0.0
      pltnox=0.0
      pltsox=0.0
      pfnco2=0.0
      pfnnox=0.0
      pfnsox=0.0
      pspco2=0.0
      pspnox=0.0
      pspsox=0.0
      phwco2=0.0
      phwnox=0.0
      phwsox=0.0

C Initial data for CPCALC.
      icpcon=0
      ble=0.00
      bwi=0.00
      bhi=0.00
      blox=0.00
      bloy=0.00
      bloz=0.00
      orient=0.00
      irt=0
      ra=0.00
      sbh=0.00
      pad=0.00
      wvpe=0.00

C Clear derived model areas and U-values.
      xfloor = 0.0; xexposed = 0.0; xvexposed = 0.0
      xvthk = 0.0; xtoground = 0.0; xuavgtran = 0.0
      xuavwall = 0.0; xuavground = 0.0
      xfndthkg = 0.0; xperimlength = 0.0
      xuavslproof = 0.0; xuavfltroof = 0.0; xuavgsky = 0.0 
      xareatran = 0.0; xareawall = 0.0
      xareaslproof = 0.0; xareafltroof = 0.0; xareaskylt = 0.0

C Initialize the indices.
      INTHPS=.FALSE.
      IGR1D=.FALSE.
      GRND3D=.FALSE.
      BLDG3D=.FALSE.
      MSTROK=.FALSE.

C Assume there is not BSim-CFD conflation in any of the zones.
      NCONF=0
      do 5 i=1,MNZ
        icfdnd(i)=0
  5   continue

C Clear image related data.
      noimg=0
      iton=0
      do 6,IX=1,MIMG
        imgfmt(IX)=' '
        imgfoc(IX)=' '
        limgfil(IX)=' '
        imgdoc(IX)='-'
  6   continue

      IAIRN=0
      LAPROB='UNKNOWN'

      sitelat=50.0
      SLAT=50.
      sitelongdif=0.0
      SLON=0.

C Assumptions about doors and windows.
      defwininsert=15.0
      defdoorwidth=0.8
      defdoorheight=2.1

C Season data.
      ihaveseason=0
      ia1wins=0; ia1winf=0; ia1sprs=0; ia1sprf=0
      iasums=0;  iasumf=0;  ia2sprs=0; ia2sprf=0; ia2wins=0; ia2winf=0
      is1wins=0; is1winf=0; is2wins=0; is2winf=0; is1sprs=0; is1sprf=0
      is2sprs=0; is2sprf=0; is1sums=0; is1sumf=0
      hddw=1.0; cddw=1.0; radw=1.0; hddbaset=0.0; cddbaset=0.0

C Zone level common blocks.
      do 42 IX = 1,MCOM

C Per-zone areas and U-values.
        uavgtran(IX) = 0.0
        uavwall(IX) = 0.0
        uavslproof(IX) = 0.0
        uavfltroof(IX) = 0.0
        uavgsky(IX) = 0.0
        uavground(IX) = 0.0
        areatran(IX) = 0.0
        areawall(IX) = 0.0
        areaslproof(IX) = 0.0
        areafltroof(IX) = 0.0
        areaskylt(IX) = 0.0
        areaground(IX) = 0.0
        exposed(IX) = 0.0
        vexposed(IX) = 0.0
        izsfloor(IX) = 0
        izsceil(IX) = 0

        do 43 IXS = 1,MS
          SNAME(IX,IXS)=' '   ! clear surface name
          zboundarytype(IX,IXS,1)=0  ! clear surface boundary
          zboundarytype(IX,IXS,2)=0
          zboundarytype(IX,IXS,3)=0
          zhasglaze(ix,ixs)=0.0
          zhasdoor(ix,ixs)=0.0
          isznbedges(ix,ixs)=0
          do 54 iivu=1,MV
            iszlist(ix,ixs,iivu)=0
            szcoords(ix,ixs,1)=0.0
            szcoords(ix,ixs,2)=0.0
            szcoords(ix,ixs,3)=0.0
  54      continue
   43   continue

C Clear obstructions in all zones.
        do 55 ibu=1,MB
          XOB(ix,ibu) = 0.0
          YOB(ix,ibu) = 0.0
          ZOB(ix,ibu) = 0.0
          DXOB(ix,ibu) = 0.0
          DYOB(ix,ibu) = 0.0
          DZOB(ix,ibu) = 0.0
          BANGOB(ix,ibu,1) = 0.0
          BANGOB(ix,ibu,2) = 0.0
          BANGOB(ix,ibu,3) = 0.0
          OPOB(ix,ibu)=0.0
          BLOCKNAME(ix,ibu) = ' '
          LNBLOCKNAME(ix,ibu) = 0
          BLOCKMAT(ix,ibu) = ' '
          LNBLOCKMAT(ix,ibu) = 0
          BLOCKTYP(ix,ibu) = 'obs '
          do 56 ibe=1,8
            XBP(ix,ibu,ibe)=0.0
            YBP(ix,ibu,ibe)=0.0
            ZBP(ix,ibu,ibe)=0.0
  56      continue
  55    continue

C Clear SHOCC zone information as well.
        bZoneSHOCCed(IX)=.false.
        SHOCCshzFile(IX)=' '

        NZSUR(IX)=0
        NZTV(IX)=0
        NBWALLS(IX)=0
        NCCODE(IX)=0
        NDP(IX)=3
        IDPN(IX,1)=0
        IDPN(IX,2)=0
        IDPN(IX,3)=0
        CTYPE(IX)=' '
        shape(IX)=' '
        gversion(ix)=1.0
        rotateit(ix,1)=0.0
        rotateit(ix,2)=0.0
        rotateit(ix,3)=0.0
        rotateit(ix,4)=0.0
        rotateit(ix,5)=0.0
        rotateit(ix,6)=0.0
        zorigin(ix,1)=0.0
        zorigin(ix,2)=0.0
        zorigin(ix,3)=0.0
        zsize(ix,1)=0.0
        zsize(ix,2)=0.0
        zsize(ix,3)=0.0
        znbglz(ix) = 0
        znbdoor(ix) = 0
        znbmass(ix) = 0
        do 158 imu=1,4
          zdatamass(ix,imu,1)=0.0
          zdatamass(ix,imu,2)=0.0
          zdatamass(ix,imu,3)=0.0
          zdatamass(ix,imu,4)=0.0
          zdatamass(ix,imu,5)=0.0
          zdatamass(ix,imu,6)=0.0
          zdatamass(ix,imu,7)=0.0
          ztextmass(ix,imu,1)=' '
          ztextmass(ix,imu,2)=' '
          ztextmass(ix,imu,3)=' '
  158   continue
        zdesc(IX)=' '
        lnzdesc(IX)=0
        zname(IX)=' '
        lnzname(IX)=0

C << consider clearing the P3N commons >>
        LPROJ(IX)='UNKNOWN'
        LGEOM(IX)='UNKNOWN'
        LTHRM(IX)='UNKNOWN'
        IVF(IX)=0
        LVIEW(IX)='UNKNOWN'
        ISI(IX)=0
        LSHAD(IX)='UNKNOWN'
        IHC(IX)=0
        LHCCO(IX)='UNKNOWN'

C HOT3000: BASESIMP.
        iBSIMP(I) = 0
        LBSIMP(I)='UNKNOWN'
        ITW(IX)=0
        LTWIN(IX)='UNKNOWN'
        ICGC(IX)=0
        LCGCIN(IX)='UNKNOWN'
        IOBS(IX)=0
        ZOBS(IX)  ='UNKNOWN'
        L3DCVS(IX)='UNKNOWN'
        L3DCNC(IX)='UNKNOWN'
        L3DNDC(IX)='UNKNOWN'
        L3DTAQ(IX)='UNKNOWN'
        IFCFD(IX)=0
        LCFD(IX)  ='UNKNOWN'
        icascf(IX)=0
        ICAAS(IX)=0
        INTHPZ(IX)=.FALSE.
        ZONE3D(IX)=.FALSE.
        MSTRZN(IX)=.FALSE.
        LMOIST(IX)='UNKNOWN'

        iaplic(IX,1)=0   ! reset shanding and insolation directives
        iaplic(IX,2)=0
        nsurfcalc(IX)=0
        nsurfinso(IX)=0
   42 continue

C Clear the connection list.
      DO 32 ICC=1,MCON
        IC1(ICC)=0
        IE1(ICC)=0
        ICT(ICC)=-1   ! Set to -1 to signal connection type is not yet defined.
        IC2(ICC)=0
        IE2(ICC)=0
        SSPARENT(icc)=' '
        SSNAME(icc)=' '
        SSOTF(icc)=' '
        SSMLCN(icc)=' '
        ssmlcindex(icc)=0  ! no matching MLC
        SSVFC(icc)=' '
        SSOTHER(icc,1)='UNKNOWN'
        SSOTHER(icc,2)='0'
        SSOTHER(icc,3)='0'
        SSUSE(icc,1)='-'
        SSUSE(icc,2)='-'
        ssna(icc)=0.0
        sspazi(icc)=0.0
        sspelv(icc)=0.0
        ssperim(icc)=0.0
        ssthick(icc)=0.0
   32 CONTINUE
      icfgv=4  ! set cfg file version to latest type
      NCON=0
      ncomp=0
      siteexposureindex=1
      groundrefl=0.2
      groundreflmodel=1
      snowgroundrefl=0.4
      DO 60 I=1,12
        groundreflmonth(I)=groundrefl
        dayswithsnow(I)=0
   60 CONTINUE
      SNFNAM=' '

C Clear the linear thermal bridges variables
      DO 3 I=1,16
        DO 2 IX=1,MCOM
          psi(IX,I) = 0.0
          lngth(IX,I) = 0.0
          losspercent(IX) = 0.0
          thbrpercent = 0.0
          totheatloss(ix) = 0.0
          uavtotal(ix) = 0.0
    2   CONTINUE
    3 CONTINUE

C Clear sbem string arrays (from sbem.h).
      pjname=' not yet defined'
      baddress=' not yet defined'
      bcity=' not yet defined'
      buildingPC =' not yet defined'
      ownername=' not yet defined'
      ownertel=' not yet defined'
      owneraddr=' not yet defined'
      ownercity=' not yet defined'
      ownerPC=' not yet defined'
      certifname=' not yet defined'
      certTel=' not yet defined'
      certaddress=' not yet defined'
      certcity=' not yet defined'
      certPC=' not yet defined'
      UPRN='000000000000'
      Accr_Scheme=' not yet defined'
      assessRegNumber='ABCD123456'
      empl_Trading_name=' not yet defined'
      addr_empl_Trading=' not yet defined'
      party_disclosure=' not yet defined'
      qualifications_assessor=' NOS3'
      insurer_Company=' not yet defined'
      policyNumberInsurance=' not yet defined'
      pjLevComplexity='Undefined'
      do 4 isystCurrent=1,MNS
        ductwork(isystCurrent)='No, use default leakage'
        AHUleakage(isystCurrent)='No, use default leakage'
        duct_tDLd(isystCurrent)=0.15
        AHU_tDLd(isystCurrent)=0.06
 4    continue

C Also integers from sbem.h
      Y_inspect=2010
      M_inspect=1
      D_inspect=1
      S_Yinsur=2010
      S_Minsur=1
      S_Dinsur=1
      E_Yinsur=2011
      E_Minsur=1
      E_Dinsur=1
      pi_limit=0
      INOTI=0  ! assume we have an original model
      return
      end

C ***** Find toggles for active description buttons and check to
C see if there are images associated with descriptive buttons.
      subroutine cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
#include "building.h"

C Passed parameters because these are also used by c function opencfg
#ifdef OSI
      integer icfg_type  ! model cfg type
      integer icfgz      ! if non-zero then there are zones
      integer icfgn      ! if non-zero then there are networks
      integer icfgc      ! if non-zero then cfg file known
      integer icfgdfn    ! if non-zero then cdf domain exists
      integer iicfgz     ! there are zone related images
      integer iicfgn     ! there are network related images
      integer iicfgc     ! there are control related images
      integer iicfgdfn   ! there are cfd related images
#else
      integer*8 icfg_type,icfgz,icfgn,icfgc,icfgdfn,iicfgz
      integer*8 iicfgn,iicfgc,iicfgdfn
#endif

      integer ncomp,ncon
      common/C1/NCOMP,NCON
      common/C6/INDCFG
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      common/cctlnm/ctldoc,lctlf
      common/entfile/entflnam, ientxist
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton

      CHARACTER ctldoc*248,lctlf*72,LCFD*72,entflnam*72
      character LAPROB*72

      icfg_type = INDCFG
      icfgz = 0
      if(INDCFG.eq.1.or.INDCFG.eq.3)icfgz = 1
      icfgn = 0
      if(INDCFG.eq.2.or.INDCFG.eq.3)icfgn = 1
      if(ientxist.gt.0)icfgn = 1
      if(IAIRN.ge.1)icfgn = 1
      if(LCTLF(1:7).eq.'UNKNOWN')then
        icfgc = 0
      else
        icfgc = 1
      endif

C Loop through all zones to see if there is a domain flow description.
      icfgdfn = 0
      if(NCOMP.gt.0)then
        do 42 i=1,NCOMP
          if(LCFD(i)(1:2).eq.'  '.or.LCFD(i)(1:4).eq.'UNKN')then
          else
           icfgdfn = 1
           goto 43
          endif
  42    continue
  43    continue
      endif
      if(noimg.eq.0)then
        iicfgz=0
        iicfgn=0
        iicfgc=0
        iicfgdfn=0
      else
        if(icfgz.ne.0)then
          iicfgz=0
          do 49 img=1,noimg
              if(imgfoc(img)(1:4).eq.'FZON')iicfgz=iicfgz+1
  49      continue
        endif
        if(icfgn.ne.0)then
          iicfgn=0
          do 48 img=1,noimg
              if(imgfoc(img)(1:4).eq.'FNET')iicfgn=iicfgn+1
  48      continue
        endif
        if(icfgc.ne.0)then
          iicfgc=0
          do 47 img=1,noimg
              if(imgfoc(img)(1:4).eq.'FCTL')iicfgc=iicfgc+1
  47      continue
        endif
        if(icfgdfn.ne.0)then
          iicfgdfn=0
          do 46 img=1,noimg
              if(imgfoc(img)(1:4).eq.'FDFS')iicfgdfn=iicfgdfn+1
  46      continue
        endif
      endif

      return
      end

C ***** Notice that an active description button has been pressed.
      subroutine cfgpk(act)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "control.h"
      common/pophelp/h(60)

      integer ncomp,ncon
      common/C1/NCOMP,NCON
      COMMON/C23/IFPNF,LPNF
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      common/entfile/entflnam, ientxist

C Control
      common/cctlnm/ctldoc,lctlf

C Images.
      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton
      COMMON/CONTM/CNTMFIL,CNTMDESC,NTSTEPC
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)

      character h*72,act*1
      character entflnam*72
      CHARACTER LCFD*72
      CHARACTER LAPROB*72,LPNF*72,CNTMFIL*72
      CHARACTER ctldoc*248,lctlf*72
      character CNTMDESC*124,CONTMNAM*12

      call usrmsg('Scanning for related images. Please wait...',' ','-')
      if(act.eq.'r')then

C Registration, show all available images.
        call imgdisp(0,'****',ier)
        h(1)='The current model is named:'
        write(h(2),'(a)')modeltitle
        h(3)='The model is documented in: '
        write(h(4),'(a)')lmodellog(1:70)
        h(5)=' '
        write(h(6),'(a,i2,a)')'There are ',noimg,' linked images.'
        h(7)=' '
        h(8)='There are no zones, networks or control defined yet. '
        CALL PHELPD('registration button popup',8,'-',0,0,IER)
      elseif(act.eq.'z')then

C Zone button, show zone/site/gridding related images.
        call imgdisp(1,'FZON',ier)
        h(1)='The current model is named:'
        write(h(2),'(a)')modeltitle
        h(3)=' '
        write(h(4),'(a,i3,a,i4,a)')'It is composed of',NCOMP,
     &    ' zones and',NCON,' surfaces.'
        h(5)=' '
        h(6)='The model is documented in: '
        write(h(7),'(a)')lmodellog(1:70)
        nh=7
        do 42 i=1,NCOMP
          if(LCFD(i).eq.'UNKNOWN')then
          else
            nh=nh+1
            write(h(nh),'(a,a)')' CFD via ',LCFD(i)(1:56)
          endif
  42    continue
        CALL PHELPD('zones button popup',nh,'-',0,0,IER)
      elseif(act.eq.'n')then

C Show network related images.
        call imgdisp(1,'FNET',ier)
        h(1)='Active networks:'
        nh=1
        if(LAPROB(1:2).eq.'  ')then
        else
          nh=nh+1
          write(h(nh),'(a,a)')'  Flow via ',LAPROB(1:56)
        endif
        if(NOCNTM.EQ.1)then
          nh=nh+1
          write(h(nh),'(a,a)')'  Contaminants via ',CNTMFIL(1:56)
        endif
        if(LPNF(1:2).eq.'  '.or.LPNF(1:4).eq.'UNKN')then
        else
          nh=nh+1
          write(h(nh),'(a,a)')'  Plant via ',LPNF(1:56)
        endif
        if(ientxist.gt.0)then
          nh=nh+1
          write(h(nh),'(a,a)')'  Electrical via ',entflnam(1:56)
        endif
        CALL PHELPD('networks button popup',nh,'-',0,0,IER)
      elseif(act.eq.'c')then

C Show control related images (assumes that control has already been scanned).
        call imgdisp(1,'FCTL',ier)
        if(ncf.gt.0.or.ncl.gt.0.or.ncc.gt.0.or.ngf.gt.0)then
          write(h(1),'(a,a)')'System control via ',LCTLF(1:50)
          nh=1
          if(ncf.gt.0)then
            nh=nh+1
            write(h(nh),'(3a,i2,a)')' Zones - ',znctldoc(1:45),
     &      '(',ncf,' loops)'
          endif
          if(ncl.gt.0)then
            nh=nh+1
            write(h(nh),'(3a,i2,a)')' Plant - (',plctldoc(1:45),
     &      '(',ncl,' loops)'
          endif
          if(ncc.gt.0)then
            nh=nh+1
            write(h(nh),'(3a,i2,a)')' Flow - ',flctldoc(1:45),
     &       '(',ncc,' loops)'
          endif
          if(ngf.gt.0)then
            nh=nh+1
            write(h(nh),'(3a,i2,a)') ' Global - ',glctldoc(1:45),
     &       '(',ngf,' loops)'
          endif
          CALL PHELPD('control button popup',nh,'-',0,0,IER)
        else
          h(1)='This model has no control definitions.'
          CALL PHELPD('control button popup',1,'-',0,0,IER)
        endif
      elseif(act.eq.'d')then

C Domain flow button, show cfd image.
        call imgdisp(1,'FDFS',ier)
      endif
      return
      end

C ************** EGOMST
C EGOMST:  Reads site obstruction data as ASCII strings, with or without
C range checking and printed summary. An obstruction block name (up to
C 12 characters) may be included as an option and is returned via BLOCKNAME.
      SUBROUTINE EGOMST(IUNIT,ICOMP,LOBS,IR,ITRC,ITRU,IER)
#include "building.h"
C geometry.h provides common block GS5/GS6/GS7/GS8
#include "geometry.h"
#include "espriou.h"

      integer lnblnk  ! function definition

C Error messages (for use by low level I/O calls)
      common/dllerr/dllsubr,dllmesg

      CHARACTER LOBS*72,OUTSTR*124,WORD*32
      character dllsubr*12,dllmesg*124,outs*124
      logical dll
      integer NGXD,NGZD  ! dummy values for depreciated NGX & NGZ
      real VX,VY,VZ   ! local values for XOB YOB ZOB
      integer llbm    ! length of block material

C Check if running in dll mode.
      call isadll(dll)
      IER=0

      CALL EFOPSEQ(IUNIT,LOBS,1,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'Obstructions file ',LOBS(1:lnblnk(LOBS)),
     &    ' could not be opened.'
        if(dll)then
          dllsubr='EGOMST'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(itru,outs)
          IER=1
          RETURN
        endif
      ENDIF
      write(currentfile,'(a)') LOBS(1:lnblnk(LOBS))

C Read past site information from the file and check for range errors.
C Read lines from file, discarding comments, check the number of items
C in the data.
      CALL STRIPC(IUNIT,OUTSTR,2,ND,1,'line 1',IER)
      IF(IER.NE.0)goto 99

C Read number of obstruction blocks and check for range errors.
      CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'number of blocks',IER)
      K=0
      CALL EGETWI(OUTSTR,K,nbo,1,MB,'F','number of obstr',IER)
      IF(IER.NE.0)GOTO 99
      nbobs(icomp)=nbo  ! remember how many for this zone

C Read each obstruction block, if 7 items found then unattributed, if
C 8 items assume the last is a name for the obstruction block, if 9 items
C then the last is the construction attribute. If no  name & or construction
C then assign the block index as the name via internal read.

C Note: legacy zone obstruction files only contain the single type
C of obstruction with one angle of rotation. Remember the number
C of tokens in variable ND.
      DO 10 I=1,nbobs(icomp)
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'obstruction block',IER)
        IF(IER.NE.0)GOTO 99
        IF(ND.eq.7.OR.ND.eq.8.OR.ND.eq.9.OR.ND.eq.10)THEN
          K=0
          CALL EGETWR(OUTSTR,K,VX,-999.,998.,'W','obs X org',IER)
          CALL EGETWR(OUTSTR,K,VY,-999.,998.,'W','obs Y org',IER)
          CALL EGETWR(OUTSTR,K,VZ, -99., 99.,'W','obs Z org',IER)
          XOB(icomp,I)=VX
          YOB(icomp,I)=VY
          ZOB(icomp,I)=VZ
          CALL EGETWR(OUTSTR,K,VX,0.,150.,'W','obs X dis',IER)
          CALL EGETWR(OUTSTR,K,VY,0.,150.,'W','obs Y dis',IER)
          CALL EGETWR(OUTSTR,K,VZ,0.,150.,'W','obs Z dis',IER)
          DXOB(icomp,I)=VX
          DYOB(icomp,I)=VY
          DZOB(icomp,I)=VZ
          CALL EGETWR(OUTSTR,K,VX,-359.,359.,'W','obs rot ang',IER)
          BANGOB(icomp,I,1)=VX
          IF(ND.EQ.7)THEN
            OPOB(icomp,I)=1.0
            IF(I.LE.9)THEN
              WRITE(BLOCKNAME(ICOMP,I),77)I
   77         FORMAT('B',I1)
              LNBLOCKNAME(ICOMP,I)=lnblnk(BLOCKNAME(ICOMP,I))
            ELSEIF(I.GT.9)THEN
              WRITE(BLOCKNAME(ICOMP,I),78)I
   78         FORMAT('B',I2)
              LNBLOCKNAME(ICOMP,I)=lnblnk(BLOCKNAME(ICOMP,I))
            ENDIF
            BLOCKMAT(ICOMP,I)='NONE'
            LNBLOCKMAT(ICOMP,I)=4
            BLOCKTYP(ICOMP,I)='obs '
          ELSEIF(ND.EQ.8)THEN
            OPOB(icomp,I)=1.0
            CALL EGETW(OUTSTR,K,WORD,'W','obs blk name',IFLAG)
            BLOCKNAME(ICOMP,I)=WORD(1:12)
            LNBLOCKNAME(ICOMP,I)=lnblnk(BLOCKNAME(ICOMP,I))
            BLOCKMAT(ICOMP,I)='NONE'
            LNBLOCKMAT(ICOMP,I)=4
            BLOCKTYP(ICOMP,I)='obs '
          ELSEIF(ND.EQ.9)THEN

C Get name of obstruction block and its construction (which might
C contain spaces).
            OPOB(icomp,I)=1.0
            CALL EGETW(OUTSTR,K,WORD,'W','obs blk name',IFLAG)
            BLOCKNAME(ICOMP,I)=WORD(1:12)
            LNBLOCKNAME(ICOMP,I)=lnblnk(BLOCKNAME(ICOMP,I))
            CALL EGETP(OUTSTR,K,WORD,'W','obs mat name',IFLAG)
            write(BLOCKMAT(ICOMP,I),'(a)') WORD(1:lnblnk(WORD))
            LNBLOCKMAT(icomp,i)=lnblnk(BLOCKMAT(icomp,i))
            BLOCKTYP(ICOMP,I)='obs '
          ELSEIF(ND.GE.10)THEN

C Get opacity and name of obstruction block and its construction (which might
C contain spaces).
            CALL EGETWR(OUTSTR,K,VX,0.,1.,'W','obs opacity',IER)
            OPOB(icomp,I)=VX
            CALL EGETW(OUTSTR,K,WORD,'W','obs blk name',IFLAG)
            BLOCKNAME(ICOMP,I)=WORD(1:12)
            LNBLOCKNAME(ICOMP,I)=lnblnk(BLOCKNAME(ICOMP,I))
            CALL EGETP(OUTSTR,K,WORD,'W','obs mat name',IFLAG)
            write(BLOCKMAT(ICOMP,I),'(a)') WORD(1:lnblnk(WORD))
            LNBLOCKMAT(icomp,i)=lnblnk(BLOCKMAT(icomp,i))
            BLOCKTYP(ICOMP,I)='obs '
          ENDIF
        ELSE
          CALL USRMSG(' mismatch block descr in',OUTSTR,'W')
        ENDIF

   10 CONTINUE

C Read surface test grid information and check ranges.
      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'obs grid infor',IER)
      IF(IER.NE.0)GOTO 99
      K=0
      CALL EGETWI(OUTSTR,K,NOX(icomp),4,MOX,'F','surf shd grid X',IER)
      CALL EGETWI(OUTSTR,K,NOZ(icomp),4,MOZ,'F','surf shd grid Z',IER)
      if(ND.eq.4)then
       CALL EGETWI(OUTSTR,K,NGXD,4,10,'F','win grid X depreciated',IER)
       CALL EGETWI(OUTSTR,K,NGZD,4,10,'F','win grid Z depreciated',IER)
      endif

C Close site file.
   99 CALL ERPFREE(IUNIT,ISTAT)

C Trace.
      IF(ITRC.GT.0)THEN
        CALL EDISP(ITRU,' ')
        CALL EDISP(ITRU,'Details of obstruction blocks:')
        WRITE(OUTSTR,997)NOX(icomp),NOZ(icomp)
  997   FORMAT('Shading based on a grid of ',
     &           I3,' by ',I3,' for surfaces.')
        CALL EDISP(ITRU,OUTSTR)
        WRITE(OUTSTR,9996)nbobs(icomp)
 9996   FORMAT('There are ',I2,
     &         ' obstructions associated with the zone.')
        CALL EDISP(ITRU,OUTSTR)
        CALL EDISP(ITRU,
     &  'Block X- Y- Z- coords DX- DY- DZ- values Orient Opacity')
        DO 9995 I=1,nbobs(icomp)
          llbm=LNBLOCKMAT(ICOMP,I)
          llbn=LNBLOCKNAME(ICOMP,I)
          WRITE(OUTSTR,9994)I,XOB(ICOMP,I),YOB(ICOMP,I),ZOB(ICOMP,I),
     &      DXOB(ICOMP,I),DYOB(ICOMP,I),DZOB(ICOMP,I),
     &      BANGOB(ICOMP,I,1),OPOB(ICOMP,I),BLOCKNAME(ICOMP,I)(1:llbn),
     &      BLOCKMAT(ICOMP,I)(1:llbm)
 9994     FORMAT(I3,7F8.1,F5.2,' ',a,' ',a)
          CALL EDISP(ITRU,OUTSTR)
 9995   CONTINUE
        CALL EDISP(ITRU,' ')
      ENDIF
      RETURN

      END


C ************** MKGOMST
C MKGOMST: Creates an annotated site obstruction file based
C on passed parameters and the contents of common GS5.
      SUBROUTINE MKGOMST(IAF,ZF,ICOMP,IER)
#include "building.h"
#include "model.h"
C geometry.h provides common block GS5/GS6/GS7/GS8
#include "geometry.h"

      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN

      CHARACTER ZF*72  ! zone file name
      character outl*124,outld*124
      integer llbm  ! length of block material

C Create/Open the file.
      CALL EFOPSEQ(IAF,ZF,3,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

C Write out a file header.
      WRITE(IAF,31)ZF(1:lnblnk(ZF)),
     &  LGEOM(ICOMP)(1:lnblnk(LGEOM(ICOMP)))
  31  FORMAT('# site obstruction file defined in ',a,/,
     &       '# associated with zone geometry file ',a)

C Write all site information.
      WRITE(IAF,'(A)',IOSTAT=IOS,ERR=3)
     &  ' 0. 0.   # dummy values for site position'

C Write number of blocks.
      WRITE(IAF,'(I5,A)',IOSTAT=IOS,ERR=3) nbobs(icomp),
     &  '    # no obstruction blocks'

C For each block write info, use space as item separation.
      WRITE(IAF,'(a)')
     &'# origin X Y Z, width, depth, height, rot ang, opac descr, cons'
      DO 10 I=1,nbobs(icomp)
        llbm=LNBLOCKMAT(ICOMP,I)
        llbn=LNBLOCKNAME(ICOMP,I)
        WRITE(outl,34,IOSTAT=IOS,ERR=3)XOB(icomp,I),YOB(icomp,I),
     &    ZOB(icomp,I),DXOB(icomp,I),DYOB(icomp,I),DZOB(icomp,I),
     &    BANGOB(icomp,I,1),OPOB(icomp,I),
     &    BLOCKNAME(ICOMP,I)(1:llbn),BLOCKMAT(ICOMP,I)(1:llbm),I
  34    FORMAT(7F9.3,F6.2,2X,A,1X,A,1X,'# block ',I3)
        call SDELIM(outl,outld,'S',IW)
        WRITE(IAF,'(a)',IOSTAT=IOS,ERR=3) outld(1:lnblnk(outld))
   10 CONTINUE

C Write surface test grid information.
      WRITE(IAF,'(A)',IOSTAT=IOS,ERR=3) '# grid surf X surf Z'
      WRITE(IAF,'(2I6)',IOSTAT=IOS,ERR=3) NOX(icomp),NOZ(icomp)

C Close site file.
      CALL ERPFREE(IAF,ISTAT)
      RETURN

   3  if(IOS.eq.2)then
        call edisp(iuout,
     &    'MKGOMST: no permission to write obstruction data.')
      else
        call edisp(iuout,'MKGOMST: error writing obstruction data.')
      endif
      CALL ERPFREE(IAF,ISTAT)
      ier=1
      return

      END

C ************* ERMRT
C ERMRT Read the MRT sensor definition file. IC is the zone index
C (for use in mrt).  IER=0 OK, IER=1 problem.
      SUBROUTINE ERMRT(ITRC,ITRU,IUF,LMRT,IC,IER)
#include "building.h"
#include "geometry.h"
#include "espriou.h"

      integer lnblnk  ! function definition

      common/TOTAL/nsurmf, NGRID, NTRPOL
      
      integer ncub,ivfok
      real XOC,YOC,ZOC,DXC,DYC,DZC,CANG
      character CUBN*6
      common/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)
      common/MRTF/FFL(MCUB,6,MSM),VEWF(MS,MS)
      common /cube5/fstgrp,fstsur,lstsur,strtgr

C Error subroutine and error details for dll mode.
      common/dllerr/dllsubr,dllmesg

      DIMENSION VA(MSM)

      CHARACTER LMRT*72,LGEO*72,OUTSTR*124,WORD*20,outs*124
      character dllsubr*12,dllmesg*124
      integer fstgrp,fstsur,lstsur,strtgr
      logical dll

C Check if running in dll mode.
      call isadll(dll)

      CALL EFOPSEQ(IUF,LMRT,1,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'Viewfactor file ',LMRT(1:lnblnk(LMRT)),
     &    ' could not be opened.'
        if(dll)then
          dllsubr='ERMRT'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(itru,outs)
          IER=1
          RETURN
        endif
      ENDIF
      write(currentfile,'(a)') LMRT(1:lnblnk(LMRT))

C Read first line which must be *VIEWFACTORS
      CALL STRIPC(IUF,OUTSTR,0,ND,1,'line 1',IER)
      if(IER.NE.0)return
      if(OUTSTR(1:12).NE.'*VIEWFACTORS')then
        write(outs,'(3a)') LMRT(1:lnblnk(LMRT)),
     &    ' is not a view factor file.'
        if(dll)then
          dllsubr='ERMRT'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(itru,outs)
          IER=1
          RETURN
        endif
      endif

C Read name of geometry file this file matches.  Information
C currently not used - mrt accesses configuration file.
      CALL STRIPC(IUF,OUTSTR,0,ND,1,'geom file',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','*GEOM tag',IFLAG)
      if(WORD(1:5).eq.'*GEOM')then
        call EGETRM(OUTSTR,K,LGEO,'W','assoc geom file',IER)
      else
        LGEO='UNKNOWN'
      endif

C Read number of sensors.
      CALL STRIPC(IUF,OUTSTR,0,ND,1,'no sensors & surfs',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NCUB,0,MCUB,'F','number mrt sensors',IER)
      CALL EGETWI(OUTSTR,K,NZS,1,MS,'F','number mrt zone surfs',IER)
      CALL EGETWI(OUTSTR,K,IZINDX,1,MCOM,'F','mrt zone index',IER)
      if(ier.ne.0)return
      if(IZINDX.ne.IC)then
        write(outs,'(3a)') LMRT(1:lnblnk(LMRT)),
     &    ' linked to wrong zone index.'
        if(dll)then
          dllsubr='ERMRT'
          dllmesg=outs
          ier=2
          CALL ERPFREE(iuf,ISTAT)
          return
        else
          call edisp(itru,outs)
          IER=1
          CALL ERPFREE(iuf,ISTAT)
          return
        endif
      endif
      if(NZS.ne.NZSUR(ic))then
        write(outs,'(3a)') LMRT(1:lnblnk(LMRT)),
     &    ' has different surface count to zone geometry.'
        if(dll)then
          dllsubr='ERMRT'
          dllmesg=outs
          ier=2
          CALL ERPFREE(iuf,ISTAT)
          return
        else
          call edisp(itru,outs)
          IER=1
          CALL ERPFREE(iuf,ISTAT)
          return
        endif
      endif
      DO 330 I = 1,NZS

C Read surface data line(s).
        IRV=NZS
        CALL EGETWRA(IUF,VA,IRV,0.,0.9999,'F','view factors',IER)
        DO 14 J=1,NZS
          VEWF(I,J)=VA(J)
   14   CONTINUE
  330 CONTINUE

C Skip past the row sums.
      DO 331 I = 1,NZS
        CALL STRIPC(IUF,OUTSTR,0,ND,1,'row sums',IER)
  331 CONTINUE

C Write out trace if requested.
C MS senstive, change is MS>90.
      if(itrc.gt.0)then
        call edisp(itru,' Black body view factors')
        if(NZS.le.22)then
          write(outs,'(4X,22I5)')(I,I=1,NZS)
          call edisp(itru,outs)
        elseif(NZS.ge.23.and.NZS.le.44)then
          write(outs,'(4X,22I5)')(I,I=1,22)
          call edisp(itru,outs)
          write(outs,'(4X,22I5)')(I,I=23,NZS)
          call edisp(itru,outs)
        elseif(NZS.ge.45.and.NZS.le.68)then
          write(outs,'(4X,22I5)')(I,I=1,22)
          call edisp(itru,outs)
          write(outs,'(4X,22I5)')(I,I=23,44)
          call edisp(itru,outs)
          write(outs,'(4X,22I5)')(I,I=45,NZS)
          call edisp(itru,outs)
        elseif(NZS.ge.69.and.NZS.le.90)then
          write(outs,'(4X,22I5)')(I,I=1,22)
          call edisp(itru,outs)
          write(outs,'(4X,22I5)')(I,I=23,44)
          call edisp(itru,outs)
          write(outs,'(4X,22I5)')(I,I=45,68)
          call edisp(itru,outs)
          write(outs,'(4X,22I5)')(I,I=69,NZS)
          call edisp(itru,outs)
        endif
        call edisp(itru,' ')

C MS senstive, change is MS>90.
        DO 9994 I=1,NZS
          if(NZS.le.22)then
            write(outs,9993)I,(VEWF(I,J),J=1,NZS)
 9993       format(I3,1X,22F5.2)
            call edisp(itru,outs)
          elseif(NZS.ge.23.and.NZS.le.44)then
            write(outs,9993)I,(VEWF(I,J),J=1,22)
            call edisp(itru,outs)
            write(outs,'(1x,22F5.2)')(VEWF(I,J),J=23,NZS)
            call edisp(itru,outs)
          elseif(NZS.ge.45.and.NZS.le.68)then
            write(outs,9993)I,(VEWF(I,J),J=1,22)
            call edisp(itru,outs)
            write(outs,'(1x,22F5.2)')(VEWF(I,J),J=23,44)
            call edisp(itru,outs)
            write(outs,'(1x,22F5.2)')(VEWF(I,J),J=45,NZS)
            call edisp(itru,outs)
          elseif(NZS.ge.45.and.NZS.le.90)then
            write(outs,9993)I,(VEWF(I,J),J=1,22)
            call edisp(itru,outs)
            write(outs,'(1x,22F5.2)')(VEWF(I,J),J=23,44)
            call edisp(itru,outs)
            write(outs,'(1x,22F5.2)')(VEWF(I,J),J=45,68)
            call edisp(itru,outs)
            write(outs,'(1x,22F5.2)')(VEWF(I,J),J=69,NZS)
            call edisp(itru,outs)
          endif
 9994   CONTINUE
      endif

C If there are sensors continue.
      if(NCUB.eq.0)goto 100

C If next line is *MRT_SENSOR
      CALL STRIPC(IUF,OUTSTR,0,ND,1,'line 1',IER)
      if(IER.NE.0)return
      if(OUTSTR(1:11).NE.'*MRT_SENSOR')then
        write(outs,'(3a)') LMRT(1:lnblnk(LMRT)),
     &    ' is missing a MRT sensor definition.'
        if(dll)then
          dllsubr='ERMRT'
          dllmesg=outs
          ier=2
          CALL ERPFREE(iuf,ISTAT)
          return
        else
          call edisp(itru,outs)
          IER=1
          CALL ERPFREE(iuf,ISTAT)
          return
        endif
      endif


C Need to figure out what fstsur & lstsur actually are with
C reference to a standard zone!!!!
      fstsur=NZS+1
      lstsur=NZS
      nsurmf=NZS+6

C Clear array of view factors from each cube to geometry.
      do 40 i=1,NCUB
        do 41 iv=1,6
          do 42 j=1,nsurmf
            ffl(i,iv,j)=0.
  42      continue
  41    continue
  40  continue

C Read each sensor block. If no name then assign the sensor index
C as the name via internal read. If FOCUS is the same as the current
C sensor name then read the viewfactors and exit.
      DO 10 I=1,NCUB
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'sensor block',IER)
        IF(IER.NE.0)return
        IF(ND.EQ.8.OR.ND.EQ.9)THEN
          K=0
          CALL EGETWI(OUTSTR,K,IVFOK(I),0,1,'W','mrt flag',IER)
          CALL EGETWR(OUTSTR,K,XOC(I),-999.,998.,'W','mrt X org',IER)
          CALL EGETWR(OUTSTR,K,YOC(I),-999.,998.,'W','mrt Y org',IER)
          CALL EGETWR(OUTSTR,K,ZOC(I),  -9., 99.,'W','mrt Z org',IER)
          CALL EGETWR(OUTSTR,K,DXC(I),   0., 99.,'W','mrt X dis',IER)
          CALL EGETWR(OUTSTR,K,DYC(I),   0., 99.,'W','mrt Y dis',IER)
          CALL EGETWR(OUTSTR,K,DZC(I),   0., 99.,'W','mrt Z dis',IER)
          CALL EGETWR(OUTSTR,K,CANG(I),-359.,359.,'W','mrt rot',IER)
          IF(ND.EQ.8)THEN
            WRITE(CUBN(I),'(A,I2.2)')'sen_',I
          ELSEIF(ND.EQ.9)THEN
            CALL EGETW(OUTSTR,K,WORD,'W','cube name',IFLAG)
            CUBN(I)=WORD(1:6)
          ENDIF
        ELSE
          CALL USRMSG(' mismatch sensor descr. in',OUTSTR,'W')
        ENDIF

C If view factors have been previously defined the keyword *MRTVIEW
C will appear next and relevant data should be read in if the
C current sensor name mathches FOCUS (then return).
C Otherwise the file will have the keyword *NOMRTVIEW and zeros.
        CALL STRIPC(IUF,OUTSTR,0,ND,1,'viewfactor key',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','viewfactor key',IFLAG)
        if(WORD(1:8).eq.'*MRTVIEW')then
          IVFOK(i)=1
        elseif(WORD(1:10).eq.'*NOMRTVIEW')then
          IVFOK(i)=0
        endif
        do 425 iv = 1,6
          IRV=NZS
          CALL EGETWRA(IUF,VA,IRV,0.,0.9999,'W','sensor vf',IER)
          DO 114 j=1,nsurmf
            ffl(i,iv,j)=VA(j)
  114     CONTINUE
425     continue
   10 CONTINUE

C Close the file and free the unit number.
  100 CALL ERPFREE(iuf,ISTAT)
      RETURN
      END


C ************* EMKMRT
C EMKMRT Write viewfactor/MRT sensor definition file based on
C information currently held in common blocks MRTC, MRTF.
C LMRT is the name of the file to be written to (any existing file
C by this name is overwritten).  zname is the zone name (12 char),
C ICOMP is the zone number.
C ITRU unit number for user output, IER=0 OK, IER=1 problem.
C act character*1 if 'a' then area weighted, if 'r' ray traced.
      SUBROUTINE EMKMRT(LMRT,LGEOM,NZS,IUF,ICOMP,act,IER)
#include "building.h"
#include "geometry.h"

      integer lnblnk  ! function definition

      integer ncub,ivfok
      real XOC,YOC,ZOC,DXC,DYC,DZC,CANG
      character CUBN*6
      common/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)
      COMMON/MRTF/FFL(MCUB,6,MSM),VEWF(MS,MS)

      dimension rval(MS)
      character LMRT*72,LGEOM*72,act*1
      character louts*248

      IER=0

C Place ouput into IUF. Open any existing file by this name,
C (ask user for confirmation to over-write) or create a new file.
      CALL EFOPSEQ(IUF,LMRT,4,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

C Write out the file data.
      WRITE(IUF,30,IOSTAT=IOS,ERR=14)
     &  zname(ICOMP)(1:lnzname(ICOMP)),LMRT(1:lnblnk(LMRT))
  30  FORMAT('# zone viewfactors and MRT sensor definition of ',a,/,
     &'# defined in ',a)

      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=14)'*VIEWFACTORS'
      WRITE(IUF,'(A,A)',IOSTAT=IOS,ERR=14)'*GEOM  ',
     &  LGEOM(1:lnblnk(LGEOM))
      WRITE(IUF,'(3I4,A)',IOSTAT=IOS,ERR=14)
     &  NCUB,NZS,ICOMP,
     &  '   # numb mrt sensors, numb zone surfs, zone index'
      if(act.eq.'a')then
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=14)
     &    '# Area weighted zone viewfactors follow'
      else
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=14)'# Zone viewfactors follow'
      endif

C Write matrix, one line at a time. Begin by copying data to a single
C dimension array and then generating & writing out as much as 248 chars.
      DO 930 I = 1,NZS
        do 32 j = 1,NZS
          rval(j)=VEWF(i,j)
  32    continue
        itrunc=1
        ipos=1
        do while (itrunc.ne.0)
          call arlist(ipos,NZS,rval,MS,'C',louts,loutlen,itrunc)
          write(iuf,'(1x,a)',IOSTAT=ios,ERR=14) louts(1:loutlen)
          ipos=itrunc+1
        end do
  930 CONTINUE

C Print sum over a row of view factors. Commented
C because it might influence use of view factors in bps.
      DO 462 I =  1,NZS
        fr=0.0
        DO 461 J =  1,NZS
          fr=fr+VEWF(i,j)
461     CONTINUE
        WRITE(IUF,4060)i,fr
4060    FORMAT(' sum over row ',i2,' is : ',F14.10)
462   CONTINUE

C If there are any cubes the save this information as well.
      if(NCUB.eq.0)goto 999
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=14) '*MRT_SENSOR'
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=14)
     & '# OK, Xorig, Yorig, Zorig,   DX,    DY,    DZ, rotation,  name'

      DO 1180 I=1,NCUB
        WRITE(IUF,34)IVFOK(I),XOC(I),YOC(I),ZOC(I),DXC(I),DYC(I),
     &    DZC(I),CANG(I),CUBN(I),I
  34    FORMAT(I4,6F7.3,F7.1,2X,A6,'   # sensor ',I2)
        if(IVFOK(I).eq.1)then
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=14)'*MRTVIEW'
        else
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=14)'*NOMRTVIEW'
        endif
        DO 425 iv = 1,6

C For each face of MRT sensor compose 1D array and generate packed
C string(s) as required.
          do 132 j = 1,NZS
            rval(j)=ffl(i,iv,j)
  132     continue
          itrunc=1
          ipos=1
          do while (itrunc.ne.0)
            call arlist(ipos,NZS,rval,MS,'C',louts,loutlen,itrunc)
            write(iuf,'(1x,a)',IOSTAT=ios,ERR=14) louts(1:loutlen)
            ipos=itrunc+1
          end do
  425   CONTINUE

1180  CONTINUE

C Close sensor file.
 999  CALL ERPFREE(IUF,ISTAT)
      RETURN

C Error messages.
  14  if(IOS.eq.2)then
        call usrmsg(' Problem writing to view factor & MRT sensor',
     &            ' file. No permission to write file.','W')
      else
        call usrmsg(' Problem writing to view factor & MRT sensor',
     &            ' file. Check disk space or computed values.','W')
      endif
      ier=1
      goto 999

      END

C ****** stuf4xyz
C stuf4xyz instanciates 4 vertices in x(MV),y(MV),z(MV) from raw data.
C Useful for block shapes prior to call to PLEQN.
      subroutine stuf4xyz(x,y,z,x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4)
#include "building.h"
      dimension x(MV), y(MV), z(MV)
      X(1) = x1
      Y(1) = y1
      Z(1) = z1
      X(2) = x2
      Y(2) = y2
      Z(2) = z2
      X(3) = x3
      Y(3) = y3
      Z(3) = z3
      X(4) = x4
      Y(4) = y4
      Z(4) = z4
      return
      end

C ********************  PLEQN
C PLEQN finds the equation EQN to a plane containing a polygon which
C is defined as a set of X() Y() Z() verticies tracing in order the
C edges of the polygon.  The plane is the best fit from points and its
C equation is in the form:
C       A*X + B*Y + C*Z = D
C where the vector (A B C) is the unit normal vector to the plane.
C The normal will point out from the surface if the vertices are
C passed in anti-clockwise.
C If ierpln=-1 then SNORM is very close to zero
C If ierpln=1 then number of points is zero

      SUBROUTINE PLEQN(X,Y,Z,NP,CG,EQN,IERPLN)
#include "building.h"

      DIMENSION X(MV), Y(MV), Z(MV), CG(3), EQN(4)
      DIMENSION VA(3), VB(3), VC(3), VN(3)
      logical close

C Initialise.
      IERPLN=0
      CG(1)=0.00
      CG(2)=0.00
      CG(3)=0.00
      EQN(1)=0.00
      EQN(2)=0.00
      EQN(3)=0.00
      EQN(4)=0.00
      VN(1)=0.00
      VN(2)=0.00
      VN(3)=0.00

C Check that we have some points.
      if (NP.eq.0) then
        IERPLN=1
        RETURN
      endif

C Find the center of gravity.
      do 10 I=1,NP
        CG(1)=CG(1)+X(I)
        CG(2)=CG(2)+Y(I)
        CG(3)=CG(3)+Z(I)
 10   continue
      do 20 I=1,3
        CG(I)=CG(I)/NP
 20   continue

C Loop through vertices calculating cross product.
      do 30 I=1,NP
        if ((I+1).gt.NP) then
          VA(1)=X(1)-X(I)
          VA(2)=Y(1)-Y(I)
          VA(3)=Z(1)-Z(I)
          VB(1)=X(2)-X(1)
          VB(2)=Y(2)-Y(1)
          VB(3)=Z(2)-Z(1)
        elseif ((I+1).eq.NP) then
          VA(1)=X(I+1)-X(I)
          VA(2)=Y(I+1)-Y(I)
          VA(3)=Z(I+1)-Z(I)
          VB(1)=X(1)-X(I+1)
          VB(2)=Y(1)-Y(I+1)
          VB(3)=Z(1)-Z(I+1)
        else
          VA(1)=X(I+1)-X(I)
          VA(2)=Y(I+1)-Y(I)
          VA(3)=Z(I+1)-Z(I)
          VB(1)=X(I+2)-X(I+1)
          VB(2)=Y(I+2)-Y(I+1)
          VB(3)=Z(I+2)-Z(I+1)
        endif

C Do cross product and if VC is not close to zero then
C normalise it (so that a polygon with long and short
C edges is treated correctly.
        call CROSS(VA,VB,VC)
        SNORM=0.00
        SNORM=sqrt(VC(1)**2+VC(2)**2+VC(3)**2)
        close=.false.
        call eclose(SNORM,0.0,0.0001,close)
        if(.not.close)then
          do 50 J=1,3
            VC(J)=VC(J)/SNORM
 50       continue
        endif

C Add to VN the normalised VC.
        VN(1)=VN(1)+VC(1)
        VN(2)=VN(2)+VC(2)
        VN(3)=VN(3)+VC(3)
 30   continue

C Make the surface normal of unit length and build EQN from VN.
      SNORM=0.
      SNORM=sqrt(VN(1)**2+VN(2)**2+VN(3)**2)
      close=.false.
      call eclose(SNORM,0.0,0.0001,close)
      if(close)then
        ierpln=-1
        return
      else
        do 40 I=1,3
          EQN(I)=VN(I)/SNORM
 40     continue
      endif

C Calculate constant term.
      EQN(4)=(EQN(1)*CG(1)) + (EQN(2)*CG(2)) + (EQN(3)*CG(3))

      RETURN
      END


C ******************** EPLNEQN
C EPLNEQN finds the equation EQN to a plane containing a polygon which
C is defined as a set of X Y Z verticies tracing in order the edges of
C the polygon (this information is contained in PNTLST).  The plane is
C the best fit from PNTLST and its equation is in the form:
C       A*X + B*Y + C*Z = D
C where the vector (A<B<C) is the unit normal vecotr to the plane.
C This normal will be in the direction given by the right hand rotation
C rule MORS is the handedness of the axes defining the polygon
C verticies.  For solid bodies whose faces have been described anti-
C clockwise when viewed from the outside the unit normals within this
C routine will point outwards from the body.

C Note: this subroutine is not called

      SUBROUTINE EPLNEQN(ITRU,MORS,ISTA,PNTLST,CG,EQN,IERR)
#include "building.h"

C Parameters
      integer itru  ! unit for writing errors
      integer mors  ! indicates handedness of polygon - see below
      integer ISTA  ! start pointer to pntlst
      real PNTLST   ! list of points - see below
      real CG       ! returned centroid of polygon
      real EQN      ! returned equation of the plane
      DIMENSION PNTLST(100),CG(3),EQN(4)
      integer IERR  ! error state - see below

      real VNORM,PNT,E,VN,EV1,EV2,EV3
      dimension VNORM(3),PNT(MV,3),E(MV,3),VN(MV,3)
      dimension EV1(3),EV2(3),EV3(3)
      CHARACTER outs*124
      logical close

C INPUTS.
C    PNTLST(ISTA)      NO. OF POINTS IN THE POLYGON.
C    PNTLST(ISTA+1)    FIRST VERTEX X COORD.
C    PNTLST(ISTA+2)    FIRST VERTEX Y COORD.
C    PNTLST(ISTA+3)    FIRST VERTEX Z COORD.
C    PNTLST(ISTA+4)    SECOND VERTEX X COORD. .....ETC.
C    MORS              (MODEL OR SCREEN) IS A FLAG TO INDICATE THE
C                      HANDEDNESS OF THE AXIS SYSTEM DEFINING THE POLYGON.
C                      IF MORS=1 FLAGS A RIGHT HANDED SYSTEM
C                                         E.G. MODEL COORDS.
C                      IF MORS=2 FLAGS A LEFT HANDED SYSTEM
C                                         E.G. SCREEN COORDS.

C    IERR   AN ERROR FLAG. IF .GE. 0 ALL O.K.
C           IF -1 THEN THE MAGNITUDE OF THE UNIT NORMAL IS .LT.10E-4
C           IF -2 THEN LESS THAN 3 POINTS IN THE POLYGON.
C           IF -3 THEN MORS IS OUT OF RANGE.

C INTERNAL VARIABLES.

C    PNT(MV,3)   AN ARRAY OF VERTEX POINTS.
C    E(MV,3)     AN ARRAY OF EDGE VECTORS.
C    VN(MV,3)    AN ARRAY OF NORMAL VECTORS.
C    EV*(3)       TEMPORARY STORAGE FOR VECTORS.
C    VNORM(1>3)   X Y Z COORDS OF THE UNIT NORMAL VECTOR TO THE PLANE.

      IERR=0

      NP=INT(PNTLST(ISTA))
      IF(NP.LT.3)THEN
        IERR=-2
        GO TO 999
      ENDIF

      IF(NP.GT.MV) NP=MV
      DO 20 J=1,NP
        J1=J+1
        IF(J1.GT.NP)J1=1
        INJ=ISTA+((J-1)*3)
        INJ1=ISTA+((J1-1)*3)
        DO 10 K=1,3
          KNJ=INJ+K
          KNJ1=INJ1+K
          PNT(J,K)=PNTLST(KNJ)
          E(J,K)=PNTLST(KNJ1)-PNTLST(KNJ)

   10   CONTINUE
   20 CONTINUE

      DO 50 J=1,NP
        J1=J+1
        IF(J1.GT.NP) J1=1
        DO 30 K=1,3
          EV1(K)=E(J,K)
          EV2(K)=E(J1,K)
   30   CONTINUE

        CALL CROSS(EV1,EV2,EV3)

        SNORM = 0.0
        SNORM=SQRT(EV3(1)**2+EV3(2)**2+EV3(3)**2)
        call eclose(SNORM,0.0,0.0001,close)
        IF (.NOT.close)  SNORM = SQRT(SNORM)
        DO 40 K=1,3
          VN(J,K) = 0.0
          IF (.NOT.close)  VN(J,K)=EV3(K)/SNORM
   40   CONTINUE
   50 CONTINUE

      CALL AVER(MV,NP,PNT,CG)

      CALL AVER(MV,NP,VN,VNORM)

      SNORM=SQRT(VNORM(1)**2+VNORM(2)**2+VNORM(3)**2)
      IF(SNORM.LT.10E-4)THEN
        WRITE(outs,'(A,F10.5)')' PLNEQN: SNORM=',SNORM
        CALL EDISP(ITRU,outs)
        IERR=-1
        GO TO 999
      ENDIF

      DO 80 J=1,3
        EQN(J)=VNORM(J)/SNORM
   80 CONTINUE

      EQN(4)=EQN(1)*CG(1)+EQN(2)*CG(2)+EQN(3)*CG(3)

      IF(MORS.EQ.1)THEN
        RETURN
      ELSEIF(MORS.EQ.2)THEN
        DO 100 J=1,4
          EQN(J)=-EQN(J)
  100   CONTINUE
        RETURN
      ELSE

C MORS flag not recognised.
        IERR=-3
        GO TO 999
      ENDIF

 999  WRITE(outs,'(A,I3)') ' PLNEQN: error flag IERR =',IERR
      CALL EDISP(ITRU,outs)

      RETURN
      END

C ********************* ETRANSW
C ETRANSW used when creating an insert rectangular shape into a surface.
C OFFSET is the offset from the polygon (zero tpically).

C STEP 1 - Find equation of surface (EQN(4)) via PLEQN and get centre
C          Centre of Gravity (VP(3))
C STEP 2 - Set up Eye Point normal to plane at C. of G. (EP(3))
C STEP 3 - Find matrix and reverse matrix via EYEMAT to transform window
C          points to normal view.
C STEP 4 - Find co-ordinates of 'origin' as transformed via ORTTRN to
C          to normal view ('origin' - first vertex of surface)
C STEP 5 - Derive displacement of Window/Door co-ordinates relative to
C          'origin'
C STEP 6 - Apply reverse transformation to Window/Door co-ordinates via
C          ORTTRN to give vertices in global co-ords.
C N is the number of surface vertices, X,Y,Z are the surface vertex arrays,
C DX,DZ are the window offsets, DDX,DDZ are the window width & hieght,
C XXW,YYW,ZZW are the transformed window coords in surface coord system.
      SUBROUTINE ETRANSW(ITRC,ITRU,N,X,Y,Z,OFFSET,DX,DZ,DDX,DDZ,
     &                   XXW,YYW,ZZW)
#include "building.h"
      DIMENSION  X(MV),Y(MV),Z(MV),XXW(4),YYW(4),ZZW(4)
      DIMENSION  TMAT(4,4),RMAT(4,4)
      DIMENSION  VP(3),EP(3),EQN(4)
      CHARACTER OUTSTR*124

C Find transformation matrices that normalise face.
      call PLEQN(X,Y,Z,N,VP,EQN,IERR)

      IF (IERR .LT. 0)  GOTO  100
      DO 250 J = 1,3
        EP(J) = VP(J) + EQN(J)
  250 CONTINUE
      IF(ITRC.GT.1)THEN
        CALL EDISP(ITRU,' Plane equation data: ')
        WRITE(OUTSTR,'(a,3F8.3)')' Center of grav:',(VP(I),I=1,3)
        CALL EDISP(ITRU,OUTSTR)
        WRITE(OUTSTR,'(a,4F8.3)')' Equation:',(EQN(I),I=1,4)
        CALL EDISP(ITRU,OUTSTR)
        WRITE(OUTSTR,'(a,3F8.3)')' Eye Point:',(EP(I),I=1,3)
        CALL EDISP(ITRU,OUTSTR)
      ENDIF
      CALL  EYEMAT(EP,VP,1.0,TMAT,RMAT)

C Transform all points in surface and find lower left corner.
      XMIN=0.0
      YMIN=0.0
      DO 300 I=1,N
        CALL ORTTRN(X(I),Y(I),Z(I),TMAT,X1,Y1,ZZZ,IERR)
        IF(X1.LT.XMIN)XMIN=X1
        IF(Y1.LT.YMIN)YMIN=Y1
  300 CONTINUE
      XXW(1)=XMIN+DX
      YYW(1)=YMIN+DZ
      XXW(2)=XXW(1)+DDX
      YYW(2)=YYW(1)
      XXW(3)=XXW(2)
      YYW(3)=YYW(1)+DDZ
      XXW(4)=XXW(1)
      YYW(4)=YYW(3)

C Take each window or door and apply transformation first shifting
C the Z point by OFFSET.
      ZZZ=ZZZ-OFFSET
      DO 350 K = 1,4
        CALL  ORTTRN(XXW(K),YYW(K),ZZZ,RMAT,XX,YY,ZZ,IERR)
        XXW(K) = XX
        YYW(K) = YY
        ZZW(K) = ZZ
  350 CONTINUE
      IF(ITRC.GT.1)THEN
        CALL EDISP(ITRU,' Transforming window coords: ')
        WRITE(OUTSTR,'(a,4F8.3)') ' X coords:',(XXW(I),I=1,4)
        CALL EDISP(ITRU,OUTSTR)
        WRITE(OUTSTR,'(a,4F8.3)') ' Y coords:',(YYW(I),I=1,4)
        CALL EDISP(ITRU,OUTSTR)
        WRITE(OUTSTR,'(a,4F8.3)') ' Z coords:',(ZZW(I),I=1,4)
        CALL EDISP(ITRU,OUTSTR)
      ENDIF

  100 CONTINUE

      RETURN
      END

C ********************* ETRANFRAME
C ETRANFRAME used when creating an fixed width frame into a surface. It
C is assumed that an initial polygon of N edges will result in a frame
C polygon of N edges, however nwvert is returned with the actual number
C of vertices.

C STEP 1 - Find equation of surface (EQN(4)) via PLEQN and get centre
C          Centre of Gravity (VP(3))
C STEP 2 - Set up Eye Point normal to plane at C. of G. (EP(3))
C STEP 3 - Find matrix and reverse matrix via EYEMAT to transform frame
C          points to normal view.
C STEP 4 - Find offset points away from the polygon edge.
C STEP 5 - Derive displacement of frame co-ordinates relative to
C          'origin'?
C STEP 6 - Apply reverse transformation to Window/Door co-ordinates via
C          ORTTRN to give vertices in global co-ords.

      SUBROUTINE ETRANFRAME(ITRC,N,X,Y,Z,FRW,XXF,YYF,ZZF,nwvert,IER)
#include "building.h"

C Passed parameters:
      integer N     ! number of edges in the parent polygon
      real X,Y,Z    ! array of coordinates of parent polygon
      DIMENSION  X(MV),Y(MV),Z(MV)
      real FRW      ! frame width (m)
      real XXF,YYF,ZZF ! array of returned frame coordinates
      DIMENSION  XXF(MV),YYF(MV),ZZF(MV)
      integer nwvert  ! returned number of conformant new vertices
      integer ier   ! if 1 can not deal with complexity

      common/OUTIN/IUOUT,IUIN

C Local variables:
      DIMENSION  TMAT(4,4),RMAT(4,4)
      DIMENSION  VP(3),EP(3),EQN(4)
      real XN,YN    ! 2D coordinate array (raw)
      real XPN,YPN    ! 2D coordinate array (plus offset)
      DIMENSION  XN(MV),YN(MV),XPN(MV),YPN(MV)
      real XL,YL    ! ?? 
      DIMENSION  XL(MV),YL(MV)
      real AIL,BIL,CIL  ! defines inner lines
      DIMENSION AIL(MVS),BIL(MVS),CIL(MVS)
      CHARACTER OUTSTR*124
      integer ITVX   ! number of vertices in polygon
      logical BCLOSE,ICLOSE,MCLOSE ! close to X or Y axis
      logical close  ! to avoid divide by zero
      logical is180,close180  ! to avoid stright edge
      dimension is180(MV)
      real pi,ang3

C Check if we can deal with additional complexity in parent surface.
      ITVX = N  ! remember N
      if((N+2+ITVX).gt.MTV)then
        call edisp(iuout,
     &    'Frame more complex than ESP-r can work with. Returning.')
        ier=1
        return
      endif

      pi=4.0*atan(1.0)

C Find transformation matrices that normalise face.
      call PLEQN(X,Y,Z,N,VP,EQN,IERR)

      IF (IERR .LT. 0)  GOTO  100
      DO 250 J = 1,3
        EP(J) = VP(J) + EQN(J)
  250 CONTINUE
      IF(ITRC.GT.1)THEN
        CALL EDISP(IUOUT,' Plane equation data: ')
        WRITE(OUTSTR,'(a,3F8.3)')' Center of grav:',(VP(I),I=1,3)
        CALL EDISP(IUOUT,OUTSTR)
        WRITE(OUTSTR,'(a,4F8.3)')' Equation:',(EQN(I),I=1,4)
        CALL EDISP(IUOUT,OUTSTR)
        WRITE(OUTSTR,'(a,3F8.3)')' Eye Point:',(EP(I),I=1,3)
        CALL EDISP(IUOUT,OUTSTR)
      ENDIF
      CALL  EYEMAT(EP,VP,1.0,TMAT,RMAT)

C Transform all points in surface into 2D working points XN & YN.
C Also find lower left corner in 2D (the 2D coordinates will be
C relative to the COG of the surface).
      XMIN=0.0
      YMIN=0.0
      DO 300 I=1,N

C Also mark which of the edge angles is not close to 180 degrees.
C Check where we are in the list of edges to ensure that we also
C check from terminus vertex back to 1 & 2nd vertices.
        is180(I)=.false.
        if(I.lt.N-2)then
          call ang3vtx(x(i),y(i),z(i),x(i+1),y(i+1),z(i+1),
     &      x(i+2),y(i+2),z(i+2),ang3)
C          write(6,*) 'angle v ',i,i+1,i+2,ang3
          call eclose(ang3,180.,1.0,close180)
          if(close180) is180(I)=.true.
        elseif(I.eq.N-1)then
          call ang3vtx(x(i),y(i),z(i),x(i+1),y(i+1),z(i+1),
     &      x(1),y(1),z(1),ang3)
C          write(6,*) 'angle v ',i,i+1,' 1',ang3
          call eclose(ang3,180.,1.0,close180)
          if(close180) is180(I)=.true.
        elseif(I.eq.N)then
          call ang3vtx(x(i),y(i),z(i),x(1),y(1),z(1),
     &      x(2),y(2),z(2),ang3)
C          write(6,*) 'angle v ',i,' 1 2',ang3
          call eclose(ang3,180.,1.0,close180)
          if(close180) is180(I)=.true.
        endif
        CALL ORTTRN(X(I),Y(I),Z(I),TMAT,X1,Y1,ZZZ,IERR)
        IF(X1.LT.XMIN)XMIN=X1
        IF(Y1.LT.YMIN)YMIN=Y1
        XN(I)=X1
        YN(I)=Y1
  300 CONTINUE

C Debug of list of angles that are close to 180 degrees.
C      write(6,*) 'angles  ',is180

C Offset the 2D coordinates so that lower left is at zero zero.
      if(XMIN.lt.0.0)then
        XOFFSET= ABS(XMIN)
      else
        XOFFSET=XMIN
      endif
      if(YMIN.lt.0.0)then
        YOFFSET= ABS(YMIN)
      else
        YOFFSET= YMIN
      endif

C Add offset to XN YN array.
      do 301 I=1,N
        XPN(I)=XN(I)+XOFFSET
        YPN(I)=YN(I)+YOFFSET
  301 continue

C Logic for determining frame coordinates.
C Solve equation of perpendicular bisector and inner lines. 
      I1=0      ! I1, I2 used to create maximum possible traiangles
      I2=0      ! using all vertices of the parent surface
      DTIN=FRW  ! distance moved in inner edge to create child surface
      XMX=0.0; YMY=0.0; 
      XAA=0.

C A1,B1,C1,A2,B2,C2,C22 are used in equation of line.
      A1=0.0; B1=0.0; C1=0.0
      A2=0.0; B2=0.0; C2=0.0; C22=0.0
      DM=0.    ! DM=(A1*B2)-(A2*B1)
      AMX=0.   ! AMX = (C1A2-C2A1)/(B1A2-A1B2)
      AMY=0.   ! AMY = (C1B2-C2B1)/(A1B2-A2B1)
      AMXX=0.  ! AMXX = (C1A2-C22A1)/(B1A2-A1B2)
      AMYY=0.  ! AMYY = (C1B2-C22B1)/(A1B2-A2B1)
C      AREA=0.  ! calculated area of inner polygon
C      ITER=0   ! number of iterations to calculate required area

C Loop through all the possivblew 2D vertices.
      NWVERT=0   ! new vertices
      DO 765 IV=1,ITVX

C If the next pair of edges would be a straight line do not bother.
C This ensures that the AIL BIL CIL array is packed with only valid
C corner data.
        if(is180(iv))then
          goto 765
        endif
        I1=IV
        I2=IV+1
        IF(I1.EQ.ITVX)I2=1   ! end point at the start of polygon
        DX = XPN(I2) - XPN(I1) ! difference in X
        DY = YPN(I2) - YPN(I1) ! difference in Y
        CALL ECLOSE(DY,0.,0.0001,CLOSE)
        if(close)then
          XAA= PI/2.0          ! to avoid division by zero
        else
          XAA= ATAN(DX/DY)     ! angle between
        endif
        XMX= (XPN(I1)+XPN(I2))/2.0  ! half way along X
        YMY= (YPN(I1)+YPN(I2))/2.0  ! half way along Y
             
C Calculate equation of perpendicular bisector a1y+b1x=c1
        BCLOSE=.FALSE.
        CALL ECLOSE(DY,0.,0.01,BCLOSE)
        IF(BCLOSE) THEN
          A1=0.0   ! Y dist was zero
          B1=1.
          C1=XMX
        ELSE  
          CALL ECLOSE(DX,0.,0.01,BCLOSE) 
          IF(BCLOSE) THEN
            A1=1.   ! X dist was zero
            B1=0.
            C1=YMY      
          ELSE
            A1=1.   ! X dist was non-zero
            B1=(DX/DY)
            C1=YMY+(B1*(XMX))
          ENDIF
        ENDIF
C Debug.
C        WRITE(6,*)'A1,B1,C1,BCLOSE',A1,B1,C1,BCLOSE
             
C Calculate equation of inner lines.
        ICLOSE=.FALSE.
        CALL ECLOSE(DY,0.,0.0001,ICLOSE)
        IF(ICLOSE) THEN
          A2=1.   ! Y dist was zero
          B2=0.
          C2 =YMY+DTIN
          C22=YMY-DTIN
        ELSE  
          CALL ECLOSE(DX,0.,0.0001,ICLOSE) 
          IF(ICLOSE) THEN
            A2=0.   ! X dist was zero
            B2=1.
            C2 =XMX+DTIN
            C22=XMX-DTIN      
          ELSE
            A2=1.   ! X dist was non-zero
            B2= -(DY/DX)

C << need to check to see that sin does not return zero >>
            C2 =YMY+(B2*(XMX))+(DTIN / SIN(XAA)) 
            C22=YMY+(B2*(XMX))-(DTIN / SIN(XAA))
          ENDIF
        ENDIF

C Solve equation of perpendicular bisector and inner lines
C to calculate M'or M''
C M' = (M'x,M'y)   M''=(M''x,M''y)
        DM=(A1*B2)-(A2*B1)
        CALL ECLOSE(DM,0.,0.01,MCLOSE)
        IF(MCLOSE) THEN
          call edisp(iuout,'A zero length detected. Returning.')
          ier=1
          RETURN
        ENDIF
        AMX= ((C1*A2)-(C2*A1))/((B1*A2)-(A1*B2))  ! at one side of bisector
        AMY= ((C1*B2)-(C2*B1))/((A1*B2)-(A2*B1))        
        AMXX=((C1*A2)-(C22*A1))/((B1*A2)-(A1*B2)) ! at other side of bisector
        AMYY=((C1*B2)-(C22*B1))/((A1*B2)-(A2*B1))  

C Figure out if M'or M'' is  contained within polygon
        CALL PNPOLY(AMX,AMY,XPN,YPN,ITVX,IN_OUT)
        IF(IN_OUT.EQ.1) THEN
          NWVERT=NWVERT+1  
          AIL(NWVERT)=A2
          BIL(NWVERT)=B2
          CIL(NWVERT)=C2
        ELSE 
          CALL PNPOLY(AMXX,AMYY,XPN,YPN,ITVX,IN_OUT)
          IF(IN_OUT.EQ.1) THEN
            C2=C22
            NWVERT=NWVERT+1  
            AIL(NWVERT)=A2
            BIL(NWVERT)=B2
            CIL(NWVERT)=C2
          ELSE
            GOTO 765  ! not a valid new corner
          ENDIF
        ENDIF
 765  CONTINUE

C Debug.
      IF(ITRC.GT.1)THEN
        WRITE(OUTSTR,'(a,42F8.3)') ' AIL:',(AIL(I),I=1,NWVERT)
        CALL EDISP(IUOUT,OUTSTR)
        WRITE(OUTSTR,'(a,42F8.3)') ' BIL:',(BIL(I),I=1,NWVERT)
        CALL EDISP(IUOUT,OUTSTR)
        WRITE(OUTSTR,'(a,42F8.3)') ' CIL:',(CIL(I),I=1,NWVERT)
        CALL EDISP(IUOUT,OUTSTR)
      endif

C Solve equation to get intersections of the inner polygon 
C XL,YL (2D points).
      DO 766 IV=1,NWVERT
        I1=IV
        I2=IV+1
        IF(I1.EQ.NWVERT)I2=1 
        XL(I1)= (((CIL(I1))*(AIL(I2)))-((CIL(I2))*(AIL(I1))))/
     &          (((BIL(I1))*(AIL(I2)))-((AIL(I1))*(BIL(I2))))
        YL(I1)= (((CIL(I1))*(BIL(I2)))-((CIL(I2))*(BIL(I1))))/
     &          (((AIL(I1))*(BIL(I2)))-((AIL(I2))*(BIL(I1))))

C Because we used XPN and YPN need to cast XL and YL back from
C the offsets.
        XL(I1)=XL(I1)-XOFFSET
        YL(I1)=YL(I1)-YOFFSET
        IF(ITRC.GT.1)THEN
          WRITE(outstr,'(a,2f8.3,i2)')'XL,YL,I1',
     &      XL(I1),YL(I1),I1
          CALL EDISP(IUOUT,OUTSTR)
        ENDIF
 766  CONTINUE  

C Take frame coords and apply reverse transformation from 2D to 3D.
C Adjust looping to prevent the first edge of the new polygon being
C vertical. Note: working within packed list.
      DO 350 K = 1,NWVERT
        if(K.eq.1)then
          I1=NWVERT
        else
          I1=K-1
        endif
        CALL  ORTTRN(XL(I1),YL(I1),ZZZ,RMAT,XX,YY,ZZ,IERR)
        XXF(K) = XX
        YYF(K) = YY
        ZZF(K) = ZZ
  350 CONTINUE
      IF(ITRC.GT.1)THEN
        CALL EDISP(IUOUT,' Transformed frame coords: ')
        WRITE(OUTSTR,'(a,42F8.3)') ' X coords:',(XXF(I),I=1,NWVERT)
        CALL EDISP(IUOUT,OUTSTR)
        WRITE(OUTSTR,'(a,42F8.3)') ' Y coords:',(YYF(I),I=1,NWVERT)
        CALL EDISP(IUOUT,OUTSTR)
        WRITE(OUTSTR,'(a,42F8.3)') ' Z coords:',(ZZF(I),I=1,NWVERT)
        CALL EDISP(IUOUT,OUTSTR)
      ENDIF

  100 CONTINUE

      RETURN
      END


C .................................................................. 
C SUBROUTINE PNPOLY 
C PURPOSE 
C   TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON 
C USAGE 
C   CALL PNPOLY (PX, PY, XX, YY, N, INOUT ) 

C DESCRIPTION OF THE PARAMETERS 
C   PX   - X-COORDINATE OF POINT IN QUESTION. 
C   PY   - Y-COORDINATE OF POINT IN QUESTION. 
C   XX   - N LONG VECTOR CONTAINING X-COORDINATES OF 
C          VERTICES OF POLYGON.
C   YY   - N LONG VECTOR CONTAING Y-COORDINATES OF 
C          VERTICES OF POLYGON.
C   N    - NUMBER OF VERTICES IN THE POLYGON. 
C  INOUT - THE SIGNAL RETURNED: 
C           -1 IF THE POINT IS OUTSIDE OF THE POLYGON, 
C           0 IF THE POINT IS ON AN EDGE OR AT A VERTEX, 
C           1 IF THE POINT IS INSIDE OF THE POLYGON. 

C REMARKS 
C   THE VERTICES MAY BE LISTED CLOCKWISE OR ANTICLOCKWISE. 
C   THE FIRST MAY OPTIONALLY BE REPEATED, IF SO N MAY C OPTIONALLY 
C   BE INCREASED BY 1. 
C   THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING 
C   OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX 
C   OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING 
C   N, THESE FIRST VERTICES MUST BE COUNTED TWICE. 
C   INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED. 
C   THE SIZE OF THE ARRAYS MUST BE INCREASED IF N > MAXDIM 
C 
C METHOD
C   A VERTICAL LINE IS DRAWN THRU THE POINT IN QUESTION. IF IT 
C   CROSSES THE POLYGON AN ODD NUMBER OF TIMES, THEN THE 
C   POINT IS INSIDE OF THE POLYGON.  
C .................................................................. 
      SUBROUTINE PNPOLY(PX,PY,XX,YY,N,INOUT)
      common/OUTIN/IUOUT,IUIN
      REAL X(200),Y(200),XX(N),YY(N) 
      LOGICAL MX,MY,NX,NY 
      character outs*124
      
      MAXDIM=200 
      IF(N.GT.MAXDIM)then
        WRITE(outs,'(a,i3,a)') 'Warning: ',N,' is beyond edge limits'
        call edisp(iuout,outs) 
        RETURN
      endif
      DO 1 I=1,N 
        X(I)=XX(I)-PX 
        Y(I)=YY(I)-PY
 1    continue
      INOUT=-1 
      DO 2 I=1,N 
        J=1+MOD(I,N) 
        MX= (X(I).GE.0.0) 
        NX= (X(J).GE.0.0)
        MY= (Y(I).GE.0.0)
        NY= (Y(J).GE.0.0)
        IF(.NOT.((MY.OR.NY).AND.(MX.OR.NX)).OR.(MX.AND.NX)) GOTO 2
        IF(.NOT.(MY.AND.NY.AND.(MX.OR.NX).AND..NOT.(MX.AND.NX))) GOTO 3
        INOUT=-INOUT 
        GO TO 2
 
 3      IF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I))) 2,4,5
 4      INOUT=0 
        RETURN 
 5      INOUT=-INOUT 
 2    CONTINUE 
      RETURN 
      END


C ********************* TRANSUR
C TRANSUR transforms a surface along its normal.
C OFFSET is the offset from the base polygon (0.02 typically).
C NB       = no. base surf vertices,
C XB,YB,ZB = base surf vertex arrays,
C XT,YT,ZT =  transformed surf vertex arrays,
      SUBROUTINE TRANSUR(ITRC,ITRU,NB,XB,YB,ZB,OFFSET,XT,YT,ZT,sname)
#include "building.h"
      DIMENSION  XB(MV),YB(MV),ZB(MV),XT(MV),YT(MV),ZT(MV)
      DIMENSION  VP(3),EQN(4),TRNS(3)
      CHARACTER sname*28,outs*144,out96*96

C Find transformation matrices that normalise face.
      call PLEQN(XB,YB,ZB,NB,VP,EQN,IERR)

C If error, return transformed points same as initial.
      IF (IERR .NE. 0)then
        write(outs,'(a,a)') ' PLEQN problem with ',sname
        call edisp(itru,outs)
        do 354 k=1,NB
          XT(k)=XB(k)
          YT(k)=YB(k)
          ZT(k)=ZB(k)
  354   continue
        return
      endif
      DO 352 K = 1,3
        TRNS(k)=EQN(k)*OFFSET
  352 continue
      do 353 k=1,NB
        XT(k)=XB(k)+TRNS(1)
        YT(k)=YB(k)+TRNS(2)
        ZT(k)=ZB(k)+TRNS(3)
  353 continue

      IF(ITRC.GT.1)THEN
        CALL EDISP(ITRU,' Plane equation data: ')
        WRITE(outs,'(a,3F8.3)')' Center of grav:',(VP(I),I=1,3)
        CALL EDISP(ITRU,outs)
        WRITE(outs,'(a,4F8.3)')' Equation:',(EQN(I),I=1,4)
        CALL EDISP(ITRU,outs)
        CALL EDISP(ITRU,' Transforming coords: ')
        ipos=1
        call arlist(ipos,NB,XT,MV,'S',out96,loutlen,itrunc)
        WRITE(outs,'(a,20F7.3)') ' X coords:',out96(1:loutlen)
        CALL EDISP(ITRU,outs)
        call arlist(ipos,NB,YT,MV,'S',out96,loutlen,itrunc)
        WRITE(outs,'(a,20F7.3)') ' Y coords:',out96(1:loutlen)
        CALL EDISP(ITRU,outs)
        call arlist(ipos,NB,ZT,MV,'S',out96,loutlen,itrunc)
        WRITE(outs,'(a,20F7.3)') ' Z coords:',out96(1:loutlen)
        CALL EDISP(ITRU,outs)
      ENDIF

      RETURN
      END

C ******* UV2AZ
C Recover azimuth & elevation from unit vector.
      SUBROUTINE UV2AZ(vdx,vdy,vdz,azim,elev)
      logical clx0,cly0,clx1,cly1,clxm1,clym1
      PI = 4.0 * ATAN(1.0)
      RAD = PI/180.
      CROWA = vdx*vdx + vdy*vdy + vdz*vdz
      CROWA=SQRT(CROWA)
      if (abs(crowa).lt.0.001) then
        return
      endif

C Make vector into unit vector.
      vdx=vdx/crowa
      vdy=vdy/crowa
      vdz=vdz/crowa
      elev=90.0-(acos(vdz)/RAD)

C Reduce the azimuth calculation to a 2D problem.
      dist = vdx**2 + vdy**2
      dist = sqrt(dist)

C Check if tollerably close to an axis.
      CALL ECLOSE(vdx,0.0,0.001,clx0)
      CALL ECLOSE(vdy,0.0,0.001,cly0)
      CALL ECLOSE(vdx,1.0,0.001,clx1)
      CALL ECLOSE(vdy,1.0,0.001,cly1)
      CALL ECLOSE(vdx,-1.0,0.001,clxm1)
      CALL ECLOSE(vdy,-1.0,0.001,clym1)
      if(clx0.and.cly1)then
        azim=0.
      elseif(clx1.and.cly0)then
        azim=90.
      elseif(clx0.and.clym1)then
        azim=180.
      elseif(clxm1.and.cly0)then
        azim=270.
      elseif(vdx.gt.0.0.and.vdy.gt.0.0)then
        azim=acos(vdy/dist)/RAD
      elseif(vdx.gt.0.0.and.vdy.lt.0.0)then
        azim=acos(vdy/dist)/RAD
      elseif(vdx.lt.0.0.and.vdy.lt.0.0)then
        azim=360.0-(acos(vdy/dist)/RAD)
      elseif(vdx.lt.0.0.and.vdy.gt.0.0)then
        azim=360.0-(acos(vdy/dist)/RAD)
      endif

C Debug.
C      write(6,*)'UV2AZ azim elev <<vdx vdy vdz=',azim,elev,vdx,vdy,vdz

      return
      end

C ******* LN2AZ
C LN2AZ Recover azimuth & elevation vector from two coordinates in space.
      SUBROUTINE LN2AZ(x1,y1,z1,x2,y2,z2,azim,elev)
      logical clx0,cly0,clx1,cly1,clxm1,clym1
      PI = 4.0 * ATAN(1.0)
      RAD = PI/180.
      vdx=x2-x1
      vdy=y2-y1
      vdz=z2-z1
      CROWA = vdx*vdx + vdy*vdy + vdz*vdz
      CROWA=SQRT(CROWA)
      if (abs(crowa).lt.0.001) then
        return
      endif

C Make vector into unit vector.
      vdx=vdx/crowa
      vdy=vdy/crowa
      vdz=vdz/crowa
      elev=90.0-(acos(vdz)/RAD)

C Reduce the azimuth calculation to a 2D problem.
      dist = (vdx*vdx) + (vdy*vdy)
      dist = sqrt(dist)

C Check if tollerably close to an axis.
      CALL ECLOSE(vdx,0.0,0.001,clx0)
      CALL ECLOSE(vdy,0.0,0.001,cly0)
      CALL ECLOSE(vdx,1.0,0.001,clx1)
      CALL ECLOSE(vdy,1.0,0.001,cly1)
      CALL ECLOSE(vdx,-1.0,0.001,clxm1)
      CALL ECLOSE(vdy,-1.0,0.001,clym1)
      if(clx0.and.cly1)then
        azim=0.
      elseif(clx1.and.cly0)then
        azim=90.
      elseif(clx0.and.clym1)then
        azim=180.
      elseif(clxm1.and.cly0)then
        azim=270.
      elseif(vdx.gt.0.0.and.vdy.gt.0.0)then
        azim=acos(vdy/dist)/RAD
      elseif(vdx.gt.0.0.and.vdy.lt.0.0)then
        azim=acos(vdy/dist)/RAD
      elseif(vdx.lt.0.0.and.vdy.lt.0.0)then
        azim=360.0-(acos(vdy/dist)/RAD)
      elseif(vdx.lt.0.0.and.vdy.gt.0.0)then
        azim=360.0-(acos(vdy/dist)/RAD)
      endif
      return
      end

C ******* AZ2UV
C Recover unit vector from azimuth & elevation.
      SUBROUTINE AZ2UV(azim,elev,vdx,vdy,vdz)
      PI = 4.0 * ATAN(1.0)
      RAD = PI/180.

C Reconstitute the viewing vector.
      RYAZI = azim*RAD
      RSALT = elev*RAD
      vdz = SIN(RSALT)
      XYDIS = COS(RSALT)
      IF (XYDIS .LT. 1E-6)THEN
        vdx = 0.
        vdy = 0.
      ELSE
        vdx = XYDIS*SIN(RYAZI)
        vdy = XYDIS*COS(RYAZI)
      ENDIF

C Debug.
C      write(6,*)'AZ2UV azim elev >>vdx vdy vdz=',azim,elev,vdx,vdy,vdz

      return
      end

C ********************* CHECKWARP
C CHECKWARP transforms surface into 2D and checks if points are
C are not in the plane of the surface.

C STEP 1 - Find equation of surface (EQN(4)) via PLEQN and get centre
C          Centre of Gravity (VP(3))
C STEP 2 - Set up Eye Point normal to plane at C. of G. (EP(3))
C STEP 3 - Find matrix and reverse matrix via EYEMAT to transform > 2D
C isurf is the current surface index (for reporting)
C N is the number of surface vertices, X,Y,Z are the surface vertex arrays,
C iwhich is worst vertex off plane and ofby is amount it deviates.
C ivoff is the number of vertices which are more than 3mm off.
      SUBROUTINE CHECKWARP(ITRC,itru,isurf,N,X,Y,Z,iwhich,ofby,ivoff)
      implicit none
#include "building.h"

C Passed parameters.
      integer itrc,itru  ! trace level and trace channel
      integer isurf,n    ! focus surface in the room and how many surfaces
      real X,Y,Z         ! arrays of X Y Z coords
      DIMENSION  X(MV),Y(MV),Z(MV)
      integer iwhich     ! which point has largest difference
      real ofby          ! largest difference
      integer ivoff      ! nb of points beyond tollerance

C Local variables

      integer i,j,ierr   ! types for local variables
      real TMAT,RMAT,VP,EP,EQN
      DIMENSION  TMAT(4,4),RMAT(4,4)
      DIMENSION  VP(3),EP(3),EQN(4)
      real X1,Y1,ZZZ     ! returned 2D x y and z of offset point
      real differ,offset ! difference and offset from plane
      CHARACTER OUTSTR*124
      logical close

C Find transformation matrices that normalise face.
      call PLEQN(X,Y,Z,N,VP,EQN,IERR)

      if (IERR .LT. 0) then
        WRITE(OUTSTR,'(a,i3)')
     &    ' CHECKWARP Plane equation error for surface: ',isurf
        return
      endif
      DO 250 J = 1,3
        EP(J) = VP(J) + EQN(J)
  250 CONTINUE

C Report if user has toggled trace on.
      IF(ITRC.GT.1)THEN
        WRITE(OUTSTR,'(a,i3)')
     &    ' CHECKWARP Plane equation data for surface: ',isurf
        CALL EDISP(ITRU,outstr)
        WRITE(OUTSTR,'(a,3F8.3)')' Center of grav:',(VP(I),I=1,3)
        CALL EDISP(ITRU,OUTSTR)
        WRITE(OUTSTR,'(a,4F8.3)')' Equation:',(EQN(I),I=1,4)
        CALL EDISP(ITRU,OUTSTR)
        WRITE(OUTSTR,'(a,3F8.3)')' Eye Point:',(EP(I),I=1,3)
        CALL EDISP(ITRU,OUTSTR)
      ENDIF

C Call eyemat with 1m offset.
      offset=1.00
      CALL  EYEMAT(EP,VP,offset,TMAT,RMAT)

C Transform all points in surface and check for ZZZ which differs
C from the 1m offset.The tollerance is currently set to 5mm.
      ivoff=0
      ofby=0.0
      DO 300 I=1,N
        CALL ORTTRN(X(I),Y(I),Z(I),TMAT,X1,Y1,ZZZ,IERR)
        call eclose(ZZZ,offset,0.005,close)
        if(.NOT.close)then
          ivoff=ivoff+1   ! increment number of vertices off by more than 5mm
          differ=offset-ZZZ
          if(abs(differ).gt.abs(ofby))then  ! keep track of largest diff
            iwhich=i
            ofby=differ
          endif
          if(ITRC.GT.1)then
            WRITE(OUTSTR,'(a,i3,a,3F9.4,a,i3,a,F7.5)')' Point ',I,
     &      ' @ ',X(I),Y(I),Z(I),' in surf ',isurf,' is off by ',differ
            CALL EDISP(ITRU,OUTSTR)
          endif
        endif
  300 CONTINUE

      RETURN
      END


C ***************** ESCZONE
C ESZONE makes a copy of the current contents of common blocks G1 G3
C G4 G5 into SG1 SG2 SG3 SG4 SG5.
      SUBROUTINE ESCZONE(ICOMP)
#include "building.h"
#include "geometry.h"

      real gversion1
      COMMON/SG0/CTYPE1,gversion1
      COMMON/SG1/X1(MTV),Y1(MTV),Z1(MTV),NSUR1,JVN1(MS,MV),NVER1(MS),
     &           NTV1
      COMMON/SG4/NDP1,IDPN1(3)
      COMMON/SG5/SNAME1(MS),SOTF1(MS),SMLCN1(MS),SVFC1(MS),
     &           SOTHER1(MS,3),SUSE1(MS,2),SPARENT1(MS)

      CHARACTER CTYPE1*4,SOTHER1*24,SNAME1*12,SMLCN1*32
      CHARACTER SVFC1*4,SOTF1*32,SUSE1*8,SPARENT1*12

C Loop through all data and copy their contents into a parallel common
C block for later recovery.
      CTYPE1=CTYPE(icomp)
      gversion1=gversion(icomp)
      NSUR1=NSUR
      NDP1=NDP(ICOMP)
      IDPN1(1)=IDPN(ICOMP,1)
      IDPN1(2)=IDPN(ICOMP,2)
      IDPN1(3)=IDPN(ICOMP,3)

      NTV1=NTV
      DO 65 IV=1,NTV1
        X1(IV)=X(IV)
        Y1(IV)=Y(IV)
        Z1(IV)=Z(IV)
   65 CONTINUE

      DO 66 IS=1,NSUR
        SNAME1(IS)=SNAME(ICOMP,IS)
        SOTF1(IS)=SOTF(IS)
        SMLCN1(IS)=SMLCN(IS)
        SVFC1(IS)=SVFC(IS)
        SOTHER1(IS,1)=SOTHER(IS,1)
        SOTHER1(IS,2)=SOTHER(IS,2)
        SOTHER1(IS,3)=SOTHER(IS,3)
        SUSE1(IS,1)=SUSE(IS,1)
        SUSE1(IS,2)=SUSE(IS,2)
        SPARENT1(IS)=SPARENT(IS)
        NVER1(IS)=NVER(IS)
        DO 67 IV=1,NVER1(IS)
          JVN1(IS,IV)=JVN(IS,IV)
   67   CONTINUE
   66 CONTINUE

      RETURN
      END


C **************** ERCZONE
C ERZONE recovers the saved contents of common blocks G1 G2 G3
C G4 G5 from SG1 SG2 SG3 SG4 SG5.
      SUBROUTINE ERCZONE(ICOMP)
#include "building.h"
#include "geometry.h"

      real gversion1
      COMMON/SG0/CTYPE1,gversion1
      COMMON/SG1/X1(MTV),Y1(MTV),Z1(MTV),NSUR1,JVN1(MS,MV),NVER1(MS),
     &           NTV1
      COMMON/SG4/NDP1,IDPN1(3)
      COMMON/SG5/SNAME1(MS),SOTF1(MS),SMLCN1(MS),SVFC1(MS),
     &           SOTHER1(MS,3),SUSE1(MS,2),SPARENT1(MS)

      CHARACTER CTYPE1*4,SOTHER1*24,SNAME1*12,SMLCN1*32
      CHARACTER SVFC1*4,SOTF1*32,SUSE1*8,SPARENT1*12

C Loop through all data and copy their contents into a parallel common
C block for later recovery.
      CTYPE(icomp)=CTYPE1
      gversion(icomp)=gversion1
      NSUR=NSUR1
      NDP(ICOMP)=NDP1
      IDPN(ICOMP,1)=IDPN1(1)
      IDPN(ICOMP,2)=IDPN1(2)
      IDPN(ICOMP,3)=IDPN1(3)

      NTV=NTV1
      DO 65 IV=1,NTV
        X(IV)=X1(IV)
        Y(IV)=Y1(IV)
        Z(IV)=Z1(IV)
   65 CONTINUE

      DO 66 IS=1,NSUR
        SNAME(ICOMP,IS)=SNAME1(IS)
        SOTF(IS)=SOTF1(IS)
        SMLCN(IS)=SMLCN1(IS)
        SVFC(IS)=SVFC1(IS)
        SOTHER(IS,1)=SOTHER1(IS,1)
        SOTHER(IS,2)=SOTHER1(IS,2)
        SOTHER(IS,3)=SOTHER1(IS,3)
        SUSE(IS,1)=SUSE1(IS,1)
        SUSE(IS,2)=SUSE1(IS,2)
        SPARENT(IS)=SPARENT1(IS)
        NVER(IS)=NVER1(IS)
        DO 67 IV=1,NVER(IS)
          JVN(IS,IV)=JVN1(IS,IV)
   67   CONTINUE
   66 CONTINUE

      RETURN
      END

C **************** EASKGEOF
C EASKGEOF asks for the zone number and geometry file name. If the
C configuration file has been read present a list of zone names,
C otherwise ask for a specific zone number and file name.
C If MOD = m or M then allowing a new zone to be created or one
C to be deleted (passed back as a negative zone number).
C If MOD = t or T then the focus is thermophysical.
C It is assumed that the user will make one selection only.
      SUBROUTINE EASKGEOF(PROMPT,CFGOK,IZONE,MOD,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "epara.h"

      common/pophelp/h(60)

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/user/browse
      LOGICAL CFGOK,SELECT,delzone,browse,cpyzone,XST

      DIMENSION VERT(35)
      character*(*) PROMPT
      CHARACTER VERT*27,KEY*1,DFILE*72,MOD*1,H*72
      CHARACTER*72 LFILG
      integer MVERT,IVERT  ! max items and current menu item

C If no configuration file then ask for zone number and name.
      delzone = .false.
      cpyzone = .false.
      IF(.NOT.CFGOK)THEN
        IER=0
        IZONE=1
        H(1)='The zone number is required only for reporting'
        H(2)='purposes but should match the numbering expected'
        H(3)='in the system configuration file.'
        H(4)=' '
        H(5)='A cancel instruction lets you start the zone '
        H(6)='creation process again. '
        CALL EASKI(IZONE,PROMPT,' Zone number?',
     &             1,'F',MCOM,'W',1,'zone number',IERI,6)
        if(ieri.eq.-3)then

C User requested a cancel set izone to zero to signal that
C nothing has been selected.
          IZONE=0
          RETURN
        endif

        H(1)='A zone geometry file will be selected via input of the'
        H(2)='file name since no system configuration file exists.'
        DFILE='new.geo'
   79   CALL EASKS(LFILG,PROMPT,' Geometry file name?',
     &    72,DFILE,'geom name',IER,2)
        IF(LFILG.NE.' ')THEN
          LGEOM(IZONE)=LFILG
        ELSE
          GOTO 79
        ENDIF
        RETURN
      ENDIF

C Initialise zone menu variables based on window size.
C IVERT is the menu position, MVERT the current number of menu lines.
C Use shorter menu depending on what MOD is.
      SELECT=.FALSE.
      MHEAD=0
      if(browse)then
        MCTL=3
      else
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
          MCTL=4
        ELSEIF(MOD.EQ.'T'.OR.MOD.EQ.'t')THEN
          MCTL=5
        ELSE
          MCTL=3
        endif
      endif
      ILEN=NCOMP
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Loop through the items until the page to be displayed. M is the
C current menu line index. Build up text strings for the menu.
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LT.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          if(MOD.EQ.'T'.OR.MOD.EQ.'t')then
            if(LTHRM(L)(1:7).eq.'UNKNOWN')then
              WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (undefined)'
            else
              call FINDFIL(LTHRM(L),XST)
              if(XST)then
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (defined)'
              else
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &            ' (not found)'
              endif
            endif
          else
            WRITE(VERT(M),14)KEY,zname(L)
          endif
   14     FORMAT(A1,1X,A12)
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text and info on portion seen.
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)=  '  ______________________'
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM
   15   FORMAT ('0 page: ',I1,' -- of:',I1)
      ENDIF

C If MOD has been passed as a M then add an item at the end allowing
C list to be modified otherwise blank choice.  If user is browsing
C no need to show additional options.
      if(browse)then
        VERT(M+2)  ='? help                   '
        VERT(M+3)  ='- exit this menu         '
      else
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
          VERT(M+2)='* add/delete/copy        '
          VERT(M+3)='? help                   '
          VERT(M+4)='- exit this menu         '
        ELSEIF(MOD.EQ.'T'.OR.MOD.EQ.'t')THEN
          VERT(M+2)='# update all zones       '
          VERT(M+3)='* non-linear properties  '
          VERT(M+4)='? help                   '
          VERT(M+5)='- exit this menu         '
        ELSE
          VERT(M+2)='? help                   '
          VERT(M+3)='- exit this menu         '
        ENDIF
      endif

C Help messages for the menus below.
C Produce help text.
      H(1)='A zone geometry file will be selected via the name of'
      H(2)='the zone held in the system configuration file.'

C Display the menu.
      call usrmsg(PROMPT,'  ','-')
      if(MOD.EQ.'M'.OR.MOD.EQ.'m')then
        CALL EMENU('Zone Geometry/Attribution',VERT,MVERT,IVERT)
      elseif(MOD.EQ.'T'.OR.MOD.EQ.'t')then
        CALL EMENU('Zone Constructions',VERT,MVERT,IVERT)
      else
        CALL EMENU('Zones',VERT,MVERT,IVERT)
      endif

C Debug.
C      write(6,*) 'zone selected (list) is ',IVERT,MOD
      IF(IVERT.LE.MHEAD)THEN
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C If no selection has been made before exit then return with 0.
        IF(.NOT.SELECT)IZONE=0
        call usrmsg('  ','no zone selected','-')
        RETURN
      ELSEIF(IVERT.EQ.0)THEN

C If nothing from list return with 0.
        IZONE=0
        call usrmsg('  ','no zone selected','-')
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Display help text.
        CALL PHELPD('zone geom file section',2,'-',0,0,IER)
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Decode from the potentially long list to the zone number via KEYIND.
C If delete zone selected previously then turn zone into negative as
C follows -11 = del zone 1, -12 = del zone 2.  In the case of
C copying a zone add 100.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        if(delzone)then
          IZONE = (IFOC + 10) * (-1)
        else
          IZONE = IFOC
        endif
        if(cpyzone)then
          IZONE = IFOC + 100
        endif
        RETURN
      endif

C Treat the remaining options depending on if the usr is browsing
C or owns the model.
      if(browse)then
        IF(IVERT.EQ.(MVERT-2))THEN

C If there are enough items allow paging control via EKPAGE.
          IF(IPFLG.EQ.1)THEN
            IPACT=EDIT
            CALL EKPAGE(IPACT)
          ENDIF
        else

C Not one of the legal menu choices.
          IVERT=-1
          goto 92
        endif
      else

C User can modify zone list.
        IF(IVERT.EQ.(MVERT-2))THEN

C Allow a zone to be added or deleted.
          IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
            H(1)='As with other entities a standard set of add/delete'
            H(2)='and copy functions is provided.  Existing zones to'
            H(3)='be copied are transformed in space, given a new '
            H(4)='name and all partitions reset to external. '
            CALL EASKABCD(' ','Action: ','add zone','delete zone',
     &        'copy zone','cancel',IW,4)
            if(IW.EQ.1)then
              delzone = .false.
              IZONE=NCOMP+1
              RETURN
            elseif(IW.EQ.2)then
              delzone = .true.
              call usrmsg(' ','Select zone to delete... ','-')
              IVERT=-2
              goto 3
            elseif(IW.EQ.3)then
              cpyzone = .true.
              call usrmsg(' ','Select zone to copy... ','-')
              IVERT=-2
              goto 3
            endif

C Display nonlinear thermophysical menu.
          ELSEIF(MOD.EQ.'T'.OR.MOD.EQ.'t')THEN
            delzone = .false.
            IZONE=-2
            RETURN
          ELSE

C If there are enough items allow paging control via EKPAGE.
            IF(IPFLG.EQ.1)THEN
              IPACT=EDIT
              CALL EKPAGE(IPACT)
            ENDIF
          ENDIF
        ELSEIF(IVERT.EQ.(MVERT-3))THEN

C All zones have been sellected return as 99
          IF(MOD.EQ.'T'.OR.MOD.EQ.'t')THEN
            IZONE=99
            RETURN
          ELSEIF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
            IF(IPFLG.EQ.1)THEN
              IPACT=EDIT
              CALL EKPAGE(IPACT)
            ENDIF
          ENDIF
        ELSEIF(IVERT.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
          IF(MOD.EQ.'T'.OR.MOD.EQ.'t')THEN
            IF(IPFLG.EQ.1)THEN
              IPACT=EDIT
              CALL EKPAGE(IPACT)
            ENDIF
          endif
        else

C Not one of the legal menu choices.
          IVERT=-1
          goto 92
        endif
      ENDIF
      IVERT=-2
      goto 3

      END

C **************** ASKZONE
C ASKZONE presents  a list of zones to select from.
C It is passed a prompt, menu title, default
C index and indicates current index (if zero then says no prior
C selection). It is assumed that the user will make one selection only.
C MOD display modifier - `c` note constructions defined, `o` note
C operations defined, `s` note shading defined, `z` note zone attrib
C complete, `v` note view factors, `g` casual gain ctl, `d` domain flow,
C `h` heat transfer (comvective) methods, `-` zone names only.
      SUBROUTINE ASKZONE(IZONE,IDZONE,prompt,title,mod,errmsg,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "schedule.h"
#include "epara.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)

      LOGICAL SELECT,XST
      CHARACTER*(*) prompt,title,errmsg

      DIMENSION VERT(35)
      CHARACTER VERT*27,KEY*1
      character h*72,mod*1,outs*124
      CHARACTER*72 LCFD
      integer MVERT,IVERT  ! max items and current menu item

C Initialise zone menu variables based on window size.
C IVERT is the menu position, MVERT the current number of menu lines.
      SELECT=.FALSE.
      MHEAD=0
      if(mod.eq.'-')then
        MCTL=4
      elseif(mod.eq.'c')then
        MCTL=4
      elseif(mod.eq.'o')then
        MCTL=5
      elseif(mod.eq.'s')then
        MCTL=5
      elseif(mod.eq.'v')then
        MCTL=5
      elseif(mod.eq.'g')then
        MCTL=4
      elseif(mod.eq.'d')then
        MCTL=4
      elseif(mod.eq.'h')then
        MCTL=4
      endif
      ILEN=NCOMP
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Build up text strings for the menu.
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          if(mod.eq.'-')then
            WRITE(VERT(M),'(a1,1x,a12)')KEY,zname(L)
          elseif(mod.eq.'c')then
            if(LTHRM(L)(1:7).eq.'UNKNOWN')then
              WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (undefined)'
            else
              call FINDFIL(LTHRM(L),XST)
              if(XST)then
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (defined)'
              else
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &            ' (not found)'
              endif
            endif
          elseif(mod.eq.'o')then

C Check for operations files. For each one that exists, scan to see
C if it has been sorted and notify user.
            if(LPROJ(L)(1:7).eq.'UNKNOWN')then
              WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (undefined)'
            else
              call FINDFIL(LPROJ(L),XST)
              if(XST)then
                IUO=IFIL+1
                CALL ERPFREE(IUO,ISTAT)
                CALL EROPER(0,iuout,IUO,L,IER)
                if(ip3ver(L).eq.0)then
                  WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (older)'
                  write(outs,'(3a)') 'Operations for ',zname(L),
     &              'needs update (periods unsorted).'
                  call edisp(iuout,outs)
                elseif(ip3ver(L).eq.1)then
                  WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &              ' (defined)'
                elseif(ip3ver(L).ge.2)then
                  WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &              ' (defined)'
                endif
              else
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &            ' (not found)'
              endif
            endif
          elseif(mod.eq.'s')then

C Check for existing shading files.
            if(LSHAD(L)(1:7).eq.'UNKNOWN')then
              WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (undefined)'
            else
              call FINDFIL(LSHAD(L),XST)
              if(XST)then
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (defined)'
              else
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &            ' (not found)'
              endif
            endif
          elseif(mod.eq.'v')then

C Check for existing viewfactor files.
            if(LVIEW(L)(1:7).eq.'UNKNOWN')then
              WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (undefined)'
            else
              call FINDFIL(LVIEW(L),XST)
              if(XST)then
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (defined)'
              else
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &            ' (not found)'
              endif
            endif
          elseif(mod.eq.'g')then

C Check for casual gain control files.
            if(LCGCIN(L)(1:7).eq.'UNKNOWN')then
              WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (undefined)'
            else
              call FINDFIL(LCGCIN(L),XST)
              if(XST)then
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (defined)'
              else
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &            ' (not found)'
              endif
            endif
          elseif(mod.eq.'d')then

C Check for CFD domain files.
            if(LCFD(L)(1:7).eq.'UNKNOWN')then
              WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (undefined)'
            else
              call FINDFIL(LCFD(L),XST)
              if(XST)then
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (defined)'
              else
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &            ' (not found)'
              endif
            endif
          elseif(mod.eq.'h')then

C Check for heat transfer regime files.
            if(LHCCO(L)(1:7).eq.'UNKNOWN')then
              WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (undefined)'
            else
              call FINDFIL(LHCCO(L),XST)
              if(XST)then
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (defined)'
              else
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &            ' (not found)'
              endif
            endif
          endif
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text and info on portion seen.
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)=  '  ______________________'
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM
   15   FORMAT ('0 page part: ',I1,' -- of:',I1)
      ENDIF

      if(idzone.ne.0)then
        write(VERT(M+2),'(a,a)') '* default is ',zname(IDZONE)
      else
        VERT(M+2)='                        '
      endif
      if(MCTL.eq.5)then
        VERT(M+3)  ='* global tasks          '
        VERT(M+4)  ='? help                  '
        VERT(M+5)  ='- exit this menu        '
      else
        VERT(M+3)  ='? help                  '
        VERT(M+4)  ='- exit this menu        '
      endif

C Help message for this menu.
      H(1)='A zone may be selected via the name in the list.'
      h(2)='If you have just created a file it may not show up'
      h(3)='as defined until you exit and re-enter this menu.'
      h(4)=' '
      h(5)='If the topic is shading then a number of global'
      h(6)='tasks can be performed - dereferencing current'
      h(7)='files or specifying zones to include in an'
      h(8)='insolation analysis.'

C Display the menu.
      CALL EMENU(title,VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C If no selection has been made before exit then display error message.
        IF(.NOT.SELECT.and.mod.eq.'-')then
          call usrmsg(errmsg,'You must select one.','W')
          IVERT=-2
          goto 92
        endif
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Display help text.
        CALL PHELPD('zone geom file section',8,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2).and.IDZONE.ne.0.and.MCTL.eq.4)THEN

C Take default zone.
        IZONE=IDZONE
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-2).and.MCTL.eq.5)THEN

C Signal global task.
        IZONE=99
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-3).and.IDZONE.ne.0.and.MCTL.eq.5)THEN

C Take default zone when there is also a global choice.
        IZONE=IDZONE
        RETURN
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Decode from the potentially long list to the zone number via KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        IZONE = IFOC
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-3).and.MCTL.eq.4)THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-4).and.MCTL.eq.5)THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      else

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      endif
      IVERT=-2
      goto 3

      END

C **************** ASKMULTIZONE
C ASKMULTIZONE presents a list of zones to select one or more from.
C It is passed a prompt, menu title, and number of allowable items inpic
C It returns inpick and array IVALS.
C MOD display modifier - `c` note constructions defined, `o` note
C operations defined, `s` note shading defined, `z` note zone attrib
C complete, `v` note view factors, `g` casual gain ctl, `d` domain flow,
C `h` heat transfer (comvective) methods, 'b' obstruction blocks,
C `-` zone names only.
      SUBROUTINE ASKMULTIZONE(INPIC,IVALS,prompt,title,mod,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "schedule.h"
#include "epara.h"

      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)

      LOGICAL XST,FOUND
      CHARACTER*(*) prompt,title

      DIMENSION VERT(35),IVALS(MCOM)
      CHARACTER VERT*27,KEY*1,prompt2*36
      character h*72,mod*1,outs*124
      CHARACTER*72 LCFD

C Clear IVALS and INPICK.
      IER=0
      INPIC=0
      DO 40 I=1,NCOMP
        IVALS(I)=0
   40 CONTINUE

C Initialise zone menu variables based on window size.
C IVERT is the menu position, MVERT the current number of menu lines.
      MHEAD=0
      MCTL=3
      ILEN=NCOMP
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Build up text strings for the menu.
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          if(mod.eq.'-')then
            WRITE(VERT(M),'(a1,1x,a12)')KEY,zname(L)
          elseif(mod.eq.'c')then
            if(LTHRM(L)(1:7).eq.'UNKNOWN')then
              WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (undefined)'
            else
              call FINDFIL(LTHRM(L),XST)
              if(XST)then
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (defined)'
              else
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &            ' (not found)'
              endif
            endif
          elseif(mod.eq.'o')then

C Check for operations files. For each one that exists, scan to see
C if it has been sorted and notify user.
            if(LPROJ(L)(1:7).eq.'UNKNOWN')then
              WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (undefined)'
            else
              call FINDFIL(LPROJ(L),XST)
              if(XST)then
                IUO=IFIL+1
                CALL ERPFREE(IUO,ISTAT)
                CALL EROPER(0,iuout,IUO,L,IER)
                if(ip3ver(L).eq.0)then
                  WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (older)'
                  write(outs,'(3a)') 'Operations for ',zname(L),
     &              'needs update (periods unsorted).'
                  call edisp(iuout,outs)
                elseif(ip3ver(L).eq.1)then
                  WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &              ' (defined v1)'
                elseif(ip3ver(L).eq.2)then
                  WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &              ' (defined v2)'
                elseif(ip3ver(L).gt.2)then
                  WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &              ' (definedv2+)'
                endif
              else
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &            ' (not found)'
              endif
            endif
          elseif(mod.eq.'s')then

C Check for existing shading files.
            if(LSHAD(L)(1:7).eq.'UNKNOWN')then
              WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (undefined)'
            else
              call FINDFIL(LSHAD(L),XST)
              if(XST)then
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (defined)'
              else
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &            ' (not found)'
              endif
            endif
          elseif(mod.eq.'b')then

C Check for existing obstruction block files.
            if(ZOBS(L)(1:7).eq.'UNKNOWN')then
              WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (undefined)'
            else
              call FINDFIL(ZOBS(L),XST)
              if(XST)then
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (defined)'
              else
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &            ' (not found)'
              endif
            endif
          elseif(mod.eq.'v')then

C Check for existing viewfactor files.
            if(LVIEW(L)(1:7).eq.'UNKNOWN')then
              WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (undefined)'
            else
              call FINDFIL(LVIEW(L),XST)
              if(XST)then
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (defined)'
              else
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &            ' (not found)'
              endif
            endif
          elseif(mod.eq.'g')then

C Check for casual gain control files.
            if(LCGCIN(L)(1:7).eq.'UNKNOWN')then
              WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (undefined)'
            else
              call FINDFIL(LCGCIN(L),XST)
              if(XST)then
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (defined)'
              else
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &            ' (not found)'
              endif
            endif
          elseif(mod.eq.'d')then

C Check for CFD domain files.
            if(LCFD(L)(1:7).eq.'UNKNOWN')then
              WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (undefined)'
            else
              call FINDFIL(LCFD(L),XST)
              if(XST)then
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (defined)'
              else
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &            ' (not found)'
              endif
            endif
          elseif(mod.eq.'h')then

C Check for heat transfer regime files.
            if(LHCCO(L)(1:7).eq.'UNKNOWN')then
              WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (undefined)'
            else
              call FINDFIL(LHCCO(L),XST)
              if(XST)then
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),' (defined)'
              else
                WRITE(VERT(M),'(a1,1x,a12,a)')KEY,zname(L),
     &            ' (not found)'
              endif
            endif
          endif
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text and info on portion seen.
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)=  '  ______________________'
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM
   15   FORMAT ('0 page part: ',I1,' -- of:',I1)
      ENDIF

      VERT(M+2)  ='? help                  '
      VERT(M+3)  ='- exit this menu        '

      write(prompt2,'(a)') ' select one or more...'

C Instanciate help message for this menu.
      if(mod.eq.'-')then
        H(1)='Zones may be selected via the names in the list.'
        h(2)='If you have just updated a file it may not show up'
        h(3)='as defined until you exit and re-enter this menu. '
        h(4)=' '
      elseif(mod.eq.'c')then
        h(1)='The list of zone construction files includes an '
        h(2)='indicator of whether the file exists or not. There'
        h(3)='are some tasks which require a file to exist.'
        h(4)=' '
      elseif(mod.eq.'o')then
        h(1)='The list of zone operation files includes an '
        h(2)='indicator of whether the file exists or not. There'
        h(3)='are some tasks which require a file to exist.'
        h(4)=' '
      elseif(mod.eq.'s')then
        h(1)='The list of zone shading files includes an '
        h(2)='indicator of whether the file exists or not. You'
        h(3)='can update existing files or mark shading/insulatoin'
        h(4)='to be included in the analysis or dereferenced.'
      elseif(mod.eq.'v')then
        h(1)='The list of zone viewfactor files includes an '
        h(2)='indicator of whether the file exists or not. You'
        h(3)='can update existing files or mark shading/insulatoin'
        h(4)='to be included in the analysis or dereferenced.'
      elseif(mod.eq.'g')then
        h(1)='The list of zone casual gain control files includes an'
        h(2)='indicator of whether the file exists or not. You'
        h(3)='can select zones to include or derefernce as'
        h(4)='required.'
      elseif(mod.eq.'d')then
        h(1)='The list of zone CFD domain files includes an'
        h(2)='indicator of whether the file exists or not. You'
        h(3)='can select zones to include or derefernce as'
        h(4)='required.'
      elseif(mod.eq.'h')then
        h(1)='The list of zone hc regime files includes an'
        h(2)='indicator of whether the file exists or not. You'
        h(3)='can select zones to include or derefernce as'
        h(4)='required.'
      endif

C Display the menu.
      call usrmsg(PROMPT,PROMPT2,'-')
      CALL EMENU(title,VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C Debug.
C        write(6,*) 'inpic is ',inpic
C        write(6,*) 'ivals is ',ivals

        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Display help text.
        CALL PHELPD('zone files section',4,'-',0,0,IER)
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Look through previous selections and see if IFOC is unique, if
C so update IVALS and loop back for another.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        FOUND=.FALSE.
        IF(INPIC.GT.0)THEN
          DO 44 J=1,INPIC
            IF(IVALS(J).EQ.IFOC.or.FOUND) then
              FOUND=.TRUE.
              if (J+1.gt.NCOMP) then
                IVALS(J)=0
              else
                IVALS(J)=IVALS(J+1)
              endif
            endif
  44      CONTINUE
          IF(.NOT.FOUND)THEN
            if (INPIC.lt.NCOMP) then
              INPIC=INPIC+1
              IVALS(INPIC)=IFOC
              write(outs,*) 'added ',vert(ivert)(2:lnblnk(vert(ivert)))
              call edisp(iuout,outs)
            endif
          ELSE
            INPIC=INPIC-1
            write(outs,*) 'removed ',
     &        vert(ivert)(2:lnblnk(vert(ivert)))
            call edisp(iuout,outs)
          ENDIF
        ELSEIF(INPIC.EQ.0)THEN
          INPIC=1
          IVALS(INPIC)=IFOC
          write(outs,*) 'added ',vert(ivert)(2:lnblnk(vert(ivert)))
          call edisp(iuout,outs)
        ENDIF
      endif

      IF(IVERT.EQ.(MVERT-3))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      else

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      endif
      IVERT=-2
      goto 3

      END

C ******************* EASKSUR
C EASKSUR presents a list of surfaces in a zone IZONE returning the
C index IS. It assumes that a geometry file has been read in.
C If MOD = '-' name list only, MOD = 'M' attribute Many, MOD = 'A'
C show attributes in list.
      SUBROUTINE EASKSUR(IZONE,IS,MOD,PROMPT1,PROMPT2,IER)
#include "building.h"
#include "geometry.h"
#include "epara.h"

      common/pophelp/h(60)
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      DIMENSION VERT(35),VERTS(35)

      CHARACTER*(*) PROMPT1,PROMPT2
      CHARACTER H*72,VERT*28,VERTS*40,KEY*1,MOD*1,title*29
      LOGICAL SELECT
      integer loopstart,loopend,i,icc   ! for looping
      logical usesbasesimp

      IER=0
      IF(IZONE.GT.NCOMP.OR.IZONE.EQ.0)THEN
        CALL USRMSG(' ',' Zone number out of range!','W')
        IER=1
        RETURN
      ENDIF

C Initialise surface menu variables based on window size.
C IVERT is the menu position, MVERT the current number of menu lines.
      SELECT=.FALSE.
      if(MOD.eq.'-')then
        write(title,'(2a)')' Surfaces in ',
     &    zname(IZONE)(1:lnzname(IZONE))
        MHEAD=1
        VERT(1)= '  Name         Composition'
      elseif(MOD.eq.'M')then
        write(title,'(2a)')'  Surfaces in ',
     &    zname(IZONE)(1:lnzname(IZONE))
        MHEAD=1
        VERTS(1)='  Name         Composition  Facing'
      elseif(MOD.eq.'A')then
        write(title,'(2a)')'  Surfaces in ',
     &    zname(IZONE)(1:lnzname(IZONE))
        MHEAD=1
        VERTS(1)='  Name         Composition  Facing'
      endif
      MCTL=4
      ILEN=NZSUR(IZONE)
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Loop through the items until the page to be displayed. M is the
C current menu line index. Build up text strings for the menu.
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          icc=IZSTOCN(izone,L)
          if(MOD.eq.'-')then
            WRITE(VERT(M),14)KEY,SSNAME(icc),SSMLCN(icc)(1:12)
   14       FORMAT(A1,1X,A,1X,A)
          else

C << consider writing out different info depending on what SSOTHER is >>
            if(icc.gt.0)then
              WRITE(VERTS(M),13)KEY,SSNAME(icc),SSMLCN(icc)(1:12),
     &          SSOTHER(icc,1)(1:12)
   13         FORMAT(A1,1X,A,1X,A,1X,A12)
            else
              WRITE(VERTS(M),'(2a)')KEY,' unresolved surface...'
            endif
          endif
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.
      IF(IPFLG.EQ.0)THEN
        VERT(M+1) ='  ________________  '
        VERTS(M+1)='  _________________________  '
      ELSE
        WRITE(VERT(M+1),16)IPM,MPM
        WRITE(VERTS(M+1),16)IPM,MPM
   16   FORMAT ('0 page --- part: ',I1,' of ',I1)
      ENDIF

C If MOD has been passed as a M then return -2 to indicate further processing
      IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
        VERTS(M+2)='* attribute many    '
      ELSE
        VERT(M+2) ='                    '
        VERTS(M+2)='                        '
      ENDIF
      VERT(M+3)   ='? help              '
      VERTS(M+3)  ='? help              '
      VERT(M+4)   ='- exit              '
      VERTS(M+4)  ='- exit              '

C Instanciate help message for this menu.
      H(1)='A surface in the zone will be selected via its name'
      H(2)='in the list (which can be edited elsewhere).'

C Display the menu.
      call usrmsg(PROMPT1,PROMPT2,'-')
      if(MOD.eq.'-')then
        CALL EMENU(title,VERT,MVERT,IVERT)
      else
        CALL EMENU(title,VERTS,MVERT,IVERT)
      endif
      IF(IVERT.LE.MHEAD)THEN
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C If no selection has been made before exit then return with 0.
        IF(.NOT.SELECT)IS=0
        call usrmsg(' ',' ','-')

C Checks the surfaces in the current zone to see if any of the surface
C attributes are 'BASESIMP'.  If there are any then present the
C BASESIMP editing facility (once).
        usesbasesimp=.false.   ! assume no basesimp attributed surfaces
        loopstart=1
        loopend=NZSUR(IZONE)
        DO i=loopstart,loopend
          icc=IZSTOCN(izone,i)
          IF(SSOTHER(icc,1)(1:8).EQ.'BASESIMP')THEN
            usesbasesimp=.true.
          END IF
        END DO
        if(usesbasesimp)then
          CALL BASESIMP_INPUTS(IZONE,IER)
        endif

        RETURN  ! return to the parent call
      ELSEIF(IVERT.EQ.(MVERT-1))THEN
        CALL PHELPD('zone geom file section',2,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Allow user to say several to be attributed.
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
          IS= -2
          call usrmsg(' ',' ','-')
          RETURN
        ELSE
          IVERT=-1
          goto 3
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Decode from the potential long list to the zone number via KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        IS=IFOC
        call usrmsg(' ',' ','-')
        RETURN
      ELSE
C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END

C ********* asksnode
C Subroutine asksnode provides a selection list for the layers
C and nodes within a construction. Passed zone index (icomp)
C and surface index (IS) and returns layer index (ilay) and
C optionally the node index (inode). It assumes that relevant
C databases have been opened.

      subroutine asksnode(icomp,is,ilay,inode,ier)
#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"

      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

C Special materials file flag
      DIMENSION PNAM(ME),item(ME+10)
      integer header,footer
      character item*32,NAM*72,PNAM*20,H*72,outs*124
      logical closemat1,closemat2  ! to check version of materials db.

C Check name of surface with construction name in MLC common.
      imatch=0
      icc=IZSTOCN(icomp,is)
      call matchmlcdesc(SSMLCN(icc),imatch)
      if(imatch.eq.0) then
         call edisp(iuout,'Error: probably no construction defined!')
         ier=1
         return
      endif
      call eclose(matver,1.1,0.001,closemat1)  ! check materials version 1.1.
      call eclose(matver,1.2,0.001,closemat2)  ! check materials version 1.2.

C Write menu displaying a list of layer names
      header=3
      footer=3
      write(item(1),'(a,a)') 'a surface name: ',SSNAME(icc)
      write(item(2),'(a,a)') 'c constr name: ',mlcname(imatch)(1:12)
      item(3)='  ----------------------------'
      do 10 j=1,LAYERS(imatch)

C Pick up the material name of each layer. Uses zero matarrayindex to signal gap.
        if(closemat1.or.closemat2)then
          matarrayindex=IPRMAT(imatch,j)
          if(matarrayindex.ne.0)then
            write(NAM,'(a)') matname(matarrayindex)(1:32)
            PNAM(j)=NAM(1:20)
          else
            write(NAM,'(a)') 'air '
            write(PNAM(j),'(a)') 'air '
          endif
        else
          write(PNAM(j),'(a)') 'unknown'
          call usrmsg('Materials are incomplete or not yet scanned.',
     &      'Please check your model materials file.','W')
        endif
        write(item(header+j),'(a,i2,1x,a12)')' Layer: ',j,PNAM(j)
        ndisp=header+j
  10  continue
      item(ndisp+footer-2)='  ----------------------------'
      item(ndisp+footer-1)='? help  '
      item(ndisp+footer)  ='- exit  '
      ino=-1
      nitms=ndisp+footer

C Instanciate help messages for this menu.
      h(1)='Choose a layer at which to situate a special'
      h(2)='materials node.                                     '

  7   call emenu('Surface layers & nodes',item,nitms,ino)
      if(ino.gt.header.and.ino.le.ndisp) then

C Ask for the nodal location, first calculate a location at centre.
        nodloc=2*(ino-header)
        h(1)='Select a node within the layer (the value shown is'
        h(2)='at the centre of the layer). Nodes are indexed from'
        h(3)='the outside face to the inside face and the default'
        h(4)='nodal scheme uses 3 nodes per layer. '
        h(5)=' '
        h(6)='A cancel leaves the model unchanged. '
        nodloca=nodloc-1
        nodlocb=nodloc+1
        nodlocd=nodloc
        call easki(nodloc,' Position material at ',' which node ? ',
     &    nodloca,'W',nodlocb,'W',nodlocd,'spm node',IERI,6)
        if(ieri.eq.-3)then
          ier=1
          return
        else
          ilay=ino-header
          inode=nodloc
          return
        endif
      elseif(ino.eq.ndisp+footer-1) then
        call phelpd('Special material',2,'-',0,0,IER)
        ino=-1
        goto 7
      elseif (ino.eq.ndisp+footer) then
        return
      elseif(ino.lt.0) then
        goto 7
      else
        goto 7
      endif

      return

      end

C ******************** SURADJ
C SURADJ is a updated version of MOFADJ which returns information about
C connections between surfaces.  Given IZONE & ISFN (the zone and
C surface under consideration) it returns the following information:
C IE identifies the zone coupling index

C IE=-1: not yet defined
C IE=0 : external and both TMP=0.0/ISC=0
C IE=1 : internal to identical environment, both TMP=0.0/ISC=0
C IE=2 : internal to environment with temperature TMP.
C IE=3 : internal where IZC,ISC defines other zone & surface.
C IE=4 : ground connection, both TMP=0.0/ISC=0
C IE=5 : adiabatic, both TMP=0.0/ISC=0
C IE=6 : BASESIMP foundation
C IE=7 : CEN 13791 partition
C DESCR: descriptive string 25 characters wide
C IC   : connection number.
C TMP  : is IE2 (constant temperature if IE=2)

      SUBROUTINE SURADJ(IZONE,ISFN,IE,TMP,IZC,ISC,IC,DESCR)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      CHARACTER*25 DESCR

      IE=0
      TMP=0.0
      IZC=0
      ISC=0
      IC=0

C Get match.
      if(izone.eq.0.or.isfn.eq.0)then
        call edisp(iuout,'SURADJ WARNING: Zone or surface index 0.')
        return
      endif
      I=IZSTOCN(IZONE,ISFN)
      if(I.eq.0)then
        call edisp(iuout,'SURADJ WARNING: returned connection 0.')
        return
      endif
      IC=I
      IE=ICT(I)

C Knowing IE establish TMP, IZC,ISC.
      IF(IE.EQ.3)THEN
        IZC=IC2(I)
        ISC=IE2(I)
        WRITE(DESCR,142)ISC,IZC
  142   FORMAT(' surface',I3,' in zone',I3)
      ELSEIF(IE.EQ.2)THEN
        TMP=IC2(I)
        WRITE(DESCR,143)TMP
  143   FORMAT(' a constant temp= ',F6.2)
      ELSEIF(IE.EQ.1)THEN
        if(IC2(I).eq.0.and.IE2(I).eq.0)then
          DESCR=' an identical environment'
        else
          DESCR=' a similar +- environment'
        endif
      ELSEIF(IE.EQ.0)THEN
        DESCR=' the outside             '
      ELSEIF(IE.EQ.-1)THEN
        DESCR=' not yet defined         '
      ELSEIF(IE.EQ.4)THEN
        DESCR=' the ground/month profile'
      ELSEIF(IE.EQ.5)THEN
        DESCR=' adiabatic               '
      ELSEIF(IE.EQ.6)THEN
        DESCR=' BASESIMP foundation     '
      ELSEIF(IE.EQ.7)THEN
        if(IC2(I).eq.0.and.IE2(I).eq.0)then
          DESCR=' an similar CEN 17391 env'
        else
          DESCR=' a similar CEN +- env    '
        endif
      ENDIF

      RETURN
      END

C *****************************    FLNAME    **************************
C FLNAME provides a name for a defined file type.
C *********************************************************************
      SUBROUTINE FLNAME(ICOMP,FILENM,FLTYP,EXT,NHLP,IER)
#include "building.h"
#include "geometry.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      LOGICAL XST
      CHARACTER FILENM*72,FLTYP*25,EXT*4
      CHARACTER*72 MSG1,TEXT,DFILE

      NUMCH=lnblnk(FILENM)
      IF(NUMCH.LE.0.OR.NUMCH.GE.72)THEN
        IF(ICOMP.GT.0.AND.ICOMP.LE.NCOMP)THEN
          WRITE(FILENM,'(A,A4)')zname(ICOMP)(1:lnzname(ICOMP)),EXT
        ELSEIF(ICOMP.EQ.-1)THEN
          WRITE(FILENM,'(A6,A4)')'ground',EXT
        ELSE
          FILENM='file_name'
        ENDIF
      ENDIF
      CALL FINDFIL(FILENM,XST)
      IF(XST)THEN
        IF(ICOMP.GT.0.AND.ICOMP.LE.NCOMP)THEN
          WRITE(DFILE,'(A,A4)')zname(ICOMP)(1:lnzname(ICOMP)),EXT
        ELSEIF(ICOMP.EQ.-1)THEN
          WRITE(DFILE,'(A6,A4)')'ground',EXT
        ELSE
          DFILE='file_name'
        ENDIF
        WRITE(MSG1,'(3a)')'  Enter the zone ',
     &               FLTYP(1:lnblnk(FLTYP)),' file name'
        CALL EASKS(FILENM,MSG1,' ',72,DFILE,
     &                   FLTYP(1:lnblnk(FLTYP)),IER,NHLP)
      ELSE
        WRITE(TEXT,'(1X,3A)')FLTYP(1:lnblnk(FLTYP)),
     &        ' file created as ',FILENM(1:lnblnk(FILENM))
        CALL EDISP(IUOUT,TEXT)
      ENDIF
      RETURN
      END

C **************** ZSID
C ZSID returns compact descriptions of the zone:surface.
C ZSDES has no truncation, ZSDESC compacts to 20 char with minimum
C of truncation, ZSDESS has maximum risk of truncation.
C << check if calling code knows that current geometry file read is >>
C << not required for this. >>
      SUBROUTINE ZSID(IZ,IS,ZSDES,ZSDESC,ZSDESS)
#include "building.h"
#include "geometry.h"

      integer lnblnk  ! function definition

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      CHARACTER ZSDES*28,ZSDESC*20,SST1*12,ZST1*12,ZSDESS*16

      ZSDESC=' '
      ZSDESS='  '
      ZSDES =' '
      if(iz.eq.0)then
        ZSDESC='-'
        ZSDESS='-'
        ZSDES ='-'
        return
      endif
      icc=izstocn(iz,is)
      SST1=SSNAME(icc)
      lsn0=lnblnk(SST1)
      lsn1=MIN0(lsn0,5)
      lsn2=MIN0(lsn0,9)
      lsn3=MIN0(lsn0,11)
      ZST1=zname(IZ)
      lzn0=lnblnk(ZST1)
      lzn1=MIN0(lsn0,6)
      lzn2=MIN0(lsn0,10)
      lzn3=MIN0(lsn0,12)
      if((lsn0+lzn0).lt.12)then
        WRITE(ZSDESS,'(a,a1,a)') SST1(1:lsn0),':',ZST1(1:lzn0)
      elseif((lsn1+lzn0).lt.12)then
        WRITE(ZSDESS,'(a,a1,a)') SST1(1:lsn1),':',ZST1(1:lzn0)
      elseif((lsn0+lzn1).lt.12)then
        WRITE(ZSDESS,'(a,a1,a)') SST1(1:lsn0),':',ZST1(1:lzn1)
      else
        WRITE(ZSDESS,'(a,a1,a)') SST1(1:lsn1),':',ZST1(1:lzn1)
      endif
      if((lsn0+lzn0).lt.20)then
        WRITE(ZSDESC,'(a,a1,a)') SST1(1:lsn0),':',ZST1(1:lzn0)
      elseif((lsn2+lzn0).lt.20)then
        WRITE(ZSDESC,'(a,a1,a)') SST1(1:lsn2),':',ZST1(1:lzn0)
      elseif((lsn0+lzn2).lt.20)then
        WRITE(ZSDESC,'(a,a1,a)') SST1(1:lsn0),':',ZST1(1:lzn2)
      elseif((lsn2+lzn2).lt.20)then
        WRITE(ZSDESC,'(a,a1,a)') SST1(1:lsn2),':',ZST1(1:lzn2)
      elseif((lsn2+lzn3).lt.20)then
        WRITE(ZSDESC,'(a,a1,a)') SST1(1:lsn2),':',ZST1(1:lzn3)
      elseif((lsn3+lzn2).lt.20)then
        WRITE(ZSDESC,'(a,a1,a)') SST1(1:lsn3),':',ZST1(1:lzn2)
      else
        WRITE(ZSDESC,'(a,a1,a)') SST1(1:lsn2),':',ZST1(1:lzn2)
      endif
      WRITE(ZSDES,'(a,a1,a)') SST1(1:lnblnk(SST1)),':',
     &      ZST1(1:lnblnk(ZST1))

      RETURN
      END

C ******************** ZNARLIST ********************
C ZNARLIST takes number of items (inlist) in the array (list)
C of size (insize) and builds a descriptive string (zdescr)
C of character length (length) to be used in reports.
C Similar to code ZLIST in esrures/utils.F.

      SUBROUTINE ZNARLIST(inlist,list,insize,zdescr,length,ierr)
#include "building.h"
#include "geometry.h"

      dimension list(insize)
      CHARACTER zdescr*248,outs*248,outsd*248
      character t124*124
      logical unixok

      t124=' '
      length=0
      do 42 i=1,inlist
        lna=lnzname(list(i))
        length=length+lna+1
  42  continue
      if(length.lt.236)then
        WRITE(outs,5,iostat=ios,err=1)(zname(list(I)),I=1,inlist)
    5   FORMAT(' Zones: ',20(a12,' '))
        call sdelim(outs,outsd,'S',IW)
        zdescr=outsd
        return
      else
        if(inlist.eq.1)then
         WRITE(ZDESCR,'(A,I2,2A)')' Zone (',list(1),') ',zname(list(1))
        elseif(inlist.gt.1)THEN

C If full names will not fit in zdescr then write out indices only.
C First try to use t124 string (so have room for zones label).
          ipos=1
          call ailist(ipos,inlist,list,MCOM,'C',t124,loutlen,itrunc)
          if(itrunc.eq.0)then
            WRITE(ZDESCR,'(2a)')' Zones: ',t124(1:loutlen)
          else
            call ailist(ipos,inlist,list,MCOM,'C',outs,loutlen,itrunc)
            WRITE(ZDESCR,'(2a)')' Zones: ',outs(1:loutlen)
          endif
        endif
        return
      endif

C Trap for I/O errors.
   1  call isunix(unixok)
      if(unixok)then
        if(IOS.eq.2)then
          write(6,*)'ZARLIST: no permission to write zone names: ',outs
        else
          write(6,*)'ZARLIST: error writing zone names: ',outs
        endif
      endif
      ierr=1
      return
      END

C ******************** DeallocateAllArrays ****************************
C *********************************************************************

C DeallocateAllArrays deallocates all the memory asociated with
C allocatable arrays.
C *********************************************************************

      SUBROUTINE DeallocateAllArrays
      
      USE AIM2, ONLY:AIM2_DEALLOCATE
      
      IMPLICIT NONE
     
      CALL AIM2_DEALLOCATE
      
      END SUBROUTINE DeallocateAllArrays
