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


C Project manager model quality assurance. This file includes the
C following subroutines.
C   prjqa:   writes out a contents (to screen or file) for the current model.
C   mupdate: checks current model to see if it should be updated.

C ******************** prjqa ********************
C Writes contents report (to screen or file) for the current model.

      subroutine prjqa(inpic,ivals,silent,ier)

C AIM-2.
C IAIM2 set to 1 if AIM-2 infiltration model active.
C LAIM2 (72 char) file holding AIM-2 infiltration model data.
C      common/aim2def/iAIM2,LAIM2
      USE AIM2_InputData, ONLY:iAIM2,LAIM2
      USE AIM2, ONLY:AIM2_READIN, AIM2_report
      
      IMPLICIT NONE
      
#include "building.h"
#include "model.h"
#include "site.h"
#include "geometry.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "control.h"
#include "tdf2.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
#include "sbem.h"
#include "ipvdata.h"
#include "seasons.h"
#include "schedule.h"
#include "derived.h"
#include "plant.h"
#include "power.h"
#include "prj3dv.h"
#include "bc_data.h"
#include "help.h"

      integer lnblnk  ! function definition

C Parameters passed to subroutine.
      integer inpic,ivals   ! number of zones to include
      dimension ivals(MCOM) ! list of zones
      logical silent        ! if true do not interact with user
      integer ier           ! non-zero if a problem

      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout

C Markdown flag.
      logical markdown
      common/markdownflag/markdown
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      INTEGER :: ic1,ie1,ict,ic2,ie2
      common/C6/INDCFG
      INTEGER :: INDCFG
      integer :: icascf
      common/cctl/icascf(mcom)

C IFPNF is the unit for the plant network file.
C LPNF (72 char) is the plant network file name.
      COMMON/C23/IFPNF,LPNF
      INTEGER :: IFPNF

C BPF is the file name for boundary values.
C PBYFIL is the logical variable for existence check.
      COMMON/C23a/BPF
      COMMON/PBYDAT/PBYFIL,NSCVP,IPS

      CHARACTER :: BPF*72
      logical :: PBYFIL, autook

      INTEGER :: NSCVP,IPS,IFBPF

      INTEGER :: IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit
      INTEGER :: ixopen,ixloc,ixunit
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      REAL :: TMCT,TMCREF,TMCT2,TMCA,TMCA2
      INTEGER :: ITMCFL
      COMMON/PRECT2/TMCT2(MCOM,MTMC,5),TMCA2(MCOM,MTMC,ME,5),
     &              TVTR2(MCOM,MTMC)
      COMMON/PRECT3/NTMC,NGLAZ(MTMC)
      INTEGER :: NTMC,NGLAZ
      common/PRECT4/TOPTIC(MCOM,MTMC)
      COMMON/TMCB1/IBCMT(MCOM,MTMC)
      COMMON/TMCB2/NBCTMC(MCOM,MTMC),IBCST(MCOM,MTMC),
     &             IBCFT(MCOM,MTMC),IBCSUR(MCOM,MTMC)
      INTEGER :: IBCMT,IBCST,IBCFT,IBCSUR,NBCTMC
      COMMON/TMCB3/NBCTT(MCOM,MTMC),BACTPT(MCOM,MTMC)
      INTEGER :: NBCTT
      REAL :: BACTPT
      common/cctlnm/ctldoc,lctlf
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas

C ISPMXIST is the existence flag for special materials.
      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &              nnodat(mspmnod),spmdat(mspmnod,mspmdat)
      common/spmatlbl/spmlabel(mspmnod)

      integer :: ispmxist,nspmnod,ispmloc,ispmtyp,nnodat
      character :: spflnam*72,spmlabel*16
      real :: spmdat

      dimension itypenum(mspmnod),numoftyp(mspmnod)
      integer :: itypenum,countspmtyp,numoftyp

      integer :: istoreIUOUT,ispm,iTMCidx

C DSC data/ spline function data
      common/pcmspline/temp1d(mspmnod,MSPMSPLM),
     &          heat1d(mspmnod,MSPMSPLM),
     &          cool1d(mspmnod,MSPMSPLM),heat2d(mspmnod,MSPMSPLM),
     &          cool2d(mspmnod,MSPMSPLM),nxy(mspmnod),
     &          bUseSpline(mspmnod),fnamDSCdat(mspmnod)
      real temp1d   ! DSC temperature data values, degC
      real heat1d   ! DSC heating enthalpy values, J/(kg K)
      real cool1d   ! DSC cooling enthalpy values, J/(kg K)
      real heat2d   ! 2nd derivatives at spline x values
      real cool2d
      integer nxy             ! number of data sets
      logical bUseSpline      ! .true. means DSC data file available, use
                              ! cubic spline function for apparent specific
                              ! heat function.
      character fnamDSCdat*72 ! DSC data file name for current spm entry

C Variables for weekdays, and weekends.
C Assume: Mon=1, Tue=2, Wed=3, Thu=4, Fri=5, Sat=6, Sun=7
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2
      INTEGER :: idwe1,idwe2
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      INTEGER :: IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/GTFIL/GTGEOM
      COMMON/GT/GTNAME
      common/PCONV/ipconv,pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
      INTEGER :: ipconv
      REAL :: pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
      common/CONVEM/phtco2,phtnox,phtsox,pclco2,pclnox,pclsox,
     &              pltco2,pltnox,pltsox,pfnco2,pfnnox,pfnsox,
     &              pspco2,pspnox,pspsox,phwco2,phwnox,phwsox

      REAL :: phtco2,phtnox,phtsox,pclco2,pclnox,pclsox
      REAL :: pltco2,pltnox,pltsox,pfnco2,pfnnox,pfnsox
      REAL :: pspco2,pspnox,pspsox,phwco2,phwnox,phwsox
      COMMON/CLMDT1/CLMLOC
      common/IPVF/lipvdatf

      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)

      logical found,XST,apply,duplicate
      logical bndrysxc,context,bound

C External text editor.
      common/texted/tedlbl,teditor

C External figure generator.
      common/figtool/figlbl,figexe

C External graphing tool.
      common/graphtool/grflbl,grfexe

C External folder browser.
      common/folderbr/foldlbl,foldexe

C A set of possible image formats (see config file) and
C matching executables. For normal image viewing take the first option.
      common/showimg/imgtyp,fmttag(5),fmtexe(5)
      INTEGER :: imgtyp
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

C iSlr_half_hr_flg value = 0 :hour-centered; =1 : half-hour centered.
      common/CWEC_SOL/iSlr_half_hr_flg
      integer iSlr_half_hr_flg

C CFD common blocks.
      CHARACTER LCFD*72
      integer IFCFD
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)

C Thermal bridges.
      integer nbrdg, ibrdg
      real psi,lngth,losspercent,totheatloss,thbrpercent
      real uavtotal
      common/THRBRDG/nbrdg(MCOM),psi(MCOM,16),lngth(MCOM,16),
     &               ibrdg(MCOM,16),losspercent(MCOM),totheatloss(MCOM),
     &               thbrpercent,uavtotal(MCOM)

      COMMON/LongRad/iExtLgRadFlag,eGrdTp(12)
      INTEGER iExtLgRadFlag
      REAL eGrdTp

C For reporting day types.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      character calename*32,calentag*12,calendayname*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      integer nbdaytype,nbcaldays,icalender
      integer idty  ! foer loop

C bridgelen was calculated in subroutine suredgeadj
      real bridgelen ! Length of potential thermal bridges in each zone.
      common/gbridge/bridgelen(MCOM,16)

C High level control scope key words.
      character hcffpattern*12    ! heat, cool, or heat+cool plus detail
      common/hlcontrol/hcffpattern(2)     

      integer IHCT,ICOREXHCT
      common/hcthry/ihct,icorexhct  ! surface heat transfer methods inside & outside

      real plen  ! combined length of standard and metal facade perimeter

      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

C Remember if each zone is fully attributed.
      logical attribok(MCOM)
      logical allattribok,changedit,ok,concat,unixok,showother
      dimension areamlc(MCOM),areamlcamb(MCOM),areamlcoth(MCOM)
      dimension areamlcb2b(MCOM),areamlcgrnd(MCOM),areamlcsimil(MCOM)
      real areamlc,areamlcamb,areamlcoth,areamlcb2b,areamlcgrnd
      real areamlcsimil,tmlc
      character items*33
      dimension items(27)

      character*72 LTMP
      character*72 GTGEOM
      character GTNAME*15,CLMLOC*42,lipvdatf*72
      character lco*72,lcot*72
      character longtfile*144,longtfiledos*144
      character DESCRC*25
      character ZN*12,SN*12
      character aok*17,CTLDOC*248,LCTLF*72
      character xfile*144,tg*1,delim*1,head*21
      character outs248*496,outs2*148
      character dstmp*24,mlc_name*32
      CHARACTER LAPROB*72,LPNF*72
      CHARACTER TOPTIC*12,doit*248
      character t16a*16,t16b*16,t16c*16,t16d*16
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15
      character figlbl*20,figexe*48
      character grflbl*20,grfexe*48,foldlbl*20,foldexe*48
      character tmode*8,fmttag*4,fmtexe*20
      character tedlbl*20,teditor*20
      character uname*24,pwd*84
      character ipvaction*3  ! signal that external ipv file
      character lworking*144

C For thermal bridge reporting
      character TBRIDGE*60
      dimension phrasemenu(16),phraselen(16)
      character phrasemenu*34,phraselen*42
      character fs*1
      logical havebrdge,haveedgebrdge
      integer howmanytbedges(16)
      
C Topic variables: value of zero = none, one = compact, two = verbose,
C three is very verbose
C Initial assumption is verbose for most topics that are included
C in the current model. A -1 means not applicable
      integer siteinfo,databaseinfo,contextinfo,controlinfo,netinfo
      integer plantinfo,geominfo,schedinfo,zoneextrainfo,fileinfo
      integer spminfo,enetinfo
      integer zonecolumns,surfcolumns  ! patterns of zone and surface columns
      common/qatopics/siteinfo,databaseinfo,contextinfo,controlinfo,
     &  netinfo,plantinfo,geominfo,schedinfo,zoneextrainfo,fileinfo,
     &  spminfo,enetinfo,zonecolumns,surfcolumns

      integer destination

      integer iercl  ! error state returned for scanning climate list.
      integer llbm,llbn  ! string lengths for obstructions
      integer ivz        ! index of current zone in list
      integer ins2,ins3  ! for radio buttons
      integer loutln     ! length of string returned
      integer itrunc     ! to signal truncation
      integer ipos       ! position indicator when calling arlist
      dimension ZDA(20)  ! array of depths for ground temperature calculation
      real sperim(MS)    ! perimeter of each polygon

      real ZD,ZDA       ! depth for ground temp calc
      real grper        ! % length of ground perimeter in a zone
      real xtranper,xskyper,xwallper,xslproofper,xflatroofper,xgrper ! % of project floor area for each
      real UVG          ! assumed U value at ground connection
      real UVH          ! U value for horizontal flow
      real betaprime    ! for CIBSE ground calcs
      real deltathick   ! dt value for CIBSE ground calcs
      real foundUv      ! U value for foundation for CIBSE ground calcs
      real Ufleft,Ufright  ! two parts of CIBSE Uf calculation std case
      real Ufall        ! sum of Ufleft and Ufright
      real Pi
      real foundRextra  ! extra CIBSE resistance for foundation

      character*10 wkd1, wkd2
      character louts*496,loutsd*496,louts600*600
      character lltmp*144,lguess*144,lldef*144  ! for working with EASKXORGTKF
      character lpath*72,fname*72 ! for use with fdroot
      character llclmdb*144,miscinf*72,msg*42
      character boundmsg*72
      character outscal*248  ! for calendar day types report
      integer llt,lndbp,jj,lnsn
      integer NRGRDT  ! actual number of ground temps calculated
      character outs*124,outs2a*124,outs3*124
      integer llbm2,llbn2,mnu,llo

      logical newgeo  ! to use for testing if new/old geometry file.
      logical nameok  ! to use for testing if duplicate names in zone
      logical foundit ! found a zone loop was referenced
      logical odd     ! for double column reporting
      integer NITMS   ! max items
      integer im,k,mnulen   ! multi column lines
      integer lr      ! length of model root string
      integer iglib   ! for detecting GTK or X11
      integer iitrc   ! for passing verbosity
      integer imlc    ! loop for MLC reporting
      integer lnsmlcn,lnl,lnlist ! char lengths
      integer llpos,lrpos,ulpos,urpos     ! closest to BB corners for parent
      
      INTEGER :: i,icc,icoth,ie,ieripv,irec,iprb,inv
      INTEGER :: ins4,ino,imerr,ilcot,ijk,ij,ii,igrdp
      INTEGER :: ictlf,iuo,iunit1,iunit,iuf,iub,itu,itru,itrc,itbmenu
      INTEGER :: itb,istat,isc,is,ic,ins1,isel,iz,izo,izc,izz
      INTEGER :: kk,j,l,lnb,lnc,lna,lnd,lsn10,lto,lzn10,m,igraphiclib
      INTEGER :: iw,isz
      INTEGER :: lsna,lk,lnk,lnkn  ! counters for string lengths
      REAL :: opq,totalarea,topq,tmp,tfla
      REAL :: tareamlc,trn,ttrn,tvol,tvtr,tvtr2
      INTEGER :: NASURF
      REAL :: UVD,UVI,UVU,vtranper
      real :: XYMAX,ZMAX,dzllff

      helpinsub='prjqa'  ! set for subroutine
      
C Check if Unix-based or DOS based.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      changedit=.false.
      newgeo=.false.     ! assume older format geometry.
      havebrdge=.false.  ! assume no thermal bridges
      IUF=IFIL+2
      siteinfo=1      ! constrained site information
      databaseinfo=2  ! assume verbose mode for database reporting
      contextinfo=1   ! constrained context information
      ICTLF=IUF
      CALL ERPFREE(ICTLF,ISTAT)
      call FINDFIL(LCTLF,XST)
      if(XST)then
        controlinfo=1  ! if control used set to terse
      else
        controlinfo=-1
      endif
      if(ispmxist.gt.0)then
        spminfo=2      ! if a special materials file is included, set to verbose
      else
        spminfo=-1
      endif
      if(IAIRN.ge.1)then
        netinfo=2      ! if a network included set to verbose
      else
        netinfo=-1
      endif
      if(indcfg.eq.2.or.indcfg.eq.3)then
        plantinfo=2   ! if plant included in model set to verbose
      else
        plantinfo=-1
      endif
      if(IENTXIST.gt.0)then
        enetinfo=2      ! if an electrical network is included, set to verbose
      else
        enetinfo=-1
      endif
      if(ncomp.gt.0)then
        if(markdown)then
          databaseinfo=1  ! medium mode for database reporting
          geominfo=2
          schedinfo=2
          zoneextrainfo=2
          zonecolumns=0   ! only name vol floor area and description
          surfcolumns=0   ! summary surface reporting
        else
          geominfo=2
          schedinfo=2
          zoneextrainfo=2
          zonecolumns=3   ! all columns of zone reporting
          surfcolumns=3   ! all columns of surface reporting
        endif
      else
        geominfo=-1
        schedinfo=-1
        zoneextrainfo=-1
        zonecolumns=-1
        surfcolumns=-1
      endif

C Assume no extra file names required and no MLC selected for filter.
      fileinfo=0
      isel = 0

C If silent mode then set file name and jump to report generation.
      if(silent)then
        destination=1
        itu = ixunit

C Create model contents report in the doc folder if possible. Use similar 
C pattern to management of lmodellog.
        lr=lnblnk(cfgroot)
        if(docpth(1:2).eq.'  '.or.docpth(1:2).eq.'./')then
          if(markdown)then
            write(lmodelqa,'(2a)') cfgroot(1:lr),'.md'
          else
            write(lmodelqa,'(2a)') cfgroot(1:lr),'.contents'
          endif
        elseif(docpth(1:3).eq.'../')then
          if(markdown)then
            write(lmodelqa,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &        cfgroot(1:lr),'.md'
          else
            write(lmodelqa,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &        cfgroot(1:lr),'.contents'
          endif
       else
          write(lmodelqa,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &      cfgroot(1:lr),'.contents'
        endif
        write(xfile,'(a)') lmodelqa(1:lnblnk(lmodelqa))
        call ctldumpt(xfile,ixopen,ixloc,ixunit,'contents',IER)
        if(ier.eq.-3)then
          destination = 0
          itu = iuout
          ier=-3
          return
        endif
        
C Prior to silent reporting rescan the construction database (sometimes
C database gets its name corrupted requiring a re-scan).
        CALL ERPFREE(IFMUL,ISTAT)
        if(ipathmul.eq.0.or.ipathmul.eq.1)then
          lworking=lfmul  ! use as is
        elseif(ipathmul.eq.2)then
          lndbp=lnblnk(standarddbpath)
          write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &      lfmul(1:lnblnk(lfmul))  ! prepend db folder path
        endif
        call FINDFIL(lworking,XST)
        if(XST)then
          CALL ERMLDB(0,IUOUT,IER)
          IF(IER.eq.4)THEN
            CALL ERMLDB2(0,iuout,IER)
          endif
        else

C Could not find Constructions db at this time, report to user.
          write(outs248,'(3a)') 'Constructions db ',
     &       LFMUL(1:lnblnk(LFMUL)),' not found!'
          call edisp248(iuout,outs248,100)
          call edisp(iuout,' ')
        endif

C Generate the report based on current options.
        goto 55
      else

C Assume file names not included and destination to screen (zero)
C and itu set so contents is written to screen.
        destination=0
        itu = iuout

      endif

C Setup the selection menu.
    3 INO=-4
      IER=0
      write(items(1),'(a)')  '1 browse folders '
      write(items(2),'(2a)') '2 edit log: ',lmodellog(1:20)
      items(3) ='3 graphing tool               '
      items(4) ='4 diagram editing tool        '
      items(5) ='5 image viewer                '
      items(6) ='  _________________________   '
      items(7) ='... contents options:         '
      if(siteinfo.eq.0)then
        items(8)='a site info >> none'
      elseif(siteinfo.eq.1)then
        items(8)='a site info >> compact'
      elseif(siteinfo.eq.2)then
        items(8)='a site info >> verbose'
      endif
      if(databaseinfo.eq.-1)then
        items(9)='b databases >> none'
      elseif(databaseinfo.eq.0)then
        items(9)='b databases >> compact'
      elseif(databaseinfo.eq.1)then
        items(9)='b databases >> medium'
      elseif(databaseinfo.eq.2)then
        items(9)='b databases >> verbose'
      elseif(databaseinfo.eq.3)then
        items(9)='b databases >> verbose all'
      endif
      if(contextinfo.eq.0)then
        items(10)='c model context >> none'
      elseif(contextinfo.eq.1)then
        items(10)='c model context >> compact'
      elseif(contextinfo.eq.2)then
        items(10)='c model context >> verbose'
      endif
      if(controlinfo.eq.-1)then
        items(11)='d controls >> not applicable'
      elseif(controlinfo.eq.0)then
        items(11)='d controls >> none'
      elseif(controlinfo.eq.1)then
        items(11)='d controls >> compact'
      elseif(controlinfo.eq.2)then
        items(11)='d controls >> verbose'
      elseif(controlinfo.eq.3)then
        items(11)='d controls >> very verbose'
      endif
      if(netinfo.eq.-1)then
        items(12)='e networks >> not applicable'
      elseif(netinfo.eq.0)then
        items(12)='e networks >> none'
      elseif(netinfo.eq.1)then
        items(12)='e networks >> compact'
      elseif(netinfo.eq.2)then
        items(12)='e networks >> verbose'
      endif
      if(plantinfo.eq.-1)then
        items(13)='f plant systems >> not applicable'
      elseif(plantinfo.eq.0)then
        items(13)='f plant systems >> none'
      elseif(plantinfo.eq.1)then
        items(13)='f plant systems >> compact'
      elseif(plantinfo.eq.2)then
        items(13)='f plant systems >> verbose'
      endif
      items(14)='g zone selection'
      if(geominfo.eq.-1)then
        items(15)='g geometry >> not applicable'
      elseif(geominfo.eq.0)then
        items(15)='g geometry >> none'
      elseif(geominfo.eq.1)then
        items(15)='g geometry >> compact'
      elseif(geominfo.eq.2)then
        items(15)='g geometry >> verbose'
      elseif(geominfo.eq.3)then
        items(15)='g geometry >> very verbose'
      endif
      if(zonecolumns.eq.-1)then
        items(16)='h                           '
      elseif(zonecolumns.eq.0)then
        items(16)='h zone columns >>  summary  '
        call edisp(iuout,
     &  'Zone summary: name volume floor area description')
      elseif(zonecolumns.eq.1)then
        items(16)='h zone columns >>  sub-set A'
        call edisp(iuout,
     &  'Zone subset A: name surfs opaque tansp floor area description')
      elseif(zonecolumns.eq.2)then
        items(16)='h zone columns >>  sub-set B'
        call edisp(iuout,
     &  'Zone subset B: name volume surfaces opaque tansp description')
      elseif(zonecolumns.eq.3)then
        items(16)='h zone columns >>  all      '
        call edisp(iuout,
     &  'Zone all: name volume surfaces opaque tansp floor description')
      endif
      if(surfcolumns.eq.-1)then
        items(17)='i                           '
      elseif(surfcolumns.eq.0)then
        items(17)='i surf columns >>  summary  '
        call edisp(iuout,
     &  'Surf summary: area azim elev name use constr boundary')
      elseif(surfcolumns.eq.1)then
        items(17)='i surf columns >>  sub-set A'
        call edisp(iuout,
     &  'Surf subset A: area azim elev name use constr boundary')
      elseif(surfcolumns.eq.2)then
        items(17)='i surf columns >>  sub-set B'
        call edisp(iuout,
     & 'Surf subset B: area azim elev name optical use constr boundary')
      elseif(surfcolumns.eq.3)then
        items(17)='i surf columns >>  all      '
        call edisp(iuout,
     & 'Surf all: area azim elev name optical use location const bndry')
      endif
      if(schedinfo.eq.-1)then
        items(18)='j operations >> none'
      elseif(schedinfo.eq.0)then
        items(18)='j operations >> none'
      elseif(schedinfo.eq.1)then
        items(18)='j operations >> compact'
      elseif(schedinfo.eq.2)then
        items(18)='j operations >> verbose'
      endif
      if(zoneextrainfo.eq.-1)then
        items(19)='k zone extras >> none'
      elseif(zoneextrainfo.eq.0)then
        items(19)='k zone extras >> none'
      elseif(zoneextrainfo.eq.1)then
        items(19)='k zone extras >> compact'
      elseif(zoneextrainfo.eq.2)then
        items(19)='k zone extras >> verbose'
      endif
      if(geominfo.ge.1)then
        items(20)='l sort by surface attrbution'
      else
        items(20)='l n/a'
      endif
      if(fileinfo.eq.0)then
        items(21)='m file names >> none'
      elseif(fileinfo.eq.1)then
        items(21)='m file names >> included'
      endif
      if(destination.eq.0)then
        items(22)='> contents >> text feedback'
      elseif(destination.eq.1)then
        if(lmodelqa(1:4).eq.'UNKN'.or.lmodelqa(1:2).eq.'  ')then
          items(22)='> contents >> file UNKNOWN'
        else
          write(items(22),'(2a)') '> contents >> ',lmodelqa(1:17) 
        endif
      endif
      items(23)  ='! generate contents       '
      if(destination.eq.0)then
        if(lmodelqa(1:4).eq.'UNKN'.or.lmodelqa(1:2).eq.'  ')then
          items(24)='  contents not yet defined'
        else
          write(items(24),'(2a)') '* edit: ',lmodelqa(1:24) 
        endif
      elseif(destination.eq.1)then
        if(lmodelqa(1:4).eq.'UNKN'.or.lmodelqa(1:2).eq.'  ')then
          items(24)='  report not yet defined'
        else
          write(items(24),'(2a)') '* edit: ',lmodelqa(1:24) 
        endif
      endif
      items(25)  ='  _________________________'
      items(26)  ='? help                     '
      items(27)  ='- exit menu'

C Help for menu.
      helptopic='setup_qa_report'
      call gethelptext(helpinsub,helptopic,nbhelp)
      NITMS=27
      call emenu('Model reports',items,NITMS,ino)

C Exit menu.
      if(ino.eq.27)then
        if(changedit)then
          continue
        endif
        return

C Help selected.
      elseif(ino.eq.26)then
        helptopic='setup_qa_report'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('contents overview',nbhelp,'-',0,0,IER)
      elseif(ino.eq.1)then

C << need to sort out changing to main project folder >>
        if(foldexe(1:2).eq.'  ')then
          call usrmsg(
     &    'The folder browsing tool has not be defined. See your',
     &    'administrator about updating the esprc definition.','W')
        else
          write(doit,'(2a)') foldexe(1:lnblnk(foldexe)),' & '
          call usrmsg('Beginning folder view via',doit,'-')
          tmode='-'
          call runit(doit,tmode)
        endif

C Edit project documentation.
      elseif(ino.eq.2)then
        if(CFGOK)then
  54      if(lmodellog(1:2).eq.'  '.or.lmodellog(1:4).eq.'UNKN')then
            lr=lnblnk(cfgroot)
            if(docpth(1:2).eq.'  '.or.docpth(1:2).eq.'./')then
              write(lmodellog,'(2a)') cfgroot(1:lr),'.log'
            elseif(docpth(1:3).eq.'../')then
              write(lmodellog,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &          cfgroot(1:lr),'.log'
            else
              write(lmodellog,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &          cfgroot(1:lr),'.log'
            endif
C            write(lmodellog,'(a,a)') cfgroot(1:lnblnk(cfgroot)),'.log'
          endif

C Detect if working on X11 or GTK and set lguess to pass into EASKXORGTKF.
C The file name returned (lltmp). If X11 lltmp will be the file name and if
C GTK it will include the full path to the file. 
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          lltmp='  '
          lguess='  '
          lldef='project.log'
          if(iglib.eq.2)then
            call addpath(lmodellog,longtfile,concat)  ! expand path for GTK browser
            write(lguess,'(a)') longtfile(1:lnblnk(longtfile))
          else
            write(lguess,'(a)') lmodellog(1:lnblnk(lmodellog))
          endif
          call edisp(iuout,' ')
          CALL EASKXORGTKF(lguess,
     &      ' ','Project log file?',lldef,lltmp,'log file',IER,nbhelp)
          write(lmodellog,'(a)') lguess(1:lnblnk(lguess)) ! might be changed 

C If user request jump back and re-display the menu.
          if(ier.eq.-3)then
            goto 54  ! cancel detected, restore name and redisplay menu.
          endif
          changedit=.true.

C Ask user whether file is in ../doc and if so use fdroot if GTK.
          if(iglib.eq.2)then
            CALL EASKOK(' ','Is this file in the model doc folder?',
     &        OK,3)
            if(OK)then
              call fdroot(lltmp,lpath,fname)
              write(lmodellog,'(3a)') docpth(1:lnblnk(docpth)),fs,
     &          fname(1:lnblnk(fname))
            endif
          else
            write(lmodellog,'(a)') lltmp(1:lnblnk(lltmp))
          endif

C See if file exists, (it should have been created within pregist
C when the model was initially created.
          uname=' '
          call usrname(uname)
          call usrdir(pwd)
          call FINDFIL(lmodellog,XST)
          IUNIT=IFIL+1
          if(XST)then
            CALL LISTAS(IUNIT,lmodellog,IER)
            IF(IER.ne.0)THEN
              CALL EASKOK('Problem reading log file!',
     &                   'Retry?',OK,nbhelp)
              IF(OK)GOTO 54
            ENDIF
          else
            CALL EASKOK('Problem opening log file!',
     &                 'Retry?',OK,nbhelp)
            IF(OK)GOTO 54
          endif

C Allow user to edit the job notes file. Append to path if
C necessary.  If vi then spawn a new window to do the editing.
          CALL EASKOK(' ','Edit file?',
     &      OK,nbhelp)
          IF(OK)then
            if(unixok)then
              call addpath(lmodellog,longtfile,concat)
            else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
              call addpath(lmodellog,longtfile,concat)
              call cmdfiledos(longtfile,longtfiledos,ier)
              longtfile=' '
              longtfile=longtfiledos
            endif
            tmode='-'
            if(teditor(1:2).eq.'  ')then
              call usrmsg(
     &      'The external word processor has not be defined. See your',
     &      'administrator about updating the esprc definition.','W')
            elseif(teditor(1:2).eq.'vi'.or.teditor(1:4).eq.'nano')then
              tmode='text'
            endif
            write(doit,'(a,2x,a,a)') teditor(1:lnblnk(teditor)),
     &        longtfile(1:lnblnk(longtfile)),' & '
            call usrmsg('Begining edit via',doit,'-')
            call runit(doit,tmode)
          endif
        endif

C Execute graphing tool.
      elseif(ino.eq.3)then
        if(grfexe(1:2).eq.'  ')then
          call usrmsg(
     &    'The data analysis tool has not been defined.',
     &    'Install and/or updating your esprc file.','W')
        else
          call tstamp('>','PRJ: start graphing tool')
          write(doit,'(2a)')grfexe(1:lnblnk(grfexe)),' & '
          call usrmsg('Invoking graphing tool via',doit,'-')
          call runit(doit,'text')
        endif
      elseif(ino.eq.4)then

C Execute figure tool.
        if(figexe(1:2).eq.'  ')then
          call usrmsg(
     &    'The diagram tool has not been defined.',
     &    'Install and/or update your epsrc file.','W')
        else
          call tstamp('>','PRJ: start xfig')
          write(doit,'(2a)')figexe(1:lnblnk(figexe)),' & '
          call usrmsg('Invoking figure tool via',doit,'-')
          call runit(doit,'text')
        endif
      elseif(ino.eq.5)then

C Execute image viewer (get tool via prior scan of default file).
        if(fmtexe(1)(1:2).eq.'  ')then
          call usrmsg(
     &    'The image tool has not been defined.',
     &    'Install and/or update your esprc file.','W')
        else
          call tstamp('>','PRJ: start xv')
          write(doit,'(2a)')fmtexe(1)(1:lnblnk(fmtexe(1))),' & '
          call usrmsg('Invoking image tool via',doit,'-')
          call runit(doit,'text')
        endif
      elseif(ino.eq.8)then
        siteinfo=siteinfo+1
        if(siteinfo.gt.2)siteinfo=0
      elseif(ino.eq.9)then
        databaseinfo=databaseinfo+1  ! allow up to 3
        if(databaseinfo.gt.3)databaseinfo=-1
      elseif(ino.eq.10)then
        contextinfo=contextinfo+1
        if(contextinfo.gt.2)contextinfo=0
      elseif(ino.eq.11)then
        if(controlinfo.ne.-1)then
          controlinfo=controlinfo+1
          if(controlinfo.gt.3)controlinfo=0
        endif
      elseif(ino.eq.12)then
        if(netinfo.ne.-1)then
          netinfo=netinfo+1
          if(netinfo.gt.2)netinfo=0
        endif
      elseif(ino.eq.13)then
        if(plantinfo.ne.-1)then
          plantinfo=plantinfo+1
          if(plantinfo.gt.2)plantinfo=0
        endif
      elseif(ino.eq.14)then

C If there are zones select from list.
        if(ncomp.gt.0)then
          INPIC=NCOMP
          CALL EPICKS(INPIC,IVALS,' ','Zones to summarise?',
     &        12,NCOMP,zname,' zone list',IER,nbhelp)
        endif
      elseif(ino.eq.15)then
        if(geominfo.ne.-1)then
          geominfo=geominfo+1
          if(geominfo.gt.3)geominfo=0
        endif
      elseif(ino.eq.16)then
        if(zonecolumns.ne.-1)then
          zonecolumns=zonecolumns+1
          if(zonecolumns.gt.3)zonecolumns=0
        endif
      elseif(ino.eq.17)then
        if(surfcolumns.ne.-1)then
          surfcolumns=surfcolumns+1
          if(surfcolumns.gt.3)surfcolumns=0
        endif
      elseif(ino.eq.18)then
        if(schedinfo.ne.-1)then
          schedinfo=schedinfo+1
          if(schedinfo.gt.2)schedinfo=0
        endif
      elseif(ino.eq.19)then
        if(zoneextrainfo.ne.-1)then
          zoneextrainfo=zoneextrainfo+1
          if(zoneextrainfo.gt.2)zoneextrainfo=0
        endif
      elseif(ino.eq.20)then

C Jump if no zones.
        if(inpic.eq.0) goto 3

C Generate report sorted by surface attributes.
        helptopic='qa_filters'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('global search by attribute',nbhelp,'-',0,0,IER)

        CALL EASKABCD(' ','Filter for surface composition:',
     &    'any construction','those marked UNKNOWN',
     &    'specific construction','cancel',ins4,nbhelp)
        if(ins4.eq.4)then
          return
        endif

        totalarea=0.0
        CALL EASKABCD(' ','First filter--surface opacity:',
     &    'opaque','transparent','both','cancel',ins1,nbhelp)
        if(ins1.eq.4)return

        ins2=1
        call EASKATOG(' ','Second filter--surface contiguity:',
     &      'external','similar','constant','partitions','ground',
     &      'adiabatic','any contiguity',ins2,nbhelp)

        ins3=1
        call EASKATOG(' ','Third filter--surface orientation:',
     &      'vertical','sloped','horizontal up','horizontal down',
     &      'any horizontal','any orientation',' ',ins3,nbhelp)


        call tstamp('>','PRJ: global reporting by attribution')
        call edisp(itu,' ')
        write(outs,'(A,A)') 'cfg: ',LCFGF(1:lnblnk(LCFGF))
        call edisp(itu,outs)
        write(outs,'(A,A)') 'model: ',modeltitle(1:lnblnk(modeltitle))
        call edisp(itu,outs)
        call dstamp(dstmp)
        write(outs,'(A,A)') 'print date: ',dstmp
        call edisp(itu,outs)

        if(ins4.eq.1)then
          call edisp(itu,'Filtering for ANY surface composition.')
        elseif(ins4.eq.2)then
          call edisp(itu,'Filtering for UNKNOWN surface composition.')
        elseif(ins4.eq.3)then
          write(outs,'(3a)') 'Filtering for ',
     &      mlcname(ISEL)(1:lnmlcname(ISEL)),' composition.'
          call edisp(itu,outs)
        endif
        if(ins1.eq.1)then
          call edisp(itu,'Filtering for opaque surfaces.')
        elseif(ins1.eq.2)then
          call edisp(itu,'Filtering for transparent surfaces.')
        elseif(ins1.eq.3)then
          call edisp(itu,'Filtering for opaque or transparent.')
        endif 
        if(ins2.eq.1)then
          call edisp(itu,'Filtering for exterior connections.')
        elseif(ins2.eq.2)then
          call edisp(itu,'Filtering for similar connections.')
        elseif(ins2.eq.3)then
          call edisp(itu,'Filtering for constant connections.')
        elseif(ins2.eq.4)then
          call edisp(itu,'Filtering for internal partitions.')
        elseif(ins2.eq.5)then
          call edisp(itu,'Filtering for ground connections.')
        elseif(ins2.eq.6)then
          call edisp(itu,'Filtering for adiabatic connections.')
        elseif(ins2.eq.7)then
          call edisp(itu,'Filtering for any connections type.')
        endif 
        if(ins3.eq.1)then
          call edisp(itu,'Filtering for vertical orientation.')
        elseif(ins3.eq.2)then
          call edisp(itu,'Filtering for sloped orientation.')
        elseif(ins3.eq.3)then
          call edisp(itu,'Filtering for horizontal facing up.')
        elseif(ins3.eq.4)then
          call edisp(itu,'Filtering for horizontal facing down.')
        elseif(ins3.eq.5)then
          call edisp(itu,'Filtering for horizontal orientation.')
        elseif(ins3.eq.6)then
          call edisp(itu,'Filtering for any orientation.')
        endif 

        WRITE(outs,93)
   93   FORMAT('Surface matching    |  Area  |Azim |Elev|',
     &         ' geometry| construction|environment')
        call edisp(itu,outs)
        WRITE(outs,94)
   94   FORMAT('                    |  m^2   |deg  |deg |',
     &         ' type|loc| name        |other side ')
        call edisp(itu,outs)
        DO 95 IZ=1,INPIC
          IF(IVALS(IZ).GT.0)THEN
            write(zn,'(A)') zname(IVALS(IZ))
            write(outs,'(3a)') ' Scanning: ',zn(1:lnblnk(zn)),
     &         '...'
            CALL USRMSG(' ',outs,'-')

C << it might not be necesary to scan geometry file >>
            LTMP=LGEOM(IVALS(IZ))
            call georead(IUF,LTMP,IVALS(IZ),1,iuout,IER)

C For each surface in current zone check for match to attributes.
            do 11 i=1,nzsur(ivals(iz))
              icc=izstocn(ivals(iz),i)
              apply=.true.
              if(ins1.eq.1.and.SOTF(ivals(iz),i)(1:4).ne.'OPAQ')
     &          apply=.false.
              if(ins1.eq.2.and.SOTF(ivals(iz),i)(1:4).ne.'TRAN')
     &          apply=.false.

C Determine the match in the system topology. Note last parameter of the
C call suradj is a character string.
              CALL SURADJ(IVALS(IZ),I,IE,TMP,IZC,ISC,IC,DESCRC)
              if(ins2.eq.1.and.IE.ne.0)then
                apply=.false.
              elseif(ins2.eq.2.and.IE.ne.1)then
                apply=.false.
              elseif(ins2.eq.3.and.IE.ne.2)then
                apply=.false.
              elseif(ins2.eq.4.and.IE.ne.3)then
                apply=.false.
              elseif(ins2.eq.5.and.IE.ne.4)then
                apply=.false.
              elseif(ins2.eq.6.and.IE.ne.5)then
                apply=.false.
              elseif(ins2.eq.7)then
                continue
              endif

C Orientation check.
C 'vertical','sloped','horizontal up','horizontal down',
C 'any horizontal','any orien.','cancel'.
              if(ins3.eq.1.and.SVFC(ivals(iz),i)(1:4).ne.'VERT')then
                apply=.false.
              elseif(ins3.eq.2.and.SVFC(ivals(iz),i)(1:4).ne.'SLOP')then
                apply=.false.
              elseif(ins3.eq.3.and.SVFC(ivals(iz),i)(1:4).ne.'CEIL')then
                apply=.false.
              elseif(ins3.eq.4.and.SVFC(ivals(iz),i)(1:4).ne.'FLOR')then
                apply=.false.
              elseif(
     &          (ins3.eq.5.and.SVFC(ivals(iz),i)(1:4).eq.'FLOR').or.
     &          (ins3.eq.5.and.SVFC(ivals(iz),i)(1:4).eq.'CEIL'))then
                continue
              elseif(ins3.eq.6)then
                continue
              endif
              if(ins4.eq.1)then
                continue
              elseif(ins4.eq.2)then
                if(SMLCN(IVALS(IZ),I)(1:4).ne.'UNKN')apply=.false.
              elseif(ins4.eq.3)then
                if(SMLCN(IVALS(IZ),I)(1:4).eq.'UNKN')apply=.false.
                lnsmlcn=lnblnk(SMLCN(IVALS(IZ),I))
                if(SMLCN(IVALS(IZ),I)(1:lnsmlcn).ne.
     &             mlcname(ISEL)(1:lnmlcname(ISEL)))then
                  apply=.false.
                endif
              endif
              
              if(apply)then
                write(sn,'(a)') SNAME(IVALS(IZ),I)
C                lsn10=MIN0(lnblnk(sn),10)
C                lzn10=MIN0(lnblnk(zn),10)
                WRITE(outs,'(a,1X,F7.2,F6.1,F6.1,1X,A,
     &            1X,A,1X,A,1X,A)') sn,SNA(IVALS(IZ),I),
     &            SPAZI(IVALS(IZ),I),SPELV(IVALS(IZ),I),
     &            SOTF(IVALS(IZ),I),SVFC(IVALS(IZ),I),
     &            SMLCN(IVALS(IZ),I),SOTHER(IVALS(IZ),I,1)
                call edisp(itu,outs)
                totalarea = totalarea + SNA(IVALS(IZ),I)
              endif
 11         continue
          endif
  95    continue
        write(outs,'(a,f10.2)') 'Area of matching surfaces: ',totalarea
        call edisp(itu,outs)

C If writing to file, call ctlexp to close it and switch back to text feedback.
        if(destination.eq.1)then
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','geom text',IER)
          destination=0
        endif
        MODIFYVIEW=.TRUE.
        MODBND=.TRUE.
      elseif(ino.eq.21)then
        fileinfo=fileinfo+1
        if(fileinfo.gt.1)fileinfo=0
      elseif(ino.eq.22)then

C Setup the name of the model contents report. If name already known
C use it in dialog, otherwise make up standard name in the doc
C folder if it exists.
        if(destination.eq.0)then
          destination = 1
          itu = ixunit
          if(lmodelqa(1:4).eq.'UNKN'.or.lmodelqa(1:2).eq.'  ')then

C Create model contents in the doc folder if possible. Use similar 
C pattern to management of lmodellog.
            lr=lnblnk(cfgroot)
            if(docpth(1:2).eq.'  '.or.docpth(1:2).eq.'./')then
              write(lmodelqa,'(2a)') cfgroot(1:lr),'.contents'
            elseif(docpth(1:3).eq.'../')then
              write(lmodelqa,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &          cfgroot(1:lr),'.contents'
            else
              write(lmodelqa,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &         cfgroot(1:lr),'.contents'
            endif
          endif

C Detect if working on X11 or GTK and set lguess to pass into EASKXORGTKF.
C The file name returned (lltmp). If X11 lltmp will be the file name and if
C GTK it will include the full path to the file.
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          lltmp='  '
          lguess='  '
          lldef='model.contents'
          if(iglib.eq.2)then
            call addpath(lmodelqa,longtfile,concat)  ! expand path for GTK browser
            write(lguess,'(a)') longtfile(1:lnblnk(longtfile))
          else
            write(lguess,'(a)') lmodelqa(1:lnblnk(lmodelqa))
          endif
          call edisp(iuout,'  ')
          CALL EASKXORGTKF(lguess,' ',
     &      'Model contents file:',lldef,lltmp,'contents',IER,nbhelp)

C Jump back and re-display the menu.
          if(ier.eq.-3)then
            goto 3  ! cancel detected, restore name and redisplay menu.
          endif

C Ask user whether file is in ../doc and if so use fdroot if GTK.
          if(iglib.eq.2)then
            CALL EASKOK(' ','Is file in the model doc folder?',
     &        OK,3)
            if(OK)then
              call fdroot(lltmp,lpath,fname)
              write(lmodelqa,'(3a)') docpth(1:lnblnk(docpth)),fs,
     &          fname(1:lnblnk(fname))
            endif
          else
            write(lmodelqa,'(a)') lltmp(1:lnblnk(lltmp))
          endif

C Save the model configuration file to register the report.
          CALL EMKCFG('-',IER)

C Copy lmodelqa to xfile so that ctlexp can deal with it.
          write(xfile,'(2a)')lmodelqa(1:lnblnk(lmodelqa))

C          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','contents',IER)
          call ctldumpt(xfile,ixopen,ixloc,ixunit,'contents',IER)
          if(ier.eq.-3)then
            destination = 0
            itu = iuout
          endif
        elseif(destination.eq.1)then
          destination = 0
          itu = iuout
        endif

C Generate the report based on current options.
      elseif(ino.eq.23)then
        goto 55

C If file just written to then allow browse and
C edit of that file. Use file browser if in GTK mode.
C If vi then spawn a new window to do the editing.
      elseif(ino.eq.24)then
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        lltmp='  '
        lguess='  '
        if(markdown)then
          lldef='model.md'
        else
          lldef='model.contents'
        endif
        if(iglib.eq.2)then
          call addpath(lmodelqa,longtfile,concat)  ! expand path for GTK browser
          write(lguess,'(a)') longtfile(1:lnblnk(longtfile))
        else
          write(lguess,'(a)') lmodelqa(1:lnblnk(lmodelqa))
        endif
        call edisp(iuout,'  ')
        CALL EASKXORGTKF(lguess,' ',
     &    'Model contents file?',lldef,lltmp,'contents file',IER,nbhelp)

C Jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 3  ! cancel detected, restore name and redisplay menu.
        endif

C Depending on whether Unix or DOS based setup paths. If DOS
C then check for spaces in name and change / to \.
        call isunix(unixok)
        if(unixok)then
          call addpath(lltmp,longtfile,concat)
        else
          call addpath(lltmp,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        tmode='-'
        if(teditor(1:2).eq.'vi'.or.teditor(1:4).eq.'nano')tmode='text'
        write(doit,'(a,2x,a,a)') teditor(1:lnblnk(teditor)),
     &    longtfile(1:lnblnk(longtfile)),' & '
        call runit(doit,tmode)

      else

C Not one of the standard choices.
        goto 3
      endif
      goto 3

C Generate the model contents report.
  55  call tstamp('>','PRJ: zone summary')
      call dstamp(dstmp)
      if(markdown)then
        call edisp(itu,'# Synopsis')
      else
        call edisp(itu,'Synopsis')
      endif
      call edisp(itu,' ')
      write(outs248,'(8A)') 'Synopsis of model ',
     &  modeltitle(1:lnblnk(modeltitle)),' defined in ',
     &  LCFGF(1:lnblnk(LCFGF)),' generated on ',
     &  dstmp,'. Associated notes are in ',
     &  lmodellog(1:lnblnk(lmodellog))
      call edisp248(itu,outs248,80)

C Site information.
      if(siteinfo.gt.0)then
        call edisp(itu,' ')
        write(outs248,'(a,f7.2,a,f6.2,2a,i4,5a)') 
     &    'Location latitude ',sitelat,
     &    ' longitude difference ',sitelongdif,
     &    ' (from local time meridian (east +ve). ',
     &    'Simulations year ',IYEAR,' with Weekends occuring on ', 
     &     wkd1(1:lnblnk(wkd1)),' and ',wkd2(1:lnblnk(wkd2)),'.'
        call edisp248(itu,outs248,80)
        if(siteexposureindex.eq.1)then
          WRITE(outs,'(a)')'Site exposure is typical city centre'
        elseif(siteexposureindex.eq.2)then
          WRITE(outs,'(a)')'Site exposure is typical urban'
        elseif(siteexposureindex.eq.3)then
          WRITE(outs,'(a)')'Site exposure is typical rural'
        elseif(siteexposureindex.eq.4)then
          WRITE(outs,'(a)')'Site exposure is city: = sky, grnd, bldgs'
        elseif(siteexposureindex.eq.5)then
          WRITE(outs,'(a)')'Site exposure is city: below surroundings'
        elseif(siteexposureindex.eq.6)then
          WRITE(outs,'(a)')'Site exposure is isolated rural'
        elseif(siteexposureindex.eq.7)then
          WRITE(outs,'(a)')'Site exposure is totally enclosed (no sky)'
        elseif(siteexposureindex.eq.8)then
          WRITE(outs,53)skyview,groundview,buildingview
  53      FORMAT('Site exposure is sky=',F4.2,' ground=',F4.2,
     &      ' other buildings=',F4.2)
        endif
        call edisp(itu,outs)
        call edisp(itu,'  ')

C If surface heat transfer directives report.
        if(icorexhct.eq.0) msg='no current exterior hc method'
        if(icorexhct.eq.1) msg='default McAdams exterior hc method'
        if(icorexhct.eq.2) msg='MoWiTT  exterior hc method'
        if(icorexhct.eq.3) msg='Aya Hagishima correlation exterior hc'
        if(icorexhct.eq.4) msg='Ya Liu (roof wind speed) exterior hc'
        if(icorexhct.eq.5) msg='Ya Lui (raw wind speed) exterior hc'
        if(icorexhct.eq.6) msg='Loveday (roof wind speed) exterior hc'
        if(icorexhct.eq.7) msg='Loveday (raw wind speed) exterior hc'
        if(icorexhct.eq.8) msg='CIBSE guide exterior hc'
        if(icorexhct.eq.9) msg='ASHRAE Task Group  exterior hc'
        if(icorexhct.eq.10)msg='Sturrock exterior hc'
        if(icorexhct.eq.11)msg='Keith Nicol exterior hc'
        if(icorexhct.eq.12)msg='S.E .G.Jayamaha exterior hc'
        call edisp(itu,'Exterior surface heat transfer method:')
        call edisp(itu,msg)
        call edisp(itu,' ')
        if(ihct.eq.0) msg='No interior HTC default method '
        if(ihct.eq.1) msg='Alamdari and Hammond (default) '
        if(ihct.eq.2) msg='Khalifa & Marshall, radiator under window'
        if(ihct.eq.3) msg='Khalifa & Marshall, no rad under window'
        if(ihct.eq.4) msg='Halcrow, time invariant low    '
        if(ihct.eq.5) msg='Halcrow, time invariant high   '
        if(ihct.eq.6) msg='CIBSE guide, time invariant    '
        if(ihct.eq.7) msg='CEN simplified calc method     '
        call edisp(itu,'Interior surface heat transfer method:')
        call edisp(itu,msg)
        call edisp(itu,' ')

C Ground reflection model.
        if (groundreflmodel.eq.1) then
          write(outs248,'(3a,f4.2,a)') 'The ',outs(1:lnblnk(outs)),
     &      ' and the ground reflectance is ',groundrefl,'.'
          call edisp248(itu,outs248,80)
        elseif (groundreflmodel.eq.2) then
          write(outs248,'(3a)') 'The ',outs(1:lnblnk(outs)),
     &      '. The ground reflectance is defined with monthly values:'
          call edisp248(itu,outs248,80)

C Write monthly values.
          itrunc=1
          ipos=1
          do while (itrunc.ne.0)
            call arlist2(ipos,12,groundreflmonth,12,'S',outs248,
     &        loutln,itrunc)
            call edisp248(itu,outs248,100)
            ipos=itrunc+1
          end do

          call edisp(itu,' ')
          write(outs248,'(a)')
     &      'Snow cover is defined as days per month with snow:'

C Write monthly values.
          itrunc=1
          ipos=1
          do while (itrunc.ne.0)
            call ailist(ipos,12,dayswithsnow,12,'S',outs248,
     &        loutln,itrunc)
            call edisp248(itu,outs248,100)
            ipos=itrunc+1
          end do

        else ! groundreflmodel.eq.3
          write(outs248,'(3a)') 'The ',outs(1:lnblnk(outs)),
     &      '. The ground reflectance is defined with monthly values:'
          call edisp248(itu,outs248,80)

C Write monthly values.
          itrunc=1
          ipos=1
          do while (itrunc.ne.0)
            call arlist2(ipos,12,groundreflmonth,12,'S',outs248,
     &        loutln,itrunc)
            call edisp248(itu,outs248,110)
            ipos=itrunc+1
          end do

          call edisp(itu,' ')
          write(outs248,'(3a)')
     &      'Hourly snow coverage is defined via the data file ',
     &           SNFNAM(1:lnblnk(SNFNAM)),'.'
          call edisp248(itu,outs248,80)

        endif

C Print address for building and client. If there is a UK NCM
C description then list that out instead.
        if(siteinfo.eq.2)then
          if(ISBEM.ge.1)then
            if(LASBEM(1:4).eq.'UNKN')then
              continue
            else

C Re-read sbem db and then the descriptive file.
              call sbempr(ier)
              call rsbem
              call lstncm('p',itu)
              call edisp(itu,'  ')          
              call lstncm('e',itu)
            endif

            call edisp(itu,'  ')          
            if(LASBEM(1:4).eq.'UNKN')then
              continue
            else
              call lstncm('b',itu)
              call edisp(itu,'  ')          
              call lstncm('s',itu)
              call edisp(itu,'  ')          
              call lstncm('d',itu)
              call edisp(itu,'  ')          
              call lstncm('o',itu)
              call edisp(itu,'  ')          
              call lstncm('l',itu)
              call edisp(itu,'  ')
            endif 
          else
            continue
          endif          
        endif

        call CLMOPB(0,IER)
        call CLMRDBMD(IER)

C Tell about climate taking note of timing of solar data.
        if(iSlr_half_hr_flg.eq.0)then
          if(markdown)then
            WRITE(outs248,'(5A)')'Weather file: ',
     &        CLMLOC(1:lnblnk(CLMLOC)),' in ',
     &          LCLIM(1:lnblnk(LCLIM)),
     &      ' with hour-centred solar data.  '
            call edisp248(itu,outs248,120)
          else
            WRITE(outs248,'(5A)')'Weather file: ',
     &        CLMLOC(1:lnblnk(CLMLOC)),' in ',
     &          LCLIM(1:lnblnk(LCLIM)),
     &      ' with hour-centred solar data.'
            call edisp248(itu,outs248,80)
          endif
        else
          if(markdown)then
            WRITE(outs248,'(5A)')'Weather file is: ',
     &        CLMLOC(1:lnblnk(CLMLOC)),' in ',
     &        LCLIM(1:lnblnk(LCLIM)),
     &        ' with half hour-centred solar data.  '
            call edisp248(itu,outs248,80)
          else
            WRITE(outs248,'(5A)')'Weather file is: ',
     &        CLMLOC(1:lnblnk(CLMLOC)),' i: ',
     &        LCLIM(1:lnblnk(LCLIM)),
     &        ' with half hour-centred solar data.'
            call edisp248(itu,outs248,80)
          endif
        endif

        if(ipathclim.eq.0.or.ipathclim.eq.1)then
          if(markdown)then
            WRITE(outs2,'(3A)')'annual weather ',
     &        LCLIM(1:lnblnk(LCLIM)),'  '
            call edispxtr(itu,outs2)          
          else
            WRITE(outs248,'(2A)')' annual weather         : ',
     &      LCLIM(1:lnblnk(LCLIM))
            call edisp248(itu,outs248,100)
          endif
        elseif(ipathclim.eq.2)then
          if(markdown)then
            WRITE(outs2,'(3A)')'Standard annual weather ',
     &        LCLIM(1:lnblnk(LCLIM)),'  '
            call edispxtr(itu,outs2)          
          else
            WRITE(outs248,'(2A)')'Standard annual weather: ',
     &        LCLIM(1:lnblnk(LCLIM))
            call edisp248(itu,outs248,100)
          endif
        endif

C List the estimated ground temperatures based on the current
C weather data at 0.5m, 1m, 2m, 4m depth.
        if(siteinfo.ge.2)then
          ZDA = (/ 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0,
     &             5.5, 6.0, 6.5, 7.0, 7.5, 8.0, 8.5, 9.0, 9.5, 10.0 /)
          NRGRDT = 20

          WRITE(outs,'(A)')'Calculated ground temperature at depth '
          call edispxtr(itu,outs)

          do 19 II=1,NRGRDT
            ZD=ZDA(II)
            CALL GTCALC(ZD,'-',IER)
            itrunc=1
            ipos=1
            do while (itrunc.ne.0)
              call arlist(ipos,12,eGrdTp,12,'S',outs2,loutln,itrunc)
              if(markdown)then
                WRITE(outs248,'(A,f4.1,A,A)')' ',ZDA(II),' m: ',
     &            outs2(1:lnblnk(outs2))
                call edispxtr(itu,outs248)
              else
                WRITE(outs248,'(A,f4.1,A,A)')' ',ZDA(II),' m: ',
     &            outs2(1:lnblnk(outs2))
                call edisp248(itu,outs248,120)
              endif
              ipos=itrunc+1
            end do
  19      continue

          call edisp(itu,' ')
        endif

C List out the maximum and minimum thickness of constructions in
C contact with the ground.
C << to be done >>

C List out the minimum and maximum thickness of constructions
C marked as WALL.
C << to be done >>

        if(NGRDP.gt.0.and.siteinfo.ge.1)then
          call edisp(itu,' ')  
          write(outs,'(a,i2,a)')'There are currently ',NGRDP,
     &      ' user-defined ground temperature profiles.' 
          if(markdown)then
            call edisp2tr(itu,outs)
          else     
            call edisp(itu,outs)
          endif     
        endif
      endif

C Jump back point if err=1000 called when reading climate.
 1001 if(siteinfo.eq.2)then
        if(NGRDP.gt.0)then
          if(markdown)then
            call edispxtr(itu,'Ground temperatures Jan-Dec  ')
          else
            if (NGRDPH.gt.0) then
              call edisp(itu,'Ground temperatures/humidities Jan-Dec:')
            else
              call edisp(itu,'Ground temperatures Jan-Dec:')
            endif
          endif
          do 22 igrdp=1,NGRDP
            WRITE(OUTS,'(12F7.1)')(UGRDTP(J,IGRDP),J=1,12)
            call edisp(itu,outs)
            if (NGRDPH.gt.0) then
              WRITE(OUTS,'(12F7.1)')(UGRDHUM(J,IGRDP),J=1,12)
              call edisp(itu,outs)
            endif
 22       continue
          call edisp(itu,' ')
        endif
        if(GTGEOM(1:2).eq.'  '.or.GTGEOM(1:4).eq.'UNKN')then
          continue
        else
          WRITE(outs248,'(5A)') 'The model includes a ground topology ',
     &      GTNAME(1:lnblnk(GTNAME)),', which is defined in ',
     &      GTGEOM(1:lnblnk(GTGEOM)),'.'
          call edisp248(itu,outs248,80)
        endif
      endif

C Context of the model.
      if(contextinfo.eq.1)then
        if(ipconv.eq.1)then
          call edisp(itu,
     &'Primary energy conversions (demand kWhr to primary kWhr) used.')
        endif
      elseif(contextinfo.eq.2)then

C If there is an AIM-2 descritption, scan the file and then report.
        if(iAIM2.eq.1)then
          call AIM2_READIN
          call AIM2_report(itu)
        endif

        if(ipconv.eq.1)then
          if(markdown)then
            call edisp(itu,
     &': Primary energy conversions (demand kWhr to primary kWhr)')
            call edisp(itu,' ')
            call edisp(itu,'------------    -----')
            WRITE(outs,'(A,F6.3)') 'heating        ',pcnvht
            call edisp(itu,outs)
            WRITE(outs,'(A,F6.3)') 'cooling        ',pcnvcl
            call edisp(itu,outs)
            WRITE(outs,'(A,F6.3)') 'lighting       ',pcnvlt
            call edisp(itu,outs)
            WRITE(outs,'(A,F6.3)') 'fans&pumps     ',pcnvfn
            call edisp(itu,outs)
            WRITE(outs,'(A,F6.3)') 'small powr     ',pcnvsp
            call edisp(itu,outs)
            WRITE(outs,'(A,F6.3)') 'hot water      ',pcnvhw
            call edisp(itu,outs)
            call edisp(itu,'------------    -----')
          else
            call edisp(itu,
     &'Primary energy conversions (demand kWhr to primary kWhr) are:')
            WRITE(outs,'(A,F6.3)') ' heating   :',pcnvht
            call edisp(itu,outs)
            WRITE(outs,'(A,F6.3)') ' cooling   :',pcnvcl
            call edisp(itu,outs)
            WRITE(outs,'(A,F6.3)') ' lighting  :',pcnvlt
            call edisp(itu,outs)
            WRITE(outs,'(A,F6.3)') ' fans&pumps:',pcnvfn
            call edisp(itu,outs)
            WRITE(outs,'(A,F6.3)') ' small powr:',pcnvsp
            call edisp(itu,outs)
            WRITE(outs,'(A,F6.3)') ' hot water :',pcnvhw
            call edisp(itu,outs)
          endif
          call edisp(itu,' ')
          if(markdown)then
            call edisp(itu,' ')
            call edisp(itu,': Emissions (g/kWh)')
            call edisp(itu,' ')
            call edisp(itu,'Source       co2      nox    sox')
            call edisp(itu,'-----------  -------  -----  ------')
            WRITE(outs,'(A,3F7.2)')'heating     ',phtco2,phtnox,phtsox
            call edisp(itu,outs)
            WRITE(outs,'(A,3F7.2)')'cooling     ',pclco2,pclnox,pclsox
            call edisp(itu,outs)
            WRITE(outs,'(A,3F7.2)')'lighting    ',pltco2,pltnox,pltsox
            call edisp(itu,outs)
            WRITE(outs,'(A,3F7.2)')'fan_pump    ',pfnco2,pfnnox,pfnsox
            call edisp(itu,outs)
            WRITE(outs,'(A,3F7.2)')'small pr    ',pspco2,pspnox,pspsox
            call edisp(itu,outs)
            WRITE(outs,'(A,3F7.2)')'hot water   ',phwco2,phwnox,phwsox
            call edisp(itu,outs)
            call edisp(itu,'-----------  -------  -----  ------')
          else
            call edisp(itu,' Emissions (g/kWh) CO2  NOx  SOx  ')
            WRITE(outs,'(A,3F7.2)')' heating : ',phtco2,phtnox,phtsox
            call edisp(itu,outs)
            WRITE(outs,'(A,3F7.2)')' cooling : ',pclco2,pclnox,pclsox
            call edisp(itu,outs)
            WRITE(outs,'(A,3F7.2)')' lighting: ',pltco2,pltnox,pltsox
            call edisp(itu,outs)
            WRITE(outs,'(A,3F7.2)')' fan_pump: ',pfnco2,pfnnox,pfnsox
            call edisp(itu,outs)
            WRITE(outs,'(A,3F7.2)')' small pr: ',pspco2,pspnox,pspsox
            call edisp(itu,outs)
            WRITE(outs,'(A,3F7.2)')' hot water:',phwco2,phwnox,phwsox
            call edisp(itu,outs)
          endif
        endif
      endif

C Check if IPV file and dispersed demands files are known.
      if(contextinfo.gt.1)then
        if(lnblnk(bdmds).eq.0)then
          continue
        elseif(bdmds(1:7).eq.'UNKNOWN')then
          continue
        else
          IUO=IFIL+1
          XST=.FALSE.
          call FINDFIL(bdmds,XST)
          if(XST)then
            CALL ERPFREE(IUO,ISTAT)
            CALL ERBDMD(ITRC,ITRU,IUO,IER)
            call edisp(itu,' ')
            call edisp(itu,
     &'Model includes dispersed casual gains (e.g. pumps & fans).')
            if(contextinfo.eq.1)then
              call edisp248(itu,dmdsdesc,80)
            elseif(contextinfo.eq.2)then
              call BDMDINF(itu,IER)
            endif
          endif
        endif

C Integrated Performance View. If compact only print description,
C if verbose print periods, seasons and ratios as well.
        ieripv=1  ! there is no IPV
        if(icfgv.lt.4)then
          if(lnblnk(lipvdatf).eq.0)then
            ieripv=1  ! there is no IPV
          elseif(lipvdatf(1:7).eq.'UNKNOWN')then
            ieripv=1  ! there is no IPV
          elseif(lipvdatf(1:8).eq.'internal')then
            if(nipvassmt.eq.0)then ! there was nothing in the cfg file.
              ieripv=1  ! there is no IPV
            else
              ieripv=0  ! there is an IPV assessment
            endif
          else
            ipvaction='ipv'
            call ripvdat(IFCFG,lipvdatf,ipvaction,ieripv)  ! this returns a value for ieripv
            call edisp(itu,' ')
          endif
        else

C Check if embedded within configuration file.
          if(nipvassmt.eq.0)then ! there was nothing in the cfg file.
            ieripv=1  ! there is no IPV
          else
            ieripv=0  ! there is an IPV assessment
          endif
        endif
        if(ieripv.eq.0)then
          if(markdown)then
            call edisp(itu,'  ')
            call edisp2tr(itu,
     &        '## Integrated Performance View  ')
            call edisp2tr(itu,
     &        'An Integrated Performance View has been defined  ')
          else
            call edisp(itu,
     &        'An Integrated Performance View has been defined:')
          endif
          if(contextinfo.eq.1)then
            if(markdown)then
              WRITE(outs,'(3A)')'Title ',ipvtitl(1:lnblnk(ipvtitl)),
     &          '  '
              call edisp2tr(itu,outs)
            else
              WRITE(outs,'(2A)')'Title   : ',ipvtitl(1:lnblnk(ipvtitl))
              call edisp(itu,outs)
            endif
            if(markdown)then
              call edisp2tr(itu,'Synopsis  ')
            else
              call edisp(itu,'Synopsis:')
            endif
            call edisp248(itu,ipvsynop,80)
          elseif(contextinfo.eq.2)then

C If there is no local season definition, rescan the climatelist
C file to get seasons. If this was successful then include
C a description of the IPV. Otherwise warn user about
C the climate file and attempt to print out IPV contents.
            if(ihaveseason.ge.1)then
              call listipvdat(itu,'a',ier)
            else
              INQUIRE (FILE=cdblfil,EXIST=XST)
              if(XST)then
                llt=lnblnk(LCLIM)
                lndbp=lnblnk(standardclmpath)
                if(ipathclim.eq.0.or.ipathclim.eq.1)then
                  llclmdb=LCLIM
                elseif(ipathclim.eq.2)then
                  write(llclmdb,'(3a)') standardclmpath(1:lndbp),fs,
     &              LCLIM(1:llt)
                endif
                IUF=IFIL+2
                call scancdblist(IUF,llclmdb,'p',ok,iercl)
                if(iercl.eq.0)then
                  call listipvdat(itu,'a',ier)
                else

C Set Default season definitions.
                  CALL EDAY(1,1,is1wins); CALL EDAY(28,2,is1winf)
                  CALL EDAY(1,11,is2wins); CALL EDAY(31,12,is2winf)
                  CALL EDAY(1,3,is1sprs); CALL EDAY(30,4,is1sprf)
                  CALL EDAY(1,9,is2sprs); CALL EDAY(31,10,is2sprf)
                  CALL EDAY(1,5,is1sums); CALL EDAY(31,8,is1sumf)
                  call edisp(itu,
     &              'Weather file is not in climatelist and no local')
                  call edisp(itu,
     &              'seasons are set - default seasons assumed.')
                  call listipvdat(itu,'a',ier)
                endif
              endif
            endif
          endif
        else
          continue
        endif

C If temporal data associated with the model report it.
        if(iabs(itdflg).eq.0)then
          continue
        elseif(iabs(itdflg).eq.1)then
          call edisp(itu,' ')
          call edisp(itu,'Temporal data is available for this model.')
          call edisp(itu,'It is an older file so update it via the')
          call edisp(itu,'edit/list option.')
        elseif(iabs(itdflg).eq.2)then
          call edisp(itu,' ')
          call edisp(itu,'Temporal data is available for this model.')
          call edisp(itu,'It is a slightly out of date file. Update')
          call edisp(itu,'it via the edit/list option.')
        elseif(iabs(itdflg).eq.3)then
          call edisp(itu,' ')
          call edisp(itu,'Temporal data is available for this model.')
          if(ITEMSTD.gt.0)then
            call edisp(itu,' ')
            call edisp(itu,' Temporal entities currently used...')
            call edisp(itu,'  ________________________________________')
            call edisp(itu,'  |temporal    |generic     |associated   ')
            call edisp(itu,
     &        '  |entity name |type        |with   zone &     surface ')
            do 28, l=1,ITEMSTD
              WRITE(outs,29)l,DBTAG(L),DBTASK(L),DBZN(L),DBSN(L)
   29         FORMAT(i2,1X,A12,1X,A8,5X,A16,A16)
              call edisp(itu,outs)
  28        continue
            call edisp(itu,' ')
          endif
        endif
      endif

C Databases associated with the model. First print the file names
C what ever the value of databaseinfo.
      if(databaseinfo.ge.0)then
        call edisp(itu,' ')
        if(markdown)then
          call edisp(itu,'## Databases')
          call edisp(itu,': Databases associated with the model')
          call edisp(itu,' ')
          call edisp(itu,
     &      '-------------------------  --------------------')
        else
          call edisp(itu,'Databases associated with the model: ')
        endif
        if(ipathapres.eq.0.or.ipathapres.eq.1)then
          if(markdown)then
            WRITE(outs248,'(2A)')'pressure distributions     ',
     &        LAPRES(1:lnblnk(LAPRES))
          else
            WRITE(outs248,'(2A)')' pressure distributions :  ',
     &        LAPRES(1:lnblnk(LAPRES))
          endif
          call edisp248(itu,outs248,100)
        elseif(ipathapres.eq.2)then
          if(markdown)then
            WRITE(outs248,'(2A)')'standard pressure distr    ',
     &        LAPRES(1:lnblnk(LAPRES))
          else
            WRITE(outs248,'(2A)')' standard pressure distr: ',
     &        LAPRES(1:lnblnk(LAPRES))
          endif
          call edisp248(itu,outs248,100)
        endif

        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          if(markdown)then
            WRITE(outs248,'(2A)')'materials                  ',
     &        LFMAT(1:lnblnk(LFMAT))
          else
            WRITE(outs248,'(2A)')' materials              :  ',
     &        LFMAT(1:lnblnk(LFMAT))
          endif
          call edisp248(itu,outs248,100)
        elseif(ipathmat.eq.2)then
          if(markdown)then
            WRITE(outs248,'(2A)')'standard materials         ',
     &        LFMAT(1:lnblnk(LFMAT))
          else
            WRITE(outs248,'(2A)')' standard materials     : ',
     &        LFMAT(1:lnblnk(LFMAT))
          endif
          call edisp248(itu,outs248,100)
        endif

        if(ipathmul.eq.0.or.ipathmul.eq.1)then
          if(markdown)then
            WRITE(outs248,'(2A)')'constructions              ',
     &        LFMUL(1:lnblnk(LFMUL))
          else
            WRITE(outs248,'(2A)')' constructions          :  ',
     &        LFMUL(1:lnblnk(LFMUL))
          endif
          call edisp248(itu,outs248,100)
        elseif(ipathmul.eq.2)then
          if(markdown)then
            WRITE(outs248,'(2A)')'standard constructions     ',
     &        LFMUL(1:lnblnk(LFMUL))
          else
            WRITE(outs248,'(2A)')' standard constructions : ',
     &        LFMUL(1:lnblnk(LFMUL))
          endif
          call edisp248(itu,outs248,100)
        endif

        if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
          if(markdown)then
            WRITE(outs248,'(2A)')'plant components           ',
     &        LPCDB(1:lnblnk(LPCDB))
          else
            WRITE(outs248,'(2A)')' plant components       :  ',
     &        LPCDB(1:lnblnk(LPCDB))
          endif
          call edisp248(itu,outs248,100)
        elseif(ipathpcdb.eq.2)then
          if(markdown)then
            WRITE(outs248,'(2A)')'standard plant comp        ',
     &        LPCDB(1:lnblnk(LPCDB))
          else
             WRITE(outs248,'(2A)')' standard plant comp    : ',
     &        LPCDB(1:lnblnk(LPCDB))
          endif
         call edisp248(itu,outs248,100)
        endif

        if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
          if(markdown)then
            WRITE(outs248,'(2A)')'event profiles             ',
     &        LPRFDB(1:lnblnk(LPRFDB))
          else
            WRITE(outs248,'(2A)')' event profiles         :  ',
     &        LPRFDB(1:lnblnk(LPRFDB))
          endif
          call edisp248(itu,outs248,100)
        elseif(ipathprodb.eq.2)then
          if(markdown)then
            WRITE(outs248,'(2A)')'standard event profiles    ',
     &        LPRFDB(1:lnblnk(LPRFDB))
          else
            WRITE(outs248,'(2A)')' standard event profiles: ',
     &        LPRFDB(1:lnblnk(LPRFDB))
          endif
          call edisp248(itu,outs248,100)
        endif

        if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
          if(markdown)then
            WRITE(outs248,'(2A)')'optical properties         ',
     &        LOPTDB(1:lnblnk(LOPTDB))
          else
            WRITE(outs248,'(2A)')' optical properties     : ',
     &        LOPTDB(1:lnblnk(LOPTDB))
          endif
          call edisp248(itu,outs248,100)   
        elseif(ipathoptdb.eq.2)then
          if(markdown)then
            WRITE(outs248,'(2A)')'standard optical prop      ',
     &        LOPTDB(1:lnblnk(LOPTDB))
          else
            WRITE(outs248,'(2A)')' standard optical prop  : ',
     &        LOPTDB(1:lnblnk(LOPTDB))
          endif
          call edisp248(itu,outs248,100)
        endif

        if(ipathsbem.eq.0.or.ipathsbem.eq.1)then
          if(markdown)then
            WRITE(outs248,'(2a)')'UK NCM data                ',
     &        LSBEM(1:lnblnk(LSBEM))
          else
            WRITE(outs248,'(2a)')' UK NCM data            : ',
     &        LSBEM(1:lnblnk(LSBEM))
          endif
          call edisp248(itu,outs248,100)   
        elseif(ipathsbem.eq.2)then
          if(markdown)then
            WRITE(outs248,'(2a)')'standard UK NCM data       ',
     &        LSBEM(1:lnblnk(LSBEM))
          else
            WRITE(outs248,'(2a)')' standard UK NCM data   : ',
     &        LSBEM(1:lnblnk(LSBEM))
          endif
          call edisp248(itu,outs248,100)   
        endif

        if(ipathpredef.eq.0.or.ipathpredef.eq.1)then
          if(markdown)then
            WRITE(outs248,'(2a)')'predefined objects         ',
     &        LPREDEF(1:lnblnk(LPREDEF))
          else
            WRITE(outs248,'(2a)')' predefined objects     : ',
     &        LPREDEF(1:lnblnk(LPREDEF))
          endif
          call edisp248(itu,outs248,100)   
        elseif(ipathpredef.eq.2)then
          if(markdown)then
            WRITE(outs248,'(2a)')'standard predefined obj    ',
     &        LPREDEF(1:lnblnk(LPREDEF))
          else
            WRITE(outs248,'(2a)')' standard predefined obj: ',
     &        LPREDEF(1:lnblnk(LPREDEF))
          endif
          call edisp248(itu,outs248,100)   
        endif

        if(ipathmould.eq.0.or.ipathmould.eq.1)then
          if(markdown)then
            WRITE(outs248,'(2A)')'mould isopleths            ',
     &        lfmould(1:lnblnk(lfmould))
          else
            WRITE(outs248,'(2A)')' mould isopleths        : ',
     &        lfmould(1:lnblnk(lfmould))
          endif
          call edisp248(itu,outs248,100)
        elseif(ipathmould.eq.2)then
          if(markdown)then
            WRITE(outs248,'(2A)')'standard mould isopleth    ',
     &        lfmould(1:lnblnk(lfmould))
          else
            WRITE(outs248,'(2A)')' standard mould isopleth: ',
     &        lfmould(1:lnblnk(lfmould))
          endif
          call edisp248(itu,outs248,100)
        endif
        if(markdown)then
          call edisp(itu,
     &      '-------------------------  --------------------')
        endif
      endif

C===============================================================
C Special materials associated with the model, if any
      if (spminfo.gt.0) then
        WRITE(outs,240)
        call edisp(itu,outs) ! write separation line
        WRITE(outs248,'(2A)')' Special materials file : ',
     &                             spflnam(1:lnblnk(spflnam))
        call edisp(itu,' ') ! empty line
        call edisp248(itu,outs248,100)
C       Initialize special material common data
        call SPMINIT
C       Write out some info on special materials
        call edisp(itu,
     &     '  Details of special materials in the model:')
        call edisp(itu,
     &     '  Type  | Number of entries ')

        countspmtyp=1
        numoftyp(countspmtyp)=0
        do i=1,nspmnod
          itypenum(countspmtyp)=ispmtyp(i,1)
          if (itypenum(countspmtyp) .eq. ispmtyp(i,1)) then
            numoftyp(countspmtyp) = numoftyp(countspmtyp) + 1
          else
            countspmtyp = countspmtyp + 1
          endif
        enddo ! loop through spm nodes
        do i=1,countspmtyp
          write(outs248,'(a,I4,a,I4)')'  ',itypenum(i),
     &                                 '  |      ',numoftyp(i)
          call edisp248(itu,outs248,100)
        enddo ! output "statistics"

        call edisp(itu,' ') ! empty line

C Report detailed
        if (spminfo.gt.1) then ! verbose mode
          do 33 ispm=1,nspmnod
            if (ispm.eq.1) then ! write header only once
              WRITE(OUTS,'(3A)')
     &        ' No. Label            Zone         Surface     ',
     &        ' Construction         Node ',
     &        ' Type  Num Data   Misc.'
              CALL EDISP(itu,OUTS)
            endif

C << could be enhanced with type-specific output of some data, e.g. nominal
C    power for PV and total latent heat capacity for PCM ... >>

C Obtain transparent surface index (>0) related to PV module location.
            iTMCidx=ITMCFL(ispmloc(ispm,1),ispmloc(ispm,2))

            if (bUseSpline(ispm)) then
              write(miscinf,'(a)')fnamDSCdat(ispm)
            else
              write(miscinf,'(a)')'n.a. '
            endif

            WRITE(LOUTS,1111)ispm,spmlabel(ispm),zname(ispmloc(ispm,1)),
     &        sname(ispmloc(ispm,1),ispmloc(ispm,2)),
     &        SMLCN(ispmloc(ispm,1),ispmloc(ispm,2))(1:20),
     &        ispmloc(ispm,3),
     &        (merge('Trn','Opq',iTMCidx.gt.0)),nnodat(ispm),
     &        miscinf(1:lnblnk(miscinf))
 1111       FORMAT(1X,I3,1X,A,1X,A,1X,A,1X,A,1X,I3,3X,A,3X,I3,8X,A,1X)

            call edisp248(itu,LOUTS,120)

  33      continue ! loop through all spm entries

          call edisp(itu,' ') ! empty line
        endif ! verbose mode
      endif ! Special materials file

C Control in the model
      if(controlinfo.gt.0)then

C List description of control and then the various zone descriptions.
C List control data.
        call edisp(itu,' ')
        IUF=IFIL+2
        ICTLF=IUF
        CALL ERPFREE(ICTLF,ISTAT)
        call FINDFIL(LCTLF,XST)
        CALL EZCTLR(ICTLF,0,IUOUT,IER)
        if(markdown)then
          call edisp(itu,'## Controls')
          call edisp(itu,' ')
        else
          WRITE(outs,240)
          call edisp(itu,outs)
          call edisp(itu,' ')
        endif
        if(fileinfo.eq.1)then
          write(outs,'(3A)') 'Ideal control is defined in: ',
     &      LCTLF(1:lnblnk(LCTLF)),' as follows:'
          call edisp(itu,outs)
        endif
        if(markdown)then

C Put all documentation together.
          call edisp(itu,'The model includes ideal controls - ')
          call edisp248(itu,ctldoc,120)
          call edisp248(itu,znctldoc,120)
        else
          call edisp(itu,
     &      'The model includes ideal controls as follows:')
          call edisp(itu,'Control description: ')
          call edisp248(itu,ctldoc,80)
          call edisp248(itu,znctldoc,80)
        endif

        if(NCF.gt.0)then

C If there are zone controls list the additional documentation if the
C toggle controlinfo is compact (1) if verbose (2) list only the controls
C that have been referenced and if very verbose (3) list all controls.
          call edisp(itu,' ')
          WRITE(outs,'(a,I2,a)')'Zones control includes ',
     &      NCF,' functions.'
          if(markdown)then
            call edisp2tr(itu,outs)
          else
            call edisp(itu,outs)
          endif

C List the day types (these can be used by all of the controls which
C are not bounded by period of validity.
          call edisp(itu,' ')
          if(markdown)then
            write(outscal,'(a,20a)') 
     &        'The following day types are defined ',
     &         (CALENTAG(idty),idty=1,NBDAYTYPE)
            call edisp2tr(itu,outscal)
          else
            write(outscal,'(a,20a)') 
     &        'The following day types are defined: ',
     &         (CALENTAG(idty),idty=1,NBDAYTYPE)
            call edisp248(itu,outscal,100)
          endif

          if(controlinfo.eq.2)then
            if(markdown)then
              call edispxtr(itu,' ')
              call edispxtr(itu,
     &        'Details of control loops referenced in the model  ')
            else
              call edispxtr(itu,' ')
              call edisp(itu,
     &        'Details of control loops referenced in the model:')
            endif
            do ii=1,NCF
              foundit=.false.
              do jj=1,ncomp
                if(icascf(jj).eq.ii) foundit=.true.
              enddo
              if(foundit)then
                write(outs,'(2a)') ' loop: ',
     &            BCTLNAME(ii)(1:lnblnk(BCTLNAME(ii)))
                if(markdown)then
                  call edisp2tr(itu,outs)
                else
                  call edisp(itu,outs)
                endif
                call LSTCNTL(itu,0,ii)
              endif
            enddo
            call LSTCNTL(itu,0,0)
          endif
          if(controlinfo.eq.3)then
            if(markdown)then
              call edispxtr(itu,
     &          'Details of all defined control loops  ')
            else
              call edisp(itu,
     &          'Details of all defined control loops:')
            endif
            do ii=1,NCF
              call LSTCNTL(itu,0,ii)
            enddo
            call LSTCNTL(itu,0,0)
          endif
        endif

        if(NCL.gt.0)then
          call edisp(itu,' ')
          WRITE(outs,'(a,I2,a)')' Plant control includes ',
     &      NCL,' loops.'
          call edisp(itu,outs)
          call edisp248(itu,plctldoc,80)
          if(controlinfo.ge.2)then
            do ii=1,NCL
              call LSTCNTL(itu,1,ii)
            enddo
          endif
        endif

        if(NCC.gt.0)then
          call edisp(itu,' ')
          WRITE(outs,'(a,I2,a)')' Flow control includes ',
     &      NCC,' loops.'
          call edisp(itu,outs)
          call edisp248(itu,flctldoc,80)
          if(controlinfo.ge.2)then
            do ii=1,NCC
              call LSTCNTL(itu,2,ii)
            enddo
          endif
        endif

        if(nCFCctlloops.gt.0)then
          call edisp(itu,' ')
          WRITE(outs,'(a,I2,a)')' CFC control includes ',
     &      nCFCctlloops,' functions.'
          call edisp(itu,outs)
          call edisp248(itu,CFCctldoc,80)
          if(controlinfo.ge.2)then
            do ii=1,nCFCctlloops
              call LSTCNTL(itu,6,ii)
            enddo
          endif
        endif

        if(NGF.gt.0)then
          call edisp(itu,' ')
          WRITE(outs,'(a,I2,a)')' Global control includes ',
     &      NGF,' loops.'
          call edisp(itu,outs)
          call edisp248(itu,glctldoc,80)
          if(controlinfo.ge.2)then
            do ii=1,NGF
              call LSTCNTL(itu,3,ii)
            enddo
          endif
        endif

        if(NOF.gt.0)then
          call edisp(itu,' ')
          WRITE(outs,'(a,I2,a)')' Optical control includes ',
     &      NOF,' loops.'
          call edisp(itu,outs)
          call edisp248(itu,opticdoc,80)
          if(controlinfo.ge.2)then
            do ii=1,NOF
              call LSTCNTL(itu,5,ii)
            enddo
          endif
        endif
      endif

C If there is a flow network, rescan and list its contents.
      if(netinfo.gt.0)then
        if(IAIRN.ge.1)then
          if(markdown)then
            call edisp(itu,' ')
          else
            WRITE(outs,240)
            call edisp(itu,outs)
            call edisp(itu,' ')
            call edisp(itu,'The model includes an air flow network.')
          endif
          if(netinfo.eq.1)then
            call mflist(itu,'s')
          elseif(netinfo.eq.2)then
            call mflist(itu,'f')
          endif
          call edisp(itu,' ')
        endif
      endif

C If there is a plant network, scan it and report.
      if(plantinfo.gt.0)then
        if(LPNF(1:4).eq.'UNKN'.or.LPNF(1:2).eq.'  ')then
          continue
        else
          XST=.false.
          call FINDFIL(LPNF,XST)
          if(xst) then
            IFPNF=IFIL+3
            iunit1=IUF
            write(currentfile,'(a)') LPNF(1:lnblnk(LPNF))
            CALL EFOPSEQ(ifpnf,LPNF,1,IER)
            IF(IER.LT.0)THEN
              IER=1
              CALL edisp(iuout,' problem opening plant network file...')
            ENDIF
            WRITE(outs,240)
            call edisp(itu,outs) ! write separation line
            call edisp(itu,' ')
            call edisp(itu,'The model includes a plant network.')
            call pltcfg(ifpnf,iunit1,itu,1)
C copy number of nodes in each plant component from node_count to
C nnodes: This is necessary because the nnodes common block (pcdat)
C is defined differently in bps, and the pltcfg.F file cannot be
C built as part of bps if it includes the prj pcdat common.
            call CopyPltNodeCounts()
          endif
C Is a boundary data file (ESRU flavor) ...
          if (BPF(1:4).eq.'UNKN'.or.BPF(1:2).eq.'  ') then
            continue
          else
            xst=.false.
            call FINDFIL(BPF,xst)
            if (xst) then
              IFBPF=IFIL+1 ! Standard +9 seems to be used for contents in prj.
              write(currentfile,'(a)') BPF(1:lnblnk(BPF))
              CALL EFOPSEQ(ifbpf,BPF,1,IER)
              IF(IER.LT.0)THEN
                IER=1
                CALL edisp(iuout,' problem opening bcd file...')
              ENDIF
              call edisp(itu,' ')
              call edisp(itu,'The plant includes a boundary data file.')
              WRITE(outs248,'(2A)')' BDFile: ',BPF(1:lnblnk(BPF))
              call edisp248(itu,outs248,100)
              call edisp(itu,' ') ! blank line
            endif
          endif
C ... or is a boundary data file (CETC flavor) being used?
          if (bBC_data_defined) then
            xst=.false.
            call FINDFIL(cBC_data_file_name,xst)
            if (xst) then
              IFBPF=IFIL+1
              write(currentfile,'(a)')
     &               cBC_data_file_name(1:lnblnk(cBC_data_file_name))
              CALL EFOPSEQ(ifbpf,cBC_data_file_name,1,IER)
C             Fill BC_data variables ...
cx << either copy BC_data.F to eprj or make simplified processing function
cx    only for qa report ... >>
cx              call process_BC_data_file(IFIL,autook,bBC_data_err)
              IF(IER.lt.0)THEN
                IER=1
                CALL edisp(iuout,' problem opening BC_data file...')
              ENDIF
              call edisp(itu,' ')
              call edisp(itu,'The plant includes a BC_data file.')
C             Give some information on file
cx << doesn't currently work because variables are not initialised ... >>
              WRITE(outs248,'(3A,f4.2)')' BDFile: ',
     &                 cBC_data_file_name(1:lnblnk(cBC_data_file_name)),
     &                 ', version ',fBC_file_version
              call edisp248(itu,outs248,100)
              WRITE(outs248,'(A,I3)')' Number of data columns: ',
     &                   iBC_col_count
              call edisp248(itu,outs248,100)
              WRITE(outs248,'(A,I3,A,I4,A)')' Number of data rows: ',
     &                   iBC_row_count,' @',NINT(fBC_frequency),
     &                   ' seconds/sample'
              call edisp248(itu,outs248,100)
              call edisp(itu,' ') ! blank line
            endif
          endif
        endif
      endif

C If there is an electrical network, scan it and report.
      if (enetinfo.gt.0) then
        call edisp(itu,' ')
        WRITE(outs,240)
        call edisp(itu,outs) ! write separation line
        WRITE(outs248,'(2A)')' Electrical network file: ',
     &      ENTFLNAM(1:lnblnk(ENTFLNAM))
        call edisp(itu,' ') ! blank line
        call edisp248(itu,outs248,100)
cx        call edisp(itu,' ') ! blank line

        istoreIUOUT=IUOUT
        IUOUT=itu
        CALL ENETREAD('Q')
        IUOUT=istoreIUOUT

      endif ! Electrical network info

C Jump if no zones.
      if(inpic.eq.0) goto 57

      call edisp(itu,' ')
      if(markdown)then
        call edisp(itu,'## Zones') 
        call edisp(itu,' ')
        call edisp(itu,': Zone summary')
        call edisp(itu,' ')
        if(zonecolumns.eq.3)then   ! All columns
          write(outs,'(3a)') 
     &    'ID  Zone name     Volume (m^3^)  Surfaces   ',
     &    'Opaque (m^2^)    Transparent (m^2^)   Floor (m^2^) ',
     &    '   Description'
          call edisp(itu,outs)
          write(outs2,'(3a)')
     &    '--  ------------  -------------  --------   ',
     &    '--------------   ------------------   -------------',
     &    '   -------------------------------------------' 
          call edisp(itu,outs2)
        elseif(zonecolumns.eq.2)then  ! Subset B
          write(outs,'(3a)') 
     &    'ID  Zone name     Volume (m^3^)  Surfaces  ',
     &    'Opaque (m^2^)  Transparent (m^2^)   ',
     &    'Description'
          call edisp(itu,outs)
          write(outs2,'(3a)')
     &    '--  ------------  -------------  --------  ',
     &    '-------------  ------------------   ',
     &    '-------------------------------------------' 
          call edisp(itu,outs2)
        elseif(zonecolumns.eq.1)then  ! Subset A
          write(outs,'(3a)') 
     &    'ID  Zone name     Surfaces  ',
     &    'Opaque (m^2^)   Transparent (m^2^)  Floor (m^2^) ',
     &    '  Description'
          call edisp(itu,outs)
          write(outs2,'(3a)')
     &    '--  ------------  --------  ',
     &    '--------------  ------------------  -------------',
     &    '  -------------------------------------------' 
          call edisp(itu,outs2)
        elseif(zonecolumns.eq.0)then  ! Summary
          write(outs,'(2a)') 
     &    'ID  Zone name     Volume (m^3^)  ',
     &    'Floor (m^2^)  Description'
          call edisp(itu,outs)
          write(outs2,'(3a)')
     &    '--  ------------  -------------  ',
     &    '------------  -------------------------------------------' 
          call edisp(itu,outs2)
        endif
      else
        WRITE(outs,240)
        call edisp(itu,outs)
        call edisp(itu,' ')
        if(zonecolumns.eq.3)then
          outs='ID Zone         Volume|          Surface          '
          call edisp(itu,outs)
          outs='   Name         m^3   | No. Opaque  Transp  ~Floor'
          call edisp(itu,outs)
        elseif(zonecolumns.eq.2)then
          outs='ID Zone         Volume|      Surface        Description'
          call edisp(itu,outs)
          outs='   Name         m^3   | No. Opaque  Transp        '
          call edisp(itu,outs)
        elseif(zonecolumns.eq.1)then
          outs='ID Zone         |       Surface      Description'
          call edisp(itu,outs)
          outs='   Name         | No. Opaque  Transp        '
          call edisp(itu,outs)
        elseif(zonecolumns.eq.0)then
          outs='ID Zone         Volume| ~Floor  Description       '
          call edisp(itu,outs)
          outs='   Name         m^3   |  area                     '
          call edisp(itu,outs)
        endif
      endif
      TVOL=0.
      TFLA=0.
      TOPQ=0.
      TTRN=0.
      NASURF=0
      do 97 IZ=1,INPIC
        if(IVALS(IZ).GT.0)then
          LTMP=LGEOM(IVALS(IZ))
          call georead(IUF,LTMP,IVALS(IZ),1,iuout,IER)
          OPQ=0.
          TRN=0.
          attribok(IVALS(IZ))=.true.
          DO 41 I=1,NZSUR(ivals(iz))
            icc=izstocn(ivals(iz),i)
            if(SOTHER(ivals(iz),i,1)(1:4).eq.'UNKN')
     &         attribok(IVALS(IZ))=.false.

C At this point CFC and CFC2 are not counted
            if(SMLCN(ivals(iz),i)(1:4).eq.'UNKN')
     &        attribok(IVALS(IZ))=.false.
            if(SOTF(ivals(iz),i)(1:4).NE.'OPAQ') then
              TRN=TRN+SNA(ivals(iz),i)
            else
              OPQ=OPQ+SNA(ivals(iz),i)
            endif
   41     CONTINUE
          if(attribok(IVALS(IZ)))then
            aok=' attributed  '
            if(markdown)then
              if(zonecolumns.eq.3)then
                write(outs248,
     &            '(i2,2x,a,f8.1,i13,11x,f8.1,13x,f8.1,8x,f9.1,5x,a)') 
     &            IVALS(IZ),
     &            zname(IVALS(IZ)),VOL(IVALS(IZ)),NZSUR(ivals(iz)),
     &            OPQ,TRN,ZBASEA(IVALS(IZ)),
     &            zdesc(IVALS(IZ))(1:lnzdesc(IVALS(IZ)))
                call edisp(itu,outs248)
C              call edisp248(itu,outs248,140)
              elseif(zonecolumns.eq.2)then
                write(outs248,
     &            '(i2,2x,a,f8.1,i13,11x,f8.1,12x,f8.1,3x,a)') 
     &            IVALS(IZ),
     &            zname(IVALS(IZ)),VOL(IVALS(IZ)),NZSUR(ivals(iz)),
     &            OPQ,TRN,zdesc(IVALS(IZ))(1:lnzdesc(IVALS(IZ)))
                call edisp(itu,outs248)
              elseif(zonecolumns.eq.1)then
                write(outs248,
     &            '(i2,2x,a,i10,8x,f8.1,12x,f8.1,6x,f9.1,2x,a)') 
     &            IVALS(IZ),zname(IVALS(IZ)),NZSUR(ivals(iz)),
     &            OPQ,TRN,ZBASEA(IVALS(IZ)),
     &            zdesc(IVALS(IZ))(1:lnzdesc(IVALS(IZ)))
                call edisp(itu,outs248)
              elseif(zonecolumns.eq.0)then
                write(outs248,
     &            '(i2,2x,a,f8.1,8x,f9.1,6x,a)') IVALS(IZ),
     &            zname(IVALS(IZ)),VOL(IVALS(IZ)),ZBASEA(IVALS(IZ)),
     &            zdesc(IVALS(IZ))(1:lnzdesc(IVALS(IZ)))
                call edisp(itu,outs248)
              endif
            else
              if(zonecolumns.eq.3)then
                write(outs,'(i2,1x,a,f7.1,i4,3f8.1,2x,a)') IVALS(IZ),
     &            zname(IVALS(IZ)),VOL(IVALS(IZ)),NZSUR(ivals(iz)),
     &            OPQ,TRN,ZBASEA(IVALS(IZ)),
     &            zdesc(IVALS(IZ))(1:lnzdesc(IVALS(IZ)))
                call edisp(itu,outs)
              elseif(zonecolumns.eq.2)then
                write(outs,'(i2,1x,a,f7.1,i4,2f8.1,2x,a)') IVALS(IZ),
     &            zname(IVALS(IZ)),VOL(IVALS(IZ)),NZSUR(ivals(iz)),
     &            OPQ,TRN,zdesc(IVALS(IZ))(1:lnzdesc(IVALS(IZ)))
                call edisp(itu,outs)
              elseif(zonecolumns.eq.1)then
                write(outs,'(i2,1x,a,i4,2f8.1,6x,f9.1,2x,a)')IVALS(IZ),
     &            zname(IVALS(IZ)),NZSUR(ivals(iz)),
     &            OPQ,TRN,ZBASEA(IVALS(IZ)),
     &            zdesc(IVALS(IZ))(1:lnzdesc(IVALS(IZ)))
                call edisp(itu,outs)
              elseif(zonecolumns.eq.0)then
                write(outs,'(i2,1x,a,f7.1,f8.1,2x,a)') IVALS(IZ),
     &            zname(IVALS(IZ)),VOL(IVALS(IZ)),ZBASEA(IVALS(IZ)),
     &            zdesc(IVALS(IZ))(1:lnzdesc(IVALS(IZ)))
                call edisp(itu,outs)
              endif
            endif
          else
            aok=' part-attributed '
            if(markdown)then
              write(outs248,
     &          '(i2,2x,a,f8.1,i11,11x,f8.1,13x,f8.1,12x,f8.1,a)')
     &          IVALS(IZ),
     &          zname(IVALS(IZ)),VOL(ivals(iz)),NZSUR(ivals(iz)),
     &          OPQ,TRN,ZBASEA(IVALS(IZ)),aok
              call edisp248(itu,outs248,140)
            else
              write(outs,'(i2,1x,a,f7.1,i4,3f8.1,a)') IVALS(IZ),
     &          zname(IVALS(IZ)),VOL(ivals(iz)),NZSUR(ivals(iz)),
     &          OPQ,TRN,ZBASEA(IVALS(IZ)),aok
              call edisp(itu,outs)
            endif
          endif
          NASURF=NASURF+NZSUR(ivals(iz))
          TVOL=TVOL+VOL(IVALS(IZ))
          TFLA=TFLA+ZBASEA(IVALS(IZ))
          TOPQ=TOPQ+OPQ
          TTRN=TTRN+TRN
        endif
  97  continue
      if(markdown)then
        if(zonecolumns.eq.3)then
          write(outs,'(a,f9.0,i14,11x,f8.1,13x,f8.1,9x,f8.1,6x,a)')
     &    ' -  all       ',TVOL,NCON,TOPQ,TTRN,TFLA,'       -'
        elseif(zonecolumns.eq.2)then
          write(outs,'(a,f9.0,i14,11x,f8.1,13x,f8.1,6x,a)')
     &    ' -  all       ',TVOL,NCON,TOPQ,TTRN,'       -'
        elseif(zonecolumns.eq.1)then
          write(outs,'(a,i12,8x,f8.1,12x,f8.1,7x,f8.1,6x,a)')
     &    ' -  all       ',NASURF,TOPQ,TTRN,TFLA,'       -'
        elseif(zonecolumns.eq.0)then
          write(outs,'(a,f9.0,9x,f8.1,6x,a)')
     &    ' -  all       ',TVOL,TFLA,'       -'
        endif
      else
        if(zonecolumns.eq.3)then
          write(outs,'(a,f9.0,i5,3f8.0)') '   all      ',TVOL,NCON,
     &    TOPQ,TTRN,TFLA
        elseif(zonecolumns.eq.2)then
          write(outs,'(a,f9.0,i14,11x,f8.1,13x,f8.1,6x,a)')
     &    ' -  all       ',TVOL,NCON,TOPQ,TTRN,'       -'
        elseif(zonecolumns.eq.1)then
          write(outs,'(a,i12,8x,f8.1,12x,f8.1,9x,f8.1,6x,a)')
     &    ' -  all       ',NASURF,TOPQ,TTRN,TFLA,'       -'
        elseif(zonecolumns.eq.0)then
          write(outs,'(a,f9.0,9x,f8.1,6x,a)')
     &    ' -  all       ',TVOL,TFLA,'       -'
        endif
      endif
      call edisp(itu,outs)

C For each selected zone produce reports.
C Zero project code complence data. 
      if(zoneextrainfo.gt.0)then

C Clear derived values (variables in derived.h).
        call findua(IVALS,inpic,'c')

C Process derived values (in derived.h).
        call findua(IVALS,inpic,'p')

        xtranper = 0.0
        xskyper = 0.0
        xwallper = 0.0
        xslproofper = 0.0
        xflatroofper = 0.0
      endif

C Jump if no zones or no zone info requested.
      if(inpic.eq.0) goto 57
      if(geominfo.eq.0) goto 57

      do 98 IZ=1,INPIC
        IZZ=IVALS(IZ)
        if(IZZ.eq.0)goto 98
        LTMP=LGEOM(IZZ)
        call georead(IUF,LTMP,IZZ,1,itu,IER)

        if(markdown)then
          call edisp(itu,'  ')
          write(outs,'(a,i2,2a)') '### Zone',iz,': ',
     &      zname(iz)(1:lnblnk(zname(iz)))
          call edisp(itu,outs)
        else
          call edisp(itu,'  ')
          WRITE(outs,240)
          if(iz.gt.1)call edisp(itu,outs)
        endif
 240    format(1x,60('_'))

        call ZINFOREP(itu,IZZ)  ! not dependant on georead.

        do 111 ij=1,nzsur(izz)
          icc=izstocn(izz,ij)
          CALL SURADJ(IZZ,IJ,IE,TMP,IZC,ISC,IC,DESCRC) ! not dependant on georead

C Check for duplicate surface names in this zone. Report is
C duplicates are found.
          write(SN,'(a12)')SNAME(izz,ij)
          call snamdup(SN,izz,ij,nameok)
          if(nameok)then
            continue
          else
            write(outs,'(3a)') 'Surface ',SN(1:lnblnk(SN)),
     &        ' has a duplicate name. Names must be unique!'
            call edisp(itu,outs)
          endif
 111    continue

C Write out UA information if extra information requested.
        if(zoneextrainfo.gt.1)then

C Debug...
C          write(itu,*) 'in zone ',izz,' area opq to ambient = ',
C     &                 areawall(IZZ)
C          write(itu,*) 'area flat roof = ',areafltroof(IZZ)
C          write(itu,*) 'area sloped roof = ',areaslproof(IZZ)
C          write(itu,*) 'area skylights = ',areaskylt(IZZ)
C          write(itu,*) 'area facade windows = ',areatran(IZZ)
C          write(itu,*) 'area inside windows = ',areainttran(IZZ)
C          write(itu,*) 'area inside opq wall floor ceil = ',
C     &                  areaintopaq(IZZ)
C          write(itu,*) 'floor area = ',ZBASEA(IZZ)
C          write(itu,*) 'to ground = ',areaground(IZZ)
C          write(itu,*) 'exposed = ',exposed(IZZ)
C          write(itu,*) 'uavgtran = ',uavgtran(IZZ)
C          write(itu,*) 'uavwall = ',uavwall(IZZ)
C          write(itu,*) 'uavslproof = ',uavslproof(IZZ)
C          write(itu,*) 'uavfltroof = ',uavfltroof(IZZ)
C          write(itu,*) 'uavgsky = ',uavgsky(IZZ)
C          uavtotal(IZZ) = uavgtran(IZZ) + uavwall(IZZ) + 
C     &         uavslproof(IZZ) + uavfltroof(IZZ) + uavgsky(IZZ)
C          write(itu,*) 'UA total = ',uavtotal(IZZ)
C          write(itu,*) 'TB heat loss = ',totheatloss(IZZ)

          if(exposed(IZZ).gt.0.1)then

C If there is external glazing or skylights then report.
            call rel16str(exposed(IZZ),t16a,lna,ier)
            call rel16str(vexposed(IZZ),t16b,lnb,ier)
            call rel16str(areawall(IZZ),t16c,lnc,ier)
            if(vexposed(IZZ).gt.0.1)then
              if(markdown)then
                write(outs,'(5a)')'There is ',
     &          t16a(1:lna),' m^2^ of exposed surface area, ',
     &          t16b(1:lnb),' m^2^ of which is vertical. '
              else
                write(outs,'(5a)')' There is ',
     &          t16a(1:lna),'m2 of exposed surface area, ',
     &          t16b(1:lnb),'m2 of which is vertical.'
              endif
            else
              if(markdown)then
                write(outs,'(3a)')'There is ',
     &          t16a(1:lna),' m^2^ of exposed surface area. '
              else
                write(outs,'(3a)')' There is ',
     &          t16a(1:lna),'m2 of exposed surface area.'
              endif
            endif
            if(attribok(IVALS(IZ)))then
              if(markdown)then
                call edisp(itu,outs)
              else
                call edisp(itu,outs)
              endif
            endif
          endif
          if(areawall(IZZ).gt.0.1)then
            if(ZBASEA(IZZ).gt.0.01)then
              wallper(IZZ) = (areawall(IZZ)/ZBASEA(IZZ)) * 100.
            else
              wallper(IZZ) = 1.0
            endif
            call rel16str(wallper(IZZ),t16a,lna,ier)
            call rel16str(uavwall(IZZ),t16b,lnb,ier)
            if(markdown)then
              write(outs,'(3a,F6.1,a,F5.3,3a)')'Facade opaque is ',
     &          t16c(1:lnc),' m^2^ & ',wallper(IZZ),
     &          ' % of floor area & average U of ',
     &          uavwall(IZZ)/areawall(IZZ),' & UA of ',t16b(1:lnb),'. '
            else
              write(outs,'(3a,F6.1,a,F5.3,3a)')' Facade opaque is ',
     &          t16c(1:lnc),'m2 & ',wallper(IZZ),
     &          ' % of floor area & average U of ',
     &          uavwall(IZZ)/areawall(IZZ),' & UA of ',t16b(1:lnb),'. '
            endif
            if(attribok(IVALS(IZ)))then
              if(markdown)then
                call edisp(itu,outs)
              else
                call edisp(itu,outs)
              endif
            endif
          endif
          if(areaslproof(IZZ).gt.0.1)then
            if(ZBASEA(IZZ).gt.0.01)then
              slproofper(IZZ) = (areaslproof(IZZ)/ZBASEA(IZZ))*100.
            else
              slproofper(IZZ) = 1.0
            endif
            call rel16str(slproofper(IZZ),t16a,lna,ier)
            call rel16str(uavslproof(IZZ),t16b,lnb,ier)
            if(markdown)then
              write(outs,'(3a,F5.3,3a)')'Sloped roof is ',
     &        t16a(1:lna),' % of floor area & average U of ',
     &        uavslproof(IZZ)/areaslproof(IZZ),' & UA of ',
     &        t16b(1:lnb),'. '
            else
              write(outs,'(3a,F5.3,2a)')' Sloped roof is ',
     &        t16a(1:lna),' % of floor area & average U of ',
     &        uavslproof(IZZ)/areaslproof(IZZ),' & UA of ',
     &        t16b(1:lnb)
            endif
            if(attribok(IVALS(IZ)))then
              if(markdown)then
                call edisp(itu,outs)
              else
                call edisp(itu,outs)
              endif
            endif
          endif
          if(areafltroof(IZZ).gt.0.1)then
            if(ZBASEA(IZZ).gt.0.01)then
              flatroofper(IZZ) = (areafltroof(IZZ)/ZBASEA(IZZ))*100.
            else
              flatroofper(IZZ) = 1.0
            endif
            call rel16str(flatroofper(IZZ),t16a,lna,ier)
            call rel16str(uavfltroof(IZZ),t16b,lnb,ier)
            write(outs,'(3a,F5.3,3a)')'Flat roof is ',
     &        t16a(1:lna),' % of floor area & average U of ',
     &        uavfltroof(IZZ)/areafltroof(IZZ),' & UA of ',
     &        t16b(1:lnb),'. '
            if(attribok(IVALS(IZ)))then
              if(markdown)then
C                call edisp2tr(itu,outs)
                call edisp(itu,outs)
              else
                call edisp(itu,outs)
              endif
            endif
          endif
          if(areatran(IZZ).gt.0.1)then
            if(ZBASEA(IZZ).gt.0.01)then
              tranper(IZZ) = (areatran(IZZ)/ZBASEA(IZZ)) * 100.
            else
              tranper(IZZ) = 1.0
            endif
            vtranper = (areatran(IZZ)/vexposed(IZZ)) * 100.
            call rel16str(tranper(IZZ),t16a,lna,ier)
            call rel16str(vtranper,t16b,lnb,ier)
            call rel16str(uavgtran(IZZ),t16c,lnc,ier)
            call rel16str(areatran(IZZ),t16d,lnd,ier)
            if(markdown)then
              write(outs,'(3a,F5.1,a,F5.1,a,F5.3,3a)')
     &        'Facade glazing is ',t16d(1:lnd),' m^2^ & ',tranper(IZZ),
     &        ' % of floor & ',vtranper,
     &        ' % facade with average U of ',
     &        uavgtran(IZZ)/areatran(IZZ),' & UA of ',t16c(1:lnc),'. '
            else
              write(outs,'(3a,F5.1,a,F5.1,a,F5.3,2a)')
     &        ' Facade glazing is ',t16d(1:lnd),'m2 & ',tranper(IZZ),
     &        ' % of floor & ',vtranper,
     &        ' % facade with average U of ',
     &        uavgtran(IZZ)/areatran(IZZ),' & UA of ',t16c(1:lnc)
            endif
            if(attribok(IVALS(IZ)))then
              if(markdown)then
                call edisp2tr(itu,outs)
              else
                call edisp(itu,outs)
              endif
            endif
          endif
          if(areaskylt(IZZ).gt.0.1)then
            if(ZBASEA(IZZ).gt.0.01)then
              skyper(IZZ) = (areaskylt(IZZ)/ZBASEA(IZZ)) * 100.
            else
              skyper(IZZ) = 1.0
            endif
            call rel16str(skyper(IZZ),t16a,lna,ier)
            call rel16str(uavgsky(IZZ),t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')'Skylights are ',
     &        t16a(1:lna),' % of floor area & average U of ',
     &        uavgsky(IZZ)/areaskylt(IZZ),' & UA of ',t16b(1:lnb)
            if(attribok(IVALS(IZ)))then
              if(markdown)then
                call edisp2tr(itu,outs)
              else
                call edisp(itu,outs)
              endif
            endif
          endif
          if(areainttran(IZZ).gt.0.1.or.areaintopaq(IZZ).gt.0.1)then
            call rel16str(areainttran(IZZ),t16a,lna,ier)
            call rel16str(areaintopaq(IZZ),t16b,lnb,ier)
            if(markdown)then
              if(areainttran(IZZ).gt.0.1.and.
     &           areaintopaq(IZZ).gt.0.1)then
                write(outs,'(5a)')'Opaque partitions:ceiling:floor ',
     &            t16b(1:lnb),
     &            ' m^2^ & trasparent partitions:ceiling:floor ',
     &            t16a(1:lna),' m^2^. '
              elseif(areainttran(IZZ).lt.0.1.and.
     &               areaintopaq(IZZ).gt.0.1)then
                write(outs,'(3a)')'Opaque partitions:ceiling:floor ',
     &            t16b(1:lnb),' m^2^. '
              endif
            else
              if(areainttran(IZZ).gt.0.1.and.
     &           areaintopaq(IZZ).gt.0.1)then
               write(outs,'(5a)')' Opaque partitions:ceiling:floor ',
     &         t16b(1:lnb),'m2 & trasparent partitions:ceiling:floor ',
     &         t16a(1:lna),'m2. '
              elseif(areainttran(IZZ).lt.0.1.and.
     &               areaintopaq(IZZ).gt.0.1)then
               write(outs,'(3a)')' Opaque partitions:ceiling:floor ',
     &         t16b(1:lnb),'m2.'
              endif
            endif
            if(attribok(IVALS(IZ)))then
              if(markdown)then
                call edisp2tr(itu,outs)
              else
                call edisp(itu,outs)
              endif
            endif
          endif
          if(areaground(IZZ).gt.0.1)then
            if(ZBASEA(IZZ).gt.0.01)then
              grper = (areaground(IZZ)/ZBASEA(IZZ)) * 100.
            else
              grper = 1.0
            endif
            call rel16str(grper,t16a,lna,ier)
            plen=bridgelen(IZZ,2)+bridgelen(IZZ,10)  ! tb length std and metal facade
            call rel16str(plen,t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')'Ground contact is ',
     &        t16a(1:lna),' % of floor area & average U of ',
     &        uavground(IZZ)/areaground(IZZ),' & perimeter ',
     &        t16b(1:lnb)
            if(attribok(IVALS(IZ)))then
              if(markdown)then
                call edisp2tr(itu,outs)
              else
                call edisp(itu,outs)
              endif
            endif
          endif
        endif

C Note: surinfo not dependant on recent geometry scan.
        if(geominfo.ge.2)then
          context=.true.
          CALL SURINFO(IVALS(IZ),itu,context)

C Bounding box for frames & doors here.
          if(markdown)then
            call edisp(itu,
     &     'The width & height & height above the floor for surfaces ')
            call edisp(itu,
     &     'marked as frames or doors or glazing is shown below.')
            call edisp(itu,'  ')
            call edisp(itu,': A summary of frames')
            call edisp(itu,'  ')
            write(outs,'(a)')
     &      'Surface      Width(m)   Height(m)  Above ff  USE    '
            call edisp(itu,outs)
            write(outs,'(a)')
     &      '------------ ---------  ---------  --------- -------'
            call edisp(itu,outs)
          else
            call edisp(itu,
     &      'Surface      Width(m)   Height(m)  Above ffl...')
          endif
          do isz = 1,NZSUR(izz)
            if(SUSE(izz,isz,1)(1:7).eq.'F-FRAME'.or.
     &         SUSE(izz,isz,1)(1:5).eq.'FRAME'.or.
     &         SUSE(izz,isz,1)(1:4).eq.'DOOR'.or.
     &         SUSE(izz,isz,1)(1:6).eq.'P-DOOR'.or.
     &         SUSE(izz,isz,1)(1:6).eq.'WINDOW'.or.
     &         SUSE(izz,isz,1)(1:8).eq.'D-WINDOW'.or.
     &         SUSE(izz,isz,1)(1:8).eq.'S-WINDOW'.or.
     &         SUSE(izz,isz,1)(1:8).eq.'C-WINDOW')then
              CALL ZSURLEHI(izz,isz,XYMAX,ZMAX,llpos,lrpos,
     &          ulpos,urpos,dzllff)
              write(outs,'(a,a,f6.2,a,f7.2,f10.2,2a)') 
     &          SNAME(izz,isz),'  ',XYMAX,'    ',ZMAX,dzllff,
     &          '    ',SUSE(izz,isz,1) 
              call edisp(itu,outs)
            endif
          enddo

          call eclose(gversion(IZZ),1.1,0.01,newgeo)
          if(newgeo)then
            if(losspercent(izz).gt.0.0)then
              havebrdge=.true.
            else
              havebrdge=.false.
            endif
            if(nbrdg(izz).gt.0.or.havebrdge)then

C Setup phrases for the menu and for editing length and psi values.
              phrasemenu(1) ='a roof-wall (eave)             :'
              phraselen(1)  ='roof-wall ' 
              phrasemenu(2) ='b wall-ground floor            :'
              phraselen(2)  ='wall-ground floor '
              phrasemenu(3) ='c wall-wall (convex corner)    :'
              phraselen(3)  ='wall-wall (convex corner) '
              phrasemenu(4) ='d wall-wall (concave corner)   :'
              phraselen(4)  ='wall-wall (concave corner) '
              phrasemenu(5) ='e wall-floor (exposed floor)   :'
              phraselen(5)  ='wall-floor (exposed floor) '
              phrasemenu(6) ='f lintel above window or door  :'
              phraselen(6)  ='lintel above window or door '
              phrasemenu(7) ='g Sill below window            :'
              phraselen(7)  ='Sill below window '
              phrasemenu(8) ='h jamb at window or door       :'
              phraselen(8)  ='jamb at window or door '
              phrasemenu(9) ='i wall-gable                   :'
              phraselen(9)  ='wall-gable '
              phrasemenu(10)='j wall-parapet                 :'
              phraselen(10) ='wall-parapet '
              phrasemenu(11)='k wall-intermediate-floor      :'
              phraselen(11) ='wall-intermediate-floor '
              phrasemenu(12)='l wall-partition               :'
              phraselen(12) ='wall-partition '
              phrasemenu(13)='m glass-frame                  :'
              phraselen(13) ='glass-frame '
              phrasemenu(14)='n balcony                      :'
              phraselen(14) ='balcony'
              phrasemenu(15)='o user defined a               :'
              phraselen(15) ='user-defined-a '
              phrasemenu(16)='p user-defined b               :'
              phraselen(16) ='user-defined-b '
              if(markdown)then
                call edisp(itu,'  ')
                call edisp(itu,': Thermal bridges have been defined.')
                call edisp(itu,'  ')
                write(outs,'(a)')
     &          'Junctions                       psi (W/mK)  length (m)'
                call edisp(itu,outs)
                write(outs,'(a)')
     &          '------------------------------  ----------  ----------'
                call edisp(itu,outs)
              else
                call edisp(itu,'Thermal bridges have been defined.')
                call edisp(itu,'Junctions      psi (W/mK) length (m)')
              endif
              do 89 itbmenu=1,16
                if(nbrdg(izz).gt.0)then
                  do itb=1,nbrdg(izz)
                    if(itbmenu.eq.ibrdg(izz,itb))then
                      WRITE(TBRIDGE,'(a,2F11.4)') 
     &                  phrasemenu(itbmenu)(3:32),
     &                  psi(izz,itb),lngth(izz,itb)
                      call edisp(itu,TBRIDGE)
                    endif
                  enddo  ! of itb
                endif
  89          continue
              write(outs,*) 'Sum of psi * length: ',
     &          totheatloss(izz),' W/K'
              call edisp(itu,' ')
              call edisp(itu,outs)
              if(losspercent(izz).gt.0.0) then
                WRITE(TBRIDGE,'(a,F7.4)') 'Themal bridge %',thbrpercent
                call edisp(itu,TBRIDGE)
              endif
            endif
          endif
        endif
        if(geominfo.eq.3)then    ! Very verbose - include coordinates.

C Write vertices with minimal white space comma separated.
C See if an even or odd number of items in list.
          im=MOD(NZTV(izz),2)
          odd=.false.
          if(im.eq.1) odd=.true.
          if(markdown)then
            call edisp(itu,'  ')
            call edisp(itu,': A summary of the vertices')
            call edisp(itu,'  ')
            write(outs,'(2a)')
     &'Vertex X          Y          Z         Vertex ',
     &'X         Y          Z'
            call edisp(itu,outs)
            write(outs,'(2a)')
     &'------ ---------  ---------  --------- ------ ',
     &'--------- ---------  ---------'
            call edisp(itu,outs)
          else
            call edisp(itu,' Vertices ( index X Y Z) in the zone...')
          endif
          if(NZTV(izz).lt.20)then
            DO I = 1,NZTV(izz)
              if(markdown)then
                WRITE(outs,'(i5,3F11.4)') I,X(I),Y(I),Z(I)
              else
                WRITE(outs,'(a,i3,3F10.4)')'*vertex ',I,
     &            X(I),Y(I),Z(I)
              endif
              call edisp(itu,outs)
            enddo  ! of I
          else
            MNULEN=(NZTV(izz)/2)
            DO K=1,MNULEN
              if(markdown)then
                WRITE(outs,'(i5,3f11.4,i5,3f11.4)') k,
     &          X(k),Y(k),Z(k),K+MNULEN,
     &          X(K+MNULEN),Y(K+MNULEN),Z(K+MNULEN)
              else
                WRITE(outs,'(a,i3,3f10.4,a,i3,3f10.4)') 'vertex ',k,
     &          X(k),Y(k),Z(k),'   vertex ',K+MNULEN,
     &          X(K+MNULEN),Y(K+MNULEN),Z(K+MNULEN)
              endif
              call edisp(itu,outs)
            enddo  ! of K

C Put odd vertex in right column.
            if(odd)then
              if(markdown)then
                WRITE(outs,'(38x,i5,3f11.4)') NZTV(izz),
     &          X(NZTV(izz)),Y(NZTV(izz)),Z(NZTV(izz))
              else
                WRITE(outs,'(46x,a,i3,3f10.4)') 'vertex ',NZTV(izz),
     &          X(NZTV(izz)),Y(NZTV(izz)),Z(NZTV(izz))
              endif
              call edisp(itu,outs)
            endif 
          endif

          if(markdown)then
            call edisp(itu,'  ')
            call edisp(itu,': A summary of the edges')
            call edisp(itu,'  ')
            call edisp(itu,
     &'Edge List                              Name          Perimeter')
            call edisp(itu,
     &'---- --------------------------------  ------------- ---------')
          else
            call edisp(itu,
     &' Number of vertices followed by list, name & perimeter length')
          endif

          call getperimeter(izz,sperim)  ! find perimeter of surfaces

          DO I = 1,NZSUR(izz)
            icc=izstocn(izz,i)           ! find connection
            lsna=lnblnk(SNAME(izz,i))    ! get length of surf name
            if(markdown)then
              write(louts,'(102I4)') (JVN(I,J),J=1,NVER(I))
            else
              write(louts,'(a,i3,102I4)') '*edges ',NVER(I),
     &        (JVN(I,J),J=1,NVER(I))
            endif
            call SDELIM(louts,loutsd,'C',IW)  ! make comma separated
            if(markdown)then
              lnlist=lnblnk(loutsd)
              if(lnlist.lt.32)lnlist=32
              if(lnlist.gt.32)lnlist=32
              write(louts600,'(i4,4a,f8.3)') NVER(I),' ',
     &        loutsd(1:lnlist),'   ',SNAME(izz,i),SPERIM(i)
            else
              write(louts600,'(3a,f8.3,a)') 
     &        loutsd(1:lnblnk(loutsd)),'   ',SNAME(izz,i)(1:lsna),
     &        SPERIM(i),'m perimeter'
            endif
            call edisp248(itu,louts600,100)
          enddo  ! of I

C If thermal bridges are associated with a surface then report this.
C Loop each surface in zone, and for each surface each edge and if
C ibridgeshr(connection,edge) is non-zero add to howmanytbedges array.
          if(nbrdg(izz).gt.0)then
            do 971 I = 1,NZSUR(izz)
              icc=izstocn(izz,i)        ! find connection
              lsna=lnblnk(SNAME(izz,i))  ! get length of surf name
              do j=1,16
                howmanytbedges(j)=0
              enddo  ! of j
              haveedgebrdge=.false.
              do j=1,NVER(I)
                if(ibridgeshr(icc,j).ne.0)then
                  howmanytbedges(ibridgeshr(icc,j))=
     &              howmanytbedges(ibridgeshr(icc,j))+1
                  haveedgebrdge=.true.
                endif
              enddo  ! of j

C If at least one thermal bridge for surface report it. Keep writing
C into outs248 for each non-zero bridges.
              if(haveedgebrdge)then
                write(outs248,'(2a)') SNAME(izz,i)(1:lsna),' bridges: '
                lk=lnblnk(outs248)+2
                do k=1,16
                  if(howmanytbedges(k).gt.0)then
                    lnk=lnblnk(phraselen(k))
                    lnkn=lk+lnk+1
                    write(outs248(lk:lnkn),'(2a)') 
     &                phraselen(k)(1:lnk),','
                    lk=lnkn+1
                  endif
                enddo  ! of k
                call edisp248(itu,outs248,100)
              endif
 971        continue
          endif
        endif

C Check if the edges are proprly bounded. Uses G1 common.
        call ckvert(0,IVALS(IZ),bound,iub,inv,'-',ier)
        if(.NOT.bound)then
          iprb=MAX0(iub,inv)
          WRITE(outs,'(A,i3,a)')  ' There are ',iub,
     &      ' edges which are not bounded (zone might be incomplete)'
          if(iub.gt.0)call edisp(itu,outs)
          WRITE(outs,'(A,i3,a)')  ' There are ',inv,
     &      ' surface with reversed edges (one might be reversed)'
          if(inv.gt.0)call edisp(itu,outs)
          if(iprb.gt.4)then
            call edisp(itu,' Please check your zone geometry.')
          endif
        endif

        if(zoneextrainfo.ge.1)then

C Additional information about the zone. Provide a summary of each MLC
C used in the zone. Scan all MLC, if used somewhere fill areamlc array
C and then report instances for current zone.
          if(markdown)then
            call edisp(itu,'  ')
            call edisp(itu,': A summary of the MLC areas m^2^')
            call edisp(itu,'  ')
            write(itu,'(2a)')
     &  'MLC name           total     to ambient to other z back-back',
     &  ' to ground to similar'
            write(itu,'(2a)')
     &  '------------------ --------- ---------- ---------- ---------',
     &  ' --------- ----------'
          else
            write(itu,'(a)') '  '
            write(itu,'(3a)') 'In ',zname(izz)(1:12),
     &      '    m^2     m^2 to   m^2 to  m^2 back  m^2 to  m^2 to'
            write(itu,'(2a)') 'MLC name       ',
     &      '    total  ambient  other z -to-back   ground  similar'
          endif
          do imlc=1,nmlc
            found=.false.
            write(mlc_name,'(a)') mlcname(imlc)(1:lnmlcname(imlc))
            call mlcrefs(mlc_name,areamlc,areamlcamb,areamlcoth,
     &        areamlcb2b,areamlcgrnd,areamlcsimil,tareamlc,found)
            if(found)then
              if(areamlc(izz).gt.0.0)then

C Sum the sub-totals (so partitions fully counted but b-to-b is half.
                tmlc=areamlcamb(izz)+areamlcoth(izz)+areamlcb2b(izz)+
     &            areamlcgrnd(izz)+areamlcsimil(izz)
                if(markdown)then
                  write(itu,'(a,f10.1,f10.1,f9.1,f9.1,f10.1,f10.1)') 
     &              mlc_name(1:18),tmlc,
     &              areamlcamb(izz),areamlcoth(izz),areamlcb2b(izz),
     &              areamlcgrnd(izz),areamlcsimil(izz)
                else
                  write(itu,'(2a,6f9.1)') mlc_name(1:18),' ',tmlc,
     &              areamlcamb(izz),areamlcoth(izz),areamlcb2b(izz),
     &              areamlcgrnd(izz),areamlcsimil(izz)
                endif
              endif
            endif
          enddo

C Also display insolation directives.
          CALL INSINFO(IVALS(IZ),itu)

C Note any optical controls. First read zone constructions and
C then parse TMC file and if there are optical controls then list.
          XST=.false.
          call FINDFIL(LTHRM(IVALS(IZ)),XST)
          if(XST)then
            CALL ECONST(LTHRM(IVALS(IZ)),IUF,IVALS(IZ),0,IUOUT,IER)
          endif
          if(ITW(IVALS(IZ)).eq.1)then
            CALL ERTWIN(0,IUOUT,IUF,LTWIN(IVALS(IZ)),IVALS(IZ),IER)
            call edisp(itu,' ')
            DO 2020 I=1,NTMC
              if(IBCMT(IVALS(IZ),I).eq.0)then
                continue
              elseif(IBCMT(IVALS(IZ),I).lt.0)then
                write(outs,'(3a,i2,a)')' Surfaces with optic ',
     &            TOPTIC(IVALS(IZ),I)(1:lto),
     &            ' link to optical control ',
     &            (-1*IBCMT(IVALS(IZ),I)),'.'
                call edisp(itu,outs)
              else
                lto=lnblnk(TOPTIC(IVALS(IZ),I))
                if(IBCSUR(IVALS(IZ),I).eq.0)then
                  write(outs,'(4a,i2,a)')' Surfaces with optic ',
     &              TOPTIC(IVALS(IZ),I)(1:lto),' individually sensed ',
     &              'over ',NBCTMC(IVALS(IZ),I),' control periods.'
                  call edisp(itu,outs)
                else
                  write(outs,'(3a,i2,a,i2,a)')' Surfaces with optic ',
     &              TOPTIC(IVALS(IZ),I)(1:lto),' sense surface ',
     &              IBCSUR(IVALS(IZ),I),' over ',NBCTMC(IVALS(IZ),I),
     &              ' control periods.'
                  call edisp(itu,outs)
                endif

C Remind user of standard optics as well as the alternatives.
                write(outs,'(a,5f6.3)') ' Standard optic solar trans:',
     &            (TMCT(IVALS(IZ),I,M),M=1,5)
                call edisp(itu,outs)
                DO KK=1,NBCTMC(IVALS(IZ),I)
                  if(NBCTT(IVALS(IZ),I).eq.0)then
                    msg=' sensing total radiation'
                  elseif(NBCTT(IVALS(IZ),I).eq.1)then
                    msg=' sensing ambient temperature'
                  elseif(NBCTT(IVALS(IZ),I).eq.2)then
                    msg=' sensing zone temperature'
                  elseif(NBCTT(IVALS(IZ),I).eq.3)then
                    msg=' sensing daylight coeff.'
                  elseif(NBCTT(IVALS(IZ),I).eq.4)then
                    msg=' Lightswitch not available'
                  elseif(NBCTT(IVALS(IZ),I).eq.-99)then
                    msg=' sensing time only'
                  endif
                  WRITE(OUTS,'(A,I2,A,I2,A,I2,2A,F7.2,a,5f6.3)') 
     &              ' Period ',KK,': from ',IBCST(IVALS(IZ),I),
     &              ' to ',IBCFT(IVALS(IZ),I),msg(1:lnblnk(msg)),
     &              ' setpoint @ ',BACTPT(IVALS(IZ),I),' alt solar',
     &              (TMCT2(IVALS(IZ),I,M),M=1,5)
                  CALL EDISP(itu,OUTS)
                enddo  ! of kk
              endif
 2020       continue
            call edisp(itu,' ')
          endif
        endif

C Note optional zone extensions. If there are casual gain controls
C then rescan that zone file with verbose setting.
        if(zoneextrainfo.ge.1)then
          ivz=IVALS(IZ)   ! remember the selected zone
          if(IVF(ivz).EQ.1)call edisp(itu,
     &      ' Explicit viewfactors have been derived for this zone.')
          if(ICGC(ivz).EQ.1)then
            call edisp(itu,
     &      ' Casual gains are controlled in this zone.')
            iitrc=1
            call ercgcf(iitrc,itu,LCGCIN(IVALS(IZ)),IVALS(IZ),ier)
          endif
          if(IOBS(ivz).gt.0.and.nbobs(ivz).gt.0)then
            if(markdown)then
              call edisp(itu,' ')
              call edisp(itu,': Blocks associated with zone')
              call edisp(itu,'  ')
              write(outs,'(2a)')
     &   'Block X       Y      Z      DX     DY     DZ     ',
     &   'Orient        Opacity Name     Constr'
              call edisp(itu,outs)
              write(outs,'(2a)')
     &   '----- ------- ------ ------ ------ ------ ------ ',
     &   '------ ------ ------- -------- -----------'
              call edisp(itu,outs)
            else
              write(outs,'(2a)')
     &   'Block  X-     Y-    Z-coords   DX-    DY-    DZ-values  ',
     &   'Orientation   Opacity  Name  Construction'
              call edisp(itu,outs)
            endif
            DO I=1,nbobs(ivz)
              llbm=LNBLOCKMAT(ivz,I)
              llbn=LNBLOCKNAME(ivz,I)
              if(BLOCKTYP(ivz,I)(1:4).eq.'obs '.or.
     &           BLOCKTYP(ivz,I)(1:4).eq.'obs3')then
                if(markdown)then
                  WRITE(OUTS,'(I5,a,8F7.1,F6.2,4a)') I,' ',
     &            XOB(ivz,I),YOB(ivz,I),ZOB(ivz,I),
     &            DXOB(ivz,I),DYOB(ivz,I),DZOB(ivz,I),
     &            BANGOB(ivz,I,1),BANGOB(ivz,I,2),OPOB(ivz,I),'   ',
     &            BLOCKNAME(ivz,I)(1:8),' ',BLOCKMAT(ivz,I)(1:llbm)
                else
                  WRITE(OUTS,'(I3,8F8.1,F5.2,4a)') I,
     &            XOB(ivz,I),YOB(ivz,I),ZOB(ivz,I),
     &            DXOB(ivz,I),DYOB(ivz,I),DZOB(ivz,I),
     &            BANGOB(ivz,I,1),BANGOB(ivz,I,2),OPOB(ivz,I),' ',
     &            BLOCKNAME(ivz,I)(1:llbn),' ',BLOCKMAT(ivz,I)(1:llbm)
                endif
                call edisp(itu,OUTS)
              else
                WRITE(OUTS,'(i3,a,44x,3a)')I,' user defined coords ',
     &            BLOCKNAME(ivz,I)(1:llbn),' ',BLOCKMAT(ivz,I)(1:llbm)
                call edisp(itu,OUTS)
              endif
            enddo  ! of i
            CALL EDISP(itu,' ')
          endif

C Report on visual entities. If there are more than 8 do as a double column.
C Pad the reporting as required.
          if(nbvis(ivz).gt.0)then
            im=MOD(nbvis(ivz),2)
            odd=.false.
            if(im.eq.1) odd=.true.
            if(nbvis(ivz).lt.8)then   ! Write as a single column
              if(markdown)then
                call edisp(itu,' ')
                call edisp(itu,': Visual entities:')
                call edisp(itu,'  ')
                call edisp(itu,'visual type name       composition')
                call edisp(itu,'------ ---- ---------- -----------')
              else
                call edisp(itu,'visual type name    composition')
              endif
              do i=1,nbvis(ivz)
                llbm=LNVISMAT(ivz,i)
                if(llbm.lt.10) llbm=10
                llbn=LNVISNAME(ivz,i)
                if(llbn.lt.10) llbm=10
                if(markdown)then
                  if(VISTYP(ivz,i)(1:4).eq.'vis ')then
                   WRITE(outs,'(4A)')' vis   1rot ',
     &              VISNAME(ivz,i)(1:10),' ',VISMAT(ivz,i)(1:10)
                  elseif(VISTYP(ivz,i)(1:4).eq.'vis3')then
                    WRITE(outs,'(4A)')' vis  3rot ',
     &              VISNAME(ivz,i)(1:10),' ',VISMAT(ivz,i)(1:10)
                  elseif(VISTYP(ivz,i)(1:4).eq.'visp')then
                    WRITE(outs,'(4A)')' vis  poly ',
     &              VISNAME(ivz,i)(1:10),' ',VISMAT(ivz,i)(1:10)
                  endif
                else
                  if(VISTYP(ivz,i)(1:4).eq.'vis ')then
                    WRITE(outs,'(4A)')' vis 1rot  ',
     &              VISNAME(ivz,i)(1:llbn),' ',VISMAT(ivz,i)(1:llbm)
                  elseif(VISTYP(ivz,i)(1:4).eq.'vis3')then
                    WRITE(outs,'(4A)')' vis 3rot  ',
     &              VISNAME(ivz,i)(1:llbn),' ',VISMAT(ivz,i)(1:llbm)
                  elseif(VISTYP(ivz,i)(1:4).eq.'visp')then
                    WRITE(outs,'(4A)')' vis poly  ',
     &              VISNAME(ivz,i)(1:llbn),' ',VISMAT(ivz,i)(1:llbm)
                  endif
                endif
                call edisp(itu,outs)
              enddo  ! of iv
            else
              if(markdown)then
                call edisp(itu,' ')
                call edisp(itu,': Visual entities:')
                call edisp(itu,'  ')
                write(outs,'(2a)')
     &            'visual type name       composition  ',
     &            'visual type name       composition'
                call edisp(itu,outs)
                write(outs,'(2a)')
     &            '------ ---- ---------- -----------  ',
     &            '------ ---- ---------- -----------'
                call edisp(itu,outs)
              else
                write(outs,'(2a)') 
     &            'visual type name    composition        ',
     &            'visual type name    composition'
                call edisp(itu,outs)
              endif
              MNU=(nbvis(ivz)/2)
              DO 193 J=1,MNU
                llbm=LNVISMAT(ivz,J)
                if(llbm.lt.10) llbm=10
                llbn=LNVISNAME(ivz,J)
                if(llbn.lt.10) llbn=10
                llbm2=LNVISMAT(ivz,J+MNU)
                llbn2=LNVISNAME(ivz,J+MNU)
                if(markdown)then
                  if(VISTYP(ivz,J)(1:4).eq.'vis ')then
                   WRITE(outs,'(4A)')' vis   1rot ',
     &               VISNAME(ivz,J)(1:10),' ',VISMAT(ivz,J)(1:10)
                   WRITE(outs2a,'(4A)')'vis  1rot ',
     &             VISNAME(ivz,J+MNU)(1:10),' ',VISMAT(ivz,J+MNU)(1:10)
                  elseif(VISTYP(ivz,i)(1:4).eq.'vis3')then
                    WRITE(outs,'(4A)')' vis  3rot ',
     &                VISNAME(ivz,J)(1:10),' ',VISMAT(ivz,J)(1:10)
                    WRITE(outs2a,'(4A)')'vis  3rot ',
     &              VISNAME(ivz,J+MNU)(1:10),' ',VISMAT(ivz,J+MNU)(1:10)
                  elseif(VISTYP(ivz,i)(1:4).eq.'visp')then
                    WRITE(outs,'(4A)')' vis  poly ',
     &                VISNAME(ivz,J)(1:10),' ',VISMAT(ivz,J)(1:10)
                    WRITE(outs2a,'(4A)')'vis  poly ',
     &              VISNAME(ivz,J+MNU)(1:10),' ',VISMAT(ivz,J+MNU)(1:10)
                  endif
                else
                  if(VISTYP(ivz,J)(1:4).eq.'vis ')then
                    WRITE(outs,'(4A)')' vis 1rot  ',
     &                VISNAME(ivz,J)(1:llbn),' ',VISMAT(ivz,J)(1:llbm)
                    WRITE(outs2a,'(4A)')' vis 1rot  ',
     &                VISNAME(ivz,J+MNU)(1:llbn2),' ',
     &                VISMAT(ivz,J+MNU)(1:llbm2)
                  elseif(VISTYP(ivz,i)(1:4).eq.'vis3')then
                    WRITE(outs,'(4A)')' vis 3rot  ',
     &                VISNAME(ivz,J)(1:llbn),' ',VISMAT(ivz,J)(1:llbm)
                    WRITE(outs2a,'(4A)')' vis 3rot  ',
     &                VISNAME(ivz,J+MNU)(1:llbn2),' ',
     &                VISMAT(ivz,J+MNU)(1:llbm2)
                  elseif(VISTYP(ivz,i)(1:4).eq.'visp')then
                    WRITE(outs,'(4A)')' vis poly  ',
     &                VISNAME(ivz,J)(1:llbn),' ',VISMAT(ivz,J)(1:llbm)
                    WRITE(outs2a,'(4A)')' vis poly  ',
     &                VISNAME(ivz,J+MNU)(1:llbn2),' ',
     &                VISMAT(ivz,J+MNU)(1:llbm2)
                  endif
                endif
                llo=lnblnk(outs)
                if(llo.lt.36) llo=36
                write(outs3,'(3a)') outs(1:llo),'  ',
     &            outs2a(1:lnblnk(outs2a))
                call edisp(itu,outs3)
  193         CONTINUE

C Put odd vertex in left column.
              if(odd)then
                J=nbvis(ivz)
                llbm=LNVISMAT(ivz,J)
                if(llbm.lt.10) llbm=10
                llbn=LNVISNAME(ivz,J)
                if(llbn.lt.10) llbn=10
                if(VISTYP(ivz,J)(1:4).eq.'vis ')then
                  WRITE(outs,'(4A)')
     &              '                                       vis 1rot  ',
     &              VISNAME(ivz,J)(1:llbn),' ',VISMAT(ivz,J)(1:llbm)
                elseif(VISTYP(ivz,i)(1:4).eq.'vis3')then
                  WRITE(outs,'(4A)')
     &              '                                       vis 3rot  ',
     &              VISNAME(ivz,J)(1:llbn),' ',VISMAT(ivz,J)(1:llbm)
                elseif(VISTYP(ivz,i)(1:4).eq.'visp')then
                  WRITE(outs,'(4A)')
     &              '                                       vis poly  ',
     &              VISNAME(ivz,J)(1:llbn),' ',VISMAT(ivz,J)(1:llbm)
                endif
                call edisp(itu,outs)
              endif
            endif
            CALL EDISP(itu,' ')
          endif

          if(ISI(ivz).EQ.1)then
            call FINDFIL(LSHAD(ivz),XST)
            if(XST)then
              call edisp(itu,
     &          ' Shading patterns have been calculated for this zone.')
            else
              call edisp(itu,
     &          ' Shading patterns have not yet been calculated.')
            endif
          endif

          if(IHC(ivz).EQ.1)then
            if(zoneextrainfo.eq.1)then
              call edisp(itu,
     &         ' Non-default convection regimes are used in this zone.')
              call FINDFIL(LHCCO(ivz),XST)
              IF(XST)THEN
                call ehtcff(LHCCO(ivz),IUF,IER)
                call listhtc(itu,ivz,'s',IER)
              endif
            elseif(zoneextrainfo.eq.2)then
              call FINDFIL(LHCCO(ivz),XST)
              IF(XST)THEN
                call ehtcff(LHCCO(ivz),IUF,IER)
                call listhtc(itu,ivz,'f',IER)
              endif
            endif
          endif

C Note cfd descriptions, rescan the zone file with verbose setting.
          if(LCFD(ivz)(1:7).eq.'UNKNOWN')then
            continue
          else
            call edisp(itu,'  ')
            iitrc=2
            call DFDREAD(ivz,iitrc,itu,ier)
          endif 
        endif

C Check for mismatch in surface attributes and boundary conditions.
        call edisp(itu,' ')
        bndrysxc=.true.
        DO 1243 IS=1,NZSUR(IVALS(IZ))
          icc=IZSTOCN(IVALS(IZ),is)
          write(SN,'(a12)')SNAME(IVALS(IZ),is)
          lnsn=lnblnk(SN)
          if(ICT(icc).eq.0.and.SOTHER(IVALS(IZ),is,1)(1:5).ne.
     &      'EXTER')then
            bndrysxc=.false.
            write(boundmsg,'(2a)') 'Expecting EXTER for ',SN(1:lnsn)
            call edisp(itu,boundmsg)
          endif
          if(ICT(icc).eq.1.and.SOTHER(IVALS(IZ),is,1)(1:5).ne.
     &      'SIMIL')then
            bndrysxc=.false.
            write(boundmsg,'(2a)') 'Expecting SIMILAR for ',SN(1:lnsn)
            call edisp(itu,boundmsg)
          endif
          if(ICT(icc).eq.2.and.SOTHER(IVALS(IZ),is,1)(1:5).ne.
     &      'CONST')then
            bndrysxc=.false.
            write(boundmsg,'(2a)') 'Expecting CONST for ',SN(1:lnsn)
            call edisp(itu,boundmsg)
          endif
          if(ICT(icc).eq.3.and.SOTHER(IVALS(IZ),is,1)(1:5).eq.
     &      'CONST')then
            bndrysxc=.false.
            write(boundmsg,'(2a)') 'Expecting OTHER for ',SN(1:lnsn)
            call edisp(itu,boundmsg)
          endif
          if(ICT(icc).eq.3.and.SOTHER(IVALS(IZ),is,1)(1:5).eq.
     &      'SIMIL')then
            bndrysxc=.false.
            write(boundmsg,'(3a)') 'Expecting OTHER for ',SN(1:lnsn),
     &        ' got SIMIL.'
            call edisp(itu,boundmsg)
          endif
          if(ICT(icc).eq.3.and.SOTHER(IVALS(IZ),is,1)(1:5).eq.
     &      'EXTER')then
            bndrysxc=.false.
            write(boundmsg,'(3a)') 'Expecting OTHER for ',SN(1:lnsn),
     &        ' got EXTER.'
            call edisp(itu,boundmsg)
          endif
          if(ICT(icc).eq.3.and.SOTHER(IVALS(IZ),is,1)(1:5).eq.
     &      'GROUN')then
            bndrysxc=.false.
            write(boundmsg,'(3a)') 'Expecting OTHER for ',SN(1:lnsn),
     &        ' got GROUND.'
            call edisp(itu,boundmsg)
          endif
          if(ICT(icc).eq.4.and.SOTHER(IVALS(IZ),is,1)(1:5).ne.
     &      'GROUN')then
            bndrysxc=.false.
            write(boundmsg,'(3a)') 'Expecting GROUND for ',SN(1:lnsn),
     &        ' got something else.'
            call edisp(itu,boundmsg)
          endif
          if(ICT(icc).eq.5.and.SOTHER(IVALS(IZ),is,1)(1:5).ne.
     &      'ADIAB')then
            bndrysxc=.false.
            write(boundmsg,'(3a)') 'Expecting ADIAB for ',SN(1:lnsn),
     &        ' got something else.'
            call edisp(itu,boundmsg)
          endif

C While looping check that constructions of partitions are matching.
          if(ICT(icc).eq.3)then
            showother=.true.
            icoth=IZSTOCN(IC2(icc),IE2(icc))
          else
            showother=.false.
          endif
          write(SN,'(a12)')SNAME(IVALS(IZ),is)
          if(showother.and.icoth.ne.0.and.
     &       smlcindex(IVALS(IZ),is).ne.0)then
            ii=smlcindex(IVALS(IZ),is)
            if(mlcsymetric(ii)(1:9).EQ.'SYMMETRIC')then
              continue
            elseif(mlcsymetric(ii).EQ.'NONSYMMETRIC')then

C If the current construction is nonsymmetric then it should not be
C used for a partition (and db does not have a linked MLC - inform the user.
              lnsmlcn=lnblnk(SMLCN(IVALS(IZ),is))
              write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &          ' has a nonsymmetric construction ',
     &          SMLCN(IVALS(IZ),is)(1:lnsmlcn),'.'
              call edisp(itu,outs)
              lnsmlcn=lnblnk(SMLCN(IC2(icc),IE2(icc)))
              write(outs,'(5a)') 'It faces ',
     &          SNAME(IC2(icc),IE2(icc)),' which is composed of ',
     &          SMLCN(IC2(icc),IE2(icc))(1:lnsmlcn),
     &          ' (which may not match).'
              call edisp(itu,outs)
            else
              continue
            endif
          endif
 1243   continue
        if(.NOT.bndrysxc)then
          call edisp(itu,
     &   ' NOTE: Some surface boundary attributions did not match the')
          call edisp(itu,
     &   ' master connections list. A topology update may be required.')
        endif

        if(schedinfo.ge.1)then

C If extended reporting, check to see if operations file is the same
C as an earlier one.
          lco=LPROJ(IVALS(IZ))
          call FINDFIL(lco,XST)
          if(XST)then
            ilcot=0
            duplicate=.false.
            if(IZ.gt.1)then
              do IZO=1,IZ
                lcot=LPROJ(IVALS(IZO))
                if(lco(1:lnblnk(lco)).eq.lcot(1:lnblnk(lcot)))then
                  if(IZO.ne.IZ)then
                    duplicate=.true.
                    ilcot=IVALS(IZO)
                    goto 101
                  endif
                endif
              enddo  ! of izo
            endif
 101        if(duplicate.and.ilcot.ne.0)then
              write(outs,'(2a)') ' Uses same operations as zone ',
     &          zname(ilcot)(1:lnzname(ilcot))
              call edisp(itu,outs)
            else
              CALL ERPFREE(IUF,ISTAT)
              CALL EROPER(0,iuout,IUF,IVALS(IZ),IER)
              if(IAIRN.ge.1.and.ICAAS(IVALS(IZ)).ne.0)then
                call edisp(itu,
     & ' Ventilation & infiltration is assessed via network analysis')
            write(outs,'(2a)') ' and the associated network node is: ',
     &             NDNAM(ICAAS(IVALS(IZ)))
                call edisp(itu,outs)
              else
                if(schedinfo.eq.1)then
                  call edisp(itu,' ')
                  call edisp248(itu,oprdesc(IVALS(IZ)),80)
                elseif(schedinfo.eq.2)then
                  call edisp(itu,' ')
                  CALL VENTINF(IVALS(IZ),ITU)
                endif
              endif
              if(schedinfo.eq.2)then
                call CASINF(IVALS(IZ),ITU)
              endif
              CALL ERPFREE(IUF,ISTAT)
            endif
          else
            write(outs,'(2a)') ' No operations defined for zone ',
     &        zname(IVALS(IZ))(1:lnzname(IVALS(IZ)))
            call edisp(itu,'  ')
            call edisp(itu,outs)
          endif
        endif
 98   continue

C Report code complience information for the project. Do this only
C if the zones are fully attributed.
      if(zoneextrainfo.ge.1)then
        allattribok=.true.
        do ijk=1,INPIC
          if(.NOT.attribok(IVALS(IJK))) allattribok=.false.
        enddo  ! of ijk
        if(.NOT.allattribok)then
          if(markdown)then
            call edisp(itu,'## Areas')
            call edisp(itu,'  ')
          else
            call edisp(itu,'  ')
          endif
          call edisp(itu,'Project AU reporting not included because')
          call edisp(itu,'some zones are not fully attributed.')
        else
          if(markdown)then
            call edisp(itu,'  ')
            call edisp(itu,'## Areas')
            call edisp(itu,'  ')
          else
            call edisp(itu,'  ')
            WRITE(outs,240)
            call edisp(itu,outs)
          endif
          call edisp(itu,'  ')
          call rel16str(xfloor,t16a,lna,ier)
          call rel16str(xareawall,t16b,lnb,ier)
          call rel16str(xareatran,t16c,lnc,ier)
          write(outs,'(7a)') ' Project floor area is ',t16a(1:lna),
     &      'm2, wall area is ',t16b(1:lnb),'m2, window area is ',
     &      t16c(1:lnc),'m2.'
          call edisp(itu,outs)
          call rel16str(xareaslproof,t16a,lna,ier)
          call rel16str(xareafltroof,t16b,lnb,ier)
          call rel16str(xareaskylt,t16c,lnc,ier)
          write(outs,'(7a)') ' Sloped roof area is ',t16a(1:lna),
     &      'm2, flat roof area is ',t16b(1:lnb),
     &      'm2, skylight area is ',t16c(1:lnc),'m2.'
          call edisp(itu,outs)
          call rel16str(xtoground,t16a,lna,ier)
          write(outs,'(3a)') ' In contact with ground ',t16a(1:lna),
     &      'm2.'
          call edisp(itu,outs)
          if(xexposed.gt.0.1)then
            call rel16str(xexposed,t16a,lna,ier)
            call rel16str(xvexposed,t16b,lnb,ier)
            if(xvexposed.gt.0.1)then
              write(outs,'(5a)')' There is ',
     &          t16a(1:lna),'m2 of outside surface area, ',
     &          t16b(1:lnb),'m2 of which is vertical.'
            else
              write(outs,'(3a)')' There is ',
     &          t16a(1:lna),'m2 of outside surface area.'
            endif
            call edisp(itu,outs)
          endif
          call edisp(itu,'  ')
          if(xareawall.gt.0.1)then
            xwallper = (xareawall/xfloor) * 100.
            call rel16str(xwallper,t16a,lna,ier)
            call rel16str(xuavwall,t16b,lnb,ier)
            write(outs,'(3a,F5.3,3a,F5.3)')' Outside walls are ',
     &        t16a(1:lna),' % of floor area & average U of ',
     &        xuavwall/xareawall,' & UA of ',t16b(1:lnb),
     &        ' & max MLC thickness ',xvthk
            call edisp(itu,outs)
          endif
          if(xareaslproof.gt.0.1)then
            xslproofper = (xareaslproof/xfloor) * 100.
            call rel16str(xslproofper,t16a,lna,ier)
            call rel16str(xuavslproof,t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')' Sloped roof is ',
     &        t16a(1:lna),' % of floor area & average U of ',
     &        xuavslproof/xareaslproof,' & UA of ',t16b(1:lnb)
            call edisp(itu,outs)
          endif
          if(xareafltroof.gt.0.1)then
            xflatroofper = (xareafltroof/xfloor) * 100.
            call rel16str(xflatroofper,t16a,lna,ier)
            call rel16str(xuavfltroof,t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')' Flat roof is ',
     &        t16a(1:lna),' % of floor area & average U of ',
     &        xuavfltroof/xareafltroof,' & UA of ',t16b(1:lnb)
            call edisp(itu,outs)
          endif
          if(xareatran.gt.0.1)then
            xtranper = (xareatran/xfloor) * 100.
            vtranper = (xareatran/xvexposed) * 100.
            call rel16str(xtranper,t16a,lna,ier)
            call rel16str(vtranper,t16b,lnb,ier)
            call rel16str(xuavgtran,t16c,lnc,ier)
            write(outs,'(5a,F5.3,2a)')
     &        ' Glazing is ',t16a(1:lna),' % of floor & ',t16b(1:lnb),
     &        ' % facade with average U of ',
     &        xuavgtran/xareatran,' & UA of ',t16c(1:lnc)
            call edisp(itu,outs)
          endif
          if(xareaskylt.gt.0.1)then
            xskyper = (xareaskylt/xfloor) * 100.
            call rel16str(xskyper,t16a,lna,ier)
            call rel16str(xuavgsky,t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')' Skylights are ',
     &        t16a(1:lna),' % of floor area & average U of ',
     &        xuavgsky/xareaskylt,' & UA of ',t16b(1:lnb)
            call edisp(itu,outs)
          endif
          if(xtoground.gt.0.1)then
            xgrper = (xtoground/xfloor) * 100.
            foundUv = xuavground/xtoground
            call rel16str(xgrper,t16a,lna,ier)
            call rel16str(xperimlength,t16b,lnb,ier)
            write(outs,'(3a,F5.3,3a,F5.3)')' Ground contact is ',
     &        t16a(1:lna),' % of floor area & average U of ',
     &        foundUv,' & perimeter ',t16b(1:lnb),
     &        ' & max MLC thickness ',xfndthkg
            call edisp(itu,outs)
            if(xperimlength.gt.0.1)then
              betaprime= xtoground/(xperimlength*0.5)
            else
              betaprime= xtoground  ! avoid div by zero
            endif
            deltathick= xvthk + 1.5 * ((1.0/foundUv)+0.04)
            PI = 4.0 * ATAN(1.0)
            Ufleft = (2.0 * 1.5)/ ((Pi * betaprime) + deltathick)
            Ufright = log ((Pi * betaprime / deltathick)+1.0)
            Ufall = Ufleft * Ufright
            foundRextra = (1.0/Ufall) - (1.0/foundUv)
            write(outs,'(a,f6.3,a,f6.3,a,f6.3,a,f6.3,a,f6.3,a,f6.3)')
     &        ' CIBSE ground beta!',betaprime,' dt',deltathick,
     &        ' Ufleft',Ufleft,' Ufright',Ufright,' Uf',Ufall,
     &        ' R extra @ virtual layer',foundRextra
            call edisp(itu,outs)

          endif
        endif
      endif

C MLC database reporting. If toggle set to 'verbose' only list out those
C constructions which have been referenced in the model. If toggle is
C set to 'verbose all' then include all of them.
   57 if(databaseinfo.ge.0)then
        if(markdown)then
          call edisp(itu,' ')
          call edisp2tr(itu,'## Constructions  ')
          call edisp2tr(itu,
     &    ' Multi-layer constructions referenced in the model.  ')
        else
          call edisp(itu,' ')
          WRITE(outs,240)
          call edisp(itu,outs)
          call edisp(itu,' Multi-layer constructions used:')
        endif
C Debug.
C        write(6,*) 'nmlc is ',nmlc,databaseinfo
        do imlc=1,nmlc
          found=.false.
          write(mlc_name,'(a)') mlcname(imlc)(1:lnmlcname(imlc))
          call mlcrefs(mlc_name,areamlc,areamlcamb,areamlcoth,
     &    areamlcb2b,areamlcgrnd,areamlcsimil,tareamlc,found)
          if(found)then
            if(databaseinfo.eq.2)then
              iitrc=2
              call etmldb(iitrc,itu,imlc,imerr)
            elseif(databaseinfo.eq.1)then
              iitrc=1
              call etmldb(iitrc,itu,imlc,imerr)
            elseif(databaseinfo.eq.0)then
              iitrc=0
              call etmldb(iitrc,itu,imlc,imerr)
            endif
            if(markdown)then
              call edisp(itu,' ')
              write(outs,'(3a,F9.2,a)') 'Total area of ',
     &        mlc_name(1:lnblnk(mlc_name)),' is ',tareamlc,'  '
              call edisp2tr(itu,outs)
            else
              write(outs,'(3a,F9.2)') ' Total area of ',
     &        mlc_name(1:lnblnk(mlc_name)),' is ',tareamlc
              call edisp(itu,outs)
            endif
          else
            if(databaseinfo.eq.3)then  ! list all MLC in the db.
              iitrc=2
              call etmldb(iitrc,itu,imlc,imerr)
              if(markdown)then
                write(outs,'(3a)') 'Total area of ',
     &          mlc_name(1:lnblnk(mlc_name)),' is zero (not referenced)'
                call edisp2tr(itu,outs)
              else
                write(outs,'(3a)') ' Total area of ',
     &          mlc_name(1:lnblnk(mlc_name)),' is zero (not referenced)'
                call edisp(itu,outs)
              endif
            endif
          endif
        enddo  ! of imlc

C << materials used portion of the report: for each find the: >>
C << total area in model, then use the thickness and density to >>
C << report on kg of each >>

      endif

C If writing to file call ctldumpt to close it and switch back to text feedback.
      if(destination.eq.1)then
        call ctldumpt(xfile,ixopen,ixloc,ixunit,'contents',IER)
        destination=0
        if(silent)then
          continue
        else
          call edisp(iuout,
     &     'Contents can be viewed via the `edit file` option.')
          call edisp(iuout,' ')
        endif
      endif

C If in silent model return without user intervention.
      if(silent)then
        return
      endif
      goto 3

C Error trap.
 1000 call lusrmsg('Problem scanning weather file',lclim,'W')
      goto 1001

      end

C ******************** mupdate ********************
C Checks whether current model should be updated and guides
C the user through several tasks. Assumes that the user is
C not in browsing mode.

      subroutine mupdate(ier)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "schedule.h"
#include "esprdbfile.h"
#include "espriou.h"
#include "seasons.h"
#include "control.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/cctlnm/ctldoc,lctlf
      common/ishdirec/iaplic(MCOM,2),nsurfcalc(MCOM),lstsfcalc(MCOM,MS),
     &     nsurfinso(MCOM),isurfinso(MCOM,MS)
      common/shad0/ISIcalc,icalcD,icalcM
      integer ISIcalc,icalcD,icalcM
      
C IPV data.
      common/IPVF/lipvdatf
      character lipvdatf*72

C Images.
      character imgfmt*4    ! GIF XBMP TIF JPG
      character imgfoc*4    ! FZON FNET FCTL FDFS FPLN
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer izconstv,iztmcv
      common/znconstrv/izconstv(MCOM),iztmcv(MCOM)

      COMMON/PRECT3/NTMC,NGLAZ(MTMC)

      COMMON/TMCB1/IBCMT(MCOM,MTMC)
      COMMON/TMCB2/NBCTMC(MCOM,MTMC),IBCST(MCOM,MTMC),
     &             IBCFT(MCOM,MTMC),IBCSUR(MCOM,MTMC)
      COMMON/TMCB3/NBCTT(MCOM,MTMC),BACTPT(MCOM,MTMC)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER nbdaytype,nbcaldays,icalender

      character outs*124
      CHARACTER CTLDOC*248,LCTLF*72
      character ZN*12
      character ipvaction*3 ! to instruct it to read from external IPV file
      logical ok,XST,anyish,unixok
      logical newgeo        ! to use for testing if new/old geometry file.
      logical allnew        ! if true then all zones are v1.1 or newer.
      logical forceupgrade  ! if any other zone is version 1.1
      logical MODOPR        ! to flag if zone operations file needs changing.
      logical opernew       ! if true then all operations are current.
      logical forceoper     ! force if any operation is older
      logical haveihc       ! model includes htc files
      logical haveivf       ! model include viewfactor files
      logical QUIET         ! for use in updating zone construction files
      logical warn_ctl      ! Advise user to convert legacy tmc control.

C Flags noting whether casual gain periods are currently sorted.
      logical sorted,problem
      integer iuo  ! file unit for operations file.
      integer imgloop ! loop counter for images

C For dealing with climate file.
      character llclmdb*144
      character fs*1,msg*72
      integer llt,lndbp

      helpinsub='prjqa'   ! set for subroutine
      
C Check if Unix-based or DOS based.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      newgeo=.false.  ! assume older format geometry.
      sorted=.false.  ! assume casual gains not sorted.

C Determine if zone geometry files are v1.1 or newer 
C (so set igupgrade=2). If yes then there is no need 
C to upgrade. If there are v1.1 geometry files but
C also some zone obstruction files then there is
C still a need to upgrade.
      allnew=.true.
      opernew=.true.
      forceoper=.false.
      haveihc=.false.
      haveivf=.false.
      do 775 iz=1,ncomp
        if(gversion(iz).lt.1.1) then
          allnew=.false.
        elseif(gversion(iz).ge.1.1) then
          if(iobs(iz).eq.0)then
            continue
          elseif(iobs(iz).eq.1)then
            allnew=.false.
          elseif(iobs(iz).eq.2)then
            continue
          endif
        endif

C If ip3ver=0 then force it to write to version 2 and if
C ip3ver=1 then write to version 2 and if already 2 then write
C to version 2.1.
        if(ip3ver(iz).eq.0)then
          opernew=.false.
        elseif(ip3ver(iz).eq.1)then
          opernew=.false.
        elseif(ip3ver(iz).eq.2)then
          opernew=.false.
        endif

        if(ihc(iz).gt.0) haveihc=.true.  ! there are htc files so ask
        if(ivf(iz).gt.0) haveivf=.true.  ! there are viewfactor files so ask
  775 continue

      if(opernew)then
        forceoper=.false.
      else
        forceoper=.true.   ! if some older operations do upgrade
      endif

C Check whether the weather file is associated with seasons defined
C in the climatelist file and if none written to the model set flag
C for this to happen.
      if(ihaveseason.gt.0)then
        continue
      else
        INQUIRE (FILE=cdblfil,EXIST=XST)
        if(XST)then
          IUF=IFIL+2
          llt=lnblnk(LCLIM)
          lndbp=lnblnk(standardclmpath)
          if(ipathclim.eq.0.or.ipathclim.eq.1)then
            llclmdb=LCLIM
          elseif(ipathclim.eq.2)then
            write(llclmdb,'(3a)') standardclmpath(1:lndbp),fs,
     &        LCLIM(1:llt)
          endif
          call scancdblist(IUF,llclmdb,'p',ok,iercl)
          if(iercl.eq.0)then
            ihaveseason=1
            allnew=.false.
          endif
        endif
      endif

      if(allnew)then
        igupgrade=2
        forceupgrade=.false.
      else

C Loop through and find out if any zone geometry files are
C version 1.0 and also if the upgrade policy is non-zero.
C So if the model is mixed then force an upgrade. If the
C igupgrade was set to 2 in preferences also force an upgrade.
        forceupgrade=.false.
        do iz=1,ncomp
          if(gversion(iz).lt.1.1) forceupgrade=.true.  ! found older geo
        enddo
        if(forceupgrade.and.igupgrade.eq.0) igupgrade=2
        if(igupgrade.eq.2) forceupgrade=.true.
      endif

      if(forceupgrade)then
        helptopic='model_upgrade_older'
        call gethelptext(helpinsub,helptopic,nbhelp)

        call easkok(
     &    'Model uses an out-of-date format for geometry files!',
     &    'Update to latest format?',ok,
     &     nbhelp)
        if(ok)then

          do 776 iz=1,ncomp
            call eclose(gversion(iz),1.1,0.01,newgeo)
            if(newgeo)then

C If all of the shading directives are zero then double check to
C see if there are shading and obstruction files.
              call georead(IFIL+2,LGEOM(iz),iz,0,iuout,IER)
              if(nsurfcalc(iz).eq.0.and.nsurfinso(iz).eq.0)then
                if(ISI(iz).eq.0.and.IOBS(iz).eq.0)then
                  continue
                elseif(ISI(iz).gt.0.and.IOBS(iz).eq.0)then
                  iaplic(iz,1)=0   ! reset for insolation only
                  iaplic(iz,2)=1
                  nsurfcalc(iz)=0
                  nsurfinso(iz)=0
                  DO 678 I=1,NZSUR(iz)
                    ioc=IZSTOCN(iz,i)

C At this point CFC and CFC2 surfaces are not included as
C insolation sources (?).
                    if(SOTHER(iz,i,1)(1:5).eq.'EXTER'.and.
     &                 SOTF(iz,i)(1:4).ne.'OPAQ'.and.
     &                 SOTF(iz,i)(1:4).ne.'CFC '.and.
     &                 SOTF(iz,i)(1:4).ne.'CFC2')then
                      nsurfinso(iz)=nsurfinso(iz)+1
                      isurfinso(iz,nsurfinso(iz))=I
                    endif
  678             continue
                  call geowrite(IFIL+2,LGEOM(iz),iz,iuout,3,IER)
                elseif(ISI(iz).gt.0.and.IOBS(iz).gt.0)then
                  iaplic(iz,1)=1   ! reset for insolation only
                  iaplic(iz,2)=1
                  nsurfcalc(iz)=0
                  nsurfinso(iz)=0
                  DO 679 I=1,NZSUR(iz)
                    ioc=IZSTOCN(iz,i)

C At this point CFC and CFC2 surfaces are not included as
C insolation sources (?).
                    if(SOTHER(iz,i,1)(1:5).eq.'EXTER'.and.
     &                 SOTF(iz,i)(1:4).ne.'OPAQ'.and.
     &                 SOTF(iz,i)(1:4).ne.'CFC '.and.
     &                 SOTF(iz,i)(1:4).ne.'CFC2')then
                      nsurfinso(iz)=nsurfinso(iz)+1
                      isurfinso(iz,nsurfinso(iz))=I
                    endif
                    if(SOTHER(iz,i,1)(1:5).eq.'EXTER')then
                      nsurfcalc(iz)=nsurfcalc(iz)+1
                      lstsfcalc(iz,nsurfcalc(iz))=I
                    endif
  679             continue

                  call geowrite(IFIL+2,LGEOM(iz),iz,iuout,3,IER)
                endif
              endif

C If there is a v1.1 geometry file and iobs() is 1 then the
C obstructions need to be moved into the geometry file and
C zobs() cleared for this zone and iobs set.
              if(iobs(iz).eq.0)then
                continue
              elseif(iobs(iz).eq.1)then
                iobs(iz)=2
                zobs(iz)=' '
                call geowrite(IFIL+2,LGEOM(iz),iz,iuout,3,IER)
              elseif(iobs(iz).eq.2)then
                continue
              endif
            else

C On first entry for version 1.0 several common blocks need to be
C filled in case the model is a mix of 1.0 and 1.1 versions. First
C figure out implied shading and insolation directives. If there
C is no shading file or obstruction file assume nothing. If there
C is a shading file an no obstruction set for insolation only.
C Otherwise assume all applicable for shading and insolation.
              call egomin(IFIL+2,LGEOM(iz),iz,0,0,iuout,IER)
              if(ISI(iz).eq.0.and.IOBS(iz).eq.0)then
                iaplic(iz,1)=0   ! reset for no implied shading or insolation
                iaplic(iz,2)=0
                nsurfcalc(iz)=0
                nsurfinso(iz)=0
              elseif(ISI(iz).gt.0.and.IOBS(iz).eq.0)then
                iaplic(iz,1)=0   ! reset for insolation only
                iaplic(iz,2)=1
                nsurfcalc(iz)=0
                nsurfinso(iz)=0
                DO 778 I=1,NZSUR(iz)
                  ioc=IZSTOCN(iz,i)

C At this point CFC and CFC2 surfaces are not included as
C insolation sources (?).
                  if(SOTHER(iz,i,1)(1:5).eq.'EXTER'.and.
     &               SOTF(iz,i)(1:4).ne.'OPAQ'.and.
     &               SOTF(iz,i)(1:4).ne.'CFC '.and.
     &               SOTF(iz,i)(1:4).ne.'CFC2')then
                    nsurfinso(iz)=nsurfinso(iz)+1
                    isurfinso(iz,nsurfinso(iz))=I
                  endif
  778           continue
              elseif(ISI(iz).gt.0.and.IOBS(iz).gt.0)then
                iaplic(iz,1)=1   ! reset for insolation only
                iaplic(iz,2)=1
                nsurfcalc(iz)=0
                nsurfinso(iz)=0
                DO 779 I=1,NZSUR(iz)
                  ioc=IZSTOCN(iz,i)

C At this point CFC and CFC2 surfaces are not included as
C insolation sources (?).
                  if(SOTHER(iz,i,1)(1:5).eq.'EXTER'.and.
     &               SOTF(iz,i)(1:4).ne.'OPAQ'.and.
     &               SOTF(iz,i)(1:4).ne.'CFC '.and.
     &               SOTF(iz,i)(1:4).ne.'CFC2')then
                    nsurfinso(iz)=nsurfinso(iz)+1
                    isurfinso(iz,nsurfinso(iz))=I
                  endif
                  if(SOTHER(iz,i,1)(1:5).eq.'EXTER')then
                    nsurfcalc(iz)=nsurfcalc(iz)+1
                    lstsfcalc(iz,nsurfcalc(iz))=I
                  endif
  779           continue
              endif
              gversion(iz) =1.1
              newgeo = .true.
              call geowrite(IFIL+2,LGEOM(iz),iz,iuout,3,IER)

C If iobs() is 1 then the obstructions need to be moved into the
C geometry file and zobs() cleared for this zone and iobs set.
              if(iobs(iz).eq.0)then
                continue
              elseif(iobs(iz).eq.1)then
                iobs(iz)=2
                zobs(iz)=' '
                call geowrite(IFIL+2,LGEOM(iz),iz,iuout,3,IER)
              elseif(iobs(iz).eq.2)then
                continue
              endif
            endif
  776     continue
        endif
      endif

C Check for the version of the zone operations file and if it is
C an older format write to a newer format.
      if(forceoper)then
        helptopic='model_upgrade_older'
        call gethelptext(helpinsub,helptopic,nbhelp)

        call easkok(' ','Refresh zone operation files?',ok,nbhelp)
        if(ok)then
          do iz=1,ncomp
            IUO=IFIL+1
            call FINDFIL(LPROJ(iz),XST)
            IF(XST)THEN

C Scan the operation file. If old format ip3ver will have been
C reset to 21 so that it will write to latest format.
              CALL ERPFREE(IUO,ISTAT)
              CALL EROPER(ITRC,ITRU,IUO,iz,IER)

C Do cursory check to see if the daytypes in the file are sorted.
              sorted=.true.
              problem=.false.
              call checksort(iz,1,problem)
              if(problem)then
                sorted=.false.
                MODOPR=.true.
                call edisp(iuout,
     &           'Weekday casual gains might be unsorted.')
              endif
              problem=.false.
              call checksort(iz,2,problem)
              if(problem)then
                sorted=.false.
                MODOPR=.true.
                call edisp(iuout,
     &            'Saturday casual gains might be unsorted.')
              endif
              problem=.false.
              call checksort(iz,3,problem)
              if(problem)then
                sorted=.false.
                MODOPR=.true.
                call edisp(iuout,
     &            'Sunday casual gains might be unsorted.')
              endif

C Force ip3ver to be 21 and set MODOPR TRUE.
              MODOPR=.true.
              ip3ver(iz)=21

C Save the file, sorting the periods if necessary.
              if(MODOPR)then
                if(sorted)then
                  CALL EMKOPER(IUO,LPROJ(IZ),IZ,IER)
                else
                  call PROCESSOLDCAS(iz,0,iuout)
                  MODOPR=.true.
                  CALL EMKOPER(IUO,LPROJ(IZ),IZ,IER)
                endif
              endif
            endif
          enddo   ! loop for operations
        endif
      endif  ! of forceoper

C Check version and ask if zone construction files should 
C also be refreshed. Note EDCON does nothing if files do not exist.
      forceupgrade=.false.
      IUO=IFIL+1
      do iz=1,ncomp
        CALL ECONST(LTHRM(iz),IUO,IZ,0,IUOUT,IER)
        if(izconstv(iz).lt.21)then  ! found older conl & tmc
          helptopic='model_upgrade_older'
          call gethelptext(helpinsub,helptopic,nbhelp)
          write(msg,'(3a)') 'Zone ',zname(iz)(1:lnblnk(zname(iz))),
     &      ' uses an out-of-date format for constructions.'
          call easkok(msg,'Update to latest format?',ok,nbhelp)
          if(ok)then
            CALL ECONST(LTHRM(iz),IUO,IZ,0,IUOUT,IER)
            if(ier.ne.0)then
              call usrmsg(
     &        'Error reading construction file, skipping upgrade.',
     &        ' ','W')
              cycle
            endif
            if(ITW(IZ).eq.1)then
              CALL ERTWIN(ITRC,IUOUT,IUO,LTWIN(IZ),IZ,IER)
              do IT=1,NTMC
                warn_ctl=.false.
                if(IBCMT(iz,it).gt.0) warn_ctl=.true.
                if(warn_ctl)then

C Convert a legacy tmc control to an optical control via logic
C similar to edcon.F near line 2170.
                  call edisp(iuout,
     &            'Legacy tmc control detected and beging converted.')
                  ICTLF=IFIL+1
                  CALL ERPFREE(ICTLF,ISTAT)
                  call FINDFIL(LCTLF,XST)
                  if(XST)then
                    CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
                  else
                    call usrmsg(
     &              'Control file not found. Canceling action.',
     &              ' ','W')
                    cycle
                  endif
                  NOF=NOF+1
                  IBCMT(IZ,IT)= -1*NOF     ! negate index
                  NOCDT(NOF)=1             ! assume one day type
                  IV=NOCDT(nof)
                  write(outs,'(a,i2,a)')
     &            'Number of optical control day types (currently',IV,
     &            ') :'
                  CALL EASKABC(outs,'(see help)',
     &            'Follow calendar day types','Just one day type',
     &            'Dates of validity (legacy)',IV,nbhelp)
                  if(iv.eq.1)then
                    NOCDT(nof)=0
                  elseif(iv.eq.2)then
                    NOCDT(nof)=1
                  elseif(iv.eq.3)then
                    IVP=3
                    CALL EASKI(IVP,'Number of control periods',
     &              'of validity in the whole year (see help) ',
     &              0,'F',MCF,'-',1,'nb periods of validity',IERI,
     &              nbhelp)
                    if(ieri.eq.-3)then
                      nof=nof-1
                      cycle
                    else
                      NOCDT(nof)=IVP
                      IDOV=1     ! date of validity used
                    endif
                  endif

                  NN=NOCDT(nof)  ! assume zero means all calendar day types
                  IF(NN.EQ.0)NN=NBDAYTYPE
                  do 5 ik=1,NN
                    if(IDOV.EQ.1)then
                      IEDY=365
                      IF(IK.EQ.1)THEN
                        IBDY=1
                      ELSE
                        IBDY=IOCDV(NOF,IK-1,2)+1
                      ENDIF
                      CALL EASKPER('Dates of validity:',IBDY,IEDY,IFDY,
     &                  IER)
                      IOCDV(NOF,IK,1)=IBDY
                      IOCDV(NOF,IK,2)=IEDY
                    else
                      IOCDV(NOF,IK,1)=1
                      IOCDV(NOF,IK,2)=365
                    endif
                    if(NBCTMC(IZ,IT).eq.1)then
                      if(IBCST(IZ,IT).eq.0.and.IBCFT(IZ,IT).eq.24)then
                        NOCDP(NOF,IK)=1          ! 1 period all day
                        TOCPS(NOF,IK,1)=0.0
                        IOCTYP(NOF,IK,1)=0
                        IOCLAW(NOF,IK,1)=2
                        OMISCD(NOF,IK,1,1)=1
                        OMISCD(NOF,IK,1,2)=BACTPT(IZ,IT)
                      else
                        NOCDP(NOF,IK)=3          ! 3 periods
                        TOCPS(NOF,IK,1)=0.0
                        TOCPS(NOF,IK,2)=IBCST(IZ,IT)
                        TOCPS(NOF,IK,3)=IBCFT(IZ,IT)
                        IOCTYP(NOF,IK,1)=0; IOCTYP(NOF,IK,2)=0
                        IOCTYP(NOF,IK,3)=0
                        IOCLAW(NOF,IK,1)=0; IOCLAW(NOF,IK,2)=2
                        IOCLAW(NOF,IK,3)=0
                        OMISCD(NOF,IK,1,1)=0; OMISCD(NOF,IK,2,1)=1
                        OMISCD(NOF,IK,3,1)=0
                        OMISCD(NOF,IK,2,2)=BACTPT(IZ,IT)
                      endif
                    else
                      NOCDP(NOF,IK)=1          ! 1 period all day
                      TOCPS(NOF,IK,1)=0.0
                      IOCTYP(NOF,IK,1)=0
                      IOCLAW(NOF,IK,1)=0
                      OMISCD(NOF,IK,1,1)=0
                    endif
  5               continue
                  if(NOF.eq.1)then
                    write(opticdoc,'(a)')'Conversion from tmc control'
                  endif
                  if(NBCTT(IZ,IT).eq.0)then       ! radiation at a surface
                    IOSN(NOF,1)= -7; IOSN(NOF,2)=iz
                    IOSN(NOF,3)=IBCSUR(IZ,IT) 
                  elseif(NBCTT(IZ,IT).eq.1)then   ! abient dbT
                    IOSN(NOF,1)= -3; IOSN(NOF,2)=0; IOSN(NOF,3)=0
                  elseif(NBCTT(IZ,IT).eq.2)then   ! zone db T
                    IOSN(NOF,1)=1; IOSN(NOF,2)=iz; IOSN(NOF,3)=0 
                  elseif(NBCTT(IZ,IT).eq.3)then   ! lux level
                    IOSN(NOF,1)= -8; IOSN(NOF,2)=0
                    IOSN(NOF,3)=IBCSUR(IZ,IT) 
                  elseif(NBCTT(IZ,IT).eq.4)then   ! lightswitch NA
                    IOSN(NOF,1)= -8; IOSN(NOF,2)=0; IOSN(NOF,3)=0 
                  elseif(NBCTT(IZ,IT).eq.-99)then ! time
                    IOSN(NOF,1)= -3; IOSN(NOF,2)=0; IOSN(NOF,3)=0 ! use ambT as placeholder
                  endif
                  IOAN(NOF,1)=0; IOAN(NOF,2)=IZ; IOAN(NOF,3)=IT

C Save to control file.
                  CALL EASKOK(' ','Save changes to control file?',OK,
     &              nbhelp)
                  IF(OK)THEN
                    ICTLF=IFIL+1
                    CALL CTLWRT(ICTLF,IER)
                  endif
                endif
              enddo
            endif
            izconstv(IZ)=21
            iztmcv(IZ)=21
            ITW(iz)=2
            LTWIN(IZ)='UNKNOWN'
            QUIET=.TRUE.
            CALL EMKCON(LTHRM(IZ),IUO,IZ,QUIET,IER)
          endif
        else
          write(msg,'(3a)') 'Zone ',zname(iz)(1:lnblnk(zname(iz))),
     &      ' uses the current format for constructions.'
          call easkok(msg,'Rebuild the file?',ok,nbhelp)
          if(ok)then
            QUIET=.TRUE.
            CALL EDCON(ITRC,ITRU,IZ,QUIET,IER)
            QUIET=.FALSE.
          endif
        endif
      enddo

C If there are htc files ask if they should be updated.
      if(haveihc)then
        helptopic='model_upgrade_older'
        call gethelptext(helpinsub,helptopic,nbhelp)

        call easkok(' ','Refresh zone heat transfer files?',ok,nbhelp)
        if(ok)then
          do iz=1,ncomp
            IUO=IFIL+1
            if(ihc(iz).eq.1) then
              call FINDFIL(LHCCO(iz),XST)
              if(XST)then
                CALL ERPFREE(IUO,ISTAT)
                call ehtcff(LHCCO(iz),IUO,IER)
                CALL ERPFREE(IUO,ISTAT)
                call EMKHTC(LHCCO(iz),iz,IUO,ITRU,IER)
              endif
            endif
          enddo
        endif
      endif

C If there are viewfactor files ask if they should be updated.
      if(haveivf)then
        helptopic='model_upgrade_older'
        call gethelptext(helpinsub,helptopic,nbhelp)

        call easkok(' ','Refresh zone viewfactor files?',ok,nbhelp)
        if(ok)then
          do iz=1,ncomp
            IUO=IFIL+1
            if(ivf(iz).eq.1) then
              call FINDFIL(LVIEW(iz),XST)
              if(XST)then
                CALL ERPFREE(IUO,ISTAT)
                CALL ERMRT(ITRC,ITRU,IUO,LVIEW(iz),iz,IER)
                CALL ERPFREE(IUO,ISTAT)
                CALL EMKMRT(LVIEW(iz),LGEOM(iz),NZSUR(iz),IUO,iz,'v',
     &            IER)
              endif
            endif
          enddo
        endif
      endif


C Save the model configuration file before continuing.
      CALL EMKCFG('-',IER)

C Check if climate file season data is available via the climatelist
C file and then include seasonal tokens in the model.
      if(ihaveseason.gt.0)then
        continue
      else
        INQUIRE (FILE=cdblfil,EXIST=XST)
        if(XST)then
          IUF=IFIL+2
          llt=lnblnk(LCLIM)
          lndbp=lnblnk(standardclmpath)
          if(ipathclim.eq.0.or.ipathclim.eq.1)then
            llclmdb=LCLIM
          elseif(ipathclim.eq.2)then
            write(llclmdb,'(3a)') standardclmpath(1:lndbp),fs,
     &        LCLIM(1:llt)
          endif
          call scancdblist(IUF,llclmdb,'p',ok,iercl)
          if(iercl.eq.0)then
            ihaveseason=1
            CALL EMKCFG('-',IER)
          endif
        endif
      endif


C While we are at it, control files often include out of date descriptions
C and if there is a control file scan it and write it back out again.
      if(LCTLF(1:2).eq.'  '.or.LCTLF(1:4).eq.'UNKN')then
        continue
      else
        ICTLF=IFIL+1
        CALL ERPFREE(ICTLF,ISTAT)
        XST=.false.
        call FINDFIL(LCTLF,XST)
        if(XST)then
          CALL EZCTLR(ICTLF,0,IUOUT,IER)
          CALL CTLWRT(ICTLF,IER)
          call usrmsg(' ',' ','-')
        endif
      endif

C Check to see if version of configuration file requires updating.
      if(icfgv.le.4)then
        helptopic='model_upgrade_v3'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkok('Configuration file has out-of-date format!',
     &     'Update to latest format?',ok,nbhelp)
        if(ok)then

C Check if IPV file known and fill common blocks before writing out to
C the cfg file. If it was previously merged in the file name will now
C be a black string or as 'internal'.
          if(lnblnk(lipvdatf).eq.0)then
            continue
          elseif(lipvdatf(1:7).eq.'UNKNOWN')then
            continue
          elseif(lipvdatf(1:8).eq.'internal')then
            continue
          else
            IUO=IFIL+1
            ipvaction='ipv'
            call ripvdat(IUO,lipvdatf,ipvaction,ier)
          endif
          icfgv=5   ! set to current version.

C If there are any images associated with the model create a filler
C string for the image documentation.
          if(noimg.gt.0)then
            do 22 imgloop=1,MIMG
              imgdoc(imgloop)='no documentation (yet) for this image'
  22        continue
          endif

C Update the model configuration file.
          CALL EMKCFG('-',IER)

        else
          return
        endif
      endif

C Offer option to update shading files.
      if(icfgv.lt.4)then
        if(ISIcalc.eq.1)then
          anyish=.false. 
          goto 44 ! embedded so jump
        endif
        anyish=.false.
        do 42 iz=1,ncomp
          if(ISI(iz).eq.1)then
            if(ISIcalc.eq.0) ISIcalc = 2  ! no set so = 2
            call FINDFIL(LSHAD(iz),XST)
            if(XST)then
              write(ZN,'(A)') zname(iz)
              write(outs,'(3a)') 'Shading for ',ZN(1:lnblnk(ZN)),
     &          ' could be updated.'
              call edisp(iuout,outs)
              anyish=.true.
            endif
          endif
  42    continue
      else
        anyish=.false.
      endif

C If there are no shading files, don't bother trying to update.
  44  if(anyish)then
        helptopic='shading_upgrade'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC(' ','Shading update options:',
     &    'recalculate (silent)','recalculate (interactive)','cancel',
     &     IRT,nbhelp)
      else
        IRT=3
      endif
      if(IRT.eq.1.or.IRT.eq.2)then
        call edisp(iuout,'Each zone will be done in sequence.')
        if(IRT.eq.1)then
          call edisp(iuout,
     & 'When finished control will be returned to the project manager.')
        endif
        do 43 iz=1,ncomp
          if(ISI(iz).eq.0) goto 43  ! skip if no monthly file
          if(ISIcalc.eq.1) goto 43  ! skip if embedded calculations
          call FINDFIL(LSHAD(iz),XST)
          if(XST)then

C If user asked for silent recalculation then do each
C one in forground in an xterm with `-act update_silent`
C as the command line.
            if(IRT.eq.1)then
              call comissionish(iz,'sr ',ier)
              if(ier.ne.0)then
                call edisp(iuout,'Possible error in calculations.')
              endif
            else
              call comissionish(iz,'ir ',ier)
              if(ier.ne.0)then
                call edisp(iuout,'Possible error in calculations.')
              endif
            endif
          endif
  43    continue
        call edisp(iuout,
     &    'Update of zones with shading files complete.')
      endif

      return

      end

C ******************* FINDUA ******************
C FindUA scans the current model common blocks and derives the overall
C UA values and areas for different elements of the facade.
      subroutine FINDUA(IVALS,nz,act)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "sbem.h"
C derived.h are derived areas and U values for zones and the model
#include "derived.h"

C Parameters passed in
      integer IVALS(MCOM)  ! the array of zones to include
      integer NZ           ! number of zones to include
      character act*1      ! 'c' clear and 'p' process
 
      common/FILEP/IFIL
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

C bridgelen was calculated in subroutine suredgeadj
      real bridgelen ! Length of potential thermal bridges in each zone.
      common/gbridge/bridgelen(MCOM,16)

      real mlcthick

      integer IUF   ! file unit
      character*72 LTMP
      character DESCRC*25

      IUF=IFIL+2   ! set file unit for scanning geometry files

C Zero the counters.
      if(act.eq.'c')then
        xfloor = 0.0
        xexposed = 0.0
        xtoground = 0.0
        xvexposed = 0.0
        xvthk = 0.0
        xuavgtran = 0.0
        xuavwall = 0.0
        xuavground = 0.0
        xfndthkg = 0.0
        xperimlength = 0.0
        xuavslproof = 0.0
        xuavfltroof = 0.0
        xuavgsky = 0.0
        xareatran = 0.0
        xareawall = 0.0
        xareaslproof = 0.0
        xareafltroof = 0.0
        xareaskylt = 0.0
        return
      endif

C Loop through the selected zones.
      do 98 IZ=1,NZ
        IZZ=IVALS(IZ)
        if(IZZ.eq.0)goto 98
        LTMP=LGEOM(IZZ)
        call georead(IUF,LTMP,IZZ,1,itu,IER)

C If there are external opaque and transparent surfaces report.
C Zero zone code complence data. xfloor is project floor area,
C tranper() skyper() wallper() slproofper() flatroofper() are % of
C floor area for each.
        xfloor = xfloor + ZBASEA(IZZ)
        plen = bridgelen(IZZ,2) ! ground edge at facade
        xperimlength = xperimlength + plen ! based on surface edge data
        uavgtran(IZZ) = 0.0
        uavwall(IZZ) = 0.0
        uavslproof(IZZ) = 0.0
        uavfltroof(IZZ) = 0.0
        uavgsky(IZZ) = 0.0
        uavground(IZZ) = 0.0
        areatran(IZZ) = 0.0
        areainttran(IZZ) = 0.0
        areaintopaq(IZZ) = 0.0
        areawall(IZZ) = 0.0
        areaslproof(IZZ) = 0.0
        areafltroof(IZZ) = 0.0
        areaskylt(IZZ) = 0.0
        areaground(IZZ) = 0.0
        tranper(IZZ) = 0.0
        skyper(IZZ) = 0.0
        wallper(IZZ) = 0.0
        slproofper(IZZ) = 0.0
        flatroofper(IZZ) = 0.0
        exposed(IZZ) = 0.0
        vexposed(IZZ) = 0.0
        do 111 ij=1,nzsur(izz)
          icc=izstocn(izz,ij)
          if(icc.eq.0)then
            continue  ! not a known surface connection
          else
            CALL SURADJ(IZZ,IJ,IE,TMP,IZC,ISC,IC,DESCRC) ! not dependant on georead

C Check the U value for each surface.
            UVH = 0.0
            UVU = 0.0
            UVD = 0.0

C Use smlcindex to find current MLC and its thickness and recover U values.
            isel=smlcindex(izz,ij)     ! identify the MLC index
            if(isel.gt.0)then
              mlcthick=THKMLC(isel)    ! current surface thickness
              call etmldbu(0,itu,isel,UVH,UVU,UVD,UVI,UVG)
            else
              continue
            endif

C Recover the ISO 6946 U values for horizontal, upwards and downwards
C flow as well as internal partition (horizontal 0.13 both sides).
C Note: ISO 6946 advises use of horizontal hc value for surfaces where
C heat flow direction is unknown. This assumption is used in
C the U and UA reporting below.

            if(IE.eq.0)then

C For surfaces connected to the outside.
              exposed(IZZ) = exposed(IZZ) + SNA(izz,ij)
              xexposed = xexposed + SNA(izz,ij)
              if(SOTF(izz,ij)(1:4).eq.'OPAQ')then
                if(SVFC(izz,ij)(1:4).eq.'VERT')then

C For vertical walls assume horizontal hc coef.
                  areawall(IZZ) = areawall(IZZ) + SNA(izz,ij)
                  uavwall(IZZ) = uavwall(IZZ) + (SNA(izz,ij)*UVH)
                  vexposed(IZZ) = vexposed(IZZ) + SNA(izz,ij)
                  xvexposed = xvexposed + SNA(izz,ij)
                  xareawall = xareawall + SNA(izz,ij)
                  xuavwall = xuavwall + (SNA(izz,ij) * UVH)
                  if(mlcthick.gt.xvthk) xvthk = mlcthick ! keep track of thickest
                elseif(SVFC(izz,ij)(1:4).eq.'SLOP')then

C For sloped surfaces assume upward hc coef.
                  areaslproof(IZZ) = areaslproof(IZZ) + SNA(izz,ij)
                  uavslproof(IZZ) = uavslproof(IZZ) + (SNA(izz,ij)*UVU)
                  xareaslproof = xareaslproof + SNA(izz,ij)
                  xuavslproof = xuavslproof + (SNA(izz,ij)*UVU)
                elseif(SVFC(izz,ij)(1:4).eq.'CEIL')then

C For ceilings assume upward hc coef.
                  areafltroof(IZZ) = areafltroof(IZZ) + SNA(izz,ij)
                  uavfltroof(IZZ) = uavfltroof(IZZ) + (SNA(izz,ij)*UVU)
                  xareafltroof = xareafltroof + SNA(izz,ij)
                  xuavfltroof = xuavfltroof + (SNA(izz,ij) * UVU)
                else

C For floors facing outside assume downward hc coef.
                  areawall(IZZ) = areawall(IZZ) + SNA(izz,ij)
                  uavwall(IZZ) = uavwall(IZZ) + (SNA(izz,ij)*UVD)
                  xareawall = xareawall + SNA(izz,ij)
                  xuavwall = xuavwall + (SNA(izz,ij) * UVD)
                endif
              elseif(SOTF(izz,ij)(1:4).ne.'OPAQ') then !.and.
                if(SVFC(izz,ij)(1:4).eq.'CEIL'.or.
     &             SVFC(izz,ij)(1:4).eq.'SLOP')then

C Consider glazing on ceiling or sloped to be a skylight.
                  areaskylt(IZZ) = areaskylt(IZZ) + SNA(izz,ij)
                  uavgsky(IZZ) = uavgsky(IZZ) + (SNA(izz,ij)*UVU) 
                  xareaskylt = xareaskylt + SNA(izz,ij)
                  xuavgsky = xuavgsky + (SNA(izz,ij) * UVU)
                elseif(SVFC(izz,ij)(1:4).eq.'VERT')then

C Consider glazing on walls to be a part of facade.
                  areatran(IZZ) = areatran(IZZ) + SNA(izz,ij)
                  uavgtran(IZZ) = uavgtran(IZZ) + (SNA(izz,ij)*UVH)
                  vexposed(IZZ) = vexposed(IZZ) + SNA(izz,ij)
                  xvexposed = xvexposed + SNA(izz,ij)
                  xareatran = xareatran + SNA(izz,ij)
                  xuavgtran = xuavgtran + (SNA(izz,ij) * UVH)
                else
                  areatran(IZZ) = areatran(IZZ) + SNA(izz,ij)
                  uavgtran(IZZ) = uavgtran(IZZ) + (SNA(izz,ij)*UVD)
                  vexposed(IZZ) = vexposed(IZZ) + SNA(izz,ij)
                  xvexposed = xvexposed + SNA(izz,ij)
                  xareatran = xareatran + SNA(izz,ij)
                  xuavgtran = xuavgtran + (SNA(izz,ij) * UVD)
                endif
              else
                continue
              endif
            elseif(IE.eq.3)then

C Internal partitions floors ceilings.
              if(SOTF(izz,ij)(1:4).eq.'OPAQ')then
                areaintopaq(IZZ) = areaintopaq(IZZ)+SNA(izz,ij)
              elseif(SOTF(izz,ij)(1:4).ne.'OPAQ'.and.
     &               SOTF(izz,ij)(1:4).ne.'CFC '.and.
     &               SOTF(izz,ij)(1:4).ne.'CFC2')then
                areainttran(IZZ) = areainttran(IZZ)+SNA(izz,ij)
              endif
            elseif(IE.eq.4)then

C For surfaces connected to the ground.
              xtoground = xtoground + SNA(izz,ij)   ! increment area to ground
              areaground(IZZ) = areaground(IZZ) + SNA(izz,ij) ! increment area to ground
              xuavground = xuavground + (SNA(izz,ij) * UVG)
              uavground(IZZ) = uavground(IZZ) + (SNA(izz,ij) * UVG)
              if(mlcthick.gt.xfndthkg) xfndthkg = mlcthick
            else
              continue
            endif
          endif
 111    continue

 98   continue

      return
      end
