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

C Project manager: QA and updating facilities comprising
C QA functions included in this file:
C   prjqa:   writes out a QA report (to screen or file) for the current model.
C   mupdate: checks current model to see if it should be updated.

C ************ prjqa
C Prjqa writes out a QA report (to screen or file) for
C the current model.
      subroutine prjqa(inpic,ivals,silent,ier)
C AIM-2.
C IAIM2 set to 1 if AIM-2 infiltration model active.
C LAIM2 (72 char) file holding AIM-2 infiltration model data.
C      common/aim2def/iAIM2,LAIM2

C << Task for future - respond to invocation via command line
C << -act QA in combination with -v or -vv to set the level
C << of reporing if from command line.

      USE AIM2_InputData, ONLY:iAIM2,LAIM2
      USE AIM2, ONLY:AIM2_READIN, AIM2_report
      
      IMPLICIT NONE
      
#include "building.h"
#include "model.h"
#include "site.h"

C geometry.h provides commons G0/G2/G7/prec2/precz/c20.
#include "geometry.h"
#include "net_flow.h"
#include "control.h"
#include "tdf2.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C database file names and unit numbers and path types
#include "material.h"
#include "espriou.h"
C espriou.h provides currentfile.
#include "sbem.h"
#include "ipvdata.h"
#include "seasons.h"
#include "schedule.h"
C derived.h are derived areas and U values for zones and the model
#include "derived.h"
      
      integer lnblnk  ! function definition

C Parameters passed into subroutine:
      integer inpic,ivals   ! number of zones to include
      dimension ivals(MCOM) ! list of zones
      logical silent        ! if true do not interact with user
      integer ier           ! non-zero if a problem

      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: iuout,iuin
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      INTEGER :: ic1,ie1,ict,ic2,ie2
      common/C6/INDCFG
      INTEGER :: INDCFG
      integer :: icascf
      common/cctl/icascf(mcom)

C IFPNF is the unit for the plant network file.
C LPNF (72 char) plant network file name.
      COMMON/C23/IFPNF,LPNF
      INTEGER :: IFPNF
      INTEGER :: IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      common/RAY3/MODIFY,MODLEN,MODBND
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit
      INTEGER :: ixopen,ixloc,ixunit
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      REAL :: TMCT,TMCREF,TMCT2,TMCA,TMCA2
      INTEGER :: ITMCFL
      COMMON/PRECT2/TMCT2(MCOM,MTMC,5,MBP),TMCA2(MCOM,MTMC,ME,5,MBP),
     &              TVTR2(MCOM,MTMC,MBP)
      COMMON/PRECT3/NTMC,NGLAZ(MTMC)
      INTEGER :: NTMC,NGLAZ
      common/PRECT4/TOPTIC(MCOM,MTMC)
      COMMON/TMCB1/IBCMT(MCOM,MTMC)
      COMMON/TMCB2/NBCTMC(MCOM,MTMC),IBCST(MCOM,MBP,MTMC),
     &             IBCFT(MCOM,MBP,MTMC),IBCSUR(MCOM,MTMC)
      INTEGER :: IBCMT,IBCST,IBCFT,IBCSUR,NBCTMC
      COMMON/TMCB3/NBCTT(MCOM,MBP,MTMC),BACTPT(MCOM,MBP,MTMC)
      INTEGER :: NBCTT
      REAL :: BACTPT
      common/cctlnm/ctldoc,lctlf
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      COMMON/MFLOW2/NDNAM(0:MNOD)


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

      logical found,XST,apply,MODIFY,MODLEN,MODBND,duplicate
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

C bridgelen was calculated in subroutine suredgeadj
      real bridgelen ! Length of potential thermal bridges in each zone.
      common/gbridge/bridgelen(MCOM,10)

      real plen  ! combined length of standard and metal facade perimeter

      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

C Remember if each zone is fully attributed.
      logical attribok(MCOM)
      logical allattribok,changedit,ok,dok,concat,unixok,showother
      dimension areamlc(MCOM)
      real areamlc
      character items*33
      dimension items(25)

      character*72 LTMP
      character*72 GTGEOM
      character GTNAME*15,CLMLOC*30,lipvdatf*72
      character lco*72,lcot*72
      character longtfile*144,longtfiledos*144
      character DESCRC*25
      character ZN*12,SN*12
      character aok*17,CTLDOC*248,LCTLF*72
      character xfile*144,tg*1,delim*1,head*21
      character outs*124,outs248*248
      character dstmp*24,mlc_name*12
      CHARACTER LAPROB*72,LPNF*72
      CHARACTER NDNAM*12,TOPTIC*12,msg*28,doit*248
      character t16a*16,t16b*16,t16c*16
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15
      character figlbl*20,figexe*48
      character grflbl*20,grfexe*48,foldlbl*20,foldexe*48
      character tmode*8,fmttag*4,fmtexe*20
      character tedlbl*20,teditor*20
      character uname*24,pwd*84
      character ipvaction*3  ! signal that external ipv file
      character lworking*144

C For thermal bridge reporting
      character TBRIDGE*58
      dimension phrasemenu(16),phraselen(16)
      character phrasemenu*34,phraselen*42
      character fs*1
      logical havebrdge,haveedgebrdge
      integer howmanytbedges(16)
      
C Topic variables: value of zero = none, one = compact, two = verbose,
C three is very verbose
C Initial assumption is verbose for each topic that is included
C in the current model. A -1 means not applicable
      integer siteinfo,databaseinfo,contextinfo,controlinfo,netinfo
      integer plantinfo,geominfo,schedinfo,zoneextrainfo,fileinfo
      integer destination

      integer iercl  ! error state returned for scanning climate list.
      integer llbm,llbn  ! string lengths for obstructions
      integer ivz        ! index of current zone in list
      integer ins2,ins3  ! for radio buttons
      integer loutln     ! length of string returned
      integer itrunc     ! to signal truncation
      integer ipos       ! position indicator when calling arlist
      real ZD           ! depth for ground temp calc
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
      character louts*248,loutsd*248
      character lltmp*144,lguess*144,lldef*144  ! for working with EASKXORGTKF
      character lpath*72,fname*72 ! for use with fdroot
      character llclmdb*144
      integer llt,lndbp,jj

      logical newgeo  ! to use for testing if new/old geometry file.
      logical nameok  ! to use for testing if duplicate names in zone
      logical foundit ! found a zone loop was referenced
      integer NITMS   ! max items
      integer im,k,mnulen   ! multi column lines
      integer lr      ! length of model root string
      integer iglib  ! for detecting GTK or X11
      integer iitrc  ! for passing verbosity
      integer imlc   ! loop for MLC reporting
      logical odd   ! for display of vertex info
      
      INTEGER :: i,icc,icoth,ie,ieripv,irec,iprb,inv
      INTEGER :: ins4,ino,imerr,ilcot,ijk,ij,ii,igrdp
      INTEGER :: ictlf,iuo,iunit1,iunit,iuf,iub,itu,itru,itrc,itbmenu
      INTEGER :: itb,istat,isc,is,ic,ins1,isel,iz,izo,izc,izz
      INTEGER :: kk,j,l,lnb,lnc,lna,lsn10,lto,lzn10,m,igraphiclib
      INTEGER :: iw
      INTEGER :: lsna,lk,lnk,lnkn  ! counters for string lengths
      REAL :: opq,totalarea,topq,tmp,tfla
      REAL :: tareamlc,trn,ttrn,tvol,tvtr,tvtr2
      REAL :: UVD,UVI,UVU,vtranper

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

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
      siteinfo=2
      databaseinfo=2  ! assume verbose mode for database reporting
      contextinfo=2
      ICTLF=IUF
      CALL ERPFREE(ICTLF,ISTAT)
      call FINDFIL(LCTLF,XST)
      if(XST)then
        controlinfo=2  ! if control used set to verbose
      else
        controlinfo=-1
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
      if(ncomp.gt.0)then
        geominfo=2
        schedinfo=2
        zoneextrainfo=2
      else
        geominfo=-1
        schedinfo=-1
        zoneextrainfo=-1
      endif

C Assume no extra file names required and no MLC selected for filter.
      fileinfo=0
      isel = 0

C If silent mode then set file name and jump to report generation.
      if(silent)then
        destination=1
        itu = ixunit

C Create model QA report in the doc folder if possible. Use similar 
C pattern to management of lmodellog.
        lr=lnblnk(cfgroot)
        if(docpth(1:2).eq.'  '.or.docpth(1:2).eq.'./')then
          write(lmodelqa,'(2a)') cfgroot(1:lr),'.contents'
        elseif(docpth(1:3).eq.'../')then
          write(lmodelqa,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &      cfgroot(1:lr),'.contents'
        else
          write(lmodelqa,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &      cfgroot(1:lr),'.contents'
        endif
        write(xfile,'(a)') lmodelqa(1:lnblnk(lmodelqa))
        call ctldumpt(xfile,ixopen,ixloc,ixunit,'QA text',IER)
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
C and itu set so QA report is written to screen.
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
      items(6) ='  -------------------------   '
      items(7) ='  QA report                   '
      if(siteinfo.eq.0)then
        items(8)='a site info >> none'
      elseif(siteinfo.eq.1)then
        items(8)='a site info >> compact'
      elseif(siteinfo.eq.2)then
        items(8)='a site info >> verbose'
      endif
      if(databaseinfo.eq.0)then
        items(9)='b databases >> none'
      elseif(databaseinfo.eq.1)then
        items(9)='b databases >> compact'
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
        items(15)='h geometry >> not applicable'
      elseif(geominfo.eq.0)then
        items(15)='h geometry >> none'
      elseif(geominfo.eq.1)then
        items(15)='h geometry >> compact'
      elseif(geominfo.eq.2)then
        items(15)='h geometry >> verbose'
      elseif(geominfo.eq.3)then
        items(15)='h geometry >> very verbose'
      endif
      if(schedinfo.eq.-1)then
        items(16)='i operations >> none'
      elseif(schedinfo.eq.0)then
        items(16)='i operations >> none'
      elseif(schedinfo.eq.1)then
        items(16)='i operations >> compact'
      elseif(schedinfo.eq.2)then
        items(16)='i operations >> verbose'
      endif
      if(zoneextrainfo.eq.-1)then
        items(17)='j zone extras >> none'
      elseif(zoneextrainfo.eq.0)then
        items(17)='j zone extras >> none'
      elseif(zoneextrainfo.eq.1)then
        items(17)='j zone extras >> compact'
      elseif(zoneextrainfo.eq.2)then
        items(17)='j zone extras >> verbose'
      endif
      if(geominfo.ge.1)then
        items(18)='k sort by surface attrbution'
      else
        items(18)='k n/a'
      endif
      if(fileinfo.eq.0)then
        items(19)='m file names >> none'
      elseif(fileinfo.eq.1)then
        items(19)='m file names >> included'
      endif
      if(destination.eq.0)then
        items(20)='> QA report >> text feedback'
      elseif(destination.eq.1)then
        if(lmodelqa(1:4).eq.'UNKN'.or.lmodelqa(1:2).eq.'  ')then
          items(20)='> QA report >> file UNKNOWN'
        else
          write(items(20),'(2a)') '> QA report >> ',lmodelqa(1:17) 
        endif
      endif
      items(21)  ='! generate QA report       '
      if(destination.eq.0)then
        if(lmodelqa(1:4).eq.'UNKN'.or.lmodelqa(1:2).eq.'  ')then
          items(22)='  QA report not yet defined'
        else
          write(items(22),'(2a)') '* edit QA: ',lmodelqa(1:21) 
        endif
      elseif(destination.eq.1)then
        if(lmodelqa(1:4).eq.'UNKN'.or.lmodelqa(1:2).eq.'  ')then
          items(22)='  QA report not yet defined'
        else
          write(items(22),'(2a)') '* edit QA: ',lmodelqa(1:21) 
        endif
      endif
      items(23)  ='  _________________________'
      items(24)  ='? help                     '
      items(25)  ='- exit menu                '

C Help for menu.
      helptopic='setup_qa_report'
      call gethelptext(helpinsub,helptopic,nbhelp)
      NITMS=25
      call emenu('Model reports',items,NITMS,ino)

      if(ino.eq.25)then
        if(changedit)then
          continue
        endif
        return
      elseif(ino.eq.24)then
        helptopic='setup_qa_report'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('QA overview',nbhelp,'-',0,0,IER)
      elseif(ino.eq.1)then

C << need to sort out changing to main project folder >>
        if(foldexe(1:2).eq.'  ')then
          call usrmsg(
     &    'The folder browsing tool has not be defined. See your',
     &    'administrator about updating the esprc definition.','W')
        else
          write(doit,'(2a)') foldexe(1:lnblnk(foldexe)),' &'
          call usrmsg('Beginning folder view via',doit,'-')
          tmode='graph'
          call runit(doit,tmode)
        endif
      elseif(ino.eq.2)then

C Edit project documentation.
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
          call edisp(iuout,'  ')  ! echo blank line
          CALL EASKXORGTKF(lguess,
     &      'Project log file:',' ',lldef,lltmp,'log file',IER,nbhelp)
          write(lmodellog,'(a)') lguess(1:lnblnk(lguess)) ! might be changed 

C If user request jump back and re-display the menu.
          if(ier.eq.-3)then
            goto 54  ! cancel detected, restore name and redisplay menu.
          endif
          changedit=.true.

C Ask user whether file is in ../doc and if so use fdroot if GTK.
          if(iglib.eq.2)then
            dok=.true.
            CALL ASKOK('Is this file in the model ../doc folder?',
     &        ' ',OK,dok,3)
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
              dok=.true.
              CALL ASKOK('Problem detected while trying to preview',
     &                   'the log file. Try again?',OK,dok,nbhelp)
              IF(OK)GOTO 54
            ENDIF
          else
            dok=.true.
            CALL ASKOK('Problem detected while trying to open',
     &                 'log file. Try again?',OK,dok,nbhelp)
            IF(OK)GOTO 54
          endif

C Allow user to edit the job notes file. Append to path if
C necessary.  If vi then spawn a new window to do the editing.
          dok=.true.
          CALL ASKOK(' ','Edit this file (with standard text editor?',
     &      OK,dok,nbhelp)
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
            tmode='graph'
            if(teditor(1:2).eq.'  ')then
              call usrmsg(
     &      'The external word processor has not be defined. See your',
     &      'administrator about updating the esprc definition.','W')
            elseif(teditor(1:2).eq.'vi')then
              tmode='text'
            endif
            write(doit,'(a,2x,a,a)') teditor(1:lnblnk(teditor)),
     &        longtfile(1:lnblnk(longtfile)),' &'
            call usrmsg('Begining edit via',doit,'-')
            call runit(doit,tmode)
          endif
        endif
      elseif(ino.eq.3)then

C Execute graphing tool.
        if(grfexe(1:2).eq.'  ')then
          call usrmsg(
     &    'The extrn. data analysis tool has not be defined. See your',
     &    'administrator about updating the esprc definition.','W')
        else
          call tstamp('>','PRJ: start graphing tool')
          write(doit,'(2a)')grfexe(1:lnblnk(grfexe)),' &'
          call usrmsg('Beginning graphing tool via',doit,'-')
          call runit(doit,'graph')
        endif
      elseif(ino.eq.4)then

C Execute figure tool.
        if(figexe(1:2).eq.'  ')then
          call usrmsg(
     &    'The external diagram tool has not be defined. See your',
     &    'administrator about updating the esprc definition.','W')
        else
          call tstamp('>','PRJ: start xfig')
          write(doit,'(2a)')figexe(1:lnblnk(figexe)),' &'
          call usrmsg('Beginning figure tool via',doit,'-')
          call runit(doit,'graph')
        endif
      elseif(ino.eq.5)then

C Execute image viewer (get tool via prior scan of default file).
        if(fmtexe(1)(1:2).eq.'  ')then
          call usrmsg(
     &    'The external image tool has not be defined. See your',
     &    'administrator about updating the esprc definition.','W')
        else
          call tstamp('>','PRJ: start xv')
          write(doit,'(2a)')fmtexe(1)(1:lnblnk(fmtexe(1))),' &'
          call usrmsg('Beginning image tool via',doit,'-')
          call runit(doit,'graph')
        endif
      elseif(ino.eq.8)then
        siteinfo=siteinfo+1
        if(siteinfo.gt.2)siteinfo=0
      elseif(ino.eq.9)then
        databaseinfo=databaseinfo+1  ! allow up to 3
        if(databaseinfo.gt.3)databaseinfo=0
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
          CALL EPICKS(INPIC,IVALS,' ',' Which zones to summarise:',
     &        12,NCOMP,zname,' zone list',IER,nbhelp)
        endif
      elseif(ino.eq.15)then
        if(geominfo.ne.-1)then
          geominfo=geominfo+1
          if(geominfo.gt.3)geominfo=0
        endif
      elseif(ino.eq.16)then
        if(schedinfo.ne.-1)then
          schedinfo=schedinfo+1
          if(schedinfo.gt.2)schedinfo=0
        endif
      elseif(ino.eq.17)then
        if(zoneextrainfo.ne.-1)then
          zoneextrainfo=zoneextrainfo+1
          if(zoneextrainfo.gt.2)zoneextrainfo=0
        endif
      elseif(ino.eq.18)then

C Jump if no zones.
        if(inpic.eq.0) goto 3

C Generate report sorted by surface attributes.
        helptopic='qa_filters'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('global search by attribute',nbhelp,'-',0,0,IER)

        CALL EASKABCD(' Filter for surface composition:',' ',
     &    'any construction','those marked UNKNOWN',
     &    'specific construction','cancel',ins4,nbhelp)
        if(ins4.eq.4)then
          return
        endif

        totalarea=0.0
        CALL EASKABCD(' First filter--surface opacity:',' ',
     &    'opaque','transparent','both','cancel',ins1,nbhelp)
        if(ins1.eq.4)return

        ins2=1
        call EASKATOG(' Second filter--surface contiguity:',' ',
     &      'external','similar','constant','partitions','ground',
     &      'adiabatic','any contiguity',ins2,nbhelp)

        ins3=1
        call EASKATOG(' Third filter--surface orientation:',' ',
     &      'vertical','sloped','horizontal up','horizontal down',
     &      'any horizontal','any orientation',' ',ins3,nbhelp)


        call tstamp('>','PRJ: global reporting by attribution')
        write(outs,'(A,A)') 'cfg: ',LCFGF(1:lnblnk(LCFGF))
        call edisp(itu,outs)
        write(outs,'(A,A)') 'model: ',modeltitle(1:lnblnk(modeltitle))
        call edisp(itu,outs)
        call dstamp(dstmp)
        write(outs,'(A,A)') 'print date: ',dstmp
        call edisp(itu,outs)
        call edisp(itu,' ')
        if(ins4.eq.1)then
          call edisp(itu,' Filtering for ANY surface composition.')
        elseif(ins4.eq.2)then
          call edisp(itu,' Filtering for UNKNOWN surface composition.')
        elseif(ins4.eq.3)then
          write(outs,'(3a)') ' Filtering for ',mlcname(ISEL)(1:12),
     &      ' composition.'
          call edisp(itu,outs)
        endif
        if(ins1.eq.1)then
          call edisp(itu,' Filtering for opaque surfaces.')
        elseif(ins1.eq.2)then
          call edisp(itu,' Filtering for transparent surfaces.')
        elseif(ins1.eq.3)then
          call edisp(itu,' Filtering for opaque or transparent.')
        endif 
        if(ins2.eq.1)then
          call edisp(itu,' Filtering for exterior connections.')
        elseif(ins2.eq.2)then
          call edisp(itu,' Filtering for similar connections.')
        elseif(ins2.eq.3)then
          call edisp(itu,' Filtering for constant connections.')
        elseif(ins2.eq.4)then
          call edisp(itu,' Filtering for internal partitions.')
        elseif(ins2.eq.5)then
          call edisp(itu,' Filtering for ground connections.')
        elseif(ins2.eq.6)then
          call edisp(itu,' Filtering for adiabatic connections.')
        elseif(ins2.eq.7)then
          call edisp(itu,' Filtering for any connections type.')
        endif 
        if(ins3.eq.1)then
          call edisp(itu,' Filtering for vertical orientation.')
        elseif(ins3.eq.2)then
          call edisp(itu,' Filtering for sloped orientation.')
        elseif(ins3.eq.3)then
          call edisp(itu,' Filtering for horizontal facing up.')
        elseif(ins3.eq.4)then
          call edisp(itu,' Filtering for horizontal facing down.')
        elseif(ins3.eq.5)then
          call edisp(itu,' Filtering for horizontal orientation.')
        elseif(ins3.eq.6)then
          call edisp(itu,' Filtering for any orientation.')
        endif 
        call edisp(itu,' ')

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
              if(ins1.eq.1.and.SSOTF(icc)(1:4).ne.'OPAQ')apply=.false.
              if(ins1.eq.2.and.SSOTF(icc)(1:4).ne.'TRAN')apply=.false.

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
              if(ins3.eq.1.and.SSVFC(icc)(1:4).ne.'VERT')then
                apply=.false.
              elseif(ins3.eq.2.and.SSVFC(icc)(1:4).ne.'SLOP')then
                apply=.false.
              elseif(ins3.eq.3.and.SSVFC(icc)(1:4).ne.'CEIL')then
                apply=.false.
              elseif(ins3.eq.4.and.SSVFC(icc)(1:4).ne.'FLOR')then
                apply=.false.
              elseif((ins3.eq.5.and.SSVFC(icc)(1:4).eq.'FLOR').or.
     &               (ins3.eq.5.and.SSVFC(icc)(1:4).eq.'CEIL'))then
                continue
              elseif(ins3.eq.6)then
                continue
              endif
              if(ins4.eq.1)then
                continue
              elseif(ins4.eq.2)then
                if(SSMLCN(icc)(1:4).ne.'UNKN')apply=.false.
              elseif(ins4.eq.3)then
                if(SSMLCN(icc)(1:4).eq.'UNKN')apply=.false.
                if(SSMLCN(icc)(1:12).ne.mlcname(ISEL)(1:12))then
                  apply=.false.
                endif
              endif
              
              if(apply)then
                write(sn,'(a)') SSNAME(icc)
                lsn10=MIN0(lnblnk(sn),10)
                lzn10=MIN0(lnblnk(zn),10)
                write(head,'(3a)') zn(1:lzn10),':',sn(1:lsn10)
                WRITE(outs,'(a,1X,F7.2,F6.1,F6.1,1X,A4,
     &            1X,A4,1X,A12,1X,A13)') head,SSNA(icc),SSPAZI(icc),
     &            SSPELV(icc),SSOTF(icc),SSVFC(icc),SSMLCN(icc),
     &            SSOTHER(icc,1)
                call edisp(itu,outs)
                totalarea = totalarea + SSNA(icc)
              endif
 11         continue
          endif
  95    continue
        write(outs,'(a,f10.2)') 'Area of matching surfaces: ',totalarea
        call edisp(itu,outs)

C If writing to file call ctlexp to close it and switch back to text feedback.
        if(destination.eq.1)then
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','geom text',IER)
          destination=0
        endif
        MODIFY=.TRUE.
        MODBND=.TRUE.
      elseif(ino.eq.19)then
        fileinfo=fileinfo+1
        if(fileinfo.gt.1)fileinfo=0
      elseif(ino.eq.20)then

C Setup the name of the model contents report. If name already known
C then use it in dialog, otherwise make up standard name in the doc
C folder if it exists.
        if(destination.eq.0)then
          destination = 1
          itu = ixunit
          if(lmodelqa(1:4).eq.'UNKN'.or.lmodelqa(1:2).eq.'  ')then

C Create model QA report in the doc folder if possible. Use similar 
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
          call edisp(iuout,'  ')  ! echo blank line
          CALL EASKXORGTKF(lguess,'Model contents file:',
     &      ' ',lldef,lltmp,'QA file',IER,nbhelp)

C If user request jump back and re-display the menu.
          if(ier.eq.-3)then
            goto 3  ! cancel detected, restore name and redisplay menu.
          endif

C Ask user whether file is in ../doc and if so use fdroot if GTK.
          if(iglib.eq.2)then
            dok=.true.
            CALL ASKOK('Is this file in the model ../doc folder?',
     &        ' ',OK,dok,3)
            if(OK)then
              call fdroot(lltmp,lpath,fname)
              write(lmodelqa,'(3a)') docpth(1:lnblnk(docpth)),fs,
     &          fname(1:lnblnk(fname))
            endif
          else
            write(lmodelqa,'(a)') lltmp(1:lnblnk(lltmp))
          endif

C Debug.
C          write(6,*) 'lguess ',lguess
C          write(6,*) 'lmodelqa ',lmodelqa
C          write(6,*) 'lltmp ',lltmp

C Save the model configuration file to register the report.
          CALL EMKCFG('-',IER)

C Copy lmodelqa to xfile so that ctlexp can deal with it.
          write(xfile,'(2a)')lmodelqa(1:lnblnk(lmodelqa))

C          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','QA text',IER)
          call ctldumpt(xfile,ixopen,ixloc,ixunit,'QA text',IER)
          if(ier.eq.-3)then
            destination = 0
            itu = iuout
          endif
        elseif(destination.eq.1)then
          destination = 0
          itu = iuout
        endif

      elseif(ino.eq.21)then

C Generate the report based on current options.
        goto 55

      elseif(ino.eq.22)then

C If user just wrote to a file then allow browse and then
C edit of that QA file.  Use file browser if in GTK mode.
C If vi then spawn a new window to do the editing in.
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
        call edisp(iuout,'  ')  ! echo blank line
        CALL EASKXORGTKF(lguess,'Model contents file:',
     &    ' ',lldef,lltmp,'QA file',IER,nbhelp)

C If user request jump back and re-display the menu.
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
        tmode='graph'
        if(teditor(1:2).eq.'vi')tmode='text'
        write(doit,'(a,2x,a,a)') teditor(1:lnblnk(teditor)),
     &    longtfile(1:lnblnk(longtfile)),' &'
        call runit(doit,tmode)

      else

C Not one of the standard choices.
        goto 3
      endif
      goto 3

C Generate the QA report.
  55  call tstamp('>','PRJ: zone summary')
      call dstamp(dstmp)
      call edisp(itu,'Synopsis')
      call edisp(itu,' ')
      write(outs248,'(9A)') 'Synopsis of the model ',
     &  modeltitle(1:lnblnk(modeltitle)),' defined in ',
     &  LCFGF(1:lnblnk(LCFGF)),' generated on ',
     &  dstmp,'. Notes associated with the model',
     &  ' are in ',lmodellog(1:lnblnk(lmodellog))
      call edisp248(itu,outs248,80)
      call edisp(itu,' ')

C Site information.
      if(siteinfo.gt.0)then
        write(outs248,'(a,f7.2,a,f6.2,2a,i4,5a)') 
     &    'The model is located at latitude ',sitelat,
     &    ' with a longitude difference of ',sitelongdif,
     &    ' from the local time meridian. The year used in',
     &    ' simulations is ',IYEAR,' and weekends occur on ', 
     &     wkd1(1:lnblnk(wkd1)),' and ',wkd2(1:lnblnk(wkd2)),'.'
        call edisp248(itu,outs248,80)
        if(siteexposureindex.eq.1)then
          WRITE(outs,'(a)')'site exposure is typical city centre'
        elseif(siteexposureindex.eq.2)then
          WRITE(outs,'(a)')'site exposure is typical urban site'
        elseif(siteexposureindex.eq.3)then
          WRITE(outs,'(a)')'site exposure is typical rural site'
        elseif(siteexposureindex.eq.4)then
          WRITE(outs,'(a)')'site exposure is city: = sky, grnd, bldgs'
        elseif(siteexposureindex.eq.5)then
          WRITE(outs,'(a)')'site exposure is city: below surroundings'
        elseif(siteexposureindex.eq.6)then
          WRITE(outs,'(a)')'site exposure is isolated rural site site'
        elseif(siteexposureindex.eq.7)then
          WRITE(outs,'(a)')'site exposure is totally enclosed (no sky)'
        elseif(siteexposureindex.eq.8)then
          WRITE(outs,53)skyview,groundview,buildingview
  53      FORMAT('site exposure is sky=',F4.2,' ground=',F4.2,
     &      ' other buildings=',F4.2)
        endif
        write(outs248,'(3a,f4.2,a)') 'The ',outs(1:lnblnk(outs)),
     &    ' and the ground reflectance is ',groundrefl,'.'
        call edisp248(itu,outs248,80)
        call edisp(itu,' ')

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
            call edisp(itu,' ')
            write(outs,'(2a)')'Simulationist name: ',certifname 
            call edisp(itu,outs)     
            write(outs,'(2a)')'Simulationist telephone: ',certTel 
            call edisp(itu,outs)     
            write(outs,'(2a)')'Simulationist address: ',certaddress   
            call edisp(itu,outs)   
            write(outs,'(2a)')'Simulationist city: ',certcity  
            call edisp(itu,outs)    
            write(outs,'(2a)')'Simulationist postcode: ',certPC  
            call edisp(itu,outs)
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
            write(outs,'(2a)')'Project name: ',pjname(1:lnblnk(pjname))
            call edisp(itu,outs)
            write(outs,'(2a)')'Building address: ',baddress
            call edisp(itu,outs)
            write(outs,'(2a)')'Building city: ',bcity(1:lnblnk(bcity)) 
            call edisp(itu,outs)
            write(outs,'(2a)')'Building Postcode: ',buildingPC  
            call edisp(itu,outs)    
            write(outs,'(2a)')'Building owner name: ',ownername
            call edisp(itu,outs)   
            write(outs,'(2a)')'Building owner telephone: ',ownertel
            call edisp(itu,outs)   
            write(outs,'(2a)')'Building owner address: ',owneraddr 
            call edisp(itu,outs) 
            write(outs,'(2a)')'Building owner city: ',ownercity 
            call edisp(itu,outs) 
            write(outs,'(2a)')'Building owner Postcode: ',ownerPC
            call edisp(itu,outs)     
            call edisp(itu,' ')
            write(outs,'(2a)')'Simulationist name: ',certifname 
            call edisp(itu,outs)     
            write(outs,'(2a)')'Simulationist telephone: ',certTel 
            call edisp(itu,outs)     
            write(outs,'(2a)')'Simulationist address: ',certaddress   
            call edisp(itu,outs)   
            write(outs,'(2a)')'Simulationist city: ',certcity  
            call edisp(itu,outs)    
            write(outs,'(2a)')'Simulationist postcode: ',certPC  
            call edisp(itu,outs)
          endif          
        endif


C Setup string buffer with distribution weather folder name.
        CALL ERPFREE(ICLIM,ISTAT)
        llt=lnblnk(LCLIM)
        lndbp=lnblnk(standardclmpath)
        if(ipathclim.eq.0.or.ipathclim.eq.1)then
          llclmdb=LCLIM
        elseif(ipathclim.eq.2)then
          write(llclmdb,'(3a)') standardclmpath(1:lndbp),fs,
     &      LCLIM(1:llt)
        endif

        call EFOPRAN(ICLIM,llclmdb,144,0,IER)
        IREC=367
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLMLOC

C Tell about climate taking note of timing of solar data.
        if(iSlr_half_hr_flg.eq.0)then
          WRITE(outs248,'(5A)')'The climate is: ',
     &      CLMLOC(1:lnblnk(CLMLOC)),' and is held in: ',
     &      LCLIM(1:lnblnk(LCLIM)),
     &      ' with hour centred solar data.'
        else
          WRITE(outs248,'(5A)')'The climate is: ',
     &      CLMLOC(1:lnblnk(CLMLOC)),' and is held in: ',
     &      LCLIM(1:lnblnk(LCLIM)),
     &      ' with half hour centred solar data.'
        endif
        call edisp248(itu,outs248,80)

        if(ipathclim.eq.0.or.ipathclim.eq.1)then
          WRITE(outs248,'(2A)')' annual weather         : ',
     &      LCLIM(1:lnblnk(LCLIM))
          call edisp248(itu,outs248,100)
        elseif(ipathclim.eq.2)then
          WRITE(outs248,'(2A)')' standard annual weather: ',
     &      LCLIM(1:lnblnk(LCLIM))
          call edisp248(itu,outs248,100)
        endif

C List out the estimated ground temperatures based on the
C current weather data at 0.5m, 1m, 2m, 4m depth.
C First get ground temperatures at depth ZD.
        if(siteinfo.ge.2)then
          ZD=0.5
          CALL GTCALC(ZD,'-',IER)
          itrunc=1
          ipos=1
          do while (itrunc.ne.0)
            call arlist(ipos,12,eGrdTp,12,'S',outs248,loutln,itrunc)
            call edisp(itu,'Calculated ground temp at 0.5m depth')
            call edisp248(itu,outs248,90)
            ipos=itrunc+1
          end do
          ZD=1.0
          CALL GTCALC(ZD,'-',IER)
          itrunc=1
          ipos=1
          do while (itrunc.ne.0)
            call arlist(ipos,12,eGrdTp,12,'S',outs248,loutln,itrunc)
            call edisp(itu,'Calculated ground temp at 1.0m depth')
            call edisp248(itu,outs248,90)
            ipos=itrunc+1
          end do
          ZD=2.0
          CALL GTCALC(ZD,'-',IER)
          itrunc=1
          ipos=1
          do while (itrunc.ne.0)
            call arlist(ipos,12,eGrdTp,12,'S',outs248,loutln,itrunc)
            call edisp(itu,'Calculated ground temp at 2.0m depth')
            call edisp248(itu,outs248,90)
            ipos=itrunc+1
          end do
          ZD=4.0
          CALL GTCALC(ZD,'-',IER)
          itrunc=1
          ipos=1
          do while (itrunc.ne.0)
            call arlist(ipos,12,eGrdTp,12,'S',outs248,loutln,itrunc)
            call edisp(itu,'Calculated ground temp at 4.0m depth')
            call edisp248(itu,outs248,90)
            ipos=itrunc+1
          end do
          call edisp(itu,' ')
        endif

C List out the maximum and minimum thickness of constructions in
C contact with the ground.
C << to be done >>

C List out the minimum and maximum thickness of the facade (walls
C marked as WALL surface use).
C << to be done >>

        if(NGRDP.gt.0.and.siteinfo.ge.1)then
          call edisp(itu,' ')  
          write(outs,'(a,i2,a)')'There are currently ',NGRDP,
     &      ' user defined ground temperature profiles.'      
          call edisp(itu,outs)
        endif
      endif

C Jump back point if err=1000 called when reading climate.
 1001 if(siteinfo.eq.2)then
        if(NGRDP.gt.0)then
          do 22 igrdp=1,NGRDP    
            CALL EDISP(itu,'Ground temperatures Jan-Dec:')
            WRITE(OUTS,'(12F6.1)')(UGRDTP(J,IGRDP),J=1,12)
            call edisp(itu,outs)
 22       continue
          call edisp(itu,' ')
        endif
        if(GTGEOM(1:2).eq.'  '.or.GTGEOM(1:4).eq.'UNKN')then
          continue
        else
          WRITE(outs248,'(5A)') 'The model includes ground topology ',
     &      GTNAME(1:lnblnk(GTNAME)),' which is defined in ',
     &      GTGEOM(1:lnblnk(GTGEOM)),'.'
          call edisp248(itu,outs248,80)
          call edisp(itu,' ')
        endif
      endif

C Context of the model.
      if(contextinfo.eq.1)then
        if(ipconv.eq.1)then
          call edisp(itu,
     &'Primary energy conversions (demand kWhr to primary kWhr) used.')
        endif
      elseif(contextinfo.eq.2)then

C If there is an AIM-2 descritption scan the file and then report.
        if(iAIM2.eq.1)then
          call AIM2_READIN
          call AIM2_report(itu)
        endif

        if(ipconv.eq.1)then
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
          call edisp(itu,' ')
          call edisp(itu,' Emissions (g/kWh) co2  nox  sox  ')
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

C Check if IPV file and dispersed demands files are known.
      if(contextinfo.gt.0)then
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
     &'The model includes dispersed casual gains (e.g. pumps & fans).')
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
          call edisp(itu,
     &      'An Integrated Performance View has been defined:')
          if(contextinfo.eq.1)then
            WRITE(outs,'(2A)')'Title   : ',ipvtitl(1:lnblnk(ipvtitl))
            call edisp(itu,outs)
            WRITE(outs,'(2A)')'Version : ',ipvvers(1:lnblnk(ipvvers))
            call edisp(itu,outs)
            call edisp(itu,'Synopsis:')
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
                call scancdblist(IUF,llclmdb,ok,iercl)
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
     &              'The climate used was not in the climatelist and')
                  call edisp(itu,
     &              'no local seasons set - default seasons assumed.')
                  call listipvdat(itu,'a',ier)
                endif
              endif
            endif
          endif
        else
          call edisp(itu,
     &     'An Integrated Performance View is incomplete or missing.')
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
            call edisp(itu,'  ________________________________')
            call edisp(itu,'  |temporal    |generic |associated')
            call edisp(itu,
     &           '  |entity name |type    |with   zone &     surface ')
            do 28, l=1,ITEMSTD
              WRITE(outs,29)l,DBTAG(L),DBTASK(L),DBZN(L),DBSN(L)
   29         FORMAT(i2,1X,A12,1X,A8,5X,A16,A16)
              call edisp(itu,outs)
  28        continue
            call edisp(itu,' ')
          endif
        endif
      endif

C Databases associated with the model. First print the file names.
      if(databaseinfo.gt.0)then
        call edisp(itu,' ')
        call edisp(itu,'Databases associated with the model: ')
        if(ipathapres.eq.0.or.ipathapres.eq.1)then
          WRITE(outs248,'(2A)')' pressure distributions : ',
     &      LAPRES(1:lnblnk(LAPRES))
          call edisp248(itu,outs248,100)
        elseif(ipathapres.eq.2)then
          WRITE(outs248,'(2A)')' standard pressure distr: ',
     &      LAPRES(1:lnblnk(LAPRES))
          call edisp248(itu,outs248,100)
        endif

        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          WRITE(outs248,'(2A)')' materials              : ',
     &      LFMAT(1:lnblnk(LFMAT))
          call edisp248(itu,outs248,100)
        elseif(ipathmat.eq.2)then
          WRITE(outs248,'(2A)')' standard materials     : ',
     &      LFMAT(1:lnblnk(LFMAT))
          call edisp248(itu,outs248,100)
        endif

        if(ipathmul.eq.0.or.ipathmul.eq.1)then
          WRITE(outs248,'(2A)')' constructions          : ',
     &      LFMUL(1:lnblnk(LFMUL))
          call edisp248(itu,outs248,100)
        elseif(ipathmul.eq.2)then
          WRITE(outs248,'(2A)')' standard constructions : ',
     &      LFMUL(1:lnblnk(LFMUL))
          call edisp248(itu,outs248,100)
        endif

        if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
          WRITE(outs248,'(2A)')' plant components       : ',
     &      LPCDB(1:lnblnk(LPCDB))
          call edisp248(itu,outs248,100)
        elseif(ipathpcdb.eq.2)then
          WRITE(outs248,'(2A)')' standard plant comp    : ',
     &      LPCDB(1:lnblnk(LPCDB))
          call edisp248(itu,outs248,100)
        endif

        if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
          WRITE(outs248,'(2A)')' event profiles         : ',
     &      LPRFDB(1:lnblnk(LPRFDB))
          call edisp248(itu,outs248,100)
        elseif(ipathprodb.eq.2)then
          WRITE(outs248,'(2A)')' standard event profiles: ',
     &      LPRFDB(1:lnblnk(LPRFDB))
          call edisp248(itu,outs248,100)
        endif

        if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
          WRITE(outs248,'(2A)')' optical properties     : ',
     &      LOPTDB(1:lnblnk(LOPTDB))
          call edisp248(itu,outs248,100)   
        elseif(ipathoptdb.eq.2)then
          WRITE(outs248,'(2A)')' standard optical prop  : ',
     &      LOPTDB(1:lnblnk(LOPTDB))
          call edisp248(itu,outs248,100)
        endif

        if(ipathsbem.eq.0.or.ipathsbem.eq.1)then
          WRITE(outs248,'(2a)')' UK NCM data            : ',
     &      LSBEM(1:lnblnk(LSBEM))
          call edisp248(itu,outs248,100)   
        elseif(ipathsbem.eq.2)then
          WRITE(outs248,'(2a)')' standard UK NCM data   : ',
     &      LSBEM(1:lnblnk(LSBEM))
          call edisp248(itu,outs248,100)   
        endif

        if(ipathmould.eq.0.or.ipathmould.eq.1)then
          WRITE(outs248,'(2A)')' mould isopleths        : ',
     &      lfmould(1:lnblnk(lfmould))
          call edisp248(itu,outs248,100)
        elseif(ipathmould.eq.2)then
          WRITE(outs248,'(2A)')' standard mould isopleth: ',
     &      lfmould(1:lnblnk(lfmould))
          call edisp248(itu,outs248,100)
        endif
      endif

C Control in the model
      if(controlinfo.gt.0)then

C List description of control and then the various zone descriptions.
C List control data.
        call edisp(itu,' ')
        WRITE(outs,240)
        call edisp(itu,outs)
        ICTLF=IUF
        CALL ERPFREE(ICTLF,ISTAT)
        call FINDFIL(LCTLF,XST)
        CALL EZCTLR(ICTLF,0,IUOUT,IER)
        if(fileinfo.eq.1)then
          call edisp(itu,' ')
          write(outs,'(3A)') 'Ideal control is defined in: ',
     &      LCTLF(1:lnblnk(LCTLF)),' as follows:'
          call edisp(itu,outs)
        else
         call edisp(itu,' ')
         call edisp(itu,'The model includes ideal controls as follows:')
        endif
        WRITE(outs,'(a)')'Control description: '
        call edisp(itu,outs)
        call edisp248(itu,ctldoc,80)

        if(NCF.gt.0)then

C If there are zone controls list the additional documentation if the
C toggle controlinfo is compact (1) if verbose (2) list only the controls
C that have been referenced and if very verbose (3) list all controls.
          call edisp(itu,' ')
          WRITE(outs,'(a,I2,a)')'Zones control includes ',
     &      NCF,' functions.'
          call edisp(itu,outs)
          call edisp248(itu,znctldoc,80)
          if(controlinfo.eq.2)then
            call edisp(itu,
     &        'Details of control loops referenced in the model:')
            do ii=1,NCF
              foundit=.false.
              do jj=1,ncomp
                if(icascf(jj).eq.ii) foundit=.true.
              enddo
              if(foundit) call LSTCNTL(itu,0,ii)
            enddo
            call LSTCNTL(itu,0,0)
          endif
          if(controlinfo.eq.3)then
            call edisp(itu,
     &        'Details of all defined control loops:')
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
            do 246 ii=1,NCC
              call LSTCNTL(itu,2,ii)
 246        continue
          endif
        endif
        if(NGF.gt.0)then
          call edisp(itu,' ')
          WRITE(outs,'(a,I2,a)')' Global control includes ',
     &      NGF,' loops.'
          call edisp(itu,outs)
          call edisp248(itu,glctldoc,80)
          if(controlinfo.ge.2)then
            do 147 ii=1,NGF
              call LSTCNTL(itu,3,ii)
 147        continue
          endif
        endif
      endif

C If there is a flow network, rescan and list its contents.
      if(netinfo.gt.0)then
        if(IAIRN.ge.1)then
          WRITE(outs,240)
          call edisp(itu,outs)
          call edisp(itu,' ')
          call edisp(itu,'The model includes an air flow network.')
          if(netinfo.eq.2)then
            call mflist(itu)
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
            call edisp(itu,' ')
            call edisp(itu,'The model includes a plant network.')
            call pltcfg(ifpnf,iunit1,itu,1)
C copy number of nodes in each plant component from node_count to
C nnodes: This is necessary because the nnodes common blocak (pcdat)
C is defined differently in bps, and the pltcfg.F file cannot be
C built as part of bps if it includes the prj pcdat common.
            call CopyPltNodeCounts()
          endif
        endif
      endif

C Jump if no zones.
      if(inpic.eq.0) goto 57

      call edisp(itu,' ')
      WRITE(outs,240)
      call edisp(itu,outs)
      call edisp(itu,' ')
      outs='ID Zone         Volume|          Surface          '
      call edisp(itu,outs)
      outs='   Name         m^3   | No. Opaque  Transp  ~Floor'
      call edisp(itu,outs)
      TVOL=0.
      TFLA=0.
      TOPQ=0.
      TTRN=0.
      do 97 IZ=1,INPIC
        if(IVALS(IZ).GT.0)then
          LTMP=LGEOM(IVALS(IZ))
          call georead(IUF,LTMP,IVALS(IZ),1,iuout,IER)
          OPQ=0.
          TRN=0.
          attribok(IVALS(IZ))=.true.
          DO 41 I=1,NZSUR(ivals(iz))
            icc=izstocn(ivals(iz),i)
            if(SSOTHER(icc,1)(1:4).eq.'UNKN')attribok(IVALS(IZ))=.false.
            if(SSMLCN(icc)(1:4).eq.'UNKN')attribok(IVALS(IZ))=.false.
            if(SSOTF(icc)(1:4).NE.'OPAQ'.AND.
     &            SSOTF(icc)(1:4).NE.'CFC ')then
              TRN=TRN+SSNA(icc)
            else
              OPQ=OPQ+SSNA(icc)
            endif
   41     CONTINUE
          if(attribok(IVALS(IZ)))then
            aok=' attributed  '
            write(outs,'(i2,1x,a,f7.1,i4,3f8.1,2x,a)') IVALS(IZ),
     &      zname(IVALS(IZ)),VOL(IVALS(IZ)),NZSUR(ivals(iz)),OPQ,TRN,
     &      ZBASEA(IVALS(IZ)),
     &      zdesc(IVALS(IZ))(1:lnzdesc(IVALS(IZ)))
          else
            aok=' part-attributed '
            write(outs,'(i2,1x,a,f7.1,i4,3f8.1,a)') IVALS(IZ),
     &      zname(IVALS(IZ)),VOL(ivals(iz)),NZSUR(ivals(iz)),OPQ,TRN,
     &      ZBASEA(IVALS(IZ)),aok
          endif
          call edisp(itu,outs)
          TVOL=TVOL+VOL(IVALS(IZ))
          TFLA=TFLA+ZBASEA(IVALS(IZ))
          TOPQ=TOPQ+OPQ
          TTRN=TTRN+TRN
        endif
  97  continue
      write(outs,'(a,f9.0,i5,3f8.0)') '   all      ',TVOL,NCON,
     &   TOPQ,TTRN,TFLA
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

        call edisp(itu,'  ')
        WRITE(outs,240)
 240    format(1x,60('_'))
        if(iz.gt.1)call edisp(itu,outs)

        call ZINFOREP(itu,IZZ)  ! not dependant on georead.

        do 111 ij=1,nzsur(izz)
          icc=izstocn(izz,ij)
          CALL SURADJ(IZZ,IJ,IE,TMP,IZC,ISC,IC,DESCRC) ! not dependant on georead

C Check for duplicate surface names in this zone. Report is
C duplicates are found.
          write(SN,'(a12)')SSNAME(icc)
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
C          write(itu,*) 'in zone ',izz,' area wall = ',areawall(IZZ)
C          write(itu,*) 'area flat roof = ',areafltroof(IZZ)
C          write(itu,*) 'area sloped roof = ',areaslproof(IZZ)
C          write(itu,*) 'area skylights = ',areaskylt(IZZ)
C          write(itu,*) 'area windows = ',areatran(IZZ)
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
            if(vexposed(IZZ).gt.0.1)then
              write(outs,'(5a)')' There is ',
     &          t16a(1:lna),'m2 of exposed surface area, ',
     &          t16b(1:lnb),'m2 of which is vertical.'
            else
              write(outs,'(3a)')' There is ',
     &          t16a(1:lna),'m2 of exposed surface area.'
            endif
            if(attribok(IVALS(IZ)))call edisp(itu,outs)
          endif
          if(areawall(IZZ).gt.0.1)then
            if(ZBASEA(IZZ).gt.0.01)then
              wallper(IZZ) = (areawall(IZZ)/ZBASEA(IZZ)) * 100.
            else
              wallper(IZZ) = 1.0
            endif
            call rel16str(wallper(IZZ),t16a,lna,ier)
            call rel16str(uavwall(IZZ),t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')' Outside walls are ',
     &        t16a(1:lna),' % of floor area & average U of ',
     &        uavwall(IZZ)/areawall(IZZ),' & UA of ',t16b(1:lnb)
            if(attribok(IVALS(IZ)))call edisp(itu,outs)
          endif
          if(areaslproof(IZZ).gt.0.1)then
            if(ZBASEA(IZZ).gt.0.01)then
              slproofper(IZZ) = (areaslproof(IZZ)/ZBASEA(IZZ))*100.
            else
              slproofper(IZZ) = 1.0
            endif
            call rel16str(slproofper(IZZ),t16a,lna,ier)
            call rel16str(uavslproof(IZZ),t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')' Sloped roof is ',
     &        t16a(1:lna),' % of floor area & average U of ',
     &        uavslproof(IZZ)/areaslproof(IZZ),' & UA of ',
     &        t16b(1:lnb)
            if(attribok(IVALS(IZ)))call edisp(itu,outs)
          endif
          if(areafltroof(IZZ).gt.0.1)then
            if(ZBASEA(IZZ).gt.0.01)then
              flatroofper(IZZ) = (areafltroof(IZZ)/ZBASEA(IZZ))*100.
            else
              flatroofper(IZZ) = 1.0
            endif
            call rel16str(flatroofper(IZZ),t16a,lna,ier)
            call rel16str(uavfltroof(IZZ),t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')' Flat roof is ',
     &        t16a(1:lna),' % of floor area & average U of ',
     &        uavfltroof(IZZ)/areafltroof(IZZ),' & UA of ',
     &        t16b(1:lnb)
            if(attribok(IVALS(IZ)))call edisp(itu,outs)
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
            write(outs,'(5a,F5.3,2a)')
     &        ' Glazing is ',t16a(1:lna),' % of floor & ',t16b(1:lnb),
     &        ' % facade with average U of ',
     &        uavgtran(IZZ)/areatran(IZZ),' & UA of ',t16c(1:lnc)
            if(attribok(IVALS(IZ)))call edisp(itu,outs)
          endif
          if(areaskylt(IZZ).gt.0.1)then
            if(ZBASEA(IZZ).gt.0.01)then
              skyper(IZZ) = (areaskylt(IZZ)/ZBASEA(IZZ)) * 100.
            else
              skyper(IZZ) = 1.0
            endif
            call rel16str(skyper(IZZ),t16a,lna,ier)
            call rel16str(uavgsky(IZZ),t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')' Skylights are ',
     &        t16a(1:lna),' % of floor area & average U of ',
     &        uavgsky(IZZ)/areaskylt(IZZ),' & UA of ',t16b(1:lnb)
            if(attribok(IVALS(IZ)))call edisp(itu,outs)
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
            write(outs,'(3a,F5.3,2a)')' Ground contact is ',
     &        t16a(1:lna),' % of floor area & average U of ',
     &        uavground(IZZ)/areaground(IZZ),' & perimeter ',
     &        t16b(1:lnb)
            if(attribok(IVALS(IZ)))call edisp(itu,outs)
          endif
        endif

C Note: surinfo not dependant on recent geometry scan.
        if(geominfo.ge.2)then
          context=.true.
          CALL SURINFO(IVALS(IZ),itu,context)
          call eclose(gversion(IZZ),1.1,0.01,newgeo)
          if(newgeo)then
            if(losspercent(izz).gt.0.0)then
              havebrdge=.true.
            else
              havebrdge=.false.
            endif
            if(nbrdg(izz).gt.0.or.havebrdge)then

C Setup phrases for the menu and for editing length and psi values.
              phrasemenu(1) ='a roof-wall                    :'
              phraselen(1)  ='roof-wall ' 
              phrasemenu(2) ='b wall-ground floor            :'
              phraselen(2)  ='wall-ground floor '
              phrasemenu(3) ='c wall-wall (convex corner)    :'
              phraselen(3)  ='wall-wall (convex corner) '
              phrasemenu(4) ='d wall-wall (concave corner)   :'
              phraselen(4)  ='wall-wall (concave corner) '
              phrasemenu(5) ='e wall-floor (not ground floor):'
              phraselen(5)  ='wall-floor (not ground floor) '
              phrasemenu(6) ='f lintel above window or door  :'
              phraselen(6)  ='lintel above window or door '
              phrasemenu(7) ='g Sill below window            :'
              phraselen(7)  ='Sill below window '
              phrasemenu(8) ='h jamb at window or door       :'
              phraselen(8)  ='jamb at window or door '
              phrasemenu(9) ='i roof-wall                    :'
              phraselen(9)  ='roof-wall-mc'
              phrasemenu(10)='j wall-ground floor            :'
              phraselen(10) ='wall-ground floor-mc'
              phrasemenu(11)='k wall-wall (corner)-          :'
              phraselen(11) ='wall-wall (corner)-mc'
              phrasemenu(12)='l wall-floor (not ground floor):'
              phraselen(12) ='wall-floor (not ground floor)-mc'
              phrasemenu(13)='m lintel above window or door  :'
              phraselen(13) ='lintel above window or door-mc'
              phrasemenu(14)='n Sill below window            :'
              phraselen(14) ='Sill below window-mc'
              phrasemenu(15)='o jamb at window or door       :'
              phraselen(15) ='jamb at window or door-mc'
              phrasemenu(16)='p user-defined                 :'
              phraselen(16) ='user-defined '
              call edisp(itu,'Thermal bridges have been defined.')
              if(nbrdg(izz).gt.0)then   
                call edisp(itu,
     &      'Junctions without metal cladding   psi (W/mK) length (m)')
              endif
              do 89 itbmenu=1,8
                if(nbrdg(izz).gt.0)then
                  do 90 itb=1,nbrdg(izz)
                    if(itbmenu.eq.ibrdg(izz,itb))then
                      WRITE(TBRIDGE,'(a,2F10.4)') 
     &                  phrasemenu(itbmenu),
     &                  psi(izz,itb),lngth(izz,itb)
                      call edisp(itu,TBRIDGE)
                    endif
  90              continue
                endif
  89          continue
              if(nbrdg(izz).gt.0)then   
                call edisp(itu,
     &      'Junctions with metal cladding      psi (W/mK) length (m)')
              endif
              do 189 itbmenu=9,16
                if(nbrdg(izz).gt.0)then   
                  do 190 itb=1,nbrdg(izz)
                    if(itbmenu.eq.ibrdg(izz,itb))then
                      WRITE(TBRIDGE,'(a,2F10.4)') 
     &                  phrasemenu(itbmenu),
     &                  psi(izz,itb),lngth(izz,itb)
                      call edisp(itu,TBRIDGE)
                    endif
 190              continue
                endif
 189          continue
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
          call edisp(itu,' Vertices ( index X Y Z) in the zone...')
          if(NZTV(izz).lt.20)then
            DO 960 I = 1,NZTV(izz)
              WRITE(outs,'(a,i3,3F12.5,a,i3)')'*vertex ',I,
     &          X(I),Y(I),Z(I)
              call edisp(itu,outs)
960         CONTINUE
          else
            MNULEN=(NZTV(izz)/2)
            DO 193 K=1,MNULEN
              WRITE(outs,'(a,i3,3f11.5,a,i3,3f11.5)') 'vertex ',k,
     &        X(k),Y(k),Z(k),'   vertex ',K+MNULEN,
     &        X(K+MNULEN),Y(K+MNULEN),Z(K+MNULEN)
              call edisp(itu,outs)
  193       CONTINUE

C Put odd vertex in right column.
            if(odd)then
              WRITE(outs,'(46x,a,i3,3f11.5)') 'vertex ',NZTV(izz),
     &          X(NZTV(izz)),Y(NZTV(izz)),Z(NZTV(izz))
              call edisp(itu,outs)
            endif 
          endif

          call edisp(itu,
     &    ' Number of vertices followed by list of associated vert')
          DO 970 I = 1,NZSUR(izz)
            icc=izstocn(izz,i)        ! find connection
            lsna=lnblnk(SSNAME(icc))  ! get length of surf name
            write(louts,'(a,42I4)') '*edges ',NVER(I),
     &        (JVN(I,J),J=1,NVER(I))
            call SDELIM(louts,loutsd,'C',IW)  ! make comma separated
            write(outs248,'(3a)') 
     &        loutsd(1:lnblnk(loutsd)),'   ',SSNAME(icc)(1:lsna)
            call edisp248(itu,outs248,100)
 970      CONTINUE

C If thermal bridges are associated with a surface then report this.
C Loop each surface in zone, and for each surface each edged and if
C ibridgeshr(connection,edge) is non-zero add to howmanytbedges array.
          if(nbrdg(izz).gt.0)then
            do 971 I = 1,NZSUR(izz)
              icc=izstocn(izz,i)        ! find connection
              lsna=lnblnk(SSNAME(icc))  ! get length of surf name
              do 973,j=1,16
                howmanytbedges(j)=0
  973         continue
              haveedgebrdge=.false.
              do 972 j=1,NVER(I)
                if(ibridgeshr(icc,j).ne.0)then
                  howmanytbedges(ibridgeshr(icc,j))=
     &              howmanytbedges(ibridgeshr(icc,j))+1
                  haveedgebrdge=.true.
                endif
 972          continue

C If at least one thermal bridge for surface report it. Keep writing
C into outs248 for each non-zero bridges.
              if(haveedgebrdge)then
                write(outs248,'(2a)') SSNAME(icc)(1:lsna),' bridges: '
                lk=lnblnk(outs248)+2
                do 974 k=1,16
                  if(howmanytbedges(k).gt.0)then
                    lnk=lnblnk(phraselen(k))
                    lnkn=lk+lnk+1
                    write(outs248(lk:lnkn),'(2a)') 
     &                phraselen(k)(1:lnk),','
                    lk=lnkn+1
                  endif
 974            continue
                call edisp248(itu,outs248,100)
              endif
 971        continue
          endif
        endif

C Check if the edges are proprly bounded. Uses G1 common.
        call ckvert(0,IVALS(IZ),bound,iub,inv,ier)
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
              if(IBCMT(IVALS(IZ),I).EQ.0)then
                continue
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
                DO 2021 KK=1,NBCTMC(IVALS(IZ),I)
                  if(NBCTT(IVALS(IZ),KK,I).eq.0)then
                    msg=' sensing total radiation'
                  elseif(NBCTT(IVALS(IZ),KK,I).eq.1)then
                    msg=' sensing ambient temperature'
                  elseif(NBCTT(IVALS(IZ),KK,I).eq.2)then
                    msg=' sensing zone temperature'
                  elseif(NBCTT(IVALS(IZ),KK,I).eq.3)then
                    msg=' sensing daylight coeff.'
                  elseif(NBCTT(IVALS(IZ),KK,I).eq.4)then
                    msg=' Lightswitch not available'
                  elseif(NBCTT(IVALS(IZ),KK,I).eq.-99)then
                    msg=' sensing time only'
                  endif
                  WRITE(OUTS,'(A,I2,A,I2,A,I2,2A,F7.2,a,5f6.3)') 
     &              ' Period ',KK,': from ',IBCST(IVALS(IZ),KK,I),
     &              ' to ',IBCFT(IVALS(IZ),KK,I),msg(1:lnblnk(msg)),
     &              ' setpoint @ ',BACTPT(IVALS(IZ),KK,I),' alt solar',
     &              (TMCT2(IVALS(IZ),I,M,KK),M=1,5)
                  CALL EDISP(itu,OUTS)
 2021           continue
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
            call edisp(itu,
     &   'Block X- Y- Z- coords DX- DY- DZ- values Orient Opacity')
            DO 9995 I=1,nbobs(ivz)
              llbm=LNBLOCKMAT(ivz,I)
              llbn=LNBLOCKNAME(ivz,I)
              if(BLOCKTYP(ivz,I)(1:4).eq.'obs '.or.
     &           BLOCKTYP(ivz,I)(1:4).eq.'obs3')then
                WRITE(OUTS,9994)I,XOB(ivz,I),YOB(ivz,I),ZOB(ivz,I),
     &            DXOB(ivz,I),DYOB(ivz,I),DZOB(ivz,I),
     &            BANGOB(ivz,I,1),BANGOB(ivz,I,2),OPOB(ivz,I),
     &            BLOCKNAME(ivz,I)(1:llbn),BLOCKMAT(ivz,I)(1:llbm)
 9994           FORMAT(I3,8F8.1,F5.2,' ',a,' ',a)
                call edisp(itu,OUTS)
              else
                WRITE(OUTS,'(i3,a,44x,3a)')I,' user defined coords ',
     &            BLOCKNAME(ivz,I)(1:llbn),' ',BLOCKMAT(ivz,I)(1:llbm)
                call edisp(itu,OUTS)
              endif
 9995       CONTINUE
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
            elseif(zoneextrainfo.eq.2)then
              call FINDFIL(LHCCO(ivz),XST)
              IF(XST)THEN
                call ehtcff(LHCCO(ivz),IUF,itu,IER)
                call listhtc(itu,ivz,IER)
              endif
            endif
          endif
        endif

C Check for mismatch in surface attributes and boundary conditions.
        call edisp(itu,' ')
        bndrysxc=.true.
        DO 1243 IS=1,NZSUR(IVALS(IZ))
          icc=IZSTOCN(IVALS(IZ),is)
          if(ICT(icc).eq.0.and.SSOTHER(icc,1)(1:5).ne.'EXTER')
     &       bndrysxc=.false.
          if(ICT(icc).eq.1.and.SSOTHER(icc,1)(1:5).ne.'SIMIL')
     &       bndrysxc=.false.
          if(ICT(icc).eq.2.and.SSOTHER(icc,1)(1:5).ne.'CONST')
     &       bndrysxc=.false.
          if(ICT(icc).eq.3.and.SSOTHER(icc,1)(1:5).eq.'CONST')
     &       bndrysxc=.false.
          if(ICT(icc).eq.3.and.SSOTHER(icc,1)(1:5).eq.'SIMIL')
     &       bndrysxc=.false.
          if(ICT(icc).eq.3.and.SSOTHER(icc,1)(1:5).eq.'EXTER')
     &       bndrysxc=.false.
          if(ICT(icc).eq.3.and.SSOTHER(icc,1)(1:5).eq.'GROUN')
     &       bndrysxc=.false.
          if(ICT(icc).eq.4.and.SSOTHER(icc,1)(1:5).ne.'GROUN')
     &       bndrysxc=.false.
          if(ICT(icc).eq.5.and.SSOTHER(icc,1)(1:5).ne.'ADIAB')
     &       bndrysxc=.false.

C While looping check that constructions of partitions are matching.
          if(ICT(icc).eq.3)then
            showother=.true.
            icoth=IZSTOCN(IC2(icc),IE2(icc))
          else
            showother=.false.
          endif
          write(SN,'(a12)')SSNAME(icc)
          if(showother.and.icoth.ne.0.and.ssmlcindex(icc).ne.0)then
            ii=ssmlcindex(icc)
            if(mlcsymetric(ii)(1:9).EQ.'SYMMETRIC')then
              continue
            elseif(mlcsymetric(ii).EQ.'NONSYMMETRIC')then

C If the current construction is nonsymmetric then it should not be
C used for a partition (and db does not have a linked MLC - inform the user.
              write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &          ' has a nonsymmetric construction ',
     &          SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),'.'
              call edisp(itu,outs)
              write(outs,'(5a)') 'It faces ',
     &          SSNAME(icoth),' which is composed of ',
     &          SSMLCN(icoth)(1:12),' (which may not match).'
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
              do 99 IZO=1,IZ
                lcot=LPROJ(IVALS(IZO))
                if(lco(1:lnblnk(lco)).eq.lcot(1:lnblnk(lcot)))then
                  if(IZO.ne.IZ)then
                    duplicate=.true.
                    ilcot=IVALS(IZO)
                    goto 101
                  endif
                endif
  99          continue
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
        do 699 ijk=1,INPIC
          if(.NOT.attribok(IVALS(IJK))) allattribok=.false.
 699    continue
        if(.NOT.allattribok)then
          call edisp(itu,'  ')
          call edisp(itu,'Project AU reporting not included because')
          call edisp(itu,'some zones are not fully attributed.')
        else
          call edisp(itu,'  ')
          WRITE(outs,240)
          call edisp(itu,outs)
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
   57 if(databaseinfo.ge.1)then
        call edisp(itu,' ')
        WRITE(outs,240)
        call edisp(itu,outs)
        call edisp(itu,' Multi-layer constructions used:')
C Debug.
C        write(6,*) 'nmlc is ',nmlc,databaseinfo
        do 342 imlc=1,nmlc
          found=.false.
          mlc_name=mlcname(imlc)(1:12)
          call mlcrefs(mlc_name,areamlc,tareamlc,found)
          if(found)then
            if(databaseinfo.eq.2)then
              iitrc=2
              call etmldb(iitrc,itu,imlc,imerr)
            endif
            write(outs,'(3a,F9.2)') ' Total area of ',
     &        mlc_name(1:lnblnk(mlc_name)),' is ',tareamlc
            call edisp(itu,outs)
          else
            if(databaseinfo.eq.3)then
              iitrc=2
              call etmldb(iitrc,itu,imlc,imerr)
              write(outs,'(3a)') ' Total area of ',
     &          mlc_name(1:lnblnk(mlc_name)),' is zero (not referenced)'
              call edisp(itu,outs)
            endif
          endif
  342   continue
      endif

C If writing to file call ctldumpt to close it and switch back to text feedback.
      if(destination.eq.1)then
        call ctldumpt(xfile,ixopen,ixloc,ixunit,'QA text',IER)
        destination=0
        if(silent)then
          continue
        else
          call edisp(iuout,
     &     'The new QA file can be edited via the `edit file` option.')
          call edisp(iuout,' ')
        endif
      endif

C If in silent model return without user intervention.
      if(silent)then
        return
      endif
      goto 3

C Error trap.
 1000 call lusrmsg('Problem scanning climate file',lclim,'W')
      goto 1001

      end

C ************ mupdate
C Mupdate checks whether current model should be updated and guides
C the user through several of the tasks. Assumes that the user is
C not browsing.

      subroutine mupdate(ier)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "schedule.h"
#include "esprdbfile.h"
#include "espriou.h"
#include "seasons.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/cctlnm/ctldoc,lctlf
      common/ishdirec/iaplic(MCOM,2),nsurfcalc(MCOM),lstsfcalc(MCOM,MS),
     &     nsurfinso(MCOM),isurfinso(MCOM,MS)
      
C IPV data.
      common/IPVF/lipvdatf
      character lipvdatf*72

C Images.
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton

      character outs*124
      CHARACTER CTLDOC*248,LCTLF*72
      character ZN*12
      character ipvaction*3  ! to instruct it to read from external IPV file
      logical ok,dok,XST,anyish,unixok
      logical newgeo  ! to use for testing if new/old geometry file.
      logical allnew  ! if true then all zones are v1.1 or newer.
      logical forceupgrade  ! if any other zone is version 1.1
      logical MODOPR ! to flag if zone operations file needs changing.
      logical opernew ! if true then all operations are current.
      logical forceoper ! force if any operation is older

C Flags noting whether casual gain periods are currently sorted.
      logical sorted,problem
      integer iuo  ! file unit for operations file.
      integer imgloop ! loop counter for images

C For dealing with climate file.
      character llclmdb*144
      character fs*1
      integer llt,lndbp

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub=' mupdate'  ! set for subroutine
      
C Check if Unix-based or DOS based.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      newgeo=.false.  ! assume older format geometry.
      sorted=.false.  ! assume casual gains not sorted.

C Loop through and find out if all zone geometry files are
C v1.1 or newer (so set igupgrade=2). If so then there is
C no need for a geometric upgrade. And if there are v1.1 geometry
C files but still some zone obstruction files then there
C is still a need for upgrading.
      allnew=.true.
      opernew=.true.
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

C If ip3ver=0 then force it to write to version 2 and if it
C ip3ver=1 then write to version 2 and if already 2 then write
C to version 21.
        if(ip3ver(iz).eq.0)then
          opernew=.false.
        elseif(ip3ver(iz).eq.1)then
          opernew=.false.
        elseif(ip3ver(iz).eq.2)then
          opernew=.false.
        endif
  775 continue

      if(opernew)then
        forceoper=.false.
      else
        forceoper=.true.   ! if some older operations do upgrade
      endif

C Check whether the climate file is associated with seasons defined
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
          call scancdblist(IUF,llclmdb,ok,iercl)
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
        do 777 iz=1,ncomp
          if(gversion(iz).gt.1.0) forceupgrade=.true.
  777   continue
        if(forceupgrade.and.igupgrade.eq.0) igupgrade=2
        if(igupgrade.eq.2) forceupgrade=.true.
      endif

      if(forceupgrade)then
        dok=.true.
        helptopic='model_upgrade_older'
        call gethelptext(helpinsub,helptopic,nbhelp)

        call askok('The current model uses an out-of-date format for',
     &    'some geometry or obstruction files. Update this?',ok,dok,
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
                    if(SSOTHER(ioc,1)(1:5).eq.'EXTER'.and.
     &                 SSOTF(ioc)(1:4).ne.'OPAQ'.and.
     &                 SSOTF(ioc)(1:4).ne.'CFC ')then
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
                    if(SSOTHER(ioc,1)(1:5).eq.'EXTER'.and.
     &                 SSOTF(ioc)(1:4).ne.'OPAQ'.and.
     &                 SSOTF(ioc)(1:4).ne.'CFC ')then
                      nsurfinso(iz)=nsurfinso(iz)+1
                      isurfinso(iz,nsurfinso(iz))=I
                    endif
                    if(SSOTHER(ioc,1)(1:5).eq.'EXTER')then
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
                  if(SSOTHER(ioc,1)(1:5).eq.'EXTER'.and.
     &               SSOTF(ioc)(1:4).ne.'OPAQ'.and.
     &               SSOTF(ioc)(1:4).ne.'CFC ')then
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
                  if(SSOTHER(ioc,1)(1:5).eq.'EXTER'.and.
     &               SSOTF(ioc)(1:4).ne.'OPAQ'.and.
     &               SSOTF(ioc)(1:4).ne.'CFC ')then
                    nsurfinso(iz)=nsurfinso(iz)+1
                    isurfinso(iz,nsurfinso(iz))=I
                  endif
                  if(SSOTHER(ioc,1)(1:5).eq.'EXTER')then
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

C The documentation in the zone construction and tmc files might
C have changed to scan and write out these files.
C << to be done >>
  776     continue
        endif
      endif

C Check for the version of the zone operations file and if it is
C an older format write to a newer format.
      if(forceoper)then
        dok=.true.
        helptopic='model_upgrade_older'
        call gethelptext(helpinsub,helptopic,nbhelp)

        call askok('The current model uses an out-of-date format for',
     &    'some zone operation files. Update this?',ok,dok,
     &     nbhelp)
        if(ok)then
          call edisp(iuout,
     &    'You might wish to update the documentation in the zone')
          call edisp(iuout,
     &    'operations (more space for air and casual gains notes).')
          do iz=1,ncomp
            IUO=IFIL+1
            call FINDFIL(LPROJ(iz),XST)
            IF(XST)THEN
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

C If ip3ver=0 then force it to write to version 2 and if it
C ip3ver=1 then write to version 2 and if already 2 then write
C to version 21.
              if(ip3ver(iz).eq.0)then
                MODOPR=.true.
                ip3ver(iz)=2
              elseif(ip3ver(iz).eq.1)then
                MODOPR=.true.
                ip3ver(iz)=2
              elseif(ip3ver(iz).eq.2)then
                MODOPR=.true.
                ip3ver(iz)=21
              endif

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
          call scancdblist(IUF,llclmdb,ok,iercl)
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
      if(icfgv.lt.4)then
        dok=.true.
        helptopic='model_upgrade_v3'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call askok('The current model uses an out-of-date format for',
     &    'the configuration file. Update this?',ok,dok,nbhelp)
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
          icfgv=4   ! set to current version.

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

C Read in and write out the control file if it exists. Why test
C for the version number?  Because older versions of ESP-r did
C not have atributed control files.
      if(icfgv.lt.3)then
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
      endif

C Offer option to update shading files.
      if(icfgv.lt.4)then
        anyish=.false.
        do 42 iz=1,ncomp
          if(ISI(iz).eq.1)then
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
      if(anyish)then
        helptopic='shading_upgrade'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC(' Update of shading options:',' ',
     &    'recalculate (silent)','recalculate (interactive)','continue',
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
          if(ISI(iz).eq.0) goto 43
          call FINDFIL(LSHAD(iz),XST)
          if(XST)then

C If user asked for silent recalculation then do each
C one in forground in an xterm with `-act update_silent`
C as the command line.
            if(IRT.eq.1)then
              call comissionish(iz,'sr ',ier)
            else
              call comissionish(iz,'ir ',ier)
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
      common/gbridge/bridgelen(MCOM,10)

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

C << possible place to use ssmlcindex >>

            do 511 ii=1,nmlc
              if(SSMLCN(icc)(1:12).eq.mlcname(ii)(1:12)) then

C Recover the ISO 6946 U values for horizontal, upwards and downwards
C flow as well as internal partition (horizontal 0.13 both sides).
C Note: ISO 6946 advises use of horizontal hc value for surfaces where
C heat flow direction is unknown. This assumption is used in
C the U and UA reporting below.
                call etmldbu(0,itu,ii,UVH,UVU,UVD,UVI,UVG)

C Debug.
C                write(6,*) mlcname(ii)(1:12),'ii UVH UVU UVD UVI UVG',
C     &            ii,UVH,UVU,UVD,UVI,UVG

              else
                continue
              endif
  511       continue

            if(IE.eq.0)then

C For surfaces connected to the outside.
              exposed(IZZ) = exposed(IZZ) + SSNA(icc)
              xexposed = xexposed + SSNA(icc)
              if(SSOTF(icc)(1:4).eq.'OPAQ')then
                if(SSVFC(icc)(1:4).eq.'VERT')then

C For vertical walls assume horizontal hc coef.
                  areawall(IZZ) = areawall(IZZ) + SSNA(icc)
                  uavwall(IZZ) = uavwall(IZZ) + (SSNA(icc) * UVH)
                  vexposed(IZZ) = vexposed(IZZ) + SSNA(icc)
                  xvexposed = xvexposed + SSNA(icc)
                  xareawall = xareawall + SSNA(icc)
                  xuavwall = xuavwall + (SSNA(icc) * UVH)
                  if(ssthick(icc).gt.xvthk) xvthk = ssthick(icc) ! keep track of thickest
                elseif(SSVFC(icc)(1:4).eq.'SLOP')then

C For sloped surfaces assume upward hc coef.
                  areaslproof(IZZ) = areaslproof(IZZ) + SSNA(icc)
                  uavslproof(IZZ) = uavslproof(IZZ) + (SSNA(icc) * UVU)
                  xareaslproof = xareaslproof + SSNA(icc)
                  xuavslproof = xuavslproof + (SSNA(icc) * UVU)
                elseif(SSVFC(icc)(1:4).eq.'CEIL')then

C For ceilings assume upward hc coef.
                  areafltroof(IZZ) = areafltroof(IZZ) + SSNA(icc)
                  uavfltroof(IZZ) = uavfltroof(IZZ) + (SSNA(icc) * UVU)
                  xareafltroof = xareafltroof + SSNA(icc)
                  xuavfltroof = xuavfltroof + (SSNA(icc) * UVU)
                else

C For floors facing outside assume downward hc coef.
                  areawall(IZZ) = areawall(IZZ) + SSNA(icc)
                  uavwall(IZZ) = uavwall(IZZ) + (SSNA(icc) * UVD)
                  xareawall = xareawall + SSNA(icc)
                  xuavwall = xuavwall + (SSNA(icc) * UVD)
                endif
              elseif(SSOTF(icc)(1:4).ne.'OPAQ'.and.
     &               SSOTF(icc)(1:4).ne.'CFC ')then
                if(SSVFC(icc)(1:4).eq.'CEIL'.or.
     &             SSVFC(icc)(1:4).eq.'SLOP')then

C Consider glazing on ceiling or sloped to be a skylight.
                  areaskylt(IZZ) = areaskylt(IZZ) + SSNA(icc)
                  uavgsky(IZZ) = uavgsky(IZZ) + (SSNA(icc) * UVU) 
                  xareaskylt = xareaskylt + SSNA(icc)
                  xuavgsky = xuavgsky + (SSNA(icc) * UVU)
                elseif(SSVFC(icc)(1:4).eq.'VERT')then

C Consider glazing on walls to be a part of facade.
                  areatran(IZZ) = areatran(IZZ) + SSNA(icc)
                  uavgtran(IZZ) = uavgtran(IZZ) + (SSNA(icc) * UVH)
                  vexposed(IZZ) = vexposed(IZZ) + SSNA(icc)
                  xvexposed = xvexposed + SSNA(icc)
                  xareatran = xareatran + SSNA(icc)
                  xuavgtran = xuavgtran + (SSNA(icc) * UVH)
                else
                  areatran(IZZ) = areatran(IZZ) + SSNA(icc)
                  uavgtran(IZZ) = uavgtran(IZZ) + (SSNA(icc) * UVD)
                  vexposed(IZZ) = vexposed(IZZ) + SSNA(icc)
                  xvexposed = xvexposed + SSNA(icc)
                  xareatran = xareatran + SSNA(icc)
                  xuavgtran = xuavgtran + (SSNA(icc) * UVD)
                endif
              else
                continue
              endif
            elseif(IE.eq.4)then

C For surfaces connected to the ground.
              xtoground = xtoground + SSNA(icc)   ! increment area to ground
              areaground(IZZ) = areaground(IZZ) + SSNA(icc) ! increment area to ground
              xuavground = xuavground + (SSNA(icc) * UVG)
              uavground(IZZ) = uavground(IZZ) + (SSNA(icc) * UVG)
              if(ssthick(icc).gt.xfndthkg) xfndthkg = ssthick(icc)
            else
              continue
            endif
          endif
 111    continue

 98   continue

      return
      end
