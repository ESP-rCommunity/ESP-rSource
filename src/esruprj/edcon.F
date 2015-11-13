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

C edcon.f provides creation and editing facilities for zone construction
C files:
C  EDCON:  Control editing/creation of zone construction file.
C  EDSCON: Edit zone surface construction attributes.
C  EDZCON: Takes the information in the MLC db, surface attributes and
C          attempts to build a zone construction file.
C  MKTWIN: Create a transparent construction file based on information
C          currently held in common blocks.
C  EDTWIN: Edit/configure transparent construction common block data.
C  EOPTKS: Edits optical properties of a transparent surface..
C MATCHDB: Takes an existing zone construction file and scans the
C          constructions database to confirm matching vaules.
C SCNTCNST: Scan model for current maximum time constant.

C ************* EDCON
C Control editing/creation of zone construction file and allow updated
C information to be saved into a new file. ITRU unit number for user
C output, IER=0 OK.
      SUBROUTINE EDCON(ITRC,ITRU,ICOMP,QUIET,IER)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G2/c20/precz.
#include "geometry.h"
#include "epara.h"
#include "sbem.h"
#include "CFC_common.h"
#include "prj3dv.h"

      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/PRECT3/NTMC,NGLAZ(MTMC)
      common/PRECT4/TOPTIC(MCOM,MTMC)
      COMMON/TMCB1/IBCMT(MCOM,MTMC)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      common/user/browse
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      LOGICAL OK,DOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL browse,savereturn

      DIMENSION VERT(35),libcmt(MTMC)

      CHARACTER*72 CFILE
      CHARACTER*72 DFILE
      CHARACTER SFILE*72,LTMP*72
      CHARACTER outs*124,VERT*48,KEY*1,head*31,ETEXT*82
      character TOPTIC*12,OPTIC*12
      LOGICAL MODGEO,MODCON,TFOUND,XST,again,QUIET
      logical newgeo  ! to use for testing if new/old geometry file.

      logical CFCFOUND !used to determine whether CFCs exist
      character*72 CFCFILE
      integer MVERT,IVERT ! max items and current menu item

C Passed parameters to cfgtogg
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

#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDCON'  ! set for subroutine

C Clear number of TMC's and layers in each before loading common data.
      NTMC=0
      DO 249 I=1,MTMC
        NGLAZ(I)=0
  249 CONTINUE
      newgeo=.false.  ! assume older format geometry.

C DFILE is the default file name for any TMC file to be created,
C UFILE is default for utility file. CFILE is default for constr file.
C CFCFILE is default for CFC file
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(DFILE,'(2a)')zname(ICOMP)(1:lnzname(ICOMP)),'.tmc'
        WRITE(CFILE,'(2a)')zname(ICOMP)(1:lnzname(ICOMP)),'.con'
        IF(INOTI.EQ.1)WRITE(DFILE,'(2A)')
     &    zname(ICOMP)(1:lnblnk(zname(ICOMP))),'_not.tmc'
        IF(INOTI.EQ.3)WRITE(DFILE,'(2A)')
     &    zname(ICOMP)(1:lnblnk(zname(ICOMP))),'_typ.tmc'
        WRITE(CFCFILE,'(2a)')zname(ICOMP)(1:lnblnk(zname(ICOMP))),
     &    '.cfc'
      else
        WRITE(DFILE,'(4a)') zonepth(1:lnblnk(zonepth)),'/',
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.tmc'
        WRITE(CFILE,'(4a)') zonepth(1:lnblnk(zonepth)),'/',
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.con'
        IF(INOTI.EQ.1)WRITE(DFILE,'(4a)')zonepth(1:lnblnk(zonepth)),'/',
     &    zname(ICOMP)(1:lnblnk(zname(ICOMP))),'_not.tmc'
        IF(INOTI.EQ.3)WRITE(DFILE,'(4a)')zonepth(1:lnblnk(zonepth)),'/',
     &    zname(ICOMP)(1:lnblnk(zname(ICOMP))),'_typ.tmc'
        WRITE(CFCFILE,'(4a)') zonepth(1:lnblnk(zonepth)),'/',
     &    zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.cfc'
      endif
      IUF=IFIL+2

C Set existance of a zone construction file to false.
      MODGEO=.FALSE.
      MODCON=.FALSE.

      WRITE(outs,248)LGEOM(ICOMP)(1:LNBLNK(LGEOM(ICOMP)))
  248 FORMAT(' Scanning : ',A)
      if(itrc.gt.1) CALL USRMSG(' ',outs,'-')

C Read in the related geometry file into common.
      call georead(IFIL+1,LGEOM(ICOMP),ICOMP,1,IUOUT,IER)
      IF(IER.NE.0)THEN
        CALL USRMSG(' ',' Problem detected in geometry... ','W')
        RETURN
      ENDIF

C Scan through the geometry file and if more than 3 construction
C attributes have not been set then advise the user to use the
C geometry facility first.
      ig=0
      DO 11 I=1,NZSUR(icomp)
        icn1=izstocn(icomp,i)
        if(icn1.eq.0)then
          ig=ig+1
        else
          if(SSMLCN(icn1)(1:4).EQ.'UNKN')ig=ig+1
        endif
  11  CONTINUE
      if(ig.gt.3)then
        helptopic='many_not_attributed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('unknowns atrib in constr',nbhelp,'-',0,0,IER)
        CALL EASKAB(outs,'Options:',
     &    'supply construction attributes now',
     &    'cancel',IWA,nbhelp)
        if(IWA.EQ.1)then
          continue
        else
          return
        endif
      endif

C On entry display the current zone. Reset all surface lines to std.
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      MODIFYVIEW=.TRUE.
      MODLEN=.TRUE.
      MODBND=.TRUE.
      again=.false.
      if(.NOT.QUIET)then
        CALL ESCZONE(ICOMP)
        CALL BNDOBJ(0,IER)
        CALL ERCZONE(ICOMP)
        CALL INLNST(1)
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL ADJVIEW(IER)
      endif

C Read construction file into common.
      helptopic='pre_zone_constructions'
      call gethelptext(helpinsub,helptopic,nbhelp)
  42  CONTINUE
      IF(again.or.LTHRM(ICOMP)(1:1).EQ.' '.OR.
     &            LTHRM(ICOMP)(1:7).EQ.'UNKNOWN')THEN

C Construction file name not yet specified.
        LTMP=CFILE
        CALL EASKS(LTMP,' Zone construction file? ',' ',
     &    72,CFILE,'construction file',IER,nbhelp)
        IF(IER.NE.0)GOTO 42
        IF(LTMP.NE.' ')THEN
          LTHRM(ICOMP)=LTMP
        ELSE
          again=.true.
          GOTO 42
        ENDIF
      ENDIF

      call FINDFIL(LTHRM(ICOMP),XST)
      if(XST)then
        IF(QUIET) THEN
          CALL EDZCON(ICOMP,MODGEO,MODCON,IER)
        ELSE
          write(outs,'(3a)') 'Found an existing constructions file: ',
     &            LTHRM(ICOMP)(1:lnblnk(LTHRM(ICOMP))),'.'
          CALL EASKABC(outs,'Options:',
     &     'use it','recreate via geometry attributes',
     &     'cancel',IWA,nbhelp)
          IF(IWA.EQ.1)THEN
            call tstamp('>','PRJ: use existing zone constructions')
            CALL ECONST(LTHRM(ICOMP),IFIL+1,ICOMP,0,IUOUT,IER)
            if(ITW(ICOMP).eq.1)then
              CALL ERTWIN(ITRC,IUOUT,IUF,LTWIN(ICOMP),ICOMP,IER)
            endif
          elseif(IWA.EQ.2)then
            call tstamp('>','PRJ: scan db & rebuild zone construction')
            CALL EDZCON(ICOMP,MODGEO,MODCON,IER)
          elseif(IWA.eq.3)then
            call tstamp('>','PRJ: abandoned rebuild zone construction')
            return
          ENDIF
        ENDIF
      else

C If in quiet mode: the file should exist => error if we are here.
        IF (QUIET) THEN
          CALL PHELPD('file error',nbhelp,'-',0,0,IER)
          RETURN
        ELSE
          write(outs,'(1x,A,A)')LTHRM(ICOMP)(:lnblnk(LTHRM(ICOMP))),
     &        ' not found!'
          CALL EASKAB(outs,'Options:','respecify',
     &        'create using this name',IWB,nbhelp)
          if(IWB.eq.1)then
            again=.true.
            goto 42
          endif
        ENDIF
        CALL EDZCON(ICOMP,MODGEO,MODCON,IER)
      endif
      if(IER.NE.0)then
        again=.true.
        GOTO 42
      endif

C Having done so check to see if there are any transparent surfaces.
C If surface attribute other than OPAQUE reset ITMCFL otherwise reconcile mismatches!
C Do not check for UK NCM models.
      TFOUND=.FALSE.
      DO 60 IS=1,NZSUR(icomp)
        icn1=izstocn(icomp,is)

C Debug.
C        write(6,*) icn1,SSOTF(icn1),ITMCFL(ICOMP,IS),inoti

        IF((SSOTF(icn1)(1:4).EQ.'OPAQ').AND.ITMCFL(ICOMP,IS).ne.0)then
          if(inoti.eq.0)then
            write(outs,'(A,A)')SSNAME(icn1),' is opaque.'
            dok=.true.
            helptopic='confirm_surf_opaque'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL ASKOK(outs,'Confirm surface is opaque?',
     &        OK,DOK,nbhelp)
            if(OK)then
              ITMCFL(ICOMP,IS)=0
              SSOTF(icn1)='OPAQUE'
            endif
          else
            continue
          endif
        endif
        IF(SSOTF(icn1)(1:4).NE.'OPAQ'.AND.
     &     SSOTF(icn1)(1:3).NE.'CFC')TFOUND=.TRUE.
        IF(ITMCFL(ICOMP,IS).GT.0)TFOUND=.TRUE.
  60  CONTINUE
      IF(.NOT.TFOUND)THEN
        IF(ITW(ICOMP).EQ.1)TFOUND=.TRUE.
      ENDIF

C If transparent surfaces, proceed to read existing data or create
C from the surface attributes.
      IF(TFOUND)THEN
        if(ITW(ICOMP).ne.1)then
          ITW(ICOMP)=1
          LTWIN(ICOMP)=DFILE
          ltmp=LTWIN(ICOMP)
          IF(.NOT.QUIET)CALL EASKS(ltmp,'TMC file name?',
     &      ' ',72,DFILE,'TMC file name',IER,nbehlp)
          if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
            LTWIN(ICOMP)=ltmp
          endif
          CALL EMKCFG('-',IER)
        endif

        if(IWA.eq.2.or.IWB.eq.2.or.QUIET)then

C Request for tmc data from scratch. If there might be blind control
C read in existing tmc file before recreating. Remember the number
C of blind controls associated with each tmc and restore after
C call to EDTWIN.
          call FINDFIL(LTWIN(ICOMP),XST)
          if(XST)then
            CALL ERTWIN(ITRC,IUOUT,IUF,LTWIN(ICOMP),ICOMP,IER)
            do 444 ix=1,ntmc
              libcmt(ix)=IBCMT(ICOMP,ix)
  444       continue
            NTMC=0
          endif
          CALL EDTWIN(ITRC,ITRU,ICOMP,IER)
          do 445 ix=1,ntmc
            IBCMT(ICOMP,ix)=libcmt(ix)
  445     continue
        else

C If TMC file exists read it, otherwise create one.
          call FINDFIL(LTWIN(ICOMP),XST)
          if(XST)then
            CALL ERTWIN(ITRC,IUOUT,IUF,LTWIN(ICOMP),ICOMP,IER)
            ITW(ICOMP)=1
          else
            CALL EDTWIN(ITRC,ITRU,ICOMP,IER)
            ITW(ICOMP)=1
          endif
        ENDIF
      ENDIF

C---------create cfc file
C Now, check to see if there are any CFC surfaces and assemble ICFCFL index.
      CFCFOUND=.FALSE.
      DO 611 IS=1,NZSUR(icomp)
        icn1=izstocn(icomp,is)
        if(SSOTF(icn1)(1:3).EQ.'CFC')then
            CFCFOUND=.TRUE.
            icfcfl(icomp,is)=1 !assign 1 by default
        else
            icfcfl(icomp,is)=0
        end if
 611  CONTINUE

C If CFC surfaces, proceed to create a *.cfc file
C At this stage only the CFC index 'icfcfl' is written. The rest
C of the *.cfc file is assembled in subroutine 'makeCFCfile'.
      call FINDFIL(lcfcin(ICOMP),XST)

      IF(CFCFOUND)THEN
        helptopic='complex_fen_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(icfc(icomp).ne.1.or.(.NOT.XST))then
          icfc(icomp)=1
          lcfcin(icomp)=CFCFILE
          ltmp=lcfcin(icomp)
          CALL EASKS(ltmp,'CFC file name?',
     &      ' ',72,CFCFILE,'CFC file name',IER,nbehlp)
          if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
            lcfcin(icomp)=ltmp
          endif
          CALL PHELPD('CFC instructions',nbhelp,'-',0,0,IER)

          CALL EMKCFG('-',IER)

          CALL makeCFCfile(ICOMP,IER)
          IF(IER.EQ.1)THEN
            icfc(icomp)=0
            CALL EMKCFG('-',IER)
            RETURN
          ENDIF
       endif
      ENDIF
C -------------------

C If in quiet mode, save data and then return without user intervention.
      if(QUIET)then
        savereturn=.false.
        goto 43
      endif

C Present menu with surfaces and their associated composite
C construction names.  The user can then select each and
C provide the proper attributes. Remember to save this new
C stuff back into the geometry file. If one or more of the
C surfaces is "unknown" then pop-up a message to inform the
C user that there should be rectified before merging details
C into the model description.
  777 MHEAD=1
      MCTL=7
      ILEN=NZSUR(icomp)
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
          if(ITMCFL(ICOMP,L).GT.0)then
            optic=TOPTIC(ICOMP,ITMCFL(ICOMP,L))
          else
            optic=' - '
          endif
          icn1=izstocn(icomp,L)
          WRITE(VERT(M),14)KEY,SSNAME(icn1),SSOTF(icn1)(1:4),
     &      SSMLCN(icn1)(1:12),OPTIC
   14     FORMAT(A1,1X,A12,1X,A4,1X,A12,1X,A12)
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  _____________________________   '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM
   15   FORMAT   ('0 Page ---- Part: ',I2,' of ',I2,' -----')
      ENDIF
      VERT(1)    ='  Surface      Type  Composition  Optics'
      VERT(M+2)  ='1 list construction details             '
      VERT(M+3)  ='2 transparent layer properties          '
      VERT(M+4)  ='3 linear thermal conductivity           '
      VERT(M+5)  ='> save construction data                '
      VERT(M+6)  ='? help                                  '
      VERT(M+7)  ='- exit this menu                        '

C If editing the constructions and user has asked whether to save or
C has resized the display, redraw.
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(IER)
      call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &             iicfgz,iicfgn,iicfgc,iicfgdfn)
      if(MMOD.EQ.8)then
        call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &               iicfgz,iicfgn,iicfgc,iicfgdfn)
        WRITE(etext,'(2A)')'Model: ',modeltitle(1:lnblnk(modeltitle))
        iside=1
        isize=1
        ifont=2
        call viewtext(etext,iside,isize,ifont)
      endif

C Help text for this menu.
      helptopic='zone_constructions_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      write(head,'(3a)') 'Composition of `',
     &  zname(ICOMP)(1:lnzname(ICOMP)),'`'
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        if(browse)return
        if(MODGEO.OR.MODCON)then
          dok=.true.
          helptopic='attribute_recent_change'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK('Save changes in surface attributes or',
     &    ' constructions before exiting?',OK,dok,nbhelp)
          if(.NOT.OK)RETURN
          savereturn=.false.
          goto 43
        else
          ITORG=0
          ITGRD=0
          return
        endif
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C If no existing file advise user to create from info in geom file and
C various databases.
        helptopic='zone_constructions_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('zone geom file section',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN
C Save data and then return to menu.
         savereturn=.true.
         goto 43
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Linear thermal conductivity.
        helptopic='zone_linear_cond'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('linear properties',nbhelp,'-',0,0,IER)
        call tstamp('>','PRJ: enter linear properties')
        CALL LKMENU(ICOMP)
        GOTO 777
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C TMC details, check that something transparent first.
        TFOUND=.FALSE.
        DO 71 IS=1,NZSUR(icomp)
          icn1=izstocn(icomp,is)
          IF(SSOTF(icn1)(1:4).NE.'OPAQ'.AND.
     &      SSOTF(icn1)(1:3).NE.'CFC')TFOUND=.TRUE.
  71    CONTINUE
        IF(TFOUND)THEN
          CALL EPMENSV
          CALL EDTWIN(ITRC,IUOUT,ICOMP,IER)
          CALL EPMENRC
        else
          call usrmsg('Nothing marked as `TRAN` Please',
     &                'attribute before editing details.','W')
        endif
      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C Browse.
        call tstamp('>','PRJ: list zone constructions')
        CALL CONINF(ICOMP,0,ITRU)
      ELSEIF(IVERT.EQ.(MVERT-6))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Decode from the potentially long list to the surface number via KEYIND.
C Produce a menu of data related to this surface.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        IS=IFOC
        CALL EPMENSV
        call tstamp('>','PRJ: enter surface construction')
        CALL EDSCON(ITRU,ICOMP,IS,MODGEO,MODCON,IER)
        CALL EPMENRC
      ELSE

C Not one of the legal menu choices.
        GOTO 92
      ENDIF
      GOTO 92

  43  continue

C Save...
      helptopic='zone_constructions_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)
  88  IF(LTHRM(ICOMP)(1:2).EQ.'  '.OR.
     &   LTHRM(ICOMP)(1:7).EQ.'UNKNOWN')THEN

C Ask for name of file to put common block information into.
        SFILE=' '
        CALL EASKS(SFILE,' New construction file? ',
     &    ' ',72,CFILE,'related constr file name',IER,nbhelp)
        IF(SFILE.NE.' ')LTHRM(ICOMP)=SFILE
      ENDIF

C Save current information into a descriptive file
C and save the system configuration file if not browsing.
      if(cfgok.and.(.NOT.browse))then
        IUF=IFIL+2
        call tstamp('>','PRJ: save zone construction')
        CALL EMKCON(LTHRM(ICOMP),IUF,ICOMP,QUIET,IER)
        IF(IER.EQ.1)THEN
          dok=.true.
          helptopic='constr_write_error'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK(
     &      ' Problem detected while saving the thermophysical',
     &      ' properties..try again? (see help)',OK,dok,nbhelp)
          IF(OK)then
            goto 88
          else
            IER=1
            RETURN
          ENDIF
        endif
        CALL EMKCFG('-',IER)
      else
        call usrmsg('Cannot save model while in browse ',
     &              'mode, you must `own` the model! ','W')
        goto 92
      endif

C If any of the surfaces are transparent save this info.
      TFOUND=.FALSE.
      DO 61 IS=1,NZSUR(icomp)
        icn1=izstocn(icomp,is)
        IF(SSOTF(icn1)(1:4).NE.'OPAQ'.AND.
     &            SSOTF(icn1)(1:3).NE.'CFC')TFOUND=.TRUE.
        IF(ITMCFL(ICOMP,IS).GT.0)TFOUND=.TRUE.
  61  CONTINUE
      IF(TFOUND)THEN
        CALL MKTWIN(IUF,ICOMP,QUIET,IER)
        IF(IER.EQ.1)RETURN
        ITW(ICOMP)=1

C If in quiet mode then files already exist => no need to save.
        IF(.NOT.QUIET) THEN
          CALL EMKCFG('-',IER)
          IF(IER.EQ.1)RETURN
        ENDIF
      else

C Check to see if there was an initial assumption that tmc file was
C needed and now it is not.
         if(ITW(ICOMP).eq.1)then
           ITW(ICOMP)=0
           call usrmsg('Removing unused tmc file: ',LTWIN(ICOMP),'-')
           CALL ERPFREE(IUF,ISTAT)
           CALL EFOPSEQ(IUF,LTWIN(ICOMP),1,IER)
           CALL EFDELET(IUF,ISTAT)
           CALL EMKCFG('-',IER)
         endif
      endif

c----------update CFC file
C If any of the surfaces are CFCs then save this info.
      CFCFOUND=.FALSE.
      DO 612 IS=1,NZSUR(icomp)
        icn1=izstocn(icomp,is)
        IF(SSOTF(icn1)(1:3).EQ.'CFC')CFCFOUND=.TRUE.
        IF(icfcfl(ICOMP,IS).GT.0)CFCFOUND=.TRUE.
 612  CONTINUE
      IF(CFCFOUND)THEN
        icfc(icomp) = 1
        CALL makeCFCfile(ICOMP,IER)
        IF(IER.EQ.1)THEN
            icfc(icomp)=0
            CALL EMKCFG('-',IER)
            RETURN
        ENDIF
C If in quiet mode then files already exist => no need to save.
        IF(.NOT.QUIET) THEN
          CALL EMKCFG('-',IER)
          IF(IER.EQ.1)RETURN
        ENDIF
      ELSE

C Check to see if there was an initial assumption that cfc file was
C needed and now it is not.
         if(icfc(ICOMP).eq.1)then
           icfc(ICOMP)=0
           call usrmsg('Removing unused cfc file: ',lcfcin(ICOMP),'-')
           CALL ERPFREE(IUF,ISTAT)
           CALL EFOPSEQ(IUF,lcfcin(ICOMP),1,IER)
           CALL EFDELET(IUF,ISTAT)
           CALL EMKCFG('-',IER)
         endif
      ENDIF
C----------------

      MODCON=.FALSE.

C If geometry info has changed then save it as well.
      IF(MODGEO)THEN
        dok=.true.
        helptopic='attribute_recent_change'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK('Save zone geometry to reflect recent changes',
     &    'to surfaces and attributes? (see help)',OK,dok,nbhelp)
        if(OK)then
          call eclose(gversion(ICOMP),1.1,0.01,newgeo)
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            call geowrite(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
          else
            CALL EMKGEO(IUF,LGEOM(ICOMP),ICOMP,3,IER)
          endif
          MODGEO=.FALSE.
        endif
      ENDIF

C Return to menu or exit from editing facility as required.
      if(savereturn)then
        goto 92
      else
        return
      endif

      END

C ************* EDSCON
C Edit zone surface construction attributes in common block G6 and
C allow this to be saved. ITRU unit number for user output, IER=0 OK,
C IER=1 problem. Make use of construction information in common MLC.
      SUBROUTINE EDSCON(ITRU,ICOMP,ISUR,MODGEO,MODCON,IER)
#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
#include "material.h"
#include "CFC_common.h"

C      integer lnblnk  ! function definition

C Parameters
      integer itru   ! unit for reporting
      integer icomp  ! zone number
      integer isur   ! associated surface
      logical MODGEO ! return true if altered geometry
      logical MODCON ! return true if altered construction
      integer ier    ! zero is ok, one materials not available

      COMMON/OUTIN/IUOUT,IUIN

C Optical and geometric properties.
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/PRECT4/TOPTIC(MCOM,MTMC)

      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)

C Material properties.
      integer matarrayindex ! the indes within matdatarray

      LOGICAL closemat1,closemat2

C CFClayers db properties
      integer cfcarrayindex
      logical closecfc1

      DIMENSION ISD(25)
      CHARACTER ISD*56
      CHARACTER KEY*1,HOLD*48,TOPTIC*12,optic*12
      integer ISDN,INODA ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

C Thermal property checking values follow.
      DATA CONCH/250./,DENCH/4000./,SHTCH/3000./,THKCH/0.5/
      helpinsub='EDSCON'  ! set for subroutine

C Establish if material data arrays have been filled. If not return
C with ier=1.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        call usrmsg('The materials arrays are incomplete so editing',
     &    'of surface attributes not allowed.','W')
        ier=1
        return
      endif

C Establish if CFC layer data arrays have been filled. If not return
C with ier=1.
      call eclose(cfcver,1.1,0.01,closecfc1)
      if(closecfc1)then
        continue
      else
        call usrmsg('The CFC layer arrays are incomplete so editing',
     &    'of surface attributes not allowed.','W')
        ier=1
        return
      endif

C Setup text for menu.
   12 INODA=-3
      icn1=izstocn(icomp,isur)
   13 write(ISD(1),'(A,A)') '  surface name    : ',SSNAME(icn1)
      if(SSOTF(icn1)(1:4).eq.'OPAQ'.or.
     &   SSOTF(icn1)(1:4).eq.'TRAN'.or.
     &   SSOTF(icn1)(1:3).eq.'CFC')then
        write(ISD(2),'(A,A)') 'a surface type    : ',SSOTF(icn1)(1:12)
      else
        WRITE(ISD(2),'(A,A)') 'a optical set name: ',SSOTF(icn1)(1:12)
      endif
      write(ISD(3),'(A,A)') 'b construction    : ',SSMLCN(icn1)(1:12)
      if(ITMCFL(ICOMP,ISUR).GT.0)then
        optic=TOPTIC(ICOMP,ITMCFL(ICOMP,ISUR))
        write(ISD(4),'(2A)')'  optical property: ',optic
      else
        optic=' - '
        write(ISD(4),'(2A)')'  optical property: ',optic
      endif
      ISD(5)='  ______________________________________ '

C Surface properties. If CFC, don't write out emiss and abs.
      IF(SSOTF(icn1)(1:3).EQ.'CFC') THEN
        ISD(6)='                                          '
        ISD(7)='                                          '
      ELSE
        WRITE(ISD(6),22)EMISI(ISUR),EMISE(ISUR)
   22 FORMAT('c emissivity   inside face: ',F5.3,' other face: ',F5.3)
        WRITE(ISD(7),23)ABSI(ISUR),ABSE(ISUR)
   23 FORMAT('d absorptivity inside face: ',F5.3,' other face: ',F5.3)
      ENDIF
      ISD(8)='  ______________________________________ '
      M=8

C Find composite which matches.
      ICF=-1
      if(ssmlcindex(icn1).ne.0) ICF=ssmlcindex(icn1)

C For each layer.
      M=M+1
      IF(SSOTF(icn1)(1:4).EQ.'CFC2') THEN
        ISD(M)=' lyr|CFCl|Thick|Conduc-|Density|Specific'
        M=M+1
        ISD(M)='    |db  |metre|tivity |       |heat    '
      ELSE
        ISD(M)=' lyr|Mat|Thick|Conduc-|Density|Specific|Air  '
        M=M+1
        ISD(M)='    |db |metre|tivity |       |heat    |gap R'
      ENDIF

      DO 45 IE=1,NE(ISUR)
        M=M+1
        CALL EMKEY(M,KEY,IER)
        IF(ICF.EQ.-1)THEN

C Multilayer construction has not been defined.
          WRITE(ISD(M),346)KEY,IE,THK(ISUR,IE),CON(ISUR,IE),
     &                     DEN(ISUR,IE),SHT(ISUR,IE)
 346      FORMAT(A1,I4,'  --   ',F6.3,3F8.2)
        ELSE

          check_CFC2_or_MLC:
     &    IF(SSOTF(icn1)(1:4).EQ.'CFC2') THEN
            cfcarrayindex=ITMCFCDB(ICF,IE)

            if(cfcarrayindex.eq.0)then
              WRITE(ISD(M),'(a1,i3,a)')KEY,IE,
     &          ' confused CFC layer reference'
            else
              WRITE(ISD(M),44)KEY,IE,cfcitmindex(ITMCFCDB(ICF,IE)),
     &          THK(ISUR,IE),CON(ISUR,IE),DEN(ISUR,IE),SHT(ISUR,IE)
  44          FORMAT(A1,I3,I4,F6.3,3F8.2)
            endif

          ELSE ! check_CFC2_or_MLC

            matarrayindex=IPRMAT(ICF,IE)   ! which materials array index
            if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &         matopaq(matarrayindex)(1:1).eq.'h'.or.
     &         matarrayindex.eq.0)then

C Find air gap resistance for this layer. 
              DO IGG=1,NAIRG(ISUR)
                IF(IPAIRG(ISUR,IGG).EQ.IE)R=RAIRG(ISUR,IGG)
                WRITE(ISD(M),48)KEY,IE,IPR(ICF,IE),THK(ISUR,IE),
     &            CON(ISUR,IE),DEN(ISUR,IE),SHT(ISUR,IE),R
  48            FORMAT(A1,I3,I4,F6.3,3F8.2,F6.2)
              ENDDO
            elseif(matarrayindex.lt.0)then
              WRITE(ISD(M),'(a1,i3,a)')KEY,IE,
     &          ' confused material reference'
            else
              WRITE(ISD(M),46)KEY,IE,IPR(ICF,IE),THK(ISUR,IE),
     &          CON(ISUR,IE),DEN(ISUR,IE),SHT(ISUR,IE)
  46          FORMAT(A1,I3,I4,F6.3,3F8.2)
            endif       
          ENDIF check_CFC2_or_MLC

        ENDIF
  45  CONTINUE
      M=M+1
      ISD(M)='  ______________________________________ '
      M=M+1
      ISD(M)='? help                                   '
      M=M+1
      ISD(M)='- exit this menu                         '
      INODA=-4

C Size of the menu.
      ISDN=M
      IPT1=ISDN-2

C Help text for this menu.
      helptopic='surface_constr_atrib'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Menu control.
      CALL EMENU('Surface construction attributes',ISD,ISDN,INODA)
      IF(INODA.EQ.ISDN)THEN
        RETURN
      ELSEIF(INODA.EQ.0)THEN

C Choose another surface.
        GOTO 12
      ELSEIF(INODA.EQ.ISDN-1)THEN

C List help text for the menu.
        helptopic='surface_constr_atrib'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('surface details',nbhelp,'-',0,0,IER)
      ELSEIF(INODA.EQ.ISDN-2)THEN
        GOTO 12
      ELSEIF(INODA.EQ.1)THEN
        GOTO 12
      ELSEIF(INODA.EQ.2)THEN

C Specify whether surface multilayer construction is opaque, transparent,
C or fictitious (bookkeeping purposes only). If transparent ask user
C which set of optical properties to use. (This will probably result in
C an additional surface attribute!!!)
C Additional option to choose CFC type.
        if(SSOTF(icn1)(1:4).eq.'OPAQ')then
          ino=1
        elseif(SSOTF(icn1)(1:4).eq.'TRAN')then
          ino=2
        elseif(SSOTF(icn1)(1:4).eq.'UNKN')then
          ino=1
        elseif(SSOTF(icn1)(1:3).eq.'CFC')then
          ino=3
        else
          ino=2
        endif
        ilno=ino
        CALL EASKABCD('Select a type for the surface.',' Surface type',
     &      'Opaque (all layers)','Transparent (at least 1 layer)',
     &      'Complex Fenestration Construction',
     &      'cancel',ino,nbhelp)
        IF(ilno.eq.ino)GOTO 12
        IF(ino.eq.0)GOTO 12
        IF(ino.EQ.1)THEN
          SSOTF(icn1)='OPAQUE'
        ELSEIF(ino.EQ.2)THEN
          SSOTF(icn1)='TRAN'
        ELSEIF(ino.EQ.3)THEN
          if(SSOTF(icn1)(1:4).eq.'CFC ')then
            SSOTF(icn1)='CFC '
          elseif(SSOTF(icn1)(1:4).eq.'CFC2')then
            SSOTF(icn1)='CFC2'
          endif
        ELSEIF(ino.EQ.4)THEN
          GOTO 12
        ENDIF
        MODGEO=.TRUE.
        MODCON=.TRUE.
      ELSEIF(INODA.EQ.3)THEN

C Select composite and then put the info into common, updating
C the OPAQUE/TRANSPARENT flag as well.
        CALL EPKMLC(IC,'Select one of these constructions ',
     &    'or -Exit. ',IER)
        IF(IER.EQ.1)THEN
          CALL USRMSG(' ',' A problem was encountered..','W')
          RETURN
        ENDIF
        IF(IC.GT.0)THEN
          WRITE(SSMLCN(icn1),'(A)') mlcname(IC)(1:12)
          ssmlcindex(icn1)=IC   ! update array
          if(mlctype(IC)(1:4).EQ.'TRAN')then
            SSOTF(icn1)='TRAN'
          endif
          if(mlctype(IC)(1:4).EQ.'OPAQ')then
            SSOTF(icn1)='OPAQUE'
            ITMCFL(ICOMP,ISUR)=0
          endif
          if(mlctype(IC)(1:4).EQ.'CFC ')then
            SSOTF(icn1)='CFC '
            ITMCFL(ICOMP,ISUR)=0
          endif
          if(mlctype(IC)(1:4).EQ.'CFC2')then
            SSOTF(icn1)='CFC2'
            ITMCFL(ICOMP,ISUR)=0
          endif
        ELSE

C User selected UNKNOWN so reset variables
          WRITE(SSMLCN(icn1),'(A)') 'UNKNOWN'
          ssmlcindex(icn1)=0   ! update array
          SSOTF(icn1)='OPAQUE'
          ITMCFL(ICOMP,ISUR)=0
          GOTO 12
        ENDIF

C Put the construction common file info into common blocks, reset number of air
C gaps in preparation of scan.
        MODCON=.TRUE.
        NAIRG(ISUR)=0
        NE(ISUR)=LAYERS(IC)

C Gather information about each layer via materials data structures.
        DO 223, IL=1,LAYERS(IC)

          check_CFC2_or_MLC_2:
     &    IF(SSOTF(icn1)(1:4).EQ.'CFC2') THEN

            cfcarrayindex=ITMCFCDB(IC,IL)
            if(cfcarrayindex.gt.0)then
              DBCON=cfcdbcon(cfcarrayindex)
              DBDEN=cfcdbden(cfcarrayindex)
              DBSHT=cfcdbsht(cfcarrayindex)
              if(CFCshdtp(cfcarrayindex).eq.iGasGap)then
                NAIRG(ISUR)=NAIRG(ISUR)+1
                IPAIRG(ISUR,NAIRG(ISUR))=IL
C...............Gap resistance is not used for CFCs, but gap layer position
C...............information is, so set to arbitrary non-zero value.
                RAIRG(ISUR,NAIRG(ISUR))=0.001          
              endif
            endif

C CFC emissivity and absorptivity calclated elsewhere so set to 0.0
            E=0.0
            A=0.0

          ELSE ! check_CFC2_or_MLC_2

            matarrayindex=IPRMAT(IC,IL)   ! which materials array index

C And if matarrayindex is zero then resetn dbcon dbden dbsht.
            if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &         matopaq(matarrayindex)(1:1).eq.'h'.or.
     &         matarrayindex.eq.0)then
              DBCON=0.0; DBDEN=0.0; DBSHT=0.0
              E=0.99; A=0.99

C Update array NAIRG.
              if(NAIRG(ISUR)+1.gt.MGP)then
                call usrmsg(
     &          'As defined, this composite contains more air gaps',
     &          ' then allowed - please change your description','W')
              else
                NAIRG(ISUR)=NAIRG(ISUR)+1
                IPAIRG(ISUR,NAIRG(ISUR))=IL
                IF(SSVFC(icn1)(1:4).EQ.'VERT')THEN
                   RAIRG(ISUR,NAIRG(ISUR))=DRAIR(IC,IL,1)
                ELSEIF(SSVFC(icn1)(1:4).EQ.'FLOR'.OR.
     &                 SSVFC(icn1)(1:4).EQ.'CEIL')THEN
                   RAIRG(ISUR,NAIRG(ISUR))=DRAIR(IC,IL,2)
                ELSE
                   RAIRG(ISUR,NAIRG(ISUR))=DRAIR(IC,IL,3)
                ENDIF
              endif
            elseif(matarrayindex.lt.0)then
              call edisp(iuout,
     &  'One material reference is confused or has a duplicate name.')
              goto 12
            else
              DBCON=matdbcon(matarrayindex)
              DBDEN=matdbden(matarrayindex)
              DBSHT=matdbsht(matarrayindex)
              E=matdbine(matarrayindex)
              A=matdbina(matarrayindex)
            endif

          ENDIF check_CFC2_or_MLC_2

          IF(IER.EQ.1)RETURN

          CON(ISUR,IL)=DBCON
          DEN(ISUR,IL)=DBDEN
          SHT(ISUR,IL)=DBSHT
          THK(ISUR,IL)=DTHK(IC,IL)

C Assign surface properties.
          IF(IL.EQ.1)         EMISE(ISUR)=E
          IF(IL.EQ.LAYERS(IC))EMISI(ISUR)=E
          IF(IL.EQ.LAYERS(IC))ABSI(ISUR)=A
          IF(IL.EQ.1)         ABSE(ISUR)=A

  223   CONTINUE
        MODGEO=.TRUE.
      ELSEIF(INODA.EQ.6)THEN
        IF(SSOTF(icn1)(1:3).NE.'CFC') THEN
C Emissivity.
C << todo take advantage of the out/in data within the material arrays. >>
          CALL EASKR(EMISI(ISUR),' ','Inside emissivity  ?',
     &    0.01,'W',0.99,'W',0.8,'inside emissivity',IER,nbhelp)
          CALL EASKR(EMISE(ISUR),' ','Outside emissivity ?',
     &    0.01,'W',0.99,'W',0.8,'outside emissivity',IER,nbhelp)
          MODCON=.TRUE.
        ENDIF
      ELSEIF(INODA.EQ.7)THEN
        IF(SSOTF(icn1)(1:3).NE.'CFC') THEN
C Absorptivity.
C << todo take advantage of the out/in data within the material arrays. >>
          CALL EASKR(ABSI(ISUR),' ',' Inside absorptivity ? ',
     &    0.01,'W',0.99,'W',0.8,'inside absorptivity',IER,nbhelp)
          CALL EASKR(ABSE(ISUR),' ',' Outside absorptivity? ',
     &    0.01,'W',0.99,'W',0.8,'outside absorptivity',IER,nbhelp)
          MODCON=.TRUE.
        ENDIF
      ELSEIF(INODA.GT.10.AND.INODA.LT.IPT1)THEN

C Edit thermophysical properties. Inform caviats on
C first pass.
        IE=INODA-10
        MODCON=.TRUE.
  42    HOLD=' '
        WRITE(HOLD,'(1x,f7.3,3f11.3)')THK(ISUR,IE),CON(ISUR,IE),
     &                             DEN(ISUR,IE),SHT(ISUR,IE)
        CALL EASKS(HOLD,
     &    ' Layer thickness(m), conductivitiy, density, specific heat:',
     &    '  ',48,' 0.025 1.0 100. 100. ','thk con den spht',
     &    IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,ZZ,0.001,THKCH,'W','thickness',IER)
        CALL EGETWR(HOLD,K,XO,0.001,CONCH,'W','conductivity',IER)
        CALL EGETWR(HOLD,K,YO,0.1,DENCH,'W','density',IER)
        CALL EGETWR(HOLD,K,ZO,0.1,SHTCH,'W','specific heat',IER)
        if(ier.ne.0)goto 42
        CON(ISUR,IE)=XO
        DEN(ISUR,IE)=YO
        SHT(ISUR,IE)=ZO
        THK(ISUR,IE)=ZZ

C Find air gap resistance for this layer.
        DO 147 IGG=1,NAIRG(ISUR)
          IF(IPAIRG(ISUR,IGG).EQ.IE)THEN
            VAL=RAIRG(ISUR,IGG)
            VAL=RAIRG(ISUR,IGG)
            CALL EASKR(VAL,' ',' Air gap resistance ? ',
     &        0.001,'W',9.99,'W',0.017,'air gap R',IER,nbhelp)
            RAIRG(ISUR,IGG)=VAL
          ENDIF
 147    CONTINUE
      ELSE
        INODA=-4
        goto 13
      ENDIF
      INODA=-4
      goto 13

      END

C ****************** EDZCON
C EDZCON takes the information in the composite construction db and the
C zone geometry file (surface attributes) and attempts to construct a
C zone construction file with minimum intervention by the user.

      SUBROUTINE EDZCON(ICOMP,MODGEO,MODCON,IER)
#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "CFC_common.h"

C Parameters
      integer icomp  ! zone number
      logical MODGEO ! return true if altered geometry
      logical MODCON ! return true if altered construction
      integer ier    ! zero is ok

      COMMON/OUTIN/IUOUT,IUIN
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)

      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

C Material properties.
      integer matarrayindex ! the indes within matdatarray

      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical closemat1,closemat2

C CFClayer db properties
      integer cfcarrayindex
      logical closecfc1

      CHARACTER outs*124

C Assume no window optical properties asked for.
      IER=0

C Loop through each surface, reading in the data from the materials and
C composite and optical db as required.
      IF(.NOT.CFGOK)THEN
        CALL USRMSG(
     &  ' In order to carry out this operation it is essential that',
     &  ' a model configuration be defined!','W')
        RETURN
      ENDIF

C If the common block MLC has not yet been filled, read in the
C composite construction database.
      IF(.NOT.MLDBOK)THEN
        CALL ERMLDB(0,iuout,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG(' Error in reading composite construction.',
     &    ' Unable to create zone construction file!','W')
          RETURN
        ELSE
          MLDBOK=.TRUE.
        ENDIF
      ENDIF

C Establish if material data arrays have been filled. If not return
C with ier=1.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        call usrmsg('The materials arrays are incomplete so zone',
     &    'construction attributes cannot be written.','W')
        ier=1
        return
      endif

C Establish if CFC layer data arrays have been filled. If not return
C with ier=1.
      call eclose(cfcver,1.1,0.01,closecfc1)
      if(closecfc1)then
        continue
      else
        call usrmsg('The CFC layer arrays are incomplete so zone',
     &    'construction attributes cannot be written.','W')
        ier=1
        return
      endif

 244  DO 10 IS=1,NZSUR(icomp)
        ioc=IZSTOCN(ICOMP,is)
        if(ioc.eq.0)then
          goto 244  ! An unknown surface in the master list. << what to do? >>
        endif

C Take the construction name associated with this surface and find the
C equivalent MLC in the array. If name is UNKN then ask user to select.

C << The logic for dealing with TRAN/OPAQ needs to be updated! >>
        NAIRG(IS)=0
        IF(SSMLCN(ioc)(1:4).EQ.'UNKN')THEN
  245     WRITE(outs,'(3A)')' Composition of ',SSNAME(ioc),
     &                         ' is unknown.'
          CALL EPKMLC(ISEL,outs,'Please select one...',IER)
          IF(ISEL.GT.0)THEN
            WRITE(SSMLCN(ioc),'(A)') mlcname(ISEL)(1:12)
            ssmlcindex(ioc)=ISEL  ! update other surf index
            if(mlctype(ISEL)(1:4).EQ.'TRAN')then
              SSOTF(ioc)='TRAN'
              ITMCFL(ICOMP,IS)=1
            endif
            if(mlctype(ISEL)(1:4).EQ.'OPAQ')then
              SSOTF(ioc)='OPAQUE'
              ITMCFL(ICOMP,IS)=0
            endif
            if(mlctype(ISEL)(1:4).EQ.'CFC ')then
              SSOTF(ioc)='CFC '
              ITMCFL(ICOMP,IS)=0
            endif
            if(mlctype(ISEL)(1:4).EQ.'CFC2')then
              SSOTF(ioc)='CFC2'
              ITMCFL(ICOMP,IS)=0
            endif

C Remember this change by saving the file.
            MODGEO=.TRUE.
            MODCON=.TRUE.
          ELSE
            GOTO 245
          ENDIF
        ENDIF

C Reset number of layers, if at end of loop it is still 0 then
C the material was not found and must be asked for.
        NE(IS)=0
        if(ssmlcindex(ioc).eq.0)then
          write(outs,'(5A)') ' The construction ',SSMLCN(ioc),
     &         ' of ',SSNAME(ioc),' was not found in the mlc db...'
          call usrmsg(outs,'resetting attribute to UNKNOWN.','W')
          SSMLCN(ioc)='UNKNOWN'
          goto 244
        endif

        IC=ssmlcindex(ioc)
        MODCON=.TRUE.
        NE(IS)=LAYERS(IC)

C Gather information about each layer. Read in the materials info.
        DO 23, IL=1,LAYERS(IC)

          check_CFC2_or_MLC:
     &    If(SSOTF(ioc)(1:4).EQ.'CFC2')THEN      
            
            cfcarrayindex=ITMCFCDB(IC,IL)
            if(cfcarrayindex.gt.0)then
              DBCON=cfcdbcon(cfcarrayindex)
              DBDEN=cfcdbden(cfcarrayindex)
              DBSHT=cfcdbsht(cfcarrayindex)
              if(CFCshdtp(cfcarrayindex).eq.iGasGap)then
                NAIRG(IS)=NAIRG(IS)+1
                IPAIRG(IS,NAIRG(IS))=IL
C...............Gap resistance is not used for CFCs, but gap layer position
C...............information is, so set to arbitrary non-zero value.
                RAIRG(IS,NAIRG(IS))=0.001          
              endif
            endif

C CFC emissivity and absorptivity calclated elsewhere so set to 0.0
            E=0.0
            A=0.0

          ELSE ! check_CFC2_or_MLC
            matarrayindex=IPRMAT(IC,IL)   ! which materials array index

C And if matarrayindex is zero then resetn dbcon dbden dbsht.
            if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &         matopaq(matarrayindex)(1:1).eq.'h'.or.
     &         matarrayindex.eq.0)then
              DBCON=0.0; DBDEN=0.0; DBSHT=0.0
              E=0.99; A=0.99

C Debug            write(6,*) 'EDZCON',ic,il,DRAIR(IC,IL,1),
C     &        DRAIR(IC,IL,2),DRAIR(IC,IL,3)

C Deal with air gap resistance if layer is a gap.
              if(NAIRG(IS)+1.gt.MGP)then
                call usrmsg(
     &          'This construction contains more air gaps than',
     &          'allowed - please change your description','W')
              else
                NAIRG(IS)=NAIRG(IS)+1
                IPAIRG(IS,NAIRG(IS))=IL
                IF(SSVFC(ioc)(1:4).EQ.'VERT')THEN
                  RAIRG(IS,NAIRG(IS))=DRAIR(IC,IL,1)
                ELSEIF(SSVFC(ioc)(1:4).EQ.'FLOR'.OR.
     &                 SSVFC(ioc)(1:4).EQ.'CEIL')THEN
                  RAIRG(IS,NAIRG(IS))=DRAIR(IC,IL,2)
                ELSE
                  RAIRG(IS,NAIRG(IS))=DRAIR(IC,IL,3)
                ENDIF
              endif
            elseif(matarrayindex.lt.0)then
              DBCON=0.0; DBDEN=0.0; DBSHT=0.0
              E=0.99; A=0.99
              continue  ! can't deal with a confused material
            else
              DBCON=matdbcon(matarrayindex)
              DBDEN=matdbden(matarrayindex)
              DBSHT=matdbsht(matarrayindex)
              E=matdbine(matarrayindex)
              A=matdbina(matarrayindex)
            endif

          ENDIF check_CFC2_or_MLC

          CON(IS,IL)=DBCON
          DEN(IS,IL)=DBDEN
          SHT(IS,IL)=DBSHT
          THK(IS,IL)=DTHK(IC,IL)

C Assign surface properties.
          IF(IL.EQ.1)         EMISE(IS)=E
          IF(IL.EQ.LAYERS(IC))EMISI(IS)=E
          IF(IL.EQ.LAYERS(IC))ABSI(IS)=A
          IF(IL.EQ.1)         ABSE(IS)=A

   23   CONTINUE

C Assume no default windows.
   10 CONTINUE

      RETURN
      END

C *************** MKTWIN
C Create a transparent construction file based on information
C currently held in common blocks.
C LTWIN is the name of the file to be written to (any existing file
C by this name is overwritten).  ZNAME is the zone name (12 char), ICOMP
C is the zone number.  ITRC= trace verbose level.
C IER=0 OK, IER=1 problem.

C Work out where the various definitive information sources should be
C combined to create this file.  In essence we need to read through
C each of the existing surfaces and find the unique optical types which
C exist in this zone.  Then for each of these we need to write out their
C optical properties as gathered from the optical database.  Make use of
C the number of composite layers to build this up.
      SUBROUTINE MKTWIN(IFU,ICOMP,QUIET,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"

      integer lnblnk  ! function definition

C Parameter
      integer IFU   ! file unit
      integer ICOMP ! zone number
      logical QUIET ! if true then no dialog
      integer IER   ! if zero then ok

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/PRECT2/TMCT2(MCOM,MTMC,5,MBP),TMCA2(MCOM,MTMC,ME,5,MBP),
     &              TVTR2(MCOM,MTMC,MBP)

      COMMON/PRECT3/NTMC,NGLAZ(MTMC)
      common/PRECT4/TOPTIC(MCOM,MTMC)

      COMMON/TMCB1/IBCMT(MCOM,MTMC)
      COMMON/TMCB2/NBCTMC(MCOM,MTMC),IBCST(MCOM,MBP,MTMC),
     &             IBCFT(MCOM,MBP,MTMC),IBCSUR(MCOM,MTMC)
      COMMON/TMCB3/NBCTT(MCOM,MBP,MTMC),BACTPT(MCOM,MBP,MTMC)
      logical XST

      dimension ival(MS)
      CHARACTER TOPTIC*12,louts*248

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MKTWIN'  ! set for subroutine

C If in quiet mode then the file should exist, but if it is not a
C silly name open it and inform.
      IF(QUIET) THEN
        call FINDFIL(LTWIN(ICOMP),XST)
        if(XST)then
          CALL EFOPSEQ(IFU,LTWIN(ICOMP),1,IER)
        else
          if(LTWIN(ICOMP)(1:4).eq.'UNKN'.or.
     &       LTWIN(ICOMP)(1:2).eq.'  ')then
            goto 98
          else
            CALL EFOPSEQ(IFU,LTWIN(ICOMP),4,IER)
C Debug
C            call edisp(iuout,' Creating a new TMC file...')
C            call edisp(iuout,LTWIN(ICOMP))
          endif
        endif
      ELSE
        CALL EFOPSEQ(IFU,LTWIN(ICOMP),4,IER)
      ENDIF
      IF(IER.NE.0)THEN
        GOTO 98
      ENDIF

C Write out the transparent construction file data.
      WRITE(IFU,30,IOSTAT=ISTAT,ERR=98)
     &  zname(ICOMP)(1:lnzname(ICOMP)),
     &  LTWIN(ICOMP)(1:lnblnk(LTWIN(ICOMP)))
  30  FORMAT('# transparent properties of ',a,' defined in ',a)

C Build one or two packed strings and write out. Should be good
C for up to NZSUR() of ~62.
      WRITE(IFU,'(I4,A)',IOSTAT=ISTAT,ERR=98)NZSUR(icomp),
     &  '   # surfaces'
      do 32 ij=1,MS
        ival(ij)=ITMCFL(icomp,ij)
  32  continue
      WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)'# tmc index for each surface'
      itrunc=1
      ipos=1
      do while (itrunc.ne.0)
        call ailist(ipos,nzsur(icomp),ival,MS,'C',louts,loutlen,itrunc)
        write(ifu,'(a)',IOSTAT=ios,ERR=98) louts(1:loutlen)
        ipos=itrunc+1
      end do

C For each of the TMC types.
      DO 100 I=1,NTMC
        WRITE(IFU,'(I4,2x,A,A,I2)',IOSTAT=ISTAT,ERR=98)NGLAZ(I),
     &    TOPTIC(ICOMP,I),' # layers in tmc type',I

        WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &         '# Transmission @ 5 angles & visible tr.'
        WRITE(IFU,'(6F8.3)',IOSTAT=ISTAT,ERR=98)
     &              (TMCT(ICOMP,I,J),J=1,5),TVTR(ICOMP,I)

        WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &         '# For each layer absorption @ 5 angles'
        DO 120 J=1,NGLAZ(I)
          WRITE(IFU,'(5F8.3)',IOSTAT=ISTAT,ERR=98)
     &                (TMCA(ICOMP,I,J,K),K=1,5)
  120   CONTINUE
        WRITE(IFU,'(I4,A)',IOSTAT=ISTAT,ERR=98)IBCMT(ICOMP,I),
     &               '  # optical control flag'
        IF(IBCMT(ICOMP,I).EQ.0)GOTO 100
        WRITE(IFU,'(2I4,A)',IOSTAT=ISTAT,ERR=98)NBCTMC(ICOMP,I),
     &      IBCSUR(ICOMP,I),'  # no control periods & sensor loc'

        WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &         '# Replacement properties for each control period'
        DO 121 K=1,NBCTMC(ICOMP,I)
          WRITE(IFU,'(2I4,A)',IOSTAT=ISTAT,ERR=98)IBCST(ICOMP,K,I),
     &        IBCFT(ICOMP,K,I),'   # period start and end '

          if(NBCTT(ICOMP,K,I).eq.0)then
            WRITE(IFU,'(I4,F6.1,A)',IOSTAT=ISTAT,ERR=98)
     &        NBCTT(ICOMP,K,I),BACTPT(ICOMP,K,I),
     &        '  # sensing total radiation @ actuation point'
          elseif(NBCTT(ICOMP,K,I).eq.1)then
            WRITE(IFU,'(I4,F6.1,A)',IOSTAT=ISTAT,ERR=98)
     &        NBCTT(ICOMP,K,I),BACTPT(ICOMP,K,I),
     &        '  # sensing ambient temperature @ actuation point'
          elseif(NBCTT(ICOMP,K,I).eq.2)then
            WRITE(IFU,'(I4,F6.1,A)',IOSTAT=ISTAT,ERR=98)
     &        NBCTT(ICOMP,K,I),BACTPT(ICOMP,K,I),
     &        '  # sensing zone temperature @ actuation point'
          elseif(NBCTT(ICOMP,K,I).eq.3)then
            WRITE(IFU,'(I4,F6.1,A)',IOSTAT=ISTAT,ERR=98)
     &        NBCTT(ICOMP,K,I),BACTPT(ICOMP,K,I),
     &        '  # sensing daylight coeff. @ actuation point'
          elseif(NBCTT(ICOMP,K,I).eq.4)then
            WRITE(IFU,'(I4,F6.1,A)',IOSTAT=ISTAT,ERR=98)
     &        NBCTT(ICOMP,K,I),BACTPT(ICOMP,K,I),
     &        '  # sensing lightswitch @ actuation point'
          elseif(NBCTT(ICOMP,K,I).eq.-99)then
            WRITE(IFU,'(I4,F6.1,A)',IOSTAT=ISTAT,ERR=98)
     &        NBCTT(ICOMP,K,I),BACTPT(ICOMP,K,I),
     &        '  # sensing time @ actuation point'
          endif

          WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &       '# Alt solar & vis trans followed by absorp for each layer'
          WRITE(IFU,'(6F8.3)',IOSTAT=ISTAT,ERR=98)
     &                (TMCT2(ICOMP,I,M,K),M=1,5),TVTR2(ICOMP,I,K)
          DO 122 J=1,NGLAZ(I)
            WRITE(IFU,'(5F8.3)',IOSTAT=ISTAT,ERR=98)
     &                  (TMCA2(ICOMP,I,J,M,K),M=1,5)
  122     CONTINUE
          ITPREP=0
          WRITE(IFU,'(I4)',IOSTAT=ISTAT,ERR=98)ITPREP
  121   CONTINUE
  100 CONTINUE

 1111 CALL ERPFREE(IFU,ISTAT)
      RETURN

c Error messages.

C If in quiet mode: the file should exist => error if we are here.
   98 IF (IER.EQ.-301) THEN
        helptopic='tmc_file_not_found'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('file error',nbhelp,'-',0,0,IER)
      ELSE
        CALL EDISP(IUOUT,' TMC file write error ')
      ENDIF
      IER=1
      goto 1111

      END

C *************** EDTWIN
C Edit and configure transparent construction information
C currently held in common blocks PRECTC, PRECT2, TMCB1,
C TMCB2, TMCB3.

C In essence this procedure assumes the use of items from the
C construction database, which if they are transparent make reference
C to a named optical property in the optical properties database (if
C UNKNOWN then the user must supply this information manually). It scans
C through each of the existing surfaces and finds unique optical types.
      SUBROUTINE EDTWIN(ITRC,ITRU,ICOMP,IER)
#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB

      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/PRECT2/TMCT2(MCOM,MTMC,5,MBP),TMCA2(MCOM,MTMC,ME,5,MBP),
     &              TVTR2(MCOM,MTMC,MBP)

      COMMON/PRECT3/NTMC,NGLAZ(MTMC)
      common/PRECT4/TOPTIC(MCOM,MTMC)

      COMMON/TMCB1/IBCMT(MCOM,MTMC)
      COMMON/TMCB2/NBCTMC(MCOM,MTMC),IBCST(MCOM,MBP,MTMC),
     &             IBCFT(MCOM,MBP,MTMC),IBCSUR(MCOM,MTMC)
      COMMON/TMCB3/NBCTT(MCOM,MBP,MTMC),BACTPT(MCOM,MBP,MTMC)
      LOGICAL found,OK,dok
      logical canceled   ! toggle set if user interupts an edit session.
      integer IRT        ! for radio button

      CHARACTER OPT*12,lastopt*12,TOPTIC*12,GDESCR*36
      CHARACTER outs*124
      CHARACTER msg*28

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDTWIN'  ! set for subroutine

C If tmc data has already been read in (ie NTMC>0) then move on to
C a display of the current data.
      IF(NTMC.GT.0)GOTO 90

C Loop through each surface and if not opaque get its composite
C description (mlcname) and remember its index ICF, otherwise set ITMCFL
C to 0.
      lastopt='  '
      DO 10 IS=1,NZSUR(icomp)
        icn1=izstocn(icomp,is)
        IF(SSOTF(icn1)(1:4).EQ.'OPAQ'.OR.
     &     SSOTF(icn1)(1:3).EQ.'CFC')THEN
          ITMCFL(ICOMP,IS)=0
          GOTO 10
        ENDIF

C Use ssmlcindex to get matching MLC index
 246    ICF=-1
        if(ssmlcindex(icn1).ne.0) ICF=ssmlcindex(icn1)
        IF(ICF.EQ.-1)THEN
          WRITE(outs,'(A,A12)')
     &    ' select a transparent one for: ',SSNAME(icn1)
          CALL EPKMLC(ISEL,'From the available composites please',
     &      outs,IER)
          IF(ISEL.GT.0)THEN
            WRITE(SSMLCN(icn1),'(A12)') mlcname(ISEL)(1:12)
            ssmlcindex(icn1)=ISEL   ! update array
            ICF=ISEL
          ELSE
            RETURN
          ENDIF
        ENDIF

C Check this composite db entry is also transparent and that it has
C an associated set of optical properties. If no equivalent set of
C optical properties then the user will have to supply this manually.
        helptopic='constr_attribute_checks'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(outs,'(A12,A)') mlcname(ICF)(1:12),' is not transparent!'
        IF(mlctype(ICF)(1:4).EQ.'OPAQ')THEN
          CALL EASKAB(outs,' do you want to:','switch to opaque',
     &    'select a transparent material',IWB,nbhelp)
          if(IWB.eq.1)then

C Switch back to opaque construction.
            CALL EPKMLC(ISEL,'Which OPAQUE construction?',' ',IER)
            if(ISEL.GT.0)then
              WRITE(SSMLCN(icn1),'(A12)') mlcname(ISEL)(1:12)
              ssmlcindex(icn1)=ISEL   ! update array
              ICF=ISEL
              ITMCFL(ICOMP,IS)=0
              SSOTF(icn1)='OPAQUE'
              OPT='OPAQUE'
              goto 10
            else
              return
            endif
          elseif(IWB.eq.2)then
            CALL EPKMLC(ISEL,'Which TRANSPARENT construction?',
     &        ' ',IER)
            if(ISEL.GT.0)then
              WRITE(SSMLCN(icn1),'(A12)') mlcname(ISEL)(1:12)
              ssmlcindex(icn1)=ISEL   ! update array
              ICF=ISEL
            else
              return
            endif
          endif
        ENDIF

C Check to see if this new construction has a known set of opticals.
        WRITE(OPT,'(A)') mlcoptical(ICF)(1:12)
        IF(OPT(1:2).EQ.'  '.OR.OPT(1:7).EQ.'UNKNOWN'.OR.
     &     OPT(1:6).EQ.'OPAQUE')THEN
          WRITE(outs,'(1X,A12,A)')SSMLCN(icn1),
     &      ' has unknown optical properties!'
          CALL EASKABC(outs,' Do you want to: ',
     &      'supply optical properties','select another composite',
     &      'assign `opaque` to this one',IW,nbhelp)
          IF(IW.EQ.1)THEN

C << todo consider how one might treat a cancel request within eoptks
            NTL=LAYERS(ICF)
            CALL EOPTKS(ITRC,ITRU,'T',IER)
          ELSEIF(IW.EQ.2)THEN
            GOTO 246
          else
            ITMCFL(ICOMP,IS)=0
            SSOTF(icn1)='OPAQUE'
            OPT='OPAQUE'
            goto 10
          ENDIF
        ENDIF

C Optical properties known so grab from db. If new then add data from
C common GOPT to the current TMC type.
        if(lastopt(1:12).ne.OPT(1:12))then
          CALL EROPTDB(ITRC,ITRU,OPT,GDESCR,IER)
          IF(IER.NE.0)GOTO 98
        else
          if(ITRC.gt.1)call edisp(iuout,'Using previous optics.')
        endif

C Save ITMCFL.
        lastopt=OPT
        found=.FALSE.
        IF(NTMC.GT.0)THEN
          DO 16 IJJ=1,NTMC
            if(TOPTIC(ICOMP,IJJ).EQ.OPT)then
              found=.TRUE.
              ITMCFL(ICOMP,IS)=IJJ
            endif
16        CONTINUE
        ENDIF
        IF(.NOT.found)THEN
          NTMC=NTMC+1
          IF(NTMC.GT.MTMC.OR.NTMC.EQ.0)GOTO 94
          TOPTIC(ICOMP,NTMC)=OPT
          ITMCFL(ICOMP,IS)=NTMC
          DO 12 ID=1,5
            TMCT(ICOMP,NTMC,ID)=DG(ID)
   12     CONTINUE
          TVTR(ICOMP,NTMC)=VTRN
          NGLAZ(NTMC)=LAYERS(ICF)
          DO 14 J=1,LAYERS(ICF)
            TMCA(ICOMP,NTMC,J,1)=AB(J,1)
            TMCA(ICOMP,NTMC,J,2)=AB(J,2)
            TMCA(ICOMP,NTMC,J,3)=AB(J,3)
            TMCA(ICOMP,NTMC,J,4)=AB(J,4)
            TMCA(ICOMP,NTMC,J,5)=AB(J,5)
   14     CONTINUE
          IBCMT(ICOMP,NTMC)=0
        ENDIF
   10 CONTINUE

  200 RETURN

C Display of current information...
   90 CONTINUE
      helptopic='constr_attribute_checks'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EDISP(ITRU,' ')
      WRITE(outs,9996)zname(ICOMP)(1:lnzname(ICOMP))
 9996 FORMAT(' Transparent construction file details for ',A)
      CALL EDISP(ITRU,outs)
      CALL EDISP(ITRU,' ')
      CALL EDISP(ITRU,' Surface      Construction OPAQ/   Optical  ')
      CALL EDISP(ITRU,' Name         Description  TRANS   Reference')
      DO 31, ISR=1,NZSUR(icomp)
        icn1=izstocn(icomp,isr)
        WRITE(outs,'(1X,A12,2X,A12,2X,A6,I4)')SSNAME(icn1),
     &    SSMLCN(icn1)(1:12),SSOTF(icn1)(1:6),ITMCFL(ICOMP,ISR)
        CALL EDISP(ITRU,outs)
   31 CONTINUE
      DO 202 I=1,NTMC
        CALL EDISP(ITRU,' ')
        WRITE(outs,'(A,I2,A,F6.3)')
     &    ' For TMC type ',I,' with visable trn:',TVTR(ICOMP,I)
        CALL EDISP(ITRU,outs)
        CALL EDISP(ITRU,' Direct transmission @ 5 angles  ')
        WRITE(outs,'(2X,5F7.3)')(TMCT(ICOMP,I,J5),J5=1,5)
        CALL EDISP(ITRU,outs)
        CALL EDISP(ITRU,' For each layer absorption @ 5 angles ')
        DO 33 IL=1,NGLAZ(I)
          WRITE(outs,'(2X,5F7.3)')(TMCA(ICOMP,I,IL,J5),J5=1,5)
          CALL EDISP(ITRU,outs)
   33   CONTINUE

        CALL EDISP(ITRU,' ')
        IF(IBCMT(ICOMP,I).EQ.0)THEN
          CALL EDISP(ITRU,' There are no controls active.')
          GOTO 202
        ENDIF

        IF(IBCSUR(ICOMP,I).EQ.0)THEN
          WRITE(outs,'(A,I2,A)')' There are ',NBCTMC(ICOMP,I),
     &     ' control periods and there is a sensor at each TMC.'
        ELSE
          WRITE(outs,'(A,I2,A,I2)')' There are ',NBCTMC(ICOMP,I),
     &     ' control periods and the sensor is at surf',IBCSUR(ICOMP,I)
        ENDIF
        CALL EDISP(ITRU,outs)
        DO 21 KK=1,NBCTMC(ICOMP,I)
          if(NBCTT(ICOMP,KK,I).eq.0)then
            msg=' sensing total radiation'
          elseif(NBCTT(ICOMP,KK,I).eq.1)then
            msg=' sensing ambient temperature'
          elseif(NBCTT(ICOMP,KK,I).eq.2)then
            msg=' sensing zone temperature'
          elseif(NBCTT(ICOMP,KK,I).eq.3)then
            msg=' sensing daylight coeff.'
          elseif(NBCTT(ICOMP,KK,I).eq.4)then
            msg=' Lightswitch (not available)'
          elseif(NBCTT(ICOMP,KK,I).eq.-99)then
            msg=' sensing time only'
          endif
          WRITE(outs,'(A,I2,A,I2,A,I2,2A,F7.2)') ' Period ',KK,
     &      ': from ',IBCST(ICOMP,KK,I),' to ',IBCFT(ICOMP,KK,I),
     &      msg(1:lnblnk(msg)),' set @ ',BACTPT(ICOMP,KK,I)
          CALL EDISP(ITRU,outs)

          CALL EDISP(ITRU,' Alt. direct trans @ 5 angles & vis trans  ')
          WRITE(outs,'(2X,6F6.2)')(TMCT2(ICOMP,I,J5,KK),J5=1,5),
     &                            TVTR2(ICOMP,I,KK)
          CALL EDISP(ITRU,outs)
          CALL EDISP(ITRU,' For each layer alt absorp @ 5 angles ')
          DO 35 IL=1,NGLAZ(I)
            WRITE(outs,'(2X,5F7.3)')(TMCA2(ICOMP,I,IL,J5,KK),J5=1,5)
            CALL EDISP(ITRU,outs)
   35     CONTINUE
   21   CONTINUE
  202 CONTINUE

C Having displayed the current common data provide an editing facility...
C For each of the TMC types found in this zone ask if there is any
C control attached.  Provide a jump back point in case the user
C invokes a cancel during the editing.
      canceled=.false.
  203 if(canceled)then
        dok=.false.
        CALL ASKOK('Try again to define the optical control?',
     &    '(A `no` clears the control definitions.)',OK,
     &    dok,nbhelp)
        if(.NOT.OK) then

C Clear the optical control common blocks that might have
C been set during the prior edit.
          DO 204 IT=1,NTMC
            IBCMT(ICOMP,IT)=0
            NBCTMC(ICOMP,IT)=0
            IBCSUR(ICOMP,IT)=0
  204     continue
          GOTO 200
        endif
      endif
      DO 22 IT=1,NTMC
        if(IBCMT(ICOMP,IT).eq.1)then
          helptopic='detect_existing_opt_ctl'
          call gethelptext(helpinsub,helptopic,nbhelp)
          WRITE(OUTS,'(A,I2,A)')' For TMC type ',IT,
     &      ' there is an optical control. Options:'
          CALL EASKABC(outs,' ','remove this optical control',
     &      'edit this optical control','accept control as-is',
     &      iwa,nbhelp)
          if(iwa.eq.1)then
            IBCMT(ICOMP,IT)=0
            GOTO 22
          elseif(iwa.eq.2)then
            OK=.true.
          elseif(iwa.eq.3)then
            IBCMT(ICOMP,IT)=1
            GOTO 22
          endif
        else
          dok=.false.
          helptopic='detect_existing_opt_ctl'
          call gethelptext(helpinsub,helptopic,nbhelp)
          WRITE(OUTS,'(A,I2,A)')'For TMC type ',IT,' is there any'
          CALL ASKOK(OUTS,'optical control?',OK,dok,nbhelp)
          IF(.NOT.OK)THEN
            IBCMT(ICOMP,IT)=0
            GOTO 22
          ENDIF
        endif

        helptopic='optical_control_summary'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IBCMT(ICOMP,IT)=1
        N1=NBCTMC(ICOMP,IT)
        CALL EASKI(N1,' ','Number of control periods?',
     &    1,'F',MBP,'F',1,'no of control periods',IERI,nbhelp)
        if(ieri.eq.-3) then
          canceled=.true.
          goto 203
        endif
        NBCTMC(ICOMP,IT)=N1

        N2=IBCSUR(ICOMP,IT)
        CALL EASKAB(' ','Transparent surfaces controlled by:',
     &    'separate sensors','a single sensor',IW,nbhelp)
        if(IW.eq.1)then
          IBCSUR(ICOMP,IT)=0
        else
          CALL EASKI(N2,' ','Surface for common sensor location?',
     &      1,'F',NZSUR(icomp),'F',1,'sensor location',
     &      IERI,nbhelp)
          if(ieri.eq.-3) then
            canceled=.true.
            goto 203
          endif
          IBCSUR(ICOMP,IT)=N2
        endif

C For each control period get the appropriate replacement properties.
        helptopic='optical_control_summary'
        call gethelptext(helpinsub,helptopic,nbhelp)
        DO 24 ICP=1,N1
          write(outs,'(A,I2)')' For control period ',ICP
          VAL=REAL(IBCST(ICOMP,ICP,IT))
          CALL EASKR(VAL,outs,' Begin at hour: ',
     &      0.00,'W',24.0,'W',0.0,'control start',IER,nbhelp)
          IBCST(ICOMP,ICP,IT)=INT(VAL)

          VAL=REAL(IBCFT(ICOMP,ICP,IT))
          CALL EASKR(VAL,outs,' End at hour  : ',
     &      0.00,'W',24.0,'W',24.0,'control end',IER,nbhelp)
          IBCFT(ICOMP,ICP,IT)=INT(VAL)

          N3=NBCTT(ICOMP,ICP,IT)
          IRT=1
          if(n3.eq.-99)then
            call EASKATOG('Controlled on: (currently time)','Confirm',
     &      'time','incident radiation','ambient temp.','zone temp.',
     &      'lux level','LtSwitch (not avail)','cancel',IRT,nbhelp)
          elseif(n3.eq.0)then
            call EASKATOG('Controlled on: (currently radiation)',
     &      'Confirm',
     &      'time','incident radiation','ambient temp.','zone temp.',
     &      'lux level','LtSwitch (not avail)','cancel',IRT,nbhelp)
          elseif(n3.eq.1)then
            call EASKATOG(
     &      'Controlled on:(currently ambient temperature)','Confirm',
     &      'time','incident radiation','ambient temp.','zone temp.',
     &      'lux level','LtSwitch (not avail)','cancel',IRT,nbhelp)
          elseif(n3.eq.2)then
            call EASKATOG(
     &      'Controlled on: (currently zone temperature)','Confirm',
     &      'time','incident radiation','ambient temp.','zone temp.',
     &      'lux level','LtSwitch (not avail)','cancel',IRT,nbhelp)
          elseif(n3.eq.3)then
            call EASKATOG(
     &      'Controlled on: (currently daylight coefficients)',
     &      'Confirm',
     &      'time','incident radiation','ambient temp.','zone temp.',
     &      'lux level','LtSwitch (not avail)','cancel',IRT,nbhelp)
          elseif(n3.eq.4)then
            call usrmsg('Currently link to Lightswitch 2002 is not',
     &        'available. Please select something else.','W')
C            call EASKATOG('Controlled on:','(currently Lightswitch)',
C     &      'time','incident radiation','ambient temp.','zone temp.',
C     &      'lux level','LtSwitch','cancel',IRT,nbhelp)
          else
            call EASKATOG('Controlled on: (currently undefined)',
     &      'Confirm',
     &      'time','incident radiation','ambient temp.','zone temp.',
     &      'lux level','LtSwitch (not avail)','cancel',IRT,nbhelp)
          endif
          if(irt.eq.1)then
            NBCTT(ICOMP,ICP,IT)= -99
            BACTPT(ICOMP,ICP,IT)=0.
          elseif(irt.eq.2)then
            NBCTT(ICOMP,ICP,IT)= 0
            VAL=BACTPT(ICOMP,ICP,IT)
            CALL EASKR(VAL,' ','Radiation set point?',
     &        0.00,'W',1000.0,'W',0.0,'radiation set point',IER,nbhelp)
            BACTPT(ICOMP,ICP,IT)=VAL
          elseif(irt.eq.3)then
            NBCTT(ICOMP,ICP,IT)= 1
            VAL=BACTPT(ICOMP,ICP,IT)
            CALL EASKR(VAL,' ','Ambient temp. set point?',
     &        0.00,'W',1000.0,'W',0.0,'ambient set point',IER,nbhelp)
            BACTPT(ICOMP,ICP,IT)=VAL
          elseif(irt.eq.4)then
            NBCTT(ICOMP,ICP,IT)= 2
            VAL=BACTPT(ICOMP,ICP,IT)
            CALL EASKR(VAL,' ','Zone temp. set point',
     &        0.00,'W',1000.0,'W',0.0,'zone set point',IER,nbhelp)
            BACTPT(ICOMP,ICP,IT)=VAL
          elseif(irt.eq.5)then
            NBCTT(ICOMP,ICP,IT)= 3
            VAL=BACTPT(ICOMP,ICP,IT)
            CALL EASKR(VAL,' ','Lux set point',
     &        0.00,'W',1000.0,'W',0.0,'lux set point',IER,nbhelp)
            BACTPT(ICOMP,ICP,IT)=VAL
          elseif(irt.eq.6)then

C << ESP-r to Lightswitch is not currently implemented >>
C            NBCTT(ICOMP,ICP,IT)= 4
C            VAL=BACTPT(ICOMP,ICP,IT)
C            CALL EASKR(VAL,' ','Lightswitch lux set point?',
C     &        0.00,'W',1000.0,'W',0.0,'LtSw illum. set point',
C     &        IER,nbhelp)
C            BACTPT(ICOMP,ICP,IT)=VAL
          elseif(irt.eq.7)then
            return
          ENDIF
          NTL=NGLAZ(IT)

C Pick the alternative optical properties for the period. Note that
C the number of layers should match that of the base case.

C << todo: consider how to react to a cancel request within eoptks

          CALL EOPTKS(ITRC,ITRU,'T',IER)
          DO 42 ID=1,5
            TMCT2(ICOMP,IT,ID,ICP)=DG(ID)
   42     CONTINUE
          TVTR2(ICOMP,IT,ICP)=VTRN
          DO 44 J=1,NGLAZ(IT)
            TMCA2(ICOMP,IT,J,1,ICP)=AB(J,1)
            TMCA2(ICOMP,IT,J,2,ICP)=AB(J,2)
            TMCA2(ICOMP,IT,J,3,ICP)=AB(J,3)
            TMCA2(ICOMP,IT,J,4,ICP)=AB(J,4)
            TMCA2(ICOMP,IT,J,5,ICP)=AB(J,5)
   44     CONTINUE
   24   CONTINUE
   22 CONTINUE
      GOTO 200


C Error messages.
  98  CALL USRMSG('A problem was encountered reading the Optical',
     &   'Properties db, or a property set was not located.','W')
      IER=1
      GOTO 200

  94  CALL USRMSG(' ','To many TMC types for one zone!','W')
      IER=1
      GOTO 200

      END


C ************* EOPTKS
C Edits optical properties associated with a transparent surface.
C Takes in and returns data via common block GOPT. If WHICH='T' for
C transparent construction data editing.
      SUBROUTINE EOPTKS(ITRC,ITRU,WHICH,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB
      CHARACTER SOPT*12,GDESCR*36,HOLD*42
      CHARACTER WHICH*1,outs*124

      integer ntlt   ! for local editing.

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EOPTKS'  ! set for subroutine

C The user will be allowed to use a item from the optical database or
C to put in the data directly.  77 is a jump back point in case the
C user cancels part way through the definition.

C << todo: consider adding a cancel option >>

  77  helptopic='manual_optical_editing'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKAB(' Get optical properties from: ',' ',
     &  ' manual data input',' optical database',IW,nbhelp)
      IF(IW.EQ.2)THEN
        CALL EDWINO(SOPT,IER)
        IF(SOPT.EQ.'UNKNOWN')RETURN
        CALL EROPTDB(ITRC,ITRU,SOPT,GDESCR,IER)
        RETURN
      ENDIF

  42  HOLD=' '
      WRITE(HOLD,'(1x,5f8.3)')DG(1),DG(2),DG(3),DG(4),DG(5)
      CALL EASKS(HOLD,' Direct trans @ 0 40 55 70 80 degrees: ',
     &   '  ',42,' 0.779 0.759 0.717 0.581 0.348 ','dir trans',
     &   IER,nbhelp)
      K=0
      CALL EGETWR(HOLD,K,DG(1),0.001,0.99,'W','dir t @ 0',IER)
      CALL EGETWR(HOLD,K,DG(2),0.001,0.99,'W','dir t @ 40',IER)
      CALL EGETWR(HOLD,K,DG(3),0.001,0.99,'W','dir t @ 55',IER)
      CALL EGETWR(HOLD,K,DG(4),0.001,0.99,'W','dir t @ 70',IER)
      CALL EGETWR(HOLD,K,DG(5),0.001,0.99,'W','dir t @ 80',IER)
      if(ier.ne.0)goto 42

      CALL EASKR(VTRN,' ',' Visible transmittance: ',
     &  0.001,'W',0.99,'W',0.85,'vis tran',IER,nbhelp)

      IF(WHICH.EQ.'w'.OR.WHICH.EQ.'W')THEN
        call usrmsg(
     &    'The concept of total heat gain factors is no longer used in',
     &    'esp-r. It uses angular transmission and layer absorb.','W')
      ELSEIF(WHICH.EQ.'t'.OR.WHICH.EQ.'T')THEN

C For each of the layers provide the absorptions.
        ntlt=ntl
        CALL EASKI(NTLT,' ',
     &    ' Current no. of layers (including air gaps)? ',
     &    1,'F',ME,'F',1,'layers in tmc',IERI,nbhelp)
        if(ieri.eq.-3)then
          goto 77
        endif
        ntl=ntlt

        DO 10 I=1,NTL
  44      HOLD=' '
          WRITE(HOLD,'(1x,5f8.3)')AB(I,1),AB(I,2),AB(I,3),AB(I,4),
     &       AB(I,5)
          WRITE(outs,'(a,I2,a)') ' Layer ',I,
     &       ' absorption @ 0 40 55 70 80 degrees:'
          CALL EASKS(HOLD,outs,'  ',
     &      42,' 0.106 0.116 0.124 0.129 0.125 ','layer abs',
     &      IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,AB(I,1),0.001,0.99,'W','abs @0',IER)
          CALL EGETWR(HOLD,K,AB(I,2),0.001,0.99,'W','abs @40',IER)
          CALL EGETWR(HOLD,K,AB(I,3),0.001,0.99,'W','abs @55',IER)
          CALL EGETWR(HOLD,K,AB(I,4),0.001,0.99,'W','abs @70',IER)
          CALL EGETWR(HOLD,K,AB(I,5),0.001,0.99,'W','abs @80',IER)
          if(ier.ne.0)goto 44
  10    CONTINUE
      ENDIF

      RETURN
      END

C ******* SCNTCNST
C Loop through referenced mlc and derives the longest timeconstant.
      subroutine scntcnst(TDM,istd,TCM,ISTC)
#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"

      COMMON/OUTIN/IUOUT,IUIN

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

c ITCNST  - number of start-up days
      COMMON/PREC7/ITCNST

      integer matarrayindex ! the indes within matdatarray

      character outs*124
      logical found,closemat1,closemat2

C Establish if material data arrays have been filled. If not return
C with ier=1.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        call usrmsg('The materials arrays are incomplete so time',
     &    'constant of constructions could not be established.','W')
        return
      endif

C Loop mlc db, if some surface references it, read properties for
C each layer and keep track of time constant.
      ITCNST=1
      TCM=0.
      TDM=0.
      do 40 ic=1,nmlc

c First determine maximum thermal diffusivity and associated
c homogeneous layer. Locate if the MLC database item is used.
        found=.false.
        do 42 icn=1,ncon
          if(ssmlcindex(icn).eq.ic) then
            found=.true.
            goto 43
          endif
  42    continue
  43    if(found)then
          RES=0.
          CAP=0.
          DO 45 il=1,LAYERS(ic)
            matarrayindex=IPRMAT(IC,IL)   ! which materials array index

C And if matarrayindex is zero or matopaq g or h then resetn dbcon dbden dbsht.
            if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &         matopaq(matarrayindex)(1:1).eq.'h'.or.
     &         matarrayindex.eq.0)then
              DBCON=0.0; DBDEN=0.0; DBSHT=0.0
            elseif(matarrayindex.gt.0)then
              DBCON=matdbcon(matarrayindex)
              DBDEN=matdbden(matarrayindex)
              DBSHT=matdbsht(matarrayindex)
            endif

C Get properties for outer layer.
            if(il.eq.1)then
              thk1=DTHK(ic,il)
              con1=DBCON
              den1=DBDEN
              sht1=DBSHT
            endif

C If an air gap, recalculate overall resistance.
            if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &         matopaq(matarrayindex)(1:1).eq.'h'.or.
     &         matarrayindex.eq.0)then
              RES=RES+((DRAIR(ic,il,1)+DRAIR(ic,il,2)+
     &            DRAIR(ic,il,3))/3.)
            else
              TD=DBCON/(DBDEN*DBSHT)
              IF(IC.GT.1)RES=RES+DTHK(ic,il)/DBCON
              IF(IC.GT.1)CAP=CAP+DBDEN*DBSHT*DTHK(ic,il)
              IF(TD.GT.TDM)then
                TDM=TD
                istd=IC
                IE=IL
              endif
            endif
 45       continue
          RO=thk1/con1
          U=1./(RES+RO)
          TC=((con1*den1*sht1*(RO-0.1*RES))+(1.1*CAP))/U
          if(TC.gt.TCM)then
            TCM=TC
            ISTC=IC
          endif
        endif
  40  continue

c Determine the start-up length as a function of this maximum
c time constant.
      ITCN=(1*INT(TCM/3600.))/24+1
      IF(ITCN.GT.ITCNST)ITCNST=ITCN
      write(outs,'(A,G12.4)')' Maximum thermal diffusivity is',TDM
      call edisp(iuout,outs)
      write(outs,'(3A,i3,A,i3)')' Associated MLC is ',
     &  mlcname(istd)(1:12),' (',istd,') Layer',IE
      call edisp(iuout,outs)
      write(outs,'(A,G12.4,A)')' Maximum time constant is',TCM,' secs.'
      call edisp(iuout,outs)
      write(outs,'(A,A)')' Associated MLC construction is',
     &  mlcname(ISTC)(1:12)
      call edisp(iuout,outs)
      write(outs,'(A,I3,A)')' Simulation start-up is',ITCNST,' days.'
      call edisp(iuout,outs)

      return
      end
