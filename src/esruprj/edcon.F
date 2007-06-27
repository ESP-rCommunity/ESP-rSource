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
#include "epara.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/PRECT3/NTMC,NGLAZ(MTMC)
      common/PRECT4/TOPTIC(MCOM,MTMC)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      COMMON/TMCB1/IBCMT(MCOM,MTMC)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      common/user/browse
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL OK,DOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL MODIFY,MODLEN,MODBND,browse,savereturn

      DIMENSION VERT(35),libcmt(MTMC)

      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS,CFILE
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,DFILE
      CHARACTER zname*12,zdesc*64,SFILE*72,H*72,LTMP*72
      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15
      CHARACTER outs*124,VERT*48,KEY*1,head*31,ETEXT*82
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,TOPTIC*12,OPTIC*12
      LOGICAL MODGEO,MODCON,TFOUND,XST,again,QUIET

C Clear number of TMC's and layers in each before loading common data.
      NTMC=0
      DO 249 I=1,MTMC
        NGLAZ(I)=0
  249 CONTINUE

C DFILE is the default file name for any TMC file to be created,
C UFILE is default for utility file. CFILE is default for constr file.
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(DFILE,'(2a)')zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.tmc'
        WRITE(CFILE,'(2a)')zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.con'
      else
        WRITE(DFILE,'(4a)') zonepth(1:lnblnk(zonepth)),'/',
     &    zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.tmc'
        WRITE(CFILE,'(4a)') zonepth(1:lnblnk(zonepth)),'/',
     &    zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.con'
      endif
      IUF=IFIL+2

C Set existance of a zone construction file to false.
      MODGEO=.FALSE.
      MODCON=.FALSE.

      WRITE(outs,248)LGEOM(ICOMP)(1:LNBLNK(LGEOM(ICOMP)))
  248 FORMAT(' Scanning : ',A)
      CALL USRMSG(' ',outs,'-')

C Read in the related geometry file into common.
      CALL EGOMIN(IFIL+1,LGEOM(ICOMP),ICOMP,1,ITRC,IUOUT,IER)
      IF(IER.NE.0)THEN
        CALL USRMSG(' ',' Problem detected in geometry... ','W')
        RETURN
      ENDIF

C Scan through the geometry file and if most of the construction
C attributes have not been set then advise the user to use the
C geometry facility first.
      ig=0
      DO 11 I=1,NSUR
        if(SMLCN(I)(1:4).EQ.'UNKN')ig=ig+1
  11  CONTINUE
      if(ig.gt.(NSUR/2))then
        H(1)='In scanning the geometry it was found that many of '
        H(2)='the surfaces have not been attributed as to their '
        H(3)='construction type.  It is recommended that you: '
        H(4)='  '
        H(5)='  a) go to the  `geometry browse & edit` facility and'
        H(6)='     attribute the surface construction types - after'
        H(7)='     which you can return to this facility,'
        H(8)='  '
        H(9)='  b) in the case of an existing `old or unattributed`'
        H(10)='     zone construction file select the `Save` '
        H(11)='     option in the following menu. '
        CALL PHELPD('unknowns atrib in constr',11,'-',0,0,IER)
      endif

C On entry display the current zone. Reset all surface lines to std.
      if(QUIET)then
        ITORG=1
        ITGRD=1
      endif 
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      MODIFY=.TRUE.
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
        CALL ADJVIEW(ITRC,IER)
      endif

C Read construction file into common.
  42  CONTINUE
      IF(again.or.LTHRM(ICOMP)(1:1).EQ.' '.OR.
     &            LTHRM(ICOMP)(1:7).EQ.'UNKNOWN')THEN

C Construction file name not yet specified.
        H(1)='The zone construction file defines the thermo-'
        H(2)='physical properties of the zone and is mandatory.'
        LTMP=CFILE
        CALL EASKS(LTMP,' Zone construction file? ',' ',
     &    72,CFILE,'construction file',IER,2)
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
          CALL EDZCON(ITRC,ICOMP,MODGEO,MODCON,IER) 
        ELSE
          write(outs,'(3a)') 'Found an existing constructions file: ',
     &            LTHRM(ICOMP)(1:lnblnk(LTHRM(ICOMP))),'.'
          h(1)='Unless there is control on optical properties, the'
          h(2)='best way to proceed is to recreate via geometry'
          h(3)='attributes.'
          CALL EASKABC(outs,'Options:',
     &     'use it','recreate via geometry attributes',
     &     'cancel',IWA,3)
          IF(IWA.EQ.1)THEN
            call tstamp('>','PRJ: use existing zone constructions')
            CALL ECONST(LTHRM(ICOMP),IFIL+1,ICOMP,0,IUOUT,IER)
            if(ITW(ICOMP).eq.1)then
              CALL ERTWIN(ITRC,IUOUT,IUF,LTWIN(ICOMP),ICOMP,IER)
            endif
          elseif(IWA.EQ.2)then
            call tstamp('>','PRJ: scan db & rebuild zone construction')
            CALL EDZCON(ITRC,ICOMP,MODGEO,MODCON,IER) 
          elseif(IWA.eq.3)then
            call tstamp('>','PRJ: abandoned rebuild zone construction')
            return
          ENDIF 
        ENDIF
      else

C If in quiet mode: the file should exist => error if we are here.
        IF (QUIET) THEN
          H(1)='The construction file does not exist, or was not'
          H(2)='found, please create/rereference this file.'
          CALL PHELPD('file error',2,'-',0,0,IER)
          RETURN
        ELSE
          write(outs,'(1x,A,A)')LTHRM(ICOMP)(:lnblnk(LTHRM(ICOMP))),
     &        ' not found!'
          CALL EASKAB(outs,'Options:','respecify',
     &        'create using this name',IWB,0)
          if(IWB.eq.1)then
            again=.true.
            goto 42
          endif
        ENDIF
        CALL EDZCON(ITRC,ICOMP,MODGEO,MODCON,IER)
      endif
      if(IER.NE.0)then
        again=.true.
        GOTO 42
      endif

C Having done so check to see if there are any transparent surfaces.
C If surface attribute TRAN reset ITMCFL otherwise reconcile mismatches!
      TFOUND=.FALSE.
      DO 60 IS=1,NSUR
        IF((SOTF(IS).NE.'TRAN').AND.ITMCFL(ICOMP,IS).ne.0)then
          write(outs,'(A,A)')SNAME(ICOMP,IS),' is opaque.'
          dok=.true.
          h(1)='The opaque/transparent attribute of the surface is not'
          h(2)='`TRAN` but another attribute suggests that the surface'
          h(3)='might be opaque. If it is opaque then select `ok`.'
          CALL ASKOK(outs,'Confirm?',OK,DOK,2)
          if(OK)then
            ITMCFL(ICOMP,IS)=0
          else
            SOTF(IS)='TRAN'
          endif
        endif
        IF(SOTF(IS).EQ.'TRAN')TFOUND=.TRUE.
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
          H(1)='A TMC file holds optical properties for transparent'
          H(2)='surfaces within a zone as well as the definition of'
          H(3)='any control of blinds associated with such surfaces.'
          LTWIN(ICOMP)=DFILE
          ltmp=LTWIN(ICOMP)
          CALL EASKS(ltmp,'TMC file name?',
     &      ' ',72,DFILE,'TMC file name',IER,3)
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
      ILEN=NSUR
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
          WRITE(VERT(M),14)KEY,SNAME(ICOMP,L),SOTF(L),SMLCN(L),OPTIC
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
      CALL ADJVIEW(ITRC,IER)
      call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &             iicfgz,iicfgn,iicfgc,iicfgdfn)
      if(MMOD.EQ.8)then
        call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &               iicfgz,iicfgn,iicfgc,iicfgdfn)
        WRITE(ETEXT,'(2A)')'Project: ',LSNAM(1:lnblnk(LSNAM))
        CALL viewtext(ETEXT,1,1,1)
      endif

C Help text for this menu.
      H(1)='The surface attributes within the zone geometry file,'
      H(2)='in conjunction with the Constructions db, allow the'
      H(3)='specific thermo-physical properties to be saved within'
      H(4)='the model definition.'
      H(5)=' '
      H(6)='The recommended procedure is to attribute surfaces'
      H(7)='using the geometry editing facility before creating'
      H(8)='the zone contructions file.'
      H(9)=' '
      H(10)='Surfaces which are composed of transparent materials'
      H(11)='will have optical properties defined from either' 
      H(12)='the optical database or by manual input. If there'
      H(13)='is a mismatch between information in a zone TMC'
      H(14)='file and existing surface attributes then you will be'
      H(15)='asked to confirm the correct choice.'
      H(16)=' '
      H(17)='Optical properties can have scheduled controls which'
      H(18)='allow an alternative optical property to be used based'
      H(19)='on the following sensd parameters:'
      H(20)=' a) time - alternative optics are used during a'
      H(21)='    specific period of the day'
      H(22)=' b) total radiation on the outside face of a zone'
      H(23)='    surface - you specify a radiation level (W/m^2)'
      H(24)='    above which the alternate optical propertiess'
      H(25)='    are used.'
      H(26)=' c) ambient temperature - you specify a temperature'
      H(27)='    above which the alternative optical properties'
      H(28)='    are used.'
      H(29)=' d) zone air temperature - you specify the temperature'
      H(30)='    above which the alternative optical properties'
      H(31)='    are used.'
      H(32)=' e) lux level - you specify the lux level above which'
      H(33)='    the alternative optical properties are used.'
      H(34)='    '
      H(35)='Note that there can be up to 8 control periods during'
      H(36)='a day and that all days within the simulation are treated'
      H(37)='the same).'
      H(38)=' '
      H(39)='The alternative optical propetry set must relate to the'
      H(40)='same number of material layers as the original set. It'
      H(41)='is a good idea to prepare the alternative set using the'
      H(42)='the Optical Properties db management facility prior to'
      H(43)='using the transparent properties facility in this menu.'
      H(44)=' '
      H(45)='Control of optical properties can be used to approximate'
      H(46)='the closing of window blinds and shutters.'
      H(47)=' '
      H(48)='If the model has an air flow network you might also'
      H(49)='wish to control any flow components associated with'
      H(50)='transparent surfaces that have optical property control.'
      H(51)='Note however that the air flow control logic is'
      H(52)='different from optical property control logic so care'
      H(53)='is required to ensure that they operate together as'
      H(54)='intended.'

C Now display the menu.
      write(head,'(3a)')'Composition of `',zname(ICOMP),'`'
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        if(browse)return
        if(MODGEO.OR.MODCON)then
          dok=.true.
          h(1)='The Project Manager thinks that there have been recent'
          h(2)='changes to either surface attributes or constructions'
          h(3)='that have not been saved. To be safe agree to saving'
          h(4)='these changes.'
          CALL ASKOK('Save changes in surface attributes or',
     &    ' constructions before exiting?',OK,dok,4)
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
        CALL PHELPD('zone geom file section',54,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Save data and then return to menu.
         savereturn=.true.
         goto 43
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Linear thermal conductivity.
        H(1)='This facility currently requires that you use an'
        H(2)='external text editor to add information to the zone' 
        H(3)='constructions file to define linear dependencies.'
        H(4)='Please contact your administrator for information on'
        H(5)='how to do this.'
        H(6)=' '
        H(7)='The linear facility identifies which layers in the'
        H(8)='model will have such dependencies applied.'
        H(9)=' '
        H(10)='A non-linear property facility is also available'
        H(11)='from the above menu.'
        CALL PHELPD('linear properties',11,'-',0,0,IER)
        call tstamp('>','PRJ: enter linear properties')
        CALL LKMENU(ICOMP)
        GOTO 777
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C TMC details, check that something transparent first.
        TFOUND=.FALSE.
        DO 71 IS=1,NSUR
          IF(SOTF(IS).EQ.'TRAN')TFOUND=.TRUE.
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
        CALL EDSCON(ITRC,ITRU,ICOMP,IS,MODGEO,MODCON,IER)
        CALL EPMENRC
      ELSE

C Not one of the legal menu choices.
        GOTO 92
      ENDIF
      GOTO 92

  43  continue
C Save...
      H(1)='The current construction data will be placed in a'
      H(2)='file. If this is the same name as the source file'
      H(3)='then it will be overwritten.'
      H(4)='In addition, details of surfaces which are transparent'
      H(5)='will be place in a zone TMC file. '
      IF (.NOT.QUIET) THEN
        CALL PHELPD('zone constr file merge',5,'-',0,0,IER)
      ENDIF
  88  IF(LTHRM(ICOMP)(1:2).EQ.'  '.OR.
     &   LTHRM(ICOMP)(1:7).EQ.'UNKNOWN')THEN

C Ask for name of file to put common block information into.
        SFILE=' '
        CALL EASKS(SFILE,' New construction file? ',
     &    ' ',72,CFILE,'related constr file name',IER,3)
        IF(SFILE.NE.' ')LTHRM(ICOMP)=SFILE
      ENDIF

C Save current information into a descriptive file
C and save the system configuration file if not browsing.
      if(cfgok.and.(.NOT.browse))then
        IUF=IFIL+2
        call tstamp('>','PRJ: save zone construction')
        CALL EMKCON(LTHRM(ICOMP),IUF,ICOMP,IUOUT,QUIET,IER)
        IF(IER.EQ.1)THEN
          dok=.true.
          h(1)='Check that you have permission to update the zone'
          h(2)='construction and/or optics files and that you have'
          h(3)='sufficient disk space. '
          CALL ASKOK(
     &      ' Problem detected while saving the thermophysical',
     &      ' properties..try again? (see help)',OK,dok,3)
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
      DO 61 IS=1,NSUR
        IF(SOTF(IS).EQ.'TRAN')TFOUND=.TRUE.
        IF(ITMCFL(ICOMP,IS).GT.0)TFOUND=.TRUE.
  61  CONTINUE
      IF(TFOUND)THEN
        CALL MKTWIN(ITRU,IUF,ICOMP,QUIET,IER)
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
      MODCON=.FALSE.

C If geometry info has changed then save it as well.
      IF(MODGEO)THEN
        dok=.true.
        h(1)='ESP-r thinks that there are recent changes that have'
        h(2)='not yet been saved to file. Information might be lost'
        h(3)='if you do not save the changes. '
        CALL ASKOK('Save zone geometry to reflect recent changes',
     &    'to surfaces and attributes? (see help)',OK,dok,3)
        if(OK)then
          CALL EMKGEO(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
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
C Edit zone surface construction attributes in common block G5 and 
C allow this to be saved. ITRU unit number for user output, IER=0 OK, 
C IER=1 problem. Make use of construction information in common MLC.
      SUBROUTINE EDSCON(ITRC,ITRU,ICOMP,ISUR,MODGEO,MODCON,IER)
#include "building.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

C Optical and geometric properties.
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/PRECT4/TOPTIC(MCOM,MTMC)

      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)

C Arrays of material properties.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      real matdbcon,matdbden,matdbsht,matdboute,matdbine
      real matdbouta,matdbina,matdbdrv,matdbthick
      common/matdatarray/matlegindex(MGIT),matcatindex(MGIT),
     &  matdbcon(MGIT),matdbden(MGIT),matdbsht(MGIT),matdboute(MGIT),
     &  matdbine(MGIT),matdbouta(MGIT),matdbina(MGIT),matdbdrv(MGIT),
     &  matdbthick(MGIT)

      integer mathash
      common/mathasharray/mathash(0:600)

      character matname*32,matdoc*248,matopaq*1
      common/matnamearray/matname(MGIT),matdoc(MGIT),matopaq(MGIT)

      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the indes within matdatarray

      LOGICAL MODGEO,MODCON,told,closemat

      DIMENSION ISD(19)
      CHARACTER DESC*48,SOTHER*15,SNAME*12,SMLCN*12,SVFC*4,SOTF*4
      CHARACTER TITL*72,NAM*72,ISD*46
      CHARACTER H*72,KEY*1,HOLD*48,TOPTIC*12,optic*12,SSPARENT*12
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12

C Thermal property checking values follow.
      DATA CONCH/250./,DENCH/4000./,SHTCH/3000./,THKCH/0.5/
      told=.false.

C Setup text for menu.
   12 INODA=-3
   13 write(ISD(1),'(A,A)') '  Surface name    : ',SNAME(ICOMP,ISUR)     
      write(ISD(2),'(A,A)') 'a Surface type    : ',SOTF(ISUR)
      write(ISD(3),'(A,A)') 'b Construction    : ',SMLCN(ISUR)     
      if(ITMCFL(ICOMP,ISUR).GT.0)then
        optic=TOPTIC(ICOMP,ITMCFL(ICOMP,ISUR))
        write(ISD(4),'(2A)')'  Optical property: ',optic     
      else
        optic=' - '
        write(ISD(4),'(2A)')'  Optical property: ',optic    
      endif
      ISD(5)='  ______________________________________ '

C Surface properties.
      WRITE(ISD(6),22)EMISI(ISUR),EMISE(ISUR)
   22 FORMAT('c Emissivity int face: ',F5.3,' ext face: ',F5.3)
      WRITE(ISD(7),23)ABSI(ISUR),ABSE(ISUR)
   23 FORMAT('d Absorp.    int face: ',F5.3,' ext face: ',F5.3)
      ISD(8)='  ______________________________________ '
      M=8

C Find composite which matches. 
      ICF=-1
      lstr=lnblnk(SMLCN(ISUR))
      DO 20 IC=1,NMLC
        if(lstr.lt.12)then
          IF(DESC(IC)(1:lstr).EQ.SMLCN(ISUR)(1:lstr))ICF=IC
        else
          IF(DESC(IC)(1:12).EQ.SMLCN(ISUR)(1:12))ICF=IC
        endif
  20  CONTINUE

C For each layer.
      M=M+1
      ISD(M)=' lyr|Mat|Thick|Conduc-|Density|Specific|Air  '
      M=M+1
      ISD(M)='    |db |metre|tivity |       |heat    |gap R'
      DO 45 IE=1,NE(ISUR)
        M=M+1
        CALL EMKEY(M,KEY,IER)
        IF(ICF.EQ.-1)THEN

C Multilayer construction has not been defined.
          WRITE(ISD(M),346)KEY,IE,THK(ISUR,IE),CON(ISUR,IE),
     &                     DEN(ISUR,IE),SHT(ISUR,IE)
 346      FORMAT(A1,I4,' --',F6.3,3F8.2)
        ELSE
          IF(IPR(ICF,IE).NE.0)THEN
            WRITE(ISD(M),46)KEY,IE,IPR(ICF,IE),THK(ISUR,IE),
     &                      CON(ISUR,IE),DEN(ISUR,IE),SHT(ISUR,IE)
  46        FORMAT(A1,I3,I4,F6.3,3F8.2)
          ELSE

C Find air gap resistance for this layer.
            DO 47 IGG=1,NAIRG(ISUR)
              IF(IPAIRG(ISUR,IGG).EQ.IE)R=RAIRG(ISUR,IGG)
              WRITE(ISD(M),48)KEY,IE,IPR(ICF,IE),THK(ISUR,IE),
     &                        CON(ISUR,IE),DEN(ISUR,IE),SHT(ISUR,IE),R
  48          FORMAT(A1,I3,I4,F6.3,3F8.2,F6.2)
  47        CONTINUE
          ENDIF
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
      H(1)='This is a display of the surface attributes which may'
      H(2)='be edited.  Most are selections from existing database'
      H(3)='entries.'
      H(4)='  '
      H(5)='You are allowed to modify the thermophysical proper-'
      H(6)='ties of the layers BUT BE CAREFUL.  Manual editing'
      H(7)='may be overwritten by subsequent file re-attrubution.'
      H(8)=' '
      H(9)='RATHER create additional multilayer constructions'
      H(10)='for each unique case and use them as attributes.   '
      H(11)='DO NOT CHANGE the TOPOLOGY (ie. move air gaps around)'
      H(12)='here - do it in the mlc database.'

C Menu control.
      CALL EMENU('Surface Attributes',ISD,ISDN,INODA)
      IF(INODA.EQ.ISDN)THEN
        RETURN
      ELSEIF(INODA.EQ.0)THEN

C Choose another surface.
        GOTO 12
      ELSEIF(INODA.EQ.ISDN-1)THEN

C List help text for the menu.
        CALL PHELPD('surface details',12,'-',0,0,IER)
      ELSEIF(INODA.EQ.ISDN-2)THEN
        GOTO 12
      ELSEIF(INODA.EQ.1)THEN
        GOTO 12
      ELSEIF(INODA.EQ.2)THEN

C Specify whether surface multilayer construction is opaque, transparent,
C or fictitious (bookkeeping purposes only). If transparent ask user
C which set of optical properties to use. (This will probably result in
C an additional surface attribute!!!)
        H(1)='The choice of using an opaque or transparent construction'
        H(2)='is dependent on the optical properties. Where there is a'
        H(3)='mix of opaque and transparent layers the construction'
        H(4)='should be marked transparent.'
        H(5)='Fictitious surfaces are used for two purposes:'
        H(6)='  1) to allow zones to be subdivided for greater'
        H(7)='     detail in mass flow simulations, '
        H(8)='  2)for handling extremely complex geometries.'
        if(SOTF(ISUR)(1:4).eq.'OPAQ')ino=1
        if(SOTF(ISUR)(1:4).eq.'TRAN')ino=2
        if(SOTF(ISUR)(1:4).eq.'FICA')ino=3
        if(SOTF(ISUR)(1:4).eq.'FICB')ino=4
        ilno=ino
        idno=1
        call MENUATOL('Select a type for the surface.',' Surface type',
     &   'a Opaque (all layers)','b Transparent (at least 1 layer)',
     &   'c (N/A) Fictitious mass flow','d (N/A) Fictitious geometric',
     &   ' ',' ',' ',' ',' ',' ',' ',' ',ino,idno,8)
        IF(ilno.eq.ino)GOTO 12
        IF(ino.eq.0)GOTO 12
        IF(ino.EQ.1)THEN
          SOTF(ISUR)='OPAQ'
        ELSEIF(ino.EQ.2)THEN
          SOTF(ISUR)='TRAN'
        ELSEIF(ino.EQ.3)THEN
          SOTF(ISUR)='FICA'
        ELSEIF(ino.EQ.4)THEN
          SOTF(ISUR)='FICB'
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
          ioc=IZSTOCN(icomp,isur)
          WRITE(SMLCN(ISUR),'(A)')DESC(IC)(1:12)
          SSMLCN(ioc)=SMLCN(isur)
          if(DESC(IC)(15:18).EQ.'TRAN')then
            SOTF(ISUR)='TRAN'
            SSOTF(ioc)='TRAN'
          endif
          if(DESC(IC)(15:18).EQ.'OPAQ')then
            SOTF(ISUR)='OPAQ'
            SSOTF(ioc)='OPAQ'
            ITMCFL(ICOMP,ISUR)=0
          endif
        ELSE
          GOTO 12
        ENDIF

C Put the composite db info into common, reset number of air
C gaps in preparation of scan.
        MODCON=.TRUE.
        NAIRG(ISUR)=0
        NE(ISUR)=LAYERS(IC)
        call eclose(matver,1.1,0.01,closemat)

C Gather information about each layer. Read in the materials db info.
        DO 223, IL=1,LAYERS(IC)
          if(closemat)then
            legmat=IPR(IC,IL)   ! which legacy index
            if(legmat.ge.0)then
              matarrayindex=mathash(legmat)  ! find matching array index
              DBCON=matdbcon(matarrayindex)
              DBDEN=matdbden(matarrayindex)
              DBSHT=matdbsht(matarrayindex)
              E=matdbine(matarrayindex)
              A=matdbina(matarrayindex)
              DRV=matdbdrv(matarrayindex)
              write(TITL,'(a)') 'placeholder'
              write(NAM,'(a)') matname(matarrayindex)(1:32)
            endif
          else
            CALL ERPCDB(IFMAT,IPR(IC,IL),ITRU,IER,DBCON,DBDEN,
     &           DBSHT,E,A,DRV,TITL,NAM)
          endif
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
          IF(IPR(IC,IL).EQ.0)THEN

C Keep track of the air gaps in the composite, these variables may
C eventually be redundant. Based on the orientation of the surface
C assign an appropriate U value to each air gap encountered.
            if(NAIRG(ISUR)+1.gt.MGP)then
              call usrmsg(
     &        ' As defined, this composite contains more air gaps',
     &        ' then allowed - please change your description','W')
            else
              NAIRG(ISUR)=NAIRG(ISUR)+1
              IPAIRG(ISUR,NAIRG(ISUR))=IL
              IF(SVFC(ISUR).EQ.'VERT')THEN
                RAIRG(ISUR,NAIRG(ISUR))=DRAIR(IC,IL,1)
              ELSEIF(SVFC(ISUR).EQ.'FLOR'.OR.SVFC(ISUR).EQ.'CEIL')THEN
                RAIRG(ISUR,NAIRG(ISUR))=DRAIR(IC,IL,2)
              ELSE
                RAIRG(ISUR,NAIRG(ISUR))=DRAIR(IC,IL,3)
              ENDIF
            endif
          ENDIF
  223   CONTINUE
        MODGEO=.TRUE.
      ELSEIF(INODA.EQ.6)THEN

C Emissivity. 
C << todo take advantage of the out/in data within the material arrays. >>

        H(1)='The infrared emissivity is normally 0.1 < x < 0.9'
        CALL EASKR(EMISI(ISUR),' ','Inside emissivity  ?',
     &      0.01,'W',0.99,'W',0.8,'inside emissivity',IER,1)
        CALL EASKR(EMISE(ISUR),' ','Outside emissivity ?',
     &      0.01,'W',0.99,'W',0.8,'outside emissivity',IER,1)
        MODCON=.TRUE.
      ELSEIF(INODA.EQ.7)THEN

C Absorptivity.
C << todo take advantage of the out/in data within the material arrays. >>

        H(1)='The short wave absorptivity is normally 0.1 < x < 0.9'
        CALL EASKR(ABSI(ISUR),' ',' Inside absorptivity ? ',
     &      0.01,'W',0.99,'W',0.8,'inside absorptivity',IER,1)
        CALL EASKR(ABSE(ISUR),' ',' Outside absorptivity? ',
     &      0.01,'W',0.99,'W',0.8,'outside absorptivity',IER,1)
        MODCON=.TRUE.
      ELSEIF(INODA.GT.10.AND.INODA.LT.IPT1)THEN

C Edit thermophysical properties. Inform caviats on
C first pass.
        IE=INODA-10
        MODCON=.TRUE.
        if(.not.told)then
          H(1)='WARNING: You are allowed to modify the properties'
          H(2)='of the layers BUT NOT CHANGE the TOPOLOGY (ie.' 
          H(3)='move air gaps around) - do that in the multi-layer '
          H(4)='construction database facility.  Quality assurance'
          H(5)='is enhanced if you use the multilayer construction'
          H(6)='facility rather than editing entities here. '
          CALL PHELPD('surface details',6,'-',0,0,IER)
          told=.true.
        endif
  42    HOLD=' '
        WRITE(HOLD,'(1x,f7.3,3f11.3)')THK(ISUR,IE),CON(ISUR,IE),
     &                             DEN(ISUR,IE),SHT(ISUR,IE)
        H(1)='The conductivity can vary from about 0.03 W/(mK) for'
        H(2)='insulation to about 200 W/(mK) for copper'
        H(3)=' '
        H(4)='The density can vary from about 30 kg/m^3 for'
        H(5)='insulation to about 8900 kg/m^3 for copper'
        H(6)=' '
        H(7)='Specify the specific heat in J/(kgK)'
        H(8)=' '
        H(9)='Specify the thickness of the layer (m)'
        CALL EASKS(HOLD,
     &    ' Layer thickness(m), conductivitiy, density, specific heat:',
     &    '  ',48,' 0.025 1.0 100. 100. ','thk con den spht',IER,9)
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
            H(1)='Enter air gap resistance.'
            H(2)='Typically this is 0.18 m^2K/W for a cavity wall'
            VAL=RAIRG(ISUR,IGG)
            CALL EASKR(VAL,' ',' Air gap resistance ? ',
     &        0.001,'W',9.99,'W',0.017,'air gap R',IER,2)
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
      SUBROUTINE EDZCON(ITRC,ICOMP,MODGEO,MODCON,IER)
#include "building.h"
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)

      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)

      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Arrays of material properties.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      real matdbcon,matdbden,matdbsht,matdboute,matdbine
      real matdbouta,matdbina,matdbdrv,matdbthick
      common/matdatarray/matlegindex(MGIT),matcatindex(MGIT),
     &  matdbcon(MGIT),matdbden(MGIT),matdbsht(MGIT),matdboute(MGIT),
     &  matdbine(MGIT),matdbouta(MGIT),matdbina(MGIT),matdbdrv(MGIT),
     &  matdbthick(MGIT)

      integer mathash
      common/mathasharray/mathash(0:600)

      character matname*32,matdoc*248,matopaq*1
      common/matnamearray/matname(MGIT),matdoc(MGIT),matopaq(MGIT)

      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the indes within matdatarray

      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,MODGEO,MODCON
      logical closemat

      CHARACTER outs*124,SSPARENT*12
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      CHARACTER DESC*48,SOTHER*15,SNAME*12,SMLCN*12,SVFC*4,SOTF*4
      CHARACTER NAM*72,TITL*72

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

 244  DO 10 IS=1,NSUR

C Take the composite name associated with this surface and find the
C equivalent composite db.  If composite name is UNKN then must ask
C the user to select one.
        NAIRG(IS)=0
        IF(SMLCN(IS)(1:4).EQ.'UNKN')THEN
  245     WRITE(outs,'(3A)')' Composition of ',SNAME(ICOMP,IS),
     &                         ' is unknown.'
          CALL EPKMLC(ISEL,outs,'Please select one...',IER)
          IF(ISEL.GT.0)THEN
            ioc=IZSTOCN(ICOMP,is)
            WRITE(SMLCN(IS),'(A)')DESC(ISEL)(1:12)
            SSMLCN(ioc)=SMLCN(is)
            if(DESC(ISEL)(15:18).EQ.'TRAN')then
              SOTF(IS)='TRAN'
              SSOTF(ioc)='TRAN'
              ITMCFL(ICOMP,IS)=1
            endif
            if(DESC(ISEL)(15:18).EQ.'OPAQ')then
              SOTF(IS)='OPAQ'
              SSOTF(ioc)='OPAQ'
              ITMCFL(ICOMP,IS)=0
            endif
C            IF(DESC(ISEL)(15:18).EQ.'TRAN')SOTF(IS)='TRAN'
C            IF(DESC(ISEL)(15:18).EQ.'OPAQ')SOTF(IS)='OPAQ'

C Remember this change by saving the file.....
            MODGEO=.TRUE.
            MODCON=.TRUE.
          ELSE
            GOTO 245
          ENDIF
        ENDIF

C Reset number of layers, if at end of loop it is still 0 then
C the material was not found and must be asked for.
        call eclose(matver,1.1,0.01,closemat)
        NE(IS)=0
        DO 20 IC=1,NMLC
          IF(DESC(IC)(1:12).EQ.SMLCN(IS))THEN
            MODCON=.TRUE.
            NE(IS)=LAYERS(IC)

C Gather information about each layer. Read in the materials db info.
            DO 23, IL=1,LAYERS(IC)
              if(closemat)then
                legmat=IPR(IC,IL)   ! which legacy index
                if(legmat.ge.0)then
                  matarrayindex=mathash(legmat)  ! find matching array index
                  DBCON=matdbcon(matarrayindex)
                  DBDEN=matdbden(matarrayindex)
                  DBSHT=matdbsht(matarrayindex)
                  E=matdbine(matarrayindex)
                  A=matdbina(matarrayindex)
                  DRV=matdbdrv(matarrayindex)
                  write(TITL,'(a)') 'placeholder'
                  write(NAM,'(a)') matname(matarrayindex)(1:32)
                endif
              else
                CALL ERPCDB(IFMAT,IPR(IC,IL),iuout,IER,DBCON,DBDEN,
     &             DBSHT,E,A,DRV,TITL,NAM)
                IF(IER.EQ.1)RETURN
              endif
              CON(IS,IL)=DBCON
              DEN(IS,IL)=DBDEN
              SHT(IS,IL)=DBSHT
              THK(IS,IL)=DTHK(IC,IL)

C Assign surface properties.
              IF(IL.EQ.1)         EMISE(IS)=E
              IF(IL.EQ.LAYERS(IC))EMISI(IS)=E
              IF(IL.EQ.LAYERS(IC))ABSI(IS)=A
              IF(IL.EQ.1)         ABSE(IS)=A
              IF(IPR(IC,IL).EQ.0)THEN

C Keep track of the air gaps in the composite, these variables may
C eventually be redundant. Based on the orientation of the surface
C assign an appropriate U value to each air gap encountered.
                if(NAIRG(IS)+1.gt.MGP)then
                  call usrmsg(
     &           ' As defined, this composite contains more air gaps',
     &           ' then allowed - please change your description','W')
                else
                  NAIRG(IS)=NAIRG(IS)+1
                  IPAIRG(IS,NAIRG(IS))=IL
                  IF(SVFC(IS).EQ.'VERT')THEN
                    RAIRG(IS,NAIRG(IS))=DRAIR(IC,IL,1)
                  ELSEIF(SVFC(IS).EQ.'FLOR'.OR.SVFC(IS).EQ.'CEIL')THEN
                    RAIRG(IS,NAIRG(IS))=DRAIR(IC,IL,2)
                  ELSE
                    RAIRG(IS,NAIRG(IS))=DRAIR(IC,IL,3)
                  ENDIF
                endif
              ENDIF
   23       CONTINUE
          ENDIF
   20   CONTINUE
        if(NE(IS).eq.0)then
          write(outs,'(5A)') ' The construction ',SMLCN(IS),
     &         ' of ',SNAME(ICOMP,IS),' was not found in the mlc db...'
          call usrmsg(outs,'resetting attribute to UNKNOWN.','W')
          ioc=IZSTOCN(ICOMP,is)
          SMLCN(IS)='UNKNOWN'          
          SSMLCN(ioc)='UNKNOWN'          
          goto 244
        endif

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
C ITRU unit number for user output, IER=0 OK, IER=1 problem.

C Work out where the various definitive information sources should be
C combined to create this file.  In essence we need to read through
C each of the existing surfaces and find the unique optical types which
C exist in this zone.  Then for each of these we need to write out their
C optical properties as gathered from the optical database.  Make use of
C the number of composite layers to build this up.
      SUBROUTINE MKTWIN(ITRU,IFU,ICOMP,QUIET,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

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
      logical QUIET,XST

      dimension ival(MS)
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER zname*12,zdesc*64,H*72,TOPTIC*12,louts*248

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
            call edisp(iuout,' Creating a new TMC file...')
            call edisp(iuout,LTWIN(ICOMP))
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
     &  zname(ICOMP)(1:lnblnk(zname(ICOMP))),
     &  LTWIN(ICOMP)(1:lnblnk(LTWIN(ICOMP)))
  30  FORMAT('# transparent properties of ',a,' defined in ',a)

C Build one or two packed strings and write out. Should be good
C for up to NSUR of ~62.
      WRITE(IFU,'(I4,A)',IOSTAT=ISTAT,ERR=98)NSUR,'   # surfaces'
      do 32 ij=1,MS
        ival(ij)=ITMCFL(icomp,ij)
  32  continue
      WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)'# tmc index for each surface'
      itrunc=1
      ipos=1
      do while (itrunc.ne.0)
        call ailist(ipos,nsur,ival,MS,'C',louts,loutlen,itrunc)
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
        H(1)=' The TMC file does not exist, or was not found, '
        H(2)='please create / rereference this file individually.'
        CALL PHELPD('file error',2,'-',0,0,IER)
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

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)

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

      CHARACTER OPT*12,lastopt*12,TOPTIC*12,GDESCR*36,H*72
      CHARACTER DESC*48,outs*124
      CHARACTER SOTHER*15,SNAME*12,SMLCN*12,SVFC*4,SOTF*4
      CHARACTER zname*12,zdesc*64,msg*28
      
C If tmc data has already been read in (ie NTMC>0) then move on to
C a display of the current data.
      IF(NTMC.GT.0)GOTO 90

C Loop through each surface and if transparent get its composite
C description (DESC) and remember its index ICF, otherwise set ITMCFL
C to 0.
      lastopt='  '
      DO 10 IS=1,NSUR
        IF(SOTF(IS).NE.'TRAN')THEN
          ITMCFL(ICOMP,IS)=0
          GOTO 10
        ENDIF

 246    ICF=-1
        DO 20 IC=1,NMLC
          IF(DESC(IC)(1:12).EQ.SMLCN(IS))ICF=IC
  20    CONTINUE
        IF(ICF.EQ.-1)THEN
          WRITE(outs,'(A,A12)')
     &    ' select a transparent one for: ',SNAME(ICOMP,IS)
          CALL EPKMLC(ISEL,'From the available composites please',
     &      outs,IER)
          IF(ISEL.GT.0)THEN
            WRITE(SMLCN(IS),'(A12)')DESC(ISEL)(1:12)
            ICF=ISEL
          ELSE
            RETURN
          ENDIF
        ENDIF

C Check this composite db entry is also transparent and that it has
C an associated set of optical properties. If no equivalent set of
C optical properties then the user will have to supply this manually.
        WRITE(outs,'(A12,A)') DESC(ICF)(1:12),' is not transparent!'
        IF(DESC(ICF)(15:18).NE.'TRAN')THEN
          CALL EASKAB(outs,' do you want to:','switch to opaque',
     &    'select a transparent material',IWB,0)
          if(IWB.eq.1)then
  
C Switch back to opaque construction.
            CALL EPKMLC(ISEL,'Which OPAQUE construction?',' ',IER)
            if(ISEL.GT.0)then
              WRITE(SMLCN(IS),'(A12)')DESC(ISEL)(1:12)
              ICF=ISEL
              ITMCFL(ICOMP,IS)=0
              SOTF(IS)='OPAQ'
              OPT='OPAQUE'
              goto 10
            endif
          elseif(IWB.eq.2)then
            CALL EPKMLC(ISEL,'Which TRANSPARENT construction?',
     &        ' ',IER)
            if(ISEL.GT.0)then
              WRITE(SMLCN(IS),'(A12)')DESC(ISEL)(1:12)
              ICF=ISEL
            endif
          endif
        ENDIF

C Check to see if this new construction has a known set of opticals.
        WRITE(OPT,'(A)') DESC(ICF)(21:32)
        IF(OPT.EQ.' '.OR.OPT.EQ.'UNKNOWN'.OR.OPT.EQ.'OPAQUE')THEN
          WRITE(outs,'(1X,A12,A)')SMLCN(IS),
     &      ' has unknown optical properties!'
          h(1)='Supplying optical properties if you know the angular'
          h(2)='transmission and absorption data. '
          CALL EASKABC(outs,' Do you want to: ',
     &      'supply optical properties','select another composite',
     &      'assign `opaque` to this one',IW,2)
          IF(IW.EQ.1)THEN

C << todo consider how one might treat a cancel request within eoptks
            NTL=LAYERS(ICF)
            CALL EOPTKS(ITRC,ITRU,'T',IER)
          ELSEIF(IW.EQ.2)THEN
            GOTO 246
          else
            ITMCFL(ICOMP,IS)=0
            SOTF(IS)='OPAQ'
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
      CALL EDISP(ITRU,' ')
      WRITE(outs,9996)zname(ICOMP)(1:lnblnk(zname(ICOMP)))
 9996 FORMAT(' Transparent construction file details for ',A)
      CALL EDISP(ITRU,outs)
      CALL EDISP(ITRU,' ')
      CALL EDISP(ITRU,' Surface      Construction OPAQ/  Optical   ')
      CALL EDISP(ITRU,' Name         Description  TRANS  Reference ')
      DO 31, ISR=1,NSUR
        WRITE(outs,'(1X,A12,2X,A12,2X,A4,I4)')SNAME(ICOMP,ISR),
     &        SMLCN(ISR),SOTF(ISR),ITMCFL(ICOMP,ISR)
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
            msg=' Lightswitch2002.'
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
        h(1)='You interupted the specification. If you want to start'
        h(2)='again say yes or if you want to not have optical controls'
        h(3)='say no and the data will be cleared. '
        CALL ASKOK('Try again to define the optical control?',
     &    '(A `no` clears the control definitions.)',OK,
     &    dok,3)
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
          h(1)='There is a surface attribute that indicates that an'
          h(2)='existing optical control. You have the following '
          h(3)='options: '
          h(4)=' a) remove this optical control '
          h(5)=' b) edit the current optical control'
          h(6)=' c) continue with out changing this control'
          h(7)=' '
          h(8)='Hint - scroll the text feedback backwards to see the'
          h(9)='current optical control details. '
          WRITE(OUTS,'(A,I2,A)')' For TMC type ',IT,
     &      ' there is an optical control. Options:'
          CALL EASKABC(outs,' ','remove this optical control',
     &      'edit this optical control','accept control as-is',iwa,9)
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
          h(1)='There is a surface attribute that indicates that an'
          h(2)='optical control could be defined. To do it say `yes`. '
          WRITE(OUTS,'(A,I2,A)')'For TMC type ',IT,' is there any'
          CALL ASKOK(OUTS,'optical control?',OK,dok,2)
          IF(.NOT.OK)THEN
            IBCMT(ICOMP,IT)=0
            GOTO 22
          ENDIF
        endif

        IBCMT(ICOMP,IT)=1
        N1=NBCTMC(ICOMP,IT)
        H(1)='Up to 8 control periods are allowed.'
        CALL EASKI(N1,' ','Number of control periods?',
     &    1,'F',MBP,'F',1,'no of control periods',IERI,1)
        if(ieri.eq.-3) then
          canceled=.true.
          goto 203
        endif
        NBCTMC(ICOMP,IT)=N1

        N2=IBCSUR(ICOMP,IT)
        H(1)='The position of the sensor may be as follows.'
        H(2)=' * each transparent surface has its own'
        H(3)='   sensor (default);'
        H(4)=' * one user defined external surface will'
        H(5)='   contain the sensor for all surfaces which'
        H(6)='   use this particular TMC type;'
        H(7)=' * one user defined internal Daylight Coefficient'
        H(8)='   sensor is employed.'
        CALL EASKAB(' ','Transparent surfaces controlled by:',
     &    'separate sensors','a single sensor',IW,8)
        if(IW.eq.1)then
          IBCSUR(ICOMP,IT)=0
        else
          CALL EASKI(N2,' ','Surface for common sensor location?',
     &      1,'F',NSUR,'F',1,'sensor location',IERI,8)
          if(ieri.eq.-3) then
            canceled=.true.
            goto 203
          endif
          IBCSUR(ICOMP,IT)=N2
        endif

C For each control period get the appropriate replacement properties.
        DO 24 ICP=1,N1
          write(outs,'(A,I2)')' For control period ',ICP
          H(1)='Control period starting and ending times should'
          H(2)='define consecutive periods.'
          VAL=REAL(IBCST(ICOMP,ICP,IT))
          CALL EASKR(VAL,outs,' Begin at hour: ',
     &      0.00,'W',24.0,'W',0.0,'control start',IER,2)
          IBCST(ICOMP,ICP,IT)=INT(VAL)

          VAL=REAL(IBCFT(ICOMP,ICP,IT))
          CALL EASKR(VAL,outs,' End at hour  : ',
     &      0.00,'W',24.0,'W',24.0,'control end',IER,2)
          IBCFT(ICOMP,ICP,IT)=INT(VAL)

          N3=NBCTT(ICOMP,ICP,IT)
          H(1)='Surfaces composed of transparent constructions'
          H(2)='will have optical properties defined from either' 
          H(3)='the Optical Properties db or by manual input. If'
          H(4)='there is a mismatch between information in a zone'
          H(5)='TMC file and existing surface attributes then you'
          H(6)='will be asked to confirm the correct choice.'
          H(7)=' '
          H(8)='Optical properties can have scheduled controls which'
          H(9)='allow an alternative optical property to be use based'
          H(10)='on the following sensed parameter:'
          H(11)=' a) time - alternative optics are used during a'
          H(12)='    specific period of the day;'
          H(13)=' b) total radiation on the outside face of a'
          H(14)='    surface in the zone - you specify a radiation'
          H(15)='    level (W/m^2) above which the alternate optical'
          H(16)='    properties will be used;'
          H(17)=' c) ambient temperature - you specify a temperature'
          H(18)='    above which the alternative optical properties'
          H(19)='    will be used;'
          H(20)=' d) zone air temperature - you specify the'
          H(21)='    temperature above which the alternative optical'
          H(22)='    properties will be used;'
          H(23)=' e) lux level - you specify the lux level above'
          H(24)='    which the alternative optical properties will'
          H(25)='    be used. Two alternative lux calculation'
          H(26)='    approaches are offered.'
          H(27)=' '
          H(28)='Note: there can be up to 8 control periods in a'
          H(29)='day (all days within the simulation will be treated'
          H(30)='the same).'
          H(31)=' '
          H(32)='The alternative optics must correspond to the same'
          H(33)='number of layers as the initial set. It is a good'
          H(34)='idea to prepare the alternative properties via the'
          H(35)='Optical Properties db management facility prior to'
          H(36)='using the transparent properties facility here.'
          H(37)=' '
          H(38)='Control of optical properties can be used to'
          H(39)='approximate closing blinds/shutters and shading'
          H(40)='devices.'
          H(41)=' '
          H(42)='If the model has an air flow network you might also'
          H(43)='think about controlling any air flow components'
          H(44)='associated with any transparent surfaces that have'
          H(45)='been assigned optical property control. Note however'
          H(46)='that the air flow control logic is different from'
          H(47)='the optical property control logic so some care and'
          H(48)='testing is required to ensure that optical and flow'
          H(49)='control works together.'
          if(n3.eq.-99)then
            call EASKATOG('Controlled on:','(currently time)',
     &      'time','incident radiation','ambient temp.','zone temp.',
     &      'lux level','LtSwitch','cancel',IRT,49)
          elseif(n3.eq.0)then
            call EASKATOG('Controlled on:','(currently radiation)',
     &      'time','incident radiation','ambient temp.','zone temp.',
     &      'lux level','LtSwitch','cancel',IRT,49)
          elseif(n3.eq.1)then
            call EASKATOG('Controlled on:','(currently ambient temp.)',
     &      'time','incident radiation','ambient temp.','zone temp.',
     &      'lux level','LtSwitch','cancel',IRT,49)
          elseif(n3.eq.2)then
            call EASKATOG('Controlled on:','(currently zone temp.)',
     &      'time','incident radiation','ambient temp.','zone temp.',
     &      'lux level','LtSwitch','cancel',IRT,49)
          elseif(n3.eq.3)then
            call EASKATOG('Controlled on:','(currently daylight coef)',
     &      'time','incident radiation','ambient temp.','zone temp.',
     &      'lux level','LtSwitch','cancel',IRT,49)
          elseif(n3.eq.4)then
            call EASKATOG('Controlled on:','(currently Lightswitch)',
     &      'time','incident radiation','ambient temp.','zone temp.',
     &      'lux level','LtSwitch','cancel',IRT,49)
          else
            call EASKATOG('Controlled on:','(currently undefined)',
     &      'time','incident radiation','ambient temp.','zone temp.',
     &      'lux level','LtSwitch','cancel',IRT,49)
          endif
          if(irt.eq.1)then
            NBCTT(ICOMP,ICP,IT)= -99
            BACTPT(ICOMP,ICP,IT)=0.
          elseif(irt.eq.2)then
            NBCTT(ICOMP,ICP,IT)= 0
            VAL=BACTPT(ICOMP,ICP,IT)
            CALL EASKR(VAL,' ','Radiation set point?',
     &      0.00,'W',1000.0,'W',0.0,'radiation set point',IER,49)
            BACTPT(ICOMP,ICP,IT)=VAL
          elseif(irt.eq.3)then
            NBCTT(ICOMP,ICP,IT)= 1
            VAL=BACTPT(ICOMP,ICP,IT)
            CALL EASKR(VAL,' ','Ambient temp. set point?',
     &      0.00,'W',1000.0,'W',0.0,'ambient set point',IER,49)
            BACTPT(ICOMP,ICP,IT)=VAL
          elseif(irt.eq.4)then
            NBCTT(ICOMP,ICP,IT)= 2
            VAL=BACTPT(ICOMP,ICP,IT)
            CALL EASKR(VAL,' ','Zone temp. set point',
     &      0.00,'W',1000.0,'W',0.0,'zone set point',IER,49)
            BACTPT(ICOMP,ICP,IT)=VAL
          elseif(irt.eq.5)then
            NBCTT(ICOMP,ICP,IT)= 3
            VAL=BACTPT(ICOMP,ICP,IT)
            CALL EASKR(VAL,' ','Lux set point',
     &      0.00,'W',1000.0,'W',0.0,'lux set point',IER,49)
            BACTPT(ICOMP,ICP,IT)=VAL
          elseif(irt.eq.6)then

C << Do not know what sort of question is to be asked here! >>
            NBCTT(ICOMP,ICP,IT)= 4
            VAL=BACTPT(ICOMP,ICP,IT)
            CALL EASKR(VAL,' ','Lightswitch lux set point?',
     &      0.00,'W',1000.0,'W',0.0,'LtSw illum. set point',IER,49)
            BACTPT(ICOMP,ICP,IT)=VAL
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
  98  CALL USRMSG('A problem was encountered reading the Ooptical',
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
      common/pophelp/h(60)

      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB
      CHARACTER SOPT*12,GDESCR*36,H*72,HOLD*42
      CHARACTER WHICH*1,outs*124

      integer ntlt   ! for local editing.

C The user will be allowed to use a item from the optical database or
C to put in the data directly.  77 is a jump back point in case the
C user cancels part way through the definition.

C << todo: consider adding a cancel option >>

  77  h(1)='Manual data input will ask for the direct transmission'
      h(2)='at each angle as well as absorption characteristics for'
      h(3)='layer. Using data from an optical database simplifies '
      h(4)='the process. '
      CALL EASKAB(' Get optical properties from: ',' ',
     &  ' manual data input',' optical database',IW,4)
      IF(IW.EQ.2)THEN
        CALL EDWINO(SOPT,IER)
        IF(SOPT.EQ.'UNKNOWN')RETURN
        CALL EROPTDB(ITRC,ITRU,SOPT,GDESCR,IER)
        RETURN
      ENDIF

      H(1)='The simulation of radiation transmission through '
      H(2)='glazing is angular dependent.  Five angles are '
      H(3)='used: 0 (normal), 40, 55, 70 & 80 degrees and the'
      H(4)='data can be obtained via program win.'
  42  HOLD=' '
      WRITE(HOLD,'(1x,5f8.3)')DG(1),DG(2),DG(3),DG(4),DG(5)
      CALL EASKS(HOLD,' Direct trans @ 0 40 55 70 80 degrees: ',
     &   '  ',42,' 0.779 0.759 0.717 0.581 0.348 ','dir trans',IER,4)
      K=0
      CALL EGETWR(HOLD,K,DG(1),0.001,0.99,'W','dir t @ 0',IER)
      CALL EGETWR(HOLD,K,DG(2),0.001,0.99,'W','dir t @ 40',IER)
      CALL EGETWR(HOLD,K,DG(3),0.001,0.99,'W','dir t @ 55',IER)
      CALL EGETWR(HOLD,K,DG(4),0.001,0.99,'W','dir t @ 70',IER)
      CALL EGETWR(HOLD,K,DG(5),0.001,0.99,'W','dir t @ 80',IER)
      if(ier.ne.0)goto 42

      H(1)='The visible transmittance at normal incidence is'
      H(2)='used for daylighting calculations.'
      CALL EASKR(VTRN,' ',' Visible transmittance: ',
     &  0.001,'W',0.99,'W',0.85,'vis tran',IER,2)

      IF(WHICH.EQ.'w'.OR.WHICH.EQ.'W')THEN
        call usrmsg(
     &    'The concept of total heat gain factors is no longer used in',
     &    'esp-r. It uses angular transmission and layer absorb.','W')
      ELSEIF(WHICH.EQ.'t'.OR.WHICH.EQ.'T')THEN

C For each of the layers provide the absorptions.
        H(1)='This is almost always equal to that of the associated'
        H(2)='multilayer construction. Layers index (outside=1).'
        H(3)='If you cancel you will be taken back to the start'
        H(4)='of the editing sequence. '
        ntlt=ntl
        CALL EASKI(NTLT,' ',
     &    ' Current no. of layers (including air gaps)? ',
     &    1,'F',ME,'F',1,'layers in tmc',IERI,4)
        if(ieri.eq.-3)then
          goto 77
        endif
        ntl=ntlt

        H(1)='The simulation of radiation transmission through '
        H(2)='transparent constructions requires that the short-'
        H(3)='wave absorptance at each layer (including air gaps)'
        H(4)='is defined for five angles of incidence: '
        H(5)='  0 (normal), 40, 55, 70 & 80 degrees '
        H(6)='This data can be obtained via program window.'
        H(7)='The values for an air gap are usually: '
        H(8)='   0.001, 0.002, 0.003, 0.004, 0.005 '
        DO 10 I=1,NTL
  44      HOLD=' '
          WRITE(HOLD,'(1x,5f8.3)')AB(I,1),AB(I,2),AB(I,3),AB(I,4),
     &       AB(I,5)
          WRITE(outs,'(a,I2,a)') ' Layer ',I,
     &       ' absorption @ 0 40 55 70 80 degrees:'
          CALL EASKS(HOLD,outs,'  ',
     &      42,' 0.106 0.116 0.124 0.129 0.125 ','layer abs',IER,8)
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
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C1/NCOMP,NCON
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)

c ITCNST  - number of start-up days
      COMMON/PREC7/ITCNST

C Arrays of material properties.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      real matdbcon,matdbden,matdbsht,matdboute,matdbine
      real matdbouta,matdbina,matdbdrv,matdbthick
      common/matdatarray/matlegindex(MGIT),matcatindex(MGIT),
     &  matdbcon(MGIT),matdbden(MGIT),matdbsht(MGIT),matdboute(MGIT),
     &  matdbine(MGIT),matdbouta(MGIT),matdbina(MGIT),matdbdrv(MGIT),
     &  matdbthick(MGIT)

      integer mathash
      common/mathasharray/mathash(0:600)

      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the indes within matdatarray

      character NAM*72,TITL*72,outs*124
      character SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character MLCN*12,DESC*48,SSPARENT*12
      logical found,closemat

C Loop mlc db, if some surface references it, read properties for
C each layer and keep track of time constant.
      ITCNST=1
      TCM=0.
      TDM=0.
      do 40 ic=1,nmlc

c First determine maximum thermal diffusivity and associated
c homogeneous layer.
        WRITE(MLCN,'(A12)')DESC(ic)(1:12)
        lmlc=lnblnk(MLCN)
        found=.false.
        do 42 icn=1,ncon
          if(MLCN(1:lmlc).eq.SSMLCN(icn)(1:lnblnk(SSMLCN(icn))))then
            found=.true.
            goto 43
          endif
  42    continue
  43    if(found)then
          call eclose(matver,1.1,0.01,closemat)
          RES=0.
          CAP=0.
          DO 45 il=1,LAYERS(ic)
            if(closemat)then
              legmat=IPR(IC,IL)   ! which legacy index
              if(legmat.ge.0)then
                matarrayindex=mathash(legmat)  ! find matching array index
                DBCON=matdbcon(matarrayindex)
                DBDEN=matdbden(matarrayindex)
                DBSHT=matdbsht(matarrayindex)
                E=matdbine(matarrayindex)
                A=matdbina(matarrayindex)
                DRV=matdbdrv(matarrayindex)
              endif
            else
              CALL ERPCDB(IFMAT,IPR(ic,il),IUOUT,IER,DBCON,DBDEN,
     &           DBSHT,E,A,DRV,TITL,NAM)
            endif

C Get properties for outer layer.
            if(il.eq.1)then
              thk1=DTHK(ic,il)
              con1=DBCON
              den1=DBDEN
              sht1=DBSHT
            endif

C If an air gap, recalculate overall resistance.
            if(IPR(ic,il).eq.0)then
             RES=RES+((DRAIR(ic,il,1)+DRAIR(ic,il,2)+DRAIR(ic,il,3))/3.)
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
     &  DESC(istd)(1:12),' (',istd,') Layer',IE
      call edisp(iuout,outs)
      write(outs,'(A,G12.4,A)')' Maximum time constant is',TCM,' secs.'
      call edisp(iuout,outs)
      write(outs,'(A,A)')' Associated MLC construction is',
     &  DESC(ISTC)(1:12)
      call edisp(iuout,outs)
      write(outs,'(A,I3,A)')' Simulation start-up is',ITCNST,' days.'
      call edisp(iuout,outs)

      return
      end
