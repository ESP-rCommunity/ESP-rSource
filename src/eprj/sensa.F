C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOtems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free softwareUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C Controlling routines for parametric studies.
C   UNCERTA:  This is the top level uncertainty definition menu. 
C   DISPOPT:  Displays the options available for the chosen 
C             type: change / location / link.
C   CNGDEF:   Uncertainty limits definition routine.
C   LOCDEF:   Uncertainty location definition routine.
C   LINKDEF:  Uncertainty limits and location connection definition 
C             routine.
C   DISPC     Generates a list of the unique materials used in
C             the model.
C   CALIB:    Top level facility for invoking sensitivity assessments.
C clearcalibarrays: figures out the names of the files and the
C             labels that will be created or used during a calibration.
C clearcshcsvfiles: clears csv and csh files from prior calibration work.
C createcalvariantfiles: creates variant cfg and zone files based on
C             the uncertainties that have been defined e.g. domlc,dogeo
C docalibrationruns: invokes the simulations needed for calibration.
C doextractsetdata: creates and executes shell scripts to recover
C             data from a MontiCarlo results set.
C docreatescripts: concatenates csv file names needed to create tar.gz
C             files to send to server based Calibro or local Calibro.
C runlocalorserver: invokes calibro runs locally or on a server.
C createjsontable: fills the jsontable array based on scanning a number of
C             json files.
C printjsontable prints the common blocks jsontable & jsontabletext.

C EGETJSNTAGPHR parses a json tag and phrase from a STRING
C EGETJSNTAGR parses a json tag and real number from the STRING
C splitstratchar splits a string at a specific character returning 2 str.
C UMLCAE02    Edits layer thicknesses in MLC db.
C UCASAE04    Edits casual gains and scheduled air movement in zone files.
C UAEDIT      Edits the given data item depending on change flag (copy)
C UMATAE01    Updates conductivity, density or specific heat values in 
C             model databases and zone files.
C CHKPRIM     Scans the layers of the MLC for a specific surface (copy)
C UCTLAT01    Identifies control data should be changed for calibration.
C

C ******************** UNCERTA ********************

      subroutine UNCERTA()
#include "building.h"
#include "model.h"
#include "uncertainty.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      common/rpath/path
     
      CHARACTER*33 ITEM(11)
      character path*72
      CHARACTER LTEMP*72,DTEMP*72,outs*124
      integer NITMS,INO ! max items and current menu item

      LOGICAL LIBXST

      helpinsub='sensa'  ! set for subroutine
      
      IUALF=IFIL
      helptopic='uncert_overview_a'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
C Ask for an uncertainty definition name and check to see if it exists.
 4    IW=0
      if(path.ne.'./'.and.path.ne.' ')then
        write(outs,'(A,A)') ' The current path is: ',path
        call edisp(iuout,outs)
        CALL EASKAB(' You are working in a remote folder...',
     &              ' place library:','using the path',
     &              'in local folder',IW,nbhelp)
      endif

C Create a sensible default name.
      if (LUALF(1:4).eq.'UNKN'.or.LUALF(1:4).eq.'unkn') then
        write (LTEMP,'(a,a)') cfgroot(1:lnblnk(cfgroot)),'.ual'
        DTEMP=LTEMP
      else
        write (DTEMP,'(a,a)') cfgroot(1:lnblnk(cfgroot)),'.ual'
        LTEMP=LUALF
      endif
      call EASKS(LTEMP,' Uncertainty definitions file name ?',
     &                      ' ',72,DTEMP,'UA library',IER,nbhelp)
      if (IER.eq.0) then
        LUALF=LTEMP
      else
        goto 4
      endif

C Check to see if file already exists.
      INQUIRE (FILE=LUALF,EXIST=LIBXST)

C Does this library contain results ?
      IF (LIBXST) then
        call edisp(iuout,'  ')
        write(outs,'(A,A)')
     &     ' Reading contents of existing UA definitions file : ',LUALF
        call edisp(iuout,outs)
        call READUAL(IUALF)
      else
        call edisp(iuout,'  ')
        write(outs,'(A,A)')
     &       ' Creating new UA definitions file : ',LUALF
        call EFOPSEQ(IUALF,LUALF,3,IER)
        call ERPFREE(IUALF,ISTAT)
        call edisp(iuout,outs)

C Set the number of distributions, locations and uncertainties 
C defined to zero. 
        NICNG=0; NILOC=0; NIACT=0
      endif


C Set up initial menu.
   10 INO=-2
      ITEM(1) ='a create default range defs      '
      ITEM(2) =' --------------------------------'
      ITEM(3) ='b define/edit distributions      '
      ITEM(4) ='c define/edit locations          '
      ITEM(5) =' ------------------------------- '
      ITEM(6) ='d link distributions to locations'
      ITEM(7) =' ------------------------------- '
      ITEM(8) ='! list current uncertainties     '
      ITEM(9) ='> save uncertainties             '
      ITEM(10)='? help                           '
      ITEM(11)='- exit menu'
      NITMS=11

   12 CALL EMENU('Uncertainty definition',ITEM,NITMS,INO)
      if (INO.EQ.NITMS) then

C Check if the library has been saved and closed.
        return
      elseif (INO.EQ.(NITMS-1)) then

C HELP!
        helptopic='uncert_overview_a'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Uncertainty definitions file',nbhelp,'-',0,0,IER)
      elseif (INO.EQ.(NITMS-2)) then

C Save uncertainty definitions.
        call WRITUAL(IUALF)
      elseif (INO.EQ.(NITMS-3)) then

C List uncertainty definitions.
        call LISTUAL(0,0)
      elseif (INO.EQ.1) then

C Set up defaults.
        call DEFUAL
      elseif (INO.EQ.3) then

C Call distribution definition menu.
        call DISPOPT(1,IUALF)
      elseif (INO.EQ.4) then

C Call location definition menu.
        call DISPOPT(2,IUALF)
      elseif (INO.EQ.6) then

C Link uncertainties with locations.
        call DISPOPT(3,IUALF)

      else
        ino=-1
        goto 12
      endif
      goto 10

      END

C ******************** DISPOPT ********************
C Displays the contents of the changes, locations or 
C actions common depending on the value of ITYP.
C ITYP: 1=changes, 2=locations, 3=actions

      subroutine DISPOPT(ITYP,IUALF)
#include "building.h"
#include "epara.h"
#include "uncertainty.h"
#include "help.h"

C muadel should be equivalent to max(MNCNG,MNIL,MNACT))
      parameter (muadel=50)

      character ta15(MNCNG)*24
      CHARACTER KEY*1,MLCITM(35)*31

      dimension IDEL(muadel)
      integer NITMS,INO ! max items and current menu item

      helpinsub='sensa'  ! set for subroutine
      
      IER=0
      if (ITYP.lt.1.and.ITYP.gt.3) return
      
C Generate help text for this subroutine.
      helptopic='uncert_display'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Create a menu showing the available database items.  Allow user to
C select one and then list details of this item, allowing editing.
C Setup for multi-page menu.
 3    MHEAD=0
      MCTLX=7
      if (ITYP.eq.1) then
        ILEN=NICNG
      elseif (ITYP.eq.2) then 
        ILEN=NILOC
      elseif (ITYP.eq.3) then 
        ILEN=NIACT
      endif
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      INO=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=0
      DO 20 IM=1,ILEN
        IF(IM.GE.IST.AND.(IM.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(M,KEY,IER)
          if (ITYP.eq.1) then
            lncng=lnblnk(LCNG(M))
            WRITE(MLCITM(M),22)KEY,LCNG(M)(1:lncng)
          elseif (ITYP.eq.2) then 
            WRITE(MLCITM(M),22)KEY,LLOC(M)(1:15)
          elseif (ITYP.eq.3) then
            lncng=lnblnk(LCNG(IACTD(M,1)))
            if(lncng.gt.21)lncng=21
            if(lncng.ge.18)then
              WRITE(MLCITM(M),23)KEY,LCNG(IACTD(M,1))(1:lncng),':',
     &                           LLOC(IACTD(M,2))(1:7)
            else
              WRITE(MLCITM(M),23)KEY,LCNG(IACTD(M,1))(1:lncng),':',
     &                           LLOC(IACTD(M,2))(1:11)
            endif
          endif
  22      FORMAT(A,1X,A)
  23      FORMAT(A,1X,3a)
        ENDIF
  20  CONTINUE

C Number of actual items displayed.
      NITMS=M+MCTLX

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        MLCITM(M+1)=' -------------------- '
      ELSE
        WRITE(MLCITM(M+1),15)IPM,MPM 
  15    FORMAT   ('0 ---Page: ',I2,' of ',I2,' ---')
      ENDIF
      MLCITM(M+2)='+ add definition        '
      MLCITM(M+3)='= delete definition     '
      MLCITM(M+4)='! list current          '
      MLCITM(M+5)='> update library        '
      MLCITM(M+6)='? help                  '
      MLCITM(M+7)='- exit menu'
      INO=-4

C Depending on ityp display different headings for menu.
  2   if (ITYP.eq.1) then
        CALL EMENU('Distributions',MLCITM,NITMS,INO)
      elseif (ITYP.eq.2) then
        CALL EMENU('Locations/Periods',MLCITM,NITMS,INO)
      elseif (ITYP.eq.3) then
        CALL EMENU('Uncertainties',MLCITM,NITMS,INO)
      endif

      IF(INO.EQ.NITMS)THEN
        return
      ELSEIF(INO.EQ.NITMS-1)THEN

C Produce help text for the menu.
        helptopic='uncert_display'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('UA file entries',4,'-',0,0,IER)
      ELSEIF(INO.EQ.NITMS-2)THEN

C Save data.
        call WRITUAL(IUALF)
      ELSEIF(INO.EQ.NITMS-3)THEN

C List data.
        call LISTUAL(ITYP,0)
      ELSEIF(INO.EQ.NITMS-4)THEN

C Delete entry.
C Present list of currently defined then remove chosen from it.
        INDEL=1
        IDEL(1)=0
        if (ITYP.eq.1.and.NICNG.ge.1) then
          call EPICKS(INDEL,IDEL,' ',' ',24,NICNG,LCNG,
     &      'Distributions',IER,nbhelp)
          if (IDEL(1).gt.0) then
            do 700 ID=IDEL(1),NICNG
              LCNG(ID)=LCNG(ID+1)
              IDTYPU(ID)=IDTYPU(ID+1)
              IDATR(ID,1)=IDATR(ID+1,1)
              IDATR(ID,2)=IDATR(ID+1,2)
              IDATR(ID,3)=IDATR(ID+1,3)
              IDATR(ID,4)=IDATR(ID+1,4)
              IDATR(ID,5)=IDATR(ID+1,5)
              DATU(ID,1)=DATU(ID+1,1)
              DATU(ID,2)=DATU(ID+1,2)
 700        continue
            NICNG=NICNG-1
          endif
        elseif (ITYP.eq.2.and.NILOC.ge.1) then
          call EPICKS(INDEL,IDEL,' ',' ',15,NILOC,LLOC,
     &      'Locations/Periods',IER,nbhelp)
          if (IDEL(1).gt.0) then
            do 701 ID=IDEL(1),NILOC
              LLOC(ID)=LLOC(ID+1)
              NZGU(ID)=NZGU(ID+1)
              do 711 IDCOMP=1,MCOM
                NZNOGU(ID,IDCOMP)=NZNOGU(ID+1,IDCOMP)
                NSGU(ID,IDCOMP)=NSGU(ID+1,IDCOMP)
                NTGU(ID,IDCOMP,1)=NTGU(ID+1,IDCOMP,1)
                NTGU(ID,IDCOMP,2)=NTGU(ID+1,IDCOMP,2)
                do 721 IDSUR=1,MCOM
                  NSNOG(ID,IDCOMP,IDSUR)=NSNOG(ID+1,IDCOMP,IDSUR)
 721            continue
 711          continue
 701        continue
            NILOC=NILOC-1
          endif
        elseif (ITYP.eq.3.and.NIACT.ge.1) then
          do 799 I=1,NIACT
           lncng=lnblnk(LCNG(IACTD(I,1)))
           if(lncng.gt.12) lncng=12
           write(ta15(I),'(2a)')LCNG(IACTD(I,1))(1:lncng),
     &       LLOC(IACTD(I,2))(1:7)
 799      continue
          call EPICKS(INDEL,IDEL,' ',' ',24,NIACT,ta15,
     &      'Uncertainties',IER,nbhelp)
          if (IDEL(1).gt.0) then
            do 702 ID=IDEL(1),NIACT
              IACTD(ID,1)=IACTD(ID+1,1)
              IACTD(ID,2)=IACTD(ID+1,2)
 702        continue
            NIACT=NIACT-1
          endif
        endif
      ELSEIF(INO.EQ.NITMS-5)THEN

C Add entry. If error 101 then do not save the new entry.
        if (ITYP.eq.1) then
          IFOC=0
          call CNGDEF(IFOC,IER)
          if (IER.eq.101) NICNG=NICNG-1
        elseif (ITYP.eq.2) then 
          IFOC=0
          call LOCDEF(IFOC,IER)

C If ier is 101 or 2 then do nothing more (niloc was already
C decremented within locdef).
          if (IER.eq.101) goto 2
          if (IER.eq.2) goto 2
        elseif (ITYP.eq.3) then 
          IFOC=0
          call LINKDEF(IFOC,IER)
          if (IER.eq.101) NIACT=NIACT-1
        endif
      elseif(INO.ge.1.and.INO.le.M) then
        CALL KEYIND(NITMS,INO,IFOC,IO)

C Edit an existing definition.
        if (ITYP.eq.1) then
          call CNGDEF(IFOC,IER)
          if (IER.eq.101) NICNG=NICNG-1
        elseif (ITYP.eq.2) then 
          call LOCDEF(IFOC,IER)

C If ier is 101 or 2 then do nothing more (niloc was already
C decremented within locdef).
          if (IER.eq.101) goto 2
          if (IER.eq.2) goto 2
        elseif (ITYP.eq.3) then 
          call LINKDEF(IFOC,IER)
          if (IER.eq.101) NIACT=NIACT-1
        endif
      else
        goto 2
      endif
      INO=-4
      goto 3

      END


C ******************** DEFUAL ********************
C Create a set of default uncertainties.

      subroutine DEFUAL
#include "building.h"
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      IERR=0
      IER=0

C <Need controlling menu here to select uncertainty category.>

C IDTYPU = 3 : Weather parameters
C        IDTYPU(ICDEF)=3
C        call SDDFLT003

C IDTYPU = 1 : Materials properties
        call SDDFLT001

C IDTYPU = 2 : Composite constructions
C        IDTYPU(ICDEF)=2
C        call SDDFLT002

C IDTYPU = 101 : Zone geometry
C        IDTYPU(ICDEF)=101
C        call SDDFLT101

C IDTYPU = 4 : Operations
C        IDTYPU(ICDEF)=4
C        call SDDFLT004

C IDTYPU = 5 : Convection coefficients
C        IDTYPU(ICDEF)=5
C        call SDDFLT005

C IDTYPU = 6 : blind controls
C        IDTYPU(ICDEF)=6
C        call SDDEF006

C IDTYPU = 1001 : Control definitions
C        IDTYPU(ICDEF)=1001
C        call SDDEF1001(ICDEF,IER)

      return
      END

C ******************** CNGDEF ********************
C Allows user to define the distribution of an uncertainty.
C ICDEF - If equal to zero then add a new definition, else edit.

      subroutine CNGDEF(ICDEF,IERR)
#include "building.h"
#include "uncertainty.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      character t24*24
      CHARACTER ITEM(17)*32, outs*124
      integer NITMS,INO ! max items and current menu item

      helpinsub='sensa'  ! set for subroutine
      
      IERR=0
      IER=0

C List current if editing.
      if (ICDEF.gt.0) then 
        call edisp (iuout,
     &             ' Editing definition of an existing distribution.')
        call LISTUAL(1,ICDEF)
      else
        NICNG=NICNG+1
        ICDEF=NICNG
        LCNG(ICDEF)='new'
        call edisp (iuout,' Adding definition of a new distribution.')
      endif

C Remember current value of ICDEF.
      ICDEFold=ICDEF

C Definition of an uncertainty.
C Ask for uncertainty type first - all types of uncertainty have been 
C sorted into different `types' or categories. 
      call edisp(iuout,'  ')
      call edisp(iuout,' Uncertainty category selection. ')
      ITEM(1) =' Databases ...               '
      ITEM(2) ='a  weather                   '
      ITEM(3) ='   pressure (N/A)            '
      ITEM(4) ='c  thermophysical properties '
      ITEM(5) ='d  construction composites   '
      ITEM(6) ='e  optical properties (draft)'
      ITEM(7) ='f  ideal controls'
      ITEM(8) =' --------------------------- '
      ITEM(9) =' Zones composition ...       '
      ITEM(10)='g  geometry (air volume)     '
      ITEM(11)='   boundary conditions (N/A) '
      ITEM(12)='i  zone operations           '
      ITEM(13)='j  convection coefficients   '
      ITEM(14)='   casual gain control (N/A) '
      ITEM(15)=' --------------------------- '
      ITEM(16)='? help                       '
      ITEM(17)='- exit this menu             '
      NITMS=17

C Display current.
      if (IDTYPU(ICDEF).gt.0) then
        write (outs,'(a,a)') ' Current category: ',ITEM(IDTYPU(ICDEF))
        call edisp(iuout,outs)
      endif

C Help text for this menu.
  5   helptopic='uncert_distribution'
      call gethelptext(helpinsub,helptopic,nbhelp)

      INO=-1
      CALL EMENU('Uncertainty categories',ITEM,NITMS,INO)

C Illegal choice?
      if (INO.eq.1.or.INO.eq.8.or.INO.eq.9.or.INO.eq.15) goto 5

C If no type has been selected then return to main SA menu.
      if (INO.eq.NITMS) then
        call edisp(iuout,' Returning to main menu. ')
        call edisp(iuout,
     &            ' * No data saved from distribution specification! ')
        IERR=101
        return
      elseif (INO.eq.NITMS-1) then
        helptopic='uncert_distribution'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Uncertainty categories',5,'-',0,0,IER)
      elseif (INO.eq.2) then

C ** NOW SETTING  -IDTYPU-  IN SDD routines.

C IDTYPU = 3 : Climate parameters
        IDTYPU(ICDEF)=3
        call SDDEF003(ICDEF,IER)
      elseif (INO.eq.4) then

C IDTYPU = 1 : Materials properties
        call SDDEF001(ICDEF,IER)
      elseif (INO.eq.5) then

C IDTYPU = 2 : Composite constructions
        IDTYPU(ICDEF)=2
        call SDDEF002(ICDEF,IER)
      elseif (INO.eq.6) then

C IDTYPU = 6 : blind controls
        IDTYPU(ICDEF)=6
        call SDDEF006(ICDEF,IER)
      elseif (INO.eq.7) then

C IDTYPU = 1001 : Control definitions
        IDTYPU(ICDEF)=1001
        call SDDEF1001(ICDEF,IER)
      elseif (INO.eq.10) then

C IDTYPU = 101 : Zone geometry
        IDTYPU(ICDEF)=101
        call SDDEF101(ICDEF,IER)
      elseif (INO.eq.12) then

C IDTYPU = 4 : Operations
        IDTYPU(ICDEF)=4
        call SDDEF004(ICDEF,IER)
      elseif (INO.eq.13) then

C IDTYPU = 5 : Convection coefficients
        IDTYPU(ICDEF)=5
        call SDDEF005(ICDEF,IER)
      else
        goto 5
      endif

C Check for errors.
      if (IER.eq.101) then 
        call EDISP(IUOUT,'  ')
        call EDISP(IUOUT,' No data recorded for this entry! ')
        IERR=101
        NICNG=NICNG-1
        return
      endif

C Provide id string name.
      if (ICDEF.gt.ICDEFold) then
        continue
      elseif (ICDEF.gt.0) then 
        t24=LCNG(ICDEF)
        CALL EASKS(t24,' ','Name for uncertainty?',
     &      24,LCNG(ICDEF),'Uncertainty name',IER,nbhelp)
        if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
          LCNG(ICDEF)=t24
        endif
      else

C Passed icdef of zero i.e. new uncertainty
        t24=LCNG(1)
        CALL EASKS(t24,' ','Name for uncertainty?',
     &      24,' ','Uncertainty name',IER,nbhelp)
        if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
          LCNG(1)=t24
        endif
      endif

      return
      END

C ******************** LOCDEF ********************
C The location definition menu allows the user to define the 
C areas within the thermal model where or periods when uncertainties exist. 
C The user is asked a series of questions and is provided with 
C a list of options to choose from where applicable.  
C If an internal construction is selected then the related surface 
C in the adjacent zone will also be selected automatically. 
C ITEM   If equal to zero then add a new definition, else edit. 
C IERR is zero is ok, 101 if there was a problem with the location
C   specification and 2 if there was a cancel issued during editing.

      subroutine LOCDEF(ITEM,IERR)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "uncertainty.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON      
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/TMCB2/NBCTMC(MCOM,MTMC),IBCST(MCOM,MTMC),
     &             IBCFT(MCOM,MTMC),IBCSUR(MCOM,MTMC)

      COMMON/UA2T/NTLOC(MNIL),NTG(MNIL,4)

      DIMENSION STMP(MS),IVALZ(MCOM),IVALS(MS)

      character t24*15
      CHARACTER STMP*12
      character outs*124,outs2*248

      integer idst,ihst,idft,ihft   ! for local editing.
      integer idf,ids,ihf,ihs
      integer icc,jjj    ! loops
      integer icurrent   ! which location we are dealing with
      logical found,foundmat,chdb,modmlc

      helpinsub='sensa'  ! set for subroutine
      
C Set initial values (for the whole year).
      IERR=0
      IER=0
      ids=1; ihs=1; 
      idf=365; ihf=24

C Increment number of uncertainty locations or periods
C if ITEM is zero. Whether new or existing set icurrent.
      if(ITEM.eq.0)then
        NILOC=NILOC+1
        NZGU(NILOC)=0
        NTLOC(NILOC)=0
        icurrent=NILOC
      else
        icurrent=ITEM
      endif

C First discover if the location is spatial or temporal
      if(NTLOC(icurrent).eq.(-1))then
        call edisp(iuout,'The current location is temporal')
      endif
      if(NZGU(icurrent).gt.0)then
        call edisp(iuout,'The current location is spacial')
      endif

      helptopic='uncert_location'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKABC(' ',
     &  'In which domain is the uncertainty location restricted: ',
     &  'spatial','temporal (e.g. weather control)','both',IW,nbhelp)

      if (IW.eq.1.or.IW.eq.3) then

C Options a) surfaces using MLC, b) surfaces using material,
C c) all surfaces facing outside d) a blind location e) other
C Internal location => ask for zone, surface and layer.
C If only one zone then set NZNO and NZ accordingly. 
C Clear zone selected array first.
        CALL EASKATOG('Spatial options:','I ',
     &    'surfaces using MLC','surfaces using material',
     &    'surfaces facing outside','blind to control','Other','  ',
     &    '  ',IWA,nbhelp)

C Echo if existing.
        if(NZGU(icurrent).gt.0)then
          do 30 J=1,NZGU(icurrent)
            if(NSGU(icurrent,J).gt.0)then
              if(NSGU(icurrent,J).gt.25)then
                write (outs2,'(1x,a,a,25i3)')
     &          ZNAME(NZNOGU(icurrent,J)),
     &          ':',(NSNOG(icurrent,J,K),K=1,25)
              else
                write (outs2,'(1x,a,a,25i3)')
     &          ZNAME(NZNOGU(icurrent,J)),
     &          ':',(NSNOG(icurrent,J,K),K=1,NSGU(icurrent,J))
              endif
            else
              write (outs2,'(1x,2a,2i3,a)')ZNAME(NZNOGU(icurrent,J)),
     &          ':',NTGU(icurrent,J,1),NTGU(icurrent,J,2),
     &          ' tmc type & period'
            endif
            call edisp248(iuout,outs2,100)
 30       continue
        endif

C Clear arrays in all cases.
 5      do II=1,MCOM
          NZNOGU(icurrent,II)=0
        enddo

        if(IWA.eq.1)then

C Ask for the MLC.
          if(mlcver.eq.0)then
            call EPKMLC(ISEL,'Which construction?','  ',IERR)
          else
            call edisp(iuout,'Which construction?')
            CALL EDMLDB2(modmlc,'-',ISEL,IER)
          endif

C Initial location name to reflect MLC name.
          write(LLOC(icurrent),'(2a)') 'all_',mlcname(ISEL)(1:10)

C Scan all connections looking for this MLC.
          write (outs,'(2a)') 'scanning for ',mlcname(ISEL)
          call edisp(iuout,outs)
          do icc = 1, NCON
            izz=IC1(icc); izs=IE1(icc)
            if(smlcindex(izz,izs).eq.ISEL)then

C If this zone referenced by IC1(icc) not yet registered add it.
C First the case of nothing known.
              if(NZGU(icurrent).eq.0)then
                NZGU(icurrent)=NZGU(icurrent)+1  ! another zone
                NZNOGU(icurrent,1)=IC1(icc)
                NSGU(icurrent,1)=NSGU(icurrent,1)+1
                NSNOG(icurrent,1,1)=IE1(icc)
              else

C The location array is partly filled.
                found=.false.   ! do we know about this zone
                do izz=1,NZGU(icurrent)
                  if(IC1(icc).eq.NZNOGU(icurrent,izz))then
                    found=.true.
                    kkk=izz  ! remember which NZNOGU
                  endif
                enddo
                if(.NOT.found)then  ! add it and the surface to the list
                  NZGU(icurrent)=NZGU(icurrent)+1  ! another zone
                  NZNOGU(icurrent,NZGU(icurrent))=IC1(icc)
                  NSGU(icurrent,NZGU(icurrent))=
     &              NSGU(icurrent,NZGU(icurrent))+1
                  jjj=NSGU(icurrent,NZGU(icurrent))
                  NSNOG(icurrent,NZGU(icurrent),jjj)=IE1(icc)
                else  ! add surface to the list
                  NSGU(icurrent,NZGU(icurrent))=
     &              NSGU(icurrent,NZGU(icurrent))+1
                  jjj=NSGU(icurrent,NZGU(icurrent))
                  NSNOG(icurrent,NZGU(icurrent),jjj)=IE1(icc)
                endif
              endif
            endif
          enddo
        elseif(IWA.eq.2)then

C Ask for the material.
          iwhich = 0
          CALL EPMENSV
          CALL ELISTMAT(iwhich,chdb,'-',matarrayindex,IER)
          CALL EPMENRC
          write(LLOC(icurrent),'(2a)') 'all_',
     &      matname(matarrayindex)(1:10)
  
C Scan all connections looking for this material.
          write (outs,'(2a)') 'scanning for ',matname(matarrayindex)
          call edisp(iuout,outs)
          do icc = 1, NCON

C Which MLC is used? And does it have a layer with the material
C we are looking for?
            izz=IC1(icc); izs=IE1(icc)
            ifoc=smlcindex(izz,izs)
            foundmat=.false.   ! do we know about this
            do ilay=1,LAYERS(ifoc)  ! for each layer
              if(IPRMAT(ifoc,ilay).eq.matarrayindex)then
                foundmat=.true.   ! yes
              endif
            enddo
            if(foundmat)then

C If this zone referenced by IC1(icc) not yet registered add it.
C First the case of nothing known.
              if(NZGU(icurrent).eq.0)then
                NZGU(icurrent)=NZGU(icurrent)+1  ! another zone
                NZNOGU(icurrent,1)=IC1(icc)
                NSGU(icurrent,1)=NSGU(icurrent,1)+1
                NSNOG(icurrent,1,1)=IE1(icc)
              else

C The location array is partly filled.
                found=.false.   ! do we know about this zone
                do izz=1,NZGU(icurrent)
                  if(IC1(icc).eq.NZNOGU(icurrent,izz))then
                    found=.true.
                    kkk=izz  ! remember which NZNOGU
                  endif
                enddo
                if(.NOT.found)then  ! add it and the surface to the list
                  NZGU(icurrent)=NZGU(icurrent)+1  ! another zone
                  NZNOGU(icurrent,NZGU(icurrent))=IC1(icc)
                  NSGU(icurrent,NZGU(icurrent))=
     &              NSGU(icurrent,NZGU(icurrent))+1
                  jjj=NSGU(icurrent,NZGU(icurrent))
                  NSNOG(icurrent,NZGU(icurrent),jjj)=IE1(icc)
                else  ! add surface to the list
                  NSGU(icurrent,NZGU(icurrent))=
     &              NSGU(icurrent,NZGU(icurrent))+1
                  jjj=NSGU(icurrent,NZGU(icurrent))
                  NSNOG(icurrent,NZGU(icurrent),jjj)=IE1(icc)
                endif
              endif
            endif
          enddo
        elseif(IWA.eq.3)then

C Scan all connections for outside facing surfaces.
          write(LLOC(icurrent),'(a)') 'all_facade'
          call edisp(iuout,'scanning for outside connections.')
          do icc = 1, NCON
            if(ICT(icc).eq.0)then

C If this zone referenced by IC1(icc) not yet registered add it.
C First the case of nothing known.
              if(NZGU(icurrent).eq.0)then
                NZGU(icurrent)=NZGU(icurrent)+1  ! another zone
                NZNOGU(icurrent,1)=IC1(icc)
                NSGU(icurrent,1)=NSGU(icurrent,1)+1
                NSNOG(icurrent,1,1)=IE1(icc)
              else

C The location array is partly filled.
                found=.false.   ! do we know about this zone
                do izz=1,NZGU(icurrent)
                  if(IC1(icc).eq.NZNOGU(icurrent,izz))then
                    found=.true.
                    kkk=izz  ! remember which NZNOGU
                  endif
                enddo
                if(.NOT.found)then  ! add it and the surface to the list
                  NZGU(icurrent)=NZGU(icurrent)+1  ! another zone
                  NZNOGU(icurrent,NZGU(icurrent))=IC1(icc)
                  NSGU(icurrent,NZGU(icurrent))=
     &              NSGU(icurrent,NZGU(icurrent))+1
                  jjj=NSGU(icurrent,NZGU(icurrent))
                  NSNOG(icurrent,NZGU(icurrent),jjj)=IE1(icc)
                else  ! add surface to the list
                  NSGU(icurrent,NZGU(icurrent))=
     &              NSGU(icurrent,NZGU(icurrent))+1
                  jjj=NSGU(icurrent,NZGU(icurrent))
                  NSNOG(icurrent,NZGU(icurrent),jjj)=IE1(icc)
                endif
              endif
            endif
          enddo

        elseif(IWA.eq.4)then

C Identify a zone and a TMC type and TMC period for blind control
          if(NCOMP.EQ.1)then
            call edisp(iuout,
     &      'Single zone model, therefore zone 1 selected.')
            call edisp(iuout,'  ')
            NZGU(icurrent)=1
            NZNOGU(icurrent,1)=1
            IZ=1
          else

C Otherwise select the zones from a list of zone names.
            call edisp(iuout,'Zone selection... ')
            call edisp(iuout,'  ')

            INPIC=1
            CALL EPICKS(INPIC,IVALZ,' ',' Which zone:',
     &        12,NCOMP,ZNAME,' zone list',IERR,nbhelp)

C Check for errors.
            if (IERR.ne.0) goto 999

C If no zone selected then return to main SA menu.
            if (INPIC.eq.0) then
              call edisp(iuout,' Returning to main menu....')
              call edisp(iuout,
     &          ' * No data saved from location specification! ')
              IERR=101
              if(icurrent.eq.NILOC) NILOC=NILOC-1
              return
            endif
            NZGU(icurrent)=1
            NZNOGU(icurrent,1)=IVALZ(1)
            IZ=IVALZ(1)
          endif

C Scan TMC file for this zone to see how many TMCs included and
C ask which to identify.

C Read in the related geometry construction and TMC files into common.
          call georead(IFIL+1,LGEOM(IZ),IZ,1,IUOUT,IER)
          IUF=IFIL+2
          CALL ECONST(LTHRM(IZ),IFIL+1,IZ,0,IUOUT,IER)
          if(ITW(IZ).eq.1)then
            CALL ERTWIN(ITRC,IUOUT,IUF,LTWIN(IZ),IZ,IER)
          endif

          nboftmctypes=0
          do loop=1,nzsur(IZ)
            if(ITMCFL(IZ,loop).gt.nboftmctypes)then
              nboftmctypes=ITMCFL(IZ,loop)
            endif
          enddo
          write(outs,'(a,i2,a)') 'There are ',nboftmctypes,
     &      ' TMC types in zone.'
          call edisp(iuout,outs)
          iwhich=1
          call EASKI(iwhich,' ','Which TMC type to focus on',1,'W',
     &      nboftmctypes,'W',1,'TMC type for uncertainty',IERI,nbhelp)
          if(ieri.eq.-3) then
            if(icurrent.eq.NILOC) NILOC=NILOC-1
            IERR=2
            return
          endif
          NSGU(icurrent,NZGU(icurrent))=0  ! no specific surfaces
          NSNOG(icurrent,NZGU(icurrent),1)=0
          NTGU(icurrent,NZGU(icurrent),1)=iwhich

C Also report on the number of TMC control periods and then
C ask which to focus on.
          write(outs,'(a,i2,a)') 'There are ',NBCTMC(IZ,iwhich),
     &      ' TMC control periods.'
          call edisp(iuout,outs)

          iwhichp=1
          call EASKI(iwhichp,' ','Which TMC period',1,'F',3,
     &      'F',1,'TMC period for uncertainty',IERI,nbhelp)
          if(ieri.eq.-3) then
            if(icurrent.eq.niloc) NILOC=NILOC-1
            IERR=2
            return
          endif
          NTGU(icurrent,NZGU(icurrent),2)=iwhichp! is the TMC period
  
        elseif(IWA.eq.5)then

C The 'Other ' option.
          if(NCOMP.EQ.1)then
            call edisp(iuout,
     &      'Single zone model, therefore zone 1 selected.')
            call edisp(iuout,'  ')
            NZGU(icurrent)=1
            NZNOGU(icurrent,1)=1
          else

C Otherwise select the zones from a list of zone names.
            call edisp(iuout,'Zone selection... ')
            call edisp(iuout,'  ')

            INPIC=NCOMP
            CALL EPICKS(INPIC,IVALZ,' ',' Which zones to include:',
     &        12,NCOMP,ZNAME,' zone list',IERR,nbhelp)

C Check for errors.
            if (IERR.ne.0) goto 999

C If no zones have been selected then return to main SA menu.
            if (INPIC.eq.0) then
              call edisp(iuout,' Returning to main menu....')
              call edisp(iuout,
     &          ' * No data saved from location specification! ')
              IERR=101
              if(icurrent.eq.niloc) NILOC=NILOC-1
              return
            endif
            NZGU(icurrent)=INPIC
            do I=1,INPIC
              NZNOGU(icurrent,I)=IVALZ(I)
            enddo
          endif

C For each zone selected ask for the surfaces which the uncertainties 
C are applicable. 
          do 50 IZ=1,NZGU(icurrent)
            NSGU(icurrent,IZ)=0
            IZONE=NZNOGU(icurrent,IZ)

            if(IZONE.GT.NCOMP.OR.IZONE.EQ.0)THEN
              call USRMSG('  ','Zone number out of range!','W')
              IER=IER+1
              if (IER.ge.5) goto 999
              call edisp(iuout,' Please reselect zones ')
              goto 5
            endif

            INPIC=NZSUR(IZONE)
            do I=1,INPIC
              icn1=izstocn(izone,i)
              STMP(I)=SNAME(izone,i)
              NSNOG(icurrent,IZ,I)=0
            enddo
            call edisp(iuout,'Surface selection ')
            call edisp(iuout,'  ')
            call EPICKS(INPIC,IVALS,'Which surfaces to include:',
     &       '(If surface selection not required choose * All) ',
     &       12,NZSUR(IZONE),STMP,ZNAME(IZONE),IERR,nbhelp)

C Check for errors.
            if (IERR.ne.0) goto 999

C If no surfaces have been selected then return to main SA menu.
            if (INPIC.eq.0) then
              call edisp(iuout,' Returning to main menu....')
              call edisp(iuout,
     &          ' * No data saved from location specification!')
              IERR=101
              if(icurrent.eq.niloc) NILOC=NILOC-1
              return
            endif
            NSGU(icurrent,IZ)=INPIC
            do 512 I=1,INPIC
              NSNOG(icurrent,IZ,I)=IVALS(I)
 512        continue

C For each surface selected in the current zone ask for the layers
C where the uncertainties are applicable. 
C * Currently I don't think that this is entirely necessary, but if 
C * in the future it is desired then this is the place to add layer 
C * selection.
 50       continue
        endif
      endif
      if (IW.eq.2.or.IW.eq.3) then

C Temporal locations (control and weather).
        NTLOC(icurrent)=-1
        idst=ids
        call EASKI(IDST,' ','Start day? ',1,'F',365,'F',9,
     &    'day of year',IERI,nbhelp)
        if(ieri.eq.-3) then
          if(icurrent.eq.niloc) NILOC=NILOC-1
          IERR=2
          return
        endif

        ihst=ihs
        call EASKI(IHST,' ','Start hour? ',1,'F',24,'F',1,
     &    'hour of day',IERI,nbhelp)
        if(ieri.eq.-3) then
          if(icurrent.eq.niloc) NILOC=NILOC-1
          IERR=2
          return
        endif

        idft=idf
        call EASKI(IDFT,' ','Finish day? ',1,'F',365,'F',15,
     &    'day of year',IERI,nbhelp)
        if(ieri.eq.-3) then
          if(icurrent.eq.niloc) NILOC=NILOC-1
          IERR=2
          return
        endif

        ihft=ihf
        call EASKI(IHFT,' ','Finish hour? ',1,'F',24,'F',24,
     &    'hour of day',IERI,nbhelp)
        if(ieri.eq.-3) then
          if(icurrent.eq.niloc) NILOC=NILOC-1
          IERR=2
          return
        endif

C Ok to instantiate the data and complete the editing.
        NTG(icurrent,1)=IDST  ! julian day start
        NTG(icurrent,2)=IHST  ! start hour
        NTG(icurrent,3)=IDFT  ! julian day finish
        NTG(icurrent,4)=IHFT  ! hour finish
      endif

C Provide id string name.
 99   t24=LLOC(icurrent)
      CALL EASKS(t24,' ','Name for location/period?',
     &  15,' ','Location/period name',IERR,nbhelp)
      if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
        LLOC(icurrent)=t24
      else
        goto 99
      endif
      return

 999  call EDISP(IUOUT,'  ')
      call EDISP(IUOUT,' Error in location specification! ')
      call EDISP(IUOUT,' Please try again. ')
      IERR=101
      if(icurrent.eq.niloc) NILOC=NILOC-1
      return

      END

C ******************** LINKDEF ********************
C LINKDEF allows the association of changes with locations to 
C make actions.  These actions are then used by the simulator 
C to model the described uncertainty. 
C ITEM  - If equal to zero then add a new definition, else edit. 

      subroutine LINKDEF(ITEM,IERR)
#include "building.h"
#include "uncertainty.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      DIMENSION IVALC(MNCNG),IVALL(MNIL)

      helpinsub='sensa'  ! set for subroutine
      
      IERR=0
      IER=0
      if (IERR.ne.0)  goto 999

C Help text for the subroutine.
      helptopic='uncert_types'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
C Generate a list of changes and then locations so as the user can 
C define actions.
      INPIC=NICNG
      CALL EPICKS(INPIC,IVALC,' ','Which uncertainty definition:',
     &   24,NICNG,LCNG,'Uncert. defn.',IERR,nbhelp)
      if (IERR.ne.0) goto 999

C If no type has been selected then return to main SA menu.
      if (INPIC.eq.0) then
        call edisp(iuout,' Returning to main menu.... ')
        call edisp(iuout,
     &    ' * No data saved from location specification! ')
        IERR=101
        return
      endif

      INPIL=NILOC
      CALL EPICKS(INPIL,IVALL,' ','Which location/period definition:',
     &   15,NILOC,LLOC,' Location defs.',IERR,nbhelp)
      if (IERR.ne.0) goto 999

C If no type has been selected then return to main SA menu.
      if (INPIC.eq.0) then
        call edisp(iuout,' Returning to main menu.... ')
        call edisp(iuout,
     &    ' * No data saved from location specification! ')
        IERR=101
        return
      endif

C Everything is OK re selection therefore add type selection to common.
      do 100 I=1,INPIC
        do 110 J=1,INPIL
          NIACT=NIACT+1
          IACTD(NIACT,1)=IVALC(I)
          IACTD(NIACT,2)=IVALL(J)
 110    continue
 100  continue

      return

 999  call EDISP(IUOUT,'  ')
      call EDISP(IUOUT,' Error in location specification. ')
      call EDISP(IUOUT,' Please try again. ')
      IERR=101
      return

      END

C ******************** UPELM ********************
C Retrieves the unique materials used in the ESP-r 
C model. This is done by copying all the materials used 
C into an array, then sorting them via a bubble search removing 
C any duplicates.

      subroutine UPELM(IPRIM,PRIMLIST,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      INTEGER PRIMLIST(MMLC*ME)

      LOGICAL CHANGED

      CHANGED=.FALSE.

C If the common block MLC has not yet been filled then read in the
C composite construction database.
      IF(.NOT.MLDBOK)THEN
        CALL ERMLDB(0,ITRU,IER)
        IF(IER.eq.4)THEN
          CALL ERMLDB2(0,iuout,IER)
          if(IER.eq.0)then
            MLDBOK=.TRUE.
          endif
        ELSEIF(IER.EQ.1.or.IER.eq.2.or.IER.eq.3)THEN
          CALL USRMSG(' ',' Unable to display selections ','W')
          IER=IER+1
          RETURN
        ELSE
          MLDBOK=.TRUE.
        ENDIF
      ENDIF

C Search through the constructions and store the materials array index.
      IPRIM=0
      do 10, IMLC=1,NMLC
        do 20, ILAY=1,LAYERS(IMLC)
          IPRIM=IPRIM+1
          PRIMLIST(IPRIM)=IPRMAT(IMLC,ILAY)
   20   continue
   10 continue

C Sort list deleting duplicates.
   30 CHANGED=.FALSE.
      do 40 II=1,IPRIM-1
        if (PRIMLIST(II).gt.PRIMLIST(II+1)) then
          ITEMP=PRIMLIST(II)
          PRIMLIST(II)=PRIMLIST(II+1)
          PRIMLIST(II+1)=ITEMP
          CHANGED=.TRUE.
        endif
   40 continue
      if (CHANGED) goto 30

  130 CHANGED=.FALSE.
      do 140, II=2,IPRIM
        if (PRIMLIST(II-1).eq.PRIMLIST(II).or.PRIMLIST(II-1).eq.0) then
          do 150 JJ=II,IPRIM
            PRIMLIST(JJ-1)=PRIMLIST(JJ)
  150     continue
          IPRIM=IPRIM-1
          CHANGED=.TRUE.
        endif
      if (CHANGED) goto 130
  140 continue

      return
      END

C ******************** CALIB ********************

      subroutine CALIB(ITRC,ITRU,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "uncertainty.h"
#include "net_flow.h"
#include "tdf2.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
#include "FMI.h"
#include "help.h"

      INTEGER :: lnblnk
      
      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout
     
C TDF related.
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15

      COMMON/UA31/MATNAM(MNCNG,2)

C Calibration observation, simulation and csv creation files.
      integer nbiobservitem       ! number of observations in temporal file
      integer iobservitem         ! pointer to temporal index for observation
      character obsdatfilename*72 ! holds observation timestep data
      character obsdatscript*72   ! script which drives res to create obsdatfilename
      character obsdatkey*1       ! 
      character obsdatzonename*12 ! zone name associated with an observation
      integer iobsdatzone         ! index of associate zone
      character simdatfilename*72 ! holds simulated timestep data
      character simdatscript*72   ! script which drives res to create simdatscript
      common/calibrobs/nbiobservitem,iobservitem(20),obsdatfilename(20),
     &  obsdatscript(20),obsdatkey(20),obsdatzonename(20),
     &  iobsdatzone(20),simdatfilename(20),simdatscript(20)

C Scripts used to run calibro calibrations for up to 48 zones.
      integer lntarlistz,lncmdlistz ! length of zone csv files
      integer lntarlist,lncmdlist   ! length of model csv files
      character tarlist*1000      ! list of csv files to tar
      character cmdlist*1100      ! list of csv files to include in local script
      character caliroot*32       ! as cfgroot but with - for _
      character calibro_local*96  ! for local csh
      character calibro_mktar*96  ! csh for invoking server calibro zone runs
      character tarlistz*500      ! list of zone based csv files to tar
      character cmdlistz*500      ! list of zone based csv files for script
      character zdashn*12         ! zone names with _ replaced with -
      logical tfcmdlistz          ! true when action completed
      common/calibinvok/lntarlistz(1:48),lncmdlistz(1:48),
     &  lntarlist,lncmdlist,
     &  tfcmdlistz(1:48),calibro_local(0:48),calibro_mktar(0:48),
     &  tarlist,cmdlist,caliroot,tarlistz(1:48),cmdlistz(1:48),
     &  zdashn(1:48)

      integer nbcalibrorun
      integer nbofzonetj  ! number of zone-actions
      integer nbofzonegf  ! number of zone-observations    
      real tablejson ! dimensioned (irun,izone,iact,3) estimate lower upper
      real tablegof  ! dimensioned (irun,izone,iobs,2) before after
      common/jsontable/nbcalibrorun,nbofzonetj(18,48),nbofzonegf(18,48),
     &  tablejson(18,48,20,3),tablegof(18,48,20,2)

C Hold average information for all the runs for each zone.
      real avgtablejson ! dimensioned (izone,iact) estimate
      real avgtablegof  ! dimensioned (izone,iobs) after
      integer itisact         ! matching uncertainty distribution
      integer itwhereact      ! matching uncertainty location
      common/avgjsontable/avgtablejson(48,20),avgtablegof(48,20),
     &  itisact(48,20),itwhereact(20)

      character actphrase*32  ! from the PARAMETER LINE
      character dataphrase*24 ! based on observation
      common/jsontabletext/actphrase(18,48,20),dataphrase(18,48,20)

C For server runs we have to establish how many calibro runs were made
C on a zone-by-zone basis.
      integer nbservercalibrorun  ! counter for server based calibro runs
      common/jsontableserver/nbservercalibrorun(48)

C Remember how many calibro runs were requested.
      integer ihowmanycalibroruns
      common/howmany/ihowmanycalibroruns
      
      logical OK
      logical unixok,LIBXST
      logical anymeasured
      logical usingcalversion
      logical domlc,dogeo,doopr,doctl,doblnd,docnn  ! tasks to do
      character*72 OLDLCFGF
      character outs*148
      character the_file*96
      integer matarrayindex ! the indes within matdatarray
      character NAM*72,MATNAM*32
      integer ier,itis,itwhere,itact   ! for matching json tokens
      CHARACTER LTEMP*96,DTEMP*96
      integer loop,loop2
      integer items
      character APE*6,EXT*4,MODE*4
      character NNAME*144
      character sfile*96,snpfile*96
      logical XST
      logical preview
      logical proceed    ! track user request to look at another suggestion
      logical foreground ! option to invoke calibrino in forground
      logical silent     ! just do all changes without user interaction
      dimension ITEMSM(10)
      character ITEMSM*34
      character*72 sfilelist(MFFOLD),snpfilelist(MFFOLD)
      integer nitems  ! nb items in the menu

      INTEGER :: itrc,itru
      INTEGER :: isauto
      INTEGER :: IUNIT
      INTEGER :: IUALF

      helpinsub='sensa'  ! set for subroutine
      helptopic='calib_overview'

      call isunix(unixok)
      preview=.false.
      usingcalversion=.false.

C Calibration. First check if there is an uncertainty definition
C in the model. If there is list its directives.
      IUALF=IFIL
      INQUIRE (FILE=LUALF,EXIST=LIBXST)
      if(.NOT.LIBXST)then
        call edisp(iuout,' ')
        call edisp(iuout,
     &    'No uncertainties associated with the model. Please define.')
        return
      endif

      call edisp(iuout,'  ')
      write(outs,'(A,A)')
     &   'Reading contents of existing UA definitions file: ',LUALF
      call edisp(iuout,outs)
      call READUAL(IUALF)
      call LISTUAL(0,0)

C The next check is whether there is a temporal file with observed
C data included.
      if(iabs(itdflg).eq.0)then
        call edisp(iuout,' ')
        call edisp(iuout,'No temporal data defined.')
        call edisp(iuout,
     &    'Please use facility for temporal definitions.')
        return
      endif
      if(ITEMSTD.eq.0)then
        call edisp(iuout,
     &    'Calibration requires directives for uncertainty.')
        call edisp(iuout,
     &    'Please use facility to define uncertainties.')
        return
      endif

C List out the current temporal items.
      anymeasured=.false.
      write(outs,'(2a)')' Temporal definitions ',LTDFA
      call edisp(iuout,outs)
      call edisp(iuout,' ')
      call edisp(iuout,' Temporal entities currently used...')
      call edisp(iuout,'  _______________________________   ')
      call edisp(iuout,'  |temporal    |generic |associated')
      call edisp(iuout,
     &   '  |entity name |type    |with   zone &     surface ')
      do loop=1,ITEMSTD
        WRITE(outs,'(i2,1x,a,1x,a,5x,2a)')loop,DBTAG(loop),
     &    DBTASK(loop),DBZN(loop),DBSN(loop)
        call edisp(iuout,outs)
      enddo
      call edisp(iuout,' ')

C Clear local arrays as well as the collection from parsing
C sets of json files.
      itis=0; itwhere=0; itact=0
      nbiobservitem=0; k=25
      nbcalibrorun=0   ! will need to be reset when moving to new zone
      ihowmanycalibroruns=0  
      do loop=1,18      ! up to 18 calibro runs
        do iz=1,48   ! up to 48 zones
          nbofzonetj(loop,iz)=0
          nbofzonegf(loop,iz)=0
          do loop2=1,20
            actphrase(loop,iz,loop2)='  '
            dataphrase(loop,iz,loop2)='  '
            tablejson(loop,iz,loop2,1)=0.0
            tablejson(loop,iz,loop2,2)=0.0
            tablejson(loop,iz,loop2,3)=0.0
            avgtablejson(iz,loop2)=0.0
            itisact(iz,loop2)=0
            itwhereact(loop2)=0
            avgtablegof(iz,loop2)=0.0
            tablegof(loop,iz,loop2,1)=0.0
            tablegof(loop,iz,loop2,2)=0.0
          enddo
        enddo
      enddo

C Clear the data arrays and figure out the names and files that
C will be required during calibrations. 
      call clearcalibarrays(anymeasured)
        
C Create initial portion of zone based calibro invocation script
C as well as for command to create tar files.
      do loop=1,ITEMSTD
        if(DBTASK(loop)(1:8).eq.'DBTZNOBS'.or.
     &     DBTASK(loop)(1:7).eq.'SURTOBS'.or.
     &     DBTASK(loop)(1:7).eq.'ZNRHOBS'.or.
     &     DBTASK(loop)(1:7).eq.'ZNHTOBS'.or.
     &     DBTASK(loop)(1:7).eq.'ZNCLOBS')then
          loopz=iobsdatzone(loop)  ! the associated zone
          lnzn=lnblnk(zdashn(loopz))

C Include zone name in the zone command list.
          write(cmdlistz(loopz),'(6a)') 'calibrino -c ',
     &      caliroot(1:lnblnk(caliroot)),zdashn(loopz)(1:lnzn),
     &      ' -b bc.csv -i calibro_input_',zdashn(loopz)(1:lnzn),
     &      '.csv -o '
          lncmdlistz(loopz)=lnblnk(cmdlistz(loopz))+2
          tfcmdlistz(loopz)=.false.  ! signal we have not yet done

C Include zone name in the zone tar list but we also need to include
C the name calibro_input.csv for use in the server. Later in the code
C the zone-based calibro_input file will be copied to calibro_input.csv.
          write(tarlistz(loopz),'(3a)')'bc.csv calibro_input_',
     &      zdashn(loopz)(1:lnzn),'.csv calibro_input.csv  '
          lntarlistz(loopz)=lnblnk(tarlistz(loopz))+2
        endif
      enddo

      if(.NOT.anymeasured)then
 
C anymeasured not found so there are no observations.
        call edisp(iuout,
     &    'Existing temporal data types cannot be calibrated')
        call edisp(iuout,
     &    'against. Please add observed zone temperatures,')
        call edisp(iuout,'humidity, heating or cooling.')
        return
      endif

C loop is related to position in counter of observations while
C itis is related to position in temporal arrays.
      do loop=1,nbiobservitem
        itis=iobservitem(loop)
        WRITE(outs,'(i2,a,i2,7a,i2)')loop,':',itis,DBTAG(itis),
     &  DBTASK(itis),' ',DBZN(itis),' ',DBSN(itis),' zindx',
     &  iobsdatzone(itis)
        call edisp(iuout,outs)
        call edisp(iuout,obsdatfilename(loop))
        call edisp(iuout,obsdatscript(loop))
        call edisp(iuout,obsdatkey(loop))
        call edisp(iuout,obsdatzonename(loop))
        call edisp(iuout,simdatfilename(loop))
        call edisp(iuout,simdatscript(loop))
      enddo
      call edisp(iuout,' ')

C Look at uncertainties and see if any of them relate to
C materials or constructions. If so we will need to have
C a local variant of materials and constructions databases
C to work with.
      domlc=.false.; dogeo=.false.; doopr=.false.; doctl=.false.
      doblnd=.false.
      do loop=1,NICNG  ! for each distribution
        if (IDTYPU(loop).eq.1) then  ! if materials focus
          IDB=IDATR(loop,1)
          if(IDB.gt.0)then
            matarrayindex=IDB  ! find matching array index
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht
            if(matarrayindex.eq.0)then
              NAM='AIR'
            else
              write(NAM,'(a)') matname(matarrayindex)(1:32)
            endif
          endif
          call edisp (iuout,'  ')
          write(outs,'(a,i4,4a)')'Distribution:',loop,'; ',LCNG(loop),
     &      ' requires a variant of material ',NAM(1:lnblnk(NAM))
          call edisp (iuout,outs)
          domlc=.true.; dogeo=.true.
        elseif(IDTYPU(loop).eq.2) then  ! if MLC focus
          write(outs,'(a,i4,4a)')'Distribution:',loop,'; ',LCNG(loop),
     &      ' requires a variant of MLC ',MATNAM(loop,1)
          call edisp (iuout,outs)
          domlc=.true.; dogeo=.true.
        elseif(IDTYPU(loop).eq.4) then  ! if operation focus
          if (IDATR(loop,1).eq.0) then
            write(outs,'(a,i4,3a)')'Distribution:',loop,'; ',
     &        LCNG(loop),' requires a variant of infil/vent schedule'
            call edisp (iuout,outs)
          else
            write(outs,'(a,i4,3a)')'Distribution :',loop,'; ',
     &        LCNG(loop),' requires a variant of casual gain '
            call edisp (iuout,outs)
          endif
          doopr=.true.
        elseif(IDTYPU(loop).eq.6) then  ! if optical focus
          if (IDATR(loop,1).eq.1) then
            write(outs,'(a,i4,3a)')'Distribution:',loop,'; ',
     &        LCNG(loop),' requires a variant of zone tmc file'
            call edisp (iuout,outs)
          endif
          doblnd=.true.
        elseif(IDTYPU(loop).eq.1001) then  ! if ideal control focus
          write(outs,'(a,i4,3a)')'Distribution:',loop,'; ',LCNG(loop),
     &      ' requires a variant control file.'
          call edisp (iuout,outs)
          doctl=.true.
        endif
      enddo  ! of loop

C Check if there is a calibro_done file, if found remove it.
      IUNIT=IFIL+1
      write(the_file,'(a)') 'calibro_done'
      call FINDFIL(the_file,XST)
      if(XST)then
        CALL EFOPSEQ(iunit,the_file,1,IER)
        call EFDELET(iunit,ISTAT)
      endif

C Check to see if there is already a model cfg file with
C _cal in the name. If someone previously ran this facility
C there offer to skip the creation of variant and go straight
C to the run assessment phase.
C Remember the initial cfg file.

C << check if there is already a _cal_cal.cfg >>

      write(OLDLCFGF,'(a)') LCFGF(1:lnblnk(LCFGF))
      write(APE,'(a)') '_cal'
      EXT='.cfg'
      CALL FNCNGR(LCFGF,APE,EXT,NNAME)
      XST=.false.
      INQUIRE (FILE=NNAME,EXIST=XST)
  79  if(XST)then
        call edisp(iuout,
     &    'A model cfg file for calibration already exists.')
        call edisp(iuout,NNAME)

C Make up overall command menu for calibration tasks.
C        WRITE(ITEMSM(1),'(A)') 'a re-generate model files'
C        WRITE(ITEMSM(2),'(A)') 'b invoke MontiCarlo assessments'
C        WRITE(ITEMSM(3),'(A)') 'c review sensitivities in res'
C        WRITE(ITEMSM(4),'(A)') 'd extract simulation data'
C        WRITE(ITEMSM(5),'(A)') 'e tabulate Calibro json file(s)'
C        WRITE(ITEMSM(6),'(A)') 'f invoke local Calibro'
C        WRITE(ITEMSM(7),'(A)') 'g invoke server Calibro'
C        WRITE(ITEMSM(8),'(A)') 'h preview Calibro suggestions'
C        WRITE(ITEMSM(9),'(A)') 'i update model with suggestions'
C        WRITE(ITEMSM(10),'(A)')'j use calibrated model'
C        WRITE(ITEMSM(11),'(A)')'k tidy csh & csv files'
C        WRITE(ITEMSM(12),'(A)')'l reload base case model'

        CALL EASKATOG('Options',': ',     
     &    're-generate model','do calibration',
     &    'read calibro json','rerun calibro','N/A','tidy csh&csv',
     &    'cancel',ioption,nbhelp)
        if(ioption.eq.1)then
          continue
        elseif(ioption.eq.2)then

C Switch focus to the calibration variant. Also update caliroot
C and then proceed with calibration.
          call edisp(iuout,'Using calibration variant of the model')
          write(OLDLCFGF,'(a)') LCFGF(1:lnblnk(LCFGF))
          write(LCFGF,'(a)') NNAME(1:lnblnk(NNAME))
          MODE='ALL '
          IUF=IPRODB   ! assign second file unit to the events db unit
          CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
          if(ier.eq.0)then
            usingcalversion=.true.
            lnroot=lnblnk(cfgroot)
            caliroot='                              '
            do loop=1,lnroot
             if(cfgroot(loop:loop).eq.'_')then
              write(caliroot(loop:loop),'(a)') '-'
             else
              write(caliroot(loop:loop),'(a)') cfgroot(loop:loop)
             endif
            enddo

C Having loaded the _cal.cfg file ask the user what to do next.
            goto 42
          else
            call usrmsg('Unable to scan calibration cfg file',
     &        'reverting to last cfg file.','W')
            write(LCFGF,'(a)') OLDLCFGF(1:lnblnk(OLDLCFGF))
            MODE='ALL '
            IUF=IPRODB   ! assign second file unit to the events db unit
            CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
            usingcalversion=.false.
            return
          endif
        elseif(ioption.eq.3)then

C Switch focus to the calibration variant and jump to 43.
          CALL EASKOK(' ','Preview suggestions?',preview,nbhelp)
          call edisp(iuout,'Using calibration variant of the model')
          write(OLDLCFGF,'(a)') LCFGF(1:lnblnk(LCFGF))
          write(LCFGF,'(a)') NNAME(1:lnblnk(NNAME))
          MODE='ALL '
          IUF=IPRODB   ! assign second file unit to the events db unit
          CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
          if(ier.eq.0)then
            usingcalversion=.true.
            lnroot=lnblnk(cfgroot)
            caliroot='                              '
            do loop=1,lnroot
             if(cfgroot(loop:loop).eq.'_')then
              write(caliroot(loop:loop),'(a)') '-'
             else
              write(caliroot(loop:loop),'(a)') cfgroot(loop:loop)
             endif
            enddo
            if(preview)then
              isauto=1  ! preview tasks
            else
              isauto=2  ! alter model tasks
            endif
            goto 44
          else
            call usrmsg('Unable to scan calibration cfg file!',
     &        'Reverting to last cfg file.','W')
            write(LCFGF,'(a)') OLDLCFGF(1:lnblnk(OLDLCFGF))
            MODE='ALL '
            IUF=IPRODB   ! assign second file unit to the events db unit
            CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
            usingcalversion=.false.
            return
          endif
        elseif(ioption.eq.4)then

C Switch focus to the calibration variant and jump to 178.
          call edisp(iuout,'Using calibration variant of the model')
          write(OLDLCFGF,'(a)') LCFGF(1:lnblnk(LCFGF))
          write(LCFGF,'(a)') NNAME(1:lnblnk(NNAME))
          MODE='ALL '
          IUF=IPRODB   ! assign second file unit to the events db unit
          CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
          if(ier.eq.0)then

C Do the steps for creating and invoking scripts.
            usingcalversion=.true.
            lnroot=lnblnk(cfgroot)
            caliroot='                              '
            do loop=1,lnroot
             if(cfgroot(loop:loop).eq.'_')then
              write(caliroot(loop:loop),'(a)') '-'
             else
              write(caliroot(loop:loop),'(a)') cfgroot(loop:loop)
             endif
            enddo
            call docreatescripts(islocal,ier)
            if(ier.eq.3)then
              call edisp(iuout,'There was a problem creating scripts')
              goto 79
            endif

C Run the local or server scripts. These could take a rather long
C time so they are started ion the background and control is then
C returned to this calling code.
            call runlocalorserver(islocal)
            goto 79  ! and ask again

          else
            call usrmsg('Unable to scan calibration cfg file',
     &        'reverting to last cfg file.','W')
            write(LCFGF,'(a)') OLDLCFGF(1:lnblnk(OLDLCFGF))
            MODE='ALL '
            IUF=IPRODB   ! assign second file unit to the events db unit
            CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
            usingcalversion=.false.
            return
          endif

        elseif(ioption.eq.5)then

C Future option.

        elseif(ioption.eq.6)then

C Clear this models past calibro csh and csv files.
          call clearcshcsvfiles(usingcalversion,NNAME)
          return
        elseif(ioption.eq.7)then
          return
        endif
      else

C There is no calibration version of model cfg file so generate it.
        call edisp(iuout,
     &    'A model cfg file for calibration will be generated.')
      endif

C Figure out the names of the cfg and zone files based on these
C toggles for tasks.
      call createcalvariantfiles(domlc,dogeo,doopr,doctl,doblnd,docnn)

      call edisp(iuout,'A variant of the model has been created')
  42  call edisp(iuout,
     &  'The next task is to undertake calibration runs...')
      helptopic='calibration_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKOK(' ','Proceed with calibration?',OK,nbhelp)
      if(.NOT.OK) return

C Check if there are simulation parameter sets, if so user should
C select them before proceeding.
      call docalibrationruns(itrc,ier)
      if(ier.ne.0)then

C Problem with assessments, jump.
        XST=.TRUE.
        call usrmsg('Unable to run calibration assessments so',
     &    'reverting to last cfg file.','W')
        write(LCFGF,'(a)') OLDLCFGF(1:lnblnk(OLDLCFGF))
        MODE='ALL '
        IUF=IPRODB   ! assign second file unit to the events db unit
        CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
        usingcalversion=.false.
        return
      endif

C The simulations are finished now create scripts to extract data
C for calibration and then invoke those scripts.
      call doextractsetdata(itrc,ier)
      if(ier.ne.0)then

C Problem with assessments, jump.
        XST=.TRUE.
        call usrmsg('Unable to extract calibration data from csv',
     &    'files so reverting to last cfg file.','W')
        write(LCFGF,'(a)') OLDLCFGF(1:lnblnk(OLDLCFGF))
        MODE='ALL '
        IUF=IPRODB   ! assign second file unit to the events db unit
        CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
        usingcalversion=.false.
        return
      endif

C Create local (islocal=1) or server (islocal=2) scripts.
C Pass back islocal for use in runlocalorserver.
      call docreatescripts(islocal,ier)
      if(ier.eq.3)then
        call edisp(iuout,'There was an issue when creating scripts')
        return
      endif

C Ask if run should be in forground or background.
      if(islocal.eq.1)then
        CALL EASKOK(' ','Run Calibro in foreground',foreground,nbhelp)
      else
        foreground=.false.
      endif

C Run the local or server scripts. These could take a rather long
C time so they are started ion the background and control is then
C returned to this calling code.
      call runlocalorserver(islocal)

C If running sequential then loop and check for the existance
C of the file calibro_done.
      if(foreground)then
        IUNIT=IFIL+1
        do loop=1,200
          call pausems(10000)
          write(the_file,'(a)') 'calibro_done'
          call FINDFIL(the_file,XST)
          if(XST)then
            call edisp(iuout,' ')
            call edisp(iuout,'Detected Calibrino finished!')
            CALL EFOPSEQ(iunit,the_file,1,IER)
            call EFDELET(iunit,ISTAT)
            goto 43  ! now get user opinion as to what to do
          endif
        enddo
      endif
      call edisp(iuout,'  ')
      call edisp(iuout,
     &  'Once you have json file(s) defining suggested changes to')
  43  call edisp(iuout,
     &  'calibrate the model choose one of the following optionsd...')

      CALL EASKATOG(
     &  'After sending the csv files to calibro you will be given',
     &  'one or more json files:','preview suggestions',
     &  'scan & act on EACH suggestion',
     &  'scan & act on ALL suggestions','delay scanning',
     &  'cancel & reload original model','  ','  ',isauto,nbhelp)
  44  if(isauto.eq.1.or.isauto.eq.2.or.isauto.eq.3)then
        if(isauto.eq.2)then
          preview=.false. ! reset if left over
          silent=.false.
        elseif(isauto.eq.3)then
          preview=.false. ! do it silently
          silent=.true.
        endif
        if(isauto.eq.1)then
          preview=.true.
          silent=.false.

C Recover the json file names associated with the model or zones in 
C the model. Confirm how many calibro iterations were run and
C re-establish caliroot if scan if the first task attempted in a
C new prj session.
          if(ihowmanycalibroruns.eq.0)then
            ihowmanycalibroruns=5
            call EASKI(ihowmanycalibroruns,
     &      'Calibro was alrealy run. How many iterations were ',
     &      'requested (so we can read that many files',1,'W',20,
     &      'W',8,'calibrino runs',IERI,nbhelp)
          endif

C If we previously did a local calibro then the following logic applies.
C Clear tfcmdlistz. If islocal is zero then ask the user.
          if(islocal.eq.0)then
            CALL EASKAB('Previously did you run Calibro:',
     &      '  ','locally','on server',IW,nbhelp)
            if(IW.eq.1) islocal=1
            if(IW.eq.2) islocal=2
          endif
          if(islocal.eq.1)then
            do loop=1,48
              tfcmdlistz(loop)=.false.
            enddo
            do loop=1,ITEMSTD
              if(DBTASK(loop)(1:8).eq.'DBTZNOBS'.or.
     &           DBTASK(loop)(1:7).eq.'SURTOBS'.or.
     &           DBTASK(loop)(1:7).eq.'ZNRHOBS'.or.
     &           DBTASK(loop)(1:7).eq.'ZNHTOBS'.or.
     &           DBTASK(loop)(1:7).eq.'ZNCLOBS')then
                loopz=iobsdatzone(loop)  ! the associated zone
                lnzn=lnblnk(zdashn(loopz))
                if(tfcmdlistz(loopz))then
                  continue ! only do ' -s ' once
                else

C Use users prior ihowmanycalibroruns here. And reset the
C counter nbcalibrorun.
                  nbcalibrorun=0
                  do ifind=1,ihowmanycalibroruns
                    write(the_file,'(2a,i2.2,a)') 
     &                caliroot(1:lnblnk(caliroot)),
     &                zdashn(loopz)(1:lnzn),ifind,'.json'
                    call FINDFIL(the_file,XST)
                    if(XST)then
                      call edisp(iuout,the_file)
                      write(6,*) 'scanning json ',
     &                  the_file(1:lnblnk(the_file))
                      call previewofjson(the_file,preview,silent,IER)

                      call createjsontable(the_file,islocal,ier)
                    endif
                  enddo
                  tfcmdlistz(loopz)=.true.  ! signal we have done it
                endif
              endif
            enddo

C Print what was found within ihowmanycalibroruns
            call printjsontable()
          else

C Server run(s) were comissioned. The user must select the files
C to scan to build up the jasontable.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelistmulti('?','cfg','jsn',
     &        sfilelist,snpfilelist,nfile,iier)
            if(nfile.gt.0)then
              call browsefilelistmulti('b','cfg','jsn',
     &          sfilelist,snpfilelist,nfile,iier)
              if(nfile.gt.0)then

C Clear the nbservercalibrorun array.
                do loopz=1,48
                  nbservercalibrorun(loopz)=0
                enddo
                do loopj=1,nfile
                  if(snpfilelist(loopj)(1:2).ne.'  ')then
                    write(LTEMP,'(a)')
     &                snpfilelist(loopj)(1:lnblnk(snpfilelist(loopj)))

                    INQUIRE (FILE=LTEMP,EXIST=LIBXST)

C Debug.
                    if (LIBXST) then
                      write(6,*) 'scanning json ',ltemp(1:lnblnk(ltemp))
                      call previewofjson(ltemp,preview,silent,IER)
                      call createjsontable(LTEMP,islocal,ier)
                    endif
                  endif
                enddo
              endif

C Print what was found within ihowmanycalibroruns
              call printjsontable()
              call usrmsg(
     &          'Review the table and/or the fort.34 file which was',
     &          'generated & select a close json file or average.','W')
            endif
          endif
          goto 43  ! present user choices again
        endif
        
C Ask for file or browse file name. Parse the file and report directives.
        CALL EASKABC(' ','Options:',
     &    'scan a .json file','use average from many runs',
     &    'cancel',iaplyoption,nbhelp)
        if(iaplyoption.eq.1)then
          LTEMP='  '; DTEMP='  '; nbhelp=1
          sfile=' '
          snpfile=' '
          call edisp(iuout,' ')
          call browsefilelist('?','cfg','jsn',sfile,snpfile,nfile,iier)
          if(nfile.gt.0)then
            sfile=' '
            snpfile=' '
            call browsefilelist('b','cfg','jsn',sfile,snpfile,nfile,
     &        iier)
            if(snpfile(1:2).ne.'  ')then
              write(LTEMP,'(a)')snpfile(1:lnblnk(snpfile))

            else
              LTEMP=DTEMP
              CALL EASKS(LTEMP,'Json file from Calibro (full path):',
     &          ' ',96,DTEMP,'Json file from calibro',IER,nbhelp)
            endif
          else
            LTEMP=DTEMP
            CALL EASKS(LTEMP,' ','Json file from Calibro (full path):',
     &            96,DTEMP,'Json file from calibro',IER,nbhelp)
          endif

C Scan and act on contents of .json file.
          call previewofjson(ltemp,preview,silent,IER)
          goto 43  ! offer user options again

        elseif(iaplyoption.eq.2)then

C Use average value. Use similar logic as in printjsontable.
          loop=1
          denom=float(ihowmanycalibroruns)
          do iz=1,48   ! for each of the possible zones
            lnzn=lnblnk(zname(iz))
            if(nbofzonetj(loop,iz).gt.0)then
              do loop2=1,nbofzonetj(loop,iz)
                itis=itisact(iz,loop2)
                itwhere=itwhereact(loop2)
                itact=loop2 ! the action index
                rv=avgtablejson(iz,itact)/denom
                write(6,*) 'itwhere iz itis rv ',itwhere,iz,itis,rv
                call applysuggestion(ITRC,ITRU,preview,silent,itis,
     &            itact,rv,proceed)
                if(.NOT.proceed) EXIT
              enddo
            endif
          enddo
          goto 43  ! present user with options
        endif

C If previewing return user to higher level question.
        if(preview)then
          call edisp(iuout,
     &      'You can now implement suggestions if you want.')
          preview=.false.
          goto 43
        endif

C Re-establish the base case model and return.
        CALL ERPFREE(IUNIT,ios)
        if(usingcalversion)then
          call edisp(iuout,'Reverting to original model...')
          write(LCFGF,'(a)') OLDLCFGF(1:lnblnk(OLDLCFGF))
          MODE='ALL '
          IUF=IPRODB   ! assign second file unit to the events db unit
          CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
          if(ier.eq.0)then
            return
          else
            call usrmsg('Unable to revert to base case cfg file',
     &        'Exiting prj.','P')

C Clear allocatable arrays.
            CALL DeallocateAllArrays
            close(ieout)
            CALL ERPFREE(ieout,ISTAT)
            CALL EPAGEND
            STOP
          endif
          XST=.true.
          call edisp(iuout,'Reverting to original model...done.')
          goto 79  ! offer user choice
        else
          XST=.true.
          goto 79  ! offer user choice
        endif

      elseif(isauto.eq.4)then

C Delay scanning.
      elseif(isauto.eq.5)then

C Re-establish the base case model and return.
        if(usingcalversion)then
          write(LCFGF,'(a)') OLDLCFGF(1:lnblnk(OLDLCFGF))
          MODE='ALL '
          IUF=IPRODB   ! assign second file unit to the events db unit
          CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
          if(ier.eq.0)then
            usingcalversion=.false.
            return
          else
            call usrmsg('Unable to revert to base case cfg file',
     &        'Exiting prj.','P')

C Clear allocatable arrays.
            CALL DeallocateAllArrays
            close(ieout)
            CALL ERPFREE(ieout,ISTAT)
            CALL EPAGEND
            STOP
          endif
        else
          return
        endif
      endif
      return
      end ! of CALIB

C ******************** clearcalibarrays ********************
C Determines the names of the files and the
C various labels that will be created or used during a calibration.
C If there are observed matches set anymeasured TRUE.

      subroutine clearcalibarrays(anymeasured)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "net_flow.h"
#include "tdf2.h"
#include "esprdbfile.h"
#include "material.h"
#include "FMI.h"

      INTEGER :: lnblnk

C Passed parameter.
      logical anymeasured
      
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout
      
      INTEGER :: ncomp,ncon
      common/C1/NCOMP,NCON

C TDF related.
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15

C Calibration observation, simulation and csv creation files.
      integer nbiobservitem       ! number of observations in temporal file
      integer iobservitem         ! pointer to temporal index for observation
      character obsdatfilename*72 ! holds observation timestep data
      character obsdatscript*72   ! script which drives res to create obsdatfilename
      character obsdatkey*1       ! 
      character obsdatzonename*12 ! zone name associated with an observation
      integer iobsdatzone         ! index of associate zone
      character simdatfilename*72 ! holds simulated timestep data
      character simdatscript*72   ! script which drives res to create simdatscript
      common/calibrobs/nbiobservitem,iobservitem(20),obsdatfilename(20),
     &  obsdatscript(20),obsdatkey(20),obsdatzonename(20),
     &  iobsdatzone(20),simdatfilename(20),simdatscript(20)

C Scripts used to run calibro calibrations for up to 48 zones.
      integer lntarlistz,lncmdlistz ! length of zone csv files
      integer lntarlist,lncmdlist   ! length of model csv files
      character tarlist*1000      ! list of csv files to tar
      character cmdlist*1100      ! list of csv files to include in local script
      character caliroot*32       ! as cfgroot but with - for _
      character calibro_local*96  ! for local csh
      character calibro_mktar*96  ! csh for invoking server calibro zone runs
      character tarlistz*500      ! list of zone based csv files to tar
      character cmdlistz*500      ! list of zone based csv files for script
      character zdashn*12         ! zone names with _ replaced with -
      logical tfcmdlistz          ! true when action completed
      common/calibinvok/lntarlistz(1:48),lncmdlistz(1:48),
     &  lntarlist,lncmdlist,
     &  tfcmdlistz(1:48),calibro_local(0:48),calibro_mktar(0:48),
     &  tarlist,cmdlist,caliroot,tarlistz(1:48),cmdlistz(1:48),
     &  zdashn(1:48)

      integer nbcolforzone  ! how many input columns for each zone
      integer icolforzone   ! array of associated columns
      common/calibcolumns/nbcolforzone(1:48),icolforzone(1:48,MCOM)

      integer loopo,loop
      integer lncurobs,lncursim  ! length of current csv files

C Include the two fixed file names in the tarlist.
      tarlist='bc.csv calibro_input.csv '
      lntarlist=26; k=lntarlist; ke=lntarlist

      do loop=1,20
        iobservitem(loop)=0
        obsdatfilename(loop)='  '
        obsdatscript(loop)='  '
        obsdatkey(loop)=' '
        obsdatzonename(loop)=' '
        iobsdatzone(loop)=0
        simdatfilename(loop)=' '
        simdatscript(loop)=' '
      enddo

C Replace _ with - in the root name.
      lnroot=lnblnk(cfgroot)
      caliroot='                              '
      do loop=1,lnroot
        if(cfgroot(loop:loop).eq.'_')then
          write(caliroot(loop:loop),'(a)') '-'
        else
          write(caliroot(loop:loop),'(a)') cfgroot(loop:loop)
        endif
      enddo

      write (cmdlist,'(3a)') 'calibrino -c ',
     &  caliroot(1:lnblnk(caliroot)),
     &  ' -b bc.csv -i calibro_input.csv -o '
      lncmdlist=lnblnk(cmdlist)+2

C Clear associations between calibro_input.csv columns and zones.
      do loop=1,48
        nbcolforzone(loop)=0  ! no columns associated
        do loop2=1,MCOM
          icolforzone(loop,loop2)=0 ! no associations yet
        enddo        

C Replace _ with - in the zone name for use later on.
        lnzn=lnblnk(zname(loop)); zdashn(loop)='  '
        do loop2=1,lnzn
          if(zname(loop)(loop2:loop2).eq.'_')then
            write(zdashn(loop)(loop2:loop2),'(a)') '-'
          else
            write(zdashn(loop)(loop2:loop2),'(a)') 
     &        zname(loop)(loop2:loop2)
          endif
        enddo
      enddo

C Loop through all items in temporal file and make note of those
C which are related to observations. The key strokes in the res
C selections is based on type: 
C a) zone dbT b) surface T c) zone RH d)zone heat e) zone cooling
C Capture a list of all of the csv files that will need to be
C collected to pass to calibro.
      nbiobservitem=0
      do loop=1,ITEMSTD
        if(DBTASK(loop)(1:8).eq.'DBTZNOBS')then
          anymeasured=.true.
          call edisp(iuout,
     &      'Calibration to zone temperatures possible.')
          nbiobservitem=nbiobservitem+1
          iobservitem(nbiobservitem)=loop  ! which ITEMSTD
          write(obsdatfilename(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_DBTZNOBS_obs.csv'
          lncurobs=lnblnk(obsdatfilename(nbiobservitem))
          ke=k+lncurobs
          write(tarlist(k:ke),'(2a)') ' ',
     &      obsdatfilename(nbiobservitem)(1:lncurobs)
          k=k+lncurobs+1
          write(simdatfilename(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_DBTZNOBS_sim.csv'
          lncursim=lnblnk(simdatfilename(nbiobservitem))
          ke=k+lncursim
          write(tarlist(k:ke),'(2a)') ' ',
     &      simdatfilename(nbiobservitem)(1:lncursim)
          k=k+lncursim+1
          write(obsdatscript(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_DBTZNOBS_obs.csh'
          write(simdatscript(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_DBTZNOBS_sim.csh'
          write(obsdatkey(nbiobservitem),'(a)') 'a'
          do loopo=1,ncomp
            if(zname(loopo).eq.DBZN(loop))then
              write(obsdatzonename(loop),'(a)')zname(loopo)
              iobsdatzone(loop) = loopo  ! remember which one
            endif
          enddo
        elseif(DBTASK(loop)(1:7).eq.'SURTOBS')then
          anymeasured=.true.
          call edisp(iuout,
     &      'Calibration to surface temperatures possible.')
          nbiobservitem=nbiobservitem+1
          iobservitem(nbiobservitem)=loop  ! which ITEMSTD
          write(obsdatfilename(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_SURTOBS_obs.csv'
          lncurobs=lnblnk(obsdatfilename(nbiobservitem))
          ke=k+lncurobs
          write(tarlist(k:ke),'(2a)') ' ',
     &      obsdatfilename(nbiobservitem)(1:lncurobs)
          k=k+lncurobs+1
          write(simdatfilename(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_SURTOBS_sim.csv'
          lncursim=lnblnk(simdatfilename(nbiobservitem))
          ke=k+lncursim
          write(tarlist(k:ke),'(2a)') ' ',
     &      simdatfilename(nbiobservitem)(1:lncursim)
          k=k+lncursim+1
          write(obsdatscript(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_SURTOBS_obs.csh'
          write(simdatscript(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_SURTOBS_sim.csh'
          write(obsdatkey(nbiobservitem),'(a)') 'b'
          do loopo=1,ncomp
            if(zname(loopo).eq.DBZN(loop))then
              write(obsdatzonename(loop),'(a)')zname(loopo)
              iobsdatzone(loop) = loopo  ! remember which one
            endif
          enddo
        elseif(DBTASK(loop)(1:7).eq.'ZNRHOBS')then
          anymeasured=.true.
          call edisp(iuout,'Calibration to zone RH possible.')
          nbiobservitem=nbiobservitem+1
          iobservitem(nbiobservitem)=loop  ! which ITEMSTD
          write(obsdatfilename(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_ZNRHOBS_obs.csv'
          lncurobs=lnblnk(obsdatfilename(nbiobservitem))
          ke=k+lncurobs
          write(tarlist(k:ke),'(2a)') ' ',
     &      obsdatfilename(nbiobservitem)(1:lncurobs)
          k=k+lncurobs+1
          write(simdatfilename(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_ZNRHOBS_sim.csv'
          lncursim=lnblnk(simdatfilename(nbiobservitem))
          ke=k+lncursim
          write(tarlist(k:ke),'(2a)') ' ',
     &      simdatfilename(nbiobservitem)(1:lncursim)
          k=k+lncursim+1
          write(obsdatscript(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_ZNRHOBS_obs.csh'
          write(simdatscript(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_ZNRHOBS_sim.csh'
          write(obsdatkey(nbiobservitem),'(a)') 'c'
          do loopo=1,ncomp
            if(zname(loopo).eq.DBZN(loop))then
              write(obsdatzonename(loop),'(a)')zname(loopo)
              iobsdatzone(loop) = loopo  ! remember which one
            endif
          enddo
        elseif(DBTASK(loop)(1:7).eq.'ZNHTOBS')then
          anymeasured=.true.
          call edisp(iuout,'Calibration to zone heating possible.')
          nbiobservitem=nbiobservitem+1
          iobservitem(nbiobservitem)=loop  ! which ITEMSTD
          write(obsdatfilename(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_ZNHTOBS_obs.csv'
          lncurobs=lnblnk(obsdatfilename(nbiobservitem))
          ke=k+lncurobs
          write(tarlist(k:ke),'(2a)') ' ',
     &      obsdatfilename(nbiobservitem)(1:lncurobs)
          k=k+lncurobs+1
          write(simdatfilename(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_ZNHTOBS_sim.csv'
          lncursim=lnblnk(simdatfilename(nbiobservitem))
          ke=k+lncursim
          write(tarlist(k:ke),'(2a)') ' ',
     &      simdatfilename(nbiobservitem)(1:lncursim)
          k=k+lncursim+1
          write(obsdatscript(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_ZNHTOBS_obs.csh'
          write(simdatscript(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_ZNHTOBS_sim.csh'
          write(obsdatkey(nbiobservitem),'(a)') 'e'
          do loopo=1,ncomp
            if(zname(loopo).eq.DBZN(loop))then
              write(obsdatzonename(loop),'(a)')zname(loopo)
              iobsdatzone(loop) = loopo  ! remember which one
            endif
          enddo
        elseif(DBTASK(loop)(1:7).eq.'ZNCLOBS')then
          anymeasured=.true.
          call edisp(iuout,'Calibration to zone cooling possible.')
          nbiobservitem=nbiobservitem+1
          iobservitem(nbiobservitem)=loop  ! which ITEMSTD
          write(obsdatfilename(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_ZNCLOBS_obs.csv'
          lncurobs=lnblnk(obsdatfilename(nbiobservitem))
          ke=k+lncurobs
          write(tarlist(k:ke),'(2a)') ' ',
     &      obsdatfilename(nbiobservitem)(1:lncurobs)
          k=k+lncurobs+1
          write(simdatfilename(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_ZNCLOBS_sim.csv'
          lncursim=lnblnk(simdatfilename(nbiobservitem))
          ke=k+lncursim
          write(tarlist(k:ke),'(2a)') ' ',
     &      simdatfilename(nbiobservitem)(1:lncursim)
          k=k+lncursim+1
          write(obsdatscript(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_ZNCLOBS_obs.csh'
          write(simdatscript(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_ZNCLOBS_sim.csh'
          write(obsdatkey(nbiobservitem),'(a)') 'f'
          do loopo=1,ncomp
            if(zname(loopo).eq.DBZN(loop))then
              write(obsdatzonename(loop),'(a)')zname(loopo)
              iobsdatzone(loop) = loopo  ! remember which one
            endif
          enddo
        endif
      enddo
      
      return
      end ! of clearcalibarrays

C ******************** clearcshcsvfiles ********************
C Clears csv and csh files from prior calibration work.

      subroutine clearcshcsvfiles(usingcalversion,NNAME)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "tdf2.h"
#include "esprdbfile.h"
#include "material.h"
#include "FMI.h"

C Function definition.
      INTEGER :: lnblnk
      
      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout
     
C TDF related.
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15

C Calibration observation, simulation and csv creation files.
      integer nbiobservitem       ! number of observations in temporal file
      integer iobservitem         ! pointer to temporal index for observation
      character obsdatfilename*72 ! holds observation timestep data
      character obsdatscript*72   ! script which drives res to create obsdatfilename
      character obsdatkey*1       ! 
      character obsdatzonename*12 ! zone name associated with an observation
      integer iobsdatzone         ! index of associate zone
      character simdatfilename*72 ! holds simulated timestep data
      character simdatscript*72   ! script which drives res to create simdatscript
      common/calibrobs/nbiobservitem,iobservitem(20),obsdatfilename(20),
     &  obsdatscript(20),obsdatkey(20),obsdatzonename(20),
     &  iobsdatzone(20),simdatfilename(20),simdatscript(20)

C Scripts used to run calibro calibrations for up to 48 zones.
      integer lntarlistz,lncmdlistz ! length of zone csv files
      integer lntarlist,lncmdlist   ! length of model csv files
      character tarlist*1000      ! list of csv files to tar
      character cmdlist*1100      ! list of csv files to include in local script
      character caliroot*32       ! as cfgroot but with - for _
      character calibro_local*96  ! for local csh
      character calibro_mktar*96  ! csh for invoking server calibro zone runs
      character tarlistz*500      ! list of zone based csv files to tar
      character cmdlistz*500      ! list of zone based csv files for script
      character zdashn*12         ! zone names with _ replaced with -
      logical tfcmdlistz          ! true when action completed
      common/calibinvok/lntarlistz(1:48),lncmdlistz(1:48),
     &  lntarlist,lncmdlist,
     &  tfcmdlistz(1:48),calibro_local(0:48),calibro_mktar(0:48),
     &  tarlist,cmdlist,caliroot,tarlistz(1:48),cmdlistz(1:48),
     &  zdashn(1:48)
      
      logical unixok
      logical usingcalversion
      character*72 OLDLCFGF
      character the_file*96
      integer ier
      integer loop
      character MODE*4
      character NNAME*144
      logical XST
      
      INTEGER :: itrc,istat
      INTEGER :: IUNIT

      call isunix(unixok)

C Delete the csh & csv files that were created.
      if(.NOT.usingcalversion)then

C To delete the correct files we need to load the _cal version of
C the model and then update caliroot.
        call edisp(iuout,
     &    'Switching to calibration variant of the model')
        write(OLDLCFGF,'(a)') LCFGF(1:lnblnk(LCFGF))
        write(LCFGF,'(a)') NNAME(1:lnblnk(NNAME))
        MODE='ALL '
        IUF=IPRODB   ! assign second file unit to the events db unit
        CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
        if(ier.eq.0)then
          usingcalversion=.true.

C Replace _ with - in the root name.
          lnroot=lnblnk(cfgroot)
          caliroot='                              '
          do loopr=1,lnroot
            if(cfgroot(loopr:loopr).eq.'_')then
              write(caliroot(loopr:loopr),'(a)') '-'
            else
              write(caliroot(loopr:loopr),'(a)') 
     &              cfgroot(loopr:loopr)
            endif
          enddo
        else
          call usrmsg('Unable to scan calibration cfg file',
     &      'reverting to last cfg file.','W')
          write(LCFGF,'(a)') OLDLCFGF(1:lnblnk(OLDLCFGF))
          MODE='ALL '
          IUF=IPRODB   ! assign second file unit to the events db unit
          CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
          usingcalversion=.false.
          return
        endif
      endif

      IUNIT=IFIL+1
      write(the_file,'(a)') 'calibro_local.csh'
      call FINDFIL(the_file,XST)
      if(XST)then
        CALL EFOPSEQ(iunit,the_file,1,IER)
        call EFDELET(iunit,ISTAT)
      endif
      write(the_file,'(a)') 'weather_extract.csh'
      call FINDFIL(the_file,XST)
      if(XST)then
        CALL EFOPSEQ(iunit,the_file,1,IER)
        call EFDELET(iunit,ISTAT)
      endif
      write(the_file,'(a)') 'bc.csv'
      call FINDFIL(the_file,XST)
      if(XST)then
        CALL EFOPSEQ(iunit,the_file,1,IER)
        call EFDELET(iunit,ISTAT)
      endif
      write(the_file,'(a)') 'calibro_input.csv'
      call FINDFIL(the_file,XST)
      if(XST)then
        CALL EFOPSEQ(iunit,the_file,1,IER)
        call EFDELET(iunit,ISTAT)
      endif
      write(the_file,'(a)') 'calibro_mktar.csh'
      call FINDFIL(the_file,XST)
      if(XST)then
        CALL EFOPSEQ(iunit,the_file,1,IER)
        call EFDELET(iunit,ISTAT)
      endif
      write(the_file,'(2a)') caliroot(1:lnblnk(caliroot)),
     &  '.tar.gz '
      call FINDFIL(the_file,XST)
      if(XST)then
        CALL EFOPSEQ(iunit,the_file,1,IER)
        call EFDELET(iunit,ISTAT)
      endif
      do loop=1,ITEMSTD
        if(DBTASK(loop)(1:8).eq.'DBTZNOBS'.or.
     &     DBTASK(loop)(1:7).eq.'SURTOBS'.or.
     &     DBTASK(loop)(1:7).eq.'ZNRHOBS'.or.
     &     DBTASK(loop)(1:7).eq.'ZNHTOBS'.or.
     &     DBTASK(loop)(1:7).eq.'ZNCLOBS')then
          loopz=iobsdatzone(loop)  ! the associated zone
          lnzn=lnblnk(zdashn(loopz))

          write(the_file,'(a)')   ! script to extract observations
     &      obsdatscript(loop)(1:lnblnk(obsdatscript(loop)))
          call FINDFIL(the_file,XST)
          if(XST)then
            CALL EFOPSEQ(iunit,the_file,1,IER)
            call EFDELET(iunit,ISTAT)
          endif
          write(the_file,'(a)')   ! csv file of observations 
     &      obsdatfilename(loop)(1:lnblnk(obsdatfilename(loop)))
          IUNIT=IFIL+1
          call FINDFIL(the_file,XST)
          if(XST)then
            CALL EFOPSEQ(iunit,the_file,1,IER)
            call EFDELET(iunit,ISTAT)
          endif
          write(the_file,'(a)')   ! script to extract predictions 
     &      simdatscript(loop)(1:lnblnk(simdatscript(loop)))
          call FINDFIL(the_file,XST)
          if(XST)then
            CALL EFOPSEQ(iunit,the_file,1,IER)
            call EFDELET(iunit,ISTAT)
          endif
          write(the_file,'(a)')   ! csv file of predictions 
     &      simdatfilename(loop)(1:lnblnk(simdatfilename(loop)))
          call FINDFIL(the_file,XST)
          if(XST)then
            CALL EFOPSEQ(iunit,the_file,1,IER)
            call EFDELET(iunit,ISTAT)
          endif
          write(the_file,'(3a)') 'calibro_local', ! script to invoke local calibro
     &      zdashn(loopz)(1:lnzn),'.csh'
          call FINDFIL(the_file,XST)
          if(XST)then
            CALL EFOPSEQ(iunit,the_file,1,IER)
            call EFDELET(iunit,ISTAT)
          endif
          write(the_file,'(a)') 'fort.36' ! trace file for calibration actions
          call FINDFIL(the_file,XST)
          if(XST)then
            CALL EFOPSEQ(iunit,the_file,1,IER)
            call EFDELET(iunit,ISTAT)
          endif
          write(the_file,'(3a)') 'calibro_mktar', ! script to create zone tar
     &      zdashn(loopz)(1:lnzn),'.csh'
          call FINDFIL(the_file,XST)
          if(XST)then
            CALL EFOPSEQ(iunit,the_file,1,IER)
            call EFDELET(iunit,ISTAT)
          endif
          write(the_file,'(3a)') caliroot(1:lnblnk(caliroot)),
     &      zdashn(loopz)(1:lnzn),'.tar.gz '
          call FINDFIL(the_file,XST)   ! name of compressed zone tar
          if(XST)then
            CALL EFOPSEQ(iunit,the_file,1,IER)
            call EFDELET(iunit,ISTAT)
          endif
          write(the_file,'(3a)') 'calibro_input_',  ! name of uncertainty delts
     &      zdashn(loopz)(1:lnzn),'.csv '
          call FINDFIL(the_file,XST)
          if(XST)then
            CALL EFOPSEQ(iunit,the_file,1,IER)
            call EFDELET(iunit,ISTAT)
          endif
        endif
      enddo

      return
      end

C ******************** createcalvariantfiles ********************
C createcalvariantfiles - creates variant cfg and zone files 
C based on the uncertainties that have been defined e.g.
C domlc, dogeo, doopr, doctl, doblnd.

      subroutine createcalvariantfiles(domlc,dogeo,doopr,doctl,doblnd,
     &  docnn)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "esprdbfile.h"
#include "material.h"
#include "FMI.h"

      INTEGER :: lnblnk
      
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout
      
      INTEGER :: ncomp,ncon
      common/C1/NCOMP,NCON
      common/cctlnm/ctldoc,lctlf
      character ctldoc*248,lctlf*72

      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      INTEGER :: nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),scfdres(MSPS),sipvres
      character sblres*72,sflres*72,splres*72,smstres*72,
     &  selres*72,scfdres*72,sipvres*72
      
      logical unixok
      logical domlc,dogeo,doopr,doctl,doblnd,docnn  ! tasks to do
      character outs*148
      integer ier
      integer loop
      integer nbitem,items
      dimension items(MCOM)
      dimension ivalsg(MCOM)
      character APE*6,EXT*4,MODE*4
      character NNAME*144
      logical XST

      INTEGER :: IUNIT

      call isunix(unixok)


C If materials and/or mlc or optical call VER_MLC_MAT.
      write(APE,'(a)') '_cal'
      if(domlc.or.doblnd)then
        call VER_MLC_MAT(APE)
      endif

C If geometry then call VER_GEO_CON_TMC.
      if(dogeo)then
        nbitem=ncomp
        do loop=1,ncomp
          items(loop)=loop
          ivalsg(loop)=0 ! none will have been processed
        enddo
        call VER_GEO_CON_TMC(nbitem,items,ivalsg,APE,docnn)
      endif

C If operations then call VER_OPR.
      if(doopr)then 
        nbitem=ncomp
        do loop=1,ncomp
          items(loop)=loop
        enddo
        call VER_OPR(nbitem,items,APE,docnn)
      endif
      if(doctl)then
        EXT='.ctl'
        IUNIT=12
        call FINDFIL(LCTLF,XST)
        IF(.NOT.XST)THEN
          CALL EDISP(IUOUT,'* Control file does not exist!')
          RETURN
        ENDIF
        CALL FNCNGR(LCTLF,APE,EXT,NNAME)
        write(LCTLF,'(a)') NNAME(1:lnblnk(NNAME))
        CALL CTLWRT(IUNIT,IER)
        docnn=.true.
        call edisp(iuout,'* Control file being copied.')
      endif

C Write out configuration file but first
C change names of results libraries zones, flow, plant,
C moisture, electrical, cfd and IPV.
      if(domlc.or.dogeo.or.doopr.or.doctl.or.doblnd)then
        DO 123 ISPS=1,NSSET
          EXT='.res'
          CALL FNCNGR(SBLRES(ISPS),APE,EXT,NNAME)
          write(SBLRES(ISPS),'(a)') NNAME(1:lnblnk(NNAME))
          EXT='.mfr'
          CALL FNCNGR(SFLRES(ISPS),APE,EXT,NNAME)
          write(SFLRES(ISPS),'(a)') NNAME(1:lnblnk(NNAME))
          EXT='.plr'
          CALL FNCNGR(SPLRES(ISPS),APE,EXT,NNAME)
          write(SPLRES(ISPS),'(a)') NNAME(1:lnblnk(NNAME))
          EXT='.msr'
          CALL FNCNGR(SMSTRES(ISPS),APE,EXT,NNAME)
          write(SMSTRES(ISPS),'(a)') NNAME(1:lnblnk(NNAME))
          EXT='.res'
          CALL FNCNGR(SELRES(ISPS),APE,EXT,NNAME)
          write(SELRES(ISPS),'(a)') NNAME(1:lnblnk(NNAME))
          EXT='.dfr'
          CALL FNCNGR(SCFDRES(ISPS),APE,EXT,NNAME)
          write(SCFDRES(ISPS),'(a)') NNAME(1:lnblnk(NNAME))
 123    CONTINUE
        EXT='.rep'
        CALL FNCNGR(SIPVRES,APE,EXT,NNAME)
        write(SIPVRES,'(a)') NNAME(1:lnblnk(NNAME))

C Check length of cfgroot string. Figure out how to include the
C whole of ape in the root name, even if this requires removing
C some characters from the middle of the string buffer.
        irootlen=LNBLNK(CFGROOT)
        iapelin=lnblnk(ape)
        IF(irootlen.GT.(32-iapelin))THEN
          irootlen=32-iapelin
        ENDIF
        write(cfgroot,'(2a)')cfgroot(1:irootlen),ape(1:iapelin)

C Create variant name for model cfg file.
        EXT='.cfg'
        CALL FNCNGR(LCFGF,APE,EXT,NNAME)
        write(LCFGF,'(a)') NNAME(1:lnblnk(NNAME))

C If a variant connections file also required set this up.
        if(docnn)then
          EXT='.cnn'
          CALL FNCNGR(LCNN,APE,EXT,NNAME)
          write(LCNN,'(a)') NNAME(1:lnblnk(NNAME))
        endif
        CALL EMKCFG('-',IER)
        WRITE (OUTS,'(2A)')'* Writing new configuration file ',LCFGF
        CALL EDISP(IUOUT,OUTS)
      endif
      return
      end  ! of createcalvariantfiles

C ******************** docalibrationruns  ********************
C Ivokes the simulations needed for calibration.

      subroutine docalibrationruns(itrc,ier)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "tdf2.h"
#include "help.h"

      integer itrc,ier

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      INTEGER :: mmod,limit,limtty

      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      INTEGER :: nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

      INTEGER :: IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      common/spfldes/spfdescr(MSPS)
      character spfdescr*30
      INTEGER :: isstday,isstmon,isfnday,isfnmon
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)

C Extended simulation parameters for each set.
      common/spfldats/isstupex(MSPS),isbnstepex(MSPS),ispnstepex(MSPS),
     &  issaveex(MSPS),isavghex(MSPS),iscfdactivate(MSPS),
     &  isicfdys(MSPS),isicfdyf(MSPS),
     &  scftims(MSPS),scftimf(MSPS)
      INTEGER :: isstupex,isbnstepex,ispnstepex,issaveex,isavghex
      INTEGER :: iscfdactivate      ! zero ignore domains
      INTEGER :: isicfdys,isicfdyf  ! CFD simulation start & finish days
      REAL :: scftims,scftimf       ! CFD simulation start & finish time

      character descra*7,descrb*7,descrst*10,descrfn*10
      character descr2st*8,descr2fn*8
      character tmode*8,doit*248
      character outs*148,aut*12,key*1
      character longtfile*144,longtfiledos*144
      character additional*12

C dd is character array for selecting simulation parameter sets.
      character dd(MSPS+2)*55,brw*11
      LOGICAL concat,unixok
      INTEGER :: ic,ii
      INTEGER :: ijdstart,ijdfinish,IRT,isauto

      helpinsub='sensa'  ! set for subroutine
      
      call isunix(unixok)
      aut=' silent'

C Check if there are simulation parameter sets, if so user should
C select them before proceeding.
      ier=0
      if(nsset.eq.0)then
        call usrmsg('Please define a simulation parameter set',
     &    'prior to invoking a calibration run.','W')
        ier=1  ! so calling code can decide what to do
        return
      endif
 170  continue

C Select an existing set or cancel.
      do ii=1, nsset
        CALL EDAY(isstday(ii),isstmon(ii),ijdstart)
        call stdate(iyear,ijdstart,descra,descrst,descr2st)
        CALL EDAY(isfnday(ii),isfnmon(ii),ijdfinish)
        call stdate(iyear,ijdfinish,descrb,descrfn,descr2fn)
        CALL EMKEY(ii,KEY,IER)
        write(dd(ii),'(7a)') key,' ',spfdescr(ii)
     &    (1:lnblnk(spfdescr(ii))),' ',descrst,'-',descrfn
      enddo
      dd(nsset+1)='! cancel           '
      dd(nsset+2)='? help             '
      dd(nsset+3)='- exit             '
      ic=nsset+3

      if(MMOD.eq.8)then
        IRT=-1
      else
        IRT=-2
      endif
      CALL EMENU('Parameter sets',dd,ic,IRT)
      if(IRT.ge.ic) then
        ier=1
        return
      elseif(IRT.eq.ic-2) then
        ier=1
        return
      elseif(IRT.eq.ic-1) then
        helpinsub='prj'  ! set other source
        helptopic='prj_sim_set_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Simulation sets',nbhelp,'-',0,0,IER)
        helpinsub='sensa'  ! reset for subroutine
        goto 170
      elseif(IRT.ge.1.and.IRT.le.nsset)then
        isset=IRT  ! choose an existing set
      endif
      aut=' silent'
      isauto=2
      if(unixok)then
        call addpath(LCFGF,longtfile,concat)
      else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
        call addpath(LCFGF,longtfile,concat)
        call cmdfiledos(longtfile,longtfiledos,ier)

C Debug the patched file name.
        write(outs,'(2a)') '* Corrected file ',
     &    longtfiledos(1:lnblnk(longtfiledos))
        call edisp248(iuout,outs,100)
        longtfile=' '
        longtfile=longtfiledos
      endif
      if(itrc.gt.1)then
        brw = ' -v -b no '
      else
        brw = ' -b no '
      endif

C Check that the timesteps of the simulation match that of the
C temporal file. If NTSPH is still zero then we need to scan the
c header of the ASCII temporal file to get this information.
      if(NTSPH.eq.0)then
        call supplyandcheck(ltdfa,'P',ier)
      endif
      if(isbnstepex(isset).eq.NTSPH)then
        continue
      else
        call usrmsg(
     &  'The simulation timestep does not match timestep of temporal',
     &  'observations so calibration tasks probably will not work!',
     &  'W')
      endif

C Start the calibration simulation in text mode so that the sensitivity
C can be run. Assume a monti-carlo assessment with 90 runs.
      tmode='text'
      write(doit,'(8a)') 'bps -mode text -s 0 0 0 ',
     &  '-file ',longtfile(1:lnblnk(longtfile)),brw,' -p ',
     &  spfdescr(isset)(1:lnblnk(spfdescr(isset))),aut,
     &  ' -mca 90'
C      call usrmsg('Begining simulation via',doit,'-')
      call runit(doit,tmode)

      return
      end

C ******************** doextractsetdata ********************
C Creates and executes shell scripts to recover
C data from a MontiCarlo results set.

      subroutine doextractsetdata(itrc,ier)
#include "building.h"
#include "model.h"
#include "uncertainty.h"
#include "net_flow.h"
#include "tdf2.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
#include "FMI.h"

      INTEGER :: lnblnk
      
      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout
      
      INTEGER :: indcfg
      common/C6/INDCFG

      common/user/browse

      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      INTEGER :: nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),scfdres(MSPS),sipvres
      character sblres*72,sflres*72,splres*72,smstres*72,
     &  selres*72,scfdres*72,sipvres*72
     
C TDF related.
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15

C Calibration observation, simulation and csv creation files.
      integer nbiobservitem       ! number of observations in temporal file
      integer iobservitem         ! pointer to temporal index for observation
      character obsdatfilename*72 ! holds observation timestep data
      character obsdatscript*72   ! script which drives res to create obsdatfilename
      character obsdatkey*1       ! 
      character obsdatzonename*12 ! zone name associated with an observation
      integer iobsdatzone         ! index of associate zone
      character simdatfilename*72 ! holds simulated timestep data
      character simdatscript*72   ! script which drives res to create simdatscript
      common/calibrobs/nbiobservitem,iobservitem(20),obsdatfilename(20),
     &  obsdatscript(20),obsdatkey(20),obsdatzonename(20),
     &  iobsdatzone(20),simdatfilename(20),simdatscript(20)

C Scripts used to run calibro calibrations for up to 48 zones.
      integer lntarlistz,lncmdlistz ! length of zone csv files
      integer lntarlist,lncmdlist   ! length of model csv files
      character tarlist*1000      ! list of csv files to tar
      character cmdlist*1100      ! list of csv files to include in local script
      character caliroot*32       ! as cfgroot but with - for _
      character calibro_local*96  ! for local csh
      character calibro_mktar*96  ! csh for invoking server calibro zone runs
      character tarlistz*500      ! list of zone based csv files to tar
      character cmdlistz*500      ! list of zone based csv files for script
      character zdashn*12         ! zone names with _ replaced with -
      logical tfcmdlistz          ! true when action completed
      common/calibinvok/lntarlistz(1:48),lncmdlistz(1:48),
     &  lntarlist,lncmdlist,
     &  tfcmdlistz(1:48),calibro_local(0:48),calibro_mktar(0:48),
     &  tarlist,cmdlist,caliroot,tarlistz(1:48),cmdlistz(1:48),
     &  zdashn(1:48)

      integer nbcolforzone  ! how many input columns for each zone
      integer icolforzone   ! array of associated columns
      common/calibcolumns/nbcolforzone(1:48),icolforzone(1:48,MCOM)

      logical browse
      logical havefile,unixok,XST
      LOGICAL remote
      character*72 tfile
      character longtfile*144,longtfiledos*144
      character tmode*8,doit*248
      character outs*148,outs2*1000,fs*1,key*1
      character ltpath*72,filen*72,the_file*72
      character weather_extr*96
      character obs_extr*96,sim_extr*96
      character LOUTSTR*1000,WORD*42,L2K*2000
      integer ier,itis   ! for matching json tokens
      integer loop,loop2
      integer izlist
      integer iobscount,isimcount,ibccount  ! number of lines in csv files
      dimension izlist(MCOM)
      character namen*32
      dimension namen(200)
      real TABU
      dimension TABU(200)
      character strval*12      

      INTEGER :: itrc,istat
      INTEGER :: IUNIT
      INTEGER :: ltrf
      
C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      remote=.false.

C The simulation has finished. Generate script to extract the
C weather data from the results file.
      write(weather_extr,'(a)') 'weather_extract.csh'
      write(currentfile,'(a)') 
     &  weather_extr(1:lnblnk(weather_extr))
      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,weather_extr,3,IER)
      write(iunit,'(a)')'#!/bin/csh'
      write(iunit,'(a)')'set RESFILE=$1'
      write(iunit,'(a)')'res -mode text -file $RESFILE<<XXX'
      write(iunit,'(a)')' '
      write(iunit,'(a)')'b # particular set'
      write(iunit,'(a)')'a # base case'
      write(iunit,'(a)')'- # continue'
      write(iunit,'(a)')'c # timestep'
      write(iunit,'(a)')'& # labels '
      write(iunit,'(a)')'& # with no # '
      write(iunit,'(a)')'g # performance metrics'
      write(iunit,'(a)')'> # to file'
      write(iunit,'(a)')'bc.csv'
      write(iunit,'(a)')'standard report'
      write(iunit,'(a)')'* # time '
      write(iunit,'(a)')'a # no separator '
      write(iunit,'(a)')'^ # delimiter '
      write(iunit,'(a)')'e # comma '
      write(iunit,'(a)')'a # climate '
      write(iunit,'(a)')'a # ambient '
      write(iunit,'(a)')'- # continue'
      write(iunit,'(a)')'a '
      write(iunit,'(a)')'b # solar globH '
      write(iunit,'(a)')'- # continue'
      write(iunit,'(a)')'a '
      write(iunit,'(a)')'c # solar diffuse '
      write(iunit,'(a)')'- # continue'
      write(iunit,'(a)')'a '
      write(iunit,'(a)')'d # wind speed '
      write(iunit,'(a)')'- # continue'
      write(iunit,'(a)')'a '
      write(iunit,'(a)')'e # wind direction '
      write(iunit,'(a)')'- # continue'
      write(iunit,'(a)')'a '
      write(iunit,'(a)')'f # RH '
      write(iunit,'(a)')'- # continue'
      write(iunit,'(a)')'! # list '
      write(iunit,'(a)')'> # close file '
      write(iunit,'(a)')'- # exit '
      write(iunit,'(a)')'- # exit '
      write(iunit,'(a)')'- # quit '
      write(iunit,'(a)')'XXX'
      CALL ERPFREE(IUNIT,ISTAT)
      call makescriptexecutable('weather_extract.csh')

      if(nsset.eq.0)then
        call usrmsg('Please define a simulation parameter set',
     &    'prior to invoking a calibration run.','W')
        ier=1  ! so calling code can decide what to do
        return
      endif

C If there are simulation parameter sets use the name of the result
C file sblres and prepend user home folder if remote is true.
      if(isset.eq.0)isset=1
      if(INDCFG.ge.1)then
        if(sblres(isset)(1:2).ne.'  '.and.
     &     sblres(isset)(1:4).ne.'UNKN')then
          ltrf = lnblnk(sblres(isset))
          write(tfile,'(a)') sblres(isset)(1:ltrf)
          write(longtfile,'(a)') sblres(isset)(1:ltrf)
          if(remote)then
            call fdroot(tfile,ltpath,filen)
            call isunix(unixok)
            if(unixok)then
              if (ICHAR(ltpath(1:1)).ne.47) then
                write(longtfile,'(3a)') upath(1:lnblnk(upath)),
     &            fs,filen(1:lnblnk(filen))
              endif
            else
              if (ltpath(2:2).ne.':') then
                write(longtfile,'(3a)') upath(1:lnblnk(upath)),
     &            fs,filen(1:lnblnk(filen))
              endif
            endif
          endif
          havefile=.true.
        else
          tfile=' '
          havefile=.false.
        endif
        call edisp(iuout,
     &    'Take a note of the results file used.')
        call edisp248(iuout,longtfile,100)
        call edisp(iuout,' ')
      endif

C Need to invoke res in a similar way to code block starting line 1203
      call pausems(1000)
      call tstamp('>','PRJ: beginning res')
      doit = ' '

C However we got to this point, if Windows based look for spaces
C and or forward slashes.
      if(unixok)then
        continue
      else
        call cmdfiledos(longtfile,longtfiledos,ier)
        longtfile=' '
        longtfile=longtfiledos
      endif

      tmode='text'
      if(havefile)then
        write(doit,'(2a)') './weather_extract.csh ',
     &    longtfile(1:lnblnk(longtfile))
        call runit(doit,tmode)
      else
        call edisp(iuout,'Results file not found!')
      endif

C Count the number of lines in the bc.csv file for later comparison
C with observations and predictions csv files.ibccount

C Scan the file of predictions counting the number of lines.
      ibccount=0
      write(the_file,'(a)') 'bc.csv'
      LN=max(1,lnblnk(the_file))
      INQUIRE (FILE=the_file(1:LN),EXIST=XST)
      IF (XST) THEN
        CALL EFOPSEQ(IUNIT,the_file,1,IER)
    9   READ(IUNIT,'(a)',IOSTAT=IOS,END=104)OUTSTR
        ibccount=ibccount+1
        goto 9
  104   CALL ERPFREE(IUNIT,ISTAT)
      ENDIF

C Construct a script to extract observation data into csv files.
C Do this once per observation. It is important that the number
C of lines created matches the number of lines of data in the
C associated predictions file.
      call edisp(iuout,' ')
      call edisp(iuout,'Extraction of observations...')
      do loop=1,nbiobservitem
        itis=iobservitem(loop)
        WRITE(outs,'(i2,a,i2,6a)')loop,':',itis,DBTAG(itis),
     &    DBTASK(itis),' ',DBZN(itis),' ',DBSN(itis)
        call edisp(iuout,outs)
        call edisp(iuout,obsdatfilename(loop))
        call edisp(iuout,obsdatkey(loop))
        write(obs_extr,'(a)') 
     &    obsdatscript(loop)(1:lnblnk(obsdatscript(loop)))
        write(currentfile,'(a)') obs_extr(1:lnblnk(obs_extr))
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,obs_extr,3,IER)
        write(iunit,'(a)')'#!/bin/csh'
        write(iunit,'(a)')'set RESFILE=$1'
        write(iunit,'(a)')'res -mode text -file $RESFILE<<XXX'
        write(iunit,'(a)')' '
        write(iunit,'(a)')'a # uncertaintes'
        write(iunit,'(a)')'c # timestep'
        write(iunit,'(a)')'g # performance metrics'
        write(iunit,'(a)')'> # to file'
        write(iunit,'(a)') obsdatfilename(loop)
        write(iunit,'(a)')'standard report'
        write(iunit,'(a)')'& # label'
        write(iunit,'(a)')'& # with no #'
        write(iunit,'(a)')'* # time '
        write(iunit,'(a)')'a # no separator '
        write(iunit,'(a)')'^ # delimiter '
        write(iunit,'(a)')'e # comma '
        write(iunit,'(a)')'p # observed '
        write(iunit,'(a)') obsdatkey(loop)

C Return position in the loop array
        itis=iobservitem(loop)
        CALL EMKEY(iobsdatzone(itis),KEY,IER)  ! find a-z for assoc zone

        write(iunit,'(a)') KEY
        write(iunit,'(a)')'- # exit '
        write(iunit,'(a)')'! # list '
        write(iunit,'(a)')'> # close file '
        write(iunit,'(a)')'- # exit '
        write(iunit,'(a)')'- # exit '
        write(iunit,'(a)')'- # quit '
        write(iunit,'(a)')'XXX'
        CALL ERPFREE(IUNIT,ISTAT)

        call makescriptexecutable(obs_extr)
        call edisp(iuout,
     &    'About to res-scan the results file...')
        call edisp248(iuout,longtfile,100)
        call edisp(iuout,' ')
        call tstamp('>','PRJ: beginning res')
        call pausems(1000)
        doit = ' '

C However we got to this point invoke the script that extracts
C observations, if Windows based look for spaces and or forward slashes.
        if(unixok)then
          continue
        else
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        tmode='text'
        if(havefile)then
          write(doit,'(4a)') './',obs_extr(1:lnblnk(obs_extr)),
     &      ' ',longtfile(1:lnblnk(longtfile))
          call runit(doit,tmode)
          call pausems(1000)
        else
          call edisp(iuout,'Could not find results file.')
        endif

C Scan the file of observations counting the number of lines.
        iobscount=0
        LN=max(1,lnblnk(obsdatfilename(loop)))
        INQUIRE (FILE=obsdatfilename(loop)(1:ln),EXIST=XST)
        IF (XST) THEN
          CALL EFOPSEQ(IUNIT,obsdatfilename(loop),1,IER)
    7     READ(IUNIT,'(a)',IOSTAT=IOS,END=102)OUTSTR
          iobscount=iobscount+1
          goto 7
  102     CALL ERPFREE(IUNIT,ISTAT)
        ENDIF

C Now invoke res for the simulated data associated with this observation.
        call edisp(iuout,'Extraction of matching predictions...')
        call edisp(iuout,simdatfilename(loop))
        call edisp(iuout,simdatscript(loop))
        itis=iobservitem(loop)
        CALL EMKEY(iobsdatzone(itis),KEY,IER)  ! find a-z for assoc zone
        write(sim_extr,'(a)') 
     &    simdatscript(loop)(1:lnblnk(simdatscript(loop)))
        write(currentfile,'(a)') sim_extr(1:lnblnk(sim_extr))
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,sim_extr,3,IER)
        write(iunit,'(a)')'#!/bin/csh'
        write(iunit,'(a)')'set RESFILE=$1'
        write(iunit,'(a)')'res -mode text -file $RESFILE<<XXX'
        write(iunit,'(a)')' '
        write(iunit,'(a)')'a # uncertaintes'
        write(iunit,'(a)')'i # sensitivity'
        write(iunit,'(a)')'4 # select zone '
        write(iunit,'(a)') KEY
        write(iunit,'(a)')'-'
        write(iunit,'(a)')'a # within sets'
        write(iunit,'(a)')'> # to file'
        write(iunit,'(a)')simdatfilename(loop)
        write(iunit,'(a)')'standard report'
        write(iunit,'(a)')'& # label'
        write(iunit,'(a)')'& # with no #'
        write(iunit,'(a)')'* # time '
        write(iunit,'(a)')'a # no separator '
        write(iunit,'(a)')'^ # delimiter '
        write(iunit,'(a)')'e # comma '
        itis=iobservitem(loop)  ! recover pointer
        if(DBTASK(itis)(1:8).eq.'DBTZNOBS')then
          write(iunit,'(a)')'b # temps'
          write(iunit,'(a)')'a # db temps'
          write(iunit,'(a)')'- '
        elseif(DBTASK(itis)(1:7).eq.'SURTOBS')then
          write(iunit,'(a)')'b # temps'  ! more information needed
          write(iunit,'(a)')'i # inside face'
          write(iunit,'(a)')'- '
          write(iunit,'(a)')'a # first surf'
        elseif(DBTASK(itis)(1:7).eq.'ZNRHOBS')then
          write(iunit,'(a)')'i # RH'
        elseif(DBTASK(itis)(1:7).eq.'ZNHTOBS')then
          write(iunit,'(a)')'h # hch'
          write(iunit,'(a)')'a # sensibled heat'
        elseif(DBTASK(itis)(1:7).eq.'ZNCLOBS')then
          write(iunit,'(a)')'h # hch'
          write(iunit,'(a)')'b # sensibled cooling'
        endif
        write(iunit,'(a)')'* # activate all'
        write(iunit,'(a)')'- '
        write(iunit,'(a)')'b # tabular list '
        write(iunit,'(a)')'> # toggle to display'
        write(iunit,'(a)')'- # exit '
        write(iunit,'(a)')'- # exit '
        write(iunit,'(a)')'- # quit '
        write(iunit,'(a)')'XXX'
        CALL ERPFREE(IUNIT,ISTAT)
        call makescriptexecutable(sim_extr)
        call pausems(1000)
        doit = ' '

C However we got to this point, if Windows based look for spaces
C and or forward slashes.
        if(unixok)then
          continue
        else
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        tmode='text'
        if(havefile)then
          write(doit,'(4a)') './',sim_extr(1:lnblnk(sim_extr)),
     &      ' ',longtfile(1:lnblnk(longtfile))
          call runit(doit,tmode)
          call pausems(1000)
        else
          call edisp(iuout,'Results file not found!')
        endif

C Scan the file of predictions counting the number of lines.
        isimcount=0
        LN=max(1,lnblnk(simdatfilename(loop)))
        INQUIRE (FILE=simdatfilename(loop)(1:ln),EXIST=XST)
        IF (XST) THEN
          CALL EFOPSEQ(IUNIT,simdatfilename(loop),1,IER)
    8     READ(IUNIT,'(a)',IOSTAT=IOS,END=103)L2K
          isimcount=isimcount+1
          goto 8
  103     CALL ERPFREE(IUNIT,ISTAT)
        ENDIF

C Check if number of lines are the same for observations and predictions.
        if(iobscount.eq.0.or.isimcount.eq.0.or.ibccount.eq.0)then
          call usrmsg(
     &      'Either boundary or observations or prediction csv file',
     &      'has ZERO lines! Data recovery failed.','W')
        elseif(iobscount.ne.isimcount)then
          call usrmsg(
     &      'Number of observation lines does not match predictions',
     &      'number of lines. Check that timesteps MATCH!','W')
        elseif(iobscount.ne.ibccount)then
          call usrmsg(
     &      'Number of observation lines does not match boundary',
     &      'conditions number of lines. Check that timesteps MATCH!',
     &      'W')
        endif

C And repeat for the next observation.
      enddo

C When all are finished copy the fort.37 file to calibro_input_model.csv.
      write(doit,'(a)') 'cp fort.37 calibro_input.csv'
      call edisp(iuout,'Copy -> calibro_input.csv')
      call runit(doit,tmode)
      call pausems(500)
      write(doit,'(a)') 'cp fort.37 calibro_input_model.csv'
      call edisp(iuout,'Copy -> calibro_input_model.csv')
      call runit(doit,tmode)
      call pausems(500)

C We no longer need the scripts that did the extract so clean up. 
      write(the_file,'(a)') 'weather_extract.csh'
      call FINDFIL(the_file,XST)
      if(XST)then
        CALL EFOPSEQ(iunit,the_file,1,IER)
        call EFDELET(iunit,ISTAT)
      endif

C The fort.37 files holds the Deltas for each of the uncertainties
C (columns) with a row for each set in the simulation. To support
C zone based Calibro runs we need to select from fort.37 the
C specific lines for each zone in the model. We do this by
C Opening the fort.37 file that was written out by bps, read the 
C first line (it will be a long string) and parse the phrases
C on the line. 
      IUNIT=IFIL
      write(tfile,'(a)') './fort.37'
      CALL EFOPSEQ(IUNIT,tfile,1,IER)
      CALL STRIPC1K(IUNIT,LOUTSTR,99,ND,1,'header',IER)
      if(ND.gt.0)then

C There are ND items on the line, each is one of the uncertainty
C actions. For each phrase call splitcalinputhead to find
C the associated zone(or zones). 
        k=0
        do loop=1,ND
          CALL EGETW(LOUTSTR,K,WORD,'W','name',IER)
          call splitcalinputhead(WORD,itact,itis,itwhere,
     &      nbzone,izlist)

C If associated zones found update calibcolumns variables.
          if(nbzone.gt.0)then
            do loop2=1,nbzone
              iz=izlist(loop2)
              nbcolforzone(iz)=nbcolforzone(iz)+1
              icolforzone(iz,nbcolforzone(iz))=loop
            enddo
          endif
        enddo
        CALL ERPFREE(IUNIT,ios)
        do loop2=1,48
          if(nbcolforzone(loop2).gt.0)then

C There is one or more columns associated with the loop2 zone.
C We could open fort.37, create a new file for the zone and
C read the heading and then, for each line, write out only the
C associated columns.
C            write(6,*) 'icolforzone ',loop2,
C     &        (icolforzone(loop2,j),j=1,nbcolforzone(loop2))
 
            lnzn=lnblnk(zdashn(loop2))
            IUNIT=IFIL

C Re-open fort.37 and then create a new file for the zone.
            write(tfile,'(a)') './fort.37'
            CALL EFOPSEQ(IUNIT,tfile,1,IER)
            write(the_file,'(3a)') 'calibro_input_',
     &        zdashn(loop2)(1:lnzn),'.csv'
            IUNIT2=IFIL+1
            CALL EFOPSEQ(IUNIT2,the_file,3,IER)

C Read the header.
            CALL STRIPC1K(IUNIT,LOUTSTR,99,ND,1,'header',IER)
            if(ND.gt.0)then
              K=0
              isva=ND
              inisz=200
              call egetagwsa(loutstr,k,IUNIT,isva,namen,inisz,'W',
     &          'column names',ierv)
C Write nbcolforzone items from the namen array into outs2 buffer.
              outs2=' '; k=1; ke=1
              do loop4=1,nbcolforzone(loop2)
                icol=icolforzone(loop2,loop4)
                lnn=lnblnk(namen(icol))
                ke=k+lnn 
                write(outs2(k:ke),'(2a)') namen(icol)(1:lnn),','
                k=k+lnn+1
              enddo
              lnn=lnblnk(outs2)
              write(iunit2,'(a)') outs2(1:lnn-1)        
            endif

C Read the data lines.
   42       CALL STRIPC1K(IUNIT,LOUTSTR,99,ND,1,'data',IER1)
            if(ier1.ne.0)goto 43
            K=0
            IRVA=ND
            call EGETAGWRA1K(loutstr,K,IUNIT,IRVA,TABU,0.00,1.00,'-',
     &        'calibro input column data',IER)
            outs2=' '; k=1; ke=1
            do loop4=1,nbcolforzone(loop2)
              icol=icolforzone(loop2,loop4)
              strval=' '        ! clear string buffer for the DIR value
              call relstr(TABU(icol),strval,lna,ier)
              lstrval=lnblnk(strval)
              ke=k+lstrval
              write(outs2(k:ke),'(2a)') strval(1:lstrval),','
              k=k+lstrval+1
            enddo
            lnn=lnblnk(outs2)
            write(iunit2,'(a)') outs2(1:lnn-1)        
            goto 42
  43        continue
            CALL ERPFREE(IUNIT2,ios)
          endif
        enddo  ! of loop2
      endif
      CALL ERPFREE(IUNIT,ios)
      return
      end ! of doextractsetdata

C ******************** docreatescripts ********************
C docreatescripts concatenates the model and zone calibration
C csv file names into lists needed in support of creating tar.gz
C files to send to server based Calibro or local Calibro.
C returns islocal for use in subsequent runlocalorserver call.

      subroutine docreatescripts(islocal,ier)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "tdf2.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
#include "FMI.h"

      INTEGER :: lnblnk
      
      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout

C TDF related.
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15

C Calibration observation, simulation and csv creation files.
      integer nbiobservitem       ! number of observations in temporal file
      integer iobservitem         ! pointer to temporal index for observation
      character obsdatfilename*72 ! holds observation timestep data
      character obsdatscript*72   ! script which drives res to create obsdatfilename
      character obsdatkey*1       ! 
      character obsdatzonename*12 ! zone name associated with an observation
      integer iobsdatzone         ! index of associate zone
      character simdatfilename*72 ! holds simulated timestep data
      character simdatscript*72   ! script which drives res to create simdatscript
      common/calibrobs/nbiobservitem,iobservitem(20),obsdatfilename(20),
     &  obsdatscript(20),obsdatkey(20),obsdatzonename(20),
     &  iobsdatzone(20),simdatfilename(20),simdatscript(20)

C Scripts used to run calibro calibrations for up to 48 zones.
      integer lntarlistz,lncmdlistz ! length of zone csv files
      integer lntarlist,lncmdlist   ! length of model csv files
      character tarlist*1000      ! list of csv files to tar
      character cmdlist*1100      ! list of csv files to include in local script
      character caliroot*32       ! as cfgroot but with - for _
      character calibro_local*96  ! for local csh
      character calibro_mktar*96  ! csh for invoking server calibro zone runs
      character tarlistz*500      ! list of zone based csv files to tar
      character cmdlistz*500      ! list of zone based csv files for script
      character zdashn*12         ! zone names with _ replaced with -
      logical tfcmdlistz          ! true when action completed
      common/calibinvok/lntarlistz(1:48),lncmdlistz(1:48),
     &  lntarlist,lncmdlist,
     &  tfcmdlistz(1:48),calibro_local(0:48),calibro_mktar(0:48),
     &  tarlist,cmdlist,caliroot,tarlistz(1:48),cmdlistz(1:48),
     &  zdashn(1:48)

C Remember how many calibro runs were requested.
      integer ihowmanycalibroruns
      common/howmany/ihowmanycalibroruns

      logical unixok,XST
      character tmode*8,doit*248
      character doit1k*1024  ! command to invoke
      character the_file*96
      integer ier
      integer loop
      
      INTEGER :: istat
      INTEGER :: IUNIT
      
      call isunix(unixok)

C Create command line for invoking server calibro for whole model.
      write(calibro_mktar(0),'(a)') 'calibro_mktar.csh'
      write(currentfile,'(a)') 
     &  calibro_mktar(0)(1:lnblnk(calibro_mktar(0)))
      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,calibro_mktar(0),3,IER)
      write(iunit,'(a)')'#!/bin/bash'
      write(iunit,'(3a)') 'calibrateme.bash ./',
     &  caliroot(1:lnblnk(caliroot)),'.tar.gz '
      CALL ERPFREE(IUNIT,ISTAT)  ! close calibro_mktar.csh
      call makescriptexecutable('calibro_mktar.csh')
      call edisp(iuout,
     &  'Generating calibro_mktar.csh which you can invoke to run')
      call edisp(iuout,
     &  'the full-matrix calibrateme.bash server script.')
      call pausems(1000)

C Create scripts that can run either local calibrino or on server.
C Initial (common section of global and zone commands).
C Replace _ with - in the root name.
      lnroot=lnblnk(cfgroot)
      caliroot='                              '
      do loop=1,lnroot
        if(cfgroot(loop:loop).eq.'_')then
          write(caliroot(loop:loop),'(a)') '-'
        else
          write(caliroot(loop:loop),'(a)') cfgroot(loop:loop)
        endif
      enddo
C      lncmdlist=lnblnk(cmdlist)+2
      k=lncmdlist; ke=lncmdlist

C Loop through all the associated temporal items and for
C those which are observed add to the cmd lists.
      loopz=0
      do loop=1,ITEMSTD
        if(DBTASK(loop)(1:8).eq.'DBTZNOBS'.or.
     &     DBTASK(loop)(1:7).eq.'SURTOBS'.or.
     &     DBTASK(loop)(1:7).eq.'ZNRHOBS'.or.
     &     DBTASK(loop)(1:7).eq.'ZNHTOBS'.or.
     &     DBTASK(loop)(1:7).eq.'ZNCLOBS')then
          lncurobs=lnblnk(obsdatfilename(loop))

C Update the global cmdlist.
          ke=k+lncurobs
          write(cmdlist(k:ke),'(2a)') 
     &      obsdatfilename(loop)(1:lncurobs),','
          k=k+lncurobs+1
          loopz=iobsdatzone(loop)  ! the associated zone

C Write the observed.csv for the zone into the tar list.
          kz=lntarlistz(loopz); kze=kz+lncurobs
          write(tarlistz(loopz)(kz:kze),'(2a)') 
     &      obsdatfilename(loop)(1:lncurobs),' '
          lntarlistz(loopz)=lnblnk(tarlistz(loopz))+2

C Write the observed.csv for the command string. First write
C the name of the observed file and then ','
C          lncmdlistz(loopz)=lnblnk(cmdlistz(loopz))+1
          kz=lncmdlistz(loopz); kze=kz+lncurobs
          write(cmdlistz(loopz)(kz:kze),'(a)') 
     &      obsdatfilename(loop)(1:lncurobs)
          lncmdlistz(loopz)=lnblnk(cmdlistz(loopz))+1
          kz=lncmdlistz(loopz); kze=kz+1
          write(cmdlistz(loopz)(kz:kze),'(a)') ','
          lncmdlistz(loopz)=lnblnk(cmdlistz(loopz))+1
        endif
      enddo
      k=k-1  ! get rid of trailing comma
      ke=k+4
      write(cmdlist(k:ke),'(a)') ' -s '

C Append a ' -s ' to the zone command. But only do it once
C per zone.
      do loop=1,ITEMSTD
        if(DBTASK(loop)(1:8).eq.'DBTZNOBS'.or.
     &     DBTASK(loop)(1:7).eq.'SURTOBS'.or.
     &     DBTASK(loop)(1:7).eq.'ZNRHOBS'.or.
     &     DBTASK(loop)(1:7).eq.'ZNHTOBS'.or.
     &     DBTASK(loop)(1:7).eq.'ZNCLOBS')then
          loopz=iobsdatzone(loop)  ! the associated zone
          if(tfcmdlistz(loopz))then
            continue ! only do ' -s ' once
          else
            lncmdlistz(loopz)=lncmdlistz(loopz)-1 ! get rid of trailing comma
            kz=lncmdlistz(loopz); kze=kz+4
            write(cmdlistz(loopz)(kz:kze),'(a)') ' -s '
            lncmdlistz(loopz)=lnblnk(cmdlistz(loopz))+2
            tfcmdlistz(loopz)=.true.  ! signal we have done it
          endif
        endif
      enddo

C Clear tfcmdlistz.
      do loop=1,48
        tfcmdlistz(loop)=.false.
      enddo

C Add in the *sim.csv files.
      k=ke
      loopz=0
      do loop=1,ITEMSTD
        if(DBTASK(loop)(1:8).eq.'DBTZNOBS'.or.
     &     DBTASK(loop)(1:7).eq.'SURTOBS'.or.
     &     DBTASK(loop)(1:7).eq.'ZNRHOBS'.or.
     &     DBTASK(loop)(1:7).eq.'ZNHTOBS'.or.
     &     DBTASK(loop)(1:7).eq.'ZNCLOBS')then
          lncursim=lnblnk(simdatfilename(loop))
          ke=k+lncursim
          write(cmdlist(k:ke),'(2a)') 
     &      simdatfilename(loop)(1:lncursim),','
          k=k+lncursim+1
          loopz=iobsdatzone(loop)  ! the associated zone

C Write the *sim.csv for the zone.
          kz=lntarlistz(loopz); kze=kz+lncursim
          write(tarlistz(loopz)(kz:kze),'(a)') 
     &      simdatfilename(loop)(1:lncursim)
          lntarlistz(loopz)=lnblnk(tarlistz(loopz))+1
          kz=lntarlistz(loopz); kze=kz+1
          write(tarlistz(loopz)(kz:kze),'(a)') ' '
          lntarlistz(loopz)=lnblnk(tarlistz(loopz))+2

C Write the *sim.csv for the command string.
C          lncmdlistz(loopz)=lnblnk(cmdlistz(loopz))+1
          kz=lncmdlistz(loopz); kze=kz+lncursim
          write(cmdlistz(loopz)(kz:kze),'(a)') 
     &      simdatfilename(loop)(1:lncursim)
          lncmdlistz(loopz)=lnblnk(cmdlistz(loopz))+1
          kz=lncmdlistz(loopz); kze=kz+1
          write(cmdlistz(loopz)(kz:kze),'(a)') ','
          lncmdlistz(loopz)=lnblnk(cmdlistz(loopz))+1
        endif
      enddo
      k=k-1    ! get rid of trailing comma
      ke=k+37  ! request both json and pdf outputs
      write(cmdlist(k:ke),'(a)') ' --fmt json,pdf --res cal,sa,train,ds'

C At this point there will be a calibro_input_model.csv but the
C server needs calibro_input.csv. Re-copy the fort.37 to this
      write(doit,'(a)') 'cp fort.37 calibro_input.csv'
      call edisp(iuout,'Copy -> calibro_input.csv')
      call runit(doit,tmode)
      call pausems(500)

C Make up the whole matrix tar command which was created when
C clearcalibarrays was called. First remove any existing gz file
C by that name.
      IUNIT=IFIL+1
      write(the_file,'(2a)') caliroot(1:lnblnk(caliroot)),
     &  '.tar.gz '
      call FINDFIL(the_file,XST)
      if(XST)then
        CALL EFOPSEQ(iunit,the_file,1,IER)
        call EFDELET(iunit,ISTAT)
      endif

C Create model level tar file for server use and compress it.
      tmode='text'
      write(doit1k,'(4a)') 'tar cf ',caliroot(1:lnblnk(caliroot)),
     &  '.tar ',tarlist(1:lnblnk(tarlist))
      call edisp(iuout,'Creating model .tar file.')
      call runit(doit1k,tmode)
      call pausems(1000)

C Compress the model level tar file.
      write(doit,'(3a)') 'gzip ',caliroot(1:lnblnk(caliroot)),
     &  '.tar '
      call edisp(iuout,'Compressing model .tar file.')
      call runit(doit,tmode)
      call pausems(1000)

C Remove the calibro_input.csv file so it does not clash with
C zone based files.
      write(doit,'(a)') 'rm calibro_input.csv'
      call edisp(iuout,'Remove -> calibro_input.csv')
      call runit(doit,tmode)
      call pausems(500)

C calibro is a stochastic process so run it 5-9 times. Get user
C confirmation.
      ihowmanycalibroruns=5
      call EASKI(ihowmanycalibroruns,
     &  'Calibro is a stocastic process so you should run it',
     &  '5-20 times. How many to do',1,'W',20,
     &  'W',8,'calibrino runs',IERI,nbhelp)

C Common tasks completed

      call edisp(iuout,
     &  'The extracted csv files as well as a tar.gz file with these')
      call edisp(iuout,
     &  'are now available. If you have a local calibrino tool or')
      call edisp(iuout,
     &  'the calibrateme.bash script you can proceed.')
      CALL EASKABC(
     &  'The csv files and tar.gz file are available. Options:',
     &  ' ','invoke local calibrino','invoke remote calibro',
     &  'cancel',islocal,nbhelp)
      if(islocal.eq.1)then

C Create command line for invoking calibrino with the full
C cmdlist and also setup a script file in case user wants
C to run it again.
        write(calibro_local(0),'(a)') 'calibro_local.csh'
        write(currentfile,'(a)') 
     &    calibro_local(0)(1:lnblnk(calibro_local(0)))
        IUNIT=IFIL+1
        CALL ERPFREE(IUNIT,ISTAT)  ! close script unit
        CALL EFOPSEQ(IUNIT,calibro_local(0),3,IER)
        write(iunit,'(a)')'#!/bin/csh'
        do looph=1,ihowmanycalibroruns
          write(iunit,'(a,i2.2,a)')'echo "Invoking run ',looph,'.."'
          write(iunit,'(a)')cmdlist(1:lnblnk(cmdlist))
          write(iunit,'(2a,i2.2,a)') 'cp ./calibro_report.json ',
     &      caliroot(1:lnblnk(caliroot)),looph,'.json'
          write(iunit,'(2a,i2.2,a)') 'cp ./calibro_report.pdf ',
     &      caliroot(1:lnblnk(caliroot)),looph,'.pdf'
          write(iunit,'(a)')'sleep 10'
        enddo

C Create a temporarly file which signals that the calibrino
C run has finished.
        write(iunit,'(2a,i2.2,a)') 'echo finished_calibrino >>',
     &    'calibro_done'
        write(iunit,'(a)')'sleep 10'
        CALL ERPFREE(IUNIT,ISTAT)  ! close calibro_local.csh
        call makescriptexecutable(calibro_local(0))

C Append a ' --fmt json,pdf --res cal,sa,train,ds' to the zone command.
        do loop=1,ITEMSTD
          if(DBTASK(loop)(1:8).eq.'DBTZNOBS'.or.
     &       DBTASK(loop)(1:7).eq.'SURTOBS'.or.
     &       DBTASK(loop)(1:7).eq.'ZNRHOBS'.or.
     &       DBTASK(loop)(1:7).eq.'ZNHTOBS'.or.
     &       DBTASK(loop)(1:7).eq.'ZNCLOBS')then
            loopz=iobsdatzone(loop)  ! the associated zone
            lnzn=lnblnk(zdashn(loopz))

            if(tfcmdlistz(loopz))then
              continue ! only do it once per zone
            else
              lncmdlistz(loopz)=lncmdlistz(loopz)-1 ! get rid of trailing comma
              kz=lncmdlistz(loopz); kze=kz+38
              write(cmdlistz(loopz)(kz:kze),'(a)') 
     &          ' --fmt json,pdf --res cal,sa,train,ds '
              lncmdlistz(loopz)=lnblnk(cmdlistz(loopz))

C Place this command in a script file for later execution. Also
C copy the resulting files to include the zone name.
              IUNIT=IFIL+1
              CALL ERPFREE(IUNIT,ISTAT)  ! close script unit
              write(calibro_local(loopz),'(3a)') 'calibro_local',
     &          zdashn(loopz)(1:lnzn),'.csh'
              write(currentfile,'(a)') 
     &          calibro_local(loopz)(1:lnblnk(calibro_local(loopz)))
              CALL EFOPSEQ(IUNIT,calibro_local(loopz),3,IER)
              write(iunit,'(a)') '#!/bin/csh'
              do looph=1,ihowmanycalibroruns
                write(iunit,'(a,i2.2,a)')'echo "Invoking run ',
     &            looph,'.."'
                write(iunit,'(a)') 
     &          cmdlistz(loopz)(1:lnblnk(cmdlistz(loopz)))
                write(iunit,'(3a,i2.2,a)') 'cp ./calibro_report.json ',
     &            caliroot(1:lnblnk(caliroot)),zdashn(loopz)(1:lnzn),
     &            looph,'.json'

C In case we want to save the *.tex file for this zone uncomment.
C                write(iunit,'(3a,i2.2,a)') 'cp ./calibro_report.tex ',
C     &            caliroot(1:lnblnk(caliroot)),zdashn(loopz)(1:lnzn),
C     &            looph,'.tex'
                write(iunit,'(3a,i2.2,a)') 'cp ./calibro_report.pdf ',
     &            caliroot(1:lnblnk(caliroot)),zdashn(loopz)(1:lnzn),
     &            looph,'.pdf'
              enddo

C Create a temporarly file which signals that the calibrino
C run has finished.
              write(iunit,'(2a,i2.2,a)') 'echo finished_calibrino >>',
     &          'calibro_done'
              write(iunit,'(a)')'sleep 10'
              CALL ERPFREE(IUNIT,ISTAT)  ! close zone calibro_local.csh
              call makescriptexecutable(calibro_local(loopz))
            endif
          endif
        enddo

C For each of the zone scripts make them executable.
C Clear tfcmdlistz.
        do loop=1,48
          tfcmdlistz(loop)=.false.
        enddo
        loopz=0
        do loop=1,ITEMSTD
          if(DBTASK(loop)(1:8).eq.'DBTZNOBS'.or.
     &       DBTASK(loop)(1:7).eq.'SURTOBS'.or.
     &       DBTASK(loop)(1:7).eq.'ZNRHOBS'.or.
     &       DBTASK(loop)(1:7).eq.'ZNHTOBS'.or.
     &       DBTASK(loop)(1:7).eq.'ZNCLOBS')then
            loopz=iobsdatzone(loop)  ! the associated zone
            lnzn=lnblnk(zdashn(loopz))

C First do the local zone command script.
            if(tfcmdlistz(loopz))then
              continue ! only do script once
            else

C Now do the commands for zone based calibro_mktar.
              IUNIT=IFIL+1
              write(calibro_mktar(loopz),'(3a)') 'calibro_mktar',
     &          zdashn(loopz)(1:lnzn),'.csh'
              write(currentfile,'(a)') 
     &          calibro_mktar(loopz)(1:lnblnk(calibro_mktar(loopz)))
              CALL ERPFREE(IUNIT,ISTAT)  ! close calibro_mktar.csh
              CALL EFOPSEQ(IUNIT,calibro_mktar(loopz),3,IER)
              write(iunit,'(a)')'#!/bin/bash'
              write(iunit,'(4a)') 'calibrateme.bash ./',
     &          caliroot(1:lnblnk(caliroot)),zdashn(loopz)(1:lnzn),
     &          '.tar.gz '
              CALL ERPFREE(IUNIT,ISTAT)  ! close calibro_mktar.csh
              call makescriptexecutable(calibro_mktar(loopz))

C Make up the zone tar command and then compress it. First remove
C any existing file by that name.
              IUNIT=IFIL+1
              write(the_file,'(3a)') caliroot(1:lnblnk(caliroot)),
     &          zdashn(loopz)(1:lnzn),'.tar.gz '
              call FINDFIL(the_file,XST)
              if(XST)then
                CALL EFOPSEQ(iunit,the_file,1,IER)
                call EFDELET(iunit,ISTAT)
              endif

C To support zone-by zone we have to copy the zone based calibro_input.csv
C file to the standard file name.
              write(the_file,'(3a)') 'calibro_input_',
     &          zdashn(loopz)(1:lnzn),'.csv'
              write(doit,'(3a)') 'cp ',the_file(1:lnblnk(the_file)),
     &          ' calibro_input.csv'
              call edisp(iuout,'Copy zone file -> calibro_input.csv')
              tmode='text'
              call runit(doit,tmode)
              call pausems(500)
C              write(doit,'(a)') 'more calibro_input.csv'
C              call edisp(iuout,'list -> calibro_input.csv')
C              tmode='text'
C              call runit(doit,tmode)
C              call pausems(500)

              write(doit1k,'(5a)') 'tar cf ',
     &          caliroot(1:lnblnk(caliroot)),zdashn(loopz)(1:lnzn),
     &          '.tar ',tarlistz(loopz)(1:lnblnk(tarlistz(loopz)))
              call edisp(iuout,'Creating zone .tar file.')
              call runit(doit1k,tmode)
              call pausems(500)
              tfcmdlistz(loopz)=.true.  ! signal we have done
              write(doit,'(4a)') 'gzip ',
     &          caliroot(1:lnblnk(caliroot)),zdashn(loopz)(1:lnzn),
     &          '.tar '
              call edisp(iuout,'Compressing zone .tar file.')
              call runit(doit,tmode)
              call pausems(500)
            endif
          endif
        enddo

        call edisp(iuout,
     &    'Generating calibro_local.csh which you can invoke manually')
        call edisp(iuout,'if you wish to repeat the calibration.')

      elseif(islocal.eq.2)then

C For each of the zone scripts make them executable and
C clear tfcmdlistz.
        tmode='text'
        do loop=1,48
          tfcmdlistz(loop)=.false.
        enddo
        loopz=0
        do loop=1,ITEMSTD
          if(DBTASK(loop)(1:8).eq.'DBTZNOBS'.or.
     &       DBTASK(loop)(1:7).eq.'SURTOBS'.or.
     &       DBTASK(loop)(1:7).eq.'ZNRHOBS'.or.
     &       DBTASK(loop)(1:7).eq.'ZNHTOBS'.or.
     &       DBTASK(loop)(1:7).eq.'ZNCLOBS')then
            loopz=iobsdatzone(loop)  ! the associated zone
            lnzn=lnblnk(zdashn(loopz))

C First do the local zone command script.
            if(tfcmdlistz(loopz))then
              continue ! only do script once
            else

C Now do the commands for zone based calibro_mktar. First copy
C the zone-based calibro_input file to calibro_input.csv
              write(the_file,'(3a)') 'calibro_input_',
     &          zdashn(loopz)(1:lnzn),'.csv'
              write(doit,'(3a)') 'cp ',the_file(1:lnblnk(the_file)),
     &          ' calibro_input.csv'
              call edisp(iuout,'Copy zone file -> calibro_input.csv')
              tmode='text'
              call runit(doit,tmode)
              call pausems(500)
              call FINDFIL('./calibro_input.csv',XST)
              if(.NOT.XST)then
                call edisp(iuout,'unable to locate calibro_input.csv')
              endif
              IUNIT=IFIL+1
              write(calibro_mktar(loopz),'(3a)') 'calibro_mktar',
     &          zdashn(loopz)(1:lnzn),'.csh'
              write(currentfile,'(a)') 
     &          calibro_mktar(loopz)(1:lnblnk(calibro_mktar(loopz)))
              CALL ERPFREE(IUNIT,ISTAT)  ! close calibro_mktar.csh
              CALL EFOPSEQ(IUNIT,calibro_mktar(loopz),3,IER)
              write(iunit,'(a)')'#!/bin/bash'
              write(iunit,'(4a)') 'calibrateme.bash ./',
     &          caliroot(1:lnblnk(caliroot)),zdashn(loopz)(1:lnzn),
     &          '.tar.gz '
              CALL ERPFREE(IUNIT,ISTAT)  ! close calibro_mktar.csh
              call makescriptexecutable(calibro_mktar(loopz))

C Make up the zone tar command and then compress it. First remove
C any existing file by that name.
              IUNIT=IFIL+1
              write(the_file,'(3a)') caliroot(1:lnblnk(caliroot)),
     &          zdashn(loopz)(1:lnzn),'.tar.gz '
              call FINDFIL(the_file,XST)
              if(XST)then
                CALL EFOPSEQ(iunit,the_file,1,IER)
                call EFDELET(iunit,ISTAT)
              endif
              write(doit1k,'(5a)') 'tar cf ',
     &          caliroot(1:lnblnk(caliroot)),zdashn(loopz)(1:lnzn),
     &          '.tar ',tarlistz(loopz)(1:lnblnk(tarlistz(loopz)))
              call edisp(iuout,'Creating zone tar file')
              call runit(doit1k,tmode)
              call pausems(500)
              tfcmdlistz(loopz)=.true.  ! signal we have done
              write(doit,'(4a)') 'gzip ',
     &          caliroot(1:lnblnk(caliroot)),zdashn(loopz)(1:lnzn),
     &          '.tar '
              call edisp(iuout,'Compressing zone tar file.')
              call runit(doit,tmode)
              call pausems(500)
            endif
          endif
        enddo

      elseif(islocal.eq.3)then
        ier=3
        return
      endif

      return
      end  ! of docreatescripts

C ******************** runlocalorserver ********************
C Invokes calibro runs locally or on a server.

      subroutine runlocalorserver(islocal)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "tdf2.h"
#include "esprdbfile.h"
#include "material.h"
#include "FMI.h"

      INTEGER :: lnblnk

      integer islocal  ! 1=local 2=server
      
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout

C TDF related.
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15

C Calibration observation, simulation and csv creation files.
      integer nbiobservitem       ! number of observations in temporal file
      integer iobservitem         ! pointer to temporal index for observation
      character obsdatfilename*72 ! holds observation timestep data
      character obsdatscript*72   ! script which drives res to create obsdatfilename
      character obsdatkey*1       ! 
      character obsdatzonename*12 ! zone name associated with an observation
      integer iobsdatzone         ! index of associate zone
      character simdatfilename*72 ! holds simulated timestep data
      character simdatscript*72   ! script which drives res to create simdatscript
      common/calibrobs/nbiobservitem,iobservitem(20),obsdatfilename(20),
     &  obsdatscript(20),obsdatkey(20),obsdatzonename(20),
     &  iobsdatzone(20),simdatfilename(20),simdatscript(20)

C Scripts used to run calibro calibrations for up to 48 zones.
      integer lntarlistz,lncmdlistz ! length of zone csv files
      integer lntarlist,lncmdlist   ! length of model csv files
      character tarlist*1000      ! list of csv files to tar
      character cmdlist*1100      ! list of csv files to include in local script
      character caliroot*32       ! as cfgroot but with - for _
      character calibro_local*96  ! for local csh
      character calibro_mktar*96  ! csh for invoking server calibro zone runs
      character tarlistz*500      ! list of zone based csv files to tar
      character cmdlistz*500      ! list of zone based csv files for script
      character zdashn*12         ! zone names with _ replaced with -
      logical tfcmdlistz          ! true when action completed
      common/calibinvok/lntarlistz(1:48),lncmdlistz(1:48),
     &  lntarlist,lncmdlist,
     &  tfcmdlistz(1:48),calibro_local(0:48),calibro_mktar(0:48),
     &  tarlist,cmdlist,caliroot,tarlistz(1:48),cmdlistz(1:48),
     &  zdashn(1:48)

C Remember how many calibro runs were requested.
      integer ihowmanycalibroruns
      common/howmany/ihowmanycalibroruns

      logical unixok
      character tmode*8,doit*248
      character outs*148
      integer loop
      
      call isunix(unixok)
      if(islocal.eq.1)then

C Local Calibro. ASK USER WHAT TO DO.
        CALL EASKABC(' ','Calibrino Options:',
     &    'run all zones together','each zone a separate run',
     &    'cancel',ioption,nbhelp)
        if(ioption.eq.1)then
          call pausems(500)
          call edisp(iuout,
     &      'Executing calibrino command. This could take some')
          call edisp(iuout,
     &      'time! You will need to wait until all requested runs')
          call edisp(iuout,
     &      'have completed.')

C Issue command in background mode so prj is still live.
          write(doit,'(3a)') './',
     &      calibro_local(0)(1:lnblnk(calibro_local(0))),' & ' 
          tmode='text'
          call runit(doit,tmode)
          call pausems(5000)

        elseif(ioption.eq.2)then

C Clear tfcmdlistz.
          do loop=1,48
            tfcmdlistz(loop)=.false.
          enddo
          do loop=1,ITEMSTD
            if(DBTASK(loop)(1:8).eq.'DBTZNOBS'.or.
     &         DBTASK(loop)(1:7).eq.'SURTOBS'.or.
     &         DBTASK(loop)(1:7).eq.'ZNRHOBS'.or.
     &         DBTASK(loop)(1:7).eq.'ZNHTOBS'.or.
     &         DBTASK(loop)(1:7).eq.'ZNCLOBS')then
              loopz=iobsdatzone(loop)  ! the associated zone
              lnzn=lnblnk(zdashn(loopz))
              if(tfcmdlistz(loopz))then
                continue ! only invoke once per zone
              else
                call edisp(iuout,
     &          'Executing calibrino command. This could take some')
                call edisp(iuout,
     &          'time! You will need to wait until all requested runs')
                call edisp(iuout,
     &          'have completed.')
                call pausems(500)
                write(outs,'(2a)') 'Executing calibrino script for ',
     &            zdashn(loopz)(1:lnzn)
                call edisp(iuout,outs)  ! do it in forground
                write(doit,'(2a)')'./',
     &            calibro_local(loopz)(1:lnblnk(calibro_local(loopz)))
C If in parallel, the created .json files can get confused.
C                write(doit,'(3a)')'./',
C     &            calibro_local(loopz)(1:lnblnk(calibro_local(loopz))),
C     &            ' & ' 
C                write(6,'(2a)')
C     &            calibro_local(loopz)(1:lnblnk(calibro_local(loopz))),
C     &            ' & ' 
                tmode='text'
                call runit(doit,tmode)
                call pausems(6000)
                tfcmdlistz(loopz)=.true.  ! signal we have done it
              endif
            endif
          enddo
        elseif(ioption.eq.3)then
          continue
        endif

      elseif(islocal.eq.2)then

C Create command line for invoking calibrino with the full
C tarlist and also setup a script file in case user wants
C to run it again. calibro is a stocastic process so run 
C it 5-9 times.
        CALL EASKABC(' ','Server options:',
     &    'run zones together','run zones separately',
     &    'cancel',ioption,nbhelp)
        tmode='text'
        if(ioption.eq.1)then
          call edisp(iuout,
     &      'Executing server calibrino command. This could take')
          call edisp(iuout,
     &      'some time! Command windows will appear. When a run is')
          call edisp(iuout,
     &      'you must [press any key to continue]. When all requested')
          call edisp(iuout,
     &      'have completed you can preview/implement suggestions.')
          call edisp(iuout,' ')
          do looph=1,ihowmanycalibroruns
            write(outs,'(2a,i2.2)')'Executing server command for run',
     &        looph
            call edisp(iuout,outs)
            write(doit,'(2a)') 
     &        calibro_mktar(0)(1:lnblnk(calibro_mktar(0))),' & '
            call edisp(iuout,'Invoking calibrateme')
            call runit(doit,tmode)
            call edisp(iuout,'Look out for the script to return.')
            call pausems(45000)   ! 45 second pause
          enddo
          ier=0
          return
        elseif(ioption.eq.2)then

C Clear tfcmdlistz in preparation for invoking a series of
C calibration tasks.
          call edisp(iuout,
     &      'Executing server calibrino command. This could take')
          call edisp(iuout,
     &      'some time! Command windows will appear. When a run is')
          call edisp(iuout,
     &      'you must [press any key to continue]. When all requested')
          call edisp(iuout,
     &      'have completed you can preview/implement suggestions.')
          call edisp(iuout,' ')
          do loop=1,48
            tfcmdlistz(loop)=.false.
          enddo
          do loop=1,ITEMSTD
            if(DBTASK(loop)(1:8).eq.'DBTZNOBS'.or.
     &         DBTASK(loop)(1:7).eq.'SURTOBS'.or.
     &         DBTASK(loop)(1:7).eq.'ZNRHOBS'.or.
     &         DBTASK(loop)(1:7).eq.'ZNHTOBS'.or.
     &         DBTASK(loop)(1:7).eq.'ZNCLOBS')then
              loopz=iobsdatzone(loop)  ! the associated zone
              lnzn=lnblnk(zdashn(loopz))

              if(tfcmdlistz(loopz))then
                continue ! only do script once
              else
                do looph=1,ihowmanycalibroruns
                  call pausems(500)
                  write(outs,'(3a,i2.2)') 
     &              'Executing server command for ',
     &              zdashn(loopz)(1:lnzn),' run ',looph
                  call edisp(iuout,outs)
                  call isunix(unixok)
                  if(unixok)then

C Try to do it explicitly so it persists until finished.
                    write(doit,'(3a)') 'xterm -e ./',
     &             calibro_mktar(loopz)(1:lnblnk(calibro_mktar(loopz))),
     &              ' & '
                    call edisp248(iuout,doit,100)
                    tmode='graphic'
                    call runit(doit,tmode)
                    call pausems(45000)   ! 45 second pause
                  else
                    write(doit,'(3a)') './',
     &             calibro_mktar(loopz)(1:lnblnk(calibro_mktar(loopz))),
     &                ' & '
                    call edisp248(iuout,doit,100)
                    tmode='text'
                    call runit(doit,tmode)
                  endif
                  call pausems(45000)  ! 45 second pause
                enddo
                tfcmdlistz(loopz)=.true.  ! signal we have done it
              endif
            endif
          enddo
          ier=0
        elseif(ioption.eq.3)then
          ier=3
          return
        endif
      endif

      return
      end


C ******************** previewofjson ********************
C Scans a .json file and recovers the patterns
C so suggestions can be previewed or acted upon.

      subroutine previewofjson(LTEMP,preview,silent,IER)
#include "building.h"
#include "model.h"
#include "uncertainty.h"
#include "net_flow.h"
#include "tdf2.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
#include "FMI.h"

      INTEGER :: lnblnk ! Function definition.
      
      common/FILEP/IFIL
      INTEGER :: ifil
      
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout
      
      logical unixok,LIBXST
      character loutstr*248
      character outs*148
      integer ier,itis,itwhere,itact   ! for matching json tokens
      CHARACTER LTEMP*96
      integer loop,loop2,loop3
      character buffer*96,first*42,second*42,CH*1
      character jtag*42,phrase*96
      logical preview
      logical proceed  ! track user request to look at another suggestion
      logical silent   ! do the change with minimal interaction
      logical withincalibresults ! we have estimates

      character lcurley*1,rcurley*1,lsquare*1,rsquare*1
      
      INTEGER :: itrc,itru
      INTEGER :: IUNIT

      call isunix(unixok)
      preview=.false.
      withincalibresults=.false.
      lcurley = char(123)  ! {
      rcurley = char(125)  ! }
      lsquare = char(91)   ! [
      rsquare = char(93)   ! ]
      
C If the .json file exists, scan it.
      INQUIRE (FILE=LTEMP,EXIST=LIBXST)
      if (.NOT.LIBXST) then
        write(outs,'(3a)') 'JSON file ',LTEMP(1:lnblnk(LTEMP)),
     &    ' does not exist.'
        call edisp(iuout,outs)
        ier=1
        return  ! Present user choices again.
      endif
      call edisp(iuout,'  ')
      write(outs,'(2A)')
     &  ' Reading calibro json file: ',LTEMP(1:lnblnk(LTEMP))
      call edisp(iuout,outs)
      IUNIT=IFIL
      CALL EFOPSEQ(IUNIT,LTEMP,1,IER)
      if(IER.LT.0)then
        write(outs,'(3a)') 'JSON file ',LTEMP(1:lnblnk(LTEMP)),
     &    ' could not be opened.'
        call edisp(iuout,outs)
        ier=1
        return
      endif
      write(currentfile,'(a)') LTEMP(1:lnblnk(LTEMP))

C Read lines from the file and look for specific key phrases.
  77  call lstripc(IUNIT,loutstr,99,ND,1,'json line 1',IER)

C If end of the file reached in preview mode, jump back.
C Otherwise revert to the original model.
      IF(IER.NE.0)then
        return
      endif
      k=0
      call egetrm(loutstr,K,buffer,'W','tokens',IER)
      if(buffer(1:1).eq.lcurley) goto 77
      if(buffer(1:1).eq.rcurley) goto 77
      if(buffer(1:1).eq.lsquare) goto 77
      if(buffer(1:1).eq.rsquare) goto 77

C Return the tag and phrase. If tag is PARAMETER then parse
C PHRASE into two strings split at the dot. The 1st string
C is the uncertainty and the 2nd is the scope/location.
      k=0
      call EGETJSNTAGPHR(buffer,K,JTAG,PHRASE,'-','json a',ier)
      if(JTAG(1:7).eq.'calibro')then
        call edisp(iuout,'Scanning a calibro file...')
        goto 77
      elseif(JTAG(1:7).eq.'CALIBRO')then
        call edisp(iuout,'Scanning a calibro file...')
        goto 77
      elseif(JTAG(1:11).eq.'calibration')then
        goto 77
      elseif(JTAG(1:19).eq.'CALIBRATION_RESULTS')then
        withincalibresults=.true.
        goto 77
      elseif(JTAG(1:11).eq.'CALIBRATION')then
        goto 77
      elseif(JTAG(1:5).eq.'error')then
        write(outs,'(3a)')'JSON file ',LTEMP(1:lnblnk(LTEMP)),
     &    ' contains errors.'
        call edisp(iuout,outs)
        ier=1
        return
      elseif(JTAG(1:11).eq.'theta.stats')then
        goto 77
      elseif(JTAG(1:9).eq.'PARAMETER')then

C Keyword PARAMETER is followed by dot separated phrase. 
C Use splitstratchar FIRST is the uncertainty and 
C SECOND is the scope/location.
        if(.NOT.withincalibresults) goto 77
        CH='.'; FIRST='  '; SECOND='  '
        call splitstratchar(PHRASE,CH,FIRST,SECOND,ier)
        write(outs,'(4a)') 'Uncertainty what is ',
     &    first(1:lnblnk(first)),' & where ',
     &    second(1:lnblnk(second))
        call edisp(iuout,outs)

C Loop through existing uncertainty defs to find matching LCNG as
C well as a maching LLOC.
        itis=0; itwhere=0; itact=0
        lnfirst=lnblnk(first)
        lnsec=lnblnk(second)
        do loop=1,NIACT
          loop2=IACTD(loop,1)  ! change
          lnlcng=lnblnk(LCNG(loop2))
          loop3=IACTD(loop,2)  ! location
          lnloc=lnblnk(LLOC(loop3))
          if((first(1:lnfirst).eq.LCNG(loop2)(1:lnlcng)).and.
     &       (second(1:lnsec).eq.LLOC(loop3)(1:lnloc)))then
            itis=loop2
            itwhere=loop3
            itact=loop ! the action index
            call LISTUAL(1,itis)  ! remind us what it is
            call LISTUAL(2,itwhere)  ! remind us where it is
            EXIT  ! no need to check further
          endif
        enddo

C Now get the new attribute value.
        call lstripc(IUNIT,loutstr,99,ND,1,'json EST',IER)

C Now look for the new attribute to apply to the uncertainty.
        k=0
        call egetrm(loutstr,K,buffer,'W','tokens',IER)
        k=0
        call EGETJSNTAGR(buffer,K,FIRST,rv,0.01,1.0,'-','ESTM',ier)

C Apply or preview the suggested change.
C        preview=.true.
        call applysuggestion(ITRC,ITRU,preview,silent,itis,itact,
     &    rv,proceed)
        if(proceed) goto 77  ! read further suggestions
        ier=0
        return  ! present user with options
      else
        goto 77  ! read another line
      endif
      return
      end

C ******************** createjsontable ********************
C Fills the jsontable array based on scanning a number of
C json files. If .json were created via local Calibro then the variable
c nbcalibrorun will have been reset prior to scanning the .json files for
C a specific zone. In the case of server runs the user will have supplied
C a list of .json files and we will not know the state of nbcalibrorun
C when each was created.

      subroutine createjsontable(jsonfile,islocal,ier)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "uncertainty.h"
#include "net_flow.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
#include "FMI.h"

      INTEGER :: lnblnk

      character jsonfile*96
      integer ier
      
      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout

      integer nbcalibrorun
      integer nbofzonetj  ! number of zone-actions
      integer nbofzonegf  ! number of zone-observations
      real tablejson ! dimensioned (irun,izone,iact,3) estimate lower upper
      real tablegof  ! dimensioned (irun,izone,iobs,2) before after
      common/jsontable/nbcalibrorun,nbofzonetj(18,48),nbofzonegf(18,48),
     &  tablejson(18,48,20,3),tablegof(18,48,20,2)

C For server runs, establish how many calibro runs were made
C on a zone-by-zone basis.
      integer nbservercalibrorun  ! counter for server based calibro runs
      common/jsontableserver/nbservercalibrorun(48)

C Hold average information for all runs for each zone.
      real avgtablejson ! dimensioned (izone,iact) estimate
      real avgtablegof  ! dimensioned (izone,iobs) after
      integer itisact         ! matching uncertainty distribution
      integer itwhereact      ! matching uncertainty location
      common/avgjsontable/avgtablejson(48,20),avgtablegof(48,20),
     &  itisact(48,20),itwhereact(20)

      character actphrase*32  ! from the PARAMETER LINE
      character dataphrase*24 ! based on observation
      common/jsontabletext/actphrase(18,48,20),dataphrase(18,48,20)
      
      logical unixok,LIBXST,establishzonenb
      character loutstr*248
      character outs*148
      integer itis,itwhere,itact   ! for matching json tokens
      integer loop,loop2,loop3
      character buffer*96,first*42,second*42,CH*1
      character jtag*42,phrase*96,currentactphrase*96
      integer izlist
      dimension izlist(MCOM)
      real rv1,rv2,rv3

      character lcurley*1,rcurley*1,lsquare*1,rsquare*1
      
      INTEGER :: istat
      INTEGER :: IUNIT

      call isunix(unixok)
      lcurley = char(123)  ! {
      rcurley = char(125)  ! }
      lsquare = char(91)   ! [
      rsquare = char(93)   ! ]
      itwhere=0

      INQUIRE (FILE=jsonfile,EXIST=LIBXST)
      if (.NOT.LIBXST) then
        ier=2
        return
      endif
      call edisp(iuout,'  ')
      write(outs,'(2A)')
     &  ' Scanninging calibro json file: ',jsonfile(1:lnblnk(jsonfile))
      call edisp(iuout,outs)
      IUNIT=IFIL
      CALL EFOPSEQ(IUNIT,jsonfile,1,IER)
      if(IER.LT.0)then
        write(outs,'(3a)') 'JSON file ',jsonfile(1:lnblnk(jsonfile)),
     &    ' could not be opened.'
        ier=1
        return
      endif
      write(currentfile,'(a)') jsonfile(1:lnblnk(jsonfile))
      nbofparam=0   ! reset counter for nb of PARAMETER in file.
      establishzonenb=.true.  ! figure out which zone prior to nbservercalibrorun

C Read lines from the file looking for specific key phrases.
  77  call lstripc(IUNIT,loutstr,99,ND,1,'json line 1',IER)

C If we reach the end of the file reset ier and return.
      IF(IER.NE.0)then
        ier=0
        CALL ERPFREE(IUNIT,ISTAT)
        return
      endif
      k=0
      call egetrm(loutstr,K,buffer,'W','tokens',IER)
      if(buffer(1:1).eq.lcurley) goto 77
      if(buffer(1:1).eq.rcurley) goto 77
      if(buffer(1:1).eq.lsquare) goto 77
      if(buffer(1:1).eq.rsquare) goto 77

C Return the tag and phrase. If tag is PARAMETER then parse
C PHRASE into two strings split at the dot. The 1st string
C is the uncertainty and the 2nd is the scope/location.
      write(6,*) 'buffer ',buffer(1:lnblnk(buffer))
      k=0
      call EGETJSNTAGPHR(buffer,K,JTAG,PHRASE,'-','json a',ier)
      if(JTAG(1:7).eq.'calibro')then
        if(islocal.eq.1)then
          nbcalibrorun=nbcalibrorun+1    ! increment for local
        else
          establishzonenb=.true.  ! figure out which zone prior to nbservercalibrorun
        endif
        goto 77
      elseif(JTAG(1:7).eq.'CALIBRO')then
        if(islocal.eq.1)then
          nbcalibrorun=nbcalibrorun+1    ! increment
        else
          establishzonenb=.true.  ! figure out which zone prior to nbservercalibrorun
        endif
        goto 77
      elseif(JTAG(1:11).eq.'calibration')then
        goto 77
      elseif(JTAG(1:5).eq.'error')then
        write(outs,'(3a)') 'JSON file ',jsonfile(1:lnblnk(jsonfile)),
     &    ' contained errors.'
        ier=2
        return
      elseif(JTAG(1:11).eq.'CALIBRATION')then
        goto 77
      elseif(JTAG(1:11).eq.'theta.stats')then
        goto 77
      elseif(JTAG(1:9).eq.'PARAMETER')then

C Keyword PARAMETER is followed by dot separated phrase. 
C Use splitstratchar FIRST is the uncertainty and 
C SECOND is the scope/location. Remember PHRASE.
        write(currentactphrase,'(a)') PHRASE(1:lnblnk(PHRASE))
        nbofparam=nbofparam +1  !increment
        CH='.'; FIRST='  '; SECOND='  '
        call splitstratchar(PHRASE,CH,FIRST,SECOND,ier)

C Loop through existing uncertainty defs to find matching LCNG as
C well as a maching LLOC.
        itis=0; itwhere=0; itact=0
        lnfirst=lnblnk(first)
        lnsec=lnblnk(second)
        do loop=1,NIACT
          loop2=IACTD(loop,1)  ! change
          lnlcng=lnblnk(LCNG(loop2))
          loop3=IACTD(loop,2)  ! location
          lnloc=lnblnk(LLOC(loop3))
          if((first(1:lnfirst).eq.LCNG(loop2)(1:lnlcng)).and.
     &       (second(1:lnsec).eq.LLOC(loop3)(1:lnloc)))then
            itis=loop2
            itwhere=loop3
            itact=loop ! the action index
            if(nbcalibrorun.eq.1)then
              call LISTUAL(1,itis)  ! remind us what it is
              call LISTUAL(2,itwhere)  ! remind us where it is
            endif
            EXIT  ! no need to check further
          endif
        enddo

C If indices are still zero then we did not find a match.
        if(itis.eq.0.or.itwhere.eq.0.or.itact.eq.0)then
          write(6,*) 'Did not find a match for itis itwhere itact',
     &      itis,itwhere,itact
          ier=1
          nbcalibrorun=nbcalibrorun-1    ! decrement
          return
        endif

C Find the associated zone. If there is more than one just take the
C first.
        if(NZGU(itwhere).ge.1)then
          nbzone=1
          izlist(1)=NZNOGU(itwhere,1)
          iz=izlist(1)
          if(islocal.eq.2.and.establishzonenb)then

C Re-establish nbcalibrorun for this zone.
            nbservercalibrorun(iz)=nbservercalibrorun(iz)+1
            nbcalibrorun=nbservercalibrorun(iz)
            establishzonenb=.false.
          endif
          nbofzonetj(nbcalibrorun,iz)=nbofzonetj(nbcalibrorun,iz)+1   ! increment
          nbtj=nbofzonetj(nbcalibrorun,iz)
          write(actphrase(nbcalibrorun,iz,nbtj),'(a)') 
     &      currentactphrase(1:lnblnk(currentactphrase))
        endif
        goto 77
      elseif(JTAG(1:8).eq.'ESTIMATE')then

C Convert phrase into a real and do range checks.
        iz=NZNOGU(itwhere,1)
        itact=nbofzonetj(nbcalibrorun,iz)
        read(phrase,*,ERR=1002)rv1
        tablejson(nbcalibrorun,iz,itact,1)=rv1
        
C Accummulate into avgtablejson and remember which distribution
C and location.
        avgtablejson(iz,itact)=avgtablejson(iz,itact)+rv1
        itisact(iz,itact)=itis
        itwhereact(itact)=itwhere
        goto 77
      elseif(JTAG(1:5).eq.'LOWER')then
        iz=NZNOGU(itwhere,1)
        itact=nbofzonetj(nbcalibrorun,iz)
        read(phrase,*,ERR=1002)rv2
        tablejson(nbcalibrorun,iz,itact,2)=rv2
        goto 77
      elseif(JTAG(1:5).eq.'UPPER')then
        iz=NZNOGU(itwhere,1)
        itact=nbofzonetj(nbcalibrorun,iz)
        read(phrase,*,ERR=1002)rv3
        tablejson(nbcalibrorun,iz,itact,3)=rv3
        goto 77
      elseif(JTAG(1:20).eq.'CALIBRATED_MODEL_GOF')then
        goto 77
      elseif(JTAG(1:7).eq.'DATASET')then

C Increment a counter for datasets. DOTO: Figure out how
C to deal with multiple zones.
        iz=NZNOGU(itwhere,1)
        nbofzonegf(nbcalibrorun,iz)=nbofzonegf(nbcalibrorun,iz)+1  ! increment
        nbgf=nbofzonegf(nbcalibrorun,iz)
        write(dataphrase(nbcalibrorun,iz,nbgf),'(a)') 
     &    PHRASE(1:lnblnk(PHRASE))
        goto 77
      elseif(JTAG(1:6).eq.'BEFORE')then
        iz=NZNOGU(itwhere,1)
        nbdsets=nbofzonegf(nbcalibrorun,iz)
        read(phrase,*,ERR=1002)gof1
        tablegof(nbcalibrorun,iz,nbdsets,1)=gof1
        goto 77
      elseif(JTAG(1:5).eq.'AFTER')then
        iz=NZNOGU(itwhere,1)
        nbdsets=nbofzonegf(nbcalibrorun,iz)
        read(phrase,*,ERR=1002)gof2
        tablegof(nbcalibrorun,iz,nbdsets,2)=gof2

C Accumulate into avgtablegof.
        avgtablegof(iz,nbdsets)=avgtablegof(iz,nbdsets)+gof2
        goto 77
      else
        goto 77  ! read another line
      endif

 1002 continue
      goto 77
      end

C ******************** printjsontable ********************
C Prints the common blocks jsontable & jsontabletext.
C Do this to the text window of the application interface as well as
C to a fort.34 file.

      subroutine printjsontable()
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "uncertainty.h"
#include "net_flow.h"
#include "tdf2.h"
#include "esprdbfile.h"
#include "material.h"
#include "FMI.h"

      INTEGER :: lnblnk
      
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout

      integer nbcalibrorun
      integer nbofzonetj  ! number of zone-actions
      integer nbofzonegf  ! number of zone-observations
      real tablejson ! dimensioned (irun,izone,iact,3) estimate lower upper
      real tablegof  ! dimensioned (irun,izone,iobs,2) before after
      common/jsontable/nbcalibrorun,nbofzonetj(18,48),nbofzonegf(18,48),
     &  tablejson(18,48,20,3),tablegof(18,48,20,2)

C Hold average information for all the runs for each zone.
      real avgtablejson ! dimensioned (izone,iact) estimate
      real avgtablegof  ! dimensioned (izone,iobs) after
      integer itisact         ! matching uncertainty distribution
      integer itwhereact      ! matching uncertainty location
      common/avgjsontable/avgtablejson(48,20),avgtablegof(48,20),
     &  itisact(48,20),itwhereact(20)

      character actphrase*32  ! from the PARAMETER LINE
      character dataphrase*24 ! based on observation
      common/jsontabletext/actphrase(18,48,20),dataphrase(18,48,20)
     
C TDF related.
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15

C Calibration observation, simulation and csv creation files.
      integer nbiobservitem       ! number of observations in temporal file
      integer iobservitem         ! pointer to temporal index for observation
      character obsdatfilename*72 ! holds observation timestep data
      character obsdatscript*72   ! script which drives res to create obsdatfilename
      character obsdatkey*1       ! 
      character obsdatzonename*12 ! zone name associated with an observation
      integer iobsdatzone         ! index of associate zone
      character simdatfilename*72 ! holds simulated timestep data
      character simdatscript*72   ! script which drives res to create simdatscript
      common/calibrobs/nbiobservitem,iobservitem(20),obsdatfilename(20),
     &  obsdatscript(20),obsdatkey(20),obsdatzonename(20),
     &  iobsdatzone(20),simdatfilename(20),simdatscript(20)

C Remember how many calibro runs were requested.
      integer ihowmanycalibroruns
      common/howmany/ihowmanycalibroruns
      
      character outs*148
      character datalabel*24
      dimension datalabel(10)
      integer loop,loop2
      integer nbdl
      
      INTEGER :: istat

      call edisp(iuout,
     &  'run zone name  parameter         suggestion lower upper')
      call edisp(34,
     &  'run zone name  parameter         suggestion lower upper')

      do iz=1,48   ! for each of the possible zones
        nbdl=0
        lnzn=lnblnk(zname(iz))
        do iclear=1,10
          datalabel(iclear)='  '
        enddo

C Find observations associated with this zone and make up labels
C for use in reporting.
        do loopi=1,ITEMSTD
          if(DBTASK(loopi)(1:8).eq.'DBTZNOBS'.or.
     &       DBTASK(loopi)(1:7).eq.'SURTOBS'.or.
     &       DBTASK(loopi)(1:7).eq.'ZNRHOBS'.or.
     &       DBTASK(loopi)(1:7).eq.'ZNHTOBS'.or.
     &       DBTASK(loopi)(1:7).eq.'ZNCLOBS')then
            loopz=iobsdatzone(loopi)  ! the associated zone
            if(loopz.eq.iz)then
              nbdl=nbdl+1
              if(DBTASK(loopi)(1:8).eq.'DBTZNOBS')then
                datalabel(nbdl)='Zone dbT (C)'
              elseif(DBTASK(loopi)(1:7).eq.'SURTOBS')then
                datalabel(nbdl)='Surface T (C)'
              elseif(DBTASK(loopi)(1:7).eq.'ZNRHOBS')then
                datalabel(nbdl)='Zone RH (%)'
              elseif(DBTASK(loopi)(1:7).eq.'ZNHTOBS')then
                datalabel(nbdl)='Zone Heating (kW)'
              elseif(DBTASK(loopi)(1:7).eq.'ZNCLOBS')then
                datalabel(nbdl)='Zone Cooling (kW)'
              endif
            endif
          endif
        enddo

C Loop through each run for the current zone and report the
C suggestion, lower & upper.
        do loop=1,ihowmanycalibroruns
          if(nbofzonetj(loop,iz).gt.0)then
            do loop2=1,nbofzonetj(loop,iz)
              lnap=lnblnk(actphrase(loop,iz,loop2))
              write(outs,'(i2,5a,3F8.4)') loop,' ',
     &          zname(iz)(1:lnzn),' ',
     &          actphrase(loop,iz,loop2)(1:lnap),'  ',
     &          tablejson(loop,iz,loop2,1),
     &          tablejson(loop,iz,loop2,2),
     &          tablejson(loop,iz,loop2,3)
              call edisp(iuout,outs)
              call edisp(34,outs)
            enddo
          endif

C Loop through each of the observations for the zone
C and report the before and after GOF.
          if(nbofzonegf(loop,iz).gt.0)then
            do loop2=1,nbofzonegf(loop,iz)
              write(outs,'(i2,5a,f8.4,a,f8.4)') loop,' ',
     &          zname(iz)(1:lnzn),'  GOfFit for ',
     &          datalabel(loop2)(1:lnblnk(datalabel(loop2))),
     &          ' before',tablegof(loop,iz,loop2,1),' after',
     &          tablegof(loop,iz,loop2,2)
              call edisp(iuout,outs)
              call edisp(34,outs)
            enddo
          endif
        enddo
      enddo

C Now print out the averages.
      call edisp(iuout,
     &  'zone name  parameter         avg suggestion')
      call edisp(34,
     &  'zone name  parameter         avg suggestion')
      loop=1
      denom=float(ihowmanycalibroruns)
      do iz=1,48   ! for each of the possible zones
        nbdl=0
        lnzn=lnblnk(zname(iz))
        do iclear=1,10
          datalabel(iclear)='  '
        enddo

C Find observations associated with this zone and make up labels
C for use in reporting.
        do loopi=1,ITEMSTD
          if(DBTASK(loopi)(1:8).eq.'DBTZNOBS'.or.
     &       DBTASK(loopi)(1:7).eq.'SURTOBS'.or.
     &       DBTASK(loopi)(1:7).eq.'ZNRHOBS'.or.
     &       DBTASK(loopi)(1:7).eq.'ZNHTOBS'.or.
     &       DBTASK(loopi)(1:7).eq.'ZNCLOBS')then
            loopz=iobsdatzone(loopi)  ! the associated zone
            if(loopz.eq.iz)then
              nbdl=nbdl+1
              if(DBTASK(loopi)(1:8).eq.'DBTZNOBS')then
                datalabel(nbdl)='Zone dbT (C)'
              elseif(DBTASK(loopi)(1:7).eq.'SURTOBS')then
                datalabel(nbdl)='Surface T (C)'
              elseif(DBTASK(loopi)(1:7).eq.'ZNRHOBS')then
                datalabel(nbdl)='Zone RH (%)'
              elseif(DBTASK(loopi)(1:7).eq.'ZNHTOBS')then
                datalabel(nbdl)='Zone Heating (kW)'
              elseif(DBTASK(loopi)(1:7).eq.'ZNCLOBS')then
                datalabel(nbdl)='Zone Cooling (kW)'
              endif
            endif
          endif
        enddo
        if(nbofzonetj(loop,iz).gt.0)then
          do loop2=1,nbofzonetj(loop,iz)
            lnap=lnblnk(actphrase(loop,iz,loop2))
            write(outs,'(4a,F7.4)') zname(iz)(1:lnzn),' ',
     &        actphrase(loop,iz,loop2)(1:lnap),' Avg suggestion ',
     &        avgtablejson(iz,loop2)/denom
            call edisp(iuout,outs)
            call edisp(34,outs)
          enddo
        endif
        if(nbofzonegf(loop,iz).gt.0)then
          do loop2=1,nbofzonegf(loop,iz)
            write(outs,'(4a,f8.4,a,f8.4)') 
     &        zname(iz)(1:lnzn),'  Agv GOfFit for ',
     &        datalabel(loop2)(1:lnblnk(datalabel(loop2))),
     &        ' before ',tablegof(loop,iz,loop2,1),' after',
     &        avgtablegof(iz,loop2)/denom
            call edisp(iuout,outs)
            call edisp(34,outs)
          enddo
        endif
      enddo
      CALL ERPFREE(34,ISTAT)
      return
      end  ! of printjsontable

C ******************** applysuggestion ********************
C Applies the 'estimate' based on the implied topic.

      subroutine applysuggestion(ITRC,ITRU,preview,silent,itis,itact,
     &  rv,proceed)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON      

      COMMON/UA31/MATNAM(MNCNG,2)
      character MATNAM*32
      integer nbpastmlc,pastmlc,nbpastmat,pastmat
      COMMON/UA33/nbpastmlc,nbpastmat,pastmlc(10),pastmat(10)

C Ask to overwrite flag.
      COMMON/OVRWT/AUTOVR
      logical AUTOVR

C Parameters.
      logical preview  ! are we looking or doing?
      integer itis  ! index of uncertainty distribution
      real rv       ! the delta to apply
      logical proceed  ! does user want to apply next
      logical silent   ! auto just do it

      logical QUIET
      character outs*148

C If in script mode, do not ask to overwrite files.
      if (MMOD.eq.-6) then
        AUTOVR=.true.
      endif

C Identify what to do.
      if (IDTYPU(itis).eq.1) then  ! if materials focus

C Clear MLC and material lists of what has been processed.
        nbpastmlc=0; nbpastmat=0
        do loop=1,10
          pastmlc(loop)=0
          pastmat(loop)=0
        enddo

        call edisp(iuout,'thermophysical property test...')

C Loop through zones and if associated then preview or edit.
        do IZ=1,NCOMP
          if(preview)then
            call UMATAE01(iz,itis,rv,'p')
          else
            call UMATAE01(iz,itis,rv,'e')
          endif
        enddo
        call edisp(iuout,'thermophysical property test...done.')
      elseif(IDTYPU(itis).eq.2) then  ! if MLC focus
        write(outs,'(3a,i2)')' Composite construction: ',
     &    MATNAM(itis,1)(1:lnblnk(MATNAM(itis,1))),
     &    ' layer ',IDATR(itis,4)
        call edisp(iuout,outs)

C Apply (preview or edit) changes to MLC database.
        if(preview)then
          call UMLCAE02(itis,rv,'p')
        else
          call UMLCAE02(itis,rv,'e')
        endif
    
C Next step is to update each of the zone construction files (if editing).
        if(preview)then
          continue
        else
          call edisp(iuout,'Updating zone construction files...')
          QUIET=.TRUE.
          do IZ=1,NCOMP
            CALL EDCON(ITRC,ITRU,IZ,QUIET,IER)
          enddo
          QUIET=.FALSE.
          call edisp(iuout,
     &      'Updating zone construction files...done.')
        endif
      elseif(IDTYPU(itis).eq.3) then  ! if weather focus
        call edisp(iuout,'weather change not yet done.')
        continue
      elseif(IDTYPU(itis).eq.4) then  ! if operation focus

C For each zone in the model call UCASAE04 to see if it is associated
C with an uncertainty action (itact) and if so preview or edit.
        do IZ=1,NCOMP
          if(preview)then
            call UCASAE04(IZ,itact,rv,'p')
          else
            call UCASAE04(IZ,itact,rv,'e')
          endif
        enddo

      elseif(IDTYPU(itis).eq.5) then  ! if hc coefficients
        call edisp(iuout,'zone hc change not yet done.')
        continue
      elseif(IDTYPU(itis).eq.6) then  ! if optical
        call edisp(iuout,'zone optical change not yet done.')
        continue
      elseif(IDTYPU(itis).eq.101) then  ! if geometry focus
        call edisp(iuout,'zone surf area change not yet done.')
        continue
      elseif(IDTYPU(itis).eq.1001) then  ! if control focus

C Apply (edit) changes to zone controls.
        if(preview)then
          call edisp(iuout,'Preview zone control changes.')
          call UCTLAT01(itis,rv,'p')
        else
          call edisp(iuout,'Updating zone control file.')
          call UCTLAT01(itis,rv,'e')
        endif
      endif

C Check and see if there are other directives in the json file
C if user scanniong a single .json file or if using averages.
      IW=0
      if(silent)then
        proceed=.true.
      else
        CALL EASKAB('Proceed with next json directive?',
     &    '  ','no','check for more directives',IW,nbhelp)
        if(IW.eq.1)then
          proceed=.false.
        elseif(IW.eq.2)then
          proceed=.true.
        endif
      endif

C Reset AUTOVR to ask before overwriting files.
      if (MMOD.eq.-6) then
        AUTOVR=.false.
      endif

      return
      end  ! of applysuggestion
      

C ******************** EGETJSNTAGPHR ********************
C Gets first json tag after position K from the STRING of
C characters and the phrase after :. Strips the leading " and the
C trailing ". Returns k at the trailing phase " Spaces within tag are ok.
C Provides a warning message if ACT='W', a failure message if ACT='F' and does
C no message if ACT='-'.
C Expects a json line similar to "calibration": "cellular",

      SUBROUTINE EGETJSNTAGPHR(STRING,K,JTAG,PHRASE,ACT,MSG,ier)
#include "espriou.h"

      common/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER*(*) JTAG,PHRASE, STRING, MSG
      CHARACTER ACT*1,A*1,loutstr*248,outs*124
      character dq*1,lcurley*1,lsquare*1,simicol*1
      logical unixok

C LS is th maximum length of STRING, L the current position,
C LW the maximum length of PHRASE.
      dq = char(34)  ! double quote
      lcurley = char(123)  ! {
      lsquare = char(91)   ! [
      simicol = char(58)   ! :
      ier=0
      PHRASE=' '; JTAG=' '
      LS=LEN(STRING)
      LW=LEN(PHRASE)
      LJ=LEN(JTAG)
      L=0; M=0

C Start by skipping blanks tabs before the JTAG.
   10 K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9)) GOTO 10
      IF(A.EQ.lcurley) GOTO 10  ! continue past {
      IF(A.EQ.lsquare) GOTO 10  ! continue past [
      if(A.EQ.dq) then
        K=K+1            ! increment to next character
        A=STRING(K:K)
        goto 20          ! proceed to copy
      elseif(A.EQ.dq) then
        continue
      endif

C Copy JTAG from STRING, character by character until ": is found.
   20 L=L+1
      IF(L.GT.LW) return
      if(A.EQ.dq) goto 21          ! now get the phrase
      JTAG(L:L)=A     ! copy character into the phrase
      K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(ICHAR(A).EQ.9.or.A.eq.',') GO TO 100
      if(A.EQ.dq) goto 21          ! now get the phrase
      GOTO 20

C Skip blanks tabs etc before PHRASE.
   21 K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9)) GOTO 21
      if(A.EQ.simicol) then
        K=K+1            ! increment to next character after :
        A=STRING(K:K)
        goto 21          ! try next character
      endif
      if(A.EQ.dq) then
        K=K+1            ! increment to next character
        A=STRING(K:K)
        goto 30          ! now copy into PHRASE
      endif

C Copy PHRASE from STRING, character by character until " is found.
   30 M=M+1
      PHRASE(M:M)=A      ! copy character into the phrase
      K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(ICHAR(A).EQ.9.or.A.eq.',') GO TO 100
      if(A.EQ.dq) goto 100     ! got final "
      goto 30          ! get another character

  100 continue
      RETURN

  999 if(ACT.EQ.'-')then
        RETURN
      elseif(ACT.EQ.'W')then
        ier=1
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' (phrase) in...'
        else
          WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: Past end of line for ',
     &      MSG(1:LNM),' (phrase) in...'
        endif
      elseif(ACT.EQ.'F')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' (phrase) in...'
        else
          WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: Past end of line for ',
     &      MSG(1:LNM),' (phrase) in...'
        endif
      endif
      CALL EDISP248(iuout,loutstr,100)
      if(lnblnk(STRING).gt.123)then
        CALL EDISP248(iuout,STRING,100)
      else
        CALL EDISP(iuout,STRING)
      endif
      GOTO 100

   1  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      LNM=max(1,lnblnk(STRING))
      if(IOS.eq.2)then
        write(outs,*)'EGETJSNTAGPHR: permission error writing warning: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      else
        write(outs,*)'EGETJSNTAGPHR: error writing warning: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      endif
      return

      END
 
C ******************** EGETJSNTAGR ********************
C Gets first json tag after position K from the STRING of
C characters and the real number after :. Strips the leading " and the
C trailing ". Returns k at the trailing phase " Spaces within tag are ok.
C Provides a warning message if ACT='W', a failure message if ACT='F' and does
C no message if ACT='-'. Expects a json line similar to "ESTIMATE": 0.0345".

      SUBROUTINE EGETJSNTAGR(STRING,K,JTAG,RV,RMN,RMX,RACT,MSG,ier)
#include "espriou.h"

      common/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER*(*) JTAG,STRING, MSG
      CHARACTER RACT*1,A*1,loutstr*248,outs*124
      CHARACTER STR1*16,STR2*16
      character dq*1,lcurley*1,lsquare*1,simicol*1
      character PHRASE*42
      logical unixok

C LS is th maximum length of STRING, L the current position,
C LW the maximum length of PHRASE.
      dq = char(34)  ! double quote
      lcurley = char(123)  ! {
      lsquare = char(91)   ! [
      simicol = char(58)   ! :
      ier=0
      PHRASE='  '; JTAG='  '
      LS=LEN(STRING)
      ils=lnblnk(STRING)
      LW=LEN(PHRASE)
      LJ=LEN(JTAG)
      L=0; M=0

C Start by skipping blanks tabs before the JTAG.
   10 K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9)) GOTO 10
      IF(A.EQ.lcurley) GOTO 10  ! continue past {
      IF(A.EQ.lsquare) GOTO 10  ! continue past [
      if(A.EQ.dq) then
        K=K+1            ! increment to next character
        A=STRING(K:K)
        goto 20          ! proceed to copy
      elseif(A.EQ.dq) then
        continue
      endif

C Copy JTAG from STRING, character by character until ": is found.
   20 L=L+1
      IF(L.GT.LW) return
      if(A.EQ.dq) goto 21          ! now get the phrase
      JTAG(L:L)=A     ! copy character into the phrase
      K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(ICHAR(A).EQ.9.or.A.eq.',') GO TO 100
      if(A.EQ.dq) goto 21          ! now get the phrase
      GOTO 20

C Skip blanks tabs etc before PHRASE. We do not expect any "
   21 K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9)) GOTO 21
      if(A.EQ.simicol) then
        K=K+1            ! increment to next character after :
        A=STRING(K:K)
        goto 21          ! try next character
      endif

C Copy PHRASE from STRING, character by character until a ',' is found.
   30 M=M+1
      PHRASE(M:M)=A      ! copy character into the phrase
      K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(ICHAR(A).EQ.9.or.A.eq.',') GO TO 100
      goto 30          ! get another character

  100 continue

C Convert phrase into a real and do range checks.
      read(phrase,*,ERR=1002)rv

C If range checking disabled, jump out of routine.
      IF(RACT.EQ.'-')RETURN

C Make up reporting string.
      CALL REL16STR(RV,STR1,IW1,IER)

C Check RV against minimum and respond based on RACT.
      IF(RV.LT.RMN)THEN
        CALL REL16STR(RMN,STR2,IW2,IER)
        IF(RACT.EQ.'W')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(LOUTSTR,'(4a)',IOSTAT=IOS,ERR=1)
     &        'WARNING: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(LOUTSTR,'(a,a)',IOSTAT=IOS,ERR=1)' WARNING in: ',
     &        STRING(1:ils)
          endif
          call edisp248(iuout,LOUTSTR,100)
          write(LOUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &      ' value (',STR1(1:IW1),') < normal minimum ',STR2(1:IW2),'!'
          call edisp248(iuout,LOUTSTR,100)
        ELSEIF(RACT.EQ.'F')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(LOUTSTR,'(4a)',IOSTAT=IOS,ERR=1)
     &        'FAILURE: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(LOUTSTR,'(a,a)',IOSTAT=IOS,ERR=1)' FAILURE in: ',
     &      STRING(1:ils)
          endif
          call edisp248(iuout,LOUTSTR,100)
          write(LOUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') < allowable minumum ',
     &       STR2(1:IW2),'!'
          call edisp248(iuout,loutstr,100)
          IER=1
          RETURN
        ENDIF
      ELSEIF(RV.GT.RMX)THEN
        CALL REL16STR(RMX,STR2,IW2,IER)
        IF(RACT.EQ.'W')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(LOUTSTR,'(4a)',IOSTAT=IOS,ERR=1)
     &        'WARNING: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(LOUTSTR,'(a,a)',IOSTAT=IOS,ERR=1)' WARNING in: ',
     &      STRING(1:ils)
          endif
          call edisp248(iuout,LOUTSTR,100)
          write(LOUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') > normal  maximum ',
     &       STR2(1:IW2),'!'
          call edisp248(iuout,LOUTSTR,100)
        ELSEIF(RACT.EQ.'F')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(LOUTSTR,'(4a)',IOSTAT=IOS,ERR=1)
     &        'FAILURE: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(LOUTSTR,'(a,a)',IOSTAT=IOS,ERR=1)' FAILURE in: ',
     &      STRING(1:ils)
          endif
          call edisp248(iuout,LOUTSTR,100)
          write(LOUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') > allowable maximum ',
     &       STR2(1:IW2),'!'
          call edisp248(iuout,loutstr,100)
          IER=1
          RETURN
        ENDIF
      ENDIF

      RETURN

  999 if(RACT.EQ.'-')then
        RETURN
      elseif(RACT.EQ.'W')then
        ier=1
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' (phrase) in...'
        else
          WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: Past end of line for ',
     &      MSG(1:LNM),' (phrase) in...'
        endif
      elseif(RACT.EQ.'F')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' (phrase) in...'
        else
          WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: Past end of line for ',
     &      MSG(1:LNM),' (phrase) in...'
        endif
      endif
      CALL EDISP248(iuout,loutstr,100)
      if(lnblnk(STRING).gt.123)then
        CALL EDISP248(iuout,STRING,100)
      else
        CALL EDISP(iuout,STRING)
      endif
      GOTO 100

   1  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      LNM=max(1,lnblnk(STRING))
      if(IOS.eq.2)then
        write(outs,*)'EGETJSNTAGR: permission error writing warning: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      else
        write(outs,*)'EGETJSNTAGR: error writing warning: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      endif
      return

 1002 if(currentfile(1:2).ne.'  ')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        WRITE(LOUTSTR,'(6a)',IOSTAT=IOS,ERR=1) 'Failed in ',
     &    currentfile(1:LN),': conversion of ',
     &    MSG(1:LNM),' value in ',phrase
      else
        LNM=max(1,lnblnk(MSG))
        WRITE(LOUTSTR,1003,IOSTAT=IOS,ERR=1)MSG(1:LNM),phrase
 1003   FORMAT(' Failed: conversion of ',A,' value in ',A)
      endif
      CALL EDISP248(iuout,LOUTSTR,100)
      IER=2
      GOTO 100

      END  ! of EGETJSNTAGR

C ******************** splitstratchar ********************
C Splits a string at a specific character returning 2 str.
C example if char is a dot it splits spandrel_ins_thick.all_insul_fram
C into spandrel_ins_thick and all_insul_fram.

      SUBROUTINE splitstratchar(STRING,CH,FIRST,SECOND,ier)


      common/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER*(*) STRING,FIRST,SECOND
      CHARACTER CH*1
      CHARACTER A*1,loutstr*248,outs*124
      integer K

C LST is th maximum length of STRING, L the current position,
C LF,LS  the maximum length of FIRST & SECOND.
      ier=0
      FIRST='  '; SECOND='  '
      LST=LEN(STRING)
      LNLST=lnblnk(STRING)
      LF=LEN(FIRST)
      LS=LEN(SECOND)
      L=0; M=0; K=0

C Start by skipping blanks tabs before FIRST.
   10 K=K+1
      IF(K.GT.LST) GOTO 999   ! past defined length
      IF(K.GT.LNLST) GOTO 100 ! past actual length
      A=STRING(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9)) GOTO 10
      A=STRING(K:K)
      goto 20          ! proceed to copy

C Copy FIRST from STRING, character by character until CH is found.
   20 L=L+1
      IF(L.GT.LST) return
      IF(L.GT.LF) goto 21
      FIRST(L:L)=A     ! copy character into the 1st phrase
      K=K+1
      IF(K.GT.LST) GOTO 999   ! past defined length
      IF(K.GT.LNLST) GOTO 100 ! past actual length
      A=STRING(K:K)
      IF(ICHAR(A).EQ.9.or.A.eq.',') GO TO 100
      if(A.EQ.CH) goto 21          ! now get the 2nd phrase
      GOTO 20

C Skip blanks tabs etc before SECOND.
   21 K=K+1
      IF(K.GT.LST) GOTO 999
      IF(K.GT.LNLST) GOTO 100 ! past actual length
      A=STRING(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9)) GOTO 21
      goto 30          ! now copy into SECOND

C Copy PHRASE from STRING, character by character until " is found.
   30 M=M+1
      if(M.gt.LS) goto 100 ! past size of 2nd string
      SECOND(M:M)=A      ! copy character into the 2nd phrase
      K=K+1
      IF(K.GT.LST) GOTO 100
      IF(K.GT.LNLST) GOTO 100 ! past actual length
      A=STRING(K:K)
      IF(ICHAR(A).EQ.9.or.A.eq.',') GO TO 100
      goto 30          ! get another character

  100 continue
      RETURN

  999 CALL EDISP248(iuout,loutstr,100)
      if(lnblnk(STRING).gt.123)then
        CALL EDISP248(iuout,STRING,100)
      else
        CALL EDISP(iuout,STRING)
      endif
      GOTO 100

      END
 
C Variants of code in sensit.F and sentimn.F to adapt model files.
C   UMLCAE02   Edits data: thickness in MLC's.
C        call UAE02(IZONE,IACT,DIR)  ! MLC layer thickness

C ******************** UMLCAE02 ********************
C Edits layer thicknesses in MLC db.
C Naming convention:
C  IACT: index of uncertainty what and where
C  DIR: change directive...
C  ACT: 'p' preview 'e' edit

      SUBROUTINE UMLCAE02(IACT,DIR,ACT)
#include "building.h"
#include "uncertainty.h"
#include "esprdbfile.h"
#include "material.h"

C Parameters.
      integer IACT
      real DIR
      character ACT*1      

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      COMMON/UA31/MATNAM(MNCNG,2)

      CHARACTER MATNAM*32

      character uhphrase*42,outs*124

C Data stored in UA02 has the following meaning in this routine:
C IDATR(?,1): The material in the chosen MLC, if this is 0 then 
C             a new MLC reference will be in MATNAM(?,2).
C IDATR(?,2): Unused.
C IDATR(?,3): Change type (%; +/-; explicit).
C Define variables for current location and change.
      ICL=IACTD(IACT,2)
      ICC=IACTD(IACT,1)

C If the common block MLC has not yet been filled then read in the
C composite construction database.
      IF(.NOT.MLDBOK)THEN
        CALL ERMLDB(0,iuout,IER)
        IF(IER.eq.4)THEN
          CALL ERMLDB2(0,iuout,IER)
          if(IER.eq.0)then
            MLDBOK=.TRUE.
          endif
        ELSEIF(IER.EQ.1.or.IER.eq.2.or.IER.eq.3)THEN
          CALL USRMSG(' ',' Unable to display selections ','W')
          IER=IER+1
          RETURN
        ELSE
          MLDBOK=.TRUE.
        ENDIF
      ENDIF
      if(IER.ne.0)then
        call usrmsg('Problem scanning MLC db.','Returning.','W')
        return
      endif

C This construction has uncertainties defined, but which MLC database 
C entry is it? Find matching MLC.
      call matchmlcdesc(MATNAM(ICC,1),imlc)
      if(imlc.eq.0)then
        call usrmsg('Problem finding MLC in db.','Returning.','W')
        return
      endif

C The layer of the MLC is found in IDATR(?,4)
      IE=IDATR(icc,4)

C Make specific header string for each change (to match urunvals)
      lncng=lnblnk(LCNG(ICC))
      lnloc=lnblnk(LLOC(ICL))
      uhphrase=' '
      write(uhphrase,'(3a)')LCNG(ICC)(1:lncng),':',LLOC(ICL)(1:lnloc)
      luhphrase=lnblnk(uhphrase)
      if(act.eq.'p'.or.act.eq.'P')then
       write(outs,'(2a)')'UMLCAE02 preview of ',uhphrase(1:luhphrase)
      else
       write(outs,'(2a)')'UMLCAE02 Implementing ',uhphrase(1:luhphrase)
      endif
      call edisp(iuout,outs)
      write (outs,'(a,f9.4)') 'DTHK: ',DTHK(imlc,ie)
      call edisp(iuout,outs)

C Either edit layer thickness.
      if (IDATR(ICC,3).eq.1) then

C Percentage change. Ensure it does not go below 1mm. 
        write(outs,'(a,2f9.4,a,f9.4)') 'DATU DIR % factor: ',
     &    DATU(ICC,1),DIR,' Suggested: ',
     &    DTHK(imlc,ie)*(((DATU(ICC,1)/100.)*DIR)+1.0)
        call edisp(iuout,outs)
        if(act.eq.'p'.or.act.eq.'P')then
          continue
        else
          DTHK(imlc,ie)=DTHK(imlc,ie)*(((DATU(ICC,1)/100.)*DIR)+1.0)
          if(DTHK(imlc,ie).lt.0.001)DTHK(imlc,ie)=0.001
        endif
      elseif (IDATR(ICC,3).eq.2) then

C Absolute change. Ensure it does not go below 1mm. 
        write (outs,'(a,2f9.4,a,f9.4)') 'DATU DIR abs factor: ',
     &    DATU(ICC,1),DIR,' Suggested:',DTHK(imlc,ie)+(DATU(ICC,1)*DIR)
        call edisp(iuout,outs)
        if(act.eq.'p'.or.act.eq.'P')then
          continue
        else
          DTHK(imlc,ie)=DTHK(imlc,ie)+(DATU(ICC,1)*DIR)
          if(DTHK(imlc,ie).lt.0.001)DTHK(imlc,ie)=0.001
        endif
      elseif (IDATR(ICC,3).eq.3) then

C Explicit change. Ensure it does not go below 1mm.
        if (DIR.gt.0.) then
          write (outs,'(a,2f9.4,a,f9.4)') 'DATU DIR expl factor: ',
     &    DATU(ICC,1),DIR,' Suggested: ',
     &    DTHK(imlc,ie)+(DATU(ICC,1)-DTHK(imlc,ie))*DIR
        else
          write (outs,'(a,2f9.4,a,f9.4)') 'DATU DIR expl factor: ',
     &    DATU(ICC,1),DIR,' Suggested: ',
     &    DTHK(imlc,ie)-(DATU(ICC,1)-DTHK(imlc,ie))*DIR
        endif
        call edisp(iuout,outs)
        if(act.eq.'p'.or.act.eq.'P')then
          continue
        else
          if (DIR.gt.0.) then
            DTHK(imlc,ie)=DTHK(imlc,ie)+(DATU(ICC,1)-DTHK(imlc,ie))*DIR
          else
            DTHK(imlc,ie)=DTHK(imlc,ie)-(DATU(ICC,2)-DTHK(imlc,ie))*DIR
            if(DTHK(imlc,ie).lt.0.001)DTHK(imlc,ie)=0.001
          endif
        endif
      endif
      if(act.eq.'p'.or.act.eq.'P')then
        continue
      else
        write (outs,'(a)') 'to:'
        call edisp(iuout,outs)
        write (outs,'(a,f9.4)') 'DTHK: ',DTHK(imlc,ie)
        call edisp(iuout,outs)

C Now update the MLC database.
        call edisp(iuout,'Updating the MLC database...')
        CALL EMKAMLD2(iuout,IER) ! write it out
      endif
      RETURN
      END

C ******************** UCASAE04 ********************

C Edits/previews casual gains and scheduled air movement in zones.

      SUBROUTINE UCASAE04(IZONE,IACT,DIR,ACT)
      include "building.h"
      include "model.h"
      include "schedule.h"
      include "uncertainty.h"

C Parameters.
      integer IZONE,IACT
      real DIR
      character ACT*1 
     
      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32

      character uhphrase*42,outs*124
      logical XST

C Data stored in UA04 has the following meaning in this routine:
C IDATR(?,1): Casual gain type(1/2/3)/ scheduled air flow(0)
C IDATR(?,2): Cas Gn: sensible(1)/ latent(2)/ radiant(3)/ convective(4)/
C IDATR(?,2):         total Q (same split) (5)/ Period start (6)/
C IDATR(?,2):         N/A (7)/ 
C IDATR(?,2):         same total frac diff split (8)
C IDATR(?,2): Sched Air flow: infiltration(1)/ ventilation(2)/
C IDATR(?,2):         N/A (3)/ vent source temp(4)
C IDATR(?,3): Change type (%; +/-; explicit).
C Define variables for current location and change.
      ICL=IACTD(IACT,2)
      ICC=IACTD(IACT,1)

C Loop through all zones listed in NZNOGU for the defined location ICL.
      do 10 II=1,NZGU(ICL)
        IZ=NZNOGU(ICL,II)
        if (IZ.eq.IZONE) then

C Re-read the zone operation file.
          ITRC=0
          ITRU=0
          IUO=IFIL+1
          INQUIRE (FILE=LPROJ(IZONE),EXIST=XST)
          IF(XST)THEN
            call edisp(iuout,'Scanning zone operation file...')
            CALL ERPFREE(IUO,ISTAT)
            CALL EROPER(ITRC,ITRU,IUO,IZONE,IER)
            call edisp(iuout,'Scanning zone operation file...done.')
          ELSE
            call usrmsg('Operation file not found.','returning','W')
            return
          ENDIF


C Define max number of periods for common P2 (flow) and P3 (Cas gns).
          IMP2=NAC(1)
          IMP3=NCAS(1)
          DO IDTY=1,NBDAYTYPE
            IF(NAC(IDTY).GT.IMP2)IMP2=NAC(IDTY)
            if (NCAS(IDTY).gt.IMP3) IMP3=NCAS(IDTY)
          ENDDO

C Edit or preview casual gains.
          if (IDATR(ICC,1).eq.0) then

C Edit scheduled air flows.
C Currently changing all defined periods.
            ICF=IDATR(ICC,3)
            ICD=IDATR(ICC,4)  ! day type
            ICP=IDATR(ICC,5)  ! period in day
            DAT1=DATU(ICC,1)
            DAT2=DATU(ICC,2)
            if(act.eq.'p'.or.act.eq.'P')then
             write (outs,*) 'Preview scheduled air flow ',ICF,DAT1,DAT2
            else
             write (outs,*) 'Editing scheduled air flow ',ICF,DAT1,DAT2
            endif
            call edisp(iuout,outs)

C Make specific header string for each change (to match urunvals)
            lncng=lnblnk(LCNG(ICC))
            lnloc=lnblnk(LLOC(ICL))
            if(lncng.gt.18)lncng=18
            uhphrase=' '
            write(uhphrase,'(4a,i2.2,a)')LCNG(ICC)(1:lncng),':',
     &        LLOC(ICL)(1:lnloc),':',IZ,','
            luhphrase=lnblnk(uhphrase)
            if(act.eq.'p'.or.act.eq.'P')then
              write(outs,'(2a)')'UCASAE04 Previewing ',
     &          uhphrase(1:luhphrase)
            else
              write(outs,'(2a)')'UCASAE04 Implementing ',
     &          uhphrase(1:luhphrase)
            endif
            call edisp(iuout,outs)

C Percentage change. 
C What data type to change? (see comments at start of routine).
            if (IDATR(ICC,2).eq.1) then
              write (outs,*) 'Zone:',IZONE,' infiltration DIR is ',DIR
              call edisp(iuout,outs)
              T=ACI(ICD,ICP); T1=ACI(ICD,ICP)
              call UAEDIT(T1,ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
              write(outs,*)'Period ',CALENTAG(ICD),ICP,' From:',T,
     &          ' to ',T1
              call edisp(iuout,outs)
              if(act.eq.'p'.or.act.eq.'P')then
                continue
              else
                 ACI(ICD,ICP)=T1
              endif
            elseif (IDATR(ICC,2).eq.2) then
              write (outs,*) 'Zone:',IZONE,' ventilation DIR is ',DIR
              call edisp(iuout,outs)
              T=ACV(ICD,ICP); T1=ACV(ICD,ICP)
              call UAEDIT(T1,ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
              write(outs,*)'Period ',CALENTAG(ICD),ICP,' From:',T,
     &          ' to ',T1
              call edisp(iuout,outs)
              if(act.eq.'p'.or.act.eq.'P')then
                continue
              else
                ACV(ICD,ICP)=T1
              endif
            elseif (IDATR(ICC,2).eq.3) then
              write (6,*)'Illegal option, cannot change zone index'
            elseif (IDATR(ICC,2).eq.4) then
              write (outs,*) 'Zone:',IZONE,' vent source temperature',
     &          ' DIR is ',DIR
              call edisp(iuout,outs)
              T=TA(ICD,ICP); T1=TA(ICD,ICP)
              call UAEDIT(T1,ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
              write(outs,*)'Period ',CALENTAG(ICD),ICP,' From:',T,
     &          ' to ',T1
              call edisp(iuout,outs)
              if(act.eq.'p'.or.act.eq.'P')then
                continue
              else
                TA(ICD,ICP)=T1
              endif
            endif

C Write zone operations file. If older than 21 upgrade.
            if(act.eq.'p'.or.act.eq.'P')then
              continue
            else
              call edisp(iuout,'Updating zone operation file...')
              if(ip3ver(IZONE).lt.21) ip3ver(IZONE)=21
              CALL EMKOPER(IUO,LPROJ(IZONE),IZONE,IER)
            endif
          else

C Edit casual gains.
C Check that we have the right element(period).
            ICF=IDATR(ICC,3)
            ICD=IDATR(ICC,4)  ! day type
            ICP=IDATR(ICC,5)  ! period in day
            DAT1=DATU(ICC,1)
            DAT2=DATU(ICC,2)
            IT=IDATR(ICC,1)

C Make specific header string for each change (to match urunvals)
            lncng=lnblnk(LCNG(ICC))
            lnloc=lnblnk(LLOC(ICL))
            if(lncng.gt.18)lncng=18
            uhphrase=' '
            write(uhphrase,'(4a,i2.2,a)')LCNG(ICC)(1:lncng),':',
     &        LLOC(ICL)(1:lnloc),':',IZ,','
            luhphrase=lnblnk(uhphrase)
            if(act.eq.'p'.or.act.eq.'P')then
              write(outs,'(2a)') 'Previewing ',uhphrase(1:luhphrase)
            else
              write(outs,'(2a)') 'Implementing ',uhphrase(1:luhphrase)
            endif
            call edisp(iuout,outs)
      
C What data type to change? (see comments at start of routine).

C The next section has the following format:
C  Check casual gain parameter to edit (sensible, latent, convect %...
C  Check if required casual gain type is defined for this period (occ, lights..
C    note weekdays/ sat/ sun checked seperately.
            if (IDATR(ICC,2).eq.1) then
              write (outs,'(a,i3,a,i2,a,f7.3)') 'Zone:',IZONE,
     &          ' type:',IT,' sensible direction: ',DIR
              call edisp(iuout,outs)

C Check matching period & casual gain type for daytype ICD.
              if (IDATR(ICC,1).eq.iabs(ICGT(ICD,ICP))) then
                T=CMGS(ICD,ICP); T1=CMGS(ICD,ICP)
                call UAEDIT(T1,ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                write (outs,*)'Period ',CALENTAG(ICD),ICP,
     &            ' From:',T,' to ',T1
                call edisp(iuout,outs)
                if(act.eq.'p'.or.act.eq.'P')then
                  continue
                else
                  CMGS(ICD,ICP)=T1
                endif
              endif
            elseif (IDATR(ICC,2).eq.2) then
              write (outs,'(a,i3,a,i2,a,f7.3)') 'Zone:',IZONE,
     &          ' type:',IT,' latent direction: ',DIR
              call edisp(iuout,outs)
              if (IDATR(ICC,1).eq.iabs(ICGT(ICD,ICP))) then
                T=CMGL(ICD,ICP); T1=CMGL(ICD,ICP)
                call UAEDIT(T1,ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                write (outs,*)'Period ',CALENTAG(ICD),ICP,
     &            ' From:',T,' to ',T1
                call edisp(iuout,outs)
                if(act.eq.'p'.or.act.eq.'P')then
                  continue
                else
                  CMGL(ICD,ICP)=T1
                endif
              endif
            elseif (IDATR(ICC,2).eq.3) then
              write (outs,'(a,i3,a,i2,a,f7.3)') 'Zone:',IZONE,
     &          ' type:',IT,' rad fraction direction ',DIR
              call edisp(iuout,outs)
              if (IDATR(ICC,1).eq.iabs(ICGT(ICD,ICP))) then
                T=RADC(ICD,ICP); T1=RADC(ICD,ICP)
                call UAEDIT(T1,ICF,DAT1,DAT2,DIR,0,0.0,1.0)
                write (outs,*)'Period ',CALENTAG(ICD),ICP,
     &            ' From:',T,' to ',T1
                call edisp(iuout,outs)
                if(act.eq.'p'.or.act.eq.'P')then
                  continue
                else
                  RADC(ICD,ICP)=T1
                  CONC(ICD,ICP)=1.0-T1  ! adjust CONC
                endif
              endif
            elseif (IDATR(ICC,2).eq.4) then
              write (outs,'(a,i3,a,i2,a,f7.3)')'Zone:',IZONE,
     &          ' type:',IT,' conv fraction direction ',DIR
              call edisp(iuout,outs)
              if (IDATR(ICC,1).eq.iabs(ICGT(ICD,ICP))) then
                T=CONC(ICD,ICP); T1=CONC(ICD,ICP)
                call UAEDIT(T1,ICF,DAT1,DAT2,DIR,0,0.0,1.0)
                write (outs,*)'Period ',CALENTAG(ICD),ICP,
     &            ' From:',T,' to ',T1
                call edisp(iuout,outs)
                if(act.eq.'p'.or.act.eq.'P')then
                  continue
                else
                  CONC(ICD,ICP)=T1
                  RADC(ICD,ICP)=1.0-T1  ! adjust RADC
                endif
              endif
            elseif (IDATR(ICC,2).eq.5) then
              write (outs,'(a,i3,a,i2,a,f7.3)') 'Zone:',IZONE,
     &          ' type:',IT,' total Q direction ',DIR
              call edisp(iuout,outs)
              if (IDATR(ICC,1).eq.iabs(ICGT(ICD,ICP))) then
                T1=CMGS(ICD,ICP)
                T2=CMGL(ICD,ICP)
                T3=T1+T2
                call UAEDIT(T3,ICF,DAT1,DAT2,DIR,-1,0.0,0.0)

                T1S=T3*(CMGS(ICD,ICP)/(T1+T2))
                T2L=T3*(CMGL(ICD,ICP)/(T1+T2))
                write (outs,*)'Period ',CALENTAG(ICD),ICP,
     &            ' Sensible from:',T1,' to ',T1S
                call edisp(iuout,outs)
                write (outs,*)'Period ',CALENTAG(ICD),ICP,
     &            ' Latent from:',T2,' to ',T2L
                call edisp(iuout,outs)
                if(act.eq.'p'.or.act.eq.'P')then
                  continue
                else
                  CMGS(ICD,ICP)=T3*(T1S/(T1+T2))
                  CMGL(ICD,ICP)=T3*(T2L/(T1+T2))
                endif
              endif
            elseif (IDATR(ICC,2).eq.6) then

C Shift the start time of subsequent periods.
              write (outs,'(a,i3,a,i2,a,f7.3)') 'Zone:',IZONE,
     &          ' type:',IT,' start hour direction ',DIR
              call edisp(iuout,outs)
              if (IDATR(ICC,1).eq.iabs(ICGT(ICD,ICP))) then
                IT=ICGS(ICD,ICP); IT1=ICGS(ICD,ICP)
                if(DAT1.gt.0.0.and.DAT1.le.1.0) IV=1  ! shift 1 hour
                if(DAT1.gt.1.0.and.DAT1.le.2.0) IV=2  ! shift 2 hours
                if (DIR.gt.0.) then
                  if(IV.eq.1)then
                    if(ICGS(ICD,ICP)+1.lt.ICGS(IDTY,ICP+1))then
                      IT1=ICGS(ICD,ICP)+1      ! if clear to shift later
                    endif
                  elseif(IV.eq.2)then
                    if(ICGS(ICD,ICP)+2.lt.ICGS(IDTY,ICP+2))then
                      IT1=ICGS(ICD,ICP)+2      ! if clear to shift later
                    else
                      IT1=ICGS(ICD,ICP)+1      ! if clear to shift later
                    endif
                  endif
                else
                  if(IV.eq.1)then
                    if(ICGS(ICD,ICP)-1.gt.ICGS(ICD,ICP-1))then
                      IT1=ICGS(ICD,ICP)-1      ! if clear to shift earlier
                    endif
                  elseif(IV.eq.2)then
                    if(ICGS(ICD,ICP)-2.gt.ICGS(ICD,ICP-2))then
                      IT1=ICGS(ICD,ICP)-2      ! if clear to shift earlier
                    else
                      IT1=ICGS(ICD,ICP)-1      ! if clear to shift earlier
                    endif
                  endif
                endif
                write (outs,*)'Period ',CALENTAG(ICD),ICP,
     &            ' From:',IT,' to ',IT1
                call edisp(iuout,outs)
                if(act.eq.'p'.or.act.eq.'P')then
                  continue
                else
                  if (DIR.gt.0.) then
                    if(ICGS(ICD,ICP)+1.lt.ICGS(ICD,ICP+1))then
                      ICGS(ICD,ICP)=ICGS(ICD,ICP)+1      ! if clear to shift later
                      ICGF(ICD,ICP-1)=ICGF(ICD,ICP-1)+1  ! and shift prior fn
                    endif
                  else
                    if(ICGS(ICD,ICP)-1.gt.ICGS(ICD,ICP-1))then
                      ICGS(ICD,ICP)=ICGS(ICD,ICP)-1      ! if clear to shift earlier
                      ICGF(ICD,ICP-1)=ICGF(ICD,ICP-1)-1  ! and shift prior fn
                    endif
                  endif
                endif
              endif
            elseif (IDATR(ICC,2).eq.8) then
              IT=IDATR(ICC,1)
              write(outs,'(a,i3,a,i2,a,f7.3)')'Zone:',IZONE,
     &          ' type:',IT,' rad/conv split direction ',DIR
              call edisp(iuout,outs)
              if (IDATR(ICC,1).eq.iabs(ICGT(ICD,ICP))) then
                T1=RADC(ICD,ICP)
                T2=CONC(ICD,ICP); T2C=CONC(ICD,ICP)
                T3=T1+T2
                call UAEDIT(T2C,ICF,DAT1,DAT2,DIR,0,0.0,T3)
                T1R=T3-T2C
                write (outs,*)'Period ',CALENTAG(ICD),ICP,
     &            ' Rad fraction from:',T1,' to ',T1R
                call edisp(iuout,outs)
                write (outs,*)'Period ',CALENTAG(ICD),ICP,
     &            ' Conv fraction from:',T2,' to ',T2C
                call edisp(iuout,outs)
                if(act.eq.'p'.or.act.eq.'P')then
                  continue
                else
                  RADC(ICD,ICP)=T3-T2C
                  CONC(ICD,ICP)=T2C
                endif
              endif
            else
              write (6,*)'Editing request unknown.'
            endif

C Write zone operations file.
            if(act.eq.'p'.or.act.eq.'P')then
              continue
            else
              call edisp(iuout,'Updating zone operation file...')
              if(ip3ver(IZONE).lt.21) ip3ver(IZONE)=21
              CALL EMKOPER(IUO,LPROJ(IZONE),IZONE,IER)
            endif
          endif  ! of editing casual gains
        endif    ! of matching zone
 10   continue

      RETURN
      END  ! of UCASAE04

C ******************** UAEDIT ********************
C Edits the given data item (VAL) depending on change flag (ICFLAG)
C and given direction or magnitude (DIR).
C ICHK: -1 check min val, +1 check max val, 0 check both vals

      SUBROUTINE UAEDIT (VAL,ICFLAG,UPval,DOWNval,DIR,ichk,vmin,vmax)
#include "building.h"

      if (ICFLAG.eq.1) then

C Percentage change. Logic works as long as UPval is less than or equal
C to 100%.  Over 100% the logic falls apart.
        VAL=VAL*(((UPval/100.)*DIR)+1.0)
      elseif (ICFLAG.eq.2) then

C Absolute change. 
        VAL=VAL+(UPval*DIR)
      elseif (ICFLAG.eq.3) then

C Explicit change - take into account if a normal
C distribution (scaled to +-1) or a uniform distribution (+-1). 
        if (DIR.gt.0.) then
          VAL=VAL+(UPval*DIR)
        else
          VAL=VAL+(DOWNval*DIR)
        endif
      endif
      
C Check limits.
      if (ICHK.le.0) then
        VAL=max(VAL,vmin)
      endif
      if (ICHK.ge.0) then
        VAL=min(VAL,vmax)
      endif
      write(6,*) 'Updated VAL after limit checks ',VAL
      RETURN
      END  ! of UAEDIT

C ******************** UMATAE01 ********************
C Update model databases and zone geometry files to reflect
C changes in thermophysical properties as done in UAE01.
C UMATAE01 updates conductivity, density or specific heat values in 
C model databases and zone files.
C Naming convention:
C IZONE, ISURF: focus zone and surface (the data files for this zone 
C               have just been rescaned). 
C IZ, IS: current zone and surface being checked (these are listed in 
C               the common/ua2/ arrays).
C      if (IDTYPU(IACTD(IACT,1)).eq.1) then 
C        call UAE01(IZONE,IACT,DIR)  ! thermophysical properties

      subroutine UMATAE01(IZONE,IACT,DIR,ACT)  ! thermophysical properties
      use CFC_Module , ONLY: ITMCFCDB
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "uncertainty.h"
#include "esprdbfile.h"
#include "material.h"

C Parameters.
      integer IZONE,IACT
      real DIR
      character ACT*1 

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout
      common/FILEP/IFIL
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      integer nbpastmlc,pastmlc,nbpastmat,pastmat
      COMMON/UA33/nbpastmlc,nbpastmat,pastmlc(10),pastmat(10)

      CHARACTER T32*32,lltmp*144,lworking*144,outs*124

      character uhphrase*42,fs*1
      integer loop
      integer iel    ! array of the position of the matched material

C To remember materials and MLC that have been processed.
      DIMENSION IEL(ME)
      logical isadupmat,isadupmlc,unixok

      isadupmat=.false.; isadupmlc=.false.  ! not a duplicate
      
C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Data stored in UA01 has the following meaning in this routine:
C IDATR(?,1): Materials array index.
C IDATR(?,2): Thermophysical property (con; den; sp ht).
C IDATR(?,3): Change type (%; +/-; explicit).
      ICL=IACTD(IACT,2)
      ICC=IACTD(IACT,1)

C If the common block MLC has not yet been filled then read in the
C composite construction database (which also checks the materials
C database.
      IF(.NOT.MLDBOK)THEN
        CALL ERMLDB(0,iuout,IER)
        IF(IER.eq.4)THEN
          CALL ERMLDB2(0,iuout,IER)
          if(IER.eq.0)then
            MLDBOK=.TRUE.
          endif
        ELSEIF(IER.EQ.1.or.IER.eq.2.or.IER.eq.3)THEN
          CALL USRMSG(' ',' Unable to display selections ','W')
          IER=IER+1
          RETURN
        ELSE
          MLDBOK=.TRUE.
        ENDIF
      ENDIF
      if(IER.ne.0)then
        call usrmsg('Problem scanning MLC db.','Returning.','W')
        return
      endif

C Make specific header string for each change (to match urunvals)
      lncng=lnblnk(LCNG(ICC))
      lnloc=lnblnk(LLOC(ICL))
      uhphrase=' '
      write(uhphrase,'(3a)')LCNG(ICC)(1:lncng),':',LLOC(ICL)(1:lnloc)
      luhphrase=lnblnk(uhphrase)
      if(act.eq.'p'.or.act.eq.'P')then
       write(outs,'(2a)') 'UMATAE01 Previewing ',uhphrase(1:luhphrase)
      else
       write(outs,'(2a)') 'UMATAE01 Implementing ',uhphrase(1:luhphrase)
      endif
      call edisp(iuout,outs)

C Loop through all zones listed in NZNOGU for the defined location ICL.
      do 10 II=1,NZGU(ICL)
       IZ=NZNOGU(ICL,II)
       if (IZ.eq.IZONE) then

C Refresh the zone geometry common blocks.
C Identify uncertain material in the required surfaces.
         call georead(IFIL+1,LGEOM(IZ),IZ,1,iuout,IER)
         do 20 JJ=1,NSGU(ICL,II)
           IS=NSNOG(ICL,II,JJ)
           ioc=IZSTOCN(iz,is)

C Check to see if material is used in current construction.
C IDATR is the material array, use matlegindex to find the
C raw IPR within the materials database in order to pass
C to chkprim (which returns the layers which are made of 
C the required material (if any).
           ip=matlegindex(IDATR(ICC,1))
C           write(6,*) 'IDATR matlegindex',IDATR(ICC,1),ip
           call CHKPRIM(IZ,IS,IP,NEL,IEL,IMLC)
           if (NEL.gt.0) then
             do 30 IEindex=1,NEL
               IE=IEL(IEindex)

C Trace.
               if(act.eq.'p'.or.act.eq.'P')then
                 write(outs,'(a,i3,a,i3,a,i3,a,i3,2a,i3)')
     &             'Matching layer in zone',IZ,' Surface: ',IS,
     &             ' layer',IE,' in MLC ',IMLC,' ',mlcname(IMLC),
     &             LAYERS(IMLC)
                 call edisp(iuout,outs)
                 goto 30
               endif

C Get the legacy material index and the next available slot.
               ip=matlegindex(IPRMAT(IMLC,IE))
               call getnextascislot(ip,inext)

C Have we already dealt with this MLC?
               if(nbpastmlc.eq.0)then
                 continue
               elseif(nbpastmlc.eq.1)then
                 if(IMLC.eq.pastmlc(1))then
                   isadupmlc=.true. ! Use the existing _cal version
                 endif
               else
                 do loop=1,nbpastmlc
                   if(IMLC.eq.pastmlc(loop))then
                     isadupmlc=.true. ! Use the existing _cal version.
                   endif
                 enddo
               endif
               if(.NOT.isadupmlc)then

C << _cal issue >>
C Make a copy of the MLC, name it with _cal and copy across
C its attributes and update the relevant layer with new material.
                 NMLC=NMLC+1
                 mlcdbitems=NMLC
                 nbpastmlc=nbpastmlc+1
                 pastmlc(nbpastmlc)=IMLC
                 lnml=lnblnk(mlcname(IMLC))
                 write(mlcname(NMLC),'(2a)')mlcname(IMLC)(1:lnml),
     &             '_cal'
                 lnmlcname(NMLC)=lnblnk(mlcname(NMLC))
                 lnml=lnblnk(mlcmenu(IMLC))
                 write(mlcmenu(NMLC),'(2a)')mlcmenu(IMLC)(1:lnml),
     &             ' cal'
                 lnmlcmenu(NMLC)=lnblnk(mlcmenu(NMLC))
                 mlctype(NMLC)=mlctype(IMLC)
                 lnml=lnblnk(mlcdoc(IMLC))
                 write(mlcdoc(NMLC),'(2a)') mlcdoc(IMLC)(1:lnml),
     &             ' cal'
                 mlcincat(NMLC)=mlcincat(IMLC)
                 mlcoptical(NMLC)=mlcoptical(IMLC)
                 matsymindex(NMLC)=0            ! reset to assume no reversed
                 mlccatindex(NMLC)=mlccatindex(IMLC)  ! assume same class 
                 LAYERS(NMLC)=LAYERS(IMLC)

C Find its category and increment its counter/
                 lncatn=lnblnk(mlcincat(NMLC))
                 do loop2=1,mlccats
                   if(mlcincat(NMLC)(1:lncatn).eq.
     &                mlccatname(loop2)(1:lnmlccatname(loop2)))then
                     mlccatitems(loop2)=mlccatitems(loop2)+1
                     IC=loop2  ! reset the focus category to one we have copied
                   endif
                 enddo

                 DO ILL=1,LAYERS(NMLC)      ! copy layer attributes
                   DTHK(NMLC,ILL)=DTHK(IMLC,ILL)
                   if(ILL.eq.IE)then
                     IPR(NMLC,ILL)=inext
                     IPRMAT(NMLC,ILL)=matdbitems+1
                   else
                     IPR(NMLC,ILL)=IPR(IMLC,ILL)
                     IPRMAT(NMLC,ILL)=IPRMAT(IMLC,ILL)
                   endif
                   ITMCFCDB(NMLC,ILL)=ITMCFCDB(IMLC,ILL)
                   DRAIR(NMLC,ILL,1)=DRAIR(IMLC,ILL,1)
                   DRAIR(NMLC,ILL,2)=DRAIR(IMLC,ILL,2)
                   DRAIR(NMLC,ILL,3)=DRAIR(IMLC,ILL,3)
                   LAYDESC(NMLC,ILL)=LAYDESC(IMLC,ILL)
                 ENDDO  ! of ILL

C << _cal issue >>
C Make up a new LAYDESC for use when writing out *layer.
                 iorig=IPRMAT(IMLC,IE)  ! remember original array index
                 lnam=lnblnk(matname(iorig))
                 lfordoc = 68 - (lnam +8)    ! space left for doc
                 write(LAYDESC(NMLC,IE),'(5a)') 
     &             matname(iorig)(1:lnam),'_cal',' : ',
     &             matdoc(iorig)(1:lfordoc),'_cal'

C Save to MLC.
                 call edisp(iuout,'Updating the MLC database...')
                 CALL EMKAMLD2(iuout,IER) ! write it out
                 
               endif

C << _cal issue >>
C Have we already dealt with this material?
               if(nbpastmat.eq.0)then
                 continue
               elseif(nbpastmat.eq.1)then
                 if(IPRMAT(IMLC,IE).eq.pastmat(1))then
                   isadupmat=.true.  ! Use the existing _cal version.
                 endif
               else
                 do loop2=1,nbpastmat
                   if(IPRMAT(IMLC,IE).eq.pastmat(loop2))then
                     isadupmat=.true.  ! Use the existing _cal version.
                   endif
                 enddo
               endif

               call getnextascislot(ip,inext)

C << _cal issue >>
C If there is room in the materials database make a copy.
               if(.NOT.isadupmat.and.inext.le.600)then
                 iorig=IPRMAT(IMLC,IE)  ! remember original array index
                 ILNE=matdbitems+1
                 lnm=lnblnk(matname(iorig))
                 nbpastmat=nbpastmat+1  ! remember
                 pastmat(nbpastmat)=iorig
                 write(matname(ILNE),'(2a)') 
     &             matname(iorig)(1:lnm),'_cal'
C User confirmation skip editing for now.
C                 write(t32,'(a)') matname(ILNE)
C                 CALL EASKS(t32,'Name of calib material','confirm:',
C     &             32,' ','material name',IER,nbhelp)
C                 write(matname(ILNE),'(a)') t32(1:lnblnk(t32))
                 lnd=lnblnk(matdoc(iorig))
                 write(matdoc(ILNE),'(2a)') 
     &             matdoc(iorig)(1:lnd),' for calibration'
                 matlegindex(ILNE)=inext
                 mathash(inext)=ILNE
                 matcatindex(ILNE)=matcatindex(iorig)
                 matdbcon(ILNE)=matdbcon(iorig)
                 matdbden(ILNE)=matdbden(iorig)
                 matdbsht(ILNE)=matdbsht(iorig)
                 matdboute(ILNE)=matdboute(iorig)
                 matdbine(ILNE)=matdbine(iorig)
                 matdbouta(ILNE)=matdbouta(iorig)
                 matdbina(ILNE)=matdbina(iorig)
                 matdbdrv(ILNE)=matdbdrv(iorig)
                 matdbthick(ILNE)=matdbthick(iorig)
                 matcatindex(ILNE)=matcatindex(iorig)  ! keep in same category
                 matopaq(ILNE)=matopaq(iorig)
                 matirtran(ILNE)=matirtran(iorig)
                 matsoldrtrn(ILNE)=matsoldrtrn(iorig)
                 matsoldrotrfl(ILNE)=matsoldrotrfl(iorig)
                 matsoldrinrfl(ILNE)=matsoldrinrfl(iorig)
                 matvistran(ILNE)=matvistran(iorig)
                 matvisotrfl(ILNE)=matvisotrfl(iorig)
                 matvisinrfl(ILNE)=matvisinrfl(iorig)
                 matrender(ILNE)=matrender(iorig)

                 matdbitems=matdbitems+1
                 matcatitems(matcatindex(iorig))=
     &             matcatitems(matcatindex(iorig))+1
                 nbpastmat=nbpastmat+1
                 pastmat(nbpastmat)=iorig  ! remember

C Depending on what needs to change update the relevant
C material attribute.
                 write(outs,*) 'from:'
                 call edisp(iuout,outs)
                 write(outs,'(3f9.3,4f5.2)')matdbcon(ILNE),
     &             matdbden(ILNE),matdbsht(ILNE),matdbine(ILNE),
     &             matdboute(ILNE),matdbina(ILNE),matdbouta(ILNE)
                 call edisp(iuout,outs)
                 if (IDATR(ICC,2).eq.1) then
                   call UAEDIT(matdbcon(ILNE),IDATR(ICC,3),DATU(ICC,1),
     &               DATU(ICC,2),DIR,-1,0.01,0.0)
                 elseif (IDATR(ICC,2).eq.2) then
                   call UAEDIT(matdbden(ILNE),IDATR(ICC,3),DATU(ICC,1),
     &               DATU(ICC,2),DIR,0,0.01,9000.0)
                 elseif (IDATR(ICC,2).eq.3) then
                   call UAEDIT(matdbsht(ILNE),IDATR(ICC,3),DATU(ICC,1),
     &               DATU(ICC,2),DIR,0,0.01,3000.0)
                 elseif (IDATR(ICC,2).eq.4) then
                   call UAEDIT(matdboute(ILNE),IDATR(ICC,3),DATU(ICC,1),
     &               DATU(ICC,2),DIR,0,0.001,0.999)
                   call UAEDIT(matdbine(ILNE),IDATR(ICC,3),DATU(ICC,1),
     &               DATU(ICC,2),DIR,0,0.001,0.999)
                 elseif (IDATR(ICC,2).eq.5) then
                   call UAEDIT(matdbouta(ILNE),IDATR(ICC,3),DATU(ICC,1),
     &               DATU(ICC,2),DIR,0,0.001,0.999)
                   call UAEDIT(matdbina(ILNE),IDATR(ICC,3),DATU(ICC,1),
     &               DATU(ICC,2),DIR,0,0.001,0.999)
                 endif

                 write(outs,*) 'to:'
                 call edisp(iuout,outs)
                 write(outs,'(3f9.3,4f5.2)')matdbcon(ILNE),
     &             matdbden(ILNE),matdbsht(ILNE),matdbine(ILNE),
     &             matdboute(ILNE),matdbina(ILNE),matdbouta(ILNE)
                 call edisp(iuout,outs)

C Update the materials database. (Following code is
C similar to that in edcondb.F).
                 call erpfree(ifmat,istat)  ! in case it is still open
                 IAF=IFIL+1
                 call erpfree(iaf,istat)  ! in case it is still open
                 lltmp=' '
                 if(ipathmat.eq.0.or.ipathmat.eq.1)then
                   write(lltmp,'(a)') LFMAT(1:lnblnk(LFMAT))
                   CALL mkascimat(IAF,lltmp,IER)
                 elseif(ipathmat.eq.2)then
                   lndbp=lnblnk(standarddbpath)
                   write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &               LFMAT(1:lnblnk(LFMAT))
                   CALL mkascimat(IAF,lworking,IER)
                 endif
               endif
 30          continue  ! of layers
           endif

C << _cal issue >>
C The surface should point to an alternative MLC which uses
C the _cal material. And write out the zone geometry file.
           if(act.eq.'p'.or.act.eq.'P')then
             continue
           else
             write(SMLCN(IZ,IS),'(a)') mlcname(NMLC)
             call geowrite(IFIL+1,LGEOM(IZ),IZ,iuout,3,IER)
           endif
 20      continue  ! of associated surfaces
       endif
 10   continue  ! of associated zones
      RETURN
      END  ! of UMATAE01

C ******************** CHKPRIM ********************
C Scans the layers of the MLC for a specific surface
C and returns if and where a specific material is used.

      SUBROUTINE CHKPRIM(IZONE,ISURF,IPRIM,NEL,IEL,IMLC)
#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"

C Parameters.
      integer izone  ! requested zone
      integer isurf  ! surface in the zone
      integer iprim  ! legacy material index
      integer nel    ! number of matching layers
      integer iel    ! array of the position of the matched material

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      DIMENSION IEL(ME)

C Set number of matching layers to zero.

      NEL=0

C Find correct MLC. Check if the legacy material index matches
C the passed value. C Find matching MLC.
      ioc=IZSTOCN(izone,isurf)
      call matchmlcdesc(SMLCN(izone,isurf),i)
      IMLC=i
      do 20 J=1,LAYERS(IMLC)
        if (IPR(IMLC,J).eq.IPRIM) then
          NEL=NEL+1   ! matches criteria
          IEL(NEL)=J  ! layer it was found in
        endif
 20   continue

      RETURN
      END

C ******************** UCTLAT01 ********************
C Identifies control data should be changed for calibration.

      SUBROUTINE UCTLAT01(IACT,DIR,ACT)
#include "building.h"
#include "uncertainty.h"
#include "control.h"

C Parameters.
      integer IACT
      real DIR
      character ACT*1 

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      COMMON/UATD/UACLIM(6,4),UACTL(7,6),UAMCSA(MNACT)
      COMMON/UA2T/NTLOC(MNIL),NTG(MNIL,4)

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      common/cctlnm/ctldoc,lctlf
      CHARACTER lctlf*72,ctldoc*248

      character uhphrase*42,outs*124
      CHARACTER ITEM(7)*20
      real ACNG
      logical unixok,XST

C Data stored in UA3 has the following meaning in this routine:
C IDATR(?,1) (ICF): Control function reference number.
C IDATR(?,2) (ICD): Control function day type.
C IDATR(?,3): Change type (%; +/-; explicit).
C IDATR(?,4) (ICP): Control function period.
C IDATR(?,5): Uncertain parameter.
      ICC=IACTD(IACT,1)
      ICL=IACTD(IACT,2)
      ICF=IDATR(ICC,1)
      ICD=IDATR(ICC,2)
      ICP=IDATR(ICC,4)
      itopic= IDATR(ICC,5)  ! which of the 7 topics

C Make specific header string for each change (to match urunvals)
      lncng=lnblnk(LCNG(ICC))
      lnloc=lnblnk(LLOC(ICL))
      uhphrase=' '
      write(uhphrase,'(4a,i2.2,a,i2.2,a)')LCNG(ICC)(1:lncng),
     &  ':',LLOC(ICL)(1:lnloc),':',ICF,':',ICP,','
      luhphrase=lnblnk(uhphrase)
      if(act.eq.'p'.or.act.eq.'P')then
        write(outs,'(2a)') 'UCTLAT01 Preview ',uhphrase(1:luhphrase)
      else
        write(outs,'(2a)') 'UCTLAT01 Implement ',uhphrase(1:luhphrase)
      endif
      call edisp(iuout,outs)

      ITEM(1) ='Period start time'
      ITEM(2) ='Maximum heating flux'
      ITEM(3) ='Minimum heating flux'
      ITEM(4) ='Maximum cooling flux'
      ITEM(5) ='Minimum cooling flux'
      ITEM(6) ='Heating set point'
      ITEM(7) ='Cooling set point'

      call edisp(iuout,'  ')
      write (outs,'(2a,i2,a,3i3)') 'Focus on ',ITEM(itopic),itopic,
     &  ' IDATR: ctl func & day type & change typ',
     &  (IDATR(ICC,IX),IX=1,3)
      call edisp(iuout,outs)

      write (outs,'(a,2i3,a,2f7.2)') ' IDATR: func period & parameter',
     &  (IDATR(ICC,IX),IX=4,5),' DATU: ',(DATU(ICC,IX),IX=1,2)
      call edisp(iuout,outs)

C UACTL (?,1)=change type (%,+/-); (?,2)=magnitude; (?,3)=associated action;
C UACTL (?,4)=time status (0 not active, 1 active)
C UACTL (?,5)=counter for the uncertainty action being dealt with
C UACTL (?,6)=value of DIR
      UACTL(itopic,1)=float(IDATR(ICC,3))
      UACTL(itopic,5)=float(IACT)  ! which of the uncertainty tasks
      if (IDATR(ICC,3).eq.1) then
        UACTL(itopic,2)=((DATU(ICC,1)/100.)*DIR)+1.0
      elseif (IDATR(ICC,3).eq.2) then
        UACTL(itopic,2)=DATU(ICC,1)*DIR
      else
        UACTL(itopic,2)=0.
      endif
      UACTL(itopic,3)=IACT
      UACTL(itopic,6)=0.  ! slot for DIR value during simulation
      write (outs,'(a,7f8.3)') ' UACTL: chg type ',(UACTL(IX,1),IX=1,7)
      call edisp(iuout,outs)
      write (outs,'(a,7f8.3)') ' UACTL: value    ',(UACTL(IX,2),IX=1,7)
      call edisp(iuout,outs)
      write (outs,'(a,7f8.3)') ' UACTL: ',(UACTL(IX,3),IX=1,7)
      call edisp(iuout,outs)
      write (outs,'(a,7f8.3)') ' UACTL: time stat',(UACTL(IX,4),IX=1,7)
      call edisp(iuout,outs)
      write (outs,'(a,7f8.3)') ' UACTL: iact     ',(UACTL(IX,5),IX=1,7)
      call edisp(iuout,outs)
      write (outs,'(a,7f8.3)') ' UACTL: DIR      ',(UACTL(IX,6),IX=1,7)
      call edisp(iuout,outs)

C Rescan the zone control file.
      ICTLF=IFIL+1
      CALL ERPFREE(ICTLF,ISTAT)
      call FINDFIL(LCTLF,XST)
      if(XST)then
        call edisp(iuout,'Reading existing control file.')
        CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
      else
        call usrmsg('No zone control defined.',
     &              'Returning.','W')
        return
      endif

C For each control loop (II) each day type (IJ) and each control
C period IK and control topic (I).
      do II=1,ncf

C Check if the correct control loop.
        ICF=IDATR(ICC,1)
        ICD=IDATR(ICC,2)
        ICP=IDATR(ICC,4)
        if(II.eq.ICF)then
          NN=nbcdt(ii)
          if(nbcdt(ii).eq.0)NN=nbdaytype  ! zero indicates all calendar day types
          do IJ=1,NN
            if(IJ.eq.ICD)then       ! Check if the correct day type.
              do IK=1,NBCDP(II,IJ)  ! for each period
                if(IK.eq.ICP)then   ! if correct period

C Next block is based on sentim.F UAT01a
                  do 10 I=1,7

C Check if control parameter is active.
                    if (nint(UACTL(I,1)).eq.0) goto 10

                    if (nint(UACTL(I,1)).gt.0) then   ! echo the before value
                      if(I.eq.1)then
                        if(act.eq.'p'.or.act.eq.'P')then
                          write (outs,'(3a,f7.2)')'Current: ',
     &                      ITEM(I)(1:lnblnk(ITEM(I))),' ',
     &                      tbcps(ICF,ICD,ICP)
                          call edisp(iuout,outs)
                        else
                          write (36,*)ITEM(I)(1:lnblnk(ITEM(I))),
     &                      tbcps(ICF,ICD,ICP)
                        endif
                      else
                        if(act.eq.'p'.or.act.eq.'P')then
                          write (outs,'(3a,f7.2)')'Current: ',
     &                      ITEM(I)(1:lnblnk(ITEM(I))),' ',
     &                      bmiscd(ICF,ICD,ICP,I)
                          call edisp(iuout,outs)
                        else
                          write (36,*)ITEM(I)(1:lnblnk(ITEM(I))),
     &                      bmiscd(ICF,ICD,ICP,I)
                        endif
                      endif
                    endif

C Uncertainty defined and in valid time period.
                    if (nint(UACTL(I,1)).eq.1) then ! % change
                      PCNG=UACTL(I,2)
                      if (I.eq.1) then  ! the period
                        if(act.eq.'p'.or.act.eq.'P')then
                          write (outs,'(a,f7.2)')'Suggested: ',
     &                      tbcps(ICF,ICD,ICP)*PCNG
                          call edisp(iuout,outs)
                        else
                          tbcps(ICF,ICD,ICP)=tbcps(ICF,ICD,ICP)*PCNG
                        endif
                      else
                        if(act.eq.'p'.or.act.eq.'P')then
                          write (outs,'(a,f7.2)')'Suggested: ',
     &                      bmiscd(ICF,ICD,ICP,I)*PCNG
                          call edisp(iuout,outs)
                        else
                        bmiscd(ICF,ICD,ICP,I)=bmiscd(ICF,ICD,ICP,I)*PCNG
                        endif
                      endif
                    elseif (nint(UACTL(I,1)).eq.2) then  ! abs change
                      ACNG=UACTL(I,2)
                      if (I.eq.1) then  ! the period
                        if(act.eq.'p'.or.act.eq.'P')then
                          write (outs,'(a,f7.2)')'Suggested: ',
     &                      tbcps(ICF,ICD,ICP)+ACNG
                          call edisp(iuout,outs)
                        else
                          tbcps(ICF,ICD,ICP)=tbcps(ICF,ICD,ICP)+ACNG
                        endif
                      else
                        if(act.eq.'p'.or.act.eq.'P')then
                          write (outs,'(a,f7.2)')'Suggested: ',
     &                      bmiscd(ICF,ICD,ICP,I)+ACNG
                          call edisp(iuout,outs)
                        else
                        bmiscd(ICF,ICD,ICP,I)=bmiscd(ICF,ICD,ICP,I)+ACNG
                        endif
                      endif
                    endif
                    if(act.eq.'p'.or.act.eq.'P')then
                      continue
                    else
                      if (nint(UACTL(I,1)).gt.0) then   ! echo the updated value
                        if(I.eq.1)then
                write (6,*)ITEM(I)(1:lnblnk(ITEM(I))),tbcps(ICF,ICD,ICP)
                        else
                write (6,*)ITEM(I)(1:lnblnk(ITEM(I))),
     &                   bmiscd(ICF,ICD,ICP,I)
                        endif
                      endif
                    endif
 10               continue ! of control attributes
                endif
              enddo    ! of each period
            endif
          enddo        ! of each day type
        endif
      enddo            ! of each control loop

C Write out the control file.
      if(act.eq.'p'.or.act.eq.'P')then
        continue
      else
        call edisp(iuout,'Updating zone control file.')
        CALL CTLWRT(ICTLF,IER)
      endif
      return
      end  ! of UCTLAT01

C ******************** UATFLG ********************
C Determines if control data is within NTG start day/time and
C NTG finish day/time.

      SUBROUTINE UATFLG(IDAY,IHOUR,ILOC,IVALID)
#include "building.h"
#include "uncertainty.h"

      COMMON/UA2T/NTLOC(MNIL),NTG(MNIL,4)

      IVALID=0

C For uncertainty location ILOC check if in valid period.
C If iday is negative check time only.
      if (IDAY.lt.0) then
        if((IHOUR.gt.NTG(ILOC,2).and.IHOUR.lt.NTG(ILOC,4)))IVALID=1
      elseif (IDAY.gt.NTG(ILOC,1).and.IDAY.lt.NTG(ILOC,3)) then
        IVALID=1
      elseif (IDAY.eq.NTG(ILOC,1).and.IDAY.lt.NTG(ILOC,3)) then
        if (IHOUR.gt.NTG(ILOC,2).or.IHOUR.eq.-1) then
          IVALID=1
        endif
      elseif (IDAY.gt.NTG(ILOC,1).and.IDAY.eq.NTG(ILOC,3)) then
        if (IHOUR.lt.NTG(ILOC,4).or.IHOUR.eq.-1) then
          IVALID=1
        endif
      elseif (IDAY.eq.NTG(ILOC,1).and.IDAY.eq.NTG(ILOC,3)) then
        if ((IHOUR.gt.NTG(ILOC,2).and.IHOUR.lt.NTG(ILOC,4)).or.
     &       IHOUR.eq.-1) then
          IVALID=1
        endif
      endif

      RETURN
      END

C ******************** splitcalinputhead ********************
C Takes a PHRASE such as facade_blinds:open_a_blinds
C and figures out which iact and zone(s) are associated.

      subroutine splitcalinputhead(PHRASE,itact,itis,itwhere,
     &  nbzone,izlist)
#include "building.h"
#include "uncertainty.h"

C Function definition.
      INTEGER :: lnblnk

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout

      character PHRASE*42
      integer itact,itis,itwhere ! index of action,distribution,location
      integer nbzone ! number of associated zones
      integer izlist(MCOM)
      character CH*1,FIRST*36,SECOND*36
      character outs*124
      integer lnfirst,lnsec,loop,loop2

C Use splitstratchar FIRST is the uncertainty and 
C SECOND is the scope/location.
      CH=':'; FIRST='  '; SECOND='  '
      call splitstratchar(PHRASE,CH,FIRST,SECOND,ier)
      write(outs,'(4a)') 'Uncertainty what is ',
     &  first(1:lnblnk(first)),' & where ',
     &  second(1:lnblnk(second))
      call edisp(iuout,outs)

C Loop through existing uncertainty defs to find matching LCNG as
C well as a maching LLOC.
      itis=0; itwhere=0; itact=0
      lnfirst=lnblnk(first)
      lnsec=lnblnk(second)
      do loop=1,NIACT
        loop2=IACTD(loop,1)  ! change
        lnlcng=lnblnk(LCNG(loop2))
        loop3=IACTD(loop,2)  ! location
        lnloc=lnblnk(LLOC(loop3))
        if((first(1:lnfirst).eq.LCNG(loop2)(1:lnlcng)).and.
     &     (second(1:lnsec).eq.LLOC(loop3)(1:lnloc)))then
          itis=loop2
          itwhere=loop3
          itact=loop ! the action index
          call LISTUAL(1,itis)  ! remind us what it is
          call LISTUAL(2,itwhere)  ! remind us where it is
          EXIT  ! no need to check further
        endif
      enddo

C If no match.
      if(itwhere.eq.0)then
        nbzone=0
        izlist(1)=0
        return
      endif
        
C Find the associated zone.
      if(NZGU(itwhere).eq.1)then
        nbzone=1
        izlist(1)=NZNOGU(itwhere,1)
      else
        nbzone=NZGU(itwhere)
        do ii=1,NZGU(itwhere)
          izlist(ii)=NZNOGU(itwhere,ii)
        enddo
      endif
C      write(6,*) 'splitcalinputhead ',itact,itis,itwhere,nbzone,izlist
      return
      end  ! of splitcalinputhead

C ******************** makescriptexecutable ********************
C Is passed a file name and applies a+x to it.

      subroutine makescriptexecutable(filename)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout

      integer lnblnk  ! function definition

      character*(*) filename
      character tmode*8
      character doit*248
      character msg*124
      logical unixok
      integer lnfn

      lnfn=lnblnk(filename)
      call isunix(unixok)
      tmode='text'
      if(unixok)then
        write(doit,'(2a)')'chmod a+x ./',filename(1:lnfn)
        call runit(doit,tmode)
      else
        write(doit,'(2a)')'chmod a+x ',filename(1:lnfn)
        call runit(doit,tmode)
      endif
      write(msg,'(2a)')'Look for script ',filename(1:lnfn)
      call pausems(500)
      return
      end  ! of makescriptexecutable
