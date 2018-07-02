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

C Controling routines for parametric studies:
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

C ***************** UNCERTA *********************
      subroutine UNCERTA()
#include "building.h"
#include "model.h"
C Common blocks UA2 UA3 UA4 within uncertainty.h
#include "uncertainty.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      common/rpath/path
     
      CHARACTER*33 ITEM(11)
      character path*72
      CHARACTER LTEMP*72,DTEMP*72,outs*124
      integer NITMS,INO ! max items and current menu item

      LOGICAL LIBXST

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='UNCERTA'  ! set for subroutine
      
      IUALF=IFIL
      helptopic='uncert_overview_a'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
C Ask for an uncertainty library name and check to see if it exists.
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
      ITEM(11)='- exit this menu                 '
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

C ***************** DISPOPT *********************
C DISPOPT displays the contents of the changes, locations or 
C actions common depending on the value of ITYP.
C ITYP: 1=changes, 2=locations, 3=actions

      subroutine DISPOPT(ITYP,IUALF)
#include "building.h"
#include "epara.h"
C Common blocks UA1 UA2 UA3 UA4 within uncertainty.h
#include "uncertainty.h"

C muadel should be equivalent to max(MNCNG,MNIL,MNACT))
      parameter (muadel=50)
      COMMON/UA32/IMISCD(MNCNG,2)

      character ta15(MNCNG)*24
      CHARACTER KEY*1,MLCITM(35)*31

      dimension IDEL(muadel)
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='DISPOPT'  ! set for subroutine
      
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
     &                           LLOC(IACTD(M,2))(1:12)
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
      MLCITM(M+7)='- exit this menu        '
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
              IDMAT(ID,1)=IDMAT(ID+1,1)
              IDMAT(ID,2)=IDMAT(ID+1,2)
              IDMAT(ID,3)=IDMAT(ID+1,3)
              DATU(ID,1)=DATU(ID+1,1)
              DATU(ID,2)=DATU(ID+1,2)
              IMISCD(ID,1)=IMISCD(ID+1,1)
              IMISCD(ID,2)=IMISCD(ID+1,2)
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


C ***************** DEFUAL *********************
C DEFUAL create a set of default uncertainties

      subroutine DEFUAL
#include "building.h"
C Common blocks UA3 within uncertainty.h
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN

      IERR=0
      IER=0

C Need controlling menu here - select uncertainty category.

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
C        call ??

C IDTYPU = 1001 : Control definitions
C        IDTYPU(ICDEF)=1001
C        call SDDEF1001(ICDEF,IER)

      return
      END

C ***************** CNGDEF *********************
C CNGDEF allows the user to define the distribution of an uncertainty.
C ICDEF  If equal to zero then add a new definition, else edit.

      subroutine CNGDEF(ICDEF,IERR)
#include "building.h"
C Common blocks UA1 UA3 within uncertainty.h
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN

      character t24*24
      CHARACTER ITEM(17)*32, outs*124
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CNGDEF'  ! set for subroutine
      
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
      ITEM(2) ='a  Weather                   '
      ITEM(3) ='   Pressure (N/A)            '
      ITEM(4) ='c  Thermophysical properties '
      ITEM(5) ='d  Construction composites   '
      ITEM(6) ='e  Optical properties (draft)'
      ITEM(7) ='f  Ideal controls'
      ITEM(8) =' --------------------------- '
      ITEM(9) =' Zones composition ...       '
      ITEM(10)='g  Geometry (air volume)     '
      ITEM(11)='   Boundary conditions (N/A) '
      ITEM(12)='i  Zone operations           '
      ITEM(13)='j  Convection coefficients   '
      ITEM(14)='   Casual gain control (N/A) '
      ITEM(15)=' --------------------------- '
      ITEM(16)='? Help                       '
      ITEM(17)='- Exit                       '
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

C ***************** LOCDEF *********************
C LOCDEF The location definition menu allows the user to define the 
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
C Common blocks UA1 UA2 within uncertainty.h
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON      
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/TMCB2/NBCTMC(MCOM,MTMC),IBCST(MCOM,MBP,MTMC),
     &             IBCFT(MCOM,MBP,MTMC),IBCSUR(MCOM,MTMC)

      COMMON/UA2T/NTLOC(MNIL),NTG(MNIL,4)

      DIMENSION STMP(MS),IVALZ(MCOM),IVALS(MS)

      character t24*15
      CHARACTER STMP*12
      character outs*124

      integer idst,ihst,idft,ihft   ! for local editing.
      integer idf,ids,ihf,ihs
      integer icc,jjj    ! loops
      integer icurrent   ! which location we are dealing with
      logical found,foundmat,chdb,modmlc

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='LOCDEF'  ! set for subroutine
      
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
              write (outs,'(1x,a,a,25i3)')ZNAME(NZNOGU(icurrent,J)),
     &          ':',(NSNOG(icurrent,J,K),K=1,NSGU(icurrent,J))
            else
              write (outs,'(1x,a,a,2i3,a)')ZNAME(NZNOGU(icurrent,J)),
     &          ':',NTGU(icurrent,J,1),NTGU(icurrent,J,2),
     &          ' tmc type & period'
            endif
            call edisp (iuout,outs)
 30       continue
        endif

C Clear arrays in all cases. << ?? existing entity? >>
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
            if(ssmlcindex(icc).eq.ISEL)then

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
            ifoc=ssmlcindex(icc)
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
          NSGU(icurrent,IZ)=0  ! no specific surfaces
          NSNOG(icurrent,IZ,1)=0
          NTGU(icurrent,IZ,1)=iwhich

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
          NTGU(icurrent,IZ,2)=iwhichp! is the TMC period
  
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
              STMP(I)=SSNAME(icn1)
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

C Ok to instanciate the data and complete the editing.
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

C ***************** LINKDEF *********************
C LINKDEF allows the association of changes with locations to 
C make actions.  These actions are then used by the simulator 
C to model the described uncertainty. 
C ITEM   If equal to zero then add a new definition, else edit. 

      subroutine LINKDEF(ITEM,IERR)
#include "building.h"
C Common blocks UA1 UA2 UA3 UA4 within uncertainty.h
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN

      DIMENSION IVALC(MNCNG),IVALL(MNIL)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub=' LINKDEF'  ! set for subroutine
      
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

C ************************ UPELM **************************
C UPELM retrieves the unique materials used in the ESP-r 
C model. This is done by copying all the materials used 
C into an array, then sorting them via a bubble search removing 
C any duplicates.

      subroutine UPELM(IPRIM,PRIMLIST,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"

C      COMMON/OUTIN/IUOUT,IUIN

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      INTEGER PRIMLIST(MMLC*ME)

C      CHARACTER MLCITM(35)*40,KEY*1,NAM*72,TITL*72

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

C ***************** CALIB *********************
      subroutine CALIB(ITRC,ITRU,IER)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G1/G2/G7/prec2/precz.
#include "geometry.h"
C UA1 in uncertainty.h
#include "uncertainty.h"
#include "net_flow.h"
#include "tdf2.h"
#include "esprdbfile.h"
#include "material.h"

C espriou.h provides current file.
#include "espriou.h"

C FMI.h provides FMU connection flag.
#include "FMI.h"

C Function definition
      INTEGER :: lnblnk
      
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      INTEGER :: mmod,limit,limtty
      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: iuout,iuin
      
      INTEGER :: ncomp,ncon
      common/C1/NCOMP,NCON
      INTEGER :: indcfg
      common/C6/INDCFG
      common/cctlnm/ctldoc,lctlf
      character ctldoc*248,lctlf*72

      common/user/browse

      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      INTEGER :: nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),scfdres(MSPS),sipvres
      character sblres*72,sflres*72,splres*72,smstres*72,
     &  selres*72,scfdres*72,sipvres*72

      common/spfldes/spfdescr(MSPS)
      character spfdescr*30
      INTEGER :: isstday,isstmon,isfnday,isfnmon
      common/spflper/isstday(MSPS),isstmon(MSPS),isfnday(MSPS),
     &               isfnmon(MSPS)
     
C TDF related.
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15
      INTEGER :: IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      COMMON/UA31/MATNAM(MNCNG,2)
      COMMON/UA32/IMISCD(MNCNG,2)
      integer nbpastmlc,pastmlc,nbpastmat,pastmat
      COMMON/UA33/nbpastmlc,nbpastmat,pastmlc(10),pastmat(10)

      logical DOK,OK
      logical browse
      logical havefile,unixok,LIBXST
      logical located
      LOGICAL concat,remote,anymeasured
      logical domlc,dogeo,doopr,doctl,doblnd  ! tasks to do
      character*72 tfile,OLDLCFGF
      character longtfile*144,longtfiledos*144
      character loutstr*248
      character tmode*8,doit*248
      character outs*148,fs*1,aut*12,key*1
      character descra*7,descrb*7,descrst*10,descrfn*10
      character ltpath*72,filen*72
      character weather_extr*96
      character obs_extr*96,sim_extr*96
      integer matarrayindex ! the indes within matdatarray
      character NAM*72,MATNAM*32
      integer ier,itis      ! non-zero if a problem
      integer nbiobservitem ! number of observations in temporal file
      integer iobservitem   ! pointer to temporal index for observation
      dimension iobservitem(20)
      character obsdatfilename(20)*72
      character obsdatscript(20)*72
      character obsdatkey(20)*1
      character obsdatzonename(20)*12
      integer iobsdatzone(20)
      character simdatfilename(20)*72
      character simdatscript(20)*72
      CHARACTER LTEMP*72,DTEMP*72
      integer loopo,loop
      integer nbitem,items
      dimension items(MCOM)
      dimension ivalsg(MCOM)
      character APE*6,EXT*4,MODE*4
      character NNAME*144
      character buffer*96,first*42,second*42,CH*1
      character jtag*42,phrase*96
      logical docnn,XST,QUIET

C dd is character array for selecting simulation parameter sets.
      character dd(MSPS+2)*55,brw*11
      character dq*1,lcurley*1,rcurley*1,lsquare*1,rsquare*1
      
      INTEGER :: itrc,itru,ic,istat,ii
      INTEGER :: ijdstart,ijdfinish,IRT,isauto
      INTEGER :: IUNIT
      INTEGER :: ltrf
      INTEGER :: IUALF

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CALIB'  ! set for subroutine
      
C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      aut=' silent'
      remote=.false.
      dq = char(34)  ! double quote
      lcurley = char(123)  ! {
      rcurley = char(125)  ! }
      lsquare = char(91)   ! [
      rsquare = char(93)   ! ]

C Calibration. First check if there is an uncertainty definition
C in the model. If there is list its directives.
      IUALF=IFIL
      INQUIRE (FILE=LUALF,EXIST=LIBXST)
      if(.NOT.LIBXST)then
        call edisp(iuout,
     &    'Currently no uncertainties are associated with')
        call edisp(iuout,
     &    'The modedl. Please define some.')
        return
      endif

      call edisp(iuout,'  ')
      write(outs,'(A,A)')
     &   ' Reading contents of existing UA definitions file : ',LUALF
      call edisp(iuout,outs)
      call READUAL(IUALF)
      call LISTUAL(0,0)

C The next check is whether there is a temporal file with observed
C data included.
      if(iabs(itdflg).eq.0)then
        call edisp(iuout,' ')
        call edisp(iuout,'No temporal data has been defined.')
        call edisp(iuout,
     &    'Please use prj facility for temporal definitions.')
        return
      endif
      if(ITEMSTD.eq.0)then
        call edisp(iuout,
     &    'Calibration requires directives for uncertainty.')
        call edisp(iuout,
     &    'Please use prj facility to define uncertainties.')
        return
      endif

C List out the current temporal items.
      anymeasured=.false.
      write(outs,'(2a)')' Temporal definitions ',LTDFA
      call edisp(iuout,outs)
      call edisp(iuout,' ')
      call edisp(iuout,' Temporal entities currently used...')
      call edisp(iuout,'  ________________________________')
      call edisp(iuout,'  |temporal    |generic |associated')
      call edisp(iuout,
     &   '  |entity name |type    |with   zone &     surface ')
      do loop=1,ITEMSTD
        WRITE(outs,'(i2,1x,a,1x,a,5x,2a)')loop,DBTAG(loop),
     &    DBTASK(loop),DBZN(loop),DBSN(loop)
        call edisp(iuout,outs)
      enddo
      call edisp(iuout,' ')

C Now test if it is an observed type. Clear local arrays.
      nbiobservitem=0
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

C Loop through all items in temporal file and make note of those
C which are related to observations. The key strokes in the res
C selections is based on type: 
C a) zone dbT b) surface T c) zone RH d)zone heat e) zone cooling
      do loop=1,ITEMSTD
        if(DBTASK(loop)(1:8).eq.'DBTZNOBS')then
          anymeasured=.true.
          call edisp(iuout,
     &      'Calibration to zone temperatures possible.')
          nbiobservitem=nbiobservitem+1
          iobservitem(nbiobservitem)=loop  ! which ITEMSTD
          write(obsdatfilename(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_DBTZNOBS_obs.csv'
          write(simdatfilename(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_DBTZNOBS_sim.csv'
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
          write(simdatfilename(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_SURTOBS_sim.csv'
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
          write(simdatfilename(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_ZNRHOBS_sim.csv'
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
          write(simdatfilename(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_ZNHTOBS_sim.csv'
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
          write(simdatfilename(nbiobservitem),'(2a)') 
     &      DBZN(loop)(1:lnblnk(DBZN(loop))),'_ZNCLOBS_sim.csv'
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

      if(.NOT.anymeasured)then
 
C anymeasured not found
        call edisp(iuout,
     &    'Existing temporal data types cannot be calibrated')
        call edisp(iuout,
     &    'against. Please add observed zone temperatures or')
        call edisp(iuout,'humidity or heating or cooling.')
        return
      endif

C loop is related to position in counter of observations while
C itis is related to position in temporal arrays.
C      write(6,*) 'obs are which temporal index'
C      write(6,*) nbiobservitem,':',iobservitem
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
          IDB=IDMAT(loop,1)
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
          write(outs,'(a,i4,4a)')'Distribution :',loop,'; ',LCNG(loop),
     &      ' requires a variant of material ',NAM(1:lnblnk(NAM))
          call edisp (iuout,outs)
          domlc=.true.; dogeo=.true.
        elseif(IDTYPU(loop).eq.2) then  ! if MLC focus
          write(outs,'(a,i4,4a)')'Distribution :',loop,'; ',LCNG(loop),
     &      ' requires a variant of MLC ',MATNAM(loop,1)
          call edisp (iuout,outs)
          domlc=.true.; dogeo=.true.
        elseif(IDTYPU(loop).eq.4) then  ! if operation focus
          if (IDMAT(loop,1).eq.0) then
            write(outs,'(a,i4,3a)')'Distribution :',loop,'; ',
     &        LCNG(loop),' requires a variant of infil/vent schedule'
            call edisp (iuout,outs)
          else
            write(outs,'(a,i4,3a)')'Distribution :',loop,'; ',
     &        LCNG(loop),' requires a variant of casual gain '
            call edisp (iuout,outs)
          endif
          doopr=.true.
        elseif(IDTYPU(loop).eq.6) then  ! if optical focus
          if (IDMAT(loop,1).eq.1) then
            write(outs,'(a,i4,3a)')'Distribution :',loop,'; ',
     &        LCNG(loop),' requires a variant of zone tmc file'
            call edisp (iuout,outs)
          endif
          doblnd=.true.
        elseif(IDTYPU(loop).eq.1001) then  ! if ideal control focus
          write(outs,'(a,i4,3a)')'Distribution :',loop,'; ',LCNG(loop),
     &      ' requires a variant control file.'
          call edisp (iuout,outs)
          doctl=.true.
        endif
      enddo  ! of loop

C Check to see if there is already a model cfg file with
C _cal in the name. If someone previously ran this facility
C there offer to skip the creation of variant and go straight
C to the run assessment phase.
      write(APE,'(a)') '_cal'
      EXT='.cfg'
      CALL FNCNGR(LCFGF,APE,EXT,NNAME)
      XST=.false.
      INQUIRE (FILE=NNAME,EXIST=XST)
  79  if(XST)then
        call edisp(iuout,
     &    'A model cfg file for calibration already exists.')
        call edisp(iuout,NNAME)
        CALL EASKABCD(' ','Options:',
     &    're-generate model files','do calibration runs with it',
     &    'read calibro json file','cancel',ioption,nbhelp)
        if(ioption.eq.1)then
          continue
        elseif(ioption.eq.2)then

C Switch focus to the calibration variant.
          call edisp(iuout,'Using calibration variant of the model')
          write(OLDLCFGF,'(a)') LCFGF(1:lnblnk(LCFGF))
          write(LCFGF,'(a)') NNAME(1:lnblnk(NNAME))
          MODE='ALL '
          IUF=IPRODB   ! assign second file unit to the events db unit
          CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
          if(ier.eq.0)then
            goto 42
          else
            call usrmsg('Unable to scan calibration cfg file',
     &        'reverting to last cfg file.','W')
            write(LCFGF,'(a)') OLDLCFGF(1:lnblnk(OLDLCFGF))
            MODE='ALL '
            IUF=IPRODB   ! assign second file unit to the events db unit
            CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
            return
          endif
        elseif(ioption.eq.3)then

C Switch focus to the calibration variant and jump to 43.
          call edisp(iuout,'Using calibration variant of the model')
          write(OLDLCFGF,'(a)') LCFGF(1:lnblnk(LCFGF))
          write(LCFGF,'(a)') NNAME(1:lnblnk(NNAME))
          MODE='ALL '
          IUF=IPRODB   ! assign second file unit to the events db unit
          CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
          if(ier.eq.0)then
            goto 43
          else
            call usrmsg('Unable to scan calibration cfg file',
     &        'reverting to last cfg file.','W')
            write(LCFGF,'(a)') OLDLCFGF(1:lnblnk(OLDLCFGF))
            MODE='ALL '
            IUF=IPRODB   ! assign second file unit to the events db unit
            CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
            return
          endif
        elseif(ioption.eq.4)then
          return
        endif
      else

      endif
      
C If materials and/or mlc or optical call VER_MLC_MAT
      write(APE,'(a)') '_cal'
      if(domlc.or.doblnd)then
        call VER_MLC_MAT(APE)
      endif

C If geometry then call VER_GEO_CON_TMC
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

C Write out configuration file
C But first change names of results libraries zones, flow, plant,
C moisture, electrical cfd and IPV
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

      call edisp(iuout,'A variant of the model has been created')
  42  call edisp(iuout,
     &  'The next task is to undertake calibration runs...')
      DOK=.true.
      helptopic='calibration_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ASKOK('Would you like to proceed with calibration?',
     &  ' ',OK,DOK,nbhelp)
      if(.NOT.OK) return

C Check if there are simulation parameter sets, if so user should
C select them before proceeding.
 170  if(nsset.gt.0)then

C Select an existing set or cancel.
        do ii=1, nsset
          CALL EDAY(isstday(ii),isstmon(ii),ijdstart)
          call stdate(iyear,ijdstart,descra,descrst)
          CALL EDAY(isfnday(ii),isfnmon(ii),ijdfinish)
          call stdate(iyear,ijdfinish,descrb,descrfn)
          CALL EMKEY(ii,KEY,IER)
          write(dd(ii),'(7a)') key,' ',spfdescr(ii)
     &      (1:lnblnk(spfdescr(ii))),' ',descrst,'-',descrfn
        enddo
        dd(nsset+1)='? help            '
        dd(nsset+2)='- end             '
        ic=nsset+2

        if(MMOD.eq.8)then
          IRT=-1
        else
          IRT=-2
        endif
        CALL EMENU('Parameter sets',dd,ic,IRT)
        if(IRT.ge.ic) then
          return
        elseif(IRT.eq.ic-1) then
          helptopic='prj_sim_set_menu'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('Simulation sets',nbhelp,'-',0,0,IER)
          goto 170
        elseif(IRT.ge.1.and.IRT.le.nsset)then
          isset=IRT  ! choose an existing set
        endif
        CALL EASKABC(' ','Simulation interaction options:',
     &    'run interactively','run silently','cancel',
     &    isauto,nbhelp)
        if(isauto.eq.1)then
          aut=' interactive'
        elseif(isauto.eq.2)then
          aut=' silent'
        else
          return
        endif
        if(unixok)then
          call addpath(LCFGF,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(LCFGF,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)

C Debug the patched file name.
          write(outs,'(2a)') '* Corrected file ',
     &      longtfiledos(1:lnblnk(longtfiledos))
          call edisp248(iuout,outs,100)
          longtfile=' '
          longtfile=longtfiledos
        endif
        if(itrc.gt.1)then
          brw = ' -v -b no '
        else
          brw = ' -b no '
        endif

C Start the calibration simulation in text mode so that the sensitivity
C can be run.
        tmode='text'
        write(doit,'(7a)') 'bps -mode text -s 0 0 0 ',
     &    '-file ',longtfile(1:lnblnk(longtfile)),brw,' -p ',
     &    spfdescr(isset)(1:lnblnk(spfdescr(isset))),aut
        call usrmsg('Begining simulation via',doit,'-')
        call runit(doit,tmode)
      else

C There are no parameter sets.
        CALL EASKABC(' ','Simulation interaction options:',
     &    'run interactively','run silently','cancel',
     &    isauto,nbhelp)
        if(isauto.eq.1)then
          aut=' interactive'
        elseif(isauto.eq.2)then
          aut=' silent'
        else
          return
        endif
        call isunix(unixok)
        if(unixok)then
          call addpath(LCFGF,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(LCFGF,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)

C Debug the patched file name.
          write(outs,'(2a)') '* Corrected file ',
     &      longtfiledos(1:lnblnk(longtfiledos))
          call edisp(iuout,outs)
          longtfile=' '
          longtfile=longtfiledos
        endif
        if(concat)then
          remote=.true.
        endif
        if(itrc.gt.1)then
          brw = ' -v -b no '
        else
          brw = ' -b no '
        endif

C Start the calibration simulation in text mode so that the sensitivity
C can be run.
        tmode='text'
        write(doit,'(5a)') 'bps -mode ',tmode,
     &    ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),brw
        call usrmsg('Begining simulation via',doit,'-')
        call runit(doit,tmode)
      endif

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
      write(iunit,'(a)')'a # uncertaintes'
      write(iunit,'(a)')'c # timestep'
      write(iunit,'(a)')'& # labels '
      write(iunit,'(a)')'& # with no # '
      write(iunit,'(a)')'g # performance metrics'
      write(iunit,'(a)')'> # to file'
      write(iunit,'(a)')'bc.csv'
      write(iunit,'(a)')'\n'
      write(iunit,'(a)')'* # time '
      write(iunit,'(a)')'a # no separator '
      write(iunit,'(a)')'^ # delimiter '
      write(iunit,'(a)')'e # comma '
      write(iunit,'(a)')'a # climate '
      write(iunit,'(a)')'a # ambient '
      write(iunit,'(a)')'a '
      write(iunit,'(a)')'b # solar globH '
      write(iunit,'(a)')'a '
      write(iunit,'(a)')'c # solar diffuse '
      write(iunit,'(a)')'a '
      write(iunit,'(a)')'d # wind speed '
      write(iunit,'(a)')'a '
      write(iunit,'(a)')'e # wind direction '
      write(iunit,'(a)')'a '
      write(iunit,'(a)')'f # RH '
      write(iunit,'(a)')'! # list '
      write(iunit,'(a)')'> # close file '
      write(iunit,'(a)')'- # exit '
      write(iunit,'(a)')'- # exit '
      write(iunit,'(a)')'- # quit '
      write(iunit,'(a)')'XXX'
      CALL ERPFREE(IUNIT,ISTAT)
      tmode='text'
      if(unixok)then
        write(doit,'(a)')'chmod a+x ./weather_extract.csh'
        call runit(doit,tmode)
      else
        write(doit,'(a)')'chmod a+x weather_extract.csh'
        call runit(doit,tmode)
      endif
      call edisp(iuout,'Look for script weather_extract.csh.')
      call pausems(1000)

C Debug.
C      write(6,*) 'edcfg res remote is ',remote,nsset
      if(nsset.gt.0)then

C If there are simulation parameter sets use the names of the results
C files splres or sblres and prepend user home folder if remote is true.
        if(isset.eq.0)isset=1
        if(INDCFG.eq.2)then
          if(splres(isset)(1:2).ne.'  '.and.
     &       splres(isset)(1:4).ne.'UNKN')then
            ltrf = lnblnk(splres(isset))
            write(tfile,'(a)') splres(isset)(1:ltrf)
            write(longtfile,'(a)') splres(isset)(1:ltrf)
            if(remote)then
              call fdroot(tfile,ltpath,filen)
              call isunix(unixok)
              if(unixok)then
                if (ICHAR(ltpath(1:1)).ne.47) then
                  write(longtfile,'(3a)') upath(1:lnblnk(upath)),
     &              fs,filen(1:lnblnk(filen))
                endif
              else
                if (ltpath(2:2).ne.':') then
                  write(longtfile,'(3a)') upath(1:lnblnk(upath)),
     &              fs,filen(1:lnblnk(filen))
                endif
              endif

C Debug.
C              write(6,*) 'remote so reset tfile to ',longtfile
            endif
            havefile=.true.
          else
            tfile=' '
            havefile=.false.
          endif
          call edisp(iuout,
     &      'Take a note of the results file used.')
          call edisp248(iuout,longtfile,100)
            call edisp(iuout,' ')
        else
          if(sblres(isset)(1:2).ne.'  '.and.
     &       sblres(isset)(1:4).ne.'UNKN')then
            ltrf = lnblnk(sblres(isset))
            write(tfile,'(a)') sblres(isset)(1:ltrf)
            write(longtfile,'(a)') sblres(isset)(1:ltrf)
            if(remote)then
              call fdroot(tfile,ltpath,filen)
              call isunix(unixok)
              if(unixok)then
                if (ICHAR(ltpath(1:1)).ne.47) then
                  write(longtfile,'(3a)') upath(1:lnblnk(upath)),
     &              fs,filen(1:lnblnk(filen))
                endif
              else
                if (ltpath(2:2).ne.':') then
                  write(longtfile,'(3a)') upath(1:lnblnk(upath)),
     &              fs,filen(1:lnblnk(filen))
                endif
              endif
C Debug.
C              write(6,*) 'remote so reset tfile to ',longtfile
            endif
            havefile=.true.
          else
            tfile=' '
            havefile=.false.
          endif
          call edisp(iuout,
     &      'Take a note of the results file used.')
          call edisp248(iuout,longtfile,100)
          call edisp(iuout,' ')
        endif
      else

C There are no simulation parameter sets so take a guess at the
C file name that the user might have given.
C << update this logic to use a default name based on the root name >>
        if(browse)then
          write(longtfile,'(3a)')upath(1:lnblnk(upath)),fs,'libb'
        else
          tfile='libb'
        endif
        call edisp(iuout,'Take a note of the results file used.')
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
        call edisp(iuout,doit)
        call usrmsg('Starting assessment recovery via',doit,'-')
        call runit(doit,tmode)
      else
        call edisp(iuout,'Could not find results file.')
      endif

C Now make up a script to extract observation data into csv files.
C Do this once per observation.
      call edisp(iuout,'Extraction of observations...')
C      write(6,*) 'obs are which temporal index'
C      write(6,*) nbiobservitem,':',iobservitem
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
        write(iunit,'(a)')'\n'
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

C Debug.
C        write(6,'(a)') '*********'
C        write(6,'(a,3i3,a)') 'loop itis & iobsdatazone',
C     &    loop,itis,iobsdatzone(itis),KEY
        write(iunit,'(a)') KEY
        write(iunit,'(a)')'- # exit '
        write(iunit,'(a)')'! # list '
        write(iunit,'(a)')'> # close file '
        write(iunit,'(a)')'- # exit '
        write(iunit,'(a)')'- # exit '
        write(iunit,'(a)')'- # quit '
        write(iunit,'(a)')'XXX'
        CALL ERPFREE(IUNIT,ISTAT)
        if(unixok)then
          write(doit,'(2a)')'chmod a+x ./',
     &      obs_extr(1:lnblnk(obs_extr))
          call runit(doit,'-')
        else
          write(doit,'(2a)')'chmod a+x ',
     &      obs_extr(1:lnblnk(obs_extr))
          call runit(doit,'-')
        endif
        call edisp(iuout,'Look for observation extract script')
        call edisp(iuout,obs_extr)
        call edisp(iuout,
     &    'About to res-scan the results file...')
        call edisp248(iuout,longtfile,100)
        call edisp(iuout,' ')
        call tstamp('>','PRJ: beginning res')
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
          write(doit,'(4a)') './',obs_extr(1:lnblnk(obs_extr)),
     &      ' ',longtfile(1:lnblnk(longtfile))
          call edisp(iuout,doit)
          call usrmsg('Starting observed recovery via',doit,'-')
          call runit(doit,tmode)
          call pausems(1000)
        else
          call edisp(iuout,'Could not find results file.')
        endif

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
        write(iunit,'(a)')'a # intra set '
        write(iunit,'(a)')'> # to file'
        write(iunit,'(a)')simdatfilename(loop)
        write(iunit,'(a)')'\n'
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
        elseif(DBTASK(itis)(1:7).eq.'SURTOBS')then
          write(iunit,'(a)')'b # temps'
          write(iunit,'(a)')'i # inside face'
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
        if(unixok)then
          write(doit,'(2a)')'chmod a+x ./',
     &      sim_extr(1:lnblnk(sim_extr))
          call runit(doit,'-')
        else
          write(doit,'(2a)')'chmod a+x ',
     &      sim_extr(1:lnblnk(sim_extr))
          call runit(doit,'-')
        endif
        call edisp(iuout,'Look for sim extract script')
        call edisp(iuout,sim_extr)
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
          call edisp(iuout,doit)
          call usrmsg('Starting sim recovery via',doit,'-')
          call runit(doit,tmode)
          call pausems(1000)
        else
          call edisp(iuout,'Could not find results file.')
        endif

C And repeat for the next observation.
      enddo
      
      call edisp(iuout,
     &  'If you have a json file defining the changes needed')
  43  call edisp(iuout,
     &  'to calibrate the model do this next...')
      CALL EASKABC(
     &  'After sending the csv files to calibro you will be given',
     &  'a json file:','scan & act on directives','delay scanning',
     &  'cancel',isauto,nbhelp)
      if(isauto.eq.1)then

C Ask for file name. Parse the file and report directives.
        LTEMP='  '; DTEMP='  '; nbhelp=1
        call EASKS(LTEMP,'Json file name from Calibro (full path):',
     &    ' ',72,DTEMP,'Json file from calibro',IER,nbhelp)
        INQUIRE (FILE=LTEMP,EXIST=LIBXST)
        if (LIBXST) then
          call edisp(iuout,'  ')
          write(outs,'(A,A)')
     &     ' Reading calibro json file: ',LTEMP
          call edisp(iuout,outs)
          IUNIT=IFIL
          CALL EFOPSEQ(IUNIT,LTEMP,1,IER)
          if(IER.LT.0)then
            write(outs,'(3a)') 'JSON file ',LTEMP(1:lnblnk(LTEMP)),
     &      ' could not be opened.'
            goto 43
          endif
          write(currentfile,'(a)') LTEMP(1:lnblnk(LTEMP))

C Read lines from the file looking for specific key phrases.
  77      call lstripc(IUNIT,loutstr,99,ND,1,'json line 1',IER)

C If we reach the end of the file.
          IF(IER.NE.0)goto 1002
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
          elseif(JTAG(1:11).eq.'calibration')then
            goto 77
          elseif(JTAG(1:11).eq.'theta.stats')then
            goto 77
          elseif(JTAG(1:9).eq.'PARAMETER')then

C Keyword PARAMETER is followed by dot separated phrase. 
C Use splitstratchar FIRST is the uncertainty and 
C SECOND is the scope/location.
            CH='.'; FIRST='  '; SECOND='  '
            call splitstratchar(PHRASE,CH,FIRST,SECOND,ier)
            write(outs,'(4a)') 'Uncertainty what is ',
     &        first(1:lnblnk(first)),' & where ',
     &        second(1:lnblnk(second))
            call edisp(iuout,outs)

C Loop through existing uncertainty defs to find matching LCNG
            itis=0
            located=.false.
            lnfirst=lnblnk(first)
            do loop=1,NICNG
              loop2=IACTD(loop,1)
              lnlcng=lnblnk(LCNG(loop2))
              write(6,*) '... ',loop,loop2,' ',
     &          LCNG(loop2)(1:lnlcng),' vs ',first(1:lnfirst)
              if(first(1:lnfirst).eq.LCNG(loop2)(1:lnlcng))then
                located=.true.
                itis=loop2
                call LISTUAL(1,itis)  ! remind us what it is
                goto 78  ! now get attribute value
              endif
            enddo

C Now get the new attribute value.
  78        call lstripc(IUNIT,loutstr,99,ND,1,'json EST',IER)

C Now look for the new attribute to apply to the uncertainty.
C If the uncertainty type is MLC layer thickness ...
            k=0
            call egetrm(loutstr,K,buffer,'W','tokens',IER)
            k=0
            call EGETJSNTAGR(buffer,K,FIRST,rv,0.01,1.0,'-','ESTM',ier)
            write(6,*) 'jtag is ',FIRST(1:lnblnk(FIRST))
            write(6,*) 'real attribute is ',rv
            goto 1001  ! process
          else
            goto 77  ! read another line
          endif

C Identify what to do.
 1001     if (IDTYPU(itis).eq.1) then  ! if materials focus

C Clear MLC and material lists of what has been processed.
            nbpastmlc=0; nbpastmat=0
            do loop=1,10
              pastmlc(loop)=0
              pastmat(loop)=0
            enddo

            call edisp(iuout,'thermophysical property test...')
            do IZ=1,NCOMP
              call UMATAE01(iz,itis,rv)
            enddo
            call edisp(iuout,'thermophysical property test...done.')
          elseif(IDTYPU(itis).eq.2) then  ! if MLC focus

            write(outs,'(3a,i2)')' Composite construction: ',
     &        MATNAM(itis,1)(1:lnblnk(MATNAM(itis,1))),
     &        ' layer ',IMISCD(itis,1)
            call edisp(iuout,outs)

C Apply changes to MLC database.
            call UMLCAE02(itis,rv)
    
C Next step is to update each of the zone construction files.
            call edisp(iuout,'Updating zone construction files...')
            QUIET=.TRUE.
            do IZ=1,NCOMP
              CALL EDCON(ITRC,ITRU,IZ,QUIET,IER)
            enddo
            QUIET=.FALSE.
            call edisp(iuout,'Updating zone construction files...done.')

          elseif(IDTYPU(itis).eq.3) then  ! if weather focus
            call edisp(iuout,'weather change not yet done.')
            continue
          elseif(IDTYPU(itis).eq.4) then  ! if operation focus

C For each zone in the model call UCASAE04 to see if it is associated
C with an uncertainty location.
             do IZ=1,NCOMP
               call UCASAE04(IZ,itis,rv)
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

C Apply changes to zone controls.
            call edisp(iuout,'Updating zone control file.')
            call UCTLAT01(itis,rv)
          endif

C Check and see if there are other directives in the json file.
          IW=0
          CALL EASKAB('Proceed with next json directive:',
     &      'no','yes','check for more directives',IW,nbhelp)
          if(IW.eq.2) goto 77

        endif

C Re-establish the base case model and return.
 1002   CALL ERPFREE(IUNIT,ios)
        call edisp(iuout,'Reverting to original model...')
        write(LCFGF,'(a)') OLDLCFGF(1:lnblnk(OLDLCFGF))
        MODE='ALL '
        IUF=IPRODB   ! assign second file unit to the events db unit
        CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
        if(ier.eq.0)then
          return
        else
          call usrmsg('Unable to revert to base case cfg file',
     &      'Exiting prj.','P')

C Clear allocatable arrays
          CALL DeallocateAllArrays
          CALL EPAGEND
          STOP
        endif
        XST=.true.
        call edisp(iuout,'Reverting to original model...done.')
        goto 79  ! offer user choice

      elseif(isauto.eq.2)then
C << TODO >>

      elseif(isauto.eq.3)then

C Re-establish the base case model and return.
        write(LCFGF,'(a)') OLDLCFGF(1:lnblnk(OLDLCFGF))
        MODE='ALL '
        IUF=IPRODB   ! assign second file unit to the events db unit
        CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
        if(ier.eq.0)then
          return
        else
          call usrmsg('Unable to revert to base case cfg file',
     &      'Exiting prj.','P')

C Clear allocatable arrays
          CALL DeallocateAllArrays
          CALL EPAGEND
          STOP
        endif
      endif
      return
      end


C ***************** EGETJSNTAGPHR
C EGETJSNTAGPHR gets first json tag after position K from the STRING of
C characters and the phrase after :. Strips the leading " and the
C trailing ". Returns k at the trailing phase " Spaces within tag are ok.
C Provides a warning message if ACT='W', a failure message if ACT='F' and does
C no message if ACT='-'. Expects a json line similar to:
C   "calibration": "cellular",
      SUBROUTINE EGETJSNTAGPHR(STRING,K,JTAG,PHRASE,ACT,MSG,ier)
#include "espriou.h"
      common/OUTIN/IUOUT,IUIN

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
        A=STRING(K:K)    ! instanciate A
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
        A=STRING(K:K)    ! instanciate A
        goto 21          ! try next character
      endif
      if(A.EQ.dq) then
        K=K+1            ! increment to next character
        A=STRING(K:K)    ! instanciate A
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

  100 write(6,*) 'jtag is ',jtag(1:lnblnk(jtag))
      write(6,*) 'phrase is ',phrase(1:lnblnk(phrase))
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
 

C ***************** EGETJSNTAGR
C EGETJSNTAGR gets first json tag after position K from the STRING of
C characters and the real number after :. Strips the leading " and the
C trailing ". Returns k at the trailing phase " Spaces within tag are ok.
C Provides a warning message if ACT='W', a failure message if ACT='F' and does
C no message if ACT='-'. Expects a json line similar to:
C   "ESTIMATE": 0.0345,
      SUBROUTINE EGETJSNTAGR(STRING,K,JTAG,RV,RMN,RMX,RACT,MSG,ier)
#include "espriou.h"
      common/OUTIN/IUOUT,IUIN

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
        A=STRING(K:K)    ! instanciate A
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
        A=STRING(K:K)    ! instanciate A
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

  100 write(6,*) 'jtag is ',jtag(1:lnblnk(jtag))
      write(6,*) 'phrase is ',phrase(1:lnblnk(phrase))

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

      END

C ***************** splitstratchar
C splitstratchar splits a string at a specific character returning 2 str.
C example if char is a dot it splits spandrel_ins_thick.all_insul_fram
C into spandrel_ins_thick and all_insul_fram
      SUBROUTINE splitstratchar(STRING,CH,FIRST,SECOND,ier)
C#include "espriou.h"
      common/OUTIN/IUOUT,IUIN

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
      A=STRING(K:K)    ! instanciate A
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

  100 write(6,*) 'first is ',first(1:lnblnk(first))
      write(6,*) 'second is ',second(1:lnblnk(second))
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
C UMLCAE02 edits layer thicknesses in MLC db.
C Naming convention:
C IACT: index of uncertainty what and where
C DIR: change directive...

      SUBROUTINE UMLCAE02(IACT,DIR)
#include "building.h"
C #include "geometry.h"
C Common blocks UA2 UA3 UA4 within uncertainty.h
#include "uncertainty.h"
#include "esprdbfile.h"
#include "material.h"
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: iuout,iuin

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      COMMON/UA31/MATNAM(MNCNG,2)
      COMMON/UA32/IMISCD(MNCNG,2)

      CHARACTER MATNAM*32

      character uhphrase*42,outs*124

C Data stored in UA02 has the following meaning in this routine:
C IDMAT(?,1): The material in the chosen MLC, if this is 0 then 
C             a new MLC reference will be in MATNAM(?,2).
C IDMAT(?,2): Unused.
C IDMAT(?,3): Change type (%; +/-; explicit).
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

C The layer of the MLC is found in IMISCD(ICC,1)
      IE=IMISCD(ICC,1)

C Make specific header string for each change (to match urunvals)
      lncng=lnblnk(LCNG(ICC))
      lnloc=lnblnk(LLOC(ICL))
      uhphrase=' '
      write(uhphrase,'(3a)')LCNG(ICC)(1:lncng),':',LLOC(ICL)(1:lnloc)
      luhphrase=lnblnk(uhphrase)
      write(outs,'(2a)') 'UMLCAE02 Implementing ',uhphrase(1:luhphrase)
      call edisp(iuout,outs)
      write (outs,'(a,f9.4)') 'DTHK: ',DTHK(imlc,ie)
      call edisp(iuout,outs)

C Either edit layer thickness.
      if (IDMAT(ICC,3).eq.1) then

C Percentage change. Ensure it does not go below 1mm. 
        write(6,'(a,3f9.4)') 'DATU DIR % factor: ',DATU(ICC,1),
     &    DIR,(((DATU(ICC,1)/100.)*DIR)+1.0)
        DTHK(imlc,ie)=DTHK(imlc,ie)*(((DATU(ICC,1)/100.)*DIR)+1.0)
        if(DTHK(imlc,ie).lt.0.001)DTHK(imlc,ie)=0.001
      elseif (IDMAT(ICC,3).eq.2) then

C Absolute change. Ensure it does not go below 1mm. 
        write (6,'(a,3f9.4)') 'DATU DIR abs factor: ',
     &    DATU(ICC,1),DIR,(DATU(ICC,1)*DIR)
        DTHK(imlc,ie)=DTHK(imlc,ie)+(DATU(ICC,1)*DIR)
        if(DTHK(imlc,ie).lt.0.001)DTHK(imlc,ie)=0.001
      elseif (IDMAT(ICC,3).eq.3) then

C Explicit change. Ensure it does not go below 1mm.
        write (6,'(a,3f9.4)') 'DATU DIR expl factor: ',
     &    DATU(ICC,1),DIR,(DATU(ICC,1)-DTHK(imlc,ie))*DIR
        if (DIR.gt.0.) then
          DTHK(imlc,ie)=DTHK(imlc,ie)+(DATU(ICC,1)-DTHK(imlc,ie))*DIR
        else
          DTHK(imlc,ie)=DTHK(imlc,ie)-(DATU(ICC,2)-DTHK(imlc,ie))*DIR
          if(DTHK(imlc,ie).lt.0.001)DTHK(imlc,ie)=0.001
        endif
      endif
      write (outs,'(a)') 'to:'
      call edisp(iuout,outs)
      write (outs,'(a,f9.4)') 'DTHK: ',DTHK(imlc,ie)
      call edisp(iuout,outs)

C Now update the MLC database.
      call edisp(iuout,'Updating the MLC database...')
      CALL EMKAMLD2(iuout,IER) ! write it out
      RETURN
      END


C In esrubld/sensit
C      elseif (IDTYPU(IACTD(IACT,1)).eq.4) then 
C        call UAE04(IZONE,IACT,DIR)  ! casual gains or scheduled air
C UCASAE04 edits casual gains and scheduled air movement in zone files.

      SUBROUTINE UCASAE04(IZONE,IACT,DIR)
      include "building.h"
      include "model.h"
      include "schedule.h"
C Common blocks UA2 UA3 UA4 within uncertainty.h
      include "uncertainty.h"
      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: iuout,iuin

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32

      character uhphrase*42,outs*124
      logical XST

C Data stored in UA04 has the following meaning in this routine:
C IDMAT(?,1): Casual gain type(1/2/3)/ scheduled air flow(0)
C IDMAT(?,2): Cas Gn: sensible(1)/ latent(2)/ radiant(3)/ convective(4)/
C IDMAT(?,2):         total Q (same split) (5)/ Period start (6)/
C IDMAT(?,2):         N/A (7)/ 
C IDMAT(?,2):         same total frac diff split (8)
C IDMAT(?,2): Sched Air flow: infiltration(1)/ ventilation(2)/
C IDMAT(?,2):         N/A (3)/ vent source temp(4)
C IDMAT(?,3): Change type (%; +/-; explicit).
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

C Edit casual gains.
          if (IDMAT(ICC,1).eq.0) then

C Edit scheduled air flows.
C Check that we have the right element(period). << TO DO >>
C Currently changing all defined periods.
            ICF=IDMAT(ICC,3)
            DAT1=DATU(ICC,1)
            DAT2=DATU(ICC,2)
            write (6,*) 'Editing scheduled air flow ',ICF,DAT1,DAT2

C Make specific header string for each change (to match urunvals)
            lncng=lnblnk(LCNG(ICC))
            lnloc=lnblnk(LLOC(ICL))
            if(lncng.gt.18)lncng=18
            uhphrase=' '
            write(uhphrase,'(4a,i2.2,a)')LCNG(ICC)(1:lncng),':',
     &        LLOC(ICL)(1:lnloc),':',IZ,','
            luhphrase=lnblnk(uhphrase)
            write(outs,'(2a)')'UCASAE04 Implementing ',
     &        uhphrase(1:luhphrase)
            call edisp(iuout,outs)

C Percentage change. 
C What data type to change? (see comments at start of routine).
C Chance all instances of this type - will probably add data 
C to specify individual periods later.
            if (IDMAT(ICC,2).eq.1) then
              write (outs,*) 'Zone:',IZONE,' infiltration'
              call edisp(iuout,outs)
              do 100 IX=1,IMP2
                DO 4010 IDTY=1,NBDAYTYPE
                  T=ACI(IDTY,IX)
                  call UAEDIT(ACI(IDTY,IX),ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                  write(outs,*)'Period ',CALENTAG(IDTY),IX,' From:',T,
     &            ' to ',ACI(IDTY,IX)
                  call edisp(iuout,outs)
 4010           CONTINUE
 100          continue
            elseif (IDMAT(ICC,2).eq.2) then
              write (outs,*) 'Zone:',IZONE,' ventilation'
              call edisp(iuout,outs)
              do 101 IX=1,IMP2
                DO 4011 IDTY=1,NBDAYTYPE
                  T=ACV(IDTY,IX)
                  call UAEDIT(ACV(IDTY,IX),ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                  write(outs,*)'Period ',CALENTAG(IDTY),IX,' From:',T,
     &            ' to ',ACV(IDTY,IX)
                  call edisp(iuout,outs)
 4011           CONTINUE
 101          continue
            elseif (IDMAT(ICC,2).eq.3) then
              write (6,*)'Illegal option, cannot change zone index'
            elseif (IDMAT(ICC,2).eq.4) then
              write (outs,*) 'Zone:',IZONE,' vent source temperature'
              call edisp(iuout,outs)
              do 104 IX=1,IMP2
                DO 4012 IDTY=1,NBDAYTYPE
                  T=TA(IDTY,IX)
                  call UAEDIT(TA(IDTY,IX),ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                  write(outs,*)'Period ',CALENTAG(IDTY),IX,' From:',T,
     &            ' to ',TA(IDTY,IX)
                  call edisp(iuout,outs)
 4012           CONTINUE
 104          continue
            endif

C Write zone operations file. If older than 21 upgrade.
            call edisp(iuout,'Updating zone operation file...')
            if(ip3ver(IZONE).lt.21) ip3ver(IZONE)=21
            CALL EMKOPER(IUO,LPROJ(IZONE),IZONE,IER)

          else

C Edit casual gains.
C Check that we have the right element(period).
C Currently changing all defined periods.
            write (6,*) 'Editing casual gains'
            ICF=IDMAT(ICC,3)
            DAT1=DATU(ICC,1)
            DAT2=DATU(ICC,2)
            IT=IDMAT(ICC,1)

C Make specific header string for each change (to match urunvals)
            lncng=lnblnk(LCNG(ICC))
            lnloc=lnblnk(LLOC(ICL))
            if(lncng.gt.18)lncng=18
            uhphrase=' '
            write(uhphrase,'(4a,i2.2,a)')LCNG(ICC)(1:lncng),':',
     &        LLOC(ICL)(1:lnloc),':',IZ,','
            luhphrase=lnblnk(uhphrase)
            write(outs,'(2a)') 'Implementing ',uhphrase(1:luhphrase)
            call edisp(iuout,outs)
      
C What data type to change? (see comments at start of routine).
C Chance all instances of this type - will probably add data 
C to specify incividual periods later.

C The next section has the following format:
C  Check casual gain parameter to edit (sensible, latent, convect %...
C  Check if required casual gain type is defined for this period (occ, lights..
C    note weekdays/ sat/ sun checked seperately.
C
            if (IDMAT(ICC,2).eq.1) then
              write (outs,*) 'Zone:',IZONE,' type:',IT,' sensible'
              call edisp(iuout,outs)
              do IX=1,IMP3
                do IDTY=1,NBDAYTYPE
                  if (IDMAT(ICC,1).eq.ICGT(IDTY,IX)) then
                    T1=CMGS(IDTY,IX)
                    call UAEDIT(CMGS(IDTY,IX),ICF,DAT1,DAT2,DIR,
     &                -1,0.0,0.0)
                    write (outs,*)'Period ',CALENTAG(IDTY),IX,
     &                ' From:',T1,' to ',CMGS(IDTY,IX)
                    call edisp(iuout,outs)
                  endif
                enddo
              enddo
            elseif (IDMAT(ICC,2).eq.2) then
              write (outs,*) 'Zone:',IZONE,' type:',IT,' latent'
              call edisp(iuout,outs)
              do IX=1,IMP3
                do IDTY=1,NBDAYTYPE
                  if (IDMAT(ICC,1).eq.ICGT(IDTY,IX)) then
                    T1=CMGL(IDTY,IX)
                    call UAEDIT(CMGL(IDTY,IX),ICF,DAT1,DAT2,DIR,
     &                -1,0.0,0.0)
                    write (outs,*)'Period ',CALENTAG(IDTY),IX,
     &                ' From:',T1,' to ',CMGL(IDTY,IX)
                    call edisp(iuout,outs)
                  endif
                enddo
              enddo
            elseif (IDMAT(ICC,2).eq.3) then
              write (outs,*) 'Zone:',IZONE,' type:',IT,' rad fraction'
              call edisp(iuout,outs)
              do IX=1,IMP3
                do IDTY=1,NBDAYTYPE
                  if (IDMAT(ICC,1).eq.ICGT(IDTY,IX)) then
                    T1=RADC(IDTY,IX)
                    call UAEDIT(RADC(IDTY,IX),ICF,DAT1,DAT2,DIR,
     &                0,0.0,1.0)
                    write (outs,*)'Period ',CALENTAG(IDTY),IX,
     &                ' From:',T1,' to ',RADC(IDTY,IX)
                    CONC(IDTY,IX)=1.0-RADC(IDTY,IX)  ! adjust CONC
                    call edisp(iuout,outs)
                  endif
                enddo
              enddo
            elseif (IDMAT(ICC,2).eq.4) then
              write (outs,*)'Zone:',IZONE,' type:',IT,' conv fraction'
              call edisp(iuout,outs)
              do IX=1,IMP3
                do IDTY=1,NBDAYTYPE
                  if (IDMAT(ICC,1).eq.ICGT(IDTY,IX)) then
                    T1=CONC(IDTY,IX)
                    call UAEDIT(CONC(IDTY,IX),ICF,DAT1,DAT2,DIR,
     &                0,0.0,1.0)
                    write (outs,*)'Period ',CALENTAG(IDTY),IX,
     &                ' From:',T1,' to ',CONC(IDTY,IX)
                    call edisp(iuout,outs)
                    RADC(IDTY,IX)=1.0-CONC(IDTY,IX)  ! adjust RADC
                  endif
                enddo
              enddo
            elseif (IDMAT(ICC,2).eq.5) then
              write (outs,*) 'Zone:',IZONE,' type:',IT,' total Q'
              call edisp(iuout,outs)
              do IX=1,IMP3
                do IDTY=1,NBDAYTYPE
                  if (IDMAT(ICC,1).eq.ICGT(IDTY,IX)) then
                    T1=CMGS(IDTY,IX)
                    T2=CMGL(IDTY,IX)
                    T3=T1+T2
                    call UAEDIT(T3,ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                    CMGS(IDTY,IX)=T3*(CMGS(IDTY,IX)/(T1+T2))
                    CMGL(IDTY,IX)=T3*(CMGL(IDTY,IX)/(T1+T2))
                    write (outs,*)'Period ',CALENTAG(IDTY),IX,
     &                ' Sensible from:',T1,' to ',CMGS(IDTY,IX)
                    call edisp(iuout,outs)
                    write (outs,*)'Period ',CALENTAG(IDTY),IX,
     &                ' Latent from:',T2,' to ',CMGL(IDTY,IX)
                    call edisp(iuout,outs)
                  endif
                enddo
              enddo
            elseif (IDMAT(ICC,2).eq.6) then

C Shift the start time of subsequent periods.
              write (outs,*) 'Zone:',IZONE,' type:',IT,' start'
              call edisp(iuout,outs)
              do IX=1,IMP3
                do IDTY=1,NBDAYTYPE
                  if (IDMAT(ICC,1).eq.ICGT(IDTY,IX)) then
                    if(IX.gt.1)then  ! no need to do it for initial period
                      IT1=ICGS(IDTY,IX)
                      if (DIR.gt.0.) then
                        if(ICGS(IDTY,IX)+1.lt.ICGS(IDTY,IX+1))then
                          ICGS(IDTY,IX)=ICGS(IDTY,IX)+1      ! if clear to shift later
                          ICGF(IDTY,IX-1)=ICGS(IDTY,IX-1)+1  ! and shift prior fn
                        endif
                      else
                        if(ICGS(IDTY,IX)-1.gt.ICGS(IDTY,IX-1))then
                          ICGS(IDTY,IX)=ICGS(IDTY,IX)-1      ! if clear to shift earlier
                          ICGF(IDTY,IX-1)=ICGS(IDTY,IX-1)-1  ! and shift prior fn
                        endif
                      endif
                      write (outs,*)'Period ',CALENTAG(IDTY),IX,
     &                  ' From:',IT1,' to ',ICGS(IDTY,IX)
                      call edisp(iuout,outs)
                    endif
                  endif
                enddo
              enddo
            elseif (IDMAT(ICC,2).eq.8) then
              IT=IDMAT(ICC,1)
              write(outs,*)'Zone:',IZONE,' type:',IT,' rad/conv split'
              call edisp(iuout,outs)
              do IX=1,IMP3
                do IDTY=1,NBDAYTYPE
                  if (IDMAT(ICC,1).eq.ICGT(IDTY,IX)) then
                    T1=RADC(IDTY,IX)
                    T2=CONC(IDTY,IX)
                    T3=T1+T2
                    call UAEDIT(CONC(IDTY,IX),ICF,DAT1,DAT2,DIR,
     &                0,0.0,T3)
                    RADC(IDTY,IX)=T3-CONC(IDTY,IX)
                    write (outs,*)'Period ',CALENTAG(IDTY),IX,
     &                ' Rad fraction from:',T1,' to ',RADC(IDTY,IX)
                    call edisp(iuout,outs)
                    write (outs,*)'Period ',CALENTAG(IDTY),IX,
     &                ' Conv fraction from:',T2,' to ',CONC(IDTY,IX)
                    call edisp(iuout,outs)
                  endif
                enddo
              enddo
            else
              write (6,*)'Editing request unknown.'
            endif

C Write zone operations file.
            call edisp(iuout,'Updating zone operation file...')
            if(ip3ver(IZONE).lt.21) ip3ver(IZONE)=21
            CALL EMKOPER(IUO,LPROJ(IZONE),IZONE,IER)
          endif  ! of editing casual gains
        endif    ! of matching zone
 10   continue

      RETURN
      END

C Copy of block of code from esrubld/sensit.F
C ******************** UAEDIT ********************
C UAEDIT edits the given data item (VAL) depending on change flag (ICFLAG)
C and given direction or magnitude (DIR).
C ICHK: -1 check min val, +1 check max val, 0 check both vals
      SUBROUTINE UAEDIT (VAL,ICFLAG,UPval,DOWNval,DIR,ichk,vmin,vmax)
#include "building.h"

      if (ICFLAG.eq.1) then

C Percentage change. 
        VAL=VAL*(((UPval/100.)*DIR)+1.0)
      elseif (ICFLAG.eq.2) then

C Absolute change. 
        VAL=VAL+(UPval*DIR)
      elseif (ICFLAG.eq.3) then

C Explicit change. 
        if (DIR.gt.0.) then
          VAL=VAL+((UPval-VAL)*DIR/3.)
        else
          VAL=VAL-((DOWNval-VAL)*DIR/3.)
        endif
      endif
      
C Check limits.
      if (ICHK.le.0) then
        VAL=max(VAL,vmin)
      endif
      if (ICHK.ge.0) then
        VAL=min(VAL,vmax)
      endif
      RETURN
      END

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
      subroutine UMATAE01(IZONE,IACT,DIR)  ! thermophysical properties
#include "building.h"
#include "model.h"
#include "geometry.h"
C Common blocks UA2 UA3 UA4 within uncertainty.h
#include "uncertainty.h"
#include "esprdbfile.h"
#include "material.h"
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: iuout,iuin
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
C IDMAT(?,1): Materials array index.
C IDMAT(?,2): Thermophysical property (con; den; sp ht).
C IDMAT(?,3): Change type (%; +/-; explicit).
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
      write(outs,'(2a)') 'UMATAE01 Implementing ',uhphrase(1:luhphrase)
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
C IDMAT is the material array, use matlegindex to find the
C raw IPR within the materials database in order to pass
C to chkprim (which returns the layers which are made of 
C the required material (if any).
           ip=matlegindex(IDMAT(ICC,1))
           write(6,*) 'idmat matlegindex',IDMAT(ICC,1),ip
           call CHKPRIM(IZ,IS,IP,NEL,IEL,IMLC)
           if (NEL.gt.0) then
             do 30 IEindex=1,NEL
               IE=IEL(IEindex)

C Trace.
               write(6,*)'Matching layer:'
               write(6,*)'Zone: ',IZ,' Surface: ',IS,' layer',IE
               write(6,*)'in MLC ',IMLC,' ',mlcname(IMLC),LAYERS(IMLC)

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

C Make a copy of the MLC, name it with _cal and copy across
C its attributes and update the relevant layer with new material.
                 NMLC=NMLC+1
                 mlcdbitems=NMLC
                 nbpastmlc=nbpastmlc+1
                 pastmlc(nbpastmlc)=IMLC
                 write(6,*) 'making _cal of ',nbpastmlc,
     &             pastmlc(nbpastmlc)
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
C                     write(6,*) 'Another item for ',mlccatname(loop2),
C     &                 ' ',mlccatitems(loop2),' focus now ',IC
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
               write(6,'(f9.3,2i4,2a,i4)') 
     &           DTHK(IMLC,IE),IPRMAT(IMLC,IE),ip,
     &           matname(IPRMAT(IMLC,IE)),' next slot is ',inext

C If there is room in the materials database make a copy.
               if(.NOT.isadupmat.and.inext.le.600)then
                 iorig=IPRMAT(IMLC,IE)  ! remember original array index
                 ILNE=matdbitems+1
                 lnm=lnblnk(matname(iorig))
                 nbpastmat=nbpastmat+1  ! remember
                 pastmat(nbpastmat)=iorig
                 write(matname(ILNE),'(2a)') 
     &             matname(iorig)(1:lnm),'_cal'
                 write(t32,'(a)') matname(ILNE)
                 CALL EASKS(t32,'Name of calib material','confirm:',
     &             32,' ','material name',IER,nbhelp)
                 write(matname(ILNE),'(a)') t32(1:lnblnk(t32))
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

C Now depending on what needs to change update the relevant
C material attribute.
                 write(outs,*) 'from:'
                 call edisp(iuout,outs)
                 write(outs,'(3f9.3,4f5.2)')matdbcon(ILNE),
     &             matdbden(ILNE),matdbsht(ILNE),matdbine(ILNE),
     &             matdboute(ILNE),matdbina(ILNE),matdbouta(ILNE)
                 call edisp(iuout,outs)
                 if (IDMAT(ICC,2).eq.1) then
                   call UAEDIT(matdbcon(ILNE),IDMAT(ICC,3),DATU(ICC,1),
     &               DATU(ICC,2),DIR,-1,0.01,0.0)
                 elseif (IDMAT(ICC,2).eq.2) then
                   call UAEDIT(matdbden(ILNE),IDMAT(ICC,3),DATU(ICC,1),
     &               DATU(ICC,2),DIR,0,0.01,9000.0)
                 elseif (IDMAT(ICC,2).eq.3) then
                   call UAEDIT(matdbsht(ILNE),IDMAT(ICC,3),DATU(ICC,1),
     &               DATU(ICC,2),DIR,0,0.01,3000.0)
                 elseif (IDMAT(ICC,2).eq.4) then
                   call UAEDIT(matdboute(ILNE),IDMAT(ICC,3),DATU(ICC,1),
     &               DATU(ICC,2),DIR,0,0.001,0.999)
                   call UAEDIT(matdbine(ILNE),IDMAT(ICC,3),DATU(ICC,1),
     &               DATU(ICC,2),DIR,0,0.001,0.999)
                 elseif (IDMAT(ICC,2).eq.5) then
                   call UAEDIT(matdbouta(ILNE),IDMAT(ICC,3),DATU(ICC,1),
     &               DATU(ICC,2),DIR,0,0.001,0.999)
                   call UAEDIT(matdbina(ILNE),IDMAT(ICC,3),DATU(ICC,1),
     &               DATU(ICC,2),DIR,0,0.001,0.999)
                 endif

                 write(outs,*) 'to:'
                 call edisp(iuout,outs)
                 write(outs,'(3f9.3,4f5.2)')matdbcon(ILNE),
     &             matdbden(ILNE),matdbsht(ILNE),matdbine(ILNE),
     &             matdboute(ILNE),matdbina(ILNE),matdbouta(ILNE)
                 call edisp(iuout,outs)

C And update the materials database. Following code is
C similar to that in edcondb.F
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

C The surface should point to an alternative MLC which uses
C the _cal material. And write out the zone geometry file.
           write(SMLCN(IS),'(a)') mlcname(NMLC)
           write(SSMLCN(ioc),'(a)') mlcname(NMLC)
           call geowrite(IFIL+1,LGEOM(IZ),IZ,iuout,3,IER)

 20      continue  ! of associated surfaces
       endif
 10   continue  ! of associated zones
      RETURN
      END


C Variant of CHKPRIM in esrubld/sensit.F
C ******************** CHKPRIM ********************
C CHKPRIM scans the layers of the MLC for a specific surface
C and returns if and where a specific material is used.
      SUBROUTINE CHKPRIM(IZONE,ISURF,IPRIM,NEL,IEL,IMLC)
#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"

C Passed parameters:
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
      call matchmlcdesc(SSMLCN(ioc),i)
      IMLC=i
      do 20 J=1,LAYERS(IMLC)
        if (IPR(IMLC,J).eq.IPRIM) then
          NEL=NEL+1   ! matches criteria
          IEL(NEL)=J  ! layer it was found in
        endif
 20   continue

      RETURN
      END

C UCTLAT01 Identifies control data should be changed for calibration.
      SUBROUTINE UCTLAT01(IACT,DIR)
#include "building.h"
C Common blocks UA3 UA4 within uncertainty.h
#include "uncertainty.h"
#include "control.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/UA32/IMISCD(MNCNG,2)
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
C IDMAT(?,1) (ICF): Control function reference number.
C IDMAT(?,2) (ICD): Control function day type.
C IMISCD(?,1)(ICP): Control function period.
C IMISCD(?,2): Uncertain parameter.
C IDMAT(?,3): Change type (%; +/-; explicit).
      ICC=IACTD(IACT,1)
      ICL=IACTD(IACT,2)
      ICF=IDMAT(ICC,1)
      ICD=IDMAT(ICC,2)
      ICP=IMISCD(ICC,1)
      itopic= IMISCD(ICC,2)  ! which of the 7 topics

C Make specific header string for each change (to match urunvals)
      lncng=lnblnk(LCNG(ICC))
      lnloc=lnblnk(LLOC(ICL))
      uhphrase=' '
      write(uhphrase,'(4a,i2.2,a,i2.2,a)')LCNG(ICC)(1:lncng),
     &  ':',LLOC(ICL)(1:lnloc),':',ICF,':',ICP,','
      luhphrase=lnblnk(uhphrase)
      write(outs,'(2a)') 'UCTLAT01 Implement ',uhphrase(1:luhphrase)
      call edisp(iuout,outs)

      ITEM(1) ='Period start time'
      ITEM(2) ='Maximum heating flux'
      ITEM(3) ='Minimum heating flux'
      ITEM(4) ='Maximum cooling flux'
      ITEM(5) ='Minimum cooling flux'
      ITEM(6) ='Heating set point'
      ITEM(7) ='Cooling set point'

      write (6,*) '   '
      write (6,*) ITEM(itopic),itopic,' will be edited.'

      write (6,*) ' IDMAT: ctl func & day type & change typ',
     &  (IDMAT(ICC,IX),IX=1,3)
      write (6,*) ' IMISCD: func period & parameter',
     &  (IMISCD(ICC,IX),IX=1,2)
      write (6,*) ' DATU: ',(DATU(ICC,IX),IX=1,2)

C UACTL (?,1)=change type (%,+/-); (?,2)=magnitude; (?,3)=associated action;
C UACTL (?,4)=time status (0 not active, 1 active)
C UACTL (?,5)=counter for the uncertainty action being dealt with
C UACTL (?,6)=value of DIR
      UACTL(itopic,1)=float(IDMAT(ICC,3))
      UACTL(itopic,5)=float(IACT)  ! which of the uncertainty tasks
      if (IDMAT(ICC,3).eq.1) then
        UACTL(itopic,2)=((DATU(ICC,1)/100.)*DIR)+1.0
      elseif (IDMAT(ICC,3).eq.2) then
        UACTL(itopic,2)=DATU(ICC,1)*DIR
      else
        UACTL(itopic,2)=0.
      endif
      UACTL(itopic,3)=IACT
      UACTL(itopic,6)=0.  ! slot for DIR value during simulation
      write (6,'(a,7f8.3)') ' UACTL: chg type ',(UACTL(IX,1),IX=1,7)
      write (6,'(a,7f8.3)') ' UACTL: value    ',(UACTL(IX,2),IX=1,7)
      write (6,'(a,7f8.3)') ' UACTL: ',(UACTL(IX,3),IX=1,7)
      write (6,'(a,7f8.3)') ' UACTL: time stat',(UACTL(IX,4),IX=1,7)
      write (6,'(a,7f8.3)') ' UACTL: iact     ',(UACTL(IX,5),IX=1,7)
      write (6,'(a,7f8.3)') ' UACTL: DIR      ',(UACTL(IX,6),IX=1,7)

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
        ICF=IDMAT(ICC,1)
        ICD=IDMAT(ICC,2)
        ICP=IMISCD(ICC,1)
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
                        write (36,*)ITEM(I)(1:lnblnk(ITEM(I))),
     &                    tbcps(ICF,ICD,ICP)
                      else
                        write (36,*)ITEM(I)(1:lnblnk(ITEM(I))),
     &                    bmiscd(ICF,ICD,ICP,I)
                      endif
                    endif

C Uncertainty defined and in valid time period.
                    if (nint(UACTL(I,1)).eq.1) then ! % change
                      PCNG=UACTL(I,2)
                      if (I.eq.1) then  ! the period
                        tbcps(ICF,ICD,ICP)=tbcps(ICF,ICD,ICP)*PCNG
                      else
                        bmiscd(ICF,ICD,ICP,I)=bmiscd(ICF,ICD,ICP,I)*PCNG
                      endif
                    elseif (nint(UACTL(I,1)).eq.2) then  ! abs change
                      ACNG=UACTL(I,2)
                      if (I.eq.1) then  ! the period
                        tbcps(ICF,ICD,ICP)=tbcps(ICF,ICD,ICP)+ACNG
                      else
                        bmiscd(ICF,ICD,ICP,I)=bmiscd(ICF,ICD,ICP,I)+ACNG
                      endif
                    endif
                    if (nint(UACTL(I,1)).gt.0) then   ! echo the updated value
                      if(I.eq.1)then
                write (6,*)ITEM(I)(1:lnblnk(ITEM(I))),tbcps(ICF,ICD,ICP)
                      else
                write (6,*)ITEM(I)(1:lnblnk(ITEM(I))),
     &                   bmiscd(ICF,ICD,ICP,I)
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
      call edisp(iuout,'Updating zone control file.')
      CALL CTLWRT(ICTLF,IER)

      return
      end

C ******************** UATFLG ********************
C UATFLG determines if control data is within NTG start day/time and
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

C Return to editing routine.

      RETURN
      END
