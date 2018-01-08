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

C Add entry.
C If error 101 then do not save the new entry.
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
      ITEM(6) ='   Optical properties (N/A)  '
      ITEM(7) ='   Ideal controls (N/A)'
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
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
C Common blocks UA1 UA2 within uncertainty.h
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON      
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/UA2T/NTLOC(MNIL),NTG(MNIL,4)

      DIMENSION STMP(MS),IVALZ(MCOM),IVALS(MS)

      character t24*15
      CHARACTER STMP*12
      character outs*124

      integer idst,ihst,idft,ihft   ! for local editing.
      integer idf,ids,ihf,ihs
      integer icc,jjj    ! loops
      logical found,foundmat,chdb,modmlc

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='LOCDEF'  ! set for subroutine
      
C Set initial values.
      IERR=0
      IER=0
      ids=1; ihs=1; 
      idf=365; ihf=24

C Increment number of uncertainty locations or periods.
      NILOC=NILOC+1
      NZGU(NILOC)=0
      NTLOC(NILOC)=0

C First discover if the location is spatial or temporal
      helptopic='uncert_location'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKABC(' ',
     &  'In which domain is the uncertainty location restricted: ',
     &  'spatial','temporal (e.g. weather control)','both',IW,nbhelp)

      if (IW.eq.1.or.IW.eq.3) then

C Options a) surfaces using MLC, b) surfaces using material,
C c) all surfaces facing outside d) other
C Internal location => ask for zone, surface and layer.
C If only one zone then set NZNO and NZ accordingly. 
C Clear zone selected array first.
        CALL EASKABCD('Spatial options:','I ',
     &    'surfaces using MLC','surfaces using material',
     &    'surfaces facing outside','Other ',IWA,nbhelp)

C Clear arrays in all cases.
 5      do II=1,MCOM
          NZNOGU(NILOC,II)=0
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
          write(LLOC(NILOC),'(2a)') 'all_',mlcname(ISEL)(1:10)

C Scan all connections looking for this MLC.
          write (outs,'(2a)') 'scanning for ',mlcname(ISEL)
          call edisp(iuout,outs)
          do icc = 1, NCON
            if(ssmlcindex(icc).eq.ISEL)then

C If this zone referenced by IC1(icc) not yet registered add it.
C First the case of nothing known.
              if(NZGU(NILOC).eq.0)then
                NZGU(NILOC)=NZGU(NILOC)+1  ! another zone
                NZNOGU(NILOC,1)=IC1(icc)
                NSGU(NILOC,1)=NSGU(NILOC,1)+1
                NSNOG(NILOC,1,1)=IE1(icc)
              else

C The location array is partly filled.
                found=.false.   ! do we know about this zone
                do izz=1,NZGU(NILOC)
                  if(IC1(icc).eq.NZNOGU(NILOC,izz))then
                    found=.true.
                    kkk=izz  ! remember which NZNOGU
                  endif
                enddo
                if(.NOT.found)then  ! add it and the surface to the list
                  NZGU(NILOC)=NZGU(NILOC)+1  ! another zone
                  NZNOGU(NILOC,NZGU(NILOC))=IC1(icc)
                  NSGU(NILOC,NZGU(NILOC))=NSGU(NILOC,NZGU(NILOC))+1
                  jjj=NSGU(NILOC,NZGU(NILOC))
                  NSNOG(NILOC,NZGU(NILOC),jjj)=IE1(icc)
                else  ! add surface to the list
                  NSGU(NILOC,NZGU(NILOC))=NSGU(NILOC,NZGU(NILOC))+1
                  jjj=NSGU(NILOC,NZGU(NILOC))
                  NSNOG(NILOC,NZGU(NILOC),jjj)=IE1(icc)
                endif
              endif
            endif
          enddo
        elseif(IWA.eq.2)then

          call usrmsg('Not yet available',' ','W')
C Ask for the material.
          iwhich = 0
          CALL EPMENSV
          CALL ELISTMAT(iwhich,chdb,'-',matarrayindex,IER)
          CALL EPMENRC
          write(LLOC(NILOC),'(2a)') 'all_',matname(matarrayindex)(1:10)
  
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
              if(NZGU(NILOC).eq.0)then
                NZGU(NILOC)=NZGU(NILOC)+1  ! another zone
                NZNOGU(NILOC,1)=IC1(icc)
                NSGU(NILOC,1)=NSGU(NILOC,1)+1
                NSNOG(NILOC,1,1)=IE1(icc)
              else

C The location array is partly filled.
                found=.false.   ! do we know about this zone
                do izz=1,NZGU(NILOC)
                  if(IC1(icc).eq.NZNOGU(NILOC,izz))then
                    found=.true.
                    kkk=izz  ! remember which NZNOGU
                  endif
                enddo
                if(.NOT.found)then  ! add it and the surface to the list
                  NZGU(NILOC)=NZGU(NILOC)+1  ! another zone
                  NZNOGU(NILOC,NZGU(NILOC))=IC1(icc)
                  NSGU(NILOC,NZGU(NILOC))=NSGU(NILOC,NZGU(NILOC))+1
                  jjj=NSGU(NILOC,NZGU(NILOC))
                  NSNOG(NILOC,NZGU(NILOC),jjj)=IE1(icc)
                else  ! add surface to the list
                  NSGU(NILOC,NZGU(NILOC))=NSGU(NILOC,NZGU(NILOC))+1
                  jjj=NSGU(NILOC,NZGU(NILOC))
                  NSNOG(NILOC,NZGU(NILOC),jjj)=IE1(icc)
                endif
              endif
            endif
          enddo
        elseif(IWA.eq.3)then

C Scan all connections for outside facing surfaces.
          write(LLOC(NILOC),'(a)') 'all_facade'
          call edisp(iuout,'scanning for outside connections.')
          do icc = 1, NCON
            if(ICT(icc).eq.0)then

C If this zone referenced by IC1(icc) not yet registered add it.
C First the case of nothing known.
              if(NZGU(NILOC).eq.0)then
                NZGU(NILOC)=NZGU(NILOC)+1  ! another zone
                NZNOGU(NILOC,1)=IC1(icc)
                NSGU(NILOC,1)=NSGU(NILOC,1)+1
                NSNOG(NILOC,1,1)=IE1(icc)
              else

C The location array is partly filled.
                found=.false.   ! do we know about this zone
                do izz=1,NZGU(NILOC)
                  if(IC1(icc).eq.NZNOGU(NILOC,izz))then
                    found=.true.
                    kkk=izz  ! remember which NZNOGU
                  endif
                enddo
                if(.NOT.found)then  ! add it and the surface to the list
                  NZGU(NILOC)=NZGU(NILOC)+1  ! another zone
                  NZNOGU(NILOC,NZGU(NILOC))=IC1(icc)
                  NSGU(NILOC,NZGU(NILOC))=NSGU(NILOC,NZGU(NILOC))+1
                  jjj=NSGU(NILOC,NZGU(NILOC))
                  NSNOG(NILOC,NZGU(NILOC),jjj)=IE1(icc)
                else  ! add surface to the list
                  NSGU(NILOC,NZGU(NILOC))=NSGU(NILOC,NZGU(NILOC))+1
                  jjj=NSGU(NILOC,NZGU(NILOC))
                  NSNOG(NILOC,NZGU(NILOC),jjj)=IE1(icc)
                endif
              endif
            endif
          enddo

        elseif(IWA.eq.4)then
          if(NCOMP.EQ.1)then
            call edisp(iuout,
     &      'Single zone model, therefore zone 1 selected.')
            call edisp(iuout,'  ')
            NZGU(NILOC)=1
            NZNOGU(NILOC,1)=1
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
              NILOC=NILOC-1
              return
            endif
            NZGU(NILOC)=INPIC
            do I=1,INPIC
              NZNOGU(NILOC,I)=IVALZ(I)
            enddo
          endif

C For each zone selected ask for the surfaces which the uncertainties 
C are applicable. 
          do 50 IZ=1,NZGU(NILOC)
            NSGU(NILOC,IZ)=0
            IZONE=NZNOGU(NILOC,IZ)

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
              NSNOG(NILOC,IZ,I)=0
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
              NILOC=NILOC-1
              return
            endif
            NSGU(NILOC,IZ)=INPIC
            do 512 I=1,INPIC
              NSNOG(NILOC,IZ,I)=IVALS(I)
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

C Temporal locations.
        NTLOC(NILOC)=-1
        idst=ids
        call EASKI(IDST,' ','Start day? ',1,'F',365,'F',9,
     &    'day of year',IERI,nbhelp)
        if(ieri.eq.-3) then
          NILOC=NILOC-1
          IERR=2
          return
        endif

        ihst=ihs
        call EASKI(IHST,' ','Start hour? ',1,'F',24,'F',1,
     &    'hour of day',IERI,nbhelp)
        if(ieri.eq.-3) then
          NILOC=NILOC-1
          IERR=2
          return
        endif

        idft=idf
        call EASKI(IDFT,' ','Finish day? ',1,'F',365,'F',15,
     &    'day of year',IERI,nbhelp)
        if(ieri.eq.-3) then
          NILOC=NILOC-1
          IERR=2
          return
        endif

        ihft=ihf
        call EASKI(IHFT,' ','Finish hour? ',1,'F',24,'F',24,
     &    'hour of day',IERI,nbhelp)
        if(ieri.eq.-3) then
          NILOC=NILOC-1
          IERR=2
          return
        endif

C Ok to instanciate the data and complete the editing.
        NTG(NILOC,1)=IDST
        NTG(NILOC,2)=IHST
        NTG(NILOC,3)=IDFT
        NTG(NILOC,4)=IHFT
      endif

C Provide id string name.
 99   t24=LLOC(NILOC)
      CALL EASKS(t24,' ','Name for location/period?',
     &  15,' ','Location/period name',IERR,nbhelp)
      if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
        LLOC(NILOC)=t24
      else
        goto 99
      endif
      return

 999  call EDISP(IUOUT,'  ')
      call EDISP(IUOUT,' Error in location specification! ')
      call EDISP(IUOUT,' Please try again. ')
      IERR=101
      NILOC=NILOC-1
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
        CALL ERMLDB(ITRC,ITRU,IER)
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

      logical DOK,OK
      logical browse
      logical havefile,unixok,LIBXST
      LOGICAL concat,remote,anymeasured
      logical domlc,dogeo,doopr  ! tasks to do
      character*72 tfile
      character longtfile*144,longtfiledos*144
      character tmode*8,doit*248
      character outs*124,fs*1,aut*12,key*1
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
      integer loopo,loop
      integer nbitem,items
      dimension items(MCOM)
      dimension ivalsg(MCOM)
      character APE*6,EXT*4
      character NNAME*144
      logical docnn

C dd is character array for selecting simulation parameter sets.
      character dd(MSPS+2)*55,brw*11
      
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
        WRITE(outs,'(i2,a,i2,8a,i2)')loop,':',itis,DBTAG(itis),
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
      domlc=.false.; dogeo=.false.; doopr=.false.
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
     &      ' requires a variant of material ',NAM
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
        endif
      enddo  ! of loop

C If materials and/or mlc call VER_MLC_MAT
      write(APE,'(a)') '_cal'
      if(domlc)then
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

C Write out configuration file
C But first change names of results libraries zones, flow, plant,
C moisture, electrical cfd and IPV
      if(domlc.or.dogeo)then
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
      call edisp(iuout,
     &  'The next task is to undertake calibration runs...')
      DOK=.true.
      helptopic='calibration_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ASKOK('Would you like to proceed with calibration?',
     &  ' ',OK,DOK,nbhelp)
      if(.NOT.OK) return

C Check if there are simulation parameter sets, if so user should
C select them before proceeding.
      if(nsset.gt.0)then

C Select an existing set or cancel.
 170    do ii=1, nsset
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
      
      return
      end

 
