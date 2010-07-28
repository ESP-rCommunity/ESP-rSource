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

C ***************** UNCERTA *********************
      subroutine UNCERTA(LUALF)
#include "building.h"
#include "uncertainty.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path

      COMMON/UA2/NILOC,NZG(MNIL),NZNOG(MNIL,MCOM),NSG(MNIL,MCOM),
     &             NSNOG(MNIL,MCOM,MS)
      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)
      COMMON/UA4/NIACT,IACTD(MNACT,2)

      CHARACTER*33 ITEM(11)
      character path*72,LUALF*72
      CHARACTER H*72,LTEMP*72,DTEMP*72,outs*124
      character cfgroot*24,LCFGF*72

      LOGICAL LIBXST

      IUALF=IFIL

C Ask for an uncertainty library name and check to see if it exists.
 4    IW=0
      if(path.ne.'./'.and.path.ne.' ')then
        write(outs,'(A,A)') ' The current path is: ',path
        call edisp(iuout,outs)
        CALL EASKAB(' You are working in a remote folder... ',
     &               ' place library:','using the path',
     &               'in local folder',IW,0)
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
     &                      ' ',72,DTEMP,'UA library',IER,0)
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
        NICNG=0
        NILOC=0
        NIACT=0
      endif


C Set up initial menu.
   10 INO=-2
      ITEM(1) ='a create default definitions'
      ITEM(2) =' --------------------------------'
      ITEM(3) ='b define/edit distributions'
      ITEM(4) ='c define/edit locations'
      ITEM(5) =' ------------------------------- '
      ITEM(6) ='d link distributions to locations'
      ITEM(7) =' ------------------------------- '
      ITEM(8) ='! list current uncertainties     '
      ITEM(9) ='> save uncertainties         '
      ITEM(10)='? help                 '
      ITEM(11)='- exit this menu       '
      NITMS=11

C Help text for this menu.
   12 H(1) ='Uncertainties are defined in a three stage process '
      H(2) ='analogous to mass flow.'
      H(3) ='  '
      H(4) ='1. A distribution for a chosen parameter is defined.  '
      H(5) ='   Minimum one distribution.'
      H(6) ='  '
      H(7) ='2. A physical location in the model is defined; zone,  '
      H(8) ='   surface etc. Minimum one location.'
      H(9) ='  '
      H(10)='3. Distributions and locations are linked to define an '
      H(11)='   uncertainty.  The validity of the uncertainty is '
      H(12)='   checked at simulation time. Minimum one uncertainty.'

      CALL EMENU('Uncertainty definition',ITEM,NITMS,INO)
      if (INO.EQ.NITMS) then

C Check if the library has been saved and closed.
        return
      elseif (INO.EQ.(NITMS-1)) then

C HELP!
        CALL PHELPD('Uncertainty definitions file',12,'-',0,0,IER)
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
#include "uncertainty.h"
C muadel should be equivalent to max(MNCNG,MNIL,MNACT))
      parameter (muadel=50)


      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      COMMON/UA2/NILOC,NZG(MNIL),NZNOG(MNIL,MCOM),NSG(MNIL,MCOM),
     &             NSNOG(MNIL,MCOM,MS)
      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)
      COMMON/UA4/NIACT,IACTD(MNACT,2)

      character LUALF*72,LCNG*15,LLOC*15,ta15(MNCNG)*15
      CHARACTER H*72,KEY*1,MLCITM(35)*24

      dimension IDEL(muadel)

      IER=0
      if (ITYP.lt.1.and.ITYP.gt.3) return

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
            WRITE(MLCITM(M),22)KEY,LCNG(M)(1:15)
          elseif (ITYP.eq.2) then 
            WRITE(MLCITM(M),22)KEY,LLOC(M)(1:15)
          elseif (ITYP.eq.3) then 
            WRITE(MLCITM(M),23)KEY,LCNG(IACTD(M,1))(1:7),':',
     &                             LLOC(IACTD(M,2))(1:7)
          endif
  22      FORMAT(A1,1X,A15)
  23      FORMAT(A1,1X,3a)
        ENDIF
  20  CONTINUE

C Number of actual items displayed.
      NITMS=M+MCTLX

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        MLCITM(M+1)=' -------------------- '
      ELSE
        WRITE(MLCITM(M+1),15)IPM,MPM 
   15   FORMAT   ('0 ---Page: ',I2,' of ',I2,' ---')
      ENDIF
      MLCITM(M+2)='+ add definition '
      MLCITM(M+3)='= delete definition '
      MLCITM(M+4)='! list current '
      MLCITM(M+5)='> update library '
      MLCITM(M+6)='? help     '
      MLCITM(M+7)='- exit this menu  '
      INO=-4

C Help text for this menu.
   2  H(1)='To create a new definition select + add entry. '
      H(2)='To edit an existing entry then select it.'
      H(3)='To remove an existing entry select = delete entry and'
      H(4)='  then the entry you wish to delete.'

      if (ITYP.eq.1) then
        CALL EMENU('Distributions',MLCITM,NITMS,INO)
      elseif (ITYP.eq.2) then
        CALL EMENU('Locations',MLCITM,NITMS,INO)
      elseif (ITYP.eq.3) then
        CALL EMENU('Uncertainties',MLCITM,NITMS,INO)
      endif

      IF(INO.EQ.NITMS)THEN
        return
      ELSEIF(INO.EQ.NITMS-1)THEN

C Produce help text for the menu.
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
          call EPICKS(INDEL,IDEL,' ',' ',15,NICNG,LCNG,
     &                                         'Distributions',IER,4)
          if (IDEL(1).gt.0) then
            do 700 ID=IDEL(1),NICNG
              LCNG(ID)=LCNG(ID+1)
              IDTYP(ID)=IDTYP(ID+1)
              IDMAT(ID,1)=IDMAT(ID+1,1)
              IDMAT(ID,2)=IDMAT(ID+1,2)
              IDMAT(ID,3)=IDMAT(ID+1,3)
              DAT(ID,1)=DAT(ID+1,1)
              DAT(ID,2)=DAT(ID+1,2)
 700        continue
            NICNG=NICNG-1
          endif
        elseif (ITYP.eq.2.and.NILOC.ge.1) then
          call EPICKS(INDEL,IDEL,' ',' ',15,NILOC,LLOC,
     &                                         'Locations',IER,4)
          if (IDEL(1).gt.0) then
            do 701 ID=IDEL(1),NILOC
              LLOC(ID)=LLOC(ID+1)
              NZG(ID)=NZG(ID+1)
              do 711 IDCOMP=1,MCOM
                NZNOG(ID,IDCOMP)=NZNOG(ID+1,IDCOMP)
                NSG(ID,IDCOMP)=NSG(ID+1,IDCOMP)
                do 721 IDSUR=1,MCOM
                  NSNOG(ID,IDCOMP,IDSUR)=NSNOG(ID+1,IDCOMP,IDSUR)
 721            continue
 711          continue
 701        continue
            NILOC=NILOC-1
          endif
        elseif (ITYP.eq.3.and.NIACT.ge.1) then
          do 799 I=1,NIACT
           write(ta15,'(2a)')LCNG(IACTD(I,1))(1:7),LLOC(IACTD(I,2))(1:7)
 799      continue
          call EPICKS(INDEL,IDEL,' ',' ',15,NIACT,ta15,
     &                                         'Uncertainties',IER,4)
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
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)

      CHARACTER H*72

      IERR=0
      IER=0

C Need controlling menu here - select uncertainty category.

C IDTYP = 3 : Climate parameters
C        IDTYP(ICDEF)=3
C        call SDDFLT003

C IDTYP = 1 : Materials properties
        call SDDFLT001

C IDTYP = 2 : Composite constructions
C        IDTYP(ICDEF)=2
C        call SDDFLT002

C IDTYP = 101 : Zone geometry
C        IDTYP(ICDEF)=101
C        call SDDFLT101

C IDTYP = 4 : Operations
C        IDTYP(ICDEF)=4
C        call SDDFLT004

C IDTYP = 5 : Convection coefficients
C        IDTYP(ICDEF)=5
C        call SDDFLT005


      return
      END

C ***************** CNGDEF *********************
C CNGDEF allows the user to define the distribution of an uncertainty.
C ICDEF  If equal to zero then add a new definition, else edit.

      subroutine CNGDEF(ICDEF,IERR)
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)

      character LUALF*72,LCNG*15,LLOC*15,t15*15
      CHARACTER H*72, ITEM(17)*40, outs*124

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
      ITEM(1) =' Databases ...'
      ITEM(2) ='a  Climate'
      ITEM(3) ='   Pressure (N/A)'
      ITEM(4) ='c  Construction elements'
      ITEM(5) ='d  Construction composites'
      ITEM(6) ='   Optical properties (N/A)'
      ITEM(7) ='   Temporal definitions (N/A)'
      ITEM(8) =' --------------------------- '
      ITEM(9) =' Zones composition ...'
      ITEM(10)='g  Geometry'
      ITEM(11)='   Boundary conditions (N/A)'
      ITEM(12)='i  Operations'
      ITEM(13)='j  Convection coefficients'
      ITEM(14)='   Casual gain control (N/A)'
      ITEM(15)=' --------------------------- '
      ITEM(16)='? Help '
      ITEM(17)='- Exit '
      NITMS=17

C Display current.
      if (IDTYP(ICDEF).gt.0) then
        write (outs,'(a,a)') ' Current category: ',ITEM(IDTYP(ICDEF))
        call edisp(iuout,outs)
      endif

C Help text for this menu.
  5   H(1)='Pick the required uncertainty category. You will then '
      H(2)='be asked further questions to define the uncertainty '
      H(3)='distribution.  '
      H(4)='   '
      H(5)='Exit returns to the main menu without saving any data. '

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
        CALL PHELPD('Uncertainty categories',5,'-',0,0,IER)
      elseif (INO.eq.2) then

C ** NOW SETTING  -IDTYP-  IN SDD routines
C IDTYP = 3 : Climate parameters
        IDTYP(ICDEF)=3
        call SDDEF003(ICDEF,IER)
      elseif (INO.eq.4) then

C IDTYP = 1 : Materials properties
        call SDDEF001(ICDEF,IER)
      elseif (INO.eq.5) then

C IDTYP = 2 : Composite constructions
        IDTYP(ICDEF)=2
        call SDDEF002(ICDEF,IER)
      elseif (INO.eq.10) then

C IDTYP = 101 : Zone geometry
        IDTYP(ICDEF)=101
        call SDDEF101(ICDEF,IER)
      elseif (INO.eq.12) then

C IDTYP = 4 : Operations
        IDTYP(ICDEF)=4
        call SDDEF004(ICDEF,IER)
      elseif (INO.eq.13) then

C IDTYP = 5 : Convection coefficients
        IDTYP(ICDEF)=5
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
      H(1)='A descriptive name (up to 15 characters) is required.'
      if (ICDEF.gt.ICDEFold) then
        continue
      elseif (ICDEF.gt.0) then 
        t15=LCNG(ICDEF)
        CALL EASKS(t15,' ',' Name for uncertainty ? ',
     &      15,LCNG(ICDEF),'Uncertainty name',IER,1)
        if(t15(1:2).ne.'  '.and.t15(1:4).ne.'UNKN')then
          LCNG(ICDEF)=t15
        endif
      else

C Passed icdef of zero i.e. new uncertainty
        t15=LCNG(1)
        CALL EASKS(t15,' ',' Name for uncertainty ? ',
     &      15,' ','Uncertainty name',IER,1)
        if(t15(1:2).ne.'  '.and.t15(1:4).ne.'UNKN')then
          LCNG(1)=t15
        endif
      endif

      return
      END

C ***************** LOCDEF *********************
C LOCDEF The location definition menu allows the user to define the 
C areas within the thermal model where uncertainties exist. 
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
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      COMMON/UA2/NILOC,NZG(MNIL),NZNOG(MNIL,MCOM),NSG(MNIL,MCOM),
     &             NSNOG(MNIL,MCOM,MS)
      COMMON/UA2T/NTLOC(MNIL),NTG(MNIL,4)

      DIMENSION STMP(MS),IVALZ(MCOM),IVALS(MS)

      character LUALF*72,LCNG*15,LLOC*15,t15*15
      CHARACTER H*72,STMP*12

      integer idst,ihst,idft,ihft   ! for local editing.
      integer idf,ids,ihf,ihs

C Set initial values.
      IERR=0
      IER=0
      ids=1
      ihs=1
      idf=365
      ihf=24

C Increment number of uncertainty locations.
      NILOC=NILOC+1
      NZG(NILOC)=0
      NTLOC(NILOC)=0

C First discover if the location is spatial or temporal
      H(1)='The location of the uncertainty can be:'
      H(2)=' - spatial and in all temporal locations.'
      H(3)=' - temporal and in all spatial locations.'
      H(4)=' - both.  Only use this option if both domains have'
      H(5)='   a restriction'
      H(6)=' '
      H(7)='If you select cancel (where a number is requested) '
      H(8)='the current model data will not be changed. '
      CALL EASKABC(' ',
     &    'In which domain is the uncertainty location restricted: ',
     &    'spatial','temporal','both',IW,8)

      if (IW.eq.1.or.IW.eq.3) then

C Internal location => ask for zone, surface and layer.
C If only one zone then set NZNO and NZ accordingly. 
C Clear zone selected array first.
 5      do 6 II=1,MCOM
          NZNOG(NILOC,II)=0
 6      continue
        if(NCOMP.EQ.1)then
          call edisp
     &        (iuout,' Single zone model, therefore zone 1 selected. ')
          call edisp(iuout,'  ')
          NZG(NILOC)=1
        else

C Otherwise select the zones from a list of zone names.
          call edisp(iuout,' Zone selection... ')
          call edisp(iuout,'  ')

          H(1)='Pick one, several or All zones for inclusion in a'
          H(2)='graph or tabular listing from the list provided. '
          H(3)='You may choose them in any order. '
          INPIC=NCOMP
          CALL EPICKS(INPIC,IVALZ,' ',' Which zones to include: ',
     &      12,NCOMP,ZNAME,' zone list',IERR,3)

C Check for errors.
          if (IERR.ne.0) goto 999

C If no zones have been selected then return to main SA menu.
          if (INPIC.eq.0) then
            call edisp(iuout,' Returning to main menu.... ')
            call edisp(iuout,
     &                ' * No data saved from location specification! ')
            IERR=101
            NILOC=NILOC-1
            return
          endif
          NZG(NILOC)=INPIC
          do 40 I=1,INPIC
            NZNOG(NILOC,I)=IVALZ(I)
 40       continue
        endif

C For each zone selected ask for the surfaces which the uncertainties 
C are applicable. 
        do 50 IZ=1,NZG(NILOC)
 51       NSG(NILOC,IZ)=0
          IZONE=NZNOG(NILOC,IZ)

          if(IZONE.GT.NCOMP.OR.IZONE.EQ.0)THEN
            call USRMSG('  ',' Zone number out of range!','W')
            IER=IER+1
            if (IER.ge.5) goto 999
            call edisp(iuout,' Please reselect zones ')
            goto 5
          endif

          INPIC=NZSUR(IZONE)
          do 511 I=1,INPIC
            icn1=izstocn(izone,i)
            STMP(I)=SSNAME(icn1)
            NSNOG(NILOC,IZ,I)=0
 511      continue
          call edisp(iuout,' Surface selection ')
          call edisp(iuout,'  ')
          H(1)=' Pick one, several or all surfaces for inclusion.'
          call EPICKS(INPIC,IVALS,' Which surfaces to include: ',
     &      ' (If surface selection not required choose * All) ',
     &      12,NZSUR(IZONE),STMP,ZNAME(IZONE),IERR,1)

C Check for errors.
          if (IERR.ne.0) goto 999

C If no surfaces have been selected then return to main SA menu.
          if (INPIC.eq.0) then
            call edisp(iuout,' Returning to main menu.... ')
            call edisp(iuout,
     &                ' * No data saved from location specification! ')
            IERR=101
            NILOC=NILOC-1
            return
          endif
          NSG(NILOC,IZ)=INPIC
          do 512 I=1,INPIC
            NSNOG(NILOC,IZ,I)=IVALS(I)
 512      continue

C For each surface selected in the current zone ask for the layers
C where the uncertainties are applicable. 
C * Currently I don't think that this is entirely necessary, but if 
C * in the future it is desired then this is the place to add layer 
C * selection.
 50     continue
      endif
      if (IW.eq.2.or.IW.eq.3) then

C Temporal locations.
        H(1)='The period when uncertainty is applicable requires '
        H(2)='starting and finishing dates. '
        H(3)='If you select cancel (where a number is requested) '
        H(4)='the current model data will not be changed. '
        NTLOC(NILOC)=-1
        idst=ids
        call EASKI(IDST,' ','Start day? ',1,'F',365,'F',9,
     &                'day of year',IERI,4)
        if(ieri.eq.-3) then
          NILOC=NILOC-1
          IERR=2
          return
        endif

        ihst=ihs
        call EASKI(IHST,' ','Start hour? ',1,'F',24,'F',1,
     &                'hour of day',IERI,4)
        if(ieri.eq.-3) then
          NILOC=NILOC-1
          IERR=2
          return
        endif

        idft=idf
        call EASKI(IDFT,' ','Finish day? ',1,'F',365,'F',15,
     &                'day of year',IERI,4)
        if(ieri.eq.-3) then
          NILOC=NILOC-1
          IERR=2
          return
        endif

        ihft=ihf
        call EASKI(IHFT,' ','Finish hour? ',1,'F',24,'F',24,
     &                'hour of day',IERI,4)
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
 99   H(1)='A descriptive name (up to 15 characters) is required.'
      t15=LLOC(NILOC)
      CALL EASKS(t15,' ',' Name for location ? ',
     &            15,' ','Location name',IERR,1)
      if(t15(1:2).ne.'  '.and.t15(1:4).ne.'UNKN')then
        LLOC(NILOC)=t15
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
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      COMMON/UA2/NILOC,NZG(MNIL),NZNOG(MNIL,MCOM),NSG(MNIL,MCOM),
     &             NSNOG(MNIL,MCOM,MS)
      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)
      COMMON/UA4/NIACT,IACTD(MNACT,2)

      character LUALF*72,LCNG*15,LLOC*15
      CHARACTER H*72

      DIMENSION IVALC(MNCNG),IVALL(MNIL)

      IERR=0
      IER=0
      if (IERR.ne.0)  goto 999

C Generate a list of changes and then locations so as the user can 
C define actions.
      H(1)='Pick the required uncertainty definition(s).'
      H(2)='  '
      H(3)='Exit returns to the main menu without saving any data. '
      INPIC=NICNG
      CALL EPICKS(INPIC,IVALC,' ',' Which uncertainty definition : ',
     &                       15,NICNG,LCNG,'Uncert. defn.',IERR,3)
      if (IERR.ne.0) goto 999

C If no type has been selected then return to main SA menu.
      if (INPIC.eq.0) then
        call edisp(iuout,' Returning to main menu.... ')
        call edisp(iuout,
     &                ' * No data saved from location specification! ')
        IERR=101
        return
      endif

      H(1)='Pick the required location definition.'
      H(2)='  '
      H(3)='Exit returns to the main menu without saving any data. '
      INPIL=NILOC
      CALL EPICKS(INPIL,IVALL,' ',' Which location definition : ',
     &                          15,NILOC,LLOC,' Location defs.',IERR,3)
      if (IERR.ne.0) goto 999

C If no type has been selected then return to main SA menu.
      if (INPIC.eq.0) then
        call edisp(iuout,' Returning to main menu.... ')
        call edisp(iuout,
     &                ' * No data saved from location specification! ')
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
C UPELM retrieves the unique materials used in the 
C esp-r model. This is done by copying all the materials used 
C into an array, then sorting them via a bubble search removing 
C any duplicates.

      subroutine UPELM(IPRIM,PRIMLIST,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"

C      COMMON/OUTIN/IUOUT,IUIN

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      INTEGER PRIMLIST(MMLC*ME)

C      CHARACTER MLCITM(35)*40,KEY*1,NAM*72,TITL*72

      LOGICAL CHANGED

      CHANGED=.FALSE.

C If the common block MLC has not yet been filled then read in the
C composite construction database.
      IF(.NOT.MLDBOK)THEN
        CALL ERMLDB(ITRC,ITRU,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG(' ',' Unable to display information','-')
          IER=IER+1
          RETURN
        ELSE
          MLDBOK=.TRUE.
        ENDIF
      ENDIF

C Search through the composites and store the materials db ref.
      IPRIM=0
      do 10, IMLC=1,NMLC
        do 20, ILAY=1,LAYERS(IMLC)
          IPRIM=IPRIM+1
          PRIMLIST(IPRIM)=IPR(IMLC,ILAY)
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
