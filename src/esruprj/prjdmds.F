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

C << missing functionality for dispersed demands:
C << a) facility to sort dispersed demand periods
C << b) facility to define sub-hour periods
C << c) it would be good to know overall floor area or to
C <<    define floor areas by selecting relevant zones.
C << d) there are no graphs of the gains

C << Figuring out these functions would indicate the resource
C << required to update zone casual gains to work with more day
C << types and casual gain types.

C -------- PRJDMDS ----------
C PRJDMDS: Creates and edits a dispersed demands file which holds
C information on building wide demands related to occupants, lighting,
C fans, pumps, lifts, domestic hot water.
C ITRC is the trace level, ITRU is the ouput channel, IUO is the
C file unit for the demands file. IER=0 is OK.
      SUBROUTINE PRJDMDS(ITRC,ITRU,IUO,IER)

#include "building.h"
#include "ipvdata.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

      common/C21/IFCFG,cfgroot,LCFGF
      common/user/browse
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

C Demands notes and file name.
      COMMON/BL1/dmdsdesc,bdmds

C Floor area (optionally) associated with each demand type.
      COMMON/BL2/dmdfla(7)

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)

C Support for up to 10 calendar day types
C NBCAS() number of gains on each day type.
C IBCGS(),IBCGF() - start and finish hours of each demand
C CBMGS(),CBMGL() - sensible and latent magnitude (in Watts) of each demand
C BRADC(),BCONC(), - radiant and convective portions of each demand
C IBCGT() - type of gain for each day type and period
      COMMON/BL3data/NBCAS(10),IBCGS(10,MC),IBCGF(10,MC),CBMGS(10,MC),
     &  CBMGL(10,MC),BRADC(10,MC),BCONC(10,MC),IBCGT(10,MC)

C idmdver is the version of dispersed demands.
C idmdver =0 standard, idmdver =1 with header
      common/b3ver/idmdver

C Descriptive label for each project demands type.
      common/bloadlabel/blodlabel(7)

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Flags noting whether casual gain periods are currently sorted.
      logical sorted,problem

      DIMENSION ITEM(13)
      CHARACTER dmdsdesc*248,tcname*248,bdmds*72
      CHARACTER ITEM*36,H*72,LTMP*72,GFILE*72
      CHARACTER*6 blodlabel
      character cfgroot*24,LCFGF*72,hold*60
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      character calename*32,calentag*12,calendayname*32
      LOGICAL OK,DOK,XST,MODOPR,browse

C GFILE is the default file name for project gains file.
      if(ctlpth(1:2).eq.'  '.or.ctlpth(1:2).eq.'./')then
        WRITE(GFILE,'(A,A5)') cfgroot(1:lnblnk(cfgroot)),'.dmds'
      else
        WRITE(GFILE,'(3A,A5)') ctlpth(1:lnblnk(ctlpth)),'/',
     &    cfgroot(1:lnblnk(cfgroot)),'.dmds'
      endif
      if(bdmds(1:7).eq.'UNKNOWN'.or.bdmds(1:2).eq.'  ')then
        bdmds=GFILE
      endif

C Initially assume periods are not sorted.
      sorted=.false.
      MODOPR=.FALSE.

C If the model calendar has not yet been setup confirm what to
C do with the user.
      if(nbdaytype.eq.0)then
        h(1)='Each model has a master calendar which indicates the'
        h(2)='day type for each Julian day (initially based on the'
        h(3)='year and a basic Weekday Saturday Sunday scheme. '
        h(4)=' '
        h(5)='No calendar has been defined so if you want the basic '
        h(6)='Weekday Saturday Sunday scheme it will be created. If '
        h(7)='you want to setup a more specific calendar then you '
        h(8)='should do that first. '
        CALL EASKABC('Calendar options','(see help) :',
     &    'accept basic calendar','setup custom calendar','abort',IW,8)
        if(IW.eq.1)then
          call calenmanage('i',ier)
          call edisp(iuout,'A reminder of current day types...')
          loop=4
          loopst=1
          if(MMOD.eq.8)then
            call calenprint(iuout,'g',iyear,loopst,loop)
          else
            call calenprint(iuout,'t',iyear,loopst,loop)
          endif
        elseif(IW.eq.2)then
          call usrmsg('Go to the Model context -> calendar option',
     &      'and when the calendar is defined return here.','W')
          return
        elseif(IW.eq.3)then
          return
        endif
      else

C List out the initial months of the calendar.
        call edisp(iuout,'A reminder of current day types...')
        loop=4
        loopst=1
        if(MMOD.eq.8)then
          call calenprint(iuout,'g',iyear,loopst,loop)
        else
          call calenprint(iuout,'t',iyear,loopst,loop)
        endif
      endif
        
C Clear all of the possible day types.
      do 12 idt = 1,10
        NBCAS(idt)=0
  12  continue

      iwss=1     ! assume weekdays initially
      dmdsdesc='no dispersed demands notes (yet)'
      IER=0

C Set the default demand type labels
      blodlabel(1)='Occup '
      dmdfla(1)= 1.0
      blodlabel(2)='Lights'
      dmdfla(2)= 1.0
      blodlabel(3)='SmPowr'      
      dmdfla(3)= 1.0
      blodlabel(4)='Fans  '
      dmdfla(4)= 1.0
      blodlabel(5)='Pumps '
      dmdfla(5)= 1.0
      blodlabel(6)='Lifts '      
      dmdfla(6)= 1.0
      blodlabel(7)='DHW   '      
      dmdfla(7)= 1.0

C Initial IPV demand inclusion for each type set to zero.
      do 343 ij=1,7
        idmdinc(ij)=0
 343  continue

C Read file for editing or listing. If there is no file then begin with
C default data.
      H(1)='A project demands file contains project level info'
      H(2)='on fans, pumps, lifts, DHW demands which may '
      H(3)='be applied to an integrated performance view. '
      ltmp=BDMDS
      CALL EASKS(ltmp,' Project demands file name: ',
     &      ' ',72,GFILE,'proj demands file',IER,3)
      if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
        BDMDS=ltmp
      endif
      IUO=IFIL+1
      XST=.FALSE.
      call FINDFIL(bdmds,XST)
      IF(XST)THEN
        CALL ERPFREE(IUO,ISTAT)
        CALL ERBDMD(ITRC,ITRU,IUO,IER)

C Do cursory check to see if the file is sorted.
        sorted=.true.
        problem=.false.
        call dmdchecksort(1,problem,ier)
        if(problem)then
          sorted=.false.
          call edisp(iuout,
     &      'Weekday demands might be unsorted or not span whole day.')
        endif
        problem=.false.
        call dmdchecksort(2,problem,ier)
        if(problem)then
          sorted=.false.
          call edisp(iuout,
     &      'Saturday demands might be unsorted or not span whole day.')
        endif
        problem=.false.
        call dmdchecksort(3,problem,ier)
        if(problem)then
          sorted=.false.
          call edisp(iuout,
     &      'Sunday demands might be unsorted or not span whole day.')
        endif
      ELSE

C << place for future user choices >>
        idmdver=1
      ENDIF

  20  write(ITEM(1),'(A,A25)')'a notes: ',dmdsdesc(1:25)
      ITEM(2) =               'b type associated base areas     '
      ITEM(4) =               '  _________________________      '
      ITEM(5)=                'c edit project demands           '
      ITEM(6)=                'd zeroise project demands        '
      if(iwss.eq.1)then
        write(item(3),'(3a)') 'b focus on >> ',calentag(iwss),'        '
        ITEM(7)=              '                                 '
      elseif(iwss.gt.1)then
        write(item(3),'(3a)') 'b focus on >> ',calentag(iwss),'        '
        write(item(7),'(3a)') 'e copy ',calentag(iwss-1),' demands     '
      endif
      ITEM(8)=                '  _________________________      '
      ITEM(9)=                'l list project demands   '
      ITEM(10)=               '  _________________________      '
      ITEM(11)=               '> save                           '
      ITEM(12)=               '? help                           '
      ITEM(13)=               '- exit this menu                 '
      NITEMS=13

C Help text for this menu.
      H(1) ='Dispersed demands are casual gains which are not'
      H(2) ='associated with a specific zone in the model.'
      H(3) ='Examples are fans and lifts and their electrical'
      H(4) ='requirements are often taken into account in '
      H(5) ='reports on building energy use. '
      H(6) =' '
      H(7) ='This facility allows you to define schedules for'
      H(8) ='seven types of dispersed gains either as W/m2 '
      H(9) ='or in absolute W. '
      H(10)=' '
      H(11)='If the calendar of the model (see Model context)'
      H(12)='includes the standard Weekday Saturday Sunday day '
      H(13)='types then you may work at that resolution. If you'
      H(14)='have also defined holidays then dispersed gains can '
      H(15)='also be assigned (UNDER TESTING) '
      H(16)=' '
      H(17)=' '
      H(18)='The data requirements are summarized below :- '
      H(19)=' 1) Description '
      H(20)=' 2) For each calendar day type e.g. Weekday, Saturday,'
      H(21)='    Sunday, holiday (etc.) demands data:'
      H(22)='       Start and finish hours for each period'
      H(23)='       Casual gain type:'
      H(24)='         type 1: usually occupant demands'
      H(25)='         type 2: usually lighting demands'
      H(26)='         type 3: usually small power demands'
      H(27)='         type 4: usually fan demands'
      H(28)='         type 5: usually pump demands'
      H(29)='         type 6: usually lift demands'
      H(30)='         type 7: usually DHW demands'
      H(31)='       Sensible and Latent magnitudes'
      H(32)='       Radiant and Convective portions'

      INO=-2
30    CALL EMENU('Project demands',ITEM,NITEMS,INO)

      IF(INO.EQ.NITEMS)THEN

C If not sorted then warn the user prior to exiting.
        sorted=.true.
        problem=.false.
        call dmdchecksort(1,problem,ier)
        if(problem)then
          sorted=.false.
          call edisp(iuout,
     &      'Weekday demands might be unsorted or not span whole day.')
        endif
        problem=.false.
        call dmdchecksort(2,problem,ier)
        if(problem)then
          sorted=.false.
          call edisp(iuout,
     &      'Saturday demands might be unsorted or not span whole day.')
        endif
        problem=.false.
        call dmdchecksort(3,problem,ier)
        if(problem)then
          sorted=.false.
          call edisp(iuout,
     &      'Sunday demands might be unsorted or not span whole day.')
        endif
        if(.NOT.sorted)then
          dok=.true.
          h(1)='The dispersed demand periods do not cover the entire'
          h(2)='day or might have gaps or overlaps. You should correct'
          h(3)='this prior to leaving this facility. If you say no then'
          h(4)='the dispersed demands might be mis-interpreted. '
          CALL askok(
     &      ' Periods might not cover entire day or have gaps. Do you',
     &      ' want to edit the periods at this time?',OK,dok,4)
          IF(OK) goto 20
        endif

C If the file changed then warn the user about lost data.
        if(MODOPR)then
          if(browse)return
          dok=.true.
          h(1)='Recent changes might be lost if you do not save the'
          h(2)='data. '
          CALL askok(' Before exiting do you wish to save any',
     &      ' changes that you might have made ?',OK,dok,2)
          IF(.not.OK) return
          LTMP=bdmds
   91     H(1)='A project demands file contains project level info'
          H(2)='on fans, pumps, lifts, DHW demands which may '
          H(3)='be applied to an integrated performance view. '
          CALL EASKS(LTMP,' Project demands file name: ',
     &      ' ',72,GFILE,'proj demands file',IER,3)
          IF(LTMP(1:2).NE.'  ')THEN
            bdmds=LTMP
            CALL EMKBDMD(IUO,bdmds,IER)
          ELSE
            GOTO 91
          ENDIF
        endif
        RETURN
      ELSEIF(INO.EQ.1)THEN

C Edit loads description.
        H(1)='Project demands schedules have notes' 
        H(2)='(<248 char) which will be included in reports.'
        tcname=dmdsdesc
        CALL EASKS248(tcname,' Dispersed demands notes: ',' ',
     &    72,'base_case','dispersed demands notes',IER,2)
        if(tcname(1:2).ne.'  ')dmdsdesc=tcname
        MODOPR=.true.
      ELSEIF(INO.EQ.2)THEN

C Floor area associated with each demand type.
C << echo the floor area of the various zones >>
C << to be done... >>
        WRITE(hold,'(7f8.1,a)')dmdfla(1),dmdfla(2),dmdfla(3),dmdfla(4),
     &    dmdfla(5),dmdfla(6),dmdfla(7),'  '
        h(1)='Optional base area associated with each demand type. '
        h(2)='This will be used to support IPV creation for period '
        h(3)='data which is per m^2. '
        CALL EASKS(hold,
     &    'Area (m^2) for occup lights sm power fans pumps lifts DHW',
     &    ' : ',60,' 1.0 1.0 1.0 1.0 1.0 1.0 1.0 ','dmd floor area',
     &    IER,3)
        K=0
        CALL EGETWR(hold,K,dmdfla(1),0.,99999.,'F','occup fla',IER)
        CALL EGETWR(hold,K,dmdfla(2),0.,99999.,'F','light fla',IER)
        CALL EGETWR(hold,K,dmdfla(3),0.,99999.,'F','smpwr fla',IER)
        CALL EGETWR(hold,K,dmdfla(4),0.,99999.,'F','fans fla',IER)
        CALL EGETWR(hold,K,dmdfla(5),0.,99999.,'F','pumps fla',IER)
        CALL EGETWR(hold,K,dmdfla(6),0.,99999.,'F','lifts fla',IER)
        CALL EGETWR(hold,K,dmdfla(7),0.,99999.,'F','DHW fla',IER)
      ELSEIF(INO.EQ.3)THEN

C Toggle day type within the range defined in the model calendar.
        iwss=iwss+1
        if(iwss.gt.nbdaytype)iwss=1
      ELSEIF(INO.EQ.5)THEN

C Edit project demands, save current menu status.
        CALL EPMENSV
        CALL EDBCAS(ITRU,IWSS,IER)
        MODOPR=.true.
        CALL EPMENRC
      ELSEIF(INO.EQ.6)THEN

C Zero demands.
        if(iwss.ge.1.and.iwss.le.nbdaytype)then
          NBCAS(iwss)=0
        endif
        call edisp(iuout,' Profile set to zero ')
        MODOPR=.true.
      ELSEIF(INO.EQ.7)THEN

C Set same as previous values (where possible) i.e.
C   Saturday = Weekday or Sunday   = Saturday
C   Weekday cannot be set by this method.
        IF(IWSS.EQ.1)THEN
          call edisp(iuout,' Weekdays cannot be set by this method !')
        ELSEIF(IWSS.gt.1.and.IWSS.le.nbdaytype)THEN
          NBCAS(iwss)=NBCAS(iwss-1)
          IF(NBCAS(iwss).gt.0)then
            DO 782 KK=1,NBCAS(iwss)
              IBCGS(iwss,KK)=IBCGS(iwss-1,KK)
              IBCGF(iwss,KK)=IBCGF(iwss-1,KK)
              IBCGT(iwss,KK)=IBCGT(iwss-1,KK)
              CBMGS(iwss,KK)=CBMGS(iwss-1,KK)
              CBMGL(iwss,KK)=CBMGL(iwss-1,KK)
              BRADC(iwss,KK)=BRADC(iwss-1,KK)
              BCONC(iwss,KK)=BCONC(iwss-1,KK)
  782       CONTINUE
          ENDIF
          MODOPR=.true.
        ENDIF
      ELSEIF(INO.EQ.9)THEN

C List contents.
        call BDMDINF(iuout,IER)
      ELSEIF(INO.EQ.11)THEN

C Save common block information to file. 
        call edisp(ITRU,' ')
        if(bdmds(1:2).EQ.'  '.or.bdmds(1:7).eq.'UNKNOWN')then
          LTMP=GFILE
        else
          LTMP=bdmds
        endif

   9    H(1)='A project demands file contains building level info'
        H(2)='on fans, pumps, lifts, DHW casual gains which may '
        H(3)='be applied to an integrated performance view. '
        CALL EASKS(LTMP,' Dispersed demands file name: ',
     &    ' ',72,GFILE,'demands file',IER,3)
        IF(LTMP(1:2).NE.'  ')THEN
          bdmds=LTMP
        ELSE
          GOTO 9
        ENDIF

C If older version check with user about upgrade.
        if(idmdver.eq.0)then
          dok=.true.
          h(1)='Older file format does not include a date stamp.'
          h(2)='If you say ok then a newer format will be used. '
          CALL askok(
     &      ' Older format (without date stamp) detected. Do you wish',
     &      ' to save changes with current format ?',OK,dok,2)
          if(ok) idmdver=1
        endif

C Merge current information into a descriptive file
C and save the system configuration file if not browsing.
        if(cfgok.and.(.NOT.browse))then
   11     CALL EMKBDMD(IUO,bdmds,IER)
          IF(IER.LT.0)THEN
            dok=.true.
            h(1)='A problem was encountered while trying to save the'
            h(2)='demands to file. Check that you have permission to'
            h(3)='update the file and that you have sufficient disk '
            h(4)='space. '
            CALL ASKOK(' ',
     &      'Problem encountered saving data...Try again?',OK,dok,4)
            IF(OK)THEN
              GOTO 11
            ELSE
              IER=1
              RETURN
            ENDIF
          endif
          call edisp(IUOUT,' updating model configuration...')
          CALL EMKCFG('-',IER)
        else
          call usrmsg('Cannot save model configuration while in ',
     &                'browse mode, you must `own` the model! ','W')
          goto 20
        endif
        MODOPR=.false.
      ELSEIF(INO.EQ.12)THEN

C Explain
        CALL PHELPD('demands help 1',35,'-',0,0,IER)
      ELSE
        INO=-1
        GOTO 30
      ENDIF
      INO=-4
      GOTO 20

      END


C -------- EDBCAS ----------
C EDBCAS: Edits dispersed demands.
C Returns: IBCGS(),IBCGF(),CBMGS(),CBMGL(),BRADC(),BCONC() common
C based on the day type passed.
C ITRC is the trace level, ITRU is the ouput channel, IER=0 is OK.
      SUBROUTINE EDBCAS(ITRU,IWSS,IER)

#include "building.h"
#include "epara.h"
#include "ipvdata.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)

      COMMON/BL3data/NBCAS(10),IBCGS(10,MC),IBCGF(10,MC),CBMGS(10,MC),
     &  CBMGL(10,MC),BRADC(10,MC),BCONC(10,MC),IBCGT(10,MC)

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)

C Descriptive label for each project demands type.
      common/bloadlabel/blodlabel(7)

C Local editing arrays.
      dimension IEBCGS(MC),IEBCGF(MC),CEBMGS(MC),CEBMGL(MC),BERADC(MC)
      dimension IEBCGT(MC),BECONC(MC)
      dimension vert(35),X(5)
      CHARACTER vert*43,head*28,H*72,outs*124,KEY*1,hold*40

C blodlabel(1)='Occup',(2)='Lights',(3)='SmPowr',(4)='Fans',(5)='Pumps',(6)='Lifts',(7)='DHW'     
      character blodlabel*6
      character*8 ll1,ll2,ll3,ll4,ll5,ll6,ll7
      character calename*32,calentag*12,calendayname*32

C Labels for loads.
      write(ll1,'(1x,a6,1x)') blodlabel(1)
      write(ll2,'(1x,a6,1x)') blodlabel(2)
      write(ll3,'(1x,a6,1x)') blodlabel(3)
      write(ll4,'(1x,a6,1x)') blodlabel(4)
      write(ll5,'(1x,a6,1x)') blodlabel(5)
      write(ll6,'(1x,a6,1x)') blodlabel(6)
      write(ll7,'(1x,a6,1x)') blodlabel(7)

C Defaults for any new gains.
      if(iwss.eq.1)idt = 1
      NEGC=1
      IEBCGS(1)=0
      IEBCGF(1)=24
      IEBCGT(1)=1
      CEBMGS(1)=0.
      CEBMGL(1)=0.
      BERADC(1)=0.5
      BECONC(1)=0.5

C Assign to local vars depending on day type.
  780 IF(iwss.GE.1.and.iwss.LE.nbdaytype)THEN
        if(mmod.eq.8)then
          write(head,'(3a,i2,a)') '  Demands: ',calentag(iwss),
     &      ' (',NBCAS(iwss),')'
        else
          write(head,'(2x,2a,i2,a)') calentag(iwss),
     &      ' (',NBCAS(iwss),')'
        endif
        IF(NBCAS(iwss).EQ.0)GOTO 265
        NEGC=NBCAS(iwss)
        DO 781 KK=1,NBCAS(iwss)
          IEBCGS(KK)=IBCGS(iwss,KK)
          IEBCGF(KK)=IBCGF(iwss,KK)
          IEBCGT(KK)=IBCGT(iwss,KK)
          CEBMGS(KK)=CBMGS(iwss,KK)
          CEBMGL(KK)=CBMGL(iwss,KK)
          BERADC(KK)=BRADC(iwss,KK)
          BECONC(KK)=BCONC(iwss,KK)
  781   CONTINUE
      ENDIF

C Setup for multi-page menu.
  265 MHEAD=4
      MCTL=6
      ILEN=NEGC
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=NEGC
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(M-MHEAD,KEY,IER)
          if(IEBCGT(L).gt.0)then
            WRITE(VERT(M),303)KEY,IEBCGS(L),IEBCGF(L),
     &       blodlabel(IABS(IEBCGT(L))),'W  ',CEBMGS(L),CEBMGL(L),
     &       BERADC(L),BECONC(L)
          else
            WRITE(VERT(M),303)KEY,IEBCGS(L),IEBCGF(L),
     &       blodlabel(IABS(IEBCGT(L))),'Wm2',CEBMGS(L),CEBMGL(L),
     &       BERADC(L),BECONC(L)
          endif
  303     FORMAT(A1,I5,I3,1x,A6,A3,F7.0,F7.0,F5.2,F5.2)
        ENDIF
   10 CONTINUE

      if(iwss.ge.1.and.iwss.le.nbdaytype)then
        if(mmod.eq.8)then
          write(head,'(3a,i2,a)') '  Demands: ',calentag(iwss),
     &      ' (',NEGC,')'
        else
          write(head,'(2x,2a,i2,a)') calentag(iwss),' (',NEGC,')'
        endif
      endif
      write(VERT(1),'(A)') head
      vert(2) =            '1 (electrical power not yet implemented)  '
      vert(3) =            '  ______________________________________  '
      vert(4) =            '  Start End Type   Sensib Latent Rad  Conv'

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  __________________________  '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 Page --- Part: ',I2,' of ',I2,' ---')
      ENDIF
      VERT(M+2)  ='* insert/ delete a period     '
      VERT(M+3)  ='! apply scaling to one type   '
      VERT(M+4)  ='< import from profiles db     '
      VERT(M+5)  ='? help                        '
      VERT(M+6)  ='- exit this menu              '

C Help text for this menu.
      H(1)='Casual gains are defined via one or more periods, the'
      H(2)='magnitude at a given time being the summation of the '
      H(3)='gains in all periods which include that time.  In '
      H(4)='this way complex patterns may be described. '
      H(5)=' '
      H(6)='Please note that neither the order in which periods'
      H(7)='are described nor their position in the display is'
      H(8)='important.  You may use an `additive` or `sequential`'
      H(9)='approach or a combination as you choose. '
      H(10)=' '
      H(11)='You may insert or delete a period at any point in '
      H(12)='the list by selecting it`s key  (a,b,c...).'
      H(13)=' '
      H(14)='See section 3.4 of the users manual for more info. '

C Now display the menu.
      CALL EMENU(' Project demands',VERT,MVERT,IVERT)

      IF(IVERT.eq.2)THEN
        continue
      ELSEIF(IVERT.EQ.MVERT)THEN
        GOTO 80
      ELSEIF(IVERT.EQ.(MVERT-1))THEN
        CALL PHELPD('gains menu',14,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN
        IF(IWSS.ge.1.and.iwss.le.nbdaytype)THEN
          IN=NBCAS(iwss)
        ENDIF
        CALL SBCASL(IN,IWSS,IER)
        goto 780
      ELSEIF(IVERT.EQ.(MVERT-3))THEN
        h(1)='You can apply a scaling factor to all entries of a '
        h(2)='particular casual gain type (useful for parametric '
        h(3)='studies on profiles which have been imported).'
        CALL EASKATOG('Apply scaling factor to which casual gain type?',
     &     ' : ',ll1,ll2,ll3,ll4,ll5,ll6,ll7,IEBCGTS,3)
        VAL=1.0
        CALL EASKR(VAL,' ',' Factor to be applied (0.1 to 5.0) ',
     &       0.1,'F',5.0,'F',1.0,'scaling factor',IER,3)
        do 33 i=1,NEGC
          if(IABS(IEBCGT(I)).eq.IEBCGTS)then
            CEBMGS(I)=CEBMGS(I)*VAL
            CEBMGL(I)=CEBMGL(I)*VAL
          endif
 33     continue
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C +- period.
        CALL EASKAB(' ','Modify list:','delete period','insert period',
     &    IW,0)
        IF(IW.EQ.1)THEN

C To delete a period redisplay the menu and allow user to indicate
C which one, then collapse the period list.
          call edisp(iuout,' Select point of deletion.')
          CALL EMENU('Demand: Delete',VERT,MVERT,IVERT)
          CALL KEYIND(MVERT,IVERT,IWHICH,INOUT)
          IF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN
            IF(NEGC.LT.2)GOTO 92
            DO 65 I=IWHICH,NEGC-1
              IEBCGS(I)=IEBCGS(I+1)
              IEBCGF(I)=IEBCGF(I+1)
              IEBCGT(I)=IEBCGT(I+1)
              CEBMGS(I)=CEBMGS(I+1)
              CEBMGL(I)=CEBMGL(I+1)
              BERADC(I)=BERADC(I+1)
              BECONC(I)=BECONC(I+1)
   65       CONTINUE
            NEGC=NEGC-1
          ELSE
            GOTO 265
          ENDIF

C Re-establish menu.
          goto 265
        ELSEIF(IW.EQ.2)then

C To insert a period redisplay the menu and allow user to indicate
C which one, then expand the period list and insert default info.
          if(NEGC+1.GT.MC)then
            call usrmsg(' ',' Sorry.. to many periods.','W')
            goto 92
          endif
          call edisp(iuout,' Select point of insertion.')
          CALL EMENU('Demand: Insert',VERT,MVERT,IVERT)
          CALL KEYIND(MVERT,IVERT,IWHICH,INOUT)
          IF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN
            NEGC=NEGC+1
            I=NEGC+1
   45       CONTINUE
            I=I-1
            IEBCGS(I)=IEBCGS(I-1)
            IEBCGF(I)=IEBCGF(I-1)
            IEBCGT(I)=IEBCGT(I-1)
            CEBMGS(I)=CEBMGS(I-1)
            CEBMGL(I)=CEBMGL(I-1)
            BERADC(I)=BERADC(I-1)
            BECONC(I)=BECONC(I-1)
            IF(I.GT.IWHICH+1)GOTO 45
            IEBCGS(IWHICH)=0
            IEBCGF(IWHICH)=24
            IEBCGT(IWHICH)=1
            CEBMGS(IWHICH)=0.
            CEBMGL(IWHICH)=0.
            BERADC(IWHICH)=0.5
            BECONC(IWHICH)=0.5
          ELSE
             GOTO 265
          ENDIF
        endif

C Re-establish menu.
        goto 265
      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit block identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,I,IO)

        WRITE(OUTS,'(A,I2)') ' Start and finish hours for period ',I
        WRITE(hold,'(2i8)')iebcgs(i),iebcgf(i)
        h(1)='Period during which demand applies. '
        CALL EASKS(hold,outs,' : ',40,' 0 24 ','st-fn hr',IER,1)
        K=0
        CALL EGETWI(hold,K,iv1,0,24,'F','period start',IER)
        CALL EGETWI(hold,K,iv,iv1,24,'F','period end',IER)
        iebcgs(I)=IV1
        iebcgf(I)=IV

C Establish the nature of the casual gain
        H(1)='The type of demand can be one of those listed in the '
        H(2)='selections. '
        CALL EASKATOG('Which casual gain type?',
     &     ' : ',ll1,ll2,ll3,ll4,ll5,ll6,ll7,IWM,2)

C Depending on type set relevant idmdinc value.
        idmdinc(IWM)=1

        call easkab('Are values',' ','absolute','per m^2',IW,8)
        if(IW.eq.1)IEBCGT(I)=IWM
        if(IW.eq.2)IEBCGT(I)=-1*IWM
 100    WRITE(HOLD,'(2f9.0,2f8.2,a)')CEBMGS(I),CEBMGL(I),BERADC(I),
     &    BECONC(I),'  '
        H(1)='Sensible and latent demands are specified in '
        h(2)='Watts. '
        h(3)='Also supply the radiant and convective fraction for'
        h(4)='the sensible load (should sum to ~1.00) '
        CALL EASKS(HOLD,
     &    ' Sensible & latent loads, radiant & convective fraction:',
     &    ' ',40,' 1000   0  0.5  0.5  ','casual gn info',IER,4)
        K=0
        CALL EGETWR(HOLD,K,PS,0.,999999.,'F','sensible',IER)
        CALL EGETWR(HOLD,K,PL,0.,999999.,'F','latent',IER)
        CALL EGETWR(HOLD,K,RAD,0.0,1.0,'F','rad frac',IER)
        CALL EGETWR(HOLD,K,CON,0.0,1.0,'F','conv frac',IER)
        CEBMGS(I)=PS
        CEBMGL(I)=PL
        BERADC(I)=RAD
        BECONC(I)=CON
        X(5)=BERADC(I)+BECONC(I)
        IF(X(5).LT..98.OR.X(5).GT.1.02)THEN
          call usrmsg(' Sum of radiant and convective',
     &                      ' fractions not close to 1.0! ','W')
          goto 100
        endif
      else
        IVERT=-1
        GOTO 92
      endif
      IVERT=-4
      GOTO 92

C Assign to common depending on day type.
   80 IF(IWSS.ge.1.and.iwss.le.nbdaytype)THEN
        NBCAS(iwss)=NEGC
        IF(NBCAS(iwss).EQ.0)GOTO 66
        DO 81 KK=1,NBCAS(iwss)
          IBCGS(iwss,KK)=IEBCGS(KK)
          IBCGF(iwss,KK)=IEBCGF(KK)
          IBCGT(iwss,KK)=IEBCGT(KK)
          CBMGS(iwss,KK)=CEBMGS(KK)
          CBMGL(iwss,KK)=CEBMGL(KK)
          BRADC(iwss,KK)=BERADC(KK)
          BCONC(iwss,KK)=BECONC(KK)
   81   CONTINUE
      ENDIF

   66 RETURN
      END

C ----------- SBCASL
C Import a profile from a profiles database to build project demand
C patterns.
      SUBROUTINE  SBCASL(IN,IDAY,IER)

#include "building.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)
#include "ipvdata.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/BL3data/NBCAS(10),IBCGS(10,MC),IBCGF(10,MC),CBMGS(10,MC),
     &  CBMGL(10,MC),BRADC(10,MC),BCONC(10,MC),IBCGT(10,MC)

C Descriptive label for a zone load or casual gain types.
      common/bloadlabel/blodlabel(7)

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)

C Profile database commons.
      COMMON/PDBITM/NPDBITEMS,lastrec,NCG(MGIT),NO(MGIT),PDBDESC(MGIT)
      COMMON/PDBELM/ICGS1(MGIT,MPRFDBPER),ICGF1(MGIT,MPRFDBPER),
     &             CGS1(MGIT,MPRFDBPER),CGL1(MGIT,MPRFDBPER)
      CHARACTER PDBDESC*40,outs248*248

      DIMENSION  DESC(MGIT),IPRVAL(MGIT)
      CHARACTER*40 DESC
      CHARACTER H*72,hold*40,blodlabel*6
      character*8 ll1,ll2,ll3,ll4,ll5,ll6,ll7
      character calename*32,calentag*12,calendayname*32

C Labels for loads.
      write(ll1,'(1x,a6,1x)') blodlabel(1)
      write(ll2,'(1x,a6,1x)') blodlabel(2)
      write(ll3,'(1x,a6,1x)') blodlabel(3)
      write(ll4,'(1x,a6,1x)') blodlabel(4)
      write(ll5,'(1x,a6,1x)') blodlabel(5)
      write(ll6,'(1x,a6,1x)') blodlabel(6)
      write(ll7,'(1x,a6,1x)') blodlabel(7)

      IER=0
      ICGT=1
      PS=0.
      PL=0.
      RAD=0.5
      CON=0.5
      
      llt=lnblnk(LPRFDB)
      write(outs248,'(2a)') 'Accessing event profile db: ',
     &  LPRFDB(1:llt)
      call edisp248(iuout,outs248,100)

C Scan the binary event profiles database. If an error state is
C returned then it might be an ASCII file, try and scan that.
      call TRIPROBIN(IER)
      if(ier.ne.0)then
        CALL ERPFREE(IPRODB,ISTAT)
        CALL USRMSG(' opened profiles db...',' ','-')
        call TRIPROCOM(IPRODB,LPRFDB,IER)
        if(ier.ne.0)then
          call usrmsg('Error opening the db. Please use one',
     &                'of the other options.','W')
          return
        else
          CALL ERPFREE(IPRODB,ISTAT)
          CALL USRMSG(' opened profiles db...ok',' ','-')
        endif
      else
        CALL USRMSG(' opened profiles db...ok',' ','-')
      endif

C Copy the string list of event profiles to local string variable.
      DO 50 I = 1,NPDBITEMS
        DESC(I)=PDBDESC(I)
   50 CONTINUE

C Present a list of profiles to select from.
      H(1)='Any one of the profiles may be selected.'
      IX=1
      CALL EPICKS(IX,IPRVAL,' ',' Profiles in database: ',
     &         40,NPDBITEMS,DESC,'event database profiles',IER,1)
      IF(IX.EQ.0)RETURN

      INDEX=IPRVAL(1)

C Identify nature of demands
      H(1)='Project level demands can be be designated to be '
      H(2)='of one of four user-defined types so that for example '
      H(3)='pump demands can be separately tracked from that of '
      H(4)='other equipment.  Values from 1 to 4 are accepted'
      H(5)='and can have any meaning that the user chooses. '
      CALL EASKATOG(' ','Type of casual gain: ',ll1,ll2,ll3,ll4,ll5,
     &  ll6,ll7,ICGT,5)

C Depending on type set relevant idmdinc value.
      idmdinc(ICGT)=1

      call easkab('Are values',' ','absolute','per m^2',IW,5)
      if(IW.eq.2)ICGT=-1*ICGT

 100  WRITE(HOLD,'(2f9.0,2f8.2,a)')PS,PL,RAD,CON,'  '
      H(1)='In order to expand the profile into energy values the'
      H(2)='sensible and latent load in Watts equivalent to 100% is'
      h(3)=' required. '
      h(4)=' '
      h(5)='Also supply the radiant and convective fraction for the'
      h(6)='sensible load (these normally should sum to 1.00) '
      CALL EASKS(HOLD,
     &  ' 100% sensible & latent loads, radiant & convective fraction:',
     &        ' ',40,' 1000   0  0.5  0.5  ','profile info',IER,3)
      K=0
      CALL EGETWR(HOLD,K,PS,0.,999999.,'F','sensible 100%',IER)
      CALL EGETWR(HOLD,K,PL,0.,999999.,'F','latent 100%',IER)
      CALL EGETWR(HOLD,K,RAD,0.0,1.0,'F','rad frac',IER)
      CALL EGETWR(HOLD,K,CON,0.0,1.0,'F','conv frac',IER)

      SUM = RAD + CON
      IF (SUM .LT. 0.98 .OR. SUM .GT. 1.02)then
        call usrmsg(
     &    ' The sum of radiant and convective fractions is not close',
     &    ' to unity, please respecify.','W')
        goto 100
      ENDIF
      call usrmsg(' ',' ','-')
          
      if(IDAY.ge.1.and.IDAY.le.nbdaytype)then

C The current calendar day type gains.
        NBCAS(iday) = NBCAS(iday) + NCG(INDEX)
        DO 140 I = 1,NCG(INDEX)
          IBCGS(iday,I+IN) = ICGS1(INDEX,I)
          IBCGF(iday,I+IN) = ICGF1(INDEX,I)
          CBMGS(iday,I+IN) = CGS1(INDEX,I)*PS/100.0
          CBMGL(iday,I+IN) = CGL1(INDEX,I)*PL/100.0
          BRADC(iday,I+IN) = RAD
          BCONC(iday,I+IN) = CON
          IBCGT(iday,I+IN) = ICGT
140     CONTINUE
      endif

      CALL ERPFREE(IPRODB,ISTAT)
      RETURN

      END


C ******************** ERBDMD
C ERBDMD reads project level demand profiles for fans, pumps, DHW etc.

C Common block variables are:
C dmdsdesc      - dispersed demands description

C NBCAS()  - number of demand sets during a typical Weekday,
C            Saturday and Sunday etc respectively.

C Maximum number of demands/day       MC=20

      SUBROUTINE ERBDMD(ITRC,ITRU,IUO,IER)
#include "building.h"
#include "espriou.h"
C espriou.h provides currentfile.
#include "ipvdata.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/BL1/dmdsdesc,bdmds
      COMMON/BL2/dmdfla(7)

C Support for up to 10 calendar day types
C IBCGS(),IBCGF() - start and finish hours of each demand
C CBMGS(),CBMGL() - sensible and latent magnitude (in Watts) of each demand
C BRADC(),BCONC(), - radiant and convective portions of each demand
C IBCGT() - type of gain (1-7)
      COMMON/BL3data/NBCAS(10),IBCGS(10,MC),IBCGF(10,MC),CBMGS(10,MC),
     &  CBMGL(10,MC),BRADC(10,MC),BCONC(10,MC),IBCGT(10,MC)

C Descriptive label for a zone load or casual gain types.
      common/bloadlabel/blodlabel(7)
      common/b3ver/idmdver

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)

C Error subroutine and error details for dll mode.
      common/dllerr/dllsubr,dllmesg

      CHARACTER dmdsdesc*248,outstr*124,bdmds*72,loutstr*248
      character dllsubr*12,dllmesg*124,WORD*20,outs*124,outs2*124
      character calename*32,calentag*12,calendayname*32
      character dstmp*24
      CHARACTER*6 blodlabel
      integer idt  ! local counter of day type.
      integer lsn  ! length of currentfile
      logical dll

      IER=0

C Check if running in dll mode.
      call isadll(dll)

C Initialise project data file.
      CALL EFOPSEQ(IUO,bdmds,1,IER)
      IF(IER.NE.0)THEN
        write(outs,'(3a)') 'Dispersed demands file ',
     &    bdmds(1:lnblnk(bdmds)),' could not be opened.'
        if(dll)then
          dllsubr='ERBDMD'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(iuout,outs)
          IER=1
          RETURN
        endif
      ENDIF
      write(currentfile,'(a)') bdmds(1:lnblnk(bdmds))
      
C Read lines from file, discarding comments. To allow for operation
C names with spaces copy directly from OUTSTR rather than parsing
C it into words. Use long strip for notes line. 
      CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'dispersed notes',IER)
      IF(IER.NE.0)RETURN

C Detect header and set a version number.
      if(loutstr(1:10).eq.'*Dispersed')then
        if(loutstr(12:14).eq.'1.0')then
          idmdver=1
        elseif(loutstr(12:14).eq.'2.0')then
          idmdver=2
        endif
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'header',IER)
        IF(IER.NE.0) goto 1002
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','header tags',IER)
        IF(IER.NE.0) goto 1002
        if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
          CALL EGETRM(OUTSTR,K,dstmp,'W','date stamp',IER)
        endif
        CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'dispersed notes',IER)
        dmdsdesc=LOUTSTR
      else
        dmdsdesc=LOUTSTR
        idmdver=0
      endif


C Loop through all of the calendar day types.
      do 30 idt=1,nbdaytype
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'day type demand periods',IER)
        K=0
        CALL EGETWI(OUTSTR,K,NBCAS(idt),0,MC,'W','periods in day',IER)
        IF(NBCAS(idt).EQ.0)goto 30
        DO 40 I=1,NBCAS(idt)
          CALL STRIPC(IUO,OUTSTR,99,ND,1,'Period gain detl',IER)
          K=0
          if(ND.eq.7)then
            CALL EGETWI(OUTSTR,K,IBCGT(idt,I),-7,7,'W','casual type',
     &        IER)
          else
            IBCGT(idt,I)=1
          endif

C Set related IPV inclusion index to 1.
          idmdinc(iabs(IBCGT(idt,I)))=1
          CALL EGETWI(OUTSTR,K,IBCGS(idt,I),0,24,'W','cas gain start',
     &      IER)
          CALL EGETWI(OUTSTR,K,IBCGF(idt,I),0,24,'W','cas gain finsh',
     &      IER)
          CALL EGETWR(OUTSTR,K,CBMGS(idt,I),0.,0.,'-','cas sensible',
     &      IER)
          CALL EGETWR(OUTSTR,K,CBMGL(idt,I),0.,0.,'-','cas latent',
     &      IER)
          CALL EGETWR(OUTSTR,K,BRADC(idt,I),0.0,1.,'W','cas rad frac',
     &      IER)
          CALL EGETWR(OUTSTR,K,BCONC(idt,I),0.0,1.,'W','cas conv frac',
     &      IER)
          if(IBCGS(idt,I).GT.IBCGF(idt,I))then

C If periods are out of sync then return with a warning.
            write(outs,'(3a)') ' Period start-end mismatch in...',
     &        OUTSTR(1:50),'...'
            if(dll)then
              dllsubr='ERBDMD'
              dllmesg=outs
              ier=2
              CALL ERPFREE(IUO,ISTAT)
              return
            else
              call edisp(iuout,outs)
              IER=1
              CALL ERPFREE(IUO,ISTAT)
              RETURN
            endif
          endif

          X=BRADC(idt,I)+BCONC(idt,I)
          if(X.GT.1.02)then

C If radiant plus convective is over unity warn.
            write(outs,'(3a)') ' Radiant & convec frc>1.02 in...',
     &        OUTSTR(1:50),'...'
            if(dll)then
              dllsubr='ERBDMD'
              dllmesg=outs
              ier=2
              CALL ERPFREE(IUO,ISTAT)
              return
            else
              call edisp(iuout,outs)
              IER=1
              CALL ERPFREE(IUO,ISTAT)
              RETURN
            endif
          endif
          IF(X.LT.0.95.AND.ITRC.GT.1)call edisp(iuout,
     &                            ' Demand rad:con sum < 1.0')
   40   CONTINUE
        IF(IER.NE.0) goto 1002
   30 continue

C Check to see if demand labels have been added to the end of the file
    8 CALL STRIPC(IUO,OUTSTR,99,ND,1,'Type labels',IERV)
      IF(ND.NE.7.OR.IERV.ne.0) THEN
        goto 1001
      ELSE
        K=0
        DO 1234 ITYP=1,7
          CALL EGETW(OUTSTR,K,blodlabel(ITYP),'W',
     &         'type label',IER) 
 1234   CONTINUE
      ENDIF      

C Check for areas associated with each demand label.
      CALL STRIPC(IUO,OUTSTR,99,ND,1,'Type areas',IERV)
      IF(ND.NE.7.OR.IERV.ne.0) THEN
        goto 1001
      ELSE
        K=0
        DO 1233 ITYP=1,7
          CALL EGETWR(OUTSTR,K,dmdfla(ITYP),0.,99999.,'W',
     &         'type area',IER) 
 1233   CONTINUE
      ENDIF      

 1235 CONTINUE    ! jump back point if types were not found.

C Distributed demands reporting if requested.
      if(ITRC.GE.1)call BDMDINF(iuout,IER)

C Now free project data file.
      CALL ERPFREE(IUO,ISTAT)
      RETURN

C Error messages.
 1001 call usrmsg(' ',' No type names found ...supplying defaults.','-')
      blodlabel(1)='Occup '
      blodlabel(2)='Lights'
      blodlabel(3)='SmPowr'      
      blodlabel(4)='Fans  '
      blodlabel(5)='Pumps '
      blodlabel(6)='Lifts '      
      blodlabel(7)='DHW   '      
      goto 1235

 1002 write(outs,'(3a)') 'ERBDMD: conversion error in...',
     &  OUTSTR(1:50),'...'
      lsn=MIN0(lnblnk(currentfile),110)
      write(outs2,'(2a)') 'in: ',currentfile(1:lsn)
      if(dll)then
        dllsubr='ERBDMD'
        dllmesg=outs
        ier=2
        CALL ERPFREE(IUO,ISTAT)
        return
      else
        call edisp(iuout,outs)
        call edisp(iuout,outs2)
        IER=1
        CALL ERPFREE(IUO,ISTAT)
        RETURN
      endif

      END


C --------- EMKBDMD
C Write project demands file. It is assumed that this data
C has been checked.  OPFIL is the name of the file to be
C written to (confirm if to be overwritten).
C ITRC unit number for user output, IER=0 OK IER=1 problem.
      SUBROUTINE EMKBDMD(IUO,OPFIL,IER)
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/BL1/dmdsdesc,bdmds
      COMMON/BL2/dmdfla(7)

      COMMON/BL3data/NBCAS(10),IBCGS(10,MC),IBCGF(10,MC),CBMGS(10,MC),
     &  CBMGL(10,MC),BRADC(10,MC),BCONC(10,MC),IBCGT(10,MC)
      common/b3ver/idmdver

C Descriptive label for a zone load or casual gain types.
      common/bloadlabel/blodlabel(7)

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)

      CHARACTER dmdsdesc*248,bdmds*72,OPFIL*72
      character calename*32,calentag*12,calendayname*32
      CHARACTER*6 blodlabel
      character dstmp*24

      IER=0

C Open any existing file by this name (ask user for confirmation to
C over-write) or create a new file.
      CALL EFOPSEQ(IUO,OPFIL,4,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

C Write version number and date for this file if newer.
C << also consider just putting it in the configuration file >>
      call dstamp(dstmp)
      if(idmdver.eq.0)then
        WRITE(IUO,31,IOSTAT=ISTAT,ERR=1)
     &    OPFIL(:lnblnk(OPFIL))
  31    FORMAT('# project demands defined in: ',/,'# ',a)
      elseif(idmdver.eq.1)then
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1) 
     &    '*Dispersed 1.0  # dispersed demands for model'
        WRITE(IUO,'(3a)',IOSTAT=IOS,ERR=1) '*date ',dstmp,
     &  '  # latest file modification '
      elseif(idmdver.eq.2)then
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1) 
     &    '*Dispersed 2.0 # dispersed demands for model'
        WRITE(IUO,'(3a)',IOSTAT=IOS,ERR=1) '*date ',dstmp,
     &  '  # latest file modification '
      endif

C Write documentation.
      WRITE(IUO,'(A)',IOSTAT=ISTAT,ERR=1)dmdsdesc(1:lnblnk(dmdsdesc))

C Write the common block data to the file. Loop through all of
C the calendar day types.
      do 30 idt=1,nbdaytype
        ldt=lnblnk(calentag(idt))
        WRITE(IUO,'(1X,I5,3A)',IOSTAT=ISTAT,ERR=1)NBCAS(idt),
     &            '   # no ',calentag(idt)(1:ldt),' demands '
        IF (NBCAS(idt) .GT. 0)THEN
          WRITE(IUO,'(3A)',IOSTAT=ISTAT,ERR=1)
     &      '# ',calentag(idt)(1:ldt),
     &      ': type, start, stop, sens, latent, rad_frac, conv_frac'
          DO 1160 I = 1,NBCAS(idt)
            WRITE(IUO,5460,IOSTAT=ISTAT,ERR=1)IBCGT(idt,I),
     &        IBCGS(idt,I),IBCGF(idt,I),CBMGS(idt,I),CBMGL(idt,I),
     &        BRADC(idt,I),BCONC(idt,I)
1160      CONTINUE
        ENDIF
  30  continue

5460  FORMAT(1X,3(I4,','),F11.3,',',F11.3,',',F6.3,',',F6.3)
C Write out the type labels and associated areas.
      WRITE(IUO,'(a)')'# Labels for demand types '
      WRITE(IUO,'(7(1X,A6))',IOSTAT=ISTAT,ERR=1)(blodlabel(I),I=1,7)
      WRITE(IUO,'(a)')'# Areas associated with demand types '
      WRITE(IUO,'(7F11.2)',IOSTAT=ISTAT,ERR=1)(dmdfla(I),I=1,7)
      CALL ERPFREE(IUO,ISTAT)
      RETURN

 1    call usrmsg(' Project demands file transfer error !',
     &            ' returning to menu...','W')
      RETURN

      END

C ****************** BDMDINF 
C BDMDINF provides an English description of non-specific project demands
C from common block data.
      SUBROUTINE BDMDINF(ITRU,ier)

#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/BL1/dmdsdesc,bdmds

C Descriptive label for a zone load or casual gain types.
      common/bloadlabel/blodlabel(7)

      COMMON/BL3data/NBCAS(10),IBCGS(10,MC),IBCGF(10,MC),CBMGS(10,MC),
     &  CBMGL(10,MC),BRADC(10,MC),BCONC(10,MC),IBCGT(10,MC)

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)

      CHARACTER dmdsdesc*248,outs*124,bdmds*72
      CHARACTER*6 blodlabel
      character calename*32,calentag*12,calendayname*32

      call edisp(itru,' ')
      call edisp(itru,'Notes: ')
      call edisp248(itru,dmdsdesc,72)

      do 30 idt=1,nbdaytype
        ldt=lnblnk(calentag(idt))
        WRITE(outs,'(3A,I3)')' No. ',calentag(idt)(1:ldt),' demands =',
     &    NBCAS(idt)
        call edisp(itru,outs)
        IF(NBCAS(idt).GT.0)THEN
          call edisp(itru,
     &  ' Gain Type Start Finish Sensible  Latent    Radiant   Convec')
          call edisp(itru,
     &  '           Hour  Hour   Magn. (W) Magn. (W) Frac      Frac')
          DO 1270 I=1,NBCAS(idt)
            if(IBCGT(idt,i).gt.0)then
              WRITE(outs,5360)I,blodlabel(IABS(IBCGT(idt,I))),'W  ',
     &          IBCGS(idt,I),IBCGF(idt,I),CBMGS(idt,I),CBMGL(idt,I),
     &          BRADC(idt,I),BCONC(idt,I)
            else
              WRITE(outs,5360)I,blodlabel(IABS(IBCGT(idt,I))),'Wm2',
     &          IBCGS(idt,I),IBCGF(idt,I),CBMGS(idt,I),CBMGL(idt,I),
     &          BRADC(idt,I),BCONC(idt,I)
            endif
            call edisp(itru,outs)
1270      CONTINUE
        ENDIF
        call edisp(itru,' ')
  30  continue

5360  FORMAT(I4,1x,A6,A3,I5,I3,F9.1,F9.1,F11.2,F11.2)

      RETURN
      END

C ************* dmdcheckcascount
C checkcascount scans current P3 & P3TYPE commons and refreshes 
C dmdloadcount() & dmdload24() in common blocks dmdloadcnt & dmdloadall
C Parameters:
C   ier - set to 1 if there was a problem
      subroutine dmdcheckcascount(ier)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

C Support for up to 10 calendar day types
C NBCAS() number of gains on each day type.
C IBCGS(),IBCGF() - start and finish hours of each demand
C CBMGS(),CBMGL() - sensible and latent magnitude (in Watts) of each demand
C BRADC(),BCONC(), - radiant and convective portions of each demand
C IBCGT() - type of gain for each day type and period
      COMMON/BL3data/NBCAS(10),IBCGS(10,MC),IBCGF(10,MC),CBMGS(10,MC),
     &  CBMGL(10,MC),BRADC(10,MC),BCONC(10,MC),IBCGT(10,MC)

C dmdloadcount(gaintype,daytype) - nb of each gain type (whether
C   absolute or W/m2).
C dmdloadm2count(gaintype,daytype) - nb of each m2 gain types.
C dmdloadmixed(gaintype,daytype) - if zero then nothing, if one
C   then all non-zero are absolute, if two then all (non-zero) are in
C   W/m2 or person/m2, if three then non-zero are mixed.
C Does each gain type on each day:
C   start at zero dmdload24(gaintype,daytype,1)=.true.
C   and end at 24 hours (dmdload24(gaintype,daytype,2)=.true.

C How many of each dispersed demands for each gaintype/daytype.
      integer dmdloadcount,dmdloadm2count,dmdloadmixed
      common/dmdsloadcnt/dmdloadcount(7,10),dmdloadm2count(7,10),
     &               dmdloadmixed(7,10)

      common/dmdloadall/dmdload24(7,10,2)
      logical dmdload24

C Keep track of how many non-zero positive and negative casual
C gain types - in order to figure out value of loadmixed.
      integer dmdloadnonzero,dmdloadm2nonzero
      dimension dmdloadnonzero(7,10),dmdloadm2nonzero(7,10)

      integer ij,ik   ! for counters
      integer idtype  ! for the type of dispersed demand
      integer idindex ! for neagtive types of dispersed demand

C Zero the dmdloadcount arrays for each gain type (ij) and day type (ik).
      do 40 ij=1,7
        do 41 ik=1,10
          dmdloadcount(ij,ik)=0
          dmdloadm2count(ij,ik)=0
          dmdloadm2nonzero(ij,ik)=0
          dmdloadmixed(ij,ik)=0
          dmdload24(ij,ik,1)=.false.
          dmdload24(ij,ik,2)=.false.
 41     continue
 40   continue

C Debug..
C      write(6,*) 'Nb of daytype periods for dispersed demands...'

C Loope through each possible day type (ik) and period (ij).
      do 42 ik=1,10
        if(NBCAS(ik).gt.0)then
          do 43 ij=1,NBCAS(ik)
            idtype=IBCGT(ik,ij)
            if(idtype.gt.0.and.idtype.le.7)then
              dmdloadcount(idtype,ik)=dmdloadcount(idtype,ik)+1
              if(CBMGS(ik,ij).gt.0.0)then
                dmdloadnonzero(idtype,ik)=dmdloadnonzero(idtype,ik)+1
              endif
              if(IBCGS(ik,ij).eq.0)dmdload24(idtype,ik,1)=.true. 
              if(IBCGF(ik,ij).eq.24)dmdload24(idtype,ik,2)=.true. 

C Determine if any of the casual gain types were mixed.
              if(dmdloadnonzero(idtype,ik).eq.0.and.
     &           dmdloadm2nonzero(idtype,ik).eq.0)then
                dmdloadmixed(idtype,ik)=0
              elseif(dmdloadnonzero(idtype,ik).gt.0.and.
     &               dmdloadm2nonzero(idtype,ik).eq.0)then
                dmdloadmixed(idtype,ik)=1
              elseif(dmdloadnonzero(idtype,ik).eq.0.and.
     &               dmdloadm2nonzero(idtype,ik).gt.0)then
                dmdloadmixed(idtype,ik)=2
              elseif(dmdloadnonzero(idtype,ik).ne.
     &               dmdloadm2nonzero(idtype,ik))then
                dmdloadmixed(idtype,ik)=3
              endif
            elseif(idtype.lt.0)then
              idindex=iabs(idtype)
              dmdloadcount(idindex,ik)=dmdloadcount(idindex,ik)+1
              dmdloadm2count(idindex,ik)=dmdloadm2count(idindex,ik)+1
              if(CBMGS(ik,ij).gt.0.0)then
                dmdloadm2nonzero(idindex,ik)=
     &            dmdloadm2nonzero(idindex,ik)+1
              endif
              if(IBCGS(ik,ij).eq.0)dmdload24(idindex,ik,1)=.true. 
              if(IBCGF(ik,ij).eq.24)dmdload24(idindex,ik,2)=.true. 

C Determine if any of the casual gain types were mixed.
              if(dmdloadnonzero(idindex,ik).eq.0.and.
     &           dmdloadm2nonzero(idindex,ik).eq.0)then
                dmdloadmixed(idindex,ik)=0
              elseif(dmdloadnonzero(idindex,ik).gt.0.and.
     &               dmdloadm2nonzero(idindex,ik).eq.0)then
                dmdloadmixed(idindex,ik)=1
              elseif(dmdloadnonzero(idindex,ik).eq.0.and.
     &               dmdloadm2nonzero(idindex,ik).gt.0)then
                dmdloadmixed(idindex,ik)=2
              elseif(dmdloadnonzero(idindex,ik).ne.
     &               dmdloadm2nonzero(idindex,ik))then
                dmdloadmixed(idindex,ik)=3
              endif
            endif
 43       continue

C Debug..
C          write(6,*) ik,' loadcount loadm2count loadmixed'
C          write(6,*)dmdloadcount(1,ik),dmdloadcount(2,ik),
C     &      dmdloadcount(3,ik),dmdloadcount(4,ik),dmdloadcount(5,ik),
C     &      dmdloadcount(6,ik),dmdloadcount(7,ik)
C          write(6,*)dmdloadm2count(1,ik),dmdloadm2count(2,ik),
C     &      dmdloadm2count(3,ik),dmdloadm2count(4,ik),
C     &      dmdloadm2count(5,ik),dmdloadm2count(6,ik)
C          write(6,*)dmdloadmixed(1,ik),dmdloadmixed(2,ik),
C     &      dmdloadmixed(3,ik),dmdloadmixed(4,ik),dmdloadmixed(5,ik),
C     &      dmdloadmixed(6,ik),dmdloadmixed(7,ik)

        endif
 42   continue
      return
      end

C ******************* dmdchecksort *********************
C dmdchecksort does a quick check of dispersed demands for sorted state.
C not extensive, but should catch most issues.
      subroutine dmdchecksort(idaytype,problem,ier)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

C Calendar commons.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)

      common/b3ver/idmdver

C Descriptive label for each project demands type.
      common/bloadlabel/blodlabel(7)

C How many of each dispersed demands for each gaintype/daytype.
      integer dmdloadcount,dmdloadm2count,dmdloadmixed
      common/dmdsloadcnt/dmdloadcount(7,10),dmdloadm2count(7,10),
     &               dmdloadmixed(7,10)

C Does each gain type on each day:
C   start at zero dmdload24(gaintype,daytype,1)=.true.
C   and end at 24 hours (dmdload24(gaintype,daytype,2)=.true.
      common/dmdloadall/dmdload24(7,10,2)
      logical dmdload24

C logical problem set to true if daytype might not be sorted.
      logical problem
      character outs*124
      character calename*32,calentag*12,calendayname*32
      CHARACTER*6 blodlabel

      if(idmdver.eq.0)then
        call edisp(iuout,'older format dispersed demands file detected')
        problem = .true.
        return
      endif

C Check how many distributed demands there are for each type on each day type.
      call dmdcheckcascount(ier)

C How many for each type?
      do 42 igt=1,7
        n1=dmdloadcount(igt,idaytype)

C See if first period is 0hr or 1hr and last period is 24.
        if(n1.gt.0.and.(.NOT.dmdload24(igt,idaytype,1)))then
          write(outs,'(4a)')'Initial ',blodlabel(igt),
     &      ' period does not start at zero for ',calentag(idaytype)
          call edisp(iuout,outs)
          problem = .true.
        endif

        if(n1.gt.0.and.(.NOT.dmdload24(igt,idaytype,2)))then
          write(outs,'(4a)') 'Last ',blodlabel(igt),
     &      ' period not 24 for daytype ',calentag(idaytype)
          call edisp(iuout,outs)
          problem = .true.
        endif
  42  continue
      return
      end
