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

C -------- PRJDMDS ----------
C PRJDMDS: Creates and edits a project demands file which holds
C information on building wide demands related to occupants, lighting,
C fans, pumps, lifts, domestic hot water.
C ITRC is the trace level, ITRU is the ouput channel, IUO is the
C file unit for the demands file. IER=0 is OK.
      SUBROUTINE PRJDMDS(ITRC,ITRU,IUO,IER)

#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

      common/C21/IFCFG,cfgroot,LCFGF
      common/user/browse
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C Demands notes and file name.
      COMMON/BL1/dmdsdesc,bdmds

C Floor area (optionally) associated with each demand type.
      COMMON/BL2/dmdfla(7)
      common/IPVDMD/idmdinc(7)

C Demands for weekdays/saturdays/sundays.
      COMMON/BL3/NBCAS1,IBCGS1(MC),IBCGF1(MC),CBMGS1(MC),CBMGL1(MC),
     &  BRADC1(MC),BCONC1(MC),NBCAS2,IBCGS2(MC),IBCGF2(MC),CBMGS2(MC),
     &  CBMGL2(MC),BRADC2(MC),BCONC2(MC),NBCAS3,IBCGS3(MC),IBCGF3(MC),
     &  CBMGS3(MC),CBMGL3(MC),BRADC3(MC),BCONC3(MC)

C Types associated with each period in weekdays/saturdays/sundays.
      COMMON/BL3TYPE/IBCGT1(MC),IBCGT2(MC),IBCGT3(MC)

C Descriptive label for each project demands type.
      common/bloadlabel/blodlabel(7)

      COMMON/FOPENED/CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK

      DIMENSION ITEM(13)
      CHARACTER dmdsdesc*248,tcname*248,bdmds*72
      CHARACTER ITEM*36,H*72,LTMP*72,GFILE*72
      CHARACTER*6 blodlabel
      character cfgroot*24,LCFGF*72,hold*60
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      LOGICAL OK,DOK,XST,MODOPR,browse

C GFILE is the default file name for project gains file.
      if(ctlpth(1:2).eq.'  '.or.ctlpth(1:2).eq.'./')then
        WRITE(GFILE,'(A,A5)') cfgroot(1:lnblnk(cfgroot)),'.dmds'
      else
        WRITE(GFILE,'(A,A,A,A5)') ctlpth(1:lnblnk(ctlpth)),'/',
     &    cfgroot(1:lnblnk(cfgroot)),'.dmds'
      endif
      if(bdmds(1:7).eq.'UNKNOWN'.or.bdmds(1:2).eq.'  ')then
        bdmds=GFILE
      endif

      MODOPR=.FALSE.

      NBCAS1=0
      NBCAS2=0
      NBCAS3=0
      iwss=1
      dmdsdesc='no dispersed demands notes (yet)'
      IER=0

C Set the default demand type labels
      blodlabel(1)='Occup'
      dmdfla(1)= 1.0
      blodlabel(2)='Lights'
      dmdfla(2)= 1.0
      blodlabel(3)='SmPowr'      
      dmdfla(3)= 1.0
      blodlabel(4)='Fans'
      dmdfla(4)= 1.0
      blodlabel(5)='Pumps'
      dmdfla(5)= 1.0
      blodlabel(6)='Lifts'      
      dmdfla(6)= 1.0
      blodlabel(7)='DHW'      
      dmdfla(7)= 1.0

C Initial demand inclusion to IPV set to zero.
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
      ENDIF

  20  write(ITEM(1),'(A,A25)')'a notes: ',dmdsdesc(1:25)
      ITEM(2) =               'b type associated base areas     '
      ITEM(4) =               '  _________________________      '
      ITEM(5)=                'c edit project demands           '
      ITEM(6)=                'd zeroise project demands        '
      if(iwss.eq.1)then
        item(3) =             'b focus on >> weekdays           '
        ITEM(7)=              '                                 '
      elseif(iwss.eq.2)then
        item(3) =             'b focus on >> saturdays          '
        ITEM(7)=              'e copy weekday project demands   '
      elseif(iwss.eq.3)then
        item(3) =             'b focus on >> sundays            '
        ITEM(7)=              'e copy saturday project demands  '
      endif
      ITEM(8)=                '  _________________________      '
      ITEM(9)=                'l list project demands   '
      ITEM(10)=               '  _________________________      '
      ITEM(11)=               '> save                           '
      ITEM(12)=               '? help                           '
      ITEM(13)=               '- exit this menu                 '
      NITEMS=13

C Help text for this menu.
      H(1) ='   Project demands file input '
      H(2) ='   ========================== '
      H(3) =' '
      H(4) ='The data requirements are summarized below :- '
      H(5) =' 1) Description '
      H(6) ='  '
      H(7) =' 2) Weekday, Saturday and Sunday demands data:'
      H(8) ='       Start and finish hours for each period'
      H(9) ='       Casual gain type:'
      H(10) ='         type 1: usually occupant demands'
      H(11) ='         type 2: usually lighting demands'
      H(12) ='         type 3: usually small power demands'
      H(13) ='         type 4: usually fan demands'
      H(14) ='         type 5: usually pump demands'
      H(15) ='         type 6: usually lift demands'
      H(16) ='         type 7: usually DHW demands'
      H(17) ='       Sensible and Latent magnitudes'
      H(18) ='       Radiant and Convective portions'

      INO=-2
30    CALL EMENU('Project demands',ITEM,NITEMS,INO)

      IF(INO.EQ.NITEMS)THEN
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
        WRITE(hold,'(7f8.1)')dmdfla(1),dmdfla(2),dmdfla(3),dmdfla(4),
     &    dmdfla(5),dmdfla(6),dmdfla(7)
        h(1)='Optional base area associated with each demand type. '
        h(2)='This will be used to support IPV creation for period '
        h(3)='data which is per m^2. '
        CALL EASKS(hold,
     &    'Area (m^2) for occup lights sm power fans pumps lifts DHW',
     &    ' : ',60,' 1. 1. 1. 1. 1. 1. 1. ','dmd floor area',IER,3)
        K=0
        CALL EGETWR(hold,K,dmdfla(1),0.,99999.,'F','occup fla',IER)
        CALL EGETWR(hold,K,dmdfla(2),0.,99999.,'F','light fla',IER)
        CALL EGETWR(hold,K,dmdfla(3),0.,99999.,'F','smpwr fla',IER)
        CALL EGETWR(hold,K,dmdfla(4),0.,99999.,'F','fans fla',IER)
        CALL EGETWR(hold,K,dmdfla(5),0.,99999.,'F','pumps fla',IER)
        CALL EGETWR(hold,K,dmdfla(6),0.,99999.,'F','lifts fla',IER)
        CALL EGETWR(hold,K,dmdfla(7),0.,99999.,'F','DHW fla',IER)
      ELSEIF(INO.EQ.3)THEN

C Toggle day type.
        iwss=iwss+1
        if(iwss.gt.3)iwss=1
      ELSEIF(INO.EQ.5)THEN

C Edit project demands, save current menu status.
        CALL EPMENSV
        CALL EDBCAS(ITRU,IWSS,IER)
        MODOPR=.true.
        CALL EPMENRC
      ELSEIF(INO.EQ.6)THEN

C Zero demands.
        if(iwss.eq.1)NBCAS1=0
        if(iwss.eq.2)NBCAS2=0
        if(iwss.eq.3)NBCAS3=0
        call edisp(iuout,' Profile set to zero ')
        MODOPR=.true.
      ELSEIF(INO.EQ.7)THEN

C Set same as previous values (where possible) ie
C   Saturday = Weekday
C   Sunday   = Saturday
C   Weekday cannot be set by this method.
        IF(IWSS.EQ.1)THEN
          call edisp(iuout,' Weekdays cannot be set by this method !')
        ELSEIF(IWSS.EQ.2)THEN
          NBCAS2=NBCAS1
          IF(NBCAS2.EQ.0)GOTO 67
          DO 782 KK=1,NBCAS2
            IBCGS2(KK)=IBCGS1(KK)
            IBCGF2(KK)=IBCGF1(KK)
            IBCGT2(KK)=IBCGT1(KK)
            CBMGS2(KK)=CBMGS1(KK)
            CBMGL2(KK)=CBMGL1(KK)
            BRADC2(KK)=BRADC1(KK)
            BCONC2(KK)=BCONC1(KK)
  782     CONTINUE
          MODOPR=.true.
        ELSE
          NBCAS3=NBCAS2
          IF(NBCAS3.EQ.0)GOTO 67
          DO 783 KK=1,NBCAS3
            IBCGS3(KK)=IBCGS2(KK)
            IBCGF3(KK)=IBCGF2(KK)
            IBCGT3(KK)=IBCGT2(KK)
            CBMGS3(KK)=CBMGS2(KK)
            CBMGL3(KK)=CBMGL2(KK)
            BRADC3(KK)=BRADC2(KK)
            BCONC3(KK)=BCONC2(KK)
  783     CONTINUE
        ENDIF
        MODOPR=.true.
   67   CONTINUE
      ELSEIF(INO.EQ.9)THEN

C List contents.
        call BDMDINF(iuout,IER)
      ELSEIF(INO.EQ.11)THEN
        call edisp(ITRU,' ')
        if(bdmds(1:2).EQ.'  '.or.bdmds(1:7).eq.'UNKNOWN')then
          LTMP=GFILE
        else
          LTMP=bdmds
        endif

   9    H(1)='A project demands file contains building level info'
        H(2)='on fans, pumps, lifts, DHW casual gains which may '
        H(3)='be applied to an integrated performance view. '
        CALL EASKS(LTMP,' Demands file name: ',
     &    ' ',72,GFILE,'demands file',IER,3)
        IF(LTMP(1:2).NE.'  ')THEN
          bdmds=LTMP
        ELSE
          GOTO 9
        ENDIF

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
        CALL PHELPD('demands help 1',18,'-',0,0,IER)
      ELSE
        INO=-1
        GOTO 30
      ENDIF
      INO=-4
      GOTO 20

      END


C -------- EDBCAS ----------
C EDBCAS: Edits model (non zone specific) demands.
C Returns: IBCGS*(),IBCGF*(),CBMGS*(),CBMGL*(),BRADC*(),BCONC*() common
C based on the day type passed.
C ITRC is the trace level, ITRU is the ouput channel, IER=0 is OK.
      SUBROUTINE EDBCAS(ITRU,IWSS,IER)

#include "building.h"
#include "epara.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

      COMMON/BL3/NBCAS1,IBCGS1(MC),IBCGF1(MC),CBMGS1(MC),CBMGL1(MC),
     &  BRADC1(MC),BCONC1(MC),NBCAS2,IBCGS2(MC),IBCGF2(MC),CBMGS2(MC),
     &  CBMGL2(MC),BRADC2(MC),BCONC2(MC),NBCAS3,IBCGS3(MC),IBCGF3(MC),
     &  CBMGS3(MC),CBMGL3(MC),BRADC3(MC),BCONC3(MC)
      COMMON/BL3TYPE/IBCGT1(MC),IBCGT2(MC),IBCGT3(MC)

C Descriptive label for each project demands type.
      common/bloadlabel/blodlabel(7)
      common/IPVDMD/idmdinc(7)

      dimension IBCGS(MC),IBCGF(MC),CBMGS(MC),CBMGL(MC),BRADC(MC)
      dimension IBCGT(MC),BCONC(MC)
      dimension vert(35),X(5)
      CHARACTER vert*43,head*28,H*72,outs*124,KEY*1,hold*40

C blodlabel(1)='Occup',(2)='Lights',(3)='SmPowr',(4)='Fans',(5)='Pumps',(6)='Lifts',(7)='DHW'     
      character blodlabel*6
      character*8 ll1,ll2,ll3,ll4,ll5,ll6,ll7

C Labels for loads.
      write(ll1,'(1x,a6,1x)') blodlabel(1)
      write(ll2,'(1x,a6,1x)') blodlabel(2)
      write(ll3,'(1x,a6,1x)') blodlabel(3)
      write(ll4,'(1x,a6,1x)') blodlabel(4)
      write(ll5,'(1x,a6,1x)') blodlabel(5)
      write(ll6,'(1x,a6,1x)') blodlabel(6)
      write(ll7,'(1x,a6,1x)') blodlabel(7)

C Defaults.
      NGC=1
      IBCGS(1)=0
      IBCGF(1)=24
      IBCGT(1)=1
      CBMGS(1)=0.
      CBMGL(1)=0.
      BRADC(1)=0.5
      BCONC(1)=0.5

C Assign to local vars depending on day type.
  780 IF(IWSS.EQ.1)THEN
        if(mmod.eq.8)then
          write(head,'(a,i2,a)') '  Demands: Weekdays (',NBCAS1,')'
        else
          write(head,'(a,i2,a)') '  Weekdays (',NBCAS1,')'
        endif
        IF(NBCAS1.EQ.0)GOTO 265
        NGC=NBCAS1
        DO 781 KK=1,NBCAS1
          IBCGS(KK)=IBCGS1(KK)
          IBCGF(KK)=IBCGF1(KK)
          IBCGT(KK)=IBCGT1(KK)
          CBMGS(KK)=CBMGS1(KK)
          CBMGL(KK)=CBMGL1(KK)
          BRADC(KK)=BRADC1(KK)
          BCONC(KK)=BCONC1(KK)
  781   CONTINUE
      ELSEIF(IWSS.EQ.2)THEN
        if(mmod.eq.8)then
          write(head,'(a,i2,a)') '  Demands: Saturdays (',NBCAS2,')'
        else
          write(head,'(a,i2,a)') '  Saturdays (',NBCAS2,')'
        endif
        IF(NBCAS2.EQ.0)GOTO 265
        NGC=NBCAS2
        DO 782 KK=1,NBCAS2
          IBCGS(KK)=IBCGS2(KK)
          IBCGF(KK)=IBCGF2(KK)
          IBCGT(KK)=IBCGT2(KK)
          CBMGS(KK)=CBMGS2(KK)
          CBMGL(KK)=CBMGL2(KK)
          BRADC(KK)=BRADC2(KK)
          BCONC(KK)=BCONC2(KK)
  782   CONTINUE
      ELSEIF(IWSS.EQ.3)THEN
        if(mmod.eq.8)then
          write(head,'(a,i2,a)') '  Demands: Sundays (',NBCAS3,')'
        else
          write(head,'(a,i2,a)') '  Sundays (',NBCAS3,')'
        endif
        IF(NBCAS3.EQ.0)GOTO 265
        NGC=NBCAS3
        DO 783 KK=1,NBCAS3
          IBCGS(KK)=IBCGS3(KK)
          IBCGF(KK)=IBCGF3(KK)
          IBCGT(KK)=IBCGT3(KK)
          CBMGS(KK)=CBMGS3(KK)
          CBMGL(KK)=CBMGL3(KK)
          BRADC(KK)=BRADC3(KK)
          BCONC(KK)=BCONC3(KK)
  783   CONTINUE
      ENDIF

C Setup for multi-page menu.
  265 MHEAD=4
      MCTL=6
      ILEN=NGC
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=NGC
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(M-MHEAD,KEY,IER)
          if(IBCGT(L).gt.0)then
            WRITE(VERT(M),303)KEY,IBCGS(L),IBCGF(L),
     &       blodlabel(IABS(IBCGT(L))),'W  ',CBMGS(L),CBMGL(L),BRADC(L),
     &       BCONC(L)
          else
            WRITE(VERT(M),303)KEY,IBCGS(L),IBCGF(L),
     &       blodlabel(IABS(IBCGT(L))),'Wm2',CBMGS(L),CBMGL(L),BRADC(L),
     &       BCONC(L)
          endif
  303     FORMAT(A1,I5,I3,1x,A6,A3,F7.0,F7.0,F5.2,F5.2)
        ENDIF
   10 CONTINUE

      if(IWSS.EQ.1)then
        if(mmod.eq.8)then
          write(head,'(a,i2,a)') '  Demands: Weekdays (',NGC,')'
        else
          write(head,'(a,i2,a)') '  Weekdays (',NGC,')'
        endif
      elseif(IWSS.EQ.2)then
        if(mmod.eq.8)then
          write(head,'(a,i2,a)') '  Demands: Saturdays (',NGC,')'
        else
          write(head,'(a,i2,a)') '  Saturdays (',NGC,')'
        endif
      elseif(IWSS.EQ.3)then
        if(mmod.eq.8)then
          write(head,'(a,i2,a)') '  Demands: Sundays (',NGC,')'
        else
          write(head,'(a,i2,a)') '  Sundays (',NGC,')'
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
        IF(IWSS.EQ.1)THEN
          IN=NBCAS1
        ELSEIF(IWSS.EQ.2)THEN
          IN=NBCAS2
        ELSEIF(IWSS.EQ.3)THEN
          IN=NBCAS3
        ENDIF
        CALL SBCASL(IN,IWSS,IER)
        goto 780
      ELSEIF(IVERT.EQ.(MVERT-3))THEN
        h(1)='You can apply a scaling factor to all entries of a '
        h(2)='particular casual gain type (useful for parametric '
        h(3)='studies on profiles which have been imported).'
        CALL EASKATOG('Apply scaling factor to which casual gain type?',
     &     ' : ',ll1,ll2,ll3,ll4,ll5,ll6,ll7,IBCGTS,3)
        VAL=1.0
        CALL EASKR(VAL,' ',' Factor to be applied (0.1 to 5.0) ',
     &       0.1,'F',5.0,'F',1.0,'scaling factor',IER,3)
        do 33 i=1,NGC
          if(IABS(IBCGT(I)).eq.IBCGTS)then
            CBMGS(I)=CBMGS(I)*VAL
            CBMGL(I)=CBMGL(I)*VAL
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
            IF(NGC.LT.2)GOTO 92
            DO 65 I=IWHICH,NGC-1
              IBCGS(I)=IBCGS(I+1)
              IBCGF(I)=IBCGF(I+1)
              IBCGT(I)=IBCGT(I+1)
              CBMGS(I)=CBMGS(I+1)
              CBMGL(I)=CBMGL(I+1)
              BRADC(I)=BRADC(I+1)
              BCONC(I)=BCONC(I+1)
   65       CONTINUE
            NGC=NGC-1
          ELSE
            GOTO 265
          ENDIF

C Re-establish menu.
          goto 265
        ELSEIF(IW.EQ.2)then

C To insert a period redisplay the menu and allow user to indicate
C which one, then expand the period list and insert default info.
          if(NGC+1.GT.MC)then
            call usrmsg(' ',' Sorry.. to many periods.','W')
            goto 92
          endif
          call edisp(iuout,' Select point of insertion.')
          CALL EMENU('Demand: Insert',VERT,MVERT,IVERT)
          CALL KEYIND(MVERT,IVERT,IWHICH,INOUT)
          IF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN
            NGC=NGC+1
            I=NGC+1
   45       CONTINUE
            I=I-1
            IBCGS(I)=IBCGS(I-1)
            IBCGF(I)=IBCGF(I-1)
            IBCGT(I)=IBCGT(I-1)
            CBMGS(I)=CBMGS(I-1)
            CBMGL(I)=CBMGL(I-1)
            BRADC(I)=BRADC(I-1)
            BCONC(I)=BCONC(I-1)
            IF(I.GT.IWHICH+1)GOTO 45
            IBCGS(IWHICH)=0
            IBCGF(IWHICH)=24
            IBCGT(IWHICH)=1
            CBMGS(IWHICH)=0.
            CBMGL(IWHICH)=0.
            BRADC(IWHICH)=0.5
            BCONC(IWHICH)=0.5
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
        WRITE(hold,'(2i8)')ibcgs(i),ibcgf(i)
        h(1)='Period during which demand applies. '
        CALL EASKS(hold,outs,' : ',40,' 0 24 ','st-fn hr',IER,1)
        K=0
        CALL EGETWI(hold,K,iv1,0,24,'F','period start',IER)
        CALL EGETWI(hold,K,iv,iv1,24,'F','period end',IER)
        ibcgs(I)=IV1
        ibcgf(I)=IV

C Establish the nature of the casual gain
        H(1)='The type of demand can be one of those listed in the '
        H(2)='selections. '
        CALL EASKATOG('Which casual gain type?',
     &     ' : ',ll1,ll2,ll3,ll4,ll5,ll6,ll7,IWM,2)

C Depending on type set relevant idmdinc value.
        idmdinc(IWM)=1

        call easkab('Are values',' ','absolute','per m^2',IW,8)
        if(IW.eq.1)IBCGT(I)=IWM
        if(IW.eq.2)IBCGT(I)=-1*IWM
 100    WRITE(HOLD,'(2f9.0,2f8.2)')CBMGS(I),CBMGL(I),BRADC(I),BCONC(I)
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
        CBMGS(I)=PS
        CBMGL(I)=PL
        BRADC(I)=RAD
        BCONC(I)=CON
        X(5)=BRADC(I)+BCONC(I)
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
   80 IF(IWSS.EQ.1)THEN
        NBCAS1=NGC
        IF(NBCAS1.EQ.0)GOTO 66
        DO 81 KK=1,NBCAS1
          IBCGS1(KK)=IBCGS(KK)
          IBCGF1(KK)=IBCGF(KK)
          IBCGT1(KK)=IBCGT(KK)
          CBMGS1(KK)=CBMGS(KK)
          CBMGL1(KK)=CBMGL(KK)
          BRADC1(KK)=BRADC(KK)
          BCONC1(KK)=BCONC(KK)
   81   CONTINUE
      ELSEIF(IWSS.EQ.2)THEN
        NBCAS2=NGC
        IF(NBCAS2.EQ.0)GOTO 66
        DO 82 KK=1,NBCAS2
          IBCGS2(KK)=IBCGS(KK)
          IBCGF2(KK)=IBCGF(KK)
          IBCGT2(KK)=IBCGT(KK)
          CBMGS2(KK)=CBMGS(KK)
          CBMGL2(KK)=CBMGL(KK)
          BRADC2(KK)=BRADC(KK)
          BCONC2(KK)=BCONC(KK)
   82   CONTINUE
      ELSEIF(IWSS.EQ.3)THEN
        NBCAS3=NGC
        IF(NBCAS3.EQ.0)GOTO 66
        DO 83 KK=1,NBCAS3
          IBCGS3(KK)=IBCGS(KK)
          IBCGF3(KK)=IBCGF(KK)
          IBCGT3(KK)=IBCGT(KK)
          CBMGS3(KK)=CBMGS(KK)
          CBMGL3(KK)=CBMGL(KK)
          BRADC3(KK)=BRADC(KK)
          BCONC3(KK)=BCONC(KK)
   83   CONTINUE
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

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/BL3/NBCAS1,IBCGS1(MC),IBCGF1(MC),CBMGS1(MC),CBMGL1(MC),
     &  BRADC1(MC),BCONC1(MC),NBCAS2,IBCGS2(MC),IBCGF2(MC),CBMGS2(MC),
     &  CBMGL2(MC),BRADC2(MC),BCONC2(MC),NBCAS3,IBCGS3(MC),IBCGF3(MC),
     &  CBMGS3(MC),CBMGL3(MC),BRADC3(MC),BCONC3(MC)
      COMMON/BL3TYPE/IBCGT1(MC),IBCGT2(MC),IBCGT3(MC)

C Descriptive label for a zone load or casual gain types.
      common/bloadlabel/blodlabel(7)
      common/IPVDMD/idmdinc(7)

      DIMENSION  ICGS(MC),ICGF(MC),CGS(MC),CGL(MC)
      DIMENSION  NO(100),DESC(100),IPRVAL(100)
      CHARACTER*40 DESC
      CHARACTER H*72,hold*40,blodlabel*6
      character*8 ll1,ll2,ll3,ll4,ll5,ll6,ll7

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
      call efopran(IPRODB,LPRFDB,10,1,IER)
      IF(ier.ne.0)return
      CALL edisp(iuout,' opened profiles db')

      IREC = 1
      READ(IPRODB,REC=IREC)  NITEMS
      DO 50 I = 1,NITEMS
        IREC = IREC + 1
        NO(I) = IREC
        READ(IPRODB,REC=IREC)  NCG
        IREC = IREC + 1
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=180)  DESC(I)
        IREC = IREC + NCG
   50 CONTINUE

C Present a list of profiles to select from.
      H(1)='Any one of the profiles may be selected.'
      IX=1
      CALL EPICKS(IX,IPRVAL,' ',' Profiles in db: ',
     &         40,NITEMS,DESC,'db profiles',IER,1)
      IF(IX.EQ.0)RETURN

      N=IPRVAL(1)
      IREC = NO(N)
      READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=181)  NCG
      IREC = IREC + 2

C There are NCG gains to be added
      DO 70 I = 1,NCG
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=183)  ICGS(I),ICGF(I),
     &       CGS(I),CGL(I)
        IREC = IREC + 1
70    CONTINUE

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

 100  WRITE(HOLD,'(2f9.0,2f8.2)')PS,PL,RAD,CON
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
          
      if(IDAY .EQ. 1)then

C Weekday gains.
        NBCAS1 = NBCAS1 + NCG
        DO 140 I = 1,NCG
          IBCGS1(I+IN) = ICGS(I)
          IBCGF1(I+IN) = ICGF(I)
          CBMGS1(I+IN) = CGS(I)*PS/100.0
          CBMGL1(I+IN) = CGL(I)*PL/100.0
          BRADC1(I+IN) = RAD
          BCONC1(I+IN) = CON
          IBCGT1(I+IN) = ICGT
140     CONTINUE
      elseif(IDAY .EQ. 2)then

C Saturday gains.
        NBCAS2 = NBCAS2 + NCG
        DO 141 I = 1,NCG
          IBCGS2(I+IN) = ICGS(I)
          IBCGF2(I+IN) = ICGF(I)
          CBMGS2(I+IN) = CGS(I)*PS/100.0
          CBMGL2(I+IN) = CGL(I)*PL/100.0
          BRADC2(I+IN) = RAD
          BCONC2(I+IN) = CON
          IBCGT2(I+IN) = ICGT
141     CONTINUE
      elseif(IDAY .EQ. 3)then

C Sunday gains.
        NBCAS3 = NBCAS3 + NCG
        DO 142 I = 1,NCG
          IBCGS3(I+IN) = ICGS(I)
          IBCGF3(I+IN) = ICGF(I)
          CBMGS3(I+IN) = CGS(I)*PS/100.0
          CBMGL3(I+IN) = CGL(I)*PL/100.0
          BRADC3(I+IN) = RAD
          BCONC3(I+IN) = CON
          IBCGT3(I+IN) = ICGT
142     CONTINUE
      endif

  999 CALL ERPFREE(IPRODB,ISTAT)
      RETURN

180   call edisp(iuout,' Casual gain database read error.')
      IER=1
      goto 999
181   call edisp(iuout,' Casual gain db NCG read error.')
      IER=1
      goto 999
183   call edisp(iuout,' Casual gain db data read error.')
      IER=1
      goto 999

      END


C ******************** ERBDMD
C ERBDMD reads project level demand profiles for fans, pumps, DHW etc.

C Common block variables are:
C dmdsdesc      - dispersed demands description

C NBCAS1,NBCAS2 - number of demand sets during a typical Weekday,
C & NBCAS3        Saturday and Sunday respectively.

C IBCGT1,IBCGT2 - Demand set type (weekdays/saturdays/sundays) :
C IBCG3T          1. Occup, 2. Lighting, 3. Small power, 4. Fans, 
C                 5. Pumps, 6. Lifts, 7. DHW
C                -1. Occup w/m2, -2. Lighting w/m2, -3. Sml power w/m2,
C                -4 Fan per w/m2, -5. Pumps per w/m2  -6. Lifts w/m^2,  
C                -7. DHW w/m^2

C IBCGS1,IBCGF1, - start and finish hours of each of the above demands
C IBCGS2,IBCGF2,   periods
C IBCGS3 & IBCGF3

C CBMGS1,CBMGL1, - sensible and latent magnitude (in Watts) of
C CBMGS2,CBMGL2,   each demand
C CBMGS3 & CBMGL3

C BRADC1,BCONC1, - radiant and convective portions (proportion
C BRADC2,BCONC2,   of 1) of each demand
C BRADC3,BCONC3

C Maximum number of demands/day       MC=20

      SUBROUTINE ERBDMD(ITRC,ITRU,IUO,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/BL1/dmdsdesc,bdmds
      COMMON/BL2/dmdfla(7)
      COMMON/BL3/NBCAS1,IBCGS1(MC),IBCGF1(MC),CBMGS1(MC),CBMGL1(MC),
     &  BRADC1(MC),BCONC1(MC),NBCAS2,IBCGS2(MC),IBCGF2(MC),CBMGS2(MC),
     &  CBMGL2(MC),BRADC2(MC),BCONC2(MC),NBCAS3,IBCGS3(MC),IBCGF3(MC),
     &  CBMGS3(MC),CBMGL3(MC),BRADC3(MC),BCONC3(MC)
      COMMON/BL3TYPE/IBCGT1(MC),IBCGT2(MC),IBCGT3(MC)

C Descriptive label for a zone load or casual gain types.
      common/bloadlabel/blodlabel(7)
      common/IPVDMD/idmdinc(7)

      CHARACTER dmdsdesc*248,outstr*124,bdmds*72,loutstr*248
      CHARACTER*6 blodlabel

      IER=0

C Initialise project data file.
      CALL EFOPSEQ(IUO,bdmds,1,IER)
      IF(IER.NE.0)THEN
        IER=1
        RETURN
      ENDIF

C Read lines from file, discarding comments. To allow for operation
C names with spaces copy directly from OUTSTR rather than parsing
C it into words. Use long strip for notes line. 
      CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'dispersed notes',IER)
      IF(IER.NE.0)RETURN
      dmdsdesc=LOUTSTR

C Weekdays.
      CALL STRIPC(IUO,OUTSTR,0,ND,1,'Weekday demand periods',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NBCAS1,0,MC,'W','Week periods',IER)
      IF(NBCAS1.EQ.0)goto 5
      DO 40 I=1,NBCAS1
        CALL STRIPC(IUO,OUTSTR,99,ND,1,'Period gain detl',IER)
        K=0
        if(ND.eq.7)then
          CALL EGETWI(OUTSTR,K,IBCGT1(I),-7,7,'W','wkd type',IER)
        else
          IBCGT1(I)=1
        endif

C Set related IPV inclusion index to 1.
        idmdinc(iabs(IBCGT1(I)))=1
        CALL EGETWI(OUTSTR,K,IBCGS1(I),0,24,'W','wkd gain st',IER)
        CALL EGETWI(OUTSTR,K,IBCGF1(I),0,24,'W','wkd gain fn',IER)
        CALL EGETWR(OUTSTR,K,CBMGS1(I),0.,0.,'-','wkd sens',IER)
        CALL EGETWR(OUTSTR,K,CBMGL1(I),0.,0.,'-','wkd latent',IER)
        CALL EGETWR(OUTSTR,K,BRADC1(I),0.0,1.,'W','wkd rad fr',IER)
        CALL EGETWR(OUTSTR,K,BCONC1(I),0.0,1.,'W','wkd conv fr',IER)
        IF(IBCGS1(I).GT.IBCGF1(I))goto 1006
        X=BRADC1(I)+BCONC1(I)
        IF(X.GT.1.)goto 1015
        IF(X.LT..95.AND.ITRC.GT.1)call edisp(iuout,
     &                            ' Demand rad:con sum < 1.0')
   40 CONTINUE
      IF(IER.NE.0)RETURN

C Saturdays.
   5  CALL STRIPC(IUO,OUTSTR,0,ND,1,'Saturday demand periods',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NBCAS2,0,MC,'W','Sat periods',IER)
      IF(NBCAS2.EQ.0)GOTO 6
      DO 50 I=1,NBCAS2
        CALL STRIPC(IUO,OUTSTR,99,ND,1,'Period gain details',IER)
        K=0
        if(ND.eq.7)then
          CALL EGETWI(OUTSTR,K,IBCGT2(I),-7,7,'W','Sat type',IER)
        else
          IBCGT2(I)=1
        endif

C Set related IPV inclusion index to 1.
        idmdinc(iabs(IBCGT2(I)))=1
        CALL EGETWI(OUTSTR,K,IBCGS2(I),0,24,'W','Sat gain st',IER)
        CALL EGETWI(OUTSTR,K,IBCGF2(I),0,24,'W','Sat gain fn',IER)
        CALL EGETWR(OUTSTR,K,CBMGS2(I),0.,0.,'-','Sat sens',IER)
        CALL EGETWR(OUTSTR,K,CBMGL2(I),0.,0.,'-','Sat latent',IER)
        CALL EGETWR(OUTSTR,K,BRADC2(I),0.0,1.0,'W','Sat rad fr',IER)
        CALL EGETWR(OUTSTR,K,BCONC2(I),0.0,1.0,'W','Sat conv fr',IER)
        IF(IBCGS2(I).GT.IBCGF2(I))goto 1006
        X=BRADC2(I)+BCONC2(I)
        IF(X.GT.1.)goto 1015
        IF(X.LT..95.AND.ITRC.GT.1)call edisp(iuout,
     &                            ' Demand rad:con sum < 1.0')
   50 CONTINUE

C Sundays.
   6  CALL STRIPC(IUO,OUTSTR,0,ND,1,'Sunday demand periods',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NBCAS3,0,MC,'W','Sun periods',IER)
      IF(NBCAS3.EQ.0)GOTO 8
      DO 60 I=1,NBCAS3
        CALL STRIPC(IUO,OUTSTR,99,ND,1,'Period details',IER)
        K=0
        if(ND.eq.7)then
          CALL EGETWI(OUTSTR,K,IBCGT3(I),-7,7,'W','Sun type',IER)
        else
          IBCGT3(I)=1
        endif

C Set related IPV inclusion index to 1.
        idmdinc(iabs(IBCGT3(I)))=1
        CALL EGETWI(OUTSTR,K,IBCGS3(I),0,24,'W','Sun gain st',IER)
        CALL EGETWI(OUTSTR,K,IBCGF3(I),0,24,'W','Sun gain fn',IER)
        CALL EGETWR(OUTSTR,K,CBMGS3(I),0.,0.,'-','Sun sens',IER)
        CALL EGETWR(OUTSTR,K,CBMGL3(I),0.,0.,'-','Sun latent',IER)
        CALL EGETWR(OUTSTR,K,BRADC3(I),0.0,1.0,'W','Sun rad fr',IER)
        CALL EGETWR(OUTSTR,K,BCONC3(I),0.0,1.0,'W','Sun conv fr',IER)
        IF(IBCGS3(I).GT.IBCGF3(I))goto 1006
        X=BRADC3(I)+BCONC3(I)
        IF(X.GT.1.)goto 1015
        IF(X.LT..95.AND.ITRC.GT.1)call edisp(iuout,
     &                            ' Demand rad:con sum < 1.0')
   60 CONTINUE

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

 1235 CONTINUE

C Deman reporting if requested.
      if(ITRC.GE.1)call BDMDINF(iuout,IER)

C Now free project data file.
    7 CALL ERPFREE(IUO,ISTAT)
      RETURN

C Error messages.
 1000 IER=1
      call edisp(iuout,' Project demands file error.')
      GOTO 7

 1001 call usrmsg(' ',' No type names found ...supplying defaults.','-')
        blodlabel(1)='Occup'
        blodlabel(2)='Lights'
        blodlabel(3)='SmPowr'      
        blodlabel(4)='Fans'
        blodlabel(5)='Pumps'
        blodlabel(6)='Lifts'      
        blodlabel(7)='DHW'      
      goto 1235
 1006 call edisp(iuout,' Period start - end mismatch in the line ...')
      call edisp(iuout,outstr)
      goto 1000
 1015 call edisp(iuout,' Radiant & convec fractions > 1.0 in...')
      call edisp(iuout,outstr)
      goto 1000

      END


C --------- EMKBDMD
C Write project demands file. It is assumed that this data
C has been checked.  OPFIL is the name of the file to be
C written to (confirm if to be overwritten).
C ITRC unit number for user output, IER=0 OK IER=1 problem.
      SUBROUTINE EMKBDMD(IUO,OPFIL,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/BL1/dmdsdesc,bdmds
      COMMON/BL2/dmdfla(7)
      COMMON/BL3/NBCAS1,IBCGS1(MC),IBCGF1(MC),CBMGS1(MC),CBMGL1(MC),
     &  BRADC1(MC),BCONC1(MC),NBCAS2,IBCGS2(MC),IBCGF2(MC),CBMGS2(MC),
     &  CBMGL2(MC),BRADC2(MC),BCONC2(MC),NBCAS3,IBCGS3(MC),IBCGF3(MC),
     &  CBMGS3(MC),CBMGL3(MC),BRADC3(MC),BCONC3(MC)
      COMMON/BL3TYPE/IBCGT1(MC),IBCGT2(MC),IBCGT3(MC)

C Descriptive label for a zone load or casual gain types.
      common/bloadlabel/blodlabel(7)

      CHARACTER dmdsdesc*248,bdmds*72,OPFIL*72
      CHARACTER*6 blodlabel

      IER=0

C Open any existing file by this name (ask user for confirmation to
C over-write) or create a new file.
      CALL EFOPSEQ(IUO,OPFIL,4,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

      WRITE(IUO,30,IOSTAT=ISTAT,ERR=1)
     &  OPFIL(:lnblnk(OPFIL))
  30  FORMAT('# project demands defined in: ',/,'# ',a)


C Write the common block data to the file.
      WRITE(IUO,'(A)',IOSTAT=ISTAT,ERR=1)dmdsdesc(1:lnblnk(dmdsdesc))
      WRITE(IUO,'(1X,I5,A)',IOSTAT=ISTAT,ERR=1)NBCAS1,
     &            '   # no Weekday demands '
      IF (NBCAS1 .GT. 0)THEN
        WRITE(IUO,'(A)',IOSTAT=ISTAT,ERR=1)
     &    '# Wkd: type, start, stop, sens, latent, rad_frac, conv_frac'
        DO 1160 I = 1,NBCAS1
          WRITE(IUO,5460,IOSTAT=ISTAT,ERR=1)IBCGT1(I),IBCGS1(I),
     &          IBCGF1(I),CBMGS1(I),CBMGL1(I),BRADC1(I),BCONC1(I)
1160    CONTINUE
      ENDIF

      WRITE(IUO,'(1X,I5,A)',IOSTAT=ISTAT,ERR=1)NBCAS2,
     &            '   # no Saturday demands '
      IF (NBCAS2 .GT. 0)THEN
        WRITE(IUO,'(A)',IOSTAT=ISTAT,ERR=1)
     &    '# Sat: type, start, stop, sens, latent, rad_frac, conv_frac'
        DO 1180 I = 1,NBCAS2
          WRITE(IUO,5460,IOSTAT=ISTAT,ERR=1)IBCGT2(I),IBCGS2(I),
     &          IBCGF2(I),CBMGS2(I),CBMGL2(I),BRADC2(I),BCONC2(I)
1180    CONTINUE
      ENDIF

      WRITE(IUO,'(1X,I5,A)',IOSTAT=ISTAT,ERR=1)NBCAS3,
     &            '   # no Sunday demands '
      IF (NBCAS3 .GT. 0)THEN
        WRITE(IUO,'(A)',IOSTAT=ISTAT,ERR=1)
     &    '# Sun: type, start, stop, sens, latent, rad_frac, conv_frac'
        DO 1185 I = 1,NBCAS3
          WRITE(IUO,5460,IOSTAT=ISTAT,ERR=1)IBCGT3(I),IBCGS3(I),
     &          IBCGF3(I),CBMGS3(I),CBMGL3(I),BRADC3(I),BCONC3(I)
1185    CONTINUE
      ENDIF

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
      COMMON/BL1/dmdsdesc,bdmds
      COMMON/BL3/NBCAS1,IBCGS1(MC),IBCGF1(MC),CBMGS1(MC),CBMGL1(MC),
     &  BRADC1(MC),BCONC1(MC),NBCAS2,IBCGS2(MC),IBCGF2(MC),CBMGS2(MC),
     &  CBMGL2(MC),BRADC2(MC),BCONC2(MC),NBCAS3,IBCGS3(MC),IBCGF3(MC),
     &  CBMGS3(MC),CBMGL3(MC),BRADC3(MC),BCONC3(MC)
      COMMON/BL3TYPE/IBCGT1(MC),IBCGT2(MC),IBCGT3(MC)

C Descriptive label for a zone load or casual gain types.
      common/bloadlabel/blodlabel(7)

      CHARACTER dmdsdesc*248,outs*124,bdmds*72
      CHARACTER*6 blodlabel

      call edisp(itru,' ')
      call edisp(itru,'Notes: ')
      call edisp248(itru,dmdsdesc,72)
      WRITE(outs,'(A,I3)')' No. of Weekday demands =',NBCAS1
      call edisp(itru,outs)
      IF(NBCAS1.GT.0)THEN
        call edisp(itru,
     &  ' Gain Type Start Finish Sensible  Latent    Radiant   Convec')
        call edisp(itru,
     &  '           Hour  Hour   Magn. (W) Magn. (W) Frac      Frac')
        DO 1270 I=1,NBCAS1
          if(IBCGT1(i).gt.0)then
            WRITE(outs,5360)I,blodlabel(IABS(IBCGT1(I))),'W  ',
     &      IBCGS1(I),IBCGF1(I),CBMGS1(I),CBMGL1(I),BRADC1(I),BCONC1(I)
          else
            WRITE(outs,5360)I,blodlabel(IABS(IBCGT1(I))),'Wm2',
     &      IBCGS1(I),IBCGF1(I),CBMGS1(I),CBMGL1(I),BRADC1(I),BCONC1(I)
          endif
          call edisp(itru,outs)
1270    CONTINUE
      ENDIF

      call edisp(itru,' ')
      WRITE(outs,'(A,I3)')' No. of Saturday demands =',NBCAS2
      call edisp(itru,outs)
      IF(NBCAS2.GT.0)THEN
        call edisp(itru,
     &  ' Gain Type Start Finish Sensible  Latent    Radiant   Convec')
        call edisp(itru,
     &  '           Hour  Hour   Magn. (W) Magn. (W) Frac      Frac')
        DO 1290 I=1,NBCAS2
          if(IBCGT2(i).gt.0)then
            WRITE(outs,5360)I,blodlabel(IABS(IBCGT2(I))),'W  ',
     &      IBCGS2(I),IBCGF2(I),CBMGS2(I),CBMGL2(I),BRADC2(I),BCONC2(I)
          else
            WRITE(outs,5360)I,blodlabel(IABS(IBCGT2(I))),'Wm2',
     &      IBCGS2(I),IBCGF2(I),CBMGS2(I),CBMGL2(I),BRADC2(I),BCONC2(I)
          endif
          call edisp(itru,outs)
1290    CONTINUE
      ENDIF

      call edisp(itru,' ')
      WRITE(outs,'(A,I3)')' No. of Sunday demands =',NBCAS3
      call edisp(itru,outs)
      IF(NBCAS3.GT.0)THEN
        call edisp(itru,
     &  ' Gain Type Start Finish Sensible  Latent    Radiant   Convec')
        call edisp(itru,
     &  '           Hour  Hour   Magn. (W) Magn. (W) Frac      Frac')
        DO 1295 I=1,NBCAS3
          if(IBCGT3(i).gt.0)then
            WRITE(outs,5360)I,blodlabel(IABS(IBCGT3(I))),'W  ',
     &      IBCGS3(I),IBCGF3(I),CBMGS3(I), CBMGL3(I),BRADC3(I),BCONC3(I)
          else
            WRITE(outs,5360)I,blodlabel(IABS(IBCGT3(I))),'Wm2',
     &      IBCGS3(I),IBCGF3(I),CBMGS3(I), CBMGL3(I),BRADC3(I),BCONC3(I)
          endif
          call edisp(itru,outs)
1295    CONTINUE
      ENDIF

5360  FORMAT(I4,1x,A6,A3,I5,I3,F9.1,F9.1,F11.2,F11.2)

      RETURN
      END
