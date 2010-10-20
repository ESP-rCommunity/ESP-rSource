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

C edoptic.F contains facilities to edit and manipulate optical properties
C databases:
C  EDOPT:   High level select and control
C  EDITOPT: Edits one set of optical properties via common block. 
C  EMKOPTD: Writes a item to the glazing optical database.
C  OPT2ED:  Takes the current GOPT common and copies it into GOPTED.
C  ED2OPT:  Takes the current GOPTED common and copies it back into GOPT.
C  CLROPT:  Clears common block GOPTED and sets it up for a new set.
C  IMPOPT:  Imports an optical set from LBL WINDOW 4.1 or 5.1 or 5.2
  
C ************* EDOPT 
C EDOPT Allow user to select and edit an optical type.
C Because of only one set of optical properties is active
C at any point in time it is necessary to use a temporary
C file to manage the reading and saving of this db.

      SUBROUTINE EDOPT(IER)
#include "building.h"
#include "epara.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure database)
C LOPTDB,IOPTDB (for optical database)
C LPRFDB,IPRODB (for event profile database)
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/GPICK/GS(MGOPT),nopt
      common/user/browse
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit
      CHARACTER GS*52
      DIMENSION OPTITM(35),SALT(9),IVALSS(9),IGSET(MGOPT)
      CHARACTER H*72,OPTITM*54,DSOPT*12
      CHARACTER SOPT*12,GTYPE*12,GDESCR*36,KEY*1,loptdbl*144
      character cpsopt*12,cpgdescr*36,SALT*48,layout*7
      CHARACTER xfile*144,tg*1,delim*1
      character OUTSTR*124,lfil*72,DFILE*72,word*24,outs248*248
      character lworking*144,fs*1
      dimension RVA(20),IVA(24)

      logical moddb,browse,OK,DOK,edt
      integer lndbp   ! for length of standard database path
      integer lnwkg   ! for length of working file name
      integer IOW     ! for radio button
      logical unixok  ! to check for database path file separators
      integer NITMS,INO ! max items and current menu item

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      IER=0
      moddb = .false.

C Create a menu showing the available database items.  Allow user to
C select one and then edit/list details.  Setup for multi-page menu.
      MHEAD=0
      MCTL=5
      ILEN=nopt
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   3  ILEN=nopt
      INO=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=0
      DO 20 IM=1,ILEN
        IF(IM.GE.IST.AND.(IM.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(M,KEY,IER)
          WRITE(OPTITM(M),'(a1,1x,a)')KEY,GS(IM)
        ENDIF
  20  CONTINUE

C Number of actual items displayed.
      NITMS=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        OPTITM(M+1)='  _____________________________  '
      ELSE
        WRITE(OPTITM(M+1),15)IPM,MPM 
   15   FORMAT     ('0 -----Page: ',I2,' of ',I2,' -------')
      ENDIF
      OPTITM(M+2)=  '* import/add/delete/copy element '
      OPTITM(M+3)=  '! list optical properties db     '
      OPTITM(M+4)=  '? help                           '
      OPTITM(M+5)=  '- exit this menu                 '
      INO=-4

C Help text for this menu.
  2   H(1)='Optical properties are used for transparent constructions'
      H(2)='as well as to support daylighting calculations and the'
      H(3)='control of casual gains.  All data is used except for '
      H(4)='that which is marked as (documentation).'
      H(5)=' '
      H(6)='For QA purposes you may fill in the surface solar abs'
      H(7)='& refl values to match the MLC surface properties.'
      H(8)='The Uvalue is for documentation only. Similarly, '
      H(9)='solar heat gain coefficients are not used by ESP-r but '
      H(10)='can be included for documentation purposes. '
      H(11)=' '
      H(12)='The angular reflections are derived from the overall '
      H(13)='solar transmission and layer absorptions. '
      H(14)=' '
      H(15)='The import function currently recognises files from '
      H(16)='the LBL program WINDOW 4.1 Here is a step by step guide'
      H(17)='to importing optical data. '
      H(18)=' '
      H(19)='1) Start up W4.exe and use the Function 5 key to enter '
      H(20)=' the glazing system library. '
      H(21)=' '
      H(22)='2) If you have existing glazing systems select one and '
      H(23)=' ensure that angular properties have been calculated'
      H(24)=' via Function 9 key. If the angular properties have not'
      H(25)=' been calculated press A to turn on angular calcs.'
      H(26)=' This should fill up the table. Type escape to return'
      H(27)=' to the main W4 window. For safety type alt S to save'
      H(28)=' the current description. Then then alt P to print'
      H(29)=' the data for the currently highlighted glazing system.'
      H(30)=' '
      H(31)=' You will be asked if it is Library or Detail, select '
      H(32)=' Detail and then give it a file name. It may take a few'
      H(33)=' moments for it to compelete the calculations before'
      H(34)=' it finishes writing the file.'
      H(35)=' '
      H(36)='3) If you want to create a new glazing system type A for'
      H(37)=' Add and then either N for New or C for Copy. Hint:'
      H(38)=' scan the Glass Lib via Function 3 to confirm the'
      H(39)=' glass code numbers you want to use first.'
      H(40)=' Double check the layer thickness is correct. In W4 (as'
      H(41)=' in esp-r) layer 1 is at the outside face and sub-'
      H(42)=' sequent layers are towards the room. Once the layers'
      H(43)=' have been defined press the space key to force initial'
      H(44)=' calculations. Escape back to the main menu, and then'
      H(45)=' type Function 9 to calculate angular properties.'
      H(46)=' Once this is done escape back to the main menu and use'
      H(47)=' alt S to save and alt P to print (as described above).'
      H(48)=' '
      H(49)='4) Edit the reports from W4 to remove the null character'
      H(50)=' before the word Temperature Distribution.'
      H(51)=' '
      H(52)='5) Transfer edited report to esp-r folder (use dos2unix'
      H(53)=' if necessary to convert line endings). In the Project'
      H(54)=' Manger use the import option in the optical database  '
      H(55)=' facility and supply the W4 print file name. A'
      H(56)=' correct translation should result in a graph of the'
      H(57)=' properties. Change the id string and description and'
      H(58)=' check that the number of layers is correct.'

      CALL EMENU('Optical Properties db',OPTITM,NITMS,INO)

      IF(INO.EQ.NITMS)THEN

C Check for changes/ask user to save.
        if(moddb)then
          H(1)='In order to reflect changes you have made in this db'
          H(2)='in the current model you will have to rebuild the '
          H(3)='zone construction and tmc files.  This is in option '
          H(4)='`Construction` of the `model definition` menu.'
          CALL PHELPD('optical db update',4,'-',0,0,IER)
          moddb = .false.
        endif
        RETURN
      ELSEIF(INO.EQ.NITMS-1)THEN

C Produce help text for the menu.
        CALL PHELPD('optics db',58,'-',0,0,IER)
      ELSEIF(INO.EQ.NITMS-2)THEN

C List database.
        h(1)='You can direct optical details to file or display'
        CALL EASKABC(' Reporting to:',' ',
     &      'text feedback','file','continue',irpt,1)
        if(irpt.eq.1)then
          itu = iuout
        elseif(irpt.eq.2)then
          itu = ixunit
          write(xfile,'(a)') 'optical_listing.txt'  ! initial file name
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','opt db text',IER)

C If user canceled the listing the reset unit to iuout and loop back.
          if(ier.eq.-3)then
            itu = iuout
            goto 3
          endif
          write(outs248,'(2a)') 'In the optical database: ',
     &      LOPTDB(1:lnblnk(LOPTDB))
          call edisp248(itu,outs248,120)
          call edisp(itu,' ')
        elseif(irpt.eq.3)then
          goto 3
        endif
        SOPT='ALL'
        CALL EROPTDB(1,itu,SOPT,GDESCR,IER)
        if(irpt.eq.2)then

C Toggle the export file closed.
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','opt db',IER)
        endif
      ELSEIF(INO.EQ.NITMS-3)THEN

C Import/Add/Del/Copy if not browsing....
        if(browse)then
          call usrmsg('Modification facilities not available in',
     &                'browse mode. ','P')
          goto 3
        endif
        h(1)='Options: Import reads from Window4.1 screen dumps'
        h(2)='or the reports generated by Window 5.1 or 5.2.'
        h(3)=' '
        h(4)='Note that Window 5.1/2 files have the thickness of the'
        h(5)='layer in an unreadable location so be sure to correct'
        h(6)='the thickness after the import.'
        h(7)=' '
        h(8)='Add/Delete/Copy work on current contents of db. '
        IOW=1
        CALL EASKATOG('Optical actions: ',' ',
     &    'Import Window4 or Window5','Import WIS',
     &    'Add','Delete','Copy','Ignore',' ',IOW,8)
        if(IOW.eq.1)then

C Import (from WINDOW 4.1 or Window 5.1 or 5.2 )....
          call OPT2TMP(ier)
          iwin4=IPRODB
          call IMPOPT(iwin4,IER)
          if(ier.eq.0.and.nopt+1.le.MGOPT)then
            call edisp(iuout,'Please edit the imported item, providing')
            call edisp(iuout,'an `id` string and description...')
            GTYPE='UNKNOWN'
            GDESCR='UNKNOWN'
            call EDITOPT(nopt+1,GTYPE,GDESCR,edt,IER)
            cpsopt=GTYPE
            cpgdescr=GDESCR
            goto 99
          else
            call usrmsg('Optics database can hold no more data sets.',
     &      'Please revise the database.','W')
          endif
        elseif(IOW.eq.2)then

C Ask for WIS report file and then attempt to scan it.
          lfil='/tmp/testtag.txt'
          H(1)='This is a WIS report file. You should provide the'
          H(2)='full path to this file.'
          DFILE='wis.txt'
          CALL EASKS(lfil,'WIS file name?',
     &     ' ',72,DFILE,'optical properties import file',IER,2)
          IER=0
          iwin4=IPRODB
          CALL EFOPSEQ(iwin4,lfil,1,IER)
  77      CALL STRIPC(iwin4,OUTSTR,99,ND,1,'line 1',IER)
          IF(IER.NE.0)goto 78
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','tag',IFLAG)
          if(word(1:18).eq.'*sol_o_tran_direct')then
            IRVA=20
            call EGETAGWRA(outstr,K,iwin4,IRVA,RVA,0.00,1.00,'W',
     &        'solar outside direct trans',IER)

C Debug.
C            write(6,*) 'sol out ',RVA

          elseif(word(1:18).eq.'*sol_i_tran_direct')then
            IRVA=20
            call EGETAGWRA(outstr,K,iwin4,IRVA,RVA,0.00,1.00,'W',
     &        'solar inside direct trans',IER)

C Debug.
C            write(6,*) 'sol in',RVA

          elseif(word(1:8).eq.'*sol_int')then
            IRVA=23
            call EGETAGWIA(outstr,K,iwin4,IRVA,IVA,0,20,'W',
     &        'solar int list',IER)

C Debug.
C            write(6,*) 'sol int',IVA

          endif
          goto 77
 78       CALL ERPFREE(iwin4,ios)
        elseif(IOW.eq.3)then

C Add a new item to the optical db
          if(nopt+1.le.MGOPT)then
            call OPT2TMP(ier)
            SALT(1)='Out |glass| Inside                              '
            SALT(2)='Out |glass|air|blind| Inside                    '
            SALT(3)='Out |glass|air|glass| Inside                    '
            SALT(4)='Out |glass|air|glass|air|blind| Inside          '
            SALT(5)='Out |glass|air|blind|air|glass| Inside          '
            SALT(6)='Out |glass|air|glass|air|glass| Inside          '
            SALT(7)='Out |glass|air|glass|air|glass||air|blind Inside'
            SALT(8)='Out |glass|air|glass|air|blind|air|glass| Inside'
            SALT(9)='Other                                           '

            H(1)='The layout of glazing, air gaps and blinds can'
            H(2)='be selected from this list or an alternative '
            H(3)='defined.'
            H(4)=' '
            H(5)='A cancel request leaves the model unchanged.'
            IX=1
            CALL EPICKS(IX,IVALSS,' ','Typical glazing types:',
     &        48,9,SALT,'Typical glazing types',IER,3)
            IOS=IVALSS(1)
            nl=0
            if(IOS.eq.9)then
              nl=1
              CALL EASKI(nl,' ',' Number of layers in the new set ?',
     &          1,'F',ME,'F',1,'opt set layers',IERI,5)
              if(ieri.eq.-3) then
                INO=-4
                GOTO 3
              endif
              layout='g------'
            elseif(IOS.eq.1)then
              nl=1
              layout='g------'
            elseif(IOS.eq.2)then
              nl=3
              layout='gab----'
            elseif(IOS.eq.3)then
              nl=3
              layout='gag----'
            elseif(IOS.eq.4)then
              nl=5
              layout='gagab--'
            elseif(IOS.eq.5)then
              nl=5
              layout='gabag--'
            elseif(IOS.eq.6)then
              nl=5
              layout='gagag--'
            elseif(IOS.eq.7)then
              nl=7
              layout='gagagab'
            elseif(IOS.eq.8)then
              nl=7
              layout='gagabag'
            endif
            call CLROPT(nl,layout)
            GTYPE='UNKNOWN'
            GDESCR='UNKNOWN'
            call EDITOPT(nopt+1,GTYPE,GDESCR,edt,IER)
            cpsopt=GTYPE
            cpgdescr=GDESCR
            goto 99
          else
            call usrmsg('Optics database can hold no more data sets.',
     &      'Please revise the database.','W')
          endif
        elseif(IOW.eq.4)then

C Delete an optical item. First pick then copy from temp file
C one by one, skipping the selected item. To read temp optical file
C as the source optical db temporarily swap file unit numbers. Open
C and then delete the original optical file.
          H(1)='Select on of the existing glazings to delete.'
          IX=1
          CALL EPMENSV
          CALL EPICKS(IX,IGSET,' ','Optical set to delete:',
     &      52,nopt,GS,'Opticals (select one to delete)',IER,1)
          CALL EPMENRC
          IFOC=IGSET(1)
          if(ifoc.ne.0)then
            dok=.false.
            h(1)='Last chance to avoid deleting the optical set. '
            CALL ASKOK(' ',' Are you sure?',OK,dok,1)
            IF(.NOT.OK)GOTO 2
            call OPT2TMP(ier)
            WRITE(DSOPT,'(A)')GS(IFOC)(1:12)
            IWHICH=IFOC
            moddb=.true.
            goto 99
          endif
        elseif(IOW.eq.5)then

C Copy an optical item. Backup existing db then pick item, edit it and
C then add it at the end during copying data back from tmp db.
          if(nopt+1.le.MGOPT)then
            call OPT2TMP(ier)
            H(1)='Select on of the existing glazings to copy.'
            IX=1
            CALL EPMENSV
            CALL EPICKS(IX,IGSET,' ','Optical set to copy:',
     &        52,nopt,GS,'Opticals (select one to copy)',IER,1)
            CALL EPMENRC
            IFOC=IGSET(1)
            if(ifoc.ne.0)then
              WRITE(DSOPT,'(A)')GS(IFOC)(1:12)
              write(SOPT,'(A)')GS(IFOC)(1:12)
              write(GDESCR,'(a)')GS(IFOC)(16:50)
              call EROPTDB(ITRC,iuout,SOPT,GDESCR,IER)
              write(GTYPE,'(a,a)')'x',GS(IFOC)(1:11)
              write(GDESCR,'(a,a)')'x',GS(IFOC)(16:50)
              CALL OPT2ED
              call EDITOPT(nopt+1,GTYPE,GDESCR,edt,IER)
              cpsopt=GTYPE
              cpgdescr=GDESCR
              IWHICH=IFOC
              goto 99
            endif
          else
            call usrmsg('Optics database can hold no more data sets.',
     &      'Please revise the database.','W')
          endif
        endif
      ELSEIF(INO.EQ.NITMS-4)THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(INO.GT.MHEAD.AND.INO.LT.(NITMS-MCTL+1))THEN

C Edit item identified by KEYIND - get into common and then into edit common
        if(.NOT.browse)call OPT2TMP(ier)
        CALL KEYIND(NITMS,INO,IFOC,IO)
        write(DSOPT,'(A)')GS(IFOC)(1:12)
        write(SOPT,'(A)')GS(IFOC)(1:12)
        call EROPTDB(ITRC,iuout,SOPT,GDESCR,IER)
        CALL OPT2ED
        call EDITOPT(IFOC,SOPT,GDESCR,edt,IER)
        if(edt)then
          moddb=.true.
        else
          goto 3
        endif
        cpsopt=SOPT
        cpgdescr=GDESCR
        IOW=6
        if(browse)then
          call usrmsg('In browse mode so no changes made to the',
     &                'optics db. ','P')
          goto 3
        endif
        goto 99
      else
        INO=-4
        GOTO 3
      ENDIF
      INO=-4
      GOTO 3

C Deal with file manipulation (shifting data from temp file to db).
C Open the file based on the current value of whichdbpath variable.
 99   CALL ERPFREE(IOPTDB,ISTAT)

C Check the value of whichdbpath variable to see what to do with
C this file. If local or absolute path then use efopseq. If in
C the standard location then create lworking which has path
C prepended.
      if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
        CALL EFOPSEQ(IOPTDB,LOPTDB,1,IER)
        loptdbl=LOPTDB
      elseif(ipathoptdb.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    loptdb(1:lnblnk(loptdb))

C Debug.
C        write(6,*) 'EDOPT working optics ',lworking

        CALL EFOPSEQ(IOPTDB,lworking,1,IER)
        loptdbl=lworking
      endif

      CALL EFDELET(IOPTDB,ISTAT)
      ioptl=ioptdb
      ioptdb=ifil+1
      LOPTDB='tmpopt'

      call usrmsg('Updating db...',' ','-')
      CALL EFOPSEQ(ioptl,loptdbl,4,IER)
      IF(IER.NE.0)THEN
        IER=1
        call usrmsg('Problem managing optical db files. ',' ','W')
        goto 3
      ENDIF
      if(nopt+1.ge.MGOPT)then
        call usrmsg('No more optical items can be added.',' ','W')
        goto 3
      endif

      write(ioptl,'(a)')'# optical properties db for transparent,'
      write(ioptl,'(a)')'# constructions (TMC) and casual gain control'
      write(ioptl,'(a)')'# '
      write(ioptl,'(a)')'# Glazed info follows:'

      if(IOW.eq.1.or.IOW.eq.2.or.IOW.eq.3.or.IOW.eq.5)then

C Add a new optical set by reading items from temp db and then writing
C these and the new (added/copied/imported) item to the new db file.

        do 41 ijopt=1,nopt
          write(SOPT,'(A)')GS(ijopt)(1:12)
          call EROPTDB(0,iuout,SOPT,GDESCR,IER)
          CALL EMKOPTD(ioptl,SOPT,GDESCR,IER)
  41    continue
        call ED2OPT
        CALL EMKOPTD(ioptl,cpsopt,cpgdescr,IER)
        nopt=nopt+1
        WRITE(GS(nopt),'(A12,3X,A36)')cpsopt,cpgdescr
        ILEN=nopt
        IPACT=CREATE
        CALL EKPAGE(IPACT)
        moddb=.true.
      elseif(IOW.eq.4)then

C Read items from temp db and then write all but deleted item to the
C new db.
        do 43 ijopt=1,nopt
          write(SOPT,'(A)')GS(ijopt)(1:12)
          call EROPTDB(0,iuout,SOPT,GDESCR,IER)
          if(SOPT.ne.DSOPT)CALL EMKOPTD(ioptl,SOPT,GDESCR,IER)
  43    continue

C Pack the descriptive list.
        DO 891 IVV=IWHICH,nopt-1
          GS(IVV)=GS(IVV+1)
  891   CONTINUE
        nopt=nopt-1
        ILEN=nopt
        IPACT=CREATE
        CALL EKPAGE(IPACT)
        moddb=.true.
      elseif(IOW.eq.6)then

C Editing an item.
        do 45 ijopt=1,nopt
          write(SOPT,'(A)')GS(ijopt)(1:12)
          call EROPTDB(0,iuout,SOPT,GDESCR,IER)
          if(SOPT.eq.DSOPT)then
            call ED2OPT
            CALL EMKOPTD(ioptl,cpsopt,cpgdescr,IER)
          else
            CALL EMKOPTD(ioptl,SOPT,GDESCR,IER)
          endif
  45    continue
      endif

C Free the files, restore the optics db name and file unit.
      CALL ERPFREE(ioptl,ISTAT)
      ioptdb=ioptl
      LOPTDB=loptdbl
      CALL ERPFREE(ioptdb,ISTAT)
      call usrmsg('Updating db...done.',' ','-')
      goto 3

      END

C ****** OPT2TMP(ier)
C Copy current optical db to temporary file.
      SUBROUTINE OPT2TMP(IER)
      COMMON/FILEP/IFIL
      CHARACTER topt*72,SOPT*12,GDESCR*36

      logical XST

C Copy existing db into temporary area.
      call usrmsg('Making temporary copy of db... ',' ','-')
      topt='tmpopt'
      iopt=ifil+1
      CALL ERPFREE(iopt,ISTAT)
      INQUIRE (FILE=topt,EXIST=XST)
      if(XST)then
        CALL EFOPSEQ(iopt,topt,1,IER)
        CALL EFDELET(iopt,ISTAT)
      endif
      CALL EFOPSEQ(iopt,topt,4,IER)
      IF(IER.NE.0)THEN
        IER=1
        RETURN
      ENDIF
      write(iopt,'(a)')'# optical properties db for transparent,'
      write(iopt,'(a)')'# constructions (TMC) and casual gain control'
      write(iopt,'(a)')'# '
      write(iopt,'(a)')'# Glazed info follows:'

C Make a copy of current db.
      SOPT='TMP'
      call EROPTDB(0,iopt,SOPT,GDESCR,IER)
      CALL ERPFREE(iopt,ISTAT)
      call usrmsg('Making temporary copy of db...done.',' ','-')
      return
      end

C ******************* EDITOPT 
C EDITOPT edits one set of optical properties via common block. 
      SUBROUTINE EDITOPT(IW,GTYPE,GDESCR,edited,IER)
#include "building.h"
      
      integer lnblnk  ! function definition

      common/pophelp/h(60)
      COMMON/GOPTED/DGED(5),HGED(5),UVALED,VTRNED,NTLED,ABED(ME,5),
     &  RFED(ME),SRFED,SABED
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/GPICK/GS(MGOPT),nopt

C ilabs is string arry for layer insert/delet selection.
      DIMENSION ITEMS(32),sr(5),ilabs(ME),IGSET(ME)
      CHARACTER GTYPE*12,GDESCR*36,h*72,ITEMS*43,hold*72
      CHARACTER KEY*1,T12*12,ilabs*43
      CHARACTER GS*52,ETEXT*72,GTEXT*72,temp*16
      logical edited
      integer nitms,INO ! max items and current menu item

#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer iigr,iigr2,iigt4,iix1,iiy1,iix2,iiy2,iix3,iiy3
      integer iix4,iiy4,iix5,iiy5,iid1,iid2,iid3,iid4,iix,iiy
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iigr,iigr2,iigt4,iix1,iiy1,iix2,iiy2,iix3,iiy3
      integer*8 iix4,iiy4,iix5,iiy5,iid1,iid2,iid3,iid4,iix,iiy
#endif

      IER=0
      edited=.false.
   3  INO=-2

C re-establish reflectance tran+absorbs
      sr(1)=1.0-DGED(1)
      sr(2)=1.0-DGED(2)
      sr(3)=1.0-DGED(3)
      sr(4)=1.0-DGED(4)
      sr(5)=1.0-DGED(5)
      do 37 i=1,ntled
        sr(1)=sr(1)-ABED(I,1)
        sr(2)=sr(2)-ABED(I,2)
        sr(3)=sr(3)-ABED(I,3)
        sr(4)=sr(4)-ABED(I,4)
        sr(5)=sr(5)-ABED(I,5)
  37  continue

C Warn user if reflectance is low or negative.
      if(sr(1).lt.0.001.or.sr(2).lt.0.001.or.sr(3).lt.0.001.or.
     &   sr(4).lt.0.001.or.sr(5).lt.0.001)then
        call usrmsg('One or more of the layer reflectances is near',
     &    'zero or negative. Please adjust properties.','W')
      endif
      write(items(1),'(a,a)')    'a type   : ',GTYPE
      write(items(2),'(a,a)')    'b descrip: ',GDESCR(1:30)
      write(items(3),'(a,f6.2)') 'c visible transmittance : ',VTRNED
      write(items(4),'(a,2f5.2)')'d solar abs & refl (for docu): ',
     &                           SABED,SRFED
      write(items(5),'(a,f6.2)') 'e U-value (for docu): ',UVALED
      items(6) =           '  __________________________________ '
      items(7) =           '          @  0deg  40deg 55deg 70deg 80deg'
      write(items(8),'(a,5f6.3)') 'f direc trn:',DGED(1),DGED(2),
     &  DGED(3),DGED(4),DGED(5)
      write(items(9),'(a,5f6.3)') '  reflect. :',sr(1),sr(2),
     &  sr(3),sr(4),sr(5)
      write(items(10),'(a,5f5.2)')'g heat gain (doc):',HGED(1),HGED(2),
     &  HGED(3),HGED(4),HGED(5)
      items(11) =               '  __________________________________ '
      write(items(12),'(a,i2)') '  layers: ',NTLED
      items(13) =          '   refrac. abs@0 abs@40 abs@55 @70 @80'
      ila=1
      ilabs(ila) =         '   refrac. abs@0 abs@40 abs@55 @70 @80'
      DO 44 IL=1,NTLED
        CALL EMKEY(IL+7,KEY,IER)
        write(items(13+IL),'(a1,3x,6F6.3)')KEY,RFED(IL),ABED(IL,1),
     &    ABED(IL,2),ABED(IL,3),ABED(IL,4),ABED(IL,5)
        ila=ila+1
        write(ilabs(ila),'(3x,6F6.3)')RFED(IL),ABED(IL,1),
     &    ABED(IL,2),ABED(IL,3),ABED(IL,4),ABED(IL,5)
  44  CONTINUE
      nitms=13+NTLED+4
      items(nitms-3) =  '  ___________________________ '
      items(nitms-2) =  '1 add/delete/copy layer    '
      items(nitms-1) =  '? help                        '
      items(nitms) =  '- exit this menu                '

C (re)draw optical properties.
      IF(MMOD.EQ.8)THEN
        XMIN=0.0
        XMAX=90.0
        YMIN=0.
        YMAX=1.0
        CALL startbuffer()
        call win3d(menuchw,12,30,5,3,igl,igr,igt,igb,igw,igwh)
        igw=igr-igl
        igwh=igb-igt
        igwid=igw
        igheight=igwh
        call axiscale(igwid,igheight,XMIN,XMAX,YMIN,YMAX,xsc,ysc,sca,
     &              Xadd,Yadd)

C Stuff into static variables for later access and draw axis.
        call linescale(igl,Xadd,xsc,igb,Yadd,ysc)
        call vrtaxis(YMIN,YMAX,igl,igb,igt,Yadd,ysc,0,0,'Value')
        call horaxis(XMIN,XMAX,igl,igr,igb,Xadd,xsc,0,
     &    'Degrees from normal')

        WRITE(ETEXT,'(4a)') 'Type: ',GTYPE(1:lnblnk(GTYPE)),
     &    ' descripion: ',GDESCR(1:34)
        GTEXT=ETEXT
        iside=1
        isize=0
        ifont=1
        call viewtext(gtext,iside,isize,ifont)

C Draw visible as a circle.
        call u2pixel(0.0,VTRNED,iix,iiy)
        CALL ecirc(iix,iiy,3,1)
        iid1=igr+10; iid2=igt;
        CALL ecirc(iid1,iid2,3,1)
        write(temp,'(A)')' Visible trn'
        iid1=igr+25; iid2=igt;
        CALL wstxpt(iid1,iid2,temp)

C Draw direct transmission.
        iid1=igr; iid2=igt+15; iid3=igr+20; iid4=igt+15;
        call edwline(iid1,iid2,iid3,iid4)
        write(temp,'(A)')' Direct trn'
        iid1=igr+25; iid2=igt+15;
        CALL wstxpt(iid1,iid2,temp)
        call u2pixel(0.0,DGED(1),iix1,iiy1)
        call u2pixel(40.0,DGED(2),iix2,iiy2)
        call u2pixel(55.0,DGED(3),iix3,iiy3)
        call u2pixel(70.0,DGED(4),iix4,iiy4)
        call u2pixel(80.0,DGED(5),iix5,iiy5)
        call edwline(iix1,iiy1,iix2,iiy2)
        call edwline(iix2,iiy2,iix3,iiy3)
        call edwline(iix3,iiy3,iix4,iiy4)
        call edwline(iix4,iiy4,iix5,iiy5)

C Draw reflection.
        iid1=igr; iid2=igt+30; iid3=igr+20; iid4=igt+30;
        call eswline(iid1,iid2,iid3,iid4)
        write(temp,'(A)')' Reflection'
        iid1=igr+25; iid2=igt+30;
        CALL wstxpt(iid1,iid2,temp)
        call u2pixel(0.0,sr(1),iix1,iiy1)
        call u2pixel(40.0,sr(2),iix2,iiy2)
        call u2pixel(55.0,sr(3),iix3,iiy3)
        call u2pixel(70.0,sr(4),iix4,iiy4)
        call u2pixel(80.0,sr(5),iix5,iiy5)
        call eswline(iix1,iiy1,iix2,iiy2)
        call eswline(iix2,iiy2,iix3,iiy3)
        call eswline(iix3,iiy3,iix4,iiy4)
        call eswline(iix4,iiy4,iix5,iiy5)

C Draw absorption for each layer.
        iigr=igr
        iigt4=igt+45
        iigr2=igr+20
        call edline(iigr,iigt4,iigr2,iigt4,3)
        write(temp,'(A)')' Absorb'
        iid1=igr+25; iid2=igt+45;
        CALL wstxpt(iid1,iid2,temp)
        DO 46 IL=1,NTLED
          call u2pixel(0.0,ABED(IL,1),iix1,iiy1)
          call u2pixel(40.0,ABED(IL,2),iix2,iiy2)
          call u2pixel(55.0,ABED(IL,3),iix3,iiy3)
          call u2pixel(70.0,ABED(IL,4),iix4,iiy4)
          call u2pixel(80.0,ABED(IL,5),iix5,iiy5)
          call edline(iix1,iiy1,iix2,iiy2,3)
          call edline(iix2,iiy2,iix3,iiy3,3)
          call esymbol(iix2,iiy2,IL+3,1)
          call edline(iix3,iiy3,iix4,iiy4,3)
          call edline(iix4,iiy4,iix5,iiy5,3)
          call esymbol(iix3,iiy3,IL+3,1)
  46    CONTINUE
        call forceflush()
      endif

C Help text for this menu.
      H(1)='Optical properties are used for transparent constructions'
      H(2)='as well as to support daylighting calculations and the'
      H(3)='control of casual gains.  All data is used except for '
      H(4)='that which is marked as (documentation).'
      H(5)=' '
      H(6)='For QA purposes you may fill in the surface solar abs'
      H(7)='& refl values to match the MLC surface properties.'
      H(8)='The Uvalue is for documentation only. Similarly, '
      H(9)='solar heat gain coefficients are not used by ESP-r but '
      H(10)='can be included for documentation purposes. '
      H(11)=' '
      H(12)='The angular reflections are derived from the overall '
      H(13)='solar transmission and layer absorptions. '
      H(14)=' '
      H(15)='The import function currently recognises files from '
      H(16)='the LBL program WINDOW 4.1 Here is a step by step guide'
      H(17)='to importing optical data. '
      H(18)=' '
      H(19)='1) Start up W4.exe and use the Function 5 key to enter '
      H(20)=' the glazing system library. '
      H(21)=' '
      H(22)='2) If you have existing glazing systems select one and '
      H(23)=' ensure that angular properties have been calculated'
      H(24)=' via Function 9 key. If the angular properties have not'
      H(25)=' been calculated press A to turn on angular calcs.'
      H(26)=' This should fill up the table. Type escape to return'
      H(27)=' to the main W4 window. For safety type alt S to save'
      H(28)=' the current description. Then then alt P to print'
      H(29)=' the data for the currently highlighted glazing system.'
      H(30)=' '
      H(31)=' You will be asked if it is Library or Detail, select '
      H(32)=' Detail and then give it a file name. It may take a few'
      H(33)=' moments for it to compelete the calculations before'
      H(34)=' it finishes writing the file.'
      H(35)=' '
      H(36)='3) If you want to create a new glazing system type A for'
      H(37)=' Add and then either N for New or C for Copy. Hint:'
      H(38)=' scan the Glass Lib via Function 3 to confirm the'
      H(39)=' glass code numbers you want to use first.'
      H(40)=' Double check the layer thickness is correct. In W4 (as'
      H(41)=' in esp-r) layer 1 is at the outside face and sub-'
      H(42)=' sequent layers are towards the room. Once the layers'
      H(43)=' have been defined press the space key to force initial'
      H(44)=' calculations. Escape back to the main menu, and then'
      H(45)=' type Function 9 to calculate angular properties.'
      H(46)=' Once this is done escape back to the main menu and use'
      H(47)=' alt S to save and alt P to print (as described above).'
      H(48)=' '
      H(49)='4) Edit the reports from W4 to remove the null character'
      H(50)=' before the word Temperature Distribution.'
      H(51)=' '
      H(52)='5) Transfer edited report to esp-r folder (use dos2unix'
      H(53)=' if necessary to convert line endings). In the Project'
      H(54)=' Manger use the import option in the optical database  '
      H(55)=' facility and supply the W4 print file name. A'
      H(56)=' correct translation should result in a graph of the'
      H(57)=' properties. Change the id string and description and'
      H(58)=' check that the number of layers is correct.'

      CALL EMENU('Optical Property Set',ITEMS,nitms,INO)
      IF(INO.EQ.nitms)THEN
        RETURN
      ELSEIF(INO.EQ.nitms-1)THEN

C List help text for the menu.
        CALL PHELPD('optics set',58,'-',0,0,IER)
      ELSEIF(INO.EQ.nitms-2)THEN
        H(1)='For insert the point of insertion will be at the location'
        H(2)='selected. For copy the layer copied will be placed at the'
        H(3)='inside face.'
        CALL EASKABCD('Layer actions: ',' ','Add (insert)',
     &    ' Delete ','Copy','Ignore',IOW,3)
        if(IOW.eq.1)then
          if(NTLED.lt.ME)then
            IX=1
            H(1)='Select a position to add/insert a new layer...'
            CALL EPICKS(IX,IGSET,' ','Point of insertion:',43,NTLED+1,
     &        ilabs,'Optical layer (select insertion point)',IER,1)
            IL=IGSET(1)-1
            if(IL.ne.0)then
              NTLED=NTLED+1
              I=NTLED+1
   45         CONTINUE
              I=I-1
              RFED(I)=RFED(I-1)
              ABED(I,1)=ABED(I-1,1)
              ABED(I,2)=ABED(I-1,2)
              ABED(I,3)=ABED(I-1,3)
              ABED(I,4)=ABED(I-1,4)
              ABED(I,5)=ABED(I-1,5)
              IF(I.GT.IL+1)GOTO 45
            endif
            edited=.true.
          else
            call usrmsg('Sorry, can`t add more layers..',' ','W')
          endif
        elseif(IOW.eq.2)then
          IX=1
          H(1)='Select a layer to delete...'
          CALL EPICKS(IX,IGSET,' ','Layer to delete:',43,NTLED,
     &      ilabs,'Optical layer (select one to delete)',IER,1)
          IL=IGSET(1)-1
          if(IL.ne.0.and.NTLED.gt.2)then
            do 791 idv=IL,NTLED-1
              RFED(IDV)=RFED(IDV+1)
              ABED(IDV,1)=ABED(IDV+1,1)
              ABED(IDV,2)=ABED(IDV+1,2)
              ABED(IDV,3)=ABED(IDV+1,3)
              ABED(IDV,4)=ABED(IDV+1,4)
              ABED(IDV,5)=ABED(IDV+1,5)
  791       continue
            NTLED=NTLED-1
            edited=.true.
          endif
        elseif(IOW.eq.3)then
          IX=1
          H(1)='Select a layer to copy. It will become a new inside'
          H(2)='layer.'
          CALL EPICKS(IX,IGSET,' ','Layer to copy:',43,NTLED,
     &      ilabs,'Optical layer (select one to copy)',IER,2)
          IL=IGSET(1)-1
          if(IL.gt.0.and.NTLED.lt.ME)then
            NTLED=NTLED+1
            RFED(NTLED)=RFED(IL)
            ABED(NTLED,1)=ABED(IL,1)
            ABED(NTLED,2)=ABED(IL,2)
            ABED(NTLED,3)=ABED(IL,3)
            ABED(NTLED,4)=ABED(IL,4)
            ABED(NTLED,5)=ABED(IL,5)
            edited=.true.
          endif
        endif
      ELSEIF(INO.EQ.1)THEN
        H(1)='This should be a UNIQUE identifier of up to 12 char '
        H(2)='with no spaces. '
        T12=GTYPE
        CALL EASKS(T12,'Id string ?',' ',12,GTYPE,'opt id',IER,2)
        call st2name(T12,GTYPE)
        WRITE(GS(IW),'(A12,3X,A36)')GTYPE,GDESCR
        edited=.true.
      ELSEIF(INO.EQ.2)THEN
        H(1)='This is an aide memorie for this set of optics. '
        CALL EASKS(GDESCR,'Description ?',' ',36,'new','opt desc',IER,1)
        WRITE(GS(IW),'(A12,3X,A36)')GTYPE,GDESCR
        edited=.true.
      ELSEIF(INO.EQ.3)THEN
        H(1)='The visible transmission is used in daylighting calcs.'
        CALL EASKR(VTRNED,' ','Visable transmission? ',
     &       0.0,'W',0.99,'W',0.72,'vis trn',IER,1)
        edited=.true.
      ELSEIF(INO.EQ.4)THEN
        H(1)='The overall solar reflectance and absorption are '
        H(2)='not yet used in the calculations - documentation only'
        write(hold,'(2F8.3)')SABED,SRFED
        CALL EASKS(hold,'Solar absoption & reflectance?',
     &    '(documentation only): ',40,' 0.  0.','opt sol',IER,2)
        H(1)='The visible transmission is used in daylighting calcs.'
        K=0
        CALL EGETWR(HOLD,K,SABED,0.,0.999,'W','sol abs',IER)
        CALL EGETWR(HOLD,K,SRFED,0.,0.999,'W','sol refl',IER)
        edited=.true.
      ELSEIF(INO.EQ.5)THEN
        H(1)='The U value is currently for documentation only.'
        CALL EASKR(UVALED,'U value','(documentation): ',
     &       0.0,'W',9.99,'W',0.72,'U value',IER,1)
        edited=.true.
      ELSEIF(INO.EQ.8)THEN
        H(1)='The overall solar transmittance at 5 angles (from '
        H(2)='normal @0deg 40deg 55deg 70deg 80deg)'
        write(hold,'(5F7.3)')DGED(1),DGED(2),DGED(3),DGED(4),DGED(5)
        CALL EASKS(hold,'Solar trn @ 0 40 55 70 80 deg:',
     &    ' ',40,'0.819, 0.802, 0.761, 0.621, 0.376','opt sol',IER,2)
        H(1)='The visible transmission is used in daylighting calcs.'
        K=0
        CALL EGETWR(HOLD,K,DGED(1),0.,0.999,'W','dir t @ 0',IER)
        CALL EGETWR(HOLD,K,DGED(2),0.,0.999,'W','dir t @ 40',IER)
        CALL EGETWR(HOLD,K,DGED(3),0.,0.999,'W','dir t @ 55',IER)
        CALL EGETWR(HOLD,K,DGED(4),0.,0.999,'W','dir t @ 70',IER)
        CALL EGETWR(HOLD,K,DGED(5),0.,0.999,'W','dir t @ 80',IER)
        edited=.true.
      ELSEIF(INO.EQ.10)THEN
        H(1)='The overall heat gain (for documentation purposes) at 5 '
        H(2)='angles (from normal @0deg 40deg 55deg 70deg 80deg)'
        write(hold,'(5F7.3)')HGED(1),HGED(2),HGED(3),HGED(4),HGED(5)
        CALL EASKS(hold,'Overall heat gain @ 0 40 55 70 80 deg:',
     &    ' ',40,'0.86, 0.85, 0.80, 0.65, 0.42','total ht gn',IER,2)
        K=0
        CALL EGETWR(HOLD,K,HGED(1),0.,0.999,'W','ht gn @ 0',IER)
        CALL EGETWR(HOLD,K,HGED(2),0.,0.999,'W','ht gn @ 40',IER)
        CALL EGETWR(HOLD,K,HGED(3),0.,0.999,'W','ht gn @ 55',IER)
        CALL EGETWR(HOLD,K,HGED(4),0.,0.999,'W','ht gn @ 70',IER)
        CALL EGETWR(HOLD,K,HGED(5),0.,0.999,'W','ht gn @ 80',IER)
        edited=.true.
      ELSEIF(INO.GT.13.and.INO.LT.NITMS-3)THEN
C Edit a layer.
        IL=INO-13
        H(1)='The Refraction index for glass is usually 1.52.'
        H(2)='Absorption (from normal @0deg 40deg 55deg 70deg 80deg)'
        write(hold,'(f6.3,2x,5F6.3)')RFED(IL),ABED(IL,1),ABED(IL,2),
     &    ABED(IL,3),ABED(IL,4),ABED(IL,5)
        CALL EASKS(hold,'Refraction index, absorb @ 0 40 55 70 80 deg:',
     &   ' ',40,' 1.52  0.149 0.163 0.173 0.179 0.169 ','refr & absorb',
     &   IER,2)
        K=0
        CALL EGETWR(HOLD,K,RFED(IL),0.,2.999,'W','refrac',IER)
        CALL EGETWR(HOLD,K,V1,0.,0.999,'W','ht gn @ 0',IER)
        ABED(IL,1)=V1
        CALL EGETWR(HOLD,K,V2,0.,0.999,'W','ht gn @ 40',IER)
        ABED(IL,2)=V2
        CALL EGETWR(HOLD,K,V3,0.,0.999,'W','ht gn @ 55',IER)
        ABED(IL,3)=V3
        CALL EGETWR(HOLD,K,V4,0.,0.999,'W','ht gn @ 70',IER)
        ABED(IL,4)=V4
        CALL EGETWR(HOLD,K,V5,0.,0.999,'W','ht gn @ 80',IER)
        ABED(IL,5)=V5
        edited=.true.
      ELSE
        GOTO 3
      ENDIF
      GOTO 3

      END

C ******************* EMKOPTD 
C EMKOPTD writes a item to the glazing optical database. Assumption
C that file has already been opened.
      SUBROUTINE EMKOPTD(ITOPT,GTYPE,GDESCR,IER)
#include "building.h"
      integer lnblnk  ! function definition
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB
      CHARACTER GTYPE*12,GDESCR*36

      IER=0
      
C Write out item GTYPE. 
      write(ITOPT,'(a)')
     &  '# 12 char id |  description       | thick | blind'
      write(ITOPT,'(a12,a,a)')GTYPE,'  :',GDESCR(1:lnblnk(GDESCR))
      write(ITOPT,'(a)')
     &  '# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val'
      write(ITOPT,'(a,i3,4f7.3)')'  1 ',NTL,VTRN,SRF,SAB,UVAL
      write(ITOPT,'(a)')
     &  '# direct trn @ 5 angles, total heat gain @ 5 angles'
      write(ITOPT,'(1x,10F6.3)')DG(1),DG(2),DG(3),DG(4),DG(5),HG(1),
     &  HG(2),HG(3),HG(4),HG(5)

C For each layer extract tmc info.
      write(ITOPT,'(a)')
     &  '# refr index, absorption @ 5 angles for each tmc layer'
      DO 44 IL=1,NTL
        write(ITOPT,'(1x,6F6.3)')RF(IL),AB(IL,1),AB(IL,2),AB(IL,3),
     &    AB(IL,4),AB(IL,5)
  44  CONTINUE

      RETURN
      END

C ************* OPT2ED 
C OPT2ED takes the current GOPT common and copies it into GOPTED.
      SUBROUTINE OPT2ED
#include "building.h"
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB
      COMMON/GOPTED/DGED(5),HGED(5),UVALED,VTRNED,NTLED,ABED(ME,5),
     &  RFED(ME),SRFED,SABED

      do 41 k=1,5
        DGED(k)=DG(k)
        HGED(k)=HG(k)
  41  continue
      UVALED=UVAL
      VTRNED=VTRN
      NTLED=NTL
      SRFED=SRF
      SABED=SAB
      do 42 i=1,ME
        RFED(i)=RF(i)
        do 43 j=1,5
          ABED(i,j)=AB(i,j)
  43    continue
  42  continue
      return
      end

C ************* ED2OPT 
C ED2OPT takes the current GOPTED common and copies it back into GOPT.
      SUBROUTINE ED2OPT
#include "building.h"
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB
      COMMON/GOPTED/DGED(5),HGED(5),UVALED,VTRNED,NTLED,ABED(ME,5),
     &  RFED(ME),SRFED,SABED

      do 41 k=1,5
        DG(k)=DGED(k)
        HG(k)=HGED(k)
  41  continue
      UVAL=UVALED
      VTRN=VTRNED
      NTL=NTLED
      SRF=SRFED
      SAB=SABED
      do 42 i=1,ME
        RF(i)=RFED(i)
        do 43 j=1,5
          AB(i,j)=ABED(i,j)
  43    continue
  42  continue
      return
      end

C ************* CLROPT
C CLROPT clears common block GOPTED and sets it up for a new set.
      SUBROUTINE CLROPT(nl,layout)
#include "building.h"
      COMMON/GOPTED/DGED(5),HGED(5),UVALED,VTRNED,NTLED,ABED(ME,5),
     &  RFED(ME),SRFED,SABED
      character layout*7

      do 41 k=1,5
        DGED(k)=0.1
        HGED(k)=0.2
  41  continue
      UVALED=5.4
      VTRNED=0.89
      NTLED=nl
      SRFED=0.07
      SABED=0.11
      do 42 i=1,nl
        if(layout(i:i).eq.'g')then
          RFED(i)=1.52
        elseif(layout(i:i).eq.'a')then
          RFED(i)=1.00
        elseif(layout(i:i).eq.'b')then
          RFED(i)=1.00
        else
          RFED(i)=1.00
        endif
        do 43 j=1,5
          if(layout(i:i).eq.'g')then
            ABED(i,j)=0.1
          elseif(layout(i:i).eq.'a')then
            ABED(i,j)=0.001
          elseif(layout(i:i).eq.'b')then
            ABED(i,j)=0.2
          else
            ABED(i,j)=0.001
          endif
  43    continue
  42  continue
      return
      end

C ************* IMPOPT
C IMPOPT reads in an optical report from Window 4.1 or 5.1 or 5.2.
      SUBROUTINE IMPOPT(iwin4,IER)
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/GOPTED/DGED(5),HGED(5),UVALED,VTRNED,NTLED,ABED(ME,5),
     &  RFED(ME),SRFED,SABED

C wlname is Window 4 name for layer, wlthk is mm thickness.
      dimension wlname(ME),wlthk(ME)
      character lwin4*72,DFILE*72,H*72,outstr*124,word*20,wdum*20
      character outs*124,wlname*12
      logical LOK,close
      integer iwversion    ! the version of the import file

C Free unit and check if import file exists.
      H(1)='This is either a WINDOW 4.1 Optical Properties file or '
      H(2)='a WINDOW 4.1 or WINDOW 5.1 or 5.2 glazing system file.'
      DFILE='wdata.tab'
      CALL EASKS(lwin4,' Optical import file name? ',
     &     ' ',72,DFILE,'optical properties import file',IER,4)
      IER=0
      CALL EFOPSEQ(iwin4,lwin4,1,IER)
      IF(IER.NE.0)RETURN
      call edisp(iuout,lwin4)

C Set initial values. Assume that there are air gaps between
C the Window 4 "layers".
      UVALED=5.4
      LOK=.true.
      SABED = 0.0
      nl=0   ! assume no initial layers

C Read header.
      CALL STRIPC(iwin4,outstr,0,ND,1,'optical import header',IER)
      call edisp(iuout,outstr)
      if(outstr(1:29).eq.'WINDOW 4.1 Optical Properties')then
        call usrmsg(
     &    'This file contains only optical properties. You should use',
     &    'the export function in glazing systems instead. ','W')
        ier=1
        return
      elseif(outstr(1:25).eq.'WINDOW 4.1 Glazing System')then

C Loop until start of "Optical Properties" section.
 10     CALL STRIPC(iwin4,outstr,0,ND,1,'data',IER)
        call edisp(iuout,outstr)
        iwversion=4
        if(outstr(1:9).eq.'Glazings:')then
          K=9
          CALL EGETWI(outstr,K,nl,0,12,'F','layers',IER)
        elseif(outstr(1:9).eq.'Uvalue  :')then
          K=9
          CALL EGETWR(outstr,K,UVALED,-10.,9.99,'W','uval',IER)
        elseif(outstr(1:18).eq.'Optical Properties')then
          goto 12
        endif
        goto 10
      elseif(outstr(1:9).eq.'Window 5.'.or.
     &       outstr(1:8).eq.'Window v')then

C A newer format file.
 13     CALL STRIPC(iwin4,outstr,0,ND,1,'data',IER)
        call edisp(iuout,outstr)
        iwversion=5
        if(outstr(1:9).eq.'Glazings:')then
          K=9
          CALL EGETWI(outstr,K,nl,0,12,'F','layers',IER)
        elseif(outstr(1:9).eq.'Uvalue  :')then
          K=9
          CALL EGETWR(outstr,K,UVALED,-10.,9.99,'W','uval',IER)
        elseif(outstr(1:18).eq.'Optical Properties')then
          goto 12
        endif
        goto 13
      else
        call usrmsg('Does not appear to be a WINDOW 4 or 5 file...',
     &    outstr,'W')
        ier=1
        return
      endif

C Estimate number of layers including air gaps.
  12  if(LOK)then
        if(nl.eq.1)then
          nl=1
        elseif(nl.eq.2)then
          nl=nl+1
        elseif(nl.eq.3)then
          nl=nl+2
        elseif(nl.eq.4)then
          nl=nl+3
        endif
        NTLED=nl
      endif

      IL=0
 11   continue
      CALL STRIPC(iwin4,outstr,99,ND,1,'opt data',IEER)
      if(IEER.ne.0)goto 42
      call edisp(iuout,outstr)
      if(lnblnk(outstr).le.1)goto 11
      K=0
      CALL EGETW(outstr,K,WORD,'W','opt data',IER)
      if(word(1:3).eq.'Vtc')then
        CALL EGETW(outstr,K,wdum,'W','skip :',IER)
        CALL EGETWR(outstr,K,VTRNED,0.0,0.999,'W','visib trn',IER)
      elseif(word(1:7).eq.'Outside')then
        do 43 ij=1,nl
          CALL STRIPC(iwin4,outstr,99,ND,1,'layer dat',IEER)
          K=0
          CALL EGETW(outstr,K,wdum,'W','skip 1',IER)
          CALL EGETW(outstr,K,wlname(ij),'W','Win lay name',IER)

C Older files do not have a # after the name so can read layer thickness.
C Newer files do not have a # after air layers.
          if(iwversion.eq.4)then
            CALL EGETWR(outstr,K,wlthk(ij),0.0,99.0,'W','lay tk mm',
     &        IER)
          elseif(iwversion.eq.5)then
            if(wlname(ij)(1:3).eq.'Air')then
              CALL EGETWR(outstr,K,wlthk(ij),0.0,99.0,'W','lay tk mm',
     &          IER)
            else
              wlthk(ij)= 6.0   ! a guess
            endif
          endif
 43     continue 
      elseif(word(1:4).eq.'Tsol')then
        CALL EGETW(outstr,K,wdum,'W','skip :',IER)
        CALL EGETWR(outstr,K,DGED(1),0.0,0.999,'W','dir t @0',IER)
        CALL EGETWR(outstr,K,DUM,0.0,0.999,'W','skip 10deg',IER)
        CALL EGETWR(outstr,K,DUM,0.0,0.999,'W','skip 20deg',IER)
        CALL EGETWR(outstr,K,DUM,0.0,0.999,'W','skip 30deg',IER)
        CALL EGETWR(outstr,K,DGED(2),0.0,0.999,'W','dir t @40',IER)
        CALL EGETWR(outstr,K,V1,0.0,0.999,'W','skip 50deg',IER)
        CALL EGETWR(outstr,K,V2,0.0,0.999,'W','skip 60deg',IER)
        DGED(3)=(V1+V2)/2.
        CALL EGETWR(outstr,K,DGED(4),0.0,0.999,'W','dir t @70',IER)
        CALL EGETWR(outstr,K,DGED(5),0.0,0.999,'W','dir t @80',IER)
      elseif(word(1:2).eq.'Rf')then
        CALL EGETW(outstr,K,wdum,'W','skip :',IER)
        CALL EGETWR(outstr,K,SRFED,0.0,0.999,'W','sol refl',IER)
      elseif(word(1:3).eq.'Abs')then
        if(ND.le.2)goto 11
        IL=IL+1
        RFED(IL)=1.52
        CALL EGETW(outstr,K,wdum,'W','skip :',IER)
        CALL EGETWR(outstr,K,v1,0.0,0.999,'W','abs @0',IER)
        ABED(IL,1)=v1
        SABED = SABED + v1
        CALL EGETWR(outstr,K,DUM,0.0,0.999,'W','skip 10deg',IER)
        CALL EGETWR(outstr,K,DUM,0.0,0.999,'W','skip 20deg',IER)
        CALL EGETWR(outstr,K,DUM,0.0,0.999,'W','skip 30deg',IER)
        CALL EGETWR(outstr,K,v2,0.0,0.999,'W','abs @40',IER)
        ABED(IL,2)=v2
        CALL EGETWR(outstr,K,v3,0.0,0.999,'W','skip 50deg',IER)
        CALL EGETWR(outstr,K,v4,0.0,0.999,'W','skip 60deg',IER)
        ABED(IL,3)=(V3+V4)/2.
        CALL EGETWR(outstr,K,v5,0.0,0.999,'W','abs @70',IER)
        ABED(IL,4)=v5
        CALL EGETWR(outstr,K,v6,0.0,0.999,'W','abs @80',IER)
        ABED(IL,5)=v6
        write(outs,'(6f6.3,a,i2)')v1,v2,v3,v4,v5,v6,wdum,IL
        call edisp(iuout,outs)

C If inserting air gaps do it here, except for the inside layer.
        if(LOK.and.(IL.lt.NTLED))then
          IL=IL+1
          RFED(IL)=1.00
          ABED(IL,1)=0.001
          SABED = SABED + 0.001
          ABED(IL,2)=0.001
          ABED(IL,3)=0.001
          ABED(IL,4)=0.001
          ABED(IL,5)=0.001
        endif
      elseif(word(1:6).eq.'SHGCc:')then
        CALL EGETWR(outstr,K,HGED(1),0.0,0.999,'W','htgn @0',IER)
        CALL EGETWR(outstr,K,DUM,0.0,0.999,'W','skip 10deg',IER)
        CALL EGETWR(outstr,K,DUM,0.0,0.999,'W','skip 20deg',IER)
        CALL EGETWR(outstr,K,DUM,0.0,0.999,'W','skip 30deg',IER)
        CALL EGETWR(outstr,K,HGED(2),0.0,0.999,'W','htgn @40',IER)
        CALL EGETWR(outstr,K,V1,0.0,0.999,'W','skip 50deg',IER)
        CALL EGETWR(outstr,K,V2,0.0,0.999,'W','skip 60deg',IER)
        HGED(3)=(V1+V2)/2.
        CALL EGETWR(outstr,K,HGED(4),0.0,0.999,'W','htgn @70',IER)
        CALL EGETWR(outstr,K,HGED(5),0.0,0.999,'W','htgn @80',IER)
      else
        goto 11
      endif
      goto 11

  42  continue
      overall=DGED(1)+SABED+SRFED
      call eclose(overall,1.0,0.05,close)
      if(.NOT.close)then
        call edisp(iuout,'Overall solar tran + reflec + absob NE 1.0 ')
        write(outs,'(a,F6.3,a,F6.3,a,F6.3)') 'Solar Trn ',DGED(1),
     &    ' absorb. ',SABED,' refl. ',SRFED
        call edisp(iuout,outs)
      endif     
      if(NTLED.ne.IL)then
        write(outs,*)'nl il ntled',nl,il,ntled
        call edisp(iuout,outs)
        NTLED=IL
      endif
      CALL ERPFREE(iwin4,ISTAT)
      return
      end


C ************* IMPWIS
C IMPWIS clears common block GOPTED and sets it up for a new set.
      SUBROUTINE IMPWIS(iwis,IER)
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/GOPTED/DGED(5),HGED(5),UVALED,VTRNED,NTLED,ABED(ME,5),
     &  RFED(ME),SRFED,SABED

C wlname is WIS name for layer, wlthk is mm thickness.
      dimension wlname(ME),wlthk(ME)
      character lwin4*72,DFILE*72,H*72,outstr*124,word*20,wdum*20
      character outs*124,wlname*12
      logical LOK,close,havediffuse,basics,details,standalone
      logical environ,network

C Free unit and check if import file exists.
      H(1)='This is a WIS transparent system report file which '
      H(2)='<< more text >>'
      DFILE='wisdata.txt'
      lwin4=DFILE
      CALL EASKS(lwin4,' WIS report file to import? ',
     &     ' ',72,DFILE,'WIS import file',IER,4)
      IER=0
      CALL EFOPSEQ(iwis,lwin4,1,IER)
      IF(IER.NE.0)RETURN
      call edisp(iuout,lwin4)
      havediffuse=.false.
      basics=.false.
      details=.false.
      standalone=.false.
      environ=.false.
      network=.false.

C Scan file to see if information is direct only or with direct
C to diffuse information.
  22  CALL STRIPC(iwis,outstr,99,ND,1,'WIS scanning',IIER)
      if(IIER.ne.0)goto 23
      if(outstr(1:38).eq.'solar direct transmittance outdoor tot')then
        havediffuse=.true.
        call edisp(iuout,'Direct->diffuse data found.')
      elseif(outstr(1:9).eq.'t_sol_o_d')then
        havediffuse=.true.
        call edisp(iuout,'Direct->diffuse details found.')
      elseif(outstr(1:10).eq.'layer 1 is')then

C << also scan for `Pane` `Gap` `Shading` in these lines.
        nl=1
      elseif(outstr(1:10).eq.'layer 2 is')then
        nl=2
      elseif(outstr(1:10).eq.'layer 3 is')then
        nl=3
      elseif(outstr(1:10).eq.'layer 4 is')then
        nl=4
      elseif(outstr(1:10).eq.'layer 5 is')then
        nl=5
      elseif(outstr(1:10).eq.'layer 6 is')then
        nl=6
      elseif(outstr(1:4).eq.'prop')then
        if(ND.eq.12)then
          call edisp(iuout,'Angles 0->90 @10deg + diffuse')
        elseif(ND.eq.21)then
          call edisp(iuout,'Angles 0->90 & -10->-90 @10deg + diffuse')
        endif
      elseif(outstr(1:10).eq.'-- layer :')then
      else
        goto 22
      endif
      goto 22

  23  continue
      REWIND(iwis,ERR=999)
      NTLED=nl
      write(outs,'(a,i2,a)') 'Found ',nl,' layers.'
      call edisp(iuout,outs)

C Note: when looking for stand-alone data for each layer the
C actual data seems to start at column 38 (and after a :)

C Set initial values. Assume that there are air gaps between
C the Window 4 "layers".
      UVALED=5.4
      LOK=.true.
      SABED = 0.0

      IL=0
 11   continue
      CALL STRIPC(iwis,outstr,99,ND,1,'opt data',IEER)
      if(IEER.ne.0)goto 42
      call edisp(iuout,outstr)
      if(lnblnk(outstr).le.1)goto 11
      if(outstr(1:10).eq.'--- Basics')then
        basics=.true.
      elseif(outstr(1:23).eq.'name transparent system')then
        if(basics)then
        endif
      elseif(outstr(1:7).eq.'U-value')then
        if(basics)then
        endif
      elseif(outstr(1:16).eq.'solar factor (g)')then
        if(basics)then
        endif
      elseif(outstr(1:26).eq.'solar direct transmittance')then
        if(basics)then
        elseif(standalone)then
        endif
      elseif(outstr(1:32).eq.'solar direct reflectance outdoor')then
        if(basics)then
        elseif(standalone)then
        endif
      elseif(outstr(1:31).eq.'solar direct reflectance indoor')then
        if(basics)then
        elseif(standalone)then
        endif
      elseif(outstr(1:19).eq.'light transmittance')then
        if(basics)then
        elseif(standalone)then
        endif
      elseif(outstr(1:25).eq.'light reflectance outdoor')then
        if(basics)then
        elseif(standalone)then
        endif
      elseif(outstr(1:24).eq.'light reflectance indoor')then
        if(basics)then
        elseif(standalone)then
        endif
      elseif(outstr(2:30).eq.'eneral colour rendering index')then
        if(basics)then
        elseif(standalone)then
          continue
        endif
      elseif(outstr(1:28).eq.'--- Short system description')then
        basics=.false.
        details=.true.
      elseif(outstr(1:22).eq.'--- System description')then
        standalone=.true.
        basics=.false.
        details=.false.
      elseif(outstr(1:10).eq.'-- layer :')then
        if(standalone)then
        else
        endif
      elseif(outstr(1:5).eq.'name ')then
        if(standalone)then
        elseif(standalone)then
        endif
      elseif(outstr(1:5).eq.'Name:')then
        if(standalone)then
        elseif(standalone)then
        endif
      elseif(outstr(1:9).eq.'thickness')then
        if(standalone)then
        else
        endif
      elseif(outstr(1:19).eq.'thermal conductance')then
        if(standalone)then
        else
        endif
      elseif(outstr(1:9).eq.'gap width')then
        if(standalone)then
        else
        endif
      elseif(outstr(1:15).eq.'--- Environment')then
        environ=.true.
        basics=.false.
        details=.false.
        standalone=.false.
      elseif(outstr(1:16).eq.'-- Total Network')then
        network=.true.
        environ=.false.
        basics=.false.
        details=.false.
        standalone=.false.
      elseif(outstr(1:2).eq.' ')then
        goto 11
      else
        write(outs,'(2a)') 'Unrecognised ',outstr(1:50)
        call edisp(iuout,outs)
        goto 11
      endif
      goto 11

  42  continue
      CALL ERPFREE(iwis,ISTAT)
      return
 999  call erpfree(iwis,istat)
      return
      end

C ******* edbioptics
C edbioptics controls the specification of bi-directional
C optic properties. These are usually the result of measurements.
      subroutine edbioptics()
#include "building.h"
#include "epara.h"
#include "esprdbfile.h"
C MSTMC is number of different bidirectional datasets, limited
C to 20 for the present.
      
C      integer lnblnk  ! function definition

      PARAMETER (MSTMC=20,MSGAL=40,MANH=37,MANV=37)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)
      common/pophelp/h(60)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
C      COMMON/C24/IZSTOCN(MCOM,MS)

C NSTMCFL flag for each model surface (connection). A non-zero
C   number indicates bi-directional information is available.
C   Limited to 0 or 1 at present.
      COMMON/BIDIR/IFLAGBI,INTVALBI,NSTMCFL(MCON)
      COMMON/BIDIRFL/bidirfile,bidirname(MSTMC)

      DIMENSION VERT(35)
      character H*72,VERT*54,CXITM*43,key*1,lfil*72
      character bidirfile*72,DFILE*72,bidirname*12
      logical MODIFY,MODLEN,MODBND !,ok,dok
C      character outs*124
      integer MVERT,IVERT ! max items and current menu item

C Initialise connection menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
  31  MHEAD=5
      MCTL=5
      ILEN=NCON
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Force redraw of zones, highlighting surfaces with bi-directional
C attributes.
      MODIFY=.TRUE.
      nzg=NCOMP
      if(nzg.gt.0)then
        DO 44 I=1,nzg
          nznog(I)=I
  44    CONTINUE
        CALL INLNST(1)
        do 2 i = 1,ncon
          if(NSTMCFL(i).ne.0)LINSTY(i)=2
  2     continue
        CALL ADJVIEW(IER)
      endif

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Set menu header text.
   3  continue
      write(VERT(1),'(2a)')   'a bi-data file: ',bidirfile(1:24)
      write(VERT(2),'(a,i2)') '  bi-data types: ',iflagbi
      VERT(3)='  ___________________________________________    '
      VERT(4)=' conn|  connection      |    connection     |bi- '
      VERT(5)=' no. | inside face      | other side data   |type'

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          call CONXMENU(L,CXITM)
          VERT(M)=' '
          if(NSTMCFL(L).ne.0)then
            WRITE(VERT(M),'(A1,1x,A42,a,i1)')KEY,CXITM(1:42),'|',
     &        NSTMCFL(L)
          else
            WRITE(VERT(M),'(A1,1x,A42,a)')KEY,CXITM(1:42),'| -'
          endif
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ________________________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page ------- Part: ',I2,' of ',I2,' -----')
      ENDIF
      VERT(M+2)  ='! list types and associated surfaces  '
      VERT(M+3)  ='> save                                '
      VERT(M+4)  ='? help                                '
      VERT(M+5)  ='- exit this menu                      '

C Help text for this menu.
      h(1)='ESP-r accepts measured optical properties at 5 degree'
      h(2)='intervals over a hemisphere. This high resolution optical'
      h(3)='data can be associated with surfaces in the model. Note'
      h(4)='that each such surface is assumed to have a normal '
      h(5)='optical attribute.'
      h(6)=' '
      h(7)='Currently this data is read in from an external file.'
      h(8)='See esrucom/econstr.F or ask ESRU for details on the '
      h(9)='file format. An example is given here. '
      h(10)='Lines starting with # are optional and for documentation '
      h(11)='purposes: '
      h(12)='*BIDIRECTIONAL '
      h(13)='*types,1 '
      h(14)='*item, Genius internal with heat mirror DGU '
      h(15)='##(3 datatypes: Te_g_rho, Te_abs_n_diffuse, Te_g_only) '
      h(16)='*datatype,Te_g_rho '
      h(17)='*layers,3 '
      h(18)='*sets,6 '
      h(19)='## Next four lines are not used (only for documentation)'
      h(20)='## Properties of DGU: '
      h(21)='## WSV Iplus neutral E (4mm/16mm/4mm) '
      h(22)='## g0 = 0.60; '
      h(23)='## tauvis0 = 0.80; '
      h(24)='## udgu := 1.1; '
      h(25)='*start_set '
      h(26)='*set_type 1,#slat angle = 0deg '
      h(27)='*Uvalue 1.1 '
      h(28)='*Rexternal 0.04 '
      h(29)='*Rinternal 0.13 '
      h(30)='#Next 2 lines: only used for datatype 1 (Te_abs_n_diffuse)'
      h(31)='*diffuse_abs,0.260,0.521,0.002,0.000,0.007 '
      h(32)='*diffuse_trn,0.345 '
      h(33)='*direct_angs,37,37 '
      h(34)='*data '
      h(35)='#Azimuth,Height,Te,g,rho '
      h(36)='-90,-90,0,0,1 '
      h(37)='-90,-85,0,0,1 '
      h(38)='-90,-80,0,0,1 '
      h(39)='... '
      h(40)='90,85,0,0,1 '
      h(41)='90,90,0,0,1 '
      h(42)='*end_set '
      h(43)='*start_set '
      h(44)='*set_type 2,#slat angle = 15deg '
      h(45)='*Uvalue 1.1 '
      h(46)='*Rexternal 0.04 '
      h(47)='*Rinternal 0.13 '
      h(48)='#Next 2 lines: only used for datatype 1 (Te_abs_n_diffuse)'
      h(49)='*diffuse_abs,0.260,0.521,0.002,0.000,0.007 '
      h(50)='*diffuse_trn,0.345 '
      h(51)='*direct_angs,37,37 '
      h(52)='*data '
      h(53)='#Azimuth,Height,Te,g,rho '
      h(54)='-90,-90,0,0,1 '
      h(55)='-90,-85,0,0,1 '
      h(56)='... '
      h(57)='90,85,0,0,1 '
      h(58)='90,90,0,0,1 '
      h(59)='*end_set '
      h(60)=' '

C Now display the menu.
      CALL EMENU('  Bi-directional attributes',VERT,MVERT,IVERT)

      if(ivert.eq.mvert)then

C Return.
        return
      elseif(ivert.eq.mvert-1)then
        CALL PHELPD('bi-directional attributes',60,'-',0,0,IER)
      elseif(ivert.eq.mvert-2)then

C Save associations.
        call EMKCFG('-',IER)
      elseif(ivert.eq.mvert-3)then

C List current data.
C << >>
      elseif(ivert.eq.mvert-4)then

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      elseif(ivert.eq.1)then

C File with raw bi-directional data.
 77     H(1)='This is a high resolution bi-directional optical data'
        H(2)='file << more text >>'
        DFILE='bidata.txt'
        if(bidirfile(1:2).eq.'  '.or.bidirfile(1:4).eq.'UNKN')then
          lfil=DFILE
          bidirfile=DFILE
        endif
        CALL EASKS(lfil,' bi-directional optical data to use?',
     &   ' ',72,DFILE,'bi-directional data file',IER,2)
        if(lfil(1:2).ne.'  ')then
          bidirfile=lfil
        else
          goto 77
        endif

C Open and scan the file.
        iua=IPRODB
        call erbiwin(itru,iua,bidirfile,ier)
        if(ier.eq.0)then
          call edisp(iuout,'The bi-directional data was sucessfully')
          call edisp(iuout,'scanned. Now indicate which surfaces are')
          call edisp(iuout,'associated with this data.')
        else
          call edisp(iuout,'There was a problem reading the bi-')
          call edisp(iuout,'directional data. Please specify again...')
          H(1)='Select if you want to re-define the bi-directional file'
          call easkab(' File not found. Do you want to specify again?',
     &      ' ','Yes','No/exit',iyesno,1)
          if(iyesno.EQ.1)then
            goto 77
          else
            bidirfile='UNKNOWN'            
            goto 3
          endif
        endif
      elseif(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))then

C Enquire about connection identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOCC,IO)

C Display information about the anchors associated with connection.
        call CONXMENU(ifocc,CXITM)
        if(NSTMCFL(ifocc).eq.0)then
          NSTMCFL(ifocc)=1
C          dok=.false.
C          h(1)='If this surface fits your critera then proceed.'
C          CALL ASKOK(
C     &      'Use bi-directional attributes with this surface?',
C     &      '(only apply to transparent surfaces)',OK,dok,1)
C          if(OK)then
C            if(IFLAGBI.eq.1)then
C              NSTMCFL(ifocc)=1
C            else
C              nl=1
C              h(1)='There are a limited number of types. '
C              h(2)=' '
C              h(3)='A cancel request leaves the model unchanged.'
C              CALL EASKI(nl,' ',' Which bi-directional type?',
C     &          0,'F',MSTMC,'F',1,'bi opt types',IERI,3)
C              if(ieri.eq.-3)then
C                continue
C              else
C                NSTMCFL(ifocc)=nl
C              endif
C            endif
C          endif
        else
          NSTMCFL(ifocc)=0

C          write(outs,'(a,a,i2)') CXITM(1:lnblnk(CXITM)),
C     &      ' is associated with bi-type',NSTMCFL(ifocc)
C          call edisp(iuout,outs)
C          dok=.false.
C          h(1)='This surface uses bi-directional optics and'
C          h(2)='you can remove this attribute. '
C          CALL ASKOK(
C     &      'Dereference this bi-directional attribute?',
C     &      '(for this surface only)',OK,dok,2)
C          if(OK)NSTMCFL(ifocc)=0
        endif
        goto 3
      else
        ivert=-1
        goto 92
      endif
      ivert=-4
      goto 3
      end
