C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C edcondb.F comprising:
C  ELISTMAT: Controls display and editing of materials commons.
C  ELISTCFC: Controls display and editing of CFClayers commons.
C  EDONEMAT: Edits a material in the common block at array index index.
C  EDONECFC: Edits a CFC layer in the common block at arary index.
C  EDMLDB: Display/edit a constructions db.
C  EDMLDB2: Controls display and editing of MLC (v2) commons.
C  EDWALL: Edit construction common block data.
C  MLCREFS: returns true/false if mlc referenced within current zone scope.
C  GVALUE: Calculate g-value optical properties of systems given info on 
C          individual glazing layers. In accordance with BS EN 410:1998
C  allmat: Presents a list of all materials to select from.

C ************* ELISTMAT 
C ELISTMAT: Controls display and editing of materials commons.
C If ACTION = 'M' then include editing if ACTION = '-' only
C allow choice.
      SUBROUTINE ELISTMAT(iwhich,chgdb,ACTION,imatarrayindex,IER)
#include "epara.h"
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer iwhich         ! is the legacy database record CLARIFY USE
      logical chgdb          ! flagged true if a mod made during session
      integer imatarrayindex ! is the returned index in matdatarray

      common/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit
      LOGICAL OK,DOK,MODDB

C CLSDES (30 char) is an array for menu items listing each class.
C CLSSEL (36 char) is an array for menu items for the selected class.
      character CLSSEL*36,CLSDES*32
      DIMENSION CLSDES(36),CLSSEL(30),ICLSSEL(30)
      character PDBM*86     ! array to hold summary of each material
      DIMENSION PDBM(37)    ! within the category
      character managearray*32  ! string array for material management
      dimension managearray(60)
      integer iwhichmanage,iwhichmsel
      dimension iwhichmanage(60),iwhichmsel(60) ! for each management array point back
      integer iwhicharray   ! for each item in menu point back to array
      integer lsn,lfordoc   ! for detecting string lengths
      dimension iwhicharray(60)
      character ACTION*1,lltmp*144
      CHARACTER KEY*1,outs*124
      CHARACTER xfile*144,tg*1,delim*1,t32*32,t248*248
      character lworking*144,fs*1
      character heading*70
      logical closemat1,closemat2
      logical mod   ! to signal whether an item has been altered.
      logical unixok
      real per1,per2   ! percentages of material for non-homogeneous layer
      integer iwhich2  ! 2nd material array index
      integer idb,idb2 ! the two legacy indices selected
      integer listc    ! for looping through classes
      integer NCO,ICO,NITMS,INO,IW  ! max items and current menu item
      integer mclist  ! counter for materials found to be in this category


C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='ELISTMAT'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Assume no changes to db amd user has not entered password.
      MODDB=.FALSE.
      chgdb=.false.
      t32 = ' '
      IAF=IFIL+1
      iwhich=1   ! initial material selections
      iwhich2=1

C Check that the material db is not empty.
    4 IER=0
      call eclose(matver,1.1,0.001,closemat1)
      call eclose(matver,1.2,0.001,closemat2)
      if(closemat1.or.closemat2)then
        IF(matcats.LE.0)THEN
          CALL USRMSG(' Materials db is empty.',' ','W')
          IER=1
          RETURN
        ENDIF
      endif
      call usrmsg('  ','  ','-')   ! clear editing box.

C Initialise material category menu size variables based on window size. 
      MHEAD=1
      MCTL=5
      ILEN=matcats
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Gather the names of the various classes and present this list.
  40  ICO=-3
      ILEN=matcats
      CLSDES(1)=  '  Description         No. Items'
      M=MHEAD
      DO 44 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          write(CLSSEL(L),'(A)') matcatname(L)(1:32)
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(CLSDES(M),'(A1,1X,A,1X,I2)')KEY,matcatname(L)(1:26),
     &      matcatitems(L)
        ENDIF
   44 CONTINUE

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        CLSDES(M+1)='  ____________________________ '
      ELSE
        WRITE(CLSDES(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' ------ ')
      ENDIF
      if(ACTION.eq.'M'.or.ACTION.eq.'m')then
        CLSDES(M+2)='+ manage classifications      '
        CLSDES(M+3)='! list database entries       '
      elseif(ACTION.eq.'-')then
        CLSDES(M+2)='                              '
        CLSDES(M+3)='                              '
        call edisp(iuout,' ')
        call edisp(iuout,' Select classification to view items')
      endif
      CLSDES(M+4)=  '? help                        '
      CLSDES(M+5)=  '- exit                        '

C Number of actual items displayed.
      NCO=M+MCTL
      ICO=-2

C Help text for this menu.
      helptopic='cat_list_materials'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      CALL EMENU('Materials Classes',CLSDES,NCO,ICO)
      IF(ICO.EQ.NCO)THEN
        IF(MODDB.and.ACTION.eq.'M')THEN

C Changes were made so remove the existing file via delfiledosorunix
C and write a new file based on current common blocks. If the original
C file was binary then alter the name of the saved file to add a '.a'
C at the end.
          dok=.true.
          CALL ASKOK('Changes have been made in this file. ',
     &      'Save the materials changes?',OK,dok,nbhelp)
          IF(.NOT.OK) return
          chgdb=.true.   ! pass back signal to update
          CALL ERPFREE(IFMUL,ISTAT)  ! in case other unit is still open
          CALL ERPFREE(IAF,ISTAT)    ! in case unit is still open

C Check the value of whichdbpath variable to see what to do with
C this file. If local or absolute path then use lltmp directly. If in
C the standard location then create lworking which has path
C prepended.
          if(ipathmat.eq.0.or.ipathmat.eq.1)then
            lltmp=' '
            if(origmatwasbin)then
              write(lltmp,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
            else
              write(lltmp,'(a)') LFMAT(1:lnblnk(LFMAT))
            endif
C            call delfiledosorunix(lltmp,ider)
            CALL mkascimat(IAF,lltmp,IER)
          elseif(ipathmat.eq.2)then
            lndbp=lnblnk(standarddbpath)
            if(origmatwasbin)then
              write(lworking,'(4a)') standarddbpath(1:lndbp),fs,
     &          LFMAT(1:lnblnk(LFMAT)),'.a'
            else
              write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &          LFMAT(1:lnblnk(LFMAT))
            endif
C Debug.
C            write(6,*) 'working materials ',lworking
            CALL mkascimat(IAF,lworking,IER)
          endif

C Having read in binary and saved to ASCII the latter is now the model
C material database.
          if(origmatwasbin)then
            if(ipathmat.eq.0.or.ipathmat.eq.1)then
              write(LFMAT,'(a)') lltmp(1:lnblnk(lltmp))
            elseif(ipathmat.eq.2)then
              write(LFMAT,'(a)') lworking(1:lnblnk(lworking))
            endif
            origmatwasbin=.false.
          endif
          MODDB=.FALSE.
        ENDIF
        RETURN
      ELSEIF(ICO.EQ.(NCO-1))THEN
        helptopic='cat_list_materials'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('materials menu',nbhelp,'-',0,0,IER)
      ELSEIF(ICO.EQ.(NCO-2))THEN

C List one or more classifications.
        INPIC=matcats
        CALL EPMENSV
        CALL EPICKS(INPIC,ICLSSEL,' ',' Which classes to list:',
     &    36,matcats,CLSSEL,'Material classes',IER,nbhelp)
        CALL EPMENRC
        IF(INPIC.EQ.0)GOTO 40
        CALL EASKABC(' Reporting to:',' ',
     &    'text feedback','summary file','cancel',irpt,nbhelp)
        if(irpt.eq.1)then
          itu = iuout
        elseif(irpt.eq.2)then
          itu = ixunit
          write(xfile,'(a)') 'material_listing.txt'  ! initial file name
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','prim db text',IER)

C If user canceled the listing the reset unit to iuout and loop back.
          if(ier.eq.-3)then
            itu = iuout
            goto 40
          endif
        elseif(irpt.eq.3)then
          goto 40
        endif
        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          call edisp(itu,'In the materials database: ')
          call edisp(itu, LFMAT)
        elseif(ipathmat.eq.2)then
          call edisp(itu,'In the standard materials database: ')
          call edisp(itu, LFMAT)
        endif
        call edisp(itu,' ')
        do 42 listc=1,INPIC
          IC=ICLSSEL(listc)
          IF(matcatitems(IC).GT.0)THEN
            WRITE(outs,'(3a,i2,a)')' Classification: ',
     &        matcatname(IC)(1:lnblnk(matcatname(IC))),' (',IC,')'
            call edisp(itu,outs)
            call edisp248(itu,matcatdoc(IC),100)
            call edisp(itu,' ')
            call edisp(itu,
     &      'Index|Conduc-|Den- |Specif|IR  |Solar|Vapour|Description')
            call edisp(itu,
     &      '     |tivity |sity |heat  |emis|abs  |resist|of material')

C Loop through all of the items in the array and list out those that are
C associated with this class.
            DO 47 J=1,matdbitems
              if(matcatindex(J).eq.IC)then
                IDB=matlegindex(J)
                lsn=MIN0(lnblnk(matname(J)),32)
                lfordoc = 72 - (lsn +4)    ! space left for doc
                WRITE(outs,46)IDB,matdbcon(J),matdbden(J),matdbsht(J),
     &            matdboute(J),matdbouta(J),matdbdrv(J),
     &            matname(J)(1:lsn),' : ',matdoc(J)(1:lfordoc)
   46           FORMAT(I4,F9.3,F6.0,F7.0,F5.2,F5.2,F7.0,2X,3A)
                call edisp(itu,outs)
              endif
   47       CONTINUE
          ELSE
            call edisp(itu,' No items in this classification.')
          ENDIF
   42   continue
        call edisp(itu,' ')
        write(outs,'(2a)')
     &    ' Units: Conductivity W/(m deg.C), Density kg/m**3,',
     &    ' Specific Heat J/(kg deg.C), Vapour (MNs g^-1m^-1)'
        call edisp(itu,outs)
        if(irpt.eq.2)then

C Call ctlexp a 2nd time to close the file.
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','prim db',IER)
        endif
      ELSEIF(ICO.EQ.(NCO-3))THEN

C Manage categories. Select and alter the category name or documentation
C or add another category to the database << add not yet tested >>
        CALL EASKABCD(' Options:',' ',
     &    'edit category name','edit category documentation',
     &    'add another category','cancel',irpt,nbhelp)
        if(irpt.eq.1.or.irpt.eq.2)then
          CALL EPMENSV
          INPIC=1
          CALL EPICKS(INPIC,ICLSSEL,' ',' Which class:',
     &      36,matcats,CLSSEL,'Material class to manage',IER,nbhelp)
          CALL EPMENRC
          IF(INPIC.EQ.0)GOTO 40
          IC=ICLSSEL(1)
        endif
        if(irpt.eq.1)then

C Edit classification name.
          write(t32,'(a)')matcatname(IC)(1:lnblnk(matcatname(IC)))
          CALL EASKS(t32,' ',' Classification name (<32 char)?',
     &      32,' ','class name',IER,nbhelp)
          if(t32(1:2).ne.'  ')then
            write(matcatname(IC),'(a)') t32(1:lnblnk(t32))
            chgdb=.true.
            MODDB=.TRUE.
          endif
        elseif(irpt.eq.2)then

C Edit classification documentation.
          t248=matcatdoc(IC)(1:lnblnk(matcatdoc(IC)))
          CALL EASKS248(t248,'Category notes:',' ',
     &      72,'this category...','category notes',IER,nbhelp)
          if(t248(1:2).ne.'  ')then
            write(matcatdoc(IC),'(a)') t248(1:lnblnk(t248))
            chgdb=.true.
            MODDB=.TRUE.
          endif
        elseif(irpt.eq.3)then

C Add a classification.
          if(matcats.LT.30)then
            matcats=matcats+1
            t32=' '
            CALL EASKS(t32,' Classification name (<32 char)?',' ',
     &        32,' ','class name',IER,nbhelp)
            write(matcatname(matcats),'(a)') t32
            write(t248,'(a,i2,3a)')
     &        'Category (',matcats,') named ',t32(1:lnblnk(t32)),
     &        ' was inserted manually. No other documentation (yet).'
            CALL EASKS248(t248,'Category notes:',' ',
     &        72,'this category...','category notes',IER,nbhelp)
            write(matcatdoc(matcats),'(a)') t248(1:lnblnk(t248))
        
            ILNE=matdbitems+1
            t32=' '
            CALL EASKS(t32,'Name of initial material','confirm:',
     &        32,' ','material name',IER,nbhelp)
            write(matname(ILNE),'(a)') t32(1:lnblnk(t32))
            matopaq(ILNE)='o'
            matdoc(ILNE)='no documentation (yet)'
            matcatindex(ILNE)=matcats
            matdbcon(ILNE)=1.0; matdbden(ILNE)=1.0; matdbsht(ILNE)=1.0
            matdboute(ILNE)=0.9; matdbine(ILNE)=0.9
            matdbouta(ILNE)=0.5; matdbina(ILNE)=0.5
            matdbdrv(ILNE)=10.0; matdbthick(ILNE)=10.0
            matirtran(ILNE)=0.0; matsoldrtrn(ILNE)=0.0
            matsoldrotrfl(ILNE)=0.5; matsoldrinrfl(ILNE)=0.5
            matvistran(ILNE)=0.0
            matvisotrfl(ILNE)=0.5; matvisinrfl(ILNE)=0.5
            matrender(ILNE)=0.0

            matcatitems(matcats)=matcatitems(matcats)+1 ! increment nb of cat items
            matcatindex(ILNE)=matcats  ! remember its category
            matdbitems=matdbitems+1    ! increment nb of db items

C See if there is an available slot in the 0-600 list of legacy
C indices.
            lastcatitem=2
            ip=matlegindex(lastcatitem)
            call getnextascislot(ip,inext)
            if(inext.le.600)then 
              matlegindex(ILNE)=inext
              mathash(inext)=ILNE
            else
              matlegindex(ILNE)=-99
            endif
            chgdb=.true.
            MODDB=.TRUE.
          endif
        endif
        goto 40

      ELSEIF(ICO.eq.(NCO-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 40    ! user paged so refresh

      ELSEIF(ICO.GT.MHEAD.AND.ICO.LT.(NCO-4))THEN
        
C This is the secondary menu focused on a single material class. Loop through 
C data for this classification for viewing and/or manipulation.
C Edit categpru identified by KEYIND.
        CALL KEYIND(NCO,ICO,IC,IO)
        IF(matcatitems(IC).EQ.0)THEN
         CALL USRMSG(' No items in this classification.',' ','W')
         GOTO 40
        ENDIF

C Echo documentation for classification when changing focus.
        call edisp(iuout,' ')
        call edisp248(iuout,matcatdoc(IC),100)
        call edisp(iuout,' ')

C Save the state of the initial menu prior to setting up the secondary
C menu layout. Ensure that each logical exit point restores initial menu.
        CALL EPMENSV
        MHEAD=4  ! reset for inner menu head
        MCTL=5   ! reset for inner menu control entries
        ILEN=matcatitems(IC) ! reset for materials in this catetory 
        IPACT=CREATE
        CALL EKPAGE(IPACT)

C Generate menu strings for materials in the class.
    3   PDBM(1)= ' Units: Conductivity W/(m deg.C), Density kg/m**3'
        PDBM(2)= ' Specific Heat J/(kg deg.C) Vapour (MNs g^-1m^-1)'
        PDBM(3)=
     &' |Conduc-|Den- |Specif|IR  |Solar|Diffu|Description of material'
        write(PDBM(4),'(2a)')
     &' |tivity |sity |heat  |emis|abs  |resis| name       :',
     &' documentation'
        M=MHEAD
        ILEN=matcatitems(IC) ! reset if materials in this catetory changed
        mclist=0  ! counter for materials found to be in this category
        DO 30 J=1,matdbitems

C Loop through all of the items in the array and list out those that are
C associated with this class (and which fit within the current page).
          if(matcatindex(J).eq.IC)then

C Increment mclist and build string array for use in managing materials
C in this category. Also create an array iwhichmanage to point from
C the management list to the original materials data array.
            mclist=mclist+1
            write(managearray(mclist),'(a)') matname(J)
            iwhichmanage(mclist)=J  ! management array to data array J

C Test if the material fits within the current paged menu and then
C remember the array index that goes with the menu position (m).
            IF(mclist.GE.IST.AND.(mclist.LE.(IST+MIFULL)))THEN
              M=M+1
              iwhicharray(M)=J  ! menu position M relates to data array J
              CALL EMKEY(mclist,KEY,IER)
              if(matopaq(J).eq.'o')then
                WRITE(PDBM(M),24)KEY,matdbcon(J),matdbden(J),
     &            matdbsht(J),matdboute(J),matdbouta(J),matdbdrv(J),
     &            matname(J)(1:32)
   24           FORMAT(A1,F8.3,F6.0,F7.0,F5.2,F5.2,F7.0,1X,A)
              elseif(matopaq(J).eq.'-')then
                WRITE(PDBM(M),25)KEY,matdbcon(J),matdbden(J),
     &            matdbsht(J),matdboute(J),matdbouta(J),matdbdrv(J),
     &            matname(J)(1:12),': ',matdoc(J)(1:31)
   25           FORMAT(A1,F8.3,F6.0,F7.0,F5.2,F5.2,F7.0,1X,3A)
              elseif(matopaq(J).eq.'t')then
                WRITE(PDBM(M),25)KEY,matdbcon(J),matdbden(J),
     &            matdbsht(J),matdboute(J),matdbouta(J),matdbdrv(J),
     &            matname(J)(1:12),': ',matdoc(J)(1:31)
              elseif(matopaq(J).eq.'g')then
                WRITE(PDBM(M),25)KEY,matdbcon(J),matdbden(J),
     &            matdbsht(J),matdboute(J),matdbouta(J),matdbdrv(J),
     &            matname(J)(1:12),': ',matdoc(J)(1:31)
              elseif(matopaq(J).eq.'h')then
                WRITE(PDBM(M),25)KEY,matdbcon(J),matdbden(J),
     &            matdbsht(J),matdboute(J),matdbouta(J),matdbdrv(J),
     &            matname(J)(1:12),': ',matdoc(J)(1:31)
              else
                WRITE(PDBM(M),24)KEY,matdbcon(J),matdbden(J),
     &            matdbsht(J),matdboute(J),matdbouta(J),matdbdrv(J),
     &            matname(J)(1:32)
              endif
            endif
          endif
   30   CONTINUE
        NC=M

C If a long list include page facility text.      
        IF(IPFLG.EQ.0)THEN  
          PDBM(NC+1)='  ____________________________________ '
        ELSE
          WRITE(PDBM(NC+1),15)IPM,MPM 
        ENDIF

        if(ACTION.eq.'M'.or.ACTION.eq.'m')then
          PDBM(NC+2)='* add/ delete/ copy material     '

C Remind user whether they are working with a model file or
C a common data file.
          if(ipathmat.eq.0.or.ipathmat.eq.1)then
            PDBM(NC+3)='! save materials file            '
          else
            PDBM(NC+3)='! save common materials file     '
          endif
        elseif(ACTION.eq.'-')then
          PDBM(NC+2)='                                 '
          PDBM(NC+3)='                                 '
        endif
        PDBM(NC+4)=  '? help                           '
        PDBM(NC+5)=  '- exit this menu                 '
        NITMS=NC+5
        INO=-4

C Help text for this menu.
        helptopic='materials_db_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)

        write(heading,'(3a,i2,a,i2,a)') 'Materials in ',
     &    matcatname(IC)(1:lnblnk(matcatname(IC))),' (',IC,') with ',
     &    matcatitems(IC),' entries.'
 
        CALL EMENU(heading,PDBM,NITMS,INO)

        IF(INO.EQ.NITMS.OR.INO.EQ.0)THEN
          IF(MODDB.and.ACTION.eq.'M')THEN

C Changes were made so remove the existing file via delfiledosorunix
C and write a new file based on current common blocks. If the original
C file was binary then alter the name of the saved file to add a '.a'
C at the end.
            dok=.true.
            CALL ASKOK('Changes have been made in this file. ',
     &        'Save the materials changes?',OK,dok,nbhelp)
            if(.NOT.OK)then
              CALL EPMENRC   ! recover initial menu state
              GOTO 4         ! jump back to rebuild initial menu
            endif
            chgdb=.true.   ! pass back signal to update
            CALL ERPFREE(IFMUL,ISTAT)  ! in case other unit is still open
            CALL ERPFREE(IAF,ISTAT)    ! in case unit is still open

C Check the value of whichdbpath variable to see what to do with
C this file. If local or absolute path then use lltmp directly. If in
C the standard location then create lworking which has path
C prepended.
            if(ipathmat.eq.0.or.ipathmat.eq.1)then
              lltmp=' '
              if(origmatwasbin)then
                write(lltmp,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
              else
                write(lltmp,'(a)') LFMAT(1:lnblnk(LFMAT))
              endif
C              call delfiledosorunix(lltmp,ider)
              CALL mkascimat(IAF,lltmp,IER)
            elseif(ipathmat.eq.2)then
              lndbp=lnblnk(standarddbpath)
              if(origmatwasbin)then
                write(lworking,'(4a)') standarddbpath(1:lndbp),fs,
     &            LFMAT(1:lnblnk(LFMAT)),'.a'
              else
                write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &            LFMAT(1:lnblnk(LFMAT))
              endif
C Debug.
C              write(6,*) 'working materials ',lworking
              CALL mkascimat(IAF,lworking,IER)
            endif

C Having read in binary and saved to ASCII the latter is now the model
C material database.
            if(origmatwasbin)then
              if(ipathmat.eq.0.or.ipathmat.eq.1)then
                write(LFMAT,'(a)') lltmp(1:lnblnk(lltmp))
              elseif(ipathmat.eq.2)then
                write(LFMAT,'(a)') lworking(1:lnblnk(lworking))
              endif
              origmatwasbin=.false.
            endif
            MODDB=.FALSE.
          ENDIF
          CALL EPMENRC   ! recover initial menu state
          GOTO 4         ! go back and rebuild initial menu
        ELSEIF(INO.EQ.1.AND.INO.LE.4)THEN
          GOTO 3         ! not a useful selection redo inner menu
        ELSEIF(INO.EQ.NITMS-1)THEN
          helptopic='materials_db_overview'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('materials database',nbhelp,'-',0,0,IER)
        ELSEIF(INO.EQ.NITMS-2)THEN

C Save materials db with all materials. First delete the
C existing file via delfiledosorunix and then write
C a new file via mkascimat. If it was an older file format
C and the user has not made any changes confirm that the
C file is to be updated.
          IF(.NOT.MODDB)THEN
            dok=.true.
            CALL ASKOK('No changes in the materials file. Do you want',
     &        'to update it anyway:',OK,dok,nbhelp)
            if(.NOT.OK)then
              goto 3  ! jump back to inner menu without doing anything
            endif
          endif
          chgdb=.true.
          call erpfree(ifmat,istat)  ! in case it is still open
          call erpfree(iaf,istat)  ! in case it is still open

          lltmp=' '
          if(ipathmat.eq.0.or.ipathmat.eq.1)then
            if(origmatwasbin)then
              write(lltmp,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
            else
              write(lltmp,'(a)') LFMAT(1:lnblnk(LFMAT))
            endif
C            call delfiledosorunix(lltmp,ider)
            CALL mkascimat(IAF,lltmp,IER)
          elseif(ipathmat.eq.2)then
            lndbp=lnblnk(standarddbpath)
            write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &        LFMAT(1:lnblnk(LFMAT))
            CALL mkascimat(IAF,lworking,IER)
          endif

C Having read in binary and saved to ASCII the latter is now the model
C material database.
          if(origmatwasbin)then
            if(ipathmat.eq.0.or.ipathmat.eq.1)then
              write(LFMAT,'(a)') lltmp(1:lnblnk(lltmp))
            elseif(ipathmat.eq.2)then
              write(LFMAT,'(a)') lworking(1:lnblnk(lworking))
            endif
            origmatwasbin=.false.
          endif

C Reset flag to show db is current with local arrays.
          MODDB=.FALSE.
          CALL USRMSG(' ','Changes saved...','-')

        ELSEIF(INO.EQ.NITMS-3)THEN

C Add delete or copy material from db.
          CALL EASKATOG('Options: ',' ','Delete existing material',
     &      'Add material','Derive non-homogeneous material',
     &      'copy existing material','cancel','  ','  ',IW,nbhelp)
          IF(IW.EQ.1)THEN

C Ask the user which material to remove (use the managearray list of material
C names created in the do 30 loop above) then loop through materials and
C copy the contents of material > IWHICH into the next lower one.
            CALL USRMSG(' ',' Which item of list? ','-')
            INPIC=1
            CALL EPMENSV
            CALL EPICKS(INPIC,iwhichmsel,'Delete which material',
     &        'Confirm:',32,matcatitems(IC),managearray,
     &        'Material to delete',IER,nbhelp)
            CALL EPMENRC
            IF(INPIC.EQ.0)GOTO 40  ! user selected nothing go back to category menu
            IW=iwhichmsel(1)       ! get position in management array
            IWHICH=iwhichmanage(IW)  ! get the original material data position
            ip=matlegindex(IWHICH)   ! todo remove legacy index from the hash table
            if(ip.gt.0) mathash(ip)=-1
            DO 791 IVV=IWHICH,matdbitems-1
              matlegindex(IVV)=matlegindex(IVV+1)
              matdbcon(IVV)=matdbcon(IVV+1); 
              matdbden(IVV)=matdbden(IVV+1)
              matdbsht(IVV)=matdbsht(IVV+1)
              matdboute(IVV)=matdboute(IVV+1)
              matdbine(IVV)=matdbine(IVV+1)
              matdbouta(IVV)=matdbouta(IVV+1)
              matdbina(IVV)=matdbina(IVV+1)
              matdbdrv(IVV)=matdbdrv(IVV+1)
              matdbthick(IVV)=matdbthick(IVV+1)
              matcatindex(IVV)=matcatindex(IVV+1)  ! keep in same category
              matname(IVV)=matname(IVV+1)
              matdoc(IVV)=matdoc(IVV+1)
              matopaq(IVV)=matopaq(IVV+1)
              matirtran(IVV)=matirtran(IVV+1)
              matsoldrtrn(IVV)=matsoldrtrn(IVV+1)
              matsoldrotrfl(IVV)=matsoldrotrfl(IVV+1)
              matsoldrinrfl(IVV)=matsoldrinrfl(IVV+1)
              matvistran(IVV)=matvistran(IVV+1)
              matvisotrfl(IVV)=matvisotrfl(IVV+1)
              matvisinrfl(IVV)=matvisinrfl(IVV+1)
              matrender(IVV)=matrender(IVV+1)

  791       CONTINUE
            matdbitems=matdbitems-1
            matcatitems(IC)=matcatitems(IC)-1
            MODDB=.TRUE.
            chgdb=.true.
          ELSEIF(IW.EQ.2)THEN

C Add a material, initiate it to the values of last material in class.
            IF(matdbitems.LT.MGIT)THEN
              ILNE=matdbitems+1
              lastcatitem=0
              DO 793 J=1,matdbitems
                if(matcatindex(J).eq.IC)then
                  lastcatitem=J
                endif
  793         continue
              if(lastcatitem.gt.0)then

C See if there is an available slot in the 0-600 list of legacy
C indices and make up name and index based on inext value.
                ip=matlegindex(lastcatitem)
                call getnextascislot(ip,inext)
                if(inext.le.600)then 
                  write(matname(ILNE),'(a,i3.3)') 'mat_',inext
                  write(t32,'(a)') matname(ILNE)
                  CALL EASKS(t32,'Name of new material','confirm:',
     &              32,' ','material name',IER,nbhelp)
                  write(matname(ILNE),'(a)') t32(1:lnblnk(t32))
                  matlegindex(ILNE)=inext
                  mathash(inext)=ILNE
                else
                  matname(ILNE)='new_material'
                  matlegindex(ILNE)=-99
                endif
                matdbcon(ILNE)=matdbcon(lastcatitem)
                matdbden(ILNE)=matdbden(lastcatitem)
                matdbsht(ILNE)=matdbsht(lastcatitem)
                matdboute(ILNE)=matdboute(lastcatitem)
                matdbine(ILNE)=matdbine(lastcatitem)
                matdbouta(ILNE)=matdbouta(lastcatitem)
                matdbina(ILNE)=matdbina(lastcatitem)
                matdbdrv(ILNE)=matdbdrv(lastcatitem)
                matdbthick(ILNE)=matdbthick(lastcatitem)
                matcatindex(ILNE)=matcatindex(lastcatitem)  ! keep in same category

C If there is room add a note about material origin.
                ils=lnblnk(matname(ILNE))
                iln=lnblnk(matname(lastcatitem))
                if(ils.le.200)then
                  write(matdoc(ILNE),'(4a)') 
     &              matname(ILNE)(1:ils),' (derived from ',
     &              matname(lastcatitem)(1:iln),')'
                else
                  matdoc(ILNE)=matdoc(lastcatitem)
                endif
                matopaq(ILNE)=matopaq(lastcatitem)
                matirtran(ILNE)=matirtran(lastcatitem)
                matsoldrtrn(ILNE)=matsoldrtrn(lastcatitem)
                matsoldrotrfl(ILNE)=matsoldrotrfl(lastcatitem)
                matsoldrinrfl(ILNE)=matsoldrinrfl(lastcatitem)
                matvistran(ILNE)=matvistran(lastcatitem)
                matvisotrfl(ILNE)=matvisotrfl(lastcatitem)
                matvisinrfl(ILNE)=matvisinrfl(lastcatitem)
                matrender(ILNE)=matrender(lastcatitem)

                matdbitems=matdbitems+1
                matcatitems(IC)=matcatitems(IC)+1

C Browse/Edit the details of this new item. Mark as unmodified first
C and then resetn MODDB and chgdb if mod is true.
                mod=.false.
                call edonemat(ILNE,mod,ier)
                if(mod)then 
                  MODDB=.TRUE.
                  chgdb=.true.
                endif
              endif
            ELSE
              CALL USRMSG(' ','Exceeds classification limit!','W')
              GOTO 3   ! go back an recreate the inner menu
            ENDIF
          ELSEIF(IW.EQ.3)THEN

C Derive non-homogeneous properties from two solid materials.
C Copy info from first existing material (save & recover the
C state of the menu when doing this).  
            CALL USRMSG(' ','Select first material?','-')
            CALL EPMENSV
            call allmat(imatarrayindex,ier)
            CALL EPMENRC
            if(imatarrayindex.gt.0)then
              IWHICH=imatarrayindex
            else
              CALL USRMSG(' ','No first material selected!','W')
              GOTO 3  ! recreate and display inner menu
            endif
          
            PER1=90.0
            CALL EASKR(PER1,' ','First material % in the layer: ',
     &            0.1,'W',99.9,'W',1.,'1st material %',IER,nbhelp)
            PER1=PER1*0.01

            call edisp(iuout,'First material: ')
            call edisp(iuout,
     &      'Index|Conduc-|Den- |Specif|IR  |Solar|Vapour|Description')
            call edisp(iuout,
     &      '     |tivity |sity |heat  |emis|abs  |resist|of material')
            IDB=matlegindex(IWHICH)
            WRITE(outs,46)IDB,matdbcon(IWHICH),matdbden(IWHICH),
     &        matdbsht(IWHICH),
     &        matdboute(IWHICH),matdbouta(IWHICH),matdbdrv(IWHICH),
     &        matname(IWHICH)(1:lnblnk(matname(IWHICH)))
            call edisp(iuout,outs)

C Copy info from second existing material (save & recover the
C state of the menu when doing this).  
            CALL USRMSG(' ','Select second material?','-')
            CALL EPMENSV
            call allmat(imatarrayindex,ier)
            CALL EPMENRC
            if(imatarrayindex.gt.0)then
              IWHICH2=imatarrayindex
            else
              CALL USRMSG(' ','No second material selected!','W')
              GOTO 3   ! recreate and display inner menu
            endif
          
            PER2=100.0-(PER1*100.0)  ! the balance from 100 percent
            CALL EASKR(PER2,' ','Second material % in the layer: ',
     &            0.1,'W',99.9,'W',1.,'2nd material %',IER,nbhelp)
            PER2=PER2*0.01

            call edisp(iuout,'Second material: ')
            IDB=matlegindex(IWHICH2)
            WRITE(outs,46)IDB,matdbcon(IWHICH2),matdbden(IWHICH2),
     &        matdbsht(IWHICH2),
     &        matdboute(IWHICH2),matdbouta(IWHICH2),matdbdrv(IWHICH2),
     &        matname(IWHICH2)(1:lnblnk(matname(IWHICH2)))
            call edisp(iuout,outs)

            ILNE=matdbitems+1  ! one more item for list

C Add a material, initiate it to the weighted values of materials.
C See if there is an available slot in the 0-600 list of legacy
C indices based on the prior item in this category.
            lastcatitem=0
            DO 794 J=1,matdbitems
              if(matcatindex(J).eq.IC)then
                lastcatitem=J
              endif
  794       continue
            if(lastcatitem.gt.0)then
              ip=matlegindex(lastcatitem)
              call getnextascislot(ip,inext)

C Make up the initial name based on the two legacy
C indices but allow the user to edit it. If not within 0-600 then
C assign -99 and make up initial name based on the original.
              if(inext.le.600)then 
                IDB=matlegindex(IWHICH)
                IDB2=matlegindex(IWHICH2)
                write(matname(ILNE),'(a,2i3.3)') 'non-h_',idb,idb2
                matlegindex(ILNE)=inext
                mathash(inext)=ILNE
              else
                write(matname(ILNE),'(2a)') 
     &            matname(IWHICH)(1:lnblnk(matname(IWHICH))),'_non-h'
                matlegindex(ILNE)=-99
              endif

C User to confirm or alter the suggested material name.
              iln1=lnblnk(matname(ILNE))
              write(t32,'(a)') matname(ILNE)(1:iln1)
              CALL EASKS(t32,'Name of non-homogeneous material',
     &          'confirm:',32,' ','material name',IER,nbhelp)
              write(matname(ILNE),'(a)') t32(1:lnblnk(t32))

C If there is room add a note about its origin.
              ils=lnblnk(matdoc(IWHICH))
              iln=lnblnk(matname(IWHICH))
              iln1=lnblnk(matname(ILNE))
              iln2=lnblnk(matname(IWHICH2))
              if(ils.le.200)then
                write(matdoc(ILNE),'(6a,2f6.3,a)') 
     &            matname(ILNE)(1:iln1),' derived from ',
     &            matname(IWHICH)(1:iln),' & ',
     &            matname(IWHICH2)(1:iln2),' (',PER1,PER2,')'
              else
                matdoc(ILNE)=matdoc(IWHICH)
              endif

C Apply weighting for each contributing material.
              matdbcon(ILNE)=matdbcon(IWHICH)*PER1+
     &                       matdbcon(IWHICH2)*PER2
              matdbden(ILNE)=matdbden(IWHICH)*PER1+
     &                       matdbden(IWHICH2)*PER2
              matdbsht(ILNE)=matdbsht(IWHICH)*PER1+
     &                       matdbsht(IWHICH2)*PER2
              matdboute(ILNE)=matdboute(IWHICH)*PER1+
     &                        matdboute(IWHICH2)*PER2
              matdbine(ILNE)=matdbine(IWHICH)*PER1+
     &                       matdbine(IWHICH2)*PER2
              matdbouta(ILNE)=matdbouta(IWHICH)*PER1+
     &                        matdbouta(IWHICH2)*PER2
              matdbina(ILNE)=matdbina(IWHICH)*PER1+
     &                       matdbina(IWHICH2)*PER2
              matdbdrv(ILNE)=matdbdrv(IWHICH)*PER1+
     &                       matdbdrv(IWHICH2)*PER2
              matdbthick(ILNE)=matdbthick(IWHICH)

C The additional properties just use the 1st material (change this later).
              matopaq(ILNE)=matopaq(IWHICH)
              matirtran(ILNE)=matirtran(IWHICH)
              matsoldrtrn(ILNE)=matsoldrtrn(IWHICH)
              matsoldrotrfl(ILNE)=matsoldrotrfl(IWHICH)
              matsoldrinrfl(ILNE)=matsoldrinrfl(IWHICH)
              matvistran(ILNE)=matvistran(IWHICH)
              matvisotrfl(ILNE)=matvisotrfl(IWHICH)
              matvisinrfl(ILNE)=matvisinrfl(IWHICH)
              matrender(ILNE)=matrender(IWHICH)

              matdbitems=matdbitems+1   ! increment nb of db items
              matcatitems(IC)=matcatitems(IC)+1 ! increment nb of cat items
              matcatindex(ILNE)=IC      ! keep in current category
            endif
          ELSEIF(IW.EQ.4)THEN

C Copy an existing material to a new one at end of the array (use the managearray
C list of material names created in the do 30 loop above).  
            CALL USRMSG(' ',' Which item of list?','-')
            INPIC=1
            CALL EPICKS(INPIC,iwhichmsel,'Copy which material',
     &        'Confirm:',32,matcatitems(IC),managearray,
     &        'Material to copy',IER,nbhelp)
            IF(INPIC.EQ.0)GOTO 40  ! user selected nothing go back to category menu
            IW=iwhichmsel(1)       ! get position in management array
            IWHICH=iwhichmanage(IW)  ! get the original material data position
            ILNE=matdbitems+1

C See if there is an available slot in the 0-600 list of legacy
C indices. If so make up the initial name but allow the user to
C edit it. If not then assign -99 and make up initial name based
C on the original.
            ip=matlegindex(IWHICH)
            call getnextascislot(ip,inext)
            if(inext.le.600)then 
              write(matname(ILNE),'(a,i3.3)') 'mat_',inext
              write(t32,'(a)') matname(ILNE)
              CALL EASKS(t32,'Name of copied material','confirm:',
     &          32,' ','material name',IER,nbhelp)
              write(matname(ILNE),'(a)') t32(1:lnblnk(t32))
              matlegindex(ILNE)=inext
              mathash(inext)=ILNE
            else
              write(matname(ILNE),'(2a)') 
     &          matname(IWHICH)(1:lnblnk(matname(IWHICH))),'c'
              matlegindex(ILNE)=-99
            endif

C If there is room add a note about its origin.
            ils=lnblnk(matname(ILNE))
            iln=lnblnk(matname(IWHICH))
            if(ils.le.200)then
              write(matdoc(ILNE),'(4a)') 
     &          matname(ILNE)(1:ils),' (copy of ',
     &          matname(IWHICH)(1:iln),')'
            else
              matdoc(ILNE)=matdoc(IWHICH)
            endif
            matdbcon(ILNE)=matdbcon(IWHICH)
            matdbden(ILNE)=matdbden(IWHICH)
            matdbsht(ILNE)=matdbsht(IWHICH)
            matdboute(ILNE)=matdboute(IWHICH)
            matdbine(ILNE)=matdbine(IWHICH)
            matdbouta(ILNE)=matdbouta(IWHICH)
            matdbina(ILNE)=matdbina(IWHICH)
            matdbdrv(ILNE)=matdbdrv(IWHICH)
            matdbthick(ILNE)=matdbthick(IWHICH)
            matcatindex(ILNE)=matcatindex(IWHICH)  ! keep in same category
            matopaq(ILNE)=matopaq(IWHICH)
            matirtran(ILNE)=matirtran(IWHICH)
            matsoldrtrn(ILNE)=matsoldrtrn(IWHICH)
            matsoldrotrfl(ILNE)=matsoldrotrfl(IWHICH)
            matsoldrinrfl(ILNE)=matsoldrinrfl(IWHICH)
            matvistran(ILNE)=matvistran(IWHICH)
            matvisotrfl(ILNE)=matvisotrfl(IWHICH)
            matvisinrfl(ILNE)=matvisinrfl(IWHICH)
            matrender(ILNE)=matrender(IWHICH)

            matdbitems=matdbitems+1
            matcatitems(IC)=matcatitems(IC)+1
            MODDB=.TRUE.
            chgdb=.true.

          ELSEIF(IW.EQ.5)THEN
            GOTO 3    ! user declined so recreate and display inner menu
          ENDIF
        ELSEIF(INO.EQ.NITMS-4)THEN

C If there are enough items allow paging control via EKPAGE.
          IF(IPFLG.EQ.1)THEN
            IPACT=EDIT
            CALL EKPAGE(IPACT)
          ENDIF
          GOTO 3    ! user paged so refresh inner menu
        ELSEIF(INO.GT.MHEAD.AND.INO.LT.NITMS-4)THEN

C Identified one of the materials to edit or select.
          IFOC=iwhicharray(INO)

C Debug.
C          write(6,*) 'selected menu ',ino,'got array',ifoc

          if(ACTION.eq.'M')then

C Browse/Edit the details of this item. Mark as unmodified first
C and then resetn MODDB and chgdb if mod is true.
            mod=.false.
            call edonemat(ifoc,mod,ier)
            if(mod)then 
              MODDB=.TRUE.
              chgdb=.true.
              imatarrayindex=ifoc ! set to edited array index
            endif
          elseif(ACTION.eq.'-')then
            call edisp(iuout,' ')
            write(outs,'(2a)')
     &        ' Units: Conductivity W/(m deg.C), Density kg/m**3,',
     &        ' Specific Heat J/(kg deg.C), Vapour (MNs g^-1m^-1)'
            call edisp(iuout,outs)
            call edisp(iuout,' ')
            call edisp(iuout,
     &    'Index|Conduc- |Den-  |Specif|IR  |Solar|Vapour|Description')
            call edisp(iuout,
     &    '     |tivity  |sity  |heat  |emis|abs  |resist|of material')
            lsn=MIN0(lnblnk(matname(IFOC)),32)
            WRITE(outs,244)matlegindex(IFOC),matdbcon(IFOC),
     &        matdbden(IFOC),matdbsht(IFOC),matdboute(IFOC),
     &        matdbouta(IFOC),matdbdrv(IFOC),matname(IFOC)(1:lsn)
  244       FORMAT(I5,F9.3,F7.1,F7.0,F5.2,F6.2,F7.0,1X,A)
            CALL EDISP(iuout,outs)
            CALL EDISP248(iuout,matdoc(IFOC),90)
            iwhich=matlegindex(ifoc)
            write(outs,'(a,i3,4a)') 'Material (',iwhich,') ',
     &        matname(IFOC)(1:lsn),' : ',matdoc(IFOC)(1:48)
            dok=.true.
            CALL ASKOK(outs,'Use it:',OK,dok,1)
            if(OK)then
              imatarrayindex=IFOC ! set to selected array index
              return              ! task complete return to calling subroutine
            endif
          endif
        ENDIF
        GOTO 3  ! regenerate and display inner menu
      else
        goto 40 ! regenerate the category list and display
      endif
      call usrmsg(' ',' ','-')
      goto 40   ! regenerate the category list and display

      END



C ************* ELISTCFC 
C ELISTCFC: Controls display and editing of CFC layer commons.
C If ACTION = 'M' then include editing if ACTION = '-' only
C allow choice.
      SUBROUTINE ELISTCFC(iwhich,chgdb,ACTION,icfcarrayindex,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
#include "epara.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer iwhich         ! is the legacy database record CLARIFY USE
      logical chgdb          ! flagged true if a mod made during session
      integer icfcarrayindex ! is the returned index in matdatarray

      common/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit
      LOGICAL OK,DOK,MODDB

C CLSDES (30 char) is an array for menu items listing each class.
C CLSSEL (36 char) is an array for menu items for the selected class.
      DIMENSION CLSDES(300),PDBM(500),CLSSEL(300),ICLSSEL(300)
      integer iwhicharray   ! for each item in menu point back to array
      dimension iwhicharray(60)
      character ACTION*1,CLSSEL*36,lltmp*144,CLSDES*32
      CHARACTER KEY*1,PDBM*76,outs*124
      CHARACTER xfile*144,tg*1,delim*1,t32*32,t248*248
      character lworking*144,fs*1
      logical closemat1
      logical mod   ! to signal whether an item has been altered.
      logical unixok
      integer NCO,ICO,NITMS,INO,IW  ! max items and current menu item
      integer ilist, imax, J

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

C Multipage menu
      integer svIPM, svMPM, svIST, svMIFULL, iItem, svcfccat
      DIMENSION svcfccat(300)

      helpinsub='ELISTCFC'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Assume no changes to db amd user has not entered password.
      MODDB=.FALSE.
      chgdb=.false.
      t32 = ' '
      IAF=IFIL+1
      iwhich=1   ! initial material selections

C Check that the CFClayers db is not empty.
    4 IER=0
      call eclose(cfcver,1.1,0.001,closemat1)
      if(closemat1)then
        IF(cfccats.LE.0)THEN
          CALL USRMSG(' CFC layers db is empty.',' ','W')
          IER=1
          RETURN
        ENDIF
      endif
      call usrmsg('  ','  ','-')   ! clear editing box.

C Create a menu showing the available database items.  Allow user to
C select one and then edit/list details.  Setup for multi-page menu.
      ILEN=cfccats
      IPACT=CREATE
      CALL EKPAGE(IPACT)

   40 ILEN=cfccats
      
C Setup for menu.
C Gather the names of the various classes and present this list.
      ICO=-1
      CLSDES(1)=  '  Description         No. Items'
      M=1
      DO 44 I=1,ILEN
        IF(I.GE.IST.AND.(I.LE.(IST+MIFULL)))THEN
          write(CLSSEL(I),'(A)') cfccatname(I)(1:32)
          IF(cfccatitems(I).GT.0)THEN
            M=M+1
            svcfccat(M)=I
            CALL EMKEY(M-1,KEY,IER)
            WRITE(CLSDES(M),'(A1,1X,A,1X,I3)')KEY,
     &      cfccatname(I)(1:26),cfccatitems(I)
          ENDIF
        ENDIF
   44 CONTINUE

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        CLSDES(M+1)='  __________________________  '
      ELSE
        WRITE(CLSDES(M+1),15)IPM,MPM 
   15   FORMAT     ('0 -----Page: ',I2,' of ',I2,' -------')
      ENDIF

      !CLSDES(M+1)= '  __________________________  '
      if(ACTION.eq.'M'.or.ACTION.eq.'m')then
        CLSDES(M+2)='+ add a classification        '
        CLSDES(M+3)='! list database entries       '
      elseif(ACTION.eq.'-')then
        CLSDES(M+2)='                              '
        CLSDES(M+3)='                              '
        call edisp(iuout,' ')
        call edisp(iuout,' Select classification to view items')
      endif
      CLSDES(M+4)=  '? help                        '
      CLSDES(M+5)=  '- exit                        '
      NCO=M+5
      ICO=-2

C Save menu state for EKPAGE since there are two menus to keep
C track of. 
      svIPM=IPM
      svMPM=MPM
      svIST=IST
      svMIFULL=MIFULL

C Help text for this menu.
      helptopic='cat_list_CFC_layers'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      CALL EMENU('CFC Layer Classes',CLSDES,NCO,ICO)
      IF(ICO.EQ.NCO)THEN
        RETURN
      ELSEIF(ICO.EQ.(NCO-1))THEN
        helptopic='cat_list_CFC_layers'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('CFC layers menu',nbhelp,'-',0,0,IER)
      ELSEIF(ICO.EQ.(NCO-2))THEN

C List one or more classifications.
        INPIC=cfccats
        CALL EPICKS(INPIC,ICLSSEL,' ',' Which classes to list:',
     &    36,cfccats,CLSSEL,'CFC layer classes',IER,nbhelp)
        IF(INPIC.EQ.0)GOTO 40
        CALL EASKABC(' Reporting to:',' ',
     &    'text feedback','summary file','cancel',irpt,nbhelp)
        if(irpt.eq.1)then
          itu = iuout
        elseif(irpt.eq.2)then
          itu = ixunit
          write(xfile,'(a)') 'CFC_layer_listing.txt'  ! initial file name
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','prim db text',IER)

C If user canceled the listing the reset unit to iuout and loop back.
          if(ier.eq.-3)then
            itu = iuout
            goto 40
          endif
        elseif(irpt.eq.3)then
          goto 40
        endif
        if(ipathcfc.eq.0.or.ipathcfc.eq.1)then
          call edisp(itu,'In the CFC layers database: ')
          call edisp(itu, LCFCDB)
        elseif(ipathcfc.eq.2)then
          call edisp(itu,'In the standard CFC layers database: ')
          call edisp(itu, LCFCDB)
        endif
        call edisp(itu,' ')
        do 42 ilist=1,INPIC
          IC=ICLSSEL(ilist)
          IF(cfccatitems(IC).GT.0)THEN
            WRITE(outs,'(3a,i3,a)')' Classification: ',
     &        cfccatname(IC)(1:lnblnk(cfccatname(IC))),' (',IC,')'
            call edisp(itu,outs)
            call edisp248(itu,cfccatdoc(IC),100)
            call edisp(itu,' ')
            call edisp(itu,
     &      'Item|Conduc-|Den- |Specif|Description')
            call edisp(itu,
     &      '     tivity |sity |heat  |of material')

C Loop through all of the items in the array and list out those that are
C associated with this class.
            DO 47 J=1,cfcdbitems
              if(cfccatindex(J).eq.IC)then
                lsn=MIN0(lnblnk(cfcname(J)),32)
                WRITE(outs,46)cfcitmindex(J),cfcdbcon(J),cfcdbden(J),
     &            cfcdbsht(J),cfcname(J)(1:lsn)
   46           FORMAT(i4,F9.3,F6.0,F7.0,2X,A)
                call edisp(itu,outs)
              endif
   47       CONTINUE
          ELSE
            call edisp(itu,' No items in this classification.')
          ENDIF
   42   continue
        call edisp(itu,' ')
        write(outs,'(2a)')
     &    ' Units: Conductivity W/(m deg.C), Density kg/m**3,',
     &    ' Specific Heat J/(kg deg.C)'
        call edisp(itu,outs)
        if(irpt.eq.2)then

C Call ctlexp a 2nd time to close the file.
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','prim db',IER)
        endif
      ELSEIF(ICO.EQ.(NCO-3))THEN

C Add another category to the database << not yet tested >>
        if(cfccats.LT.MGCL_CFC)then
          cfccats=cfccats+1
          t32=' '
          CALL EASKS(t32,' Classification name (<32 char)?',' ',
     &      32,' ','class name',IER,nbhelp)
          write(cfccatname(cfccats),'(a)') t32
          write(cfccatdoc(cfccats),'(a,i2,3a)')
     &      'Category (',cfccats,') named ',t32(1:lnblnk(t32)),
     &      ' was inserted manually. No other documentation (yet).'
          ILNE=cfcdbitems+1
          cfcname(ILNE)='new_CFClayer'
          cfcdoc(ILNE)='no documentation (yet)'
          cfccatindex(ILNE)=cfccats
          cfcdbcon(ILNE)=1.0
          cfcdbden(ILNE)=1.0
          cfcdbsht(ILNE)=1.0
          cfcdbthick(ILNE)=15.0

          CFCsolreflout(ILNE)=1.0
          CFCsolreflin(ILNE)=1.0
          CFCsoltrandir(ILNE)=1.0
          CFCsoltrantotout(ILNE)=1.0
          CFCsoltrantotin(ILNE)=1.0
          CFCemissout(ILNE)=1.0
          CFCemissin(ILNE)=1.0
          CFClwtran(ILNE)=1.0
          CFCdrpwidth(ILNE)=1.0
          CFCdrpspacing(ILNE)=1.0
          CFCslatorient(ILNE)='HORZ'

          cfccatitems(cfccats)=cfccatitems(cfccats)+1 ! increment nb of cat items
          cfccatindex(ILNE)=cfccats  ! remember its category
          cfcdbitems=cfcdbitems+1    ! increment nb of db items

C.........Search for maximum item index in the db and add 1
          imax=0
          DO J=1,cfcdbitems
            if(cfcitmindex(J).gt.imax)then
              imax=cfcitmindex(J)
            endif
          END DO
          cfcitmindex(ILNE)=imax+1

          chgdb=.true.
          MODDB=.true.
        endif
        goto 40
      ELSEIF(ICO.EQ.(NCO-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPM=svIPM
          MPM=svMPM
          IST=svIST
          MIFULL=svMIFULL
          ILEN=cfccats
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(ICO.GT.1.AND.ICO.LT.(NCO-4))THEN
        
C Loop through data for this classification for manipulation.
        !IC=ICO-1
        IC = svcfccat(ICO)
        IF(cfccatitems(IC).EQ.0)THEN
         CALL USRMSG(' No items in this classification.',' ','W')
         GOTO 40
        ENDIF
C Create a menu showing the available database items.  Allow user to
C select one and then edit/list details.  Setup for multi-page menu.
        ILEN=cfccatitems(IC)
        IPACT=CREATE
        CALL EKPAGE(IPACT)

    3   ILEN=cfccatitems(IC)

C Display the materials of the classification in a menu.
        PDBM(1)= ' Units: Conductivity W/(m deg.C), Density kg/m**3'
        PDBM(2)= ' Specific Heat J/(kg deg.C)'
        WRITE(PDBM(3),'(3a,i3,a,i3,a)') '1 Classification: ',
     &    cfccatname(IC)(1:lnblnk(cfccatname(IC))),' (',IC,') with ',
     &    cfccatitems(IC),' entries.'
        PDBM(4)=   '  ___________________________________'
        PDBM(5)=
     &' Item|Conduc-|Den- |Specif|Description of layer'
        PDBM(6)=
     &'     |tivity |sity |heat  |name   : documentation'
        M=6

        iItem=1
        DO 30 J=1,cfcdbitems
C Loop through all of the items in the array and list out those that are
C associated with this class.
          if(cfccatindex(J).eq.IC)then
            IF(iItem.GE.IST.AND.(iItem.LE.(IST+MIFULL)))THEN
C Remember the array index that goes with the menu position (m).
              M=M+1
              iwhicharray(M)=J  ! menu position M relates to data array J.
              CALL EMKEY(M-6,KEY,IER)
              WRITE(PDBM(M),25)KEY,cfcitmindex(J),
     &          cfcdbcon(J),cfcdbden(J),
     &          cfcdbsht(J),
     &          cfcname(J)(1:12),': ',cfcdoc(J)(1:21)
   25         FORMAT(A1,i4, F8.3,F6.0,F7.0,1X,3A)
            ENDIF
            iItem = iItem+1
          endif
   30   CONTINUE
        NC=M

C If a long list include page facility text.      
        IF(IPFLG.EQ.0)THEN
          PDBM(NC+1)='  _____________________________  '
        ELSE
          WRITE(PDBM(NC+1),16)IPM,MPM 
   16     FORMAT     ('0 -----Page: ',I2,' of ',I2,' -------')
        ENDIF

        if(ACTION.eq.'M'.or.ACTION.eq.'m')then
          PDBM(NC+2)='* add/ delete/ copy CFC layer     '

C Remind user whether they are working with a model file or
C a common data file.
          if(ipathcfc.eq.0.or.ipathcfc.eq.1)then
            PDBM(NC+3)='! save CFC layers file           '
          else
            PDBM(NC+3)='! save common CFC layers file    '
          endif
        elseif(ACTION.eq.'-')then
          PDBM(NC+2)='                                 '
          PDBM(NC+3)='                                 '
        endif
        PDBM(NC+4)=  '? help                           '
        PDBM(NC+5)=  '- exit this menu                 '
        NITMS=NC+5
        INO=-4

C Help text for this menu.
    2   continue
        helptopic='CFC_layers_db_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
 
        CALL EMENU('CFC Layers Database',PDBM,NITMS,INO)

        IF(INO.EQ.NITMS.OR.INO.EQ.0)THEN
          IF(MODDB.and.ACTION.eq.'M')THEN

C Changes were made so remove the existing file via delfiledosorunix
C and write a new file based on current common blocks. If the original
C file was binary then alter the name of the saved file to add a '.a'
C at the end.
            dok=.true.
            CALL ASKOK('Changes have been made in this file. ',
     &        'Save the CFC layers changes?',OK,dok,nbhelp)
            IF(.NOT.OK)GOTO 4
            chgdb=.true.   ! pass back signal to update
            CALL ERPFREE(IFMUL,ISTAT)  ! in case other unit is still open
            CALL ERPFREE(IAF,ISTAT)    ! in case unit is still open

C Check the value of whichdbpath variable to see what to do with
C this file. If local or absolute path then use lltmp directly. If in
C the standard location then create lworking which has path
C prepended.
            if(ipathcfc.eq.0.or.ipathcfc.eq.1)then
              lltmp=' '
              write(lltmp,'(a)') LCFCDB(1:lnblnk(LCFCDB))
C              call delfiledosorunix(lltmp,ider)
              CALL mkascicfc(IAF,lltmp,IER)
            elseif(ipathcfc.eq.2)then
              lndbp=lnblnk(standarddbpath)
              write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &            LCFCDB(1:lnblnk(LCFCDB))
C Debug.
C              write(6,*) 'working materials ',lworking
              CALL mkascicfc(IAF,lworking,IER)
            endif

            MODDB=.FALSE.
          ENDIF
          GOTO 4
        ELSEIF(INO.EQ.1.OR.INO.EQ.2)THEN
          GOTO 2
        ELSEIF(INO.eq.3)THEN

C Edit classification name.
          write(t32,'(a)')cfccatname(IC)(1:lnblnk(cfccatname(IC)))
          CALL EASKS(t32,' ',' Classification name (<32 char)?',
     &      32,' ','class name',IER,nbhelp)
          if(t32(1:2).ne.'  ')then
            write(cfccatname(IC),'(a)') t32(1:lnblnk(t32))
            chgdb=.true.
            MODDB=.TRUE.
          endif

C Also edit classification documentation.
          t248=cfccatdoc(IC)(1:lnblnk(cfccatdoc(IC)))
          CALL EASKS248(t248,'Category notes:',' ',
     &      72,'this category...','category notes',IER,nbhelp)
          if(t248(1:2).ne.'  ')then
            write(cfccatdoc(IC),'(a)') t248(1:lnblnk(t248))
            chgdb=.true.
            MODDB=.TRUE.
          endif
        ELSEIF(INO.GE.4.AND.INO.LE.6)THEN
          GOTO 2
        ELSEIF(INO.EQ.NITMS-1)THEN
          helptopic='CFC_layers_db_overview'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('CFC layers database',nbhelp,'-',0,0,IER)
        ELSEIF(INO.EQ.NITMS-2)THEN

C Save CFC layers db. First delete the
C existing file via delfiledosorunix and then write
C a new file via mkascicfc. If it was an older file format
C and the user has not made any changes confirm that the
C file is to be updated.
          IF(.NOT.MODDB)THEN
            dok=.true.
            CALL ASKOK(
     &        'No changes in the CFC layers file. Do you want',
     &        'to update it anyway:',OK,dok,nbhelp)
            if(.NOT.OK)then
              goto 3  ! jump without doing anything
            endif
          endif
          chgdb=.true.
          call erpfree(icfcdb,istat)  ! in case it is still open
          call erpfree(iaf,istat)  ! in case it is still open

          lltmp=' '
          if(ipathcfc.eq.0.or.ipathcfc.eq.1)then
            write(lltmp,'(a)') LCFCDB(1:lnblnk(LCFCDB))
C            call delfiledosorunix(lltmp,ider)
            CALL mkascicfc(IAF,lltmp,IER)
          elseif(ipathcfc.eq.2)then
            lndbp=lnblnk(standarddbpath)
            write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &        LCFCDB(1:lnblnk(LCFCDB))
            CALL mkascicfc(IAF,lworking,IER)
          endif

C Reset flag to show db is current with local arrays.
          MODDB=.FALSE.
          CALL USRMSG(' ','Changes saved...','-')

        ELSEIF(INO.EQ.NITMS-3)THEN

C Add delete or copy CFC layer from db.
          CALL EASKATOG('Options: ',' ','Delete existing CFC layer',
     &      'Add CFC layer',
     &      'copy existing CFC layer','cancel',' ',' ','  ',IW,nbhelp)
          IF(IW.EQ.1)THEN

C Ask the user which one to remove then loop through CFC layers and
C copy the contents of CFC layer > IWHICH into the next lower one.
            CALL USRMSG(' ',' Which item of list? ','-')
            CALL EMENU('delete CFC layer',PDBM,NITMS,IW)
            IWHICH=iwhicharray(IW)
            DO 791 IVV=IWHICH,cfcdbitems-1
              cfcdbcon(IVV)=cfcdbcon(IVV+1)
              cfcdbden(IVV)=cfcdbden(IVV+1)
              cfcdbsht(IVV)=cfcdbsht(IVV+1)
              cfcdbthick(IVV)=cfcdbthick(IVV+1)
              cfccatindex(IVV)=cfccatindex(IVV+1)  ! keep in same category
              cfcitmindex(IVV)=cfcitmindex(IVV+1)
              cfcname(IVV)=cfcname(IVV+1)
              cfcdoc(IVV)=cfcdoc(IVV+1)

              CFCshdtp(IVV) = CFCshdtp(IVV+1)
              CFCsolreflout(IVV) = CFCsolreflout(IVV+1)
              CFCsolreflin(IVV) = CFCsolreflin(IVV+1)
              CFCsoltrandir(IVV) =  CFCsoltrandir(IVV+1)
              CFCsoltrantotout(IVV) = CFCsoltrantotout(IVV+1)
              CFCsoltrantotin(IVV) = CFCsoltrantotin(IVV+1)
              CFCemissout(IVV) = CFCemissout(IVV+1)
              CFCemissin(IVV) = CFCemissin(IVV+1)
              CFClwtran(IVV) = CFClwtran(IVV+1)
              CFCdrpwidth(IVV) = CFCdrpwidth(IVV+1)
              CFCdrpspacing(IVV) = CFCdrpspacing(IVV+1)
              CFCwireemiss(IVV) = CFCwireemiss(IVV+1)
              CFCwirediam(IVV) = CFCwirediam(IVV+1)
              CFCwirespace(IVV) = CFCwirespace(IVV+1)
              CFCslattran(IVV) = CFCslattran(IVV+1)
              CFCslatwidth(IVV) = CFCslatwidth(IVV+1)
              CFCslatspacing(IVV) = CFCslatspacing(IVV+1)
              CFCslatangle(IVV) = CFCslatangle(IVV+1)
              CFCslatorient(IVV) = CFCslatorient(IVV+1)
              CFCslatcrown(IVV)  = CFCslatcrown(IVV+1)
              CFCslatwr(IVV) = CFCslatwr(IVV+1)
              CFCslatthk(IVV) = CFCslatthk(IVV+1)
              CFCfillAir(IVV) = CFCfillAir(IVV+1)
              CFCfillAr(IVV) = CFCfillAr(IVV+1)
              CFCfillKr(IVV) = CFCfillKr(IVV+1)
              CFCfillXe(IVV)  = CFCfillXe(IVV+1)
              CFCfillSF6(IVV) = CFCfillSF6(IVV+1)

  791       CONTINUE
            cfcdbitems=cfcdbitems-1
            cfccatitems(IC)=cfccatitems(IC)-1
            MODDB=.TRUE.
            chgdb=.true.
          ELSEIF(IW.EQ.2)THEN

C Add a CFC layer, initiate it to the values of last material in class.
            IF(cfcdbitems.LT.MGIT_CFC)THEN
              ILNE=cfcdbitems+1
              lastcatitem=0
              DO 793 J=1,cfcdbitems
                if(cfccatindex(J).eq.IC)then
                  lastcatitem=J
                endif
  793         continue

              if(lastcatitem.gt.0)then

                cfcname(ILNE)='new_cfc_layer'

                cfcdbcon(ILNE)=cfcdbcon(lastcatitem)
                cfcdbden(ILNE)=cfcdbden(lastcatitem)
                cfcdbsht(ILNE)=cfcdbsht(lastcatitem)
                cfcdbthick(ILNE)=cfcdbthick(lastcatitem)
                cfccatindex(ILNE)=cfccatindex(lastcatitem)  ! keep in same category

C...............Search for maximum item index in the db and add 1
                imax=0
                DO J=1,cfcdbitems
                  if(cfcitmindex(J).gt.imax)then
                    imax=cfcitmindex(J)
                  endif
                END DO
                cfcitmindex(ILNE)=imax+1

C If there is room add a note about its origin.
                ils=lnblnk(cfcdoc(lastcatitem))
                iln=lnblnk(cfcname(lastcatitem))
                if(ils.le.200)then
                  write(cfcdoc(ILNE),'(4a)') 
     &              cfcdoc(IWHICH)(1:ils),' (derived from ',
     &              cfcname(lastcatitem)(1:iln),')'
                else
                  cfcdoc(ILNE)=cfcdoc(lastcatitem)
                endif

                CFCshdtp(ILNE) = CFCshdtp(lastcatitem)
                CFCsolreflout(ILNE) = CFCsolreflout(lastcatitem)
                CFCsolreflin(ILNE) = CFCsolreflin(lastcatitem)
                CFCsoltrandir(ILNE) =  
     &                  CFCsoltrandir(lastcatitem)
                CFCsoltrantotout(ILNE) = 
     &                  CFCsoltrantotout(lastcatitem)
                CFCsoltrantotin(ILNE) = 
     &                  CFCsoltrantotin(lastcatitem)
                CFCemissout(ILNE) = CFCemissout(lastcatitem)
                CFCemissin(ILNE) = CFCemissin(lastcatitem)
                CFClwtran(ILNE) = CFClwtran(lastcatitem)
                CFCdrpwidth(ILNE) = CFCdrpwidth(lastcatitem)
                CFCdrpspacing(ILNE) = 
     &                  CFCdrpspacing(lastcatitem)
                CFCwireemiss(ILNE) = CFCwireemiss(lastcatitem)
                CFCwirediam(ILNE) = CFCwirediam(lastcatitem)
                CFCwirespace(ILNE) = CFCwirespace(lastcatitem)
                CFCslattran(ILNE) = CFCslattran(lastcatitem)
                CFCslatwidth(ILNE) = CFCslatwidth(lastcatitem)
                CFCslatspacing(ILNE) = 
     &                  CFCslatspacing(lastcatitem)
                CFCslatangle(ILNE) = CFCslatangle(lastcatitem)
                CFCslatorient(ILNE) = CFCslatorient(lastcatitem)
                CFCslatcrown(ILNE)  = CFCslatcrown(lastcatitem)
                CFCslatwr(ILNE) = CFCslatwr(lastcatitem)
                CFCslatthk(ILNE) = CFCslatthk(lastcatitem)
                CFCfillAir(ILNE) = CFCfillAir(lastcatitem)
                CFCfillAr(ILNE) = CFCfillAr(lastcatitem)
                CFCfillKr(ILNE) = CFCfillKr(lastcatitem)
                CFCfillXe(ILNE)  = CFCfillXe(lastcatitem)
                CFCfillSF6(ILNE) = CFCfillSF6(lastcatitem)

                cfcdbitems=cfcdbitems+1
                cfccatitems(IC)=cfccatitems(IC)+1

C Browse/Edit the details of this new item. Mark as unmodified first
C and then resetn MODDB and chgdb if mod is true.
                mod=.false.
                call edonecfc(ILNE,mod,ier) 
                MODDB=.TRUE.
                chgdb=.true.
              endif
            ELSE
              CALL USRMSG(' ','Exceeds classification limit!','W')
              GOTO 3
            ENDIF

          ELSEIF(IW.EQ.3)THEN

C Copy an existing CFC layer to a new one at end of the array.
C Use iwhicharray to go from the menu position to the array
C end of the array.  
            CALL USRMSG(' ',' Which item of list?','-')
            CALL EMENU('Copy CFC layer',PDBM,NITMS,IW)
            IWHICH=iwhicharray(IW)
            ILNE=cfcdbitems+1

            write(cfcname(ILNE),'(2a)') 
     &          cfcname(IWHICH)(1:lnblnk(cfcname(IWHICH))),'c'

C If there is room add a note about its origin.
            ils=lnblnk(cfcdoc(IWHICH))
            iln=lnblnk(cfcname(IWHICH))
            if(ils.le.200)then
              write(cfcdoc(ILNE),'(4a)') 
     &          cfcdoc(IWHICH)(1:ils),' (copy of ',
     &          cfcname(IWHICH)(1:iln),')'
            else
              cfcdoc(ILNE)=cfcdoc(IWHICH)
            endif
            cfcdbcon(ILNE)=cfcdbcon(IWHICH)
            cfcdbden(ILNE)=cfcdbden(IWHICH)
            cfcdbsht(ILNE)=cfcdbsht(IWHICH)
            cfcdbthick(ILNE)=cfcdbthick(IWHICH)
            cfccatindex(ILNE)=cfccatindex(IWHICH)  ! keep in same category

C...........Search for maximum item index in the db and add 1
            imax=0
            DO J=1,cfcdbitems
              if(cfcitmindex(J).gt.imax)then
                imax=cfcitmindex(J)
              endif
            END DO
            cfcitmindex(ILNE)=imax+1

C...........CFC shade or fill gas layer attributes
            CFCshdtp(ILNE) = CFCshdtp(IWHICH)
            CFCsolreflout(ILNE) = CFCsolreflout(IWHICH)
            CFCsolreflin(ILNE) = CFCsolreflin(IWHICH)
            CFCsoltrandir(ILNE) =  
     &                  CFCsoltrandir(IWHICH)
            CFCsoltrantotout(ILNE) = 
     &                  CFCsoltrantotout(IWHICH)
            CFCsoltrantotin(ILNE) = 
     &                  CFCsoltrantotin(IWHICH)
            CFCemissout(ILNE) = CFCemissout(IWHICH)
            CFCemissin(ILNE) = CFCemissin(IWHICH)
            CFClwtran(ILNE) = CFClwtran(IWHICH)
            CFCdrpwidth(ILNE) = CFCdrpwidth(IWHICH)
            CFCdrpspacing(ILNE) = 
     &                  CFCdrpspacing(IWHICH)
            CFCwireemiss(ILNE) = CFCwireemiss(IWHICH)
            CFCwirediam(ILNE) = CFCwirediam(IWHICH)
            CFCwirespace(ILNE) = CFCwirespace(IWHICH)
            CFCslattran(ILNE) = CFCslattran(IWHICH)
            CFCslatwidth(ILNE) = CFCslatwidth(IWHICH)
            CFCslatspacing(ILNE) = 
     &                  CFCslatspacing(IWHICH)
            CFCslatangle(ILNE) = CFCslatangle(IWHICH)
            CFCslatorient(ILNE) = CFCslatorient(IWHICH)
            CFCslatcrown(ILNE)  = CFCslatcrown(IWHICH)
            CFCslatwr(ILNE) = CFCslatwr(IWHICH)
            CFCslatthk(ILNE) = CFCslatthk(IWHICH)
            CFCfillAir(ILNE) = CFCfillAir(IWHICH)
            CFCfillAr(ILNE) = CFCfillAr(IWHICH)
            CFCfillKr(ILNE) = CFCfillKr(IWHICH)
            CFCfillXe(ILNE)  = CFCfillXe(IWHICH)
            CFCfillSF6(ILNE) = CFCfillSF6(IWHICH)

            cfcdbitems=cfcdbitems+1
            cfccatitems(IC)=cfccatitems(IC)+1
            MODDB=.TRUE.
            chgdb=.true.

          ELSEIF(IW.EQ.4)THEN
            GOTO 3    ! user declined
          ENDIF
        ELSEIF(INO.EQ.(NITMS-4))THEN

C If there are enough items allow paging control via EKPAGE.
          IF(IPFLG.EQ.1)THEN
            ILEN=cfccatitems(IC)
            IPACT=EDIT
            CALL EKPAGE(IPACT)
          ENDIF
        ELSEIF(INO.GT.6.AND.INO.LT.NITMS-4)THEN

C Identified one of the materials to edit or select.
          IFOC=iwhicharray(INO)

C Debug.
C          write(6,*) 'selected menu ',ino,'got array',ifoc

          if(ACTION.eq.'M')then

C Browse/Edit the details of this item. Mark as unmodified first
C and then resetn MODDB and chgdb if mod is true.
            mod=.false.
            call edonecfc(ifoc,mod,ier)
            if(mod)then 
              MODDB=.TRUE.
              chgdb=.true.
              icfcarrayindex=ifoc ! set to edited array index
            endif
          elseif(ACTION.eq.'-')then
            call edisp(iuout,' ')
            write(outs,'(2a)')
     &        ' Units: Conductivity W/(m deg.C), Density kg/m**3,',
     &        ' Specific Heat J/(kg deg.C)'
            call edisp(iuout,outs)
            call edisp(iuout,' ')
            call edisp(iuout,
     &    'Conduc- |Den-  |Specif|Description')
            call edisp(iuout,
     &    'tivity  |sity  |heat  |of material')
            lsn=MIN0(lnblnk(cfcname(IFOC)),32)
            WRITE(outs,244)cfcdbcon(IFOC),
     &        cfcdbden(IFOC),cfcdbsht(IFOC),cfcname(IFOC)(1:lsn)
  244       FORMAT(F9.3,F7.1,F7.0,1X,A)
            CALL EDISP(iuout,outs)
            CALL EDISP248(iuout,cfcdoc(IFOC),90)
            write(outs,'(4a)') 'CFC layer ',
     &        cfcname(IFOC)(1:lsn),' : ',cfcdoc(IFOC)(1:48)
            dok=.true.
            CALL ASKOK(outs,'Use it:',OK,dok,1)
            if(OK)then
              icfcarrayindex=IFOC ! set to selected array index
              return
            endif
          endif
        ENDIF
        GOTO 3
      else
        goto 40
      endif
      call usrmsg(' ',' ','-')
      goto 40

      END


C ************* EDONEMAT 
C EDONEMAT: Edits a material in the common block at array index index.
C mod is returned as true if data has changed.
      SUBROUTINE EDONEMAT(index,mod,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C Passed parameters.
      integer index  ! material array index
      logical mod    ! set to true if modified
      integer ier    ! non-zero if there is an issue

      COMMON/OUTIN/IUOUT,IUIN

      DIMENSION PELM(30)
      CHARACTER PELM*40,tnam*32,tdoc*248,message*32
      real DBCON,DBDEN,DBSHT,EOUT,EIN,AOUT,AIN,DBDRV,THICK
      real IRTRAN,SOLDRTRN,soldrotrfl,soldrinrfl,vistran
      real visotrfl,visinrfl,render
      real airg1,airg2,airg3  ! for gap resistance
      logical modify,dok,ok

C Local variables for gas materials.
      integer jsur   ! for radio button

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDONEMAT'  ! set for subroutine

C If index is outwith range just return.
      if(index.gt.0.and.index.le.matdbitems)then
        mod=.false.
      else
        return
      endif

C Work with local variables.
      modify=.false.
      write(tnam,'(a)') matname(index)(1:lnblnk(matname(index)))
      write(tdoc,'(a)') matdoc(index)(1:lnblnk(matdoc(index)))
      DBCON = matdbcon(index)
      DBDEN = matdbden(index)
      DBSHT = matdbsht(index)
      EOUT = matdboute(index)
      EIN = matdbine(index)
      AOUT = matdbouta(index)
      AIN = matdbina(index)
      dbdrv = matdbdrv(index)
      THICK = matdbthick(index)
      IRTRAN = matirtran(index)
      SOLDRTRN = matsoldrtrn(index)
      soldrotrfl = matvisotrfl(index)
      soldrinrfl = matsoldrinrfl(index)
      vistran= matvistran(index)
      visotrfl= matvisotrfl(index)
      visinrfl= matvisinrfl(index)
      render= matrender(index)
      airg1=matgapares(index,1)
      airg2=matgapares(index,2)
      airg3=matgapares(index,3)

   3  IW=-4
      WRITE(PELM(1),'(A,1X,A)')    'a Name:',tnam(1:32)
      WRITE(PELM(2),'(A,1X,A)')    'b Note:',tdoc(1:32)
      WRITE(PELM(3),'(A,1X,F10.4)')'c Conductivity (W/(m-K)  :',DBCON
      WRITE(PELM(4),'(A,1X,F8.2)') 'd Density (kg/m**3)      :',DBDEN
      WRITE(PELM(5),'(A,1X,F8.2)') 'e Specific Heat (J/(kg-K):',DBSHT
      WRITE(PELM(6),'(A,1X,F8.3)') 'f Emissivity out (-)     :',EOUT
      WRITE(PELM(7),'(A,1X,F8.3)') 'g Emissivity in (-)      :',EIN
      WRITE(PELM(8),'(A,1X,F8.3)') 'h Absorptivity out (-)   :',AOUT
      WRITE(PELM(9),'(A,1X,F8.3)') 'i Absorptivity in (-)    :',AIN
      WRITE(PELM(10),'(A,F9.2)')   'j Vapour res (MNs g^-1m^-1):',dbdrv
      WRITE(PELM(11),'(A,F9.2)')   'k Default thickness (mm) :',THICK

C Depending on whether the item is legacy (-), opaque (o), transp (t),
C or a gas (g), include additional items.
      if(matopaq(index).eq.'-')then
        WRITE(PELM(12),'(A)')  'l type >>legacy opaque'
        m=12
      elseif(matopaq(index).eq.'o')then
        WRITE(PELM(12),'(A)')  'l type >>opaque       '
        m=12
      elseif(matopaq(index).eq.'t')then
       WRITE(PELM(12),'(A)')  'l type >>transparent  '
       WRITE(PELM(13),'(A,F7.3)')'m Longwave tran (-)      :',IRTRAN
       WRITE(PELM(14),'(A,F7.3)')'n Solar direct tran (-)  :',SOLDRTRN
       WRITE(PELM(15),'(A,F7.3)')'o Solar reflec out (-)   :',soldrotrfl
       WRITE(PELM(16),'(A,F7.3)')'p Solar refled in (-)    :',soldrinrfl
       WRITE(PELM(17),'(A,F7.3)')'q Visable tran (-)       :',vistran
       WRITE(PELM(18),'(A,F7.3)')'r Visable reflec out (-) :',visotrfl
       WRITE(PELM(19),'(A,F7.3)')'s Visable reflec in (-)  :',visinrfl
       WRITE(PELM(20),'(A,F7.2)')'t Colour rendering (-)   :',render
        m=20
      elseif(matopaq(index).eq.'g')then
        WRITE(PELM(12),'(A)')  'l type >>gap layer'
        WRITE(PELM(13),'(A,F9.5)') 'm gap resistance wall    :',airg1
        WRITE(PELM(14),'(A,F9.5)') 'n gap resistance flr/ceil:',airg2
        WRITE(PELM(15),'(A,F9.5)') 'o gap resistance other   :',airg3
        call edisp(iuout,'For gaps ignore Con:Den:SpHt values.')
        m=15
      endif
      PELM(m+1) =' _____________________ '
      PELM(m+2)='? Help                 '
      PELM(m+3)='- Exit                 '
      NELM=m+3   ! number of items to display

C Help text for this menu.
   2  continue
      helptopic='material_db_edit_one'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('Material details',PELM,NELM,IW)
      IF(IW.EQ.0.OR.IW.EQ.NELM)THEN
C If there has been a modification check with user prior to
C updating the common blocks.
        if(modify)then
          dok=.true.
          CALL ASKOK(' ','Accept changes in material (see help)?',
     &      OK,dok,nbhelp)
          if(OK)then
            write(matname(index),'(a)') tnam(1:lnblnk(tnam))
            write(matdoc(index),'(a)') tdoc(1:lnblnk(tdoc))
            matdbcon(index) = DBCON
            matdbden(index) = DBDEN
            matdbsht(index) = DBSHT
            matdboute(index) = EOUT
            matdbine(index) = EIN
            matdbouta(index) = AOUT
            matdbina(index) = AIN
            matdbdrv(index) = dbdrv
            matdbthick(index) = THICK
            matirtran(index) = IRTRAN
            matsoldrtrn(index) = SOLDRTRN
            matvisotrfl(index) = soldrotrfl
            matsoldrinrfl(index) = soldrinrfl
            matvistran(index) = vistran
            matvisotrfl(index) = visotrfl
            matvisinrfl(index) = visinrfl
            matrender(index) = render
            matgapares(index,1)=airg1
            matgapares(index,2)=airg2
            matgapares(index,3)=airg3

            modify=.false.
            mod=.true.   ! pass this back to calling code.
          endif
        endif
        RETURN
      ELSEIF(IW.EQ.NELM-2)THEN
        GOTO 3
      ELSEIF(IW.EQ.1)THEN
        CALL EASKS(tnam,'Description of material','confirm:',
     &    32,' ','material name',IER,nbhelp)
        if(tnam(1:2).ne.'  ')then
          modify=.true.
        endif
      ELSEIF(IW.EQ.2)THEN
        CALL EASKS248(tdoc,'Documentation for material','confirm:',
     &    72,' ','material documentation',IER,nbhelp)
        if(tdoc(1:2).ne.'  ')then
          modify=.true.
        endif
      ELSEIF(IW.EQ.3)THEN
        CALL EASKR(DBCON,'Conductivity (W/(m deg.C)','confirm:',
     &    0.001,'W',300.,'W',1.,'conductivity',IER,nbhelp)
        modify=.true.
      ELSEIF(IW.EQ.4)THEN
        CALL EASKR(DBDEN,'Density (kg/m**3)','confirm:',
     &    1.0,'W',9000.,'W',100.,'density',IER,nbhelp)
        modify=.true.
      ELSEIF(IW.EQ.5)THEN
        CALL EASKR(DBSHT,'Specific heat (J/(kg deg.C)','confirm:',
     &    1.0,'W',2000.,'W',100.,'specific heat',IER,nbhelp)
        modify=.true.
      ELSEIF(IW.EQ.6)THEN

C For legacy materials both faces should have the same value.
        if(matopaq(index).eq.'-')then
          CALL EASKR(EOUT,'Surface emissivity','confirm:',
     &      0.001,'W',0.999,'W',0.9,'out emissivity',IER,nbhelp)
          EIN=EOUT
          modify=.true.
        else
          CALL EASKR(EOUT,'Outside face emissivity','confirm:',
     &      0.001,'W',0.999,'W',0.9,'out emissivity',IER,nbhelp)
          modify=.true.
        endif
      ELSEIF(IW.EQ.7)THEN

C For legacy materials both faces should have the same value.
        if(matopaq(index).eq.'-')then
          CALL EASKR(EIN,'Surface emissivity','confirm:',
     &      0.001,'W',0.999,'W',0.9,'out emissivity',IER,nbhelp)
          EOUT=EIN
          modify=.true.
        else
          CALL EASKR(EIN,'Inside face emissivity','confirm:',
     &      0.001,'W',0.999,'W',0.9,'in emissivity',IER,nbhelp)
          modify=.true.
        endif
      ELSEIF(IW.EQ.8)THEN
        if(matopaq(index).eq.'-')then
          CALL EASKR(AOUT,'Surface absorptance','confirm:',
     &      0.001,'W',0.999,'W',0.9,'absorptance',IER,nbhelp)
          AIN=AOUT
          modify=.true.
        else
          CALL EASKR(AOUT,'Outside face absorptance','confirm:',
     &      0.001,'W',0.999,'W',0.9,'out absorptance',IER,nbhelp)
          modify=.true.
        endif
      ELSEIF(IW.EQ.9)THEN
        if(matopaq(index).eq.'-')then
          CALL EASKR(AIN,'Surface absorptance','confirm:',
     &      0.001,'W',0.999,'W',0.9,'absorptance',IER,nbhelp)
          AOUT=AIN
          modify=.true.
        else
          CALL EASKR(AIN,'Inside face absorptance','confirm:',
     &      0.001,'W',0.999,'W',0.9,'in absorptance',IER,nbhelp)
          modify=.true.
        endif
      ELSEIF(IW.EQ.10)THEN
        CALL EASKR(dbdrv,'Vapour resistvity (MNs g^-1m^-1)','confirm:',
     &    1.0,'W',20000.,'W',1.,'vapour resistvity',IER,nbhelp)
        modify=.true.
      ELSEIF(IW.EQ.11)THEN
        CALL EASKR(THICK,'Default thickness (mm)','confirm:',
     &    1.0,'W',500.,'W',10.,'default thickness',IER,nbhelp)
        modify=.true.
      ELSEIF(IW.EQ.12)THEN

C Offer to toggle between different types.
        helptopic='material_db_edit_type'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(matopaq(index).eq.'-')then
          message='(currently legacy opaque)'
        elseif(matopaq(index).eq.'o')then
          message='(currently opaque)'
        elseif(matopaq(index).eq.'t')then
          message='(currently transparent)'
        elseif(matopaq(index).eq.'g')then
          message='(currently gap data)'
        elseif(matopaq(index).eq.'h')then
          message='(currently gap data)'
        endif
        jsur=1
        CALL EASKATOG('Material options:',message,
     &    'legacy opaque','opaque','transparent',
     &    'gap layer ',' ', ' ', 
     &    'leave un-changed',jsur,nbhelp)
        if(jsur.eq.1)then
          matopaq(index)='-'
          modify=.true.
        elseif(jsur.eq.2)then
          matopaq(index)='o'
          modify=.true.
        elseif(jsur.eq.3)then
          matopaq(index)='t'
          modify=.true.
        elseif(jsur.eq.4)then
          matopaq(index)='g'
          modify=.true.
        else
          continue
        endif
        goto 3   ! need to re-establish how many items in menu

      ELSEIF(IW.gt.12.and.IW.LE.NELM-3)THEN
        if(matopaq(index).eq.'-')then
          goto 3
        elseif(matopaq(index).eq.'o')then
          goto 3
        elseif(matopaq(index).eq.'t')then

C Single layer optical properties editing.
          if(IW.eq.13)then
            CALL EASKR(IRTRAN,'Longwave transmittance (-)','confirm:',
     &        0.00,'W',1.00,'W',1.,'ir tran',IER,nbhelp)
            modify=.true.
          elseif(IW.eq.14)then
            CALL EASKR(SOLDRTRN,'Solar direct transmittance (-)',
     &        'confirm:',0.00,'W',1.00,'W',1.,'solar direct tran',IER,
     &         nbhelp)
            modify=.true.
          elseif(IW.eq.15)then
            CALL EASKR(soldrotrfl,'Solar reflectance outside face (-)',
     &        'confirm:',0.00,'W',1.00,'W',1.,'solar reflec outside',
     &        IER,nbhelp)
            modify=.true.
          elseif(IW.eq.16)then
            CALL EASKR(soldrinrfl,'Solar reflectance inside face (-)',
     &        'confirm:',0.00,'W',1.00,'W',1.,'solar reflec inside',
     &        IER,nbhelp)
            modify=.true.
          elseif(IW.eq.17)then
            CALL EASKR(vistran,'Visible trans (-)','confirm:',
     &        0.00,'W',1.00,'W',1.,'visible tran',IER,nbhelp)
            modify=.true.
          elseif(IW.eq.18)then
            CALL EASKR(visotrfl,'Visible reflectance outside face (-)',
     &        'confirm:',0.00,'W',1.00,'W',1.,'visible reflec outside',
     &        IER,nbhelp)
            modify=.true.
          elseif(IW.eq.19)then
            CALL EASKR(visinrfl,'Visible reflectance inside face (-)',
     &        'confirm:',0.00,'W',1.00,'W',1.,'visible reflec inside',
     &        IER,nbhelp)
            modify=.true.
          elseif(IW.eq.20)then !render
            CALL EASKR(visinrfl,'Colour rendering index (-)',
     &        'confirm:',0.00,'W',100.00,'W',1.,'colour rendering',
     &        IER,nbhelp)
            modify=.true.
          endif
          goto 3
        elseif(matopaq(index).eq.'g'.or.matopaq(index).eq.'h')then

C Gap resistance for wall orientations.
          if(IW.eq.13)then
            CALL EASKR(airg1,'Gap resistance (wall orientation)',
     &        'confirm:',0.00,'W',10.00,'W',1.,'gap r wall',IER,nbhelp)
            modify=.true.
          elseif(IW.eq.14)then
            CALL EASKR(airg2,'Gap resistance (floor/ceiling)',
     &        'confirm:',0.00,'W',10.00,'W',1.,'gap r flr',IER,nbhelp)
            modify=.true.
          elseif(IW.eq.15)then
            CALL EASKR(airg3,'Gap resistance (other)','confirm:',
     &        0.00,'W',1.00,'W',1.,'gap r other',IER,nbhelp)
            modify=.true.
          endif
          goto 3
        endif
      ELSEIF(IW.EQ.NELM-1)THEN
        helptopic='material_db_edit_one'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('material attributes',nbhelp,'-',0,0,IER)
      ELSE
        IW=-1
        GOTO 2
      ENDIF
      GOTO 3

      END

C ************* EDONECFC 
C EDONECFC: Edits a CFC layer in the common block at array index index.
C mod is returned as true if data has changed.
      SUBROUTINE EDONECFC(index,mod,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
#include "CFC_common.h"
      
      integer lnblnk  ! function definition

C Passed parameters.
      integer index  ! material array index
      logical mod    ! set to true if modified
      integer ier    ! non-zero if there is an issue

      DIMENSION PELM(30)
      CHARACTER PELM*40,tnam*32,tdoc*248,message*32
      real DBCON,DBDEN,DBSHT,THICK
      logical modify,dok,ok

C.....CFC shade or fill gas layer attributes
      real locCFCsolreflout
      real locCFCsolreflin
      real locCFCsoltrandir
      real locCFCsoltrantotout
      real locCFCsoltrantotin
      real locCFCemissout
      real locCFCemissin
      real locCFClwtran
      real locCFCdrpwidth
      real locCFCdrpspacing
      real locCFCwireemiss
      real locCFCwirediam
      real locCFCwirespace
      real locCFCslattran
      real locCFCslatwidth
      real locCFCslatspacing
      real locCFCslatangle
      character locCFCslatorient*4
      real locCFCslatcrown
      real locCFCslatwr
      real locCFCslatthk
      integer locCFCfillAir
      integer locCFCfillAr
      integer locCFCfillKr
      integer locCFCfillXe
      integer locCFCfillSF6
      character locCFC_IGDB_ID*8

C Local variables for gas materials.
      integer jsur   ! for radio button

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDONECFC'  ! set for subroutine

C If index is outwith range just return.
      if(index.gt.0.and.index.le.cfcdbitems)then
        mod=.false.
      else
        return
      endif

C Work with local variables.
      modify=.false.
      write(tnam,'(a)') cfcname(index)(1:lnblnk(cfcname(index)))
      write(tdoc,'(a)') cfcdoc(index)(1:lnblnk(cfcdoc(index)))
      DBCON = cfcdbcon(index)
      DBDEN = cfcdbden(index)
      DBSHT = cfcdbsht(index)
      THICK = cfcdbthick(index)

      locCFCsolreflout = CFCsolreflout(index)
      locCFCsolreflin = CFCsolreflin(index)
      locCFCsoltrandir = CFCsoltrandir(index)
      locCFCsoltrantotout = CFCsoltrantotout(index)
      locCFCsoltrantotin = CFCsoltrantotin(index)
      locCFCemissout = CFCemissout(index)
      locCFCemissin = CFCemissin(index)
      locCFClwtran = CFClwtran(index)
      locCFCdrpwidth = CFCdrpwidth(index)
      locCFCdrpspacing = CFCdrpspacing(index)
      locCFCwireemiss = CFCwireemiss(index)
      locCFCwirediam = CFCwirediam(index)
      locCFCwirespace = CFCwirespace(index)
      locCFCslattran = CFCslattran(index)
      locCFCslatwidth = CFCslatwidth(index)
      locCFCslatspacing = CFCslatspacing(index)
      locCFCslatangle = CFCslatangle(index)
      locCFCslatorient = CFCslatorient(index)
      locCFCslatcrown = CFCslatcrown(index)
      locCFCslatwr = CFCslatwr(index)
      locCFCslatthk = CFCslatthk(index)
      locCFCfillAir = CFCfillAir(index)
      locCFCfillAr = CFCfillAr(index)
      locCFCfillKr = CFCfillKr(index)
      locCFCfillXe = CFCfillXe(index)
      locCFCfillSF6 = CFCfillSF6(index)
      locCFC_IGDB_ID = CFC_IGDB_ID(index)

   3  IW=-4
      WRITE(PELM(1),'(A,1X,A)')    'a Name:',tnam(1:32)
      WRITE(PELM(2),'(A,1X,A)')    'b Note:',tdoc(1:32)
      WRITE(PELM(3),'(A,1X,F9.3)') 'c Conductivity (W/(m-K)  :',DBCON
      WRITE(PELM(4),'(A,1X,F8.2)') 'd Density (kg/m**3)      :',DBDEN
      WRITE(PELM(5),'(A,1X,F8.2)') 'e Specific Heat (J/(kg-K):',DBSHT
      WRITE(PELM(6),'(A,F9.2)')    'k Default thickness (mm) :',THICK


C.......Show shade layer attributes depending on type
      if(CFCshdtp(index).eq.iGlazing)then
            WRITE(PELM(7),'(A)')  'l type >>Glazing'
            WRITE(PELM(8),'(A)')
     &      'Glazing properties:'
            WRITE(PELM(9),'(A,F7.3)')
     &      'm Solar refl out (-)     :',locCFCsolreflout
            WRITE(PELM(10),'(A,F7.3)')
     &      'n Solar refl in (-)      :',locCFCsolreflin
            WRITE(PELM(11),'(A,F7.3)')
     &      'o Solar direct tran      :',locCFCsoltrandir
            WRITE(PELM(12),'(A,F7.3)')
     &      'p Emissivity out         :',locCFCemissout
            WRITE(PELM(13),'(A,F7.3)')
     &      'q Emissivity in          :',locCFCemissin
            WRITE(PELM(14),'(A,F7.3)')
     &      'r Longwave tran.         :',locCFClwtran
            WRITE(PELM(15),'(A,A8)')
     &      's IGDB ID                :',locCFC_IGDB_ID     
            m=15
      elseif(CFCshdtp(index).eq.iVenBlind)then
            WRITE(PELM(7),'(A)')  'l type >>Ven. blind'
            WRITE(PELM(8),'(A)')
     &      'Slat properties:'
            WRITE(PELM(9),'(A,F7.3)')
     &      'm Solar refl top         :',locCFCsolreflout
            WRITE(PELM(10),'(A,F7.3)')
     &      'n Solar refl bottom      :',locCFCsolreflin
            WRITE(PELM(11),'(A,F7.3)')
     &      'o Solar beam-diff tran   :',locCFCslattran
            WRITE(PELM(12),'(A,F7.3)')
     &      'p Emissivity top         :',locCFCemissout
            WRITE(PELM(13),'(A,F7.3)')
     &      'q Emissivity bottom      :',locCFCemissin
            WRITE(PELM(14),'(A,F7.3)')
     &      'r Longwave tran.         :',locCFClwtran
            WRITE(PELM(15),'(A,F7.3)')
     &      's Slat width (mm)        :',locCFCslatwidth
            WRITE(PELM(16),'(A,F7.3)')
     &      't Slat spacing (mm)      :',locCFCslatspacing
            WRITE(PELM(17),'(A,F7.3)')
     &      'u Slat angle (deg)       :',locCFCslatangle
            WRITE(PELM(18),'(A,A6)')
     &      'v Slat orient (HORZ/VERT):',locCFCslatorient
            WRITE(PELM(19),'(A,F7.3)')
     &      'w Slat crown (mm)        :',locCFCslatcrown
            WRITE(PELM(20),'(A,F7.3)')
     &      'x Slat width/rad. ratio  :',locCFCslatwr
            WRITE(PELM(21),'(A,F7.3)')
     &      'y Slat thickness (mm)    :',locCFCslatthk
            m=21
      elseif(CFCshdtp(index).eq.iPleatedDrape)then
            WRITE(PELM(7),'(A)')  'l type >>Pleated drape'
            WRITE(PELM(8),'(A)')
     &      'Fabric properties:'
            WRITE(PELM(9),'(A,F7.3)')
     &      'm Solar refl out (-)     :',locCFCsolreflout
            WRITE(PELM(10),'(A,F7.3)')
     &      'n Solar refl in (-)      :',locCFCsolreflin
            WRITE(PELM(11),'(A,F7.3)')
     &      'o Solar direct tran      :',locCFCsoltrandir
            WRITE(PELM(12),'(A,F7.3)')
     &      'p Sol. tot. tran out     :',locCFCsoltrantotout
            WRITE(PELM(13),'(A,F7.3)')
     &      'q Sol. tot. tran in      :',locCFCsoltrantotin
            WRITE(PELM(14),'(A)')
     &      'Pleat geometry:'
            WRITE(PELM(15),'(A,F7.3)')
     &      'u Width (mm)             :',locCFCdrpwidth
            WRITE(PELM(16),'(A,F7.3)')
     &      'v Spacing (mm)           :',locCFCdrpspacing
            m=16
      elseif(CFCshdtp(index).eq.iRollerBlind)then
            WRITE(PELM(7),'(A)')  'l type >>Roller blind'
            WRITE(PELM(8),'(A)')
     &      'Fabric properties:'
            WRITE(PELM(9),'(A,F7.3)')
     &      'm Solar refl out (-)     :',locCFCsolreflout
            WRITE(PELM(10),'(A,F7.3)')
     &      'n Solar refl in (-)      :',locCFCsolreflin
            WRITE(PELM(11),'(A,F7.3)')
     &      'o Solar direct tran      :',locCFCsoltrandir
            WRITE(PELM(12),'(A,F7.3)')
     &      'p Sol. tot. tran out     :',locCFCsoltrantotout
            WRITE(PELM(13),'(A,F7.3)')
     &      'q Sol. tot. tran in      :',locCFCsoltrantotin
            m=13
      elseif(CFCshdtp(index).eq.iInsectScreen)then
            WRITE(PELM(7),'(A)')  'l type >>Insect screen'
            WRITE(PELM(8),'(A)')
     &      'Mesh properties:'
            WRITE(PELM(9),'(A,F7.3)')
     &      'm Solar refl out (-)     :',locCFCsolreflout
            WRITE(PELM(10),'(A,F7.3)')
     &      'n Solar refl in (-)      :',locCFCsolreflin
            WRITE(PELM(11),'(A,F7.3)')
     &      'o Solar direct tran      :',locCFCsoltrandir
            WRITE(PELM(12),'(A,F7.3)')
     &      'p Sol. tot. tran out     :',locCFCsoltrantotout
            WRITE(PELM(13),'(A,F7.3)')
     &      'q Sol. tot. tran in      :',locCFCsoltrantotin
            WRITE(PELM(14),'(A,F7.3)')
     &      'r Wire emissivity        :',locCFCwireemiss
            WRITE(PELM(15),'(A,F7.3)')
     &      's Wire diameter (mm)     :',locCFCwirediam
            WRITE(PELM(16),'(A,F7.3)')
     &      't Wire Spacing (mm)      :',locCFCwirespace
            m=16
      elseif(CFCshdtp(index).eq.iGasGap)then
            WRITE(PELM(7),'(A)')  'l type >>fill gas'
            WRITE(PELM(8),'(A)')
     &      'Fill gas mixture (% mole fraction):'
            WRITE(PELM(9),'(A,I4)')
     &      'm Air (%)                :',locCFCfillAir
            WRITE(PELM(10),'(A,I4)')
     &      'n Argon (%)              :',locCFCfillAr
            WRITE(PELM(11),'(A,I4)')
     &      'o Krypton (%)            :',locCFCfillKr
            WRITE(PELM(12),'(A,I4)')
     &      'p Xenon (%)              :',locCFCfillXe
            WRITE(PELM(13),'(A,I4)')
     &      'q SF6-Sulfur hexafluoride (%) :',locCFCfillSF6
            WRITE(PELM(14),'(A,I4)')
     &      'Total (%)                :',
     &      locCFCfillAir + locCFCfillAr + 
     &      locCFCfillKr + locCFCfillXe +
     &      locCFCfillSF6
            m=14
      endif
      PELM(m+1) =' _____________________ '
      PELM(m+2)='? Help                 '
      PELM(m+3)='- Exit                 '
      NELM=m+3   ! number of items to display

C Help text for this menu.
   2  continue
      helptopic='cfc_layer_db_edit_one'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('CFC layer details',PELM,NELM,IW)
      IF(IW.EQ.0.OR.IW.EQ.NELM)THEN
C.......If CFC fill gas layer composition does not 
C.......sum to 100%, alert user and goto 3
        if(CFCshdtp(index).eq.iGasGap.and.
     &    ( locCFCfillAir + locCFCfillAr + 
     &      locCFCfillKr + locCFCfillXe +
     &      locCFCfillSF6).ne.100 )then
            CALL USRMSG(
     &        'Fill gas mixture does not total 100 %.',
     &        'Change gas composition and try again.', '-')
          goto 3
        endif
C If there has been a modification check with user prior to
C updating the common blocks.
        if(modify)then
          dok=.true.
          CALL ASKOK(' ','Accept changes in CFC layer (see help)?',
     &      OK,dok,nbhelp)
          if(OK)then
            write(cfcname(index),'(a)') tnam(1:lnblnk(tnam))
            write(cfcdoc(index),'(a)') tdoc(1:lnblnk(tdoc))
            cfcdbcon(index) = DBCON
            cfcdbden(index) = DBDEN
            cfcdbsht(index) = DBSHT
            cfcdbthick(index) = THICK

            CFCsolreflout(index) = locCFCsolreflout
            CFCsolreflin(index) = locCFCsolreflin
            CFCsoltrandir(index) = locCFCsoltrandir
            CFCsoltrantotout(index) = locCFCsoltrantotout
            CFCsoltrantotin(index) = locCFCsoltrantotin
            CFCemissout(index) = locCFCemissout
            CFCemissin(index) = locCFCemissin
            CFClwtran(index) = locCFClwtran
            CFCdrpwidth(index) = locCFCdrpwidth
            CFCdrpspacing(index) = locCFCdrpspacing
            CFCwireemiss(index) = locCFCwireemiss
            CFCwirediam(index) = locCFCwirediam
            CFCwirespace(index) = locCFCwirespace
            CFCslattran(index) = locCFCslattran
            CFCslatwidth(index) = locCFCslatwidth
            CFCslatspacing(index) = locCFCslatspacing
            CFCslatangle(index) = locCFCslatangle
            CFCslatorient(index) = locCFCslatorient
            CFCslatcrown(index)  = locCFCslatcrown
            CFCslatwr(index) = locCFCslatwr
            CFCslatthk(index) = locCFCslatthk
            CFCfillAir(index) = locCFCfillAir
            CFCfillAr(index) = locCFCfillAr
            CFCfillKr(index) = locCFCfillKr
            CFCfillXe(index)  = locCFCfillXe
            CFCfillSF6(index) = locCFCfillSF6

            modify=.false.
            mod=.true.   ! pass this back to calling code.
          endif
        endif
        RETURN
      ELSEIF(IW.EQ.NELM-2)THEN
        GOTO 3
      ELSEIF(IW.EQ.1)THEN
        CALL EASKS(tnam,'Description of CFC layer','confirm:',
     &    32,' ','CFC layer name',IER,nbhelp)
        if(tnam(1:2).ne.'  ')then
          modify=.true.
        endif
      ELSEIF(IW.EQ.2)THEN
        CALL EASKS248(tdoc,'Documentation for CFC layer','confirm:',
     &    72,' ','CFC layer documentation',IER,nbhelp)
        if(tdoc(1:2).ne.'  ')then
          modify=.true.
        endif
      ELSEIF(IW.EQ.3)THEN
        CALL EASKR(DBCON,'Conductivity (W/(m deg.C)','confirm:',
     &    0.001,'W',300.,'W',1.,'conductivity',IER,nbhelp)
        modify=.true.
      ELSEIF(IW.EQ.4)THEN
        CALL EASKR(DBDEN,'Density (kg/m**3)','confirm:',
     &    1.0,'W',9000.,'W',100.,'density',IER,nbhelp)
        modify=.true.
      ELSEIF(IW.EQ.5)THEN
        CALL EASKR(DBSHT,'Specific heat (J/(kg deg.C)','confirm:',
     &    1.0,'W',2000.,'W',100.,'specific heat',IER,nbhelp)
        modify=.true.
      ELSEIF(IW.EQ.6)THEN
        CALL EASKR(THICK,'Default thickness (mm)','confirm:',
     &    1.0,'W',500.,'W',10.,'default thickness',IER,nbhelp)
        modify=.true.
      ELSEIF(IW.EQ.7)THEN

C Offer to toggle between different types.
        helptopic='material_db_edit_type'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(CFCshdtp(index).eq.iGasGap)then
          message='(currently fill gas)'
        elseif(CFCshdtp(index).eq.iVenBlind)then
          message='(currently Venetian blind)'
        elseif(CFCshdtp(index).eq.iPleatedDrape)then
          message='(currently pleated drape)'
        elseif(CFCshdtp(index).eq.iRollerBlind)then
          message='(currently roller blind)'
        elseif(CFCshdtp(index).eq.iInsectScreen)then
          message='(currently insect screen)'
        elseif(CFCshdtp(index).eq.iGlazing)then
          message='(currently glazing)'
        endif
        jsur=1
        CALL EASKATOG('CFC layer options:',message,
     &    'glazing','Ven. blind','plt. drape',
     &    'roll. blind ','bug screen', 'fill gas', 
     &    'leave un-changed',jsur,nbhelp)
        if(jsur.eq.1)then
          CFCshdtp(index) = iGlazing
          modify=.true.
        elseif(jsur.eq.2)then
          CFCshdtp(index) = iVenBlind
          modify=.true.
        elseif(jsur.eq.3)then
          CFCshdtp(index) = iPleatedDrape
          modify=.true.
        elseif(jsur.eq.4)then
          CFCshdtp(index) = iRollerBlind
          modify=.true.
        elseif(jsur.eq.5)then
          CFCshdtp(index) = iInsectScreen
          modify=.true.
        elseif(jsur.eq.6)then
          CFCshdtp(index) = iGasGap
          modify=.true.
        else
          continue
        endif
        goto 3   ! need to re-establish how many items in menu

      ELSEIF(IW.gt.8.and.IW.LE.NELM-3)THEN

C.......Edit shade layer attributes depending on type
        IF(CFCshdtp(index).eq.iGlazing)THEN
           if(IW.eq.9)then
            CALL EASKR(locCFCsolreflout,
     &        'Solar refl, outside','confirm:',
     &        0.00,'W',1.0,'W',1.,'sol refl out',IER,nbhelp)
            modify=.true.
           elseif(IW.eq.10)then
            CALL EASKR(locCFCsolreflin,'Solar refl, inside',
     &        'confirm:',0.00,'W',1.0,'W',.5,'sol refl in',IER,
     &         nbhelp)
            modify=.true.
           elseif(IW.eq.11)then
            CALL EASKR(locCFCsoltrandir,
     &        'Solar tran direct',
     &        'confirm:',0.00,'W',1.00,'W',0.,'sol tran dir',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.12)then
            CALL EASKR(locCFCemissout,
     &        'Emissivity, outside',
     &        'confirm:',0.00,'W',1.00,'W',0.,'lw emiss (out)',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.13)then
            CALL EASKR(locCFCemissin,
     &        'Emissivity, inside','confirm:',
     &        0.00,'W',1.00,'W',0.,'lw emiss (in)',IER,nbhelp)
            modify=.true.
           elseif(IW.eq.14)then
            CALL EASKR(locCFClwtran,
     &        'Longwave transmittance',
     &        'confirm:',0.00,'W',1.00,'W',0.,'lw trans (out)',
     &        IER,nbhelp)
            modify=.true. 
           elseif(IW.eq.15)then
            CALL EASKS(locCFC_IGDB_ID,'IGDB ID','Confirm:',
     &        8,' ','IGDB ID',IER,nbhelp)
            modify=.true.             
           endif
           goto 3         
        ELSEIF(CFCshdtp(index).eq.iVenBlind)THEN
           if(IW.eq.9)then
            CALL EASKR(locCFCsolreflout,
     &        'Slat solar refl (top)','confirm:',
     &        0.00,'W',1.0,'W',.5,'sol refl top',IER,nbhelp)
            modify=.true.
           elseif(IW.eq.10)then
            CALL EASKR(locCFCsolreflin,'Slat solar refl (bottom)',
     &        'confirm:',0.00,'W',1.0,'W',.5,'sol refl bottom',IER,
     &         nbhelp)
            modify=.true.
           elseif(IW.eq.11)then
            CALL EASKR(locCFCslattran,
     &        'Slat beam-diff trans',
     &        'confirm:',0.00,'W',1.00,'W',.5,'slat bm-diff tran',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.12)then
            CALL EASKR(locCFCemissout,
     &        'Slat emissivity (top)',
     &        'confirm:',0.00,'W',1.00,'W',.5,'slat emiss top',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.13)then
            CALL EASKR(locCFCemissin,
     &        'Slat emissivity (bottom)','confirm:',
     &        0.00,'W',1.00,'W',.5,'slat emiss bot',IER,nbhelp)
            modify=.true.
           elseif(IW.eq.14)then
            CALL EASKR(locCFClwtran,
     &        'Slat longwave tran',
     &        'confirm:',0.00,'W',1.00,'W',.5,'slat lw tran',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.15)then
            CALL EASKR(locCFCslatwidth,'Slat width (mm)',
     &        'confirm:',0.00,'W',300.00,'W',15.,'slat width',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.16)then
            CALL EASKR(locCFCslatspacing,'Slat spacing (mm)',
     &        'confirm:',0.00,'W',300.00,'W',15.,'slat spacing',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.17)then
            CALL EASKR(locCFCslatangle,'Slat angle (deg)',
     &        'confirm:',0.00,'W',89.00,'W',0.,'slat angle',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.18)then
C...........Select slat orientation
            CALL EASKATOG('CFC venetian blind: slat orientation:',' ',
     &      'Horizontal','Vertical',' ',' ',' ', ' ', ' ',
     &      jsur,nbhelp)
            if(jsur.eq.1)then
              locCFCslatorient = 'HORZ'
            elseif(jsur.eq.2)then
              locCFCslatorient = 'VERT'
            else
              continue
            endif
            modify=.true.
           elseif(IW.eq.19)then
            CALL EASKR(locCFCslatcrown,'Slat crown (mm)',
     &        'confirm:',0.00,'W',locCFCslatwidth/2.,
     &        'W',5.,'slat crown',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.20)then
            CALL EASKR(locCFCslatwr,'Slat width/rad ratio',
     &        'confirm:',0.00,'W',1.99,'W',1.2,'slat spacing',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.21)then
            CALL EASKR(locCFCslatthk,'Slat thickness (mm)',
     &        'confirm:',0.00,'W',300.00,'W',2.,'slat spacing',
     &        IER,nbhelp)
            modify=.true.
           endif
           goto 3
        ELSEIF(CFCshdtp(index).eq.iPleatedDrape)THEN
           if(IW.eq.9)then
            CALL EASKR(locCFCsolreflout,
     &        'Solar refl, outside','confirm:',
     &        0.00,'W',1.0,'W',1.,'sol refl out',IER,nbhelp)
            modify=.true.
           elseif(IW.eq.10)then
            CALL EASKR(locCFCsolreflin,'Solar refl, inside',
     &        'confirm:',0.00,'W',1.0,'W',.5,'sol refl in',IER,
     &         nbhelp)
            modify=.true.
           elseif(IW.eq.11)then
            CALL EASKR(locCFCsoltrandir,
     &        'Solar tran direct',
     &        'confirm:',0.00,'W',1.00,'W',0.,'sol tran dir',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.12)then
            CALL EASKR(locCFCsoltrantotout,
     &        'Solar tran total (out)',
     &        'confirm:',0.00,'W',1.00,'W',0.,'sol tran tot, outside',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.13)then
            CALL EASKR(locCFCsoltrantotin,
     &        'Solar tran total, inside','confirm:',
     &        0.00,'W',1.00,'W',0.,'sol tran tot (in)',IER,nbhelp)
            modify=.true.
           elseif(IW.eq.15)then
            CALL EASKR(locCFCdrpwidth,'Pleat width (mm)',
     &        'confirm:',0.00,'W',500.00,'W',50.,'pleat width',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.16)then
            CALL EASKR(locCFCdrpspacing,'Pleat spacing (mm)',
     &        'confirm:',0.00,'W',500.00,'W',50.,'pleat spacing',
     &        IER,nbhelp)
            modify=.true.
           endif
           goto 3 
        ELSEIF(CFCshdtp(index).eq.iRollerBlind)THEN
           if(IW.eq.9)then
            CALL EASKR(locCFCsolreflout,
     &        'Solar refl, outside','confirm:',
     &        0.00,'W',1.0,'W',1.,'sol refl out',IER,nbhelp)
            modify=.true.
           elseif(IW.eq.10)then
            CALL EASKR(locCFCsolreflin,'Solar refl, inside',
     &        'confirm:',0.00,'W',1.0,'W',.5,'sol refl in',IER,
     &         nbhelp)
            modify=.true.
           elseif(IW.eq.11)then
            CALL EASKR(locCFCsoltrandir,
     &        'Solar tran direct',
     &        'confirm:',0.00,'W',1.00,'W',0.,'sol tran dir',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.12)then
            CALL EASKR(locCFCsoltrantotout,
     &        'Solar tran total, outside',
     &        'confirm:',0.00,'W',1.00,'W',0.,'sol tran tot (out)',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.13)then
            CALL EASKR(locCFCsoltrantotin,
     &        'Solar tran total, inside','confirm:',
     &        0.00,'W',1.00,'W',0.,'sol tran tot (in)',IER,nbhelp)
            modify=.true.
           endif
           goto 3 
        ELSEIF(CFCshdtp(index).eq.iInsectScreen)THEN
           if(IW.eq.9)then
            CALL EASKR(locCFCsolreflout,
     &        'Solar refl, outside','confirm:',
     &        0.00,'W',1.0,'W',1.,'sol refl out',IER,nbhelp)
            modify=.true.
           elseif(IW.eq.10)then
            CALL EASKR(locCFCsolreflin,'Solar refl, inside',
     &        'confirm:',0.00,'W',1.0,'W',.5,'sol refl in',IER,
     &         nbhelp)
            modify=.true.
           elseif(IW.eq.11)then
            CALL EASKR(locCFCsoltrandir,
     &        'Solar tran direct',
     &        'confirm:',0.00,'W',1.00,'W',0.,'sol tran dir',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.12)then
            CALL EASKR(locCFCsoltrantotout,
     &        'Solar tran total, outside',
     &        'confirm:',0.00,'W',1.00,'W',0.,'sol tran tot (out)',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.13)then
            CALL EASKR(locCFCsoltrantotin,
     &        'Solar tran total, inside','confirm:',
     &        0.00,'W',1.00,'W',0.,'sol tran tot (in)',IER,nbhelp)
            modify=.true.
           elseif(IW.eq.14)then
            CALL EASKR(locCFCwireemiss,'Wire emissivity',
     &        'confirm:',0.00,'W',1.00,'W',1.,'wire emiss',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.15)then
            CALL EASKR(locCFCwirediam,'Wire diameter (mm)',
     &        'confirm:',0.00,'W',10.00,'W',1.,'wire diam',
     &        IER,nbhelp)
            modify=.true.
           elseif(IW.eq.16)then
            CALL EASKR(locCFCwirespace,'Wire spacing (mm)',
     &        'confirm:',0.00,'W',300.0,
     &        'W',5.,'wire spacing',
     &        IER,nbhelp)
            modify=.true.
           endif
           goto 3 
        ELSEIF(CFCshdtp(index).eq.iGasGap)THEN
           if(IW.eq.9)then
             CALL EASKI(locCFCfillAir,
     &        '% mole fraction, air',
     &        'confirm:',0,'W',100,'W',100,'% air',IER,nbhelp)
             modify=.true.
           elseif(IW.eq.10)then
             CALL EASKI(locCFCfillAr,
     &        '% mole fraction, argon',
     &        'confirm:',0,'W',100,'W',100,'% argon',IER,nbhelp)
             modify=.true.
           elseif(IW.eq.11)then
             CALL EASKI(locCFCfillKr,
     &        '% mole fraction, krypton',
     &        'confirm:',0,'W',100,'W',100,'% krypton',IER,nbhelp)
             modify=.true.
           elseif(IW.eq.12)then
             CALL EASKI(locCFCfillXe,
     &        '% mole fraction, xenon',
     &        'confirm:',0,'W',100,'W',100,'% xenon',IER,nbhelp)
             modify=.true.
           elseif(IW.eq.13)then
             CALL EASKI(locCFCfillSF6,
     &        '% mole fraction, SF6',
     &        'confirm:',0,'W',100,'W',100,'% SF6',IER,nbhelp)
             modify=.true.
           endif
           goto 3
        ENDIF
      ELSEIF(IW.EQ.NELM-1)THEN
        helptopic='cfc_layer_db_edit_one'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('cfc layer attributes',nbhelp,'-',0,0,IER)
      ELSE
        IW=-1
        GOTO 2
      ENDIF
      GOTO 3

      END


C ************* EDMLDB 
C EDMLDB Display data with a common constructions (MLC) V0 file.
C Information is available via common block MLC. 
C Overload MLC information on whether the construction is
C opaque or transparent and if the latter include the optical property
C id string.

      SUBROUTINE EDMLDB(ITRC,chgdb,IER,ISEL)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "epara.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C Passed parameters
      integer itrc    ! non-zero if trace active
      logical chgdb   ! if constructions altered
      integer ier     ! IER 0 OK IER 1 or 2 problem
      integer isel    ! returns zero is nothing edited otherwise the item

C MPN is max number of tranparent glazings per multilayer construction
C If this parameter is changed also change it in subroutine GVALUE
      PARAMETER (MPN=3)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      COMMON/GVALCAL/CLAMDA(MMLC)

      integer matarrayindex ! the indes within matdatarray

      LOGICAL OK,DOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,ISZERO,CFCDBOK
      logical moddb,layermatch,closemat1,closemat2
      logical unixok,modmlc
      DIMENSION IVAL(MGCL)  ! for selecting categories
      CHARACTER KEY*1,T12*12,T12OTH*12,T14*14,T72*72
      CHARACTER T32*32,T32OTH*32
      CHARACTER PROMP2*56,PROMP1*56
      CHARACTER*33 MLCITM(36)
      CHARACTER xfile*144,tg*1,delim*1,outs*124
      character LAYD*124
      character lworking*144,fs*1,message*66
      CHARACTER STGLP(7)*56
      DIMENSION IVALS(7)
      REAL TRNT(MPN),REF(MPN),OREF(MPN),OEMS(MPN)
      integer IW   ! for radio button
      integer NITMS,INO ! max items and current menu item
      integer icn1,ii  ! for current connection
      integer lnssmlc,lnt32 ! for string lengths

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDMLDB'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      IER=0
      ISEL=0  ! reset
      moddb = .false.
      chgdb=.false.
      matarrayindex=0

C If the common block MLC has not yet been filled the read in the
C construction database.
      IF(.NOT.MLDBOK)THEN
        CALL ERMLDB(ITRC,iuout,IER)
        IF(IER.eq.4)THEN
          CALL ERMLDB2(0,iuout,IER)
          if(IER.eq.0)then
            MLDBOK=.TRUE.
          endif
        ELSEIF(IER.EQ.1.or.IER.eq.2.or.IER.eq.3)THEN
          CALL USRMSG(' ','Unable to display constructions.','-')
          IER=2
          RETURN
        ELSE
          MLDBOK=.TRUE.
        ENDIF
      ENDIF

C Check the version of the materials database and exit if they are
C not available.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        CALL USRMSG(' ','Unable to access materials data.','-')
        IER=2
        RETURN
      endif

C Create a menu showing the available database items.  Allow user to
C select one and then list details of this construction based on info
C in the materials database.
C Setup for multi-page menu.
      MHEAD=0
      MCTL=7
      ILEN=NMLC
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
 3    IER=0
      ILEN=NMLC
      INO=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=0
      DO 20 IM=1,NMLC
        IF(IM.GE.IST.AND.(IM.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(M,KEY,IER)
          if(mlcver.eq.0)then
            WRITE(MLCITM(M),'(a,1x,3a)')KEY,mlcname(im)(1:12),' ',
     &        mlcoptical(im)(1:12)
          else
            WRITE(MLCITM(M),'(a,1x,3a)')KEY,mlcname(im)(1:15),' ',
     &        mlcoptical(im)(1:15)
          endif
        ENDIF
  20  CONTINUE

C Number of actual items displayed.
      NITMS=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        MLCITM(M+1)='  ______________  '
      ELSE
        WRITE(MLCITM(M+1),15)IPM,MPM 
   15   FORMAT   ('0 ---Page: ',I2,' of ',I2,' ---')
      ENDIF
      MLCITM(M+2)='# view g-value'
      MLCITM(M+3)='1 add/delete/copy/invert    '
      MLCITM(M+4)='! list contents    '
      if(ipathmul.eq.0.or.ipathmul.eq.1)then
        MLCITM(M+5)='> save data                 '
      else
        MLCITM(M+5)='> save common data          '
      endif
      MLCITM(M+6)='? help                      '
      MLCITM(M+7)='- exit this menu            '
      INO=-4

C Help text for this menu.
    2 continue
      helptopic='constr_db_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('Constuctions file',MLCITM,NITMS,INO)

      IF(INO.EQ.NITMS.or.INO.EQ.NITMS-2)THEN

C Check for changes or respond to user request to save.
        if(moddb.or.INO.EQ.NITMS-2)then
          CALL EASKABCD('Save recent changes to constructions?',' ',
     &      'legacy MLC database','V1 MLC database',
     &      'V1 MLC database w/ categories','cancel save',irpt,nbhelp)
          if(irpt.eq.1)then
            CALL EMKAMLD(iuout,IER)  ! write legacy version
            chgdb=.true.
            moddb=.false.
          elseif(irpt.eq.2)then
            mlcver=1
            CALL EMKAMLD2(iuout,IER) ! write V1 all to 1 category
            chgdb=.true.
            moddb=.false.
          elseif(irpt.eq.3)then
            mlcver=1; mlcdocs=2
            mlccats=12      ! standard categories
            mlcdbdoc(1)=
     & 'A V1 format constructions file imported from a legacy file and'
            mlcdbdoc(2)=
     & 'based on material.db4.a and CFClayers db CFClayers.db1.a'
            mlccatname(1)='walls'; lnmlccatname(1)=5
            mlccatmenu(1)='opaque facade constructions'
            mlccatdoc(1)=
     & 'opaque facades representing different ages & performance'
            mlccatname(2)='partitions'; lnmlccatname(2)=10
            mlccatmenu(2)='internal partitions'
            mlccatdoc(2)=
     & 'partitions between zones (some have matching mirror versions)'
            mlccatname(3)='doors '; lnmlccatname(3)=5
            mlccatmenu(3)='inside and outside doors '
            mlccatdoc(3)=
     & 'doors of different ages types and performance characteristics'
            mlccatname(4)='glazing'; lnmlccatname(4)=7
            mlccatmenu(4)='glazing (transparent construc) '
            mlccatdoc(4)=
     & 'transparent constructions (using TMC optical properties)'
            mlccatname(5)='frames'; lnmlccatname(5)=6
            mlccatmenu(5)='frames for doors & windows'
            mlccatdoc(5)=
     & 'frames of various types for use with windows and doors'
            mlccatname(6)='roofs'; lnmlccatname(6)=5
            mlccatmenu(6)='flat and sloped roofs'
            mlccatdoc(6)=
     & 'roof constructions various ages & performances & orientations'
            mlccatname(7)='ceil_floor'; lnmlccatname(7)=10
            mlccatmenu(7)='internal ceilings and floors'
            mlccatdoc(7)=
     & 'internal ceilings and floors (& matched mirror constructions)'
            mlccatname(8)='ground'; lnmlccatname(8)=6
            mlccatmenu(8)='ground floors & crawl-spaces'
            mlccatdoc(8)=
     & 'floors & earth - crawl spaces & ground contact floors & walls'
            mlccatname(9)='fittings'; lnmlccatname(9)=8
            mlccatmenu(9)='equipment cases furniture etc.'
            mlccatdoc(9)=
     & 'collection of cases for equipment and furnature or fittings'
            mlccatname(10)='legacy'; lnmlccatname(10)=6
            mlccatmenu(10)='legacy constructions & models'
            mlccatdoc(10)=
     & 'collection of legacy constructions for training & validation'
            mlccatname(11)='UK_code'; lnmlccatname(11)=7
            mlccatmenu(11)='constructions for UK compliance'
            mlccatdoc(11)=
     & 'collection of constructions taken from UK SBEM'
            mlccatname(12)='project'; lnmlccatname(12)=7
            mlccatmenu(12)='constructions unique for project'
            mlccatdoc(12)=
     & 'collection of constructions for this project'

C For each of the MLC items allow user to assign a category.
C Do bookkeepping as each category is selected.
            do loop=1,mlcdbitems
              write(message,'(3a)') 'Select category for ',
     &          mlcname(loop)(1:lnmlcname(loop)),' from list.'
              IX=1
              CALL EPICKS(IX,IVAL,message,' ',
     &          32,mlccats,mlccatmenu,'Available categories',IER,14)
              isel=ival(1)
              write(mlcincat(loop),'(a)')
     &          mlccatname(isel)(1:lnmlccatname(isel))
              mlccatindex(loop)= isel
              mlccatitems(isel)=mlccatitems(isel)+1  ! increment
            enddo
            CALL EMKAMLD2(iuout,IER) ! write it out
            chgdb=.true.
            moddb=.false.
          elseif(irpt.eq.4)then
            continue
          endif
        endif
        if(INO.EQ.NITMS) return
      ELSEIF(INO.EQ.NITMS-1)THEN

C List help text for the menu.
        helptopic='constr_db_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('construction db',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.NITMS-3)THEN

C List database.
        CALL EASKABCD(' Reporting to:',' ',
     &    'summary text feedback','detailed text feedback',
     &    'summary file','detailed file',irpt,nbhelp)
        if(irpt.eq.1.or.irpt.eq.2)then
          itu = iuout
        elseif(irpt.eq.3.or.irpt.eq.4)then
          itu = ixunit
          write(xfile,'(a)') 'construction_listing.txt'  ! initial file name
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','MLC db text',IER)

C If user canceled the listing the reset unit to iuout and loop back.
          if(ier.eq.-3)then
            itu = iuout
            goto 3
          endif
        endif

C Based on ipathmul generate text string.
        if(ipathmul.eq.0.or.ipathmul.eq.1)then
          call edisp(itu,'In the MLC database: ')
          lworking=LFMUL
        elseif(ipathmul.eq.2)then
          call edisp(itu,'In the standard MLC database: ')
          lndbp=lnblnk(standarddbpath)
          write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &      LFMUL(1:lnblnk(LFMUL))
        endif
        call edisp(itu,lworking)
        call edisp(itu,' ')
        do 45 IM=1,NMLC
          if(irpt.eq.1)then
            call etmldb(1,itu,IM,imerr)   ! brief description
          elseif(irpt.eq.2)then
            call etmldb(2,itu,IM,imerr)   ! verbose description
          elseif(irpt.eq.3)then
            call etmldb(1,itu,IM,imerr)   ! brief description
          elseif(irpt.eq.4)then
            call etmldb(2,itu,IM,imerr)   ! verbose description
          endif
   45   continue
        call edisp(itu, ' U value assumes for horizontal heat flow:
     &  external wall with Rso = 0.04m**2deg.C/W')
        call edisp(itu,
     &  '                  and Rsi = 0.13m**2deg.C/W')
        if(irpt.eq.3.or.irpt.eq.4)then

C Call ctlexp a 2nd time to close the file.
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','prim db',IER)
        endif
      ELSEIF(INO.EQ.NITMS-4)THEN

C Manage the list of constructions.
        IW=1
        call easkatog('Constructions options:',' ',
     &    'add (at end of list)','delete','copy','copy & invert',
     &    'continue',' ',' ',IW,nbhelp)
        if(IW.EQ.1)then

C Add a construction. Update this to account for OPAQ/TRAN/CFC.
          if(NMLC.ge.MMLC)then
            CALL USRMSG('Adding not allowed as the new item',
     &        'would exceed database space! Skipping request...','W')
            INO=-4
            GOTO 3
          endif
          moddb = .true.
          chgdb=.true.
          NMLC=NMLC+1

C Ask for name of the new construction. V0 only works with first 12 char
          mlcname(NMLC)='  '  ! initial attributes
          mlctype(NMLC)='OPAQ'
          mlcoptical(NMLC)='OPAQUE'
          mlcsymetric(NMLC)='SYMMETRIC   '
          matsymindex(NMLC)=0
          mlccatindex(NMLC)=1  ! assume 1st class NEEDS THOUGHT
          if(mlcver.eq.0)then
            T14='  '
            CALL EASKS(T14,'Name of construction (unique)','Confirm:',
     &        14,' ','Construction name (short)',IER,mbhelp)
            write(T12,'(a)') T14(1:12)
            call st2name(T12,mlcname(NMLC))
            lnmlcname(NMLC)=lnblnk(mlcname(NMLC))  ! remember length
          else
            write(T32,'(a)') mlcname(NMLC)
            CALL EASKS(T32,
     &        'Name of construction (<32 char and UNIQUE)?',
     &        'Confirm:',32,' ','Construction name',IER,nbhelp)
            write(mlcname(NMLC),'(a)') T32(1:32)
            lnmlcname(NMLC)=lnblnk(mlcname(NMLC))  ! remember length
          endif

          LAYERS(NMLC)=1
          IPR(NMLC,1)=1  ! find alternative to IPR 
          IPRMAT(NMLC,1)=1
          DTHK(NMLC,1)=0.10

C Bring up editing menu for new item. Ignore jump requests.
          CALL EPMENSV
          CALL EDWALL(ITRC,NMLC,moddb,jump,IER)
          CALL EPMENRC
          ILEN=NMLC
          IPACT=CREATE
          CALL EKPAGE(IPACT)
          if(jump.ne.0) call edisp(iuout,
     &      'Sorry, no jump after new construction.')
        elseif(IW.EQ.2)then

C Compact the list by moving all the items in common MLC up one.
          CALL EPMENSV
          call epkmlc(iwhich,'Delete which construction?',' ',ierr)
          CALL EPMENRC
          if(iwhich.eq.0) goto 2
          dok=.false.
          CALL ASKOK(' ',' Are you sure? ',OK,dok,1)
          IF(.NOT.OK)GOTO 2
          moddb = .true.
          chgdb=.true.
          DO 891 IVV=IWHICH,NMLC-1
            mlcname(IVV)=mlcname(IVV+1)  ! shift existing attributes
            lnmlcname(IVV)=lnmlcname(IVV+1)
            mlctype(IVV)=mlctype(IVV+1)
            mlcoptical(IVV)=mlcoptical(IVV+1)
            mlcsymetric(IVV)=mlcsymetric(IVV+1)
            matsymindex(IVV)=matsymindex(IVV+1)
            mlccatindex(IVV)=mlccatindex(IVV+1)  ! assume same class 
            LAYERS(IVV)=LAYERS(IVV+1)
            DO 892 ILL=1,LAYERS(IVV)
              DTHK(IVV,ILL)=DTHK(IVV+1,ILL)
              IPR(IVV,ILL)=IPR(IVV+1,ILL)
              IPRMAT(IVV,ILL)=IPRMAT(IVV+1,ILL)
              DRAIR(IVV,ILL,1)=DRAIR(IVV+1,ILL,1)
              DRAIR(IVV,ILL,2)=DRAIR(IVV+1,ILL,2)
              DRAIR(IVV,ILL,3)=DRAIR(IVV+1,ILL,3)
              LAYDESC(IVV,ILL)=LAYDESC(IVV+1,ILL)
  892       CONTINUE
  891     CONTINUE
          NMLC=NMLC-1
          ILEN=NMLC
          IPACT=CREATE
          CALL EKPAGE(IPACT)

C At this point update the construction file and then if there are
C zones in the model update the pointers to ssmlcindex.
          CALL EMKAMLD(iuout,IER)
          chgdb=.true.
          moddb=.false.

C Scan for matching MLC for surfaces.
          if(NCOMP.gt.0)then
            do 30 ICOMP=1,NCOMP
              call georead(IFIL+1,LGEOM(ICOMP),ICOMP,1,iuout,ier)

C Find the index of the MLC which matches each surface.
              DO 9994 I=1,NZSUR(icomp)
                icn1=izstocn(icomp,i)
                ssmlcindex(icn1)=0  ! assume no matching MLC          
                lnssmlc=lnblnk(SSMLCN(icn1))
                do 5 ii=1,nmlc
                  if(SSMLCN(icn1)(1:lnssmlc).eq.
     &               mlcname(ii)(1:lnmlcname(ii)))then
                    ssmlcindex(icn1)=ii   ! remember MLC index     
                  endif
  5             continue
 9994         continue
 30         continue
          endif

        ELSEIF(IW.EQ.3)THEN

C Copy an existing construction to a new one at end of list.

C << for mlcver=1 there is no need for ipr >>
          if(NMLC.ge.MMLC)then
            CALL USRMSG('Copy not allowed as the new item',
     &        'would exceed database space! Skipping request...','W')
            INO=-4
            GOTO 3
          endif
          CALL EPMENSV
          call epkmlc(iwhich,'Copy which construction?',' ',ierr)
          CALL EPMENRC
          if(iwhich.eq.0) goto 3
          moddb = .true.
          chgdb=.true.
          NMLC=NMLC+1
          mlcname(NMLC)=mlcname(IWHICH)  ! copy existing attributes
          lnmlcname(NMLC)=lnmlcname(IWHICH)
          mlctype(NMLC)=mlctype(IWHICH)
          mlcoptical(NMLC)=mlcoptical(IWHICH)
          matsymindex(NMLC)=0            ! reset to assume no reversed
          mlccatindex(NMLC)=mlccatindex(IWHICH)  ! assume same class 
          LAYERS(NMLC)=LAYERS(IWHICH)
          DO 792 ILL=1,LAYERS(NMLC)      ! copy layer attributes
            DTHK(NMLC,ILL)=DTHK(IWHICH,ILL)
            IPR(NMLC,ILL)=IPR(IWHICH,ILL)
            IPRMAT(NMLC,ILL)=IPRMAT(IWHICH,ILL)
            ITMCFCDB(NMLC,ILL)=ITMCFCDB(IWHICH,ILL)
            DRAIR(NMLC,ILL,1)=DRAIR(IWHICH,ILL,1)
            DRAIR(NMLC,ILL,2)=DRAIR(IWHICH,ILL,2)
            DRAIR(NMLC,ILL,3)=DRAIR(IWHICH,ILL,3)
            LAYDESC(NMLC,ILL)=LAYDESC(IWHICH,ILL)
  792     CONTINUE

C Re-check the symmetric link so SYMMETRIC or NONSYMMETRIC can be re-established.
C When editing the construction name use a slightly wider string buffer.
          call ismlcsymmetric(nmlc,layermatch)
          if(layermatch)then
            mlcsymetric(NMLC)='SYMMETRIC   '
          else
            mlcsymetric(NMLC)='NONSYMMETRIC'
          endif

          if(mlcver.eq.0)then
            write(T14,'(2a)') mlcname(NMLC)(1:12),'  '
            CALL EASKS(T14,
     &        'Name of new construction (<12 char and UNIQUE)',
     &        'Confirm:',14,' ','New construction name',IER,nbhelp)
            write(T12,'(a)') T14(1:12)
            call st2name(T12,mlcname(NMLC))
            lnmlcname(NMLC)=lnblnk(mlcname(NMLC))  ! remember length

          else
            write(T32,'(a)') mlcname(NMLC)
            CALL EASKS(T32,
     &        'Name of new construction (<32 char and UNIQUE)?',
     &        'Confirm:',32,' ','New construction name',IER,nbhelp)
            write(mlcname(NMLC),'(a)') T32(1:32)
            lnmlcname(NMLC)=lnblnk(mlcname(NMLC))  ! remember length
          endif

C Bring up editing menu for new item.
          CALL EPMENSV
          CALL EDWALL(ITRC,NMLC,moddb,jump,IER)
          CALL EPMENRC
          ILEN=NMLC
          IPACT=CREATE
          CALL EKPAGE(IPACT)
          if(jump.ne.0) call edisp(iuout,
     &      'Sorry, no jump after copy construction.')
        ELSEIF(IW.EQ.4)THEN

C Invert an existing construction. Ask user to select original and if
C there is room in the database copy data to a new item.
          if(NMLC.ge.MMLC)then
            CALL USRMSG('Copy & invert not allowed as the new item',
     &        'would exceed database space! Skipping request...','W')
            INO=-4
            GOTO 3
          endif
          CALL EPMENSV
          call epkmlc(iwhich,
     &      'Make inverted version of which construction?',' ',ierr)
          CALL EPMENRC
          if(iwhich.eq.0) goto 3
          moddb = .true.
          chgdb=.true.
          NMLC=NMLC+1
          mlcdbitems=NMLC
          mlcname(NMLC)=mlcname(IWHICH)  ! copy existing attributes
          lnmlcname(NMLC)=lnmlcname(IWHICH)
          mlctype(NMLC)=mlctype(IWHICH)
          mlcoptical(NMLC)=mlcoptical(IWHICH)
          mlccatindex(NMLC)=mlccatindex(IWHICH)  ! assume same class 

C Remember original construction name as T12OTH scratch for editing as T12
C and T32 for matver 1.
          WRITE(T12OTH,'(A)')mlcname(NMLC)(1:12)
          WRITE(T12,'(A)')mlcname(NMLC)(1:12)
          WRITE(T32,'(A)')mlcname(NMLC)
          WRITE(T32OTH,'(A)')mlcname(NMLC)

C If there is space attach an "_i" to the end of the name to save the
C user this task. Get user to confirm the name and then use st2name to
C remove any wild card charaters.
          if(mlcver.eq.0)then
            if(lnblnk(T12).le.8)then
              WRITE(T14,'(2A)') T12OTH(1:lnblnk(T12OTH)),'_inv  '
            elseif(lnblnk(T12).gt.9.and.lnblnk(T12).le.10)then
              WRITE(T14,'(2A)') T12OTH(1:lnblnk(T12OTH)),'_i  '
            endif
            write(outs,'(3a)') '(original name ',
     &        T12OTH(1:lnblnk(T12OTH)),')'
            CALL EASKS(T14,'Name of inverted construction (<12 char)?',
     &        outs,14,' ','Inverted construction name',IER,nbhelp)
            write(T12,'(a)') T14(1:12)
            call st2name(T12,mlcname(NMLC))
            lnmlcname(NMLC)=lnblnk(mlcname(NMLC))  ! remember length

C Update linked MLC including the name of the original MLC.
            write(mlcsymetric(NMLC),'(a)') T12OTH  ! point back to orig
            matsymindex(NMLC)=IWHICH
          else
            if(lnblnk(T32).le.28)then
              WRITE(T32,'(2A)') T32OTH(1:lnblnk(T32OTH)),'_inv  '
            elseif(lnblnk(T12).gt.29.and.lnblnk(T32).le.30)then
              WRITE(T32,'(2A)') T32OTH(1:lnblnk(T32OTH)),'_i  '
            endif
            write(outs,'(3a)') '(original name ',
     &        T32OTH(1:lnblnk(T32OTH)),')'
            CALL EASKS(T32,
     &        'Name of inverted construction (<32 char)?',
     &        outs,32,' ','Inverted construction name',IER,nbhelp)
            write(mlcname(NMLC),'(a)') T32(1:32)
            lnmlcname(NMLC)=lnblnk(mlcname(NMLC))  ! remember length

C Update linked MLC including the name of the original MLC.
            write(mlcsymetric(NMLC),'(a)') T32OTH  ! point back to orig
            matsymindex(NMLC)=IWHICH
          endif

C Update the original to point to the inverted item.
          write(mlcsymetric(IWHICH),'(a)') mlcname(NMLC)  ! point to inverted
          matsymindex(IWHICH)=NMLC

C Copy other data associated with the MLC.
          LAYERS(NMLC)=LAYERS(IWHICH)
          DO 793 ILL=1,LAYERS(NMLC)
            DTHK(NMLC,ILL)=DTHK(IWHICH,ILL)
            IPR(NMLC,ILL)=IPR(IWHICH,ILL)
            IPRMAT(NMLC,ILL)=IPRMAT(IWHICH,ILL)
            ITMCFCDB(NMLC,ILL)=ITMCFCDB(IWHICH,ILL)
            DRAIR(NMLC,ILL,1)=DRAIR(IWHICH,ILL,1)
            DRAIR(NMLC,ILL,2)=DRAIR(IWHICH,ILL,2)
            DRAIR(NMLC,ILL,3)=DRAIR(IWHICH,ILL,3)
            LAYDESC(NMLC,ILL)=LAYDESC(IWHICH,ILL)
  793     CONTINUE

C Loop is one less than half the number of layers. Copy each layer
C into temporary space and then write to opposite side.
          LOOP=INT(FLOAT(LAYERS(NMLC))/2.)
          DO 692 ILL=1,LOOP
            DT=DTHK(NMLC,ILL)
            IPRT=IPR(NMLC,ILL)
            IPRMT=IPRMAT(NMLC,ILL)
            ITCFCDB=ITMCFCDB(IWHICH,ILL)
            DRT1=DRAIR(NMLC,ILL,1)
            DRT2=DRAIR(NMLC,ILL,2)
            DRT3=DRAIR(NMLC,ILL,3)
            LAYD=LAYDESC(IWHICH,ILL)

C Opposite layer is IOP. Copy its data into ILL layer.
            IOP=LAYERS(NMLC)-ILL+1
            DTHK(NMLC,ILL)=DTHK(NMLC,IOP)
            IPR(NMLC,ILL)=IPR(NMLC,IOP)
            IPRMAT(NMLC,ILL)=IPRMAT(NMLC,IOP)
            ITMCFCDB(NMLC,ILL)=ITMCFCDB(NMLC,IOP)
            DRAIR(NMLC,ILL,1)=DRAIR(NMLC,IOP,1)
            DRAIR(NMLC,ILL,2)=DRAIR(NMLC,IOP,2)
            DRAIR(NMLC,ILL,3)=DRAIR(NMLC,IOP,3)
            LAYDESC(NMLC,ILL)=LAYDESC(NMLC,IOP)

C Finally copy temporary data into opposite layer.
            DTHK(NMLC,IOP)=DT
            IPR(NMLC,IOP)=IPRT
            IPRMAT(NMLC,IOP)=IPRMT
            ITMCFCDB(NMLC,IOP)=ITCFCDB
            DRAIR(NMLC,IOP,1)=DRT1
            DRAIR(NMLC,IOP,2)=DRT2
            DRAIR(NMLC,IOP,3)=DRT3
            LAYDESC(NMLC,IOP)=LAYD
  692     CONTINUE

C Bring up detailed editing menu.
          CALL EPMENSV
          CALL EDWALL(ITRC,NMLC,moddb,jump,IER)
          CALL EPMENRC
          ILEN=NMLC
          IPACT=CREATE
          CALL EKPAGE(IPACT)
          if(jump.ne.0) call edisp(iuout,
     &      'Sorry, no jump after copy invert construction.')
        ENDIF
      ELSEIF(INO.EQ.NITMS-6)THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(INO.EQ.NITMS-5)THEN

C Calculate g-value of transparent constructions
C First check if mlc is transparent. It does not yet count
C CFC and CFC2 as transparent for g-value calcs.
        helptopic='construction_db_g-value'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('G-value for tran construction',nbhelp,'-',0,0,IER)
        CALL EPMENSV
        call epkmlc(iwh,'g-value for which construction?',' ',ierr)
        CALL EPMENRC
        if(iwh.eq.0) goto 3
        moddb = .true.
        chgdb=.true.
        THCON=CLAMDA(IWH)
        IF(mlctype(IWH)(1:4).EQ.'OPAQ'.OR.
     &     mlctype(IWH)(1:4).EQ.'UNKN'.OR.
     &     mlctype(IWH)(1:4).EQ.'CFC '.OR.
     &     mlctype(IWH)(1:4).EQ.'CFC2')THEN
          CALL EDISP(IUOUT,' ')
          CALL EDISP(IUOUT,
     &    ' G-value only defined for [tmc] transparent constructions')
          CALL EDISP(IUOUT,' ')
        ELSE

C Get number of non-air layers of glass.

C << todo: make use of the single lay optics from the materials commons
C << to supply much of the information needed for the g-value calculation.
          IPN=0
          DO 501 ILAY=1,LAYERS(IWH)
            matarrayindex=IPRMAT(IWH,ILAY)   ! which legacy index
             if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &         matopaq(matarrayindex)(1:1).eq.'h'.or.
     &         matarrayindex.eq.0)then
              continue
            else
              IPN=IPN+1

C For each nonair layer ask for reflectance, tranmittance and emissivity
              INPICK=1
              PROMP1='Choose TM33:2006 values or manually define '
              WRITE(PROMP2,'(A,I2)')'glazing layer properties:',IPN
             STGLP(1)='trnt  ref   o-ref ems   o-ems description'
             STGLP(2)='0.789 0.072 0.072 0.837 0.837 clear glass'
*            STGLP(3)='0.678 0.091 0.108 0.837 0.170 low-e glass'
             STGLP(3)='0.678 0.108 0.091 0.170 0.837 low-e glass'
             STGLP(4)='0.460 0.053 0.053 0.837 0.837 absorbing glass'
             STGLP(5)='0.390 0.310 0.450 0.837 0.250 hi perf reflecting'
*            STGLP(5)='0.390 0.450 0.310 0.250 0.837 hi perf reflecting'
             STGLP(6)='manual / user defined'
              CALL EPICKS(INPICK,IVALS,PROMP1,PROMP2,
     &                56,6,STGLP,' ',IER,nbhelp)
              IF(IER.NE.0)RETURN
              IPKD=IVALS(1)
              IF(IPKD.GE.2.AND.IPKD.LE.5)THEN

C Copy the numbers section of STGLP array item.
                write(T72,'(a)') STGLP(IPKD)(1:29)
              ELSEIF(IPKD.EQ.6)THEN
                T72='  '
                CALL EASKS(T72,
     &          'enter trnt, reft, op-reft, ems and op-ems (see help)',
     &          ' ',72,' ','Optical data for g-value',IER,nbhelp)
              ELSE
                CALL EDISP(IUOUT,'No choice made ... returning')
                RETURN
              ENDIF
              K=0
              CALL EGETWR(T72,K,VAL,0.,1.,'W','trans',IER)
              TRNT(IPN)=VAL
              CALL EGETWR(T72,K,VAL,0.,1.,'W','reflect',IER)
              REF(IPN)=VAL
              CALL EGETWR(T72,K,VAL,0.,1.,'W','op-reflect',IER)
              OREF(IPN)=VAL

C Emissivity value not required.
              CALL EGETWR(T72,K,VAL,0.,1.,'W','emiss',IER)
              CALL EGETWR(T72,K,VAL,0.,1.,'W','op-emiss',IER)
              OEMS(IPN)=VAL
            endif 
 501      CONTINUE

C Call subroutine to calculate g-value and report 
          CALL GVALUE(IPN,TRNT,REF,OREF,OEMS,THCON,G)
          CALL EDISP (IUOUT,' ')
          CALL ECLOSE(0.0,G,0.001,ISZERO)
          IF(ISZERO)THEN
            CALL EDISP(IUOUT,'EDMLDB ... Error finding G-value')
          ELSE
            WRITE(OUTS,'(A,F4.2)')'G-value is ',G
            CALL EDISP (IUOUT,OUTS)
          ENDIF
        ENDIF
        CALL EDISP (IUOUT,' ')
        IPACT=CREATE
        CALL EKPAGE(IPACT)

      ELSEIF(INO.GT.MHEAD.AND.INO.LT.(NITMS-MCTL+1))THEN

C Edit block identified by KEYIND, remind user of details and then
C bring up an editing facility. User may have indicated that they
C would like to jump to a previous or subsequent construction.
C << recode later to avoid goto statement >>
        CALL KEYIND(NITMS,INO,IFOC,IO)
 99     CALL EDISP(iuout,' ')
        call etmldb(2,iuout,IFOC,imerr)
        CALL EPMENSV
        CALL EDWALL(ITRC,IFOC,moddb,jump,IER)
        CALL EPMENRC
        ISEL=IFOC  ! pass back what was edited
        chgdb=moddb
        if(jump.eq.0)then
          continue
        elseif(jump.eq.-1)then
          ifoc=ifoc-1
          goto 99
        elseif(jump.eq.1)then
          ifoc=ifoc+1
          goto 99
        endif
      else
        INO=-1
        GOTO 2
      ENDIF
      INO=-4
      GOTO 3

      END


C ************* EDMLDB2
C EDMLDB2: Controls display and editing of MLC (v2) commons.
C If ACTION = 'M' then include editing if ACTION = '-' only
C allow choice.
      SUBROUTINE EDMLDB2(chgdb,ACTION,isel,IER)
#include "epara.h"
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C Parameters
      logical chgdb          ! flagged true if a mod made during session
      character ACTION*1
      integer isel ! is the returned index in mlcdatarray
      integer ier     ! IER 0 OK IER 1 or 2 problem

      common/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      COMMON/GVALCAL/CLAMDA(MMLC)
      LOGICAL OK,DOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,ISZERO,CFCDBOK
      logical moddb,layermatch
      logical unixok,modmlc

C CLSDES (30 char) is an array for menu items listing each class.
C CLSSEL (36 char) is an array for menu items for the selected class.
      character CLSSEL*36,CLSDES*38
      DIMENSION CLSDES(36),CLSSEL(30),ICLSSEL(30)
      CHARACTER*66 MLCITM(37)
      character managearray*32  ! string array for material management
      dimension managearray(60)
      integer pointback(30) ! to point back to item in list
      integer iwhichmanage,iwhichmsel
      dimension iwhichmanage(60),iwhichmsel(60) ! for each management array point back
      integer iwhicharray   ! for each item in menu point back to array
      integer lsn,lfordoc   ! for detecting string lengths
      dimension iwhicharray(60)
      character lltmp*144
      CHARACTER KEY*1,outs*124
      CHARACTER T12*12,T12OTH*12,T14*14,T72*72
      CHARACTER T32*32,T32OTH*32,tdoc*248
      CHARACTER PROMP2*56,PROMP1*56
      CHARACTER xfile*144,tg*1,delim*1,t248*248
      character LAYD*124
      character lworking*144,fs*1,catn*32
      CHARACTER STGLP(7)*56
      DIMENSION IVALS(7)

C MPN is max number of tranparent glazings per multilayer construction
C If this parameter is changed also change it in subroutine EDMLDB
      PARAMETER(MPN=3)
      REAL TRNT(MPN),REF(MPN),OREF(MPN),OEMS(MPN)
      character heading*70
      logical mod   ! to signal whether an item has been altered.
      integer iwhich2  ! 2nd material array index
      integer listc    ! for looping through classes
      integer NCO,ICO,NITMS,INO,IW  ! max items and current menu item
      integer mclist  ! counter for materials found to be in this category


C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='ELISTMLC2'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Assume no changes to db amd user has not entered password.
      MODDB=.FALSE.
      chgdb=.false.
      t32 = ' '
      IAF=IFIL+1
      iwhich=1   ! initial construction selections
      iwhich2=1
      INO=0

C Check that the constructions db is not empty.
    4 IER=0
      IF(mlccats.LE.0)THEN
        CALL USRMSG(' Constructions db is empty.',' ','W')
        IER=1
        RETURN
      ENDIF
      call usrmsg('  ','  ','-')   ! clear editing box.

C Initialise material category menu size variables based on window size. 
      MHEAD=1
      MCTL=5
      ILEN=mlccats
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Gather the names of the various classes and present this list.
  40  ICO=-3
      ILEN=mlccats
      CLSDES(1)=  '  Description               No. Items'
      M=MHEAD
      DO 44 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          write(CLSSEL(L),'(A)') mlccatmenu(L)(1:32)
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(CLSDES(M),'(A1,1X,A,1X,I2)')KEY,mlccatmenu(L)(1:32),
     &      mlccatitems(L)
        ENDIF
   44 CONTINUE

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        CLSDES(M+1)='  ______________________________ '
      ELSE
        WRITE(CLSDES(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' ------ ')
      ENDIF
      if(ACTION.eq.'M'.or.ACTION.eq.'m')then
        CLSDES(M+2)='+ manage classifications      '
        CLSDES(M+3)='! list database entries       '
      elseif(ACTION.eq.'-')then
        CLSDES(M+2)='+ select MLC as UNKNOWN       '
        CLSDES(M+3)='! list database entries       '
        call edisp(iuout,' ')
        call edisp(iuout,'Select classification to view items')
        call edisp(iuout,'Select + to make MLC as UNKNOWN ')
      endif
      CLSDES(M+4)=  '? help                        '
      CLSDES(M+5)=  '- exit                        '

C Number of actual items displayed.
      NCO=M+MCTL
      ICO=-2

C Help text for this menu.
      helptopic='cat_list_constructions'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      CALL EMENU('Construction Classes',CLSDES,NCO,ICO)
      IF(ICO.EQ.NCO)THEN
        IF(MODDB.and.ACTION.eq.'M')THEN

C Changes were made so remove the existing file via delfiledosorunix
C and write a new file based on current common blocks. << TO DO >>
          dok=.true.
          CALL ASKOK('Changes have been made in this file. ',
     &      'Save the constructions changes?',OK,dok,nbhelp)
          IF(.NOT.OK) return
          chgdb=.true.   ! pass back signal to update
          CALL ERPFREE(IFMUL,ISTAT)  ! in case other unit is still open
          CALL ERPFREE(IAF,ISTAT)    ! in case unit is still open

C Check the value of whichdbpath variable to see what to do with
C this file. If local or absolute path then use lltmp directly. If in
C the standard location then create lworking which has path
C prepended.
          if(ipathmul.eq.0.or.ipathmul.eq.1)then
            lltmp=' '
            write(lltmp,'(2a)') LFMUL(1:lnblnk(LFMUL)),'-'
            call delfiledosorunix(lltmp,ider)
            CALL EMKAMLD2(iuout,IER) ! write it out
          elseif(ipathmul.eq.2)then
            lndbp=lnblnk(standarddbpath)
            write(lworking,'(4a)') standarddbpath(1:lndbp),fs,
     &          LFMUL(1:lnblnk(LFMUL)),'-'
C Debug.
C            write(6,*) 'working constructions ',lworking
            CALL EMKAMLD2(iuout,IER) ! write it out
          endif

          MODDB=.FALSE.
        ENDIF
        RETURN
      ELSEIF(ICO.EQ.(NCO-1))THEN
        helptopic='cat_list_constructions'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('constructions menu',nbhelp,'-',0,0,IER)
      ELSEIF(ICO.EQ.(NCO-2))THEN

C List one or more classifications.
        INPIC=mlccats
        CALL EPMENSV
        CALL EPICKS(INPIC,ICLSSEL,' ',' Which classes to list:',
     &    36,mlccats,CLSSEL,'Construction classes',IER,nbhelp)
        CALL EPMENRC
        IF(INPIC.EQ.0)GOTO 40
        CALL EASKABC(' Reporting to:',' ',
     &    'text feedback','summary file','cancel',irpt,nbhelp)
        if(irpt.eq.1)then
          itu = iuout
        elseif(irpt.eq.2)then
          itu = ixunit
          write(xfile,'(a)') 'construction_listing.txt'  ! initial file name
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','prim db text',IER)

C If user canceled the listing the reset unit to iuout and loop back.
          if(ier.eq.-3)then
            itu = iuout
            goto 40
          endif
        elseif(irpt.eq.3)then
          goto 40
        endif
        if(ipathmul.eq.0.or.ipathmul.eq.1)then
          call edisp(itu,'In the constructions database: ')
          call edisp(itu, LFMUL)
        elseif(ipathmat.eq.2)then
          call edisp(itu,'In the standard constructions database: ')
          call edisp(itu, LFMUL)
        endif
        call edisp(itu,' ')
        do 42 listc=1,INPIC
          IC=ICLSSEL(listc)
          IF(mlccatitems(IC).GT.0)THEN
            WRITE(outs,'(3a,i2,a)')' Classification: ',
     &        mlccatmenu(IC)(1:lnblnk(mlccatmenu(IC))),' (',IC,')'
            call edisp(itu,outs)
            call edisp248(itu,mlccatdoc(IC),100)
            call edisp(itu,' ')

C Loop through all of the items in the array and list out those that are
C associated with this class.
            matching=0
            catn=mlccatname(IC)
            lncatn=lnblnk(catn)
            do loop=1,mlcdbitems
              if(mlcincat(loop)(1:lncatn).eq.catn(1:lncatn))then
                matching=matching+1
                pointback(matching)=loop
                lfordoc = 72 - (lnmlcmenu(loop) +4)    ! space left for doc
                WRITE(outs,'(2x,3a)')
     &            mlcname(loop)(1:lnmlcmenu(loop)),' : ',
     &            mlcdoc(loop)(1:lfordoc)
                call edisp(itu,outs)
              endif
            enddo
          ELSE
            call edisp(itu,' No items in this classification.')
          ENDIF
   42   continue
        call edisp(itu,' ')
        if(irpt.eq.2)then

C Call ctlexp a 2nd time to close the file.
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','prim db',IER)
        endif
      ELSEIF(ICO.EQ.(NCO-3))THEN

C Manage categories. Select and alter the category name or documentation
C or add another category to the database << add not yet tested >>
        if(ACTION.eq.'M'.or.ACTION.eq.'m')then
          CALL EASKABCD(' Options:',' ',
     &      'edit category name','edit category documentation',
     &      'add another category','cancel',irpt,nbhelp)
          if(irpt.eq.1.or.irpt.eq.2)then
            CALL EPMENSV
            INPIC=1
            CALL EPICKS(INPIC,ICLSSEL,' ',' Which class:',
     &        36,matcats,CLSSEL,'Construction class to manage',
     &        IER,nbhelp)
            CALL EPMENRC
            IF(INPIC.EQ.0)GOTO 40
            IC=ICLSSEL(1)
          endif
          if(irpt.eq.1)then

C Edit classification name and menu.
            write(t32,'(a)')mlccatname(IC)(1:lnmlccatname(IC))
            CALL EASKS(t32,' ',' Classification name (<32 char)?',
     &        32,' ','class name',IER,nbhelp)
            if(t32(1:2).ne.'  ')then
              write(mlccatname(IC),'(a)') t32(1:lnblnk(t32))
              lnmlccatname(IC)=lnblnk(mlccatname(IC))
              write(t32,'(a)')mlccatname(IC)(1:lnmlccatname(IC))
              CALL EASKS(t32,' ',' Classification menu (<32 char)?',
     &          32,' ','class menu',IER,nbhelp)
              write(mlccatmenu(IC),'(a)') t32(1:lnblnk(t32))
              chgdb=.true.
              MODDB=.TRUE.
            endif
          elseif(irpt.eq.2)then

C Edit classification documentation.
            t248=mlccatdoc(IC)(1:lnblnk(mlccatdoc(IC)))
            CALL EASKS248(t248,'Category notes:',' ',
     &      72,'this category...','category notes',IER,nbhelp)
            if(t248(1:2).ne.'  ')then
              write(mlccatdoc(IC),'(a)') t248(1:lnblnk(t248))
              chgdb=.true.
              MODDB=.TRUE.
            endif
          elseif(irpt.eq.3)then

C Add a classification.
            if(mlccats.LT.30)then
              mlccats=mlccats+1
              t32=' '
              CALL EASKS(t32,' Classification name (<32 char)?',' ',
     &          32,' ','class name',IER,nbhelp)
              write(mlccatname(mlccats),'(a)') t32
              lnmlccatname(mlccats)=lnblnk(mlccatname(mlccats))
              write(t32,'(a)') mlccatname(mlccats)
              CALL EASKS(t32,' ',' Classification menu (<32 char)?',
     &          32,' ','class menu',IER,nbhelp)
              write(mlccatmenu(mlccats),'(a)') t32(1:lnblnk(t32))
              write(t248,'(a,i2,3a)')
     &          'Category (',mlccats,') named ',t32(1:lnblnk(t32)),
     &          ' was inserted manually. No other documentation (yet).'
              CALL EASKS248(t248,'Category notes:',' ',
     &          72,'this category...','category notes',IER,nbhelp)
              write(mlccatdoc(mlccats),'(a)') t248(1:lnblnk(t248))
        
              ILNE=mlcdbitems+1  ! new material for this classification
              t32=' '
              CALL EASKS(t32,'Name of initial construction','confirm:',
     &          32,' ','construction name',IER,nbhelp)
              write(mlcname(ILNE),'(a)') t32(1:lnblnk(t32))
              lnmlcname(ILNE)=lnblnk(mlcname(ILNE))  ! remember length
              write(mlcmenu(ILNE),'(a)') t32(1:lnblnk(t32))
              lnmlcmenu(ILNE)=lnblnk(mlcmenu(ILNE))
              write(mlcincat(ILNE),'(a)') mlccatname(mlccats)
              write(mlcdoc(ILNE),'(4a)') 'no documentation yet for ',
     &          mlcname(ILNE)(1:lnmlcname(ILNE)),' in',
     &          mlccatname(mlccats)(1:lnmlccatname(mlccats))
              write(mlctype(ILNE),'(a)') 'OPAQ'
              write(mlcoptical(ILNE),'(a)') 'OPAQUE'
              write(mlcsymetric(ILNE),'(a)') 'SYMMETRIC'
              matsymindex(ILNE)=0
              LAYERS(ILNE)=1
              DTHK(ILNE,1)=0.1
              IPR(ILNE,1)=1  ! find alternative to IPR 
              IPRMAT(ILNE,1)=1

C Make up LAYDESC as combination of matnam and matdoc.
              lnam=lnblnk(matname(1))
              lfordoc = 72 - (lnam +4)    ! space left for doc
              write(LAYDESC(ILNE,1),'(3a)') matname(1)(1:lnam),' : ',
     &          matdoc(1)(1:lfordoc)
              mlccatitems(mlccats)=mlccatitems(mlccats)+1 ! increment nb of cat items
              mlccatindex(ILNE)=mlccats  ! remember its category
              mlcdbitems=mlcdbitems+1    ! increment nb of db items
              MODDB=.TRUE.
            endif
            goto 40  ! to refresh the menu
          endif
        elseif(ACTION.eq.'-')then
          isel=0   ! this should signal UNKNOWN
          chgdb=.false.
          return
        endif
        goto 40

      ELSEIF(ICO.eq.(NCO-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 40    ! user paged so refresh

      ELSEIF(ICO.GT.MHEAD.AND.ICO.LT.(NCO-4))THEN
        
C This is the secondary menu focused on a single construction class.
C Loop through data for this classification for viewing and/or manipulation.
C Edit categpru identified by KEYIND.
        CALL KEYIND(NCO,ICO,IC,IO)
        IF(mlccatitems(IC).EQ.0)THEN
         CALL USRMSG(' No items in this classification.',' ','W')
         GOTO 40
        ENDIF

C Echo documentation for classification when changing focus.
        call edisp(iuout,' ')
        call edisp248(iuout,mlccatdoc(IC),100)
        call edisp(iuout,' ')

C See how wide if no truncation.
        iwid=0
        do loop=1,mlcdbitems
          lsn=lnmlcname(loop)
          lsm=lnmlcmenu(loop)
          lso=lnblnk(mlcoptical(loop))
          if((lsn+lsm+lso+5).gt.iwid) iwid=lsn+lsm+lso+5
        enddo  ! of loop

C Save the state of the initial menu prior to setting up the secondary
C menu layout. Ensure that each logical exit point restores initial menu.
        CALL EPMENSV
        MHEAD=1  ! reset for inner menu head
        MCTL=7   ! reset for inner menu control entries
        ILEN=mlccatitems(IC) ! reset for materials in this catetory 
        IPACT=CREATE
        CALL EKPAGE(IPACT)

C Generate menu strings for constructions in the class.
    3   write(MLCITM(1),'(2a)') ' Constructions in category ',
     &    mlccatname(IC)(1:lnmlccatname(IC))
        M=MHEAD
        ILEN=mlccatitems(IC) ! reset if constructions in this catetory changed
        mclist=0  ! counter for materials found to be in this category
        catn=mlccatname(IC)
        lncatn=lnblnk(catn)
        do loop=1,mlcdbitems
          if(mlcincat(loop)(1:lncatn).eq.catn(1:lncatn))then
            mclist=mclist+1
            write(managearray(mclist),'(a)') mlcmenu(loop)
            iwhichmanage(mclist)=loop  ! management array to data array loop
            pointback(mclist)=loop

C Test if the construction fits within the current paged menu and then
C remember the array index that goes with the menu position (m).
            if(mclist.GE.IST.AND.(mclist.LE.(IST+MIFULL)))then
              M=M+1
              iwhicharray(M)=loop  ! menu position M relates to data array loop
              CALL EMKEY(mclist,KEY,IER)
              if(iwid.lt.66)then
                lsn=lnmlcname(loop)
                lsm=lnmlcmenu(loop)
                lso=lnblnk(mlcoptical(loop))
                WRITE(MLCITM(M),'(a,1x,5a)')KEY,mlcname(loop)(1:lsn),
     &            '  ',mlcoptical(loop)(1:lso),' ',mlcmenu(loop)(1:lsm)          
              else
                lsn=MIN0(lnmlcname(loop),24)
                lsm=MIN0(lnmlcmenu(loop),24)
                lso=MIN0(lnblnk(mlcoptical(loop)),15)
                WRITE(MLCITM(M),'(a,1x,5a)')KEY,mlcname(loop)(1:lsn),
     &            '  ',mlcoptical(loop)(1:14),' ',mlcmenu(loop)(1:lsm)
              endif         
            endif
          endif
        enddo
        NC=M

C If a long list include page facility text.      
        IF(IPFLG.EQ.0)THEN  
          MLCITM(NC+1)='  ____________________________________ '
        ELSE
          WRITE(MLCITM(NC+1),15)IPM,MPM 
        ENDIF

        if(ACTION.eq.'M'.or.ACTION.eq.'m')then
          MLCITM(NC+2)='# view g-value'
          MLCITM(NC+3)='1 add/delete/copy/invert    '
          MLCITM(NC+4)='! list contents    '
          if(ipathmul.eq.0.or.ipathmul.eq.1)then
            MLCITM(NC+5)='> save data                 '
          else
            MLCITM(NC+5)='> save common data          '
          endif
          MLCITM(NC+6)='? help                      '
          MLCITM(NC+7)='- exit this menu            '
        elseif(ACTION.eq.'-')then
          MLCITM(NC+2)='# view g-value'
          MLCITM(NC+3)='                                 '
          MLCITM(NC+4)='! list contents    '
          MLCITM(NC+5)='                                 '
          MLCITM(NC+6)='? help                      '
          MLCITM(NC+7)='- exit this menu            '
        endif
        NITMS=NC+7
        INO=-4

C Help text for this menu.
    2   continue
        helptopic='constr_db_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)

        write(heading,'(3a,i2,a,i2,a)') 'Constructions in ',
     &    mlccatname(IC)(1:lnmlccatname(IC)),' (',IC,') with ',
     &    mlccatitems(IC),' entries.'
 
        CALL EMENU(heading,MLCITM,NITMS,INO)

        IF(INO.EQ.NITMS.OR.INO.EQ.0)THEN
          IF(MODDB.and.ACTION.eq.'M')THEN

C Changes were made so remove the existing file via delfiledosorunix
C and write a new file based on current common blocks.
            dok=.true.
            CALL ASKOK('Changes have been made in this file. ',
     &        'Save the constructions changes?',OK,dok,nbhelp)
            if(.NOT.OK)then
              CALL EPMENRC   ! recover initial menu state
              GOTO 4         ! jump back to rebuild initial menu
            endif
            chgdb=.true.   ! pass back signal to update
            CALL ERPFREE(IFMUL,ISTAT)  ! in case other unit is still open

C Check the value of whichdbpath variable to see what to do with
C this file. If local or absolute path then use lltmp directly. If in
C the standard location then create lworking which has path
C prepended.
            if(ipathmat.eq.0.or.ipathmat.eq.1)then
              lltmp=' '
              write(lltmp,'(2a)') LFMUL(1:lnblnk(LFMUL)),'-'
              call delfiledosorunix(lltmp,ider)
              CALL EMKAMLD2(iuout,IER) ! write it out
            elseif(ipathmat.eq.2)then
              lndbp=lnblnk(standarddbpath)
              write(lworking,'(4a)') standarddbpath(1:lndbp),fs,
     &          LFMUL(1:lnblnk(LFMUL)),'-'
              CALL EMKAMLD2(iuout,IER) ! write it out
            endif

            chgdb=.true.
            MODDB=.FALSE.
          ENDIF
          CALL EPMENRC   ! recover initial menu state
          GOTO 4         ! go back and rebuild initial menu
        ELSEIF(INO.EQ.1.AND.INO.LE.4)THEN
          GOTO 4         ! not a useful selection redo inner menu
        ELSEIF(INO.EQ.NITMS-1)THEN
          helptopic='constr_db_overview'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('construction database',nbhelp,'-',0,0,IER)
        ELSEIF(INO.EQ.NITMS-2)THEN

C Save constructions db with all entities. First delete the
C existing file via delfiledosorunix and then write
C a new file via XXXX.
          if(.NOT.MODDB)then
            dok=.true.
            CALL ASKOK('No changes in the constructions file. ',
     &        'Do you want to update it anyway:',OK,dok,nbhelp)
            if(.NOT.OK)then
              goto 3  ! jump back to inner menu without doing anything
            endif
          endif
          chgdb=.true.
          call erpfree(ifmul,istat)  ! in case it is still open

          lltmp=' '
          if(ipathmul.eq.0.or.ipathmul.eq.1)then
            write(lltmp,'(2a)') LFMUL(1:lnblnk(LFMUL)),'-'
            call delfiledosorunix(lltmp,ider)
            CALL EMKAMLD2(iuout,IER) ! write it out
          elseif(ipathmat.eq.2)then
            lndbp=lnblnk(standarddbpath)
            write(lworking,'(4a)') standarddbpath(1:lndbp),fs,
     &        LFMUL(1:lnblnk(LFMUL)),'-'
            CALL EMKAMLD2(iuout,IER) ! write it out
          endif

C Reset flag to show db is current with local arrays.
          MODDB=.FALSE.
          CALL USRMSG(' ','Changes saved...','-')

        ELSEIF(INO.EQ.NITMS-3)THEN

C List database.
          CALL EASKABCD(' Reporting to:',' ',
     &      'summary text feedback','detailed text feedback',
     &      'summary file','detailed file',irpt,nbhelp)
          if(irpt.eq.1.or.irpt.eq.2)then
            itu = iuout
          elseif(irpt.eq.3.or.irpt.eq.4)then
            itu = ixunit
            write(xfile,'(a)') 'construction_listing.txt'  ! initial file name
            call ctlexp(xfile,ixopen,ixloc,ixunit,'T','MLC db text',IER)

C If user canceled the listing the reset unit to iuout and loop back.
            if(ier.eq.-3)then
              itu = iuout
              goto 3
            endif
          endif

C Based on ipathmul generate text string.
          if(ipathmul.eq.0.or.ipathmul.eq.1)then
            call edisp(itu,'In the MLC database: ')
            write(lworking,'(2a)') LFMUL(1:lnblnk(LFMUL)),'-'
          elseif(ipathmul.eq.2)then
            call edisp(itu,'In the standard MLC database: ')
            lndbp=lnblnk(standarddbpath)
            write(lworking,'(4a)') standarddbpath(1:lndbp),fs,
     &        LFMUL(1:lnblnk(LFMUL)),'-'
          endif
          call edisp(itu,lworking)

          call edisp(itu,' ')
          do IM=1,NMLC
            if(irpt.eq.1)then
              call etmldb(1,itu,IM,imerr)   ! brief description
            elseif(irpt.eq.2)then
              call etmldb(2,itu,IM,imerr)   ! verbose description
            elseif(irpt.eq.3)then
              call etmldb(1,itu,IM,imerr)   ! brief description
            elseif(irpt.eq.4)then
              call etmldb(2,itu,IM,imerr)   ! verbose description
            endif
          enddo  ! of IM
          call edisp(itu, ' U value assumes for horizontal heat flow:
     &    external wall with Rso = 0.04m**2deg.C/W')
          call edisp(itu,
     &    '                  and Rsi = 0.13m**2deg.C/W')
          if(irpt.eq.3.or.irpt.eq.4)then

C Call ctlexp a 2nd time to close the file.
            call ctlexp(xfile,ixopen,ixloc,ixunit,'T','prim db',IER)
          endif

        ELSEIF(INO.EQ.NITMS-4)THEN

C Manage the list of constructions.
          IW=1
          call easkatog('Constructions options:',' ',
     &      'add (at end of list)','delete','copy','copy & invert',
     &      'continue',' ',' ',IW,nbhelp)
          if(IW.EQ.1)then

C Add a construction. Update this to account for OPAQ/TRAN/CFC.
            if(NMLC.ge.MMLC)then
              CALL USRMSG('Adding not allowed as the new item',
     &        'would exceed database space! Skipping request...','W')
              INO=-4
              GOTO 3
            endif
            moddb = .true.
            chgdb=.true.
            NMLC=NMLC+1
            mlcdbitems=NMLC

C Ask for name of the new construction.
            mlcname(NMLC)='  '  ! initial attributes
            mlcmenu(NMLC)='not yet defined'  ! initial attributes
            mlcdoc(NMLC) ='not yet defined'  ! initial
            mlcincat(NMLC)= mlccatname(IC)
            mlctype(NMLC)='OPAQ'
            mlcoptical(NMLC)='OPAQUE'
            mlcsymetric(NMLC)='SYMMETRIC   '
            matsymindex(NMLC)=0
            mlccatindex(NMLC)=IC  ! assume the current classification
            mlccatitems(IC)=mlccatitems(IC)+1 ! increment
            if(mlcver.eq.0)then
              T14='  '
              CALL EASKS(T14,'Name of construction (unique)','Confirm:',
     &        14,' ','Construction name',IER,mbhelp)
              write(T12,'(a)') T14(1:12)
              call st2name(T12,mlcname(NMLC))
              lnmlcname(NMLC)=lnblnk(mlcname(NMLC))  ! remember length
              write(mlcmenu(NMLC),'(a)') t12(1:lnblnk(t12))
              lnmlcmenu(NMLC)=lnblnk(mlcmenu(NMLC))  ! remember length
              write(mlcincat(NMLC),'(a)') mlccatname(IC)
            else
              t32=' '
              CALL EASKS(t32,'Name of initial construction','Confirm:',
     &          32,' ','construction name',IER,nbhelp)
              write(mlcname(NMLC),'(a)') t32(1:lnblnk(t32))
              lnmlcname(NMLC)=lnblnk(mlcname(NMLC))  ! remember length
              write(mlcmenu(NMLC),'(a)') t32(1:lnblnk(t32))
              lnmlcmenu(NMLC)=lnblnk(mlcmenu(NMLC))  ! remember length
              write(mlcincat(NMLC),'(a)') mlccatname(IC)
              write(mlcdoc(NMLC),'(4a)') 'no documentation yet for ',
     &          mlcname(NMLC)(1:lnmlcname(NMLC)),' in',
     &          mlccatname(IC)(1:lnmlccatname(IC))
            endif

            write(mlctype(NMLC),'(a)') 'OPAQ'
            write(mlcoptical(NMLC),'(a)') 'OPAQUE'
            write(mlcsymetric(NMLC),'(a)') 'SYMMETRIC'
            matsymindex(NMLC)=0
            LAYERS(NMLC)=1
            IPR(NMLC,1)=1  ! find alternative to IPR 
            IPRMAT(NMLC,1)=1
            DTHK(NMLC,1)=0.10

C Make up LAYDESC as combination of matnam and matdoc.
            lnam=lnblnk(matname(1))
            lfordoc = 72 - (lnam +4)    ! space left for doc
            write(LAYDESC(NMLC,1),'(3a)') matname(1)(1:lnam),' : ',
     &        matdoc(1)(1:lfordoc)

C Bring up editing menu for new item. Ignore jump requests.
            CALL EPMENSV
            CALL EDWALL(ITRC,NMLC,moddb,jump,IER)
            CALL EPMENRC
            ILEN=NMLC
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            if(jump.ne.0) call edisp(iuout,
     &        'Sorry, no jump after new construction.')

C In this case the category stays the same so just jump back.
            goto 3
          elseif(IW.EQ.2)then

C Compact the list by moving all the items in common MLC up one.
C We have to select via epkmlc so as not to be recursive.
            CALL EPMENSV
            if(mlcver.eq.0)then
              call epkmlc(iwhich,'Delete which construction?',' ',ierr)
            else
              call epkmlc(iwhich,'Delete which construction?',
     &          '(via this global list)',ierr)
            endif
            CALL EPMENRC
            if(iwhich.eq.0) goto 2
            dok=.false.
            CALL ASKOK(' ',' Are you sure? ',OK,dok,1)
            IF(.NOT.OK) GOTO 2
            moddb = .true.
            chgdb=.true.

C Find its category and decrement its counter
            lncatn=lnblnk(mlcincat(iwhich))
            do loop=1,mlccats
              if(mlcincat(iwhich)(1:lncatn).eq.
     &           mlccatname(loop)(1:lnmlccatname(loop)))then
                mlccatitems(loop)=mlccatitems(loop)-1
                IC=loop  ! reset the focus category to one we have removed
                write(6,*) 'One less item for ',mlccatname(loop),' ',
     &            loop,mlccatitems(loop)
              endif
            enddo

            DO IVV=IWHICH,NMLC-1
              mlcname(IVV)=mlcname(IVV+1)  ! shift existing attributes
              lnmlcname(IVV)=lnmlcname(IVV+1)
              mlcmenu(IVV)=mlcmenu(IVV+1)
              lnmlcmenu(IVV)=lnmlcmenu(IVV+1)
              mlctype(IVV)=mlctype(IVV+1)
              mlcdoc(IVV)=mlcdoc(IVV+1)
              mlcincat(IVV)=mlcincat(IVV+1)
              mlcoptical(IVV)=mlcoptical(IVV+1)
              mlcsymetric(IVV)=mlcsymetric(IVV+1)
              matsymindex(IVV)=matsymindex(IVV+1)
              mlccatindex(IVV)=mlccatindex(IVV+1)  ! assume same class 
              LAYERS(IVV)=LAYERS(IVV+1)
              DO ILL=1,LAYERS(IVV)
                DTHK(IVV,ILL)=DTHK(IVV+1,ILL)
                IPR(IVV,ILL)=IPR(IVV+1,ILL)
                IPRMAT(IVV,ILL)=IPRMAT(IVV+1,ILL)
                DRAIR(IVV,ILL,1)=DRAIR(IVV+1,ILL,1)
                DRAIR(IVV,ILL,2)=DRAIR(IVV+1,ILL,2)
                DRAIR(IVV,ILL,3)=DRAIR(IVV+1,ILL,3)
                LAYDESC(IVV,ILL)=LAYDESC(IVV+1,ILL)
              ENDDO
            ENDDO
            NMLC=NMLC-1
            mlcdbitems=NMLC
            ILEN=NMLC
            IPACT=CREATE
            CALL EKPAGE(IPACT)

C At this point update the construction file and then if there are
C zones in the model update the pointers to ssmlcindex.
            CALL EMKAMLD(iuout,IER)
            chgdb=.true.
            moddb=.false.

C Scan for matching MLC for surfaces.
            if(NCOMP.gt.0)then
              do ICOMP=1,NCOMP
                call georead(IFIL+1,LGEOM(ICOMP),ICOMP,1,iuout,ier)

C Find the index of the MLC which matches each surface.
                DO I=1,NZSUR(icomp)
                  icn1=izstocn(icomp,i)
                  ssmlcindex(icn1)=0  ! assume no matching MLC          
                  lnssmlc=lnblnk(SSMLCN(icn1))
                  do ii=1,nmlc
                    if(SSMLCN(icn1)(1:lnssmlc).eq.
     &                 mlcname(ii)(1:lnmlcname(ii)))then
                      ssmlcindex(icn1)=ii   ! remember MLC index     
                    endif
                  enddo ! of II
                enddo   ! of I
              enddo     ! of icomp
            endif
            goto 3

          ELSEIF(IW.EQ.3)THEN

C Copy an existing construction to a new one at end of list.
            if(NMLC.ge.MMLC)then
              CALL USRMSG('Copy not allowed as the new item',
     &        'would exceed database space! Skipping request...','W')
              INO=-4
              GOTO 3
            endif
            CALL EPMENSV
            if(mlcver.eq.0)then
              call epkmlc(iwhich,'Copy which construction?',' ',ierr)
            else
              call epkmlc(iwhich,'Copy which construction?',
     &          '(via this global list)',ierr)
            endif
            CALL EPMENRC
            if(iwhich.eq.0) goto 3
            moddb = .true.
            chgdb=.true.
            NMLC=NMLC+1
            mlcdbitems=NMLC
            mlcname(NMLC)=mlcname(IWHICH)  ! copy existing attributes
            lnmlcname(NMLC)=lnmlcname(IWHICH)
            mlcmenu(NMLC)=mlcmenu(IWHICH)
            lnmlcmenu(NMLC)=lnmlcmenu(IWHICH)
            mlctype(NMLC)=mlctype(IWHICH)
            mlcdoc(NMLC)=mlcdoc(IWHICH)
            mlcincat(NMLC)=mlcincat(IWHICH)
            mlcoptical(NMLC)=mlcoptical(IWHICH)
            matsymindex(NMLC)=0            ! reset to assume no reversed
            mlccatindex(NMLC)=mlccatindex(IWHICH)  ! assume same class 
            LAYERS(NMLC)=LAYERS(IWHICH)

C Find its category and increment its counter/
            lncatn=lnblnk(mlcincat(NMLC))
            do loop=1,mlccats
              if(mlcincat(NMLC)(1:lncatn).eq.
     &           mlccatname(loop)(1:lnmlccatname(loop)))then
                mlccatitems(loop)=mlccatitems(loop)+1
                IC=loop  ! reset the focus category to one we have copied
                write(6,*) 'Another item for ',mlccatname(loop),' ',
     &            mlccatitems(loop),' focus now ',IC
              endif
            enddo

            DO ILL=1,LAYERS(NMLC)      ! copy layer attributes
              DTHK(NMLC,ILL)=DTHK(IWHICH,ILL)
              IPR(NMLC,ILL)=IPR(IWHICH,ILL)
              IPRMAT(NMLC,ILL)=IPRMAT(IWHICH,ILL)
              ITMCFCDB(NMLC,ILL)=ITMCFCDB(IWHICH,ILL)
              DRAIR(NMLC,ILL,1)=DRAIR(IWHICH,ILL,1)
              DRAIR(NMLC,ILL,2)=DRAIR(IWHICH,ILL,2)
              DRAIR(NMLC,ILL,3)=DRAIR(IWHICH,ILL,3)
              LAYDESC(NMLC,ILL)=LAYDESC(IWHICH,ILL)
            ENDDO  ! of ILL

C Re-check the symmetric link so SYMMETRIC or NONSYMMETRIC can be re-established.
C When editing the construction name use a slightly wider string buffer.
            call ismlcsymmetric(nmlc,layermatch)
            if(layermatch)then
              mlcsymetric(NMLC)='SYMMETRIC   '
            else
              mlcsymetric(NMLC)='NONSYMMETRIC'
            endif
            if(mlcver.eq.0)then
              write(T14,'(2a)') mlcname(NMLC)(1:12),'  '
              CALL EASKS(T14,
     &        'Name of new construction (<12 char and UNIQUE)',
     &        'Confirm:',14,' ','New construction name',IER,nbhelp)
              write(T12,'(a)') T14(1:12)
              call st2name(T12,mlcname(NMLC))
              lnmlcname(NMLC)=lnblnk(mlcname(NMLC))  ! remember length
            else
              write(T32,'(a)') mlcname(NMLC)
              CALL EASKS(T32,
     &        'Name of new construction (<32 char and UNIQUE)?',
     &        'Confirm:',32,' ','New construction name',IER,nbhelp)
              write(mlcname(NMLC),'(a)') T32(1:32)
              lnmlcname(NMLC)=lnblnk(mlcname(NMLC))  ! remember length
              write(mlcmenu(NMLC),'(a)') T32(1:32)
              lnmlcmenu(NMLC)=lnblnk(mlcmenu(NMLC))  ! remember length
              write(mlcdoc(NMLC),'(2a)') 'no documentation yet for ',
     &          mlcname(NMLC)(1:lnmlcname(NMLC))
            endif

C Find its category and increment its counter/
            lncatn=lnblnk(mlcincat(NMLC))
            do loop=1,mlccats
              if(mlcincat(NMLC)(1:lncatn).eq.
     &           mlccatname(loop)(1:lnmlccatname(loop)))then
                mlccatitems(loop)=mlccatitems(loop)+1
                write(6,*) 'Another item for ',mlccatname(loop),' ',
     &            loop,mlccatitems(loop)
              endif
            enddo

C Bring up editing menu for new item.
            CALL EPMENSV
            CALL EDWALL(ITRC,NMLC,moddb,jump,IER)
            CALL EPMENRC
            ILEN=NMLC
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            if(jump.ne.0) call edisp(iuout,
     &        'Sorry, no jump after copy construction.')
            goto 3

          ELSEIF(IW.EQ.4)THEN

C Invert an existing construction. Ask user to select original and if
C there is room in the database copy data to a new item.
            if(NMLC.ge.MMLC)then
              CALL USRMSG('Copy & invert not allowed as the new item',
     &          'would exceed database space! Skipping request...','W')
              INO=-4
              GOTO 3
            endif
            CALL EPMENSV
            if(mlcver.eq.0)then
              call epkmlc(iwhich,
     &        'Make inverted version of which construction?',' ',ierr)
            else
              call epkmlc(iwhich,
     &          'Make inverted version of which construction?',
     &          '(via this global list)',ierr)
            endif
            CALL EPMENRC
            if(iwhich.eq.0) goto 3
            moddb = .true.
            chgdb=.true.
            NMLC=NMLC+1
            mlcdbitems=NMLC
            mlcname(NMLC)=mlcname(IWHICH)  ! copy existing attributes
            lnmlcname(NMLC)=lnmlcname(IWHICH)
            mlcmenu(NMLC)=mlcmenu(IWHICH)
            lnmlcmenu(NMLC)=lnmlcmenu(IWHICH)
            mlcdoc(NMLC)=mlcdoc(IWHICH)
            mlcincat(NMLC)=mlcincat(IWHICH)
            mlctype(NMLC)=mlctype(IWHICH)
            mlcoptical(NMLC)=mlcoptical(IWHICH)
            mlccatindex(NMLC)=mlccatindex(IWHICH)  ! assume same class 

C Remember original construction name as T12OTH or T32OTH scratch for editing as T12
C and T32 for matver 1.
            WRITE(T12OTH,'(A)')mlcname(NMLC)(1:12)
            WRITE(T12,'(A)')mlcname(NMLC)(1:12)
            WRITE(T32,'(A)')mlcname(NMLC)
            WRITE(T32OTH,'(A)')mlcname(NMLC)

C If there is space attach an "_i" to the end of the name to save the
C user this task. Get user to confirm the name and then use st2name to
C remove any wild card charaters.
            if(mlcver.eq.0)then
              if(lnblnk(T12).le.8)then
                WRITE(T14,'(2A)') T12OTH(1:lnblnk(T12OTH)),'_inv  '
              elseif(lnblnk(T12).gt.9.and.lnblnk(T12).le.10)then
                WRITE(T14,'(2A)') T12OTH(1:lnblnk(T12OTH)),'_i  '
              endif
              write(outs,'(3a)') '(original name ',
     &          T12OTH(1:lnblnk(T12OTH)),')'
              CALL EASKS(T14,
     &          'Name of inverted construction (<12 char)?',
     &          outs,14,' ','Inverted construction name',IER,nbhelp)
              write(T12,'(a)') T14(1:12)
              call st2name(T12,mlcname(NMLC))
              lnmlcname(NMLC)=lnblnk(mlcname(NMLC))  ! remember length

C Update linked MLC including the name of the original MLC.
              write(mlcsymetric(NMLC),'(a)') T12OTH  ! point back to orig
              matsymindex(NMLC)=IWHICH
            else
              if(lnblnk(T32).le.28)then
                WRITE(T32,'(2A)') T32OTH(1:lnblnk(T32OTH)),'_inv  '
              elseif(lnblnk(T12).gt.29.and.lnblnk(T32).le.30)then
                WRITE(T32,'(2A)') T32OTH(1:lnblnk(T32OTH)),'_i  '
              endif
              write(outs,'(3a)') '(original name ',
     &          T32OTH(1:lnblnk(T32OTH)),')'
              CALL EASKS(T32,
     &          'Name of inverted construction (<32 char)?',
     &          outs,32,' ','Inverted construction name',IER,nbhelp)
              write(mlcname(NMLC),'(a)') T32(1:lnblnk(T32))
              lnmlcname(NMLC)=lnblnk(mlcname(NMLC))  ! remember length
              write(T32,'(a)') mlcmenu(NMLC)
              CALL EASKS(T32,' ','Menu entry?',
     &          32,' ','Construction menu',IER,nbhelp)
              write(mlcmenu(NMLC),'(a)') T32(1:lnblnk(T32))
              lnmlcmenu(NMLC)=lnblnk(mlcmenu(NMLC))  ! remember length
              write(tdoc,'(a)') mlcdoc(NMLC)(1:lnblnk(mlcdoc(NMLC)))
              CALL EASKS248(tdoc,'Documentation for MLC','confirm:',
     &          72,' ','MLC documentation',IER,nbhelp)
              write(mlcdoc(NMLC),'(a)') tdoc(1:lnblnk(tdoc))
     
C Update linked MLC including the name of the original MLC.
              write(mlcsymetric(NMLC),'(a)') T32OTH  ! point back to orig
              matsymindex(NMLC)=IWHICH
            endif

C Update the original to point to the inverted item.
            write(mlcsymetric(IWHICH),'(a)') mlcname(NMLC)  ! point to inverted
            matsymindex(IWHICH)=NMLC

C Copy other data associated with the MLC.
            LAYERS(NMLC)=LAYERS(IWHICH)
            DO ILL=1,LAYERS(NMLC)
              DTHK(NMLC,ILL)=DTHK(IWHICH,ILL)
              IPR(NMLC,ILL)=IPR(IWHICH,ILL)
              IPRMAT(NMLC,ILL)=IPRMAT(IWHICH,ILL)
              ITMCFCDB(NMLC,ILL)=ITMCFCDB(IWHICH,ILL)
              DRAIR(NMLC,ILL,1)=DRAIR(IWHICH,ILL,1)
              DRAIR(NMLC,ILL,2)=DRAIR(IWHICH,ILL,2)
              DRAIR(NMLC,ILL,3)=DRAIR(IWHICH,ILL,3)
              LAYDESC(NMLC,ILL)=LAYDESC(IWHICH,ILL)
            ENDDO  ! of ILL

C Loop is one less than half the number of layers. Copy each layer
C into temporary space and then write to opposite side.
            LOOP=INT(FLOAT(LAYERS(NMLC))/2.)
            DO ILL=1,LOOP
              DT=DTHK(NMLC,ILL)
              IPRT=IPR(NMLC,ILL)
              IPRMT=IPRMAT(NMLC,ILL)
              ITCFCDB=ITMCFCDB(IWHICH,ILL)
              DRT1=DRAIR(NMLC,ILL,1)
              DRT2=DRAIR(NMLC,ILL,2)
              DRT3=DRAIR(NMLC,ILL,3)
              LAYD=LAYDESC(IWHICH,ILL)

C Opposite layer is IOP. Copy its data into ILL layer.
              IOP=LAYERS(NMLC)-ILL+1
              DTHK(NMLC,ILL)=DTHK(NMLC,IOP)
              IPR(NMLC,ILL)=IPR(NMLC,IOP)
              IPRMAT(NMLC,ILL)=IPRMAT(NMLC,IOP)
              ITMCFCDB(NMLC,ILL)=ITMCFCDB(NMLC,IOP)
              DRAIR(NMLC,ILL,1)=DRAIR(NMLC,IOP,1)
              DRAIR(NMLC,ILL,2)=DRAIR(NMLC,IOP,2)
              DRAIR(NMLC,ILL,3)=DRAIR(NMLC,IOP,3)
              LAYDESC(NMLC,ILL)=LAYDESC(NMLC,IOP)

C Finally copy temporary data into opposite layer.
              DTHK(NMLC,IOP)=DT
              IPR(NMLC,IOP)=IPRT
              IPRMAT(NMLC,IOP)=IPRMT
              ITMCFCDB(NMLC,IOP)=ITCFCDB
              DRAIR(NMLC,IOP,1)=DRT1
              DRAIR(NMLC,IOP,2)=DRT2
              DRAIR(NMLC,IOP,3)=DRT3
              LAYDESC(NMLC,IOP)=LAYD
            ENDDO  ! of ILL

C Bring up detailed editing menu.
            CALL EPMENSV
            CALL EDWALL(ITRC,NMLC,moddb,jump,IER)
            CALL EPMENRC
            ILEN=NMLC
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            if(jump.ne.0) call edisp(iuout,
     &        'Sorry, no jump after copy invert construction.')
            goto 3
          ENDIF

        ELSEIF(INO.EQ.NITMS-6)THEN

C If there are enough items allow paging control via EKPAGE.
          IF(IPFLG.EQ.1)THEN
            IPACT=EDIT
            CALL EKPAGE(IPACT)
          ENDIF
          GOTO 3    ! user paged so refresh inner menu
  
        ELSEIF(INO.EQ.NITMS-5)THEN

C Calculate g-value of transparent constructions
C First check if mlc is transparent. It does not yet count
C CFC and CFC2 as transparent for g-value calcs.
          helptopic='construction_db_g-value'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('G-value for tran construction',nbhelp,'-',
     &      0,0,IER)
          CALL EPMENSV
          if(mlcver.eq.0)then
            call epkmlc(iwh,'g-value for which construction?',' ',ierr)
          else
            call epkmlc(iwh,'g-value for which construction?',
     &        '(via this global list)',ierr)
          endif
          CALL EPMENRC
          if(iwh.eq.0) goto 3
          moddb = .true.
          chgdb=.true.
          THCON=CLAMDA(IWH)
          IF(mlctype(IWH)(1:4).EQ.'OPAQ'.OR.
     &       mlctype(IWH)(1:4).EQ.'UNKN'.OR.
     &       mlctype(IWH)(1:4).EQ.'CFC '.OR.
     &       mlctype(IWH)(1:4).EQ.'CFC2')THEN
            CALL EDISP(IUOUT,' ')
            CALL EDISP(IUOUT,
     &      ' G-value only defined for [tmc] transparent constructions')
            CALL EDISP(IUOUT,' ')
          ELSE

C Get number of non-air layers of glass.

C << todo: make use of the single lay optics from the materials commons
C << to supply much of the information needed for the g-value calculation.
            IPN=0
            DO 501 ILAY=1,LAYERS(IWH)
              matarrayindex=IPRMAT(IWH,ILAY)   ! which legacy index
              if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &           matopaq(matarrayindex)(1:1).eq.'h'.or.
     &           matarrayindex.eq.0)then
                continue
              else
                IPN=IPN+1

C For each nonair layer ask for reflectance, tranmittance and emissivity
                INPICK=1
                PROMP1='Choose TM33:2006 values or manually define '
                WRITE(PROMP2,'(A,I2)')'glazing layer properties:',IPN
             STGLP(1)='trnt  ref   o-ref ems   o-ems description'
             STGLP(2)='0.789 0.072 0.072 0.837 0.837 clear glass'
             STGLP(3)='0.678 0.091 0.108 0.837 0.170 low-e glass'
*            STGLP(3)='0.678 0.108 0.091 0.170 0.837 low-e glass'
             STGLP(4)='0.460 0.053 0.053 0.837 0.837 absorbing glass'
             STGLP(5)='0.390 0.310 0.450 0.837 0.250 hi perf reflecting'
*            STGLP(5)='0.390 0.450 0.310 0.250 0.837 hi perf reflecting'
             STGLP(6)='manual / user defined'
                CALL EPICKS(INPICK,IVALS,PROMP1,PROMP2,
     &                56,6,STGLP,' ',IER,nbhelp)
                IF(IER.NE.0) RETURN
                IPKD=IVALS(1)
                IF(IPKD.GE.2.AND.IPKD.LE.5)THEN

C Copy the numbers section of STGLP array item.
                  write(T72,'(a)') STGLP(IPKD)(1:29)
                ELSEIF(IPKD.EQ.6)THEN
                  T72='  '
                  CALL EASKS(T72,
     &            'enter trnt, reft, op-reft, ems & op-ems (see help)',
     &            ' ',72,' ','Optical data for g-value',IER,nbhelp)
                ELSE
                  CALL EDISP(IUOUT,'No choice made ... returning')
                  RETURN
                ENDIF
                K=0
                CALL EGETWR(T72,K,VAL,0.,1.,'W','trans',IER)
                TRNT(IPN)=VAL
                CALL EGETWR(T72,K,VAL,0.,1.,'W','reflect',IER)
                REF(IPN)=VAL
                CALL EGETWR(T72,K,VAL,0.,1.,'W','op-reflect',IER)
                OREF(IPN)=VAL

C Emissivity value not required.
                CALL EGETWR(T72,K,VAL,0.,1.,'W','emiss',IER)
                CALL EGETWR(T72,K,VAL,0.,1.,'W','op-emiss',IER)
                OEMS(IPN)=VAL
              endif 
 501        CONTINUE

C Call subroutine to calculate g-value and report 
            CALL GVALUE(IPN,TRNT,REF,OREF,OEMS,THCON,G)
            CALL EDISP (IUOUT,' ')
            CALL ECLOSE(0.0,G,0.001,ISZERO)
            IF(ISZERO)THEN
              CALL EDISP(IUOUT,'EDMLDB ... Error finding G-value')
            ELSE
              WRITE(OUTS,'(A,F4.2)')'G-value is ',G
              CALL EDISP (IUOUT,OUTS)
            ENDIF
          ENDIF
          CALL EDISP (IUOUT,' ')
          IPACT=CREATE
          CALL EKPAGE(IPACT)

        ELSEIF(INO.GT.MHEAD.AND.INO.LT.(NITMS-MCTL+1))THEN

C Identified one of the constructions to edit or select.
          IFOC=iwhicharray(INO)

C Debug.
C          write(6,*) 'selected menu ',ino,'got array',ifoc

          if(ACTION.eq.'M')then

C Browse/Edit the details of this item. Mark as unmodified first
C and then resetn MODDB and chgdb if mod is true.
 99         CALL EDISP(iuout,' ')
            call etmldb(2,iuout,IFOC,imerr)
            CALL EPMENSV
            CALL EDWALL(ITRC,IFOC,moddb,jump,IER)
            CALL EPMENRC
            ISEL=IFOC  ! pass back what was edited
            chgdb=moddb
            if(jump.eq.0)then
              continue
            elseif(jump.eq.-1)then
C V1 find prior in category
              if(INO.GT.MHEAD)then
                IFOC=iwhicharray(INO-1)
C               ifoc=ifoc-1
                goto 99
              else
                continue
              endif
            elseif(jump.eq.1)then

C V1 find next in category
              if(INO.LT.(NITMS-MCTL+1))then
                IFOC=iwhicharray(INO+1)
C               ifoc=ifoc+1
                goto 99
              else
                continue
              endif
            endif
          elseif(ACTION.eq.'-')then
            call edisp(iuout,' ')
            call etmldb(2,iuout,IFOC,imerr)
            dok=.true.
            CALL ASKOK(outs,'Use it:',OK,dok,1)
            if(OK)then
              ISEL=IFOC ! set to selected array index
              return    ! task complete return to calling subroutine
            endif
          endif
        ENDIF
        GOTO 3  ! regenerate and display inner menu
      else
        goto 40 ! regenerate the category list and display
      endif
      call usrmsg(' ',' ','-')
      goto 40   ! regenerate the category list and display

      END


C ************* EDWALL 
C EDWALL Edits a construction common block data.
C IFMAT is the unit for the associated materials file.
C ICFCDB is the unit for the associated CFC layers file.
C Information is available via common blocks in material.h. 
C Information on opaque/transparent is added.

C Note: gap layers have several representations. The legacy
C approach is to use the materials index zero to flag an
C air gap (and the user would overload the MLC layer
C with 3 air gap resistance values). An emerging technique
C is to label materials (matopaq) as follows: 
C matopaq (1 char) 'o' is opaque, 't' is transparent,
C                  'g' is air gap, '-' imported from legacy
C A -99 index (no equivalent in legacy databases) and the current code
C assumes that -99 entities are UNKNOWN.
C
C Add note about CFC modifications...

      SUBROUTINE EDWALL(ITRC,IFOC,moddb,jump,IER)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G1/G2/G7/precz.
#include "geometry.h"
#include "epara.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C Paramters
      integer ITRC  ! if > 1 then be verbose
      integer IFOC  ! index in the construction (array)
      logical moddb ! set to true of construction altered
      integer jump  ! zero no jump, 1 jump to next, -1 jump to previous
      integer IER   ! zero is ok, one is a problem

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB
      LOGICAL     OK,DOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical newgeo  ! to use for testing if new/old geometry file.


C Material properties.
      integer matarrayindex ! the index within matdatarray
      integer iwhich  ! typically current position
      integer innermat,outermat ! material index at inner and outer layers
      real guessthick ! initial thickness if from version 1.1 or 1.2

      logical chdb,showother,layermatch,layermatcha,modgeo
      logical closemat1,closemat2,modmlc

C CFC layer properties
      integer cfcarrayindex
      logical closecfc1

C Note: size of MLED supports a dozen layers in a construction.
      DIMENSION MLED(32),PNAM(ME),MMLAY(ME+2),imlay(ME+2)
      character*72 LTMP
      CHARACTER NAM*72,KEY*1,GDESCR*36
      CHARACTER SOPT*12,LAYD*124
      character T12*12,T14*14,T32*32,tdoc*248
      CHARACTER outs*124,MLED*42,MMLAY*38
      character PNAM*72
      character ZN*12,SN*12
      integer NLED,ILED  ! max items and current menu item
      integer icontinue  ! to control while loop
      integer lnmlcn,lnmlct,lnt32,lnssmlc  ! length of MLC name
      real startm, finishm, currentm
      real currentu, deltau, curdeltau ! values for adjust
      real valx, valy, valz  ! for reading range and desired U value

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDWALL'  ! set for subroutine

C Check to see that optical properties are available.
      IF(.NOT.OPTKOK)THEN
        SOPT='ALL'
        CALL EROPTDB(0,iuout,SOPT,GDESCR,IER)
        if(IER.eq.0)then
          OPTKOK=.TRUE.
        else
          CALL USRMSG('Opitcal database data not yet filled.',
     &      'Please fix this first.','W')
          return
        endif
      ENDIF

C Check version of material database.  If either closemat1 or closemat2
C is true then the common blocks are filled.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        CALL USRMSG('Materials database data not yet filled.',
     &    'Please fix this first.','W')
        ier=2
        return
      endif

C Check version of CFC layers database.  If closecfc1
C is true then the common blocks are filled.
      call eclose(cfcver,1.1,0.01,closecfc1)
      if(closecfc1)then
        continue
      else
        CALL USRMSG('CFC layers database data not yet filled.',
     &    'Please fix this first.','W')
        ier=2
        return
      endif
      
      IER=0
      moddb=.false.
      newgeo=.false.   ! assume older format geometry.
      guessthick=100.0 ! initial assumption for layer thickness
      matarrayindex=0
      cfcarrayindex=0
      iuf=IFIL+1
      lnmlct=lnblnk(mlcsymetric(ifoc))

C Check that mlc name has no illegal characters (mlcver 1 does not
C YET know how to deal with spaces in MLC names).
      if(mlcver.eq.0)then
        call st2name(mlcname(ifoc),t32)
        if(T32.ne.mlcname(ifoc))then
          lnmlct=lnblnk(t32)
          write(outs,'(4a)')
     &      mlcname(ifoc)(1:lnmlcname(ifoc)),' vs ',t32(1:lnmlct),'.'
          call usrmsg(' WARNING: mlc name has blanks',outs,'W')
        endif
      endif

C If mlcname is `UNKNOWN` and mlcsymetric is blank then we have a new
C construction. If we got this far with a blank mlcsymetric then
C set it to SYMMETRIC.
      if(mlcname(ifoc)(1:4).eq.'UNKN'.and.
     &   mlcsymetric(ifoc)(1:2).eq.'  ')then
        mlcsymetric(ifoc)='SYMMETRIC'
        matsymindex(ifoc)=0
      elseif(mlcsymetric(ifoc)(1:2).eq.'  ')then
        mlcsymetric(ifoc)='SYMMETRIC'
        matsymindex(ifoc)=0
      endif

C Read name of inverted MLC or symmetry, otherwise set mlcsymetric
C to a blank. If the tag refers to another MLC then include
C this in the menu.
      if(mlcsymetric(ifoc)(1:9).EQ.'SYMMETRIC')then
        showother=.false.
      elseif(mlcsymetric(ifoc)(1:12).EQ.'NONSYMMETRIC')then
        showother=.false.
      else
        showother=.true.
      endif

C Present editing menu and echo the construction documentation.
   2  WRITE(MLED(1),'(A,1X,A)')'a Construction:',mlcname(ifoc)(1:26)
      call edisp248(iuout,mlcdoc(ifoc),100)
      if(mlcver.eq.1)then
        WRITE(MLED(2),'(A,1X,A12)')'  Category:',
     &    mlcincat(ifoc)(1:28)
      else
        MLED(2)='  Category: general'
      endif
      if(mlcver.eq.1)then
        WRITE(MLED(3),'(A,1X,A)') 'b Menu:',
     &    mlcmenu(ifoc)(1:28)
        WRITE(MLED(4),'(A,1X,2A)')'c Doc:',
     &    mlcdoc(ifoc)(1:28),'..'
      else
        WRITE(MLED(3),'(A,1X,A)') '  Menu:',
     &    mlcname(ifoc)(1:28)
        WRITE(MLED(4),'(A,1X, A)')'  Doc:',
     &    ' Not applicable'
      endif

      IF(mlctype(ifoc)(1:4).EQ.'TRAN')THEN
        MLED(5)='d General type: Transparent '
      ELSEIF(mlctype(ifoc)(1:4).EQ.'CFC ')THEN
        MLED(5)='d General type: Complex Fenestration'
      ELSEIF(mlctype(ifoc)(1:4).EQ.'CFC2')THEN
        MLED(5)='d General type: Complex Fenestration 2'
      ELSE
        MLED(5)='d General type: Opaque      '
      ENDIF
      WRITE(MLED(6),'(A,1X,A12)')'e Optical properties:',
     &  mlcoptical(ifoc)(1:20)
      WT=0.0   ! figure out the total thickness
      DO 22, IL=1,LAYERS(IFOC)
        WT=WT+DTHK(IFOC,IL)
  22  continue
      WRITE(MLED(7),'(A,I2,a,F6.1,a)') '  Number of layers:',
     &  LAYERS(IFOC),' (',WT*1000.0,'mm thick)'
      if(showother)then
        WRITE(MLED(8),'(2a)')  'f Linked with: ',
     &    mlcsymetric(ifoc)(1:26)
      else
        WRITE(MLED(8),'(2a)')  'e Layers are: ',
     &    mlcsymetric(ifoc)(1:26)
      endif
      MLED(9)='  _____________________________  '
      IF(mlctype(ifoc)(1:4).EQ.'CFC2')THEN
        MLED(10)=' Layer|CFCl|Thick| Description   '
        MLED(11)='      |db  |(mm) | of CFC layer  '
      ELSE
        MLED(10)=' Layer|Thick | Description        '
        MLED(11)='      |(mm)  | of material        '
      ENDIF

C Detail menu.
      M=11
      RT=0.
      DO 23, IL=1,LAYERS(IFOC)
        check_CFC2_or_MLC:
     &  IF(mlctype(ifoc)(1:4).EQ.'CFC2')THEN
          cfcarrayindex=ITMCFCDB(IFOC,IL)

          if(cfcarrayindex.le.0)then

C If an earlier scan of MLC detected duplicate CFC layer names or
C was confused then cfcarrayindex could be 0.
            call edisp(iuout,
     &'One CFC layer reference is confused or a duplicate name.')
            NAM='UNKNOWN'
          else
            DBCON=cfcdbcon(cfcarrayindex)
            write(NAM,'(a)') cfcname(cfcarrayindex)(1:32)
          endif

          PNAM(IL)=NAM

          M=M+1
          CALL EMKEY(M,KEY,IER)
          WRITE(MLED(M),30)KEY,IL,cfcitmindex(ITMCFCDB(IFOC,IL)),
     &      DTHK(IFOC,IL)*1000.0, PNAM(IL)(1:19)
  30      FORMAT(A1,I4,I5,F7.1,2X,A)

        ELSE ! check_CFC2_or_MLC
          matarrayindex=IPRMAT(IFOC,IL)   ! which legacy index
 
C And if an air gap reset dbcon and NAM and assign DRAIR if newer materials file
C and we are dealing with something other than the traditional gap material.
          if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &     matopaq(matarrayindex)(1:1).eq.'h'.or.
     &     matarrayindex.eq.0)then
            DBCON=0.0
            WRITE(NAM,'(A,3F5.2)')'gap ',(DRAIR(IFOC,IL,IY),IY=1,3)
          elseif(matarrayindex.lt.0)then

C If an earlier scan of MLC detected duplicate material names or
C was confused then matarrayindex could be -1.
            call edisp(iuout,
     &'One layer material reference is confused or a duplicate name.')
            NAM='UNKNOWN'
          else
            DBCON=matdbcon(matarrayindex)
            write(NAM,'(a)') matname(matarrayindex)(1:32)
          endif

C Calculate standard U value and display as well.
C If a gap layer include the R values in the display.
          PNAM(IL)=NAM
          if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &       matopaq(matarrayindex)(1:1).eq.'h'.or.
     &       matarrayindex.eq.0)then
            RT=RT+DRAIR(IFOC,IL,1)
          ELSEIF(matarrayindex.lt.0)THEN
            continue  ! a confused material
          ELSE
            RT=RT+DTHK(IFOC,IL)/DBCON
          ENDIF
          M=M+1
          CALL EMKEY(M,KEY,IER)
          WRITE(MLED(M),29)KEY,IL,DTHK(IFOC,IL)*1000.0,
     &                   PNAM(IL)(1:26)
  29      FORMAT(A1,I5,F7.2,2X,A)

        ENDIF check_CFC2_or_MLC
  23  CONTINUE

C Rest of detail menu.
      M=M+1

      IF(mlctype(ifoc)(1:4).NE.'CFC2')THEN
C Historic ESP-r assumptions of outside hc of 0.055 and horizontal
C flow internal hc of 0.123.
C       RT=RT+0.055+0.123
C       UVALUE=1.0/RT

C ISO 6946 hc assumptions are 0.04 external, 0.13 inside horizontal,
C 0.10 inside upward flow and 0.17 inside downward flow.
        RTH=RT+0.04+0.13; UVALUEH=1.0/RTH
        RTU=RT+0.04+0.10; UVALUEU=1.0/RTU
        RTD=RT+0.04+0.17; UVALUED=1.0/RTD
        write(MLED(M),'(A,3F6.3)') ' ISO 6946 U hor/up/down',
     &    UVALUEH,UVALUEU,UVALUED
        currentu=UVALUEH  ! remember the initial U value
        M=M+1
      ENDIF
      MLED(M)='  ____________________________   '
      M=M+1
      MLED(M)='! add or delete a layer          '
      M=M+1
      IF(mlctype(ifoc)(1:4).EQ.'CFC2')THEN
        MLED(M)='                                 '
      ELSE
        MLED(M)='* adjust layer to reach U-value  '
      ENDIF
      M=M+1

C If possible to jump previous.
      if(ifoc.eq.1)then
        MLED(M)='<                              '
      else
        MLED(M)='< previous construction        '
      endif
      M=M+1

C If possible to jump next.
      if(ifoc.eq.NMLC)then
        MLED(M)='>                              '
      else
        MLED(M)='> next construction            '
      endif
      M=M+1
      MLED(M)='? help                           '
      M=M+1
      MLED(M)='- exit this menu                 '
      NLED=M
      ILED=-4

C Also compose a list of layers for additions/deletions.
      check_CFC2_or_MLC_2:
     &IF(mlctype(ifoc)(1:4).EQ.'CFC2')THEN

        MMLAY(1)=' Layer|CFCl|Thick| Description   '
        MMLAY(2)='      |db  |(mm) | of CFC layer  '
        DO 11, IL=1,LAYERS(IFOC)
        WRITE(MMLAY(IL+2),299)IL,cfcitmindex(ITMCFCDB(IFOC,IL)),
     &         DTHK(IFOC,IL)*1000.0,PNAM(IL)(1:20)
 299      FORMAT(I4,I5,F7.1,2X,A)
  11    CONTINUE

      ELSE !check_CFC2_or_MLC_2

        MMLAY(1)=' Layer|Thick| Description   '
        MMLAY(2)='      |(mm) | of material   '
        DO 12, IL=1,LAYERS(IFOC)
          WRITE(MMLAY(IL+2),300)IL,DTHK(IFOC,IL)*1000.0,
     &                   PNAM(IL)(1:20)
 300      FORMAT(I4,F7.1,2X,A)
  12    CONTINUE

      ENDIF check_CFC2_or_MLC_2

C Help text for this menu.
    3 continue
      helptopic='edit_wall_details'
      call gethelptext(helpinsub,helptopic,nbhelp)
 
      CALL EMENU('Construction editing',MLED,NLED,ILED)
      IF(ILED.EQ.NLED.or.ILED.EQ.NLED-2.or.ILED.EQ.NLED-3)THEN

        check_CFC2_or_MLC_3:
     &  IF(mlctype(ifoc)(1:4).NE.'CFC2')THEN

C Check to see that mlc of a single gap layer is not specified as well as
C an outer or inner gap layer.
          innermat=IPRMAT(IFOC,LAYERS(IFOC)) 
          outermat=IPRMAT(IFOC,1) 
          if(LAYERS(IFOC).eq.1.and.(matopaq(outermat)(1:1).eq.'g'.or.
     &       matopaq(outermat)(1:1).eq.'h'.or.outermat.eq.0))then
            call usrmsg('A construction cannot be a single gap layer.',
     &        'Please redefine it.','W')
            goto 3
          endif
          if(matopaq(outermat)(1:1).eq.'g'.or.
     &       matopaq(outermat)(1:1).eq.'h'.or.outermat.eq.0)then
            call usrmsg('An outer layer of a construction cannot be',
     &        'a gap. Please redefine it.','W')
            goto 3
          endif
          if(matopaq(innermat)(1:1).eq.'g'.or.
     &       matopaq(innermat)(1:1).eq.'h'.or.innermat.eq.0)then
            call usrmsg('An inner layer of a construction cannot be',
     &        'a gap. Please redefine it.','W')
            goto 3
          endif

C After editing MLC, re-check to see if construction is symmetric
C and then if it is linked to another `inverted` MLC save it as well.
          if(moddb)then
            call ismlcsymmetric(ifoc,layermatch)
            if(mlcsymetric(ifoc)(1:9).EQ.'SYMMETRIC')then
              if(layermatch)then
                continue
              else
                mlcsymetric(ifoc)='NONSYMMETRIC'
                matsymindex(ifoc)=0
                call edisp(iuout,
     &            'Resetting construction to nonsymmetric.')
              endif
            elseif(mlcsymetric(ifoc)(1:12).EQ.'NONSYMMETRIC')then
              if(layermatch)then
                mlcsymetric(ifoc)='SYMMETRIC   '
                matsymindex(ifoc)=0
                call edisp(iuout,'Resetting construction to symmetric.')
              else
                continue
              endif
            else

C Linked to another construction, check if it should be updated to match.
              lnmlct=lnblnk(mlcsymetric(ifoc))
              write(outs,'(5a,i4)') 'Construction ',
     &          mlcname(ifoc)(1:lnmlcname(ifoc)),' is linked to ',
     &          mlcsymetric(ifoc)(1:lnmlct),' index ',matsymindex(ifoc)
              dok=.true.
              CALL ASKOK(outs,'Update the linked construction as well?',
     &          OK,dok,nbhelp)
              if(OK)then

C Find matching MLC and copy data associated with the current MLC to
C the linked MLC.
                lnmlct=lnblnk(mlcsymetric(ifoc))
                do 794 k=1,NMLC
                  if(mlcname(k)(1:lnmlcname(k)).eq.
     &               mlcsymetric(ifoc)(1:lnmlct))then
                    mlctype(k)=mlctype(ifoc)
                    mlcoptical(k)=mlcoptical(ifoc)
                    mlccatindex(k)=mlccatindex(ifoc)  ! assume same class 
                    write(mlcsymetric(k),'(a)') mlcname(ifoc)  ! point to this one
                    matsymindex(k)=ifoc   ! update other to point to this one
                    matsymindex(ifoc)=k   ! remember its array position
                    LAYERS(k)=LAYERS(IFOC)
                    DO 793 ILL=1,LAYERS(IFOC)
                      DTHK(k,ILL)=DTHK(IFOC,ILL)
                      IPR(k,ILL)=IPR(IFOC,ILL)
                      IPRMAT(k,ILL)=IPRMAT(IFOC,ILL)
                      DRAIR(k,ILL,1)=DRAIR(IFOC,ILL,1)
                      DRAIR(k,ILL,2)=DRAIR(IFOC,ILL,2)
                      DRAIR(k,ILL,3)=DRAIR(IFOC,ILL,3)
                      LAYDESC(k,ILL)=LAYDESC(IFOC,ILL)
  793               CONTINUE

C Loop is one less than half the number of layers. Copy each layer
C into temporary space and then write to opposite side.
                    LOOP=INT(FLOAT(LAYERS(k))/2.)
                    DO 692 ILL=1,LOOP
                      DT=DTHK(k,ILL)
                      IPRT=IPR(k,ILL)
                      IPRMT=IPRMAT(k,ILL)
                      DRT1=DRAIR(k,ILL,1)
                      DRT2=DRAIR(k,ILL,2)
                      DRT3=DRAIR(k,ILL,3)
                      LAYD=LAYDESC(k,ILL)

C Opposite layer is IOP. Copy its data into ILL layer.
                      IOP=LAYERS(k)-ILL+1
                      DTHK(k,ILL)=DTHK(k,IOP)
                      IPR(k,ILL)=IPR(k,IOP)
                      IPRMAT(k,ILL)=IPRMAT(k,IOP)
                      DRAIR(k,ILL,1)=DRAIR(k,IOP,1)
                      DRAIR(k,ILL,2)=DRAIR(k,IOP,2)
                      DRAIR(k,ILL,3)=DRAIR(k,IOP,3)
                      LAYDESC(k,ILL)=LAYDESC(k,IOP)

C Finally copy temporary data into opposite layer.
                      DTHK(k,IOP)=DT
                      IPR(k,IOP)=IPRT
                      IPRMAT(k,IOP)=IPRMT
                      DRAIR(k,IOP,1)=DRT1
                      DRAIR(k,IOP,2)=DRT2
                      DRAIR(k,IOP,3)=DRT3
                      LAYDESC(k,IOP)=LAYD
  692               CONTINUE
                  endif
  794           continue
              endif
            endif

            call edisp(iuout,'  ')
            call edisp(iuout,
     &        'Please ensure that you save the constructions...')
          endif

        ENDIF check_CFC2_or_MLC_3

C Take into account whether the user as requested a jump.
        if(ILED.EQ.NLED-2)then
          jump=1
        elseif(ILED.EQ.NLED-3)then
          jump=-1
        else
          jump=0
        endif
        RETURN

      ELSEIF(ILED.EQ.NLED-1)THEN

C List the help.
        helptopic='edit_wall_details'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('construction editing',nbhelp,'-',0,0,IER)
      ELSEIF(ILED.EQ.NLED-4)THEN

        check_CFC2_or_MLC_4:
     &  IF(mlctype(ifoc)(1:4).NE.'CFC2')THEN

C Adjust layer.
          CALL EASKABCD('Adjust options: ',' ',
     &      'Alter (solid) thickness','Alter gap R',
     &      'Find alternative material','Continue',IW,nbhelp)
          IF(IW.EQ.1.or.IW.eq.2)THEN

C Report the current U value.
            write(outs,'(A,3F6.3)') ' ISO 6946 U h/u/d',
     &        UVALUEH,UVALUEU,UVALUED
            call edisp(iuout,outs)

C Ask for which layer to adjust, make sure it is not a gap.
            call edisp(iuout,' Select layer to adjust.')
            IX=1
            CALL EPMENSV
            CALL EPICKS(IX,imlay,' ',' Layer to adjust: ',
     &        38,LAYERS(IFOC)+2,MMLAY,'Layer to adjust',IER,nbhelp)
            CALL EPMENRC
          ENDIF
          IF(IW.EQ.1)THEN

C If adjust thickness and a layer picked start thickness equal to
C what exists up to 250mm.
            if(IX.eq.1.and.imlay(1).gt.2)then
              IWHICH=imlay(1)-2
              write(outs,'(3A,F7.4,A)')' Adjusting ',PNAM(IWHICH)(1:18),
     &        '... which is currently ',DTHK(IFOC,IWHICH)*1000.0,
     &        ' thick.'
              call edisp(iuout,outs)
              write(t32,'(f7.1,a)') DTHK(IFOC,IWHICH)*1000.0,
     &        '  250   0.1  '
              CALL EASKS(t32,
     &        'Thickness (minimum & maximum mm) & desired U value',
     &        ' ',32,' ','thickness and U',IER,nbhelp)
              K=0
              CALL EGETWR(t32,K,VALX,0.1,250.,'W','minimum mm',IER)
              CALL EGETWR(t32,K,VALY,0.1,250.,'W','maximum mm',IER)
              CALL EGETWR(t32,K,VALZ,0.001,10.,'W','desired U',IER)

C Start at the VALX thickness and increment 1mm each time until
C close to VALZ.
              deltau=abs(currentu-valz)  ! remember difference
              write(outs,'(a,f6.3,a)') 'Improvement of ',deltau,
     &          ' needed.'
              call edisp(iuout,outs)
              icontinue=1
              startm=VALX/1000.0
              currentm=startm
              finishm=VALY/1000.0
              do while (icontinue.ne.0)
                currentm=currentm+0.001  ! 1mm increment
                if(currentm.gt.finishm)then
                  icontinue=0  ! signal no to loop again
                endif
                RT=0.

                DO 123, IL=1,LAYERS(IFOC)
                  matarrayindex=IPRMAT(IFOC,IL)   ! which material index
 
C And if a gap layer reset dbcon.
                  if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &               matopaq(matarrayindex)(1:1).eq.'h'.or.
     &               matarrayindex.eq.0)then
                    DBCON=0.0
                    if(closemat2)then
                      DRAIR(IFOC,IL,1)=matgapares(matarrayindex,1)
                      RT=RT+DRAIR(IFOC,IL,1)  ! while we are sure
                    else
                      RT=RT+DRAIR(IFOC,IL,1)
                    endif
                  elseif(matarrayindex.lt.0)then
                    continue  ! confused material
                  else
                    DBCON=matdbcon(matarrayindex)

C If this is the layer we are adjusting then substitute the current
C layer thickness into the U value equation.
                    if(IL.eq.IWHICH)then
                      RT=RT+currentm/DBCON
                    else
                      RT=RT+DTHK(IFOC,IL)/DBCON
                    endif
                  endif

  123           CONTINUE

C ISO 6946 hc assumptions are 0.04 external, 0.13 inside horizontal,
C 0.10 inside upward flow and 0.17 inside downward flow.
                RTH=RT+0.04+0.13; UVALUEH=1.0/RTH
                RTU=RT+0.04+0.10; UVALUEU=1.0/RTU
                RTD=RT+0.04+0.17; UVALUED=1.0/RTD
                curdeltau=abs(UVALUEH-valz)  ! remember new difference

C Is UVALUEH closer to what we want. If it is not then stop looking.
C Report as we get close to desired U value.
                if(curdeltau.lt.deltau)then
                  write(outs,'(A,3F6.3)') 'Test ISO 6946 U h/u/d',
     &            UVALUEH,UVALUEU,UVALUED
                  if(curdeltau.lt.0.03)call edisp(iuout,outs)
                  write(outs,'(f6.3,a,f6.3,a,f6.1)') UVALUEH,
     &              ' is getting closer to desired U',valz,
     &              ' with a layer thickness of ',currentm*1000.0
                  if(curdeltau.lt.0.03)call edisp(iuout,outs)
                  deltau=curdeltau  ! update deltau
                  if(currentm.gt.finishm)then
                    icontinue=0   ! exceeded the maximum thickness
                    write(outs,'(2a,f6.3,a,f6.1)') PNAM(IWHICH)(1:18),
     &                ' range limit reached at U',UVALUEH,
     &                ' and thickness ',currentm*1000.0
                    call edisp(iuout,outs)
                  endif
                else

C No longer getting closer so step back one and report.
                  icontinue=0
                  write(outs,'(A,3F6.3)') 'Test ISO 6946 U h/u/d',
     &              UVALUEH,UVALUEU,UVALUED
                  call edisp(iuout,outs)
                  currentm=currentm-0.001
                  write(outs,'(2a,f6.3,a,f6.1)') PNAM(IWHICH)(1:18),
     &              ' is close to desired U',valz,
     &              ' with a thickness of ',currentm*1000.0
                  call edisp(iuout,outs)
                endif
              end do
            endif
          ELSEIF(IW.EQ.2)THEN
            if(IX.eq.1.and.imlay(1).gt.2)then
              IWHICH=imlay(1)-2
              write(outs,'(3A,F7.4,A)')' Adjusting ',PNAM(IWHICH)(1:18),
     &        '... which has current ',DRAIR(IFOC,IWHICH,1),
     &        ' gap resistance.'
              call edisp(iuout,outs)
              write(t32,'(f7.4,a)') DRAIR(IFOC,IWHICH,1),
     &        '  0.60   1.4  '
              CALL EASKS(t32,
     &        'Gap resistance (min & max) & desired U value',
     &        ' ',32,' ','gap resistance and U',IER,nbhelp)
              K=0
              CALL EGETWR(t32,K,VALX,0.1,250.,'W','minimum r',IER)
              CALL EGETWR(t32,K,VALY,0.1,250.,'W','maximum r',IER)
              CALL EGETWR(t32,K,VALZ,0.001,10.,'W','desired U',IER)

C Start at the VALX resistance and increment by .002 each time until
C close to VALZ.
              deltau=abs(currentu-valz)  ! remember difference
              write(outs,'(a,f6.3,a)') 'Improvement of ',deltau,
     &          ' needed.'
              call edisp(iuout,outs)
              icontinue=1
              startm=VALX
              currentm=startm
              finishm=VALY
              do while (icontinue.ne.0)
                currentm=currentm+0.002  ! resistance increment
                if(currentm.gt.finishm)then
                  icontinue=0  ! signal no to loop again
                endif
                RT=0.
                DO 124, IL=1,LAYERS(IFOC)

                  matarrayindex=IPRMAT(IFOC,IL)   ! which legacy index
 
C And if a gap reset dbcon.
                  if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &               matopaq(matarrayindex)(1:1).eq.'h'.or.
     &               matarrayindex.eq.0)then
                    DBCON=0.0
                    if(closemat2)then

C If this is the layer we are adjusting then substitute the current
C gap R into the equation.
                      if(IL.eq.IWHICH)then
                        RT=RT+currentm  ! while we are sure
                      else
                        DRAIR(IFOC,IL,1)=matgapares(matarrayindex,1)
                        RT=RT+DRAIR(IFOC,IL,1)  ! while we are sure
                      endif
                    else
                      if(IL.eq.IWHICH)then
                        RT=RT+currentm  ! use the current R
                      else
                        RT=RT+DRAIR(IFOC,IL,1)
                      endif
                    endif
                  elseif(matarrayindex.lt.0)then
                    continue  ! use RT calc above
                  else
                    DBCON=matdbcon(matarrayindex)
                    RT=RT+DTHK(IFOC,IL)/DBCON
                  endif

  124           CONTINUE

C ISO 6946 hc assumptions are 0.04 external, 0.13 inside horizontal,
C 0.10 inside upward flow and 0.17 inside downward flow.
                RTH=RT+0.04+0.13; UVALUEH=1.0/RTH
                RTU=RT+0.04+0.10; UVALUEU=1.0/RTU
                RTD=RT+0.04+0.17; UVALUED=1.0/RTD
                curdeltau=abs(UVALUEH-valz)  ! remember new difference

C Is UVALUEH closer to what we want. If it is not then stop looking.
C Report as we get close to desired U value.
                if(curdeltau.lt.deltau)then
                  write(outs,'(A,3F6.3)') 'Test ISO 6946 U h/u/d',
     &            UVALUEH,UVALUEU,UVALUED
                  if(curdeltau.lt.0.03)call edisp(iuout,outs)
                  write(outs,'(f6.3,a,f6.3,a,f7.4)') UVALUEH,
     &            ' is getting closer to desired U',valz,
     &            ' with a gap R of ',currentm
                  if(curdeltau.lt.0.03)call edisp(iuout,outs)
                  deltau=curdeltau  ! update deltau
                  if(currentm.gt.finishm)then
                    icontinue=0   ! exceeded the maximum R
                    write(outs,'(2a,f6.3,a,f7.4)') PNAM(IWHICH)(1:18),
     &              ' range limit reached at U',UVALUEH,
     &              ' and gap R of ',currentm
                    call edisp(iuout,outs)
                  endif
                else

C No longer getting closer so step back one and report.
                  icontinue=0
                  write(outs,'(A,3F6.3)') 'Test ISO 6946 U h/u/d',
     &              UVALUEH,UVALUEU,UVALUED
                  call edisp(iuout,outs)
                  currentm=currentm-0.002   ! decrement R
                  write(outs,'(2a,f6.3,a,f7.4)') PNAM(IWHICH)(1:18),
     &            ' is close to desired U',valz,
     &            ' with a gap R of ',currentm
                  call edisp(iuout,outs)
                endif
              end do
            endif
          ELSEIF(IW.EQ.3)THEN
            call edisp(iuout,'this option not yet working')
            goto 2  ! jump back to the menue.
          ELSEIF(IW.EQ.4)THEN
            goto 2  ! jump back to the menue.
          ENDIF

C Process the request.

        ENDIF check_CFC2_or_MLC_4

      ELSEIF(ILED.EQ.NLED-5)THEN

C +- Layer.
        CALL EASKABCD(' Layer options: ',' ',' Delete',
     &    ' Append @ inside face',' Insert','Continue',IW,nbhelp)
        IF(IW.EQ.1)THEN

C If number of layers is edited then ask the user which one to remove.
C Now loop through the layers and copy the contents of layers >
C IWHICH into the next lower one.
          call edisp(iuout,' Select layer to delete.')
          IX=1
          CALL EPMENSV
          CALL EPICKS(IX,imlay,' ',' Layer to delete: ',
     &      38,LAYERS(IFOC)+2,MMLAY,'Layer to delete',IER,nbhelp)
          CALL EPMENRC
          if(IX.eq.1.and.imlay(1).gt.2)then
            IWHICH=imlay(1)-2
            write(outs,'(3A)')' Removing ',PNAM(IWHICH)(1:18),'...'
            DO 791 IVV=IWHICH,LAYERS(IFOC)-1
              IF(mlctype(ifoc)(1:4).EQ.'CFC2')THEN
                ITMCFCDB(IFOC,IVV)=ITMCFCDB(IFOC,IVV+1)
              ELSE
                IPR(IFOC,IVV)=IPR(IFOC,IVV+1)
                IPRMAT(IFOC,IVV)=IPRMAT(IFOC,IVV+1)
                DRAIR(IFOC,IVV,1)=DRAIR(IFOC,IVV+1,1)
                DRAIR(IFOC,IVV,2)=DRAIR(IFOC,IVV+1,2)
                DRAIR(IFOC,IVV,3)=DRAIR(IFOC,IVV+1,3)
              ENDIF
              DTHK(IFOC,IVV)=DTHK(IFOC,IVV+1)
              LAYDESC(IFOC,IVV)=LAYDESC(IFOC,IVV+1)
  791       CONTINUE
            LAYERS(IFOC)=LAYERS(IFOC)-1
          endif

C Check if layers no longer match.
          IF(mlctype(ifoc)(1:4).NE.'CFC2')THEN
            call ismlcsymmetric(ifoc,layermatcha)
            if(.NOT.layermatcha)then
            call edisp(iuout,'Layers might now be nonsymmetric.')
            call edisp(iuout,'This will be reconciled when exiting.')
            endif
          ENDIF
        ELSEIF(IW.EQ.2.or.IW.eq.3)THEN

C Although the data structure can support ME layers the interface is
C currently limited to 12 layers.
          IF(LAYERS(IFOC).LT.ME)THEN
            if(LAYERS(IFOC).GE.12)then
              call edisp(iuout,
     &          'Sorry, can only edit up to 12 layers.')
              goto 3
            endif
            if(iw.eq.3)then

C If inserting internal layer, shift inner layers before editing.

C << todo: find a way to restore prior state if the user elects
C << to cancel part way through the process.

              IX=1
              CALL EPMENSV
              CALL EPICKS(IX,imlay,' ',' Point of insertion: ',
     &          38,LAYERS(IFOC)+2,MMLAY,'Point of insertion',
     &          IER,nbhelp)
              CALL EPMENRC
              if(IX.eq.1.and.imlay(1).gt.2)then
                IWHICH=imlay(1)-2
                ILN=IWHICH
                LAYERS(IFOC)=LAYERS(IFOC)+1
                IVV=LAYERS(IFOC)+1
  792           CONTINUE
                IVV=IVV-1
                IF(mlctype(ifoc)(1:4).EQ.'CFC2')THEN
                  ITMCFCDB(IFOC,IVV)=ITMCFCDB(IFOC,IVV-1)
                ELSE
                  IPR(IFOC,IVV)=IPR(IFOC,IVV-1)
                  IPRMAT(IFOC,IVV)=IPRMAT(IFOC,IVV-1)
                  DRAIR(IFOC,IVV,1)=DRAIR(IFOC,IVV-1,1)
                  DRAIR(IFOC,IVV,2)=DRAIR(IFOC,IVV-1,2)
                  DRAIR(IFOC,IVV,3)=DRAIR(IFOC,IVV-1,3)
                ENDIF
                DTHK(IFOC,IVV)=DTHK(IFOC,IVV-1)
                LAYDESC(IFOC,IVV)=LAYDESC(IFOC,IVV-1)
                IF(IVV.GT.IWHICH+1) GOTO 792
              endif
            else
              ILN=LAYERS(IFOC)+1
            endif

C Ask if user wishes to browse through materials to find suitable
C reference. Use (potentially multi-page) elistmat to select via materials array.
            dok=.true.
            check_CFC2_or_MLC_5:
     &      IF(mlctype(ifoc)(1:4).EQ.'CFC2')THEN
              call askok(
     &          'Change the CFC layer reference via browsing available',
     &          'CFC layers? (NO keeps the current CFC layer)',
     &          OK,dok,nbhelp)
              if(OK)then
                iwhich = 0
                CALL ELISTCFC(iwhich,chdb,'-',cfcarrayindex,IER)
              else
                IWHICH=ITMCFCDB(IFOC,ILN)  ! keep the current material
                cfcarrayindex=ITMCFCDB(IFOC,ILN)   ! and the material index
              endif

              if(iw.eq.2)LAYERS(IFOC)=ILN

C Make up LAYDESC and PNAM as combination of cfcnam and cfcdoc based on
C the newly selected CFC layer. This is necessary so that other
C code can recover the details of a confused layer.
              if(cfcarrayindex.gt.0)then
                lnam=lnblnk(cfcname(cfcarrayindex))
                lfordoc = 72 - (lnam +4)    ! space left for doc
                write(LAYDESC(IFOC,ILN),'(3a)') 
     &            cfcname(cfcarrayindex)(1:lnam),' : ',
     &            cfcdoc(cfcarrayindex)(1:lfordoc)
                write(PNAM(ILN),'(3a)')
     &            cfcname(cfcarrayindex)(1:lnam),' : ',
     &            cfcdoc(cfcarrayindex)(1:lfordoc)
                ITMCFCDB(IFOC,ILN)=cfcarrayindex  ! update array index for this layer as well
              endif

              VAL=10.0 ! defalt layer thickness (mm)

            ELSE ! check_CFC2_or_MLC_5

              call askok(
     &          'Change the material reference via browsing available',
     &          'materials? (NO keeps the current material)',
     &          OK,dok,nbhelp)
              IF(OK)then
                iwhich = 0
                CALL EPMENSV
                CALL ELISTMAT(iwhich,chdb,'-',matarrayindex,IER)
                CALL EPMENRC

C If iwhich came back as a zero then the material actually was
C the traditional gap so reset matarrayindex.
                if(iwhich.eq.0)then
                  matarrayindex=0 
                endif
                if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &             matopaq(matarrayindex)(1:1).eq.'h'.or.
     &             matarrayindex.eq.0)then
                  CALL EASKABC('Your selection is a gap. Options:',
     &              ' ','accept','reselect material','abort',
     &              iwair,nbhelp)
                  if(iwair.eq.2)then
                    CALL EPMENSV
                    CALL ELISTMAT(iwhich,chdb,'-',matarrayindex,IER)
                    CALL EPMENRC
                    if(iwhich.eq.0)then
                      matarrayindex=0 
                    endif
                  elseif(iwair.eq.3)then
                    ILED=-1
                    GOTO 3
                  endif
                  IPRMAT(IFOC,ILN)=matarrayindex   ! set new material index to layer
                elseif(matarrayindex.lt.0)then
                  continue  ! confused material
                endif
              else
                IWHICH=IPR(IFOC,ILN)  ! keep the current material
                matarrayindex=IPRMAT(IFOC,ILN)   ! and the material index
              endif

C Take the legacy index and assign it to this layer.
              IPR(IFOC,ILN)=IWHICH
              if(iw.eq.2) LAYERS(IFOC)=ILN

C Make up LAYDESC and PNAM as combination of matnam and matdoc based on
C the newly selected material. This is necessary so that other
C code can recover the details of a -99 item.
              if(matarrayindex.gt.0)then
                lnam=lnblnk(matname(matarrayindex))
                lfordoc = 72 - (lnam +4)    ! space left for doc
                write(LAYDESC(IFOC,ILN),'(3a)') 
     &            matname(matarrayindex)(1:lnam),' : ',
     &            matdoc(matarrayindex)(1:lfordoc)
                write(PNAM(ILN),'(3a)')
     &            matname(matarrayindex)(1:lnam),' : ',
     &            matdoc(matarrayindex)(1:lfordoc)
                IPRMAT(IFOC,ILN)=matarrayindex  ! update array index for this layer as well
              elseif(matarrayindex.eq.0)then
                IPRMAT(IFOC,ILN)=matarrayindex  ! update array index for air gap
              endif

C If a gap then ask for gap resistance, recover resistance if newer matereials file.
              if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &           matopaq(matarrayindex)(1:1).eq.'h'.or.
     &           matarrayindex.eq.0)then
                if(matarrayindex.eq.0)then
                  guessthick=0.1
                else
                  guessthick=matdbthick(matarrayindex)
                endif
                if(closemat2)then
                  DRAIR(IFOC,ILN,1)=matgapares(matarrayindex,1)
                  DRAIR(IFOC,ILN,2)=matgapares(matarrayindex,2)
                  DRAIR(IFOC,ILN,3)=matgapares(matarrayindex,3)
                endif

                WRITE(outs,117)mlcname(IFOC)(1:lnmlcname(ifoc))
  117           FORMAT('Default R value for gap in ',a)
                if(DRAIR(IFOC,ILN,1).lt.0.001)then
                  VAL=0.17
                else
                  VAL=DRAIR(IFOC,ILN,1)
                endif
                CALL EASKR(VAL,outs,
     &            'if the orientation is vertical:',
     &            0.01,'W',99.9,'W',0.17,'default gap R',IER,nbhelp)
                DRAIR(IFOC,ILN,1)=VAL
                if(DRAIR(IFOC,ILN,2).lt.0.001)then
                   VAL=0.17
                else
                   VAL=DRAIR(IFOC,ILN,2)
                endif
                CALL EASKR(VAL,outs,
     &            'if the orientation is a floor or ceiling:',
     &            0.01,'W',99.9,'W',0.17,'default gap R',IER,nbhelp)
                DRAIR(IFOC,ILN,2)=VAL
                if(DRAIR(IFOC,ILN,3).lt.0.001)then
                   VAL=0.17
                else
                   VAL=DRAIR(IFOC,ILN,3)
                endif
                CALL EASKR(VAL,outs,
     &            'if the orientation is sloped or UNKNOWN:',
     &            0.01,'W',99.9,'W',0.17,'default gap R',IER,nbhelp)
                DRAIR(IFOC,ILN,3)=VAL
                guessthick=100.0

C Now update LAYDESC & PNAM.
                write(LAYDESC(IFOC,ILN),'(A,3F5.2)')'gap ',
     &            (DRAIR(IFOC,ILN,IY),IY=1,3)
                write(PNAM(ILN),'(A,3F5.2)')'gap ',
     &            (DRAIR(IFOC,ILN,IY),IY=1,3)

              ELSE

C Not a gap so set DRAIR to something and set default thickness.
                DRAIR(IFOC,ILN,1)=0.17; DRAIR(IFOC,ILN,2)=0.17
                DRAIR(IFOC,ILN,3)=0.17
                if(matarrayindex.eq.0)then
                  guessthick=0.1
                else
                  guessthick=matdbthick(matarrayindex)
                endif
              ENDIF
              VAL=guessthick

C Debug
C              write(6,*) 'laydesc',LAYDESC(IFOC,ILN)
C              write(6,*) 'PNAM',PNAM(ILN),iln
C              write(6,*) 'iprmat',IPRMAT(IFOC,ILN),matarrayindex

            ENDIF check_CFC2_or_MLC_5

            CALL EASKR(VAL,'Layer thickness (mm)','Confirm:',
     &        1.0,'W',300.0,'W',100.0,'layer thickness mm',
     &        IER,nbhelp)
            DTHK(IFOC,ILN)=VAL*0.001
          ELSE
            CALL USRMSG(' ',' Exceeds layer limit!','W')
            ILED=-1
            GOTO 3
          ENDIF
        ELSEIF(IW.EQ.4)THEN
          moddb = .false.
          GOTO 3
        ENDIF
        moddb = .true.

C Check if layers no longer match.
        IF(mlctype(ifoc)(1:4).NE.'CFC2')THEN
          call ismlcsymmetric(ifoc,layermatcha)
          if(.NOT.layermatcha)then
            call edisp(iuout,'Layers might now be nonsymmetric.')
            call edisp(iuout,'This will be reconciled when exiting.')
          endif
        ENDIF
      ELSEIF(ILED.EQ.1)THEN

C Edit construction name, if it is linked to another MLC then the
C linked MLC should point back to the edited name.
        T32=mlcname(ifoc)
        lnt32=lnblnk(T32)

C If changing the name of construction then find out if
C any of the surfaces in the model use this construction
C If they do then warn the user.
        if(ncon.gt.0)then
          modgeo=.false.
          write(outs,'(a)') ' Checking surface attributes...'
          CALL USRMSG(' ',outs,'-')
          do icc=1,ncon
            lnssmlc=lnblnk(SSMLCN(icc))
            if(SSMLCN(icc)(1:lnssmlc).eq.T32(1:lnt32))then
              WRITE(outs,'(a,I3,1X,F7.2,F5.0,F5.0,1X,A,1X,A,
     &          1X,A,1X,A,1X,A)')'found: ',icc,
     &          SSNA(icc),SSPAZI(icc),SSPELV(icc),SSNAME(icc),
     &          SSOTF(icc),SSVFC(icc),SSMLCN(icc)(1:lnssmlc),
     &          SSOTHER(icc,1)
              call edisp(iuout,outs)
              modgeo=.true.
            endif
          enddo
          write(outs,'(a)') ' Checking surface attributes...done.'
          CALL USRMSG(' ',outs,'P')
          if(modgeo)then
            call usrmsg(
     &      'Surfaces use this construction so the name cannot be',
     &      'changed. Replace these with another construction first.',
     &      'W')
            moddb = .false.
            goto 3
          endif
        endif
        if(mlcver.eq.0)then
          write(T14,'(2a)') mlcname(ifoc)(1:12),'  '
          CALL EASKS(T14,' ','Name of the construction?',
     &      14,' ','Construction name',IER,nbhelp)
          write(T12,'(a)') T14(1:12)
          call st2name(T12,mlcname(ifoc))
          lnmlcname(ifoc)=lnblnk(mlcname(ifoc))  ! remember length
          call st2name(T12,mlcmenu(ifoc))
          lnmlcmenu(ifoc)=lnblnk(mlcmenu(ifoc))  ! remember length
          write(mlcincat(ifoc),'(a)') 'general constructions'
          mlcdoc(ifoc)='Not yet documented'
        else

C Need to check whether logic elsewhere works with spaces in MLC name.
          write(T32,'(a)') mlcname(ifoc)(1:lnmlcname(ifoc))
          CALL EASKS(T32,' ','Name of the construction?',
     &      32,' ','Construction name',IER,nbhelp)
          write(mlcname(ifoc),'(a)') T32(1:lnblnk(T32))
          lnmlcname(ifoc)=lnblnk(mlcname(ifoc))  ! remember length
C          call st2name(T32,mlcname(ifoc))   ! NOT SURE IF THIS STILL REQUIRED
        endif

        IF(mlctype(ifoc)(1:4).NE.'CFC2')THEN
          if(mlcsymetric(ifoc)(1:9).EQ.'SYMMETRIC')then
            continue
          elseif(mlcsymetric(ifoc)(1:12).EQ.'NONSYMMETRIC')then
            continue
          else

C Update current MLC then find matching MLC and copy data associated
C with the current MLC to the linked MLC. Lastly re-establish tokens
C for the current item.
            do k=1,NMLC
              lnmlct=lnblnk(mlcsymetric(ifoc))
              if(mlcname(k)(1:lnmlcname(k)).eq.
     &           mlcsymetric(ifoc)(1:lnmlct))then
                mlcsymetric(k)=mlcname(ifoc)  ! update other array
                matsymindex(k)=ifoc           ! update other to point to this one
                matsymindex(ifoc)=k           ! remember its array position
              endif
            enddo
          endif
          moddb = .true.
        ENDIF

      ELSEIF(ILED.EQ.2)THEN

C Category ??

      ELSEIF(ILED.EQ.3)THEN

C Menue entry for this construction.
        write(T32,'(a)') mlcmenu(ifoc)
        CALL EASKS(T32,' ','Menu entry?',
     &    32,' ','Construction menu',IER,nbhelp)
        if(T32(1:2).ne.'  ')then
          write(mlcmenu(ifoc),'(a)') T32(1:lnblnk(T32))
          lnmlcmenu(ifoc)=lnblnk(mlcmenu(ifoc))  ! remember length
          moddb = .true.
        endif

      ELSEIF(ILED.EQ.4)THEN

C MLC Documentation.
        write(tdoc,'(a)') mlcdoc(ifoc)(1:lnblnk(mlcdoc(ifoc)))
        CALL EASKS248(tdoc,'Documentation for MLC','confirm:',
     &    72,' ','MLC documentation',IER,nbhelp)
        if(tdoc(1:2).ne.'  ')then
          write(mlcdoc(ifoc),'(a)') tdoc(1:lnblnk(tdoc))
          moddb = .true.
        endif

      ELSEIF(ILED.EQ.5)THEN

C Select Opaque or transparent or complex fenestration construction.
        helptopic='construction_opaque_trn'
        call gethelptext(helpinsub,helptopic,nbhelp)

        CALL EASKABC(' ','Please choose one type :',
     &    'Opaque construction','Transparent construction',
     &    'Complex Fenestration Construction',IW,nbhelp)

        IF(IW.EQ.1)THEN
          mlctype(ifoc)='OPAQ' 
          mlcoptical(ifoc)='OPAQUE'
        ELSEIF(IW.EQ.2)THEN
          mlctype(ifoc)='TRAN' 
        ELSEIF(IW.EQ.3)THEN
          CALL PHELPD('CFC instructions',nbhelp,'-',0,0,IER)
          mlcoptical(ifoc)='N/A'
          CALL EASKAB(' ','Please choose CFC input method :',
     &      'Use native CFC layers db',
     &      'Import CFC layer properties (legacy)',
     &      IW,nbhelp)
          if(IW.eq.1)then
            mlctype(ifoc)='CFC2'
          elseif(IW.eq.2)then
            mlctype(ifoc)='CFC '
          endif

        ENDIF
        moddb = .true.

C At this point need to check to see if this change needs to be
C applied to surfaces within the model. If there are zones loop
C through them and see if any surfaces have this construction 
C attribute. If they do reset SSOTF and update the geometry file.

C << good place to try and use connection based information >>
        if(ncomp.gt.0)then
          do iz=1,ncomp
            write(zn,'(A)') zname(IZ)
            write(outs,'(3a)') ' Checking: ',zn(1:lnblnk(zn)),
     &         ' attributes...'
            CALL USRMSG(' ',outs,'-')
            LTMP=LGEOM(IZ)
            call georead(IUF,LTMP,IZ,1,iuout,IER)
            WRITE(outs,'(2a)')
     &        '           Sur|  Area  |Azim|Elev| surface    |',
     &        'geometry| multilayer  |environment'
            call edisp(iuout,outs)
            WRITE(outs,'(2a)')
     &        '              |  m^2   |deg |deg | name       |',
     &        'type|loc| constr name |other side '
            call edisp(iuout,outs)
            modgeo=.false.

C Loop to find surfaces which use this construction.
            do i=1,nsur
              ioc=IZSTOCN(IZ,i)
              lnssmlc=lnblnk(SSMLCN(ioc))
              if(SSMLCN(ioc)(1:lnssmlc).eq.
     &           mlcname(ifoc)(1:lnmlcname(ifoc)))then
                write(sn,'(a)') SSNAME(ioc)
                write(SSOTF(ioc),'(a)') mlctype(ifoc)
                WRITE(outs,'(a,I3,1X,F7.2,F5.0,F5.0,1X,A,1X,A,
     &            1X,A,1X,A,1X,A)')'updated:   ',I,
     &            SSNA(ioc),SSPAZI(ioc),SSPELV(ioc),SN,SSOTF(ioc),
     &            SSVFC(ioc),SSMLCN(ioc)(1:lnssmlc),SSOTHER(ioc,1)(1:13)
                call edisp(iuout,outs)
                modgeo=.true.
              endif
            enddo  ! of i

C If SOFT has changed update the zone geometry file. Warn user about
C out of date zone construction files.
            if(modgeo)then
              call eclose(gversion(iz),1.1,0.01,newgeo)
              if(igupgrade.eq.2.and.(.NOT.newgeo))then
                gversion(iz) =1.1
                newgeo = .true.
              endif
              if(newgeo)then
                call geowrite(IUF,LTMP,IZ,ITRU,3,IER)
              else
                call emkgeo(IUF,LTMP,IZ,3,IER)
              endif
              write(outs,'(3a)') ' Updating: ',zn(1:lnblnk(zn)),
     &          ' attributes... done.'
              CALL USRMSG(' ',outs,'-')
            endif
          enddo  ! of iz
          call usrmsg(
     &      'Zone construction files may need to be updated',
     &      'after the database modifications are completed.','W')
          call usrmsg(
     &      'You next task it to select an optical property set for',
     &      'this construction (option c).','W')
        endif

      ELSEIF(ILED.EQ.6)THEN

C Edit optical properties. Also check to see that the number of layers
C and reference for materials matches.
        helptopic='construction_optics'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if((mlctype(ifoc)(1:4).NE.'OPAQ').AND.
     &     (mlctype(ifoc)(1:3).NE.'CFC'))THEN
          CALL PHELPD('optical selection',nbhelp,'-',0,0,IER)

  777     CALL EDWINO(SOPT,IER)
          CALL EROPTDB(ITRC,iuout,SOPT,GDESCR,IER)
          IF(IER.EQ.2)THEN
            dok=.true.
            CALL ASKOK(' ',' Not found... try again? ',OK,dok,nbhelp)
            IF(OK)GOTO 777
          ENDIF
          write(mlcoptical(ifoc),'(a)') SOPT
          IF(LAYERS(IFOC).NE.NTL) CALL USRMSG(
     &      ' The number of layers in the optical and ',
     &      ' construction databases differ!','W')
        ENDIF
        moddb = .true.

C Check to see if this change needs to be applied to surfaces within the model.
        if(ncomp.gt.0)then
          do iz=1,ncomp
            write(zn,'(A)') zname(IZ)
            write(outs,'(3a)') ' Checking: ',zn(1:lnblnk(zn)),
     &         ' attributes...'
            CALL USRMSG(' ',outs,'-')
            LTMP=LGEOM(IZ)
            call georead(IUF,LTMP,IZ,1,iuout,IER)

            WRITE(outs,'(2a)')
     &        '           Sur|  Area  |Azim|Elev| surface    |',
     &        'geometry| multilayer  |environment'
            call edisp(iuout,outs)
            WRITE(outs,'(2a)')
     &        '              |  m^2   |deg |deg | name       |',
     &        'type|loc| constr name |other side '
            call edisp(iuout,outs)
            modgeo=.false.

C Loop to find surfaces which use this construction.
            do i=1,nsur
              ioc=IZSTOCN(IZ,i)
              lnssmlc=lnblnk(SSMLCN(ioc))
              if(SSMLCN(ioc)(1:lnssmlc).eq.
     &           mlcname(ifoc)(1:lnmlcname(ifoc)))then
                write(sn,'(a)') SSNAME(ioc)
                write(SSOTF(ioc),'(a)') mlctype(ifoc)
                WRITE(outs,'(a,I3,1X,F7.2,F5.0,F5.0,1X,A,1X,A,
     &            1X,A,1X,A,1X,A)')'updated:   ',I,
     &            SSNA(ioc),SSPAZI(ioc),SSPELV(ioc),SN,SSOTF(ioc),
     &            SSVFC(ioc),SSMLCN(ioc)(1:lnssmlc),SSOTHER(ioc,1)(1:13)
                call edisp(iuout,outs)
                modgeo=.true.
              endif
            enddo  ! of i

C If SOFT has changed update the zone geometry file. Warn user about
C out of date zone construction files.
            if(modgeo)then
              call eclose(gversion(iz),1.1,0.01,newgeo)
              if(igupgrade.eq.2.and.(.NOT.newgeo))then
                gversion(iz) =1.1
                newgeo = .true.
              endif
              if(newgeo)then
                call geowrite(IUF,LTMP,IZ,ITRU,3,IER)
              else
                call emkgeo(IUF,LTMP,IZ,3,IER)
              endif
              write(outs,'(3a)') ' Updating: ',zn(1:lnblnk(zn)),
     &          ' attributes... done.'
              CALL USRMSG(' ',outs,'-')
            endif
          enddo  ! of iz
        endif

      ELSEIF(ILED.EQ.7)THEN

C Allow linking to inverted construction.
        helptopic='edit_wall_details'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(mlcsymetric(ifoc)(1:9).EQ.'SYMMETRIC')then
          call usrmsg(
     &      'There is no need to link to an `inverted` version of this',
     &      'construction. It is symmetric and ok for partitions.','W')
        elseif(mlcsymetric(ifoc)(1:12).EQ.'NONSYMMETRIC')then
          dok=.true.
          CALL ASKOK('Would you like to link this construction to',
     &      'another which has `inverted` layers?',OK,dok,nbhelp)
          if(OK)then
            if(mlcver.eq.0)then
              call epkmlc(iwhich,
     &          'Link with which `inverted` construction?',' ',ierr)
            else
              call epkmlc(iwhich,
     &          'Link with which `inverted` construction?',
     &          '(via this global list)',ierr)
            endif
            if(iwhich.eq.0) goto 3
            if(LAYERS(IFOC).eq.LAYERS(iwhich))then

C Update current MLC then find matching MLC and copy data associated
C with the current MLC to the linked MLC.
              mlcsymetric(iwhich)=mlcname(ifoc)  ! update array

C Extract strings from focus construction prior to updating its SYM tag.
              mlcsymetric(ifoc)=mlcname(iwhich)  ! update array
              matsymindex(ifoc)=iwhich   ! remember its array position
              matsymindex(iwhich)=ifoc   ! update other to point to this one
              moddb = .true.
              showother = .true.
            else
              write(outs,'(5a)') 'Constructions ',
     &          mlcname(IFOC)(1:lnmlcname(IFOC)),
     &          ' and ',mlcname(iwhich)(1:lnmlcname(iwhich)),
     &          ' have different numbers of layers.'
              call edisp(iuout,outs)
            endif
          else

C User declines link to recast mlcsymetric and update MLC for the current
C construction and reset showother.
            call ismlcsymmetric(ifoc,layermatch)
            if(layermatch)then
              mlcsymetric(ifoc)='SYMMETRIC   '
            else
              mlcsymetric(ifoc)='NONSYMMETRIC'
            endif

            moddb = .true.
            showother = .false.
          endif
        endif
      ELSEIF(ILED.GT.8.AND.ILED.LE.NLED-3)THEN

C Ask if user wishes to browse through materials to find suitable
C reference.
        dok=.true.
        helptopic='edit_wall_details'
        call gethelptext(helpinsub,helptopic,nbhelp)

        check_CFC2_or_MLC_6:
     &  IF(mlctype(ifoc)(1:4).EQ.'CFC2')THEN

          CALL ASKOK(
     &    'Change the CFC layer reference via browsing available',
     &    'CFC layers?(NO keeps the current CFC layer)',OK,dok,nbhelp)
          if(OK)then
            iwhich=0
            CALL ELISTCFC(iwhich,chdb,'-',cfcarrayindex,IER)
          else
            cfcarrayindex=ITMCFCDB(IFOC,ILED-11)   ! and the CFC layer index
          endif

C Make up LAYDESC and PNAM as combination of cfcnam and cfcdoc based on
C the newly selected CFC layer. This is necessary so that other
C code can recover the details of a CFC layer item and to update the
C menu display.
          if(cfcarrayindex.gt.0)then
            lnam=lnblnk(cfcname(cfcarrayindex))
            lfordoc = 72 - (lnam +4)    ! space left for doc
            write(LAYDESC(IFOC,ILED-11),'(3a)') 
     &        cfcname(cfcarrayindex)(1:lnam),' : ',
     &        cfcdoc(cfcarrayindex)(1:lfordoc)
            write(PNAM(ILED-11),'(3a)')
     &        cfcname(cfcarrayindex)(1:lnam),' : ',
     &        cfcdoc(cfcarrayindex)(1:lfordoc)
            ITMCFCDB(IFOC,ILED-11)=cfcarrayindex ! update array index for this layer
          endif

          VAL = cfcdbthick(cfcarrayindex) ! default layer thickness (mm)

        ELSE ! check_CFC2_or_MLC_6

          CALL ASKOK(
     &    'Change the material reference via browsing available',
     &    'materials? (NO keeps the current material)',OK,dok,nbhelp)
          if(OK)then
            iwhich=0
            CALL EPMENSV
            CALL ELISTMAT(iwhich,chdb,'-',matarrayindex,IER)
            CALL EPMENRC

C If iwhich came back as a zero then the material actually was
C the traditional gap so reset matarrayindex.
            if(iwhich.eq.0)then
              matarrayindex=0 
C Debug.
C              write(6,*) 'reset matarrayindex for legacy gap',
C     &          matarrayindex,iwhich
            endif
            if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &         matopaq(matarrayindex)(1:1).eq.'h'.or.
     &         matarrayindex.eq.0)then
              CALL EASKABC('Your selection is a gap. Options:',
     &        ' ','accept','reselect material','abort',iwair,nbhelp)
              if(iwair.eq.2)then
                CALL EPMENSV
                CALL ELISTMAT(iwhich,chdb,'-',matarrayindex,IER)
                CALL EPMENRC
                if(iwhich.eq.0)then
                  matarrayindex=0   ! a gap assumption
                endif
              elseif(iwair.eq.3)then
                ILED=-1
                GOTO 3
              endif
              IPRMAT(IFOC,ILED-11)=matarrayindex   ! set zero material index to layer
            elseif(matarrayindex.lt.0)then
              continue  ! confused material
            elseif(matarrayindex.gt.0)then
              IPR(IFOC,ILED-11)=iwhich  ! 
              IPRMAT(IFOC,ILED-11)=matarrayindex   ! set zero material index to layer
            endif
          else
            IWHICH=IPR(IFOC,ILED-11)  ! keep the current material
            matarrayindex=IPRMAT(IFOC,ILED-11)   ! and the material index
          endif

C Make up LAYDESC and PNAM as combination of matnam and matdoc based on
C the newly selected material. This is necessary so that other
C code can recover the details of a -99 item and to update the
C menu display.
          if(matarrayindex.gt.0)then
            lnam=lnblnk(matname(matarrayindex))
            lfordoc = 72 - (lnam +4)    ! space left for doc
            write(LAYDESC(IFOC,ILED-11),'(3a)') 
     &        matname(matarrayindex)(1:lnam),' : ',
     &        matdoc(matarrayindex)(1:lfordoc)
            write(PNAM(ILED-11),'(3a)')
     &        matname(matarrayindex)(1:lnam),' : ',
     &        matdoc(matarrayindex)(1:lfordoc)
            IPRMAT(IFOC,ILED-11)=matarrayindex ! update array index for this layer
          elseif(matarrayindex.eq.0)then
            IPRMAT(IFOC,ILED-11)=matarrayindex  ! update array index for air gap
          endif

C If a gap then ask for gap resistance, recover resistance if newer matereials file.
          if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &       matopaq(matarrayindex)(1:1).eq.'h'.or.
     &       matarrayindex.eq.0)then
            if(matarrayindex.eq.0)then
              guessthick=0.1
            else
              guessthick=matdbthick(matarrayindex)
            endif
            if(closemat2)then
              DRAIR(IFOC,ILED-11,1)=matgapares(matarrayindex,1)
              DRAIR(IFOC,ILED-11,2)=matgapares(matarrayindex,2)
              DRAIR(IFOC,ILED-11,3)=matgapares(matarrayindex,3)
            endif

            WRITE(outs,118)mlcname(IFOC)(1:lnmlcname(ifoc))  
  118       FORMAT('Default R value for gap in ',a)
            if(DRAIR(IFOC,ILED-11,1).lt.0.001)then
              VAL=0.17
            else
              VAL=DRAIR(IFOC,ILED-11,1)
            endif
            CALL EASKR(VAL,outs,
     &        ' if the orientation is vertical          : ',
     &        0.0,'W',99.9,'W',0.17,'default gap R',IER,nbhelp)
            DRAIR(IFOC,ILED-11,1)=VAL
            if(DRAIR(IFOC,ILED-11,2).lt.0.001)then
              VAL=0.17
            else
              VAL=DRAIR(IFOC,ILED-11,2)
            endif
            CALL EASKR(VAL,outs,
     &        ' if the orientation is horizontal        : ',
     &        0.0,'W',99.9,'W',0.17,'default gap R',IER,nbhelp)
            DRAIR(IFOC,ILED-11,2)=VAL
            if(DRAIR(IFOC,ILED-11,3).lt.0.001)then
              VAL=0.17
            else
              VAL=DRAIR(IFOC,ILED-11,3)
            endif
            CALL EASKR(VAL,outs,
     &        ' if the orientation is sloped or UNKNOWN : ',
     &        0.0,'W',99.9,'W',0.17,'default gap R',IER,nbhelp)
            DRAIR(IFOC,ILED-11,3)=VAL

C Now update LAYDESC & PNAM.
            write(LAYDESC(IFOC,ILED-11),'(A,3F5.2)')'gap ',
     &           (DRAIR(IFOC,ILED-11,IY),IY=1,3)
            write(PNAM(ILED-11),'(A,3F5.2)')'gap ',
     &           (DRAIR(IFOC,ILED-11,IY),IY=1,3)
    
          ELSEIF(matarrayindex.lt.0)THEN

C This is a confused material (perhaps a duplicate). Assign zero
C for air gap resistances and 0.1m estimated thickness.
            DRAIR(IFOC,ILED-11,1)=0.0
            DRAIR(IFOC,ILED-11,2)=0.0
            DRAIR(IFOC,ILED-11,3)=0.0
            guessthick=0.1
          ELSE

C Non zero legacy index so assign thickness. And check if the
C material attribute is a [g] and might have gap R values.
            DRAIR(IFOC,ILED-11,1)=0.0
            DRAIR(IFOC,ILED-11,2)=0.0
            DRAIR(IFOC,ILED-11,3)=0.0
            if(matarrayindex.eq.0)then
              guessthick=0.1
            else
              guessthick=matdbthick(matarrayindex)
            endif
            write(outs,'(a,f6.1,a)') 
     &        'Material selected has a thickness of ',guessthick,'mm.'
            call edisp(iuout,outs)
            if(matopaq(matarrayindex)(1:1).eq.'g')then
              if(closemat2)then
                DRAIR(IFOC,ILED-11,1)=matgapares(matarrayindex,1)
                DRAIR(IFOC,ILED-11,2)=matgapares(matarrayindex,2)
                DRAIR(IFOC,ILED-11,3)=matgapares(matarrayindex,3)

C Now update LAYDESC & PNAM.
                write(LAYDESC(IFOC,ILED-11),'(A,3F5.2)')'gap ',
     &            (DRAIR(IFOC,ILED-11,IY),IY=1,3)
                write(PNAM(ILED-11),'(A,3F5.2)')'gap ',
     &            (DRAIR(IFOC,ILED-11,IY),IY=1,3)
              endif
            endif
          ENDIF

          VAL=DTHK(IFOC,ILED-11)*1000.0

        ENDIF check_CFC2_or_MLC_6

        CALL EASKR(VAL,' ',' Layer thickness (mm): ',
     &    1.0,'W',300.0,'W',100.0,'layer thickness mm',IER,nbhelp)
        DTHK(IFOC,ILED-11)=VAL*0.001
        moddb = .true.
        goto 2   ! re-establish the menu strings
      else
        ILED=-1
        GOTO 3
      endif
      call usrmsg(' ',' ','-')
      ILED=-4
      GOTO 2
   
      END


C ******* mlcrefs
C mlcrefs returns true/false if a construction name is referenced within
C the current zone scope. areamlc(MCOM) is the area of this construction
C in each zone, tareamlc is the total area of thie construction (weighted
C so that partitions are counted as half area).
      subroutine mlcrefs(mlcname,areamlc,tareamlc,found)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      character mlcname*32
      logical found
      dimension areamlc(MCOM)

C For each of the selected zones check each surface mlc attribute
C for a match against mlcname.
      found=.false.
      if(mlcname(1:4).ne.'UNKN'.and.nzg.gt.0)then
        lm=lnblnk(mlcname)
        tareamlc=0.0
        do 42 i=1,nzg
          iz=nznog(i)
          areamlc(iz)=0.0
          do 43 j=1,NZSUR(iz)
            ic=IZSTOCN(iz,j)
            lnsmlcn=lnblnk(SSMLCN(ic))
            if(mlcname(1:lm).eq.SSMLCN(ic)(1:lnsmlcn))then
              found=.true.

C Depending on the connection type, add the full or half area of surface.
              if(ICT(ic).eq.0.or.ICT(ic).eq.1.or.ICT(ic).eq.2)then
                areamlc(iz)=areamlc(iz)+SSNA(ic)
                tareamlc=tareamlc+SSNA(ic)
              elseif(ICT(ic).eq.4.or.ICT(ic).eq.5.or.ICT(ic).eq.3)then
                areamlc(iz)=areamlc(iz)+SSNA(ic)
                tareamlc=tareamlc+SSNA(ic)
              else
                areamlc(iz)=areamlc(iz)+(SSNA(ic)*0.5)
                tareamlc=tareamlc+(SSNA(ic)*0.5)
              endif
            endif
  43      continue
  42    continue
      endif

      return
      end
      
C **************************************
C Calculate g-value optical properties of systems given info on 
C individual glazing layers. In accordance with BS EN 410:1998

C << update to use single layer optical data common block data. >>

      SUBROUTINE GVALUE(IPN,TRNT,REF,OREF,OEMS,CLAMDA,G)

C MPN is max number of tranparent glazings per multilayer construction
C If this parameter is changed also change it in subroutine EDMLDB
      PARAMETER(MPN=3)
      REAL TRNT(MPN),REF(MPN),OREF(MPN),OEMS(MPN)

      HE=23.
      HI=3.6+4.4*OEMS(IPN)/0.837
*  std CIBSE value is HI=8.
      IF(IPN.EQ.1)THEN
        TR=TRNT(1)
        AE=1.-TRNT(1)-REF(1)
        QI=AE*HI/(HE+HI)
        G=TR+QI
      ELSEIF(IPN.EQ.2)THEN
        T1=TRNT(1)
        R1=REF(1)
        OR1=OREF(1)
        T2=TRNT(2)
        R2=REF(2)
        OR2=OREF(2)
        A1=1.-T1-R1
        A2=1.-T2-R2
        OA1=1.-T1-OR1

C Following formula may not be the correct one 
C        AE1=OA1+A1*T1*R2/(1.-OR1*R2)
        AE1=OA1+A1*T1*R2/(1.-R1*R2)
        AE2=A2*T1/(1.-OR1*R2)
        QI=(AE1/HE+AE2/HE+AE2/CLAMDA)/(1./HI+1./HE+1./CLAMDA)

C Following formula may not be the correct one 
C        TR=T1*T2/(1.-OR1*R2)
        TR=T1*T2/(1.-R1*R2)
        G=TR+QI
      ELSEIF(IPN.EQ.3)THEN
        CLAMDA12=CLAMDA/2.
        CLAMDA23=CLAMDA/2.

C<< Possible future development is to take thermal conductance from mlc
C properties and not simply assuming symmetry
        T1=TRNT(1)
        R1=REF(1)
        OR1=OREF(1)
        T2=TRNT(2)
        R2=REF(2)
        OR2=OREF(2)
        T3=TRNT(3)
        R3=REF(3)
*        OR3=OREF(3)
        A1=1.-T1-R1
        A2=1.-T2-R2
        A3=1.-T3-R3
        OA1=1.-T1-OR1
        OA2=1.-T2-OR2
*        OA3=1.-T3-OR3
        AE1=A1+(T1*OA1*R2*(1.-OR2*R3)+T1*T2**2.*OA1*R3)/
     &      ((1.-OR1*R2)*(1.-OR2*R3)-(T2**2.*OR1*R3))
        AE2=(T1*A2*(1.-OR2*R3)+T1*T2*OA2*R3)/
     &      ((1.-OR1*R2)*(1.-OR2*R3)-(T2**2.*OR1*R3))
        AE3=(T1*T2*A3)/
     &      ((1.-OR1*R2)*(1.-OR2*R3)-(T2**2.*OR1*R3))
        QI=(AE3/CLAMDA23+(AE3+AE2)/CLAMDA12+(AE3+AE2+AE1)/HE)/
     &     (1./HI+1./HE+1./CLAMDA23+1./CLAMDA12)
        TR=T1*T2*T3/((1.-OR1*R2)*(1.-OR2*R3)-(T2**2.*OR1*R3))
        G=TR+QI
      ELSE
        G=0.0
      ENDIF
      RETURN
      END

C ************* allmat
C allmat presents a list of all materials to select from.
      subroutine allmat(imatarrayindex,ier)
#include "epara.h"
#include "building.h"
#include "esprdbfile.h"
#include "material.h"

C Parameters
      integer imatarrayindex ! is the index in matdatarray

C      COMMON/OUTIN/IUOUT,IUIN
      DIMENSION VERT(35)
      CHARACTER VERT*80,KEY*1
C      character outs*124
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='allmat'  ! set for subroutine

C Set to zero first.
      imatarrayindex=0

C Initialise materials menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      MHEAD=2
      MCTL=3
      ILEN=matdbitems
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=matdbitems
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          if(matopaq(L).eq.'o')then
            WRITE(VERT(M),24)KEY,matdbcon(L),matdbden(L),
     &        matdbsht(L),matdboute(L),matdbouta(L),matdbdrv(L),
     &        matname(L)(1:32)
   24       FORMAT(A1,F8.3,F6.0,F7.0,F5.2,F5.2,F7.0,1X,A)
          elseif(matopaq(L).eq.'-')then
            WRITE(VERT(M),25)KEY,matdbcon(L),matdbden(L),
     &        matdbsht(L),matdboute(L),matdbouta(L),matdbdrv(L),
     &        matname(L)(1:12),': ',matdoc(L)(1:25)
   25       FORMAT(A1,F8.3,F6.0,F7.0,F5.2,F5.2,F7.0,1X,3A)
          elseif(matopaq(L).eq.'t')then
            WRITE(VERT(M),25)KEY,matdbcon(L),matdbden(L),
     &        matdbsht(L),matdboute(L),matdbouta(L),matdbdrv(L),
     &        matname(L)(1:12),': ',matdoc(L)(1:25)
          elseif(matopaq(L).eq.'g')then
            WRITE(VERT(M),25)KEY,matdbcon(L),matdbden(L),
     &        matdbsht(L),matdboute(L),matdbouta(L),matdbdrv(L),
     &        matname(L)(1:12),': ',matdoc(L)(1:25)
          elseif(matopaq(L).eq.'h')then
            WRITE(VERT(M),25)KEY,matdbcon(L),matdbden(L),
     &        matdbsht(L),matdboute(L),matdbouta(L),matdbdrv(L),
     &        matname(L)(1:12),': ',matdoc(L)(1:25)
          else
            WRITE(VERT(M),24)KEY,matdbcon(L),matdbden(L),
     &        matdbsht(L),matdboute(L),matdbouta(L),matdbdrv(L),
     &        matname(L)(1:32)
          endif
        ENDIF
   10 CONTINUE

      VERT(1)=
     &  ' |Conduc-|Den- |Specif|IR  |Solr|Diffu|Description  '
      VERT(2)=
     &  ' |tivity |sity |heat  |emis|abs |resis|of material  '

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        VERT(M+1)='  ______________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      VERT(M+2)  ='? help                           '
      VERT(M+3)  ='- exit this menu                 '

C Help text for this menu.
      helptopic='select_from_materials'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      CALL EMENU(' Materials in all categories',VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C List help text for the vertex menu.
        helptopic='select_from_materials'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('materials section',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Material identified by KEYIND as ifoc.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        imatarrayindex=ifoc
C        L=ifoc
C        call edisp(iuout,'Selected:')
C        WRITE(outs,27)L,matdbcon(L),matdbden(L),
C     &    matdbsht(L),matdboute(L),matdbouta(L),matdbdrv(L),
C     &    matname(L)(1:24)
C   27   FORMAT(I3,F8.3,F6.0,F7.0,F5.2,F5.2,F7.0,1X,A)
C        call edisp(iuout,outs)

        return

      ELSE

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END 

