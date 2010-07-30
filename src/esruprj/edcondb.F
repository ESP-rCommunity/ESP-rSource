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
C  EDONEMAT: Edits a material in the common block at array index index.
C  EDMLDB: Display/edit a constructions db.
C  EDWALL: Edit construction common block data.
C  MKBPRC: Creates a binary materials db based on the
C          current contents of an ASCII file. 
C  MKAPRC: Creates an ascii materials db based on the
C          current contents of binary db.
C  MLCREFS: returns true/false if mlc referenced within current zone scope.

C ************* ELISTMAT 
C ELISTMAT: Controls display and editing of materials commons.
C If ACTION = 'M' then include editing if ACTION = '-' only
C allow choice.
      SUBROUTINE ELISTMAT(iwhich,chgdb,ACTION,imatarrayindex,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer iwhich         ! is the legacy database index
      logical chgdb          ! flagged true if an mod made to db during session
      integer imatarrayindex ! is the index in matdatarray

      common/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit
      LOGICAL OK,DOK,MODDB

C CLSDES (30 char) is an array for menu items listing each class.
C CLSSEL (36 char) is an array for menu items for the selected class.
      DIMENSION CLSDES(30),PDBM(35),CLSSEL(30),ICLSSEL(30)
      integer iwhicharray   ! for each item in menu point back to array
      dimension iwhicharray(60)
      character ACTION*1,CLSSEL*36,lltmp*144,CLSDES*32,H*72
      CHARACTER KEY*1,PDBM*68,outs*124
      CHARACTER xfile*144,tg*1,delim*1,t32*32,t248*248
      character lworking*144,fs*1
      logical closemat1,closemat2
      logical mod   ! to signal whether an item has been altered.
      logical unixok
      real per1,per2   ! percentages of material for non-homogeneous layer
      integer iwhich2  ! 2nd material array index
      integer idb,idb2 ! the two legacy indices selected

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

C Setup for menu.
C Gather the names of the various classes and present this list.
  40  ICO=-1
      CLSDES(1)=  '  Description         No. Items'
      M=1
      DO 44 I=1,matcats
        write(CLSSEL(I),'(A)') matcatname(I)(1:32)
        IF(matcatitems(I).GT.0)THEN
          M=M+1
          CALL EMKEY(M-1,KEY,IER)
          WRITE(CLSDES(M),'(A1,1X,A,1X,I2)')KEY,matcatname(I)(1:26),
     &      matcatitems(I)
        ENDIF
   44 CONTINUE

      CLSDES(M+1)=  '  __________________________  '
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

C Help text for this menu.
      H(1)='The classifications shown in the display include up to'
      H(2)='18 materials. To get a listing of the items included'
      H(3)='select one of the classifications.'

C Now display the menu.
      CALL EMENU('Materials Classes',CLSDES,NCO,ICO)
      IF(ICO.EQ.NCO)THEN
        RETURN
      ELSEIF(ICO.EQ.(NCO-1))THEN
        CALL PHELPD('materials menu',3,'-',0,0,IER)
      ELSEIF(ICO.EQ.(NCO-2))THEN

C List one or more classifications.
        INPIC=matcats
        h(1)='You may select one or more classes to list. '
        CALL EPICKS(INPIC,ICLSSEL,' ',' Which classes to list:',
     &        36,matcats,CLSSEL,'Material classes',IER,1)
        IF(INPIC.EQ.0)GOTO 40
        h(1)=' Summary reporting can be shown in the text feedback'
        h(2)=' or a text file.'
        CALL EASKABC(' Reporting to:',' ',
     &      'text feedback','summary file','cancel',irpt,2)
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
        do 42 list=1,INPIC
          IC=ICLSSEL(list)
          IF(matcatitems(IC).GT.0)THEN
            WRITE(outs,'(3a,i2,a)')' Classification: ',
     &        matcatname(IC)(1:lnblnk(matcatname(IC))),' (',IC,')'
            call edisp(itu,outs)
            call edisp(itu,' ')
            call edisp(itu,
     &      'Index|Con-   |Den- |Specif|IR  |Solr|Vapour|Description')
            call edisp(itu,
     &      '     |duct.  |sity |heat  |emis|abs |resist|of material')

C Loop through all of the items in the array and list out those that are
C associated with this class.
            DO 47 J=1,matdbitems
              if(matcatindex(J).eq.IC)then
                IDB=matlegindex(J)
                WRITE(outs,46)IDB,matdbcon(J),matdbden(J),matdbsht(J),
     &            matdboute(J),matdbouta(J),matdbdrv(J),
     &            matname(J)(1:lnblnk(matname(J)))
   46           FORMAT(I4,F9.3,F6.0,F7.0,F5.2,F5.2,F7.0,1X,A)
                call edisp(itu,outs)
              endif
   47       CONTINUE
          ELSE
            call edisp(itu,' No items in this classification.')
          ENDIF
   42   continue
        call edisp(itu,' ')
        call edisp(itu,' Units: Conduct W/(m deg.C), Density kg/m**3')
        call edisp(itu,'        Specific Heat J/(kg deg.C) ')
        if(irpt.eq.2)then

C Call ctlexp a 2nd time to close the file.
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','prim db',IER)
        endif
      ELSEIF(ICO.EQ.(NCO-3))THEN

C Add another category to the database << not yet tested >>
        if(matcats.LT.30)then
          matcats=matcats+1
          H(1)='This description may be up to 32 char. '
          t32=' '
          CALL EASKS(t32,' Classification name (<32 char)?',' ',
     &      32,' ','class name',IER,1)
          write(matcatname(matcats),'(a)') t32
          write(matcatdoc(matcats),'(a,i2,3a)')
     &      'Category (',matcats,') named ',t32(1:lnblnk(t32)),
     &      ' was inserted manually. No other documentation (yet).'
          ILNE=matdbitems+1
          matname(ILNE)='new_material'
          matopaq(ILNE)='o'
          matdoc(ILNE)='no documentation (yet)'
          matcatindex(ILNE)=matcats
          matdbcon(ILNE)=1.0
          matdbden(ILNE)=1.0
          matdbsht(ILNE)=1.0
          matdboute(ILNE)=0.9
          matdbine(ILNE)=0.9
          matdbouta(ILNE)=0.5
          matdbina(ILNE)=0.5
          matdbdrv(ILNE)=10.0
          matdbthick(ILNE)=10.0
          matirtran(ILNE)=0.0
          matsoldrtrn(ILNE)=0.0
          matsoldrotrfl(ILNE)=0.5
          matsoldrinrfl(ILNE)=0.5
          matvistran(ILNE)=0.0
          matvisotrfl(ILNE)=0.5
          matvisinrfl(ILNE)=0.5
          matrender(ILNE)=0.0

          matgapcnd(ILNE,1)=0.0
          matgapcnd(ILNE,2)=0.0
          matgapcnd(ILNE,3)=0.0
          matgapcnd(ILNE,4)=0.0
          matvisco(ILNE,1)=0.0
          matvisco(ILNE,2)=0.0
          matvisco(ILNE,3)=0.0
          matvisco(ILNE,4)=0.0
          matgapden(ILNE,1)=0.0
          matgapden(ILNE,2)=0.0
          matgapden(ILNE,3)=0.0
          matgapden(ILNE,4)=0.0
          matprandtl(ILNE,1)=0.0
          matprandtl(ILNE,2)=0.0
          matgapsht(ILNE,1)=0.0
          matgapsht(ILNE,2)=0.0
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
        endif
        goto 40
      ELSEIF(ICO.GT.1.AND.ICO.LT.(NCO-4))THEN
        
C Loop through data for this classification for manipulation.
        IC=ICO-1
        IF(matcatitems(IC).EQ.0)THEN
         CALL USRMSG(' No items in this classification.',' ','W')
         GOTO 40
        ENDIF

C Display the materials of the classification in a menu.
    3   PDBM(1)= '  Units:  Conductivity W/(m deg.C), Density kg/m**3'
        PDBM(2)= '          Specific Heat J/(kg deg.C) '
        WRITE(PDBM(3),33)matcatname(IC)(1:lnblnk(matcatname(IC))),IC
   33   FORMAT    ('a Classification: ',A,' (',I2,')')
        WRITE(PDBM(4),'(A,I3)')'  Number of materials:',matcatitems(IC)
        PDBM(5)=   '  ___________________________________'
        PDBM(6)=
     &  ' |Conduc-|Den- |Specif|IR  |Solr|Diffu|Description  '
        PDBM(7)=
     &  ' |tivity |sity |heat  |emis|abs |resis|of material  '
        M=7
        DO 30 J=1,matdbitems

C Loop through all of the items in the array and list out those that are
C associated with this class.
          if(matcatindex(J).eq.IC)then

C Remember the array index that goes with the menu position (m).
            M=M+1
            iwhicharray(M)=J  ! menu position M relates to data array J.
            CALL EMKEY(M-6,KEY,IER)
            if(matopaq(J).eq.'o')then
              WRITE(PDBM(M),24)KEY,matdbcon(J),matdbden(J),
     &          matdbsht(J),matdboute(J),matdbouta(J),matdbdrv(J),
     &          matname(J)(1:24)
   24         FORMAT(A1,F8.3,F6.0,F7.0,F5.2,F5.2,F7.0,1X,A)
            elseif(matopaq(J).eq.'-')then
              WRITE(PDBM(M),25)KEY,matdbcon(J),matdbden(J),
     &          matdbsht(J),matdboute(J),matdbouta(J),matdbdrv(J),
     &          matname(J)(1:8),': ',matdoc(J)(1:17)
   25         FORMAT(A1,F8.3,F6.0,F7.0,F5.2,F5.2,F7.0,1X,3A)
            elseif(matopaq(J).eq.'t')then
              WRITE(PDBM(M),25)KEY,matdbcon(J),matdbden(J),
     &          matdbsht(J),matdboute(J),matdbouta(J),matdbdrv(J),
     &          matname(J)(1:8),': ',matdoc(J)(1:17)
            elseif(matopaq(J).eq.'g')then
              WRITE(PDBM(M),25)KEY,matdbcon(J),matdbden(J),
     &          matdbsht(J),matdboute(J),matdbouta(J),matdbdrv(J),
     &          matname(J)(1:8),': ',matdoc(J)(1:17)
            elseif(matopaq(J).eq.'h')then
              WRITE(PDBM(M),25)KEY,matdbcon(J),matdbden(J),
     &          matdbsht(J),matdboute(J),matdbouta(J),matdbdrv(J),
     &          matname(J)(1:8),': ',matdoc(J)(1:17)
            else
              WRITE(PDBM(M),24)KEY,matdbcon(J),matdbden(J),
     &          matdbsht(J),matdboute(J),matdbouta(J),matdbdrv(J),
     &          matname(J)(1:24)
            endif
          endif
   30   CONTINUE
        NC=M
        PDBM(NC+1)=  '  __________________________     '
        if(ACTION.eq.'M'.or.ACTION.eq.'m')then
          PDBM(NC+2)='1 add/ delete material            '
          PDBM(NC+3)='2 copy material                   '
          PDBM(NC+4)='! save materials database        '
        elseif(ACTION.eq.'-')then
          PDBM(NC+2)='                                 '
          PDBM(NC+3)='                                 '
          PDBM(NC+4)='                                 '
        endif
        PDBM(NC+5)=  '? help                           '
        PDBM(NC+6)=  '- exit this menu                 '
        NITMS=NC+6
        INO=-4

C Help text for this menu.
    2   continue
        H(1)='The information in this display is based on data from'
        H(2)='a materials database.  Databases of this'
        H(3)='type are used to build a construction database.'
        H(4)='Data in this db rarely changes. However, users may '
        H(5)='wish to add additional materials or classifications to'
        H(6)='conform to materials in their region.  '
        H(7)='  '
        H(8)='Adding or Deleting a material is supported. The'
        H(9)='former creates a material at the end of the current'
        H(10)='classification which may then be edited.  You have'
        H(11)='the choice of manually supplying the data or to'
        H(12)='select two materials and give each a weighting'
        H(13)='(useful for approximating ISO 6946 non-homogeneous'
        H(14)='layers in constructions).'
        H(15)=' '
        H(16)='You may copy an existing material into a new item at'
        H(17)='the end of the list. BE SURE TO ALTER ITS NAME! '
 
        CALL EMENU('Materials Database',PDBM,NITMS,INO)

        IF(INO.EQ.NITMS.OR.INO.EQ.0)THEN
          IF(MODDB.and.ACTION.eq.'M')THEN

C Changes were made so remove the existing file via delfiledosorunix
C and write a new file based on current common blocks. If the original
C file was binary then alter the name of the saved file to add a '.a'
C at the end.
            dok=.true.
            h(1)='Recent changes to the materials database are held'
            h(2)='in memory and might be lost if not saved to file.'
            CALL ASKOK('Changes have been made in database. ',
     &        'Save the materials database changes?',OK,dok,2)
            IF(.NOT.OK)GOTO 4
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
              write(6,*) 'working materials ',lworking
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
          GOTO 4
        ELSEIF(INO.EQ.1.OR.INO.EQ.2)THEN
          GOTO 2
        ELSEIF(INO.eq.3)THEN

C Edit classification name.
          H(1)='A classification has a descriptive name (up to '
          H(2)='32 char).'
          write(t32,'(a)')matcatname(IC)(1:lnblnk(matcatname(IC)))
          CALL EASKS(t32,' ',' Classification name (<32 char)?',
     &      32,' ','class name',IER,2)
          if(t32(1:2).ne.'  ')then
            write(matcatname(IC),'(a)') t32(1:lnblnk(t32))
            chgdb=.true.
            MODDB=.TRUE.
          endif

C Also edit classification documentation.
          H(1)='Notes (<248 char) for this category of materials' 
          H(2)='are useful for descriptions that apply to all'
          H(3)='the materials.'
          t248=matdoc(IC)(1:lnblnk(matdoc(IC)))
          CALL EASKS248(t248,'Category notes:',' ',
     &      72,'this category...','category notes',IER,3)
          if(t248(1:2).ne.'  ')then
            write(matdoc(IC),'(a)') t248(1:lnblnk(t248))
            chgdb=.true.
            MODDB=.TRUE.
          endif
        ELSEIF(INO.GE.4.AND.INO.LE.7)THEN
          GOTO 2
        ELSEIF(INO.EQ.NITMS-1)THEN
          CALL PHELPD('materials database',17,'-',0,0,IER)
        ELSEIF(INO.EQ.NITMS-2)THEN

C Save materials db with all materials. First delete the
C existing file via delfiledosorunix and then write
C a new file via mkascimat.
          IF(.NOT.MODDB)THEN
            CALL USRMSG(' ',' No changes in db...','-')
          ELSE
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
C              call delfiledosorunix(lltmp,ider)
              CALL mkascimat(IAF,lltmp,IER)
            elseif(ipathmat.eq.2)then
              lndbp=lnblnk(standarddbpath)
              write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &          LFMAT(1:lnblnk(LFMAT))
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
          ENDIF
        ELSEIF(INO.EQ.NITMS-3)THEN

C Copy an existing material to a new one at end of the array.
C Use iwhicharray to go from the menu position to the array
C end of the array.  
          CALL USRMSG(' ',' Which item of list?','-')
          CALL EMENU('Copy material',PDBM,NITMS,IW)
          IWHICH=iwhicharray(IW)
          ILNE=matdbitems+1

C See if there is an available slot in the 0-600 list of legacy
C indices. If so make up the initial name but allow the user to
C edit it. If not then assign -99 and make up initial name based
C on the original.
          ip=matlegindex(IWHICH)
          call getnextascislot(ip,inext)
          if(inext.le.600)then 
            write(matname(ILNE),'(a,i3.3)') 'mat_',inext
            matlegindex(ILNE)=inext
            mathash(inext)=ILNE
          else
            write(matname(ILNE),'(2a)') 
     &        matname(IWHICH)(1:lnblnk(matname(IWHICH))),'c'
            matlegindex(ILNE)=-99
          endif

C If there is room add a note about its origin.
          ils=lnblnk(matdoc(IWHICH))
          iln=lnblnk(matname(IWHICH))
          if(ils.le.200)then
            write(matdoc(ILNE),'(4a)') 
     &        matdoc(IWHICH)(1:ils),' (copy of ',
     &        matname(IWHICH)(1:iln),')'
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

          matgapcnd(ILNE,1)=matgapcnd(IWHICH,1)
          matgapcnd(ILNE,2)=matgapcnd(IWHICH,2)
          matgapcnd(ILNE,3)=matgapcnd(IWHICH,3)
          matgapcnd(ILNE,4)=matgapcnd(IWHICH,4)
          matvisco(ILNE,1)=matvisco(IWHICH,1)
          matvisco(ILNE,2)=matvisco(IWHICH,2)
          matvisco(ILNE,3)=matvisco(IWHICH,3)
          matvisco(ILNE,4)=matvisco(IWHICH,4)
          matgapden(ILNE,1)=matgapden(IWHICH,1)
          matgapden(ILNE,2)=matgapden(IWHICH,2)
          matgapden(ILNE,3)=matgapden(IWHICH,3)
          matgapden(ILNE,4)=matgapden(IWHICH,4)
          matprandtl(ILNE,1)=matprandtl(IWHICH,1)
          matprandtl(ILNE,2)=matprandtl(IWHICH,2)
          matgapsht(ILNE,1)=matgapsht(IWHICH,1)
          matgapsht(ILNE,2)=matgapsht(IWHICH,2)
          matdbitems=matdbitems+1
          matcatitems(IC)=matcatitems(IC)+1
          MODDB=.TRUE.
          chgdb=.true.
        ELSEIF(INO.EQ.NITMS-4)THEN

C Add or delete and material from db.
          h(1)='You cannot recover a deleted material. You can add a'
          h(2)='homogeneous or non-homogeneous material (type in the'
          h(3)='new thermophysical values or you can derive a new'
          h(4)='non-homogeneous material (repeating thermal bridges)'
          h(5)='based on percentages of two existing materials for '
          h(6)='use with ISO 6946:2007 section 6.2.4 lower limit '
          h(7)='calculations. '
          h(8)=' '
          h(9)='Note: currently the derivation only works with two'
          h(10)='solid materials. If air is in the layer then its'
          h(11)='air gap resistance needs to be converted into an'
          h(12)='equivalent thermal conductivity. The 6946 spec'
          h(13)='applies to pairs of materials which do not differ'
          h(14)='greatly in conductivity and thus is not applicable'
          h(15)='for metal & insulation combinations. '
          h(16)=' '
          h(17)='The conductivity, density, spedific heat and vapour'
          h(18)='properties will be weighted from the two sources.'
          CALL EASKABCD(' ','Options: ','Delete existing material',
     &      'Add material','Derive non-homogeneous material',
     &      'continue',IW,18)
          IF(IW.EQ.1)THEN

C Ask the user which one to remove then loop through materials and
C copy the contents of material > IWHICH into the next lower one.
            CALL USRMSG(' ',' Which item of list? ','-')
            CALL EMENU('delete material',PDBM,NITMS,IW)
            IWHICH=iwhicharray(IW)
            ip=matlegindex(IWHICH)  ! remove legacy index from the hash table.
            if(ip.gt.0) mathash(ip)=-1
            DO 791 IVV=IWHICH,matdbitems-1
              matlegindex(IVV)=matlegindex(IVV+1)
              matdbcon(IVV)=matdbcon(IVV+1)
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

              matgapcnd(IVV,1)=matgapcnd(IVV+1,1)
              matgapcnd(IVV,2)=matgapcnd(IVV+1,2)
              matgapcnd(IVV,3)=matgapcnd(IVV+1,3)
              matgapcnd(IVV,4)=matgapcnd(IVV+1,4)
              matvisco(IVV,1)=matvisco(IVV+1,1)
              matvisco(IVV,2)=matvisco(IVV+1,2)
              matvisco(IVV,3)=matvisco(IVV+1,3)
              matvisco(IVV,4)=matvisco(IVV+1,4)
              matgapden(IVV,1)=matgapden(IVV+1,1)
              matgapden(IVV,2)=matgapden(IVV+1,2)
              matgapden(IVV,3)=matgapden(IVV+1,3)
              matgapden(IVV,4)=matgapden(IVV+1,4)
              matprandtl(IVV,1)=matprandtl(IVV+1,1)
              matprandtl(IVV,2)=matprandtl(IVV+1,2)
              matgapsht(IVV,1)=matgapsht(IVV+1,1)
              matgapsht(IVV,2)=matgapsht(IVV+1,2)
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

C If there is room add a note about its origin.
                ils=lnblnk(matdoc(lastcatitem))
                iln=lnblnk(matname(lastcatitem))
                if(ils.le.200)then
                  write(matdoc(ILNE),'(4a)') 
     &              matdoc(IWHICH)(1:ils),' (derived from ',
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

                matgapcnd(ILNE,1)=matgapcnd(lastcatitem,1)
                matgapcnd(ILNE,2)=matgapcnd(lastcatitem,2)
                matgapcnd(ILNE,3)=matgapcnd(lastcatitem,3)
                matgapcnd(ILNE,4)=matgapcnd(lastcatitem,4)
                matvisco(ILNE,1)=matvisco(lastcatitem,1)
                matvisco(ILNE,2)=matvisco(lastcatitem,2)
                matvisco(ILNE,3)=matvisco(lastcatitem,3)
                matvisco(ILNE,4)=matvisco(lastcatitem,4)
                matgapden(ILNE,1)=matgapden(lastcatitem,1)
                matgapden(ILNE,2)=matgapden(lastcatitem,2)
                matgapden(ILNE,3)=matgapden(lastcatitem,3)
                matgapden(ILNE,4)=matgapden(lastcatitem,4)
                matprandtl(ILNE,1)=matprandtl(lastcatitem,1)
                matprandtl(ILNE,2)=matprandtl(lastcatitem,2)
                matgapsht(ILNE,1)=matgapsht(lastcatitem,1)
                matgapsht(ILNE,2)=matgapsht(lastcatitem,2)
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
              GOTO 3
            ENDIF
          ELSEIF(IW.EQ.3)THEN

C Derive non-homogeneous properties from two solid materials.
C Copy info from first existing material.  
            CALL USRMSG(' ','Select first material?','-')
            call allmat(imatarrayindex,ier)
            if(imatarrayindex.gt.0)then
              IWHICH=imatarrayindex
            else
              CALL USRMSG(' ','No first material selected!','W')
            GOTO 3
            endif
          
            H(1)='Assuming there are two materials used in a layer of'
            H(2)='a construction you will be asked for the percentage'
            H(3)='(1-99%) for the first and second materials. '
            PER1=90.0
            CALL EASKR(PER1,' ','First material % in the layer: ',
     &            0.1,'W',99.9,'W',1.,'1st material %',IER,3)
            PER1=PER1*0.01

            call edisp(iuout,'First material: ')
            call edisp(iuout,
     &      'Index|Con-   |Den- |Specif|IR  |Solr|Vapour|Description')
            call edisp(iuout,
     &      '     |duct.  |sity |heat  |emis|abs |resist|of material')
            IDB=matlegindex(IWHICH)
            WRITE(outs,46)IDB,matdbcon(IWHICH),matdbden(IWHICH),
     &        matdbsht(IWHICH),
     &        matdboute(IWHICH),matdbouta(IWHICH),matdbdrv(IWHICH),
     &        matname(IWHICH)(1:lnblnk(matname(IWHICH)))
            call edisp(iuout,outs)

            CALL USRMSG(' ','Select second material?','-')
            call allmat(imatarrayindex,ier)
            if(imatarrayindex.gt.0)then
              IWHICH2=imatarrayindex
            else
              CALL USRMSG(' ','No second material selected!','W')
              GOTO 3
            endif
          
            H(1)='Assuming there are two materials used in a layer of'
            H(2)='a construction you will be asked for the percentage'
            H(3)='(1-99%) for the first and second materials. '
            PER2=10.0
            CALL EASKR(PER2,' ','Second material % in the layer: ',
     &            0.1,'W',99.9,'W',1.,'2nd material %',IER,3)
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

C If there is room add a note about its origin.
              ils=lnblnk(matdoc(IWHICH))
              iln=lnblnk(matname(IWHICH))
              iln2=lnblnk(matname(IWHICH2))
              if(ils.le.200)then
                write(matdoc(ILNE),'(5a,2f5.2,a)') ' Derived from ',
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

              matgapcnd(ILNE,1)=matgapcnd(IWHICH,1)
              matgapcnd(ILNE,2)=matgapcnd(IWHICH,2)
              matgapcnd(ILNE,3)=matgapcnd(IWHICH,3)
              matgapcnd(ILNE,4)=matgapcnd(IWHICH,4)
              matvisco(ILNE,1)=matvisco(IWHICH,1)
              matvisco(ILNE,2)=matvisco(IWHICH,2)
              matvisco(ILNE,3)=matvisco(IWHICH,3)
              matvisco(ILNE,4)=matvisco(IWHICH,4)
              matgapden(ILNE,1)=matgapden(IWHICH,1)
              matgapden(ILNE,2)=matgapden(IWHICH,2)
              matgapden(ILNE,3)=matgapden(IWHICH,3)
              matgapden(ILNE,4)=matgapden(IWHICH,4)
              matprandtl(ILNE,1)=matprandtl(IWHICH,1)
              matprandtl(ILNE,2)=matprandtl(IWHICH,2)
              matgapsht(ILNE,1)=matgapsht(IWHICH,1)
              matgapsht(ILNE,2)=matgapsht(IWHICH,2)
              matdbitems=matdbitems+1   ! increment nb of db items
              matcatitems(IC)=matcatitems(IC)+1 ! increment nb of cat items
              matcatindex(ILNE)=IC      ! keep in current category
            endif
          ELSEIF(IW.EQ.4)THEN
            GOTO 3
          ENDIF
        ELSEIF(INO.EQ.NITMS-5)THEN
          GOTO 2
        ELSEIF(INO.GT.7.AND.INO.LT.NITMS-5)THEN

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
            call edisp(iuout,
     &        ' Units: Conduct W/(m deg.C), Density kg/m**3')
            call edisp(iuout,
     &        '        Specific Heat J/(kg deg.C) ')
            call edisp(iuout,' ')
            call edisp(iuout,
     &    'Index|Con-    |Den-  |Specif|IR  |Solar|Vapour|Description')
            call edisp(iuout,
     &    '     |duct.   |sity  |heat  |emis|abs  |resist|of material')
            WRITE(outs,244)matlegindex(IFOC),matdbcon(IFOC),
     &        matdbden(IFOC),matdbsht(IFOC),matdboute(IFOC),
     &        matdbouta(IFOC),matdbdrv(IFOC),matname(IFOC)(1:20)
  244       FORMAT(I5,F9.3,F7.1,F7.0,F5.2,F6.2,F7.0,1X,A)
            CALL EDISP(iuout,outs)
            CALL EDISP(iuout,' ')
            iwhich=matlegindex(ifoc)
            lsn=MIN0(lnblnk(matname(IFOC)),32)
            write(outs,'(a,i3,a,a)') ' Use material (',iwhich,') ',
     &        matname(IFOC)(1:lsn)
            dok=.true.
            h(1)='Last chance to abort the selection. '
            CALL ASKOK(outs,'as your selection ?',OK,dok,1)
            if(OK)then
              imatarrayindex=IFOC ! set to selected array index
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

      common/pophelp/h(60)

      DIMENSION PELM(29)
      CHARACTER H*72,PELM*40,tnam*32,tdoc*248,message*32
      real DBCON,DBDEN,DBSHT,EOUT,EIN,AOUT,AIN,DBDRV,THICK
      real IRTRAN,SOLDRTRN,soldrotrfl,soldrinrfl,vistran
      real visotrfl,visinrfl,render
      logical modify,dok,ok

C Local variables for gas materials.
      real gapcnd,visco,gapden,prandtl,gapsht
      dimension gapcnd(4),visco(4),gapden(4),prandtl(2),gapsht(2)

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
      gapcnd(1)= matgapcnd(index,1)
      gapcnd(2)= matgapcnd(index,2)
      gapcnd(3)= matgapcnd(index,3)
      gapcnd(4)= matgapcnd(index,4)
      visco(1)= matvisco(index,1)
      visco(2)= matvisco(index,2)
      visco(3)= matvisco(index,3)
      visco(4)= matvisco(index,4)
      gapden(1)= matgapden(index,1)
      gapden(2)= matgapden(index,2)
      gapden(3)= matgapden(index,3)
      gapden(4)= matgapden(index,4)
      prandtl(1)= matprandtl(index,1)
      prandtl(2)= matprandtl(index,2)
      gapsht(1)= matgapsht(index,1)
      gapsht(2)= matgapsht(index,2)

   3  IW=-4
      WRITE(PELM(1),'(A,1X,A)')    'a Name:',tnam(1:32)
      WRITE(PELM(2),'(A,1X,A)')    'b Note:',tdoc(1:32)
      WRITE(PELM(3),'(A,1X,F9.3)') 'c Conductivity (W/(m-K)  :',DBCON
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
        WRITE(PELM(12),'(A)')  'l type >>gas (data+temp correction)'
        WRITE(PELM(13),'(A,F8.5)') 'm gap conduct (W/(m-K)   :',
     &    gapcnd(1)
        WRITE(PELM(14),'(A,F9.6)') 'n gap conduct T deriv    :',
     &    gapcnd(2)
        WRITE(PELM(15),'(A,F11.8)')'o gap viscosity (kg/msK) :',visco(1)
        WRITE(PELM(16),'(A,F11.8)')'p gap viscosity T deriv  :',visco(2)
        WRITE(PELM(17),'(A,F8.4)') 'q gap density (kg/m**3)  :',
     &    gapden(1)
        WRITE(PELM(18),'(A,F10.6)')'r gap density T deriv    :',
     &    gapden(2)
        WRITE(PELM(19),'(A,F9.6)') 's gap Prandtl number     :',
     &    prandtl(1)
        WRITE(PELM(20),'(A,F9.6)') 't gap Prandtl T deriv    :',
     &    prandtl(2)
        WRITE(PELM(21),'(A,F9.3)') 'u gap Spec Ht (J/(kg-K)  :',
     &    gapsht(1)
        WRITE(PELM(22),'(A,F9.6)') 'v gap Spec Ht deriv      :',
     &    gapsht(2)
        m=22
      elseif(matopaq(index).eq.'h')then
        WRITE(PELM(12),'(A)')  'l type >>gas (data at 4 temps)'
        WRITE(PELM(13),'(A,F9.5)') 'm gap cond @-10C (W/(m-K):',
     &    gapcnd(1)
        WRITE(PELM(14),'(A,F9.5)') 'n gap cond @  0C (W/(m-K):',
     &    gapcnd(2)
        WRITE(PELM(15),'(A,F9.5)') 'o gap cond @ 10C (W/(m-K):',
     &    gapcnd(3)
        WRITE(PELM(16),'(A,F9.5)') 'p gap cond @ 20C (W/(m-K):',
     &    gapcnd(4)
        WRITE(PELM(17),'(A,F11.8)') 'q gap visc @-10C (kg/msK):',
     &    visco(1)
        WRITE(PELM(18),'(A,F11.8)')'r gap visc @  0C (kg/msK):',
     &    visco(2)
        WRITE(PELM(19),'(A,F11.8)')'s gap visc @ 10C (kg/msK):',
     &    visco(3)
        WRITE(PELM(20),'(A,F11.8)')'t gap visc @ 20C (kg/msK):',
     &    visco(4)
        WRITE(PELM(21),'(A,F9.3)') 'u gap dens @-10C (kg/m^3):',
     &    gapden(1)
        WRITE(PELM(22),'(A,F9.3)') 'v gap dens @  0C (kg/m^3):',
     &    gapden(2)
        WRITE(PELM(23),'(A,F9.3)') 'w gap dens @ 10C (kg/m^3):',
     &    gapden(3)
        WRITE(PELM(24),'(A,F9.3)') 'x gap dens @ 20C (kg/m^3):',
     &    gapden(4)
        WRITE(PELM(25),'(A,F9.6)') 'y gap Prandtl number     :',
     &    prandtl(1)
        WRITE(PELM(26),'(A,F9.6)') 'z gap spec Ht (J/(kg-K)  :',
     &    gapsht(1)
        m=26
      endif
      PELM(m+1) =' _____________________ '
      PELM(m+2)='? Help                 '
      PELM(m+3)='- Exit                 '
      NELM=m+3   ! number of items to display

C Help text for this menu.
   2  continue
      h(1)='This display shows the attributes of this material.'
      h(2)='The attributes vary depending on the source of the '
      h(3)='material (legacy materials have fewer attributes). '
      h(4)=' '
      h(5)='Some attributes (e.g. single layer optics) will be '
      h(6)='used in a future ESP-r version but are required for'
      h(7)='expport to other assessment tools. '
      h(8)=' '
      h(9)='Edit by selecting an item. '
      CALL EMENU('Material details',PELM,NELM,IW)
      IF(IW.EQ.0.OR.IW.EQ.NELM)THEN

C If there has been a modification check with user prior to
C updating the common blocks.
        if(modify)then
          dok=.true.
          h(1)='Recent changed to materials are held in memory '
          h(2)='and if you say no then edits will be discarded. '
          CALL ASKOK(' ','Accept changes in material (see help)?',
     &      OK,dok,2)
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
            matgapcnd(index,1) = gapcnd(1)
            matgapcnd(index,2) = gapcnd(2)
            matgapcnd(index,3) = gapcnd(3)
            matgapcnd(index,4) = gapcnd(4)
            matvisco(index,1) = visco(1)
            matvisco(index,2) = visco(2)
            matvisco(index,3) = visco(3)
            matvisco(index,4) = visco(4)
            matgapden(index,1) = gapden(1)
            matgapden(index,2) = gapden(2)
            matgapden(index,3) = gapden(3)
            matgapden(index,4) = gapden(4)
            matprandtl(index,1) = prandtl(1)
            matprandtl(index,2) = prandtl(2)
            matgapsht(index,1) = gapsht(1)
            matgapsht(index,2) = gapsht(2)
            modify=.false.
            mod=.true.   ! pass this back to calling code.
          endif
        endif
        RETURN
      ELSEIF(IW.EQ.NELM-2)THEN
        GOTO 3
      ELSEIF(IW.EQ.1)THEN
        H(1)='A descriptive name (up to 32 char) is required.'
        H(2)='This will appear in selection lists (but might be'
        H(3)='be shown in a truncated form in some locations). '
        CALL EASKS(tnam,' ',' Description of material? ',
     &    32,' ','material name',IER,3)
        if(tnam(1:2).ne.'  ')then
          modify=.true.
        endif
      ELSEIF(IW.EQ.2)THEN
        H(1)='Documentation (up to 248 char) is a really good idea'
        H(2)='and might include the source and test regime etc.'
        CALL EASKS248(tdoc,' ',' Documentation for material? ',
     &    72,' ','material documentation',IER,2)
        if(tdoc(1:2).ne.'  ')then
          modify=.true.
        endif
      ELSEIF(IW.EQ.3)THEN
        H(1)='Conductivity units are W/(m deg.C) '
        CALL EASKR(DBCON,' ',' Conductivity (W/(m deg.C): ',
     &    0.001,'W',300.,'W',1.,'conductivity',IER,1)
        modify=.true.
      ELSEIF(IW.EQ.4)THEN
        H(1)='Density units are  kg/m**3 '
        CALL EASKR(DBDEN,' ',' Density (kg/m**3): ',
     &    1.0,'W',9000.,'W',100.,'density',IER,1)
        modify=.true.
      ELSEIF(IW.EQ.5)THEN
        H(1)='Specific Heat units are J/(kg deg.C) '
        CALL EASKR(DBSHT,' ',' Specific heat (J/(kg deg.C): ',
     &    1.0,'W',2000.,'W',100.,'specific heat',IER,1)
        modify=.true.
      ELSEIF(IW.EQ.6)THEN

C For legacy materials both faces should have the same value.
        if(matopaq(index).eq.'-')then
          H(1)='Surface emissivity is based on infrared band. '
          H(2)='It is specified as fraction 0.01 < x < 0.99 '
          H(3)='This legacy material will have the same value '
          H(4)='for the inside and outside face. '
          CALL EASKR(EOUT,' ',' Surface emissivity : ',
     &      0.001,'W',0.999,'W',0.9,'out emissivity',IER,4)
          EIN=EOUT
          modify=.true.
        else
          H(1)='Surface emissivity is based on infrared band. '
          H(2)='It is specified as fraction 0.01 < x < 0.99 '
          H(3)='You may specify different values for each face '
          H(4)='(but ESP-r does not yet use this). '
          CALL EASKR(EOUT,' ',' Outside face emissivity : ',
     &      0.001,'W',0.999,'W',0.9,'out emissivity',IER,4)
          modify=.true.
        endif
      ELSEIF(IW.EQ.7)THEN

C For legacy materials both faces should have the same value.
        if(matopaq(index).eq.'-')then
          H(1)='Surface emissivity is based on infrared band. '
          H(2)='It is specified as fraction 0.01 < x < 0.99 '
          H(3)='This legacy material will have the same value '
          H(4)='for the inside and outside face. '
          CALL EASKR(EIN,' ',' Surface emissivity : ',
     &      0.001,'W',0.999,'W',0.9,'out emissivity',IER,4)
          EOUT=EIN
          modify=.true.
        else
          H(1)='Surface emissivity is based on infrared band. '
          H(2)='It is specified as fraction 0.01 < x < 0.99 '
          H(3)='You may specify different values for each face '
          H(4)='(but ESP-r does not yet use this). '
          CALL EASKR(EIN,' ',' Inside face emissivity : ',
     &      0.001,'W',0.999,'W',0.9,'in emissivity',IER,4)
          modify=.true.
        endif
      ELSEIF(IW.EQ.8)THEN
        if(matopaq(index).eq.'-')then
          H(1)='Surface absorptance is based on solar band. '
          H(2)='It is specified as fraction 0.01 < x < 0.99 '
          H(3)='This legacy material will have the same value '
          H(4)='for the inside and outside face. '
          CALL EASKR(AOUT,' ',' Surface absorptance: ',
     &      0.001,'W',0.999,'W',0.9,'absorptance',IER,4)
          AIN=AOUT
          modify=.true.
        else
          H(1)='Surface absorptance is based on solar band. '
          H(2)='It is specified as fraction 0.01 < x < 0.99 '
          H(3)='You may specify different values for each face '
          H(4)='(but ESP-r does not yet use this). '
          CALL EASKR(AOUT,' ',' Outside face absorptance: ',
     &      0.001,'W',0.999,'W',0.9,'out absorptance',IER,4)
          modify=.true.
        endif
      ELSEIF(IW.EQ.9)THEN
        if(matopaq(index).eq.'-')then
          H(1)='Surface absorptance is based on solar band. '
          H(2)='It is specified as fraction 0.01 < x < 0.99 '
          H(3)='This legacy material will have the same value '
          H(4)='for the inside and outside face. '
          CALL EASKR(AIN,' ',' Surface absorptance : ',
     &      0.001,'W',0.999,'W',0.9,'absorptance',IER,4)
          AOUT=AIN
          modify=.true.
        else
          H(1)='Surface absorptance is based on solar band. '
          H(2)='It is specified as fraction 0.01 < x < 0.99 '
          H(3)='You may specify different values for each face '
          H(4)='(but ESP-r does not yet use this). '
          CALL EASKR(AIN,' ',' Inside face absorptance: ',
     &      0.001,'W',0.999,'W',0.9,'in absorptance',IER,4)
          modify=.true.
        endif
      ELSEIF(IW.EQ.10)THEN
        H(1)='Vapour resistvity for vapour transfer has units'
        H(2)='of MNs g^-1m^-1. Used to support assessments of'
        H(3)='condensation within constructions. '
        CALL EASKR(dbdrv,' ','Vapour resistvity (MNs g^-1m^-1): ',
     &         1.0,'W',20000.,'W',1.,'vapour resistvity',IER,3)
        modify=.true.
      ELSEIF(IW.EQ.11)THEN
        H(1)='Thickness (mm) is the typical/default thickness of this'
        H(2)='material. '
        CALL EASKR(THICK,' ',' Default thickness (mm) : ',
     &         1.0,'W',500.,'W',10.,'default thickness',IER,2)
        modify=.true.
      ELSEIF(IW.EQ.12)THEN

C Offer to toggle between different types.
        if(matopaq(index).eq.'-')then
          message='(currently legacy opaque)'
        elseif(matopaq(index).eq.'o')then
          message='(currently opaque)'
        elseif(matopaq(index).eq.'t')then
          message='(currently transparent)'
        elseif(matopaq(index).eq.'g')then
          message='(currently gas data + T cor)'
        elseif(matopaq(index).eq.'h')then
          message='(currently gas data @4 temp)'
        endif
        h(1)='Legacy opaque was imported from an older materials'
        h(2)='database and only contains essential data. '
        h(3)=' '
        h(4)='Opaque will have option of additional parameters. '
        h(5)=' '
        h(6)='Transparent includes single layer optics such as '
        h(7)='used by WIS and Window5.1 etc. '
        h(8)=' '
        h(9)='There are two types of gas (work in progress): '
        h(10)=' Data with T correction to match data model of'
        h(11)=' the UK Energy Performance Directive'
        h(12)=' '
        h(13)=' Data (viscosity, conductivity, density, CP at'
        h(14)=' 4 temperature points.'
        CALL EASKATOG('Material options:',message,
     &    'legacy opaque','opaque','transparent',
     &    'gas data+correction ','gas data @4 temps','leave un-changed',
     &    ' ',jsur,14)
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
        elseif(jsur.eq.5)then
          matopaq(index)='h'
          modify=.true.
        else
          continue
        endif
      ELSEIF(IW.gt.12.and.IW.LE.NELM-4)THEN
        if(matopaq(index).eq.'-')then
          goto 3
        elseif(matopaq(index).eq.'o')then
          goto 3
        elseif(matopaq(index).eq.'t')then

C Single layer optical properties editing.
          if(IW.eq.13)then
            H(1)='Longwave transmittance units (-) between 0.0 & 1.0'
            CALL EASKR(IRTRAN,' ','Longwave transmittance (-): ',
     &        0.00,'W',1.00,'W',1.,'ir tran',IER,1)
            modify=.true.
          elseif(IW.eq.14)then
            H(1)='Solar transmittance units(-) between 0.0 & 1.0'
            CALL EASKR(SOLDRTRN,' ','Solar direct transmittance (-): ',
     &        0.00,'W',1.00,'W',1.,'solar direct tran',IER,1)
            modify=.true.
          elseif(IW.eq.15)then
            H(1)='Solar reflectance units are (-) between 0.0 and 1.0'
            H(2)='The outside face is farthest from zone air.'
            CALL EASKR(soldrotrfl,' ',
     &        'Solar reflectance outside face (-): ',
     &        0.00,'W',1.00,'W',1.,'solar reflec outside',IER,2)
            modify=.true.
          elseif(IW.eq.16)then
            H(1)='Solar reflectance units are (-) between 0.0 and 1.0'
            H(2)='The inside face is nearest the zone air.'
            CALL EASKR(soldrinrfl,' ',
     &        'Solar reflectance inside face (-): ',
     &        0.00,'W',1.00,'W',1.,'solar reflec inside',IER,2)
            modify=.true.
          elseif(IW.eq.17)then
            H(1)='Visible transmittance units are (-) between 0.0 & 1.0'
            CALL EASKR(vistran,' ','Visible trans (-): ',
     &        0.00,'W',1.00,'W',1.,'visible tran',IER,1)
            modify=.true.
          elseif(IW.eq.18)then
            H(1)='Visible reflectance units are (-) between 0.0 and 1.0'
            H(2)='The outside face is farthest from zone air.'
            CALL EASKR(visotrfl,' ',
     &        'Visible reflectance outside face (-): ',
     &        0.00,'W',1.00,'W',1.,'visible reflec outside',IER,2)
            modify=.true.
          elseif(IW.eq.19)then
            H(1)='Visable reflectance units are (-) between 0.0 and 1.0'
            H(2)='The inside face is nearest the zone air.'
            CALL EASKR(visinrfl,' ',
     &        'Visible reflectance inside face (-): ',
     &        0.00,'W',1.00,'W',1.,'visible reflec inside',IER,2)
            modify=.true.
          elseif(IW.eq.20)then !render
            H(1)='Colour rendering units are (?) between 0.0 & 100.0'
            H(2)='<<more text to be added...>>'
            CALL EASKR(visinrfl,' ',
     &        'Colour rendering index (-): ',
     &        0.00,'W',100.00,'W',1.,'colour rendering',IER,2)
            modify=.true.
          endif
          goto 3
        elseif(matopaq(index).eq.'g')then

C << Gas properties for data + T deriv to be added >>
          call usrmsg('This facility not yet available.',' ','-')
          goto 3
        elseif(matopaq(index).eq.'h')then

C << Gas properties for data at 4 T to be added >>
          call usrmsg('This facility not yet available.',' ','-')
          goto 3
        endif
      ELSEIF(IW.EQ.NELM-1)THEN
        CALL PHELPD('material attributes',9,'-',0,0,IER)
      ELSE
        IW=-1
        GOTO 2
      ENDIF
      GOTO 3

      END

C ************* EDMLDB 
C EDMLDB Display a constructions database.
C ITRU unit number  for user output, IER=0 OK, IER=1 problem.
C Information is available via common block MLC. 
C Overload DESC to include information on whether the construction is
C opaque or transparent and if the latter include the optical property
C id string.

      SUBROUTINE EDMLDB(ITRC,chgdb,IER,ISEL)
#include "building.h"
#include "epara.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C MPN is max number of tranparent glazings per multilayer construction
C If this parameter is changed also change it in subroutine GVALUE
      PARAMETER (MPN=3)
      COMMON/OUTIN/IUOUT,IUIN

      common/pophelp/h(60)
      
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      COMMON/GVALCAL/CLAMDA(MMLC)

      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the indes within matdatarray

      LOGICAL     OK,DOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,ISZERO
      logical moddb,chgdb,layermatch,closemat1,closemat2
      logical unixok

      CHARACTER H*72,KEY*1,T12*12,T12OTH*12,T14*14,MLCSYM*12
      CHARACTER MLCN*12,OTF*4,OPT*12,PROMP2*56,PROMP1*56,T72*72
      CHARACTER*29 MLCITM(36)
      CHARACTER xfile*144,tg*1,delim*1,outs*124
      character NAM*72,NAM99*72,LAYD*124
      character lworking*144,fs*1
      CHARACTER STGLP(7)*56
      DIMENSION IVALS(7)
      REAL TRNT(MPN),REF(MPN),OREF(MPN),OEMS(MPN)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      IER=0
      ISEL=0
      moddb = .false.
      chgdb=.false.
      matarrayindex=0

C If the common block MLC has not yet been filled the read in the
C construction database.
      IF(.NOT.MLDBOK)THEN
        CALL ERMLDB(ITRC,iuout,IER)
        IF(IER.NE.0)THEN
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
          call parsemlcdesc(DESC(IM),MLCN,OTF,OPT,MLCSYM)
          WRITE(MLCITM(M),22)KEY,MLCN(1:12),' ',OPT
  22      FORMAT(A1,1X,A12,a,a)
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
      MLCITM(M+4)='! list database contents    '
      MLCITM(M+5)='> save database             '
      MLCITM(M+6)='? help                      '
      MLCITM(M+7)='- exit this menu            '
      INO=-4

C Help text for this menu.
    2 continue
      H(1)='The information in this display is based on data from'
      H(2)='both a materials database and a construction database.'
      H(3)='It is used to build zone construction files'
      H(4)='according to the construction name associated with'
      H(5)='each surface.'
      H(6)='  '
      H(7) ='Note that the reporting level influences the detail '
      H(8) ='in this display and you may wish to reset the'
      H(9) ='reporting level to see additional information.'
      H(10)='  '
      H(11)='Adding or Deleting constructions is supported. The'
      H(12)='former creates a single layer construction at the end'
      H(13)='of the list which may then be edited. '
      H(14)=' '
      H(15)='You may copy an existing construction into a new item at'
      H(16)='the end of the list. Be sure its name is unique! '
      H(17)=' '
      H(18)='To cope with non-symmetrical partitions you may wish'
      H(19)='to use the copy & invert function.'

      CALL EMENU('Constuctions database',MLCITM,NITMS,INO)

      IF(INO.EQ.NITMS)THEN

C Check for changes/ask user to save. << TODO >>
        if(moddb)then
          dok=.true.
          h(1)='Recent changed to constructions are held in memory '
          h(2)='and might be lost if not save to file. '
          CALL ASKOK(' ','Save recent changes to constructions db?',
     &      OK,dok,2)
          if(OK)then
            CALL EMKAMLD(iuout,IER)
            chgdb=.true.
            moddb=.false.
          endif
        endif
        RETURN
      ELSEIF(INO.EQ.NITMS-1)THEN

C List help text for the menu.
        CALL PHELPD('construction db',19,'-',0,0,IER)
      ELSEIF(INO.EQ.NITMS-2)THEN
        CALL EMKAMLD(iuout,IER)
        moddb = .false.
        chgdb=.true.
        H(1)='In order to reflect changes you have made in this db'
        H(2)='in the current model you will have to rebuild the '
        H(3)='zone construction and tmc files.   '
        H(4)=' '
        H(5)='This option will be presented to you when you exit'
        H(6)='the database management session. '
        CALL PHELPD('construction db save',6,'-',0,0,IER)
      ELSEIF(INO.EQ.NITMS-3)THEN

C List database.
        h(1)='The list of MLC database entries can be sent to a'
        h(2)='text file or displayed.  Optical properties are'
        h(3)='included in detailed files or if the reporting level'
        h(4)='has been set to verbose. '
        CALL EASKABCD(' Reporting to:',' ',
     &    'summary text feedback','detailed text feedback',
     &    'summary file','detailed file',irpt,4)
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
        h(1)='Copies of exsiting constructions should have unique '
        h(2)='names. Before deleting a construction check that no '
        h(3)='surfaces use this attribute. '
        h(4)=' '
        h(5)='The copy and invert function is useful where nonsymmetric'
        h(6)='constructions are used in partitions. For example, in'
        h(7)='the case of a raised floor system from the point of view'
        h(8)='of the room the carpet is on the inside face and from the'
        h(9)='point of view of the floor void the carpet should be on '
        h(10)='the outside face. '
        call easkatog('Constructions options:',' ',
     &    'add (at end of list)','delete','copy','copy & invert',
     &    'continue',' ',' ',IW,10)
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

C Ask for name of the new construction
          h(1)='Although you only have 12 characters to work with'
          h(2)='try and create a useful name for the construction'
          h(3)='that is not a duplicate of an existing name. '
          T14='  '
          CALL EASKS(T14,' ',' Name of construction? ',
     &      14,' ','Construction name',IER,3)
          write(T12,'(a)') T14(1:12)
          call st2name(T12,DESC(NMLC))

          LAYERS(NMLC)=1
          IPR(NMLC,1)=1
          IPRMAT(NMLC,1)=1
          DTHK(NMLC,1)=0.10

C Bring up editing menu for new item.
          CALL EPMENSV
          CALL EDWALL(ITRC,NMLC,moddb,IER)
          CALL EPMENRC
          ILEN=NMLC
          IPACT=CREATE
          CALL EKPAGE(IPACT)
        elseif(IW.EQ.2)then

C Compact the list by moving all the items in common MLC up one.
          CALL EPMENSV
          call epkmlc(iwhich,'Delete which construction?',' ',ierr)
          CALL EPMENRC
          dok=.false.
          h(1)='Last chance to abort removal of construction. '
          CALL ASKOK(' ',' Are you sure? ',OK,dok,1)
          IF(.NOT.OK)GOTO 2
          moddb = .true.
          chgdb=.true.
          DO 891 IVV=IWHICH,NMLC-1
            DESC(IVV)=DESC(IVV+1)
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
        ELSEIF(IW.EQ.3)THEN

C Copy an existing construction to a new one at end of list.
          if(NMLC.ge.MMLC)then
            CALL USRMSG('Copy not allowed as the new item',
     &        'would exceed database space! Skipping request...','W')
            INO=-4
            GOTO 3
          endif
          CALL EPMENSV
          call epkmlc(iwhich,'Copy which construction?',' ',ierr)
          CALL EPMENRC
          moddb = .true.
          chgdb=.true.
          NMLC=NMLC+1
          DESC(NMLC)=DESC(IWHICH)
          LAYERS(NMLC)=LAYERS(IWHICH)
          DO 792 ILL=1,LAYERS(NMLC)
            DTHK(NMLC,ILL)=DTHK(IWHICH,ILL)
            IPR(NMLC,ILL)=IPR(IWHICH,ILL)
            IPRMAT(NMLC,ILL)=IPRMAT(IWHICH,ILL)
            DRAIR(NMLC,ILL,1)=DRAIR(IWHICH,ILL,1)
            DRAIR(NMLC,ILL,2)=DRAIR(IWHICH,ILL,2)
            DRAIR(NMLC,ILL,3)=DRAIR(IWHICH,ILL,3)
            LAYDESC(NMLC,ILL)=LAYDESC(IWHICH,ILL)
  792     CONTINUE

C Clear the symmetric link so SYMMETRIC or NONSYMMETRIC can be re-established.
C When editing the construction name use a slightly wider string buffer.
          call parsemlcdesc(DESC(NMLC),MLCN,OTF,OPT,MLCSYM)
          call ismlcsymmetric(nmlc,layermatch)
          if(layermatch)then
            MLCSYM='SYMMETRIC   '
          else
            MLCSYM='NONSYMMETRIC'
          endif
          write(T14,'(2a)') MLCN(1:12),'  '
          h(1)='Each construction in the database should have a unique'
          h(2)='name. Please edit the name (12 character or less) with'
          h(3)='no spaces or unprintable characters. '
          CALL EASKS(T14,' ',' Name of new construction (<12 char)?',
     &      14,' ','New construction name',IER,3)
          write(T12,'(a)') T14(1:12)
          call st2name(T12,MLCN)
          WRITE(DESC(NMLC),89)MLCN,OTF,OPT,MLCSYM

C Bring up editing menu for new item.
          CALL EPMENSV
          CALL EDWALL(ITRC,NMLC,moddb,IER)
          CALL EPMENRC
          ILEN=NMLC
          IPACT=CREATE
          CALL EKPAGE(IPACT)
        ELSEIF(IW.EQ.4)THEN

C Invert an existing construction. Ask user to select original and if
C there is room in the database copy data to a new item.
          if(NMLC.ge.MMLC)then
            CALL USRMSG('Copy & invert not allowed as the new item',
     &        'would xceed database space! Skipping request...','W')
            INO=-4
            GOTO 3
          endif
          CALL EPMENSV
          call epkmlc(iwhich,
     &      'Make inverted version of which construction?',' ',ierr)
          CALL EPMENRC
          moddb = .true.
          chgdb=.true.
          NMLC=NMLC+1
          DESC(NMLC)=DESC(IWHICH)

C Extract construction name and check that mlc name has no illegal characters.
          call parsemlcdesc(DESC(NMLC),T12,OTF,OPT,MLCSYM)
          WRITE(T12OTH,'(A)')DESC(IWHICH)(1:12)

C If there is space attach an "_i" to the end of the name to save the
C user this task. Get user to confirm the name and then use st2name to
C remove any wild card charaters.
          if(lnblnk(T12).le.8)then
            WRITE(T14,'(2A)') T12OTH(1:lnblnk(T12OTH)),'_inv  '
          elseif(lnblnk(T12).gt.9.and.lnblnk(T12).le.10)then
            WRITE(T14,'(2A)') T12OTH(1:lnblnk(T12OTH)),'_i  '
          endif
          write(outs,'(3a)') '(original name ',
     &      T12OTH(1:lnblnk(T12OTH)),')'
          h(1)='Typically the original and interted constructions have'
          h(2)='similar names with one indicating its inverted state.'
          h(3)='The name should be less than or equal to 12 characters'
          h(4)='and not use spaces or unprintable characters. '
          CALL EASKS(T14,'Name of inverted construction (<12 char)?',
     &      outs,14,' ','Inverted construction name',IER,4)
          write(T12,'(a)') T14(1:12)
          call st2name(T12,MLCN)
          if(T12.ne.MLCN)then
            write(outs,'(a,a12,a,a12,a)')' WARNING: Corrected name ',
     &        T12,' to ',MLCN,' in the database entry.'
            call usrmsg(outs,DESC(NMLC),'W')
          endif

C Update DESC including the name of the original MLC.
          WRITE(DESC(NMLC),89)MLCN,OTF,OPT,T12OTH
   89     FORMAT(A12,2X,A4,2X,A12,2X,A12)

C Update the original to point to the inverted item.
          call parsemlcdesc(DESC(IWHICH),T12,OTF,OPT,MLCSYM)
          WRITE(DESC(IWHICH),89)T12,OTF,OPT,MLCN

C Copy other data associated with the MLC.
          LAYERS(NMLC)=LAYERS(IWHICH)
          DO 793 ILL=1,LAYERS(NMLC)
            DTHK(NMLC,ILL)=DTHK(IWHICH,ILL)
            IPR(NMLC,ILL)=IPR(IWHICH,ILL)
            IPRMAT(NMLC,ILL)=IPRMAT(IWHICH,ILL)
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
            DRT1=DRAIR(NMLC,ILL,1)
            DRT2=DRAIR(NMLC,ILL,2)
            DRT3=DRAIR(NMLC,ILL,3)
            LAYD=LAYDESC(IWHICH,ILL)

C Opposite layer is IOP. Copy its data into ILL layer.
            IOP=LAYERS(NMLC)-ILL+1
            DTHK(NMLC,ILL)=DTHK(NMLC,IOP)
            IPR(NMLC,ILL)=IPR(NMLC,IOP)
            IPRMAT(NMLC,ILL)=IPRMAT(NMLC,IOP)
            DRAIR(NMLC,ILL,1)=DRAIR(NMLC,IOP,1)
            DRAIR(NMLC,ILL,2)=DRAIR(NMLC,IOP,2)
            DRAIR(NMLC,ILL,3)=DRAIR(NMLC,IOP,3)
            LAYDESC(NMLC,ILL)=LAYDESC(NMLC,IOP)

C Finally copy temporary data into opposite layer.
            DTHK(NMLC,IOP)=DT
            IPR(NMLC,IOP)=IPRT
            IPRMAT(NMLC,IOP)=IPRMT
            DRAIR(NMLC,IOP,1)=DRT1
            DRAIR(NMLC,IOP,2)=DRT2
            DRAIR(NMLC,IOP,3)=DRT3
            LAYDESC(NMLC,IOP)=LAYD
  692     CONTINUE

C Bring up detailed editing menu.
          CALL EPMENSV
          CALL EDWALL(ITRC,NMLC,moddb,IER)
          CALL EPMENRC
          ILEN=NMLC
          IPACT=CREATE
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(INO.EQ.NITMS-6)THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(INO.EQ.NITMS-5)THEN

C Calculate g-value of transparent constructions
C First check if mlc is transparent
        H(1)= 'Although g-values for transparent glazings are not used'
        H(2)= 'in ESP-r is is possible to report these in accordance '
        H(3)= 'with Test G5 TM33:2006 CIBSE. '
        H(4)= 'You will be required to provide for each pane its  '
        H(5)= 'transmittance and reflectance (both in the direction of'
        H(6)= 'incident radiation and against it)'
        H(7)= 'Single/double and triple glazings are supported '
        H(8)= 'G-value calculations are currently restricted to '
        H(9)= 'air or gas filled triple glazings which are symmetric'
        H(10)='in construction'
        CALL PHELPD('G-value for tran construction',10,'-',0,0,IER)
        CALL EPMENSV
        call epkmlc(iwh,'g-value for which construction?',' ',ierr)
        CALL EPMENRC
        moddb = .true.
        chgdb=.true.
        THCON=CLAMDA(IWH)
        call parsemlcdesc(DESC(IWH),MLCN,OTF,OPT,MLCSYM)
        IF(OPT(1:4).EQ.'OPAQ'.OR.OPT(1:4).EQ.'UNKN'.OR.
     &     OPT(1:4).EQ.'CFC ')THEN
          CALL EDISP(IUOUT,' ')
          CALL EDISP(IUOUT,
     &    ' G-value only defined for transparent constructions')
          CALL EDISP(IUOUT,' ')
        ELSE

C Get number of non-air layers of glass.

C << todo: make use of the single lay optics from the materials commons
C << to supply much of the information needed for the g-value calculation.

C << need alternative logic to locate air gaps >>

          IPN=0
          DO 501 ILAY=1,LAYERS(IWH)
            legmat=IPR(IWH,ILAY)   ! which legacy index
            if(legmat.gt.0)then
              matarrayindex=mathash(legmat)  ! find matching array index
 
C And if matarrayindex is zero then get NAM.
              if(matarrayindex.eq.0)then
                NAM='AIR'
              else
                write(NAM,'(a)') matname(matarrayindex)(1:32)
              endif
            elseif(legmat.eq.-99)then

C Use the text within the MLC database to find the data.
              lex=MIN0(72,lnblnk(LAYDESC(IWH,ILAY)))
              write(NAM99,'(a)')LAYDESC(IWH,ILAY)(1:lex)
              ileghope=IPR(IWH,ILAY)   ! what we hope we will find a match for
              call parsematdesc(ileghope,NAM99,matarrayindex)
              if(matarrayindex.ne.0)then
                write(NAM,'(a)') NAM99(1:32)
              endif
            endif
            IF(IER.EQ.1)RETURN
            IF(IPR(IWH,ILAY).EQ.0)GOTO 501
            IPN=IPN+1

C For each nonair layer ask for reflectance, tranmittance and emissivity
            H(1) ='In order to calculate G-value ESP-r needs values for'
            H(2) ='trnt   = transmittance of the pane,'
            H(3) ='ref    = reflectance of this pane in direction of'
            H(4) ='         incident radiation'
            H(5) ='o-ref  = reflectance of this pane in direction '
            H(6) ='         opposite incident radiation'
            H(7) ='ems    = emissivity of this pane in direction of'
            H(8) ='         incident radiation'
            H(9) ='o-ems  = emissivity of this pane in direction '
            H(10)='         opposite incident radiation'
            H(11)=' '
            H(12)='Choose CIBSE TM33:2006 prescribed values or   '
            H(13)='define your own'
            H(14)='Note that low emissivity glass has been inverted'
            H(15)='because it is always the 2nd pane for TM33'
            INPICK=1
            PROMP1='Choose TM33:2006 values or manually define '
            WRITE(PROMP2,'(A,I2)')'properties for glazing layer no.',IPN
            STGLP(1)='trnt  ref   o-ref ems   o-ems description'
            STGLP(2)='0.789 0.072 0.072 0.837 0.837 clear glass'
*            STGLP(3)='0.678 0.091 0.108 0.837 0.170 low-e glass'
            STGLP(3)='0.678 0.108 0.091 0.170 0.837 low-e glass'
            STGLP(4)='0.460 0.053 0.053 0.837 0.837 absorbing glass'
            STGLP(5)='0.390 0.310 0.450 0.837 0.250 hi perf reflecting'
*            STGLP(5)='0.390 0.450 0.310 0.250 0.837 hi perf reflecting'
            STGLP(6)='manual / user defined'
            CALL EPICKS(INPICK,IVALS,PROMP1,PROMP2,
     &                56,6,STGLP,' ',IER,15)
            IF(IER.NE.0)RETURN
            IPKD=IVALS(1)
            IF(IPKD.GE.2.AND.IPKD.LE.5)THEN

C Copy the numbers section of STGLP array item.
              write(T72,'(a)') STGLP(IPKD)(1:29)
            ELSEIF(IPKD.EQ.6)THEN
              T72='  '
              CALL EASKS(T72,
     &        'enter trnt, reft, op-reft, ems and op-ems (see help)',
     &        ' ',72,' ','Optical data for g-value',IER,15)
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

C emissivity value not required
            CALL EGETWR(T72,K,VAL,0.,1.,'W','emiss',IER)
            CALL EGETWR(T72,K,VAL,0.,1.,'W','op-emiss',IER)
            OEMS(IPN)=VAL 
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
C bring up an editing facility.
        CALL KEYIND(NITMS,INO,IFOC,IO)
        CALL EDISP(iuout,' ')
        call etmldb(2,iuout,IFOC,imerr)
        CALL EPMENSV
        CALL EDWALL(ITRC,IFOC,moddb,IER)
        CALL EPMENRC
        ISEL=IFOC
        chgdb=moddb
      else
        INO=-1
        GOTO 2
      ENDIF
      INO=-4
      GOTO 3

      END

C ************* EDWALL 
C EDWALL Edit a construction common block data.
C IFMAT is the unit for the associated materials db.
C Information is available via common block MLC. 
C Information on opaque/transparent is added to DESC.

C Note: air gaps have several representations. The legacy
C approach is to use the materials database index zero to
C flag an air gap (and the user would overload the MLC layer
C with 3 air gap resistance values). An emerging technique
C is to label materials (matopaq) as follows: 
C matopaq (1 char) 'o' is opaque, 't' is transparent,
C                  'g' is gas (data+temp correction),
C                  'h' is gas (data at 4 temps),
C                  'r' is gas (fixed resistances (not yet available)),
C                  '-' imported from legacy
C Some of the items in the GAPS material category have a -99
C index (no equivalent in legacy databases) and the current code
C assumes that -99 entities may be gasses.

      SUBROUTINE EDWALL(ITRC,IFOC,moddb,IER)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G2/G7/precz.
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C Paramters
      integer ITRC  ! if > 1 then be verbose
      integer IFOC  ! index in the construction (array)
      logical moddb ! set to true of construction altered
      integer IER   ! zero is ok, one is a problem

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      common/pophelp/h(60)
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB
      LOGICAL     OK,DOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical newgeo  ! to use for testing if new/old geometry file.


C Material properties.
      integer matarrayindex ! the index within matdatarray
      real guessthick ! initial thickness if from version 1.1 or 1.2

      logical chdb,showother,layermatch,layermatcha,modgeo
      logical closemat1,closemat2

C Note: size of MLED supports a dozen layers in a construction.
      DIMENSION MLED(26),PNAM(ME),MMLAY(ME+2),imlay(ME+2)
      character*72 LTMP
      CHARACTER NAM*72,H*72,KEY*1,GDESCR*36
      CHARACTER MLCN*12,OTF*4,OPT*12,SOPT*12,LAYD*124
      character T12OTH*12,T12*12,T14*14
      CHARACTER outs*124,MLED*38,MMLAY*38,MLCSYM*12
      character PNAM*72
      character ZN*12,SN*12

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
C is true then we can continue (the common blocks are filled).
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

      IER=0
      moddb=.false.
      newgeo=.false.   ! assume older format geometry.
      guessthick=100.0 ! initial assumption for layer thickness
      matarrayindex=0
      iuf=IFIL+1

C Extract construction name, opaque/transp, optical and symmetry properties.
C Check that mlc name has no illegal characters.
      call parsemlcdesc(DESC(IFOC),T12,OTF,OPT,MLCSYM)
      call st2name(T12,MLCN)
      if(T12.ne.MLCN)then
        write(outs,'(a,a12,a,a12,a)')' WARNING: Corrected mlc ',
     &    T12,' to ',MLCN,' in the db entry '
        call usrmsg(outs,DESC(IFOC),'W')
      endif

C If T12 is `UNKNOWN` and MLCSYM is blank then we have a new
C construction. If we got this far with a blank MLCSYM then
C set it to SYMMETRIC.
      if(T12(1:4).eq.'UNKN'.and.MLCSYM(1:2).eq.'  ')then
        MLCSYM='SYMMETRIC'
      elseif(MLCSYM(1:2).eq.'  ')then
        MLCSYM='SYMMETRIC'
      endif

C Read tag for name of inverted version or symmetry if the length of the
C DESC string could hold the tag, otherwise set MLCSYM to a blank. If
C the tag refers to another MLC then include this in the menu.
      if(MLCSYM(1:9).EQ.'SYMMETRIC')then
        showother=.false.
      elseif(MLCSYM(1:12).EQ.'NONSYMMETRIC')then
        showother=.false.
      else
        showother=.true.
      endif

C Editing menu.
   2  WRITE(MLED(1),'(A,1X,A12)')'a Construction:',MLCN
      IF(OTF(1:4).EQ.'TRAN')THEN
        MLED(2)='b General type: Transparent '
      ELSEIF(OTF(1:4).EQ.'CFC ')THEN
        MLED(2)='b General type: Complex Fenestration'
      ELSE
        MLED(2)='b General type: Opaque      '
      ENDIF
      WRITE(MLED(3),'(A,1X,A12)')'c Optical properties:',OPT
      WT=0.0
      DO 22, IL=1,LAYERS(IFOC)
        WT=WT+DTHK(IFOC,IL)
  22  continue
      WRITE(MLED(4),'(A,I2,a,F7.1,a)')  '  No of layers:',LAYERS(IFOC),
     &  ' (',WT*1000.0,'mm thick)'
      if(showother)then
        WRITE(MLED(5),'(2a)')  'd Linked with: ',MLCSYM
      else
        WRITE(MLED(5),'(2a)')  'd Layers are: ',MLCSYM
      endif
      MLED(6)='  _____________________________  '
      MLED(7)=' Layer|Matr|Thick| Description   '
      MLED(8)='      |db  |(mm) | of material   '

C Detail menu.
      M=8
      RT=0.
      DO 23, IL=1,LAYERS(IFOC)
        matarrayindex=IPRMAT(IFOC,IL)   ! which legacy index
        if(matarrayindex.ge.0)then
 
C And if matarrayindex is zero then reset dbcon and NAM.
          if(matarrayindex.eq.0)then
            DBCON=0.0; NAM='AIR'
          else
            DBCON=matdbcon(matarrayindex)
            write(NAM,'(a)') matname(matarrayindex)(1:32)
          endif

C If g or h air gap and version 1.2 then assign DRAIR
          if(matarrayindex.eq.0)then
            continue
          elseif(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &       matopaq(matarrayindex)(1:1).eq.'h')then
            if(closemat2)then
              DRAIR(IFOC,IL,1)=matgapares(matarrayindex,1)
              DRAIR(IFOC,IL,2)=matgapares(matarrayindex,2)
              DRAIR(IFOC,IL,3)=matgapares(matarrayindex,3)
              RT=RT+DRAIR(IFOC,IL,1)  ! while we are sure
            endif
          endif
        else

C If an earlier scan of MLC detected duplicate material names or
C was confused then matarrayindex could be -99.
          call edisp(iuout,
     &      'One layer material ref is confused or a duplicate name.')
          NAM='UNKNOWN'
        endif

C Calculate standard U value and display as well.
C If a standard air layer include the R values in the display.
        IF(IPR(IFOC,IL).EQ.0)THEN
          WRITE(PNAM(IL),'(A,3F5.2)')'air ',(DRAIR(IFOC,IL,IY),IY=1,3)
          RT=RT+DRAIR(IFOC,IL,1)
        ELSEIF(IPR(IFOC,IL).EQ.-99)THEN
          PNAM(IL)=NAM  ! use RT calc above
        ELSE
          PNAM(IL)=NAM
          RT=RT+DTHK(IFOC,IL)/DBCON
        ENDIF
        M=M+1
        CALL EMKEY(M,KEY,IER)
        WRITE(MLED(M),29)KEY,IL,IPR(IFOC,IL),DTHK(IFOC,IL)*1000.0,
     &                   PNAM(IL)(1:19)
  29    FORMAT(A1,I4,I5,F7.1,2X,A)
  23  CONTINUE

C Rest of detail menu.
      M=M+1

C Historic ESP-r assumptions of outside hc of 0.055 and horizontal
C flow internal hc of 0.123.
C       RT=RT+0.055+0.123
C       UVALUE=1.0/RT

C ISO 6946 hc assumptions are 0.04 external, 0.13 inside horizontal,
C 0.10 inside upward flow and 0.17 inside downward flow.
      RTH=RT+0.04+0.13
      UVALUEH=1.0/RTH
      RTU=RT+0.04+0.10
      UVALUEU=1.0/RTU
      RTD=RT+0.04+0.17
      UVALUED=1.0/RTD
      RTI=RT+0.13+0.13
      UVALUEI=1.0/RTI
      write(MLED(M),'(A,3F6.3)') ' ISO 6946 U h/u/d',
     &    UVALUEH,UVALUEU,UVALUED
      M=M+1
      MLED(M)='  ____________________________   '
      M=M+1
      MLED(M)='! add or delete a layer          '
      M=M+1
      MLED(M)='? help                           '
      M=M+1
      MLED(M)='- exit this menu                 '
      NLED=M
      ILED=-4

C Also compose a list of layers for additions/deletions.
      MMLAY(1)=' Layer|Prim|Thick| Description   '
      MMLAY(2)='      |db  |(mm) | of material   '
      DO 11, IL=1,LAYERS(IFOC)
        WRITE(MMLAY(IL+2),299)IL,IPR(IFOC,IL),DTHK(IFOC,IL)*1000.0,
     &                   PNAM(IL)(1:20)
 299    FORMAT(I4,I5,F7.1,2X,A)
  11  CONTINUE

C Help text for this menu.
    3 continue
      H(1)='A construction is composed of between one and eight'
      H(2)='layers (three of which may be air gaps). Each layer'
      H(3)='has a thickness and is composed of one material. '
      H(4)='The thermophysical properties of each layer are based'
      H(5)='on data in the current materials database.'
      H(6)='  '
      H(7)='Instances of the data for this construction will be'
      H(8)='inserted in the zone construction file. To ensure'
      H(9)='model quality, avoid further editing of the zone con-'
      H(10)='struction files (create more constructions if you need'
      H(11)='variant thermophysical properties).'
      H(12)=' '
      H(13)='Current options allow one layer at a time to be added'
      H(14)='to the end of the list or inserted into the list. You'
      H(15)='may also delete one layer at a time.  If you alter a'
      H(16)='construction you will be asked (later) if you want to'
      H(17)='update the zone construction files.'
      H(18)='  '
      H(19)='Note: layer 1 is considered the `outside`.'
      H(20)='  '
      H(21)='The terms SYMMETRIC and NONSYMMETRIC have the following'
      H(22)='meaning: SYMMETRIC constructions can be referenced from'
      H(23)='either zone associated with a partition; a NONSYMMETRIC'
      H(24)='construction must have an `inverted` version in the'
      H(25)='database in the case of surfaces which are partitions.'
      H(26)=' '
      H(27)='In ESP-r air gaps in constructions are represented as'
      H(28)='a fixed resistance to heat flow (depending on the orien-'
      H(29)='tation of the surface using the construction). The R'
      H(30)='value supplied should account for the resistance across'
      H(31)='the cavity (including the impact of low emissivity'
      H(32)='at the adjacent solid layers).'
      H(33)=' '
      H(34)='Air gaps are assumed not to have air movement. If the'
      H(35)='cavity is ventilated you might have to represent it as'
      H(36)='a thermal zone and include it in an air flow network.'
 
      CALL EMENU('Construction editing',MLED,NLED,ILED)
      IF(ILED.EQ.NLED)THEN

C Check to see that mlc of a single air layer is not specified.
        if(LAYERS(IFOC).eq.1.and.IPR(IFOC,1).EQ.0)then
          call usrmsg('A construction cannot be a single layer',
     &      'of air. Please redefine it.','W')
          goto 3
        endif
        if(IPR(IFOC,LAYERS(IFOC)).eq.0.or.IPR(IFOC,1).EQ.0)then
          call usrmsg('An outer or inner layer of a construction',
     &      'cannot be composed of air. Please redefine it.','W')
          goto 3
        endif

C After editing MLC, re-check to see if construction is symmetric
C and then if it is linked to another `inverted` MLC save it as well.
        if(moddb)then
          call ismlcsymmetric(ifoc,layermatch)
          if(MLCSYM(1:9).EQ.'SYMMETRIC')then
            if(layermatch)then
              continue
            else
              MLCSYM='NONSYMMETRIC'
              call edisp(iuout,
     &          'Resetting construction to nonsymmetric.')
            endif
          elseif(MLCSYM(1:12).EQ.'NONSYMMETRIC')then
            if(layermatch)then
              MLCSYM='SYMMETRIC   '
              call edisp(iuout,'Resetting construction to symmetric.')
            else
              continue
            endif
          else

C Linked to another construction, check if it should be updated to match.
            write(outs,'(4a)') 'Construction ',MLCN(1:lnblnk(MLCN)),
     &        ' is linked to ',MLCSYM
            dok=.true.
            h(1)='The current construction is linked to a reversed '
            h(2)='version of itself. You will want to keep these in'
            h(3)='sync with each other. '
            CALL ASKOK(outs,'Update the linked construction as well?',
     &        OK,dok,3)
            if(OK)then

C Find matching MLC and copy data associated with the current MLC to
C the linked MLC.
              do 794 k=1,NMLC
                if(DESC(k)(1:12).eq.MLCSYM(1:12))then
                  LAYERS(k)=LAYERS(IFOC)
                  DO 793 ILL=1,LAYERS(IFOC)
                    DTHK(k,ILL)=DTHK(IFOC,ILL)
                    IPR(k,ILL)=IPR(IFOC,ILL)
                    IPRMAT(k,ILL)=IPRMAT(IFOC,ILL)
                    DRAIR(k,ILL,1)=DRAIR(IFOC,ILL,1)
                    DRAIR(k,ILL,2)=DRAIR(IFOC,ILL,2)
                    DRAIR(k,ILL,3)=DRAIR(IFOC,ILL,3)
                    LAYDESC(k,ILL)=LAYDESC(IFOC,ILL)
  793             CONTINUE

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
  692             CONTINUE
                endif
  794         continue
            endif
          endif

C Update DESC before returning.
          WRITE(DESC(IFOC),89)MLCN,OTF,OPT,MLCSYM
   89     FORMAT(A12,2X,A4,2X,A12,2x,A12)
          call edisp(iuout,'  ')
          call edisp(iuout,
     &      'Please ensure that you save the database...')
        endif
        RETURN
      ELSEIF(ILED.EQ.NLED-1)THEN

C List the help.
       CALL PHELPD('construction editing',36,'-',0,0,IER)
      ELSEIF(ILED.EQ.NLED-2)THEN

C +- Layer.
        h(1)='You can delete an existing layer, append a new layer '
        h(2)='at the `inside face` of the surface (remember the '
        h(3)='order is other-side to zone-side), or insert a layer'
        h(4)='(you will be asked to select a point of insertion and'
        h(5)='existing inner layers will be shifted).'
        h(6)=' '
        h(7)='If you cancel no changes will be made in the mode. '
        CALL EASKABCD(' Layer options: ',' ',' Delete',
     &    ' Append @ inside face',' Insert','Continue',IW,7)
        IF(IW.EQ.1)THEN

C If no of layers is edited then ask the user which one to remove.
C Now loop through the layers and copy the contents of layers >
C IWHICH into the next lower one.
          call edisp(iuout,' Select layer to delete.')
          H(1)='Any one of the layers may be selected.'
          IX=1
          CALL EPMENSV
          CALL EPICKS(IX,imlay,' ',' Layer to delete: ',
     &         38,LAYERS(IFOC)+2,MMLAY,'Layer to delete',IER,1)
          CALL EPMENRC
          if(IX.eq.1.and.imlay(1).gt.2)then
            IWHICH=imlay(1)-2
            write(outs,'(3A)')' Removing ',PNAM(IWHICH)(1:18),'...'
            DO 791 IVV=IWHICH,LAYERS(IFOC)-1
              IPR(IFOC,IVV)=IPR(IFOC,IVV+1)
              IPRMAT(IFOC,IVV)=IPRMAT(IFOC,IVV+1)
              DTHK(IFOC,IVV)=DTHK(IFOC,IVV+1)
              DRAIR(IFOC,IVV,1)=DRAIR(IFOC,IVV+1,1)
              DRAIR(IFOC,IVV,2)=DRAIR(IFOC,IVV+1,2)
              DRAIR(IFOC,IVV,3)=DRAIR(IFOC,IVV+1,3)
              LAYDESC(IFOC,IVV)=LAYDESC(IFOC,IVV+1)
  791       CONTINUE
            LAYERS(IFOC)=LAYERS(IFOC)-1
          endif

C Check is layers no longer match.
          call ismlcsymmetric(ifoc,layermatcha)
          if(.NOT.layermatcha)then
            call edisp(iuout,'Layers might now be nonsymmetric.')
            call edisp(iuout,'This will be reconciled when exiting.')
          endif
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

              H(1)='Any one of the layers may be selected.'
              IX=1
              CALL EPMENSV
              CALL EPICKS(IX,imlay,' ',' Point of insertion: ',
     &          38,LAYERS(IFOC)+2,MMLAY,'Point of insertion',IER,1)
              CALL EPMENRC
              if(IX.eq.1.and.imlay(1).gt.2)then
                IWHICH=imlay(1)-2
                ILN=IWHICH
                LAYERS(IFOC)=LAYERS(IFOC)+1
                IVV=LAYERS(IFOC)+1
  792           CONTINUE
                IVV=IVV-1
                IPR(IFOC,IVV)=IPR(IFOC,IVV-1)
                IPRMAT(IFOC,IVV)=IPRMAT(IFOC,IVV-1)
                DTHK(IFOC,IVV)=DTHK(IFOC,IVV-1)
                DRAIR(IFOC,IVV,1)=DRAIR(IFOC,IVV-1,1)
                DRAIR(IFOC,IVV,2)=DRAIR(IFOC,IVV-1,2)
                DRAIR(IFOC,IVV,3)=DRAIR(IFOC,IVV-1,3)
                LAYDESC(IFOC,IVV)=LAYDESC(IFOC,IVV-1)
                IF(IVV.GT.IWHICH+1) GOTO 792
              endif
            else
              ILN=LAYERS(IFOC)+1
            endif

C Ask if user wishes to browse through materials to find suitable
C reference. Use elistmat to select via materials array.
            dok=.true.
            h(1)='If you want to keep the current material answer NO'
            h(2)='otherwise you can find one by browsing the database.'
            CALL ASKOK('Browse the materials db for a new material',
     &        '(NO keeps the current material)?',OK,dok,2)
            IF(OK)then
              iwhich = 0
              CALL ELISTMAT(iwhich,chdb,'-',matarrayindex,IER)
              if(iwhich.eq.0)then
                H(1)='If air is correct then accept, otherwise'
                H(2)='reselect from the list or abort the task.'
                CALL EASKABC('Your selection is `0` i.e. air. Options:',
     &            ' ','accept','reselect material','abort',iwair,2)
                 if(iwair.eq.2)then
                   CALL ELISTMAT(iwhich,chdb,'-',matarrayindex,IER)
                 elseif(iwair.eq.3)then
                   ILED=-1
                   GOTO 3
                 endif
              elseif(iwhich.eq.-99)then
                continue
              endif
            else
              IWHICH=IPR(IFOC,ILN)  ! keep the current material
            endif

C Take the legacy index and assign it to this layer.
            IPR(IFOC,ILN)=IWHICH
            if(iw.eq.2)LAYERS(IFOC)=ILN

C Make up LAYDESC and PNAM as combination of matnam and matdoc based on
C the newly selected material. This is necessary so that other
C code can recover the details of a -99 item.
            if(matarrayindex.gt.0)then
              lnam=lnblnk(matname(matarrayindex))
              lfordoc = 72 - (lnam +4)    ! space left for doc
              write(LAYDESC(IFOC,ILN),'(3a)') 
     &          matname(matarrayindex)(1:lnam),' : ',
     &          matdoc(matarrayindex)(1:lfordoc)
              write(PNAM(ILN),'(3a)')
     &          matname(matarrayindex)(1:lnam),' : ',
     &          matdoc(matarrayindex)(1:lfordoc)
              IPRMAT(IFOC,ILN)=matarrayindex  ! update array index for this layer as well
            endif

C If a standard air layer then ask for gap resistance.
            IF(IPR(IFOC,ILN).EQ.0)THEN
              H(1)='A set of default assumptions on the R value of'
              H(2)='an air gap is requested, in case the construction '
              H(3)='is used in different orientations.'
              H(4)='This information will allow for the automatic'
              H(5)='creation of zone construction files with sub-'
              H(6)='stantially less user interaction.'
              WRITE(outs,117)DESC(IFOC)(1:LNBLNK(DESC(IFOC)))
  117         FORMAT('Default R value for air gap in ',a)

              if(DRAIR(IFOC,ILN,1).lt.0.001)then
                 VAL=0.17
              else
                 VAL=DRAIR(IFOC,ILN,1)
              endif
              CALL EASKR(VAL,outs,
     &          ' if the construction orientation is vertical: ',
     &          0.01,'W',99.9,'W',0.17,'def air gap R',IER,6)
              DRAIR(IFOC,ILN,1)=VAL
              if(DRAIR(IFOC,ILN,2).lt.0.001)then
                 VAL=0.17
              else
                 VAL=DRAIR(IFOC,ILN,2)
              endif
              CALL EASKR(VAL,outs,
     &          ' if the construction is a floor or ceiling  : ',
     &          0.01,'W',99.9,'W',0.17,'def air gap R',IER,6)
              DRAIR(IFOC,ILN,2)=VAL
              if(DRAIR(IFOC,ILN,3).lt.0.001)then
                 VAL=0.17
              else
                 VAL=DRAIR(IFOC,ILN,3)
              endif
              CALL EASKR(VAL,outs,
     &          ' if the construction is sloped or UNKNOWN   : ',
     &          0.01,'W',99.9,'W',0.17,'def air gap R',IER,6)
              DRAIR(IFOC,ILN,3)=VAL
              guessthick=100.0
            ELSEIF(IPR(IFOC,ILN).EQ.-99)THEN

C Recover the stipulated air gap resistance if we have a GAP
C type of [g] or [h].
              if(matarrayindex.gt.0)then
                guessthick=matdbthick(matarrayindex)
                if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &             matopaq(matarrayindex)(1:1).eq.'h')then
                  if(closemat2)then
                    DRAIR(IFOC,ILN,1)=matgapares(matarrayindex,1)
                    DRAIR(IFOC,ILN,2)=matgapares(matarrayindex,2)
                    DRAIR(IFOC,ILN,3)=matgapares(matarrayindex,3)
                  endif
                endif
              endif

            ELSE

C Not an air gap so set DRAIR to something and set default thickness.
              DRAIR(IFOC,ILN,1)=0.17
              DRAIR(IFOC,ILN,2)=0.17
              DRAIR(IFOC,ILN,3)=0.17
              if(matarrayindex.gt.0)then
                guessthick=matdbthick(matarrayindex)
              endif
            ENDIF
            H(1)='The thickness of the layer is nominally between'
            H(2)='1 mm and 300 mm. It is stored in the file as m.'
            VAL=guessthick
            CALL EASKR(VAL,' ',' Layer thickness (mm): ',
     &        1.0,'W',300.0,'W',100.0,'layer thickness mm',IER,2)
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
        call ismlcsymmetric(ifoc,layermatcha)
        if(.NOT.layermatcha)then
          call edisp(iuout,'Layers might now be nonsymmetric.')
          call edisp(iuout,'This will be reconciled when exiting.')
        endif
      ELSEIF(ILED.EQ.1)THEN

C Edit construction name, if it is linked to another MLC then the
C linked MLC should point back to the edited name.
        H(1)='A descriptive name (up to 12 characters) is required.'
        H(2)='Because the name is short it is a good idea to also'
        H(3)='separately document what this name really means. '
        T12=MLCN

C If changing the name of construction then find out if
C any of the surfaces in the model use this construction
C If they do then warn the user.
        if(ncon.gt.0)then
          modgeo=.false.
          write(outs,'(a)') ' Checking surface attributes...'
          CALL USRMSG(' ',outs,'-')
          do 97 icc=1,ncon
            if(SSMLCN(icc)(1:12).eq.T12(1:12))then
              WRITE(outs,'(a,I3,1X,F7.2,F5.0,F5.0,1X,A12,1X,A4,
     &          1X,A4,1X,A12,1X,A13)')'found: ',icc,
     &          SSNA(icc),SSPAZI(icc),SSPELV(icc),SSNAME(icc),
     &          SSOTF(icc),SSVFC(icc),SSMLCN(icc),SSOTHER(icc,1)
              call edisp(iuout,outs)
              modgeo=.true.
            endif
   97     continue
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
        write(T14,'(2a)') T12(1:12),'  '
        CALL EASKS(T14,' ','Name of the construction?',
     &      14,' ','Construction name',IER,3)
        write(T12,'(a)') T14(1:12)
        call st2name(T12,MLCN)
        if(MLCSYM(1:9).EQ.'SYMMETRIC')then
          continue
        elseif(MLCSYM(1:12).EQ.'NONSYMMETRIC')then
          continue
        else

C Update current DESC then find matching MLC and copy data associated
C with the current MLC to the linked MLC. Lastly re-establish tokens
C for the current item.
          WRITE(DESC(IFOC),89)MLCN,OTF,OPT,MLCSYM
          do 795 k=1,NMLC
            if(DESC(k)(1:12).eq.MLCSYM(1:12))then
              call parsemlcdesc(DESC(k),T12,OTF,OPT,MLCSYM)
              WRITE(T12OTH,'(A)')DESC(IFOC)(1:12)
              WRITE(DESC(k),89)T12,OTF,OPT,T12OTH
            endif
  795     continue
          call parsemlcdesc(DESC(IFOC),MLCN,OTF,OPT,MLCSYM)
        endif
        moddb = .true.
      ELSEIF(ILED.EQ.2)THEN

C Select Opaque or transparent or complex fenestration construction.
        H(1)='In order to make cross checking easier the user is'
        H(2)='asked to define the construction as either'
        H(3)='OPAQUE or TRANSPARENT. A complex fenestration '
        H(4)='construction, CFC, can also be chosen to model'
        H(5)='windows with slat type operable blinds.' 

        CALL EASKABC(' ','Please choose one type :',
     &    'Opaque construction','Transparent construction',
     &    'Complex Fenestration Construction',IW,5)

        IF(IW.EQ.1)THEN
          OTF='OPAQ' 
          OPT='OPAQUE'
        ELSEIF(IW.EQ.2)THEN
          OTF='TRAN' 
        ELSEIF(IW.EQ.3)THEN
          OTF='CFC '
          OPT='USE GSLedit'
          H(1)='Complex Fenestration Construction (CFC)'
          H(2)=''
          H(3)='A CFC allows for a more accurate treatment of'
          H(4)='modeling windows with or without shading devices.'
          H(5)=''
          H(6)='A CFC surface has the following attributes:'
          H(7)=' '
          H(8)='-Exterior, vertical surface (IE=0)'
          H(9)='-Each glazing/shading layer is separated by gas gap'
          H(10)='-Currently limited to one shading layer per CFC'
          H(11)=' '
          H(12)='For an example of usage of CFCs see'
          H(13)='"Appendix F: CFC User Reference" from'
          H(14)='http://uwspace.uwaterloo.ca'
          H(15)='/bitstream/10012/4164/1/Lomanowski_Bartosz.pdf'
          H(16)=''
          H(17)='The Glazing Shading Layer editor (GSLedit) used for'
          H(18)='importing CFC input properties can be downloaded from'
          H(19)='here:'
          H(20)='TBA'
          CALL PHELPD('CFC instructions',20,'-',0,0,IER)

        ENDIF
        moddb = .true.

C At this point need to check to see if this change needs to be
C applied to surfaces within the model. If there are zones loop
C through them and see if any surfaces have this construction 
C attribute. If they do reset SSOTF to OTF and update the geometry
C file.

C << good place to try and use connection based information >>

C << if the writing of geometry files can use something other
C << than G1 common blocks then there is no need to scan the
C << geometry file before writing it out. 
        if(ncomp.gt.0)then
          do 96 iz=1,ncomp
            write(zn,'(A)') zname(IZ)
            write(outs,'(3a)') ' Checking: ',zn(1:lnblnk(zn)),
     &         ' attributes...'
            CALL USRMSG(' ',outs,'-')
            LTMP=LGEOM(IZ)
              call georead(IUF,LTMP,IZ,1,iuout,IER)

   93       FORMAT('           Sur|  Area  |Azim|Elev| surface    |',
     &        'geometry| multilayer  |environment')
            WRITE(outs,93)
            call edisp(iuout,outs)
   94       FORMAT('              |  m^2   |deg |deg | name       |',
     &        'type|loc| constr name |other side ')
            WRITE(outs,94)
            call edisp(iuout,outs)
            modgeo=.false.
            do 91 i=1,nsur
              ioc=IZSTOCN(IZ,i)
              if(SSMLCN(ioc)(1:12).eq.DESC(IFOC)(1:12))then
                write(sn,'(a)') SSNAME(ioc)
                write(SSOTF(ioc),'(a)') OTF
                WRITE(outs,'(a,I3,1X,F7.2,F5.0,F5.0,1X,A12,1X,A4,
     &            1X,A4,1X,A12,1X,A13)')'updated:   ',I,
     &            SSNA(ioc),SSPAZI(ioc),SSPELV(ioc),SN,SSOTF(ioc),
     &            SSVFC(ioc),SSMLCN(ioc)(1:12),SSOTHER(ioc,1)(1:13)
                call edisp(iuout,outs)
                modgeo=.true.
              endif
   91       continue

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
   96     continue
          call usrmsg(
     &      'Zone construction files may need to be updated',
     &      'after the database modifications are completed.','W')
          call usrmsg(
     &      'You next task it to select an optical property set for',
     &      'this construction (option c).','W')
        endif
      ELSEIF(ILED.EQ.3)THEN

C Edit optical properties. Also check to see that the number of layers
C and reference for materials matches.
        IF((OTF(1:4).NE.'OPAQ').AND.(OTF(1:4).NE.'CFC '))THEN
          H(1)='In order to allow for the optical properties to be'
          H(2)='used in the model description and to be acted on'
          H(3)='by control laws the relevant optical property id  '
          H(4)='(from the optical properties db) is required.'
          CALL PHELPD('optical selection',4,'-',0,0,IER)

   77     CALL EDWINO(SOPT,IER)
          CALL EROPTDB(ITRC,iuout,SOPT,GDESCR,IER)
          IF(IER.EQ.2)THEN
            dok=.true.
            h(1)='Could not recover that particular optical property'
            h(2)='set. You might want to select a different one. '
            CALL ASKOK(' ',' Not found... try again? ',OK,dok,2)
            IF(OK)GOTO 77
          ENDIF
          OPT=SOPT
          IF(LAYERS(IFOC).NE.NTL)CALL USRMSG(
     &      ' The number of layers in the optical and ',
     &      ' construction databases differ!','W')
        ENDIF
        moddb = .true.
      ELSEIF(ILED.EQ.5)THEN

C Allow linking to inverted construction.
        if(MLCSYM(1:9).EQ.'SYMMETRIC')then
          call usrmsg(
     &      'There is no need to link to an `inverted` version of this',
     &      'construction. It is symmetric and ok for partitions.','W')
        elseif(MLCSYM(1:12).EQ.'NONSYMMETRIC')then
          dok=.true.
          h(1)='This construction is not symmetrical and needs to '
          h(2)='be linked to a reversed copy of itself if it is going'
          h(3)='to be used as a partition between two zones. '
          CALL ASKOK('Would you like to link this construction to',
     &               'another which has `inverted` layers?',OK,dok,3)
          if(OK)then
            call epkmlc(iwhich,
     &        'Link with which `inverted` construction?',' ',ierr)
            if(LAYERS(IFOC).eq.LAYERS(iwhich))then

C Update the linked construction to point back to the current item.
              call parsemlcdesc(DESC(IWHICH),T12,OTF,OPT,MLCSYM)
              WRITE(T12OTH,'(A)')DESC(IFOC)(1:12)
              WRITE(DESC(IWHICH),89)T12,OTF,OPT,T12OTH

C Extract strings from focus DESC prior to updating its SYM tag.
              call parsemlcdesc(DESC(IFOC),T12,OTF,OPT,MLCSYM)
              WRITE(T12OTH,'(A)')DESC(IWHICH)(1:12)
              WRITE(MLCSYM,'(A)')DESC(IWHICH)(1:12)
              WRITE(DESC(IFOC),89)T12,OTF,OPT,T12OTH
              moddb = .true.
              showother = .true.
            else
              write(outs,'(5a)') 'Constructions ',DESC(IFOC)(1:36),
     &          ' and ',DESC(iwhich)(1:36),
     &          ' have different numbers of layers.'
              call edisp(iuout,outs)
            endif
          else

C User declines link to recast MLCSYM and update DESC for the current
C construction and reset showother.
            call ismlcsymmetric(ifoc,layermatch)
            if(layermatch)then
              MLCSYM='SYMMETRIC   '
            else
              MLCSYM='NONSYMMETRIC'
            endif
            WRITE(DESC(ifoc),89)MLCN,OTF,OPT,MLCSYM
            moddb = .true.
            showother = .false.
          endif
        endif
      ELSEIF(ILED.GT.8.AND.ILED.LE.NLED-3)THEN

C Ask if user wishes to browse through materials to find suitable
C reference.
        dok=.true.
        h(1)='To change to a different material select it from the'
        h(2)='the list within the current materials database.'
        h(3)='If you need an air gap, choose one from the GAPS '
        h(4)='category (you may be asked for the air gap resistance'
        h(5)='values for various orientations of the construction.'
        h(6)=' '
        h(7)='If you want to keep the current material just say NO.'
        CALL ASKOK('Change the materials db reference via browsing',
     &             'the material db? (NO keeps the current material)',
     &             OK,dok,7)
        if(OK)then
          iwhich=0
          CALL ELISTMAT(iwhich,chdb,'-',matarrayindex,IER)
          if(iwhich.eq.0)then
            H(1)='If air is correct then accept, otherwise'
            H(2)='reselect from the list or abort the task.'
            CALL EASKABC('Your selection is `0` i.e. air. Options:',
     &        ' ','accept','reselect material','abort',iwair,2)
            if(iwair.eq.2)then
              CALL ELISTMAT(iwhich,chdb,'-',matarrayindex,IER)
            elseif(iwair.eq.3)then
              ILED=-1
              GOTO 3
            endif
          elseif(iwhich.eq.-99)then
            continue
          endif
        else

C Use the current material array index.
          IWHICH=IPR(IFOC,ILED-8)
          if(IWHICH.ge.0)then
            matarrayindex=mathash(IWHICH)  ! find matching array index
            IPRMAT(IFOC,ILED-8)=matarrayindex
          endif
        endif
        IPR(IFOC,ILED-8)=IWHICH

C Make up LAYDESC and PNAM as combination of matnam and matdoc based on
C the newly selected material. This is necessary so that other
C code can recover the details of a -99 item and to update the
C menu display.
        if(matarrayindex.gt.0)then
          lnam=lnblnk(matname(matarrayindex))
          lfordoc = 72 - (lnam +4)    ! space left for doc
          write(LAYDESC(IFOC,ILED-8),'(3a)') 
     &      matname(matarrayindex)(1:lnam),' : ',
     &      matdoc(matarrayindex)(1:lfordoc)
          write(PNAM(ILED-8),'(3a)')
     &      matname(matarrayindex)(1:lnam),' : ',
     &      matdoc(matarrayindex)(1:lfordoc)
          IPRMAT(IFOC,ILED-8)=matarrayindex ! update array index for this layer
        endif

C If an air gap ask for the three R values.
        IF(IPR(IFOC,ILED-8).EQ.0)THEN
          H(1)='A set of default assumptions on the R value of'
          H(2)='an air gap is requested, in case the construction '
          H(3)='is used in different orientations.'
          H(4)='This information will allow for the automatic'
          H(5)='creation of zone construction files with sub-'
          H(6)='stantially less user interaction.'
          WRITE(outs,118)DESC(IFOC)(1:LNBLNK(DESC(IFOC)))  
  118     FORMAT('Default R value for air gap in ',a)
          if(DRAIR(IFOC,ILED-8,1).lt.0.001)then
             VAL=0.17
          else
             VAL=DRAIR(IFOC,ILED-8,1)
          endif
          CALL EASKR(VAL,outs,
     &         ' if the orientation is vertical          : ',
     &         0.0,'W',99.9,'W',0.17,'def air gap R',IER,6)
          DRAIR(IFOC,ILED-8,1)=VAL
          if(DRAIR(IFOC,ILED-8,2).lt.0.001)then
             VAL=0.17
          else
             VAL=DRAIR(IFOC,ILED-8,2)
          endif
          CALL EASKR(VAL,outs,
     &         ' if the orientation is horizontal        : ',
     &         0.0,'W',99.9,'W',0.17,'def air gap R',IER,5)
          DRAIR(IFOC,ILED-8,2)=VAL
          if(DRAIR(IFOC,ILED-8,3).lt.0.001)then
             VAL=0.17
          else
             VAL=DRAIR(IFOC,ILED-8,3)
          endif
          CALL EASKR(VAL,outs,
     &         ' if the orientation is sloped or UNKNOWN : ',
     &         0.0,'W',99.9,'W',0.17,'def air gap R',IER,5)
          DRAIR(IFOC,ILED-8,3)=VAL
        ELSEIF(IPR(IFOC,ILED-8).EQ.-99)THEN

C We got the new item from ELISTMAT call so use matarryindex.
C Find the typical thickness and if [g] or [h] gap resistance.
          if(matarrayindex.ne.0)then
            guessthick=matdbthick(matarrayindex)
            write(outs,'(a,f6.1,a)') 
     &        'Material selected has a thickness of ',guessthick,'mm.'
            call edisp(iuout,outs)
            if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &         matopaq(matarrayindex)(1:1).eq.'h')then
              if(closemat2)then
                DRAIR(IFOC,ILED-8,1)=matgapares(matarrayindex,1)
                DRAIR(IFOC,ILED-8,2)=matgapares(matarrayindex,2)
                DRAIR(IFOC,ILED-8,3)=matgapares(matarrayindex,3)
              endif
            endif
          endif
        ELSE

C Non zero legacy index so assign thickness. And check if the
C material is a [g] or [h] and might have air gap values.
          DRAIR(IFOC,ILED-8,1)=0.0
          DRAIR(IFOC,ILED-8,2)=0.0
          DRAIR(IFOC,ILED-8,3)=0.0
          guessthick=matdbthick(matarrayindex)
          write(outs,'(a,f6.1,a)') 
     &      'Material selected has a thickness of ',guessthick,'mm.'
          call edisp(iuout,outs)
          if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &       matopaq(matarrayindex)(1:1).eq.'h')then
            if(closemat2)then
              DRAIR(IFOC,ILED-8,1)=matgapares(matarrayindex,1)
              DRAIR(IFOC,ILED-8,2)=matgapares(matarrayindex,2)
              DRAIR(IFOC,ILED-8,3)=matgapares(matarrayindex,3)
            endif
          endif
        ENDIF

        H(1)='The thickness of the layer is normally between'
        H(2)='0.001 m and 0.500 mm. It is held in the file as'
        H(3)='metres.'
        VAL=DTHK(IFOC,ILED-8)*1000.0
        CALL EASKR(VAL,' ',' Layer thickness (mm): ',
     &    1.0,'W',300.0,'W',100.0,'layer thickness mm',IER,3)
        DTHK(IFOC,ILED-8)=VAL*0.001
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


C ************* MKBPRC 
C MKBPRC: Creates a binary materials db based on the
C current contents of an ASCII file LASCI opened on unit IFA.
C Note: this subroutine is being depreciated because models
C can use ascii version of materials database.  If we could not
C convert pass back ier as -2.
      SUBROUTINE MKBPRC(IFA,LASCI,IER)
#include "esprdbfile.h"
      integer icln
      real val
      DIMENSION ICLN(30),VAL(6)
      CHARACTER IDESC*72,NDESC*72,OUTSTR*124
      character lworking*144,fs*1
      logical unixok

C Parameters
      integer ifa  ! file unit
      character LASCI*144 ! ascii file to scan
      integer ier ! error state

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C The binary file will be overwritten.
C Check the value of whichdbpath variable to see what to do with
C this file. If local or absolute path then use eforan. If in
C the standard location then create lworking which has path
C prepended.
      if(ipathmat.eq.0.or.ipathmat.eq.1)then
        ier=0
        call EFOPRAN(IFMAT,LFMAT,40,4,IER)
      elseif(ipathmat.eq.2)then
        ier=0
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    LFMAT(1:lnblnk(LFMAT))
        call EFOPRAN(IFMAT,lworking,40,4,IER)
      endif
      if(ier.ne.0)return

C Open the ASCII file for reading.
      CALL EFOPSEQ(IFA,LASCI,1,IER)

C Proceed assuming that we are reading a legacy ascii materials
C database file. If the materials arrays are filled then we could
C write directly from memory.
      CALL STRIPC(IFA,OUTSTR,0,ND,1,'no classes',IER)
      IF(OUTSTR(1:5).eq.'*Mate')then
        call usrmsg(
     &    'The ascii file is a newer rather than a legacy',
     &    'materials file format. Abandoning conversion.','W')
        ier=-2
        RETURN
      endif
      K=0
      CALL EGETWI(OUTSTR,K,NCLASS,1,1,'-','no classes',IER)

      DO 10 I=1,NCLASS
        ICLASS=I
        CALL STRIPC(IFA,OUTSTR,0,ND,1,'IC NE NDESC',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(OUTSTR,K,IC,0,30,'F','class',IER)
        CALL EGETWI(OUTSTR,K,NE,0,19,'-','no elem',IER)
        CALL EGETRM(OUTSTR,K,NDESC,'-','description',IER)
        ICLN(ICLASS)=IC
        IREC=601

C Write number of classifications and number of items in each.
        WRITE(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1001)ICLASS,
     &        (ICLN(K),K=1,30)
        IREC=ICLASS*20-19
        NREC=IREC

C Write values for classification.
        WRITE(IFMAT,REC=NREC,IOSTAT=ISTAT,ERR=1001)NE,NDESC
        DO 20 J=1,NE
          IREC=IREC+1
          CALL STRIPC(IFA,OUTSTR,0,ND,1,'NCODE,IDESC',IER)
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWI(OUTSTR,K,NCODE,0,600,'F','class',IER)
          CALL EGETRM(OUTSTR,K,IDESC,'W','cl description',IER)

          CALL STRIPC(IFA,OUTSTR,0,ND,1,'NCODE,IDESC',IER)
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWR(OUTSTR,K,VAL(1),0.,0.,'-','VAL 1',IER)
          CALL EGETWR(OUTSTR,K,VAL(2),0.,0.,'-','VAL 2',IER)
          CALL EGETWR(OUTSTR,K,VAL(3),0.,0.,'-','VAL 3',IER)
          CALL EGETWR(OUTSTR,K,VAL(4),0.,0.,'-','VAL 4',IER)
          CALL EGETWR(OUTSTR,K,VAL(5),0.,0.,'-','VAL 5',IER)
          CALL EGETWR(OUTSTR,K,VAL(6),0.,0.,'-','VAL 6',IER)

C Write values for layer.
          WRITE(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1002)
     &          (VAL(IL),IL=1,6),IDESC
   20   CONTINUE
   10 CONTINUE
      
  99  CALL ERPFREE(IFA,ISTAT)
      RETURN

C File errors.
 1001 CALL USRMSG(' NE,NDESC write error in ',LFMAT,'W')
      IER=1
      goto 99
 1002 CALL USRMSG(' Layer data write error in ',LFMAT,'W')
      IER=1
      goto 99
    
      END

C ************* MKAPRC 
C MKAPRC: Creates an ascii materials db based on the
C current contents of binary materials db.
      SUBROUTINE MKAPRC(LASCI,IFA,IER)
#include "esprdbfile.h"
      integer lnblnk  ! function definition
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      integer icln
      integer ier
      real val
      DIMENSION ICLN(30),VAL(6)
      CHARACTER IDESC*72,NDESC*72
      character lworking*144,fs*1
      logical unixok

C Parameters
      integer IFA  ! file unit
      character LASCI*144  ! file name

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Open the binary materials database (in clase it was closed elsewhere).

C Check the value of whichdbpath variable to see what to do with
C this file. If local or absolute path then use efopseq. If in
C the standard location then create lworking which has path
C prepended.
      CALL ERPFREE(IFMAT,ISTAT)
      if(ipathmat.eq.0.or.ipathmat.eq.1)then
        ier=0
        call EFOPRAN(IFMAT,LFMAT,40,1,IER)
      elseif(ipathmat.eq.2)then
        ier=0
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    LFMAT(1:lnblnk(LFMAT))

C Debug.
        write(6,*) 'working materials ',lworking
        call EFOPRAN(IFMAT,lworking,40,1,IER)
      endif
      if(ier.ne.0)return
      MATDBOK=.TRUE.

C The ASCII file will be overwritten if it exists.
      CALL EFOPSEQ(IFA,LASCI,4,IER)
      
C Write out a file header.
      WRITE(IFA,30)LASCI(1:lnblnk(LASCI))
  30  FORMAT('# materials db defined in ',A)

C Check binary contents.
      IREC=601
      READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NCLASS,(ICLN(K),K=1,30)
      IF(NCLASS.LE.0)THEN
        CALL USRMSG(' binary materials database empty! ',' ','W')
        IER=1
        RETURN
      ENDIF

C Proceed.
      WRITE(IFA,'(I5,4X,A)')NCLASS,'#  number of classifications'
      DO 10 I=1,NCLASS
        IC=ICLN(I)
        IREC=IC*20-19
        READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NE,IDESC
        WRITE(IFA,'(a)') '# class|no materials|description'
        WRITE(IFA,'(2a)') 
     &    '# for each item index|description| conduc. density. ',
     &    'specific ht, emis. abs. vapour resistivity.'
        WRITE(IFA,'(I5,I5,3X,A)')IC,NE,IDESC(1:lnblnk(IDESC))
        IF(NE.GT.0)then
          DO 20 J=1,NE
            IREC=IREC+1
            READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1001)(VAL(IL),IL=1,6),
     &        NDESC
            NCODE=IREC-1
            WRITE(IFA,'(I5,3X,A)') NCODE,NDESC(1:lnblnk(NDESC))
            WRITE(IFA,13) (VAL(K),K=1,6)
   13       FORMAT(F13.3,2(', ',F9.3),2(', ',F6.3),', ',F10.3)
   20     CONTINUE
        ENDIF
   10 CONTINUE

  99  CALL ERPFREE(IFA,ISTAT)
      RETURN

C File errors.
 1000 CALL USRMSG(' NE,NDESC read error in ',LFMAT,'W')
      IER=1
      goto 99
 1001 CALL USRMSG(' Layer data write error in ',LFMAT,'W')
      IER=1

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
      COMMON/C24/IZSTOCN(MCOM,MS)

      character mlcname*12
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
            if(mlcname(1:lm).eq.SSMLCN(ic)(1:lnblnk(SSMLCN(ic))))then
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
      
C ******************* GVALUE *******************
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
      common/pophelp/h(60)
      character h*72
      DIMENSION VERT(35)
      CHARACTER VERT*68,KEY*1
C      character outs*124

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
     &        matname(L)(1:24)
   24       FORMAT(A1,F8.3,F6.0,F7.0,F5.2,F5.2,F7.0,1X,A)
          elseif(matopaq(L).eq.'-')then
            WRITE(VERT(M),25)KEY,matdbcon(L),matdbden(L),
     &        matdbsht(L),matdboute(L),matdbouta(L),matdbdrv(L),
     &        matname(L)(1:8),': ',matdoc(L)(1:17)
   25       FORMAT(A1,F8.3,F6.0,F7.0,F5.2,F5.2,F7.0,1X,3A)
          elseif(matopaq(L).eq.'t')then
            WRITE(VERT(M),25)KEY,matdbcon(L),matdbden(L),
     &        matdbsht(L),matdboute(L),matdbouta(L),matdbdrv(L),
     &        matname(L)(1:8),': ',matdoc(L)(1:17)
          elseif(matopaq(L).eq.'g')then
            WRITE(VERT(M),25)KEY,matdbcon(L),matdbden(L),
     &        matdbsht(L),matdboute(L),matdbouta(L),matdbdrv(L),
     &        matname(L)(1:8),': ',matdoc(L)(1:17)
          elseif(matopaq(L).eq.'h')then
            WRITE(VERT(M),25)KEY,matdbcon(L),matdbden(L),
     &        matdbsht(L),matdboute(L),matdbouta(L),matdbdrv(L),
     &        matname(L)(1:8),': ',matdoc(L)(1:17)
          else
            WRITE(VERT(M),24)KEY,matdbcon(L),matdbden(L),
     &        matdbsht(L),matdboute(L),matdbouta(L),matdbdrv(L),
     &        matname(L)(1:24)
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
      H(1)='This menu allows existing materials to be viewed and'
      H(2)='one of them selected.'

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
        CALL PHELPD('materials section',2,'-',0,0,IER)
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

