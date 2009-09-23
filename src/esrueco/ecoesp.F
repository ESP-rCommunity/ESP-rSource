C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001 as well as
C Stephane.Citherlet@epfl.ch

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

C ************* ELISTCMOD 
C ELISTCMOD: Controls display and editing of materials.
C If ACTION = 'M' then include editing if ACTION = '-' only
C allow choice.  Chgdb flagged true if an mod made to db during session.

C << there might be an alternative to this for materials arrays >>

      SUBROUTINE ELISTCMOD(iwhich,chgdb,ACTION,IER)
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/pophelp/H(60)
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit

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
      logical closemat1,closemat2

      LOGICAL     OK,dok,MODDB,chgdb
      DIMENSION ICLN(30),CLSDES(30),PDBM(35),CLSSEL(30),ICLSSEL(30)
      DIMENSION EDBCON(19),EDBDEN(19),EDBSHT(19),EE(19),EA(19),EDRV(19)
      DIMENSION ENAM(19),IDBE(19)
      CHARACTER ACTION*1,CLSSEL*36
      CHARACTER CLSDES*30,IDESC*72,H*72,TITL*72,NAM*72,ENAM*72
      CHARACTER OUTSTR*124,KEY*1,PDBM*64,outs*124
      CHARACTER xfile*144,tg*1,delim*1

C Assume no changes to db amd user has not entered password.
      MODDB=.FALSE.
      chgdb=.false.

C Check that the primitive db is not empty.
    4 IER=0
      IREC=601
      READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NCLASS,(ICLN(I),I=1,30)
      IF(NCLASS.LE.0)THEN
        CALL USRMSG(' Materials db is empty.',' ','W')
        IER=1
        RETURN
      ENDIF

C Find version of materials database.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)

C Setup for menu.
C Gather the names of the various classes and present this list.
  40  ICO=-1
      CLSDES(1)=  '  Description        No. Items'
      M=1
      DO 44 I=1,NCLASS
        IC=ICLN(I)
        IREC=IC*20-19
        READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NE,IDESC
        write(CLSSEL(I),'(A)') IDESC(1:36)
        IF(NE.GT.0)THEN
          M=M+1
          CALL EMKEY(M-1,KEY,IER)
          WRITE(CLSDES(M),'(A1,1X,A,2X,I2)')KEY,IDESC(1:24),NE
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

C Now display the menu.
      CALL EMENU('Materials Classes',CLSDES,NCO,ICO)
      IF(ICO.EQ.NCO)THEN
        RETURN
      ELSEIF(ICO.EQ.(NCO-1))THEN
        H(1)='The classifications shown in the display include up to'
        H(2)='18 elements. To get a listing of the items included'
        H(3)='select one of the classifications.'
        CALL PHELPD('materials menu',3,'-',0,0,IER)
      ELSEIF(ICO.EQ.(NCO-2))THEN

C List one or more classifications.
        INPIC=NCLASS
        h(1)='You may select one or more classes to list. '
        CALL EPICKS(INPIC,ICLSSEL,' ',' Which classes to list:',
     &        36,NCLASS,CLSSEL,'Primative classes',IER,1)
        IF(INPIC.EQ.0)GOTO 40
        h(1)=' Summary reporting can be shown in the text feedback'
        h(2)=' or a text file.'
        CALL EASKABC(' Reporting to:',' ',
     &      'text feedback','summary file','cancel',irpt,2)
        if(irpt.eq.1)then
          itu = iuout
        elseif(irpt.eq.2)then
          itu = ixunit
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','mat db text',IER)
        elseif(irpt.eq.3)then
          goto 40
        endif
        call edisp(itu,'In the primitive elements database: ')
        call edisp(itu, LFMAT)
        call edisp(itu,' ')
        do 42 list=1,INPIC
          IC=ICLSSEL(list)
          IREC=IC*20-19
          READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NE,IDESC
          IF(NE.GT.0)THEN
            WRITE(outs,'(a,a,a,i2,a)')' Classification: ',
     &        IDESC(1:lnblnk(IDESC)),' (',IC,')'
            call edisp(itu,outs)
            call edisp(itu,' ')
            call edisp(itu,
     &      'Index|Con-   |Den- |Specif|IR  |Solr|Diffus|Description')
            call edisp(itu,
     &      '     |duct.  |sity |heat  |emis|abs |resist|of material')
            IDB=IC*20-20
            DO 47 J=1,NE
              IDB=IDB+1
              if(closemat1.or.closemat2)then
                legmat=IDB   ! which legacy index
                if(legmat.ge.0)then
                  matarrayindex=mathash(legmat)  ! find matching array index
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht to mimic erpcdb
                  if(matarrayindex.eq.0)then
                    DBCON=0.0; DBDEN=0.0; DBSHT=0.0 
                    E=0.99; A=0.99; DRV=1.0
                    TITL='GAPS'; NAM='AIR'
                  else
                    DBCON=matdbcon(matarrayindex)
                    DBDEN=matdbden(matarrayindex)
                    DBSHT=matdbsht(matarrayindex)
                    E=matdbine(matarrayindex)
                    A=matdbina(matarrayindex)
                    DRV=matdbdrv(matarrayindex)
                    write(TITL,'(a)') 'placeholder'
                    write(NAM,'(a)') matname(matarrayindex)(1:32)
                  endif
                endif
              else
                CALL ERPCDB(IFMAT,IDB,iuout,IER,DBCON,DBDEN,DBSHT,E,A,
     &            DRV,TITL,NAM)
              endif
              WRITE(outs,46)IDB,DBCON,DBDEN,DBSHT,E,A,DRV,NAM(1:36)
   46         FORMAT(I4,F9.3,F6.0,F7.0,F5.2,F5.2,F7.0,1X,A)
              call edisp(itu,outs)
   47       CONTINUE
          ELSE
            call edisp(itu,' No items in this classification.')
          ENDIF
   42   continue
        call edisp(itu,' ')
        call edisp(itu,' Units: Conduct W/(m deg.C), Density kg/m**3')
        call edisp(itu,'        Specific Heat J/(kg deg.C) ')
        if(irpt.eq.2)then
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','prim db',IER)
        endif
      ELSEIF(ICO.EQ.(NCO-3))THEN

C Rewrite the last record so that when file is closed it contains
C all the information.
        if(NCLASS.LT.30)then
          NCLASS=NCLASS+1
          ICLN(NCLASS)=NCLASS
          IREC=601
          WRITE(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1001)NCLASS,
     &          (ICLN(I),I=1,30)
          H(1)='This description may be up to 72 char. '
          CALL EASKS(IDESC,' Classification name ?',' ',
     &      72,' ','class name',IER,1)
          ILNE=1
          IDBE(ILNE)=(NCLASS*20-19)+1
          ENAM(ILNE)='new_element'
          EDBCON(ILNE)=1.
          EDBDEN(ILNE)=1.
          EDBSHT(ILNE)=1.
          EE(ILNE)=0.9
          EA(ILNE)=0.5
          EDRV(ILNE)=10.

          IREC=NCLASS*20-19
          NREC=IREC
          WRITE(IFMAT,REC=NREC,IOSTAT=ISTAT,ERR=1001)ILNE,IDESC
          DO 41 J=1,ILNE
            IREC=IREC+1
            WRITE(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1002)EDBCON(J),
     &            EDBDEN(J),EDBSHT(J),EE(J),EA(J),EDRV(J),ENAM(J)
   41     CONTINUE
          chgdb=.true.
        endif
        goto 40
      ELSEIF(ICO.GT.1.AND.ICO.LT.(NCO-4))THEN
        
C Loop through and read in data for this classification into local
C arrays for manipulation.
        IC=ICO-1
        IREC=IC*20-19
        READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NE,IDESC
        IF(NE.GT.0)THEN
          IDB=IC*20-20
          DO 20 J=1,NE
            IDB=IDB+1
            if(closemat1.or.closemat2)then
              legmat=IDB   ! which legacy index
              if(legmat.ge.0)then
                matarrayindex=mathash(legmat)  ! find matching array index
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht to mimic erpcdb
                if(matarrayindex.eq.0)then
                  DBCON=0.0; DBDEN=0.0; DBSHT=0.0 
                  E=0.99; A=0.99; DRV=1.0
                  TITL='GAPS'; NAM='AIR'
                else
                  DBCON=matdbcon(matarrayindex)
                  DBDEN=matdbden(matarrayindex)
                  DBSHT=matdbsht(matarrayindex)
                  E=matdbine(matarrayindex)
                  A=matdbina(matarrayindex)
                  DRV=matdbdrv(matarrayindex)
                  write(TITL,'(a)') 'placeholder'
                  write(NAM,'(a)') matname(matarrayindex)(1:32)
                endif
              endif
            else
              CALL ERPCDB(IFMAT,IDB,iuout,IER,DBCON,DBDEN,DBSHT,E,A,
     &          DRV,TITL,NAM)
            endif
            IDBE(J)=IDB
            EDBCON(J)=DBCON
            EDBDEN(J)=DBDEN
            EDBSHT(J)=DBSHT
            EE(J)=E
            EA(J)=A
            EDRV(J)=DRV
            ENAM(J)=NAM
   20     CONTINUE
        ELSE
         CALL USRMSG(' No items in this classification.',' ','W')
         GOTO 40
        ENDIF
C Set_up the default display
      Idisp=2

C Display the elements of the classification in a menu.
    3   PDBM(1)= '  Units:  Conductivity W/(m deg.C), Density kg/m**3'
        PDBM(2)= '         Specific Heat J/(kg deg.C) '
        WRITE(PDBM(3),33)IDESC(1:lnblnk(IDESC)),IC
   33   FORMAT    ('a Classification: ',A,' (',I2,')')
        WRITE(PDBM(4),'(A,I2)')'  Number of elements: ',NE
        PDBM(5)=   '  ___________________________________'

C Display thermal properties, starting from letter b.
        IF (Idisp.EQ.1) THEN
          PDBM(6)=
     &    ' |Prm|Con-  |Den- |Specif|IR  |Solr|Diffu|Description  '
          PDBM(7)=
     &    ' |db |duct. |sity |heat  |emis|abs |resis|of material  '
          M=7
          DO 30 J=1,NE
            M=M+1
            CALL EMKEY(M-6,KEY,IER)
            WRITE(PDBM(M),24)KEY,IDBE(J),EDBCON(J),EDBDEN(J),EDBSHT(J),
     &                     EE(J),EA(J),EDRV(J),ENAM(J)(1:20)
   24       FORMAT(A1,1X,I3,F7.2,F6.0,F7.0,F5.2,F5.2,F7.0,1X,A)
   30     CONTINUE
        ELSEIF (Idisp.EQ.2) THEN

C Display Ecobalance properties
          PDBM(6)=
     &    ' |Prm|Con-  |Den- |Specif|IR  |Solr|Diffu|Description  '
          PDBM(7)=
     &    ' |db |duct. |sito |heat  |emis|abs |resis|of material  '
          M=7
          DO 31 J=1,NE
            M=M+1
            CALL EMKEY(M-6,KEY,IER)
            WRITE(PDBM(M),25)KEY,IDBE(J),EDBCON(J),EDBDEN(J),EDBSHT(J),
     &                     EE(J),EA(J),EDRV(J),ENAM(J)(1:20)
   25       FORMAT(A1,1X,I3,F7.2,F6.0,F7.0,F5.2,F5.2,F7.0,1X,A)
   31     CONTINUE
        ELSEIF (Idisp.EQ.3) THEN

C       Display Acoustic properties
          PDBM(6)=
     &    ' |Prm|Con-  |Den- |Specif|IR  |Solr|Diffu|Description  '
          PDBM(7)=
     &    ' |db |duct. |sito |heat  |emis|abs |resis|of material  '
          M=7
          DO 32 J=1,NE
            M=M+1
            CALL EMKEY(M-6,KEY,IER)
            WRITE(PDBM(M),26)KEY,IDBE(J),EDBCON(J),EDBDEN(J),EDBSHT(J),
     &                     EE(J),EA(J),EDRV(J),ENAM(J)(1:20)
   26       FORMAT(A1,1X,I3,F7.2,F6.0,F7.0,F5.2,F5.2,F7.0,1X,A)
   32     CONTINUE
        ELSEIF (Idisp.EQ.4) THEN

C Display Visible properties
          PDBM(6)=
     &    ' |Prm|Con-  |Den- |Specif|IR  |Solr|Diffu|Description  '
          PDBM(7)=
     &    ' |db |duct. |sito |heat  |emis|abs |resis|of material  '
          M=7
          DO 34 J=1,NE
            M=M+1
            CALL EMKEY(M-6,KEY,IER)
            WRITE(PDBM(M),27)KEY,IDBE(J),EDBCON(J),EDBDEN(J),EDBSHT(J),
     &                     EE(J),EA(J),EDRV(J),ENAM(J)(1:20)
   27       FORMAT(A1,1X,I3,F7.2,F6.0,F7.0,F5.2,F5.2,F7.0,1X,A)
   34     CONTINUE
        ENDIF
      
        NC=M
        PDBM(NC+1)=  '  __________________________     '
        if(ACTION.eq.'M'.or.ACTION.eq.'m')then
          PDBM(NC+2)='1 add/ delete element            '
          PDBM(NC+3)='2 copy element                   '
          PDBM(NC+4)='! update construction elements db'
        elseif(ACTION.eq.'-')then
          PDBM(NC+2)='                                 '
          PDBM(NC+3)='                                 '
          PDBM(NC+4)='                                 '
        endif
        IF (Idisp.EQ.1) THEN
          PDBM(NC+5)=  't display >> Thermal properties'
        ELSEIF (Idisp.EQ.2) THEN
          PDBM(NC+5)=  't display >> Ecobalance propert.'
        ELSEIF (Idisp.EQ.3) THEN
          PDBM(NC+5)=  't display >> Acoustic properties'
        ELSEIF (Idisp.EQ.4) THEN
          PDBM(NC+5)=  't display >> Visible properties'
        ENDIF
        PDBM(NC+6)=  '? help                           '
        PDBM(NC+7)=  '- exit this menu                 '
        NITMS=NC+7
        INO=-4

    2   CALL EMENU('Elements Database',PDBM,NITMS,INO)

        IF(INO.EQ.NITMS.OR.INO.EQ.0)THEN
          IF(MODDB.and.ACTION.eq.'M')THEN
            dok=.true.
            h(1)='Recent changes are held in memory and might be '
            h(2)='lost if you do not update the file. '
            CALL ASKOK('Changes have been made in db to save these ',
     &        'update the db.  Update the db now ? ',OK,dok,2)
            IF(.NOT.OK)GOTO 4
            chgdb=.true.
            IREC=IC*20-19
            NREC=IREC
            WRITE(IFMAT,REC=NREC,IOSTAT=ISTAT,ERR=1001)NE,IDESC
            DO 51 J=1,NE
              IREC=IREC+1
              WRITE(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1002)EDBCON(J),
     &              EDBDEN(J),EDBSHT(J),EE(J),EA(J),EDRV(J),ENAM(J)
   51       CONTINUE

C Rewrite the last record so that when file is closed it contains
C all the information.
            IREC=601
            WRITE(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1001)NCLASS,
     &            (ICLN(I),I=1,30)
            MODDB=.FALSE.
          ENDIF
          GOTO 4
        ELSEIF(INO.EQ.1.OR.INO.EQ.2)THEN
          GOTO 2
        ELSEIF(INO.eq.3)THEN

C Edit classification name.
          H(1)='A classification has a descriptive name (up to '
          H(2)='72 char).'
          CALL EASKS(IDESC,' ',' Classification name ? ',
     &      72,' ','class name',IER,2)
        ELSEIF(INO.GE.4.AND.INO.LE.7)THEN

C Go back to previous menu
          GOTO 2
        ELSEIF(INO.EQ.(NC+5))THEN

C Change the display
          Idisp = Idisp +1
          if (Idisp.gt.4) Idisp=1
        ELSEIF(INO.EQ.NITMS-1)THEN

C Display HELP
         H(1)='The information in this display is based on data from'
         H(2)='a primitive construction database.  Databases of this'
         H(3)='type are used to build a composite construction db.'
         H(4)='Data in this db rarely changes. However, users may '
         H(5)='wish to add additional elements or classifications to'
         H(6)='conform to materials in their region.  '
         H(7)='  '
         H(8)='Adding or Deleting an element is supported. The'
         H(9)='former creates an element at the end of the current'
         H(10)='classification which may then be edited. '
         H(11)=' '
         H(12)='You may copy an existing element into a new item at'
         H(13)='the end of the list. BE SURE TO ALTER ITS NAME! '
        ELSEIF(INO.EQ.NITMS-2)THEN

C Update material db with all elements in this classification and
C reset flag to show db is current with local arrays.
          IF(.NOT.MODDB)THEN
            CALL USRMSG(' ',' No changes in db...','-')
          ELSE
            dok=.true.
            h(1)='Recent changes will be lost unless you save them '
            h(2)='to file. '
            CALL ASKOK(' ','Do you want to update database?',OK,dok,2)
            IF(.NOT.OK)GOTO 2
            chgdb=.true.
            IREC=IC*20-19
            NREC=IREC
            WRITE(IFMAT,REC=NREC,IOSTAT=ISTAT,ERR=1001)NE,IDESC
            DO 50 J=1,NE
              IREC=IREC+1
              WRITE(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1002)EDBCON(J),
     &              EDBDEN(J),EDBSHT(J),EE(J),EA(J),EDRV(J),ENAM(J)
   50       CONTINUE

C Rewrite the last record so that when file is closed it contains
C all the information.
            IREC=601
            WRITE(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1001)NCLASS,
     &            (ICLN(I),I=1,30)
            MODDB=.FALSE.
          ENDIF
        ELSEIF(INO.EQ.NITMS-3)THEN

C Copy an existing element to a new one at end of classificaton.
          CALL USRMSG(' ',' Which item of list?','-')
          CALL EMENU('Copy Element',PDBM,NITMS,IW)
          IWHICH=IW-7
          IF(NE.LT.18)THEN
            ILNE=NE+1
            IDBE(ILNE)=(IC*20-19)+NE
            EDBCON(ILNE)=EDBCON(IWHICH)
            EDBDEN(ILNE)=EDBDEN(IWHICH)
            EDBSHT(ILNE)=EDBSHT(IWHICH)
            EE(ILNE)=EE(IWHICH)
            EA(ILNE)=EA(IWHICH)
            EDRV(ILNE)=EDRV(IWHICH)
            ENAM(ILNE)=ENAM(IWHICH)
            NE=NE+1
            MODDB=.TRUE.
            chgdb=.true.
          ELSE
            CALL USRMSG(' ',' Exceeds classification limit!','W')
            GOTO 3
          ENDIF
        ELSEIF(INO.EQ.NITMS-4)THEN

C Add or delete and element from db.
          h(1)='You cannot recover a deleted element.  After you '
          h(2)='copy an element, be sure to give it a unique name. '
          CALL EASKABC(' ','Element: ',' Delete existing element',
     &      ' Insert element @ next available slot','continue',IW,2)
          IF(IW.EQ.1)THEN

C Ask the user which one to remove then loop through elements and
C copy the contents of elements > IWHICH into the next lower one.
            CALL USRMSG(' ',' Which item of list? ','-')
            CALL EMENU('delete element',PDBM,NITMS,IW)
            dok=.false.
            h(1)='Pause in case you made a selection error. '
            CALL ASKOK(' ',' Are you sure?',OK,dok,1)
            IF(.NOT.OK)GOTO 2
            IWHICH=IW-7
            DO 791 IVV=IWHICH,NE-1
              IDBE(IVV)=IDBE(IVV+1)-1
              EDBCON(IVV)=EDBCON(IVV+1)
              EDBDEN(IVV)=EDBDEN(IVV+1)
              EDBSHT(IVV)=EDBSHT(IVV+1)
              EE(IVV)=EE(IVV+1)
              EA(IVV)=EA(IVV+1)
              EDRV(IVV)=EDRV(IVV+1)
              ENAM(IVV)=ENAM(IVV+1)
  791       CONTINUE
            NE=NE-1
            MODDB=.TRUE.
            chgdb=.true.
          ELSEIF(IW.EQ.2)THEN

C Add an element, initiate it to the values of last element in class.
            IF(NE.LT.18)THEN
              ILNE=NE+1
              IDBE(ILNE)=(IC*20-19)+NE
              ENAM(ILNE)='new_element'
              EDBCON(ILNE)=EDBCON(NE)
              EDBDEN(ILNE)=EDBDEN(NE)
              EDBSHT(ILNE)=EDBSHT(NE)
              EE(ILNE)=EE(NE)
              EA(ILNE)=EA(NE)
              EDRV(ILNE)=EDRV(NE)
              CALL EDPELEM(EDBCON(ILNE),EDBDEN(ILNE),EDBSHT(ILNE),
     &           EE(ILNE),EA(ILNE),EDRV(ILNE),ENAM(ILNE),IER) 
              NE=NE+1
              MODDB=.TRUE.
              chgdb=.true.
            ELSE
              CALL USRMSG(' ','Exceeds classification limit!','W')
              GOTO 3
            ENDIF
          ELSEIF(IW.EQ.3)THEN
            GOTO 3
          ENDIF
        ELSEIF(INO.EQ.NITMS-4)THEN
            GOTO 2
        ELSEIF(INO.GT.7.AND.INO.LT.NITMS-4)THEN

C Identified one of the elements to edit or select.
          IFOC=INO-7
          CALL EDPELEM(EDBCON(IFOC),EDBDEN(IFOC),EDBSHT(IFOC),
     &               EE(IFOC),EA(IFOC),EDRV(IFOC),ENAM(IFOC),IER) 
          if(ACTION.eq.'M')then
            MODDB=.TRUE.
            chgdb=.true.
          elseif(ACTION.eq.'-')then
            iwhich=IDBE(ifoc)
            lsn=MIN0(lnblnk(ENAM(IFOC)),32)
            write(outs,'(a,i3,a,a)') ' Use element (',iwhich,') ',
     &        ENAM(IFOC)(1:lsn)
            dok=.true.
            h(1)='Pause to confirm the selection. '
            CALL ASKOK(outs,'as your selection?',OK,dok,1)
            if(OK)return
          endif
        ENDIF
        GOTO 3
      else
        goto 40
      endif
      call usrmsg(' ',' ','-')
      goto 40
      
   99 RETURN

C Error messages.
 1000 WRITE(OUTSTR,8)IREC
    8 FORMAT(' Primitive db error NE,IDESC: Record',I6)
      CALL EDISP(iuout,OUTSTR)
      GOTO 99

 1001 WRITE(OUTSTR,9)IREC
    9 FORMAT(' Primitive db write error NE,IDESC: Record',I6)
      CALL EDISP(iuout,OUTSTR)
      GOTO 99

 1002 WRITE(OUTSTR,10)IREC
   10 FORMAT(' Primitive db element data write error: Record',I6)
      CALL EDISP(iuout,OUTSTR)
      GOTO 99
      END

C ********** opendb (based on code from eddb.F)
C Open materials constructions, multi-layer constructions and
C optical properties databases.
      subroutine opendb(ier)
#include "esprdbfile.h"

      common/OUTIN/IUOUT,IUIN
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Material data arrays.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      logical XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical closemat1,closemat2
      DIMENSION ICLN(30)

      character SOPT*12,outs*124,GDESCR*36

      IER=0

C Open currently named materials database.  Note that
C by a minor rewrite of ICLN in rec 601 we can get by with changing
C this to 24 words wide.
      CALL ERPFREE(IFMAT,ISTAT)
      call EFOPRAN(IFMAT,LFMAT,40,0,IER)
      IF(ier.ne.0)THEN
        write(outs,'(a,a,a)') ' No construction, materials db ',
     &     LFMAT(1:lnblnk(LFMAT)),' found!'
        call edisp(iuout,outs)
        MATDBOK=.FALSE.
      else

C Also check to see if the file has correct info. 
        IREC=601
        READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NCLASS,
     &    (ICLN(I),I=1,30)
        if(NCLASS.LE.0)then
          goto 1000   ! no classifications so try alternatives
        else
          MATDBOK=.TRUE.
          matver = 0.0
          goto 139  !  a binary file found so carry on.
        endif
 1000   CALL ERPFREE(IFMAT,ISTAT)  ! if a read error look at alternatives.
        call rascimat(IFMAT,LFMAT,IER)  ! try current ascii format
        if(ier.eq.-2)then
          ier=0
          call rlegacymat(IFMAT,LFMAT,ier)  ! try older ascii format
          if(ier.ne.0)then
            call usrmsg('No readable materials database was found',
     &                  'or file was corrupt','W')
            MATDBOK=.FALSE.
          else
            call eclose(matver,1.1,0.001,closemat1)
            call eclose(matver,1.2,0.001,closemat2)
            if(closemat1.or.closemat2)then
              call edisp(iuout,' Filled material data arrays...')
              MATDBOK=.TRUE.
            else
              call usrmsg(
     &          'No readable material database was found. ',
     &          'check other warnings for advise.','W')
              MATDBOK=.FALSE.
            endif
          endif
        elseif(ier.eq.0)then
          call eclose(matver,1.1,0.001,closemat1)
          call eclose(matver,1.2,0.001,closemat2)
          if(closemat1.or.closemat2)then
            call edisp(iuout,' Filled material data arrays...')
            MATDBOK=.TRUE.
          endif
        endif
      endif
        
C Read multilayer db information into common.
 139  CALL ERPFREE(IFMUL,ISTAT)
      call FINDFIL(LFMUL,XST)
      if(XST)then
        CALL ERMLDB(0,IUOUT,IER)
        IF(IER.NE.0)then
          write(outs,'(a,a,a)') ' Problem with constr. composites db ',
     &     LFMAT(1:lnblnk(LFMAT)),' !'
          call edisp(iuout,outs)
          MLDBOK=.FALSE.
        else
          call usrmsg(' Opened constr. composites db.',' ','p')
          MLDBOK=.TRUE.
        endif
      else
        write(outs,'(a,a,a)') ' No constr. composites db ',
     &     LFMAT(1:lnblnk(LFMAT)),' found!'
        call edisp(iuout,outs)
      endif

C Open optical database and read into common.
      SOPT='ALL'
      CALL EROPTDB(0,iuout,SOPT,GDESCR,IER)
      if(ier.ne.0)then
        call usrmsg(
     &  ' Optical properties db not found or there was a problem',
     &  ' reading it!','W')
        OPTKOK=.FALSE.
      else
        call usrmsg(' Opened optical properties db!',' ','p')
        OPTKOK=.TRUE.
      endif
      return
      end

C********************* Zoneslist (copy of MOZDFN from resdef.F) ****
C Display Zones list and wait for INIPIC item selected
C the zone no are recorded in NZNOG

      SUBROUTINE Zoneslist
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/pophelp/h(60)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/surfsel/isursel(MCON),NbSurSel(MCOM),ISelCon(MCON),
     &               ISelComp(MCON)
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      COMMON/ERRHAND/IDerr

      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      DIMENSION IVALZ(MCOM)
     

      CHARACTER H*72
      
      do 5 i=1,MCOM
        NbSurSel(i) = 0
  5   continue

      if (.not.cfgok) goto 666

C Clear nznog array and isursel array (remembers which surfaces have
C been selected).
      do 6 II=1,MCOM
        nznog(II)=0
  6   continue
      NZG=0
      do 8 II=1,MCON
        isursel(II)=0
  8   continue
      if(NCOMP.EQ.1)then
        call edisp(iuout,' Single zone model, this zone selected. ')
        call edisp(iuout,'  ')
        NZG=1
        nznog(1)=1
      else

C Otherwise select the zones from a list of zone names.
        call edisp(iuout,' Zone selection... ')
        call edisp(iuout,'  ')

 7      H(1)=' Pick one, several or All zones for inclusion '
        H(2)=' from the list provided. '
        H(3)=' You may choose them in any order. '
        INPIC=NCOMP
        CALL EPICKS(INPIC,IVALZ,' ',' Which zones to include: ',
     &    12,NCOMP,ZNAME,' Zones list',IDerr,3)

C Check for errors (if => exit sub).
        if (IDerr.ne.0) goto 666
C If no zones have been selected then return to main menu.
        if (INPIC.eq.0) then
          izgfoc = 0           
          call edisp(iuout,' Returning to main menu.... ')
          GOTO 666
        endif
        NZG=INPIC
        izgfoc = Ncomp
        do 40 I=1,INPIC
          NZNOG(I)=IVALZ(I)
 40     continue

C Two zones cannot have the same number.
        DO 32 I=1,NZG
          DO 30 J=1,NZG
            IF(I.EQ.J)goto 30
            J1=NZNOG(I)
            J2=NZNOG(J)
            IF(J1.EQ.J2)then
              call edisp(iuout,'Two zones cannot have the same number!')
              goto 7
            endif
 30       continue
 32     continue
      endif

 666  RETURN
      END


C ******************** ZNLIST (edited from esrures/utils.F) *******
C ZNLIST takes the current selected zones and builds a descriptive
C string to be used in headers. 
C zdescr : string contening the name of slected zone(s)
C length : Length of zdescr
      SUBROUTINE ZNLIST(zdescr,length,ierr)
#include "building.h"
#include "geometry.h"

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      CHARACTER zdescr*80,outs*244,outsd*80
      logical unixok

      length=0
      do 42 i=1,nzg
        lna=lnzname(nznog(i))
        length=length+lna+1
  42  continue
      if(length.lt.72)then
        WRITE(outs,5,iostat=ios,err=1)(zname(nznog(I)),I=1,nzg)
    5   FORMAT(20(a12,' '))
        call sdelim(outs,outsd,'S',IW)
        zdescr=outsd
        return
      else
        if(nzg.eq.1)then
         WRITE(ZDESCR,'(A,I2,A,A)')' (',nznog(1),') ',zname(nznog(1))
        elseif(nzg.gt.1.and.nzg.LE.20)THEN
         WRITE(ZDESCR,6)(nznog(I),I=1,nzg)
    6    FORMAT(20(I2,','))
        ELSEIF(nzg.GT.20.AND.nzg.LE.24)THEN
         WRITE(ZDESCR,7)(nznog(I),I=1,9),(nznog(J),J=10,nzg)
    7    FORMAT(9(I1,','),15(I2,','))
        ELSEIF(nzg.GT.24)THEN
         WRITE(ZDESCR,8)(nznog(I),I=1,9),(nznog(J),J=10,24)
    8    FORMAT(9(I1,','),15(I2,','))
        ENDIF
        return
      endif

C Trap for I/O error.
 1    call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      write(6,*) 'ZLIST: error writing zone names: ',outs
      ierr=1
      
      return
      END


C ************* ZoneDisp
C ZoneDisp - displays zone based information and allows....
C Icomp : Zone number
C Iskip : If = 0, then skip the list of surfaces menu
      SUBROUTINE ZoneDisp(icomp, iskip)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "epara.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)

      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/surfsel/isursel(MCON),NbSurSel(MCOM),ISelCon(MCON),
     &               ISelComp(MCON)
      COMMON/Displ/SelSurf(MCON)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      real sna,pazi,pelv
      COMMON/SDATA/SNA(MS),PAZI(MS),PELV(MS)

      DIMENSION VERT(35)

      CHARACTER H*72
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      CHARACTER VERT*44,KEY*1,head*31,OutStr*124

      LOGICAL MODIFY,MODLEN,MODBND,zbzero,context,SelSurf
      logical changedit
      logical newgeo  ! to use for testing if new/old geometry file.

      zbzero=.FALSE.
      changedit=.false.

C Read geometry file for chosen zone.
      ITMP=IFIL+1
      itru=6
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      if(newgeo)then
        call georead(ITMP,LGEOM(ICOMP),ICOMP,0,iuout,IER)
      else
        call egomin(ITMP,LGEOM(ICOMP),ICOMP,0,0,iuout,IER)
      endif
      MODIFY =.TRUE.
      MODBND =.TRUE.
      If (Icomp .eq. 1) then
        DO 33 I=1,MCON      
          SelSurf(I) =.FALSE.
  33    CONTINUE
      endif

      if (Iskip.eq.1) CALL ADJVIEW(IER)

C Multi-page list
      call edisp(iuout,'Please select surfaces to include in analysis.')

C Recover zone data (read only, no display).
      CALL ZINFO(SNA,ZOA,PAZI,PELV,ZVOL)
      vol(icomp)=zvol
      CALL ECLOSE(ZBASEA(ICOMP),0.0,0.001,zbzero)
      OPQ=0.
      TRN=0.
      DO 41 I=1,NSUR
        if(SOTF(I).EQ.'TRAN')then
          TRN=TRN+SNA(I)
        else
          OPQ=OPQ+SNA(I)
        endif
   41 CONTINUE

C Present menu with surfaces and their associated composite
C construction names.
  777 MHEAD=6
      MCTL=4
      ILEN=NSUR
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Set up header text.
      WRITE(VERT(1),'(a,f8.2,a)')'  Volume :          ',VOL(icomp),'m^3'
      WRITE(VERT(2),'(a,f8.2,a)')'  Base/floor area : ',ZBASEA(icomp),
     &  'm^2'
      WRITE(VERT(3),'(a,f8.2,a)')'  Opaque constr. (OPQ):  ',OPQ,'m^2'
      WRITE(VERT(4),'(a,f8.2,a)')'  Trans. constr. (TRN): ',TRN,'m^2'
      VERT(5)='  ------------------------------ '
      VERT(6)='  Surface name | Composite |  Net area [m^2}'

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(VERT(M),14)KEY,SNAME(ICOMP,L),SMLCN(L),SNA(L)
   14     FORMAT(A1,1X,A12,2X,A12,2X,F7.2)
        ENDIF
   10 CONTINUE
      M=M+1
      VERT(M)='+ all surfaces '

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  _____________________________   '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 Page ---- Part: ',I2,' of ',I2,' -----')
      ENDIF
      VERT(M+2)  ='* browse construction details     '
      VERT(M+3)  ='? help                            '
      VERT(M+4)  ='- exit this menu                  '

C Now display the menu.
      write(head,'(A,A)')' Composition of ',ZNAME(ICOMP)
      if (iskip.eq.1) then
        CALL EMENU(head,VERT,MVERT,IVERT)
      else
        IVERT=(MVERT-4)
      endif
      IF(IVERT.LE.MHEAD)THEN
        IVERT=-1
        GOTO 3

C IF EXIT
      ELSEIF(IVERT.EQ.MVERT)THEN
        if (SelSurf(Icomp) .eqv. .FALSE.) then
          CALL EDISP(iuout,'No selected surface(s) !')
        endif
        return
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C << Help message required. >>
        H(1)='The surface attributes of the zone geometry file,'
        CALL PHELPD('zone geom file section',1,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C IF * BROWSE 
        itru=6
        context=.false.
        CALL SURINFO(ICOMP,itru,context)
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

CC set for all surfaces in zone.
        SelSurf(Icomp) =.TRUE.
        do 42 ij=1,NSUR
          ioc=IZSTOCN(ICOMP,ij)
          isursel(ioc)=1
  42    continue
        NbSurSel(icomp) = NSUR
        call edisp(iuout,'All surfaces selected')
        itru=6
        context=.false.
        CALL SURINFO(ICOMP,itru,context)
        if (iskip.eq.0) return
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C IF SELECT A SURFACE 
C Decode from the potentially long list to the surface number (IS) via KEYIND.
C Produce a menu of data related to this surface.
        SelSurf(Icomp) =.TRUE.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        IS=IFOC
        ioc=IZSTOCN(ICOMP,is)
C Test to see if already selected. To avoid the calculation of the same surface
C Selected more thane once.
        IF (isursel(ioc).EQ.0) THEN
          NbSurSel(icomp) = NbSurSel(icomp) + 1 
          isursel(ioc)=1
        ENDIF

        CALL EDISP(ITRU,' ')
        CALL EDISP(ITRU,' The selected surface is : ')
        CALL EDISP(ITRU,' ')
        WRITE(OUTSTR,93)
   93   FORMAT('  surface    |    Areas    |Azim|Elev|geometry|',
     &  ' multilayer  |environment')
        CALL EDISP(ITRU,OUTSTR)
        WRITE(OUTSTR,94)
   94   FORMAT('    | name       | m^2   |deg |deg |type|loc|',
     &  ' constr name |other side ')
        CALL EDISP(ITRU,OUTSTR)
        CALL EDISP(ITRU,' ')
        WRITE(OUTSTR,8894)SNAME(ICOMP,IS),SNA(IS),PAZI(IS),
     &       PELV(IS),SOTF(IS),SVFC(IS),SMLCN(IS),SOTHER(IS,1)(1:13)
 8894   FORMAT(1X,A12,1X,F7.2,F5.0,F5.0,1X,A4,1X,A4,1X,A12,1X,A13)
        CALL EDISP(ITRU,OUTSTR)
      ELSE
        goto 92
      ENDIF
      goto 92
      END


C********************* CompoList **************************
C Display Composites list and wait for INIPIC item selected

C IComfoc      (Integer) Composite selected
C NComPik          (Integer) Number of selected composites (INPIC)
C LstComp(NMLC) (Integer) List of the selected composites number
C WidthCo (Real) : Width [m] of the composite, for LCA calculation.
C HeightCo (Real) : Height [m] of the composite, for LCA calculation.


      SUBROUTINE CompoList
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/pophelp/h(60)

      character DESC*48
      integer NMLC,IPR,LAYERS
      real DTHK,DRAIR
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      COMMON/ERRHAND/IDerr
      COMMON/CompPik/IComfoc,NComPik,LstComp(MMLC)

      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      DIMENSION IVALZ(MCOM)

      CHARACTER H*72

C Read database if not already done.
      if (.not.MLDBOK) then 
        call ERMLDB(0,iuout,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG(
     &    ' Error in reading composite construction.',
     &    ' Unable to create zone construction file!','W')
          RETURN
        ELSE
          MLDBOK=.TRUE.
        ENDIF
      ENDIF

      DO 6 II=1,NComPik
        LstComp(II)=0
 6    continue
      NComPik=0

C Otherwise select the zones from a list of zone names.
      call edisp(iuout,' Composite(s) selection... ')
      call edisp(iuout,'  ')

 7    H(1)='Pick one, several or All zones for inclusion in a'
      H(2)='xx from the list provided. '
      H(3)='You may choose them in any order. '

C <<Jon : allowing *Select all surface allow only one pik for the moment (Max.: NMLC)
      INPIC=NMLC
      CALL EPICKS(INPIC,IVALZ,' ',' Which composites to include:',
     &  48,NMLC,Desc,' Composite(s) list',IDerr,3)

C Check for errors (if => exit sub).
      if (IDerr.ne.0) RETURN

C If no composite have been selected then return to main menu.
      NComPik=INPIC
      if (NComPik .eq. 0) then
        IComfoc = 0           
        call edisp(iuout,' Returning to main menu.... ')
        RETURN
      ELSEIF (NComPik .eq. NMLC) THEN
C <<Jon : Icomfoc =? NComPik>>
        IComfoc = 0                       
        call edisp(iuout,' "*All items in list" selected.')
        RETURN
      ENDIF

      IComfoc = NComPik
      do 40 I=1,NComPik
        LstComp(I)=IVALZ(I)
 40   continue

C Two composites cannot have the same number.
      DO 32 I=1,NComPik
        DO 30 J=1,NComPik
          IF(I.EQ.J)goto 30
          J1=LstComp(I)
          J2=LstComp(J)
          IF(J1.EQ.J2)then
            call edisp(iuout,
     &            ' Two composites cannot have the same number!')
            goto 7
          endif
 30     continue
 32   continue
 666  RETURN
      END



C****************STEPIMP************************************
C
C This function calculates the 12 elementary stages for the LCA 
C calculation.
C IZ (Integer): Zone analysed,required if relative results is required (/m2) 
C I (Integer) : Composite number ! if =0 then  LCA is material based
C J (Integer) : Material number (in BCF material block)
C K (Integer) : Maintenance or transport number
C L (Integer) : Impacts number
C IStep (Integer) : Elementary step number of LCA calculation
C
C See LCAcalc subroutine, for the definition od each Elementary step.
C 
C IF calculation is material based (I.EQ.0) the calculation is per 
C mass of the selected material (WeigtMat).
C IF calculation is composite or project based (I.NE.0) the calculation is per 
C square meter of the selected composite.

      FUNCTION STEPIMP(IZ,I,J,K,L,NoStep)
#include "building.h"
#include "geometry.h"
#include "LCA.h"

      COMMON/CompDim/HeightCo,WidthCo, Perimeter, Area
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/EIAScnPt/FabPt(MMAT),TrPt(MMAT,MNbTrans),
     &                RecyPt(MMAT),ReTrPt(MMAT,MNbTrans),
     &                BurnPt(MMAT),BuTrPt(MMAT,MNbTrans),
     &                DumpPt(MMAT),DuTrPt(MMAT,MNbTrans),
     &                ComAsPt(MMLC,MNbAss),CoTrPt(MMLC,MNbTrans),
     &                MaintTyp(MMLC,MNbM),MaintPt(MMLC,MNbM),
     &                ComAsPa(MMLC,MNbAss),AsProjPt(MMLC,MNbAss),
     &                AsProjPa(MMLC,MNbAss),LayerCat(MMLC,ME),
     &                MainPart(MMLC,MNbM),LayerNam(MMLC,ME),
     &                LayerTyp(MMLC,ME)


      COMMON/EIAImp/FabImp(MMAT,MIMP),TrImp(MMAT,MNbTrans,MIMP),
     &          RecyImp(MMAT,MIMP),ReTrImp(MMAT,MNbTrans,MIMP),
     &          BurnImp(MMAT,MIMP),BuTrImp(MMAT,MNbTrans,MIMP),
     &          DumpImp(MMAT,MIMP),DuTrImp(MMAT,MNbTrans,MIMP),
     &          ComAsImp(MMLC,MNbAss,MIMP),CoTrImp(MMLC,MNbTrans,MIMP),
     &          AsProImp(MMLC,MNbAss,MIMP), RMainImp(MMLC,MNbM,MIMP)

      COMMON/EIAMisc/NbEIALay(MMLC),ProjLife, RMatLife(MMAT),
     &               PerMaint(MMLC,MNbM),RecyRate(MMAT),BurnRate(MMAT),
     &               DumpRate(MMAT),AssLoss(MMAT),TrBrk(MMAT,MNbTrans),
     &               AsProBrk(MMAT,MNbTrans),CoTrBrk(MMLC,MNbAss),
     &               QuaLayer(MMLC,ME),FixLayer(MMLC,ME),DenLay(MMAT),
     &               SpecData(MMAT,2), NRi(MMLC,ME)

      COMMON/SPECMASS/SMPLayer(MMLC,ME),SMSLayer(MMLC,ME),IWhich,
     &                SupFab(MMLC,ME),CompSMP(MMLC),CompSMS(MMLC),
     &                SupTr(MMLC,ME),SupTrMat(MMLC,ME),WeigtMat

      COMMON/ECOCalcB/RecPWast(MMLC,ME), RecSWast(MMLC,ME),
     &               RecPTr(MMLC,ME,MNbTrans,MIMP),
     &               RecSTr(MMLC,ME,MNbTrans,MIMP),
     &               RecPPr(MMLC,ME,MIMP),RecSPr(MMLC,ME,MIMP), 
     &               DumPWast(MMLC,ME),DumSWast(MMLC,ME),
     &               DumPTr(MMLC,ME,MNbTrans,MIMP),
     &               DumSTr(MMLC,ME,MNbTrans,MIMP),
     &               DumPPr(MMLC,ME,MIMP),DumSPr(MMLC,ME,MIMP), 
     &               BurPWast(MMLC,ME),BurSWast(MMLC,ME),
     &               BurPTr(MMLC,ME,MNbTrans,MIMP),
     &               BurSTr(MMLC,ME,MNbTrans,MIMP),
     &               BurPPr(MMLC,ME,MIMP),BurSPr(MMLC,ME,MIMP)


      COMMON/ECOCalcA/SMassImp(MMLC,ME,MIMP),PMassImp(MMLC,ME,MIMP),
     &               TrSImp(MMLC,ME,MNbTrans,MIMP),
     &               TrPImp(MMLC,ME,MNbTrans,MIMP),
     &               ComAsIm(MMLC,MNbAss,MIMP), 
     &               PerAsIm(MMLC,MNbAss,MIMP),
     &               TrComImp(MMLC,ME,MNbTrans,MIMP),
     &               TrPerImp(MMLC,ME,MNbTrans,MIMP),
     &               ComBuImp(MMLC,MNbAss,MIMP),
     &               PerBuImp(MMLC,MNbAss,MIMP),
     &               ComCoImp(MMLC,MIMP),PerCoImp(MMLC,MIMP),
     &               ComSMain(MMLC,MNbAss,MIMP),
     &               ComPMain(MMLC,MNbAss,MIMP),
     &               SurReMas(MMLC,ME),PerReMas(MMLC,ME),
     &               SurReImp(MMLC,ME,MIMP),PerReImp(MMLC,ME,MIMP),
     &               SurLfImp(MMLC,MIMP),PerLfImp(MMLC,MIMP)

      common/EIAhigh/NbComp,NbMat,IMatID(MMLC,ME),ImatDbID(0:MMAT-1),
     &               LCIATag

      COMMON/IMPABREV/AbrevIMP(MIMP)
      COMMON/LCAUnit/UnitEner,UnitMass,UnitImp

      REAL STEPIMP
      INTEGER I,J,K,L,NoStep
      REAL HeightCo,WidthCo, Perimeter, Area

      CHARACTER DESC*48

      CHARACTER*12 FabPt,TrPt,RecyPt,ReTrPt,BurnPt,BuTrPt,DumpPt,DuTrPt
      CHARACTER*12 ComAsPt,CoTrPt,MaintPt,AsProjPt
      CHARACTER*1 ComAsPa,MaintTyp,LayerCat,MainPart,AsProjPa,LayerTyp

      Character*12 LayerNam

      CHARACTER*3 AbrevIMP
      CHARACTER UnitEner*5, UnitMass*4, UnitImp*8
      
      IM = IMatID(I,J)
            

      STEPIMP = 0.
      IF (NoStep .EQ. 1) THEN
C 1 Materials fabrication
        IF (I.EQ.0) THEN
          STEPIMP = FabImp(J,L) * WeigtMat
        ELSE
          if (LayerCat(I,J) .eq. 'P') then
            STEPIMP = PMassImp(I,J,L) * Perimeter
          else
            STEPIMP = SMassImp(I,J,L) * Area
          endif
        ENDIF
      ELSEIF (NoStep .EQ. 2) THEN
C 2 Transp. to construction site
        if (LayerCat(I,J) .eq. 'P') then
          STEPIMP = TrPImp(I,J,K,L) * Perimeter
        else
          STEPIMP =  TrSImp(I,J,K,L) * Area 
        endif
      ELSEIF (NoStep .EQ. 3) THEN
C 3 Composite assembly
        if (LayerCat(I,J) .eq. 'P') then
          STEPIMP = PerAsIm(I,K,L) * Perimeter
        else
          STEPIMP = ComAsIm(I,K,L) * Area
        endif
      ELSEIF (NoStep .EQ. 4) THEN
C 4 Transport to building site
        if (LayerCat(I,J) .eq. 'P') then
          STEPIMP = TrPerImp(I,J,K,L)*Perimeter
        else
          STEPIMP = TrComImp(I,J,K,L) * Area
        endif
      ELSEIF (NoStep .EQ. 5) THEN
C 5 Assembly on the building
        if (LayerCat(I,J) .eq. 'P') then
          STEPIMP = PerBuImp(I,K,L) * Perimeter
        else
          STEPIMP = ComBuImp(I,K,L) * Area
        endif
      ELSEIF (NoStep .EQ. 6) THEN
C 6 Composite maintenance
        if (LayerCat(I,J) .eq. 'P') then
          STEPIMP =RMainImp(I,K,L) *Perimeter *PerMaint(I,K)*ProjLife
        else
          STEPIMP =RMainImp(I,K,L) *Area *PerMaint(I,K)*ProjLife
        endif
      ELSEIF (NoStep .EQ. 7) THEN
C 7 Composite replacement (Perimter)
        if (LayerCat(I,J) .eq. 'P') then
          STEPIMP = PerReImp(I,J,L) * Perimeter
        else
          STEPIMP = SurReImp(I,J,L) * Area 
        endif
      ELSEIF (NoStep .EQ. 8) THEN
C 8 Dumped waste transport
        if (LayerCat(I,J) .eq. 'P') then
          STEPIMP = DumPTr(I,J,K,L) * Perimeter
        else
          STEPIMP = DumSTr(I,J,K,L) * Area
        endif
      ELSEIF (NoStep .EQ. 9) THEN
C 9 Dump process (perimeter + area)
        STEPIMP = DumPPr(I,J,L) * Perimeter
        STEPIMP = STEPIMP + DumSPr(I,J,L) * Area
      ELSEIF (NoStep .EQ. 10) THEN
C 10 Incinerated waste transport
        if (LayerCat(I,J) .eq. 'P') then
          STEPIMP = BurPTr(I,J,K,L) * Perimeter
        else
          STEPIMP = BurSTr(I,J,K,L) * Area
        endif
      ELSEIF (NoStep .EQ. 11) THEN
C 11 Incineration process  (Perimter + Area)
        STEPIMP = BurPPr(I,J,L) * Perimeter
        STEPIMP = STEPIMP + BurSPr(I,J,L) * Area
      ELSEIF (NoStep .EQ. 12) THEN
C 12 Recycled waste transport
        if (LayerCat(I,J) .eq. 'P') then
          STEPIMP = RecPTr(I,J,K,L) * Perimeter 
        else
          STEPIMP = RecSTr(I,J,K,L) * Area
        endif
      ELSEIF (NoStep .EQ. 13) THEN
C 13 Recycling process  (Perimter + Area)
        STEPIMP = RecPPr(I,J,L) * Perimeter
        STEPIMP = STEPIMP + RecSPr(I,J,L) * Area
      ENDIF

C Transform to the right unit
        if (AbrevIMP(L).EQ.'NRE') then
          IF (UnitEner(1:5).EQ.'[kWh]') THEN
            STEPIMP = STEPIMP / 3.6
          ENDIF
        else
          IF (UnitMass(1:3).EQ.'[g]') THEN
            STEPIMP = STEPIMP * 1000.
          ENDIF
        endif
        if (UnitImp(1:8).EQ.'[Imp/m2]') then
          STEPIMP = STEPIMP/ ZBASEA(IZ)
        endif

      RETURN
      END



C********************* LCAcalc **************************
C
C This subroutine displays LCA results.
C Step 1 : Calculation >> User can select  "Elementary steps", "Major phase"
C                         or "Whole cycle"
C Step 2 : LCA calculation . See HELP below for more information.
C Step 3 : Results display >> User can select between "Whole project", 
C                             "Zone level", "Surface level" or 'Layer level"
C For each stage
C   For each Zone
C     For each Surface/mlc
C       For each Layer
C         For each life-cycle stage
C           Display result according to the results display slelected
C         Next life-cycle stage
C       Next Layer
C     Next Surface/mlc
C   Next Zone
C Next Stage
C
C ICalType Integer : 
C   1 : For selected zone/building
C   2 : For selected composite
C   3 : For selected material
C
C IWB: step type : 1= elementary step ; 2=major step; 3=whole life cycle
C IW : Major step type : 1=CONSTRUCTION ; 2=UTILISATION; 3=DECONSTRUCTION 
C      (only relevant for IWB=2)
C Itrc (Integer) : define the results reporting level:
C     0: Whole Project
C     1: Zone level
C     2: Surface level
C     3: Layer level
C ISelCon(MCON)      : List of the selected surface Return the absolute surface 
C                      value within the project. 1 if corresponding connection (MCON) 
C                      has been selected
C ISelComp(MCON)     : List of selected composite(s) in a project 
C                      (return the mlc inedex (database) of surface as listed in the BCF file)
C LstComp(MMLC)      : List of selected composite(s) without any project
C IFOC (Tmp Integer): Absolute index of the selected connectionC
C WidthCo REAL : Height [m] of the composite, for LCA calculation.
C TotDumpR REAL : Total dump rate [-]
C TotDumpM REAL : Total dump mass [kg] = Layer mass * layer dump rate
C NbSteps INT : Nb of LCA calculation elementary steps 
C StepName(NbSteps) CHAR : Name of LCA elementary steps
C
C Perimeter REAL : Perimeter [m] of the composite define when user is 
C                  interested in LCA of a composite (existing in the 
C                  project) for a special dimension.
C
C Area REAL : Area [m]of the composite define when user is interested in LCA
C             of a composite (existing in the project) for a special 
C             dimension .
C
C DTHKTot REAL : Thickness [m] of a composite = Sum of layers thickness.
C
C LCAValue(MIMP) REAL : LCA impacts [impacts] of a layer (dummy variable)
C
C
C TotRecyM REAL      : Total mass of recycled composite [kg].
C CompoMass REAL     : Mass of one surface in a zone [kg].
C TotMass REAL       : Total mass of surfaces in a zone [kg] (=Somme(CompoMass))
C isursel(MCON)      : 1 if connexion (index) selected or 0 if not
C NbSurSel INT       : Nb of selected connexion(s)
C
C IChanel : Sending result to that chanel: 
C          1 = textual feedback or 2 = File
C
C IMatDbID(0:MMAT-1) Integer : the value at the ith position corrspond to 
C to material number in the BCF file of the ith materal in the 
C Db of the primitive material
C 

      SUBROUTINE LCAcalc(itrc,Ichanel)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "LCA.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/pophelp/h(60)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      character DESC*48
      integer NMLC,IPR,LAYERS
      real DTHK,DRAIR
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/surfsel/isursel(MCON),NbSurSel(MCOM),ISelCon(MCON),
     &               ISelComp(MCON)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)

C << Why is SDATA used? >>
      real sna,pazi,pelv
      COMMON/SDATA/SNA(MS),PAZI(MS),PELV(MS)
      COMMON/CompPik/IComfoc,NComPik,LstComp(MMLC)

      COMMON/ProjTxt/TxtCompo,TxtMat
      COMMON/EIAScnPt/FabPt(MMAT),TrPt(MMAT,MNbTrans),
     &                RecyPt(MMAT),ReTrPt(MMAT,MNbTrans),
     &                BurnPt(MMAT),BuTrPt(MMAT,MNbTrans),
     &                DumpPt(MMAT),DuTrPt(MMAT,MNbTrans),
     &                ComAsPt(MMLC,MNbAss),CoTrPt(MMLC,MNbTrans),
     &                MaintTyp(MMLC,MNbM),MaintPt(MMLC,MNbM),
     &                ComAsPa(MMLC,MNbAss),AsProjPt(MMLC,MNbAss),
     &                AsProjPa(MMLC,MNbAss),LayerCat(MMLC,ME),
     &                MainPart(MMLC,MNbM),LayerNam(MMLC,ME),
     &                LayerTyp(MMLC,ME)

      COMMON/EIAImp/FabImp(MMAT,MIMP),TrImp(MMAT,MNbTrans,MIMP),
     &          RecyImp(MMAT,MIMP),ReTrImp(MMAT,MNbTrans,MIMP),
     &          BurnImp(MMAT,MIMP),BuTrImp(MMAT,MNbTrans,MIMP),
     &          DumpImp(MMAT,MIMP),DuTrImp(MMAT,MNbTrans,MIMP),
     &          ComAsImp(MMLC,MNbAss,MIMP),CoTrImp(MMLC,MNbTrans,MIMP),
     &          AsProImp(MMLC,MNbAss,MIMP), RMainImp(MMLC,MNbM,MIMP)

      COMMON/EIADist/TrDist(MMAT,MNbTrans), CoTrDist(MMLC,MNbTrans),
     &                 ReTrDist(MMAT,MNbTrans), BuTrDist(MMAT,MNbTrans),
     &                 DuTrDist(MMAT,MNbTrans)



      COMMON/EIAMisc/NbEIALay(MMLC),ProjLife, RMatLife(MMAT),
     &               PerMaint(MMLC,MNbM),RecyRate(MMAT),BurnRate(MMAT),
     &               DumpRate(MMAT),AssLoss(MMAT),TrBrk(MMAT,MNbTrans),
     &               AsProBrk(MMAT,MNbTrans),CoTrBrk(MMLC,MNbAss),
     &               QuaLayer(MMLC,ME),FixLayer(MMLC,ME),DenLay(MMAT),
     &               SpecData(MMAT,2), NRi(MMLC,ME)

      COMMON/SPECMASS/SMPLayer(MMLC,ME),SMSLayer(MMLC,ME),IWhich,
     &                SupFab(MMLC,ME),CompSMP(MMLC),CompSMS(MMLC),
     &                SupTr(MMLC,ME),SupTrMat(MMLC,ME),WeigtMat

      COMMON/ECOCalcB/RecPWast(MMLC,ME), RecSWast(MMLC,ME),
     &               RecPTr(MMLC,ME,MNbTrans,MIMP),
     &               RecSTr(MMLC,ME,MNbTrans,MIMP),
     &               RecPPr(MMLC,ME,MIMP),RecSPr(MMLC,ME,MIMP), 
     &               DumPWast(MMLC,ME),DumSWast(MMLC,ME),
     &               DumPTr(MMLC,ME,MNbTrans,MIMP),
     &               DumSTr(MMLC,ME,MNbTrans,MIMP),
     &               DumPPr(MMLC,ME,MIMP),DumSPr(MMLC,ME,MIMP), 
     &               BurPWast(MMLC,ME),BurSWast(MMLC,ME),
     &               BurPTr(MMLC,ME,MNbTrans,MIMP),
     &               BurSTr(MMLC,ME,MNbTrans,MIMP),
     &               BurPPr(MMLC,ME,MIMP),BurSPr(MMLC,ME,MIMP)


      COMMON/ECOCalcA/SMassImp(MMLC,ME,MIMP),PMassImp(MMLC,ME,MIMP),
     &               TrSImp(MMLC,ME,MNbTrans,MIMP),
     &               TrPImp(MMLC,ME,MNbTrans,MIMP),
     &               ComAsIm(MMLC,MNbAss,MIMP), 
     &               PerAsIm(MMLC,MNbAss,MIMP),
     &               TrComImp(MMLC,ME,MNbTrans,MIMP),
     &               TrPerImp(MMLC,ME,MNbTrans,MIMP),
     &               ComBuImp(MMLC,MNbAss,MIMP),
     &               PerBuImp(MMLC,MNbAss,MIMP),
     &               ComCoImp(MMLC,MIMP),PerCoImp(MMLC,MIMP),
     &               ComSMain(MMLC,MNbAss,MIMP),
     &               ComPMain(MMLC,MNbAss,MIMP),
     &               SurReMas(MMLC,ME),PerReMas(MMLC,ME),
     &               SurReImp(MMLC,ME,MIMP),PerReImp(MMLC,ME,MIMP),
     &               SurLfImp(MMLC,MIMP),PerLfImp(MMLC,MIMP)

      COMMON/LCAUnit/UnitEner,UnitMass,UnitImp
      COMMON/LCAFlag/ILoss,ICalType,Irep
      COMMON/StepsName/StepName(NbSteps),LabelGen(3),GenUnt(3),
     &      Label(NbSteps+1),LabelUnt(NbSteps+1),LabelImp,ImpUnt
      COMMON/CompDim/HeightCo,WidthCo, Perimeter, Area
      COMMON/EIAhigh/NbComp,NbMat,IMatID(MMLC,ME),ImatDbID(0:MMAT-1),
     &               LCIATag

      COMMON/DesMLC/MatNameco(MMAT),MatDesc(MMAT),MatCat(MMAT),
     &              CompoNam(MMLC), LayerDes(MMLC,ME)

      COMMON/ZFunction/ZFun(NbSteps,MMLC,ME)

      DIMENSION ITEM(NbSteps+5),StepSName(NbSteps)

      CHARACTER TxtCompo*22,TxtMat*22
      CHARACTER*12 FabPt,TrPt,RecyPt,ReTrPt,BurnPt,BuTrPt,DumpPt,DuTrPt
      CHARACTER*12 ComAsPt,CoTrPt,MaintPt,AsProjPt
      CHARACTER*1 ComAsPa,MaintTyp,LayerCat,MainPart,AsProjPa,LayerTyp
      CHARACTER*12 CompoNam,LayerNam,MatNameco
      CHARACTER*72 MatDesc,MatCat,LayerDes
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      CHARACTER UnitEner*5, UnitMass*4, UnitImp*8
      CHARACTER H*72,outs*170,ITEM*36,OposLayName*12
      CHARACTER StepName*36,StepSName*12
      CHARACTER*110 LabelGen, GenUnt, Label, LabelUnt, LabelImp, ImpUnt
      CHARACTER*55 LabelB, LabelA, LabelC, LabelD
      CHARACTER*12 Labelout(4+MIMP),LabelTmp(4+MIMP)
      LOGICAL Exist, dupedges,context

      REAL HeightCo,WidthCo, Perimeter, Area, WeigtMat
      REAL TotDumpM, TotBurnM, TotRecyM
      REAL CompoMass, TotMass, DTHKTot
      REAL LCAValue(MIMP),LCAValuP(MIMP),LCAValuS(MIMP),StepImp
      REAL LayerTot(MIMP),CompoTot(MIMP),ZoneTot(MIMP),StageTot(MIMP),
     &     WholeTot(MIMP),ProjTot(MIMP)
      logical newgeo  ! to use for testing if new/old geometry file.
 

C Initialisation
      newgeo=.false.  ! assume older format geometry.
      do 142 I=1,MIMP
        LCAValue(I)=0.0
        LCAValuP(I)=0.0
        LCAValuS(I)=0.0
        LayerTot(I)=0.0
        CompoTot(I)=0.0
        ZoneTot(I)=0.0
        StageTot(I)=0.0
        WholeTot(I)=0.0
        ProjTot(I)=0.0
 142  continue

      do 14 I=1,(4+MIMP)
        Labelout(I)=' '
        LabelTmp(I)=' '
 14   continue

      NbSurf=0
      DO 610 IS = 1,MCON
        ISelCon(IS) = 0
        ISelComp(IS) = 0
610   CONTINUE
      TotDumpS= 0.0
      TotBurnS= 0.0
      TotRecyS= 0.0
      DTHKTot = 0.0
      TotDumpM= 0.0
      TotBurnM= 0.0
      TotRecyM= 0.0
      TotDumpZ= 0.0
      TotBurnZ= 0.0
      TotRecyZ= 0.0
      CompoMass = 0.0
      CompoMasI = 0.0
      TotMass = 0.0
      dupedges=.false.
      IV = 0

C Life cycle long name
      StepName(1) = 'Material fabrication'
      StepName(2) = 'Transp. to assembly site '
      StepName(3) = 'Construction assembly '
      StepName(4) = 'Transport to building site '
      StepName(5) = 'Assembly on the building '
      StepName(6) = 'Construction maintenance '
      StepName(7) = 'Material replacement '
      StepName(8) = 'Dumped waste transport '
      StepName(9) = 'Deposite in landfill process '
      StepName(10)= 'Incinerated waste transport '
      StepName(11)= 'Incineration process '
      StepName(12)= 'Recycled waste transport '
      StepName(13)= 'Recycling process '
      StepName(14)= 'Disposal mass '

C Life cycle short name
      StepSName(1) = 'Fabrication '
      StepSName(2) = 'Fab->Assmbly'
      StepSName(3) = 'MLC assembly'
      StepSName(4) = 'Assmbly->Bui'
      StepSName(5) = 'Assmbly buil'
      StepSName(6) = 'Maintenance '
      StepSName(7) = 'Replacement '
      StepSName(8) = '->landfill  '
      StepSName(9) = 'Landfill    '
      StepSName(10)= '->Incinerat.'
      StepSName(11)= 'Incineration'
      StepSName(12)= '->Recycling '
      StepSName(13)= 'Recycling   '
      StepSName(14)= 'Disposal mas'


C Whih LCA display ; elementary step or major step ?
      H(1)='Selection of the LCA results'
      H(2)='----------------------------'
      H(3)=' '
      H(4)='LCA results can be display as :'
      H(5)=' '
      H(6)='1) Single step: In this case, the results correspond only '
      H(7)='   to one LCA elementary step, such as material mass '
      H(8)='   impacts or composite assembly. If this option is '
      H(9)='   selected, you will have to select a step in a list.'
      H(10)=' '
      H(11)='2) Major phase: In this case, the results correspond to a'
      H(12)='   group of elementary step define in 1). If this option '
      H(13)='   is selected, you will have to select which major step.'
      H(14)=' '
      H(15)='3) Whole cycle: In this case, the results correspond to '
      H(16)='   the impacts from cradle to grave.'


3     IWB=0
      IWBB=0
      IW = 0

C Ask which step. Message slightly different in interactive mode and  when exporting 
C the results to a file
      if (Ichanel.ne.iuout)then
        CALL EASKABCD(' ', 'Do you want to add in output file :',
     &                  ' An elementary phase ',' A major phase ',
     &                  ' Whole life cycle ',' Cancel ',IWB,16)
        IF(IWB.EQ.4) GOTO 666 
      else
        CALL EASKABCD(' ', 'Do you want to analyse:',
     &                  ' An elementary phase ',' A major phase ',
     &                  ' Whole life cycle ',' Cancel ',IWB,16)
      endif
      CALL edisp(Ichanel,' ')

      
      IF (IWB .EQ.1) THEN
C If analysis of a single step then display the list of availabale steps
10      INO = -4
        ITEM(1)=                '  SELECT AN ELEMENTARY LCA STAGE  '
        ITEM(2)=                '----------------------------------'
        DO 5 I = 1,NbSteps
          WRITE(ITEM(2+I),'(A)') StepName(I)(1:lnblnk(StepName(I)))
5       CONTINUE
        ITEM(NbSteps+3)=         '----------------------------------'
        ITEM(NbSteps+4)=         ' ?  Help                          '
        ITEM(NbSteps+5)=         ' -  Cancel                        '
        NITEMS = NbSteps+5
        CALL EMENU(' LCA elementary stages',ITEM,NITEMS,INO)

        IF (INO.EQ.NITEMS) THEN
C If no single step has been selected go back
           GOTO 666

C if Help is required
        ELSEIF(INO.EQ.NITEMS-1)THEN
          H(1)='ELEMENTARY LCA PHASE'
          H(2)='--------------------'
          H(3)=' '
          H(4)='The LCA results display will correspond to your '
          H(5)='selection. The LCA elementary phase correspond to :'
          H(6)='  '
          H(7)=' 1 Materials fabrication : Impacts due to materials'
          H(8)='   fabrication. Include processes from the material '
          H(9)='   extraction to the material producrion factory.'
          H(10)=' 2 Transp. to construction site : Impacts due to '
          H(11)='   materials transportation from material production'
          H(12)='   factory site to the composite assembly site. This'
          H(13)='   latest site is either the building site if the '
          H(14)='   composite is build on the building site (such as'
          H(15)='   a brick wall) or an other factory in'
          H(16)='   the case of a prefabricated composite (such as'
          H(17)='   window.'
          H(18)=' 3 Composite assembly : Impacts due to the processes'
          H(19)='   required to assemble the composite on the '
          H(20)='   composite site.'
          H(21)=' 4 Transport to building site : Impacts due to the'
          H(22)='   composite transport. Only <> 0 for prefabricated'
          H(23)='   composite.'
          H(24)=' 5 Assembly on the building : Impacts due to the '
          H(25)='   assembly of the composite on the building.'
          H(26)=' 6 Building maintenance : Impacts due to the building'
          H(27)='   maintenance over its lifetime.' 
          H(28)=' 7 Dumped waste transport : Impacts due to the part '
          H(29)='   of waste transported from the building site to the'
          H(30)='   dump site.'
          H(31)=' 8 Dumped waste treatment : Impacts due to the dumped'
          H(32)='   waste treatment at the dump site.'
          H(33)=' 9 Dumped waste mass : Mass of the dumped waste.'
          H(34)=' 10 Incinarated waste transport : Impacts due to the '
          H(35)='    part of waste transported from the building site '
          H(36)='    to the incinaration site.'
          H(37)=' 11 Incinarated waste treatment : Impacts due to the '
          H(38)='    incinaration treatment at the incinaration site.'
          H(39)=' 12 Incinarated waste mass : Mass of the incinarated '
          H(40)='    waste.'
          H(41)=' 13 Recycled waste : Mass of recycled waste '
          CALL PHELPD('ELEMENTARY LCA PHASE menu',41,'-',0,0,IER)
          GOTO 10
        ENDIF

C If a Major stage was selected
      ELSEIF (IWB .EQ.2) THEN
C ask which major stage to analyse
        call EASKABCD('LCA impacts:',' Which major stage ? ',
     &     ' Building construction ',' Building utilisation ',
     &     ' Building elimination ', ' CANCEL ',IW,20)
        if (IW.EQ.4) goto 666

C Cancel LCA calculation if analyse type does not exist
      ELSEIF (IWB .EQ. 4) THEN
        goto 666
C End of stage selection
      ENDIF

C According to user selection, define the starting Elementary Step (IStep)
C and the Last Elementary Step (LStep) for the looging trough the steps
      IF (IWB .EQ. 1) THEN
C If single step, then Starting step = Ending step
        IStep = INO-2
        LStep = IStep
      ELSEIF (IWB .EQ. 2) THEN
C If magor step, then Starting step and  Ending step
C depends on the which major step (IW) was selected
        IF (IW .EQ. 1) THEN
          IStep = 1
          LStep = 5
        ELSEIF (IW .EQ. 2) THEN
          IStep = 6
          LStep = 7
        ELSEIF (IW .EQ. 3) THEN
          IStep = 8
          LStep = 13
        ELSE
          IStep = 0
          LStep = 0
        ENDIF
      ELSEIF (IWB .EQ. 3) THEN
        IStep = 1
        LStep = 13
      ENDIF

        
C Display a message telling if the results include or not the break and loss
C      IF (M .EQ. 2) Then         
        IF (ILoss .EQ. 0) Then 
          CALL edisp(Ichanel,' Results include break & loss')
        else 
          CALL edisp(Ichanel,' Results does not include break & loss')
        ENDIF         
C      ENDIF         
         
C Display the main title
      IF (IWB.EQ.1) THEN
C        CALL edisp(Ichanel,'Phase:')
      ELSEIF (IWB.EQ.2) THEN
        if (ICalType.eq.1) then
          IF (IW.EQ.1) THEN        
            CALL edisp(Ichanel,
     &'Phase: Build. CONSTRUCTION = Fabrication + transport + assembly')
          ELSEIF (IW.EQ.2) THEN
            CALL edisp(Ichanel,
     &' Phase: Building UTILISATION = Maintenance + replacement')
          ELSEIF (IW.EQ.3) THEN
            CALL edisp(Ichanel,
     &' Phase; Building ELIMINATION = Deconstruction + waste treatment')
          ENDIF
        elseif (ICalType.eq.2) then
          IF (IW.EQ.1) THEN        
            CALL edisp(Ichanel,
     &' Phase: MLC CONSTRUCTION = Fabrication + transport + assembly')
          ELSEIF (IW.EQ.2) THEN
            CALL edisp(Ichanel,
     &' Phase: MLC UTILISATION = Maintenance + replacement')
          ELSEIF (IW.EQ.3) THEN
            CALL edisp(Ichanel,
     &' Phase; MLC ELIMINATION = Deconstruction + waste treatment')
          ENDIF
        elseif (ICalType.eq.3) then
          IF (IW.EQ.1) THEN        
            CALL edisp(Ichanel,
     &'Phase: Mater. CONSTRUCTION = Fabrication + transport + assembly')
          ELSEIF (IW.EQ.2) THEN
            CALL edisp(Ichanel,
     &' Phase: Material UTILISATION = Maintenance + replacement')
          ELSEIF (IW.EQ.3) THEN
            CALL edisp(Ichanel,
     &' Phase; Material ELIMINATION = Deconstruction + waste treatment')
          ENDIF
        else
          CALL edisp(Ichanel,'This lcafoc does not exists')
        endif
        CALL edisp(Ichanel,' ------------------')
        CALL edisp(Ichanel,' ')
      ELSEIF (IWB.EQ.3) THEN
        if (ICalType.eq.1) then
          CALL edisp(Ichanel,' Phase: Building life-cycle')
        elseif (ICalType.eq.2) then
          CALL edisp(Ichanel,' Phase: MLC life-cycle')
        elseif (ICalType.eq.3) then
          CALL edisp(Ichanel,' Phase: Material life-cycle')
        else
        endif
        CALL edisp(Ichanel,' ------------------')
        CALL edisp(Ichanel,' ')
      ENDIF
      WRITE(outs,'(a,a)')

C Display the column labels for stage and zone level
      IF (ICalType.EQ.1) THEN
        LabelA ='|    Zone    |   Surface  |    Layer   |   Cycle    |'
      ELSEIF (ICalType.EQ.2) THEN
        LabelA ='|            |  Composite |    Layer   |   Cycle    |'
      ENDIF
      LabelB='    GWP    |    AP     |   POCP    |   NRE    |'
      WRITE(outs,'(a,a)')LabelA(1:lnblnk(LabelA)), 
     &                     LabelB(1:lnblnk(LabelB))
      CALL edisp(Ichanel,outs)
      LabelC ='|            |            |            |            |'

      if (UnitImp(1:8).eq.'[Imp/m2]') then
        LabelD='  [kg/m2]  |  [kg/m2]  |  [kg/m2]  |  [MJ/m2] |'
      else
        LabelD='    [kg]   |    [kg]   |   [kg]    |   [MJ]   |'
      endif
      WRITE(outs,'(a,a)')LabelC(1:lnblnk(LabelC)),
     &                       LabelD(1:lnblnk(LabelD))
      CALL edisp(Ichanel,outs)

C Adapt the loop accoring to user approach
C If project approch: selected surface(s)
C If composite approch: selected composite(s)
C If material approch: 1 surface (mat) 
      IF (ICalType.EQ.1) THEN
        NbZones = nzg
      ELSEIF (ICalType.EQ.2) THEN
        NbZones = 1
      ELSEIF (ICalType.EQ.3) THEN
        NbZones = 1
      ENDIF

C Adapt the loop trough surfaces accoring to user approach
C If project approch: selected surface(s)
C If composite approch: selected composite(s)
C If material approch: 1 surface (mat) 
      NbSurf=0
      DO 61 IS = 1,MCON
        ISelCon(IS) = 0
61    CONTINUE

C Group and count the selected surfaces, then find the correponding mlc index
C in bcf file corresponding to current surface 
C when all selected surface scannes exit the loop
      IF (ICalType.EQ.1) THEN               
        ITmp = 0
        DO 611 IN = 1,MCON
          IF (isursel(IN).EQ.1) THEN
C Add an element in the array only if connection has been selected
            ITmp = ITmp + 1
C XXX            ISelCon(ITmp) = IN

C Look for the mlc index (IFC)
C Scan the list of MLC name in database (CompoNam) and check
C if it correspond to the selected connection (SSMLCN)
C if so affect the MLC nomber to ISelComp and pass to the next 
C Selected connection
C            lstr=lnblnk(SMLCN(IN))
            ICF=0
            DO 20 IC=1,NbComp
C              if(lstr.le.MS)then
                IF (CompoNam(IC)(1:12).EQ.SSMLCN(IN)(1:12))THEN
                  ICF=IC
                  GOTO 363
                ENDIF
C              else
C                IF(CompoNam(IC)(1:12).EQ.SSMLCN(IN)(1:12)) THEN 
C                  ICF=IC
C                  GOTO 363
C                ENDIF
C              endif
 20         CONTINUE
363         IF (ICF.EQ.0) THEN
              WRITE(outs,'(a,a)')SSMLCN(IN),'not in mlc db.'
              CALL edisp(Ichanel,outs)
              GOTO 666
            ENDIF
C Affect the mlc nomber (within the db) to the selected Composite
            ISelComp(IN)=ICF
          ENDIF
C         IF (ITmp.EQ.NbSurSel) GOTO 613
611     CONTINUE
      ENDIF



C***********************
C For each seleced zone
C ------------------------
      DO 1 IZ = 1, NbZones
        TotDumpZ= 0.0
        TotBurnZ= 0.0
        TotRecyZ= 0.0

C If approach is model based, read the current zone information
        IF (ICalType.EQ.1) THEN
          ICurZone=nznog(IZ)
          call eclose(gversion(ICurZone),1.1,0.01,newgeo)
          if(newgeo)then
            call georead(IFIL+1,LGEOM(ICurZone),ICurZone,0,iuout,IER)
          else
            call egomin(IFIL+1,LGEOM(ICurZone),ICurZone,0,0,iuout,IER)
          endif
          call ZINFO(SNA,ZOA,PAZI,PELV,ZVOL)
          vol(ICurZone)=zvol
          context=.false.
        ENDIF

C Adapt the Nb of surface to loop trough according to the calculation type
        IF (ICalType.EQ.1) THEN
C for project, look at the selected zone
          NbSurf = NbSurSel(ICurZone)
          NbSurf = NSur
          LabelOut(1)=zname(ICurZone)
        ELSEIF (ICalType.EQ.2) THEN
C  For MLC(s) level, look at the selected composite(s)
          NbSurf = NComPik
          LabelOut(1)=' '
        ELSEIF (ICalType.EQ.3) THEN
C  Fora material, only on surface obviously
          NbSurf = 1
          LabelOut(1)=' '
        ENDIF

C***********************
C For each surface
C ------------------------
C Loop trough constructions
        DO 901 N=1,NbSurf
C          NSSurf = NSSurf + 1
          IF (ICalType.EQ.1) THEN 
            ICurCon= IZSTOCN(ICurZone,N)
            ICurComp = ISelcomp(ICurCon)
            ICurSurf = IE1(ICurCon)
            if (ISursel(ICurCon).ne.1) GOTO 901
            if (LabelTmp(2)(1:12).eq.Sname(ICurZone,N)(1:12)) then
              LabelOut(2)=' '
            else
              LabelOut(2)=Sname(ICurZone,N)
              LabelTmp(2)=Sname(ICurZone,N)
            endif
          ELSEIF (ICalType.EQ.2) THEN               
            ICurComp = LstComp(N)
            LabelOut(1)=' '
            LabelOut(2)=CompoNam(ICurComp)
          ELSEIF (ICalType.EQ.3) THEN               
            ICurComp = 1
          ENDIF

            
C If want to perform an calculation over whole zones, and whole surfaces in the 
C zones, It will not take account of the the surface related to a surface in a 
C previous zone. Thus, partition are not taken into account twice.
C No need to test the first zone 
          IF(IZ.GT.1)THEN
            Iother = iCurCon
            if ( (SSOTHER(Iother,1)(1:8).NE.'EXTERIOR') .OR.
     &                 (SSOTHER(Iother,1)(1:7).NE.'SIMILAR')) then
              IF (zname(nznog(IZ)).eq.'zname(nznog(1)') THEN
                      ii = 0
              ENDIF
              Exist=.FALSE.
C Loop over all zones to find if other side is a zone that was
C already scaned. No need to test the last zone 
              DO 345 IT=1,(IZ-1)
C if other side is one of the previous zone,  go to next surface
                if(SSOTHER(Iother,1)(1:lnblnk(SSOTHER(Iother,1))).EQ. 
     &             zname(nznog(IT))(1:lnzname(nznog(IT))) ) then
                  Exist=.TRUE.
                  IOpsitZone= ic2(IZSTOCN(ICurZone,N))
                  IOpositSurf = ie2(IZSTOCN(ICurZone,N))
                  IOpositCon= IZSTOCN(IOpsitZone,IOpositSurf)
                  IOpositComp = ISelcomp(IOpositCon)
C                  IMatch = 0
C        IF(CompoNam(ICurComp)(1:12).ne.CompoNam(IOpositComp)(1:12))then
C                    DO 393 ip=1, NbEIALay(ICurComp)
C                      IOpositeLay= NbEIALay(ICurComp)- (ip-1)
C                      OposLayName= LayerNam(IOpositComp,IOpositeLay)
C               if(LayerNam(ICurComp,ip)(1:12).ne.OposLayName(1:12)) Then
C                     IMatch = 1
C                      endif
C 393                Continue
C                   endif
C                  If (IMatch.eq.1) THEN
CC                    CALL edisp(Ichanel,'Constructions does not match')
C                    CALL edisp(Ichanel,' Material of connection')
C                    CALL edisp(Ichanel,' Current    |   Opposite ')
C                  DO 394 ip=1, NbEIALay(ICurComp)
C                    WRITE(outs,'(a12,2x,a12)')LayerNam(ICurComp,ip),
C     &                                LayerNam(IOpositComp,ip)             C 
C                    CALL edisp(Ichanel,outs)
C 394              Continue
C                  ENDIF
                  WRITE(outs,'(a12,2x,7a)')Zname(nznog(IZ)),  
     &                    Sname(nznog(IZ),N),' Already accounted as ',
     &                     'Zone: ', Zname(IOpsitZone),', ',
     &                     'Surface :', Sname(IOpsitZone,IOpositSurf)
                  CALL edisp(Ichanel,outs)

C Before to skip the surface calculation re-initialiase the surface  
                  DO 251 L =1,MIMP
                    LayerTot(L)= 0.0
                    CompoTot(L)= 0.0
                    StageTot(L)= 0.0
 251              CONTINUE
                  GOTO 901
                endif
C Next zone
345           CONTINUE
            endif
          ENDIF

C For each corresponding layers adapt the loop 
C trough material accoring to user approach
C If material approch: 1 layer (mat) otherwise use all
          IF (ICalType.EQ.1) THEN 
C If project based, nb layers = nb layers of the current surface of the current zone              
            NbLays= NbEIALay(ISelComp(ICurCon))
          ELSEIF (ICalType.EQ.2) THEN
C If MLC based, nb layers = nb layers of the current composite          
            NbLays = NbEIALay(ICurComp)
          ELSEIF (ICalType.EQ.3) THEN
C If material based, nb layers = 1      
            NbLays = 1
          ENDIF

C IF project based, up-date the surface area of current zone
          IF (ICalType.EQ.1) THEN
            Area = SNA(ICurSurf)
            call surfprm(nznog(IZ),ICurSurf,dupedges,Perimeter)
          ENDIF

C***********************
C For each layers
C ------------------------
          DO 23, J=1,NbLays 
            LabelOut(3)=LayerNam(ICurComp,J)
            LabelTmp(3)=LayerNam(ICurComp,J)

C Loop trough the steps, that are material related
 16         DO 999 M = IStep,LStep
C If step at composite level, skips this step at layer level
              if (M.EQ.3.or.M.eq.5.or.M.eq.6) then
                IF (IWB.EQ.3) THEN
                  GOTO 999
                ELSEIF (IWB.EQ.2) THEN
                  GOTO 999
                ELSEIF (IWB.EQ.1) THEN
C If only one step is requred, and is not at layer level,skip calculation at 
C layer level and go directly at the calculation at construction level
                  GOTO 66
                ENDIF
              endif

C If layer level, adapt the loop trough the K index, which can be either
C the max nb of Transport (MNbTrans), maximum of Assembly process (MNbAss)
C  or the maximum of maintenance(MNbM)
              if ((M.EQ.2).OR.(M.EQ.4).OR.(M.EQ.8).OR.(M.EQ.10)
     &              .OR.(M.EQ.12)) then
                Lindex=MNbTrans
              elseif (M.EQ.3) then
                Lindex=MNbAss
              elseif (M.EQ.6) then
                Lindex=MNbM
              else
                Lindex=1
              endif

C***********************
C For each impacts
C ------------------------
C Loop trough transport or maintenance or steps
              DO 112 K =1,Lindex
                DO 111 L =1,MIMP
                  RImpact = STEPIMP(ICurzone,ICurComp,J,K,L,M)
                  LayerTot(L)= LayerTot(L) + RImpact
                  CompoTot(L)= CompoTot(L) + RImpact
                  StageTot(L)= StageTot(L) + RImpact
                  ZoneTot(L)= ZoneTot(L) + RImpact
                  WholeTot(L)= WholeTot(L) + RImpact
                  ProjTot(L)= WholeTot(L) + RImpact
 111            CONTINUE
 112          CONTINUE

              LabelOut(4)=StepSName(M)
              LabelTmp(4)=StepSName(M)

C Write results at stage level, only if required by the user
              IF (itrc.EQ.4) THEN
                WRITE(outs,'(4(1x,a12),4(2x,1P1E9.3,1x))')
     &            (LabelOut(L),L=1,4),(StageTot(L),L=1,MIMP)
                CALL edisp(Ichanel,outs)
              ENDIF

C Re-initialise the stage impacts before to go to the next lCA step
              DO 24 L =1,MIMP
                  StageTot(L)= 0.0
 24           CONTINUE
C Next Step
 999        CONTINUE


C if reporting level is layer, adapt some of the labels
              IF (itrc.eq.3) THEN
                LabelOut(3)=LabelTmp(3)
C If step type is whole life cycle
                if (IWB.eq.3) then
                  LabelOut(4)='Whole Cycle '
C If Major step
                elseif (IWB.eq.2) then
                  IF (J.EQ.1) THEN
                    LabelOut(1)=LabelTmp(1)
                    LabelOut(2)=LabelTmp(2)
                    LabelOut(3)=LabelTmp(3)
                  ENDIF
C depends on the selected major step
                  IF (IW.EQ.1) THEN
                    LabelOut(4)='CONSTRUCTION'
                  ELSEIF (IW.EQ.2) THEN
                    LabelOut(4)='UTILISATION '
                  ELSEIF (IW.EQ.3) THEN
                    LabelOut(4)='DECONSTRUCT.'
                  ENDIF
C If elementary step
                elseif (IWB.eq.1) then
                  LabelOut(4)=LabelTmp(4)
                endif
                WRITE(outs,'(4(1x,a12),4(2x,1P1E9.3,1x))')
     &                (LabelOut(L),L=1,4),(LayerTot(L),L=1,MIMP)
                CALL edisp(Ichanel,outs)
              ENDIF


C if reporting level is stage, adapt some of the labels
              IF (itrc.eq.4) THEN
                LabelOut(3)=LabelTmp(3)
C If step type is whole life cycle
                if (IWB.eq.3) then
                  LabelOut(4)='Sub-sum=----'
C If Major step
                elseif (IWB.eq.2) then
                  LabelOut(1)=LabelTmp(1)
                  LabelOut(2)=LabelTmp(2)
                  LabelOut(3)='Total layers'
                  LabelOut(4)='------------'

C If elementary step
                elseif (IWB.eq.1) then
                  LabelOut(4)=LabelTmp(4)
                endif
                if (IWB.NE.1) then
                  WRITE(outs,'(4(1x,a12),4(2x,1P1E9.3,1x))')
     &                (LabelOut(L),L=1,4),(LayerTot(L),L=1,MIMP)
                  CALL edisp(Ichanel,outs)
                endif
              ENDIF


C Reset the total impacts for a layer, before to loop trough the next layer
              DO 26 L =1,MIMP
                LayerTot(L)= 0.0
 26           CONTINUE
C              IF ((LSetp.EQ.3))then
C                GOTO 665
C              endif
C Next Layer
 23         CONTINUE

C ------Composite level----------------------------
C After having looping trouht the impact at the layer level, 
C needs to calculate the impacts at the composite level

C A)  Assembly of the element. Is calculated only if elementary, 
C                              whole or Major CONSTRUCTION
 66         IF (((IWB.EQ.1).AND.(LStep.EQ.3)).OR.(IWB.EQ.3).OR.
     &          ((IWB.EQ.2).AND.(IW.EQ.1))) THEN
C If reporting level: surface, layer of stage
              if(itrc.ge.1) then
                LabelOut(3)='Whole compo.'
                LabelOut(4)=StepSName(3)
C Loop trough the assembly processes
                DO 8 K=1,MNbAss
                  if ((K.eq.1).OR.(ComAsPt(J,K)(1:4).ne.'none')) then
C If process exits, loop trough the impacts
                    DO 27 L =1,MIMP
                      Rimpact =  STEPIMP(ICurzone,ICurComp,J,K,L,3)
                      CompoTot(L)= CompoTot(L) + Rimpact
                      ZoneTot(L)= ZoneTot(L) + Rimpact
                      ProjTot(L)= ProjTot(L) + Rimpact
                      WholeTot(L)= WholeTot(L) + Rimpact
                      LayerTot(L)= 0.0
 27                 CONTINUE
C Display reults if user has required surface, layer or impacts level results
                    IF (itrc.gt.2) THEN
                      WRITE(outs,'(4(1x,a12),4(2x,1P1E9.3,1x))')
     &                    (LabelOut(L),L=1,4),
     &                    (STEPIMP(ICurzone,ICurComp,J,K,L,3),L=1,MIMP)
                      CALL edisp(Ichanel,outs)
                    ENDIF
                  endif
C Next assembly
 8              CONTINUE
              endif
C End of assembly stage
            ENDIF



C B) Assembly on the building. Calculated only if if elementary, whole 
C                              or Major CONSTRUCTION
            IF (((IWB.EQ.1).AND.(LStep.EQ.5)).OR.(IWB.EQ.3).OR.
     &          ((IWB.EQ.2).AND.(IW.EQ.1))) THEN
C If reporting level: surface, layer of stage
              if(itrc.ge.1) then
                LabelOut(3)='Whole compo.'
                LabelOut(4)=StepSName(5)
C Loop trough the assembly processes
                DO 18 K=1,MNbAss
                  if ((K.eq.1).OR.(AsProjPt(J,K)(1:4).ne.'none')) then
C If process exits, loop trough the impacts
                    DO 127 L =1,MIMP
                      Rimpact =  STEPIMP(ICurzone,ICurComp,J,K,L,5)
                      CompoTot(L)= CompoTot(L) + Rimpact
                      ZoneTot(L)= ZoneTot(L) + Rimpact
                      ProjTot(L)= ProjTot(L) + Rimpact
                      WholeTot(L)= WholeTot(L) + Rimpact
                      LayerTot(L)= 0.0
 127                CONTINUE
C Display reults if user has required surface, layer or impacts level results
                    IF (itrc.gt.2) THEN
                      WRITE(outs,'(4(1x,a12),4(2x,1P1E9.3,1x))')
     &                     (LabelOut(L),L=1,4),
     &                    (STEPIMP(ICurzone,ICurComp,J,K,L,5),L=1,MIMP)
                      CALL edisp(Ichanel,outs)
                    ENDIF
                  endif
C Next assembly
 18             CONTINUE
              endif
C End assembly on building stage
            ENDIF


C C) Maintenance of the composite  
            IF ((LStep.EQ.6).OR.(IWB.EQ.3).OR.
     &         ((IWB.EQ.2).AND.(IW.EQ.2))) THEN
              if(itrc.ge.1) then
                LabelOut(3)='Whole compo.'
                LabelOut(4)=StepSName(6)
C Loop trough the maintenance processes
                DO 7 K=1,MNbM
                  if ((K.eq.1).OR.(MaintPt(J,K)(1:4).ne.'none')) then
C If process exits, loop trough the impacts
                    DO 29 L =1,MIMP
                      Rimpact =  STEPIMP(ICurzone,ICurComp,J,K,L,6)
                      CompoTot(L)= CompoTot(L) + Rimpact
                      ZoneTot(L)= ZoneTot(L) + Rimpact
                      ProjTot(L)= ProjTot(L) + Rimpact
                      WholeTot(L)= WholeTot(L) + Rimpact
                      LayerTot(L)= 0.0
 29                   CONTINUE
                    IF (itrc.gt.2) THEN
C Display reults if user has required surface, layer or impacts level results
                      WRITE(outs,'(4(1x,a12),4(2x,1P1E9.3,1x))')
     &                          (LabelOut(L),L=1,4),
     &                    (STEPIMP(ICurzone,ICurComp,J,K,L,6),L=1,MIMP)
                      CALL edisp(Ichanel,outs)
                    ENDIF
                  endif
C Next Maintenance
 7              CONTINUE
              endif
C End of maintenance stage
            ENDIF


C This block update, the fields of LabelOut, which are used to display the
C the zone name, the surface name, the layer name and the life-cycle stage
C according to the selected option by the user.
C |    Zone    |   Surface  |    Layer   |   Cycle    |
C   LabelOut(1)  LabelOut(2)  LabelOut(3)  LabelOut(4) 

            if(itrc.eq.2) then
C if surface level reporting
              IF (ICalType.eq.1) THEN
                LabelOut(1)=Zname(ICurZone)
                LabelOut(2)=Sname(ICurZone,N)
              ENDIF
              LabelOut(3)='Tot. Surface'

C If elementary stage
              IF (IWB.EQ.1) THEN
                 LabelOut(4)=LabelTmp(4)
C If major stage,
              ELSEIF (IWB.EQ.2) THEN
C adapt the label "Cycle" depending on the focused major step
                if (IW.eq.1) then
                  LabelOut(4)='CONSTRUCTION'
                elseif (IW.eq.2) then
                  LabelOut(4)='UTILISATION '
                else
                  LabelOut(4)='DECONSTRUCT.'
                endif
C If whole life cycle
              ELSE
                    LabelOut(4)='Life-cycle  '
              ENDIF
C 665          WRITE(outs,'(4(1x,a12),4(2x,1P1E9.3,1x))')
              WRITE(outs,'(4(1x,a12),4(2x,1P1E9.3,1x))')
     &            (LabelOut(L),L=1,4), (CompoTot(L),L=1,MIMP)
              CALL edisp(Ichanel,outs)

C If Layer level
            elseif(itrc.eq.3) then
              IF (ICalType.EQ.1) THEN
                LabelOut(1)=Zname(ICurzone)
                LabelOut(2)=Sname(ICurzone,N)
                LabelOut(3)='Tot.surface='
              ELSEIF (ICalType.EQ.2) THEN
                 LabelOut(1)=' '
                 LabelOut(2)=CompoNam(ICurComp)
                 LabelOut(3)='Tot.compos.='
              ENDIF
              LabelOut(4)= '-----------'
 664          WRITE(outs,'(4(1x,a12),4(2x,1P1E9.3,1x))')
     &             (LabelOut(L),L=1,4), (CompoTot(L),L=1,MIMP)
              CALL edisp(Ichanel,outs)
              CALL edisp(Ichanel,' ')
C if life-cycle stage
            elseif (itrc.eq.4) then
              LabelOut(1)=Zname(ICurzone)
              LabelOut(2)= 'Tot.surface='
              LabelOut(3)= '-----------'
              LabelOut(4)= '-----------'
 663          WRITE(outs,'(4(1x,a12),4(2x,1P1E9.3,1x))')
     &             (LabelOut(L),L=1,4), (CompoTot(L),L=1,MIMP)
              CALL edisp(Ichanel,outs)

              CALL edisp(Ichanel,' ')
            endif

C Re-initialise the impacts at the surface level before to analyse next composite 
            DO 25 L =1,MIMP
              CompoTot(L)= 0.0
 25         CONTINUE
C Next Surface
 901      Continue


C For project based, display the results at zone level
          IF (IcalType.EQ.1) THEN
            if(itrc.gt.0) then
              WRITE(outs,'(1x,a12,2a,4(3x,1P1E9.3))')Zname(nznog(IZ)),
     &               'Total Zone = ','-------------------------',
     &               (ZoneTot(L),L=1,MIMP)
              CALL edisp(Ichanel,outs)
              CALL edisp(Ichanel,' ')
            endif
          ENDIF

C Re-initialiase the total at the zone level before to analyse the next zone
          DO 222 L =1,MIMP
            ZoneTot(L) = 0.0
 222        CONTINUE
C Next zones
 1      CONTINUE


C If a Project is analyse, then display the total for the project
        IF (IcalType.EQ.1) THEN
C Absolute value
          WRITE(outs,'(53x,a)')LabelB(1:lnblnk(LabelB))
          CALL edisp(Ichanel,outs)
          WRITE(outs,'(53x,a)')LabelD(1:lnblnk(LabelD))
          CALL edisp(Ichanel,outs)
          WRITE(outs,'(1x,a,4(3x,1P1E9.3))')
     &     'Total project (absolute value) = -----------------',
     &                (WholeTot(L),L=1,MIMP)
          CALL edisp(Ichanel,outs)
          CALL edisp(Ichanel,' ')
C Relative value
          SRE=0.
          DO 99 iz=1,nzg
             SRE = SRE + ZBASEA(IZ)
 99       Continue
          WRITE(outs,'(53x,a)')LabelB(1:lnblnk(LabelB))
          CALL edisp(Ichanel,outs)
          LabelD='  [kg/m2]* |  [kg/m2]* |  [kg/m2]* | [MJ/m2]* |'
          WRITE(outs,'(53x,a)')LabelD(1:lnblnk(LabelD))
          CALL edisp(Ichanel,outs)
          WRITE(outs,'(1x,a,4(3x,1P1E9.3))')
     &     'Total project (relative value) = -----------------',
     &                (WholeTot(L)/SRE,L=1,MIMP)
          CALL edisp(Ichanel,outs)
          CALL edisp(Ichanel,' ')
          WRITE(outs,'(1x,a,f9.1)')
     &      '* per square meter of floor area of selected zone(s) = ',
     &      SRE
          CALL edisp(Ichanel,outs)
          CALL edisp(Ichanel,' ')
         ENDIF

666   RETURN
      END




C Detailed
C********************* LCADet **************************
C
C This subroutine displays LCA results more detailed, but use the same 
C phylosophy as LCACalc


      SUBROUTINE LCADet(itrc,Ichanel)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "LCA.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/pophelp/h(60)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      character DESC*48
      integer NMLC,IPR,LAYERS
      real DTHK,DRAIR
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      
      integer IPRMAT  ! similar to IPR but with the index to
                      ! matdatarray array rather than DB position
      common/MLCMA/IPRMAT(MMLC,ME)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/surfsel/isursel(MCON),NbSurSel(MCOM),ISelCon(MCON),
     &               ISelComp(MCON)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      real sna,pazi,pelv
      COMMON/SDATA/SNA(MS),PAZI(MS),PELV(MS)
      COMMON/CompPik/IComfoc,NComPik,LstComp(MMLC)

      COMMON/ProjTxt/TxtCompo,TxtMat
      COMMON/EIAScnPt/FabPt(MMAT),TrPt(MMAT,MNbTrans),
     &                RecyPt(MMAT),ReTrPt(MMAT,MNbTrans),
     &                BurnPt(MMAT),BuTrPt(MMAT,MNbTrans),
     &                DumpPt(MMAT),DuTrPt(MMAT,MNbTrans),
     &                ComAsPt(MMLC,MNbAss),CoTrPt(MMLC,MNbTrans),
     &                MaintTyp(MMLC,MNbM),MaintPt(MMLC,MNbM),
     &                ComAsPa(MMLC,MNbAss),AsProjPt(MMLC,MNbAss),
     &                AsProjPa(MMLC,MNbAss),LayerCat(MMLC,ME),
     &                MainPart(MMLC,MNbM),LayerNam(MMLC,ME),
     &                LayerTyp(MMLC,ME)

      COMMON/EIAImp/FabImp(MMAT,MIMP),TrImp(MMAT,MNbTrans,MIMP),
     &          RecyImp(MMAT,MIMP),ReTrImp(MMAT,MNbTrans,MIMP),
     &          BurnImp(MMAT,MIMP),BuTrImp(MMAT,MNbTrans,MIMP),
     &          DumpImp(MMAT,MIMP),DuTrImp(MMAT,MNbTrans,MIMP),
     &          ComAsImp(MMLC,MNbAss,MIMP),CoTrImp(MMLC,MNbTrans,MIMP),
     &          AsProImp(MMLC,MNbAss,MIMP), RMainImp(MMLC,MNbM,MIMP)

      COMMON/EIADist/TrDist(MMAT,MNbTrans), CoTrDist(MMLC,MNbTrans),
     &                 ReTrDist(MMAT,MNbTrans), BuTrDist(MMAT,MNbTrans),
     &                 DuTrDist(MMAT,MNbTrans)



      COMMON/EIAMisc/NbEIALay(MMLC),ProjLife, RMatLife(MMAT),
     &               PerMaint(MMLC,MNbM),RecyRate(MMAT),BurnRate(MMAT),
     &               DumpRate(MMAT),AssLoss(MMAT),TrBrk(MMAT,MNbTrans),
     &               AsProBrk(MMAT,MNbTrans),CoTrBrk(MMLC,MNbAss),
     &               QuaLayer(MMLC,ME),FixLayer(MMLC,ME),DenLay(MMAT),
     &               SpecData(MMAT,2), NRi(MMLC,ME)

      COMMON/SPECMASS/SMPLayer(MMLC,ME),SMSLayer(MMLC,ME),IWhich,
     &                SupFab(MMLC,ME),CompSMP(MMLC),CompSMS(MMLC),
     &                SupTr(MMLC,ME),SupTrMat(MMLC,ME),WeigtMat

      COMMON/ECOCalcB/RecPWast(MMLC,ME), RecSWast(MMLC,ME),
     &               RecPTr(MMLC,ME,MNbTrans,MIMP),
     &               RecSTr(MMLC,ME,MNbTrans,MIMP),
     &               RecPPr(MMLC,ME,MIMP),RecSPr(MMLC,ME,MIMP), 
     &               DumPWast(MMLC,ME),DumSWast(MMLC,ME),
     &               DumPTr(MMLC,ME,MNbTrans,MIMP),
     &               DumSTr(MMLC,ME,MNbTrans,MIMP),
     &               DumPPr(MMLC,ME,MIMP),DumSPr(MMLC,ME,MIMP), 
     &               BurPWast(MMLC,ME),BurSWast(MMLC,ME),
     &               BurPTr(MMLC,ME,MNbTrans,MIMP),
     &               BurSTr(MMLC,ME,MNbTrans,MIMP),
     &               BurPPr(MMLC,ME,MIMP),BurSPr(MMLC,ME,MIMP)


      COMMON/ECOCalcA/SMassImp(MMLC,ME,MIMP),PMassImp(MMLC,ME,MIMP),
     &               TrSImp(MMLC,ME,MNbTrans,MIMP),
     &               TrPImp(MMLC,ME,MNbTrans,MIMP),
     &               ComAsIm(MMLC,MNbAss,MIMP), 
     &               PerAsIm(MMLC,MNbAss,MIMP),
     &               TrComImp(MMLC,ME,MNbTrans,MIMP),
     &               TrPerImp(MMLC,ME,MNbTrans,MIMP),
     &               ComBuImp(MMLC,MNbAss,MIMP),
     &               PerBuImp(MMLC,MNbAss,MIMP),
     &               ComCoImp(MMLC,MIMP),PerCoImp(MMLC,MIMP),
     &               ComSMain(MMLC,MNbAss,MIMP),
     &               ComPMain(MMLC,MNbAss,MIMP),
     &               SurReMas(MMLC,ME),PerReMas(MMLC,ME),
     &               SurReImp(MMLC,ME,MIMP),PerReImp(MMLC,ME,MIMP),
     &               SurLfImp(MMLC,MIMP),PerLfImp(MMLC,MIMP)

      COMMON/LCAUnit/UnitEner,UnitMass,UnitImp
      COMMON/LCAFlag/ILoss,ICalType,Irep
      COMMON/StepsName/StepName(NbSteps),LabelGen(3),GenUnt(3),
     &      Label(NbSteps+1),LabelUnt(NbSteps+1),LabelImp,ImpUnt
      COMMON/CompDim/HeightCo,WidthCo, Perimeter, Area
      common/EIAhigh/NbComp,NbMat,IMatID(MMLC,ME),ImatDbID(0:MMAT-1),
     &               LCIATag

      COMMON/DesMLC/MatNameco(MMAT),MatDesc(MMAT),MatCat(MMAT),
     &              CompoNam(MMLC), LayerDes(MMLC,ME)

      COMMON/ZFunction/ZFun(NbSteps,MMLC,ME)

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

      character matname*32,matdoc*248,matopaq*1
      common/matnamearray/matname(MGIT),matdoc(MGIT),matopaq(MGIT)

      integer matarrayindex ! the indes within matdatarray
      logical closemat1,closemat2
      logical newgeo  ! to use for testing if new/old geometry file.

      DIMENSION ITEM(NbSteps+5)
      DIMENSION WeitLayr(ME),WtLayrDu(ME),WtLayrBu(ME),WtLayrRe(ME)

      CHARACTER LCFGF*72
      CHARACTER TxtCompo*22,TxtMat*22

      CHARACTER*12 FabPt,TrPt,RecyPt,ReTrPt,BurnPt,BuTrPt,DumpPt,DuTrPt
      CHARACTER*12 ComAsPt,CoTrPt,MaintPt,AsProjPt
      CHARACTER*1 ComAsPa,MaintTyp,LayerCat,MainPart,AsProjPa,LayerTyp
      
      CHARACTER*12 CompoNam,LayerNam,MatNameco
      CHARACTER*72 MatDesc,MatCat,LayerDes, TITL


      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      CHARACTER SSPARENT*12
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      CHARACTER UnitEner*5, UnitMass*4, UnitImp*8
      CHARACTER*72 NAM
      CHARACTER H*72,outs*160,ITEM*36, Tmpstring*72
      CHARACTER StepName*36
      CHARACTER*110 LabelGen, GenUnt, Label, LabelUnt, LabelImp, ImpUnt
      LOGICAL Exist, dupedges, context

      REAL HeightCo,WidthCo, Perimeter, Area, WeigtMat,WeitLayr
      REAL TotDumpM, TotBurnM, TotRecyM
      REAL CompoMass, TotMass, DTHKTot
      REAL LCAValue(MIMP),LCAValuP(MIMP),LCAValuS(MIMP),StepImp
      REAL LayerTot(MIMP),CompoTot(MIMP),ZoneTot(MIMP),StageTot(MIMP),
     &     WholeTot(MIMP),ProjTot(MIMP)
      INTEGER NbEIALay, ICalType
 
C Find version of materials database.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)

      dupedges=.false.
      newgeo=.false.  ! assume older format geometry.
      IV = 0

      LabelImp = '    GWP    |    AP     |   POCP    |   NRE   '
      WRITE(ImpUnt,'(9A)')
     &    '    ', UnitMass, '   |   ', UnitMass, '    |   ', UnitMass,
     &      '    |   ', UnitEner, '    '
      
      LabelGen(1) = '              Stage              |'
      GenUnt(1) =   '                                 |'
      LabelGen(2) = '        Zone        |'
      GenUnt(2) =   '                    |'
      LabelGen(3) = 
     &        '    Surface   |  Composite   |   Mass    |'
      GenUnt(3) = 
     &        '              |              |   [kg]    |'

      Label(1) =     '      Composite part       |   Mass    |'
      LabelUnt(1) =  '                           |   [kg]    |'
      Label(2) =
     &  '   Material   |    Mass   |   Transport  |  ZF | Dist.  |'
      LabelUnt(2)= 
     &  '              |    [kg]   |   category   | [-] | [km]   |'
      Label(3) =     '   Assembly on building  |'
      LabelUnt(3) =  '           process       |'
      Label(4) = 
     &  '    Layer     |   Mass    |   Transport  |  Dist.  |'
      LabelUnt(4) =
     &  '              |   [kg]    |   category   |  [km]   |'
      Label(5) =  Label(3) 
      LabelUnt(5) = LabelUnt(3) 
      Label(6)  =   '         Maintenance       |  Mainten. |'
      LabelUnt(6) = '                           | freq.[y-1]|'
      Label(7) =
     &  '   Material   | Service  | Nb of  |'
      LabelUnt(7) = 
     &  '              | life [y] | replac.|'

C Landfill labels
      Label(8) =  
     &  '   Material   |    Mass   |   Transport  |  Dist.  |'
      LabelUnt(8)= 
     &  '              |    [kg]   |   category   |   [km]  |'
      Label(9) =
     &  '   Material   |   Dump   |    Mass   |'
      LabelUnt(9) =
     &  '              | rate [%] |    [kg]   |'

C Incineration labels
      Label(10) =
     &  '   Material   |    Mass   |   Transport  |  Dist.  |'
      LabelUnt(10) =
     &  '              |    [kg]   |   category   |   [km]  |'
      Label(11) = 
     &  '   Material   | Incine.  |    Mass   |'
      LabelUnt(11) =
     &  '              | rate [%] |    [kg]   |'

C Recycling labels
      Label(12) =
     &  '   Material   |    Mass   |  Transport   |  Dist.  |'
      LabelUnt(12) =
     &  '              |    [kg]   |  category    |   [km]  |'
      Label(13) = 
     &  '   Material   | Recyling |    Mass   |'
      LabelUnt(13) =
     &  '              | rate [%] |    [kg]   |'

C Disposal mass
      Label(14) =  
     &'   Material   | Tot. Mass |      Recycling       |    Inciner' //
     &'ation      |        Dumped        |'
      LabelUnt(14) =
     &'              |    [kg]   | rate [%] |    [kg]   | rate [%] |' //
     &'    [kg]   | rate [%] |    [kg]   |'
      Label(15) =  
     &' Total mass | Recycled mass | Inciner.mass | Dumped mass |'
      LabelUnt(15) =
     &'      [kg]  |     [kg]      |    [kg]      |     [kg]    |'

      StepName(1) = 'Material fabrication'
      StepName(2) = 'Transp. to assembly site '
      StepName(3) = 'Construction assembly '
      StepName(4) = 'Transport to building site '
      StepName(5) = 'Assembly on the building '
      StepName(6) = 'Construction maintenance '
      StepName(7) = 'Material replacement '
      StepName(8) = 'Dumped waste transport '
      StepName(9) = 'Deposite in landfill process '
      StepName(10)= 'Incinerated waste transport '
      StepName(11)= 'Incineration process '
      StepName(12)= 'Recycled waste transport '
      StepName(13)= 'Recycling process '
      StepName(14)= 'Disposal mass '


C Whih LCA display ; elementary step or major step ?
      H(1)='Selection of the LCA results'
      H(2)='----------------------------'
      H(3)=' '
      H(4)='LCA results can be display as :'
      H(5)=' '
      H(6)='1) Single step: In this case, the results correspond only '
      H(7)='   to one LCA elementary step, such as material mass '
      H(8)='   impacts or composite assembly. If this option is '
      H(9)='   selected, you will have to select a step in a list.'
      H(10)=' '
      H(11)='2) Major phase: In this case, the results correspond to a'
      H(12)='   group of elementary step define in 1). If this option '
      H(13)='   is selected, you will have to select which major step.'
      H(14)=' '
      H(15)='3) Whole cycle: In this case, the results correspond to '
      H(16)='   the impacts from cradle to grave.'

3     IWB=0
      IWBB=0
      IW = 0

C ask which step only in the interactive mode and not when exporting 
C the results to a file
      if (Ichanel.ne.iuout)then
        CALL EASKABCD(' ', 'Do you want to add in output file :',
     &                  ' An elementary phase ',' A major phase ',
     &                  ' Whole life cycle ',' Cancel ',IWB,16)
        IF(IWBB.EQ.4) then
          GOTO 666 
        ELSE
          IWB=IWBB
        ENDIF
      else
        CALL EASKABCD(' ', 'Do you want to analyse:',
     &                  ' An elementary phase ',' A major phase ',
     &                  ' Whole life cycle ',' Cancel ',IWB,16)
      endif
      CALL edisp(Ichanel,' ')


C Initialisation
      do 142 I=1,MIMP
        LCAValue(I)=0.0
        LCAValuP(I)=0.0
        LCAValuS(I)=0.0
        LayerTot(I)=0.0
        CompoTot(I)=0.0
        ZoneTot(I)=0.0
        StageTot(I)=0.0
        WholeTot(I)=0.0
        ProjTot(I)=0.0
 142  continue

      DTHKTot = 0.0
      TotDumpM= 0.0
      TotBurnM= 0.0
      TotRecyM= 0.0
      TotDumpZ= 0.0
      TotBurnZ= 0.0
      TotRecyZ= 0.0
      CompoMass = 0.0
      CompoMasI = 0.0
      TotMass = 0.0      

C Display the list of elementary step, if user want a single 
C step calculation
       IF (IWB .EQ.1) THEN
10      INO = -4
        ITEM(1)=                '  SELECT AN ELEMENTARY LCA STAGE  '
        ITEM(2)=                '----------------------------------'
        DO 5 I = 1,NbSteps
          WRITE(ITEM(2+I),'(A)') StepName(I)(1:lnblnk(StepName(I)))
5       CONTINUE
        ITEM(NbSteps+3)=         '----------------------------------'
        ITEM(NbSteps+4)=         ' ?  Help                          '
        ITEM(NbSteps+5)=         ' -  Cancel                        '
        NITEMS = NbSteps+5
        CALL EMENU(' LCA elementary stages',ITEM,NITEMS,INO)

        IF (INO.EQ.NITEMS) THEN
C If no single step has been selected go back
           GOTO 666
        ELSEIF(INO.EQ.NITEMS-1)THEN
          H(1)='ELEMENTARY LCA PHASE'
          H(2)='--------------------'
          H(3)=' '
          H(4)='The LCA results display will correspond to your '
          H(5)='selection. The LCA elementary phase correspond to :'
          H(6)='  '
          H(7)=' 1 Materials fabrication : Impacts due to materials'
          H(8)='   fabrication. Include processes from the material '
          H(9)='   extraction to the material producrion factory.'
          H(10)=' 2 Transp. to construction site : Impacts due to '
          H(11)='   materials transportation from material production'
          H(12)='   factory site to the composite assembly site. This'
          H(13)='   latest site is either the building site if the '
          H(14)='   composite is build on the building site (such as'
          H(15)='   a brick wall) or an other factory in'
          H(16)='   the case of a prefabricated composite (such as'
          H(17)='   window.'
          H(18)=' 3 Composite assembly : Impacts due to the processes'
          H(19)='   required to assemble the composite on the '
          H(20)='   composite site.'
          H(21)=' 4 Transport to building site : Impacts due to the'
          H(22)='   composite transport. Only <> 0 for prefabricated'
          H(23)='   composite.'
          H(24)=' 5 Assembly on the building : Impacts due to the '
          H(25)='   assembly of the composite on the building.'
          H(26)=' 6 Building maintenance : Impacts due to the building'
          H(27)='   maintenance over its lifetime.' 
          H(28)=' 7 Dumped waste transport : Impacts due to the part '
          H(29)='   of waste transported from the building site to the'
          H(30)='   dump site.'
          H(31)=' 8 Dumped waste treatment : Impacts due to the dumped'
          H(32)='   waste treatment at the dump site.'
          H(33)=' 9 Dumped waste mass : Mass of the dumped waste.'
          H(34)=' 10 Incinarated waste transport : Impacts due to the '
          H(35)='    part of waste transported from the building site '
          H(36)='    to the incinaration site.'
          H(37)=' 11 Incinarated waste treatment : Impacts due to the '
          H(38)='    incinaration treatment at the incinaration site.'
          H(39)=' 12 Incinarated waste mass : Mass of the incinarated '
          H(40)='    waste.'
          H(41)=' 13 Recycled waste : Mass of recycled waste '
          CALL PHELPD('ELEMENTARY LCA PHASE menu',41,'-',0,0,IER)
          GOTO 10
        ENDIF

C If Selection of a Major stage, then ask for which one
      ELSEIF (IWB .EQ.2) THEN
        CALL EASKABCD('LCA impacts:',' Which major stage ? ',
     &     ' Building construction ',' Building maintenance ',
     &     ' Building deconstruction ', ' CANCEL ',IW,20)
        IF (IW.EQ.4) GOTO 666

C Cancel LCA calculation if analyse type does not exist
      ELSEIF (IWB .EQ. 4) THEN
        GOTO 666
C End of stage selection
      ENDIF

C Cancel LCA calculation if results display not yet implemented
      IF ((IRep .EQ. 2).AND.(Itrc.EQ.4)) THEN
        CALL edisp(Ichanel, 
     &            'Sorry, results display but not yet implemented.')
        GOTO 666
      ENDIF


C According to user selection, define the starting Elementary Step (IStep)
C and the Last Elementary Step (LStep) for the looging trough the steps
      IF (IWB .EQ. 1) THEN
C If single step, then Starting step = Ending step
        IStep = INO-2
        LStep = IStep
      ELSEIF (IWB .EQ. 2) THEN
C If Major step calculation, then Starting step and  Ending step
C depends on the which major step was selected previously (IW)
        IF (IW .EQ. 1) THEN
          IStep = 1
          LStep = 5
        ELSEIF (IW .EQ. 2) THEN
          IStep = 6
          LStep = 7
        ELSEIF (IW .EQ. 3) THEN
          IStep = 8
          LStep = 13
        ELSE
          IStep = 0
          LStep = 0
        ENDIF
      ELSEIF (IWB .EQ. 3) THEN
        IStep = 1
        LStep = 13
      ENDIF

        
C Display message wether or not results include break and loss
      IF (M .EQ. 2) Then         
        if (ILoss .EQ. 0) then 
          CALL edisp(Ichanel,' Results include break & loss')
        else 
          CALL edisp(Ichanel,' Results does not include break & loss')
        endif   
      ENDIF         
         
C Display the main title
      IF (IWB.EQ.1) THEN
C        CALL edisp(Ichanel,'Phase:')
      ELSEIF (IWB.EQ.2) THEN
C A Major phase is anlalysed
        if (ICalType.eq.1) then
          IF (IW.EQ.1) THEN        
            CALL edisp(Ichanel,
     &'Phase: Build. CONSTRUCTION = Fabrication + transport + assembly')
          ELSEIF (IW.EQ.2) THEN
            CALL edisp(Ichanel,
     &' Phase: Building UTILISATION = Maintenance + replacement')
          ELSEIF (IW.EQ.3) THEN
            CALL edisp(Ichanel,
     &' Phase; Building ELIMINATION = Deconstruction + waste treatment')
          ENDIF
        elseif (ICalType.eq.2) then
          IF (IW.EQ.1) THEN        
            CALL edisp(Ichanel,
     &' Phase: MLC CONSTRUCTION = Fabrication + transport + assembly')
          ELSEIF (IW.EQ.2) THEN
            CALL edisp(Ichanel,
     &' Phase: MLC UTILISATION = Maintenance + replacement')
          ELSEIF (IW.EQ.3) THEN
            CALL edisp(Ichanel,
     &' Phase; MLC ELIMINATION = Deconstruction + waste treatment')
          ENDIF
        elseif (ICalType.eq.3) then
          IF (IW.EQ.1) THEN        
            CALL edisp(Ichanel,
     &'Phase: Mater. CONSTRUCTION = Fabrication + transport + assembly')
          ELSEIF (IW.EQ.2) THEN
            CALL edisp(Ichanel,
     &' Phase: Material UTILISATION = Maintenance + replacement')
          ELSEIF (IW.EQ.3) THEN
            CALL edisp(Ichanel,
     &' Phase; Material ELIMINATION = Deconstruction + waste treatment')
          ENDIF
        else
          CALL edisp(Ichanel,'This lcafoc does not exists')
        endif
        CALL edisp(Ichanel,' ------------------')
        CALL edisp(Ichanel,' ')
      ELSEIF (IWB.EQ.3) THEN
C Whole life cycle is anlalysed
        if (ICalType.eq.1) then
          CALL edisp(Ichanel,' Phase: Building life-cycle')
        elseif (ICalType.eq.2) then
          CALL edisp(Ichanel,' Phase: MLC life-cycle')
        elseif (ICalType.eq.3) then
          CALL edisp(Ichanel,' Phase: Material life-cycle')
        else
        endif
        CALL edisp(Ichanel,' ------------------')
        CALL edisp(Ichanel,' ')
      ENDIF
      WRITE(outs,'(a,a)')


C Display the column labels for stage and zone level
      IF (itrc.EQ.0) THEN
        WRITE(outs,'(a,a)')LabelGen(itrc+1)(1:lnblnk(LabelGen(itrc+1))),
     &                     LabelImp(1:lnblnk(LabelImp))
        CALL edisp(Ichanel,outs)
        WRITE(outs,'(a,a)')GenUnt(itrc+1)(1:lnblnk(GenUnt(itrc+1))), 
     &                          ImpUnt(1:lnblnk(ImpUnt))
        CALL edisp(Ichanel,outs)
      ENDIF


C Adapt the loop accoring to user approach
C If project approch: selected surface(s)
C If composite approch: selected composite(s)
C If material approch: 1 surface (mat) 
      IF (ICalType.EQ.1) THEN
        NbZones = nzg
      ELSEIF (ICalType.EQ.2) THEN
        NbZones = 1
      ELSEIF (ICalType.EQ.3) THEN
        NbZones = 1
      ENDIF

      NbSurf=0
      DO 610 IS = 1,MCON
        ISelCon(IS) = 0
610   CONTINUE

C Group and count the selected surfaces, then find the correponding
C mlc index in bcf file corresponding to current surface when 
C all selected surface scannes exit the loop
      IF (ICalType.EQ.1) THEN               
        ITmp = 0
        DO 611 IN = 1,MCON
          IF (isursel(IN).EQ.1) THEN
C Add an element in the array only if connection has been selected
            ITmp = ITmp + 1
C            ISelCon(ITmp) = IN

C Look for the mlc index (IFC)
C Scan the list of MLC name in database (CompoNam) and check
C if it correspond to the selected connection (SSMLCN)
C if so affect the MLC nomber to ISelComp and pass to the next 
C Selected connection
            lstr=lnblnk(SMLCN(IN))
            ICF=0
            DO 20 IC=1,NbComp
Cxxx              if(lstr.le.MS)then
                IF (CompoNam(IC)(1:12).EQ.SSMLCN(IN)(1:12))THEN
                  ICF=IC
                  GOTO 363
                ENDIF
Cxxx              else
Cxxx                IF(CompoNam(IC)(1:12).EQ.SSMLCN(IN)(1:12)) THEN 
Cxxx                  ICF=IC
Cxxx                  GOTO 363
Cxxx                ENDIF
Cxxx              endif
 20         CONTINUE
363         IF (ICF.EQ.0) THEN
              WRITE(outs,'(a,a)')SSMLCN(IN),'not in mlc db.'
              CALL edisp(Ichanel,outs)
              GOTO 666
            ENDIF
C Affect the mlc nomber (within the db) to the selected Composite
            ISelComp(IN)=ICF
          ENDIF
611     CONTINUE
      ENDIF

C Reinitialise the Mass of eliminated material
      TotDumpS= 0.0
      TotBurnS= 0.0
      TotRecyS= 0.0

C For each selected elemetary stage
C ------------------------
613   DO 999 M = IStep,LStep

C Display stage title
        IF (itrc.NE.0) THEN
          IF ((IWB.EQ.1 .OR. itrc.GE.1) .OR. 
     &          (IWB.GT.1 .AND. itrc.NE.0)) THEN
            WRITE(outs,'(a,a)')' Elementary stage : ', StepName(M)
            CALL edisp(Ichanel,outs)
            if (ILoss.eq.0) then 
              WRITE(outs,'(a)')' Loss and Breakage : Included'
            else
              WRITE(outs,'(a)')' Loss and Breakage : Not included'
            endif
            CALL edisp(Ichanel,outs)
          ENDIF
        ENDIF
        if ((ICalType.eq.1).AND.(NbZones.gt.1)) then
          CALL edisp(Ichanel,
     &       ' Total for zone does not include contiguous surfaces !')
        endif
C Display the column labels for stage and zone level
        IF (itrc .EQ. 1) THEN
          if ((ICalType.le.2).and.(M.eq.14)) then
            WRITE(outs,'(a,a)')
     &          LabelGen(ICalType+1)(1:lnblnk(LabelGen(ICalType+1))),
     &          Label(15)(1:lnblnk(Label(15)))
          else
            WRITE(outs,'(a,a)')
     &           LabelGen(ICalType+1)(1:lnblnk(LabelGen(ICalType+1))),
     &           LabelImp(1:lnblnk(LabelImp))
          endif
          CALL edisp(Ichanel,outs)
          if ((ICalType.le.2).and.(M.eq.14)) then
            WRITE(outs,'(a,a)')
     &              GenUnt(ICalType+1)(1:lnblnk(GenUnt(ICalType+1))),
     &              LabelUnt(15)(1:lnblnk( LabelUnt(15)))
          else
            WRITE(outs,'(a,a)')
     &              GenUnt(ICalType+1)(1:lnblnk(GenUnt(ICalType+1))),
     &                         ImpUnt(1:lnblnk(ImpUnt))
          endif
          CALL edisp(Ichanel,outs)
        ENDIF

C Display for each stage the results according to reporting toggle
        IF(IStep.GE.1 .AND. LStep.LE.Nbsteps)THEN
C        NSSurf = 0

C For each seleced zone(s)
C ------------------------
        DO 997 IZ = 1, NbZones
          TotDumpZ= 0.0
          TotBurnZ= 0.0
          TotRecyZ= 0.0

C If approach is model based, read the current zone information
          IF (ICalType.EQ.1) THEN
            ICurZone=nznog(IZ)
            call eclose(gversion(ICurZone),1.1,0.01,newgeo)
            if(newgeo)then
              call georead(IFIL+1,LGEOM(ICurZone),ICurZone,0,iuout,
     &          IER)
            else
              call egomin(IFIL+1,LGEOM(ICurZone),ICurZone,0,0,iuout,
     &          IER)
            endif
            call ZINFO(SNA,ZOA,PAZI,PELV,ZVOL)
            vol(ICurZone)=zvol
            context=.false.
          ENDIF

C Display zone name if surface level
          IF (itrc.EQ.2) THEN
            if (ICalType.EQ.1) then
              WRITE(outs,'(a,a)') ' Zone : ',Zname(nznog(IZ))
              CALL edisp(Ichanel,outs)
              CALL edisp(Ichanel,' ')
            endif
            if (M.EQ.14) then
              WRITE(outs,'(a,a)')Label(M)(1:lnblnk(Label(M)))
              CALL edisp(Ichanel,outs)
              WRITE(outs,'(a,a)')LabelUnt(M)(1:lnblnk(LabelUnt(M)))
              CALL edisp(Ichanel,outs)
            else
              WRITE(outs,'(a,a)')
     &                     LabelGen(itrc+1)(1:lnblnk(LabelGen(itrc+1))),
     &                     LabelImp(1:lnblnk(LabelImp))
              CALL edisp(Ichanel,outs)
              WRITE(outs,'(a,a)')
     &                         GenUnt(itrc+1)(1:lnblnk(GenUnt(itrc+1))),
     &                         ImpUnt(1:lnblnk(ImpUnt))
              CALL edisp(Ichanel,outs)
            endif
          ENDIF
          TotMass = 0.0

C Adapt the Nb of surface to loop trough according to the calculation type
          IF (ICalType.EQ.1) THEN
C for project, look at the selected zone
C                NbSurf = NbSurSel(ICurZone)
            NbSurf = NSur
          ELSEIF (ICalType.EQ.2) THEN
C  For MLC(s) level, look at the selected composite(s)
            NbSurf = NComPik
          ELSEIF (ICalType.EQ.3) THEN
C  Fora material, only on surface obviously
            NbSurf = 1
          ENDIF


C For each selected surface(s)
C ------------------------
          DO 901 N=1,NbSurf
C            NSSurf = NSSurf + 1
            IF (ICalType.EQ.1) THEN  
              ICurCon= IZSTOCN(ICurZone,N)
              ICurComp = ISelcomp(ICurCon)
              ICurSurf = IE1(ICurCon)
              if (ISursel(ICurCon).ne.1) GOTO 901
            ELSEIF (ICalType.EQ.2) THEN               
              ICurComp = LstComp(N)
            ELSEIF (ICalType.EQ.3) THEN               
              ICurComp = 1
            ENDIF
C If want to perform an calculation over whole zones, and whole 
C surfaces in the zones, It will not take account of the the 
C surface related to a surface in a previous zone. Thus, 
C partition are not taken into account twice.
C No need to test the first zone 
            IF(IZ.GT.1)THEN
              Iother = iCurCon
              if ( (SSOTHER(Iother,1)(1:8).NE.'EXTERIOR') .OR.
     &                 (SSOTHER(Iother,1)(1:7).NE.'SIMILAR')) then
                IF (zname(nznog(IZ)).eq.'zname(nznog(1)') THEN
                      ii = 0
                ENDIF
                Exist=.FALSE.
C Loop over all zones to find if other side is a zone that was
C already scaned. No need to test the last zone 
                DO 345 IT=1,(IZ-1)
C if other side is one of the previous zone,  go to next surface
                  if(SSOTHER(Iother,1)(1:lnblnk(SSOTHER(Iother,1))).EQ. 
     &              zname(nznog(IT))(1:lnzname(nznog(IT))) ) then
                    Exist=.TRUE.
                    IOpsitZone= ic2(IZSTOCN(ICurZone,N))
                    IOpositSurf = ie2(IZSTOCN(ICurZone,N))
                    IOpositCon= IZSTOCN(IOpsitZone,IOpositSurf)
                    IOpositComp = ISelcomp(IOpositCon)
C                    IMatch = 0
C                    if(CompoNam(ICurComp)(1:12).ne.
C     &                                 CompoNam(IOpositComp)(1:12))then
C                      DO 393 ip=1, NbEIALay(ICurComp)
C                        IOpositeLay= NbEIALay(ICurComp)- (ip-1)
C                        OposLayName= LayerNam(IOpositComp,IOpositeLay)
C                        if(LayerNam(ICurComp,ip)(1:12).ne.
C     &                                           OposLayName(1:12)) then
C                          IMatch = 1
C                        endif
C 393                  CONTINUE
C                    endif
C                    if (IMatch.eq.1) then
C                      CALL edisp(Ichanel,'Constructions does not match')
C                      CALL edisp(Ichanel,' Material of connection')
C                      CALL edisp(Ichanel,' Current    |   Opposite ')
C                     DO 394 ip=1, NbEIALay(ICurComp)
C                       WRITE(outs,'(a12,2x,a12)')LayerNam(ICurComp,ip),
C     &                                LayerNam(IOpositComp,ip)             
C                        CALL edisp(Ichanel,outs)
C 394                  CONTINUE
C                    endif
                    WRITE(outs,'(a12,2x,7a)')zname(nznog(IZ)),  
     &                    Sname(nznog(IZ),N),' Already accounted as ',
     &                     'Zone: ', zname(IOpsitZone),', ',
     &                     'Surface :', Sname(IOpsitZone,IOpositSurf)
                    CALL edisp(Ichanel,outs)

C Before to skip the surface calculation re-initialiase the surface  
                    DO 251 L =1,MIMP
                      LayerTot(L)= 0.0
                      CompoTot(L)= 0.0
                      StageTot(L)= 0.0
 251                CONTINUE
                    GOTO 901
                  endif
C Next zone
345             CONTINUE
              endif
            ENDIF



C Initialisation
            CompoMass = 0.0
            CompoMasI = 0.0
            TotDumpM = 0.0
            TotBurnM = 0.0
            TotRecyM = 0.0
            DO 222 IY=1,ME
              WeitLayr(IY)=0.0
222         CONTINUE

C For each corresponding layers adapt the loop 
C trough material accoring to user approach
C If material approch: 1 layer (mat) otherwise use all
            IF (ICalType.EQ.1) THEN 
C If project based, nb layers = nb layers of the current surface of the current zone     
              NbLays= NbEIALay(ISelComp(ICurCon))
            ELSEIF (ICalType.EQ.2) THEN
C If MLC based, nb layers = nb layers of the current composite          
              NbLays = NbEIALay(ICurComp)
            ELSEIF (ICalType.EQ.3) THEN
C If material based, nb layers = 1      
              NbLays = 1
            ENDIF

C IF project loaded up-date the area to current surface area
            IF (ICalType.EQ.1) THEN
              Area = SNA(ICurSurf)
              call surfprm(nznog(IZ),ICurSurf,dupedges,Perimeter)
            ENDIF

C Re-Initialisation
            DO 22, II=1,MImp 
              LayerTot(II) = 0.0
              LCAvaluP(II)=0.0
              LCAvaluS(II)=0.0
  22        CONTINUE                  


C For each corresponding layers
C ------------------------
            DO 23, J=1,NbLays 
C Mass of one surface and sum over the zone
              if (ICalType.EQ.3) then
                  WeitLayr(J) = WeigtMat
              else
                IF (LayerCat(ICurComp,J) .EQ. 'P') THEN
                  WeitLayr(J) = SMPLayer(ICurComp,J)*
     &                              SupFab(ICurComp,J)* Perimeter
                ELSE
                  WeitLayr(J)=SMSLayer(ICurComp,J)*
     &                            SupFab(ICurComp,J)* Area
                ENDIF
              endif
              WtLayrDu(J) = WeitLayr(J)*DumpRate(IMatID(ICurComp,J))
     &                          *(REAL(NRi(ICurComp,J)+1))
              WtLayrBu(J) = WeitLayr(J)*BurnRate(IMatID(ICurComp,J))
     &                          *(REAL(NRi(ICurComp,J)+1))
              WtLayrRe(J) = WeitLayr(J)*RecyRate(IMatID(ICurComp,J))
     &                          *(REAL(NRi(ICurComp,J)+1))
              CompoMass = CompoMass + WeitLayr(J)
              CompoMasI = CompoMasI + WeitLayr(J)/SupFab(ICurComp,J)
              TotDumpM = TotDumpM + WtLayrDu(J)
              TotBurnM = TotBurnM + WtLayrBu(J)
              TotRecyM = TotRecyM + WtLayrRe(J)
 23         CONTINUE



            IF (itrc.EQ.3) THEN
              IF (ICalType.EQ.1) THEN
                IV = IV + 1
                WRITE(outs,93) ' Zone: ',zname(nznog(IZ)),
     &                         ' Surface: ', SName(nznog(IZ),ICurSurf),
     &                         ' Composite: ',SMLCN(ICurSurf),
     &                         'Area [m2]: ',Area,
     &                         'Perim. [m]: ',Perimeter
 93             FORMAT(6(a),2(2x,a,1x, F6.2))
                CALL edisp(Ichanel,outs)
              ELSEIF (ICalType.EQ.2) THEN
                WRITE(outs,'(2a)')' Composite : ',CompoNam(ICurComp)
                CALL edisp(Ichanel,outs)
                WRITE(outs,'(a,f6.2)') ' Area : ',Area
                CALL edisp(Ichanel,outs)
                WRITE(outs,'(a,f6.2)') ' Perimeter : ',Perimeter
                CALL edisp(Ichanel,outs)
              ENDIF
              CALL edisp(Ichanel,' ')
            ENDIF

            IF (itrc .EQ. 3) THEN
C If 13 Recycle mass, nos Impacts labels to display 
              IF (M.EQ.14) THEN
                WRITE(outs,'(a,a)')Label(M)(1:lnblnk(Label(M)))
                CALL edisp(Ichanel,outs)
                WRITE(outs,'(a,a)') LabelUnt(M)(1:lnblnk(LabelUnt(M)))
                CALL edisp(Ichanel,outs)
              ELSE
                IF ((ICalType.EQ.3).AND.((M.EQ.1).OR.(M.EQ.4))) THEN
                  IF (M.EQ.1) THEN
                    WRITE(outs,'(a,a)')
     &               '         Material          |   Mass    |',LabelImp
                    CALL edisp(Ichanel,outs)
                  ELSEIF (M.EQ.4) THEN
                    WRITE(outs,'(a,a)')
     &           '   Material   |   Mass    |   Transport  |  Dist.  |',
     &                              LabelImp
                    CALL edisp(Ichanel,outs)
                  ENDIF
                ELSE
                  WRITE(outs,'(a,a)')Label(M)(1:lnblnk(Label(M))),
     &                               LabelImp(1:lnblnk(LabelImp))
                  CALL edisp(Ichanel,outs)
                ENDIF 
                WRITE(outs,'(a,a)') LabelUnt(M)(1:lnblnk(LabelUnt(M))),
     &                              ImpUnt(1:lnblnk(ImpUnt))
                CALL edisp(Ichanel,outs)
              ENDIF
            ENDIF

C For whole composite
C -------------------
C 3 Composite assembly (Calculation + display)
            IF (M.EQ.3) THEN
C First transport line display is different than others
              DO 601 K=1,MNbAss
                if (ComAsPt(ICurComp,K)(1:4).ne.'none') then
                  DO 602 L=1,MIMP
                    Rimpact=STEPIMP(ICurzone,ICurComp,J,K,L,3)
                    LayerTot(L)=LayerTot(L) + Rimpact
                    CompoTot(L)= CompoTot(L)+ Rimpact
                    ZoneTot(L)= ZoneTot(L) + Rimpact
                    StageTot(L)= StageTot(L)+ Rimpact
                    WholeTot(L)= WholeTot(L)+ Rimpact
 602              CONTINUE
                  WRITE(outs,603) ComAsPt(ICurComp,K),
     &                          (StageTot(L),IY=1,MIMP)
 603              FORMAT(1X,A12, 11X, 4(3x, 1P1E9.3))
                  IF (itrc.eq.3) CALL edisp(Ichanel,outs)
                endif
 601          CONTINUE

C 4 Composite transport to building site (Calculation + display)
            ELSEIF (M.EQ.4) THEN
              continue

C 5 assembly on the building (Calculation + display)
            ELSEIF (M.EQ.5) THEN
C First transport line display is different than others
              DO 620 K=1,MNbAss
                if (AsProjPt(ICurComp,K)(1:4).NE.'none') then
                  DO 621 L=1,MIMP
                    Rimpact=STEPIMP(ICurzone,ICurComp,-1,K,L,5)
                    LayerTot(L)=LayerTot(L) + Rimpact
                    CompoTot(L)= CompoTot(L)+ Rimpact
                    ZoneTot(L)= ZoneTot(L) + Rimpact
                    StageTot(L)= StageTot(L)+ Rimpact
                    WholeTot(L)= WholeTot(L)+ Rimpact
 621              CONTINUE
                  WRITE(outs,623)AsProjPt(ICurComp,K), 
     &                          (LCAValuP(IY)+LCAValuS(IY),IY=1,MIMP)
 623              FORMAT(1X,A12,12x, 4(3x, 1P1E9.3))
                  IF (itrc.eq.3) CALL edisp(Ichanel,outs)
                endif
 620          CONTINUE

C 6 Composite maintenance (calculation + display)
C Separation between Perimeter and Surfacic to allow maintenance differnce
C Important for instance for Window (Frame >< Glass)
            ELSEIF (M.EQ.6) THEN
C First transport line display is different than others
              DO 720 K=1,MNbM
                IF (MaintPt(ICurComp,K)(1:4).NE.'none') THEN
                  DO 721 L=1,MIMP
                    Rimpact=STEPIMP(ICurzone,ICurComp,-1,K,L,6)
                    LayerTot(L)=LayerTot(L) + Rimpact
                    CompoTot(L)= CompoTot(L)+ Rimpact
                    ZoneTot(L)= ZoneTot(L) + Rimpact
                    StageTot(L)= StageTot(L)+ Rimpact
                    WholeTot(L)= WholeTot(L)+ Rimpact
 721              CONTINUE
                  WRITE(outs,723)MaintPt(ICurComp,K),
     &                           PerMaint(ICurComp,K),
     &                           (LCAValuP(IY)+LCAValuS(IY),IY=1,MIMP)
 723              FORMAT(1X,A12,13x, 3x, f9.1, 4(3x, 1P1E9.3))
                  IF (itrc.eq.3) CALL edisp(Ichanel,outs)
                ENDIF
 720          CONTINUE

C Results related to whole composite
            ENDIF


C layers Transport 
C ----------------
C Result calculation
            IF ((M.EQ.2).OR.(M.EQ.4).OR.(M.EQ.8).OR.(M.EQ.10).OR.
     &          (M.EQ.12)) THEN
              DO 40 J=1,NbEIALay(ICurComp)
                ICurMat = IMatID(ICurComp,J)
                do 41 K=1,MNbTrans             
                  DO 42 L=1,MIMP
                    if (M.EQ.2) then
                      IF(TrPt(ICurMat,K)(1:4) .NE. 'none') THEN
                        LCAValue(L)=STEPIMP(ICurzone,ICurComp,J,K,L,2)
                      ELSE
                        LCAValue(L)= 0.
                      ENDIF
                    elseif (M.EQ.4) then
                      IF(CoTrPt(ICurComp,K)(1:4) .NE. 'none') THEN
                        LCAValue(L)= STEPIMP(ICurzone,ICurComp,J,K,L,4)
                      ELSE
                        LCAValue(L)= 0.
                      ENDIF
                    elseif (M.EQ.8) then
                      IF(DuTrPt(ICurMat,K)(1:4) .NE. 'none') THEN
                     LCAValue(L)=STEPIMP(ICurzone,ICurComp,J,K,L,8)
                      ELSE
                        LCAValue(L)= 0.
                      ENDIF
                    elseif (M.EQ.10) then
                      IF(BuTrPt(ICurMat,K)(1:4).NE. 'none') THEN
                        LCAValue(L)= STEPIMP(ICurzone,ICurComp,J,K,L,10)
                      ELSE
                        LCAValue(L)= 0.
                      ENDIF
                    elseif (M.EQ.12) then
                      IF(ReTrPt(ICurMat,K)(1:4).NE. 'none') THEN
                        LCAValue(L)= STEPIMP(ICurzone,ICurComp,J,K,L,12)
                      ELSE
                        LCAValue(L)= 0.
                      ENDIF
                    endif
                    LayerTot(L)=LayerTot(L) + LCAValue(L)
                    CompoTot(L)= CompoTot(L)+ LCAValue(L)
                    ZoneTot(L)= ZoneTot(L) + LCAValue(L)
                    StageTot(L)= StageTot(L)+ LCAValue(L)
                    WholeTot(L)= WholeTot(L)+ LCAValue(L)
 42               CONTINUE

C Display results
C ===============
C 2 Transport to construction site
                  IF (M.EQ.2) THEN
                    if (K.EQ.1) then
C First transport line display is different than others
                      WRITE(outs,312)LayerNam(ICurComp,J),
     &                               WeitLayr(J),TrPt(ICurMat,K),
     &                               INT(ZFun(M,ICurComp,J)),
     &                               TrDist(ICurMat,K),
     &                               (LCAValue(IY),IY=1,MIMP)
312                   FORMAT(1X,A12,3x,1PE9.3,3X,A12,3x,I2,3x,
     &                          0PF7.1,4(3x,1P1E9.3))
                      IF (itrc.eq.3) CALL edisp(Ichanel,outs)
                      ELSE
                        if (TrPt(ICurMat,K)(1:4).NE.'none') then
                          WRITE(outs,314) '+',TrPt(ICurMat,K),
     &                       INT(ZFun(M,ICurComp,J)),TrDist(ICurMat,K),
     &                       (LCAValue(IY),IY=1,MIMP)
314                       FORMAT(26x,A1,1x,A12,3x,I2,3x,F7.1,
     &                             4(3x,1P1E9.3))
                          IF (itrc.eq.3) CALL edisp(Ichanel,outs)
                        endif
                      ENDIF

C 4 Composite transport from assembly to building site.
C Relevant only for pre-fabricated element
                    elseif (M.EQ.4) then
                      IF (CoTrPt(ICurComp,1)(1:4).EQ.'none') THEN
                        if (itrc.ge.2) then
                          WRITE(outs,301)Sname(nznog(IZ),N),
     &                                   CompoNam(ICurComp),
     &              ' has no transport from assembly to building site.',
     &                 ' May be not a prefabricated element.'
 301                      FORMAT(1X,A12,3x,3A)
                          CALL edisp(Ichanel,outs)
                        endif
                        if ((IRep.eq.2).AND.(itrc.eq.3))then
                          WRITE(outs,505)
     &              'Total for composite = --------------------------',
     &                            (LCAValue(IY),IY=1,MIMP)
505                        FORMAT(2X,A, 4(3x, 1P1E9.3))
                           CALL edisp(Ichanel,outs)
                           CALL edisp(Ichanel,' ')
                         endif
                        GOTO 901
                      ENDIF 
                      IF (K .EQ. 1) THEN
C First transport line display is different than others
                        WRITE (outs,325) LayerNam(ICurComp,J),
     &                    WeitLayr(J),CoTrPt(ICurComp,K),
     &                   CoTrDist(ICurComp,K),(LCAValue(IY),IY=1,MIMP)
325                     FORMAT(1X,A12,3x,1PE9.3,3X,A12,3x,
     &                               0PF7.1, 4(3x,1P1E9.3))
                        IF (itrc.eq.3) CALL edisp(Ichanel,outs)
                      ELSE
                        if (CoTrPt(ICurMat,K)(1:4).NE.'none') then
                          WRITE (outs,326) '+', CoTrPt(ICurComp,K),
     &                     CoTrDist(ICurComp,K),(LCAValue(IY),IY=1,MIMP)
326                       FORMAT(26x,A1,1x,A12,3x,F7.1,4(3x,1P1E9.3))
                          IF (itrc.eq.3) CALL edisp(Ichanel,outs)
                        endif
                      ENDIF

C 8 Dump waste transport
                    elseif (M.EQ.8) then
                      IF (K .EQ. 1) THEN
C First transport line display is different than others
                        WRITE (outs,315) LayerNam(ICurComp,J),
     &                          WtLayrDu(J),DuTrPt(ICurMat,K),
     &                   DuTrDist(ICurMat,K),(LCAValue(IY),IY=1,MIMP)
315                     FORMAT(1X,A12,3x,1PE9.3,3X,A12,3x,
     &                               0PF7.1, 4(3x,1P1E9.3))
                        IF (itrc.eq.3) CALL edisp(Ichanel,outs)
                      ELSE
                        if (DuTrPt(ICurMat,K)(1:4).NE.'none') then
                          WRITE (outs,316) '+', DuTrPt(ICurMat,K),
     &                      DuTrDist(ICurMat,K),(LCAValue(IY),IY=1,MIMP)
316                       FORMAT(26x,A1,1x,A12,3x,F7.1,4(3x,1P1E9.3))
                          IF (itrc.eq.3) CALL edisp(Ichanel,outs)
                        endif
                      ENDIF

C 10 Incinarated waste transport
                    elseif (M .EQ. 10) then
                      IF (K .EQ. 1) THEN
C First transport line display is different than others
                        WRITE (outs,318 )LayerNam(ICurComp,J), 
     &                     WtLayrBu(J),BuTrPt(ICurMat,K),
     &                     BuTrDist(ICurMat,K),(LCAValue(IY),IY=1,MIMP)
318                     FORMAT(1X,A12,3x,1PE9.3,3X,A12,3x,
     &                               0PF7.1, 4(3x,1P1E9.3))
                        IF (itrc.eq.3) CALL edisp(Ichanel,outs)
                      ELSE
                        if (BuTrPt(ICurMat,K)(1:4).NE.'none') then
                          WRITE (outs,319) '+',BuTrPt(ICurMat,K),
     &                     BuTrDist(ICurMat,K),(LCAValue(IY),IY=1,MIMP)
319                       FORMAT(26x,A1,1x,A12,3x,F7.1,4(3x,1P1E9.3))
                          IF (itrc.eq.3) CALL edisp(Ichanel,outs)
                        endif
                      ENDIF


C 11 Transport to recycling site
                    elseif (M.EQ.12) then
                      IF (K.EQ.1) THEN
C First transport line display is different than others
                        WRITE (outs,321) LayerNam(ICurComp,J),
     &                         WtLayrRe(J),ReTrPt(ICurMat,K),
     &                     ReTrDist(ICurMat,K),(LCAValue(IY),IY=1,MIMP)
321                     FORMAT(1X,A12,3x,1PE9.3,3X,A12,3x,
     &                                            0PF7.1, 4(3x,1P1E9.3))
                        IF (itrc.eq.3) CALL edisp(Ichanel,outs)
                      ELSE
                        if (ReTrPt(ICurMat,K)(1:4).NE.'none') then
                          WRITE (outs,322) '+', ReTrPt(ICurMat,K),
     &                     ReTrDist(ICurMat,K),(LCAValue(IY),IY=1,MIMP)
322                       FORMAT(26x,A1,1x,A12,3x,F7.1,4(3x,1P1E9.3))
                          IF (itrc.eq.3) CALL edisp(Ichanel,outs)
                        endif
                      ENDIF
                    endif
C Next transport
 41               CONTINUE
C Next layer
 40             CONTINUE
              ENDIF

C Layer related
C -------------
              IF (M.EQ.1 .OR. M.EQ.7 .OR. M.EQ.9 .OR. M.EQ.11 .OR. 
     &              M.EQ.13 .OR. M.EQ.14) THEN
                DO 11 J=1,NbLays
                  ICurMat = IMatID(ICurComp,J)
                  if(closemat1.or.closemat2)then
                    matarrayindex=IPRMAT(ICurComp,J)   ! which arrat index
                    if(matarrayindex.ge.0)then
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht to mimic erpcdb
                      if(matarrayindex.eq.0)then
                        DBCON=0.0; DBDEN=0.0; DBSHT=0.0 
                        E=0.99; A=0.99; DRV=1.0
                        TITL='GAPS'; NAM='AIR'
                      else
                        DBCON=matdbcon(matarrayindex)
                        DBDEN=matdbden(matarrayindex)
                        DBSHT=matdbsht(matarrayindex)
                        E=matdbine(matarrayindex)
                        A=matdbina(matarrayindex)
                        DRV=matdbdrv(matarrayindex)
                        write(TITL,'(a)') 'placeholder'
                        write(NAM,'(a)') matname(matarrayindex)(1:32)
                      endif
                    endif
                  else
                    CALL ERPCDB(IFMAT,IPR(ICurComp,J),iuout,IER,
     &                DBCON,DBDEN,DBSHT,E,A,DRV,TITL,NAM)
                  endif

C Loop over the impacts
                  DO 12 L=1,MIMP
                    IF (M.EQ.1.OR.M.EQ.9.OR.M.EQ.11.OR.M.EQ.13)THEN
                      LCAValue(L) = STEPIMP(ICurzone,ICurComp,J,-1,L,M)
C Material replacement
                    ELSEIF (M.EQ.7) THEN
                      LCAValue(L)=STEPIMP(ICurzone,ICurComp,J,-1,L,7)
                      LCAValue(L)=STEPIMP(ICurzone,ICurComp,J,-1,L,7)
                    ENDIF
                    LayerTot(L)= LayerTot(L) + LCAValue(L)
                    CompoTot(L)= CompoTot(L) + LCAValue(L)
                    ZoneTot(L)= ZoneTot(L) + LCAValue(L)
                    StageTot(L)= StageTot(L) + LCAValue(L)
                    WholeTot(L)= WholeTot(L) + LCAValue(L)
 12               CONTINUE
C Material fabrication 
                  IF (M.EQ.1) THEN 
                    WRITE (outs,80)LayerNam(ICurComp,J),WeitLayr(J),
     &                               (LayerTot(IY),IY=1,MIMP)
 80                 FORMAT(1x,A12,13x, 3x, 1P1E9.3, 4(3x, 1P1E9.3))

C Material replacement 
                  ELSEIF (M.EQ.7) THEN
                    Rltmp = ProjLife/RMatLife(ICurMat)
                    if ((Rltmp - INT(Rltmp)).GT. 0.5) then
                      NbRep = INT(Rltmp)
                    else
                      NbRep = INT(Rltmp)-1
                    endif
                    IF (LayerCat(ICurComp,J).eq.'P') THEN
                      WRITE(outs,92)LayerNam(ICurMat,J),
     &                              RMatLife(ICurMat), NbRep,
     &                   (PerReImp(ICurComp,J,IY)*Perimeter,IY=1,MIMP)
                    ELSE
                      WRITE(outs,92)LayerNam(ICurComp,J),
     &                                RMatLife(ICurMat),NbRep,
     &                          (SurReImp(ICurComp,J,IY)*Area,IY=1,MIMP)
                    ENDIF
 92                 FORMAT(1X,A12,1x,f9.1, 5x, I2,4x, 4(3x, 1P1E9.3))

C 9 Dump treatment  
                  ELSEIF (M.EQ.9) THEN
                    WRITE(outs,109)LayerNam(ICurComp,J),
     &                             DumpRate(IMatID(ICurComp,J))*100,
     &                             WtLayrDu(J), (LayerTot(IY),IY=1,MIMP)
109                 FORMAT(1X,A12,4x,F5.1,5x,1P1E9.3,4(3x,1P1E9.3))

C 11 Incineration treatment  
                  ELSEIF (M.EQ.11) THEN
                    WRITE(outs,110)LayerNam(ICurComp,J),
     &                             BurnRate(IMatID(ICurComp,J))*100,
     &                             WtLayrBu(J),(LayerTot(IY),IY=1,MIMP)
110                 FORMAT(1X,A12,4x,F5.1,5x,1P1E9.3,4(3x,1P1E9.3))

C 13 Recycling treatment  
                  ELSEIF (M.EQ.13) THEN
                    WRITE(outs,166)LayerNam(ICurComp,J),
     &                             RecyRate(IMatID(ICurComp,J))*100,
     &                             WtLayrRe(J),(LayerTot(IY),IY=1,MIMP)
166                 FORMAT(1X,A12,4x,F5.1,5x,1P1E9.3,4(3x,1P1E9.3))

C 14 Total disposal waste  
                  ELSEIF (M.EQ.14) THEN
                    WRITE(outs,111) LayerNam(ICurComp,J),
     &                     WtLayrRe(J)+WtLayrBu(J)+WtLayrDu(J),
     &                     RecyRate(IMatID(ICurComp,J))*100,WtLayrRe(J),
     &                     BurnRate(IMatID(ICurComp,J))*100,WtLayrBu(J),
     &                     DumpRate(IMatID(ICurComp,J))*100,WtLayrDu(J)
  111               FORMAT(1X,A12,3x,1P1E9.3,3(4x,0PF6.1,4x,1P1E9.3))
C                    ELSEIF (M.EQ.14) THEN
C                      WRITE(outs,112)LayerNam(IFOC,J),WtLayrRe(J),
C     &                                     WtLayrBu(J),WtLayrDu(J)
C  112                 FORMAT(1X,A12,3(4x,1P1E9.3))
C End of Layer related stage
                    ENDIF

                    IF (itrc.eq.3) CALL edisp(Ichanel,outs)
                    DO 33 IN = 1,MIMP
                      LayerTot(IN)=0.
 33                 CONTINUE
                    DTHKTot = DTHKTot + DTHK(ICurComp,J)
C Next layer
 11               CONTINUE
                ENDIF

C Display Sub-total results
C--------------------------
                IF ((itrc.eq.1).or.(itrc.eq.2)) THEN
                  if (ICalType.eq.1) then
                    IF (M.EQ.14) THEN
                      TotMass= TotRecyM + TotBurnM + TotDumpM
                      WRITE(outs,58) SMLCN(N), TotMass,
     &                    TotRecyM/TotMass, TotRecyM,
     &                    TotBurnM/TotMass, TotBurnM,
     &                    TotDumpM/TotMass, TotDumpM
58                   FORMAT(1x,A,3x,1P1E9.3,5x,3(F6.2, 4x,1P1E9.3,3x))
                      TotDumpZ = TotDumpZ + TotDumpM
                      TotBurnZ = TotBurnZ + TotBurnM
                      TotRecyZ = TotRecyZ + TotRecyM
                      TotDumpM = 0.0
                      TotBurnM = 0.0
                      TotRecyM = 0.0
                    ELSE
                      WRITE(outs,581)SName(nznog(IZ),N),SMLCN(N),
     &                        CompoMass ,(CompoTot(IY),IY=1,MIMP)
581                   FORMAT(1X,A12, 3x, A12, 5(3x, 1P1E9.3))
                    ENDIF
                  elseif (ICalType.eq.2) then
                    WRITE(outs,582)SMLCN(N),
     &               CompoMass ,(CompoTot(IY),IY=1,MIMP)
582                 FORMAT(1X,12x, 3x, A12, 5(3x, 1P1E9.3))
                  endif
                ELSEIF (itrc.eq.3) THEN
                  IF (M.EQ.1) THEN
                    WRITE(outs,583)
     &                'Total for composite = ---',CompoMass,
     &                (CompoTot(IY),IY=1,MIMP)
583                 FORMAT(1X,A, 5(3x, 1P1E9.3))
                  ELSEIF (M.EQ.2) THEN
                    WRITE(outs,584) 'Total for composite = ----',
     &                              (CompoTot(IY),IY=1,MIMP)
584                 FORMAT(1X,28x,A,4(3x, 1P1E9.3))
                  ELSEIF (M.EQ.3 .OR. M.EQ.5) THEN
C Check mise en page
                    WRITE(outs,83)'Total for composite =',
     &                            (CompoTot(IY),IY=1,MIMP)
 83                 FORMAT(3X,A, 4(3x, 1P1E9.3))

C 4 Composite Transport
                  ELSEIF (M.EQ.4) THEN
                    WRITE(outs,585)
     &              'Total for composite = --------------------------',
     &                            (CompoTot(IY),IY=1,MIMP)
585                 FORMAT(2X,A, 4(3x, 1P1E9.3))
                  ELSEIF (M.EQ.6) THEN
                   WRITE(outs,600)'Total for composite = -------------',
     &                             (CompoTot(IY),IY=1,MIMP)
600                 FORMAT(3X,A, 4(3x, 1P1E9.3))
                  ELSEIF (M.EQ.7) THEN
                    WRITE(outs,599)'Total for composite = -----------', 
     &                             (CompoTot(IY),IY=1,MIMP)
599                 FORMAT(1X,A, 4(3x, 1P1E9.3))

C 9 Dumped waste treatment 
                  ELSEIF (M.EQ.9) THEN
                    WRITE(outs,586)
     &               ' Total for composite = --- ', TotDumpM,
     &                                          (CompoTot(IY),IY=1,MIMP)
586                 FORMAT(A, 5(1P1E9.3,3x))
C 11 Incinarated waste treatment
                  ELSEIF (M.EQ.11) THEN
                    WRITE(outs,587)
     &               ' Total for composite = --- ', TotBurnM,
     &                                          (CompoTot(IY),IY=1,MIMP)
587                 FORMAT(A, 5(1P1E9.3,3x))
C 13 Recycled waste treatment
                  ELSEIF (M.EQ.13) THEN
                    WRITE(outs,508)
     &               ' Total for composite = --- ', TotRecyM,
     &                                          (CompoTot(IY),IY=1,MIMP)
508                 FORMAT(A, 5(1P1E9.3,3x))
C 8 Dumped waste transportation & 10 Incinerated waste transportation
C & 12 Recycling transport 
                  ELSEIF (M.EQ.8 .OR. M.EQ.10 .OR. M.EQ.12) THEN
                    WRITE(outs,588)
     &            'Total for composite = ----------------------------', 
     &                             (CompoTot(IY),IY=1,MIMP)
588                 FORMAT(A, 4(3x, 1P1E9.3))
C 14 Disposal mass
                  ELSEIF (M.EQ.14) THEN
                    WRITE(outs,589) 'Total =', 
     &                              TotRecyM + TotBurnM + TotDumpM,
     &                              TotRecyM, TotBurnM, TotDumpM
589                 FORMAT(3x,A,6x,4(1P1E9.3,14x))
                    TotDumpZ = TotDumpZ+TotDumpM
                    TotBurnZ = TotBurnZ+TotBurnM
                    TotRecyZ = TotRecyZ+TotRecyM
                    TotDumpM = 0.0
                    TotBurnM = 0.0
                    TotRecyM = 0.0
                  ENDIF
                ENDIF
                IF (itrc.EQ.2 .OR. itrc.EQ.3) CALL edisp(Ichanel,outs)
                IF (itrc.EQ.3) CALL edisp(Ichanel,' ')
                DO 35 IN = 1,MIMP
                  CompoTot(IN)=0.
 35             CONTINUE
                TotMass = TotMass + CompoMass
C Next Surface 
901           CONTINUE
 
              IF (ICalType.eq.1) THEN
                if (itrc.eq.1) then
                  IF (M.eq.14) THEN
                    WRITE(outs,818) zname(nznog(IZ)),
     &                           TotRecyZ + TotBurnZ + TotDumpZ,
     &                           TotRecyZ, TotBurnZ, TotDumpZ
 818                FORMAT(1x,A12,10x,4(1P1E9.3,6x))
                  ELSE
                    WRITE (outs,812) zname(nznog(IZ)),
     &                        (ZoneTot(IY),IY=1,MIMP)
 812                FORMAT(1X,A18, 4(3x, 1P1E9.3))
                  ENDIF
                elseif (itrc.eq.2) then
                  IF (M.eq.14) THEN
                    WRITE(outs,819) 'Total zone = ',
     &                           TotRecyZ + TotBurnZ + TotDumpZ,
     &                           TotRecyZ, TotBurnZ, TotDumpZ
 819                FORMAT(1x,A,2x,4(1P1E9.3,14x))
                  ELSE
                    WRITE (outs,813)' Total for zone = ',TotMass,
     &                        (ZoneTot(IY),IY=1,MIMP)
 813                FORMAT(10X,A18, 5(3x, 1P1E9.3))
                  ENDIF
                else
                  IF (M.eq.14) THEN
                    WRITE(outs,820)'Total zone = ',
     &                          TotRecyZ +TotBurnZ + TotDumpZ,
     &                          TotRecyZ, TotBurnZ, TotDumpZ
 820                 FORMAT(1x,A,2x,4(1P1E9.3,14x))
                  ELSE                
                    WRITE (outs,84) zname(nznog(IZ)),
     &                           (ZoneTot(IY),IY=1,MIMP)
 84                 FORMAT(2X,A12,5x, 4(3x, 1P1E9.3))
                  ENDIF
                endif
              ELSEIF (ICalType.eq.2) THEN
                if ((itrc.eq.1).and.(ICalType.eq.2).and.(M.eq.14)) then
                  WRITE(outs,88)CompoNam(ICurComp),TotRecyM+TotBurnM+
     &                          TotDumpM,TotRecyM, TotBurnM, TotDumpM
 88               FORMAT(16x,A12,3x,4(1P1E9.3,8x))
                else
                 IF (ZoneTot(1).GT.0) THEN
                   IF (M.eq.1) THEN
                     TotMasDisp = TotMass
                   ELSEIF (M.eq.2) THEN
                     TotMasDisp = TotMass
                   ELSEIF ((M.eq.3).OR.(M.eq.4).OR.(M.eq.5)) THEN
                     TotMasDisp = CompSMS(ICurComp)+CompSMP(ICurComp)
                   ELSEIF (M.eq.7) THEN
                     TotMasDisp = 0.0
                     DO 676 II=1,NbEIALay(ICurComp)
                       TotMasDisp = TotMasDisp + 
     &                 PerReMas(ICurComp,II)+SurReMas(ICurComp,II)
676                  CONTINUE
                   ELSEIF ((M.eq.8).OR.(M.eq.9)) THEN
                       TotMasDisp = TotDumpM
                   ELSEIF ((M.eq.10).OR.(M.eq.11)) THEN
                       TotMasDisp = TotBurnM
                   ELSEIF ((M.eq.12).OR.(M.eq.13)) THEN
                       TotMasDisp = TotRecyM
                   ENDIF
                 ELSE
                   TotMasDisp = 0.0
                 ENDIF
                   IF (M.eq.6) THEN
                     WRITE (outs,685)CompoNam(ICurComp) , '-',
     &                        (ZoneTot(IY),IY=1,MIMP)
 685                  FORMAT(1X, A27,A12, 4(3x, 1P1E9.3))
                   ELSE                  
                     WRITE (outs,85)CompoNam(ICurComp) ,TotMasDisp,
     &                        (ZoneTot(IY),IY=1,MIMP)
 85                  FORMAT(1X, A27, 5(3x, 1P1E9.3))
                  ENDIF
                endif
              ENDIF
              IF (itrc.EQ.1 .OR. itrc.EQ.2) CALL edisp(Ichanel,outs)
              IF (itrc.EQ.2) CALL edisp(Ichanel,' ')

              DO 37 IN = 1,MIMP
                ZoneTot(IN)=0.
 37           CONTINUE
              TotDumpS = TotDumpS+TotDumpZ
              TotBurnS = TotBurnS+TotBurnZ
              TotRecyS = TotRecyS+TotRecyZ

            IF (itrc.GE.2) THEN
C              CALL edisp(Ichanel,'-------------------')
              CALL edisp(Ichanel,' ')
            ENDIF
C Next zone
997         CONTINUE



            IF (itrc.eq.0) THEN
              WRITE (outs,86)StepName(M),(StageTot(IY),IY=1,MIMP)
 86           FORMAT(1X,A32,4(3x, 1P1E9.3))
            ELSEIF (itrc.eq.1) THEN
            ENDIF
            if (ICalType.eq.1) then
              IF (M.eq.14) THEN
                if (itrc.eq.1) THEN
                  WRITE(outs,190)'Total disposal',
     &                          TotRecyS +TotBurnS + TotDumpS,
     &                          TotRecyS, TotBurnS, TotDumpS
 190              FORMAT(A15,8x,4(1P1E9.3,6x))
                elseif (itrc.eq.2) then
                  WRITE(outs,191)'Total disposal',
     &                          TotRecyS +TotBurnS + TotDumpS,
     &                          TotRecyS, TotBurnS, TotDumpS
 191              FORMAT(A15,1x,4(1P1E9.3,14x))
                endif
              ELSE                
                if (Itrc.eq.1) then        
                  WRITE (outs,184)' Total for stage = ', 
     &                                          (StageTot(IY),IY=1,MIMP)
184               FORMAT(A,4(3x, 1P1E9.3))
                elseif (Itrc.eq.2) then
                  IF ((M.EQ.1).or.(M.EQ.2)) THEN
                   WRITE (outs,185)' Total for stage = ---------------', 
     &                                          (StageTot(IY),IY=1,MIMP)
 185                FORMAT(4X,A,2x,4(3x, 1P1E9.3))
                  ELSE
                   WRITE (outs,176)' Total for stage = ---------------', 
     &                                          (StageTot(IY),IY=1,MIMP)
 176                FORMAT(4X,A,2x,4(3x, 1P1E9.3))
                  ENDIF
                elseif (Itrc.le.3) then
C Ckeck mise en page for itrc 3
                  IF (M.EQ.1) THEN
                    WRITE(outs,19)
     &                         ' Total for stage = -------------------', 
     &                                          (StageTot(IY),IY=1,MIMP)
 19                 FORMAT(A,4(3x, 1P1E9.3))
                  ELSEIF (M.EQ.2)  THEN
                    WRITE(outs,18)
     &        ' Total for stage = ------------------------------------', 
     &                                          (StageTot(IY),IY=1,MIMP)
 18                 FORMAT(A,4(3x, 1P1E9.3))
                  ELSEIF ((M.EQ.3).OR.(M.EQ.5)) THEN
                    WRITE(outs,180)' Total for stage = ------- ', 
     &                                          (StageTot(IY),IY=1,MIMP)
 180                 FORMAT(A,4(1P1E9.3, 3x))
                  ELSEIF (M.EQ.4)  THEN
                    WRITE(outs,181)
     &             ' Total for stage = -------------------------------', 
     &                                          (StageTot(IY),IY=1,MIMP)
 181                FORMAT(A,4(3x, 1P1E9.3))
                  ELSEIF (M.EQ.6) THEN
                    WRITE(outs,182)
     &             ' Total for stage = -------------------', 
     &                                          (StageTot(IY),IY=1,MIMP)
 182                FORMAT(A,4(3x, 1P1E9.3))
                  ELSEIF (M.EQ.7) THEN
                    WRITE(outs,183)' Total for stage = ---------------', 
     &                                          (StageTot(IY),IY=1,MIMP)
 183                FORMAT(A,4(3x, 1P1E9.3))
                  ELSEIF ((M.EQ.8).or.(M.EQ.10).or.(M.EQ.12)) THEN
                    WRITE(outs,135)
     &             ' Total for stage = -------------------------------', 
     &                                          (StageTot(IY),IY=1,MIMP)
 135                FORMAT(A,4(3x, 1P1E9.3))
                  ELSEIF ((M.EQ.9).or.(M.EQ.11).or.(M.EQ.13)) THEN
                    WRITE(outs,136)
     &                 ' Total for stage = ------------------- ',
     &                                          (StageTot(IY),IY=1,MIMP)
 136                FORMAT(A,4(1P1E9.3,3x))
                  ENDIF
                endif
              ENDIF
              CALL edisp(Ichanel,outs)
              CALL edisp(Ichanel,' ')
            elseif (ICalType.eq.3) then
              IF (itrc.EQ.0 .OR. itrc.EQ.1) CALL edisp(Ichanel,outs)
            endif
            IF (itrc.EQ.1) CALL edisp(Ichanel,' ')
            DO 39 IN = 1,MIMP
              ProjTot(IN) = ProjTot(IN)+StageTot(IN) 
              StageTot(IN)=0.
 39         CONTINUE
          ENDIF
C Next Stage
999     CONTINUE




        IF ((itrc.le.2).and.(iwb.ne.1))THEN
          if (itrc.eq.1)THEN
            IF(IRep.EQ.2) then
              WRITE (outs,87)'Total for all stages=',
     &                      (ProjTot(IY),IY=1,MIMP)
 87           FORMAT(A,1x,4(1P1E9.3,3x))
            ELSE
              WRITE (outs,89)' Total for all stages = ',
     &                      (ProjTot(IY),IY=1,MIMP)
 89           FORMAT(6X,A,1x,4(1P1E9.3,3x))
            ENDIF
          elseif (itrc.eq.2)THEN
            IF (IWB.EQ.2) THEN        
              IF (IW.EQ.1) THEN        
                TmpString=  'Total for CONSTRUCTION = --------------'
              ELSEIF (IW.EQ.2) THEN
                TmpString=  'Total for UTILISATION = --------------'
              ELSEIF (IW.EQ.3) THEN
                TmpString=  'Total for ELIMINATION = --------------'
              ENDIF
            ELSEIF (IWB.EQ.3) THEN        
              TmpString=  'Total for WHOLE CYCLE = --------------'
            ENDIF
            WRITE (outs,90) TmpString,
     &                      (ProjTot(IY),IY=1,MIMP)
 90         FORMAT(A40,3x,4(1P1E9.3,3x))
          endif
          CALL edisp(Ichanel,outs)
          DO 888 IN = 1,MIMP
            WholeTot(IN)=0.
            ProjTot(IN)=0.
888        CONTINUE
        ELSE
          if(iwb.ne.1)then
            WRITE (outs,889)' Total for stages = ',
     &                      (ProjTot(IY),IY=1,MIMP)
889         FORMAT(10X,A,7x,4(1P1E9.3,3x))
            CALL edisp(Ichanel,outs)
          endif
        ENDIF
      GOTO 3

666   RETURN
      END




C ******* surfprm
C Surfprm returns the number of edges and total length of
C the perimiter of the polygon. Assumes that the surface is
C in the current zones common blocks.
      subroutine surfprm(izone,isurf,dupedges,perim)
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      DIMENSION x1(3),x2(3)
      logical dupedges

      perim=0.0
      dupedges=.false.
      if(izone.ne.0.and.izone.le.NCOMP)then
        if(isurf.le.NZSUR(izone).and.isurf.ne.0)then

C First confirm whether there are any duplicate references to vertices
c (indicates a polygon with a hole in it).
          do 40 m=1,NVER(isurf)
            do 41 n=1,NVER(isurf)
              IF(m.EQ.n)goto 41
              J1=JVN(isurf,n)
              J2=JVN(isurf,m)
              IF(J1.EQ.J2)dupedges=.true.
  41        continue
  40      continue

C Step through the vertices of each edge in turn and find distance.
          list=NVER(isurf)-1
          do 42 i=1,list
            j=JVN(isurf,i)
            k=JVN(isurf,i+1)
            x1(1)=X(j)
            x1(2)=Y(j)
            x1(3)=Z(j)
            x2(1)=X(k)
            x2(2)=Y(k)
            x2(3)=Z(k)
            vdos=0.0
            vdis= crow(x1,x2)
            perim=perim+vdis
  42      continue

C Link back to start vertex
          j=JVN(isurf,NVER(isurf))
          k=JVN(isurf,1)
          x1(1)=X(j)
          x1(2)=Y(j)
          x1(3)=Z(j)
          x2(1)=X(k)
          x2(2)=Y(k)
          x2(3)=Z(k)
          vdos=0.0
          vdis= crow(x1,x2)
          perim=perim+vdis
        endif
      endif
      return
      end

