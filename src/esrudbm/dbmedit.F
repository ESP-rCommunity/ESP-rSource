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

C Subroutines for managing items and categories within generic
C databases and create minimal new databases.
C   EDSELCAT:Given a generic db, provides a list of classifications.
C   EDSELITEM: Given a generic db, provides a list of items in category ic.
C   INTDBDOC: Initial db documentation text is instanciated based on act and
C     on the type of database.
C   EDCATDOC: Edit category level documentation (act = c).
C   newgcat: Depending on the db type create a new class and place in it one
C     item of the current database type.
C   newgitem: Depending on the db type create a new instance of an item for
C     subsequent editing.
C   instanitm: Instanciate common blocks gendb8b and gendb8c for a new item 
C     (index iit) for a given database type (db).
C   AGDBNEW: Create a minimal boilerplate ascii generic database which can be read
C     in and converted to a new binary db file.

C << note nbitusc might not be being recovered properly in
C << some places in dbm - shows up as incorrect numbers in
C << the ascii version of file.

C ************* EDSELCAT
C EDSELCAT:Given a generic db, provides a list of classifications.
C If ACTION = 'M' then include editing if ACTION = '-' only
C allow choice.  Chgdb flagged true if an mod made to db during session.
C Itwhich is the index of the item selected from the database and
C Icwhich is its category. Idbh is the position in the hash table.
      subroutine edselcat(IFB,idbh,itwhich,icwhich,action,IER)
#include "epara.h"
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit

C Generic db commons: file log, list of db, current classifications,
C and current items then lengths of each list.
C      common/gendb/LGENDB
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendbu/unitchr(24),unitstr(24),unitnote(24)
      common/gendbs/srcchr(24),srcnote(24)

C Backup of documentation (for editing).
      common/tmpdoca/bkdoctyp,bkdoc(60),bk4(24),bk12(24),bk72(24),
     &  bk248(24)
      common/tmpdocb/bkdocok,nbkdoc,indexbk

      logical bkdocok,primod,ok,dprimod,dok
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character dbnote*72,outs*124
      DIMENSION CLDES(35),ICATSEL(MGCL),catlist(MGCL)

      character CLDES*38,action*1,KEY*1,tab*1,catlist*32
      character head*37,h*72,t72*72,t12*12,c8*8,t248*248
      CHARACTER xfile*72,tg*1,delim*1
      character bkdoctyp*2,bkdoc*72,bk4*4,bk12*12,bk72*72,bk248*248
      character unitchr*4,unitstr*12,unitnote*72
      character srcchr*4,srcnote*248,LGENDB*72,dstmp*24,bl2*2

C Read the header.
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)
      if(ier.ne.0)then
        call edisp(iuout,'Problem scanning the header of ')
        call edisp(iuout,gendbfn(idbh))
        itwhich=0
        icwhich=0
        return
      endif

C Compose a local category selection list.
      nbc=nbcat(idbh)
      do 11 ij=1,nbc
        catlist(ij)=catmenu(ij)
  11  continue

      tab=','
      t12=' '
      t72=' '
      bl2='  '
      IAFIL=IFIL+1

C Setup for menu.
C Gather the names of the various classes and present this list.
  39  MHEAD=6
      if(ACTION.eq.'M'.or.ACTION.eq.'m')then
        MCTL=5
      elseif(ACTION.eq.'-')then
        MCTL=4
      endif
      ILEN=nbcat(idbh)
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
  40  ICO=-4
      if(ACTION.eq.'M'.or.ACTION.eq.'m')then
        WRITE(CLDES(1),'(A,A)')   '1 Note: ',dbnote(idbh)(1:30)
        WRITE(CLDES(2),'(a,i1,a)')'2 Units (',nbunit(idbh),')'
        WRITE(CLDES(3),'(a,i1,a)')'3 Sources (',nbsrc(idbh),')'
        WRITE(CLDES(4),'(a)')     '4 Uncert ( )'
      else
        WRITE(CLDES(1),'(A,A)')   '  Note: ',dbnote(idbh)(1:30)
        WRITE(CLDES(2),'(a,i1,a)')'  Units (',nbunit(idbh),')'
        WRITE(CLDES(3),'(a,i1,a)')'  Sources (',nbsrc(idbh),')'
        WRITE(CLDES(4),'(a)')     '  Uncert ( )'
      endif
      CLDES(5)=  '3 Documentation                      '
      CLDES(6)=  '  Catgory description       No. Items'
      M=MHEAD

C Build up a display.
      do 34 i=1,ILEN
        if(i.GE.IST.AND.(i.LE.(IST+MIFULL)))then
          M=M+1
          CALL EMKEY(M-MHEAD,KEY,IER)
          WRITE(CLDES(M),'(A1,1X,A,2X,I2)')KEY,catmenu(i)(1:32),
     &      nbcatit(i)
        endif
  34  continue

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
         CLDES(M+1)='  __________________________  '
      ELSE
        WRITE(CLDES(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page ---- Part: ',I2,' of ',I2,' ---')
      ENDIF
      if(ACTION.eq.'M'.or.ACTION.eq.'m')then
        CLDES(M+2)='* Modify catagory list        '
        CLDES(M+3)='! list database entries       '
        CLDES(M+4)='? Help                        '
        CLDES(M+5)='- Exit                        '
      elseif(ACTION.eq.'-')then
        CLDES(M+2)='! list database entries       '
        CLDES(M+3)='? Help                        '
        CLDES(M+4)='- Exit                        '
      endif
      NCO=M+MCTL

C Help text for this menu.
      if (idbh.eq.5) then
        H(1)='This menu allows to browse/select the category of'
        H(2)='materials/systems, in order to display the'
        H(3)='corresponding acoustic absorption coefficients,'
        H(4)='which is used for room acoustic calculation.'
      elseif (idbh.eq.6) then
        H(1)='This menu allows to browse/select a colour category,'
        H(2)='in order to display the corresponding colorymertry'
        H(3)='information used for lighting calculation.'
      elseif (idbh.eq.8) then
        H(1)='This menu allows to browse/select the environmental'
        H(2)='impacts of a phase during the building life span.'
        H(3)='These impacts are used for the Life Cycle Impact'
        H(4)='assessment (LCIA) of the project construction.'
      elseif (idbh.eq.9) then
        H(1)='This menu allows to browse/select a category of'
        H(2)='transport, in order to display the corresponding'
        H(3)='environmental impacts.These impacts are used for'
        H(4)='the Life Cycle Impact assessment (LCIA) of the '
        H(5)='project construction. '
      endif

      write(head,'(a,a)') ' Db: ',dbmenu(idbh)

C Now display the menu.
      CALL EMENU(head,CLDES,NCO,ICO)
      IF(ICO.EQ.NCO)THEN
        goto 99
C Explains main menu.
      ELSEIF(ICO.EQ.NCO-1)THEN
        if (idbh.eq.5) then
          CALL PHELPD('Category menu',4,'-',0,0,IER)
        elseif (idbh.eq.6) then
          CALL PHELPD('Category menu',3,'-',0,0,IER)
        elseif (idbh.eq.8) then
          CALL PHELPD('Category menu',4,'-',0,0,IER)
        elseif (idbh.eq.9) then
          CALL PHELPD('Category menu',5,'-',0,0,IER)
        endif
      ELSEIF(ICO.EQ.1)THEN

C Display/edit the db note:
        if(ACTION.eq.'M'.or.ACTION.eq.'m')then
 333      call edisp(iuout,'The aide memoire for this db is:')
          call edisp(iuout,dbnote(idbh))
          H(1)='An aide memoire for this instance of the database'
          H(2)='(separate from the database documentation).'
          t72=dbnote(idbh)
          CALL EASKS(t72,'Aide memoire',' ',72,'This instance... ',
     &      'db note',IER,2)
          if(ier.ne.0)goto 333
          if(dbnote(idbh)(1:lnblnk(dbnote(idbh))).ne.
     &       t72(1:lnblnk(t72)))then
            dbnote(idbh)=t72
            call dstamp(dstmp)
            write(dbdate(idbh),'(a)') dstmp
            IREC=2
            WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)dbdate(idbh),
     &        dbnote(idbh)
            call tagend(IFB,idbh,ier)
            call usrmsg('Updated database. ',bl2,'P')
            bkdocok=.FALSE.
            goto 39
          endif
        else
          call edisp(iuout,'The aide memoire for this db is: ')
          call edisp(iuout,dbnote(idbh))
        endif
      ELSEIF(ICO.EQ.2)THEN

C Display/edit the units:
        write(outs,'(2a)')'Units associated with ',dbmenu(idbh)
        call edisp(iuout,outs)
        call edisp(iuout,' char   tag  description')
        do 4 inbu=1,nbunit(idbh)
          write(outs,'(6a)') ' ',unitchr(inbu)(1:1),tab,
     &      unitstr(inbu),tab,unitnote(inbu)(1:lnblnk(unitnote(inbu)))
          call edisp(iuout,outs)
  4     continue
        if(ACTION.eq.'M'.or.ACTION.eq.'m')then
          H(1)='Documentation of the database is encouraged. You '
          H(2)='can enter information on units used in the database'
          H(3)='in the form of a 12 characterstring to be displayed '
          H(4)='and a 72 character explanation as in:'
          H(5)=' '
          H(6)='  a    meters/sec    wind velocity from logger'
          H(7)='  b    degrees       wind direction 0=north'
          dprimod=.false.
          CALL ASKOK(' ','Edit units ?',primod,dprimod,7)
          if(primod)then
            bkdocok=.TRUE.
            bkdoctyp='UN'
            indexbk=0
            nbkdoc=nbunit(idbh)
            do 24 ik=1,nbunit(idbh)
              write(bk12(ik),'(a)')unitstr(ik)
              write(t12,'(a)')unitstr(ik)
              write(outs,'(3a)')' Unit ',unitchr(ik),' display string:'
              CALL EASKS(t12,outs,' ',12,'none ','UN',IER,7)
              if(ier.eq.0)bk12(ik)=t12

              write(bk72(ik),'(a)')unitnote(ik)
              write(t72,'(a)')unitnote(ik)
              write(outs,'(3a)')' Unit ',unitchr(ik),' explanation:'
              CALL EASKS(t72,outs,' ',72,'none ','UN',IER,7)
              if(ier.eq.0)bk72(ik)=t72
  24        continue

C If more units possible check to see if more are to be added.
 154        if(nbkdoc.le.12)then
              h(1)='Additional unit descriptors can be added as'
              h(1)='required. '
              dok=.false.
              CALL ASKOK(' ','Add another unit idbh',ok,dok,7)
              if(OK)then
                nbkdoc=nbkdoc+1
                write(bk12(nbkdoc),'(a)')unitstr(nbkdoc)
                write(t12,'(a)')unitstr(nbkdoc)
                write(outs,'(3a)')' Unit ',unitchr(nbkdoc),
     &            ' display string:'
                CALL EASKS(t12,outs,' ',12,'none ','UN',IER,7)
                if(ier.eq.0)bk12(nbkdoc)=t12

                write(bk72(nbkdoc),'(a)')unitnote(nbkdoc)
                write(t72,'(a)')unitnote(nbkdoc)
                write(outs,'(3a)')' Unit ',unitchr(nbkdoc),
     &            ' explanation:'
                CALL EASKS(t72,outs,' ',72,'none ','UN',IER,7)
                if(ier.eq.0)bk72(nbkdoc)=t72
                goto 154
              endif
            endif
            call usrmsg('Updating database units...',bl2,'-')
            call dstamp(dstmp)
            write(dbdate(idbh),'(a)') dstmp
            IREC=2
            WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)dbdate(idbh),
     &        dbnote(idbh)

            if(nbkdoc.eq.nbunit(idbh))then

C If no additional units added update the db.
              do 25 ik=1,nbunit(idbh)
                write(unitstr(ik),'(a)')bk12(ik)
                write(unitnote(ik),'(a)')bk72(ik)
                IREC = 2 + ik
                c8='*UNIT   '
                WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,unitchr(ik),
     &            unitstr(ik),unitnote(ik)
  25          continue
              call tagend(IFB,idbh,ier)
              call usrmsg('Updating database units...done.',bl2,'-')
              bkdocok=.FALSE.
              goto 39
            else

C Reconfigure the database by exporting to ascii file and then re-reading.
              call tagend(IFB,idbh,ier)
              LGENDB=gendbfn(idbh)
              call recongdb(IAFIL,IFB,LGENDB,idbh,IER)
              if(ier.ne.0)then
                call edisp(iuout,'Error reconfiguring db.')
              endif
              call usrmsg('Updating database units...done.',bl2,'-')
              bkdocok=.FALSE.
              goto 39
            endif
          endif
        endif
      ELSEIF(ICO.EQ.3)THEN
        call edisp(iuout,' ')
        call edisp(iuout,'Sources associated with this database:')
        call edisp(iuout,' char   description')
        do 5 inbsrc=1,nbsrc(idbh)
          write(outs,'(2a)')' ',srcchr(inbsrc)(1:1)
          call edisp(iuout,outs)
          call edisp248(iuout,srcnote(inbsrc),80)
  5     continue
        if(ACTION.eq.'M'.or.ACTION.eq.'m')then
          H(1)='Documentation of the database is encouraged. You '
          H(2)='can enter information on sources used in the database'
          H(3)='in the form of a 248 character description'
          H(4)=' '
          H(5)='  a    AIVC document 13.5 table 3'
          dprimod=.false.
          CALL ASKOK(' ','Edit sources ?',primod,dprimod,5)
          if(primod)then

C Edit the existing sources.
            bkdocok=.TRUE.
            bkdoctyp='SR'
            indexbk=0
            nbkdoc=nbsrc(idbh)
            do 55 inbsrc=1,nbsrc(idbh)
              write(bk248(inbsrc),'(a)')
     &          srcnote(inbsrc)(1:lnblnk(srcnote(inbsrc)))
              t248=' '
              write(t248,'(a)')
     &          srcnote(inbsrc)(1:lnblnk(srcnote(inbsrc)))
              write(outs,'(2a)')' Source documentation for ',
     &          srcchr(inbsrc)(1:1)
              CALL EASKS248(t248,outs,': ',72,': ','srcnote',IER,5)
              if(ier.eq.0)then
                bk248(inbsrc)=t248
                write(srcnote(inbsrc),'(a)')t248(1:lnblnk(t248))
              endif
  55        continue

C Offer to add more sources.
 155        if(nbkdoc.le.24)then
              h(1)='Additional sources can be added as'
              h(2)='required. '
              dok=.false.
              CALL ASKOK(' ','Add another source',ok,dok,2)
              if(OK)then
                nbkdoc=nbkdoc+1
                write(bk248(nbkdoc),'(a)')
     &            srcnote(nbkdoc)(1:lnblnk(srcnote(nbkdoc)))
                t248=' '
                write(t248,'(a)')
     &            srcnote(nbkdoc)(1:lnblnk(srcnote(nbkdoc)))
                write(outs,'(2a)')' Source documentation for ',
     &            srcchr(nbkdoc)(1:1)
                CALL EASKS248(t248,outs,': ',72,': ','srcnote',IER,2)
                if(ier.eq.0)then
                  bk248(nbkdoc)=t248
                  write(srcnote(nbkdoc),'(a)')t248(1:lnblnk(t248))
                endif
                goto 155
              endif
            endif
            call usrmsg('Updating database sources...',bl2,'-')
            call dstamp(dstmp)
            write(dbdate(idbh),'(a)') dstmp
            IREC=2
            WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)dbdate(idbh),
     &        dbnote(idbh)

            if(nbkdoc.eq.nbsrc(idbh))then

C If no additional sources added update the db.
              irec = 2 + nbunit(idbh)
              do 225 ik=1,nbsrc(idbh)

C write srcnote into next record.
                c8='*SOURCE '
                IREC=IREC+1
                WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,srcchr(ik),
     &            srcnote(ik)
  225         continue
              call tagend(IFB,idbh,ier)
              call usrmsg('Updating database sources...done.',bl2,'-')
              bkdocok=.FALSE.
              goto 39
            else

C Reconfigure the database by exporting to ascii file and then re-reading.
              call tagend(IFB,idbh,ier)
              LGENDB=gendbfn(idbh)
              call recongdb(IAFIL,IFB,LGENDB,idbh,IER)
              if(ier.ne.0)then
                call edisp(iuout,'Error reconfiguring db.')
              endif
              call usrmsg('Updating database sources...done.',bl2,'-')
              bkdocok=.FALSE.
              goto 39
            endif
          endif
        endif
      ELSEIF(ICO.EQ.4)THEN
        call edisp(iuout,'uncdrtainty tbd...')
      ELSEIF(ICO.EQ.5)THEN

C Db documentation.
        primod=.FALSE.
        do 151 i=1,nbdbdoc(idbh)
          call edisp248(iuout,dbdoc(i),80)
 151    continue
        if(ACTION.eq.'M'.or.ACTION.eq.'m')then
          H(1)='Documentation of the database is encouraged. You '
          H(2)='can enter one or several lines of text.'
          H(3)='Retain the `: ` in the first two columns as it is '
          H(4)='used within the db and not seen by the user.'
          dprimod=.false.
          CALL ASKOK(' ','Edit database documentation?',primod,
     &      dprimod,4)
          if(primod)then
            bkdocok=.TRUE.
            bkdoctyp='DD'
            indexbk=0
            nbkdoc=nbdbdoc(idbh)
            do 152 ik=1,nbkdoc
              write(bk248(ik),'(a)')dbdoc(ik)(1:lnblnk(dbdoc(ik)))
              t248=' '
              write(t248,'(a)')dbdoc(ik)(1:lnblnk(dbdoc(ik)))
              write(outs,'(a,I2)')' Documentation line ',ik
              CALL EASKS248(t248,outs,' ',72,': ','dd',IER,4)
              if(ier.eq.0)bk248(ik)=t248
 152        continue
 153        dok=.false.
            CALL ASKOK(' ','Add another line ?',ok,dok,4)
            if(OK)then
              nbkdoc=nbkdoc+1
              t248=' '
              write(outs,'(a,I2)')' Documentation line ',nbkdoc
              write(bk248(nbkdoc),'(a)')': '
              write(t248,'(a)')': '
              CALL EASKS248(t248,outs,' ',72,': ','dd',IER,4)
              if(ier.eq.0)bk248(nbkdoc)=t248
              goto 153
            endif

            call usrmsg('Updating database...',bl2,'-')
            call dstamp(dstmp)
            write(dbdate(idbh),'(a)') dstmp
            IREC=2
            WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)dbdate(idbh),
     &        dbnote(idbh)
            if(nbkdoc.eq.nbdbdoc(idbh))then

C No change in length so update the database and commons by writing updated text.
              IREC = 2+nbunit(idbh)+nbsrc(idbh)
              c8='*DBDOC  '
              do 19 iu=1,nbdbdoc(idbh)
                IREC=IREC+1
                WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,bk248(iu)
                write(dbdoc(iu),'(a)')bk248(iu)(1:lnblnk(bk248(iu)))
  19          continue
              call tagend(IFB,idbh,ier)
              call usrmsg('Updating database...done.',bl2,'P')
              bkdocok=.FALSE.
              goto 39
            else

C Reconfigure the database by exporting to ascii file and then re-reading.
              call tagend(IFB,idbh,ier)
              LGENDB=gendbfn(idbh)
              call recongdb(IAFIL,IFB,LGENDB,idbh,IER)
              if(ier.ne.0)then
                call edisp(iuout,'Error reconfiguring db.')
              endif
              call usrmsg('Updating database...done.',bl2,'P')
              bkdocok=.FALSE.
              goto 39
            endif
          endif
        endif
      ELSEIF(ICO.EQ.(NCO-2))THEN

C List one or more categories.
        CALL EPMENSV
        INPIC=nbcat(idbh)
        h(1)='You may select one or more classes to list. '
        CALL EPICKS(INPIC,ICATSEL,' ',' Which categories to list:',
     &        32,nbcat(idbh),catlist,'categories',IER,1)
        CALL EPMENRC
        IF(INPIC.EQ.0)GOTO 40
        h(1)=' Summary reporting can be shown in the text feedback'
        h(2)=' or a text file.'
        CALL EASKABC(' Reporting to:',' ',
     &      'text feedback','summary file','cancel',irpt,2)
        if(irpt.eq.1)then
          itu = iuout
          delim = '-'
        elseif(irpt.eq.2)then
          H(1) ='Tabular data can be sent to file with various '
          H(2) ='delimiters: '
          H(3) ='  spaces (format using spaces to lineup columns)'
          H(4) ='  single space between columns'
          H(5) ='  comma separator (for excel)'
          H(6) ='  tab separator (for excel)'
          CALL EASKATOG('Delimeter to use between columns of data:',' ',
     &      'normal spaces','single space','tab','comma','continue',' ',
     &      ' ',IWM,6)
          if(iwm.eq.1)then
            delim = '-'
          elseif(iwm.eq.2)then
            delim = 'S'
          elseif(iwm.eq.3)then
            delim = 'T'
          elseif(iwm.eq.4)then
            delim = 'C'
          endif
          itu = ixunit
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','db text',IER)
        elseif(irpt.eq.3)then
          goto 40
        endif

C List classess and items and then close the file (if writing to one).
        itrc = 2
        call REPGDB(itrc,itu,IFB,LGENDB,idbh,ICATSEL,IER)
        if(irpt.eq.2)then
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','db text',IER)
        endif
      ELSEIF(ICO.EQ.(NCO-3).and.(ACTION.eq.'M'.or.ACTION.eq.'m'))then

C Modify class list.
        h(1)='You may remove an empty category (i.e. which has no more'
        h(2)='than one item in it) or you may add a category at the end'
        h(3)='of the database. '
        CALL EASKABC('Category options: ',' ',
     &      'delete','add','continue',IV,3)
        if(iv.eq.1)then
          INPIC=1
          h(1)='Select one category to delete. '
          CALL EPICKS(INPIC,ICATSEL,' ',' Which category to delete?',
     &        32,nbcat(idbh),catlist,'del category',IER,1)
          IF(INPIC.EQ.0)GOTO 40
          icat=ICATSEL(1)
          if(nbcatit(icat).le.1)then

C << function to delete the category >>
            call usrmsg(
     &    'This function is not yet operational. To delete a category',
     &    'you must currently edit a text version of the db.','W')
          else
            call usrmsg(
     &      'Items must be removed from the category before it can',
     &      'be deleted.','W')
            goto 40
          endif
        elseif(iv.eq.2)then
          if(nbcat(idbh).lt.MGCL)then

C Add a category with an item to the database. If sucessful loop back
C and rebuid the interface.
            call newgcat(IFB,idbh,iclnew,iitnew,ier)
            if(ier.ne.0)then
              call usrmsg('A problem was encountered when attempting',
     &          'to create anew category.','W')
            else
              goto 39
            endif
          else
            call usrmsg(' ',
     &        'No more categories can be added to this database.','W')
            goto 40
          endif
        elseif(iv.eq.3)then
          goto 40
        endif
      ELSEIF(ICO.EQ.(NCO-4).and.(ACTION.eq.'M'.or.ACTION.eq.'m'))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        goto 39
      ELSEIF(ICO.EQ.(NCO-3).and.ACTION.eq.'-')THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        goto 39
      ELSEIF(ICO.GT.MHEAD.AND.ICO.LT.(NCO-MCTL+1))THEN

C Return the index of a category and present items list.
        CALL KEYIND(NCO,ICO,IFOC,IO)
        icwhich=IFOC
        icl=icwhich
        CALL EPMENSV
        call EDSELITEM(IFB,idbh,itwhich,icl,action,IER)
        CALL EPMENRC
        goto 39
      else
        ICO=-1
        goto 40
      endif
      call usrmsg(' ',bl2,'-')
      goto 40

  99  RETURN

   1  write(outs,'(a,I6,a,i3)') 'edselcat:Error reading record ',IREC,
     &  ' of file unit ',ifb
      call edisp(iuout,outs)
      CALL ERPFREE(IFB,IOS)
      ier=1
      return
      end

C ************* EDSELITEM
C EDSELITEM: Given a generic db, provides a list of items in category ic.
C If ACTION = 'M' then include editing if ACTION = '-' only
C allow choice.  Itwhich is the index of the db item selected.
      subroutine edselitem(IFB,idbh,itwhich,ic,action,IER)
#include "epara.h"
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      common/pophelp/h(60)

C Generic db commons: file log, list of db, current classifications,
C and current items then lengths of each list.
C      common/gendb/LGENDB
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

      common/tmpdoca/bkdoctyp,bkdoc(60),bk4(24),bk12(24),bk72(24),
     &  bk248(24)
      common/tmpdocb/bkdocok,nbkdoc,indexbk

      logical primod,dprimod,bkdocok
      character dbtag*12,db*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character dbnote*72,c8*8,outs*124,item*12,LGENDB*72
      character bkdoctyp*2,bkdoc*72,bk4*4,bk12*12,bk72*72,bk248*248
      DIMENSION PDBM(35),ipntrm(35),IVALC(MGCL),catlist(MGCL)

      character action*1,KEY*1,PDBM*38,head*42,h*72,t32*32,t12*12
      CHARACTER dstmp*24,catlist*32
      character ct*8,bl2*2

C Assumes that the db file is open, gather the catg documentation.
      bl2='  '
      primod=.false.
      IAFIL=IFIL+1
      call scandbhd(IFB,idbh,ier)

C Compose a local category selection list.
      nbc=nbcat(idbh)
      do 11 ij=1,nbc
        catlist(ij)=catmenu(ij)
  11  continue

C Gather the names of the various items in catg and present list.
  39  if(ACTION.eq.'M'.or.ACTION.eq.'m')then
        MHEAD=5
        MCTL=5
      elseif(ACTION.eq.'-')then
        MHEAD=5
        MCTL=3
      endif
      ILEN=nbcatit(ic)
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup, clear the item:selection hash list.
  40  INO=-4

C If anything has changed it needs to be written out. First
C remove spaces in data lines(s).
      if(primod.and.(ACTION.eq.'M'.or.ACTION.eq.'m'))then
        call usrmsg('Updating category data... ',bl2,'-')
        call dstamp(dstmp)
        write(dbdate(idbh),'(a)') dstmp
        write(catdate(ic),'(a)') dstmp
        IREC=2
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)dbdate(idbh),dbnote(idbh)
        IREC=irscat(ic)
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(ic),cattype(ic),
     &    catmenu(ic),nbcatdoc(ic),nbcatit(ic),catdate(ic)
        call tagend(IFB,idbh,ier)
        call usrmsg('Updating category data...done. ',bl2,'-')
      endif

      do 33 i=1,35
        ipntrm(i)=0
  33  continue

C Check for the length of items to be displayed.
C IWA is the minimum menu width to hold repeating items.
      la=LNBLNK(catmenu(ic))
      lb=LNBLNK(cattype(IC))
      lc=LNBLNK(cattag(IC))
      ld=0
      le=0
      j=0
      do 32 i=1,nbitem(idbh)
        if(itcat(i).eq.ic)then
          j=j+1
          if(j.GE.IST.AND.(j.LE.(IST+MIFULL)))then
            ld=LNBLNK(itmenu(i))
            if(ld.gt.le)le=ld
          endif
        endif
  32  continue
      write(6,*) la,lb,lc,ld,le

      WRITE(PDBM(2),'(2A)')'  date: ',catdate(ic)
      lm=MIN0(la,31)
      if(ACTION.eq.'M'.or.ACTION.eq.'m')then
        WRITE(PDBM(1),'(2a)') '1 cat: ',catmenu(ic)(1:lm)
        WRITE(PDBM(3),'(4A)') '2 type: ',cattype(IC)(1:lb),
     &    ' tag: ',cattag(IC)(1:lc)
      elseif(ACTION.eq.'-')then
        WRITE(PDBM(1),'(2a)') '  catg: ',catmenu(ic)(1:lm)
        WRITE(PDBM(3),'(4A)') '  type: ',cattype(IC)(1:lb),
     &    ' tag: ',cattag(ic)(1:lc)
      endif
      PDBM(4)= '3 notes                             '
      write(PDBM(5),'(a,i3,a)') '  ___________________(',
     &  nbcatit(IC),' items)_____'
      M=MHEAD

C Build up a display of entities within this catg.
      j=0
      do 34 i=1,nbitem(idbh)
        if(itcat(i).eq.ic)then
          j=j+1
          if(j.GE.IST.AND.(j.LE.(IST+MIFULL)))then
            M=M+1
            CALL EMKEY(M-MHEAD,KEY,IER)
            WRITE(PDBM(M),'(a1,1x,a)')KEY,itmenu(i)(1:le)
     &        

C Remember which item index is with each m.
            ipntrm(m)=i
          endif
        endif
  34  continue
      NC=M

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        PDBM(NC+1)='  ___________________________________'
      ELSE
        WRITE(PDBM(NC+1),15)IPM,MPM 
   15   FORMAT   ('0 page ---- Part: ',I2,' of ',I2,' ---')
      ENDIF
      if(ACTION.eq.'M'.or.ACTION.eq.'m')then
        PDBM(NC+2)='* delete/add/copy/move item      '
        PDBM(NC+3)='! update database                '
        PDBM(NC+4)='? help                           '
        PDBM(NC+5)='- exit to categories             '
        NITMS=NC+5
      elseif(ACTION.eq.'-')then
        PDBM(NC+2)='? help                           '
        PDBM(NC+3)='- exit to categories             '
        NITMS=NC+3
      endif

C Help text for this menu.
      h(1)='This menu lists all the items stored for the category'
      h(2)='previously selected. '
      h(3)='Please, select an item to display/edit its'
      if (idbh.eq.5) then
        h(4)='acoustical absorption coefficients.'
      elseif (idbh.eq.6) then
        h(4)='colorimetry.'
      elseif (idbh.eq.8) then
        h(4)='environmental impacts.'
      elseif (idbh.eq.9) then
        h(4)='environmental impacts.'
      endif

C Display the elements of the classification in a menu.
      INO=-4
      write(head,'(a,a)')'  Items in ',dbmenu(idbh)(1:26)

      CALL EMENU(head,PDBM,NITMS,INO)
      if(INO.EQ.NITMS)then
        return
      elseif(INO.eq.NITMS-1)then

C Help.
        CALL PHELPD('category list',4,'-',0,0,IER)
      elseif(INO.eq.1.and.(ACTION.eq.'M'.or.ACTION.eq.'m'))then

C Cat menu editing if ACTION = 'M'.
        H(1)='A descriptive phrase (up to 32 char) is required for'
        H(2)='display purposes (selection menu`s etc).'
        t32=catmenu(ic)
        CALL EASKS(t32,' ',' Category descriptive phrase ? ',
     &    32,'new category','category descriptor',IER,2)
        if(ier.eq.0.and.t32(1:2).ne.'  ')then
          catmenu(ic)=t32
          primod=.true.
        endif
        call usrmsg(' ',bl2,'-')
        goto 40
      elseif(INO.eq.3)then

C Type and tag editing if ACTION = 'M'.
        if(ACTION.eq.'M'.or.ACTION.eq.'m')then
          H(1)='Each category is identified by a unique name (up to '
          H(2)='12 char) .'
          t12=cattag(ic)
          CALL EASKS(t12,' ',' Category identity (unique string) ? ',
     &      12,'catgx','category tag',IER,2)
          if(ier.eq.0.and.t12(1:2).ne.'  ')then
            cattag(ic)=t12
            primod=.true.
          endif
          call usrmsg(' ',bl2,'-')
          goto 40
        endif
        goto 40
      elseif(INO.eq.4)then

C Documentation. Editing if ACTION = 'M'.
C Help. Strip off the first char ":" from the string before displaying.
        do 152 i=1,nbcatdoc(ic)
          call edisp248(iuout,catdoc(i),80)
 152    continue
        if(ACTION.eq.'M'.or.ACTION.eq.'m')then
          H(1)='Documentation of categories is encouraged. You can'
          H(2)='enter one or several lines of text.'
          H(3)='Retain the `: ` in the first two columns as it is '
          H(4)='used within the db and will not be seen by the user.'
          primod=.FALSE.
          dprimod=.false.
          CALL ASKOK(' ','Edit category documentation ?',primod,
     &      dprimod,4)
          if(primod)then

C Mark for update of cat documentation (which requires a reconfiguration
C of the database) and then re-read catdoc records before re-forming the
C menu interface.
            call EDCATDOC(IFB,idbh,ic,'c',IER)
            primod=.false.
            IREC=irscat(ic)
            do 252 i=1,nbcatdoc(ic)
              IREC=IREC+1
              read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,catdoc(i)
 252        continue
            goto 39
          endif
        endif
        goto 40
      elseif(INO.eq.NITMS-2.and.ACTION.eq.'-')then

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        goto 40
      elseif(INO.eq.NITMS-4.and.(ACTION.eq.'M'.or.ACTION.eq.'m'))then

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        goto 40
      elseif(INO.eq.NITMS-3.and.(ACTION.eq.'M'.or.ACTION.eq.'m'))then

C Delete/add/copy/move...
        h(1)='You may remove an empty category (i.e. which has no more'
        h(2)='than one item in it) or you may add a category at the end'
        h(3)='of the database. '
        CALL EASKATOG('Options for items: ',' ','delete','add','copy',
     &    'move (to another catg)','continue',' ',' ',IV,3)
        if(iv.eq.1)then
          call edisp(iuout,'delete function...')
          CALL EPMENSV
          CALL EMENU(' Delete which item',PDBM,NITMS,INOC)
          CALL EPMENRC
          iitorig=ipntrm(inoc)
          if(iitorig.ne.0)then

C If one of the items decrement its category association to zero and
C decrement nbcatit.
            bkdocok=.TRUE.
            bkdoctyp='DE'
            indexbk=0
            write(item,'(a)')ittag(iitorig)
            call usrmsg('Deleting item from database...',bl2,'-')
            call dstamp(dstmp)
            write(catdate(ic),'(a)') dstmp
            nbcatit(ic)=nbcatit(ic)-1
            IREC=irscat(ic)
            WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(ic),cattype(ic),
     &        catmenu(ic),nbcatdoc(ic),nbcatit(ic),catdate(ic)

            itcat(iitorig)=0
            IREC=irsit(iitorig)
            if(idbver(idbh).eq.1)then
              nbitdoc=1
              WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iitorig),
     &          itmenu(iitorig),itdate(iitorig),itcat(iitorig),
     &          nbitdoc,nbitdat(iitorig),nbitrep(iitorig),
     &          nbitusc(iitorig),ibitusc(iitorig)
            else
              WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iitorig),
     &          itmenu(iitorig),itdate(iitorig),itcat(iitorig),
     &          nbitdat(iitorig),nbitrep(iitorig),
     &          nbitusc(iitorig),ibitusc(iitorig)
            endif
            call tagend(IFB,idbh,ier)

C Reconfigure the database (the item will not be read back in).
            LGENDB=gendbfn(idbh)
            call recongdb(IAFIL,IFB,LGENDB,idbh,IER)
            if(ier.ne.0)then
              call usrmsg('Problem reconfiguring db.',item,'W')
              goto 40
            endif
            call usrmsg('Deleting item from database...done.',
     &        bl2,'-')
            bkdocok=.false.
            goto 39
          endif
        elseif(iv.eq.2)then

C Adding an item (via newgitem) appends data at end of database. A
C subsequent call to reconfigure the db moves data into the relevant
C category. To assist, remember the new item name and scan reconfigured
C database for it.
          call usrmsg('Adding item to database...',bl2,'-')
          call newgitem(IFB,idbh,ic,iit,ier)
          write(item,'(a)')ittag(iit)
          LGENDB=gendbfn(idbh)
          call recongdb(IAFIL,IFB,LGENDB,idbh,IER)
          if(ier.ne.0)then
            call usrmsg('Problem reconfiguring db.',item,'W')
            goto 40
          endif
          iit=0
          do 36 it=1,nbitem(idbh)
            if(item(1:12).eq.ittag(it)(1:12))then
              call rgenitem(IFB,idbh,it,ier)
              if(ier.ne.0)then
                call usrmsg('Problem recovering the item:',item,'W')
                goto 40
              else
                iit=it
              endif
            endif
  36      continue
          call rgenitem(IFB,idbh,iit,ier)
          call usrmsg('Adding item to database...done.',bl2,'P')
          goto 39
        elseif(iv.eq.3)then
          CALL EPMENSV
          CALL EMENU(' Copy which item',PDBM,NITMS,INOC)
          CALL EPMENRC
          iitorig=ipntrm(inoc)

          H(1)='An item can be copied into the current or another '
          H(2)='category. It will be given a unique identifier. '
          write(outs,'(3a)')'Destination category (currently ',
     &      catmenu(ic)(1:20),'):'
          IX=1
          CALL EPMENSV
	  CALL EPICKS(IX,IVALC,' ',outs,
     &      32,nbcat(idbh),catlist,'category list',IER,2)
          CALL EPMENRC
          call usrmsg('Copy item within database...',bl2,'P')
          iccopy=IVALC(1)
          icorig=ic
          if(icorig.ne.0.and.iccopy.ne.0)then
            call CPITEM(IFB,idbh,icorig,iitorig,iccopy,iitcopy,'n',ier)

            write(item,'(a)')ittag(iitcopy)
            write(6,*)nbitem(idbh),iccopy,iitcopy,item
            LGENDB=gendbfn(idbh)
            call recongdb(IAFIL,IFB,LGENDB,idbh,IER)
            if(ier.ne.0)then
              call edisp(iuout,'Error reconfiguring db.')
            endif
            iit=0
            do 37 it=1,nbitem(idbh)
              if(item(1:12).eq.ittag(it)(1:12))then
                call rgenitem(IFB,idbh,it,ier)
                if(ier.ne.0)then
                  call usrmsg('Problem recovering the item:',item,'W')
                  goto 40
                else
                  iit=it
                  write(6,*)nbitem(idbh),ic,iit,' c ',ittag(iit)
                endif
              endif
  37        continue
            write(6,*)nbitem(idbh),ic,iit,' g ',ittag(iit)
            call rgenitem(IFB,idbh,iit,ier)
            call usrmsg('Copy item within database...done.',bl2,'P')
            goto 39
          endif
        elseif(iv.eq.4)then
          CALL EPMENSV
          CALL EMENU(' Move which item?',PDBM,NITMS,INOC)
          CALL EPMENRC
          iitorig=ipntrm(inoc)

          H(1)='An item can be moved into another category. It will'
          H(2)='retain its current identifier. '
          write(outs,'(3a)')'Destination category (i.e. other than ',
     &      catmenu(ic)(1:20),'):'
          IX=1
          CALL EPMENSV
	  CALL EPICKS(IX,IVALC,' ',outs,
     &      32,nbc,catlist,'destination category',IER,2)
          CALL EPMENRC
          call usrmsg('Move item within database...',bl2,'P')
          icmove=IVALC(1)
          icorig=ic
          if(icorig.ne.0.and.icmove.ne.0)then
            call MVITEM(IFB,idbh,icorig,iitorig,icmove,iitmove,ier)

C A move implies removal of the original item so direct the reconfiguration
C of the database to decrement the number of items.
            bkdocok=.TRUE.
            bkdoctyp='DE'
            indexbk=0
            write(item,'(a)')ittag(iitmove)
            write(6,*)nbitem(idbh),icorig,icmove,iitmove,' ',item
            LGENDB=gendbfn(idbh)
            call recongdb(IAFIL,IFB,LGENDB,idbh,IER)
            if(ier.ne.0)then
              call edisp(iuout,'Error reconfiguring db.')
            endif
            bkdocok=.false.
            call usrmsg('Move item within database...done.',bl2,'P')
            goto 39
          endif
        elseif(iv.eq.5)then
          goto 40
        endif
        goto 40
      elseif(INO.gt.MHEAD.and.INO.lt.NITMS-MCTL+1)then

C Have selecte an item, decode and then....
        itwhich=ipntrm(ino)
        write(item,'(a)')ittag(itwhich)
        if(ACTION.eq.'M'.or.ACTION.eq.'m')then
          call edisp(iuout,'edit the item...')
        else
          call edisp(iuout,'item selected...')
        endif

C Display information on the item. << >>
C Search each item until name match found and recover data.
        iit=0
        do 35 it=1,nbitem(idbh)
          if(item(1:12).eq.ittag(it)(1:12))then
            call rgenitem(IFB,idbh,it,ier)
            if(ier.ne.0)then
              call edisp(iuout,'Problem reading this item.')
              goto 40
            else
              iit=it
            endif
          endif
  35    continue

C If reached end of items and no match then return error condition.
        if(iit.eq.0.or.(iit.ne.itwhich))then
          call usrmsg('An item of an identical name found in ',
     &      'another category...or no item found. ','W')
        endif

C Depending on the db call appropriate tabular or editing facility.
C Note some editing variants may require EPMENSV/EPMENRC.
        if(dbtag(idbh)(1:11).eq.'multicon_db')then
          db=dbtag(idbh)
          call edmat2(db,IFB,idbh,ic,iit,'T',ier)
          if(ACTION.eq.'M'.or.ACTION.eq.'m')then
            call EDMLC(IFB,idbh,ic,iit,'M',IER)
          else
            call EDMLC(IFB,idbh,ic,iit,'-',IER)
          endif
        elseif(dbtag(idbh)(1:8).eq.'material')then
          db=dbtag(idbh)
          call edmat2(db,IFB,idbh,ic,iit,'T',ier)
          if(ACTION.eq.'M'.or.ACTION.eq.'m')then
            call EDMAT(IFB,idbh,ic,iit,'M',IER)
          else
            call EDMAT(IFB,idbh,ic,iit,'-',IER)
          endif
        elseif(dbtag(idbh)(1:7).eq.'optical')then
          call usrmsg(
     &      'Optical database editing not yet implemented.',bl2,'W')
        elseif(dbtag(idbh)(1:8).eq.'acoustic')then
          call EDACOUS(IFB,idbh,ic,iit,'T',IER)
          if(ACTION.eq.'M'.or.ACTION.eq.'m')then
            call EDACOUS(IFB,idbh,ic,iit,'M',IER)
          else
            call EDACOUS(IFB,idbh,ic,iit,'-',IER)
          endif
        elseif(dbtag(idbh)(1:12).eq.'colourimetry')then
          call EDCOLOUR(IFB,idbh,ic,iit,'T',IER)
          if(ACTION.eq.'M'.or.ACTION.eq.'m')then
            call EDCOLOUR(IFB,idbh,ic,iit,'M',IER)
          else
            call EDCOLOUR(IFB,idbh,ic,iit,'-',IER)
          endif
        elseif(dbtag(idbh)(1:9).eq.'micotoxin')then
          call usrmsg(
     &    'Micotoxin database editing not yet implemented.',
     &    bl2,'W')
        elseif(dbtag(idbh)(1:9).eq.'envir_imp')then

C Depending on the category type, invoke editing functions.
          ict=itcat(iit)
          ct=cattype(ict)
          if(ct(1:6).eq.'RECYCP'.or.
     &       ct(1:6).eq.'INCENP'.or.ct(1:5).eq.'DUMPP')then
            call epopenv(IFB,idbh,ict,iit,'T',IER)
          elseif(ct(1:6).eq.'ASMPRJ'.or.ct(1:8).eq.'CNSTRMNT'.or.
     &           ct(1:8).eq.'CNSTRASM')then
            call epopenv(IFB,idbh,ict,iit,'T',IER)
          elseif(ct(1:6).eq.'MATLCA')then
            call epopenv(IFB,idbh,ict,iit,'T',IER)
          elseif(ct(1:7).eq.'CONSIMP')then
            call epopenv(IFB,idbh,ict,iit,'T',IER)
          endif
          if(ACTION.eq.'M'.or.ACTION.eq.'m')then
            if(ct(1:6).eq.'RECYCP'.or.
     &         ct(1:6).eq.'INCENP'.or.ct(1:5).eq.'DUMPP')then
              call edenvi(IFB,idbh,ic,iit,'M',IER)
            elseif(ct(1:6).eq.'ASMPRJ'.or.ct(1:8).eq.'CNSTRMNT'.or.
     &             ct(1:8).eq.'CNSTRASM')then
              call edenvp(IFB,idbh,ict,iit,'M',IER)
            elseif(ct(1:6).eq.'MATLCA')then
              call edenvf(IFB,idbh,ict,iit,'M',IER)
            elseif(ct(1:7).eq.'CONSIMP')then
              call edenvci(IFB,idbh,ict,iit,'M',IER)
            endif
          else
            if(ct(1:6).eq.'RECYCP'.or.
     &         ct(1:6).eq.'INCENP'.or.ct(1:5).eq.'DUMPP')then
              call edenvi(IFB,idbh,ict,iit,'-',IER)
            elseif(ct(1:6).eq.'ASMPRJ'.or.ct(1:8).eq.'CNSTRMNT'.or.
     &             ct(1:8).eq.'CNSTRASM')then
              call edenvp(IFB,idbh,ict,iit,'-',IER)
            elseif(ct(1:6).eq.'MATLCA')then
              call edenvf(IFB,idbh,ict,iit,'-',IER)
            elseif(ct(1:7).eq.'CONSIMP')then
              call edenvci(IFB,idbh,ict,iit,'-',IER)
            endif
          endif
        elseif(dbtag(idbh)(1:9).eq.'transport')then
          call EDTRANSP(IFB,idbh,ic,iit,'T',IER)
          if(ACTION.eq.'M'.or.ACTION.eq.'m')then
            call EDTRANSP(IFB,idbh,ic,iit,'M',IER)
          else
            call EDTRANSP(IFB,idbh,ic,iit,'-',IER)
          endif
        elseif(dbtag(idbh)(1:9).eq.'pressc_db')then
          call usrmsg(
     &    'Wind pressure editing not yet implemented.',bl2,'W')
        elseif(dbtag(idbh)(1:8).eq.'profiles')then
          call edprof(IFB,idbh,ic,iit,'T',IER)
          if(ACTION.eq.'M'.or.ACTION.eq.'m')then
            call EPMENSV
            call edprof(IFB,idbh,ic,iit,'M',IER)
            call EPMENRC
          else
            call EPMENSV
            call edprof(IFB,idbh,ic,iit,'-',IER)
            call EPMENRC
          endif
        else
          call usrmsg('Unknown database type.',bl2,'W')
        endif
        goto 39
      else
        INO=-1
        goto 40
      endif
      call usrmsg(' ',bl2,'-')
      goto 40

C Error trap.
   1  write(outs,'(a,I6,a,i3)') 'edselitem:Error reading record ',IREC,
     &  ' of file unit ',ifb
      call edisp(iuout,outs)
      CALL ERPFREE(IFB,IOS)
      ier=1
      return
      end

C ************* INTDBDOC
C INTDBDOC: Initial db documentation text is instanciated based on act and
C on the type of database. If act='n' then a new database from scratch,
C else if act='i' then setup documentation to reflect import of a legacy
C database file. Idbh is position in hash table. afile is the legacy file
C if known.
      subroutine intdbdoc(idbh,act,afil,ier)
#include "esprdbfile.h"
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,dbnote*72,act*1
      character afil*72

      if(dbtag(idbh)(1:11).eq.'multicon_db')then

C Create basic constructions database with initial documentation
C and one category with one item.
C << take text from _e.gdba files to fill this in >>
        nbdbdoc(idbh)=6
        if(act(1:1).eq.'n')then
          dbdoc(1)=
     &  'Constructions database (from scratch): Documentation follows:'
        elseif(act(1:1).eq.'i')then
          write(dbdoc(1),'(3a)')
     &      'Constructions (imported from ',afil(1:lnblnk(afil)),
     &      ') Documentation follows:'
        endif
        write(dbdoc(2),'(2a)')
     & 'Each entry contains the following data fields: ',
     & 'matching construction name (`same` if symetric),'
        write(dbdoc(3),'(3a)')
     & 'colour name on `other` face, colour name on `inside` face, ',
     & 'OPAQ:TRAN tag and optical name, acoustic name on `other` face,',
     & ' acoustic name on `inside` face,'
        write(dbdoc(4),'(3a)')
     & 'longwave emissivity at `inside` and `other` face, ',
     & 'solar absorb at `inside` and `other` face, ',
     & 'environmental effects name'
        write(dbdoc(5),'(3a)')
     & 'For each layer: indicator for thermal or non-thermal layer, ',
     & 'indicator for solid or air gap, material name, thickness (m),',
     & ' material index (for legacy models),'
        write(dbdoc(6),'(3a)')
     & 'air gap resistance (vertical placement),',
     & 'air gap resistance (floor:ceiling placement), ',
     & 'air gap resistance (sloped placement)'
      elseif(dbtag(idbh)(1:8).eq.'material')then
        nbdbdoc(idbh)=4
        if(act(1:1).eq.'n')then
          dbdoc(1)=
     &    'Materials database (from scratch): Documentation follows:'
        elseif(act(1:1).eq.'i')then
          write(dbdoc(1),'(3a)')
     &      'Materials (imported from ',afil(1:lnblnk(afil)),
     &      ') Documentation follows:'
        endif
        write(dbdoc(2),'(2a)')
     & 'A material is composed of a set of names to ',
     & 'colour, acoustic, environmental effects databases.'
        write(dbdoc(3),'(3a)')
     & 'Entries contain the following minimal data fields: ',
     & 'Conductivity W/(m deg.C), Density kg/m**3, ',
     & 'Specific Heat J/(kg deg.C), IR emissivity (-),'
        write(dbdoc(4),'(3a)')
     & 'Solar absop (-), Specularity (-), Roughness (-), ',
     & 'Refractive index (-), Extinction coefficient (-), ',
     & 'and two data fields (for future use).'
      elseif(dbtag(idbh)(1:8).eq.'acoustic')then
        nbdbdoc(idbh)=3
        if(act(1:1).eq.'n')then
          dbdoc(1)=
     &    'Acoustic database (from scratch): Documentation follows:'
        elseif(act(1:1).eq.'i')then
          write(dbdoc(1),'(3a)')
     &      'Acoustic (imported from ',afil(1:lnblnk(afil)),
     &      ') Documentation follows:'
        endif
        write(dbdoc(2),'(3a)')
     & 'Data for a material layer (Names should start with an `M`) or',
     & ' a composite construction (Names should start with a `C`). ',
     & 'Frequencies are listed below:'
        write(dbdoc(3),'(2a)')
     & '100, 125, 160, 200, 250, 315, 400, 500, 630, ',
     & '800, 1000, 1250, 1600, 2000, 2500, 3150, 4000.'
      elseif(dbtag(idbh)(1:12).eq.'colourimetry')then
        nbdbdoc(idbh)=2
        if(act(1:1).eq.'n')then
          dbdoc(1)=
     &    'Colour database (from scratch): Documentation follows:'
        elseif(act(1:1).eq.'i')then
          write(dbdoc(1),'(3a)')
     &      'Colour database (imported from ',afil(1:lnblnk(afil)),
     &      ') Documentation follows:'
        endif
        write(dbdoc(2),'(4a)')
     & 'Data is Red, Green, Blue values (range 0-255) and an ',
     & 'equivalent grey reflectance (range 0.0-1.0) with two optional',
     & ' fields (12 char and 72 char) supporting surface patterns or ',
     & 'bitmaps. '
      elseif(dbtag(idbh)(1:9).eq.'envir_imp')then
        nbdbdoc(idbh)=9
        write(dbdoc(1),'(2a)')
     & 'Environmental impacts database (from scratch) The following ',
     & 'categories are included:'
        write(dbdoc(2),'(3a)')
     & 'a) Constructions environmental information: holds the ',
     & 'environmental effects related to a construction - e.g. its ',
     & 'assembly, possible prefabrication, transport and maintenance.'
        write(dbdoc(3),'(3a)')
     & 'b) Constructions assembly: holds details of effects for either',
     & ' the surface face or perimeter of a construction at its ',
     & 'assembly site.'
        write(dbdoc(4),'(2a)')
     & 'c) Constructions maintenance: holds details of effects for ',
     & 'maintaining a construction (either its surface or perimeter)'
        write(dbdoc(5),'(3a)')
     & 'd) Assembly at the project: for pre-fabricated components ',
     & 'such as windows to account for additional assembly effects ',
     & 'at the building site including breakage rate.'
        write(dbdoc(6),'(4a)')
     & 'e) Material environmental imformation holds details for a ',
     & 'material - e.g. fabrication effects, material lifetime, ',
     & 'transport to the, assembly site (factory or building site) ',
     & 'including distance and breakage) and disposal.'
        write(dbdoc(7),'(2a)')
     & 'g) Recycling processes holds details of recycling process ',
     & 'including transport.'
        write(dbdoc(8),'(2a)')
     & 'h) Incineration processes holds details of incineration ',
     & 'process including transport.'
        write(dbdoc(9),'(2a)')
     & 'i) Dump processes holds details of dump process including ',
     & 'transport.'
      elseif(dbtag(idbh)(1:9).eq.'transport')then
        nbdbdoc(idbh)=2
        if(act(1:1).eq.'n')then
          dbdoc(1)=
     &    'Transport database (from scratch): Documentation follows:'
        elseif(act(1:1).eq.'i')then
          write(dbdoc(1),'(3a)')
     &      'Transport database (imported from ',afil(1:lnblnk(afil)),
     &      ') Documentation follows:'
        endif
        write(dbdoc(2),'(4a)')
     & 'Each entry contains the following data fields: Global Warming',
     & ' Potential (GWP) CO2/km/t, Acidification Potential (AP) ',
     & 'SO2/km/t, PhotoSmog depletion (PS) NOX/km/t, Utilisation ',
     & 'of Non Renewable Energy (NRE) MJ/km/t.'
      elseif(dbtag(idbh)(1:9).eq.'pressc_db')then
        nbdbdoc(idbh)=4
        if(act(1:1).eq.'n')then
          dbdoc(1)=
     &    'Wind pressures (from scratch): Documentation follows:'
        elseif(act(1:1).eq.'i')then
          write(dbdoc(1),'(3a)')
     &      'Wind pressure database (imported from ',
     &      afil(1:lnblnk(afil)),') Documentation follows:'
        endif
        write(dbdoc(2),'(2a)')
     & 'A wind pressure database holds pressure coef. related to ',
     & 'surfaces with particul aspect ratios and tilt of the surface.'
        write(dbdoc(3),'(4a)')
     & 'A pressure coef set contains data at the following angles ',
     & '(the first value is the `normal` from the surface) and ',
     & 'subsequent values are in 22.5 deg increments in the ',
     & 'clockwise direction (in plan view).'
        write(dbdoc(4),'(4a)')
     & 'The aspect ratios relates to the length and width of the ',
     & 'whole facade of the building on which the opening is found. ',
     & 'The tilt relates to the surface in which the opening is ',
     & 'found (0=?).'
      elseif(dbtag(idbh)(1:8).eq.'profiles')then
        nbdbdoc(idbh)=4
        if(act(1:1).eq.'n')then
          dbdoc(1)=
     &    'Profiles (from scratch): Documentation follows:'
        elseif(act(1:1).eq.'i')then
          write(dbdoc(1),'(3a)')
     &      'Profiles database (imported from ',
     &      afil(1:lnblnk(afil)),') Documentation follows:'
        endif
        write(dbdoc(2),'(4a)')
     & 'Each entry contains the following data fields: nb of active ',
     & 'day types (up to 10), nb of periods including, one for each ',
     & 'non-active day type (so the minimum is 10) and for each day ',
     & 'type the nb of periods (zero for non-active).'
        write(dbdoc(3),'(4a)')
     & 'Each entry also contains repeating data fields: day type ',
     & '(8 char string) default = `all` , attribute (8 char string) ',
     & 'default = `none`, start time (decimal hour), and 2 or 4 or 9 ',
     & 'or 21 items of real data.' 
        write(dbdoc(4),'(5a)')
     & 'Depending on the category: cat CASUAL uses sensible%, latent%',
     & ' (5per), cat CASUAL3 uses sensible/lat % for 3 gain types ',
     & '(3per), cat CASUAL7 uses sens/lat % for 7 gain types (2per)',
     & ', cat thermostat uses heating%/cooling%/RH low%/RH high% ',
     & '(4per).'
      endif

      return
      end

C ************* EDCATDOC
C EDCATDOC: Edit category level documentation (act = c). Assumes that common blocks
C are current (i.e. scandbhd already called). If changes made update
C the current database.
      subroutine edcatdoc(IFB,idbh,ic,act,IER)
#include "esprdbfile.h"
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

C      common/gendb/LGENDB
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)

C Backup of documentation (for editing).
      common/tmpdoca/bkdoctyp,bkdoc(60),bk4(24),bk12(24),bk72(24),
     &  bk248(24)
      common/tmpdocb/bkdocok,nbkdoc,indexbk

      logical OK,dok,bkdocok
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character dbnote*72,c8*8,outs*124
      character bkdoctyp*2,bkdoc*72,bk4*4,bk12*12,bk72*72,bk248*248
      character t72*72,LGENDB*72,act*1,h*72,dstmp*24,t248*248

C Assumes that the db file is open, gather the catg documentation. 
      IAFIL=IFIL+1

C Mark for update of cat documentation.
      if(act.eq.'c'.or.act.eq.'C')then
        IREC=irscat(ic)
        do 53 ij=1,nbcatdoc(ic)
          IREC=IREC+1
          read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,catdoc(ij)
C Debug...
          write(6,*) catdoc(ij)
  53    continue
        H(1)='Documentation of categories is encouraged. You can'
        H(2)='enter one or several lines of text.'
        H(3)='Retain the `: ` in the first two columns as it is '
        H(4)='used within the db and will not be seen by the user.'
        bkdocok=.TRUE.
        bkdoctyp='CD'
        nbkdoc=nbcatdoc(ic)
        indexbk=ic

C Non-proforma editing of category documentation.     
        do 252 ik=1,nbkdoc
          write(bk248(ik),'(a)')catdoc(ik)(1:lnblnk(catdoc(ik)))
          t248=' '
          write(t248,'(a)')catdoc(ik)(1:lnblnk(catdoc(ik)))
          write(outs,'(a,I2)')' Documentation line ',ik
          CALL EASKS248(t248,outs,' ',72,': ','cd',IER,4)
          if(ier.eq.0)bk248(ik)=t248
 252    continue
 253    dok=.false.
        CALL ASKOK(' ','Add another line ?',ok,dok,4)
        if(OK)then
          nbkdoc=nbkdoc+1
          write(bkdoc(nbkdoc),'(a)')': '
          write(t248,'(a)')': '
          write(outs,'(a,I2)')' Documentation line ',nbkdoc
          CALL EASKS248(t248,outs,' ',72,': ','cd',IER,4)
          if(ier.eq.0)bk248(nbkdoc)=t248
          goto 253
        endif

        call usrmsg('Updating category data...',' ','-')
        call dstamp(dstmp)
        write(dbdate(idbh),'(a)') dstmp
        write(catdate(ic),'(a)') dstmp
        IREC=2
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)dbdate(idbh),dbnote(idbh)
        IREC=irscat(ic)
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(ic),cattype(ic),
     &    catmenu(ic),nbcatdoc(ic),nbcatit(ic),catdate(ic)
        if(nbkdoc.eq.nbcatdoc(ic))then

C No change in length so update the database and commons by writing updated text.
          c8='*CATDOC '
          do 19 iu=1,nbcatdoc(ic)
            IREC=IREC+1
            WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,bkdoc(iu)
            write(catdoc(iu),'(a)')bk248(iu)(1:lnblnk(bk248(iu)))
  19      continue
          call tagend(IFB,idbh,ier)
          call usrmsg('Updating category data...done.',' ','P')
          bkdocok=.FALSE.
          return
        else

C Update the database by exporting to ascii file and then re-reading.
          call tagend(IFB,idbh,ier)
          LGENDB=gendbfn(idbh)
          call recongdb(IAFIL,IFB,LGENDB,idbh,IER)
          if(ier.ne.0)then
            call edisp(iuout,'Error reconfiguring db.')
          endif
          bkdocok=.FALSE.
          call usrmsg('Updating category data...done.',' ','P')
          return
        endif
      else

C << >>
      endif
      return

C Error trap.
   1  write(outs,'(a,I6)') 'EDCATDOC Error writing record ',IREC
      call edisp(iuout,outs)
      CALL ERPFREE(IFB,IOS)
      ier=1
      return
      end

C ************* newgcat
C  newgcat: Depending on the db type create a new class and place in it one
C item of the current database type. Iclnew is the new class and
C iitnew is the new item associated with it.
      subroutine newgcat(IFB,idbh,iclnew,iitnew,ier)
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

C      common/gendb/LGENDB
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)

      dimension clist(8),icatlst(8)
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,dbnote*72
      character catdoc*248,cattag*12,cattype*8,catmenu*32,catdate*24
      character ittag*12,itmenu*32,LGENDB*72,h*72
      character c8*8,c20*20,outs*124,dstmp*24,item*12,clist*28

C Position of new item will be at lastr+1 so compute and updated lastr
C for the current type of database.
      IAFIL=IFIL+1
      if(nbcat(idbh)+1.gt.MGCL)then
        call usrmsg(' ',' ','W')
        ier=1
        return
      endif
      nbcat(idbh)=nbcat(idbh)+1
      icl=nbcat(idbh)

C Clear catg documentation.
      do 191 i=1,5
        catdoc(i)=': '
  191 continue

C Update category pointers.
      IREC=lastr(idbh)+1
      irscat(icl)=IREC
      IREC = 3+nbunit(idbh)+nbsrc(idbh)+nbdbdoc(idbh)
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(irscat(j),j=1,24)
      IREC=IREC+1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(irscat(j),j=25,MGCL)
      if(icl.le.9)then
        write(cattag(icl),'(a,i1)') 'category',icl
      elseif(icl.ge.10.and.icl.le.99)then
        write(cattag(icl),'(a,i2)') 'category',icl
      endif
      write(catmenu(icl),'(a,i3)') 'new category',icl

C In the case of acoustics and environmental impacts user must specify
C which category type to use.
      if(dbtag(idbh)(1:8).eq.'acoustic')then
        h(1)='An acoustic category can hold items related to individual'
        h(2)='materials or to constructions (composites of several  '
        h(3)='layers). '
        call easkab('Acoustic category options: ',' ',
     &      'materials','constructions',IV,3)
        if(iv.eq.1)then
          write(cattype(icl),'(a)') 'MAT     '
        elseif(iv.eq.2)then
          write(cattype(icl),'(a)') 'SYS     '
        endif
      elseif(dbtag(idbh)(1:9).eq.'envir_imp')then
        h(1) ='An environmental effects category can be one of the'
        h(2) ='following types:  '
        h(3) =' '
        h(4) =' Constructions effects '
        h(5) =' Constructions assembly '
        h(6) =' Constructions maintenance '
        h(7) =' Assembly at the project (for prefabricated items)'
        h(8) ='  '
        h(9) =' Material effects  '
        h(10)=' Recycling processes  '
        h(11)=' Incineration processes  '
        h(12)=' Dump processes  '
        clist(1)='constructions effects     '
        clist(2)='constructions assembly    '
        clist(3)='constructions maintenance '
        clist(4)='assembly at the project   '
        clist(5)='material effects          '
        clist(6)='recycling processes       '
        clist(7)='incineration processes    '
        clist(8)='dump processes            '
        INPIC=1
        CALL EPICKS(INPIC,icatlst,' ',
     &    ' Environmental impacts category options: ',
     &    28,8,clist,'env impacts categ list',IER,12)
        if(INPIC.EQ.0)then
          write(cattype(icl),'(a)') 'STD     '
        else
          if(icatlst(1).eq.0)then
            write(cattype(icl),'(a)') 'STD     '
          elseif(icatlst(1).eq.1)then
            write(cattype(icl),'(a)') 'CONSIMP'
          elseif(icatlst(1).eq.2)then
            write(cattype(icl),'(a)') 'CNSTRASM'
          elseif(icatlst(1).eq.3)then
            write(cattype(icl),'(a)') 'CNSTRMNT'
          elseif(icatlst(1).eq.4)then
            write(cattype(icl),'(a)') 'ASMPRJ  '
          elseif(icatlst(1).eq.5)then
            write(cattype(icl),'(a)') 'MATLCA  '
          elseif(icatlst(1).eq.6)then
            write(cattype(icl),'(a)') 'RECYCP  '
          elseif(icatlst(1).eq.7)then
            write(cattype(icl),'(a)') 'INCENP  '
          elseif(icatlst(1).eq.8)then
            write(cattype(icl),'(a)') 'DUMPP   '
          endif
        endif
      else
        write(cattype(icl),'(a)') 'STD     '
      endif
      nbcatdoc(icl)=1
      write(catdoc(1),'(3a)') ': Category ',
     &  cattag(icl)(1:lnblnk(cattag(icl))),' was added by a user.'
      call dstamp(dstmp)
      write(catdate(icl),'(a)') dstmp
      nbcatit(icl)=0

C Write category record and documentation.
      IREC=irscat(icl)
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(icl),cattype(icl),
     &  catmenu(icl),nbcatdoc(icl),nbcatit(icl),catdate(icl)
      do 52 ijb=1,nbcatdoc(icl)
        IREC=IREC+1
        c8='*CATDOC '
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,catdoc(ijb)
  52  continue
      lastr(idbh)=IREC+1

C Update the db header and then write the end tag (new cagetory is
C at the end of the db and does not yet have any items.
C      c20='*ESP-r binary db    '
      c20='*ESP-r binary db 2  '
      irec=1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c20,dbtag(idbh),dbmenu(idbh),
     &  idbwidth(idbh),nbunit(idbh),nbsrc(idbh),nbcat(idbh),
     &  nbitem(idbh),nbdbdoc(idbh),lastr(idbh)
      call tagend(IFB,idbh,ier)

C Now create an item for this new category.
      call usrmsg('Adding item in new category...',' ','-')
      call newgitem(IFB,idbh,icl,iit,ier)
      write(item,'(a)')ittag(iit)
      LGENDB=gendbfn(idbh)
      call recongdb(IAFIL,IFB,LGENDB,idbh,IER)
      if(ier.ne.0)then
        call usrmsg('Problem reconfiguring db.',item,'W')
        return
      endif
      iit=0
      do 36 it=1,nbitem(idbh)
        if(item(1:12).eq.ittag(it)(1:12))then
          call rgenitem(IFB,idbh,it,ier)
          if(ier.ne.0)then
            call usrmsg('Problem recovering the item:',item,'W')
            return
          else
            iit=it
          endif
        endif
  36  continue
      call rgenitem(IFB,idbh,iit,ier)
      if(ier.eq.0)then
        iclnew=icl
        iitnew=iit
        call usrmsg('Adding item in new category...done.',' ','P')
      else
        call usrmsg('Problem recovering the item:',item,'W')
        iclnew=0
        iitnew=0
        return
      endif
      return

   1  write(outs,'(a,I6)') 'newgitem: Error writing record ',IREC
      call edisp(iuout,outs)
      ier=1
      CALL ERPFREE(IFB,IOS)
      return
      end

C ************* newgitem
C  newgitem: Depending on the db type create a new instance of an item for
C subsequent editing. Icl is the class the item is associated with.
      subroutine newgitem(IFB,idbh,icl,iit,ier)
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN

      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc

      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,dbnote*72
      character catdoc*248,cattag*12,cattype*8,catmenu*32,catdate*24
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character c20*20,outs*124,ptag*12,dstmp*24,db*12,ctype*8

      character ituntstr*42,itsrcstr*1,itcnfstr*42,itemtag*12
      logical ok

C Rescan the header of this database and then re-establish the item
C tags and categories before adding new one.
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)

C For each item in database recover full details.
      do 34 iit=1,nbitem(idbh)
        call rgenitem(IFB,idbh,iit,ier)
  34  continue

C Position of new item will be at lastr+1 so compute and updated lastr
C for the current type of database. The increment (increm) in records
C in the file for this item is 2 for header, 3 for help (used to be
C one per help line, revised to support 248 char help), plus data
C and repeating records.
      db=dbtag(idbh)
      if(nbitem(idbh)+1.le.MGIT)then
        IREC=lastr(idbh)+1
        if(dbtag(idbh)(1:11).eq.'multicon_db')then
          increm=10
          lastr(idbh)=lastr(idbh)+increm
          nbitem(idbh)=nbitem(idbh)+1
          iit=nbitem(idbh)
          irsit(iit)=IREC
          ptag = itemtag('constr',iit)
          ctype='STD'
          call instanitm(db,ctype,iit,ier)
          call edisp(iuout,' construction db...')
        elseif(dbtag(idbh)(1:8).eq.'material')then
          increm=7
          lastr(idbh)=lastr(idbh)+increm
          nbitem(idbh)=nbitem(idbh)+1
          iit=nbitem(idbh)
          irsit(iit)=IREC
          ptag = itemtag('mat',iit)
          ctype='STD'
          call instanitm(db,ctype,iit,ier)
          call edisp(iuout,' material db...')
        elseif(dbtag(idbh)(1:7).eq.'optical')then
          call edisp(iuout,' optical db... << not yet >>')
          increm=0
          ier=1
          return
        elseif(dbtag(idbh)(1:8).eq.'acoustic')then
          increm=7
          lastr(idbh)=lastr(idbh)+increm
          nbitem(idbh)=nbitem(idbh)+1
          iit=nbitem(idbh)
          irsit(iit)=IREC
          irpt=0
          if(cattype(icl)(1:3).eq.'MAT')irpt=1
          if(cattype(icl)(1:3).eq.'SYS')irpt=2
          if(irpt.eq.1)then
            ptag = itemtag('Macou',iit)
            itcat(iit)=1
            ctype='MAT'
            call instanitm(db,ctype,iit,ier)
          elseif(irpt.eq.2)then
            ptag = itemtag('Cacou',iit)
            itcat(iit)=2
            ctype='SYS'
            call instanitm(db,ctype,iit,ier)
          else
            call usrmsg(' Acoustic category not defined as one',
     &        ' related to Materials or to Constructions. ','W')
            increm=0
            ier=1
            return
          endif
          call edisp(iuout,' acoustic db...')
        elseif(dbtag(idbh)(1:12).eq.'colourimetry')then
          increm=7
          lastr(idbh)=lastr(idbh)+increm
          nbitem(idbh)=nbitem(idbh)+1
          iit=nbitem(idbh)
          irsit(iit)=IREC
          ptag = itemtag('colour',iit)
          ctype='STD'
          call instanitm(db,ctype,iit,ier)
          call edisp(iuout,' colourimetry db...')
        elseif(dbtag(idbh)(1:9).eq.'micotoxin')then
          call usrmsg(
     &      'Micotoxin database not implemented.',' ','W')
          ier=1
          return
        elseif(dbtag(idbh)(1:9).eq.'envir_imp')then
          if(cattype(icl)(1:7).eq.'CONSIMP')irpt=1
          if(cattype(icl)(1:8).eq.'CNSTRASM')irpt=2
          if(cattype(icl)(1:8).eq.'CNSTRMNT')irpt=3
          if(cattype(icl)(1:6).eq.'ASMPRJ')irpt=4
          if(cattype(icl)(1:6).eq.'MATLCA')irpt=5
          if(cattype(icl)(1:6).eq.'RECYCP')irpt=6
          if(cattype(icl)(1:6).eq.'INCENP')irpt=7
          if(cattype(icl)(1:5).eq.'DUMPP')irpt=8
          if(irpt.eq.1)then
            ptag = itemtag('cnstrimp',iit)
            increm=8
            lastr(idbh)=lastr(idbh)+increm
            nbitem(idbh)=nbitem(idbh)+1
            iit=nbitem(idbh)
            irsit(iit)=IREC
            itcat(iit)=1
            ctype='CONSIMP'
            call instanitm(db,ctype,iit,ier)
          elseif(irpt.eq.2)then
            ptag = itemtag('cnstrasmb',iit)
            increm=8
            lastr(idbh)=lastr(idbh)+increm
            nbitem(idbh)=nbitem(idbh)+1
            iit=nbitem(idbh)
            irsit(iit)=IREC
            itcat(iit)=2
            ctype='CNSTRASM'
            call instanitm(db,ctype,iit,ier)
          elseif(irpt.eq.3)then
            ptag = itemtag('cnstrmnt',iit)
            increm=8
            lastr(idbh)=lastr(idbh)+increm
            nbitem(idbh)=nbitem(idbh)+1
            iit=nbitem(idbh)
            irsit(iit)=IREC
            itcat(iit)=3
            ctype='CNSTRMNT'
            call instanitm(db,ctype,iit,ier)
          elseif(irpt.eq.4)then
            ptag = itemtag('asmbprj',iit)
            increm=8
            lastr(idbh)=lastr(idbh)+increm
            nbitem(idbh)=nbitem(idbh)+1
            iit=nbitem(idbh)
            irsit(iit)=IREC
            itcat(iit)=4
            ctype='ASMPRJ'
            call instanitm(db,ctype,iit,ier)
          elseif(irpt.eq.5)then
            ptag = itemtag('matlca',iit)
            increm=8
            lastr(idbh)=lastr(idbh)+increm
            nbitem(idbh)=nbitem(idbh)+1
            iit=nbitem(idbh)
            irsit(iit)=IREC
            itcat(iit)=5
            ctype='MATLCA'
            call instanitm(db,ctype,iit,ier)
          elseif(irpt.eq.6)then
            ptag = itemtag('recyc',iit)
            increm=7
            lastr(idbh)=lastr(idbh)+increm
            nbitem(idbh)=nbitem(idbh)+1
            iit=nbitem(idbh)
            irsit(iit)=IREC
            itcat(iit)=6
            ctype='RECYCP'
            call instanitm(db,ctype,iit,ier)
          elseif(irpt.eq.7)then
            ptag = itemtag('incin',iit)
            increm=7
            lastr(idbh)=lastr(idbh)+increm
            nbitem(idbh)=nbitem(idbh)+1
            iit=nbitem(idbh)
            irsit(iit)=IREC
            itcat(iit)=7
            ctype='INCENP'
            call instanitm(db,ctype,iit,ier)
          elseif(irpt.eq.8)then
            ptag = itemtag('dump',iit)
            increm=7
            lastr(idbh)=lastr(idbh)+increm
            nbitem(idbh)=nbitem(idbh)+1
            iit=nbitem(idbh)
            irsit(iit)=IREC
            itcat(iit)=8
            ctype='DUMPP'
            call instanitm(db,ctype,iit,ier)
          endif
        elseif(dbtag(idbh)(1:9).eq.'transport')then
          increm=6
          lastr(idbh)=lastr(idbh)+increm
          nbitem(idbh)=nbitem(idbh)+1
          iit=nbitem(idbh)
          irsit(iit)=IREC
          ptag = itemtag('transp',iit)
          ctype='STD'
          call instanitm(db,ctype,iit,ier)
          call edisp(iuout,' transport db...')
        elseif(dbtag(idbh)(1:9).eq.'pressc_db')then
          increm=7
          lastr(idbh)=lastr(idbh)+increm
          nbitem(idbh)=nbitem(idbh)+1
          iit=nbitem(idbh)
          irsit(iit)=IREC
          ptag = itemtag('pressc',iit)
          ctype='STD'
          call instanitm(db,ctype,iit,ier)
          call edisp(iuout,' wind pressure db...')
        elseif(dbtag(idbh)(1:8).eq.'profiles')then
          increm=45
          lastr(idbh)=lastr(idbh)+increm
          nbitem(idbh)=nbitem(idbh)+1
          iit=nbitem(idbh)
          irsit(iit)=IREC
          ptag = itemtag('profile',iit)
          ctype='STD'
          call instanitm(db,ctype,iit,ier)
          call edisp(iuout,' profile db...')
        else
          call usrmsg('Exiting, database type not recognised.',' ','W')
          ier=1
          return
        endif

C If default id name is unique then use it otherwise make unique.
        call uniqtag(idbh,ptag,'i',ok)
        if(ok)then
          write(ittag(iit),'(a)') ptag
        else
          if(lnblnk(ptag).lt.12)then
            write(ittag(iit),'(2a)') ptag(1:lnblnk(ptag)),'x'
          else
            write(ittag(iit),'(2a)') ptag(1:11),'x'
          endif
        endif
        write(itmenu(iit),'(2a)') 'new item ',ptag
        call dstamp(dstmp)
        write(itdate(iit),'(a)') dstmp
        if(dbtag(idbh)(1:11).eq.'multicon_db')then
          write(itdoc,'(3a)') 'construction ',
     &      itmenu(iit)(1:lnblnk(itmenu(iit))),' is...'
        elseif(dbtag(idbh)(1:8).eq.'material')then
          write(itdoc,'(3a)') 'material ',
     &      itmenu(iit)(1:lnblnk(itmenu(iit))),' is...'
        elseif(dbtag(idbh)(1:7).eq.'optical')then
          write(itdoc,'(3a)') 'optical ',
     &      itmenu(iit)(1:lnblnk(itmenu(iit))),' is...'
        elseif(dbtag(idbh)(1:8).eq.'acoustic')then
          write(itdoc,'(3a)') 'acoustic ',
     &      itmenu(iit)(1:lnblnk(itmenu(iit))),' is...'
        elseif(dbtag(idbh)(1:12).eq.'colourimetry')then
          write(itdoc,'(3a)') 'colourimetry ',
     &      itmenu(iit)(1:lnblnk(itmenu(iit))),' is...'
        elseif(dbtag(idbh)(1:9).eq.'micotoxin')then
          write(itdoc,'(3a)') 'micotoxin ',
     &      itmenu(iit)(1:lnblnk(itmenu(iit))),' is...'
        elseif(dbtag(idbh)(1:9).eq.'envir_imp')then
          write(itdoc,'(3a)') 'environmental impact ',
     &      itmenu(iit)(1:lnblnk(itmenu(iit))),' is...'
        elseif(dbtag(idbh)(1:9).eq.'transport')then
          write(itdoc,'(3a)') 'transport ',
     &      itmenu(iit)(1:lnblnk(itmenu(iit))),' is...'
        elseif(dbtag(idbh)(1:8).eq.'profiles')then
          write(itdoc,'(3a)') 'profile ',
     &      itmenu(iit)(1:lnblnk(itmenu(iit))),' is...'
        else
          write(itdoc,'(3a)') ': ',
     &      itmenu(iit)(1:lnblnk(itmenu(iit))),' is...'
        endif
        itcat(iit)=icl

C Update the database with pointers to items, update db and category dates
C and number of db items and catg associated items, then write out the new
C item data. 
        call updirsit(IFB,idbh,'W')
        call dstamp(dstmp)
        c20='*ESP-r binary db    '
        irec=1
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c20,dbtag(idbh),
     &    dbmenu(idbh),idbwidth(idbh),nbunit(idbh),nbsrc(idbh),
     &    nbcat(idbh),nbitem(idbh),nbdbdoc(idbh),lastr(idbh)
        write(dbdate(idbh),'(a)') dstmp
        IREC=2
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)dbdate(idbh),dbnote(idbh)
        write(catdate(icl),'(a)') dstmp
        nbcatit(icl)=nbcatit(icl)+1
        IREC=irscat(icl)
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(icl),cattype(icl),
     &    catmenu(icl),nbcatdoc(icl),nbcatit(icl),catdate(icl)
        call wgenitem(IFB,idbh,iit,ier)
      else
        call usrmsg('Database is full, unable to add items.',' ','W')
        ier=1
        return
      endif
      return

   1  write(outs,'(a,I6)') 'newgitem: Error writing record ',IREC
      call edisp(iuout,outs)
      ier=1
      CALL ERPFREE(IFB,IOS)
      return
      end

C ********* instanitm
C instanitm: Instanciate common blocks gendb8b and gendb8c for a new item 
C (index iit) for a given database type (db). These data are
C are the same whether the item is to be inserted in a binary
C db or used in the creation of a new minimal db. Used by 
C newgitem and agdbnew.
      subroutine instanitm(db,type,iit,ier)
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons: file log, list of db, current classifications,
C and current items then lengths of each list.
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8b/itdat(20),itrep(20)
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

      character db*12,type*8
      character itdat*248,itrep*248,itdate*24,itdoc*248
      character ituntstr*42,itsrcstr*1,itcnfstr*42

      if(db(1:11).eq.'multicon_db')then
        nbitusc(iit)=23
        ibitusc(iit)=0
        ituntstr='-------cccc------b-----' 
        itsrcstr='-'
        itcnfstr='-----------------------'
        nbitdat(iit)=1
        nbitrep(iit)=1
        write(itdat(1),'(2a)',IOSTAT=IOS,ERR=1)
     &    'same,grey50,grey50,OPAQ,OPAQUE,none,none,',
     &    '0.900,0.900,0.500,0.500,none,1'
        itrep(1)='t,s,mat1,hgth1,0.2,1,0.00,0.00,0.00,initial material'
      elseif(db(1:8).eq.'material')then
        nbitusc(iit)=17
        ibitusc(iit)=0
        ituntstr='-------abc-------' 
        itsrcstr='-'
        itcnfstr='-----------------'
        nbitdat(iit)=1
        nbitrep(iit)=0
        itdat(1)=' '
        write(itdat(1),'(a,i3,a)',IOSTAT=IOS,ERR=1)
     &    'none,none,none,-,0.000,0.000,',
     &    iit,',1.0,10.0,10.0,0.8,0.5,10.0,0.0,0.0,0.0,0.0'
      elseif(db(1:7).eq.'optical')then
        call edisp(iuout,' optical db... << not yet >>')
      elseif(db(1:8).eq.'acoustic')then
        nbitusc(iit)=2
        ibitusc(iit)=1
        ituntstr='-a' 
        itsrcstr='-'
        itcnfstr='--'
        nbitdat(iit)=1
        nbitrep(iit)=0
        if(type(1:3).eq.'MAT')then
          write(itdat(1),'(2a)',IOSTAT=IOS,ERR=1)
     &      'M,0.16,0.15,0.14,0.12,0.11,0.11,0.10,',
     &      '0.10,0.09,0.08,0.07,0.07,0.06,0.06,0.06,0.06,0.06'
        elseif(type(1:3).eq.'SYS')then
          write(itdat(1),'(2a)',IOSTAT=IOS,ERR=1)
     &      'C,0.40,0.46,0.39,0.31,0.24,0.17,0.11,',
     &      '0.04,0.03,0.02,0.01,0.01,0.00,0.00,0.06,0.01,0.01'
        endif
      elseif(db(1:12).eq.'colourimetry')then
        nbitusc(iit)=6
        ibitusc(iit)=0
        ituntstr='aaaa--' 
        itsrcstr='-'
        itcnfstr='------'
        nbitdat(iit)=1
        nbitrep(iit)=0
        itdat(1)='128,128,128,0.5,none,none'
      elseif(db(1:9).eq.'envir_imp')then
        ibitusc(iit)=1
        if(type(1:7).eq.'CONSIMP')then

C Constructions effects:
          nbitusc(iit)=15
          ituntstr='---aa-aa-aa----' 
          itsrcstr='-'
          itcnfstr='---------------'
          nbitdat(iit)=1
          nbitrep(iit)=0
          write(itdat(1),'(2a)',IOSTAT=IOS,ERR=1)
     &      'none,none,none,0.000,0.000,none,0.000,0.000,',
     &      'none,0.000,0.000,none,none,none,none'
        elseif(type(1:8).eq.'CNSTRASM')then

C Construction assembly: surf-perim, future use, 10 impacts. 
          nbitusc(iit)=12
          ituntstr='--cdefcccccc' 
          itsrcstr='-'
          itcnfstr='------------'
          nbitdat(iit)=1
          nbitrep(iit)=0
          write(itdat(1),'(2a)',IOSTAT=IOS,ERR=1)
     &      's,0.000,',
     &    '0.000,0.000,0.000,0.000,0.000,0.00,0.000,0.000,0.000,0.000'
        elseif(type(1:8).eq.'CNSTRMNT')then

C Construction maintenence: tag for perim or surf, period, 10 impacts. 
          nbitusc(iit)=12
          ituntstr='-acdefcccccc' 
          itsrcstr='-'
          itcnfstr='------------'
          nbitdat(iit)=1
          nbitrep(iit)=0
          write(itdat(1),'(2a)',IOSTAT=IOS,ERR=1)
     &      's,1.000,',
     &   '0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000'
        elseif(type(1:6).eq.'ASMPRJ')then

C Assembly on project: surf-perim-all tag, breakage rate, 10 impacts. 
          nbitusc(iit)=12
          ituntstr='--cdefcccccc' 
          itsrcstr='-'
          itcnfstr='------------'
          nbitdat(iit)=1
          nbitrep(iit)=0
          write(itdat(1),'(2a)',IOSTAT=IOS,ERR=1)
     &      'a,0.000,',
     &   '0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000'
        elseif(type(1:6).eq.'MATLCA')then

C Material effects: material life, fabrication effects (in itdat(3)),
C 3 trns name > assembly site, 3 dist > site, 3 breakage > site
C recyc name & rate, incineration name & rate, dump name & rate. 
          nbitusc(iit)=21
          ituntstr='a--b--b--b-------cdef' 
          itsrcstr='-'
          itcnfstr='---------------------'
          nbitdat(iit)=1
          nbitrep(iit)=0
          write(itdat(1),'(3a)',IOSTAT=IOS,ERR=1)
     &      '10.00,0.100,none,0.00,0.000,none,0.00,0.000,',
     &      'none,0.00,0.000,none,0.000,none,0.000,none,0.000,',
     &   '0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000'
        elseif(type(1:6).eq.'RECYCP')then

C Recycling: 3 transport names and distances + 10 impacts. 
          nbitusc(iit)=16
          ituntstr='-b-b-bcdefcccccc' 
          itsrcstr='-'
          itcnfstr='----------------'
          nbitdat(iit)=1
          nbitrep(iit)=0
          write(itdat(1),'(2a)',IOSTAT=IOS,ERR=1)
     &      'none,0.000,none,0.000,none,0.000,',
     &    '0.000,0.000,0.000,0.000,0.000,0.00,0.000,0.000,0.000,0.000'
        elseif(type(1:6).eq.'INCENP')then

C Incineration: 3 transport names and distances + 10 impacts. 
          nbitusc(iit)=16
          ituntstr='-b-b-bcdefcccccc' 
          itsrcstr='-'
          itcnfstr='----------------'
          nbitdat(iit)=1
          nbitrep(iit)=0
          write(itdat(1),'(2a)',IOSTAT=IOS,ERR=1)
     &      'none,0.000,none,0.000,none,0.000,',
     &    '0.000,0.000,0.000,0.000,0.000,0.00,0.000,0.000,0.000,0.000'
        elseif(type(1:5).eq.'DUMPP')then

C Dump: 3 transport names and distances + 10 impacts. 
          nbitusc(iit)=16
          ituntstr='-b-b-bcdefcccccc' 
          itsrcstr='-'
          itcnfstr='----------------'
          nbitdat(iit)=1
          nbitrep(iit)=0
          write(itdat(1),'(2a)',IOSTAT=IOS,ERR=1)
     &      'none,0.000,none,0.000,none,0.000,',
     &    '0.000,0.000,0.000,0.000,0.000,0.00,0.000,0.000,0.000,0.000'
        endif
      elseif(db(1:9).eq.'transport')then
        nbitusc(iit)=10
        ibitusc(iit)=0
        ituntstr='abcd------' 
        itsrcstr='-'
        itcnfstr='----------'
        nbitdat(iit)=1
        nbitrep(iit)=0
        itdat(1)=
     &    '0.000,0.000,0.000,0.00,0.000,0.000,0.000,0.00,0.000,0.00'
      elseif(db(1:9).eq.'pressc_db')then
        nbitusc(iit)=16
        ibitusc(iit)=1
        ituntstr='----------------' 
        itsrcstr='a'
        itcnfstr='----------------'
        nbitdat(iit)=1
        nbitrep(iit)=0
        write(itdat(1),'(2a)',IOSTAT=IOS,ERR=1)
     &    '0.70,0.525,0.350,-0.075,-0.500,-0.450,-0.40,-0.300,',
     &    '-0.20,-0.30,-0.40,-0.450,-0.500,-0.075,0.350,0.525'
      elseif(db(1:8).eq.'profiles')then
        if(type(1:6).eq.'CASUAL')then
          nbitusc(iit)=17
          ibitusc(iit)=1
          ituntstr='--------------a--' 
          itsrcstr='-'
          itcnfstr='-----------------'
          nbitdat(iit)=1
          nbitrep(iit)=2
          itdat(1)='3,10,1,1,1,0,0,0,0,0,0,0'
          write(itrep(1),'(3a)',IOSTAT=IOS,ERR=1)
     & 'weekday,none,0.00,100.0,100.0,saturday,none,0.00,100.0,100.0,',
     & 'sunday,none,0.00,100.0,100.0,all,none,0.00,0.00,0.00,',
     & 'all,none,0.00,0.00,0.00'
          write(itrep(2),'(3a)',IOSTAT=IOS,ERR=1)
     & 'all,none,0.00,0.00,0.00,all,none,0.00,0.00,0.00,',
     & 'all,none,0.00,0.00,0.00,all,none,0.00,0.00,0.00,',
     & 'all,none,0.00,0.00,0.00,'
        endif
      else
        call usrmsg('Exit instanitm, database not recognised.',' ','W')
        ier=1
        return
      endif
      return

  1   call edisp(iuout,'INSTANITM: error composing data line.')
      ier=1
      return 
      end

C ************* AGDBNEW
C AGDBNEW: Create a minimal boilerplate ascii generic database which can be read
C in and converted to a new binary db file. Act is << >>. Db is the
C type of database to create, idbh is its position in the hash table.
      subroutine agdbnew(IFA,AFIL,db,idbh,act,IER)
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons: file log, list of db, current classifications,
C and current items then lengths of each list.
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendbu/unitchr(24),unitstr(24),unitnote(24)
      common/gendbs/srcchr(24),srcnote(24)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8b/itdat(20),itrep(20)
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

      character AFIL*72,tab*1,dstmp*24,act*1,db*12,ctype*8
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character itdat*248,itrep*248,ittag*12,itmenu*32,itdate*24
      character itdoc*248
      character dbnote*72,c20*20,ituntstr*42,itsrcstr*1,itcnfstr*42
      character unitchr*4,unitstr*12,unitnote*72,srcchr*4,srcnote*248

C Setup a tab character.
C      tab=CHAR(9)
      tab=','

C Open the ascii file, clear db commons and initialise (via clrgdbc).
      CALL EFOPSEQ(IFA,AFIL,4,IER)
      if(ier.ne.0)return
      call clrgdbc(idbh)
      call dstamp(dstmp)
      c20='*ESP-r ascii gdb    '
      nbdbdoc(idbh)=1
      write(dbdoc(1),'(2a)') ': New generic database (from scratch) ',
     &  'Documentation follows: '
      idbwidth(idbh)=65
      nbcat(idbh)=1
      cattag(1)='Initial'
      catmenu(1)='Initial category'
      nbcatdoc(1)=1
      catdoc(1)=': Initial category documentation follows: '
      nbitem(idbh)=1
      itcat(1)=1
      write(dbdate(idbh),'(a)') dstmp

C The pattern is that there is one item in each category created
C and that with few exceptions items are of type 'STD '. Currently
C we need to loop MGDB times to fill cattype & itcat. Exceptions are
C handled as required later in the code.
      do 19 i=1,MGDB
        cattype(i)='STD'
        itcat(i)=i
  19  continue

      if(db(1:11).eq.'multicon_db')then

C Create basic constructions database with initial documentation
C and one category with one item.
        dbtag(idbh)='multicon_db'
        dbmenu(idbh)='Constructions db'
        dbnote(idbh)='Constructions db log...'
        call intdbdoc(idbh,'n',afil,ier)
        nbsrc(idbh)=1
        nbunit(idbh)=3
        unitstr(1)='(-)'
        unitnote(1)='(-)'
        unitstr(2)='(km)'
        unitnote(2)='(distance in km)'
        unitstr(3)='(m)'
        unitnote(3)='(thickness in m)'
        nbcat(idbh)=8
        nbitem(idbh)=8

        cattag(1)='ext_walls'
        catmenu(1)='External walls'
        ittag(1)='def_extwall'
        itmenu(1)='default external wall'

        cattag(2)='int_walls'
        catmenu(2)='Internal walls & partitions'
        ittag(2)='def_intwall'
        itmenu(2)='default partition'

        cattag(3)='floor'
        catmenu(3)='Floors (all types)'
        ittag(3)='def_floor'
        itmenu(3)='default floor'

        cattag(4)='foundation'
        catmenu(4)='Foundations (base & walls)'
        ittag(4)='def_fndt'
        itmenu(4)='default foundation'

        cattag(5)='ceiling'
        catmenu(5)='Ceilings'
        ittag(5)='def_ceil'
        itmenu(5)='default ceiling'

        cattag(6)='roof'
        catmenu(6)='Roofs (flat & sloped)'
        ittag(6)='def_roof'
        itmenu(6)='default roof'

        cattag(7)='glazing'
        catmenu(7)='Glazing and frames'
        ittag(7)='def_glaz'
        itmenu(7)='default glazing'

        cattag(8)='furniture'
        catmenu(8)='furniture & fittings'
        ittag(8)='def_desk'
        itmenu(8)='default desk'
      elseif(db(1:8).eq.'material')then

C Create basic material (list) database.
        dbtag(idbh)='material'
        dbmenu(idbh)='Materials (list) db'
        dbnote(idbh)='Materials db log...'
        call intdbdoc(idbh,'n',afil,ier)
        nbsrc(idbh)=1
        nbunit(idbh)=4
        unitstr(1)='(-)'
        unitnote(1)='(-)'
        unitstr(2)='W/(m deg.C)'
        unitnote(2)='Conduc W/(m deg.C)'
        unitstr(3)='kg/m**3'
        unitnote(3)='Density kg/m**3'
        unitstr(4)='J/(kg deg.C)'
        unitnote(4)='Specific Heat J/(kg deg.C)'
        nbcat(idbh)=13
        nbitem(idbh)=13

C Common categories for material database.
        cattag(1)='gasses'
        cattype(1)='GAS'
        catmenu(1)='Gasses (for air gaps)'
        ittag(1)='mat0'
        itmenu(1)='air (@ 25C)'

        cattag(2)='brick'
        catmenu(2)='Brick (ceramic & concrete)'
        ittag(2)='def_brick'
        itmenu(2)='default brick'

        cattag(3)='concrete'
        catmenu(3)='Concrete (cast & formed)'
        ittag(3)='def_concr'
        itmenu(3)='default concrete'

        cattag(4)='metal'
        catmenu(4)='Metals (ferrous & non-ferrous)'
        ittag(4)='def_metal'
        itmenu(4)='default metal'

        cattag(5)='wood'
        catmenu(5)='Wood (hard & soft & ply)'
        ittag(5)='def_wood'
        itmenu(5)='default wood'

        cattag(6)='stone'
        catmenu(6)='Stone'
        ittag(6)='def_stone'
        itmenu(6)='default stone'

        cattag(7)='earth'
        catmenu(7)='Earth & gravel'
        ittag(7)='def_earth'
        itmenu(7)='default earth'

        cattag(8)='glass'
        catmenu(8)='Glass (float & block & coatings)'
        ittag(8)='def_glass'
        itmenu(8)='default glass'

        cattag(9)='tile'
        catmenu(9)='Tiles (floor & ceiling & wall)'
        ittag(9)='def_tile'
        itmenu(9)='default tile'

        cattag(10)='plaster'
        catmenu(10)='plaster & renders'
        ittag(10)='def_plaster'
        itmenu(10)='default plaster'

        cattag(11)='insulation'
        catmenu(11)='Insulation'
        ittag(11)='def_insul'
        itmenu(11)='default insulation'

        cattag(12)='waterproof'
        catmenu(12)='Waterproofing'
        ittag(12)='def_dpc'
        itmenu(12)='default damp proof course'

        cattag(13)='carpet'
        catmenu(13)='Carpet & fabric'
        ittag(13)='def_carpet'
        itmenu(13)='default carpet'
      elseif(db(1:7).eq.'optical')then

C Create basic optical database with initial documentation
C and a category with one item.
        call edisp(iuout,' Creating ascii optical db...')
        dbtag(idbh)='optical'
        dbmenu(idbh)='Optical properties db'
        dbnote(idbh)='Optical properties db log...'
        ier = 1
        return
      elseif(db(1:8).eq.'acoustic')then

C Create basic optical database with initial documentation
C and two categories, each with one item. Note, item details
C are instanciated just before write statement.
        dbtag(idbh)='acoustic'
        dbmenu(idbh)='Acoustic properties db'
        dbnote(idbh)='Acoustic properties db log...'
        call intdbdoc(idbh,'n',afil,ier)
        nbsrc(idbh)=1
        nbunit(idbh)=4
        unitstr(1)='(-)'
        unitnote(1)='(-)'
        unitstr(2)='(m^2)'
        unitnote(2)='(per m^2 of surface area)'
        unitstr(3)='(person)'
        unitnote(3)='(per person)'
        unitstr(4)='(1000 m^3)'
        unitnote(4)='(per 1000 m^3 space volume)'
        nbcat(idbh)=2
        cattag(1)='mat_acou'
        cattype(1)='MAT '
        catmenu(1)='Initial material category'
        nbcatdoc(1)=1
        catdoc(1)=': Category material documentation follows: '
        cattag(2)='sys_acou'
        cattype(2)='SYS '
        catmenu(2)='Initial construction category'
        nbitem(idbh)=2

        ittag(1)='Macou1'
        ittag(2)='Cacou2'
        itmenu(1)='initial material acoustic'
        itmenu(2)='initial construction acoustic'
        ctype='MAT '
        call instanitm(db,ctype,1,ier)
      elseif(db(1:12).eq.'colourimetry')then

C Create basic colourimetry database with initial documentation
C and a category with one item.
        dbtag(idbh)='colourimetry'
        dbmenu(idbh)='Colourimetry db'
        dbnote(idbh)='Colourimetry db log...'
        call intdbdoc(idbh,'n',afil,ier)
        nbsrc(idbh)=1
        nbunit(idbh)=1
        unitstr(1)='(-)'
        unitnote(1)='(-)'
        ittag(1)='grey50'
        itmenu(1)='initial colour 50% refl'
        ctype='STD'
        call instanitm(db,ctype,1,ier)
        itdoc='The colour (grey50) is...'
      elseif(db(1:9).eq.'envir_imp')then

C For environmental effacts setup each of the 8 categories with one
C item in each.
        dbtag(idbh)='envir_imp'
        dbmenu(idbh)='Environmental effects db'
        dbnote(idbh)='Environmental effects db log...'
        call intdbdoc(idbh,'n',afil,ier)
        nbsrc(idbh)=5
        srcnote(2)=
     &   'Okoinventaire und Wirkungsbilanzen von Baumaterialen-1995'
        srcnote(3)=
     &   'Okoinventaire fur Energiesysteme-1997'
        srcnote(4)=
     &'Energie und Stoffflussbil. von Gebauden wahrend ihrer Lebensd 94'
        srcnote(5)=
     &'Okolo. Bewertung von Fensterkonstr. verschiedener Rahmen-mat 96'
        nbunit(idbh)=7
        unitstr(1)='(-)'
        unitnote(1)='(-)'
        unitstr(2)='(yr)'
        unitnote(2)='(year)'
        unitstr(3)='(km)'
        unitnote(3)='(distance in km)'
        unitstr(4)='kg eqCO2/kg*'
        unitnote(4)='kg of equivalent CO2 emitted per kg of mat'
        unitstr(5)='kg eqSO2/kg*'
        unitnote(5)='kg of equivalent SO2 emitted per kg of mat'
        unitstr(6)='kg eqNOX/kg*'
        unitnote(6)='kg of equivalent NOX emitted per kg of mat'
        unitstr(7)='(MJ/kg*)'
        unitnote(7)='MJ emitted per kg of material'
        nbcat(idbh)=8
        nbitem(idbh)=8

        cattag(1)='constr_imp'
        cattype(1)='CONSIMP'
        catmenu(1)='Impacts at construction level'
        ittag(1)='cnstrimp'
        itmenu(1)='constructions effects 1'

        cattag(2)='constr_asmb'
        cattype(2)='CNSTRASM'
        catmenu(2)='..prefabrication assembly'
        ittag(2)='cnstrasmb1'
        itmenu(2)='construction assembly 1'

        cattag(3)='constr_mnt'
        cattype(3)='CNSTRMNT'
        catmenu(3)='..site maintenence'
        ittag(3)='compmnt1'
        itmenu(3)='construction maintenence 1'

        cattag(4)='asmbly_prj'
        cattype(4)='ASMPRJ'
        catmenu(4)='..site assembly'
        ittag(4)='asmbprj1'
        itmenu(4)='assembly on project 1'

        cattag(5)='mat_imp'
        cattype(5)='MATLCA'
        catmenu(5)='Impacts at material level'
        ittag(5)='matimp1'
        itmenu(5)='material effects 1'

        cattag(6)='recycling'
        cattype(6)='RECYCP'
        catmenu(6)='..recycling processes'
        ittag(6)='recyc1'
        itmenu(6)='recycling process 1'

        cattag(7)='incineration'
        cattype(7)='INCENP'
        catmenu(7)='..incineration processes'
        ittag(7)='incin1'
        itmenu(7)='incineration process 1'

        cattag(8)='dump_proc'
        cattype(8)='DUMPP'
        catmenu(8)='..dump processes'
        ittag(8)='dump1'
        itmenu(8)='dump process 1'
      elseif(db(1:9).eq.'transport')then

C Create basic transport database with initial documentation
C and one category with one item.
        dbtag(idbh)='transport'
        dbmenu(idbh)='Transport effects db'
        dbnote(idbh)='Transport effects db log...'
        call intdbdoc(idbh,'n',afil,ier)
        nbsrc(idbh)=1
        nbunit(idbh)=5
        unitstr(1)='(-)'
        unitnote(1)='(-)'
        unitstr(2)='eqkgCO2/km/t'
        unitnote(2)='kg equivalent CO2/km/t'
        unitstr(3)='eqkgSO2/km/t'
        unitnote(3)='kg equivalent SO2/km/t'
        unitstr(4)='eqkgNOX/km/t'
        unitnote(4)='kg equivalent NOX/km/t'
        unitstr(5)='(MJ/km/t)'
        unitnote(5)='(MJ/km/t)'
        ittag(1)='transp1'
        itmenu(1)='initial transport'
        ctype='STD'
        call instanitm(db,ctype,1,ier)
        itdoc='The transport is...'
      elseif(db(1:9).eq.'pressc_db')then

C Create wind pressure database with initial documentation
C and one category with one item.
        dbtag(idbh)='pressc_db'
        dbmenu(idbh)='Wind pressure db'
        dbnote(idbh)='Wind pressure db log...'
        call intdbdoc(idbh,'n',afil,ier)
        nbsrc(idbh)=2
        srcnote(2)='AIVC: Air Infiltration Calculation Techniques'
        nbunit(idbh)=1
        unitstr(1)='(-)'
        unitnote(1)='(-)'
        ittag(1)='pressc1'
        itmenu(1)='1:1 exposed wall'
        ctype='STD'
        call instanitm(db,ctype,1,ier)
        itdoc='This pressure coef set is...'
      elseif(db(1:8).eq.'profiles')then

C Create basic profiles database with initial documentation
C and one category with one item.
        dbtag(idbh)='profiles'
        dbmenu(idbh)='Event profiles db'
        dbnote(idbh)='Event profiles db log...'
        call intdbdoc(idbh,'n',afil,ier)
        nbsrc(idbh)=1
        nbunit(idbh)=2
        unitstr(1)='(-)'
        unitnote(1)='(-)'
        unitstr(2)='(hr)'
        unitnote(2)='(hours)'
        ittag(1)='profile1'
        itmenu(1)='event profile (profile1) is...'
        ctype='CASUAL'
        call instanitm(db,ctype,1,ier)
        itdoc='This profile is...'
      else
        call edisp(iuout,' unknown db...')
        ier=1
        return
      endif

      write(IFA,'(a)')c20
      write(IFA,'(a)')
     &  '# db type, menu, width,nb of units:sources:cat:items, date'

      write(IFA,'(4a,i2,a,i2,a,i2,a,i2,a,i3,2a)')
     &  dbtag(idbh)(1:lnblnk(dbtag(idbh))),tab,
     &  dbmenu(idbh)(1:lnblnk(dbmenu(idbh))),tab,
     &  idbwidth(idbh),tab,nbunit(idbh),tab,nbsrc(idbh),tab,
     &  nbcat(idbh),tab,nbitem(idbh),tab,dbdate(idbh)

      write(IFA,'(a,a1,a)')'*DBNOTE',tab,
     &  dbnote(idbh)(1:lnblnk(dbnote(idbh)))

C Write out Units and Sources.
      if(nbunit(idbh).gt.0)then
        do 4 inbu=1,nbunit(idbh)
          write(IFA,'(7a)')'*UNIT',tab,unitchr(inbu)(1:1),tab,
     &      unitstr(inbu)(1:lnblnk(unitstr(inbu))),tab,
     &      unitnote(inbu)(1:lnblnk(unitnote(inbu)))
  4     continue
      endif

      if(nbsrc(idbh).gt.0)then
        do 5 inbsrc=1,nbsrc(idbh)
          write(IFA,'(5a)')'*SOURCE',tab,srcchr(inbsrc)(1:1),tab,
     &      srcnote(inbsrc)(1:lnblnk(srcnote(inbsrc)))
  5     continue
      endif

C Write out the db documentation records.
      write(IFA,'(a)')'# db level documentation'
      do 51 i=1,nbdbdoc(idbh)
        write(IFA,'(2a)') '*DD ',dbdoc(i)(1:lnblnk(dbdoc(i)))
  51  continue

C Write category data, at this point instanciate catdate and
C that each cateory has one item (nbcatit=1)
      do 42 icl=1,nbcat(idbh)
        write(catdate(icl),'(a)') dstmp
        nbcatit(icl) = 1
        write(IFA,'(a)')'# CAT tag,id,type,menu,items_in_cat,date'
        write(IFA,'(8a,i2,2a)')'*CATEG',tab,
     &    cattag(icl)(1:lnblnk(cattag(icl))),tab,
     &    cattype(icl)(1:lnblnk(cattype(icl))),tab,
     &    catmenu(icl)(1:lnblnk(catmenu(icl))),tab,
     &    nbcatit(icl),tab,catdate(icl)

C If this category documentation to be updated then use bkdoc text.
        write(IFA,'(a)')'# cat documentation'

C In case of constructions write 8 categories.
        if(dbtag(idbh)(1:11).eq.'multicon_db')then
          nbcatdoc(icl)=1
          if(icl.eq.1)then
            write(catdoc(1),'(2a)')
     &    'The category external wall is for all types ',
     &    'of walls used in an exterior context.'
          elseif(icl.eq.2)then
            write(catdoc(1),'(2a)')
     &    'The category partition is for all types of ',
     &    'walls used within a building or as partitions.'
          elseif(icl.eq.3)then
            write(catdoc(1),'(2a)')
     &    'The category floors is for horizontal ',
     &    'constructions at ground or between levels.'
          elseif(icl.eq.4)then
            write(catdoc(1),'(2a)')
     &    'The category foundations is for floors and ',
     &    'walls at ground or below ground level.'
          elseif(icl.eq.5)then
            write(catdoc(1),'(2a)')
     &     'The category ceiling is for structural or ',
     &     'suspended ceilings or combined roof spaces.'
          elseif(icl.eq.6)then
            write(catdoc(1),'(2a)')
     &     'The category roof is for flat or sloped ',
     &     'roofs or combined roof spaces.'
          elseif(icl.eq.7)then
            write(catdoc(1),'(2a)')
     &     'The category glazing is for single, double ',
     &     'or tripple systems, with or without blinds.'
          elseif(icl.eq.8)then
            write(catdoc(1),'(2a)')
     &     'The category furniture is for items associated ',
     &     'with thermal mass within a zone.'
          endif
        endif

C Environmental impacts.
        if(dbtag(idbh)(1:9).eq.'envir_imp')then
          if(icl.eq.1)then
            write(catdoc(1),'(3a)')
     & 'The constructions impacts category includes: name of assembly',
     & ' effects (for surface), name of assembly effects (for ',
     & 'perimeter),'
            write(catdoc(2),'(2a)')
     & '1st transport > building site name, distance > building site ',
     & '(1st), loss rate > building site (1st),'
            write(catdoc(3),'(2a)')
     & '2nd transport > building site name, distance > building site ',
     & '(2nd), loss rate > building site (2nd),'
            write(catdoc(4),'(2a)')
     & '3rd transport > building site name, distance > building site ',
     & '(3rd), loss rate > building site (3rd),'
            write(catdoc(5),'(3a)')
     & 'name of assembly at building site (1st), name of assembly at ',
     & 'building site (2nd), maintenance name (1st), maintenance name',
     & ' (2nd).'
            nbcatdoc(icl)=5
          elseif(icl.eq.2)then
            write(catdoc(1),'(2a)')
     & 'The constructions assembly category includes: a) tag ',
     & 'indicating perimeter or surface or all.'
            write(catdoc(2),'(4a)')
     & 'b) loss rate during assembly (0.0-1.0), c) global warming ',
     & 'potential [kg eqCO2/kg*] d) acidification potential [kg ',
     & 'eqNOx/kg*] e) photo-smog potential [kg eqSOx/kg*] f) ',
     & 'nonrenewable energy [MJ/kg*].'
            nbcatdoc(icl)=2
          elseif(icl.eq.3)then
            write(catdoc(1),'(2a)')
     & 'The maintenance category includes: a) tag indicating ',
     & 'perimeter or surface or all.'
            write(catdoc(2),'(4a)')
     & 'b) maintenance period (years) c) global warming potential ',
     & '[kg eqCO2/kg*] d) acidification potential [kg eqNOx/kg*] ',
     & 'e) photo-smog potential [kg eqSOx/kg*] f) nonrenewable ',
     & 'energy [MJ/kg*].'
            nbcatdoc(icl)=2
          elseif(icl.eq.4)then
            write(catdoc(1),'(3a)')
     & 'Assembly on project data can be referenced by prefabricated ',
     & 'constructions and includes: a) tag indicating perimeter or ',
     & 'surface or all.'
            write(catdoc(2),'(4a)')
     & 'b) breakage rate during assembly (0.0-1.0) c) global warming ',
     & 'potential [kg eqCO2/kg*] d) acidification potential [kg ',
     & 'eqNOx/kg*] e) photo-smog potential [kg eqSOx/kg*] f) ',
     & 'nonrenewable energy [MJ/kg*].'
            nbcatdoc(icl)=2
          elseif(icl.eq.5)then
            write(catdoc(1),'(a)')
     & 'The material effects category includes:'
            write(catdoc(2),'(4a)')
     & 'c) material life (years) d) global warming potential [kg ',
     & 'eqCO2/kg*] e) acidification potential [kg eqNOx/kg*] f) ',
     & 'photo-smog potential [kg eqSOx/kg*] g) nonrenewable energy ',
     & '[MJ/kg*],'
            write(catdoc(3),'(4a)')
     & 'h) breakage rate during assembly process i) 1st means of ',
     & 'transport to assembly site j) 1st distance traveled [km] k) ',
     & '1st breakage rate (0.0-1.0) l) 2nd means of transport to ',
     & 'assembly site, '
            write(catdoc(4),'(3a)')
     & 'm) 2nd distance traveled [km] n) 2nd breakage rate (0.0-1.0)',
     & ' o) 3rd means of transport to assembly site p) 3rd distance ',
     & 'traveled [km] q) 3rd breakage rate (0.0-1.0), '
            write(catdoc(5),'(3a)')
     & 'r) recycling process name s) recycling rate (0.0-1.0) t) ',
     & 'incineration process name u) incineration rate (0.0-1.0) ',
     & 'v) dump process name w) dump rate (0.0-1.0).'
            nbcatdoc(icl)=5
          elseif(icl.eq.6)then
            write(catdoc(1),'(2a)')
     & 'The material recycling category holds data for processes ',
     & 'related to material recycling and includes: '
            write(catdoc(2),'(4a)')
     & 'a) name of 1st means of recycling transport b) 1st distance ',
     & 'traveled [km] c) name of 2nd means of transport d) 2nd ',
     & 'distance traveled [km] e) name of 3rd means of transport ',
     & 'f) 3rd distance traveled [km].'
            write(catdoc(3),'(3a)')
     & 'g) global warming potential [kg eqCO2/kg*] h) acidification ',
     & 'potential [kg eqNOx/kg*] i) photo-smog potential [kg ',
     & 'eqSOx/kg*] j) nonrenewable energy potential [MJ/kg*].'
            nbcatdoc(icl)=3
          elseif(icl.eq.7)then
            write(catdoc(1),'(2a)')
     & 'The material incineration category holds data for processes ',
     & 'related to the incineration of materials and includes:'
            write(catdoc(2),'(4a)')
     & 'a) name of 1st means of transport b) 1st distance traveled ',
     & '[km] c) name of 2nd means of transport d) 2nd distance ',
     & 'traveled [km] e) name of 3rd means of transport f) 3rd ',
     & 'distance traveled [km],'
            write(catdoc(3),'(3a)')
     & 'g) global warming potential [kg eqCO2/kg*] h) acidification ',
     & 'potential [kg eqNOx/kg*] i) photo-smog potential [kg ',
     & 'eqSOx/kg*] j) nonrenewable energy potential [MJ/kg*].'
            nbcatdoc(icl)=3
          elseif(icl.eq.8)then
            write(catdoc(1),'(2a)')
     & 'The material dump category holds data for processes related ',
     & 'to the dumping of materials and includes:'
            write(catdoc(2),'(3a)')
     & 'a) name of 1st means of dump transport b) 1st distance ',
     & 'traveled [km] c) name of 2nd means of transport d) 2nd ',
     & 'distance traveled [km] e) name of 3rd means of transport ',
     & 'f) 3rd distance traveled [km], '
            write(catdoc(3),'(3a)')
     & 'g) global warming potential [kg eqCO2/kg*] h) acidification ',
     & 'potential [kg eqNOx/kg*] i) photo-smog potential [kg ',
     & 'eqSOx/kg*] j) nonrenewable energy potential [MJ/kg*].'
            nbcatdoc(icl)=3
          endif
        endif
        do 53 ij=1,nbcatdoc(icl)
          write(IFA,'(2a)') '*CD ',catdoc(ij)(1:lnblnk(catdoc(ij)))
  53    continue

C Comment line for item and then the initial item data and instanciate
C the item date.
        if(idbver(idbh).eq.1)then
          write(IFA,'(a)')
     &      '# item id, menu, data & reptng lines, USC, date'
        else
          write(IFA,'(a)')'# item id, menu, date'
        endif
        do 34 iit=1,nbitem(idbh)
          write(itdate(iit),'(a)') dstmp
          if(itcat(iit).eq.icl)then
            if(idbver(idbh).eq.1)then
              write(IFA,'(6a,i2,a,i2,a,i2,a,i2,2a)')'*ITEM',tab,
     &          ittag(iit)(1:lnblnk(ittag(iit))),tab,
     &          itmenu(iit)(1:lnblnk(itmenu(iit))),tab,
     &          nbitdat(iit),tab,nbitrep(iit),tab,nbitusc(iit),tab,
     &          ibitusc(iit),tab,itdate(iit)(1:lnblnk(itdate(iit)))
            else
              write(IFA,'(5a)')'*ITEM',tab,
     &          ittag(iit)(1:lnblnk(ittag(iit))),tab,
     &          itmenu(iit)(1:lnblnk(itmenu(iit))),tab,
     &          itdate(iit)(1:lnblnk(itdate(iit)))
            endif

C In case of constructions write 8 categories.
            if(dbtag(idbh)(1:11).eq.'multicon_db')then
              ctype='STD'
              if(icl.eq.1)then
                itdoc='This external wall is...'
                call instanitm(db,ctype,1,ier)
                write(itdat(1),'(2a)')
     &            'same,grey50,grey50,OPAQ,OPAQUE,none,none,',
     &            '0.900,0.900,0.500,0.500,none,1'
        itrep(1)='t,s,def_brick,def_brick,0.1,1,0.00,0.00,0.00,brick'
              elseif(icl.eq.2)then
                itdoc='This partition is...'
                call instanitm(db,ctype,2,ier)
                write(itdat(1),'(2a)')
     &            'same,grey50,grey50,OPAQ,OPAQUE,none,none,',
     &            '0.900,0.900,0.500,0.500,none,1'
        itrep(1)='t,s,def_brick,def_brick,0.1,1,0.00,0.00,0.00,brick'
              elseif(icl.eq.3)then
                itdoc='This floor is...'
                call instanitm(db,ctype,3,ier)
                write(itdat(1),'(2a)')
     &            'same,grey50,grey50,OPAQ,OPAQUE,none,none,',
     &            '0.900,0.900,0.500,0.500,none,1'
        itrep(1)='t,s,def_concr,def_concr,0.15,1,0.00,0.0,0.0,concrete'
              elseif(icl.eq.4)then
                itdoc='This foundation is...'
                call instanitm(db,ctype,4,ier)
                write(itdat(1),'(2a)')
     &           'same,grey50,grey50,OPAQ,OPAQUE,none,none,',
     &           '0.900,0.900,0.500,0.500,none,1'
        itrep(1)='t,s,def_concr,def_concr,0.15,1,0.00,0.0,0.0,concrete'
C << DONE THIS FAR >>
              elseif(icl.eq.5)then
                itdoc='This ceiling is...'
                call instanitm(db,ctype,5,ier)
        itdat(1)='same,grey50,grey50,OPAQ,OPAQUE,none,none'
        itdat(2)='0.900,0.900,0.500,0.500,none,1'
        itrep(1)='t,s,def_tile,def_tile,0.015,1,0.00,0.0,0.0,ceil tile'
              elseif(icl.eq.6)then
                itdoc='This roof is...'
                call instanitm(db,ctype,6,ier)
        itdat(1)='same,grey50,grey50,OPAQ,OPAQUE,none,none'
        itdat(2)='0.900,0.900,0.500,0.500,none,1'
        itrep(1)='t,s,def_concr,def_concr,0.15,1,0.00,0.0,0.0,concrete'
              elseif(icl.eq.7)then
                itdoc='This glazing is...'
                call instanitm(db,ctype,7,ier)
        itdat(1)='same,grey50,grey50,TRAN,SC_fictit,none,none'
        itdat(2)='0.900,0.900,0.500,0.500,none,1'
        itrep(1)='t,s,def_glass,def_glass,0.015,1,0.00,0.00,0.00,glass'
              elseif(icl.eq.8)then
                itdoc='This furniture is...'
                call instanitm(db,ctype,8,ier)
        itdat(1)='same,grey50,grey50,OPAQ,OPAQUE,none,none'
        itdat(2)='0.900,0.900,0.500,0.500,none,1'
        itrep(1)='t,s,def_wood,def_wood,0.025,1,0.00,0.0,0.0,wood desk'
              endif
            endif

C In case of materials write MGDB categories. After each general instanciation
C set itdat to values appropriate for the category (approx).
            if(dbtag(idbh)(1:8).eq.'material')then
              ctype='STD'
              if(icl.eq.1)then
                itdoc='The gas air used in air gaps is...'
                call instanitm(db,ctype,1,ier)
                itdat(1)='none,none,none,-,0.000,0.000'
                itdat(2)=
     &'0,0.026,1.177,1004.900,0.010,0.010,1.000,0.0,0.0,0.0,0.0'
             elseif(icl.eq.2)then
                itdoc='This brick is...'
                call instanitm(db,ctype,1,ier)
                itdat(1)='def_brick,none,none,-,0.000,0.000'
                itdat(2)=
     &'1,0.960,2000.000,840.000,0.930,0.700,12.0,0.0,0.0,0.0,0.0,'
             elseif(icl.eq.3)then
                itdoc='This concrete is...'
                call instanitm(db,ctype,2,ier)
                itdat(1)='def_concr,none,none,-,0.000,0.000'
                itdat(2)=
     &'21,0.380,1200.000,653.000,0.900,0.650,6.0,0.0,0.0,0.0,0.0,'
              elseif(icl.eq.4)then
                itdoc='This metal is...'
                call instanitm(db,ctype,3,ier)
                itdat(1)='def_metal,none,none,-,0.000,0.000'
                itdat(2)=
     &'41,200.000,8900.000,418.000,0.720,0.650,19200.0,0.0,0.0,0.0,0.0,'
              elseif(icl.eq.5)then
                itdoc='This wood is...'
                call instanitm(db,ctype,4,ier)
                itdat(1)='def_wood,none,none,-,0.000,0.000'
                itdat(2)=
     &'61,0.160,800.000,2093.000,0.900,0.650,11.000,0.0,0.0,0.0,0.0,'
              elseif(icl.eq.6)then
                itdoc='This stone is...'
                call instanitm(db,ctype,5,ier)
                itdat(1)='def_stone,none,none,-,0.000,0.000'
                itdat(2)=
     &'81,1.830,2200.000,712.000,0.900,0.600,29.000,0.0,0.0,0.0,0.0,'
              elseif(icl.eq.7)then
                itdoc=': This earth is...'
                call instanitm(db,ctype,6,ier)
                itdat(1)='def_earth,none,none,-,0.000,0.000'
                itdat(2)=
     &'261,0.090,480.000,180.000,0.900,0.850,5.000,0.0,0.0,0.0,0.0,'
              elseif(icl.eq.8)then
                itdoc='This glass is...'
                call instanitm(db,ctype,7,ier)
                itdat(1)='def_glass,none,none,-,0.000,0.000'
                itdat(2)=
     &'242,0.760,2710.000,837.000,0.830,0.050,19200.0,0.0,0.0,0.0,0.0,'
              elseif(icl.eq.9)then
                itdoc='This tile is...'
                call instanitm(db,ctype,8,ier)
                itdat(1)='def_tile,none,none,-,0.000,0.000'
                itdat(2)=
     &'141,0.850,1900.000,837.000,0.900,0.600,52.000,0.0,0.0,0.0,0.0,'
              elseif(icl.eq.10)then
                itdoc='This plaster is...'
                call instanitm(db,ctype,9,ier)
                itdat(1)='def_plaster,none,none,-,0.000,0.000'
                itdat(2)=
     &'101,0.500,1300.000,1000.000,0.910,0.500,11.000,0.0,0.0,0.0,0.0,'
              elseif(icl.eq.11)then
                itdoc='This insulation is...'
                call instanitm(db,ctype,10,ier)
                itdat(1)='def_insul,none,none,-,0.000,0.000'
                itdat(2)=
     &'201,0.060,300.000,1000.000,0.900,0.500,13.000,0.0,0.0,0.0,0.0,'
              elseif(icl.eq.12)then
                itdoc='This waterproofing is...'
                call instanitm(db,ctype,11,ier)
                itdat(1)='def_dpc,none,none,-,0.000,0.000'
                itdat(2)=
     &'161,0.500,1700.000,1000.000,0.900,0.900,1000.0,0.0,0.0,0.0,0.0,'
              elseif(icl.eq.13)then
                itdoc='This carpet is...'
                call instanitm(db,ctype,12,ier)
                itdat(1)='def_carpet,none,none,-,0.000,0.000'
                itdat(2)=
     &'221,0.060,186.000,1360.000,0.900,0.600,10.000,0.0,0.0,0.0,0.0,'
              endif
            endif

C In the case of acoustic db, write a material acoustic and a construction.
            if(dbtag(idbh)(1:8).eq.'acoustic')then
              if(icl.eq.1.and.iit.eq.1)then
                itdoc='This material acoustic is...'
                ctype='MAT'
                call instanitm(db,ctype,1,ier)
              elseif(icl.eq.2.and.iit.eq.2)then
                itdoc='This system acoustic is...'
                ctype='SYS'
                call instanitm(db,ctype,2,ier)
              endif
            endif
            if(dbtag(idbh)(1:9).eq.'envir_imp')then
              itdoc='This environmental impact is...'
              if(icl.eq.1)then
                ctype='CONSIMP'
                call instanitm(db,ctype,1,ier)
              elseif(icl.eq.2)then
                ctype='CNSTRASM'
                call instanitm(db,ctype,2,ier)
              elseif(icl.eq.3)then
                ctype='CNSTRMNT'
                call instanitm(db,ctype,3,ier)
              elseif(icl.eq.4)then
                ctype='ASMPRJ'
                call instanitm(db,ctype,4,ier)
              elseif(icl.eq.5)then
                ctype='MATLCA'
                call instanitm(db,ctype,5,ier)
              elseif(icl.eq.6)then
                ctype='RECYCP'
                call instanitm(db,ctype,6,ier)
              elseif(icl.eq.7)then
                ctype='INCENP'
                call instanitm(db,ctype,7,ier)
              elseif(icl.eq.8)then
                ctype='DUMPP'
                call instanitm(db,ctype,8,ier)
              endif
            endif

C Item documentation. Each item has one line of documentation.
            write(IFA,'(3a)') '*ID',tab,itdoc(1:lnblnk(itdoc))

            write(IFA,'(8a)') '*USC',tab,ituntstr(1:lnblnk(ituntstr)),
     &        tab,itsrcstr,tab,itcnfstr(1:lnblnk(itcnfstr)),
     &        ' # units srcs uncert'

C If there is item data (remember no tab for item data).
            if(nbitdat(iit).gt.0)then
              do 37 m=1,nbitdat(iit)
                write(IFA,'(3a)')'*DAT',tab,itdat(m)(1:lnblnk(itdat(m)))
  37          continue
            endif

C If there is repeating data.
            if(nbitrep(iit).gt.0)then
              do 38 n=1,nbitrep(iit)
                write(IFA,'(3a)')'*REP',tab,itrep(n)(1:lnblnk(itrep(n)))
  38          continue
            endif
            write(IFA,'(a)')'*ENDITEM'
          endif
  34    continue
        write(IFA,'(a)')'*ENDCAT'
  42  continue
      write(IFA,'(a)')'*ENDDB'

C Close file before exiting.
      CALL ERPFREE(IFA,IOS)
      RETURN

    1 call edisp(iuout,' Error writing ascii file.')
      CALL ERPFREE(IFA,IOS)
      return
      end
