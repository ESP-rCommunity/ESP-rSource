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

C This includes facilities to manipulate a generic database structures
C for ESP-r.
C  ERAGDB:     read a generic ascii db.
C  RGENITEM:   reads one generic db item into common blocks.
C  WGENITEM:   writes one generic db item from common block data.
C  LSTGENITEM: lists one generic db item from common block data.
C  LSTCAT:     lists one category from common block data.
C  wgdbhead:   writes header of generic binary db from current commons
C  tagend:     finish updating generic db by re-writing the last record.
C  clrgdbc:    clear generic db common blocks for idbh portion of hash table
C  updirsit:   write/read place holders for items start record pointers (irsit).
C  MKAGDB:     read generic binary db and writes out an ascii db.
C  recongdb:   reconfigure generic db via exporting to ascii and re-importing.
C  CKGDBTYPE:  open & check a genericdb for its type (ascii/binary/legacy).
C  SCANDB:     open and check a generic binary db.
C  SCANDBHD:   scan generic binary db header (to fill catg and item commons).
C  REPGDB:     read generic binary db and writes out an ascii report.
C  SELCAT:     provides a list of classifications to select from.
C  SELITEM:    provides a list of items in category.
C  uniqtag:    checks if a proposed id is unique (within the current database).
C  prsunit:    given current index of data field return unitstr and unitnote.
C  prsmenu:    given current item name, return its menu text and hash index

C Common block contents:
C  nbitusc() is the number of item data fields (for units & uncertainty)
C  ibitusc() flag denoting treatment for uncertainty 0=each field separate
C            and ibitusc=1 uncertaintly applied to whole item.

C ************* ERAGDB
C Read a generic ascii db.
      subroutine eragdb(itrc,IFA,AFIL,IFB,BFIL,idbh,IER)
#include "esprdbfile.h"
C providing paramaters MGDB,MGCL,MGIT
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons:
C Database width, last record, current record, nb of doc lines, nb of
C unit lines, nb of source lines, nb of cat, nb of docs for each cat.
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      

C Database tag string, db menu entry, data stamp, db log string, doc string(s).
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)

C Units common.
      common/gendbu/unitchr(24),unitstr(24),unitnote(24)

C Sources common.
      common/gendbs/srcchr(24),srcnote(24)

C Record starts for each cat and number of documents.
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)

C Category tag string, menu entry, data stamp, doc string(s).
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)

      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8b/itdat(20),itrep(20)
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

      dimension izero(24)

      CHARACTER OUTSTR*124,WORD*20,AFIL*72,BFIL*72,LOUTSTR*248
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character itdat*248,itrep*248
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character dbnote*72,c8*8,c20*20,outs*124,LW*4
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character unitchr*4,unitstr*12,unitnote*72,srcchr*4,srcnote*248
      integer ier

      data izero/24*0/

C Open the ascii file.
      CALL EFOPSEQ(IFA,AFIL,1,IER)
      if(ier.ne.0)return

C Read the headder and see if "*ESP-r ascii gdb".
      CALL STRIPC(IFA,OUTSTR,99,ND,0,'gen db header',IER)
      if(outstr(1:16).ne.'*ESP-r ascii gdb')then
        call edisp(iuout,' Not a generic Ascii database file. ')
        ier=1
        return
      endif
      if(outstr(1:20).eq.'*ESP-r ascii gdb    ')then
        idbver(idbh)=1
      elseif(outstr(1:20).eq.'*ESP-r ascii gdb 2  ')then
        idbver(idbh)=2
      endif

C Initialise variables.
      call clrgdbc(idbh)
      inbu=0
      inbsrc=0
      icl=0
      icatdoc=0
      iit=0

C Proceed to read a line, parse the initial tag and assign the string
C to the correct point of the db structure. Start with LW (last word) blank.
      LW='    '
      CALL STRIPC(IFA,OUTSTR,99,ND,0,'db id line',IER)
      K=0
      call egetp(outstr,K,dbtag(idbh),'W','db tag',IER)
      call egetp(outstr,K,dbmenu(idbh),'W','db menu',IER)
      call egetwi(outstr,K,idbwidth(idbh),1,65,'W','idbwidth',IER)
      call egetwi(outstr,K,nbunit(idbh),1,24,'W','nb units',IER)
      call egetwi(outstr,K,nbsrc(idbh),1,24,'W','nb sources',IER)
      call egetwi(outstr,K,nbcat(idbh),1,MGCL,'W','nb cat',IER)
      call egetwi(outstr,K,nbitem(idbh),1,MGIT,'W','nb items',IER)
      call egetp(outstr,K,dbdate(idbh),'W','db date',IER)
      write(dbnote(idbh),'(a,a)')dbmenu(idbh),' log...'

C Depending on the db type set the number of fields.
      if(dbtag(idbh)(1:11).eq.'multicon_db')then
        if(itrc.ge.1)call usrmsg(' Reading ascii constructions',' ','-')
      elseif(dbtag(idbh)(1:8).eq.'material')then
        if(itrc.ge.1)call usrmsg(' Reading ascii materials...',' ','-')
      elseif(dbtag(idbh)(1:7).eq.'optical')then
        if(itrc.ge.1)call usrmsg(' Reading ascii optics...',' ','-')
      elseif(dbtag(idbh)(1:8).eq.'acoustic')then
        if(itrc.ge.1)call usrmsg(' Reading ascii acoustics...',' ','-')
      elseif(dbtag(idbh)(1:12).eq.'colourimetry')then
        if(itrc.ge.1)call usrmsg(' Reading ascii colourimetry...',
     &     ' ','-')
      elseif(dbtag(idbh)(1:9).eq.'micotoxin')then
        if(itrc.ge.1)call usrmsg(
     &    ' Reading ascii micotoxin...',' ','-')
      elseif(dbtag(idbh)(1:9).eq.'envir_imp')then
        if(itrc.ge.1)call usrmsg(
     &    ' Reading ascii environmental effects...',' ','-')
      elseif(dbtag(idbh)(1:9).eq.'transport')then
        if(itrc.ge.1)call usrmsg(' Reading ascii transports...',' ','-')
      elseif(dbtag(idbh)(1:9).eq.'pressc_db')then
        if(itrc.ge.1)call usrmsg(' Reading ascii wind pressures...',
     &    ' ','-')
      elseif(dbtag(idbh)(1:8).eq.'profiles')then
        if(itrc.ge.1)call usrmsg(' Reading ascii event profiles...',
     &    ' ','-')
      elseif(dbtag(idbh)(1:8).eq.'plant_db')then
        if(itrc.ge.1)call usrmsg(' Reading ascii plant templates...',
     &    ' ','-')
      else
        call edisp(iuout,' Reading unknown db...')
      endif

C Open the binary file and write the header.
      CALL ERPFREE(IFB,IOS)
      idbw=idbwidth(idbh)
      ier=0
      call EFOPRAN(IFB,BFIL,idbw,3,IER)
C      c20='*ESP-r binary db    '
      c20='*ESP-r binary db 2  '
      irec=1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c20,dbtag(idbh),dbmenu(idbh),
     &  idbwidth(idbh),nbunit(idbh),nbsrc(idbh),nbcat(idbh),
     &  nbitem(idbh),nbdbdoc(idbh),lastr(idbh)

  2   CALL LSTRIPC(IFA,LOUTSTR,99,ND,0,'db line',IER)
      K=0
      CALL EGETW(loutstr,K,WORD,'-','tag',IER)

C Now loop through header of db.
      if(WORD(1:7).eq.'*DBNOTE')then
        call egetp(loutstr,K,dbnote(idbh),'W','db note',IER)
        IREC=2
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)dbdate(idbh),dbnote(idbh)
        LW='*DBN'
      elseif(WORD(1:5).eq.'*UNIT')then
        LW='*UNI'
        inbu=inbu+1
        if(inbu.le.nbunit(idbh))then
          CALL EGETW(loutstr,K,unitchr(inbu),'-','unit char',IER)
          call egetp(loutstr,K,unitstr(inbu),'-','unit str',IER)
          unitnote(inbu)=' '
          call EGETRM(loutstr,K,unitnote(inbu),'W','unit note',IER)
          IREC = 2 + inbu
          c8='*UNIT   '
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,unitchr(inbu),
     &      unitstr(inbu),unitnote(inbu)
        endif
      elseif(WORD(1:7).eq.'*SOURCE')then

C Found a source, this will require 3 records to store. Because
C inbsrc has been incremented before record is counted for subsequent
C sources decrement the inbsrc counter by one to get the start.
        inbsrc=inbsrc+1
        if(inbsrc.le.nbsrc(idbh))then
          CALL EGETW(loutstr,K,srcchr(inbsrc),'-','src char',IER)
          srcnote(inbsrc)=' '
          call EGETRM(loutstr,K,srcnote(inbsrc),'W','src doc',IER)
          if(inbsrc.eq.1)then
            IREC = 2 + nbunit(idbh)
          else
            IREC = 2 + nbunit(idbh) + (inbsrc-1)
          endif

C write srcnote into next record.
          c8='*SOURCE '
          IREC=IREC+1
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,srcchr(inbsrc),
     &      srcnote(inbsrc)

C Debug.
C          WRITE(6,*)irec,c8,srcchr(inbsrc),srcnote(inbsrc)

        endif
        LW='*SOU'
      elseif(WORD(1:3).eq.'*DD')then

C Gather and write db level help strings.  
        nbdbdoc(idbh)=nbdbdoc(idbh)+1
        k=4
        call EGETRM(loutstr,K,dbdoc(nbdbdoc(idbh)),'W',
     &    'db doc',IER)
        IREC = 2 + nbunit(idbh) + nbsrc(idbh) + nbdbdoc(idbh)
        c8='*DBDOC  '
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,dbdoc(nbdbdoc(idbh))
        LW='*DD '
        goto 2
      elseif(WORD(1:6).eq.'*CATEG')then
        if(LW(1:3).eq.'*DD')then

C If just after reading db documention, write out place holders for category 
C start record pointers (irscat).
          IREC = 3+nbunit(idbh)+nbsrc(idbh)+nbdbdoc(idbh)
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(izero(j),j=1,24)
          IREC=IREC+1
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(izero(j),j=1,24)

C Write out place holders for items start record pointers (irsit)
C 12 records are reserved for this << ?? why >>
          IREC = 3+nbunit(idbh)+nbsrc(idbh)+nbdbdoc(idbh)+2
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(izero(j),j=1,24)
          do 44 i=1,12
            IREC=IREC+1
            WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(izero(j),j=1,24)
 44       continue
          LW='*CAT'
        elseif(LW(1:4).eq.'*EIT'.or.LW(1:4).eq.'*EIC')then

C Clear catg documentation because the last thing that was read was
C an *ENDITEM or *ENDCAT statement.
          icatdoc=0
          do 191 i=1,5
            catdoc(i)=': '
  191     continue
        endif
        icl=icl+1
        if(icl.gt.nbcat(idbh))nbcat(idbh)=icl
        IREC=IREC+1
        irscat(icl)=IREC
        call egetw(loutstr,K,cattag(icl),'-','cat tag',IER)
        call egetw(loutstr,K,cattype(icl),'-','cat type',IER)
        call egetp(loutstr,K,catmenu(icl),'W','cat menu',IER)
        call egetwi(loutstr,K,nbcatit(icl),0,60,'W',
     &    'items in cat',IER)
        call egetp(loutstr,K,catdate(icl),'-','cat date',IER)
        write(outs,'(a,a)')' Reading category ',catmenu(icl)
        if(itrc.gt.0)call edisp(iuout,outs)
        goto 2
      elseif(WORD(1:3).eq.'*CD')then

C Gather additional class level documentation.
        icatdoc=icatdoc+1
        k=4
        call EGETRM(loutstr,K,catdoc(icatdoc),'W','cat doc',IER)
        LW='*CD '
        goto 2
      elseif(WORD(1:5).eq.'*ITEM')then

C If just finished reading categ documentation then write that out
C before dealing with the item
        if(LW(1:4).eq.'*CD ')then
          nbcatdoc(icl)=icatdoc
          IREC=irscat(icl)
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(icl),cattype(icl),
     &      catmenu(icl),nbcatdoc(icl),nbcatit(icl),catdate(icl)
          do 52 i=1,nbcatdoc(icl)
            IREC=IREC+1
            c8='*CATDOC '
            WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,catdoc(i)
  52      continue
          LW='*ITE'
        endif

C Gather item header information after clearing any previous item data.
        iit=iit+1
        itdate(iit)=' '
        nbitusc(iit)=1
        nbitdat(iit)=0
        nbitrep(iit)=0
        ibitusc(iit)=0
        do 23 i=1,20
          itdat(i)=' '
          itrep(i)=' '
  23    continue
        inbitdoc=0
        itdoc=': '
        ituntstr='-'
        itsrcstr='-'
        itcnfstr='-'
        inbitdat=0
        inbitrep=0
        if(idbver(idbh).eq.1)then
          call egetw(loutstr,K,ittag(iit),'-','ittag',IER)
          call egetp(loutstr,K,itmenu(iit),'W','itmenu',IER)
          call egetwi(loutstr,K,nbitdat(iit),0,20,'W','data',IER)
          call egetwi(loutstr,K,nbitrep(iit),0,96,'W','rptg dat',IER)
          call egetwi(loutstr,K,nbitusc(iit),0,28,'W','nb of USC',IER)
          call egetwi(loutstr,K,ibitusc(iit),0,1,'W','itm uncert',IER)
          call egetp(loutstr,K,itdate(iit),'W','itdate',IER)
        elseif(idbver(idbh).eq.2)then

C Read fewer items if version 2. 
          call egetw(loutstr,K,ittag(iit),'-','ittag',IER)
          call egetp(loutstr,K,itmenu(iit),'W','itmenu',IER)
          call egetp(loutstr,K,itdate(iit),'W','itdate',IER)
        endif

        write(outs,'(a,a)')' Reading item ',itmenu(iit)
        if(itrc.gt.0)call usrmsg(outs,' ','-')

C Loop item lines.
    5   CALL LSTRIPC(IFA,LOUTSTR,99,ND,0,'item line',IER)
        K=0
        CALL EGETW(loutstr,K,WORD,'-','tag',IER)
        if(WORD(1:4).eq.'*ID ')then

C Gather item documentation there is one line of up to 248 char
C for each item.
          if(inbitdoc.lt.1)then
            inbitdoc=inbitdoc+1
             k=4
            call EGETRM(loutstr,K,itdoc,'W','itm doc',IER)
            LW='*ID '
            goto 5
          else

C Debug.
C            write(6,*) 'more than one item doc line for item ',iit,' ',
C     &        ittag(iit),' ',itmenu(iit)

            LW='*ID '
            goto 5
          endif
        elseif(WORD(1:4).eq.'*USC')then
          call egetp(loutstr,K,ituntstr,'W','item units',IER)
          call egetw(loutstr,K,itsrcstr,'W','item source',IER)
          call egetp(loutstr,K,itcnfstr,'W','item conf',IER)
          LW='*USC'
          if(idbver(idbh).eq.2)then

C Figure out the number of unit pointers from the length of ituntstr.
            nbitusc(iit)=lnblnk(ituntstr)
          endif
          goto 5
        elseif(WORD(1:4).eq.'*DAT')then

C Item data might contain tab characters so read remainder of line.
          inbitdat=inbitdat+1
          call EGETRM(loutstr,K,itdat(inbitdat),'W','item',IER)
          LW='*DAT'
          if(idbver(idbh).eq.2)then

C If version 2 increment number of data lines.
            nbitdat(iit)=nbitdat(iit)+1
          endif
          goto 5
        elseif(WORD(1:4).eq.'*REP')then

C Repeating data might contain tab characters so read remainder of line.
          inbitrep=inbitrep+1
          call EGETRM(loutstr,K,itrep(inbitrep),'W','dat rep',IER)
          LW='*REP'
          if(idbver(idbh).eq.2)then

C If version 2 increment number of repeting lines.
            nbitrep(iit)=nbitrep(iit)+1
          endif
          goto 5
        elseif(WORD(1:8).eq.'*ENDITEM')then

C Dump out item header and data.
          LW='*EIT'
          IREC=IREC+1
          itcat(iit)=icl
          nbitdat(iit)=inbitdat

C Debug.
C          write(6,*) 'inbitdat nbitdat ',inbitdat,nbitdat(iit)
C          write(6,*) 'nbitdat nbitrep nbitusc ibitusc ',nbitdat(iit),
C     &      nbitrep(iit),nbitusc(iit),ibitusc(iit)

          nbitrep(iit)=inbitrep
          irsit(iit)=IREC
          if(idbver(idbh).eq.1)then
            nbitdoc=1
            WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),
     &        itmenu(iit),itdate(iit),itcat(iit),nbitdoc,
     &        nbitdat(iit),nbitrep(iit),nbitusc(iit),ibitusc(iit)
          else
            WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),
     &        itmenu(iit),itdate(iit),itcat(iit),
     &        nbitdat(iit),nbitrep(iit),nbitusc(iit),ibitusc(iit)
          endif

C write itdoc on one record.
          c8='*ITDOC  '
          IREC=IREC+1
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,itdoc
          c8='*USC    '
          IREC=IREC+1
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,ituntstr,itsrcstr,
     &      itcnfstr
          do 246 ij=1,nbitdat(iit)
            IREC=IREC+1
            WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itdat(ij)
 246      continue
          lastr(idbh)=IREC
          if(nbitrep(iit).gt.0)then
            do 247 ij=1,nbitrep(iit)
              IREC=IREC+1
              WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itrep(ij)
 247        continue
            lastr(idbh)=IREC
          endif
          goto 2
        elseif(WORD(1:9).eq.'*ENDCAT  ')then
          goto 2
        endif

C Loop back and read/parse the next line.
        goto 2

      elseif(WORD(1:9).eq.'*ENDCAT  ')then

C If we have reached *ENDCAT and the previous word with *EIT then
C dump out the last items data.
        if(LW(1:4).eq.'*EIT')then
          IREC=IREC+1
          itcat(iit)=icl
          nbitdat(iit)=inbitdat

C Debug.
C          write(6,*) 'inbitdat nbitdat ',inbitdat,nbitdat(iit)

          nbitrep(iit)=inbitrep
          irsit(iit)=IREC
          if(idbver(idbh).eq.1)then
            nbitdoc=1
            WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),
     &        itmenu(iit),itdate(iit),itcat(iit),nbitdoc,
     &        nbitdat(iit),nbitrep(iit),nbitusc(iit),ibitusc(iit)
          else
            WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),
     &        itmenu(iit),itdate(iit),itcat(iit),
     &        nbitdat(iit),nbitrep(iit),nbitusc(iit),ibitusc(iit)
          endif

C write itdoc to 1 record.
          c8='*ITDOC  '
          IREC=IREC+1
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,itdoc
          IREC=IREC+1
          c8='*USC    '
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,ituntstr,itsrcstr,
     &      itcnfstr
          do 346 ij=1,nbitdat(iit)
            IREC=IREC+1
            WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itdat(ij)
 346      continue
          lastr(idbh)=IREC
          if(nbitrep(iit).gt.0)then
            do 347 ij=1,nbitrep(iit)
              IREC=IREC+1
              WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itrep(ij)
 347        continue
            lastr(idbh)=IREC
          endif
        endif
        LW='*EIC'
        goto 2
      elseif(WORD(1:6).eq.'*ENDDB')then

C End of db, complete the headder. First update all cat and item pointers.
        IREC = 3+nbunit(idbh)+nbsrc(idbh)+nbdbdoc(idbh)
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(irscat(j),j=1,24)
        IREC=IREC+1
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(irscat(j),j=25,MGCL)
        call updirsit(IFB,idbh,'W')
C        c20='*ESP-r binary db    '
        c20='*ESP-r binary db 2  '
        irec=1
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c20,dbtag(idbh),
     &    dbmenu(idbh),idbwidth(idbh),nbunit(idbh),nbsrc(idbh),
     &    nbcat(idbh),nbitem(idbh),nbdbdoc(idbh),lastr(idbh)
        call tagend(IFB,idbh,ier)

C If no errors flag the "ok" tag for the file type as true.
        if(ier.eq.0)then
          if(dbtag(idbh)(1:11).eq.'multicon_db')then
            gendbok(1)=.true.
          elseif(dbtag(idbh)(1:8).eq.'material')then
            gendbok(2)=.true.
          elseif(dbtag(idbh)(1:7).eq.'optical')then
            gendbok(3)=.true.
          elseif(dbtag(idbh)(1:8).eq.'acoustic')then
            gendbok(5)=.true.
          elseif(dbtag(idbh)(1:12).eq.'colourimetry')then
            gendbok(6)=.true.
          elseif(dbtag(idbh)(1:9).eq.'micotoxin')then
            gendbok(7)=.true.
          elseif(dbtag(idbh)(1:9).eq.'envir_imp')then
            gendbok(8)=.true.
          elseif(dbtag(idbh)(1:9).eq.'transport')then
            gendbok(9)=.true.
          elseif(dbtag(idbh)(1:9).eq.'pressc_db')then
            gendbok(10)=.true.
          elseif(dbtag(idbh)(1:8).eq.'profiles')then
            gendbok(11)=.true.
          elseif(dbtag(idbh)(1:8).eq.'plant_db')then
            gendbok(12)=.true.
          elseif(dbtag(idbh)(1:12).eq.'component_db')then
            gendbok(13)=.true.
          endif
        endif

C Close file before exiting.
        CALL ERPFREE(IFA,IOS)
        CALL ERPFREE(IFB,IOS)
        RETURN
      endif
      goto 2

    1 write(outs,'(a,I6)') ' Error in record ',IREC
      call edisp(iuout,outs)
      write(outs,'(a,a72)') ' Last line:.. ',loutstr(1:72)
      call edisp(iuout,outs)
      write(outs,'(a,a72)') ' Last line: ',outstr(1:72)
      call edisp(iuout,outs)
      CALL ERPFREE(IFA,IOS)
      CALL ERPFREE(IFB,IOS)
      return
      end
      
C ************* RGENITEM
C RGENITEM reads one generic db item into common blocks.
      subroutine rgenitem(IFB,idbh,iit,ier)
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN

      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8b/itdat(20),itrep(20)
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

      character itdat*248,itrep*248,ittag*12,itmenu*32,itdate*24
      character itdoc*248
      character c8*8,outs*124,ituntstr*42,itsrcstr*1,itcnfstr*42

C Assume the binary file is open. Clear the strings and then read.
      ier=0
      itdate(iit)=' '
      ituntstr='-'
      itsrcstr='-'
      itcnfstr='-'
      do 23 i=1,20
       itdat(i)=' '
       itrep(i)=' '
  23  continue
      IREC=irsit(iit)
      if(idbver(idbh).eq.1)then
        READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),itmenu(iit),
     &    itdate(iit),itcat(iit),nbitdoc,nbitdat(iit),nbitrep(iit),
     &    nbitusc(iit),ibitusc(iit)
      else
        READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),itmenu(iit),
     &    itdate(iit),itcat(iit),nbitdat(iit),nbitrep(iit),
     &    nbitusc(iit),ibitusc(iit)
      endif

C Item documentation read from next record.
      IREC=IREC+1
      READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,itdoc

C Item units, sources and confidence strings to parse.
      IREC=IREC+1
      read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,ituntstr,itsrcstr,
     &  itcnfstr

C Lines of item data.
      if(nbitdat(iit).gt.0)then
        do 37 m=1,nbitdat(iit)
          IREC=IREC+1
          READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itdat(m)
  37    continue
      endif

C Repeating data for item.
      if(nbitrep(iit).gt.0)then
        do 38 n=1,nbitrep(iit)
          IREC=IREC+1
          READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itrep(n)
  38    continue
      endif
      return

   1  write(outs,'(a,I6,a,i3)') 'RGENITEM: Error reading record ',IREC,
     &  ' of file unit ',ifb
      call edisp(iuout,outs)
      CALL ERPFREE(IFB,IOS)
      ier=1
      return
      end
      
C ************* WGENITEM
C WGENITEM writes one generic db item from common block data.
      subroutine wgenitem(IFB,idbh,iit,ier)
#include "esprdbfile.h"

      COMMON/OUTIN/IUOUT,IUIN

      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8b/itdat(20),itrep(20)
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

      character itdat*248,itrep*248,ittag*12,itmenu*32,itdate*24
      character itdoc*248
      character c8*8,outs*124,ituntstr*42,itsrcstr*1,itcnfstr*42

C Assume the binary file is open.
      ier=0
      IREC=irsit(iit)
      if(idbver(idbh).eq.1)then
        nbitdoc=1
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),itmenu(iit),
     &    itdate(iit),itcat(iit),nbitdoc,nbitdat(iit),nbitrep(iit),
     &    nbitusc(iit),ibitusc(iit)
      else
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),itmenu(iit),
     &    itdate(iit),itcat(iit),nbitdat(iit),nbitrep(iit),
     &    nbitusc(iit),ibitusc(iit)
      endif

C write itdoc into next record.
      c8='*ITDOC  '
      IREC=IREC+1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,itdoc
      IREC=IREC+1
      c8='*USC    '
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,ituntstr,itsrcstr,
     &  itcnfstr
      do 246 ij=1,nbitdat(iit)
        IREC=IREC+1
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itdat(ij)
 246  continue
      if(nbitrep(iit).gt.0)then
        do 247 ij=1,nbitrep(iit)
          IREC=IREC+1
          WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itrep(ij)
 247    continue
      endif

C Just to be sure re-write the end-of-file record.
      call tagend(IFB,idbh,ier)
      return

   1  write(outs,'(a,I6,a,i3)') 'WGENITEM: Error reading record ',IREC,
     &  ' of file unit ',ifb
      call edisp(iuout,outs)
      CALL ERPFREE(IFB,IOS)
      ier=1
      return
      end
      
C ************* LSTGENITEM
C LSTGENITEM lists one generic db item from common block data.
      subroutine lstgenitem(itru,idbh,iit,ier)
#include "esprdbfile.h"

      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8b/itdat(20),itrep(20)
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

      character itdat*248,itrep*248,ittag*12,itmenu*32,itdate*24
      character itdoc*248
      character outs*124,ituntstr*42,itsrcstr*1,itcnfstr*42

      ier=0
      write(outs,'(3a)') 'Item tag & menu ',ittag(iit),itmenu(iit)
      call edisp(itru,outs)
      if(idbver(idbh).eq.1)then
        nbitdoc=1
        write(outs,'(2a,6i3)') ' date catg nb doc nb dat nb rep ',
     &    itdate(iit),itcat(iit),nbitdoc,nbitdat(iit),nbitrep(iit),
     &    nbitusc(iit),ibitusc(iit)
      else
        write(outs,'(2a,5i3)') ' date catg nb dat nb rep ',
     &    itdate(iit),itcat(iit),nbitdat(iit),nbitrep(iit),
     &    nbitusc(iit),ibitusc(iit)
      endif
      call edisp(itru,outs)

C Revised to cope with 248 char itdoc.
      call edisp248(itru,itdoc,96)
      write(itru,'(4a)') 'unit src cnf ',ituntstr,itsrcstr,itcnfstr
      call edisp(itru,outs)
      do 246 ij=1,nbitdat(iit)
        write(itru,'(1x,a)') itdat(ij)
 246  continue
      if(nbitrep(iit).gt.0)then
        do 247 ij=1,nbitrep(iit)
          write(itru,'(1x,a)') itrep(ij)
 247    continue
      endif
      call edisp(itru,' ')
      return
      end
      
C ************* LSTCAT
C LSTCAT lists one category from common block data.
      subroutine lstcat(itru,icl,ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      character cattag*12,cattype*8,catmenu*32,catdate*24,catdoc*248
      character outs*124,tab*1

      tab=' '
      write(outs,'(a)')'# CAT tag,id,type,menu,items_in_cat,date'
      call edisp(itru,outs)
      write(outs,'(6a,i2,2a)')
     &    cattag(icl)(1:lnblnk(cattag(icl))),tab,
     &    cattype(icl)(1:lnblnk(cattype(icl))),tab,
     &    catmenu(icl)(1:lnblnk(catmenu(icl))),tab,
     &    nbcatit(icl),tab,catdate(icl)
      call edisp(itru,outs)
      return
      end

C ************* wgdbhead 
C wgdbhead: Writes out header of generic binary db in (BFIL) from current
C common block data. Idbh is position in hash table.
      subroutine wgdbhead(IFB,idbh,IER)
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN

      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendbu/unitchr(24),unitstr(24),unitnote(24)
      common/gendbs/srcchr(24),srcnote(24)

      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248
      character dbnote*72,c8*8,c20*20,outs*124
      character unitchr*4,unitstr*12,unitnote*72,srcchr*4,srcnote*248

C Write first two records of db.
      c20='*ESP-r binary db 2  '
      irec=1
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c20,dbtag(idbh),dbmenu(idbh),
     &  idbwidth(idbh),nbunit(idbh),nbsrc(idbh),nbcat(idbh),
     &  nbitem(idbh),nbdbdoc(idbh),lastr(idbh)
      IREC=2
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)dbdate(idbh),dbnote(idbh)

C Units and sources.
      IREC=2
      c8='*UNIT   '
      do 17 iu=1,nbunit(idbh)
        IREC=IREC+1
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,unitchr(iu),
     &    unitstr(iu),unitnote(iu)
  17  continue

      IREC = 2 + nbunit(idbh)
      c8='*SOURCE '
      do 18 iu=1,nbsrc(idbh)

C write srcnote into next record.
        IREC=IREC+1
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,srcchr(iu),srcnote(iu)
  18  continue

C Write db level documentation after units & sources (which take 3 rec each).
      IREC = 2+nbunit(idbh)+nbsrc(idbh)
      c8='*DBDOC  '
      do 19 iu=1,nbdbdoc(idbh)
        IREC=IREC+1
        WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,dbdoc(iu)
  19  continue
      return

   1  write(outs,'(a,I6)') 'dmpghead: Error writing record ',IREC
      call edisp(iuout,outs)
      ier=1
      CALL ERPFREE(IFB,IOS)
      RETURN
      end

C ********* tagend
C tagend Finish updating generic db by re-writing the last record.
      subroutine tagend(IFB,idbh,ier)
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      character c8*8,outs*124
    
      IREC=lastr(idbh)+1
      c8='*ENDFILE'
      WRITE(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8
      return
   1  write(outs,'(a,I6)') 'tagend: Error writing record ',IREC
      call edisp(iuout,outs)
      CALL ERPFREE(IFB,IOS)
      ier=1
      return
      end

C ********** clrgdbc
C clrgdbc Clear generic db common blocks for idbh portion of hash table
C and common blocks.
      subroutine clrgdbc(idbh)
#include "esprdbfile.h"

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

      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24,itdoc*248
      character itdat*248,itrep*248,ittag*12,itmenu*32,itdate*24
      character dbnote*72,ituntstr*42,itsrcstr*1,itcnfstr*42
      character unitchr*4,unitstr*12,unitnote*72,srcchr*4,srcnote*248

C Initialise variables.
      idbwidth(idbh)=65
      lastr(idbh)=0
      dbtag(idbh)=' '
      dbmenu(idbh)=' '
      dbnote(idbh)=' '
      dbdate(idbh)=' '

C Initialise units and sources.
      do 7 i=1,24
        if(i.eq.1)unitchr(i)='-'
        if(i.eq.2)unitchr(i)='a'
        if(i.eq.3)unitchr(i)='b'
        if(i.eq.4)unitchr(i)='c'
        if(i.eq.5)unitchr(i)='d'
        if(i.eq.6)unitchr(i)='e'
        if(i.eq.7)unitchr(i)='f'
        if(i.eq.8)unitchr(i)='g'
        if(i.eq.9)unitchr(i)='h'
        if(i.eq.10)unitchr(i)='i'
        if(i.eq.11)unitchr(i)='j'
        if(i.eq.12)unitchr(i)='k'
        if(i.eq.13)unitchr(i)='l'
        if(i.eq.14)unitchr(i)='m'
        if(i.eq.15)unitchr(i)='n'
        if(i.eq.16)unitchr(i)='o'
        if(i.eq.17)unitchr(i)='p'
        if(i.eq.18)unitchr(i)='q'
        if(i.eq.19)unitchr(i)='r'
        if(i.eq.20)unitchr(i)='s'
        if(i.eq.21)unitchr(i)='t'
        if(i.eq.22)unitchr(i)='u'
        if(i.eq.23)unitchr(i)='v'
        if(i.eq.24)unitchr(i)='w'
        unitstr(i)='none'
        unitnote(i)='no documentation'
  7   continue

      do 8 i=1,24
        if(i.eq.1)srcchr(i)='-'
        if(i.eq.2)srcchr(i)='a'
        if(i.eq.3)srcchr(i)='b'
        if(i.eq.4)srcchr(i)='c'
        if(i.eq.5)srcchr(i)='d'
        if(i.eq.6)srcchr(i)='e'
        if(i.eq.7)srcchr(i)='f'
        if(i.eq.8)srcchr(i)='g'
        if(i.eq.9)srcchr(i)='h'
        if(i.eq.10)srcchr(i)='i'
        if(i.eq.11)srcchr(i)='j'
        if(i.eq.12)srcchr(i)='k'
        if(i.eq.13)srcchr(i)='l'
        if(i.eq.14)srcchr(i)='m'
        if(i.eq.15)srcchr(i)='n'
        if(i.eq.16)srcchr(i)='o'
        if(i.eq.17)srcchr(i)='p'
        if(i.eq.18)srcchr(i)='q'
        if(i.eq.19)srcchr(i)='r'
        if(i.eq.20)srcchr(i)='s'
        if(i.eq.21)srcchr(i)='t'
        if(i.eq.22)srcchr(i)='u'
        if(i.eq.23)srcchr(i)='v'
        if(i.eq.24)srcchr(i)='w'
        srcnote(i)='no documentation supplied (yet)'
  8   continue
      nbsrc(idbh)=0
      nbunit(idbh)=0

C Clear database level help and classes.
      nbdbdoc(idbh)=0
      do 9 i=1,10
        dbdoc(i)=': '
        if(i.le.5)catdoc(i)=': '
   9  continue
      nbcat(idbh)=0
      do 11 i=1,MGCL
        nbcatdoc(i)=0
        catdate(i)=' '
        cattag(i)=' '
        cattype(i)=' '
        catmenu(i)=' '
        irscat(i)=0
  11  continue

      nbitem(idbh)=0
      do 50 i=1,MGIT
        ittag(i)=' '
        itmenu(i)=' '
        irsit(i)=0
        itcat(i)=0
        itdate(i)=' '
        nbitusc(i)=1
        ibitusc(i)=0
  50  continue
        
      ituntstr='-'
      itdoc=' '
      itsrcstr='-'
      itcnfstr='-'
      do 23 i=1,20
       itdat(i)=' '
       itrep(i)=' '
  23  continue
      return
      end

C ********** updirsit
C updirsit Write or read place holders for items start record pointers (irsit).
C If act = `W` write irsit, if act = `R` read irsit, if act = `I` then
C initialise irsit and irscat records.
      subroutine updirsit(IFB,idbh,act)
#include "esprdbfile.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb8/irsit(MGIT),itcat(MGIT)
      dimension ile(MGDB),izero(24)
      character outs*124,act*1
      data ile/24,48,72,96,120,144,168,192,216,240,264,288,300/
      data izero/24*0/

      if(act.eq.'I')then

C Write out place holders for category start record pointers (irscat)
        IR = 3+nbunit(idbh)+nbsrc(idbh)+nbdbdoc(idbh)
        WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)(izero(j),j=1,24)
        IR=IR+1
        WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)(izero(j),j=1,24)

C Write out place holders for items start record pointers (irsit)
        IR = 3+nbunit(idbh)+nbsrc(idbh)+nbdbdoc(idbh)+2
        WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)(izero(j),j=1,24)
        do 44 i=1,12
          IR=IR+1
          WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)(izero(j),j=1,24)
 44     continue
        return
      endif

C Read or write irsit.
      IR = 3+nbunit(idbh)+nbsrc(idbh)+nbdbdoc(idbh)+1
      do 42 ij=1,MGDB
        li=ile(ij)
        IR=IR+1
        if(act.eq.'W')then
         if(ij.eq.1)WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)(irsit(j),j=1,li)
         if(ij.eq.2)WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)(irsit(j),j=25,li)
         if(ij.eq.3)WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)(irsit(j),j=49,li)
         if(ij.eq.4)WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)(irsit(j),j=73,li)
         if(ij.eq.5)WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)(irsit(j),j=97,li)
          if(ij.eq.6)WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)
     &      (irsit(j),j=121,li)
          if(ij.eq.7)WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)
     &      (irsit(j),j=145,li)
          if(ij.eq.8)WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)
     &      (irsit(j),j=169,li)
          if(ij.eq.9)WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)
     &      (irsit(j),j=193,li)
          if(ij.eq.10)WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)
     &      (irsit(j),j=217,li)
          if(ij.eq.11)WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)
     &      (irsit(j),j=241,li)
          if(ij.eq.12)WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)
     &      (irsit(j),j=265,li)
          if(ij.eq.13)WRITE(IFB,REC=IR,IOSTAT=IOS,ERR=1)
     &      (irsit(j),j=289,li)
        elseif(act.eq.'R')then
         if(ij.eq.1)read(IFB,REC=IR,IOSTAT=IOS,ERR=1)(irsit(j),j=1,li)
         if(ij.eq.2)read(IFB,REC=IR,IOSTAT=IOS,ERR=1)(irsit(j),j=25,li)
         if(ij.eq.3)read(IFB,REC=IR,IOSTAT=IOS,ERR=1)(irsit(j),j=49,li)
         if(ij.eq.4)read(IFB,REC=IR,IOSTAT=IOS,ERR=1)(irsit(j),j=73,li)
         if(ij.eq.5)read(IFB,REC=IR,IOSTAT=IOS,ERR=1)(irsit(j),j=97,li)
         if(ij.eq.6)read(IFB,REC=IR,IOSTAT=IOS,ERR=1)(irsit(j),j=121,li)
         if(ij.eq.7)read(IFB,REC=IR,IOSTAT=IOS,ERR=1)(irsit(j),j=145,li)
         if(ij.eq.8)read(IFB,REC=IR,IOSTAT=IOS,ERR=1)(irsit(j),j=169,li)
         if(ij.eq.9)read(IFB,REC=IR,IOSTAT=IOS,ERR=1)(irsit(j),j=193,li)
          if(ij.eq.10)read(IFB,REC=IR,IOSTAT=IOS,ERR=1)
     &      (irsit(j),j=217,li)
          if(ij.eq.11)read(IFB,REC=IR,IOSTAT=IOS,ERR=1)
     &      (irsit(j),j=241,li)
          if(ij.eq.12)read(IFB,REC=IR,IOSTAT=IOS,ERR=1)
     &      (irsit(j),j=265,li)
          if(ij.eq.13)read(IFB,REC=IR,IOSTAT=IOS,ERR=1)
     &      (irsit(j),j=289,li)
        endif
 42   continue
      return

    1 write(outs,'(a,I6)') ' Error in record ',IR
      call edisp(iuout,outs)
      return
      end

C ************* MKAGDB
C MKAGDB Read a generic binary db and writes out an ascii db. Overloaded so
C that it can be used to update databases where the number of records
C need to change. It detects this via the current contents of tmpdoca
C as follows:
C   bkdocok (logical) if true then substitue data fields.
C   bkdoctyp says which data to substitute. Currently UN for units,
C     SR for sources, DD for dabase documentation, CD for category
C     documentation, DE to delete an entity, IL to change the number
C     of repeating elements in an item.
C   bkdoc, bk4, bk12,bk72, bk248 are arrays of character strings which
C     are filled by the user when editing and at as substitutes.

      subroutine mkagdb(IFA,AFIL,IFB,BFIL,idbh,IER)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

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
      common/tmpdoca/bkdoctyp,bkdoc(60),bk4(24),bk12(24),bk72(24),
     &  bk248(24)

C bkdocok is true nbkdoc is the number of lines held.
C bkascidb is backup file used when editing the database (i.e. 
C changing its length).
      common/tmpdocb/bkdocok,nbkdoc,indexbk

      character AFIL*72,BFIL*72,tab*1,class*12,item*12
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24,itdoc*248
      character itdat*248,itrep*248,ittag*12,itmenu*32,itdate*24
      character dbnote*72,c8*8,c20*20,outs*124
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character unitchr*4,unitstr*12,unitnote*72
      character srcchr*4,srcnote*248,bkdoctyp*2
      character bkdoc*72,bk4*4,bk12*12,bk72*72
      character bk248*248
      character errmsg*48
      logical bkdocok,ok,dok
      integer ier

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='mkagdb'  ! set for subroutine

C Setup a tab character.
C      tab=CHAR(9)
      tab=','

C Open the binary file and read the header.
      CALL ERPFREE(IFB,IOS)
      idbw=idbwidth(idbh)
      ier=0
      call EFOPRAN(IFB,BFIL,idbw,1,IER)

      errmsg='reading how many tags menus units etc.'
      irec=1
      READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c20,dbtag(idbh),dbmenu(idbh),
     &  idbwidth(idbh),nbunit(idbh),nbsrc(idbh),nbcat(idbh),
     &  nbitem(idbh),nbdbdoc(idbh),lastr(idbh)
      if(c20(1:15).ne.'*ESP-r ascii db'.and.
     &   c20(1:16).ne.'*ESP-r binary db')then
        call edisp(iuout,' Not a generic binary database file. ')
        ier=1
        return
      endif
      if(c20(1:20).eq.'*ESP-r binary db    ')then
        dok=.true.
        helptopic='option_to_upgrade'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call askok(' ',' Update to version 2?',OK,dok,nbhelp)
        if(ok)then
          idbver(idbh)=2
        else
          idbver(idbh)=1
        endif
      elseif(c20(1:20).eq.'*ESP-r binary db 2  ')then
        idbver(idbh)=2
      endif

      errmsg='reading database date and note.'
      irec=2
      READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)dbdate(idbh),dbnote(idbh)

C Open the ascii file and write headder.
      CALL EFOPSEQ(IFA,AFIL,4,IER)
      if(ier.ne.0)return
C      write(IFA,'(a)') '*ESP-r ascii gdb'
      write(IFA,'(a)') '*ESP-r ascii gdb 2'
      write(IFA,'(a)')
     &  '# db type, menu, width,nb of units:sources:cat:items, date'
      if(bkdocok.and.(bkdoctyp(1:2).eq.'UN'))then

C If update to units patch in new nbunit value (subsequent
C read of ascii file will find and update nbunit in common blocks).
        write(IFA,'(4a,i2,a,i2,a,i2,a,i2,a,i3,2a)')
     &    dbtag(idbh)(1:lnblnk(dbtag(idbh))),tab,
     &    dbmenu(idbh)(1:lnblnk(dbmenu(idbh))),tab,
     &    idbwidth(idbh),tab,nbkdoc,tab,nbsrc(idbh),tab,nbcat(idbh),
     &    tab,nbitem(idbh),tab,dbdate(idbh)
      elseif(bkdocok.and.(bkdoctyp(1:2).eq.'SR'))then

C If update to units patch in new nbsrc value (subsequent
C read of ascii file will find and update nbsrc in common blocks).
        write(IFA,'(4a,i2,a,i2,a,i2,a,i2,a,i3,2a)')
     &    dbtag(idbh)(1:lnblnk(dbtag(idbh))),tab,
     &    dbmenu(idbh)(1:lnblnk(dbmenu(idbh))),tab,
     &    idbwidth(idbh),tab,nbunit(idbh),tab,nbkdoc,tab,nbcat(idbh),
     &    tab,nbitem(idbh),tab,dbdate(idbh)
      elseif(bkdocok.and.(bkdoctyp(1:2).eq.'DE'))then

C If an item has been deleted, write header with one less item count
C so that on subsequent read the correct number will be found.
        write(IFA,'(4a,i2,a,i2,a,i2,a,i2,a,i3,2a)')
     &    dbtag(idbh)(1:lnblnk(dbtag(idbh))),tab,
     &    dbmenu(idbh)(1:lnblnk(dbmenu(idbh))),tab,
     &    idbwidth(idbh),tab,nbunit(idbh),tab,nbsrc(idbh),tab,
     &    nbcat(idbh),tab,nbitem(idbh)-1,tab,dbdate(idbh)
      else
        write(IFA,'(4a,i2,a,i2,a,i2,a,i2,a,i3,2a)')
     &    dbtag(idbh)(1:lnblnk(dbtag(idbh))),tab,
     &    dbmenu(idbh)(1:lnblnk(dbmenu(idbh))),tab,
     &    idbwidth(idbh),tab,nbunit(idbh),tab,nbsrc(idbh),tab,
     &    nbcat(idbh),tab,nbitem(idbh),tab,dbdate(idbh)
      endif

      write(IFA,'(a,a1,a)')'*DBNOTE',tab,
     &  dbnote(idbh)(1:lnblnk(dbnote(idbh)))

C Write out Units and Sources.
      if(bkdocok.and.(bkdoctyp(1:2).eq.'UN'))then

C If units edited then write updated text.
        do 252 inbu=1,nbkdoc
          write(IFA,'(7a)')'*UNIT',tab,unitchr(inbu)(1:1),tab,
     &      bk12(inbu)(1:lnblnk(bk12(inbu))),tab,
     &      bk72(inbu)(1:lnblnk(bk72(inbu)))
 252    continue
      else
        if(nbunit(idbh).gt.0)then
          errmsg='reading unit char unit string & note.'
          irec = 2
          do 4 inbu=1,nbunit(idbh)
            IREC=IREC+1
            READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,unitchr(inbu),
     &        unitstr(inbu),unitnote(inbu)
            write(IFA,'(7a)')'*UNIT',tab,unitchr(inbu)(1:1),tab,
     &        unitstr(inbu)(1:lnblnk(unitstr(inbu))),tab,
     &        unitnote(inbu)(1:lnblnk(unitnote(inbu)))
  4       continue
        endif
      endif

      if(bkdocok.and.(bkdoctyp(1:2).eq.'SR'))then

C If sources edited then write updated text from bk248 array
C which had been setup as the user edited the text.
        do 253 inbu=1,nbkdoc
          write(IFA,'(5a)')'*SOURCE',tab,srcchr(inbu)(1:1),tab,
     &      bk248(inbu)(1:lnblnk(bk248(inbu)))
 253    continue
      else
        if(nbsrc(idbh).gt.0)then
          errmsg='reading source char source note.'
          irec = 2 + nbunit(idbh)
          do 5 inbsrc=1,nbsrc(idbh)

C Read next record for srcnote.
            IREC=IREC+1
            READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,srcchr(inbsrc),
     &        srcnote(inbsrc)
            write(IFA,'(5a)')'*SOURCE',tab,srcchr(inbsrc)(1:1),tab,
     &        srcnote(inbsrc)(1:lnblnk(srcnote(inbsrc)))
  5       continue
        endif
      endif

C << place for uncertanty here >>

C Write out the db documentation records. If db documentation needs to
C be updated then write out alternative text.
      write(IFA,'(a)')'# db level documentation'
      if(bkdocok.and.(bkdoctyp(1:2).eq.'DD'))then
        do 52 i=1,nbkdoc
          write(IFA,'(2a)') '*DD ',bkdoc(i)(1:lnblnk(bkdoc(i)))
  52    continue
      else
        if(nbdbdoc(idbh).eq.0)then
          nbdbdoc(idbh)=1
          dbdoc(1)='Database documentation to be inserted here'
        endif

        errmsg='reading database documentation.'
        IREC = 2+nbunit(idbh)+nbsrc(idbh)
        do 51 i=1,nbdbdoc(idbh)
          IREC=IREC+1
          read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,dbdoc(i)
          write(IFA,'(2a)') '*DD ',dbdoc(i)(1:lnblnk(dbdoc(i)))
  51    continue
      endif

C Read start record of each category.
      errmsg='reading start record for each category.'
      IREC = 3+nbunit(idbh)+nbsrc(idbh)+nbdbdoc(idbh)
      read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(irscat(j),j=1,24)
      IREC=IREC+1
      if(nbcat(idbh).gt.24)then
        read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)(irscat(j),j=25,MGCL)
      endif

C Read start record of each item.
      call updirsit(IFB,idbh,'R')

C Skip to first category record and read cat data.
      icl=0
      iit=0
      do 42 k=1,nbcat(idbh)
        errmsg='reading category type menu date etc.'
        icl=icl+1
        IREC=irscat(icl)
        read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(icl),cattype(icl),
     &    catmenu(icl),nbcatdoc(icl),nbcatit(icl),catdate(icl)

        write(IFA,'(a)')'# CAT tag,id,type,menu,items_in_cat,date'
        write(IFA,'(8a,i2,2a)')'*CATEG',tab,
     &    cattag(icl)(1:lnblnk(cattag(icl))),tab,
     &    cattype(icl)(1:lnblnk(cattype(icl))),tab,
     &    catmenu(icl)(1:lnblnk(catmenu(icl))),tab,
     &    nbcatit(icl),tab,catdate(icl)

C If this category documentation to be updated then use bkdoc text.
        write(IFA,'(a)')'# cat documentation'
        if(bkdocok.and.(bkdoctyp(1:2).eq.'CD').and.
     &    (indexbk.eq.icl))then
          do 152 ij=1,nbkdoc
            write(IFA,'(2a)') '*CD ',bkdoc(ij)(1:lnblnk(bkdoc(ij)))
 152      continue
        else
          if(nbcatdoc(icl).eq.0)then
            nbcatdoc(icl)=1
            catdoc(1)='Category documentation to be inserted here.'
          endif
          errmsg='reading category documentation.'
          do 53 ij=1,nbcatdoc(icl)
            IREC=IREC+1
            read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,catdoc(ij)
            write(IFA,'(2a)') '*CD ',catdoc(ij)(1:lnblnk(catdoc(ij)))
  53      continue
        endif

C Comment line for item. 
        if(idbver(idbh).eq.1)then
          write(IFA,'(a)')
     &      '# item id, menu, data & reptng lines, USC, date'
        elseif(idbver(idbh).eq.2)then
          write(IFA,'(a)')'# item id, menu, date'
        endif

C Scan through all items checking for itcat()=icl, this re-orders items
C in binary if a subsequent re-read from ascii is done.
         do 34 iit=1,nbitem(idbh)
           IREC=irsit(iit)

C If repeating data has changed (constr db is an example) for this item
C use calls to RCVITEM and PRSMLC rather than reading from db.
           if(bkdocok.and.(bkdoctyp(1:2).eq.'IL').and.
     &       (indexbk.eq.iit))then
             call RCVITEM(idbh,class,item,iclr,iitr)

C Debug.
C             write(6,*) idbh,class,item,iclr,iitr,icl,iit

           else
             if(idbver(idbh).eq.1)then
               READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),
     &           itmenu(iit),itdate(iit),itcat(iit),nbitdoc,
     &           nbitdat(iit),nbitrep(iit),nbitusc(iit),ibitusc(iit)
             else
               READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),
     &           itmenu(iit),itdate(iit),itcat(iit),
     &           nbitdat(iit),nbitrep(iit),nbitusc(iit),ibitusc(iit)
             endif
           endif
           if(itcat(iit).eq.icl)then

C If adjusting number of layers use nbkdoc for nbitrep.
             if(idbver(idbh).eq.1)then
               if(bkdocok.and.(bkdoctyp(1:2).eq.'IL').and.
     &           (indexbk.eq.iit))then
                 write(IFA,'(6a,i2,a,i2,a,i2,a,i2,2a)')'*ITEM',tab,
     &             ittag(iit)(1:lnblnk(ittag(iit))),tab,
     &             itmenu(iit)(1:lnblnk(itmenu(iit))),tab,
     &             nbitdat(iit),tab,nbkdoc,tab,nbitusc(iit),tab,
     &             ibitusc(iit),tab,itdate(iit)(1:lnblnk(itdate(iit)))
               else
                 write(IFA,'(6a,i2,a,i2,a,i2,a,i2,2a)')'*ITEM',tab,
     &             ittag(iit)(1:lnblnk(ittag(iit))),tab,
     &             itmenu(iit)(1:lnblnk(itmenu(iit))),tab,
     &             nbitdat(iit),tab,nbitrep(iit),tab,nbitusc(iit),tab,
     &             ibitusc(iit),tab,itdate(iit)(1:lnblnk(itdate(iit)))
               endif
             elseif(idbver(idbh).eq.2)then
               write(IFA,'(7a)')'*ITEM',tab,
     &           ittag(iit)(1:lnblnk(ittag(iit))),tab,
     &           itmenu(iit)(1:lnblnk(itmenu(iit))),tab,
     &           itdate(iit)(1:lnblnk(itdate(iit)))
             endif

C Item documentation.

C If using recovered data then just write out documentation, otherwise
C read documentation and then write.
             IREC=IREC+1
             if(bkdocok.and.(bkdoctyp(1:2).eq.'IL').and.
     &         (indexbk.eq.iit))then
               continue
             else

C Item documentation read from next record.
               READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,itdoc
             endif
             write(IFA,'(3a)') '*ID',tab,itdoc(1:lnblnk(itdoc))

             IREC=IREC+1
             if(bkdocok.and.(bkdoctyp(1:2).eq.'IL').and.
     &         (indexbk.eq.iit))then
               continue
             else
               read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,ituntstr,itsrcstr,
     &           itcnfstr
             endif
             if(lnblnk(ituntstr).gt.20)then
              write(IFA,'(7a)') '*USC',tab,ituntstr(1:lnblnk(ituntstr)),
     &         tab,itsrcstr,tab,itcnfstr(1:lnblnk(itcnfstr))
             else
              write(IFA,'(8a)') '*USC',tab,ituntstr(1:lnblnk(ituntstr)),
     &         tab,itsrcstr,tab,itcnfstr(1:lnblnk(itcnfstr)),
     &         ' # units srcs uncert'
             endif

C Read item fixed data.
             if(nbitdat(iit).gt.0)then
               do 37 m=1,nbitdat(iit)
                 IREC=IREC+1
                 if(bkdocok.and.(bkdoctyp(1:2).eq.'IL').and.
     &             (indexbk.eq.iit))then
                   continue
                 else
                   READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itdat(m)
                 endif
                write(IFA,'(3a)')'*DAT',tab,itdat(m)(1:lnblnk(itdat(m)))
  37           continue
             endif

C If there is repeating data get it. In the case of a different number
C of layers use recovered data (incrementing IREC with nbitrep).
             if(bkdocok.and.(bkdoctyp(1:2).eq.'IL').and.
     &         (indexbk.eq.iit))then
               do 138 n=1,nbkdoc
                write(IFA,'(3a)')'*REP',tab,itrep(n)(1:lnblnk(itrep(n)))
 138           continue
               IREC=IREC+nbitrep(iit)
             else
               if(nbitrep(iit).gt.0)then
                 do 38 n=1,nbitrep(iit)
                   IREC=IREC+1
                   READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)itrep(n)
                   write(IFA,'(3a)')'*REP',tab,
     &               itrep(n)(1:lnblnk(itrep(n)))
  38             continue
               endif
             endif
             write(IFA,'(a)')'*ENDITEM'
           endif
  34    continue
        write(IFA,'(a)')'*ENDCAT'
  42  continue
      write(IFA,'(a)')'*ENDDB'

C Close file before exiting.
      CALL ERPFREE(IFA,IOS)
      CALL ERPFREE(IFB,IOS)
      RETURN

    1 write(outs,'(a,I6,4a)') 'SCANDB error at record',IREC,
     &  ' of file ',dbtag(idbh),' when ',errmsg(1:lnblnk(errmsg))
      call edisp(iuout,outs)
      CALL ERPFREE(IFA,IOS)
      CALL ERPFREE(IFB,IOS)
      return
      end

C ************* recongdb
C recongdb reconfigure a generic db (change its length) via exporting to ascii 
C and re-importing. If no errors it exits with IFB still open.
      subroutine recongdb(IAF,IFB,LFB,idbh,IER)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Backup of documentation (for editing).
      common/tmpdocc/bkascidb
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)

      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,dbnote*72,db*12
      character bkascidb*72,fs*1,tfile*72,LFB*72
      logical XST,unixok

C First remove any existing temporary ascii file.
      ier=0
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      call esppid(ipid)
      write (tfile,'((3a,i7))') upath(1:lnblnk(upath)),fs,'.dbm',ipid
      call st2file(tfile,bkascidb)
      call FINDFIL(bkascidb,XST)
      if(XST)then
        call ERPFREE(IAF,IOS)
        call FPOPEN(IAF,IOS,1,0,bkascidb)
        call EFDELET(IAF,IOS)
      endif
      call usrmsg('Reconfiguring database.',' ','-')
      call MKAGDB(IAF,bkascidb,IFB,LFB,idbh,IER)
      if(ier.eq.0)then
        call usrmsg('Reconfiguring database..',' ','-')
      else
        call usrmsg('Problem reconfiguring database.',
     &    '(during creation of tmp ascii file)','W')
        ier=1
      endif
      call ERAGDB(0,IAF,bkascidb,IFB,LFB,idbh,IER)
      if(ier.eq.0)then
        call usrmsg('Reconfiguring database...',' ','-')
      else
        call usrmsg('Problem reconfiguring database.',
     &    '(during reading of tmp ascii file)','W')
        ier=1
      endif
      db=dbtag(idbh)
      call SCANDB(IFB,LFB,db,idbh,IER)
      if(ier.eq.0)then
        call usrmsg('Reconfiguring database...done.',' ','-')
        CALL ERPFREE(IFB,IOS)
        idbw=idbwidth(idbh)
        ier=0
        call EFOPRAN(IFB,LFB,idbw,1,IER)
      else
        call usrmsg('Problem reconfiguring database.',
     &    '(during scan of reconfigured db file)','W')
        ier=1
      endif
      return
      end

C ************* CKGDBTYPE
C CKGDBTYPE Open and check a generic binary db for its type (returned in the
C string dbtype) and its hash table index (returned in idbhash) and
C the sucessful width in idbw. Close IFB after sucessful opening.
C Return ier=0 if binary genericfile, ier=2 if ascii generic,
C ier=3 if legacy file.
      subroutine ckgdbtype(IFB,LGENDB,dbtype,idbhash,idbw,IER)
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN

      character dbtype*12,dbmenu*32,c20*20,outs*124,LGENDB*72
      character reqdbtype*12,OUTSTR*124
      integer ier

C Open the binary file and read the header.
      reqdbtype=dbtype
      CALL ERPFREE(IFB,IOS)
      iwidth=24
      ier=0
      call EFOPRAN(IFB,LGENDB,iwidth,1,IER)
      if(ier.ne.0)then
        ier=1
        CALL ERPFREE(IFB,IOS)
        return
      endif
      irec=1
      ier=0
      READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c20,dbtype,dbmenu,
     &  idbwidth,nbunit,nbsrc,nbcat,nbitem,nbdbdoc,lastr
      if(c20(1:16).eq.'*ESP-r binary db'.or.
     &   c20(1:16).eq.'*ESP-r ascii db ')then
        continue
      elseif(c20(1:16).eq.'*ESP-r ascii gdb')then
        if(c20(1:20).eq.'*ESP-r ascii gdb    ')then
          idbv=1
          call edisp(iuout,' Converting generic ascii version 1...')
        elseif(c20(1:20).eq.'*ESP-r ascii gdb 2  ')then
          idbv=2
          call edisp(iuout,' Converting generic ascii version 2...')
        endif
        CALL ERPFREE(IFB,IOS)
        CALL EFOPSEQ(IFB,LGENDB,1,IER)
        CALL STRIPC(IFB,OUTSTR,99,ND,0,'gen db header',IER)
        if(outstr(1:16).ne.'*ESP-r ascii gdb')then
          call edisp(iuout,' Not a generic Ascii database file. ')
          CALL ERPFREE(IFB,IOS)
          ier=1
          return
        endif
        CALL STRIPC(IFB,OUTSTR,99,ND,0,'db id line',IER)
        K=0
        call egetp(outstr,K,dbtype,'W','db tag',IER)
        call egetp(outstr,K,dbmenu,'W','db menu',IER)
        call egetwi(outstr,K,idbwidth,1,65,'W','idbwidth',IER)
        call egetwi(outstr,K,nbunit,1,24,'W','nb units',IER)
        call egetwi(outstr,K,nbsrc,1,24,'W','nb sources',IER)
        call egetwi(outstr,K,nbcat,1,MGCL,'W','nb cat',IER)
        call egetwi(outstr,K,nbitem,1,MGIT,'W','nb items',IER)

C If no error reading first lines of db then flag for ascii type
C and then set idbhash
        if(ier.eq.0)then
          ier=2
          goto 42
        else
          call edisp(iuout,
     &      ' Problem scanning header of Ascii database file. ')
          CALL ERPFREE(IFB,IOS)
          ier=1
          return
        endif

        return
      else

C Test for legacy files.
        if(reqdbtype(1:3).eq.'any')reqdbtype=dbtype
        if(reqdbtype(1:9).eq.'pressc_db')then

C If one item on first line and 10 items on 2nd line then it could be
C a pressure db file.
          CALL ERPFREE(IFB,IOS)
          CALL EFOPSEQ(IFB,LGENDB,1,IER)
          CALL STRIPC(IFB,OUTSTR,99,ND1,1,'no coef',IER)
          CALL STRIPC(IFB,OUTSTR,99,ND2,1,'press db data 1st',IER)
          if(ND1.eq.1.and.ND2.eq.10)then
            CALL ERPFREE(IFB,IOS)
            idbhash=10
            call edisp(iuout,' Found legacy wind pressure db...')
            ier=3
            return
          else
            CALL ERPFREE(IFB,IOS)
            idbhash=0
            call edisp(iuout,' Found unknown file type...')
            ier=1
            return
          endif
        elseif(reqdbtype(1:8).eq.'profiles')then

C If one item on first line and at least 2 items on 2nd line then it could be
C a profiles db file.
          CALL ERPFREE(IFB,IOS)
          CALL EFOPSEQ(IFB,LGENDB,1,IER)
          CALL STRIPC(IFB,OUTSTR,99,ND1,1,'no items',IER)
          CALL STRIPC(IFB,OUTSTR,99,ND2,1,'NCG,DESC',IER)
          if(ND1.eq.1.and.ND2.ge.2)then
            CALL ERPFREE(IFB,IOS)
            idbhash=11
            call edisp(iuout,' Found legacy event profiles db...')
            ier=3
            return
          endif
        elseif(reqdbtype(1:8).eq.'material')then

C Check if ASCII materials file.
C It might be a text materials db file.
          CALL EFOPSEQ(IFB,LGENDB,1,IER)
          CALL STRIPC(IFB,OUTSTR,99,ND1,1,'no classes',IER)
          CALL STRIPC(IFB,OUTSTR,99,ND2,1,'IC NE NDESC',IER)
          if(ND1.eq.1.and.ND2.ge.3)then
            CALL ERPFREE(IFB,IOS)
            idbhash=2
            call edisp(iuout,' Found legacy materials db...')
            ier=3
            return
          else
            CALL ERPFREE(IFB,IOS)
            idbhash=0
            call edisp(iuout,' Found unknown file type...')
            ier=1
            return
          endif
        elseif(reqdbtype(1:11).eq.'multicon_db')then

C If one item on first line and 4 items on 2nd line and at least 3 items
C on the 3rd line then it could be a construction db file.
          CALL ERPFREE(IFB,IOS)
          CALL EFOPSEQ(IFB,LGENDB,1,IER)
          CALL STRIPC(IFB,OUTSTR,99,ND1,1,'constr db items',IER)
          CALL STRIPC(IFB,OUTSTR,99,ND2,1,'constr descr',IER)
          CALL STRIPC(IFB,OUTSTR,99,ND3,1,'layer',IER)
          if(ND1.eq.1.and.ND2.eq.4.and.ND3.ge.3)then
            CALL ERPFREE(IFB,IOS)
            idbhash=1
            call edisp(iuout,' Found legacy construction db...')
            ier=3
            return
          else
            CALL ERPFREE(IFB,IOS)
            idbhash=0
            call edisp(iuout,' Found unknown file type...')
            ier=1
            return
          endif
        else
          continue
        endif
      endif

C Depending on dbtyp set the position in the common block has
C table.
  42  if(dbtype(1:11).eq.'multicon_db')then
        idbhash=1
      elseif(dbtype(1:8).eq.'material')then
        idbhash=2
      elseif(dbtype(1:7).eq.'optical')then
        idbhash=3
      elseif(dbtype(1:8).eq.'acoustic')then
        idbhash=5
      elseif(dbtype(1:12).eq.'colourimetry')then
        idbhash=6
      elseif(dbtype(1:9).eq.'micotoxin')then
        idbhash=7
      elseif(dbtype(1:9).eq.'envir_imp')then
        idbhash=8
      elseif(dbtype(1:9).eq.'transport')then
        idbhash=9
      elseif(dbtype(1:9).eq.'pressc_db')then
        idbhash=10
      elseif(dbtype(1:8).eq.'profiles')then
        idbhash=11
      elseif(dbtype(1:8).eq.'plant_db')then
        idbhash=12
      elseif(dbtype(1:12).eq.'component_db')then
        idbhash=13
      else
        CALL ERPFREE(IFB,IOS)
        idbhash=0
        write(outs,'(3a)') 'Found unknown db type ',dbtype,LGENDB
        call edisp(iuout,outs)
        ier=1
        return
      endif
      idbw=idbwidth
      CALL ERPFREE(IFB,IOS)
      return

C Error message.
    1 write(outs,'(a,I6,a,i3)')'CKGDBTYPE: Error reading record ',IREC,
     &  ' of file unit ',ifb
      call edisp(iuout,outs)
      CALL ERPFREE(IFB,IOS)
      return
      end

C ************* SCANDB
C SCANDB Open and check a generic binary db.
      subroutine scandb(IFB,LGENDB,db,idbh,IER)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons: file log, list of db, current classifications,
C and current items then lengths of each list.
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

      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,db*12
      character dbnote*72,c8*8,c20*20,outs*124,LGENDB*72
      character unitchr*4,unitstr*12,unitnote*72,srcchr*4,srcnote*248
      character cattag*12,cattype*8,catmenu*32,catdate*24,catdoc*248
      character errmsg*48
      integer ier

C Open the binary file and read the header if not already done so.
      CALL ERPFREE(IFB,IOS)
      idbw=idbwidth(idbh)
      ier=0
      call EFOPRAN(IFB,LGENDB,idbw,1,IER)
      if(ier.ne.0)return

      errmsg='reading how many tags menus units etc.'
      irec=1
      READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c20,dbtag(idbh),dbmenu(idbh),
     &  idbwidth(idbh),nbunit(idbh),nbsrc(idbh),nbcat(idbh),
     &  nbitem(idbh),nbdbdoc(idbh),lastr(idbh)
      if(c20(1:16).ne.'*ESP-r binary db'.and.
     &   c20(1:15).ne.'*ESP-r ascii db')then
        call edisp(iuout,' Not a generic binary database file. ')
        CALL ERPFREE(IFB,IOS)
        ier=1
        return
      endif
      if(db(1:lnblnk(db)).ne.dbtag(idbh)(1:lnblnk(dbtag(idbh))))then
        call edisp(iuout,' Not same or expected db type. ')
        CALL ERPFREE(IFB,IOS)
        ier=2
        return
      endif

      errmsg='reading database date and note.'
      IREC=2
      READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)dbdate(idbh),dbnote(idbh)

      if(dbtag(idbh)(1:11).eq.'multicon_db')then
        call usrmsg(' Reading construction db...',' ','-')
      elseif(dbtag(idbh)(1:8).eq.'material')then
        call usrmsg(' Reading material db...',' ','-')
      elseif(dbtag(idbh)(1:7).eq.'optical')then
        call usrmsg(' Reading optical db...',' ','-')
      elseif(dbtag(idbh)(1:8).eq.'acoustic')then
        call usrmsg(' Reading acoustic db...',' ','-')
      elseif(dbtag(idbh)(1:12).eq.'colourimetry')then
        call usrmsg(' Reading colourimetry db...',' ','-')
      elseif(dbtag(idbh)(1:9).eq.'micotoxin')then
        call usrmsg(' Reading micotoxin db...',' ','-')
      elseif(dbtag(idbh)(1:9).eq.'envir_imp')then
        call usrmsg(' Reading environmental impact db...',' ','-')
      elseif(dbtag(idbh)(1:9).eq.'transport')then
        call usrmsg(' Reading transport db...',' ','-')
      elseif(dbtag(idbh)(1:9).eq.'pressc_db')then
        call usrmsg(' Reading wind pressure db...',' ','-')
      elseif(dbtag(idbh)(1:8).eq.'profiles')then
        call usrmsg(' Reading event profiles db...',' ','-')
      elseif(dbtag(idbh)(1:8).eq.'plant_db')then
        call usrmsg(' Reading plant template db...',' ','-')
      else
        call edisp(iuout,' Reading unknown db...')
      endif

C Units and Sources.
      if(nbunit(idbh).gt.0.and.nbunit(idbh).le.12)then
        errmsg='reading unit char unit string & note.'
        IREC=2
        do 4 inbu=1,nbunit(idbh)
          IREC=IREC+1
          READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,unitchr(inbu),
     &      unitstr(inbu),unitnote(inbu)
  4     continue
      else

C Debug.
C        write(6,*) 'In db ',idbh,' found ',nbunit(idbh),' units.'

      endif
      if(nbsrc(idbh).gt.0.and.nbsrc(idbh).le.24)then
        errmsg='reading source char source note.'
        IREC = 2 + nbunit(idbh)
        do 5 inbsrc=1,nbsrc(idbh)

C Read next record for srcnote.
          IREC=IREC+1
          READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,srcchr(inbsrc),
     &      srcnote(inbsrc)
  5     continue
      else

C Debug.
C        write(6,*) 'In db ',idbh,' found ',nbsrc(idbh),' sources.'

      endif

C Db and attribute help records.
      if(nbdbdoc(idbh).ge.1.and.nbdbdoc(idbh).le.10)then
        errmsg='reading database documentation.'
        IREC = 2+nbunit(idbh)+nbsrc(idbh)
        do 51 i=1,nbdbdoc(idbh)
          IREC = IREC+1
          read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,dbdoc(i)
  51    continue
      else

C Debug.
C        write(6,*) 'In db ',idbh,' found ',nbdbdoc(idbh),' doc lines.'

      endif

C Read start record of each category.
      errmsg='reading start record for each category.'
      IREC = 3+nbunit(idbh)+nbsrc(idbh)+nbdbdoc(idbh)
      read(IFB,REC=IREC,IOSTAT=IOS,ERR=1) (irscat(j),j=1,24)
      IREC=IREC+1
      if(nbcat(idbh).gt.24)then
        read(IFB,REC=IREC,IOSTAT=IOS,ERR=1) (irscat(j),j=25,MGCL)
      endif

C Read start record of each item.
      call updirsit(IFB,idbh,'R')

C Skip to first category record and read cat data.
      icl=0
      iit=0
      do 42 k=1,nbcat(idbh)
        errmsg='reading category type menu date etc.'
        icl=icl+1
        IREC=irscat(icl)
        read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(icl),cattype(icl),
     &    catmenu(icl),nbcatdoc(icl),nbcatit(icl),catdate(icl)

C Debug.
C        write(6,*)'scan ',cattag(icl),
C     &      catmenu(icl),nbcatdoc(icl),nbcatit(icl)

        errmsg='reading category documentation.'
        do 52 i=1,nbcatdoc(icl)
          IREC=IREC+1
          read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,catdoc(i)
  52    continue

C For each item in category... 
        do 34 i=1,nbcatit(icl)
          iit=iit+1
          call rgenitem(IFB,idbh,iit,ier)
  34    continue
  42  continue

C If no errors flag the "ok" tag for the file type as true.
      if(ier.eq.0)then
        if(dbtag(idbh)(1:11).eq.'multicon_db')then
          gendbok(1)=.true.
        elseif(dbtag(idbh)(1:8).eq.'material')then
          gendbok(2)=.true.
        elseif(dbtag(idbh)(1:7).eq.'optical')then
          gendbok(3)=.true.
        elseif(dbtag(idbh)(1:8).eq.'acoustic')then
          gendbok(5)=.true.
        elseif(dbtag(idbh)(1:12).eq.'colourimetry')then
          gendbok(6)=.true.
        elseif(dbtag(idbh)(1:9).eq.'micotoxin')then
          gendbok(7)=.true.
        elseif(dbtag(idbh)(1:9).eq.'envir_imp')then
          gendbok(8)=.true.
        elseif(dbtag(idbh)(1:9).eq.'transport')then
          gendbok(9)=.true.
        elseif(dbtag(idbh)(1:9).eq.'pressc_db')then
          gendbok(10)=.true.
        elseif(dbtag(idbh)(1:8).eq.'profiles')then
          gendbok(11)=.true.
        elseif(dbtag(idbh)(1:8).eq.'plant_db')then
          gendbok(12)=.true.
        elseif(dbtag(idbh)(1:12).eq.'component_db')then
          gendbok(13)=.true.
        endif
      endif

C Close file before exiting.
      CALL ERPFREE(IFB,IOS)
      RETURN

C Error message.
    1 write(outs,'(a,I6,4a)') 'SCANDB error at record',IREC,
     &  ' of file ',dbtag(idbh),' when ',errmsg(1:lnblnk(errmsg))
      call edisp(iuout,outs)
      CALL ERPFREE(IFB,IOS)
      return
      end


C ************* SCANDBHD
C SCANDBHD Scan a generic binary db header (file assumed to be open).
C Recover data for commons gendb1, gendb2, gendb3, gendb4, gendb6, 
C gendb8, gendb8a, gendbu, gendbs
      subroutine scandbhd(IFB,idbh,IER)
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons: file log, list of db, current classifications,
C and current items then lengths of each list.
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
      common/gendbu/unitchr(24),unitstr(24),unitnote(24)
      common/gendbs/srcchr(24),srcnote(24)

      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248
      character dbnote*72,c8*8,c20*20,outs*124
      character unitchr*4,unitstr*12,unitnote*72,srcchr*4,srcnote*248
      character ittag*12,itmenu*32,itdate*24,itdoc*248
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character cattag*12,cattype*8,catmenu*32,catdate*24,catdoc*248

C Read the first record and confirm that it is a generic db file.
      irec=1
      READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c20,dbtag(idbh),dbmenu(idbh),
     &  idbwidth(idbh),nbunit(idbh),nbsrc(idbh),nbcat(idbh),
     &  nbitem(idbh),nbdbdoc(idbh),lastr(idbh)
      if(c20(1:16).ne.'*ESP-r binary db'.and.
     &   c20(1:15).ne.'*ESP-r ascii db')then
        call edisp(iuout,' Not a generic binary database file. ')
        CALL ERPFREE(IFB,IOS)
        ier=1
        return
      endif

      IREC=2
      READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)dbdate(idbh),dbnote(idbh)

C Units and Sources.
      if(nbunit(idbh).gt.0)then
        IREC=2
        do 4 inbu=1,nbunit(idbh)
          IREC=IREC+1
          READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,unitchr(inbu),
     &      unitstr(inbu),unitnote(inbu)
  4     continue
      endif
      if(nbsrc(idbh).gt.0)then
        IREC = 2+nbunit(idbh)
        do 5 inbsrc=1,nbsrc(idbh)

C Read next record for srcnote.
          IREC=IREC+1
          READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,srcchr(inbsrc),
     &      srcnote(inbsrc)
  5     continue
      endif

C Db and attribute help records.
      IREC = 2+nbunit(idbh)+nbsrc(idbh)
      do 51 i=1,nbdbdoc(idbh)
        IREC=IREC + 1
        read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,dbdoc(i)
  51  continue

C Read start record of each category.
      IREC = 3+nbunit(idbh)+nbsrc(idbh)+nbdbdoc(idbh)
      read(IFB,REC=IREC,IOSTAT=IOS,ERR=1) (irscat(j),j=1,24)
      IREC=IREC+1
      if(nbcat(idbh).gt.24)then
        read(IFB,REC=IREC,IOSTAT=IOS,ERR=1) (irscat(j),j=25,MGCL)
      endif

C Read start record of each item.
      call updirsit(IFB,idbh,'R')

C Skip to first category record and read cat data.
      icl=0
      iit=0
      do 42 k=1,nbcat(idbh)
        icl=icl+1
        IREC=irscat(icl)
        read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)cattag(icl),cattype(icl),
     &    catmenu(icl),nbcatdoc(icl),nbcatit(icl),catdate(icl)

        do 52 i=1,nbcatdoc(icl)
          IREC=IREC+1
          read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,catdoc(i)
  52    continue

C For each item in category read tag, menu, category (but not data). 
        do 34 i=1,nbcatit(icl)
          iit=iit+1
          itdate(iit)=' '
          IREC=irsit(iit)
          READ(IFB,REC=IREC,IOSTAT=IOS,ERR=1)ittag(iit),itmenu(iit),
     &      itdate(iit),itcat(iit)
  34    continue
  42  continue

      RETURN

C Error message.
    1 write(outs,'(a,I6,a,i3)') 'SCANDBHD Error reading record ',IREC,
     &  ' of file unit ',ifb
      call edisp(iuout,outs)
      CALL ERPFREE(IFB,IOS)
      return
      end

C ************* REPGDB
C REPGDB Read a generic binary db and writes out an ascii report.
      subroutine repgdb(itrc,itu,IFB,BFIL,idbh,ICATSEL,IER)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/exporttg/xfile,tg,delim

C Generic db commons:
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

      dimension ICATSEL(MGCL)

      character BFIL*72,tab*1,xfile*144,tg*1,delim*1
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24,itdoc*248
      character itdat*248,itrep*248,ittag*12,itmenu*32,itdate*24
      character dbnote*72,c8*8,outs*124
      character ituntstr*42,itsrcstr*1,itcnfstr*42,srcchr*4,srcnote*248
      character unitchr*4,unitstr*12,unitnote*72
      logical match
      integer ier

C Setup a tab character.
      if(delim.eq.'-')then
        tab=' '
      elseif(delim.eq.'T')then
        tab=CHAR(9)
      elseif(delim.eq.'C')then
        tab=','
      elseif(delim.eq.'S')then
        tab=' '
      endif

C Open the binary file and use scandbhd to read the header.
      CALL ERPFREE(IFB,IOS)
      idbw=idbwidth(idbh)
      ier=0
      call EFOPRAN(IFB,BFIL,idbw,1,IER)
      call scandbhd(IFB,idbh,ier)

C Open the ascii file and write headder.
      if(dbtag(idbh)(1:11).eq.'multicon_db')then
        call edisp(itu,'Construction database contents:')
      elseif(dbtag(idbh)(1:8).eq.'material')then
        call edisp(itu,'Material database contents:')
      elseif(dbtag(idbh)(1:7).eq.'optical')then
        call edisp(itu,'Optical database contents:')
      elseif(dbtag(idbh)(1:8).eq.'acoustic')then
        call edisp(itu,'Acoustic database contents:')
      elseif(dbtag(idbh)(1:12).eq.'colourimetry')then
        call edisp(itu,'Colourimetry database contents:')
      elseif(dbtag(idbh)(1:9).eq.'micotoxin')then
        call edisp(itu,'Micotoxin database contents:')
      elseif(dbtag(idbh)(1:9).eq.'envir_imp')then
        call edisp(itu,'Environmental impact database contents:')
      elseif(dbtag(idbh)(1:9).eq.'transport')then
        call edisp(itu,'Transport database contents:')
      elseif(dbtag(idbh)(1:9).eq.'pressc_db')then
        call edisp(itu,'Wind pressure coef database contents:')
      elseif(dbtag(idbh)(1:8).eq.'profiles')then
        call edisp(itu,'Event profiles database contents:')
      else
        call edisp(itu,'Contents of an unknown db:')
      endif
      write(outs,'(3a)') 'Named:',tab,
     &  dbmenu(idbh)(1:lnblnk(dbmenu(idbh)))
      call edisp(itu,outs)
      write(outs,'(3a)') 'File:',tab,BFIL(1:lnblnk(BFIL))
      call edisp(itu,outs)
      write(outs,'(3a)') ' which was last modified on:',tab,dbdate(idbh)
      call edisp(itu,outs)
      write(outs,'(a,i2,a,i3,a)') ' and contains ',nbcat(idbh),
     &  ' categories and ',nbitem(idbh),' items.'
      call edisp(itu,outs)

      write(outs,'(a,a1,a)')'Aide memoire:',tab,
     &  dbnote(idbh)(1:lnblnk(dbnote(idbh)))
      call edisp(itu,outs)

C Write out Units and Sources.
      if(nbunit(idbh).gt.0.and.itrc.gt.1)then
        do 4 inbu=1,nbunit(idbh)
          write(outs,'(7a)')'Units:',tab,unitchr(inbu)(1:1),tab,
     &      unitstr(inbu)(1:lnblnk(unitstr(inbu))),tab,
     &      unitnote(inbu)(1:lnblnk(unitnote(inbu)))
          call edisp(itu,outs)
  4     continue
      endif
      if(nbsrc(idbh).gt.0.and.itrc.gt.1)then
        do 5 inbsrc=1,nbsrc(idbh)
          write(outs,'(3a)')'Source:',tab,srcchr(inbsrc)(1:1)
          call edisp(itu,outs)
          call edisp248(itu,srcnote(inbsrc),80)
  5     continue
      endif

C Write out the db documentation records. If db documentation needs to
C be updated then write out alternative text.
      call edisp(itu,' ')
      call edisp(itu,'Database documentation:')
      do 51 i=1,nbdbdoc(idbh)
        call edisp248(itu,dbdoc(i),80)
  51  continue
      call edisp(itu,' ')

C Loop all categories and report on user selected subset.
      do 42 icl=1,nbcat(idbh)
        match=.false.
        do 43 iclx=1,nbcat(idbh)
          if(ICATSEL(iclx).eq.icl)match=.true.
  43    continue
        if(.NOT.match) goto 42

        call edisp(itu,'________________________________')
        write(outs,'(10a,i2,4a)')'Category id:',tab,
     &    cattag(icl)(1:lnblnk(cattag(icl))),tab,'Name:',tab,
     &    catmenu(icl)(1:lnblnk(catmenu(icl))),tab,'Items:',
     &    tab,nbcatit(icl),tab,'Date:',tab,catdate(icl)
        call edisp(itu,outs)

C If this category documentation to be updated then use bkdoc text.
        if(itrc.gt.1)then
          IREC=irscat(icl)
          call edisp(itu,'Category documentation:')
          do 53 i=1,nbcatdoc(icl)
            IREC=IREC+1
            read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,catdoc(i)
            call edisp248(itu,catdoc(i),80)
  53      continue
        endif

C List standard item details for this category.
        call edisp(itu,'Items:')
        do 34 iit=1,nbitem(idbh)
          if(itcat(iit).eq.icl)then
            call rgenitem(IFB,idbh,iit,ier)
            write(outs,'(11a)')'Id:',tab,
     &        ittag(iit)(1:lnblnk(ittag(iit))),tab,
     &        'Name:',tab,itmenu(iit)(1:lnblnk(itmenu(iit))),
     &        tab,'Date:',tab,itdate(iit)(1:lnblnk(itdate(iit)))
            call edisp(itu,outs)

C Item documentation, if edited write out new text.
            call edisp248(itu,outs,96)

C Depending on database type need to write out various titles and formats.
C If there is item data (remember no tab for item data).
            if(nbitdat(iit).gt.0)then
              do 37 m=1,nbitdat(iit)
              write(outs,'(3a)')'Data:',tab,itdat(m)(1:lnblnk(itdat(m)))
              call edisp(itu,outs)
  37          continue
            endif

C If there is repeating data.
            if(nbitrep(iit).gt.0)then
              do 38 n=1,nbitrep(iit)
                write(outs,'(3a)')' ',tab,itrep(n)(1:lnblnk(itrep(n)))
                call edisp(itu,outs)
  38          continue
            endif
            call edisp(itu,' ')
          endif
  34    continue
  42  continue
      call edisp(itu,' ')

C Close file before exiting.
      RETURN

    1 write(outs,'(a,I6,a,i3)') 'REPGDB Error reading record ',IREC,
     &  ' of file unit ',ifb
      call edisp(iuout,outs)
      CALL ERPFREE(IFB,IOS)
      return
      end

C ************* SELCAT
C SELCAT Given a generic db, provides a list of classifications.
C Itwhich is the index of the item selected from the database and
C Icwhich is its category. Ctype filters for particular cagegory
C types (e.g. ALL shows all, MATLCA shows only material impacts).
      subroutine selcat(IFB,idbh,ctype,itwhich,icwhich,IER)
#include "epara.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons: file log, list of db, current classifications,
C and current items then lengths of each list.
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)

      DIMENSION CLDES(35)
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character dbnote*72,CLDES*38,KEY*1,head*37,ctype*8,ct*8
      integer NCO,ICO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='selcat'  ! set for subroutine

C Setup for menu.
C Gather the names of the various classes and present this list.
      call scandbhd(IFB,idbh,ier)
      if(ier.ne.0)goto 1
  39  MHEAD=3
      MCTL=3
      ILEN=nbcat(idbh)
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
  40  ICO=-4
      WRITE(CLDES(1),'(A,A)')'  Note: ',dbnote(idbh)(1:30)
      CLDES(2)=  '3 Documentation                      '
      CLDES(3)=  '  Catgory description       No. Items'
      M=MHEAD

C Build up a display.
      do 34 i=1,ILEN
        if(i.GE.IST.AND.(i.LE.(IST+MIFULL)))then
          ct=cattype(i)
          if(ctype(1:3).eq.'ALL')then
            M=M+1
            CALL EMKEY(M-MHEAD,KEY,IER)
            WRITE(CLDES(M),'(A1,1X,A,2X,I2)')KEY,catmenu(i)(1:32),
     &        nbcatit(i)
          elseif(ctype(1:lnblnk(ctype)).eq.ct(1:lnblnk(ct)))then
            M=M+1
            CALL EMKEY(M-MHEAD,KEY,IER)
            WRITE(CLDES(M),'(A1,1X,A,2X,I2)')KEY,catmenu(i)(1:32),
     &        nbcatit(i)
          else
            M=M+1
            CALL EMKEY(M-MHEAD,KEY,IER)
            WRITE(CLDES(M),'(A1,1X,A)')KEY,' Not Applicable '
          endif
        endif
  34  continue

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
         CLDES(M+1)='  __________________________  '
      ELSE
        WRITE(CLDES(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page ---- Part: ',I2,' of ',I2,' ---')
      ENDIF
      CLDES(M+2)='? Help                        '
      CLDES(M+3)='- Exit                        '
      NCO=M+MCTL

C Help text for this menu.
      helptopic='dbm_select_category'
      call gethelptext(helpinsub,helptopic,nbhelp)

      write(head,'(a,a)') ' Db: ',dbmenu(idbh)

C Now display the menu.
      CALL EMENU(head,CLDES,NCO,ICO)
      IF(ICO.EQ.NCO)THEN
        goto 99
      ELSEIF(ICO.EQ.1)THEN

C Display/edit the db note:
        call edisp(iuout,'The aide memoire for this db is: ')
        call edisp(iuout,dbnote(idbh))
      ELSEIF(ICO.EQ.2)THEN

C Db documentation. Strip off initial ":" from the string before displaying.
        do 151 i=1,nbdbdoc(idbh)
          call edisp248(iuout,dbdoc(i),80)
 151    continue
      ELSEIF(ICO.EQ.(NCO-1))THEN
        helptopic='dbm_select_category'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('category list',nbhelp,'-',0,0,IER)
      ELSEIF(ICO.EQ.(NCO-2))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        goto 39
      ELSEIF(ICO.GT.MHEAD.AND.ICO.LT.(NCO-MCTL+1))THEN

C Return the index of a category and present items list. If user has
C selected one then pass it back to calling subroutine.
        CALL KEYIND(NCO,ICO,IFOC,IO)
        icwhich=IFOC
        icl=icwhich
        CALL EPMENSV
        call SELITEM(IFB,idbh,itwhich,icl,IER)
        CALL EPMENRC
        if(itwhich.ne.0)then
          icwhich=icl
          return
        endif
        goto 39
      else
        ICO=-1
        goto 40
      endif
      call usrmsg(' ',' ','-')
      goto 40

  99  RETURN

    1 call edisp(iuout,'Error updating data record')
      return
      end

C ************* SELITEM
C SELITEM Given a generic db, provides a list of items in category ic.
C Itwhich is the index of the db item selected.
      subroutine selitem(IFB,idbh,itwhich,ic,IER)
#include "epara.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Generic db commons: file log, list of db, current classifications,
C and current items then lengths of each list.
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

      character dbtag*12,db*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24
      character ittag*12,itmenu*32,dbnote*72,c8*8,outs*124,item*12
      DIMENSION PDBM(35),ipntrm(35)

      character KEY*1,PDBM*34,head*42
      logical OK,dok
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='selitem'  ! set for subroutine

C Assumes that the db file is open, get gendb6 via scandbhd and
C then gather the catg documentation. 
      call scandbhd(IFB,idbh,ier)
      IREC=irscat(ic)
      do 52 i=1,nbcatdoc(ic)
        IREC=IREC+1
        read(IFB,REC=IREC,IOSTAT=IOS,ERR=1)c8,catdoc(i)
  52  continue

C Gather the names of the various items in catg and present list.
  39  MHEAD=5
      MCTL=3
      ILEN=nbcatit(ic)
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup, clear the item:selection hash list.
  40  INO=-4
      do 33 i=1,35
        ipntrm(i)=0
  33  continue
      lm=MIN0(lnblnk(catmenu(ic)),15)
      WRITE(PDBM(1),'(3a,i2,a)') '  Catg: ',catmenu(ic)(1:lm),
     &  ' (',nbcatit(IC),' items)'
      WRITE(PDBM(2),'(4A)')'  Type: ',cattype(IC),' tag: ',cattag(IC)
      PDBM(3)= '3 Documentation                '
      PDBM(4)= '  _____________________________'
      PDBM(5)= '  Item description             '
      M=MHEAD

C Build up a display of entities within this catg.
      j=0
      do 34 i=1,nbitem(idbh)
        if(itcat(i).eq.ic)then
          j=j+1
          if(j.GE.IST.AND.(j.LE.(IST+MIFULL)))then
            M=M+1
            CALL EMKEY(M-MHEAD,KEY,IER)
            WRITE(PDBM(M),'(a1,1x,a)')KEY,itmenu(i)(1:32)

C Remember which item index is with each m.
            ipntrm(m)=i
          endif
        endif
  34  continue
      NC=M

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
         PDBM(NC+1)='  _____________________________'
      ELSE
        WRITE(PDBM(NC+1),15)IPM,MPM 
   15   FORMAT   ('0 page -- Part: ',I2,' of ',I2,' --')
      ENDIF
      PDBM(NC+2)='? help                           '
      PDBM(NC+3)='- exit to categories             '
      NITMS=NC+3

C Help text for this menu.
      helptopic='dbm_category_contents'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display the elements of the classification in a menu.
      INO=-4
      write(head,'(a,a)')'  Items in ',
     &  dbmenu(idbh)(1:lnblnk(dbmenu(idbh)))

      CALL EMENU(head,PDBM,NITMS,INO)
      IF(INO.EQ.NITMS)THEN
        return
      elseif(INO.eq.4)then

C Documentation. Editing if ACTION = 'M'.
C Help. Strip off the first char ":" from the string before displaying.
        do 152 i=1,nbcatdoc(ic)
          call edisp248(iuout,catdoc(i),80)
 152    continue
        goto 40
      elseif(INO.eq.NITMS-1)then

C Help.
        helptopic='dbm_category_contents'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('category list',nbhelp,'-',0,0,IER)
        goto 40
      elseif(INO.eq.NITMS-2)then

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        goto 39
      elseif(INO.gt.MHEAD.and.INO.lt.NITMS-MCTL+1)then

C Have selecte an item, decode and then....
        itwhich=ipntrm(ino)
        write(item,'(a)')ittag(itwhich)

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
              goto 139
            endif
          endif
  35    continue

C If reached end of items and no match then return error condition.
 139    if(iit.eq.0.or.(iit.ne.itwhich))then
          call usrmsg('An item of an identical name found in ',
     &      'another category...or no item found. ','W')
        endif

C Depending on the db call appropriate tabular or editing facility.
C Note some editing variants may require EPMENSV/EPMENRC.
        if(dbtag(idbh)(1:11).eq.'multicon_db')then
          db=dbtag(idbh)
          call edmat2(db,IFB,idbh,ic,iit,'H',ier)
          dok=.true.
          call askok(' ',' Use this item?',OK,dok,nbhelp)
          if(OK)then
            itwhich=iit
            return
          else
            itwhich=0
          endif
        elseif(dbtag(idbh)(1:8).eq.'material')then
          db=dbtag(idbh)
          call edmat2(db,IFB,idbh,ic,iit,'H',ier)
          dok=.true.
          call askok(' ',' Use this item?',OK,dok,nbhelp)
          if(OK)then
            itwhich=iit
            return
          else
            itwhich=0
          endif
        elseif(dbtag(idbh)(1:7).eq.'optical')then
          call usrmsg('Optical database editing TBD.',' ','W')
        elseif(dbtag(idbh)(1:8).eq.'acoustic')then
          call edacous(IFB,idbh,ic,iit,'H',IER)
          dok=.true.
          call askok(' ',' Use this item?',OK,dok,nbhelp)
          if(OK)then
            itwhich=iit
            return
          else
            itwhich=0
          endif
        elseif(dbtag(idbh)(1:12).eq.'colourimetry')then
          call edcolour(IFB,idbh,ic,iit,'H',IER)
          dok=.true.
          call askok(' ',' Use this item?',OK,dok,nbhelp)
          if(OK)then
            itwhich=iit
            return
          else
            itwhich=0
          endif
        elseif(dbtag(idbh)(1:9).eq.'micotoxin')then
          call usrmsg('Micotoxin database editing TBD.',' ','W')
        elseif(dbtag(idbh)(1:9).eq.'envir_imp')then
          call epopenv(IFB,idbh,ic,iit,'H',IER)
          dok=.true.
          call askok(' ',' Use this item?',OK,dok,nbhelp)
          if(OK)then
            itwhich=iit
            return
          else
            itwhich=0
          endif
C          call edenvci(IFB,idbh,ic,iit,'H',IER)
        elseif(dbtag(idbh)(1:9).eq.'transport')then
          call edtransp(IFB,idbh,ic,iit,'H',IER)
          dok=.true.
          call askok(' ',' Use this item?',OK,dok,nbhelp)
          if(OK)then
            itwhich=iit
            return
          else
            itwhich=0
          endif
        elseif(dbtag(idbh)(1:9).eq.'pressc_db')then
          call usrmsg('Wind pressure database editing TDB.',' ','W')
        elseif(dbtag(idbh)(1:8).eq.'profiles')then
          call usrmsg('Event profiles database editing TBD.',' ','W')
        elseif(dbtag(idbh)(1:8).eq.'plant_db')then
          call usrmsg('Plant templates database editing TBD.',' ','W')
        else
          call usrmsg('Unknown database type.',' ','W')
        endif
        goto 39
      else
        INO=-1
        goto 40
      endif
      call usrmsg(' ',' ','-')
      goto 40

C Error trap.
   1  write(outs,'(a,I6,a,i3)') 'SELITEM Error reading record ',IREC,
     &  ' of file unit ',ifb
      call edisp(iuout,outs)
      CALL ERPFREE(IFB,IOS)
      ier=1
      return
      end

C ************* uniqtag
C uniqtag Checks if a proposed id is unique (within the current database).
C If ok=true then the proposed id string is ok. If act='i' then
C checks against item id strings and if act='c' then it checks
C against category id strings. After each call to uniqtag the
C calling code should issue a call scandbhd() if it relates to
C a different idbh.

      subroutine uniqtag(idbh,ptag,act,ok)
#include "esprdbfile.h"
      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      character catdoc*248,cattag*12,cattype*8,catmenu*32,catdate*24
      character ittag*12,itmenu*32,ptag*12,act*1
      logical ok

C Rescan the header of this database and then re-establish the item
C tags and categories before checking to see if item tag exists.
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)
      ok=.true.
      if(act.eq.'i')then
        do 42 i = 1,MGIT
          if(ittag(i)(1:2).ne.'  ')then
            if(ptag.eq.ittag(i))then
              ok=.false.
              return
            endif
          endif
  42    continue
      elseif(act.eq.'c')then

C For each of the categories in this db, recover the cattag data
C and compare with ptag to see if unique.
        IFB=igendbf(idbh)
        do 43 i = 1,nbcat(idbh)
          if(cattag(i)(1:2).ne.'  ')then
            if(ptag.eq.cattag(i))then
              ok=.false.
              return
            endif
          endif
  43    continue
      endif
      return

      end

C *********** prsunit
C prsunit Given current index of data field return strings for unitstr and unitnote.
      subroutine prsunit(idbh,index,ustr,unote)
#include "esprdbfile.h"

      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendbu/unitchr(24),unitstr(24),unitnote(24)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      character unitchr*4,unitstr*12,unitnote*72,itdate*24
      character ituntstr*42,itsrcstr*1,itcnfstr*42,itdoc*248
      character ustr*12,unote*72,ch*1

C Get character pointer for unit at the requested position within ituntstr
C and find matching unitchr. Begin by scanning the current db header.
C After each call to prsunit the calling code should issue a
C call scandbhd().
      ch=ituntstr(index:index)
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)
      do 11 iu=1,nbunit(idbh)
        if(unitchr(iu)(1:1).eq.ch(1:1))then
          ustr=unitstr(iu)
          unote=unitnote(iu)
          return
        endif
  11  continue
      return
      end


C *********** prssrc
C prssrc: given current item return string for source (snote). Assumes that
C the database has already been scanned as well as the item.
      subroutine prssrc(idbh,snote)
#include "esprdbfile.h"

      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendbs/srcchr(24),srcnote(24)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc

      character ituntstr*42,itsrcstr*1,itcnfstr*42,itdoc*248
      character ch*1,srcchr*4,srcnote*248,snote*248,itdate*24

C The current item source character (itsrcstr) is used to compare
C with all this databases source tags (srcchr) and when a match
C is found a 248char string snote is returned.
      ch=itsrcstr
      snote='No source found'
      do 11 iu=1,nbsrc(idbh)
        if(srcchr(iu)(1:1).eq.ch(1:1))then
          snote=srcnote(iu)
          return
        endif
  11  continue
      return
      end

C *********** prsmenu
C prsmenu Given current item name, return its menu text (itsmenu) and hash 
C index (itshash) and itscat is the category of the item (for confirmation
C of scanning). After each call to prsmenu the calling code should issue
C a call scandbhd().
      subroutine prsmenu(idbh,item,itsmenu,itshash,itscat)
#include "esprdbfile.h"

      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8/irsit(MGIT),itcat(MGIT)
      character ittag*12,itmenu*32,item*12,itsmenu*32

      if(item(1:4).eq.'none')then
        itsmenu='none'
        itshash=0
        itscat=0
        return
      endif

C Rescan the header of this database and then re-establish the item
C tags and categories before checking to see if `item` exists.
      IFB=igendbf(idbh)
      call scandbhd(IFB,idbh,ier)

      do 11 iu=1,nbitem(idbh)
        if(ittag(iu)(1:12).eq.item(1:12))then
          itsmenu=itmenu(iu)
          itshash=iu
          itscat=itcat(iu)
          return
        endif
  11  continue

C If reached this point then item not found.
      itsmenu='not found'
      itshash=0
      itscat=0
      return

      end
