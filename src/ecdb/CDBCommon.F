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
C
C What the code in this file does.
C The following common routines are available.
C   CDBScanDatabase  - scans the components database and returns data according
C     to the scan type. 'light' returns structural data only, 
C     'detailed' reads the full database into memory.
C   CDBSaveDatabase - writes a database held in memory to the standard database format.
C     alternatively different formats adopted (.csv,tab separated, etc.)
C   
C   CDBGetItemData  -  takes up to three strings 'domain_id','category_id' and 
C     'item_id', searches the database for a match and extracts 
C     the data if a match is found; only 'item_id' is mandatory, 
C     the other two strings can be used to search more efficiently;
C     the data is returned to a generic list.
C   CDBPsetList  -  Presents a list of parameter sets and returns a selected set.
C   CDBParamList -  Presents a list of parameters in a set and returns a selected parameter.
C   CDBParamEdit -  Allows a user to edit the data associated with a specific parameter.
C
C 
C ************************ SUBROUTINE ********************************** 
C ----------------------------------------------------------------------
      subroutine CDBScanDataBase(cdbunit,cdbfile,scantype,IER)
C ----------------------------------------------------------------------
C ***********************************************************************
C This routine scans an icon database and returns the following 
C structural data common data (in gencompdatabase.h):
C   
C   'light' scan:
C
C   cdbversion:    the version of the database
C   nbdomain:      the number of domains
C   domaintag(Max_Domain_Types):   a list of domain names (16 char)
C   nbcat(Max_Domain_Types):       number of categories in each domain,
C   cattag(Max_Domain_Types,Max_Categories):  name_id for each category (max 16 char),
C   catmenu(Max_Domain_Types,Max_Categories): corresponding menu entry for each category (max 32 char)
C   nbitem(Max_Domain_Types,Max_Categories):  number of items in each category. 
C   itemtag(Max_Domain_Types,Max_Categories,Max_Items):  name_id for each item (16 char),
C   itemmenu(Max_Domain_Types,Max_Categories,Max_Items): corresponding menu entry for each item (40 char)
C
C   <<global data to be added>>
C
C
C   'detailed' scan ('light scan' data +) :
C   itempointer(Max_Domain_Types,Max_Categories,Max_Items) ? pointer to compact component list entry
C
C   Attributes   
C   tagatr(?,Max_parameters,6)  
C   atrib(?,Max_parameters,3)
C   menuatr(?,Max_parameters)
C   item_text_desc(?,Max_text_Desc)
C
C   vert(?,Max_vert,2)
C   iedge(?,Max_edge,5)
C   idot(?,Max_dots,4)
C   iarc(?,Max_dots,7)
C   ilabel(?,Max_dots,4)
C   iatt(?,Max_conn_p,2)
C   labeltx(?,Max_dots)

      implicit none
#include "gencompDB.h"
#include "gencompDB_common.h"
      
      COMMON/OUTIN/IUOUT,IUIN

      character*16 all_itemtag(Max_DB_Items)
      
      character*16 tmp_domaintag,tmp_cattag,tmp_itemtag
      character*32 tmp_catmenu,tmp_domainmenu
      character*64 tmp_itemmenu
      
      CHARACTER OUTSTR*124,outs*124,WORD*24,WORD1*24,WORD2*24,PHRASE*48
      CHARACTER cdbfile*144, scantype*8,outs2*124
      CHARACTER colourstr*12,ctype*1

      integer nbd,nbc,nbi,total_DB_items,i,j,k,ic,ND,IER,IERx,ISTAT
      integer itp,textl
      integer iuout,iuin,natrib
      integer lnblnk,cdbunit,indxcolour,ival1,ival2,ival3,ival4
      INTEGER nbverts,nbedges,nbdots,nbarcs,nbatts,nblabels

      LOGICAL CONT,InDomain,InCategory
      
      REAL cdbv,val1,val2

      IER=0
      word=' '
      CONT=.TRUE.
      
C Open the components database file 

      Comp_DBase_Unit=cdbunit
      Comp_DBase_Name=cdbfile(1:LNBLNK(cdbfile))

      CALL EFOPSEQ(Comp_DBase_Unit,Comp_DBase_Name,1,IER)
      IF(IER.NE.0) THEN
        IER=1
        CALL ERPFREE(Comp_DBase_Unit,ISTAT)
        RETURN
      ENDIF

C Clear the parameter list. nbd is local counter of current domain.
      InDomain=.FALSE.
      InCategory=.FALSE.
      nbdomain=0
      nbd=0
      nbc=0
      nbi=0
      itp=0
      cdbversion=0.0
      total_DB_items=0
      do 3 i=1,Max_Domain_Types
        nbcat(i)=0
        domaintag(i)=' '
        domainmenu(i)=' '

C Clear domain structural data
        do 4 j=1,Max_Categories
          cattag(i,j)='  '
          catmenu(i,j)='  '
          nbitem(i,j)=0
          do 5 k=1,Max_Items
            itemtag(i,j,k)='  '
            itemmenu(i,j,k)='  '
            itempointer(i,j,k)=0
  5       continue
  4     continue
  3   continue

      do 6 ic=1,Max_DB_items
C Clear the returned parameter list.
        natribs(ic)=0
        do 8 i=1,Max_Parameters
          tagatr(ic,i,1)=' '
          tagatr(ic,i,2)=' '
          tagatr(ic,i,3)=' '
          tagatr(ic,i,4)=' '
          tagatr(ic,i,5)=' '
          tagatr(ic,i,6)=' '
          atrib(ic,i,1)=' '
          atrib(ic,i,2)=' '
          atrib(ic,i,3)=' '
          menuatr(ic,i)=' '
          n_text_desc(ic)=0
          
    8   continue

        do 10 i=1,Max_Text_Desc
          item_text_desc(ic,i)=' '
  10    continue
  
C Graphics here.
C Clear the returned graphics data list.
        nbvert(ic)=0
        nbedge(ic)=0
        nbdot(ic)=0
        nbarc(ic)=0
        nbatt(ic)=0
        nblabel(ic)=0
        do 12 k=1,Max_Vert
          vert(ic,k,1)=0.0
          vert(ic,k,2)=0.0
  12    continue
        do 14 k=1,Max_Edge
          iedge(ic,k,1)=0
          iedge(ic,k,2)=0
          iedge(ic,k,3)=0
          iedge(ic,k,4)=0
          iedge(ic,k,5)=0
  14    continue
          do 16 k=1,Max_Dots
          idot(ic,k,1)=0
          idot(ic,k,2)=0
          idot(ic,k,3)=0
          idot(ic,k,4)=0
          iarc(ic,k,1)=0
          iarc(ic,k,2)=0
          iarc(ic,k,3)=0
          iarc(ic,k,4)=0
          iarc(ic,k,5)=0
          iarc(ic,k,6)=0
          iarc(ic,k,7)=0
          ilabel(ic,k,1)=0
          ilabel(ic,k,2)=0
          ilabel(ic,k,3)=0
          labeltx(ic,k)=' '
  16    continue

  6   continue

C Check that the opened file is a components database and which version.
      CALL STRIPC(Comp_DBase_Unit,OUTSTR,99,ND,0,'check file',IER)
      K=0
      IF(IER.NE.0) CONT=.FALSE. 
      CALL EGETW(OUTSTR,K,WORD,'W','database id',IER)
      IF(WORD(1:11).EQ.'*Components')THEN
        if(ND.gt.1)then
          CALL EGETWR(OUTSTR,K,cdbv,0.,2.,'-','version',IER)
          cdbversion=cdbv
        endif
        WRITE(OUTS,'(A)')' '
        CALL EDISP(IUOUT,OUTS)
        WRITE(OUTS,'(A,F4.2,A)')
     &' Opened Components Database (Beta Version: ',cdbv,')'
        IF(scantype(1:4).eq.'open') CALL EDISP(IUOUT,OUTS)
      else
        WRITE(OUTS,'(3A)') 'cdb error, file: ',
     &Comp_DBase_Name(1:lnblnk(Comp_DBase_Name)), 
     &    ' is not a components database file.'
        CALL USRMSG(OUTS,' ','W') 
        ier=1
        CALL ERPFREE(Comp_DBase_Unit,ISTAT)
        return
      endif 

      IF(scantype(1:4).eq.'open') RETURN   

C Read in the header, and scan the available domains.
  20  CALL STRIPC(Comp_DBase_Unit,OUTSTR,99,ND,0,'database header',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','domain tag',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:7).eq.'*Domain'.or.WORD(1:7).eq.'*domain')then
        InDomain=.TRUE.
        CALL EGETW(OUTSTR,K,WORD,'W','the domain',IER)
        IF(IER.NE.0) CONT=.FALSE.
        tmp_domaintag=' '
        tmp_domaintag=word(1:16)
        if(nbdomain.gt.0)then 
          do 21 i=1,nbdomain
            if(tmp_domaintag(1:lnblnk(tmp_domaintag)).eq.
     &           domaintag(k)(1:lnblnk(domaintag(i)))) then
              WRITE(OUTS,'(3A)') 
     &'cdb warning: the domain ',tmp_domaintag(1:lnblnk(tmp_domaintag)),
     &        ' has been defined more than once. Correct this'
              WRITE(OUTS2,'(A)') 
     &'as it will cause errors when searching for data.'
              CALL USRMSG(OUTS,OUTS2,'W')  
              IER=1 
              CONT=.FALSE.
              RETURN
            endif 
 21       continue
        endif
        if(nbdomain+1.GT.Max_Domain_Types)THEN
          write(OUTS,'(A)')'cdb error: Max # of domains exceeded.'
          CALL USRMSG(OUTS,' ','W')            
          RETURN
        ENDIF
        nbdomain=nbdomain+1
        nbd=nbdomain
        domaintag(nbdomain)=
     &tmp_domaintag(1:lnblnk(tmp_domaintag))
        CALL EGETRM(OUTSTR,K,PHRASE,'W','domainmenu',IER)
        IF(IER.NE.0) CONT=.FALSE.
        tmp_domainmenu=PHRASE(1:32)
        write(domainmenu(nbd),'(a)')
     &tmp_domainmenu(1:lnblnk(tmp_domainmenu))

C Trace
c        write(outs,'(a)') ' '
c        call edisp(iuout,outs)
c        write(outs,'(a,a)') 'Database domain: ',domainmenu(nbd)
c        call edisp(iuout,outs)
        if(CONT)goto 20

C Scan the categories within this domain (if any)
      elseif(WORD(1:9).eq.'*Category'.or.WORD(1:9).eq.'*category')then

C Check category is defined in a specified domain.
        if(.not.InDomain)then
           WRITE(OUTS,'(3A)') 
     &'cdb warning: the category ',tmp_cattag(1:lnblnk(tmp_cattag)),
     &    ' has been not been assigned a domain. Correct this as'
          CALL USRMSG(OUTS,' ','W') 
          WRITE(OUTS,'(A)') 
     &'as it will cause errors when searching for data.'
          CALL USRMSG(OUTS,' ','W')   
          ier=1
          CONT=.FALSE.
          RETURN
        endif

        InCategory=.TRUE.
        CALL EGETW(OUTSTR,K,WORD,'W','category tag',IER)
        IF(IER.NE.0) CONT=.FALSE.
        tmp_cattag=' '
        tmp_cattag=word(1:16)

C Check for duplicate category names.
        if(nbcat(nbd).gt.0)then 
          do 23 i=1,nbcat(nbd)
            if(tmp_cattag(1:lnblnk(tmp_cattag)).eq.
     &           cattag(nbd,i)(1:lnblnk(cattag(nbd,i)))) then
              WRITE(OUTS,'(3A)') 
     &'cdb warning: the category ',tmp_cattag(1:lnblnk(tmp_cattag)),
     &        ' has been defined more than once. Please correct'
              WRITE(OUTS2,'(A)') 
     &'this as it will cause errors when searching for data.'
              CALL USRMSG(OUTS,OUTS2,'W')   
              IER=1
              CONT=.FALSE.
              RETURN
            end if
 23       continue
        endif

C No duplicates found so add the category to the list.
        if(nbcat(nbd)+1.GT.Max_Categories)THEN
          write(OUTS,'(A)')
     &'cdb error: Max # of categories exceeded.'
          CALL USRMSG(OUTS,' ','W')            
          RETURN
        ENDIF
        nbcat(nbd)=nbcat(nbd)+1
        nbc=nbcat(nbd)
        cattag(nbd,nbc)=tmp_cattag(1:lnblnk(tmp_cattag))
        CALL EGETRM(OUTSTR,K,PHRASE,'W','category menu',IER)
        IF(IER.NE.0) CONT=.FALSE.
        IF(CONT)THEN
          tmp_catmenu=PHRASE(1:32)
          write(catmenu(nbd,nbc),'(a)') 
     &tmp_catmenu(1:lnblnk(tmp_catmenu))

C Trace
c          write(outs,'(a)') ' '
c          call edisp(iuout,outs)
c          write(outs,'(a,a)') 'Component category: ',catmenu(nbd,nbc)
c          call edisp(iuout,outs)
          goto 20
        ENDIF


C Scan through the individual items in a category.
      elseif(WORD(1:5).eq.'*Item'.or.WORD(1:5).eq.'*item')then

C Check item is defined in a specified category.
        if(.not.InCategory)then
           WRITE(OUTS,'(3A)') 
     &'cdb warning: the item ',tmp_itemtag(1:lnblnk(tmp_itemtag)),
     &    ' has not been defined within a category. Please correct'
          CALL USRMSG(OUTS,' ','W') 
          WRITE(OUTS,'(A)') 
     &'this as it will cause errors when searching for data.'
          CALL USRMSG(OUTS,' ','W')   
          ier=1
          CONT=.FALSE.
          RETURN
        endif

        CALL EGETW(OUTSTR,K,WORD,'W','item tag',IER)
        IF(IER.NE.0) CONT=.FALSE.
          tmp_itemtag=' '
          tmp_itemtag=word(1:16)
        if(total_DB_items.gt.0)then 
          do 25 i=1,total_DB_items
            if(tmp_itemtag(1:lnblnk(tmp_itemtag)).eq.
     &           all_itemtag(i)(1:lnblnk(all_itemtag(i)))) then
              WRITE(OUTS,'(3A)') 
     &'cdb warning: the item ',tmp_itemtag(1:lnblnk(tmp_itemtag)),
     &        ' has been defined more than once. Please correct'
              WRITE(OUTS2,'(A)') 
     &'this as it will cause errors when searching for data.'
              CALL USRMSG(OUTS,OUTS2,'W')   
              ier=1
              CALL ERPFREE(Comp_DBase_Unit,ISTAT)
              RETURN
            end if
 25       continue
        endif

C No duplicates so add the new item into the list.    
        if(nbitem(nbd,nbc)+1.GT.Max_Items)THEN
          write(OUTS,'(A)')
     &'cdb error: max # of items exceeded.'
          CALL USRMSG(OUTS,' ','W')            
          RETURN
        ENDIF    
        total_DB_items=total_DB_items+1
        all_itemtag(total_DB_items)=tmp_itemtag(1:lnblnk(tmp_itemtag))
        nbitem(nbd,nbc)=nbitem(nbd,nbc)+1
        nbi=nbitem(nbd,nbc)
        itemtag(nbd,nbc,nbi)=tmp_itemtag(1:lnblnk(tmp_itemtag))
        CALL EGETRM(OUTSTR,K,PHRASE,'W','item menu',IER)
        IF(IER.NE.0) CONT=.FALSE.
        tmp_itemmenu=PHRASE(1:48) 
        write(itemmenu(nbd,nbc,nbi),'(a)') 
     &tmp_itemmenu(1:lnblnk(tmp_itemmenu))
C Trace
c        write(outs,'(a,a)') '    Component: ',itemmenu(nbd,nbc,nbi)
c        call edisp(iuout,outs)
        
C Read the component data if the scan type is detailed. 
        IF(scantype(1:8).eq.'detailed')THEN
C Set the pointer to the corresponding compact component data array. 
          itempointer(nbd,nbc,nbi)=total_DB_items
          itp=itempointer(nbd,nbc,nbi)
          natrib=0
          textl=0
          nbverts=0
          nbedges=0
          nbdots=0
          nbarcs=0
          nbatts=0
          nblabels=0
          
          
c          write(67,*)' '
c          write(67,*)'Component pointer *-> ',itp
                
C Read in the item data here. 
C Read of attributes achived using a strip loop..
 41       CALL STRIPC(Comp_DBase_Unit,OUTSTR,99,ND,0,'icons',IER)
          IF(IER.NE.0) CONT=.FALSE. 
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','icon tag',IER)
          IF(IER.NE.0) CONT=.FALSE.
    
          if(WORD(1:7).eq.'*Attrib'.or.WORD(1:7).eq.'*attrib')then
    
C Extract data for *thermal,*flow,*control,*output ... until *End_attribute
 42         CALL STRIPC(Comp_DBase_Unit,OUTSTR,99,ND,0,'attributes',IER)
            IF(IER.NE.0) CONT=.FALSE. 
            if(OUTSTR(1:14).eq.'*End_attribute')then
              continue
            else
    
C Extract data for *thermal,*flow,*control,*output etc. groups.
C The 1st word signals which 'group' the attribute is in, the
C 2nd word the data type (integer/real/text)
              if(natrib+1.gt.Max_Parameters) THEN
                write(outs,*)
     &'cdb error: maximum number of parameters @',natrib+1,' for #',
     &all_itemtag(itp),' exceeded.' 
                CALL USRMSG(OUTS,' ','W')
                write(OUTS,*)'reading -> ',OUTSTR
                CALL USRMSG(OUTS,' ','W')
                RETURN
                
              ENDIF
              K=0

C Strip the attribute tag.
              CALL EGETW(OUTSTR,K,WORD,'W','atrib group',IER)
              IF(IER.NE.0) THEN
                CONT=.FALSE.
                              
                write(outs,*)
     &'cdb error in attribute type ',WORD(1:lnblnk(WORD)),
     &'in component: ',all_itemtag(itp) 
                CALL USRMSG(OUTS,' ','W')
                RETURN
              ELSE
                natrib=natrib+1
                tagatr(itp,natrib,1)=WORD(2:LNBLNK(WORD))
              ENDIF
              IF(IER.NE.0) CONT=.FALSE.
              natribs(itp)=natrib

C Scan three words, the first is the data type, 2nd is either the
C string `external` or `-` and the 3rd is either a tag to find
C in an external file or `-`
              CALL EGETW(OUTSTR,K,WORD,'W','atrib data type',IER)
              CALL EGETW(OUTSTR,K,WORD1,'W','atrib external or -',IER)
              write(tagatr(itp,natrib,3),'(a)') WORD1(1:12)
              CALL EGETW(OUTSTR,K,WORD2,'W','atrib ext tag or -',IER)
              write(tagatr(itp,natrib,4),'(a)') WORD2(1:12)
    
C Based on the data type parse the remaining items on the line.
              if(WORD(1:4).eq.'intg'.or.WORD(1:4).eq.'INTG')then
                tagatr(itp,natrib,2)='intg'
                CALL EGETW(OUTSTR,K,WORD,'W','intg value',IER)
                write(atrib(itp,natrib,1),'(a)') WORD(1:12)
                CALL EGETW(OUTSTR,K,WORD,'W','intg min',IER)
                write(atrib(itp,natrib,2),'(a)') WORD(1:12)
                CALL EGETW(OUTSTR,K,WORD,'W','intg max',IER)
                write(atrib(itp,natrib,3),'(a)') WORD(1:12)
              elseif(WORD(1:4).eq.'real'.or.WORD(1:4).eq.'REAL')then
                tagatr(itp,natrib,2)='real'
                CALL EGETW(OUTSTR,K,WORD,'W','real value',IER)
                write(atrib(itp,natrib,1),'(a)') WORD(1:12)
                CALL EGETW(OUTSTR,K,WORD,'W','real min',IER)
                write(atrib(itp,natrib,2),'(a)') WORD(1:12)
                CALL EGETW(OUTSTR,K,WORD,'W','real max',IER)
                write(atrib(itp,natrib,3),'(a)') WORD(1:12)
              elseif(WORD(1:4).eq.'text'.or.WORD(1:4).eq.'TEXT')then
                tagatr(itp,natrib,2)='text'
                CALL EGETP(OUTSTR,K,PHRASE,'W','data string',IER)
                write(atrib(itp,natrib,1),'(a)') PHRASE(1:32)
                atrib(itp,natrib,2)=' '
                atrib(itp,natrib,3)=' '
              endif
              CALL EGETW(OUTSTR,K,WORD,'W','static:user',IER)
              write(tagatr(itp,natrib,5),'(a)') WORD(1:12)
              CALL EGETP(OUTSTR,K,PHRASE,'-','atrib menu',IER)
              write(menuatr(itp,natrib),'(a)') PHRASE(1:32)
    
C Get units (may not be available so suppress warnings & dont return IER).
              CALL EGETW(OUTSTR,K,WORD,'-','units',IERx)
      
              if(tagatr(itp,natrib,2)(1:4).eq.'text')then
                write(tagatr(itp,natrib,6),'(a)')' '
              elseif(word(1:2).ne.'  ')then
                write(tagatr(itp,natrib,6),'(a)') WORD(1:12)
              else
                write(tagatr(itp,natrib,6),'(a)')'(-)'
              endif
              IF(IER.NE.0) CONT=.FALSE.
              
C Trace ....
c            write(67,*)'Recovered data tags: '
c            write(67,*)
c     &'   DOMAIN   |    TYPE    |    EXT?    |   SOURCE   |',
c     &'   STATIC?  |    UNIT     '
c            write(67,*)(tagatr(itp,natrib,j),j=1,6)
c            write(67,*)'Value and max/min (if applicable): '       
c            write(67,*)(atrib(itp,natrib,j),j=1,3)

              if(CONT)goto 42
            endif

          elseif(WORD(1:5).eq.'*Text'.or.WORD(1:5).eq.'*text')then
            textl=0
    
C Strip text strings.
 45         CALL STRIPC(Comp_DBase_Unit,OUTSTR,99,ND,0,'icons',IER)
            IF(IER.NE.0) CONT=.FALSE. 
            if(OUTSTR(1:9).eq.'*End_Text')then
              continue
            else
              textl=textl+1
              n_text_desc(itp)=textl              
              if(textl.LT.Max_Text_Desc) 
     &item_text_desc(itp,textl)=OUTSTR(1:72)
              if(CONT) goto 45
            endif      
    
C Graphics data here. 
          elseif(WORD(1:6).eq.'*Graph'.or.WORD(1:6).eq.'*graph')then
    
C Read graphic data.
            CALL STRIPC(Comp_DBase_Unit,OUTSTR,99,ND,0,'icons',IER)
            IF(IER.NE.0) CONT=.FALSE. 
            K=0
            CALL EGETW(OUTSTR,K,WORD,'W','icon tag',IER)
            IF(IER.NE.0) CONT=.FALSE.
            if(WORD(1:9).eq.'*Graphics'.or.WORD(1:9).eq.'*graphics')then
 47           CALL STRIPC(Comp_DBase_Unit,OUTSTR,99,ND,0,'icons',IER)
              IF(IER.NE.0) CONT=.FALSE. 
              if(WORD(1:7).eq.'*Vertex'.or.WORD(1:7).eq.'*vertex')then
                CALL EGETWR(OUTSTR,K,VAL1,0.,0.,'-','icon vert X',IER)
                CALL EGETWR(OUTSTR,K,VAL2,0.,0.,'-','icon vert Y',IER)
                IF(IER.NE.0) CONT=.FALSE.
                nbverts=nbverts+1
                nbvert(itp)=nbverts
                vert(itp,nbverts,1)=VAL1
                vert(itp,nbverts,2)=VAL2
                if(CONT)goto 47
              elseif(WORD(1:5).eq.'*Edge'.or.WORD(1:5).eq.'*edge')then

C For each edge get start and end vertex and colour string.
                CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','start vertex',IER)
                CALL EGETWI(OUTSTR,K,IVAL2,0,0,'-','end vertex',IER)
                IF(IER.NE.0) CONT=.FALSE.
                nbedges=nbedges+1
                nbedge(itp)=nbedges
                iedge(itp,nbedges,1)=IVAL1
                iedge(itp,nbedges,2)=IVAL2
                if(ND.gt.3)then
                  CALL EGETW(OUTSTR,K,colourstr,'W','edge colour',IER)
                  call whichcolour(colourstr,indxcolour,ctype)
                  iedge(itp,nbedges,3)=indxcolour
                  iedge(itp,nbedges,4)=0
                  if(ctype.eq.'i')iedge(itp,nbedges,4)=0
                  if(ctype.eq.'g')iedge(itp,nbedges,4)=1
                  if(ctype.eq.'z')iedge(itp,nbedges,4)=2
                  CALL EGETW(OUTSTR,K,phrase,'W','edge line style',IER)
                  if(phrase(1:5).eq.'solid')then
                    iedge(itp,nbedges,5)=1
                  elseif(phrase(1:6).eq.'dotted')then
                    iedge(itp,nbedges,5)=2
                  elseif(phrase(1:6).eq.'dashed')then
                    iedge(itp,nbedges,5)=3
                  elseif(phrase(1:6).eq.'double')then
                    iedge(itp,nbedges,5)=4
                  else
                    iedge(itp,nbedges,5)=0
                  endif
                else
                  colourstr='black'
                  call whichcolour(colourstr,indxcolour,ctype)
                  iedge(itp,nbedges,3)=indxcolour
                  iedge(itp,nbedges,4)=1
                endif
                if(CONT)goto 47
              elseif(WORD(1:4).eq.'*Dot'.or.WORD(1:4).eq.'*dot')then
                CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','dot vertex',IER)
                IF(IER.NE.0) CONT=.FALSE.
                nbdots=nbdots+1
                nbdot(itp)=nbdots
                idot(itp,nbdots,1)=IVAL1
                CALL EGETW(OUTSTR,K,colourstr,'W','dot colour',IER)
                call whichcolour(colourstr,indxcolour,ctype)
                idot(itp,nbdots,2)=indxcolour
                idot(itp,nbdots,3)=0
                if(ctype.eq.'i')idot(itp,nbdots,3)=0
                if(ctype.eq.'g')idot(itp,nbdots,3)=1
                if(ctype.eq.'z')idot(itp,nbdots,3)=2
                CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','dot size index',IER)
                idot(itp,nbdots,4)=IVAL1
                if(CONT)goto 47
              elseif(WORD(1:4).eq.'*Arc'.or.WORD(1:4).eq.'*arc')then
                CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','arc cnt vertex',IER)
                IF(IER.NE.0) CONT=.FALSE.
                nbarcs=nbarcs+1
                nbarc(itp)=nbarcs
                iarc(itp,nbarcs,1)=IVAL1
                CALL EGETWI(OUTSTR,K,IVAL2,0,0,'-','arc rad vertex',IER)
                IF(IER.NE.0) CONT=.FALSE.
                iarc(itp,nbarcs,2)=IVAL2
                CALL EGETWI(OUTSTR,K,IVAL3,-360,360,'w','arc ang 1',IER)
                IF(IER.NE.0) CONT=.FALSE.
                iarc(itp,nbarcs,3)=IVAL3
                CALL EGETWI(OUTSTR,K,IVAL4,-360,360,'w','arc ang 2',IER)
                IF(IER.NE.0) CONT=.FALSE.
                iarc(itp,nbarcs,4)=IVAL4
                CALL EGETW(OUTSTR,K,colourstr,'W','arc colour',IER)
                call whichcolour(colourstr,indxcolour,ctype)
                iarc(itp,nbarcs,5)=indxcolour
                iarc(itp,nbarcs,6)=0
                if(ctype.eq.'i')iarc(itp,nbarcs,6)=0
                if(ctype.eq.'g')iarc(itp,nbarcs,6)=1
                if(ctype.eq.'z')iarc(itp,nbarcs,6)=2
                CALL EGETW(OUTSTR,K,phrase,'W','arc fill',IER)
                if(phrase(1:4).eq.'fill')then
                  iarc(itp,nbarcs,7)=1
                elseif(phrase(1:6).eq.'nofill')then
                  iarc(itp,nbarcs,7)=0
                endif
                if(CONT)goto 47
              elseif(WORD(1:6).eq.'*Label'.or.WORD(1:6).eq.'*label')then
                CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','label vertex',IER)
                IF(IER.NE.0) CONT=.FALSE.
                nblabels=nblabels+1
                ilabel(itp,nblabels,1)=IVAL1
                CALL EGETW(OUTSTR,K,colourstr,'W','label colour',IER)
                call whichcolour(colourstr,indxcolour,ctype)
                ilabel(itp,nblabels,2)=indxcolour
                ilabel(itp,nblabels,3)=0
                if(ctype.eq.'i')ilabel(itp,nblabels,3)=0
                if(ctype.eq.'g')ilabel(itp,nblabels,3)=1
                if(ctype.eq.'z')ilabel(itp,nblabels,3)=2
                CALL EGETW(OUTSTR,K,WORD,'W','label text',IER)
                labeltx(itp,nblabels)=WORD(1:4)
                if(CONT)goto 47
              elseif(WORD(1:7).eq.'*Attach'.or.WORD(1:7).eq.
     &'*attach')then
                nbatts=nbatts+1
                nbatt(itp)=nbatts
                CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','attachment vertex',
     &IER)
                IF(IER.NE.0) CONT=.FALSE.
                iatt(itp,nbatts,1)=IVAL1
                if(ND.gt.2)then
                  CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','attachment type',
     &IER)
                  IF(IER.NE.0) CONT=.FALSE.
                  iatt(itp,nbatts,2)=IVAL1
                else
                  iatt(itp,nbatts,2)=0
                endif
                if(CONT)goto 47
    
              else
                if(CONT)goto 41
              endif
    
            elseif(WORD(1:12).eq.'*End_graphic'.or.WORD(1:12).eq.
     &'*end_graphic')then                                  
              if(CONT)goto 41
    
            endif
    
          elseif(WORD(1:9).eq.'*End_item'.or.WORD(1:9).eq.
     &'*end_item')then
            if(CONT)goto 20

          else
            if(CONT)goto 41
          endif

        ENDIF !end of detailed scan
 
        if(CONT)goto 20

      elseif(WORD(1:11).eq.'*End_domain'.or.WORD(1:11).eq.
     &'*End_Domain')then
        InDomain=.FALSE.
        if(CONT)goto 20
      elseif(WORD(1:13).eq.'*End_category'.or.WORD(1:13).eq.
     &'*End_Category')then
        InCategory=.FALSE.
        if(CONT)goto 20
      elseif(WORD(1:15).eq.'*End_components'.or.WORD(1:15).eq.
     &'*End_Components')then

C End of file reached - return IER=0
        IF(CONT)THEN
          IER=0
          CALL ERPFREE(Comp_DBase_Unit,ISTAT)
          RETURN
        ELSE
          IER=1
          CALL ERPFREE(Comp_DBase_Unit,ISTAT)
          RETURN
        ENDIF
      else
        if(CONT)goto 20
      endif

C Abnormal termination.
      IER=1
      CALL ERPFREE(Comp_DBase_Unit,ISTAT)
      RETURN
      
      END

C ************************ SUBROUTINE ********************************** 
C ----------------------------------------------------------------------
      subroutine CDBGetItemData(domain_s,category_s,item_s,data_type)
C ----------------------------------------------------------------------
C ***********************************************************************
#include "gencompDB.h"
#include "gencompDB_common.h"

      integer lnblnk, textl  ! function definition

      COMMON/FILEP/IFIL

      CHARACTER OUTSTR*124,WORD*24,WORD1*24,WORD2*24,PHRASE*48
      CHARACTER data_type*12,outs*124,get_what*12
      character domain_s*16,category_s*16,item_s*16
      character colourstr*12,ctype*1
      character*32 tmp_item_cat,tmp_item_dom

      INTEGER nbverts,nbedges,nbdots,nbarcs,nbatts,nblabels

      LOGICAL CONT,param,texts,graphics,clear

      IER=0
      CONT=.TRUE.
      PHRASE=' '
      WORD=' '
      WORD1=' '
      WORD2=' '
      OUTS=' '
      OUTSTR=' '
 
C Set the logical recovery values
      param=.false.
      texts=.false.
      graphics=.false.
      get_what=data_type(1:lnblnk(data_type))
      if(get_what.eq.'all')then
        param=.true.
        texts=.true.
        graphics=.true.
      elseif(get_what.eq.'parameters')then
        param=.true.
      elseif(get_what.eq.'text')then
        texts=.true.
      elseif(get_what.eq.'graphics')then
        graphics=.true.
      elseif(get_what.eq.'clear')then
        clear=.true.
      else
        write(outs,'(A,A,A)')'cdb search - data type: ',
     &data_type(1:lnblnk(data_type)),' is not supported.'
        CALL USRMSG(OUTS,' ','W')
        RETURN
      endif
      
c      write (*,*) 'getting ... ',get_what,' for ',item_s
C Clear the returned parameter list.
      natrib=0
      s_item_menu=' ' 
      s_item_tag=' ' 
      do 3 i=1,Max_Parameters
        s_tagatr(i,1)=' '
        s_tagatr(i,2)=' '
        s_tagatr(i,3)=' '
        s_tagatr(i,4)=' '
        s_tagatr(i,5)=' '
        s_tagatr(i,6)=' '
        s_atrib(i,1)=' '
        s_atrib(i,2)=' '
        s_atrib(i,3)=' '
        s_menuatr(i)=' '
  3   continue

      do 2 i=1,Max_Text_Desc
        s_item_text_desc(i)=' '
  2   continue

C Graphics here.
C Clear the returned graphics data list.
      nbverts=0
      nbedges=0
      nbdots=0
      nbarcs=0
      nbatts=0
      nblabels=0
      do 5 k=1,Max_Vert
        s_vert(k,1)=0.0
        s_vert(k,2)=0.0
  5   continue
      do 6 k=1,Max_Edge
        s_iedge(k,1)=0
        s_iedge(k,2)=0
        s_iedge(k,3)=0
        s_iedge(k,4)=0
        s_iedge(k,5)=0
  6   continue
        do 7 k=1,Max_Dots
        s_idot(k,1)=0
        s_idot(k,2)=0
        s_idot(k,3)=0
        s_idot(k,4)=0
        s_iarc(k,1)=0
        s_iarc(k,2)=0
        s_iarc(k,3)=0
        s_iarc(k,4)=0
        s_iarc(k,5)=0
        s_iarc(k,6)=0
        s_iarc(k,7)=0
        s_ilabel(k,1)=0
        s_ilabel(k,2)=0
        s_ilabel(k,3)=0
        s_labeltx(k)=' '
  7   continue

      if(clear) RETURN

C Open the database file on IFIL+1 and find the required data. 
c      write(*,*)'opening ... ',Comp_DBase_Name
      if(Comp_DBase_Unit.eq.0) Comp_DBase_Unit=IFIL+1
      CALL EFOPSEQ(Comp_DBase_Unit,Comp_DBase_Name,1,IER)
      IF(IER.NE.0) THEN
        IER=1
        CALL ERPFREE(Comp_DBase_Unit,ISTAT)
        RETURN
      ENDIF

C Skip domain check if no domain is supplied.
      IF(domain_s(1:1).ne.' '.and.domain_s(1:1).ne.'-')THEN
C Read in the header, look for the domain and see if it matches.
 20     CALL STRIPC(Comp_DBase_Unit,OUTSTR,99,ND,0,'icon header',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','domain tag',IER)
        IF(IER.NE.0) CONT=.FALSE.
        if(WORD(1:7).eq.'*Domain'.or.WORD(1:7).eq.'*domain')then
          CALL EGETW(OUTSTR,K,WORD,'W','the domain',IER)
          IF(IER.NE.0) CONT=.FALSE.
          if(WORD(1:lnblnk(WORD)).eq.domain_s(1:lnblnk(domain_s)))then
            continue
          else
            if(CONT)goto 20
          endif 
          if(.NOT.CONT)then
            IER=0
            write(OUTS,'(A)')
     &'CDBGetItemData: An error occured during search of the database'
            CALL USRMSG(OUTS,' ','W')  
            RETURN
          endif
        ELSEIF(WORD(1:15).EQ.'*End_components')THEN
          write(outs,'(A,A16,A1,A16,A1,A16)')
     &'CDBGetItemData: Scanned database and no match was found for: '
     &,domain_s,'*',category_s,'>',item_s
          CALL USRMSG(OUTS,' ','W')
          RETURN
        else
          goto 20
        endif
      ENDIF

C Skip category check if no domain is supplied.
      IF(category_s(1:1).ne.' '.and.category_s(1:1).ne.'-')THEN
     
C Read in the categories, see which matches.
  21    CALL STRIPC(Comp_DBase_Unit,OUTSTR,99,ND,0,'categories',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','category tag',IER)
        IF(IER.NE.0) CONT=.FALSE.
        if(WORD(1:9).eq.'*Category'.or.WORD(1:9).eq.'*category')then
          CALL EGETW(OUTSTR,K,WORD,'W','category tag',IER)
          IF(IER.NE.0) CONT=.FALSE.
  
C If correct category process icons otherwise loop again
          if(WORD(1:lnblnk(WORD)).eq.
     &       category_s(1:lnblnk(category_s)))then
            continue
          else
            goto 21
          endif
          if(.NOT.CONT)then
            IER=0
            CALL ERPFREE(Comp_DBase_Unit,ISTAT)
            RETURN
          endif
        else
          if(WORD(1:15).EQ.'*End_components')THEN
            write(outs,'(A,A16,A1,A16,A1,A16)')
     &'CDBGetItemData: Scanned database and no match was found for: '
     &,domain_s,'>',category_s,'*',item_s
            CALL USRMSG(OUTS,' ','W')
            RETURN
          else
            goto 21
          endif
        endif
      ENDIF

C If no item name is supplied return with error message.
      IF(item_s(1:1).eq.' '.or.item_s(1:1).eq.'-')THEN
        write(outs,'(A)')'CDBGetItemData: search item not supplied'
        CALL USRMSG(OUTS,' ','W')
        RETURN
      ENDIF

C Read in the item, see if it matches.
c      write (*,*) 'scanning for ... ',item_s
  22  CALL STRIPC(Comp_DBase_Unit,OUTSTR,99,ND,0,'icons',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','icon tag',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:7).eq.'*Domain'.or.WORD(1:7).eq.'*domain')THEN
        CALL EGETW(OUTSTR,K,WORD,'W','domain tag',IER)
        CALL EGETP(OUTSTR,K,PHRASE,'-','domain desc',IER)
        tmp_item_dom=PHRASE(1:32)
        goto 22
      ENDIF
      if(WORD(1:9).eq.'*Category'.or.WORD(1:9).eq.'*category')THEN
        CALL EGETW(OUTSTR,K,WORD,'W','category tag',IER)
        CALL EGETP(OUTSTR,K,PHRASE,'-','category desc',IER)
        tmp_item_cat=PHRASE(1:32)
        goto 22
      ENDIF
      if(WORD(1:5).eq.'*Item'.or.WORD(1:5).eq.'*item')then
        CALL EGETW(OUTSTR,K,WORD,'W','item tag',IER)
        IF(IER.NE.0) CONT=.FALSE.
c        write(*,*) 'comparing ...',word,' with ',item_s
        if(WORD(1:lnblnk(WORD)).eq.item_s(1:lnblnk(item_s)))then
c          write(*,*)'found ... ',item_s
C Read in basic information
          s_item_tag=WORD(1:16)
c          write(*,*) 'tag  ',s_item_tag
          CALL EGETP(OUTSTR,K,PHRASE,'-','atrib menu',IER)
          s_item_menu=PHRASE(1:48)
c          write(88,*) 'description ...  ',s_item_menu
          s_item_dom=tmp_item_dom(1:32)
          s_item_cat=tmp_item_cat(1:32)
          goto 23
        else
          goto 22          
        endif
        if(.NOT.CONT)then
          IER=1
          CALL ERPFREE(Comp_DBase_Unit,ISTAT)
          RETURN
        endif
      else
        IF(WORD(1:15).EQ.'*End_components')THEN
            write(outs,'(A,A16,A1,A16,A1,A16,A)')
     &'CDBGetItemData: Scanned database and no match was found for: '
     &,domain_s,'>',category_s,'>',item_s,'*'
            CALL USRMSG(OUTS,' ','W')
            RETURN
        ELSE
          goto 22
        ENDIF
      endif

C Read of attributes achived using a strip loop..
  23  CALL STRIPC(Comp_DBase_Unit,OUTSTR,99,ND,0,'icons',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','icon tag',IER)
      IF(IER.NE.0) CONT=.FALSE.

      if(WORD(1:10).eq.'*Attribute'.or.WORD(1:10).eq.'*attribute'
     &.and.param)then

C Extract data for *thermal,*flow,*control,*output ... until *End_attribute
  42    CALL STRIPC(Comp_DBase_Unit,OUTSTR,99,ND,0,'attributes',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        if(OUTSTR(1:14).eq.'*End_attribute')then
          continue
          CONT=.FALSE.
        else

C Extract data for *thermal,*flow,*control,*output etc. groups.
C The 1st word signals which 'group' the attribute is in, the
C 2nd word the data type (integer/real/text)
          if(natrib+1.gt.Max_Parameters) THEN
            write(outs,*)
     &'CDBGetItemData: : maximum number of parameters for ',item_s,
     &' exceeded.' 
            RETURN
            
          ENDIF
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','atrib group',IER)
          IF(IER.NE.0) THEN
            CONT=.FALSE.
            write(outs,*)
     &'cdb error in attribute type ',WORD(1:lnblnk(WORD)),
     &'in component: ',item_s 
            CALL USRMSG(OUTS,' ','W')
            RETURN
          ELSE
            natrib=natrib+1
            s_tagatr(natrib,1)=WORD(2:LNBLNK(WORD))
          endif
          IF(IER.NE.0) CONT=.FALSE.
          s_natribs=natrib

C Scan three words, the first is the data type, 2nd is either the
C string `external` or `-` and the 3rd is either a tag to find
C in an external file or `-`
          CALL EGETW(OUTSTR,K,WORD,'W','atrib data type',IER)
          CALL EGETW(OUTSTR,K,WORD1,'W','atrib external or -',IER)
          write(s_tagatr(natrib,3),'(a)') WORD1(1:12)
          CALL EGETW(OUTSTR,K,WORD2,'W','atrib ext tag or -',IER)
          write(s_tagatr(natrib,4),'(a)') WORD2(1:12)

C Based on the data type parse the remaining items on the line.
          if(WORD(1:4).eq.'intg'.or.WORD(1:4).eq.'INTG')then
            s_tagatr(natrib,2)='intg'
            CALL EGETW(OUTSTR,K,WORD,'W','intg value',IER)
            write(s_atrib(natrib,1),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','intg min',IER)
            write(s_atrib(natrib,2),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','intg max',IER)
            write(s_atrib(natrib,3),'(a)') WORD(1:12)
          elseif(WORD(1:4).eq.'real'.or.WORD(1:4).eq.'REAL')then
            s_tagatr(natrib,2)='real'
            CALL EGETW(OUTSTR,K,WORD,'W','real value',IER)
            write(s_atrib(natrib,1),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','real min',IER)
            write(s_atrib(natrib,2),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','real max',IER)
            write(s_atrib(natrib,3),'(a)') WORD(1:12)
          elseif(WORD(1:4).eq.'text'.or.WORD(1:4).eq.'TEXT')then
            s_tagatr(natrib,2)='text'
            CALL EGETP(OUTSTR,K,PHRASE,'W','data string',IER)
            write(s_atrib(natrib,1),'(a)') PHRASE(1:32)
            s_atrib(natrib,2)=' '
            s_atrib(natrib,3)=' '
          endif
          CALL EGETW(OUTSTR,K,WORD,'W','static:user',IER)
          write(s_tagatr(natrib,5),'(a)') WORD(1:12)
          CALL EGETP(OUTSTR,K,PHRASE,'-','atrib menu',IER)
          write(s_menuatr(natrib),'(a)') PHRASE(1:32)

C Get units (may not be available so suppress error).
          CALL EGETW(OUTSTR,K,WORD,'-','units',IERu)
   
          if(s_tagatr(natrib,2)(1:4).eq.'text')then
            write(s_tagatr(natrib,6),'(a)')' '
          elseif(word(1:2).ne.'  ')then
            write(s_tagatr(natrib,6),'(a)') WORD(1:12)
          else
            write(s_tagatr(natrib,6),'(a)')'(-)'
          endif
          IF(IER.NE.0) CONT=.FALSE.
          if(CONT)goto 42
        endif

      elseif(WORD(1:5).eq.'*Text'.or.WORD(1:5).eq.'*text'.and.texts)then
        textl=0

C Strip text strings.
  25    CALL STRIPC(Comp_DBase_Unit,OUTSTR,99,ND,0,'icons',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        if(OUTSTR(1:9).eq.'*End_Text')then
          continue
        else
          textl=textl+1
          if(textl.LT.Max_Text_Desc) 
     &s_item_text_desc(textl)=OUTSTR(1:72)
          if(CONT) goto 25
        endif      

C Graphics data here. 
      elseif(WORD(1:5).eq.'*Graphics'.or.WORD(1:5).eq.'*graphics'
     &.and.graphics)then

C Read graphic data.
        CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'icons',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','icon tag',IER)
        IF(IER.NE.0) CONT=.FALSE.
        if(WORD(1:9).eq.'*Graphics'.or.WORD(1:9).eq.'*graphics')then
  27      CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'icons',IER)
          IF(IER.NE.0) CONT=.FALSE. 
          if(WORD(1:7).eq.'*Vertex'.or.WORD(1:7).eq.'*vertex')then
            CALL EGETWR(OUTSTR,K,VAL1,0.,0.,'-','icon vert X',IER)
            CALL EGETWR(OUTSTR,K,VAL2,0.,0.,'-','icon vert Y',IER)
            IF(IER.NE.0) CONT=.FALSE.
            nbverts=nbverts+1
            s_vert(nbverts,1)=VAL1
            s_vert(nbverts,2)=VAL2
            if(CONT)goto 27
          elseif(WORD(1:5).eq.'*Edge'.or.WORD(1:5).eq.'*edge')then

C For each edge get start and end vertex and colour string.
            CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','start vertex',IER)
            CALL EGETWI(OUTSTR,K,IVAL2,0,0,'-','end vertex',IER)
            IF(IER.NE.0) CONT=.FALSE.
            nbedges=nbedges+1
            s_iedge(nbedges,1)=IVAL1
            s_iedge(nbedges,2)=IVAL2
            if(ND.gt.3)then
              CALL EGETW(OUTSTR,K,colourstr,'W','edge colour',IER)
              call whichcolour(colourstr,indxcolour,ctype)
              s_iedge(nbedges,3)=indxcolour
              s_iedge(nbedges,4)=0
              if(ctype.eq.'i')s_iedge(nbedges,4)=0
              if(ctype.eq.'g')s_iedge(nbedges,4)=1
              if(ctype.eq.'z')s_iedge(nbedges,4)=2
              CALL EGETW(OUTSTR,K,phrase,'W','edge line style',IER)
              if(phrase(1:5).eq.'solid')then
                s_iedge(nbedges,5)=1
              elseif(phrase(1:6).eq.'dotted')then
                s_iedge(nbedges,5)=2
              elseif(phrase(1:6).eq.'dashed')then
                s_iedge(nbedges,5)=3
              elseif(phrase(1:6).eq.'double')then
                s_iedge(nbedges,5)=4
              else
                s_iedge(nbedges,5)=0
              endif
            else
              colourstr='black'
              call whichcolour(colourstr,indxcolour,ctype)
              s_iedge(nbedges,3)=indxcolour
              s_iedge(nbedges,4)=1
            endif
            if(CONT)goto 27
          elseif(WORD(1:4).eq.'*Dot'.or.WORD(1:4).eq.'*dot')then
            CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','dot vertex',IER)
            IF(IER.NE.0) CONT=.FALSE.
            nbdots=nbdots+1
            s_idot(nbdots,1)=IVAL1
            CALL EGETW(OUTSTR,K,colourstr,'W','dot colour',IER)
            call whichcolour(colourstr,indxcolour,ctype)
            s_idot(nbdots,2)=indxcolour
            s_idot(nbdots,3)=0
            if(ctype.eq.'i')s_idot(nbdots,3)=0
            if(ctype.eq.'g')s_idot(nbdots,3)=1
            if(ctype.eq.'z')s_idot(nbdots,3)=2
            CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','dot size index',IER)
            s_idot(nbdots,4)=IVAL1
            if(CONT)goto 27
          elseif(WORD(1:4).eq.'*Arc'.or.WORD(1:4).eq.'*arc')then
            CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','arc cnt vertex',IER)
            IF(IER.NE.0) CONT=.FALSE.
            nbarcs=nbarcs+1
            s_iarc(nbarcs,1)=IVAL1
            CALL EGETWI(OUTSTR,K,IVAL2,0,0,'-','arc rad vertex',IER)
            IF(IER.NE.0) CONT=.FALSE.
            s_iarc(nbarcs,2)=IVAL2
            CALL EGETWI(OUTSTR,K,IVAL3,-360,360,'w','arc ang 1',IER)
            IF(IER.NE.0) CONT=.FALSE.
            s_iarc(nbarcs,3)=IVAL3
            CALL EGETWI(OUTSTR,K,IVAL4,-360,360,'w','arc ang 2',IER)
            IF(IER.NE.0) CONT=.FALSE.
            s_iarc(nbarcs,4)=IVAL4
            CALL EGETW(OUTSTR,K,colourstr,'W','arc colour',IER)
            call whichcolour(colourstr,indxcolour,ctype)
            s_iarc(nbarcs,5)=indxcolour
            s_iarc(nbarcs,6)=0
            if(ctype.eq.'i')s_iarc(nbarcs,6)=0
            if(ctype.eq.'g')s_iarc(nbarcs,6)=1
            if(ctype.eq.'z')s_iarc(nbarcs,6)=2
            CALL EGETW(OUTSTR,K,phrase,'W','arc fill',IER)
            if(phrase(1:4).eq.'fill')then
              s_iarc(nbarcs,7)=1
            elseif(phrase(1:6).eq.'nofill')then
              s_iarc(nbarcs,7)=0
            endif
            if(CONT)goto 27
          elseif(WORD(1:6).eq.'*Label'.or.WORD(1:6).eq.'*label')then
            CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','label vertex',IER)
            IF(IER.NE.0) CONT=.FALSE.
            nblabels=nblabels+1
            s_ilabel(nblabels,1)=IVAL1
            CALL EGETW(OUTSTR,K,colourstr,'W','label colour',IER)
            call whichcolour(colourstr,indxcolour,ctype)
            s_ilabel(nblabels,2)=indxcolour
            s_ilabel(nblabels,3)=0
            if(ctype.eq.'i')s_ilabel(nblabels,3)=0
            if(ctype.eq.'g')s_ilabel(nblabels,3)=1
            if(ctype.eq.'z')s_ilabel(nblabels,3)=2
            CALL EGETW(OUTSTR,K,WORD,'W','label text',IER)
            s_labeltx(nblabels)=WORD(1:4)
            if(CONT)goto 27
          elseif(WORD(1:7).eq.'*Attach'.or.WORD(1:7).eq.'*attach')then
            nbatts=nbatts+1
            CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','attachment vertex',IER)
            IF(IER.NE.0) CONT=.FALSE.
            s_iatt(nbatts,1)=IVAL1
            if(ND.gt.2)then
              CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','attachment type',IER)
              IF(IER.NE.0) CONT=.FALSE.
              s_iatt(nbatts,2)=IVAL1
            else
              s_iatt(nbatts,2)=0
            endif
            if(CONT)goto 27

          else
            if(CONT)goto 23
          endif

        elseif(WORD(1:12).eq.'*End_graphic'.or.WORD(1:7).eq.
     &'*end_graphic')then
          if(CONT)goto 23

        endif

      elseif(WORD(1:5).eq.'*End_item'.or.WORD(1:5).eq.'*end_item')then
        CALL ERPFREE(Comp_DBase_Unit,ISTAT)
        RETURN
      else
        if(CONT)goto 23
      endif

      if(CONT)goto 23
      CALL ERPFREE(Comp_DBase_Unit,ISTAT)
      RETURN

      END

C ***** whichcolour()
C whichcolour is given a colour string within the list offered
C by zscalestr[] in wwcut.c and returns index between 0 and 28 and
C type = 'z'.  Or if the string is within the list offered by
C gscalestr it returns an index between 0 and 28 and type = 'g'.
C Or if black return 6 and white return 7 with type = 'i'.
      subroutine whichcolour(colourstr,indxcolour,type)
      integer lnblnk  ! function definition
      character colourstr*12,type*1

      indxcolour = -1
      lc=lnblnk(colourstr)
      if(lc.le.1.or.colourstr(1:2).eq.'  ')then
        type=' '
        return
      endif
      if(colourstr(1:lc).eq.'black')then
        indxcolour = 6
        type='i'
      elseif(colourstr(1:lc).eq.'white')then
        indxcolour = 7
        type='i'
      elseif(colourstr(1:lc).eq.'red')then
        indxcolour = 0
        type='z'
      elseif(colourstr(1:lc).eq.'MidnightBlue')then
        indxcolour = 1
        type='z'
      elseif(colourstr(1:lc).eq.'peru')then
        indxcolour = 2
        type='z'
      elseif(colourstr(1:lc).eq.'ForestGreen')then
        indxcolour = 3
        type='z'
      elseif(colourstr(1:6).eq.'khaki ')then
        indxcolour = 4
        type='z'
      elseif(colourstr(1:lc).eq.'turquoise')then
        indxcolour = 5
        type='z'
      elseif(colourstr(1:lc).eq.'magenta')then
        indxcolour = 6
        type='z'
      elseif(colourstr(1:lc).eq.'firebrick')then
        indxcolour = 7
        type='z'
      elseif(colourstr(1:lc).eq.'DarkCyan')then
        indxcolour = 8
        type='z'
      elseif(colourstr(1:lc).eq.'khaki3')then
        indxcolour = 9
        type='z'
      elseif(colourstr(1:lc).eq.'RoyalBlue')then
        indxcolour = 10
        type='z'
      elseif(colourstr(1:lc).eq.'tomato')then
        indxcolour = 11
        type='z'
      elseif(colourstr(1:lc).eq.'OliveDrab')then
        indxcolour = 12
        type='z'
      elseif(colourstr(1:lc).eq.'PaleGreen')then
        indxcolour = 13
        type='z'
      elseif(colourstr(1:lc).eq.'orange')then
        indxcolour = 14
        type='z'
      elseif(colourstr(1:lc).eq.'grey40')then
        indxcolour = 15
        type='z'
      elseif(colourstr(1:lc).eq.'coral2')then
        indxcolour = 16
        type='z'
      elseif(colourstr(1:lc).eq.'grey60')then
        indxcolour = 17
        type='z'
      elseif(colourstr(1:lc).eq.'maroon4')then
        indxcolour = 18
        type='z'
      elseif(colourstr(1:lc).eq.'gold3')then
        indxcolour = 19
        type='z'
      elseif(colourstr(1:lc).eq.'PowderBlue')then
        indxcolour = 20
        type='z'
      elseif(colourstr(1:lc).eq.'sienna')then
        indxcolour = 21
        type='z'
      elseif(colourstr(1:lc).eq.'azure4')then
        indxcolour = 22
        type='z'
      elseif(colourstr(1:lc).eq.'grey20')then
        indxcolour = 23
        type='z'
      elseif(colourstr(1:lc).eq.'grey50')then
        indxcolour = 24
        type='z'
      elseif(colourstr(1:lc).eq.'NavyBlue')then
        indxcolour = 24
        type='z'
      elseif(colourstr(1:lc).eq.'DarkGreen')then
        indxcolour = 25
        type='z'
      elseif(colourstr(1:lc).eq.'gold')then
        indxcolour = 26
        type='z'
      elseif(colourstr(1:lc).eq.'grey80')then
        indxcolour = 27
        type='z'
      elseif(colourstr(1:lc).eq.'grey97')then
        indxcolour = 0
        type='g'
      elseif(colourstr(1:lc).eq.'grey94')then
        indxcolour = 1
        type='g'
      elseif(colourstr(1:lc).eq.'grey91')then
        indxcolour = 2
        type='g'
      elseif(colourstr(1:lc).eq.'grey88')then
        indxcolour = 3
        type='g'
      elseif(colourstr(1:lc).eq.'grey85')then
        indxcolour = 4
        type='g'
      elseif(colourstr(1:lc).eq.'grey82')then
        indxcolour = 5
        type='g'
      elseif(colourstr(1:lc).eq.'grey79')then
        indxcolour = 6
        type='g'
      elseif(colourstr(1:lc).eq.'grey76')then
        indxcolour = 7
        type='g'
      elseif(colourstr(1:lc).eq.'grey73')then
        indxcolour = 8
        type='g'
      elseif(colourstr(1:lc).eq.'grey70')then
        indxcolour = 9
        type='g'
      elseif(colourstr(1:lc).eq.'grey67')then
        indxcolour = 0
        type='g'
      elseif(colourstr(1:lc).eq.'grey64')then
        indxcolour = 11
        type='g'
      elseif(colourstr(1:lc).eq.'grey61')then
        indxcolour = 12
        type='g'
      elseif(colourstr(1:lc).eq.'grey58')then
        indxcolour = 13
        type='g'
      elseif(colourstr(1:lc).eq.'grey55')then
        indxcolour = 14
        type='g'
      elseif(colourstr(1:lc).eq.'grey52')then
        indxcolour = 15
        type='g'
      elseif(colourstr(1:lc).eq.'grey49')then
        indxcolour = 16
        type='g'
      elseif(colourstr(1:lc).eq.'grey46')then
        indxcolour = 17
        type='g'
      elseif(colourstr(1:lc).eq.'grey43')then
        indxcolour = 18
        type='g'
      elseif(colourstr(1:lc).eq.'grey40')then
        indxcolour = 19
        type='g'
      elseif(colourstr(1:lc).eq.'grey37')then
        indxcolour = 20
        type='g'
      elseif(colourstr(1:lc).eq.'grey34')then
        indxcolour = 21
        type='g'
      elseif(colourstr(1:lc).eq.'grey31')then
        indxcolour = 22
        type='g'
      elseif(colourstr(1:lc).eq.'grey28')then
        indxcolour = 23
        type='g'
      elseif(colourstr(1:lc).eq.'grey25')then
        indxcolour = 24
        type='g'
      elseif(colourstr(1:lc).eq.'grey22')then
        indxcolour = 25
        type='g'
      elseif(colourstr(1:lc).eq.'grey19')then
        indxcolour = 26
        type='g'
      elseif(colourstr(1:lc).eq.'grey16')then
        indxcolour = 27
        type='g'
      elseif(colourstr(1:lc).eq.'grey14')then
        indxcolour = 28
        type='g'
      else
        indxcolour = -1
      endif
      return
      end
