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

C ******** scanicondb
C This routine scans an icon database and returns:
C   nbdomain: the number of domains and verdomain the version
C   idomain(MNWKTYP): a list of domain types indices
C   nbcat(MNWKTYP): number of categories in each domain,
C   cattag(MNWKTYP,MICNCAT): tag for each category (12 char),
C   catmenu(MNWKTY,MICNCAT): menu entry for each category (32 char)
C   nbicons(MNWKTYP,MICNCAT): number of items in each category. 
C   icontag(MNWKTYP,MICNCAT): tag for each icon (12 char),
C   iconmenu(MNWKTY,MICNCAT): menu entry for each icon (32 char)

C Domain connection attributes.
C   iatrdom(MNWKTYP): number of domain (connection) attributes
C   dtagatr(MNWKTYP,MIATRB,5): array of tags (12 char) where the first is the
C     group (thermal/flow/control/output/location/environment/electrical)
C     the 2nd signals data type, the 3rd is `external` or `-`, the 4th
C     is an external key word or `-`, and the 5th is key word 'static' or 'user'.
C     Note `external` signals that an external domain summary file should 
C     be consulted for information associated with the external key word.
C   datrib(MNWKTYP,MIATRB,3): array of strings holding attribute data
C     by type: for integers and real ,datrib(?,?,1) is a value
C     datrib(?,?,2) is minimum, atrib(?,?,3) is maximum, for text data 
C     datrib(?,?,1) is the string and datrib(?,?,2) & datrib(?,?,3) blank.
C   dmenuatr(MNWKTYP,MIATRB): array of menu entries (32 char)

C Domain global attributes.
C   igatrdom(MNWKTYP): number of domain (global) attributes
C   dgtagatr(MNWKTYP,MIATRB,5): array of tags (12 char) where the first is the
C     group (thermal/flow/control/output/location/environment/electrical)
C     the 2nd signals data type, the 3rd is `external` or `-`, the 4th
C     is an external key word or `-`, and the 5th is key word 'static' or 'user'.
C     Note `external` signals that an external domain summary file should 
C     be consulted for information associated with the external key word.
C   dgatrib(MNWKTYP,MIATRB,3): array of strings holding attribute data
C     by type: for integers and real ,datrib(?,?,1) is a value
C     datrib(?,?,2) is minimum, atrib(?,?,3) is maximum, for text data 
C     datrib(?,?,1) is the string and datrib(?,?,2) & datrib(?,?,3) blank.
C   dgmenuatr(MNWKTYP,MIATRB): array of menu entries (32 char)
C
      subroutine scanicondb(nbdomain,verdomain,idomain,nbcat,cattag,
     &  catmenu,nbicons,icontag,iconmenu,iatrdom,dtagatr,
     &  datrib,dmenuatr,igatrdom,dgtagatr,dgatrib,dgmenuatr,IER)
#include "gnetwk.h" 
      
      integer lnblnk  ! function definition

      COMMON/ICONDBNAM/ICONDBFL
      COMMON/FILEP/IFIL
      COMMON/POPHELP/H(60)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DMCMPDBFL,DICONDBFL,dmdbnam

      dimension idomain(MNWKTYP),nbcat(MNWKTYP),
     &  cattag(MNWKTYP,MICNCAT),catmenu(MNWKTYP,MICNCAT),
     &  nbicons(MNWKTYP,MICNCAT),icontag(MNWKTYP,MICNCAT,MICN),
     &  iconmenu(MNWKTYP,MICNCAT,MICN)
      dimension iatrdom(MNWKTYP),dtagatr(MNWKTYP,MIATRB,5),
     &  datrib(MNWKTYP,MIATRB,3),dmenuatr(MNWKTYP,MIATRB)
      dimension igatrdom(MNWKTYP),dgtagatr(MNWKTYP,MIATRB,5),
     &  dgatrib(MNWKTYP,MIATRB,3),dgmenuatr(MNWKTYP,MIATRB)

      CHARACTER OUTSTR*124,outs*124,WORD*24,WORD1*24,WORD2*24,PHRASE*48
      CHARACTER ICONDBFL*72,ltmp*72,h*72,DFILE*72
      character cattag*12,catmenu*32
      character icontag*12,iconmenu*40
      character dtagatr*12,dmenuatr*32,datrib*12
      character dgtagatr*12,dgmenuatr*32,dgatrib*12
      character*72 DFCFD,DECMPDBFL,DMCMPDBFL,DICONDBFL,dmdbnam

      LOGICAL OK,DOK,CONT

      IER=0
      ltmp='  '
      word=' '
      word1=' '
      word2=' '
      OK=.FALSE.
      CONT=.TRUE.
      
C Open the icon components database file 
 2    IF(ICONDBFL(1:3).EQ.'UNK'.or.ICONDBFL(1:2).EQ.'  ') THEN

C Hard coded default data base at the moment, later add to default data
C base list.
        ICONDBFL=DICONDBFL
        DFILE=DICONDBFL
        h(1)='<< help to be added >>  '
        ltmp=ICONDBFL
        CALL EASKS(ltmp,'icon database filename ?',
     &     ' ',72,DFILE,'ecomp dbnam',IER,1)
        if(LTMP(1:2).ne.'  '.and.LTMP(1:4).ne.'UNKN') ICONDBFL=ltmp
      ENDIF

      ICONFIL=IFIL+1
      CALL EFOPSEQ(ICONFIL,ICONDBFL,1,IER)
      IF(IER.NE.0) THEN
        dok=.false.
        h(1)='Icons for this application are held in a file which is'
        h(2)='typically /usr/esru/esp-r/databases/flow_icons.db1 for'
        h(3)='flow networks and /usr/esru/esp-r/databases/icons.db1'
        h(4)='for other networks. '
        h(5)=' '
        h(6)='The the folder /usr/esru/esp-r/databases '
        h(7)=' '
        h(8)='A no will set this file name to UNKNOWN. '
        CALL ASKOK('could not find the icon database.','Try again?',
     &    OK,dok,8)
        IF(.NOT.OK) THEN 
          IER=1
          CALL ERPFREE(ICONFIL,ISTAT)
          RETURN
        ELSE
          ICONDBFL='UNKNOWN'
          GOTO 2
        ENDIF
      ENDIF

C Clear the parameter list. nbd is local counter of current domain.
      nbdomain=0
      verdomain=0.0
      do 3 i=1,MNWKTYP
        idomain(i)=0
        nbcat(i)=0
        iatrdom(i)=0
        igatrdom(i)=0
        do 6 m=1,MIATRB
          dtagatr(i,m,1)=' '
          dtagatr(i,m,2)=' '
          dtagatr(i,m,3)=' '
          dtagatr(i,m,4)=' '
          dtagatr(i,m,5)=' '
          datrib(i,m,1)=' '
          datrib(i,m,2)=' '
          datrib(i,m,3)=' '
          dmenuatr(i,m)=' '
          dgtagatr(i,m,1)=' '
          dgtagatr(i,m,2)=' '
          dgtagatr(i,m,3)=' '
          dgtagatr(i,m,4)=' '
          dgtagatr(i,m,5)=' '
          dgatrib(i,m,1)=' '
          dgatrib(i,m,2)=' '
          dgatrib(i,m,3)=' '
          dgmenuatr(i,m)=' '
  6     continue
        do 4 j=1,MICNCAT
          cattag(i,j)='  '
          catmenu(i,j)='  '
          nbicons(i,j)=0
          do 5 k=1,MICN
            icontag(i,j,k)='  '
            iconmenu(i,j,k)='  '
  5       continue
  4     continue
  3   continue
      nbd=0
      nbc=0

C Clear any global attributes (e.g. wind reduction factor in a
C flow icons file)

C Check that the opened file is an icons database and which version.
      CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'check file',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      IF(OUTSTR(1:6).EQ.'*Icons')THEN
        if(ND.gt.1)then
          K=6
          CALL EGETWR(OUTSTR,K,verdomain,0.,2.,'-','version',IER)
        endif
      else
        WRITE(OUTS,'(3A)') 'File: ',ICONDBFL(1:LNBLNK(ICONDBFL)), 
     &    ' is not an icons file.'
        CALL USRMSG(OUTS,' ','W') 
        ier=1
        CALL ERPFREE(ICONFIL,ISTAT)
        return
      endif 
   
C Read in the header, look for the domain and see if if it is known.
  20  CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'icon header',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','icon domain tag',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:7).eq.'*Domain'.or.WORD(1:7).eq.'*domain')then
        CALL EGETW(OUTSTR,K,WORD,'W','the domain',IER)
        IF(IER.NE.0) CONT=.FALSE.
        if(WORD(1:5).eq.'PLNDB')then
          nbdomain=nbdomain+1
          nbd=nbdomain
          idomain(nbd)=1
        elseif(WORD(1:5).eq.'FLODB')then
          nbdomain=nbdomain+1
          nbd=nbdomain
          idomain(nbd)=2
        elseif(WORD(1:5).eq.'CTLDB')then
          nbdomain=nbdomain+1
          nbd=nbdomain
          idomain(nbd)=4
        elseif(WORD(1:4).eq.'PPDB')then
          nbdomain=nbdomain+1
          nbd=nbdomain
          idomain(nbd)=6
        else
          WRITE(OUTS,'(3A)') 'Domain: ',WORD,' is not supported.'
          CALL USRMSG(OUTS,' ','W') 
          ier=1
          CALL ERPFREE(ICONFIL,ISTAT)
          return
        endif 
        if(CONT)goto 20
      elseif(WORD(1:14).eq.'*Attribute_cnn'.or.
     &       WORD(1:14).eq.'*attribute_cnn')then

C Extract data for *thermal,*flow,*control,*output ... until *End_cnn_attribute
  42    CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'attributes',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        if(OUTSTR(1:18).eq.'*End_cnn_attribute')then
          goto 20
        else

C Extract connections data for *thermal,*flow,*control,*output etc. groups.
C The 1st word signals which 'group' the attribute is in, the
C 2nd word the data type (integer/real/text)
          if(iatrdom(nbd)+1.gt.MIATRB)goto 43
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','atrib group',IER)
          IF(IER.NE.0) CONT=.FALSE.
          if(WORD(1:8).eq.'*thermal'.or.WORD(1:8).eq.'*Thermal')then
            iatrdom(nbd)=iatrdom(nbd)+1
            dtagatr(nbd,iatrdom(nbd),1)='thermal'
          elseif(WORD(1:5).eq.'*flow'.or.WORD(1:5).eq.'*Flow')then
            iatrdom(nbd)=iatrdom(nbd)+1
            dtagatr(nbd,iatrdom(nbd),1)='flow'
          elseif(WORD(1:8).eq.'*control'.or.WORD(1:8).eq.'*Control')then
            iatrdom(nbd)=iatrdom(nbd)+1
            dtagatr(nbd,iatrdom(nbd),1)='control'
          elseif(WORD(1:7).eq.'*output'.or.WORD(1:7).eq.'*Output')then
            iatrdom(nbd)=iatrdom(nbd)+1
            dtagatr(nbd,iatrdom(nbd),1)='output'
          elseif(WORD(1:9).eq.'*location'.or.
     &           WORD(1:9).eq.'*Location')then
            iatrdom(nbd)=iatrdom(nbd)+1
            dtagatr(nbd,iatrdom(nbd),1)='location'
          elseif(WORD(1:11).eq.'*electrical'.or.
     &           WORD(1:11).eq.'*Electrical')then
            iatrdom(nbd)=iatrdom(nbd)+1
            dtagatr(nbd,iatrdom(nbd),1)='location'
          elseif(WORD(1:12).eq.'*primativept'.or.
     &           WORD(1:12).eq.'*PrimativePt')then
            iatrdom(nbd)=iatrdom(nbd)+1
            dtagatr(nbd,iatrdom(nbd),1)='primativept'
          endif
          IF(IER.NE.0) CONT=.FALSE.

C Scan three words, the first is the data type, 2nd is either the
C string `external` or `-` and the 3rd is either a tag to find
C in an external file or `-`
          CALL EGETW(OUTSTR,K,WORD,'W','atrib data type',IER)
          CALL EGETW(OUTSTR,K,WORD1,'W','atrib external or -',IER)
          write(dtagatr(nbd,iatrdom(nbd),3),'(a)') WORD1(1:12)
          CALL EGETW(OUTSTR,K,WORD2,'W','atrib extrn tag or -',IER)
          write(dtagatr(nbd,iatrdom(nbd),4),'(a)') WORD2(1:12)

C Based on the data type parse the remaining items on the line.
          if(WORD(1:4).eq.'intg'.or.WORD(1:4).eq.'INTG')then
            dtagatr(nbd,iatrdom(nbd),2)='intg'
            CALL EGETW(OUTSTR,K,WORD,'W','intg value',IER)
            write(datrib(nbd,iatrdom(nbd),1),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','intg min',IER)
            write(datrib(nbd,iatrdom(nbd),2),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','intg max',IER)
            write(datrib(nbd,iatrdom(nbd),3),'(a)') WORD(1:12)
          elseif(WORD(1:4).eq.'real'.or.WORD(1:4).eq.'REAL')then
            dtagatr(nbd,iatrdom(nbd),2)='real'
            CALL EGETW(OUTSTR,K,WORD,'W','real value',IER)
            write(datrib(nbd,iatrdom(nbd),1),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','real min',IER)
            write(datrib(nbd,iatrdom(nbd),2),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','real max',IER)
            write(datrib(nbd,iatrdom(nbd),3),'(a)') WORD(1:12)
          elseif(WORD(1:4).eq.'text'.or.WORD(1:4).eq.'TEXT')then
            dtagatr(nbd,iatrdom(nbd),2)='text'
            CALL EGETW(OUTSTR,K,WORD,'W','data string',IER)
            write(datrib(nbd,iatrdom(nbd),1),'(a)') WORD(1:12)
            datrib(nbd,iatrdom(nbd),2)=' '
            datrib(nbd,iatrdom(nbd),3)=' '
          endif
          CALL EGETW(OUTSTR,K,WORD,'W','static:user',IER)
          write(dtagatr(nbd,iatrdom(nbd),5),'(a)') WORD(1:12)
          CALL EGETRM(OUTSTR,K,PHRASE,'W','atrib menu',IER)
          write(dmenuatr(nbd,iatrdom(nbd)),'(a)') PHRASE(1:32)
          IF(IER.NE.0) CONT=.FALSE.
          if(CONT)goto 42
        endif
      elseif(WORD(1:17).eq.'*Attribute_global'.or.
     &       WORD(1:17).eq.'*attribute_global')then

C Extract global attribute data for *thermal,*flow,*control,*output ...
C until *End_global_attribute
 142    CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'global attributes',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        if(OUTSTR(1:21).eq.'*End_global_attribute')then
          goto 20
        else

C Extract global data for *thermal,*flow,*control,*output etc. groups.
C The 1st word signals which 'group' the attribute is in, the
C 2nd word the data type (integer/real/text)
          if(igatrdom(nbd)+1.gt.MIATRB)goto 43
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','glob atrib group',IER)
          IF(IER.NE.0) CONT=.FALSE.
          if(WORD(1:8).eq.'*thermal'.or.WORD(1:8).eq.'*Thermal')then
            igatrdom(nbd)=igatrdom(nbd)+1
            dgtagatr(nbd,igatrdom(nbd),1)='thermal'
          elseif(WORD(1:5).eq.'*flow'.or.WORD(1:5).eq.'*Flow')then
            igatrdom(nbd)=igatrdom(nbd)+1
            dgtagatr(nbd,igatrdom(nbd),1)='flow'
          elseif(WORD(1:8).eq.'*control'.or.WORD(1:8).eq.'*Control')then
            igatrdom(nbd)=igatrdom(nbd)+1
            dgtagatr(nbd,igatrdom(nbd),1)='control'
          elseif(WORD(1:7).eq.'*output'.or.WORD(1:7).eq.'*Output')then
            igatrdom(nbd)=igatrdom(nbd)+1
            dgtagatr(nbd,igatrdom(nbd),1)='output'
          elseif(WORD(1:9).eq.'*location'.or.
     &           WORD(1:9).eq.'*Location')then
            igatrdom(nbd)=igatrdom(nbd)+1
            dgtagatr(nbd,igatrdom(nbd),1)='location'
          elseif(WORD(1:11).eq.'*electrical'.or.
     &           WORD(1:11).eq.'*Electrical')then
            igatrdom(nbd)=igatrdom(nbd)+1
            dgtagatr(nbd,igatrdom(nbd),1)='location'
          elseif(WORD(1:12).eq.'*primativept'.or.
     &           WORD(1:12).eq.'*PrimativePt')then
            igatrdom(nbd)=igatrdom(nbd)+1
            dgtagatr(nbd,igatrdom(nbd),1)='primativept'
          endif
          IF(IER.NE.0) CONT=.FALSE.

C Scan three words, the first is the data type, 2nd is either the
C string `external` or `-` and the 3rd is either a tag to find
C in an external file or `-`
          CALL EGETW(OUTSTR,K,WORD,'W','glo atrib data type',IER)
          CALL EGETW(OUTSTR,K,WORD1,'W','glo atrib external or -',IER)
          write(dgtagatr(nbd,igatrdom(nbd),3),'(a)') WORD1(1:12)
          CALL EGETW(OUTSTR,K,WORD2,'W','glo atrib extrn tag or -',IER)
          write(dgtagatr(nbd,igatrdom(nbd),4),'(a)') WORD2(1:12)

C Based on the data type parse the remaining items on the line.
          if(WORD(1:4).eq.'intg'.or.WORD(1:4).eq.'INTG')then
            dgtagatr(nbd,igatrdom(nbd),2)='intg'
            CALL EGETW(OUTSTR,K,WORD,'W','glo intg value',IER)
            write(dgatrib(nbd,igatrdom(nbd),1),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','glo intg min',IER)
            write(dgatrib(nbd,igatrdom(nbd),2),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','glo intg max',IER)
            write(dgatrib(nbd,igatrdom(nbd),3),'(a)') WORD(1:12)
          elseif(WORD(1:4).eq.'real'.or.WORD(1:4).eq.'REAL')then
            dgtagatr(nbd,igatrdom(nbd),2)='real'
            CALL EGETW(OUTSTR,K,WORD,'W','glo real value',IER)
            write(dgatrib(nbd,igatrdom(nbd),1),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','glo real min',IER)
            write(dgatrib(nbd,igatrdom(nbd),2),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','glo real max',IER)
            write(dgatrib(nbd,igatrdom(nbd),3),'(a)') WORD(1:12)
          elseif(WORD(1:4).eq.'text'.or.WORD(1:4).eq.'TEXT')then
            dgtagatr(nbd,igatrdom(nbd),2)='text'
            CALL EGETW(OUTSTR,K,WORD,'W','glo data string',IER)
            write(dgatrib(nbd,igatrdom(nbd),1),'(a)') WORD(1:12)
            dgatrib(nbd,igatrdom(nbd),2)=' '
            dgatrib(nbd,igatrdom(nbd),3)=' '
          endif
          CALL EGETW(OUTSTR,K,WORD,'W','static:user',IER)
          write(dgtagatr(nbd,igatrdom(nbd),5),'(a)') WORD(1:12)
          CALL EGETRM(OUTSTR,K,PHRASE,'W','glo atrib menu',IER)
          write(dgmenuatr(nbd,igatrdom(nbd)),'(a)') PHRASE(1:32)
          IF(IER.NE.0) CONT=.FALSE.
          if(CONT)goto 142
        endif
      elseif(WORD(1:9).eq.'*Category'.or.WORD(1:9).eq.'*category')then
        CALL EGETW(OUTSTR,K,WORD,'W','category tag',IER)
        IF(IER.NE.0) CONT=.FALSE.
        nbcat(nbd)=nbcat(nbd)+1
        nbc=nbcat(nbd)
        write(cattag(nbd,nbc),'(a)') WORD(1:12)
        CALL EGETRM(OUTSTR,K,PHRASE,'W','category menu',IER)
        IF(IER.NE.0) CONT=.FALSE.
        write(catmenu(nbd,nbc),'(a)') PHRASE(1:32)
        if(CONT)goto 20
      elseif(WORD(1:5).eq.'*Item'.or.WORD(1:5).eq.'*item')then
        CALL EGETW(OUTSTR,K,WORD,'W','item tag',IER)
        IF(IER.NE.0) CONT=.FALSE.
        nbicons(nbd,nbc)=nbicons(nbd,nbc)+1
        nbi=nbicons(nbd,nbc)
        write(icontag(nbd,nbc,nbi),'(a)') WORD(1:12)
        CALL EGETRM(OUTSTR,K,PHRASE,'W','icon menu',IER)
        IF(IER.NE.0) CONT=.FALSE.
        write(iconmenu(nbd,nbc,nbi),'(a)') PHRASE(1:40)
        if(CONT)goto 20
      else
        if(CONT)goto 20
      endif

C Probably reached the end of the file.
  43  IER=0
      CALL ERPFREE(ICONFIL,ISTAT)
      RETURN
      end

C ******** getanicon
C This routine gets information about an icon in an icon database.
C It is passed the domain type (domain) and the category type (category)
C and the item type (icon) and returns:
C   number of vertices (nbvert),
C   array of vertices vert(MICNV,2) where 1st is X and 2nd is Y
C   number of edges (nbedge), array of edges (idege(MICNE,4) 1st is
C     start vertex index, 2nd next vertex index, 3rd colour index,
C     4th line style)
C   number of dots (nbdot), array of dots (idot(MICND,4)) 1st is
C     vertex index, 2nd is colour, 3rd is size index)
C   number of internal labels (nblabel), array ilabel and labeltx (4 char)
C   number of arcs (nbarc), array of arc data iarc 1st is vertex index
C     of centre of arc, 2nd is vertex index at some point of radius, 3rd
C     is integer angle clockwise from 3o'clock position to the start
C     of acr, 4th is integer angle clockwise from 3o'clock position to
C     end of arc, 5th is colour name, 6th is colour type, 7th is fill type.
C   number of attachment points (nbatt), array of attachments
C     (iatt(MICNE,2)) where iatt(?,1) is the index of its vertex and
C     iatt(?,2) is attachment (connection) type 0=none, 1=air, 2=water,
C     3=steam, 4=refrigerant, 5=fuel, 6=combustion product,
C     7=signal (0-10v)
C   number of lines of text (nbtext) array of text strings (72 char)
 
      subroutine getanicon(idomain,category,icontag,
     &   nbvert,vert,nbedge,iedge,nbdot,idot,nblabel,ilabel,labeltx,
     &   nbarc,iarc,nbatt,iatt,nbtext,text,IER)
#include "gnetwk.h" 
      
      integer lnblnk  ! function definition

      COMMON/ICONDBNAM/ICONDBFL
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      dimension vert(MICNV,2),iedge(MICNE,5),idot(MICND,4),
     &  iarc(MICND,7),ilabel(MICND,4),labeltx(MICND),text(60),
     &  iatt(MCNP,2)

      CHARACTER OUTSTR*124,outs*124,WORD*24,PHRASE*48
      CHARACTER ICONDBFL*72
      character category*12,icontag*12,labeltx*4,text*72
      character colourstr*12,ctype*1

      LOGICAL CONT
      
C << to be done: put explicit range checks in i.e. so that nbedge
C << is not greater than MICNE.

      IER=0
      CONT=.TRUE.
      ICONFIL=IFIL+1
      
C Open the icon components database file, assume it has been previously
C parsed with scanicondb so only minimal checking is needed.
      IF(ICONDBFL(1:3).EQ.'UNK'.or.ICONDBFL(1:2).EQ.'  ') THEN
        CALL EDISP(IUOUT,'ERROR - could not find the icon database.')
        IER=1
        CALL ERPFREE(ICONFIL,ISTAT)
        RETURN
      ENDIF

      CALL EFOPSEQ(ICONFIL,ICONDBFL,1,IER)
      IF(IER.NE.0) THEN
        CALL EDISP(IUOUT,'ERROR - could not find the icon database.')
        IER=1
        CALL ERPFREE(ICONFIL,ISTAT)
        RETURN
      ENDIF

C Clear the returned parameter list.
      nbtext=0
      nbvert=0
      nbedge=0
      nbdot=0
      nbarc=0
      nbatt=0
      nblabel=0
      do 4 j=1,60
        text(j)='  '
  4   continue
      do 5 k=1,MICNV
        vert(k,1)=0.0
        vert(k,2)=0.0
  5   continue
      do 6 k=1,MICNE
        iedge(k,1)=0
        iedge(k,2)=0
        iedge(k,3)=0
        iedge(k,4)=0
        iedge(k,5)=0
  6   continue
      do 7 k=1,MICND
        idot(k,1)=0
        idot(k,2)=0
        idot(k,3)=0
        idot(k,4)=0
        iarc(k,1)=0
        iarc(k,2)=0
        iarc(k,3)=0
        iarc(k,4)=0
        iarc(k,5)=0
        iarc(k,6)=0
        iarc(k,7)=0
        ilabel(k,1)=0
        ilabel(k,2)=0
        ilabel(k,3)=0
        labeltx(k)=' '
  7   continue

C Check that the opened file is an icons database.
      CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'check file',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      IF(OUTSTR(1:6).EQ.'*Icons')THEN
        continue
      else
        WRITE(OUTS,'(3A)') 'File: ',ICONDBFL(1:LNBLNK(ICONDBFL)), 
     &    ' is not an icons file.'
        CALL USRMSG(OUTS,' ','W') 
        ier=1
        CALL ERPFREE(ICONFIL,ISTAT)
        return
      endif 
   
C Read in the header, look for the domain and see if it matches.
  20  CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'icon header',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','icon domain tag',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:7).eq.'*Domain'.or.WORD(1:7).eq.'*domain')then
        CALL EGETW(OUTSTR,K,WORD,'W','the domain',IER)
        IF(IER.NE.0) CONT=.FALSE.
        if(WORD(1:5).eq.'PLNDB'.and.idomain.eq.1)then
          continue
        elseif(WORD(1:5).eq.'FLODB'.and.idomain.eq.2)then
          continue
        elseif(WORD(1:5).eq.'CTLDB'.and.idomain.eq.4)then
          continue
        elseif(WORD(1:4).eq.'PPDB'.and.idomain.eq.6)then
          continue
        else
          if(CONT)goto 20
        endif 
        if(.NOT.CONT)then
          IER=0
          CALL ERPFREE(ICONFIL,ISTAT)
          RETURN
        endif
      else
        goto 20
      endif

C Read in the categories, see which matches.
  21  CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'categories',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','category tag',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:9).eq.'*Category'.or.WORD(1:9).eq.'*category')then
        CALL EGETW(OUTSTR,K,WORD,'W','category tag',IER)
        IF(IER.NE.0) CONT=.FALSE.

C If correct category process icons otherwise loop again
        if(WORD(1:lnblnk(WORD)).eq.category(1:lnblnk(category)))then
          goto 22
        else
          goto 21
        endif
        if(.NOT.CONT)then
          IER=0
          CALL ERPFREE(ICONFIL,ISTAT)
          RETURN
        endif
      else
        goto 21
      endif

C Read in the icons, see if it matches.
  22  CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'icons',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','icon tag',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:5).eq.'*Item'.or.WORD(1:5).eq.'*item')then
        CALL EGETW(OUTSTR,K,WORD,'W','item tag',IER)
        IF(IER.NE.0) CONT=.FALSE.
        if(WORD(1:lnblnk(WORD)).eq.icontag(1:lnblnk(icontag)))then
          goto 23
        else
          goto 22
        endif
        if(.NOT.CONT)then
          IER=0
          CALL ERPFREE(ICONFIL,ISTAT)
          RETURN
        endif
      else
        goto 22
      endif

C Read icon data.
  23  CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'icons',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','icon tag',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:10).eq.'*Attribute'.or.WORD(1:10).eq.'*attribute')then

C Skip over reading of attributes (done in another subroutine).
  42    CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'icons',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        if(OUTSTR(1:14).eq.'*End_attribute')then
          continue
        else
          goto 42
        endif
        if(CONT)goto 23
      elseif(WORD(1:5).eq.'*Text'.or.WORD(1:5).eq.'*text')then
  25    CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'icons',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        if(OUTSTR(1:9).eq.'*End_Text')then
          continue
        else
          nbtext=nbtext+1
          write(text(nbtext),'(a)') OUTSTR(1:72)
          goto 25
        endif
        if(CONT)goto 23
      elseif(WORD(1:7).eq.'*Vertex'.or.WORD(1:7).eq.'*vertex')then
        CALL EGETWR(OUTSTR,K,VAL1,0.,0.,'-','icon vert X',IER)
        CALL EGETWR(OUTSTR,K,VAL2,0.,0.,'-','icon vert Y',IER)
        IF(IER.NE.0) CONT=.FALSE.
        nbvert=nbvert+1
        vert(nbvert,1)=VAL1
        vert(nbvert,2)=VAL2
        if(CONT)goto 23
      elseif(WORD(1:5).eq.'*Edge'.or.WORD(1:5).eq.'*edge')then

C For each edge get start and end vertex and colour string.
        CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','start vertex',IER)
        CALL EGETWI(OUTSTR,K,IVAL2,0,0,'-','end vertex',IER)
        IF(IER.NE.0) CONT=.FALSE.
        nbedge=nbedge+1
        iedge(nbedge,1)=IVAL1
        iedge(nbedge,2)=IVAL2
        if(ND.gt.3)then
          CALL EGETW(OUTSTR,K,colourstr,'W','edge colour',IER)
          call whichzonecolour(colourstr,indxzncolour,ctype)
          iedge(nbedge,3)=indxzncolour
          iedge(nbedge,4)=0
          if(ctype.eq.'i')iedge(nbedge,4)=0
          if(ctype.eq.'g')iedge(nbedge,4)=1
          if(ctype.eq.'z')iedge(nbedge,4)=2
          CALL EGETW(OUTSTR,K,phrase,'W','edge line style',IER)
          if(phrase(1:5).eq.'solid')then
            iedge(nbedge,5)=1
          elseif(phrase(1:6).eq.'dotted')then
            iedge(nbedge,5)=2
          elseif(phrase(1:6).eq.'dashed')then
            iedge(nbedge,5)=3
          elseif(phrase(1:6).eq.'double')then
            iedge(nbedge,5)=4
          else
            iedge(nbedge,5)=0
          endif
        else
          colourstr='black'
          call whichzonecolour(colourstr,indxzncolour,ctype)
          iedge(nbedge,3)=indxzncolour
          iedge(nbedge,4)=1
        endif
        if(CONT)goto 23
      elseif(WORD(1:4).eq.'*Dot'.or.WORD(1:4).eq.'*dot')then
        CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','dot vertex',IER)
        IF(IER.NE.0) CONT=.FALSE.
        nbdot=nbdot+1
        idot(nbdot,1)=IVAL1
        CALL EGETW(OUTSTR,K,colourstr,'W','dot colour',IER)
        call whichzonecolour(colourstr,indxzncolour,ctype)
        idot(nbdot,2)=indxzncolour
        idot(nbdot,3)=0
        if(ctype.eq.'i')idot(nbdot,3)=0
        if(ctype.eq.'g')idot(nbdot,3)=1
        if(ctype.eq.'z')idot(nbdot,3)=2
        CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','dot size index',IER)
        idot(nbdot,4)=IVAL1
        if(CONT)goto 23
      elseif(WORD(1:4).eq.'*Arc'.or.WORD(1:4).eq.'*arc')then
        CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','arc cnt vertex',IER)
        IF(IER.NE.0) CONT=.FALSE.
        nbarc=nbarc+1
        iarc(nbarc,1)=IVAL1
        CALL EGETWI(OUTSTR,K,IVAL2,0,0,'-','arc rad vertex',IER)
        IF(IER.NE.0) CONT=.FALSE.
        iarc(nbarc,2)=IVAL2
        CALL EGETWI(OUTSTR,K,IVAL3,-360,360,'w','arc ang 1',IER)
        IF(IER.NE.0) CONT=.FALSE.
        iarc(nbarc,3)=IVAL3
        CALL EGETWI(OUTSTR,K,IVAL4,-360,360,'w','arc ang 2',IER)
        IF(IER.NE.0) CONT=.FALSE.
        iarc(nbarc,4)=IVAL4
        CALL EGETW(OUTSTR,K,colourstr,'W','arc colour',IER)
        call whichzonecolour(colourstr,indxzncolour,ctype)
        iarc(nbarc,5)=indxzncolour
        iarc(nbarc,6)=0
        if(ctype.eq.'i')iarc(nbarc,6)=0
        if(ctype.eq.'g')iarc(nbarc,6)=1
        if(ctype.eq.'z')iarc(nbarc,6)=2
        CALL EGETW(OUTSTR,K,phrase,'W','arc fill',IER)
        if(phrase(1:4).eq.'fill')then
          iarc(nbarc,7)=1
        elseif(phrase(1:6).eq.'nofill')then
          iarc(nbarc,7)=0
        endif
        if(CONT)goto 23
      elseif(WORD(1:6).eq.'*Label'.or.WORD(1:6).eq.'*label')then
        CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','label vertex',IER)
        IF(IER.NE.0) CONT=.FALSE.
        nblabel=nblabel+1
        ilabel(nblabel,1)=IVAL1
        CALL EGETW(OUTSTR,K,colourstr,'W','label colour',IER)
        call whichzonecolour(colourstr,indxzncolour,ctype)
        ilabel(nblabel,2)=indxzncolour
        ilabel(nblabel,3)=0
        if(ctype.eq.'i')ilabel(nblabel,3)=0
        if(ctype.eq.'g')ilabel(nblabel,3)=1
        if(ctype.eq.'z')ilabel(nblabel,3)=2
        CALL EGETW(OUTSTR,K,labeltx(nblabel),'W','label text',IER)
        if(CONT)goto 23
      elseif(WORD(1:7).eq.'*Attach'.or.WORD(1:7).eq.'*attach')then
        nbatt=nbatt+1
        CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','attachment vertex',IER)
        IF(IER.NE.0) CONT=.FALSE.
        iatt(nbatt,1)=IVAL1
        if(ND.gt.2)then
          CALL EGETWI(OUTSTR,K,IVAL1,0,0,'-','attachment type',IER)
          IF(IER.NE.0) CONT=.FALSE.
          iatt(nbatt,2)=IVAL1
        else
          iatt(nbatt,2)=0
        endif
        if(CONT)goto 23
      else
      endif

C Probably reached the end of the file.
      IER=0
      CALL ERPFREE(ICONFIL,ISTAT)
      RETURN
      end


C ******** geticonatr
C This routine gets attributes of an icon in an icon.
C It is passed the domain type (domain) and the category type (category)
C and the item type (icon) and returns:
C  total number of attributes (natrib),
C  array of tags (12 char) (tagatr) where the first is the
C   group (thermal/flow/control/output/location/environment/electrical)
C   the 2nd signals data type,
C   the 3rd is key word 'external' or '-'
C   the 4th is external key word or '-'
C   the 5th is key word 'static' or 'user'
C  array of strings holding attribute data (atrib(MIATRB,3)) depending
C   on which data type: for integers and real ,atrib(?,1) is a value
C   atrib(?,2) is minimum, atrib(?,3) is maximum, for text data 
C   atrib(?,1) is the string attribute and atrib(?,2) & atrib(?,3) blank.
C  array of menu entries (32 char) menuatr(MIATRB)

 
      subroutine geticonatr(idomain,category,icontag,natrib,tagatr,
     &  atrib,menuatr,IER)
#include "gnetwk.h" 
      
      integer lnblnk  ! function definition

      COMMON/ICONDBNAM/ICONDBFL
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      dimension tagatr(MIATRB,5),atrib(MIATRB,3),menuatr(MIATRB)

      CHARACTER OUTSTR*124,outs*124,WORD*24,WORD1*24,WORD2*24,PHRASE*48
      CHARACTER ICONDBFL*72
      character category*12,icontag*12
      character tagatr*12,menuatr*32,atrib*12

      LOGICAL CONT

      IER=0
      CONT=.TRUE.
      ICONFIL=IFIL+1
      
C Open the icon components database file, assume it has been previously
C parsed with scanicondb so only minimal checking is needed.
      IF(ICONDBFL(1:3).EQ.'UNK'.or.ICONDBFL(1:2).EQ.'  ') THEN
        CALL EDISP(IUOUT,'ERROR - could not find the icon database.')
        IER=1
        CALL ERPFREE(ICONFIL,ISTAT)
        RETURN
      ENDIF

      CALL EFOPSEQ(ICONFIL,ICONDBFL,1,IER)
      IF(IER.NE.0) THEN
        CALL EDISP(IUOUT,'ERROR - could not find the icon database.')
        IER=1
        CALL ERPFREE(ICONFIL,ISTAT)
        RETURN
      ENDIF

C Clear the returned parameter list.
      natrib=0
      do 3 i=1,MIATRB
        tagatr(i,1)=' '
        tagatr(i,2)=' '
        tagatr(i,3)=' '
        tagatr(i,4)=' '
        tagatr(i,5)=' '
        atrib(i,1)=' '
        atrib(i,2)=' '
        atrib(i,3)=' '
	menuatr(i)=' '
  3   continue

C Check that the opened file is an icons database.
      CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'check file',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      IF(OUTSTR(1:6).EQ.'*Icons')THEN
        continue
      else
        WRITE(OUTS,'(3A)') 'File: ',ICONDBFL(1:LNBLNK(ICONDBFL)), 
     &    ' is not an icons file.'
        CALL USRMSG(OUTS,' ','W') 
        ier=1
        CALL ERPFREE(ICONFIL,ISTAT)
        return
      endif 
   
C Read in the header, look for the domain and see if it matches.
  20  CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'icon header',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','icon domain tag',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:7).eq.'*Domain'.or.WORD(1:7).eq.'*domain')then
        CALL EGETW(OUTSTR,K,WORD,'W','the domain',IER)
        IF(IER.NE.0) CONT=.FALSE.
        if(WORD(1:5).eq.'PLNDB'.and.idomain.eq.1)then
          continue
        elseif(WORD(1:5).eq.'FLODB'.and.idomain.eq.2)then
          continue
        elseif(WORD(1:5).eq.'CTLDB'.and.idomain.eq.4)then
          continue
        elseif(WORD(1:4).eq.'PPDB'.and.idomain.eq.6)then
          continue
        else
          if(CONT)goto 20
        endif 
        if(.NOT.CONT)then
          IER=0
          CALL ERPFREE(ICONFIL,ISTAT)
          RETURN
        endif
      else
        goto 20
      endif

C Read in the categories, see which matches.
  21  CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'categories',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','category tag',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:9).eq.'*Category'.or.WORD(1:9).eq.'*category')then
        CALL EGETW(OUTSTR,K,WORD,'W','category tag',IER)
        IF(IER.NE.0) CONT=.FALSE.

C If correct category process icons otherwise loop again
        if(WORD(1:lnblnk(WORD)).eq.category(1:lnblnk(category)))then
          goto 22
        else
          goto 21
        endif
        if(.NOT.CONT)then
          IER=0
          CALL ERPFREE(ICONFIL,ISTAT)
          RETURN
        endif
      else
        goto 21
      endif

C Read in the icons, see if it matches.
  22  CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'icons',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','icon tag',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:5).eq.'*Item'.or.WORD(1:5).eq.'*item')then
        CALL EGETW(OUTSTR,K,WORD,'W','item tag',IER)
        IF(IER.NE.0) CONT=.FALSE.
        if(WORD(1:lnblnk(WORD)).eq.icontag(1:lnblnk(icontag)))then
          goto 23
        else
          goto 22
        endif
        if(.NOT.CONT)then
          IER=0
          CALL ERPFREE(ICONFIL,ISTAT)
          RETURN
        endif
      else
        goto 22
      endif

C Read icon data.
  23  CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'icons',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','icon tag',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:10).eq.'*Attribute'.or.WORD(1:10).eq.'*attribute')then

C Extract data for *thermal,*flow,*control,*output ... until *End_attribute
  42    CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'attributes',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        if(OUTSTR(1:14).eq.'*End_attribute')then
          continue
        else

C Extract data for *thermal,*flow,*control,*output etc. groups.
C The 1st word signals which 'group' the attribute is in, the
C 2nd word the data type (integer/real/text)
          if(natrib+1.gt.MIATRB)goto 43
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','atrib group',IER)
          IF(IER.NE.0) CONT=.FALSE.
          if(WORD(1:8).eq.'*thermal'.or.WORD(1:8).eq.'*Thermal')then
            natrib=natrib+1
            tagatr(natrib,1)='thermal'
          elseif(WORD(1:5).eq.'*flow'.or.WORD(1:5).eq.'*Flow')then
            natrib=natrib+1
            tagatr(natrib,1)='flow'
          elseif(WORD(1:8).eq.'*control'.or.WORD(1:8).eq.'*Control')then
            natrib=natrib+1
            tagatr(natrib,1)='control'
          elseif(WORD(1:7).eq.'*output'.or.WORD(1:7).eq.'*Output')then
            natrib=natrib+1
            tagatr(natrib,1)='output'
          elseif(WORD(1:9).eq.'*location'.or.
     &           WORD(1:9).eq.'*Location')then
            natrib=natrib+1
            tagatr(natrib,1)='location'
          elseif(WORD(1:11).eq.'*electrical'.or.
     &           WORD(1:11).eq.'*Electrical')then
            natrib=natrib+1
            tagatr(natrib,1)='location'
          elseif(WORD(1:12).eq.'*primativept'.or.
     &           WORD(1:12).eq.'*PrimativePt')then
            natrib=natrib+1
            tagatr(natrib,1)='primativept'
          endif
          IF(IER.NE.0) CONT=.FALSE.

C Scan three words, the first is the data type, 2nd is either the
C string `external` or `-` and the 3rd is either a tag to find
C in an external file or `-`
          CALL EGETW(OUTSTR,K,WORD,'W','atrib data type',IER)
          CALL EGETW(OUTSTR,K,WORD1,'W','atrib external or -',IER)
          write(tagatr(natrib,3),'(a)') WORD1(1:12)
          CALL EGETW(OUTSTR,K,WORD2,'W','atrib ext tag or -',IER)
          write(tagatr(natrib,4),'(a)') WORD2(1:12)

C Based on the data type parse the remaining items on the line.
          if(WORD(1:4).eq.'intg'.or.WORD(1:4).eq.'INTG')then
            tagatr(natrib,2)='intg'
            CALL EGETW(OUTSTR,K,WORD,'W','intg value',IER)
            write(atrib(natrib,1),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','intg min',IER)
            write(atrib(natrib,2),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','intg max',IER)
            write(atrib(natrib,3),'(a)') WORD(1:12)
          elseif(WORD(1:4).eq.'real'.or.WORD(1:4).eq.'REAL')then
            tagatr(natrib,2)='real'
            CALL EGETW(OUTSTR,K,WORD,'W','real value',IER)
            write(atrib(natrib,1),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','real min',IER)
            write(atrib(natrib,2),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','real max',IER)
            write(atrib(natrib,3),'(a)') WORD(1:12)
          elseif(WORD(1:4).eq.'text'.or.WORD(1:4).eq.'TEXT')then
            tagatr(natrib,2)='text'
            CALL EGETW(OUTSTR,K,WORD,'W','data string',IER)
            write(atrib(natrib,1),'(a)') WORD(1:12)
            atrib(natrib,2)=' '
            atrib(natrib,3)=' '
          endif
          CALL EGETW(OUTSTR,K,WORD,'W','static:user',IER)
          write(tagatr(natrib,5),'(a)') WORD(1:12)
          CALL EGETRM(OUTSTR,K,PHRASE,'W','atrib menu',IER)
	  write(menuatr(natrib),'(a)') PHRASE(1:32)
          IF(IER.NE.0) CONT=.FALSE.
          if(CONT)goto 42
        endif
      elseif(WORD(1:5).eq.'*Text'.or.WORD(1:5).eq.'*text')then
  25    CALL STRIPC(ICONFIL,OUTSTR,99,ND,0,'icons',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        if(OUTSTR(1:9).eq.'*End_Text')then
          continue
        else
          goto 25
        endif
        if(CONT)goto 23
      elseif(WORD(1:7).eq.'*Vertex'.or.WORD(1:7).eq.'*vertex')then
        goto 23
      elseif(WORD(1:5).eq.'*Edge'.or.WORD(1:5).eq.'*edge')then
        goto 23
      elseif(WORD(1:4).eq.'*Dot'.or.WORD(1:4).eq.'*dot')then
        goto 23
      elseif(WORD(1:4).eq.'*Arc'.or.WORD(1:4).eq.'*arc')then
         goto 23
      elseif(WORD(1:6).eq.'*Label'.or.WORD(1:6).eq.'*label')then
        goto 23
      elseif(WORD(1:7).eq.'*Attach'.or.WORD(1:7).eq.'*attach')then
        goto 23
      else
      endif

C Probably reached the end of the file.
  43  IER=0
      CALL ERPFREE(ICONFIL,ISTAT)
      RETURN
      end


C ***** whichzonecolour()
C whichzonecolour is given a colour string within the list offered
C by zscalestr[] in wwcut.c and returns index between 0 and 28 and
C type = 'z'.  Or if the string is within the list offered by
C gscalestr it returns an index between 0 and 28 and type = 'g'.
C Or if black return 6 and white return 7 with type = 'i'.
      subroutine whichzonecolour(colourstr,indxzncolour,type)
      integer lnblnk  ! function definition
      character colourstr*12,type*1

      indxzncolour = -1
      lc=lnblnk(colourstr)
      if(lc.le.1.or.colourstr(1:2).eq.'  ')then
        type=' '
        return
      endif
      if(colourstr(1:lc).eq.'black')then
        indxzncolour = 6
        type='i'
      elseif(colourstr(1:lc).eq.'white')then
        indxzncolour = 7
        type='i'
      elseif(colourstr(1:lc).eq.'red')then
        indxzncolour = 0
        type='z'
      elseif(colourstr(1:lc).eq.'MidnightBlue')then
        indxzncolour = 1
        type='z'
      elseif(colourstr(1:lc).eq.'peru')then
        indxzncolour = 2
        type='z'
      elseif(colourstr(1:lc).eq.'ForestGreen')then
        indxzncolour = 3
        type='z'
      elseif(colourstr(1:6).eq.'khaki ')then
        indxzncolour = 4
        type='z'
      elseif(colourstr(1:lc).eq.'turquoise')then
        indxzncolour = 5
        type='z'
      elseif(colourstr(1:lc).eq.'magenta')then
        indxzncolour = 6
        type='z'
      elseif(colourstr(1:lc).eq.'firebrick')then
        indxzncolour = 7
        type='z'
      elseif(colourstr(1:lc).eq.'DarkCyan')then
        indxzncolour = 8
        type='z'
      elseif(colourstr(1:lc).eq.'khaki3')then
        indxzncolour = 9
        type='z'
      elseif(colourstr(1:lc).eq.'RoyalBlue')then
        indxzncolour = 10
        type='z'
      elseif(colourstr(1:lc).eq.'tomato')then
        indxzncolour = 11
        type='z'
      elseif(colourstr(1:lc).eq.'OliveDrab')then
        indxzncolour = 12
        type='z'
      elseif(colourstr(1:lc).eq.'PaleGreen')then
        indxzncolour = 13
        type='z'
      elseif(colourstr(1:lc).eq.'orange')then
        indxzncolour = 14
        type='z'
      elseif(colourstr(1:lc).eq.'grey40')then
        indxzncolour = 15
        type='z'
      elseif(colourstr(1:lc).eq.'coral2')then
        indxzncolour = 16
        type='z'
      elseif(colourstr(1:lc).eq.'grey60')then
        indxzncolour = 17
        type='z'
      elseif(colourstr(1:lc).eq.'maroon4')then
        indxzncolour = 18
        type='z'
      elseif(colourstr(1:lc).eq.'gold3')then
        indxzncolour = 19
        type='z'
      elseif(colourstr(1:lc).eq.'PowderBlue')then
        indxzncolour = 20
        type='z'
      elseif(colourstr(1:lc).eq.'sienna')then
        indxzncolour = 21
        type='z'
      elseif(colourstr(1:lc).eq.'azure4')then
        indxzncolour = 22
        type='z'
      elseif(colourstr(1:lc).eq.'grey20')then
        indxzncolour = 23
        type='z'
      elseif(colourstr(1:lc).eq.'grey50')then
        indxzncolour = 24
        type='z'
      elseif(colourstr(1:lc).eq.'NavyBlue')then
        indxzncolour = 24
        type='z'
      elseif(colourstr(1:lc).eq.'DarkGreen')then
        indxzncolour = 25
        type='z'
      elseif(colourstr(1:lc).eq.'gold')then
        indxzncolour = 26
        type='z'
      elseif(colourstr(1:lc).eq.'grey80')then
        indxzncolour = 27
        type='z'
      elseif(colourstr(1:lc).eq.'grey97')then
        indxzncolour = 0
        type='g'
      elseif(colourstr(1:lc).eq.'grey94')then
        indxzncolour = 1
        type='g'
      elseif(colourstr(1:lc).eq.'grey91')then
        indxzncolour = 2
        type='g'
      elseif(colourstr(1:lc).eq.'grey88')then
        indxzncolour = 3
        type='g'
      elseif(colourstr(1:lc).eq.'grey85')then
        indxzncolour = 4
        type='g'
      elseif(colourstr(1:lc).eq.'grey82')then
        indxzncolour = 5
        type='g'
      elseif(colourstr(1:lc).eq.'grey79')then
        indxzncolour = 6
        type='g'
      elseif(colourstr(1:lc).eq.'grey76')then
        indxzncolour = 7
        type='g'
      elseif(colourstr(1:lc).eq.'grey73')then
        indxzncolour = 8
        type='g'
      elseif(colourstr(1:lc).eq.'grey70')then
        indxzncolour = 9
        type='g'
      elseif(colourstr(1:lc).eq.'grey67')then
        indxzncolour = 0
        type='g'
      elseif(colourstr(1:lc).eq.'grey64')then
        indxzncolour = 11
        type='g'
      elseif(colourstr(1:lc).eq.'grey61')then
        indxzncolour = 12
        type='g'
      elseif(colourstr(1:lc).eq.'grey58')then
        indxzncolour = 13
        type='g'
      elseif(colourstr(1:lc).eq.'grey55')then
        indxzncolour = 14
        type='g'
      elseif(colourstr(1:lc).eq.'grey52')then
        indxzncolour = 15
        type='g'
      elseif(colourstr(1:lc).eq.'grey49')then
        indxzncolour = 16
        type='g'
      elseif(colourstr(1:lc).eq.'grey46')then
        indxzncolour = 17
        type='g'
      elseif(colourstr(1:lc).eq.'grey43')then
        indxzncolour = 18
        type='g'
      elseif(colourstr(1:lc).eq.'grey40')then
        indxzncolour = 19
        type='g'
      elseif(colourstr(1:lc).eq.'grey37')then
        indxzncolour = 20
        type='g'
      elseif(colourstr(1:lc).eq.'grey34')then
        indxzncolour = 21
        type='g'
      elseif(colourstr(1:lc).eq.'grey31')then
        indxzncolour = 22
        type='g'
      elseif(colourstr(1:lc).eq.'grey28')then
        indxzncolour = 23
        type='g'
      elseif(colourstr(1:lc).eq.'grey25')then
        indxzncolour = 24
        type='g'
      elseif(colourstr(1:lc).eq.'grey22')then
        indxzncolour = 25
        type='g'
      elseif(colourstr(1:lc).eq.'grey19')then
        indxzncolour = 26
        type='g'
      elseif(colourstr(1:lc).eq.'grey16')then
        indxzncolour = 27
        type='g'
      elseif(colourstr(1:lc).eq.'grey14')then
        indxzncolour = 28
        type='g'
      else
        indxzncolour = -1
      endif
      return
      end

