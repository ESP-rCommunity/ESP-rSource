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

C The routines for reading and writing a network graphics file are 
C contained in this file: 
C   NETREAD - reads the network graphics file and can be called with 
C             mode 'S' - silent and mode 'R' - reporting mode.
C   NETWRITE- writes the network graphics file.
C   NWKINIT - initialisation for network common blocks. 


C ******************************** NETREAD *********************************
C Read tag-data version of graphic network file.
C MODE = `R` to read an existing file and MODE = `N` to read a new or unknown
C file. MODE = `S` to read silently. INUNIT is the file unit to use
C when reading the file.
      SUBROUTINE NETREAD(INUNIT,MODE,IER)

#include "gnetwk.h"
#include "espriou.h"
C espriou.h provides currentfile.
      
      integer lnblnk  ! function definition

C Graphics and ESP-r default commons
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/POPHELP/H(60)

C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)

C Icon commons
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)

      COMMON/NWKICS/NWICNM(MICN),NWICNHIS(MICN),NWICNLBL(MICN,MICND),
     & NWICNTXT(MICN,60)

C Connection commons
      COMMON/NWKCON/NICNN,ICNS(MNCNN),ICNE(MNCNN),ICNNT(MNCNN),
     & ICNSP(MNCNN),ICNEP(MNCNN),CNWNP(MNCNN,MCIP,3),
     & NCONWP(MNCNN),idatrdom(MNCNN),ddtagatr(MNCNN,MIATRB,5),
     & ddatrib(MNCNN,MIATRB,3),ddmenuatr(MNCNN,MIATRB)

C Global attributes commons (associated with igatrdom(),dgtagatr(),
C     &  dgatrib(),dgmenuatr() passed back via scanicondb call.
      COMMON/NWKGLOB/idgatrdom,ddgtagatr(MIATRB,5),
     & ddgatrib(MIATRB,3),ddgmenuatr(MIATRB)

C Grid commons
      COMMON/NWKGRD/GRMAX(3),GRSPC(3),GRLYRH(MLYRS)
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKFLNAM,NWKTYPSTR(MNWKTYP)
      COMMON/NWKTYP/INWKTYP,vergnf
      COMMON/NWKGRDL/GON,SON
      COMMON/ICONDBNAM/ICONDBFL

      CHARACTER OUTS*124,OUTSTR*124,WORD*24,WORD1*24,WORD2*24,WORD3*40
      CHARACTER*72 NWKNAM,NWKDSC,NWKFLNAM,ICONDBFL,DFILE
      CHARACTER NWICNM*12,NWICNHIS*40,NWICNLBL*4,NWKTYPSTR*12
      character NWICNTXT*72
      CHARACTER LFIL*72,H*72,MODE*1,ectime*24,PHRASE*48
      character ATRTAG*12,ATRMENU*32,ATRICN*12
      character ddtagatr*12,ddmenuatr*32,ddatrib*12
      character ddgtagatr*12,ddgmenuatr*32,ddgatrib*12
      LOGICAL ISEL,CSEL,GON,SON,CONT,OK,dok

C Set a local counter for the number of icons INNINC. 
      INNICN=0
      CONT=.TRUE.

C Open the file with user defined or existing filename
  2   DFILE='UNKNOWN'
      IF(NWKFLNAM(1:3).NE.'   '.AND.NWKFLNAM(1:3).NE.'UNK')THEN
        LFIL=NWKFLNAM
      ELSE
        LFIL='UNKNOWN'
      ENDIF
      H(1)='A network graphics description file holds information    '
      H(2)='on network topology: the components, their connectivity, '
      H(3)='component spatial position, component data and connection'
      H(4)='routing.                                                 '
      IF(MODE.EQ.'R'.OR.MODE.EQ.'N')THEN
        CALL EASKS(LFIL,' Network graphics description file? ',' ',72,
     &    DFILE,' file open',IER,4)
        IF(LFIL(1:3).NE.'   '.AND.LFIL(1:3).NE.'UNK') THEN
          NWKFLNAM=LFIL
        ELSE
          dok=.true.
          h(1)='You gave a blank file name or UNKNOWN for the name. '
          CALL ASKOK('Filename blank - try again?',' ',OK,dok,1)
          IF(OK)THEN
            GOTO 2
          ELSE
            RETURN
          ENDIF
        ENDIF
      ELSE

C Read in silently.
        IF(LFIL(1:3).NE.'   '.AND.LFIL(1:3).NE.'UNK') THEN
          WRITE(OUTS,'(3A)')' Reading network graphics ...',
     &      NWKFLNAM(1:LNBLNK(NWKFLNAM)),'.'
          CALL USRMSG(OUTS,' ','-')
        ELSE
          CALL EDISP(IUOUT,'Warning: network graphics filename was')
          CALL EDISP(IUOUT,'blank - network has not been read in.')
          RETURN
        ENDIF
      ENDIF

C Open the network graphics file.
      CALL EFOPSEQ(INUNIT,NWKFLNAM,1,IER)
      IF(IER.EQ.-301) THEN
        IF(MODE.NE.'S') THEN
          WRITE(OUTS,'(3A)')
     &  'File ',LFIL(1:LNBLNK(LFIL)),' is a new network description.'
          dok=.true.
          h(1)='The file does not already exist and will be assumed to'
          h(2)='be for a new network unless you say no. '
          CALL ASKOK(OUTS,'Define a new network using this name?',
     &      OK,dok,2)     
          IF(OK) THEN
            RETURN
          ELSE
            GOTO 2
          ENDIF
        ELSE
          CALL EDISP(IUOUT,'Warning: graphics network filename was')
          CALL EDISP(IUOUT,'blank - network has not been read in')
          RETURN
        ENDIF
      ELSEIF(IER.NE.0.AND.IER.NE.-301) THEN
        WRITE(OUTS,'(3A)')
     &    ' Problem opening ',LFIL(1:LNBLNK(LFIL)),'.'
        dok=.true.
        h(1)='The file was expected to exist, but was not found or'
        h(2)='could not be read. Double check the name before trying'
        h(3)='again.  '
        CALL ASKOK(OUTS,'Try again?',OK,dok,3)
        IF(OK) THEN
          GOTO 2
        ELSE
          RETURN
        ENDIF
      ENDIF
      write(currentfile,'(a)') NWKFLNAM(1:lnblnk(NWKFLNAM))

C Read the file header and check for first-line tag.
C << need a common block for vergnf >>
      CALL STRIPC(INUNIT,OUTSTR,99,ND,0,'1st line of gnf',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      IF(OUTSTR(1:18).EQ.'*Graphical_network')THEN
        vergnf=0.0
        if(ND.gt.1)then
          K=18
          CALL EGETWR(OUTSTR,K,vergnf,0.,2.,'-','version',IER)
        endif
        CALL EDISP(IUOUT,' ')    
        WRITE(OUTS,'(3A)') 
     &    ' Opened network graphics file: ',LFIL(1:LNBLNK(LFIL)),'.' 
        CALL USRMSG(OUTS,' ','-') 
      else
        WRITE(OUTS,'(3A)') 'File: ',LFIL(1:LNBLNK(LFIL)), 
     &    ' is not a network graphics file.'
        CALL USRMSG(OUTS,' ','W') 
        ier=1
        return
      endif 
   
C Read in the header lines of the file, look for the date, network
C description and viewing information.
  20  CALL STRIPC(INUNIT,OUTSTR,99,ND,0,'gnf header lines',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','gnf header tags',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then

C Read date stamp << not yet used >>.
        CALL EGETRM(OUTSTR,K,ectime,'W','gnf date stamp',IER)
        IF(IER.NE.0) CONT=.FALSE.
      elseif(WORD(1:4).EQ.'*Doc')then

C Read doncumentation phrase.
        CALL EGETRM(OUTSTR,K,NWKDSC,'W','gnf documentation',IER)
        IF(IER.NE.0) CONT=.FALSE.
      elseif(WORD(1:7).EQ.'*Domain')then

C Read the network type string and index and set NWKTYPSTR.
        WORD2='  '
        CALL EGETW(OUTSTR,K,WORD2,'W','gnf network type string',IER)
        CALL EGETWI(OUTSTR,K,INWKTYP,1,MNWKTYP,'W','gnf type index',IER)
        if(INWKTYP.gt.0)NWKTYPSTR(INWKTYP)=WORD2(1:12)
        IF(IER.NE.0) CONT=.FALSE.
      elseif(WORD(1:8).EQ.'*Icon_db')then
 
C Get the associated icon database.
        CALL EGETRM(OUTSTR,K,ICONDBFL,'W','gnf icon database',IER)
        IF(IER.NE.0) CONT=.FALSE.
      elseif(WORD(1:5).EQ.'*View')then

C Get the view type.
        CALL EGETWI(OUTSTR,K,IVIEW,1,3,'W','gnf view type',IER)      
        IF(IER.GT.0) CONT=.FALSE.
      elseif(WORD(1:7).EQ.'*Centre')then

C Get the view centre.
        CALL EGETWR(OUTSTR,K,VAL,0.0,999.,'W','gnf X cent',IER)
        VIEWCEN(1)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.0,999.,'W','gnf Y cent',IER)
        VIEWCEN(2)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.0,999.,'W','gnf Z cent',IER)
        VIEWCEN(3)=VAL  
        IF(IER.GT.0) CONT=.FALSE.
      elseif(WORD(1:5).EQ.'*Grid')then

C Get the grid on/off state.
        CALL EGETWI(OUTSTR,K,IGRID,1,3,'W','gnf grid on/off',IER)
        GON=.TRUE. 
        IF(IGRID.EQ.0) GON=.FALSE.

C Get the grid spacing.   
        CALL EGETWR(OUTSTR,K,VAL,0.0,999.,'W','gnf X spac',IER)
        GRSPC(1)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.0,999.,'W','gnf Y spac',IER)
        GRSPC(2)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.0,999.,'W','gnf Z spac',IER)
        GRSPC(3)=VAL
        IF(IER.GT.0) CONT=.FALSE.
      elseif(WORD(1:5).EQ.'*Snap')then

C Get the snapping on/off state.
        CALL EGETWI(OUTSTR,K,ISNAP,1,3,'W','gnf snap on/off',IER)
        SON=.TRUE. 
        IF(ISNAP.EQ.0) SON=.FALSE.  
        goto 20
      elseif(WORD(1:17).eq.'*Attribute_global')then

C Get attributes which are global to this domain.
 546    CALL STRIPC(INUNIT,OUTSTR,99,ND,0,'glob attribute',IER)
        IF(IER.NE.0) CONT=.FALSE.
        if(OUTSTR(1:21).eq.'*End_global_attribute')then
          continue
        else
          idgatrdom=idgatrdom+1
          JJ=idgatrdom
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','gnf glob grp tags',IER)
          IF(IER.NE.0) CONT=.FALSE.

C Scan three words, the first is the data type, 2nd is either the
C string `external` or `-` and the 3rd is either a tag to find
C in an external file or `-`
          write(ddgtagatr(JJ,1),'(a)')WORD(1:12)
          CALL EGETW(OUTSTR,K,WORD,'W','glob atrib data type',IER)
          CALL EGETW(OUTSTR,K,WORD1,'W','glob atrib external or -',IER)
          write(ddgtagatr(JJ,3),'(a)')WORD1(1:12)
          CALL EGETW(OUTSTR,K,WORD2,'W','glob atrib ext tag or -',IER)
          write(ddgtagatr(JJ,4),'(a)')WORD2(1:12)
          IF(IER.NE.0) CONT=.FALSE.

C Based on the data type parse the remaining items on the line.
          if(WORD(1:4).eq.'intg'.or.WORD(1:4).eq.'INTG')then
            write(ddgtagatr(JJ,2),'(a)')WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','glob intg value',IER)
            write(ddgatrib(JJ,1),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','glob intg min',IER)
            write(ddgatrib(JJ,2),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','glob intg max',IER)
            write(ddgatrib(JJ,3),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','glob atrib static',IER)
            write(ddgtagatr(JJ,5),'(a)')WORD(1:12)
            CALL EGETRM(OUTSTR,K,PHRASE,'W','glob atrib menu',IER)
            IF(IER.NE.0) CONT=.FALSE.
            write(ddgmenuatr(JJ),'(a)')PHRASE(1:32)
          elseif(WORD(1:4).eq.'real'.or.WORD(1:4).eq.'REAL')then
            write(ddgtagatr(JJ,2),'(a)')WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','glob real value',IER)
            write(ddgatrib(JJ,1),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','glob real min',IER)
            write(ddgatrib(JJ,2),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','glob read max',IER)
            write(ddgatrib(JJ,3),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','glob atrib static',IER)
            write(ddgtagatr(JJ,5),'(a)')WORD(1:12)
            CALL EGETRM(OUTSTR,K,PHRASE,'W','glob atrib menu',IER)
            IF(IER.NE.0) CONT=.FALSE.
            write(ddgmenuatr(JJ),'(a)')PHRASE(1:32)
          elseif(WORD(1:4).eq.'text'.or.WORD(1:4).eq.'TEXT')then
            write(ddgtagatr(JJ,2),'(a)')WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','text value',IER)
            write(ddgatrib(JJ,1),'(a)') WORD(1:12)
            write(ddgatrib(JJ,2),'(a)') '  '
            write(ddgatrib(JJ,3),'(a)') '  '
            CALL EGETW(OUTSTR,K,WORD,'W','glob atrib static',IER)
            write(ddgtagatr(JJ,5),'(a)')WORD(1:12)
            CALL EGETRM(OUTSTR,K,PHRASE,'W','gnf atrib menu',IER)
            IF(IER.NE.0) CONT=.FALSE.
            write(ddgmenuatr(JJ),'(a)')PHRASE(1:32)
          endif
          if(CONT)goto 546
        endif
      elseif(WORD(1:11).EQ.'*End_header')then

C Finished with header section, go on to icons definitions.
        goto 21
      else
        write(outs,'(3a)') 'Unknown gnf tag ',WORD,' continuing...'
        call edisp(iuout,outs)
        goto 20
      endif

C If there were no errors in reading header line then read another.
      if(CONT)then
        goto 20
      else
        call usrmsg('Error reading network graphics file @',outstr,'W')
        ier=1
        CLOSE(INUNIT)
        RETURN
      endif

C Icons description section of gnf file.
  21  CALL STRIPC(INUNIT,OUTSTR,99,ND,0,'gnf icon lines',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','gnf icon tags',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:9).eq.'*Nb_icons')then

C Read in the expected number of icons.
        CALL EGETWI(OUTSTR,K,NNICN,1,99,'W','gnf nb of icons',IER)
        IF(IER.NE.0) CONT=.FALSE. 
      elseif(WORD(1:5).eq.'*Icon')then

C Read gnf file index of this icon and compare it with expected
C order of icons warning user if the order is not as expected.
        INNICN=INNICN+1
        CALL EGETWI(OUTSTR,K,IVAL,1,99,'W','gnf index of icon',IER) 
        IF(IER.NE.0) CONT=.FALSE. 
        I=IVAL
        if(I.ne.INNICN)then
          call usrmsg(
     &      'Ordering of icons is inconsistent (index after *Icon in',
     &      'the file) continuing to read the gnf file...','W')  
        endif  

C Begin counting number of vertices, edges, dots, attachment boxes in this icon.     
        NIVC(I)=0
        NIVE(I)=0
        NCONP(I)=0
        NIVD(I)=0
        NIVA(I)=0
        NIVL(I)=0
        NIVT(I)=0

C Get the (user edited) icon name.
        CALL EGETW(OUTSTR,K,WORD2,'W','gnf icon name',IER)
        IF(IER.NE.0) CONT=.FALSE.
        l2=lnblnk(WORD2)
        NWICNM(I)=WORD2(1:l2)
c        write(*,*) 'name',WORD2(1:l2)
        CALL EGETW(OUTSTR,K,WORD3,'W','gnf icon history',IER)
        IF(IER.NE.0) CONT=.FALSE.
        l3=lnblnk(WORD3)
        NWICNHIS(I)=WORD3(1:l3)
c        write(*,*)'history',WORD3(1:l3)
C Get the selection state.
        CALL EGETWI(OUTSTR,K,IS,0,1,'W','gnf select state',IER) 
c        WRITE(*,*)'selection state',IS
        ISEL(I)=.FALSE.
        IF(IS.GT.0) ISEL(I)=.TRUE.
        IF(IER.NE.0) CONT=.FALSE.
      elseif(WORD(1:5).eq.'*Text')then
  23    CALL STRIPC(INUNIT,OUTSTR,99,ND,0,'gnf icon text',IER)
        IF(IER.NE.0) CONT=.FALSE.
        if(OUTSTR(1:9).eq.'*End_Text')then
          continue
        else
          NIVT(I)=NIVT(I)+1
          JJ=NIVT(I)
          write(NWICNTXT(I,JJ),'(a)') OUTSTR(1:72)
          goto 23
        endif
      elseif(WORD(1:10).eq.'*Attribute')then
 445    CALL STRIPC(INUNIT,OUTSTR,99,ND,0,'gnf attribute',IER)
        IF(IER.NE.0) CONT=.FALSE.
        if(OUTSTR(1:14).eq.'*End_attribute')then
          goto 21
        else
          NICONATR(I)=NICONATR(I)+1
          JJ=NICONATR(I)
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','gnf icon grp tags',IER)
          IF(IER.NE.0) CONT=.FALSE.
          write(ATRTAG(I,JJ,1),'(a)')WORD(1:12)
          CALL EGETW(OUTSTR,K,WORD,'W','gnf atrib data type',IER)
          CALL EGETW(OUTSTR,K,WORD1,'W','gnf atrib external or -',IER)
          write(ATRTAG(I,JJ,3),'(a)')WORD1(1:12)
          CALL EGETW(OUTSTR,K,WORD2,'W','gnf atrib extrn tag or -',IER)
          write(ATRTAG(I,JJ,4),'(a)')WORD2(1:12)
          IF(IER.NE.0) CONT=.FALSE.
          if(WORD(1:4).eq.'intg'.or.WORD(1:4).eq.'INTG')then
            write(ATRTAG(I,JJ,2),'(a)')WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','intg value',IER)
            write(ATRICN(I,JJ,1),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','intg min',IER)
            write(ATRICN(I,JJ,2),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','intg max',IER)
            write(ATRICN(I,JJ,3),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','atrib static',IER)
            write(ATRTAG(I,JJ,5),'(a)')WORD(1:12)
            CALL EGETRM(OUTSTR,K,PHRASE,'W','gnf atrib menu',IER)
            IF(IER.NE.0) CONT=.FALSE.
            write(ATRMENU(I,JJ),'(a)')PHRASE(1:32)
          elseif(WORD(1:4).eq.'real'.or.WORD(1:4).eq.'REAL')then
            write(ATRTAG(I,JJ,2),'(a)')WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','real value',IER)
            write(ATRICN(I,JJ,1),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','real min',IER)
            write(ATRICN(I,JJ,2),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','read max',IER)
            write(ATRICN(I,JJ,3),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','atrib static',IER)
            write(ATRTAG(I,JJ,5),'(a)')WORD(1:12)
            CALL EGETRM(OUTSTR,K,PHRASE,'W','gnf atrib menu',IER)
            IF(IER.NE.0) CONT=.FALSE.
            write(ATRMENU(I,JJ),'(a)')PHRASE(1:32)
          elseif(WORD(1:4).eq.'text'.or.WORD(1:4).eq.'TEXT')then
            write(ATRTAG(I,JJ,2),'(a)')WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','text value',IER)
            write(ATRICN(I,JJ,1),'(a)') WORD(1:12)
            write(ATRICN(I,JJ,2),'(a)') '  '
            write(ATRICN(I,JJ,3),'(a)') '  '
            CALL EGETW(OUTSTR,K,WORD,'W','atrib static',IER)
            write(ATRTAG(I,JJ,5),'(a)')WORD(1:12)
            CALL EGETRM(OUTSTR,K,PHRASE,'W','gnf atrib menu',IER)
            IF(IER.NE.0) CONT=.FALSE.
            write(ATRMENU(I,JJ),'(a)')PHRASE(1:32)
          endif
          if(CONT)goto 445
        endif
      elseif(WORD(1:6).eq.'*Where')then

C Get the icon position.
        CALL EGETWR(OUTSTR,K,VAL,-10.0,999.,'W','gnf IcX cent',IER)
        XYZICON(I,1)=VAL
        CALL EGETWR(OUTSTR,K,VAL,-10.0,999.,'W','gnf IcY cent',IER)
        XYZICON(I,2)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.0,999.,'W','gnf IcZ cent',IER)
        XYZICON(I,3)=VAL  
        IF(IER.NE.0) CONT=.FALSE.
      elseif(WORD(1:7).eq.'*Vertex')then

C Get the vertex co-ordinates.
        NIVC(I)=NIVC(I)+1
        J=NIVC(I)
        CALL EGETWR(OUTSTR,K,VAL,-10.0,999.,'W','gnf vertex X',IER)
        VCICON(I,J,1)=VAL
        CALL EGETWR(OUTSTR,K,VAL,-10.0,999.,'W','gnf vertex Y',IER)
        VCICON(I,J,2)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.0,999.,'W','gnf vertex Z',IER)
        VCICON(I,J,3)=VAL   
        IF(IER.NE.0) CONT=.FALSE.
      elseif(WORD(1:5).eq.'*Edge')then

C Increment number of edges and then get start and end vertex index.
        NIVE(I)=NIVE(I)+1
        JJ=NIVE(I)
        CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf edge start',IER)
        IVEICN(I,JJ,1)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf edge end',IER)
        IVEICN(I,JJ,2)=IVAL
        if(ND.gt.3)then
          CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf edge clr indx',IER)
          IVEICN(I,JJ,3)=IVAL
          CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf edge clr type',IER)
          IVEICN(I,JJ,4)=IVAL
          CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf edge line style',IER)
          IVEICN(I,JJ,5)=IVAL
        else
          IVEICN(I,JJ,3)=6
          IVEICN(I,JJ,4)=0
          IVEICN(I,JJ,5)=1
        endif
        IF(IER.NE.0) CONT=.FALSE.
      elseif(WORD(1:4).eq.'*Dot')then
        NIVD(I)=NIVD(I)+1
        JJ=NIVD(I)
        CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf doc vertex',IER)
        IVDOT(I,JJ,1)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf dot clr indx',IER)
        IVDOT(I,JJ,2)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf dot clr type',IER)
        IVDOT(I,JJ,3)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf dot size',IER)
        IVDOT(I,JJ,4)=IVAL
        IF(IER.NE.0) CONT=.FALSE.
      elseif(WORD(1:4).eq.'*Arc')then
        NIVA(I)=NIVA(I)+1
        JJ=NIVA(I)
        CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf arc cntr vertex',IER)
        IVARC(I,JJ,1)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf arc rad vertex',IER)
        IVARC(I,JJ,2)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,-360,360,'W','gnf arc ang 1',IER)
        IVARC(I,JJ,3)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,-360,360,'W','gnf arc ang 2',IER)
        IVARC(I,JJ,4)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf arc clr indx',IER)
        IVARC(I,JJ,5)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf arc clr type',IER)
        IVARC(I,JJ,6)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf arc fill index',IER)
        IVARC(I,JJ,7)=IVAL
        IF(IER.NE.0) CONT=.FALSE.
      elseif(WORD(1:6).eq.'*Label')then
        NIVL(I)=NIVL(I)+1
        JJ=NIVL(I)
        CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf lbl vertex',IER)
        IVLBL(I,JJ,1)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf lbl clr indx',IER)
        IVLBL(I,JJ,2)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,0,999,'W','gnf lbl clr type',IER)
        IVLBL(I,JJ,3)=IVAL
        CALL EGETW(OUTSTR,K,WORD2,'W','gnf label text',IER)
        IF(IER.NE.0) CONT=.FALSE.
        NWICNLBL(I,JJ)=WORD2(1:4)
      elseif(WORD(1:7).eq.'*Attach')then

C Increment number of attachement boxes and get X and Y position.
        NCONP(I)=NCONP(I)+1
        JJJ=NCONP(I)
        CALL EGETWR(OUTSTR,K,VAL,0.,999.,'W','gnf attach X',IER)
        CONCP(I,JJJ,1)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,999.,'W','gnf attach Y',IER)
        CONCP(I,JJJ,2)=VAL 
        if(ND.gt.2)then
          CALL EGETWI(OUTSTR,K,IVAL,0,9,'W','gnf attach type',IER)
          ICNCT(I,JJJ)=IVAL 
        else
          ICNCT(I,JJJ)=0
        endif
        IF(IER.NE.0) CONT=.FALSE.
      elseif(WORD(1:9).eq.'*End_icon')then

C Finished with an icon, scan for another.
        continue
      elseif(WORD(1:15).eq.'*Nb_connections')then

C Get the number of connections expected, set local counter INICNN
C and then jump to 22.
        CALL EGETWI(OUTSTR,K,NICNN,1,999,'W','gnf connection sets',IER)
        IF(IER.NE.0) CONT=.FALSE.
        if(CONT)then
          INICNN=0
          goto 22 
        endif       
      else
        write(outs,'(3a)') 'Unknown gnf tag ',WORD,' continuing...'
        call edisp(iuout,outs)
        goto 21
      endif

C If there were no errors in reading icons line then read another.
      if(CONT)then
        goto 21
      else
        call usrmsg('Error reading network graphics file @',outstr,'W')
        ier=1
        CLOSE(INUNIT)
        RETURN
      endif

C Connections (links between icons). Expected format is:
C *Conn source icon name, icon index, destination icon name, icon index.
C Begin by reading one line with this data followed by attachment
C points. 
  22  CALL STRIPC(INUNIT,OUTSTR,99,ND,0,'gnf connection lines',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','gnf connection tag',IER)
      if(WORD(1:5).eq.'*Conn')then
        INICNN=INICNN+1
        NCONWP(INICNN)=0  
        CALL EGETW(OUTSTR,K,WORD,'W','gnf conn source icon tag',IER)
        IF(IER.NE.0) CONT=.FALSE.
        CALL EGETWI(OUTSTR,K,IVAL,1,999,'W','gnf connc start index',IER)
        ICNS(INICNN)=IVAL
        CALL EGETW(OUTSTR,K,WORD2,'W','gnf connc end icon tag',IER)
        IF(IER.NE.0) CONT=.FALSE.
        CALL EGETWI(OUTSTR,K,IVAL,1,999,'W','gnf connc end index',IER)
        ICNE(INICNN)=IVAL
        IF(IER.NE.0) CONT=.FALSE.

C Read 2nd line which holds the index of the attachment points (in
C the list of attachements points for the starting and ending icon)
C as well as the type of the connection .
        CALL STRIPC(INUNIT,OUTSTR,99,ND,0,'connection',IER)   
        K=0  
        CALL EGETWI(OUTSTR,K,IVAL,1,MCNP,'W','gnf attach start ndx',IER)
        ICNSP(INICNN)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,1,MCNP,'W','gnf attach end index',IER)
        ICNEP(INICNN)=IVAL
        if(ND.gt.2)then
          CALL EGETWI(OUTSTR,K,IVAL,0,9,'W','gnf attach type',IER)
          ICNNT(INICNN)=IVAL
        else
          ICNNT(INICNN)=0
        endif
        IF(IER.NE.0) CONT=.FALSE.
      elseif(WORD(1:14).eq.'*Attribute_cnn')then
 446    CALL STRIPC(INUNIT,OUTSTR,99,ND,0,'cnn attribute',IER)
        IF(IER.NE.0) CONT=.FALSE.
        if(OUTSTR(1:18).eq.'*End_cnn_attribute')then
          continue
        else
          idatrdom(INICNN)=idatrdom(INICNN)+1
          JJ=idatrdom(INICNN)
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','gnf cnn grp tags',IER)
          IF(IER.NE.0) CONT=.FALSE.

C Scan three words, the first is the data type, 2nd is either the
C string `external` or `-` and the 3rd is either a tag to find
C in an external file or `-`
          write(ddtagatr(INICNN,JJ,1),'(a)')WORD(1:12)
          CALL EGETW(OUTSTR,K,WORD,'W','gnf atrib data type',IER)
          CALL EGETW(OUTSTR,K,WORD1,'W','gnf atrib external or -',IER)
          write(ddtagatr(INICNN,JJ,3),'(a)')WORD1(1:12)
          CALL EGETW(OUTSTR,K,WORD2,'W','gnf atrib ext tag or -',IER)
          write(ddtagatr(INICNN,JJ,4),'(a)')WORD2(1:12)
          IF(IER.NE.0) CONT=.FALSE.

C Based on the data type parse the remaining items on the line.
          if(WORD(1:4).eq.'intg'.or.WORD(1:4).eq.'INTG')then
            write(ddtagatr(INICNN,JJ,2),'(a)')WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','intg value',IER)
            write(ddatrib(INICNN,JJ,1),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','intg min',IER)
            write(ddatrib(INICNN,JJ,2),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','intg max',IER)
            write(ddatrib(INICNN,JJ,3),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','atrib static',IER)
            write(ddtagatr(INICNN,JJ,5),'(a)')WORD(1:12)
            CALL EGETRM(OUTSTR,K,PHRASE,'W','gnf atrib menu',IER)
            IF(IER.NE.0) CONT=.FALSE.
            write(ddmenuatr(INICNN,JJ),'(a)')PHRASE(1:32)
          elseif(WORD(1:4).eq.'real'.or.WORD(1:4).eq.'REAL')then
            write(ddtagatr(INICNN,JJ,2),'(a)')WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','real value',IER)
            write(ddatrib(INICNN,JJ,1),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','real min',IER)
            write(ddatrib(INICNN,JJ,2),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','read max',IER)
            write(ddatrib(INICNN,JJ,3),'(a)') WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','atrib static',IER)
            write(ddtagatr(INICNN,JJ,5),'(a)')WORD(1:12)
            CALL EGETRM(OUTSTR,K,PHRASE,'W','gnf atrib menu',IER)
            IF(IER.NE.0) CONT=.FALSE.
            write(ddmenuatr(INICNN,JJ),'(a)')PHRASE(1:32)
          elseif(WORD(1:4).eq.'text'.or.WORD(1:4).eq.'TEXT')then
            write(ddtagatr(INICNN,JJ,2),'(a)')WORD(1:12)
            CALL EGETW(OUTSTR,K,WORD,'W','text value',IER)
            write(ddatrib(INICNN,JJ,1),'(a)') WORD(1:12)
            write(ddatrib(INICNN,JJ,2),'(a)') '  '
            write(ddatrib(INICNN,JJ,3),'(a)') '  '
            CALL EGETW(OUTSTR,K,WORD,'W','atrib static',IER)
            write(ddtagatr(INICNN,JJ,5),'(a)')WORD(1:12)
            CALL EGETRM(OUTSTR,K,PHRASE,'W','gnf atrib menu',IER)
            IF(IER.NE.0) CONT=.FALSE.
            write(ddmenuatr(INICNN,JJ),'(a)')PHRASE(1:32)
          endif
          if(CONT)goto 446
        endif
      elseif(WORD(1:9).eq.'*Waypoint')then

C Now read *Waypoint to get XYZ coords.
        NCONWP(INICNN)=NCONWP(INICNN)+1  
        JJJJ=NCONWP(INICNN)
        CALL EGETWR(OUTSTR,K,VAL,0.0,999.,'W','gnf waypoint X',IER)
        CNWNP(INICNN,JJJJ,1)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.0,999.,'W','gnf waypoint Y',IER)
        CNWNP(INICNN,JJJJ,2)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.0,999.,'W','gnf waypoint Z',IER)
        CNWNP(INICNN,JJJJ,3)=VAL
        IF(IER.GT.0) CONT=.FALSE.
      elseif(WORD(1:16).eq.'*End_connections')then
        if(INICNN.ne.NICNN)then
          call usrmsg('Expected number of icon connections not found',
     &      'please check the network!','W')
        endif
        continue
      elseif(WORD(1:12).eq.'*End_network')then

C Reached file end, close it.
        CLOSE(INUNIT)
        RETURN 
      endif

C Check if read continues, if error return.
      IF(CONT) GOTO 22

      CALL EDISP(IUOUT,' Error reading the network graphics file.')
      CLOSE(INUNIT)
      RETURN

      END


C ******************************** NETTOFLW *********************************
C Convert from graphic network file to network flow common blocks.
      SUBROUTINE NETTOFLW(IER)

#include "building.h"
#include "gnetwk.h"
#include "net_flow.h"
      
      integer lnblnk  ! function definition

C Graphics and ESP-r default commons
      COMMON/OUTIN/IUOUT,IUIN

C Icon commons
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)

      COMMON/NWKICS/NWICNM(MICN),NWICNHIS(MICN),NWICNLBL(MICN,MICND),
     & NWICNTXT(MICN,60)

C Connection commons
      COMMON/NWKCON/NICNN,ICNS(MNCNN),ICNE(MNCNN),ICNNT(MNCNN),
     & ICNSP(MNCNN),ICNEP(MNCNN),CNWNP(MNCNN,MCIP,3),
     & NCONWP(MNCNN),idatrdom(MNCNN),ddtagatr(MNCNN,MIATRB,5),
     & ddatrib(MNCNN,MIATRB,3),ddmenuatr(MNCNN,MIATRB)

C Global attributes commons (associated with igatrdom(),dgtagatr(),
C     &  dgatrib(),dgmenuatr() passed back via scanicondb call.
      COMMON/NWKGLOB/idgatrdom,ddgtagatr(MIATRB,5),
     & ddgatrib(MIATRB,3),ddgmenuatr(MIATRB)

C Grid commons
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKFLNAM,NWKTYPSTR(MNWKTYP)
      COMMON/NWKTYP/INWKTYP,vergnf

C Network flow commons are as follows:
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)

      COMMON/MFLOW6/IVALCM(MCMV)
      COMMON/MFLOW7/LVALCM(MCMV)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)

      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED

C icontoflow() is a pointer from icon to flow node (positive) or flow
C component (negative).
C icnisnode(MNCNN) toggle where .true. means icon index is
C associated with a flow node.
      common/NWKFLW/icontoflow(MNCNN),icnisnode(MNCNN)

C logical icnused(MNCNN) is a toggle for marking halves of icon connections
C which have been combined into one flow icon.
      dimension icnused(MNCNN)

C Height difference attribute at an icon connection.
      dimension deltah(MNCNN)

      CHARACTER LAPROB*72,OUTS*124,OUTSTR*124
      CHARACTER*72 NWKNAM,NWKDSC,NWKFLNAM
      CHARACTER NWICNM*12,NWICNHIS*40,NWICNLBL*4,NWKTYPSTR*12
      character NWICNTXT*72,LVALCM*60
      character ATRTAG*12,ATRMENU*32,ATRICN*12
      character ddtagatr*12,ddmenuatr*32,ddatrib*12
      character ddgtagatr*12,ddgmenuatr*32,ddgatrib*12
      logical foundloc,icnused,icnisnode,close,modify,ok

      DIMENSION NDID(MNOD)
      CHARACTER NDID*12,CMNAM*12,NDNAM*12
      CHARACTER LTPCMP*60,atrerr*72

C It is assumed that a graphical flow network file has already
C been opened and read and the common blocks will now be scanned
C for relevant information for network flow.
      if(INWKTYP.eq.2.and.NWKTYPSTR(INWKTYP)(1:4).eq.'Flow')then
        NNOD=0
        NCMP=0
        NCNN=0
        do 9 i=0,MNOD
          NDNAM(i)=' '
  9     continue
        do 10 i=1,MCMP
          CMNAM(i)=' '
 10     continue
      endif

C In some cases relationships found while creating flow network
C commons requires icon commons to be updated. Use modify to
C signal this and check at the end of the conversion.
      modify=.false.

C Establish the short network flow description for each of the
C components (typically done in EMFREAD).
      call mfcdat

C Loop through global attributes and see if wind pressure reduction
C as been defined. If it has not set WRED.
      if(idgatrdom.gt.0)then
        do 22 idg=1,idgatrdom
          i=idg
          if(ddgtagatr(I,1)(1:4).eq.'flow'.and.
     &       ddgtagatr(I,2)(1:4).eq.'real')then
            read(ddgatrib(I,1),*,IOSTAT=IOS,ERR=1001) WRED
          else
            WRED=0.5
          endif
  22    continue
      endif

C Loop through each of the icons and parse them into a flow
C node or a flow component based on the type attribute of
C the icon.
      do 42 innicn=1,NNICN
        i=innicn
        if(ATRTAG(I,1,1)(1:4).eq.'flow'.and.
     &     ATRTAG(I,1,2)(1:4).eq.'intg')then

C Found an internal node unknown pressure (ATRICN(I,1,1) is zero or
C internal node known pressure (one) or Boundary node known 
C pressure (two). Cast string to integer for index of node type or component type.
          read(ATRICN(I,1,1),*,IOSTAT=IOS,ERR=1001)iv
          if(iv.ge.0.and.iv.le.9)then
            NNOD=NNOD+1
            NDNAM(NNOD)=NWICNM(I)
            NDTYP(NNOD)=iv
            icontoflow(I)=NNOD
            icnisnode(I)=.true.
            write(atrerr,'(6a)') 'atributes=',ATRICN(I,1,1),
     &        ATRICN(I,2,1),ATRICN(I,3,1),ATRICN(I,4,1),ATRICN(I,5,1)
            if(NDTYP(NNOD).eq.0)then
              read(ATRICN(I,2,1),*,IOSTAT=IOS,ERR=1002)NDFLD(NNOD)
              NDID(NNOD)=ATRICN(I,3,1)
              read(ATRICN(I,4,1),*,IOSTAT=IOS,ERR=1002)SUPNOD(NNOD,2)
            elseif(NDTYP(NNOD).eq.1)then
              read(ATRICN(I,2,1),*,IOSTAT=IOS,ERR=1002)NDFLD(NNOD)
              NDID(NNOD)=ATRICN(I,3,1)
              read(ATRICN(I,4,1),*,IOSTAT=IOS,ERR=1002)SUPNOD(NNOD,1)
              read(ATRICN(I,5,1),*,IOSTAT=IOS,ERR=1002)SUPNOD(NNOD,2)
            elseif(NDTYP(NNOD).eq.2)then
              read(ATRICN(I,2,1),*,IOSTAT=IOS,ERR=1002)NDFLD(NNOD)
              NDID(NNOD)=ATRICN(I,4,1)
              read(ATRICN(I,3,1),*,IOSTAT=IOS,ERR=1002)SUPNOD(NNOD,1)
              read(ATRICN(I,4,1),*,IOSTAT=IOS,ERR=1002)SUPNOD(NNOD,2)
            elseif(NDTYP(NNOD).eq.3)then

C Found a Boundary node wind pressure (three). No temperature held
C in the GNF file for this type so set NDID to zero.
              NDID(NNOD)='0.000'
              read(ATRICN(I,2,1),*,IOSTAT=IOS,ERR=1002)NDFLD(NNOD)
              read(ATRICN(I,3,1),*,IOSTAT=IOS,ERR=1002)SUPNOD(NNOD,1)
              read(ATRICN(I,4,1),*,IOSTAT=IOS,ERR=1002)SUPNOD(NNOD,2)
            endif
          elseif(iv.gt.9)then

C Flow component, set number of supplemental items depending on ITPCMP.
C save pointer from icon to flow component as negative index.
            NCMP=NCMP+1
            CMNAM(NCMP)=NWICNM(I)
            ITPCMP(NCMP)=iv

C Find matching network flow component text descriptor and remember it.
            IC=0
   31       IC=IC+1
            IF(ITPCMP(NCMP).EQ.IVALCM(IC)) GOTO 45
            IF(IC.LT.MCMV) GOTO 31
   45       LTPCMP(NCMP)=LVALCM(IC)

            icontoflow(I)=NCMP*(-1)
            icnisnode(I)=.false.
            if(ITPCMP(NCMP).eq.10)then
              ISDCMP(NCMP)=3
            elseif(ITPCMP(NCMP).eq.15)then
              ISDCMP(NCMP)=3
            elseif(ITPCMP(NCMP).eq.17)then
              ISDCMP(NCMP)=3
            elseif(ITPCMP(NCMP).eq.20)then
              ISDCMP(NCMP)=3
            elseif(ITPCMP(NCMP).eq.25)then
              ISDCMP(NCMP)=3
            elseif(ITPCMP(NCMP).eq.30)then
              ISDCMP(NCMP)=2
            elseif(ITPCMP(NCMP).eq.35)then
              ISDCMP(NCMP)=2
            elseif(ITPCMP(NCMP).eq.40)then
              ISDCMP(NCMP)=3
            elseif(ITPCMP(NCMP).eq.50)then
              ISDCMP(NCMP)=3
            elseif(ITPCMP(NCMP).eq.110)then
              ISDCMP(NCMP)=2
            elseif(ITPCMP(NCMP).eq.120)then
              ISDCMP(NCMP)=3
            elseif(ITPCMP(NCMP).eq.130)then
              ISDCMP(NCMP)=5
            elseif(ITPCMP(NCMP).eq.210)then
              ISDCMP(NCMP)=6
            elseif(ITPCMP(NCMP).eq.220)then
              ISDCMP(NCMP)=13
            elseif(ITPCMP(NCMP).eq.230)then
              ISDCMP(NCMP)=13
            elseif(ITPCMP(NCMP).eq.240)then
              ISDCMP(NCMP)=17
            elseif(ITPCMP(NCMP).eq.250)then
              ISDCMP(NCMP)=17
            elseif(ITPCMP(NCMP).eq.310)then
              ISDCMP(NCMP)=7
            elseif(ITPCMP(NCMP).eq.410)then
              ISDCMP(NCMP)=7
            elseif(ITPCMP(NCMP).eq.420)then
              ISDCMP(NCMP)=6
            endif
            do 62 ijk=1,ISDCMP(NCMP)
              read(ATRICN(I,ijk+1,1),*,IOSTAT=IOS,ERR=1001)
     &             SUPCMP(NCMP,ijk)
  62        continue
          endif
        endif

C Now look for the first location attribute which is the height...
C << sort out what to do with height of a component.
        foundloc=.false.
        do 61 ijj=1,NICONATR(I)
          if(ATRTAG(I,ijj,1)(1:8).eq.'location')then
            if(.NOT.foundloc)then
              foundloc=.true.
              if(ATRTAG(I,ijj,2)(1:4).eq.'real')then
                read(ATRICN(I,1,1),*,IOSTAT=IOS,ERR=1001)icp
                if(icp.le.9)then

C Assign icon Z value if different to hnod().
                  read(ATRICN(I,ijj,1),*,IOSTAT=IOS,ERR=1001)
     &                 HNOD(NNOD)
                  call eclose(HNOD(NNOD),XYZICON(I,3),0.01,close)
                  if(.NOT.close)then
                    XYZICON(I,3)=HNOD(NNOD)
                    modify=.true.
                  endif
                else

C Component height assign icon Z value here if different from
C icon Z value
                  read(ATRICN(I,ijj,1),*,IOSTAT=IOS,ERR=1001) HVAL
                  call eclose(HVAL,XYZICON(I,3),0.01,close)
                  if(.NOT.close)then
                    XYZICON(I,3)=HVAL
                    modify=.true.
                  endif
                endif
              elseif(ATRTAG(I,ijj,2)(1:4).eq.'intg')then
C << ? >>
              elseif(ATRTAG(I,ijj,2)(1:4).eq.'text')then
C << ? >>
              endif
            else

C The 2nd location attribute for internal unknown nodes is linked zone name.
              if(ATRTAG(I,ijj,2)(1:4).eq.'real')then
              elseif(ATRTAG(I,ijj,2)(1:4).eq.'intg')then
              elseif(ATRTAG(I,ijj,2)(1:4).eq.'text')then
                read(ATRICN(I,1,1),*,IOSTAT=IOS,ERR=1001)icp
                if(icp.le.9)then
C << this should be a zone name or `nozone`, should be checked
C << against the list of zone names in the current model.
C << this depends on being able to read in a model synopsis
C << generated by the project manager prior to starting the net module.
                endif
              endif
            endif
          endif
  61    continue
  42  continue

C At this point, if this this a flow network all of the nodes and
C components will be known. Loop through each of the nodes and figure
C out its temperature (same logic as in emfnetw.F)
      DO 18 I=1,NNOD
        ITND(I)=0
        DO 17 J=1,NNOD

C See if this node name has already been defined.
          IF(NDID(I)(1:12).EQ.NDNAM(J)(1:12)) THEN
            ITND(I)=J
          ENDIF
  17    CONTINUE

C No, it is not. Now interpret what we have as a number.
        IF(ITND(I).EQ.0) THEN
          ITND(I)=0
          read(NDID(I),*,IOSTAT=IOS,ERR=1001)TNOD(I)
        ENDIF
  18  CONTINUE

C Clear working data for connections. logical icnused(MNCNN) is
C a toggle for marking halves of icon connections which have
C been combined into one flow icon. icntoflow(MNCNN) identifies
C which flow connection is associated with each icon connection.
      do 143 inicnn=1,NICNN
        icnused(inicnn)=.false.
        deltah(inicnn)=0.0
  143 continue

C The number of connections expected is NICNN. Note, only the
C index of the icon at the start ICNS(INICNN) and end ICNE(INICNN)
C was saved to common. Recover whether the icons are flow nodes
C or flow components and the difference in height attribute.
      do 43 inicnn=1,NICNN

C Connections (links between icons). Expected format is:
C *Conn source icon name, icon index, destination icon name, icon index.
C Decode start icon. Set `is` as icon index at start and `ie` as the
C icon index at the end point.
        is=ICNS(INICNN)
        ie=ICNE(INICNN)

C Look for the first location attribute which is the height...
        foundloc=.false.
        do 63 ijj=1,idatrdom(inicnn)
          if(ddtagatr(inicnn,ijj,1)(1:8).eq.'location')then
            if(.NOT.foundloc)then
              foundloc=.true.
              if(ddtagatr(inicnn,ijj,2)(1:4).eq.'real')then
                read(ddatrib(inicnn,ijj,1),*,IOSTAT=IOS,ERR=1001)
     &          deltah(inicnn)
              endif
            endif
          endif
  63    continue

C Check that each icon connection links one flow node to one
C flow connection.
        if(icnisnode(is).and.icnisnode(ie))then
          write(outs,'(5a)') 'Icons ',NWICNM(is),' & ',NWICNM(ie),
     &    ' are both flow nodes.'
          call usrmsg(outs,'and cannot be directly connected.','W')
        elseif((.NOT.icnisnode(is)).and.(.NOT.icnisnode(ie)))then
          write(outs,'(5a)') 'Icons ',NWICNM(is),' & ',NWICNM(ie),
     &    ' are both flow components and cannot be directly connected.'
          call usrmsg(outs,'and cannot be directly connected.','W')
        endif
  43  continue

C Debug.
C      write(6,*) (deltah(j),j=1,20)

C Now revisit each icon connection, identify component end and the
C search for another icon connection with that index.
      do 144 icheck=1,NICNN

C If icon connection icheck has already been used skip it. If the
C start icon is a node use the end icon to seach against.
        if(.NOT.icnused(icheck))then
          if(icnisnode(ICNS(icheck)))then
            itest=ICNE(icheck)
            iotest=ICNS(icheck)
          else

C This icon connection starts with a component and thus it cannot
C be the start of a flow connection. Skip on to the next icheck
            itest=ICNS(icheck)
            iotest=ICNE(icheck)

C Debug.
C             write(6,*) 'Skipping ',icheck,' icon connection begins ',
C     &         'with an icon component ',itest

             goto 144
          endif

C Loop through all icons and see if there is another icon connection
C which has a start or end point which matches this one and has not
C already been used.
          do 145 ijj=1,NICNN
            if(ijj.ne.icheck)then
              if(.NOT.icnused(ijj))then
                if(ICNS(ijj).eq.itest)then

C We have a flow connection based the start point of the other icon
C connection. Instanciate the common blocks. Find the `node` in the
C other icon connection. Note: assign delta height from icheck and
C from ijj.
                  icnused(ijj)=.true.
                  ncnn=ncnn+1
                  HGTPS(ncnn)=deltah(icheck)
                  if(icnisnode(itest))then

C The current node of the icon connection uses itest and the delta
C height uses icheck
                    nodps(ncnn)=icontoflow(itest)
                  else
                    nodps(ncnn)=icontoflow(iotest)
                  endif

C Now check the other icon connection, and depending on which icon
C in that connection is the node assign the commons.
                  HGTNE(ncnn)=deltah(ijj)
                  if(icnisnode(ICNS(ijj)))then
                    nodne(ncnn)=icontoflow(ICNS(ijj))
                    ITPCON(ncnn)=ABS(icontoflow(ICNE(ijj)))
                    NDSCNN(ncnn,1)=0
                    NDSCNN(ncnn,2)=0
                  else
                    nodne(ncnn)=icontoflow(ICNE(ijj))
                    ITPCON(ncnn)=ABS(icontoflow(ICNS(ijj)))
                    NDSCNN(ncnn,1)=0
                    NDSCNN(ncnn,2)=0
                  endif
                elseif(ICNE(ijj).eq.itest)then

C We have a flow connection. Instanciate the common blocks.
                  icnused(ijj)=.true.
                  ncnn=ncnn+1
                  HGTPS(ncnn)=deltah(icheck)
                  if(icnisnode(itest))then

C The current node of the icon connection uses itest.
                    nodps(ncnn)=icontoflow(itest)
                  else
                    nodps(ncnn)=icontoflow(iotest)
                  endif

C Now check the other icon connection, and depending on which icon
C in that connection is the node assign the commons.
                  HGTNE(ncnn)=deltah(ijj)
                  if(icnisnode(ICNE(ijj)))then
                    nodne(ncnn)=icontoflow(ICNE(ijj))
                    ITPCON(ncnn)=ABS(icontoflow(ICNS(ijj)))
                    NDSCNN(ncnn,1)=0
                    NDSCNN(ncnn,2)=0
                  else
                    nodne(ncnn)=icontoflow(ICNS(ijj))
                    ITPCON(ncnn)=ABS(icontoflow(ICNE(ijj)))
                    NDSCNN(ncnn,1)=0
                    NDSCNN(ncnn,2)=0
                  endif
                endif
              endif
            endif
  145     continue
        endif
  144 continue

C If graphic network file should be updated do it here.
C << there is something which multiplies the number of icon
C << attributes when the graphic network is written out again
C << disable this until we sort out what is happening.
C      if(modify)then
C        dok=.true.
C        h(1)='text to be added'
C        call askok(
C     &    'Heights of icons should reflect node and component',
C     &    'height attributes. Update graphic network file?',ok,dok,0)
C        if(ok)then
C          IM=1
C          CALL NETWRITE(IM)
C        endif
C      endif
      return

C Internal read errors.
 1001 if(IOS.eq.2)then
        WRITE(OUTSTR,'(a,i3)') 
     &  ' Permissions during conversion temp/node value @ icon ',I
      else
        WRITE(OUTSTR,'(a,i3)') 
     &  ' Failed: conversion of temperature/node value @ icon ',I
      endif
      CALL EDISP(iuout,OUTSTR)
      ier=1
      RETURN

C Internal read errors.
 1002 if(IOS.eq.2)then
        WRITE(OUTSTR,'(3a,i3)') 
     &  ' Permissions: icon atribute ',atrerr(1:lnblnk(atrerr)),
     &  ' @ icon ',I
      else
        WRITE(OUTSTR,'(3a,i3)') 
     &  ' Failed: icon atribute ',atrerr(1:lnblnk(atrerr)),' @ icon ',I
      endif
      CALL EDISP(iuout,OUTSTR)
      ier=1
      RETURN

      END


C ******************************** NETWRITE ***************************************
C This routine writes out a network graphics file for use with ESP-r's Network
C tool. The routine can be called using two modes: 
C 1 - save current file in the current location with the current filename (if any).
C 2 - save the current file with a specified location and name.
      SUBROUTINE NETWRITE(IMODE)
#include "gnetwk.h"
      
      integer lnblnk  ! function definition

C Graphics and ESP-r default commons
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)

C Icon commons
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)

      COMMON/NWKICS/NWICNM(MICN),NWICNHIS(MICN),NWICNLBL(MICN,MICND),
     & NWICNTXT(MICN,60) 

C Connection commons
      COMMON/NWKCON/NICNN,ICNS(MNCNN),ICNE(MNCNN),ICNNT(MNCNN),
     & ICNSP(MNCNN),ICNEP(MNCNN),CNWNP(MNCNN,MCIP,3),
     & NCONWP(MNCNN),idatrdom(MNCNN),ddtagatr(MNCNN,MIATRB,5),
     & ddatrib(MNCNN,MIATRB,3),ddmenuatr(MNCNN,MIATRB)

C Global attributes commons (associated with igatrdom(),dgtagatr(),
C     &  dgatrib(),dgmenuatr() passed back via scanicondb call.
      COMMON/NWKGLOB/idgatrdom,ddgtagatr(MIATRB,5),
     & ddgatrib(MIATRB,3),ddgmenuatr(MIATRB)

C Grid commons
      COMMON/NWKGRD/GRMAX(3),GRSPC(3),GRLYRH(MLYRS)
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKFLNAM,NWKTYPSTR(MNWKTYP)
      COMMON/NWKTYP/INWKTYP,vergnf
      COMMON/NWKGRDL/GON,SON
      COMMON/ICONDBNAM/ICONDBFL

      CHARACTER*72 NWKNAM,NWKDSC,NWKFLNAM,ICONDBFL,DFILE,LFIL
      CHARACTER NWICNM*12,NWKTYPSTR*12,NWICNHIS*40
      CHARACTER TFILE*72,OUTS*124,tab*1,dstmp*24,ts12*12,te12*12
      character NWICNLBL*4,NWICNTXT*72,longtfile*144
      character ATRTAG*12,ATRMENU*32,ATRICN*12
      character ddtagatr*12,ddmenuatr*32,ddatrib*12
      character ddgtagatr*12,ddgmenuatr*32,ddgatrib*12
      LOGICAL ISEL,CSEL,CONCAT,GON,SON

C << CSEL, ICONTP() not written out >>

C Open the file to be written, if 'save-as' offer existing file
C name for editing.
      tab=','
      DFILE='net_graphics_file.gnf'
      IF(IMODE.EQ.1.AND.NWKFLNAM(1:3).NE.'UNK')THEN
        LFIL=NWKFLNAM(1:LNBLNK(NWKFLNAM))    
      ELSE
C        LFIL=DFILE(1:LNBLNK(DFILE))
        LFIL=NWKFLNAM(1:LNBLNK(NWKFLNAM))
        CALL EASKS(LFIL,' ',' Graphical network filename ?',72,
     &  DFILE,' elec file',IER,8)
        IF(LFIL(1:2).NE.'  ') NWKFLNAM=LFIL
      ENDIF

C Add the networks path to the filename if required.
C << Why call addpath at this point? >>
      LONGTFILE=' '
      CALL ADDPATH(LFIL,LONGTFILE,CONCAT)
      IF(CONCAT) LFIL=LONGTFILE(1:72)


C Open the file 
      INUNIT=IFIL+1
      CALL EFOPSEQ(INUNIT,NWKFLNAM,4,IER)
      IF(IER.NE.0) THEN
        WRITE(OUTS,'(A,A,A)') ' Problem opening ',
     &  NWKFLNAM(1:LNBLNK(NWKFLNAM)),' aborting write.'
        CALL EDISP(IUOUT,OUTS)
        RETURN
      ENDIF
   
C Write out the general network data and current view parameters
C << Need a common block to hold version number >>
        vergnf=1.2
      WRITE(INUNIT,'(A,f4.1)',iostat=ios,err=1)'*Graphical_network',
     &  vergnf
      call dstamp(dstmp)
      write(INUNIT,'(3a)',iostat=ios,err=1) '*Date',tab,dstmp

C Write out the network description
      write(INUNIT,'(3a)',iostat=ios,err=1) '*Doc',tab,
     &  NWKDSC(1:LNBLNK(NWKDSC))

C Write out the network type
      write(INUNIT,'(4a,i1,a)',iostat=ios,err=1) '*Domain',tab,
     &  NWKTYPSTR(INWKTYP)(1:LNBLNK(NWKTYPSTR(INWKTYP))),tab,INWKTYP,
     &  ' # domain for this network'

C Write the location of the icon database used with this file
C <<can be moved later if required>>
      write(INUNIT,'(3a)',iostat=ios,err=1) '*Icon_db',tab,
     &  ICONDBFL(1:LNBLNK(ICONDBFL))   

C Write out the view parameters
      if(IVIEW.eq.1)then
        write(INUNIT,'(2a,i1,a)',iostat=ios,err=1) '*View',tab,IVIEW,
     &    ' # XY view '      
      elseif(IVIEW.eq.2)then
        write(INUNIT,'(2a,i1,a)',iostat=ios,err=1) '*View',tab,IVIEW,
     &    ' # XZ view  '      
      elseif(IVIEW.eq.3)then
        write(INUNIT,'(2a,i1,a)',iostat=ios,err=1) '*View',tab,IVIEW,
     &    ' # YZ view '      
      endif
      write(INUNIT,'(2a,F8.4,a,F8.4,a,F8.4,a)',iostat=ios,err=1) 
     &  '*Centre',tab,VIEWCEN(1),tab,VIEWCEN(2),tab,VIEWCEN(3),
     &  ' # centre of view'

C Current network state
      IG=0
      IF(GON)IG=1
      if(IG.eq.0)then
        write(INUNIT,'(2a,i1,a,F8.4,a,F8.4,a,F8.4,a)',iostat=ios,err=1) 
     &    '*Grid',tab,IG,tab,GRSPC(1),tab,GRSPC(2),tab,GRSPC(3),
     &    ' # grid off & XYZ spacing '
      elseif(IG.eq.1)then
        write(INUNIT,'(2a,i1,a,F8.4,a,F8.4,a,F8.4,a)',iostat=ios,err=1) 
     &    '*Grid',tab,IG,tab,GRSPC(1),tab,GRSPC(2),tab,GRSPC(3),
     &    ' # grid on & XYZ spacing '
      endif
      IS=0
      IF(SON)IS=1
      if(IS.eq.0)then
        write(INUNIT,'(2a,i1,a)',iostat=ios,err=1) '*Snap',tab,IS,
     &    ' # Snap off '      
      elseif(IS.eq.1)then
        write(INUNIT,'(2a,i1,a)',iostat=ios,err=1) '*Snap',tab,IS,
     &    ' # Snap on '   
      endif 

C Write out any global attributes for this domain.
      if(idgatrdom.gt.0)then
        write(INUNIT,'(2a,i2)',iostat=ios,err=1) '*Attribute_global',
     &    tab,idgatrdom
        do 212 J=1,idgatrdom

C Write out global attributes with minimal white space.
          l3=lnblnk(ddgtagatr(J,1))
          l4=lnblnk(ddgtagatr(J,2))
          l5=lnblnk(ddgtagatr(J,3))
          l5a=lnblnk(ddgtagatr(J,4))
          l5b=lnblnk(ddgtagatr(J,5))
          l6=lnblnk(ddgmenuatr(J))
          l7=MAX0(lnblnk(ddgatrib(J,1)),1)
          l8=MAX0(lnblnk(ddgatrib(J,2)),1)
          l9=MAX0(lnblnk(ddgatrib(J,3)),1)
          if(ddgtagatr(J,2)(1:4).eq.'real'.or.
     &       ddgtagatr(J,2)(1:4).eq.'intg')then
            write(INUNIT,'(17a)') ddgtagatr(J,1)(1:l3),tab,
     &        ddgtagatr(J,2)(1:l4),tab,ddgtagatr(J,3)(1:l5),tab,
     &        ddgtagatr(J,4)(1:l5a),tab,ddgatrib(J,1)(1:l7),tab,      
     &        ddgatrib(J,2)(1:l8),tab,ddgatrib(J,3)(1:l9),tab,
     &        ddgtagatr(J,5)(1:l5b),tab,ddgmenuatr(J)(1:l6)
          elseif(ddgtagatr(J,2)(1:4).eq.'text')then
            write(INUNIT,'(13a)') ddgtagatr(J,1)(1:l3),tab,
     &        ddgtagatr(J,2)(1:l4),tab,ddgtagatr(J,3)(1:l5),tab,
     &        ddgtagatr(J,4)(1:l5a),tab,ddgatrib(J,1)(1:l7),tab,      
     &        ddgtagatr(J,5)(1:l5b),tab,ddgmenuatr(J)(1:l6)
          endif
 212    continue
        write(INUNIT,'(a)',iostat=ios,err=1) '*End_global_attribute'
      endif
      WRITE(INUNIT,'(A)',iostat=ios,err=1)'*End_header'   
      WRITE(INUNIT,'(A)',iostat=ios,err=1)'# '

C Write out the current icon data: tags and historical context first,
C then any documentation, then attributes, then vertices, then
C graphic entities (edges, dots, internal labels, arcs etc.)
       write(INUNIT,'(2a,i3,a)',iostat=ios,err=1) '*Nb_icons',tab,
     &    NNICN,' # Number of icons '      
      DO 10 I=1,NNICN  
        ISL=0
        IF(ISEL(I))ISL=1
        WRITE(INUNIT,'(A)')'# '
        l2=lnblnk(NWICNM(I))
        l3=lnblnk(NWICNHIS(I))
        if(ISL.eq.0)then
          write(INUNIT,'(2a,i2,5a,i1,a)',iostat=ios,err=1) '*Icon',tab,
     &      I,tab,NWICNM(I)(1:l2),tab,NWICNHIS(I)(1:l3),tab,ISL,
     &      ' # name (user & icon db) & state (normal)'
        elseif(ISL.eq.1)then
          write(INUNIT,'(2a,i2,5a,i1,a)',iostat=ios,err=1) '*Icon',tab,
     &      I,tab,NWICNM(I)(1:l2),tab,NWICNHIS(I)(1:l3),tab,ISL,
     &      ' # name (user & icon db) & state (selected)'
        endif 
        if(NIVT(I).ge.1)then
          write(INUNIT,'(a)',iostat=ios,err=1) '*Text'
          do 11 J=1,NIVT(I)
            l3=lnblnk(NWICNTXT(I,J))
            write(INUNIT,'(a)',iostat=ios,err=1) NWICNTXT(I,J)(1:l3)
  11      continue
          write(INUNIT,'(a)',iostat=ios,err=1) '*End_Text'
        endif 
        write(INUNIT,'(2a,i2)',iostat=ios,err=1) '*Attribute',
     &    tab,NICONATR(I)
        if(NICONATR(I).gt.0)then
          do 211 J=1,NICONATR(I)

C Write out attributes with minimal white space.
            l3=lnblnk(ATRTAG(I,J,1))
            l4=lnblnk(ATRTAG(I,J,2))
            l5=lnblnk(ATRTAG(I,J,3))
            l5a=lnblnk(ATRTAG(I,J,4))
            l5b=lnblnk(ATRTAG(I,J,5))
            l6=lnblnk(ATRMENU(I,J))
            l7=MAX0(lnblnk(ATRICN(I,J,1)),1)
            l8=MAX0(lnblnk(ATRICN(I,J,2)),1)
            l9=MAX0(lnblnk(ATRICN(I,J,3)),1)
            if(ATRTAG(I,J,2)(1:4).eq.'real'.or.
     &         ATRTAG(I,J,2)(1:4).eq.'intg')then
              write(INUNIT,'(17a)') ATRTAG(I,J,1)(1:l3),tab,
     &          ATRTAG(I,J,2)(1:l4),tab,ATRTAG(I,J,3)(1:l5),tab,
     &          ATRTAG(I,J,4)(1:l5a),tab,ATRICN(I,J,1)(1:l7),tab,      
     &          ATRICN(I,J,2)(1:l8),tab,ATRICN(I,J,3)(1:l9),tab,
     &          ATRTAG(I,J,5)(1:l5b),tab,ATRMENU(I,J)(1:l6)
            elseif(ATRTAG(I,J,2)(1:4).eq.'text')then
              write(INUNIT,'(13a)') ATRTAG(I,J,1)(1:l3),tab,
     &          ATRTAG(I,J,2)(1:l4),tab,ATRTAG(I,J,3)(1:l5),tab,
     &          ATRTAG(I,J,4)(1:l5a),tab,ATRICN(I,J,1)(1:l7),tab,      
     &          ATRTAG(I,J,5)(1:l5b),tab,ATRMENU(I,J)(1:l6)
            endif
 211      continue
        endif
        write(INUNIT,'(a)',iostat=ios,err=1) '*End_attribute'
        write(INUNIT,'(2a,F8.4,a,F8.4,a,F8.4,a)',iostat=ios,err=1) 
     &    '*Where',tab,XYZICON(I,1),tab,XYZICON(I,2),tab,XYZICON(I,3),
     &    ' # Icon position XYZ:'
        DO 12 J=1,NIVC(I) 
          WRITE(INUNIT,'(2a,F8.4,a,F8.4,a,F8.4,a)',iostat=ios,err=1) 
     &      '*Vertex',tab,VCICON(I,J,1),tab,VCICON(I,J,2),tab,
     &      VCICON(I,J,3),' # XYZ '     
  12    CONTINUE
        if(NIVE(I).ge.1)then
          WRITE(INUNIT,'(A)',iostat=ios,err=1)
     &      '# Edge start & end, colour index, colour type & line type' 
          DO 14 J=1,NIVE(I) 
            WRITE(INUNIT,'(2a,i2,a,i2,a,i2,a,i2,a,i2)') '*Edge',tab,
     &        IVEICN(I,J,1),tab,IVEICN(I,J,2),tab,IVEICN(I,J,3),tab,
     &        IVEICN(I,J,4),tab,IVEICN(I,J,5)
  14      CONTINUE
        endif
        if(NIVD(I).ge.1)then
          WRITE(INUNIT,'(A)',iostat=ios,err=1)
     &      '# Dot vertex index, colour index, colour type & size' 
          DO 15 J=1,NIVD(I) 
            WRITE(INUNIT,'(2a,i2,a,i2,a,i2,a,i2)') '*Dot',tab,
     &        IVDOT(I,J,1),tab,IVDOT(I,J,2),tab,IVDOT(I,J,3),tab,
     &        IVDOT(I,J,4)
  15      CONTINUE
        endif
        if(NIVA(I).ge.1)then
          WRITE(INUNIT,'(A)',iostat=ios,err=1)
     &    '# Arc centre&radius vert, angles, clr indx, clr type & size' 
          DO 18 J=1,NIVA(I) 
            WRITE(INUNIT,'(2a,i2,a,i2,a,i4,a,i4,a,i2,a,i2,a,i2)') 
     &       '*Arc',tab,IVARC(I,J,1),tab,IVARC(I,J,2),tab,IVARC(I,J,3),
     &       tab,IVARC(I,J,4),tab,IVARC(I,J,5),tab,IVARC(I,J,6),tab,
     &       IVARC(I,J,7)
  18      CONTINUE
        endif
        if(NIVL(I).ge.1)then
          WRITE(INUNIT,'(A)',iostat=ios,err=1)
     &      '# Label vertex index, colour index, colour type & the text' 
          DO 16 J=1,NIVL(I) 
            WRITE(INUNIT,'(2a,i2,a,i2,a,i2,2a)') '*Label',tab,
     &        IVLBL(I,J,1),tab,IVLBL(I,J,2),tab,IVLBL(I,J,3),tab,
     &        NWICNLBL(I,J)
  16      CONTINUE
        endif
        WRITE(INUNIT,'(A)')'# Icon attachment boxes:'
        DO 17 J=1,NCONP(I)
          WRITE(INUNIT,'(2a,F8.4,a,F8.4,a,i2,a)',iostat=ios,err=1) 
     &      '*Attach',tab,CONCP(I,J,1),tab,CONCP(I,J,2),
     &      tab,ICNCT(I,J),' # X Y & type of attachment'
  17    CONTINUE
        WRITE(INUNIT,'(A)',iostat=ios,err=1)'*End_icon'
  10  CONTINUE 

C Write out the current connection data
      WRITE(INUNIT,'(A)',iostat=ios,err=1)'# '
      WRITE(INUNIT,'(2A,i3)',iostat=ios,err=1)'*Nb_connections',tab,
     &  NICNN
      WRITE(INUNIT,'(A)',iostat=ios,err=1)'# '
      DO 20 I=1,NICNN
        WRITE(INUNIT,'(A)',iostat=ios,err=1) 
     &    '# Icon at start, (name index), Icon at end, (name index)'
        write(ts12,'(a)')NWICNM(ICNS(I))
        write(te12,'(a)')NWICNM(ICNE(I))
        WRITE(INUNIT,'(4a,I3,3a,I3)',iostat=ios,err=1) '*Conn',tab, 
     &    ts12(1:lnblnk(ts12)),tab,ICNS(I),tab,
     &    te12(1:lnblnk(te12)),tab,ICNE(I)
        WRITE(INUNIT,'(I3,a,I3,a,I3,a)',iostat=ios,err=1) ICNSP(I),tab,
     &    ICNEP(I),tab,ICNNT(I),' # attach index @ each icon & type'
        if(idatrdom(I).gt.0)then
          write(INUNIT,'(2a,i2)',iostat=ios,err=1) '*Attribute_cnn',
     &    tab,idatrdom(I)
          do 111 J=1,idatrdom(I)

C Write out attributes with minimal white space.
            l3=lnblnk(ddtagatr(I,J,1))
            l4=lnblnk(ddtagatr(I,J,2))
            l5=lnblnk(ddtagatr(I,J,3))
            l5a=lnblnk(ddtagatr(I,J,4))
            l5b=lnblnk(ddtagatr(I,J,5))
            l6=lnblnk(ddmenuatr(I,J))
            l7=MAX0(lnblnk(ddatrib(I,J,1)),1)
            l8=MAX0(lnblnk(ddatrib(I,J,2)),1)
            l9=MAX0(lnblnk(ddatrib(I,J,3)),1)
            write(INUNIT,'(17a)') ddtagatr(I,J,1)(1:l3),tab,
     &        ddtagatr(I,J,2)(1:l4),tab,ddtagatr(I,J,3)(1:l5),tab,
     &        ddtagatr(I,J,4)(1:l5a),tab,ddatrib(I,J,1)(1:l7),tab,      
     &        ddatrib(I,J,2)(1:l8),tab,ddatrib(I,J,3)(1:l9),tab,
     &        ddtagatr(I,J,5)(1:l5b),tab,ddmenuatr(I,J)(1:l6)
 111      continue
          write(INUNIT,'(a)',iostat=ios,err=1) '*End_cnn_attribute'
        endif

        DO 22 J=1,NCONWP(I)
          WRITE(INUNIT,'(2a,F8.4,a,F8.4,a,F8.4,a)',iostat=ios,err=1) 
     &      '*Waypoint',tab,CNWNP(I,J,1),tab,CNWNP(I,J,2),tab,
     &      CNWNP(I,J,3),' # XYZ point along this connection'
  22    CONTINUE
  20  CONTINUE
      WRITE(INUNIT,'(A)',iostat=ios,err=1)'*End_connections'

C Close the file and exit
      WRITE(INUNIT,'(A)',iostat=ios,err=1)'*End_network'

C Inform the user of  a sucessful write
      OUTS=' '
      WRITE(OUTS,'(A,A)') 'Written graphical network file ',NWKFLNAM
      CALL EDISP(IUOUT,OUTS)

Close the file and return
      CLOSE(INUNIT)

      RETURN

C Error trap.
    1 if(IOS.eq.2)then
        write(outs,'(a)') 'No permission to write network file.'
      else
        write(outs,'(a)') 'Error writing network file.'
      endif
      call edisp(iuout,outs)
      CALL ERPFREE(INUNIT,IOS)
      return
      END

C *************************** NWKINIT ************************
C NWKINIT is the initialisation for network common blocks. 
      SUBROUTINE NWKINIT

#include "gnetwk.h"

C Icon commons
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)
  
      COMMON/NWKICS/NWICNM(MICN),NWICNHIS(MICN),NWICNLBL(MICN,MICND),
     & NWICNTXT(MICN,60) 

C Connection commons
      COMMON/NWKCON/NICNN,ICNS(MNCNN),ICNE(MNCNN),ICNNT(MNCNN),
     & ICNSP(MNCNN),ICNEP(MNCNN),CNWNP(MNCNN,MCIP,3),
     & NCONWP(MNCNN),idatrdom(MNCNN),ddtagatr(MNCNN,MIATRB,5),
     & ddatrib(MNCNN,MIATRB,3),ddmenuatr(MNCNN,MIATRB)

C Global attributes commons (associated with igatrdom(),dgtagatr(),
C     &  dgatrib(),dgmenuatr() passed back via scanicondb call.
      COMMON/NWKGLOB/idgatrdom,ddgtagatr(MIATRB,5),
     & ddgatrib(MIATRB,3),ddgmenuatr(MIATRB)

C Grid commons
      COMMON/NWKGRD/GRMAX(3),GRSPC(3),GRLYRH(MLYRS)
      COMMON/NWKGRDL/GON,SON
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKFLNAM,NWKTYPSTR(MNWKTYP)
      COMMON/ICONDBNAM/ICONDBFL      
      COMMON/NWKTYP/INWKTYP,vergnf

C Icon rotation angle
      COMMON/IROT/ROTA

C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DMCMPDBFL,DICONDBFL,dmdbnam

      CHARACTER*72 NWKNAM,NWKDSC,NWKFLNAM,ICONDBFL
      CHARACTER NWICNM*12,NWKTYPSTR*12,NWICNHIS*40,NWICNLBL*4
      character NWICNTXT*72
      character ATRTAG*12,ATRMENU*32,ATRICN*12
      character ddtagatr*12,ddmenuatr*32,ddatrib*12
      character ddgtagatr*12,ddgmenuatr*32,ddgatrib*12
      character*72 DFCFD,DECMPDBFL,DMCMPDBFL,DICONDBFL,dmdbnam
      LOGICAL GON,SON,ISEL,CSEL

C Initialise the location of the icons
      ICONDBFL=DICONDBFL

C Set the initial view 1 - xy 2 - xz 3 - yz.
      IVIEW=1
      
C Set the x,y and z co-ordinates.
      GRMAX(1)=FLOAT(MGRXL)
      GRMAX(2)=FLOAT(MGRYL)

C Z-ordinate (also relates to layers).
      GRMAX(3)=FLOAT(MGRZL)

C Switch the grid ON.
      GON=.TRUE.

C Switch the grid snap on.
      SON=.TRUE.

C Set the grid spacing and number of layers, ensuring the spacing always gives
C a whole number of layers!
      GRSPC(1)=0.5
      GRSPC(2)=0.5
      GRSPC(3)=1.0

C Each layer height can also be individually set by the user in the options
C later the concept of layers in the z-direction use a height instead!

C Set the scaling factor
      SCALF=1.

C Set the view centre
      VIEWCEN(1)=MGRXE*0.5
      VIEWCEN(2)=MGRYE*0.5
      VIEWCEN(3)=3.

C Set the initial x y and z view limits
      VIEWLIM(1)=VIEWCEN(1)-((FLOAT(MGRXE)*0.5)/SCALF)
      VIEWLIM(2)=VIEWCEN(1)+((FLOAT(MGRXE)*0.5)/SCALF)
      VIEWLIM(3)=VIEWCEN(2)-((FLOAT(MGRYE)*0.5)/SCALF)
      VIEWLIM(4)=VIEWCEN(2)+((FLOAT(MGRYE)*0.5)/SCALF)

C Start z limit at ground level.
      VIEWLIM(5)=VIEWCEN(3)
      VIEWLIM(6)=VIEWCEN(3)+((FLOAT(MGRZE)*0.5)/SCALF)

      IF(INWKTYP.EQ.0)THEN

C Set the network type (default is plant).
        INWKTYP=1
        NWKTYPSTR(1)='HVAC'
      ENDIF
      NWKTYPSTR(2)='Flow'
      NWKTYPSTR(3)='Electrical'
      NWKTYPSTR(4)='Control'
      NWKTYPSTR(5)='Hygroscopic'
      NWKTYPSTR(6)='PrimitivePt'
       
      NWKDSC='None'

C Set the rotation angle.
      ROTA=90.0

C Clear icon data.
      NNICN=0
      DO 20 I=1,MICN
        ISEL(I)=.false.
        ICONTP(I)=0
        NICONATR(I)=0
        NWICNM(I)=' '
        NWICNHIS(I)=' '
        do 36 k=1,60
          NWICNTXT(I,K)=' '
  36    continue
        do 35 j=1,MICND
          NWICNLBL(I,J)=' '
  35    continue
        DO 21 K=1,MIATRB
          ATRICN(I,K,1)=' '
          ATRICN(I,K,2)=' '
          ATRICN(I,K,3)=' '
  21    CONTINUE
        ISEL(I)=.FALSE.
        DO 23 J=1,3
          XYZICON(I,J)=0.0
  23    CONTINUE

C Clear the icon drawing data.
        NIVE(I)=0
        DO 25 J=1,MICNV
          DO 26 K=1,3
            VCICON(I,J,K)=0.0
  26      CONTINUE
  25    CONTINUE

        NIVC(I)=0
        DO 27 J=1,MICNE
          DO 28 K=1,5
            IVEICN(I,J,K)=0          
  28      CONTINUE
  27    CONTINUE

        NCONP(I)=0
        DO 29 J=1,MCNP
          CONCP(I,J,1)=0.0
          CONCP(I,J,2)=0.0
          ICNCT(I,J)=0
  29    CONTINUE
  20  CONTINUE

C Clear connection data.
      NICNN=0
      DO 30 I=1,MNCNN
        ICNS(I)=0
        ICNE(I)=0        
        ICNNT(I)=0 
        CSEL(I)=.FALSE.
        DO 33 J=1,MCIP
          CNWNP(I,J,1)=0.0
          CNWNP(I,J,2)=0.0
          CNWNP(I,J,3)=0.0
  33    CONTINUE 
        NCONWP(I)=0
        idatrdom(I)=0
        idgatrdom=0
        do 49 J=1,MIATRB
          ddtagatr(I,J,1)=' '
          ddtagatr(I,J,2)=' '
          ddtagatr(I,J,3)=' '
          ddtagatr(I,J,4)=' '
          ddtagatr(I,J,5)=' '
          ddatrib(I,J,1)=' '
          ddatrib(I,J,2)=' '
          ddatrib(I,J,3)=' '
          ddmenuatr(I,J)=' '
          ddgtagatr(J,1)=' '
          ddgtagatr(J,2)=' '
          ddgtagatr(J,3)=' '
          ddgtagatr(J,4)=' '
          ddgtagatr(J,5)=' '
          ddgatrib(J,1)=' '
          ddgatrib(J,2)=' '
          ddgatrib(J,3)=' '
          ddgmenuatr(J)=' '
  49    continue     
  30  CONTINUE

      RETURN

      END

