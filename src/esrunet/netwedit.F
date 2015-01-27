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

C netwedit.F contains:
C   NETICONDATR   - edit an icon data attribute
C   updintunknflow - update attributes of internal unkn press flow node
C   updintknflow  - update attributes of internal known press flow node
C   updwindflow   - update attributes of boundary wind pressure flow node
C   updfixbound   - update attributes of boundary fixed pressure flow node
C   NETCNNDATR    - edit a connection data attribute
C   NETIDATR      - alternate edits of icon domain attributes.
C   NETCDATR      - alternate edit of connection attributes.

C ********************* NETICONDATR ************
C NETICONDATR edits domain attributes of an icon.
C Similar to NETIDATR, but for icons with fewer attributes.
C icnpick is the icon to edit.
      SUBROUTINE NETICONDATR(icnpick,ier)
# include "epara.h"
# include "gnetwk.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      character h*72 

C Icon commons from gnetwk.h      
C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)
      LOGICAL ISEL,CSEL
      COMMON/NWKTYP/INWKTYP,vergnf
      integer INWKTYP
      real vergnf

C Summary of external data.
      common/exsum/isexavail,iuex

C ICATIE: pointer from menu  back to the ATRTAG/ATRMENU/ATRICN/ATRTAG arrays.
      DIMENSION ICATIE(MIATRB)
  
C IECDATE is menu array for attribute data
      DIMENSION IECDATE(MIATRC+10)
      
C Return up to 6 real valus from external subroutine.
      dimension AVAL(6),SVAL(6)
      character KEY*1,outs*124,outsd*124,SVAL*12
      character VSTR*12,VNPSTR*12,ISTR*12,INPSTR*12,exttag*12
      character lasttag*12
      character fluidtag*8  ! for readable fluid type
      character typetag*36  ! user readable node or component

      dimension vert(MICNV,2),iedge(MICNE,5),idot(MICND,4),
     &  iarc(MICND,7),ilabel(MICND,4),labeltx(MICND),text(60),
     &  iatt(MCNP,2)
      character labeltx*4,text*72

      CHARACTER IECDATE*40

C icontoflow() is a pointer from icon to flow node (positive) or flow
C component (negative).
C icnisnode(MNCNN) toggle where .true. means icon index is
C associated with a flow node.
      common/NWKFLW/icontoflow(MNCNN),icnisnode(MNCNN),
     &  inodetoicon(MNCNN),icomptoicon(MNCNN)
      logical icnisnode

      LOGICAL close,isexavail,OK,dok

C Logical flags set depending on the type of flow node.
      logical closect,closewt,closeft,closekt,CLOSEBT
      integer NECDATA,ICOUT1 ! max items and current menu item
      integer iftype,ifluid  ! attribute indicating node type or component type and fluid
      integer la,lb,lf       ! lengths

C WORDSS array of string tokens from each NWICHNIS.
      CHARACTER*32 WORDSS(12)
      character ICDNAM*12,category*12,icmenu*36

#ifdef OSI
      integer impx,impy,iwe
#else
      integer*8 impx,impy,iwe
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='NETICONDATR'  ! set for subroutine

C Clear local storage arrays.
      icn=icnpick
      ISEL(ICN)=.TRUE.
      NCATI=0
      DO 507 J=1,MIATRC
        ICATIE(J)=0
  507 CONTINUE

C Search for unique categories to present to the user.
      DO 520 I=1,NICONATR(ICN)
        if(i.eq.1)then
          lasttag=atrtag(icn,I,1)
          ncati=1
        else
          if(lasttag.eq.atrtag(icn,i,1))then
            continue
          else
            lasttag=atrtag(icn,I,1)
            ncati=ncati + 1
          endif
        endif
  520 CONTINUE

C Decode NWICNHIS() so that getanicon can be called to populate NWICNME.
      call getutokens(NWICNHIS(icn),':',IW,WORDSS)
      write(category,'(a)') WORDSS(2)(1:lnblnk(WORDSS(2)))
      write(ICDNAM,'(a)') WORDSS(3)(1:lnblnk(WORDSS(3)))
      call geticonindex(inwktyp,category,ICDNAM,idbcat,
     &  idbitem,IER)
 
C Get details of icon and then get its attributes (if any).
      if(idbitem.gt.0)then
        call getanicon(inwktyp,category,ICDNAM,nbvert,vert,
     &    nbedge,iedge,nbdot,idot,nblabel,ilabel,labeltx,
     &    nbarc,iarc,nbatt,iatt,nbtext,text,icmenu,IER)
      endif

C Display the attributes for this icon. Put a label line at the beginning
C of each category to separate the data. The logic below assumes that the
C attributes of each catagory are grouped together.
  705 continue
      NECDATA=0
      MHEAD=5
      MCTL=3

C The length is the number of attributes plus one line for each different
C category.
      ILEN=NICONATR(ICN) + ncati
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C << if we allowed re-definition of the database entry that would
C << allow (for flow components or for nodes as well?) changes to
C << a different type.  How to do this?

C Initial menu entry setup.
  706 IER=0
      ILEN=NICONATR(ICN) + ncati
      ICOUT1=-3
      WRITE(IECDATE(1),'(2A)') 'a name: ', NWICNM(ICN)
      WRITE(IECDATE(2),'(A)')  'b database entry information:'
      WRITE(IECDATE(3),'(2A)') '  ',NWICNHIS(ICN)(1:38)

C Clarify the node or component type.
      WRITE(IECDATE(4),'(2A)') '  ',NWICNME(ICN)
      WRITE(IECDATE(5),'(A)')  'c view entry notes                '          
      M=MHEAD

C Build potentially long list of menu selections. Create a key
C for each iecdate, but do not include the key in lines which
C act as labels.
      latrindex=0

C Debug.
C      write(6,*) 'ilen ',ilen

      DO 707 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(M-3,KEY,IER)
          if(L.eq.1)then

C First in list is label line for first category. Set icatie for
C menu entry M to 0 to signal to ignore.
            lasttag=atrtag(icn,1,1)
            write(iecdate(m),'(3a)')' ___data for ',
     &        lasttag(1:lnblnk(lasttag)),'_________'
            ncati=1
            icatie(m)=0
            latrindex=1
          else

C Subsetuent attribute, if atrtag matches lasttag then we have an
C attribute to include in the menu otherwise begin a new category
C count.
            if(lasttag.eq.atrtag(icn,latrindex,1))then

C Write attributes name and data. Set icatie for menu item M
C to latrindex for use later. Also adjust the menu to reflect
C whether the item can be edited by the user. In the case of
C a flow component fluid type decode the 2nd attribute number 
C into a text equivalent.
              fluidtag='  '
              if(INWKTYP.eq.2)then
                read(ATRICN(icn,1,1),*,iostat=ios,ERR=99) iftype  ! flow node or component type

C Decode node or component type into a short string.
                read(ATRICN(icn,2,1),*,iostat=ios,ERR=99) ifluid  ! fluid type
                if(latrindex.eq.2.and.ifluid.eq.1) fluidtag=' air '
                if(latrindex.eq.2.and.ifluid.eq.2) fluidtag=' water '
                if(latrindex.eq.2.and.ifluid.eq.0) fluidtag=' unknown'
              endif
              la=lnblnk(ATRMENU(ICN,latrindex))
              lb=lnblnk(ATRICN(ICN,latrindex,1))
              lf=lnblnk(fluidtag)
              if(lf.eq.0) lf=1
              if(ATRTAG(ICN,latrindex,5)(1:4).ne.'stat')THEN
                WRITE(IECDATE(M),'(A,1x,A,1x,2A)') Key,
     &            ATRMENU(ICN,latrindex)(1:la),
     &            ATRICN(ICN,latrindex,1)(1:lb),fluidtag(1:lf)
                icatie(m)=latrindex
                latrindex=latrindex+1
              else
                WRITE(IECDATE(M),'(2x,A,1x,2A)') 
     &            ATRMENU(ICN,latrindex)(1:la),
     &            ATRICN(ICN,latrindex,1)(1:lb),fluidtag(1:lf)
                icatie(m)=0
                latrindex=latrindex+1
              endif
            else

C Write a label, set icatie for menu item M to reflect this.
              lasttag=atrtag(icn,latrindex,1)
              ncati=ncati + 1
              write(iecdate(m),'(3a)')' ___data for ',
     &          lasttag(1:lnblnk(lasttag)),'________'
              icatie(m)=0
            endif
          endif
        endif
  707 CONTINUE

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        IECDATE(M+1)='  ________________________________ '
      ELSE
        WRITE(IECDATE(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' ----------')
      ENDIF
      WRITE(IECDATE(M+2),'(A)') '? help '
      WRITE(IECDATE(M+3),'(A)') '- exit this menu '
      NECDATA=M+MCTL  
      CALL NETWDRW

C Help for the menu.
      helptopic='component_data_edit'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Find width of menu.
      ilwa=0
      do 22 ij=1,necdata
        la=lnblnk(iecdate(ij))
        if(la.gt.ilwa) ilwa=la
  22  continue

C Make menu only as wide as it needs to be.
      if(MMOD.eq.8)then
        impx=0
        impy=0
        iwe=ilwa
        if(INWKTYP.eq.2)then

C Adapt the menu title based on the domain and if flow whether we
C are working with a node or a component.
          read(ATRICN(icn,1,1),*,iostat=ios,ERR=99) iftype  ! flow node or component type
          if(iftype.le.3)then
            CALL VWMENU('Node data',IECDATE,NECDATA,impx,impy,iwe,
     &        irpx,irpy,ICOUT1)
          else
            CALL VWMENU('Component data',IECDATE,NECDATA,impx,impy,iwe,
     &        irpx,irpy,ICOUT1)
          endif
        else
          CALL VWMENU('Component data',IECDATE,NECDATA,impx,impy,iwe,
     &      irpx,irpy,ICOUT1)
        endif
      else
        CALL EMENU('Component data',IECDATE,NECDATA,ICOUT1)
      endif

      IF(ICOUT1.EQ.NECDATA)THEN
        ISEL(ICN)=.FALSE.  ! unhilight the icon on exit
        RETURN
      ELSEIF(ICOUT1.EQ.NECDATA-1)THEN   
        helptopic='component_data_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Data categories',7,'-',0,0,IER)
        goto 706

      ELSEIF(ICOUT1.EQ.NECDATA-2)THEN   

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        goto 705

      ELSEIF(ICOUT1.EQ.1)THEN

C Edit the icon name.
        CALL NETWDRW
        CALL EASKS(NWICNM(ICN),'Give a (unique) name',
     &    '(with no blank spaces) ',12,NWICNM(ICN),'edtcmp',
     &    IER,nbhelp)

C Check for duplicate names ....
        call dupicname(icn)
        goto 706

      ELSEIF(ICOUT1.EQ.2)THEN

C Ask user if they want to replace the current icon with a
C different one.
        dok=.false.
        CALL ASKOK('Select a different node or component',
     &    'for the current one at this position?',OK,dok,nbhelp)
        if(OK)then
          call ICONREPLACE(icn)

C Decode updated NWICNHIS() so that getanicon can be called to populate NWICNME.
          call getutokens(NWICNHIS(icn),':',IW,WORDSS)
          write(category,'(a)') WORDSS(2)(1:lnblnk(WORDSS(2)))
          write(ICDNAM,'(a)') WORDSS(3)(1:lnblnk(WORDSS(3)))
          call geticonindex(inwktyp,category,ICDNAM,idbcat,
     &      idbitem,IER)
 
C Get details of icon and then get its attributes (if any).
          if(idbitem.gt.0)then
            call getanicon(inwktyp,category,ICDNAM,nbvert,vert,
     &        nbedge,iedge,nbdot,idot,nblabel,ilabel,labeltx,
     &        nbarc,iarc,nbatt,iatt,nbtext,text,icmenu,IER)
          endif
        endif
        goto 705

      ELSEIF(ICOUT1.EQ.5)THEN

C Display text associated with this icon.
        if(nbtext.gt.0)then
          call edisp(iuout,'Component notes: ')
          do 116 jh=1,nbtext
            write(h(jh),'(a)') text(jh)(1:70) 
  116     continue
          CALL PHELPD('Component information',nbtext,'-',0,0,IER)
        endif
        goto 706

      ELSEIF(ICOUT1.GT.MHEAD.AND.ICOUT1.LT.(NECDATA-MCTL+1))THEN

C One of the list. Determine if it is a label (icatie is zero), data
C that cannot be edited by the user (icatie is zero) or actual data.

C Debug.
C        write(6,*) 'icatie ',icatie(icout1)

        if(icatie(icout1).eq.0)then
          goto 705
        endif
        ifocus = icatie(icout1)

C Debug.
C        write(6,*) 'ifocus is ',ifocus,icout1

        ll=lnblnk(ATRMENU(ICN,ifocus))
        write(outs,'(4a)') 'The current value of ',
     &    ATRMENU(ICN,ifocus)(1:ll),' is ',ATRICN(ICN,ifocus,1)
        call edisp(iuout,outs)
        IF(ATRTAG(ICN,ifocus,2)(1:4).eq.'intg')THEN

C For an integer attribute, first read DATA strings into IVMIN/IVMAX/IVAL 
C variables, then edit and write it back to the string ISTR, strip off any
C blanks at the start and assign back to DATA and ATRICN.
          read(ATRICN(icn,ifocus,2),*,iostat=ios,ERR=99)IVMIN
          read(ATRICN(icn,ifocus,3),*,iostat=ios,ERR=99)IVMAX
          read(ATRICN(icn,ifocus,1),*,iostat=ios,ERR=99)IVAL
          read(ATRICN(icn,ifocus,1),*,iostat=ios,ERR=99)IVALD

C If ATRTAG(ICN,ifocus,3) is `external` then call selrelexttag
C based on the search string in ATRTAG(ICN,ifocus,4). If iier
C returned as -1 then no selection made.
          if(ATRTAG(ICN,ifocus,3)(1:8).eq.'external'.and.
     &       ATRTAG(ICN,ifocus,5)(1:4).eq.'user')then
            if(isexavail)then
              exttag=ATRTAG(ICN,ifocus,4)
              call epmensv
              call selrelexttag(icn,exttag,AVAL,SVAL,iier)
              call epmenrc
              if(iier.eq.-1)then
                continue
              elseif(iier.eq.0)then

C If flow network (INWKTYP = 2) and the component is internal unknown
C pressure (ATRICN(icn,1,1) is zero) and the fluid type is air
C (ATRICN(icn,2,1) = 1) then import summary atrributes.

C Debug.
                write(6,*) 'external data search for ',exttag,ifocus
                write(6,*) 'data 1:',ATRICN(icn,1,1),ATRICN(icn,2,1),
     &                ATRICN(icn,3,1),ATRICN(icn,4,1),ATRICN(icn,5,1)
                write(6,*) 'aval:',AVAL
                write(6,*) 'aval:',SVAL

C << more code to go here.... >>
              endif
            endif
          endif
          IF(ATRTAG(ICN,ifocus,5)(1:4).eq.'user')THEN
            CALL EASKI(IVAL,ATRMENU(ICN,ifocus),' ',IVMIN,'W',
     &        IVMAX,'W',IVALD,'atrib intg data',IER,nbhelp)
            if(ier.eq.0)then
              write(istr,'(I8)') IVAL
              call removepad(istr,inpstr,ilena,iflag)
              WRITE(ATRICN(ICN,ifocus,1),'(A)') inpstr
            endif
          ENDIF

        ELSEIF(ATRTAG(ICN,ifocus,2)(1:4).eq.'real')THEN

C The attribute is a real number. First read ATRICN strings
C into VMIN/VMAX/VAL real variables. 
          read(ATRICN(icn,ifocus,2),*,iostat=ios,ERR=99)VMIN
          read(ATRICN(icn,ifocus,3),*,iostat=ios,ERR=99)VMAX
          read(ATRICN(icn,ifocus,1),*,iostat=ios,ERR=99)VAL
          read(ATRICN(icn,ifocus,1),*,iostat=ios,ERR=99)VALD

C If ATRTAG(ICN,ifocus,3) is `external` then call selrelexttag
C based on the search string in ATRTAG(ICN,ifocus,4). If iier
C returned as -1 then no selection made.
          if(ATRTAG(ICN,ifocus,3)(1:8).eq.'external'.and.
     &       ATRTAG(ICN,ifocus,5)(1:4).eq.'user')then
            if(isexavail)then
              exttag=ATRTAG(ICN,ifocus,4)
              call epmensv
              call selrelexttag(icn,exttag,AVAL,SVAL,iier)
              call epmenrc
              if(iier.eq.-1)then
                continue
              elseif(iier.eq.0)then

C If flow network (INWKTYP = 2) and the component is internal unknown
C pressure (ATRICN(icn,1,1) is zero) and the fluid type is air
C (ATRICN(icn,2,1) = 1) then import summary atrributes. Use read
C to convert string array ATRICN to number for checking.

C Debug.
C                write(6,*) 'external data search for ',exttag,ifocus
C                write(6,*) 'data 1:',ATRICN(icn,1,1),ATRICN(icn,2,1),
C     &                ATRICN(icn,3,1),ATRICN(icn,4,1),ATRICN(icn,5,1)

                read(ATRICN(icn,1,1),*,iostat=ios,ERR=99) VALCT  ! flow node or component type
                read(ATRICN(icn,2,1),*,iostat=ios,ERR=99) VALFT  ! fluid type
                read(ATRICN(icn,4,1),*,iostat=ios,ERR=99) VALV   ! typically its volume
                CALL ECLOSE(VALCT,0.00,0.01,CLOSECT)  ! internal unknown
                CALL ECLOSE(VALCT,1.00,0.01,CLOSEKT)  ! internal known
                CALL ECLOSE(VALCT,2.00,0.01,CLOSEBT)  ! boundary known
                CALL ECLOSE(VALCT,3.00,0.01,CLOSEWT)  ! wind pressure
                CALL ECLOSE(VALFT,1.00,0.01,CLOSEFT)  ! 1 is air
                if(INWKTYP.eq.2.and.CLOSECT.and.CLOSEFT)then

C If flow network (INWKTYP = 2) and internal unknown pressure, pass in the 
C volume and edit via updintunknflow.
                  call updintunknflow(icn,AVAL,SVAL,VALV)
                  goto 706

                elseif(INWKTYP.eq.2.and.CLOSEKT.and.CLOSEFT)then

C If flow network (INWKTYP = 2) and internal fixed air pressure the volume
C is the 5th atricn entry. Edit via updintknflow.
                  read(ATRICN(icn,5,1),*,iostat=ios,ERR=99)VALV
                  call updintknflow(icn,AVAL,SVAL,VALV)
                  goto 706

                elseif(INWKTYP.eq.2.and.CLOSEWT.and.CLOSEFT)then

C If flow network (INWKTYP = 2) and the component is wind pressure
C pressure the update attributes.
                  call updwindflow(icn,exttag,AVAL,SVAL,VALV)
                  goto 706

                elseif(INWKTYP.eq.2.and.CLOSEBT.and.CLOSEFT)then

C If flow network (INWKTYP = 2) and the component is fixed boundary
C pressure then update attributes.
                  call updfixbound(icn,exttag,AVAL,SVAL)
                  goto 706

                else

C Something else. Use AVAL array returned from selrelexttag.
                  if(AVAL(1).ge.VMIN.and.AVAL(1).le.VMAX)then
                    VAL=AVAL(1)
                  endif
                endif
              endif
            else

C Value not available from an external source, do direct edit.
              CALL EASKR(VAL,ATRMENU(ICN,ifocus),' ',VMIN,'W',
     &          VMAX,'W',VALD,'atrib real data',IER,nbhelp)
            endif

          elseif(ATRTAG(ICN,ifocus,3)(1:2).eq.'- '.and.
     &           ATRTAG(ICN,ifocus,5)(1:4).eq.'user')then

C Otherwise edit the variable and then write it back to the string VSTR, strip off any
C blanks at the start and assign back to DATA and ATRICN.
            CALL EASKR(VAL,ATRMENU(ICN,ifocus),' ',VMIN,'W',
     &        VMAX,'W',VALD,'atrib real data',IER,nbhelp)
          endif

          if(ier.eq.0)then
            call relstr(val,vnpstr,ilena,iflag)
            WRITE(ATRICN(ICN,ifocus,1),'(a)') vnpstr

C If this real attribute is `location` then it is assumed to be a height
C attribute so check and update the Z value of the icon to match it.
            if(atrtag(icn,ifocus,1)(1:8).eq.'location')then
              call eclose(VAL,XYZICON(ICN,3),0.001,close)
              if(.NOT.close)then
                XYZICON(ICN,3)=VAL
                call edisp(iuout,
     &  'Updating icon height (please double check connection data!).')
              endif
            endif

C Report associated connections and update their delta heights.
            do 515 J=1,NICNN
              lns=lnblnk(NWICNM(ICNS(J)))
              lne=lnblnk(NWICNM(ICNE(J)))
              if(icnisnode(ICNS(J)))then
                delta=XYZICON(ICNE(J),3)-XYZICON(ICNS(J),3)
              else
                delta=XYZICON(ICNS(J),3)-XYZICON(ICNE(J),3)
              endif
              if(ICNS(J).eq.ICN.or.ICNE(J).eq.ICN)then
                write(outs,'(3a,f6.3,3a,f6.3,a,f6.3)') 'Height of ',
     &            NWICNM(ICNS(J))(1:lns),' is',XYZICON(ICNS(J),3),
     &          ' & ',NWICNM(ICNE(J))(1:lne),' is ',XYZICON(ICNE(J),3),
     &          ' & delta is ',delta
                call sdelim(outs,outsd,'S',IW)
                call edisp(iuout,outsd)

C Locate data for that connection and its location attribute to update.
                write(outs,*) 'The current value of ',
     &            ddtagatr(J,1,1),' ',ddatrib(J,1,1)
                call sdelim(outs,outsd,'S',IW)
                call edisp(iuout,outsd)
                if(idatrdom(j).gt.0)then
                  do 43 ii=1,idatrdom(j)
                    if(ddtagatr(j,ii,1)(1:8).eq.'location')then
                      call relstr(delta,vnpstr,ilena,iflag)
                      write(ddatrib(j,ii,1),'(a)') vnpstr
                    endif
   43             continue
                endif
                write(outs,*) 'The revised value of ',
     &            ddtagatr(J,1,1),' ',ddatrib(J,1,1)
                call sdelim(outs,outsd,'S',IW)
                call edisp(iuout,outsd)
              endif
  515       continue
          endif

        ELSEIF(atrtag(icn,ifocus,2)(1:4).eq.'text')THEN

C Working with a text attribute. First check if there is an external
C source that is user selectable. If so present the current string
C SVAL(1) for editing. A location attribute might include an associated
C zone name (for an unknown or known pressure flow node).
C << consider adding logic similar to that ~440
          VSTR=ATRICN(ICN,ifocus,1)
          if(atrtag(icn,ifocus,3)(1:8).eq.'external'.and.
     &       atrtag(icn,ifocus,5)(1:4).eq.'user')then
             if(isexavail)then
               exttag=atrtag(icn,ifocus,4)
               call epmensv
               call selrelexttag(icn,exttag,AVAL,SVAL,iier)
               call epmenrc
               if(iier.eq.-1)then
                 continue
               elseif(iier.eq.0)then

C Similar logic to treatment of real data from exernal sources. E.g.
C if location implies a change in other dependencies.

C Debug.
                 write(6,*) 'text ext data search for ',exttag,ifocus
                 write(6,*) 'data 1:',ATRICN(icn,1,1),ATRICN(icn,2,1),
     &               ATRICN(icn,3,1),ATRICN(icn,4,1),ATRICN(icn,5,1)

                 read(ATRICN(icn,1,1),*,iostat=ios,ERR=99) VALCT  ! flow node or component type
                 read(ATRICN(icn,2,1),*,iostat=ios,ERR=99) VALFT  ! fluid type
                 read(ATRICN(icn,4,1),*,iostat=ios,ERR=99) VALV   ! typically its volume
                 CALL ECLOSE(VALCT,0.00,0.01,CLOSECT)  ! internal unknown
                 CALL ECLOSE(VALCT,1.00,0.01,CLOSEKT)  ! internal known
                 CALL ECLOSE(VALCT,2.00,0.01,CLOSEBT)  ! boundary known
                 CALL ECLOSE(VALCT,3.00,0.01,CLOSEWT)  ! wind pressure
                 CALL ECLOSE(VALFT,1.00,0.01,CLOSEFT)  ! 1 is air
                 if(INWKTYP.eq.2.and.CLOSECT.and.CLOSEFT)then

C If flow network (INWKTYP = 2) and internal unknown pressure, pass in the 
C volume and edit via updintunknflow.
                   call edisp(iuout,
     &               'Attributes of internal unknown pressure nodes')
                   call edisp(iuout,'can also be updated.')
                   call updintunknflow(icn,AVAL,SVAL,VALV)
                   goto 706

                 elseif(INWKTYP.eq.2.and.CLOSEKT.and.CLOSEFT)then

C If flow network (INWKTYP = 2) and internal fixed air pressure the volume
C is the 5th atricn entry. Edit via updintknflow.
                   read(ATRICN(icn,5,1),*,iostat=ios,ERR=99)VALV
                   call updintknflow(icn,AVAL,SVAL,VALV)
                   goto 706

                 elseif(INWKTYP.eq.2.and.CLOSEWT.and.CLOSEFT)then

C If flow network (INWKTYP = 2) and the component is wind pressure
C pressure the update attributes.
                   call updwindflow(icn,exttag,AVAL,SVAL,VALV)
                   goto 706

                 elseif(INWKTYP.eq.2.and.CLOSEBT.and.CLOSEFT)then
  
C If flow network (INWKTYP = 2) and the component is fixed boundary
C pressure then update attributes.
                   call updfixbound(icn,exttag,AVAL,SVAL)
                   goto 706

                 else

                   VSTR=SVAL(1)
                   CALL EASKS(VSTR,ATRMENU(ICN,ifocus),'(confirm)',12,
     &               'nothing','atrib text data',IER,nbhelp)
                 endif
               endif
             else
               CALL EASKS(VSTR,ATRMENU(ICN,ifocus),' ',12,
     &           'nothing','atrib text data',IER,nbhelp)
             endif
          elseif(atrtag(icn,ifocus,3)(1:2).eq.'- '.and.
     &           atrtag(icn,ifocus,5)(1:4).eq.'user')then
            CALL EASKS(VSTR,ATRMENU(ICN,ifocus),' ',12,
     &        'nothing','atrib text data',IER,nbhelp)
          endif
          if(ier.eq.0.and.VSTR(1:2).ne.'  ')then
            call removepad(vstr,vnpstr,ilena,iflag)
            ATRICN(ICN,ifocus,1)=VNPSTR
          endif
        ENDIF
        goto 706
      ELSE
        GOTO 705
      ENDIF

      return

  99  call edisp(iuout,'error composing attribute for editing.')
      return
      end

C ************** updintunknflow
C updintunknflow update attributes of internal unkn press flow node
      subroutine updintunknflow(icn,AVAL,SVAL,VALV)
# include "gnetwk.h"
      
      integer lnblnk  ! function definition

C Icon commons from gnetwk.h     
      common/outin/iuout,iuin

C User up to 6 real valus from external subroutine.
      dimension AVAL(6),SVAL(6)
      character outs*124,SVAL*12
      character VNPSTR*12,stra*12,strb*12
      logical closev,closex,closey,closez,ok,dok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='updintunknflow'  ! set for subroutine

C If flow network (INWKTYP = 2) and the component is internal unknown
C pressure (ATRICN(icn,1,1) is zero) and the fluid type is air
C (ATRICN(icn,2,1) = 1) then (ATRICN(icn,4,1) is the volume,
C (ATRICN(icn,5,1) is the height (m), (ATRICN(icn,6,1) is the zone name.
C  map the returned data as follows:
C AVAL(1) is volume, AVAL(2) is cogX, AVAL(3) is cogY, AVAL(4) is cogZ
C SVAL(1) is zone name.
C *typical entry in gnf*
C flow,intg,-,-,0,0,3,static,flow node type
C flow,intg,-,-,1,0,2,user,fluid type
C flow,text,-,-,20.0,user,temperature or node name
C flow,real,external,*Zones,40.50000,0.0,10000.0,user,volume
C location,real,external,*Zones,1.50000,-10.0,99.0,user,height
C location,text,external,*Zones,manager,user,linked zone name

      call edisp(iuout,'Internal unknown pressure comp.')
      
      helptopic='network_mismatch_detect'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ECLOSE(AVAL(1),VALV,0.001,CLOSEV)
      CALL ECLOSE(AVAL(2),XYZICON(ICN,1),0.001,CLOSEX)
      CALL ECLOSE(AVAL(3),XYZICON(ICN,2),0.001,CLOSEY)
      CALL ECLOSE(AVAL(4),XYZICON(ICN,3),0.001,CLOSEZ)
      if(.NOT.CLOSEV)then
        call relstr(AVAL(1),stra,ilena,iflag)
        call relstr(VALV,strb,ilenb,iflag)
        write(outs,'(4a)') 'The zone volume ',stra(1:ilena),
     &    ' does not match current ',strb(1:ilenb)
        dok=.true.
        CALL ASKOK(outs,'Update volume?',OK,dok,nbhelp)
        if(OK)then
          call relstr(AVAL(1),vnpstr,ilena,iflag)
          WRITE(ATRICN(ICN,4,1),'(a)') vnpstr
        endif
      endif
      if(.NOT.CLOSEZ)then
        call relstr(AVAL(4),stra,ilena,iflag)
        call relstr(XYZICON(ICN,3),strb,ilenb,iflag)
        write(outs,'(4a)') 'The zone height ',stra(1:ilena),
     &    ' does not match current ',strb(1:ilenb)
        dok=.true.
        CALL ASKOK(outs,'Update height?',OK,dok,nbhelp)
        if(OK)then

C For this icon the hieght is in the attribute category `location` which
C is the second attribute category.
          call relstr(AVAL(4),vnpstr,ilena,iflag)
          WRITE(ATRICN(ICN,5,1),'(a)') vnpstr
          XYZICON(ICN,3)=AVAL(4)
        endif
      endif
      if(CLOSEX.and.CLOSEY)then
        continue
      else
        call relstr(AVAL(2),stra,ilena,iflag)
        call relstr(AVAL(3),strb,ilenb,iflag)
        write(outs,'(5a,2F8.2)')'The zone COG X Y ',stra(1:ilena),' & ',
     &    strb(1:ilenb),' do not match current X Y ',
     &    XYZICON(ICN,1),XYZICON(ICN,2)
        dok=.true.
        CALL ASKOK(outs,'Update position?',OK,dok,nbhelp)
        if(OK)then

C Store old icon position, find DELTAs between positions.

C Debug.
C          write(6,*) 'for icon ',icn,'current xyz ',XYZICON(ICN,1),
C     &      XYZICON(ICN,2),XYZICON(ICN,3)

          OLDPOSX=XYZICON(ICN,1)
          OLDPOSY=XYZICON(ICN,2)
          OLDPOSZ=XYZICON(ICN,3)
          DELTAX=0.0
          DELTAY=0.0
          DELTAZ=0.0
          XYZICON(ICN,1)=AVAL(2)
          XYZICON(ICN,2)=AVAL(3)
          XYZICON(ICN,3)=AVAL(4)
          DELTAX=XYZICON(ICN,1)-OLDPOSX
          DELTAY=XYZICON(ICN,2)-OLDPOSY
          DELTAZ=XYZICON(ICN,3)-OLDPOSZ

C Debug.
C          write(6,*) 'for icon ',icn,'new xyz ',XYZICON(ICN,1),
C     &      XYZICON(ICN,2),XYZICON(ICN,3),DELTAX,DELTAY,DELTAZ

C Update the icon vertices.

C Debug.
C          write(6,*) 'for icon ',icn,'old vert 1 ',VCICON(ICN,1,1),
C     &      VCICON(ICN,1,2),VCICON(ICN,1,3)

          DO 10 I=1,NIVC(ICN) 
            VCICON(ICN,I,1)=VCICON(ICN,I,1)+DELTAX
            VCICON(ICN,I,2)=VCICON(ICN,I,2)+DELTAY
            VCICON(ICN,I,3)=VCICON(ICN,I,3)+DELTAZ
  10      CONTINUE

C Debug.
C          write(6,*) 'for icon ',icn,'new vert 1 ',VCICON(ICN,1,1),
C     &      VCICON(ICN,1,2),VCICON(ICN,1,3)

          DO 11 I=1,NCONP(ICN)
            CONCP(ICN,I,1)=CONCP(ICN,I,1)+DELTAX
            CONCP(ICN,I,2)=CONCP(ICN,I,2)+DELTAY
  11      CONTINUE
        endif
      endif

C Check if the location name is the same as the zone name.
      if(SVAL(1)(1:12).ne.ATRICN(ICN,6,1)(1:12))then
        write(outs,'(4a)') 'Updating associated zone name to ',
     &    SVAL(1)(1:lnblnk(SVAL(1))),' from ',ATRICN(ICN,6,1)
        call edisp(iuout,outs)
        WRITE(ATRICN(ICN,6,1),'(a)')SVAL(1)
      endif

      return
      end

C ************** updintknflow
C updintknflow update attributes of internal known press flow node.
C It is passed arrays of reals (AVAL) and strings (SVAL) and then
C coordinates updating icon data.
      subroutine updintknflow(icn,AVAL,SVAL,VALV)
# include "gnetwk.h"
      
      integer lnblnk  ! function definition

C Icon commons from gnetwk.h     
      common/outin/iuout,iuin

C User up to 6 real valus from external subroutine.
      dimension AVAL(6),SVAL(6)
      character outs*124,SVAL*12
      character VNPSTR*12,stra*12,strb*12
      logical closev,closep,closez,ok,dok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='updintunknflow'  ! set for subroutine

C If flow network (INWKTYP = 2) and the component is internal known
C pressure (ATRICN(icn,1,1) is 1) and the fluid type is air
C (ATRICN(icn,2,1) = 1) then (ATRICN(icn,4,1) is the pressure,
C (ATRICN(icn,5,1) is the volume (m^3), (ATRICN(icn,6,1) is the height (m),
C (ATRICN(icn,7,1) is the zone name.
C  map the returned data as follows:
C AVAL(1) is volume, AVAL(2) is cogX, AVAL(3) is cogY, AVAL(4) is cogZ
C SVAL(1) is zone name.
C *typical entry in gnf*
C flow,intg,-,-,0,0,3,static,flow node type
C flow,intg,-,-,1,0,2,user,fluid type
C flow,text,-,-,20.0,user,temperature or node name
C flow,real,-,-,1.0,0.0,100.0,user,pressure
C flow,real,external,*Zones,40.50000,0.0,10000.0,user,volume
C location,real,external,*Zones,1.50000,-10.0,99.0,user,height
C location,text,external,*Zones,manager,user,linked zone name

      call edisp(iuout,'Internal known pressure comp.')
      
      helptopic='network_mismatch_detect'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ECLOSE(AVAL(1),VALV,0.001,CLOSEV)
      CALL ECLOSE(AVAL(4),XYZICON(ICN,3),0.001,CLOSEZ)
      if(.NOT.CLOSEV)then
        call relstr(AVAL(1),stra,ilena,iflag)
        call relstr(VALV,strb,ilenb,iflag)
        write(outs,'(4a)') 'The volume ',stra(1:ilena),
     &    ' does not match current ',strb(1:ilenb)
        dok=.true.
        CALL ASKOK(outs,'Update volume?',OK,dok,nbhelp)
        if(OK)then
          call relstr(AVAL(1),vnpstr,ilena,iflag)
          WRITE(ATRICN(ICN,5,1),'(a)') vnpstr
        endif
      endif
      if(.NOT.CLOSEZ)then
        call relstr(AVAL(4),stra,ilena,iflag)
        call relstr(XYZICON(ICN,3),strb,ilenb,iflag)
        write(outs,'(4a)') 'The height ',stra(1:ilena),
     &    ' does not match current ',strb(1:ilenb)
        dok=.true.
        CALL ASKOK(outs,'Update height?',OK,dok,nbhelp)
        if(OK)then

C For this icon the height is in the attribute category `location` which
C is the second attribute category.
          call relstr(AVAL(4),vnpstr,ilena,iflag)
          WRITE(ATRICN(ICN,6,1),'(a)') vnpstr
          XYZICON(ICN,3)=AVAL(4)
        endif
      endif

C Check if the location name is the same as the zone name.
      if(SVAL(1)(1:12).ne.ATRICN(ICN,7,1)(1:12))then
        write(outs,'(4a)') 'Updating associated zone name ',
     &    SVAL(1)(1:lnblnk(SVAL(1))),' to ',ATRICN(ICN,7,1)
        call edisp(iuout,outs)
        WRITE(ATRICN(ICN,7,1),'(a)')SVAL(1)
      endif

      return
      end

C ************** updwindflow
C updwindflow update attributes of boundary wind pressure flow node
C from the arrays of reals and strings passed in. 
      subroutine updwindflow(icn,exttag,AVAL,SVAL,VALV)
# include "gnetwk.h"
      
      integer lnblnk  ! function definition

C Icon commons from gnetwk.h     
      COMMON/OUTIN/IUOUT,IUIN

      common/NWKFLW/icontoflow(MNCNN),icnisnode(MNCNN),
     &  inodetoicon(MNCNN),icomptoicon(MNCNN)
      logical icnisnode

C User up to 6 real valus from external subroutine.
      dimension AVAL(6),SVAL(6)
      character outs*124,outsd*124,SVAL*12
      character VNPSTR*12,exttag*12
      logical ok,dok,close

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='updwindflow'  ! set for subroutine

C If flow network (INWKTYP = 2) and the component is wind pressure
C pressure (ATRICN(ICN,1,1)=3) and the fluid type is air
C (ATRICN(icn,2,1)=1) and ATRICN(icn,3,1) ispressure coef and
C ATRICN(icn,4,1) is azimuth, and ATRICN(icn,5,1) is height and
C ATRICN(icn,6,1) is zone name and ATRICN(icn,7,1) is surface name.
C   then map the returned data as follows:
C *typical entry in gnf*
C  flow,intg,-,-,3,0,3,static,flow node type
C  flow,intg,-,-,1,0,1,static,fluid type
C  flow,real,external,*Pressures,1.0,0.0,100.0,user,pressure coef index
C  flow,real,external,*Surfaces,180.0,0.0,360.0,user,azimuth
C  location,real,external,*Surfaces,1.95000,-10.0,99.0,user,height
C  location,text,external,*Zones,manager,user,linked zone name
C  location,text,external,*Surfaces,glazing,user,linked surface name

      if(exttag(1:10).eq.'*Pressures')then

C AVAL(1) is pressure coef seta.

C Debug.
C        write(6,*) 'Wind pressure component PC.'

        call relstr(AVAL(1),vnpstr,ilena,iflag)
        WRITE(ATRICN(ICN,3,1),'(a)') vnpstr
        return
      elseif(exttag(1:9).eq.'*Surfaces')then

C Map the returned data as follows:
C AVAL(1) is area, AVAL(2) is azi, AVAL(3) is elev, AVAL(4) is Z(m).
C SVAL(1) is zone name SVAL(2) is surface name.

C Debug.
C        write(6,*) 'Wind pressure component AZ.'

C Inform user about azimuth.
        write(outs,'(3a,f7.2)') 'Azimuth (was) ',ATRICN(ICN,4,1),
     &    ' (now) ',AVAL(2)
        call edisp(iuout,outs)
        call relstr(AVAL(2),vnpstr,ilena,iflag)  ! azimuth
        WRITE(ATRICN(ICN,4,1),'(a)') vnpstr

C Inform user about height.
        write(outs,'(3a,f7.3)') 'Height (was) ',ATRICN(ICN,5,1),
     &    ' (now) ',AVAL(4)
        call edisp(iuout,outs)
        call relstr(AVAL(4),vnpstr,ilena,iflag)  ! height
        WRITE(ATRICN(ICN,5,1),'(a)')vnpstr
        call eclose(AVAL(4),XYZICON(ICN,3),0.001,close)

C If height has changed then look for connection data to update.
C << not yet done >>
        if(.NOT.close)then
          XYZICON(ICN,3)=AVAL(4)
          call edisp(iuout,
     &  'Updating icon height (please double check connection data!).')

C Report associated connections and update their delta heights.
          do 515 J=1,NICNN
            lns=lnblnk(NWICNM(ICNS(J)))
            lne=lnblnk(NWICNM(ICNE(J)))
            if(icnisnode(ICNS(J)))then
              delta=XYZICON(ICNE(J),3)-XYZICON(ICNS(J),3)
            else
              delta=XYZICON(ICNS(J),3)-XYZICON(ICNE(J),3)
            endif
            if(ICNS(J).eq.ICN.or.ICNE(J).eq.ICN)then
              write(outs,'(3a,f6.3,3a,f6.3,a,f6.3)') 'Height of ',
     &          NWICNM(ICNS(J))(1:lns),' is',XYZICON(ICNS(J),3),
     &          ' & ',NWICNM(ICNE(J))(1:lne),' is ',XYZICON(ICNE(J),3),
     &          ' & delta is ',delta
              call sdelim(outs,outsd,'S',IW)
              call edisp(iuout,outsd)

C Locate data for that connection and its location attribute to update.
              write(outs,*) 'The current value of ',
     &          ddtagatr(J,1,1),' ',ddatrib(J,1,1)
              call sdelim(outs,outsd,'S',IW)
              call edisp(iuout,outsd)
              if(idatrdom(j).gt.0)then
                do 43 ii=1,idatrdom(j)
                  if(ddtagatr(j,ii,1)(1:8).eq.'location')then
                    call relstr(delta,vnpstr,ilena,iflag)
                    write(ddatrib(j,ii,1),'(a)') vnpstr
                  endif
   43           continue
              endif
              write(outs,*) 'The revised value of ',
     &          ddtagatr(J,1,1),' ',ddatrib(J,1,1)
              call sdelim(outs,outsd,'S',IW)
              call edisp(iuout,outsd)
            endif
  515     continue
        endif

C Check if the location zone name is the same as the zone name.
        helptopic='network_mismatch_detect'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(SVAL(1)(1:12).ne.ATRICN(ICN,6,1)(1:12))then
          write(outs,'(4a)')
     &      'Updating associated zone & surface name to',
     &      SVAL(1)(1:lnblnk(SVAL(1))),' from ',ATRICN(ICN,6,1)
          call edisp(iuout,outs)
          WRITE(ATRICN(ICN,6,1),'(a)')SVAL(1)
          WRITE(ATRICN(ICN,7,1),'(a)')SVAL(2)
        endif

C Debug.
C        write(6,*) 'data 1:',ATRICN(icn,1,1),
C     &    ATRICN(icn,2,1),ATRICN(icn,3,1),
C     &    ATRICN(icn,4,1),ATRICN(icn,5,1),ATRICN(icn,6,1)

      endif
      return
      end


C ************** updfixbound
C updfixbound update attributes of boundary fixed pressure flow node
C from the arrays of reals and strings passed in. 
      subroutine updfixbound(icn,exttag,AVAL,SVAL)
# include "gnetwk.h"
      
      integer lnblnk  ! function definition

C Icon commons from gnetwk.h     
      COMMON/OUTIN/IUOUT,IUIN

      common/NWKFLW/icontoflow(MNCNN),icnisnode(MNCNN),
     &  inodetoicon(MNCNN),icomptoicon(MNCNN)
      logical icnisnode

C User up to 6 real valus from external subroutine.
      dimension AVAL(6),SVAL(6)
      character outs*124,outsd*124,SVAL*12
      character VNPSTR*12,exttag*12
      logical ok,dok,close

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='updfixbound'  ! set for subroutine

C If flow network (INWKTYP = 2) and the component is wind pressure
C pressure (ATRICN(ICN,1,1)=2) and the fluid type is air
C (ATRICN(icn,2,1)=1) and ATRICN(icn,3,1) is pressure
C ATRICN(icn,4,1) is text, and ATRICN(icn,5,1) is height and
C ATRICN(icn,6,1) is zone name and ATRICN(icn,7,1) is surface name.
C   then map the returned data as follows:
C *typical entry in gnf*
C  flow,intg,-,-,3,0,3,static,flow node type
C  flow,intg,-,-,1,0,1,static,fluid type
C  flow,real,-,-,1.0,0.0,100.0,user,fixed pressure
C  flow,text,-,-,20.0,user,temperature or node name
C  location,real,external,*Surfaces,1.95000,-10.0,99.0,user,height
C  location,text,external,*Surfaces,manager,user,linked zone name
C  location,text,external,*Surfaces,glazing,user,linked surface name

      if(exttag(1:9).eq.'*Surfaces')then

C Map the returned data as follows:
C AVAL(1) is area, AVAL(2) is azi, AVAL(3) is elev, AVAL(4) is Z(m).
C SVAL(1) is zone name SVAL(2) is surface name.

C Debug.
C        write(6,*) 'Bounday fixed pressure component.'

C Inform user about height.
        write(outs,'(3a,f7.3)') 'Height (was) ',ATRICN(ICN,5,1),
     &    ' (now) ',AVAL(4)
        call edisp(iuout,outs)
        call relstr(AVAL(4),vnpstr,ilena,iflag)  ! height
        WRITE(ATRICN(ICN,5,1),'(a)')vnpstr
        call eclose(AVAL(4),XYZICON(ICN,3),0.001,close)

C If height has changed then look for connection data to update.
        if(.NOT.close)then
          XYZICON(ICN,3)=AVAL(4)
          call edisp(iuout,
     &  'Updating icon height (please double check connection data!).')

C Report associated connections and update their delta heights.
          do 515 J=1,NICNN
            lns=lnblnk(NWICNM(ICNS(J)))
            lne=lnblnk(NWICNM(ICNE(J)))
            if(icnisnode(ICNS(J)))then
              delta=XYZICON(ICNE(J),3)-XYZICON(ICNS(J),3)
            else
              delta=XYZICON(ICNS(J),3)-XYZICON(ICNE(J),3)
            endif
            if(ICNS(J).eq.ICN.or.ICNE(J).eq.ICN)then
              write(outs,'(3a,f6.3,3a,f6.3,a,f6.3)') 'Height of ',
     &          NWICNM(ICNS(J))(1:lns),' is',XYZICON(ICNS(J),3),
     &          ' & ',NWICNM(ICNE(J))(1:lne),' is ',XYZICON(ICNE(J),3),
     &          ' & delta is ',delta
              call sdelim(outs,outsd,'S',IW)
              call edisp(iuout,outsd)

C Locate data for that connection and its location attribute to update.
              write(outs,*) 'The current value of ',
     &          ddtagatr(J,1,1),' ',ddatrib(J,1,1)
              call sdelim(outs,outsd,'S',IW)
              call edisp(iuout,outsd)
              if(idatrdom(j).gt.0)then
                do 43 ii=1,idatrdom(j)
                  if(ddtagatr(j,ii,1)(1:8).eq.'location')then
                    call relstr(delta,vnpstr,ilena,iflag)
                    write(ddatrib(j,ii,1),'(a)') vnpstr
                  endif
   43           continue
              endif
              write(outs,*) 'The revised value of ',
     &          ddtagatr(J,1,1),' ',ddatrib(J,1,1)
              call sdelim(outs,outsd,'S',IW)
              call edisp(iuout,outsd)
            endif
  515     continue
        endif

C Check if the location zone name is the same as the zone name.
        helptopic='network_mismatch_detect'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(SVAL(1)(1:12).ne.ATRICN(ICN,6,1)(1:12))then
          write(outs,'(4a)')'Associated zone name ',
     &      SVAL(1)(1:lnblnk(SVAL(1))),
     &      ' does not match current atrribute ',ATRICN(ICN,6,1)
          dok=.true.
          CALL ASKOK(outs,'Update zone & surface names?',OK,dok,nbhelp)
          if(OK)then
            WRITE(ATRICN(ICN,6,1),'(a)')SVAL(1)
            WRITE(ATRICN(ICN,7,1),'(a)')SVAL(2)
          endif
        endif

C Debug.
C        write(6,*) 'data 3:',ATRICN(icn,1,1),
C     &    ATRICN(icn,2,1),ATRICN(icn,3,1),
C     &    ATRICN(icn,4,1),ATRICN(icn,5,1),ATRICN(icn,6,1)

      endif
      return
      end

C ********************* NETCNNDATR ************
C Edit a connection data attributes. This version is used for
C connections with few attributes.
C Imatch is the index of the connection to edit.
C      SUBROUTINE NETCNNDATR(IER,IX,IY)
      SUBROUTINE NETCNNDATR(IER,imatch)

# include "epara.h"
# include "gnetwk.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN

C Icon commons from gnetwk.h
C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)

C icontoflow() is a pointer from icon to flow node (positive) or flow
C component (negative).
C icnisnode(MNCNN) toggle where .true. means icon index is
C associated with a flow node.
      common/NWKFLW/icontoflow(MNCNN),icnisnode(MNCNN),
     &  inodetoicon(MNCNN),icomptoicon(MNCNN)
      logical icnisnode

C ICATIE: pointer from DATACATIE bookkeepping back to the NWKCON arrays.
      DIMENSION ICATIE(MIATRB)
  
C IECDATE is menu array for attribute categories.
      DIMENSION IECDATE(MIATRC+6)

      LOGICAL ISEL,CSEL

      CHARACTER outs*124,outsd*124,ct*12

C WORDSS and WORDSE are arrays of string tokens from each NWICHNIS.
      CHARACTER*32 WORDSS(12),WORDSE(12)
      character VSTR*12,VNPSTR*12,ISTR*12,INPSTR*12,key*1
      CHARACTER IECDATE*40,t32*32,lasttag*12
      integer NECDATA,ICOUT1 ! max items and current menu item

#ifdef OSI
      integer impx,impy,iwe
#else
      integer*8 impx,impy,iwe
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='NETCNNDATR'  ! set for subroutine
      helptopic='connection_attributes'
      call gethelptext(helpinsub,helptopic,nbhelp)

      J=imatch
      CSEL(J)=.TRUE.
      CALL NETWDRW
      if(icnnt(j).eq.0) ct='none'
      if(icnnt(j).eq.1) ct='air'
      if(icnnt(j).eq.2) ct='water'
      if(icnnt(j).eq.3) ct='steam'
      if(icnnt(j).eq.4) ct='refrigerant'
      if(icnnt(j).eq.5) ct='fuel'
      if(icnnt(j).eq.6) ct='comb_product'
      if(icnnt(j).eq.7) ct='signal'
      lns=lnblnk(NWICNM(ICNS(J)))
      lne=lnblnk(NWICNM(ICNE(J)))
      lhs=lnblnk(NWICNHIS(ICNS(J)))
      lhe=lnblnk(NWICNHIS(ICNE(J)))
      write(outs,'(10a)') 'Between ',NWICNM(ICNS(J))(1:lns),' (',
     &  NWICNHIS(ICNS(J))(1:lhs),') & ',NWICNM(ICNE(J))(1:lne),
     &  ' (',NWICNHIS(ICNE(J))(1:lhe),') via ',ct
      call sdelim(outs,outsd,'S',IW)
      call edisp(iuout,outsd)

C If looking from node to a component (first portion of a network
C flow connection) subtract the node height from the component
C height to get delta. If looking from a component to
C a node (2nd portion of a network flow connection) then subtract
C the node height from the component height.
      if(icnisnode(ICNS(J)))then
        delta=XYZICON(ICNE(J),3)-XYZICON(ICNS(J),3)
      else
        delta=XYZICON(ICNS(J),3)-XYZICON(ICNE(J),3)
      endif
      write(outs,'(3a,f6.3,3a,f6.3,a,f6.3)') 'Height of ',
     &  NWICNM(ICNS(J))(1:lns),' is',XYZICON(ICNS(J),3),
     &  ' & ',NWICNM(ICNE(J))(1:lne),' is ',XYZICON(ICNE(J),3),
     &  ' & delta is ',delta
      call sdelim(outs,outsd,'S',IW)
      call edisp(iuout,outsd)

C Parse details of the start icon via getutokens.
      call getutokens(NWICNHIS(ICNS(J)),':',IW,WORDSS)

C Parse details of the end icon via getutokens. Compare and
C if they match then scan for a matching domain (which will
C indicate attributes of this connection which can be edited).
      call getutokens(NWICNHIS(ICNE(J)),':',IW,WORDSE)
      if(WORDSS(1)(1:4).ne.WORDSE(1)(1:4))then
        call edisp(iuout,'WARNING: start and end fluids do not match!')
      endif

C Clear the menu structures.
      NCATI=0
      DO 507 JJ=1,MIATRC
        ICATIE(JJ)=0
  507 CONTINUE

C Build the menu commands. First present a list of connection
C attribute categories.
      do 43 i=1,idatrdom(J)
        if(i.eq.1)then
          lasttag=ddtagatr(J,I,1)
          ncati=1
        else
          if(lasttag.eq.ddtagatr(J,i,1))then
            continue
          else
            lasttag=ddtagatr(J,I,1)
            ncati=ncati + 1
          endif
        endif
  43  continue

C Display the connection attributes. Put a label line at the beginning
C of each category to separate the data. The logic below assumes that the
C attributes of each catagory are grouped together.
  705 NECDATA=0
      MHEAD=4
      MCTL=3
      ILEN = idatrdom(J) + NCATI
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
      IER=0
      ILEN = idatrdom(J) + NCATI
      ICOUT1=-3
      WRITE(IECDATE(1),'(A)') ' connection between: '
      lns=lnblnk(NWICNM(ICNS(J)))
      lhs=lnblnk(NWICNHIS(ICNS(J)))
      write(outs,'(2a)')'start component: ',NWICNM(ICNS(J))(1:lns)
      WRITE(IECDATE(2),'(2A)') '  ',outs(1:32)
      lne=lnblnk(NWICNM(ICNE(J)))
      lhe=lnblnk(NWICNHIS(ICNE(J)))
      write(outs,'(2a)') 'end component: ',NWICNM(ICNE(J))(1:lne)
      WRITE(IECDATE(3),'(2A)')  '  ',outs(1:32)
      WRITE(IECDATE(4),'(2A)')  '  working fluid: ',ct         
      M=MHEAD

C Build potentially long list of menu selections. Create a key
C for each iecdate, but do not include the key in lines which
C act as labels.
      latrindex=0

C Debug.
C      write(6,*) 'ilen ',ilen

      DO 707 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          if(L.eq.1)then

C First in list is label line for first category. Set icatie for
C menu entry M to 0 to signal to ignore.
            lasttag=ddtagatr(J,1,1)
            write(iecdate(m),'(3a)')' ___data for ',
     &        lasttag(1:lnblnk(lasttag)),'_________'
            ncati=1
            icatie(m)=0
            latrindex=1
          else

C Subsetuent attribute, if atrtag matches lasttag then we have an
C attribute to include in the menu otherwise beging a new category
C count.
            if(lasttag.eq.ddtagatr(J,latrindex,1))then

C Write attributes name and data. Set icatie for
C menu item M to latrindex for use later. Also adjust the menu to reflect
C whether the item can be edited by the user.
              la=lnblnk(ddmenuatr(J,latrindex))
              lb=lnblnk(ddatrib(J,latrindex,1))
              if(ddtagatr(J,latrindex,5)(1:4).ne.'stat')THEN
                WRITE(IECDATE(M),'(A,2x,A,1x,A)') Key,
     &            ddmenuatr(J,latrindex)(1:la),
     &            ddatrib(J,latrindex,1)(1:lb)
                icatie(m)=latrindex
                latrindex=latrindex+1
              else
                WRITE(IECDATE(M),'(3x,A,1x,A)') 
     &            ddmenuatr(J,latrindex)(1:la),
     &            ddatrib(J,latrindex,1)(1:lb)
                icatie(m)=0
                latrindex=latrindex+1
              endif
            else

C Write a label, set icatie for menu item M to reflect this
              lasttag=ddtagatr(J,latrindex,1)
              ncati=ncati + 1
              write(iecdate(m),'(3a)')' ___data for ',
     &          lasttag(1:lnblnk(lasttag)),'________'
              icatie(m)=0
            endif
          endif
        endif
  707 CONTINUE

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        IECDATE(M+1)='  ______________________________ '
      ELSE
        WRITE(IECDATE(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      WRITE(IECDATE(M+2),'(A)') '? help '
      WRITE(IECDATE(M+3),'(A)') '- exit this menu '
      NECDATA=M+MCTL  
      CALL NETWDRW

C Help text.
      helptopic='connection_attributes'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Find width of menu.
      ilwa=0
      do 22 ij=1,necdata
        la=lnblnk(iecdate(ij))
        if(la.gt.ilwa) ilwa=la
  22  continue

C Make menu only as wide as it needs to be.
      if(MMOD.eq.8)then
        impx=0
        impy=0
        iwe=ilwa
        CALL VWMENU('Connection attributes',IECDATE,NECDATA,impx,impy,
     &    iwe,irpx,irpy,ICOUT1)
      else
        CALL EMENU('Connection attributes',IECDATE,NECDATA,ICOUT1)
      endif
      IF(ICOUT1.EQ.NECDATA)THEN
        RETURN
      ELSEIF(ICOUT1.EQ.NECDATA-1)THEN   
        helptopic='connection_attributes'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Connection attributes',4,'-',0,0,IER)
      ELSEIF(ICOUT1.EQ.NECDATA-2)THEN   

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(ICOUT1.GT.MHEAD.AND.ICOUT1.LT.(NECDATA-MCTL+1))THEN

C One of the list. Determine if it is a label (icatie is zero), data
C that cannot be edited by the user (icatie is zero) or actual data.

C Debug.
C        write(6,*) 'icatie ',icatie(icout1)

        if(icatie(icout1).eq.0)then
          goto 705
        endif
        ifocus = icatie(icout1)

C Debug.
C        write(6,*) 'ifocus is ',ifocus,icout1

        ll=lnblnk(ddmenuatr(J,ifocus))
        write(outs,'(4a)') 'The current value of ',
     &    ddmenuatr(J,ifocus)(1:ll),' is ',ddatrib(J,ifocus,1)
        call edisp(iuout,outs)
        IF(ddtagatr(J,ifocus,2)(1:4).eq.'intg')THEN

C First read DATA strings into IVMIN/IVMAX/IVAL variables, then edit
C the variable and then write it back to the string ISTR, strip off any
C blanks at the start and assign back to ddatrib.

C << this conversion happens a lot - turn it into a subroutine to de-clutter code >>
C << can we use min and max from the icon database getaniconatr?? >>
          read(ddatrib(J,ifocus,2),*,iostat=ios,ERR=99)IVMIN
          read(ddatrib(J,ifocus,3),*,iostat=ios,ERR=99)IVMAX
          read(ddatrib(J,ifocus,1),*,iostat=ios,ERR=99)IVAL
          read(ddatrib(J,ifocus,1),*,iostat=ios,ERR=99)IVALD
          IF(ddtagatr(J,ifocus,5)(1:4).eq.'user')THEN
            t32=ddmenuatr(J,ifocus)
            CALL EASKI(IVAL,t32,' ',IVMIN,'W',
     &        IVMAX,'W',IVALD,'atrib intg data',IER,nbhelp)
            if(ier.eq.0)then
              write(istr,'(I8)') IVAL
              call removepad(istr,inpstr,ilena,iflag)
              write(ddatrib(J,ifocus,1),'(A)') inpstr
            endif
          ENDIF
        ELSEIF(ddtagatr(J,ifocus,2)(1:4).eq.'real')THEN

C First read DATA strings into VMIN/VMAX/VAL real variables, then edit
C the variable and then write it back to the string VSTR, strip off any
C blanks at the start and assign back to ddatrib.
C << can we use min and max from the icon database getaniconatr?? >>
          read(ddatrib(J,ifocus,2),*,iostat=ios,ERR=99)VMIN
          read(ddatrib(J,ifocus,3),*,iostat=ios,ERR=99)VMAX
          read(ddatrib(J,ifocus,1),*,iostat=ios,ERR=99)VAL
          read(ddatrib(J,ifocus,1),*,iostat=ios,ERR=99)VALD

C If this is a `location` attribute report on the geometric Z difference
C between the node icon and the component icon of this connection.
          if(ddtagatr(J,ifocus,1)(1:8).eq.'location')then
            if(icnisnode(ICNS(J)))then
              delta=XYZICON(ICNE(J),3)-XYZICON(ICNS(J),3)
            else
              delta=XYZICON(ICNS(J),3)-XYZICON(ICNE(J),3)
            endif
            write(outs,'(a,f6.3,a,f6.3)') 
     &        'Reminder: the icon Z difference is ',delta,
     &        ' and the current attribute is ',VAL
            call edisp(iuout,outs)
          endif
          IF(ddtagatr(J,ifocus,5)(1:4).eq.'user')THEN
            t32=ddmenuatr(J,ifocus)
            CALL EASKR(VAL,t32,' ',VMIN,'W',
     &        VMAX,'W',VALD,'atrib real data',IER,nbhelp)
            if(ier.eq.0)then
              call relstr(val,vnpstr,ilena,iflag)
              WRITE(ddatrib(J,ifocus,1),'(a)') vnpstr
            endif
          ENDIF
        ELSEIF(ddtagatr(J,ifocus,2)(1:4).eq.'text')THEN
          VSTR=ddatrib(J,ifocus,1)
          IF(ddtagatr(J,ifocus,5)(1:4).eq.'user')THEN
            t32=ddmenuatr(J,ifocus)
            CALL EASKS(VSTR,t32,' ',12,'nothing',
     &        'atrib text data',IER,nbhelp)
            if(ier.eq.0.and.VSTR(1:2).ne.'  ')then
              call removepad(vstr,vnpstr,ilena,iflag)
              ddatrib(J,ifocus,1)=VNPSTR
            endif
          ENDIF
        ENDIF
      ELSE
        GOTO 705
      ENDIF
      GOTO 705

  99  CALL EDISP(IUOUT,'Error in string formatting NETCNNDATR')
      END

C ********************* NETIDATR ************
C NETIDATR alternative edits domain attributes of an icon.
C This version is for icons with large numbers of items (data is
C presented by category). This is not yet called from network.F.
      SUBROUTINE NETIDATR(ier)
# include "epara.h"
# include "gnetwk.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY 
      common/pophelp/h(60)
      character h*72 

C Icon commons from gnetwk.h      
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)
      LOGICAL ISEL,CSEL
      COMMON/NWKTYP/INWKTYP,vergnf
      integer INWKTYP
      real vergnf

C Summary of external data.
      common/exsum/isexavail,iuex

C CATSI: list of attribute categories (presented for selection).
C NCATIE: number of data fields in each attribute category.
C ICATIE: pointer from DATACATIE bookkeepping back to the 
C         ATRTAG/ATRMENU/ATRICN/ATRTAG arrays.
      DIMENSION CATSI(MIATRC),NCATIE(MIATRC),ICATIE(MIATRC,MIATRB)

C DATCATIE: attribute editing array for the current category. 
C TAGCATIE: ajucent for DATCATIE of the icon attribute tags. 
C IECDATE is menu array for attribute categories.
C IECDATEs is menu arrary for attribute data fields.
C CATIES is menu entry for each data field
      DIMENSION DATCATIE(MIATRC,MIATRB,3),IECDATE(MIATRC+6),
     &CATIES(MIATRC,MIATRB),IECDATEs(30),TAGCATIE(MIATRC,MIATRB,5)
      
C Return up to 5 real valus from external subroutine.
      dimension AVAL(6),SVAL(6)
      character KEY*1,outs*124,SVAL*12
      character VSTR*12,VNPSTR*12,ISTR*12,INPSTR*12,exttag*12
      character stra*12,strb*12

      CHARACTER CATSI*12,IECDATE*40,CATIES*32,
     &IECDATEs*49,DATCATIE*12,TAGCATIE*12

      dimension vert(MICNV,2),iedge(MICNE,5),idot(MICND,4),
     &  iarc(MICND,7),ilabel(MICND,4),labeltx(MICND),text(60),
     &  iatt(MCNP,2)
      character labeltx*4,text*72

C icontoflow() is a pointer from icon to flow node (positive) or flow
C component (negative).
C icnisnode(MNCNN) toggle where .true. means icon index is
C associated with a flow node.
      common/NWKFLW/icontoflow(MNCNN),icnisnode(MNCNN),
     &  inodetoicon(MNCNN),icomptoicon(MNCNN)
      LOGICAL icnisnode

C WORDSS array of string tokens from each NWICHNIS.
      CHARACTER*32 WORDSS(12)
      character ICDNAM*12,category*12,icmenu*36

      LOGICAL MATCH,close,isexavail,ok,dok
      logical closect,closewt,closeft,closev,closex,closey,closez
      integer NECDATA,ICOUT1,NECDATAs,ICOUT2 ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

#ifdef OSI
      integer impx,impy,iwem,iii,iix,iiy,iipx,iipy  ! for use with trackview
#else
      integer*8 impx,impy,iwe,iii,iix,iiy,iipx,iipy ! for use with trackview
#endif

      helpinsub='NETIDATR'  ! set for subroutine

      if(INWKTYP.eq.1.or.INWKTYP.eq.3.or.INWKTYP.eq.6)then
        CALL USRMSG('Click on the component to edit',' ','-')
      elseif(INWKTYP.eq.2)then
        CALL USRMSG('Click on the node/component to edit',' ','-')
      elseif(INWKTYP.eq.4)then
        CALL USRMSG('Click on the control entity to edit',' ','-')
      endif
      CALL TRACKVIEW(iii,iix,iiy)
      IIPX=IIX
      IIPY=IIY
      CALL PIXEL2U(IIPX,IIPY,GX,GY)
      CGX=GX
      CGY=GY
      CLOSEX=.FALSE.
      CLOSEY=.FALSE.
      DO 515 ICN=1,NNICN
        IF(IVIEW.EQ.1)THEN
          CALL ECLOSE(CGX,XYZICON(ICN,1),0.3,CLOSEX)
          CALL ECLOSE(CGY,XYZICON(ICN,2),0.3,CLOSEY)
        ELSEIF(IVIEW.EQ.2)THEN
          CALL ECLOSE(CGX,XYZICON(ICN,1),0.3,CLOSEX)
          CALL ECLOSE(CGY,XYZICON(ICN,3),0.3,CLOSEY)
        ELSE
          CALL ECLOSE(CGX,XYZICON(ICN,2),0.3,CLOSEX)
          CALL ECLOSE(CGY,XYZICON(ICN,3),0.3,CLOSEY)
        ENDIF

        IF(CLOSEX.AND.CLOSEY)THEN

C Clear local storage arrays.
          ISEL(ICN)=.TRUE.
          NCATI=0
          DO 507 J=1,MIATRC
            NCATIE(J)=0
            CATSI(J)=' '
            DO 508 K=1,MIATRB
              ICATIE(J,K)=0
              DO 509 L=1,5
                if(L.le.3)DATCATIE(J,K,L)='  '
                TAGCATIE(J,K,L)='  '
  509         CONTINUE
  508       CONTINUE
  507     CONTINUE

C Decode NWICNHIS() so that getanicon can be called to populate NWICNME.
          call getutokens(NWICNHIS(icn),':',IW,WORDSS)
          write(category,'(a)') WORDSS(2)(1:lnblnk(WORDSS(2)))
          write(ICDNAM,'(a)') WORDSS(3)(1:lnblnk(WORDSS(3)))
          call geticonindex(inwktyp,category,ICDNAM,idbcat,
     &      idbitem,IER)
 
C Get details of icon and then get its attributes (if any).
          if(idbitem.gt.0)then
            call getanicon(inwktyp,category,ICDNAM,nbvert,vert,
     &        nbedge,iedge,nbdot,idot,nblabel,ilabel,labeltx,
     &        nbarc,iarc,nbatt,iatt,nbtext,text,icmenu,IER)
          endif

C Search for unique categories to present to the user.
          DO 520 I=1,NICONATR(ICN)
            MATCH=.FALSE.
            IF(I.NE.1)THEN
              DO 525 J=1,NCATI
                IF(CATSI(J).EQ.ATRTAG(ICN,I,1))THEN
                  MATCH=.TRUE.

C Put the data into an existing category and increment
C the number of entries for this category.
                  NCATIE(J)=NCATIE(J)+1
                  CATIES(NCATI,NCATIE(J))=ATRMENU(ICN,I)

C Set the index for this category entry: relates category data -> icon data.
                  ICATIE(J,NCATIE(J))=I
                  DO 523 L=1,5
                    if(L.le.3)DATCATIE(J,NCATIE(J),L)=ATRICN(ICN,I,L)
                    TAGCATIE(J,NCATIE(J),L)=ATRTAG(ICN,I,L)
  523             CONTINUE
                ENDIF
  525         CONTINUE
            ENDIF

C Put the data into a new category.
            IF(.NOT.MATCH)THEN
              NCATI=NCATI+1
              CATSI(NCATI)=ATRTAG(ICN,I,1)
              NCATIE(NCATI)=NCATIE(NCATI)+1

C Set the index for this category entry: relates category data -> icon data.
              ICATIE(NCATI,NCATIE(NCATI))=I
              CATIES(NCATI,NCATIE(NCATI))=ATRMENU(ICN,I)
              DO 575 L=1,5
              if(L.le.3)DATCATIE(NCATI,NCATIE(NCATI),L)=ATRICN(ICN,I,L)
                TAGCATIE(NCATI,NCATIE(NCATI),L)=ATRTAG(ICN,I,L)
  575         CONTINUE           
            ENDIF
  520     CONTINUE

C Display the category and data menus
  705     continue
C          NEIDATA=0
          NECDATA=0
          MHEAD=6
          MCTL=3
          ILEN=NCATI
          IPACT=CREATE
          CALL EKPAGE(IPACT)

C Initial menu entry setup.
          IER=0
          ILEN=NCATI
          ICOUT1=-3
          WRITE(IECDATE(1),'(2A)') 'a name: ', NWICNM(ICN)
          WRITE(IECDATE(2),'(A)') ' database entry information:'
          WRITE(IECDATE(3),'(2A)') '  ',NWICNHIS(ICN)(1:38)
          WRITE(IECDATE(4),'(A)') 'b view entry notes              '
          WRITE(IECDATE(5),'(A)') '  ______________________________' 
          WRITE(IECDATE(6),'(A)') ' component data categories:'           
          M=MHEAD
          DO 707 L=1,ILEN
            IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
              M=M+1
              CALL EMKEY(M-4,KEY,IER)
              WRITE(IECDATE(M),'(A,1x,A)') Key,CATSI(L)
            endif
  707     CONTINUE

C If a long list include page facility text.      
          IF(IPFLG.EQ.0)THEN  
            IECDATE(M+1)='  ______________________________ '
          ELSE
            WRITE(IECDATE(M+1),15)IPM,MPM 
   15       FORMAT   ('0 page: ',I2,' of ',I2,' --------')
          ENDIF
          WRITE(IECDATE(M+2),'(A)') '? help '
          WRITE(IECDATE(M+3),'(A)') '- exit this menu '
          NECDATA=M+MCTL  
          CALL NETWDRW
          helptopic='component_cat_edits'
          call gethelptext(helpinsub,helptopic,nbhelp)
   42     CALL EMENU('Component data ',IECDATE,NECDATA,ICOUT1)
          IF(ICOUT1.EQ.NECDATA)THEN
           ISEL(ICN)=.FALSE.  ! unhilight the icon on exit
           RETURN
          ELSEIF(ICOUT1.EQ.NECDATA-1)THEN   
            helptopic='component_cat_edits'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL PHELPD('Data categories',nbhelp,'-',0,0,IER)
            goto 705
          ELSEIF(ICOUT1.EQ.NECDATA-2)THEN   

C If there are enough items allow paging control via EKPAGE.
            IF(IPFLG.EQ.1)THEN
              IPACT=EDIT
              CALL EKPAGE(IPACT)
            ENDIF
          ELSEIF(ICOUT1.EQ.1)THEN
C            NAMATCH=0
            CALL NETWDRW
            CALL EASKS(NWICNM(ICN),'Give a (unique) name',
     &        '(with no blank spaces) ',12,NWICNM(ICN),'edtcmp',
     &        IER,nbhelp)

C Check for duplicate names ....
            call dupicname(icn)
          ELSEIF(ICOUT1.EQ.4)THEN

C Text associated with this icon.
            if(nbtext.gt.0)then
              call edisp(iuout,'Component notes: ')
              do 116 jh=1,nbtext
                write(h(jh),'(a)') text(jh)(1:70) 
  116         continue
              CALL PHELPD('Component information',nbtext,'-',0,0,IER)
            endif
            goto 42
          ELSEIF(ICOUT1.GT.MHEAD.AND.ICOUT1.LT.(NECDATA-MCTL+1))THEN

C Record the selected data entry and set up sub-menu with data
C Note use EPMENSV to remember the category menu state.
            CALL KEYIND(NECDATA,ICOUT1,IAST,IO)
            CALL EPMENSV

  718       MHEAD=3
            MCTL=4
            ICOUT2=-1
            ILEN=NCATIE(IAST)
            IPACT=CREATE
            CALL EKPAGE(IPACT)

C Initial menu entry setup.
  792       IER=0
            ILEN=NCATIE(IAST)
            ICOUT2=-3

            WRITE(IECDATEs(1),'(2A)')'  name: ',NWICNM(ICN)
            WRITE(IECDATEs(2),'(2A)')'  category: ',CATSI(IAST) 
            WRITE(IECDATEs(3),'(A)')'  ______________________________' 
            M=MHEAD

C Check for longest length to display. iwla is for caties,
C iwlb is for datcatie, iwa is both plus necessary spaces.
            iwla=0
            iwlb=0
            do 817 L=1,ILEN
              IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
                la=lnblnk(CATIES(IAST,L))
                if(la.gt.iwla) iwla = la
                lb=lnblnk(DATCATIE(IAST,L,1))
                if(lb.gt.iwlb) iwlb = lb
              endif
  817       continue
            iwla=MIN0(30,iwla)
            iwa=(iwla+iwlb+7)
            iw=MAX0(33,iwa)
            DO 717 L=1,ILEN
              IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
                M=M+1
                CALL EMKEY(L,KEY,IER)

C Write out the menu entries depending upon whether data can be edited.
                IF(TAGCATIE(IAST,L,5)(1:4).ne.'stat')THEN
                  WRITE(IECDATEs(M),'(A,1x,3A)') Key,
     &             CATIES(IAST,L)(1:iwla),' ',DATCATIE(IAST,L,1)(1:iwlb)
                else
                  WRITE(IECDATEs(M),'(2x,4A)') CATIES(IAST,L)(1:iwla),
     &             ' ',DATCATIE(IAST,L,1)(1:iwlb),' #'
                endif
              endif
  717       CONTINUE

C If a long list include page facility text.      
            IF(IPFLG.EQ.0)THEN  
              IECDATEs(M+1)='  ______________________________ '
            ELSE
              WRITE(IECDATEs(M+1),15)IPM,MPM 
            ENDIF
            WRITE(IECDATEs(M+2),'(A)')'  cannot edit values marked (#)'
            WRITE(IECDATEs(M+3),'(A)')'? help '
            WRITE(IECDATEs(M+4),'(A)')'- exit this menu '
            NECDATAs=M+MCTL
            ICOUT2=-1
            CALL NETWDRW
            helptopic='component_cat_edits'
            call gethelptext(helpinsub,helptopic,nbhelp)

C Make menu only as wide as it needs to be.
            if(MMOD.eq.8)then
              impx=0
              impy=0
              iwe=iw
              CALL VWMENU('Icon attributes',IECDATEs,NECDATAs,
     &          impx,impy,iwe,irpx,irpy,ICOUT2)
            else
              CALL EMENU('Icon attributes',IECDATEs,NECDATAs,ICOUT2)
            endif
            IF(ICOUT2.EQ.NECDATAs)THEN

C Restore category menu setup before returning to label 705.
              CALL EPMENRC
              GOTO 705
            ELSEIF(ICOUT2.EQ.NECDATAs-1)THEN
              helptopic='component_cat_edits'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL PHELPD('Data values',nbhelp,'-',0,0,IER)
            ELSEIF(ICOUT2.EQ.NECDATAs-3)THEN

C If there are enough items allow paging control via EKPAGE.
              IF(IPFLG.EQ.1)THEN
                IPACT=EDIT
                CALL EKPAGE(IPACT)
              ENDIF
            ELSEIF(ICOUT2.GT.MHEAD.AND.ICOUT2.LT.(NECDATAs-MCTL+1))THEN
              CALL KEYIND(NECDATAs,ICOUT2,ISL,IO)
              write(outs,'(4a)') 'The current value of ',
     &          CATIES(IAST,ISL),' is ',DATCATIE(IAST,ISL,1)
              call edisp(iuout,outs)
              IF(TAGCATIE(IAST,ISL,2)(1:4).eq.'intg')THEN

C First read DATCATIE strings into IVMIN/IVMAX/IVAL variables, then edit
C the variable and then write it back to the string ISTR, strip off any
C blanks at the start and assign back to DATCATIE and ATRICN.
                call catie_int(datcatie,iast,isl,ivmin,ivmax,ival,
     &            ivald,ier)

                IF(TAGCATIE(IAST,ISL,5)(1:4).eq.'user')THEN
                  helptopic='component_cat_edits'
                  call gethelptext(helpinsub,helptopic,nbhelp)
                  CALL EASKI(IVAL,CATIES(IAST,ISL),' ',IVMIN,'W',
     &              IVMAX,'W',IVALD,'atrib intg data',IER,nbhelp)
                  if(ier.eq.0)then
                    write(istr,'(I8)') IVAL
                    call removepad(istr,inpstr,ilena,iflag)
                    write(DATCATIE(IAST,ISL,1),'(A)') inpstr
                    WRITE(ATRICN(ICN,ICATIE(IAST,ISL),1),'(A)') inpstr
                  endif
                ENDIF
              ELSEIF(TAGCATIE(IAST,ISL,2)(1:4).eq.'real')THEN

C First read DATCATIE strings into VMIN/VMAX/VAL real variables. 
                call catie_r(datcatie,iast,isl,vmin,vmax,val,vald,ier)

C If TAGCATIE(IAST,ISL,3) is `external` then call selrelexttag
C based on the search string in TAGCATIE(IAST,ISL,4). If iier
C returned as -1 then no selection made.
                if(TAGCATIE(IAST,ISL,3)(1:8).eq.'external'.and.
     &             TAGCATIE(IAST,ISL,5)(1:4).eq.'user')then
                  if(isexavail)then
                    exttag=TAGCATIE(IAST,ISL,4)
                    call selrelexttag(icn,exttag,AVAL,SVAL,iier)
                    if(iier.eq.-1)then
                      continue
                    elseif(iier.eq.0)then

C If flow network (INWKTYP = 2) and the component is internal unknown
C pressure (DATCATIE(1,1,1) is zero) and the fluid type is air
C (DATCATIE(1,2,1) = 1) then map the returned data as follows:
C AVAL(1) is volume, AVAL(2) is cogX, AVAL(3) is cogY, AVAL(4) is cogZ
C SVAL(1) is zone name.
C *typical entry in gnf*
C flow,intg,-,-,0,0,3,static,flow node type
C flow,intg,-,-,1,0,2,user,fluid type
C flow,text,-,-,20.0,user,temperature or node name
C flow,real,external,*Zones,40.50000,0.0,10000.0,user,volume
C location,real,external,*Zones,1.50000,-10.0,99.0,user,height
C location,text,external,*Zones,manager,user,linked zone name

C Debug.
C                     write(6,*) 'external data search for ',exttag,iast
C                     write(6,*) 'datcatie 1:',DATCATIE(1,1,1),
C     &                 DATCATIE(1,2,1),DATCATIE(1,3,1),
C     &                 DATCATIE(1,4,1),DATCATIE(1,5,1)
C                     write(6,*) 'datcatie 2:',DATCATIE(2,1,1),
C     &                 DATCATIE(2,2,1),DATCATIE(2,3,1),
C     &                 DATCATIE(2,4,1)

                     read(DATCATIE(1,1,1),*,iostat=ios,ERR=99)VALCT
                     read(DATCATIE(1,2,1),*,iostat=ios,ERR=99)VALFT
                     read(DATCATIE(IAST,4,1),*,iostat=ios,ERR=99)VALV
                      CALL ECLOSE(VALCT,0.00,0.01,CLOSECT)
                      CALL ECLOSE(VALCT,3.00,0.01,CLOSEWT)
                      CALL ECLOSE(VALFT,1.00,0.01,CLOSEFT)
                      if(INWKTYP.eq.2.and.CLOSECT.and.CLOSEFT)then

C Debug.
C                        write(6,*) 'Internal unknown pressure comp.'

                        CALL ECLOSE(AVAL(1),VALV,0.001,CLOSEV)
                        CALL ECLOSE(AVAL(2),XYZICON(ICN,1),0.001,CLOSEX)
                        CALL ECLOSE(AVAL(3),XYZICON(ICN,2),0.001,CLOSEY)
                        CALL ECLOSE(AVAL(4),XYZICON(ICN,3),0.001,CLOSEZ)
                        if(.NOT.CLOSEV)then

                          helptopic='network_mismatch_detect'
                          call gethelptext(helpinsub,helptopic,nbhelp)
                          call relstr(AVAL(1),stra,ilena,iflag)
                          call relstr(VALV,strb,ilenb,iflag)
                          write(outs,'(4a)') 'The zone volume ',
     &                      stra(1:ilena),' does not match current ',
     &                      strb(1:ilenb)
                          dok=.true.
                          CALL ASKOK(outs,'Update volume?',
     &                      OK,dok,nbhelp)
                          if(OK)then
                            call relstr(AVAL(1),vnpstr,ilena,iflag)
                            WRITE(DATCATIE(1,4,1),'(a)') vnpstr
                            WRITE(ATRICN(ICN,ICATIE(1,4),1),'(a)')
     &vnpstr
                          endif
                        endif
                        if(.NOT.CLOSEZ)then
                          helptopic='network_mismatch_detect'
                          call gethelptext(helpinsub,helptopic,nbhelp)
                          call relstr(AVAL(4),stra,ilena,iflag)
                          call relstr(XYZICON(ICN,3),strb,ilenb,iflag)
                          write(outs,'(4a)') 'The zone height ',
     &                      stra(1:ilena),' does not match current ',
     &                      strb(1:ilenb)
                          dok=.true.
                          CALL ASKOK(outs,'Update it?',OK,dok,nbhelp)
                          if(OK)then

C For this icon the hieght is in the attribute category `location` which
C is the second attribute category.
                            call relstr(AVAL(4),vnpstr,ilena,iflag)
                            WRITE(DATCATIE(2,1,1),'(a)') vnpstr
                            WRITE(ATRICN(ICN,ICATIE(2,1),1),'(a)')
     &vnpstr
                            XYZICON(ICN,3)=AVAL(4)
                          endif
                        endif
                        if(CLOSEX.and.CLOSEY)then
                          continue
                        else
                          helptopic='network_mismatch_detect'
                          call gethelptext(helpinsub,helptopic,nbhelp)
                          call relstr(AVAL(2),stra,ilena,iflag)
                          call relstr(AVAL(3),strb,ilenb,iflag)
                          write(outs,'(5a,2F8.2)') 'The zone COG X Y ',
     &                      stra(1:ilena),' & ',strb(1:ilenb),
     &                      ' do not match current X Y ',
     &                      XYZICON(ICN,1),XYZICON(ICN,2)
                          dok=.true.
                          CALL ASKOK(outs,'Update them?',OK,dok,nbhelp)
                          if(OK)then

C Store old icon position, find DELTAs between positions.

C Debug.
C       write(6,*) 'for icon ',icn,'current xyz ',XYZICON(ICN,1),
C     &   XYZICON(ICN,2),XYZICON(ICN,3)

                            OLDPOSX=XYZICON(ICN,1)
                            OLDPOSY=XYZICON(ICN,2)
                            OLDPOSZ=XYZICON(ICN,3)
                            DELTAX=0.0
                            DELTAY=0.0
                            DELTAZ=0.0
                            XYZICON(ICN,1)=AVAL(2)
                            XYZICON(ICN,2)=AVAL(3)
                            XYZICON(ICN,3)=AVAL(4)
                            DELTAX=XYZICON(ICN,1)-OLDPOSX
                            DELTAY=XYZICON(ICN,2)-OLDPOSY
                            DELTAZ=XYZICON(ICN,3)-OLDPOSZ

C Debug.
C       write(6,*) 'for icon ',icn,'new xyz ',XYZICON(ICN,1),
C     &   XYZICON(ICN,2),XYZICON(ICN,3),DELTAX,DELTAY,DELTAZ

C Update the icon vertices.
C       write(6,*) 'for icon ',icn,'old vert 1 ',VCICON(ICN,1,1),
C     &   VCICON(ICN,1,2),VCICON(ICN,1,3)

                            DO 10 I=1,NIVC(ICN) 
                              VCICON(ICN,I,1)=VCICON(ICN,I,1)+DELTAX
                              VCICON(ICN,I,2)=VCICON(ICN,I,2)+DELTAY
                              VCICON(ICN,I,3)=VCICON(ICN,I,3)+DELTAZ
  10                        CONTINUE

C Debug.
C       write(6,*) 'for icon ',icn,'new vert 1 ',VCICON(ICN,1,1),
C     &   VCICON(ICN,1,2),VCICON(ICN,1,3)

                            DO 11 I=1,NCONP(ICN)
                              CONCP(ICN,I,1)=CONCP(ICN,I,1)+DELTAX
                              CONCP(ICN,I,2)=CONCP(ICN,I,2)+DELTAY
  11                        CONTINUE
                          endif
                        endif

C Check if the location name is the same as the zone name.
                        if(SVAL(1)(1:12).ne.DATCATIE(2,2,1)(1:12))then
                          write(outs,'(4a)') 
     &                      'Updating associated zone name to ',
     &                      SVAL(1)(1:lnblnk(SVAL(1))),
     &                      ' from ',DATCATIE(2,2,1)
                          call edisp(iuout,outs)
                          WRITE(DATCATIE(2,2,1),'(a)') SVAL(1)
                          WRITE(ATRICN(ICN,ICATIE(2,2),1),'(a)')SVAL(1)
                        endif
                        goto 792
                      elseif(INWKTYP.eq.2.and.CLOSEWT.and.CLOSEFT)then

C If flow network (INWKTYP = 2) and the component is wind pressure
C pressure (DATCATIE(IAST,1,1) is three) and the fluid type is air
C (DATCATIE(IAST,2,1) = 1) then map the returned data as follows:
C *typical entry in gnf*
C  flow,intg,-,-,3,0,3,static,flow node type
C  flow,intg,-,-,1,0,1,static,fluid type
C  flow,real,external,*Pressures,1.0,0.0,100.0,user,pressure coef index
C  flow,real,external,*Surfaces,180.0,0.0,360.0,user,azimuth
C  location,real,external,*Surfaces,1.95000,-10.0,99.0,user,height
C  location,text,external,*Zones,manager,user,linked zone name
C  location,text,external,*Surfaces,glazing,user,linked surface name
                        if(exttag(1:10).eq.'*Pressures')then
C AVAL(1) is pressure coef seta.

C Debug.
C                          write(6,*) 'Wind pressure component PC.'

                          call relstr(AVAL(1),vnpstr,ilena,iflag)
                          WRITE(DATCATIE(1,3,1),'(a)') vnpstr
                          WRITE(ATRICN(ICN,ICATIE(1,3),1),'(a)') vnpstr
                          goto 792
                        elseif(exttag(1:9).eq.'*Surfaces')then

C AVAL(1) is area, AVAL(2) is azi, AVAL(3) is elev, AVAL(4) is Z(m).
C SVAL(1) is zone name SVAL(2) is surface name.

C Debug.
C                          write(6,*) 'Wind pressure component AZ.'

                          call relstr(AVAL(2),vnpstr,ilena,iflag)
                          WRITE(DATCATIE(1,4,1),'(a)') vnpstr
                          WRITE(ATRICN(ICN,ICATIE(1,4),1),'(a)') vnpstr
                          call relstr(AVAL(4),vnpstr,ilena,iflag)
                          WRITE(DATCATIE(2,1,1),'(a)') vnpstr
                          WRITE(ATRICN(ICN,ICATIE(2,1),1),'(a)')vnpstr
                         call eclose(AVAL(4),XYZICON(ICN,3),0.001,close)
                          if(.NOT.close)then
                            XYZICON(ICN,3)=AVAL(4)
                            call edisp(iuout,'Updating icon height.')
                          endif

C Check if the location zone name is the same as the zone name.
                          if(SVAL(1)(1:12).ne.DATCATIE(2,2,1)(1:12))then
                            helptopic='network_mismatch_detect'
                            call gethelptext(helpinsub,helptopic,nbhelp)
                            write(outs,'(4a)')
     &                        'Updating associated zone name to ',
     &                        SVAL(1)(1:lnblnk(SVAL(1))),
     &                        ' from ',DATCATIE(2,2,1)
                            call edisp(iuout,outs)
                            WRITE(DATCATIE(2,2,1),'(a)') SVAL(1)
                            WRITE(ATRICN(ICN,ICATIE(2,2),1),'(a)')
     &                        SVAL(1)
                            WRITE(DATCATIE(2,3,1),'(a)') SVAL(2)
                            WRITE(ATRICN(ICN,ICATIE(2,3),1),'(a)')
     &                        SVAL(2)
                          endif
                          goto 792
                        endif
                      else
                        if(AVAL(1).ge.VMIN.and.AVAL(1).le.VMAX)then
                          VAL=AVAL(1)
                        endif
                      endif
                    endif
                  else
                    helptopic='component_cat_edits'
                    call gethelptext(helpinsub,helptopic,nbhelp)
                    CALL EASKR(VAL,CATIES(IAST,ISL),' ',VMIN,'W',
     &                VMAX,'W',VALD,'atrib real data',IER,nbhelp)
                  endif
                elseif(TAGCATIE(IAST,ISL,3)(1:2).eq.'- '.and.
     &            TAGCATIE(IAST,ISL,5)(1:4).eq.'user')then

C Otherwise edit the variable and then write it back to the string VSTR, strip off any
C blanks at the start and assign back to DATCATIE and ATRICN.
                  helptopic='component_cat_edits'
                  call gethelptext(helpinsub,helptopic,nbhelp)
                  CALL EASKR(VAL,CATIES(IAST,ISL),' ',VMIN,'W',
     &              VMAX,'W',VALD,'atrib real data',IER,nbhelp)
                endif
                if(ier.eq.0)then
                  call relstr(val,vnpstr,ilena,iflag)
                  WRITE(DATCATIE(IAST,ISL,1),'(a)') vnpstr
                  WRITE(ATRICN(ICN,ICATIE(IAST,ISL),1),'(a)') vnpstr

C If this real attribute is `location` then it is assumed to be a height
C attribute so check and update the Z value of the icon to match it.
                  if(TAGCATIE(IAST,ISL,1)(1:8).eq.'location')then
                    call eclose(VAL,XYZICON(ICN,3),0.001,close)
                    if(.NOT.close)then
                      XYZICON(ICN,3)=VAL
                      call edisp(iuout,'Updating icon height.')
                    endif
                  endif
                endif
              ELSEIF(TAGCATIE(IAST,ISL,2)(1:4).eq.'text')THEN
                VSTR=DATCATIE(IAST,ISL,1)
                if(TAGCATIE(IAST,ISL,3)(1:8).eq.'external'.and.
     &             TAGCATIE(IAST,ISL,5)(1:4).eq.'user')then
                   if(isexavail)then
                     exttag=TAGCATIE(IAST,ISL,4)
                     call selrelexttag(icn,exttag,AVAL,SVAL,iier)
                     if(iier.eq.-1)then
                       continue
                     elseif(iier.eq.0)then
                       VSTR=SVAL(1)
                       helptopic='component_cat_edits'
                       call gethelptext(helpinsub,helptopic,nbhelp)
                     CALL EASKS(VSTR,CATIES(IAST,ISL),'(confirm)',12,
     &                'nothing','atrib text data',IER,nbhelp)
                     endif
                   else
                     helptopic='component_cat_edits'
                     call gethelptext(helpinsub,helptopic,nbhelp)
                     CALL EASKS(VSTR,CATIES(IAST,ISL),' ',12,
     &                'nothing','atrib text data',IER,nbhelp)
                   endif
                elseif(TAGCATIE(IAST,ISL,3)(1:2).eq.'- '.and.
     &             TAGCATIE(IAST,ISL,5)(1:4).eq.'user')then
                  helptopic='component_cat_edits'
                  call gethelptext(helpinsub,helptopic,nbhelp)
                  CALL EASKS(VSTR,CATIES(IAST,ISL),' ',12,
     &              'nothing','atrib text data',IER,nbhelp)
                endif
                if(ier.eq.0.and.VSTR(1:2).ne.'  ')then
                  call removepad(vstr,vnpstr,ilena,iflag)
                  DATCATIE(IAST,ISL,1)=VNPSTR
                  ATRICN(ICN,ICATIE(IAST,ISL),1)=VNPSTR
                endif
              ENDIF
            ELSE
              GOTO 792
            ENDIF
            GOTO 718
          ELSE

C Restore category menu setup before returning to label 705.
            CALL EPMENRC
            GOTO 705
          ENDIF
          GOTO 705
        ELSE
          CONTINUE
        ENDIF
  515 CONTINUE

      return

  99  call edisp(iuout,'error composing attribute for editing.')
      return
      end


C ********************* NETCDATR ************
C NETCDATR alternate edit of connection data attributes. This version
C spearates the editing of each category of atrribute.
      SUBROUTINE NETCDATR(IER,IX,IY)

# include "epara.h"
# include "gnetwk.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN

C Icon commons via gnetwk.h
C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)

C Net work view information.
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

C icontoflow() is a pointer from icon to flow node (positive) or flow
C component (negative).
C icnisnode(MNCNN) toggle where .true. means icon index is
C associated with a flow node.
      common/NWKFLW/icontoflow(MNCNN),icnisnode(MNCNN),
     &  inodetoicon(MNCNN),icomptoicon(MNCNN)
      logical icnisnode

C CATSI: list of attribute categories (presented for selection).
C NCATIE: number of data fields in each attribute category.
C ICATIE: pointer from DATACATIE bookkeepping back to the NWKCON arrays.
      DIMENSION CATSI(MIATRC),NCATIE(MIATRC),ICATIE(MIATRC,MIATRB)

C DATCATIE: attribute editing array for the current category. 
C TAGCATIE: ajucent for DATCATIE of the icon attribute tags. 
C IECDATE is menu array for attribute categories.
C IECDATEs is menu arrary for attribute data fields.
C CATIES is menu entry for each data field
      DIMENSION DATCATIE(MIATRC,MIATRB,3),IECDATE(MIATRC+6),
     &CATIES(MIATRC,MIATRB),IECDATEs(30),TAGCATIE(MIATRC,MIATRB,5)

      LOGICAL ISEL,CSEL,MATCH,CLOSEX,CLOSEY

      CHARACTER outs*124,outsd*124,ct*12

C WORDSS and WORDSE are arrays of string tokens from each NWICHNIS.
      CHARACTER*32 WORDSS(12),WORDSE(12)
      character VSTR*12,VNPSTR*12,ISTR*12,INPSTR*12,key*1
      CHARACTER CATSI*12,IECDATE*34,CATIES*32,
     &IECDATEs*49,DATCATIE*12,TAGCATIE*12,t32*32
      integer NECDATA,ICOUT1,NECDATAs,ICOUT2 ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

#ifdef OSI
      integer impx,impy,iwe,iipx,iipy
#else
      integer*8 impx,impy,iwe,iipx,iipy
#endif

C Let the user select the connection to be edited (click on a 
C way-point).

      helpinsub='NETCDATR'  ! set for subroutine
      IIPX=IX
      IIPY=IY
      CALL PIXEL2U(IIPX,IIPY,GX,GY)
      CGX=GX
      CGY=GY
      CLOSEX=.FALSE.
      CLOSEY=.FALSE.
      IMATCH=0
      DO 515 J=1,NICNN
        DO 516 ICWP=1,NCONWP(J)
          IF(IVIEW.EQ.1)THEN
            CALL ECLOSE(CGX,CNWNP(J,ICWP,1),0.2,CLOSEX)
            CALL ECLOSE(CGY,CNWNP(J,ICWP,2),0.2,CLOSEY)
          ELSEIF(IVIEW.EQ.2)THEN
            CALL ECLOSE(CGX,CNWNP(J,ICWP,1),0.2,CLOSEX)
            CALL ECLOSE(CGY,CNWNP(J,ICWP,3),0.2,CLOSEY)
          ELSE
            CALL ECLOSE(CGX,CNWNP(J,ICWP,2),0.2,CLOSEX)
            CALL ECLOSE(CGY,CNWNP(J,ICWP,3),0.2,CLOSEY)
          ENDIF
          IF(CLOSEX.AND.CLOSEY)THEN
            IMATCH=J
            CLOSEX=.FALSE.
            CLOSEY=.FALSE.
          ENDIF

 516    CONTINUE
        IF (IMATCH.GT.0)THEN
          CSEL(J)=.TRUE.
          CALL NETWDRW
          if(icnnt(j).eq.0) ct='none'
          if(icnnt(j).eq.1) ct='air'
          if(icnnt(j).eq.2) ct='water'
          if(icnnt(j).eq.3) ct='steam'
          if(icnnt(j).eq.4) ct='refrigerant'
          if(icnnt(j).eq.5) ct='fuel'
          if(icnnt(j).eq.6) ct='comb_product'
          if(icnnt(j).eq.7) ct='signal'
          lns=lnblnk(NWICNM(ICNS(J)))
          lne=lnblnk(NWICNM(ICNE(J)))
          lhs=lnblnk(NWICNHIS(ICNS(J)))
          lhe=lnblnk(NWICNHIS(ICNE(J)))
          write(outs,'(10a)') 'Between ',NWICNM(ICNS(J))(1:lns),' (',
     &      NWICNHIS(ICNS(J))(1:lhs),') & ',NWICNM(ICNE(J))(1:lne),
     &      ' (',NWICNHIS(ICNE(J))(1:lhe),') via ',ct
          call sdelim(outs,outsd,'S',IW)
          call edisp(iuout,outsd)

C If looking from node to a component (first portion of a network
C flow connection) subtract the node height from the component
C height to get delta. If looking from a component to
C a node (2nd portion of a network flow connection) then subtract
C the node height from the component height.
          if(icnisnode(ICNS(J)))then
            delta=XYZICON(ICNE(J),3)-XYZICON(ICNS(J),3)
          else
            delta=XYZICON(ICNS(J),3)-XYZICON(ICNE(J),3)
          endif
          write(outs,'(3a,f6.3,3a,f6.3,a,f6.3)') 'Height of ',
     &      NWICNM(ICNS(J))(1:lns),' is',XYZICON(ICNS(J),3),
     &      ' & ',NWICNM(ICNE(J))(1:lne),' is ',XYZICON(ICNE(J),3),
     &      ' & delta is ',delta
          call sdelim(outs,outsd,'S',IW)
          call edisp(iuout,outsd)

C Parse details of the start icon via getutokens.
          call getutokens(NWICNHIS(ICNS(J)),':',IW,WORDSS)

C Parse details of the end icon via getutokens. Compare and
C if they match then scan for a matching domain (which will
C indicate attributes of this connection which can be edited).
          call getutokens(NWICNHIS(ICNE(J)),':',IW,WORDSE)
          if(WORDSS(1)(1:4).ne.WORDSE(1)(1:4))then
            call edisp(iuout,
     &        'WARNING: start and end fluids do not match!')
          endif

C Clear the menu structures.
          NCATI=0
          DO 507 JJ=1,MIATRC
            NCATIE(JJ)=0
            CATSI(JJ)=' '
            DO 508 K=1,MIATRB
              ICATIE(JJ,K)=0
              DO 509 L=1,5
                if(L.le.3)DATCATIE(JJ,K,L)='  '
                TAGCATIE(JJ,K,L)='  '
  509         CONTINUE
  508       CONTINUE
  507     CONTINUE

C Build the menu commands. First present a list of connection
C attribute categories.
          if(idatrdom(J).eq.0) goto 30
          do 43 i=1,idatrdom(J)
            MATCH=.FALSE.
            IF(I.NE.1)THEN
              DO 525 JJ=1,NCATI
                IF(CATSI(JJ).EQ.ddtagatr(J,I,1))THEN
                  MATCH=.TRUE.

C Put the data into an existing category and 
C Increment the number of entries for this category
                  NCATIE(JJ)=NCATIE(JJ)+1
                  CATIES(NCATI,NCATIE(JJ))=ddmenuatr(J,I)

C Set the index for this category entry: relates category data -> icon data
                  ICATIE(JJ,NCATIE(JJ))=I
                  DO 523 L=1,5
                    if(L.le.3)DATCATIE(JJ,NCATIE(JJ),L)=ddatrib(J,I,L)
                    TAGCATIE(JJ,NCATIE(JJ),L)=ddtagatr(J,I,L)
  523             CONTINUE
                ENDIF
  525         CONTINUE
            ENDIF

C Put the data into a new category
            IF(.NOT.MATCH)THEN
              NCATI=NCATI+1
              CATSI(NCATI)=ddtagatr(J,I,1)
              NCATIE(NCATI)=NCATIE(NCATI)+1

C Set the index for this category entry: relates category data -> conn data
              ICATIE(NCATI,NCATIE(NCATI))=I
              CATIES(NCATI,NCATIE(NCATI))=ddmenuatr(J,I)
              DO 575 L=1,5
                if(L.le.3)DATCATIE(NCATI,NCATIE(NCATI),L)=ddatrib(J,I,L)
                TAGCATIE(NCATI,NCATIE(NCATI),L)=ddtagatr(J,I,L)
  575         CONTINUE           
            ENDIF
   43     CONTINUE

C Display the category and data menus
  705     NECDATA=0
          MHEAD=6
          MCTL=3
          ILEN=NCATI
          IPACT=CREATE
          CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92     IER=0
          ILEN=NCATI
          ICOUT1=-3
          if(icnnt(j).eq.0) ct='none'
          if(icnnt(j).eq.1) ct='air'
          if(icnnt(j).eq.2) ct='water'
          if(icnnt(j).eq.3) ct='steam'
          if(icnnt(j).eq.4) ct='refrigerant'
          if(icnnt(j).eq.5) ct='fuel'
          if(icnnt(j).eq.6) ct='comb_product'
          if(icnnt(j).eq.7) ct='signal'
          WRITE(IECDATE(1),'(A)') ' connection between: '
          lns=lnblnk(NWICNM(ICNS(J)))
          lhs=lnblnk(NWICNHIS(ICNS(J)))
          write(outs,'(2a)')'start component: ',NWICNM(ICNS(J))(1:lns)
          WRITE(IECDATE(2),'(2A)') '  ',outs(1:32)
          lne=lnblnk(NWICNM(ICNE(J)))
          lhe=lnblnk(NWICNHIS(ICNE(J)))
          write(outs,'(2a)') 'end component: ',NWICNM(ICNE(J))(1:lne)
          WRITE(IECDATE(3),'(2A)')  '  ',outs(1:32)
          WRITE(IECDATE(4),'(2A)')  '  working fluid: ',ct
          WRITE(IECDATE(5),'(A)') '  ______________________________' 
          WRITE(IECDATE(6),'(A)') ' connection data categories:'          
          M=MHEAD
          DO 707 L=1,ILEN
            IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
              M=M+1
              CALL EMKEY(L,KEY,IER)
              WRITE(IECDATE(M),'(A,1x,A)') Key,CATSI(L)
            endif
  707     CONTINUE

C If a long list include page facility text.      
          IF(IPFLG.EQ.0)THEN  
            IECDATE(M+1)='  ______________________________ '
          ELSE
            WRITE(IECDATE(M+1),15)IPM,MPM 
   15       FORMAT   ('0 page: ',I2,' of ',I2,' --------')
          ENDIF
          WRITE(IECDATE(M+2),'(A)') '? help '
          WRITE(IECDATE(M+3),'(A)') '- exit this menu '
          NECDATA=M+MCTL  
          CALL NETWDRW
          helptopic='connection_attributes'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EMENU('Connection attribute categories',IECDATE,
     &      NECDATA,ICOUT1)
          IF(ICOUT1.EQ.NECDATA)THEN
            CSEL(J)=.FALSE.
            RETURN
          ELSEIF(ICOUT1.EQ.NECDATA-1)THEN   
            helptopic='connection_attributes'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL PHELPD('Data categories',nbhelp,'-',0,0,IER)
          ELSEIF(ICOUT1.EQ.NECDATA-2)THEN   

C If there are enough items allow paging control via EKPAGE.
            IF(IPFLG.EQ.1)THEN
              IPACT=EDIT
              CALL EKPAGE(IPACT)
            ENDIF
          ELSEIF(ICOUT1.GT.MHEAD.AND.
     &           ICOUT1.LT.(NECDATA-MCTL+1))THEN

C Record the selected data entry and set up sub-menu with data
C Note use EPMENSV to remember the category menu state.
            CALL KEYIND(NECDATA,ICOUT1,IAST,IO)
            CALL EPMENSV

  718       MHEAD=3
            MCTL=4
            ICOUT2=-1
            ILEN=NCATIE(IAST)
            IPACT=CREATE
            CALL EKPAGE(IPACT)

C Initial menu entry setup.
  792       IER=0
            ILEN=NCATIE(IAST)
            ICOUT2=-3

            lcs=lnblnk(NWICNM(ICNS(J)))
            lce=lnblnk(NWICNM(ICNE(J)))
            write(IECDATEs(1),'(4a)') ' ',NWICNM(ICNS(J))(1:lcs),
     &        ' <-> ',NWICNM(ICNE(J))(1:lce)
            WRITE(IECDATEs(2),'(A,A)')'  category: ',CATSI(IAST) 
            WRITE(IECDATEs(3),'(A)')'  _______________________________' 
            M=MHEAD

C Check for longest length to display. iwla is for caties,
C iwlb is for datcatie, iwa is both plus necessary spaces.
            iwla=0
            iwlb=0
            do 817 L=1,ILEN
              IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
                la=lnblnk(CATIES(IAST,L))
                if(la.gt.iwla) iwla = la
                lb=lnblnk(DATCATIE(IAST,L,1))
                if(lb.gt.iwlb) iwlb = lb
              endif
  817       continue
            iwla=MIN0(30,iwla)
            iwa=(iwla+iwlb+7)
            iw=MAX0(33,iwa)
            DO 717 L=1,ILEN
              IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
                M=M+1
                CALL EMKEY(L,KEY,IER)

C Depending on whether the data is static or user editable.
                IF(TAGCATIE(IAST,L,5)(1:4).eq.'user')THEN
                  WRITE(IECDATEs(M),'(A,1x,3A)') Key,
     &             CATIES(IAST,L)(1:iwla),' ',
     &             DATCATIE(IAST,L,1)(1:iwlb)
                else
                  WRITE(IECDATEs(M),'(2x,4A)') 
     &             CATIES(IAST,L)(1:iwla),' ',
     &             DATCATIE(IAST,L,1)(1:iwlb),' #'
                endif
              endif
  717       CONTINUE

C If a long list include page facility text.      
            IF(IPFLG.EQ.0)THEN  
              IECDATEs(M+1)='  _______________________________'
            ELSE
              WRITE(IECDATEs(M+1),15)IPM,MPM 
            ENDIF
            WRITE(IECDATEs(M+2),'(A)')'  cannot edit this value (#)'
            WRITE(IECDATEs(M+3),'(A)')'? help '
            WRITE(IECDATEs(M+4),'(A)')'- exit this menu '
            NECDATAs=M+MCTL
            ICOUT2=-1
            CALL NETWDRW

C Make menu only as wide as it needs to be.
            helptopic='connection_attributes'
            call gethelptext(helpinsub,helptopic,nbhelp)
            if(MMOD.eq.8)then
              impx=0
              impy=0
              iwe=iw
              CALL VWMENU('Connection attributes',IECDATEs,NECDATAs,
     &          impx,impy,iwe,irpx,irpy,ICOUT2)
            else
              CALL EMENU('Connection attributes',IECDATEs,NECDATAs,
     &          ICOUT2)
            endif
            IF(ICOUT2.EQ.NECDATAs)THEN

C Restore category menu setup before returning to label 705.
              CALL EPMENRC
              GOTO 705
            ELSEIF(ICOUT2.EQ.NECDATAs-1)THEN
              helptopic='connection_attributes'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL PHELPD('Data values',nbhelp,'-',0,0,IER)
            ELSEIF(ICOUT2.EQ.NECDATAs-3)THEN

C If there are enough items allow paging control via EKPAGE.
              IF(IPFLG.EQ.1)THEN
                IPACT=EDIT
                CALL EKPAGE(IPACT)
              ENDIF
            ELSEIF(ICOUT2.GT.MHEAD.AND.
     &             ICOUT2.LT.(NECDATAs-MCTL+1))THEN
              CALL KEYIND(NECDATAs,ICOUT2,ISL,IO)
              IF(TAGCATIE(IAST,ISL,2)(1:4).eq.'intg')THEN

C First read DATCATIE strings into IVMIN/IVMAX/IVAL variables, then edit
C the variable and then write it back to the string ISTR, strip off any
C blanks at the start and assign back to DATCATIE and ATRICN.
                call catie_int(datcatie,iast,isl,ivmin,ivmax,ival,
     &            ivald,ier)
    
                IF(TAGCATIE(IAST,ISL,5)(1:4).eq.'user')THEN
                  t32=CATIES(IAST,ISL)
                  CALL EASKI(IVAL,t32,' ',IVMIN,'W',
     &              IVMAX,'W',IVALD,'atrib intg data',IER,nbhelp)
                  if(ier.eq.0)then
                    write(istr,'(I8)') IVAL
                    call removepad(istr,inpstr,ilena,iflag)
                    write(DATCATIE(IAST,ISL,1),'(A)') inpstr
                    write(ddatrib(J,ICATIE(IAST,ISL),1),'(A)') inpstr
                  endif
                ENDIF
              ELSEIF(TAGCATIE(IAST,ISL,2)(1:4).eq.'real')THEN

C First read DATCATIE strings into VMIN/VMAX/VAL real variables, then edit
C the variable and then write it back to the string VSTR, strip off any
C blanks at the start and assign back to DATCATIE and ATRICN.
                call catie_r(datcatie,iast,isl,vmin,vmax,val,vald,ier)

C If this is a `location` attribute report on the geometric Z difference
C between the node icon and the component icon of this connection.
                if(TAGCATIE(IAST,ISL,1)(1:8).eq.'location')then
                  if(icnisnode(ICNS(J)))then
                    delta=XYZICON(ICNE(J),3)-XYZICON(ICNS(J),3)
                  else
                    delta=XYZICON(ICNS(J),3)-XYZICON(ICNE(J),3)
                  endif
                  write(outs,'(a,f6.3,a,f6.3)') 
     &              'Reminder: the icon Z difference is ',delta,
     &              ' and the current attribute is ',VAL
                  call edisp(iuout,outs)
                endif
                IF(TAGCATIE(IAST,ISL,5)(1:4).eq.'user')THEN
                  t32=CATIES(IAST,ISL)
                  CALL EASKR(VAL,t32,' ',VMIN,'W',
     &              VMAX,'W',VALD,'atrib real data',IER,nbhelp)
                  if(ier.eq.0)then
                    call relstr(val,vnpstr,ilena,iflag)
                    WRITE(DATCATIE(IAST,ISL,1),'(a)') vnpstr
                    WRITE(ddatrib(J,ICATIE(IAST,ISL),1),'(a)') vnpstr
                  endif
                ENDIF
              ELSEIF(TAGCATIE(IAST,ISL,2)(1:4).eq.'text')THEN
                VSTR=DATCATIE(IAST,ISL,1)
                IF(TAGCATIE(IAST,ISL,5)(1:4).eq.'user')THEN
                  t32=CATIES(IAST,ISL)
                  CALL EASKS(VSTR,t32,' ',12,'nothing',
     &              'atrib text data',IER,nbhelp)
                  if(ier.eq.0.and.VSTR(1:2).ne.'  ')then
                    call removepad(vstr,vnpstr,ilena,iflag)
                    DATCATIE(IAST,ISL,1)=VNPSTR
                    ddatrib(J,ICATIE(IAST,ISL),1)=VNPSTR
                  endif
                ENDIF
              ENDIF
            ELSE
              GOTO 792
            ENDIF
            GOTO 718
          ELSE

C Restore category menu setup before returning to label 705.
            CALL EPMENRC
            GOTO 705
          ENDIF
          GOTO 92
  30      continue
        ENDIF        


 515  CONTINUE  

      RETURN
      END

C Notes on a typical flow component entry of icons database. There is
C the possibility of a *control line with a real variable. Not yet
C used for anything.
C *Item,mf210,general flow conduit MF210     # icon tag and menu entry
C *Attribute   # nb attributes and data
C *flow,intg,-,-,210,0,500,static,flow component index
C *flow,intg,-,-,1,0,2,user,fluid type
C *flow,real,-,-,0.20,0.0,100.0,user,roughness (mm)
C *flow,real,-,-,0.20,0.0,100.0,user,conduit length (m)
C *flow,real,-,-,0.20,0.0,100.0,user,hydraulic diameter (m)
C *flow,real,-,-,0.20,0.0,100.0,user,cross-sectional area (m^")
C *flow,real,-,-,0.20,0.0,100.0,user,local loss factor (-)
C *location,real,-,-,1.00,-10.0,99.0,user,height (m)
C *control,real,-,-,0.0,0.0,1.0,user,default control state
C *End_attribute
C *Text
C General flow conduit (MF210) component.
C  m = rho.f(D,A,L,k,SCi)
C  with attributes:
C  - fluid type (1=air, 2=water)
C  - conduit hydraulic diameter (m)
C  - cross-sectional area (m^2)
C  - conduit length (m)
C  - absolute wall roughness (m)
C  - sum of local dynamic loss factors (-)
C *End_Text


C ********************* ICONREPLACE *********************************
C ICONREPLACE allows the user to select replacement icon data for
C an existing icon. It is a subset of NETWMIC. ICURRENT is the
C index of the existing icon.

      SUBROUTINE ICONREPLACE(icurrent)

#include "gnetwk.h"
#include "epara.h"
      
      integer lnblnk  ! function definition

C Parameter
      integer icurrent  ! index of current icon

C Icon commons are in gnetwk.h       
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/NWKTYP/INWKTYP,vergnf
      integer INWKTYP
      real vergnf 
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKTYPSTR(MNWKTYP)
      CHARACTER NWKNAM*72,NWKDSC*72,NWKTYPSTR*12

C Common for component/connection toggle
      COMMON/NWKGTOG/ITOG

      dimension iatrdom(MNWKTYP),dtagatr(MNWKTYP,MIATRB,5),
     &  datrib(MNWKTYP,MIATRB,3),dmenuatr(MNWKTYP,MIATRB)
      dimension igatrdom(MNWKTYP),dgtagatr(MNWKTYP,MIATRB,5),
     &  dgatrib(MNWKTYP,MIATRB,3),dgmenuatr(MNWKTYP,MIATRB)
      dimension vert(MICNV,2),iedge(MICNE,5),idot(MICND,4),
     &  iarc(MICND,7),ilabel(MICND,4),labeltx(MICND),text(60),
     &  iatt(MCNP,2)

C ICITEM1 is ...
C ICITEM2 is ...
      DIMENSION ICITEM1(MICNCAT+3),ICITEM2(MICN+4)
      
      CHARACTER ICDNAM*12,ICMENU*36
      CHARACTER ICITEM1*44,ICITEM2*44, KEY*1
      character iconphrase*40,category*12
      character labeltx*4,text*72
      character tagatr*12,menuatr*32,atrib*12
      dimension tagatr(MIATRB,5),atrib(MIATRB,3),menuatr(MIATRB)
      character dtagatr*12,dmenuatr*32,datrib*12
      character dgtagatr*12,dgmenuatr*32,dgatrib*12

      real cgx,cgy

      LOGICAL MREPT,found
      integer NICITEM,IMOUT,NICITEM2,IMOUT2 ! max items and current menu item
      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='ICONREPLACE'  ! set for subroutine

C Set the component reporting flag.
      MREPT=.FALSE.
   
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.

C Merge icon data by asking the user to select from the
C icon database. First recover existing categories.
      CALL USRMSG('Please select from list:',' ','-')
  8   call scanicondb(nbdomain,verdomain,iatrdom,dtagatr,
     &  datrib,dmenuatr,igatrdom,dgtagatr,dgatrib,dgmenuatr,IER)
      IF(IER.GT.0) GOTO 999
      found=.false.
      if(nbdomain.ge.1)then
        do 42 id=1,nbdomain
          if(idomain(id).eq.inwktyp)then
            found=.true.
            idf=id
          endif
  42    continue
      endif

      if(.NOT.found) goto 999
      id=idf
      NCAT=nbcat(id)  

C Set up a menu with the available categories.   
  717 ILEN=NCAT
      MHEAD=3
      MCTL=3   
      IPACT=CREATE
      CALL EKPAGE(IPACT)

      ILEN=NCAT
      WRITE(ICITEM1(1),'(A,A)') ' network domain: ',NWKTYPSTR(INWKTYP)
      WRITE(ICITEM1(2),'(A)') ' available categories: '
      WRITE(ICITEM1(3),'(A)') '  ______________________________ '

C If this is a flow domain and focused on nodes only accept the category
C with tags `inodes` or `bnodes`. If focused on components display all
C other categories.
      M=MHEAD
      DO 17 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          if(INWKTYP.eq.2.and.ITOG.eq.0)then
            if(cattag(id,L)(1:6).eq.'inodes'.or.
     &         cattag(id,L)(1:6).eq.'bnodes')then
               M=M+1   ! found a node category
               CALL EMKEY(L,KEY,IER)
               WRITE(ICITEM1(M),'(A,1X,A)') key,catmenu(id,L)(1:31)
            endif
          elseif(INWKTYP.eq.2.and.ITOG.eq.1)then
            if(cattag(id,L)(1:6).eq.'inodes'.or.
     &         cattag(id,L)(1:6).eq.'bnodes')then 
               M=M+1     ! mark as not applicable
               CALL EMKEY(L,KEY,IER)
               WRITE(ICITEM1(M),'(A)') '  Not applicable'
            else
               M=M+1     ! found a non-node category
               CALL EMKEY(L,KEY,IER)
               WRITE(ICITEM1(M),'(A,1X,A)') key,catmenu(id,L)(1:31)
            endif
          else
            M=M+1   ! not a flow domain so present all categories
            CALL EMKEY(L,KEY,IER)
            WRITE(ICITEM1(M),'(A,1X,A)') key,catmenu(id,L)(1:31)
          endif
        ENDIF
  17  CONTINUE

C If there is a long list then allow paging.
      IF(IPFLG.EQ.0)THEN  
        ICITEM1(M+1)='  ______________________________ '
      ELSE
        WRITE(ICITEM1(M+1),109)IPM,MPM 
  109   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      ICITEM1(M+2)='? help'
      ICITEM1(M+3)='- exit this menu'
      NICITEM=M+MCTL
      IMOUT=-1
      CALL NETWDRW
  9   CALL EMENU('Choose a category:',ICITEM1,NICITEM,IMOUT)
      IF(IMOUT.GT.MHEAD.AND.IMOUT.LE.NICITEM-MCTL+1) THEN
        CALL KEYIND(NICITEM,IMOUT,ICHOSE,IO)

C Store the number of the selected icons category. If nothing selected
C then check again.
        NIT=ICHOSE
        if(NIT.eq.0)goto 9
        CALL EPMENSV
      ELSEIF(IMOUT.EQ.NICITEM-1) then
        helptopic='icon_categories'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Icon Category',nbhelp,'-',0,0,IER)
        GOTO 9
      ELSEIF(IMOUT.EQ.NICITEM) then
        RETURN
      ENDIF  

C Show components in the chosen category.
      MHEAD=3
      MCTL=4
      IMOUT2=-1
      IER=0
      ILEN=nbicons(id,NIT)
      IMOUT2=-1
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      WRITE(ICITEM2(1),'(A,1X,A)') ' category: ',catmenu(id,NIT)
      if(INWKTYP.eq.2.and.ITOG.eq.0)then
        WRITE(ICITEM2(2),'(A)')' select a node: '
      elseif(INWKTYP.eq.2.and.ITOG.eq.1)then
        WRITE(ICITEM2(2),'(A)')' select a component: '
      else
        WRITE(ICITEM2(2),'(A)')' select a component: '
      endif
      WRITE(ICITEM2(3),'(A)')'  ______________________________ '
 11   M=MHEAD
      ILEN=nbicons(id,NIT)
      IMOUT2=-1
      DO 12 J=1,ILEN
        IF(J.GE.IST.AND.(J.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(J,KEY,IER)
          WRITE(ICITEM2(M),'(A,1X,A)')key,iconmenu(id,NIT,J)(1:40)
        ENDIF
 12   CONTINUE
      IF(IER.GT.0) GOTO 8
 14   IF(M.GT.0) THEN

C If a long list include page facility text.      
        IF(IPFLG.EQ.0)THEN  
          ICITEM2(M+1)='  ______________________________ '
        ELSE
          WRITE(ICITEM2(M+1),108)IPM,MPM 
 108      FORMAT   ('0 page: ',I2,' of ',I2,' --------')
        ENDIF
        IF(MREPT) THEN
          if(INWKTYP.eq.2.and.ITOG.eq.0)then
            ICITEM2(M+2)='>> node info ON         '
          elseif(INWKTYP.eq.2.and.ITOG.eq.1)then
            ICITEM2(M+2)='>> component info ON    '
          else
            ICITEM2(M+2)='>> component info ON    '
          endif
        ELSE
          if(INWKTYP.eq.2.and.ITOG.eq.0)then
            ICITEM2(M+2)='>> node info OFF        '
          elseif(INWKTYP.eq.2.and.ITOG.eq.1)then
            ICITEM2(M+2)='>> component info OFF   '
          else
            ICITEM2(M+2)='>> component info OFF   '
          endif
        ENDIF
        ICITEM2(M+3)  ='? help                  '
        ICITEM2(M+4)  ='- exit this menu        '
        NICITEM2=M+MCTL

        IMOUT2=-1   
        CALL NETWDRW   
 13     continue
        if(INWKTYP.eq.2.and.ITOG.eq.0)then
          CALL EMENU('Nodes in category',ICITEM2,NICITEM2,IMOUT2)
        elseif(INWKTYP.eq.2.and.ITOG.eq.1)then
          CALL EMENU('Components in category',ICITEM2,NICITEM2,IMOUT2)
        else
          CALL EMENU('Components in category',ICITEM2,NICITEM2,IMOUT2)
        endif

        IF(IMOUT2.EQ.NICITEM2) THEN

C return to the previous menu ..
          GOTO 717
        ELSEIF(IMOUT2.EQ.NICITEM2-1) THEN
          helptopic='icon_cat_components'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('Component',nbhelp,'-',0,0,IER)
          GOTO 13
        ELSEIF(IMOUT2.EQ.NICITEM2-2) THEN
          IF(MREPT) THEN
            MREPT=.FALSE.
          ELSE
            MREPT=.TRUE.
          ENDIF
          GOTO 14
        ELSEIF(IMOUT2.EQ.NICITEM2-3) THEN

C If there are enough items allow paging control via EKPAGE.
          IF(IPFLG.EQ.1)THEN
            IPACT=EDIT
            CALL EKPAGE(IPACT)
          ENDIF
        ELSEIF(IMOUT2.GT.MHEAD.AND.IMOUT2.LT.NICITEM2-MCTL+1)THEN
          CALL KEYIND(NICITEM2,IMOUT2,ICHOSE2,IO)

C Store the number of the selected icons category.
          IVAL2=ICHOSE2

C Store the name of the selected string, compose NWICNHIS from the
C icon's original domain:catetory:name and get the data overwriting
C the current icons information (other than its name).
          category=cattag(id,NIT)
          ICDNAM=icontag(id,NIT,IVAL2)
          l2=lnblnk(NWKTYPSTR(INWKTYP))
          l3=lnblnk(category)
          l4=lnblnk(ICDNAM)
          write(NWICNHIS(icurrent),'(5a)') NWKTYPSTR(INWKTYP)(1:l2),':',
     &      category(1:l3),':',ICDNAM(1:l4)

C Check if there is a matching entity.
          call geticonindex(inwktyp,category,ICDNAM,idbcat,idbitem,
     &      IER)
 
C Get details of icon and then get its attributes (if any).
          if(idbitem.gt.0)then 
            call getanicon(inwktyp,category,ICDNAM,nbvert,vert,
     &        nbedge,iedge,nbdot,idot,nblabel,ilabel,labeltx,
     &        nbarc,iarc,nbatt,iatt,nbtext,text,icmenu,IER)
          else
            call usrmsg('Could not locate a matching entity',
     &        'in the icon database.','W')
          endif

          write(NWICNME(icurrent),'(a)') icmenu(1:36)
    
C Debug.
          write(6,*) nbvert,nbedge,nbdot,nblabel,nbarc,nbatt,nbtext
C          write(6,*) iedge
C          write(6,*) idot
C          write(6,*) ilabel
C          write(6,*) inwktyp,category
   
          call geticonatr(inwktyp,category,ICDNAM,natrib,
     &      tagatr,atrib,menuatr,iconphrase,IER)
     
C Debug.
C         write(6,*) ICDNAM 
C         write(6,*) natrib 
C         write(6,*) tagatr 
C         write(6,*) atrib 
         write(6,*) iconphrase 

C For (natrib times) extract attributes of the icon (tags, values,
C menu entries). Note atrib can have up to MIATRB attributes. Also
C remember (most) of the descriptive phrase from the selection list.
          NICONATR(icurrent)=natrib
          write(NWICNME(icurrent),'(a)')iconphrase(1:36)
          if(natrib.ge.1)then
            do 39 IV=1,NICONATR(icurrent)
              ATRTAG(icurrent,IV,1)=tagatr(IV,1)
              ATRTAG(icurrent,IV,2)=tagatr(IV,2)
              ATRTAG(icurrent,IV,3)=tagatr(IV,3)
              ATRTAG(icurrent,IV,4)=tagatr(IV,4)
              ATRTAG(icurrent,IV,5)=tagatr(IV,5)
              ATRICN(icurrent,IV,1)=atrib(IV,1)
              ATRICN(icurrent,IV,2)=atrib(IV,2)
              ATRICN(icurrent,IV,3)=atrib(IV,3)
              ATRMENU(icurrent,IV)=menuatr(IV)
  39        continue
          endif

C For (nbvert times) extract vert data.
          NIVC(icurrent)=nbvert
          DO 40 IV=1,NIVC(icurrent)
            VCICON(icurrent,IV,1)=vert(IV,1)*XSIZIC
            VCICON(icurrent,IV,2)=vert(IV,2)*YSIZIC 
  40      CONTINUE

C For (nbedge times) extract edge vertex indices colour index, colour
C type, line type.
          NIVE(icurrent)=nbedge
          if(nbedge.ge.1)then
            DO 41 IE=1,NIVE(icurrent)
              IVEICN(icurrent,IE,1)=iedge(IE,1)
              IVEICN(icurrent,IE,2)=iedge(IE,2)
              IVEICN(icurrent,IE,3)=iedge(IE,3)
              IVEICN(icurrent,IE,4)=iedge(IE,4)
              IVEICN(icurrent,IE,5)=iedge(IE,5)
  41        CONTINUE
          endif
  
C For nbdot times extract dot vertex index, colour index and size
          NIVD(icurrent)=nbdot
          if(nbdot.ge.1)then
            do 143 IE=1,NIVD(icurrent)
              IVDOT(icurrent,IE,1)=idot(IE,1)
              IVDOT(icurrent,IE,2)=idot(IE,2)
              IVDOT(icurrent,IE,3)=idot(IE,3)
              IVDOT(icurrent,IE,4)=idot(IE,4)
 143        continue
          endif

C For nbarc times extract arc centre vertex index, radiaus vertex index,
C 1st and 2nd angles, colour index and fill type
          NIVA(icurrent)=nbarc
          if(nbarc.ge.1)then
            do 145 IE=1,NIVA(icurrent)
              IVARC(icurrent,IE,1)=iarc(IE,1)
              IVARC(icurrent,IE,2)=iarc(IE,2)
              IVARC(icurrent,IE,3)=iarc(IE,3)
              IVARC(icurrent,IE,4)=iarc(IE,4)
              IVARC(icurrent,IE,5)=iarc(IE,5)
              IVARC(icurrent,IE,6)=iarc(IE,6)
              IVARC(icurrent,IE,7)=iarc(IE,7)
 145        continue
          endif
  
C For nblabel times extract internal label vertex index, colour index and text
          NIVL(icurrent)=nblabel
          if(nblabel.ge.1)then
            do 144 IE=1,NIVL(icurrent)
              IVLBL(icurrent,IE,1)=ilabel(IE,1)
              IVLBL(icurrent,IE,2)=ilabel(IE,2)
              IVLBL(icurrent,IE,3)=ilabel(IE,3)
              NWICNLBL(icurrent,IE)=labeltx(IE)
 144        continue
          endif

C For (nbatt times) extract attachment point vertex X and Y. Icc
C is the index of the vertex associated with the attachment point.
C Also set the type of connection (ICNCT)
          NCONP(icurrent)=nbatt
          DO 142 IC=1,NCONP(icurrent)
            iic=iatt(ic,1)
            CONCP(icurrent,IC,1)=vert(iic,1)*XSIZIC
            CONCP(icurrent,IC,2)=vert(iic,2)*YSIZIC
            ICNCT(icurrent,IC)=iatt(ic,2)
 142      CONTINUE      
        ELSE
          IMOUT2=-1
          GOTO 11   
        ENDIF
      ELSE
        IMOUT=-1 
        GOTO 717
      ENDIF

C Find the current centere of the icon.
      CGX=XYZICON(icurrent,1)
      CGY=XYZICON(icurrent,2)

C Translate the component image to the icon centre.
      DO 100 IV=1,NIVC(icurrent)
        VCICON(icurrent,IV,1)=CGX+VCICON(icurrent,IV,1)-XSIZIC/2.
        VCICON(icurrent,IV,2)=CGY+VCICON(icurrent,IV,2)-YSIZIC/2.
 100  CONTINUE  

      DO 110 IC=1,NCONP(icurrent)
        CONCP(icurrent,IC,1)=CGX+CONCP(icurrent,IC,1)-XSIZIC/2.
        CONCP(icurrent,IC,2)=CGY+CONCP(icurrent,IC,2)-YSIZIC/2.
 110  CONTINUE

      if(iglib.eq.1.or.iglib.eq.2)then
        CALL NETWDRW
      endif

      RETURN

999   CALL EDISP(IUOUT,'ERROR: problem opening icon database, aborting')
      RETURN
      
      END 

