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

C Icon commons       
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)

      COMMON/NWKICS/NWICNM(MICN),NWICNHIS(MICN),NWICNLBL(MICN,MICND),
     & NWICNTXT(MICN,60) 

C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)
      COMMON/NWKTYP/INWKTYP,vergnf 

C Summary of external data.
      common/exsum/isexavail,iuex

C ICATIE: pointer from menu  back to the ATRTAG/ATRMENU/ATRICN/ATRTAG arrays.
      DIMENSION ICATIE(MIATRB)
  
C IECDATE is menu array for attribute data
      DIMENSION IECDATE(MIATRC+10)
      
C Return up to 6 real valus from external subroutine.
      dimension AVAL(6),SVAL(6)
      CHARACTER NWICNM*12,NWICNHIS*40,NWICNLBL*4
      character NWICNTXT*72,KEY*1,outs*124,SVAL*12
      character ATRTAG*12,ATRMENU*32,ATRICN*12
      character VSTR*12,VNPSTR*12,ISTR*12,INPSTR*12,exttag*12
      character lasttag*12

      CHARACTER IECDATE*40

      LOGICAL ISEL,CSEL,close,isexavail
      logical closect,closewt,closeft
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

      helpinsub='NETICONDATR'  ! set for subroutine

C Clear local storage arrays.
      icn=icnpick
      ISEL(ICN)=.TRUE.
      NCATI=0
      DO 507 J=1,MIATRC
        ICATIE(J)=0
  507 CONTINUE

C Display any text associated with this icon.
      if(NIVT(ICN).gt.0)then
        do 410 jh=1,NIVT(ICN)
          call edisp(iuout,NWICNTXT(ICN,jh))
  410   continue
      endif

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

C Display the attributes for this icon. Put a label line at the beginning
C of each category to separate the data. The logic below assumes that the
C attributes of each catagory are grouped together.
  705 continue
      NECDATA=0
      MHEAD=4
      MCTL=3

C The length is the number of attributes plus one line for each different
C category.
      ILEN=NICONATR(ICN) + ncati
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
  706 IER=0
      ILEN=NICONATR(ICN) + ncati
      ICOUT1=-3
      WRITE(IECDATE(1),'(2A)') 'a name: ', NWICNM(ICN)
      WRITE(IECDATE(2),'(A)')  ' database entry information:'
      WRITE(IECDATE(3),'(2A)') '  ',NWICNHIS(ICN)(1:38)
      WRITE(IECDATE(4),'(A)')  'b view component notes          '          
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
C attribute to include in the menu otherwise beging a new category
C count.
            if(lasttag.eq.atrtag(icn,latrindex,1))then

C Write attributes name and data. Set icatie for
C menu item M to latrindex for use later. Also adjust the menu to reflect
C whether the item can be edited by the user.
              la=lnblnk(ATRMENU(ICN,latrindex))
              lb=lnblnk(ATRICN(ICN,latrindex,1))
              if(ATRTAG(ICN,latrindex,5)(1:4).ne.'stat')THEN
                WRITE(IECDATE(M),'(A,2x,A,1x,A)') Key,
     &            ATRMENU(ICN,latrindex)(1:la),
     &            ATRICN(ICN,latrindex,1)(1:lb)
                icatie(m)=latrindex
                latrindex=latrindex+1
              else
                WRITE(IECDATE(M),'(3x,A,1x,A)') 
     &            ATRMENU(ICN,latrindex)(1:la),
     &            ATRICN(ICN,latrindex,1)(1:lb)
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
        IECDATE(M+1)='  ______________________________ '
      ELSE
        WRITE(IECDATE(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
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
        CALL VWMENU('Component data',IECDATE,NECDATA,impx,impy,iwe,
     &    irpx,irpy,ICOUT1)
      else
        CALL EMENU('Component data',IECDATE,NECDATA,ICOUT1)
      endif
      IF(ICOUT1.EQ.NECDATA)THEN
        RETURN
      ELSEIF(ICOUT1.EQ.NECDATA-1)THEN   
        helptopic='component_data_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Data categories',7,'-',0,0,IER)
      ELSEIF(ICOUT1.EQ.NECDATA-2)THEN   

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(ICOUT1.EQ.1)THEN
        CALL NETWDRW
        CALL EASKS(NWICNM(ICN),'Give a (unique) name',
     &    '(with no blank spaces) ',12,NWICNM(ICN),'edtcmp',
     &    IER,nbhelp)

C Check for duplicate names ....
        call dupicname(icn)
      ELSEIF(ICOUT1.EQ.4)THEN

C Text associated with this icon. If allowed, edit.
C << get code from dbm >>
        if(NIVT(ICN).gt.0)then
          call edisp(iuout,'Component notes: ')
          do 116 jh=1,NIVT(ICN)
            call edisp(iuout,NWICNTXT(ICN,jh))
  116     continue
        endif
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

C First read DATA strings into IVMIN/IVMAX/IVAL variables, then edit
C the variable and then write it back to the string ISTR, strip off any
C blanks at the start and assign back to DATA and ATRICN.
          read(ATRICN(icn,ifocus,2),*,iostat=ios,ERR=99)IVMIN
          read(ATRICN(icn,ifocus,3),*,iostat=ios,ERR=99)IVMAX
          read(ATRICN(icn,ifocus,1),*,iostat=ios,ERR=99)IVAL
          read(ATRICN(icn,ifocus,1),*,iostat=ios,ERR=99)IVALD
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

C First read DATA strings into VMIN/VMAX/VAL real variables. 
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
              call selrelexttag(exttag,AVAL,SVAL,iier)
              call epmenrc
              if(iier.eq.-1)then
                continue
              elseif(iier.eq.0)then

C If flow network (INWKTYP = 2) and the component is internal unknown
C pressure (ATRICN(icn,1,1) is zero) and the fluid type is air
C (ATRICN(icn,2,1) = 1) then import summary atrributes.

C Debug.
C                write(6,*) 'external data search for ',exttag,ifocus
C                write(6,*) 'data 1:',ATRICN(icn,1,1),ATRICN(icn,2,1),
C     &                ATRICN(icn,3,1),ATRICN(icn,4,1),ATRICN(icn,5,1)

                read(ATRICN(icn,1,1),*,iostat=ios,ERR=99)VALCT
                read(ATRICN(icn,2,1),*,iostat=ios,ERR=99)VALFT
                read(ATRICN(icn,4,1),*,iostat=ios,ERR=99)VALV
                CALL ECLOSE(VALCT,0.00,0.01,CLOSECT)
                CALL ECLOSE(VALCT,3.00,0.01,CLOSEWT)
                CALL ECLOSE(VALFT,1.00,0.01,CLOSEFT)
                if(INWKTYP.eq.2.and.CLOSECT.and.CLOSEFT)then

C Debug.
C                  write(6,*) 'Internal unknown pressure comp.'

                  call updintunknflow(icn,AVAL,SVAL,VALV)
                  goto 706
                elseif(INWKTYP.eq.2.and.CLOSEWT.and.CLOSEFT)then

C If flow network (INWKTYP = 2) and the component is wind pressure
C pressure the update attributes.
                  call updwindflow(icn,exttag,AVAL,SVAL,VALV)
                  goto 706
                else
                  if(AVAL(1).ge.VMIN.and.AVAL(1).le.VMAX)then
                    VAL=AVAL(1)
                  endif
                endif
              endif
            else
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
              call eclose(VAL,XYZICON(ICN,3),0.01,close)
              if(.NOT.close)then
                XYZICON(ICN,3)=VAL
                call edisp(iuout,'Updating icon height.')
              endif
            endif
          endif
        ELSEIF(atrtag(icn,ifocus,2)(1:4).eq.'text')THEN
          VSTR=ATRICN(ICN,ifocus,1)
          if(atrtag(icn,ifocus,3)(1:8).eq.'external'.and.
     &       atrtag(icn,ifocus,5)(1:4).eq.'user')then
             if(isexavail)then
               exttag=atrtag(icn,ifocus,4)
               call epmensv
               call selrelexttag(exttag,AVAL,SVAL,iier)
               call epmenrc
               if(iier.eq.-1)then
                 continue
               elseif(iier.eq.0)then
                 VSTR=SVAL(1)
                 CALL EASKS(VSTR,ATRMENU(ICN,ifocus),'(confirm)',12,
     &            'nothing','atrib text data',IER,nbhelp)
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

C Icon commons       
      common/outin/iuout,iuin
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)

C User up to 6 real valus from external subroutine.
      dimension AVAL(6),SVAL(6)
      character outs*124,SVAL*12,ATRTAG*12,ATRMENU*32,ATRICN*12
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
      CALL ECLOSE(AVAL(1),VALV,0.01,CLOSEV)
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

C Check if the location name is the same as the zone name. << ?2 >>
      if(SVAL(1)(1:12).ne.ATRICN(ICN,6,1)(1:12))then
        write(outs,'(4a)') 'The zone name ',
     &    SVAL(1)(1:lnblnk(SVAL(1))),' do not match current atrribute ',
     &    ATRICN(ICN,6,1)
        dok=.true.
        CALL ASKOK(outs,'Update name?',OK,dok,nbhelp)
        if(OK)then
          WRITE(ATRICN(ICN,6,1),'(a)')SVAL(1)
        endif
      endif

      return
      end

C ************** updwindflow
C updwindflow update attributes of boundary wind pressure flow node
      subroutine updwindflow(icn,exttag,AVAL,SVAL,VALV)
# include "gnetwk.h"
      
      integer lnblnk  ! function definition

C Icon commons       
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)

C User up to 6 real valus from external subroutine.
      dimension AVAL(6),SVAL(6)
      character outs*124,SVAL*12,ATRTAG*12,ATRMENU*32,ATRICN*12
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
C  location,text,-,-,manager,user,linked zone name
C  location,text,-,-,glazing,user,linked surface name

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
C        write(6,*) 'Window pressure component AZ.'

        call relstr(AVAL(2),vnpstr,ilena,iflag)
        WRITE(ATRICN(ICN,4,1),'(a)') vnpstr
        call relstr(AVAL(4),vnpstr,ilena,iflag)
        WRITE(ATRICN(ICN,5,1),'(a)')vnpstr
        call eclose(AVAL(4),XYZICON(ICN,3),0.01,close)
        if(.NOT.close)then
          XYZICON(ICN,3)=AVAL(4)
          call edisp(iuout,'Updating icon height.')
        endif

C Check if the location zone name is the same as the zone name.
        helptopic='network_mismatch_detect'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(SVAL(1)(1:12).ne.ATRICN(ICN,6,1)(1:12))then
          write(outs,'(4a)')'Associated zone name ',
     &      SVAL(1)(1:lnblnk(SVAL(1))),
     &      ' does not match current atrribute ',ATRICN(ICN,6,1)
          dok=.true.
          CALL ASKOK(outs,'Update name?',OK,dok,nbhelp)
          if(OK)then
            WRITE(ATRICN(ICN,6,1),'(a)')SVAL(1)
            WRITE(ATRICN(ICN,7,1),'(a)')SVAL(2)
          endif
        endif

C Debug.
C        write(6,*) 'data 1:',ATRICN(icn,1,1),
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

C Icon commons
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)

C Connection commons
      COMMON/NWKCON/NICNN,ICNS(MNCNN),ICNE(MNCNN),ICNNT(MNCNN),
     & ICNSP(MNCNN),ICNEP(MNCNN),CNWNP(MNCNN,MCIP,3),
     & NCONWP(MNCNN),idatrdom(MNCNN),ddtagatr(MNCNN,MIATRB,5),
     & ddatrib(MNCNN,MIATRB,3),ddmenuatr(MNCNN,MIATRB)

      COMMON/NWKICS/NWICNM(MICN),NWICNHIS(MICN),NWICNLBL(MICN,MICND),
     & NWICNTXT(MICN,60)

C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)

C icontoflow() is a pointer from icon to flow node (positive) or flow
C component (negative).
C icnisnode(MNCNN) toggle where .true. means icon index is
C associated with a flow node.
      common/NWKFLW/icontoflow(MNCNN),icnisnode(MNCNN)

C ICATIE: pointer from DATACATIE bookkeepping back to the 
C         NWKCON arrays.
      DIMENSION ICATIE(MIATRB)
  
C IECDATE is menu array for attribute categories.
      DIMENSION IECDATE(MIATRC+6)

      LOGICAL ISEL,CSEL,icnisnode

      CHARACTER outs*124,outsd*124
      character ddtagatr*12,ddmenuatr*32,ddatrib*12
      CHARACTER NWICNM*12,NWICNHIS*40,NWICNLBL*4,ct*12
      character NWICNTXT*72
      character ATRTAG*12,ATRMENU*32,ATRICN*12

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
C presented by category).
      SUBROUTINE NETIDATR(ier)
# include "epara.h"
# include "gnetwk.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY 

C Icon commons       
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)

      COMMON/NWKICS/NWICNM(MICN),NWICNHIS(MICN),NWICNLBL(MICN,MICND),
     & NWICNTXT(MICN,60) 

      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)
      COMMON/NWKTYP/INWKTYP,vergnf 

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
      CHARACTER NWICNM*12,NWICNHIS*40,NWICNLBL*4
      character NWICNTXT*72,A*1,KEY*1,outs*124,SVAL*12
      character ATRTAG*12,ATRMENU*32,ATRICN*12
      character VSTR*12,VNPSTR*12,ISTR*12,INPSTR*12,exttag*12
      character stra*12,strb*12

      CHARACTER CATSI*12,IECDATE*40,CATIES*32,
     &IECDATEs*49,DATCATIE*12,TAGCATIE*12

      LOGICAL ISEL,CSEL,MATCH,close,isexavail,ok,dok
      logical closect,closewt,closeft,closev,closex,closey,closez
      integer NECDATA,ICOUT1,NECDATAs,ICOUT2 ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

#ifdef OSI
      integer impx,impy,iwe
#else
      integer*8 impx,impy,iwe
#endif

      helpinsub='NETIDATR'  ! set for subroutine

      if(INWKTYP.eq.1.or.INWKTYP.eq.3.or.INWKTYP.eq.6)then
        CALL USRMSG('Click on the component to edit',' ','-')
      elseif(INWKTYP.eq.2)then
        CALL USRMSG('Click on the node/component to edit',' ','-')
      elseif(INWKTYP.eq.4)then
        CALL USRMSG('Click on the control entity to edit',' ','-')
      endif
      CALL TRACKVIEW(A,IX,IY)
      IPX=IX
      IPY=IY
      CALL PIXEL2U(IPX,IPY,GX,GY)
      CGX=GX
      CGY=GY
      CLOSEX=.FALSE.
      CLOSEY=.FALSE.
      DO 515 ICN=1,NNICN
        IF(IVIEW.EQ.1)THEN
          CALL ECLOSE(CGX,XYZICON(ICN,1),0.5,CLOSEX)
          CALL ECLOSE(CGY,XYZICON(ICN,2),0.5,CLOSEY)
        ELSEIF(IVIEW.EQ.2)THEN
          CALL ECLOSE(CGX,XYZICON(ICN,1),0.5,CLOSEX)
          CALL ECLOSE(CGY,XYZICON(ICN,3),0.5,CLOSEY)
        ELSE
          CALL ECLOSE(CGX,XYZICON(ICN,2),0.5,CLOSEX)
          CALL ECLOSE(CGY,XYZICON(ICN,3),0.5,CLOSEY)
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

C Display any text associated with this icon.
          if(NIVT(ICN).gt.0)then
            do 410 jh=1,NIVT(ICN)
              call edisp(iuout,NWICNTXT(ICN,jh))
  410       continue
          endif

C Search for unique categories to present to the user.
          DO 520 I=1,NICONATR(ICN)
            MATCH=.FALSE.
            IF(I.NE.1)THEN
              DO 525 J=1,NCATI
                IF(CATSI(J).EQ.ATRTAG(ICN,I,1))THEN
                  MATCH=.TRUE.

C Put the data into an existing category and 
C Increment the number of entries for this category
                  NCATIE(J)=NCATIE(J)+1
                  CATIES(NCATI,NCATIE(J))=ATRMENU(ICN,I)

C Set the index for this category entry: relates category data -> icon data
                  ICATIE(J,NCATIE(J))=I
                  DO 523 L=1,5
                    if(L.le.3)DATCATIE(J,NCATIE(J),L)=ATRICN(ICN,I,L)
                    TAGCATIE(J,NCATIE(J),L)=ATRTAG(ICN,I,L)
  523             CONTINUE
                ENDIF
  525         CONTINUE
            ENDIF

C Put the data into a new category
            IF(.NOT.MATCH)THEN
              NCATI=NCATI+1
              CATSI(NCATI)=ATRTAG(ICN,I,1)
              NCATIE(NCATI)=NCATIE(NCATI)+1

C Set the index for this category entry: relates category data -> icon data
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
          WRITE(IECDATE(4),'(A)') 'b view component notes          '
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
          CALL EMENU('Component data ',IECDATE,NECDATA,ICOUT1)
          IF(ICOUT1.EQ.NECDATA)THEN
            RETURN
          ELSEIF(ICOUT1.EQ.NECDATA-1)THEN   
            helptopic='component_cat_edits'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL PHELPD('Data categories',nbhelp,'-',0,0,IER)
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

C Text associated with this icon. If allowed, edit.
C << get code from dbm >>
            if(NIVT(ICN).gt.0)then
              call edisp(iuout,'Component notes: ')
              do 116 jh=1,NIVT(ICN)
                call edisp(iuout,NWICNTXT(ICN,jh))
  116         continue
            endif
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
            WRITE(IECDATEs(M+2),'(A)')'  cannot edit this value (#)'
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
                read(DATCATIE(IAST,ISL,2),*,iostat=ios,ERR=99)IVMIN
                read(DATCATIE(IAST,ISL,3),*,iostat=ios,ERR=99)IVMAX
                read(DATCATIE(IAST,ISL,1),*,iostat=ios,ERR=99)IVAL
                read(DATCATIE(IAST,ISL,1),*,iostat=ios,ERR=99)IVALD
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
                read(DATCATIE(IAST,ISL,2),*,iostat=ios,ERR=99)VMIN
                read(DATCATIE(IAST,ISL,3),*,iostat=ios,ERR=99)VMAX
                read(DATCATIE(IAST,ISL,1),*,iostat=ios,ERR=99)VAL
                read(DATCATIE(IAST,ISL,1),*,iostat=ios,ERR=99)VALD

C If TAGCATIE(IAST,ISL,3) is `external` then call selrelexttag
C based on the search string in TAGCATIE(IAST,ISL,4). If iier
C returned as -1 then no selection made.
                if(TAGCATIE(IAST,ISL,3)(1:8).eq.'external'.and.
     &             TAGCATIE(IAST,ISL,5)(1:4).eq.'user')then
                  if(isexavail)then
                    exttag=TAGCATIE(IAST,ISL,4)
                    call selrelexttag(exttag,AVAL,SVAL,iier)
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

                        CALL ECLOSE(AVAL(1),VALV,0.01,CLOSEV)
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
                          helptopic='network_mismatch_detect'
                          call gethelptext(helpinsub,helptopic,nbhelp)
                          write(outs,'(4a)') 'The zone name ',
     &                      SVAL(1)(1:lnblnk(SVAL(1))),
     &                      ' do not match current atrribute ',
     &                      DATCATIE(2,2,1)
                          dok=.true.
                          CALL ASKOK(outs,'Update name?',OK,dok,nbhelp)
                          if(OK)then
                           WRITE(DATCATIE(2,2,1),'(a)') SVAL(1)
                           WRITE(ATRICN(ICN,ICATIE(2,2),1),'(a)')SVAL(1)
                          endif
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
C  location,text,-,-,manager,user,linked zone name
C  location,text,-,-,glazing,user,linked surface name
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
C                          write(6,*) 'Window pressure component AZ.'

                          call relstr(AVAL(2),vnpstr,ilena,iflag)
                          WRITE(DATCATIE(1,4,1),'(a)') vnpstr
                          WRITE(ATRICN(ICN,ICATIE(1,4),1),'(a)') vnpstr
                          call relstr(AVAL(4),vnpstr,ilena,iflag)
                          WRITE(DATCATIE(2,1,1),'(a)') vnpstr
                          WRITE(ATRICN(ICN,ICATIE(2,1),1),'(a)')vnpstr
                          call eclose(AVAL(4),XYZICON(ICN,3),0.01,close)
                          if(.NOT.close)then
                            XYZICON(ICN,3)=AVAL(4)
                            call edisp(iuout,'Updating icon height.')
                          endif

C Check if the location zone name is the same as the zone name.
                          if(SVAL(1)(1:12).ne.DATCATIE(2,2,1)(1:12))then
                            helptopic='network_mismatch_detect'
                            call gethelptext(helpinsub,helptopic,nbhelp)
                            write(outs,'(4a)')'Associated zone name ',
     &                        SVAL(1)(1:lnblnk(SVAL(1))),
     &                        ' do not match current atrribute ',
     &                        DATCATIE(2,2,1)
                            dok=.true.
                            CALL ASKOK(outs,'Update name?',
     &                        OK,dok,nbhelp)
                            if(OK)then
                             WRITE(DATCATIE(2,2,1),'(a)') SVAL(1)
                             WRITE(ATRICN(ICN,ICATIE(2,2),1),'(a)')
     &                         SVAL(1)
                             WRITE(DATCATIE(2,3,1),'(a)') SVAL(2)
                             WRITE(ATRICN(ICN,ICATIE(2,3),1),'(a)')
     &                         SVAL(2)
                            endif
                          endif

C Debug.
C                          write(6,*) 'datcatie 1:',DATCATIE(1,1,1),
C     &                      DATCATIE(1,2,1),DATCATIE(1,3,1),
C     &                      DATCATIE(1,4,1),DATCATIE(1,5,1)
C                          write(6,*) 'datcatie 2:',DATCATIE(2,1,1),
C     &                      DATCATIE(2,2,1),DATCATIE(2,3,1),
C     &                      DATCATIE(2,4,1)

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
                    call eclose(VAL,XYZICON(ICN,3),0.01,close)
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
                     call selrelexttag(exttag,AVAL,SVAL,iier)
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

C Icon commons
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)

C Connection commons
      COMMON/NWKCON/NICNN,ICNS(MNCNN),ICNE(MNCNN),ICNNT(MNCNN),
     & ICNSP(MNCNN),ICNEP(MNCNN),CNWNP(MNCNN,MCIP,3),
     & NCONWP(MNCNN),idatrdom(MNCNN),ddtagatr(MNCNN,MIATRB,5),
     & ddatrib(MNCNN,MIATRB,3),ddmenuatr(MNCNN,MIATRB)

      COMMON/NWKICS/NWICNM(MICN),NWICNHIS(MICN),NWICNLBL(MICN,MICND),
     & NWICNTXT(MICN,60)

C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)

C Net work view information.
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

C icontoflow() is a pointer from icon to flow node (positive) or flow
C component (negative).
C icnisnode(MNCNN) toggle where .true. means icon index is
C associated with a flow node.
      common/NWKFLW/icontoflow(MNCNN),icnisnode(MNCNN)

C CATSI: list of attribute categories (presented for selection).
C NCATIE: number of data fields in each attribute category.
C ICATIE: pointer from DATACATIE bookkeepping back to the 
C         NWKCON arrays.
      DIMENSION CATSI(MIATRC),NCATIE(MIATRC),ICATIE(MIATRC,MIATRB)

C DATCATIE: attribute editing array for the current category. 
C TAGCATIE: ajucent for DATCATIE of the icon attribute tags. 
C IECDATE is menu array for attribute categories.
C IECDATEs is menu arrary for attribute data fields.
C CATIES is menu entry for each data field
      DIMENSION DATCATIE(MIATRC,MIATRB,3),IECDATE(MIATRC+6),
     &CATIES(MIATRC,MIATRB),IECDATEs(30),TAGCATIE(MIATRC,MIATRB,5)

      LOGICAL ISEL,CSEL,MATCH,CLOSEX,CLOSEY,icnisnode

      CHARACTER outs*124,outsd*124
      character ddtagatr*12,ddmenuatr*32,ddatrib*12
      CHARACTER NWICNM*12,NWICNHIS*40,NWICNLBL*4,ct*12
      character NWICNTXT*72
      character ATRTAG*12,ATRMENU*32,ATRICN*12

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
      integer impx,impy,iwe
#else
      integer*8 impx,impy,iwe
#endif

C Let the user select the connection to be edited (click on a 
C way-point).

      helpinsub='NETCDATR'  ! set for subroutine
      IPX=IX
      IPY=IY
      CALL PIXEL2U(IPX,IPY,GX,GY)
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
     &'WARNING: start and end fluids do not match!')
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
                read(DATCATIE(IAST,ISL,2),*,iostat=ios,ERR=99)IVMIN
                read(DATCATIE(IAST,ISL,3),*,iostat=ios,ERR=99)IVMAX
                read(DATCATIE(IAST,ISL,1),*,iostat=ios,ERR=99)IVAL
                read(DATCATIE(IAST,ISL,1),*,iostat=ios,ERR=99)IVALD
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
                read(DATCATIE(IAST,ISL,2),*,iostat=ios,ERR=99)VMIN
                read(DATCATIE(IAST,ISL,3),*,iostat=ios,ERR=99)VMAX
                read(DATCATIE(IAST,ISL,1),*,iostat=ios,ERR=99)VAL
                read(DATCATIE(IAST,ISL,1),*,iostat=ios,ERR=99)VALD

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
  99  CALL EDISP(IUOUT,'Error in string formatting NETCDATR')
      END

