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

C explor.f provides menu and help facilities for the main item exploration
C editing and viweing sections of TDF.  Written by the staff of ESRU
C and making use of the ESRU standard library files libesru.a

C The following routines are included:
C  SCHEDDT: returns a 3 and 8 character description of the day type.
C  EDTITEM:  Edits static and period data for a tabular TDF item.
C  EDPERD:  Edits period/temporal information related to an instance
C           in the TDF db.
C  EDIMPT:  Edits data importation specification.

C ******** SCHEDDT
C SCHEDDT returns a 3 and 8 character description of the day type.
      subroutine scheddt(indx,d3,d8)
      character d3*3,d8*8
      if(indx.eq.0)then
        d3='all'
        d8='All days'
      elseif(indx.eq.1)then
        d3='wkd'
        d8='Weekdays'
      elseif(indx.eq.2)then
        d3='sat'
        d8='Waturday'
      elseif(indx.eq.3)then
        d3='sun'
        d8='Sunday  '
      elseif(indx.eq.4)then
        d3='hol'
        d8='Holiday '
      elseif(indx.eq.5)then
        d3='oth'
        d8='Other   '
      endif
      return
      end

C ************* EDTITEM ******************************************
C EDTITEM edits information about item (IFOC).
C The common blocks ??? are updated by this routine.
C ITRU is the unit number for user output, IER=0 OK, IER=1 problem.
C IALT is the number of text alternatives available for a text field.
      SUBROUTINE EDTITEM(ITRC,IFOC,IER)
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"
#include "epara.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL      
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TDFG1/GAIDE,GHELP(MHEL),GDHDRT(3),GMH
      COMMON/TDFG5/GMF(MBITS),MGMA(MBITS)
      COMMON/TDFG6/IALT(MBITS)
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/GDPER/igbdoy,igedoy
      CHARACTER LIMP*96,LEXP*96
      COMMON/TDFIO/LIMP,IUIMP,ITIMP,LEXP,IUEXP,ITEXP

C Explicit use of pophelp required because of dynamic generation.
      common/pophelp/h(60)
      character h*72  
                        
      DIMENSION SALT(6),IVAL(6),IPD(34)
      CHARACTER KEY*1,SALT*16
      CHARACTER PROMP*32,MSG*32,head*52
      CHARACTER GAIDE*36,GHELP*72,GDHDRT*72,GMH*32
      CHARACTER GMF*32,MGMA*32,TTAG*12,TTAIDE*36
      CHARACTER IPD*64,CSTR*54,DESCR*7,DESCR1*10,TPROMP*42
      CHARACTER*5 DESCRH,DESCRD
      CHARACTER outs*124,LINE*72,OLINE*72,msgl2*48
      logical changed,clkok,textalt,edited,XST
      character*35 ITEMS(9)      
      CHARACTER DFILE*96,tfile*96
      CHARACTER LOUTSTR*1000
      integer IPDL,INODA,nitms,INO  ! max items and current menu item

#ifdef OSI
      integer imw  ! for use with vwmenu
#else
      integer*8 imw  ! for use with vwmenu
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDTITEM'  ! set for subroutine
      
      IER=0

C On initial entry mark for checking bounds and redrawing graphics.
      changed = .true.
      clkok=.false.
      textalt=.false.
      edited=.false.
      itext=0

C Find number of lines of specific help text for
C this type by stripping info from from GMH.
      K=0
      CALL EGETWI(GMH,K,NHELP,0,MHEL,'F','help',IER)

C Reset pending actions on entry to the explor menu. See tdf.F for notes
C on menu common blocks. Initialise explor menu size variables based on 
C text window size. 
      IPACT=CREATE
      if(NTSTAT(IFOC).eq.0)then
        MHEAD=4
      else
        MHEAD=2+NTSTAT(IFOC)+3
      endif
      MCTL=3
      ILEN=(itdedoy-itdbdoy+1)*NTSPH*24
      CALL EKPAGE(IPACT)

C Standard menu entries first.
   28 ILEN=(itdedoy-itdbdoy+1)*NTSPH*24
      WRITE(IPD(1),'(A,A)')  '1 Entry name : ',TAG(IFOC)
      WRITE(IPD(2),'(A,A)')  '2 Description: ',TAIDE(IFOC)(1:36)
      CDAY=itdbdoy+AINT(FLOAT(IST)/FLOAT(NTSPH*24))
      CALL STDATE(itdyear,INT(CDAY),DESCR,DESCR1)
      IFIELD=NTSTAT(IFOC)
      if(IFIELD.gt.0)then

C Loop through the static fields and build up menu text etc.
        M=2
        IFIELD=NTSTAT(IFOC)
        DO 61 J=1,IFIELD
          M=M+1
          CALL EMKEY(M-2,KEY,IER)
          IA=IATDF(IFOC,J)
          IF(ITDFTR(IFOC,J).EQ.1)THEN
            WRITE(IPD(M),62)KEY,TDFDES(IFOC,J),TMAR(IFOC,IA)
   62       FORMAT(A1,1X,A,F10.3)
          ELSEIF(ITDFTR(IFOC,J).EQ.2)THEN
            WRITE(IPD(M),63)KEY,TDFDES(IFOC,J),TMAT(IFOC,IA)
   63       FORMAT(A1,1X,A,1X,A16)
          ELSEIF(ITDFTR(IFOC,J).EQ.3)THEN
            WRITE(IPD(M),64)KEY,TDFDES(IFOC,J),ITMAR(IFOC,IA)
   64       FORMAT(A1,1X,A,I8)
          ENDIF
   61   CONTINUE
      else
        M=2
      endif
      if(NTSTAT(IFOC).gt.0)then
        IPD(MHEAD-2)  ='  __________________________________ '
      endif
      IPD(MHEAD-1)  =GDHDRT(1)(1:60)
      IPD(MHEAD)    =GDHDRT(2)(1:60)

C Regenerate the menu text from current common blocks via UPEXP
C if some action has been requested. 
      M=MHEAD

C Loop through the temporal items until the page to be displayed.  
      ITR=49
      DO 10 IP=1,ILEN
        ITR=ITR+1
        IF(IP.GE.IST.AND.(IP.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(M-MHEAD+IFIELD,KEY,IER)

C Read in this timesteps data.
          CALL CLRTAB
          CALL ERTABU(ITRC,ITR,IER)

C Loop through the period fields and build up menu text etc.
C where IEFLDS is the total number of fields in this instance,
C ISFLDS is the field for the first period column.
C If this instance is a tabular type then we need to read in the
C relevent data from the file to display. The transform
C from IP to record is !!!!
          IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
          ISFLDS=IEFLDS-NTBITS(IFOC)+1
          NB=NTBITS(IFOC)
          CSTR=' '
          K=1
          DO 60 J=ISFLDS,IEFLDS

C The actual column. Read the current timestep or period data.
            IF(ITDFTR(IFOC,J).EQ.1)THEN
              if(NB.le.3)then
                K1=K+10
                WRITE(CSTR(K:K1),'(F10.3)')TABU(ITCOL(IFOC,J))
              elseif(NB.eq.4)then
                K1=K+9
                WRITE(CSTR(K:K1),'(F9.2)')TABU(ITCOL(IFOC,J))
              elseif(NB.eq.5)then
                K1=K+7
                WRITE(CSTR(K:K1),'(F7.2)')TABU(ITCOL(IFOC,J))
              elseif(NB.eq.6)then
                K1=K+6
                WRITE(CSTR(K:K1),'(F6.1)')TABU(ITCOL(IFOC,J))
              elseif(NB.eq.9)then
                K1=K+5
                WRITE(CSTR(K:K1),'(F5.0)')TABU(ITCOL(IFOC,J))
              else
                K1=K+7
                WRITE(CSTR(K:K1),'(F7.2)')TABU(ITCOL(IFOC,J))
              endif
            ELSEIF(ITDFTR(IFOC,J).EQ.2)THEN

C Note that the actual data passed into the file is the alt-string 
C index INT(TABU(ITCOL(IFOC,J))) rather than the text itself.
              textalt=.true.
              itext=J
              K1=K+16
              IWHICH=INT(TABU(ITCOL(IFOC,J)))
              WRITE(CSTR(K:K1),'(A16)')TMAT(IFOC,IWHICH)
            ELSEIF(ITDFTR(IFOC,J).EQ.3)THEN
              if(NB.le.4)then
                K1=K+8
                WRITE(CSTR(K:K1),'(I8)')INT(TABU(ITCOL(IFOC,J)))
              elseif(NB.gt.4.and.NB.le.6)then
                K1=K+6
                WRITE(CSTR(K:K1),'(I6)')INT(TABU(ITCOL(IFOC,J)))
              elseif(NB.eq.9)then
                K1=K+5
                WRITE(CSTR(K:K1),'(I5)')INT(TABU(ITCOL(IFOC,J)))
              endif
            ENDIF
            K=K1+1
   60     CONTINUE

C If tabular reserve first display column for period which is a
C string or julian day fraction depending on the user selected format.
          CDAY=AINT(FLOAT(IP)/FLOAT(NTSPH*24))
          ITIME=IP-(INT(CDAY)*NTSPH*24)
          RDOTY=real(itdbdoy)+CDAY+(real(ITIME)/(real(NTSPH)*24.))
          CALL ESTIME(NTSPH,1,ITIME,DESCRH,DESCRD,TIMER)
          IF(IFTIME.EQ.0)THEN
            WRITE(IPD(M),'(A1,1X,A5,1X,A54)') KEY,DESCRH,CSTR(1:54)
          ELSEIF(IFTIME.EQ.1)THEN
            WRITE(IPD(M),'(A1,1X,A5,1X,A54)') KEY,DESCRD,CSTR(1:54)
          ELSEIF(IFTIME.EQ.2)THEN
            WRITE(IPD(M),'(A1,F8.4,1X,A54)')  KEY,RDOTY,CSTR(1:54)
          ENDIF
        ENDIF
   10 CONTINUE

C Length of menu. If creating a long list setup the paging vi EKPAGE
C and include page facility text as appropriate.
      IPDL=M+4
      IF(IPACT.EQ.CREATE)THEN
        CALL EKPAGE(IPACT)
        IPACT=NONE
      ENDIF

      IF(IPFLG.EQ.0)THEN
        WRITE(IPD(IPDL-3),'(a,1x,a)')'  ------------',DESCR1
      ELSE
        WRITE(IPD(IPDL-3),'(a,i3,a,i3,a,a)')'0 Page -- Part: ',
     &    IPM,' of ',MPM,' - ',DESCR1
      ENDIF

C Set the menu control text.
      IPD(IPDL-2)  ='! Set display period or import raw data'
      IPD(IPDL-1)  ='? Help                               '
      IPD(IPDL)    ='- Exit to TDF contents               '

C Call the graphing routine.
      if(changed)then
        CALL TMPLOT(ITRC,IFOC,IER)
        changed = .false.
      endif

C Update scratch file if editing detected.
C << when to update text file? >>
      if(edited)then
        msgl2='  '
        call usrmsg('Updating the scratch file...',msgl2,'-')
        CALL EWTDF(IER)
        call usrmsg('Updating the scratch file...done.',msgl2,'-')
        edited=.false.
      endif

C Loop through and find longest string in menu.
      IMW=24
      do 66 IM=1,IPDL
        if(lnblnk(IPD(IM)).gt.IMW)IMW=lnblnk(IPD(IM))
   66 continue

C Help text for this menu.
  13  continue
      helptopic='temporal_static_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      INODA=-2
      write(head,'(a,a,2x,a)') ' Tabular: ',TMENU(IFOC),TTYPE(IFOC)
      if(MMOD.EQ.8)then
        impx=0
        impy=0
        CALL VWMENU(head,IPD,IPDL,impx,impy,imw,irpx,irpy,INODA)
      else
        CALL EMENU(head,IPD,IPDL,INODA)
      endif

      IF(INODA.EQ.IPDL)THEN
        RETURN
      ELSEIF(INODA.EQ.IPDL-1)THEN

C Produce help text for this menu, including specific help text for
C this type by stripping info from from GMH. NHELP is the number
C of additional lines.
        K=0
        CALL EGETWI(GMH,K,NHELP,0,MHEL,'F','help',IER)
        helptopic='temporal_static_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IF(nhelp.GE.1)THEN
          DO 202 IHH=1,NHELP
            write(H(nbhelp+IHH),'(a)')GHELP(IHH)
  202     CONTINUE
        ENDIF
        CALL PHELPD('static fields',nbhelp+nhelp,'-',0,0,IER)
      ELSEIF(INODA.EQ.IPDL-2)THEN

C Begin with emenu.
    3   INO=-4
        IER=0
        ITEMS(1)= 'a graph day of current data     '
        ITEMS(2)= 'b graph sequence                '
        ITEMS(3)= 'c graph all                     '
        ITEMS(4)= 'd import column data            '
        ITEMS(5)= 'e import static values          '
        if(TTYPE(IFOC)(1:8).eq.'ACTIVITY')then
          ITEMS(6)= 'f import UK NCM data            '
          ITEMS(7)= ' _____________________________ '
          ITEMS(8)= '? help                          '
          ITEMS(9)= '- exit this menu                '
          nitms=9
        else
          ITEMS(6)= ' _____________________________ '
          ITEMS(7)= '? help                          '
          ITEMS(8)= '- exit this menu                '
          nitms=8  
        endif
       
C Reset the graphic display period. 
        helptopic='temporal_graph_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)

        CALL EMENU('Model Context',ITEMS,nitms,INO)
        IF(INO.EQ.nitms)THEN
          continue

        ELSEIF(INO.EQ.nitms-1)THEN

C Produce help text for the menu.
          helptopic='temporal_graph_menu'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('data options',nbhelp,'-',0,0,IER)

        ELSEIF(INO.EQ.1)THEN
          CDAY=itdbdoy+AINT(FLOAT(IST)/FLOAT(NTSPH*24))
          CALL STDATE(itdyear,INT(CDAY),DESCR,DESCR1)
          write(outs,'(2a)') 'graph will focus on ',descr1
          call edisp(iuout,outs)
          igedoy=INT(CDAY)
          igbdoy=INT(CDAY)
          changed = .true.
        ELSEIF(INO.EQ.2)THEN

C Define the beginning and ending dates for the TDF display.
   48     igbdoy=itdbdoy
          igedoy=itdedoy
          CALL EASKPER('Display period:',igbdoy,igedoy,IFDAY,IER)
          if(igbdoy.lt.itdbdoy)then
            msgl2=' please re-enter start date...'
            call usrmsg(' display period is before db starts',msgl2,'W')
            goto 48
          endif
          if(igedoy.gt.itdedoy)then
            msgl2=' please re-enter end date...'
            call usrmsg(' display period is after db ends',msgl2,'W')
            goto 48
          endif
          changed = .true.
          INODA=-4
          GOTO 28
        ELSEIF(INO.EQ.3)THEN
          igbdoy=itdbdoy
          igedoy=itdedoy
          write(outs,'(a)') 'graph will focus on full period.'
          call edisp(iuout,outs)
          changed = .true.
        ELSEIF(INO.EQ.4)THEN
          helptopic='temporal_timestep_import'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABC('Import data options: ','(see help)',
     &      'start @ temporal timestep one','start @ other timestep',
     &      'cancel',IWI,nbhelp)
          if(IWI.eq.1)then
            IPS=1
            CALL EDIMPT(ITRC,IFOC,IPS,IER)
            changed = .true.
            edited = .true.
          elseif(IWI.eq.2)then
            head=' Select insertion point'
            if(MMOD.EQ.8)then
              impx=0
              impy=0
              CALL VWMENU(head,IPD,IPDL,impx,impy,imw,irpx,irpy,INODA)
            else
              CALL EMENU(head,IPD,IPDL,INODA)
            endif
            CALL KEYIND(IPDL,INODA,IWHICH,INOUT)
            IPS=IWHICH

C Debug.
C            write(6,*) 'ips=',ips

            CALL EDIMPT(ITRC,IFOC,IPS,IER)
            changed = .true.
            edited = .true.
          endif
        ELSEIF(INO.EQ.5)THEN
          CALL EDSTATICIMPT(IFOC,IER)
          changed = .true.
          edited = .true.

C Check if there is an activity data type from SBEM//DOE to
C import NCM data for UK
        ELSEIF(TTYPE(IFOC)(1:8).EQ.'ACTIVITY'.AND.INO.EQ.6)THEN
          IPS=1
          helptopic='temporal_import_source'
          call gethelptext(helpinsub,helptopic,nbhelp)
          DFILE='/tmp/71.csv'
          write(tfile,'(a)') DFILE(1:lnblnk(DFILE))
          CALL EASKS(tfile,' Import file name? ',
     &      ' ',96,DFILE,'import file name',IER,nbhelp)
          if(tfile(1:2).ne.'  ')then
            write(LIMP,'(a)') tfile(1:lnblnk(tfile))
          endif
          IF(ITIMP.EQ.1)THEN
            CALL ERPFREE(IUIMP,ISTAT)
            call findfil(LIMP,XST)
            IF(.NOT.XST)THEN
              msgl2='  '
              CALL USRMSG('This file does not exist.',msgl2,'W')
            ENDIF
          ENDIF

C Import data - open the import file and read them.
          IUNIT=IFIL+4
          CALL EFOPSEQ(IUNIT,LIMP,1,IER)
          if(IER.ne.0) then
            msgl2='Is the name correct (option 1)?'
            CALL USRMSG('Could not open import data file.',msgl2,'W')
            goto 3
          endif

          DO 80 IP=IPS,NDBSTP
            ITREC=IP+49
            CALL STRIPC1K(IUNIT,LOUTSTR,0,ND,1,'import raw data',IER)
            IF(IER.NE.0)GOTO 103

C Read value, associate with proper column and check if it changes
C the minimum/maximum of that column.
            K=0
            VAL=0.
            CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','imp data',IER)
            CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','imp data',IER)
            TABU(ITCOL(IFOC,12))=VAL
            if(TABU(ITCOL(IFOC,12)).lt.TABUMN(ITCOL(IFOC,12)))
     &        TABUMN(ITCOL(IFOC,12))=TABU(ITCOL(IFOC,12))
            if(TABU(ITCOL(IFOC,12)).gt.TABUMX(ITCOL(IFOC,12)))
     &        TABUMX(ITCOL(IFOC,12))=TABU(ITCOL(IFOC,12))

            CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','imp data',
     &                  IER)
            TABU(ITCOL(IFOC,13))=VAL
            if(TABU(ITCOL(IFOC,13)).lt.TABUMN(ITCOL(IFOC,13)))
     &        TABUMN(ITCOL(IFOC,13))=TABU(ITCOL(IFOC,13))
            if(TABU(ITCOL(IFOC,13)).gt.TABUMX(ITCOL(IFOC,13)))
     &        TABUMX(ITCOL(IFOC,13))=TABU(ITCOL(IFOC,13))

            CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','imp data',
     &                  IER)
            TABU(ITCOL(IFOC,14))=VAL
            if(TABU(ITCOL(IFOC,14)).lt.TABUMN(ITCOL(IFOC,14)))
     &        TABUMN(ITCOL(IFOC,14))=TABU(ITCOL(IFOC,14))
            if(TABU(ITCOL(IFOC,14)).gt.TABUMX(ITCOL(IFOC,14)))
     &        TABUMX(ITCOL(IFOC,14))=TABU(ITCOL(IFOC,14))
                    
            CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','imp data',IER)
            TABU(ITCOL(IFOC,15))=INT(VAL)
            if(TABU(ITCOL(IFOC,15)).lt.TABUMN(ITCOL(IFOC,15)))
     &        TABUMN(ITCOL(IFOC,15))=TABU(ITCOL(IFOC,15))
            if(TABU(ITCOL(IFOC,15)).gt.TABUMX(ITCOL(IFOC,15)))
     &        TABUMX(ITCOL(IFOC,15))=TABU(ITCOL(IFOC,15))

            CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','imp data',IER)
            TABU(ITCOL(IFOC,16))=INT(VAL)
            if(TABU(ITCOL(IFOC,16)).lt.TABUMN(ITCOL(IFOC,16)))
     &        TABUMN(ITCOL(IFOC,16))=TABU(ITCOL(IFOC,16))
            if(TABU(ITCOL(IFOC,16)).gt.TABUMX(ITCOL(IFOC,16)))
     &        TABUMX(ITCOL(IFOC,16))=TABU(ITCOL(IFOC,16))

            CALL EWTABU(ITRC,ITREC,IER)
  80      CONTINUE

C End of file encountered, close the file and return to menu.
  103     msgl2=' End of import file reached'
          CALL USRMSG(' ',msgl2,'-')
          CALL ERPFREE(IUNIT,ISTAT)
          changed = .true.
          edited = .true.
          continue
        ELSE
          goto 3
        ENDIF
      ELSEIF(INODA.EQ.IPDL-3)THEN

C If there are enough items to warrent it, allow paging control - first
C get the action via EKPAGE then re-establish top of page key character
C and array index.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(INODA.GT.MHEAD.AND.INODA.LT.(IPDL-MCTL+1))THEN

C Edit period data in format defined by generic type with the
C current item data. Treat differently if Text/Real/Integer field.
C If tabular the allow editing of the value only.
C Read the timestep data for the period selected.
        CALL KEYIND(IPDL,INODA,IWHICH,INOUT)
        ITS=IWHICH
        ITREC=ITS+49
        CALL ERTABU(ITRC,ITREC,IER)
        IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
        ISFLDS=IEFLDS-NTBITS(IFOC)+1
        LINE=' '
        CDAY=AINT(FLOAT(ITS)/FLOAT(NTSPH*24))
        ITIME=ITS-(INT(CDAY)*NTSPH*24)
        RDOTY=REAL(itdbdoy)+CDAY+(REAL(ITIME)/(REAL(NTSPH)*24.))
        CALL ESTIME(NTSPH,1,ITIME,DESCRH,DESCRD,TIMER)
        IF(IFTIME.EQ.0)THEN
          WRITE(outs,'(A,A5)')  ' Current time: ',DESCRH
          WRITE(TPROMP,'(A,A,A5)') TDFDES(IFOC,ISFLDS),' @ ',DESCRH
        ELSEIF(IFTIME.EQ.1)THEN
          WRITE(outs,'(A,A5)')  ' Current time: ',DESCRD
          WRITE(TPROMP,'(A,A,A5)') TDFDES(IFOC,ISFLDS),' @ ',DESCRD
        ELSEIF(IFTIME.EQ.2)THEN
          WRITE(outs,'(A,F10.6)')' Current time: ',RDOTY
          WRITE(TPROMP,'(A,A,F10.6)') TDFDES(IFOC,ISFLDS),' @ ',RDOTY
        ENDIF
        call edisp(iuout,outs)
        call edisp(iuout,TPROMP)
        NB=NTBITS(IFOC)
        K=1
        DO 601 J=ISFLDS,IEFLDS

C The actual column. Read the current timestep or period data.
          ico=ITCOL(IFOC,J)
          IA=IATDF(IFOC,J)
          IF(ITDFTR(IFOC,J).EQ.1)THEN
            if(NB.le.3)then
              K1=K+10
              WRITE(LINE(K:K1),'(F10.3)')TABU(ico)
            elseif(NB.eq.4.or.NB.eq.5)then
              K1=K+9
              WRITE(LINE(K:K1),'(F9.2)')TABU(ico)
            elseif(NB.eq.6)then
              K1=K+8
              WRITE(LINE(K:K1),'(F8.2)')TABU(ico)
            elseif(NB.eq.9)then
              K1=K+7
              WRITE(LINE(K:K1),'(F7.1)')TABU(ico)
            else
              K1=K+8
              WRITE(LINE(K:K1),'(F8.2)')TABU(ico)
            endif
          ELSEIF(ITDFTR(IFOC,J).EQ.2)THEN

C Note that the actual data passed into the file is the alt-string 
C index INT(TABU(ITCOL(IFOC,J))) rather than the text itself.
            K1=K+16
            IWHICH=INT(TABU(ico))
            WRITE(LINE(K:K1),'(A16)')TMAT(IFOC,IWHICH)
          ELSEIF(ITDFTR(IFOC,J).EQ.3)THEN
            if(NB.le.5)then
              K1=K+8
              WRITE(LINE(K:K1),'(I8)')INT(TABU(ico))
            else
              K1=K+7
              WRITE(LINE(K:K1),'(I7)')INT(TABU(ico))
            endif
          ENDIF
          K=K1+1
 601    CONTINUE

C Use explicit help from the temporal data.
        IF(NHELP.GE.1)THEN
          DO 29 IH=1,NHELP
            H(IH)=GHELP(IH)
  29      CONTINUE 
        ENDIF
        OLINE=LINE
 129    if(.NOT.textalt)then
          CALL EASKS(LINE,GDHDRT(3)(1:68),' ',
     &      72,' 0 0 0','tabul edit',IER,NHELP)
          edited = .true.
        else
          CALL EASKSCMD(LINE,GDHDRT(3)(1:68),' ',
     &      '*edit:alts',clkok,72,' 0 0 0','tabu:alts edit',IER,NHELP)
          if(clkok)then

C A text field so display all of the generic alternatives for selection.
C Begin by copying alternates into an array for presentation.
            IA=IATDF(IFOC,itext)
            call exgmat(ia,SALT,ier)
            IX=1
            MSG=TDFDES(IFOC,itext)
            CALL EPICKS(IX,IVAL,' ',PROMP,
     &        16,IALT(itext),SALT,MSG,IER,NHELP)

C If one of the alternates selected then transfer this into TMAT.
            if(IVAL(1).NE.0)then
              ico=ITCOL(IFOC,itext)
              TABU(ico)=REAL(IVAL(1))
              edited = .true.
            endif

C If other data has not changed then loop back to prompt, otherwise
C decode the data and then loop back.
            if(OLINE(1:lnblnk(OLINE)).eq.LINE(1:lnblnk(LINE)))then
              goto 129
            endif
          endif
        endif

C Extract the data.
        K=0
        DO 602 J=ISFLDS,IEFLDS

C The actual column. Read the current timestep or period data.
          IA=IATDF(IFOC,J)
          ico=ITCOL(IFOC,J)
          IF(ITDFTR(IFOC,J).EQ.1)THEN
            CALL EGETWR(LINE,K,VAL,TMIN(IFOC,IA),TMAX(IFOC,IA),
     &        '-',TDFDES(IFOC,J),IER)
            TABU(ico)=VAL
          ELSEIF(ITDFTR(IFOC,J).EQ.2)THEN

C Note that the actual data passed into the file is the alt-string 
C index INT(TABU(ITCOL(IFOC,J))) rather than the text itself.
            CALL EGETWI(LINE,K,IV,ITMIN(IFOC,IA),ITMAX(IFOC,IA),
     &        'W',TDFDES(IFOC,J),IER)
            TABU(ico) = real(IV)
          ELSEIF(ITDFTR(IFOC,J).EQ.3)THEN
            CALL EGETWI(LINE,K,IV,ITMIN(IFOC,IA),ITMAX(IFOC,IA),
     &        'W',TDFDES(IFOC,J),IER)
            TABU(ico) = real(IV)
          ENDIF
          if(TABU(ico).lt.TABUMN(ico))TABUMN(ico)=TABU(ico)
          if(TABU(ico).gt.TABUMX(ico))TABUMX(ico)=TABU(ico)
  602   CONTINUE
        ITREC=ITS+49
        CALL EWTABU(ITRC,ITREC,IER)
        msgl2=' Problem detected...'
        IF(IER.EQ.1) CALL USRMSG(' ',msgl2,'-')
        changed = .true.
        edited = .true.
      ELSEIF(INODA.EQ.1)THEN
        helptopic='temporal_unique_id'
        call gethelptext(helpinsub,helptopic,nbhelp)
        TTAG=TAG(IFOC)
        CALL EASKS(TTAG,' ',' Unique id string? ',
     &    12,'new_item','item id',IER,nbhelp)
        if(TTAG(1:2).ne.'  ')then
          TAG(IFOC)=TTAG
          edited = .true.
        endif
      ELSEIF(INODA.EQ.2)THEN
        helptopic='temporal_unique_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        TTAIDE=TAIDE(IFOC)
        CALL EASKS(TTAIDE,' ',' Item description ',
     &    36,'no comment','aide',IER,nbhelp)
        if(TTAIDE(1:2).ne.'  ')then
          TAIDE(IFOC)=TTAIDE
          edited = .true.
        endif

C Static fields: Display text attributes in format defined by generic 
C type with the current item data. Treat differently if Text or Real field.
C Use explicit h for this section.
      ELSEIF(INODA.GT.2.AND.INODA.LE.(2+IFIELD))THEN
        IFLD=INODA-2
        IA=IATDF(IFOC,IFLD)
        IF(NHELP.GE.1)THEN
          DO 209 IH=1,NHELP
            H(IH)=GHELP(IH)
 209      CONTINUE 
        ENDIF
        PROMP=MGMA(IFLD)
        IF(ITDFTR(IFOC,IFLD).EQ.1)THEN

C Ask for a real value using generic range checking, default and prompt.
          CALL EASKR(TMAR(IFOC,IA),' ',PROMP,TMIN(IFOC,IA),
     &      'W',TMAX(IFOC,IA),'W',TMAR(IFOC,IA),TDFDES(IFOC,IFLD),
     &      IER,NHELP)
          edited = .true.
        ELSEIF(ITDFTR(IFOC,IFLD).EQ.2)THEN

C A text field so display all of the generic alternatives for selection.
C Begin by copying alternates into an array for presentation.
          CALL EPMENSV
          call exgmat(ia,SALT,ier)
          IX=1
          MSG=TDFDES(IFOC,IFLD)
          CALL EPICKS(IX,IVAL,' ',PROMP,
     &      16,IALT(IFLD),SALT,MSG,IER,NHELP)

C If one of the alternates selected then transfer this into TMAT.
          IF(IVAL(1).NE.0)THEN
            WRITE(TMAT(IFOC,IA),'(A16)')SALT(IVAL(1))
            edited = .true.
          ENDIF
          CALL EPMENRC
        ELSEIF(ITDFTR(IFOC,IFLD).EQ.3)THEN

C Ask for a integer value using generic range checking, default and prompt.
          PROMP=MGMA(IFLD)
          CALL EASKI(ITMAR(IFOC,IA),' ',PROMP,ITMIN(IFOC,IA),
     &      'W',ITMAX(IFOC,IA),'W',ITMAR(IFOC,IA),TDFDES(IFOC,IFLD),
     &      IER,NHELP)
          edited = .true.
        ENDIF
      ELSE
        INODA=-1
        GOTO 13
      ENDIF
      INODA=-4
      GOTO 28

      END

C ************* STRTIM 
C STRTIM: Takes times-of-day in string (WORD) possibly edited by the user
C returning DTIME as a fraction of a day between 0.0 and 1.0,
C HTIME as an hour+fracton between 0.0 and 24.0 and ITPER as the nearest
C timestep based on NTS per hour and the current settings of IFTIME. 
C IFTIME 0 gives '10h30', 1 gives '10.50', 2 gives '0.4375'
      SUBROUTINE STRTIM(WORD,IFTIME,NTS,DTIME,HTIME,ITPER,IER)
      
      integer lnblnk  ! function definition
      integer iCountWords

      CHARACTER USRT*10,WORD*10
      LOGICAL CLOSE

      IER=0
      IF(IFTIME.EQ.0)THEN

C Looking for either a '12h00' input or '12 00' input, if one word 
C detected strip out the 'h' or 'H' and then parse the hours & minutes.
        USRT=WORD
        IW = iCountWords(USRT)
        IF(IW.EQ.1)THEN
          DO 12 I=1,lnblnk(USRT)
            IF(USRT(I:I).EQ.'h'.OR.USRT(I:I).EQ.'h')USRT(I:I)=' '
   12     CONTINUE
          K=0
          CALL EGETWI(USRT,K,IXX,0,24,'W','hour',IER)
          CALL EGETWI(USRT,K,IYY,0,60,'W','minute',IER)
        ELSEIF(IW.EQ.2)THEN
          K=0
          CALL EGETWI(USRT,K,IXX,0,24,'W','hour',IER)
          CALL EGETWI(USRT,K,IYY,0,60,'W','minute',IER)
        ENDIF
        HTIME=REAL(IXX)+(REAL(IYY)/60.0)
        DTIME=(REAL(IXX)+(REAL(IYY)/60.0))/24.0
      ELSEIF(IFTIME.EQ.1)THEN
        read(WORD,'(F10.4)')HTIME
        DTIME=HTIME/24.0
      ELSEIF(IFTIME.EQ.2)THEN
        read(WORD,'(F10.4)')DTIME
        HTIME=DTIME*24.
      ENDIF

C Loop through each timestep of the day and find out which one is
C closest to DTIME. 
      LAST=NTS*24
      TOL=REAL(NTS)/48.
      DFRAC=REAL(NTS)/24.
      DO 10 I=1,LAST
        CURR=REAL(I)*DFRAC
        CALL ECLOSE(DTIME,CURR,TOL,CLOSE)
        IF(CLOSE)THEN
          ITPER=I
          RETURN
        ENDIF
   10 CONTINUE

C If reached this point then an error finding the nearest
C timestep.
      IER=1
      RETURN

      END

C ************* EDIMPT ************************************************
C  EDIMPT:  Edits data importation specification.
C Set up to allow user to specify the association between one of more
C columns in an import file and fields in a tabular instance.
C IPS is the timestep to insert data at.
      SUBROUTINE EDIMPT(ITRC,IFOC,IPS,IER)
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"
      
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      CHARACTER LIMP*96,LEXP*96
      COMMON/TDFIO/LIMP,IUIMP,ITIMP,LEXP,IUEXP,ITEXP
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)

      DIMENSION IMPT(21),IMSTC(MBITS),SALT(6)
      CHARACTER IMPT*42,KEY*1,OUTSTR*124,WORD*20,OUTS*124
      CHARACTER SALT*16
      CHARACTER DFILE*96,tfile*96,msgl2*48
      logical warncas,XST
      integer IMPTL,INODA ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDIMPT'  ! set for subroutine

C Clear column association.
      DO 27 I=1,MBITS
        IMSTC(I)=0
   27 CONTINUE
      IMSTL=1

C Standard menu entries first.
      MHEAD=7
      MCTL=5
   28 WRITE(IMPT(1),'(A,A)') '  Entry name : ',TAG(IFOC)
      WRITE(IMPT(2),'(A,A)') '  Description: ',TAIDE(IFOC)(1:26)

C Import file.
      WRITE(IMPT(3),'(A,A)') '1 Import file: ',LIMP(1:26)
      IF(ITIMP.EQ.0)THEN
        IMPT(4) =    '  Import file: UNKNWN (go to setup)  '
      ELSEIF(ITIMP.EQ.1)THEN
        IMPT(4) =    '  Import file: column text '
      ENDIF
      WRITE(IMPT(5),'(A,I3)') '2 Begin reading @ line: ',IMSTL
      IMPT(6) =      '  __________________________________   '
      IMPT(7) =      '  Column associations:                 '

C Loop through the timestep fields and build up menu text etc.
C where IEFLDS is the total number of fields in this instance,
C ISFLDS is the field for the first period column.
      IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
      ISFLDS=IEFLDS-NTBITS(IFOC)+1

C Regenerate the menu text from current common blocks. 
      M=7
      DO 60 J=ISFLDS,IEFLDS
        M=M+1
        CALL EMKEY(M-7,KEY,IER)
        WRITE(IMPT(M),'(A1,1X,A,I3)')KEY,TDFDES(IFOC,J),IMSTC(J)
   60 CONTINUE

C Set the menu control text.
      M=M+1
      IMPT(M) =     '  __________________________________   '
      M=M+1
      IMPT(M) =     '! View start of import file            '
      M=M+1
      IMPT(M) =     '< Import data                          '
      M=M+1
      IMPT(M) =     '? Help                                 '
      M=M+1
      IMPT(M) =     '- Exit to timestep display             '
      IMPTL=M

C Help text for this menu.
      helptopic='temporal_import_param'
      call gethelptext(helpinsub,helptopic,nbhelp)
 
      INODA=-2
   13 CALL EMENU('  Import specs',IMPT,IMPTL,INODA)

      IF(INODA.EQ.IMPTL)THEN
        RETURN
      ELSEIF(INODA.EQ.3)THEN
        DFILE='data.imp'
        write(tfile,'(a)') LIMP(1:lnblnk(LIMP))
        CALL EASKS(tfile,' Import file name? ',
     &   ' ',96,DFILE,'import file name',IER,nbhelp)
        if(tfile(1:2).ne.'  ')then
          write(LIMP,'(a)') tfile(1:lnblnk(tfile))
        endif
        IF(ITIMP.EQ.1)THEN
          CALL ERPFREE(IUIMP,ISTAT)
          call findfil(LIMP,XST)
          IF(.NOT.XST)THEN
            msgl2='  '
            CALL USRMSG('This file does not exist.',msgl2,'W')
          ENDIF
        ENDIF
      ELSEIF(INODA.EQ.4)THEN

C Define import file format.
        CALL EASKABC('Import options:',' ',
     &    'UNKNOWN','ASCII column data','continue',IWM,nbhelp)
        if(IWM.ge.3)goto 13
        ITIMP=IWM-1
      ELSEIF(INODA.EQ.5)THEN

C Specify that read of import file begins at (non_comment) line IMSTL.
C Later extend logic to deal with specific file types such as timestep
C casual gain files.
        IV=IMSTL
        CALL EASKI(IV,' ','Begin reading @ import file line no:',
     &    0,'F',43800,'W',1,'begin read @',IER,nbhelp)
        IMSTL=IV
      ELSEIF(INODA.EQ.IMPTL-1)THEN
        helptopic='temporal_import_param'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('period fields',nbhelp,'-',0,0,IER)
      ELSEIF(INODA.EQ.IMPTL-2)THEN

C Import data - open the import file and read until the user defined
C beginning point read the whole line in.
        IUNIT=IFIL+4
        CALL EFOPSEQ(IUNIT,LIMP,1,IER)
        if (IER.ne.0) then
          msgl2='Is the name correct (option 1)?'
          CALL USRMSG('Could not open import data file.',msgl2,'W')
          goto 28
        endif

C Check that columns have been specified.
        do 85 J=ISFLDS,IEFLDS
          if (IMSTC(J).EQ.0) then
            write(outs,'(2a)')'A column was not associated with entry ',
     &         TDFDES(IFOC,J)
            msgl2='No data will be read for this entry.'
            CALL USRMSG(outs,msgl2,'W')
          endif
 85     continue
        N=0
   70   N=N+1
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'import raw data',IER)

C If the user has specified that data import should start at the
C line imstl of the column data file. If we are not there yet
C read another of the column data file.
        IF(N.LT.IMSTL) GOTO 70

        DO 80 IP=IPS,NDBSTP

C Read lines from file, starting at timestep ips (which was passed
C into this subroutine. While reading discard comments, check the 
C number of items in the data.
C If importing from first line do not stripc this pass.
C And do not stripc if IMSTL reached because the above stripc has
C the line of data we want.
          IF(IMSTL.EQ.1.AND.IP.EQ.1)THEN
            CALL EDISP(IUOUT,' ')
          ELSEIF(IMSTL.GT.1.AND.IP.EQ.1)THEN
            CALL EDISP(IUOUT,' ')
          ELSE
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'import data',IER)
          ENDIF
          IF(IER.NE.0)GOTO 103

C Read timestep from the TAB file.
          ITREC=IP+49
          CALL ERTABU(ITRC,ITREC,IER)
          IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
          ISFLDS=IEFLDS-NTBITS(IFOC)+1
          DO 6 J=ISFLDS,IEFLDS
            IA=IATDF(IFOC,J)
            IF(IMSTC(J).EQ.0)GOTO 6
            IF(ITDFTR(IFOC,J).EQ.1)THEN

C Real field. Overwrite column with the appropriate value from the
C import file. Parse the import line until the column is loacted.
              K=0
              DO 33 N1=1,IMSTC(J)
                IF(N1.EQ.IMSTC(J))THEN

C Read value, associate with proper column and check if it changes
C the minimum/maximum of that column.
                  VAL=0.
                  CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','imp data',
     &                        IER)
                  TABU(ITCOL(IFOC,J))=VAL
                  if(TABU(ITCOL(IFOC,J)).lt.TABUMN(ITCOL(IFOC,J)))
     &              TABUMN(ITCOL(IFOC,J))=TABU(ITCOL(IFOC,J))
                  if(TABU(ITCOL(IFOC,J)).gt.TABUMX(ITCOL(IFOC,J)))
     &              TABUMX(ITCOL(IFOC,J))=TABU(ITCOL(IFOC,J))
                ELSE
                  CALL EGETW(OUTSTR,K,WORD,'-','skip',IFLAG)
                ENDIF
  33          CONTINUE
              IF(ITRC.GT.1)THEN
                WRITE(OUTS,'(A,I2,A,F10.3)')'Fld:',J,' value:',VAL
                CALL EDISP(IUOUT,OUTS)
              ENDIF
            ELSEIF(ITDFTR(IFOC,J).EQ.2)THEN

C Text attribute field. Last word read is appropriate one. Compare it
C with allowable strings and then store the matching index.
              call exgmat(ia,SALT,ier)
              DO 34 N1=1,IMSTC(J)
                CALL EGETW(OUTSTR,K,WORD,'-','skip',IFLAG)
  34          CONTINUE
              IF(WORD(1:16).EQ.SALT(1)(1:16))THEN
                TABU(ITCOL(IFOC,J))=1.0
              ELSEIF(WORD(1:16).EQ.SALT(2)(1:16))THEN
                TABU(ITCOL(IFOC,J))=2.0
              ELSEIF(WORD(1:16).EQ.SALT(3)(1:16))THEN
                TABU(ITCOL(IFOC,J))=3.0
              ELSEIF(WORD(1:16).EQ.SALT(4)(1:16))THEN
                TABU(ITCOL(IFOC,J))=4.0
              ELSEIF(WORD(1:16).EQ.SALT(5)(1:16))THEN
                TABU(ITCOL(IFOC,J))=5.0
              ELSEIF(WORD(1:16).EQ.SALT(6)(1:16))THEN
                TABU(ITCOL(IFOC,J))=6.0
              ENDIF
              IF(ITRC.GT.1)THEN
                WRITE(OUTS,'(A,I2,A,A,A,F4.1)')'Fld:',J,' text:',WORD,
     &             ' val:',TABU(ITCOL(IFOC,J))
                CALL EDISP(IUOUT,OUTS)
              ENDIF
            ELSEIF(ITDFTR(IFOC,J).EQ.3)THEN

C Integer field. Overwrite column with the appropriate value from the
C import file. Parse the import line until the column is located.
              K=0
              DO 36 N1=1,IMSTC(J)
                IF(N1.EQ.IMSTC(J))THEN
                  IVAL=0
                  CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','imp data',
     &                        IER)
                  TABU(ITCOL(IFOC,J))=REAL(IVAL)
                  if(TABU(ITCOL(IFOC,J)).lt.TABUMN(ITCOL(IFOC,J)))
     &              TABUMN(ITCOL(IFOC,J))=TABU(ITCOL(IFOC,J))
                  if(TABU(ITCOL(IFOC,J)).gt.TABUMX(ITCOL(IFOC,J)))
     &              TABUMX(ITCOL(IFOC,J))=TABU(ITCOL(IFOC,J))
                ELSE
                  CALL EGETW(OUTSTR,K,WORD,'-','skip',IFLAG)
                ENDIF
  36          CONTINUE
              IF(ITRC.GT.1)THEN
                WRITE(OUTS,'(A,I2,A,I5)')'Fld:',J,' value:',IVAL
                CALL EDISP(IUOUT,OUTS)
              ENDIF
            ENDIF
            CALL EWTABU(ITRC,ITREC,IER)
   6      CONTINUE
  80    CONTINUE

C Return to main data editing menu after warning user.
        call usrmsg(
     &    'Imported data is currently in memory. Remember to use the',
     &    'save->save asci command after you exit from this menu.','W')
        return
      ELSEIF(INODA.EQ.IMPTL-3)THEN

C Preview import file.
        IUNIT=IFIL+4
        CALL EFOPSEQ(IUNIT,LIMP,1,IER)
        IF(IER.LT.0)THEN
          msgl2=' Problem detected... '
          CALL USRMSG(' ',msgl2,'W')
          INODA=-4
          GOTO 28
        ENDIF

C Actual read of ASCII import file.  If it is an older timestep
C casual gain file provide a warning to the user.
        warncas=.false.
        K=0
    7   READ(IUNIT,10,IOSTAT=ISTAT,END=102)OUTSTR
   10   FORMAT(A124)
        if(k.eq.0.and.OUTSTR(1:14).eq.'* CASUAL GAINS')warncas=.true.
        K=K+1
        CALL  EDISP(IUOUT,OUTSTR)
        IF(K.LT.8)GOTO 7
        CALL EDISP(IUOUT,'  .......  ')
        CALL EDISP(IUOUT,' ')
        CALL ERPFREE(IUNIT,ISTAT)
        if(warncas)then
          helptopic='temporal_warn_casgains'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('timestep import warning',nbhelp,'-',0,0,IER)
        endif
        GOTO 28

C End of file encountered, close the file and return to menu.
  102   msgl2=' End of file reached'
        CALL USRMSG(' ',msgl2,'-')
        CALL ERPFREE(IUNIT,ISTAT)
      ELSEIF(INODA.GT.MHEAD.AND.INODA.LT.(IMPTL-MCTL+1))THEN

C Specify the column to be read in for each of the timestep fields.
C If 0 then ignore.
        IFLD=INODA-MHEAD-1+ISFLDS
        IV=IMSTC(IFLD)
        helptopic='temporal_import_param'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKI(IV,' ','Associated column in import file:',
     &    0,'F',0,'-',1,'associated column',IER,nbhelp)
        IMSTC(IFLD)=IV
      ELSE
        INODA=-1
        GOTO 13
      ENDIF
      INODA=-4
      GOTO 28

C End of file encountered, close the file and return to menu.
  103 msgl2=' End of import file reached'
      CALL USRMSG(' ',msgl2,'-')
      CALL ERPFREE(IUNIT,ISTAT)
      GOTO 28

      END

C ************* EDSTATICIMPT ************************************************
C  EDSTATICIMPT:  Edits data importation specification for the static values.
C thew tdf entities. Set up to allow user to specify which item should be read
C from a file and to which static value it matches.
      SUBROUTINE EDSTATICIMPT(IFOC,IER)
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"
      
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      CHARACTER LIMP*96,LEXP*96
      COMMON/TDFIO/LIMP,IUIMP,ITIMP,LEXP,IUEXP,ITEXP
     
      DIMENSION IMPT(27),IMSTC(MBITS)
      CHARACTER IMPT*42,KEY*1,OUTSTR*124,WORD*20,OUTS*124
      CHARACTER DFILE*96,tfile*96,msgl2*48
      logical XST
      integer IMPTL,INODA ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDSTATICIMPT'  ! set for subroutine

C Static item association.
      DO 27 I=1,MBITS

C At the moment this is assigned as I+2 for the UK NCM 2007 activity data
C it will probably be necessary to start reading from the 3rd item of csv files 
C containing the static values for this UK activities database
        IMSTC(I)=I+2
   27 CONTINUE
      IMSTL=1

C Standard menu entries first.
      MHEAD=6
      MCTL=5
   28 WRITE(IMPT(1),'(A,A)') '  Entry name : ',TAG(IFOC)
      WRITE(IMPT(2),'(A,A)') '  Description: ',TAIDE(IFOC)(1:26)


C<< Give only one value to this one
C Import file.
      WRITE(IMPT(3),'(A,A)') '1 Import file: ',LIMP(1:26)
      WRITE(IMPT(4),'(A,I3)') '2 Begin reading @ line: ',IMSTL
      IMPT(5) =      '  __________________________________   '
      IMPT(6) =      '  Item associations:                 '

C Loop through the timestep fields and build up menu text etc.
C where IEFLDS is the total number of fields in this instance,
C ISFLDS is the field for the first period column.
      IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
      ISFLDS=IEFLDS-NTBITS(IFOC)

C Regenerate the menu text from current common blocks. 
      M=6
      DO 60 J=1,ISFLDS
        M=M+1
        CALL EMKEY(M-5,KEY,IER)
        WRITE(IMPT(M),'(A1,1X,A,I3)')KEY,TDFDES(IFOC,J),IMSTC(J)
   60 CONTINUE

C Set the menu control text.
      M=M+1
      IMPT(M) =     '  __________________________________   '
      M=M+1
      IMPT(M) =     '! View start of import file            '
      M=M+1
      IMPT(M) =     '< Import data                          '
      M=M+1
      IMPT(M) =     '? Help                                 '
      M=M+1
      IMPT(M) =     '- Exit to timestep display             '
      IMPTL=M

C Help text for this menu.
      helptopic='temporal_import_param'
      call gethelptext(helpinsub,helptopic,nbhelp)
 
      INODA=-2
   13 CALL EMENU('  Import specs',IMPT,IMPTL,INODA)

      IF(INODA.EQ.IMPTL)THEN
        RETURN
      ELSEIF(INODA.EQ.3)THEN
        DFILE='data.csv'
        write(tfile,'(a)') LIMP(1:lnblnk(LIMP))
        CALL EASKS(tfile,' Import file name? ',
     &   ' ',96,DFILE,'import file name',IER,nbhelp)
        if(tfile(1:2).ne.'  ')then
          write(LIMP,'(a)') tfile(1:lnblnk(tfile))
        endif
        IF(ITIMP.EQ.1)THEN
          CALL ERPFREE(IUIMP,ISTAT)
          call findfil(LIMP,XST)
          IF(.NOT.XST)THEN
            msgl2='  '
            CALL USRMSG('This file does not exist.',msgl2,'W')
          ENDIF
        ENDIF

      ELSEIF(INODA.EQ.4)THEN        

C Specify that read of import file begins at (non_comment) line IMSTL.
        IV=IMSTL
        CALL EASKI(IV,' ','Begin reading @ import file line no:',
     &    0,'F',43800,'W',1,'begin read @',IER,nbhelp)
        IMSTL=IV
      ELSEIF(INODA.EQ.IMPTL-1)THEN
        helptopic='temporal_import_param'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('period fields',nbhelp,'-',0,0,IER)
      ELSEIF(INODA.EQ.IMPTL-2)THEN      

C Import data - open the import file and read until the user defined
C beginning point read the whole line in.
        IUNIT=IFIL+4
        CALL EFOPSEQ(IUNIT,LIMP,1,IER)
        if (IER.ne.0) then
          msgl2='Is the name correct (option 1)?'
          CALL USRMSG('Could not open import data file.',msgl2,'W')
          goto 28
        endif

C Check that item numbers have been specified.
        do 85 J=1,ISFLDS
          if (IMSTC(J).EQ.0) then
            write(outs,'(2a)')'An item was not associated with entry ',
     &         TDFDES(IFOC,J)
            msgl2='No data will be read for this entry.'
            CALL USRMSG(outs,msgl2,'W')
          endif
  85    continue
        N=0
  70    N=N+1
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'import raw data',IER)

C If the user has specified that data import should start at the
C line imstl of the data file. If we are not there yet
C read another line of the data file.
        IF(N.LT.IMSTL) GOTO 70

        IF(IER.NE.0)GOTO 103

        DO 6 J=1,ISFLDS
          IA=IATDF(IFOC,J)
          IF(IMSTC(J).EQ.0)GOTO 6
          IF(ITDFTR(IFOC,J).EQ.1)THEN

C Real field. Overwrite static values with the appropriate value from 
C the import file. Parse the import line until the data item is located.

            K=0
            DO 33 N1=1,IMSTC(J)
              IF(N1.EQ.IMSTC(J))THEN
                VAL=0.
                CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','imp data',IER)
                TMAR(IFOC,IA)=VAL
              ELSE
                CALL EGETW(OUTSTR,K,WORD,'-','skip',IFLAG)
              ENDIF          
  33        CONTINUE

          ELSEIF(ITDFTR(IFOC,J).EQ.2)THEN

C Text attribute field. Last word read is appropriate one. Compare it
C with allowable strings and then store the matching index.
            DO 34 N1=1,IMSTC(J)
              CALL EGETW(OUTSTR,K,WORD,'-','skip',IFLAG)
  34        CONTINUE

          ELSEIF(ITDFTR(IFOC,J).EQ.3)THEN

C Integer field. Overwrite static values with the appropriate value from 
C the import file. Parse the import line until the data item is located.
            K=0
            DO 36 N1=1,IMSTC(J)
              IF(N1.EQ.IMSTC(J))THEN
                VAL=0

C Here although we are looking for an integer, the csv files usually
C convert everything to real. So, the scan is for a real at the moment
C and then this is taken an integer.
                CALL EGETWR(OUTSTR,K,VAL,0.0,0.0,'-','imp data',IER)
                ITMAR(IFOC,IA)=NINT(VAL)
              ELSE
                CALL EGETW(OUTSTR,K,WORD,'-','skip',IFLAG)
              ENDIF
  36        CONTINUE

          ENDIF
   6    CONTINUE 

C Return to main data editing menu after warning user.
        call usrmsg(
     &    'Imported data is currently in memory. Remember to use the',
     &    'save->save asci command after you exit from this menu.','W')
        return
      ELSEIF(INODA.EQ.IMPTL-3)THEN

C Preview import file.
        IUNIT=IFIL+4
        CALL EFOPSEQ(IUNIT,LIMP,1,IER)
        IF(IER.LT.0)THEN
          msgl2=' Problem detected... '
          CALL USRMSG(' ',msgl2,'W')
          INODA=-4
          GOTO 28
        ENDIF

C Actual read of ASCII import file.  
        K=0
    7   READ(IUNIT,10,IOSTAT=ISTAT,END=102)OUTSTR
   10   FORMAT(A124)
        K=K+1
        CALL  EDISP(IUOUT,OUTSTR)
        IF(K.LT.8)GOTO 7
        CALL EDISP(IUOUT,'  .......  ')
        CALL EDISP(IUOUT,' ')
        CALL ERPFREE(IUNIT,ISTAT)
        GOTO 28

C End of file encountered, close the file and return to menu.
  102   msgl2=' End of file reached'
        CALL USRMSG(' ',msgl2,'-')
        CALL ERPFREE(IUNIT,ISTAT)
      ELSEIF(INODA.GT.MHEAD.AND.INODA.LT.(IMPTL-MCTL+1))THEN

C Specify the item number to be read in for each of the fields.
C If 0 then ignore.
        IFLD=INODA-MHEAD
        IV=IMSTC(IFLD)
        CALL EASKI(IV,' ','Associated item number in import file:',
     &    0,'F',0,'-',1,'associated item',IER,nbhelp)
        IMSTC(IFLD)=IV
      ELSE
        INODA=-1
        GOTO 13
      ENDIF
      INODA=-4
      GOTO 28  

C End of file encountered, close the file and return to menu.
  103 msgl2=' End of import file reached'
      CALL USRMSG(' ',msgl2,'-')
      CALL ERPFREE(IUNIT,ISTAT)
      GOTO 28          

      END
