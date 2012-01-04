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

C edpro.F manages the editing of event profiles databases
C         which used to be found in separate source files
C         in esrupro

C  CASFMK:  Manage event profiles databases.
C  EDPROF:  Mid level profile editing facility.
C  EDEVENT: Edit details of a profile event.
C  TRIPRO:  Reads data supplied from an ASCII profiles db and writes
C           it to a NEW binary events profiles database file.
C  TRIPROBIN: Fills common blocks from scan of binary profiles db.
C  TRIPROCOM: Reads data supplied from an ASCII profiles db into
C           common blocks.
C  TROPRO:  Creates an ASCII profiles db from the binary profiles db.
C  TROPROCOM:  Creates an ASCII profiles db from common blocks.
C  PROGRF:  Outputs a graphical display of an event profile (IPRO).

C Event profiles databases were binary databases with an ASCII equivalent
C and it both cases data is held in common blocks. As with the materials
C database once the common blocks have been filled then subsequent changes
C can be saved to the ASCII version and gradually all events databases
C become ASCII and the binary is only a function of the database management
C facility rather than the editing menus.

C ************* CASFMK
C Event profile input/edit control routine. Assume that the
C common blocks for the event profiles have already been
C filled prior to calling CASFMK.
C These profile can be used to aid the input of:
C   casual gains to the operations file,
C   air flow patterns to the operations file,
C   control information to the configuration control file.
      SUBROUTINE  CASFMK(ier)
#include "epara.h"
#include "esprdbfile.h"
#include "profile.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      DIMENSION ITEM(35)
      CHARACTER outs*124,KEY*1,ITEM*42
      character LLASCI*144
      character ermsg*48  ! for read error messages
      character fs*1

      logical asciupdate   ! to signal if user has edited data.
      logical unixok  ! to check for database path file separators
      integer MVERT,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CASFMK'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C If there is at least one profile then the common blocks have
C been filled. 
  77  if(NPDBITEMS.le.0)then
        CALL USRMSG(
     &    'There were no profiles in the database. You are being ',
     &    'returned to the main menu so you can try again.','W')
        ier=2
        return
      endif

C Setup for multi page menu.
      MHEAD=3
      MCTL=5
      ILEN=NPDBITEMS
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Menu call back loop start point.
   92 IER=0
      ILEN=NPDBITEMS
      INO=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu.

C << if we move to hundreds of profiles then I3 needs to be I4 >>

    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(ITEM(M),303)KEY,NCG(L),PDBDESC(L)(1:36)
  303     FORMAT(A1,I3,2X,A)
        ENDIF
   10 CONTINUE

      write(ITEM(1),'(A,I3)')' number of profiles: ',NPDBITEMS
      ITEM(2)=               ' __________________________________ '
      ITEM(3)=               ' periods| description               '

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        ITEM(M+1)=           '  __________________________________ '
      ELSE
        WRITE(ITEM(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page --- Part: ',I2,' of ',I2,' ---')
      ENDIF
      ITEM(M+2)  ='* add a profile                   '
      ITEM(M+3)  ='! list profiles in database       '
      ITEM(M+4)  ='? help                            '
      ITEM(M+5)  ='- exit                            '

      helptopic='events_db_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU(' Event profiles',ITEM,MVERT,INO)
      IF(INO.EQ.MVERT)THEN
        return
      ELSEIF(INO.EQ.(MVERT-1))THEN
        helptopic='events_db_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('event profiles',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.(MVERT-2))THEN

C List contents of database.
        WRITE(outs,345) NPDBITEMS
  345   FORMAT('There are ',I4,' profiles in this database')
        call edisp(iuout,outs)
        call edisp(iuout,'their order and description now follow:')
        call edisp(iuout,' ')
        call edisp(iuout,'Profile|events|description')
        DO 158 I=1,NPDBITEMS
          WRITE(outs,'(I7,I7,2X,A40)')I,NCG(I),PDBDESC(I)
          call edisp(iuout,outs)
  158   CONTINUE
        call tstamp('>','PRO: list database contents')
      ELSEIF(INO.EQ.(MVERT-3))THEN

C Insert a profile.
        NPDBITEMS = NPDBITEMS + 1
        WRITE(outs,'(a,I3)') ' Profile  ',NPDBITEMS
        call edisp(iuout,outs)

        write(outs,'(a,I3,a)') 'Between 1 and ',MPRFDBPER,
     &    ' events can be acommodated.'
        call edisp(iuout,outs)
        IV=NCG(NPDBITEMS)
        CALL EASKI(IV,' ','Number of events in profile: ',
     &       1,'F',MPRFDBPER,'F',1,'no events',IERI,nbhelp)
        if(ieri.eq.-3) then
          NPDBITEMS = NPDBITEMS - 1
          goto 92
        else

C Instanciate the data.
          NCG(NPDBITEMS)=IV
          PDBDESC(NPDBITEMS)='New profile'
	  
          IPACT=CREATE
          CALL EKPAGE(IPACT)
          CALL EPMENSV

C Because we are creating a new profile set asciupdate true.
C And pass 'N' so that some initial values for the periods
C can be filled in.
          lndbp=lnblnk(standarddbpath)
          asciupdate=.true.
          CALL EDPROF(NPDBITEMS,'N',asciupdate,IER)
          if(asciupdate)then

C If update needed write ascii file from commons and
C then jump back to 77 to check the number of items.
C If LPRFDB does not end in .a then update LLASACI
            llt=lnblnk(LPRFDB)
	    if(LPRFDB(llt-1:llt).eq.'.a')then
              if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
              LLASCI=LPRFDB
              elseif(ipathprodb.eq.2)then
                write(LLASCI,'(3a)') standarddbpath(1:lndbp),fs,
     &            LPRFDB(1:lnblnk(LPRFDB))
              endif
            else
              if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
                write(LLASCI,'(2a)') LPRFDB(1:lnblnk(LPRFDB)),'.a'
              elseif(ipathprodb.eq.2)then
                lndbp=lnblnk(standarddbpath)
                write(LLASCI,'(4a)') standarddbpath(1:lndbp),fs,
     &            LPRFDB(1:lnblnk(LPRFDB)),'.a'
              endif
            endif
            IAF=IFIL+1
            CALL TROPROCOM(IAF,LLASCI,IER)
            if(ier.eq.0)then
              call edisp(iuout,'Event data ASCII version placed in...')
              call edisp248(iuout,LLASCI,100)

C Set the LPRFDB common block string via call to findwhichdbpath.
              call findwhichdbpath('evn',LLASCI,ier)
            endif
          endif
          CALL EPMENRC
          CALL USRMSG(' ',' ','-')
          call tstamp('>','PRO: insert a new db item')
          GOTO 77
        endif
      ELSEIF(INO.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(INO.GT.MHEAD.AND.INO.LT.(MVERT-MCTL+1))THEN

C Edit profile identified by KEYIND.
        call tstamp('>','PRO: edit database entry')
        CALL KEYIND(MVERT,INO,IFOC,IO)
        CALL EPMENSV
        asciupdate=.false.
        CALL EDPROF(IFOC,'E',asciupdate,IER)
        if(asciupdate)then

C If update needed write ascii file from common blocks
C then jump back to 77 to rescan the header of the database.
          lndbp=lnblnk(standarddbpath)
          llt=lnblnk(LPRFDB)
	  if(LPRFDB(llt-1:llt).eq.'.a')then
            if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
          LLASCI=LPRFDB
            elseif(ipathprodb.eq.2)then
              write(LLASCI,'(3a)') standarddbpath(1:lndbp),fs,
     &          LPRFDB(1:lnblnk(LPRFDB))
            endif
          else
            if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
              write(LLASCI,'(2a)') LPRFDB(1:lnblnk(LPRFDB)),'.a'
            elseif(ipathprodb.eq.2)then
              lndbp=lnblnk(standarddbpath)
              write(LLASCI,'(4a)') standarddbpath(1:lndbp),fs,
     &          LPRFDB(1:lnblnk(LPRFDB)),'.a'
            endif
          endif
          IAF=IFIL+1
          CALL TROPROCOM(IAF,LLASCI,IER)
          if(ier.eq.0)then
            call edisp(iuout,'Events ASCII version updated in...')
            call edisp248(iuout,LLASCI,100)
          endif
        endif
        CALL EPMENRC
        goto 77
      else
        GOTO 92
      endif
      INO=-2
      goto 3

      END

C *********** EDPROF
C Mid level profile editing facility.  In the case of a new profile
C dummy data is displayed first. NFLAG='N' new profile, NFLAG='E'
C existing profile. Asciupdate is set to true if the profile has
C been edited so that the calling routine will know to write out
C a fresh ASCII version file.

C The timing of profiles assumes that the end of one event coincides
C with the start of the next event. Currently timings are in hours
C but this will change to real numbers in the future.

C << there is a name clash for IGS1 and ICGF1 with
C << the common block P3

C << introduce concept of profiles with multiple day types as well
C << as multiple casual gain types

      SUBROUTINE  EDPROF(ITM,NFLAG,asciupdate,IER)
#include "epara.h"
#include "esprdbfile.h"
#include "profile.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      DIMENSION ITEM(30)
      character NFLAG*1
      CHARACTER KEY*1,TMP*40
      character ITEM*33,cstr*72
      character llasci*144 ! for writing the ascii file.
      character ermsg*48   ! for read error messages
      character outs*124   ! string buffer for messages
      character fs*1       ! file separator
      logical asciupdate   ! signal if user edits data
      logical unixok  ! to check for database path file separators
      integer MVERT,INO ! max items and current menu item

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDPROF'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Initailly set asciupdate toggle to false (no changes made yet).
      asciupdate=.false.

C If existing - use data in common block for the profile and display.
      if(NFLAG.EQ.'E')then
         continue
      elseif(NFLAG.EQ.'N')then

C Loop through each of the periods and instanciate with default values
C before passing into the standard editing facilties.  Make the start
C of each successive event equal to the finish of the previous.
        if(NCG(ITM).lt.24)incr=1
        if(NCG(ITM).lt.12)incr=2
        if(NCG(ITM).lt.8)incr=3
        if(NCG(ITM).lt.4)incr=6
        DO 100 I = 1,NCG(ITM)
          if(I.eq.1)then
            ICGS1(ITM,I)=0
          else
            ICGS1(ITM,I)=ICGF1(ITM,I-1)
          endif
          ICGF1(ITM,I)=ICGS1(ITM,I)+incr
          IF(ICGF1(ITM,I).gt.24)ICGF1(ITM,I)=24
          CGS1(ITM,I)=0.
          CGL1(ITM,I)=0.
100     CONTINUE

C Make up an editing string for the start of each period.
        CSTR=' '
        K=1
        DO 60 J=1,NCG(ITM)
          K1=K+2
          WRITE(CSTR(K:K1),'(i2)')ICGS1(ITM,J)
          K=K1+1
   60   CONTINUE

        helptopic='events_db_start'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKS(CSTR,' Start hour of each event',
     &    ' ',72,' 0  4   6   8','profile starts',IER,nbhelp)

C Extract the start timings and then sort out the finishes.
        K=0
        DO 61 J=1,NCG(ITM)
          CALL EGETWI(cstr,K,IV,0,24,'W','event start',IER)
          ICGS1(ITM,J)=IV
   61   CONTINUE
        DO 62 J=1,NCG(ITM)
          if(j.ne.NCG(ITM))then
            ICGF1(ITM,J)=ICGS1(ITM,J+1)
          else
            ICGF1(ITM,J)=24
          endif
   62   CONTINUE
      endif

C Setup for multi page menu.
      MHEAD=4
      MCTL=5
      ILEN=NCG(ITM)
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=NCG(ITM)
      INO=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(M,KEY,IER)
          WRITE(ITEM(M),303)KEY,L,ICGS1(ITM,L),
     &      CGS1(ITM,L),CGL1(ITM,L)
  303     FORMAT(A1,I4,I6,F10.2,F9.2)
        ENDIF
   10 CONTINUE

C << number of day types to be added and the description extended
C << add logic depending on the type of profile

      write(ITEM(1),'(2A)')  'a profile: ',PDBDESC(ITM)(1:20)
      write(ITEM(2),'(A,I3)')'  number of periods: ',NCG(ITM)
      ITEM(3)=               '  ______________________________'
      ITEM(4)=               '  event|start|sensible%|latent% '         

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        ITEM(M+1)=           '  ____________________________'
      ELSE
        WRITE(ITEM(M+1),15)IPM,MPM 
   15   FORMAT   ('0 Page --- Part: ',I2,' of ',I2,' ---')
      ENDIF
      ITEM(M+2)  ='! list current profile          '
      ITEM(M+3)  ='> save current profile into db  '
      ITEM(M+4)  ='? help                          '
      ITEM(M+5)  ='- exit                          '

C If in graphic mode graph the current profile.
      if(MMOD.ge.8)then
        CALL USRMSG(' ',' ','-')
        CALL PROGRF(ITM)
      endif

C Display information about the profile.
      if(NFLAG.EQ.'E')then
        WRITE(outs,5120)  ITM,PDBDESC(ITM)
        call edisp(iuout,outs)
        WRITE(outs,5130)  NCG(ITM)
        call edisp(iuout,outs)
        call edisp(iuout,' ')
        call edisp(iuout,
     &  'Period  From    First     Second')
        call edisp(iuout,
     &  '        Hour      %          %  ')
        DO 291 I = 1,NCG(ITM)
          WRITE(outs,5151)  I,ICGS1(ITM,I),CGS1(ITM,I),CGL1(ITM,I)
5151      FORMAT(I4,I7,F10.2,F10.2)
          call edisp(iuout,outs)
291     CONTINUE
      endif

C Help text for this menu.
      helptopic='events_db_start'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('  Profile editing',ITEM,MVERT,INO)
      IF(INO.EQ.MVERT)THEN

C If in graphic mode restore the standard win3d values for prj.
        if(MMOD.ge.8)then

C Setup and pass in parameters to win3d.
          iiw1=4; iiw2=1; iiw3=1; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
C          CALL win3d(menuchw,4,1,1,3,igl,igr,igt,igb,igw,igwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
        endif
        RETURN
      ELSEIF(INO.EQ.(MVERT-1))THEN

C HELP information.
        helptopic='events_db_start'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('pro edit',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.(MVERT-2))THEN

C Update the ascii database in all cases.
        lndbp=lnblnk(standarddbpath)
        llt=lnblnk(LPRFDB)
        if(LPRFDB(llt-1:llt).eq.'.a')then
          if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
            LLASCI=LPRFDB
          elseif(ipathprodb.eq.2)then
            write(LLASCI,'(3a)') standarddbpath(1:lndbp),fs,
     &        LPRFDB(1:lnblnk(LPRFDB))
          endif
        else
          if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
            write(LLASCI,'(2a)') LPRFDB(1:lnblnk(LPRFDB)),'.a'
          elseif(ipathprodb.eq.2)then
            lndbp=lnblnk(standarddbpath)
            write(LLASCI,'(4a)') standarddbpath(1:lndbp),fs,
     &        LPRFDB(1:lnblnk(LPRFDB)),'.a'
          endif
        endif
          IAF=IFIL+1
          CALL TROPROCOM(IAF,LLASCI,IER)
          if(ier.eq.0)then
            call edisp(iuout,'Event data updated...')
            call edisp248(iuout,LLASCI,100)

C Set the LPRFDB common block string via call to findwhichdbpath.
          call findwhichdbpath('evn',LLASCI,ier)
            asciupdate=.false.   ! no subsequent update of ascii file needed.
          call edisp(iuout,'Be sure to UPDATE the model cfg file')
          call edisp(iuout,'to reflect changes in events db.')
          endif

      ELSEIF(INO.EQ.(MVERT-3))THEN

C List current profile.
        WRITE(outs,5120)  ITM,PDBDESC(ITM)
5120    FORMAT(' Profile ',I3,5X,A40)
        call edisp(iuout,outs)
        WRITE(outs,5130)  NCG(ITM)
5130    FORMAT(' Number of periods = ',I4)
        call edisp(iuout,outs)
        call edisp(iuout,' ')
        call edisp(iuout,
     &  'Period  From    First     Second')
        call edisp(iuout,
     &  '        Hour      %          %  ')
        DO 290 I = 1,NCG(ITM)
          WRITE(outs,5151)  I,ICGS1(ITM,I),CGS1(ITM,I),
     &      CGL1(ITM,I)
          call edisp(iuout,outs)
290     CONTINUE
      ELSEIF(INO.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      elseif(INO.EQ.1)then

C New profile name.
        TMP=PDBDESC(ITM)
        CALL EASKS(TMP,' Description ? ',
     &    ' ',40,'new profile','profile descr',IER,nbhelp)
        PDBDESC(ITM)=TMP
        CALL USRMSG(' ',' ','-')
      ELSEIF(INO.GT.MHEAD.AND.INO.LT.(MVERT-MCTL+1))THEN

C Edit profile identified by KEYIND.
        CALL KEYIND(MVERT,INO,IFOC,IO)
        CALL EDEVENT(ITM,IFOC,IER)
      endif
      INO=-4
      GOTO 92

      END

C ************* EDEVENT
C EDEVENT: Edit details of a profile event.
      SUBROUTINE EDEVENT(IPR,IFOC,IER)
#include "esprdbfile.h"
#include "profile.h"
      COMMON/OUTIN/IUOUT,IUIN
      character outs*124
      logical shiftnext

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDEVENT'  ! set for subroutine

C << probably need to pass in the day type index as well >>

C Set notification of need to shift next period start to false
      shiftnext=.false.

  10  IER=0
      WRITE(outs,'(A,I2,A,A)') ' Event - ',IFOC,' in ',PDBDESC(IPR)
      call edisp(iuout,' ')
      call edisp(iuout,outs)

  11  IES=ICGS1(IPR,IFOC)
      helptopic='events_item_edit'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKI(IES,outs,' start hour : ',
     &    0,'F',23,'F',0,'start',IERI,nbhelp)
      if(ieri.eq.-3) return

C Checks that need to be made on the start time: a) is it between
C the start times of the previous and the next period, b) is it
C not exactly the same as another start time.

      if(IFOC.gt.1.and.IES.lt.ICGF1(IPR,IFOC-1))then
        call usrmsg('The start time overlaps previous event...',
     &              'you must respecify.','W')
        goto 11
      endif
      CALL USRMSG(' ',' ','-')

  12  IEF=ICGF1(IPR,IFOC)
      CALL EASKI(IEF,outs,' finish hour: ',
     &    IES,'F',24,'F',0,'finish',IERI,nbehlp)
      if(ieri.eq.-3) return

      if(IFOC.lt.NCG(IPR).and.IEF.gt.ICGS1(IPR,IFOC+1))then
        CALL EASKAB(' Finish time overlaps next event...',
     &     ' ','respecify finish','shift next event start',
     &     IW,nbhelp)
        if(IW.eq.1)goto 12
        shiftnext=.true.
      endif
      CALL USRMSG(' ',' ','-')

      EC=CGS1(IPR,IFOC)
      CALL EASKR(EC,' ',' First value (sensible) percentage ? ',
     &   0.,'F',100.,'F',50.,'first value',IER,nbhelp)

      EL=CGL1(IPR,IFOC)
      CALL EASKR(EL,' ',' Second value (latent) percentage ? ',
     &   0.,'F',100.,'F',50.,'first value',IER,nbhelp)

      if(IER.ne.0)then
        CALL usrmsg('A problem was detected during input...',
     &              'Please repeat the specification.','W')
        goto 10
      endif

C If we got to this point we can instanciate the data.
      ICGS1(IPR,IFOC)=IES
      ICGF1(IPR,IFOC)=IEF
      if(shiftnext) ICGS1(IPR,IFOC+1)=IEF
      CGS1(IPR,IFOC)=EC
      CGL1(IPR,IFOC)=EL

      RETURN
      END


C ***************** TRIPRO 
C TRIPRO reads data supplied from an ASCII profiles db (LASCI) and writes
C it to a NEW binary events profiles database file (LBIN).
      SUBROUTINE TRIPRO(IFA,IFB,LASCI,LBIN,IER)
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)

#include "espriou.h"
C espriou.h provides currentfile.
#include "profile.h"

C << the ASCII file should have a header with a version number as well
C << as a date stamp a block of documentation but none of this would
C << fit in the binary file
      
      integer lnblnk  ! function definition

C Parameters
      integer IFA,IFB       ! file unit numbers  
      CHARACTER*(*) LASCI   ! ascii file name (full path)
      CHARACTER*(*) LBIN    ! binary file name (full path)
      integer IER           ! zero is ok, one is asci does not exist
                            ! two is binary cannot be opened

      COMMON/OUTIN/IUOUT,IUIN
      CHARACTER OUTSTR*124
      character ermsg*48   ! for read error messages
      character outs*124   ! string buffer for messages

C Note existing binary files will be overwritten.
      CALL ERPFREE(IFB,ISTAT)
      ier=0
      call EFOPRAN(IFB,LBIN,10,3,IER)
      IF(IER.NE.0)THEN
        CALL USRMSG(' ','Binary file cannot be written!','W')
        IER=2
        RETURN
      ENDIF
        if(ier.ne.0)return
      write(currentfile,'(a)') LASCI(1:lnblnk(LASCI))

C Open the ASCII file for reading.
      CALL EFOPSEQ(IFA,LASCI,1,IER)
      IF(IER.NE.0)THEN
        CALL USRMSG(' ',' ASCII file does not exist!','W')
        IER=1
        RETURN
      ENDIF

C Proceed.
      CALL STRIPC(IFA,OUTSTR,0,ND,1,'no items',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETWI(OUTSTR,K,NPDBITEMS,0,0,'-','no items',IER)

      ermsg='writing record one nb of profiles '
      IREC = 1
      WRITE(IFB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NPDBITEMS

C Get number of events and description of profile (EGETRM gets the
C remainder of the line after the first word (NCG).
      DO 10 I = 1,NPDBITEMS
        CALL STRIPC(IFA,OUTSTR,0,ND,1,'NCG,PDBDESC',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(OUTSTR,K,NCG(I),0,0,'-','NCG',IER)
        CALL EGETRM(OUTSTR,K,PDBDESC(I),'W','profile descr',IER)

        write(ermsg,'(a,i3)') 'writing nb of periods in profile ',I
        IREC = IREC + 1
        WRITE(IFB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NCG(I)
        write(ermsg,'(a,i3)') 'writing description of profile ',I
        IREC = IREC + 1
        WRITE(IFB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  PDBDESC(I)

        DO 20 J = 1,NCG(I)
          CALL STRIPC(IFA,OUTSTR,0,ND,1,'IS,IF,A,B',IER)
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWI(OUTSTR,K,IS,0,0,'-','Start',IER)
          CALL EGETWI(OUTSTR,K,IF,0,0,'-','Finish',IER)
          CALL EGETWR(OUTSTR,K,A,0.,0.,'-','VAL 1',IER)
          CALL EGETWR(OUTSTR,K,B,0.,0.,'-','VAL 2',IER)
          write(ermsg,'(a,2i4)') 'writing data of profile ',I,J
          IREC = IREC + 1
          WRITE(IFB,REC=IREC,IOSTAT=ISTAT,ERR=1001)IS,IF,A,B
   20   CONTINUE
   10 CONTINUE

      ermsg='writing record one nb of profiles & last rec '
      IR=IREC+1
      IREC = 1
      WRITE(IFB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NPDBITEMS,IR

C Free both file units.
  99  CALL ERPFREE(IFB,ISTAT)
      CALL ERPFREE(IFA,ISTAT)
      RETURN

 1001 write(outs,'(3a,i4)') 'In profile db: error ',
     &  ermsg(1:lnblnk(ermsg)),' at record ',irec
      call edisp(iuout,outs)
      goto 99

      END

C ***************** TRIPROCOM 
C TRIPROCOM reads data supplied from an ASCII profiles db into common.
      SUBROUTINE TRIPROCOM(IFA,LLASCI,IER)
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)

#include "espriou.h"
C espriou.h provides currentfile.
#include "profile.h"

C << the ASCII file should have a header with a version number as well
C << as a date stamp a block of documentation and categories
      
      integer lnblnk  ! function definition

      CHARACTER OUTSTR*124,LLASCI*144
      character ermsg*48   ! for read error messages

C Free file unit
      CALL ERPFREE(IFA,ISTAT)
      write(currentfile,'(a)') LLASCI(1:lnblnk(LLASCI))

C Open the ASCII file for reading.
      CALL EFOPSEQ(IFA,LLASCI,1,IER)
      IF(IER.NE.0)THEN
        CALL USRMSG(' ',' ASCII file does not exist!','W')
        IER=1
        RETURN
      ENDIF

C Proceed.
      CALL STRIPC(IFA,OUTSTR,0,ND,1,'no items',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETWI(OUTSTR,K,NPDBITEMS,1,MGIT,'W',
     &  'number of event profiles',IER)

C Get number of events and description of profile (EGETRM gets the
C remainder of the line after the first word (NCG).
      DO 10 I = 1,NPDBITEMS
        CALL STRIPC(IFA,OUTSTR,0,ND,1,'NCG,PDBDESC',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(OUTSTR,K,NCG(I),1,MPRFDBPER,'W',
     &    'Number of periods in an event item',IER)
        CALL EGETRM(OUTSTR,K,PDBDESC(I),'W','profile descr',IER)
        DO 20 J = 1,NCG(I)
          CALL STRIPC(IFA,OUTSTR,0,ND,1,'IS,IF,A,B',IER)
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWI(OUTSTR,K,IS,0,0,'-','Start',IER)
          CALL EGETWI(OUTSTR,K,IF,0,0,'-','Finish',IER)
          CALL EGETWR(OUTSTR,K,A,0.,0.,'-','VAL 1',IER)
          CALL EGETWR(OUTSTR,K,B,0.,0.,'-','VAL 2',IER)
          ICGS1(I,J)=IS
          ICGF1(I,J)=IF
          CGS1(I,J)=A
          CGL1(I,J)=B
   20   CONTINUE
   10 CONTINUE

      CALL ERPFREE(IFA,ISTAT)
      RETURN

      END

C **************** TROPRO 
C TROPRO creates an ASCII profiles db from the binary profiles db.
C This is called only by esrudbm.
      SUBROUTINE TROPRO(IFA,LASCI,IER)
#include "esprdbfile.h"
#include "espriou.h"
#include "profile.h"
      
      integer lnblnk  ! function definition
      CHARACTER*(*) LASCI   ! ascii file name

      COMMON/OUTIN/IUOUT,IUIN
      character ermsg*48   ! for read error messages
      character outs*124   ! string buffer for messages
      character fs*1       ! file separator
      character lworking*144 ! file name
      logical unixok  ! to check for database path file separators

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      IER =0

C Because it is a binary file we are expecting a non-zero ier
C does not require that we attempt to scan an ascii version of
C the event profiles db.
      lndbp=lnblnk(standarddbpath)
      CALL ERPFREE(IPRODB,ISTAT)
      if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
        ier=0
      call EFOPRAN(IPRODB,LPRFDB,10,1,IER)
      elseif(ipathprodb.eq.2)then
        ier=0
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    LPRFDB(1:lnblnk(LPRFDB))
        call EFOPRAN(IPRODB,lworking,10,1,IER)
      endif
      if(ier.ne.0)return

      ermsg='reading record one number of profiles '
      IREC=1
      READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)NPDBITEMS
      IF(NPDBITEMS.LE.0)THEN
        CALL USRMSG(' ',' Events profiles db is empty !','W') 
        RETURN
      ENDIF

C Open or overwrite the ASCII file.
      CALL EFOPSEQ(IFA,LASCI,4,IER)
      IF(IER.NE.0)THEN
        CALL USRMSG(' Could not open ASCII file!',LASCI,'W')
        IER=1
        RETURN
      ENDIF
      write(currentfile,'(a)') LASCI(1:lnblnk(LASCI))
      
C Write out a file header.
      WRITE(IFA,30)LASCI(1:lnblnk(LASCI))
  30  FORMAT('# events profiles db defined in ',A)

C Proceed
      ermsg='reading record one number of profiles '
      IREC=1
      READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NPDBITEMS

      WRITE(IFA,21,IOSTAT=ISTAT,ERR=1000)NPDBITEMS
   21 FORMAT(I5,'    # Number of profiles')

      DO 10 I = 1,NPDBITEMS
        write(ermsg,'(a,i2)') 'reading number events in profile ',I
        IREC = IREC + 1
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NCG(I)
        write(ermsg,'(a,i2)') 'reading description of profile ',I
        IREC = IREC + 1
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  PDBDESC(I)

        WRITE(IFA,22)NCG(I),PDBDESC(I)
   22   FORMAT(I5,3X,A40,'  # No periods & description',/,
     &                   '# start finish data data')

C For each period.
        DO 20 J = 1,NCG(I)
          write(ermsg,'(a,i3,a,i2)') 'reading data of profile ',I,
     &      ' at period ',J
          IREC = IREC + 1
          READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)IS,IF,A,B
          ICGS1(I,J)=IS
          ICGF1(I,J)=IF
          CGS1(I,J)=A
          CGL1(I,J)=B
          WRITE(IFA,23,IOSTAT=ISTAT,ERR=1000)IS,IF,A,B
   23     FORMAT(I4,',',I4,',',F8.3,',',F8.3)
   20   CONTINUE
   10 CONTINUE

  99  CALL ERPFREE(IPRODB,ISTAT)
      CALL ERPFREE(IFA,ISTAT)
      RETURN

 1000 CALL USRMSG(' ',' ASCII file write error in ','W')
      goto 99

 1001 write(outs,'(3a,i4)') 'In profile db: error ',
     &  ermsg(1:lnblnk(ermsg)),' at record ',irec
      call edisp(iuout,outs)
      goto 99

      END

C **************** TRIPROBIN 
C TRIPROBIN fills common blocks from a binary profiles db.
C Pass back the following error states:
C ier= 0 file read ok.
C ier= -1 open file error
C ier= -2 error opening or reading header
C ier= -3 no items in the database
      SUBROUTINE TRIPROBIN(IFB,LBIN,IER)
#include "esprdbfile.h"
#include "espriou.h"
#include "profile.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer IFB         ! file unit
      CHARACTER*(*) LBIN  ! binary file name (full path)
      integer IER         ! zero is ok, -2 or -3 errors

      COMMON/OUTIN/IUOUT,IUIN
      character ermsg*48   ! for read error messages
      character outs*124   ! string buffer for messages

C Because it is a binary file we are expecting a non-zero ier
C does not require that we attempt to scan an ascii version of
C the event profiles db. But if there is an error in efopran or
C there is an impossible number of items pass back ier=-2 so
C other code can check if the file is an ascii version.
      CALL ERPFREE(IFB,ISTAT)
      ier=0
      call EFOPRAN(IFB,LBIN,10,1,IER)
      if(ier.ne.0)then
        ier=-1
        return
      endif

      ermsg='reading record one number of profiles '
      IREC=1
      READ(IFB,REC=IREC,IOSTAT=ISTAT,ERR=1001)NPDBITEMS
      if(NPDBITEMS.LE.0)then
        CALL USRMSG(' ','Events profiles db is empty !','W') 
        ier=-3
        return
      elseif(NPDBITEMS.gt.MGIT)then
        npdbitems=0   ! reset 
        ier=-2
        return
      endif

      write(currentfile,'(a)') LPRFDB(1:lnblnk(LPRFDB))
      
C Proceed
      ermsg='reading record one number of profiles '
      IREC=1
      READ(IFB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NPDBITEMS

      DO 10 I = 1,NPDBITEMS
        write(ermsg,'(a,i2)') 'reading number events in profile ',I
        IREC = IREC + 1
        READ(IFB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NCG(I)
        write(ermsg,'(a,i2)') 'reading description of profile ',I
        IREC = IREC + 1
        READ(IFB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  PDBDESC(I)

C For each period.
        DO 20 J = 1,NCG(I)
          write(ermsg,'(a,i3,a,i2)') 'reading data of profile ',I,
     &      ' at period ',J
          IREC = IREC + 1
          READ(IFB,REC=IREC,IOSTAT=ISTAT,ERR=1001)IS,IF,A,B
          ICGS1(I,J)=IS
          ICGF1(I,J)=IF
          CGS1(I,J)=A
          CGL1(I,J)=B
   20   CONTINUE
   10 CONTINUE

  99  CALL ERPFREE(IFB,ISTAT)
      RETURN

 1001 write(outs,'(3a,i4)') 'In profile db: error ',
     &  ermsg(1:lnblnk(ermsg)),' at record ',irec
      call edisp(iuout,outs)
      goto 99

      END

C **************** TROPROCOM 
C TROPROCOM creates an ASCII profiles db from common blocks.
      SUBROUTINE TROPROCOM(IFA,LLASCI,IER)
#include "esprdbfile.h"
#include "espriou.h"
#include "profile.h"

C << the ASCII file should have a header with a version number as well
C << as a date stamp a block of documentation and categories
C << and a type indicator for each profile. Legacy profiles are ?
C << a subset of activities profiles.  Use standard delimeter code
C << to pack the output.
      
      integer lnblnk  ! function definition
      CHARACTER*(*) LLASCI   ! ascii file name

      COMMON/OUTIN/IUOUT,IUIN
      character ermsg*48   ! for read error messages

      IER =0

      IF(NPDBITEMS.LE.0)THEN
        CALL USRMSG(' ',' Events profiles db is empty !','W') 
        IER=1
        RETURN
      ENDIF

C Open or overwrite the ASCII file.
      CALL EFOPSEQ(IFA,LLASCI,4,IER)
      IF(IER.NE.0)THEN
        CALL LUSRMSG(' Could not open ASCII file!',LLASCI,'W')
        IER=1
        RETURN
      ENDIF
      write(currentfile,'(a)') LLASCI(1:lnblnk(LLASCI))
      
C Write out a file header.
      WRITE(IFA,30)LLASCI(1:lnblnk(LLASCI))
  30  FORMAT('# events profiles db defined in ',A)

C Proceed
      WRITE(IFA,21,IOSTAT=ISTAT,ERR=1000)NPDBITEMS
   21 FORMAT(I5,'    # Number of profiles')

      DO 10 I = 1,NPDBITEMS
        WRITE(IFA,22)NCG(I),PDBDESC(I)
   22   FORMAT(I5,3X,A40,'  # No periods & description',/,
     &                   '# start finish data data')

C For each period.
        DO 20 J = 1,NCG(I)
          WRITE(IFA,23,IOSTAT=ISTAT,ERR=1000)ICGS1(I,J),ICGF1(I,J),
     &      CGS1(I,J),CGL1(I,J)
   23     FORMAT(I4,',',I4,',',F8.3,',',F8.3)
   20   CONTINUE
   10 CONTINUE

  99  CALL ERPFREE(IFA,ISTAT)
      RETURN

 1000 CALL USRMSG(' ',' ASCII file write error in ','W')
      goto 99

      END

C *********** PROGRF
C 'PROGRF' outputs a graphical display of an event profile (IPRO)
C based on the current common block data.
      SUBROUTINE PROGRF(IPRO)
#include "esprdbfile.h"
#include "profile.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

      CHARACTER ETEXT*72
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer iupdown,isym    ! passed to etplot
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

      DIMENSION VAL1(MPRFDBPER),VAL2(MPRFDBPER)

      IF(IPRO.LT.1.OR.IPRO.GT.NPDBITEMS)then
        call usrmsg(' ',' Profile requested not within db','W')
        return
      endif

      YMAX=100.
      YMIN=0.

C Set up data ready for graph drawing.
      call startbuffer

C Setup and pass in parameters to win3d.
      iiw1=10; iiw2=10; iiw3=4; iiw4=4; iimenu=menuchw
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
C      call win3d(menuchw,10,10,4,4,igl,igr,igt,igb,igw,igwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,0.0,24.0,YMIN,YMAX,xsc,ysc,sca,
     &  Xadd,Yadd)
      call linescale(iigl,Xadd,xsc,iigb,Yadd,ysc)
      call horaxis(0.,24.,iigl,iigr,iigb,Xadd,xsc,1,'Time (hours)')
      call vrtaxis(YMIN,YMAX,iigl,iigb,iigt,Yadd,ysc,0,0,'Percent')

C Pick up the data increment for the horizontal axis.
      call dinterval(0.,24.,DT,NDEC,1)

C Print header
      WRITE(etext,16)IPRO,PDBDESC(IPRO)
   16 FORMAT('Profile ',I3,': ',A40)
      iside=1
      isize=0
      ifont=ifs
      call viewtext(etext,iside,isize,ifont)

      DO 60 I=1,MPRFDBPER
        VAL1(I)=0.
        VAL2(I)=0.
   60 CONTINUE

c Assign hourly values of each event (from its start to finish time).
      DO 20 I=1,NCG(IPRO)
        DO 30 J=ICGS1(IPRO,I),ICGF1(IPRO,I)
          J1=J+1
          VAL1(J1)=CGS1(IPRO,I)
          VAL2(J1)=CGL1(IPRO,I)
   30   CONTINUE
   20 CONTINUE

c Plot VAL1 array
      IPEN=1
      DO 40 I=0,24
        MARK=0
        TI=FLOAT(I)
        IR=AMOD(TI,DT)+.001
        IF(IR.EQ.0)MARK=4
        IF(I.GT.0)goto 9
        iupdown=0
        isym=0
        CALL ETPLOT(TI,VAL1(I+1),iupdown,isym)
        goto 40
    9   iupdown=IPEN
        isym=MARK
        CALL ETPLOT(TI,VAL1(I+1),iupdown,isym)
   40 CONTINUE
      call forceflush()

c Plot VAL2 array
      IPEN=-6
      DO 50 I=0,24
        MARK=0
        TI=FLOAT(I)
        IR=AMOD(TI,DT)+.001
        IF(IR.EQ.0)MARK=9
        IF(I.GT.0)goto 11
        iupdown=0
        isym=0
        CALL ETPLOT(TI,VAL2(I+1),iupdown,isym)
        goto 50
   11   iupdown=IPEN
        isym=MARK
        CALL ETPLOT(TI,VAL2(I+1),iupdown,isym)
   50 CONTINUE
      call forceflush()
      return

      END

