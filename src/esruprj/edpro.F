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
C           it to a NEW events profiles database file.
C  TROPRO:  Creates an ASCII profiles db from the binary profiles db.
C  PROGRF:  Outputs a graphical display of an event profile (IPRO).

C ************* CASFMK
C Event profile input/edit control routine.
C These profile can be used to aid the input of:
C   casual gains to the operations file,
C   air flow patterns to the operations file,
C   control information to the configuration control file.
      SUBROUTINE  CASFMK(L144,ier)
#include "epara.h"
#include "esprdbfile.h"

      PARAMETER  (MC=35)
      common/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/PDBITM/NITEMS,lastrec,NCG(100),NO(100),DESC(100)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

      DIMENSION ITEM(35)
      CHARACTER DESC*40,outs*124,KEY*1,H*72,ITEM*38,L144*144,LASCI*72
      character ermsg*48  ! for read error messages

      logical asciupdate   ! to signal if user has edited data.

C L144 is a longer string buffer.
      IER=0
      llt=lnblnk(L144)

C Attempt to open the binary file and if successful scan the header
C for the number of items and their periods and descriptions to
C include in the menu. If a problem found with the file warn the
C user and return.
  77  call EFOPRAN(IPRODB,L144,10,1,IER)
      IF(ier.eq.0)THEN
        CALL USRMSG(' opened profiles db',' ','-')
        ermsg='reading record one number of items '
        IREC=1
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)NITEMS
        DO 58 I=1,NITEMS
          IREC=IREC+1
          NO(I)=IREC
          ermsg='reading number events in each profile '
          READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)NCG(I)
          IREC=IREC+1
          ermsg='reading profile description '
          READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)DESC(I)
          IREC=IREC+NCG(I)
   58   CONTINUE
        lastrec=IREC+1
        CALL USRMSG(' ',' ','-')
      ELSE
        CALL USRMSG(
     &    'The specified file was not found. You are being returned',
     &    'to the main menu so you can try again.','W')
        ier=2
        return
      endif

C Setup for multi page menu.
      MHEAD=3
      MCTL=5
      ILEN=NITEMS
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Menu call back loop start point.
   92 IER=0
      ILEN=NITEMS
      INO=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(ITEM(M),303)KEY,NCG(L),DESC(L)(1:32)
  303     FORMAT(A1,I3,2X,A)
        ENDIF
   10 CONTINUE

      write(ITEM(1),'(A,I3)')'  number of profiles: ',NITEMS
      ITEM(2)=               '  __________________________________ '
      ITEM(3)=               '  events| description                '

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

      H(1)= '   Events Database '
      H(3)= ' '
      H(4)= 'Profiles in this database are currently used to  '
      H(5)= 'assist in the defintion of zone operations.  The'
      H(6)= 'standard is found in /usr/esru/esp-r/databases '
      H(7)= ' '
      H(8)= ' The data requirements are summarised below :- '
      H(9)= ' '
      H(10)=' for each profile - number of events.'
      H(11)=' '
      H(12)=' for each event - start and finish hours'
      H(13)='                  percentage of first magnitude '
      H(14)='                  percentage of second magnitude '
      H(15)=' '
      H(16)='If you edit data in the database the file will be'
      H(17)='updated and a back-up ASCII file will be created.'

      CALL EMENU(' Event profiles',ITEM,MVERT,INO)
      IF(INO.EQ.MVERT)THEN
        return
      ELSEIF(INO.EQ.(MVERT-1))THEN
        CALL PHELPD('event profiles',17,'-',0,0,IER)
      ELSEIF(INO.EQ.(MVERT-2))THEN

C List contents of database.
        WRITE(outs,345)NITEMS
  345   FORMAT(' There are ',I4,' profiles in this database')
        call edisp(iuout,outs)
        call edisp(iuout,' their order and description now follow:')
        call edisp(iuout,' ')
        call edisp(iuout,' Profile|events|description')
        DO 158 I=1,NITEMS
          WRITE(outs,'(I7,I7,3X,A40)')I,NCG(I),DESC(I)
          call edisp(iuout,outs)
  158   CONTINUE
        call tstamp('>','PRO: list database contents')
      ELSEIF(INO.EQ.(MVERT-3))THEN

C Insert a profile. (lastrec is next available record).
        NO(NITEMS+1)=lastrec
        NITEMS = NITEMS + 1
        WRITE(outs,'(a,I4)') ' Profile  ',NITEMS
        call edisp(iuout,outs)

        H(1)='Between 1 and 35 events can be acommodated.'
        H(2)='An event is equivalent to a period of time with'
        H(3)='associated values. '
        IV=NCG(NITEMS)
        CALL EASKI(IV,' ',' Number of events in profile: ',
     &       1,'F',MC,'F',1,'no events',IER,3)
        NCG(NITEMS)=IV
        DESC(NITEMS)='New profile'
        IPACT=CREATE
        CALL EKPAGE(IPACT)
        CALL EPMENSV

C Because we are creating a new profile set asciupdate true.
        asciupdate=.true.
        CALL EDPROF(NITEMS,'N',asciupdate,IER)
        if(asciupdate)then

C If update needed free the binary file, do conversion and
C then jump back to 77 to rescan the header of the database.
          CALL ERPFREE(IPRODB,ISTAT)
          write(LASCI,'(A,A2)')LPRFDB(1:lnblnk(LPRFDB)),'.a'
          IAF=IFIL+1
          CALL TROPRO(iuout,IAF,LASCI,IER)
          if(ier.eq.0)then
            call edisp(iuout,'A backup ASCII version placed in...')
            call edisp(iuout,LASCI)
          endif
        endif
        CALL EPMENRC
        CALL USRMSG(' ',' ','-')
        call tstamp('>','PRO: insert a new db item')
        GOTO 77
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

C If update needed free the binary file, do conversion and
C then jump back to 77 to rescan the header of the database.
          CALL ERPFREE(IPRODB,ISTAT)
          write(LASCI,'(A,A2)')LPRFDB(1:lnblnk(LPRFDB)),'.a'
          IAF=IFIL+1
          CALL TROPRO(iuout,IAF,LASCI,IER)
          if(ier.eq.0)then
            call edisp(iuout,'A backup ASCII version placed in...')
            call edisp(iuout,LASCI)
          endif
        endif
        CALL EPMENRC
        goto 77
      else
        GOTO 92
      endif
      INO=-2
      goto 3

450   write(outs,'(3a,i4)') 'In profile db: error ',
     &  ermsg(1:lnblnk(ermsg)),' at record ',irec
      call edisp(iuout,outs)
      RETURN
      END

C *********** EDPROF
C Mid level profile editing facility.  In the case of a new profile
C dummy data is displayed first. NFLAG='N' new profile, NFLAG='E'
C existing profile. Asciupdate is set to true if the profile has
C been edited so that the calling routine will know to write out
C a fresh ASCII version file.
      SUBROUTINE  EDPROF(ITM,NFLAG,asciupdate,IER)
#include "epara.h"
#include "esprdbfile.h"

      PARAMETER  (MC=35)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      COMMON/PDBITM/NITEMS,lastrec,NCG(100),NO(100),DESC(100)
      COMMON/PDBELM/ICGS1(MC),ICGF1(MC),CGS1(MC),CGL1(MC)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

      DIMENSION ITEM(30)
      character NFLAG*1
      CHARACTER DESC*40,KEY*1,TMP*40
      character ITEM*33,H*72,cstr*72
      character ermsg*48   ! for read error messages
      character outs*124   ! string buffer for messages
      logical asciupdate   ! signal if user edits data

C Initailly set asciupdate toggle to false (no changes made yet).
      asciupdate=.false.

C If existing - read data for the profile and display.
      if(NFLAG.EQ.'E')then
        write(ermsg,'(a,i2)') 'reading number events in profile ',ITM
        IREC = NO(ITM)
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)  NCG(ITM)
        write(ermsg,'(a,i2)') 'reading description of profile ',ITM
        IREC = IREC + 1
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)  DESC(ITM)
        DO 270 I = 1,NCG(ITM)
          write(ermsg,'(a,i2,a,i2)') 'reading data of profile ',ITM,
     &      ' at period ',i
          IREC = IREC + 1
          READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)
     &      ICGS1(I),ICGF1(I),CGS1(I),CGL1(I)
270     CONTINUE
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
            ICGS1(I)=0
          else
            ICGS1(I)=ICGF1(I-1)
          endif
          ICGF1(I)=ICGS1(I)+incr
          IF(ICGF1(I).gt.24)ICGF1(I)=24
          CGS1(I)=0.
          CGL1(I)=0.
100     CONTINUE

C Make up an editing string for the start of each period.
        CSTR=' '
        K=1
        DO 60 J=1,NCG(ITM)
          K1=K+2
          WRITE(CSTR(K:K1),'(i2)')ICGS1(J)
          K=K1+1
   60   CONTINUE
        h(1)='If you edit in actual start times the subsequent '
        h(2)='editing tasks will be easier. '
        CALL EASKS(CSTR,' Start hour of each event',
     &    ' ',72,' 0  4   6   8','profile starts',IER,2)

C Extract the start timings and then sort out the finishes.
        K=0
        DO 61 J=1,NCG(ITM)
          CALL EGETWI(cstr,K,IV,0,24,'W','event start',IER)
          ICGS1(J)=IV
   61   CONTINUE
        DO 62 J=1,NCG(ITM)
          if(j.ne.NCG(ITM))then
            ICGF1(J)=ICGS1(J+1)
          else
            ICGF1(J)=24
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
          WRITE(ITEM(M),303)KEY,L,ICGS1(L),ICGF1(L),CGS1(L),CGL1(L)
  303     FORMAT(A1,I4,I6,I4,F10.1,F7.1)
        ENDIF
   10 CONTINUE

      write(ITEM(1),'(A,A20)') 'a profile: ',DESC(ITM)(1:20)
      write(ITEM(2),'(A,I3)')'  no. of events: ',NCG(ITM)
      ITEM(3)=               '  ______________________________'
      ITEM(4)=               '  event|from-to |sensible|latent'         

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
        WRITE(outs,5120)  ITM,DESC(ITM)
        call edisp(iuout,outs)
        WRITE(outs,5130)  NCG(ITM)
        call edisp(iuout,outs)
        call edisp(iuout,' ')
        call edisp(iuout,
     &  '           From      To         First        Second')
        call edisp(iuout,
     &  '           Hour      Hour       Value        Value ')
        DO 291 I = 1,NCG(ITM)
          WRITE(outs,5150)  I,ICGS1(I),ICGF1(I),CGS1(I),CGL1(I)
          call edisp(iuout,outs)
291     CONTINUE
      endif

C Help text for this menu.
      H(1)='You may alter the start and/or stop times of any '
      H(2)='event as well as the associated values.  For casual'
      H(3)='gain type profiles the first value is the sensible '
      H(4)='and the second is the latent. '
      H(5)=' '
      H(6)='When a change is made to the data a fresh copy of '
      H(7)='the ASCII version will be written out. '

      CALL EMENU('  Profile editing',ITEM,MVERT,INO)
      IF(INO.EQ.MVERT)THEN

C If in graphic mode restore the standard win3d values for prj.
        if(MMOD.ge.8)then
          CALL win3d(menuchw,4,1,1,3,igl,igr,igt,igb,igw,igwh)
        endif
        RETURN
      ELSEIF(INO.EQ.(MVERT-1))THEN

C HELP information.
        CALL PHELPD('pro edit',7,'-',0,0,IER)
      ELSEIF(INO.EQ.(MVERT-2))THEN

C Update the database.
        IREC=NO(ITM)
        WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)NCG(ITM)
        IREC=IREC+1
        WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)DESC(ITM)
        DO 232 I=1,NCG(ITM)
        IREC=IREC+1
        WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)
     &          ICGS1(I),ICGF1(I),CGS1(I),CGL1(I)
  232   CONTINUE

C If a new profile update lastrec and the number of items in record 1.
        if(NFLAG.eq.'N')then
          lastrec = IREC + 1
          IREC = 1
          WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)  NITEMS
        endif

C And signal that a fresh ASCII version needs to be written out.
        asciupdate=.true.
      ELSEIF(INO.EQ.(MVERT-3))THEN

C List current profile.
        WRITE(outs,5120)  ITM,DESC(ITM)
5120    FORMAT(' Profile ',I3,5X,A40)
        call edisp(iuout,outs)
        WRITE(outs,5130)  NCG(ITM)
5130    FORMAT(' Number of distinct events = ',I4)
        call edisp(iuout,outs)
        call edisp(iuout,' ')
        call edisp(iuout,
     &  '          Start     Finish      First        Second')
        call edisp(iuout,
     &  '           Hour      Hour       Value        Value ')
        DO 290 I = 1,NCG(ITM)
          WRITE(outs,5150)  I,ICGS1(I),ICGF1(I),CGS1(I),CGL1(I)
5150      FORMAT(3X,I3,4X,I4,6X,I4,7X,F7.2,6X,F7.2)
          call edisp(iuout,outs)
290     CONTINUE
      ELSEIF(INO.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      elseif(INO.EQ.1)then

C New profile name...
        TMP=DESC(ITM)
        H(1)='The description (up to 40 char) is provided as a way '
        H(2)='to select a profile.  It helps to make it unique.'
        CALL EASKS(TMP,' Description ? ',
     &    ' ',40,'new profile','profile descr',IER,2)
        DESC(ITM)=TMP
        CALL USRMSG(' ',' ','-')
      ELSEIF(INO.GT.MHEAD.AND.INO.LT.(MVERT-MCTL+1))THEN

C Edit profile identified by KEYIND.
        CALL KEYIND(MVERT,INO,IFOC,IO)
        CALL EDEVENT(ITM,IFOC,IER)
      endif
      INO=-4
      GOTO 92

C Error messsages.
450   write(outs,'(3a,i4)') 'In profile db: error ',
     &  ermsg(1:lnblnk(ermsg)),' at record ',irec
      call edisp(iuout,outs)
      RETURN

      END

C ************* EDEVENT
C EDEVENT: Edit details of a profile event.
      SUBROUTINE EDEVENT(IPR,IFOC,IER)
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      PARAMETER  (MC=35)
      COMMON/PDBITM/NITEMS,lastrec,NCG(100),NO(100),DESC(100)
      COMMON/PDBELM/ICGS1(MC),ICGF1(MC),CGS1(MC),CGL1(MC)
      character outs*124,H*72,DESC*40
      logical OK

  10  IER=0
      WRITE(outs,'(A,I4,A,A)') ' Event  - ',IFOC,' in ',DESC(IPR)
      call edisp(iuout,' ')
      call edisp(iuout,outs)

  11  IES=ICGS1(IFOC)
      H(1)='An event may start between hours 0 and 23. '
      CALL EASKI(IES,outs,' start hour : ',
     &    0,'F',23,'F',0,'start',IER,0)
      if(IFOC.gt.1.and.IES.lt.ICGF1(IFOC-1))then
        call usrmsg('The start time overlaps previous event...',
     &              'you must respecify.','W')
        goto 11
      endif
      CALL USRMSG(' ',' ','-')
      ICGS1(IFOC)=IES

  12  IEF=ICGF1(IFOC)
      H(1)='An event may finish  between hours 1 and 24.'
      CALL EASKI(IEF,outs,' finish hour: ',
     &    IES,'F',24,'F',0,'finish',IER,0)
      if(IFOC.lt.NCG(IPR).and.IEF.gt.ICGS1(IFOC+1))then
        CALL EASKAB(' Finish time overlaps next event...',
     &     ' ','respecify finish','shift next event start',IW,0)
        if(IW.eq.1)goto 12
        ICGS1(IFOC+1)=IEF
      endif
      CALL USRMSG(' ',' ','-')
      ICGF1(IFOC)=IEF

      EC=CGS1(IFOC)
      H(1)='The first value is the percentage of sensible gain.'
      CALL EASKR(EC,' ',' First value (sensible) percentage ? ',
     &   0.,'F',100.,'F',50.,'first value',IER,1)
      CGS1(IFOC)=EC

      EL=CGL1(IFOC)
      H(1)='The second value is the percentage of latent gain.'
      CALL EASKR(EL,' ',' Second value (latent) percentage ? ',
     &   0.,'F',100.,'F',50.,'first value',IER,1)
      CGL1(IFOC)=EL

      if(IER.ne.0)then
        CALL usrmsg('A problem was detected during input...',
     &              'Please repeat the specification.','W')
        goto 10
      endif

      RETURN
      END


C ***************** TRIPRO 
C TRIPRO reads data supplied from an ASCII profiles db and writes
C it to a NEW events profiles database file.
      SUBROUTINE TRIPRO(ITRU,IFA,LASCI,IER)
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)

#include "espriou.h"
C espriou.h provides currentfile.

      common/pophelp/h(60)
      COMMON/PDBITM/NITEMS,lastrec,NCG(100),NO(100),DESC(100)
      COMMON/OUTIN/IUOUT,IUIN
      CHARACTER DESC*40,OUTSTR*124,LASCI*72,h*72
      character ermsg*48   ! for read error messages
      character outs*124   ! string buffer for messages
      LOGICAL OK,dok

C The binary file will be overwritten.
      dok=.true.
      h(1)='Reading in an ASCII version of the profiles database '
      h(2)='will overwrite the current profiles database. If this '
      h(3)='is ok say yes. If not say no so you can respecify. '
      CALL ASKOK(' ','Ok to overwrite the profiles database?',OK,dok,3)
      IF(.NOT.OK)THEN
        IER=1
        RETURN
      ELSE
        CALL ERPFREE(IPRODB,ISTAT)
        call EFOPRAN(IPRODB,LPRFDB,10,3,IER)
        if(ier.ne.0)return
      ENDIF
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
      CALL EGETWI(OUTSTR,K,NITEMS,0,0,'-','no items',IER)

      ermsg='writing record one number of items '
      IREC = 1
      WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NITEMS

C Get number of events and description of profile (EGETRM gets the
C remainder of the line after the first word (NCG).
      DO 10 I = 1,NITEMS
        CALL STRIPC(IFA,OUTSTR,0,ND,1,'NCG,DESC',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(OUTSTR,K,NCG(I),0,0,'-','NCG',IER)
        CALL EGETRM(OUTSTR,K,DESC(I),'W','profile descr',IER)

        write(ermsg,'(a,i2)') 'writing number events in profile ',I
        IREC = IREC + 1
        NO(I)=IREC
        WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NCG(I)
        write(ermsg,'(a,i2)') 'writing description of profile ',I
        IREC = IREC + 1
        WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  DESC(I)

        DO 20 J = 1,NCG(I)
          CALL STRIPC(IFA,OUTSTR,0,ND,1,'IS,IF,A,B',IER)
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWI(OUTSTR,K,IS,0,0,'-','Start',IER)
          CALL EGETWI(OUTSTR,K,IF,0,0,'-','Finish',IER)
          CALL EGETWR(OUTSTR,K,A,0.,0.,'-','VAL 1',IER)
          CALL EGETWR(OUTSTR,K,B,0.,0.,'-','VAL 2',IER)
          write(ermsg,'(a,2i3)') 'writing data of profile ',I,J
          IREC = IREC + 1
          WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)IS,IF,A,B
   20   CONTINUE
   10 CONTINUE

      ermsg='writing record one number of items and last rec '
      IR=IREC+1
      IREC = 1
      WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NITEMS,IR

  99  CALL ERPFREE(IPRODB,ISTAT)
      CALL ERPFREE(IFA,ISTAT)
      RETURN

 1001 write(outs,'(3a,i4)') 'In profile db: error ',
     &  ermsg(1:lnblnk(ermsg)),' at record ',irec
      call edisp(iuout,outs)
      goto 99

      END

C **************** TROPRO 
C TROPRO creates an ASCII profiles db from the binary profiles db.
      SUBROUTINE TROPRO(ITRU,IFA,LASCI,IER)
#include "esprdbfile.h"
#include "espriou.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PDBITM/NITEMS,lastrec,NCG(100),NO(100),DESC(100)
      CHARACTER DESC*40,OUTSTR*124,LASCI*72
      character ermsg*48   ! for read error messages
      character outs*124   ! string buffer for messages

      IER =0

      CALL ERPFREE(IPRODB,ISTAT)
      call EFOPRAN(IPRODB,LPRFDB,10,1,IER)
      if(ier.ne.0)return

      ermsg='reading record one number of items '
      IREC=1
      READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)NITEMS
      IF(NITEMS.LE.0)THEN
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
      ermsg='reading record one number of items '
      IREC=1
      READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NITEMS

      WRITE(IFA,21,IOSTAT=ISTAT,ERR=1000)NITEMS
   21 FORMAT(I5,'    # Number of items')

      DO 10 I = 1,NITEMS
        write(ermsg,'(a,i2)') 'reading number events in profile ',I
        IREC = IREC + 1
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NCG(I)
        write(ermsg,'(a,i2)') 'reading description of profile ',I
        IREC = IREC + 1
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  DESC(I)

        WRITE(IFA,22)NCG(I),DESC(I)
   22   FORMAT(I5,3X,A40,'  # No periods & description',/,
     &                   '# start finish data data')

C For each period.
        DO 20 J = 1,NCG(I)
          write(ermsg,'(a,i2,a,i2)') 'reading data of profile ',I,
     &      ' at period ',J
          IREC = IREC + 1
          READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)IS,IF,A,B
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

C *********** PROGRF
C 'PROGRF' outputs a graphical display of an event profile (IPRO).
      SUBROUTINE PROGRF(IPRO)

      PARAMETER  (MC=35)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/PDBITM/NITEMS,lastrec,NCG(100),NO(100),DESC(100)
      COMMON/PDBELM/ICGS1(MC),ICGF1(MC),CGS1(MC),CGL1(MC)
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GFONT/IFS,ITFS,IMFS

      CHARACTER DESC*40,ETEXT*72

      DIMENSION VAL1(25),VAL2(25)

      IF(IPRO.LT.1.OR.IPRO.GT.NITEMS)then
        call usrmsg(' ',' Profile requested not within db','W')
        return
      endif

      YMAX=100.
      YMIN=0.

C Set up data ready for graph drawing.
      call startbuffer
      call win3d(menuchw,10,10,4,4,igl,igr,igt,igb,igw,igwh)
      call axiscale(igw,igwh,0.0,24.0,YMIN,YMAX,xsc,ysc,sca,Xadd,Yadd)
      call linescale(igl,Xadd,xsc,igb,Yadd,ysc)
      call horaxis(0.,24.,igl,igr,igb,Xadd,xsc,1,'Time (hours)')
      call vrtaxis(YMIN,YMAX,igl,igb,igt,Yadd,ysc,0,0,'Percent')

C Pick up the data increment for the horizontal axis.
      call dinterval(0.,24.,DT,NDEC,1)

C Print header
      WRITE(ETEXT,16)IPRO,DESC(IPRO)
   16 FORMAT('Profile ',I3,': ',A40)
      call viewtext(ETEXT,1,0,IFS)

      DO 60 I=1,25
        VAL1(I)=0.
        VAL2(I)=0.
   60 CONTINUE

c Assign hourly values of each event.
      DO 20 I=1,NCG(IPRO)

c Period of each event.
        DO 30 J=ICGS1(I),ICGF1(I)
          J1=J+1
          VAL1(J1)=CGS1(I)
          VAL2(J1)=CGL1(I)
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
        CALL ETPLOT(TI,VAL1(I+1),0,0)
        goto 40
    9   CALL ETPLOT(TI,VAL1(I+1),IPEN,MARK)
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
        CALL ETPLOT(TI,VAL2(I+1),0,0)
        goto 50
   11   CALL ETPLOT(TI,VAL2(I+1),IPEN,MARK)
   50 CONTINUE
      call forceflush()
      return

      END

