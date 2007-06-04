C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-6.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C PRO
C ESRU event profiles database manager.

      program pro
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)
C default file names for databases

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      common/pophelp/h(60)
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appw/iappw,iappx,iappy
      COMMON/FILEP/IFIL

C Path to problem
      common/rpath/path
      common/uhome/upath
      common/udot/esprc

C Command line file (if any).
      common/rcmd/LCMDFL

C Project journal on/off, unit number, cmd, file name.
      common/journopt/journio,iuj,journcmd,jfile

C Where exemplars list and default db list are kept.
      common/defdb/dfdblbl,defdbfl

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF
      
C Name of current application
      common/APPNAME/cAppName
      
      character cAppName*12      
      
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF

      character LCMDFL*144,inf*144,H*72,outs248*248
      character outs*124,path*72,upath*72,esprc*72,fs*1
      character dfdblbl*20,defdbfl*72,OUTSTR*124
      character journcmd*20,jfile*72,uname*24,cjfile*72
      logical unixok,there

C Some initial values, which may be changed later:
      call ezero
      call curmodule('pro ')
      cAppName = 'pro'
      IUOUT=6
      IUIN=5
      ICOUT=0
      IFIL=10
      iuj=IFIL+10
      IFS=1
      ITFS=0
      IMFS=1
      LIMTTY=24
      LIMIT =24
      call parpsf(MODL,iappw,iappx,iappy,inf)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

C Instantiate terminal type to that passed from C main and set the
C size of the overall window slightly smaller than primary ESRU modules.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=520
        iappx=75
        iappy=170
      else
        if(iappx.le.0)iappx=75
        if(iappy.le.0)iappy=170
        if(iappw.le.200)then
          iappwi=int(520*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(36*iappw*0.01),24)
        LIMTTY= MAX0(int(6*iappw*0.01),4)
        LIMIT = MAX0(int(6*iappw*0.01),4)
      else
        menuchw = 36
        LIMTTY=6
        LIMIT =6
      endif
      IF(MMOD.EQ.8)THEN

C Set initial font sizes (IMFS is for graphs, IFS is for dialog & text feedback).
        IMFS=1
        IFS=1
        ITFS=1
        call userfonts(IFS,ITFS,IMFS)
      ELSE
        LIMTTY=16
        LIMIT =16
      ENDIF
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &'Event Profiles Manager: enquiries to esru@strath.ac.uk')

      IF(MMOD.EQ.-6)then
        ICOUT=0
      else
        ICOUT=IUOUT
      endif

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN
        CALL win3d(menuchw,10,10,3,3,igl,igr,igt,igb,igw,igwh)
#ifdef X11
        call opencpw
        call opensetup
#endif
        mdispl=0
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      ENDIF
      IF(MMOD.NE.-6)ICOUT=IUOUT
      
      CALL ESPrVersion("quiet","continue")
      

      call edisp(IUOUT,' ')

C Find the users home folder then get users custom settings.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(a,a,a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(a,a,a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IFIL+1,0,IIER)

C If there is a journal active set up to add to the journal. First
C get the root journal file (setup by prj) from the users home folder.
C All subsequent tstamp messages will be appended to root journal.
      if(journio.eq.1)then
        uname=' '
        call usrname(uname)
        cjfile=' '
        write(cjfile,'(a,a1,a,a,a)')upath(1:lnblnk(upath)),fs,'.',
     &    uname(1:lnblnk(uname)),'cur_j'
        there=.false.
        INQUIRE(FILE=cjfile,EXIST=there)
        if(there)then
          close(iuj)
          open(iuj,file=cjfile,status='UNKNOWN',err=901)
          CALL STRIPC(iuj,OUTSTR,0,ND,1,'header',IER)
          if(OUTSTR(1:15).eq.'Current_Journal')then
            k=16
            CALL EGETRM(OUTSTR,K,jfile,'W','root journ',IER)
          endif
          close(iuj)
        endif

        write(outs,'(a,a)')'Journal (profiles database) for:',
     &    uname(1:lnblnk(uname))
        call tstamp('>',outs)
      endif

C Scan the defaults file for default configuration.
C Make temporary use of file unit IFIL+1.
 902  call escdef(dfdblbl,defdbfl,IFIL+1,IER)

C Take command line file name as initial input file.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
        write(outs248,'(a,a)')' the input file is: ',LCMDFL
        call edisp248(iuout,outs248,80)
        call tstamp('>',outs)
      else
        LCMDFL=' '
      endif

      IPRODB=IFIL
      LPRFDB=DPRFDB

      CALL  CASFMK
      STOP

 901  call edisp(iuout,'Error opening journal file, continuing.')
      goto 902

      END


C ************* CASFMK
C Event profile input/edit control routine.
C These profile can be used to aid the input of:
C   casual gains to the operations file,
C   air flow patterns to the operations file,
C   control information to the configuration control file.
      SUBROUTINE  CASFMK
#include "epara.h"
#include "esprdbfile.h"

      integer igraphiclib  ! external definition

      PARAMETER  (MC=35)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/pophelp/h(60)
      COMMON/PDBITM/NITEMS,lastrec,NCG(100),NO(100),DESC(100)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

C Command line file (if any).
      common/rcmd/LCMDFL
      common/rpath/path

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

      DIMENSION ITEM(35)
      CHARACTER LASCI*72,DFILE*72,DESC*40,outs*124,KEY*1
      character H*72,ITEM*38,LCMDFL*144,LTMP*72,path*72,L144*144
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      logical XST,OK,dok

 155  h(1)='An event profiles database holds named sets of profiles'
      h(2)='in a binary representation. '
      L144=LCMDFL
      llt=lnblnk(L144)

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        if(llt.lt.96)then
          CALL EASKF(L144,' Event profile db? ',
     &      ' ',96,DPRFDB,'binary events db',IER,2)
        elseif(llt.ge.96.and.llt.lt.124)then
          CALL EASKF(L144,' Event profile db? ',
     &      ' ',124,DPRFDB,'binary events db',IER,2)
        elseif(llt.ge.124.and.llt.le.144)then
          CALL EASKF(L144,' Event profile db? ',
     &      ' ',144,DPRFDB,'binary events db',IER,2)
        endif
      elseif(iglib.eq.2)then
        CALL EASKF(L144,' Event profile db? ',
     &    ' ',144,DPRFDB,'binary events db',IER,2)
      else
        CALL EASKF(L144,' Event profile db? ',
     &    ' ',96,DPRFDB,'binary events db',IER,2)
      endif

      IF(L144(1:2).EQ.'  '.or.L144(1:4).eq.'UNKN')GOTO 155
      LCMDFL=L144
      IER=0
      call EFOPRAN(IPRODB,LCMDFL,10,1,IER)
      IF(ier.eq.0)THEN
        llt=lnblnk(LCMDFL)
        if(llt.le.72)then 
          write(LPRFDB,'(a)') LCMDFL(1:lnblnk(LCMDFL))
        else
          write(LPRFDB,'(a)') LCMDFL(1:72)
        endif
        CALL USRMSG(' opened profiles db',' ','-')
        IREC=1
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)NITEMS
        DO 58 I=1,NITEMS
          IREC=IREC+1
          NO(I)=IREC
          READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)NCG(I)
          IREC=IREC+1
          READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)DESC(I)
          IREC=IREC+NCG(I)
   58   CONTINUE
        lastrec=IREC+1
        CALL USRMSG(' ',' ','-')
      ELSE
        dok=.true.
        h(1)='This module works with a profiles database. The name'
        h(2)='you supplied does not point to an existing file. If '
        h(3)='you say yes a new database will be created. '
        CALL ASKOK(' The file you specified was not found.',
     &    ' Do you want this to be a new profiles db?',OK,dok,3)
        if(.NOT.OK)goto 155
        call EFOPRAN(IPRODB,LCMDFL,10,3,IER)
        if(ier.ne.0)goto 155
        llt=lnblnk(LCMDFL)
        if(llt.le.72)then 
          write(LPRFDB,'(a)') LCMDFL(1:lnblnk(LCMDFL))
        else
          write(LPRFDB,'(a)') LCMDFL(1:72)
        endif
        CALL USRMSG(' opened new profiles db',' ','P')
        call tstamp('>','PRO: opened new profiles db')
        call tstamp('>',LPRFDB)
        NITEMS = 1
        NO(1) = 2
        NCG(1)= 1
        DESC(1)='First profile'
        lastrec=4
      endif

C Setup for multi page menu.
      MHEAD=4
      MCTL=6
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

      write(ITEM(1),'(A,A)') '1 db: ',LPRFDB(1:32)
      write(ITEM(2),'(A,I3)')'  number of profiles: ',NITEMS
      ITEM(3)=               '  __________________________________ '
      ITEM(4)=               '  events| description                '

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
      ITEM(M+4)  ='~ convert db ASCII>>BINARY>>ASCII '
      ITEM(M+5)  ='? help                            '
      ITEM(M+6)  ='- exit pro                        '

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('pro ',1)

      CALL EMENU('  Profile manager',ITEM,MVERT,INO)
      IF(INO.EQ.MVERT)THEN
        call usrmsg(' Closing Profile manager...',' ','P')
        call tstamp('>','Finish profiles database manager')
        call EPAGEND
        CALL ERPFREE(IPRODB,ISTAT)
        STOP
      ELSEIF(INO.EQ.(MVERT-1))THEN

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('pro ',0)
      ELSEIF(INO.EQ.(MVERT-2))THEN

C Convert database.
        H(1)='In order to transfer profiles databases between  '
        H(2)='different computer environments or via e_mail it is '
        H(3)='often useful to be able to convert from the normal '
        H(4)='binary format into ascii format and back. '
        CALL EASKAB(' Conversion of profiles db format',
     &    ' ','binary >> ascii','ascii >> binary',IW,4)
        if(IW.eq.1)then

C Event profiles db: binary > ASCII    
C Ask for the binary file name and then confirm the ASCII db name.
          IAF=IFIL+1
          H(1)='The events db contains one or more descriptions of'
          H(2)='profiles created via a diary entry format.'
          CALL EASKS(LPRFDB,' Binary event profile db? ',
     &      ' ',72,DPRFDB,'binary events db',IER,2)

          write(LASCI,'(A,A2)')LPRFDB(1:lnblnk(LPRFDB)),'.a'
          DFILE='./newprofiles.db1.a'
          CALL EASKS(LASCI,' ASCII event profile? ',
     &      ' ',72,DFILE,'ascii events db',IER,2)

          CALL TROPRO(iuout,IAF,LASCI,IER)
          call tstamp('>','PRO: convert binary to ascii')
        elseif(IW.eq.2)then

C Event profiles db: ASCII > binary 
C Ask for the ASCII file name and then confirm the binary db name.
          IAF=IFIL+1
          H(1)='The events db contains one or more descriptions of'
          H(2)='profiles created via a diary entry format.'
          LASCI=' '
          CALL EASKS(LASCI,' ASCII event profile source? ',
     &      ' ',72,'profiles.db1.a','ascii events db',IER,2)

          H(1)='The binary events db will be overwritten.'
          CALL EASKS(LPRFDB,' Binary event profile db? ',
     &      ' ',72,DPRFDB,'binary events db',IER,1)

          CALL TRIPRO(iuout,IAF,LASCI,IER)
          call tstamp('>','PRO: convert ascii to binary')
        endif

C After conversion reopen the binary file and rescan menu.
        call EFOPRAN(IPRODB,LCMDFL,10,1,IER)
        IREC=1
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)NITEMS
        DO 157 I=1,NITEMS
          IREC=IREC+1
          NO(I)=IREC
          READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)NCG(I)
          IREC=IREC+1
          READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)DESC(I)
          IREC=IREC+NCG(I)
  157   CONTINUE
        lastrec=IREC+1
        CALL USRMSG(' ',' ','-')
        goto 92
      ELSEIF(INO.EQ.(MVERT-3))THEN

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
      ELSEIF(INO.EQ.(MVERT-4))THEN

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
        CALL EDPROF(NITEMS,'N',IER)
        CALL EPMENRC
        CALL USRMSG(' ',' ','-')
        call tstamp('>','PRO: insert a new db item')
        GOTO 92
      ELSEIF(INO.EQ.(MVERT-5))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      elseif(INO.EQ.1)then

C Change database name. If new file has data then read it in, if none
C found then confirm with user to respecify file name or open as a
C new database.
 156    LTMP=LPRFDB
        call tstamp('>','PRO: change database')
        CALL EASKS(LTMP,' Event profile db? ',
     &    ' ',72,DPRFDB,'binary events db',IER,2)

C Find the path and local file name.
        call fdroot(LTMP,path,LPRFDB)
        CALL ERPFREE(IPRODB,ISTAT)
        call FINDFIL(LPRFDB,XST)
        IF(XST)THEN
          call EFOPRAN(IPRODB,LPRFDB,10,1,IER)
          IREC=1
          READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)NITEMS
          if(NITEMS.gt.0)then
            DO 159 I=1,NITEMS
              IREC=IREC+1
              NO(I)=IREC
              READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)NCG(I)
              IREC=IREC+1
              READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)DESC(I)
              IREC=IREC+NCG(I)
  159       CONTINUE
            lastrec=IREC+1
          else
            CALL EASKAB(' This file is empty or not a profiles db:',
     &      ' ','specify another name','make it a new db',IW,0)
            if(IW.eq.1)goto 156
            NITEMS = 1
            NO(1) = 2
            H(1)='Between 1 and 35 events can be acommodated.'
            H(2)='An event is equivalent to a period of time with'
            H(3)='associated values. '
            CALL EASKI(IV,' ',' Number of events in first profile: ',
     &        1,'F',MC,'F',1,'no events',IER,3)
            NCG(1)=IV
            lastrec=NO(1)+NCG(1)+1
            CALL EPMENSV
            CALL EDPROF(1,'N',IER)
            CALL EPMENRC
          endif
        else
          CALL EASKAB(' Profiles database not found...',
     &      ' ','specify another name','make new db with this name',
     &    IW,0)
          if(IW.eq.1)goto 156
          call tstamp('>','PRO: creating new database')
          call tstamp('>',LPRFDB)
          call EFOPRAN(IPRODB,LPRFDB,10,3,IER)
          NITEMS = 1
          NO(1) = 2
          H(1)='Between 1 and 35 events can be acommodated.'
          H(2)='An event is equivalent to a period of time with'
          H(3)='associated values. '
          CALL EASKI(IV,' ',' Number of events in first profile: ',
     &       1,'F',MC,'F',1,'no events',IER,3)
          NCG(1)=IV
          lastrec=NO(1)+NCG(1)+1
          CALL EPMENSV
          CALL EDPROF(1,'N',IER)
          CALL EPMENRC
        endif
        CALL USRMSG(' ',' ','-')
      ELSEIF(INO.GT.MHEAD.AND.INO.LT.(MVERT-MCTL+1))THEN

C Edit profile identified by KEYIND.
        call tstamp('>','PRO: edit database entry')
        CALL KEYIND(MVERT,INO,IFOC,IO)
        CALL EPMENSV
        CALL EDPROF(IFOC,'E',IER)
        CALL EPMENRC
      else
        GOTO 92
      endif
      INO=-2
      goto 3

450   call edisp(iuout,' File error  ')
      RETURN
      END


C *********** EDPROF
C Mid level profile editing facility.  In the case of a new profile
C dummy data is displayed first. NFLAG='N' new profile, NFLAG='E'
C existing profile.
      SUBROUTINE  EDPROF(ITM,NFLAG,IER)
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
      CHARACTER DESC*40,outs*124,KEY*1,TMP*40
      character ITEM*33,H*72,cstr*72

C If existing - read data for the profile and display.
      if(NFLAG.EQ.'E')then
        IREC = NO(ITM)
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)  NCG(ITM)
        IREC = IREC + 1
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)  DESC(ITM)
        DO 270 I = 1,NCG(ITM)
          IREC = IREC + 1
          READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)
     &      ICGS1(I),ICGF1(I),CGS1(I),CGL1(I)
270     CONTINUE
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
      ITEM(M+3)  ='> merge current profile into db '
      ITEM(M+4)  ='? Help                          '
      ITEM(M+5)  ='- Exit to main menu             '

C If in graphic mode graph the current profile.
      if(MMOD.ge.8)then
        CALL USRMSG(' ',' ','-')
        CALL PROGRF(ITM)
      endif

C Help text for this menu.
      H(1)='You may alter the start and/or stop times of any '
      H(2)='event as well as the associated values.  For casual'
      H(3)='gain type profiles the first value is the sensible '
      H(4)='and the second is the latent. '

      CALL EMENU('  Profile editing',ITEM,MVERT,INO)
      IF(INO.EQ.MVERT)THEN
        RETURN
      ELSEIF(INO.EQ.(MVERT-1))THEN

C HELP information.
        CALL PHELPD('pro edit',4,'-',0,0,IER)
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
450   call edisp(iuout,' Profile file read error. ')
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

C ***** dummy routine for c code active descriptions buttons.
      subroutine cfgpk(act)
      character act*1

      return
      end

C Dummy routines from common3dv.F
      SUBROUTINE ADJVIEW(ITRC,IER)
      ier=0
      return
      end

      subroutine chgazi(icazi)
      return
      end

      subroutine chgelev(icelev)
      return
      end

      SUBROUTINE EMKVIEW(ITRC,IUO,CFGOK,IER)
      logical cfgok
      ier=0
      return
      end

      SUBROUTINE EPKMLC(ISEL,PROMPT1,PROMPT2,IER)
      CHARACTER*(*) PROMPT1,PROMPT2
      ier=0
      return
      end

      SUBROUTINE INLNST(ITYP)
      return
      end

      SUBROUTINE PLELEV(IUF,direc,ier)
      CHARACTER direc*1
      ier=0
      return
      end
