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

C dbm provides access to generic esp-r database, conversions
C and editing facilities.

C << issues to bring generic databases in line with current concepts:
C << a)
C <<
C << b)
C <<
C << c)
C <<

      program dbm
#include "esprdbfile.h"
C providing paramaters MGDB,MGCL,MGIT

#include "espriou.h"
C espriou.h provides currentfile and defdbfl.
      
      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/FILEP/IFIL
      COMMON/MOD/MODEL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appw/iappw,iappx,iappy

C Redirected text/graphics parameters.
      common/exporttg/xfile,tg,delim
      common/exporttgi/ixopen,ixloc,ixunit

      common/C21/IFCFG,cfgroot,LCFGF
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth,aimpth,bsmpth,hvacpth

C Path to problem and command line file (if any).
      common/rpath/path
      common/uhome/upath
      common/rcmd/LCMDFL

C Generic db commons.
C Name of databases in common/gendbfn and file unit index in common/gendbfi
C are from esprdbfile.h

C << why is LGENDB in common? >>
C      common/gendb/LGENDB

      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)
      common/gendb6/ittag(MGIT),itmenu(MGIT)
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

C Backup of documentation (for editing) bkdoc is the text, 
C bkdoctyp = "dd","cd","id" for db doc, cat doc and item doc.
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      
C Name of current application
      common/APPNAME/cAppName
      
      character cAppName*12      
      
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL OK,dok,statusok
      LOGICAL moddb,unixok,browse,status

      DIMENSION item(23),cnsel(100),icnsel(100)
      dimension dba(MGDB)
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,dbnote*72
      character cattag*12,cattype*8,catmenu*32,catdate*24,catdoc*248
      character ittag*12,itmenu*32,seldb*12,cnsel*12,MODE*4,LCFGF*72
      character LGENDB*72,LAGENDB*72
      character item*54,db*12,fs*1,inf*144,LCMDFL*144,dfile*72
      character tfile*72
      character outs248*248,LTMP*144
      character path*72,upath*72,pass*12,H*72,outs*124
      character xfile*144,tg*1,delim*1,cfgroot*24
      character dba*12,act*1,action*12
C      character curfile*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,aimpth*24,bsmpth*24
      character hvacpth*24

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer ier
      integer mitm,INO  ! max items and current menu item

#ifdef OSI
      integer impx,impy,iw
#else
      integer*8 impx,impy,iw
#endif

      call ezero
      call curmodule('dbm ')
      cAppName = 'dbm'
      IUOUT=6
      IUIN=5
      IFIL=11
      IAFIL=IFIL+1
      IBFIL=IFIL+2
      IFS=1
      ITFS=1
      IMFS=1
      LIMTTY=26
      LIMIT =26
      NSIGFIG=3
      LAGENDB=' '
C      curfile=' '
      moddb=.false.

C Text/graphic feedback redirection on channel ifil +9 
      ixunit = ifil + 9
      delim = '-'
      ixopen = 0

C Get command line parameters.
      call parpsf(MODL,iappw,iappx,iappy,inf)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

C Initialise output device, assume minimal trace, and a smallish
C wide display.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappwi= 740
        iapphi= 530
        iappx=30
        iappy=30
      else
        if(iappx.le.0)iappx=30
        if(iappy.le.0)iappy=30
        if(iappw.le.200)then
          iappwi=int(740*iappw*0.01)
          iapphi=int(530*iappw*0.01)
        elseif(iappw.gt.200)then
          continue
        endif
      endif

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(41*iappw*0.01),12)
        LIMTTY=18
        LIMIT =18
      else
        menuchw = 41
        LIMTTY=18
        LIMIT =18
      endif
      IF(MMOD.EQ.8)THEN

C Set initial font sizes (IMFS is for graphs, IFS is for dialog & text feedback).
        IMFS=1
        IFS=1
        ITFS=1
        call userfonts(IFS,ITFS,IMFS)
      ELSE
        LIMTTY=18
        LIMIT =18
      ENDIF
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &'ESP-r db manager: enquiries to esru@strath.ac.uk')

C Open the text display box equal to LIMTTY if MODEL = 8.
      IF(MODEL.EQ.8)THEN
        CALL win3d(menuchw,8,20,3,3,igl,igr,igt,igb,igw,igwh)
        igw=igr-igl
        igwh=igb-igt
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call opencpw
          call opensetup
        endif
        mdispl=0
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()

C << this might not have been implemented yet >>
C        call charsusingfnt(ifs,icw,nlines)
        icw=80
      else
        icw=72
      endif
      IF(MODEL.NE.-6)ICOUT=IUOUT

C Display copywrite based on current text display width.
      !CALL ESPrVersion("summary",cAppName,IUOUT)
      call edisp(IUOUT,' ')

C Find the user's home folder then get user's custom settings.
C Make temporary use of file unit IAF=IFIL+1.
      IAF=IFIL+1
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IAF,0,IIER)

C Scan the defaults file for default configuration. << tmp name >>
      defdbfl='./gdefaults'
      call escgdef(IAF,IER)

      LGENDB=' '

C Assume configuration file is from IFIL, constructions input from IFIL+3. 
      IFCFG=IFIL
      igendbf(1)=IFIL+3

C Assume materials input from IFIL+4 and optical db from IFIL+5.
      igendbf(2)=IFIL+4
      igendbf(3)=IFIL+5

C Assume << unused >> db on IFIL+6, acoustics db on IFIL+7, colour on
C IFIL+8, micotoxins on IFIL+9, environmental impacts on IFIL+10,
C transport on IFIL+11, wind prssures on IFIL+12, profiles on IFIL+13,
C plant templates on IFIL+14 and components on IFIL+15.
C << could use igendbf(4) for contaminates??
      igendbf(4)=IFIL+6
      igendbf(5)=IFIL+7
      igendbf(6)=IFIL+8
      igendbf(7)=IFIL+9
      igendbf(8)=IFIL+10
      igendbf(9)=IFIL+11
      igendbf(10)=IFIL+12
      igendbf(11)=IFIL+13
      igendbf(12)=IFIL+14
      igendbf(13)=IFIL+15
      
C Assume that the configuration, mlc db, primitive db, control file
C have not been read in. Initial assumption that tool is in browse
C mode.
      CFGOK=.FALSE.
      MLDBOK=.FALSE.
      MATDBOK=.FALSE.
      CTLOK=.FALSE.
      browse=.TRUE.
      statusok=.FALSE.

C Initial clear of (all possible db) generic db commons and hash tables.
      do 245 j=1,MGDB
        gendbok(j)=.FALSE.
        gendbfn(j)='  '
        call clrgdbc(j)
 245  continue

C Take command line file name to LCMDF and use as initial climate file.
      if(inf(1:2).eq.'  '.or.inf(1:4).eq.'UNKN')then
        LCMDFL=' '
      else
        LCMDFL=inf
        write(outs248,'(a,a)')' the input file is: ',LCMDFL
        call edisp248(iuout,outs248,80)
      endif

C Confirm problem name.
C << see about something other than /usr/esru >>
      H(1)='The system configuration file holds the definition of'
      H(2)='the building/plant to be simulated, including the'
      H(3)='names of all of the files required. If the name given'
      H(4)='matches an existing file it will be read in and the'
      H(5)='data will be available for editing.'
      DFILE='/usr/esru/esp-r/defaults/defcfg' 
      LTMP=LCMDFL

C Call EASKF depending on the current file name length.
C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
  29  llt=lnblnk(ltmp)
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        if(llt.lt.96)then
          CALL EASKF(LTMP,' Model name ? ',' ',96,DFILE,
     &      'config file name',IER,5)
        elseif(llt.ge.96.and.llt.lt.124)then
          CALL EASKF(LTMP,' Model name ? ',' ',124,DFILE,
     &      'config file name',IER,5)
        elseif(llt.ge.124.and.llt.le.144)then
          CALL EASKF(LTMP,' Model name ? ',' ',144,DFILE,
     &      'config file name',IER,5)
        endif
      elseif(iglib.eq.2)then
        CALL EASKF(LTMP,' Model name ? ',' ',144,DFILE,
     &    'config file name',IER,5)
      else
        CALL EASKF(LTMP,' Model name ? ',' ',96,DFILE,
     &    'config file name',IER,5)
      endif

      IF(LTMP(1:2).EQ.'  ')GOTO 29
      LCMDFL=LTMP

C Find the path and local file name. Reset all gendbfn to defaults.
      call fdroot(LCMDFL,path,LCFGF)
      call edisp(iuout,' ')
      call usrmsg('Scanning the model configuration...',' ','-')
      MODE='ALL '
      itrc=0
      IAPROB=IFIL+2
      call ersysg(LCFGF,IFCFG,IAPROB,MODE,ITRC,IER)
      if(IER.eq.0)then
        CFGOK=.TRUE.
        call usrmsg('Scanning the model configuration...done.',' ','-')
      else
        call usrmsg('Scanning the model configuration...failed.',
     &    '(file not found or error during scan) assuming defaults.',
     &    'W')
      endif

C Open and scan known databases (from least dependant to most dependant).
C In each case check iter return code to see if file can be directly
C scanned (iter=0), if it should be converted from asci (iter=2), or
C if it is a legacy file which might be imported (iter=3).
      call usrmsg('Scanning the databases...',' ','-')
      action='all_depend'
      call dealwithgdb(action,ier)
      if(ier.eq.0)then
        call usrmsg('Scanning the databases...done.',' ','-')
      else
        call usrmsg('Scanning the databases...might have had a',
     &    'problem (see messages generated during scan).','W')
      endif

C Main menu and control loop.
    3 INO=-4

C Find maximum width of file names to be displayed.
      la=LNBLNK(gendbfn(1))
      lb=LNBLNK(gendbfn(2))
      lc=LNBLNK(gendbfn(3))
C      ld=LNBLNK(gendbfn(4))
      le=LNBLNK(gendbfn(5))
      lf=LNBLNK(gendbfn(6))
      lg=LNBLNK(gendbfn(7))
      lh=LNBLNK(gendbfn(8))
      li=LNBLNK(gendbfn(9))
      lj=LNBLNK(gendbfn(10))
      lk=LNBLNK(gendbfn(11))
      ll=LNBLNK(gendbfn(12))
      lm=LNBLNK(gendbfn(13))
      ln=LNBLNK(LCFGF)
      lo=LNBLNK(path)
      IWA=MAX0(38,la+17,lb+17,lc+17,le+17,lf+17,lg+17,lh+17,
     &  li+17,lj+17,lk+17,ll+17,lm+17)
      if(IWA.le.38)then
        IWB=IWA-17
        IWA=IWB+17
      else
        IWB=21
        IWA=IWB+17
      endif
      IWC=MAX0(38,ln+8,lo+8)
      if(IWC.le.38)then
        IWD=IWC-8
      else
        IWD=30
      endif
      IWE=MAX0(38,IWA,IWC)

C Debug.
C      write(6,*) la,lb,lc,le,lf,lg,lh,li,lj,lk,lm,ln,lo,
C     &  '  ',IWA,IWB,IWC,IWD,IWE

      WRITE(ITEM(1),'(A,1X,A)')   'a model:',LCFGF(1:IWD)
      WRITE(ITEM(2),'(A,1X,A)')   '  path :',path(1:IWD)
      ITEM(3) ='  ___________________________________ '
      if(gendbfn(1).ne.dgendbfn(1))then
        WRITE(ITEM(4),'(A,1X,A)')'d construction :',gendbfn(1)(1:IWB)
      else
        WRITE(ITEM(4),'(A)')'d construction : default'
      endif
      if(gendbfn(2).ne.dgendbfn(2))then
        WRITE(ITEM(5),'(A,1X,A)')'e material     :',gendbfn(2)(1:IWB)
      else
        WRITE(ITEM(5),'(A)')'e material     : default'
      endif
      if(gendbfn(3).ne.dgendbfn(3))then
        WRITE(ITEM(6),'(A,1X,A)')'f optic        :',gendbfn(3)(1:IWB)
      else
        WRITE(ITEM(6),'(A)')'f optic        : default'
      endif
      if(gendbfn(5).ne.dgendbfn(5))then
        WRITE(ITEM(7),'(A,1X,A)')'h acoustic     :',gendbfn(5)(1:IWB)
      else
        WRITE(ITEM(7),'(A)') 'h acoustic     : default'
      endif
      if(gendbfn(6).ne.dgendbfn(6))then
        WRITE(ITEM(8),'(A,1X,A)')'i colour       :',gendbfn(6)(1:IWB)
      else
        WRITE(ITEM(8),'(A)') 'i colour       : default'
      endif
      if(gendbfn(7).ne.dgendbfn(7))then
        WRITE(ITEM(9),'(A,1X,A)')'j micotoxin   :',
     &    gendbfn(7)(1:IWB)
      else
        WRITE(ITEM(9),'(A)')'j micotoxin    : default'
      endif
      if(gendbfn(8).ne.dgendbfn(8))then
        WRITE(ITEM(10),'(A,1X,A)')'k LCA/impact   :',
     &    gendbfn(8)(1:IWB)
      else
        WRITE(ITEM(10),'(A)')     'k LCA/impact   : default'
      endif
      if(gendbfn(9).ne.dgendbfn(9))then
        WRITE(ITEM(11),'(A,1X,A)')'l transport    :',
     &    gendbfn(9)(1:IWB)
      else
        WRITE(ITEM(11),'(A)') 'l transport    : default'
      endif
      if(gendbfn(10).ne.dgendbfn(10))then
        WRITE(ITEM(12),'(A,1X,A)')'m wind pressure:',
     &    gendbfn(10)(1:IWB)
      else
        WRITE(ITEM(12),'(A)') 'm wind pressure: default'
      endif
      if(gendbfn(11).ne.dgendbfn(11))then
        WRITE(ITEM(13),'(A,1X,A)')'n profile      :',
     &    gendbfn(11)(1:IWB)
      else
        WRITE(ITEM(13),'(A)') 'n profile      : default'
      endif
      if(gendbfn(12).ne.dgendbfn(12))then
        WRITE(ITEM(14),'(A,1X,A)')'o plant templ  :',
     &    gendbfn(12)(1:IWB)
      else
        WRITE(ITEM(14),'(A)') 'o plant templ  : default'
      endif
      WRITE(ITEM(15),'(A)') 'p other                 '
      ITEM(16)='  ___________________________________ '
C      ITEM(17)='< convert: ASCII>BIN>ASCII & legacy   '
      ITEM(17)='                                      '
      ITEM(18)='1 test creation bcf                   '
      ITEM(19)='2 make a new database                 '
      if(browse)then
        ITEM(20)='* current mode >> browse              '
      else
        ITEM(20)='* current mode >> editing database    '
      endif
      ITEM(21)='> save model                          '
      ITEM(22)='? Help                                '
      ITEM(23)='- Exit                                '
      mitm=23

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('dbm ',1)

      if(MMOD.eq.8)then
        impx=0
        impy=0
        iw=iwe
        CALL VWMENU('Database manager',ITEM,mitm,impx,impy,IW,
     &    irpx,irpy,INO)
      else
        CALL EMENU(' Database manager',ITEM,mitm,INO)
      endif

      IF(INO.EQ.mitm)THEN

C Before exiting, update the cfg file if database files have changed.
        if(cfgok.and.moddb)then
          dok=.true.
          h(1)='ESP-r detected recent changes which may not have been'
          h(2)='saved to the configuration file. Just to be sure you '
          h(3)='should answer yes. '
          CALL ASKOK(
     &      ' A possible change in database(s) detected.  Update the',
     &      ' model configuration file?',OK,dok,3)
          if(OK)CALL EMKCFGG('-',IER)
        endif
        call erpfree(IBFIL,ISTAT)
        do 244 j=1,MGDB
          if(gendbok(j)) call erpfree(igendbf(j),istat)
 244    continue
        CALL EPAGEND
        STOP
      elseif(INO.EQ.mitm-1)THEN

C Help, if a db loaded then offer user a choice to view its documentation.
        h(1)='You can access a description of the db manager or the'
        h(2)='documentation associated with the current database. '
        CALL EASKABC(' Options:',' ','dbm module help',
     &    'database documentation','continue',irpt,2)
        if(irpt.eq.1)then

C Help via the askabout facility in esru_ask.F The zero requests display
          call askabout('dbm ',0)
        elseif(irpt.eq.2)then

C Db documentation, select from those available.
          call pkavailgbd(seldb,statusok,idbh)
          if(statusok)then
            if(nbdbdoc(idbh).gt.0)then

C Display word-warped blocks of documentaton.
              do 151 i=1,nbdbdoc(idbh)
                call edisp248(iuout,dbdoc(i),80)
 151          continue
              goto 3
            endif
          else
            call usrmsg('Requested database is not available.',' ','W')
          endif
        endif
      elseif(INO.EQ.mitm-2)THEN

C Update the cfg file if database files have changed.
        if(cfgok)then
          dok=.true.
          h(1)='ESP-r detected recent changes which may not have been'
          h(2)='saved to the configuration file. Just to be sure you '
          h(3)='should answer yes. '
          CALL ASKOK(
     &      ' Update the model configuration file to reference the',
     &      ' current database file names?',OK,dok,3)
          if(OK)then
            CALL EMKCFGG('-',IER)
            moddb=.false.
          endif
        endif
      elseif(INO.EQ.mitm-3)THEN

C Switch from browse mode.
        if(browse)then
          h(1)='A password is required for modification of databases'
          pass=' '
          CALL EASKS(pass,' password ',' ',12,'0000',
     &        'password',IER,1)
          if(pass(1:3).eq.'101')then
            browse=.FALSE.
          else
            call usrmsg('Sorry, contact your administrator ',
     &         'or ESRU for the current password ','W')
            browse=.TRUE.
          endif
        else
          browse=.TRUE.
        endif
      elseif(INO.EQ.mitm-4)THEN

C Create new database (minimal).
        call pkavailgbd(seldb,status,mdbhash)
        if(mdbhash.gt.0.and.mdbhash.le.MGDB)then
          idbhash=mdbhash
          IBFIL=igendbf(idbhash)
          write(db,'(a)') seldb
        endif
  31    H(1) ='To create a new database, you are asked for the type of'
        H(2) ='data and a base name for the database (both an ascii &'
        H(3) ='a binary version will be created).'
        dba(1)='multicon_db'
        dba(2)='material'
        dba(3)='optical'
        dba(4)='        '
        dba(5)='acoustic'
        dba(6)='colourimetry'
        dba(7)='micotoxin'
        dba(8)='envir_imp'
        dba(9)='transport'
        dba(10)='pressc_db'
        dba(11)='profiles'
        dba(12)='plant_db'
        dba(13)='component_db'
        lenfn=lnblnk(dba(idbhash))
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(tfile,'(a)') dba(idbhash)(1:lenfn)
        else
          write(tfile,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &      dba(idbhash)(1:lenfn)
        endif
        write(outs,'(3a)') ' Root name of new ',dba(idbhash)(1:lenfn),
     &    ' database file?'
        CALL EASKS(tfile,outs,' ',72,'xx_db','root file name',IER,3)
        IF(tfile(1:2).EQ.'  '.or.ier.NE.0)GOTO 31
        write(LAGENDB,'(a,a)')tfile(1:lnblnk(tfile)),'.gdba'
        write(LGENDB,'(a,a)')tfile(1:lnblnk(tfile)),'.gdb'
        call AGDBNEW(IAFIL,LAGENDB,db,idbhash,'c',IER)
        if(ier.eq.0)then
          call ERAGDB(1,IAFIL,LAGENDB,IBFIL,LGENDB,idbhash,IER)
          if(ier.eq.0)then
            moddb=.true.
            gendbfn(idbhash)=LGENDB

C If sucessful scan, the open the database on ibfil for subsequent tasks.
            CALL ERPFREE(IBFIL,IOS)
            idbw=idbwidth(idbhash)
            ier=0
            call EFOPRAN(IBFIL,LGENDB,idbw,1,IER)
            if(browse)then
              call EDSELCAT(ibfil,idbhash,itm,icl,'-',IER)
            else
              call EDSELCAT(ibfil,idbhash,itm,icl,'M',IER)
            endif
            if(itm.ne.0.and.icl.ne.0)then
              write(outs,*) 'catg & item is ',catmenu(icl),
     &          itmenu(itm)
              call edisp(iuout,outs)
            endif
            goto 3
          endif
        endif
      elseif(INO.EQ.mitm-5)THEN

C Test writing out a bcf file...
C << make a variant which writes out only those constructions which >>
C << are referenced in the current model >>
  38    tfile='test.bcf'
        CALL EASKS(tfile,' BCF file name ? ',' ',72,
     &        'xxx','bcf file name',IER,0)
        IF(tfile(1:2).EQ.'  '.or.ier.NE.0)GOTO 38

C Select some of the constructions for writing. First, scan the
C constructions db header to get list of constructions.
        idbhc=1
        write(currentfile,'(a)') 
     &    gendbfn(idbhc)(1:lnblnk(gendbfn(idbhc)))
        IFB=igendbf(idbhc)
        call scandbhd(IFB,idbhc,ier)
        do 444 ik=1,nbitem(idbhc)
          cnsel(ik)=ittag(ik)
 444    continue
        INPIC=nbitem(idbhc)
        CALL EPICKS(INPIC,icnsel,' ',
     &   ' Select constructions to include',
     &   12,nbitem(idbhc),cnsel,'BCF contents:',IER,3)
        call bcfwrite(iafil,tfile,'p',inpic,cnsel,ier)

      elseif(INO.EQ.mitm-6)THEN

C Nothing associated with this option.
        continue
      ELSEIF(INO.EQ.1)THEN

C Change problem name.
        H(1)='The system configuration file holds the definition of'
        H(2)='the building/plant to be simulated, including the'
        H(3)='names of all of the files required. If the name given'
        H(4)='matches an existing file it will be read in and the'
        H(5)='data will be available for editing.'
        DFILE='/usr/esru/esp-r/defaults/defcfg' 
        LTMP=LCMDFL

C Call EASKF depending on the current file name length.
  289   llt=lnblnk(ltmp)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(LTMP,' Model name ? ',' ',96,DFILE,
     &        'config file name',IER,5)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(LTMP,' Model name ? ',' ',124,DFILE,
     &        'config file name',IER,5)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(LTMP,' Model name ? ',' ',144,DFILE,
     &        'config file name',IER,5)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(LTMP,' Model name ? ',' ',144,DFILE,
     &      'config file name',IER,5)
        else
          CALL EASKF(LTMP,' Model name ? ',' ',96,DFILE,
     &      'config file name',IER,5)
        endif

        IF(LTMP.EQ.' ')GOTO 289
        LCMDFL=LTMP

C Find the path and local file name, reet all gendbfn to defaults.
        call fdroot(LCMDFL,path,LCFGF)
        call edisp(iuout,' ')
        call edisp(iuout,' Scanning the problem description...')
        MODE='ALL '
        itrc=0
        IAPROB=IFIL+2
        call ersysg(LCFGF,IFCFG,IAPROB,MODE,ITRC,IER)
        if(IER.eq.0)then
          CFGOK=.TRUE.
        else
          goto 289
        endif
      ELSEIF(INO.EQ.4)THEN

C Constructions db. Confirm name, if no change and database scanned
C then goto editing, otherwise check its type and scan as required.
        idbhash=1
        db='multicon_db'
        act='-'
        call modifygdb(act,db,idbhash,browse,moddb,ier)
        goto 3
      ELSEIF(INO.EQ.5)THEN

C Materials db, if already known and read in then don't bother rescanning.
        idbhash=2
        db='material'
        act='-'
        call modifygdb(act,db,idbhash,browse,moddb,ier)
        goto 3
      ELSEIF(INO.EQ.6)THEN

C Optics db.  If not found attempt to scan it.
        db='optical'
        goto 3
      ELSEIF(INO.EQ.7)THEN

C Acoustic db.
        idbhash=5
        db='acoustic'
        act='-'
        call modifygdb(act,db,idbhash,browse,moddb,ier)
        goto 3
      ELSEIF(INO.EQ.8)THEN

C Colour db. 
        idbhash=6
        db='colourimetry'
        act='-'
        call modifygdb(act,db,idbhash,browse,moddb,ier)
        goto 3
      ELSEIF(INO.EQ.9)THEN

C Micotoxin db.
        idbhash=7
        db='micotoxin'
        act='-'
        call modifygdb(act,db,idbhash,browse,moddb,ier)
C << >>
      ELSEIF(INO.EQ.10)THEN

C Environmental impacts db.
        idbhash=8
        db='envir_imp'
        act='-'
        call modifygdb(act,db,idbhash,browse,moddb,ier)
        goto 3
      ELSEIF(INO.EQ.11)THEN

C Transport db.
        idbhash=9
        db='transport'
        act='-'
        call modifygdb(act,db,idbhash,browse,moddb,ier)
        goto 3
      ELSEIF(INO.EQ.12)THEN

C Pressure db, check type and contents of file.
        idbhash=10
        db='pressc_db'
        act='-'
        call modifygdb(act,db,idbhash,browse,moddb,ier)
        goto 3
      ELSEIF(INO.EQ.13)THEN

C Event profiles
        idbhash=11
        db='profiles'
        act='-'
        call modifygdb(act,db,idbhash,browse,moddb,ier)
        goto 3
      ELSEIF(INO.EQ.14)THEN

C Plant components.
        idbhash=12
        db='plant_db'
        act='-'
        call modifygdb(act,db,idbhash,browse,moddb,ier)
        goto 3
      ELSEIF(INO.EQ.15)THEN
        call usrmsg(' ',' Other db not yet available...','W')
      ELSE
        GOTO 3
      ENDIF
      goto 3

      END

C ***** dummy routines for c code active descriptions buttons.
      subroutine chgazi(icazi)
      return
      end

      subroutine chgelev(icelev)
      return
      end

C **** kact returns a single character for use as a key in a menu based
C on browse or modify act character.
      character*1 function kact(act,key)
      character act*1,key*1
      if(act.eq.'M'.or.act.eq.'m')then
        kact=key
      else
        kact=' '
      endif
      return
      end

C **** itemtag returns a 12 character unique item name given a root name
C and an index.
      character*12 function itemtag(root,index)
      character*(*) root
      integer lnblnk  ! function definition
      itemtag=' '
      lr=lnblnk(root)
      if(index.le.9)then
        if(lr.le.11)then
          write(itemtag,'(a,i1)') root(1:lr),index
        else
          write(itemtag,'(a,i1)') root(1:11),index
        endif
      elseif(index.gt.9.and.index.le.99)then
        if(lr.le.10)then
          write(itemtag,'(a,i2)') root(1:lr),index
        else
          write(itemtag,'(a,i2)') root(1:10),index
        endif
      elseif(index.gt.99.and.index.le.999)then
        if(lr.le.9)then
          write(itemtag,'(a,i3)') root(1:lr),index
        else
          write(itemtag,'(a,i3)') root(1:9),index
        endif
      endif
      return
      end


C *********** PKAVAILGBD 
C PKAVAILGBD presents a list of currently scanned generic databases
C and return selected dbtype (string), status (logical) and hash
C position.
      subroutine pkavailgbd(seldb,status,idbhash)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)

      dimension dblist(MGDB),idbsel(MGDB),dbsel(MGDB),statarray(MGDB)
      dimension lenn(MGDB),dba(MGDB)
      logical status,statarray
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,dba*12
      character dbnote*72,dblist*36,h*72,seldb*12,dbsel*12

      idbhash=0
      status=.FALSE.

C Array of database types for use in reporting.
      dbsel(1)= 'construction'
      dbsel(2)= 'material    '
      dbsel(3)= 'optical     '
      dbsel(4)= '            '
      dbsel(5)= 'acoustic    '
      dbsel(6)= 'colourimetry'
      dbsel(7)= 'micotoxin   '
      dbsel(8)= 'impact      '
      dbsel(9)= 'transport   '
      dbsel(10)='pressure    '
      dbsel(11)='profiles    '
      dbsel(12)='components  '
      dbsel(13)='other  '

C Setup root names for any new files that might be made.
      dba(1)='multicon_db'
      dba(2)='material'
      dba(3)='optical'
      dba(4)='        '
      dba(5)='acoustic'
      dba(6)='colourimetry'
      dba(7)='micotoxin'
      dba(8)='envir_imp'
      dba(9)='transport'
      dba(10)='pressc_db'
      dba(11)='profiles'
      dba(12)='plant_db'
      dba(13)='component_db'
 
C Loop through list of databases creating strings.
      do 37 i=1,MGDB
        if(gendbok(i))then
          lenn(i)=lnblnk(dbmenu(i))
          write(dblist(i),'(2a)') dbmenu(i)(1:lenn(i)),' available'
          statarray(i)=.TRUE.
        else
          lenn(i)=lnblnk(dbsel(i))
          statarray(i)=.FALSE.
          write(dblist(i),'(2a)') dbsel(i)(1:lenn(i)),' not yet scanned'
        endif
  37  continue
      INPIC=1
      h(1)='You may select one database to focus on.'
      CALL EPICKS(INPIC,idbsel,' ',' Which database:',
     &        36,MGDB,dblist,'databases',IER,1)
      IF(INPIC.EQ.0)then
        seldb='none'
        status=.false.
        idbhash=0
      else
        if(idbsel(1).eq.0)then
          seldb='none'
          status=.false.
          idbhash=0
        else

C If the database has alread been loaded then use dbtag,
C otherwise if a new database for one that has not yet been
C loaded return dba array strings.
          if(statarray(idbsel(1)))then
            seldb=dbtag(idbsel(1))
            status=statarray(idbsel(1))
            idbhash=idbsel(1)
          else
            seldb=dba(idbsel(1))
            status=.true.
            idbhash=idbsel(1)
          endif
        endif
      endif

      return
      end

C *********** dealwithgdb deal with loading and/or conversion
C of importing of generic database files. 
C Act can be 'all_depend' which check from least dependant to
C    most dependant
C ier returned as non-zero if a problem in scanning files.

      subroutine dealwithgdb(act,ier)
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth,aimpth,bsmpth,hvacpth

      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      
      common/gendb2/dbtag(MGDB),dbmenu(MGDB),dbdate(MGDB),dbnote(MGDB),
     &              dbdoc(10)

      dimension idpendord(MGDB),dba(MGDB)

      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,dbnote*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,aimpth*24,bsmpth*24
      character hvacpth*24
      character H*72,outs*124
      character tfile*72,act*12,dba*12,db*12,fs*1
      character LAGENDB*72,LTMP*72,afil*72,LBGENDB*72
      logical unixok
      integer ier

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      if(act(1:10).eq.'all_depend')then

C Set the db hash index and db type in order of dependency (idependord).
        idpendord(1)=10
        dba(1)='pressc_db'
        idpendord(2)=11
        dba(2)='profiles'
        idpendord(3)=9
        dba(3)='transport'
        idpendord(4)=6
        dba(4)='colourimetry'
        idpendord(5)=5
        dba(5)='acoustic'
        idpendord(6)=8
        dba(6)='envi_imp'
        idpendord(7)=2
        dba(7)='material'
        idpendord(8)=1
        dba(8)='multicon_db'

C Open and scan known databases (from least dependant to most dependant).
C In each case check iter return code to see if file can be directly
C scanned (iter=0), if it should be converted from asci (iter=2), or
C if it is a legacy ascii file which might be imported (iter=3), or
C a legacy binary file which can be imported after conversion to an
C legacy ascii version (iter=4).
        do 42 i=1,8
          db=dba(i)
          idp=idpendord(i)
          lenf=lnblnk(gendbfn(idp))
          if(gendbfn(idp)(1:2).eq.'  '.or.lenf.le.1)then
            gendbfn(idp)=dgendbfn(idp)
          endif
          call ckgdbtype(igendbf(idp),gendbfn(idp),db,jdbhash,idbw,ITER)
          if(jdbhash.eq.0)then
            write(outs,'(3a)') 'The database ',
     &        gendbfn(idp)(1:lnblnk(gendbfn(idp))),
     &      ' is probably corrupt or unreadable on this machine.'
            call edisp(iuout,outs)
          elseif(jdbhash.ne.idp)then
            write(outs,'(4a)') 'The database type ',dba(jdbhash),
     &      ' is different than the expected type ',dba(idp)
            call edisp(iuout,outs)
          endif
          if(iter.eq.0)then

C Is binary format, scan into common blocks.
            call SCANDB(igendbf(idp),gendbfn(idp),db,idp,IER)
            CALL ERPFREE(igendbf(idp),IOS)
            ier=0
            call EFOPRAN(igendbf(idp),gendbfn(idp),idbw,1,IER)
C << ? is the next line necessary? >>
            LAGENDB=gendbfn(idp)
            gendbok(idp)=.true.
          elseif(iter.eq.1)then

C << what to do here? ask if new file required? >>
C Debug.
C            write(6,*) 'file not found: ',gendbfn(idp)

            gendbok(idp)=.false.
          elseif(iter.eq.2)then

C Is ASCII format so set file name extension for binary version,
C scan the ASCII file.
            lenf=lnblnk(gendbfn(idp))
            write(outs,*) 'ASCII ',gendbfn(idp)(1:lenf),' to BINARY...'
            call edisp(iuout,outs)

C Debug.
C            write(6,*) 'asci generic asci needing conversion'

            LAGENDB=gendbfn(idp)
            LTMP=gendbfn(idp)
            IAFIL=IFIL+1
            IBFIL=IFIL+2
            lcfgr=lnblnk(LTMP)
            lcfgl4=lcfgr-4
            lcfgl3=lcfgr-3
            if(lcfgr.gt.4)then
              if(LAGENDB(lcfgl4:lcfgr).eq.'.gdba')then
                write(LTMP,'(a,a)') LTMP(1:lcfgl4-1),'.gdb'
              elseif(LAGENDB(lcfgl3:lcfgr).eq.'.dba')then
                write(LTMP,'(a,a)') LTMP(1:lcfgl3-1),'.gdb'
              else
                write(LTMP,'(a,a)') LTMP(1:lcfgr),'.gdb'
              endif
            else
              write(LTMP,'(a,a)') LTMP(1:lcfgr),'.gdb'
            endif
            h(1)='This will be the binary version of the database.'
            LBGENDB=LTMP
  40        CALL EASKS(LBGENDB,' Generic (destination) binary name ? ',
     &        ' ',72,'xxx','generic binary file name',IER,1)
            IF(LBGENDB(1:2).EQ.'  '.or.ier.NE.0)GOTO 40
            call ERAGDB(0,IAFIL,LAGENDB,IBFIL,LBGENDB,idp,IER)
            if(ier.eq.0)then
              gendbfn(idp)=LBGENDB
              db=dbtag(idp)
              call SCANDB(igendbf(idp),gendbfn(idp),db,idp,IER)
              if(ier.eq.0)then
                CALL ERPFREE(igendbf(idp),IOS)
                idbw=idbwidth(idp)
                ier=0
                call EFOPRAN(igendbf(idp),gendbfn(idp),idbw,1,IER)
                gendbok(idp)=.true.
              endif
            endif
          elseif(iter.eq.3)then

C Legacy file detected, import it if possible.
            lenf=lnblnk(gendbfn(idp))
            write(outs,*) 'Legacy ascii ',gendbfn(idp)(1:lenf),
     &        ' needing conversion...'
            call edisp(iuout,outs)
            IAFIL=IFIL+1
            IBFIL=IFIL+2
            afil=gendbfn(idp)
            if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
              write(tfile,'(a)') dba(i)(1:lnblnk(dba(i)))
            else
              write(tfile,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &          dba(i)(1:lnblnk(dba(i)))
            endif
            h(1)='An older format database detected. This will'
            h(2)='be converted into generic asci and binary format.'
            h(3)='Please supply a root name for the new files.'
  49        CALL EASKS(tfile,'Root name of converted database?',
     &        ' ',72,'xx_db','root file name',IER,3)
            IF(tfile(1:2).EQ.'  '.or.ier.NE.0)GOTO 49
            write(LAGENDB,'(a,a)')tfile(1:lnblnk(tfile)),'.gdba'
            write(LBGENDB,'(a,a)')tfile(1:lnblnk(tfile)),'.gdb'
            IBFIL=igendbf(idp)

C Depending on the type of database, import data.
            if(idp.eq.1)then
              call IMPMLC(IAFIL,afil,IBFIL,LBGENDB,idp,IER)
            elseif(idp.eq.2)then
              call imprimmat(IAFIL,afil,IBFIL,LBGENDB,idp,IER)
            elseif(idp.eq.10)then
              call IMPRESSC(IAFIL,afil,IBFIL,LBGENDB,idp,IER)
            elseif(idp.eq.11)then
              call IMPROFL(IAFIL,afil,IBFIL,LBGENDB,idp,IER)
            else
              call edisp(iuout,'Not a legacy file that can be imported')
              gendbok(idp)=.false.
              return
            endif

C Create a matching ascii version of the generic db.
            call MKAGDB(IAFIL,LAGENDB,IBFIL,LBGENDB,idp,IER)
            if(ier.ne.0)then
              call usrmsg(' Problem encountered creating a ',
     &          'text version of the database.','W')
              gendbok(idp)=.false.
              return
            endif
            db=dbtag(idp)
            idbw=idbwidth(idp)
            call SCANDB(ibfil,LBGENDB,db,idp,IER)
            if(ier.eq.0)then
              gendbfn(idp)=LBGENDB
              CALL ERPFREE(igendbf(idp),IOS)
              ier=0
              call EFOPRAN(igendbf(idp),gendbfn(idp),idbw,1,IER)
              gendbok(idp)=.true.
            endif
          elseif(iter.eq.4)then

C Legacy binary materials db found. First transfer from legacy
C binary to legacy ASCII and then import that to generic format.
            lenf=lnblnk(gendbfn(idp))
            write(outs,*) 'Legacy bin ',gendbfn(idp)(1:lenf),
     &        ' needing conversion...'
            call edisp(iuout,outs)

            if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
              write(tfile,'(a)') dba(i)(1:lnblnk(dba(i)))
            else
              write(tfile,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &          dba(i)(1:lnblnk(dba(i)))
            endif
            h(1)='An older format database detected. This will'
            h(2)='be converted into generic asci and binary format.'
            h(3)='Please supply a root name for the new files.'
 149        CALL EASKS(tfile,'Root name of converted database?',
     &        ' ',72,'xx_db','root file name',IER,3)
            IF(tfile(1:2).EQ.'  '.or.ier.NE.0)GOTO 149
            write(afil,'(2a)')   tfile(1:lnblnk(tfile)),'.la'
            write(LAGENDB,'(2a)')tfile(1:lnblnk(tfile)),'.gdba'
            write(LBGENDB,'(2a)')tfile(1:lnblnk(tfile)),'.gdb'
            IAFIL=IFIL+1
            IBFIL=IFIL+2
            if(idp.eq.2)then

C Use standard esp-r common flow for legacy materials db.
              LFMAT=gendbfn(idp)
              IFMAT=IFIL+2
              ier=0
              call EFOPRAN(IFMAT,LFMAT,40,1,IER)
              if(ier.ne.0)then
                call usrmsg('unable to open legacy binary materials db',
     &            LFMAT,'W')
                gendbok(idp)=.false.
                goto 42
              endif 
              CALL MKAPRC(afil,IAFIL,IER)
              IBFIL=IFIL+2
              call imprimmat(IAFIL,afil,IBFIL,LBGENDB,idp,IER)

C Create a matching ascii version of the generic db.
              call MKAGDB(IAFIL,LAGENDB,IBFIL,LBGENDB,idp,IER)
              if(ier.ne.0)then
                call usrmsg(' Problem encountered creating a ',
     &            'text version of the generic database.','W')
                gendbok(idp)=.false.
                goto 42
              endif
            elseif(idp.eq.11)then

C Convert binary events profiles database into the ASCII version
C filling the common blocks. 
              write(LPRFDB,'(a)') gendbfn(idp)
              IPRODB=IFIL+2
              CALL TROPRO(IAFIL,afil,IER)
              if(ier.eq.0)then
                lenf=lnblnk(afil)
                write(outs,*) 'Legacy asci profiles ',afil(1:lenf),
     &            ' created...'
                call edisp(iuout,outs)
                call IMPROFL(IAFIL,afil,IBFIL,LBGENDB,idp,IER)

C Create a matching ascii version of the generic db.
                call MKAGDB(IAFIL,LAGENDB,IBFIL,LBGENDB,idp,IER)
                if(ier.ne.0)then
                  call usrmsg(' Problem encountered creating a ',
     &              'text version of the generic database.','W')
                  gendbok(idp)=.false.
                  goto 42
                endif
              endif
            endif
            db=dbtag(idp)
            idbw=idbwidth(idp)
            call SCANDB(ibfil,LBGENDB,db,idp,IER)
            if(ier.eq.0)then
              gendbfn(idp)=LBGENDB
              CALL ERPFREE(igendbf(idp),IOS)
              ier=0
              call EFOPRAN(igendbf(idp),gendbfn(idp),idbw,1,IER)
              gendbok(idp)=.true.
            endif
          endif
  42    continue
      else

C Deal with a specific database.
        dba(1)='multicon_db'
        dba(2)='material'
        dba(3)='optical'
        dba(4)='       '
        dba(5)='acoustic'
        dba(6)='colourimetry'
        dba(7)='micotoxin'
        dba(8)='envir_imp'
        dba(9)='transport'
        dba(10)='pressc_db'
        dba(11)='profiles'
        dba(12)='plant_db'
        dba(13)='component_db'
        if(act(1:8).eq.'plant_db')then
          i=12
        elseif(act(1:8).eq.'profiles')then
          i=11
        elseif(act(1:9).eq.'pressc_db')then
          i=10
        elseif(act(1:9).eq.'transport')then
          i=9
        elseif(act(1:8).eq.'envi_imp')then
          i=8
        elseif(act(1:12).eq.'colourimetry')then
          i=6
        elseif(act(1:8).eq.'acoustic')then
          i=5
        elseif(act(1:8).eq.'material')then
          i=2
        elseif(act(1:11).eq.'multicon_db')then
          i=1
        endif

C Open and scan this database. Check the iter return code to see
C if file can be directly scanned (iter=0), if it should be
C converted from asci (iter=2), or
C if it is a legacy ascii file which might be imported (iter=3), or
C a legacy binary file which can be imported after conversion to an
C legacy ascii version (iter=4).
        db=dba(i)
        idp=i
        lenf=lnblnk(gendbfn(idp))
        if(gendbfn(idp)(1:2).eq.'  '.or.lenf.le.1)then
          gendbfn(idp)=dgendbfn(idp)
        endif
        call ckgdbtype(igendbf(idp),gendbfn(idp),db,jdbhash,idbw,ITER)
        if(jdbhash.eq.0)then
          write(outs,'(3a)') 'The database ',
     &      gendbfn(idp)(1:lnblnk(gendbfn(idp))),
     &    ' is probably corrupt or unreadable on this machine.'
          call edisp(iuout,outs)
        elseif(jdbhash.ne.idp)then
          write(outs,'(4a)') 'The database type ',dba(jdbhash),
     &    ' is different than the expected type ',dba(idp)
          call edisp(iuout,outs)
        endif
        if(iter.eq.0)then

C Is binary format, scan into common blocks.
          call SCANDB(igendbf(idp),gendbfn(idp),db,idp,IER)
          CALL ERPFREE(igendbf(idp),IOS)
          ier=0
          call EFOPRAN(igendbf(idp),gendbfn(idp),idbw,1,IER)
C << ? is the next line necessary? >>
          LAGENDB=gendbfn(idp)
          gendbok(idp)=.true.
        elseif(iter.eq.1)then

C << what to do here? ask if new file required? >>
C Debug.
C          write(6,*) 'file not found: ',gendbfn(idp)

          gendbok(idp)=.false.
        elseif(iter.eq.2)then

C Is ASCII format so set file name extension for binary version,
C scan the ASCII file.
          lenf=lnblnk(gendbfn(idp))
          write(outs,*) 'ASCII ',gendbfn(idp)(1:lenf),' to BINARY...'
          call edisp(iuout,outs)
          LAGENDB=gendbfn(idp)
          LTMP=gendbfn(idp)
          IAFIL=IFIL+1
          IBFIL=IFIL+2
          lcfgr=lnblnk(LTMP)
          lcfgl4=lcfgr-4
          lcfgl3=lcfgr-3
          if(lcfgr.gt.4)then
            if(LAGENDB(lcfgl4:lcfgr).eq.'.gdba')then
              write(LTMP,'(a,a)') LTMP(1:lcfgl4-1),'.gdb'
            elseif(LAGENDB(lcfgl3:lcfgr).eq.'.dba')then
              write(LTMP,'(a,a)') LTMP(1:lcfgl3-1),'.gdb'
            else
              write(LTMP,'(a,a)') LTMP(1:lcfgr),'.gdb'
            endif
          else
            write(LTMP,'(a,a)') LTMP(1:lcfgr),'.gdb'
          endif
          h(1)='This will be the binary version of the database.'
          LBGENDB=LTMP
  50      CALL EASKS(LBGENDB,' Generic (destination) binary name ? ',
     &      ' ',72,'xxx','generic binary file name',IER,1)
          IF(LBGENDB(1:2).EQ.'  '.or.ier.NE.0)GOTO 50
          call ERAGDB(0,IAFIL,LAGENDB,IBFIL,LBGENDB,idp,IER)
          if(ier.eq.0)then
            gendbfn(idp)=LBGENDB
            db=dbtag(idp)
            call SCANDB(igendbf(idp),gendbfn(idp),db,idp,IER)
            if(ier.eq.0)then
              CALL ERPFREE(igendbf(idp),IOS)
              idbw=idbwidth(idp)
              ier=0
              call EFOPRAN(igendbf(idp),gendbfn(idp),idbw,1,IER)
              gendbok(idp)=.true.
            endif
          endif
        elseif(iter.eq.3)then

C Legacy file detected, import it if possible.
          lenf=lnblnk(gendbfn(idp))
          write(outs,*) 'Legacy ascii ',gendbfn(idp)(1:lenf),
     &      ' needing conversion...'
          call edisp(iuout,outs)
          IAFIL=IFIL+1
          IBFIL=IFIL+2
          afil=gendbfn(idp)
          if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
            write(tfile,'(a)') dba(i)(1:lnblnk(dba(i)))
          else
            write(tfile,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        dba(i)(1:lnblnk(dba(i)))
          endif
          h(1)='An older format database detected. This will'
          h(2)='be converted into generic asci and binary format.'
          h(3)='Please supply a root name for the new files.'
  59      CALL EASKS(tfile,'Root name of converted database?',
     &      ' ',72,'xx_db','root file name',IER,3)
          IF(tfile(1:2).EQ.'  '.or.ier.NE.0)GOTO 59
          write(LAGENDB,'(a,a)')tfile(1:lnblnk(tfile)),'.gdba'
          write(LBGENDB,'(a,a)')tfile(1:lnblnk(tfile)),'.gdb'
          IBFIL=igendbf(idp)

C Depending on the type of database, import data.
          if(idp.eq.1)then
            call IMPMLC(IAFIL,afil,IBFIL,LBGENDB,idp,IER)
          elseif(idp.eq.2)then
            call imprimmat(IAFIL,afil,IBFIL,LBGENDB,idp,IER)
          elseif(idp.eq.10)then
            call IMPRESSC(IAFIL,afil,IBFIL,LBGENDB,idp,IER)
          elseif(idp.eq.11)then
            call IMPROFL(IAFIL,afil,IBFIL,LBGENDB,idp,IER)
          else
            call edisp(iuout,'Not a legacy file that can be imported')
            gendbok(idp)=.false.
            return
          endif

C Create a matching ascii version of the generic db.
          call MKAGDB(IAFIL,LAGENDB,IBFIL,LBGENDB,idp,IER)
          if(ier.ne.0)then
            call usrmsg(' Problem encountered creating a ',
     &        'text version of the database.','W')
            gendbok(idp)=.false.
            return
          endif
          db=dbtag(idp)
          idbw=idbwidth(idp)
          call SCANDB(ibfil,LBGENDB,db,idp,IER)
          if(ier.eq.0)then
            gendbfn(idp)=LBGENDB
            CALL ERPFREE(igendbf(idp),IOS)
            ier=0
            call EFOPRAN(igendbf(idp),gendbfn(idp),idbw,1,IER)
            gendbok(idp)=.true.
          endif
        elseif(iter.eq.4)then

C Legacy binary materials db found. First transfer from legacy
C binary to legacy ASCII and then import that to generic format.
          lenf=lnblnk(gendbfn(idp))
          write(outs,*) 'Legacy bin ',gendbfn(idp)(1:lenf),
     &      ' needing conversion...'
          call edisp(iuout,outs)

          if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
            write(tfile,'(a)') dba(i)(1:lnblnk(dba(i)))
          else
            write(tfile,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        dba(i)(1:lnblnk(dba(i)))
          endif
          h(1)='An older format database detected. This will'
          h(2)='be converted into generic asci and binary format.'
          h(3)='Please supply a root name for the new files.'
 159      CALL EASKS(tfile,'Root name of converted database?',
     &      ' ',72,'xx_db','root file name',IER,3)
          IF(tfile(1:2).EQ.'  '.or.ier.NE.0)GOTO 159
          write(afil,'(2a)')   tfile(1:lnblnk(tfile)),'.la'
          write(LAGENDB,'(2a)')tfile(1:lnblnk(tfile)),'.gdba'
          write(LBGENDB,'(2a)')tfile(1:lnblnk(tfile)),'.gdb'
          IAFIL=IFIL+1
          IBFIL=IFIL+2
          if(idp.eq.2)then

C Use standard esp-r common flow for legacy materials db.
            LFMAT=gendbfn(idp)
            IFMAT=IFIL+2
            ier=0
            call EFOPRAN(IFMAT,LFMAT,40,1,IER)
            if(ier.ne.0)then
              call usrmsg('unable to open legacy binary materials db',
     &          LFMAT,'W')
              gendbok(idp)=.false.
              return
            endif 
            CALL MKAPRC(afil,IAFIL,IER)
            IBFIL=IFIL+2
            call imprimmat(IAFIL,afil,IBFIL,LBGENDB,idp,IER)

C Create a matching ascii version of the generic db.
            call MKAGDB(IAFIL,LAGENDB,IBFIL,LBGENDB,idp,IER)
            if(ier.ne.0)then
              call usrmsg(' Problem encountered creating a ',
     &          'text version of the generic database.','W')
              gendbok(idp)=.false.
              return
            endif
          elseif(idp.eq.11)then

C Event profiles. 
C << move to using common blocks version of events >>
            write(LPRFDB,'(a)') gendbfn(idp)
            IPRODB=IFIL+2
            CALL TROPRO(IAFIL,afil,IER)
            if(ier.eq.0)then
              lenf=lnblnk(afil)
              write(outs,*) 'Legacy asci profiles ',afil(1:lenf),
     &          ' created...'
              call edisp(iuout,outs)
              call IMPROFL(IAFIL,afil,IBFIL,LBGENDB,idp,IER)

C Create a matching ascii version of the generic db.
              call MKAGDB(IAFIL,LAGENDB,IBFIL,LBGENDB,idp,IER)
              if(ier.ne.0)then
                call usrmsg(' Problem encountered creating a ',
     &            'text version of the generic database.','W')
                gendbok(idp)=.false.
                return
              endif
            endif
          endif
          db=dbtag(idp)
          idbw=idbwidth(idp)
          call SCANDB(ibfil,LBGENDB,db,idp,IER)
          if(ier.eq.0)then
            gendbfn(idp)=LBGENDB
            CALL ERPFREE(igendbf(idp),IOS)
            ier=0
            call EFOPRAN(igendbf(idp),gendbfn(idp),idbw,1,IER)
            gendbok(idp)=.true.
          endif
        endif
      endif
      return
      end


C ********** imgdisp
C imgdisp displays images associated with start-up or at specific points
C and the image browser has not been invoked do this now. In the case
C of the GTK version use an in-built GTK function rather than an
C external tool.
      subroutine imgdisp(iforce,focus,ier)
#include "building.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      common/CFGV/icfgv
      common/pophelp/h(60)

C A set of possible image formats which can be accepted within
C a model configuration file. These definitions are held in the
C .esprc file. To add additional image types and/or 3rd
C party display applications edit the ESP-r Install script which
C creates the .esprc file.
C   imgtyp is the number of different image formats supported
C   fmttag (4 char) is a tag for each image formt (e.g. GIF, XBMP)
C   fmtexe (20 char) is the application name used to display
C          images of type fmttag.
      common/showimg/imgtyp,fmttag(5),fmtexe(5)

C Images in the model.
C  imgfmt (4 char) gives the format of each image associated with
C         the model, it must match one of the known fmttag.
C  imgfoc (4 char) associates an image with a specific topic:
C         'FZON' is related to zone composition
C         'FNET' is related to network composition
C         'FCTL' is related to control composition
C         'FDFS' is related to CFD domains
C         'FPER' is related to predicted performance
C         '****' is a general image displayed at startup
C  limgfil (72 char) name of the image file << ?? longer string >>
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton

      character ilist*200,dolist*254
      character fmttag*4,fmtexe*20,tfile*72,focus*4
      character longtfile*144,longtfiledos*144
      character topic*248,head*136,act*1,h*72
      integer iglib  ! if one then X11 if 2 then GTK if 3 text only.

      logical concat,show,unixok

C Check if Unix-based or DOS based.
      call isunix(unixok)

C Loop through the images and display those which match the 'focus'
C and can be displayed with a known utility. Iforce, if set to 1,
C forces display.
      if(noimg.eq.0)return
      if(imgtyp.eq.0)then
        call edisp(iuout,'* No image display application available.')
        call edisp(iuout,'Ask your administrator to define one via')
        call edisp(iuout,'your .esprc file.')
        return
      endif

      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1)then

C If X11 version then request an external agent to display the image.
        if((iton.eq.0.or.iforce.eq.1))then
          do 498 imgv=1,imgtyp
            ilist=' '
            ix=1
            do 49 img=1,noimg
              if(imgfmt(img)(1:3).eq.fmttag(imgv)(1:3))then

C If focus is `****` then show images marked with `****` , otherwise
C begins with and `S`, otherwise attempt to match the focus and the image. 
                show=.false.
                if(focus(1:4).eq.'****'.and.
     &             imgfoc(img)(1:1).eq.'S')show=.true.
                if(imgfoc(img)(1:4).eq.focus(1:4))show=.true.
                if(show)then
                  longtfile=' '
                  if(unixok)then
                    call addpath(limgfil(img),longtfile,concat)
                  else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.  If X11 then an external application is used
C an double quoting is useful. If GTK then internal function used and
C quotes are not required.
                    call addpath(limgfil(img),longtfile,concat)
                    if(iglib.eq.1)then
                      call cmdfiledos(longtfile,longtfiledos,ier)
                    elseif(iglib.eq.2)then
                      call imgfiledos(longtfile,longtfiledos,ier)
                    endif
                    longtfile=' '
                    longtfile=longtfiledos
                  endif
                  ixl=lnblnk(longtfile)
                  ixe=ix+ixl
                  if(ixe.le.200)then
                    WRITE(ilist(ix:ixe),'(a,2x)')longtfile(1:ixl)
                  endif
                  ix=ixe+1
                endif
              endif
 49         continue
            if(ix.gt.1)then
              dolist=' '
              if(fmtexe(imgv)(1:2).eq.'  ')then
                call usrmsg(
     &    'The image display tool has not be defined. Ask your',
     &    'administrator to update the .esprc file definition.','W')
              else

C If cfg recent version then display image documentation as well.
                if(icfgv.gt.3)then
                  call edisp248(iuout,imgdoc(img),100)
                endif
                write(dolist,'(a,2x,a,a)')
     &            fmtexe(imgv)(1:lnblnk(fmtexe(imgv))),
     &            ilist(1:lnblnk(ilist)),' &'
                call runit(dolist,'graph')
                iton=1
              endif
            endif
 498      continue
        endif
      elseif(iglib.eq.2)then

C If GTK version then use GTK calls to display images in popup.
        if((iton.eq.0.or.iforce.eq.1))then

C Setup help text for the popup.
          h(1)='Images can be associated with a model for various'
          h(2)='purposes: '
          h(3)=' * document conditions at the physical site'
          h(4)=' * document assumptions in the model'
          h(5)=' * indicate what performance is expected'
          h(6)='   '
          h(7)='Images are added to a model via the context->images'
          h(8)='menu item. There can be up to 10 images associate with'
          h(9)='a model. '
          h(10)='Images of type GIF and XBM and ?? are supported.'
          CALL PHELPW(10,IHW,IER)

          do 99 img=1,noimg
            if(imgfmt(img)(1:3).eq.'GIF'.or.
     &         imgfmt(img)(1:4).eq.'XBMP'.or.
     &         imgfmt(img)(1:3).eq.'TIF'.or.
     &         imgfmt(img)(1:3).eq.'JPG')then

C If focus is `****` then show images marked with `****` , otherwise
C begins with and `S`, otherwise attempt to match the focus and the image. 
C Create a string topic to pass to the display routine along with
C the image file name.
C << tag for editing ?? >>
              show=.false.
              if(focus(1:4).eq.'****'.and.
     &           imgfoc(img)(1:1).eq.'S')show=.true.
              if(imgfoc(img)(1:4).eq.focus(1:4))show=.true.
              if(show)then
                longtfile=' '
                if(unixok)then
                  call addpath(limgfil(img),longtfile,concat)
                else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
                  call addpath(limgfil(img),longtfile,concat)
                  call cmdfiledos(longtfile,longtfiledos,ier)
                  longtfile=' '
                  longtfile=longtfiledos
                endif
                if(icfgv.gt.3)then
                  idl=lnblnk(imgdoc(img))
                  write(topic,'(a)') imgdoc(img)(1:idl)
                endif

C Provide some feedback, set the title of the popup and then call C
C function in lib/esp_draw.c
                call edisp(iuout,topic)
                write(head,'(2a)') 'Image: ',longtfile(1:ixl)
                act='-'
                call popupimage(head,topic,act,longtfile)
                iton=1
              endif
            endif
  99      continue
        endif
      endif

      return
      end

C *********** modifygdb deal with loading and/or conversion
C of importing of generic database files. Act is << >>,
C db is text id of database type expeced to be found,
C jdbhash is the gendbfn array index expected to be found,
C browse restricts user options, moddb is returned as true
C if database names have changed.

      subroutine modifygdb(act,db,jdbhash,browse,moddb,ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth,aimpth,bsmpth,hvacpth
      common/rpath/path
      common/deflt4/dinstpath

C Generic db commons.
C      common/gendb/LGENDB

      common/gendb1/idbwidth(MGDB),lastr(MGDB),nbdbdoc(MGDB),
     &              nbunit(MGDB),nbsrc(MGDB),nbcat(MGDB),nbitem(MGDB),
     &              idbver(MGDB)      

      dimension dba(MGDB)

      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,aimpth*24,bsmpth*24
      character hvacpth*24
      character H*72,outs*124,LTMP*72,afil*72,LEXP*72
      character tfile*72,act*1,dba*12,db*12,dbask*12,fs*1,doit*248
      character LAGENDB*72,LGENDB*72
      character dinstpath*48,path*72,dirpath*48
      logical unixok,browse,moddb,copydef,XST,newfile
      logical legacy,asciifound
      integer ier

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Setup string buffer with distribution database folder name.
      write(dirpath,'(4a)') dinstpath(1:lnblnk(dinstpath)),
     &  fs,'databases',fs

C Assume no changes in database, that file does not exist and
C it is not a new file. Also remember the database tag (db) in dbask.
      moddb=.false.
      XST=.false.
      newfile=.false.
      legacy=.false.
      asciifound=.false.
      dbask=db

C Setup root names for any new files that might be made.
      dba(1)='multicon_db'
      dba(2)='material'
      dba(3)='optical'
      dba(4)='       '
      dba(5)='acoustic'
      dba(6)='colourimetry'
      dba(7)='micotoxin'
      dba(8)='envir_imp'
      dba(9)='transport'
      dba(10)='pressc_db'
      dba(11)='profiles'
      dba(12)='plant_db'
      dba(13)='component_db'

C Confirm name of database.
      tfile=gendbfn(jdbhash)
 129  if(jdbhash.eq.1)then
        h(1)='This file holds information on constructions (multi-'
        h(2)='layer (highest level abstraction).'
        CALL EASKS(tfile,' Constructions db file name ? ',' ',72,
     &    dgendbfn(jdbhash),'MLC db file name',IER,2)
      elseif(jdbhash.eq.2)then
        h(1)='This file holds the attributes of each material.'
        CALL EASKS(tfile,' Material db file name ? ',' ',72,
     &    dgendbfn(jdbhash),'Mat db file name',IER,1)
      elseif(jdbhash.eq.4)then
        call edisp(iuout,'hash of 4 currently not used.')
      elseif(jdbhash.eq.5)then
        h(1)='The room acoustic db holds the absorption coefficients of'
        h(2)='materials and systems in order to perform the calculation'
        h(3)='of the reverberation time in a zone.' 
        CALL EASKS(tfile,' Acoustic db file name ? ',' ',72,
     &        dgendbfn(jdbhash),'acoustic db file name',IER,3)
      elseif(jdbhash.eq.6)then
        h(1)='This file holds RGB data pointed to by'
        h(2)='material database entries. '
        CALL EASKS(tfile,' Colour db file name ? ',' ',72,
     &        dgendbfn(jdbhash),'colour db file name',IER,2)
      elseif(jdbhash.eq.7)then
        h(1)='This file holds growth data for mixotoxins and does not'
        h(2)='reference any other database.'
        CALL EASKS(tfile,' Micotoxin db file name ? ',' ',72,
     &        dgendbfn(jdbhash),'micotoxin db file name',IER,2)
      elseif(jdbhash.eq.8)then
        h(1)='The environmental impacts db holds the environmental'
        h(2)='information in order to perform the calculation'
        h(3)='of the Life Cycle Impact Assessment (LCIA) of' 
        h(4)='a material, a construction or the whole project.' 
        CALL EASKS(tfile,' Environmental impacts db file name ? ',' ',
     &        72,dgendbfn(jdbhash),'LCA impacts db file',IER,4)
      elseif(jdbhash.eq.9)then
        H(1)='The transport db holds the environmental impacts'
        H(2)='of various mean of transport, which are used'
        H(3)='during the project life span.'
        CALL EASKS(tfile,' Transport db file name ? ',' ',
     &        72,dgendbfn(jdbhash),'transport db file name',IER,3)
      elseif(jdbhash.eq.10)then
        h(1)='This file holds sets of wind pressure coefficients.'
        CALL EASKS(tfile,' Pressure db file name ? ',' ',72,
     &        dgendbfn(jdbhash),'pressure db file name',IER,1)
      elseif(jdbhash.eq.11)then
        h(1)='This file holds profiles for use by casual gains.'
        CALL EASKS(tfile,' Event profiles db file name ? ',' ',72,
     &        dgendbfn(jdbhash),'event profiles db file name',IER,1)
      elseif(jdbhash.eq.12)then
        h(1)='This file holds templates for plant components.'
        CALL EASKS(tfile,' Plant component db file name ? ',' ',72,
     &        dgendbfn(jdbhash),'plant component db file name',IER,1)
      elseif(jdbhash.eq.13)then
        call usrmsg(' ',' Component db not yet available...','W')
        return
      elseif(jdbhash.eq.14)then
        call usrmsg(' ',' Other db not yet available...','W')
        return
      endif
      if(tfile(1:2).EQ.'  '.or.ier.NE.0)GOTO 129

C File name is non-blank so check if it exists, what its type is
C and whether it needs to be converted from asci to binary etc
C or has previously been opened.
      lenf=lnblnk(tfile)
      if(tfile(1:lnblnk(tfile)).EQ.gendbfn(jdbhash)(1:lenf).and.
     &   gendbok(jdbhash))then
        continue
      else
        gendbfn(jdbhash)=tfile
      endif

C Check the database type returing information via the variables
C idbhash (which type of database) and ITER (what state it is in).
      db=dba(jdbhash)
      idbw=idbwidth(jdbhash)
      call ckgdbtype(igendbf(jdbhash),gendbfn(jdbhash),db,idbhash,idbw,
     &  ITER)
      if(idbhash.eq.0)then
        write(outs,'(3a)') 'The database ',
     &    gendbfn(jdbhash)(1:lnblnk(gendbfn(jdbhash))),
     &    ' is probably corrupt or unreadable on this machine.'
        call edisp(iuout,outs)
      elseif(jdbhash.ne.idbhash)then
        write(outs,'(4a)') 'The database type ',dba(idbhash),
     &    ' is different than the expected type ',dba(jdbhash)
        call edisp(iuout,outs)
      endif
      if(iter.eq.0)then

C The file is a binary file of the correct type so just continue.
        XST=.true.
      elseif(iter.eq.1)then
        XST=.false.
      elseif(iter.eq.2)then

C << why not just do the conversion? >>
        call usrmsg('This is an ASCII version of the database',
     &    'please request a conversion ASCII-BINARY.','W')
        XST=.true.
        asciifound=.true.
      elseif(iter.eq.3)then
        call usrmsg('This is a legacy version of the database',
     &    'please request an conversion.','W')
        XST=.true.
        legacy=.true.
      elseif(iter.eq.4)then
        call usrmsg('This is a legacy materials database',
     &    'please request an conversion.','W')
        XST=.true.
        legacy=.true.
      endif
      IBFIL=igendbf(jdbhash)
      CALL ERPFREE(IBFIL,ISTAT)
C      call FINDFIL(tfile,XST)

C Note in the code below "isw" is an index returned based on the
C users response to what they want done. The variable "iter" is
C returned from the call to ckgdbtype based on the format of the
C file (current or legacy & text or binary).
 130  H(1)='The following options can be selected:'
      H(2)=' '
      H(3)='a) If the file exists than you can browse the database '
      H(4)='   and if you have permission to update the file you'
      H(5)='   can edit the entities in the database.'
      H(6)=' '
      H(7)='b) select another file: you will asked for a file'
      H(8)='   name and if it is found you can browse or edit it.'
      H(9)=' '
      H(10)='c) make a new database: a new (minimal) database will'
      H(11)='   be created based on the file name you supply.'
      H(12)=' '
      H(13)='d) copy default database: a project copy of the default'
      H(14)='  database will be made and you will be offered a '
      H(15)='  chance to rename it.'
      if(.NOT.XST)then

C If db does not exist offer the following choices:
        idno=2
        isw=0
        write(outs,'(2a)') 'Options for ',gendbfn(jdbhash)
        call MENUATOL(outs,'Database options:',
     &    'a browse/edit (NOT APPLICABLE)','b select another database',
     &    'c make a new database','d make project copy of database',
     &    ' ',' ',' ',' ',
     &    ' ',' ',' ',' ',isw,idno,15)
        copydef=.true.
        newfile=.true.
        idbhash=jdbhash
      else

C If db exists offer the following choices:
        if(gendbfn(jdbhash)(1:25).eq.'/usr/esru/esp-r/databases'.or.
     &     gendbfn(jdbhash)(1:28).eq.
     &     'c:\\user\\esru\\esp-r\\databases')then

C If corporate database then offer the following choices:
          idno=1
          isw=0
          write(outs,'(2a)') 'Options for ',gendbfn(jdbhash)
          if(asciifound.and.legacy)then
            call MENUATOL(outs,'Database options:',
     &       'a browse or edit this database',
     &       'b select another database','c make a new database',
     &       'd make project copy of database',
     &       'e binary >> ascii export','f ascii >> binary import',
     &       'g legacy conversion',' ',' ',' ',' ',' ',isw,idno,15)
          elseif(asciifound.and.(.NOT.legacy))then
            call MENUATOL(outs,'Database options:',
     &       'a browse or edit this database',
     &       'b select another database','c make a new database',
     &       'd make project copy of database',
     &       'e binary >> ascii export','f ascii >> binary import',
     &       ' ',' ',' ',' ',' ',' ',isw,idno,15)
          elseif((.NOT.asciifound).and.legacy)then
            call MENUATOL(outs,'Database options:',
     &       'a browse or edit this database',
     &       'b select another database','c make a new database',
     &       'd make project copy of database',
     &       'e binary >> ascii export','f N/A',
     &       'g legacy conversion',' ',' ',' ',' ',' ',isw,idno,15)
          else
            call MENUATOL(outs,'Database options:',
     &       'a browse or edit this database',
     &       'b select another database','c make a new database',
     &       'd make project copy of database',
     &       'e binary >> ascii export',' ',' ',
     &       ' ',' ',' ',' ',' ',isw,idno,15)
          endif
          copydef=.false.
          if(isw.eq.3)then
            newfile=.true.
            idbhash=jdbhash
          else
            newfile=.false.
          endif
        else

C If db exists locally offer the following choices:
          idno=1
          isw=0
          write(outs,'(2a)') 'Options for ',gendbfn(jdbhash)
          if(asciifound.and.legacy)then
            call MENUATOL(outs,'Database options:',
     &       'a browse or edit this database',
     &       'b select another database','c make a new database',
     &       'd make copy of database',
     &       'e binary >> ascii export','f ascii >> binary import',
     &       'g legacy conversion',' ',' ',' ',' ',' ',isw,idno,15)
          elseif(asciifound.and.(.NOT.legacy))then
            call MENUATOL(outs,'Database options:',
     &       'a browse or edit this database',
     &       'b select another database','c make a new database',
     &       'd make copy of database',
     &       'e binary >> ascii export','f ascii >> binary import',
     &       ' ',' ',' ',' ',' ',' ',isw,idno,15)
          elseif((.NOT.asciifound).and.legacy)then
            call MENUATOL(outs,'Database options:',
     &       'a browse or edit this database',
     &       'b select another database','c make a new database',
     &       'd make copy of database',
     &       'e binary >> ascii export','f N/A',
     &       'g legacy conversion',' ',' ',' ',' ',' ',isw,idno,15)
          else
            call MENUATOL(outs,'Database options:',
     &       'a browse or edit this database',
     &       'b select another database','c make a new database',
     &       'd make copy of database',
     &       'e binary >> ascii export',' ',' ',
     &       '  ',' ',' ',' ',' ',isw,idno,15)
          endif
          copydef=.true.
          if(isw.eq.3)then
            newfile=.true.
            idbhash=jdbhash
          else
            newfile=.false.
            idbhash=jdbhash
          endif
        endif
      endif

C If nothing selected just return,
      if(isw.eq.0)then
        jdbhash=0
        return
      elseif(isw.eq.1.and.newfile)then

C If file does not exist return, if file exists then scan it, convert if
C necessary do jump to 42.
        jdbhash=0
        return
      elseif(isw.eq.1.and.(.NOT.newfile))then
         
C Check first few lines of the file to see its type. If generic
C binary then scan it, if unknown file type jump to menu control and
C if a legacy file convert it and save binary and ascii version.
        idbw=idbwidth(jdbhash)
        call ckgdbtype(IBFIL,gendbfn(jdbhash),db,idbhash,idbw,ITER)
        if(idbhash.eq.0)then
          write(outs,'(3a)') 'The database ',
     &      gendbfn(jdbhash)(1:lnblnk(gendbfn(jdbhash))),
     &    ' is probably corrupt or unreadable on this machine.'
          call edisp(iuout,outs)
        elseif(jdbhash.ne.idbhash)then
          write(outs,'(4a)') 'The database type ',dba(idbhash),
     &      ' is different than the expected type ',dba(jdbhash)
          call edisp(iuout,outs)
        else
          idbhash=jdbhash
        endif

        if(iter.eq.0)then
          call SCANDB(IBFIL,gendbfn(idbhash),db,idbhash,IER)
          if(ier.ne.0) goto 129
          gendbok(idbhash)=.true.
          moddb=.true.
          goto 42
        elseif(iter.eq.1)then

C scan returned advise that file did not exist or could not be
C read so go back and ask again.
          goto 129
        elseif(iter.eq.2)then

C Convert ascii to binary for a working copy.
          LTMP='  '
          lcfgr=lnblnk(gendbfn(idbhash))
          lcfgl4=lcfgr-4
          lcfgl3=lcfgr-3
          if(lcfgr.gt.4)then
            if(gendbfn(idbhash)(lcfgl4:lcfgr).eq.'.gdba')then
              write(LTMP,'(a,a)') gendbfn(idbhash)(1:lcfgl4-1),'.gdb'
            elseif(gendbfn(idbhash)(lcfgl3:lcfgr).eq.'.dba')then
              write(LTMP,'(a,a)') gendbfn(idbhash)(1:lcfgl3-1),'.gdb'
            else
              write(LTMP,'(a,a)') gendbfn(idbhash)(1:lcfgr),'.gdb'
            endif
          else
            write(LTMP,'(a,a)') gendbfn(idbhash)(1:lcfgr),'.gdb'
          endif
          LGENDB=LTMP
          IAFIL=IFIL+1
          call ERAGDB(1,IAFIL,gendbfn(idbhash),IBFIL,LGENDB,idbhash,
     &      IER)
          if(ier.ne.0) goto 129
          gendbok(idbhash)=.true.
          gendbfn(idbhash)=LGENDB
          moddb=.true.
          goto 42
        elseif(iter.eq.3)then

C A legacy db found. Convert it to generic ascii and binary.
          lenf=lnblnk(gendbfn(idbhash))
          write(outs,*) 'Legacy ascii ',gendbfn(idbhash)(1:lenf),
     &      ' needing conversion...'
          call edisp(iuout,outs)
          if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
            write(tfile,'(a)') dba(idbhash)(1:lnblnk(dba(idbhash)))
          else
            write(tfile,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        dba(idbhash)(1:lnblnk(dba(idbhash)))
          endif
          h(1)='An older format database detected. This will'
          h(2)='be converted into generic asci and binary format.'
          h(3)='Please supply a root name for the new files.'
  49      CALL EASKS(tfile,'Root name of converted database?',
     &      ' ',72,'xx_db','root file name',IER,3)
          IF(tfile(1:2).EQ.'  '.or.ier.NE.0)GOTO 49

C In this case gendbfn() is the legacy file and LGENDB is the generic file.
          write(LAGENDB,'(a,a)')tfile(1:lnblnk(tfile)),'.gdba'
          write(LGENDB,'(a,a)')tfile(1:lnblnk(tfile)),'.gdb'

C << the next 3 lines might be useful to undo association of database >>
C            icurfu=igendbf(jdbhash)
C            curfile=gendbfn(jdbhash)
C            IBFIL=igendbf(jdbhash)

C Depending on the type of database call various conversion functions.
          if(dbask(1:8).eq.'material')then
            call imprimmat(IAFIL,gendbfn(idbhash),IBFIL,LGENDB,
     &        idbhash,IER)
          elseif(dbask(1:9).eq.'pressc_db')then
            call IMPRESSC(IAFIL,gendbfn(idbhash),IBFIL,LGENDB,
     &        idbhash,IER)
          elseif(dbask(1:8).eq.'profiles')then
            call IMPROFL(IAFIL,gendbfn(idbhash),IBFIL,LGENDB,
     &        idbhash,IER)
          elseif(dbask(1:11).eq.'multicon_db')then
            call IMPMLC(IAFIL,gendbfn(idbhash),IBFIL,LGENDB,idbhash,
     &        IER)
          endif
          if(ier.eq.0)then
            call MKAGDB(IAFIL,LAGENDB,IBFIL,LGENDB,idbhash,IER)
            if(ier.ne.0) goto 129
            gendbok(idbhash)=.true.
            gendbfn(idbhash)=LGENDB
            moddb=.true.
C << next lines ask if user want to use this database >>
C << and if not to back out to the earlier database name >>
C              dok=.true.
C              h(1)='<< text to be added >> '
C              write(outs,'(3a)') 'Use ',LGENDB(1:lnblnk(LGENDB)),'?'
C              call askok(' ',outs,OK,dok,1)
C              if(OK)then
C                ...
C              else
C Re-establish original file, idbhash is returned.
C                idbw=24
C                db='any'
C                call ckgdbtype(icurfu,curfile,db,idbhash,idbw,ITER)
C                if(iter.eq.0)then
C                  call SCANDB(icurfu,curfile,db,idbhash,IER)
C                  if(ier.eq.0)then
C                    CALL ERPFREE(icurfu,IOS)
C                    idbw=idbwidth(idbhash)
C                    call EFOPRAN(icurfu,curfile,idbw,1,IER)
C                  endif
C                endif

            goto 42
          endif
        elseif(iter.eq.4)then

C Legacy binary db found. First transfer from legacy binary
C (gendbfn()) to legacy ASCII (afil) and then import that to
C generic format ASCII (LAGENDB) and binary (LGENDB).
          lenf=lnblnk(gendbfn(idbhash))
          write(outs,*) 'Legacy bin ',gendbfn(idbhash)(1:lenf),
     &      ' needing conversion...'
          call edisp(iuout,outs)
          if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
            write(tfile,'(a)') dba(idbhash)(1:lnblnk(dba(idbhash)))
          else
            write(tfile,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        dba(idbhash)(1:lnblnk(dba(idbhash)))
          endif
          h(1)='An older format database detected. This will'
          h(2)='be converted into generic asci and binary format.'
          h(3)='Please supply a root name for the new files.'
 149      CALL EASKS(tfile,'Root name of converted database?',
     &      ' ',72,'xx_db','root file name',IER,3)
          IF(tfile(1:2).EQ.'  '.or.ier.NE.0)GOTO 149
          write(afil,'(2a)')   tfile(1:lnblnk(tfile)),'.la'
          write(LAGENDB,'(2a)')tfile(1:lnblnk(tfile)),'.gdba'
          write(LGENDB,'(2a)')tfile(1:lnblnk(tfile)),'.gdb'
          IAFIL=IFIL+1
          IBFIL=IFIL+2
          if(dbask(1:8).eq.'material')then

C Use standard esp-r common code for legacy materials db.
            LFMAT=gendbfn(idbhash)
            IFMAT=IFIL+2
            ier=0
            call EFOPRAN(IFMAT,LFMAT,40,1,IER)
            if(ier.ne.0)then
              call usrmsg('unable to open legacy binary materials db',
     &          LFMAT,'W')
              jdbhash=0
              return
            endif 
            CALL MKAPRC(afil,IAFIL,IER)
            IBFIL=IFIL+2

C Depending on the type of database call various conversion functions.
            if(dbask(1:8).eq.'material')then
              call imprimmat(IAFIL,afil,IBFIL,LGENDB,idbhash,IER)
            endif

C Create a matching ascii version of the generic db.
            call MKAGDB(IAFIL,LAGENDB,IBFIL,LGENDB,idbhash,IER)
            if(ier.ne.0)then
              call usrmsg(' Problem encountered creating a ',
     &          'text version of the generic database.','W')
              jdbhash=0
              return
            endif
            moddb=.true.
            jdbhash=idbhash
            gendbok(idbhash)=.true.
            gendbfn(idbhash)=LGENDB
            goto 42
          elseif(dbask(1:8).eq.'profiles')then

C Event profiles database. 
C << need to move to common block version of data >>
            write(LPRFDB,'(a)')  gendbfn(idbhash)
            IPRODB=IFIL+2
            CALL TROPRO(IAFIL,afil,IER)
            if(ier.eq.0)then
              lenf=lnblnk(afil)
              write(outs,*) 'Legacy asci profiles ',afil(1:lenf),
     &          ' created...'
              call edisp(iuout,outs)
              call IMPROFL(IAFIL,afil,IBFIL,LGENDB,jdbhash,IER)

C Create a matching ascii version of the generic db.
              call MKAGDB(IAFIL,LAGENDB,IBFIL,LGENDB,jdbhash,IER)
              if(ier.ne.0)then
                call usrmsg(' Problem encountered creating a ',
     &            'text version of the generic database.','W')
                jdbhash=0
                return
              endif
            endif
            moddb=.true.
            jdbhash=idbhash
            gendbok(idbhash)=.true.
            gendbfn(idbhash)=LGENDB
            goto 42
          endif
        elseif(iter.eq.-301)then

C Nothing found, set XST to false and present options.
          XST=.false.
          goto 130
        endif
      elseif(isw.eq.2)then
        goto 129
      elseif(isw.eq.3)then

C Create a new (minimal) database. Scan current name for .gdb ending.
        LTMP='  '
        lcfgr=lnblnk(gendbfn(idbhash))
        lcfgl3=lcfgr-3
        if(lcfgr.gt.4)then
          if(gendbfn(idbhash)(lcfgl3:lcfgr).eq.'.gdb')then
            write(LTMP,'(a)') gendbfn(idbhash)(1:lcfgr)
          elseif(gendbfn(idbhash)(lcfgl3:lcfgr).eq.'.dba')then
            write(LTMP,'(a,a)') gendbfn(idbhash)(1:lcfgl3-1),'.gdb'
          else
            write(LTMP,'(a,a)') gendbfn(idbhash)(1:lcfgr),'.gdb'
          endif
        else
          write(LTMP,'(a,a)') gendbfn(idbhash)(1:lcfgr),'.gdb'
        endif
        lenf=lnblnk(LTMP)
        gendbfn(idbhash)=LTMP
        LGENDB=LTMP
        write(LAGENDB,'(2a)')LTMP(1:lenf),'a'
        moddb=.true.
        jdbhash=idbhash
        IAFIL=IFIL+1
        goto 43
      elseif(isw.eq.4)then

C If the default is to be copied ask for local name. Use LTMP for
C the file name as known to esp-r and LCOPY for the system cp call.   
        if(copydef)then
          call edisp(iuout,'The source file is:')
          call edisp(iuout,dgendbfn(idbhash))
        else
          call edisp(iuout,'The source file is:')
          call edisp(iuout,gendbfn(idbhash))
        endif
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(tfile,'(2a)') dba(idbhash)(1:lnblnk(dba(idbhash))),
     &      '.gdb'
        else
          write(tfile,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      dba(idbhash)(1:lnblnk(dba(idbhash))),'.gdb'
        endif
        h(1)='This is the name of the local copy of the database.'
 549    CALL EASKS(tfile,'Name of local database?',
     &    ' ',72,'xx_db','local file name',IER,3)
        IF(tfile(1:2).EQ.'  '.or.ier.NE.0)GOTO 549
        write(LTMP,'(3a)') path(1:lnblnk(path)),fs,
     &    tfile(1:lnblnk(tfile))
        if(copydef)then
          doit = ' '
          write(doit,'(4a)',IOSTAT=IOS,ERR=549) 'cp ',
     &      dgendbfn(idbhash)(1:lnblnk(dgendbfn(idbhash))),' ',
     &      LTMP(1:lnblnk(LTMP))
          call usrmsg('copying default file via',doit,'-')
          call runit(doit,'-')
          gendbfn(idbhash)=LTMP
        else
          doit = ' '
          write(doit,'(4a)',IOSTAT=IOS,ERR=549) 'cp ',
     &      gendbfn(idbhash)(1:lnblnk(gendbfn(idbhash))),' ',
     &      LTMP(1:lnblnk(LTMP))
          call usrmsg('copying default file via',doit,'-')
          call runit(doit,'-')
          gendbfn(idbhash)=LTMP
        endif
        gendbok(idbhash)=.true.
        moddb=.true.
        jdbhash=idbhash
        goto 42
      elseif(isw.eq.5.and.(.NOT.newfile))then

C Make an ascii version of current database. When finished return
C to file options.
        LEXP='  '
        lcfgr=lnblnk(gendbfn(idbhash))
        lcfgl4=lcfgr-4
        lcfgl3=lcfgr-3
        lcfgl2=lcfgr-2
        if(lcfgr.gt.4)then
          if(gendbfn(idbhash)(lcfgl3:lcfgr).eq.'.gdb')then
            write(LEXP,'(a,a)') gendbfn(idbhash)(1:lcfgl3-1),'.gdba'
          elseif(gendbfn(idbhash)(lcfgl2:lcfgr).eq.'.db')then
            write(LEXP,'(a,a)') gendbfn(idbhash)(1:lcfgl2-1),'.gdba'
          else
            write(LEXP,'(a,a)') gendbfn(idbhash)(1:lcfgr),'.gdba'
          endif
        else
          write(LEXP,'(a,a)') gendbfn(idbhash)(1:lcfgr),'.gdba'
        endif
        IAFIL=IFIL+1
        IBFIL=igendbf(idbhash)
        LGENDB=gendbfn(idbhash)
  41    h(1)='This must be the text version of a generic database.'
        CALL EASKS(LEXP,' Export ascii name ? ',' ',72,
     &    'xx','generic ascii file name',IER,1)
        IF(LEXP(1:2).EQ.'  '.or.LEXP(1:4).eq.'UNKN')GOTO 41
        call MKAGDB(IAFIL,LEXP,IBFIL,LGENDB,idbhash,IER)
        if(ier.eq.0)then
          call usrmsg(' Conversion complete (no errors).',' ','P')
        endif
        goto 130
      elseif(isw.eq.6.and.asciifound)then
        db=dba(idbhash)
        call dealwithgdb(db,ier)
        goto 42
      elseif(isw.eq.7.and.legacy)then
        db=dba(idbhash)
        call dealwithgdb(db,ier)
        goto 42
      endif
      return

C Offer browsing/editing for the selected db.
  42  CALL ERPFREE(IBFIL,IOS)
      idbw=idbwidth(idbhash)
      ier=0
      call EFOPRAN(IBFIL,gendbfn(idbhash),idbw,1,IER)
      if(ier.eq.0)then
        if(browse)then
          call EDSELCAT(ibfil,idbhash,itm,icl,'-',IER)
        else
          call EDSELCAT(ibfil,idbhash,itm,icl,'M',IER)
        endif
        write(outs,'(a,2i3)')'catg and item returned ',itm,icl
        call edisp(iuout,outs)
        return
      else
        call edisp(iuout,'Problem opening the database ')
        call edisp(iuout,gendbfn(idbhash))
        itm=0
        icl=0
        return
      endif

C Create a minimal db and then offer browsing/editing.
  43  call AGDBNEW(IAFIL,LAGENDB,db,idbhash,'c',IER)
      if(ier.eq.0)then
        call ERAGDB(0,IAFIL,LAGENDB,IBFIL,LGENDB,idbhash,IER)
        if(ier.eq.0)then
          gendbok(idbhash)=.true.
          gendbfn(idbhash)=LGENDB
          goto 42
        else
          return
        endif
      endif

      return
      end

C Dummy routines from common3dv.F needed to match implied
C calls in esru_lib.F
      SUBROUTINE ADJVIEW(IER)
      ier=0
      return
      end

      SUBROUTINE EMKVIEW(IUO,CFGOK,IER)
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

      SUBROUTINE PLELEV(direc)
      CHARACTER direc*1
      return
      end
