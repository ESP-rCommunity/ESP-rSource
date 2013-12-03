C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-6.
 
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

C ************************ PROGRAM ********************************** 
C ----------------------------------------------------------------------
      program cdb
C ----------------------------------------------------------------------
C ***********************************************************************
C The module cdb is the main controlling routine for the management of the components 
C database. 
C
C The data structures used by the module is held in two include files:
C   gencompDB.h  -  parameter specifications 
C   gencompDB_common.h  -  common variables
C  
C The module uses the following routines. 
C
C   CDBDomainExplore  - presents a list of the technical domains in the database and
C     allows user to manage those domains or 'drill down' further
C     to categories.
C   CDBCategoryExplore -  presents a list of the component categories in a domain, 
C     allows user to manage those categories or 'drill down' to view
C     spedific components.
C   CDBItemExplore -  presents a list of the components in a category, allows the user
C     to manage those components or 'drill down' to view the data associated
C     with that component. 
C   CDBPSetsExplore - presents a list of the parameter sets associated with a component and 
C     allows the user to manage those or 'drill down' to view parameters in a
C     specific set [Uses CDBPSetList]
C   CDBParamExplore - presents a list of parameters from a set, allowing the user to manage 
C     and edit specific data entries [Uses CDBParamList,CDBParamEdit]        
C   CDBManageStruct -  this routine allows users to add/delete/edit domains,categories, parameter 
C     sets and parameters.
C   CDBNewDataBase - sets up a new components database.
C   CDBImport  - this routine imports an ASCII component text descriptor and inserts it into 
C     the database. 
C   CDBExport  -  exports all or a subset of the database to an ASCII file (.csv or tab separated).
C   CDBSaveExpDatabase - writes/exports a database held in memory to the standard database format.
C     alternatively different formats adopted (.csv,tab separated, etc.)

C These additional routines are used by other ESP-r modules and include searching and 
C retrieving data for a specific component - this is returned to a list, which
C is then decoded by a dedicated plugin, depending upon the technical domain of 
C the data. 
C
C The following common routines are available.
C   CDBScanDatabase  - scans the components database and returns data according
C     to the scan type. 'light' returns structural data only, 
C     'detailed' reads the full database into memory.
C   
C   CDBGetItemData  -  takes up to three strings 'domain_id','category_id' and 
C     'item_id', searches the database for a match and extracts 
C     the data if a match is found; only 'item_id' is mandatory, 
C     the other two strings can be used to search more efficiently;
C     the data is returned to a generic list.
C   CDBPsetList  -       Presents a list of parameter sets and returns a selected set.
C   CDBParamList -  Presents a list of parameters in a set and returns a selected parameter.
C   CDBParamEdit -  Allows a user to edit the data associated with a specific parameter.

      USE START_UP

#include "building.h"
#include "model.h"
#include "espriou.h"
#include "gencompDB.h"

      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition

C The Plant component DataBase program maintains a
C database containing plant component descriptions.
C The database holds the data needed by ESRUbps to
C allow simulation of plant operation.
C Facilities are provided to enter, list, edit and
C delete component descriptions.
C The routine PCHELP contains a description of a 
C database entry.
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

C Defaults.
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DICONDBFL

C Path to problem and command line file (if any).
      common/rpath/path
      common/rcmd/LCMDFL
      common/appw/iappw,iappx,iappy

      common/OUTIN/IUOUT,IUIN
      COMMON/GFONT/IFS,ITFS,IMFS
      common/FILEP/IFIL
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

C Name of current application
      common/APPNAME/cAppName
       
      character cAppName*12

      logical   unixok,OK,DOK,XST,EDITS,CDBOPEN
      CHARACTER ZITEM(14)*28 
      CHARACTER LTMP*72,h*72,inf*144,L144*144
      character cdbfile*144,lcmdfl*144,outs248*248
      character*72 DFCFD,DECMPDBFL,DMCMPDBFL,DICONDBFL
      character path*72,fs*1,scanning*8

      integer iglib, cdbunit,id   ! if 1 then X11, if 2 then GTK, if 3 then text only.

C Initialise output device, assume minimal trace, and a smallish
C display.  If passed zero size and offsets use default, if size
C is <200 then take % of default, otherwise use passed width.
C If left & top offsets are 0 then use defaults.
      call ezero
      call curmodule('cdb ')
      cAppName = 'cdb'
      IUOUT=6
      IUIN=5
      IFIL=10
      ltmp=' '
      ITFS=1
      CDBOPEN=.FALSE.
      call parpsf(MODL,iappw,iappx,iappy,inf)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs


      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=600
        iappx=115
        iappy=130
      else
        if(iappx.le.0)iappx=85
        if(iappy.le.0)iappy=110
        if(iappw.le.200)then
          iappwi=int(480*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(20*iappw*0.01),20)
        LIMIT =26
        LIMTTY=26
      else
        menuchw = 20
        LIMIT =26
        LIMTTY=26
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
     &'ESP-r Component db Manager: enquiries to esru@strath.ac.uk')

      IF(MMOD.EQ.8)THEN
        CALL win3d(menuchw,7,1,1,3,igl,igr,igt,igb,igw,igh)
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
      endif
      CALL ESPrVersion("summary",cAppName,IUOUT)
      call edisp(IUOUT,' ')

C Find the user's home folder then get users custom settings.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IFIL+1,0,IIER)

C Scan the defaults file for default configuration.
C Make temporary use of file unit IFIL+1.  Note: escdef must come
C after scan of .esprc file.
      call escdef(IFIL+1,IER)

C Take command line file name as initial file.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf(1:lnblnk(inf))
        write(outs248,'(a,a)')' the input file is: ',LCMDFL
        call edisp248(iuout,outs248,80)
      else
        LCMDFL='UNKNOWN'
        GOTO 205 
      endif

C Initialize plant component database file.
  100 h(1)='The component database file holds the'
      h(2)='description of available components. If the name'
      h(3)='given matches an existing file, it will be read in'
      h(4)='and the components data will be available for'
      h(5)='listing and editing. If there is no such file then'
      h(6)='a new one will be created to hold data for new'
      h(7)='components.'
      L144=LCMDFL
      llt=lnblnk(L144)
      
C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        if(llt.lt.96)then
          CALL EASKF(L144,'Open this database? ',' ',
     &      96,DICONDBFL,' Database name ',IER,7)
        elseif(llt.ge.96.and.llt.lt.124)then
          CALL EASKF(L144,'Open this database? ',' ',
     &      124,DICONDBFL,' Database name ',IER,7)
        elseif(llt.ge.124.and.llt.le.144)then
          CALL EASKF(L144,'Open this database? ',' ',
     &      144,DICONDBFL,' Database name ',IER,7)
        endif
      elseif(iglib.eq.2)then
        CALL EASKF(L144,'Open this database? ',' ',
     &    144,DICONDBFL,' Database name ',IER,7)
      else
        CALL EASKF(L144,'Open this database? ',' ',
     &    96,DICONDBFL,' Database name ',IER,7)
      endif

      if(L144(1:2).ne.'  ')THEN
        LCMDFL=L144
      ELSE
        GOTO 205
      ENDIF

C <<Need to check what this is doing!>>
C Find the path and local file name, if exists open otherwise fill.
      call fdroot(LCMDFL,path,LTMP)
      XST=.false.
      call findfil(LTMP,XST)
      IF(XST)then

C open and scan the existing file. 
        cdbunit=ifil
        cdbfile=LCMDFL(1:LNBLNK(LCMDFL)) 

C Read the full database into memory. 
        scanning='detailed'       
        call CDBScanDataBase(cdbunit,cdbfile,scanning,IER)
        
        IF(IER.NE.0) THEN
          CDBOPEN=.FALSE.
          dok=.false.
          h(1)='The components database file is typically held in'
          h(2)='/usr/esru/esp-r/databases/components.db1'
          h(3)=' '
          h(4)='Check that this folder exists in your ESP-r '
          h(5)='installation.'
          CALL ASKOK('cdb: Warning problem reading this database file.',
     &    'please check it is a valid database. Try Again?',OK,dok,5)
          if(OK) THEN
            GOTO 100
          endif
        ENDIF

        CDBOPEN=.TRUE.
      ELSE

C File doesn't exist.
        CDBOPEN=.FALSE.
        dok=.true.
        OK=.false.
        h(1)='The components database file is typically held in'
        h(2)='/usr/esru/esp-r/databases/components.db1'
        h(3)=' '
        h(4)='Please check that this file or the file '
        h(5)='you have specified actually exist.'
        CALL ASKOK('This file was not found.',
     &  'Try Again?',OK,dok,5)
        IF(OK) GOTO 100
      ENDIF

C Build main menu.
 205  write(zitem(1),'(a,a)') 'a Database: ',LTMP(1:16)
      zitem(2)=' Please select an option: '
      zitem(3)='  ____________________    '
      zitem(4)='c Create new database <na>'
      zitem(5)='o Open a database         '
      zitem(6)=' '
      IF(CDBOPEN)THEN
        zitem(7)='s Search the database <na>'
        zitem(8)='e Explore/edit database   '
        zitem(9)='x Export/Import data <na> '
      ELSE
        zitem(7)='                          '
        zitem(8)='  [no database opened]    '
        zitem(9)='                          '
      ENDIF
      zitem(10)='  ____________________    '
      zitem(11)='> Save                     '
      zitem(12)='! Save as ...             '           
      zitem(13)='? Help                    '
      zitem(14)='- Finish cdb              '

C Display menu.
  210 NITEMS = 14
      INO = -5

C Use askabout to instanciate the initial help messages (2nd parameter is one).
c      call askabout('pdb ',1)

  220 CALL EMENU('  ESP-r components db',ZITEM,NITEMS,INO)
      IF(INO.EQ.NITEMS)then

C Exit from program.
        call usrmsg(' Closing component dbase management...',' ','P')
        call erpfree(ifil,istat)
        CALL EPAGEND
        STOP
      elseif(INO .EQ. NITEMS-1)then

C Describe components and menu structure.
c        call askabout('cdb ',0)
      ELSEIF(INO.EQ.7)THEN
c        CALL CDBSearch(ICMP,IER)
      ELSEIF(INO.eq.8) then
        EDITS=.TRUE.
        ID=0
        IML=6
        IF(CDBOPEN)CALL CDBDomainExplore(EDITS,ID,IML,IER)      
      ELSEIF (INO .EQ. 1)THEN
        GOTO 100
      ELSEIF (INO .EQ. 4)THEN
C        CALL CDBInit
      ELSEIF (INO .EQ. 5)THEN
        CDBOPEN=.FALSE.
        GOTO 100
      ELSEIF (INO .EQ. 8)THEN
C        IF(CDBOPEN)CALL CDBDataIO()
      ELSEIF (INO .EQ. 10)THEN
C        NewName=.false.
C        CALL CDBSave(NewName)
      ELSEIF (INO .EQ. 11)THEN
C        NewName=.true.
C        CALL CDBSave(NewName)
      ELSE
        ino=-1
        goto 220
      endif
      CALL EPAGE
      GOTO 210

      END


C ***** dummy routine for c code active descriptions buttons.
      subroutine cfgpk(act)
      character act*1

      return
      end

C Dummy routines from common3dv.F
      SUBROUTINE ADJVIEW(IER)
      ier=0
      return
      end

      subroutine chgazi(icazi)
      return
      end

      subroutine chgelev(icelev)
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

      subroutine chgsun(isunhour)
      return
      end

      subroutine chgeye(EVX,EVY,EVZ,VX,VY,VZ,EAN,JITZNM,JITSNM,JITVNO,
     &   JITOBS,JITSNR,JITGRD,JITORG,DIS,JITBND,JITDSP,JITHLS,JITHLZ,
     &   JITPPSW)

C Passed paramters.
      real EVX,EVY,EVZ,VX,VY,VZ,EAN,DIS

C Depending on computer type set integer size of passed parameters.
#ifdef OSI
      integer JITZNM,JITSNM,JITVNO,JITOBS,JITSNR,JITGRD,JITORG
      integer JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW
#else
      integer*8 JITZNM,JITSNM,JITVNO,JITOBS,JITSNR,JITGRD,JITORG
      integer*8 JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW
#endif     
      return
      end

      subroutine chgzonpik(jizgfoc,jnzg)
#ifdef OSI
      integer jizgfoc,jnzg  ! for use with viewtext
#else
      integer*8 jizgfoc,jnzg  ! for use with viewtext
#endif
      return
      end

      subroutine chgzonpikarray(jnznog,jnznogv)
#ifdef OSI
      integer jnznog,jnznogv
#else
      integer*8 jnznog,jnznogv
#endif
      return
      end


