C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-2006.

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

C Project Manager: file eddb.f providing manipulation of
C system databases:
C  EDDB  : Controls specification/ browsing of databases.
C  opendb: Standard opening of thermophysical and optical databases.
C EDDBPROFILE Changes the events profile database current cfg file references.
C EDDBOPT Changes the optical database current cfg file references.
C EDDBCLM: Changes the climate database current cfg file references.

C ********** EDDB 
C Controls specification/ browsing of databases.
C ITRU is channel for user output, IER=0 means no errors encountered.

      SUBROUTINE EDDB(ITRC,ITRU,IER)
      INCLUDE 'building.h'
      INCLUDE 'net_flow.h'
      INCLUDE 'MultiYear_simulations.h'
      INCLUDE 'esprdbfile.h'
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure databases)
C LOPTDB,IOPTDB (for optical database)
C LPCDB/IPCDB (for plant template database)
C LPRFDB,IPRODB (for event profile database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
C ICLIM,LCLIM (for climate file)
C default file names for databases

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/FILEP/IFIL
      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN

      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      common/MFLDOC/DEPRE(MPRD)
      common/rpath/path
      common/user/browse
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C Default esp-r distribution path.
      common/deflt4/dinstpath

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      logical OK,DOK,XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical moddb,browse,modcon,QUIET
      logical unixok,copydef,haveconstr,clkok

      character H*72,LTMP*72,path*72,outs*124
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,linstd*60
      character cfgroot*24,LCFGF*72,fs*1,message*48
      character*72 ITEMS(15)
      character doit*248
      character DEPRE*40,OUTSTR*124,LCOPY*72
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      character dinstpath*48

C Strings for dos version of source and destination of db file.
C      character rootpdos*72,upathdos*72

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      moddb=.false.
      modcon=.false.
C Begin with menu of default dbs.
    3 INO=-4
      IER=0

      if ( bMY_climates_defined ) then
         cTemp = cMY_climate_db_name
      else
         cTemp = 'None'
      endif
      
      WRITE(ITEMS(1),'(2A)')'  path: ',path(1:62)
      ITEMS(2) ='  _______________________________ '
      WRITE(ITEMS(3),'(2A)')'a annual climate         : ',LCLIM(1:44)
      WRITE(ITEMS(4),'(2A)')'b multi-year climate     : ',cTemp(1:44)
      WRITE(ITEMS(5),'(2A)')'c pressure distributions : ',LAPRES(1:44)
      WRITE(ITEMS(6),'(2A)')'d materials              : ',LFMAT(1:44)
      WRITE(ITEMS(7),'(2A)')'e constructions          : ',LFMUL(1:44)
      WRITE(ITEMS(8),'(2A)')'f plant components       : ',LPCDB(1:44)
      WRITE(ITEMS(9),'(2A)')'g event profiles         : ',LPRFDB(1:44)
      WRITE(ITEMS(10),'(2A)')'h optical properties     : ',LOPTDB(1:44)
      WRITE(ITEMS(11),'(A)')'i mould & micotoxins     : '
      ITEMS(12)='  _______________________________ '
      ITEMS(13)='? help                            '
      ITEMS(14)='- exit this menu                  '

C Try to compact the width of the menu.
      IW=0
      do 141 ij=1,14
        if(lnblnk(ITEMS(ij)).gt.IW)IW=lnblnk(ITEMS(ij))
 141  continue

C Help text for this menu.
      H(1)='ESP-r is provided with a number of corporate (standard)'
      write(h(2),'(3a)') 'databases located in ',
     &  dinstpath(1:lnblnk(dinstpath)),'/databases'
      H(3)='databases.  For some simulation models a user may'
      H(4)='require a bespoke version of a database.  In such a'
      H(5)='case the user may either copy a corporate database'
      H(6)='into a local directory for modification or simply begin'
      H(7)='from scratch.'
      H(8)=' '
      H(9)='In most cases a bespoke database may be created by'
      H(10)='copying a corporate database into the project'
      H(11)='directory (hint: give it a name identifiable with' 
      H(12)='the project) and then modifying it to fit the needs'
      H(13)='of the project.'
      H(14)=' '
      H(15)='Initially the database names will correspond to'
      H(16)='the defaults supplied by ESRU.  If these defaults'
      H(17)='are not found, the most likely reason is that they'
      H(18)='have not been correctly installed in folders'
      H(19)='/usr/esru/esp-r/climate & /usr/esru/esp-r/databases.'

      if(MMOD.EQ.8)then
        CALL VWMENU('Databases',ITEMS,14,0,0,IW,irpx,irpy,INO)
      else
        CALL EMENU('Databases',ITEMS,14,INO)
      endif
      IF(INO.EQ.14)THEN
        if(browse)return
        if(cfgok.and.moddb)then
          dok=.true.
          h(1)='Recent changes in database file names need to be'
          h(2)='recorded in the model if the correct databases are'
          h(3)='to be accessed in the future. If you do not update'
          h(4)='the model configuration other dependencies might be'
          h(5)='left unresolved. '
          h(6)=' '
          h(7)='If you are have not quite finished work on your'
          h(8)='databases (e.g. you are creating an alternative)'
          h(9)='you might wish to delay updating the model. '
          CALL ASKOK(
     &      'A possible change in database names detected. Update the',
     &      'model database NAME LIST to match? (see help)',
     &      OK,DOK,9)
          if(.NOT.OK)return
          CALL EMKCFG('-',IER)
        endif

C If constructions db updated, check if zone construction files should
C also be updated (but not if in registration mode or there are no zones.
        if(cfgok.and.modcon)then
          if(ncomp.eq.0)then
            ok=.false.
          else

C See if there are existing zone construction files.
            haveconstr=.false.
            do 142 iiz=1,ncomp
              call FINDFIL(LTHRM(iiz),XST)
              if(XST)haveconstr=.true.
  142       continue
            if(.NOT.haveconstr)then
              ok=.false.
            else
              dok=.true.
              h(1)='Recent changes to the materials, constructions'
              h(2)='or optical databases may make the zone files '
              h(3)='out-of-date. If you want these changes to be '
              h(4)='reflected in the zone files say yes. '
              CALL ASKOK(
     &         'Rebuild existing zone construction files to reflect',
     &         'database changes? (see help)',OK,dok,4)
            endif
          endif
          if(.NOT.OK)return
          QUIET=.true.
          do 52 iz=1,ncomp
            XST=.false.
            call FINDFIL(LTHRM(iz),XST)
            if(XST)then
              CALL EDCON(0,iuout,iz,QUIET,IER)
            endif
  52      continue
          QUIET=.false.
        endif
        RETURN
      elseif(INO.EQ.3)then

C Climate database.
        call eddbclm(moddb,'    ',-1)

      ELSEIF(INO.eq.4)THEN

C Multi-year climate database. Note this currently only works in
C conjunction with save level 5.
         call usrmsg(
     &     'Note: the use of multi-year assessments is currently',
     &     'limited to save level 5 (not all versions support this).',
     &     'W')
         call MY_clm_db_menu()
        
      ELSEIF(INO.EQ.5)THEN

C Pressure distribution database...
  30    LTMP=LAPRES
        IAPRES=IFIL+1
        write(outs,'(2a)') 'Options for pressure distribution db: ',
     &    LTMP(1:lnblnk(LTMP))
        H(1)='To enable the calculation of wind-induced surface'
        H(2)='pressures (as required the simulator), this database'
        H(3)='of angle dependent pressure coefficients is provided.'
        H(4)=' '
        H(5)='Wind pressure coefficients are pressure coefficients'
        H(6)='for surfaces in typical positions and within several'
        H(7)='different exposure categories. '
        H(8)=' '
        H(9)='The following options can be selected:'
        H(10)=' '
        H(11)='a) If the file exists than you can browse the database '
        H(12)='   and if you have permission to update the file you'
        H(13)='   can edit the entities in the database.'
        H(14)=' '
        H(15)='b) select another file: you will asked for a file'
        H(16)='   name and if it is found you can browse or edit it.'
        H(17)=' '
        H(18)=' '
        H(19)='c) make a new database: a new database will be created '
        H(20)='   based on the file name you supply.'
        H(21)=' '
        H(22)='d) copy default database: a project copy of the default'
        H(23)='  database will be made and you will be offered a '
        H(24)='  chance to rename it.'
        H(25)=' '
        CALL ERPFREE(IAPRES,ISTAT)

C Get length of path to where esp-r databases were installed.
        write(linstd,'(2a)') dinstpath(1:lnblnk(dinstpath)),
     &    '/databases'
        linst=lnblnk(linstd)
        call FINDFIL(LTMP,XST)
        IF(.NOT.XST)THEN
          idno=2
          isw=0
          call MENUATOL(outs,'Pressure distributions options:',
     &      'a browse/edit (NOT APPLICABLE)','b select another file',
     &      'c make a new database','d copy the default database',
     &      ' ',' ',' ',' ',' ',' ',' ',' ',isw,idno,25)
          copydef=.true.
        elseif(LTMP(1:linst).eq.linstd(1:linst))then

C If corportate database then offer the following choices:
          idno=1
          isw=0
          call MENUATOL(outs,'Pressure distributions options:',
     &      'a browse or edit this file','b select another file',
     &      'c make a new database','d make project copy of database',
     &      ' ',' ',' ',' ',' ',' ',' ',' ',isw,idno,25)
          copydef=.false.
        else

C If db exists locally offer the following choices:
          idno=1
          isw=0
          call MENUATOL(outs,'Pressure distributions options:',
     &      'a browse or edit this file','b select another file',
     &      'c make a new database','d copy the default database',
     &      ' ',' ',' ',' ',' ',' ',' ',' ',isw,idno,25)
          copydef=.true.
        endif

C Act on the users choice. If no choice return to main menu.
        if(isw.eq.0)then
          goto 3
        elseif(isw.eq.1)then

C If file exists open it, test file type, get number of items and close.
          if(XST)then
            LAPRES=LTMP
            moddb=.true.
            CALL EFOPSEQ(IAPRES,LAPRES,1,IER)
            IF(IER.NE.0) GOTO 30  
            CALL STRIPC(IAPRES,OUTSTR,0,ND,1,'pressure db',IER)
            K=0
            CALL EGETWI(OUTSTR,K,NPRE,1,MPRD,'W',
     &        'no pressure item',IER)
            IF(NPRE.LT.1.OR.NPRE.GT.MPRD) THEN
              CALL USRMSG(' The specified file may not be a',
     &                    ' pressure coef db, please try again','W')
              CALL ERPFREE(IAPRES,ISTAT)
              GOTO 30
            ENDIF
            CALL ERPFREE(IAPRES,ISTAT)

C Open pressure file, read in data, if OK, display coefficients and
C enter editing facilitiy.
            CALL ERPRCDB(LAPRES,0,3,IER)
            IF(IER.NE.0) GOTO 30
            CALL EDPCDB(IER) 
          else
            call usrmsg('Sorry there is no file to browse/edit.',
     &                  'Please use one of the other options. ','W')
            goto 30
          endif
        elseif(isw.eq.2)then
 31       H(1)='To enable the calculation of wind-induced surface'
          H(2)='pressures (as required the simulator), this database'
          H(3)='of angle dependent pressure coefficients is provided.'
          H(4)=' '
          H(5)='Wind pressure coefficients are pressure coefficients'
          H(6)='for surfaces in typical positions and within several'
          H(7)='different exposure categories. '
          H(8)=' '
          H(9)='Please supply and confirm the file name.'
          CALL EASKS(LTMP,' Pressure coefficients database?',
     &      ' ',72,DAPRES,'pressure coefficients database',IER,9)

C Check if name ok and if a file exists by that name.
          if(LTMP(1:2).NE.'  ')then
            LAPRES=LTMP
          else
            goto 31
          endif
          CALL ERPFREE(IAPRES,ISTAT)
          call FINDFIL(LAPRES,XST)
          IF(XST)THEN

C If file exists scan the file and use the editing facility.
            write(outs,'(2a)') 'Accessing pressure db ',
     &        LAPRES(1:lnblnk(LAPRES))
            call edisp(iuout,outs)
            CALL ERPRCDB(LAPRES,0,3,IER)
            IF(IER.NE.0) GOTO 30
            CALL EDPCDB(IER) 
            moddb=.true.
          else
            write(outs,'(2a)') LAPRES(1:lnblnk(LAPRES)),
     &        ' does not appear to exist.'
            call edisp(iuout,outs)
            goto 30
          endif
        elseif(isw.eq.3)then

C No pressure distribution db found so set-up minimal common block, create
C a new db, populate it and present editing facility.
          h(1)='The suggested name of this database is based on the '
          h(2)='project name. '
          lr=lnblnk(cfgroot)
          if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
            write(LTMP,'(2a)') cfgroot(1:lr),'.pressuredb'
          else
            write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &        cfgroot(1:lr),'.pressuredb'
          endif
          clkok=.false.
 32       CALL EASKSCMD(LTMP,' New pressure coefficients database?',' ',
     &      'cancel',clkok,72,DAPRES,'pressure coefficients database',
     &      IER,2)
          if(clkok) goto 3
          if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
            LAPRES=LTMP
          else
            goto 32
          endif

          moddb=.true.
          NPRE=1
          DEPRE(NPRE)='undefined pc set'
          do 33 ij=1,16
            FPRE(ij,1)=0.00
  33      continue 

C Update the file and open editing facilty. 
          CALL EMKAPCDB(LAPRES,IER)
          IF(IER.NE.0) GOTO 30
          CALL ERPRCDB(LAPRES,0,3,IER)
          IF(IER.NE.0) GOTO 30
          CALL EDPCDB(IER) 
        elseif(isw.eq.4)then

C Copy the standard db to project folder. Suggest a local
C file name based on the project root name. Lastly do the copy.
C If the default is to be copied ask for local name. Use LTMP for
C the file name as known to esp-r and LCOPY for the system cp call.   
          if(copydef)then
            call edisp(iuout,'The source file is:')
            call edisp(iuout,DAPRES)
          else
            call edisp(iuout,'The source file is:')
            call edisp(iuout,LAPRES)
          endif
          h(1)='The suggested name of this database is based on the '
          h(2)='project name. '
          lr=lnblnk(cfgroot)
          if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
            write(LTMP,'(2a)') cfgroot(1:lr),'.pressuredb'
          else
            write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &        cfgroot(1:lr),'.pressuredb'
          endif
  34      CALL EASKS(LTMP,
     &      'Database name after it is copied into the project?',
     &      ' ',72,'pressure.db','copied pressure database',IER,2)
          call isunix(unixok)
          IF(LTMP(1:2).NE.'  ')then
            write(LCOPY,'(3a)') path(1:lnblnk(path)),fs,
     &        LTMP(1:lnblnk(LTMP))
            if(copydef)then
              doit = ' '
              if(unixok)then
                write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cp ',
     &            DAPRES(1:lnblnk(DAPRES)),' ',LCOPY(1:lnblnk(LCOPY))
              else

C Debug statements for native windows version. If lnblankrp or
C lnblankup is one then there are no spaces in the string so double quotes
C are not required.  But in either case it is useful to change any '/'
C in rootp and upath to '\'. 
                message='getting pressure coef file from '
                call dbdoscopy(DAPRES,lcopy,message,doit,ier)
              endif
              call usrmsg('copying default file via',doit,'-')
              call runit(doit,'-')
              LAPRES=LTMP
            else
              doit = ' '
              if(unixok)then
                write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cp ',
     &            LAPRES(1:lnblnk(LAPRES)),' ',LCOPY(1:lnblnk(LCOPY))
              else

C Similar logic but using LAPRES.
                message='getting pressure coef file from '
                call dbdoscopy(LAPRES,lcopy,message,doit,ier)
              endif
              call usrmsg('copying file via',doit,'-')
              call runit(doit,'-')
              LAPRES=LTMP
            endif
            moddb=.true.
            write(outs,'(2a)') 'Accessing pressure distribution db: ',
     &        LAPRES(1:lnblnk(LAPRES))
            call edisp(iuout,outs)
            CALL ERPRCDB(LAPRES,0,3,IER)
            IF(IER.NE.0) GOTO 30
            CALL EDPCDB(IER) 
          else
            goto 34
          endif
        endif
      ELSEIF(INO.EQ.6)THEN

C Since it may be repeatedly accessed, open materials db.
        call eddbmat(moddb,modcon,'    ')
        if(moddb)then
          call usrmsg(
     &      'The database name might have changed. Suggest',
     &      'using the `save model` option to record this.','W')
        endif
        if(modcon)then
          call usrmsg(
     &      'The database details might have changed. In a moment',
     &      'you will be offered the option to update zone files.','W')
        endif
      ELSEIF(INO.EQ.7)THEN

C Deal with construction databases.
        call eddbmlc(moddb,modcon,'    ')
        if(modcon)then
          call usrmsg(
     &      'The database details might have changed. In a moment',
     &      'you will be offered the option to update zone files.','W')
        endif
      ELSEIF(INO.EQ.8)THEN

C Plant template database.
        call eddbpcdb(moddb,'    ')
        if(moddb)then
          call edisp(iuout,
     &      'The database name or details have changed. If you want')
          call edisp(iuout,
     &      'to keep it use the `save model` option.')
        endif
      ELSEIF(INO.EQ.9)THEN

C Profiles database.
        call eddbprofile(moddb,'    ')
        if(moddb)then
          call edisp(iuout,
     &      'The database name or details have changed. If you want')
          call edisp(iuout,
     &      'to keep it use the `save model` option.')
        endif
      ELSEIF(INO.EQ.10)THEN

C Optical database.
        call eddbopt(moddb,'    ')
        if(moddb)then
          call edisp(iuout,
     &      'The database name or details have changed. If you want')
          call edisp(iuout,
     &      'to keep it use the `save model` option.')
        endif
      ELSEIF(INO.EQ.11)THEN

C Place holder for mould database
         call UsrMsg(
     &   'the project manager does not currently start the mould',
     &   'databases directly (invoke mld manually).','W')
      ELSEIF(INO.EQ.13)THEN

C Menu help text.
        H(1)='Information on each of the databases is provided. '
        call easkatog('Help options:',' ','overview','climate',
     &   'materials','constructions','optics','plant','continue',ihd,1)
        if(ihd.eq.1)then
          H(1)='ESP-r is provided with a number of corporate (standard)'
          H(2)='databases located in the directory /usr/esru/esp-r/'
          H(3)='databases.  For some simulation models a user may'
          H(4)='require a bespoke version of a database.  In such a'
          H(5)='case the user may either copy a corporate database'
          H(6)='into a local directory for modification or simply begin'
          H(7)='from scratch.'
          H(8)=' '
          H(9)='In most cases a bespoke database may be created by'
          H(10)='copying a corporate database into the project'
          H(11)='directory (hint: give it a name identifiable with' 
          H(12)='the project) and then modifying it to fit the needs'
          H(13)='of the project.'
          H(14)=' '
          H(15)='Initially the database names will correspond to'
          H(16)='the defaults supplied by ESRU.  If these defaults'
          H(17)='are not found, the most likely reason is that they'
          H(18)='have not been correctly installed in folders'
          H(19)='/usr/esru/esp-r/climate & /usr/esru/esp-r/databases.'
          CALL PHELPD('databases overview',19,'-',0,0,IER)
        elseif(ihd.eq.2)then
          H(1)='Climate databases are hourly sets of values from '
          H(2)='1h00 on 1 Jan to 24h00 on 31 December.  For each hour' 
          H(3)=' the following data is held:'
          H(4)=' '
          H(5)=' Diffuse horizontal solar intensity (W/m^2)'
          H(6)=' Dry bulb temp. (tenths of degree C ie. 102 = 10.2C)'
          H(7)=' Direct normal or global horiz. solar intensity (W/m^2)'
          H(8)=' Wind speed (tenths m/s ie. 15 = 1.5 m/s)'
          H(9)=' Wind direction (degrees from north, clockwise)'
          H(10)=' Relative humidity (percent)'
          CALL PHELPD('climate overview',10,'clm_dbitems ',0,0,IER)
        elseif(ihd.eq.3)then
          H(1)='The materials database holds the density, specific' 
          H(2)='heat, conductivity, diffusion resistance and surface'
          H(3)='absorption and emissivity of elements such as earth,' 
          H(4)='stone, steel, oak etc.  This database is normally in' 
          H(5)='a binaryformat for quick random access although an'
          H(6)='ASCII form is available fortransfer between machine' 
          H(7)='types.'
          CALL PHELPD('materials db overview',7,'materials_db',0,0,IER)
        elseif(ihd.eq.4)then
          H(1)='A multilayer construction database defines the com-'
          H(2)='position of constructions such as walls and glazing'
          H(3)='systems in terms of layers of materials of particular'
          H(4)='thickness in a particular order and, where appropriate,' 
          H(5)='with a named set of optical properties. '
          CALL PHELPD('constr db overview',5,'mlc_database',0,0,IER)
        elseif(ihd.eq.5)then
          H(1)='The optical properties of multilayer constructions'
          H(2)='are defined in an optical database.  The data include'
          H(3)='angular (0, 40, 55, 70, 80 degrees from normal) solar' 
          H(4)='transmission and absorption at each layer.'
          CALL PHELPD('optics overview',4,'optical_db  ',0,0,IER)
        elseif(ihd.eq.6)then
          H(1)='The plant components database is a binary, random '
          H(2)='access file holding plant component descriptions to '
          H(3)='enable the establishment of component state-space '
          H(4)=' representations used by ESP-r.'
          CALL PHELPD('plant db overview',4,'plant_db_fld',0,0,IER)
        endif
      ELSE
        INO=-4
        GOTO 3
      ENDIF
      call usrmsg(' ',' ','-')
      INO=-4
      GOTO 3

      END

C ********** opendb
C Open materials constructions, multi-layer constructions and
C optical properties databases.

      subroutine opendb(ier)
      INCLUDE 'esprdbfile.h'

      common/FILEP/IFIL
      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      logical    XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      character H*72,LASCI*72,DFILE*72
      character SOPT*12,outs*124,GDESCR*36

      IER=0

C Open currently named materials database.  Note that
C by a minor rewrite of ICLN in rec 601 we can get by with changing
C this to 24 words wide.
      CALL ERPFREE(IFMAT,ISTAT)
      call EFOPRAN(IFMAT,LFMAT,40,0,IER)
      IF(ier.ne.0)THEN
        write(outs,'(3a)') ' No materials db ',
     &     LFMAT(1:lnblnk(LFMAT)),' found!'
        call edisp(iuout,outs)
        MATDBOK=.FALSE.
      else
        MATDBOK=.TRUE.
      endif
        
C Read multilayer db information into common.
      CALL ERPFREE(IFMUL,ISTAT)
      call FINDFIL(LFMUL,XST)
      if(XST)then
        CALL ERMLDB(0,IUOUT,IER)
        IF(IER.eq.1)then
          write(outs,'(3a)') 
     &    ' A few problems with materials used by constructions db ',
     &     LFMAT(1:lnblnk(LFMAT)),' !'
          call edisp(iuout,outs)
          MLDBOK=.FALSE.
        ELSEIF(IER.eq.2)then

C There were lots of undefined materials so likely the materials
C database is for the wrong machine type. Guess the name of
C the ascii version and if it exists, offer to convert it.
          h(1)='It is likely that the materials db associated'
          h(2)='with this model is for a different computer '
          h(3)='type. ESP-r will try to find an ASCII version '
          h(4)='of this file and convert it if you agree. '
          IAF=IFIL+1
          LASCI=' '
          write(LASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
          CALL EFOPSEQ(IAF,LASCI,1,IER)
          if(ier.eq.0)then
            CALL ERPFREE(IAF,ISTAT)
            call edisp(iuout,
     &        'Attempting to convert an ASCII materials db into a')
            call edisp(iuout,
     &        'binary materials db that matches the constructions db.')
            CALL MKBPRC(IUOUT,IAF,LASCI,IER)
            if(ier.eq.0)then
              call usrmsg(
     &          'An ASCII materials file was found and has been',
     &          'converted re-scanning constructions...',
     &          'P')
              CALL ERMLDB(0,IUOUT,IER)
              if(ier.eq.0)then
                MLDBOK=.TRUE.
                call usrmsg(
     &            'An ASCII materials file was found and has been',
     &            'converted re-scanning constructions...done.',
     &            '-')
              else
                MLDBOK=.FALSE.
                call usrmsg(
     &            'An ASCII materials file was found and has been',
     &            'converted re-scanning constructions...failed.',
     &            'W')
              endif
            else
              call usrmsg(
     &        'An materials file was not found or was not readable and',
     &        'you should correct this before using constructions.',
     &           'W')
              MLDBOK=.FALSE.
            endif
          else

C Ask user for ascii materials database to convert.
          H(1)='The materials database holds the thermo-physical'
          H(2)='properties of a range of materials such as'
          H(3)='brick, concrete, insulation, etc. An asci version'
          H(4)='of this database is ofen used when transporting models'
          H(5)='between operating systems. '
            IAF=IFIL+1
            DFILE=' '
            CALL EASKS(LASCI,' Materials database (ASCII)?',
     &        ' ',72,DFILE,'materials db (ascii)',
     &        IER,5)
            CALL MKBPRC(IUOUT,IAF,LASCI,IER)
            if(ier.eq.0)then
              call usrmsg(
     &          'An ASCII materials file was found and has been',
     &          'converted attempting re-scan the constructions',
     &          'W')
              CALL ERMLDB(0,IUOUT,IER)
              if(ier.eq.0)then
                MLDBOK=.TRUE.
              else
                MLDBOK=.FALSE.
              endif
            else
              call usrmsg(
     &        'An materials file was not found or was not readable and',
     &        'you should correct this before using constructions.',
     &          'W')
              MLDBOK=.FALSE.
            endif
          endif
        else

C Scan was ok so set mldbok to true.
          MLDBOK=.TRUE.
        endif
      else

C Could not find constructions db (at this time), report to user.
        write(outs,'(3a)') ' No constructions db ',
     &     LFMUL(1:lnblnk(LFMUL)),' found!'
        call edisp(iuout,outs)
        MLDBOK=.FALSE.
      endif

C Open optical database and read into common.
      SOPT='ALL'
      CALL EROPTDB(0,iuout,SOPT,GDESCR,IER)
      if(ier.ne.0)then
        call usrmsg(' Optical properties db not found or there was a',
     &  ' problem reading it!','W')
        OPTKOK=.FALSE.
      else
        OPTKOK=.TRUE.
      endif

C Open the pressure coef db (needed for network flow descriptions).
      call FINDFIL(LAPRES,XST)
      if(XST)then
        CALL ERPRCDB(LAPRES,0,3,IER)
        call usrmsg(' ',' ','-')
      else
        call usrmsg(' Wind pressure db not found or there was a',
     &  ' problem reading it!','W')
      endif

      return
      end


C ********** EDDBMAT
C EDDBMAT Changes the materials database current cfg file references.
C moddb is from version manager
C modcon signals change in constructions.
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.

      SUBROUTINE EDDBMAT(moddb,modcon,APP)
      INCLUDE 'esprdbfile.h'

      common/FILEP/IFIL
      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN

      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C Default esp-r distribution path.
      common/deflt4/dinstpath

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      logical XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical moddb,modcon,unixok,copydef,clkok,chgdb

      character H*72,LTMP*72,path*72,outs*124
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,linstd*60
      character cfgroot*24,LCFGF*72,fs*1,message*48
      character doit*248,LCOPY*72,dinstpath*48,APP*4
      character DFILE*72,LASCI*72,LTMPFL*72

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      moddb=.false.
      modcon=.false.

 138  LTMP=LFMAT
      write(outs,'(2a)') 'Options for materials db: ',
     &  LTMP(1:lnblnk(LTMP))

C If db does not exist offer the following choices:
      H(1)='The materials database holds the density, specific' 
      H(2)='heat, conductivity, diffusion resistance and surface'
      H(3)='absorption and emissivity of elements such as earth,' 
      H(4)='stone, steel, oak etc.  This database is normally in' 
      H(5)='a binaryformat for quick random access although an'
      H(6)='ASCII form is available fortransfer between machine' 
      H(7)='types.'
      H(8)=' '
      H(9)='The following options can be selected:'
      H(10)=' '
      H(11)='a) If the file exists than you can browse the database '
      H(12)='   and if you have permission to update the file you'
      H(13)='   can edit the entities in the database.'
      H(14)=' '
      H(15)='b) select another file: you will asked for a file'
      H(16)='   name and if it is found you can browse or edit it.'
      H(17)=' '
      H(18)='c) make a new database: a new database will be created '
      H(19)='   based on the file name you supply.'
      H(20)=' '
      H(21)='d) copy default database: a project copy of the default'
      H(22)='  database will be made and you will be offered a '
      H(23)='  chance to rename it.'
      H(24)=' '
      H(25)='e) export material db (a binary file which is'
      H(26)='   platform dependant to an ascii file) '
      H(27)=' '
      H(28)='f) import legacy ascii material db file and convert it'
      H(29)='   to a binary file. '
      H(30)=' '
      H(31)='g) import ascii material db file into common blocks,view'
      H(32)='   it and write out to ascii file (BETA facility). '
      CALL ERPFREE(IFMAT,ISTAT)

C Get length of path to where esp-r databases were installed.
      write(linstd,'(3a)') dinstpath(1:lnblnk(dinstpath)),
     &  fs,'databases'
      linst=lnblnk(linstd)
      call FINDFIL(LFMAT,XST)
      IF(.NOT.XST)THEN
        idno=2
        isw=0
        call MENUATOL(outs,'Materials options:',
     &    'a browse/edit (NOT APPLICABLE)','b select another file',
     &    'c make a new database','d copy the default database',
     &    'e binary >> ascii export','f ascii >> binary import',
     &    'g ascii >> common (BETA)',' ',' ',' ',' ',' ',isw,idno,32)
          copydef=.true.
      elseif(LFMAT(1:linst).eq.linstd(1:linst))then

C If corportate database then offer the following choices:
        idno=1
        isw=0
        call MENUATOL(outs,'Materials options:',
     &    'a browse or edit this file','b select another file',
     &    'c make a new database','d make project copy of database',
     &    'e binary >> ascii export','f ascii >> binary import',
     &    'g ascii >> common (BETA)',' ',' ',' ',' ',' ',isw,idno,32)
          copydef=.false.
      else

C If db exists locally offer the following choices:
        idno=1
        isw=0
        call MENUATOL(outs,'Materials options:',
     &    'a browse or edit this file','b select another file',
     &    'c make a new database','d copy the default database',
     &    'e binary >> ascii export','f ascii >> binary import',
     &    'g ascii >> common (BETA)',' ',' ',' ',' ',' ',isw,idno,32)
          copydef=.true.
      endif

C Act on the users choice. If no choice return to main menu.
      if(isw.eq.0)then
        return
      elseif(isw.eq.1)then
        if(XST)then
          IER=0
          call EFOPRAN(IFMAT,LTMP,40,0,IER)
          if(ier.ne.0)then
            call usrmsg(' The materials database was not found or ',
     &                  ' was corrupt','W')
            goto 138
          endif
          MATDBOK=.TRUE.
          moddb=.true.
          modcon=.true.
          LFMAT=LTMP
          call tstamp('>','PRJ: enter materials db')
          CALL ELISTC(iw,chgdb,'M',IER)
          if(ier.eq.1)then

C The materials db was empty...so.
            goto 138
          elseif(ier.eq.2)then

C The materials db did not scan. Could be wrong machine type.
            call usrmsg(
     &        ' The materials database probably needs to be',
     &        ' restored from archival ASCII version.','W')
            goto 138
          endif
          if(chgdb)then

C If the materials db was edited write a backup ASCII version.
            H(1)='The materials db is a binary file. If this gets'
            H(2)='corrupted it is good to have a backup to restore'
            H(3)='from. Ant ASCII version is being written.'
            DFILE='./newmat.db1.a'
            clkok=.false.
            LASCI=' '
            write(LASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
            CALL EASKSCMD(LASCI,' Materials database (ASCII backup)?',
     &        ' ','cancel',clkok,72,DFILE,'materials database (ascii)',
     &       IER,3)
            if(clkok)then
              continue
            else
              IAF=IFIL+1
              CALL MKAPRC(IUOUT,LASCI,IAF,IER)
            endif
          endif
          modcon=chgdb
        else
          call usrmsg('Sorry there is no file to browse/edit.',
     &                'Please use one of the other options. ','W')
          goto 138
        endif
      elseif(isw.eq.2)then
        h(1)='Please supply and confirm material database file'
        h(2)='name. If it exists you will be able to browse it. If '
        h(3)='you want this to be a new database the next task is to'
        h(4)='select the `make new database` option. '
 144    CALL EASKS(LTMP,' Materials database?',
     &    ' ',72,DFCON,'materials database',IER,4)
        IF(LTMP(1:2).NE.'  ')then
          LFMAT=LTMP
        else
          goto 144
        endif
        CALL ERPFREE(IFMAT,ISTAT)
        call FINDFIL(LFMAT,XST)
        if(XST)then

C If file exists and user wants to browse then scan the file and
C use the editing facility.
          write(outs,'(2a)') 'Accessing materials db ',
     &      LFMAT(1:lnblnk(LFMAT))
          call edisp(iuout,outs)
          IER=0
          call EFOPRAN(IFMAT,LFMAT,40,0,IER)
          if(ier.ne.0)then
            call usrmsg(' The materials database was not found or ',
     &                  ' was corrupt','W')
            goto 138
          endif
          call tstamp('>','PRJ: enter materials db')
          CALL ELISTC(iw,chgdb,'M',IER)
          if(ier.eq.1)then

C The materials db was empty...so.
            goto 138
          elseif(ier.eq.2)then

C The materials db did not scan. Could be wrong machine type.
            call usrmsg(
     &        ' The materials database probably needs to be',
     &        ' restored from archival ASCII version.','W')
            goto 138
          endif
          if(chgdb)then

C If the materials db was edited write a backup ASCII version.
            H(1)='The materials db is a binary file. If this gets'
            H(2)='corrupted it is good to have a backup to restore'
            H(3)='from. Ant ASCII version is being written.'
            DFILE='./newmat.db1.a'
            clkok=.false.
            LASCI=' '
            write(LASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
            CALL EASKSCMD(LASCI,' Materials database (ASCII backup)?',
     &        ' ','cancel',clkok,72,DFILE,'materials database (ascii)',
     &        IER,3)
            if(clkok)then
              continue
            else
              IAF=IFIL+1
              CALL MKAPRC(IUOUT,LASCI,IAF,IER)
            endif
          endif
          modcon=chgdb
        else
          write(outs,'(2a)') LFMAT(1:lnblnk(LFMAT)),
     &      ' does not appear to exist.'
          call edisp(iuout,outs)
          goto 138
        endif
      elseif(isw.eq.3)then

C No materials db found so set-up minimal common block, create
C a new db, populate it and present editing facility.
        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.materialdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.materialdb'
        endif
        clkok=.false.
  145   CALL EASKSCMD(LTMP,' New material database?',
     &    ' ','cancel',clkok,72,DFCON,'new material database',IER,2)
        if(clkok) return
        IF(LTMP(1:2).NE.'  ')then
          LFMAT=LTMP
        else
          goto 145
        endif
        CALL ERPFREE(IFMAT,ISTAT)
        call EFOPRAN(IFMAT,LFMAT,40,3,IER)
        if(ier.ne.0)then
          call usrmsg('This materials database could not be created',
     &                '(check file permissions).','W')
          goto 138
        endif
        MATDBOK=.TRUE.
        moddb=.true.
        modcon=.true.

C << need to update this to offer choice of legacy or current
C << ascii materials file.

C Write a scratch file which can be read in to create a binary material.
C Include typical categories with one item in each.
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMPFL,'(2a)') cfgroot(1:lr),'.materialdb.a'
        else
          write(LTMPFL,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.materialdb.a'
        endif
        IAF=IFIL+1
        CALL EFOPSEQ(IAF,LTMPFL,3,IER)
        write(IAF,'(2a)',IOSTAT=ios,ERR=13)
     &    '# materials db defined in ',LFMAT(1:lnblnk(LFMAT))
        NCLASS=15
        write(IAF,'(i5,a)',IOSTAT=ios,ERR=13) NCLASS,
     &    '#  number of classifications'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '# class|no elements|description'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) '  1   1   Brick'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) '  1   Paviour Brick'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '  0.960, 2000.000, 840.000, 0.930, 0.700, 12.000'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '# class|no elements|description'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) '  2   1   Concrete'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 21   Light mix conc'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '  0.380, 1200.000, 653.000, 0.900, 0.650, 6.000'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '# class|no elements|description'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) '  3   1   Metal'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 41   Copper   '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '  200.000, 8900.000, 418.000, 0.720, 0.650, 19200.000'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '# class|no elements|description'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) '  4   1   Wood'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 61   Wood block   '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '  0.160, 800.000, 2093.000, 0.900, 0.650, 11.000'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '# class|no elements|description'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) '  5   1   Stone'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 81   Sandstone   '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '  1.830, 2200.000, 712.000, 0.900, 0.600, 29.000'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '# class|no elements|description'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) '  6   1   Plaster'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 101  Dense plaster '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '  0.500, 1300.000, 1000.000, 0.910, 0.500, 11.000  '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '# class|no elements|description'
        write(IAF,'(a)',IOSTAT=ios,ERR=13)
     &    '  7   1   Screeds and renders'
        write(IAF,'(a)',IOSTAT=ios,ERR=13)
     &    ' 121  Ltwt concrete screed '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '  0.410, 1200.000, 840.000, 0.900, 0.800, 30.000'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '# class|no elements|description'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) '  8   1   Tiles'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 141  Clay tile '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    ' 0.850, 1900.000, 837.000, 0.900, 0.600, 52.000'  
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '# class|no elements|description'
        write(IAF,'(a)',IOSTAT=ios,ERR=13)
     &    '  9   1   Asphalt and bitumen'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 161  Bitumen felt '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    ' 0.500, 1700.000, 1000.000, 0.900, 0.900, 1000.000'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '# class|no elements|description'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) '  10   1  Fabric '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 181  wool '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '  0.060, 198.000, 1360.000, 0.900, 0.600, 10.000'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '# class|no elements|description'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) '  11   1  Insulation '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 201  Fibreboard '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '  0.060, 300.000, 1000.000, 0.900, 0.500, 13.000'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '# class|no elements|description'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) '  12   1  Carpet '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 221  Wilton '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    ' 0.060, 186.000, 1360.000, 0.900, 0.600, 10.000' 
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '# class|no elements|description'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) '  13   1  Glass '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 241  Glass block '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '  0.700, 3500.000, 837.000, 0.830, 0.050, 19200.000'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '# class|no elements|description'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) '  14   1  Earth '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 261  Infusorial (9% mc)'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    ' 0.090, 480.000, 180.000, 0.900, 0.850, 5.000 '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '# class|no elements|description'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) '  15   1  Plastic '
        write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 281   Plastic tile'
        write(IAF,'(a)',IOSTAT=ios,ERR=13) 
     &    '  0.500, 1050.000, 837.000, 0.900, 0.400, 1000.000 '
        CALL ERPFREE(IAF,ISTAT)
        call pausems(400)
        CALL MKBPRC(IUOUT,IAF,LTMPFL,IER)
      elseif(isw.eq.4)then

C Copy the standard db to project folder. Suggest a local
C file name based on the project root name. Lastly do the copy.

C If the default is to be copied ask for local name. Use LTMP for
C the file name as known to esp-r and LCOPY for the system cp call.   
        if(copydef)then
          call edisp(iuout,'The source file is:')
          call edisp(iuout,DFCON)
        else
          call edisp(iuout,'The source file is:')
          call edisp(iuout,LFMAT)
        endif
        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.materialdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.materialdb'
        endif
  146   CALL EASKS(LTMP,
     &    'Database name after it is copied into the project?',
     &    ' ',72,'materials.db','copied materials database',IER,2)
        call isunix(unixok)
        IF(LTMP(1:2).NE.'  ')then
          write(LCOPY,'(3a)') path(1:lnblnk(path)),fs,
     &      LTMP(1:lnblnk(LTMP))
          if(copydef)then
            doit = ' '
            if(unixok)then
              write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &          DFCON(1:lnblnk(DFCON)),' ',LCOPY(1:lnblnk(LCOPY))
            else

C Use similar logic as DAPRES to setup for copy command.
              message='getting materials db file from '
              call dbdoscopy(DFCON,lcopy,message,doit,ier)
            endif
            call lusrmsg('copying default file via',doit,'-')
            call runit(doit,'-')
            LFMAT=LTMP
          else
            doit = ' '
            if(unixok)then
              write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &          LFMAT(1:lnblnk(LFMAT)),' ',LCOPY(1:lnblnk(LCOPY))
            else

C Similar logic but using LFMAT.
              message='getting materials db file from '
              call dbdoscopy(LFMAT,lcopy,message,doit,ier)
            endif
            call lusrmsg('copying file via',doit,'-')
            call runit(doit,'-')
            LFMAT=LTMP
          endif
          write(outs,'(2a)') 'Accessing materials db: ',
     &      LFMAT(1:lnblnk(LFMAT))
          call edisp(iuout,outs)
          IER=0
          CALL ERPFREE(IFMAT,ISTAT)
          call EFOPRAN(IFMAT,LFMAT,40,0,IER)
          if(ier.ne.0)then
            call usrmsg(
     &        ' The materials database was not found or was corrupt',
     &        ' (e.g. version for different computer).','W')
            goto 138
          endif
          call tstamp('>','PRJ: enter materials db')
          CALL ELISTC(iw,chgdb,'M',IER)
          if(ier.eq.1)then

C The materials db was empty...so.
            goto 138
          elseif(ier.eq.2)then

C The materials db did not scan. Could be wrong machine type.
            call usrmsg(
     &        ' The materials database probably needs to be',
     &        ' restored from archival ASCII version.','W')
            goto 138
          endif
          modcon=chgdb

C On return from display and/or edit of materials create a back-up
C ascii version of this new project materials db.
          H(1)='Some databases are held as BINARY files for speed'
          H(2)='of access and security. It is sometimes convenient'
          H(3)='to convert to ASCII - e.g. for implementation under'
          H(4)='a different operating system or for word processing.'
          H(5)=' '
          H(6)='Since you have just requested a project copy an '
          H(7)='ASCII backup is also being made. '
          DFILE='./newmat.db1.a'
          clkok=.false.
          LASCI=' '
          write(LASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
          CALL EASKSCMD(LASCI,' Materials database (ASCII backup)?',
     &     '  ','cancel',clkok,72,DFILE,'materials database (ascii)',
     &    IER,7)
          if(clkok)then
            return
          else
            IAF=IFIL+1
            CALL MKAPRC(IUOUT,LASCI,IAF,IER)
          endif
        else
          goto 146
        endif
      elseif(isw.eq.5)then

C Materials db: BINARY > ASCII. Open the ascii file on IAF=IFIL+1.

C << offer option to write to either format of ascii file. >>

        IAF=IFIL+1
        IF(.NOT.MATDBOK)THEN

C If materials not opened do it now.
          IER=0
          call EFOPRAN(IFMAT,LFMAT,40,1,IER)
          if(ier.ne.0)return 
          MATDBOK=.TRUE.
        ENDIF

C Confirm suggested name and then write out asci file.
        H(1)='Some databases are held as BINARY files for speed'
        H(2)='of access and security. It is sometimes convenient'
        H(3)='to convert to ASCII - e.g. for implementation under'
        H(4)='a different operating system or for word processing. '
        DFILE='./newmat.db1.a'
        clkok=.false.
        LASCI=' '
        write(LASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
        CALL EASKSCMD(LASCI,' Materials database (ASCII)?',
     &    '  ','cancel',clkok,72,DFILE,'materials database (ascii)',
     &    IER,4)
        if(clkok)then
          return
        else
          CALL MKAPRC(IUOUT,LASCI,IAF,IER)
        endif
      elseif(isw.eq.6)then

C Materials db: ASCII > BINARY. Guess that the ascii version has
C the same name as the binary one and is in the same location.
        H(1)='The materials database holds the thermo-physical'
        H(2)='properties of a range of materials such as'
        H(3)='brick, concrete, insulation, etc. An asci version'
        H(4)='of this database is ofen used when transporting models'
        H(5)='between operating systems. '
        IAF=IFIL+1
        clkok=.false.
        DFILE=' '
        LASCI=' '
        write(LASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
        CALL EASKSCMD(LASCI,' Materials database (ASCII)?',
     &    '  ','cancel',clkok,72,DFILE,'materials db (ascii)',IER,5)
        if(clkok) return

        DFILE='./newprim.db1'
        clkok=.false.
        ltmp=LFMAT
 143    CALL EASKSCMD(ltmp,' Materials database (BINARY)?',
     &    '  ','cancel',clkok,72,DFILE,'Materials db (binary)',IER,5)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          LFMAT=ltmp
        else
          goto 143
        endif
        if(clkok)then
          return
        else
          CALL MKBPRC(IUOUT,IAF,LASCI,IER)
        endif
      elseif(isw.eq.7)then

C Legacy materials to common blocks.
        H(1)='The materials database holds the thermo-physical'
        H(2)='properties of a range of materials such as'
        H(3)='brick, concrete, insulation, etc. An asci version'
        H(4)='of this database is ofen used when transporting models'
        H(5)='between operating systems. '
        IAF=IFIL+1
        clkok=.false.
        DFILE=' '
        LASCI=' '
        write(LASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
        CALL EASKSCMD(LASCI,' Materials database (ASCII)?',
     &    '  ','cancel',clkok,72,DFILE,'materials db (ascii)',IER,5)
        if(clkok) return

C Attempt to scan version 1.1 ASCII file, if ier returned as -2 then
C re-read as a legacy materials file.
        call rascimat(iuout,IAF,LASCI,IER)
        if(ier.eq.-2)then
          ier=0
          call rlegacymat(iuout,IAF,LASCI,ier)
        endif

C Offer display of material common blocks.
        CALL ELISTMAT(iw,chgdb,'M',IER)

        DFILE='./newmat.db1a'
        clkok=.false.
        write(ltmp,'(2a)') LASCI(1:lnblnk(LASCI)),'a1.1'
 147    CALL EASKSCMD(ltmp,' Materials database (1.1 ASCII)?',
     &    '  ','cancel',clkok,72,DFILE,'Materials 1.1 ASCII)',IER,5)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          continue
        else
          goto 147
        endif
        if(clkok)then
          return
        else
          CALL mkascimat(IUOUT,IAF,ltmp,IER)
        endif
      endif

      return

C Error messages.
   13 CALL USRMSG(' File write error in asci materials ',LTMPFL,'W')
      IER=1
      return
      end

C ********** EDDBMLC
C EDDBMLC Changes the constructions database current cfg file references.
C moddb is from version manager
C modcon signals change in constructions.
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.

      SUBROUTINE EDDBMLC(moddb,modcon,APP)
      INCLUDE 'building.h'
      INCLUDE 'esprdbfile.h'

      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN
      common/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)

      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C Default esp-r distribution path.
      common/deflt4/dinstpath

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      logical XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical moddb,modcon,modmlc,unixok,copydef,clkok

      character H*72,LTMP*72,DESC*48,path*72,outs*124
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,linstd*60
      character cfgroot*24,LCFGF*72,fs*1,message*48
      character doit*248,LCOPY*72,dinstpath*48,APP*4

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      moddb=.false.
      modcon=.false.

  38  LTMP=LFMUL
      write(outs,'(2a)') 'Options for constructions db: ',
     &  LTMP(1:lnblnk(LTMP))

C If db does not exist offer the following choices:
      CALL ERPFREE(IFMUL,ISTAT)
      call FINDFIL(LFMUL,XST)
      H(1)='A (multilayer) construction database defines the com-'
      H(2)='position of constructions such as walls and glazing'
      H(3)='systems in terms of layers of materials of particular'
      H(4)='thickness in a particular order and, where appropriate,' 
      H(5)='with a named set of optical properties. '
      H(6)=' '
      H(7)='The following options can be selected:'
      H(8)=' '
      H(9)='a) If the file exists than you can browse the database'
      H(10)='   and if you have permission to update the file you'
      H(11)='   can edit the entities in the database.'
      H(12)=' '
      H(13)='b) select another file: you will asked for a file'
      H(14)='   name and if it is found you can browse or edit it.'
      H(15)=' '
      H(16)='c) make a new database: a new database will be created '
      H(17)='   based on the file name you supply.'
      H(18)=' '
      H(19)='d) copy default database: a project copy of the default'
      H(20)='  database will be made and you will be offered a '
      H(21)='  chance to rename it.'

C Get length of path to where esp-r databases were installed.
      write(linstd,'(3a)') dinstpath(1:lnblnk(dinstpath)),
     &    fs,'databases'
      linst=lnblnk(linstd)
      IF(.NOT.XST)THEN
        idno=2
        isw=0
        call MENUATOL(outs,'Constructions options:',
     &    'a browse/edit (NOT APPLICABLE)','b select another file',
     &    'c make a new database','d copy the default database',
     &    ' ',' ',' ',' ',' ',' ',' ',' ',isw,idno,21)
          copydef=.true.
      elseif(LFMUL(1:linst).eq.linstd(1:linst))then

C If corportate database then offer the following choices:
        idno=1
        isw=0
        call MENUATOL(outs,'Constructions options:',
     &    'a browse or edit this file','b select another file',
     &    'c make a new database','d make project copy of database',
     &    ' ',' ',' ',' ',' ',' ',' ',' ',isw,idno,21)
          copydef=.false.
      else

C If db exists locally offer the following choices:
        idno=1
        isw=0
        call MENUATOL(outs,'Constructions options:',
     &    'a browse or edit this file','b select another file',
     &    'c make a new database','d copy the default database',
     &    ' ',' ',' ',' ',' ',' ',' ',' ',isw,idno,21)
        copydef=.true.
      endif

C Act on the users choice.
      if(isw.eq.0)then
        goto 38
      elseif(isw.eq.1)then
        if(XST)then
          CALL ERMLDB(0,IUOUT,IER)
          IF(IER.eq.1)then

C there was a minor problem scanning the constructions db.
            call usrmsg(
     &     'There were a few problems scanning the constructions db',
     &     'and perhaps the materials db. Please double chek.','W')
            goto 38
          ELSEIF(IER.eq.2)then

C There were lots of undefined materials so likely the materials
C database is for the wrong machine type. Guess the name of
C the ascii version and if it exists, offer to convert it.
            call usrmsg(
     &        ' The materials database probably needs to be',
     &        ' restored from archival ASCII version.','W')
            return
          endif
          MLDBOK=.TRUE.
          call tstamp('>','PRJ: enter construction db')
          CALL EDMLDB(ITRC,modmlc,IER,ISEL)
          if(IER.EQ.1)then
            CALL USRMSG(' ','Problem accessing constructions db','W')
            goto 38
          endif
          moddb=modmlc
          modcon=modmlc
        else
          call usrmsg('Sorry there is no file to browse/edit.',
     &                'Please use one of the other options. ','W')
          goto 38
        endif
      elseif(isw.eq.2)then
        h(1)='Please supply and confirm constructions database file'
        h(2)='name. If it exists you will be able to browse it. If '
        h(3)='you want this to be a new database the next task is to'
        h(4)='select the `make new database` option. '
  44    CALL EASKS(LTMP,' Constructions database?',
     &   ' ',72,DFMUL,'constructions database',IER,4)
        IF(LTMP(1:2).NE.'  ')then
          LFMUL=LTMP
        else
          goto 44
        endif
        CALL ERPFREE(IFMUL,ISTAT)
        call FINDFIL(LFMUL,XST)
        if(XST)then

C If file exists and user wants to browse then scan the file and
C use the editing facility.
          write(outs,'(2a)') 'Accessing constructions db ',
     &       LFMUL(1:lnblnk(LFMUL))
          call edisp(iuout,outs)
          CALL ERMLDB(0,ITRU,IER)
          IF(IER.eq.1)then

C there was a minor problem scanning the constructions db.
            call usrmsg(
     &   'There were a few problems scanning the constructions db',
     &   'and perhaps the materials db. Please double chek.','W')
            goto 38
          ELSEIF(IER.eq.2)then

C There were lots of undefined materials so likely the materials
C database is for the wrong machine type. Guess the name of
C the ascii version and if it exists, offer to convert it.
            call usrmsg(
     &        ' The materials database probably needs to be',
     &        ' restored from archival ASCII version.','W')
            return
          endif
          MLDBOK=.TRUE.
          call tstamp('>','PRJ: enter construction db')
          CALL EDMLDB(ITRC,modmlc,IER,ISEL)
          if(IER.EQ.1)then
            CALL USRMSG(' ','Problem accessing construction db','W')
            goto 38
          endif
          moddb=modmlc
          modcon=modmlc
        else
          write(outs,'(2a)') LFMUL(1:lnblnk(LFMUL)),
     &     ' does not appear to exist.'
          call edisp(iuout,outs)
          goto 38
        endif
      elseif(isw.eq.3)then

C No Constructions db found so set-up minimal common block, create
C a new db, populate it and present editing facility.
        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.constrdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.constrdb'
        endif
        clkok=.false.
   45   CALL EASKSCMD(LTMP,' New constructions database?',
     &    ' ','cancel',clkok,72,DFMUL,'constructions database',IER,2)
        if(clkok) return
        IF(LTMP(1:2).NE.'  ')then
          LFMUL=LTMP
        else
          goto 45
        endif
        NMLC=1
        DESC(1)='first_constr  OPAQ  OPAQUE        SYMMETRIC'
        DTHK(1,1)=0.100
        IPR(1,1)=1
        LAYERS(1)=1
        DRAIR(1,1,1)=0.0
        DRAIR(1,1,2)=0.0
        DRAIR(1,1,3)=0.0
        MLDBOK=.TRUE.
        moddb=.true.
        CALL EMKAMLD(iuout,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG(' ',' Unable to create database!','W')
          RETURN
        ENDIF
        ISEL=1
        CALL ERMLDB(0,ITRU,IER)
        IF(IER.NE.0)goto 45
        MLDBOK=.TRUE.
        call tstamp('>','PRJ: enter construction db')
        CALL EDMLDB(ITRC,modmlc,IER,ISEL)
        if(IER.EQ.1)then
          CALL USRMSG(' ','Problem accessing constructions db','W')
          goto 38
        endif
        moddb=modmlc
        modcon=modmlc
      elseif(isw.eq.4)then

C Copy the standard db to project folder. Suggest a local
C file name based on the project root name. Lastly do the copy.

C If the default is to be copied ask for local name. Use LTMP for
C the file name as known to esp-r and LCOPY for the system cp call.   
        if(copydef)then
          call edisp(iuout,'The source file is:')
          call edisp(iuout,DFMUL)
        else
          call edisp(iuout,'The source file is:')
          call edisp(iuout,LFMUL)
        endif
        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.constrdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.constrdb'
        endif
   46   CALL EASKS(LTMP,
     &    'Database name after it is copied into the project?',
     &    ' ',72,'multicon.db','copied constructions database',IER,2)
        call isunix(unixok)
        IF(LTMP(1:2).NE.'  ')then
          write(LCOPY,'(3a)') path(1:lnblnk(path)),fs,
     &      LTMP(1:lnblnk(LTMP))
          if(copydef)then
            doit = ' '
            if(unixok)then
              write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &          DFMUL(1:lnblnk(DFMUL)),' ',LCOPY(1:lnblnk(LCOPY))
            else

C Logic as in materials database copy command.
              message='getting constructions db file from '
              call dbdoscopy(DFMUL,lcopy,message,doit,ier)
            endif
            call lusrmsg('copying default file via',doit,'-')
            call runit(doit,'-')
            LFMUL=LTMP
          else
            doit = ' '
            if(unixok)then
              write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &          LFMUL(1:lnblnk(LFMUL)),' ',LCOPY(1:lnblnk(LCOPY))
            else

C Logic as in materials database copy command.
              message='getting constructions db file from '
              call dbdoscopy(LFMUL,lcopy,message,doit,ier)
            endif
            call lusrmsg('copying default file via',doit,'-')
            call runit(doit,'-')
            LFMUL=LTMP
          endif
          write(outs,'(2a)') 'Accessing constructions db: ',
     &      LCOPY(1:lnblnk(LCOPY))
          call edisp(iuout,outs)
          call tstamp('>','PRJ: enter construction db')
          CALL ERMLDB(0,IUOUT,IER)
          IF(IER.eq.1)then

C there was a minor problem scanning the constructions db.
            call usrmsg(
     &     'There were a few problems scanning the constructions db',
     &     'and perhaps the materials db. Please double check.','W')
            goto 38
          ELSEIF(IER.eq.2)then

C There were lots of undefined materials so likely the materials
C database is for the wrong machine type. Guess the name of
C the ascii version and if it exists, offer to convert it.
            call usrmsg(
     &        ' The materials database probably needs to be',
     &        ' restored from archival ASCII version.','W')
            return
          endif
          MLDBOK=.TRUE.
          call tstamp('>','PRJ: enter construction db')
          CALL EDMLDB(ITRC,modmlc,IER,ISEL)
          if(IER.EQ.1)then
            CALL USRMSG(' ','Problem accessing construction db','W')
            goto 38
          endif
          moddb=modmlc
          modcon=modmlc
        else
          goto 46
        endif
      endif

      return
      end

C ********** EDDBPCDB
C EDDBPCDB Changes the plant component database current cfg file references.
C moddb is from version manager
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.

      SUBROUTINE EDDBPCDB(moddb,APP)
      INCLUDE 'esprdbfile.h'
      INCLUDE 'plantdb.h'

      PARAMETER  (MCP=35)

      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/C21/IFCFG,cfgroot,LCFGF
      common/appw/iappw,iappx,iappy
      common/rpath/path
      common/deflt4/dinstpath

      CHARACTER H*72,LTMP*72
      character longtfile*144,message*48
      CHARACTER DOIT*248,fs*1,LCOPY*72
      CHARACTER OUTS*124,APP*4,ext*4
      CHARACTER*24 ZONEPTH,NETPTH,CTLPTH,IMGPTH,RADPTH,DBSPTH
      CHARACTER*24 DOCPTH,TMPPTH
      LOGICAL CLKOK,OK,COPYDEF,MODDB,XST,DOK,UNIXOK,concat
      character cfgroot*24,LCFGF*72
      character dinstpath*48,path*72,linstd*60,tmode*8
      CHARACTER ZPDESC*25

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      moddb=.false.

  60  LTMP=LPCDB
      write(outs,'(2a)') 'Options for plant template db: ',
     &  LTMP(1:lnblnk(LTMP))
      H(1)='The plant components database is a binary, random '
      H(2)='access file holding plant component descriptions to '
      H(3)='enable the establishment of component state-space '
      H(4)=' representations used by ESP-r.'
      h(5)=' '
      h(6)='If there is no such file then a new one will be'
      h(7)='created to hold data for new components.'
      h(8)=' '
      H(9)='The following options can be selected:'
      H(10)=' '
      H(11)='a) If the file exists than you can browse the database '
      H(12)='   and if you have permission to update the file you'
      H(13)='   can edit the entities in the database.'
      H(14)=' '
      H(15)='b) select another file: you will asked for a file'
      H(16)='   name and if it is found you can browse or edit it.'
      H(17)=' '
      H(18)='c) make a new database: a new database will be created '
      H(19)='   based on the file name you supply.'
      H(20)=' '
      H(21)='d) copy default database: a project copy of the default'
      H(22)='  database will be made and you will be offered a '
      H(23)='  chance to rename it.'

C Use the profile database unit for testing plant db.
      if(ipcdb.eq.0)ipcdb=iprodb
      CALL ERPFREE(ipcdb,ISTAT)

C Get length of path to where esp-r databases were installed.
      write(linstd,'(3a)') dinstpath(1:lnblnk(dinstpath)),
     &  fs,'databases'
      linst=lnblnk(linstd)
      call FINDFIL(LTMP,XST)
      IF(.NOT.XST)THEN
        idno=2
        isw=0
        call MENUATOL(outs,'Plant component options:',
     &    'a browse/edit (NOT APPLICABLE)','b select another file',
     &    'c make a new database','d copy the default database',
     &    ' ',' ',' ',' ',' ',' ',' ',' ',isw,idno,23)
        copydef=.true.
      elseif(LTMP(1:linst).eq.linstd(1:linst))then

C If corportate database then offer the following choices:
        idno=1
        isw=0
        call MENUATOL(outs,'Plant component options:',
     &    'a browse or edit this file','b select another file',
     &    'c make a new database','d make project copy of database',
     &    ' ',' ',' ',' ',' ',' ',' ',' ',isw,idno,23)
        copydef=.false.
      else

C If db exists locally offer the following choices:
        idno=1
        isw=0
        call MENUATOL(outs,'Plant component options:',
     &    'a browse or edit this file','b select another file',
     &    'c make a new database','d copy the default database',
     &    ' ',' ',' ',' ',' ',' ',' ',' ',isw,idno,23)
        copydef=.true.
      endif

C Act on the users choice. If no choice retrun to main menu.
      if(isw.eq.0)then
        return
      elseif(isw.eq.1)then

C If file exists open it, test file type, get number of items and close.
        CALL ERPFREE(ipcdb,ISTAT)
        call FINDFIL(LTMP,XST)
        if(XST)then
          IER=0
          call EFOPRAN(ipcdb,LTMP,IRECLN,1,IER)
          IF(ier.eq.0)THEN
            IREC = 1
            READ (ipcdb,REC=IREC,IOSTAT=ISTAT,ERR=60) ZPDESC
            IF (ZPDESC.NE.' PLANT COMPONENT DATABASE')then
              call usrmsg('File is not a plant component database,',
     &          'please supply the file name again.','W')
              goto 60
            endif
            IREC = 2
            READ (ipcdb,REC=IREC,IOSTAT=ISTAT,ERR=60) NPC,NXTREC
            write(outs,'(a,i3,2a)') 'There are ',NPC,
     &        ' components in ',LTMP(1:lnblnk(LTMP))
            call edisp(iuout,outs)
            CALL ERPFREE(ipcdb,ISTAT)

C Get logical name of child process terminal type, expand model 
C name to include the path and create a string to drive pdb.
            call tstamp('>','PRJ: start pdb')
            doit = ' '
            call tchild(ICPMOD)
            call termode(ICPMOD,tmode)
            call addpath(LPCDB,longtfile,concat)
            if(iappw.gt.0.and.iappw.le.200)then
              write(doit,'(3a,3i4,3a)') 'pdb -mode ',tmode,
     &          ' -s ',iappw,iappx+10,iappy+40,' -file ',
     &          longtfile(1:lnblnk(longtfile)),' &'
            else
              write(doit,'(5a)') 'pdb -mode ',tmode,
     &          ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),
     &          ' &'
            endif
            call usrmsg(' ','staring plant db module.','-')
            call runit(doit,tmode)
          else
            call usrmsg('Sorry, error opening the file.',
     &        'Please use one of the other options.','W')
            goto 60
          endif
        else
          call usrmsg('Sorry there is no file to browse/edit.',
     &      'Please use one of the other options. ','W')
          goto 60
        endif
      elseif(isw.eq.2)then
        H(1)='The plant components database is a binary, random '
        H(2)='access file holding plant component descriptions to '
        H(3)='enable the establishment of component state-space '
        H(4)=' representations used by ESP-r.'
        CALL EASKS(ltmp,'Plant component database?',
     &    ' ',72,DPCDB,'plant component database',IER,4)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          LPCDB=ltmp
        else
          goto 60
        endif
        CALL ERPFREE(ipcdb,ISTAT)
        call FINDFIL(LPCDB,XST)
        IF(XST)THEN

C If file exists and user wants to browse then scan the file and
C use the editing facility.
          IER=0
          call EFOPRAN(ipcdb,LTMP,IRECLN,1,IER)
          IF(ier.eq.0)THEN
            IREC = 1
            READ (ipcdb,REC=IREC,IOSTAT=ISTAT,ERR=60) ZPDESC
            IF (ZPDESC.NE.' PLANT COMPONENT DATABASE')then
              call usrmsg('File is not a plant component database,',
     &          'please supply the file name again.','W')
              goto 60
            endif
            IREC = 2
            READ (ipcdb,REC=IREC,IOSTAT=ISTAT,ERR=60) NPC,NXTREC
            write(outs,'(a,i3,2a)') 'There are ',NPC,
     &        ' components in ',LTMP(1:lnblnk(LTMP))
            call edisp(iuout,outs)
            CALL ERPFREE(ipcdb,ISTAT)
          else
            call usrmsg('Sorry, error opening the file.',
     &        'Please use one of the other options.','W')
            goto 60
          endif
          write(outs,'(2a)') 'Accessing plant db ',
     &      LPCDB(1:lnblnk(LPCDB))
          call edisp(iuout,outs)

C Get logical name of child process terminal type, expand model 
C name to include the path and create a string to drive pdb.
          call tstamp('>','PRJ: start pdb')
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          call addpath(LPCDB,longtfile,concat)
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,3a)') 'pdb -mode ',tmode,
     &        ' -s ',iappw,iappx+10,iappy+40,' -file ',
     &        longtfile(1:lnblnk(longtfile)),' &'
          else
            write(doit,'(5a)') 'pdb -mode ',tmode,
     &        ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),' &'
          endif
          call usrmsg(' ','staring plant db module.','-')
          call runit(doit,tmode)
        else
          write(outs,'(2a)') LPCDB(1:lnblnk(LPCDB)),
     &      ' does not appear to exist.'
          call edisp(iuout,outs)
          goto 60
        endif
      elseif(isw.eq.3)then

C No plant component db found so set-up minimal common block, create
C a new db, populate it and present editing facility.
        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.plantdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.plantdb'
        endif
 62     h(1)='The plant component database binary file holds the'
        h(2)='description of available components. If the name'
        h(3)='given matches an existing file, it will be read in'
        h(4)='and the components data will be available for'
        h(5)='listing and editing. If there is no such file then'
        h(6)='a new one will be created to hold data for new'
        h(7)='components.'
        clkok=.false.
        CALL EASKSCMD(ltmp,'Plant component database?',
     &    ' ','cancel',clkok,72,DPCDB,'plant component database',
     &    IER,7)
        if(clkok) return
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          LPCDB=ltmp
        else
          goto 62
        endif

C Create a minimal plant databases (code similar to pdb.F).
        moddb=.true.
        CALL ERPFREE(ipcdb,ISTAT)
        call EFOPRAN(ipcdb,LPCDB,IRECLN,3,IER)
        CALL PCUPDT (ipcdb,0)
        CALL ERPFREE(ipcdb,ISTAT)

C If file exists and user wants to browse then scan the file and
C use the editing facility.
        write(outs,'(2a)') LPCDB(1:lnblnk(LPCDB)),' created.'
        dok=.true.
        h(1)='An empty plant component database has been created'
        h(2)='and it it useless until it has been populated with'
        h(3)='data. If you have the information required you can'
        h(4)='proceed with that task. '
        CALL ASKOK(outs,' Browse or edit new database?',OK,dok,4)
        if(OK)then

C Get logical name of child process terminal type, expand model 
C name to include the path and create a string to drive pdb.
          call tstamp('>','PRJ: start pdb')
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          call addpath(LPCDB,longtfile,concat)
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,3a)') 'pdb -mode ',tmode,
     &        ' -s ',iappw,iappx+10,iappy+40,' -file ',
     &        longtfile(1:lnblnk(longtfile)),' &'
          else
            write(doit,'(5a)') 'pdb -mode ',tmode,
     &        ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),' &'
          endif
          call usrmsg(' ','staring plant db module.','-')
          call runit(doit,tmode)
        endif
      elseif(isw.eq.4)then

C Copy the standard db to project folder. Suggest a local
C file name based on the project root name. Lastly do the copy.
C If the default is to be copied ask for local name. Use LTMP for
C the file name as known to esp-r and LCOPY for the system cp call.   
        if(copydef)then
          call edisp(iuout,'The source file is:')
          call edisp(iuout,DPCDB)
        else
          call edisp(iuout,'The source file is:')
          call edisp(iuout,LPCDB)
        endif
        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.plantdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.plantdb'
        endif
   66   CALL EASKS(LTMP,
     &    'Database name after it is copied into the project?',
     &    ' ',72,'plant.db','copied plant database',IER,2)
        call isunix(unixok)
        IF(LTMP(1:2).NE.'  ')then
          write(LCOPY,'(3a)') path(1:lnblnk(path)),fs,
     &      LTMP(1:lnblnk(LTMP))
          if(copydef)then
            doit = ' '
            if(unixok)then
              write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &          DPCDB(1:lnblnk(DPCDB)),' ',LCOPY(1:lnblnk(LCOPY))
            else

C Logic same as other copy database commands.
              message='getting constructions db file from '
              call dbdoscopy(DPCDB,lcopy,message,doit,ier)
            endif
            call usrmsg('copying default file via',doit,'-')
            call runit(doit,'-')
            LPCDB=LTMP
          else
            doit = ' '
            if(unixok)then
              write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &          LPCDB(1:lnblnk(LPCDB)),' ',LCOPY(1:lnblnk(LCOPY))
            else

C Logic as other copy database.
              message='getting constructions db file from '
              call dbdoscopy(LPCDB,lcopy,message,doit,ier)
            endif
            call usrmsg('copying default file via',doit,'-')
            call runit(doit,'-')
            LPCDB=LTMP
          endif
          write(outs,'(2a)') 'Accessing plant template db: ',
     &      LCOPY(1:lnblnk(LCOPY))
          call edisp(iuout,outs)
          call tstamp('>','PRJ: start pdb')
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          call addpath(LPCDB,longtfile,concat)
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,3a)') 'pdb -mode ',tmode,
     &        ' -s ',iappw,iappx+10,iappy+40,' -file ',
     &        longtfile(1:lnblnk(longtfile)),' &'
          else
            write(doit,'(5a)') 'pdb -mode ',tmode,
     &        ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),' &'
          endif
          call usrmsg(' ','staring plant db module.','-')
          call runit(doit,tmode)
          moddb=.true.
        else
          goto 66
        endif
      endif

      return
      end

C ********** EDDBPROFILE
C EDDBPROFILE Changes the events profile database current cfg file references.
C This version uses call to subroutine casfmk. 
C moddb is from version manager
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.
C
      SUBROUTINE EDDBPROFILE(moddb,APP)
      INCLUDE 'esprdbfile.h'

      PARAMETER  (MCP=35)

      common/FILEP/IFIL
      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path
      common/deflt4/dinstpath
      COMMON/PDBITM/NITEMS,lastrec,NCG(100),NO(100),DESCP(100)
      COMMON/PDBELM/ICGS1(MCP),ICGF1(MCP),CGS1(MCP),CGL1(MCP)

      CHARACTER H*72,LTMP*72,LASCI*72,DFILE*72,lltmp*144,lprev*144
      character longtfile*144,LCOPY*144,message*48
      CHARACTER DOIT*300,fs*1,APP*4,ext*4
      CHARACTER OUTS*124,OUTS248*248
      CHARACTER*24 ZONEPTH,NETPTH,CTLPTH,IMGPTH,RADPTH,DBSPTH
      CHARACTER*24 DOCPTH,TMPPTH
      character DESCP*40
      LOGICAL CLKOK,OK,COPYDEF,MODDB,XST,DOK,UNIXOK,concat
      character cfgroot*24,LCFGF*72
      character dinstpath*48,path*72,dirpath*48

C Local strings for user selections dependent on graphic library.
      character opte*28,optf*28,optg*28,opth*28,opti*28

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
      ldirpath=lnblnk(dirpath)

      moddb=.false.
      lprev=LPRFDB

 40   lltmp=LPRFDB
      llt=lnblnk(lltmp)
      write(outs248,'(2a)') 'Options for event profiles db: ',
     &  lltmp(1:lnblnk(lltmp))
      write(outs,'(a)') outs248(1:123)
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
      H(16)='The following options can be selected:'
      H(17)=' '
      H(18)='a) If the file exists than you can browse the database '
      H(19)='   and if you have permission to update the file you'
      H(20)='   can edit the entities in the database.'
      H(21)=' '
      H(22)='b) Select from list (not available for optics databases).'
      H(23)=' '
      H(24)='c) Make a new database: a new database will be created '
      H(25)='   based on the file name you supply. It will contain a'
      H(26)='   minimal number of items and you can then modify this'
      H(27)='   as required. '
      H(28)='   Exercise care if you use a text editor to copy sets'
      H(29)='   of data from another optical database.'
      H(30)=' '
      H(31)='d) copy default database: a project copy of the default'
      H(32)='  database will be made and you will be offered a '
      H(33)='  chance to rename it.'
      H(34)=' '
      H(35)='e) binary -> ascii conversion saves current database '
      H(36)='  to a computer independant form.'
      H(37)=' '
      H(38)='f) ascii -> binary useful when switching computers.'
      H(39)=' '

C If GTK library then there are additional options so include in help.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.2)then
        H(40)='g) browse ../dbs allows you to look in the model ../dbs'
        H(41)='  folder for alternative optics files. If you do not'
        H(42)='  find anything the previous value will be restored.'
        H(43)=' '
        H(44)='h) browse distribution displays files in the databases'
        H(45)='  folder of the ESP-r distribution. If you do not find'
        H(46)='  anything the previous value will be restored.'
        H(47)=' '
        H(48)='i) copy another database to the project allows you to'
        H(49)=' locate a database and get a project copy of it.'
      else
        H(40)='g) select a file (X11 only): edit the file name'
        H(41)='   and if it is found you can browse or edit it.'
        H(42)='   Hint: if you eventually want to copy that other file'
        H(43)='   into the model folder first select it and then ask'
        H(44)='   for a copy.'
        H(45)=' '
        H(46)=' '
        H(47)=' '
        H(48)=' '
        H(49)=' '
      endif
      CALL ERPFREE(IPRODB,ISTAT)
      call addpath(LLTMP,longtfile,concat)
      ltf=max(1,LNBLNK(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up additional options depending on graphics lib. The second
C option (list select) is not available for profiles database. If using
C text or X11 option e edits the string while for GTK there are two
C options to browse for files in ../dbs or the esp-r distribution.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        opte='e binary > ascii'
        optf='f ascii > binary'
        optg='g edit file name'
        opth=' '
        opti=' '
      elseif(iglib.eq.2)then
        opte='e binary > ascii'
        optf='f ascii > binary'
        optg='g browse ../dbs'
        opth='h browse distribution'
        opti='i make copy of another file'
      endif

      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices:
        idno=2
        isw=0
        call MENUATOL(outs,'Event profile options:',
     &    'a browse/edit (NOT APPLICABLE)','b select from list (N/A)',
     &    'c make a new database','d copy the default database',
     &    opte,optf,optg,opth,opti,' ',' ',' ',isw,idno,49)
        copydef=.true.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If corportate database then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL(outs,'Event profile options:',
     &    'a browse or edit this file','b select from list (N/A)',
     &    'c make a new database','d make project copy of database',
     &    opte,optf,optg,opth,opti,' ',' ',' ',isw,idno,49)
        copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If corportate database then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL(outs,'Event profile options:',
     &    'a browse or edit this file','b select from list (N/A)',
     &    'c make a new database','d make project copy of database',
     &    opte,optf,optg,opth,opti,' ',' ',' ',isw,idno,49)
        copydef=.false.
      else

C If db exists locally toggle whether the user as asked to copy the
C default database or the current database.
        idno=1
        isw=0
        call MENUATOL(outs,'Event profile options:',
     &    'a browse or edit this file','b select from list (N/A)',
     &    'c make a new database','d copy the default database',
     &    opte,optf,optg,opth,opti,' ',' ',' ',isw,idno,49)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.9)copydef=.false.
      endif

C Act on the users choice. If nothing selected return to main menu.
      if(isw.eq.0)then
        return
      elseif(isw.eq.1)then

C If file exists open it, test file type, get number of items and close.
        if(XST)then
          LPRFDB=lltmp
          goto 44
        else
          call usrmsg('Sorry there is no file to browse/edit.',
     &      'Please use one of the other options. ','W')
          goto 40
        endif
      elseif(isw.eq.2)then

C There is no select by list for optics database.
        goto 40
      elseif(isw.eq.3)then

C No event profile db found so set-up minimal common block, create
C a new db, populate it and present editing facility.
        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.profiledb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.profiledb'
        endif
        H(1)='An event profiles db holds a schedule of patterns'
        H(2)='over a day. '
        clkok=.false.
 42     CALL EASKSCMD(ltmp,' Event profiles database?',
     &    ' ','cancel',clkok,72,DPRFDB,'event profiles database',
     &    IER,2)
        if(clkok) return
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          write(LPRFDB,'(a)') ltmp(1:lnblnk(ltmp))
        else
          goto 42
        endif

C Create a minimal events databases.
        moddb=.true.
        call EFOPRAN(IPRODB,LPRFDB,10,3,IER)
        NITEMS = 1
        NO(1) = 2
        NCG(1)= 1
        DESCP(1)='Always ON 100%'
        ICGS1(1)=0
        ICGF1(1)=24
        CGS1(1)=100.0
        CGL1(1)=100.0
        lastrec=4
        IREC = 1
        WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450) NITEMS
        IREC=NO(1)
        WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)NCG(1)
        IREC=IREC+1
        WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)DESCP(1)
        IREC=IREC+1
        WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)
     &        ICGS1(1),ICGF1(1),CGS1(1),CGL1(1)
        CALL ERPFREE(IPRODB,ISTAT)

C If file exists and user wants to browse then scan the file and
C use the editing facility.
        if(lnblnk(LPRFDB).lt.100)then
          write(outs,'(3a)') 'Events db ',LPRFDB(1:lnblnk(LPRFDB)),
     &     ' created.'
        else
          write(outs,'(3a)') 'Events db ',LPRFDB(1:100),
     &     '... created.'
        endif
        dok=.true.
        h(1)='An empty profiles database has been created. It'
        h(2)='is useless unless it is populated with data. If '
        h(3)='you have the required data, use this facility '
        h(4)='to do this.'
        CALL ASKOK(outs,' Browse or edit database?',OK,dok,4)
        if(OK)then
          moddb=.true.
          goto 44
        endif
      elseif(isw.eq.4.or.isw.eq.9)then

C Copy a database to project dbs folder. If iws is 4 then the
C user has asked for the default database. If isw is 7 then the
C user wishes to browse for the source file. Suggest a local
C file name based on the project root name then do the copy.
C Use LTMP (short buffer) for the file name as known to esp-r
C and LCOPY for the system cp call.   
        if(copydef)then
          write(outs248,'(2a)') 'The source file is:',
     &      DPRFDB(1:lnblnk(DPRFDB))
          call edisp248(iuout,outs248,100)
        else
          h(1)='You are asked to confirm the source file for the'
          h(2)='copy action. If this one is ok then accept it. '
          write(outs248,'(2a)') 'The current source file is:',
     &      LPRFDB(1:lnblnk(LPRFDB))
          call edisp248(iuout,outs248,100)
          lltmp=LPRFDB
          llt=lnblnk(lltmp)
          iglib = igraphiclib()
          if(iglib.eq.1.or.iglib.eq.3)then
            if(llt.lt.96)then
              CALL EASKF(lltmp,' Event profile db (source)? ',
     &          ' ',96,DPRFDB,'binary events db',IER,2)
            elseif(llt.ge.96.and.llt.lt.124)then
              CALL EASKF(lltmp,' Event profile db(source)? ',
     &          ' ',124,DPRFDB,'binary events db',IER,2)
            elseif(llt.ge.124.and.llt.le.144)then
              CALL EASKF(lltmp,' Event profile db(source)? ',
     &          ' ',144,DPRFDB,'binary events db',IER,2)
            endif
          elseif(iglib.eq.2)then
            CALL EASKF(lltmp,' Event profile db(source)? ',
     &        ' ',144,DPRFDB,'binary events db',IER,2)
          else
            CALL EASKF(lltmp,' Event profile db(source)? ',
     &        ' ',96,DPRFDB,'binary events db',IER,2)
          endif
          LPRFDB=lltmp
        endif
        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.eventdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.eventdb'
        endif
 244    CALL EASKS(LTMP,
     &    'Database name after it is copied into the project?',
     &    ' ',72,'event.db','copied events database',IER,2)
        call isunix(unixok)
        IF(LTMP(1:2).NE.'  ')then

C If Unix and the path is ./ then no need to prepend this (to
C avoid .//../dbs in the buffer).  If path ends with file separator
C no need to duplicat the file separator.
          if(unixok)then
            lpp=lnblnk(path)
            if(path(1:2).eq.'./')then
              write(LCOPY,'(a)') LTMP(1:lnblnk(LTMP))
            elseif(path(lpp:lpp).eq.fs)then
              write(LCOPY,'(2a)') path(1:lnblnk(path)),
     &          LTMP(1:lnblnk(LTMP))
            else
              write(LCOPY,'(3a)') path(1:lnblnk(path)),fs,
     &          LTMP(1:lnblnk(LTMP))
            endif
          else
            write(LCOPY,'(3a)') path(1:lnblnk(path)),fs,
     &        LTMP(1:lnblnk(LTMP))
          endif
          if(copydef)then
            doit = ' '
            if(unixok)then
              write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &          DPRFDB(1:lnblnk(DPRFDB)),' ',LCOPY(1:lnblnk(LCOPY))
            else

C Logic for copy similar to the pattern established above.
              message='getting events db file from '
              call dblongdoscopy(DPRFDB,lcopy,message,doit,ier)
            endif
            call usrmsg('copying default file via',doit,'-')
            call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
            call FINDFIL(LCOPY,XST)
            if(XST)then
              write(LPRFDB,'(a)') LCOPY(1:lnblnk(LCOPY))
            else
              write(LPRFDB,'(a)') LTMP(1:lnblnk(LTMP))
            endif
          else
            doit = ' '
            if(unixok)then
              write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &          LPRFDB(1:lnblnk(LPRFDB)),' ',LCOPY(1:lnblnk(LCOPY))
            else

C Logic for copy similar to the pattern established above.
              message='getting events db file from '
              call dblongdoscopy(LPRFDB,lcopy,message,doit,ier)
            endif
            call usrmsg('copying file via',doit,'-')
            call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
            call FINDFIL(LCOPY,XST)
            if(XST)then
              write(LPRFDB,'(a)') LCOPY(1:lnblnk(LCOPY))
            else
              call FINDFIL(LTMP,XST)
              if(XST)then
                write(LPRFDB,'(a)') LTMP(1:lnblnk(LTMP))
              else
                write(LPRFDB,'(a)') LTMP(1:lnblnk(LTMP))
                call edisp(iuout,'Problem locating the new database.')
                call edisp(iuout,'try browse ../dbs to find it.')
              endif
            endif
          endif
          moddb=.true.
          goto 44
        else
          goto 244
        endif
      elseif(isw.eq.5)then

C Profile db: BINARY > ASCII. 
C Ask for the binary file name and then confirm the ASCII db name.
        IAF=IFIL+1
        H(1)='The events db contains one or more descriptions of'
        H(2)='profiles created via a diary entry format.'
        lltmp=LPRFDB
        llt=lnblnk(lltmp)

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(lltmp,' Event profile db? ',
     &        ' ',96,DPRFDB,'binary events db',IER,2)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(lltmp,' Event profile db? ',
     &        ' ',124,DPRFDB,'binary events db',IER,2)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(lltmp,' Event profile db? ',
     &        ' ',144,DPRFDB,'binary events db',IER,2)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(lltmp,' Event profile db? ',
     &      ' ',144,DPRFDB,'binary events db',IER,2)
        else
          CALL EASKF(lltmp,' Event profile db? ',
     &      ' ',96,DPRFDB,'binary events db',IER,2)
        endif
        if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then
          LPRFDB=lltmp
        else
          goto 40
        endif

C 144 -> 72 character switch so limit portion written to LASCI.
        llt=lnblnk(LPRFDB)
        if(llt.le.69)then 
          write(LASCI,'(2a)') LPRFDB(1:lnblnk(LPRFDB)),'.a'
        else
          write(LASCI,'(2a)') LPRFDB(1:69),'.a'
        endif

        DFILE='./newprofiles.db1.a'
        CALL EASKS(LASCI,' ASCII event profile? ',
     &    ' ',72,DFILE,'ascii events db',IER,2)

C Do conversion to ASCII and then redisplay the menu.
        CALL TROPRO(iuout,IAF,LASCI,IER)
        call tstamp('>','PRO: convert binary to ascii')
        goto 40
      elseif(isw.eq.6)then

C Profile db: ASCII > BINARY.
C Ask for the ASCII file name and then confirm the binary db name.
        IAF=IFIL+1
        H(1)='The events db contains one or more descriptions of'
        H(2)='profiles created via a diary entry format.'
        llt=lnblnk(LPRFDB)
        if(llt.le.69)then 
          write(LASCI,'(2a)') LPRFDB(1:lnblnk(LPRFDB)),'.a'
        else
          write(LASCI,'(2a)') LPRFDB(1:69),'.a'
        endif
        CALL EASKS(LASCI,' ASCII event profile source? ',
     &    ' ',72,'profiles.db1.a','ascii events db',IER,2)

        H(1)='The existing binary events db will be overwritten'
        H(2)='with the data from the ASCII version.'
        write(lltmp,'(a)') LPRFDB(1:lnblnk(LPRFDB))
        llt=lnblnk(lltmp)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(lltmp,' Event profile db? ',
     &        ' ',96,DPRFDB,'binary events db',IER,2)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(lltmp,' Event profile db? ',
     &        ' ',124,DPRFDB,'binary events db',IER,2)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(lltmp,' Event profile db? ',
     &        ' ',144,DPRFDB,'binary events db',IER,2)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(lltmp,' Event profile db? ',
     &      ' ',144,DPRFDB,'binary events db',IER,2)
        else
          CALL EASKF(lltmp,' Event profile db? ',
     &      ' ',96,DPRFDB,'binary events db',IER,2)
        endif

        if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then

C Do conversion and then to prove that the file was correctly
C converted proceed to browse/edit the details.
          LPRFDB=lltmp
          CALL TRIPRO(iuout,IAF,LASCI,IER)
          moddb=.true.
          goto 44
        else
          goto 40
        endif

      elseif(isw.eq.7)then

C Browse ../dbs if GTK. Fill LCOPY with the model dbspth so user
C can see the files in that folder. Note the user may browse away
C from this location. Easkf returns the absolut path to the
C file selected (later see if this can be resolved to ../dbs). 
C Test the returned file and if it is not readable reset LPRFDB.
        if(iglib.eq.2)then
          h(1)='An event profiles database holds named sets of profiles'
          h(2)='in a binary representation. '
          H(3)=' '
          H(4)='A local database folder (../dbs) is usually associated'
          H(5)='with each model. '
          write(LCOPY,'(2a)') dbspth(1:lnblnk(dbspth)),fs
          CALL EASKF(LCOPY,' Event profiles database?',
     &      ' ',144,DOPTDB,'Event profiles database',IER,5)
          LPRFDB=LCOPY
          moddb=.true.
          goto 44
        else

C For X11 and text allow user to type in a file name.
          h(1)='An event profiles database holds named sets of profiles'
          h(2)='in a binary representation. '
          write(lltmp,'(a)') LPRFDB(1:lnblnk(LPRFDB))
          llt=lnblnk(lltmp)

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
          if(llt.lt.96)then
            CALL EASKF(lltmp,' Event profile db? ',
     &        ' ',96,DPRFDB,'binary events db',IER,2)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(lltmp,' Event profile db? ',
     &        ' ',124,DPRFDB,'binary events db',IER,2)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(lltmp,' Event profile db? ',
     &        ' ',144,DPRFDB,'binary events db',IER,2)
          endif
          if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then
            LPRFDB=lltmp
            moddb=.true.
            goto 44
          else
            goto 40
          endif
        endif

      elseif(isw.eq.8)then

C Browse distribution database folder. Setup LCOPY to point to
C the database folder (but no particular file). Use easkf to
C return a file (which might be from somewhere else if the user
C decides to do some browsing). Test this suggested file via
C call to eroptdb and if this is not successful reset LOPTDB.
        if(iglib.eq.2)then
          h(1)='An event profiles database holds named sets of profiles'
          h(2)='in a binary representation. '
          H(3)=' '
          H(4)='The ESP-r distribution includes alternatives to the'
          H(5)='default which might be useful.'
          write(LCOPY,'(4a)') dinstpath(1:lnblnk(dinstpath)),
     &      fs,'databases',fs
          CALL EASKF(LCOPY,' Event profiles database?',
     &      ' ',144,DOPTDB,'Event profiles database',IER,5)
          LPRFDB=LCOPY
          moddb=.true.
          goto 44
        else
          call usrmsg('Browse feature not available',' ','W')
          goto 40
        endif
      endif

C Test the selected or copied database. If a problem restore
C the previous file name and loop back, otherwise edit the
C database contents.
   44 continue
      write(outs248,'(2a)') 'Accessing event profile db: ',
     &  LPRFDB(1:lnblnk(LPRFDB))
      call edisp248(iuout,outs248,100)
      IER=0
      call EFOPRAN(IPRODB,LPRFDB,10,1,IER)
      IF(ier.eq.0)then
        CALL USRMSG(' opened profiles db...',' ','-')
        IREC=1
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=450)NITEMS
        CALL USRMSG(' opened profiles db...ok',' ','-')
        CALL ERPFREE(IPRODB,ISTAT)

C Call casfmk for the editing/browsing interface.
        call casfmk(LPRFDB,ier)
        return
      else
        call usrmsg('Sorry, error opening the file.',
     &    'Please use one of the other options.','W')
        LPRFDB=lprev
        goto 40
      endif

 450  call edisp(iuout,' Event profile file error  ')
      return

      end

C ********** EDDBOPT
C EDDBOPT Changes the optical database current cfg file references.
C moddb is from version manager
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.
C This facility offers browsing for GTK version.
      SUBROUTINE EDDBOPT(moddb,APP)
      INCLUDE 'building.h'
      INCLUDE 'esprdbfile.h'
      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path
      common/deflt4/dinstpath
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      CHARACTER H*72,LTMP*72,lltmp*144,lprev*144
      character longtfile*144,LCOPY*144,message*48
      CHARACTER DOIT*300,fs*1,APP*4,ext*4
      CHARACTER OUTS*124,outs248*248
      CHARACTER*24 ZONEPTH,NETPTH,CTLPTH,IMGPTH,RADPTH,DBSPTH
      CHARACTER*24 DOCPTH,TMPPTH
      character GTYPE*12,GDESCR*36,SOPT*12
      LOGICAL CLKOK,OK,COPYDEF,MODDB,XST,DOK,UNIXOK
      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,concat
      character cfgroot*24,LCFGF*72
      character dinstpath*48,path*72,dirpath*48

C Local strings for user selections dependent on graphic library.
      character opte*28,optf*28,optg*28

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
      ldirpath=lnblnk(dirpath)

C Remember the initial file name in case user choice fails.
      moddb=.false.
      lprev=LOPTDB

  50  lltmp=LOPTDB
      write(outs248,'(2a)') 'Options for optical properties db: ',
     &  LLTMP(1:lnblnk(LLTMP))
      write(outs,'(a)') outs248(1:123)
      H(1)='The optical properties of multilayer constructions'
      H(2)='are defined in an optical database.  The data include'
      H(3)='angular (0, 40, 55, 70, 80 degrees from normal) solar' 
      H(4)='transmission and absorption at each layer.'
      H(5)=' '
      H(6)='The following options can be selected:'
      H(7)=' '
      H(8)='a) If the file exists than you can browse the database '
      H(9)='   and if you have permission to update the file you'
      H(10)='   can edit the entities in the database.'
      H(11)=' '
      H(12)='b) Select from list (not available for optics databases).'
      H(13)=' '
      H(14)='c) Make a new database: a new database will be created '
      H(15)='   based on the file name you supply. It will contain a'
      H(16)='   minimal number of items and you can then modify this'
      H(17)='   as required. '
      H(18)='   Exercise care if you use a text editor to copy sets'
      H(19)='   of data from another optical database.'
      H(20)=' '
      H(21)='d) copy default database: a project copy of the default'
      H(22)='  database will be made and you will be offered a '
      H(23)='  chance to rename it.'
      H(24)=' '

C If GTK library then there are additional options so include in help.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.2)then
        H(25)='e) browse ../dbs allows you to look in the model ../dbs'
        H(26)='  folder for alternative optics files. If you do not'
        H(27)='  find anything the previous value will be restored.'
        H(28)=' '
        H(29)=' f) browse distribution displays files in the databases'
        H(30)='  folder of the ESP-r distribution. If you do not find'
        H(31)='  anything the previous value will be restored.'
        H(32)=' '
        H(33)=' g) copy another database to the project allows you to'
        H(34)=' locate a database and get a project copy of it.'
      else
        H(25)='e) select a file (X11 only): edit the file name'
        H(26)='   and if it is found you can browse or edit it.'
        H(27)='   Hint: if you eventually want to copy that other file'
        H(28)='   into the model folder first select it and then ask'
        H(29)='   for a copy.'
        H(30)=' '
        H(31)=' '
        H(32)=' '
        H(33)=' '
        H(34)=' '
      endif
      CALL ERPFREE(IOPTDB,ISTAT)
      call addpath(LLTMP,longtfile,concat)
      ltf=max(1,LNBLNK(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up additional options depending on graphics lib. The second
C option (list select) is not available for optics database. If using
C text or X11 option e edits the string while for GTK there are two
C options to browse for files in ../dbs or the esp-r distribution.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        opte='e edit file name'
        optf='   '
        optg='   '
      elseif(iglib.eq.2)then
        opte='e browse ../dbs'
        optf='f browse distribution'
        optg='g make copy of another file'
      endif

      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices:
        idno=2
        isw=0
        call MENUATOL(outs,'Optical properties options:',
     &    'a browse/edit (NOT APPLICABLE)','b select from list (N/A)',
     &    'c make a new database','d copy the default database',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,34)
        copydef=.true.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If corportate database then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL(outs,'Optical properties options:',
     &    'a browse or edit this file','b select from list (N/A)',
     &    'c make a new database','d make project copy of database',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,34)
        copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If corportate database then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL(outs,'Optical properties options:',
     &    'a browse or edit this file','b select from list (N/A)',
     &    'c make a new database','d make project copy of database',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,34)
        copydef=.false.
      else

C If db exists locally toggle whether the user as asked to copy the
C default database or the current database.
        idno=1
        isw=0
        call MENUATOL(outs,'Optical properties options:',
     &    'a browse or edit this file','b select from list (N/A)',
     &    'c make a new database','d copy the default database',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,34)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.7)copydef=.false.
      endif

C Act on the users choice. If no choice, return to main menu.
      if(isw.eq.0)then
        return
      elseif(isw.eq.1)then

C If file exists read it and enter editing facility.
        if(XST)then
          LOPTDB=lltmp
          goto 44
        else
          call usrmsg('Sorry there is no file to browse/edit.',
     &                'Please use one of the other options. ','W')
          goto 50
        endif
      elseif(isw.eq.2)then

C There is no select by list for optics database.
        goto 50
      elseif(isw.eq.3)then

C No optics db found so set-up minimal common block, create
C a new db, populate it and present editing facility. Use the
C shorter string ltmp for this (easkf cancel option needs to
C be explored).
        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.opticsdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.opticsdb'
        endif
        H(1)='An optics db holds properties of transparent'
        H(2)='constructions '
        clkok=.false.
 352    CALL EASKSCMD(ltmp,' Optics database?',
     &    ' ','cancel',clkok,72,'optics.db','optics database',IER,2)
        if(clkok) return
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          write(LOPTDB,'(a)') ltmp(1:lnblnk(ltmp))
        else
          goto 352
        endif

C Create a minimal optics databases (code similar to edoptic.F).
        moddb=.true.
        CALL EFOPSEQ(IOPTDB,LOPTDB,4,IER)
        write(IOPTDB,'(a)')'# optical properties db for transparent,'
        write(IOPTDB,'(a)')'# constructions and casual gain controls'
        write(IOPTDB,'(a)')'# '
        write(IOPTDB,'(a)')'# Glazed info follows:'
        GTYPE='SC_fictit'
        GDESCR='Fictitious 99/99'
        NTL=1
        VTRN=0.99
        SRF=0.01
        SAB=0.01
        UVAL=9.0
        DG(1)=0.998
        DG(2)=0.987
        DG(3)=0.986
        DG(4)=0.985
        DG(5)=0.984
        HG(1)=0.980
        HG(2)=0.970
        HG(3)=0.960
        HG(4)=0.950
        HG(5)=0.940
        RF(1)=1.52
        AB(1,1)=0.001
        AB(1,2)=0.001
        AB(1,3)=0.001
        AB(1,4)=0.001
        AB(1,5)=0.001
        CALL EMKOPTD(IOPTDB,GTYPE,GDESCR,IER)
        CALL ERPFREE(IOPTDB,ISTAT)
        OPTKOK=.TRUE.

C Scan the new file and use the editing facility.
        if(lnblnk(LOPTDB).lt.100)then
          write(outs,'(3a)') 'Optics db ',LOPTDB(1:lnblnk(LOPTDB)),
     &     ' created.'
        else
          write(outs,'(3a)') 'Optics db ',LOPTDB(1:100),
     &     '... created.'
        endif
        dok=.true.
        h(1)='An empty optics database has been created. It is'
        h(2)='useless until it is populated with data. If you '
        h(3)='have the relevant information you can start now.'
        CALL ASKOK(outs,' Browse or edit it?',OK,dok,3)
        if(OK)then
          goto 44
        endif
      elseif(isw.eq.4.or.isw.eq.7)then

C Copy a database to project dbs folder. If iws is 4 then the
C user has asked for the default database. If isw is 7 then the
C user wishes to browse for the source file. Suggest a local
C file name based on the project root name then do the copy.
C Use LTMP (short buffer) for the file name as known to esp-r
C and LCOPY for the system cp call.   
        if(copydef)then
          write(outs248,'(2a)') 'The source file is:',
     &      DOPTDB(1:lnblnk(DOPTDB))
          call edisp248(iuout,outs248,100)
        else
          write(outs248,'(2a)') 'The current source file is:',
     &      LOPTDB(1:lnblnk(LOPTDB))
          call edisp248(iuout,outs248,100)
          lltmp=LOPTDB
          llt=lnblnk(lltmp)
          iglib = igraphiclib()
          if(iglib.eq.1.or.iglib.eq.3)then
            if(llt.lt.96)then
              CALL EASKF(lltmp,' Optical properties database?',
     &          ' ',96,DOPTDB,'optical properties database',IER,3)
            elseif(llt.ge.96.and.llt.lt.124)then
              CALL EASKF(lltmp,' Optical properties database?',
     &          ' ',124,DOPTDB,'optical properties database',IER,3)
            elseif(llt.ge.124.and.llt.le.144)then
              CALL EASKF(lltmp,' Optical properties database?',
     &          ' ',144,DOPTDB,'optical properties database',IER,3)
            endif
          elseif(iglib.eq.2)then
            CALL EASKF(lltmp,' Optical properties database?',
     &        ' ',144,DOPTDB,'optical properties database',IER,3)
          else
            CALL EASKF(lltmp,' Optical properties database?',
     &        ' ',96,DOPTDB,'optical properties database',IER,3)
          endif
          LOPTDB=lltmp
        endif
        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.opticdb'
        elseif(dbspth(1:3).eq.'../')then
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.opticdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.opticdb'
        endif
  54    CALL EASKS(LTMP,
     &    'Database name after it is copied into the project?',
     &    ' ',72,'optics.db','copied optics database',IER,2)
        call isunix(unixok)
        IF(LTMP(1:2).NE.'  ')then

C If Unix and the path is ./ then no need to prepend this (to
C avoid .//../dbs in the buffer).  If path ends with file separator
C no need to duplicat the file separator.
          if(unixok)then
            lpp=lnblnk(path)
            if(path(1:2).eq.'./')then
              write(LCOPY,'(a)') LTMP(1:lnblnk(LTMP))
            elseif(path(lpp:lpp).eq.fs)then
              write(LCOPY,'(2a)') path(1:lnblnk(path)),
     &          LTMP(1:lnblnk(LTMP))
            else
              write(LCOPY,'(3a)') path(1:lnblnk(path)),fs,
     &          LTMP(1:lnblnk(LTMP))
            endif
          else
            write(LCOPY,'(3a)') path(1:lnblnk(path)),fs,
     &        LTMP(1:lnblnk(LTMP))
          endif
          if(copydef)then
            doit = ' '
            if(unixok)then
              write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &          DOPTDB(1:lnblnk(DOPTDB)),' ',LCOPY(1:lnblnk(LCOPY))
            else

C Similar dos copy logic but using DOPTDB.
              message='getting optics db file from '
              call dblongdoscopy(DOPTDB,lcopy,message,doit,ier)
            endif
            call usrmsg('copying default file via',doit,'-')
            call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
            call FINDFIL(LCOPY,XST)
            if(XST)then
              write(LOPTDB,'(a)') LCOPY(1:lnblnk(LCOPY))
            else
              write(LOPTDB,'(a)') LTMP(1:lnblnk(LTMP))
            endif
          else
            doit = ' '
            if(unixok)then
              write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &          LOPTDB(1:lnblnk(LOPTDB)),' ',LCOPY(1:lnblnk(LCOPY))
            else

C Similar dos copy logic but using LOPTDB.
              message='getting optics db file from '
              call dblongdoscopy(LOPTDB,lcopy,message,doit,ier)
            endif
            call usrmsg('copying file via',doit,'-')
            call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
            call FINDFIL(LCOPY,XST)
            if(XST)then
              write(LOPTDB,'(a)') LCOPY(1:lnblnk(LCOPY))
            else
              call FINDFIL(LTMP,XST)
              if(XST)then
                write(LOPTDB,'(a)') LTMP(1:lnblnk(LTMP))
              else
                write(LOPTDB,'(a)') LTMP(1:lnblnk(LTMP))
                call edisp(iuout,'Problem locating the new database.')
                call edisp(iuout,'try browse ../dbs to find it.')
              endif
            endif
          endif
          moddb=.true.
          goto 44
        else
          goto 54
        endif
      elseif(isw.eq.5)then

C Browse ../dbs if GTK. Fill LCOPY with the model dbspth so user
C can see the files in that folder. Note the user may browse away
C from this location. Easkf returns the absolut path to the
C file selected (later see if this can be resolved to ../dbs). 
C Test the returned file and if it is not readable reset LOPTDB.
        if(iglib.eq.2)then
          H(1)='An optical properties database holds information for'
          H(2)='transparent constructions. It is closely related to'
          H(3)='the constructions database referenced by the project.'
          H(4)=' '
          H(5)='A local database folder (../dbs) is usually associated'
          H(6)='with each model. '
          write(LCOPY,'(2a)') dbspth(1:lnblnk(dbspth)),fs
          CALL EASKF(LCOPY,' Optical properties database?',
     &      ' ',144,DOPTDB,'optical properties database',IER,6)
          LOPTDB=LCOPY
          moddb=.true.
          goto 44
        else

C For X11 and text allow user to type in a file name.
          H(1)='An optical properties database holds information for'
          H(2)='transparent constructions. It is closely related to'
          H(3)='the constructions database referenced by the project.'
          llt=lnblnk(lltmp)
          if(llt.lt.96)then
            CALL EASKF(lltmp,' Optical properties database?',
     &        ' ',96,DOPTDB,'optical properties database',IER,3)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(lltmp,' Optical properties database?',
     &        ' ',124,DOPTDB,'optical properties database',IER,3)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(lltmp,' Optical properties database?',
     &        ' ',144,DOPTDB,'optical properties database',IER,3)
          endif
          if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then
            LOPTDB=lltmp
          else
            goto 50
          endif
          CALL ERPFREE(IOPTDB,ISTAT)
          call FINDFIL(LOPTDB,XST)
          IF(XST)THEN

C If file exists and user wants to browse then scan the file and
C use the editing facility.
            goto 44
          else
            write(outs248,'(2a)') LOPTDB(1:lnblnk(LOPTDB)),
     &        ' does not appear to exist.'
            call edisp248(iuout,outs248,100)
            goto 50
          endif
        endif
      elseif(isw.eq.6)then

C Browse distribution database folder. Setup LCOPY to point to
C the database folder (but no particular file). Use easkf to
C return a file (which might be from somewhere else if the user
C decides to do some browsing). Test this suggested file via
C call to eroptdb and if this is not successful reset LOPTDB.
        if(iglib.eq.2)then
          H(1)='An optical properties database holds information for'
          H(2)='transparent constructions. It is closely related to'
          H(3)='the constructions database referenced by the project.'
          H(4)=' '
          H(5)='The ESP-r distribution includes alternatives to the'
          H(6)='default which might be useful.'
          write(LCOPY,'(4a)') dinstpath(1:lnblnk(dinstpath)),
     &      fs,'databases',fs
          CALL EASKF(LCOPY,' Optical properties database?',
     &      ' ',144,DOPTDB,'optical properties database',IER,6)
          LOPTDB=LCOPY
          moddb=.true.
          goto 44
        else
          call usrmsg('Browse feature not available',' ','W')
          goto 50
        endif
      endif

C Test the selected or copied database. If a problem loop back
C otherwise edit the database contents.
   44 continue
      write(outs248,'(2a)') 'Accessing optics db: ',
     &  LOPTDB(1:lnblnk(LOPTDB))
      call edisp248(iuout,outs248,100)
      SOPT='ALL'
      ier=0
      call tstamp('>','PRJ: enter optical db')
      CALL EROPTDB(0,ITRU,SOPT,GDESCR,IER)
      if(ier.ne.0)then
        call usrmsg(' Problem encountered with optical db!',
     &    ' Please check database location and content.','W')
        OPTKOK=.FALSE.
        LOPTDB=lprev
        goto 50
      else
        OPTKOK=.TRUE.
        call EDOPT(IIER)
      endif

      return
      end

C ********** EDDBCLM
C EDDBCLM Changes the climate database current cfg file references.
C moddb is
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.
C ISHD is -1 if no change in shading file names are required, otherwise
C     use the APP characters.
C from version manager
      SUBROUTINE EDDBCLM(moddb,APP,ISHD)
      INCLUDE 'building.h'
      INCLUDE 'esprdbfile.h'
      common/FILEP/IFIL
      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN
      common/clmlst/cdblbl,cdblfil
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C1/NCOMP,NCON
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/appw/iappw,iappx,iappy
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      common/rpath/path
      COMMON/CLMDT0/CLAT,CLONG
      COMMON/CLMDT1/CLMLOC
      COMMON/C4/XLAT,XLON
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/deflt4/dinstpath

      CHARACTER*72 H,CDBLFIL,DFILE,LASCI,LTMP
      character longtfile*144,longtafile*144,message*48
      CHARACTER CDBLBL*20,DOIT*248,fs*1,TMODE*8,CLMLOC*30,LCOPY*72
      CHARACTER OUTS*124,APP*4,ext*4
      CHARACTER*24 ZONEPTH,NETPTH,CTLPTH,IMGPTH,RADPTH,DBSPTH
      CHARACTER*24 DOCPTH,TMPPTH
      LOGICAL CLKOK,OK,concat,COPYDEF,MODDB,XST,DOK,UNIXOK
      character cfgroot*24,LCFGF*72,linstd*60
      character dinstpath*48,path*72
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LUTIL,LTHRM,NNAME
      dimension ICLM(24,6)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      moddb=.false.

 552  LTMP=LCLIM
      H(1)='Climate databases are hourly sets of values from '
      H(2)='1h00 on 1 Jan to 24h00 on 31 December.  For each hour' 
      H(3)=' the following data is held:'
      H(4)=' '
      H(5)=' Diffuse horizontal solar intensity (W/m^2)'
      H(6)=' Dry bulb temp. (tenths of degree C ie. 102 = 10.2C)'
      H(7)=' Direct normal or global horiz. solar intensity (W/m^2)'
      H(8)=' Wind speed (tenths m/s ie. 15 = 1.5 m/s)'
      H(9)=' Wind direction (degrees from north, clockwise)'
      H(10)=' Relative humidity (percent)'
      H(11)=' '
      H(12)='The following options can be selected:'
      H(13)=' '
      H(14)='a) If the file exists than you can browse the database '
      H(15)='   and if you have permission to update the file you'
      H(16)='   can edit the entities in the database.'
      H(17)=' '
      H(18)='b) select another file: you will asked for a file'
      H(19)='   name and if it is found you can browse or edit it.'
      H(20)=' '
      H(21)='c) make a new database: a new database will be created '
      H(22)='   based on the file name you supply.'
      H(23)=' '
      H(24)='d) copy default database: a project copy of the default'
      H(25)='  database will be made and you will be offered a '
      H(26)='  chance to rename it.'
      H(27)=' '
      H(28)='e) convert binary file to an (esp-r) ascii file '
      H(29)='   (typically for transport between machine types).'
      H(30)=' '
      H(31)='f) convert ascii (esp-r) climate file to a binary'
      H(32)='   suitable to the current computer.'
      H(33)=' '
      H(34)='g) convert an EPW file (which has already been edited'
      H(35)='   as per EPW_readme instructions) to an esp-r binary'
      H(36)='   climate file'

C Just in case we got this far and ICLIM was not instanciated do it now.
      if(ICLIM.eq.0)then
        ICLIM=IFIL
      endif
      CALL ERPFREE(ICLIM,ISTAT)

C Get length of path to where esp-r databases were installed.
      write(linstd,'(2a)') dinstpath(1:lnblnk(dinstpath)),
     &    '/databases'
      linst=lnblnk(linstd)
      call FINDFIL(LTMP,XST)
      IF(.NOT.XST)THEN
        idno=2
        isw=0
        call MENUATOL(outs,'Climate data options:',
     &      'a browse/edit (NOT APPLICABLE)','b select another file',
     &      'c make a new database','d copy the default database',
     &      ' ',' ',' ',' ',' ',' ',' ',' ',isw,idno,36)
        copydef=.true.
      elseif(LTMP(1:linst).eq.linstd(1:linst))then

C If corportate database then offer the following choices:
        idno=1
        isw=0
        call MENUATOL(outs,'Climate data options:',
     &    'a browse or edit this file','b select another file',
     &    'c make a new database','d make project copy of database',
     &    'e binary >> ascii export','f ascii >> binary import',
     &    'g EPW >> binary import',' ',' ',' ',' ',' ',isw,idno,36)
        copydef=.false.
      else

C If db exists locally offer the following choices:
        idno=1
        isw=0
        call MENUATOL(outs,'Climate data options:',
     &      'a browse or edit this file','b select another file',
     &      'c make a new database','d copy the default database',
     &      'e binary >> ascii export','f ascii >> binary import',
     &      'g EPW >> binary import',' ',' ',' ',' ',' ',isw,idno,36)
        copydef=.true.
      endif

C Act on the users choice. If no choice return to main menu.
      if(isw.eq.0)then
        RETURN
      elseif(isw.eq.1)then

C If file exists read it and enter editing facility.
        if(XST)then
          IER=0
          call EFOPRAN(ICLIM,LTMP,144,0,IER)
          if(ier.eq.0)then
            moddb=.true.
            LCLIM=LTMP
            CALL USRMSG(' Climate data opened.',' ','P')
          else
            call usrmsg('Problem encountered with climate data!',
     &        'Please check database location and content.','W')
            goto 552
          endif
        else
          call usrmsg('Sorry there is no file to browse/edit.',
     &                  'Please use one of the other options. ','W')
          goto 552
        endif
        CALL ERPFREE(ICLIM,ISTAT)

C Get logical name of child process terminal type, expand climate 
C name to include the path and create a string to drive clm.
        call tstamp('>','PRJ: start clm')
        doit = ' '
        call tchild(ICPMOD)
        call termode(ICPMOD,tmode)
        call addpath(LCLIM,longtfile,concat)
        if(iappw.gt.0.and.iappw.le.200)then
          write(doit,'(3a,3i4,2a)') 'clm -mode ',tmode,
     &        ' -s ',iappw,iappx+10,iappy+10,' -file ',
     &        longtfile(1:lnblnk(longtfile))
        else
          write(doit,'(4a)') 'clm -mode ',tmode,' -file ',
     &        longtfile(1:lnblnk(longtfile))
        endif
        CALL ASKOK(' ','Browse/edit new climate data?',OK,dok,3)
        if(ok)then
          call edisp(iuout,' Invoking climate analysis facility.')
          call edisp(iuout,doit)
          call runit(doit,tmode)
        endif
      elseif(isw.eq.2)then
        H(1)='Climate databases are hourly sets of values from '
        H(2)='1h00 on 1 Jan to 24h00 on 31 December. Please' 
        H(3)='specify which file in the list.'
        iuf=IFIL+1
        INQUIRE (FILE=cdblfil,EXIST=XST)
        if(XST)then
          call rdblist(IUF,cdblfil,LTMP,ier)
          if(ier.eq.0)then
            if(LTMP(1:12).eq.'USER_DEFINED')then
              LTMP='newclim'
 291          CALL EASKS(LTMP,' New climate file (esp-r binary) ?',
     &         ' ',72,DCLIM,'new climate file',IER,15)
              IF(LTMP.EQ.' ')GOTO 291
              IER=0
            elseif(LTMP.eq.'UNKNOWN'.or.LTMP(1:2).eq.'  ')then
              call usrmsg(' Nothing selected from the list ',
     &                    ' no change made...','W')
            else
              LCLIM=LTMP
            endif
          endif
        else
          call usrmsg(
     &       'No `climate db` list available so using default...',
     &       '(Check with your administrator about `climatelist` file)',
     &       'W')
          goto 552
        endif

        CALL ERPFREE(ICLIM,ISTAT)
        call FINDFIL(LCLIM,XST)
        IF(XST)THEN

C If file exists and user wants to browse then scan the file and
C use the editing facility.
          IER=0
          call EFOPRAN(ICLIM,LTMP,144,0,IER)
          if(ier.eq.0)then
            write(outs,'(2a)') 'Accessing ',LCLIM(1:lnblnk(LCLIM))
            call edisp(iuout,outs)
            call tstamp('>','PRJ: start clm')
            doit = ' '
            call tchild(ICPMOD)
            call termode(ICPMOD,tmode)
            call addpath(LCLIM,longtfile,concat)
            if(iappw.gt.0.and.iappw.le.200)then
              write(doit,'(3a,3i4,2a)') 'clm -mode ',tmode,
     &            ' -s ',iappw,iappx+10,iappy+10,' -file ',
     &            longtfile(1:lnblnk(longtfile))
            else
              write(doit,'(4a)') 'clm -mode ',tmode,' -file ',
     &            longtfile(1:lnblnk(longtfile))
            endif
            CALL ASKOK(' ','Browse/edit selected climate data?',
     &                  OK,dok,3)
            if(ok)then
              call edisp(iuout,' Invoking climate analysis facility.')
              call edisp(iuout,doit)
              call runit(doit,tmode)
            endif
          else
            call usrmsg('Problem encountered with climate data!',
     &          'Please check database location and content.','W')
            goto 552
          endif
        else
          call usrmsg('Sorry there is no file to browse/edit.',
     &                'Please use one of the other options. ','W')
          goto 552
        endif
      elseif(isw.eq.3)then

C No climate db found so set-up minimal common block, create
C a new db, populate it and present editing facility.
        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.climate'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &        cfgroot(1:lr),'.climate'
        endif
        clkok=.false.
 553    CALL EASKSCMD(LTMP,' Climate database?',
     &      ' ','cancel',clkok,72,DCLIM,'climate database',IER,2)
        if(clkok) RETURN
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          LCLIM=ltmp
        else
          goto 553
        endif

C Create a blank climate file (code similar to clm.F).
        moddb=.true.
        clmloc='new site'
        ID1=1
        IM1=1
        IT1=1
        ID2=31
        IM2=12
        IT2=24
        IDS=1
        IDF=365
        IYEAR=2006
        CLAT=50.
        CLONG=0.
        IDNGH=0
        IER=0
        call EFOPRAN(ICLIM,LCLIM,144,4,IER)

C Loop through days and zero the data to be written.
        DO 101 I=IDS,IDF
          IDD=I
          DO 201 J=1,24
            ICLM(J,1)=0
            ICLM(J,2)=0
            ICLM(J,3)=0
            ICLM(J,4)=0
            ICLM(J,5)=0
            ICLM(J,6)=50
  201     CONTINUE

C Transfer this to the binary file.
          IREC=IDD
          WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1002)
     &     ((ICLM(J,K),K=1,6),J=1,24)
  101   CONTINUE

C Insert Climatic Year at tail of file and close.
        IREC=366
        IADN=IDNGH
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)IYEAR,IADN
        IREC=367
        WRITE(ICLIM,REC=IREC)CLMLOC
        IREC=368
        WRITE(ICLIM,REC=IREC)CLAT,CLONG
        CALL ERPFREE(ICLIM,ISTAT)

        dok=.true.
        h(1)='An empty climate file has been created. It can`t'
        h(2)='be used for anything until it is populated with '
        h(3)='climate data.  '
        CALL ASKOK(' ','Browse/edit new climate data?',OK,dok,3)
        IF(OK)then

C Get logical name of child process terminal type, expand climate 
C name to include the path and create a string to drive clm.
          call tstamp('>','PRJ: start clm')
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          call addpath(LCLIM,longtfile,concat)
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,2a)') 'clm -mode ',tmode,
     &          ' -s ',iappw,iappx+10,iappy+10,' -file ',
     &          longtfile(1:lnblnk(longtfile))
          else
            write(doit,'(4a)') 'clm -mode ',tmode,' -file ',
     &          longtfile(1:lnblnk(longtfile))
          endif
          CALL ASKOK(' ','Browse/edit new climate data?',OK,dok,3)
          if(ok)then
            call edisp(iuout,' Invoking climate analysis facility.')
            call edisp(iuout,doit)
            call runit(doit,tmode)
          endif
        endif
      elseif(isw.eq.4)then

C Copy the standard db to project folder. Suggest a local
C file name based on the project root name. Lastly do the copy.
C If the default is to be copied ask for local name. Use LTMP for
C the file name as known to esp-r and LCOPY for the system cp call.   
        if(copydef)then
          call edisp(iuout,'The source file is:')
          call edisp(iuout,DCLIM)
        else
          call edisp(iuout,'The source file is:')
          call edisp(iuout,LCLIM)
        endif
        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.climate'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &        cfgroot(1:lr),'.climate'
        endif
 554    CALL EASKS(LTMP,
     &      'Database name after it is copied into the project?',
     &      ' ',72,'climate.db','copied climate database',IER,2)
        call isunix(unixok)
        IF(LTMP(1:2).NE.'  ')then
          write(LCOPY,'(3a)') path(1:lnblnk(path)),fs,
     &        LTMP(1:lnblnk(LTMP))
          if(copydef)then
            doit = ' '
            if(unixok)then
              write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &          DCLIM(1:lnblnk(DCLIM)),' ',LCOPY(1:lnblnk(LCOPY))
            else

C Similar logic but using DCLIM.
              message='getting climate file from '
              call dbdoscopy(DCLIM,lcopy,message,doit,ier)
            endif
            call usrmsg('copying default file via',doit,'-')
            call runit(doit,'-')
            LCLIM=LTMP
          else
            doit = ' '
            if(unixok)then
              write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &          LCLIM(1:lnblnk(LCLIM)),' ',LCOPY(1:lnblnk(LCOPY))
            else

C Similar logic but using LCLIM.
              message='getting climate file from '
              call dbdoscopy(LCLIM,lcopy,message,doit,ier)
            endif
            call usrmsg('copying file via',doit,'-')
            call runit(doit,'-')
            LCLIM=LTMP
          endif
          moddb=.true.
          write(outs,'(2a)') 'Accessing climate file: ',
     &        LCLIM(1:lnblnk(LCLIM))
          call edisp(iuout,outs)
          call tstamp('>','PRJ: start clm')
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          call addpath(LCLIM,longtfile,concat)

C Clm is started up in forground mode because when it was starting
C up in background mode the questions about whether to use its
C latitude & longitude and year pop-up over the climate module.
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,2a)') 'clm -mode ',tmode,
     &          ' -s ',iappw,iappx+10,iappy+10,' -file ',
     &          longtfile(1:lnblnk(longtfile))
          else
            write(doit,'(4a)') 'clm -mode ',tmode,' -file ',
     &          longtfile(1:lnblnk(longtfile))
          endif
          CALL ASKOK(' ','Browse/edit copied climate data?',OK,dok,3)
          if(ok)then
            call edisp(iuout,' Invoking climate analysis facility.')
            call edisp(iuout,doit)
            call runit(doit,tmode)
            call edisp(iuout,' Climate analysis facility complete.')
          endif
        else
          goto 554
        endif
      elseif(isw.eq.5)then

C Climate binary >> ascii export.
C Confirm suggested name and then write out asci file.
        H(1)='Some databases are held as BINARY files for speed'
        H(2)='of access and security. It is sometimes convenient'
        H(3)='to convert to ASCII - e.g. for implementation under'
        H(4)='a different operating system or for word processing. '
        DFILE='./climate.a'
        clkok=.false.
        LASCI=' '
        write(LASCI,'(2a)') LCLIM(1:lnblnk(LCLIM)),'.a'
        CALL EASKSCMD(LASCI,' Climate data (ASCII)?',
     &     ' ','cancel',clkok,72,DFILE,'climate data (ascii)',IER,4)
        if(clkok)then
          RETURN
        else

C Convert both binary and asci file names into full paths prior
C to asking clm to do the conversion.
          call addpath(LCLIM,longtfile,concat)
          call addpath(LASCI,longtafile,concat)
          write(outs,'(2a)') 'Converting climate data: ',
     &        LCLIM(1:lnblnk(LCLIM))
          call edisp(iuout,outs)
          call tstamp('>','PRJ: start clm for conversion')
          doit = ' '
          write(doit,'(5a)') 'clm -mode text -file ',
     &        longtfile(1:lnblnk(longtfile)),' -act bin2asci silent ',
     &        longtafile(1:lnblnk(longtafile))
          CALL ASKOK(' ','Browse/edit exported climate data?',OK,dok,3)
          if(ok)then
            call edisp(iuout,' Invoking climate analysis facility.')
            call edisp(iuout,doit)
            call runit(doit,tmode)
            call edisp(iuout,' Climate conversion complete.')
          endif
        endif
      elseif(isw.eq.6)then

C Climate ascii >> binary import
C Confirm suggested name and then write out binary file.
        H(1)='Some databases are held as BINARY files for speed'
        H(2)='of access and security. There are machine dependant'
        H(3)='and can be created from ASCII file versions.'
        DFILE='./climate.a'
        clkok=.false.
        LASCI=' '
        write(LASCI,'(2a)') LCLIM(1:lnblnk(LCLIM)),'.a'
        CALL EASKSCMD(LASCI,' Climate data (ASCII)?',
     &     ' ','cancel',clkok,72,DFILE,'climate data (ascii)',IER,3)
        if(clkok) RETURN

        ltmp=LCLIM
        CALL EASKSCMD(ltmp,' Climate data (binary)?',
     &     ' ','cancel',clkok,72,DFILE,'climate data (binary)',IER,3)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          LCLIM=ltmp
        endif
        if(clkok)then
          RETURN
        else

C Convert both binary and asci file names into full paths prior
C to asking clm to do the conversion.
          call addpath(LCLIM,longtfile,concat)
          call addpath(LASCI,longtafile,concat)
          write(outs,'(2a)') 'Converting climate data: ',
     &        LCLIM(1:lnblnk(LCLIM))
          call edisp(iuout,outs)
          call tstamp('>','PRJ: start clm for conversion')
          doit = ' '
          write(doit,'(5a)') 'clm -mode text -file ',
     &        longtfile(1:lnblnk(longtfile)),' -act asci2bin silent ',
     &        longtafile(1:lnblnk(longtafile))
          CALL ASKOK(' ','Browse/edit exported climate data?',OK,dok,3)
          if(ok)then
            call edisp(iuout,' Invoking climate analysis facility.')
            call edisp(iuout,doit)
            call runit(doit,tmode)
            call edisp(iuout,' Climate conversion complete.')
          endif
        endif
      elseif(isw.eq.7)then

C EPW >> binary import
C Confirm suggested name and then write out binary file.
        H(1)='ESP-r can import climate data from EPW format files'
        H(2)='(some editing is required, see instructions in the'
        H(3)='file EPW_readme). This option converts EPW files'
        H(4)='to ESP-r climate files.'
        DFILE='./climate.a'
        clkok=.false.
        LASCI=' '
        write(LASCI,'(2a)') LCLIM(1:lnblnk(LCLIM)),'.a'
        CALL EASKSCMD(LASCI,' Climate data (EPW)?',
     &     ' ','cancel',clkok,72,DFILE,'climate data (EPW)',IER,4)
        if(clkok) RETURN

        ltmp=LCLIM
        CALL EASKSCMD(ltmp,' Climate data (esp-r binary)?',
     &     ' ','cancel',clkok,72,DFILE,'climate data (binary)',IER,4)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          LCLIM=ltmp
        endif
        if(clkok)then
          RETURN
        else

C Convert both binary and asci file names into full paths prior
C to asking clm to do the conversion.
          call addpath(LCLIM,longtfile,concat)
          call addpath(LASCI,longtafile,concat)
          call tstamp('>','PRJ: start clm for conversion')
          doit = ' '
          write(doit,'(5a)') 'clm -mode text -file ',
     &        longtfile(1:lnblnk(longtfile)),' -act epw2bin silent ',
     &        longtafile(1:lnblnk(longtafile))
          h(1)='Use the browse function to clarify whether the climate'
          h(2)='data you have selected is appropriate for the current '
          h(3)='model. '
          dok=.false.
          CALL ASKOK(' ','Browse/edit imported climate data?',OK,dok,3)
          if(ok)then
            call edisp(iuout,' Invoking climate analysis facility.')
            call edisp(iuout,doit)
            call runit(doit,tmode)
            call edisp(iuout,' Climate conversion complete.')
          endif
        endif
      endif

C Confirm with user about update Site latitude and longitude, recover
C clat and clong. First check if climate file exists: if so open,
C otherwise create with 0 length.
      CALL ERPFREE(ICLIM,ISTAT)
      CALL FPRAND(ICLIM,ISTAT,144,0,LCLIM)
      if(ISTAT.ge.0)then
        CALL ERPFREE(ICLIM,ISTAT)
        CALL FPRAND(ICLIM,ISTAT,144,1,LCLIM)
        call edisp(iuout,'Opened climate to collect information...')
        IREC=366
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)ICYEAR,ICDNGH
        IREC=368
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLAT,CLONG
        CALL ERPFREE(ICLIM,ISTAT)
      endif
      write(outs,'(A,F7.3,A,F7.3,a,2F7.3,a)')
     &   'The climate lat is ',clat,
     &   ' and longitude diff is ',clong,
     &   '. The model currently uses ',xlat,xlon,'.'
      dok=.false.
      h(1)='Normally the project location should not deviate much'
      h(2)='from the location of climate data. '
      h(3)=' '
      h(4)='IF YOU CHANGE the model location remember to save '
      h(5)='your model before you exit the project manager. '
      CALL ASKOK(outs,
     &  'Update your model with climate data? (see help)',OK,DOK,5)
      IF(OK)THEN
        XLAT=CLAT
        XLON=CLONG
        WRITE(OUTS,'(A,F6.2,A,F6.2,A)')'A latitude of ',xlat,
     &    ' and longitude diff of ',xlon,
     &    ' is associated with the model.'
        CALL EDISP(IUOUT,OUTS)
      ENDIF

C Update year 
      h(1)='The year associated with a model primarily tells the'
      h(2)='simulator what day of the week is 1 January. '
      h(3)=' '
      h(4)='IF YOU CHANGE the model year remember to save '
      h(5)='your model before you exit the project manager. '
      dok=.false.
      WRITE(OUTS,'(A,I4,a,I4,a)')'The climate year is ',ICYEAR,
     &  '. and the model year is ',IYEAR,'.'      
      CALL ASKOK(OUTS,
     &  'Use climate year in your model? (see help)',OK,DOK,5)
      IF(OK)THEN
        IYEAR=ICYEAR
        WRITE(OUTS,'(I4,A)')IYEAR,' will be used as year of simulation'
        CALL EDISP(IUOUT,OUTS)
      ENDIF        

C Change name of shading files in common block if present only if called
C from version manager
      IF(ISHD.NE.-1)THEN
        do 42 iz=1,ncomp
          if(ISI(iz).eq.1)then
            call FINDFIL(LSHAD(iz),XST)
            if(XST)then
              EXT='.shd'
              CALL FNCNGR(LSHAD(IZ),APP,EXT,NNAME)
              LSHAD(IZ)=NNAME
              ISHD=1
            endif
          endif
  42    continue
      ENDIF
      RETURN

 1000 WRITE(outs,774)ISTAT
  774 FORMAT(' Error ',I7,' reading climate database information')
      call edisp(iuout,outs)
      RETURN

 1001 WRITE(outs,775)ISTAT
  775 FORMAT(' Error ',I7,' writing climate database year')
      call edisp(iuout,outs)
      RETURN

 1002 WRITE(outs,777)ISTAT
  777 FORMAT(' Error ',I7,' writing climate database')
      call edisp(iuout,outs)
      
      RETURN
      END
