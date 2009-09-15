C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-2007.

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
C EDDBMAT Changes the materials database current cfg file references.
C EDDBMLC Changes the Constructions db references in the cfg file.
C EDDBPROFILE Changes the events profile database current cfg file references.
C EDDBOPT Changes the optical database current cfg file references.
C EDDBCLM: Changes the climate database current cfg file references.

C ********** EDDB 
C Controls specification/ browsing of databases.
C ITRU is channel for user output, IER=0 means no errors encountered.

      SUBROUTINE EDDB(IER)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "MultiYear_simulations.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure databases)
C LOPTDB,IOPTDB (for optical database)
C LPCDB/IPCDB (for plant template database)
C LPRFDB,IPRODB (for event profile database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
C ICLIM,LCLIM (for climate file)
C MCMPDBFL (miscellaneous components database)
C default file names for databases
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      common/rpath/path
      common/user/browse

C Default esp-r distribution path.
      common/deflt4/dinstpath

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      common/texted/tedlbl,teditor

C Defaults for some of the obscure files.
      character*72 DFCFD,DECMPDBFL,DMCMPDBFL,DICONDBFL,dmdbnam
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DMCMPDBFL,DICONDBFL,dmdbnam

      logical OK,DOK,XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical moddb,browse,modcon,QUIET,concat
      logical haveconstr
      logical unixok  ! to check for database path file separators

      character H*72,path*72
      character*72 ITEMS(16)
      character dinstpath*48
      character longtfile*144,llitem*144,longtfiledos*144
      character doit*248,tedlbl*20,teditor*20
      character tmode*8
      
C Strings for dos version of source and destination of db file.
C      character rootpdos*72,upathdos*72

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
      
      WRITE(ITEMS(1),'(2A)')'  model path: ',path(1:56)
      ITEMS(2) ='  _______________________________ '
      WRITE(ITEMS(3),'(2A)')'a annual climate         : ',LCLIM(1:44)
      WRITE(ITEMS(4),'(2A)')'b multi-year climate     : ',cTemp(1:44)
      if(ipathapres.eq.0.or.ipathapres.eq.1)then
        WRITE(ITEMS(5),'(2A)')'c pressure distributions : ',LAPRES(1:44)
      elseif(ipathapres.eq.2)then
        WRITE(ITEMS(5),'(2A)')'c std pressure distrib   : ',LAPRES(1:44)
      endif
      WRITE(ITEMS(6),'(2A)')'d materials              : ',LFMAT(1:44)
      WRITE(ITEMS(7),'(2A)')'e constructions          : ',LFMUL(1:44)
      if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
        WRITE(ITEMS(8),'(2A)') 'f plant components       : ',LPCDB(1:44)
      elseif(ipathpcdb.eq.2)then
        WRITE(ITEMS(8),'(2A)') 'f std plant components   : ',LPCDB(1:44)
      endif
      WRITE(ITEMS(9),'(2A)')'g event profiles         : ',LPRFDB(1:44)
      if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
        WRITE(ITEMS(10),'(2A)')'h optical properties     : ',
     &    LOPTDB(1:44)
      elseif(ipathoptdb.eq.2)then
        WRITE(ITEMS(10),'(2A)')'h std optical properties : ',
     &    LOPTDB(1:44)
      endif
      WRITE(ITEMS(11),'(A)')   'i mould & mycotoxins     : None'
      WRITE(ITEMS(12),'(2A)')  'j miscel components      : ',
     &  MCMPDBFL(1:44)
      ITEMS(13)='  _______________________________ '
      ITEMS(14)='? help                            '
      ITEMS(15)='- exit this menu                  '

C Try to compact the width of the menu.
      IW=0
      do 141 ij=1,15
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
        CALL VWMENU('Database Maintenance',ITEMS,15,0,0,IW,irpx,irpy,
     &               INO)
      else
        CALL EMENU('Database Maintenance',ITEMS,15,INO)
      endif
      IF(INO.EQ.15)THEN
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
        call eddbclm(moddb,'      ',-1)

      ELSEIF(INO.eq.4)THEN

C Multi-year climate database - only works in conjunction
C with save level 5.
         call usrmsg('The use of multi-year assessments is',
     &               'limited to results save level 5.','W')
         call MY_clm_db_menu()
        
      ELSEIF(INO.EQ.5)THEN

C Pressure distribution db.
        call eddbpres(moddb,'    ')

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
     &      'The database name or details have changed. If you')
          call edisp(iuout,
     &      'want to keep it use the `save model` option.')
          call edisp(iuout,' ')
        endif
      ELSEIF(INO.EQ.9)THEN

C Profiles database.
        call eddbprofile(moddb,'    ')
        if(moddb)then
          call edisp(iuout,
     &      'The database name or details have changed. If you')
          call edisp(iuout,
     &      'want to keep it use the `save model` option.')
          call edisp(iuout,' ')
        endif
      ELSEIF(INO.EQ.10)THEN

C Optical database.
        call eddbopt(moddb,'    ')
        if(moddb)then
          call edisp(iuout,
     &      'The database name or details have changed. If you')
          call edisp(iuout,
     &      'want to keep it use the `save model` option.')
          call edisp(iuout,' ')
        endif
      ELSEIF(INO.EQ.11)THEN

C Place holder for mould database
         call UsrMsg(
     &   'Project Manager does not yet invoke the mould',
     &   'db manager. Start `mld` manually.','W')
      ELSEIF(INO.EQ.12)THEN

C Special materials/miscel components database.
C Use file browser if in GTK mode, in X11 user must type file name.
C If vi then spawn a new window to do the editing in.
        H(1)='The Special materials/miscel components database can'
        H(2)='be edited using the current defined text editor'
        H(3)='(as defined in the default file).'
        H(4)='BE CAREFUL!'
	llitem=' '    ! reset return string
        CALL EASKXORGTKF(MCMPDBFL,
     &    'Special materials/misc. comp file name?',' ',
     &    DMCMPDBFL,llitem,'Special materials file',IER,4)
     
C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 3  ! redisplay menu.
        endif

C Depending on whether Unix or DOS based setup paths. If DOS
C then check for spaces in name and change / to \.
        call isunix(unixok)
        if(unixok)then
          call addpath(llitem,longtfile,concat)
        else
          call addpath(llitem,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        tmode='graph'
        if(teditor(1:2).eq.'vi')tmode='text'
        write(doit,'(a,2x,a)') teditor(1:lnblnk(teditor)),
     &    longtfile(1:lnblnk(longtfile))
        call runit(doit,tmode)

        h(1)='Exercise care when editing this text file as the'
        h(2)='format may be sensitive to user edits! '
        dok=.false.
        CALL ASKOK('  ','Use this edited database?',OK,dok,2)
        if(OK)then
          write(MCMPDBFL,'(a)') llitem(1:lnblnk(llitem))
          moddb=.true.   ! set so user is asked to update cfg file
        endif

      ELSEIF(INO.EQ.14)THEN

C Menu help text.
        H(1)='Information on each databases is provided.'
        call easkatog(' ','Help options:','overview','climate',
     &   'materials','constructions','optics','plant','cancel',ihd,1)
        if(ihd.eq.1)then
          H(1)='ESP-r is provided with a number of standard databases'
          H(2)='and these are located in `/usr/esru/esp-r/databases`.'
          H(3)='Where a model requires a bespoke version of a'
          H(4)='database, the user may either copy a standard database'
          H(6)='into a local directory and modify it or create a new'
          H(7)='database from scratch.'
          CALL PHELPD('databases overview',7,'-',0,0,IER)
        elseif(ihd.eq.2)then
          H(1)='Climate databases contain hourly parameters over the'
          H(2)='period 1h00 on 1 Jan to 24h00 on 31 December (i.e. no'
          H(3)='leap years). Where data is supplied for only one or' 
          H(4)='more parts of this period, the remaining data will be' 
          H(5)='set to zero. The parameters required for each hour are' 
          H(6)='as follows.'
          H(7)=' '
          H(8)=' 1. Diffuse horizontal solar intensity (W/m^2).'
          H(9)=' 2. Dry bulb temperature (tenths degC, i.e. 102 ='
          H(10)='   10.2degC).'
          H(11)=' 3. Direct normal solar intensity (W/m^2). As an'
          H(12)='    alternative, global horizontal intensity may be'
          H(13)='    used in which case a corresponding db flag must'
          H(14)='    be set.'
          H(15)=' 4. Wind speed (tenths m/s, i.e. 15 = 1.5 m/s).'
          H(16)=' 5. Wind direction (clockwise degrees from North.'
          H(17)=' 6. Relative humidity (%).'
          H(18)=' '
          H(19)='When using these climate parameters ESP-r will'
          H(20)='assume that the data represents the one hour period'
          H(21)='centered on the hour in question. Where sub hour'
          H(22)='simulation time steps are used, interpolation is'
          H(22)='applied.'
          H(23)=' ' 
          H(24)='The database is held in binary format to allow random' 
          H(25)='access. Conversion to an ASCII format is supported to'
          H(26)='allow transfer between different operating systems.' 
          CALL PHELPD('climate overview',26,'clm_dbitems ',0,0,IER)
        elseif(ihd.eq.3)then
          H(1)='The Materials database holds the density, specific' 
          H(2)='heat, conductivity, diffusion resistance and surface'
          H(3)='absorptivity and emissivity of products such as brick,' 
          H(4)='concrete, metal, wood and so on.' 
          H(5)=' ' 
          H(6)='The database is held in binary format to allow random' 
          H(7)='access. Conversion to an ASCII format is supported to'
          H(8)='allow transfer between different operating systems.' 
          CALL PHELPD('materials db overview',8,'materials_db',0,0,IER)
        elseif(ihd.eq.4)then
          H(1)='The Constructions database defines the composition of'
          H(2)='walls, floors, roofs etc in terms of layer ordering,'
          H(3)='thickness and material. Where the construction is' 
          H(4)='transparent a named set of optical properties, as held'
          H(5)='in the Optical Properties database, is associated.'
          CALL PHELPD('constr db overview',5,'mlc_database',0,0,IER)
        elseif(ihd.eq.5)then
          H(1)='The Optical Properties database holds the solar'
          H(2)='transmissivity and absorptivity for each layer within'
          H(3)='a transparent construction and for 5 angles of' 
          H(4)='incidence - 0 (normal), 40, 55, 70 and 80 degrees.' 
          CALL PHELPD('optics overview',4,'optical_db',0,0,IER)
        elseif(ihd.eq.6)then
          H(1)='The Plant Ccomponents database holds data defining'
          H(2)='individual plant components for assembly into networks'
          H(3)='defining particular systems of arbitrary complexity.'
          H(4)=' ' 
          H(5)='The database is held in binary format to allow random' 
          H(6)='access. Conversion to an ASCII format is supported to'
          H(7)='allow transfer between different operating systems.' 
          CALL PHELPD('plant db overview',7,'plant_db_fld',0,0,IER)
        endif
      ELSE
        INO=-4
        GOTO 3
      ENDIF
C      call usrmsg(' ',' ','-')
      INO=-4
      GOTO 3

      END

C ********** opendb
C NOTE: if this code change - be sure to update the copy of this
C subroutine embedded within esruish/ish.F

C Open materials constructions, multi-layer constructions and
C optical properties databases. In the case of materials, first
C assume it is a binary file, check its contents and if a problem
C then scan new ascii format and if that does not work try the
C older ascii materials file to fill the materials data arrays.

      subroutine opendb(ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Material data arrays.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      logical    XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,close
      DIMENSION ICLN(30)
      character H*72,LASCI*72,DFILE*72
      character SOPT*12,outs*124,GDESCR*36
      character t144*144   ! for use with erprcdb

      IER=0

C Open currently named Materials database.  Note that
C by a minor rewrite of ICLN in rec 601 we can get by
C with changing this to 24 words wide.
      CALL ERPFREE(IFMAT,ISTAT)
      call EFOPRAN(IFMAT,LFMAT,40,0,IER)
      IF(ier.ne.0)THEN
        write(outs,'(3a)') ' Materials db ',
     &     LFMAT(1:lnblnk(LFMAT)),' not found!'
        call edisp(iuout,outs)
        call edisp(iuout,' ')
        MATDBOK=.FALSE.
      else

C Check to see if the file has correct info. 
        IREC=601
        READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NCLASS,
     &    (ICLN(I),I=1,30)
        if(NCLASS.LE.0)then
          goto 1000   ! no classifications so try alternatives
        else
          MATDBOK=.TRUE.
          matver = 0.0
          goto 139  !  a binary file found so carry on.
        endif
 1000   CALL ERPFREE(IFMAT,ISTAT)  ! if a read error look at alternatives.
        call rascimat(IFMAT,LFMAT,IER)  ! try current ascii format
        if(ier.eq.-2)then
          ier=0
          call rlegacymat(IFMAT,LFMAT,ier)  ! try older ascii format
          if(ier.ne.0)then
            call usrmsg('No readable materials database was not found',
     &                  'or file was corrupt','W')
            MATDBOK=.FALSE.
          else
            call eclose(matver,1.1,0.001,close)
            if(close)then
              MATDBOK=.TRUE.
            else
              call usrmsg(
     &          'No readable Materials database was found!',
     &          ' ','W')
              MATDBOK=.FALSE.
            endif
          endif
        elseif(ier.eq.0)then
          call eclose(matver,1.1,0.001,close)
          if(close)then
            MATDBOK=.TRUE.
          endif
        endif
      endif

        
C Read multilayer db information into common.
 139  CALL ERPFREE(IFMUL,ISTAT)
      call FINDFIL(LFMUL,XST)
      if(XST)then
        CALL ERMLDB(0,IUOUT,IER)
        IF(IER.eq.1)then
          write(outs,'(3a)') 
     &    ' Problems with materials used by Constructions db',
     &     LFMAT(1:lnblnk(LFMAT)),'!'
          call edisp(iuout,outs)
          call edisp(iuout,' ')
          MLDBOK=.FALSE.
        ELSEIF(IER.eq.2)then

C There were lots of undefined materials so likely the Materials
C database is for the wrong machine type. Guess the name of
C the ascii version and if it exists, offer to convert it or use it.
          h(1)='It is likely that the Materials db associated with'
          h(2)='this model is for use under a different operating'
          h(3)='system. ESP-r will attempt to locate an ASCII version'
          h(4)='of this database and, if successful, convert it to'
          h(5)='the required binary format if you agree.'
          IAF=IFIL+1
          LASCI=' '
          write(LASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'

C Label 155 is a jump back point for the case of users supplying a name.
  155     CALL EFOPSEQ(IAF,LASCI,1,IER)
          if(ier.eq.0)then

C If the ascii version exists there is no need to convert it.
C Ask whether to scan it into the materials arrays or convert
C to a binary file.
            CALL EASKABC(
     &        'Ascii version of Materials database found.',
     &        'Options:','use it',
     &        'convert to a binary file','cancel',IW,5)
            if(iw.eq.1)then
              call rascimat(IAF,LASCI,IER)  ! try current ascii format
              if(ier.eq.-2)then
                ier=0
                call rlegacymat(IAF,LASCI,ier)  ! try older ascii format
                if(ier.ne.0)then
                  call usrmsg('No readable Materials database was',
     &                        'found or the file was corrupt.','W')
                  MATDBOK=.FALSE.
                else
                  call eclose(matver,1.1,0.001,close)
                  if(close)then

C Materials data in place, set this as the new file name and then
C try and re-scan the Constructions database.
                    call usrmsg(
     &                'An ASCII Materials db was found and used.',
     &                'Re-scanning constructions...','P')
                    MATDBOK=.TRUE.
                    LFMAT=LASCI
                    CALL ERPFREE(IFMUL,ISTAT)
                    call FINDFIL(LFMUL,XST)
                    if(XST)then
                      CALL ERMLDB(0,IUOUT,IER)
                      if(ier.eq.0)then
                        MLDBOK=.TRUE.
                        call usrmsg(
     &                  'An ASCII Materials db was found and used.',
     &                  'Re-scanning constructions...done.','-')
                      else
                        MLDBOK=.FALSE.
                        call usrmsg(
     &                  'An ASCII Materials db was found and used.',
     &                  'Re-scanning constructions...failed.','W')
                      endif
                    endif
                  else
                    call usrmsg(
     &                'No readable Material db was found.',
     &                'Check other warnings for advice.','W')
                    MATDBOK=.FALSE.
                  endif
                endif
              elseif(ier.eq.0)then

C Materials data in place, set this as the new file name and then
C try and re-scan the constructions database.
                call usrmsg(
     &            'An ASCII Materials db was found and used.',
     &            'Re-scanning constructions...','P')
                MATDBOK=.TRUE.
                LFMAT=LASCI
                CALL ERPFREE(IFMUL,ISTAT)
                call FINDFIL(LFMUL,XST)
                if(XST)then
                  CALL ERMLDB(0,IUOUT,IER)
                  if(ier.eq.0)then
                    MLDBOK=.TRUE.
                    call usrmsg(
     &                'An ASCII Materials db was found and used.',
     &                'Re-scanning constructions...done.','-')
                  else
                    MLDBOK=.FALSE.
                    call usrmsg(
     &                'An ASCII Materials db was found and used.',
     &                'Re-scanning constructions...failed.','W')
                  endif
                endif
              endif
            elseif(iw.eq.2)then
              CALL ERPFREE(IAF,ISTAT)
              call edisp(iuout,
     &          'Attempting to convert an ASCII Materials db to a')
              call edisp(iuout,
     &          'binary version matching the Constructions db.')
              CALL MKBPRC(IAF,LASCI,IER)
              if(ier.eq.0)then
                call usrmsg(
     &            'An ASCII Materials db was found and converted.',
     &            'Re-scanning constructions...','P')
                CALL ERMLDB(0,IUOUT,IER)
                if(ier.eq.0)then
                  MLDBOK=.TRUE.
                  call usrmsg(
     &              'An ASCII Materials db was found and converted.',
     &              'Re-scanning constructions...done.','-')
                else
                  MLDBOK=.FALSE.
                  call usrmsg(
     &              'An ASCII Materials db was found and converted.',
     &              'Re-scanning constructions...failed.','W')
                endif
              else
                call usrmsg(
     &          'A Materials db was not found or was not readable.',
     &          'Please correct this before using constructions.',
     &             'W')
                MLDBOK=.FALSE.
              endif
            endif
          else

C Ask user for ascii Materials database to convert.
          H(1)='The Materials db holds the thermo-physical properties'
          H(2)='of a range of materials. An ascii version of this db'
          H(3)='may be used to transfer the contents between operating'
          H(4)='systems that have incompatible binary representations'
          H(5)='(e.g. Unix and Windows).'
            IAF=IFIL+1
            DFILE=' '
            CALL EASKS(LASCI,' ','Materials database (ASCII)?',
     &        72,DFILE,'materials db (ascii)',IER,5)
            goto 155
          endif
        else

C Scan was ok so set mldbok to true.
          MLDBOK=.TRUE.
        endif
      else

C Could not find Constructions db at this time, report to user.
        write(outs,'(3a)') 'Constructions db ',
     &     LFMUL(1:lnblnk(LFMUL)),' not found!'
        call edisp(iuout,outs)
        call edisp(iuout,' ')
        MLDBOK=.FALSE.
      endif

C Open Ooptical Properties db and read into common.
      SOPT='ALL'
      CALL EROPTDB(0,iuout,SOPT,GDESCR,IER)
      if(ier.ne.0)then
        call usrmsg('Optical Properties db not found or',
     &              'there was a problem reading it!','W')
        OPTKOK=.FALSE.
      else
        OPTKOK=.TRUE.
      endif

C Open the current Pressure Coefficients db (signal by passing
C a blank string to erprcdb).
      t144='  '
      CALL ERPRCDB(t144,0,3,IER)
      if(ier.ne.0)then
        call usrmsg('Pressure Coefficients db not found',
     &              'or there was a problem reading it!','W')
      endif

      return
      end


C ********** EDDBMAT
C EDDBMAT Changes the Materials db reference in the cfg file.
C moddb is from version manager
C modcon signals change in constructions.
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.

      SUBROUTINE EDDBMAT(moddb,modcon,APP)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN

      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C Default ESP-r distribution path.
      common/deflt4/dinstpath

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      logical XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical moddb,modcon,unixok,copydef,clkok,chgdb,close

      character H*72,LTMP*72,path*72,outs*124
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,linstd*60
      character cfgroot*24,LCFGF*72,fs*1,message*48
      character doit*248,LCOPY*72,dinstpath*48,APP*4
      character DFILE*72,LASCI*72,LTMPFL*72
      character sourcefile*72  ! the file to copy
      DIMENSION ICLN(30)

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

C If db does not exist offer the following choices.
      H(1)='The Materials database holds the density, specific' 
      H(2)='heat, conductivity, diffusion resistance and surface'
      H(3)='absorptivity/emissivity for materials such as brick' 
      H(4)='concrete, wood, metal etc. This db is held in binary' 
      H(5)='format to permit random access. The db may be converted'
      H(6)='converted to ASCII to allow transfer between operating' 
      H(7)='systems with incompatible binary representations.'
      H(8)=' '
      H(9)='The following options are available.'
      H(10)=' '
      H(11)='a) If the db exists then it may be browsed and used.'
      H(12)='   If you are the owner you may also edit the db'
      H(13)='   entries.'
      H(14)=' '
      H(15)='b) Select another db: e.g. you might copy a standard'
      H(16)='   db to your directory (option below) in order to own'
      H(17)='   it. Selecting it here then gives you edit permission.'
      H(18)=' '
      H(19)='c) Create a new db: to which you can then make material'
      H(20)='   entries as required.'
      H(21)=' '
      H(22)='d) Copy standard db: a copy of ESP-r`s standard db is'
      H(23)='   made which you own and can therefore modify.'
      H(24)=' '
      H(25)='e) Export the binary representation of the db (which is'
      H(26)='   platform dependant) to an ascii representation.'
      H(27)=' '
      H(28)='f) Import an ascii representation of the db and convert'
      H(29)='   it to a binary representation (for legacy models).'
      H(30)=' '
      H(31)='g) Import an ascii representation of the db into memory,'
      H(32)='   view/modify it and then write out to an ascii'
      H(33)='   representation.'
      CALL ERPFREE(IFMAT,ISTAT)

C Get length of path to where ESP-r databases were installed.
      write(linstd,'(3a)') dinstpath(1:lnblnk(dinstpath)),
     &  fs,'databases'
      linst=lnblnk(linstd)
      call FINDFIL(LFMAT,XST)
      IF(.NOT.XST)THEN
        idno=2
        isw=0
        call MENUATOL('  ','Materials db:',
     &    ' ','b select another',
     &    'c create new','d copy standard',
     &    'e binary >> ascii export','f ascii >> binary import',
     &    'g ascii >> memory import',' ',' ',' ',' ',' ',isw,idno,33)
          copydef=.true.
      elseif(LFMAT(1:linst).eq.linstd(1:linst))then

C If standard db.
        idno=1
        isw=0
        call MENUATOL('  ','Materials db:',
     &    'a browse/edit','b select another',
     &    'c create new','d copy standard',
     &    'e binary >> ascii export','f ascii >> binary import',
     &    'g ascii >> memory import',' ',' ',' ',' ',' ',isw,idno,33)
          copydef=.false.
      else

C If db exists locally.
        idno=1
        isw=0
        call MENUATOL('  ','Materials db:',
     &    'a browse/edit','b select another',
     &    'c create new','d copy standard',
     &    'e binary >> ascii export','f ascii >> binary import',
     &    'g ascii >> memory import',' ',' ',' ',' ',' ',isw,idno,33)
          copydef=.true.
      endif

C Act on the user's choice. If no choice return to main menu.
C This is also a jump back point if the user has supplied a
C new file name so it can be processed.
 149  if(isw.eq.0)then
        return
      elseif(isw.eq.1)then
        if(XST)then
          IER=0
          call EFOPRAN(IFMAT,LTMP,40,0,IER)
 140      if(ier.ne.0)then

C It might be an ascii version of the file.
            CALL ERPFREE(IFMAT,ISTAT)

C Attempt to scan version 1.1 ASCII file, if ier returned as -2 then
C re-read as a legacy materials file.
            call rascimat(IFMAT,LTMP,IER)
            if(ier.eq.-2)then
              ier=0
              call rlegacymat(IFMAT,LTMP,ier)
              if(ier.ne.0)then
                call usrmsg('The Materials db was not found',
     &                      'or was corrupt!','W')
                goto 138
              endif
            endif
          else

C Check to see that the file has the correct information. If read
C fails use error jump to start testing alternatives. If read is ok
C then proceed with matver still set to zero.
            IREC=601
            READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NCLASS,
     &        (ICLN(I),I=1,30)
            if(NCLASS.LE.0) goto 1000
            matver = 0.0   ! reset version number ( in case of file change)
            goto 139  ! scan was ok so proceed with subsequent binary access.

 1000       ier=-1    ! error reading record or no classes try other options
            goto 140
          endif
  139     MATDBOK=.TRUE.
          moddb=.true.
          modcon=.true.
          LFMAT=LTMP
          call tstamp('>','PRJ: enter materials db')
          call eclose(matver,1.1,0.001,close)
          if(close)then

C Offer display of material common blocks.
            CALL ELISTMAT(iw,chgdb,'M',IER)
          else
            CALL ELISTC(iw,chgdb,'M',IER)
          endif
          if(ier.eq.1)then

C The Materials db was empty so it might be an ascii version
C of the file.
            goto 138
          elseif(ier.eq.2)then

C Materials db did not scan. Could be wrong machine type.
            call usrmsg('The Materials db needs to be restored',
     &                  'from an archived ASCII version.','W')
            goto 138
          endif

C Having returned from editing/browsing the materials, offer to
C make a backup copy if the initial file was binary.
          call eclose(matver,1.1,0.001,close)
          if(close)then
            continue
          else
            if(chgdb)then

C If the materials db was edited write a backup ASCII version.
              H(1)='The binary Materials db file could get corrupted.'
              H(2)='It is good to have a backup to restore from and'
              H(3)='this is held in ascii format.'
              DFILE='./newmat.db1.a'
              clkok=.false.
              LASCI=' '
              write(LASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
              CALL EASKSCNCL(LASCI,
     &          'Materials database (ASCII backup)?',
     &          ' ','cancel',clkok,72,DFILE,'materials db (ascii)',
     &          IER,3)
              if(clkok)then
                continue
              else
                IAF=IFIL+1
                CALL MKAPRC(LASCI,IAF,IER)
              endif
            endif
            modcon=chgdb
          endif
        else
          call usrmsg('Sorry there is no db to browse/edit.',
     &                'Please select another option.','W')
          goto 138
        endif
      elseif(isw.eq.2)then
        h(1)='Please supply the name of the Materials db (including'
        h(2)='its complete path).'
 144    CALL EASKS(LTMP,' ','Materials db?',
     &    72,DFCON,'materials database',IER,2)
        IF(LTMP(1:2).NE.'  ')then
          LFMAT=LTMP
        else
          goto 144
        endif
        CALL ERPFREE(IFMAT,ISTAT)

        call FINDFIL(LFMAT,XST)
        if(XST)then
          isw = 1      ! reset isw so correct if statment will be used.
          goto 149     ! jump back and process this file name.
        else
          write(outs,'(2a)') LFMAT(1:lnblnk(LFMAT)),
     &      ' does not exist!'
          call edisp(iuout,outs)
          call edisp(iuout,' ')
          goto 138
        endif
      elseif(isw.eq.3)then

C Materials db not found so set up minimal common block, create
C a new db, populate it and offer editing.
        h(1)='The suggested name of this db is related to the'
        h(2)='current project name.'
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.materialdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.materialdb'
        endif
        clkok=.false.
  145   CALL EASKSCNCL(LTMP,' ','New Materials db?',
     &       'cancel',clkok,72,DFCON,'new material database',IER,2)
        if(clkok) return
        IF(LTMP(1:2).NE.'  ')then
          LFMAT=LTMP
        else
          goto 145
        endif
        CALL ERPFREE(IFMAT,ISTAT)
        call EFOPRAN(IFMAT,LFMAT,40,3,IER)
        if(ier.ne.0)then
          call usrmsg('Materials db could not be created!',
     &                'Check your file permissions.','W')
          goto 138
        endif
        MATDBOK=.TRUE.
        moddb=.true.
        modcon=.true.

C << Need to update this to offer choice of legacy or current
C << ascii Materials db.

C Write a scratch file which can be read in to create a binary.
C Materials db. Include typical categories with one item in each.

C << Extend logic to support newer ASCII Material db. >>

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
     &    '# Materials db defined in ',LFMAT(1:lnblnk(LFMAT))
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
        CALL MKBPRC(IAF,LTMPFL,IER)
      elseif(isw.eq.4)then

C Copy the standard db to project folder. Suggest a local
C file name based on the project root name. Lastly do the copy.

C If the default is to be copied ask for local name. Use LTMP for
C the file name as known to ESP-r and LCOPY for the system cp call.   
        call edisp(iuout,'The source file is:')
        if(copydef)then
          call edisp(iuout,DFCON)
          sourcefile = LFMAT
        else
          call edisp(iuout,LFMAT)
          sourcefile = DFCON
        endif
        call edisp(iuout,' ')
        h(1)='The suggested name of this db is related to the'
        h(2)='active project name.'
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.materialdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.materialdb'
        endif
  146   CALL EASKS(LTMP,' ',
     &    'Name for db after copy to project folder?',
     &     72,'materials.db','copied materials database',IER,2)
        call isunix(unixok)
        IF(LTMP(1:2).NE.'  ')then
          write(LCOPY,'(3a)') path(1:lnblnk(path)),fs,
     &      LTMP(1:lnblnk(LTMP))
          doit = ' '
          if(unixok)then
            write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &        sourcefile(1:lnblnk(sourcefile)),' ',
     &        LCOPY(1:lnblnk(LCOPY))
          else

C Use similar logic as DAPRES to set up copy command.
            message='getting materials db file from '
            call dbdoscopy(sourcefile,lcopy,message,doit,ier)
          endif

C Debug.
C          call lusrmsg('copying default file via',doit,'-')
          call runit(doit,'-')
          LFMAT=LTMP

C Debug.
C          write(outs,'(2a)') 'Accessing materials db: ',
C     &      LFMAT(1:lnblnk(LFMAT))
C          call edisp(iuout,outs)
          IER=0
          CALL ERPFREE(IFMAT,ISTAT)
          call EFOPRAN(IFMAT,LFMAT,40,0,IER)
          if(ier.ne.0)then
            call usrmsg(
     &        'Materials db not found or not readable! (Perhaps its',
     &        'binary representation corresponds to another OS.)','W')
            goto 138
          endif
          call tstamp('>','PRJ: enter materials db')
          CALL ELISTC(iw,chgdb,'M',IER)
          if(ier.eq.1)then

C Materials db was empty.
            goto 138
          elseif(ier.eq.2)then

C Materials db did not scan. Could be wrong machine type.
            call usrmsg('The Materials db needs to be restored',
     &                  'from an archived ASCII version.','W')
            goto 138
          endif
          modcon=chgdb

C On return from display and/or edit of materials create a back-up
C ascii version of this new Materials db.
          H(1)='This db is held as a binary representation for speed'
          H(2)='of access. It is sometimes necessary to convert to'
          H(3)='an ASCII representation, e.g. to facilitate manual'
          H(4)='editing or implementation on an operating system that'
          H(5)='uses a different binary representation. As you have'
          H(6)='requested a project copy, an ASCII backup copy will'
          H(7)='also be made.'
          DFILE='./newmat.db1.a'
          clkok=.false.
          LASCI=' '
          write(LASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
          CALL EASKSCNCL(LASCI,' ','Materials db (ascii backup)?',
     &     'cancel',clkok,72,DFILE,'materials database (ascii)',IER,7)
          if(clkok)then
            return
          else
            IAF=IFIL+1
            call eclose(matver,1.1,0.001,close)
            if(close)then
              CALL mkascimat(IAF,LASCI,IER)
            else
              CALL MKAPRC(LASCI,IAF,IER)
            endif
          endif
        else
          goto 146
        endif

      elseif(isw.eq.5)then

C Materials db: BINARY > ASCII. Open the ascii file on IAF=IFIL+1.

C << Offer option to write to either format of ascii file. >>

        IAF=IFIL+1
        IF(.NOT.MATDBOK)THEN

C If Materials db not opened do it now.
          IER=0
          call EFOPRAN(IFMAT,LFMAT,40,1,IER)
          if(ier.ne.0)return 
          MATDBOK=.TRUE.
        ENDIF

C Confirm suggested name and then write out ascii file.
        H(1)='This db is held as a binary representation for speed'
        H(2)='of access. It is sometimes necessary to convert to'
        H(3)='an ASCII representation, e.g. to facilitate manual'
        H(4)='editing or implementation on an operating system that'
        H(5)='uses a different binary representation.'
        DFILE='./newmat.db1.a'
        clkok=.false.
        LASCI=' '
        write(LASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
        CALL EASKSCNCL(LASCI,' ','Materials db (ascii)?',
     &    'cancel',clkok,72,DFILE,'materials database (ascii)',IER,5)
        if(clkok)then
          return
        else
          call eclose(matver,1.1,0.001,close)
          if(close)then
            CALL mkascimat(IAF,LASCI,IER)
          else
            CALL MKAPRC(LASCI,IAF,IER)
          endif
        endif
      elseif(isw.eq.6)then

C Materials db: ASCII > BINARY. Guess that the ascii version has
C the same name as the binary one and is in the same location.
        H(1)='An ascii (human readable) version of the Materials db'
        H(2)='may be manually edited (and then imported) or used to'
        H(3)='transfer the db to an operating system that employs a'
        H(4)='different binary representation.'
        IAF=IFIL+1
        clkok=.false.
        DFILE=' '
        LASCI=' '
        write(LASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
        CALL EASKSCNCL(LASCI,' ','Materials db (ascii)?',
     &    'cancel',clkok,72,DFILE,'materials db (ascii)',IER,4)
        if(clkok) return

        DFILE='./newprim.db1'
        clkok=.false.
        ltmp=LFMAT
 143    CALL EASKSCNCL(ltmp,' ','Materials db (binary)?',
     &    'cancel',clkok,72,DFILE,'Materials db (binary)',IER,5)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          LFMAT=ltmp
        else
          goto 143
        endif
        if(clkok)then
          return
        else
          CALL MKBPRC(IAF,LASCI,IER)
        endif
      elseif(isw.eq.7)then

C Legacy materials to common blocks.
        H(1)='An ascii (human readable) version of the Materials db'
        H(2)='may be manually edited (and then imported) or used to'
        H(3)='transfer the db to an operating system that employs a'
        H(4)='different binary representation.'
        IAF=IFIL+1
        clkok=.false.
        DFILE=' '
        LASCI=' '
        write(LASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
        CALL EASKSCNCL(LASCI,' ','Materials db (ascii)?',
     &    'cancel',clkok,72,DFILE,'materials db (ascii)',IER,4)
        if(clkok) return

C Attempt to scan version 1.1 ASCII file, if ier returned as -2 then
C re-read as a legacy materials file.
        call rascimat(IAF,LASCI,IER)
        if(ier.eq.-2)then
          ier=0
          call rlegacymat(IAF,LASCI,ier)
        endif

C Offer display of materials common blocks.
        CALL ELISTMAT(iw,chgdb,'M',IER)

        DFILE='./newmat.db1a'
        clkok=.false.
        write(ltmp,'(2a)') LASCI(1:lnblnk(LASCI)),'a1.1'
 147    CALL EASKSCNCL(ltmp,' ','Materials database (1.1 ascii)?',
     &    'cancel',clkok,72,DFILE,'Materials (1.1 ascii)',IER,5)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          continue
        else
          goto 147
        endif
        if(clkok)then
          return
        else
          CALL mkascimat(IAF,ltmp,IER)
        endif
      endif

      return

C Error messages.
   13 CALL USRMSG(' Write error to ascii Materials db ',LTMPFL,'W')
      IER=1
      return
      end

C ********** EDDBMLC
C EDDBMLC Changes the Constructions db references in the cfg file.
C moddb is from version manager
C modcon signals change in constructions.
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.

      SUBROUTINE EDDBMLC(moddb,modcon,APP)
#include "building.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN

      character DESC*48
      integer NMLC,IPR,LAYERS
      real DTHK,DRAIR
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

      character H*72,LTMP*72,path*72,outs*124
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,linstd*60
      character cfgroot*24,LCFGF*72,fs*1,message*48
      character doit*248,LCOPY*72,dinstpath*48,APP*4
      character sourcefile*72  ! the file to copy

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
        call MENUATOL('  ','Constructions db',
     &    ' ','b select another db',
     &    'c create a new db','d copy default db',
     &    ' ',' ',' ',' ',' ',' ',' ',' ',isw,idno,21)
          copydef=.true.
      elseif(LFMUL(1:linst).eq.linstd(1:linst))then

C If corportate database then offer the following choices:
        idno=1
        isw=0
        call MENUATOL('  ','Constructions db',
     &    'a browse or edit db','b select another db',
     &    'c create new db','d copy of db',
     &    ' ',' ',' ',' ',' ',' ',' ',' ',isw,idno,21)
          copydef=.false.
      else

C If db exists locally offer the following choices:
        idno=1
        isw=0
        call MENUATOL('  ','Constructions db',
     &    'a browse or edit db','b select another db',
     &    'c create a new db','d copy the default db',
     &    ' ',' ',' ',' ',' ',' ',' ',' ',isw,idno,21)
        copydef=.true.
      endif

C Act on the user's choice.
      if(isw.eq.0)then
        return
      elseif(isw.eq.1)then
        if(XST)then
          CALL ERMLDB(0,IUOUT,IER)
          IF(IER.eq.1)then

C Problem scanning the constructions db.
            call usrmsg(
     &        'There were problems scanning the Constructions db',
     &        '(and perhaps the Materials db). Please check.','W')
            goto 38
          ELSEIF(IER.eq.2)then

C There were lots of undefined materials so likely the materials
C database is for the wrong machine type. Guess the name of
C the ascii version and if it exists, offer to convert it.
            call usrmsg(
     &        'The Materials db probably needs to be',
     &        'restored from an archived ASCII version.','W')
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
          call usrmsg('No db to browse/edit. Please use one',
     &                'of the other options.','W')
          goto 38
        endif
      elseif(isw.eq.2)then
        h(1)='Please supply and confirm constructions database file'
        h(2)='name. If it exists you will be able to browse it. If '
        h(3)='you want this to be a new database the next task is to'
        h(4)='select the `make new database` option. '
  44    CALL EASKS(LTMP,' ','Constructions database?',
     &          72,DFMUL,'constructions database',IER,4)
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
          write(outs,'(2a)') 'Accessing Constructions db ',
     &       LFMUL(1:lnblnk(LFMUL))
          call edisp(iuout,outs)
          call edisp(iuout,' ')
          CALL ERMLDB(0,ITRU,IER)
          IF(IER.eq.1)then

C there was a minor problem scanning the constructions db.
            call usrmsg(
     &       'There were problems scanning the Constructions db',
     &       '(and perhaps the Materials db). Please check.','W')
            goto 38
          ELSEIF(IER.eq.2)then

C There were lots of undefined materials so likely the materials
C database is for the wrong machine type. Guess the name of
C the ascii version and if it exists, offer to convert it.
            call usrmsg(
     &        'The Materials db probably needs to be',
     &        'restored from an archived ASCII version.','W')
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
     &     ' does not exist!'
          call edisp(iuout,outs)
          call edisp(iuout,' ')
          goto 38
        endif
      elseif(isw.eq.3)then

C No Constructions db found so set-up minimal common block, create
C a new db, populate it and present editing facility.
        h(1)='The suggested name of this database is based on the'
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.constrdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.constrdb'
        endif
        clkok=.false.
   45   CALL EASKSCNCL(LTMP,' ','New constructions db name?',
     &         'cancel',clkok,72,DFMUL,'constructions database',IER,2)
        if(clkok) goto 38
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
C Note: use of easks is so that the file name can be specified
C as local to the model. If easkf was used then it would need to
C be passed the full path to the destination file.   
        call edisp(iuout,'The source file is:')
        if(copydef)then
          call edisp(iuout,DFMUL)
          sourcefile = DFMUL
        else
          call edisp(iuout,LFMUL)
          sourcefile = LFMUL
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
          doit = ' '
          if(unixok)then
            write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &        sourcefile(1:lnblnk(sourcefile)),' ',
     &        LCOPY(1:lnblnk(LCOPY))
          else

C Logic as in materials database copy command.
            message='getting constructions db file from '
            call dbdoscopy(sourcefile,lcopy,message,doit,ier)
          endif

C Debug.
C          call lusrmsg('copying default file via',doit,'-')
          call runit(doit,'-')
          LFMUL=LTMP
          write(outs,'(2a)') 'Accessing Constructions db: ',
     &      LCOPY(1:lnblnk(LCOPY))
          call edisp(iuout,outs)
          call edisp(iuout,' ')
          call tstamp('>','PRJ: enter Construction db')
          CALL ERMLDB(0,IUOUT,IER)
          IF(IER.eq.1)then

C There was a minor problem scanning the constructions db.
            call usrmsg(
     &        'There were problems scanning the Constructions db',
     &        '(and perhaps the Materials db). Please check.','W')
            goto 38
          ELSEIF(IER.eq.2)then

C There were lots of undefined materials so likely the materials
C database is for the wrong machine type. Guess the name of
C the ascii version and if it exists, offer to convert it.
            call usrmsg(
     &        'The Materials db probably needs to be',
     &        'restored from an archived ASCII version.','W')
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
#include "esprdbfile.h"
#include "plantdb.h"
      
      integer lnblnk  ! function definition

      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/C21/IFCFG,cfgroot,LCFGF
      common/appw/iappw,iappx,iappy
      common/rpath/path
      common/deflt4/dinstpath

      CHARACTER H*72,LTMP*72,lltmp*144,lprev*144
      character longtfile*144,message*48
      CHARACTER DOIT*300,fs*1,LCOPY*144
      CHARACTER OUTS*124,outs248*248,APP*4,ext*4
      CHARACTER*24 ZONEPTH,NETPTH,CTLPTH,IMGPTH,RADPTH,DBSPTH
      CHARACTER*24 DOCPTH,TMPPTH
      LOGICAL CLKOK,OK,COPYDEF,MODDB,XST,DOK,concat
      character cfgroot*24,LCFGF*72
      character dinstpath*48,path*72,tmode*8
      character sourcefile*144  ! the file to copy ??
      character dirpath*48

C Local strings for user selections dependent on graphic library.
      character opte*28,optf*28,optg*28
      character lpath*72,fname*72 ! for use with fdroot
      CHARACTER ZPDESC*25
      integer lndbp   ! for length of standard database path
      logical unixok  ! to check for database path file separators
      integer iglib  ! for detecting GTK or X11

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
C Take into account the current value of whichdbpath.
      moddb=.false.
      lndbp=lnblnk(standarddbpath)
      if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
        lprev=LPCDB
      elseif(ipathpcdb.eq.2)then
        write(lprev,'(3a)') standarddbpath(1:lndbp),fs,
     &    lpcdb(1:lnblnk(lpcdb))
      endif

C Debug.
C      write(6,*) 'previous plant ',lprev(1:lnblnk(lprev))

  60  continue
      if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
        lltmp=LPCDB  ! use as is
      elseif(ipathpcdb.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    lpcdb(1:lnblnk(lpcdb))  ! prepend db folder path
      endif

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

C Use the profile database unit for testing plant db.
      if(ipcdb.eq.0)ipcdb=iprodb
      CALL ERPFREE(ipcdb,ISTAT)

C If local or absolute path call addpath otherwise if a standard
C database then set longtfile equal to LLTMP.
      if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
        call addpath(LLTMP,longtfile,concat)
      elseif(ipathpcdb.eq.2)then
        longtfile=LLTMP
      endif
      ltf=max(1,LNBLNK(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up additional options depending on graphics lib. The second
C option (list select) is not available for plant database. If using
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

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Plant Components db',
     &    ' ','b select from list',
     &    'c create a new db','d copy the default db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,34)
        copydef=.true.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If corportate database then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Plant Components db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy default db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,34)
        copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If corportate database then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Plant components db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy default db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,34)
        copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default database or the current database.
        idno=1
        isw=0
        call MENUATOL('  ','Plant Components db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy the default db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,34)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.7)copydef=.false.
      endif

C Act on the user's choice. If no choice return to calling menu.
      if(isw.eq.0)then
        return
      elseif(isw.eq.1)then

C If file exists read it and enter editing facility after
C check to see if it is local or standard or absolute path.
        if(XST)then
          call findwhichdbpath('pdb',lltmp,ier)
          goto 44
        else
          call usrmsg('No file to browse/edit. Please use one',
     &                'of the other options.','W')
          LPCDB=lprev
          goto 60
        endif

      elseif(isw.eq.2)then

C There is no select by list for plant template database.
        call usrmsg('No list select for plant template databases.',
     &    ' ','-')
        goto 60

      elseif(isw.eq.3)then

C No plant component db found so set-up minimal common block, create
C a new db, populate it and present editing facility. Use the
C shorter string ltmp for this (easkf cancel option needs to
C be explored).
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
        CALL EASKSCNCL(ltmp,' ','Plant Components db?',
     &    'cancel',clkok,72,DPCDB,'plant components db',
     &    IER,7)
        if(clkok)then
          LPCDB=lprev  ! user canceled, restore initial name
          return
        endif
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          write(LPCDB,'(a)') ltmp(1:lnblnk(ltmp))
        else
          goto 62
        endif

C Create a minimal plant databases (code similar to pdb.F).
        ipathpcdb=1  
        moddb=.true.
        CALL ERPFREE(ipcdb,ISTAT)
        call EFOPRAN(ipcdb,LPCDB,IRECLN,3,IER)
        CALL PCUPDT (ipcdb,0)
        CALL ERPFREE(ipcdb,ISTAT)

C If file exists and user wants to browse then scan the file and
C use the editing facility.
        if(lnblnk(LPCDB).lt.100)then
          write(outs,'(2a)') LPCDB(1:lnblnk(LPCDB)),' created.'
        else
          write(outs,'(2a)') LPCDB(1:100),' created.'
        endif
        dok=.true.
        h(1)='An empty plant component database has been created'
        h(2)='and it it useless until it has been populated with'
        h(3)='data. If you have the information required you can'
        h(4)='proceed with that task. '
        CALL ASKOK(outs,' Browse or edit new database?',OK,dok,4)
        if(OK)then
          moddb=.true.
          goto 44
        endif

      elseif(isw.eq.4.or.isw.eq.7)then

C Copy a database to project folder. If iws is 4 then the
C user has asked for the default database. If isw is 7 then the
C user wishes to browse for the source file. Suggest a local
C file name based on the project root name. Lastly do the copy.
C If the default is to be copied ask for local name. Use LTMP for
C the file name as known to esp-r and LCOPY for the system cp call.
C Remember the file to be copied as sourcefile so that subsequent
C code is general rather than specific (for later consolidation).   
        if(copydef)then
          write(outs248,'(2a)') 'The source file is:',
     &      DPCDB(1:lnblnk(DPCDB))
          call edisp248(iuout,outs248,100)
          sourcefile = DPCDB
        else
          h(1)='You are asked to confirm the source file for the'
          h(2)='copy action. If this one is ok then accept it. '
          lltmp='  '
          CALL EASKXORGTKF(LPCDB,
     &      'Plant template file name (source)?',' ',
     &      DPCDB,lltmp,'plant templates database',IER,2)

          sourcefile = lltmp   ! remember this

C If user request jump back and re-display the menu.
          if(ier.eq.-3)then
            LPCDB=lprev
            goto 60  ! cancel detected, restore name and redisplay menu.
          endif
          ipathpcdb=1  ! signal that it is a local database.
          LPCDB=lltmp
        endif
        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.plantdb'
        elseif(dbspth(1:3).eq.'../')then
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.plantdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.plantdb'
        endif
   66   CALL EASKS(LTMP,
     &    'Database name after it is copied into the project?',
     &    ' ',72,'plant.db','copied plant database',IER,2)
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
     &          sourcefile(1:lnblnk(sourcefile)),' ',
     &          LCOPY(1:lnblnk(LCOPY))
            else

C Standard dos copy logic.
              message='getting plant db file from '
              call dblongdoscopy(sourcefile,lcopy,message,doit,ier)
            endif
            call usrmsg('copying default file via',doit,'-')
            call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
C If the file exists with the normal path to model prepended then
C it should be possible to rebuild the name based on ../dbs.
            call FINDFIL(LCOPY,XST)
            if(XST)then
              call fdroot(lcopy,lpath,fname)
              write(LPCDB,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &          fname(1:lnblnk(fname))
            else
              write(LPCDB,'(a)') LTMP(1:lnblnk(LTMP))
            endif
          else
            doit = ' '
            if(unixok)then
              write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &          LPCDB(1:lnblnk(LPCDB)),' ',LCOPY(1:lnblnk(LCOPY))
            else

C Standard dos copy logic.
              message='getting plant db file from '
              call dblongdoscopy(LPCDB,lcopy,message,doit,ier)
            endif
            call usrmsg('copying file via',doit,'-')
            call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
            call FINDFIL(LCOPY,XST)
            if(XST)then
              call fdroot(lcopy,lpath,fname)
              write(LPCDB,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &          fname(1:lnblnk(fname))
            else
              call FINDFIL(LTMP,XST)
              if(XST)then
                write(LPCDB,'(a)') LTMP(1:lnblnk(LTMP))
              else
                write(LPCDB,'(a)') LTMP(1:lnblnk(LTMP))
                call edisp(iuout,'Problem locating the new database.')
                call edisp(iuout,'Try browsing ../dbs to find it.')
                call edisp(iuout,' ')
              endif
            endif
          endif
          moddb=.true.
          ipathpcdb=1  ! signal that it is a local database.
          goto 44
        else
          goto 66
        endif

      elseif(isw.eq.5)then

C Browse ../dbs if GTK. Fill LCOPY with the model dbspth so user
C can see the files in that folder. Note the user may browse away
C from this location. Easkf returns the absolut path to the
C file selected (later see if this can be resolved to ../dbs). 
C Test the returned file and if it is not readable reset LOPTDB.
        if(iglib.eq.2)then
          h(1)='The plant component database binary file holds the'
          h(2)='description of available components. If the name'
          h(3)='given matches an existing file, it will be read in'
          h(4)='and the components data will be available for'
          h(5)='listing and editing. If there is no such file then'
          h(6)='a new one will be created to hold data for new'
          h(7)='components.'
          write(LCOPY,'(2a)') dbspth(1:lnblnk(dbspth)),fs
          CALL EASKF(LCOPY,' ','Plant templates db?',
     &      144,DPCDB,'plant templates database',IER,7)
          if(ier.eq.-3)then
            LPCDB=lprev
            goto 60  ! cancel detected, restore name and redisplay menu.
          endif

C If the file exists with the normal path to model prepended then
C it should be possible to rebuild the name based on ../dbs.
          call FINDFIL(LCOPY,XST)
          if(XST)then
            call fdroot(lcopy,lpath,fname)
            write(LPCDB,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          else
            write(LPCDB,'(a)') LTMP(1:lnblnk(LTMP))
          endif
          moddb=.true.
          ipathpcdb=1  ! signal that it is a local database.
          goto 44
        else

C For X11 and text allow user to type in a file name.
          h(1)='The plant component database binary file holds the'
          h(2)='description of available components. If the name'
          h(3)='given matches an existing file, it will be read in'
          h(4)='and the components data will be available for'
          h(5)='listing and editing. If there is no such file then'
          h(6)='a new one will be created to hold data for new'
          h(7)='components.'
          llt=lnblnk(lltmp)
          if(llt.lt.96)then
            CALL EASKF(lltmp,' ','Plant templates db?',
     &        96,DPCDB,'plant database',IER,7)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(lltmp,' ','Plant templates db?',
     &        124,DPCDB,'plant database',IER,7)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(lltmp,' ','Plant templates db?',
     &        144,DPCDB,'plant database',IER,7)
          endif
          if(ier.eq.-3)then
            LPCDB=lprev
            goto 60  ! cancel detected, restore name and redisplay menu.
          endif

          if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then

C This is another place where we need to check what the user might
C have edited to be sure of setting the correct location value. The
C common block lpcdb will be updated with call to findwhichdbpath.
            call findwhichdbpath('pdb',lltmp,ier)
            moddb=.true.
            goto 44
          else
            goto 60
          endif
        endif
      elseif(isw.eq.6)then

C Browse distribution database folder. Setup LCOPY to point to
C the database folder (but no particular file). Use easkf to
C return a file (which might be from somewhere else if the user
C decides to do some browsing). Test this suggested file via
C call to eroptdb and if this is not successful reset LOPTDB.
        if(iglib.eq.2)then
          h(1)='The plant component database binary file holds the'
          h(2)='description of available components. If the name'
          h(3)='given matches an existing file, it will be read in'
          h(4)='and the components data will be available for'
          h(5)='listing and editing. If there is no such file then'
          h(6)='a new one will be created to hold data for new'
          h(7)='components.'
          write(LCOPY,'(4a)') dinstpath(1:lnblnk(dinstpath)),
     &      fs,'databases',fs
          CALL EASKF(LCOPY,' ','Plant templates db?',
     &      144,DPCDB,'plant templates database',IER,6)
          if(ier.eq.-3)then
            LPCDB=lprev
            goto 60  ! cancel detected, restore name and redisplay menu.
          endif

C This is yet another place where we need to check what the user might
C have edited to be sure of setting the correct location value. The
C common block lpcdb will have been updated by call to findwhichdbpath.
          call findwhichdbpath('pdb',lcopy,ier)
          moddb=.true.
          goto 44
        else
          call usrmsg('Browse feature not available.',' ','W')
          goto 60
        endif
      endif

C Test the selected or copied database. If a problem loop back
C otherwise edit the database contents.Pay attention to the
C implied path. 
   44 continue
      IER=0
      if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
        call addpath(lltmp,longtfile,concat)
      elseif(ipathpcdb.eq.2)then
        longtfile=lltmp
      endif

C Debug.
C      write(6,*) 'longtfile plant b ',longtfile(1:lnblnk(longtfile))

      call EFOPRAN(ipcdb,longtfile,IRECLN,1,IER)
      IF(ier.eq.0)THEN
        IREC = 1
        READ (ipcdb,REC=IREC,IOSTAT=ISTAT,ERR=60) ZPDESC
        IF (ZPDESC.NE.' PLANT COMPONENT DATABASE')then
          call usrmsg('File is not a Plant Component db.',
     &                'Please re-enter.','W')
          goto 60
        endif
        IREC = 2
        READ (ipcdb,REC=IREC,IOSTAT=ISTAT,ERR=60) NPC,NXTREC
        write(outs,'(a,i3,2a)') 'There are ',NPC,
     &    ' components in ',LPCDB(1:lnblnk(LPCDB))
        call edisp(iuout,outs)
        call edisp(iuout,' ')
        CALL ERPFREE(ipcdb,ISTAT)

C Get logical name of child process terminal type and create a string to drive pdb.
        call tstamp('>','PRJ: start pdb')
        doit = ' '
        call tchild(ICPMOD)
        call termode(ICPMOD,tmode)
        if(iappw.gt.0.and.iappw.le.200)then
          write(doit,'(3a,3i4,3a)') 'pdb -mode ',tmode,
     &      ' -s ',iappw,iappx+10,iappy+40,' -file ',
     &      longtfile(1:lnblnk(longtfile)),' &'
        else
          write(doit,'(5a)') 'pdb -mode ',tmode,
     &      ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),
     &      ' &'
        endif
        call runit(doit,tmode)
      else
        call usrmsg('Error opening db. Please use one',
     &              'of the other options.','W')
        goto 60
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
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path
      common/deflt4/dinstpath
      COMMON/PDBITM/NPDBITEMS,lastrec,NCG(MGIT),NO(MGIT),PDBDESC(MGIT)
      COMMON/PDBELM/ICGS1(MGIT,MPRFDBPER),ICGF1(MGIT,MPRFDBPER),
     &             CGS1(MGIT,MPRFDBPER),CGL1(MGIT,MPRFDBPER)

      CHARACTER H*72,LTMP*72,lltmp*144,lprev*144
      character LLASCI*144
      character longtfile*144,LCOPY*144,message*48
      CHARACTER DOIT*300,fs*1,APP*4,ext*4
      CHARACTER OUTS*124,OUTS248*248
      CHARACTER*24 ZONEPTH,NETPTH,CTLPTH,IMGPTH,RADPTH,DBSPTH
      CHARACTER*24 DOCPTH,TMPPTH
      character PDBDESC*40
      LOGICAL CLKOK,OK,COPYDEF,MODDB,XST,DOK,concat
      character cfgroot*24,LCFGF*72
      character dinstpath*48,path*72,dirpath*48

C Local strings for user selections dependent on graphic library.
      character opte*28,optf*28,optg*28,opth*28,opti*28
      character sourcefile*144  ! the file to copy
      logical unixok  ! to check for database path file separators
      integer iglib  ! for detecting GTK or X11

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
      lprev=LPRFDB

  60  continue
      lltmp=LPRFDB

      llt=lnblnk(lltmp)
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
      H(22)='b) Select from list (not available for events databases).'
      H(23)=' '
      H(24)='c) Make a new database: a new database will be created '
      H(25)='   based on the file name you supply. It will contain a'
      H(26)='   minimal number of items and you can then modify this'
      H(27)='   as required. '
      H(28)='   Exercise care if you use a text editor to copy sets'
      H(29)='   of data from another event profile database.'
      H(30)=' '
      H(31)='d) copy default database: a project copy of the default'
      H(32)='  database will be made and you will be offered a '
      H(33)='  chance to rename it.'
      H(34)=' '
      H(35)='e) binary -> ascii conversion saves current database '
      H(36)='  to a computer independant form.'
      H(37)=' '
      H(38)='f) ascii -> binary useful when you need to pass the model.'
      H(39)='   to someone with an old version of ESP-r that only reads'
      H(40)='   a binary version,'
      H(41)=' '

C If GTK library then there are additional options so include in help.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.2)then
        H(42)='g) browse ../dbs allows you to look in the model ../dbs'
        H(43)='  folder for alternative profile files. If you do not'
        H(44)='  find anything the previous name will be restored.'
        H(45)=' '
        H(46)='h) browse distribution displays files in the databases'
        H(47)='  folder of the ESP-r distribution. If you do not find'
        H(48)='  anything the previous file will be restored.'
        H(49)=' '
        H(50)='i) copy another database to the project allows you to'
        H(51)=' locate a database and get a project copy of it.'
      else
        H(42)='g) select a file (X11 only): edit the file name'
        H(43)='   and if it is found you can browse or edit it.'
        H(44)='   Hint: if you eventually want to copy that other file'
        H(45)='   into the model folder first select it and then ask'
        H(46)='   for a copy.'
        H(47)=' '
        H(48)=' '
        H(49)=' '
        H(50)=' '
        H(51)=' '
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
        call MENUATOL('  ','Event Profiles db',
     &    ' ','b select from list',
     &    'c create a new db','d copy the default db',
     &    opte,optf,optg,opth,opti,' ',' ',' ',isw,idno,51)
        copydef=.true.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If corportate database then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Event Profiles db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy the default db',
     &    opte,optf,optg,opth,opti,' ',' ',' ',isw,idno,51)
        copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If corportate database then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Event Profiles db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy the default db',
     &    opte,optf,optg,opth,opti,' ',' ',' ',isw,idno,51)
        copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default database or the current database.
        idno=1
        isw=0
        call MENUATOL('  ','Event Profiles db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy the default db',
     &    opte,optf,optg,opth,opti,' ',' ',' ',isw,idno,51)
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
          call usrmsg('No db to browse/edit. Please use one',
     &                'of the other options.','W')
          LPRFDB=lprev
          goto 60
        endif

      elseif(isw.eq.2)then

C There is no select by list for events database.
        call usrmsg('No list select for events databases.',
     &    ' ','-')
        goto 60

      elseif(isw.eq.3)then

C No event profile db found so set-up minimal common block, create
C a new ascii db, populate it and present editing facility.
        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.profiledb.a'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.profiledb.a'
        endif
 62     H(1)='An event profiles db holds a schedule of patterns'
        H(2)='over a day. When a new db is created in the model'
        H(3)='it will be the ASCII version.'
        clkok=.false.

C << DPRFDB might point to binary or ascii, need to check >>
        CALL EASKSCNCL(ltmp,' ','Event Profiles db? (ASCII)',
     &    'cancel',clkok,72,DPRFDB,'event profiles db',
     &    IER,3)
        if(clkok)then
          LPRFDB=lprev  ! user canceled, restore initial name
	  return
        endif
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          write(LPRFDB,'(a)') ltmp(1:lnblnk(ltmp))
        else
          goto 62
        endif

C Create a minimal events databases in common and write to ascii file.
C The name of the ascii file includes an .a at the end.
        moddb=.true.
        NPDBITEMS = 1
        NO(1) = 2
        NCG(1)= 1
        PDBDESC(1)='Always ON 100%'
        ICGS1(1,1)=0
        ICGF1(1,1)=24
        CGS1(1,1)=100.0
        CGL1(1,1)=100.0

C The model database is now an ASCII version.
        LLASCI=LPRFDB
        IAF=IFIL+1
        CALL TROPROCOM(IAF,LLASCI,IER)

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

C << will need to check if the default is an ascii version >>

C Copy a database to project dbs folder. If iws is 4 then the
C user has asked for the default database. If isw is 7 then the
C user wishes to browse for the source file.  Suggest a local
C file name based on the project root name. Lastly do the copy.
C If the default is to be copied ask for local name. Use LTMP for
C the file name as known to esp-r and LCOPY for the system cp call.
C Remember the file to be copied as sourcefile so that subsequent
C code is general rather than specific (for later consolidation).   
        if(copydef)then
          write(outs248,'(2a)') 'The source file is:',
     &      DPRFDB(1:lnblnk(DPRFDB))
          call edisp248(iuout,outs248,100)
          sourcefile = DPRFDB
        else
          h(1)='You are asked to confirm the source file for the'
          h(2)='copy action. If this one is ok then accept it. '

          lltmp='  '
          CALL EASKXORGTKF(LPRFDB,
     &      'Event Profiles db (source)?',' ',
     &      DPRFDB,lltmp,'binary events db',IER,2)
          sourcefile = lltmp

C If user request jump back and re-display the menu.
          if(ier.eq.-3)then
            LPRFDB=lprev
	    goto 60  ! restore name and redisplay menu.
          endif

          LPRFDB=lltmp
        endif

C << why is it eventdb here and profiledb elsewhere?? >>

C << do we want to convert any binary version into a local
C << ascii version?

        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.eventdb'
        elseif(dbspth(1:3).eq.'../')then
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.eventdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.eventdb'
        endif
  66    CALL EASKS(LTMP,
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
          doit = ' '
          if(unixok)then
            write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &        sourcefile(1:lnblnk(sourcefile)),' ',
     &        LCOPY(1:lnblnk(LCOPY))
          else

C Logic for copy similar to the pattern established above.
            message='getting events db file from '
            call dblongdoscopy(sourcefile,lcopy,message,doit,ier)
          endif
C          call usrmsg('copying default file via',doit,'-')
          call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
          call FINDFIL(LCOPY,XST)
          if(XST)then
            write(LPRFDB,'(a)') LCOPY(1:lnblnk(LCOPY))
          else
            call FINDFIL(LTMP,XST)
            if(XST)then
              write(LPRFDB,'(a)') LTMP(1:lnblnk(LTMP)) ! short local to long
            else
              write(LPRFDB,'(a)') LTMP(1:lnblnk(LTMP)) ! short local to long
              call edisp(iuout,'Problem locating the new database.')
              call edisp(iuout,'Try browsing ../dbs to find it.')
              call edisp(iuout,' ')
            endif
          endif
          moddb=.true.
          goto 44
        else
          goto 66
        endif

      elseif(isw.eq.5)then

C Event profile db: BINARY > ASCII. 
C Ask for the binary file name and then confirm the ASCII db name.
        IAF=IFIL+1
        H(1)='The events db contains one or more descriptions of'
        H(2)='profiles created via a diary entry format. This'
        H(3)='otion converts from binary to asci version of the '
        H(4)='file. '
        lltmp='  '
        CALL EASKXORGTKF(LPRFDB,
     &    'Event Profiles db (source)?',' ',
     &    DPRFDB,lltmp,'binary events db',IER,2)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          LPRFDB=lprev
	  goto 60  ! restore name and redisplay menu.
        endif

        if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then
          LPRFDB=lltmp
        else
          goto 60
        endif

C If LPRFDB does not end in .a then update LLASACI
        llt=lnblnk(LPRFDB)
	if(LPRFDB(llt-1:llt).eq.'.a')then
          LLASCI=LPRFDB
        else
          if(llt.le.141)then 
            write(LLASCI,'(2a)') LPRFDB(1:llt),'.a'
          else
            write(LLASCI,'(2a)') LPRFDB(1:141),'.a'
          endif
        endif

C Read binary file into common blocks.
        call TRIPROBIN(IER)

C Write commons to ASCII and then redisplay the menu.
        call TROPROCOM(IAF,LLASCI,IER)
        call tstamp('>','PRO: convert binary to ascii')
        goto 60
      elseif(isw.eq.6)then

C Profile db: ASCII > BINARY.
C Ask for the ASCII file name and then confirm the binary db name.
        IAF=IFIL+1
        H(1)='The events db contains one or more descriptions of'
        H(2)='profiles created via a diary entry format. This option'
        H(3)='allows you to convert an ASCII version to a binary'
        H(4)='version of the file if you need to pass the model to'
        H(5)='someone with an older version of ESP-r that only deals'
        H(6)='with binary versions. '

C Check the last two characters of the file name.
        llt=lnblnk(LPRFDB)
	if(LPRFDB(llt-1:llt).eq.'.a')then
          LLASCI=LPRFDB
        else
          if(llt.le.141)then 
            write(LLASCI,'(2a)') LPRFDB(1:llt),'.a'
          else
            write(LLASCI,'(2a)') LPRFDB(1:141),'.a'
          endif
        endif

        CALL EASKXORGTKF(LLASCI,
     &    'Event Profiles db (ASCII source)?',' ',
     &    DPRFDB,lltmp,'ascii events db',IER,2)
        LLASCI=lltmp  ! copy returned name to LLASCI

        H(1)='The existing binary events db will be overwritten'
        H(2)='with the data from the ASCII version.'

C If LPRFDB ends with .a then make up a temporary file name
C for the binary that has the .a stripped off.
	if(LPRFDB(llt-1:llt).eq.'.a')then
          write(lltmp,'(a)') LPRFDB(1:llt-2)
        else
          write(lltmp,'(a)') LPRFDB(1:lnblnk(LPRFDB))
        endif
        CALL EASKXORGTKF(LPRFDB,
     &    'Event Profiles db?',' ',
     &    DPRFDB,lltmp,'binary events db',IER,2)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          LPRFDB=lprev
	  goto 60  ! restore name and redisplay menu.
        endif

        if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then

C Do conversion and then to prove that the file was correctly
C converted proceed to browse/edit the details.
          LPRFDB=lltmp
          CALL TRIPRO(IAF,LLASCI,IER)
          moddb=.true.
          goto 44
        else
          goto 60
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
          CALL EASKF(LCOPY,' ','Event Profiles db?',
     &      144,DOPTDB,'Event profiles db',IER,5)
          if(ier.eq.-3)then
            LPRFDB=lprev
	    goto 60   ! cancel detected, restore name and redisplay menu.
          endif

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
            CALL EASKF(lltmp,' ','Event Profile db?'
     &        ,96,DPRFDB,'binary events db',IER,2)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(lltmp,' ','Event Profile db?',
     &        124,DPRFDB,'binary events db',IER,2)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(lltmp,' ','Event Profile db?',
     &        144,DPRFDB,'binary events db',IER,2)
          endif
          if(ier.eq.-3)then
            LPRFDB=lprev
	    goto 60  ! a cancel was detected, redisplay menu.
          endif
          if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then
            LPRFDB=lltmp
            moddb=.true.
            goto 44
          else
            goto 60
          endif
        endif

      elseif(isw.eq.8)then

C Browse distribution database folder. Setup LCOPY to point to
C the database folder (but no particular file). Use easkf to
C return a file (which might be from somewhere else if the user
C decides to do some browsing). Test this suggested file via
C call to eroptdb and if this is not successful reset LPRFDB.
        if(iglib.eq.2)then
          h(1)='An event profiles database holds named sets of profiles'
          h(2)='in a binary representation. '
          H(3)=' '
          H(4)='The ESP-r distribution includes alternatives to the'
          H(5)='default which might be useful.'
          write(LCOPY,'(4a)') dinstpath(1:lnblnk(dinstpath)),
     &      fs,'databases',fs
          CALL EASKF(LCOPY,' ','Event Profiles db?',
     &      144,DOPTDB,'Event profiles database',IER,5)
          if(ier.eq.-3)then
            LPRFDB=lprev
	    goto 60  ! cancel was detected, restore name and redisplay menu.
          endif
          LPRFDB=LCOPY
          moddb=.true.
          goto 44
        else
          call usrmsg('Browse feature not available.',' ','W')
          goto 60
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

C Scan the binary event profiles database. If an error state is
C returned then it might be an ASCII file, try and scan that.
      call TRIPROBIN(IER)
      if(ier.eq.-2)then
        CALL ERPFREE(IPRODB,ISTAT)
        CALL USRMSG(' opened profiles db...',' ','-')
        call TRIPROCOM(IPRODB,LPRFDB,IER)
        if(ier.ne.0)then
          call usrmsg('Error opening the db. Please use one',
     &                'of the other options.','W')
          LPRFDB=lprev
          goto 60
        else
          CALL ERPFREE(IPRODB,ISTAT)
          CALL USRMSG(' opened profiles db...ok',' ','-')

C Call casfmk for the editing/browsing interface.
          call casfmk(ier)
          return
        endif
      elseif(ier.eq.0)then
        CALL USRMSG(' opened profiles db...ok',' ','-')

C Call casfmk for the editing/browsing interface.
        call casfmk(ier)
        return
      else
        call usrmsg('Error opening the db. Please use one',
     &              'of the other options.','W')
        LPRFDB=lprev
        goto 60
      endif
      return

      end

C ********** EDDBOPT
C EDDBOPT Changes the optical database current cfg file references.
C moddb is from version manager
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.
C This facility offers browsing for GTK version.
      SUBROUTINE EDDBOPT(moddb,APP)
#include "building.h"
#include "esprdbfile.h"
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
      CHARACTER outs248*248
      CHARACTER*24 ZONEPTH,NETPTH,CTLPTH,IMGPTH,RADPTH,DBSPTH
      CHARACTER*24 DOCPTH,TMPPTH
      character GTYPE*12,GDESCR*36,SOPT*12
      LOGICAL CLKOK,OK,COPYDEF,MODDB,XST,DOK
      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,concat
      character cfgroot*24,LCFGF*72
      character dinstpath*48,path*72,dirpath*48
      character sourcefile*144  ! the file to copy ??

C Local strings for user selections dependent on graphic library.
      character opte*28,optf*28,optg*28
      character lpath*72,fname*72 ! for use with fdroot
      integer lndbp   ! for length of standard database path
      logical unixok  ! to check for database path file separators
      integer iglib  ! for detecting GTK or X11

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
C Take into account the current value of whichdbpath.
      moddb=.false.
      lndbp=lnblnk(standarddbpath)
      if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
        lprev=LOPTDB
      elseif(ipathoptdb.eq.2)then
        write(lprev,'(3a)') standarddbpath(1:lndbp),fs,
     &    loptdb(1:lnblnk(loptdb))
      endif

C Debug.
C      write(6,*) 'previous optics ',lprev(1:lnblnk(lprev))

  60  continue
      if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
        lltmp=LOPTDB  ! use as is
      elseif(ipathoptdb.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    loptdb(1:lnblnk(loptdb))  ! prepend db folder path
      endif

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

C If local or absolute path call addpath otherwise if a standard
C database then set longtfile equal to LLTMP.
      if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
        call addpath(LLTMP,longtfile,concat)
      else
        longtfile=LLTMP
      endif
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

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Optical Properties db',
     &    ' ','b select from list',
     &    'c create a new db','d copy the default db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,34)
        copydef=.true.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If corportate database then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Optical Properties db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy default db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,34)
        copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If corportate database then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Optical Properties db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy default db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,34)
        copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default database or the current database.
        idno=1
        isw=0
        call MENUATOL('  ','Optical Properties db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy the default db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,34)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.7)copydef=.false.
      endif

C Act on the users choice. If no choice, return to calling menu.
      if(isw.eq.0)then
        return
      elseif(isw.eq.1)then

C If file exists read it and enter editing facility after
C check to see if it is local or standard or absolute path.
        if(XST)then
          call findwhichdbpath('opt',lltmp,ier)
          goto 44
        else
          call usrmsg('No file to browse/edit. Please use one',
     &                'of the other options.','W')
          LOPTDB=lprev
          goto 60
        endif
      elseif(isw.eq.2)then

C There is no select by list for optics database.
        call usrmsg('No list select for optical databases.',' ','-')
        goto 60
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
  62    H(1)='An optics db holds properties of transparent'
        H(2)='constructions '
        clkok=.false.
        CALL EASKSCNCL(ltmp,' Optics database?',
     &    ' ','cancel',clkok,72,'optics.db','optics database',IER,2)
        if(clkok)then
          LOPTDB=lprev
          return
        endif
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          write(LOPTDB,'(a)') ltmp(1:lnblnk(ltmp)) ! local so direct asignment
        else
          goto 62
        endif

C Create a minimal optics databases (code similar to edoptic.F).
C Set the value of ipathoptdb to signal local database.
        ipathoptdb=1  
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
        dok=.true.
        h(1)='An empty optics database has been created. It is'
        h(2)='useless until it is populated with data. If you '
        h(3)='have the relevant information you can start now.'
        CALL ASKOK('  ',' Browse or edit new database?',OK,dok,3)
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
          sourcefile = DOPTDB
        else
          h(1)='You are asked to confirm the source file for the'
          h(2)='copy action. If this one is ok then accept it. '

          lltmp='  '
          CALL EASKXORGTKF(LPRFDB,
     &      'Optical Properties db (source)?',' ',
     &      DOPTDB,lltmp,'optical properties database',IER,2)
          sourcefile = lltmp

C If user request jump back and re-display the menu.
          if(ier.eq.-3)then
            LOPTDB=lprev
	    goto 60  ! cancel detected, redisplay menu.
          endif
          ipathoptdb=1  ! signal that it is a local database.
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
  66    CALL EASKS(LTMP,
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
     &          sourcefile(1:lnblnk(sourcefile)),' ',
     &          LCOPY(1:lnblnk(LCOPY))
            else

C Standard dos copy logic.
              message='getting optics db file from '
              call dblongdoscopy(sourcefile,lcopy,message,doit,ier)
            endif
            call usrmsg('copying default file via',doit,'-')
            call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
C If the file exists with the normal path to model prepended then
C it should be possible to rebuild the name based on ../dbs.
            call FINDFIL(LCOPY,XST)
            if(XST)then
              call fdroot(lcopy,lpath,fname)
              write(LOPTDB,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &          fname(1:lnblnk(fname))
            else
              write(LOPTDB,'(a)') LTMP(1:lnblnk(LTMP))
            endif
          else
            doit = ' '
            if(unixok)then
              write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &          LOPTDB(1:lnblnk(LOPTDB)),' ',LCOPY(1:lnblnk(LCOPY))
            else

C Standard dos copy logic.
              message='getting optics db file from '
              call dblongdoscopy(LOPTDB,lcopy,message,doit,ier)
            endif
            call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
            call FINDFIL(LCOPY,XST)
            if(XST)then
              call fdroot(lcopy,lpath,fname)
              write(LOPTDB,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &          fname(1:lnblnk(fname))
            else
              call FINDFIL(LTMP,XST)
              if(XST)then
                write(LOPTDB,'(a)') LTMP(1:lnblnk(LTMP))
              else
                write(LOPTDB,'(a)') LTMP(1:lnblnk(LTMP))
                call edisp(iuout,'Problem locating the new database.')
                call edisp(iuout,'Try browsing ../dbs to find it.')
                call edisp(iuout,' ')
              endif
            endif
          endif
          moddb=.true.
          ipathoptdb=1  ! signal that it is a local database.
          goto 44
        else
          goto 66
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
          CALL EASKF(LCOPY,' ','Optical Properties db?',
     &      144,DOPTDB,'optical properties database',IER,6)
          if(ier.eq.-3)then
            LOPTDB=lprev
	    goto 60  ! cancel detected, restore name and redisplay menu.
          endif

C If the file exists with the normal path to model prepended then
C it should be possible to rebuild the name based on ../dbs.
          call FINDFIL(LCOPY,XST)
          if(XST)then
            call fdroot(lcopy,lpath,fname)
            write(LOPTDB,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          else
            write(LOPTDB,'(a)') LTMP(1:lnblnk(LTMP))
          endif
          moddb=.true.
          ipathoptdb=1  ! signal that it is a local database.
          goto 44
        else

C For X11 and text allow user to type in a file name.
          H(1)='An optical properties database holds information for'
          H(2)='transparent constructions. It is closely related to'
          H(3)='the constructions database referenced by the project.'
          llt=lnblnk(lltmp)
          if(llt.lt.96)then
            CALL EASKF(lltmp,' ','Optical Properties db?',
     &        96,DOPTDB,'optical properties database',IER,3)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(lltmp,' ','Optical Properties db?',
     &        124,DOPTDB,'optical properties database',IER,3)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(lltmp,' ','Optical Properties db?',
     &        144,DOPTDB,'optical properties database',IER,3)
          endif
          if(ier.eq.-3)then
            LOPTDB=lprev
	    goto 60  ! cancel detected, restore name and redisplay menu.
          endif

          if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then

C This is another place where we need to check what the user might
C have edited to be sure of setting the correct location value. The
C common block will be updated with call to findwhichdbpath.
            call findwhichdbpath('opt',lltmp,ier)
            moddb=.true.
            goto 44
          else
            goto 60
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
          CALL EASKF(LCOPY,' ','Optical Properties db?',
     &      144,DOPTDB,'optical properties database',IER,6)
          if(ier.eq.-3)then
            LOPTDB=lprev
	    goto 60  ! cancel detected, restore name and redisplay menu.
          endif

C This is yet another place where we need to check what the user might
C have edited to be sure of setting the correct location value. The
C common block will be updated with call to findwhichdbpath.
          call findwhichdbpath('opt',lltmp,ier)
          moddb=.true.
          goto 44
        else
          call usrmsg('Browse feature not available.',' ','W')
          goto 60
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
        call usrmsg('Problem encountered with Optical Properties db!',
     &              'Please check ddb location and content.','W')
        OPTKOK=.FALSE.
        LOPTDB=lprev
        goto 60
      else
        OPTKOK=.TRUE.
        call EDOPT(IIER)
      endif

      return
      end


C ********** EDDBPRES
C EDDBPRES Changes the wind pressure database current cfg file references.
C moddb is from version manager
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.
C This facility offers browsing for GTK version.
      SUBROUTINE EDDBPRES(moddb,APP)
#include "building.h"
#include "net_flow.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/pophelp/h(60)
      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path
      common/deflt4/dinstpath

      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      character DEPRE*40
      common/MFLDOC/DEPRE(MPRD)

      CHARACTER H*72,LTMP*72,lltmp*144,lprev*144
      character longtfile*144,LCOPY*144,message*48
      CHARACTER DOIT*300,fs*1,APP*4
      CHARACTER outs248*248
      CHARACTER*24 ZONEPTH,NETPTH,CTLPTH,IMGPTH,RADPTH,DBSPTH
      CHARACTER*24 DOCPTH,TMPPTH

      LOGICAL CLKOK,OK,COPYDEF,MODDB,XST,DOK,concat
      character cfgroot*24,LCFGF*72
      character dinstpath*48,path*72,dirpath*48
      character sourcefile*144  ! the file to copy ??
      character OUTSTR*124      ! string buffer for data read
      character t144*144        ! for passing to erprcdb

C Local strings for user selections dependent on graphic library.
      character opte*28,optf*28,optg*28
      character lpath*72,fname*72 ! for use with fdroot
      character lworking*144   ! file name in std folder
      integer lndbp   ! for length of standard database path
      logical unixok  ! to check for database path file separators
      integer iglib  ! for detecting GTK or X11

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
C Take into account the current value of whichdbpath.
      moddb=.false.
      lndbp=lnblnk(standarddbpath)
      if(ipathapres.eq.0.or.ipathapres.eq.1)then
        lprev=lapres
      elseif(ipathapres.eq.2)then
        write(lprev,'(3a)') standarddbpath(1:lndbp),fs,
     &    lapres(1:lnblnk(lapres))
      endif

C Debug.
C      write(6,*) 'previous pres coef ',lprev(1:lnblnk(lprev))

  60  continue
      if(ipathapres.eq.0.or.ipathapres.eq.1)then
        lltmp=lapres  ! use as is
      elseif(ipathapres.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    lapres(1:lnblnk(lapres))  ! prepend db folder path
      endif
      IAPRES=IFIL+1

      H(1)='To enable the calculation of wind-induced surface'
      H(2)='pressures (as required the simulator), this database'
      H(3)='of angle dependent pressure coefficients is provided.'
      H(4)=' '
      H(5)='Wind pressure coefficients are pressure coefficients'
      H(6)='for surfaces in typical positions and within several'
      H(7)='different exposure categories.'
      H(8)=' '
      H(9)='The following options can be selected:'
      H(10)=' '
      H(11)='a) If the file exists than you can browse the database '
      H(12)='   and if you have permission to update the file you'
      H(13)='   can edit the entities in the database.'
      H(14)=' '
      H(15)='b) Select from list (not available for optics databases).'
      H(16)=' '
      H(17)='c) Make a new database: a new database will be created '
      H(18)='   based on the file name you supply. It will contain a'
      H(19)='   minimal number of items and you can then modify this'
      H(20)='   as required. '
      H(21)='   Exercise care if you use a text editor to copy sets'
      H(22)='   of data from another optical database.'
      H(23)=' '
      H(24)='d) copy default database: a project copy of the default'
      H(25)='  database will be made and you will be offered a '
      H(26)='  chance to rename it.'
      H(27)=' '

C If GTK library then there are additional options so include in help.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.2)then
        H(28)='e) browse ../dbs allows you to look in the model ../dbs'
        H(29)='  folder for alternative database. If you do not'
        H(30)='  find anything the previous value will be restored.'
        H(31)=' '
        H(32)=' f) browse distribution displays files in the databases'
        H(33)='  folder of the ESP-r distribution. If you do not find'
        H(34)='  anything the previous value will be restored.'
        H(35)=' '
        H(36)=' g) copy another database to the project allows you to'
        H(37)=' locate a database and get a project copy of it.'
      else
        H(28)='e) select a file (X11 only): edit the file name'
        H(29)='   and if it is found you can browse or edit it.'
        H(30)='   Hint: if you eventually want to copy that other file'
        H(31)='   into the model folder first select it and then ask'
        H(32)='   for a copy.'
        H(33)=' '
        H(34)=' '
        H(35)=' '
        H(36)=' '
        H(37)=' '
      endif
      CALL ERPFREE(IAPRES,ISTAT)

C If local or absolute path call addpath otherwise if a standard
C database then set longtfile equal to LLTMP.
      if(ipathapres.eq.0.or.ipathapres.eq.1)then
        call addpath(lltmp,longtfile,concat)
      else
        longtfile=lltmp
      endif
      ltf=max(1,LNBLNK(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up additional options depending on graphics lib. The second
C option (list select) is not available for pressure database. If using
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

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Pressure Distributions db',
     &    ' ','b select from list',
     &    'c create a new db','d copy the default db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,37)
        copydef=.true.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If corportate database then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Pressure Distributions db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy default db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,37)
        copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If corportate database then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Pressure Distributions db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy default db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,37)
        copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default database or the current database.
        idno=1
        isw=0
        call MENUATOL('  ','Pressure Distributions db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy the default db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,37)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.7)copydef=.false.
      endif

C Act on the users choice. If no choice, return to calling menu.
      if(isw.eq.0)then
        return
      elseif(isw.eq.1)then

C If file exists read it and enter editing facility after
C check to see if it is local or standard or absolute path.
        if(XST)then
          call findwhichdbpath('prs',lltmp,ier)
          goto 44
        else
          call usrmsg('No file to browse/edit. Please use one',
     &                'of the other options.','W')
          lapres=lprev
          goto 60
        endif
      elseif(isw.eq.2)then

C There is no select by list for pressure database.
        call usrmsg('No list select for pressure databases.',' ','-')
        goto 60
      elseif(isw.eq.3)then

C No pressure db found so set-up minimal common block, create
C a new db, populate it and present editing facility. Use the
C shorter string ltmp for this (easkf cancel option needs to
C be explored).
        H(1)='To enable the calculation of wind-induced surface'
        H(2)='pressures (as required the simulator), this database'
        H(3)='of angle dependent pressure coefficients is provided.'
        H(4)=' '
        H(5)='Wind pressure coefficients are pressure coefficients'
        H(6)='for surfaces in typical positions and within several'
        H(7)='different exposure categories. '
        H(8)=' '
        H(9)='Please supply and confirm the file name.'
        h(10)='The suggested name of this database is based on the '
        h(11)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(ltmp,'(2a)') cfgroot(1:lr),'.pressuredb'
        else
          write(ltmp,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.pressuredb'
        endif
        clkok=.false.
  62    CALL EASKSCNCL(ltmp,' New pressure coefficients database?',
     &      ' ','cancel',clkok,72,DAPRES,
     &      'pressure coefficients database',IER,11)
        if(clkok) return    ! abandon this task
        if(ltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then
          write(lapres,'(a)') ltmp(1:lnblnk(ltmp))
        else
          goto 62
        endif

C Create a minimal pressure databases.
        moddb=.true.
        NPRE=1
        DEPRE(NPRE)='undefined pc set'
        do 33 ij=1,16
          FPRE(ij,1)=0.00
  33    continue 

C Update the current file and re-scan it and then open editing facilty. 
        CALL EMKAPCDB(LAPRES,IER)
        IF(IER.NE.0) GOTO 60
	t144='  '
        CALL ERPRCDB(t144,0,3,IER)
        IF(IER.NE.0) GOTO 60
        CALL EDPCDB(IER) 

C Scan the new file and use the editing facility.
        dok=.true.
        h(1)='An empty pressure database has been created. It is'
        h(2)='useless until it is populated with data. If you '
        h(3)='have the relevant information you can start now.'
        CALL ASKOK('  ',' Browse or edit new database?',OK,dok,3)
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
     &      DAPRES(1:lnblnk(DAPRES))
          call edisp248(iuout,outs248,100)
          call edisp(iuout,' ')
          sourcefile = DAPRES
        else
          h(1)='You are asked to confirm the source file for the'
          h(2)='copy action. If this one is ok then accept it. '

          lltmp='  '
          CALL EASKXORGTKF(LPRFDB,
     &      'Pressure coefficients db (source)?',' ',
     &      dapres,lltmp,'pressure coef database',IER,2)
          sourcefile = lltmp

C If user request jump back and re-display the menu.
          if(ier.eq.-3)then
            lapres=lprev
	    goto 60  ! cancel detected, redisplay menu.
          endif
          ipathapres=1  ! signal that it is a local database.
          lapres=lltmp
        endif
        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.pressuredb'
        elseif(dbspth(1:3).eq.'../')then
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.pressuredb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.pressuredb'
        endif
  66    CALL EASKS(LTMP,
     &    'Database name after it is copied into the project?',
     &    ' ',72,'pressure.db','copied pres coef database',IER,2)
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
     &          sourcefile(1:lnblnk(sourcefile)),' ',
     &          LCOPY(1:lnblnk(LCOPY))
            else

C Standard dos copy logic.
              message='getting pressure db file from '
              call dblongdoscopy(sourcefile,lcopy,message,doit,ier)
            endif
            call usrmsg('copying default file via',doit,'-')
            call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
C If the file exists with the normal path to model prepended then
C it should be possible to rebuild the name based on ../dbs.
            call FINDFIL(LCOPY,XST)
            if(XST)then
              call fdroot(lcopy,lpath,fname)
              write(lapres,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &          fname(1:lnblnk(fname))
            else
              write(lapres,'(a)') LTMP(1:lnblnk(LTMP))
            endif
          else
            doit = ' '
            if(unixok)then
              write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &          lapres(1:lnblnk(lapres)),' ',LCOPY(1:lnblnk(LCOPY))
            else

C Standard dos copy logic.
              message='getting pressure db file from '
              call dblongdoscopy(lapres,lcopy,message,doit,ier)
            endif
            call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
            call FINDFIL(LCOPY,XST)
            if(XST)then
              call fdroot(lcopy,lpath,fname)
              write(lapres,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &          fname(1:lnblnk(fname))
            else
              call FINDFIL(LTMP,XST)
              if(XST)then
                write(lapres,'(a)') LTMP(1:lnblnk(LTMP))
              else
                write(lapres,'(a)') LTMP(1:lnblnk(LTMP))
                call edisp(iuout,'Problem locating the new database.')
                call edisp(iuout,'Try browsing ../dbs to find it.')
                call edisp(iuout,' ')
              endif
            endif
          endif
          moddb=.true.
          ipathapres=1  ! signal that it is a local database.
          goto 44
        else
          goto 66
        endif

      elseif(isw.eq.5)then

C Browse ../dbs if GTK. Fill LCOPY with the model dbspth so user
C can see the files in that folder. Note the user may browse away
C from this location. Easkf returns the absolut path to the
C file selected (later see if this can be resolved to ../dbs). 
C Test the returned file and if it is not readable reset lapres.
        if(iglib.eq.2)then
          H(1)='Wind pressure coefficients are pressure coefficients'
          H(2)='for surfaces in typical positions and within several'
          H(3)='different exposure categories.'
          H(4)=' '
          H(5)='A local database folder (../dbs) is usually associated'
          H(6)='with each model. '
          write(LCOPY,'(2a)') dbspth(1:lnblnk(dbspth)),fs
          CALL EASKF(LCOPY,' ','Pressure coefficients db?',
     &      144,dapres,'pressure coef database',IER,6)
          if(ier.eq.-3)then
            lapres=lprev
	    goto 60  ! cancel detected, restore name and redisplay menu.
          endif

C If the file exists with the normal path to model prepended then
C it should be possible to rebuild the name based on ../dbs.
          call FINDFIL(LCOPY,XST)
          if(XST)then
            call fdroot(lcopy,lpath,fname)
            write(lapres,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          else
            write(lapres,'(a)') LTMP(1:lnblnk(LTMP))
          endif
          moddb=.true.
          ipathapres=1  ! signal that it is a local database.
          goto 44
        else

C For X11 and text allow user to type in a file name.
          H(1)='Wind pressure coefficients are pressure coefficients'
          H(2)='for surfaces in typical positions and within several'
          H(3)='different exposure categories.'
          llt=lnblnk(lltmp)
          if(llt.lt.96)then
            CALL EASKF(lltmp,' ','Pressure coefficients db?',
     &        96,dapres,'pressure coef database',IER,3)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(lltmp,' ','Pressure coefficients db?',
     &        124,dapres,'pressure coef database',IER,3)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(lltmp,' ','Pressure coefficients db?',
     &        144,dapres,'pressure coef database',IER,3)
          endif
          if(ier.eq.-3)then
            lapres=lprev
	    goto 60  ! cancel detected, restore name and redisplay menu.
          endif

          if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then

C This is another place where we need to check what the user might
C have edited to be sure of setting the correct location value. The
C common block will be updated with call to findwhichdbpath.
            call findwhichdbpath('prs',lltmp,ier)
            moddb=.true.
            goto 44
          else
            goto 60
          endif
        endif
      elseif(isw.eq.6)then

C Browse distribution database folder. Setup LCOPY to point to
C the database folder (but no particular file). Use easkf to
C return a file (which might be from somewhere else if the user
C decides to do some browsing). Test this suggested file and if
C this is not successful reset lapres.
        if(iglib.eq.2)then
          H(1)='Wind pressure coefficients are pressure coefficients'
          H(2)='for surfaces in typical positions and within several'
          H(3)='different exposure categories.'
          H(4)=' '
          H(5)='The ESP-r distribution includes alternatives to the'
          H(6)='default which might be useful.'
          write(LCOPY,'(4a)') dinstpath(1:lnblnk(dinstpath)),
     &      fs,'databases',fs
          CALL EASKF(LCOPY,' ','Pressure coefficients db?',
     &      144,dapres,'pressure coef database',IER,6)
          if(ier.eq.-3)then
            lapres=lprev
	    goto 60  ! cancel detected, restore name and redisplay menu.
          endif

C This is yet another place where we need to check what the user might
C have edited to be sure of setting the correct location value. The
C common block will be updated with call to findwhichdbpath.
          call findwhichdbpath('prs',lltmp,ier)
          moddb=.true.
          goto 44
        else
          call usrmsg('Browse feature not available.',' ','W')
          goto 60
        endif
      endif

C Test the selected or copied database. If a problem loop back
C otherwise edit the database contents.
   44 continue
      write(outs248,'(2a)') 'Accessing presure coef db: ',
     &  lapres(1:lnblnk(lapres))
      call edisp248(iuout,outs248,100)
      moddb=.true.
      if(ipathapres.eq.0.or.ipathapres.eq.1)then
        CALL EFOPSEQ(IAPRES,lapres,1,IER)
      elseif(ipathapres.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    lapres(1:lnblnk(lapres))

C Debug.
C        write(6,*) 'working pressure coef ',lworking

        CALL EFOPSEQ(IAPRES,lworking,1,IER)
      endif
      IF(IER.NE.0) GOTO 60
       
      CALL STRIPC(IAPRES,OUTSTR,0,ND,1,'pressure db',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NPRE,1,MPRD,'W','no pressure item',IER)
      IF(NPRE.LT.1.OR.NPRE.GT.MPRD) THEN
        CALL USRMSG(' The specified file may not be a',
     &              ' pressure coef db, please try again','W')
        CALL ERPFREE(IAPRES,ISTAT)
        GOTO 60
      ENDIF
      CALL ERPFREE(IAPRES,ISTAT)

C Open current pressure file, read in data, if OK, display coefficients and
C enter editing facilitiy.
      t144='  '
      CALL ERPRCDB(t144,0,3,IER)
      if(ier.ne.0)then
        call usrmsg('Problem encountered with Optical Properties db!',
     &              'Please check ddb location and content.','W')
        GOTO 60
      else
        CALL EDPCDB(IER) 
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
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "espriou.h"
C espriou.h provides currentfile.
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN

      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/C21/IFCFG,cfgroot,LCFGF
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      common/appw/iappw,iappx,iappy
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      common/rpath/path
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/C4/XLAT,XLON
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/deflt4/dinstpath

C Calendar.
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)

      CHARACTER H*72,DFILE*72,LLASCI*144
      character longtfile*144,longtafile*144,message*48,longtmp*144
      CHARACTER DOIT*248,fs*1,TMODE*8,CLMLOC*30,LCOPY*72
      CHARACTER OUTS*124,APP*6,ext*4,llclmdb*144
      CHARACTER*24 ZONEPTH,NETPTH,CTLPTH,IMGPTH,RADPTH,DBSPTH
      CHARACTER*24 DOCPTH,TMPPTH
      LOGICAL OK,concat,COPYDEF,MODDB,XST,DOK,UNIXOK
      character cfgroot*24,LCFGF*72,linstd*60
      character dinstpath*48,path*72
      CHARACTER*72 NNAME
      dimension ICLM(24,6)
      integer llt  ! length of string buffer
      integer icreport  ! to signal that calendar has been updated.
      integer IDOL,IDAYNUM,IMTHNUM,IDWKNUM,IDTYY  ! for correcting calendar

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      moddb=.false.
 552  llt=lnblnk(LCLIM)
      write(llclmdb,'(a)') lclim(1:llt)

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
      call FINDFIL(llclmdb,XST)
      IF(.NOT.XST)THEN
        idno=2
        isw=0
        call MENUATOL('  ','Climate db',
     &      ' ','b select another db',
     &      'c create a new db','d copy default db',
     &      ' ',' ',' ',' ',' ',' ',' ',' ',isw,idno,36)
        copydef=.true.
      elseif(llclmdb(1:linst).eq.linstd(1:linst))then

C If corportate database then offer the following choices:
        idno=1
        isw=0
        call MENUATOL('  ','Climate db',
     &    'a browse/edit db','b select another db',
     &    'c create a new db','d copy default db',
     &    'e binary >> ascii export','f ascii >> binary import',
     &    'g EPW >> binary import',' ',' ',' ',' ',' ',isw,idno,36)
        copydef=.false.
      else

C If db exists locally offer the following choices:
        idno=1
        isw=0
        call MENUATOL('  ','Climate db',
     &      'a browse/edit db','b select another db',
     &      'c create a new db','d copy default db',
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
          call EFOPRAN(ICLIM,llclmdb,144,0,IER)
          if(ier.eq.0)then
            moddb=.true.

C Cast back to LCLIM.
            llt=lnblnk(llclmdb)
            write(LCLIM,'(a)') llclmdb(1:llt)
          else
            call usrmsg('Problem encountered with Climate db!',
     &                  'Please check db location and content.','W')
            goto 552
          endif
        else
          call usrmsg('Sorry there is no file to browse/edit.',
     &                'Please use one of the other options.','W')
          goto 552
        endif
        CALL ERPFREE(ICLIM,ISTAT)

C Get logical name of child process terminal type, expand climate 
C name to include the path and create a string to drive clm.

C << update to deal with dos paths >>

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
        call runit(doit,tmode)
      elseif(isw.eq.2)then

C Pull up the official list of climate files and if the user selects
C one that exists then assign it. If the user picks USER_DEFINED then
C use a file browser to go looking for it.
        H(1)='Climate databases are hourly sets of values from '
        H(2)='1h00 on 1 Jan to 24h00 on 31 December. Please' 
        H(3)='specify which file in the list.'
        iuf=IFIL+1
        INQUIRE (FILE=cdblfil,EXIST=XST)
        if(XST)then
          call rdblist(IUF,longtfile,ier)
          if(ier.eq.0)then
            if(longtfile(1:12).eq.'USER_DEFINED')then
              write(llclmdb,'(a)') 'newclim'
 291          llt=lnblnk(llclmdb)
              iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
              if(iglib.eq.1.or.iglib.eq.3)then
                if(llt.lt.96)then
                  CALL EASKF(llclmdb,' Climate file (esp-r binary)?',
     &              ' ',96,DCLIM,'climate file name',IER,4)
                elseif(llt.ge.96.and.llt.lt.124)then
                  CALL EASKF(llclmdb,' Climate file (esp-r binary)?',
     &              ' ',124,DCLIM,'climate file name',IER,4)
                elseif(llt.ge.124.and.llt.le.144)then
                  CALL EASKF(llclmdb,' Climate file (esp-r binary)?',
     &              ' ',144,DCLIM,'climate file name',IER,4)
                endif
              elseif(iglib.eq.2)then
                CALL EASKF(llclmdb,' Climate file (esp-r binary)?',
     &            ' ',144,DCLIM,'climate file name',IER,4)
              else
                CALL EASKF(llclmdb,' Climate file (esp-r binary)?',
     &            ' ',96,DCLIM,'climate file name',IER,4)
              endif
              IF(llclmdb(1:2).EQ.'  ')GOTO 291
              IER=0

C Cast back to LCLIM.
              llt=lnblnk(llclmdb)
              write(LCLIM,'(a)') llclmdb(1:llt)
              write(longtfile,'(a)') llclmdb(1:llt)
            elseif(longtfile.eq.'UNKNOWN'.or.longtfile(1:2).eq.'  ')then
              call usrmsg('Nothing selected from the list!',' ','W')
              goto 552
            else
              write(LCLIM,'(a)') longtfile(1:lnblnk(longtfile))
            endif
          endif
        else
          call usrmsg('No Climate db list available so using default.',
     &      '(Check with administrator about the `climatelist` file.)',
     &      'W')
          goto 552
        endif

        CALL ERPFREE(ICLIM,ISTAT)
        call FINDFIL(LCLIM,XST)
        IF(XST)THEN

C If file exists and user wants to browse then scan the file and
C use the editing facility.

C << next try using llclmdb for the file opening and then try >>
C << it instead of longtfile >>
          IER=0
          call EFOPRAN(ICLIM,LCLIM,144,0,IER)
          if(ier.eq.0)then
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
            call runit(doit,tmode)
          else
            call usrmsg('Problem encountered with Climate db!',
     &                  'Please check db location and content.','W')
            goto 552
          endif
        else
          call usrmsg('No Climate db to browse/edit.',
     &                'Please use one of the other options.','W')
          goto 552
        endif
      elseif(isw.eq.3)then

C No climate db found so set-up minimal common block, create
C a new db, populate it and present editing facility.

C << revise to support browsing for the file (askf has a cancel option now) >>

        h(1)='The suggested name of this database is based on the '
        h(2)='project name. '
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(longtmp,'(2a)') cfgroot(1:lr),'.climate'
        else
          write(longtmp,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &        cfgroot(1:lr),'.climate'
        endif

C If using X11 assume that the full path is less than 96 chars
C but if using GTK then we need the full length to avoid truncation.
 553    iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      96,DCLIM,'climate file name',IER,3)
        elseif(iglib.eq.2)then
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      144,DCLIM,'climate file name',IER,3)
        else
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      96,DCLIM,'climate file name',IER,3)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

        if(longtmp(1:2).ne.'  '.and.longtmp(1:4).ne.'UNKN')then
          LCLIM=longtmp
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
            call runit(doit,tmode)
          endif
        endif
      elseif(isw.eq.4)then

C Copy the standard db to project folder. Suggest a local
C file name based on the project root name. Lastly do the copy.
C If the default is to be copied ask for local name. Use longtmp for
C the file name as known to esp-r and LCOPY for the system cp call.   
        if(copydef)then
          call edisp(iuout,'The source file is:')
          call edisp(iuout,DCLIM)
          call edisp(iuout,' ')
        else
          call edisp(iuout,'The source file is:')
          call edisp(iuout,LCLIM)
          call edisp(iuout,' ')
        endif
        h(1)='The suggested name of this database is related to the'
        h(2)='active project name.'
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(longtmp,'(2a)') cfgroot(1:lr),'.climate'
        else
          write(longtmp,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &        cfgroot(1:lr),'.climate'
        endif

C If using X11 assume that the full path is less than 96 chars
C but if using GTK then we need the full length to avoid truncation.
 554    iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      96,DCLIM,'climate file name',IER,3)
        elseif(iglib.eq.2)then
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      144,DCLIM,'climate file name',IER,3)
        else
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      96,DCLIM,'climate file name',IER,3)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

        if(longtmp(1:2).ne.'  '.and.longtmp(1:4).ne.'UNKN')then
          LCLIM=longtmp
        else
          goto 554
        endif
        call isunix(unixok)
        IF(longtmp(1:2).NE.'  ')then
          write(LCOPY,'(3a)') path(1:lnblnk(path)),fs,
     &        longtmp(1:lnblnk(longtmp))
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
            call runit(doit,'-')
            LCLIM=longtmp
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
            call runit(doit,'-')
            LCLIM=longtmp
          endif
          moddb=.true.
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
            call runit(doit,tmode)
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
        H(4)='a different operating system or for word processing.'

        DFILE='./climate.a'
        LLASCI=' '
        write(LLASCI,'(2a)') LCLIM(1:lnblnk(LCLIM)),'.a'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      96,DFILE,'climate file name',IER,3)
        elseif(iglib.eq.2)then
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      144,DFILE,'climate file name',IER,3)
        else
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      96,DFILE,'climate file name',IER,3)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

C Convert both binary and asci file names into full paths prior
C to asking clm to do the conversion.
        call addpath(LCLIM,longtfile,concat)
        call addpath(LLASCI,longtafile,concat)
        call tstamp('>','PRJ: start clm for conversion')
        doit = ' '
        write(doit,'(5a)') 'clm -mode text -file ',
     &      longtfile(1:lnblnk(longtfile)),' -act bin2asci silent ',
     &      longtafile(1:lnblnk(longtafile))
        CALL ASKOK(' ','Browse/edit exported climate data?',OK,dok,3)
        if(ok)then
          call runit(doit,tmode)
        endif
      elseif(isw.eq.6)then

C Climate ascii >> binary import
C Confirm suggested name and then write out binary file.
        H(1)='Some databases are held as BINARY files for speed'
        H(2)='of access and security. There are machine dependant'
        H(3)='and can be created from ASCII file versions.'
        DFILE='./climate.a'

        LLASCI=' '
        write(LLASCI,'(2a)') LCLIM(1:lnblnk(LCLIM)),'.a'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      96,DFILE,'climate file name',IER,3)
        elseif(iglib.eq.2)then
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      144,DFILE,'climate file name',IER,3)
        else
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      96,DFILE,'climate file name',IER,3)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

        longtmp=LCLIM
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      96,DCLIM,'climate file name',IER,3)
        elseif(iglib.eq.2)then
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      144,DCLIM,'climate file name',IER,3)
        else
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      96,DCLIM,'climate file name',IER,3)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.
        if(longtmp(1:2).ne.'  '.and.longtmp(1:4).ne.'UNKN')then
          LCLIM=longtmp
        endif

C Convert both binary and asci file names into full paths prior
C to asking clm to do the conversion.
        call addpath(LCLIM,longtfile,concat)
        call addpath(LLASCI,longtafile,concat)
        call tstamp('>','PRJ: start clm for conversion')
        doit = ' '
        write(doit,'(5a)') 'clm -mode text -file ',
     &      longtfile(1:lnblnk(longtfile)),' -act asci2bin silent ',
     &      longtafile(1:lnblnk(longtafile))
        CALL ASKOK(' ','Browse/edit exported climate data?',OK,dok,3)
        if(ok)then
          call runit(doit,tmode)
        endif
      elseif(isw.eq.7)then

C EPW >> binary import
C Confirm suggested name and then write out binary file.
        H(1)='ESP-r can import climate data from EPW format files'
        H(2)='(some editing is required, see instructions in the'
        H(3)='file EPW_readme). This option converts EPW files'
        H(4)='to ESP-r climate files.'
        DFILE='./climate.a'

        LLASCI=' '
        write(LLASCI,'(2a)') LCLIM(1:lnblnk(LCLIM)),'.a'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      96,DFILE,'climate file name',IER,3)
        elseif(iglib.eq.2)then
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      144,DFILE,'climate file name',IER,3)
        else
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      96,DFILE,'climate file name',IER,3)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

        longtmp=LCLIM
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(longtmp,' ',' Climate file name (binary)? ',
     &      96,DCLIM,'climate file name',IER,3)
        elseif(iglib.eq.2)then
          CALL EASKF(longtmp,' ',' Climate file name (binary)? ',
     &      144,DCLIM,'climate file name',IER,3)
        else
          CALL EASKF(longtmp,' ',' Climate file name (binary)? ',
     &      96,DCLIM,'climate file name',IER,3)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.
        if(longtmp(1:2).ne.'  '.and.longtmp(1:4).ne.'UNKN')then
          LCLIM=longtmp
        endif

C Convert both binary and asci file names into full paths prior
C to asking clm to do the conversion.
        call addpath(LCLIM,longtfile,concat)
        call addpath(LLASCI,longtafile,concat)
        call tstamp('>','PRJ: start clm for conversion')
        doit = ' '
        write(doit,'(5a)') 'clm -mode text -file ',
     &      longtfile(1:lnblnk(longtfile)),' -act epw2bin silent ',
     &      longtafile(1:lnblnk(longtafile))
        h(1)='Use the browse function to clarify whether the climate'
        h(2)='data you have selected is appropriate for the current '
        h(3)='model. '
        dok=.false.
        CALL ASKOK(' ','Browse/edit imported climate data?',OK,dok,3)
        if(ok)then
          call runit(doit,tmode)
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
        IREC=366
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)ICYEAR,ICDNGH
        IREC=368
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLAT,CLONG
        CALL ERPFREE(ICLIM,ISTAT)
      endif
      write(outs,'(A,F7.3,A,F7.3,a,i4,i5,a,2F7.3,a)')
     &   'The climate lat is ',clat,
     &   ' & long diff is ',clong,' & Year & DN flag ',ICYEAR,ICDNGH,
     &   '. The model currently uses ',xlat,xlon,'.'
      dok=.false.
      h(1)='Typically the project location will not deviate'
      h(2)='by much from the location of Climate db.'
      CALL ASKOK(outs,
     &  'Update your model with climate data?',OK,DOK,3)
      IF(OK)THEN
        XLAT=CLAT
        XLON=CLONG
        WRITE(OUTS,'(A,F6.2,A,F6.2,A)')'A latitude of ',xlat,
     &    ' and longitude diff of ',xlon,
     &    ' is associated with the model.'
        CALL EDISP(IUOUT,OUTS)
        CALL EDISP(IUOUT,' ')
      ENDIF

C Update year 
      h(1)='The year associated with a model primarily tells the'
      h(2)='simulator what day of the week is 1 January. '
      h(3)=' '
      h(4)='IF YOU CHANGE the model year remember to save '
      h(5)='your model before you exit the project manager. '
      h(6)=' '
      H(7)='The choice is year impacts the day of the week for'
      H(8)='1 January. It also will alter the day type associated'
      H(9)='of days in the calendar which are the standard default'
      H(10)='weekday/saturday/sunday (but not user defined day types.'
      dok=.false.
      WRITE(OUTS,'(A,I4,a,I4,a)')'The climate year is ',ICYEAR,
     &  '. and the model year is ',IYEAR,'.'      
      CALL ASKOK(OUTS,
     &  'Use climate year in your model? (see help)',OK,DOK,10)
      IF(OK)THEN
        IYEAR=ICYEAR
        WRITE(OUTS,'(I4,A)')IYEAR,' will be used as simulation year.'
        CALL EDISP(IUOUT,OUTS)
        CALL EDISP(IUOUT,' ')
        if(nbdaytype.eq.0)then
          call calenmanage('i',ier)
        elseif(nbdaytype.eq.3)then
          call calenmanage('i',ier)
        elseif(nbdaytype.gt.3)then

C Perform check that calendar day types correspond with simulation year
C If more than three day types are declared then retain those day types
C otherwise overwrite. (Same logic as in esystem.F 
          ICREPORT=0
          DO 123 IDOL=1,365
            IF(ICALENDER(IDOL).LE.3)THEN
              CALL EDAYR(IDOL,IDAYNUM,IMTHNUM)
              CALL EWEEKD(IDAYNUM,IMTHNUM,IYEAR,IDWKNUM)
              IF(IDWKNUM.LT.6)THEN
                IDTYY=1 ! WEEKDAY
              ELSEIF(IDWKNUM.EQ.6)THEN
                IDTYY=2 ! SATURDAY
              ELSEIF(IDWKNUM.EQ.7)THEN
                IDTYY=3 ! SUNDAY
              ENDIF
              IF(ICALENDER(IDOL).NE.IDTYY)THEN
                ICALENDER(IDOL)=IDTYY
                ICREPORT=1
              ENDIF
            ENDIF
 123      CONTINUE
          IF(ICREPORT.EQ.1)CALL EDISP(IUOUT,
     &      'Simulation year and calendar mismatch rectified')
        endif
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
  774 FORMAT(' Error ',I7,' reading Climate db information.')
      call edisp(iuout,outs)
      call edisp(iuout,' ')
      RETURN

 1001 WRITE(outs,775)ISTAT
  775 FORMAT(' Error ',I7,' writing Climate db year.')
      call edisp(iuout,outs)
      call edisp(iuout,' ')
      RETURN

 1002 WRITE(outs,777)ISTAT
  777 FORMAT(' Error ',I7,' writing Climate db.')
      call edisp(iuout,outs)
      call edisp(iuout,' ')
      
      RETURN
      END
