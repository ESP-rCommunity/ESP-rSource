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
C EDDBMATERIAL Changes the materials database current cfg file references.
C EDDBMLCON Changes the Constructions db references in the cfg file.
C EDDBPCDB Changes the plant component database current cfg file references.
C EDDBPROFILE Changes the events profile database current cfg file references.
C EDDBOPT Changes the optical database current cfg file references.
C EDDBCLM: Changes the climate database current cfg file references.
C EDDBMSC Changes the miscel comp database current cfg file references.

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
      common/OUTIN/IUOUT,IUIN

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      character cfgroot*24,LCFGF*72
      common/C21/IFCFG,cfgroot,LCFGF

      common/rpath/path
      common/user/browse

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      logical OK,DOK,XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical moddb,browse,modcon,QUIET
      logical haveconstr
      integer ihd  ! for radio button
      integer NITMS,INO ! max items and current menu item

      character path*72
      character*72 ITEMS(16)
      character lworking*144  ! for processing database file names
      
C Strings for dos version of source and destination of db file.
C      character rootpdos*72,upathdos*72

#ifdef OSI
      integer impx,impy,iwe
#else
      integer*8 impx,impy,iwe
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDDB'  ! set for subroutine

      moddb=.false.
      modcon=.false.

C Begin with menu of the current databases. If there is
C currently no model focus then use calls to findwhichdbpath
C to establish whether standard (2) or local (1) or (0) not
C yet defined.
      if(cfgroot(1:2).ne.'  '.and.cfgroot(1:4).ne.'UNKN')then
        continue
      else
        lworking=LAPRES
        call findwhichdbpath('prs',lworking,ier)
        lworking=LFMAT
        call findwhichdbpath('mat',lworking,ier)
        lworking=LFMUL
        call findwhichdbpath('mul',lworking,ier)
        lworking=LPCDB
        call findwhichdbpath('pdb',lworking,ier)
        lworking=LPRFDB
        call findwhichdbpath('evn',lworking,ier)
        lworking=LOPTDB
        call findwhichdbpath('opt',lworking,ier)
        lworking=MCMPDBFL
        call findwhichdbpath('msc',lworking,ier)
      endif
 
    3 INO=-4
      IER=0

      if ( bMY_climates_defined ) then
        cTemp = cMY_climate_db_name
      else
        cTemp = 'None'
      endif
      
      lndbp=lnblnk(standarddbpath)
      if(lndbp.lt.45)then
        WRITE(ITEMS(1),'(2A)')'  standard db path (std): ',
     &    standarddbpath(1:lndbp)
      else
        WRITE(ITEMS(1),'(2A)')'  standard db path (std): ',
     &    standarddbpath(1:45)
      endif
      if(cfgroot(1:2).ne.'  '.and.cfgroot(1:4).ne.'UNKN')then
        lnpth=lnblnk(path)
        lndbpth=lnblnk(dbspth)
        if(lnpth+lndbpth.lt.45)then
          WRITE(ITEMS(2),'(3A)')
     &      '  model db path (mod): ',path(1:lnpth),dbspth(1:lndbpth)
        else
          if(lnpth.lt.40)then
            WRITE(ITEMS(2),'(3A)')
     &        '  model db path (mod): ',path(1:lnpth),dbspth(1:5)
          else
            WRITE(ITEMS(2),'(2A)')
     &        '  model db path (mod): ',path(1:lnpth)
          endif
        endif
      else
        WRITE(ITEMS(2),'(A)')
     &    '  model db path (mod): no model defined yet'
      endif
      ITEMS(3) ='  _______________________________ '
      WRITE(ITEMS(4),'(2A)')  'a annual weather        : ',LCLIM(1:43)
      WRITE(ITEMS(5),'(2A)')  'b multi-year weather    : ',cTemp(1:43)
      if(ipathapres.eq.0)then
        WRITE(ITEMS(6),'(2A)')'c pressure distribution : ',
     &    LAPRES(1:43)
      elseif(ipathapres.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(6),'(2A)')'c pressure distribution : <mod> ',
     &    LAPRES(lndbpth:38)
      elseif(ipathapres.eq.2)then
        WRITE(ITEMS(6),'(2A)')'c pressure distribution : <std> ',
     &    LAPRES(1:38)
      endif
      if(ipathmat.eq.0)then
        WRITE(ITEMS(7),'(2A)')'d materials             : ',
     &    LFMAT(1:43)
      elseif(ipathmat.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(7),'(2A)')'d materials             : <mod> ',
     &    LFMAT(lndbpth:38)  ! try to print file name only
      elseif(ipathmat.eq.2)then
        WRITE(ITEMS(7),'(2A)')'d materials             : <std> ',
     &    LFMAT(1:38)
      endif

      if(ipathmul.eq.0)then
        WRITE(ITEMS(8),'(2A)')'e constructions         : ',
     &    LFMUL(1:43)
      elseif(ipathmul.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(8),'(2A)')'e constructions         : <mod> ',
     &    LFMUL(lndbpth:38)
      elseif(ipathmul.eq.2)then
        WRITE(ITEMS(8),'(2A)')'e constructions         : <std> ',
     &    LFMUL(1:38)
      endif
      if(ipathpcdb.eq.0)then
        WRITE(ITEMS(9),'(2A)')'f plant components      : ',
     &    LPCDB(1:43)
      elseif(ipathpcdb.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(9),'(2A)')'f plant components      : <mod> ',
     &    LPCDB(lndbpth:38)
      elseif(ipathpcdb.eq.2)then
        WRITE(ITEMS(9),'(2A)')'f plant components      : <std> ',
     &    LPCDB(1:38)
      endif
      if(ipathprodb.eq.0)then
        WRITE(ITEMS(10),'(2A)')'g event profiles        : ',
     &    LPRFDB(1:43)
      elseif(ipathprodb.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(10),'(2A)')'g event profiles        : <mod> ',
     &    LPRFDB(lndbpth:38)
      elseif(ipathprodb.eq.2)then
        WRITE(ITEMS(10),'(2A)')'g event profiles        : <std> ',
     &    LPRFDB(1:38)
      endif
      if(ipathoptdb.eq.0)then
        WRITE(ITEMS(11),'(2A)')'h optical properties    : ',
     &    LOPTDB(1:43)
      elseif(ipathoptdb.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(11),'(2A)')'h optical properties    : <mod> ',
     &    LOPTDB(lndbpth:38)
      elseif(ipathoptdb.eq.2)then
        WRITE(ITEMS(11),'(2A)')'h optical properties    : <std> ',
     &    LOPTDB(1:38)
      endif
      if(ipathmould.eq.0)then
        WRITE(ITEMS(12),'(2A)')'i mould isopleths       : ',
     &    lfmould(1:43)
      elseif(ipathmould.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(12),'(2A)')'i mould isopleths       : <mod> ',
     &    lfmould(lndbpth:38)
      elseif(ipathmould.eq.2)then
        WRITE(ITEMS(12),'(2A)')'i mould isopleths       : <std> ',
     &    lfmould(1:38)
      endif

      if(ipathmsc.eq.0)then
        WRITE(ITEMS(13),'(2A)')'j special components    : ',
     &    MCMPDBFL(1:43)
      elseif(ipathmsc.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(13),'(2A)')'j special components    : <mod> ',
     &    MCMPDBFL(lndbpth:38)
      elseif(ipathmsc.eq.2)then
        WRITE(ITEMS(13),'(2A)')'j special components    : <std> ',
     &    MCMPDBFL(1:38)
      endif
      ITEMS(14)='  _______________________________ '
      ITEMS(15)='? help                            '
      ITEMS(16)='- exit this menu                  '

C Try to compact the width of the menu.
      IW=0
      do 141 ij=1,16
        if(lnblnk(ITEMS(ij)).gt.IW)IW=lnblnk(ITEMS(ij))
 141  continue

C Help text for this menu.
      helptopic='database_mgt_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
      NITMS=16
      if(MMOD.EQ.8)then
        impx=0
        impy=0
        iwe=iw
        CALL VWMENU('Database Maintenance',ITEMS,NITMS,impx,impy,iwe,
     &    irpx,irpy,INO)
      else
        CALL EMENU('Database Maintenance',ITEMS,NITMS,INO)
      endif
      IF(INO.EQ.16)THEN
        if(browse)return   ! no change since browsing

        if(cfgroot(1:2).ne.'  '.and.cfgroot(1:4).ne.'UNKN')then
          continue
        else

C No model yet so reset to the default names and exit.
          write(LCLIM,'(a)') DCLIM(1:lnblnk(DCLIM))
          write(LAPRES,'(a)') DAPRES(1:lnblnk(DAPRES))
          write(LFMAT,'(a)') DFCON(1:lnblnk(DFCON))
          write(LFMUL,'(a)') DFMUL(1:lnblnk(DFMUL))
          write(LOPTDB,'(a)') DOPTDB(1:lnblnk(DOPTDB))
          write(LPRFDB,'(a)') DPRFDB(1:lnblnk(DPRFDB))
          write(LPCDB,'(a)')  DPCDB(1:lnblnk(DPCDB))
          write(LSBEM,'(a)')  DSBEM(1:lnblnk(DSBEM))  ! assign default SBEM db
          write(MCMPDBFL,'(a)') DMCMPDBFL(1:lnblnk(DMCMPDBFL))
          write(lfmould,'(a)') dmdbnam(1:lnblnk(dmdbnam))
          return
        endif

C If a model has been loaded and there are possible changes ask
C user for confirmation.
        if(cfgok.and.moddb)then
          dok=.true.
          CALL ASKOK(
     &      'A possible change in database names detected. Update the',
     &      'model database NAME LIST to match? (see help)',
     &      OK,DOK,nbhelp)
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
              CALL ASKOK(
     &         'Rebuild existing zone construction files to reflect',
     &         'database changes? (see help)',OK,dok,nbhelp)
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

      elseif(INO.EQ.4)then

C Climate database.
        call eddbclm(moddb,'      ',-1)

      ELSEIF(INO.eq.5)THEN

C Multi-year climate database - only works in conjunction
C with save level 5.
         call usrmsg('The use of multi-year assessments is',
     &               'limited to results save level 5.','W')
         call MY_clm_db_menu()
        
      ELSEIF(INO.EQ.6)THEN

C Pressure distribution db.
        call eddbpres(moddb,'    ')

      ELSEIF(INO.EQ.7)THEN

C Manage materials db and if a model loaded then warn user 
C if db name or contents have changed.
        call eddbmaterial(moddb,modcon,'    ')
        if(cfgroot(1:2).ne.'  '.and.cfgroot(1:4).ne.'UNKN')then
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
        endif

      ELSEIF(INO.EQ.8)THEN

C Deal with construction databases.
        call eddbmlcon(moddb,modcon,'    ')
        if(cfgroot(1:2).ne.'  '.and.cfgroot(1:4).ne.'UNKN')then
          if(modcon)then
            call usrmsg(
     &      'The database details might have changed. In a moment',
     &      'you will be offered the option to update zone files.','W')
          endif
        endif

      ELSEIF(INO.EQ.9)THEN

C Plant template database.
        call eddbpcdb(moddb,'    ')
        if(cfgroot(1:2).ne.'  '.and.cfgroot(1:4).ne.'UNKN')then
          if(moddb)then
            call edisp(iuout,
     &      'The database name or details have changed. If you')
            call edisp(iuout,
     &      'want to keep it use the `save model` option.')
            call edisp(iuout,' ')
          endif
        endif

      ELSEIF(INO.EQ.10)THEN

C Profiles database.
        call eddbprofile(moddb,'    ')
        if(cfgroot(1:2).ne.'  '.and.cfgroot(1:4).ne.'UNKN')then
          if(moddb)then
            call edisp(iuout,
     &      'The database name or details have changed. If you')
            call edisp(iuout,
     &      'want to keep it use the `save model` option.')
            call edisp(iuout,' ')
          endif
        endif

      ELSEIF(INO.EQ.11)THEN

C Optical database.
        call eddbopt(moddb,'    ')
        if(cfgroot(1:2).ne.'  '.and.cfgroot(1:4).ne.'UNKN')then
          if(moddb)then
            call edisp(iuout,
     &      'The database name or details have changed. If you')
            call edisp(iuout,
     &      'want to keep it use the `save model` option.')
            call edisp(iuout,' ')
          endif
        endif

      ELSEIF(INO.EQ.12)THEN

C Mould isopleths.
        call EDDBMOULD(moddb,'    ')

      ELSEIF(INO.EQ.13)THEN

C Manage common active components file.
        call EDDBMSC(moddb,'    ')

      ELSEIF(INO.EQ.15)THEN

C Menu help text.
        helptopic='database_mgt_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call phelpd('databases overview',nbhelp,'-',0,0,IER)
        ihd=1
        call easkatog(' ','Help options:','overview','climate',
     &   'materials','constructions','optics','plant','cancel',ihd,1)
        if(ihd.eq.1)then
          helptopic='database_mgt_overview'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('databases overview',nbhelp,'-',0,0,IER)
        elseif(ihd.eq.2)then
          helptopic='database_clm_overview'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('climate overview',nbhelp,'clm_dbitems ',0,0,IER)
        elseif(ihd.eq.3)then
          helptopic='database_mat_overview'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('materials db overview',nbhelp,'materials_db',
     &      0,0,IER)
        elseif(ihd.eq.4)then
          helptopic='database_mlc_overview'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('constr db overview',nbhelp,'mlc_database',
     &      0,0,IER)
        elseif(ihd.eq.5)then
          helptopic='database_optic_overview'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('optics overview',nbhelp,'optical_db',0,0,IER)
        elseif(ihd.eq.6)then
          helptopic='database_plant_overview'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('plant db overview',nbhelp,'plant_db_fld',
     &      0,0,IER)
        endif

      ELSE
        INO=-4
        GOTO 3
      ENDIF

      INO=-4
      GOTO 3

      END

C ********** opendb
C NOTE: if this code change - be sure to update the copy of this
C subroutine embedded within esruish/ish.F and esrueco/ecoesp.F

C Open materials constructions, multi-layer constructions and
C optical properties databases. In the case of materials, first
C assume it is a binary file, check its contents and if a problem
C then scan new ascii format and if that does not work try the
C older ascii materials file to fill the materials data arrays.
C If sucessful the material common blocks will be filled and
C closemat1 or closemat2 will be set.

      subroutine opendb(ier)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      logical XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical closemat1,closemat2
      character LASCI*144,DFILE*144,fs*1
      character SOPT*12,outs248*248,GDESCR*36
      character t144*144   ! for use with erprcdb
      character lworking*144 
      integer lndbp   ! for length of standard database path
      logical unixok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='opendb'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Scan the binary file data into materials commons and if this was
C sucessful and matver was set to 1.1 in matformbin then we can
C carry on using the materials common blocks for subsequent access.
      call scananymat(ier)
      call eclose(matver,1.1,0.001,closemat1)
      call eclose(matver,1.2,0.001,closemat2)

C Set help text for this subroutine.
      helptopic='many_mat_not_found'
      call gethelptext(helpinsub,helptopic,nbhelp)
        
C Read multilayer db information into common depending on its location.
      CALL ERPFREE(IFMUL,ISTAT)
      if(ipathmul.eq.0.or.ipathmul.eq.1)then
        lworking=lfmul  ! use as is
      elseif(ipathmul.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    lfmul(1:lnblnk(lfmul))  ! prepend db folder path
      endif
      call FINDFIL(lworking,XST)
      if(XST)then
        CALL ERMLDB(0,IUOUT,IER)
        IF(IER.eq.1)then
          write(outs248,'(3a)') 
     &    ' Problems with materials used by Constructions db',
     &     LFMAT(1:lnblnk(LFMAT)),'!'
          call edisp248(iuout,outs248,100)
          call edisp(iuout,' ')
          MLDBOK=.FALSE.
        ELSEIF(IER.eq.2)then

C There were lots of undefined materials so likely the Materials
C database is for the wrong machine type. Guess the name of
C the ascii version and if it exists, offer to convert it or use it.
          IAF=IFIL+1
          LASCI=' '
          if(ipathmat.eq.0.or.ipathmat.eq.1)then
            write(LASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
          elseif(ipathmat.eq.2)then
            lndbp=lnblnk(standarddbpath)
            write(LASCI,'(4a)') standarddbpath(1:lndbp),fs,
     &        LFMAT(1:lnblnk(LFMAT)),'.a'
          endif

C Label 155 is a jump back point for the case of users supplying a name.
  155     CALL EFOPSEQ(IAF,LASCI,1,IER)
          if(ier.eq.0)then

C If the ascii version exists there is no need to convert it just
C scan it into common blocks.
            call rascimat(IAF,LASCI,IER)  ! try current ascii format
            if(ier.eq.-2)then
              ier=0
              call rlegacymat(IAF,LASCI,ier)  ! try older ascii format
              if(ier.ne.0)then
                call usrmsg('No readable Materials database was',
     &                      'found or the file was corrupt.','W')
                MATDBOK=.FALSE.
              else
                call eclose(matver,1.1,0.001,closemat1)
                call eclose(matver,1.1,0.001,closemat2)
                if(closemat1.or.closemat2)then

C Materials data in place, set this as the new file name and then
C try and re-scan the Constructions database.
                  call usrmsg(
     &              'An ASCII Materials db was found and used.',
     &              'Re-scanning constructions...','P')
                  MATDBOK=.TRUE.
                  write(LFMAT,'(a)') LASCI(1:lnblnk(LASCI))
                  CALL ERPFREE(IFMUL,ISTAT)

C Search again for lworking.
                  call FINDFIL(lworking,XST)
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
     &              'No readable Material db was found.',
     &              'Check other warnings for advice.','W')
                  MATDBOK=.FALSE.
                endif
              endif
            elseif(ier.eq.0)then

C Materials data in place, set this as the new file name and then
C try and re-scan the constructions database.
              call usrmsg(
     &          'An ASCII Materials db was found and used.',
     &          'Re-scanning constructions...','P')
              MATDBOK=.TRUE.
              write(LFMAT,'(a)') LASCI(1:lnblnk(LASCI))
              CALL ERPFREE(IFMUL,ISTAT)

C Search again for lworking.
              call FINDFIL(lworking,XST)
              if(XST)then
                CALL ERMLDB(0,IUOUT,IER)
                if(ier.eq.0)then
                  MLDBOK=.TRUE.
                  call usrmsg(
     &              'An ASCII Materials db was found and used.',
     &              'Re-scanning constructions...done.','-')
                else
                  MLDBOK=.FALSE.
                  call usrmsg(
     &              'An ASCII Materials db was found and used.',
     &              'Re-scanning constructions...failed.','W')
                endif
              endif
            endif
          else

C Ask user for ascii Materials database to convert.
            IAF=IFIL+1
            DFILE=' '
            CALL EASKS(LASCI,' ','Materials database (ASCII)?',
     &        144,DFILE,'materials db (ascii)',IER,nbhelp)
            goto 155
          endif
        elseif(IER.eq.3)then
          write(outs248,'(3a)') 
     &    ' Not enough constructions found in Constructions db',
     &     lworking(1:lnblnk(lworking)),'!'
          call edisp248(iuout,outs248,100)
          call edisp(iuout,' ')
          MLDBOK=.FALSE.
        else

C Scan was ok so set mldbok to true.
          MLDBOK=.TRUE.
        endif
      else

C Could not find Constructions db at this time, report to user.
        write(outs248,'(3a)') 'Constructions db ',
     &     LFMUL(1:lnblnk(LFMUL)),' not found!'
        call edisp248(iuout,outs248,100)
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


C ********** EDDBMATERIAL
C Eddbmaterial Changes the materials database current cfg file references.
C moddb is from version manager
C modcon signals change in constructions.
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.

      SUBROUTINE EDDBMATERIAL(moddb,modcon,APP)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN

      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path

C Default ESP-r distribution path.
      common/deflt4/dinstpath

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical moddb,modcon,copydef,clkok,chgdb
      logical closemat1,closemat2

      character LLTMP*144,LTMP*92,lguess*144
      character fs*1,message*48
      character doit*300,LCOPY*144
      character DFILE*144,LLASCI*144,LLTMPFL*144
      character lprev*144
      character longtfile*144
      character lworking*144
      character dirpath*48
      integer iglib  ! for detecting GTK or X11

      CHARACTER outs248*248,APP*4
      LOGICAL OK,XST,DOK,concat
      character cfgroot*24,LCFGF*72
      character dinstpath*48,path*72
      character sourcefile*144  ! the file to copy

C Local strings for user selections dependent on graphic library.
      character opte*28,optf*28,optg*28
      character lpath*72,fname*72 ! for use with fdroot

      integer lndbp   ! for length of standard database path
      integer ier
      logical unixok  ! to check for database path file separators

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDDBMATERIAL'  ! set for subroutine

C Clear string buffers.
      ltmp=' '
      lltmp=' '

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
C Take into account the current value of ipathmat.
      lndbp=lnblnk(standarddbpath)
      if(ipathmat.eq.0.or.ipathmat.eq.1)then
        write(lprev,'(a)') LFMAT(1:lnblnk(LFMAT))
      elseif(ipathmat.eq.2)then
        write(lprev,'(3a)') standarddbpath(1:lndbp),fs,
     &    LFMAT(1:lnblnk(LFMAT))
      endif

C Debug.
C      write(outs248,*) 'Current materials ',lprev(1:lnblnk(lprev))
C      call edisp248(iuout,outs248,100)

C Setup lltmp to hold the full path to the file for use in dialog.
  60  continue
      if(ipathmat.eq.0.or.ipathmat.eq.1)then
        lltmp=lfmat  ! use as is
      elseif(ipathmat.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    lfmat(1:lnblnk(lfmat))  ! prepend db folder path
      endif

      helptopic='mat_db_edit_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C If GTK library then there are additional options so include in help.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.

C If local or absolute path call addpath otherwise if a standard
C database then set longtfile equal to LLTMP.
      if(ipathmat.eq.0.or.ipathmat.eq.1)then
        call addpath(lltmp,longtfile,concat)
      elseif(ipathmat.eq.2)then
        longtfile=lltmp
      endif
      ltf=max(1,lnblnk(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up additional options depending on graphics lib. If using
C text or X11 option g edits the string while for GTK there are two
C options to browse for files in ../dbs or the esp-r distribution.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        opte='g edit file name'
        optf='   '
        optg='   '
      elseif(iglib.eq.2)then
        opte='g browse model ../dbs'
        optf='h browse esp-r distribution'
        optg='i make copy of another file'
      endif

      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Materials db',
     &    ' ','b select from list',
     &    'c create a new db','d copy standard db',
     &    'e binary >> ascii convert','f ascii >> binary convert',
     &    opte,optf,optg,' ',' ',' ',isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.9)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If corportate database then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Materials db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    'e binary >> ascii convert','f ascii >> binary convert',
     &    opte,optf,optg,' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If corportate database then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Materials db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    'e binary >> ascii convert','f ascii >> binary convert',
     &    opte,optf,optg,' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default database or the current database.
        idno=1
        isw=0
        call MENUATOL('  ','Materials db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    'e binary >> ascii convert','f ascii >> binary convert',
     &    opte,optf,optg,' ',' ',' ',isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.9)copydef=.false.
      endif

C Act on the user's choice. If no choice return to calling menu.
      if(isw.eq.0)then
        return
      elseif(isw.eq.1)then

C If file exists read it and enter editing facility after
C check to see if it is local or standard or absolute path.
        if(XST)then
          call findwhichdbpath('mat',lltmp,ier)
          goto 44
        else
          call edisp(iuout,
     &    'No file to browse/edit. Please use another option.')
          LFMAT=lprev
          goto 60
        endif

      elseif(isw.eq.2)then

C There is no select by list for materials database.
        call edisp(iuout,
     &    'No list select for materials databases.')
        goto 60

      elseif(isw.eq.3)then

C User requested the creation of a new materials db. Set up minimal
C set of categories and items, write this to a scratch file and
C then scan that into material arrays and create a new db and offer editing.
        lr=lnblnk(cfgroot)
        LTMP=' '
        LLTMP=' '
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.materialdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.materialdb'
        endif
        clkok=.false.
 62     clkok=.false.
        CALL EASKSCNCL(ltmp,' ','New materials db?',
     &    'cancel',clkok,92,DFCON,'new materials db',
     &    IER,nbhelp)
        if(clkok)then
          LFMAT=lprev  ! user canceled, restore initial name
          return
        endif
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          write(LFMAT,'(a)') ltmp(1:lnblnk(ltmp))
          ipathmat=1  ! this is a local materials database.
        else
          goto 62
        endif

        CALL ERPFREE(IFMAT,ISTAT)
        MATDBOK=.TRUE.
        moddb=.true.
        modcon=.true.

C Write a scratch file which can be re-scanned into common blocks.
C Materials db. Include typical categories with one item in each.
C The scratch file is in the format of an older ASCII materials
C database which will then be scanned into materials arrays.

C << Need to update this scratch file to use a newer format
C << of ascii Materials db or just assign the data arrays
C << directly.

        lr=lnblnk(cfgroot)
        LLTMPFL=' '
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LLTMPFL,'(2a)') cfgroot(1:lr),'.scratch'
        else
          write(LLTMPFL,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.scratch'
        endif
        IAF=IFIL+1
        CALL EFOPSEQ(IAF,LLTMPFL,3,IER)
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

C After creating the new database file close the file unit.
        CALL ERPFREE(IAF,ISTAT)
        call pausems(400)

C Now scan the scratch file into material arrays.
        ier=0
        call rlegacymat(IAF,LLTMPFL,ier)
        if(ier.ne.0)then
          call usrmsg('The initial materials database data could',
     &                'not be scanned into memory!','W')
        else
          call usrmsg('An initial set of material categories has been',
     &                'setup and you can modify as required.','-')
        endif

C And now write out the database from the materials arrays. mkascimat
C expects to be passed the expanded file name.
        CALL ERPFREE(IAF,ISTAT)
        CALL ERPFREE(IFMUL,ISTAT)  ! in case other unit is still open
        LLTMPFL=' '
        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          write(LLTMPFL,'(a)') LFMAT(1:lnblnk(LFMAT))
        elseif(ipathmat.eq.2)then
          lndbp=lnblnk(standarddbpath)
          write(LLTMPFL,'(3a)') standarddbpath(1:lndbp),fs,
     &      LFMAT(1:lnblnk(LFMAT))
        endif
        CALL mkascimat(IAF,LLTMPFL,IER)

        dok=.true.
        CALL ASKOK(' ','Browse or edit new database?',OK,dok,nbhelp)
        if(OK)then
          moddb=.true.
          goto 44
        endif

      elseif(isw.eq.4.or.isw.eq.9)then

C Copy a database to project folder. If iws is 4 then the
C user has asked for the default database. If isw is 9 then the
C user wishes to browse for the source file. Suggest a local
C file name based on the project root name. Lastly do the copy.
C If the default is to be copied ask for local name. Use LTMP for
C the file name as known to esp-r and LCOPY for the system cp call.
C Remember the file to be copied as sourcefile so that subsequent
C code is general rather than specific (for later consolidation).   
        if(copydef)then
          write(outs248,'(2a)') 'The source file is:',
     &      DFCON(1:lnblnk(DFCON))
          call edisp248(iuout,outs248,100)
          sourcefile = DFCON
        else
          lltmp='  '
          lguess='  '
          if(ipathmat.eq.0.or.ipathmat.eq.1)then
            write(lguess,'(a)') LFMAT(1:lnblnk(LFMAT))
          elseif(ipathmat.eq.2)then
            write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &        LFMAT(1:lnblnk(LFMAT))
          endif
          call edisp(iuout,'  ')  ! echo blank line
          CALL EASKXORGTKF(lguess,
     &      'Materials file name (source)?',' ',
     &      DFCON,lltmp,'materials database',IER,nbhelp)

          sourcefile = lltmp   ! remember this

C If user request jump back and re-display the menu.
          if(ier.eq.-3)then
            goto 60  ! cancel detected, restore name and redisplay menu.
          endif
          ipathmat=1  ! signal that it is a local database.
        endif

        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.materialdb'
        elseif(dbspth(1:3).eq.'../')then
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.materialdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.materialdb'
        endif
   66   CALL EASKS(LTMP,
     &    'Database name after it is copied into the project?',
     &    ' ',92,'material.db','copied material database',IER,nbhelp)
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

C Now copy sourcefile to lcopy.
          doit = ' '
          if(unixok)then
            write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &        sourcefile(1:lnblnk(sourcefile)),' ',
     &        LCOPY(1:lnblnk(LCOPY))
          else

C Standard dos copy logic.
            message='getting materials file from '
            call dblongdoscopy(sourcefile,lcopy,message,doit,ier)
          endif
          call usrmsg('copying default file via',doit,'-')
          call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
          call FINDFIL(LCOPY,XST)
          if(XST)then
            call fdroot(lcopy,lpath,fname)
            write(LFMAT,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          else
            call FINDFIL(LTMP,XST)
            if(XST)then
              write(LFMAT,'(a)') LTMP(1:lnblnk(LTMP))
            else
              write(LFMAT,'(a)') LTMP(1:lnblnk(LTMP))
              call edisp(iuout,'Problem locating the new database.')
              call edisp(iuout,'Try browsing ../dbs to find it.')
              call edisp(iuout,' ')
            endif
          endif
          moddb=.true.
          ipathmat=1  ! signal that it is a local database.
          goto 44
        else
          goto 66
        endif

      elseif(isw.eq.5)then

C Materials db: BINARY > ASCII. Open the ascii file on IAF=IFIL+1.

C Do not assume that LFMAT is necessarily the binary file
C the user may have been working earlier with an ascii version
C revise to request the name of the binary file before opening it.

C << what about value of ipathmat? >>

        write(LLTMP,'(a)') LFMAT(1:lnblnk(LFMAT))  ! remember current file name
        lsn2=MIN0(lnblnk(LLTMP),92)
        LTMP=' '
        write(LTMP,'(a)') LLTMP(1:lsn2)
        CALL EASKSCNCL(LTMP,'Materials db (binary)?',' ',
     &    'cancel',clkok,92,DFILE,'materials database (bin)',IER,nbhelp)
        if(clkok) return

C Open binary Materials db based on value of ipathmat.
C << cast back from shorter buffer >>
        write(LFMAT,'(a)') LTMP(1:lnblnk(LTMP))
        IAF=IFIL+1
        IER=0
        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          ier=0
          call EFOPRAN(IFMAT,LFMAT,40,1,IER)
        elseif(ipathmat.eq.2)then
          ier=0
          lndbp=lnblnk(standarddbpath)
          write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &      LFMAT(1:lnblnk(LFMAT))
          call EFOPRAN(IFMAT,lworking,40,1,IER)
        endif
        if(ier.ne.0)then
          write(LFMAT,'(a)') LLTMP(1:lnblnk(LLTMP))  ! restore previous
          call edisp(iuout,'Restoring the previous materials db')
          call edisp(iuout,LLTMP)
          goto 60    ! jump back and reload and re-display
        endif
        MATDBOK=.TRUE.

C Confirm suggested name and then write out ascii file.
        DFILE='./newmat.db1.a'
        clkok=.false.
        LLASCI=' '
        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          write(LLASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
        elseif(ipathmat.eq.2)then
          lndbp=lnblnk(standarddbpath)
          write(LLASCI,'(4a)') standarddbpath(1:lndbp),fs,
     &      LFMAT(1:lnblnk(LFMAT)),'.a'
        endif

C << temporary shorter string until EASKXORGTKF >>
        lsn2=MIN0(lnblnk(LLASCI),92)
        LTMP=' '
        write(LTMP,'(a)') LLASCI(1:lsn2)
        CALL EASKSCNCL(LTMP,'Materials db (ascii)?',' ',
     &    'cancel',clkok,92,DFILE,'materials database (ascii)',
     &    IER,nbhelp)
        if(clkok)then
          write(LFMAT,'(a)') LLTMP(1:lnblnk(LLTMP))  ! restore previous
          call edisp(iuout,'Restoring the previous materials db')
          call edisp(iuout,LLTMP)
          goto 60    ! jump back and reload and re-display
        else
          write(LLASCI,'(a)') LTMP(1:lnblnk(LTMP))
          call eclose(matver,1.1,0.001,closemat1)
          call eclose(matver,1.2,0.001,closemat2)
          if(closemat1.or.closemat2)then
            CALL ERPFREE(IFMUL,ISTAT)  ! in case other unit is still open
            CALL mkascimat(IAF,LLASCI,IER)
          else
            CALL MKAPRC(LLASCI,IAF,IER)
          endif
        endif

C Before returning restore the original database file and
C rescan into common blocks.
        write(LFMAT,'(a)') LLTMP(1:lnblnk(LLTMP)) ! restore previous
        call edisp(iuout,'Restoring the previous materials db')
        call edisp(iuout,LLTMP)
        goto 60    ! jump back and reload and re-display

      elseif(isw.eq.6)then

C Materials db: ASCII > BINARY. Guess that the ascii version has
C the same name as the binary one and is in the same location.
        IAF=IFIL+1
        clkok=.false.
        DFILE=' '
        LLASCI=' '
        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          write(LLASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
        elseif(ipathmat.eq.2)then
          lndbp=lnblnk(standarddbpath)
          write(LLASCI,'(4a)') standarddbpath(1:lndbp),fs,
     &      LFMAT(1:lnblnk(LFMAT)),'.a'
        endif

C << temporary shorter string until EASKXORGTKF >>
        lsn2=MIN0(lnblnk(LLASCI),92)
        LTMP=' '
        write(LTMP,'(a)') LLASCI(1:lsn2)
        CALL EASKSCNCL(LTMP,'Materials db (legacy ascii)?',' ',
     &    'cancel',clkok,92,DFILE,'materials db (ascii)',IER,nbhelp)
        if(clkok) return
        write(LLASCI,'(a)') LTMP(1:lnblnk(LTMP))

        DFILE='./newmaterial.db1'
        clkok=.false.
        lltmp=' '; ltmp=' '
        write(lltmp,'(a)') LFMAT(1:lnblnk(LFMAT))
        write(ltmp,'(a)') LFMAT(1:lnblnk(LFMAT))
 143    CALL EASKSCNCL(ltmp,'Materials db (legacy binary)?',' ',
     &    'cancel',clkok,92,DFILE,'Materials db (binary)',IER,nbhelp)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          write(LFMAT,'(a)') ltmp(1:lnblnk(ltmp))
        else
          goto 143
        endif
        if(clkok)then
          write(LFMAT,'(a)') lltmp(1:lnblnk(lltmp))  ! restore original file name
          call edisp(iuout,'Restoring the previous materials db')
          call edisp(iuout,LLTMP)
          goto 60    ! jump back and reload and re-display
        else
          lastpathmat=ipathmat
          ipathmat=0 ! reset to expect full path for binary
          CALL MKBPRC(IAF,LLASCI,IER)
          write(LFMAT,'(a)') lltmp(1:lnblnk(lltmp))  ! restore original file name
          call edisp(iuout,'Restoring the previous materials db')
          call edisp(iuout,LLTMP)
          ipathmat=lastpathmat
          goto 60    ! jump back and reload and re-display
        endif

      elseif(isw.eq.7)then

C Make up browse to the model ../dbs folder suggest the name LCOPY
C and return file name in lltmp.
        lltmp=' '
        call edisp(iuout,'  ')  ! echo blank line
        write(LCOPY,'(2a)') dbspth(1:lnblnk(dbspth)),fs
        CALL EASKXORGTKF(LCOPY,'Materials db?',' ',
     &    DFCON,lltmp,'materials db',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 60  ! redisplay menu.
        endif

        if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then

C Ask user whether file is in ../dbs and if so use fdroot if GTK.
          if(iglib.eq.2)then
            dok=.true.
            CALL ASKOK('Is this file in the model ../dbs folder?',
     &        ' ',OK,dok,3)
            if(OK)then
              call fdroot(lltmp,lpath,fname)
              write(LFMAT,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &          fname(1:lnblnk(fname))
              ipathmat=1  ! signal that it is a local database.
              moddb=.true.
              goto 44
            endif
          endif

C Check what the user might have edited to be sure of setting the
C correct location value via call to findwhichdbpath.
          call findwhichdbpath('mat',lltmp,ier)
          moddb=.true.
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.8)then

C Browse distribution database folder. Setup LCOPY to point to
C the database folder (but no particular file). Use easkf to
C return a file (which might be from somewhere else if the user
C decides to do some browsing).
        if(iglib.eq.2)then
          write(LCOPY,'(4a)') dinstpath(1:lnblnk(dinstpath)),
     &      fs,'databases',fs
          CALL EASKF(LCOPY,' ','Materials db?',
     &      144,DFCON,'materials database',IER,nbhelp)
          if(ier.eq.-3)then
            LFMAT=lprev
            goto 60  ! cancel detected, restore name and redisplay menu.
          endif

C This is yet another place where we need to check what the user might
C have edited to be sure of setting the correct location value. The
C common block lpcdb will have been updated by call to findwhichdbpath.
          call findwhichdbpath('mat',lcopy,ier)
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

C Debug.
C      write(outs248,'(2a)') 'Accessing materials db: ',
C     &  LFMAT(1:lnblnk(LFMAT))
C      call edisp248(iuout,outs248,100)
      ier=0

C Scan the binary file data into materials commons and if this was
C sucessful and matver was set to 1.1 in matformbin then we can
C carry on using the materials common blocks for subsequent access.
C If original was binary origmatwasbin will be .true.
      call scananymat(ier)
      if(ier.ne.0)then
        call usrmsg('Readable materials database was not found',
     &              'or file was corrupt','W')
        MATDBOK=.FALSE.
      else
        call eclose(matver,1.1,0.001,closemat1)
        call eclose(matver,1.2,0.001,closemat2)
        MATDBOK=.TRUE.
      endif

      moddb=.true.
      modcon=.true.
      chgdb=.false.

      call tstamp('>','PRJ: enter materials db')
      call eclose(matver,1.1,0.001,closemat1)
      call eclose(matver,1.2,0.001,closemat2)
      if(closemat1.or.closemat2)then

C Offer display and editing of material common blocks. If chgdb is
C returned as true then write out the ASCII file.  If the original
C was binary then go ahead and set chgdb .true. to force save.
        CALL ELISTMAT(iw,chgdb,'M',imatarryindex,IER)
        if(origmatwasbin)then
          chgdb=.true.
        endif
      else

C If the materials arrays were not filled warn the user.
        call usrmsg('Materials database arrays were not filled',
     &              'properly or db file was corrupt','W')
        goto 60
      endif

C Having returned from editing/browsing the materials, offer to
C make a backup copy if the initial file was binary.
      if(origmatwasbin.and.chgdb)then

C If the binary materials db was edited save an ASCII version.
        DFILE='./newmat.db1.a'
        clkok=.false.

C Create LLASCI based on value of ipathmat.
        LLASCI=' '
        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          write(LLASCI,'(2a)') LFMAT(1:lnblnk(LFMAT)),'.a'
        elseif(ipathmat.eq.2)then
          lndbp=lnblnk(standarddbpath)
          write(LLASCI,'(3a)') standarddbpath(1:lndbp),fs,
     &      LFMAT(1:lnblnk(LFMAT))
        endif

C Work with a shorter string until we can switch to EASKXORGTKF
        lsn2=MIN0(lnblnk(LLASCI),92)
        LTMP=' '
        write(LTMP,'(a)') LLASCI(1:lsn2)
        CALL EASKSCNCL(LTMP,
     &    'Materials database (ASCII backup)?',
     &    ' ','cancel',clkok,92,DFILE,'materials db (ascii)',
     &    IER,nbhelp)
        if(clkok)then
          continue
        else

C Write out the ASCII version and make it the new LFMAT.
C << shorter string until replacement of EASKXORGTKF >>
          write(LLASCI,'(a)') LTMP(1:lnblnk(LTMP))
          IAF=IFIL+1
          CALL MKAPRC(LLASCI,IAF,IER)
          write(LFMAT,'(a)') LLASCI(1:lnblnk(LLASCI))
          origmatwasbin=.false.   ! reset so only done once
        endif
      endif
      modcon=chgdb

      return

C Error messages.
   13 CALL USRMSG(' Write error to ascii Materials db ',LLTMPFL,'W')
      IER=1
      return
      end


C ********** EDDBPCDB
C EDDBPCDB Changes the plant component database current cfg file references.
C moddb is from version manager
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.

      SUBROUTINE EDDBPCDB(moddb,APP)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "plantdb.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      common/C21/IFCFG,cfgroot,LCFGF
      common/appw/iappw,iappx,iappy
      common/rpath/path
      common/deflt4/dinstpath

      CHARACTER LTMP*72,lltmp*144,lprev*144
      character longtfile*144,lguess*144,message*48
      CHARACTER DOIT*300,fs*1,LCOPY*144
      CHARACTER OUTS*124,outs248*248,APP*4,ext*4
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
      integer ier
      logical unixok  ! to check for database path file separators
      integer iglib  ! for detecting GTK or X11
#ifdef OSI
      integer ICPMOD     ! passed from tchild
#else
      integer*8 ICPMOD     ! passed from tchild
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDDBPCDB'  ! set for subroutine

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
C      write(outs248,*) 'Current plant ',lprev(1:lnblnk(lprev))
C      call edisp248(iuout,outs248,100)

  60  continue
      if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
        lltmp=LPCDB  ! use as is
      elseif(ipathpcdb.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    lpcdb(1:lnblnk(lpcdb))  ! prepend db folder path
      endif

      helptopic='plant_template_manage'
      call gethelptext(helpinsub,helptopic,nbhelp)

C If GTK library then there are additional options so include in help.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.

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
      if(iglib.eq.1.or.iglib.eq.3)then
        opte='e edit file name'
        optf='   '
        optg='   '
      elseif(iglib.eq.2)then
        opte='e browse model ../dbs'
        optf='f browse esp-r distribution'
        optg='g make copy of another file'
      endif

      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Plant Components db',
     &    ' ','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.7)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If corportate database then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Plant Components db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If corportate database then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Plant components db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default database or the current database.
        idno=1
        isw=0
        call MENUATOL('  ','Plant Components db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy the standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
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
          call edisp(iuout,
     &    'No file to browse/edit. Please use another option.')
          LPCDB=lprev
          goto 60
        endif

      elseif(isw.eq.2)then

C There is no select by list for plant template database.
        call edisp(iuout,
     &    'No list select for plant template databases.')
        goto 60

      elseif(isw.eq.3)then

C No plant component db found so set-up minimal common block, create
C a new db, populate it and present editing facility. Use the
C shorter string ltmp for this (easkf cancel option needs to
C be explored).
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.plantdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.plantdb'
        endif
 62     clkok=.false.
        CALL EASKSCNCL(ltmp,' ','Plant Components db?',
     &    'cancel',clkok,72,DPCDB,'plant components db',
     &    IER,nbhelp)
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
        ier=0
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
        CALL ASKOK(outs,' Browse or edit new database?',
     &    OK,dok,nbhelp)
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
          lltmp='  '
          lguess='  '
          if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
            lguess=LPCDB
          elseif(ipathpcdb.eq.2)then
            write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &        lpcdb(1:lnblnk(lpcdb))
          endif
          call edisp(iuout,'  ')  ! echo blank line
          CALL EASKXORGTKF(lguess,
     &      'Plant template file name (source)?',' ',
     &      DPCDB,lltmp,'plant templates database',IER,nbhelp)

          sourcefile = lltmp   ! remember this

C If user request jump back and re-display the menu.
          if(ier.eq.-3)then
            goto 60  ! cancel detected, restore name and redisplay menu.
          endif
          ipathpcdb=1  ! signal that it will be a local database.
        endif

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
     &    ' ',72,'plant.db','copied plant database',IER,nbhelp)
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

C Now copy sourcefile to lcopy.
          doit = ' '
          if(unixok)then
            write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &        sourcefile(1:lnblnk(sourcefile)),' ',
     &        LCOPY(1:lnblnk(LCOPY))
          else

C Standard dos copy logic.
            message='getting plant db file from '
            call dblongdoscopy(sourcefile,lcopy,message,doit,ier)
          endif
          call usrmsg('copying default file via',doit,'-')
          call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
          call FINDFIL(LCOPY,XST)
          if(XST)then
            call fdroot(lcopy,lpath,fname)
            write(LPCDB,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
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
          moddb=.true.
          ipathpcdb=1  ! signal that it is a local database.
          lltmp=LPCDB  ! reset lltmp for use at 44
          goto 44
        else
          goto 66
        endif

      elseif(isw.eq.5)then

C Make up browse to the model ../dbs folder suggest the name LCOPY
C and return file name in lltmp.
        lltmp=' '
        call edisp(iuout,'  ')  ! echo blank line
        write(LCOPY,'(2a)') dbspth(1:lnblnk(dbspth)),fs

        CALL EASKXORGTKF(LCOPY,'Plant templates db?',' ',
     &    DPCDB,lltmp,'plant templates database',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 60  ! redisplay menu.
        endif

        if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then

C Ask user whether file is in ../dbs and if so use fdroot if GTK.
          if(iglib.eq.2)then
            dok=.true.
            CALL ASKOK('Is this file in the model ../dbs folder?',
     &        ' ',OK,dok,nbhelp)
            if(OK)then
              call fdroot(lltmp,lpath,fname)
              write(LPCDB,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &          fname(1:lnblnk(fname))
              ipathpcdb=1  ! signal that it is a local database.
              lltmp=LPCDB  ! reset lltmp for use at 44
              moddb=.true.
              goto 44
            endif
          endif

C Check what the user might have edited to be sure of setting the
C correct location value via call to findwhichdbpath.
          call findwhichdbpath('pdb',lltmp,ier)
          moddb=.true.
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.6)then

C Browse distribution database folder. Setup LCOPY to point to
C the database folder (but no particular file). Use easkf to
C return a file (which might be from somewhere else if the user
C decides to do some browsing). Test this suggested file via
C call to eroptdb and if this is not successful reset LOPTDB.
        if(iglib.eq.2)then
          write(LCOPY,'(4a)') dinstpath(1:lnblnk(dinstpath)),
     &      fs,'databases',fs
          CALL EASKF(LCOPY,' ','Plant templates db?',
     &      144,DPCDB,'plant templates database',IER,nbhelp)
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

      ier=0
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
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "profile.h"

C << introduce concept of profiles with multiple day types as well
C << as multiple casual gain types

C << also consider that periods can be defined as reals and for
C << only the start time to be held in the file

C << introduce concept of types of profiles
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path
      common/deflt4/dinstpath

      CHARACTER LTMP*72,lltmp*144,lprev*144
      character LLASCI*144,lbin*144,llbin*144,lguess*144
      character longtfile*144,LCOPY*144,message*48
      CHARACTER DOIT*300,fs*1,APP*4,ext*4
      CHARACTER OUTS*124,OUTS248*248
      LOGICAL CLKOK,OK,COPYDEF,MODDB,XST,DOK,concat
      character cfgroot*24,LCFGF*72
      character dinstpath*48,path*72,dirpath*48

C Local strings for user selections dependent on graphic library.
      character opte*28,optf*28,optg*28,opth*28,opti*28
      character lpath*72,fname*72 ! for use with fdroot
      character sourcefile*144  ! the file to copy
      logical unixok  ! to check for database path file separators
      integer iglib  ! for detecting GTK or X11

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDDBPROFILE'  ! set for subroutine

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
      lndbp=lnblnk(standarddbpath)
      if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
        lprev=LPRFDB
      elseif(ipathprodb.eq.2)then
        write(lprev,'(3a)') standarddbpath(1:lndbp),fs,
     &    LPRFDB(1:lnblnk(LPRFDB))
      endif

C Debug.
C      write(outs248,*) 'Current profiles ',lprev(1:lnblnk(lprev))
C      call edisp248(iuout,outs248,100)

  60  continue

C Generate file name lltmp taking into account ipathprodb for use later.
      if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
        lltmp=LPRFDB  ! use as is
      elseif(ipathprodb.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    LPRFDB(1:lnblnk(LPRFDB))  ! prepend db folder path
      endif
      llt=lnblnk(lltmp)

      helptopic='events_db_manage'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL ERPFREE(IPRODB,ISTAT)

C If local or absolute path call addpath otherwise if a standard
C database then set longtfile equal to LLTMP.
      if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
        call addpath(LLTMP,longtfile,concat)
      elseif(ipathprodb.eq.2)then
        longtfile=LLTMP
      endif
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
        optg='g browse model ../dbs'
        opth='h browse esp-r distribution'
        opti='i make copy of another file'
      endif

      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices:
        idno=2
        isw=0
        call MENUATOL('  ','Event Profiles db',
     &    ' ','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,opth,opti,' ',' ',' ',isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.9)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If corportate database then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Event Profiles db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,opth,opti,' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If corportate database then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Event Profiles db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,opth,opti,' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default database or the current database.
        idno=1
        isw=0
        call MENUATOL('  ','Event Profiles db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,opth,opti,' ',' ',' ',isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.9)copydef=.false.
      endif

C Act on the users choice. If nothing selected return to main menu.
      if(isw.eq.0)then
        return
      elseif(isw.eq.1)then

C If file exists read it and enter editing facility after
C check to see if it is local or standard or absolute path.
        if(XST)then
          call findwhichdbpath('evn',lltmp,ier)
          goto 44
        else
          call edisp(iuout,
     &      'No file to browse/edit. Please use another option.')
          LPRFDB=lprev
          goto 60
        endif

      elseif(isw.eq.2)then

C There is no select by list for events database.
        call edisp(iuout,
     &    'No list select for events databases.')
        goto 60

      elseif(isw.eq.3)then

C No event profile db found so set-up minimal common block, create
C a new ascii db, populate it and present editing facility.
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.eventdb.a'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.eventdb.a'
        endif
 62     clkok=.false.

C DPRFDB might point to binary or ascii.
        CALL EASKSCNCL(ltmp,' ','Event Profiles db? (ASCII)',
     &    'cancel',clkok,72,DPRFDB,'event profiles db',
     &    IER,nbhelp)
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
        ipathprodb=1  
        moddb=.true.
        NPDBITEMS = 1
        NCG(1)= 1
        PDBDESC(1)='Always ON 100%'
        ICGS1(1,1)=0
        ICGF1(1,1)=24
        CGS1(1,1)=100.0
        CGL1(1,1)=100.0

C The model database is now an ASCII version.
        if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
          LLASCI=LPRFDB
        elseif(ipathprodb.eq.2)then
          write(LLASCI,'(3a)') standarddbpath(1:lndbp),fs,
     &      LPRFDB(1:lnblnk(LPRFDB))
        endif
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
        CALL ASKOK(outs,' Browse or edit database?',OK,dok,nbhelp)
        if(OK)then
          moddb=.true.
          goto 44
        endif

      elseif(isw.eq.4.or.isw.eq.9)then

C Copy a database to project dbs folder. If iws is 4 then the
C user has asked for the default database. If isw is 9 then the
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

C Provide a guess based on the current LPRFDB.
          lltmp='  '
          lguess='  '
          if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
            lguess=LPRFDB  ! use as is
          elseif(ipathprodb.eq.2)then
            write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &        LPRFDB(1:lnblnk(LPRFDB))  ! prepend db folder path
          endif
          call edisp(iuout,'  ')  ! echo blank line
          CALL EASKXORGTKF(lguess,'Event Profiles db (source)?',' ',
     &      DPRFDB,lltmp,'events db',IER,nbhelp)
          sourcefile = lltmp

C If user request jump back and re-display the menu.
          if(ier.eq.-3)then
            goto 60  ! restore name and redisplay menu.
          endif
          ipathprodb=1  ! signal that it is a local database.
        endif

C Ask the user what to call the local file (suggest name).
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.eventdb.a'
        elseif(dbspth(1:3).eq.'../')then
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.eventdb.a'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.eventdb.a'
        endif
  66    CALL EASKS(LTMP,
     &    'Database name after it is copied into the project?',
     &    ' ',72,'event.db','copied events database',IER,nbhelp)
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

C Now copy sourcefile to lcopy.
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
          call usrmsg('copying default file via',doit,'-')
          call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
          call FINDFIL(LCOPY,XST)
          if(XST)then
            call fdroot(lcopy,lpath,fname)
            write(LPRFDB,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          else
            call FINDFIL(LTMP,XST)
            if(XST)then
              write(LPRFDB,'(a)') LTMP(1:lnblnk(LTMP))
            else
              write(LPRFDB,'(a)') LTMP(1:lnblnk(LTMP))
              call edisp(iuout,'Problem locating the new database.')
              call edisp(iuout,'Try browsing ../dbs to find it.')
              call edisp(iuout,' ')
            endif
          endif
          moddb=.true.
          ipathprodb=1  ! signal that it is a local database.
          goto 44
        else
          goto 66
        endif

      elseif(isw.eq.5)then

C Event profile db: BINARY > ASCII. 

C Users selects a binary file (initial guess is LPRFDB and
C the file name is returned in lltmp.
        IAF=IFIL+1
        lltmp='  '

C << lguess logic to be added later >>
        call edisp(iuout,'  ')  ! echo blank line
        CALL EASKXORGTKF(LPRFDB,'Event Profiles db (source)?',' ',
     &    DPRFDB,lltmp,'source events db',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 60  ! restore name and redisplay menu.
        endif

        if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then
          continue
        else
          goto 60
        endif

C Create the ASCII version as variant of lltmp. If it does not 
C end in .a then update LLASACI
        llt=lnblnk(lltmp)
        if(lltmp(llt-1:llt).eq.'.a')then
          LLASCI=lltmp
        else
          if(llt.le.141)then 
            write(LLASCI,'(2a)') lltmp(1:llt),'.a'
          else
            write(LLASCI,'(2a)') lltmp(1:141),'.a'
          endif
        endif

C Read binary file lltmp into common blocks.
        call TRIPROBIN(IPRODB,lltmp,IER)

C Write commons to ASCII and then redisplay the menu.
        if(ier.eq.0)then
          ier=0  ! reset prior to call to troprocom
          call TROPROCOM(IAF,LLASCI,IER)
          if(ier.eq.0)then
            call edisp(iuout,'Binary -> ASCII conversion done.')
          else
            call edisp(iuout,'Binary -> ASCII conversion problems.')
          endif
          call tstamp('>','PRO: convert binary to ascii')
          goto 60
        else
          call edisp(iuout,
     &      'Source file was probably not a binary events file.')
          goto 60
        endif

      elseif(isw.eq.6)then

C Profile db: ASCII > BINARY.
C Ask for the ASCII file name and then confirm the binary db name.
C Guess ASCII name based on current LPRFDB.
        IAF=IFIL+1

C Check the last two characters of the file name. Append a '.a'
C to the current LPRFDB as an initial guess for the name.
        llt=lnblnk(LPRFDB)
        if(LPRFDB(llt-1:llt).eq.'.a')then
          if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
            write(LLASCI,'(a)') LPRFDB(1:llt)
          elseif(ipathprodb.eq.2)then
            lndbp=lnblnk(standarddbpath)
            write(LLASCI,'(3a)') standarddbpath(1:lndbp),fs,
     &        LPRFDB(1:llt)
          endif
        else
          if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
            write(LLASCI,'(2a)') LPRFDB(1:llt),'.a'
          elseif(ipathprodb.eq.2)then
            lndbp=lnblnk(standarddbpath)
            write(LLASCI,'(4a)') standarddbpath(1:lndbp),fs,
     &        LPRFDB(1:llt),'.a'
          endif
        endif

C Return the selected file in lltmp based on initial guess LLASCI.
        call edisp(iuout,'  ')  ! echo blank line
        CALL EASKXORGTKF(LLASCI,
     &    'Event Profiles db (ASCII source)?',' ',
     &    DPRFDB,lltmp,'ascii events db',IER,nbhelp)


C If lltmp ends with .a then make up a temporary file name
C lbin for the binary that has the .a stripped off. Return
C llbin for use in tripro.
        llt=lnblnk(lltmp)
        llbin=' '
        lbin=' '
        if(lltmp(llt-1:llt).eq.'.a')then
          write(lbin,'(a)') lltmp(1:llt-2)
        else
          write(lbin,'(a)') lltmp(1:llt)
        endif
        CALL EASKXORGTKF(lbin,'Event Profiles db (binary)?',' ',
     &    DPRFDB,llbin,'binary events db',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 60  ! redisplay menu.
        endif

        if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then

C Convert data in lltmp to binary form in llbin (this does not alter
c the current profiles database.
          CALL TRIPRO(IAF,IPRODB,lltmp,llbin,IER)
          if(ier.eq.0)then
            call edisp(iuout,'ASCII -> Binary conversion done.')
          else
            call edisp(iuout,'ASCII -> Binary conversion problems.')
          endif
          goto 60  ! re-display the menu
        else
          goto 60  ! re-display the menu
        endif

      elseif(isw.eq.7)then

C Make up browse to the model ../dbs folder suggest the name LCOPY
C and return file name in lltmp.
        lltmp=' '
        call edisp(iuout,'  ')  ! echo blank line
        write(LCOPY,'(2a)') dbspth(1:lnblnk(dbspth)),fs
        CALL EASKXORGTKF(LCOPY,'Event Profiles db?',' ',
     &    DPRFDB,lltmp,'ascii events db',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 60  ! redisplay menu.
        endif

        if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then

C Ask user whether file is in ../dbs and if so use fdroot if GTK.
          if(iglib.eq.2)then
            dok=.true.
            CALL ASKOK('Is this file in the model ../dbs folder?',
     &        ' ',OK,dok,nbhelp)
            if(OK)then
              call fdroot(lltmp,lpath,fname)
              write(LPRFDB,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &          fname(1:lnblnk(fname))
              ipathprodb=1  ! signal that it is a local database.
              moddb=.true.
              goto 44
            endif
          endif

C Check what the user might have edited to be sure of setting the
C correct location value via call to findwhichdbpath.
          call findwhichdbpath('evn',lltmp,ier)
          moddb=.true.
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.8)then

C Browse distribution database folder. Setup LCOPY to point to
C the database folder (but no particular file). Use easkf to
C return a file (which might be from somewhere else if the user
C decides to do some browsing). Test this suggested file via
C call to eroptdb and if this is not successful reset LPRFDB.
        if(iglib.eq.2)then
          write(LCOPY,'(4a)') dinstpath(1:lnblnk(dinstpath)),
     &      fs,'databases',fs
          CALL EASKF(LCOPY,' ','Event Profiles db?',
     &      144,DPRFDB,'Event profiles database',IER,nbhelp)
          if(ier.eq.-3)then
            LPRFDB=lprev
            goto 60  ! cancel was detected, restore name and redisplay menu.
          endif

C This is yet another place where we need to check what the user might
C have edited to be sure of setting the correct location value. The
C common block will be updated with call to findwhichdbpath.
          call findwhichdbpath('evn',LCOPY,ier)
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

C Debug.
C      write(outs248,'(2a)') 'Accessing event profile db: ',
C     &  LPRFDB(1:lnblnk(LPRFDB))
C      call edisp248(iuout,outs248,100)
      IER=0

C Scan file (with full path) as a binary event profiles database. 
C If an error state is returned as -2 then it might be an ASCII
C file. Either way commons are filled.
      lndbp=lnblnk(standarddbpath)
      if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
        lltmp=LPRFDB  ! use as is
      elseif(ipathprodb.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    LPRFDB(1:lnblnk(LPRFDB))  ! prepend db folder path
      endif
      call TRIPROBIN(IPRODB,lltmp,IER)
      if(ier.eq.-2)then
        CALL ERPFREE(IPRODB,ISTAT)
        CALL USRMSG('opened profiles db...',' ','-')
        ier=0   ! reset ier after triprobin call
        call TRIPROCOM(IPRODB,lltmp,IER)
        if(ier.ne.0)then
          call usrmsg('Error opening the profiles db. Please use one',
     &                'of the other options.','W')
          LPRFDB=lprev
          goto 60
        else
          CALL ERPFREE(IPRODB,ISTAT)
          CALL USRMSG('opened profiles db...ok',' ','-')

C Call casfmk for the editing/browsing interface.
          call casfmk(ier)
          return
        endif
      elseif(ier.eq.0)then
        CALL USRMSG('opened profiles db...ok',' ','-')

C Call casfmk for the editing/browsing interface.
        call casfmk(ier)
        return
      else
        call usrmsg('Error opening the profiles db. Please use one',
     &              'of the other options.','W')
        LPRFDB=lprev
        goto 60
      endif
      return

      end

C ********** EDDBMLCON
C EDDBMLCON Changes the MLC constructions database current cfg file references.
C moddb is from version manager
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.

      SUBROUTINE EDDBMLCON(moddb,modcon,APP)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path

C Default esp-r distribution path.
      common/deflt4/dinstpath

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical concat
      logical moddb,modcon,modmlc,copydef,clkok

      CHARACTER LTMP*72,lltmp*144,lprev*144
      character longtfile*144,LCOPY*144,lguess*144,message*48
      CHARACTER DOIT*300,fs*1,APP*4,ext*4
      CHARACTER outs248*248
      LOGICAL OK,XST,DOK
      character cfgroot*24,LCFGF*72
      character dinstpath*48,path*72,dirpath*48
      character sourcefile*144  ! the file to copy ??

C Local strings for user selections dependent on graphic library.
      character opte*28,optf*28,optg*28
      character lpath*72,fname*72 ! for use with fdroot
      integer lndbp   ! for length of standard database path
      logical unixok  ! to check for database path file separators
      integer iglib  ! for detecting GTK or X11

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDDBMLCON'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      moddb=.false.
      modcon=.false.

C Setup string buffer with distribution database folder name.
      write(dirpath,'(4a)') dinstpath(1:lnblnk(dinstpath)),
     &  fs,'databases',fs
      ldirpath=lnblnk(dirpath)

C Remember the initial file name in case user choice fails.
C Take into account the current value of whichdbpath.
      lndbp=lnblnk(standarddbpath)
      if(ipathmul.eq.0.or.ipathmul.eq.1)then
        lprev=lfmul
      elseif(ipathmul.eq.2)then
        write(lprev,'(3a)') standarddbpath(1:lndbp),fs,
     &    lfmul(1:lnblnk(lfmul))
      endif

C Debug.
C      write(outs248,*) 'Current MLC ',lprev(1:lnblnk(lprev))
C      call edisp248(iuout,outs248,100)

  60  continue
      if(ipathmul.eq.0.or.ipathmul.eq.1)then
        lltmp=lfmul  ! use as is
      elseif(ipathmul.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    lfmul(1:lnblnk(lfmul))  ! prepend db folder path
      endif

      helptopic='mlc_db_manage'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL ERPFREE(IFMUL,ISTAT)

C If local or absolute path call addpath otherwise if a standard
C database then set longtfile equal to LLTMP.
      if(ipathmul.eq.0.or.ipathmul.eq.1)then
        call addpath(LLTMP,longtfile,concat)
      else
        longtfile=LLTMP
      endif
      ltf=max(1,LNBLNK(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up additional options depending on graphics lib. The second
C option (list select) is not available for mLC database. If using
C text or X11 option e edits the string while for GTK there are two
C options to browse for files in ../dbs or the esp-r distribution.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        opte='e edit file name'
        optf='   '
        optg='   '
      elseif(iglib.eq.2)then
        opte='e browse model ../dbs'
        optf='f browse esp-r distribution'
        optg='g make copy of another file'
      endif

      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Constructions db',
     &    ' ','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.7)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If corportate database then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Constructions db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If corportate database then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Constructions db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default database or the current database.
        idno=1
        isw=0
        call MENUATOL('  ','Constructions db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.7)copydef=.false.
      endif

C Act on the users choice. If no choice, return to calling menu.
      if(isw.eq.0)then
        return
      elseif(isw.eq.1)then

C << to here >>
C If file exists read it and enter editing facility after
C check to see if it is local or standard or absolute path.
        if(XST)then
          call findwhichdbpath('mul',lltmp,ier)
          goto 44
        else
          call edisp(iuout,
     &    'No file to browse/edit. Please use another option.')
          LFMUL=lprev
          goto 60
        endif
      elseif(isw.eq.2)then

C There is no select by list for MLC database.
        call edisp(iuout,
     &    'No list select for MLC databases.')
        goto 60
      elseif(isw.eq.3)then

C No MLC db found so set-up minimal common block, create
C a new db, populate it and present editing facility. Use the
C shorter string ltmp for this (easkf cancel option needs to
C be explored).
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.constrdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.constrdb'
        endif
  62    clkok=.false.
        CALL EASKSCNCL(ltmp,'Constructions database?',
     &    ' ','cancel',clkok,72,'construction.db','MLC database',
     &    IER,nbhelp)
        if(clkok)then
          LFMUL=lprev
          return
        endif
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          write(LFMUL,'(a)') ltmp(1:lnblnk(ltmp)) ! local so direct asignment
        else
          goto 62
        endif

C Create a minimal MLC databases.
C Set the value of ipathmul to signal local database.
        ipathmul=1  

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
        IF(IER.NE.0)goto 60
        MLDBOK=.TRUE.

C Scan the new file and use the editing facility.
        dok=.true.
        CALL ASKOK('  ','Browse or edit new database?',OK,dok,nbhelp)
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
     &      DFMUL(1:lnblnk(DFMUL))
          call edisp248(iuout,outs248,100)
          sourcefile = DFMUL
        else

C Provide a guess based on the current LFMUL.
          lltmp='  '
          lguess='  '
          if(ipathmul.eq.0.or.ipathmul.eq.1)then
            lguess=LFMUL  ! use as is
          elseif(ipathmul.eq.2)then
            write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &        lfmul(1:lnblnk(lfmul))  ! prepend db folder path
          endif
          call edisp(iuout,'  ')  ! echo blank line
          CALL EASKXORGTKF(lguess,'Constructions db (source)?',
     &      ' ',DFMUL,lltmp,'MLC source database',IER,nbhelp)
          sourcefile = lltmp

C If user request jump back and re-display the menu.
          if(ier.eq.-3)then
            goto 60  ! cancel detected, redisplay menu.
          endif
          ipathmul=1  ! signal that it will be a local database.
        endif

        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.constrdb'
        elseif(dbspth(1:3).eq.'../')then
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.constrdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.constrdb'
        endif
  66    CALL EASKS(LTMP,
     &    'Database name after it is copied into the project?',
     &    ' ',72,'construction.db','copied MLC database',IER,nbhelp)
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

C Now copy sourcefile to lcopy.
          doit = ' '
          if(unixok)then
            write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &        sourcefile(1:lnblnk(sourcefile)),' ',
     &        LCOPY(1:lnblnk(LCOPY))
          else

C Standard dos copy logic.
            message='getting MLC db file from '
            call dblongdoscopy(sourcefile,lcopy,message,doit,ier)
          endif
          call usrmsg('copying default file via',doit,'-')
          call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
          call FINDFIL(LCOPY,XST)
          if(XST)then
            call fdroot(lcopy,lpath,fname)
            write(LFMUL,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          else
            call FINDFIL(LTMP,XST)
            if(XST)then
              write(LFMUL,'(a)') LTMP(1:lnblnk(LTMP))
            else
              write(LFMUL,'(a)') LTMP(1:lnblnk(LTMP))
              call edisp(iuout,'Problem locating the new database.')
              call edisp(iuout,'Try browsing ../dbs to find it.')
              call edisp(iuout,' ')
            endif
          endif
          moddb=.true.
          mldbok=.true.
          ipathmul=1  ! signal that it is a local database.
          goto 44
        else
          goto 66
        endif

      elseif(isw.eq.5)then

C Make up browse to the model ../dbs folder suggest the name LCOPY
C and return file name in lltmp.
        lltmp=' '
        call edisp(iuout,'  ')  ! echo blank line
        write(LCOPY,'(2a)') dbspth(1:lnblnk(dbspth)),fs
        CALL EASKXORGTKF(LCOPY,'Constructions db?',' ',
     &    DFMUL,lltmp,'MLC db in ../dbs',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 60  ! redisplay menu.
        endif

        if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then

C Ask user whether file is in ../dbs and if so use fdroot if GTK.
          if(iglib.eq.2)then
            dok=.true.
            CALL ASKOK('Is this file in the model ../dbs folder?',
     &        ' ',OK,dok,nbhelp)
            if(OK)then
              call fdroot(lltmp,lpath,fname)
              write(LFMUL,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &          fname(1:lnblnk(fname))
              ipathmul=1  ! signal that it is a local database.
              moddb=.true.
              goto 44
            endif
          endif

C Check what the user might have edited to be sure of setting the
C correct location value via call to findwhichdbpath.
          call findwhichdbpath('mul',lltmp,ier)
          moddb=.true.
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.6)then

C Browse distribution database folder. Setup LCOPY to point to
C the database folder (but no particular file). Use easkf to
C return a file (which might be from somewhere else if the user
C decides to do some browsing).
        if(iglib.eq.2)then
          write(LCOPY,'(4a)') dinstpath(1:lnblnk(dinstpath)),
     &      fs,'databases',fs
          CALL EASKF(LCOPY,' ','Constructions db?',
     &      144,DFMUL,'MLC database',IER,nbhelp)
          if(ier.eq.-3)then
            LFMUL=lprev
            goto 60  ! cancel detected, restore name and redisplay menu.
          endif

C This is yet another place where we need to check what the user might
C have edited to be sure of setting the correct location value. The
C common block will be updated with call to findwhichdbpath.
          call findwhichdbpath('mul',LCOPY,ier)
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

C Debug.
C      write(outs248,'(2a)') 'Accessing MLC db: ',
C     &  LFMUL(1:lnblnk(LFMUL))
C      call edisp248(iuout,outs248,100)

      ier=0
      call tstamp('>','PRJ: enter MLC db')
      CALL ERMLDB(0,IUOUT,IER)
      IF(IER.eq.1)then

C Problem scanning the constructions db.
        call usrmsg(
     &    'There were problems scanning the Constructions db',
     &    '(and perhaps the Materials db). Please check.','W')
        goto 60
      ELSEIF(IER.eq.2)then

C There were lots of undefined materials so likely the materials
C database is for the wrong machine type. Guess the name of
C the ascii version and if it exists, offer to convert it.
        call usrmsg(
     &    'The Materials db probably needs to be',
     &    'restored from an archived ASCII version.','W')
        return
      endif
      MLDBOK=.TRUE.
      call tstamp('>','PRJ: enter construction db')
      CALL EDMLDB(ITRC,modmlc,IER,ISEL)
      if(IER.EQ.1)then
        CALL USRMSG(' ','Problem accessing constructions db','W')
        LFMUL=lprev
        MLDBOK=.false.
        goto 60
      endif
      moddb=modmlc
      modcon=modmlc

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
#include "model.h"
#include "esprdbfile.h"
      common/OUTIN/IUOUT,IUIN
      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path
      common/deflt4/dinstpath
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      CHARACTER LTMP*72,lltmp*144,lprev*144
      character longtfile*144,LCOPY*144,lguess*144,message*48
      CHARACTER DOIT*300,fs*1,APP*4,ext*4
      CHARACTER outs248*248
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

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDDBOPT'  ! set for subroutine

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
C      write(outs248,*) 'Current optics ',lprev(1:lnblnk(lprev))
C      call edisp248(iuout,outs248,100)

  60  continue
      if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
        lltmp=LOPTDB  ! use as is
      elseif(ipathoptdb.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    loptdb(1:lnblnk(loptdb))  ! prepend db folder path
      endif

      helptopic='optic_db_manage'
      call gethelptext(helpinsub,helptopic,nbhelp)

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
        opte='e browse model ../dbs'
        optf='f browse esp-r distribution'
        optg='g make copy of another file'
      endif

      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Optical Properties db',
     &    ' ','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.7)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If corportate database then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Optical Properties db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If corportate database then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Optical Properties db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default database or the current database.
        idno=1
        isw=0
        call MENUATOL('  ','Optical Properties db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
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
          call edisp(iuout,
     &    'No file to browse/edit. Please use another option.')
          LOPTDB=lprev
          goto 60
        endif
      elseif(isw.eq.2)then

C There is no select by list for optics database.
        call edisp(iuout,
     &    'No list select for optical databases.')
        goto 60
      elseif(isw.eq.3)then

C No optics db found so set-up minimal common block, create
C a new db, populate it and present editing facility. Use the
C shorter string ltmp for this (easkf cancel option needs to
C be explored).
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.opticsdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.opticsdb'
        endif
  62    clkok=.false.
        CALL EASKSCNCL(ltmp,' Optics database?',
     &    ' ','cancel',clkok,72,'optics.db','optics database',
     &    IER,nbhelp)
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
        CALL ASKOK('  ',' Browse or edit new database?',
     &    OK,dok,nbhelp)
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

C Provide a guess based on the current LOPTDB.
          lltmp='  '
          lguess='  '
          if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
            lguess=LOPTDB  ! use as is
          elseif(ipathoptdb.eq.2)then
            write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &        loptdb(1:lnblnk(loptdb))  ! prepend db folder path
          endif
          call edisp(iuout,'  ')  ! echo blank line
          CALL EASKXORGTKF(lguess,'Optical Properties db (source)?',
     &      ' ',DOPTDB,lltmp,'optical properties database',IER,nbhelp)
          sourcefile = lltmp

C If user request jump back and re-display the menu.
          if(ier.eq.-3)then
            goto 60  ! cancel detected, redisplay menu.
          endif
          ipathoptdb=1  ! signal that it will be a local database.
        endif

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
     &    ' ',72,'optics.db','copied optics database',IER,nbhelp)
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

C Now copy sourcefile to lcopy.
          doit = ' '
          if(unixok)then
            write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &        sourcefile(1:lnblnk(sourcefile)),' ',
     &        LCOPY(1:lnblnk(LCOPY))
          else

C Standard dos copy logic.
            message='getting optics db file from '
            call dblongdoscopy(sourcefile,lcopy,message,doit,ier)
          endif
          call usrmsg('copying default file via',doit,'-')
          call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
          call FINDFIL(LCOPY,XST)
          if(XST)then
            call fdroot(lcopy,lpath,fname)
            write(LOPTDB,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
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
          moddb=.true.
          ipathoptdb=1  ! signal that it is a local database.
          goto 44
        else
          goto 66
        endif

      elseif(isw.eq.5)then

C Make up browse to the model ../dbs folder suggest the name LCOPY
C and return file name in lltmp.
        lltmp=' '
        call edisp(iuout,'  ')  ! echo blank line
        write(LCOPY,'(2a)') dbspth(1:lnblnk(dbspth)),fs
        CALL EASKXORGTKF(LCOPY,'Optical Properties db?',' ',
     &    DOPTDB,lltmp,'optical properties db',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 60  ! redisplay menu.
        endif

        if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then

C Ask user whether file is in ../dbs and if so use fdroot if GTK.
          if(iglib.eq.2)then
            dok=.true.
            CALL ASKOK('Is this file in the model ../dbs folder?',
     &        ' ',OK,dok,nbhelp)
            if(OK)then
              call fdroot(lltmp,lpath,fname)
              write(LOPTDB,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &          fname(1:lnblnk(fname))
              ipathoptdb=1  ! signal that it is a local database.
              moddb=.true.
              goto 44
            endif
          endif

C Check what the user might have edited to be sure of setting the
C correct location value via call to findwhichdbpath.
          call findwhichdbpath('opt',lltmp,ier)
          moddb=.true.
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.6)then

C Browse distribution database folder. Setup LCOPY to point to
C the database folder (but no particular file). Use easkf to
C return a file (which might be from somewhere else if the user
C decides to do some browsing). Test this suggested file via
C call to eroptdb and if this is not successful reset LOPTDB.
        if(iglib.eq.2)then
          write(LCOPY,'(4a)') dinstpath(1:lnblnk(dinstpath)),
     &      fs,'databases',fs
          CALL EASKF(LCOPY,' ','Optical Properties db?',
     &      144,DOPTDB,'optical properties database',IER,nbhelp)
          if(ier.eq.-3)then
            LOPTDB=lprev
            goto 60  ! cancel detected, restore name and redisplay menu.
          endif

C This is yet another place where we need to check what the user might
C have edited to be sure of setting the correct location value. The
C common block will be updated with call to findwhichdbpath.
          call findwhichdbpath('opt',LCOPY,ier)
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

C Debug.
C      write(outs248,'(2a)') 'Accessing optics db: ',
C     &  LOPTDB(1:lnblnk(LOPTDB))
C      call edisp248(iuout,outs248,100)

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
#include "model.h"
#include "net_flow.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path
      common/deflt4/dinstpath

      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      character DEPRE*40
      common/MFLDOC/DEPRE(MPRD)

      CHARACTER LTMP*72,lltmp*144,lprev*144
      character longtfile*144,LCOPY*144,lguess*144,message*48
      CHARACTER DOIT*300,fs*1,APP*4
      CHARACTER outs248*248

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

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDDBPRES'  ! set for subroutine

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
C      write(outs248,*) 'Current presure coef ',lprev(1:lnblnk(lprev))
C      call edisp248(iuout,outs248,100)

  60  continue
      if(ipathapres.eq.0.or.ipathapres.eq.1)then
        lltmp=lapres  ! use as is
      elseif(ipathapres.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    lapres(1:lnblnk(lapres))  ! prepend db folder path
      endif
      IAPRES=IFIL+1

      helptopic='pressure_db_manage'
      call gethelptext(helpinsub,helptopic,nbhelp)

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
        opte='e browse model ../dbs'
        optf='f browse esp-r distribution'
        optg='g make copy of another file'
      endif

      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Pressure Distributions db',
     &    ' ','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.7)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If corportate database then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Pressure Distributions db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If corportate database then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Pressure Distributions db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default database or the current database.
        idno=1
        isw=0
        call MENUATOL('  ','Pressure Distributions db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
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
          call edisp(iuout,
     &    'No file to browse/edit. Please use another option.')
          lapres=lprev
          goto 60
        endif
      elseif(isw.eq.2)then

C There is no select by list for pressure database.
        call edisp(iuout,
     &    'No list select for pressure databases.')
        goto 60
      elseif(isw.eq.3)then

C No pressure db found so set-up minimal common block, create
C a new db, populate it and present editing facility. Use the
C shorter string ltmp for this (easkf cancel option needs to
C be explored).
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
     &      'pressure coefficients database',IER,nbhelp)
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
        CALL ASKOK('  ',' Browse or edit new database?',
     &    OK,dok,nbhelp)
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

C Provide a guess based on the current lapres.
          lltmp='  '
          lguess='  '
          if(ipathapres.eq.0.or.ipathapres.eq.1)then
            lguess=lapres
          elseif(ipathapres.eq.2)then
            write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &        lapres(1:lnblnk(lapres))  ! prepend db folder path
          endif
          call edisp(iuout,'  ')  ! echo blank line
          CALL EASKXORGTKF(lguess,
     &      'Pressure coefficients db (source)?',' ',
     &      dapres,lltmp,'pressure coef database',IER,nbhelp)
          sourcefile = lltmp

C If user request jump back and re-display the menu.
          if(ier.eq.-3)then
            goto 60  ! cancel detected, redisplay menu.
          endif
          ipathapres=1  ! signal that it is a local database.
        endif

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
     &    ' ',72,'pressure.db','copied pres coef database',
     &    IER,nbhelp)
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

C Now copy sourcefile to lcopy.
          doit = ' '
          if(unixok)then
            write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &        sourcefile(1:lnblnk(sourcefile)),' ',
     &        LCOPY(1:lnblnk(LCOPY))
          else

C Standard dos copy logic.
            message='getting pressure db file from '
            call dblongdoscopy(sourcefile,lcopy,message,doit,ier)
          endif
          call usrmsg('copying default file via',doit,'-')
          call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
          call FINDFIL(LCOPY,XST)
          if(XST)then
            call fdroot(lcopy,lpath,fname)
            write(lapres,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
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
          moddb=.true.
          ipathapres=1  ! signal that it is a local database.
          goto 44
        else
          goto 66
        endif

      elseif(isw.eq.5)then

C Make up browse to the model ../dbs folder suggest the name LCOPY
C and return file name in lltmp.
        lltmp=' '
        call edisp(iuout,'  ')  ! echo blank line
        write(LCOPY,'(2a)') dbspth(1:lnblnk(dbspth)),fs

        CALL EASKXORGTKF(LCOPY,'Pressure coefficients db?',' ',
     &    dapres,lltmp,'pressure coef database',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 60  ! redisplay menu.
        endif

        if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then

C Ask user whether file is in ../dbs and if so use fdroot if GTK.
          if(iglib.eq.2)then
            dok=.true.
            CALL ASKOK('Is this file in the model ../dbs folder?',
     &        ' ',OK,dok,nbhelp)
            if(OK)then
              call fdroot(lltmp,lpath,fname)
              write(lapres,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &          fname(1:lnblnk(fname))
              ipathapres=1  ! signal that it is a local database.
              moddb=.true.
              goto 44
            endif
          endif

C Check what the user might have edited to be sure of setting the
C correct location value via call to findwhichdbpath.
          call findwhichdbpath('prs',lltmp,ier)
          moddb=.true.
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.6)then

C Browse distribution database folder. Setup LCOPY to point to
C the database folder (but no particular file). Use easkf to
C return a file (which might be from somewhere else if the user
C decides to do some browsing). Test this suggested file and if
C this is not successful reset lapres.
        if(iglib.eq.2)then
          write(LCOPY,'(4a)') dinstpath(1:lnblnk(dinstpath)),
     &      fs,'databases',fs
          CALL EASKF(LCOPY,' ','Pressure coefficients db?',
     &      144,dapres,'pressure coef database',IER,nbhelp)
          if(ier.eq.-3)then
            lapres=lprev
            goto 60  ! cancel detected, restore name and redisplay menu.
          endif

C This is yet another place where we need to check what the user might
C have edited to be sure of setting the correct location value. The
C common block will be updated with call to findwhichdbpath.
          call findwhichdbpath('prs',LCOPY,ier)
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

C Debug.
C      write(outs248,'(2a)') 'Accessing presure coef db: ',
C     &  lapres(1:lnblnk(lapres))
C      call edisp248(iuout,outs248,100)

      moddb=.true.
      if(ipathapres.eq.0.or.ipathapres.eq.1)then
        CALL EFOPSEQ(IAPRES,lapres,1,IER)
      elseif(ipathapres.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    lapres(1:lnblnk(lapres))
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
#include "site.h"
#include "esprdbfile.h"
#include "espriou.h"
C espriou.h provides currentfile.
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN

      common/C21/IFCFG,cfgroot,LCFGF
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      common/appw/iappw,iappx,iappy
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      common/rpath/path
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/deflt4/dinstpath

C Calendar.
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)

      CHARACTER DFILE*72,LLASCI*144
      character longtfile*144,longtafile*144,message*48,longtmp*144
      CHARACTER DOIT*248,fs*1,TMODE*8,CLMLOC*30,LCOPY*72
      CHARACTER OUTS*124,APP*6,ext*4,llclmdb*144
      LOGICAL OK,concat,COPYDEF,MODDB,XST,DOK,UNIXOK
      character cfgroot*24,LCFGF*72,linstd*60
      character dinstpath*48,path*72
      CHARACTER*72 NNAME
      dimension ICLM(24,6)
      integer llt  ! length of string buffer
      integer ier
      integer icreport  ! to signal that calendar has been updated.
      integer IDOL,IDAYNUM,IMTHNUM,IDWKNUM,IDTYY  ! for correcting calendar
#ifdef OSI
      integer ICPMOD     ! passed from tchild
#else
      integer*8 ICPMOD     ! passed from tchild
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDDBCLM'  ! set for subroutine

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

      helptopic='clm_db_manage'
      call gethelptext(helpinsub,helptopic,nbhelp)

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
     &    ' ','b select another db',
     &    'c create a new db','d copy standard db',
     &    ' ',' ',' ',' ',' ',' ',' ',' ',isw,idno,nbhelp)
        copydef=.true.
      elseif(llclmdb(1:linst).eq.linstd(1:linst))then

C If corportate database then offer the following choices:
        idno=1
        isw=0
        call MENUATOL('  ','Climate db',
     &    'a browse/edit db','b select another db',
     &    'c create a new db','d copy standard db',
     &    'e binary >> ascii export','f ascii >> binary import',
     &    'g EPW >> binary import',' ',' ',' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      else

C If db exists locally offer the following choices:
        idno=1
        isw=0
        call MENUATOL('  ','Climate db',
     &    'a browse/edit db','b select another db',
     &    'c create a new db','d copy standard db',
     &    'e binary >> ascii export','f ascii >> binary import',
     &    'g EPW >> binary import',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
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
     &              ' ',96,DCLIM,'climate file name',IER,nbhelp)
                elseif(llt.ge.96.and.llt.lt.124)then
                  CALL EASKF(llclmdb,' Climate file (esp-r binary)?',
     &              ' ',124,DCLIM,'climate file name',IER,nbhelp)
                elseif(llt.ge.124.and.llt.le.144)then
                  CALL EASKF(llclmdb,' Climate file (esp-r binary)?',
     &              ' ',144,DCLIM,'climate file name',IER,nbhelp)
                endif
              elseif(iglib.eq.2)then
                CALL EASKF(llclmdb,' Climate file (esp-r binary)?',
     &            ' ',144,DCLIM,'climate file name',IER,nbhelp)
              else
                CALL EASKF(llclmdb,' Climate file (esp-r binary)?',
     &            ' ',96,DCLIM,'climate file name',IER,nbhelp)
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
     &      96,DCLIM,'climate file name',IER,nbhelp)
        elseif(iglib.eq.2)then
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      144,DCLIM,'climate file name',IER,nbhelp)
        else
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      96,DCLIM,'climate file name',IER,nbhelp)
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
        CALL ASKOK(' ','Browse/edit new climate data?',OK,dok,nbhelp)
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
          CALL ASKOK(' ','Browse/edit new climate data?',OK,dok,nbhelp)
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
     &      96,DCLIM,'climate file name',IER,nbhelp)
        elseif(iglib.eq.2)then
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      144,DCLIM,'climate file name',IER,nbhelp)
        else
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      96,DCLIM,'climate file name',IER,nbhelp)
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
          CALL ASKOK(' ','Browse/edit copied climate data?',
     &      OK,dok,nbhelp)
          if(ok)then
            call runit(doit,tmode)
          endif
        else
          goto 554
        endif

      elseif(isw.eq.5)then

C Climate binary >> ascii export.
C Confirm suggested name and then write out asci file.
        DFILE='./climate.a'
        LLASCI=' '
        write(LLASCI,'(2a)') LCLIM(1:lnblnk(LCLIM)),'.a'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      96,DFILE,'climate file name',IER,nbhelp)
        elseif(iglib.eq.2)then
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      144,DFILE,'climate file name',IER,nbhelp)
        else
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      96,DFILE,'climate file name',IER,nbhelp)
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
        CALL ASKOK(' ','Browse/edit exported climate data?',
     &    OK,dok,nbhelp)
        if(ok)then
          call runit(doit,tmode)
        endif
      elseif(isw.eq.6)then

C Climate ascii >> binary import
C Confirm suggested name and then write out binary file.
        DFILE='./climate.a'
        LLASCI=' '
        write(LLASCI,'(2a)') LCLIM(1:lnblnk(LCLIM)),'.a'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      96,DFILE,'climate file name',IER,nbhelp)
        elseif(iglib.eq.2)then
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      144,DFILE,'climate file name',IER,nbhelp)
        else
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      96,DFILE,'climate file name',IER,nbhelp)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

        longtmp=LCLIM
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      96,DCLIM,'climate file name',IER,nbhelp)
        elseif(iglib.eq.2)then
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      144,DCLIM,'climate file name',IER,nbhelp)
        else
          CALL EASKF(longtmp,' ',' Climate file name ? ',
     &      96,DCLIM,'climate file name',IER,nbhelp)
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
        CALL ASKOK(' ','Browse/edit exported climate data?',
     &    OK,dok,nbhelp)
        if(ok)then
          call runit(doit,tmode)
        endif
      elseif(isw.eq.7)then

C EPW >> binary import
C Confirm suggested name and then write out binary file.
        DFILE='./climate.a'
        LLASCI=' '
        write(LLASCI,'(2a)') LCLIM(1:lnblnk(LCLIM)),'.a'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      96,DFILE,'climate file name',IER,nbhelp)
        elseif(iglib.eq.2)then
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      144,DFILE,'climate file name',IER,nbhelp)
        else
          CALL EASKF(LLASCI,' ',' Climate file name (ASCII)?',
     &      96,DFILE,'climate file name',IER,nbhelp)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

        longtmp=LCLIM
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(longtmp,' ',' Climate file name (binary)? ',
     &      96,DCLIM,'climate file name',IER,nbhelp)
        elseif(iglib.eq.2)then
          CALL EASKF(longtmp,' ',' Climate file name (binary)? ',
     &      144,DCLIM,'climate file name',IER,nbhelp)
        else
          CALL EASKF(longtmp,' ',' Climate file name (binary)? ',
     &      96,DCLIM,'climate file name',IER,nbhelp)
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
        dok=.false.
        CALL ASKOK(' ','Browse/edit imported climate data?',
     &    OK,dok,nbhelp)
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
     &   '. The model currently uses ',sitelat,sitelongdif,'.'
      dok=.false.
      CALL ASKOK(outs,
     &  'Update your model with climate data?',OK,DOK,3)
      IF(OK)THEN
        sitelat=CLAT
        sitelongdif=CLONG
        WRITE(OUTS,'(A,F6.2,A,F6.2,A)')'A latitude of ',sitelat,
     &    ' and longitude diff of ',sitelongdif,
     &    ' is associated with the model.'
        CALL EDISP(IUOUT,OUTS)
        CALL EDISP(IUOUT,' ')
      ENDIF

C Update year 
      dok=.false.
      WRITE(OUTS,'(A,I4,a,I4,a)')'The climate year is ',ICYEAR,
     &  '. and the model year is ',IYEAR,'.'      
      CALL ASKOK(OUTS,
     &  'Use climate year in your model? (see help)',OK,DOK,nbhelp)
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

C Perform check that calendar day types correspond with simulation year.
C The logic below assumes that the first three day types are still weekday
C saturday sunday. It loops through each Julian day and if the model
C calendar day type is one of the standard 3 it checks that the model weekday
C is the same as a Julian weekday. Any calendar day types beyond 3 are 
C retained. If no calendar was defined in the model then it should do the
C work silently. (Same logic as in esystem.F) 
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
              IF(ICALENDER(IDOL).EQ.0)THEN
                ICALENDER(IDOL)=IDTYY  ! update the model calendar
                ICREPORT=2             ! do it silently
              ELSEIF(ICALENDER(IDOL).NE.IDTYY)THEN
                ICALENDER(IDOL)=IDTYY  ! update the model calendar
                ICREPORT=1
              ENDIF
            ENDIF
 123      CONTINUE
          if(ICREPORT.EQ.1)then
            CALL EDISP(IUOUT,'  ')
            CALL EDISP(IUOUT,
     &        'Simulation year and calendar mismatch rectified')
          endif
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

C ********** EDDBMSC
C EDDBMSC Changes the active components database current cfg file references.
C moddb is from version manager
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.
C This facility offers browsing for GTK version and follows the pattern
C of the other db file management facilties except that there is no
C working option for creating a new database and editing of the database
C is via a text editor.
      SUBROUTINE EDDBMSC(moddb,APP)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN

      common/texted/tedlbl,teditor
      character tedlbl*20,teditor*20

      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path
      common/deflt4/dinstpath

      CHARACTER LTMP*72,lltmp*144,lprev*144
      character longtfile*144,longtfiledos*144,LCOPY*144
      character lguess*144,message*48
      CHARACTER DOIT*300,fs*1,APP*4,ext*4
      CHARACTER outs248*248
      LOGICAL OK,COPYDEF,MODDB,XST,DOK,concat
      character cfgroot*24,LCFGF*72
      character dinstpath*48,path*72,dirpath*48
      character sourcefile*144  ! the file to copy ??
      character tmode*8

C Local strings for user selections dependent on graphic library.
      character opte*28,optf*28,optg*28
      character lpath*72,fname*72 ! for use with fdroot
      integer lndbp   ! for length of standard database path
      logical unixok  ! to check for database path file separators
      integer iglib  ! for detecting GTK or X11

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDDBMSC'  ! set for subroutine

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
      if(ipathmsc.eq.0.or.ipathmsc.eq.1)then
        lprev=MCMPDBFL
      elseif(ipathmsc.eq.2)then
        write(lprev,'(3a)') standarddbpath(1:lndbp),fs,
     &    MCMPDBFL(1:lnblnk(MCMPDBFL))
      endif

C Debug.
C      write(outs248,*) 'Current misc components ',
C     &   MCMPDBFL(1:lnblnk(MCMPDBFL))
C      call edisp248(iuout,outs248,100)

  60  continue
      if(ipathmsc.eq.0.or.ipathmsc.eq.1)then
        lltmp=MCMPDBFL  ! use as is
      elseif(ipathmsc.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    MCMPDBFL(1:lnblnk(MCMPDBFL))  ! prepend db folder path
      endif

      helptopic='misc_db_manage'
      call gethelptext(helpinsub,helptopic,nbhelp)

      IMCFIL=IFIL+1
      CALL ERPFREE(IMCFIL,ISTAT)

C If local or absolute path call addpath otherwise if a standard
C database then set longtfile equal to LLTMP.
      if(ipathmsc.eq.0.or.ipathmsc.eq.1)then
        call addpath(LLTMP,longtfile,concat)
      else
        longtfile=LLTMP
      endif
      ltf=max(1,LNBLNK(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up additional options depending on graphics lib. The second
C option (list select) is not available for msc comp database. If using
C text or X11 option e edits the string while for GTK there are two
C options to browse for files in ../dbs or the esp-r distribution.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        opte='e edit file name'
        optf='   '
        optg='   '
      elseif(iglib.eq.2)then
        opte='e browse model ../dbs'
        optf='f browse esp-r distribution'
        optg='g make copy of another file'
      endif

      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Miscellaneous components db',
     &    ' ','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.7)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If corportate database then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Miscellaneous components db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If corportate database then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Miscellaneous components db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default database or the current database.
        idno=1
        isw=0
        call MENUATOL('  ','Miscellaneous components db',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    opte,optf,optg,' ',' ',' ',' ',' ',isw,idno,nbhelp)
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
          call findwhichdbpath('msc',lltmp,ier)
          goto 44
        else
          call edisp(iuout,
     &    'No file to browse/edit. Please use another option.')
          MCMPDBFL=lprev
          goto 60
        endif
      elseif(isw.eq.2)then

C There is no select by list for misc comp database.
        call edisp(iuout,
     &    'No list select for misellaneous components databases.')
        goto 60
      elseif(isw.eq.3)then

C There is no select by list for misc comp database.
        call edisp(iuout,
     &    'No create new for misellaneous components databases.')
        goto 60

      elseif(isw.eq.4.or.isw.eq.7)then

C Copy a database to project dbs folder. If iws is 4 then the
C user has asked for the default database. If isw is 7 then the
C user wishes to browse for the source file. Suggest a local
C file name based on the project root name then do the copy.
C Use LTMP (short buffer) for the file name as known to esp-r
C and LCOPY for the system cp call.
        if(copydef)then
          write(outs248,'(2a)') 'The source file is:',
     &      DMCMPDBFL(1:lnblnk(DMCMPDBFL))
          call edisp248(iuout,outs248,100)
          sourcefile = DMCMPDBFL
        else

C Provide a guess based on the current MCMPDDBFL.
          lltmp='  '
          lguess='  '
          if(ipathmsc.eq.0.or.ipathmsc.eq.1)then
            lguess=MCMPDBFL  ! use as is
          elseif(ipathmsc.eq.2)then
            write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &        MCMPDBFL(1:lnblnk(MCMPDBFL))  ! prepend db folder path
          endif
          call edisp(iuout,'  ')  ! echo blank line
          CALL EASKXORGTKF(lguess,'Miscellaneous comp db (source)?',
     &      ' ',DMCMPDBFL,lltmp,'miscel comp database',IER,nbhelp)
          sourcefile = lltmp

C If user request jump back and re-display the menu.
          if(ier.eq.-3)then
            goto 60  ! cancel detected, redisplay menu.
          endif
          ipathmsc=1  ! signal that it will be a local database.
        endif

        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.miscdb'
        elseif(dbspth(1:3).eq.'../')then
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.miscdb'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.miscdb'
        endif
  66    CALL EASKS(LTMP,
     &    'Database name after it is copied into the project?',
     &    ' ',72,'optics.db','copied msc comp database',IER,nbhelp)
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

C Now copy sourcefile to lcopy.
          doit = ' '
          if(unixok)then
            write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &        sourcefile(1:lnblnk(sourcefile)),' ',
     &        LCOPY(1:lnblnk(LCOPY))
          else

C Standard dos copy logic.
            message='getting optics db file from '
            call dblongdoscopy(sourcefile,lcopy,message,doit,ier)
          endif
          call usrmsg('copying default file via',doit,'-')
          call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
          call FINDFIL(LCOPY,XST)
          if(XST)then
            call fdroot(lcopy,lpath,fname)
            write(MCMPDBFL,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          else
            call FINDFIL(LTMP,XST)
            if(XST)then
              write(MCMPDBFL,'(a)') LTMP(1:lnblnk(LTMP))
            else
              write(MCMPDBFL,'(a)') LTMP(1:lnblnk(LTMP))
              call edisp(iuout,'Problem locating the new database.')
              call edisp(iuout,'Try browsing ../dbs to find it.')
              call edisp(iuout,' ')
            endif
          endif
          moddb=.true.
          ipathmsc=1  ! signal that it is a local database.
          goto 44
        else
          goto 66
        endif

      elseif(isw.eq.5)then

C Make up browse to the model ../dbs folder suggest the name LCOPY
C and return file name in lltmp.
        lltmp=' '
        call edisp(iuout,'  ')  ! echo blank line
        write(LCOPY,'(2a)') dbspth(1:lnblnk(dbspth)),fs
        CALL EASKXORGTKF(LCOPY,'Miscellaneous comp db?',' ',
     &    DMCMPDBFL,lltmp,'misc comp db',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 60  ! redisplay menu.
        endif

        if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then

C Ask user whether file is in ../dbs and if so use fdroot if GTK.
          if(iglib.eq.2)then
            dok=.true.
            CALL ASKOK('Is this file in the model ../dbs folder?',
     &        ' ',OK,dok,nbhelp)
            if(OK)then
              call fdroot(lltmp,lpath,fname)
              write(MCMPDBFL,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &          fname(1:lnblnk(fname))
              ipathmsc=1  ! signal that it is a local database.
              moddb=.true.
              goto 44
            endif
          endif

C Check what the user might have edited to be sure of setting the
C correct location value via call to findwhichdbpath.
          call findwhichdbpath('msc',lltmp,ier)
          moddb=.true.
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.6)then

C Browse distribution database folder. Setup LCOPY to point to
C the database folder (but no particular file). Use easkf to
C return a file (which might be from somewhere else if the user
C decides to do some browsing). Test this suggested file via
C call to eroptdb and if this is not successful reset LOPTDB.
        if(iglib.eq.2)then
          write(LCOPY,'(4a)') dinstpath(1:lnblnk(dinstpath)),
     &      fs,'databases',fs
          CALL EASKF(LCOPY,' ','Miscellaneous components db?',
     &      144,DMCMPDBFL,'misc comp database',IER,nbhelp)
          if(ier.eq.-3)then
            MCMPDBFL=lprev
            goto 60  ! cancel detected, restore name and redisplay menu.
          endif

C This is yet another place where we need to check what the user might
C have edited to be sure of setting the correct location value. The
C common block will be updated with call to findwhichdbpath.
          call findwhichdbpath('msc',LCOPY,ier)
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

C Debug.
C      write(outs248,'(2a)') 'Accessing miscellaneous comp db: ',
C     &  MCMPDBFL(1:lnblnk(MCMPDBFL))
C      call edisp248(iuout,outs248,100)

      if(ipathmsc.eq.0.or.ipathmsc.eq.1)then
        lltmp=MCMPDBFL  ! use as is
      elseif(ipathmsc.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    MCMPDBFL(1:lnblnk(MCMPDBFL))  ! prepend db folder path
      endif

C Depending on whether Unix or DOS based setup paths. If DOS
C then check for spaces in name and change / to \.
      call isunix(unixok)
      if(unixok)then
        call addpath(lltmp,longtfile,concat)
      else
        call addpath(lltmp,longtfile,concat)
        call cmdfiledos(longtfile,longtfiledos,ier)
        longtfile=' '
        longtfile=longtfiledos
      endif
      tmode='graph'
      if(teditor(1:2).eq.'vi')tmode='text'
      write(doit,'(a,2x,a)') teditor(1:lnblnk(teditor)),
     &  longtfile(1:lnblnk(longtfile))
      call runit(doit,tmode)

C If this is ok to use then set moddb true but do not try
C and overwrite the file name so that no additional mangling
C happens to the path.  This assumes that the user has not
C altered the file name when saving via the editor.
      dok=.false.
      CALL ASKOK('  ','Use this edited database (see help)?',
     &  OK,dok,nbhelp)
      if(OK)then
        moddb=.true.   ! set so user is asked to update cfg file
      else
        MCMPDBFL=lprev
      endif

      return
      end


C ********** EDDBMOULD
C EDDBMOULD Changes the mould isopleths current cfg file references.
C moddb is from version manager
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.
C This facility offers browsing for GTK version and follows the pattern
C of the other db file management facilties except that there is no
C working option for creating a new database and editing of the database
C is via a text editor.
      SUBROUTINE EDDBMOULD(moddb,APP)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/appw/iappw,iappx,iappy

      common/texted/tedlbl,teditor
      character tedlbl*20,teditor*20

      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path
      common/deflt4/dinstpath

      CHARACTER LTMP*72,lltmp*144,lprev*144
      character longtfile*144,longtfiledos*144,LCOPY*144
      character lguess*144,message*48
      CHARACTER DOIT*300,fs*1,APP*4,ext*4
      CHARACTER outs248*248
      LOGICAL OK,COPYDEF,MODDB,XST,DOK,concat
      character cfgroot*24,LCFGF*72
      character dinstpath*48,path*72,dirpath*48
      character sourcefile*144  ! the file to copy ??
      character tmode*8

C Local strings for user selections dependent on graphic library.
      character opte*28,optf*28,optg*28
      character lpath*72,fname*72 ! for use with fdroot
      integer lndbp   ! for length of standard database path
      logical unixok  ! to check for database path file separators
      integer iglib  ! for detecting GTK or X11
#ifdef OSI
      integer ICPMOD     ! passed from tchild
#else
      integer*8 ICPMOD     ! passed from tchild
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDDBMOULD'  ! set for subroutine

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
      if(ipathmould.eq.0.or.ipathmould.eq.1)then
        lprev=lfmould
      elseif(ipathmould.eq.2)then
        write(lprev,'(3a)') standarddbpath(1:lndbp),fs,
     &    lfmould(1:lnblnk(lfmould))
      endif

C Debug.
      write(outs248,*) 'Current mould isopleths ',
     &   lfmould(1:lnblnk(lfmould))
      call edisp248(iuout,outs248,100)

  60  continue
      if(ipathmould.eq.0.or.ipathmould.eq.1)then
        lltmp=lfmould  ! use as is
      elseif(ipathmould.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    lfmould(1:lnblnk(lfmould))  ! prepend db folder path
      endif

      helptopic='mould_db_manage'
      call gethelptext(helpinsub,helptopic,nbhelp)

      IMCFIL=IFIL+1
      CALL ERPFREE(IMCFIL,ISTAT)

C If local or absolute path call addpath otherwise if a standard
C database then set longtfile equal to LLTMP.
      if(ipathmould.eq.0.or.ipathmould.eq.1)then
        call addpath(LLTMP,longtfile,concat)
      else
        longtfile=LLTMP
      endif
      ltf=max(1,LNBLNK(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up additional options depending on graphics lib. The second
C option (list select) is not available for moulds. If using
C text or X11 option e edits the string while for GTK there are two
C options to browse for files in ../dbs or the esp-r distribution.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        opte='f edit file name'
        optf='   '
        optg='   '
      elseif(iglib.eq.2)then
        opte='f browse model ../dbs'
        optf='g browse esp-r distribution'
        optg='h make copy of another file'
      endif

      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Mould isopleths',
     &    ' ','b select from list',
     &    'c create a new db','d copy standard db',
     &    'e invoke mould utility',opte,optf,optg,
     &    ' ',' ',' ',' ',isw,idno,nbhelp)
        if(isw.eq.5)copydef=.true.
        if(isw.eq.8)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If corportate database then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Mould isopleths',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    'e invoke mould utility',opte,optf,optg,
     &    ' ',' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If corportate database then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Mould isopleths',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    'e invoke mould utility',opte,optf,optg,
     &    ' ',' ',' ',' ',isw,idno,nbhelp)
        copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default database or the current database.
        idno=1
        isw=0
        call MENUATOL('  ','Mould isopleths',
     &    'a browse/edit db','b select from list',
     &    'c create a new db','d copy standard db',
     &    'e invoke mould utility',opte,optf,optg,
     &    ' ',' ',' ',' ',isw,idno,nbhelp)
        if(isw.eq.5)copydef=.true.
        if(isw.eq.8)copydef=.false.
      endif

C Act on the users choice. If no choice, return to calling menu.
      if(isw.eq.0)then
        return
      elseif(isw.eq.1)then

C If file exists read it and enter editing facility after
C check to see if it is local or standard or absolute path.
        if(XST)then
          call findwhichdbpath('mld',lltmp,ier)
          goto 44
        else
          call edisp(iuout,
     &    'No file to browse/edit. Please use another option.')
          lfmould=lprev
          goto 60
        endif
      elseif(isw.eq.2)then

C There is no select by list for misc comp database.
        call edisp(iuout,'No list select for mould isopleths.')
        goto 60
      elseif(isw.eq.3)then

C There is no select by list for mould isopleths.
        call edisp(iuout,'No create new for mould isopleths.')
        goto 60

      elseif(isw.eq.4.or.isw.eq.8)then

C Copy a common data file to project dbs folder. If iws is 4 then the
C user has asked for the default database. If isw is 7 then the
C user wishes to browse for the source file. Suggest a local
C file name based on the project root name then do the copy.
C Use LTMP (short buffer) for the file name as known to esp-r
C and LCOPY for the system cp call.
        if(copydef)then
          write(outs248,'(2a)') 'The source file is:',
     &      dmdbnam(1:lnblnk(dmdbnam))
          call edisp248(iuout,outs248,100)
          sourcefile = dmdbnam
        else

C Provide a guess based on the current lfmould.
          lltmp='  '
          lguess='  '
          if(ipathmould.eq.0.or.ipathmould.eq.1)then
            lguess=lfmould  ! use as is
          elseif(ipathmould.eq.2)then
            write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &        lfmould(1:lnblnk(lfmould))  ! prepend db folder path
          endif
          call edisp(iuout,'  ')  ! echo blank line
          CALL EASKXORGTKF(lguess,'Mould isopleths (source)?',
     &      ' ',dmdbnam,lltmp,'mould isopleths',IER,nbhelp)
          sourcefile = lltmp

C If user request jump back and re-display the menu.
          if(ier.eq.-3)then
            goto 60  ! cancel detected, redisplay menu.
          endif
          ipathmould=1  ! signal that it will be a local database.
        endif

        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(LTMP,'(2a)') cfgroot(1:lr),'.moulddta'
        elseif(dbspth(1:3).eq.'../')then
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.moulddta'
        else
          write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &      cfgroot(1:lr),'.moulddta'
        endif
  66    CALL EASKS(LTMP,
     &    'File name after it is copied into the project?',
     &    ' ',72,'mould.db','copied mould isopleths',IER,nbhelp)
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

C Now copy sourcefile to lcopy.
          doit = ' '
          if(unixok)then
            write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &        sourcefile(1:lnblnk(sourcefile)),' ',
     &        LCOPY(1:lnblnk(LCOPY))
          else

C Standard dos copy logic.
            message='getting mould file from '
            call dblongdoscopy(sourcefile,lcopy,message,doit,ier)
          endif
          call usrmsg('copying default file via',doit,'-')
          call runit(doit,'-')

C Use LTMP or LCOPY depending on the path because the path will already
C be pre-pended when using most inbuilt file opening calls.
          call FINDFIL(LCOPY,XST)
          if(XST)then
            call fdroot(lcopy,lpath,fname)
            write(lfmould,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          else
            call FINDFIL(LTMP,XST)
            if(XST)then
              write(lfmould,'(a)') LTMP(1:lnblnk(LTMP))
            else
              write(lfmould,'(a)') LTMP(1:lnblnk(LTMP))
              call edisp(iuout,'Problem locating the new file.')
              call edisp(iuout,'Try browsing ../dbs to find it.')
              call edisp(iuout,' ')
            endif
          endif
          moddb=.true.
          ipathmould=1  ! signal that it is a local database.
          goto 44
        else
          goto 66
        endif

      elseif(isw.eq.5)then

C Start the mould utility.
        doit = ' '
        call tchild(ICPMOD)
        call termode(ICPMOD,tmode)
        if(iappw.gt.0.and.iappw.le.200)then
          write(doit,'(3a,3i4,3a)') 'mld -mode ',tmode,
     &      ' -s ',iappw,iappx+10,iappy+40,' -file ',
     &      longtfile(1:lnblnk(longtfile)),' &'
        else
          write(doit,'(5a)') 'mld -mode ',tmode,
     &      ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),
     &      ' &'
        endif
        call runit(doit,tmode)
        goto 60  ! redisplay menu.

      elseif(isw.eq.6)then

C Make up browse to the model ../dbs folder suggest the name LCOPY
C and return file name in lltmp.
        lltmp=' '
        call edisp(iuout,'  ')  ! echo blank line
        write(LCOPY,'(2a)') dbspth(1:lnblnk(dbspth)),fs
        CALL EASKXORGTKF(LCOPY,'Mould isopleths?',' ',
     &    dmdbnam,lltmp,'mould isopleths',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 60  ! redisplay menu.
        endif

        if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then

C Ask user whether file is in ../dbs and if so use fdroot if GTK.
          if(iglib.eq.2)then
            dok=.true.
            CALL ASKOK('Is this file in the model ../dbs folder?',
     &        ' ',OK,dok,nbhelp)
            if(OK)then
              call fdroot(lltmp,lpath,fname)
              write(lfmould,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &          fname(1:lnblnk(fname))
              ipathmould=1  ! signal that it is a local database.
              moddb=.true.
              goto 44
            endif
          endif

C Check what the user might have edited to be sure of setting the
C correct location value via call to findwhichdbpath.
          call findwhichdbpath('mld',lltmp,ier)
          moddb=.true.
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.7)then

C Browse distribution database folder. Setup LCOPY to point to
C the database folder (but no particular file). Use easkf to
C return a file (which might be from somewhere else if the user
C decides to do some browsing). Test this suggested file via
C call to eroptdb and if this is not successful reset LOPTDB.
        if(iglib.eq.2)then
          write(LCOPY,'(4a)') dinstpath(1:lnblnk(dinstpath)),
     &      fs,'databases',fs
          CALL EASKF(LCOPY,' ','Mould isopleths file?',
     &      144,dmdbnam,'mould isopleths',IER,nbhelp)
          if(ier.eq.-3)then
            lfmould=lprev
            goto 60  ! cancel detected, restore name and redisplay menu.
          endif

C This is yet another place where we need to check what the user might
C have edited to be sure of setting the correct location value. The
C common block will be updated with call to findwhichdbpath.
          call findwhichdbpath('mld',LCOPY,ier)
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

C Debug.
      write(outs248,'(2a)') 'Accessing mould isopleths: ',
     &  lfmould(1:lnblnk(lfmould))
      call edisp248(iuout,outs248,100)

      if(ipathmould.eq.0.or.ipathmould.eq.1)then
        lltmp=lfmould  ! use as is
      elseif(ipathmould.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    lfmould(1:lnblnk(lfmould))  ! prepend db folder path
      endif

C Depending on whether Unix or DOS based setup paths. If DOS
C then check for spaces in name and change / to \.
      call isunix(unixok)
      if(unixok)then
        call addpath(lltmp,longtfile,concat)
      else
        call addpath(lltmp,longtfile,concat)
        call cmdfiledos(longtfile,longtfiledos,ier)
        longtfile=' '
        longtfile=longtfiledos
      endif
      tmode='graph'
      if(teditor(1:2).eq.'vi')tmode='text'
      write(doit,'(a,2x,a)') teditor(1:lnblnk(teditor)),
     &  longtfile(1:lnblnk(longtfile))
      call runit(doit,tmode)

C If this is ok to use then set moddb true but do not try
C and overwrite the file name so that no additional mangling
C happens to the path.  This assumes that the user has not
C altered the file name when saving via the editor.
      dok=.false.
      CALL ASKOK('  ','Use this edited database (see help)?',
     &  OK,dok,nbhelp)
      if(OK)then
        moddb=.true.   ! set so user is asked to update cfg file
      else
        lfmould=lprev
      endif

      return
      end

