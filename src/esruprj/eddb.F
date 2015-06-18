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
C standard data files:
C  EDDB  : Controls specification/ browsing of standard data files.
C  opendb: Standard opening of thermophysical and optical data.
C EDDBMATERIAL Changes the materials referemces in cfg file.
C EDDBMLCON Changes the constructions references in the cfg file.
C EDDBPCDB Changes the plant components current cfg file references.
C EDDBPROFILE Changes the events profile in cfg file.
C EDDBOPT Changes the common optics in cfg file.
C EDDBCLM: Changes the weather file in cfg file.
C EDDBMSC Changes the active components in cfg file.

C ********** EDDB 
C Controls specification/ browsing of standard data files.
C ITRU is channel for user output, IER=0 means no errors encountered.

      SUBROUTINE EDDB(IER)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "MultiYear_simulations.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for wind pressure coefficients)
C LOPTDB,IOPTDB (for optical properties)
C LPCDB/IPCDB (for plant templates)
C LPRFDB,IPRODB (for event profiles)
C LFMAT,IFMAT,LFMUL,IFMUL (for standard materials and constructions)
C ICLIM,LCLIM (for weather file)
C MCMPDBFL (active components file)
C default file names for standard files
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      common/rpath/path
      common/user/browse

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      logical OK,DOK,XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical moddb,browse,modcon,QUIET
      logical haveconstr
      integer ihd  ! for radio button
      integer NITMS,INO ! max items and current menu item

      character path*72
      character*72 ITEMS(18)
      character lworking*144  ! for processing file names
      
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

C Begin with menu of the current files. If there is
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
        lworking=lfmould
        call findwhichdbpath('mld',lworking,ier)
        lworking=LCLIM
        call findwhichdbpath('clm',lworking,ier)
        lworking=LCFCDB
        call findwhichdbpath('cfc',lworking,ier)
      endif
 
    3 INO=-4
      IER=0

      if ( bMY_climates_defined ) then
        cTemp = cMY_climate_db_name
      else
        cTemp = 'None'
      endif
      
      lndbp=lnblnk(standarddbpath)
      WRITE(ITEMS(1),'(A)')'  Folder paths: '
      if(lndbp.lt.52)then
        WRITE(ITEMS(2),'(2A)')'  Standard <std> = ',
     &    standarddbpath(1:lndbp)
      else
        WRITE(ITEMS(2),'(2A)')'  Standard <std> = ',
     &    standarddbpath(1:52)
      endif
      if(cfgroot(1:2).ne.'  '.and.cfgroot(1:4).ne.'UNKN')then
        lnpth=lnblnk(path)
        lndbpth=lnblnk(dbspth)
        if(lnpth+lndbpth.lt.55)then
          WRITE(ITEMS(3),'(3A)')
     &      '  Model <mod> = ',path(1:lnpth),dbspth(1:lndbpth)
        else
          if(lnpth.lt.50)then
            WRITE(ITEMS(3),'(3A)')
     &        '  Model <mod> = ',path(1:lnpth),dbspth(1:5)
          else
            WRITE(ITEMS(3),'(2A)')
     &        '  Model <mod> = ',path(1:lnpth)
          endif
        endif
      else
        WRITE(ITEMS(3),'(A)')
     &    '  Model <mod> = no model defined yet'
      endif
      ITEMS(4) ='  _______________________________ '
      if(ipathclim.eq.0)then
        WRITE(ITEMS(5),'(2A)')'a annual weather        : ',
     &    LCLIM(1:43)
      elseif(ipathclim.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(5),'(2A)')'a annual weather        : <mod>',
     &    LCLIM(lndbpth:38)
      elseif(ipathclim.eq.2)then
        WRITE(ITEMS(5),'(2A)')'a annual weather        : <std>',
     &    LCLIM(1:38)
      endif

      WRITE(ITEMS(6),'(2A)')  'b multi-year weather    : ',cTemp(1:43)
      if(ipathmat.eq.0)then
        WRITE(ITEMS(7),'(2A)')'c material properties   : ',
     &    LFMAT(1:43)
      elseif(ipathmat.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(7),'(2A)')'c material properties   : <mod>',
     &    LFMAT(lndbpth:38)  ! try to print file name only
      elseif(ipathmat.eq.2)then
        WRITE(ITEMS(7),'(2A)')'c material properties   : <std>',
     &    LFMAT(1:38)
      endif
      if(ipathoptdb.eq.0)then
        WRITE(ITEMS(8),'(2A)')'d optical properties    : ',
     &    LOPTDB(1:43)
      elseif(ipathoptdb.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(8),'(2A)')'d optical properties    : <mod>',
     &    LOPTDB(lndbpth:38)
      elseif(ipathoptdb.eq.2)then
        WRITE(ITEMS(8),'(2A)')'d optical properties    : <std>',
     &    LOPTDB(1:38)
      endif

      if(ipathmul.eq.0)then
        WRITE(ITEMS(9),'(2A)')'e constructions         : ',
     &    LFMUL(1:43)
      elseif(ipathmul.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(9),'(2A)')'e constructions         : <mod>',
     &    LFMUL(lndbpth:38)
      elseif(ipathmul.eq.2)then
        WRITE(ITEMS(9),'(2A)')'e constructions         : <std>',
     &    LFMUL(1:38)
      endif
      if(ipathmsc.eq.0)then
        WRITE(ITEMS(10),'(2A)')'f active components     : ',
     &    MCMPDBFL(1:43)
      elseif(ipathmsc.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(10),'(2A)')'f active components     : <mod>',
     &    MCMPDBFL(lndbpth:38)
      elseif(ipathmsc.eq.2)then
        WRITE(ITEMS(10),'(2A)')'f active components     : <std>',
     &    MCMPDBFL(1:38)
      endif
      if(ipathprodb.eq.0)then
        WRITE(ITEMS(11),'(2A)')'g event profiles        : ',
     &    LPRFDB(1:43)
      elseif(ipathprodb.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(11),'(2A)')'g event profiles        : <mod>',
     &    LPRFDB(lndbpth:38)
      elseif(ipathprodb.eq.2)then
        WRITE(ITEMS(11),'(2A)')'g event profiles        : <std>',
     &    LPRFDB(1:38)
      endif
      if(ipathapres.eq.0)then
        WRITE(ITEMS(12),'(2A)')'h pressure coefficients : ',
     &    LAPRES(1:43)
      elseif(ipathapres.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(12),'(2A)')'h pressure coefficients : <mod>',
     &    LAPRES(lndbpth:38)
      elseif(ipathapres.eq.2)then
        WRITE(ITEMS(12),'(2A)')'h pressure coefficients : <std>',
     &    LAPRES(1:38)
      endif
      if(ipathpcdb.eq.0)then
        WRITE(ITEMS(13),'(2A)')'i plant components      : ',
     &    LPCDB(1:43)
      elseif(ipathpcdb.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(13),'(2A)')'i plant components      : <mod>',
     &    LPCDB(lndbpth:38)
      elseif(ipathpcdb.eq.2)then
        WRITE(ITEMS(13),'(2A)')'i plant components      : <std>',
     &    LPCDB(1:38)
      endif
      if(ipathmould.eq.0)then
        WRITE(ITEMS(14),'(2A)')'j mould isopleths       : ',
     &    lfmould(1:43)
      elseif(ipathmould.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(14),'(2A)')'j mould isopleths       : <mod>',
     &    lfmould(lndbpth:38)
      elseif(ipathmould.eq.2)then
        WRITE(ITEMS(14),'(2A)')'j mould isopleths       : <std>',
     &    lfmould(1:38)
      endif
      if(ipathcfc.eq.0)then
        WRITE(ITEMS(15),'(2A)')'k CFC layers            : ',
     &    LCFCDB(1:43)
      elseif(ipathcfc.eq.1)then
        lndbpth=lnblnk(dbspth)+2
        WRITE(ITEMS(15),'(2A)')'k CFC layers            : <mod>',
     &    LCFCDB(lndbpth:38)
      elseif(ipathcfc.eq.2)then
        WRITE(ITEMS(15),'(2A)')'k CFC layers            : <std>',
     &    LCFCDB(1:38)
      endif

      ITEMS(16)='  _______________________________ '
      ITEMS(17)='? help                            '
      ITEMS(18)='- exit this menu                  '

C Try to compact the width of the menu.
      IW=0
      do 141 ij=1,18
        if(lnblnk(ITEMS(ij)).gt.IW)IW=lnblnk(ITEMS(ij))
 141  continue

C Help text for this menu.
      helptopic='database_mgt_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
      NITMS=18
      if(MMOD.EQ.8)then
        impx=0
        impy=0
        iwe=iw
        CALL VWMENU('Standard data maintenance',ITEMS,NITMS,impx,impy,
     &    iwe,irpx,irpy,INO)
      else
        CALL EMENU('Standard data maintenance',ITEMS,NITMS,INO)
      endif
      IF(INO.EQ.18)THEN
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
          write(LCFCDB,'(a)') DCFCDB(1:lnblnk(DCFCDB))
          return
        endif

C If a model has been loaded and there are possible changes ask
C user for confirmation.
        if(cfgok.and.moddb)then
          dok=.true.
          CALL ASKOK(
     &      'A possible change in file names detected. Update the',
     &      'model NAME LIST to match? (see help)',
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
     &         'standard file changes? (see help)',OK,dok,nbhelp)
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

      elseif(INO.EQ.5)then

C Weather file.
        call eddbclm(moddb,'      ',-1)

      ELSEIF(INO.eq.6)THEN

C Multi-year weather file - only works in conjunction
C with save level 5.
         call usrmsg('The use of multi-year assessments is',
     &               'limited to results save level 5.','W')
         call MY_clm_db_menu()

      ELSEIF(INO.EQ.7)THEN

C Manage materials db and if a model loaded then warn user 
C if db name or contents have changed.
        call eddbmaterial(moddb,modcon,'    ')
        if(cfgroot(1:2).ne.'  '.and.cfgroot(1:4).ne.'UNKN')then
          if(moddb)then
            call usrmsg(
     &      'The file name might have changed. Suggest',
     &      'using the `save model` option to record this.','W')
          endif
          if(modcon)then
            call usrmsg(
     &      'The file details might have changed. In a moment',
     &      'you will be offered the option to update zone files.','W')
          endif
        endif

      ELSEIF(INO.EQ.8)THEN

C Optical properties.
        call eddbopt(moddb,'    ')
        if(cfgroot(1:2).ne.'  '.and.cfgroot(1:4).ne.'UNKN')then
          if(moddb)then
            call edisp(iuout,
     &      'The file name or details have changed. If you')
            call edisp(iuout,
     &      'want to keep it use the `save model` option.')
            call edisp(iuout,' ')
          endif
        endif

      ELSEIF(INO.EQ.9)THEN

C Deal with constructions.
        call eddbmlcon(moddb,modcon,'    ')
        if(cfgroot(1:2).ne.'  '.and.cfgroot(1:4).ne.'UNKN')then
          if(modcon)then
            call usrmsg(
     &      'The file details might have changed. In a moment',
     &      'you will be offered the option to update zone files.','W')
          endif
        endif

      ELSEIF(INO.EQ.10)THEN

C Manage common active components file.
        call EDDBMSC(moddb,'    ')

      ELSEIF(INO.EQ.11)THEN

C Event profiles.
        call eddbprofile(moddb,'    ')
        if(cfgroot(1:2).ne.'  '.and.cfgroot(1:4).ne.'UNKN')then
          if(moddb)then
            call edisp(iuout,
     &      'The file name or details have changed. If you')
            call edisp(iuout,
     &      'want to keep it use the `save model` option.')
            call edisp(iuout,' ')
          endif
        endif
        
      ELSEIF(INO.EQ.12)THEN

C Pressure distribution db.
        call eddbpres(moddb,'    ')

      ELSEIF(INO.EQ.13)THEN

C Plant templates.
        call eddbpcdb(moddb,'    ')
        if(cfgroot(1:2).ne.'  '.and.cfgroot(1:4).ne.'UNKN')then
          if(moddb)then
            call edisp(iuout,
     &      'The file name or details have changed. If you')
            call edisp(iuout,
     &      'want to keep it use the `save model` option.')
            call edisp(iuout,' ')
          endif
        endif

      ELSEIF(INO.EQ.14)THEN

C Mould isopleths.
        call EDDBMOULD(moddb,'    ')

      ELSEIF(INO.EQ.15)THEN

C Manage CFC layers db and if a model loaded then warn user 
C if db name or contents have changed.
        call eddbcfc(moddb,modcon,'    ')
        if(cfgroot(1:2).ne.'  '.and.cfgroot(1:4).ne.'UNKN')then
          if(moddb)then
            call usrmsg(
     &      'The file name might have changed. Suggest',
     &      'using the `save model` option to record this.','W')
          endif
          if(modcon)then
            call usrmsg(
     &      'The file details might have changed. In a moment',
     &      'you will be offered the option to update zone files.','W')
          endif
        endif

      ELSEIF(INO.EQ.17)THEN

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

C Open materials, constructions and optical properties. In the 
C case of materials, first assume it is a binary file, check its
C contents and if a problem then scan new ascii format.
C Uf that does not work try the older ascii materials file to
C fill the materials data arrays.
C If sucessful the material common blocks will be filled and
C closemat1 or closemat2 will be set.

      subroutine opendb(ier)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      logical XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical closemat1,closemat2
      character LASCI*144,DFILE*144,fs*1
      character SOPT*12,outs248*248,GDESCR*36
      character t144*144   ! for use with erprcdb
      character lworking*144 
      integer lndbp   ! for length of standard path
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
C binary file is for the wrong machine type. Guess the name of
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
                call usrmsg('No readable Materials were found or',
     &                      'or the file was corrupt.','W')
                MATDBOK=.FALSE.
              else
                call eclose(matver,1.1,0.001,closemat1)
                call eclose(matver,1.1,0.001,closemat2)
                if(closemat1.or.closemat2)then

C Materials data in place, set this as the new file name and then
C try and re-scan the Constructions.
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
C try and re-scan the constructions.
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

C Ask user for ascii Materials file to convert.
            IAF=IFIL+1
            DFILE=' '
            CALL EASKS(LASCI,'Materials file (ASCII)','Confirm:',
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
C Eddbmaterial Changes the materials current cfg file references.
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

C      common/rpath/path
C      character path*72

C Default ESP-r distribution path.
      common/deflt4/dinstpath

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical moddb,modcon,copydef,clkok,chgdb
      logical closemat1,closemat2

      character LLTMP*144,LTMP*92
      character fs*1
      character DFILE*144,LLASCI*144,LLTMPFL*144
      character lprev*144
      character longtfile*144
      character lworking*144
      character dirpath*72

      CHARACTER APP*4
C      CHARACTER outs248*248
      LOGICAL OK,XST,DOK,concat
      character dinstpath*60

C Local strings for user selections dependent on graphic library.
      integer lndbp   ! for length of standard path
      integer ier
      logical unixok  ! to check for path file separators
      logical allowbrowse ! to be done

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

C If local or absolute path call addpath otherwise if a standard
C file then set longtfile equal to LLTMP.
      if(ipathmat.eq.0.or.ipathmat.eq.1)then
        call addpath(lltmp,longtfile,concat)
      elseif(ipathmat.eq.2)then
        longtfile=lltmp
      endif
      ltf=max(1,lnblnk(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up options depending on where the file is.
      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Material properties',
     &    ' ','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',
     &    'f binary >> ascii export','g ascii >> binary',
     &    'h create new materials',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If standard file then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Material properties <std>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',
     &    'f binary >> ascii export','g ascii >> binary',
     &    'h create new materials',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5)copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If standard file then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Material properties <std>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',
     &    'f binary >> ascii export','g ascii >> binary',
     &    'h create new materials',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5)copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default file or the current database.
        idno=1
        isw=0
        call MENUATOL('  ','Material properties <mod>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',
     &    'f binary >> ascii export','g ascii >> binary',
     &    'h create new materials',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5)copydef=.false.
      endif

C << Layout of prior menu. NOTE: when menu order changes the script
C << bin/materials_db_conv needs to be updated to support the Install
C << process.
C        call MENUATOL('  ','Material properties <mod>',
C     &    'a browse/edit','b select file from list',
C     &    'c create new materials','d copy standard materials',
C     &    'e binary >> ascii convert','f ascii >> binary convert',
C     &    opte,optf,optg,' ',' ',' ',isw,idno,nbhelp)
C        if(isw.eq.4)copydef=.true.
C        if(isw.eq.9)copydef=.false.

C Act on the user's choice. If no choice return to calling menu.
      if(isw.eq.0)then
        return
      elseif(isw.eq.1)then

C If file exists read it and enter editing facility after
C check to see if it is local or standard or absolute path.
        if(XST)then
          call findwhichdbpath('mat',lltmp,ier)
          goto 44  ! process the file
        else
          call edisp(iuout,
     &    'No file to browse/edit. Please use another option.')
          LFMAT=lprev
          goto 60
        endif

      elseif(isw.eq.2)then

C Allow user to select from the distribution databases folder for an
C materials file. If moddb was set to true then proceed to edit
C the materials.
        numhelp=nbhelp
        lltmp='  '
        call usecommondbsfile('mat',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44  ! process the file
        else
          goto 60
        endif

      elseif(isw.eq.8)then

C User requested the creation of a new materials file. Set up minimal
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
        CALL EASKSCNCL(ltmp,'Name for new material properties file',
     &    'Confirm:','cancel',clkok,92,DFCON,'new materials',
     &    IER,nbhelp)
        if(clkok)then
          LFMAT=lprev  ! user canceled, restore initial name
          return
        endif
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          write(LFMAT,'(a)') ltmp(1:lnblnk(ltmp))
          ipathmat=1  ! this is a local materials.
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
C file which will then be scanned into materials arrays.

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

C After creating the new file close the file unit.
        CALL ERPFREE(IAF,ISTAT)
        call pausems(400)

C Now scan the scratch file into material arrays.
        ier=0
        call rlegacymat(IAF,LLTMPFL,ier)
        if(ier.ne.0)then
          call usrmsg('The initial materials data could not',
     &                'be scanned into memory!','W')
        else
          call usrmsg('An initial set of material categories has been',
     &                'setup and you can modify as required.','-')
        endif

C And now write out the file with the materials arrays. mkascimat
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
        CALL ASKOK(' ','Browse or edit new file?',OK,dok,nbhelp)
        if(OK)then
          moddb=.true.
          goto 44  ! process the file
        endif

      elseif(isw.eq.4)then

C Copy standard file to model folder. Suggest a local
C file name based on the project root name. Update common to
C reflect it is local.
        allowbrowse=.false.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('mat',copydef,allowbrowse,
     &  numhelp,lltmp,istat,moddb)
     
        if(istat.eq.1)then
          LFMAT=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44  ! process the file
        endif

      elseif(isw.eq.6)then

C Materials db: BINARY > ASCII. Open the ascii file on IAF=IFIL+1.

C Do not assume that LFMAT is necessarily the binary file
C the user may have been working earlier with an ascii version
C revise to request the name of the binary file before opening it.
        write(LLTMP,'(a)') LFMAT(1:lnblnk(LFMAT))  ! remember current file name
        lsn2=MIN0(lnblnk(LLTMP),92)
        LTMP=' '
        write(LTMP,'(a)') LLTMP(1:lsn2)
        CALL EASKSCNCL(LTMP,'Material properties (binary)?',
     &    'Confirm:','cancel',clkok,92,DFILE,'materials (binary)',
     &    IER,nbhelp)
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
        CALL EASKSCNCL(LTMP,'Materials properties file (ascii).',
     &    'Confirm:','cancel',clkok,92,DFILE,
     &    'materials file (ascii)',IER,nbhelp)
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

C Use a call to MATFROMBIN (to scan binary file into common blocks)
C followed by a call to mkascimat.
            call MATFROMBIN(IER)  ! fill materials common blocks
            if(ier.eq.0)then
              CALL ERPFREE(IFMUL,ISTAT)  ! in case other unit is still open
              CALL mkascimat(IAF,LLASCI,IER)
            endif
          endif
        endif

C Before returning restore the original file and
C rescan into common blocks.
        write(LFMAT,'(a)') LLTMP(1:lnblnk(LLTMP)) ! restore previous
        call edisp(iuout,'Restoring the previous materials db')
        call edisp(iuout,LLTMP)
        goto 60    ! jump back and reload and re-display

      elseif(isw.eq.7)then

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
        CALL EASKSCNCL(LTMP,'Materials file (ascii) source.',
     &    'Confirm:','cancel',clkok,92,DFILE,'materials (ascii)',
     &    IER,nbhelp)
        if(clkok) return
        write(LLASCI,'(a)') LTMP(1:lnblnk(LTMP))

        DFILE='./newmaterial.db1'
        clkok=.false.
        lltmp=' '; ltmp=' '
        write(lltmp,'(a)') LFMAT(1:lnblnk(LFMAT))
        write(ltmp,'(a)') LFMAT(1:lnblnk(LFMAT))
 143    CALL EASKSCNCL(ltmp,'Materials properties (binary)?',
     &    'Confirm:','cancel',clkok,92,DFILE,'Materials (binary)',
     &    IER,nbhelp)
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

      elseif(isw.eq.3)then

C Allow user to select from the model ../dbs folder for an
C materials file. If moddb was set to true then proceed to edit
C the materials.
        numhelp=nbhelp
        lltmp='  '
        call usemodeldbsfile('mat',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44  ! process the file
        else
          goto 60
        endif

      elseif(isw.eq.5)then

C Create a menu list of files in the distribution databases folder
C If user requests, copy one of those files to the model folder.
        copydef=.false.
        allowbrowse=.true.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('mat',copydef,allowbrowse,
     &  numhelp,lltmp,istat,moddb)
     
        if(istat.eq.1)then
          LFMAT=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44  ! process the file
        endif

      endif

C Test the selected or copied file. If a problem loop back
C otherwise edit its contents.
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
        call usrmsg('Readable materials file was not found',
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
        call usrmsg('Materials arrays were not filled',
     &              'properly or file was corrupt','W')
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
        CALL EASKSCNCL(LTMP,'Materials (automatic ASCII backup).',
     &    'Confirm:','cancel',clkok,92,DFILE,'materials db (ascii)',
     &    IER,nbhelp)
        if(clkok)then
          continue
        else

C Write ASCII version from the common blocks and make it the new LFMAT.
C << shorter string until replacement of EASKXORGTKF >>
          write(LLASCI,'(a)') LTMP(1:lnblnk(LTMP))
          IAF=IFIL+1
          CALL ERPFREE(IFMUL,ISTAT)  ! in case other unit is still open
          CALL mkascimat(IAF,LLASCI,IER)
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

C ********** EDDBCFC
C Eddbcfc Changes the CFC layers db current cfg file references.
C moddb is from version manager
C modcon signals change in constructions.
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.
C Unused variables commented out.

      SUBROUTINE EDDBCFC(moddb,modcon,APP)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN

C      character path*72
C      common/rpath/path

C Default ESP-r distribution path.
      common/deflt4/dinstpath

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical moddb,modcon,copydef,chgdb
C      logical clkok,closemat2,DOK,OK
      logical closemat1

C      character LTMP*92,LLTMPFL*144
      character fs*1
      character LLTMP*144
      character lprev*144
      character longtfile*144
      character dirpath*72

      CHARACTER APP*4
C      CHARACTER outs248*248
      LOGICAL XST,concat
      character dinstpath*60

C Local strings for user selections dependent on graphic library.
      integer lndbp   ! for length of standard path
      integer ier
      logical unixok  ! to check for path file separators
      logical allowbrowse ! to be done

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDDBCFC'  ! set for subroutine

C Clear string buffers.
C      ltmp=' '
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
      if(ipathcfc.eq.0.or.ipathcfc.eq.1)then
        write(lprev,'(a)') LCFCDB(1:lnblnk(LCFCDB))
      elseif(ipathcfc.eq.2)then
        write(lprev,'(3a)') standarddbpath(1:lndbp),fs,
     &    LCFCDB(1:lnblnk(LCFCDB))
      endif

C Debug.
C      write(outs248,*) 'Current materials ',lprev(1:lnblnk(lprev))
C      call edisp248(iuout,outs248,100)

C Setup lltmp to hold the full path to the file for use in dialog.
  60  continue
      if(ipathcfc.eq.0.or.ipathcfc.eq.1)then
        lltmp=LCFCDB  ! use as is
      elseif(ipathcfc.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    LCFCDB(1:lnblnk(LCFCDB))  ! prepend db folder path
      endif

      helptopic='cfc_db_edit_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C If local or absolute path call addpath otherwise if a standard
C file then set longtfile equal to LLTMP.
      if(ipathcfc.eq.0.or.ipathcfc.eq.1)then
        call addpath(lltmp,longtfile,concat)
      elseif(ipathcfc.eq.2)then
        longtfile=lltmp
      endif
      ltf=max(1,lnblnk(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up options depending on where the file is.
      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','CFC layer properties',
     &    ' ','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',
     &    'f create new CFC layers db',' ',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If standard file then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','CFC layer properties <std>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',
     &    'f create new CFC layers db',' ',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If standard file then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','CFC layer properties <std>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',
     &    'f create new CFC layers db',' ',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default file or the current database.
        idno=1
        isw=0
        call MENUATOL('  ','CFC layer properties <mod>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',
     &    'f create new CFC layers db',' ',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5)copydef=.false.
      endif

C Act on the user's choice. If no choice return to calling menu.
      if(isw.eq.0)then
        return
      elseif(isw.eq.1)then

C If file exists read it and enter editing facility after
C check to see if it is local or standard or absolute path.
        if(XST)then
          call findwhichdbpath('cfc',lltmp,ier)
          goto 44
        else
          call edisp(iuout,
     &    'No file to browse/edit. Please use another option.')
          LCFCDB=lprev
          goto 60
        endif

      elseif(isw.eq.2)then

C Allow user to select from the distribution databases folder for an
C CFClayers file. If moddb was set to true then proceed to edit
C the materials.
        numhelp=nbhelp
        lltmp='  '
        call usecommondbsfile('cfc',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.6)then

! C User requested the creation of a new CFC layers db file. Set up minimal
! C set of categories and items, write this to a scratch file and
! C then scan that into material arrays and create a new db and offer editing.
!         lr=lnblnk(cfgroot)
!         LTMP=' '
!         LLTMP=' '
!         if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
!           write(LTMP,'(2a)') cfgroot(1:lr),'.cfcdb'
!         else
!           write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
!      &      cfgroot(1:lr),'.cfcdb'
!         endif
!         clkok=.false.
!  62     clkok=.false.
!         CALL EASKSCNCL(ltmp,'Name for new CFC layers properties file',
!      &    'Confirm:','cancel',clkok,92,DFCON,'new CFC layers',
!      &    IER,nbhelp)
!         if(clkok)then
!           LCFCDB=lprev  ! user canceled, restore initial name
!           return
!         endif
!         if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
!           write(LCFCDB,'(a)') ltmp(1:lnblnk(ltmp))
!           ipathcfc=1  ! this is a local materials.
!         else
!           goto 62
!         endif
! 
!         CALL ERPFREE(IFMAT,ISTAT)
!         CFCDBOK=.TRUE.
!         moddb=.true.
!         modcon=.true.
! 
! C Write a scratch file which can be re-scanned into common blocks.
! C CFC layers db. Include typical categories with one item in each.
! C The scratch file is in the format of an older ASCII materials
! C file which will then be scanned into materials arrays.
! 
! C << Need to update this scratch file to use a newer format
! C << of ascii Materials db or just assign the data arrays
! C << directly.
! 
!         lr=lnblnk(cfgroot)
!         LLTMPFL=' '
!         if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
!           write(LLTMPFL,'(2a)') cfgroot(1:lr),'.scratch'
!         else
!           write(LLTMPFL,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
!      &      cfgroot(1:lr),'.scratch'
!         endif
!         IAF=IFIL+1
!         CALL EFOPSEQ(IAF,LLTMPFL,3,IER)
!         write(IAF,'(2a)',IOSTAT=ios,ERR=13)
!      &    '# Materials db defined in ',LFMAT(1:lnblnk(LFMAT))
!         NCLASS=15
!         write(IAF,'(i5,a)',IOSTAT=ios,ERR=13) NCLASS,
!      &    '#  number of classifications'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '# class|no elements|description'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) '  1   1   Brick'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) '  1   Paviour Brick'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '  0.960, 2000.000, 840.000, 0.930, 0.700, 12.000'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '# class|no elements|description'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) '  2   1   Concrete'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 21   Light mix conc'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '  0.380, 1200.000, 653.000, 0.900, 0.650, 6.000'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '# class|no elements|description'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) '  3   1   Metal'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 41   Copper   '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '  200.000, 8900.000, 418.000, 0.720, 0.650, 19200.000'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '# class|no elements|description'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) '  4   1   Wood'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 61   Wood block   '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '  0.160, 800.000, 2093.000, 0.900, 0.650, 11.000'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '# class|no elements|description'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) '  5   1   Stone'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 81   Sandstone   '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '  1.830, 2200.000, 712.000, 0.900, 0.600, 29.000'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '# class|no elements|description'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) '  6   1   Plaster'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 101  Dense plaster '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '  0.500, 1300.000, 1000.000, 0.910, 0.500, 11.000  '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '# class|no elements|description'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13)
!      &    '  7   1   Screeds and renders'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13)
!      &    ' 121  Ltwt concrete screed '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '  0.410, 1200.000, 840.000, 0.900, 0.800, 30.000'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '# class|no elements|description'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) '  8   1   Tiles'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 141  Clay tile '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    ' 0.850, 1900.000, 837.000, 0.900, 0.600, 52.000'  
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '# class|no elements|description'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13)
!      &    '  9   1   Asphalt and bitumen'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 161  Bitumen felt '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    ' 0.500, 1700.000, 1000.000, 0.900, 0.900, 1000.000'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '# class|no elements|description'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) '  10   1  Fabric '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 181  wool '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '  0.060, 198.000, 1360.000, 0.900, 0.600, 10.000'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '# class|no elements|description'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) '  11   1  Insulation '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 201  Fibreboard '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '  0.060, 300.000, 1000.000, 0.900, 0.500, 13.000'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '# class|no elements|description'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) '  12   1  Carpet '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 221  Wilton '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    ' 0.060, 186.000, 1360.000, 0.900, 0.600, 10.000' 
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '# class|no elements|description'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) '  13   1  Glass '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 241  Glass block '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '  0.700, 3500.000, 837.000, 0.830, 0.050, 19200.000'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '# class|no elements|description'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) '  14   1  Earth '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 261  Infusorial (9% mc)'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    ' 0.090, 480.000, 180.000, 0.900, 0.850, 5.000 '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '# class|no elements|description'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) '  15   1  Plastic '
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) ' 281   Plastic tile'
!         write(IAF,'(a)',IOSTAT=ios,ERR=13) 
!      &    '  0.500, 1050.000, 837.000, 0.900, 0.400, 1000.000 '
! 
! C After creating the new file close the file unit.
!         CALL ERPFREE(IAF,ISTAT)
!         call pausems(400)
! 
! C Now scan the scratch file into material arrays.
!         ier=0
!         call rlegacymat(IAF,LLTMPFL,ier)
!         if(ier.ne.0)then
!           call usrmsg('The initial materials data could not',
!      &                'be scanned into memory!','W')
!         else
!           call usrmsg('An initial set of material categories has been',
!      &                'setup and you can modify as required.','-')
!         endif
! 
! C And now write out the file with the materials arrays. mkascimat
! C expects to be passed the expanded file name.
!         CALL ERPFREE(IAF,ISTAT)
!         CALL ERPFREE(IFMUL,ISTAT)  ! in case other unit is still open
!         LLTMPFL=' '
!         if(ipathmat.eq.0.or.ipathmat.eq.1)then
!           write(LLTMPFL,'(a)') LFMAT(1:lnblnk(LFMAT))
!         elseif(ipathmat.eq.2)then
!           lndbp=lnblnk(standarddbpath)
!           write(LLTMPFL,'(3a)') standarddbpath(1:lndbp),fs,
!      &      LFMAT(1:lnblnk(LFMAT))
!         endif
!         CALL mkascimat(IAF,LLTMPFL,IER)
! 
!         dok=.true.
!         CALL ASKOK(' ','Browse or edit new file?',OK,dok,nbhelp)
!         if(OK)then
!           moddb=.true.
!           goto 44
!         endif

      elseif(isw.eq.4)then

C Copy standard file to model folder. Suggest a local
C file name based on the project root name. Update common to
C reflect it is local.
        allowbrowse=.false.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('cfc',copydef,allowbrowse,
     &  numhelp,lltmp,istat,moddb)
     
        if(istat.eq.1)then
          LCFCDB=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44
        endif

      elseif(isw.eq.3)then

C Allow user to select from the model ../dbs folder for a
C CFC layers file. If moddb was set to true then proceed to edit
C the CFC layers.
        numhelp=nbhelp
        lltmp='  '
        call usemodeldbsfile('cfc',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.5)then

C Create a menu list of files in the distribution databases folder
C If user requests, copy one of those files to the model folder.
        copydef=.false.
        allowbrowse=.true.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('cfc',copydef,allowbrowse,
     &  numhelp,lltmp,istat,moddb)
     
        if(istat.eq.1)then
          LCFCDB=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44
        endif

      endif

C Test the selected or copied file. If a problem loop back
C otherwise edit its contents.
   44 continue

      ier=0

C Scan CFC layers file into commons.
      call scancfcdb(ier)
      if(ier.ne.0)then
        call usrmsg('Readable CFC layers file was not found',
     &              'or file was corrupt','W')
        CFCDBOK=.FALSE.
      else
        CFCDBOK=.TRUE.
      endif

      moddb=.true.
      modcon=.true.
      chgdb=.false.

      call tstamp('>','PRJ: enter CFC layers db')
      call eclose(cfcver,1.1,0.001,closemat1)
      if(closemat1)then

C Offer display and editing of CFC layer common blocks. If chgdb is
C returned as true then write out the ASCII file.  
        CALL ELISTCFC(iw,chgdb,'M',imatarryindex,IER)

      else

C If the CFC layer arrays were not filled warn the user.
        call usrmsg('CFC layer arrays were not filled',
     &              'properly or file was corrupt','W')
        goto 60
      endif

C Write ASCII version from the common blocks.
      if(chgdb)then
        IAF=IFIL+1
        CALL ERPFREE(IFMUL,ISTAT)  ! in case other unit is still open
        CALL mkascicfc(IAF,LCFCDB,IER)
      endif
      modcon=chgdb

      return

C Error messages.
C   13 CALL USRMSG(' Write error to ascii CFC layers db ',LLTMPFL,'W')
C      IER=1
C      return
      end


C ********** EDDBPCDB
C EDDBPCDB Changes the plant template current cfg file references.
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
      common/appw/iappw,iappx,iappy
      integer iappw,iappx,iappy
      integer childterminal  ! picks up mmod from starting of prj
      common/childt/childterminal
      common/rpath/path
      common/deflt4/dinstpath

      CHARACTER DFILE*72,LLASCI*144
      character LTMP*72,lltmp*144,lprev*144,lltmp2*144
      character longtfile*144,longtafile*144,longtmp*144
      CHARACTER DOIT*300,fs*1
      CHARACTER OUTS*124,APP*4
C      CHARACTER outs248*248
      LOGICAL CLKOK,OK,COPYDEF,MODDB,XST,DOK,concat
      character dinstpath*60,path*72,tmode*8
      character dirpath*72
      integer iappwpc ! application %

C Local strings for user selections dependent on graphic library.
      CHARACTER ZPDESC*25
      integer lndbp   ! for length of standard path
      integer ier
      logical unixok  ! to check for path file separators
      integer iglib  ! for detecting GTK or X11
      logical allowbrowse ! to be done

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

C Use the event profiles unit for testing plant db.
      if(ipcdb.eq.0)ipcdb=iprodb
      CALL ERPFREE(ipcdb,ISTAT)

C If local or absolute path call addpath otherwise if a standard
C file then set longtfile equal to LLTMP.
      if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
        call addpath(LLTMP,longtfile,concat)
      elseif(ipathpcdb.eq.2)then
        longtfile=LLTMP
      endif
      ltf=max(1,LNBLNK(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up options depending on where the file is.
      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Plant components',
     &    ' ','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',
     &    'f components >> ascii export','g ascii >> components',
     &    'h create new plant components',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If standard file then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Plant components',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',
     &    'f components >> ascii export','g ascii >> components',
     &    'h create new plant components',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5)copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If standard file then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Plant components <std>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',
     &    'f components >> ascii export','g ascii >> components',
     &    'h create new plant components',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5)copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default file or the current file.
        idno=1
        isw=0
        call MENUATOL('  ','Plant components <mod>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',
     &    'f components >> ascii export','g ascii >> components',
     &    'h create new plant components',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5)copydef=.false.
      endif

C << Original menu structure.
C        call MENUATOL('  ','Plant components <mod>',
C     &    'a browse/edit','b select file from list',
C     &    'c create new components','d copy standard components',
C     &    'e components >> ascii export','f ascii >> components',
C     &    optg,opth,opti,' ',' ',' ',isw,idno,nbhelp)

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

C Allow user to select from the distribution databases folder for a
C plant template. If moddb was set to true then proceed to edit
C the plant templates. Note: lltmp2 will be returned without the
C path so pre-pend this so that the call to efopran will work.
C This pattern is only required because the plant template file
C is binary (other select from list uses do not seem to require
C this pre-pend).
        numhelp=nbhelp
        lltmp2='  '
        call usecommondbsfile('pdb',numhelp,lltmp2,istat,moddb)
        if(istat.eq.0.and.moddb)then
          ipathpcdb=2  ! remember that it is a common file
          write(lltmp,'(5a)') dinstpath(1:lnblnk(dinstpath)),
     &      fs,'databases',fs,lltmp2(1:lnblnk(lltmp2))
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.8)then

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
        CALL EASKSCNCL(ltmp,'New plant component templates file.',
     &    'Confirm:','cancel',clkok,72,DPCDB,'plant components',
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

C Create a minimal plant templates (code similar to pdb.F).
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
        CALL ASKOK(outs,' Browse or edit new file?',
     &    OK,dok,nbhelp)
        if(OK)then
          moddb=.true.
          goto 44
        endif

      elseif(isw.eq.4)then

C Copy default file to project folder. Suggest a local
C file name based on the project root name.  Suggest a local
C file name based on the project root name. Update common to
C reflect it is local.   
        allowbrowse=.false.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('pdb',copydef,allowbrowse,
     &    numhelp,lltmp,istat,moddb)
     
        if(istat.eq.1)then
          LPCDB=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44
        endif
       
      elseif(isw.eq.6)then

C Components >> ASCII. If current components are
C in standard folder start ASCII name in users home folder.
C Confirm suggested name and then write out asci file.
        DFILE='./plant_templates.a'
        if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
          write(LLASCI,'(2a)') LPCDB(1:lnblnk(LPCDB)),'.a'
        elseif(ipathpcdb.eq.2)then
          write(LLASCI,'(4a)') path(1:lnblnk(path)),fs,
     &      LPCDB(1:lnblnk(LPCDB)),'.a'
        endif
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(LLASCI,'Export ASCII components file','Confirm:',
     &      96,DFILE,'asci plant comp file name',IER,nbhelp)
        elseif(iglib.eq.2)then
          CALL EASKF(LLASCI,'Export ASCII components file','Confirm:',
     &      144,DFILE,'asci plant comp file name',IER,nbhelp)
        else
          CALL EASKF(LLASCI,'Export ASCII components file','Confirm:',
     &      96,DFILE,'asci plant comp file name',IER,nbhelp)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

C Convert both current binary and asci file names into full paths prior
C to asking pdb to do the conversion.
        if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
          lltmp=LPCDB  ! use as is
        elseif(ipathpcdb.eq.2)then
          write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &      lpcdb(1:lnblnk(lpcdb))  ! prepend db folder path
        endif
        if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
          call addpath(lltmp,longtfile,concat)
        elseif(ipathpcdb.eq.2)then
          longtfile=lltmp
        endif
        call addpath(LLASCI,longtafile,concat)

C Setup the command line for pdb.
        doit = ' '
        write(doit,'(4a)') 'pdb -mode text -file ',
     &    longtfile(1:lnblnk(longtfile)),' -act bin2asci ',
     &    longtafile(1:lnblnk(longtafile))

C Debug.
C        write(6,*) doit
        call runit(doit,tmode)

        call edisp(iuout,'  ')
        call edisp(iuout,'The conversion resulted in a new file:')
        call edisp(iuout,longtafile)
        goto 60  ! jump to redisplay menu.
       
      elseif(isw.eq.7)then

C Components: ASCII >> BINARY
C Confirm suggested name and then write out binary file.
        if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
          write(LLASCI,'(2a)') LPCDB(1:lnblnk(LPCDB)),'.a'
        elseif(ipathpcdb.eq.2)then
          write(LLASCI,'(4a)') path(1:lnblnk(path)),fs,
     &      LPCDB(1:lnblnk(LPCDB)),'.a'
        endif
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(LLASCI,'ASCII (source) file','Confirm:',
     &      96,DFILE,'asci plant comp file name',IER,nbhelp)
        elseif(iglib.eq.2)then
          CALL EASKF(LLASCI,'ASCII (source) file','Confirm:',
     &      144,DFILE,'asci plant comp file name',IER,nbhelp)
        else
          CALL EASKF(LLASCI,'ASCII (source) file','Confirm:',
     &      96,DFILE,'asci plant comp file name',IER,nbhelp)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

        longtmp=LPCDB
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(longtmp,'New plant templates file','Confirm:',
     &      96,DPCDB,'plant file name',IER,nbhelp)
        elseif(iglib.eq.2)then
          CALL EASKF(longtmp,'New plant templates file','Confirm:',
     &      144,DPCDB,'plant file name',IER,nbhelp)
        else
          CALL EASKF(longtmp,'New plant templates file','Confirm:',
     &      96,DPCDB,'plant file name',IER,nbhelp)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

C Convert both binary and asci file names into full paths prior
C to asking pdb to do the conversion.
        if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
          call addpath(longtmp,longtfile,concat)
        elseif(ipathpcdb.eq.2)then
          longtfile=longtmp
        endif
        call addpath(LLASCI,longtafile,concat)

C Setup the command line for pdb.
        doit = ' '
        write(doit,'(4a)') 'pdb -mode text -file ',
     &    longtfile(1:lnblnk(longtfile)),' -act asci2bin ',
     &    longtafile(1:lnblnk(longtafile))

C Debug.
C        write(6,*) doit
        call runit(doit,tmode)

        call edisp(iuout,'  ')
        call edisp(iuout,'The conversion resulted in a new file:')
        call edisp(iuout,longtfile)
        goto 60  ! jump to re-display the menu.
       
      elseif(isw.eq.3)then

C Allow user to select from the model ../dbs folder for an
C plant template file. If moddb was set to true then proceed 
C to pdb
        numhelp=nbhelp
        lltmp='  '
        call usemodeldbsfile('pdb',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.5)then

C Create a menu list of files in the distribution databases folder
C If user requests, copy one of those files to the model folder.
        copydef=.false.
        allowbrowse=.true.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('pdb',copydef,allowbrowse,
     &    numhelp,lltmp,istat,moddb)
     
        if(istat.eq.1)then
          LPCDB=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44
        endif

      endif

C Test the selected or copied file. If a problem loop back
C otherwise edit the data. Pay attention to the implied path. 
   44 continue
      IER=0
      if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
        call addpath(lltmp,longtfile,concat)
      elseif(ipathpcdb.eq.2)then
        longtfile=lltmp
      endif

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
        call terminalmode(childterminal,tmode)
        if(iappw.eq.630)then
          iappwpc=100
        else
          iappwpc=nint(100.0*(real(iappw)/630.0))  ! reconstitute %
        endif
        if(iappwpc.gt.0.and.iappwpc.le.200)then
          write(doit,'(3a,3i4,3a)') 'pdb -mode ',tmode,
     &      ' -s ',iappwpc,iappx+10,iappy+40,' -file ',
     &      longtfile(1:lnblnk(longtfile)),' &'
        else
          write(doit,'(5a)') 'pdb -mode ',tmode,
     &      ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),
     &      ' &'
        endif
        call runit(doit,tmode)
        goto 60  ! re-display menu
      else
        call usrmsg('Error opening plant templates. Please use one',
     &              'of the other options.','W')
        goto 60  ! re-display menu
      endif

      return
      end

C ********** EDDBPROFILE
C EDDBPROFILE Changes the events profiles current cfg file references.
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
C      common/rpath/path
C      character path*72
      common/deflt4/dinstpath

      CHARACTER LTMP*72,lltmp*144,lprev*144
      character LLASCI*144
      character longtfile*144
      CHARACTER fs*1,APP*4
      CHARACTER OUTS*124
      CHARACTER OUTS248*248
      LOGICAL CLKOK,OK,COPYDEF,MODDB,XST,DOK,concat
      character dinstpath*60,dirpath*72

C Local strings for user selections dependent on graphic library.
      character optf*28
      logical unixok  ! to check for path file separators
      integer iglib  ! for detecting GTK or X11
      logical allowbrowse  ! future facility

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
      write(outs248,*) 'Current profiles ',lprev(1:lnblnk(lprev))
      call edisp248(iuout,outs248,100)

  60  continue

C Generate file name lltmp taking into account ipathprodb for use later.
      if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
        lltmp=LPRFDB  ! use as is
      elseif(ipathprodb.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    LPRFDB(1:lnblnk(LPRFDB))  ! prepend db folder path
      endif

      helptopic='events_db_manage'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL ERPFREE(IPRODB,ISTAT)

C If local or absolute path call addpath otherwise if a standard
C file then set longtfile equal to LLTMP.
      if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
        call addpath(LLTMP,longtfile,concat)
      elseif(ipathprodb.eq.2)then
        longtfile=LLTMP
      endif
      ltf=max(1,LNBLNK(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up additional options depending on graphics lib. The second
C option (list select) shows files in the distribution databases
C folder. If using text or X11 option e edits the string while 
C for GTK there are options to browse for files in ../dbs or the
C esp-r distribution.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        optf='f not applicable   '
      elseif(iglib.eq.2)then
        optf='f make copy of another file'
      endif

      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices:
        idno=2
        isw=0
        call MENUATOL('  ','Event profiles',
     &    ' ','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new event profile',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If standard file then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Event profiles',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new event profile',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If standard file then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Event profiles <std>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new event profile',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default file or the current database.
        idno=1
        isw=0
        call MENUATOL('  ','Event profiles <mod>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new event profile',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
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

C Allow user to select from the distribution databases folder for an
C events profiles file. If moddb was set to true then proceed to edit
C the profiles.
        numhelp=nbhelp
        lltmp='  '
        call usecommondbsfile('pro',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.7)then

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

C DPRFDB assumed to point to ascii.
        CALL EASKSCNCL(ltmp,
     &    'New event profiles hold % of sensible & latent gains',
     &    'Confirm:','cancel',clkok,72,DPRFDB,'event profiles',
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

C Create a minimal event profile in common and write to ascii file.
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

C The events file is ASCII version.
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
          write(outs,'(3a)') 'Event profiles ',LPRFDB(1:lnblnk(LPRFDB)),
     &     ' created.'
        else
          write(outs,'(3a)') 'Event profiles ',LPRFDB(1:100),
     &     '... created.'
        endif
        dok=.true.
        CALL ASKOK(outs,' Browse or edit file?',OK,dok,nbhelp)
        if(OK)then
          moddb=.true.
          goto 44
        endif

      elseif(isw.eq.4.or.isw.eq.6)then

C Copy a standard file to project dbs folder. If iws is 4 then the
C user has asked for the default file. If isw is 7 then the
C user wishes to browse for the source file.  Suggest a local
C file name based on the project root name. Update common to
C reflect it is local.   
        allowbrowse=.false.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('pro',copydef,allowbrowse,
     &    numhelp,lltmp,istat,moddb)
     
        if(istat.eq.1)then
          LPRFDB=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44
        endif

      elseif(isw.eq.3)then

C Allow user to select from the model ../dbs folder for an
C profiles file. If moddb was set to true then proceed to edit
C the profiles.
        numhelp=nbhelp
        lltmp='  '
        call usemodeldbsfile('pro',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.5)then

C Create a menu list of files in the distribution databases folder
C and copy that file into the model.
        copydef=.false.
        allowbrowse=.true.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('pro',copydef,allowbrowse,
     &    numhelp,lltmp,istat,moddb)
     
        if(istat.eq.1)then
          LPRFDB=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44
        endif

      endif

C Test the selected or copied file. If a problem restore
C the previous file name and loop back, otherwise edit the
C data.
   44 continue

C Debug.
C      write(outs248,'(2a)') 'Accessing event profile: ',
C     &  LPRFDB(1:lnblnk(LPRFDB))
C      call edisp248(iuout,outs248,100)
      IER=0

C Scan ASCII file (with full path) to fill common blocks.
      lndbp=lnblnk(standarddbpath)
      if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
        lltmp=LPRFDB  ! use as is
      elseif(ipathprodb.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    LPRFDB(1:lnblnk(LPRFDB))  ! prepend db folder path
      endif
      CALL ERPFREE(IPRODB,ISTAT)
      CALL USRMSG('opened profiles db...',' ','-')
      ier=0   ! reset ier before TRIPROCOM call
      call TRIPROCOM(IPRODB,lltmp,IER)
      if(ier.ne.0)then
        call usrmsg('Error opening the profiles db. Please use one',
     &              'of the other options.','W')
        LPRFDB=lprev
        goto 60
      else
        CALL ERPFREE(IPRODB,ISTAT)
        CALL USRMSG('opened profiles db...ok',' ','-')

C Call casfmk for the editing/browsing interface.
        call casfmk(ier)
        goto 60
      endif
      return

      end

C ********** EDDBMLCON
C EDDBMLCON Changes the constructions current cfg file references.
C moddb is from version manager
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.

C If we are pointing to a file in the common folder then we should
C warn the user when attempting to update that file.

      SUBROUTINE EDDBMLCON(moddb,modcon,APP)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "geometry.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
C      common/rpath/path
C      character path*72

C Default esp-r distribution path.
      common/deflt4/dinstpath
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      integer ifil
      COMMON/FILEP/IFIL

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical concat
      logical moddb,modcon,modmlc,copydef,clkok

      CHARACTER LTMP*72,lltmp*144,lprev*144
      character longtfile*144
      CHARACTER fs*1,APP*4
C      CHARACTER outs248*248
      LOGICAL OK,XST,DOK
      character dinstpath*60,dirpath*72

C Local strings for user selections dependent on graphic library.
      character optf*28
      integer lndbp   ! for length of standard path
      logical unixok  ! to check for path file separators
      logical allowbrowse ! future option
      integer iglib  ! for detecting GTK or X11
      integer icomp,icn1,ii,i  ! for use in updataing ssmlcindex

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
C data file then set longtfile equal to LLTMP.
      if(ipathmul.eq.0.or.ipathmul.eq.1)then
        call addpath(LLTMP,longtfile,concat)
      else
        longtfile=LLTMP
      endif
      ltf=max(1,LNBLNK(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up additional options depending on graphics lib. The second
C option (list select) shows files in the distribution databases
C folder. If using text or X11 option e edits the string while 
C for GTK there are options to browse for files in ../dbs or the
C esp-r distribution.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        optf='f not applicable   '
      elseif(iglib.eq.2)then
        optf='f make copy of another file'
      endif

      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Constructions',
     &    ' ','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new constructions',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If standard file then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Constructions',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new constructions',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If standard file then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Constructions <std>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new constructions',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default file or the current file.
        idno=1
        isw=0
        call MENUATOL('  ','Constructions <mod>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new constructions',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      endif

C Act on the users choice. If no choice, return to calling menu.

C Note the order in the menu changed but the blocks of code
C have not yet been re-arranged to reflect this only the 
C elseif isw index has been changed.

      if(isw.eq.0)then
        return   ! user requested to exit
      elseif(isw.eq.1)then

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

C Allow user to select from the distribution databases folder for an
C constructions file. If moddb was set to true then proceed to edit
C the constructions.
        numhelp=nbhelp
        lltmp='  '
        call usecommondbsfile('mlc',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44  ! process the file
        else
          goto 60
        endif

      elseif(isw.eq.7)then

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
        CALL EASKSCNCL(ltmp,
     &    'New constructions file (with minimal contents).',
     &    'Confirm:','cancel',clkok,72,'construction.db',
     &    'constructions',IER,nbhelp)
        if(clkok)then
          LFMUL=lprev
          return
        endif
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          write(LFMUL,'(a)') ltmp(1:lnblnk(ltmp)) ! local so direct asignment
        else
          goto 62
        endif

C Create a minimal constructions.
C Set the value of ipathmul to signal local file.

C << alternative to IPR needed >>
        ipathmul=1  

        NMLC=1
        DTHK(1,1)=0.100
        IPR(1,1)=1
        LAYERS(1)=1
        DRAIR(1,1,1)=0.0; DRAIR(1,1,2)=0.0; DRAIR(1,1,3)=0.0
        MLDBOK=.TRUE.
        moddb=.true.
        CALL EMKAMLD(iuout,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG(' ',' Unable to create file!','W')
          RETURN
        ENDIF
        ISEL=1
        CALL ERMLDB(0,ITRU,IER)
        IF(IER.NE.0)goto 60
        MLDBOK=.TRUE.

C Scan the new file and use the editing facility.
        dok=.true.
        CALL ASKOK('  ','Browse or edit new file?',OK,dok,nbhelp)
        if(OK)then
          goto 44  ! process the file
        endif

      elseif(isw.eq.4.or.isw.eq.6)then

C Copy the default common file to model dbs folder. If iws is 4 then the
C user has asked for the default file. If isw is 6 add user dialog for
C which file. Suggest a local file name based on the project root name. 
C Update common to reflect it is local.
        allowbrowse=.false.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('mlc',copydef,allowbrowse,
     &  numhelp,lltmp,istat,moddb)
     
        if(istat.eq.1)then
          LFMUL=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44  ! process the file
        endif

      elseif(isw.eq.3)then

C Allow user to select from the model ../dbs folder for an
C constructions file. If moddb was set to true then proceed to edit
C the constructions.
        numhelp=nbhelp
        lltmp='  '
        call usemodeldbsfile('mlc',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44  ! process the file
        else
          goto 60
        endif

      elseif(isw.eq.5)then

C Create a menu list of files in the distribution databases folder
C and copy that file into the model.
        copydef=.false.
        allowbrowse=.true.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('mlc',copydef,allowbrowse,
     &    numhelp,lltmp,istat,moddb)
        if(istat.eq.1)then
          LFMUL=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44  ! process the file
        endif
      endif

C Test the selected or copied file. If a problem loop back
C otherwise edit the contents.
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
C file is for the wrong machine type. Guess the name of
C the ascii version and if it exists, offer to convert it.
        call usrmsg(
     &    'The Materials db probably needs to be',
     &    'restored from an archived ASCII version.','W')
        return
      endif
      MLDBOK=.TRUE.

C If a changed MLC need to re-scan for matching MLC array index
C for surfaces to support subsequent rebuilding of the zone
C construction files.
      do 30 ICOMP=1,NCOMP
        call georead(IFIL+1,LGEOM(ICOMP),ICOMP,1,iuout,ier)

C Find the index of the MLC which matches each surface.
        DO 9994 I=1,NZSUR(icomp)
          icn1=izstocn(icomp,i)
          ssmlcindex(icn1)=0  ! assume no matching MLC          
          do 5 ii=1,nmlc
            if(SSMLCN(icn1)(1:12).eq.mlcname(ii)(1:12)) then
              ssmlcindex(icn1)=ii   ! remember MLC index  
            endif
  5       continue
 9994   continue
 30   continue

      call tstamp('>','PRJ: enter construction db')
      CALL EDMLDB(ITRC,modmlc,IER,ISEL)
      if(IER.EQ.1)then
        CALL USRMSG(' ','Problem accessing constructions db','W')
        LFMUL=lprev
        MLDBOK=.false.
        goto 60
      endif
      moddb=modmlc   ! remember if changes made
      modcon=modmlc
      goto 60        ! re-display the constructions menu

      end

C ********** EDDBOPT
C EDDBOPT Changes the optical properties current cfg file references.
C moddb is from version manager
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.
C This facility offers browsing for GTK version.
      SUBROUTINE EDDBOPT(moddb,APP)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
      common/OUTIN/IUOUT,IUIN
C      common/rpath/path
C      character path*72
      common/deflt4/dinstpath
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      CHARACTER LTMP*72,lltmp*144,lprev*144
      character longtfile*144
      CHARACTER fs*1,APP*4
C      CHARACTER outs248*248
      character GTYPE*12,GDESCR*36,SOPT*12
      LOGICAL CLKOK,OK,COPYDEF,MODDB,XST,DOK
      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,concat,CFCDBOK
      character dinstpath*60,dirpath*72

C Local strings for user selections dependent on graphic library.
      character optf*28
      integer lndbp   ! for length of standard path
      logical unixok  ! to check for path file separators
      logical allowbrowse  ! future facility
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
C data file then set longtfile equal to LLTMP.
      if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
        call addpath(LLTMP,longtfile,concat)
      else
        longtfile=LLTMP
      endif
      ltf=max(1,LNBLNK(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up additional options depending on graphics lib. The second
C option (list select) shows files in the distribution databases
C folder. Another option allows browsing of the model ../dbs folder.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        optf='f not applicable   '
      elseif(iglib.eq.2)then
        optf='f make copy of another file'
      endif

      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Optical properties',
     &    ' ','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new optics',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If standard file then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Optical properties',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new optics',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If standard file then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Optical properties <std>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new optics',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default file or the current file.
        idno=1
        isw=0
        call MENUATOL('  ','Optical properties <mod>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new optics',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      endif

C Act on the users choice. If no choice, return to calling menu.

C Note the order in the menu changed but the blocks of code
C have not yet been re-arranged to reflect this only the 
C elseif isw index has been changed.
      if(isw.eq.0)then
        return   ! user requested to exit
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

C Allow user to select from the distribution databases folder for an
C optics file. If moddb was set to true then proceed to edit
C the optics.
        numhelp=nbhelp
        lltmp='  '
        call usecommondbsfile('opt',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.7)then

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
        CALL EASKSCNCL(ltmp,'New Optical properties file.',
     &    'Confirm:','cancel',clkok,72,'optics.db','optics file',
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

C Create a minimal optical properties (code similar to edoptic.F).
C Set the value of ipathoptdb to signal local file.
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
        VTRN=0.99; SRF=0.01; SAB=0.01; UVAL=9.0
        DG(1)=0.998; DG(2)=0.987; DG(3)=0.986; DG(4)=0.985; DG(5)=0.984
        HG(1)=0.980; HG(2)=0.970; HG(3)=0.960; HG(4)=0.950; HG(5)=0.940
        RF(1)=1.52
        AB(1,1)=0.001; AB(1,2)=0.001; AB(1,3)=0.001
        AB(1,4)=0.001; AB(1,5)=0.001
        CALL EMKOPTD(IOPTDB,GTYPE,GDESCR,IER)
        CALL ERPFREE(IOPTDB,ISTAT)
        OPTKOK=.TRUE.

C Scan the new file and use the editing facility.
        dok=.true.
        CALL ASKOK('  ',' Browse or edit new file?',
     &    OK,dok,nbhelp)
        if(OK)then
          goto 44
        endif

      elseif(isw.eq.4.or.isw.eq.6)then

C Copy the default common file to model dbs folder. If iws is 4 then the
C user has asked for the default file. If isw is 6 then the
C user wishes to browse for the source file. Suggest a local
C file name based on the project root name.  Update common to
C reflect it is local.
        allowbrowse=.false.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('opt',copydef,allowbrowse,
     &  numhelp,lltmp,istat,moddb)
     
        if(istat.eq.1)then
          LOPTDB=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44
        endif

      elseif(isw.eq.3)then

C Allow user to select from the model ../dbs folder for an
C optics file. If moddb was set to true then proceed to edit
C the optics.
        numhelp=nbhelp
        lltmp='  '
        call usemodeldbsfile('opt',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.5)then

C Create a menu list of files in the distribution databases folder
C If user requests, copy one of those files to the model folder.
        copydef=.false.
        allowbrowse=.true.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('opt',copydef,allowbrowse,
     &    numhelp,lltmp,istat,moddb)
        if(istat.eq.1)then
          LOPTDB=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44
        endif
      endif


C Test the selected or copied file. If a problem loop back
C otherwise edit the contents.
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
      goto 60        ! re-display the optics menu

      end


C ********** EDDBPRES
C EDDBPRES Changes the wind pressure coefficients current cfg file references.
C moddb is from version manager
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.
C This facility offers browsing for GTK version and list select
C for all interfaces.
      SUBROUTINE EDDBPRES(moddb,APP)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
C      common/rpath/path
C      character path*72
      common/deflt4/dinstpath

      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      character DEPRE*40
      common/MFLDOC/DEPRE(MPRD)

      CHARACTER LTMP*72,lltmp*144,lprev*144
      character longtfile*144
      CHARACTER fs*1,APP*4
C      CHARACTER outs248*248

      LOGICAL CLKOK,OK,COPYDEF,MODDB,XST,DOK,concat
      character dinstpath*60,dirpath*72
      character OUTSTR*124      ! string buffer for data read
      character t144*144        ! for passing to erprcdb

C Local strings for user selections dependent on graphic library.
      character optf*28
      character lworking*144   ! file name in std folder
      integer lndbp   ! for length of standard path
      logical unixok  ! to check for path file separators
      logical allowbrowse ! future option
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
C data file then set longtfile equal to LLTMP.
      if(ipathapres.eq.0.or.ipathapres.eq.1)then
        call addpath(lltmp,longtfile,concat)
      else
        longtfile=lltmp
      endif
      ltf=max(1,LNBLNK(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up additional options depending on graphics lib. The second
C option (list select) shows files in the distribution databases
C folder. If using text or X11 option e edits the string while 
C for GTK there are options to browse for files in ../dbs or the
C esp-r distribution.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        optf='f not applicable   '
      elseif(iglib.eq.2)then
        optf='f make copy of another file'
      endif

      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Pressure coefficients',
     &    ' ','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new coefficients',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If standard file then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Pressure coefficients',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new coefficients',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If standard file then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Pressure coefficients <std>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new coefficients',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default file or the current file.
        idno=1
        isw=0
        call MENUATOL('  ','Pressure coefficients <mod>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new coefficients',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
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

C Allow user to select from the distribution databases folder for a
C pressure coefficients file. If moddb was set to true then proceed
C to edit the coefficients.
        numhelp=nbhelp
        lltmp='  '
        call usecommondbsfile('prs',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.7)then

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
  62    CALL EASKSCNCL(ltmp,
     &    'New pressure coefficients sets file',
     &    'Confirm:','cancel',clkok,72,DAPRES,
     &    'pressure coefficients',IER,nbhelp)
        if(clkok) return    ! abandon this task
        if(ltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then
          write(lapres,'(a)') ltmp(1:lnblnk(ltmp))
        else
          goto 62
        endif

C Create a minimal pressure coefficients.
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
        CALL ASKOK('  ',' Browse or edit new file?',
     &    OK,dok,nbhelp)
        if(OK)then
          goto 44
        endif

      elseif(isw.eq.4.or.isw.eq.6)then

C Copy a standard file to project dbs folder. If iws is 4 then the
C user has asked for the default file. If isw is 6 then the
C user wishes to browse for the source file. Suggest a local
C file name based on the project root name. Update common to
C reflect it is local.
        allowbrowse=.false.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('prs',copydef,allowbrowse,
     &  numhelp,lltmp,istat,moddb)
     
        if(istat.eq.1)then
          lapres=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44
        endif

      elseif(isw.eq.3)then

C Allow user to select from the model ../dbs folder for an
C pressure coefficients file. If moddb was set to true then proceed
C to edit the coefficients.
        numhelp=nbhelp
        lltmp='  '
        call usemodeldbsfile('prs',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.5)then

C Create a menu list of files in the distribution databases folder
C and copy that file into the model.
        copydef=.false.
        allowbrowse=.true.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('prs',copydef,allowbrowse,
     &  numhelp,lltmp,istat,moddb)
     
        if(istat.eq.1)then
          lapres=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44
        endif

      endif

C Test the selected or copied file. If a problem loop back
C otherwise edit the contents.
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
        call usrmsg('Problem encountered with pressure coef db!',
     &              'Please check ddb location and content.','W')
        GOTO 60
      else
        CALL EDPCDB(IER) 
      endif
      goto 60        ! re-display the pressure coef menu

      end

C ********** EDDBCLM
C EDDBCLM Changes the weather file current cfg file references.
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
      integer childterminal  ! picks up mmod from starting of prj
      common/childt/childterminal
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      common/appw/iappw,iappx,iappy
      integer iappw,iappx,iappy
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      common/rpath/path
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/deflt4/dinstpath

C Calendar.
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER :: icalender,nbcaldays,nbdaytype

      CHARACTER DFILE*72,LLASCI*144,ltmp*96,lguess*144
      character longtfile*144,longtafile*144
      character message*48,longtmp*144
      CHARACTER DOIT*248,ldoit*300
      CHARACTER fs*1,TMODE*8,CLMLOC*30,LCOPY*144
      CHARACTER OUTS*124,APP*6,ext*4,llclmdb*144
      LOGICAL OK,concat,COPYDEF,MODDB,XST,DOK,UNIXOK
      character dinstpath*60,path*72
      character dirpath*72
      character actreturn*1  ! users preference
      CHARACTER*72 NNAME
      dimension ICLM(24,6)
      integer llt  ! length of string buffer
      integer ier
      integer icreport  ! to signal that calendar has been updated.
      integer IDOL,IDAYNUM,IMTHNUM,IDWKNUM,IDTYY  ! for correcting calendar
      logical closelat,closelong,clkok,usercreatednew
      integer iappwpc ! application %

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

C Setup string buffer with distribution weather folder name.
      write(dirpath,'(4a)') dinstpath(1:lnblnk(dinstpath)),
     &  fs,'climate',fs
      ldirpath=lnblnk(dirpath)

C Take into account whether weather file is in standard folder.
C If so llclmdb will be expanded to the full path.
      moddb=.false.
      usercreatednew=.false.  ! dealing with a new weather file
 552  llt=lnblnk(LCLIM)
      lndbp=lnblnk(standardclmpath)
      if(ipathclim.eq.0.or.ipathclim.eq.1)then
        llclmdb=LCLIM
      elseif(ipathclim.eq.2)then
        write(llclmdb,'(3a)') standardclmpath(1:lndbp),fs,
     &    LCLIM(1:lnblnk(LCLIM))
      endif

      helptopic='clm_db_manage'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Just in case we got this far and ICLIM was not instanciated do it now.
      if(ICLIM.eq.0)then
        ICLIM=IFIL
      endif
      CALL ERPFREE(ICLIM,ISTAT)

C Depending on where the weather is located offer different options.
      call FINDFIL(llclmdb,XST)
      IF(.NOT.XST)THEN
        idno=2
        isw=0
        call MENUATOL('  ','Weather',
     &    ' ','b select another from list',
     &    'c select from model ../dbs',
     &    'd create new weather','e copy standard weather',
     &    ' ',' ',' ',' ',' ',' ',' ',isw,idno,nbhelp)
        copydef=.true.
      elseif(llclmdb(1:ldirpath).eq.dirpath(1:ldirpath))then

C If corportate weather then offer the following choices:
        idno=1
        isw=0
        call MENUATOL('  ','Weather <std>',
     &    'a browse/edit','b select another from list',
     &    'c select from model ../dbs',
     &    'd create new weather','e copy it to model',
     &    'f weather >> ascii export','g ascii >> weather import',
     &    'h EPW >> weather import',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        copydef=.false.
      else

C If db exists locally offer the following choices:
        idno=1
        isw=0
        call MENUATOL('  ','Weather <mod>',
     &    'a browse/edit','b select another from list',
     &    'c select from model ../dbs',
     &    'd create new weather','e copy standard weather',
     &    'f weather >> ascii export','g ascii >> weather import',
     &    'h EPW >> weather import',' ',' ',' ',' ',
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
          else
            call usrmsg('Problem encountered with weather!',
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
        call terminalmode(childterminal,tmode)
        if(ipathclim.eq.0.or.ipathclim.eq.1)then
          call addpath(llclmdb,longtfile,concat)
        elseif(ipathclim.eq.2)then
          longtfile=llclmdb
        endif
        if(iappw.eq.630)then
          iappwpc=100
        else
          iappwpc=nint(100.0*(real(iappw)/630.0))  ! reconstitute %
        endif
        if(iappwpc.gt.0.and.iappwpc.le.200)then
          write(doit,'(3a,3i4,2a)') 'clm -mode ',tmode,
     &        ' -s ',iappwpc,iappx+10,iappy+10,' -file ',
     &        longtfile(1:lnblnk(longtfile))
        else
          write(doit,'(4a)') 'clm -mode ',tmode,' -file ',
     &        longtfile(1:lnblnk(longtfile))
        endif
        call runit(doit,tmode)

      elseif(isw.eq.2)then

C Pull up the official list of climate files and if the user selects
C one that exists then assign it. If the user picks USER_DEFINED then
C use a file dialog (X11) or browser (GTK) to go looking for it.
        iuf=IFIL+1
        INQUIRE (FILE=cdblfil,EXIST=XST)
        if(XST)then
          call rdblist(IUF,longtfile,actreturn,ier)
          if(ier.eq.0)then
            if(longtfile(1:12).eq.'USER_DEFINED')then

              write(llclmdb,'(a)') 'newclim'
              write(lguess,'(a)') 'newclim'
              ipathclim=0
 291          llt=lnblnk(lguess)
              iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
              CALL EASKXORGTKF(lguess,'Weather file:','Confirm:',
     &          DCLIM,llclmdb,'climate file name',IER,numhelp)
              IF(llclmdb(1:2).EQ.'  ')GOTO 291
              if(ier.eq.-3)then
                moddb=.false.
                goto 552
              endif
              IER=0

C Cast back to LCLIM.
              llt=lnblnk(llclmdb)
              write(LCLIM,'(a)') llclmdb(1:llt)
              write(longtfile,'(a)') llclmdb(1:llt)
            elseif(longtfile.eq.'UNKNOWN'.or.longtfile(1:2).eq.'  ')then
              call usrmsg('Nothing selected from the list!',' ','W')
              goto 552
            else

C Assign weather file name based on its location via findwhichdbpath.
              call findwhichdbpath('clm',longtfile,ier)
              if(ipathclim.eq.0.or.ipathclim.eq.1)then
                llclmdb=LCLIM
              elseif(ipathclim.eq.2)then
                write(llclmdb,'(3a)') standardclmpath(1:lndbp),fs,
     &            LCLIM(1:lnblnk(LCLIM))
              endif
            endif
          endif
        else
          call usrmsg('No weather list available so using default.',
     &      '(Check with administrator about the `climatelist` file.)',
     &      'W')
          goto 552
        endif

        CALL ERPFREE(ICLIM,ISTAT)
        call FINDFIL(llclmdb,XST)
        IF(XST)THEN

C If file exists and user wants to browse then scan the file and
C use the editing facility. If user does not want to start clm jump
C straight to the code that checks for differences in model and the
C weather file locations.
          if(actreturn(1:1).eq.'u')then
            goto 44    ! start of clm module
          elseif(actreturn(1:1).eq.'s')then
            moddb=.true.
            goto 43    ! skip clm do check for site information
          else
            moddb=.true.
            goto 44    ! start clm module
          endif
        else
          call usrmsg('No weather to browse/edit.',
     &                'Please use one of the other options.','W')
          goto 552
        endif

      elseif(isw.eq.3)then

C Allow user to select from the model ../dbs folder for a
C weather file. If moddb was set to true then proceed.
        numhelp=nbhelp
        llclmdb='  '
        call usemodeldbsfile('clm',numhelp,llclmdb,istat,moddb)
        if(istat.eq.0.and.moddb)then
          CALL ERPFREE(ICLIM,ISTAT)
          call FINDFIL(llclmdb,XST)
          IF(XST)THEN

C If file exists scan the file and use the editing facility.
            goto 44
          else
            call usrmsg('No weather to browse/edit.',
     &                  'Please use one of the other options.','W')
            goto 552
          endif
        else
          goto 552
        endif

      elseif(isw.eq.4)then

C Set-up minimal common block, create a new weather file, populate
C it with zeros and present editing facility.
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(ltmp,'(2a)') cfgroot(1:lr),'.climate'
        else
          write(ltmp,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &        cfgroot(1:lr),'.climate'
        endif

        clkok=.false.
        CALL EASKSCNCL(ltmp,
     &    'Suggested name of the model copy of standard file.',
     &    'Confirm:','cancel',clkok,96,DCLIM,'copied file',IER,numhelp)
        if(clkok)then
          moddb=.false.
          goto 552
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.
        write(longtmp,'(a)') ltmp  ! cast file name to longer buffer

C Create a blank climate file (code similar to clm.F).
        moddb=.true.
        clmloc='new site'
        ID1=1; IM1=1; IT1=1
        ID2=31; IM2=12; IT2=24
        IDS=1; IDF=365
        IYEAR=2006
        CLAT=50.0; CLONG=0.
        IDNGH=0
        IER=0
        call EFOPRAN(ICLIM,longtmp,144,4,IER)

C Loop through days and zero the data to be written.
        DO 101 I=IDS,IDF
          IDD=I
          DO 201 J=1,24
            ICLM(J,1)=0; ICLM(J,2)=0; ICLM(J,3)=0; ICLM(J,4)=0
            ICLM(J,5)=0; ICLM(J,6)=50
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
        CALL ASKOK(' ','Browse/edit new weather data?',OK,dok,nbhelp)
        IF(OK)then

C Assign weather file name based on longtmp location via findwhichdbpath.
          call findwhichdbpath('clm',longtmp,ier)
          if(ipathclim.eq.0.or.ipathclim.eq.1)then
            llclmdb=LCLIM
          elseif(ipathclim.eq.2)then
            write(llclmdb,'(3a)') standardclmpath(1:lndbp),fs,
     &        LCLIM(1:lnblnk(LCLIM))
          endif
          usercreatednew=.true.
          goto 44
        endif

      elseif(isw.eq.5)then

C Copy standard weather to project folder. Suggest a local
C file name based on the project root name. Update common to
C reflect it is local.   
        if(copydef)then
          call edisp(iuout,' ')
          call edisp(iuout,'The source weather is:')
          call edisp(iuout,DCLIM)
          call edisp(iuout,' ')
        else
          call edisp(iuout,' ')
          call edisp(iuout,'The source weather is:')
          call edisp(iuout,llclmdb)
          call edisp(iuout,' ')
        endif
        lr=lnblnk(cfgroot)
        if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
          write(ltmp,'(2a)') cfgroot(1:lr),'.climate'
        else
          write(ltmp,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &        cfgroot(1:lr),'.climate'
        endif

 554    clkok=.false.
        CALL EASKSCNCL(ltmp,
     &    'Suggested name of the model copy of standard file.',
     &    'Confirm:','cancel',clkok,96,DCLIM,'copied file',IER,numhelp)
        if(clkok)then
          moddb=.false.
          goto 552
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.
        write(longtmp,'(a)') ltmp  ! cast file name to longer buffer

        if(longtmp(1:2).ne.'  '.and.longtmp(1:4).ne.'UNKN')then
          LCLIM=longtmp
          ipathclim=1   ! signal that it is a local file.
        else
          goto 554
        endif

        call isunix(unixok)
        IF(longtmp(1:2).NE.'  ')then
          write(LCOPY,'(3a)') path(1:lnblnk(path)),fs,
     &      longtmp(1:lnblnk(longtmp))
          if(copydef)then

C Copy the default weather file (dclim) to the model folder.
            ldoit = ' '
            if(unixok)then
              write(ldoit,'(4a)',IOSTAT=IOS) 'cp ',
     &          DCLIM(1:lnblnk(DCLIM)),' ',LCOPY(1:lnblnk(LCOPY))
            else
              message='getting climate file from '
              call dblongdoscopy(DCLIM,lcopy,message,ldoit,ier)
            endif
            call runit(ldoit,'-')
            llclmdb=longtmp
            LCLIM=longtmp
          else

C Copy the current common weather file to the model folder.
            ldoit = ' '
            if(unixok)then
              write(ldoit,'(4a)',IOSTAT=IOS) 'cp ',
     &          llclmdb(1:lnblnk(llclmdb)),' ',LCOPY(1:lnblnk(LCOPY))
            else

C Similar logic but using LCLIM.
              message='getting weather file from '
              call dblongdoscopy(llclmdb,lcopy,message,ldoit,ier)
            endif
            call runit(ldoit,'-')

C Assign weather file name to pass to the clm application.
            llclmdb=LCLIM
          endif
          moddb=.true.
          CALL ASKOK(' ','Browse/edit copied climate data?',
     &      OK,dok,nbhelp)
          if(ok)then
            goto 44
          endif
        endif

      elseif(isw.eq.6)then

C Weather >> ascii export.  If current weather is in standard
C folder or model folder put it in the model ../dbs folder.
        DFILE='./climate.a'
        if(ipathclim.eq.0.or.ipathclim.eq.1)then
          write(LLASCI,'(2a)') LCLIM(1:lnblnk(LCLIM)),'.a'
        elseif(ipathclim.eq.2)then
          if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
            write(LLASCI,'(4a)') path(1:lnblnk(path)),fs,
     &        LCLIM(1:lnblnk(LCLIM)),'.a'
          else
            write(LLASCI,'(6a)') path(1:lnblnk(path)),fs,
     &        dbspth(1:lnblnk(dbspth)),fs,LCLIM(1:lnblnk(LCLIM)),'.a'
          endif
        endif

C Confirm suggested name and then write out asci file. In GTK the full
C path is returned.
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(LLASCI,'Export ASCII weather file','Confirm:',
     &      96,DFILE,'climate file name',IER,nbhelp)
        elseif(iglib.eq.2)then
          CALL EASKF(LLASCI,'Export ASCII weather file','Confirm:',
     &      144,DFILE,'climate file name',IER,nbhelp)
        else
          CALL EASKF(LLASCI,'Export ASCII weather file','Confirm:',
     &      96,DFILE,'climate file name',IER,nbhelp)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

C Convert both binary and asci file names into full paths prior
C to asking clm to do the conversion.
        if(ipathclim.eq.0.or.ipathclim.eq.1)then
          call addpath(llclmdb,longtfile,concat)
        elseif(ipathclim.eq.2)then
          longtfile=llclmdb
        endif
        call addpath(LLASCI,longtafile,concat)

C Debug.
C        write(6,*) 'llasci ',LLASCI(1:lnblnk(LLASCI))
C        write(6,*) 'longtafile ',longtafile(1:lnblnk(longtafile))
        call tstamp('>','PRJ: start clm for conversion')
        doit = ' '
        write(doit,'(4a)') 'clm -mode text -file ',
     &    longtfile(1:lnblnk(longtfile)),' -act bin2asci silent ',
     &    longtafile(1:lnblnk(longtafile))
        call runit(doit,tmode)
        goto 552  ! jump to redisplay menu.

      elseif(isw.eq.7)then

C Climate ascii >> weather import.
C Confirm suggested name and then write out binary file.
        DFILE='./climate.a'
        if(ipathclim.eq.0.or.ipathclim.eq.1)then
          write(LLASCI,'(2a)') LCLIM(1:lnblnk(LCLIM)),'.a'
        elseif(ipathclim.eq.2)then
          write(LLASCI,'(4a)') path(1:lnblnk(path)),fs,
     &      LCLIM(1:lnblnk(LCLIM)),'.a'
        endif
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(LLASCI,'ASCII (source) weather file','Confirm:',
     &      96,DFILE,'climate file name',IER,nbhelp)
        elseif(iglib.eq.2)then
          CALL EASKF(LLASCI,'ASCII (source) weather file','Confirm:',
     &      144,DFILE,'climate file name',IER,nbhelp)
        else
          CALL EASKF(LLASCI,'ASCII (source) weather file','Confirm:',
     &      96,DFILE,'climate file name',IER,nbhelp)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

        longtmp=LCLIM
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(longtmp,'New weather file','Confirm:',
     &      96,DCLIM,'new weather file name',IER,nbhelp)
        elseif(iglib.eq.2)then
          CALL EASKF(longtmp,'New weather file','Confirm:',
     &      144,DCLIM,'new weather file name',IER,nbhelp)
        else
          CALL EASKF(longtmp,'New weather file','Confirm:',
     &      96,DCLIM,'new weather file name',IER,nbhelp)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

C Convert both binary and asci file names into full paths prior
C to asking clm to do the conversion.
        if(ipathclim.eq.0.or.ipathclim.eq.1)then
          call addpath(longtmp,longtfile,concat)
        elseif(ipathclim.eq.2)then
          longtfile=longtmp
        endif
        call addpath(LLASCI,longtafile,concat)
        call tstamp('>','PRJ: start clm for conversion')
        doit = ' '
        write(doit,'(4a)') 'clm -mode text -file ',
     &    longtfile(1:lnblnk(longtfile)),' -act asci2bin silent ',
     &    longtafile(1:lnblnk(longtafile))
        call runit(doit,tmode)

C Assign weather file name based on longtmp location via findwhichdbpath.
        call findwhichdbpath('clm',longtfile,ier)
        if(ipathclim.eq.0.or.ipathclim.eq.1)then
          llclmdb=LCLIM
        elseif(ipathclim.eq.2)then
          write(llclmdb,'(3a)') standardclmpath(1:lndbp),fs,
     &      LCLIM(1:lnblnk(LCLIM))
        endif

C After clm module run there will be a new binary file so fall
C through to the code near line 4700 to check contents.

      elseif(isw.eq.8)then

C EPW >> weather import
C Confirm suggested name and then write out binary file.
        DFILE='./climate.epw'
        if(ipathclim.eq.0.or.ipathclim.eq.1)then
          write(LLASCI,'(2a)') LCLIM(1:lnblnk(LCLIM)),'.epw'
        elseif(ipathclim.eq.2)then
          write(LLASCI,'(4a)') path(1:lnblnk(path)),fs,
     &      LCLIM(1:lnblnk(LCLIM)),'.epw'
        endif
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(LLASCI,'EPW (source) file','Confirm:',
     &      96,DFILE,'climate file name',IER,nbhelp)
        elseif(iglib.eq.2)then
          CALL EASKF(LLASCI,'EPW (source) file','Confirm:',
     &      144,DFILE,'climate file name',IER,nbhelp)
        else
          CALL EASKF(LLASCI,'EPW (source) file','Confirm:',
     &      96,DFILE,'climate file name',IER,nbhelp)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

        longtmp=LCLIM
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(longtmp,'New weather file','Confirm:',
     &      96,DCLIM,'new weather file name',IER,nbhelp)
        elseif(iglib.eq.2)then
          CALL EASKF(longtmp,'New weather file','Confirm:',
     &      144,DCLIM,'new weather file name',IER,nbhelp)
        else
          CALL EASKF(longtmp,'New weather file','Confirm:',
     &      96,DCLIM,'new weather file name',IER,nbhelp)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

C Convert both binary and asci file names into full paths prior
C to asking clm to do the conversion.
        if(ipathclim.eq.0.or.ipathclim.eq.1)then
          call addpath(longtmp,longtfile,concat)
        elseif(ipathclim.eq.2)then
          longtfile=longtmp
        endif
        call addpath(LLASCI,longtafile,concat)
        call tstamp('>','PRJ: start clm for conversion')
        doit = ' '
        write(doit,'(4a)') 'clm -mode text -file ',
     &      longtfile(1:lnblnk(longtfile)),' -act epw2bin silent ',
     &      longtafile(1:lnblnk(longtafile))
        call runit(doit,tmode)

C Assign weather file name based on longtmp location via findwhichdbpath.
        call findwhichdbpath('clm',longtfile,ier)
        if(ipathclim.eq.0.or.ipathclim.eq.1)then
          llclmdb=LCLIM
        elseif(ipathclim.eq.2)then
          write(llclmdb,'(3a)') standardclmpath(1:lndbp),fs,
     &      LCLIM(1:lnblnk(LCLIM))
        endif

C After clm module run there will be a new binary file so fall through.

      endif

C Confirm with user about update Site latitude and longitude, recover
C clat and clong. First check if climate file exists: if so open,
C otherwise create with 0 length.
  43  CALL ERPFREE(ICLIM,ISTAT)
      call EFOPRAN(ICLIM,llclmdb,144,0,IER)
      if(ISTAT.ge.0)then
        CALL ERPFREE(ICLIM,ISTAT)
        call EFOPRAN(ICLIM,llclmdb,144,1,IER)
        IREC=366
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)ICYEAR,ICDNGH
        IREC=368
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLAT,CLONG
        CALL ERPFREE(ICLIM,ISTAT)
      else
        call usrmsg('Unable to open weather file.',llclmdb,'W')
        goto 552  ! jump back to redisplay menu
      endif

C Check if weather site and model site differ.
      call eclose(clat,sitelat,0.01,closelat)
      call eclose(clong,sitelongdif,0.01,closelong)
      if(closelat.and.closelong)then
        continue
      else
        write(outs,'(A,F6.2,A,F6.2,a,i4,a,2F6.2,a)')
     &    'The climate lat is ',clat,
     &    ' & long diff is ',clong,' & Year & DN flag ',ICYEAR,
     &    '. The model uses ',sitelat,sitelongdif,'.'
        dok=.false.
        CALL ASKOK(outs,
     &    'Update your model with climate data?',OK,DOK,3)
        if(OK)then
          sitelat=CLAT
          sitelongdif=CLONG
          WRITE(OUTS,'(A,F6.2,A,F6.2,A)')'A latitude of ',sitelat,
     &     ' and longitude diff of ',sitelongdif,
     &     ' is associated with the model.'
          CALL EDISP(IUOUT,OUTS)
          CALL EDISP(IUOUT,' ')
        endif
      ENDIF

C Update year.  This is similar logic as is used in esystem.F ~line 1569
      if(icyear.eq.iyear)then
        continue
      else 
        dok=.false.
        WRITE(OUTS,'(A,I4,a,I4,a)')'The climate year is ',ICYEAR,
     &    '. and the model year is ',IYEAR,'.'      
        CALL ASKOK(OUTS,
     &    'Use climate year in your model? (see help)',OK,DOK,nbhelp)
        IF(OK)THEN
          IYEAR=ICYEAR
          WRITE(OUTS,'(I4,A)')IYEAR,' will be used as simulation year.'
          CALL EDISP(IUOUT,OUTS)
          CALL EDISP(IUOUT,' ')
          if(nbdaytype.eq.0)then

C If no day types found instanciate initial day types. 
            call calenmanage('i',ier)
          elseif(nbdaytype.ge.3)then

C Perform check that calendar day types correspond with simulation year.
C The logic below assumes that the first three day types are weekday
C Saturday Sunday. It loops through each Julian day and if the model
C calendar day type is one of the initial 3 it checks that the model weekday
C is the same as a Julian weekday. Any calendar day types beyond 3 are 
C retained. If no calendar was defined in the model then it should do the
C work silently. (Same logic as in esystem.F). If more than 4 day types
C do not bother with this check.
            ICREPORT=0
            if(nbdaytype.gt.4)then
              continue
            else
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
 123          CONTINUE
            endif
            if(ICREPORT.EQ.1)then
              CALL EDISP(IUOUT,'  ')
              CALL EDISP(IUOUT,
     &          'Simulation year and calendar mismatch rectified')
            endif
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

C Invoke the climate module with llclmdb, it is assumed that the
C weather file exists.
  44  continue
      IER=0
      call EFOPRAN(ICLIM,llclmdb,144,0,IER)
      if(ier.eq.0)then
        call tstamp('>','PRJ: start clm')
        doit = ' '
        call terminalmode(childterminal,tmode)

C Debug.
        write(6,*) childterminal,' ',tmode,ipathclim
        if(ipathclim.eq.0.or.ipathclim.eq.1)then
          call addpath(llclmdb,longtfile,concat)
        elseif(ipathclim.eq.2)then
          longtfile=llclmdb
        endif
        if(iappw.eq.630)then
          iappwpc=100
        else
          iappwpc=nint(100.0*(real(iappw)/630.0))  ! reconstitute %
        endif
        if(iappwpc.gt.0.and.iappwpc.le.200)then
          write(doit,'(3a,3i4,2a)') 'clm -mode ',tmode,
     &      ' -s ',iappwpc,iappx+10,iappy+10,' -file ',
     &      longtfile(1:lnblnk(longtfile))
        else
          write(doit,'(4a)') 'clm -mode ',tmode,' -file ',
     &      longtfile(1:lnblnk(longtfile))
        endif
        call runit(doit,tmode)

C If this was a freshly created weather file check if user wants to
C checks its site data against the model.
        if(usercreatednew)then
          dok=.false.
          CALL ASKOK(' ','Check new site data against model?',
     &      OK,dok,nbhelp)
          if(OK)then
            usercreatednew=.false.
            continue
          else
            usercreatednew=.false.
            goto 552  ! go back and re-display menu
          endif
        endif
        goto 43   ! check if site location or year has changed
      else
         call usrmsg('Problem encountered with Climate db!',
     &               'Please check db location and content.','W')
         goto 552
      endif


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
C EDDBMSC Changes the active components current cfg file references.
C moddb is from version manager
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.
C This facility offers browsing for GTK version and follows the pattern
C of the other db file management facilties except that there is no
C working option for creating a new file and editing of the contents
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

C      common/rpath/path
C      character path*72
      common/deflt4/dinstpath

      CHARACTER lltmp*144,lprev*144
      character longtfile*144,longtfiledos*144
      CHARACTER DOIT*300,fs*1,APP*4
C      CHARACTER outs248*248
      LOGICAL OK,COPYDEF,MODDB,XST,DOK,concat
      character dinstpath*60,dirpath*72
      character tmode*8

C Local strings for user selections dependent on graphic library.
      character optf*28
      integer lndbp   ! for length of standard path
      logical unixok  ! to check for path file separators
      logical allowbrowse  ! future facility
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
C data file then set longtfile equal to LLTMP.
      if(ipathmsc.eq.0.or.ipathmsc.eq.1)then
        call addpath(LLTMP,longtfile,concat)
      else
        longtfile=LLTMP
      endif
      ltf=max(1,LNBLNK(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up additional options depending on graphics lib. The second
C option (list select) shows files in the distribution databases
C folder. If using text or X11 option e edits the string while 
C for GTK there are options to browse for files in ../dbs or the
C esp-r distribution.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        optf='f not applicable   '
      elseif(iglib.eq.2)then
        optf='f make copy of another file'
      endif

      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Active components',
     &    ' ','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new components',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If standard file then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Active components',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new components',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If standard file then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Active components <std>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new components',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default file or the current file.
        idno=1
        isw=0
        call MENUATOL('  ','Active components <mod>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model',optf,
     &    'g create new components',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5.or.isw.eq.6)copydef=.false.
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

C Allow user to select from the distribution databases folder for a
C active components file. If moddb was set to true then proceed
C to edit the components.
        numhelp=nbhelp
        lltmp='  '
        call usecommondbsfile('msc',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.7)then

C There is no create new option for active components.
        call edisp(iuout,'No create new for active components.')
        goto 60

      elseif(isw.eq.4.or.isw.eq.6)then

C Copy a file to project dbs folder. If iws is 4 then the
C user has asked for the default file. If isw is 6 then the
C user wishes to browse for the source file. Suggest a local
C file name based on the project root name. Update common to
C reflect it is local.
        allowbrowse=.false.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('msc',copydef,allowbrowse,
     &  numhelp,lltmp,istat,moddb)
     
        if(istat.eq.1)then
          MCMPDBFL=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44
        endif

      elseif(isw.eq.3)then

C Allow user to select from the model ../dbs folder for an
C actove components file. If moddb was set to true then proceed to edit
C the components.
        numhelp=nbhelp
        lltmp='  '
        call usemodeldbsfile('msc',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.5)then

C Create a menu list of files in the distribution databases folder
C and copy that file into the model.
        copydef=.false.
        allowbrowse=.true.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('msc',copydef,allowbrowse,
     &  numhelp,lltmp,istat,moddb)
     
        if(istat.eq.1)then
          MCMPDBFL=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44
        endif

      endif

C Test the selected or copied file. If a problem loop back
C otherwise edit the contents.
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
      CALL ASKOK('  ','Use this edited file (see help)?',
     &  OK,dok,nbhelp)
      if(OK)then
        moddb=.true.   ! set so user is asked to update cfg file
      else
        MCMPDBFL=lprev
      endif
      goto 60        ! re-display the active components menu

      end


C ********** EDDBMOULD
C EDDBMOULD Changes the mould isopleths current cfg file references.
C moddb is from version manager
C APP is 4 character file name modifier (from version facility)
C     if blank then no file name change implied.
C This facility offers browsing for GTK version and follows the pattern
C of the other db file management facilties except that there is no
C working option for creating a new file and editing of the data
C is via a text editor.
      SUBROUTINE EDDBMOULD(moddb,APP)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/appw/iappw,iappx,iappy
      integer iappw,iappx,iappy
      integer childterminal  ! picks up mmod from starting of prj
      common/childt/childterminal

      common/texted/tedlbl,teditor
      character tedlbl*20,teditor*20

C      common/rpath/path
C      character path*72
      common/deflt4/dinstpath

      CHARACTER lltmp*144,lprev*144
      character longtfile*144,longtfiledos*144
      CHARACTER DOIT*300,fs*1,APP*4
      CHARACTER outs248*248
      LOGICAL OK,COPYDEF,MODDB,XST,DOK,concat
      character dinstpath*60,dirpath*72
      character tmode*8
      integer iappwpc ! application %

C Local strings for user selections dependent on graphic library.
      integer lndbp   ! for length of standard path
      logical allowbrowse  ! future feature
      logical unixok  ! to check for path file separators

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
C data file then set longtfile equal to LLTMP.
      if(ipathmould.eq.0.or.ipathmould.eq.1)then
        call addpath(LLTMP,longtfile,concat)
      else
        longtfile=LLTMP
      endif
      ltf=max(1,LNBLNK(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C Make up options depending on where the current file lives.
      IF(.NOT.XST)THEN

C If db does not exist locally offer limited choices.
        idno=2
        isw=0
        call MENUATOL('  ','Mould isopleths',
     &    ' ','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model','f invoke mould utility',
     &    'g create new isopleths',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5)copydef=.false.
      elseif(longtfile(1:ldirpath).eq.dirpath(1:ldirpath))then

C If standard file then offer the following choices (incl /):
        idno=1
        isw=0
        call MENUATOL('  ','Mould isopleths',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model','f invoke mould utility',
     &    'g create new isopleths',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5)copydef=.false.
      elseif(longtfile(1:ldirpath-1).eq.dirpath(1:ldirpath-1))then

C If standard file then offer the following choices (without /):
        idno=1
        isw=0
        call MENUATOL('  ','Mould isopleths <std>',
     &    'a browse/edit','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model','f invoke mould utility',
     &    'g create new isopleths',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5)copydef=.false.
      else

C If db exists locally toggle whether the user is asked to copy the
C default file or the current file.
        idno=1
        isw=0
        call MENUATOL('  ','Mould isopleths <mod>',
     &    'a browse/edit db','b select file from list',
     &    'c select from model ../dbs',
     &    'd copy default file to model',
     &    'e copy file from common to model','f invoke mould utility',
     &    'g create new isopleths',' ',' ',' ',' ',' ',
     &    isw,idno,nbhelp)
        if(isw.eq.4)copydef=.true.
        if(isw.eq.5)copydef=.false.
      endif

C The earlier layout of menu.
C        call MENUATOL('  ','Mould isopleths <mod>',
C     &    'a browse/edit db','b select file from list',
C     &    'c create new isopleths','d copy standard isopleths',
C     &    'e invoke mould utility',opte,optf,optg,
C     &    ' ',' ',' ',' ',isw,idno,nbhelp)
C        if(isw.eq.5)copydef=.true.
C        if(isw.eq.8)copydef=.false.

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

C Allow user to select from the distribution databases folder for a
C mould isopleths file. If moddb was set to true then proceed
C to edit it.
        numhelp=nbhelp
        lltmp='  '
        call usecommondbsfile('mld',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.7)then

C There is no create new mould isopleths.
        call edisp(iuout,'No create new for mould isopleths.')
        goto 60

      elseif(isw.eq.4)then

C Copy a common data file to project dbs folder. Suggest a local
C file name based on the project root name. Update common to
C reflect it is local.
        allowbrowse=.false.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('mld',copydef,allowbrowse,
     &    numhelp,lltmp,istat,moddb)
     
        if(istat.eq.1)then
          lfmould=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44
        endif

      elseif(isw.eq.6)then

C Start the mould utility.
        doit = ' '
        call terminalmode(childterminal,tmode)
        if(iappw.eq.630)then
          iappwpc=100
        else
          iappwpc=nint(100.0*(real(iappw)/630.0))  ! reconstitute %
        endif
        if(iappwpc.gt.0.and.iappwpc.le.200)then
          write(doit,'(3a,3i4,3a)') 'mld -mode ',tmode,
     &      ' -s ',iappwpc,iappx+10,iappy+40,' -file ',
     &      longtfile(1:lnblnk(longtfile)),' &'
        else
          write(doit,'(5a)') 'mld -mode ',tmode,
     &      ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),
     &      ' &'
        endif
        call runit(doit,tmode)
        goto 60  ! redisplay menu.

      elseif(isw.eq.3)then

C Allow user to select from the model ../dbs folder for an
C mould file. If moddb was set to true then proceed to edit
C the isopleths.
        numhelp=nbhelp
        lltmp='  '
        call usemodeldbsfile('mld',numhelp,lltmp,istat,moddb)
        if(istat.eq.0.and.moddb)then
          goto 44
        else
          goto 60
        endif

      elseif(isw.eq.5)then

C Create a menu list of files in the distribution databases folder
C If user requests, copy one of those files to the model folder.
        copydef=.false.
        allowbrowse=.true.
        numhelp=nbhelp
        lltmp='  '
        call copycommonfile('mld',copydef,allowbrowse,
     &    numhelp,lltmp,istat,moddb)
     
        if(istat.eq.1)then
          lfmould=lprev
          goto 60  ! cancel detected, restore name and redisplay menu.
        elseif(istat.eq.0)then
          goto 44
        endif

      endif

C Test the selected or copied file. If a problem loop back
C otherwise edit the contents.
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
      CALL ASKOK('  ','Use this edited file (see help)?',
     &  OK,dok,nbhelp)
      if(OK)then
        moddb=.true.   ! set so user is asked to update cfg file
      else
        lfmould=lprev
      endif

      return
      end

C ******** copycommonfile
C Copycommonfile is a general facility handling copying files
C from the ESP-r distribution common file store to a model ../dbs
C folder.   If copydef true the user has asked for the default
C file name for the file type.
C If allowbrowse true then the user wishes to browse for the 
C source file (this logic needs to be completed).
C After source is identified Suggest a local (model) file
C name based on the project root name. Lastly do the copy.
C If the default is to be copied confirm local name. Use LTMP for
C the file name as known to esp-r and LCOPY for the system cp call.
C Remember the file to be copied as sourcefile so that subsequent
C code is general rather than specific (for later consolidation).

      subroutine copycommonfile(topic,copydef,allowbrowse,
     &  numhelp,lltmp,istat,moddb)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"

      common/OUTIN/IUOUT,IUIN

      common/rpath/path
      
      integer lnblnk  ! function definition
      
C Passed parameters:
      character topic*3   ! which type of file e.g. mat = materials
                          ! mlc = constructions  opt = optics
                          ! pdb = plant components  prs = pressure coef
                          ! msc = active components  mld = mould isopleths
                          ! pro = event profiles  clm = weather
      logical copydef     ! copy the default file or current file
      logical allowbrowse ! whether browse of common folder allowed
      integer numhelp     ! number of help lines from calling code
      character lltmp*144 ! the returned file name
      integer istat       ! return status index for calling code
      logical moddb       ! set true if new file is change of file

C Local variables:
      character defaultfile*144  ! default file name
C      character sourcemsg*48     ! dialog for source dialog
      character ending*12         ! end of file name
      character gdef*72          ! fall back file name
      logical clkok

      character LTMP*92,lguess*144
      character fs*1,message*48
      character doit*300,LCOPY*144
C      integer iglib  ! for detecting GTK or X11
      integer lr     ! length of model root

      CHARACTER outs248*248
      LOGICAL XST
      character path*72
      character sourcefile*144  ! the file to copy
      character longtfile*144,longtfiledos*144
      
C Local strings for user selections dependent on graphic library.
      character lpath*72,fname*72 ! for use with fdroot
      character sfile*72,snpfile*72

      integer lndbp   ! for length of standard database path
      integer ier
      logical unixok  ! to check for database path file separators
      logical concat

C Clear string buffers.
      ltmp=' '
      lltmp='  '
      lguess='  '

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Setup assumptions based on topic.
      lndbp=lnblnk(standarddbpath)
      if(topic(1:3).eq.'mat')then
        defaultfile=dfcon     ! default standard file
C        sourcemsg='Material properties file (in common).'   ! dialog
        ending='.materialdb'  ! file ending
        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          write(lguess,'(a)') LFMAT(1:lnblnk(LFMAT)) ! suggested name
        elseif(ipathmat.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      LFMAT(1:lnblnk(LFMAT))
        endif
      elseif(topic(1:3).eq.'mlc')then
        defaultfile=dfmul
C        sourcemsg='Constructions file (in common).'
        ending='.constrdb'
        if(ipathmul.eq.0.or.ipathmul.eq.1)then
          lguess=LFMUL  ! use as is
        elseif(ipathmul.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      lfmul(1:lnblnk(lfmul))  ! prepend db folder path
        endif
      elseif(topic(1:3).eq.'opt')then
        defaultfile=doptdb
C        sourcemsg='Optical properties file (in common).'
        ending='.opticdb'
        if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
          lguess=LOPTDB  ! use as is
        elseif(ipathoptdb.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      LOPTDB(1:lnblnk(LOPTDB))  ! prepend db folder path
        endif
      elseif(topic(1:3).eq.'pdb')then
        defaultfile=DPCDB
C        sourcemsg='Plant components file (in common).'
        ending='.plantdb'
        if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
          lguess=LPCDB
        elseif(ipathpcdb.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      lpcdb(1:lnblnk(lpcdb))
        endif
      elseif(topic(1:3).eq.'prs')then
        defaultfile=DAPRES
C        sourcemsg='Pressure coefficients file (in common).'
        ending='.pressuredb'
        if(ipathapres.eq.0.or.ipathapres.eq.1)then
          lguess=LPCDB
        elseif(ipathapres.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      lapres(1:lnblnk(lapres))
        endif
      elseif(topic(1:3).eq.'msc')then
        defaultfile=DMCMPDBFL
C        sourcemsg='Active components file (in common).'
        ending='.miscdb'
        if(ipathmsc.eq.0.or.ipathmsc.eq.1)then
          lguess=MCMPDBFL
        elseif(ipathmsc.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      MCMPDBFL(1:lnblnk(MCMPDBFL))
        endif
      elseif(topic(1:3).eq.'mld')then
        defaultfile=dmdbnam
C        sourcemsg='Mould isopleths file (in common).'
        ending='.moulddta'
        if(ipathmsc.eq.0.or.ipathmsc.eq.1)then
          lguess=lfmould
        elseif(ipathmsc.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      lfmould(1:lnblnk(lfmould))
        endif
      elseif(topic(1:3).eq.'pro')then
        defaultfile=DPRFDB
C        sourcemsg='Event profile file (in common).'
        ending='.eventdb.a'
        if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
          lguess=LPRFDB
        elseif(ipathprodb.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      LPRFDB(1:lnblnk(LPRFDB))
        endif
      elseif(topic(1:3).eq.'clm')then
        defaultfile=DCLIM
C        sourcemsg='Weather file (in common).'
        if(ipathclim.eq.0.or.ipathclim.eq.1)then
          lguess=LCLIM
        elseif(ipathclim.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      LCLIM(1:lnblnk(LCLIM))
        endif
      elseif(topic(1:3).eq.'cfc')then
        defaultfile=DCFCDB     ! default standard file
C        sourcemsg='CFC layers properties file (in common).'   ! dialog
        ending='.cfcdb.a'  ! file ending
        if(ipathcfc.eq.0.or.ipathcfc.eq.1)then
          write(lguess,'(a)') LCFCDB(1:lnblnk(LCFCDB)) ! suggested name
        elseif(ipathcfc.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      LCFCDB(1:lnblnk(LCFCDB))
        endif

      endif

C Create a general fall-back file name.
      lr=lnblnk(cfgroot)
      write(GDEF,'(2a)') cfgroot(1:lr),ending(1:lnblnk(ending))
   
      if(copydef)then
        write(outs248,'(2a)') 'The source file is:',
     &    defaultfile(1:lnblnk(defaultfile))
        call edisp248(iuout,outs248,100)
        sourcefile = defaultfile
      else
        call edisp(iuout,'  ')  ! echo blank line
C        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.

C Assumes that browsefilelist works on all platforms. If a problem
C then uncomment the iglib lines
C        if(iglib.eq.1.or.iglib.eq.3)then

C Use the standard ESP-r folder browse facility.
          sfile=' '
          snpfile=' '
          call edisp(iuout,' ')
          call browsefilelist('?','dbm','fil',sfile,snpfile,nfile,iier)
          if(nfile.gt.0)then
            sfile=' '
            snpfile=' '
            call browsefilelist('b','dbm','fil',sfile,snpfile,nfile,
     &        iier)
            if(snpfile(1:2).ne.'  ')then
              lndbp=lnblnk(standarddbpath)
              write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &          snpfile(1:lnblnk(snpfile))
C Debug.
              if(unixok) write(6,*) lltmp
            else

C User did not select a file. Take them back to the question.
              call usrmsg('You did not select any existing file.',
     &          'Canceling the action.','W')
              moddb=.false.
              istat=1
              return  ! cancel detected, restore name and redisplay menu.
            endif
          endif

C Assumes that browsefilelist works on all platforms. If a problem
C then uncomment the iglib lines
C        elseif(iglib.eq.2)then
C          CALL EASKXORGTKF(lguess,sourcemsg,'Confirm:',
C     &      defaultfile,lltmp,'sourcefile name',IER,numhelp)
C        endif

C If user request jump back and re-display the menu.
C        if(ier.eq.-3)then
C          moddb=.false.
C          istat=1
C          return  ! cancel detected, restore name and redisplay menu.
C        endif
        sourcefile = lltmp   ! remember this

        if(topic(1:3).eq.'mat')then
          ipathmat=1  ! signal that it is a local file.
        elseif(topic(1:3).eq.'mlc')then
          ipathmul=1  ! signal that it will be a local file.
        elseif(topic(1:3).eq.'opt')then
          ipathoptdb=1  ! signal that it will be a local file.
        elseif(topic(1:3).eq.'pdb')then
          ipathpcdb=1  ! signal that it will be a local file.
        elseif(topic(1:3).eq.'prs')then
          ipathapres=1  ! signal that it is a local file.
        elseif(topic(1:3).eq.'msc')then
          ipathmsc=1  ! signal that it will be a local file.
        elseif(topic(1:3).eq.'mld')then
          ipathmould=1  ! signal that it will be a local file.
        elseif(topic(1:3).eq.'pro')then
          ipathprodb=1  ! signal that it is a local file.
        elseif(topic(1:3).eq.'clm')then
          ipathclim=1   ! signal that it is a local file.
        elseif(topic(1:3).eq.'cfc')then
          ipathcfc=1  ! signal that it is a local file.
        else

        endif
      endif

      lr=lnblnk(cfgroot)
      if(dbspth(1:2).eq.'  '.or.dbspth(1:2).eq.'./')then
        write(LTMP,'(2a)') cfgroot(1:lr),ending(1:lnblnk(ending))
      elseif(dbspth(1:3).eq.'../')then
        write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &    cfgroot(1:lr),ending(1:lnblnk(ending))
      else
        write(LTMP,'(4a)') dbspth(1:lnblnk(dbspth)),fs,
     &    cfgroot(1:lr),ending(1:lnblnk(ending))
      endif

C Echo the suggested name of the destination file.
      if(copydef)then
        write(outs248,'(2a)') 'The suggested destination file is:',
     &    LTMP(1:lnblnk(LTMP))
        call edisp248(iuout,outs248,100)
      endif

C Confirm name of model file.
   66 clkok=.false.
      CALL EASKSCNCL(ltmp,
     & 'Suggested name of the model copy of standard file.','Confirm:',
     &  'cancel',clkok,72,gdef,'copied file',IER,numhelp)
      if(clkok)then
        moddb=.false.
        istat=1
        return  ! cancel detected, restore name and redisplay menu.
      endif

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
     &        LTMP(1:lnblnk(LTMP))
          else
            write(LCOPY,'(3a)') path(1:lnblnk(path)),fs,
     &        LTMP(1:lnblnk(LTMP))
          endif
        else
          write(LCOPY,'(3a)') path(1:lnblnk(path)),fs,
     &      LTMP(1:lnblnk(LTMP))
        endif

C Now copy sourcefile to lcopy.
        doit = ' '
        if(unixok)then
          write(doit,'(4a)',IOSTAT=IOS) 'cp ',
     &      sourcefile(1:lnblnk(sourcefile)),' ',
     &      LCOPY(1:lnblnk(LCOPY))
        else

C Standard dos copy logic.
          message='getting materials file from '
          call dblongdoscopy(sourcefile,lcopy,message,doit,ier)
        endif
        call usrmsg('copying file via',doit,'-')
        call runit(doit,'-')

C Depending on whether Unix or DOS based setup explicit path
C to detect if a file exists at the desired location. If DOS
C then check for spaces in name and change / to \.
        call isunix(unixok)
        if(unixok)then
          call addpath(LTMP,longtfile,concat)
        else
          call addpath(LTMP,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        ltf=max(1,LNBLNK(longtfile))
        INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

C If it exists then update the common block name.
        if(XST)then
          call fdroot(lcopy,lpath,fname)
          if(topic(1:3).eq.'mat')then
            write(LFMAT,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          elseif(topic(1:3).eq.'mlc')then
            write(LFMUL,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          elseif(topic(1:3).eq.'opt')then
            write(LOPTDB,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          elseif(topic(1:3).eq.'pdb')then
            write(LPCDB,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          elseif(topic(1:3).eq.'prs')then
            write(lapres,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          elseif(topic(1:3).eq.'msc')then
            write(MCMPDBFL,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          elseif(topic(1:3).eq.'mld')then
            write(lfmould,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          elseif(topic(1:3).eq.'pro')then
            write(LPRFDB,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          elseif(topic(1:3).eq.'clm')then
            write(LCLIM,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          elseif(topic(1:3).eq.'cfc')then
            write(LCFCDB,'(3a)') dbspth(1:lnblnk(dbspth)),fs,
     &        fname(1:lnblnk(fname))
          else

          endif
        else
          call edisp(iuout,'Problem locating the local file.')
          call edisp(iuout,'Try browsing ../dbs to find it.')
          call edisp(iuout,' ')
        endif

C On return parent code can use this flag.
        moddb=.true.

C Signal that file is a local to model.
        if(topic(1:3).eq.'mat')then
          ipathmat=1
        elseif(topic(1:3).eq.'mlc')then
          ipathmul=1  
        elseif(topic(1:3).eq.'opt')then
          ipathoptdb=1
        elseif(topic(1:3).eq.'pdb')then
          ipathpcdb=1
          lltmp=LPCDB  ! reset lltmp for use at 44
        elseif(topic(1:3).eq.'prs')then
          ipathapres=1
        elseif(topic(1:3).eq.'msc')then
          ipathmsc=1
        elseif(topic(1:3).eq.'mld')then
          ipathmould=1
        elseif(topic(1:3).eq.'pro')then
          ipathprodb=1
        elseif(topic(1:3).eq.'clm')then
          ipathclim=1
        elseif(topic(1:3).eq.'cfc')then
          ipathcfc=1
        else

        endif
        istat=0   ! after return use the new file
        return
      else
        goto 66  ! ask for the file name again
      endif
      
      end
      

C ******** usemodeldbsfile
C usemodeldbsfile is a general facility to browse files in the
C model ../dbs folder. All files in the ../dbs folder will 
C be added to a menu list for selection. Selected file becomes
C the current common file type and is returned via lltmp.
      subroutine usemodeldbsfile(topic,numhelp,lltmp,istat,moddb)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"

      common/OUTIN/IUOUT,IUIN
      
      integer lnblnk  ! function definition
      
C Passed parameters:
      character topic*3   ! which type of file e.g. mat = materials
                          ! mlc = constructions  opt = optics
                          ! pdb = plant components  prs = pressure coef
                          ! msc = active components  mld = mould isopleths
                          ! pro = event profiles  clm = weather
      integer numhelp     ! number of help lines from calling code
      character lltmp*144 ! the returned file name
      integer istat       ! return status index for calling code
      logical moddb       ! set true if file name change
      integer nfile       ! nb of files in dbs folder

C Local variables:
      character sourcemsg*48     ! dialog for source dialog
      character sfile*72,snpfile*72,fname*96

      character lguess*144
      character fs*1

      CHARACTER outs248*248
      
C Local strings for user selections dependent on graphic library.
      integer lndbp   ! for length of standard database path
      integer iier
      logical unixok  ! to check for database path file separators

C Clear string buffers.
      lltmp='  '
      lguess='  '

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Remember current file for the current topic, set message to
C users and the guess for file name.
      lndbp=lnblnk(standarddbpath)
      if(topic(1:3).eq.'mat')then
        sourcemsg='material properties file (in ../dbs).'   ! dialog
        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          write(lguess,'(a)') LFMAT(1:lnblnk(LFMAT)) ! suggested name
        elseif(ipathmat.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      LFMAT(1:lnblnk(LFMAT))
        endif
      elseif(topic(1:3).eq.'mlc')then
        sourcemsg='constructions file (in ../dbs).'
        if(ipathmul.eq.0.or.ipathmul.eq.1)then
          lguess=LFMUL  ! use as is
        elseif(ipathmul.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      lfmul(1:lnblnk(lfmul))  ! prepend db folder path
        endif
      elseif(topic(1:3).eq.'opt')then
        sourcemsg='optical properties file (in ../dbs).'
        if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
          lguess=LOPTDB  ! use as is
        elseif(ipathoptdb.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      LOPTDB(1:lnblnk(LOPTDB))  ! prepend db folder path
        endif
      elseif(topic(1:3).eq.'pdb')then
        sourcemsg='plant components file (in ../dbs).'
        if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
          lguess=LPCDB
        elseif(ipathpcdb.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      lpcdb(1:lnblnk(lpcdb))
        endif
      elseif(topic(1:3).eq.'prs')then
        sourcemsg='pressure coefficients file (in ../dbs).'
        if(ipathapres.eq.0.or.ipathapres.eq.1)then
          lguess=LPCDB
        elseif(ipathapres.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      lapres(1:lnblnk(lapres))
        endif
      elseif(topic(1:3).eq.'msc')then
        sourcemsg='active components file (in ../dbs).'
        if(ipathmsc.eq.0.or.ipathmsc.eq.1)then
          lguess=MCMPDBFL
        elseif(ipathmsc.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      MCMPDBFL(1:lnblnk(MCMPDBFL))
        endif
      elseif(topic(1:3).eq.'mld')then
        sourcemsg='mould isopleths file (in ../dbs).'
        if(ipathmsc.eq.0.or.ipathmsc.eq.1)then
          lguess=lfmould
        elseif(ipathmsc.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      lfmould(1:lnblnk(lfmould))
        endif
      elseif(topic(1:3).eq.'pro')then
        sourcemsg='event profile file (in ../dbs).'
        if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
          lguess=LPRFDB
        elseif(ipathprodb.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      LPRFDB(1:lnblnk(LPRFDB))
        endif
      elseif(topic(1:3).eq.'clm')then
        sourcemsg='weather file (in ../dbs).'
        if(ipathclim.eq.0.or.ipathclim.eq.1)then
          lguess=LCLIM
        elseif(ipathclim.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      LCLIM(1:lnblnk(LCLIM))
        endif
      elseif(topic(1:3).eq.'cfc')then
        sourcemsg='CFC layers file (in ../dbs).'
        if(ipathcfc.eq.0.or.ipathcfc.eq.1)then
          lguess=LCFCDB
        elseif(ipathcfc.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      LCFCDB(1:lnblnk(LCFCDB))
        endif
      endif

C Remind user the current common data file.
      write(outs248,'(4a)') 'Currently using:',
     &  lguess(1:lnblnk(lguess)),' please identify a ',
     &  sourcemsg(1:lnblnk(sourcemsg))
      call edisp248(iuout,outs248,90)

C Use the standard ESP-r folder browse facility.
      sfile=' '
      snpfile=' '
      call edisp(iuout,' ')
      call browsefilelist('?','dbs','fil',sfile,snpfile,nfile,iier)
      if(nfile.gt.0)then
        sfile=' '
        snpfile=' '
        call browsefilelist('b','dbs','fil',sfile,snpfile,nfile,iier)
        if(snpfile(1:2).ne.'  ')then
          write(fname,'(3a)')dbspth(1:lnblnk(dbspth)),fs,
     &      snpfile(1:lnblnk(snpfile))

C Debug.
C          if(unixok) write(6,*) fname
          write(lltmp,'(a)') fname(1:lnblnk(fname))

        else

C User did not select a file. Take them back to the question.
          call usrmsg('You did not select any existing file.',
     &      'Canceling the action.','W')
          istat=1
          moddb=.false.
          return  ! cancel detected, restore name and redisplay menu.
        endif
      else
        call usrmsg('You did not select any existing file.',
     &    'Canceling the action.','W')
        istat=1
        moddb=.false.
        return  ! cancel detected, restore name and redisplay menu.
      endif

      moddb=.true.
      if(topic(1:3).eq.'mat')then
        ipathmat=1  ! signal that it is a local file.
        write(LFMAT,'(a)') fname(1:lnblnk(fname))
      elseif(topic(1:3).eq.'mlc')then
        ipathmul=1  ! signal that it will be a local file.
        write(LFMUL,'(a)') fname(1:lnblnk(fname))
      elseif(topic(1:3).eq.'opt')then
        ipathoptdb=1  ! signal that it will be a local file.
        write(LOPTDB,'(a)') fname(1:lnblnk(fname))
      elseif(topic(1:3).eq.'pdb')then
        ipathpcdb=1  ! signal that it will be a local file.
        write(LPCDB,'(a)') fname(1:lnblnk(fname))
      elseif(topic(1:3).eq.'prs')then
        ipathapres=1  ! signal that it is a local file.
        write(lapres,'(a)') fname(1:lnblnk(fname))
      elseif(topic(1:3).eq.'msc')then
        ipathmsc=1  ! signal that it will be a local file.
        write(MCMPDBFL,'(a)') fname(1:lnblnk(fname))
      elseif(topic(1:3).eq.'mld')then
        ipathmould=1  ! signal that it will be a local file.
        write(lfmould,'(a)') fname(1:lnblnk(fname))
      elseif(topic(1:3).eq.'pro')then
        ipathprodb=1  ! signal that it is a local file.
        write(LPRFDB,'(a)') fname(1:lnblnk(fname))
      elseif(topic(1:3).eq.'clm')then
        ipathclim=1  ! signal that it is a local file.
        write(LCLIM,'(a)') fname(1:lnblnk(fname))
      elseif(topic(1:3).eq.'cfc')then
        ipathcfc=1  ! signal that it is a local file.
        write(LCFCDB,'(a)') fname(1:lnblnk(fname))
      else

      endif

C By definition of the filebrowse facility the selected file
C will exist.
      istat=0   ! after return use the new file
      return
      
      end
      

C ******** usecommondbsfile
C usecommondbsfile is a general facility to browse files in the
C distribution databases folder. All files in this folder will 
C be added to a menu list for selection. Selected file becomes
C the current common file type and is returned via lltmp.
      subroutine usecommondbsfile(topic,numhelp,lltmp,istat,moddb)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"

      common/OUTIN/IUOUT,IUIN
      
      integer lnblnk  ! function definition
      
C Passed parameters:
      character topic*3   ! which type of file e.g. mat = materials
                          ! mlc = constructions  opt = optics
                          ! pdb = plant components  prs = pressure coef
                          ! msc = active components  mld = mould isopleths
                          ! pro = event profiles  clm = weather
      integer numhelp     ! number of help lines from calling code
      character lltmp*144 ! the returned file name
      integer istat       ! return status index for calling code
      logical moddb       ! set true if file name change
      integer nfile       ! nb of files in dbs folder

C Local variables:
      character sourcemsg*48     ! dialog for source dialog
      character sfile*72,snpfile*72
C     character fname*96

      character lguess*144
      character fs*1

      CHARACTER outs248*248
      
C Local strings for user selections dependent on graphic library.

      integer lndbp   ! for length of standard database path
      integer iier
      logical unixok  ! to check for database path file separators

C Clear string buffers.
      lltmp='  '
      lguess='  '

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Remember current file for the current topic, set message to
C users and the guess for file name.
      lndbp=lnblnk(standarddbpath)
      if(topic(1:3).eq.'mat')then
        sourcemsg='material properties file (in common).'   ! dialog
        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          write(lguess,'(a)') LFMAT(1:lnblnk(LFMAT)) ! suggested name
        elseif(ipathmat.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      LFMAT(1:lnblnk(LFMAT))
        endif
      elseif(topic(1:3).eq.'mlc')then
        sourcemsg='constructions file (in common).'
        if(ipathmul.eq.0.or.ipathmul.eq.1)then
          lguess=LFMUL  ! use as is
        elseif(ipathmul.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      lfmul(1:lnblnk(lfmul))  ! prepend db folder path
        endif
      elseif(topic(1:3).eq.'opt')then
        sourcemsg='optical properties file (in common).'
        if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
          lguess=LOPTDB  ! use as is
        elseif(ipathoptdb.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      LOPTDB(1:lnblnk(LOPTDB))  ! prepend db folder path
        endif
      elseif(topic(1:3).eq.'pdb')then
        sourcemsg='plant components file (in common).'
        if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
          lguess=LPCDB
        elseif(ipathpcdb.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      lpcdb(1:lnblnk(lpcdb))
        endif
      elseif(topic(1:3).eq.'prs')then
        sourcemsg='pressure coefficients file (in common).'
        if(ipathapres.eq.0.or.ipathapres.eq.1)then
          lguess=LPCDB
        elseif(ipathapres.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      lapres(1:lnblnk(lapres))
        endif
      elseif(topic(1:3).eq.'msc')then
        sourcemsg='active components file (in common).'
        if(ipathmsc.eq.0.or.ipathmsc.eq.1)then
          lguess=MCMPDBFL
        elseif(ipathmsc.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      MCMPDBFL(1:lnblnk(MCMPDBFL))
        endif
      elseif(topic(1:3).eq.'mld')then
        sourcemsg='mould isopleths file (in common).'
        if(ipathmsc.eq.0.or.ipathmsc.eq.1)then
          lguess=lfmould
        elseif(ipathmsc.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      lfmould(1:lnblnk(lfmould))
        endif
      elseif(topic(1:3).eq.'pro')then
        sourcemsg='event profile file (in common).'
        if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
          lguess=LPRFDB
        elseif(ipathprodb.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      LPRFDB(1:lnblnk(LPRFDB))
        endif
      elseif(topic(1:3).eq.'clm')then
        sourcemsg='weather file (in common).'
        if(ipathclim.eq.0.or.ipathclim.eq.1)then
          lguess=LCLIM
        elseif(ipathclim.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      LCLIM(1:lnblnk(LCLIM))
        endif
      elseif(topic(1:3).eq.'cfc')then
        sourcemsg='CFC layer properties file (in common).'   ! dialog
        if(ipathcfc.eq.0.or.ipathcfc.eq.1)then
          write(lguess,'(a)') LCFCDB(1:lnblnk(LCFCDB)) ! suggested name
        elseif(ipathcfc.eq.2)then
          write(lguess,'(3a)') standarddbpath(1:lndbp),fs,
     &      LCFCDB(1:lnblnk(LCFCDB))
        endif
      endif

C Remind user the current common data file.
      write(outs248,'(4a)') 'Currently using:',
     &  lguess(1:lnblnk(lguess)),' please identify a ',
     &  sourcemsg(1:lnblnk(sourcemsg))
      call edisp248(iuout,outs248,90)

C Use the standard ESP-r folder browse facility.
      sfile=' '
      snpfile=' '
      call edisp(iuout,' ')
      call browsefilelist('?','dbm','fil',sfile,snpfile,nfile,iier)
      if(nfile.gt.0)then
        sfile=' '
        snpfile=' '
        call browsefilelist('b','dbm','fil',sfile,snpfile,nfile,iier)
        if(snpfile(1:2).ne.'  ')then
          lndbp=lnblnk(standarddbpath)
C          write(fname,'(3a)')standarddbpath(1:lndbp),fs,
C     &      snpfile(1:lnblnk(snpfile))

C Debug.
C          if(unixok) write(6,*) 'snpfile ',snpfile
C          if(unixok) write(6,*) 'fname ',fname
          write(lltmp,'(a)') snpfile(1:lnblnk(snpfile))

        else

C User did not select a file. Take them back to the question.
          call usrmsg('You did not select any existing file.',
     &      'Canceling the action.','W')
          istat=1
          moddb=.false.
          return  ! cancel detected, restore name and redisplay menu.
        endif
      else
        call usrmsg('You did not select any existing file.',
     &    'Canceling the action.','W')
        istat=1
        moddb=.false.
        return  ! cancel detected, restore name and redisplay menu.
      endif

      moddb=.true.
      if(topic(1:3).eq.'mat')then
        ipathmat=2  ! signal that it is a common file.
        write(LFMAT,'(a)') snpfile(1:lnblnk(snpfile))
      elseif(topic(1:3).eq.'mlc')then
        ipathmul=2  ! signal that it will be a common file.
        write(LFMUL,'(a)') snpfile(1:lnblnk(snpfile))
      elseif(topic(1:3).eq.'opt')then
        ipathoptdb=2  ! signal that it will be a common file.
        write(LOPTDB,'(a)') snpfile(1:lnblnk(snpfile))
      elseif(topic(1:3).eq.'pdb')then
        ipathpcdb=2  ! signal that it will be a common file.
        write(LPCDB,'(a)') snpfile(1:lnblnk(snpfile))
      elseif(topic(1:3).eq.'prs')then
        ipathapres=2  ! signal that it is a common file.
        write(lapres,'(a)') snpfile(1:lnblnk(snpfile))
      elseif(topic(1:3).eq.'msc')then
        ipathmsc=2  ! signal that it will be a common file.
        write(MCMPDBFL,'(a)') snpfile(1:lnblnk(snpfile))
      elseif(topic(1:3).eq.'mld')then
        ipathmould=2  ! signal that it will be a common file.
        write(lfmould,'(a)') snpfile(1:lnblnk(snpfile))
      elseif(topic(1:3).eq.'pro')then
        ipathprodb=2  ! signal that it is a common file.
        write(LPRFDB,'(a)') snpfile(1:lnblnk(snpfile))
      elseif(topic(1:3).eq.'clm')then
        ipathclim=2   ! signal that it is a common file.
        write(LCLIM,'(a)') snpfile(1:lnblnk(snpfile))
      elseif(topic(1:3).eq.'cfc')then
        ipathcfc=2  ! signal that it will be a common file.
        write(LCFCDB,'(a)') snpfile(1:lnblnk(snpfile))
      else

      endif

C By definition of the filebrowse facility the selected file
C will exist.
      istat=0   ! after return use the new file
      return
      
      end
      


