C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file contains the following subroutines.
C EDCFG           - editing of a system configuration file.
C NEWPRB          - new model specification.
C visualz         - provides hidden line or coloured surface visualisation.
C EDZCOMP         - editing of zone composition.
C globaltransform - move all zones.
C globalrotate    - rotation all zones.
C globalscale     - scale all zones.


C ******************** EDCFG ********************
C Controls editing of a system configuration file and allows updated 
C information to be saved to a new file.

      subroutine edcfg(ITRC,ITRU,IER)
      USE AIM2_Inputs, ONLY : AIM2_Interface, iAIM2
      IMPLICIT NONE     
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "uncertainty.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "tdf2.h"
#include "control.h"
#include "gnetwk.h"
#include "esprdbfile.h"
#include "plant.h"
#include "power.h"
#include "prj3dv.h"
#include "espriou.h"
#include "FMI.h"
#include "help.h"

C Function definition.
      INTEGER :: lnblnk
      
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      INTEGER :: mmod,limit,limtty
      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER :: iuout,iuin,ieout

      logical markdown   ! Markdown flag
      common/markdownflag/markdown
      common/appw/iappw,iappx,iappy
      INTEGER :: iappw,iappx,iappy
      integer childterminal   ! picks up mmod from starting of prj
      common/childt/childterminal
      common/gzonpik/izgfoc,nzg,nznog(mcom)
      INTEGER :: izgfoc,nzg,nznog
      
      INTEGER :: ncomp,ncon
      common/C1/NCOMP,NCON
      INTEGER :: indcfg
      common/C6/INDCFG
      INTEGER :: IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      integer IC1,IE1,ICT,IC2,IE2
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      common/user/browse

C Plant network.
      common/C23/IFPNF,LPNF
      INTEGER :: IFPNF

      common/cctlnm/ctldoc,lctlf
      CHARACTER CTLDOC*248,LCTLF*72

C Defaults.
      character*96 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      common/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF
      character DFCFD*72,DECMPDBFL*72,DICONDBFL*72
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DICONDBFL

      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      INTEGER :: nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),scfdres(MSPS),sipvres
      character sblres*72,sflres*72,splres*72,smstres*72,
     &  selres*72,scfdres*72,sipvres*72

      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      INTEGER :: NPRE
      REAL :: FPRE
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      common/MFLDOC/DEPRE(MPRD)
      COMMON/NWKGRD/GRMAX(3),GRSPC(3),GRLYRH(MLYRS)
      REAL :: GRMAX,GRSPC,GRLYRH
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW
      REAL :: SCALF,VIEWCEN,VIEWLIM
      INTEGER :: IVIEW
      COMMON/NWKSTR/LEGNWKNAM,NWKNAM,NWKDSC,NWKTYPSTR(MNWKTYP)
      CHARACTER LEGNWKNAM*72,NWKNAM*72,NWKDSC*72,NWKTYPSTR*12
      COMMON/ICONDBNAM/ICONDBFL
      character ICONDBFL*72
      COMMON/NWKTYP/INWKTYP,vergnf
      INTEGER :: inwktyp
      REAL :: vergnf
      COMMON/NWKGRDL/GON,SON
      LOGICAL GON,SON
     
C TDF related.
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15
     
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)

C Global flow control options.
      integer igflowtype    ! as in ictype
      real    cmgflow      ! as in cm()
      common/globalflow/igflowtype,cmgflow(35)

C Global flow component options.
      integer igusercrack                   ! zero use default values, 1 user global pref, 2 assign indiv
      real    usecrackinmm,usecrackexmm     ! user global crack width for inside and external cracks
      integer iguserdooruc                  ! ditto
      real    userdoorucinmm,userdoorucexmm ! user global door undercut mm for inside and external doors
      integer igusersash                    ! ditto
      real    usersashmm                    ! user global sash window opening height
      integer iguseropenpc                  ! zero use default, 1 user global prefs, 2 assign indiv
      real    useropenwinpc                 ! percent of surface area for orifice 
      integer  igusertrickle                ! zero use default, 1 user global for height, 2 assign each
      real    usertricklemm                 ! user global trickle vent opening height
      integer iguserdischfact               ! zero use default values, 1 user global prefs, 2 assign each
      real    userdfdooruc,userdfdoor,userdfwin,userdftrickle,userdfbi
      integer iguserajar                    ! zero use default values, 1 user global pref, 2 assign indiv
      real    userinajar,userexajar         ! user adar open width for inside and exteran bi-directional                  
      common/userflow/igusercrack,usecrackinmm,usecrackexmm,
     &  iguserdooruc,userdoorucinmm,userdoorucexmm,igusersash,
     &  usersashmm,iguseropenpc,useropenwinpc,igusertrickle,
     &  usertricklemm,iguserdischfact,userdfdooruc,userdfdoor,
     &  userdfwin,userdftrickle,userdfbi,
     &  iguserajar,userinajar,userexajar   

      INTEGER :: NCONTM,NOCNTM
      logical OK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical browse
      logical xst,xstl,clkok,havefile,unixok
      LOGICAL concat,remote
      integer iglib   ! to test for X11 interface
      integer iappwpc ! application height as % of nominal size

      dimension ITEMS(31)
      character*72 LAPROB,DFILE,LPNF,tfile
      character longtfile*144,longtfiledos*144
      character ITEMS*34,ETEXT*82
      character*72 DCNN,ltmp
      character tmode*8,doit*248
      character DEPRE*40,dstmp*24,tab*1,outsn*124,outsd*124
      character CXITM*48,outs*124,fs*1
      character CONTMNAM*12
      character ltpath*72,filen*72
      character t144*144    ! for passing to erprcdb
      integer ictl
      integer inpic,ivals   ! number of zones to include
      dimension ivals(MCOM) ! list of zones
      logical silent        ! if true do not interact with user
      integer ier           ! non-zero if a problem
      integer loop
      character hold32*32,hold*32
      logical found_pandoc ! to test application existance
      logical isrelated    ! test whether surface USE is flow related
      logical isbi,isopen  ! for checking if an opening or bi-directional
      logical proceed

      INTEGER :: itrc,itru,i,ic,icc,istat,k
      INTEGER :: iacc,icfoc,iclkok,idindcfg,lnnwknam
      INTEGER :: iier,im,ino,ios,ipedit,ipass,is
      INTEGER :: isindcfg,iuf,ium,iuo,iv,iver,iw,iz
      INTEGER :: iser,lenlfil,ln,ltrf,nets,nitems
      REAL :: szlow,szcog,szhigh
      REAL :: VAL
      integer haveflowattributes

C Function definition.   
      INTEGER :: igraphiclib
      integer :: nbopen,nbbi  ! how many USE openings and bi-directionals found

#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont   ! passed to viewtext
#endif

C HVAC
C ihvacflag=1 indicates HVAC models are active; 0 indicates no HVAC models.
      common/hvacinfo/ihvacflag,hvacfile
      integer ihvacflag
      character hvacfile*72,HVAC_ITEMS*24
      integer nhvac_items
      integer hvind, indhvac, hvidind
      PARAMETER (nhvac_items = 5)
      dimension HVAC_ITEMS(nhvac_items)
      integer ISTRW

      helpinsub='edcfg'  ! set for subroutine
      
C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Begin with high level menu, determine how many networks and control loops
C are active.
      IOS=0   ! initial guess
      IUF=IFIL+2
    3 INO=-4
      nets=0
      if(indcfg.ge.2)nets=nets+1
      if(iairn.ge.1)nets=nets+1
      if(ientxist.gt.0)nets=nets+1
      if(ihvacflag.gt.0)nets=nets+1

      ictl=ncf+ncl+ncc+ngf+nof+nCFCctlloops
      IER=0
      WRITE(ITEMS(1),'(A,A)') ' model: ',LCFGF(1:22)
      IF(INDCFG.EQ.0)THEN 
        ITEMS(2)= 'a domains >> registration'
      ELSEIF(INDCFG.EQ.1)THEN
        ITEMS(2)= 'a domains >> building only'
      ELSEIF(INDCFG.EQ.2)THEN
        ITEMS(2)= 'a domains >> plant only'
      ELSEIF(INDCFG.EQ.3)THEN
        ITEMS(2)= 'a domains >> building & plant'
      ENDIF
      ITEMS(3)   ='b context'
      WRITE(ITEMS(4),'(A,I2,A)')' ..... Building (',
     &                         NCOMP,' zones) .....'
      ITEMS(5)   ='c composition'
      ITEMS(6)   ='d management agents'
      WRITE(ITEMS(7),'(A,I2,A)')' ..... Networks (',
     &                         nets,' defined) .....'
      if(indcfg.ge.2.or.ihvacflag.gt.0)then
        ITEMS(8)='e plant & systems (defined)'
      else
        ITEMS(8)='e plant & systems'
      endif

C Check flags for air flow type. (How does the reference to iAIM2
C work? If iAIM2=0 then the next line tests for iAIM2=1.)
      IF (iairn.lt.1) THEN
        IF (iAIM2.EQ.1) THEN
          ITEMS(9)='f empirical air infiltration'
        ELSE
          ITEMS(9)='f prescribed fluid flow'
        ENDIF
      ELSE
        if(iairn.ge.1)then
          ITEMS(9)='f network flow (defined)'
        else
          ITEMS(9)='f network flow'
        endif
      ENDIF
      if(ientxist.gt.0)then
        ITEMS(10)='g electrical (defined)'
      else
        ITEMS(10)='g electrical'
      endif
      if(nocntm.gt.0)then
        ITEMS(11)='h contaminant (defined)'
      else
        ITEMS(11)='h contaminant'
      endif
      WRITE(ITEMS(12),'(A,I2,A)')' ..... Controls (',
     &                ictl,' defined) .....'
      if(ncf.eq.0)then
        WRITE(ITEMS(13),'(A)')     'i zones'
      else
        WRITE(ITEMS(13),'(A,I2,A)')'i zones (',ncf,' loop)'
      endif
      if(ncl.eq.0)then
        WRITE(ITEMS(14),'(A)')     'j plant & systems'
      else
        WRITE(ITEMS(14),'(A,I2,A)')'j plant & systems (',ncl,' loops)'
      endif
      if(ncc.eq.0)then
        WRITE(ITEMS(15),'(A)')     'k network flow'
      else
        WRITE(ITEMS(15),'(A,I2,A)')'k network flow (',ncc,' loops)'
      endif
      if(nof.eq.0)then
        WRITE(ITEMS(16),'(A)')     'l optics'
      else
        WRITE(ITEMS(16),'(A,I2,A)')'l optics (',nof,' loops)'
      endif
      if(ngf.eq.0)then
        WRITE(ITEMS(17),'(A)')     'm global system'
      else
        WRITE(ITEMS(17),'(A,I2,A)')'m global system (',ngf,' loops)'
      endif
      if(nCFCctlloops.eq.0)then
        WRITE(ITEMS(18),'(A)')     'n complex fenestration '
      else
        WRITE(ITEMS(18),'(A,I2,A)')
     &   'n complex fenestration (',nCFCctlloops,' loops)'
      endif
      if(is_FMU)then
        WRITE(ITEMS(19),'(A,I2,A)')'o FMI (',FMUNOF,' FMUs)'
      else
        WRITE(ITEMS(19),'(A)')     'o FMI'
      endif
      ITEMS(20)=' ..... Uncertainty .....'
      ITEMS(21)='q define'
      ITEMS(22)=' ..... Actions .....'
      ITEMS(23)='r visualisation'
      ITEMS(24)='s simulation'
      ITEMS(25)='t results analysis'
      ITEMS(26)='u contents report'
      ITEMS(27)='v calibration'
      ITEMS(28)=' ...... Miscellaneous .....'
      ITEMS(29)='! save model'
      ITEMS(30)='? help'
      ITEMS(31)='- exit menu'
      nitems=31

C Help text for this menu.
      helptopic='model_comprises_br_sim'
      call gethelptext(helpinsub,helptopic,nbhelp)

C If user has defined model and perhaps resized the display then
C redraw the model image.
      if(indcfg.eq.2)then
        if(browse)then
          CALL EMENU('Browse/simulate',ITEMS,nitems,INO)
        else
          CALL EMENU('Browse/edit/simulate',ITEMS,nitems,INO)
        endif
      elseif(indcfg.eq.0)then
        if(browse)then
          CALL EMENU('Browse/simulate',ITEMS,nitems,INO)
        else
          CALL EMENU('Browse/edit/simulate',ITEMS,nitems,INO)
        endif
      else
        if(CFGOK.AND.MODIFYVIEW)then
          MODBND=.TRUE.; MODLEN=.TRUE.
          ITSNM=1
          ITVNO=1
          nzg=NCOMP
          if(nzg.gt.0)then
            DO 44 I=1,nzg
              nznog(I)=I
  44        CONTINUE

C (Re)set all surfaces to standard line width.
            CALL INLNST(1)
            izgfoc=0
            CALL redraw(IER)
          endif

C Redraw the configuration buttons.
          if(MMOD.EQ.8)then
            call redrawbuttons()
            WRITE(ETEXT,'(2A)')'Model: ',
     &        modeltitle(1:lnblnk(modeltitle))
            iside=1; isize=1; ifont=1
            call viewtext(etext,iside,isize,ifont)
          endif
        endif
        if(browse)then
          CALL EMENU('Browse/simulate',ITEMS,nitems,INO)
        else
          CALL EMENU('Browse/edit/simulate',ITEMS,nitems,INO)
        endif
      endif
      
C Exit back to calling menu and clear dialogue box.
      IF(INO.EQ.nitems)THEN
        CALL USRMSG(' ',' ','-')
        RETURN
        
C Help text for the menu.
      ELSEIF(INO.EQ.nitems-1)THEN
        helptopic='model_comprises_br_sim'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('configuration images',nbhelp,'-',0,0,IER)
        goto 3
        
C Save current common block information to file.
      ELSEIF(INO.EQ.nitems-2)THEN
        if(browse)then
          call usrmsg('Cannot save the model while in browse',
     &                'mode. You must own the model.','W')
          goto 3
        endif

C If configuration file format is version 3 then check to see if there
C are any utility files. If found, ask if the user wants to save in
C the older format.
        DFILE=' '
        ltmp=LCFGF
   89   CALL EASKS(ltmp,' ','Update system configuration file?',
     &    72,DFILE,'update configuration file',IER,nbhelp)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          LCFGF=ltmp
        else
          call usrmsg('Please re-enter.',' ','W')
          goto 89
        endif

C Check connections file.
        if(ncon.gt.1)then
  289     write(DCNN,'(a,a)')cfgroot(1:lnblnk(cfgroot)),'.cnn'
          if(LCNN(1:1).eq.' ')LCNN=DCNN
          ltmp=LCNN
          CALL EASKS(ltmp,' ','Surface connections file name?',
     &      72,DCNN,'surface connx file name',IER,nbhelp)
          if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
            LCNN=ltmp
          else
            call usrmsg('Please re-enter the file name. ',' ','W')
            goto 289
          endif
        endif

        call tstamp('>','prj: save configuration')
        call tstamp('>',LCFGF)
        CALL EMKCFG('-',IER)
        IF(IER.EQ.1)THEN
          helptopic='disk_might_be_full'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK('Problem creating new file.',
     &      'Try again?',OK,nbhelp)
          IF(OK)GOTO 89
        ENDIF
        goto 3
      ELSEIF(INO.EQ.1)THEN
        continue
        
C Technical domains associated with the model.
      ELSEIF(INO.EQ.2)THEN
        if(indcfg.eq.0)outs='Currently registration only'
        if(indcfg.eq.1)outs='Currently building only'
        if(indcfg.eq.2)outs='Currently plant only'
        if(indcfg.eq.3)outs='Currently building and plant'
        idindcfg=INDCFG+1
        isindcfg=0
        call MENUATOL(outs,'Domain options:',
     &    'a registration (site details only)',
     &    'b building (+ optional networks & CFD)',
     &    'c plant only',
     &    'd building & plant (+ all options)',
     &    ' ',' ',' ',' ',' ',' ',' ',' ',isindcfg,idindcfg,nbhelp)
        if(isindcfg.eq.0)then
          continue
        elseif(isindcfg-1.ne.indcfg)then
          call easkok('Are you sure you want to change',
     &      'the model domains?',
     &      ok,nbhelp)
          if(ok)then
            indcfg=isindcfg-1
            call usrmsg('Updating model domains ...',' ','-')
            CALL EMKCFG('-',IER)
            call usrmsg('Updating model domains ...done.',' ','-')
          endif
        endif
        IF(MMOD.EQ.8)then
          LN=max(1,LNBLNK(modeltitle))
          call redrawbuttons()
          WRITE(ETEXT,'(2A)')'Model: ',modeltitle(1:ln)
          iside=1; isize=1; ifont=1
          call viewtext(etext,iside,isize,ifont)
        endif

C Model context.
      ELSEIF(INO.EQ.3)THEN
        IF(CFGOK)THEN
          CALL EDSITE(ITRC,IER)
        ELSE
          CALL USRMSG('Please define your model first.',' ','W')
        ENDIF
        goto 3

C Go to zones definition menu.
      elseif(INO.EQ.5)THEN
        call EDZCOMP(ITRC,ITRU,IER)

C Define management agents.
      elseif(INO.eq.6)then
        call tstamp('>','PRJ: entering management agent control')
        call AGT_MNG(0,IER)
      
C Plant network description: select either an idealised HVAC network model (from the
C options offered) or define a new plant network model. In the former case, set the
C required parameters. In the latter case, ask for a plant network file, display its
C contents and invoke editing facilities.
      ELSEIF(INO.EQ.8)THEN
        IF(.NOT.CFGOK)THEN
          CALL USRMSG('Please define your plant network model first.',
     &                ' ','W')
          GOTO 3
        else
      
C Present options for the plant network selection.
          helptopic='submenu_plant_options'
          call gethelptext(helpinsub,helptopic,nbhelp)
          hvind=-1
          do while (hvind.ne.nhvac_items)
          
          if(ihvacflag.eq.0)then
            HVAC_ITEMS(1)='a idealised           '
          elseif(ihvacflag.eq.1)then
            HVAC_ITEMS(1)='a idealised (defined) '
          endif
          if(LPNF(1:4).eq.'UNKN'.or.LPNF(1:2).eq.'  ')then
            HVAC_ITEMS(2)='b explicit            '
          elseif(LPNF(1:4).ne.'UNKN'.or.LPNF(1:2).ne.'  ')then
            HVAC_ITEMS(2)='b explicit (defined)  '
          endif
          HVAC_ITEMS(3)  =' ___________________  '
          HVAC_ITEMS(4)  ='? help                '
          HVAC_ITEMS(5)  ='- exit menu           '
                    
          CALL EMENU('Plant model',HVAC_ITEMS,nhvac_items,hvind)
          
          if(hvind.eq.nhvac_items)then

C If user does not wish to continue, exit and clear dialogue box.
            CALL USRMSG(' ',' ','-')
            GOTO 3
          elseif(hvind.eq.nhvac_items-1)then
            CALL PHELPD('Idealised or explicit plant model?',
     &                   nbhelp,'-',0,0,IER)
          elseif(hvind.le.0)then

C No selection made.
            continue
          elseif(hvind.eq.1)then 

C Idealized plant model selected.
            CALL HVACGIN()
          elseif(hvind.eq.2)then

C Explicit plant model selected.
            if(LPNF(1:4).eq.'UNKN'.or.LPNF(1:2).eq.'  ')then
              helptopic='plt_config_create'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL EASKOK(
     &          'Currently there is no plant network associated with',
     &          'the model. Create one or link to existing file?',
     &          OK,nbhelp)
              IF(.NOT.ok)GOTO 3

              if(netpth(1:2).eq.'  '.or.netpth(1:2).eq.'./')then
                WRITE(LPNF,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.pln'
              else
                WRITE(LPNF,'(4a)') netpth(1:lnblnk(netpth)),'/',
     &               cfgroot(1:lnblnk(cfgroot)),'.pln'
              endif
              clkok=.false.
              ltmp=LPNF
              helptopic='plt_config_name'
              call gethelptext(helpinsub,helptopic,nbhelp)
              CALL EASKS(ltmp,' ','Plant network definition file?',72,
     &                   DPNF,'plant network file',IER,nbhelp)
              goto 47
            endif

            ltmp=LPNF
            iglib = igraphiclib()    ! determine if in X11, GTK or text mode.
            helptopic='plt_config_open'
            call gethelptext(helpinsub,helptopic,nbhelp)
 301        if(iglib.eq.2)then
              ISTRW=72
              CALL EASKSCMD(ltmp,' ','Plant network definition file?',
     &          'dereference',clkok,ISTRW,DPNF,
     &          'new file',iser,nbhelp)
              if(clkok)iclkok=2      ! note dereference.
              if(iser.eq.-3) goto 3  ! note GTK cancel.
            else
              CALL EASKS2CMD(ltmp,' ','Plant network definition file?',
     &          'cancel','dereference',iclkok,72,DPNF,
     &          'new file',iser,nbhelp)
              if(iclkok.eq.1) goto 3 ! note X11 cancel.
            endif

C If user wishes to deselect the current file, reset name to
C blank and save the configuration file.
  46        if(iclkok.eq.2)then
              LPNF='  '
              if(indcfg.eq.3)then
                indcfg=1
              elseif(indcfg.eq.2)then
                indcfg=0
              endif

C If simulation parameters, clear plant results names.
              if(nsset.gt.0)then
                splres(1)='  '
                splres(2)='  '
                splres(3)='  '
              endif
              call usrmsg('Removing plant network ...',' ','-')
              CALL EMKCFG('-',IER)
              call usrmsg('Removing plant network ...done.',' ','-')
              goto 3
            endif

C Otherwise, check the supplied file name and either setup a new
C plant network or edit the existing network.
  47        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
              LPNF=ltmp
            else
              call usrmsg(' ','Re-enter file name.','W')
              goto 301
            endif

            XST=.false.
            call FINDFIL(LPNF,XST)
            helptopic='plt_net_des'
            call gethelptext(helpinsub,helptopic,nbhelp)
            if(.not.xst) then            
              CALL EASKOK('This is a new plant network file.',
     &          'Proceed with network description?',OK,nbhelp)
              ipedit=0
            else
              CALL EASKOK('Found existing plant network file.',
     &          'Use or modify it?',OK,nbhelp)
              ipedit=1
            endif
            if(OK)then
              call tstamp('>','prj: enter epltnet')

C Reset the configuration type if appropriate but remember the
C initial state to return to if there is a problem with the
C plant network.
              isindcfg=indcfg
              if(indcfg.eq.2)then
                continue
              elseif(indcfg.eq.0)then
                indcfg=2
              elseif(indcfg.eq.1)then
                indcfg=3
              endif
              call epltnet(ipedit,iier)
 
C If an error has been encountered then reset the configuration type. 
              if(iier.ne.0)then
                indcfg=isindcfg
                call usrmsg(
     &            'Model change not recorded because of an error',
     &            'in the plant network description.','E')
              else
                call usrmsg('Updating model for plant network ...',
     &            ' ','-')
                CALL EMKCFG('-',IER)
                call usrmsg('Updating model for plant network ...done.',
     &            ' ','-')
              endif
            endif
          endif
          enddo  ! of while hvind ne nhvac_items
        endif

C Air flow definition: this can be held in text or graphics format.
      ELSEIF(INO.EQ.9)THEN
        XSTL=.false.
        helptopic='submenu_flow_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(IAIRN.eq.0)then
          call EASKABCD(' ','Define air flow via:',
     &      'empirical infiltration model','schedule',
     &      'new network','cancel',IW,nbhelp)
        elseif(IAIRN.eq.1)then
          XST=.false.
          call FINDFIL(LAPROB,XST)
          if(XST) then
            call edisp(iuout,' ')
            call edisp(iuout,'The current flow network is:')
            call edisp(iuout,LAPROB)
          endif
          call EASKATOG(' ','Define air flow via:',
     &      'empirical infiltration model','schedule',
     &      'existing network','new network',
     &      'cancel',' ',' ',IW,nbhelp)
        elseif(IAIRN.eq.2)then
          XST=.false.
          call FINDFIL(LAPROB,XST)
          if(XST) then
            call edisp(iuout,' ')
            call edisp(iuout,'The current flow network is:')
            call edisp(iuout,LAPROB)
          endif
          call EASKATOG(' ','Define air flow via:',
     &      'empirical infiltration model','schedule',
     &      'existing network','new network',
     &      'cancel',' ',' ',IW,nbhelp)
        elseif(IAIRN.eq.3)then

C Check if a legacy file also exists and if so offer it as an option.
          XST=.false.
          call FINDFIL(LAPROB,XST)
          if(XST) then
            call edisp(iuout,' ')
            call edisp(iuout,'The current flow network is:')
            call edisp(iuout,LAPROB)
          endif
          lnnwknam=lnblnk(LAPROB)
          write(LEGNWKNAM,'(2a)') LAPROB(1:lnnwknam-6),'.afn'
          XSTL=.false.
          call FINDFIL(LEGNWKNAM,XSTL)
          if(XSTL) then
            call edisp(iuout,' ')
            call edisp(iuout,'Also a legacy flow network is:')
            call edisp(iuout,LEGNWKNAM)
          endif
          if(.NOT.XSTL)then
            call EASKATOG(' ','Define air flow via:',
     &        'empirical infiltration model','schedule',
     &        '3D network','new network',
     &        'cancel',' ',' ',IW,nbhelp)
          else
            call EASKATOG(' ','Define air flow via:',
     &        'empirical infiltration model','schedule',
     &        '3D network','legacy network',
     &        'new network','cancel',' ',IW,nbhelp)
          endif
        endif

C Use AIM2 interface to define air flow.     
        airflowtype: IF(IW.EQ.1)THEN
          CALL AIM2_Interface()
          goto 3
        ELSE IF(IW.EQ.2)THEN airflowtype 

C Operation file creation and editing. Set IVER to zero to signal
C that prjfmk is not being called from the versioning facility.
          IC=-1
 250      CALL EASKGEOF('Select a zone:',CFGOK,IC,'-',34,IER)
          IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
          call tstamp('>','prj: enter zone operations')
          iver=0
          CALL PRJFMK(ITRC,ITRU,IUO,IC,IER,iver)

C Determine if another zone is requested.
          IC=-1
          GOTO 250
        ELSEIF(IW.EQ.3)THEN airflowtype
          call tstamp('>','prj: enter fluid network')
          if(IAIRN.eq.0)then  ! none defined

C Look and see if any surfaces have USE attributes. If so
C ask user if they want to scan model, otherwise ask user if
C text or graphic mode is required.
            haveflowattributes=0
            do loop=1,NCON
              iz=IC1(loop); is=IE1(loop)  ! current zone and surface.
              call isuseflowrelated(iz,is,isrelated)
              if(isrelated)then
                haveflowattributes=haveflowattributes+1
              endif
            enddo

C If there are surface attributes for flow, offer user the option
C to create the initial network based on these attributes.
            iacc=2
            if(haveflowattributes.gt.2)then
              helptopic='define_flow_via_USE'
              call gethelptext(helpinsub,helptopic,nbhelp)
              call edisp(iuout,
     &          'There are surfaces with mass flow attributes.')
              call edisp(iuout,
     &          'These can be used to help building your network.')
              iacc=3
              if(.NOT.XSTL)then
                CALL EASKABCD(' ','Descriptive method to employ:',
     &            'menus & lists','graphics tool',
     &            'scan of surface USE attributes',
     &            'cancel',iacc,nbhelp)
               else
                CALL EASKATOG(' ','Descriptive method to employ:',
     &            'legacy menus & lists','3D menus & lists',
     &            'graphics tool',
     &            'scan of surface USE attributes',
     &            'cancel',' ',' ',iacc,nbhelp)
               endif
            else
              if(.NOT.XSTL)then
                CALL EASKABC(' ','Descriptive method to employ:',
     &          'menus & lists','graphic tool','cancel',iacc,nbhelp)
              else
                CALL EASKABCD(' ','Descriptive method to employ:',
     &            'legacy menus & lists','3D menus & lists',
     &            'graphics tool','cancel',iacc,nbhelp)
              endif
            endif
            if(iacc.eq.1)then
              if(.NOT.XSTL)then
                IAIRN=3   ! new networks include position  ??
                LAPROB='UNKN'
                call MFPROB(IER)
              else
                IAIRN=1   ! user working with legacy formats
                LAPROB='UNKN'
                call MFPROB(IER)
              endif
              goto 3   ! jump back
            elseif(iacc.eq.2)then
              if(.NOT.XSTL)then
                IAIRN=2
                LAPROB='UNKN'
              else
                IAIRN=3   ! new networks include position
                LAPROB='UNKN'
                call MFPROB(IER)
                goto 3   ! jump back
              endif
              continue
            else
              if(haveflowattributes.ge.2)then

C Ask user preferences for flow component attributes. Set global defaults.
                proceed=.false.
                call globalflowprefs(proceed,ier)
                if(.NOT.proceed) goto 3

C Debug.
C                write(6,'(5a)') 'igusercrack,usecrackinmm,usecrackexmm',
C     &  'iguserdooruc,userdoorucinmm,userdoorucexmm,igusersash',
C     &  'usersashmm,iguseropenpc,useropenwinpc,igusertrickle',
C     &  'usertricklemm,iguserdischfact,userdfdooruc,userdfdoor',
C     &  'userdfwin,userdftrickle,userdfbi iguserajar'    
C                write(6,*) igusercrack,usecrackinmm,usecrackexmm,
C     &  iguserdooruc,userdoorucinmm,userdoorucexmm,igusersash,
C     &  usersashmm,iguseropenpc,useropenwinpc,igusertrickle,
C     &  usertricklemm,iguserdischfact,userdfdooruc,userdfdoor,
C     &  userdfwin,userdftrickle,userdfbi,iguserajar  

C See if any of the surface USE might require flow controls, i.e. open
C or bi-directional windows or doors at the outside.
                nbopen=0; nbbi=0              ! Clear counters.
                do loop=1,NCON
                  iz=IC1(loop); is=IE1(loop)  ! Get current zone & surface.
                  isbi=.false.; isopen=.false.

                  if(ICT(loop).eq.3)then      ! Check if partition.
                     CYCLE                    ! No need to create anything for this surface
                  endif
                  if(SUSE(iz,is,1)(1:1).eq.'-'.or.      ! Cases to ignore.
     &               SUSE(iz,is,1)(1:4).eq.'WALL'.or.
     &               SUSE(iz,is,1)(1:5).eq.'FLOOR'.or.
     &               SUSE(iz,is,1)(1:5).eq.'FURNI'.or.
     &               SUSE(iz,is,1)(1:7).eq.'ITEQUIP'.or.
     &               SUSE(iz,is,1)(1:5).eq.'PARTN'.or.
     &               SUSE(iz,is,1)(1:4).eq.'ROOF'.or.
     &               SUSE(iz,is,1)(1:5).eq.'STRUC'.or.
     &               SUSE(iz,is,1)(1:7).eq.'FIXTURE'.or.
     &               SUSE(iz,is,1)(1:6).eq.'PLANTS')then
                    CYCLE
                  endif
                  if(SUSE(iz,is,1)(1:4).eq.'DOOR'.or.
     &               SUSE(iz,is,1)(1:6).eq.'P-DOOR'.or.
     &               SUSE(iz,is,1)(1:6).eq.'H-DOOR'.or.
     &               SUSE(iz,is,1)(1:6).eq.'V-DOOR')then      ! For DOOR variants.
                     if(SUSE(iz,is,2)(1:4).eq.'OPEN')then
                       isopen=.true.
                     elseif(SUSE(iz,is,2)(1:5).eq.'BIDIR'.or.
     &                      SUSE(iz,is,2)(1:10).eq.'AJAR-BIDIR')then
                       isbi=.true.
                     endif
                   elseif(SUSE(iz,is,1)(1:5).eq.'FRAME'.or.
     &                    SUSE(iz,is,1)(1:7).eq.'F-FRAME')then  ! For FRAME vairants.
                     isbi=.false.; isopen=.false.
                   elseif(SUSE(iz,is,1)(1:5).eq.'GRILL')then    ! For GRILLs.
                     isbi=.false.; isopen=.false.
                     if(SUSE(iz,is,2)(1:4).eq.'OPEN')then
                       isopen=.true.                            ! Grill Open
                     endif
                   elseif(SUSE(iz,is,1)(1:6).eq.'WINDOW'.or.
     &                    SUSE(iz,is,1)(1:8).eq.'D-WINDOW'.or.
     &                    SUSE(iz,is,1)(1:8).eq.'S-WINDOW'.or.
     &                    SUSE(iz,is,1)(1:8).eq.'C-WINDOW')then  ! For Windows.
                     if(SUSE(iz,is,2)(1:4).eq.'OPEN')then
                       isopen=.true.
                     elseif(SUSE(iz,is,2)(1:4).eq.'SASH')then
                       isopen=.true.    ! AND there will be two connections to control!
                     elseif(SUSE(iz,is,2)(1:5).eq.'BIDIR'.or.
     &                      SUSE(iz,is,2)(1:10).eq.'AJAR-BIDIR')then
                       isbi=.true.
                     endif
                  elseif(SUSE(iz,is,1)(1:4).eq.'FICT')then
                    isbi=.false.; isopen=.false.
                  endif
                  if(isbi) nbbi=nbbi+1        ! increment counts
                  if(isopen) nbopen=nbopen+1
                enddo
                if(nbopen.eq.0.and.nbbi.eq.0)then
                  call edisp(iuout,
     &              'No controllable facade components detected.')
                endif
                if(nbopen.ge.1.or.nbbi.ge.1)then

C Check if there is a control file....
                  if(LCTLF(1:2).eq.'  '.or.LCTLF(1:4).eq.'UNKN')then
                     CALL EASKOK(
     & 'No control file associated with the model yet so all openings',
     & 'and bi-directional are in OPEN state. Continue?',OK,nbhelp)
                     IF(OK)then
                       igflowtype= -1      ! no control
                       continue
                     else
                       goto 3
                     endif
                   endif
                   helptopic='flow_opening_ctl'
                   call gethelptext(helpinsub,helptopic,nbhelp)
                   call EASKATOG(
     &               'Surface USE suggests facade openings.',
     &               'Control options:','always closed','always open',
     &               'open above X','open below X',
     &               'range based','natural ventilation',
     &               'do not control',IW,nbhelp)
                   if(IW.eq.1)then       ! set high setpoint, direct action full fraction
                     igflowtype=0        ! on/off
                     cmgflow(1)= 3
                     cmgflow(2)= 50.0
                     cmgflow(3)= 1.0
                     cmgflow(4)= 1.0
                   elseif(IW.eq.2)then   ! set low setpoint, indirect action, full fraction
                     igflowtype=0        ! on/off
                     cmgflow(1)= 3
                     cmgflow(2)= 0.0
                     cmgflow(3)= -1.0
                     cmgflow(4)= 1.0
                   elseif(IW.eq.3)then   ! ask setpoint, direct action full fraction
                     igflowtype=0        ! on/off
                     cmgflow(1)= 3
                     VAL=20.
                     CALL EASKR(VAL,' ','Open/ON if above (degrees)?',
     &                 -10.0,'W',100.0,'W',20.0,'setpoint',IER,nbhelp)
                     cmgflow(2)= VAL
                     cmgflow(3)= 1.0
                     cmgflow(4)= 1.0
                   elseif(IW.eq.4)then   ! ask setpoint, indirect action, full fraction
                     igflowtype=0        ! on/off
                     cmgflow(1)= 3
                     VAL=20.
                     CALL EASKR(VAL,' ','Open/ON if below (degrees)?',
     &                 -10.0,'W',100.0,'W',20.0,'setpoint',IER,nbhelp)
                     cmgflow(2)= VAL
                     cmgflow(3)= -1.0
                     cmgflow(4)= 1.0
                   elseif(IW.eq.5)then   ! range based
                     igflowtype=2        
                     cmgflow(1)= 6.
                     cmgflow(2)= 15.; cmgflow(3)= 21.; cmgflow(4)= 25.;
  55                 WRITE(HOLD,'(3f8.2)')cmgflow(2),cmgflow(3),
     &                 cmgflow(4)
                     CALL EASKS(HOLD,
     &      'Component nominal rate or area is used between low & mid.',
     &                 'Range setpoints: low, mid, high?',
     &                 32,' 15. 21. 25. ',' flow range controller',
     &                 IER,nbhelp)
                     K=0
                     CALL EGETWR(HOLD,K,cmgflow(2),0.,0.,'-',
     &                 'low set pt',IER)
                     CALL EGETWR(HOLD,K,cmgflow(3),0.,0.,'-',
     &                 'mid set pt',IER)
                     CALL EGETWR(HOLD,K,cmgflow(4),0.,0.,'-',
     &                 'high set pt',IER)
                     if(ier.ne.0)goto 55
                     cmgflow(5)= 0.1; cmgflow(6)= 1.2; cmgflow(7)= 2.
 157                 WRITE(HOLD,'(3f8.3)')cmgflow(5),cmgflow(6),
     &                 cmgflow(7)
                     CALL EASKS(HOLD,' ',
     &               ' Ratios of rate or area for: low, mid, high?',
     &               32,' 0.1 1.0 2.0 ',' flow range controller',
     &               IER,nbhelp)
                     K=0
                     CALL EGETWR(HOLD,K,cmgflow(5),0.,0.,'-',
     &                 'low ratio',IER)
                     CALL EGETWR(HOLD,K,cmgflow(6),0.,0.,'-',
     &                 'mid ratio',IER)
                     CALL EGETWR(HOLD,K,cmgflow(7),0.,0.,'-',
     &                 'high ratio',IER)
                     if(ier.ne.0)goto 157

                   elseif(IW.eq.6)then   ! set room and ambient temperature ranges
                                         ! later logic fills in cmgflow(5) & cmgflow(13) node number
                     igflowtype=3         ! multi-sensor, normally closed with 4 sensors
                     cmgflow(1)= 34; cmgflow(2)= 0; cmgflow(3)= 4
                     cmgflow(4)= -4; cmgflow(5)=-99; cmgflow(6)=0     ! 1st node info
                     cmgflow(7)= 0; cmgflow(8)= 21; cmgflow(9)= 1
                     cmgflow(10)= 0; cmgflow(11)= 1
                     cmgflow(12)= -4; cmgflow(13)=-99; cmgflow(14)=0  ! 2nd node info
                     cmgflow(15)= 0; cmgflow(16)= 24; cmgflow(17)= -1
                     cmgflow(18)= 0;  cmgflow(19)= 1 
                     cmgflow(20)= -3; cmgflow(21)= 0; cmgflow(22)= 0  ! 3rd node (ambient)
                     cmgflow(23)= 0;  cmgflow(24)= 15; cmgflow(25)= 1
                     cmgflow(26)= 0;  cmgflow(27)= 1
                     cmgflow(28)= -3; cmgflow(29)= 0;  cmgflow(30)= 0 ! 4th node (ambient)
                     cmgflow(31)= 0;  cmgflow(32)= 25; cmgflow(33)= -1
                     cmgflow(34)= 0;  cmgflow(35)= 1
                     write(hold,'(2f6.2)')  cmgflow(8),cmgflow(16)  ! confirm room range
                     call easks(HOLD,
     &                 'Room temperature range for natural vent.',
     &                 'Open above & close below?',32,
     &                 ' 21.0  24.0 ','room nat vent range',IER,nbhelp)
                     K=0
                     CALL EGETWR(HOLD,K,cmgflow(8),0.,99.,'W',
     &                 'room open above',IER)
                     CALL EGETWR(HOLD,K,cmgflow(16),-99.,99.,'W',
     &                 'room close below',IER)
                     write(hold,'(2f6.2)')  cmgflow(24),cmgflow(32) ! confirm ambient range
                     call easks(HOLD,
     &                 'Ambient temperature range for natural vent.',
     &                 'Open above & close below?',32,
     &                 ' 21.0  24.0 ','amb nat vent range',IER,nbhelp)
                     K=0
                     CALL EGETWR(HOLD,K,cmgflow(24),0.,99.,'W',
     &                 'ambient open above',IER)
                     CALL EGETWR(HOLD,K,cmgflow(32),-99.,99.,'W',
     &                 'ambient close below',IER)
                   elseif(IW.eq.7)then   ! remember that no control requested
                     igflowtype= -1
                   endif
                endif

C Build flow network from zone and surface attributes.
                call scan_flow_use()
                IAIRN=3
                write(outs,'(2a)') '!!In next dialog use:',
     &            NWKNAM(1:lnblnk(NWKNAM))
                call edisp(iuout,outs)
                write(LAPROB,'(a)') NWKNAM(1:lnblnk(NWKNAM))  ! update LAPROB.
                call edisp(iuout,
     &            'Updating configuration flow links ...')
                CALL EMKCFG('-',IER)
                call edisp(iuout,
     &            'Updating configuration flow links ...done.')
                call MFPROB(IER)   ! browse/edit the network.
                goto 3
              else
                goto 3
              endif
            endif
          elseif(IAIRN.eq.1.or.IAIRN.eq.3)then
            call MFPROB(IER)
            goto 3
          elseif(IAIRN.eq.2)then
            continue
          endif
        elseif(IW.eq.4.and.XSTL)then

C There was a legacy file and the user wants to work with it.
           IAIRN=1   ! user working with legacy formats
           LAPROB=LEGNWKNAM
           call MFPROB(IER)
           goto 3   ! jump back

        elseif((IW.eq.4.and.(.NOT.XSTL)).or.
     &         (IW.EQ.5.and.XSTL))then airflowtype

C User asked for a new flow network, confirm their approach.
C Look and see if any surfaces have USE attributes. If so
C ask user if they want to scan otherwise ask user text or graphic.
          haveflowattributes=0
          do loop=1,NCON
            iz=IC1(loop); is=IE1(loop)  ! get current zone & surface.
            call isuseflowrelated(iz,is,isrelated)
            if(isrelated)then
              haveflowattributes=haveflowattributes+1
            endif
          enddo
          iacc=2
          if(haveflowattributes.gt.2)then
            call edisp(iuout,
     &        'There are surfaces with mass flow USE attributes.')
            call edisp(iuout,
     &        'These can be used to help building your network.')
            iacc=3
            CALL EASKABCD(' ','Descriptive method to employ:',
     &        'menus & lists','graphics tool',
     &        'from scan of surface USE attributes',
     &        'cancel',iacc,nbhelp)
          else
            CALL EASKABC(' ','Descriptive method to employ:',
     &        'menus & lists','graphics tool','cancel',iacc,nbhelp)
            if(iacc.eq.3) goto 3  ! jump back
          endif
          if(iacc.eq.1)then

C There might already be an existing LAPROB, the new name must
C not clash so make a  *m.afn variant.
            if(LAPROB(1:2).eq.'  '.or.LAPROB(1:4).eq.'UNKN')then
              IAIRN=1
              LAPROB='UNKN'
            else
              lenlfil=lnblnk(LAPROB)
              if(LAPROB(lenlfil-3:lenlfil).eq.'.afn')then
                if(IAIRN.eq.1)then
                  write(ltmp,'(2a)',iostat=ios,err=13)
     &              LAPROB(1:lenlfil-4),'m.afn'
                elseif(IAIRN.eq.3)then
                  write(ltmp,'(2a)',iostat=ios,err=13)
     &              LAPROB(1:lenlfil-6),'m.3dafn'
                endif
                LAPROB=ltmp
              endif
            endif
            call MFPROB(IER)
            goto 3   ! jump back after defining network
          elseif(iacc.eq.2)then

C There might already be an existing LAPROB, the new name must
C not clash with it so make a *g.afn variant.
            if(LAPROB(1:2).eq.'  '.or.LAPROB(1:4).eq.'UNKN')then
              IAIRN=2
              LAPROB='UNKN'
            else
              lenlfil=lnblnk(LAPROB)
              if(LAPROB(lenlfil-3:lenlfil).eq.'.afn')then
                write(ltmp,'(2a)',iostat=ios,err=13)
     &            LAPROB(1:lenlfil-4),'g.afn'
                LAPROB=ltmp
              endif
            endif
          else

C Build flow network from zone and surface attributes.
            call scan_flow_use()
            IAIRN=3
            write(outs,'(2a)') '!!In next dialog use:',
     &        NWKNAM(1:lnblnk(NWKNAM))
            call edisp(iuout,outs)
            write(LAPROB,'(a)') NWKNAM(1:lnblnk(NWKNAM))  ! update LAPROB
            call edisp(iuout,
     &        'Updating configuration flow links ...')
            CALL EMKCFG('-',IER)
            call edisp(iuout,
     &        'Updating configuration flow links ...done.')
            call MFPROB(IER)  ! and now browse/edit the network
            goto 3   ! jump back
          endif
        ELSEIF(IW.EQ.6)THEN airflowtype
          GOTO 3     ! user cancelled
        END IF airflowtype

C If LAPROB not known or a new file requested set up graphics network flow file based
C on cfgroot and place it in the netpth folder (differentiate between Unix and non-Unix
C operting systems) and generate matching name for the summary file that enet uses.
        if(LAPROB(1:2).eq.'  '.or.LAPROB(1:4).eq.'UNKN')then
          if(unixok)then
            if(netpth(1:2).eq.'  '.or.netpth(1:2).eq.'./')then
              WRITE(LAPROB,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.afn'
              WRITE(ltmp,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.summary'
            else
              WRITE(LAPROB,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &          cfgroot(1:lnblnk(cfgroot)),'.afn'
              WRITE(ltmp,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &          cfgroot(1:lnblnk(cfgroot)),'.summary'
            endif
          else
            if(netpth(1:2).eq.'  '.or.(ichar(netpth(1:1)).eq.46.and.
     &         ichar(netpth(2:2)).eq.92))then
              WRITE(LAPROB,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.afn'
              WRITE(ltmp,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.summary'
            else
              WRITE(LAPROB,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &          cfgroot(1:lnblnk(cfgroot)),'.afn'
              WRITE(ltmp,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &          cfgroot(1:lnblnk(cfgroot)),'.summary'
            endif
          endif
C          write(6,*) 'LAPROB ',LAPROB(1:lnblnk(LAPROB))
C          write(6,*) 'ltmp ',ltmp(1:lnblnk(ltmp))
        else

C Non-blank name. Set a matching .summary file.
          lenlfil=lnblnk(LAPROB)
          if(LAPROB(lenlfil-3:lenlfil).eq.'.gnf')then
            write(ltmp,'(2a)',iostat=ios,err=13)
     &        LAPROB(1:lenlfil-3),'summary'
          elseif(LAPROB(lenlfil-3:lenlfil).eq.'.afn')then
            write(ltmp,'(2a)',iostat=ios,err=13)
     &        LAPROB(1:lenlfil-3),'summary'
          else
            write(ltmp,'(2a)',iostat=ios,err=13)
     &        LAPROB(1:lenlfil),'.summary'
          endif
        endif

C Graphics network. Create a summary file of zone domain information
C for `enet` to use.
        call tstamp('>','PRJ: export to graphic agent')
        t144='  '
        CALL ERPRCDB(t144,0,3,IER)
        if (ier.ne.0) THEN
          CALL EDISP(IUOUT,
     &      'Error opening pressure coefficients db!.')
          call edisp(iuout,' ')
          goto 3
        endif

C Write to the .summary file.
        write(currentfile,'(a)') ltmp(1:lnblnk(ltmp))
        CALL EFOPSEQ(42,ltmp,3,IER)
        tab=','
        call dstamp(dstmp)
        write(42,'(3a)',iostat=ios,err=13) '*Synopsis',tab,
     &    'for_flow'
        write(42,'(3a)',iostat=ios,err=13) '*Date',tab,dstmp
        write(42,'(3A)',iostat=ios,err=13) '*cfg',tab,
     &    LCFGF(1:lnblnk(LCFGF))
        write(42,'(3A)',iostat=ios,err=13) '*afn',tab,
     &    LAPROB(1:lnblnk(LAPROB))
        write(42,'(2a,i3)',iostat=ios,err=13) '*Pressures',tab,
     &    NPRE
        do 201 IM=1,NPRE
          write(42,'(a)') DEPRE(IM)
  201   continue
        write(42,'(a)',iostat=ios,err=13) '*End_Pressures'
        write(42,'(2a,i3,a)',iostat=ios,err=13) '*Zones',tab,
     &    NCOMP,' # name, volume, centre @ XYZ'
        do 97 IZ=1,NCOMP
          write(outsn,'(a,1x,f7.1,3f8.3)') 
     &      zname(IZ),VOL(IZ),ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3)
          call SDELIM(outsn,outsd,'C',IW)
          write(42,'(a)') outsd(1:lnblnk(outsd))
  97    continue
        write(42,'(a)',iostat=ios,err=13) '*End_Zones'

C Write surface attributes of interest. Find lowest
C and highest point and centre of gravity of surface.
        write(42,'(2a,i4,2a)',iostat=ios,err=13) '*Surfaces',tab,NCON,
     &    ' # connection, description, area, azimuth, elevation, ',
     &    ' Z Min/COG/Max'
        DO 930 IZ = 1,NCOMP
          call georead(IUF,LGEOM(IZ),IZ,1,iuout,IER)
          DO 931 IS = 1,NSUR
            szlow= 1000.0
            szcog= 0.0
            szhigh= -1000.0
            do 932 IV = 1, NVER(is)
              if(Z(JVN(is,iv)).lt.szlow) szlow = Z(JVN(is,iv))
              if(Z(JVN(is,iv)).gt.szhigh) szhigh = Z(JVN(is,iv))
              szcog = szcog + Z(JVN(is,iv))
  932       continue
            szcog = szcog / real(nver(is))
            icc=IZSTOCN(IZ,IS)
            CALL CONXMENU(icc,CXITM)
            write(outsn,'(a,1x,a,1x,6f9.3)',iostat=ios,err=13) 
     &        zname(IZ),SNAME(IZ,IS),SNA(IZ,IS),SPAZI(IZ,IS),
     &        SPELV(iz,is),szlow,szcog,szhigh
            call SDELIM(outsn,outsd,'C',IW)
            write(42,'(3a)',iostat=ios,err=13)
     &        CXITM(1:lnblnk(CXITM)),tab,outsd(1:lnblnk(outsd))
 931      continue
 930    continue
        write(42,'(a)',iostat=ios,err=13) '*End_Surfaces'
        CALL ERPFREE(42,ios)

C Check if graphics file exists. If not, create a minimal file for
C a flow domain.
        XST=.false.
        call FINDFIL(LAPROB,XST)
        if(.not.xst) then
          call usrmsg('Creating a new minimal graphic network',
     &      'file for flow.','P')
          write(NWKDSC,'(2a)') 'Flow network for ',
     &      cfgroot(1:lnblnk(cfgroot))
          INWKTYP=2
          NWKTYPSTR(INWKTYP)='Flow'
          ICONDBFL=DICONDBFL  ! set default icon file.
          IVIEW=1
          VIEWCEN(1)=5.0; VIEWCEN(2)=5.0; VIEWCEN(3)=3.0
          GRSPC(1)=0.5; GRSPC(2)=0.5; GRSPC(3)=1.0
          SON=.true.
          GON=.true.
          NNICN=0; NICNN=0
          IM=1
          call NETWRITE(IM)
        else

C The file exists.
          continue
        endif

C Append path to new command line depending on whether Unix or not.
        call isunix(unixok)
        if(unixok)then
          call addpath(LAPROB,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any '/' to '\'.
          call addpath(LAPROB,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif

C Run the `enet` module and pass it the graphics network file.
        write(doit,'(2a)')'enet -mode graphic -file ',
     &    longtfile(1:lnblnk(longtfile))
        call edisp(iuout,doit)
        call runit(doit,'-')

C On return from enet display a message
        call edisp(iuout,'You will be asked to confirm the names of')
        call edisp(iuout,'the text and graphics flow network files')
        call edisp(iuout,'produced by the `enet` program.')
        call edisp(iuout,' ')
        if(IPRODB.eq.IFIL+6)then
          IUM=IPRODB
        else
          IUM=IFIL+6
        endif

C Free file unit IUM, initiate network flow commons, read the graphic
C network file, convert into flow network commons and list out those commons.
        CALL ERPFREE(IUM,ISTAT)
        CALL MFCDAT
        CALL NETREAD(IUM,'R',IER)
        if(ier.ne.0)then
C         write(6,*) 'error state on return from enet ',ier
        endif
        CALL NETTOFLW(ier)
        if(ier.ne.0)then
C          write(6,*) 'error state on filling common blocks ',ier
        endif
        call mflist(iuout,'s')

C << TODO: look at the attributes of the nodes to see
C << if there is an associated zone.

C Link nodes and zones.
        DO 33 IZ=1,NCOMP
          if(ICAAS(IZ).gt.0)then
            write(outs,'(3A)')zname(IZ)(1:lnzname(IZ)),
     &        ' is currently linked to: ',NDNAM(ICAAS(IZ))
            CALL EASKABC(outs,' ','ok',
     &        'select another','free link',iacc,nbhelp)
            if(iacc.eq.1)then
              goto 33
            elseif(iacc.eq.2)then
              IC=0
              call ASKRNOD('available nodes','-',IC,IER)
              ICAAS(IZ)=IC
            else
              ICAAS(IZ)=0
            endif
          else
            write(outs,'(2A)') zname(IZ)(1:lnzname(IZ)),
     &        ' has no mass flow node.'
            CALL EASKAB(outs,' ','ok','select a node',iacc,nbhelp)
            if(iacc.eq.1)then
              ICAAS(IZ)=0
            elseif(iacc.eq.2)then
              IC=0
              call ASKRNOD('available nodes','-',IC,IER)
              ICAAS(IZ)=IC
            endif
          endif
  33    CONTINUE

C Save the configuration file so that connections and graphics flow
C network are known. 
        CALL EASKOK('Save flow network node links to zones',
     &    'in the model configuration file?',OK,nbhelp)
        IF(OK)then
          IAIRN=2
          call edisp(iuout,'Updating configuration flow links...')
          CALL EMKCFG('-',IER)
          call edisp(iuout,'Updating configuration flow links...done.')
          call edisp(iuout,' ')
        endif

C Electrical network description.
      ELSEIF(INO.EQ.10)THEN
        call tstamp('>','PRJ: entering electrical network')
        call ELECNET

C Contaminant model description.
      ELSEIF(INO.EQ.11)THEN
        call tstamp('>','PRJ: entering contaminant model')
        call CTPROB

C Zone control description.
      ELSEIF(INO.EQ.13)THEN
        call tstamp('>','PRJ: entering zone control')
        icfoc=0
        CALL CONTRL(ITRC,ITRU,icfoc,IER)
        CTLOK=.TRUE.

C Plant control description.
      ELSEIF(INO.EQ.14)THEN
        call tstamp('>','PRJ: entering plant control')
        icfoc=1
        CALL CONTRL(ITRC,ITRU,icfoc,IER)
        CTLOK=.TRUE.

C Flow control description.
      ELSEIF(INO.EQ.15)THEN
        call tstamp('>','PRJ: entering flow control')
        icfoc=2
        CALL CONTRL(ITRC,ITRU,icfoc,IER)
        CTLOK=.TRUE.

C Optical control description.
      ELSEIF(INO.EQ.16)THEN
        call tstamp('>','PRJ: entering optics control')
        icfoc=5
        CALL CONTRL(ITRC,ITRU,icfoc,IER)
        CTLOK=.TRUE.

C Global control description.
      ELSEIF(INO.EQ.17)THEN
        call tstamp('>','PRJ: entering global control')
        icfoc=3
        CALL CONTRL(ITRC,ITRU,icfoc,IER)
        CTLOK=.TRUE.

C Complex fenestration control description.
      ELSEIF(INO.EQ.18)THEN
        call tstamp('>','PRJ: entering complex fen. control')
        icfoc=6
        CALL CONTRL(ITRC,ITRU,icfoc,IER)
        CTLOK=.TRUE.

C FMI.
      ELSEIF(INO.EQ.19)THEN
        call tstamp('>','PRJ: entering FMI specification')
        call FMI_ED(IER)

C Call uncertainty definition controller.
      ELSEIF(INO.EQ.21)THEN
C        helptopic='password_is_needed'
C        call gethelptext(helpinsub,helptopic,nbhelp)
C        call easki(IPASS,' ','Password?',
C     &    0,'-',0,'-',0,'password',IER,nbhelp)
C        if (IPASS.eq.101) then
          call tstamp('>','PRJ: entering uncertainty control')
          call UNCERTA()
C        endif

C Select visualisation option.
      ELSEIF(INO.EQ.23)THEN
        IF(CFGOK)THEN
          helptopic='model_visual_options'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABC(' ','Visualisation options:','hidden line',
     &      'colour rendered','cancel',IW,nbhelp)
          if(IW.eq.1)then
            call visualz('V',ier)
          elseif(IW.eq.2)then
            call visualz('R',ier)
          elseif(IW.eq.3)then
            goto 3
          endif
        ELSE
          CALL USRMSG(' ','Please define your model first!','W')
        ENDIF
        goto 3

C Call simulation controller.
      ELSEIF(INO.EQ.24)THEN
        call tstamp('>','PRJ: entering simulation setup')
        call simula(ier)

C Call results analysis module.        
      ELSEIF(INO.EQ.25)THEN

C Logic to react when browsing a model. If remote set to 'true' then
C the results file should be placed in the user's home folder.
        remote=.false.
        if(browse)then
          remote=.true.
        else

C Check the configuration file name and if concat is 'true' then
C prj was started in somewhere other than the cfg folder so
C results files should be saved to the user's home folder.
C (Note: logic in esrubps/reslib.F should be similar, but it
C makes use of calls to dealremote.)
          ltmp = LCFGF
          call isunix(unixok)
          if(unixok)then
            call addpath(ltmp,longtfile,concat)
          else

C If running on a non-unix machine see if there are spaces in the name
C and change any '/' to '\'.
            call addpath(ltmp,longtfile,concat)
            call cmdfiledos(longtfile,longtfiledos,ier)
            longtfile=' '
            longtfile=longtfiledos
          endif
          if(concat)then
            remote=.true.
          endif
        endif

C Set folder separator (fs) to \ or / as required.
        call isunix(unixok)
        if(unixok)then
          fs = char(47)
        else
          fs = char(92)
        endif

C If there are simulation parameter sets, use the names of the results
C files splres or sblres and prepend user home folder if remote is 'true'.
        if(nsset.gt.0)then

C Assume same simulation parameter set as the last simulation. 
C If isset not instantiated then set = 1.
          if(isset.eq.0)isset=1
          if(INDCFG.eq.2)then
            if(splres(isset)(1:2).ne.'  '.and.
     &         splres(isset)(1:4).ne.'UNKN')then
              ltrf = lnblnk(splres(isset))
              write(tfile,'(a)') splres(isset)(1:ltrf)
              write(longtfile,'(a)') splres(isset)(1:ltrf)
              if(remote)then
                call fdroot(tfile,ltpath,filen)
                call isunix(unixok)
                if(unixok)then
                  if (ICHAR(ltpath(1:1)).ne.47) then
                    write(longtfile,'(3a)') upath(1:lnblnk(upath)),fs,
     &                filen(1:lnblnk(filen))
                  endif
                else
                  if (ltpath(2:2).ne.':') then
                    write(longtfile,'(3a)') upath(1:lnblnk(upath)),fs,
     &                filen(1:lnblnk(filen))
                  endif
                endif
              endif
              havefile=.true.
            else
              tfile=' '
              havefile=.false.
            endif

          else
            if(sblres(isset)(1:2).ne.'  '.and.
     &         sblres(isset)(1:4).ne.'UNKN')then
              ltrf = lnblnk(sblres(isset))
              write(tfile,'(a)') sblres(isset)(1:ltrf)
              write(longtfile,'(a)') sblres(isset)(1:ltrf)
              if(remote)then
                call fdroot(tfile,ltpath,filen)
                call isunix(unixok)
                if(unixok)then
                  if (ICHAR(ltpath(1:1)).ne.47) then
                    write(longtfile,'(3a)') upath(1:lnblnk(upath)),fs,
     &                filen(1:lnblnk(filen))
                  endif
                else
                  if (ltpath(2:2).ne.':') then
                    write(longtfile,'(3a)') upath(1:lnblnk(upath)),fs,
     &                filen(1:lnblnk(filen))
                  endif
                endif
              endif
              havefile=.true.
            else
              tfile=' '
              havefile=.false.
            endif

          endif
        else

C There are no simulation parameter sets so take a guess at the
C file name that the user might have given.
C << Update this logic to use a default name based on the root name. >>
C          if(browse)then
C            write(longtfile,'(3a)') upath(1:lnblnk(upath)),fs,'libb'
C          else
            tfile='libb'
C          endif

        endif

        call tstamp('>','PRJ: beginning res')
        doit = ' '
        call terminalmode(childterminal,tmode)

C If windows look for spaces and/or forward slashes.
        if(unixok)then
          continue
        else
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif

C If prj initial size is a % of default, pass this to a child process with
C an offset from the prj start position.
        if(iappw.eq.690)then
          iappwpc=100
        else
          iappwpc=nint(100.0*(real(iappw)/690.0))  ! reconstitute %
        endif
        if(iappwpc.gt.0.and.iappwpc.le.200)then
          if(havefile)then
            write(doit,'(3a,3i4,3a)') 'res -mode ',tmode,
     &        ' -s ',iappwpc,iappx+35,iappy+45,' -file ',
     &        longtfile(1:lnblnk(longtfile)),' & '
          else
            write(doit,'(3a,3i4,a)') 'res -mode ',tmode,
     &        ' -s ',iappw,iappx+35,iappy+45,' & '
          endif
        else
          if(havefile)then
            write(doit,'(5a)') 'res -mode ',tmode,' -s 0 0 0 -file ',
     &        longtfile(1:lnblnk(longtfile)),' & '
          else
            write(doit,'(3a)') 'res -mode ',tmode,' -s 0 0 0 & '
          endif
          call edisp(iuout,doit)
        endif
C        call usrmsg('Starting assessment recovery via',doit,'-')
        if(tmode(1:7).eq.'graphic')then ! use '-' to request execute_command_line
          call runit(doit,'-')
        else
          call runit(doit,tmode)
        endif

C Execute editor and report generation tools. 
      ELSEIF(INO.EQ.26)THEN
        
        call tstamp('>','PRJ: enter report controller')
        silent= .false.
        
C Start with assumption that all zones in the model are to be included.
        inpic=NCOMP
        do 29 iz=1,inpic
          ivals(iz)=iz
  29    continue

C Ask about Markdown or plain text. Other subroutines will check
C the common block value of markdown. Also see if [pandoc] has
C been installed on this computer.
        call isunix(unixok)

C Check if xfig exists
        if(unixok)then
          found_pandoc=.false.; hold32='pandoc'
          call isinstalled(hold32,found_pandoc)
          if(.NOT.found_pandoc)then
            call usrmsg('You have not yet installed pandoc so markdown',
     &        'files cannot be converted to html docx etc.','W')
          endif
        else
          found_pandoc=.false.  ! not in Windows
        endif

        helptopic='use_markdown'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(' ','Use Markdown format?',
     &      OK,nbhelp)
        if(OK)then
          markdown=.true.
        else
          markdown=.false.
        endif
        call prjqa(inpic,ivals,silent,ier)

C If markdown true reset after report generated.
        if(markdown)then
          markdown=.false.
          if(found_pandoc)then
            call edisp(iuout,
     &        'You can convert the markdown file you generated via:')
            call edisp(iuout,'pandoc -s -o file.html file.md')
            call edisp(iuout,'or pandoc -s -o file.docx file.md')
          endif
        endif

C Calibration.
      ELSEIF(INO.EQ.27)THEN
        call CALIB(ITRC,ITRU,IER)
        goto 3
      ELSE
        GOTO 3
      ENDIF
      GOTO 3

C Error messages.
   13 if(IOS.eq.2)then
        CALL USRMSG(' No permission to write ',ltmp,'W')
      else
        CALL USRMSG(' File write error in ',ltmp,'W')
      endif
      GOTO 3

      END

C ************* EDZCOMP 
C Control editing of zone composition ITRU unit number for user 
C output, IER=0 indicates no error.

      SUBROUTINE EDZCOMP(ITRC,ITRU,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "schedule.h"
#include "prj3dv.h"
#include "cfd.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN,IEOUT
      integer childterminal  ! picks up mmod from starting of prj
      common/childt/childterminal
      common/gzonpik/izgfoc,nzg,nznog(mcom)
     
      integer ncomp,ncon
      common/C1/NCOMP,NCON

      common/appw/iappw,iappx,iappy
      integer iappw,iappx,iappy

      common/GR3D100/BLDG3D,ZONE3D(MCOM)
      LOGICAL BLDG3D,ZONE3D
      common/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)
      common/MOIST01/MSTROK,MSTRZN(MCOM)
      LOGICAL MSTROK,MSTRZN
      common/MOIST02/LMOIST(MCOM)
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      COMMON/ICFNOD/ICFD,ICP
      INTEGER NCFDND,ICFDND,NCONF,ICFD,ICP

      common/shad0/ISIcalc,icalcD,icalcM
      integer ISIcalc,icalcD,icalcM

C 3D visualisation mode.
      COMMON/MODVIS/IVISMOD

      logical OK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,context,CFCDBOK
      logical concat,QUIET
      logical XST,OKZR
      logical clkok
      logical unixok
      logical silent
      integer iappwpc ! application height as % of nominal size.

C Flag noting whether casual gain periods are currently sorted.
      logical sorted,problem
      
      character ITEMS*34
      integer ivals
      dimension ITEMS(27),IVALS(MCOM)
      integer iznumber        ! how many zones in the list
      dimension izclist(MCOM) ! list of zones

      character*72 LCFD
      character*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,LMOIST
      character SFIL*72
      character doit*248,tmode*8,DSFIL*72,ZN*12
      character outs*124,outs248*248
      character longtfile*144
      character HOLD*32
      character lworking*144
      CHARACTER ETEXT*82
      character fs*1

      integer iglib      ! 1 X11, 2 GTK, 3 text.
      integer itru,ier   ! type for passed parameters
      real val,x1,y1     ! type for passed parameters
      integer inpic      ! type for passed parameters
      integer iuf        ! file unit type
      integer IRT        ! for radio buttons
      integer nitems,INO ! max items and current menu item
      integer ianother   ! flag for jumping to prior or next zone
      integer ISTRW

#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont   ! passed to viewtext
#endif

      helpinsub='edcfg'     ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Begin with high level menu.
      IUF=IFIL+2
    3 INO=-3
      IER=0
      WRITE(ITEMS(1),'(A,I3,A)') ' .... Zones (',NCOMP,' defined) ....'
      ITEMS(2)   ='a geometry & attribution        '
      ITEMS(3)   ='b construction materials        '
      ITEMS(4)   ='c operational details           '
      ITEMS(5)   =' _____________________________  '
      WRITE(ITEMS(6),'(A,I4,A)') ' ... Topology (',NCON,
     &                           ' connects) ...'
      ITEMS(7)   ='d surface connections           '
      ITEMS(8)   ='e anchors points                '
      ITEMS(9)   =' _____________________________  '
      ITEMS(10)  =' .... Options ....              ' 
      ITEMS(11)  ='f shading & insolation          '   
      ITEMS(12)  ='g convection coefficients       ' 
      ITEMS(13)  ='h view factors & radiant sensors'
      ITEMS(14)  ='i lighting casual gain control  '
      ITEMS(15)  ='j computational fluid dynamics  '
      ITEMS(16)  ='k adaptive gridding & moisture  '
      ITEMS(17)  ='l occupant agents               '
      ITEMS(18)  =' _____________________________  '    
      ITEMS(19)  =' .... Special components ....   '
      ITEMS(20)  ='m integrated renewables         '
      ITEMS(21)  ='n active materials              '
      ITEMS(22)  ='o optical properties            '
      ITEMS(23)  =' _____________________________  '
      ITEMS(24)  ='! zone grouping                 '
      ITEMS(25)  ='* global tasks                  '
      ITEMS(26)  ='? help                          '
      ITEMS(27)  ='- exit menu                     '
      nitems=27

C If user has defined model and perhaps resized the display then
C redraw the model image.
      if(CFGOK.AND.MODIFYVIEW)then
        MODBND=.TRUE.
        MODLEN=.TRUE.
        ITSNM=1
        ITVNO=1
        nzg=NCOMP
        if(nzg.gt.0)then
          DO 44 I=1,nzg
            nznog(I)=I
  44      CONTINUE

C (Re)Set all surfaces to standard line width.
          CALL INLNST(1)
          izgfoc=0
          CALL redraw(IER)
        endif
        if(MMOD.eq.8)then
          call redrawbuttons()
          WRITE(etext,'(2A)')'Model: ',modeltitle(1:lnblnk(modeltitle))
          iside=1; isize=1; ifont=1
          call viewtext(etext,iside,isize,ifont)
        endif
      endif

C Help text.
      helptopic='zone_composition_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Present menu.
      CALL EMENU('Building composition',ITEMS,nitems,INO)

C Exit to calling menu and clear dialogue box.
      IF(INO.EQ.nitems)THEN
        CALL USRMSG(' ',' ','-')
        RETURN

C Menu help.
      elseif(INO.EQ.nitems-1)THEN
        CALL PHELPD('zones definition',32,'-',0,0,IER)

C Global transforms, rotations and attributions.
      elseif(INO.EQ.nitems-2)THEN
        iglib = igraphiclib()  ! establish if X11, GTK or text only.
        IRT=1
        if(iglib.eq.2)then
          call EASKATOG(' ','Global task:',
     &      'transform','rotate','scale','attribute',
     &      'search & replace',' ',' ',IRT,nbhelp)
        else
          call EASKATOG(' ','Global task:',
     &      'transform','rotate','scale','attribute',
     &      'search & replace','cancel',' ',IRT,nbhelp)
        endif
        if(IRT.eq.-3)then
          goto 3
        elseif(IRT.eq.6)then
          goto 3
        elseif(IRT.eq.1)then

C Ask for transform distance and affected zones then apply.
          hold = ' 0.0  0.0  0.0'
  43      if(iglib.eq.2)then
            CALL EASKS(HOLD,' ','X, Y and Z transform (m)?',
     &        32,' 0.0  0.0   0.0 ','transforms XYZ',ISER,nbhelp)
            if(iser.eq.-3) goto 3
          else
            clkok=.false.
            ISTRW=32
            CALL EASKSCMD(HOLD,' ','X, Y and Z transform (m)?',
     &        'cancel',clkok,ISTRW,' 0.0 0.0 0.0 ','transforms XYZ',
     &        IER,nbhelp)
            if(clkok) goto 3
          endif
          K=0
          CALL EGETWR(HOLD,K,VALX,-99.,99.,'W','X tr',IER)
          CALL EGETWR(HOLD,K,VALY,-99.,99.,'W','Y tr',IER)
          CALL EGETWR(HOLD,K,VALZ,-99.,99.,'W','Z tr',IER)
          if(ier.ne.0)goto 43
          INPIC=NCOMP
          CALL EPICKS(INPIC,IVALS,' ','Zones to transform?',
     &                12,NCOMP,zname,'zone list',IER,nbhelp)
          IF(INPIC.EQ.0)GOTO 3

C Perform global transform.
          silent= .false.
          call globaltransform(valx,valy,valz,inpic,ivals,
     &      silent,itru,ier) 
          MODIFYVIEW=.TRUE.
          MODBND=.TRUE.

C Rotation degree and choices.
        elseif(IRT.eq.2)then
          VAL=0.
          CALL EASKR(VAL,' ','Rotation degrees (+ve anticlockwise)?',
     &      -359.0,'W',359.0,'W',0.0,'rotation',IER,nbhelp)
          if(VAL.LT.-.01.OR.VAL.GT..01)then
            CALL EASKABC(' ','Rotation point?',
     &        'site origin','specified X & Y','cancel',
     &        IRTP,nbhelp)
            if(IRTP.eq.1)then
              x1=0.
              y1=0.
            elseif(IRTP.EQ.2)THEN
              x1=0.
              CALL EASKR(x1,' ','X point (m)?',
     &          0.0,'-',0.0,'-',0.0,'x point',IER,nbhelp)
              y1=0.
              CALL EASKR(y1,' ','Y point (m)?',
     &          0.0,'-',0.0,'-',0.0,'y point',IER,nbhelp)
            elseif(IRTP.EQ.3)THEN
              goto 3
            endif
            INPIC=NCOMP
            CALL EPICKS(INPIC,IVALS,' ','Zones to rotate?',
     &        12,NCOMP,zname,' zone list',IER,nbhelp)
            IF(INPIC.EQ.0)GOTO 3
            call tstamp('>','PRJ: zone rotation')

C Implement rotations for selected zones.
            silent= .false.
            call globalrotate(val,x1,y1,inpic,ivals,silent,itru,ier)

          endif
          MODIFYVIEW=.TRUE.
          MODBND=.TRUE.

C Determine scaling factor (1.0=100%) and zones then apply.
C Warning given if scale is <0.1 or >10.1. To 'grow' floor area
C but not height keep the Z scale at 1.0.
        elseif(IRT.eq.3)then
          hold = ' 1.0  1.0  1.0'
  45      if(iglib.eq.2)then
            CALL EASKS(HOLD,' ','X Y & Z axis scaling factor?',
     &        32,' 1.0  1.0   1.0 ','scaling XYZ',ISER,nbhelp)
            if(iser.eq.-3) goto 3
          else
            clkok=.false.
            ISTRW=32
            CALL EASKSCMD(HOLD,' ','X Y & Z axis scaling factor?',
     &        'cancel',clkok,ISTRW,' 1.0 1.0 1.0 ','scaling XYZ',
     &        IER,nbhelp)
            if(clkok) goto 3
          endif
          K=0
          CALL EGETWR(HOLD,K,VALX,0.1,10.1,'W','X scale',IER)
          CALL EGETWR(HOLD,K,VALY,0.1,10.1,'W','Y scale',IER)
          CALL EGETWR(HOLD,K,VALZ,0.1,10.1,'W','Z scale',IER)
          if(ier.ne.0)goto 45
          INPIC=NCOMP
          CALL EPICKS(INPIC,IVALS,' ','Zones to scale?',
     &                12,NCOMP,zname,'zone list',IER,nbhelp)
          IF(INPIC.EQ.0)GOTO 3

C Perform global scaling.
          silent= .false.
          call globalscale(valx,valy,valz,inpic,ivals,
     &      silent,itru,ier) 
          MODIFYVIEW=.TRUE.
          MODBND=.TRUE.

C Search and replace an attribute.
        elseif(IRT.eq.4)then
          call serchrpl('a',itrc,iier)
          if(iier.eq.0)then
            call usrmsg(' ','Surface attribute updated.','W')
          else
            call usrmsg(' ','Surface attribute update failed.','W')
          endif
          goto 3

C Search and replace a construction.
        elseif(IRT.eq.5)then
          call serchrpl('c',itrc,iier)
          if(iier.eq.0)then
            call usrmsg('Construction attribute search & replace',
     &        'completed.','W')
          else
            call usrmsg('Construction attribute search & replace',
     &        'failed.','W')
          endif

C Re-establish the ssmlcindex array for the model so that 'other-side' changes are
C known when editing the zone geometry. First, scan for matching MLCs for surfaces.
          do 30 ICOMP=1,NCOMP
            call georead(IFIL+1,LGEOM(ICOMP),ICOMP,1,iuout,ier)

C Find the index of the MLC that matches each surface.
            DO 9994 I=1,NZSUR(icomp)
              smlcindex(icomp,i)=0  ! assume no matching MLC          
              lnssmlc=lnblnk(SMLCN(icomp,i))
              do 5 ii=1,nmlc
                if(SMLCN(icomp,i)(1:lnssmlc).eq.
     &             mlcname(ii)(1:lnmlcname(ii))) then
                  smlcindex(icomp,i)=ii   ! remember MLC index     
                endif
  5           continue
 9994       continue
 30       continue
          call usrmsg(
     &    'Surface attributes have been updated. Go to ',
     &    'Zone Composition->construction materials & update zones.',
     &    'W')
          goto 3
        endif

C Zone grouping.
      elseif(INO.EQ.nitems-3)THEN
        call tstamp('>','PRJ: enter groups menu')
        call grouplist(iasel,ier)

C Geometry and attribution. If creating a zone from scratch,
C the file name will be ' ' and IC will become NCOMP+1; otherwise
C read in the existing file data and then call the editing facility.
C IC=-1 flags 'from scratch' in the selection list.
      elseif(INO.EQ.2)THEN
        IC=-1

C Allow user to modify the zone list. If no zones exist
C go directly to creating one.
  248   if(NCOMP.eq.0)then
          IC=1
        else
          CALL EASKGEOF('Select a zone:',CFGOK,IC,'M',34,IER)
        endif

C Return to menu.
        IF(IC.EQ.0.OR.IC.EQ.-1)THEN
          GOTO 3

C If IC < -10 request to delete a zone and recover zone number.
        ELSEIF(IC.lt.-10)THEN
          IC = ABS(IC) - 10
          call DELZONE(ITRC,IC,IER)
          IC = -1
          GOTO 3

C Define a new zone and update nzg in case multiple zones have been read in.
        ELSEIF(IC.EQ.NCOMP+1)THEN
          if(NCOMP.gt.MCOM-1)then
            call usrmsg(' ',
     &      'Adding a zone will exceed the maximum allowed!','W')
            goto 3
          endif
          call NEWZONE(ITRC,IC,IER)
          if(IER.eq.-3)then
            goto 3   ! user cancel
          else
            nzg=NCOMP
          endif

C Copy a zone and get index by decrementing 100.
        ELSEIF(IC.gt.100.and.IC.lt.200)THEN
          if(NCOMP.gt.MCOM-1)then
            call usrmsg(' ',
     &      'Copying a zone will exceed the maximum allowed!','W')
            goto 3
          endif
          IC = IC - 100
          call tstamp('>','PRJ: copy zone')

C Transforms and rotations to apply.
          helptopic='zone_copy_and_ctl'
          call gethelptext(helpinsub,helptopic,nbhelp)
          hold = ' 0.0  0.0  0.0'
          DX=0.0; DY=0.0; DZ=0.0
          VALX=0.0; VALY=0.0; VALZ=0.0
          clkok=.false.
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
 243      if(iglib.eq.2)then
            ISTRW=32
            CALL EASKSCMD(HOLD,' ','X, Y & Z axes transforms (m):',
     &        'skip transform',clkok,ISTRW,' 0. 0. 0.','transforms XYZ',
     &        iser,nbhelp)
            if(clkok)iclkok=2   ! notice skip transform button
            if(iser.eq.-3)then  ! notice GTK cancel button
              goto 3
            endif
          else
            CALL EASKS2CMD(HOLD,' ','X, Y & Z axes transforms (m):',
     &        'cancel copy','omit transform',iclkok,32,' 0. 0. 0.',
     &        'transforms XYZ',iser,nbhelp)
            if(iclkok.eq.1)then ! notice X11 cancel option
              goto 3
            endif
          endif 

C Act on the omit transform request.   
          if(iclkok.eq.2) then
            continue
          else
            K=0
            CALL EGETWR(HOLD,K,VALX,-99.,99.,'W','zone X trnsf',IER)
            CALL EGETWR(HOLD,K,VALY,-99.,99.,'W','zone Y trnsf',IER)
            CALL EGETWR(HOLD,K,VALZ,-99.,99.,'W','zone Z trnsf',IER)
            if(ier.ne.0)goto 243
          endif

C Record rotation information in rotateit array.
          CALL EASKOK(' ','Rotate copied zone?',OKZR,nbhelp)
          if(OKZR)then
            VAL=0.
            CALL EASKR(VAL,' ',
     &        'Rotation degrees (+ve anticlockwise)?',
     &        -359.0,'W',359.0,'W',0.0,'rotation',IER,nbhelp)
          endif

          iznumber=1        ! Setup call to COPYZONE
          izclist(1) = IC
          DX=VALX; DY=VALY; DZ=VALZ; DANG=VAL; IER=0
          call COPYZONE(ITRC,iznumber,izclist,DX,DY,DZ,DANG,IER)

          CALL EMKCFG('-',IER)
          MODIFYVIEW=.TRUE.
          MODBND=.TRUE.
          IC = -1
          GOTO 3

C User asked to copy multiple zones. Create list and call COPYZONE.
        ELSEIF(IC.eq.301)THEN
          inpic=0
          call askmultizone(inpic,ivals,
     &      ' Select zones or group to copy?',
     &      'Zone copy multiple','-',ier)
          if(inpic.gt.0)then

C Transforms and rotations.
            helptopic='zone_copy_and_ctl'
            call gethelptext(helpinsub,helptopic,nbhelp)
            hold = ' 0.0  0.0  0.0'
            DX=0.0; DY=0.0; DZ=0.0
            VALX=0.0; VALY=0.0; VALZ=0.0
            clkok=.false.
            iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
 244        if(iglib.eq.2)then
              ISTRW=32
              CALL EASKSCMD(HOLD,' ','X, Y & Z axes transforms (m):',
     &        'skip transform',clkok,ISTRW,' 0. 0. 0.','transforms XYZ',
     &        iser,nbhelp)
              if(clkok)iclkok=2   ! notice skip transform button
              if(iser.eq.-3)then  ! notice GTK cancel button
                goto 3
              endif
            else
              CALL EASKS2CMD(HOLD,' ','X, Y & Z axes transforms (m):',
     &        'cancel copy','omit transform',iclkok,32,' 0. 0. 0.',
     &        'transforms XYZ',iser,nbhelp)
              if(iclkok.eq.1)then ! notice X11 cancel option
                goto 3
              endif
            endif

C Act on the omit transform request.   
            if(iclkok.eq.2) then
              continue
            else
              K=0
              CALL EGETWR(HOLD,K,VALX,-99.,99.,'W','zone X trnsf',IER)
              CALL EGETWR(HOLD,K,VALY,-99.,99.,'W','zone Y trnsf',IER)
              CALL EGETWR(HOLD,K,VALZ,-99.,99.,'W','zone Z trnsf',IER)
              if(ier.ne.0)goto 244
            endif

C Record rotation information in rotateit array.
            CALL EASKOK(' ','Rotate copied zone(s)?',OKZR,nbhelp)
            if(OKZR)then
              VAL=0.
              CALL EASKR(VAL,' ',
     &          'Rotation degrees (+ve anticlockwise)?',
     &          -359.0,'W',359.0,'W',0.0,'rotation',IER,nbhelp)
            endif
            DX=VALX; DY=VALY; DZ=VALZ; DANG=VAL; IER=0
            call COPYZONE(ITRC,inpic,ivals,DX,DY,DZ,DANG,IER)

            CALL EMKCFG('-',IER)
            MODIFYVIEW=.TRUE.
            MODBND=.TRUE.
            IC = -1
            GOTO 3
          endif

C Scan the geometry file and refresh common blocks.
        ELSEIF(IC.LE.NCOMP)THEN
          call georead(IFIL+1,LGEOM(IC),IC,1,iuout,IER)
          if(IER.NE.0)then
            helptopic='problem_scanning_file'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKOK('Problem reading geometry file!',
     &            'Retry?',OK,nbhelp)
            IF(OK)GOTO 248
          else
            call zgupdate(0,ic,ier)     ! Re-derive values
            call zinfo(ic,zoa,zvol,'q')
          endif

C Trace geometry related information. Because all zones will have
c beeen read in, include the extended surface `context`.
          if(ITRC.ne.0)then
            call ZINFOREP(itru,ic)
            context=.true.
            CALL SURINFO(IC,ITRU,context)
            IF(ITRC.GT.1)CALL VERINFO(IC,ITRU)
            CALL INSINFO(IC,ITRU)
          endif

C Obstructions are alread in common - no need to scan.
        ENDIF

C Present zone editing menu.
        ianother=0
 443    CALL EDZONE(ITRC,IC,ianother,IER)
        MODIFYVIEW=.TRUE.

C If user asked for next or prior zone, reset IC and call again.
        if(ianother.eq.0)then
          continue
        elseif(ianother.lt.0.and.IC.gt.1)then

C Decrement ic and re-scan the geometry file and refresh associated
C common blocks.
          ic=ic-1
          call georead(IFIL+1,LGEOM(IC),IC,1,iuout,IER)
          if(IER.NE.0)then
            helptopic='problem_scanning_file'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKOK('Problem reading geometry file!',
     &            'Retry?',OK,nbhelp)
            IF(OK)GOTO 248
          else
            call zgupdate(0,ic,ier)
            call zinfo(ic,zoa,zvol,'q')
          endif

C Trace geometry related information. Because all zones will have
c beeen read in, include the extended surface `context`.
          if(ITRC.ne.0)then
            call ZINFOREP(itru,ic)
            context=.true.
            CALL SURINFO(IC,ITRU,context)
            IF(ITRC.GT.1)CALL VERINFO(IC,ITRU)
            CALL INSINFO(IC,ITRU)
          endif
          goto 443

C Increment ic and re-scan the geometry file and refresh associated
C common blocks.
        elseif(ianother.gt.0.and.IC.lt.NCOMP)then
          ic=ic+1
          call georead(IFIL+1,LGEOM(IC),IC,1,iuout,IER)
          if(IER.NE.0)then
            helptopic='problem_scanning_file'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKOK('Problem reading geometry file!',
     &            'Retry?',OK,nbhelp)
            IF(OK)GOTO 248
          else
            call zgupdate(0,ic,ier)
            call zinfo(ic,zoa,zvol,'q')
          endif

C Trace geometry related information. Because all zones will have
c beeen read in, include the extended surface `context`.
          if(ITRC.ne.0)then
            call ZINFOREP(itru,ic)
            context=.true.
            CALL SURINFO(IC,ITRU,context)
            IF(ITRC.GT.1)CALL VERINFO(IC,ITRU)
            CALL INSINFO(IC,ITRU)
          endif
           goto 443
        endif

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 248

C Zone construction files. Prior to working with the zones, rescan
C the construction database.
      elseif(INO.EQ.3)THEN
        CALL ERPFREE(IFMUL,ISTAT)
        if(ipathmul.eq.0.or.ipathmul.eq.1)then
          lworking=lfmul  ! use as is
        elseif(ipathmul.eq.2)then
          lndbp=lnblnk(standarddbpath)
          write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &      lfmul(1:lnblnk(lfmul))  ! prepend db folder path
        endif
        call FINDFIL(lworking,XST)

        if(XST)then
          CALL ERMLDB(0,IUOUT,IER)
          IF(IER.eq.4)THEN
            CALL ERMLDB2(0,iuout,IER)
          endif
         else

C Constructions db problem.
          write(outs248,'(3a)') 'Constructions db ',
     &       LFMUL(1:lnblnk(LFMUL)),' not found!'
          call edisp(iuout,' ')
          call edisp248(iuout,outs248,100)
        endif

        IC=-1
 249    CALL EASKGEOF('Select a zone:',CFGOK,IC,'t',34,IER)
        IF(IC.EQ.0.OR.IC.EQ.-1)THEN
          GOTO 3
        ELSEIF(IC.EQ.-2)THEN
          call tstamp('>','PRJ: enter thermophysical')
          CALL THMENU(ITRC)
        ELSEIF(IC.EQ.99)THEN

C Re-write all zones without prompting the user for input unless an 
C error occurs.
          QUIET=.FALSE.
          helptopic='update_all_constructions'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK(' ','Update zone constructions files?',
     &           QUIET,nbhelp)
          if(.NOT.QUIET)goto 3
          call tstamp('>','PRJ: auto update zone construction')
          DO 2491, IZ=1,NCOMP
            CALL EDCON(ITRC,ITRU,IZ,QUIET,IER)
 2491     CONTINUE
          QUIET=.FALSE.
        ELSE
          call tstamp('>','PRJ: enter zone construction')
          imc=ic
          CALL EDCON(ITRC,ITRU,IMC,QUIET,IER)
        ENDIF

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 249

C Zone operations file.
      elseif(INO.EQ.4)THEN
 250    IC=-1
        izdef=0
        call askzone(ic,izdef,'Zone operations',
     &    'o','Zone index for operations.',34,ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)then
          MODIFYVIEW=.TRUE.
          GOTO 3
        endif

C Global tasks.
C << Consider an additional global task of imposing a new infiltration
C    rate on more than one zone possibly via a search and replace function.
C    Also, perhaps take an infiltration or casual gain profile from one zone
C    and apply it to one or more other zones. >>
        if(IC.eq.99)then
          helptopic='update_all_operations'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABC(' ',' Multiple zone options:',
     &      'update old files','check period order',
     &      'cancel',IW,nbhelp)
          if(IW.eq.1.or.IW.eq.2)then
            inpic=NCOMP
            if(inpic.gt.0)then
              call usrmsg(
     &          'Updating model to reflect changes',
     &          'in operation periods ...','-')
              DO 155 IC=1,inpic
                icomp=IC
                call FINDFIL(LPROJ(icomp),XST)
                if(XST)then
                  IUO=IFIL+1
                  CALL ERPFREE(IUO,ISTAT)
                  CALL EROPER(0,iuout,IUO,icomp,IER)
                  if(ip3ver(icomp).eq.0)then

C File is older format and assumed to be unsorted.
                    sorted=.false.
                    call PROCESSOLDCAS(ICOMP,0,iuout)
                    ip3ver(icomp)=21
                    CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,IER)
                  else

C File is current format and might be unsorted. If user is not
C browsing, update the operations file.
                    sorted=.true.
                    problem=.false.
                    DO 400 IDTY=1,NBDAYTYPE
                      call checksort(icomp,IDTY,problem)
                      if(problem)then
                        sorted=.false.
                        call edisp(iuout,' ')
                        call edisp(iuout,
     &                     'Casual gains may be time unsorted.')
                      endif
 400                CONTINUE
                    if(.NOT.sorted)then
                      call PROCESSOLDCAS(ICOMP,0,iuout)
                      ip3ver(icomp)=21
                      CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,IER)
                    endif
                  endif
                endif
 155          continue
              call usrmsg(
     &          'Updating model to reflect changes',
     &          'in operation periods ... done.','-')
              goto 250
            endif
          elseif(IW.eq.3)then
            goto 3
          endif
        endif
        call tstamp('>','PRJ: enter zone operations')

C Set iver to zero to indicate it is being called from some place
C other than the versioning facility.
        iver=0
        CALL PRJFMK(ITRC,ITRU,IUO,IC,IER,iver)

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 250

C Surface connections.
      elseif(INO.EQ.7)THEN
        call tstamp('>','PRJ: enter topology menu')
        CALL EDCONN(IER)

C Anchor points.
      elseif(INO.EQ.8)THEN
        call tstamp('>','PRJ: enter anchors menu')
        call anchlist(iasel,ier)

C Shading/ insolation analysis.
      elseif(INO.EQ.11)THEN

C Calculation type option.
        H(1)='The ESP-r module ish may be invoked pre-simulation to'
        H(2)='determine hourly shading & insolation data for a'
        H(3)='representative day each month and store these data'
        H(4)='in a zone shading/insolation file. This option reduces'
        H(5)='the simulation run-time. Alternatively, the ish methods'
        H(6)='can be accessed throughout a simulation to regenerate'
        H(7)='the data as a simulation proceeds. A third option is'
        H(8)='to impose a prescribed treatment: zero shading and'
        H(9)='the assignment of the solar flux penetrating windows'
        H(10)='to prescribed internal surfaces. This last treatment'
        H(11)='is appropriate at an early design stage when details'
        H(12)='of internal and external obstructions are not known.'
        CALL EASKABCD(' ','Calculation options:','prescribed',
     &           'embedded','monthly file','cancel',IW,12)
        ISIcalc=IW-1 ! dictates the S/I calculation directive in the cfg file

C Prescribed or cancel selected.
        if(IW.eq.1.or.IW.eq.4) goto 3

C Zone selection.
 253    IC=-1
        izdef=0
        call askzone(ic,izdef,'Zone for shading attribution',
     &         's','Zone index for shading.',34,ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3  ! no zone selected

C Global tasks: only available for pre-constructed S/I file case.
        if(IC.eq.99)then
          if(ISIcalc.ne.2)then
            call edisp(iuout,
     &       'Option requires a pre-constructed S/I file!')
            goto 253
          endif
          helptopic='global_shading_tasks'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKATOG(' ','Options for selected zones:',
     &      're-establish missing shading','recalculate shading',
     &      'dereference S/I file','add insolation','cancel',
     &      ' ',' ',IW,nbhelp)
          if(IW.eq.1)then      ! re-establish missing data
            call eddshd('asci')
          elseif(IW.eq.2)then  ! recalculate shading
            call eddshd('site')
          elseif(IW.eq.3)then  ! dereference files
            inpic=0
            call askmultizone(inpic,ivals,
     &        'Zones to dereference?',
     &        'Zone shd/ins dereference','s',ier)
            if(inpic.gt.0)then
              DO 153 IC=1,inpic
                ICOMP=ivals(IC)
                ISI(ICOMP)=0
 153          continue
            endif

C << Note: the add insolation option should cause
C    the zone geometry file to be updated if version 1.1
C    geometry files are used. This is not yet done. >>
         elseif(IW.eq.4)then  ! add insolation
            inpic=0
            call askmultizone(inpic,ivals,
     &        'Select zones to add insolation?',
     &        'Zone add insolation','s',ier)
            if(inpic.gt.0)then
              DO 151 IC=1,inpic
                ICOMP=ivals(IC)
                ISI(ICOMP)=1
                if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                  WRITE(LSHAD(icomp),'(A,A4)')
     &              zname(icomp)(1:lnzname(icomp)),'.shd'
                else
                  WRITE(LSHAD(icomp),'(3A,A4)') 
     &              zonepth(1:lnblnk(zonepth)),'/',
     &              zname(icomp)(1:lnzname(icomp)),'.shd'
                endif

C Create a place-holder file that subsequent calculation
C procedure can overwrite.
                IUF=IFIL+2
                CALL EFOPSEQ(IUF,LSHAD(ICOMP),3,IER)
                WRITE(IUF,'(2a)') 'Placeholder shading file for ',
     &            zname(icomp)(1:lnzname(icomp))
                CALL ERPFREE(IUF,ios)
 151          continue
            endif
            CALL EMKCFG('-',IER)
            call eddshd('site')
          elseif(IW.eq.5)then
            continue
          endif
          IC=-1
          GOTO 253
        endif

C Deal with case of one selected zone.
        write(ZN,'(A)') zname(IC)
        IUF=IFIL+1

C Embedded S/I calculation mode.
        IF(ISIcalc.eq.1)THEN
          ISI(IC)=1
          call edisp(iuout,' ')
          call edisp(iuout,
     &       'Zone selected for embedded S/I analysis.')
          goto 253

C Pre-constructed S/I file case.
        ELSEIF(ISIcalc.eq.2)THEN
          IF(ISI(IC).eq.0)THEN
            if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
              WRITE(SFIL,'(A,A4)')zname(IC)(1:lnzname(IC)),'.shd'
            else
              WRITE(SFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &          zname(IC)(1:lnzname(IC)),'.shd'
            endif
            DSFIL = 'new.shd'
            CALL EASKS(SFIL,' ','Zone shading/insolation file?',
     &           72,DSFIL,'shd/ins file',IER,nbhelp)
            if(SFIL(1:2).ne.'  ')then
              LSHAD(IC)=SFIL
              ISI(IC)=1
              CALL EMKCFG('-',IER)
            endif
          ELSE

C Confirm existing shading file name and offer option to dereference.
            if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
              WRITE(DSFIL,'(A,A4)')zname(IC)(1:lnzname(IC)),'.shd'
            else
              WRITE(DSFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &          zname(IC)(1:lnzname(IC)),'.shd'
            endif
            SFIL = LSHAD(IC)
            clkok=.false.
            ISTRW=72
            CALL EASKSCMD(SFIL,' ','Zone shading/insolation file?',
     &         'dereference',clkok,ISTRW,DSFIL,'shd/ins db',IER,nbhelp)
            if(clkok)then

C Reset common blocks, save configuration and see if another zone is requested.
              LSHAD(IC)='UNKNOWN'
              ISI(IC)=0
              CALL EMKCFG('-',IER)
              IC=-1
              GOTO 253
            else
              if(SFIL(1:2).ne.'  ')then
                LSHAD(IC)=SFIL
                ISI(IC)=1
                CALL EMKCFG('-',IER)
              endif
            endif
          endif

          CALL EASKOK(' ',
     &      'Proceed with shading/insolation analysis?',OK,nbhelp)
          IF(OK)then

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive ish.
            call tstamp('>','PRJ: start shading')
            call comissionish(ic,'in ',ier)
            if(ier.ne.0)then
              call edisp(iuout,' ')
              call edisp(iuout,
     &          'Possible error in shading/insolation calculations.')
            endif
          endif

C Loop back and see if another zone is requested.
          IC=-1
          GOTO 253
        ENDIF

C Zone surface convection coefficients. If file exists, the
C call to hcfmk presents the option to dereference it.
      elseif(INO.EQ.12)THEN
 260    IC=-1
        izdef=0
        call askzone(ic,izdef,
     &    'Zone for convection attribution',
     &    'h','Zone index for hc.',34,ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        IUO=IFIL+1
        call tstamp('>','PRJ: enter hc file')
        CALL HCFMK(ITRU,IUO,IC,IER)
        MODIFYVIEW=.TRUE.

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 260

C View factor file editing, begin by identifying the zone and then
C reading in the geometry file followed by view factor editing 
C facilities. Call to edmrt might result in dereferenceing of
C the file.
      elseif(INO.EQ.13)THEN
 251    IC=-1
        izdef=0
        call askzone(ic,izdef,'Zone for view factor attribution',
     &         'v','Zone index for viewfact.',34,ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        if(IC.eq.99)then

C Global tasks.
          helptopic='area_weighted_or_calc'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABCD(' ','View factor options:',
     &        'area weighted','ray traced',
     &        'dereference file','cancel',IW,nbhelp)
          IUO=IFIL+1
          if(iw.eq.1.or.iw.eq.2)then
            inpic=0
            call askmultizone(inpic,ivals,
     &        'Select zones','Select zones','v',ier)
            if(inpic.gt.0)then
              DO 164 IC=1,inpic
                ICOMP=ivals(IC)
                if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                  WRITE(LVIEW(icomp),'(A,A4)')
     &              zname(icomp)(1:lnzname(icomp)),'.vwf'
                else
                  WRITE(LVIEW(icomp),'(3A,A4)') 
     &              zonepth(1:lnblnk(zonepth)),'/',
     &              zname(icomp)(1:lnzname(icomp)),'.vwf'
                endif
                silent=.true.
                if(iw.eq.1)then
                  call EDMRT(ITRC,ITRU,IUF,ICOMP,silent,'a',IER)
                elseif(iw.eq.2)then
                  call EDMRT(ITRC,ITRU,IUF,ICOMP,silent,'v',IER)
                endif
 164          continue
              CALL EMKCFG('-',IER)
            endif
          elseif(iw.eq.3)then
            inpic=0
            call askmultizone(inpic,ivals,
     &        'Select zones for view factors dereferencing?',
     &        'Zone viewfactor dereference','v',ier)
            if(inpic.gt.0)then
              DO 163 IC=1,inpic
                ICOMP=ivals(IC)
                LVIEW(icomp)=' '
                IVF(ICOMP)=0
 163          continue
              CALL EMKCFG('-',IER)
            endif
          elseif(iw.eq.4)then
            continue
          endif
          IC=-1
          GOTO 251
        endif

C Deal with one zone.
        IUO=IFIL+1
        call tstamp('>','PRJ: enter viewfactors')
        silent=.false.
        CALL EDMRT(ITRC,ITRU,IUO,IC,silent,'v',IER)

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 251

C Casual gain control.
      elseif(INO.EQ.14)THEN
        helptopic='zone_cgc_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
 252    IC=-1
        izdef=0
        call askzone(ic,izdef,'Zone for casual gain control',
     &        'g','Zone index for cgc.',34,ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        IUF=IFIL+1
        if(ICGC(IC).eq.1)then

C Initial user options.
          CALL EASKABC(' ','File options:','edit',
     &      'dereference','cancel',irpt,nbhelp)
          if(irpt.eq.1)then
            IUF=IFIL+1
            call EDCASCTL(ITRC,IC,IER)
          elseif(irpt.eq.2)then
            ICGC(IC)=0
            LCGCIN(IC)='UNKNOWN'
            CALL EMKCFG('-',IER)
          else
            goto 3
          endif
        else
          if(ICGC(IC).eq.0)then
            CALL EASKAB(' ','File options:',
     &        'create','cancel',irpt,nbhelp)
            if(irpt.eq.1)then
              IUF=IFIL+1
              call EDCASCTL(ITRC,IC,IER)
            elseif(irpt.eq.2)then
              goto 252
            endif
          else
            CALL EASKABCD(' ','File options:',
     &        'create new','open exising','dereference','cancel',
     &        irpt,nbhelp)
            if(irpt.eq.1)then
              IUF=IFIL+1
              call EDCASCTL(ITRC,IC,IER)
            elseif(irpt.eq.2)then
              IUF=IFIL+1
              call EDCASCTL(ITRC,IC,IER)
           elseif(irpt.eq.3)then
              ICGC(IC)=0
              LCGCIN(IC)='UNKNOWN'
              CALL EMKCFG('-',IER)
            endif
          endif
        endif

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 252

C CFD domain.
      elseif(INO.EQ.15)THEN
        helptopic='zone_cfd_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('CFD domain + parameters',nbhelp,'-',0,0,IER)

        IC=-1
        izdef=0
        call askzone(ic,izdef,'Zone for CFD attribution',
     &         'd','Zone index for cfd.',34,ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        call georead(IFIL+1,LGEOM(IC),IC,1,iuout,IER)
        call zgupdate(0,ic,ier)     ! Re-derive values
        call zinfo(ic,zoa,zvol,'q')

        IUF=IFIL+1
        call cfdcomp(ic,iuf,ier)
        IVISMOD=1
        MODIFYVIEW=.TRUE.

C Offer CFD domain inclusion option.
        write(outs,'(3a)') 'Include ',LCFD(ic)(1:lnblnk(LCFD(ic))),
     &    ' in the model?'
        CALL EASKOK(' ',outs,OK,nbhelp)
        IF(OK)then
          CALL EMKCFG('-',IER)
          IF(ICFD.GT.NCONF)NCONF=NCONF+1
        else
          IFCFD(IC)=0 
          LCFD(IC)='UNKNOWN'
          CALL EMKCFG('-',IER)
          IF(ICFD.LE.NCONF)NCONF=NCONF-1
        endif

C Adaptive constructional gridding.
      elseif(INO.EQ.16)THEN
        IC=-1
        CALL EASKGEOF('Zone for adaptive gridding',CFGOK,IC,
     &    '-',34,IER)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        write(ZN,'(A)') zname(IC)
        helptopic='zone_adaptive_grid_opt'
        call gethelptext(helpinsub,helptopic,nbhelp)

        IUF=IFIL+1

C Offer creation of moisture transport data file or dereference.
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(SFIL,'(A,A4)')zname(IC)(1:lnzname(IC)),'.mst'
        else
          WRITE(SFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnzname(IC)),'.mst'
        endif
        DSFIL = SFIL
        write(outs,'(3a)')'Moisture transport file for ',
     &    zname(IC)(1:lnzname(IC)),'?'
        clkok=.false.
        if(.NOT.MSTRZN(IC))then

C << What does ignore mean here? >>
          ISTRW=72
          CALL EASKSCMD(SFIL,outs,' ','ignore',clkok,ISTRW,DSFIL,
     &      'moisture file',IER,nbhelp)
          if(clkok) goto 3
        else
          ISTRW=72
          CALL EASKSCMD(SFIL,outs,' ','dereference',clkok,ISTRW,DSFIL,
     &      'moisture file',IER,nbhelp)
          if(clkok)then
            MSTRZN(IC)=.false.
            LMOIST(IC)=' '
            CALL EMKCFG('-',IER)
            IC=-1
            GOTO 3
          endif
        endif
        if(SFIL(1:2).ne.'  ')then
          MSTRZN(IC)=.true.
          LMOIST(IC)=SFIL
          call usrmsg('updating model for moisture transport ...',
     &      ' ','-')
          CALL EMKCFG('-',IER)
          call usrmsg('updating model for moisture transport ... done.',
     &      ' ','-')
        endif

        if(.NOT.ZONE3D(IC))then
          CALL EASKAB(' ','Addaptive gridding options:',
     &      'define files','cancel',IW,nbhelp)
          if(iw.eq.2)then
            IC=-1
            GOTO 3
          endif
        else
          CALL EASKABC(' ','Adaptive gridding options:',
     &      'browse/edit','dereference','cancel',IW,nbhelp)
          if(iw.eq.2)then
            L3DCVS(IC)='UNKNOWN'
            L3DCNC(IC)='UNKNOWN'
            L3DNDC(IC)='UNKNOWN'
            L3DTAQ(IC)='UNKNOWN'
            ZONE3D(IC)=.false.
            call usrmsg('Dereferencing adaptive gridding ...',
     &        ' ','-')
            CALL EMKCFG('-',IER)
            call usrmsg('Dereferencing adaptive gridding ... done.',
     &        ' ','-')
            IC=-1
            GOTO 3
          elseif(iw.eq.3)then
            IC=-1
            GOTO 3
          endif
        endif

C Confirm file names.
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DSFIL,'(A,A4)')zname(IC)(1:lnzname(IC)),'.cvs'
        else
          WRITE(DSFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnzname(IC)),'.cvs'
        endif
        SFIL = L3DCVS(IC)
        CALL EASKS(SFIL,'3D control volumes file?',
     &    ' ',72,DSFIL,'control volumes file',IER,nbhelp)
        if(SFIL(1:2).ne.'  ') L3DCVS(IC)=SFIL

        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DSFIL,'(A,A4)')zname(IC)(1:lnzname(IC)),'.cnc'
        else
          WRITE(DSFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnzname(IC)),'.cnc'
        endif
        SFIL = L3DCNC(IC)
        CALL EASKS(SFIL,'3D control volumes connections file?',
     &    ' ',72,DSFIL,'control volumes connections file',
     &    IER,nbhelp)
        if(SFIL(1:2).ne.'  ') L3DCNC(IC)=SFIL

        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DSFIL,'(A,A4)')zname(IC)(1:lnzname(IC)),'.ndc'
        else
          WRITE(DSFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnzname(IC)),'.ndc'
        endif
        SFIL = L3DNDC(IC)
        CALL EASKS(SFIL,' 3D nodes coordinates file',
     &    ' ',72,DSFIL,'3D nodes coordinates file',IER,nbhelp)
        if(SFIL(1:2).ne.'  ') L3DNDC(IC)=SFIL

        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DSFIL,'(A,A4)')zname(IC)(1:lnzname(IC)),'.3dt'
        else
          WRITE(DSFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnzname(IC)),'.3dt'
        endif
        SFIL = L3DTAQ(IC)
        CALL EASKS(SFIL,' 3D nodes temperatures file',
     &    ' ',72,DSFIL,'3D nodes temperatures file',IER,nbhelp)
        if(SFIL(1:2).ne.'  ') L3DTAQ(IC)=SFIL
        ZONE3D(IC)=.true.
        call usrmsg('Including addaptive gridding...',' ','-')
        CALL EMKCFG('-',IER)
        call usrmsg('Including addaptive gridding...done.',' ','-')

        CALL EASKOK(' ','Proceed with adaptive gridding?',
     &    OK,nbhelp)
        IF(OK)then
          doit=' '
          call terminalmode(childterminal,tmode)
          CALL ADDPATH(LCFGF,longtfile,CONCAT)
          call tstamp('>','PRJ: edit adaptive gridding')
          if(iappw.eq.690)then
            iappwpc=100
          else
            iappwpc=nint(100.0*(real(iappw)/690.0))  ! reconstitute %
          endif
          if(iappwpc.gt.0.and.iappwpc.le.200)then
            write(doit,'(3a,3i4,5a)') 'grd -mode ',tmode,
     &        ' -s ',iappwpc,iappx+10,iappy+40,' -file ',
     &        longtfile(1:lnblnk(longtfile)),' -zone ',
     &        ZN(1:lnblnk(ZN)),' & '
          else
            write(doit,'(7a)') 'grd -mode ',tmode,
     &        ' -s 0 0 0 -file ',longtfile(:lnblnk(longtfile)),
     &        ' -zone ',ZN(1:lnblnk(ZN)),' & '
          endif
          call usrmsg('starting gridding display via',doit,'-')
          if(tmode(1:7).eq.'graphic')then ! use '-' to request execute_command_line
            call runit(doit,'-')
          else
            call runit(doit,tmode)
          endif
        endif

C Occupant agents.
      elseif(INO.eq.17)then
        itmp=0
        CALL EASKGEOF('Select a zone',CFGOK,itmp,'-',34,IER)
        if (itmp.gt.0 .and. itmp.le.NCOMP) call AGT_MNG(itmp,IER)

C Building-integrated renewables.
      elseif(INO.EQ.20)THEN
        call EDSPLIST('EG',IDV,IER)

C Active materials.
      elseif(INO.EQ.21)THEN
        call EDSPLIST('EM',IDV,IER)

C Optical properties.
      elseif(INO.EQ.22)THEN
        helptopic='advaned_optics_tasks'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABCD(' ','Options:',
     &    'bidirectional optics','IES data for lighting',
     &    'diffuse light source','cancel',IRTP,nbhelp)
        if(IRTP.eq.1)then
          call edbioptics()
        elseif(IRTP.eq.2)then
          call setupies()
        elseif(IRTP.eq.3)then
          call setupdiffuse()
        endif

C Fall through case.
      ELSE
        INO=-1
        GOTO 3
      ENDIF
      INO=-2
      GOTO 3

      END

C ************* NEWPRB 
C NEWPRB specifies a new model via existing or new configuration file
C and the various CAD and native definition facilities. 
C Parameters passed into newprb:
C  itrc = zero silent, one summary, two verbose and this may be
C         reset via user actions
C  confirm=true request confirmation of configuration file name
C  ckpath=true finds the path to it (should be false for CAD use as
C         the file and its location is already known). 
C  itisanexemplar=true then ensure that browse is set to true in
C         any case because the user has previously selected an exemplar model.
C Parameter returned:
C  ier = 0 if no problems found.
      SUBROUTINE NEWPRB(ITRC,confirm,ckpath,itisanexemplar,IER)
#include "espriou.h"
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "prj3dv.h"
#include "material.h"
#include "help.h"

      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      COMMON/GTFIL/GTGEOM
      COMMON/GT/GTNAME
      common/cctlnm/ctldoc,lctlf
      common/OUTIN/IUOUT,IUIN,IEOUT
      common/warncounter/iecount
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
     
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/rpath/path
      common/rcmd/LCMDFL
      common/user/browse
      common/deflt4/dinstpath

C Defaults.
      character*96 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      common/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical XST,OK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical browse
      logical confirm,ckpath,itisanexemplar
      logical unixok,prob
      
      character*32 WORDS(12)

      character path*72,LCMDFL*144,LTMP*144,GTGEOM*72
      character MODE*4,fs*1,outs*144,msg*96
      character OUTSTR*124,ctldoc*248,LCTLF*72,t96*96,t144*144,ETEXT*82
      character sfile*144,odir*84,subpath*84
      character fstring*144,ppath*72,filen*72
      character GTNAME*15
      CHARACTER dinstpath*60,dirpath*72
      character pwd*84
      character cfgpath*144  ! full path to cfg folder for refocus
      character thecfgis*72 ! cfg file only & cfg/cfg_file

C Passed parameters for pregist.
      character root*32,mpath*72,menu*72

      integer igraphiclib  ! external definition
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer numberofzones ! to pass to updwire rather than ncomp
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 numberofzones ! to pass to updwire rather than ncomp
#endif

      helpinsub='edcfg'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      t96='  '
      t144='  '
      msg='  '
      thecfgis='  '; cfgpath='  '

C Ask for source file name and if the file exists then read it;
C otherwise make general defaults and allow input from scratch.
C Assume configuration file is from IFIL+5 and any leakage
C description is fom IFIL+6 (unit used by events profile db).
C Also scan utility files if they exist.
      IAPROB=IPRODB
  289 if(confirm)then 
        IER=0
        helptopic='new_model_cfg_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)

        LTMP=' '    ! reset return string
        call edisp(iuout,'  ')  ! echo a blank line
        CALL EASKXORGTKF(LCMDFL,
     &    ' ','Configuration file?',
     &    DFCFG,LTMP,'model config file name',IER,nbhelp)

C Cancel request, return and ensure that the calling
C routine deals with the -3 ier passed back.
        if(ier.eq.-3) return

C If X11 lltmp will be the file name and if GTK it will include
C the full path to the file.
        IF(LTMP(1:2).EQ.'  ')GOTO 289
        LCMDFL='  '
        call st2file(LTMP,LCMDFL)  ! remove unprintable chars
      endif

C Debug.
C      write(6,*) ' after easkf ',ckpath
C      write(6,*) ' ltmp is ',ltmp(1:lnblnk(ltmp))
C      write(6,*) ' lcmdfl is ',lcmdfl(1:lnblnk(ltmp))
C      write(6,*) ' path is ',path(1:lnblnk(path))

C If requested, find the path and local file name.
      if(ckpath)then
        if(unixok)then
          write(dirpath,'(3a)') 
     &      dinstpath(1:lnblnk(dinstpath)),fs,'training'
        else
          write(dirpath,'(3a)') 
     &      dinstpath(1:lnblnk(dinstpath)),fs,'training'
        endif
        ldirpath=lnblnk(dirpath)
        call fdroot(LCMDFL,path,LCFGF)

C Debug (and the following tests are primarily for Linux/OSX/Unix).
C        write(t144,*) ' after fdroot path is ',path(1:lnblnk(path))
C        call edisp(iuout,t144)
C        write(t144,*) ' model configuration file is ',lcfgf(1:lnblnk(lcfgf))
C        call edisp(iuout,t144)
        if(path(1:2).eq.'./')then

C A ./ indicates that fdroot found no file separators. Must be in cfg folder.
          browse=.false.
        elseif(path(1:1).eq.'.'.and.path(2:2).eq.fs)then

C A .\ indicates that fdroot found no file separators. Must be in cfg folder.
          browse=.false.
        elseif(path(1:ldirpath).eq.dirpath(1:ldirpath))then

C The path is to the installed esp-r training folder so we must be
C browsing the model.  Users should not work here. Warn them.
          call usrmsg('Opening a model inside the ESP-r distribution',
     &      'folder is discouraged!','W')
          browse=.true.
        else

C There were folder separators in the name.
          ifold=-1
          call ckaccess(ifold,iaccess,icerr,path)

C Debug.
C          write(t144,*) 'a iaccess is ',iaccess,icerr,ifold,path
C          call edisp(iuout,t144)
          if(iaccess.eq.1)then

C If iaccedss is non-zero then we do not have permission to write there
C and so we treat that model as if browsing.
            call tstamp('>','PRJ: on browse mode')
            browse=.true.
            call edisp(iuout,'Folder permission prevents modificaton!')
          else
            browse=.false.
          endif

          iglib = igraphiclib()  ! find out if GTK.
          if(iglib.eq.2)then

C For GTK versions, find odir (where prj was started) and compare with the
C current path (both lnpath and lnpath-1 lengths) if there
C is a match then even though there was a full explicit path
C we can recognise that we started in a cfg folder. 
            call usrdir(odir)
            lnpath=lnblnk(path)

C Debug.
C            write(t144,*) ' GTK path is ',path(1:lnblnk(path))
C            call edisp(iuout,t144)
C            write(t144,*) ' GTK odir is ',odir(1:lnblnk(odir))
C            call edisp(iuout,t144)
            if(path(1:lnpath).eq.odir(1:lnpath))then
              browse=.false.
C              call edisp(iuout,'User can modify this model.')
            elseif(path(1:lnpath-1).eq.odir(1:lnpath-1))then
              browse=.false.
C              call edisp(iuout,'User can modify this model.')
            else

C We could get false browse=.true. in terms of the logic.
C The check against the location of the exemplar models
C as defined in the default file (which was done above)
C should be retained. At this point just continue.
              continue
            endif
          endif
        endif
      else

C If the user already selected an exemplar just force browse=true
C otherwise check the folder access permissions.
        if(itisanexemplar)then
          browse=.true.
          call edisp(iuout,'Exemplar models may not be edited.')
          call edisp(iuout,' ')
        else

C Called without request to check paths. See if the folder can be
C writtn to (if iaccess is something other than one.)
          ifold=-1
          call ckaccess(ifold,iaccess,icerr,path)

C Debug.
C          write(6,*) 'b iaccess is ',iaccess,icerr,ifold,path

          if(iaccess.eq.1)then
            call tstamp('>','PRJ: on browse mode')
            browse=.true.
            call edisp(iuout,'Folder permission prevents modificaton.')
          else
            browse=.false.
          endif
        endif
      endif

C Get file separator.
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Check if LCMDFL is a folder or file (if a folder proceed as if
C it was a root name).
      XST=.FALSE.
      ifold=-1
      call ckaccess(ifold,iaccess,icerr,LCMDFL)

C Debug.
C      write(6,*) 'c iaccess is ',iaccess,icerr,ifold,LCMDFL

      if(ifold.eq.0.and.icerr.eq.0)then

C It is a file. Attempt to call fdpwdtocfg to see if there is
C a pwdtocfg in it.
        write(fstring,'(a)') LCMDFL(1:lnblnk(LCMDFL))
        call fdpwdtocfg(fstring,ppath,filen)
        lnpth=lnblnk(ppath)
        if(lnpth.eq.2)then  ! if ./ keep the ./
          write(cfgpath,'(a)') ppath(1:lnpth)
        else
          write(cfgpath,'(a)') ppath(1:lnpth-1)
        endif

        write(thecfgis,'(a)') lcfgf(1:lnblnk(lcfgf))

        CALL ERPFREE(IFCFG,ISTAT)
        call FINDFIL(LCFGF,XST)

C If it exists and the path is something other than ./
C then we can refocus.
        if(XST.and.lnpth.gt.2)then
          call pausems(1000)

C Suggest to user to refocus prj within the model cfg folder at
C this specific cfg file.
          call refocus(cfgpath,thecfgis,iret)
          if(iret.eq.0)then
            continue
          elseif(iret.eq.-2)then
            call edisp(iuout,
     &      'Features using Radiance calls may not work as expected.')
          endif
          goto 444
        endif
      elseif(ifold.eq.1.and.icerr.eq.0)then

C The supplied name is a folder name so check and see if there
C is a cfg folder within it and if so if there is one or more
C configuration files.
        XST=.FALSE.
        write(t144,'(3a)')LCMDFL(1:lnblnk(LCMDFL)),fs,'cfg'
        ifold=-1
        call ckaccess(ifold,iaccess,icerr,t144)

        if(ifold.eq.1.and.icerr.eq.0)then

C What we found was a folder cfg so t144 is equivalent to pwdtocfg.
          write(pwdtocfg,'(a)') t144(1:lnblnk(t144))
          lnpwdc=lnblnk(pwdtocfg)  ! remember its length

C The next lines take the model folder name passed in the
C command line to build up the model root name.
          lex=MIN0(lnblnk(LCMDFL),32)
          write(cfgroot,'(a)',IOSTAT=ios,ERR=13) LCMDFL(1:lex)
          subpath=' '
          call usrdir(odir)
          write(subpath,'(3a)',IOSTAT=ios,ERR=14) 
     &      odir(1:lnblnk(odir)),fs,t144(1:lnblnk(t144))
     
#ifdef MINGW
C In MINGW slightly different logic. If path ls ./ then standard logic
C works. If path begins C:/ or D:/ or E:/ or F:/ no need to prepend odir.
          if(path(1:2).eq.'./'.or.path(1:2).eq.'.\\')then
            continue
          elseif(path(1:2).eq.'C:'.or.path(1:2).eq.'c:'.or.
     &           path(1:2).eq.'D:'.or.path(1:2).eq.'d:'.or.
     &           path(1:2).eq.'E:'.or.path(1:2).eq.'e:'.or.
     &           path(1:2).eq.'F:'.or.path(1:2).eq.'f:')then
            subpath=' '
            write(subpath,'(a)',IOSTAT=ios,ERR=14) path(1:lnblnk(path))
          elseif(path(4:7).eq.'user')then
            subpath=' '
            write(subpath,'(a)',IOSTAT=ios,ERR=14) path(1:lnblnk(path))
          else
            continue
          endif
#endif

C Use standard call to discover if there are cfg files in the subpath
C folder and which (sfile) the user has selected.
          call discovercfg(subpath,sfile,ier)

C Return from discovercfg and derive the path and LCFGF portions
C of the string sfile. If we find a cfg file that exists then
C re-start prj in the cfg folder focused on that file.
          if(ier.eq.0)then
            call fdroot(sfile,path,LCFGF)
    
            lcfgr=lnblnk(LCFGF)
            lcfgl=lcfgr-3
            if(lcfgr.gt.4)then
              if(LCFGF(lcfgl:lcfgr).eq.'.cfg')then
                if(lcfgl-1.le.32)then
                  write(cfgroot,'(a)') LCFGF(1:lcfgl-1)
                else
                  write(cfgroot,'(a)') LCFGF(1:32)
                endif
                write(thecfgis,'(a,a)')LCFGF(1:lcfgr)
              else
                if(lcfgr.le.32)then
                  write(cfgroot,'(a)') LCFGF(1:lcfgr)
                else
                  write(cfgroot,'(a)') LCFGF(1:32)
                endif
                write(thecfgis,'(a,a)')LCFGF(1:lcfgr),'.cfg'
              endif
            else
              if(lcfgr.le.32)then
                write(cfgroot,'(a)') LCFGF(1:lcfgr)
              else
                write(cfgroot,'(a)') LCFGF(1:32)
              endif
              write(thecfgis,'(a,a)')LCFGF(1:lcfgr),'.cfg'
            endif

            lnpth=lnblnk(path)
            if(lnpth.eq.2)then  ! if ./ keep the ./
              write(cfgpath,'(a)') path(1:lnpth)
            else
              write(cfgpath,'(a)') path(1:lnpth-1)
            endif
            call edisp(iuout,
     &        'Model configuration file is in folder:')
            call edisp(iuout,path)

C Refocus prj on the model cfg folder and re-load the model cfg file
C as is done in cadio.F
            call pausems(1000)

C Suggest to user to refocus prj within the model cfg folder at
C this specific cfg file.
            call refocus(cfgpath,thecfgis,iret)
            if(iret.eq.0)then
              continue
            elseif(iret.eq.-2)then
              call edisp(iuout,
     &       'Features using Radiance calls may not work as expected.')
            endif
            goto 444
          endif
        endif
      else

C Was not a folder or a file.
        write(LCFGF,'(a)',IOSTAT=ios,ERR=18) LCMDFL(1:lnblnk(LCMDFL))
        CALL ERPFREE(IFCFG,ISTAT)
        call FINDFIL(LCFGF,XST)
      endif

C If not initially found check for if root name only given. Parse
C LCFGF into tokens and check the last token for .cfg.
      if(.NOT.XST)then
        LTMP=LCFGF
        lcfgr=lnblnk(LTMP)
        lcfgl=lcfgr-3
        call GETTOKENS(LTMP,IW,WORDS)
        lwcfgr=lnblnk(WORDS(IW))
        lwcfgl=lwcfgr-3
        if(lwcfgr.gt.4)then
          if(WORDS(IW)(lwcfgl:lwcfgr).eq.'.cfg')then
            lex=MIN0((lwcfgl-1),32)  ! ensure cfgroot within length
            write(cfgroot,'(a)',IOSTAT=ios,ERR=13)WORDS(IW)(1:lwcfgl-1)
          else
            lex=MIN0(lwcfgr,32)  ! ensure cfgroot within length
            write(cfgroot,'(a)',IOSTAT=ios,ERR=13) WORDS(IW)(1:lwcfgr)
            write(LTMP,'(a,a)',IOSTAT=ios,ERR=19) LTMP(1:lcfgr),'.cfg'
          endif
        elseif(lwcfgr.le.4)then
          write(cfgroot,'(a)',IOSTAT=ios,ERR=13) WORDS(IW)(1:lwcfgr)
          write(LTMP,'(a,a)',IOSTAT=ios,ERR=19) LTMP(1:lcfgr),'.cfg'
        endif


C Check existance of this file (should not overwrite an existing file).
        XST=.false.
        INQUIRE (FILE=LTMP,EXIST=XST)
        if(XST)then
          call fdroot(LTMP,path,LCFGF)
          lcfgr=lnblnk(LCFGF)
          write(thecfgis,'(a,a)')LCFGF(1:lcfgr)
          lnpth=lnblnk(path)
          call usrdir(odir)
          write(cfgpath,'(3a)') odir(1:lnblnk(odir)),fs,
     &      path(1:lnpth-1)  ! omit trailing /
          call pausems(1000)

C Suggest to user to refocus prj within the model cfg folder at
C this specific cfg file.
          call refocus(cfgpath,thecfgis,iret)
          if(iret.eq.0)then
            continue
          elseif(iret.eq.-2)then
            call edisp(iuout,
     &      'Features using Radiance calls may not work as expected.')
          endif
          goto 444
        else

C Check to see if LCFGF is in a cfg folder.
          LTMP=LCFGF
          call GETTOKENS(LTMP,IW,WORDS)
          lwcfgr=lnblnk(WORDS(IW))
          lwcfgl=lwcfgr-3
          if(lwcfgr.gt.4)then
            if(WORDS(IW)(lwcfgl:lwcfgr).eq.'.cfg')then
              write(cfgroot,'(a)',IOSTAT=ios,ERR=13) 
     &          WORDS(IW)(1:lwcfgl-1)
              write(t96,'(4a)',IOSTAT=ios,ERR=20) 'cfg',fs,
     &          LTMP(1:lcfgl-1),'.cfg'
            else
              write(cfgroot,'(a)',IOSTAT=ios,ERR=13)WORDS(IW)(1:lwcfgr)
              lex=MIN0((3+1+4+lcfgr),96)
              if(lex.ge.96)then
                write(msg,'(a,i2,a)') 'The overall path is',lex,
     &            ' char (which is >96 char).'
                call usrmsg(msg,'Suggest relocating model.','W')
              endif
              write(t96,'(4a)',IOSTAT=ios,ERR=20) 'cfg',fs,
     &          LTMP(1:lcfgr),'.cfg'
            endif
          elseif(lwcfgr.le.4)then
            write(cfgroot,'(a)',IOSTAT=ios,ERR=13) WORDS(IW)(1:lwcfgr)
            write(t96,'(a,a1,a,a)',IOSTAT=ios,ERR=20) 'cfg',fs,
     &        LTMP(1:lcfgr),'.cfg'
          endif
          if((t96(1:2).ne.'  ').and.(t96(1:4).ne.'UNKN'))then
            XST=.false.
            INQUIRE (FILE=t96,EXIST=XST)
            if(XST)then

C User provided model cfg file name and prj found a cfg folder
C with that cfg file in it. The path from fdroot will thus be
C cfg and we need it to be a full path in order to refocus.
              call fdroot(t96,path,LCFGF) ! we found something
              lcfgr=lnblnk(LCFGF)
              write(thecfgis,'(a,a)')LCFGF(1:lcfgr)
              lnpth=lnblnk(path)
              call usrdir(odir)
              write(cfgpath,'(3a)') odir(1:lnblnk(odir)),fs,
     &          path(1:lnpth-1)  ! omit trailing /
              call pausems(1000)

C Suggest to user to refocus prj within the model cfg folder at
C this specific cfg file.
              call refocus(cfgpath,thecfgis,iret)
              if(iret.eq.0)then
                continue
              elseif(iret.eq.-2)then
                call edisp(iuout,
     &       'Features using Radiance calls may not work as expected.')
              endif
              goto 444
            else

C Check to see if a cfg folder within a folder with the same
C name as that the user entered.
              LTMP=LCFGF
              call GETTOKENS(LTMP,IW,WORDS)
              lwcfgr=lnblnk(WORDS(IW))
              lwcfgl=lwcfgr-3
              if(lwcfgr.gt.4)then
                if(WORDS(IW)(lwcfgl:lwcfgr).eq.'.cfg')then
                  write(cfgroot,'(a)',IOSTAT=ios,ERR=13) 
     &              WORDS(IW)(1:lwcfgl-1)
                  lex=MIN0((lcfgl+1+3+1+lcfgl+4),96)
                  if(lex.ge.96)call usrmsg(
     &              'The overall path is more than 96 char.',
     &              'Suggest relocating model.','W')
                  write(t96,'(6a)',IOSTAT=ios,ERR=20) 
     &              LTMP(1:lcfgl-1),fs,
     &              'cfg',fs,LTMP(1:lcfgl-1),'.cfg'
                else
                  write(cfgroot,'(a)',IOSTAT=ios,ERR=13) 
     &              WORDS(IW)(1:lwcfgr)
                  lex=MIN0((lcfgr+1+3+1+lcfgr+4),96)
                  if(lex.ge.96)call usrmsg(
     &              'The overall path is more than 96 char.',
     &              'Suggest relocating model.','W')
                  write(t96,'(6a)',IOSTAT=ios,ERR=20) 
     &              LTMP(1:lcfgr),fs,'cfg',fs,LTMP(1:lcfgr),'.cfg'
                endif
              elseif(lwcfgr.le.4)then
                write(cfgroot,'(a)',IOSTAT=ios,ERR=13) 
     &            WORDS(IW)(1:lwcfgr)
                write(t96,'(6a)',IOSTAT=ios,ERR=20) 
     &            LTMP(1:lcfgr),fs,'cfg',fs,LTMP(1:lcfgr),'.cfg'
              endif
              if((t96(1:2).ne.'  ').and.(t96(1:4).ne.'UNKN'))then
                XST=.false.
                INQUIRE (FILE=t96,EXIST=XST)
                if(XST)then
                  call fdroot(t96,path,LCFGF)
                  lcfgr=lnblnk(LCFGF)
                  write(thecfgis,'(a,a)')LCFGF(1:lcfgr)
                  lnpth=lnblnk(path)
                  write(cfgpath,'(a)') path(1:lnpth-1)  ! omit trailing /
                  call pausems(1000)
  
C << Have not been able to set a test that goes to this point in
C << the code >>
                  goto 444
                else

C Check to see adding a cfg folder and cfgroot to LCFGF works.
                  LTMP=LCFGF
                  call GETTOKENS(LTMP,IW,WORDS)
                  lwcfgr=lnblnk(WORDS(IW))
                  lwcfgl=lwcfgr-3
                  if(lwcfgr.gt.4)then
                    if(WORDS(IW)(lwcfgl:lwcfgr).eq.'.cfg')then
                      write(cfgroot,'(a)',IOSTAT=ios,ERR=13) 
     &                  WORDS(IW)(1:lwcfgl-1)
                      write(t96,'(6a)',IOSTAT=ios,ERR=20) 
     &                  LTMP(1:lcfgl-1),fs,
     &                  'cfg',fs,cfgroot(1:lnblnk(cfgroot)),'.cfg'
                    else
                      write(cfgroot,'(a)',IOSTAT=ios,ERR=13) 
     &                  WORDS(IW)(1:lwcfgr)
                      lex=MIN0((lcfgr+1+3+1+lcfgr+4),96)
                      if(lex.ge.96)call usrmsg(
     &                  'The overall path is more than 96 char.',
     &                  'Suggest relocating model.','W')
                      write(t96,'(6a)',IOSTAT=ios,ERR=20) 
     &                  LTMP(1:lcfgr),fs,
     &                  'cfg',fs,cfgroot(1:lnblnk(cfgroot)),'.cfg'
                    endif
                  elseif(lwcfgr.le.4)then
                    write(cfgroot,'(a)',IOSTAT=ios,ERR=13) 
     &                WORDS(IW)(1:lwcfgr)
                    write(t96,'(6a)',IOSTAT=ios,ERR=20) 
     &                LTMP(1:lcfgr),fs,
     &                'cfg',fs,cfgroot(1:lnblnk(cfgroot)),'.cfg'
                  endif
                  if((t96(1:2).ne.'  ').and.(t96(1:4).ne.'UNKN'))then
                    XST=.false.
                    INQUIRE (FILE=t96,EXIST=XST)
                    if(XST)then
                      call fdroot(t96,path,LCFGF)
                      lcfgr=lnblnk(LCFGF)
                      write(thecfgis,'(a,a)')LCFGF(1:lcfgr)
                      lnpth=lnblnk(path)
                      write(cfgpath,'(a)') path(1:lnpth-1)  ! omit trailing /
                      call pausems(1000)

C << Have not been able to set a test that goes to this point in
C << the code >>
                      goto 444
                    endif
                  endif
                endif
              endif
            endif
          endif
        endif
      endif
 444  IF(XST)THEN
 
C If user already said silent or very verbose follow this.
C Otherwise allow a choice to switch to a different level.
        if(itrc.eq.2)then
          continue  ! user already set full report
        elseif(itrc.eq.1)then
          CALL EASKABC('When reading the model description,',
     &      'do it:','silently','with synopsis','full report',
     &      IW,0)
          itrc=IW-1
        endif
        call edisp(iuout,'  ')  ! echo blank line in case of warning

C At this point read in an existing model cfg file.
        MODE='ALL '
        IUF=IPRODB   ! assign second file unit to the events db unit
        CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
        if(ier.eq.3)then
          write(ieout,'(a)')'Model is to complex - exiting prj.'
          iecount=iecount+1

C Close the session log but do not remove so message persists.
          close(ieout)
          call epagend
          stop
        endif

C copy # of plant component nodes from bps to prj commons 
        call CopyPltNodeCounts()
        IF(IER.NE.0)THEN
          helptopic='unspecified_prob_found'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK('Problem reading system configuration file!',
     &                'Retry?',OK,nbhelp)
          if(OK)then
            confirm=.true.
            goto 289
          endif
        ENDIF

C We know how many zone so use ZDATA to finish instantiating the
C common blocks for:
C   szcoords : X,Y & Z coordinates of each vertice in zone.
C   isznver : number of vertices associated with each surface.
C   iszjvn : topology of vertices.
        NZONES=NCOMP
        ISFSUM=0
        CALL ZDATA (ITRC,IER,NZONES,ISFSUM)
        call tstamp('>','PRJ: current model')
        call tstamp('>',LCFGF)

C If there is a ground topology read it.
        if(GTGEOM(1:2).eq.'  '.or.GTGEOM(1:4).eq.'UNKN')then
          continue
        else
          iunit=IAPROB
          call egrnin(iunit,gtgeom,itrc,itru,ier)
C          write(outs,'(2a)') 'Found ground definition ',GTNAME
C          call edisp(iuout,outs)
        endif

        IF(MMOD.EQ.8)then
          numberofzones=ncomp
          call updwire(numberofzones)
          call redrawbuttons()
          WRITE(etext,'(2A)')'Model: ',modeltitle(1:lnblnk(modeltitle))
          iside=1; isize=1; ifont=1
          call viewtext(etext,iside,isize,ifont)
        endif

C Open core databases associated with the exemplar or new model.
        if(MLDBOK.and.MATDBOK.and.OPTKOK)then
          continue
        else
          call opendb(ier)
          if(ier.ne.0)then
            call usrmsg(
     &        'Possible problems with one or more of the construction',
     &        'and optical databases, please check!','W')
            ier = 0
          endif
        endif

C Scan for matching MLC for surfaces.
        do 30 ICOMP=1,NCOMP
          call georead(IFIL+1,LGEOM(ICOMP),ICOMP,1,iuout,ier)
          call zgupdate(0,icomp,ier)     ! Re-derive values
          call zinfo(icomp,zoa,zvol,'q')

C Find the index of the MLC which matches each surface.
          DO 9994 I=1,NZSUR(icomp)
            icn1=izstocn(icomp,i)
            if(icn1.ge.1)then
              smlcindex(icomp,i)=0  ! assume no matching MLC          
              lnssmlc=lnblnk(SMLCN(icomp,i))
              do 5 ii=1,nmlc
                if(SMLCN(icomp,i)(1:lnssmlc).eq.
     &             mlcname(ii)(1:lnmlcname(ii)))then
                  smlcindex(icomp,i)=ii   ! remember MLC index     
                endif
  5           continue
            endif
 9994     continue
 30     continue

C Scan control file (silently) if one exists.
        ICTLF=IFIL+1
        CALL ERPFREE(ICTLF,ISTAT)
        call FINDFIL(LCTLF,XST)
        if(XST)then
          CALL EZCTLR(ICTLF,0,IUOUT,IER)
          if(IER.NE.0)then
            helptopic='control_problem_found'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKOK('Problem reading control file!',
     &        'Retry?',OK,nbhelp)
            if(OK)then
              confirm=.true.
              goto 289
            endif
          endif
        endif

C Display any general associated images if in graphic mode.
        if(MMOD.EQ.8) call imgdisp(0,'****',ier)

C If a plant only model then no need to read zone files.
C Set flag so that bounds are checked.
        DO 77 ICU=1,NCOMP
          iZBFLG(ICU)=0
   77   CONTINUE
        IF(IER.NE.0)THEN
          helptopic='unspecified_prob_found'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK('Problem reading system configuration file!',
     &                'Retry?',OK,nbhelp)
          if(OK)then
            confirm=.true.
            goto 289
          endif
        ENDIF

C Scan the model topology and report on any problems encountered.
        call ckcurmatch(prob,iprob)
        if(prob)then
          call tstamp('>','PRJ: found topology incinsistent')
          if(IPROB.gt.10)then
            write(outs,*) IPROB,' inconsistencies found!'
            call edisp(iuout,outs)
            call usrmsg(
     &        'Model topology (surface connections) is inconsistent.',
     &        ' ','W')
          else
            call usrmsg(
     &      'Model topology (surface connections) is inconsistent.',
     &      'Clear current topology and use topology tool to resolve.',
     &      'W')
          endif
        endif

        CFGOK=.TRUE.
        MODIFYVIEW=.TRUE.
        MODLEN=.TRUE.
      ELSE

C Model configuration file not found. Confirm with user.
        helptopic='cfg_not_found'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(OUTSTR,'(A,2X,A)')' Could not locate',LCFGF
        CALL EASKABC(OUTSTR,' Options:',
     &    'respecify','new file','abort Project Manager',IW,nbhelp)
        if(IW.eq.1)then
          confirm=.true.
          goto 289
        elseif(IW.eq.2)then
          continue
        elseif(IW.eq.3)then  ! User abort request. Update and close session log.
          write(ieout,'(a)')'Model cfg file not found user abort prj.'
          iecount=iecount+1  ! retain the log file
          close(ieout)
          CALL ERPFREE(ieout,ISTAT)
          CALL DeallocateAllArrays  ! Clear allocatable arrays
          CALL EPAGEND
          STOP
        endif

C Clear any previous model in memory and call pregisst in interactive
C mode (with blanks for the parameters).
        call clrprb
        root=' '
        mpath=' '
        menu=' '
        call pregist('i ',root,mpath,menu,ier)
        IF(IER.LT.0)RETURN

        CFGOK=.TRUE.

C Open default databases (user can alter later).
        IF(MMOD.EQ.8)then
          numberofzones=ncomp
          call updwire(numberofzones)
        endif
        call opendb(ier)
        if(ier.ne.0)then
          call usrmsg(
     &      'Possible problems with one or more of the construction',
     &      'and optical databases, please check!','W')
          ier = 0
        endif
      ENDIF
      return

C Error messages.
   13 if(IOS.eq.2)then
        CALL USRMSG('No permission to write cfgroot',cfgroot,'W')
      else
        CALL USRMSG('String write error in cfgroot',cfgroot,'W')
      endif
      IER=1
      return
   14 if(IOS.eq.2)then
        CALL USRMSG('No permission to write subpath',subpath,'W')
      else
        CALL USRMSG('String write error in subpath',subpath,'W')
      endif
      IER=1
      return
   18 if(IOS.eq.2)then
        CALL LUSRMSG('No permission to write LCFGF',LCFGF,'W')
      else
        CALL LUSRMSG('String write error in LCFGF',LCFGF,'W')
      endif
      IER=1
      return
   19 if(IOS.eq.2)then
        CALL USRMSG('No permission to write LTMP',LTMP,'W')
      else
        CALL USRMSG('String write error in LTMP',LTMP,'W')
      endif
      IER=1
      return
   20 if(IOS.eq.2)then
        CALL USRMSG('No permission to write t96',t96,'W')
      else
        CALL USRMSG('String write error in t96',t96,'W')
      endif
      IER=1
      return

      end

C ***** visualz
C visualz drives visualisation process - hidden line or raytracing.
C It duplicates opts within the model export facility.
C << it would be clearer if all model I/O happened there >>
      subroutine visualz(act,ier)
#include "building.h"
#include "model.h"
#include "help.h"
      
      integer lnblnk  ! function definition
      integer ier

C Parameters.
      character act*1  !  V (for Viewer) or R (for Radiance)

      common/OUTIN/IUOUT,IUIN,IEOUT
      common/FILEP/IFIL
      common/appw/iappw,iappx,iappy
      integer iappw,iappx,iappy
      integer childterminal  ! picks up mmod from starting of prj
      common/childt/childterminal
      common/user/browse
      character path*72
      common/rpath/path
      logical concat,OK,browse,XST,deref,unixok,modcfg
      character hold32*32
      character dradcf*72,OUTSTR*124,outs*124,outs248*248
      character dvfil*72,ltmp*72
      character doit*248,tmode*8
      character fs*1,tfile*72
      character longtfile*144,longtfiledos*144,L144*144,lltmp*144
      character brw*8   ! to signal model is being browsed
      integer iappwpc   ! application height as % of nominal size
      integer lnradcf,lnltmp  ! lengths of file names

      helpinsub='edcfg'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      helptopic='visualization_options'
      call gethelptext(helpinsub,helptopic,nbhelp)

  11  if(act.eq.'V')then

C Construct a 'VIEWER' format file.  First get file name. Take
C into account the application start and model path.
        if(pwdtocfg(1:1).eq.'!')then
          write(L144,'(4a)')path(1:lnblnk(path)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.vew'
        else
          write(L144,'(6a)') pwdinitial(1:lnpwdi),fs,
     &      pwdtocfg(1:lnblnk(pwdtocfg)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.vew'
        endif
C Debug write(6,*) 'c l144 is ',L144(1:lnblnk(L144))

        dvfil='std.vew'
        doit = ' '

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path. L144 is the suggested name and
C lltmp is the file name returned by the user selection.
        lltmp='  '
        call edisp(iuout,'  ')  ! echo a blank line
        CALL EASKXORGTKF(L144,
     &    'Viewer export file name?',' ',
     &    dvfil,lltmp,'viewer file',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          return  ! cancel detected, return.
        endif
        if(ier.ne.0.or.lltmp(1:2).eq.'  ')then
          goto 11
        endif
        if(unixok)then
          call addpath(LCFGF,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(LCFGF,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        write(doit,'(4a)') 'ecnv -obs -if esp -in ',
     &    longtfile(1:lnblnk(longtfile)),' -of viewer -out ',
     &    lltmp(1:lnblnk(lltmp))
        call usrmsg('starting conversion via',doit,'-')
        call runit(doit,'-')

C Tell user about the new file.
        write(outs248,'(3a)') 'The file ',lltmp(1:lnblnk(lltmp)),
     &    ' has been created.'
        call edisp248(iuout,outs248,90)
        CALL EASKOK(' ',' Display hidden line view?',OK,nbhelp)
        IF(OK)then

C Get logical name of child process terminal type and create a
C string to drive VIEWER.
          doit = ' '
          call terminalmode(childterminal,tmode)
          if(iappw.eq.690)then
            iappwpc=100
          else
            iappwpc=nint(100.0*(real(iappw)/690.0))  ! reconstitute %
          endif
          if(iappwpc.gt.0.and.iappwpc.le.200)then
            write(doit,'(3a,3i4,3a)') 'viewer -mode ',tmode,
     &        ' -s ',iappwpc,iappx+25,iappy+20,' -file ',
     &        lltmp(1:lnblnk(lltmp)),' & '
          else
            write(doit,'(5a)') 'viewer -mode ',tmode,
     &        ' -s 0 0 0 -file ',lltmp(1:lnblnk(lltmp)),' & '
          endif
          call usrmsg('starting hidden line viewer via',doit,'-')
          if(tmode(1:7).eq.'graphic')then ! tmode for runit is different from
                                          ! tmode ESP-r command line argument
             call runit(doit,'-')         ! use '-' to request execute_command_line
          else
            call runit(doit,tmode)
          endif
        endif
        return

      elseif(act.eq.'R')then

C Check if Radiance has been installed. If not do not start e2r.
        if(unixok)then
          found_ximage=.false.; hold32='ximage'
          call isinstalled(hold32,found_ximage)
          if(.NOT.found_ximage)then
            call usrmsg(
     &      'You have not yet installed Radiance so the ESP-r module',
     &      'e2r will fail to produce correct Radiance files!','W')
            return
          endif
        else
          found_ximage=.false.  ! not in Windows
        endif

C Assume model is multi-folder with a rad folder.
C Set rcf name if unknown and set default name. If path for radiance
C files is ./ then update this to ../rad, create a folder. And
C proceed on the basis that the rcf file will be in the rad folder.
        modcfg=.false.
        if(pwdtocfg(1:1).eq.'!')then
          if(radpth(1:3).eq.'./ '.or.radpth(1:3).eq.'.\\ ')then
            CALL PHELPD('visu-rad-warning',7,'-',0,0,IER)
            write(radpth,'(3a)')'..',fs,'rad'
            write(doit,'(4a)') 'mkdir ',
     &        cfgroot(1:lnblnk(cfgroot)),fs,'rad'
            call usrmsg('Creating folder for radiance model:',doit,'P')
            call runit(doit,'-')

C And update radpath. The L144 is for future use of EASKXORGTKF
            write(radpth,'(3a)') '..',fs,'rad'
            modcfg=.true.
          endif

          if(lradcf(1:7).eq.'UNKNOWN')then  ! setup in form of ../rad/xx
            write(lradcf,'(6a)') '..',fs,'rad',fs,
     &        cfgroot(1:lnblnk(cfgroot)),'.rcf'
C            write(L144,'(8a)')path(1:lnblnk(path)),fs,'..',fs,'rad',
C     &        fs,cfgroot(1:lnblnk(cfgroot)),'.rcf'
            modcfg=.true.
          endif
        else

C Prj has not been invoked from within the model cfg folder.
          if(radpth(1:3).eq.'./ '.or.radpth(1:3).eq.'.\\ ')then
            CALL PHELPD('visu-rad-warning',7,'-',0,0,IER)
            write(radpth,'(3a)')'..',fs,'rad'
            write(doit,'(8a)') 'mkdir ',pwdinitial(1:lnpwdi),fs,
     &        pwdtocfg(1:lnblnk(pwdtocfg)),fs,'..',fs,'rad'
            call usrmsg('Creating folder for radiance model:',doit,'P')
            call runit(doit,'-')

C And update radpath.
            write(radpth,'(3a)') '..',fs,'rad'
            modcfg=.true.
          endif

          if(lradcf(1:7).eq.'UNKNOWN')then
            write(lradcf,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.rcf'
C            write(L144,'(10a)') pwdinitial(1:lnpwdi),fs,
C     &        pwdtocfg(1:lnblnk(pwdtocfg)),fs,'..',fs,'rad',fs,
C     &        cfgroot(1:lnblnk(cfgroot)),'.rcf'
            modcfg=.true.
          endif
        endif
        dvfil='std.rcf'

C Check rcf name and contents of file on return from e2r.
C Set browsing no.
        brw = ' -b no '

 319    ltmp=lradcf
        dradcf='scene.rcf'
        CALL EASKS(ltmp,' ','Radiance scene file?',
     &    72,dradcf,'Radiance scene file name',IER,nbhelp)
        if(ltmp.eq.' ')goto 319 

C rcf name OK and differs from prior name, update cfg file and start e2r.
        lnradcf=lnblnk(lradcf)
        lntmp=lnblnk(ltmp)
        if(ltmp(1:lntmp).eq.lradcf(1:lnradcf))then
          continue
        else
          modcfg=.true.
          lradcf = ltmp
        endif
        if(modcfg)then
          call tstamp('>','PRJ: save configuration (with rcf file)')
          CALL EMKCFG('-',IER)
        endif

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive e2r.
        doit = ' '
        call terminalmode(childterminal,tmode)
        call addpath(LCFGF,longtfile,concat)

C If prj initial size is a % of default, pass this on to child with
C an offset from prj start position.
        if(iappw.eq.690)then
          iappwpc=100
        else
          iappwpc=nint(100.0*(real(iappw)/690.0))  ! reconstitute %
        endif
        if(iappwpc.gt.0.and.iappwpc.le.200)then
          write(doit,'(3a,3i4,4a)') 'e2r -mode ',tmode,
     &      ' -s ',iappwpc,iappx+35,iappy+40,' -file ',
     &      longtfile(1:lnblnk(longtfile)),brw,' & '
        else
          write(doit,'(6a)') 'e2r -mode ',tmode,
     &      ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),brw,' & '
        endif
        if(tmode(1:7).eq.'graphic')then ! tmode for runit is different from
                                        ! tmode ESP-r command line argument
          call runit(doit,'-')          ! use '-' to request execute_command_line
        else
          call runit(doit,tmode)
        endif
        helptopic='click_when_done'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('e2r finished menu',nbhelp,'-',0,0,IER)

C Check if radiance scene file exists. If not or zero length or not correct 
C file type then dereference it from the cfg file. The alternative location
C for the scene file is in ../rad if user owned the model.  
        IRCFG=IFIL+1
        call ERPFREE(IRCFG,ISTAT)
        XST=.FALSE.
        deref=.FALSE.
C        write(6,*) 'expecting ',lradcf(1:lnblnk(lradcf))
        call FINDFIL(lradcf,XST)
        if(XST)then
          CALL EFOPSEQ(IRCFG,lradcf,1,IER)

C If end of file encountered on first line then remove the file.
          if(IER.eq.-301)then
            call EFDELET(IRCFG,ISTAT)
            deref=.true.
          else
            CALL STRIPC(IRCFG,OUTSTR,99,ND,1,'line 1',IER)
            if (OUTSTR(1:13).ne.'*ESP-r visual')deref=.true.
            call ERPFREE(IRCFG,ISTAT)
          endif
        else

C Check for it within the model rad folder. If so copy it
C to the model cfg folder.
          write(tfile,'(3a)')radpth(1:lnblnk(radpth)),fs,
     &      LRADCF(1:lnblnk(LRADCF))
          call FINDFIL(tfile,XST)
          if(XST)then
            CALL EFOPSEQ(IRCFG,tfile,1,IER)

C If end of file encountered on first line then remove the file.
            if(IER.eq.-301)then
              call EFDELET(IRCFG,ISTAT)
              deref=.true.
            else
              CALL STRIPC(IRCFG,OUTSTR,99,ND,1,'line 1',IER)
              if (OUTSTR(1:13).ne.'*ESP-r visual')deref=.true.
              call ERPFREE(IRCFG,ISTAT)
            endif

C Found it and good so just point to the file in ../rad folder
C and update the cfg file to remember this.
            if(.NOT.deref)then
              write(lradcf,'(a)') tfile(1:lnblnk(tfile))
              call tstamp('>','PRJ: save configuration (no rcf file)')
              CALL EMKCFG('-',IER)
            endif
          else
            deref=.true.
          endif
        endif
        if(deref)then

C Problem with rcf file therefore dereference it.
          call usrmsg(
     &      'Could not find scene file so dereferencing it.',' ','W')
          lradcf='UNKNOWN'
          call tstamp('>','PRJ: save configuration (no rcf file)')
          CALL EMKCFG('-',IER)
        else

C << Place to associate images (when logic ready.) >>

        endif
      endif
      return
      end

C ******************** globaltransform ********************
C Applies global transforms to model zones. Parameters:
C   valx,valy,valz - transforms (m) for each axis.
C   izlistnb - number of selected zones.
C   izlist - array of selected zones to transform.
C   silent - if true, do not ask user about transforms
C            of obstruction data.

      subroutine globaltransform(valx,valy,valz,izlistnb,izlist,
     &  silent,itru,ier)
      implicit none
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
#include "help.h"
      
      integer lnblnk  ! function definition
      real valx,valy,valz  ! the transforms
      integer izlistnb,izlist
      dimension izlist(MCOM)  ! array of which zones are selected for transform.
      logical silent  ! if true then skip dialogs
      integer itru,ier  ! type for passed parameters

      integer ifil
      common/FILEP/IFIL
      integer iuout,iuin,ieout
      common/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      CHARACTER LAPROB*72
            
      logical newgeo  ! to use for testing if new/old geometry file.
      
      character*72 LTMP
      character ZN*12
      character outs*124
      logical OK,goforit
      integer iuf          ! file type 
      integer iicomp       ! index of current zone
      integer itrc,ij,i,iz ! types for local variables
      integer ibe          ! loop for faces of obstruction
      integer inod,icmp  ! for flow logic
      real zoa,zvol

      helpinsub='edcfg'  ! set for subroutine
      helptopic='global_transform_options'
      call gethelptext(helpinsub,helptopic,nbhelp)
       
      IUF=IFIL+2
      DO 83 IZ=1,izlistnb
        IF(izlist(IZ).GT.0)THEN
          IICOMP=izlist(IZ)
          call sumrchg(IICOMP,'i',silent)  ! clear the whats changed data
          write(zn,'(A)') zname(IICOMP)
          write(outs,'(3a)') ' Transforming: ',zn(1:lnblnk(zn)),'...'
          if(.NOT.silent)CALL USRMSG(' ',outs,'-')
          LTMP=LGEOM(IICOMP)

C Find out the version number so we know what to do with obstructions later.
          call eclose(gversion(IICOMP),1.1,0.01,newgeo)

C Scan the zone geometry and apply transform.
          call georead(IUF,LTMP,IICOMP,1,iuout,IER)
          DO 62 I=1,NTV
            X(I)=X(I)+VALX
            Y(I)=Y(I)+VALY
            Z(I)=Z(I)+VALZ
   62     continue
          iZBFLG(IICOMP)=0

          if(nbvis(iicomp).gt.0)then

C Visual object transforms.
            do ij=1,nbvis(iicomp)
              XOV(iicomp,ij)=XOV(iicomp,ij)+VALX
              YOV(iicomp,ij)=YOV(iicomp,ij)+VALY
              ZOV(iicomp,ij)=ZOV(iicomp,ij)+VALZ
              do ibe=1,8
                XVP(iicomp,ij,ibe)=XVP(iicomp,ij,ibe)+VALX
                YVP(iicomp,ij,ibe)=YVP(iicomp,ij,ibe)+VALY
                ZVP(iicomp,ij,ibe)=ZVP(iicomp,ij,ibe)+VALZ
              enddo  ! of ibe
            enddo    ! of ij
          endif

C Decide whether to upgrade the geometry file format and
C then write out the updated geometry.
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(iicomp) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            call geowrite(IUF,LTMP,IICOMP,ITRU,3,IER)
          else
            call emkgeo(IUF,LTMP,IICOMP,3,IER)
          endif

C After transform update the g6 & g7 commons and find dependencies.
          call zgupdate(0,IICOMP,ier)
          call zinfo(iicomp,zoa,zvol,'q')

C Depending on the nature of the change update the common block pmchange.
          call warnmod(IICOMP,'str')

C If there are associated flow entities transform them as well.
          if(IAIRN.ge.1.and.ICAAS(IICOMP).ne.0)then

C Apply the transform to the position of nodes and components
C associated with this zone. Loop through all the surfaces and
C items associated in the network apply this transform to.
            goforit=.false.
            do ij=1,nzsur(iicomp)
              call doesflowrefsurface(iicomp,ij,inod,icmp)
              if(inod.gt.0.or.icmp.gt.0)call usrmsg(
     &          'Updating associated network entities.',
     &          'Please double check!','-')
              if(inod.gt.0)then
                HNOD(INOD,1)=HNOD(INOD,1)+VALX
                HNOD(INOD,2)=HNOD(INOD,2)+VALY
                HNOD(INOD,3)=HNOD(INOD,3)+VALZ
                goforit=.true.
              endif
              if(icmp.gt.0)then
                HCMP(ICMP,1,1)=HCMP(ICMP,1,1)+VALX 
                HCMP(ICMP,1,2)=HCMP(ICMP,1,2)+VALY
                HCMP(ICMP,1,3)=HCMP(ICMP,1,3)+VALZ
                goforit=.true.
              endif
            enddo

C Also reflect change in zone COG.
            i=ICAAS(iicomp)
            HNOD(i,1)=ZCOG(iicomp,1); HNOD(i,2)=ZCOG(iicomp,2)
            HNOD(i,3)=ZCOG(iicomp,3)
            write(outs,'(3a,3f7.3)') 'Updating ',NDNAM(i),' @',
     &        HNOD(i,1),HNOD(i,2),HNOD(i,3)
            call edisp(iuout,outs)
            if(goforit)then
              call updatebothflownetworks(ier)
            endif
          endif

C Transform any MRT sensors associated with this zone and if a
C newer geometry file also update the block attributes held there.
          if (IVF(IICOMP).eq.1) then
            CALL ERMRT(ITRC,ITRU,IUF,LVIEW(IICOMP),IICOMP,IER)
            if (NCUB(IICOMP).gt.0) then
              if(silent)then
                ok= .true.
              else
                write(outs,'(3a)') 'Transform MRT sensors of ',
     &                zn(1:lnblnk(zn)),'?'
                call easkok(' ',outs,OK,nbhelp)
              endif
              if (OK) then
                do 73 ij=1,NCUB(IICOMP)
                  XOC(ij)=XOC(ij)+VALX
                  YOC(ij)=YOC(ij)+VALY
                  ZOC(ij)=ZOC(ij)+VALZ
  73            continue
                CALL EMKMRT(LVIEW(IICOMP),LGEOM(IICOMP),
     &                      NZSUR(IICOMP),IUF,IICOMP,'v',IER)
                if(newgeo)then
                  call geowrite(IUF,LTMP,IICOMP,ITRU,3,IER)
                endif
              endif
            endif
          endif

C If there is an obstructions file, loop through each and
C transform origin.
          if(IOBS(IICOMP).EQ.1)then
            if(silent)then
              ok= .true.
            else
              write(outs,'(5a)') 'Transforms obstructions of ',
     &          zn(1:lnblnk(zn)),' in ',
     &          ZOBS(IICOMP)(1:lnblnk(ZOBS(IICOMP))),'?'
              call easkok(' ',outs,OK,nbhelp)
            endif
            if(OK)then
              CALL EGOMST(IUF,IICOMP,ZOBS(IICOMP),0,ITRC,ITRU,IER)
              do 63 ij=1,nbobs(iicomp)
                if(BLOCKTYP(iicomp,ij)(1:4).eq.'obs '.or.
     &             BLOCKTYP(iicomp,ij)(1:4).eq.'obs3')then
                  XOB(iicomp,ij)=XOB(iicomp,ij)+VALX
                  YOB(iicomp,ij)=YOB(iicomp,ij)+VALY
                  ZOB(iicomp,ij)=ZOB(iicomp,ij)+VALZ
                else
                  do ibe=1,8
                    XBP(iicomp,ij,ibe)=XBP(iicomp,ij,ibe)+VALX
                    YBP(iicomp,ij,ibe)=YBP(iicomp,ij,ibe)+VALY
                    ZBP(iicomp,ij,ibe)=ZBP(iicomp,ij,ibe)+VALZ
                  enddo
                endif
  63          continue
              CALL MKGOMST(IUF,ZOBS(IICOMP),IICOMP,IER)
            endif
          elseif(IOBS(IICOMP).EQ.2)then

C If obstructions are held in the geometry file
C apply the transform and write the file back out.
            if(newgeo)then
              if(silent)then
                ok= .true.
              else
                write(outs,'(3a)') 'Transforms obstructions of ',
     &            zn(1:lnblnk(zn)),'?'
                call easkok(' ',outs,OK,nbhelp)
              endif
              if(OK)then
                do 64 ij=1,nbobs(iicomp)
                  if(BLOCKTYP(iicomp,ij)(1:4).eq.'obs '.or.
     &               BLOCKTYP(iicomp,ij)(1:4).eq.'obs3')then
                    XOB(iicomp,ij)=XOB(iicomp,ij)+VALX
                    YOB(iicomp,ij)=YOB(iicomp,ij)+VALY
                    ZOB(iicomp,ij)=ZOB(iicomp,ij)+VALZ
                  else
                    do ibe=1,8
                      XBP(iicomp,ij,ibe)=XBP(iicomp,ij,ibe)+VALX
                      YBP(iicomp,ij,ibe)=YBP(iicomp,ij,ibe)+VALY
                      ZBP(iicomp,ij,ibe)=ZBP(iicomp,ij,ibe)+VALZ
                    enddo
                  endif
  64            continue
                call geowrite(IUF,LTMP,IICOMP,ITRU,3,IER)
              endif
            endif
          endif

          write(outs,'(3a)') ' Transforming: ',zn(1:lnblnk(zn)),
     &      '...done'
          if(.NOT.silent)CALL USRMSG(' ',outs,'-')

C Report to the user the implications of the recent change in the model.
          call sumrchg(IICOMP,'r',silent)
        endif
   83 continue

      return
      end

C ******************** globalrotate ********************
C Applies rotation to selected zones. Parameters:
C   val - the rotation angle (degrees, +ve anticlockwise)
C   x1,y1 - the X Y point to rotate around
C   izlistnb - the number of selected zones
C   izlist - an array holding the zones to rotate
C   silent - if true, do not ask user about rotations
C            of obstruction data.

C Stores the rotation directives in the rotateit array so
C that this action can be undone.

      subroutine globalrotate(val,x1,y1,izlistnb,izlist,silent,itru,ier)
      implicit none
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
#include "help.h"
      
      integer lnblnk  ! function definition
      integer izlistnb,izlist,itru,ier  ! type for passed parameters
      dimension izlist(MCOM)  ! array of which zones are selected for transform.
      real val        ! rotation angl
      real x1,y1      ! the point to rotate around
      logical silent  ! if true then skip dialogs

      integer ifil
      common/FILEP/IFIL
      integer iuout,iuin,ieout
      common/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      CHARACTER LAPROB*72
      
      logical newgeo  ! to use for testing if new/old geometry file.
      
      character*72 LTMP
      character ZN*12
      character outs*124
      logical OK,goforit
      integer itrc,iuf,iicomp,ij,iz
      integer ibe   ! loop for faces of obstruction
      real A,pi,XR,YR,XXX,YYY,CA,SA
      real angr   ! rotation angle
      integer i,inod,icmp
      real zoa,zvol

      helpinsub='edcfg'  ! set for subroutine
      helptopic='global_rotate_options'
      call gethelptext(helpinsub,helptopic,nbhelp)

      IUF=IFIL+2
      DO 84 IZ=1,izlistnb
        IF(izlist(IZ).GT.0)THEN
          IICOMP=izlist(IZ)
          call sumrchg(IICOMP,'i',silent)  ! clear the whats changed data
          write(zn,'(A)') zname(IICOMP)
          write(outs,'(3a)') ' Rotating: ',zn(1:lnblnk(zn)),'...'
          if(.NOT.silent)CALL USRMSG(' ',outs,'-')
          LTMP=LGEOM(IICOMP)

C Find out version number so we know if upgrade required.
          call eclose(gversion(IICOMP),1.1,0.01,newgeo)
          call georead(IUF,LTMP,IICOMP,1,iuout,IER)
          CALL ESCROT(VAL,x1,y1)
          iZBFLG(IICOMP)=0
  
          if(nbvis(iicomp).gt.0)then

C Visual object rotation.
            ANGR=VAL
            PI = 4.0 * ATAN(1.0)
            A=-ANGR*PI/180.
            CA=COS(A)
            SA=SIN(A)
            do ij=1,nbvis(iicomp)
              XXX=XOV(IICOMP,ij)-X1; YYY=YOV(IICOMP,ij)-Y1
              XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
              XOV(IICOMP,ij)=XR+X1; YOV(IICOMP,ij)=YR+Y1
              BANGOV(IICOMP,ij,1)=BANGOV(IICOMP,ij,1)+ANGR
              do ibe=1,8
                XXX=XVP(iicomp,ij,ibe)-X1
                YYY=YVP(iicomp,ij,ibe)-Y1
                XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
                XVP(iicomp,ij,ibe)=XR+X1
                YVP(iicomp,ij,ibe)=YR+Y1
              enddo  ! of ibe
            enddo    ! of ij
          endif

C Remember user directive for future un-do.
          rotateit(iicomp,1)=0.0
          rotateit(iicomp,2)=0.0
          rotateit(iicomp,3)=0.0
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(iicomp) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            call geowrite(IUF,LTMP,IICOMP,ITRU,3,IER)
          else
            call emkgeo(IUF,LTMP,IICOMP,3,IER)
          endif

C After transform update the g6 & g7 commons and find dependencies.
          call zgupdate(0,IICOMP,ier)
          call zinfo(iicomp,zoa,zvol,'q')

C Depending on the nature of the change update the common block pmchange.
          call warnmod(IICOMP,'str')

C Rotation of nodes and components associated with surfaces in the
C zone need to be dealt with.
          if(IAIRN.ge.1.and.ICAAS(IICOMP).ne.0)then
            call usrmsg(
     &      'Updating associated flow entities.',
     &      'Please check!','-')
            goforit=.false.
            PI = 4.0 * ATAN(1.0)
            A=-VAL*PI/180.0; CA=COS(A); SA=SIN(A)
            do ij=1,nzsur(iicomp)
              call doesflowrefsurface(iicomp,ij,inod,icmp)
              if(inod.gt.0)then  ! relocate node and alter orientation
                XXX=HNOD(INOD,1)-X1; YYY=HNOD(INOD,2)-Y1
                XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
                HNOD(INOD,1)=XR+X1; HNOD(INOD,2)=YR+Y1
                SUPNOD(INOD,2)=SUPNOD(INOD,2)+VAL
                goforit=.true.
              endif
              if(icmp.gt.0)then  ! relocate component
                XXX=HCMP(ICMP,1,1)-X1; YYY=HCMP(ICMP,1,2)-Y1
                XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
                HCMP(ICMP,1,1)=XR+X1; HCMP(ICMP,1,2)=YR+Y1
                goforit=.true.
              endif
            enddo

C Also reflect change in zone COG.
            i=ICAAS(iicomp)
            HNOD(i,1)=ZCOG(iicomp,1); HNOD(i,2)=ZCOG(iicomp,2)
            HNOD(i,3)=ZCOG(iicomp,3)
            write(outs,'(3a,3f7.3)') 'Updating ',NDNAM(i),' @',
     &        HNOD(i,1),HNOD(i,2),HNOD(i,3)
            call edisp(iuout,outs)
            if(goforit)then
              call updatebothflownetworks(ier)
            endif
          endif

C Rotate any MRT sensors associated with this zone and if newer
C geometry file update its block attributes as well.
          if (IVF(IICOMP).eq.1) then
            CALL ERMRT(ITRC,ITRU,IUF,LVIEW(IICOMP),IICOMP,IER)
            if (NCUB(IICOMP).gt.0) then
              if(silent)then
                ok= .true.
              else
                write(outs,'(3a)') 'Rotate MRT sensors of ',
     &                  zn(1:lnblnk(zn)),'?'
                call easkok(' ',outs,OK,nbhelp)
              endif
              if (OK) then
                PI = 4.0 * ATAN(1.0)
                A=-VAL*PI/180.0; CA=COS(A); SA=SIN(A)
                do 76 ij=1,NCUB(IICOMP)
                  XXX=XOC(ij)-X1; YYY=YOC(ij)-Y1
                  XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
                  XOC(ij)=XR+X1; YOC(ij)=YR+Y1
                  CANG(ij)=CANG(ij)+VAL
  76            continue
                CALL EMKMRT(LVIEW(IICOMP),LGEOM(IICOMP),
     &                      NZSUR(IICOMP),IUF,IICOMP,'v',IER)
                if(newgeo)then
                  call geowrite(IUF,LTMP,IICOMP,ITRU,3,IER)
                endif
              endif
            endif
          endif

C Rotate any obstructions associated with this zone.
          if(IOBS(IICOMP).EQ.1)then
            if(silent)then
              ok= .true.
            else
              write(outs,'(5a)') 'Rotate obstructions of ',
     &          zn(1:lnblnk(zn)),' in ',
     &          ZOBS(IICOMP)(1:lnblnk(ZOBS(IICOMP))),'?'
              call easkok(' ',outs,OK,nbhelp)
            endif
            if(OK)then
              CALL EGOMST(IUF,IICOMP,ZOBS(IICOMP),0,ITRC,ITRU,IER)
              PI = 4.0 * ATAN(1.0)
              A=-VAL*PI/180.
              CA=COS(A)
              SA=SIN(A)
              do 86 ij=1,nbobs(iicomp)
                XXX=XOB(iicomp,ij)-X1; YYY=YOB(iicomp,ij)-Y1
                XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
                if(BLOCKTYP(iicomp,ij)(1:4).eq.'obs '.or.
     &             BLOCKTYP(iicomp,ij)(1:4).eq.'obs3')then
                  XOB(iicomp,ij)=XR+X1; YOB(iicomp,ij)=YR+Y1
                  BANGOB(iicomp,ij,1)=BANGOB(iicomp,ij,1)+VAL
                else
                  do ibe=1,8
                    XXX=XBP(iicomp,ij,ibe)-X1
                    YYY=YBP(iicomp,ij,ibe)-Y1
                    XR=XXX*CA+YYY*SA
                    YR=YYY*CA-XXX*SA
                    XBP(iicomp,ij,ibe)=XR+X1
                    YBP(iicomp,ij,ibe)=YR+Y1
                  enddo
                endif
  86          continue
              CALL MKGOMST(IUF,ZOBS(IICOMP),IICOMP,IER)
              write(outs,'(3a)') ' Rotating: ',zn(1:lnblnk(zn)),
     &          '... done.'
              if(.NOT.silent)CALL USRMSG(' ',outs,'-')
              call sumrchg(IICOMP,'r',silent)
            endif
          elseif(IOBS(IICOMP).EQ.2)then

C If obstructions are held in the geometry file,
C apply the transform and write the file back out.
            if(newgeo)then
              if(silent)then
                ok= .true.
              else
                write(outs,'(3a)') 'Transforms obstructions of ',
     &          zn(1:lnblnk(zn)),'?'
                call easkok(' ',outs,OK,nbhelp)
              endif
              if(OK)then
                PI = 4.0 * ATAN(1.0)
                A=-VAL*PI/180.
                CA=COS(A)
                SA=SIN(A)
                do 87 ij=1,nbobs(iicomp)
                  XXX=XOB(iicomp,ij)-X1
                  YYY=YOB(iicomp,ij)-Y1
                  XR=XXX*CA+YYY*SA
                  YR=YYY*CA-XXX*SA
                  XOB(iicomp,ij)=XR+X1
                  YOB(iicomp,ij)=YR+Y1
                  BANGOB(iicomp,ij,1)=BANGOB(iicomp,ij,1)+VAL
                  do 90 ibe=1,8
                    XXX=XBP(iicomp,ij,ibe)-X1
                    YYY=YBP(iicomp,ij,ibe)-Y1
                    XR=XXX*CA+YYY*SA
                    YR=YYY*CA-XXX*SA
                    XBP(iicomp,ij,ibe)=XR+X1
                    YBP(iicomp,ij,ibe)=YR+Y1
  90              continue
  87            continue
                call geowrite(IUF,LTMP,IICOMP,ITRU,3,IER)
                write(outs,'(3a)') ' Rotating: ',zn(1:lnblnk(zn)),
     &            '... done.'
                if(.NOT.silent)CALL USRMSG(' ',outs,'-')
                call sumrchg(IICOMP,'r',silent)
              endif
            endif
          endif  ! obstr in geometry file
        endif
  84  continue   ! loop zones in list

      return
      end


C **************** globalscale
C globalscale is passed instructions on coordiate scaling to be globally
C applied to zones in an esp-r model. It knows about dependencies and
C will act on or warn users as appropriate (if in interactive mode).
C Parameters:
C   valx,valy,valz (real) are the scaling factors (1.0=100%0 for each axis.
C   izlistnb (integer) is the number of selected zones
C   izlist is the array holding selected zone indicies to transform
C   silent (logical) if true then do not ask user about transforms
C      of obstruction data.
      subroutine globalscale(valx,valy,valz,izlistnb,izlist,
     &  silent,itru,ier)
      implicit none
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
#include "help.h"
      
      integer lnblnk  ! function definition
      real valx,valy,valz  ! the scaling factors for each axis 
      integer izlistnb,izlist
      dimension izlist(MCOM)  ! array of which zones are selected for scaling.
      logical silent  ! if true then skip dialogs
      integer itru,ier  ! type for passed parameters

      integer ifil
      common/FILEP/IFIL
      integer iuout,iuin,ieout
      common/OUTIN/IUOUT,IUIN,IEOUT
            
      logical newgeo  ! to use for testing if new/old geometry file.
      
      character*72 LTMP
      character ZN*12
      character outs*124
      logical OK
      integer iuf   ! file type 
      integer iicomp  ! index of current zone
      integer itrc,ij,i,iz  ! types for local variables
      integer ibe   ! loop for faces of obstruction
      real zoa,zvol

      helpinsub='edcfg'  ! set for subroutine
      helptopic='global_transform_options'
      call gethelptext(helpinsub,helptopic,nbhelp)
       
      IUF=IFIL+2
      DO 83 IZ=1,izlistnb
        IF(izlist(IZ).GT.0)THEN
          IICOMP=izlist(IZ)
          call sumrchg(IICOMP,'i',silent)  ! clear the whats changed data
          write(zn,'(A)') zname(IICOMP)
          write(outs,'(3a)') ' Scaling: ',zn(1:lnblnk(zn)),'...'
          if(.NOT.silent)CALL USRMSG(' ',outs,'-')
          LTMP=LGEOM(IICOMP)

C Find out the version number so we know what to do with obstructions later.
          call eclose(gversion(IICOMP),1.1,0.01,newgeo)
          call georead(IUF,LTMP,IICOMP,1,iuout,IER)
          DO 62 I=1,NTV
            X(I)=X(I)*VALX
            Y(I)=Y(I)*VALY
            Z(I)=Z(I)*VALZ
   62     continue
          iZBFLG(IICOMP)=0
  
          if(nbvis(iicomp).gt.0)then

C Visual object origin scaling.
            do ij=1,nbvis(iicomp)
              XOV(iicomp,ij)=XOV(iicomp,ij)*VALX
              YOV(iicomp,ij)=YOV(iicomp,ij)*VALY
              ZOV(iicomp,ij)=ZOV(iicomp,ij)*VALZ
              do ibe=1,8
                XVP(iicomp,ij,ibe)=XVP(iicomp,ij,ibe)*VALX
                YVP(iicomp,ij,ibe)=YVP(iicomp,ij,ibe)*VALY
                ZVP(iicomp,ij,ibe)=ZVP(iicomp,ij,ibe)*VALZ
              enddo  ! of ibe
            enddo    ! of ij
          endif

C Decide whether to upgrade the geometry file format.
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(iicomp) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            call geowrite(IUF,LTMP,IICOMP,ITRU,3,IER)
          else
            call emkgeo(IUF,LTMP,IICOMP,3,IER)
          endif

C After scaling update the g6 & g7 commons and find dependencies.
          call zgupdate(0,IICOMP,ier)
          call zinfo(iicomp,zoa,zvol,'q')

C Depending on the nature of the change update the common block pmchange.
          call warnmod(IICOMP,'str')

C Scale origins of any MRT sensors associated with this zone and
C also update block attributes held in the geometry file.
          if (IVF(IICOMP).eq.1) then
C            NCUB(IICOMP) = 0
            CALL ERMRT(ITRC,ITRU,IUF,LVIEW(IICOMP),IICOMP,IER)
            if (NCUB(IICOMP).gt.0) then
              if(silent)then
                ok= .true.
              else
                write(outs,'(3a)') 'Scale MRT sensors positions of ',
     &               zn(1:lnblnk(zn)),'?'
                call easkok(' ',outs,OK,nbhelp)
              endif
              if (OK) then
                do 73 ij=1,NCUB(IICOMP)
                  XOC(ij)=XOC(ij)*VALX
                  YOC(ij)=YOC(ij)*VALY
                  ZOC(ij)=ZOC(ij)*VALZ
  73            continue
                CALL EMKMRT(LVIEW(IICOMP),LGEOM(IICOMP),
     &                      NZSUR(IICOMP),IUF,IICOMP,'v',IER)
                if(newgeo)then
                  call geowrite(IUF,LTMP,IICOMP,ITRU,3,IER)
                endif
              endif
            endif
          endif

C If there is an obstructions file, loop through each and
C scale origins.
          if(IOBS(IICOMP).EQ.1)then
            if(silent)then
              ok= .true.
            else
              write(outs,'(5a)') 'Scale obstruction origins of ',
     &          zn(1:lnblnk(zn)),' in ',
     &          ZOBS(IICOMP)(1:lnblnk(ZOBS(IICOMP))),'?'
              call easkok(' ',outs,OK,nbhelp)
            endif
            if(OK)then
              CALL EGOMST(IUF,IICOMP,ZOBS(IICOMP),0,ITRC,ITRU,IER)
              do 63 ij=1,nbobs(iicomp)
                XOB(iicomp,ij)=XOB(iicomp,ij)*VALX
                YOB(iicomp,ij)=YOB(iicomp,ij)*VALY
                ZOB(iicomp,ij)=ZOB(iicomp,ij)*VALZ
                do 88 ibe=1,8
                  XBP(iicomp,ij,ibe)=XBP(iicomp,ij,ibe)*VALX
                  YBP(iicomp,ij,ibe)=YBP(iicomp,ij,ibe)*VALY
                  ZBP(iicomp,ij,ibe)=ZBP(iicomp,ij,ibe)*VALZ
  88            continue
  63          continue
              CALL MKGOMST(IUF,ZOBS(IICOMP),IICOMP,IER)
            endif
          elseif(IOBS(IICOMP).EQ.2)then

C If obstructions are held in the geometry file
C apply the scaling to their origins and write the file back out.
            if(newgeo)then
              if(silent)then
                ok= .true.
              else
                write(outs,'(3a)') 'Scale obstruction origins of ',
     &            zn(1:lnblnk(zn)),'?'
                call easkok(' ',outs,OK,nbhelp)
              endif
              if(OK)then
                do 64 ij=1,nbobs(iicomp)
                  XOB(iicomp,ij)=XOB(iicomp,ij)*VALX
                  YOB(iicomp,ij)=YOB(iicomp,ij)*VALY
                  ZOB(iicomp,ij)=ZOB(iicomp,ij)*VALZ
                  do 89 ibe=1,8
                    XBP(iicomp,ij,ibe)=XBP(iicomp,ij,ibe)*VALX
                    YBP(iicomp,ij,ibe)=YBP(iicomp,ij,ibe)*VALY
                    ZBP(iicomp,ij,ibe)=ZBP(iicomp,ij,ibe)*VALZ
  89              continue
  64            continue
                call geowrite(IUF,LTMP,IICOMP,ITRU,3,IER)
              endif
            endif
          endif

          write(outs,'(3a)') ' Scaling: ',zn(1:lnblnk(zn)),
     &      '...done'
          if(.NOT.silent)CALL USRMSG(' ',outs,'-')

C Report to the user the implications of the recent change in the model.
          call sumrchg(IICOMP,'r',silent)
        endif
   83 continue

      return
      end
