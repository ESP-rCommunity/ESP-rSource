C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

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

C edcfg.f provides creation and editing facilities for system
C configuration files:
C  EDCFG   Control system level facilities and configuration file entities.
C  NEWPRB  Support specification of a new model.
C  visualz drives visualisation process - hidden line or raytracing.
C  EDZCOMP Control zone composition facilities.
C globaltransform is passed instructions on a transform to be globally
C           applied to zones in an esp-r model.
C globalrotate is passed instructions on a rotation to be globally
C           applied to zones in an esp-r model.

C ************* EDCFG 
C Control editing of system configuration file and allow updated 
C information to be saved into a new file. ITRU unit number for user 
C output; IER=0 indicates no error. 
      subroutine edcfg(ITRC,ITRU,IER)
      
      USE AIM2_Inputs, ONLY : AIM2_Interface, iAIM2
      
      IMPLICIT NONE
            
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G1/G2/G7/prec2/precz.
#include "geometry.h"
#include "uncertainty.h"
#include "net_flow.h"
#include "control.h"
#include "gnetwk.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure databases)
C LPRFDB,IPRODB (for event profile database)

C Function definition
      INTEGER :: lnblnk
      
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      INTEGER :: mmod,limit,limtty
      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: iuout,iuin
      common/pophelp/h(60)
      common/appw/iappw,iappx,iappy
      INTEGER :: iappw,iappx,iappy
      common/gzonpik/izgfoc,nzg,nznog(mcom)
      INTEGER :: izgfoc,nzg,nznog
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      REAL :: GRDIS
      INTEGER :: ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR
      INTEGER :: ITOBS,ITHLS,ITHLZ,ITGRD,ITPPSW
      common/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      REAL :: ZCOG,XMN,YMN,ZMN,XMX,YMX,ZMX
      
      common/C1/NCOMP,NCON
      INTEGER :: ncomp,ncon
      common/C6/INDCFG
      INTEGER :: indcfg
      common/C21/IFCFG,cfgroot,LCFGF
      INTEGER :: ifcfg
      COMMON/C24/IZSTOCN(MCOM,MS)
      INTEGER :: IZSTOCN

      common/user/browse

C Plant network.
      common/C23/IFPNF,LPNF
      INTEGER :: IFPNF

C Defaults.
      common/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Uncertainty.
      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      INTEGER :: nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres
      common/entfile/entflnam,ientxist

      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      INTEGER :: NPRE
      REAL :: FPRE
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      COMMON/MFLOW2/NDNAM(0:MNOD)
      common/MFLDOC/DEPRE(MPRD)
      COMMON/NWKGRD/GRMAX(3),GRSPC(3),GRLYRH(MLYRS)
      REAL :: GRMAX,GRSPC,GRLYRH
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW
      REAL :: SCALF,VIEWCEN,VIEWLIM
      INTEGER :: IVIEW
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKFLNAM,NWKTYPSTR(MNWKTYP)
      COMMON/ICONDBNAM/ICONDBFL      
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DICONDBFL
      COMMON/NWKTYP/INWKTYP,vergnf
      REAL :: vergnf
      COMMON/NWKGRDL/GON,SON
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)
      INTEGER :: NNICN,ICONTP,NICONATR,NCONP,ICNCT,IVEICN,NIVC,NIVE,NIVD
      INTEGER :: IVDOT,NIVA,IVARC,NIVL,IVLBL,NIVT
      REAL :: XYZICON,CONCP,VCICON
     
      COMMON/NWKCON/NICNN,ICNS(MNCNN),ICNE(MNCNN),ICNNT(MNCNN),
     & ICNSP(MNCNN),ICNEP(MNCNN),CNWNP(MNCNN,MCIP,3),
     & NCONWP(MNCNN),idatrdom(MNCNN),ddtagatr(MNCNN,MIATRB,5),
     & ddatrib(MNCNN,MIATRB,3),ddmenuatr(MNCNN,MIATRB)
      INTEGER :: NICNN,ICNS,ICNE,ICNNT,ICNSP,ICNEP,NCONWP,idatrdom
      REAL ::CNWNP

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      INTEGER :: NCONTM,NOCNTM
      logical DOK,OK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical MODIFY,MODLEN,MODBND,browse
      logical xst,clkok,havefile,unixok
      LOGICAL GON,SON,concat,remote
      integer iglib   ! to test for X11 interface

      dimension ITEMS(30)
      character LUALF*72,LCNG*15,LLOC*15,cfgroot*24
      character*72 LAPROB,DFILE,LPNF,tfile
      character longtfile*144,longtfiledos*144
      character LCFGF*72,H*72,ITEMS*34,ETEXT*82
      character NDNAM*12,LFIL*72
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF,DCNN,ltmp
      character tmode*8,doit*248
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,entflnam*72
      character DEPRE*40,dstmp*24,tab*1,outsn*124,outsd*124
      character CXITM*43,outs*124,fs*1
      CHARACTER*72 NWKNAM,NWKDSC,NWKFLNAM,ICONDBFL
      character ATRTAG*12,ATRMENU*32,ATRICN*12,NWKTYPSTR*12
      character ddtagatr*12,ddmenuatr*32,ddatrib*12
      character CONTMNAM*12
      character*72 DFCFD,DECMPDBFL,DICONDBFL
      character ltpath*72,filen*72
      character t144*144    ! for passing to erprcdb
      integer ictl
      
      INTEGER :: itrc,itru,ier,i,ic,icc,istat
      INTEGER :: icfg_type,icfgz,icfgn,icfgc,icfgdfn,iicfgz,iicfgn
      INTEGER :: iicfgc,iicfgdfn,iacc,icfoc,iclkok,idindcfg
      INTEGER :: ientxist,iier,im,ino,inwktyp,ios,ipedit,ipass,is
      INTEGER :: isindcfg,iuf,ium,iuo,iv,iver,iw,iz
      INTEGER :: iser,lenlfil,ln,ltrf,nets,nitems,szlow
      REAL :: szcog,szhigh
C Function definition      
      INTEGER :: igraphiclib

#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer ICPMOD                ! passed from tchild
#else
      integer*8 iside,isize,ifont   ! passed to viewtext
      integer*8 ICPMOD              ! passed from tchild
#endif

C HVAC
C Flag indicating presence of HVAC models:
C ihvacflag=1 indicates HVAC models are active; 0 indicates no HVAC models.
      common/hvacinfo/ihvacflag,hvacfile
      integer ihvacflag
      character hvacfile*72,HVAC_ITEMS*24
      integer nhvac_items
      integer hvind, indhvac, hvidind
      PARAMETER (nhvac_items = 5)
      dimension HVAC_ITEMS(nhvac_items)
      
C Menu flag
C To avoid the further use of goto loops, plan to use a do while loop with
C flags to return to the main menu instead of the goto 3 statment
      
C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Begin with high level menu, see how many networks and control loops
C are active.
      IOS=0   ! initial guess
      IUF=IFIL+2
    3 INO=-4
      nets=0
      if(indcfg.ge.2)nets=nets+1
      if(iairn.ge.1)nets=nets+1
      if(ientxist.gt.0)nets=nets+1
      if(ihvacflag.gt.0)nets=nets+1

      ictl=ncf+ncl+ncc+ngf+nof
      IER=0
      WRITE(ITEMS(1),'(A,A)') ' cfg file: ',LCFGF(1:22)
      IF(INDCFG.EQ.0)THEN 
        ITEMS(2)= 'a model domains >> registration'
      ELSEIF(INDCFG.EQ.1)THEN
        ITEMS(2)= 'a model domains >> building only'
      ELSEIF(INDCFG.EQ.2)THEN
        ITEMS(2)= 'a model domains >> plant only'
      ELSEIF(INDCFG.EQ.3)THEN
        ITEMS(2)= 'a model domains >> building/plant'
      ENDIF
      ITEMS(3)   ='b model context                '
      ITEMS(4)   =' _____________________________ '
      WRITE(ITEMS(5),'(A,I2,A)')' ... Zones.....(',NCOMP,' defined)'
      ITEMS(6)   ='c composition                ' 
      WRITE(ITEMS(7),'(A,I2,A)')' ... Networks......(',nets,' defined)'
      if(indcfg.ge.2.or.ihvacflag.gt.0)then
        ITEMS(8)='d plant & systems (defined)  '
      else
        ITEMS(8)='d plant & systems            '
      endif

C Check flags for air flow type
      IF ((iairn.lt.1).AND.(iAIM2.EQ.0)) THEN
        IF (iAIM2.EQ.1) THEN
          ITEMS(9)='e empirical air infiltration'
        ELSE 
          ITEMS(9)='e air flow                  '
        ENDIF
      ELSE
        if(iairn.eq.1)then
          ITEMS(9)='e network flow (menu)       '
        elseif(iairn.eq.2)then
          ITEMS(9)='e network flow (graphic)    '
        else
          ITEMS(9)='e network flow              '
        endif
      ENDIF
      if(ientxist.gt.0)then
        ITEMS(10)='f electrical (defined)      '
      else
        ITEMS(10)='f electrical                '
      endif
      if(nocntm.gt.0)then
        ITEMS(11)='g contaminant (defined)     '
      else
        ITEMS(11)='g contaminant               '
      endif
      WRITE(ITEMS(12),'(A,I2,A)')' ... Controls.....(',ictl,' defined)'
      if(ncf.eq.0)then
        WRITE(ITEMS(13),'(A)')     'j zones                       '
      else
        WRITE(ITEMS(13),'(A,I2,A)')'j zones (',ncf,' loops)       '
      endif
      if(ncl.eq.0)then
        WRITE(ITEMS(14),'(A)')     'k plant & systems '
      else
        WRITE(ITEMS(14),'(A,I2,A)')'k plant & systems (',ncl,' loops)'
      endif
      if(ncc.eq.0)then
        WRITE(ITEMS(15),'(A)')     'l network flow                '
      else
        WRITE(ITEMS(15),'(A,I2,A)')'l network flow (',ncc,' loops)'
      endif
      if(nof.eq.0)then
        WRITE(ITEMS(16),'(A)')     'm optics                '
      else
        WRITE(ITEMS(16),'(A,I2,A)')'m optics (',nof,' loops)'
      endif
      if(ngf.eq.0)then
        WRITE(ITEMS(17),'(A)')     'n global system '
      else
        WRITE(ITEMS(17),'(A,I2,A)')'n global system (',ngf,' loops)'
      endif
      if(nCFCctlloops.eq.0)then
        WRITE(ITEMS(18),'(A)')     'o complex fenestration '
      else
        WRITE(ITEMS(18),'(A,I2,A)')
     &   'o complex fenestration (',nCFCctlloops,' loops)'
      endif
      WRITE(ITEMS(19),'(A)')       ' _____________________________   '
      ITEMS(20)  ='p define uncertainties         '
      ITEMS(21)  =' _____________________________ '
      ITEMS(22)  =' ... Actions.....              '
      ITEMS(23)  ='q visualisation                '
      ITEMS(24)  ='r simulation                   '
      ITEMS(25)  ='s results analysis             '
      ITEMS(26)  ='t results & QA reporting       '
      ITEMS(27)  =' _____________________________ '
      ITEMS(28)  ='! save model                   '
      ITEMS(29)  ='? help                         '
      ITEMS(30)  ='- exit this menu               '
      nitems=30

C Help text for this menu.
      H(1)='A model, as defined within the system configuration file,'
      H(2)='comprises several entities such as simulation domain'
      H(3)='definition, site location, geometry, construction, usage'
      H(4)='and control. The system configuration file references'
      H(5)='other files where the actual defining data is held.'
      H(6)=' '
      H(7)='The structure of this menu reflects the entities of a'
      H(8)='system configuration file. The options may be used to'
      H(9)='define a model to be simulated.  For example, it is'
      H(10)='possible to initially define a single building zone and'
      H(11)='then add further zones, plant and control later. It is'
      H(12)='also possible to increase the resolution of a model by,'
      H(13)='adding in a CFD domain to represent zone air movement'
      H(14)='or plant component working fluid flow.'
      H(15)=' '
      H(16)='The `actions` section includes comissioning assessments'
      H(17)='of the model, recovering performance indidators, as'
      H(18)='well as generating QA reports on the composition of'
      H(19)='the model. '

C If user has defined model and perhaps resized the display then
C redraw the model image.
      if(indcfg.eq.2)then
        if(browse)then
          CALL EMENU('Browse/ Simulate',ITEMS,nitems,INO)
        else
          CALL EMENU('Browse/ Edit/ Simulate',ITEMS,nitems,INO)
        endif
      elseif(indcfg.eq.0)then
        if(browse)then
          CALL EMENU('Browse/ Simulate',ITEMS,nitems,INO)
        else
          CALL EMENU('Browse/ Edit/ Simulate',ITEMS,nitems,INO)
        endif
      else
        if(CFGOK.AND.MODIFY)then
          MODBND=.TRUE.
          MODLEN=.TRUE.
          ITSNM=1
          ITVNO=1
          nzg=NCOMP
          if(nzg.gt.0)then
            DO 44 I=1,nzg
              nznog(I)=I
  44        CONTINUE

C (Re)Set all surfaces to standard line width.
            CALL INLNST(1)
            izgfoc=0
            CALL ADJVIEW(IER)
          endif

C Re-draw the configuration buttons.
          if(MMOD.EQ.8)then
            call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
            call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
            WRITE(ETEXT,'(2A)')'Model: ',
     &        modeltitle(1:lnblnk(modeltitle))
            iside=1
            isize=1
            ifont=1
            call viewtext(etext,iside,isize,ifont)
          endif
        endif
        if(browse)then
          CALL EMENU('Browse/ Simulate',ITEMS,nitems,INO)
        else
          CALL EMENU('Browse/ Edit/ Simulate',ITEMS,nitems,INO)
        endif
      endif
      IF(INO.EQ.nitems)THEN

C Exit back to calling menu and clear dialogue box.
        CALL USRMSG(' ',' ','-')
        RETURN
      ELSEIF(INO.EQ.nitems-1)THEN

C Help text for the menu.
        CALL PHELPD('configuration images',19,'-',0,0,IER)
        goto 3
      ELSEIF(INO.EQ.nitems-2)THEN

C Save current common block information to file.
        if(browse)then
          call usrmsg('Cannot save the model while in browse',
     &                'mode: you must own the model!','W')
          goto 3
        endif

C If configuration file format is version 3 then check to see if there
C are any utility files. If found then ask if the user wants to save
C in older format.
        H(1)='The current model description will be written to'
        H(2)='this file.'
        DFILE=' '
        ltmp=LCFGF
   89   CALL EASKS(ltmp,' ','Update system configuration file?',
     &             72,DFILE,'update configuration file',IER,2)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          LCFGF=ltmp
        else
          call usrmsg('Please re-enter the file name.',' ','W')
          goto 89
        endif
        if(icfgv.eq.3.or.icfgv.eq.4)then

C Also check about connections file.
          if(ncon.gt.1)then
  289       H(1)='The surface topology of the model is held'
            H(2)='in the surface connections file.'
            write(DCNN,'(a,a)')cfgroot(1:lnblnk(cfgroot)),'.cnn'
            if(LCNN(1:1).eq.' ')LCNN=DCNN
            ltmp=LCNN
            CALL EASKS(ltmp,' ','Surface connections file name?',
     &        72,DCNN,'surface connx file name',IER,2)
            if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
              LCNN=ltmp
            else
              call usrmsg('Please re-enter the file name. ',' ','W')
              goto 289
            endif
          endif
        endif

        call tstamp('>','PRJ: save configuration')
        call tstamp('>',LCFGF)
        CALL EMKCFG('-',IER)
        IF(IER.EQ.1)THEN
          DOK=.true.
          h(1)='Check to see that the disk is not full and that you'
          h(2)='have permission to write to the folder.'
          CALL ASKOK('Problem creating new file!',
     &      'Try again?',OK,DOK,2)
          IF(OK)GOTO 89
        ENDIF
        goto 3
      ELSEIF(INO.EQ.1)THEN
        continue
      ELSEIF(INO.EQ.2)THEN

C Domains associated with the model.
        h(1)='The domains included in a model are automatically'
        h(2)='updated in the system configuration file as you add'
        h(3)='new information. While you can change the defined domain'
        h(4)='at any time, BE WARNED - if you change from a building'
        h(5)='only domain to a plant only domain, all zone data will'
        h(6)='be lost from your model.'
        h(7)=' '
        h(8)='The `registration level` domain holds only site'
        h(9)='information and images (i.e. no building/plant details).'
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
     &    ' ',' ',' ',' ',' ',' ',' ',' ',isindcfg,idindcfg,9)
        if(isindcfg.eq.0)then
          continue
        elseif(isindcfg-1.ne.indcfg)then
          dok=.false.
          h(1)='Normally the domains included in a model are updated'
          h(2)='automatically as new information is added. Changing'
          h(3)='the domain can have a significant impact: for example,'
          h(4)='if the domain is `building and plant` and you change'
          h(5)='it to `plant only`, all references to the building'
          h(6)='zones will be removed. The zone files will still exist'
          h(7)='but would need to be reattached to the model.'
          call askok('Are you sure you want to change the domains',
     &      'in the model? (you might lose information)',ok,dok,7)
          if(ok)then
            indcfg=isindcfg-1
            call usrmsg('Updating model domains...',' ','-')
            CALL EMKCFG('-',IER)
            call usrmsg('Updating model domains...done.',' ','-')
          endif
        endif
        IF(MMOD.EQ.8)then
          LN=max(1,LNBLNK(modeltitle))
          call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                 iicfgz,iicfgn,iicfgc,iicfgdfn)
          call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                 iicfgz,iicfgn,iicfgc,iicfgdfn)
          WRITE(ETEXT,'(2A)')'Model: ',modeltitle(1:ln)
          iside=1
          isize=1
          ifont=1
          call viewtext(etext,iside,isize,ifont)
        endif
      ELSEIF(INO.EQ.3)THEN

C Model context.
        IF(CFGOK)THEN
          CALL EDSITE(ITRC,IER)
        ELSE
          CALL USRMSG('Please define your model first!',' ','W')
        ENDIF
        goto 3
      elseif(INO.EQ.6)THEN

C Go to zones definintion menu.
        call EDZCOMP(ITRC,ITRU,IER)
      ELSEIF(INO.EQ.8)THEN

C ---------------------------------------------------------------------------------------------
C -----------------------------------PLANT NETWORK DESCRIPTION---------------------------------
C Plant network description, select either an idealized HVAC network (5 choices currently 
C available) or define and explicit plant network. If an idealized model is selected, choose 
C an option and set required parameters. If an explicit model is chosen, ask for plant network 
C file and then display it before starting up editing facilities.
C ---------------------------------------------------------------------------------------------

        IF(.NOT.CFGOK)THEN
          CALL USRMSG('Please define your model first!',' ','W')
          GOTO 3
        else
      
C Options for the plant network menu
          hvind=-1
          do while (hvind.ne.nhvac_items)
          
          if(ihvacflag.eq.0)then
            HVAC_ITEMS(1)='a Idealized           '
          elseif(ihvacflag.eq.1)then
            HVAC_ITEMS(1)='a Idealized (defined) '
          endif
          if(LPNF(1:4).eq.'UNKN'.or.LPNF(1:2).eq.'  ')then
            HVAC_ITEMS(2)='b Explicit            '
          elseif(LPNF(1:4).ne.'UNKN'.or.LPNF(1:2).ne.'  ')then
            HVAC_ITEMS(2)='b Explicit (defined)  '
          endif
          HVAC_ITEMS(3)  =' ___________________  '
          HVAC_ITEMS(4)  ='? help                '
          HVAC_ITEMS(5)  ='- exit this menu      '
          
C Help text for the plant network menu
          H(1)='Idealized model creates an .hvac file with    '
          H(2)='descriptions of main and backup systems.      '
          H(3)='Explicit creates a plant network file which   '
          H(4)='defines plant components and their connection '
          H(5)='topology.                                     '
          
          CALL EMENU('Plant Model',HVAC_ITEMS,nhvac_items,hvind)
          
          if(hvind.eq.nhvac_items)then
C If user does not wish to continue, exit back to calling menu and clear dialogue box.
            CALL USRMSG(' ',' ','-')
            GOTO 3
          elseif(hvind.eq.nhvac_items-1)then
            CALL PHELPD('Idealized or Explicit Plant Model?'
     &                    ,5,'-',0,0,IER)
          elseif(hvind.le.0)then
C If no selection is made, or user clicks of screen, do nothing and redraw menu.
          elseif(hvind.eq.1)then 
C If idealized plant model is desired, enter idealized HVAC subroutine.
            CALL HVACGIN()
          elseif(hvind.eq.2)then
C If explicit plant model is desired, continue to define an explicit plant network.
            
            if(LPNF(1:4).eq.'UNKN'.or.LPNF(1:2).eq.'  ')then
              dok=.false.
              h(1)='A plant network file holds the definition of a     '
              h(2)='system, the purpose of which is usually to supply  '
              h(3)='the building with heat, coolth and/or power. Unless'
              h(4)='you have access to a suitable components database, '
              h(5)='DO NOT use this facility.                          '
              CALL ASKOK(
     &        'Currently there is no plant network associated with',
     &        'the model. Create/select one?',OK,DOK,5)
              IF(.NOT.ok)GOTO 3

              if(netpth(1:2).eq.'  '.or.netpth(1:2).eq.'./')then
                WRITE(LPNF,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.pln'
              else
                WRITE(LPNF,'(4a)') netpth(1:lnblnk(netpth)),'/',
     &               cfgroot(1:lnblnk(cfgroot)),'.pln'
              endif
              clkok=.false.
              ltmp=LPNF
              H(1)='The plant network file defines plant components'
              H(2)='and their connection topology.'
              CALL EASKS(ltmp,' ','Plant network definition file?',72,
     &                   DPNF,'plant network file',IER,2)
              goto 46
            endif

            H(1)='The plant network file defines plant components and'
            H(2)='topology. If you supply an existing file it will be'
            H(3)='used, otherwise a new file will be created.'
            H(4)=' '
            H(5)='The dereference option removes the name of the plant'
            H(6)='network file from the system configuration file but'
            H(7)='does not delete it. The plant domain can later be'
            H(8)='re-established by rintrosucting the configuration'
            H(9)='file reference.'
            ltmp=LPNF
            iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
 301        if(iglib.eq.2)then
              CALL EASKSCMD(ltmp,'Plant network definition file?',' ',
     &                       'dereference',clkok,72,DPNF,
     &                        'plant network file',iser,9)
              if(clkok)iclkok=2   ! notice dereference button
              if(iser.eq.-3) goto 3  ! notice GTK cancel button
            else
              CALL EASKS2CMD(ltmp,'Plant network definition file?',' ',
     &          'cancel','dereference',iclkok,72,DPNF,
     &          'plant network file',iser,9)
              if(iclkok.eq.1) goto 3    ! notice X11 cancel option
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
              call usrmsg('Removing plant network...',' ','-')
              CALL EMKCFG('-',IER)
              call usrmsg('Removing plant network...done.',' ','-')
              goto 3
            endif

C Otherwise, check the supplied file name and either setup a new
C plant network or edit the existing network.
            if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
              LPNF=ltmp
            else
              call usrmsg(' ','Re-enter file name.','W')
              goto 301
            endif

            XST=.false.
            call FINDFIL(LPNF,XST)
            if(.not.xst) then
              dok=.false.
              h(1)='File not found, select `OK` if you want this file'
              h(2)='to hold the definition of a new plant network.'
              CALL ASKOK('This is a new plant network file.',
     &          'Proceed with network description?',OK,dok,2)
              ipedit=0
            else
              dok=.true.
              h(1)='Found an existing file, select `OK` if you want to'
              h(2)='use or modify it.'
              CALL ASKOK('Found existing plant network file.',
     &          'Use or modify it?',OK,dok,2)
              ipedit=1
            endif
            if(OK)then
              call tstamp('>','PRJ: enter epltnet')

C Reset the configuration type if this is appropriate, remember
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
     &            'No change in model recorded because of an error in',
     &            'the plant network description.','E')
              else
                call usrmsg('Updating model for plant network...',
     &            ' ','-')
                CALL EMKCFG('-',IER)
                call usrmsg('Updating model for plant network...done.',
     &            ' ','-')
              endif
            endif
          endif
          enddo  ! of while hvind ne nhvac_items
        endif
        
      ELSEIF(INO.EQ.9)THEN

C Flow description.
        H(1)='Air flow can be modelled using an empirical air'
        H(2)='infiltration model, sheduling flow by prescribing'
        H(3)='flowrates and imposing controls or by defining a'
        H(4)='detailed air flow network.'
        H(5)=''
        H(6)='The Alberta Air Infiltration Model (AIM-2) is used '
        H(7)='to describe the empirical air infilration model.'
        H(8)=''
        H(9)='It is possible to schedule air flowing from the'
        H(10)='outside (infiltration) and from specified zones or'
        H(11)='plant components (ventilation). This is sometimes'
        H(12)='appropriate at an early design stage and for flow'
        H(13)='problems of limited complexity.'
        H(14)=' '
        H(15)='The flow network approach employs flow components'
        H(16)='(doors, cracks, ducts, fans etc.) to explicitly'
        H(17)='represent the distributed flow paths. This approach'
        H(18)='requires significantly more defining information.'
        H(19)='(A related facility, described elsewhere, allows'
        H(20)='a CFD domain to be linked to this flow network so'
        H(21)='that intra-zone flows can be evaluated under the'
        H(22)='influence of the above flow network.)'
        H(23)=' '
        H(24)='Flow networks may be defined by selecting components'
        H(25)='from a menu or by graphical manipulation.'
        call EASKATOG('Define air flow via:',' ',
     &    'empirical air inflitration model','schedule',
     &     'flow network (menu)','flow network (graphic)',
     &    'cancel',' ',' ',IW,25)

C Use AIM2 interface     
        airflowtype: IF(IW.EQ.1)THEN
          CALL AIM2_Interface()
        ELSE IF(IW.EQ.2)THEN airflowtype 

C Operation file creation and editing. Set IVER to zero to signal
C that prjfmk is not being called from the versioning facility.
          IC=-1
 250      CALL EASKGEOF('Select a zone:',CFGOK,IC,'-',IER)
          IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
          call tstamp('>','PRJ: enter zone operations')
          iver=0
          CALL PRJFMK(ITRC,ITRU,IUO,IC,IER,iver)

C Loop back and see if another zone is requested.
          IC=-1
          GOTO 250
        ELSEIF(IW.EQ.3)THEN airflowtype
          call tstamp('>','PRJ: enter fluid network')
          IAIRN=1
          call MFPROB(IER)
        elseif(IW.eq.4)then airflowtype

C Guess graphic network flow file. If file has yet to be named then base
C it on cfgroot and place it in the netpth folder (differienciate between
C unix and non-unix machine types).
          IAIRN=2
          if(LAPROB(1:2).eq.'  '.or.LAPROB(1:4).eq.'UNKN')then
            if(unixok)then
              if(netpth(1:2).eq.'  '.or.netpth(1:2).eq.'./')then
                WRITE(LAPROB,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.gnf'
              else
                WRITE(LAPROB,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &          cfgroot(1:lnblnk(cfgroot)),'.gnf'
              endif
            else
              if(netpth(1:2).eq.'  '.or.(ichar(netpth(1:1)).eq.46
     &          .and.ichar(netpth(2:2)).eq.92))then
                WRITE(LAPROB,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.gnf'
              else
                WRITE(LAPROB,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &            cfgroot(1:lnblnk(cfgroot)),'.gnf'
              endif
            endif
          endif
          LFIL=LAPROB

C Graphic network. Create a summary file of zone domain information
C for `net` to use. Begin by reading current pressure database by
C passing a black string to erprcdeb. Write to unit number 42.
          call tstamp('>','PRJ: export to graphic agent')
          t144='  '
          CALL ERPRCDB(t144,0,3,IER)
          if (ier.ne.0) THEN
            CALL EDISP(IUOUT,
     &        'Error opening pressure coefficients db!.')
            call edisp(iuout,' ')
            goto 3
          endif 
          lenlfil=lnblnk(LFIL)
          write(ltmp,'(2a)',iostat=ios,err=13)
     &      LFIL(1:lenlfil-4),'.summary'
          CALL EFOPSEQ(42,ltmp,4,IER)
          tab=','
          call dstamp(dstmp)
          write(42,'(3a)',iostat=ios,err=13) '*Synopsis',tab,
     &      'for_flow'
          write(42,'(3a)',iostat=ios,err=13) '*Date',tab,dstmp
          write(42,'(3A)',iostat=ios,err=13) '*cfg',tab,
     &      LCFGF(1:lnblnk(LCFGF))
          write(42,'(2a,i3)',iostat=ios,err=13) '*Pressures',tab,
     &      NPRE
          do 201 IM=1,NPRE
            write(42,'(a)') DEPRE(IM)
  201     continue
          write(42,'(a)',iostat=ios,err=13) '*End_Pressures'
          write(42,'(2a,i3,a)',iostat=ios,err=13) '*Zones',tab,
     &       NCOMP,' # name, volume, centre @ XYZ'
          do 97 IZ=1,NCOMP
            write(outsn,'(a,1x,f7.1,3f8.3)') 
     &      zname(IZ),VOL(IZ),ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3)
            call SDELIM(outsn,outsd,'C',IW)
            write(42,'(a)') outsd(1:lnblnk(outsd))
  97      continue
          write(42,'(a)',iostat=ios,err=13) '*End_Zones'

C Write surface attributes that might be of interest. Find
C lowest and highest point and cog of surface.
          write(42,'(2a,i4,2a)',iostat=ios,err=13) '*Surfaces',tab,NCON,
     &      ' # connection, description, area, azimuth, elevation, ',
     &      ' Z Min/COG/Max'
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
  932         continue
              szcog = szcog / real(nver(is))
              icc=IZSTOCN(IZ,IS)
              CALL CONXMENU(icc,CXITM)
              write(outsn,'(a,1x,a,1x,6f9.3)',iostat=ios,err=13) 
     &          zname(IZ),SSNAME(icc),SSNA(icc),SSPAZI(icc),
     &          SSPELV(icc),szlow,szcog,szhigh
              call SDELIM(outsn,outsd,'C',IW)
              write(42,'(3a)',iostat=ios,err=13)
     &          CXITM(1:lnblnk(CXITM)),tab,outsd(1:lnblnk(outsd))
 931        continue
 930      continue
          write(42,'(a)',iostat=ios,err=13) '*End_Surfaces'
          CALL ERPFREE(42,ios)
          DFILE='network_flow.gnf'
          h(1)='This is the network graphics file generated by the'
          h(2)='`net` tool.'
          CALL EASKS(LFIL,' ','Network graphics file?',72,
     &      DFILE,'network file open',IER,2)

C Check if this file exists. If it does not fill a few of the
C graphic network commons and write out a minimal file for
C a flow domain.
          XST=.false.
          call FINDFIL(LFIL,XST)
          if(.not.xst) then
            call usrmsg('Creating a NEW minimal graphic network',
     &        'file for flow.','P')
            write(NWKDSC,'(2a)') 'Flow network for ',
     &        cfgroot(1:lnblnk(cfgroot))
            INWKTYP=2
            NWKTYPSTR(INWKTYP)='Flow'
            ICONDBFL=DICONDBFL
            IVIEW=1

C << TODO: take viewcen from wireframe view information. >>
            VIEWCEN(1)=5.0
            VIEWCEN(2)=5.0
            VIEWCEN(3)=3.0
            GRSPC(1)=0.5
            GRSPC(2)=0.5
            GRSPC(3)=1.0
            SON=.true.
            GON=.true.
            NNICN=0
            NICNN=0
            IM=1
            NWKFLNAM=LFIL
            call NETWRITE(IM)
          endif

C append path to new command line depending on whether Unix or Dos.
          call isunix(unixok)
          if(unixok)then
            call addpath(LFIL,longtfile,concat)
          else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
            call addpath(LFIL,longtfile,concat)
            call cmdfiledos(longtfile,longtfiledos,ier)
            longtfile=' '
            longtfile=longtfiledos
          endif

C Run the `net` module with graphic network file.
          write(doit,'(2a)')'net -file ',longtfile(1:lnblnk(longtfile))
          call runit(doit,'-')

          call edisp(iuout,'You will be asked to confirm the name of')
          call edisp(iuout,'the graphic flow network file produced by')
          call edisp(iuout,'the `net` program.')
          call edisp(iuout,' ')
          if(LFIL(1:2).ne.'  '.and.LFIL(1:7).ne.'UNKNOWN')then
            NWKFLNAM=LFIL
          else
            NWKFLNAM=DFILE
          endif
          if(IPRODB.eq.IFIL+6)then
            IUM=IPRODB
          else
            IUM=IFIL+6
          endif

C Free file unit IUM, initiat network flow commons, read the graphic
C network file (asking user the confirm name), convert into flow
C network commons and list out those commons.
          CALL ERPFREE(IUM,ISTAT)
          CALL MFCDAT
          CALL NETREAD(IUM,'R',IER)
          CALL NETTOFLW(ier)
          call mflist(iuout)

C Link nodes and zones.
          DO 33 IZ=1,NCOMP
            if(ICAAS(IZ).gt.0)then
              write(outs,'(3A)')zname(IZ)(1:lnzname(IZ)),
     &          'is currently linked to:',NDNAM(ICAAS(IZ))
              h(1)='You may have nodes that are not linked to a zone'
              h(2)='but where network nodes and zones are linked, the'
              h(3)='temperature of the former will be matched to the'
              h(4)='latter. '
              CALL EASKABC(outs,' ','ok',
     &          'select another','free link',iacc,4)
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
              write(outs,'(A,A)') zname(IZ)(1:lnzname(IZ)),
     &          'has no mass flow node.'
              CALL EASKAB(outs,' ','ok','select a node',iacc,3)
              if(iacc.eq.1)then
                ICAAS(IZ)=0
              elseif(iacc.eq.2)then
                IC=0
                call ASKRNOD('available nodes','-',IC,IER)
                ICAAS(IZ)=IC
              endif
            endif
  33        CONTINUE

C Save the configuration file so that connections and graphic mass flow
C network are know.
            dok=.true.
            h(1)='You have been working on an independant description'
            h(2)='of a flow network. To use it in a simulation you '
            h(3)='need to associate it with zones in the current' 
            h(4)='model.'
            CALL ASKOK('Save graphic flow network links to zones',
     &        'in the configuration file?',OK,dok,4)
            IF(OK)then
              IAIRN=2
              LAPROB=NWKFLNAM
              call edisp(iuout,'Updating configuration flow links...')
              CALL EMKCFG('-',IER)
              call edisp(iuout,
     &                  'Updating configuration flow links...done.')
              call edisp(iuout,' ')
            endif

C << version with external agent >>
C        elseif(IW.eq.4)then
C          call tstamp('>','PRJ: export to java agent')
C          doit = '/usr/esru/bin/flow_agent.sh'
C          call runit(doit,'-')
        ELSEIF(IW.EQ.5)THEN airflowtype
         GOTO 3 
        END IF airflowtype
      ELSEIF(INO.EQ.10)THEN

C Electrical network description.
        call tstamp('>','PRJ: enter electrical network')
        call ELECNET
      ELSEIF(INO.EQ.11)THEN

C Contaminant model description
        call tstamp('>','PRJ: enter contaminant model')
        call CTPROB
      ELSEIF(INO.EQ.13)THEN

C Zone control description.
        call tstamp('>','PRJ: enter zone control')
        icfoc=0
        CALL CONTRL(ITRC,ITRU,icfoc,IER)
        CTLOK=.TRUE.
      ELSEIF(INO.EQ.14)THEN

C Plant control description.
        call tstamp('>','PRJ: enter plant control')
        icfoc=1
        CALL CONTRL(ITRC,ITRU,icfoc,IER)
        CTLOK=.TRUE.
      ELSEIF(INO.EQ.15)THEN

C Flow control description.
        call tstamp('>','PRJ: enter flow control')
        icfoc=2
        CALL CONTRL(ITRC,ITRU,icfoc,IER)
        CTLOK=.TRUE.
      ELSEIF(INO.EQ.16)THEN

C Optical control description.
        call tstamp('>','PRJ: enter optics control')
        icfoc=5
        CALL CONTRL(ITRC,ITRU,icfoc,IER)
        CTLOK=.TRUE.
      ELSEIF(INO.EQ.17)THEN

C Global control description.
        call tstamp('>','PRJ: enter global control')
        icfoc=3
        CALL CONTRL(ITRC,ITRU,icfoc,IER)
        CTLOK=.TRUE.
      ELSEIF(INO.EQ.18)THEN

C Complex fenestration control description.
        call tstamp('>','PRJ: enter complex fen. control')
        icfoc=6
        CALL CONTRL(ITRC,ITRU,icfoc,IER)
        CTLOK=.TRUE.
      ELSEIF(INO.EQ.20)THEN

C Call uncertainty definition controller.
        H(1)='This option is password protected because it is'
        H(2)='under development and so must be used with care.'
        H(4)='The password is available from ESRU on request.'
        call easki(IPASS,' ','Password?',
     &                            0,'-',0,'-',0,'password',IER,4)
        if (IPASS.eq.101) then
          call tstamp('>','PRJ: enter uncertainty control')
          call UNCERTA(LUALF)
        endif
      ELSEIF(INO.EQ.23)THEN

C Select visualisation option.
        IF(CFGOK)THEN
          H(1)='In addition to the in-built perspective view you can'
          H(2)='request a hidden line view of the model or send'
          H(3)='it to be rendered by Radiance.'
          CALL EASKABC(' ','Visualisation options:','hidden line',
     &      'colour rendered','cancel',IW,3)
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
      ELSEIF(INO.EQ.24)THEN

C Call simulation controller.
        call tstamp('>','PRJ: enter simulation setup')
        call simula(ier)
      ELSEIF(INO.EQ.25)THEN

C Call results analysis module assuming same simulation parameter
C set as the last simulation. If isset not instanciated then set = 1.

C Logic to react when browsing a model. If remote set to true then
C the results file should be placed in the users home folder.
        remote=.false.
        if(browse)then
          remote=.true.
        else

C Check the configuration file name and if concat is true then
C prj was started up somewhere other than the cfg folder so
C results files should be saved to the users home folder.
C Note: logic in esrubps/reslib.F should be similar, but it
C makes use of calls to dealremote.
          ltmp = LCFGF
          call isunix(unixok)
          if(unixok)then
            call addpath(ltmp,longtfile,concat)
          else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
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

C Debug.
C        write(6,*) 'edcfg res remote is ',remote,nsset

        if(nsset.gt.0)then

C If there are simulation parameter sets use the names of the results
C files splres or sblres and prepend user home folder if remote is true.
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

C Debug.
C                write(6,*) 'remote so reset tfile to ',longtfile

              endif
              havefile=.true.
            else
              tfile=' '
              havefile=.false.
            endif
            call edisp(iuout,
     &            'Take a note of the results file used.')
            call edisp248(iuout,longtfile,100)
            call edisp(iuout,' ')
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

C Debug.
C                write(6,*) 'remote so reset tfile to ',longtfile

              endif
              havefile=.true.
            else
              tfile=' '
              havefile=.false.
            endif
            call edisp(iuout,
     &        'Take a note of the results file used.')
            call edisp248(iuout,longtfile,100)
            call edisp(iuout,' ')
          endif
        else

C There are no simulation parameter sets so take a guess at the
C file name that the user might have given.
C << update this logic to use a default name based on the root name >>
          if(browse)then
            write(longtfile,'(3a)') upath(1:lnblnk(upath)),fs,'libb'
          else
            tfile='libb'
          endif
          call edisp(iuout,'Take a note of the results file used.')
          call edisp248(iuout,longtfile,100)
          call edisp(iuout,' ')
        endif

        call tstamp('>','PRJ: beginning res')
        doit = ' '
        call tchild(ICPMOD)
        call termode(ICPMOD,tmode)

C However we got to this point, if Windows based look for spaces
C and or forward slashes.
        if(unixok)then
          continue
        else
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
        if(iappw.gt.0.and.iappw.le.200)then
          if(havefile)then
            write(doit,'(3a,3i4,3a)') 'res -mode ',tmode,
     &        ' -s ',iappw,iappx+35,iappy+45,' -file ',
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
        call usrmsg('Starting assessment recovery via',doit,'-')
        call runit(doit,tmode)

      ELSEIF(INO.EQ.26)THEN

C Execute editor and report generation tools.
        call tstamp('>','PRJ: enter report controller')
        call prjqa(ier)
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
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/gzonpik/izgfoc,nzg,nznog(mcom)
      common/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      common/RAY3/MODIFY,MODLEN,MODBND
     
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)

      common/user/browse
      common/appw/iappw,iappx,iappy
      common/cctlnm/ctldoc,lctlf
      common/cctl/icascf(mcom)

      common/GR3D100/BLDG3D,ZONE3D(MCOM)
      common/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)
      common/MOIST01/MSTROK,MSTRZN(MCOM)
      common/MOIST02/LMOIST(MCOM)
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
C =2 with more than 3 day types
      common/p3ver/ip3ver(MCOM)

      CHARACTER oprdesc*248,ventdesc*248,ctlstr*24
      COMMON/P1/oprdesc(MCOM),ventdesc(MCOM),ctlstr(MCOM,MDTY)
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Descriptive label for a zone load or casual gain types.
      character lodlabel*6
      common/loadlabel/lodlabel(mcom,MGTY)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      logical OK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,context
      logical MODIFY,MODLEN,MODBND,concat,browse,QUIET
      logical XST,OKOB,OKZT,OKZR
      logical BLDG3D,ZONE3D,MSTROK,MSTRZN,OKC,dok,clkok
      logical unixok
      logical silent

C Flag noting whether casual gain periods are currently sorted.
      logical sorted,problem
      logical newgeo  ! to use for testing if new/old geometry file.
      
      character ITEMS*32
      integer ivals
      dimension ITEMS(25),IVALS(MCOM)
      dimension jict(MS),jic2(MS),jie2(MS)
      dimension jsotf(MS),jsmlcn(MS),jsvfc(ms),jsother(ms,3)
      dimension jsname(MS),jsparent(MS),jsuse(ms,3)

      character*72 CFILE,DFILE,LTMP,LCFD
      character*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,LMOIST
      character LCFGF*72,H*72,SFIL*72
      CHARACTER jsmlcn*32,jsvfc*4,jsotf*32,jsother*24,jsname*12
      character jsparent*12,jsuse*8
      character doit*248,tmode*8,DSFIL*72,ZN*12
      character outs*124,outs248*248,cfgroot*24
      character ctldoc*248,LCTLF*72,lctmp*72,longtfile*144
      character HOLD*32,zd*64
      character lworking*144
      CHARACTER ETEXT*82
      character sfile*72,snpfile*72,fs*1
      CHARACTER LAPROB*72

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer itru,ier  ! type for passed parameters
      real val,x1,y1    ! type for passed parameters
      integer inpic     ! type for passed parameters
      integer iuf       ! file unit type
      integer IRT,irop  ! for radio buttons
      integer nitems,INO ! max items and current menu item

#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer ICPMOD     ! passed from tchild
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 ICPMOD     ! passed from tchild
#endif

C Set folder separator (fs) to \ or / as required.
      newgeo=.false.  ! assume older format geometry.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Begin with high level menu.
      IUF=IFIL+2
      OKOB=.false.  ! set before if else 
    3 INO=-3
      IER=0
      WRITE(ITEMS(1),'(A,I3,A)') ' .... Zones ....(',NCOMP,' defined)'
      ITEMS(2)   ='a geometry & attribution        '
      ITEMS(3)   ='b construction materials        '
      ITEMS(4)   ='c operational details           '
      ITEMS(5)   =' _____________________________  '
      WRITE(ITEMS(6),'(A,I4,A)') ' .... Topology ..(',NCON,
     &               ' connects)'
      ITEMS(7)   ='d surface connections & boundary'
      ITEMS(8)   ='e anchors (groups of surfaces)  '
      ITEMS(9)   =' _____________________________  '
      ITEMS(10)   =' .... Options ....              ' 
      ITEMS(11)  ='f shading & insolation          '   
      ITEMS(12)  ='g convection coefficients       ' 
      ITEMS(13)  ='h view factors & radiant sensors'
      ITEMS(14)  ='i casual gain control           '
      ITEMS(15)  ='j computational fluid dynamics  '
      ITEMS(16)  ='k adaptive gridding & moisture  '
      ITEMS(17)  =' _____________________________  '    
      ITEMS(18)  =' .... Special components ....   '
      ITEMS(19)  ='m integrated renewables'
      ITEMS(20)  ='n active materials              '
      ITEMS(21)  ='o advanced optics               '
      ITEMS(22)  =' _____________________________  '
      ITEMS(23)  ='* global tasks                  '
      ITEMS(24)  ='? help                          '
      ITEMS(25)  ='- exit this menu                '
      nitems=25

C If user has defined model and perhaps resized the display then
C redraw the model image.
      if(CFGOK.AND.MODIFY)then
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
          CALL ADJVIEW(IER)
        endif
        if(MMOD.eq.8)then
          call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
          call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
          WRITE(etext,'(2A)')'Model: ',modeltitle(1:lnblnk(modeltitle))
          iside=1
          isize=1
          ifont=1
          call viewtext(etext,iside,isize,ifont)
        endif
      endif

C Help text for this menu.
      H(1)='This menu provides facilities to create, browse'
      H(2)='and edit zone-level data such as geometry and'
      H(3)='construction, schedules and miscellaneous technical'
      H(4)='data relating to CFD, casual gain control etc.'
      H(5)=' '
      H(6)='The `Zones` section (geometry, construction, operations)'
      H(7)='are the essential attributes of zones which must be'
      H(8)='completed prior to simulation.'
      H(9)=' '
      H(10)='The `Topology` section allows you to automate the'
      H(11)='specification of what-is-on-the-other-side of surfaces'
      H(12)='in the model (which can also be done on a surface by'
      H(13)='surface basis within the geometry facility). Anchors'
      H(14)='can be thought of as named lists of surfaces e.g.'
      H(15)='a list called `finistration` might point to all windows'
      H(16)='in a facade to allow you to change them all to a new'
      H(17)='type of glazing.'
      H(18)=' '
      H(19)='The `Options` section allows you to add additional'
      H(20)='descriptions and or detail to your model as and when'
      H(21)='such concepts become important to your performance'
      H(22)='assessments. If you do not specify these additional'
      H(23)='model attributes default assumptions are made.'
      H(24)=' '
      H(25)='The `Special components` section also allows you to'
      H(26)='add detail to your model to support specialists'
      H(27)='tasks (e.g. assigning electrical characteristics to'
      H(28)='PV cells embedded within surfaces).'
      H(29)=' '
      H(30)='The `Global tasks` option allows you to transform and'
      H(31)='rotate the model on the site and do search & replace '
      H(32)='actions on surface attributes. '

      CALL EMENU('Zones Composition',ITEMS,nitems,INO)
      IF(INO.EQ.nitems)THEN

C Exit back to calling menu and clear dialogue box.
        CALL USRMSG(' ',' ','-')
        RETURN
      elseif(INO.EQ.nitems-1)THEN
        CALL PHELPD('zones definition',32,'-',0,0,IER)
      elseif(INO.EQ.nitems-2)THEN

C Global actions such as transforms, rotations, attributions. Use
C different list of options for X11 and GTK.
        h(1)='Transforms and rotations may be applied to one or more'
        h(2)='zones in the model. Global attribution allows all'
        h(3)='surfaces matching some criteria (e.g. vertical and'
        h(4)='opaque) to be assigned a particular attribute.'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        IRT=1
        if(iglib.eq.2)then
          call EASKATOG(' Global action?',' ',
     &      'transform','rotate','attribute','search & replace',
     &      ' ',' ',' ',IRT,4)
        else
          call EASKATOG(' Global action?',' ',
     &      'transform','rotate','attribute','search & replace',
     &      'cancel',' ',' ',IRT,4)
        endif
        if(IRT.eq.-3)then
          goto 3
        elseif(IRT.eq.5)then
          goto 3
        elseif(IRT.eq.1)then

C Ask for transform distance then which zones and then apply.
          H(1)='A transform shifts each zone & its solar obstructions'
          H(2)='by a given amount along each axis.'
          hold = ' 0.0  0.0  0.0'
  43      if(iglib.eq.2)then
            CALL EASKS(HOLD,'X, Y and Z transform (m)?',' ',
     &        32,' 0.0  0.0   0.0 ','transforms XYZ',ISER,2)
            if(iser.eq.-3) goto 3
          else
            clkok=.false.
            CALL EASKSCMD(HOLD,'X, Y and Z transform (m)?',' ',
     &        'cancel',clkok,32,' 0.0 0.0 0.0 ','transforms XYZ',IER,2)
            if(clkok) goto 3
          endif
          K=0
          CALL EGETWR(HOLD,K,VALX,-99.,99.,'W','X tr',IER)
          CALL EGETWR(HOLD,K,VALY,-99.,99.,'W','Y tr',IER)
          CALL EGETWR(HOLD,K,VALZ,-99.,99.,'W','Z tr',IER)
          if(ier.ne.0)goto 43
          H(1)=' Pick one, several or all zones for inclusion.'
          INPIC=NCOMP
          CALL EPICKS(INPIC,IVALS,' ','Zones to transform?',
     &                12,NCOMP,zname,'zone list',IER,1)
          IF(INPIC.EQ.0)GOTO 3

C Perform the global transform interactively.
          silent= .false.
          call globaltransform(valx,valy,valz,inpic,ivals,
     &      silent,itru,ier) 
          MODIFY=.TRUE.
          MODBND=.TRUE.

        elseif(IRT.eq.2)then

C Rotation degree and choices.
          H(1)='The rotation is applied to all selected zones about'
          H(2)='a common point or the site origin. Positive is  '
          H(3)='anticlockwise.'
          VAL=0.
          CALL EASKR(VAL,' ','Rotation (degrees, +ve anticlockwise)?',
     &                 -359.0,'W',359.0,'W',0.0,'rotation',IER,3)
          if(VAL.LT.-.01.OR.VAL.GT..01)then
            CALL EASKABC(' ','Rotation choice?',
     &        'site origin','user specified X & Y','cancel ?',IRTP,3)
            if(IRTP.eq.1)then
              x1=0.
              y1=0.
            elseif(IRTP.EQ.2)THEN
              H(1)='The point is relative to the site'
              H(2)='Tcoordinate system.'
              x1=0.
              CALL EASKR(x1,' ','X point (m)?',
     &          0.0,'-',0.0,'-',0.0,'x point',IER,2)
              y1=0.
              CALL EASKR(y1,' ','Y point (m)?',
     &          0.0,'-',0.0,'-',0.0,'y point',IER,1)
            elseif(IRTP.EQ.3)THEN
              goto 3
            endif
            H(1)='Pick one, several or all zones to rotate.'
            INPIC=NCOMP
            CALL EPICKS(INPIC,IVALS,' ','Zones to rotate?',
     &        12,NCOMP,zname,' zone list',IER,1)
            IF(INPIC.EQ.0)GOTO 3
            call tstamp('>','PRJ: zone rotation')

C Implement the rotations for the selected zones.
            silent= .false.
            call globalrotate(val,x1,y1,inpic,ivals,silent,itru,ier)

          endif
          MODIFY=.TRUE.
          MODBND=.TRUE.

        elseif(IRT.eq.3)then

          call serchrpl('a',itrc,iier)
          if(iier.eq.0)then
            call usrmsg('Surface attribute update',
     &        'has been sucessfully completed.','W')
          else
            call usrmsg('Surface attribute update',
     &        'was possibly unsucessful..','W')
          endif
          goto 3
        elseif(IRT.eq.4)then
          call serchrpl('c',itrc,iier)
          if(iier.eq.0)then
            call usrmsg('Construction attribute search & replace',
     &        'has been sucessfully completed.','W')
          else
            call usrmsg('Construction attribute search & replace',
     &        'was possibly unscuessful..','W')
          endif
          goto 3
        endif

      elseif(INO.EQ.2)THEN

C Geometry editing and browsing facilities. If creating zone from
C scratch the file name will be ' ' and IC will be NCOMP+1; otherwise
C read in the file data and then call the editing facility. Note:
C IC=-1 to flag addition of 'from scratch' in selection list.
        IC=-1

C Allow user to modify the zone list at this point. If no zones
C go directly to adding one.
  248   if(NCOMP.eq.0)then
          IC=1
        else
          CALL EASKGEOF('Select a zone to focus on:',CFGOK,IC,'M',IER)
        endif
        IF(IC.EQ.0.OR.IC.EQ.-1)THEN

C Return to configuration menu.
          GOTO 3
        ELSEIF(IC.lt.-10)THEN

C If IC < -10 then request to delete a zone: recover zone number.
          IC = ABS(IC) - 10
          call DELZONE(ITRC,IC,IER)
          IC = -1
          GOTO 3
        ELSEIF(IC.EQ.NCOMP+1)THEN

C Define a new zone and update nzg in case multiple zones read in.
          if(NCOMP.gt.MCOM-1)then
            call usrmsg('Maximum number of zones will be',
     &        'exceeded if a zone is added.','W')
            goto 3
          endif
          call NEWZONE(ITRC,IC,IER)
          if(IER.eq.-3)then
            goto 3   ! user cancel detected
          else
            nzg=NCOMP
          endif
        ELSEIF(IC.gt.100.and.IC.lt.200)THEN

C Copy a zone, get index by decrementing 100.
          if(NCOMP.gt.MCOM-1)then
            call usrmsg('Maximum number of zones will be',
     &        'exceeded if a zone is copied.','W')
            goto 3
          endif
          IC = IC - 100
          LTMP=LGEOM(IC)
          call tstamp('>','PRJ: copy zone')

C Recover connections associated with that zone.
          jixu=0
          do 35 IXU = 1,NCON
            if(IC1(IXU).eq.IC)then
              jixu=jixu+1
              jict(jixu)=ICT(IXU)
              jic2(jixu)=IC2(IXU)
              jie2(jixu)=IE2(IXU)
              jsotf(jixu)=SSOTF(IXU)
              jsmlcn(jixu)=SSMLCN(IXU)
              jsvfc(jixu)=SSVFC(IXU)
              jsother(jixu,1)=SSOTHER(IXU,1)
              jsother(jixu,2)=SSOTHER(IXU,2)
              jsother(jixu,3)=SSOTHER(IXU,3)
              jsuse(jixu,1)=SSUSE(IXU,1)
              jsuse(jixu,2)=SSUSE(IXU,2)
              jsname(jixu)=SSNAME(IXU)
              jsparent(jixu)=SSPARENT(IXU)
            endif
   35     continue

C If control exists confirm update to zone links (assume no control
C in the copied zone).  Begin by reading the existing control before
C incrementing the ncomp.
          OKC=.false.

C Debug.
C          write(6,*) 'lctlf is ',LCTLF

          if(LCTLF(1:2).eq.'  '.or.LCTLF(1:4).eq.'UNKN')then
            continue
          else
            dok=.true.
            h(1)='When you copy a zone, the control file needs to be'
            h(2)='updated to reflect the additional zone. Separately'
            h(3)='(in the control interface) you must indicate which'
            h(4)='control loop to use with the copied zone. The'
            h(5)='initial assumption is that it free-floats. '
            CALL ASKOK(
     &        'Control are zone based and should be updated to',
     &        'reflect the copied zone. Ok to do this?',OKC,dok,5)
          endif
          if(OKC)then
            H(1)='The (optional) system control file holds the '
            H(2)='definition of all building/plant controls. '
            H(3)='A free-flot control has been associated with the'
            H(4)='copied zone.'
            lctmp=LCTLF
            CALL EASKS(lctmp,' Control file ? ',
     &        ' ',72,'std.ctl','Control file',IER,4)
            if(lctmp(1:2).ne.'  '.and.lctmp(1:4).ne.'UNKN')then
              lctlf=lctmp
              ICTLF=IFIL+2
              CALL ERPFREE(ICTLF,ISTAT)
              call FINDFIL(LCTLF,XST)
              if(XST)then
                CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
              else
                OKC=.false.
              endif
            endif
          endif

C Read the geometry of the source zone to fill in the data below.
C Make the source zone and the destination zone the same version
C number. When reading in the source zone use the updated ncomp
C zone index so that the zone related commons are filled.
          NCOMP=NCOMP+1
          call eclose(gversion(IC),1.1,0.01,newgeo)
          gversion(ncomp)=gversion(ic)  ! set to same version
          call georead(IFIL+1,LTMP,ncomp,1,iuout,IER)
          IF(IER.NE.0)THEN
            dok=.true.
            h(1)='While scanning the file a problem was noted. If you'
            h(2)='think this might be because the wrong file name was'
            h(3)='given you can try it again. '
            CALL ASKOK('Problem reading geometry file.','Try again?',
     &        OK,dok,3)
            if(OK)then
              goto 248
            else
              goto 3
            endif
          ENDIF

          H(1)='The name of the zone is used both for descriptive'
          H(2)='and bookkeeping purposes.  Each name should be'
          H(3)='unique and should not contain blanks. '
 42       ZN=' '
          CALL EASKS(ZN,' What do you want to call this copied zone ',
     &      ' ( <12 char, no blanks) ?',12,'new_zone','zone name',IER,3)
          IF(ZN.eq.' '.or.ier.ne.0)goto 42
          call st2name(ZN,zname(NCOMP))
          lnzname(NCOMP)=lnblnk(zname(NCOMP))
          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(DFILE,'(A,A4)') zname(NCOMP)(1:lnzname(NCOMP)),'.geo'
          else
            WRITE(DFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &        zname(NCOMP)(1:lnzname(NCOMP)),'.geo'
          endif
          LGEOM(NCOMP)=DFILE

          H(1)='The zone description allows information about'
          H(2)='zone form and composition to be recorded.'
          write(zd,'(2a)') 
     &      zname(NCOMP)(1:lnzname(NCOMP)),' describes a '
  40      CALL EASKS(zd,'What does it represent?',
     &      ' (<64 char)',64,'no description entered','zone descr',
     &      IER,2)
          IF(zd.eq.' '.or.ier.ne.0)goto 40
          zdesc(NCOMP)=zd
          lnzdesc(NCOMP)=lnblnk(zd)  ! update the length of this string.

C If an obstruction file check if this should be copied as well.
C Save zone geometry, read in obstructions, write out to another
C file and then recover the zone commons.
          if(IOBS(ic).eq.1)then
            dok=.true.
            OKOB=.true.
            h(1)='The original zone had associated solar obstructions'
            h(2)='and you have two options '
            h(3)=' a) copy the obstructions so that modifications'
            h(4)='    relevant to the copied zones obstructions can'
            h(5)='    be made.'
            h(6)=' b) do not copy the obstructions. Optionally you'
            h(7)='    can reference the original obstructions (via'
            h(8)='    the project files menu).'
            CALL ASKOK(' ','Also copy obstructions of source zone?',
     &        OKOB,dok,8)
            if(OKOB)then
              CALL ESCZONE(NCOMP)
              call FINDFIL(ZOBS(IC),XST)
              IF(XST)THEN
                CALL EGOMST(IUF,IC,ZOBS(IC),0,ITRC,ITRU,IER)
                IOBS(NCOMP)=1 
                if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                  WRITE(DFILE,'(2a)') 
     &              zname(NCOMP)(1:lnzname(NCOMP)),'.obs'
                else
                  WRITE(DFILE,'(4a)') 
     &              zonepth(1:lnblnk(zonepth)),fs,
     &              zname(NCOMP)(1:lnzname(NCOMP)),'.obs'
                endif
                ZOBS(NCOMP)=DFILE
                CALL MKGOMST(IUF,ZOBS(NCOMP),NCOMP,IER)
              endif
              CALL ERCZONE(NCOMP)
            endif
          elseif(IOBS(ic).eq.2)then
            continue   ! obstr within geometry file so copy not required.
          endif

C Zone rotations and transforms.
          if(OKOB)then
            H(1)='When copying a zone and obstructions the copy is'
            H(2)='usually shifted away from the position of the '
            H(3)='source. You can skip such transforms.'
          else
            H(1)='When copying a zone the copy is usually shifted'
            H(2)='away from the position of the source.  You can '
            H(3)='skip such transforms. '
          endif
          H(4)='Transform shifts the zone by a given amount in'
          H(5)='each axis.'
          hold = ' 0.0  0.0  0.0'
          clkok=.false.
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
 243      if(iglib.eq.2)then
            CALL EASKSCMD(HOLD,'Transform (X Y Z metres):',' ',
     &        'skip transform',clkok,32,' 0. 0. 0.','transforms XYZ',
     &        iser,5)
            if(clkok)iclkok=2   ! notice skip transform button
            if(iser.eq.-3)then  ! notice GTK cancel button
              LGEOM(NCOMP)=' '
              NCOMP=NCOMP-1
              goto 3
            endif
          else
            CALL EASKS2CMD(HOLD,'Transform (X Y Z metres):',' ',
     &        'cancel copy','skip transform',iclkok,32,' 0. 0. 0.',
     &        'transforms XYZ',iser,5)
            if(iclkok.eq.1)then    ! notice X11 cancel option
              LGEOM(NCOMP)=' '
              NCOMP=NCOMP-1
              goto 3
            endif
          endif 

C Act on the skip transform request.   
          if(iclkok.eq.2) then
            okzt=.false.
          else
            K=0
            CALL EGETWR(HOLD,K,VALX,-99.,99.,'W','zone X trnsf',IER)
            CALL EGETWR(HOLD,K,VALY,-99.,99.,'W','zone Y trnsf',IER)
            CALL EGETWR(HOLD,K,VALZ,-99.,99.,'W','zone Z trnsf',IER)
            if(ier.ne.0)goto 243
            DO 162 I=1,NTV
              X(I)=X(I)+VALX
              Y(I)=Y(I)+VALY
              Z(I)=Z(I)+VALZ
  162       continue
            iZBFLG(NCOMP)=0
            okzt=.true.
          endif

C Record rotation information in rotateit array.
          dok=.false.
          h(1)='If you want to rotate the zone from its initial'
          h(2)='position now is a good time to do this if the '
          h(3)='point of rotation is the first vertex of the zone.'
          CALL ASKOK(' ',' Rotate the copied zone',OKZR,dok,3)
          if(OKZR)then
            H(1)='The rotation is applied around vertex 1,'
            H(2)='positive is anticlockwise.'
            VAL=0.
            CALL EASKR(VAL,' ',' Rotation (deg., +ve anticlockwise)?',
     &        -359.0,'W',359.0,'W',0.0,'rotation',IER,2)
            if(VAL.LT.-.01.OR.VAL.GT..01)then

C Rotation choices. Clear pending rotateit, update prior rotateit.
             rotateit(ncomp,1)=VAL
             H(1)='The default rotation is about vertex 1 of the'
             H(2)='zone with a positive angle denoting anticlockwise.'
             H(3)='You may change this to rotate the zone about the'
             H(4)='site origin or a specified point.'
              CALL EASKABCD(' ','Rotation choices:',
     &         'vertex 1 of zone','site origin',
     &         'specified X & Y','skip rotation',IRT,4)
              call usrmsg(' ',' ','-')
              if(IRT.eq.1)then
                x1=X(1)
                y1=Y(1)
                rotateit(ncomp,2)=x1
                rotateit(ncomp,3)=y1
                CALL ESCROT(VAL,x1,y1)
                rotateit(ncomp,4)=rotateit(ncomp,1)   ! shift the pending rotation
                rotateit(ncomp,5)=rotateit(ncomp,2)   ! and the origin of the zone
                rotateit(ncomp,6)=rotateit(ncomp,3)
                rotateit(ncomp,1)=0.0
                rotateit(ncomp,2)=0.0
                rotateit(ncomp,3)=0.0
              elseif(IRT.EQ.2)THEN
                x1=0.
                y1=0.
                rotateit(ncomp,2)=x1
                rotateit(ncomp,3)=y1
                CALL ESCROT(VAL,x1,y1)
                rotateit(ncomp,4)=rotateit(ncomp,1)   ! shift the pending rotation
                rotateit(ncomp,5)=rotateit(ncomp,2)   ! and the origin of the zone
                rotateit(ncomp,6)=rotateit(ncomp,3)
                rotateit(ncomp,1)=0.0
                rotateit(ncomp,2)=0.0
                rotateit(ncomp,3)=0.0
              elseif(IRT.EQ.3)THEN
                H(1)='Point is in the site coordinate system.'
                x1=0.
                CALL EASKR(x1,' ',' X point (metres) ? ',
     &            0.0,'-',0.0,'-',0.0,'x point',IER,1)
                y1=0.
                CALL EASKR(y1,' ',' Y point (metres) ? ',
     &            0.0,'-',0.0,'-',0.0,'y point',IER,1)
                rotateit(ncomp,2)=x1
                rotateit(ncomp,3)=y1
                CALL ESCROT(VAL,x1,y1)
                rotateit(ncomp,4)=rotateit(ncomp,1)   ! shift the pending rotation
                rotateit(ncomp,5)=rotateit(ncomp,2)   ! and the origin of the zone
                rotateit(ncomp,6)=rotateit(ncomp,3)
                rotateit(ncomp,1)=0.0
                rotateit(ncomp,2)=0.0
                rotateit(ncomp,3)=0.0
                call usrmsg(' ',' ','-')
              elseif(IRT.EQ.4)THEN
                rotateit(ncomp,1)=0.0
                rotateit(ncomp,2)=0.0
                rotateit(ncomp,3)=0.0
              endif
              iZBFLG(NCOMP)=0
            endif
          endif

C Because the writing of geometry files is based on G6 common
C block data update connections based info prior to writing
C the new geometry file. Retain similar and adiabatic connections if found.
          NZSUR(NCOMP)=NSUR
          NZTV(NCOMP)=NTV
          NCCODE(NCOMP)=NCOMP
          nzg=NCOMP
          ICCC=NCON
          DO 132 ICC=1,NSUR
            ICCC=ICCC+1
            IC1(ICCC)=NCOMP
            IE1(ICCC)=ICC 
            IZSTOCN(ncomp,icc)=iccc
            SSOTF(ICCC)=jsotf(icc)
            SSMLCN(ICCC)=jsmlcn(icc)
            SSVFC(ICCC)=jsvfc(icc)
            SSOTHER(ICCC,1)=jsother(icc,1)
            SSOTHER(ICCC,2)=jsother(icc,2)
            SSOTHER(ICCC,3)=jsother(icc,3)
            SSUSE(ICCC,1)=jsuse(icc,1)
            SSUSE(ICCC,2)=jsuse(icc,2)
            SSNAME(ICCC)=jsname(icc)
            SSPARENT(ICCC)=jsparent(icc)
            if(jict(ICC).eq.0)then
              ICT(ICCC)=0
              IC2(ICCC)=0
              IE2(ICCC)=0
            elseif(jict(ICC).eq.1.or.jict(ICC).eq.2)then
              ICT(ICCC)=jict(ICC)
              IC2(ICCC)=jic2(ICC)
              IE2(ICCC)=jie2(ICC)
            elseif(jict(ICC).eq.3)then
              SOTHER(ICC,1)='UNKNOWN'
              SOTHER(ICC,2)='0'
              SOTHER(ICC,3)='0'
              SSOTHER(ICCC,1)='UNKNOWN'
              SSOTHER(ICCC,2)='0'
              SSOTHER(ICCC,3)='0'
              ICT(ICCC)=0
              IC2(ICCC)=0
              IE2(ICCC)=0
            elseif(jict(ICC).eq.4.or.jict(ICC).eq.5)then
              ICT(ICCC)=jict(ICC)
              IC2(ICCC)=jic2(ICC)
              IE2(ICCC)=jie2(ICC)
            endif
  132     CONTINUE
          NCON=ICCC
          call zgupdate(0,ncomp,ier)

C Find co-planer surfaces and edges of similar materials.
          call suredgeadj(itrc,'-',ncomp,ier)

C Update the global coordinates for this zones surfaces so that
C the subsequent wireframe image can be drawn and the bounds of
C the zone can be calculated.
          DO 41 J=1,NZTV(ncomp)
            VCOORD(ncomp,J,1)=X(J)
            VCOORD(ncomp,J,2)=Y(J)
            VCOORD(ncomp,J,3)=Z(J)
   41     CONTINUE

C Now it is ok to write out the copied zone. If upgrading
C ensure that we use the latest version.
          call eclose(gversion(NCOMP),1.1,0.01,newgeo)
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(ncomp) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            iuf=ifil+2
            call geowrite(iuf,LGEOM(NCOMP),NCOMP,ITRU,3,IER)
          else
            iuf=ifil+2
            call emkgeo(iuf,LGEOM(NCOMP),NCOMP,3,IER)
          endif
          IF(IER.EQ.1)THEN
            dok=.true.
            h(1)='When writing the geometry file a problem was found.'
            h(2)='Check that you have permission to update the file '
            h(3)='and that there is sufficient disk space. '
            CALL ASKOK(' ','Problem creating geometry...try again?',
     &        OK,dok,3)
            IF(OK)GOTO 42
          ENDIF

C Do same transformation to obstructions? If so data is in common
C so modify common block data. Take into account location of
C obstruction data.
          if(OKOB)then
            CALL ESCZONE(NCOMP)
            if(OKZT)then
              do 85 ij=1,nbobs(ncomp)
                XOB(ncomp,ij)=XOB(ncomp,ij)+VALX
                YOB(ncomp,ij)=YOB(ncomp,ij)+VALY
                ZOB(ncomp,ij)=ZOB(ncomp,ij)+VALZ
                do 95 ibe=1,8
                  XBP(ncomp,ij,ibe)=XBP(ncomp,ij,ibe)+VALX
                  YBP(ncomp,ij,ibe)=YBP(ncomp,ij,ibe)+VALY
                  ZBP(ncomp,ij,ibe)=ZBP(ncomp,ij,ibe)+VALZ
   95           continue
   85         continue
            endif
            if(OKZR)then
              PI = 4.0 * ATAN(1.0)
              A=-VAL*PI/180.
              CA=COS(A)
              SA=SIN(A)
              do 88 ij=1,nbobs(ncomp)
                XXX=XOB(ncomp,ij)-X1
                YYY=YOB(ncomp,ij)-Y1
                XR=XXX*CA+YYY*SA
                YR=YYY*CA-XXX*SA
                XOB(ncomp,ij)=XR+X1
                YOB(ncomp,ij)=YR+Y1
                BANGOB(ncomp,ij,1)=BANGOB(ncomp,ij,1)+VAL
                do 89 ibe=1,8
                  XXX=XBP(ncomp,ij,ibe)-X1
                  YYY=YBP(ncomp,ij,ibe)-Y1
                  XR=XXX*CA+YYY*SA
                  YR=YYY*CA-XXX*SA
                  XBP(ncomp,ij,ibe)=XR+X1
                  YBP(ncomp,ij,ibe)=YR+Y1
  89            continue
  88          continue
            endif

C If geometry file can hold obstructions save them there
C otherwise write out write out zone obstruction file.
            call eclose(gversion(ncomp),1.1,0.01,newgeo)
            if(newgeo)then
              iuf=ifil+2
              call geowrite(iuf,LGEOM(NCOMP),NCOMP,ITRU,3,IER)
              IOBS(NCOMP)=2    ! reset to signal obs in geom file
              ZOBS(NCOMP)='  '
            else
              CALL MKGOMST(IUF,ZOBS(NCOMP),NCOMP,IER)
            endif
            CALL ERCZONE(NCOMP)
          endif

C If control exists update zone links (assume no control
C in the copied zone).
          if(OKC)then
            icascf(NCOMP)=0
            CALL CTLWRT(ICTLF,IER)
          endif

C Ask if user wants to copy or point to other zone files.
C Construction and zone TMC file.
          call FINDFIL(LTHRM(IC),XST)
          irop=1
          if(XST)then
            h(1)='You can have the new zone constructions based'
            h(2)='on the source zone (by copying the file)'
            h(3)='or by pointing to the file (exercise care)'
            h(4)='or browse available files or continue without'
            h(5)='defining constructions at this time.'
            CALL EASKATOG(' ','Source zone has a constructions file:',
     &        'copy source file','use source file',
     &        'browse','cancel',' ',' ',' ',irop,5)
          else
            h(1)='You can have the new zone constructions by'
            h(2)='browing available files or continue without'
            h(3)='defining constructions at this time.'
            CALL EASKATOG(' ','Source zone has no constructions file:',
     &        '-','-','browse','cancel',' ',' ',' ',irop,3)
            if(irop.eq.1.or.irop.eq.2) irop=4
          endif

C DFILE is the default file name for any TMC file to be created,
C CFILE is default for constr file.
          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(DFILE,'(A,A4)')zname(NCOMP)(1:lnzname(NCOMP)),'.tmc'
            WRITE(CFILE,'(A,A4)')zname(NCOMP)(1:lnzname(NCOMP)),'.con'
          else
            WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &        zname(NCOMP)(1:lnzname(NCOMP)),'.tmc'
            WRITE(CFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &        zname(NCOMP)(1:lnzname(NCOMP)),'.con'
          endif
          if(irop.eq.1)then

C Read in source constructions and write out to file for new zone.
C Also update lodlabel commons before writing out.
            CALL ERPFREE(IUF,ISTAT)
            CALL ECONST(LTHRM(IC),IFIL+1,IC,0,IUOUT,IER)
            if(ITW(IC).eq.1)then
              CALL ERTWIN(ITRC,IUOUT,IUF,LTWIN(IC),IC,IER)
            endif
   90       H(1)='A zone construction file contains thermophysical'
            H(2)='data for the zone surfaces.'
            CALL EASKS(CFILE,' New construction file name:',
     &        ' ',72,'new.con','construction file',IER,2)
            IF(CFILE.NE.'  ')THEN
              QUIET=.false.
              write(LTHRM(NCOMP),'(a)') CFILE(1:lnblnk(CFILE))
              CALL EMKCON(LTHRM(NCOMP),IUF,NCOMP,QUIET,IER)
              if(ITW(IC).eq.1)then

C Update itmcfl for the new zone.
                do 61 ISS=1,NZSUR(NCOMP)
                  ITMCFL(NCOMP,ISS)=ITMCFL(IC,ISS)
   61           continue
                H(1)='A zone tmc file contains optical data for the'
                H(2)='data zone surfaces.'
                CALL EASKS(DFILE,' ','New zone tmc file name:',
     &                         72,'new.tmc','tmc file',IER,2)
                if(DFILE(1:2).NE.'  ')then
                  write(LTWIN(NCOMP),'(a)') DFILE(1:lnblnk(DFILE))
                  ITW(NCOMP)=1
                  QUIET=.false.
                  CALL MKTWIN(IUF,NCOMP,QUIET,IER)
                else
                  goto 90
                endif
              endif
            else
              goto 90
            endif
          elseif(irop.eq.2)then

C Point to source zones file.
            LTHRM(NCOMP)=LTHRM(IC)
            LTWIN(NCOMP)=LTWIN(IC)
            ITW(NCOMP)=1
          elseif(irop.eq.3)then

C Find out if there are construction files in the model ../zones folder.
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','con',sfile,snpfile,nfile,
     &          iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','con',sfile,snpfile,
     &          nfile,iier)
              if(snpfile(1:2).ne.'  ')then
                write(CFILE,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))

C Debug.
C                write(6,*) CFILE

                LTHRM(NCOMP)=CFILE
              endif
            endif

C Find out if there are tmc files in the model ../zones folder.
            if(ITW(IC).eq.1)then
              sfile=' '
              snpfile=' '
              call browsefilelist('?','zon','tmc',sfile,snpfile,nfile,
     &          iier)
              if(nfile.gt.0)then
                sfile=' '
                snpfile=' '
                call browsefilelist('b','zon','tmc',sfile,snpfile,
     &            nfile,iier)
                if(snpfile(1:2).ne.'  ')then
                  write(DFILE,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &              snpfile(1:lnblnk(snpfile))

C Debug.
C                  write(6,*) DFILE

                  LTWIN(NCOMP)=DFILE
                  ITW(NCOMP)=1
                endif
              endif
            endif
          elseif(irop.eq.4)then
            continue
          endif

C Operations file. If source has a file offer options to copy or
C point to it, otherwise offer option to browse for an existing file.
          call FINDFIL(LPROJ(IC),XST)
          irop=1
          if(XST)then
            h(1)='You can have the new zone operations based initially'
            h(2)='on the source zones operations (by copying the file)'
            h(3)='or use the same operations (by pointing to the file)'
            h(4)='or browse available files or continue without '
            h(5)='defining operations at this time. '
            CALL EASKATOG(
     &        'Source zone has an operations file.',
     &        'Options:','copy file','point to file',
     &        'browse file','cancel',' ',' ',' ',irop,5)
          else
            h(1)='You can have the new zone operations by'
            h(2)='browing available files or continue without'
            h(3)='defining operations at this time.'
            CALL EASKATOG(
     &        'Source zone has no operations file.',
     &        'Options:','-','-','browse','cancel',' ',' ',' ',irop,3)
            if(irop.eq.1.or.irop.eq.2) irop=4
          endif
          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(DFILE,'(A,A4)')zname(NCOMP)(1:lnzname(NCOMP)),'.opr'
          else
            WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &        zname(NCOMP)(1:lnzname(NCOMP)),'.opr'
          endif
          if(irop.eq.1)then

C Read in source operations and write out to file for new zone.
C Also update lodlabel commons before writing out.
            CALL ERPFREE(IUF,ISTAT)
            CALL EROPER(0,iuout,IUF,IC,IER)
   91       H(1)='A zone operation file contains schedules of air'
            H(2)='flow and casual gains.'
            CALL EASKS(DFILE,' New zone operations file name:',
     &         ' ',72,'new.opr','operations file',IER,2)
            IF(DFILE.NE.' ')THEN
              LPROJ(NCOMP)=DFILE
              ip3ver(ncomp)=ip3ver(ic)     ! set copied to same version
              oprdesc(ncomp)=oprdesc(ic)   ! use same description
              ventdesc(ncomp)=ventdesc(ic) ! use same vent description
              ctlstr(ncomp,1)=ctlstr(ic,1) ! use same control descriptions
              ctlstr(ncomp,2)=ctlstr(ic,2)
              ctlstr(ncomp,3)=ctlstr(ic,3)
              ITCTL(ncomp)=ITCTL(ic)        ! copy infil and vent controls
              TLO(ncomp)=TLO(ic); TUP(ncomp)=TUP(ic); THI(ncomp)=THI(ic)
              ACIL(ncomp)=ACIL(ic); ACVL(ncomp)=ACVL(ic)
              IVL(ncomp)=IVL(ic); TAL(ncomp)=TAL(ic)
              ACIU(ncomp)=ACIU(ic); ACVU(ncomp)=ACVU(ic)
              IVU(ncomp)=IVU(ic); TAU(ncomp)=TAU(ic)
              ACIH(ncomp)=ACIH(ic); ACVH(ncomp)=ACVH(ic)
              IVH(ncomp)=IVH(ic); TAH(ncomp)=TAH(ic)
              lodlabel(ncomp,1)=lodlabel(ic,1)  ! revise to be in a loop
              lodlabel(ncomp,2)=lodlabel(ic,2)
              lodlabel(ncomp,3)=lodlabel(ic,3)
              CALL ERPFREE(IUF,ISTAT)
              CALL EMKOPER(IUF,LPROJ(NCOMP),NCOMP,IER)
            else
              goto 91
            endif
          elseif(irop.eq.2)then
            LPROJ(NCOMP)=LPROJ(IC)
          elseif(irop.eq.3)then

C Find out if there are operation files in the model ../zones folder.
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','opr',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','opr',sfile,snpfile,
     &          nfile,iier)
              if(snpfile(1:2).ne.'  ')then
                write(DFILE,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))

C Debug.
C                write(6,*) DFILE

                LPROJ(NCOMP)=DFILE
              endif
            endif
          elseif(irop.eq.4)then
            continue
          endif

C See if the source zone has a viewfactor file.
          if(IVF(ic).eq.1)then
            h(1)='The new zone view factors file can be based on the'
            h(2)='source zone (by copying the file) or you can'
            h(3)='point to the source zone file (use care because'
            h(4)='the two zones may change shape) or browse available '
            h(5)='files or continue. '
            irop=1
            CALL EASKATOG(
     &        'Source zone has a view factor file.',
     &        'Options:','copy file','point to file',
     &        'browse file','cancel',' ',' ',' ',irop,5)
            if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
              WRITE(DFILE,'(A,A4)')zname(NCOMP)(1:lnzname(NCOMP)),
     &          '.vwf'
            else
              WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &          zname(NCOMP)(1:lnzname(NCOMP)),'.vwf'
            endif
            if(irop.eq.1)then
              CALL ERPFREE(IUF,ISTAT)
              CALL ERMRT(0,itru,IUF,LVIEW(IC),IC,IER)
   92         H(1)='A zone view factor file contains calculations of'
              H(2)='how much each surface can see other surfaces.'
              CALL EASKS(DFILE,' ','New view factor file name?',
     &             72,'new.vwf','viewfactor file',IER,2)
              IF(DFILE.NE.' ')THEN
                LVIEW(NCOMP)=DFILE
                IVF(NCOMP)=1
                CALL ERPFREE(IUF,ISTAT)
                NZS=NZSUR(NCOMP)
                CALL EMKMRT(LVIEW(NCOMP),LGEOM(NCOMP),NZS,IUF,NCOMP,
     &            'v',IER)
              else
                goto 92
              endif
            elseif(irop.eq.2)then
              LVIEW(NCOMP)=LVIEW(IC)
              IVF(NCOMP)=1
            elseif(irop.eq.3)then

C Find out if there are viewfactor files in the model ../zones folder.
              sfile=' '
              snpfile=' '
              call browsefilelist('?','zon','vwf',sfile,snpfile,nfile,
     &          iier)
              if(nfile.gt.0)then
                sfile=' '
                snpfile=' '
                call browsefilelist('b','zon','vwf',sfile,snpfile,
     &            nfile,iier)
                if(snpfile(1:2).ne.'  ')then
                  write(DFILE,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &              snpfile(1:lnblnk(snpfile))

C Debug.
C                  write(6,*) DFILE

                  LVIEW(NCOMP)=DFILE
                  IVF(NCOMP)=1
                endif
              endif
            elseif(irop.eq.4)then
              continue
            endif
          endif

C See if the source zone has a casual gain control file.
          if(ICGC(ic).eq.1)then
            h(1)='The new zone casual gain control can be based on'
            h(2)='the source zone (by copying the file) or you'
            h(3)='can point to the source zone file or browse'
            h(4)='available files.'
            irop=1
            CALL EASKATOG(
     &        'Source zone has a CG ctl file.',
     &        'Options:','copy file','point to file',
     &        'browse file','cancel',' ',' ',' ',irop,4)
            if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
              WRITE(DFILE,'(A,A4)')zname(NCOMP)(1:lnzname(NCOMP)),
     &          '.cgc'
            else
              WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &          zname(NCOMP)(1:lnzname(NCOMP)),'.cgc'
            endif
            if(irop.eq.1)then
              CALL ERPFREE(IUF,ISTAT)
              call ercgcf(0,iuout,LCGCIN(IC),IC,ier)
   93         H(1)='A zone casual gain control file contains logic'
              H(2)='for light switching in a zone.'
              CALL EASKS(DFILE,' ','Zone CG file name?',
     &           72,'new.vwf','casual gain control file',IER,2)
              IF(DFILE.NE.' ')THEN
                LCGCIN(NCOMP)=DFILE
                ICGC(NCOMP)=1
                CALL ERPFREE(IUF,ISTAT)
                call CASCTMK(LCGCIN(NCOMP),NCOMP,'-',IER)
              else
                goto 93
              endif
            elseif(irop.eq.2)then
              LCGCIN(NCOMP)=LCGCIN(IC)
              ICGC(NCOMP)=1
            elseif(irop.eq.3)then

C Find out if there are casual gain control files in the model ../zones folder.
              sfile=' '
              snpfile=' '
              call browsefilelist('?','zon','cgc',sfile,snpfile,nfile,
     &          iier)
              if(nfile.gt.0)then
                sfile=' '
                snpfile=' '
                call browsefilelist('b','zon','cgc',sfile,snpfile,
     &            nfile,iier)
                if(snpfile(1:2).ne.'  ')then
                  write(DFILE,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &              snpfile(1:lnblnk(snpfile))

C Debug.
C                  write(6,*) DFILE

                  LCGCIN(NCOMP)=DFILE
                  ICGC(NCOMP)=1
                endif
              endif
            elseif(irop.eq.4)then
              continue
            endif
          endif

C See if the source zone has a convection regime file.
          if(IHC(ic).eq.1)then
            h(1)='The new zone convection regime can be based on'
            h(2)='the source zone (by copying the file) or you'
            h(3)='can point to the source zone file or browse  '
            h(4)='available files or continue.'
            irop=1
            CALL EASKATOG('Source zone has a CC file.',
     &        'Options:','copy file','point file',
     &        'browse file','cancel',' ',' ',' ',irop,4)
            if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
              WRITE(DFILE,'(A,A4)')zname(NCOMP)(1:lnzname(NCOMP)),
     &          '.htc'
            else
              WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &          zname(NCOMP)(1:lnzname(NCOMP)),'.htc'
            endif
            if(irop.eq.1)then
              CALL ERPFREE(IUF,ISTAT)
              call ehtcff(LHCCO(IC),IUF,ITRU,IER)
   94         H(1)='A zone convection coefficients file contains data'
              H(2)='used to impose surface convection regimes within'
              H(3)='a zone.'
              CALL EASKS(DFILE,' ','Zone CC file name?',
     &            72,'new.vwf','convection regime file',IER,3)
              IF(DFILE.NE.' ')THEN
                LHCCO(NCOMP)=DFILE
                IHC(NCOMP)=1
                CALL ERPFREE(IUF,ISTAT)
                CALL EMKHTC(LHCCO(NCOMP),NCOMP,IUF,ITRU,IER)
              else
                goto 94
              endif
            elseif(irop.eq.2)then
              LHCCO(NCOMP)=LHCCO(IC)
              IHC(NCOMP)=1
            elseif(irop.eq.3)then

C Find out if there are casual gain control files in the model ../zones folder.
              sfile=' '
              snpfile=' '
              call browsefilelist('?','zon','htc',sfile,snpfile,nfile,
     &          iier)
              if(nfile.gt.0)then
                sfile=' '
                snpfile=' '
                call browsefilelist('b','zon','htc',sfile,snpfile,
     &            nfile,iier)
                if(snpfile(1:2).ne.'  ')then
                  write(DFILE,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &              snpfile(1:lnblnk(snpfile))

C Debug.
C                  write(6,*) DFILE

                  LHCCO(NCOMP)=DFILE
                  IHC(NCOMP)=1
                endif
              endif
            elseif(irop.eq.4)then
              continue
            endif
          endif

C If there is a flow network, update ?? for the new zone before
C writing out configuration file.
          if(IAIRN.ge.1)then
            ICAAS(ncomp)=0
            call usrmsg(
     &        'The current air flow network needs to be updated to',
     &        'include a node and connections for this new zone.','W')
          endif

C Update the configuration file.
          CALL EMKCFG('-',IER)
          MODIFY=.TRUE.
          MODBND=.TRUE.
          IC = -1
          GOTO 3
        ELSEIF(IC.LE.NCOMP)THEN

C Scan the geometry file and freshen derived geometric common blocks.
          call georead(IFIL+1,LGEOM(IC),IC,1,iuout,IER)
          if(IER.NE.0)then
            dok=.true.
            h(1)='A problem was found while reading the geometry and'
            h(2)='you have the option to try again. Do this if you '
            h(3)='think you migh have the wrong file. '
            CALL ASKOK('Problem reading geometry file!',
     &            'Try again?',OK,dok,3)
            IF(OK)GOTO 248
          else
            call zgupdate(0,ic,ier)
          endif

C Trace geometry related information. Because all zones will have
c beeen read in so can include the extended surface `context`.
          if(ITRC.ne.0)then
            call ZINFOREP(itru,ic)
            context=.true.
            CALL SURINFO(IC,ITRU,context)
            IF(ITRC.GT.1)CALL VERINFO(IC,ITRU)
            CALL INSINFO(IC,ITRU)
          endif

C Obstructions are alread in common - no need to scan.
        ENDIF

C Present zone detail editing menu.
        CALL EDZONE(ITRC,IC,IER)
        MODIFY=.TRUE.

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 248

      elseif(INO.EQ.3)THEN

C Zone construction files. Prior to working with the zones rescan
C the construction database (occassionally the initial item in this
C database gets its name corrupted requiring a re-scan).
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
        else

C Could not find Constructions db at this time, report to user.
          write(outs248,'(3a)') 'Constructions db ',
     &       LFMUL(1:lnblnk(LFMUL)),' not found!'
          call edisp248(iuout,outs248,100)
          call edisp(iuout,' ')
        endif

        IC=-1
 249    CALL EASKGEOF('Select a zone to focus on:',CFGOK,IC,'t',IER)
        IF(IC.EQ.0.OR.IC.EQ.-1)THEN
          GOTO 3
        ELSEIF(IC.EQ.-2)THEN
          call tstamp('>','PRJ: enter thermophysical')
          CALL THMENU(ITRC)
        ELSEIF(IC.EQ.99)THEN

C Re-write all zones without prompting the user for input - unless an 
C error occurs.
          QUIET=.FALSE.
          dok=.false.
          h(1)='If you suspect that your zone constructions file has'
          h(2)='been corrupted or is out of date then this command'
          h(3)='will refresh it. This requires that the model has'
          h(4)='existing zone construction files because the process'
          h(5)='of refreshing requires the file names.'
          CALL ASKOK(' ','Update zone constructions files?',
     &           QUIET,dok,5)
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

      elseif(INO.EQ.4)THEN

C Operation file creation and editing.
 250    IC=-1
        izdef=0
        call askzone(ic,izdef,' ','Zone Operations Selection',
     &            'o','Zone index for operations.',ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)then
          MODIFY=.TRUE.
          GOTO 3
        endif
        if(IC.eq.99)then

C Global tasks.
          if(browse)then
            call edisp(itru,'You are currently in browse mode.')
            call edisp(itru,'Changes to zone operations file will')
            call edisp(itru,'not be saved!')
            call edisp(itru,' ')
          endif

C << consider an additional global task of imposing a new infiltration
C << rate on more than one zone either via a search and replace or
C << by asking some questions.
C << one idea would be to take an infiltration pattern from one zone
C << and apply it to one or more other zones.
C << one idea would be to take a casual gain pattern from one zone
C << and apply it to another zone.
          h(1)='You can upgrade older unsorted zone operations files'
          h(2)='if they exist in the current model.'
          h(3)=' '
          h(4)='You can also check current operations files to see'
          h(5)='if the casual gains periods are sorted.'
          CALL EASKABC(
     &      'Zone operations options for multiple zones:',' ',
     &      'update all old files','check period order',
     &      'cancel',IW,5)
          if(IW.eq.1.or.IW.eq.2)then
            inpic=NCOMP
            if(inpic.gt.0)then
              call usrmsg(
     &          'Updating model to reflect changes',
     &          'in operation periods...','-')
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
                    call PROCESSOLDCAS(ICOMP,0,iuout,IER)
                    if(browse)then
                      continue
                    else
                      ip3ver(icomp)=2
                      CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,IER)
                    endif
                  else

C File is current format and might be unsorted. If user is not
C browsing update the operation file.
                    sorted=.true.
                    problem=.false.
                    DO 400 IDTY=1,NBDAYTYPE
                      call checksort(icomp,IDTY,problem,ier)
                      if(problem)then
                        sorted=.false.
                        call edisp(iuout,'casual gains may be unsorted')
                      endif
 400                CONTINUE
                    if(.NOT.sorted)then
                      call PROCESSOLDCAS(ICOMP,0,iuout,IER)
                      if(browse)then
                        continue
                      else
                        ip3ver(icomp)=2
                        CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,IER)
                      endif
                    endif
                  endif
                endif
 155          continue
              call usrmsg(
     &          'Updating model to reflect changes',
     &          'in operation periods...done.','-')
              goto 250
            endif
          elseif(IW.eq.3)then
            goto 3
          endif
        endif
        call tstamp('>','PRJ: enter zone operations')

C Set iver to zero to indicate it is being called from someplace
C other than the versioning facility.
        iver=0
        CALL PRJFMK(ITRC,ITRU,IUO,IC,IER,iver)

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 250

      elseif(INO.EQ.7)THEN

C Check and edit topology.
        call tstamp('>','PRJ: enter topology menu')
        CALL EDCONN(IER)

      elseif(INO.EQ.8)THEN

C Set and tag anchor points.
        call tstamp('>','PRJ: enter anchors menu')
        call anchlist(iasel,ier)

      elseif(INO.EQ.11)THEN

C Shading/ insolation analysis.
 253    IC=-1
        izdef=0
        call askzone(ic,izdef,' ','Zone Shading Selection',
     &         's','Zone index for shading.',ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        if(IC.eq.99)then

C << note: the add insolation option actually should cause
C << the zone geometry file to be updated if the version 1.1
C << geometry files are used. This is not yet done.

C Global tasks.
          h(1)='You can establish detailed shading and/orinsolation'
          h(2)='patterns in nominated zones as follows.'
          h(3)=' '
          h(4)='  Recalculate shading in all zones where a shading'
          h(5)='  file exists.'
          h(6)=' '
          h(7)='  Nominate zones for which you want to include an'
          h(8)='  insolation analysis (i.e. track the distribution of'
          h(9)='  the directly transmitted solar component).'
          h(10)=' '
          h(11)='  Nominate zones for which you want to create solar'
          h(12)='  obstructions so that shading and insolation can'
          h(13)='  be calculated.'
          h(14)=' '
          h(15)='  Nominate zones for which you want to dereference'
          h(16)='  the shading/insolation file.'
          H(17)=' '
          H(18)='If you review the result of a shading analysis'
          H(19)='and find unexpected patterns, this may be because'
          H(20)='of incorrectly ordered surface edges.'
          H(21)=' '
          H(22)='  a) Check that a surface that appears to face a'
          H(23)='     particular directyion in the wireframe view'
          H(24)='     actually does!'
          H(25)=' '
          H(26)='  b) Check the ordering of the edges of all'
          H(27)='     problematic surfaces by adhering to the'
          H(28)='     following advice.'
          H(29)=' '
          H(30)='It is suggested that the first surface edge be'
          H(31)='close to horizontal if possible.'

          CALL EASKABCD(' ','Options for multiple zones:',
     &      'recalculate shading','dereference shading files',
     &      'add insolation','cancel',IW,31)
          if(IW.eq.1)then
            call eddshd
          elseif(IW.eq.2)then
            inpic=0
            call askmultizone(inpic,ivals,
     &        'Select zones do dereference insolation?',
     &        'Zone insolation dereference','s',ier)
            if(inpic.gt.0)then
              DO 153 IC=1,inpic
                ICOMP=ivals(IC)
                ISI(ICOMP)=0
 153          continue
            endif
          elseif(IW.eq.3)then
            inpic=0
            call askmultizone(inpic,ivals,
     &        'Select zones to add insolation?',
     &        'Zone add insolation variant','s',ier)
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

C Create a small place-holder file that subsequent calculation
C procedure can overwrite.
                IUF=IFIL+2
                CALL EFOPSEQ(IUF,LSHAD(ICOMP),3,IER)
                WRITE(IUF,'(2a)') 'Placeholder shading file for ',
     &            zname(icomp)(1:lnzname(icomp))
                CALL ERPFREE(IUF,ios)

C << note: the add insolation option actually should cause
C << the zone geometry file to be updated if the version 1.1
C << geometry files are used. Add code here to open gem file
C << figure out insolation sources and then write out the
C << geometry file (if version 1.1).

 151          continue
            endif
            CALL EMKCFG('-',IER)
            call eddshd
          elseif(IW.eq.4)then
            continue
          endif
          IC=-1
          GOTO 253
        endif

C Deal with one zone.
        write(ZN,'(A)') zname(IC)

        IUF=IFIL+1

C If browsing, do not save files but allow ish to be started.
        if(browse)then
          call edisp(itru,'You are currently in browse mode.')
          call edisp(itru,'The shading analysis module will start')
          call edisp(itru,'but DO NOT attempt to save results!')
          call edisp(itru,' ')
          goto 255
        endif
        IF(ISI(IC).eq.0)THEN
          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(SFIL,'(A,A4)')zname(IC)(1:lnzname(IC)),'.shd'
          else
            WRITE(SFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &        zname(IC)(1:lnzname(IC)),'.shd'
          endif
          DSFIL = 'new.shd'
          H(1)='The zone shading/insulation database holds time'
          H(2)='varying external surface shading and internal surface'
          H(3)='insolation.'
          CALL EASKS(SFIL,' ','Zone shading/insulation file?',
     &         72,DSFIL,'shd/ins db',IER,3)
          if(SFIL(1:2).ne.'  ')then
            LSHAD(IC)=SFIL
            ISI(IC)=1
            CALL EMKCFG('-',IER)
          endif
        else

C Confirm existing shading file name and offer option to dereference.
          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(DSFIL,'(A,A4)')zname(IC)(1:lnzname(IC)),'.shd'
          else
            WRITE(DSFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &        zname(IC)(1:lnzname(IC)),'.shd'
          endif
          SFIL = LSHAD(IC)
          clkok=.false.
          H(1)='The zone shading/insulation database holds time'
          H(2)='varying external surface shading and internal surface'
          H(3)='insolation. You may dereference an existing file.'
          CALL EASKSCMD(SFIL,' ','Zone shading/insulation file?',
     &        'dereference',clkok,72,DSFIL,'shd/ins db',IER,3)
          if(clkok)then

C Reset common blocks, save configuration and see if another zone is requested.
            LSHAD(IC)='UNKNOWN'
            ISI(IC)=0
            call usrmsg(' updating model to ignore shading file...',
     &        ' ','-')
            CALL EMKCFG('-',IER)
            call usrmsg(
     &        ' updating model to ignore shading file...done.',
     &        ' ','-')
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

 255    dok=.true.
        h(1)='If you reached this point you probably have all you'
        h(2)='need to carry out the shading/insolation analysis.'
        h(3)='If in doubt, double check. '
        CALL ASKOK(' ',
     &    ' Proceed with shading/ insolation analysis?',OK,dok,3)
        IF(OK)then

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive ish.
          call tstamp('>','PRJ: start shading')
          call comissionish(ic,'in ',ier)
        endif

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 253

      elseif(INO.EQ.12)THEN

C Zone convective regime description. If an existing file the
C call to hcfmk has the option to dereference it.

C If browsing, do not save files but allow ish to be started.
        if(browse)then
          call edisp(itru,'You are currently in browse mode. You')
          call edisp(itru,'can examine the heat transfer regimes')
          call edisp(itru,'but DO NOT attempt to change them!')
          call edisp(itru,' ')
        endif
 260    IC=-1
        izdef=0
        call askzone(ic,izdef,' ','Zone Convection Selection',
     &           'h','Zone index for hc.',ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        IUO=IFIL+1
        call tstamp('>','PRJ: enter hc file')
        CALL HCFMK(ITRU,IUO,IC,IER)
        MODIFY=.TRUE.

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 260

      elseif(INO.EQ.13)THEN

C View factor editing, begin by identifying the zone and then
C reading in the geometry file followed by view factor editing 
C facilities. Call to edmrt might result in dereferenceing of
C the file.         
        if(browse)then
          call edisp(itru,'You are currently in browse mode. You')
          call edisp(itru,'can look at the surface viewfactor data')
          call edisp(itru,'but DO NOT attempt to recalculate them!')
          call edisp(itru,' ')
        endif
 251    IC=-1
        izdef=0
        call askzone(ic,izdef,' ','Zone View Factors Selection',
     &         'v','Zone index for viewfact.',ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        if(IC.eq.99)then

C Global tasks.
          h(1)='Inter-surface view factors may be determined as simple'
          h(2)='area weightings or exactly using ray tracing. Area '
          h(3)='weighting is the default treament.'
          h(4)=' '
          h(5)='You can nominate the zones to which you want to apply'
          h(6)='either calculation type or, alternatively, you may'
          h(7)='dereference an exiting zone view factor file.analysis'
          CALL EASKABCD(' ','View factor options:',
     &        'area weighted','ray traced',
     &        'dereference file','cancel',IW,7)
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
     &        'Select zones for viewfactors to dereference?',
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

      elseif(INO.EQ.14)THEN

C Casual gain control.
        if(browse)then
          call edisp(itru,'You are currently in browse mode. You can')
          call edisp(itru,'look at the zone casual gain controls')
          call edisp(itru,'but DO NOT attempt to modify them!')
          call edisp(itru,' ')
        endif
 252    IC=-1
        izdef=0
        call askzone(ic,izdef,' ','Zone CG Ctl Selection',
     &        'g','Zone index for cgc.',ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        IUF=IFIL+1
        if(ICGC(IC).eq.1)then

C If browsing do not bother to confirm file name, present menu.
          if(browse)then
            IUF=IFIL+1
            call EDCASCTL(ITRC,IC,IER)
            goto 252
          endif
          H(1)='This menu allows the dereferencing or editing of a'
          H(2)='zone casual gain control file.'
          CALL EASKABC(' ','Casual gain control options:','edit',
     &      'dereference','cancel',irpt,2)
          if(irpt.eq.1)then
            IUF=IFIL+1
            call EDCASCTL(ITRC,IC,IER)
          elseif(irpt.eq.2)then
            ICGC(IC)=0
            LCGCIN(IC)='UNKNOWN'
C            call usrmsg(
C     &        ' Dereferencing casual gain control file...',' ','-')
            CALL EMKCFG('-',IER)
C            call usrmsg(
C     &        ' Dereferencing casual gain control file...done.',' ','-')
          else
            goto 3
          endif
        else
          if(browse)then
            call usrmsg('No casual gain defined for this zone.',
     &        'In browse mode so nothing to do.','P')
            goto 252
          endif
          H(1)='This menu allows a zone casual control file to be'
          H(2)='created/editted. The data within this file controls'
          H(3)='zone casual gains based on the internal or external'
          H(4)='illuminance. Different control algorithms are offered.'
          CALL EASKABC(' ','Casual gain ctl options:',
     &      'create','dereference','cancel',irpt,4)
          if(irpt.eq.1)then
            IUF=IFIL+1
            call EDCASCTL(ITRC,IC,IER)
          elseif(irpt.eq.2)then
            ICGC(IC)=0
            LCGCIN(IC)='UNKNOWN'
C            call usrmsg(
C     &        ' Dereferencing casual gain control file...',' ','-')
            CALL EMKCFG('-',IER)
C            call usrmsg(
C     &        ' Dereferencing casual gain control file...done.',' ','-')
          endif
        endif

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 252

      elseif(INO.EQ.15)THEN

C CFD.
        H(1)='This facility allows you to compose a file defining'
        H(2)='the zone parameters required for a CFD simulation.'
        H(3)='You may reference and dereference the CFD domain'
        H(4)='within the current model at any time.'
        CALL PHELPD('CFD domain + parameters',4,'-',0,0,IER)

        IC=-1
        izdef=0
        call askzone(ic,izdef,' ','Zone CFD Selection',
     &         'd','Zone index for cfd.',ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        call georead(IFIL+1,LGEOM(IC),IC,1,iuout,IER)

        IUF=IFIL+1
        call cfdcomp(ic,iuf,ier)
        MODIFY=.TRUE.

C If browsing do not offer inclusion option.
        if(browse)then
          continue
        else
          write(outs,'(3a)') 'Include ',LCFD(ic)(1:lnblnk(LCFD(ic))),
     &      ' in the model? '
          dok=.true.
          h(1)='If the CFD domain is complete you will probably want'
          h(2)='to associate it with the model. If you do not do this'
          h(3)='now, you can do it later (but you will need to supply'
          h(4)='the name of the file to complete the association). '
          h(5)=' '
          h(6)='If you want to ignore the CFD domain just say no. '
          CALL ASKOK(outs,'(see help)',OK,dok,6)
          IF(OK)then
            call usrmsg(' Including CFD domain...',' ','-')
            CALL EMKCFG('-',IER)
            call usrmsg(' Including CFD domain...done.',' ','-')
          else
            IFCFD(IC)=0 
            LCFD(IC)='UNKNOWN'
            call usrmsg(' Dereferencing CFD domain...',' ','-')
            CALL EMKCFG('-',IER)
            call usrmsg(' Dereferencing CFD domain...done.',' ','-')
          endif
        endif

      elseif(INO.EQ.16)THEN

C Adaptive gridding module.
        IC=-1
        CALL EASKGEOF('Select zone for adaptive gridding:',CFGOK,IC,
     &    '-',IER)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        write(ZN,'(A)') zname(IC)

        IUF=IFIL+1

C If browsing do not save files but allow grd to be started.
        if(browse)then
          call edisp(itru,'You are currently in browse mode.')
          call edisp(itru,'The adaptive gridding module will start')
          call edisp(itru,'but DO NOT attempt to save results!')
          call edisp(itru,' ')
          goto 257
        endif

C Offer creation of moisture transport data file or dereference.
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(SFIL,'(A,A4)')zname(IC)(1:lnzname(IC)),'.mst'
        else
          WRITE(SFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnzname(IC)),'.mst'
        endif
        DSFIL = SFIL
        H(1)='The moisture file holds moisture transport data'
        H(2)='for nodes.'
        write(outs,'(3a)')'Moisture transport file for ',
     &    zname(IC)(1:lnzname(IC)),'?'
        clkok=.false.
        if(.NOT.MSTRZN(IC))then

C << What does ignore mean here? >>
          CALL EASKSCMD(SFIL,outs,' ','ignore',clkok,72,DSFIL,
     &      'moisture file',IER,2)
          if(clkok) goto 3
        else
          CALL EASKSCMD(SFIL,outs,' ','dereference',clkok,72,DSFIL,
     &      'moisture file',IER,2)
          if(clkok)then
            MSTRZN(IC)=.false.
            LMOIST(IC)=' '
            call usrmsg('Dereferencing moisture transport...',
     &        ' ','-')
            CALL EMKCFG('-',IER)
            call usrmsg('Dereferencing moisture transport...done.',
     &        ' ','-')
            IC=-1
            GOTO 3
          endif
        endif
        if(SFIL(1:2).ne.'  ')then
          MSTRZN(IC)=.true.
          LMOIST(IC)=SFIL
          call usrmsg('updating model for moisture transport...',
     &      ' ','-')
          CALL EMKCFG('-',IER)
          call usrmsg('updating model for moisture transport...done.',
     &      ' ','-')
        endif

        H(1)='There are four adaptive gridding files which must be'
        H(2)='specified for each zone to which such gridding is to'
        H(3)='be applied. Check documentation for more information.'
        if(.NOT.ZONE3D(IC))then
          CALL EASKAB(' Addaptive gridding options:',
     &      ' ','define file names','continue',IW,3)
          if(iw.eq.2)then
            IC=-1
            GOTO 3
          endif
        else
          CALL EASKABC(' Addaptive gridding options:',
     &      ' ','browse/edit','dereference ','continue',IW,3)
          if(iw.eq.2)then
            L3DCVS(IC)='UNKNOWN'
            L3DCNC(IC)='UNKNOWN'
            L3DNDC(IC)='UNKNOWN'
            L3DTAQ(IC)='UNKNOWN'
            ZONE3D(IC)=.false.
            call usrmsg('Dereferencing addaptive gridding...',
     &        ' ','-')
            CALL EMKCFG('-',IER)
            call usrmsg('Dereferencing addaptive gridding...done.',
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
        H(1)='The 3D control volumes file defines...'
        H(2)='<< more text to be added here >>'
        CALL EASKS(SFIL,' 3D control volumes file',
     &    ' ',72,DSFIL,'control volumes file',IER,3)
        if(SFIL(1:2).ne.'  ') L3DCVS(IC)=SFIL

        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DSFIL,'(A,A4)')zname(IC)(1:lnzname(IC)),'.cnc'
        else
          WRITE(DSFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnzname(IC)),'.cnc'
        endif
        SFIL = L3DCNC(IC)
        H(1)='The 3D control volumes connections file defines...'
        H(2)='<< more text to be added here >>'
        CALL EASKS(SFIL,' 3D control volumes connections file',
     &    ' ',72,DSFIL,'control volumes connections file',IER,3)
        if(SFIL(1:2).ne.'  ') L3DCNC(IC)=SFIL

        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DSFIL,'(A,A4)')zname(IC)(1:lnzname(IC)),'.ndc'
        else
          WRITE(DSFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnzname(IC)),'.ndc'
        endif
        SFIL = L3DNDC(IC)
        H(1)='The 3D nodes coordinates file defines...'
        H(2)='<< more text to be added here >>'
        CALL EASKS(SFIL,' 3D nodes coordinates file',
     &    ' ',72,DSFIL,'3D nodes coordinates file',IER,3)
        if(SFIL(1:2).ne.'  ') L3DNDC(IC)=SFIL

        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DSFIL,'(A,A4)')zname(IC)(1:lnzname(IC)),'.3dt'
        else
          WRITE(DSFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnzname(IC)),'.3dt'
        endif
        SFIL = L3DTAQ(IC)
        H(1)='The 3D nodes temperatures file defines...'
        H(2)='<< more text to be added here >>'
        CALL EASKS(SFIL,' 3D nodes temperatures file',
     &    ' ',72,DSFIL,'3D nodes temperatures file',IER,3)
        if(SFIL(1:2).ne.'  ') L3DTAQ(IC)=SFIL
        ZONE3D(IC)=.true.
        call usrmsg('Including addaptive gridding...',' ','-')
        CALL EMKCFG('-',IER)
        call usrmsg('Including addaptive gridding...done.',' ','-')

  257   dok=.false.
        h(1)='To proceed with adaptive gridding you first have to...'
        h(2)='<< instructions to be added >> '
        CALL ASKOK(' ',' Proceed with adaptive gridding?',OK,dok,2)
        IF(OK)then
          doit=' '
          CALL TCHILD(ICPMOD)
          CALL TERMODE(ICPMOD,TMODE)
          CALL ADDPATH(LCFGF,longtfile,CONCAT)
          call tstamp('>','PRJ: edit adaptive gridding')
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,5a)') 'grd -mode ',tmode,
     &        ' -s ',iappw,iappx+10,iappy+40,' -file ',
     &        longtfile(1:lnblnk(longtfile)),' -zone ',
     &        ZN(1:lnblnk(ZN)),' &'
          else
            write(doit,'(7a)') 'grd -mode ',tmode,
     &        ' -s 0 0 0 -file ',longtfile(:lnblnk(longtfile)),
     &        ' -zone ',ZN(1:lnblnk(ZN)),' &'
          endif
          call usrmsg('starting gridding display via',doit,'-')
          call runit(doit,tmode)
        endif

      elseif(INO.EQ.19)THEN

C Call embedded renewables.
        call EDSPLIST('EG',IDV,IER)

      elseif(INO.EQ.20)THEN

C Call active materials menu.
        call EDSPLIST('EM',IDV,IER)

      elseif(INO.EQ.21)THEN

C Call advanced (bidirectional) optics.
        call edbioptics()
      ELSE

C Fall through case.
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
C  confirm=true request confirmation of configuration file name
C  ckpath=true finds the path to it (should be false for CAD use as
C         the file and its location is already known). 
C  itisanexemplar=true then ensure that browse is set to true in
C         any case because the user has previously selected an exemplar model.
C Parameter returned:
C  ier = 0 if no problems found.
      SUBROUTINE NEWPRB(ITRC,confirm,ckpath,itisanexemplar,IER)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "prj3dv.h"
#include "espriou.h"  
C espriou.h for parameter MFFOLD
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/GTFIL/GTGEOM
      COMMON/GT/GTNAME
      common/cctlnm/ctldoc,lctlf
      common/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/RAY3/MODIFY,MODLEN,MODBND
     
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path
      common/rcmd/LCMDFL
      common/user/browse
      common/deflt4/dinstpath

C Defaults.
      common/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical XST,OK,DOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical MODIFY,MODLEN,MODBND,browse
      logical confirm,ckpath,itisanexemplar
      logical unixok,prob

      dimension nwlistf(50),ivlist(50)
      character*72 listf(50),listfc(50)
      character*70 listfc70(50)
      character*32 WORDS(12)

      character path*72,LCMDFL*144,LTMP*144,GTGEOM*72
      character MODE*4,LCFGF*72,H*72,cfgroot*24,fs*1,outs*124
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      character OUTSTR*124,ctldoc*248,LCTLF*72,t96*96,t144*144,ETEXT*82
      character sfile*144,odir*72,subpath*72,action*3,GTNAME*15
      CHARACTER dinstpath*48,dirpath*72

C Passed parameters for pregist.
      character root*24,mpath*72,folder*12,menu*72

      integer igraphiclib  ! external definition
      integer numberofzones ! to pass to updwire rather than ncomp
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      t96='  '
      t144='  '

C Ask for source file name and if the file exists then read it;
C otherwise make general defaults and allow input from scratch.
C Assume configuration file is from IFIL+5 and any leakage
C description is fom IFIL+6 (unit used by events profile db).
C Also scan utility files if they exist.
      IAPROB=IPRODB
  289 if(confirm)then 
        IER=0
        H(1)='The system configuration file holds the definition'
        H(2)='of the building/plant to be simulated, including'
        H(3)='references to the various files comprising a model.'
        H(4)='If the name given matches an existing file then that'
        H(5)='model is loaded by the Project Manager and becomes'
        H(6)='available for browsing and editing (if you are the'
        H(7)='owner. If the file does not exist then a new one is'
        H(8)='created and loaded with a set ofdefaults that can'
        H(9)='subsequently be modified. The default model is'
        H(10)=' '
        write(H(11),'(1x,60a)') DFCFG(1:60)
        H(12)=' '
        H(13)='Note that before this model can be edited, it must'
        H(14)='be copied to your user area to give you write'
        H(15)='permission. Other example models are available'
        H(16)='via the `exemplars` option.'

        LTMP=' '    ! reset return string
        CALL EASKXORGTKF(LCMDFL,
     &    'System configuration file?',' ',
     &    DFCFG,LTMP,'config file name',IER,16)

C Cancel request, return and ensure that the calling
C routine deals with the -3 ier passed back.
        if(ier.eq.-3) return

        IF(LTMP(1:2).EQ.'  ')GOTO 289
        LCMDFL='  '
        call st2file(LTMP,LCMDFL)
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
C        write(t144,*) ' model configuration is ',lcfgf(1:lnblnk(lcfgf))
C        call edisp(iuout,t144)
        if(path(1:2).eq.'./')then

C A ./ indicates that fdroot found no file separators. Must be in cfg folder.
          browse=.false.
        elseif(path(1:1).eq.'.'.and.path(2:2).eq.fs)then

C A .\ indicates that fdroot found no file separators. Must be in cfg folder.
          browse=.false.
        elseif(path(1:ldirpath).eq.dirpath(1:ldirpath))then

C The path is to the installed esp-r training folder so we must be
C browsing the model.
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
C            call edisp(iuout,'User can modify this model.')
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
C            call edisp(iuout,'User can modify this model.')
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
        CALL ERPFREE(IFCFG,ISTAT)
        call FINDFIL(LCFGF,XST)
      elseif(ifold.eq.1.and.icerr.eq.0)then

C The supplied name is a folder name so check and see if there
C is a cfg folder within it and if so if there is one or more
C configuration files.
        XST=.FALSE.
        write(t144,'(3a)')LCMDFL(1:lnblnk(LCMDFL)),fs,'cfg'
        ifold=-1
        call ckaccess(ifold,iaccess,icerr,t144)
        if(ifold.eq.1.and.icerr.eq.0)then
          lex=MIN0(lnblnk(LCMDFL),24)
          write(cfgroot,'(a)',IOSTAT=ios,ERR=13) LCMDFL(1:lex)
          subpath=' '
          call usrdir(odir)
          write(subpath,'(3a)',IOSTAT=ios,ERR=14) 
     &      odir(1:lnblnk(odir)),fs,t144(1:lnblnk(t144))
     
#ifdef OSX
C In OSX slightly different logic. If path ls ./ then standard logic
C works. If path begins with /usr/esru then no need to prepend odir.
          if(path(1:2).eq.'./')then
            continue
          elseif(path(1:9).eq.'/usr/esru')then
            subpath=' '
            write(subpath,'(a)',IOSTAT=ios,ERR=14) path(1:lnblnk(path))
          else
            continue
          endif

C Debug.
C          write(6,*) ' subpath of folder is ',subpath(1:lnblnk(subpath))

#endif
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
          write(outs,*,IOSTAT=ios,ERR=16) 'models in folder ',subpath
          call edisp(iuout,outs)
          call clearfilelist(listf,MFFOLD)

C Re-establish action and file separator prior to call to getfilelist.
          action='cfg'
          if(unixok)then
            fs = char(47)
          else
            fs = char(92)
          endif
          call getfilelist(subpath,action,listf,nwlistf,nlistf)
          call copyfilelist(listf,nwlistf,listfc,MFFOLD,maxw)
          call printfilelist(outs,'p',listfc,MFFOLD,nlistf)

C Try to use information gathered from file scan.
          if(nlistf.gt.0)then
            h(1)='The following files match the search criteria.'
            h(2)='Select one to use or, if none selected, it will'
            h(3)='revert to prior value. '
            ix=1
            do 76 ij=1,nlistf
              listfc70(ij)=' '
              lex=MIN0(nwlistf(ij),70)  ! trap file names longer than 70
              write(listfc70(ij),'(a)',IOSTAT=ios,ERR=13) 
     &          listfc(ij)(1:lex)
  76        continue
            if(maxw.gt.70) maxw=70  ! ensure list fits
            CALL EPICKS(IX,ivlist,outs,' ',
     &        maxw,nlistf,listfc70,'Available files',IER,3)
            if(ix.eq.1)then
              write(t144,'(3a)',IOSTAT=ios,ERR=17)
     &          LCMDFL(1:lnblnk(LCMDFL)),fs,'cfg'
              write(sfile,'(3a)',IOSTAT=ios,ERR=17) 
     &          t144(1:lnblnk(t144)),fs,
     &          listfc(ivlist(ix))(1:nwlistf(ivlist(ix)))

C Debug.
C              write(6,*) 'sfile is ',sfile

              call fdroot(sfile,path,LCFGF)
              CALL ERPFREE(IFCFG,ISTAT)
              call FINDFIL(LCFGF,XST)
            endif
          endif
        endif
      else
        if(lnblnk(LCMDFL).le.72)then
          write(LCFGF,'(a)',IOSTAT=ios,ERR=18) LCMDFL(1:lnblnk(LCMDFL))
        else
          write(LCFGF,'(a)',IOSTAT=ios,ERR=18) LCMDFL(1:72)
        endif
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
            lex=MIN0((lwcfgl-1),24)  ! ensure cfgroot within length
            write(cfgroot,'(a)',IOSTAT=ios,ERR=13)WORDS(IW)(1:lwcfgl-1)
          else
            lex=MIN0(lwcfgr,24)  ! ensure cfgroot within length
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
              if(lex.ge.96)call usrmsg(
     &          'The overall path is more than 96 char.',
     &          'Suggest relocating model.','W')
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
              call fdroot(t96,path,LCFGF)
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
     &              LTMP(1:lcfgr),fs,'cfg',
     &              fs,LTMP(1:lcfgr),'.cfg'
                endif
              elseif(lwcfgr.le.4)then
                write(cfgroot,'(a)',IOSTAT=ios,ERR=13) 
     &            WORDS(IW)(1:lwcfgr)
                write(t96,'(6a)',IOSTAT=ios,ERR=20) 
     &            LTMP(1:lcfgr),fs,'cfg',fs,
     &            LTMP(1:lcfgr),'.cfg'
              endif
              if((t96(1:2).ne.'  ').and.(t96(1:4).ne.'UNKN'))then
                XST=.false.
                INQUIRE (FILE=t96,EXIST=XST)
                if(XST)then
                  call fdroot(t96,path,LCFGF)
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
        if(itrc.gt.0)then
          CALL EASKABC('When reading the model description,',
     &      'do it:','silently','with synopsis','full report',
     &      IW,0)
          itrc=IW-1
        endif
        MODE='ALL '
        IUF=IPRODB   ! assign second file unit to the events db unit
        CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
C copy # of plant component nodes from bps to prj commons 
        call CopyPltNodeCounts()
        IF(IER.NE.0)THEN
          dok=.true.
          h(1)='There was an unspecified problem, review any warnings'
          h(2)='to see if the problem is one you can fix. Also check'
          h(3)='that the file was of the correct type.'
          h(4)=' '
          h(5)='If you say `no` then it may be possible to work with'
          h(6)='the model, but it may be incomplete or inconsistent'
          h(7)='and should be treated as at risk.'
          CALL ASKOK('While reading the model configuration file',
     &               'a problem was detected! Try again?',OK,dok,7)
          if(OK)then
            confirm=.true.
            goto 289
          endif
        ENDIF

C We know how many zone so use ZDATA to finish instanciating the
C common blocks for:
C   VCOORD : X,Y & Z coordinates of each vertice in zone.
C   NZNVER : number of vertices associated with each connection.
C   NZJVN : topology of vertices associated with each connection.
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
          write(outs,'(2a)') 'Found ground definition ',GTNAME
          call edisp(iuout,outs)
        endif

        IF(MMOD.EQ.8)then
          numberofzones=ncomp
          call updwire(numberofzones)
          call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
          call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                           iicfgz,iicfgn,iicfgc,iicfgdfn)
          WRITE(etext,'(2A)')'Model: ',modeltitle(1:lnblnk(modeltitle))
          iside=1
          isize=1
          ifont=1
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
     &         'and optical databases, please check!','W')
            ier = 0
          endif
        endif

C Scan control file (silently) if one exists.
        ICTLF=IFIL+1
        CALL ERPFREE(ICTLF,ISTAT)
        call FINDFIL(LCTLF,XST)
        if(XST)then
          CALL EZCTLR(ICTLF,0,IUOUT,IER)
          if(IER.NE.0)then
            dok=.true.
            h(1)='There was a problem while scanning the control'
            h(2)='file. Review any messages to see if you can correct'
            h(3)='the problem. Also check to see that the file is'
            h(4)='of the correct type. '
            h(5)=' '
            h(6)='If you say `no`, the model description may be'
            h(7)='incomplete so be careful. Consider dereferencing'
            h(8)='the control file and recreating it from scratch.'
            CALL ASKOK('While reading the control file a',
     &        'problem was detected! Try again?',OK,dok,8)
            if(OK)then
              confirm=.true.
              goto 289
            endif
          endif
        endif

C Display any general associated images.
        call imgdisp(0,'****',ier)

C If a plant only model then no need to read zone files.
C Set flag so that bounds are checked.
        DO 77 ICU=1,NCOMP
          iZBFLG(ICU)=0
   77   CONTINUE
        IF(IER.NE.0)THEN
          dok=.true.
          h(1)='There is an unspecified error with this model. Please'
          h(2)='review earlier error messages.'
          CALL ASKOK('There was an error reported as the',
     &      'model was read! Try again?',OK,dok,2)
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
     &         'Model topology (surface connections) is inconsistent.',
     &         ' ','W')
          else
            call usrmsg(
     &      'Model opology (surface connections) is inconsistent.',
     &      'Clear current topology and use topology tool to resolve.',
     &      'W')
          endif
        endif

        CFGOK=.TRUE.
        MODIFY=.TRUE.
        MODLEN=.TRUE.
      ELSE
        h(1)='If the name supplied is not found you can: '
        h(2)='  a) edit the name,'
        h(3)='  b) create a new configuration file using that name,'
        h(4)='  c) delay the decision by selecting `cancel`.'
        h(5)=' '
        h(6)='If you are running the GTK version of Project Manager,'
        h(7)='and the name of the model appears correct (including'
        h(8)='the correct directory path), then select the respecify'
        h(9)='option and use the file browser to locate the model.'
        WRITE(OUTSTR,'(A,2X,A)')' Could not find',LCFGF
        CALL EASKABC(OUTSTR,' Options: (see help)',
     &    'respecify','new configuration file','cancel',IW,9)
        if(IW.eq.1)then
          confirm=.true.
          goto 289
        elseif(IW.eq.2)then
          continue
        elseif(IW.eq.3)then
          ier=-1
          return
        endif

C Clear any previous model in memory and call pregisst in interactive
C mode (with blanks for the parameters).
        call clrprb
        root=' '
        mpath=' '
        folder=' '
        menu=' '
        call pregist('i ',root,mpath,folder,menu,ier)
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
   16 if(IOS.eq.2)then
        CALL USRMSG('No permission to write outs',outs,'W')
      else
        CALL USRMSG('String write error in outs',outs,'W')
      endif
      IER=1
      return
   17 if(IOS.eq.2)then
        CALL LUSRMSG('No permission to write t144',t144,'W')
      else
        CALL LUSRMSG('String write error in t144',t144,'W')
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
C << this duplicates opts within the model export facility >>
C << it would be clearer if all model I/O happened there >>
      subroutine visualz(act,ier)
#include "building.h"
#include "model.h"
      
      integer lnblnk  ! function definition

C      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      common/pophelp/h(60)
      common/C21/IFCFG,cfgroot,LCFGF
      common/appw/iappw,iappx,iappy
      common/user/browse
      logical concat,OK,DOK,browse,XST,deref,unixok
      character dradcf*72,LCFGF*72,OUTSTR*124
      character LFIL*72,dvfil*72,cfgroot*24,ltmp*72
      character doit*124,tmode*8,H*72,act*1
      character fs*1
      character longtfile*124
      character brw*8   ! to signal model is being browsed
#ifdef OSI
      integer ICPMOD     ! passed from tchild
#else
      integer*8 ICPMOD     ! passed from tchild
#endif

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      if(act.eq.'V')then

C Construct a 'VIEWER' format file.  First get file name.
        dvfil='std.vew'
        write(LFIL,'(a,a4)')LCFGF(1:lnblnk(LCFGF)),'.vew'
        doit = ' '
        H(1)='The hidden line perspective program requires an input'
        H(2)='file, which may be automatically created from the'
        H(3)='current model.'
        CALL EASKS(LFIL,' ','File name for image data?',
     &             72,dvfil,'perspective input file',IER,3)
        call addpath(LCFGF,longtfile,concat)
        write(doit,'(4a)') 'ecnv -obs -if esp -in ',
     &    longtfile(1:lnblnk(longtfile)),' -of viewer -out ',
     &    LFIL(1:lnblnk(LFIL))
        call usrmsg('starting conversion via',doit,'-')
        call runit(doit,'-')

        dok=.true.
        h(1)='If the export of vector information was accomplished'
        h(2)='without error you should be able to start up the module'
        h(3)='that displays hidden line views.'
        CALL ASKOK(' ',' Display hidden line views?',OK,dok,3)
        IF(OK)then

C Get logical name of child process terminal type and create a
C string to drive VIEWER.
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,3a)') 'viewer -mode ',tmode,
     &        ' -s ',iappw,iappx+25,iappy+20,' -file ',
     &        LFIL(1:lnblnk(LFIL)),' &'
          else
            write(doit,'(5a)') 'viewer -mode ',tmode,
     &        ' -s 0 0 0 -file ',LFIL(1:lnblnk(LFIL)),' &'
          endif
          call usrmsg('starting hidden line viewer via',doit,'-')
          call runit(doit,tmode)
        endif
        return
      elseif(act.eq.'R')then

C If in browse mode then run e2r and return, otherwise check rcf name
C and contents of file on return from e2r.

C If browsing pass this information to e2r.
        if(browse)then
          brw = ' -b yes'
        else
          brw = ' -b no '
        endif
        if(.NOT.browse)then

C Some small esp-r models may have all files in the same folder. If
C that is the case then it is probably not safe to create a separate
C rad folder. All need multi-folder models will have a rad folder.

C Set rcf name if unknown and set default name. If path for radiance
C files is ./ then update this to ../rad and create a folder.
          if(lradcf(1:7).eq.'UNKNOWN')then
            write(lradcf,'(a,a)') cfgroot(1:lnblnk(cfgroot)),'.rcf'
          else
            if(radpth(1:3).eq.'./ '.or.radpth(1:3).eq.'.\\ ')then
              H(1)='Creating a folder for Radiance model: `../rad`.'
              H(2)='The current model includes a Radiance model in the'
              H(3)='`cfg` folder. If you want to use this old model,'
              H(4)='you should move the files to `../rad` and update'
              H(5)='the `scenes` and `rif` files. The current scenes'
              H(6)='file is:'
              write(H(7),'(a)') lradcf(1:lnblnk(lradcf))
              CALL PHELPD('visu-rad-warning',7,'-',0,0,IER)
            endif
          endif
          if(radpth(1:3).eq.'./ '.or.radpth(1:3).eq.'.\\ ')then
            write(radpth,'(3a)')'..',fs,'rad'
            write(doit,'(4a)') 'mkdir ',
     &        cfgroot(1:lnblnk(cfgroot)),fs,'rad'
            call usrmsg('Creating folder for radiance model:',doit,'P')
            call runit(doit,'-')
          endif
 319      H(1)='A Radiance scene file specifies the composition of the'
          H(2)='model for Radiance as well as the purpose of various'
          H(3)='scenes (general viewing, daylight appraisal etc.).'
          ltmp=lradcf
          dradcf='scene.rcf'
          CALL EASKS(ltmp,' Radiance scene file for this model?',
     &     '  ',72,dradcf,'Radiance scene file name',IER,3)
          if(ltmp.eq.' ')goto 319 

C rcf name OK, therefore save cfg file and start e2r.
          lradcf = ltmp
          call tstamp('>','PRJ: save configuration (with rcf file)')
          CALL EMKCFG('-',IER)
        endif

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive e2r.
        doit = ' '
        call tchild(ICPMOD)
        call termode(ICPMOD,tmode)
        call addpath(LCFGF,longtfile,concat)

C If prj initial size is a % of default, pass this on to child with
C an offset from prj start position.
        if(iappw.gt.0.and.iappw.le.200)then
          write(doit,'(3a,3i4,4a)') 'e2r -mode ',tmode,
     &      ' -s ',iappw,iappx+35,iappy+40,' -file ',
     &      longtfile(1:lnblnk(longtfile)),brw,' &'
        else
          write(doit,'(6a)') 'e2r -mode ',tmode,
     &      ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),brw,' &'
        endif
        call usrmsg('starting Radiance desktop via',doit,'-')
        call runit(doit,tmode)
        H(1)=' CLICK here when you have finished with the'
        H(2)=' colour rendering module.'
        CALL PHELPD('e2r finished menu',2,'-',0,0,IER)

C Check if radiance scene file exists. If not or zero length or not correct 
C file type then dereference it from the cfg file. The alternative location
C for the scene file is in ../rad if user owned the model.  
        if(.NOT.browse)then
          IRCFG=IFIL+1
          call ERPFREE(IRCFG,ISTAT)
          XST=.FALSE.
          deref=.FALSE.
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
            deref=.true.
          endif
          if(deref)then

C Problem with rcf file therefore dereference it.
            call usrmsg(
     &        'Could not find scene file so dereferencing it.',' ','W')
            lradcf='UNKNOWN'
            call tstamp('>','PRJ: save configuration (no rcf file)')
            CALL EMKCFG('-',IER)
          else

C << Place to associate images (when logic ready.) >>
          endif
        endif
      endif
      return
      end

C **************** globaltransform
C globaltransform is passed instructions on a transform to be globally
C applied to zones in an esp-r model. It knows about dependencies and
C will act on or warn users as appropriate (if in interactive mode).
C Parameters:
C   valx,valy,valz (real) are the transforms (m) for each axis.
C   izlistnb (integer) is the number of selected zones
C   izlist is the array holding selected zone indicies to transform
C   silent (logical) if true then do not ask user about transforms
C      of obstruction data.
      subroutine globaltransform(valx,valy,valz,izlistnb,izlist,
     &  silent,itru,ier)
      implicit none
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition
      real valx,valy,valz  ! the transforms
      integer izlistnb,izlist
      dimension izlist(MCOM)  ! array of which zones are selected for transform.
      logical silent  ! if true then skip dialogs
      integer itru,ier  ! type for passed parameters

      integer ifil
      common/FILEP/IFIL
      integer iuout,iuin
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      
      integer ncub,ivfok
      real XOC,YOC,ZOC,DXC,DYC,DZC,CANG
      character CUBN*6
      COMMON/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)
      
      logical newgeo  ! to use for testing if new/old geometry file.
      
      character*72 LTMP
      character ZN*12,h*72
      character outs*124
      logical DOK,OK
      integer iuf   ! file type 
      integer iicomp  ! index of current zone
      integer itrc,ij,i,iz  ! types for local variables
      integer ibe   ! loop for faces of obstruction
       
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
          call georead(IUF,LTMP,IICOMP,1,iuout,IER)
          DO 62 I=1,NTV
            X(I)=X(I)+VALX
            Y(I)=Y(I)+VALY
            Z(I)=Z(I)+VALZ
   62     continue
          iZBFLG(IICOMP)=0

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

C After transform update the g6 & g7 commons and find dependencies.
          call zgupdate(0,IICOMP,ier)

C Depending on the nature of the change update the common block pmchange.
          call warnmod(IICOMP,'str')

C Transform any MRT sensors associated with this zone.
          if (IVF(IICOMP).eq.1) then
            NCUB = 0
            CALL ERMRT(ITRC,ITRU,IUF,LVIEW(IICOMP),IICOMP,IER)
            if (NCUB.gt.0) then
              if(silent)then
                ok= .true.
              else
                write(outs,'(2a)') zn(1:lnblnk(zn)),' as well?'
                dok=.true.
                h(1)='You have rotated the zone and probably also'
                h(2)='want to rotate the sensors to match.'
                call askok('Transform MRT sensors of',outs,OK,dok,2)
              endif
              if (OK) then
                do 73 ij=1,NCUB
                  XOC(ij)=XOC(ij)+VALX
                  YOC(ij)=YOC(ij)+VALY
                  ZOC(ij)=ZOC(ij)+VALZ
  73            continue
                CALL EMKMRT(LVIEW(IICOMP),LGEOM(IICOMP),
     &                      NZSUR(IICOMP),IUF,IICOMP,'v',IER)
              endif
            endif
          endif

C If there is an obstructions file, loop through each and
C transform origin.
          if(IOBS(IICOMP).EQ.1)then
            if(silent)then
              ok= .true.
            else
              dok=.true.
              write(outs,'(4a)') 'Transforms obstructions of ',
     &          zn(1:lnblnk(zn)),' in ',
     &          ZOBS(IICOMP)(1:lnblnk(ZOBS(IICOMP)))
              h(1)='You rotated the zone(s) and so will probably'
              h(2)='also want to rotate associated obstructions.'
              call askok(outs,' ?',OK,dok,2)
            endif
            if(OK)then
              CALL EGOMST(IUF,IICOMP,ZOBS(IICOMP),0,ITRC,ITRU,IER)
              do 63 ij=1,nbobs(iicomp)
                XOB(iicomp,ij)=XOB(iicomp,ij)+VALX
                YOB(iicomp,ij)=YOB(iicomp,ij)+VALY
                ZOB(iicomp,ij)=ZOB(iicomp,ij)+VALZ
                do 88 ibe=1,8
                  XBP(iicomp,ij,ibe)=XBP(iicomp,ij,ibe)+VALX
                  YBP(iicomp,ij,ibe)=YBP(iicomp,ij,ibe)+VALY
                  ZBP(iicomp,ij,ibe)=ZBP(iicomp,ij,ibe)+VALZ
  88            continue
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
                dok=.true.
                write(outs,'(2a)') 'Transforms obstructions of ',
     &            zn(1:lnblnk(zn))
                h(1)='You rotated the zone(s) and so will probably'
                h(2)='also want to rotate associated obstructions.'
                call askok(outs,' ?',OK,dok,2)
              endif
              if(OK)then
                do 64 ij=1,nbobs(iicomp)
                  XOB(iicomp,ij)=XOB(iicomp,ij)+VALX
                  YOB(iicomp,ij)=YOB(iicomp,ij)+VALY
                  ZOB(iicomp,ij)=ZOB(iicomp,ij)+VALZ
                  do 89 ibe=1,8
                    XBP(iicomp,ij,ibe)=XBP(iicomp,ij,ibe)+VALX
                    YBP(iicomp,ij,ibe)=YBP(iicomp,ij,ibe)+VALY
                    ZBP(iicomp,ij,ibe)=ZBP(iicomp,ij,ibe)+VALZ
  89              continue
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

C **************** globalrotate
C globalrotate is passed instructions on a rotation to be globally
C applied to zones in an esp-r model. It knows about dependencies and
C will act on or warn users as appropriate (if in interactive mode).
C Parameters:
C   val (real) is the rotation angle (degrees, +ve anticlockwise)
C   x1,y1 (real) are the X Y point to rotate around
C   izlistnb (integer) is the number of selected zones
C   izlist is the array holding selected zone indicies to rotate
C   silent (logical) if true then do not ask user about rotations
C      of obstruction data.

C Also apply rotation directives to rotateit array so that later the
C user can un-do this action.

      subroutine globalrotate(val,x1,y1,izlistnb,izlist,silent,itru,ier)

      implicit none
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition
      integer izlistnb,izlist,itru,ier  ! type for passed parameters
      dimension izlist(MCOM)  ! array of which zones are selected for transform.
      real val        ! rotation angl
      real x1,y1      ! the point to rotate around
      logical silent  ! if true then skip dialogs

      integer ifil
      common/FILEP/IFIL
      integer iuout,iuin
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      integer ncub,ivfok
      real XOC,YOC,ZOC,DXC,DYC,DZC,CANG
      character CUBN*6
      COMMON/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)
      
      logical newgeo  ! to use for testing if new/old geometry file.
      
      character*72 LTMP
      character ZN*12,h*72
      character outs*124
      logical DOK,OK
      integer itrc,iuf,iicomp,ij,iz
      integer ibe   ! loop for faces of obstruction
      real A,pi,XR,YR,XXX,YYY,CA,SA
       
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

C Remember user directive for future un-do.
          rotateit(iicomp,1)=0.0
          rotateit(iicomp,2)=0.0
          rotateit(iicomp,3)=0.0
          rotateit(iicomp,4)=val
          rotateit(iicomp,5)=x1; rotateit(iicomp,6)=y1
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

C Depending on the nature of the change update the common block pmchange.
          call warnmod(IICOMP,'str')

C Rotate any MRT sensors associated with this zone.
          if (IVF(IICOMP).eq.1) then
            NCUB = 0
            CALL ERMRT(ITRC,ITRU,IUF,LVIEW(IICOMP),IICOMP,IER)
            if (NCUB.gt.0) then
              if(silent)then
                ok= .true.
              else
                dok=.true.
                h(1)='You already rotated the zone, you will most'
                h(2)='likely want to rotate MRT sensors for the '
                h(3)='zone as well. '
                write(outs,'(a,a)') zn(1:lnblnk(zn)), ' ?'
                call askok('Rotate MRT sensors of',outs,OK,DOK,3)
              endif
              if (OK) then
                PI = 4.0 * ATAN(1.0)
                A=-VAL*PI/180.
                CA=COS(A)
                SA=SIN(A)
                do 76 ij=1,NCUB
                  XXX=XOC(ij)-X1
                  YYY=YOC(ij)-Y1
                  XR=XXX*CA+YYY*SA
                  YR=YYY*CA-XXX*SA
                  XOC(ij)=XR+X1
                  YOC(ij)=YR+Y1
                  CANG(ij)=CANG(ij)+VAL
  76            continue
                CALL EMKMRT(LVIEW(IICOMP),LGEOM(IICOMP),
     &                      NZSUR(IICOMP),IUF,IICOMP,'v',IER)
              endif
            endif
          endif

C Rotate any obstructions associated with this zone.
          if(IOBS(IICOMP).EQ.1)then
            if(silent)then
              ok= .true.
            else
              dok=.true.
              h(1)='You rotated the zone and so will probably'
              h(2)='want to rotate the solar obstructions which'
              h(3)='are associated with it as well. '
              write(outs,'(4a)') 'Rotate obstructions of ',
     &          zn(1:lnblnk(zn)),' in ',
     &          ZOBS(IICOMP)(1:lnblnk(ZOBS(IICOMP)))
              call askok(outs,' ?',OK,dok,3)
            endif
            if(OK)then
              CALL EGOMST(IUF,IICOMP,ZOBS(IICOMP),0,ITRC,ITRU,IER)
              PI = 4.0 * ATAN(1.0)
              A=-VAL*PI/180.
              CA=COS(A)
              SA=SIN(A)
              do 86 ij=1,nbobs(iicomp)
                XXX=XOB(iicomp,ij)-X1
                YYY=YOB(iicomp,ij)-Y1
                XR=XXX*CA+YYY*SA
                YR=YYY*CA-XXX*SA
                XOB(iicomp,ij)=XR+X1
                YOB(iicomp,ij)=YR+Y1
                BANGOB(iicomp,ij,1)=BANGOB(iicomp,ij,1)+VAL
                do 89 ibe=1,8
                  XXX=XBP(iicomp,ij,ibe)-X1
                  YYY=YBP(iicomp,ij,ibe)-Y1
                  XR=XXX*CA+YYY*SA
                  YR=YYY*CA-XXX*SA
                  XBP(iicomp,ij,ibe)=XR+X1
                  YBP(iicomp,ij,ibe)=YR+Y1
  89            continue
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
                dok=.true.
                write(outs,'(2a)') 'Transforms obstructions of ',
     &          zn(1:lnblnk(zn))
                h(1)='You rotated the zone and so will probably'
                h(2)='want to rotate the solar obstructions which'
                h(3)='are associated with it as well. '
                write(outs,'(2a)') 'Rotate obstructions of ',
     &            zn(1:lnblnk(zn))
                call askok(outs,' ?',OK,dok,3)
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
