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

C edcfg.f provides creation and editing facilities for system
C configuration files:
C  EDCFG   Control system level facilities and configuration file entities.
C  NEWPRB  Support specification of a new model.
C  visualz drives visualisation process - hidden line or raytracing.
C  EDZCOMP Control zone composition facilities.

C ************* EDCFG 
C Control editing of system configuration file and allow updated 
C information to be saved into a new file. ITRU unit number for user 
C output; IER=0 indicates no error. 
      subroutine edcfg(ITRC,ITRU,IER)
#include "building.h"
#include "plant.h"
#include "uncertainty.h"
#include "net_flow.h"
#include "control.h"
#include "gnetwk.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/appw/iappw,iappx,iappy
      common/gzonpik/izgfoc,nzg,nznog(mcom)
      common/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS
      common/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      common/C1/NCOMP,NCON
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/C3F/LCNN
      common/C6/INDCFG
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/PRODB/LPRFDB,IPRODB
      COMMON/PREC2/VOL(MCOM)
      common/user/browse
      common/uhome/upath
      common/CFGV/icfgv
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C Plant network.
      common/C23/IFPNF,LPNF

C Defaults.
      common/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Uncertainty.
      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres
      common/entfile/entflnam,ientxist

C Controls.
      common/bctl/ncf,ibsn(mcf,4),iban(mcf,3),nbcdt(mcf),
     &       ibcdv(mcf,mbcdt,2),nbcdp(mcf,mbcdt),tbcps(mcf,mbcdt,mbcdp),
     &       ibctyp(mcf,mbcdt,mbcdp),ibclaw(mcf,mbcdt,mbcdp),
     &       bmiscd(mcf,mbcdt,mbcdp,misc)
      common/pctl/ncl,ipsn(mcl,4),ipan(mcl,4),npcdt(mcl),
     &       ipcdv(mcl,mpcdt,2),npcdp(mcl,mpcdt),tpcps(mcl,mpcdt,mpcdp),
     &       ipctyp(mcl,mpcdt,mpcdp),ipclaw(mcl,mpcdt,mpcdp),
     &       pmiscd(mcl,mpcdt,mpcdp,misc)
      common/fctl/ncc,ifsn(mcc,4),ifan(mcc,3),nfcdt(mcc),
     &       ifcdv(mcc,mfcdt,2),nfcdp(mcc,mfcdt),tfcps(mcc,mfcdt,mfcdp),
     &       ifctyp(mcc,mfcdt,mfcdp),ifclaw(mcc,mfcdt,mfcdp),
     &       fmiscd(mcc,mfcdt,mfcdp,misc)
      common/gctl/ngf,igsn(mgl,4),igan(mgl,3),ngcdt(mgl),
     &       igcdv(mgl,mgcdt,2),ngcdp(mgl,mgcdt),tgcps(mgl,mgcdt,mgcdp),
     &       igctyp(mgl,mgcdt,mgcdp),igclaw(mgl,mgcdt,mgcdp),
     &       gmiscd(mgl,mgcdt,mgcdp,misc)

      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      common/AFN/IAIRN,LAPROB,LAPRES,LAFRES,ICAAS(MCOM)
      COMMON/AFNZN/zmfn1,zmfn2
      COMMON/MFLOW2/NDNAM(0:MNOD)
      common/MFLDOC/DEPRE(MPRD)
      COMMON/NWKGRD/GRMAX(3),GRSPC(3),GRLYRH(MLYRS)
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKFLNAM,NWKTYPSTR(MNWKTYP)
      COMMON/ICONDBNAM/ICONDBFL      
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DMCMPDBFL,DICONDBFL,dmdbnam
      COMMON/NWKTYP/INWKTYP,vergnf
      COMMON/NWKGRDL/GON,SON
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)
      COMMON/NWKCON/NICNN,ICNS(MNCNN),ICNE(MNCNN),ICNNT(MNCNN),
     & ICNSP(MNCNN),ICNEP(MNCNN),CNWNP(MNCNN,MCIP,3),
     & NCONWP(MNCNN),idatrdom(MNCNN),ddtagatr(MNCNN,MIATRB,5),
     & ddatrib(MNCNN,MIATRB,3),ddmenuatr(MNCNN,MIATRB)

      common/FOPENED/CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      logical DOK,OK,CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      logical MODIFY,MODLEN,MODBND,browse
      logical foundutl,xst,clkok,havefile,unixok
      LOGICAL GON,SON,concat,remote

      dimension ITEMS(29)
      character*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,LCNN
      character LUALF*72,LCNG*15,LLOC*15,cfgroot*24
      character*72 LAPROB,LAPRES,LAFRES,DFILE,LPNF,tfile
      character longtfile*144
      character LCFGF*72,H*72,ITEMS*33,t24*24,ETEXT*82
      character zmfn1*124,zmfn2*124,NDNAM*12,LFIL*72,LPRFDB*72
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF,DCNN,ltmp
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,tmode*8,doit*248
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,entflnam*72,upath*72,remainder*72
      character DEPRE*40,dstmp*24,tab*1,outsn*124,outsd*124
      character zname*12,zdesc*64,CXITM*43,outs*124,fs*1
      CHARACTER*72 NWKNAM,NWKDSC,NWKFLNAM,ICONDBFL
      character ATRTAG*12,ATRMENU*32,ATRICN*12,NWKTYPSTR*12
      character ddtagatr*12,ddmenuatr*32,ddatrib*12
      character SSPARENT*12,CONTMNAM*12
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character*72 DFCFD,DECMPDBFL,DMCMPDBFL,DICONDBFL,dmdbnam
      character ltpath*72,filen*72

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Begin with high level menu, see how many networks and control loops
C are active.
      IUF=IFIL+2
    3 INO=-4
      nets=0
      if(indcfg.ge.2)nets=nets+1
      if(iairn.ge.1)nets=nets+1
      if(ientxist.gt.0)nets=nets+1
      ictl=0
      ictl=ncf+ncl+ncc+ngf
      IER=0
      WRITE(ITEMS(1),'(A,A20)') '  cfg: ',LCFGF(1:22)
      IF(INDCFG.EQ.0)THEN
        ITEMS(2)= 'a domains >> registration only '
      ELSEIF(INDCFG.EQ.1)THEN
        ITEMS(2)= 'a domains >> building only     '
      ELSEIF(INDCFG.EQ.2)THEN
        ITEMS(2)= 'a domains >> plant only        '
      ELSEIF(INDCFG.EQ.3)THEN
        ITEMS(2)= 'l  domains >> building & plant '
      ENDIF
      ITEMS(3)   ='b model context                '
      ITEMS(4)   =' _____________________________ '
      WRITE(ITEMS(5),'(A,I2,A)')' Zones______(',NCOMP,' included)_____'
      ITEMS(6)   ='c   composition                ' 
      WRITE(ITEMS(7),'(A,I2,A)')' Networks___(',nets,' included)_____'
      if(indcfg.ge.2)then
        ITEMS(8)='d   plant & systems (defined)  '
      else
        ITEMS(8)='d   plant & systems            '
      endif
      if(iairn.eq.1)then
        ITEMS(9)='e   vent/hydronic (text)       '
      elseif(iairn.eq.2)then
        ITEMS(9)='e   vent/hydronic (graphic)    '
      else
        ITEMS(9)='e   vent/hydronic              '
      endif
      if(ientxist.gt.0)then
        ITEMS(10)='f   electrical (defined)       '
      else
        ITEMS(10)='f   electrical                 '
      endif
      if(nocntm.gt.0)then
        ITEMS(11)='g   contaminants (defined)     '
      else
        ITEMS(11)='g   contaminants               '
      endif
      WRITE(ITEMS(12),'(A,I2,A)')' Controls___(',ictl,' included)_____'
      if(ncf.eq.0)then
        WRITE(ITEMS(13),'(A)')     'j   zones                       '
      else
        WRITE(ITEMS(13),'(A,I2,A)')'j   zones (',ncf,' loops)       '
      endif
      if(ncl.eq.0)then
        WRITE(ITEMS(14),'(A)')     'k   plant & systems '
      else
        WRITE(ITEMS(14),'(A,I2,A)')'k   plant & systems (',ncl,' loops)'
      endif
      if(ncc.eq.0)then
        WRITE(ITEMS(15),'(A)')     'l   vent/hydronic                '
      else
        WRITE(ITEMS(15),'(A,I2,A)')'l   vent/hydronic (',ncc,' loops)'
      endif
      if(ngf.eq.0)then
        WRITE(ITEMS(16),'(A)')     'm   global system '
      else
        WRITE(ITEMS(16),'(A,I2,A)')'m   global system (',ngf,' loops)'
      endif
      WRITE(ITEMS(17),'(A)')       ' _____________________________   '
      ITEMS(18)  ='n define uncertainties         '
      ITEMS(19)  =' _____________________________ '
      ITEMS(20)  =' Actions ...                   '
      ITEMS(21)  ='o  visualisation               '
      ITEMS(22)  ='p  simulation                  '
      ITEMS(23)  ='q  results analysis            '
      ITEMS(24)  ='r  reporting                   '
      ITEMS(25)  =' _____________________________ '
      ITEMS(26)  ='! save model                   '
      ITEMS(27)  ='? help                         '
      ITEMS(28)  ='- exit this menu               '
      nitems=28

C Help text for this menu.
      H(1)='A model is defined within the system configuration'
      H(2)='file which comprises several sections defining the'
      H(3)='simulation type (zone(s) only, zones + plant, all +'
      H(4)='explicit flow, etc.), the site, the layout, operation'
      H(5)='and construction of zones and sytems, and the control.'
      H(6)='Essentially, the system configuration file references'
      H(7)='other files where the actual defining data is held.'
      H(8)=' '
      H(9)='The structure of this menu reflects the entities of a'
      H(10)='system configuration file; these entities may be used'
      H(11)='jointly and severally to define a model for'
      H(12)='simulation.  For example, it is possible to initially'
      H(13)='define a one zone model and to then add further'
      H(14)='zones or a plant system at some later time.  It is'
      H(15)='also possible to increase the resolution of'
      H(16)='a model by, for example, increasing the number of'
      H(17)='nodes within a wall or associating a CFD domain'
      H(18)='with a zone.'
      H(19)=' '
      H(20)='Specifying a non-existent system configuration file'
      H(21)='will cause one to be created based on default'
      H(22)='information.'

C If user has defined model and perhaps resized the display then
C redraw the model image.
      if(indcfg.eq.2)then
        CALL EMENU('  Project Management',ITEMS,nitems,INO)
      elseif(indcfg.eq.0)then
        CALL EMENU('  Project Management',ITEMS,nitems,INO)
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
            CALL ADJVIEW(ITRC,IER)
          endif

C Re-draw the configuration buttons.
          if(MMOD.EQ.8)then
            call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
            call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
            WRITE(ETEXT,'(2A)')'Project: ',LSNAM(1:lnblnk(LSNAM))
            CALL viewtext(ETEXT,1,1,1)
          endif
        endif
        CALL EMENU('Model Definition',ITEMS,nitems,INO)
      endif
      IF(INO.EQ.nitems)THEN

C Exit back to calling menu and clear dialogue box.
        CALL USRMSG(' ',' ','-')
        RETURN
      ELSEIF(INO.EQ.nitems-1)THEN

C Help text for the menu.
        CALL PHELPD('configuration images',22,'-',0,0,IER)
        goto 3
      ELSEIF(INO.EQ.nitems-2)THEN

C Save current common block information to file.
        if(browse)then
          call usrmsg('Cannot save the model while in browse',
     &                'mode: you must own the model!','W')
          goto 3
        endif

C If configuration file format is version 3 then check to see if there
C are any utility files. If found then ask is the user wants to save
C in older format.
        H(1)='The current model description will be placed'
        H(2)='into this file.'
        DFILE=' '
        ltmp=LCFGF
   89   CALL EASKS(ltmp,' Updated model configuration file?',
     &    ' ',72,DFILE,'updated configuration file',IER,2)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          LCFGF=ltmp
        else
          call usrmsg('Please re-enter the file name. ',' ','W')
          goto 89
        endif
        if(icfgv.eq.3)then
          foundutl=.false.
          DO 45 I=1,ncomp
            if(INDUTL(I).ne.0)foundutl=.true.
  45      CONTINUE
          if(foundutl)then
            CALL EASKAB(
     &      ' Configuration file loaded includes legacy utility file',
     &      ' names. Options:','Save in current format','continue',
     &      IW,3)
            if(IW.EQ.1)then
              icfgv=3

C Get root name and ensure that configuration file ends in .cfg.
              lcfgr=lnblnk(LCFGF)
              lcfgl=lcfgr-3
              if(lcfgr.gt.4)then
                if(LCFGF(lcfgl:lcfgr).eq.'.cfg')then
                  write(cfgroot,'(a)') LCFGF(1:lcfgl-1)
                else
                  write(cfgroot,'(a)') LCFGF(1:lcfgr)
                  write(LCFGF,'(a,a)')LCFGF(1:lcfgr),'.cfg'
                endif
              else
                write(cfgroot,'(a)') LCFGF(1:lcfgr)
                write(LCFGF,'(a,a)')LCFGF(1:lcfgr),'.cfg'
              endif
              h(1)='This `root` word will be used to help create'
              h(2)='system level file names (control, connections).'
              h(3)='It is derived from the configuration file name.'
              t24=cfgroot
              CALL EASKS(t24,' Root name for the project?',
     &         '(single word <24 char)',24,'project','root name',IER,3)
              if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')cfgroot=t24
            elseif(IW.eq.2)then
              goto 3
            endif
          endif

C Also check about connections file.
          if(ncon.gt.1)then
  289       H(1)='The surface topology of the model is held'
            H(2)='in a connections file.'
            write(DCNN,'(a,a)')cfgroot(1:lnblnk(cfgroot)),'.cnn'
            if(LCNN(1:1).eq.' ')LCNN=DCNN
            ltmp=LCNN
            CALL EASKS(ltmp,' Surface connections file name?',' ',
     &        72,DCNN,'system connx file name',IER,2)
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
          CALL ASKOK(' ',
     &      ' Problem creating new file! Try again?',OK,DOK,2)
          IF(OK)GOTO 89
        ENDIF
        goto 3
      ELSEIF(INO.EQ.1)THEN
        continue
      ELSEIF(INO.EQ.2)THEN

C Domains associated with the model.
        h(1)='The domains included in a model are automatically'
        h(2)='updated as you add information to the model. You'
        h(3)='can change this manually - but be WARNED, if you'
        h(4)='change from building only to plant only no zone data'
        h(5)='will be retained in the model.'
        h(6)=' '
        h(7)='Registration level holds site information, images'
        h(8)='documentation prior to the creation of zones and '
        h(9)='sytems. '
        if(indcfg.eq.0)outs='Currently registration only (see help)'
        if(indcfg.eq.1)outs='Currently building only (see help)'
        if(indcfg.eq.2)outs='Currently plant only (see help)'
        if(indcfg.eq.3)outs='Currently building and plant (see help)'
        idindcfg=INDCFG+1
        isindcfg=0
        call MENUATOL(outs,'Domain options:',
     &    'a registration (no zones or system)',
     &    'b building (with optional networks & CFD)',
     &    'c plant only','d building & systems & all options',
     &    ' ',' ',' ',' ',' ',' ',' ',' ',isindcfg,idindcfg,9)
        if(isindcfg.eq.0)then
          continue
        elseif(isindcfg-1.ne.indcfg)then
          dok=.false.
          h(1)='Normally the simulation domains included in a model'
          h(2)='will be updated automatically as new information is'
          h(3)='added. Changing these manually can revise your model'
          h(4)='considerably: for example if you have zones and plant'
          h(5)='defined and you change to plant only, all references'
          h(6)='to zone geometry will be removed. The files will still'
          h(7)='exist, but you will have to re-establish the referencs.'
          call askok('Are you sure you want to change the domains',
     &      'in the model? (you might loose information)',ok,dok,7)
          if(ok)then
            indcfg=isindcfg-1
            call usrmsg('Updating model domains...',' ','-')
            CALL EMKCFG('-',IER)
            call usrmsg('Updating model domains...done.',' ','-')
          endif
        endif
        IF(MMOD.EQ.8)then
          LN=max(1,LNBLNK(LSNAM))
          call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                 iicfgz,iicfgn,iicfgc,iicfgdfn)
          call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                 iicfgz,iicfgn,iicfgc,iicfgdfn)
          WRITE(ETEXT,'(2A)')'Project: ',LSNAM(1:ln)
          CALL viewtext(ETEXT,1,1,1)
        endif
      ELSEIF(INO.EQ.3)THEN

C Model context.
        IF(CFGOK)THEN
          CALL EDSITE(ITRC,IER)
        ELSE
          CALL USRMSG(' ',' Please define your model first!','W')
        ENDIF
        goto 3
      elseif(INO.EQ.6)THEN

C Go to zones definintion menu.
        call EDZCOMP(ITRC,ITRU,IER)
      ELSEIF(INO.EQ.8)THEN

C Plant network description, ask for plant network file and then
C display it before starting up editing facilities.
        IF(.NOT.CFGOK)THEN
          CALL USRMSG(' ',' Please define your model first!','W')
          GOTO 3
        ELSE
          if(LPNF(1:4).eq.'UNKN'.or.LPNF(1:2).eq.'  ')then
            dok=.false.
            h(1)='A plant network contains descriptions of detailed'
            h(2)='system components (e.g. boilers) which support'
            h(3)='higher resolution performance assessments. Unless'
            h(4)='you have access to component data DO NOT use this'
            h(5)='facility. '
            CALL ASKOK(
     &      'Currently there is no plant network associated with the',
     &      'model. Create/ select one? (see help)',OK,DOK,5)
            if(.NOT.ok)goto 3
            if(netpth(1:2).eq.'  '.or.netpth(1:2).eq.'./')then
              WRITE(LPNF,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.pln'
            else
              WRITE(LPNF,'(4a)') netpth(1:lnblnk(netpth)),'/',
     &         cfgroot(1:lnblnk(cfgroot)),'.pln'
            endif
            clkok=.false.
            ltmp=LPNF
            H(1)='The plant network description file defines plant'
            H(2)='components and topology. '
            CALL EASKS(ltmp,' Plant network definition file?',' ',72,
     &        DPNF,'plant network file',IER,2)
            goto 46
          endif
          H(1)='The plant network description file defines plant'
          H(2)='components and topology. If you supply an existing'
          H(3)='file it will be used, otherwise a new file will'
          H(4)='be created.'
          H(5)='The dereference option clears the name of the plant '
          H(6)='file but does not alter plant description. '
          H(7)='You can re-establish this domain by including '
          H(8)='the file name. '
          ltmp=LPNF
 301      CALL EASKSCMD(ltmp,' Plant network definition file?',' ',
     &      'dereference',clkok,72,DPNF,'plant network file',IER,8)
          call usrmsg(' ',' ','- ')

C If user wishes to deselect the current file name to
C blank and save the configuration file.
  46      if(clkok)then
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
            call usrmsg('Please re-enter the file name. ',' ','W')
            goto 301
          endif

          XST=.false.
          call FINDFIL(LPNF,XST)
          if(.not.xst) then
            dok=.false.
            h(1)='Did not find the file, indicate ok if you want '
            h(2)='this to be a new plant network. '
            CALL ASKOK(' This is a NEW plant network file.',
     &        ' Proceed with plant network description?',OK,dok,2)
            ipedit=0
          else
            dok=.true.
            h(1)='An existing file was found, if you want to use it'
            h(2)='or modify it indicate ok. '
            CALL ASKOK(' Found existing plant network file. ',
     &        ' Proceed with plant network description?',OK,dok,2)
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
     &          'No change in model recorded because of an error in',
     &          'the plant network description.','E')
            else
              call usrmsg('Updating model for plant network...',
     &          ' ','-')
              CALL EMKCFG('-',IER)
              call usrmsg('Updating model for plant network...done.',
     &          ' ','-')
            endif
          endif
        endif
      ELSEIF(INO.EQ.9)THEN

C Flow description.
        H(1)='It is possible to schedule air flowing from the'
        H(2)='outside (infiltration) and from specified zones or'
        H(3)='plant components (ventilation).  This is sometimes'
        H(4)='appropriate at an early design stage and for flow'
        H(5)='problems of limited complexity.'
        H(6)=' '
        H(7)='The flow network approach is based on the use of'
        H(8)='flow components, representing doors, cracks, ducts,'
        H(9)='fans, etc, to explicitly represent the distributed'
        H(10)='leakage for numerical solution.  This approach '
        H(11)='requires significantly more information.'
        H(12)=' '
        H(13)='A related facility, as described elsewhere, allows'
        H(14)='a CFD domain to be linked to this flow network so'
        H(15)='that intra-zone flows can be evaluated as a function'
        H(16)='of the flows throughout the overall model.'
        H(17)=' '
        H(18)='Flow networks can be represented as a text-based  '
        H(19)='network or a network of icons (otherwise known as a'
        H(20)='graphic network). Both are made up of lists of'
        H(21)='nodes, components and connections. The latter is '
        H(22)='manipulated by an application called `net` and the'
        H(23)='text-based version is edited within the project '
        H(24)='manager. '
        H(25)=' '
        H(26)='Note that a text-based network is a sub-set of the'
        H(27)='information within a graphic network file, so a text-'
        H(28)='based file can be derived from a graphic flow network'
        H(29)='but the reverse is not possible.'
        if(iairn.eq.1)then
          call edisp(iuout,'Ventilation/hydronic network (text).')
        elseif(iairn.eq.2)then
          call edisp(iuout,'Ventilation/hydronic network (graphic).')
        else
          call edisp(iuout,'No network defined.')
        endif

C << version with external agent >>
C        CALL EASKABCD(' Define air movement',
C     &    ' via:','schedules','network','network agent','continue',IW,16)
        CALL EASKABCD(' Define air movement',' via:',
     &    'schedules','network','graphic network','continue',IW,29)
        IF(IW.EQ.1)THEN

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
        ELSEIF(IW.EQ.2)THEN
          call tstamp('>','PRJ: enter fluid network')
          IAIRN=1
          call MFPROB(IER)
        elseif(IW.eq.3)then

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
              if(netpth(1:2).eq.'  '.or.(ichar(netpth(1:1)).eq.46.and.
     &           ichar(netpth(2:2)).eq.92))then
                WRITE(LAPROB,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.gnf'
              else
                WRITE(LAPROB,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &          cfgroot(1:lnblnk(cfgroot)),'.gnf'
              endif
            endif
          endif
          LFIL=LAPROB

C Graphic network. Create a summery file of zone domain information
C for `net` to use. Begin by reading current pressure database. Write
C to the unit number 42.
          call tstamp('>','PRJ: export to graphic agent')
          CALL ERPRCDB(LAPRES,0,3,IER)
          if (ier.ne.0) THEN
            CALL EDISP(IUOUT,'Error opening pressure coef. database.')
            goto 3
          endif 
          lenlfil=lnblnk(LFIL)
          write(ltmp,'(2a)',iostat=ios,err=13)
     &      LFIL(1:lenlfil-4),'.summary'
          CALL EFOPSEQ(42,ltmp,4,IER)
          tab=','
          call dstamp(dstmp)
          write(42,'(3a)',iostat=ios,err=13) '*Synopsis',tab,'for_flow'
          write(42,'(3a)',iostat=ios,err=13) '*Date',tab,dstmp
          write(42,'(3A)',iostat=ios,err=13) '*cfg',tab,
     &      LCFGF(1:lnblnk(LCFGF))
          write(42,'(2a,i3)',iostat=ios,err=13) '*Pressures',tab,NPRE
          do 201 IM=1,NPRE
            write(42,'(a)') DEPRE(IM)
  201     continue
          write(42,'(a)',iostat=ios,err=13) '*End_Pressures'
          write(42,'(2a,i3,a)',iostat=ios,err=13) '*Zones',tab,NCOMP,
     &      ' # name, volume, centre @ XYZ'
          do 97 IZ=1,NCOMP
            write(outsn,'(a,1x,f7.1,3f8.3)') 
     &      zname(IZ),VOL(IZ),ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3)
            call SDELIM(outsn,outsd,'C',IW)
            write(42,'(a)') outsd(1:lnblnk(outsd))
  97      continue
          write(42,'(a)',iostat=ios,err=13) '*End_Zones'

C Write surface attributes that might be of interest. Find
C lowest and highest point and cog of surface.
          write(42,'(2a,i3,2a)',iostat=ios,err=13) '*Surfaces',tab,NCON,
     &      ' # connection, description, area, azimuth, elevation, ',
     &      ' Z Min/COG/Max'
          DO 930 IZ = 1,NCOMP
            CALL EGOMIN(IUF,LGEOM(IZ),IZ,1,0,iuout,IER)
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
          h(2)='net tool. If it contains information about a flow'
          h(3)='network it can be used as a replacement for a non-'
          h(4)='graphic flow network description.'
          CALL EASKS(LFIL,' Network graphics file? ',' ',72,
     &      DFILE,'network file open',IER,4)

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

C append path to new command line.
          call addpath(LFIL,longtfile,concat)

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
              write(outs,'(3A)')zname(IZ)(1:lnblnk(zname(IZ))),
     &          ' is currently linked to: ',NDNAM(ICAAS(IZ))
              h(1)='You can have nodes which are not linked to a'
              h(2)='thermal zone, but if you do link nodes and zones'
              h(3)='then their temperature will match the zone. '
              CALL EASKABC(outs,' ','accept',
     &          'select another','free link',iacc,3)
              if(iacc.eq.1)then
                goto 33
              elseif(iacc.eq.2)then
                IC=0
                call ASKRNOD(' available nodes','-',IC,IER)
                ICAAS(IZ)=IC
              else
                ICAAS(IZ)=0
              endif
            else
              write(outs,'(A,A)') zname(IZ)(1:lnblnk(zname(IZ))),
     &          ' has no mass flow node. '
              CALL EASKAB(outs,' ','accept','select a node',iacc,3)
              if(iacc.eq.1)then
                ICAAS(IZ)=0
              elseif(iacc.eq.2)then
                IC=0
                call ASKRNOD(' available nodes','-',IC,IER)
                ICAAS(IZ)=IC
              endif
            endif
  33      CONTINUE

C Clear zmfn1 & zmfn2 so that updated data will be written in cfg file.
          zmfn1 = ' '
          zmfn2 = ' '

C Save the configuration file so that connections and graphic mass flow
C network are know.
          dok=.true.
          h(1)='You have been working on an independant description of '
          h(2)='a flow network. To use it in a simulation you need to '
          h(3)='associate it with zones in the current model. '
          CALL ASKOK(' Save graphic flow network links to zones',
     &      ' in the configuration file? ',OK,dok,3)
          IF(OK)then
            IAIRN=2
            LAPROB=NWKFLNAM
            call edisp(iuout,' ')
          call edisp(iuout,' Updating configuration flow links...')
            CALL EMKCFG('-',IER)
          call edisp(iuout,' Updating configuration flow links...done.')
          endif

C << version with external agent >>
C        elseif(IW.eq.3)then
C          call tstamp('>','PRJ: export to java agent')
C          doit = '/usr/esru/bin/flow_agent.sh'
C          call runit(doit,'-')
        endif
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

C Global control description.
        call tstamp('>','PRJ: enter global control')
        icfoc=3
        CALL CONTRL(ITRC,ITRU,icfoc,IER)
        CTLOK=.TRUE.
      ELSEIF(INO.EQ.17)THEN

C Optical << future position >>.
      ELSEIF(INO.EQ.18)THEN

C Call uncertainty definition controller.
        H(1)=' This option should be used with care '
        H(2)=' since it is still under development. '
        H(3)=' Consequently it is password protected. '
        H(4)=' '
        H(5)=' The password is available from ESRU  '
        H(6)='    Email: esru@strath.ac.uk  '
        H(7)='    Tele: +44 141 548 3986  '
        H(8)='    Fax:  +44 141 552 8513  '
        call easki(IPASS,' ',' Password ? ',
     &                            0,'-',0,'-',0,'password',IER,8)
        if (IPASS.eq.101) then
          call tstamp('>','PRJ: enter uncertainty control')
          call UNCERTA(LUALF)
        endif
      ELSEIF(INO.EQ.21)THEN

C Select visualisation option.
        IF(CFGOK)THEN
          H(1)='In addition to the in-built perspective view you can'
          H(2)='request a hidden line view of the model or send '
          H(3)='it to be rendered within a visual simulator. '
          CALL EASKABC('Visualisation options:',' ','Wireframe',
     &      'Colour rendered','continue',IW,3)
          if(IW.eq.1)then
            call visualz('V',ier)
          elseif(IW.eq.2)then
            call visualz('R',ier)
          elseif(IW.eq.3)then
            goto 3
          endif
        ELSE
          CALL USRMSG(' ',' Please define your model first!','W')
        ENDIF
        goto 3
      ELSEIF(INO.EQ.22)THEN

C Call simulation controller.
        call tstamp('>','PRJ: enter simulation setup')
        call simula(ier)
      ELSEIF(INO.EQ.23)THEN

C Call results analysis module assuming same simulation parameter
C set as the last simulation. If isset not instanciated then set = 1.

C Logic to react when browsing a model. If remote set to true then
C the results file should be placed in the users home folder.
        remote=.false.
        if(browse)then
          remote=.true.
        else
          ltmp = LCFGF
          call addpath(ltmp,longtfile,concat)
          if(concat)then
            remote=.true.
          endif
        endif

        if(nsset.gt.0)then
          if(isset.eq.0)isset=1
          if(INDCFG.eq.2)then
            if(splres(isset)(1:2).ne.'  '.and.
     &         splres(isset)(1:4).ne.'UNKN')then
              tfile=splres(isset)
              if(remote)then
                call fdroot(tfile,ltpath,filen)
                call isunix(unixok)
                if(unixok)then
                  if (ICHAR(ltpath(1:1)).ne.47) then
                    write(tfile,'(3a)') upath(1:lnblnk(upath)),'/',
     &                filen(1:lnblnk(filen))
                  endif
                else
                  if (ltpath(2:2).ne.':') then
                    write(tfile,'(3a)') upath(1:lnblnk(upath)),'/',
     &                filen(1:lnblnk(filen))
                  endif
                endif
C Debug.
C                write(6,*) 'remote so reset tfile to ',tfile
              endif
              havefile=.true.
            else
              tfile=' '
              havefile=.false.
            endif
            call edisp(iuout,
     &        'Take a note of the results file used...')
            call edisp(iuout,tfile)
          else
            if(sblres(isset)(1:2).ne.'  '.and.
     &         sblres(isset)(1:4).ne.'UNKN')then
              tfile=sblres(isset)
              if(remote)then
                call fdroot(tfile,ltpath,filen)
                call isunix(unixok)
                if(unixok)then
                  if (ICHAR(ltpath(1:1)).ne.47) then
                    write(tfile,'(3a)') upath(1:lnblnk(upath)),'/',
     &                filen(1:lnblnk(filen))
                  endif
                else
                  if (ltpath(2:2).ne.':') then
                    write(tfile,'(3a)') upath(1:lnblnk(upath)),'/',
     &                filen(1:lnblnk(filen))
                  endif
                endif
C Debug.
C                write(6,*) 'remote so reset tfile to ',tfile
              endif
              havefile=.true.
            else
              tfile=' '
              havefile=.false.
            endif
            call edisp(iuout,
     &        'Take a note of the results file used...')
            call edisp(iuout,tfile)
          endif
        else

C There are no simulation parameter sets so take a guess at the
C file name that the user might have given.
          if(browse)then
            write(tfile,'(2a)') upath(1:lnblnk(upath)),'/libb'
          else
            tfile='libb'
          endif
          call edisp(iuout,'Take a note of the results file used...')
          call edisp(iuout,tfile)
        endif
        call tstamp('>','PRJ: beginning res')
        doit = ' '
        call tchild(ICPMOD)
        call termode(ICPMOD,tmode)

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
        if(iappw.gt.0.and.iappw.le.200)then
          if(havefile)then
            write(doit,'(3a,3i4,3a)') 'res -mode ',tmode,
     &        ' -s ',iappw,iappx+35,iappy+45,' -file ',
     &        tfile(1:lnblnk(tfile)),' & '
          else
            write(doit,'(3a,3i4,a)') 'res -mode ',tmode,
     &        ' -s ',iappw,iappx+35,iappy+45,' & '
          endif
        else
          if(havefile)then
            write(doit,'(5a)') 'res -mode ',tmode,' -s 0 0 0 -file ',
     &        tfile(1:lnblnk(tfile)),' & '
          else
            write(doit,'(3a)') 'res -mode ',tmode,' -s 0 0 0 & '
          endif
          call edisp(iuout,doit)
        endif
        call usrmsg('starting assessment recovery via',doit,'-')
        call runit(doit,tmode)

      ELSEIF(INO.EQ.24)THEN

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
      common/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/gzonpik/izgfoc,nzg,nznog(mcom)
      common/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS
      common/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY7/ZXMN(MCOM),ZYMN(MCOM),ZZMN(MCOM),ZXMX(MCOM),
     &            ZYMX(MCOM),ZZMX(MCOM),ZBFLG(MCOM)
      common/C1/NCOMP,NCON
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),BANG(MB)
      common/user/browse
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      common/appw/iappw,iappx,iappy
      common/cctlnm/ctldoc,lctlf
      common/cctl/icascf(mcom)

      common/GR3D100/BLDG3D,ZONE3D(MCOM)
      common/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)
      common/MOIST01/MSTROK,MSTRZN(MCOM)
      common/MOIST02/LMOIST(MCOM)
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
      common/p3ver/ip3ver

      common/FOPENED/CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK

      COMMON/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)

C Descriptive label for a zone load or casual gain types.
      common/loadlabel/lodlabel(mcom,7)
      COMMON/AFN/IAIRN,LAPROB,LAPRES,LAFRES,ICAAS(MCOM)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)

      logical OK,CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK,context
      logical MODIFY,MODLEN,MODBND,concat,browse,QUIET
      logical XST,OKOB,OKZT,OKZR
      logical BLDG3D,ZONE3D,MSTROK,MSTRZN,OKC,dok,clkok
      logical unixok,silent

C Flag noting whether casual gain periods are currently sorted.
      logical sorted,problem

      dimension ITEMS(24),IVALS(MCOM)
      dimension jict(MS),jic2(MS),jie2(MS)
      dimension jsotf(MS),jsmlcn(MS),jsvfc(ms),jsother(ms)
      dimension jsname(MS),jsparent(MS)

      character*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      character*72 CFILE,DFILE,LTMP,LCFD
      character*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,LMOIST
      character LCFGF*72,H*72,ITEMS*33,SFIL*72
      character SOTHER*15,SMLCN*12,SVFC*4,SOTF*4,SSPARENT*12
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      CHARACTER jsmlcn*12,jsvfc*4,jsotf*4,jsother*15,jsname*12
      character jsparent*12
      character doit*248,tmode*8,zname*12,DSFIL*72,ZN*12
      character outs*124,SNAME*12,cfgroot*24
      character ctldoc*248,LCTLF*72,lctmp*72,longtfile*144
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      character HOLD*32,zdesc*64,zd*64
      CHARACTER ETEXT*82,CUBN*6
      character sfile*72,snpfile*72,fs*1,lodlabel*6
      CHARACTER*72 LAPROB,LAPRES,LAFRES

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
      WRITE(ITEMS(1),'(A,I3,A)') ' Zones ...    (',NCOMP,' included)'
      ITEMS(2)   ='a  geometry & attribution        '
      ITEMS(3)   ='b  constructions                 '
      ITEMS(4)   ='c  operation                     '
      ITEMS(5)   =' _____________________________   '
      WRITE(ITEMS(6),'(A,I3,A)') ' Topology ... (',NCON,' connections)'
      ITEMS(7)   ='d  connections & anchors         '
      ITEMS(8)   =' _____________________________   '
      ITEMS(9)   =' Options ...                     ' 
      ITEMS(10)  ='f  shading & insolation          '   
      ITEMS(11)  ='g  convection calculations       ' 
      ITEMS(12)  ='h  view factors & radiant sensors'
      ITEMS(13)  ='i  casual gains control          '
      ITEMS(14)  ='j  computational fluid dynamics  '
      ITEMS(15)  ='k  adaptive gridding & moisture  '
      ITEMS(16)  =' _____________________________   '    
      ITEMS(17)  =' Special components ...          '
      ITEMS(18)  ='m  building-integrated renewables'
      ITEMS(19)  ='n  active materials              '
      ITEMS(20)  ='o  advanced optics               '
      ITEMS(21)  =' _____________________________   '
      ITEMS(22)  ='* global tasks                   '
      ITEMS(23)  ='? help                           '
      ITEMS(24)  ='- exit this menu                 '
      nitems=24

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
          CALL ADJVIEW(ITRC,IER)
        endif
        if(MMOD.eq.8)then
          call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
          call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
          WRITE(ETEXT,'(2A)')'Project: ',LSNAM(1:lnblnk(LSNAM))
          CALL viewtext(ETEXT,1,1,1)
        endif
      endif

C Help text for this menu.
      H(1)='This menu provides facilities to define zone-level'
      H(2)='data including geometry & surface attributes,'
      H(3)='constructions, schedules, shading & insolation,'
      H(4)='convection calculations, view factor, etc.  It also'
      H(5)='supports the definition of contiguity and surface'
      H(6)='boundary conditions'

      CALL EMENU('Zones Definition',ITEMS,nitems,INO)
      IF(INO.EQ.nitems)THEN

C Exit back to calling menu and clear dialogue box.
        CALL USRMSG(' ',' ','-')
        RETURN
      elseif(INO.EQ.nitems-1)THEN
        CALL PHELPD('zones definition',6,'-',0,0,IER)
      elseif(INO.EQ.nitems-2)THEN

C Global tasks such as transforms, rotations, attributions.
        h(1)='You can apply transforms and rotations to one or more'
        h(2)='zones in the model. Global attribution allows for '
        h(3)='surfaces matching some criteria (i.e. vertical &'
        h(4)='opaque) to be given a particular attribute.'
        call EASKATOG(' Global tasks (ie. to several zones):',' ',
     &    'transform','rotate','attribute','search & replace',
     &    'cancel',' ',' ',IRT,4)
        if(IRT.eq.1)then

C Ask for transform distance then which zones and then apply.
          H(1)='Transform shifts each zone & its solar obstructions'
          H(2)='by a given amount in each axis.'
          hold = ' 0.0  0.0  0.0'
          clkok=.false.
  43      CALL EASKSCMD(HOLD,'Transform (X Y Z metres):',' ','cancel',
     &      clkok,32,' 0. 0. 0.','transforms XYZ',IER,2)
          if(clkok) goto 3
          K=0
          CALL EGETWR(HOLD,K,VALX,-99.,99.,'W','X tr',IER)
          CALL EGETWR(HOLD,K,VALY,-99.,99.,'W','Y tr',IER)
          CALL EGETWR(HOLD,K,VALZ,-99.,99.,'W','Z tr',IER)
          if(ier.ne.0)goto 43
          H(1)=' Pick one, several or all zones for inclusion'
          INPIC=NCOMP
          CALL EPICKS(INPIC,IVALS,' ',' Which zones to transform: ',
     &      12,NCOMP,zname,' zone list',IER,1)
          IF(INPIC.EQ.0)GOTO 3
          DO 83 IZ=1,INPIC
            IF(IVALS(IZ).GT.0)THEN
              IICOMP=IVALS(IZ)
              write(zn,'(A)') zname(IICOMP)
              write(outs,'(3a)') ' Transforming: ',zn(1:lnblnk(zn)),
     &          '...'
              CALL USRMSG(' ',outs,'-')
              LTMP=LGEOM(IICOMP)
              CALL EGOMIN(IUF,LTMP,IICOMP,1,0,iuout,IER)
              DO 62 I=1,NTV
                X(I)=X(I)+VALX
                Y(I)=Y(I)+VALY
                Z(I)=Z(I)+VALZ
   62         continue
              ZBFLG(IICOMP)=0.
              CALL EMKGEO(IUF,LTMP,IICOMP,ITRU,3,IER)

C After transform update the g6 & g7 commons and find dependencies.
              call zgupdate(0,IICOMP,ier)
              call warnmod(IICOMP,'str')

C Transform any MRT sensors associated with this zone.
              if (IVF(IICOMP).eq.1) then
                NCUB = 0
                CALL ERMRT(ITRC,ITRU,IUF,LVIEW(IICOMP),IICOMP,IER)
                if (NCUB.gt.0) then
                  write(outs,'(2a)') zn(1:lnblnk(zn)),' as well?'
                  dok=.true.
                  h(1)='You rotated the zone, you probably also want'
                  h(2)='to rotate the sensors so match.'
                  call askok('Transform MRT sensors of',outs,OK,dok,2)
                  if (OK) then
                    do 73 ij=1,NCUB
                      XOC(ij)=XOC(ij)+VALX
                      YOC(ij)=YOC(ij)+VALY
                      ZOC(ij)=ZOC(ij)+VALZ
  73                continue
                    CALL EMKMRT(LVIEW(IICOMP),LGEOM(IICOMP),
     &                          NZSUR(IICOMP),IUF,IICOMP,'v',IER)
                  endif
                endif
              endif

C If there is an obstructions file, loop through each and transform origin.
              IF(IOBS(IICOMP).EQ.1)THEN
                dok=.true.
                write(outs,'(4a)') 'Transforms obstructions of ',
     &            zn(1:lnblnk(zn)),' in ',
     &            ZOBS(IICOMP)(1:lnblnk(ZOBS(IICOMP)))
                h(1)='You rotated the zone(s) so you probably also '
                h(2)='will want to rotate the associated obstructions.'
                call askok(outs,' ?',OK,dok,2)
                if(OK)then
                  CALL EGOMST(IUF,ZOBS(IICOMP),0,ITRC,ITRU,IER)
                  do 63 ij=1,NB
                    XO(ij)=XO(ij)+VALX
                    YO(ij)=YO(ij)+VALY
                    ZO(ij)=ZO(ij)+VALZ
  63              continue
                  CALL MKGOMST(IUF,ZOBS(IICOMP),IICOMP,IER)
                endif
              endif
              write(outs,'(3a)') ' Transforming: ',zn(1:lnblnk(zn)),
     &          '...done'
              CALL USRMSG(' ',outs,'-')
              call sumrchg(IICOMP,'r')
            endif
   83     continue
          MODIFY=.TRUE.
          MODBND=.TRUE.
        elseif(IRT.eq.2)then

C Rotation degree and choices.
          H(1)='The rotation is applied to all selected zones about'
          H(2)='a common point or the site origin. Positive is  '
          H(3)='anticlockwise. '
          VAL=0.
          CALL EASKR(VAL,' ',' Rotation (deg + = anticlockwise) ? ',
     &      -359.0,'W',359.0,'W',0.0,'rotation',IER,3)
          if(VAL.LT.-.01.OR.VAL.GT..01)then
            CALL EASKABC(' Rotation choices :',' ',
     &        'site origin','user specified X & Y','cancel ?',IRTP,3)
            if(IRTP.eq.1)then
              x1=0.
              y1=0.
            elseif(IRTP.EQ.2)THEN
              H(1)='Point is in the site coordinate system.'
              x1=0.
              CALL EASKR(x1,' ',' X point (metres) ? ',
     &          0.0,'-',0.0,'-',0.0,'x point',IER,1)
              y1=0.
              CALL EASKR(y1,' ',' Y point (metres) ? ',
     &          0.0,'-',0.0,'-',0.0,'y point',IER,1)
            elseif(IRTP.EQ.3)THEN
              goto 3
            endif
            H(1)=' Pick one, several or all zones to rotate.'
            INPIC=NCOMP
            CALL EPICKS(INPIC,IVALS,' ',' Which zones to rotate: ',
     &        12,NCOMP,zname,' zone list',IER,1)
            IF(INPIC.EQ.0)GOTO 3
            call tstamp('>','PRJ: zone rotation')
            DO 84 IZ=1,INPIC
              IF(IVALS(IZ).GT.0)THEN
                IICOMP=IVALS(IZ)
                write(zn,'(A)') zname(IICOMP)
                write(outs,'(3a)') ' Rotating: ',zn(1:lnblnk(zn)),
     &           '...'
                CALL USRMSG(' ',outs,'-')
                LTMP=LGEOM(IICOMP)
                CALL EGOMIN(IUF,LTMP,IICOMP,1,0,iuout,IER)
                CALL ESCROT(VAL,x1,y1)
                ZBFLG(IICOMP)=0.
                CALL EMKGEO(IUF,LTMP,IICOMP,ITRU,3,IER)

C After transform update the g6 & g7 commons and find dependencies.
                call zgupdate(0,IICOMP,ier)
                call warnmod(IICOMP,'str')

C Rotate any MRT sensors associated with this zone.
                if (IVF(IICOMP).eq.1) then
                  NCUB = 0
                  CALL ERMRT(ITRC,ITRU,IUF,LVIEW(IICOMP),IICOMP,IER)
                  if (NCUB.gt.0) then
                    dok=.true.
                    h(1)='You already rotated the zone, you will most'
                    h(2)='likely want to rotate MRT sensors for the '
                    h(3)='zone as well. '
                    write(outs,'(a,a)') zn(1:lnblnk(zn)), ' ?'
                    call askok('Rotate MRT sensors of',outs,OK,DOK,3)
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
  76                  continue
                      CALL EMKMRT(LVIEW(IICOMP),LGEOM(IICOMP),
     &                            NZSUR(IICOMP),IUF,IICOMP,'v',IER)

                    endif
                  endif
                endif

C Rotate any obstructions associated with this zone.
                IF(IOBS(IICOMP).EQ.1)THEN
                  dok=.true.
                  h(1)='You rotated the zone and you probably will'
                  h(2)='want to rotate the solar obstructions which'
                  h(3)='are associated with it as well. '
                  write(outs,'(4a)') 'Rotate obstructions of ',
     &              zn(1:lnblnk(zn)),' in ',
     &              ZOBS(IICOMP)(1:lnblnk(ZOBS(IICOMP)))
                  call askok(outs,' ?',OK,dok,3)
                  if(OK)then
                    CALL EGOMST(IUF,ZOBS(IICOMP),0,ITRC,ITRU,IER)
                    PI = 4.0 * ATAN(1.0)
                    A=-VAL*PI/180.
                    CA=COS(A)
                    SA=SIN(A)
                    do 86 ij=1,NB
                      XXX=XO(ij)-X1
                      YYY=YO(ij)-Y1
                      XR=XXX*CA+YYY*SA
                      YR=YYY*CA-XXX*SA
                      XO(ij)=XR+X1
                      YO(ij)=YR+Y1
                      BANG(ij)=BANG(ij)+VAL
  86                continue
                    CALL MKGOMST(IUF,ZOBS(IICOMP),IICOMP,IER)
                  endif
                endif
                write(outs,'(3a)') ' Rotating: ',zn(1:lnblnk(zn)),
     &          '... done.'
                CALL USRMSG(' ',outs,'-')
                call sumrchg(IICOMP,'r')
              endif
  84        continue
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
          nzg=NCOMP
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
              jsother(jixu)=SSOTHER(IXU)
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

C Read the geometry to fill in the data below.
          NCOMP=NCOMP+1
          CALL EGOMIN(IFIL+1,LTMP,NCOMP,1,ITRC,iuout,IER)
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
          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(DFILE,'(A,A4)') zname(NCOMP)(1:lnblnk(zname(NCOMP))),
     &      '.geo'
          else
            WRITE(DFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &         zname(NCOMP)(1:lnblnk(zname(NCOMP))),'.geo'
          endif
          LGEOM(NCOMP)=DFILE

          H(1)='The zone description allows further information about'
          H(2)='the zone form and composition to be recorded.  '
          write(zd,'(a,a)') 
     &      zname(NCOMP)(1:lnblnk(zname(NCOMP))),' describes a...'
  40      CALL EASKS(zd,' What does it represent ',
     &      ' ( <64 char) ?',64,'no description entered','zone descr',
     &      IER,2)
          IF(zd.eq.' '.or.ier.ne.0)goto 40
          zdesc(NCOMP)=zd

C If an obstruction file check if this should be copied as well.
C Save zone geometry, read in obstructions, write out to another
C file and then recover the zone commons.
          if(IOBS(ic).eq.1)then
            dok=.true.
            h(1)='The original zone had associated solar obstructions'
            h(2)='and you have two options '
            h(3)=' a) copy the obstructions so that modifications'
            h(4)='    relevant to the copied zones obstructions can'
            h(5)='    be made.'
            h(6)=' b) do not copy the obstructions. Optionally you'
            h(7)='    can reference the original obstructions (via'
            h(8)='    the project files menu).'
            CALL ASKOK('Also copy obstructions of source zone?',
     &        '(see help)',OKOB,dok,8)
            if(OKOB)then
              CALL ESCZONE(NCOMP)
              call FINDFIL(ZOBS(IC),XST)
              IF(XST)THEN
                CALL EGOMST(IUF,ZOBS(IC),0,ITRC,ITRU,IER)
                IOBS(NCOMP)=1 
                if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                  WRITE(DFILE,'(2a)') 
     &              zname(NCOMP)(1:lnblnk(zname(NCOMP))),'.obs'
                else
                  WRITE(DFILE,'(4a)') 
     &              zonepth(1:lnblnk(zonepth)),fs,
     &              zname(NCOMP)(1:lnblnk(zname(NCOMP))),'.obs'
                endif
                ZOBS(NCOMP)=DFILE
                CALL MKGOMST(IUF,ZOBS(NCOMP),NCOMP,IER)
              endif
              CALL ERCZONE(NCOMP)
            endif
          endif

C Zone rotations and transforms.
          if(OKOB)then
            H(1)='When copying a zone the copy is usually shifted'
            H(2)='away from the position of the source.  You can '
            H(3)='skip such transforms. '
          else
            H(1)='When copying a zone the copy is usually shifted'
            H(2)='away from the position of the source.  You can '
            H(3)='skip such transforms. '
          endif
          H(4)='Transform shifts the zone by a given amount in'
          H(5)='each axis.'
          hold = ' 0.0  0.0  0.0'
          clkok=.false.
 243      CALL EASKSCMD(HOLD,'Transform (X Y Z metres):',' ',
     &      'skip',clkok,32,' 0. 0. 0.','transforms XYZ',IER,2)
          if(clkok) then
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
            ZBFLG(NCOMP)=0.
            okzt=.true.
          endif
          dok=.false.
          h(1)='If you want to rotate the zone from its initial'
          h(2)='position now is a good time to do this if the '
          h(3)='point of rotation is the first vertex of the zone.'
          CALL ASKOK(' ',' Rotate the copied zone',OKZR,dok,3)
          if(OKZR)then
            H(1)='The rotation is applied around vertex 1 and '
            H(2)='positive is anticlockwise. '
            VAL=0.
            CALL EASKR(VAL,' ',' Rotation (deg + = anticlockwise) ? ',
     &        -359.0,'W',359.0,'W',0.0,'rotation',IER,2)
            if(VAL.LT.-.01.OR.VAL.GT..01)then

C Rotation choices.
             H(1)='The traditional rotation is about vertex 1 of the'
             H(2)='zone with a positive angle being anticlockwise. '
             H(3)='You may wish to rotate a zone about the site '
             H(4)='origin X=0., Y=0. or at a user specified point. '
              CALL EASKABCD(' Rotation choices :',' ',
     &         'Vert 1 of zone','Site origin',
     &         'User specified X & Y','continue ?',IRT,4)
              call usrmsg(' ',' ','-')
              if(IRT.eq.1)then
                x1=X(1)
                y1=Y(1)
                CALL ESCROT(VAL,x1,y1)
              elseif(IRT.EQ.2)THEN
                x1=0.
                y1=0.
                CALL ESCROT(VAL,x1,y1)
              elseif(IRT.EQ.3)THEN
                H(1)='Point is in the site coordinate system.'
                x1=0.
                CALL EASKR(x1,' ',' X point (metres) ? ',
     &            0.0,'-',0.0,'-',0.0,'x point',IER,1)
                y1=0.
                CALL EASKR(y1,' ',' Y point (metres) ? ',
     &            0.0,'-',0.0,'-',0.0,'y point',IER,1)
                CALL ESCROT(VAL,x1,y1)
                call usrmsg(' ',' ','-')
              endif
              ZBFLG(NCOMP)=0.
            endif
          endif
          CALL EMKGEO(IFIL+2,LGEOM(NCOMP),NCOMP,ITRU,3,IER)
          IF(IER.EQ.1)THEN
            dok=.true.
            h(1)='When writing the geometry file a problem was found.'
            h(2)='Check that you have permission to update the file '
            h(3)='and that there is sufficient disk space. '
            CALL ASKOK(' ','Problem creating geometry...try again?',
     &        OK,dok,3)
            IF(OK)GOTO 42
          ENDIF

C Update the connection list and then the model configuration.
C Retain similar and adiabatic connections if found.
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
            SSOTHER(ICCC)=jsother(icc)
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
              SOTHER(ICC)='UNKNOWN'
              SSOTHER(ICCC)='UNKNOWN'
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

C Do same transformation to obstructions?
          if(OKOB)then
            CALL ESCZONE(NCOMP)
            CALL EGOMST(IUF,ZOBS(NCOMP),0,ITRC,ITRU,IER)
            if(OKZT)then
              do 85 ij=1,NB
                XO(ij)=XO(ij)+VALX
                YO(ij)=YO(ij)+VALY
                ZO(ij)=ZO(ij)+VALZ
   85         continue
            endif
            if(OKZR)then
              PI = 4.0 * ATAN(1.0)
              A=-VAL*PI/180.
              CA=COS(A)
              SA=SIN(A)
              do 87 ij=1,NB
                XXX=XO(ij)-X1
                YYY=YO(ij)-Y1
                XR=XXX*CA+YYY*SA
                YR=YYY*CA-XXX*SA
                XO(ij)=XR+X1
                YO(ij)=YR+Y1
                BANG(ij)=BANG(ij)+VAL
  87          continue
            endif
            CALL MKGOMST(IUF,ZOBS(NCOMP),NCOMP,IER)
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
          if(XST)then
            h(1)='You can have the new zone constructions based '
            h(2)='on the source zone (by copying the file)'
            h(3)='or by pointing to the file (exercise care)'
            h(4)='or browse available files or continue without '
            h(5)='defining constructions at this time. '
            CALL EASKATOG(
     &        'Source zone has a constructions file (see help)',
     &        ' ','copy source file','point to source file',
     &        'browse','continue',' ',' ',' ',irop,5)
          else
            h(1)='You can have the new zone constructions by '
            h(2)='browing available files or continue without '
            h(3)='defining constructions at this time. '
            CALL EASKATOG(
     &        'Source zone has no constructions file (see help)',
     &        ' ','-','-','browse','continue',' ',' ',' ',irop,3)
            if(irop.eq.1.or.irop.eq.2) irop=4
          endif

C DFILE is the default file name for any TMC file to be created,
C CFILE is default for constr file.
          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(DFILE,'(A,A4)')zname(NCOMP)(1:lnblnk(zname(NCOMP))),
     &        '.tmc'
            WRITE(CFILE,'(A,A4)')zname(NCOMP)(1:lnblnk(zname(NCOMP))),
     &        '.con'
          else
            WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &        zname(NCOMP)(1:lnblnk(zname(NCOMP))),'.tmc'
            WRITE(CFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &        zname(NCOMP)(1:lnblnk(zname(NCOMP))),'.con'
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
            IF(CFILE.NE.' ')THEN
              QUIET=.false.
              CALL EMKCON(LTHRM(NCOMP),IUF,NCOMP,IUOUT,QUIET,IER)
              if(ITW(IC).eq.1)then

C Update itmcfl for the new zone.
                DO 61 ISS=1,NZSUR(NCOMP)
                  ITMCFL(NCOMP,ISS)=ITMCFL(IC,ISS)
   61           continue
                LTWIN(NCOMP)=DFILE
                ITW(NCOMP)=1
                QUIET=.false.
                CALL MKTWIN(ITRU,IUF,NCOMP,QUIET,IER)
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
            call edisp(iuout,' ')
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
              call edisp(iuout,' ')
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
          if(XST)then
            h(1)='You can have the new zone operations based initially'
            h(2)='on the source zones operations (by copying the file)'
            h(3)='or use the same operations (by pointing to the file)'
            h(4)='or browse available files or continue without '
            h(5)='defining operations at this time. '
            CALL EASKATOG(
     &        'Source zone has an operations file (see help)',
     &        ' ','copy of source file','point to source file',
     &        'browse','continue',' ',' ',' ',irop,5)
          else
            h(1)='You can have the new zone operations by '
            h(2)='browing available files or continue without '
            h(3)='defining operations at this time. '
            CALL EASKATOG(
     &        'Source zone has no operations file (see help)',
     &        ' ','-','-','browse','continue',' ',' ',' ',irop,3)
            if(irop.eq.1.or.irop.eq.2) irop=4
          endif
          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(DFILE,'(A,A4)')zname(NCOMP)(1:lnblnk(zname(NCOMP))),
     &        '.opr'
          else
            WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &        zname(NCOMP)(1:lnblnk(zname(NCOMP))),'.opr'
          endif
          if(irop.eq.1)then

C Read in source operations and write out to file for new zone.
C Also update lodlabel commons before writing out.
            CALL ERPFREE(IUF,ISTAT)
            CALL EROPER(0,iuout,IUF,IC,IER)
   91       H(1)='A zone operation file contains schedules of air '
            H(2)='flow and casual gains.'
            CALL EASKS(DFILE,' New operations file name:',
     &        ' ',72,'new.opr','operations file',IER,2)
            IF(DFILE.NE.' ')THEN
              LPROJ(NCOMP)=DFILE
              lodlabel(ncomp,1)=lodlabel(ic,1)
              lodlabel(ncomp,2)=lodlabel(ic,2)
              lodlabel(ncomp,3)=lodlabel(ic,3)
              CALL ERPFREE(IUF,ISTAT)
              CALL EMKOPER(IUF,LPROJ(NCOMP),NCOMP,iuout,IER)
            else
              goto 91
            endif
          elseif(irop.eq.2)then
            LPROJ(NCOMP)=LPROJ(IC)
          elseif(irop.eq.3)then

C Find out if there are operation files in the model ../zones folder.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
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
            h(1)='The new zone viewfactors can be based initially'
            h(2)='on the source zone (by copying the file) or you'
            h(3)='can point to the source zone file (use care because'
            h(4)='the two zones may change shape) or browse available '
            h(5)='files or continue. '
            CALL EASKATOG(
     &        'Source zone has a viewfactor file (see help)',
     &        ' ','copy source file','point to source file',
     &        'browse','continue',' ',' ',' ',irop,5)
            if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
              WRITE(DFILE,'(A,A4)')zname(NCOMP)(1:lnblnk(zname(NCOMP))),
     &          '.vwf'
            else
              WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &          zname(NCOMP)(1:lnblnk(zname(NCOMP))),'.vwf'
            endif
            if(irop.eq.1)then
              CALL ERPFREE(IUF,ISTAT)
              CALL ERMRT(0,itru,IUF,LVIEW(IC),IC,IER)
   92         H(1)='A zone viewfactor file contains calculations of'
              H(2)='how much each surfaces can see other surfaces.'
              CALL EASKS(DFILE,' New viewfactor file name: ',
     &          ' ',72,'new.vwf','viewfactor file',IER,2)
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
              call edisp(iuout,' ')
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
            h(3)='can point to the source zone file or browse  '
            h(4)='available files or continue.'
            CALL EASKATOG(
     &        'Source zone has a casual gain control file (see help)',
     &        ' ','copy source file','point to source file',
     &        'browse','continue',' ',' ',' ',irop,5)
            if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
              WRITE(DFILE,'(A,A4)')zname(NCOMP)(1:lnblnk(zname(NCOMP))),
     &          '.cgc'
            else
              WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &          zname(NCOMP)(1:lnblnk(zname(NCOMP))),'.cgc'
            endif
            if(irop.eq.1)then
              CALL ERPFREE(IUF,ISTAT)
              call ercgcf(0,iuout,LCGCIN(IC),IC,ier)
   93         H(1)='A zone casual gain control file contains logic'
              H(2)='for light switching in a zone.'
              CALL EASKS(DFILE,' New casual gain control file name:',
     &          ' ',72,'new.vwf','casual gain control file',IER,2)
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
              call edisp(iuout,' ')
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
            CALL EASKATOG(
     &        'Source zone has a convection regime file (see help)',
     &        ' ','copy source file','point to source file',
     &        'browse','continue',' ',' ',' ',irop,4)
            if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
              WRITE(DFILE,'(A,A4)')zname(NCOMP)(1:lnblnk(zname(NCOMP))),
     &          '.htc'
            else
              WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),fs,
     &          zname(NCOMP)(1:lnblnk(zname(NCOMP))),'.htc'
            endif
            if(irop.eq.1)then
              CALL ERPFREE(IUF,ISTAT)
              call ehtcff(LHCCO(IC),IUF,ITRU,IER)
   94         H(1)='A zone convection regime file contains data'
              H(2)='for imposing convections regimes in a zone.'
              CALL EASKS(DFILE,' New convection regime file name:',
     &          ' ',72,'new.vwf','convection regime file',IER,2)
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
              call edisp(iuout,' ')
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
          CALL EGOMIN(IFIL+1,LGEOM(IC),IC,1,ITRC,iuout,IER)
          if(IER.NE.0)then
            dok=.true.
            h(1)='A problem was found while reading the geometry and'
            h(2)='you have the option to try again. Do this if you '
            h(3)='think you migh have the wrong file. '
            CALL ASKOK(' ',
     &       ' Problem reading geometry, try again?',OK,dok,3)
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
            IF(ITRC.GT.1)CALL VERINFO(ITRU)
            CALL INSINFO(IC,ITRU)
          endif

C If there is an obstructions file read it.
          IUF=IFIL+1
          IF(IOBS(IC).EQ.1)CALL EGOMST(IUF,ZOBS(IC),IR,ITRC,ITRU,IER)
        ENDIF
        CALL EDZONE(ITRC,IC,IER)
        MODIFY=.TRUE.

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 248
      elseif(INO.EQ.3)THEN

C Construction file.
C Tell the user to create a model configuration file!
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
          h(1)='If you suspect that your zone thermophysical data'
          h(2)='has been corrupted, is out of date or needs to be'
          h(3)='refreshed this command will do them all. '
          h(4)='But it only works for zones with existing zone '
          h(5)='construction files (it needs to know the names of'
          h(6)='to re-create). '
          CALL ASKOK(
     &'All construction & related files must exist to use this option.',
     &'Continue with automatic update of construction files?',
     &      QUIET,dok,6)
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
        call askzone(ic,izdef,' Select zone for operations focus ?',
     &     'Zone operations','o','Zone index for operations.',ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)then
          MODIFY=.TRUE.
          GOTO 3
        endif
        if(IC.eq.99)then

C Global tasks.
          if(browse)then
            call edisp(itru,' ')
            call edisp(itru,'You are currently in browse mode.')
            call edisp(itru,'Changes to operations file will not')
            call edisp(itru,'be saved!')
            call edisp(itru,' ')
          endif
          h(1)='Can upgrade older unsorted operations files if'
          h(2)='they exist in the current model.'
          h(3)=' '
          h(3)='This will also look at current operations files'
          h(3)='to see if the casual gains periods are still sorted.'
          h(4)='Note this upgrade is an automated process which'
          h(5)='should be checked for validity. '
          CALL EASKABC(
     &      'Zone operations options for multiple zones:',' ',
     &      'update all older files','check ordering in files',
     &      'continue',IW,17)
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
                  if(ip3ver.eq.0)then

C File is older format and assumed to be unsorted.
                    sorted=.false.
                    call PROCESSOLDCAS(ICOMP,0,iuout,IER)
                    if(browse)then
                      continue
                    else
                      ip3ver=1
                      CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,ITRU,IER)
                    endif
                  else

C File is current format and might be unsorted.
                    sorted=.true.
                    problem=.false.
                    call checksort(icomp,1,problem,ier)
                    if(problem)then
                      sorted=.false.
                      call edisp(iuout,
     &                  'Weekday casual gains might be unsorted')
                    endif
                    problem=.false.
                    call checksort(icomp,2,problem,ier)
                    if(problem)then
                      sorted=.false.
                      call edisp(iuout,
     &                  'Saturday casual gains might be unsorted')
                    endif
                    problem=.false.
                    call checksort(icomp,3,problem,ier)
                    if(problem)then
                      sorted=.false.
                      call edisp(iuout,
     &                  'Sunday casual gains might be unsorted')
                    endif
                    if(.NOT.sorted)then
                      call PROCESSOLDCAS(ICOMP,0,iuout,IER)
                      if(browse)then
                        continue
                      else
                        ip3ver=1
                        CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,ITRU,IER)
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
        call tstamp('>','PRJ: enter topology')
        CALL EDCONN(IER)
      elseif(INO.EQ.10)THEN

C Shading/ insolation analysis.
 253    IC=-1
        izdef=0
        call askzone(ic,izdef,' Select zone for shading focus ?',
     &     'Zone shading','s','Zone index for shading.',ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        if(IC.eq.99)then

C Global tasks.
          h(1)='Can evaluate detailed shading and/or insolation'
          h(2)='patterns. Some actions you can take on multiple'
          h(3)='zones.... '
          h(4)=' '
          h(5)='You can recalculate shading in all zones where '
          h(6)='a shading file exists. '
          h(7)=' '
          h(8)='You can nominate zones which you want to include an'
          h(9)='insolation analysis (i.e. track direct solar radiation'
          h(10)='within zones). '
          h(11)=' '
          h(12)='You can nominate zones which you want to create solar'
          h(13)='obstructions in so that shading and insolation can'
          h(14)='be assessed.'
          h(15)=' '
          h(16)='You can nominate zones which you want to dereference'
          h(17)='the insolation analysis'
          CALL EASKABCD(
     &      'Shading/insolation options for multiple zones:',
     &      ' ','recalculate shading','dereference shading files',
     &      'add insolation calculations','continue',IW,17)
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
     &              zname(icomp)(1:lnblnk(zname(icomp))),'.shd'
                else
                  WRITE(LSHAD(icomp),'(3A,A4)') 
     &              zonepth(1:lnblnk(zonepth)),'/',
     &              zname(icomp)(1:lnblnk(zname(icomp))),'.shd'
                endif

C Create a small place-holder file that subsequent calculation
C procedure can overwrite.
                IUF=IFIL+2
                CALL EFOPSEQ(IUF,LSHAD(ICOMP),3,IER)
                WRITE(IUF,'(2a)') 'Placeholder shading file for ',
     &            zname(icomp)(1:lnblnk(zname(icomp)))
                CALL ERPFREE(IUF,ios)
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
          call edisp(itru,' ')
          call edisp(itru,'You are currently in browse mode.')
          call edisp(itru,'The shading analysis module will start')
          call edisp(itru,'but DO NOT attempt to save results!')
          call edisp(itru,' ')
          goto 255
        endif
        IF(ISI(IC).eq.0)THEN
          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(SFIL,'(A,A4)')zname(IC)(1:lnblnk(zname(IC))),'.shd'
          else
            WRITE(SFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &        zname(IC)(1:lnblnk(zname(IC))),'.shd'
          endif
          DSFIL = 'new.shd'
          H(1)='The shading/ insulation database holds time varying'
          H(2)='external surface shading and internal surface'
          H(3)='insolation.'
          CALL EASKS(SFIL,' New zone shading/insulation file?',
     &      ' ',72,DSFIL,'shd/ins db',IER,3)
          if(SFIL(1:2).ne.'  ')then
            LSHAD(IC)=SFIL
            ISI(IC)=1
            call usrmsg(' updating model for shading file...',' ','-')
            CALL EMKCFG('-',IER)
            call usrmsg(' updating model for shading file...done.',
     &        ' ','-')
          endif
        else

C Confirm existing shading file name and offer option to dereference.
          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(DSFIL,'(A,A4)')zname(IC)(1:lnblnk(zname(IC))),'.shd'
          else
            WRITE(DSFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &        zname(IC)(1:lnblnk(zname(IC))),'.shd'
          endif
          SFIL = LSHAD(IC)
          clkok=.false.
          H(1)='The shading/ insulation database holds time varying'
          H(2)='external surface shading and internal surface'
          H(3)='insolation. If you want to ignore the existing'
          H(4)='shading file click on dereference. '
          CALL EASKSCMD(SFIL,'Zone shading/insulation file?',
     &      ' ','dereference',clkok,72,DSFIL,'shd/ins db',IER,4)
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
              call usrmsg(' updating model to include shading file...',
     &        ' ','-')
              CALL EMKCFG('-',IER)
              call usrmsg(
     &          ' updating model to include shading file...done.',
     &          ' ','-')
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
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          call addpath(LCFGF,longtfile,concat)

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,5a)') 'ish -mode ',tmode,
     &        ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &        longtfile(1:lnblnk(longtfile)),' -zone ',
     &        ZN(1:lnblnk(ZN)),' &'
          else
            write(doit,'(7a)') 'ish -mode ',tmode,' -s 0 0 0 -file ',
     &        longtfile(1:lnblnk(longtfile)),' -zone ',
     &        ZN(1:lnblnk(ZN)),' &'
          endif
          call usrmsg('starting shading analysis via',doit,'-')
          call runit(doit,tmode)
        endif

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 253
      elseif(INO.EQ.11)THEN

C Zone convective regime description. If an existing file the
C call to hcfmk has the option to dereference it.

C If browsing, do not save files but allow ish to be started.
        if(browse)then
          call edisp(itru,' ')
          call edisp(itru,'You are currently in browse mode. You can')
          call edisp(itru,'look at the heat transfer regimes')
          call edisp(itru,'but DO NOT attempt to change them!')
          call edisp(itru,' ')
        endif
 260    IC=-1
        izdef=0
        call askzone(ic,izdef,' Select zone for convective focus ?',
     &     'Zone convective methods','h','Zone index for hc.',ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        IUO=IFIL+1
        call tstamp('>','PRJ: enter hc file')
        CALL HCFMK(ITRC,ITRU,IUO,IC,IER)
        MODIFY=.TRUE.

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 260
      elseif(INO.EQ.12)THEN

C View factor editing, begin by identifying the zone and then
C reading in the geometry file followed by view factor editing 
C facilities. Call to edmrt might result in dereferenceing of
C the file.         
        if(browse)then
          call edisp(itru,' ')
          call edisp(itru,'You are currently in browse mode. You can')
          call edisp(itru,'look at the surface viewfactor data')
          call edisp(itru,'but DO NOT attempt to recalculate them!')
          call edisp(itru,' ')
        endif
 251    IC=-1
        izdef=0
        call askzone(ic,izdef,' Select zone for viewfactor focus ?',
     &     'Zone viewfactors','v','Zone index for viewfact.',ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        if(IC.eq.99)then

C Global tasks.
          h(1)='Can evaluate surface-to-surface viewfactors as either'
          h(2)='area weighted or via ray tracing. Some actions you '
          h(3)='can take on multiple zones.... '
          h(4)=' '
          h(5)='Area weighted viewfactors are, essentailly the default'
          h(6)='treament and take almost no time to calculate. '
          h(7)=' '
          h(8)='Ray-traced viewfactors have a computational cost but '
          h(9)='may be useful in some models. '
          h(10)=' '
          h(11)='You can nominate zones which you want to include'
          h(12)='either type of viewfactor. After selection these will'
          h(13)='be instanciated within the relevant zones. '
          h(14)=' '
          h(15)='You can nominate zones which you want to dereference'
          h(16)='the viewfactor analysis'
          CALL EASKABCD(
     &      'Viewfactor options for multiple zones:',
     &      ' ','do area weighted','do ray trace calculations',
     &      'dereference viewfactors','continue',IW,16)
          IUO=IFIL+1
          if(iw.eq.1.or.iw.eq.2)then
            inpic=0
            call askmultizone(inpic,ivals,
     &        ' Select zones for viewfactors?',
     &        'Zone viewfactor selection','v',ier)
            if(inpic.gt.0)then
              DO 164 IC=1,inpic
                ICOMP=ivals(IC)
                if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                  WRITE(LVIEW(icomp),'(A,A4)')
     &              zname(icomp)(1:lnblnk(zname(icomp))),'.vwf'
                else
                  WRITE(LVIEW(icomp),'(3A,A4)') 
     &              zonepth(1:lnblnk(zonepth)),'/',
     &              zname(icomp)(1:lnblnk(zname(icomp))),'.vwf'
                endif
                silent=.true.
                if(iw.eq.1)then
                  call EDMRT(ITRC,ITRU,IUF,ICOMP,silent,'a',IER)
                elseif(iw.eq.2)then
                  call EDMRT(ITRC,ITRU,IUF,ICOMP,silent,'v',IER)
                endif
 164          continue
              call usrmsg(
     &        ' updating model to add viewfactor files...',
     &        ' ','-')
              CALL EMKCFG('-',IER)
              call usrmsg(
     &        ' updating model to add viewfactor files...done.',
     &        ' ','-')
            endif
          elseif(iw.eq.3)then
            inpic=0
            call askmultizone(inpic,ivals,
     &        ' Select zones for viewfactors to dereference?',
     &        'Zone viewfactor dereference','v',ier)
            if(inpic.gt.0)then
              DO 163 IC=1,inpic
                ICOMP=ivals(IC)
                LVIEW(icomp)=' '
                IVF(ICOMP)=0
 163          continue
              call usrmsg(
     &        ' updating model to ignore viewfactor file...',
     &        ' ','-')
              CALL EMKCFG('-',IER)
              call usrmsg(
     &        ' updating model to ignore viewfactor file...done.',
     &        ' ','-')
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
      elseif(INO.EQ.13)THEN

C Casual gain control.
        if(browse)then
          call edisp(itru,' ')
          call edisp(itru,'You are currently in browse mode. You can')
          call edisp(itru,'look at the zone casual gain controls')
          call edisp(itru,'but DO NOT attempt to modify them!')
          call edisp(itru,' ')
        endif
 252    IC=-1
        izdef=0
        call askzone(ic,izdef,' Select zone for casual gain control ?',
     &     'Zone casual gain ctl','g','Zone index for cgc.',ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        IUF=IFIL+1
        if(ICGC(IC).eq.1)then

C If browsing do not bother to confirm file name, present menu.
          if(browse)then
            IUF=IFIL+1
            call EDCASCTL(ITRC,IC,IER)
            goto 252
          endif
          H(1)='This menu allows to dereference or edit zone casual'
          H(2)='control file. This facility supports control of'
          H(3)='zone casual gains based on the internal or external'
          H(4)='photocell illuminance. Different levels of calculation'
          H(5)='of photocell illuminance and control algorithms are'
          H(6)='supported.'
          H(7)=' '
          H(8)='If you want to ignore casual gain controls in this'
          H(8)='zone choose dereference '
          CALL EASKABC(' Casual gain control options:',' ','edit',
     &      'dereference','continue',irpt,6)
          if(irpt.eq.1)then
            IUF=IFIL+1
            call EDCASCTL(ITRC,IC,IER)
            if(ier.eq.0)then
              call edisp(iuout,' Casual gain control definitions ok.')
            endif
          elseif(irpt.eq.2)then
            ICGC(IC)=0
            LCGCIN(IC)='UNKNOWN'
            call usrmsg(
     &        ' Dereferencing casual gain control file...',' ','-')
            CALL EMKCFG('-',IER)
            call usrmsg(
     &        ' Dereferencing casual gain control file...done.',' ','-')
          else
            goto 3
          endif
        else
          if(browse)then
            call usrmsg('No casual gain defined for this zone.',
     &        'In browse mode so nothing to do.','P')
            goto 252
          endif
          H(1)='This menu allows to create/edit zone casual control'
          H(2)='file. The casual gain control file allows to control'
          H(3)='zone casual gains based on the internal or external'
          H(4)='photocell illuminance. Different levels of calculation'
          H(5)='of photocell illuminance and control algorithms are'
          H(6)='supported.'
          CALL EASKABC(' Casual gain control options:',' ',
     &      'create','dereference','continue',irpt,6)
          if(irpt.eq.1)then
            IUF=IFIL+1
            call EDCASCTL(ITRC,IC,IER)
            if(ier.eq.0)then
              call edisp(iuout,' Casual gain control definitions ok.')
            endif
          elseif(irpt.eq.2)then
            ICGC(IC)=0
            LCGCIN(IC)='UNKNOWN'
            call usrmsg(
     &        ' Dereferencing casual gain control file...',' ','-')
            CALL EMKCFG('-',IER)
            call usrmsg(
     &        ' Dereferencing casual gain control file...done.',' ','-')
          endif
        endif

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 252
      elseif(INO.EQ.14)THEN

C CFD.
        H(1)= 'This facility will assist you to compose a file'
        H(2)= 'characterizing the geometry and calculation parameters'
        H(3)= 'of a CFD domain.'
        H(4)= ' '
        H(5)= 'You should refer to Section 5.9 - Zone CFD Domain File'
        H(6)= 'in: Data Model Summary ESP-r Version 9 Series,'
        H(7)= 'ESRU Report TR96/2, which forms part of the ESP-r'
        H(8)= 'distribution (/usr/esru/esp-r/manual/DATA_MODEL).'
        H(9)= ' '
        H(10)='Note that CFD domains are currently restricted to'
        H(11)='cuboidal zones.'
        H(12)=' '
        H(13)='If you wish to dereference the CFD domain, do nothing'
        H(14)='in the editing facity and when exiting do not include'
        H(15)='it in the model. '
        CALL PHELPD('CFD domain + parameters',15,'-',0,0,IER)

        IC=-1
        izdef=0
        call askzone(ic,izdef,' Select zone for CFD focus:',
     &     'Zone CDF domain','d','Zone index for cfd.',ier)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        CALL EGOMIN(IFIL+1,LGEOM(IC),IC,1,ITRC,iuout,IER)

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
      elseif(INO.EQ.15)THEN

C Adaptive gridding module.
        IC=-1
        CALL EASKGEOF('Select a zone for adaptive gridding:',CFGOK,IC,
     &    '-',IER)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 3
        write(ZN,'(A)') zname(IC)

        IUF=IFIL+1

C If browsing do not save files but allow grd to be started.
        if(browse)then
          call edisp(itru,' ')
          call edisp(itru,'You are currently in browse mode.')
          call edisp(itru,'The adaptive gridding module will start')
          call edisp(itru,'but DO NOT attempt to save results!')
          call edisp(itru,' ')
          goto 257
        endif

C Offer creation of moisture transport data file or dereference.
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(SFIL,'(A,A4)')zname(IC)(1:lnblnk(zname(IC))),'.mst'
        else
          WRITE(SFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnblnk(zname(IC))),'.mst'
        endif
        DSFIL = SFIL
        H(1)='The moisture file holds moisture transport data'
        H(2)='for nodes. << more text to be added here >>'
        write(outs,'(3a)')' Moisture transport file for ',
     &    zname(IC)(1:lnblnk(zname(IC))),' ?'
        clkok=.false.
        if(.NOT.MSTRZN(IC))then
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
          WRITE(DSFIL,'(A,A4)')zname(IC)(1:lnblnk(zname(IC))),'.cvs'
        else
          WRITE(DSFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnblnk(zname(IC))),'.cvs'
        endif
        SFIL = L3DCVS(IC)
        H(1)='The 3D control volumes file defines...'
        H(2)='<< more text to be added here >>'
        CALL EASKS(SFIL,' 3D control volumes file',
     &    ' ',72,DSFIL,'control volumes file',IER,3)
        if(SFIL(1:2).ne.'  ') L3DCVS(IC)=SFIL

        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DSFIL,'(A,A4)')zname(IC)(1:lnblnk(zname(IC))),'.cnc'
        else
          WRITE(DSFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnblnk(zname(IC))),'.cnc'
        endif
        SFIL = L3DCNC(IC)
        H(1)='The 3D control volumes connections file defines...'
        H(2)='<< more text to be added here >>'
        CALL EASKS(SFIL,' 3D control volumes connections file',
     &    ' ',72,DSFIL,'control volumes connections file',IER,3)
        if(SFIL(1:2).ne.'  ') L3DCNC(IC)=SFIL

        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DSFIL,'(A,A4)')zname(IC)(1:lnblnk(zname(IC))),'.ndc'
        else
          WRITE(DSFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnblnk(zname(IC))),'.ndc'
        endif
        SFIL = L3DNDC(IC)
        H(1)='The 3D nodes coordinates file defines...'
        H(2)='<< more text to be added here >>'
        CALL EASKS(SFIL,' 3D nodes coordinates file',
     &    ' ',72,DSFIL,'3D nodes coordinates file',IER,3)
        if(SFIL(1:2).ne.'  ') L3DNDC(IC)=SFIL

        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(DSFIL,'(A,A4)')zname(IC)(1:lnblnk(zname(IC))),'.3dt'
        else
          WRITE(DSFIL,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      zname(IC)(1:lnblnk(zname(IC))),'.3dt'
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
      elseif(INO.EQ.18)THEN

C Call embedded renewables.
        call EDSPLIST('EG',IDV,IER)

      elseif(INO.EQ.19)THEN

C Call active materials menu.
        call EDSPLIST('EM',IDV,IER)
      elseif(INO.EQ.20)THEN

C Call advanced (bidirectional) optics.
        call edbioptics()
      ELSE
        INO=-1
        GOTO 3
      ENDIF
      INO=-2
      GOTO 3

      END

C ************* NEWPRB 
C NEWPRB specifies a new model via existing or new configuration file
C and the various CAD and native definition facilities. Confirm=true
C request confirmation of configuration file name, ckpath=true finds
C the path to it (should be false for CAD use as the file and its
C location is already known).
      SUBROUTINE NEWPRB(ITRC,confirm,ckpath,IER)
#include "building.h"

      common/FILEP/IFIL
      common/pophelp/h(60)
      common/PRODB/LPRFDB,IPRODB
      COMMON/GTFIL/GTGEOM
      COMMON/GT/GTNAME
      common/cctlnm/ctldoc,lctlf
      common/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/RAY3/MODIFY,MODLEN,MODBND
      common/RAY7/ZXMN(MCOM),ZYMN(MCOM),ZZMN(MCOM),ZXMX(MCOM),
     &            ZYMX(MCOM),ZZMX(MCOM),ZBFLG(MCOM)
      common/C1/NCOMP,NCON
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path
      common/rcmd/LCMDFL
      common/user/browse
      common/deflt4/dinstpath

C Defaults.
      common/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

      common/FOPENED/CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      logical XST,OK,DOK,CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      logical MODIFY,MODLEN,MODBND,browse,confirm,ckpath
      logical unixok,prob

      dimension nwlistf(50),ivlist(50)
      character*72 listf(50),listfc(50)
      character*70 listfc70(50)
      character*32 WORDS(12)

      character LPRFDB*72,path*72,LCMDFL*144,LTMP*144,GTGEOM*72
      character MODE*4,LCFGF*72,H*72,cfgroot*24,fs*1,outs*124
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      character OUTSTR*124,ctldoc*248,LCTLF*72,t72*72,t144*144,ETEXT*82
      character*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      character sfile*144,odir*72,subpath*72,action*3,GTNAME*15
      CHARACTER dinstpath*48,dirpath*48

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      t72='  '
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
        H(2)='of the building/ plant to be simulated, including the'
        H(3)='names of all of the files required. If the name given'
        H(4)='matches an existing file then that model will be'
        H(5)='loaded by the Project Manager and made available for'
        H(6)='browsing/ editing.  If the file does not exist then a'
        H(7)='new one will be created and loaded with a set of'
        H(8)='defaults which can subsequently be modified. The'
        H(9)='current default model is'
        H(10)=' '
        write(H(11),'(1x,60a)') DFCFG(1:60)
        H(12)=' '
        H(13)='which is available for browsing.  Note that before'
        H(14)='this model can be edited, it must be copied to'
        H(15)='your user area to give you write permission. Lists '
        H(16)='of other on-line example models are also available'
        H(17)='through the `exemplars` option of the Main Menu.'
        if(LCMDFL(1:4).eq.'UNKN'.or.LCMDFL(1:4).eq.'unkn')then
          LTMP=' '
        else
          LTMP=LCMDFL
        endif

C Call EASKF depending on the current file name length.
C Use ifdefs because the X11 version will be returning only the
C name of the file.
        llt=lnblnk(ltmp)
#ifdef X11
        if(llt.lt.96)then
          CALL EASKF(LTMP,' Model configuration file?',' ',96,DFCFG,
     &    'config file name',IER,17)
        elseif(llt.ge.96.and.llt.lt.124)then
          CALL EASKF(LTMP,' Model configuration file?',' ',124,DFCFG,
     &    'config file name',IER,17)
        elseif(llt.ge.124.and.llt.le.144)then
          CALL EASKF(LTMP,' Model configuration file?',' ',144,DFCFG,
     &    'config file name',IER,17)
        endif
#else

C Note for GTK, LTMP will be returned with the absolute path to the folder
C even if prj started up in the cfg folder. There is subsequent
C logic which determines if we actually are browsing.
        CALL EASKF(LTMP,' Model configuration file?',' ',144,DFCFG,
     &    'config file name',IER,17)
#endif

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
        write(dirpath,'(2a)') dinstpath(1:lnblnk(dinstpath)),
     &    '/training'
        ldirpath=lnblnk(dirpath)
        call fdroot(LCMDFL,path,LCFGF)

C Debug << check out this on a native windows machine >>
C        write(6,*) ' after fdroot path is ',path(1:lnblnk(path))
C        write(6,*) ' lcfgf is ',lcfgf(1:lnblnk(lcfgf))
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
C          write(6,*) 'a iaccess is ',iaccess,icerr,ifold,path
          if(iaccess.eq.1)then

C If iaccedss is non-zero then we do not have permission to write there
C and so we treat that model as if browsing.
            call tstamp('>','PRJ: on browse mode')
            browse=.true.
          else
            browse=.false.
          endif

#ifndef X11

C For GTK versions, find odir (where prj was started) and compare with the
C current path (both lnpath and lnpath-1 lengths) if there
C is a match then even though there was a full explicit path
C we can recognise that we started in a cfg folder. 
          call usrdir(odir)
          lnpath=lnblnk(path)

C Debug.
C          write(6,*) ' GTK path is ',path(1:lnblnk(path))
C          write(6,*) ' GTK odir is ',odir(1:lnblnk(odir))
          if(path(1:lnpath).eq.odir(1:lnpath))then
            browse=.false.
          elseif(path(1:lnpath-1).eq.odir(1:lnpath-1))then
            browse=.false.
          else
            call tstamp('>','PRJ: on browse mode')
            browse=.true.
          endif
#endif
        endif
      else

C Called without request to check paths.
        ifold=-1
        call ckaccess(ifold,iaccess,icerr,path)

C Debug.
C        write(6,*) 'b iaccess is ',iaccess,icerr,ifold,path
        if(iaccess.eq.1)then
          call tstamp('>','PRJ: on browse mode')
          browse=.true.
        else
          browse=.false.
        endif
      endif

C Debug.
      write(6,*) 'NEWPRB browse status is ',browse

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
          write(cfgroot,'(a)') LCMDFL(1:lnblnk(LCMDFL))
          subpath=' '
          call usrdir(odir)
          write(subpath,'(3a)') odir(1:lnblnk(odir)),fs,
     &      t144(1:lnblnk(t144))
#ifdef OSX
C In OSX slightly different logic. If path ls ./ then standard logic
C works. If path begins with /usr/esru then no need to prepend odir.
          if(path(1:2).eq.'./')then
            continue
          elseif(path(1:9).eq.'/usr/esru')then
            subpath=' '
            write(subpath,'(a)') path(1:lnblnk(path))
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
            write(subpath,'(a)') path(1:lnblnk(path))
          elseif(path(4:7).eq.'user')then
            subpath=' '
            write(subpath,'(a)') path(1:lnblnk(path))
          else
            continue
          endif
#endif
          action='cfg'
          write(outs,*) 'models in ',subpath
          call clearfilelist(listf,50)
          call getfilelist(subpath,action,listf,nwlistf,nlistf)
          call copyfilelist(listf,nwlistf,listfc,50,maxw)
          call printfilelist(outs,'p',listfc,50,nlistf)

C Try to use information gathered from file scan.
          if(nlistf.gt.0)then
            h(1)='The following files match the search criteria.'
            h(2)='Select one to use or, if none selected, it will'
            h(3)='revert to prior value. '
            ix=1
            do 76 ij=1,nlistf
              listfc70(ij)=' '
              write(listfc70(ij),'(a)') listfc(ij)(1:nwlistf(ij))
  76        continue
            CALL EPICKS(IX,ivlist,outs,' ',
     &        maxw,nlistf,listfc70,'Available files',IER,3)
            if(ix.eq.1)then
              write(t144,'(3a)')LCMDFL(1:lnblnk(LCMDFL)),fs,'cfg'
              write(sfile,'(3a)') t144(1:lnblnk(t144)),fs,
     &          listfc(ivlist(ix))(1:nwlistf(ivlist(ix)))

C Debug.
              write(6,*) 'sfile is ',sfile
              call fdroot(sfile,path,LCFGF)
              CALL ERPFREE(IFCFG,ISTAT)
              call FINDFIL(LCFGF,XST)
            endif
          endif
        endif
      else
        if(lnblnk(LCMDFL).le.72)then
          write(LCFGF,'(a)') LCMDFL(1:lnblnk(LCMDFL))
        else
          write(LCFGF,'(a)') LCMDFL(1:72)
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
            write(cfgroot,'(a)') WORDS(IW)(1:lwcfgl-1)
          else
            write(cfgroot,'(a)') WORDS(IW)(1:lwcfgr)
            write(LTMP,'(a,a)') LTMP(1:lcfgr),'.cfg'
          endif
        elseif(lwcfgr.le.4)then
          write(cfgroot,'(a)') WORDS(IW)(1:lwcfgr)
          write(LTMP,'(a,a)') LTMP(1:lcfgr),'.cfg'
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
              write(cfgroot,'(a)') WORDS(IW)(1:lwcfgl-1)
              write(t72,'(a,a1,a,a)') 'cfg',fs,LTMP(1:lcfgl-1),'.cfg'
            else
              write(cfgroot,'(a)') WORDS(IW)(1:lwcfgr)
              write(t72,'(a,a1,a,a)') 'cfg',fs,LTMP(1:lcfgr),'.cfg'
            endif
          elseif(lwcfgr.le.4)then
            write(cfgroot,'(a)') WORDS(IW)(1:lwcfgr)
            write(t72,'(a,a1,a,a)') 'cfg',fs,LTMP(1:lcfgr),'.cfg'
          endif
          if((t72(1:2).ne.'  ').and.(t72(1:4).ne.'UNKN'))then
            XST=.false.
            INQUIRE (FILE=t72,EXIST=XST)
            if(XST)then
              call fdroot(t72,path,LCFGF)
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
                  write(cfgroot,'(a)') WORDS(IW)(1:lwcfgl-1)
                  write(t72,'(a,a1,a,a1,a,a)') LTMP(1:lcfgl-1),fs,
     &              'cfg',fs,LTMP(1:lcfgl-1),'.cfg'
                else
                  write(cfgroot,'(a)') WORDS(IW)(1:lwcfgr)
                  write(t72,'(a,a1,a,a1,a,a)') LTMP(1:lcfgr),fs,'cfg',
     &              fs,LTMP(1:lcfgr),'.cfg'
                endif
              elseif(lwcfgr.le.4)then
                write(cfgroot,'(a)') WORDS(IW)(1:lwcfgr)
                write(t72,'(a,a1,a,a1,a,a)') LTMP(1:lcfgr),fs,'cfg',fs,
     &            LTMP(1:lcfgr),'.cfg'
              endif
              if((t72(1:2).ne.'  ').and.(t72(1:4).ne.'UNKN'))then
                XST=.false.
                INQUIRE (FILE=t72,EXIST=XST)
                if(XST)then
                  call fdroot(t72,path,LCFGF)
                  goto 444
                else

C Check to see adding a cfg folder and cfgroot to LCFGF works.
                  LTMP=LCFGF
                  call GETTOKENS(LTMP,IW,WORDS)
                  lwcfgr=lnblnk(WORDS(IW))
                  lwcfgl=lwcfgr-3
                  if(lwcfgr.gt.4)then
                    if(WORDS(IW)(lwcfgl:lwcfgr).eq.'.cfg')then
                      write(cfgroot,'(a)') WORDS(IW)(1:lwcfgl-1)
                      write(t72,'(a,a1,a,a1,a,a)') LTMP(1:lcfgl-1),fs,
     &                  'cfg',fs,cfgroot(1:lnblnk(cfgroot)),'.cfg'
                    else
                      write(cfgroot,'(a)') WORDS(IW)(1:lwcfgr)
                      write(t72,'(a,a1,a,a1,a,a)') LTMP(1:lcfgr),fs,
     &                  'cfg',fs,cfgroot(1:lnblnk(cfgroot)),'.cfg'
                    endif
                  elseif(lwcfgr.le.4)then
                    write(cfgroot,'(a)') WORDS(IW)(1:lwcfgr)
                    write(t72,'(a,a1,a,a1,a,a)') LTMP(1:lcfgr),fs,
     &                'cfg',fs,cfgroot(1:lnblnk(cfgroot)),'.cfg'
                  endif
                  if((t72(1:2).ne.'  ').and.(t72(1:4).ne.'UNKN'))then
                    XST=.false.
                    INQUIRE (FILE=t72,EXIST=XST)
                    if(XST)then
                      call fdroot(t72,path,LCFGF)
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
          CALL EASKABC(' When reading in the model description,',
     &      ' do it:','silently','with synopsis','report composition',
     &      IW,0)
          itrc=IW-1
        endif
        MODE='ALL '
        CALL ERSYS(LCFGF,IFCFG,IAPROB,MODE,itrc,IER)
        IF(IER.NE.0)THEN
          dok=.true.
          h(1)='There was an unspecified problem, review any warnings'
          h(2)='to see if the problem is one you can fix. Also double'
          h(3)='check that the file was of the correct type. '
          h(4)=' '
          h(5)='If you say no then it may be possible to work with'
          h(6)='the model, but it may be incomplete or inconsistent'
          h(7)='and should be treated as at=risk. '
          CALL ASKOK(' While reading the model configuration file',
     &               ' a problem was detected! Try again?',OK,dok,7)
          if(OK)then
            confirm=.true.
            goto 289
          endif
        ENDIF
        NZONES=NCOMP
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
          call updwire(ncomp)
          call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                             iicfgz,iicfgn,iicfgc,iicfgdfn)
          call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                           iicfgz,iicfgn,iicfgc,iicfgdfn)
          WRITE(ETEXT,'(2A)')'Project: ',LSNAM(1:lnblnk(LSNAM))
          CALL viewtext(ETEXT,1,1,1)
        endif

C Open core databases associated with the exemplar or new model.
        if(MLDBOK.and.CONDBOK.and.OPTKOK)then
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
            h(3)='the problem. Also check to see that the file is '
            h(4)='of the correct type. '
            h(5)=' '
            h(6)='If you say no the model description may be incomplete'
            h(7)='so be careful. Consider dereferencing the control '
            h(8)='file. '
            CALL ASKOK(' While reading the control a',
     &        ' problem was detected!  Try again?',OK,dok,8)
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
          ZBFLG(ICU)=0
   77   CONTINUE
        IF(IER.NE.0)THEN
          dok=.true.
          h(1)='If this point has been reached there may have '
          h(2)='been a problem with displaying project images. '
          h(3)='or with another portion of the model. Sorry to '
          h(4)='be vague... Specific errors should have been '
          h(5)='seen earlier in the sequence of events. '
          CALL ASKOK('There was a residual error reported as the',
     &      'model was read!  Try again?',OK,dok,5)
          if(OK)then
            confirm=.true.
            goto 289
          endif
        ENDIF

C Scan the model topology and report on any problems encountered.
        call ckcurmatch(prob,iprob,ier)
        if(prob)then
          call tstamp('>','PRJ: found topology incinsistent')
          if(IPROB.gt.10)then
            write(outs,*) IPROB,' inconsistencies found!'
            call edisp(iuout,outs)
            call usrmsg(
     &      'The model topology (what surface is connectect to what',
     &      'boundary condition) is dangerously inconsistent...','W')
          else
            call usrmsg(
     &      'Topology list inconsistent. Either clear current',
     &      'topology and/or use topology tool to resolve this.','W')
          endif
        endif

        CFGOK=.TRUE.
        MODIFY=.TRUE.
        MODLEN=.TRUE.
      ELSE
        h(1)='If the name supplied cannot be found you can: '
        h(2)=' a) re-edit the name,'
        h(3)=' b) create a new configuration file using the name,'
        h(4)=' c) delay deciding via cancel.'
        WRITE(OUTSTR,'(A,2X,A)')' Could not find',LCFGF
        CALL EASKABC(OUTSTR,' Options:',
     &    'respecify','new configuration file','cancel',IW,4)
        if(IW.eq.1)then
          confirm=.true.
          goto 289
        elseif(IW.eq.2)then
          continue
        elseif(IW.eq.3)then
          ier=-1
          return
        endif

C Clear any previous model in memory.
        call clrprb

        call pregist('i',ier)
        IF(IER.LT.0)RETURN

        CFGOK=.TRUE.

C Open default databases (user can alter later).
        IF(MMOD.EQ.8)call updwire(ncomp)
        call opendb(ier)
        if(ier.ne.0)then
          call usrmsg(
     &      'Possible problems with one or more of the construction',
     &      'and optical databases, please check!','W')
          ier = 0
        endif
      ENDIF
      return
      end

C ***** visualz
C visualz drives visualisation process - hidden line or raytracing.
C << this duplicates opts within the model export facility >>
C << it would be clearer if all model I/O happened there >>
      subroutine visualz(act,ier)
      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      common/pophelp/h(60)
      common/C21/IFCFG,cfgroot,LCFGF
      common/radcfg/LRADCF
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/appw/iappw,iappx,iappy
      common/user/browse
      logical concat,OK,DOK,browse,XST,deref,unixok
      character lradcf*72,dradcf*72,LCFGF*72,OUTSTR*124
      character LFIL*72,dvfil*72,cfgroot*24,ltmp*72
      character doit*124,tmode*8,tfile*72,H*72,act*1
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24,fs*1
      character docpth*24,tmppth*24,dbspth*24,longtfile*124

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
        H(1)='The hidden line perspective program requires an'
        H(2)='input file (which is created from the current model.'
        CALL EASKS(LFIL,' File name for image data ? ',
     &    ' ',72,dvfil,'VIEWER input file',IER,2)
        call addpath(LCFGF,longtfile,concat)
        write(doit,'(4a)') 'ecnv -obs -if esp -in ',
     &    longtfile(1:lnblnk(longtfile)),' -of viewer -out ',
     &    LFIL(1:lnblnk(LFIL))
        call usrmsg('starting conversion via',doit,'-')
        call runit(doit,'-')

        dok=.true.
        h(1)='If the export of vector information was accomplished'
        h(2)='without error you should be able to start up the module'
        h(3)='that displays hidden line views. '
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
        if(.NOT.browse)then

C Set rcf name if unknown and set default name. If path for radiance
C files is ./ then update this to ../rad and create a folder.
          if(lradcf(1:7).eq.'UNKNOWN')then
            write(lradcf,'(a,a)') cfgroot(1:lnblnk(cfgroot)),'.rcf'
          else
            if(radpth(1:3).eq.'./ '.or.radpth(1:3).eq.'.\\ ')then
              H(1)='Creating a new folder for radiance model: ../rad'
              H(2)='The curren model includes a radiance model in the '
              H(3)='configuration folder. If you want to use this'
              H(4)='old radiance model, you should move the files to'
              H(5)='../rad and update the scenes and rif files.'
              H(6)='The current scenes file is:'
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
 319      H(1)='A Radiance scene file specifies the composition'
          H(2)='of the model for Radiance as well as the purpose'
          H(3)='of various scenes (inside view/daylight factors).'
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
          write(doit,'(3a,3i4,3a)') 'e2r -mode ',tmode,
     &      ' -s ',iappw,iappx+35,iappy+40,' -file ',
     &      longtfile(1:lnblnk(longtfile)),' &'
        else
          write(doit,'(5a)') 'e2r -mode ',tmode,
     &      ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),' &'
        endif
        call usrmsg('starting Radiance desktop via',doit,'-')
        call runit(doit,tmode)
        H(1)=' CLICK here when you have finished with the'
        H(2)=' colour rendering module.'
        CALL PHELPD('e2r finished menu',2,'-',0,0,IER)

C Check if radiance scene file exists. If not or zero length or not correct 
C file type then dereference it from the cfg file.
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

C << place to associate images (when logic worked out to do that) >>
          endif
        endif
      endif
      return
      end


