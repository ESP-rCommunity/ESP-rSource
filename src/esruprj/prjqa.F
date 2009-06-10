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

C Project manager: QA and updating facilities comprising
C QA functions included in this file:
C   prjqa:   writes out a QA report (to screen or file) for the current model.
C   mupdate: checks current model to see if it should be updated.

C ************ prjqa
C Prjqa writes out a QA report (to screen or file) for
C the current model.
      subroutine prjqa(ier)
#include "building.h"

C geometry.h provides commons G0/G2/G7/prec2/precz/c20.
#include "geometry.h"
#include "net_flow.h"
#include "control.h"
#include "tdf2.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure databases)
C LOPTDB,IOPTDB (for optical database)
C LPCDB/IPCDB (for plant template database)
C LPRFDB,IPRODB (for event profile database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
C ICLIM,LCLIM (for climate file)
#include "espriou.h"
C espriou.h provides currentfile.
#include "sbem.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      common/C4/XLAT,XLON
      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM
      CHARACTER*72 SNFNAM
      common/C5R/SKYR,GRDR,BLDR
      common/C6/INDCFG
      common/C14/UGRDTP(12,MGRDP),NGRDP
      common/C21/IFCFG,cfgroot,LCFGF

C IFPNF is the unit for the plant network file.
C LPNF (72 char) plant network file name.
      COMMON/C23/IFPNF,LPNF
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)

      character DESC*48
      integer NMLC,IPR,LAYERS
      real DTHK,DRAIR
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      common/RAY3/MODIFY,MODLEN,MODBND
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/PRECT2/TMCT2(MCOM,MTMC,5,MBP),TMCA2(MCOM,MTMC,ME,5,MBP),
     &              TVTR2(MCOM,MTMC,MBP)
      COMMON/PRECT3/NTMC,NGLAZ(MTMC)
      common/PRECT4/TOPTIC(MCOM,MTMC)
      COMMON/TMCB1/IBCMT(MCOM,MTMC)
      COMMON/TMCB2/NBCTMC(MCOM,MTMC),IBCST(MCOM,MBP,MTMC),
     &             IBCFT(MCOM,MBP,MTMC),IBCSUR(MCOM,MTMC)
      COMMON/TMCB3/NBCTT(MCOM,MBP,MTMC),BACTPT(MCOM,MBP,MTMC)
      common/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      common/cctlnm/ctldoc,lctlf
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/MFLOW2/NDNAM(0:MNOD)

C AIM-2.
      common/aim2def/iAIM2,LAIM2

      common/ctlnm/znctldoc,plctldoc,flctldoc,elctldoc,glctldoc,
     &             opticdoc

C Variables for weekdays, and weekends.
C Assume: Mon=1, Tue=2, Wed=3, Thu=4, Fri=5, Sat=6, Sun=7
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/GTFIL/GTGEOM
      COMMON/GT/GTNAME
      common/PCONV/ipconv,pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
      common/CONVEM/phtco2,phtnox,phtsox,pclco2,pclnox,pclsox,
     &              pltco2,pltnox,pltsox,pfnco2,pfnnox,pfnsox,
     &              pspco2,pspnox,pspsox,phwco2,phwnox,phwsox
      COMMON/CLMDT1/CLMLOC
      common/IPVF/lipvdatf
      common/IPVSEA/nipvassmt,nipvdispjd,ipvastjd(MIPVA),
     &              ipvafnjd(MIPVA),ipvdispjd(10)
      common/CFGV/icfgv

C ipvtitl (char 40) is the title that shows up in reports
C ipvvers (char 40) is the version of the IPV e.g. 'case with mass walls'
C ipvsynop (char 248) documents the integrated performance view
C ipvsimu (char 6) is a short tag describing the assessments to be run
C   for example 'icautf' is an autumn fortnight and 'i3s' is a three
C   season assessment with all days included.
      character ipvtitl*40,ipvvers*40,ipvsynop*248,ipvsimu*6
      common/IPVA/ipvtitl,ipvvers,ipvsynop,ipvsimu

C itdflg=0 none, 1 older file, 2 asci v2, 3 asci v3, -2 bin v2, -3 bin v3
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFLG/ITEMSTD
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)

      logical found,XST,apply,MODIFY,MODLEN,MODBND,duplicate
      logical bndrysxc,context,bound

      CHARACTER oprdesc*248,ventdesc*248,ctlstr*24
      COMMON/P1/oprdesc(MCOM),ventdesc(MCOM),ctlstr(MCOM,MDTY)

C Demands notes and file name.
      COMMON/BL1/dmdsdesc,bdmds

C Project log file.
      common/LOG/LPRJLG

C External text editor.
      common/texted/tedlbl,teditor

C External figure generator.
      common/figtool/figlbl,figexe

C External graphing tool.
      common/graphtool/grflbl,grfexe

C External folder browser.
      common/folderbr/foldlbl,foldexe

C A set of possible image formats (see config file) and
C matching executables. For normal image viewing take the first option.
      common/showimg/imgtyp,fmttag(5),fmtexe(5)
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C iSlr_half_hr_flg value = 0 :hour-centered; =1 : half-hour centered.
      common/CWEC_SOL/iSlr_half_hr_flg

      integer iSlr_half_hr_flg

      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Remember if each zone is fully attributed.
      logical attribok(MCOM)
      logical allattribok,changedit,ok,dok,concat,unixok,showother
      dimension IVALS(MCOM),areamlc(MCOM)
      dimension uavgtran(MCOM),uavwall(MCOM),uavslproof(MCOM)
      dimension uavfltroof(MCOM),uavgsky(MCOM),areatran(MCOM)
      dimension areawall(MCOM),areaslproof(MCOM),areafltroof(MCOM)
      dimension areaskylt(MCOM),tranper(MCOM),skyper(MCOM)
      dimension wallper(MCOM),slproofper(MCOM),flatroofper(MCOM)
      dimension exposed(MCOM),vexposed(MCOM)
      dimension items(25)

      character*72 LTMP
      character*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS,GTGEOM
      character GTNAME*15,CLMLOC*30,lipvdatf*72
      character LCFGF*72,H*72,lco*72,lcot*72
      character longtfile*144,longtfiledos*144
      character LFIL*96,llitem*144    ! for editing the QA report file.
      character DESCRC*25
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      character cfgroot*24,ZN*12,SN*12
      character aok*17,CTLDOC*248,LCTLF*72,items*33
      character xfile*144,tg*1,delim*1,head*21,mlcmatch*12
      character outs*124,outs248*248
      character znctldoc*248,plctldoc*248,flctldoc*248,elctldoc*248
      character glctldoc*248,opticdoc*248
      character dstmp*24,mlcname*12
      CHARACTER LAPROB*72,LPNF*72
      CHARACTER NDNAM*12,TOPTIC*12,msg*28,doit*248
      character t16a*16,t16b*16,t16c*16
      CHARACTER dmdsdesc*248,bdmds*72
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15
      character figlbl*20,figexe*48
      character grflbl*20,grfexe*48,foldlbl*20,foldexe*48
      character LPRJLG*72,tmode*8,fmttag*4,fmtexe*20
      character tedlbl*20,teditor*20
      character uname*24,pwd*40
      character LAIM2*72
      character ipvaction*3  ! signal that external ipv file
      character MLCSYM*12
      character MLCN*12,OTF*4,OPT*12 ! for use with prsmlcname
      
C Topic variables: value of zero = none, one = compact, two = verbose
C Initial assumption is verbose for each topic that is included
C in the current model. A -1 means not applicable
      integer siteinfo,databaseinfo,contextinfo,controlinfo,netinfo
      integer plantinfo,geominfo,schedinfo,zoneextrainfo,fileinfo
      integer destination

      integer iercl  ! error state returned for scanning climate list.

      character*10 wkd1, wkd2
      logical newgeo  ! to use for testing if new/old geometry file.
      logical nameok  ! to use for testing if duplicate names in zone,

C Check if Unix-based or DOS based.
      call isunix(unixok)

      changedit=.false.
      newgeo=.false.  ! assume older format geometry.
      IUF=IFIL+2
      siteinfo=2
      databaseinfo=1
      contextinfo=2
      ICTLF=IUF
      CALL ERPFREE(ICTLF,ISTAT)
      call FINDFIL(LCTLF,XST)
      if(XST)then
        controlinfo=2
      else
        controlinfo=-1
      endif 
      if(IAIRN.ge.1)then
        netinfo=2
      else
        netinfo=-1
      endif
      if(indcfg.eq.2.or.indcfg.eq.3)then
        plantinfo=2
      else
        plantinfo=-1
      endif
      if(ncomp.gt.0)then
        geominfo=2
        schedinfo=2
        zoneextrainfo=2
      else
        geominfo=-1
        schedinfo=-1
        zoneextrainfo=-1
      endif

C Assume file names not included and destination to screen (zero)
C and itu set so QA report is written to screen.
      fileinfo=0
      destination=0
      itu = iuout

C Setup the selection menu.
    3 INO=-4
      IER=0
      write(items(1),'(a)')  '1 browse folders '
      write(items(2),'(2a)') '2 model log file ',LPRJLG(1:15)
      items(3) ='3 graphing tool               '
      items(4) ='4 diagram editing tool        '
      items(5) ='5 image viewer                '
      items(6) ='  -------------------------   '
      items(7) ='  QA report                   '
      if(siteinfo.eq.0)then
        items(8)='a site info >> none'
      elseif(siteinfo.eq.1)then
        items(8)='a site info >> compact'
      elseif(siteinfo.eq.2)then
        items(8)='a site info >> verbose'
      endif
      if(databaseinfo.eq.0)then
        items(9)='b databases >> none'
      elseif(databaseinfo.eq.1)then
        items(9)='b databases >> compact'
      elseif(databaseinfo.eq.2)then
        items(9)='b databases >> verbose'
      endif
      if(contextinfo.eq.0)then
        items(10)='c model context >> none'
      elseif(contextinfo.eq.1)then
        items(10)='c model context >> compact'
      elseif(contextinfo.eq.2)then
        items(10)='c model context >> verbose'
      endif
      if(controlinfo.eq.-1)then
        items(11)='d controls >> not applicable'
      elseif(controlinfo.eq.0)then
        items(11)='d controls >> none'
      elseif(controlinfo.eq.1)then
        items(11)='d controls >> compact'
      elseif(controlinfo.eq.2)then
        items(11)='d controls >> verbose'
      endif
      if(netinfo.eq.-1)then
        items(12)='e networks >> not applicable'
      elseif(netinfo.eq.0)then
        items(12)='e networks >> none'
      elseif(netinfo.eq.1)then
        items(12)='e networks >> compact'
      elseif(netinfo.eq.2)then
        items(12)='e networks >> verbose'
      endif
      if(plantinfo.eq.-1)then
        items(13)='f plant systems >> not applicable'
      elseif(plantinfo.eq.0)then
        items(13)='f plant systems >> none'
      elseif(plantinfo.eq.1)then
        items(13)='f plant systems >> compact'
      elseif(plantinfo.eq.2)then
        items(13)='f plant systems >> verbose'
      endif
      items(14)='g zone selection'
      if(geominfo.eq.-1)then
        items(15)='h geometry >> not applicable'
      elseif(geominfo.eq.0)then
        items(15)='h geometry >> none'
      elseif(geominfo.eq.1)then
        items(15)='h geometry >> compact'
      elseif(geominfo.eq.2)then
        items(15)='h geometry >> verbose'
      elseif(geominfo.eq.2)then
        items(15)='h geometry >> very verbose'
      endif
      if(schedinfo.eq.-1)then
        items(16)='i operations >> none'
      elseif(schedinfo.eq.0)then
        items(16)='i operations >> none'
      elseif(schedinfo.eq.1)then
        items(16)='i operations >> compact'
      elseif(schedinfo.eq.2)then
        items(16)='i operations >> verbose'
      endif
      if(zoneextrainfo.eq.-1)then
        items(17)='j zone extras >> none'
      elseif(zoneextrainfo.eq.0)then
        items(17)='j zone extras >> none'
      elseif(zoneextrainfo.eq.1)then
        items(17)='j zone extras >> compact'
      elseif(zoneextrainfo.eq.2)then
        items(17)='j zone extras >> verbose'
      endif
      if(geominfo.eq.1.or.geominfo.eq.2)then
        items(18)='k sort by surface attrbution'
      else
        items(18)='k n/a'
      endif
      if(fileinfo.eq.0)then
        items(19)='m file names >> none'
      elseif(fileinfo.eq.1)then
        items(19)='m file names >> included'
      endif
      if(destination.eq.0)then
        items(20)='> QA report to >> text feedback'
      elseif(destination.eq.1)then
        items(20)='> QA report to >> text file'
      endif
      items(21)  ='! generate QA report       '
      items(22)  ='* edit the QA report       '
      items(23)  ='  _________________________'
      items(24)  ='? help                     '
      items(25)  ='- exit menu                '

C Help for menu.
      H(1)='Normally reports are generated from a standard pattern'
      H(2)='and edited to fit the current project. The `notes` file'
      H(3)='is created during project registration and should be '
      H(4)='updated as the project evloves.'
      H(5)=' '
      H(6)='The graphing tool can be invoked after exporting'
      H(7)='performance data from the assessment tool.'
      H(8)=' '
      H(9)='If you wish to view bitmapped images oor capture '
      H(10)='images from the screen use option `h`. '
      H(11)=' '
      H(12)='The preview tool has a default assumption that the '
      H(13)='report uses the troff typesetting convention. '
      h(14)=' '
      h(15)='This menu also defines the contents of a QA report for'
      h(16)='the current model. '
      h(17)=' '
      h(18)='A number of topics can be selected for inclusion in the'
      h(19)='report and each topic can be reported in a compact or'
      h(20)='verbose format.'
      h(21)=' '
      h(22)='Topics which are not included in the current model will' 
      h(23)='show up as not applicable.'
      h(24)=' '
      h(25)='One use of this report is to track the progress of a'
      h(26)='model (comparing reports generated as the model evolves).'
      h(27)=' '
      h(28)='Another use of this report is to confirm that different'
      h(29)='different versions of a model have been correctly'
      h(30)='defined. '
      h(31)=' '
      h(32)='The primary use of this report is as an aide to check'
      h(33)='models and provide a text based synopsis of the model'
      h(34)='composition. '
      h(35)=' '
      h(36)='Once you generate a QA report file you can select the'
      h(37)='`edit QA report` option and an editor will be launched.'

      call emenu('Model reports',items,25,ino)

      if(ino.eq.25)then
        if(changedit)then
          continue
        endif
        return
      elseif(ino.eq.24)then
        CALL PHELPD('QA overview',37,'-',0,0,IER)
      elseif(ino.eq.1)then

C << need to sort out changing to main project folder >>
        if(foldexe(1:2).eq.'  ')then
          call usrmsg(
     &    'The folder browsing tool has not be defined. See your',
     &    'administrator about updating the esprc definition.','W')
        else
          write(doit,'(2a)') foldexe(1:lnblnk(foldexe)),' &'
          call usrmsg('Beginning folder view via',doit,'-')
          tmode='graph'
          call runit(doit,tmode)
        endif
      elseif(ino.eq.2)then

C Edit project documentation.
        if(CFGOK)then
  54      H(1)='The project log file contains descriptive text about'
          H(2)='the project.  Keep it up to date via a text editor.'
          if(LPRJLG(1:2).eq.'  '.or.LPRJLG(1:4).eq.'UNKN')then
            write(LPRJLG,'(a,a)') cfgroot(1:lnblnk(cfgroot)),'.log'
          endif
          ltmp=LPRJLG
          CALL EASKS(ltmp,
     &      'A log file (notes/documentation) for this project,',
     &      'Its name? ',72,'project.notes','log file',IER,2)
          if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
            LPRJLG=ltmp
            changedit=.true.
          else
            goto 54
          endif

C See if file exists, (it should have been created within pregist
C when the model was initially created.
          uname=' '
          call usrname(uname)
          call usrdir(pwd)
          call FINDFIL(LPRJLG,XST)
          IUNIT=IFIL+1
          if(XST)then
            CALL LISTAS(IUNIT,LPRJLG,IER)
            IF(IER.ne.0)THEN
              dok=.true.
              h(1)='When attempting to preview the log file a problem'
              h(2)='was reported so check the spelling and location or'
              h(3)='type of file before trying again. '
              CALL ASKOK(' Problem detected while trying to preview',
     &                   ' the log file. Try again?',OK,dok,3)
              IF(OK)GOTO 54
            ENDIF
          else
            dok=.true.
            h(1)='No file by that name or in that location was found.'
            h(2)='Check the spelling and location of the file name'
            h(3)='before trying again. '
            CALL ASKOK(' Problem detected while trying to open',
     &                 ' log file. Try again?',OK,dok,3)
            IF(OK)GOTO 54
          endif

C Allow user to edit the job notes file. Append to path if
C necessary.  If vi then spawn a new window to do the editing.
          dok=.true.
          h(1)='If you have a editor associated with ESP-r you can'
          h(2)='use it to edit this file.  '
          CALL ASKOK(' ',' Edit this file?',OK,dok,2)
          IF(OK)then
            if(unixok)then
              call addpath(LPRJLG,longtfile,concat)
            else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
              call addpath(LPRJLG,longtfile,concat)
              call cmdfiledos(longtfile,longtfiledos,ier)
              longtfile=' '
              longtfile=longtfiledos
            endif
            tmode='graph'
            if(teditor(1:2).eq.'  ')then
              call usrmsg(
     &      'The external word processor has not be defined. See your',
     &      'administrator about updating the esprc definition.','W')
            elseif(teditor(1:2).eq.'vi')then
              tmode='text'
            endif
            write(doit,'(a,2x,a,a)') teditor(1:lnblnk(teditor)),
     &        longtfile(1:lnblnk(longtfile)),' &'
            call usrmsg('Begining edit via',doit,'-')
            call runit(doit,tmode)
          endif
        endif
      elseif(ino.eq.3)then

C Execute graphing tool.
        if(grfexe(1:2).eq.'  ')then
          call usrmsg(
     &    'The extrn. data analysis tool has not be defined. See your',
     &    'administrator about updating the esprc definition.','W')
        else
          call tstamp('>','PRJ: start graphing tool')
          write(doit,'(2a)')grfexe(1:lnblnk(grfexe)),' &'
          call usrmsg('Beginning graphing tool via',doit,'-')
          call runit(doit,'graph')
        endif
      elseif(ino.eq.4)then

C Execute figure tool.
        if(figexe(1:2).eq.'  ')then
          call usrmsg(
     &    'The external diagram tool has not be defined. See your',
     &    'administrator about updating the esprc definition.','W')
        else
          call tstamp('>','PRJ: start xfig')
          write(doit,'(2a)')figexe(1:lnblnk(figexe)),' &'
          call usrmsg('Beginning figure tool via',doit,'-')
          call runit(doit,'graph')
        endif
      elseif(ino.eq.5)then

C Execute image viewer (get tool via prior scan of default file).
        if(fmtexe(1)(1:2).eq.'  ')then
          call usrmsg(
     &    'The external image tool has not be defined. See your',
     &    'administrator about updating the esprc definition.','W')
        else
          call tstamp('>','PRJ: start xv')
          write(doit,'(2a)')fmtexe(1)(1:lnblnk(fmtexe(1))),' &'
          call usrmsg('Beginning image tool via',doit,'-')
          call runit(doit,'graph')
        endif
      elseif(ino.eq.8)then
        siteinfo=siteinfo+1
        if(siteinfo.gt.2)siteinfo=0
      elseif(ino.eq.9)then
        databaseinfo=databaseinfo+1
        if(databaseinfo.gt.2)databaseinfo=0
      elseif(ino.eq.10)then
        contextinfo=contextinfo+1
        if(contextinfo.gt.2)contextinfo=0
      elseif(ino.eq.11)then
        if(controlinfo.ne.-1)then
          controlinfo=controlinfo+1
          if(controlinfo.gt.2)controlinfo=0
        endif
      elseif(ino.eq.12)then
        if(netinfo.ne.-1)then
          netinfo=netinfo+1
          if(netinfo.gt.2)netinfo=0
        endif
      elseif(ino.eq.13)then
        if(plantinfo.ne.-1)then
          plantinfo=plantinfo+1
          if(plantinfo.gt.2)plantinfo=0
        endif
      elseif(ino.eq.14)then

C If there are zones select from list.
        if(ncomp.gt.0)then
          H(1)=' Pick one, several or all zones to include.'
          INPIC=NCOMP
          CALL EPICKS(INPIC,IVALS,' ',' Which zones to summarise:',
     &        12,NCOMP,zname,' zone list',IER,1)
        endif
      elseif(ino.eq.15)then
        if(geominfo.ne.-1)then
          geominfo=geominfo+1
          if(geominfo.gt.3)geominfo=0
        endif
      elseif(ino.eq.16)then
        if(schedinfo.ne.-1)then
          schedinfo=schedinfo+1
          if(schedinfo.gt.2)schedinfo=0
        endif
      elseif(ino.eq.17)then
        if(zoneextrainfo.ne.-1)then
          zoneextrainfo=zoneextrainfo+1
          if(zoneextrainfo.gt.2)zoneextrainfo=0
        endif
      elseif(ino.eq.18)then

C Jump if no zones.
        if(inpic.eq.0) goto 3

C Generate report sorted by surface attributes.
        H(1)='Often it is convenient to review which surfaces in a '
        H(2)='project have particular attributes.'
        H(3)=' '
        H(4)='The following dialogue will allow you to define a '
        H(5)='subset of surfaces via filters for position,  '
        H(6)='orientation, opacity, etc. '
        CALL PHELPD('global search by attribute',6,'-',0,0,IER)

        H(1)='You may filter for already attributed surfaces or'
        H(2)='surfaces marked UNKNOWN. '
        CALL EASKABCD(' Filter for surface composition:',' ',
     &    'any construction','those marked UNKNOWN',
     &    'specific construction','cancel',ins4,2)
        if(ins4.eq.1)then
          write(mlcmatch,'(a)') 'ANY         '
        elseif(ins4.eq.2)then
          write(mlcmatch,'(a)') 'UNKNOWN     '
        elseif(ins4.eq.3)then
          CALL EPKMLC(ISEL,'Select construction for matching surfaces',
     &      'or -Exit',IER)
          if(ISEL.eq.0)return
          write(mlcmatch,'(a)')DESC(ISEL)(1:12)
        elseif(ins4.eq.4)then
          return
        endif

        totalarea=0.0
        H(1)='You may filter for opaque surfaces, transparent surfaces'
        H(2)='or both. '
        CALL EASKABCD(' First filter--surface opacity:',' ',
     &    'opaque','transparent','both','cancel',ins1,2)
        if(ins1.eq.4)return

        H(1)='You may filter for surfaces which face the outside,'
        H(2)='partitions (internal) etc. '
        call EASKATOG(' Second filter--surface contiguity:',' ',
     &      'external','similar','constant','partitions','ground',
     &      'adiabatic','any contiguity',ins2,2)

        H(1)='You may filter for surfaces which have a particular'
        H(2)='orientation. '
        call EASKATOG(' Third filter--surface orientation:',' ',
     &      'vertical','sloped','horizontal up','horizontal down',
     &      'any horizontal','any orientation',' ',ins3,2)


        call tstamp('>','PRJ: global reporting by attribution')
        write(outs,'(A,A)') 'cfg: ',LCFGF(1:lnblnk(LCFGF))
        call edisp(itu,outs)
        write(outs,'(A,A)') 'project: ',LSNAM(1:lnblnk(LSNAM))
        call edisp(itu,outs)
        call dstamp(dstmp)
        write(outs,'(A,A)') 'print date: ',dstmp
        call edisp(itu,outs)
        call edisp(itu,' ')
        if(ins4.eq.1)then
          call edisp(itu,' Filtering for ANY surface composition.')
        elseif(ins4.eq.2)then
          call edisp(itu,' Filtering for UNKNOWN surface composition.')
        elseif(ins4.eq.3)then
          write(outs,'(3a)') ' Filtering for ',DESC(ISEL)(1:12),
     &      ' composition.'
          call edisp(itu,outs)
        endif
        if(ins1.eq.1)then
          call edisp(itu,' Filtering for opaque surfaces.')
        elseif(ins1.eq.2)then
          call edisp(itu,' Filtering for transparent surfaces.')
        elseif(ins1.eq.3)then
          call edisp(itu,' Filtering for opaque or transparent.')
        endif 
        if(ins2.eq.1)then
          call edisp(itu,' Filtering for exterior connections.')
        elseif(ins2.eq.2)then
          call edisp(itu,' Filtering for similar connections.')
        elseif(ins2.eq.3)then
          call edisp(itu,' Filtering for constant connections.')
        elseif(ins2.eq.4)then
          call edisp(itu,' Filtering for internal partitions.')
        elseif(ins2.eq.5)then
          call edisp(itu,' Filtering for ground connections.')
        elseif(ins2.eq.6)then
          call edisp(itu,' Filtering for adiabatic connections.')
        elseif(ins2.eq.7)then
          call edisp(itu,' Filtering for any connections type.')
        endif 
        if(ins3.eq.1)then
          call edisp(itu,' Filtering for vertical orientation.')
        elseif(ins3.eq.2)then
          call edisp(itu,' Filtering for sloped orientation.')
        elseif(ins3.eq.3)then
          call edisp(itu,' Filtering for horizontal facing up.')
        elseif(ins3.eq.4)then
          call edisp(itu,' Filtering for horizontal facing down.')
        elseif(ins3.eq.5)then
          call edisp(itu,' Filtering for horizontal orientation.')
        elseif(ins3.eq.6)then
          call edisp(itu,' Filtering for any orientation.')
        endif 
        call edisp(itu,' ')

        WRITE(outs,93)
   93   FORMAT('Surface matching    |  Area  |Azim |Elev|',
     &         ' geometry| construction|environment')
        call edisp(itu,outs)
        WRITE(outs,94)
   94   FORMAT('                    |  m^2   |deg  |deg |',
     &         ' type|loc| name        |other side ')
        call edisp(itu,outs)
        DO 95 IZ=1,INPIC
          IF(IVALS(IZ).GT.0)THEN
            write(zn,'(A)') zname(IVALS(IZ))
            write(outs,'(3a)') ' Scanning: ',zn(1:lnblnk(zn)),
     &         '...'
            CALL USRMSG(' ',outs,'-')

C << it might not be necesary to scan geometry file >>
            LTMP=LGEOM(IVALS(IZ))
            call eclose(gversion(IVALS(IZ)),1.1,0.01,newgeo)
            if(newgeo)then
              call georead(IUF,LTMP,IVALS(IZ),1,0,iuout,IER)
            else
              call egomin(IUF,LTMP,IVALS(IZ),1,0,iuout,IER)
            endif

C For each surface in current zone check for match to attributes.
            do 11 i=1,nzsur(ivals(iz))
              icc=izstocn(ivals(iz),i)
              apply=.true.
              if(ins1.eq.1.and.SSOTF(icc)(1:4).ne.'OPAQ')apply=.false.
              if(ins1.eq.2.and.SSOTF(icc)(1:4).ne.'TRAN')apply=.false.

C Determine the match in the system topology. Note last parameter of the
C call suradj is a character string.
              CALL SURADJ(IVALS(IZ),I,IE,TMP,IZC,ISC,IC,DESCRC)
              if(ins2.eq.1.and.IE.ne.0)then
                apply=.false.
              elseif(ins2.eq.2.and.IE.ne.1)then
                apply=.false.
              elseif(ins2.eq.3.and.IE.ne.2)then
                apply=.false.
              elseif(ins2.eq.4.and.IE.ne.3)then
                apply=.false.
              elseif(ins2.eq.5.and.IE.ne.4)then
                apply=.false.
              elseif(ins2.eq.6.and.IE.ne.5)then
                apply=.false.
              elseif(ins2.eq.7)then
                continue
              endif

C Orientation check.
C 'vertical','sloped','horizontal up','horizontal down',
C 'any horizontal','any orien.','cancel'.
              if(ins3.eq.1.and.SSVFC(icc)(1:4).ne.'VERT')then
                apply=.false.
              elseif(ins3.eq.2.and.SSVFC(icc)(1:4).ne.'SLOP')then
                apply=.false.
              elseif(ins3.eq.3.and.SSVFC(icc)(1:4).ne.'CEIL')then
                apply=.false.
              elseif(ins3.eq.4.and.SSVFC(icc)(1:4).ne.'FLOR')then
                apply=.false.
              elseif((ins3.eq.5.and.SSVFC(icc)(1:4).eq.'FLOR').or.
     &               (ins3.eq.5.and.SSVFC(icc)(1:4).eq.'CEIL'))then
                continue
              elseif(ins3.eq.6)then
                continue
              endif
              if(ins4.eq.1)then
                continue
              elseif(ins4.eq.2)then
                if(SSMLCN(icc)(1:4).ne.'UNKN')apply=.false.
              elseif(ins4.eq.3)then
                if(SSMLCN(icc)(1:4).eq.'UNKN')apply=.false.
                if(SSMLCN(icc)(1:12).ne.DESC(ISEL)(1:12))apply=.false.
              endif
              
              if(apply)then
                write(sn,'(a)') SSNAME(icc)
                lsn10=MIN0(lnblnk(sn),10)
                lzn10=MIN0(lnblnk(zn),10)
                write(head,'(3a)') zn(1:lzn10),':',sn(1:lsn10)
                WRITE(outs,'(a,1X,F7.2,F6.1,F6.1,1X,A4,
     &            1X,A4,1X,A12,1X,A13)') head,SSNA(icc),SSPAZI(icc),
     &            SSPELV(icc),SSOTF(icc),SSVFC(icc),SSMLCN(icc),
     &            SSOTHER(icc,1)
                call edisp(itu,outs)
                totalarea = totalarea + SSNA(icc)
              endif
 11         continue
          endif
  95    continue
        write(outs,'(a,f10.2)') 'Area of matching surfaces: ',totalarea
        call edisp(itu,outs)

C If writing to file call ctlexp to close it and switch back to text feedback.
        if(destination.eq.1)then
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','geom text',IER)
          destination=0
        endif
        MODIFY=.TRUE.
        MODBND=.TRUE.
      elseif(ino.eq.19)then
        fileinfo=fileinfo+1
        if(fileinfo.gt.1)fileinfo=0
      elseif(ino.eq.20)then
        if(destination.eq.0)then
          destination = 1
          itu = ixunit
          write(xfile,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.contents'
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','QA text',IER)
          if(ier.eq.-3)then
            destination = 0
            itu = iuout
          endif
        elseif(destination.eq.1)then
          destination = 0
          itu = iuout
        endif
      elseif(ino.eq.21)then

C Generate the report based on current options.
        goto 55
      elseif(ino.eq.22)then

C If user just wrote to a file then allow browse and then
C edit of that QA file.  Use file browser if in GTK mode.
C If vi then spawn a new window to do the editing in.
        H(1)='The QA report file can be edited using the current'
        H(2)='defined text editor (as defined in the default file).'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        write(LFIL,'(a)') xfile(1:96)
        llitem=xfile
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(LFIL,'ASCII file name?',
     &      ' ',96,'xxx.rep','ASCII file',IER,2)
          write(llitem,'(a)') LFIL(1:lnblnk(LFIL))
        elseif(iglib.eq.2)then
          CALL EASKF(llitem,'ASCII file name?',
     &      ' ',144,'xxx.rep','ASCII file',IER,2)
        else
          CALL EASKF(LFIL,'ASCII file name?',
     &      ' ',96,'xxx.rep','ASCII file',IER,2)
          write(llitem,'(a)') LFIL(1:lnblnk(LFIL))
        endif

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
        write(doit,'(a,2x,a,a)') teditor(1:lnblnk(teditor)),
     &    longtfile(1:lnblnk(longtfile)),' &'
        call runit(doit,tmode)

      else

C Not one of the standard choices.
        goto 3
      endif
      goto 3

C Generate the QA report.
  55  call tstamp('>','PRJ: zone summary')
      call dstamp(dstmp)
      call edisp(itu,'Synopsis')
      call edisp(itu,' ')
      write(outs248,'(9A)') 'This is a synopsis of the model ',
     &  LSNAM(1:lnblnk(LSNAM)),' defined in ',LCFGF(1:lnblnk(LCFGF)),
     &  ' generated on ',dstmp,'. Notes associated with the model',
     &  ' are in ',LPRJLG(1:lnblnk(LPRJLG))
      call edisp248(itu,outs248,80)
      call edisp(itu,' ')

C Site information.
      if(siteinfo.gt.0)then
        write(outs248,'(a,f7.2,a,f6.2,2a,i4,5a)') 
     &    'The model is located at latitude ',XLAT,
     &    ' with a longitude difference of ',XLON,
     &    ' from the local time meridian. The year used in',
     &    ' simulations is ',IYEAR,' and weekends occur on ', 
     &     wkd1(1:lnblnk(wkd1)),' and ',wkd2(1:lnblnk(wkd2)),'.'
        call edisp248(itu,outs248,80)
        if(ixpos.eq.1)then
          WRITE(outs,'(a)')'site exposure is typical city centre'
        elseif(ixpos.eq.2)then
          WRITE(outs,'(a)')'site exposure is typical urban site'
        elseif(ixpos.eq.3)then
          WRITE(outs,'(a)')'site exposure is typical rural site'
        elseif(ixpos.eq.4)then
          WRITE(outs,'(a)')'site exposure is city: = sky, grnd, bldgs'
        elseif(ixpos.eq.5)then
          WRITE(outs,'(a)')'site exposure is city: below surroundings'
        elseif(ixpos.eq.6)then
          WRITE(outs,'(a)')'site exposure is isolated rural site site'
        elseif(ixpos.eq.7)then
          WRITE(outs,'(a)')'site exposure is totally enclosed (no sky)'
        elseif(ixpos.eq.8)then
          WRITE(outs,53)SKYR,GRDR,BLDR
  53      FORMAT('site exposure is sky=',F4.2,' ground=',F4.2,
     &      ' other buildings=',F4.2)
        endif
        write(outs248,'(3a,f4.2,a)') 'The ',outs(1:lnblnk(outs)),
     &    ' and the ground reflectance is ',GREF,'.'
        call edisp248(itu,outs248,80)
        call edisp(itu,' ')

C Print address for building and client.
        if(siteinfo.eq.2)then
          write(outs,'(2a)')'Project name: ',pjname(1:lnblnk(pjname))
          call edisp(itu,outs)
          write(outs,'(2a)')'Building address: ',baddress
          call edisp(itu,outs)
          write(outs,'(2a)')'Building city: ',bcity(1:lnblnk(bcity)) 
          call edisp(itu,outs)
          write(outs,'(2a)')'Building Postcode: ',buildingPC  
          call edisp(itu,outs)    
          call edisp(itu,' ')
          write(outs,'(2a)')'Building owner name: ',ownername
          call edisp(itu,outs)   
          write(outs,'(2a)')'Building owner telephone: ',ownertel
          call edisp(itu,outs)   
          write(outs,'(2a)')'Building owner address: ',owneraddr 
          call edisp(itu,outs) 
          write(outs,'(2a)')'Building owner city: ',ownercity 
          call edisp(itu,outs) 
          write(outs,'(2a)')'Building owner Postcode: ',ownerPC
          call edisp(itu,outs)     
          call edisp(itu,' ')
          write(outs,'(2a)')'Simulationist name: ',certifname 
          call edisp(itu,outs)     
          write(outs,'(2a)')'Simulationist telephone: ',certTel 
          call edisp(itu,outs)     
          write(outs,'(2a)')'Simulationist address: ',certaddress   
          call edisp(itu,outs)   
          write(outs,'(2a)')'Simulationist city: ',certcity  
          call edisp(itu,outs)    
          write(outs,'(2a)')'Simulationist postcode: ',certPC  
          call edisp(itu,outs)          
        endif

        CALL ERPFREE(ICLIM,ISTAT)
        CALL FPRAND(ICLIM,ISTAT,144,0,LCLIM)
        IREC=367
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLMLOC

C Tell about climate taking note of timing of solar data.
        if(iSlr_half_hr_flg.eq.0)then
          WRITE(outs248,'(5A)')'The climate used is: ',
     &      CLMLOC(1:lnblnk(CLMLOC)),' and is held in: ',
     &      LCLIM(1:lnblnk(LCLIM)),
     &      ' and uses hour centred solar data.'
        else
          WRITE(outs248,'(5A)')'The climate used is: ',
     &      CLMLOC(1:lnblnk(CLMLOC)),' and is held in: ',
     &      LCLIM(1:lnblnk(LCLIM)),
     &      ' and uses half hour centred solar data.'
        endif
        call edisp248(itu,outs248,80)

        if(NGRDP.gt.0.and.siteinfo.ge.1)then
          call edisp(itu,' ')  
          write(outs,'(a,i2,a)')'There are currently ',NGRDP,
     &      ' user defined ground temperature profiles.'      
          call edisp(itu,outs)
        endif
      endif

C Jump back point if err=1000 called when reading climate.
 1001 if(siteinfo.eq.2)then
        if(NGRDP.gt.0)then
          do 22 igrdp=1,NGRDP    
            CALL EDISP(itu,'Ground temperatures Jan-Dec:')
            WRITE(OUTS,'(12F6.1)')(UGRDTP(J,IGRDP),J=1,12)
            call edisp(itu,outs)
 22       continue
          call edisp(itu,' ')
        endif
        if(GTGEOM(1:2).eq.'  '.or.GTGEOM(1:4).eq.'UNKN')then
          continue
        else
          WRITE(outs248,'(5A)') 'The model includes ground topology ',
     &      GTNAME(1:lnblnk(GTNAME)),' which is defined in ',
     &      GTGEOM(1:lnblnk(GTGEOM)),'.'
            call edisp(itu,outs)
          call edisp248(itu,outs248,80)
          call edisp(itu,' ')
        endif
      endif

C Context of the model.
      if(contextinfo.eq.1)then
        if(ipconv.eq.1)then
          call edisp(itu,
     &'Primary energy conversions (demand kWhr to primary kWhr) used.')
        endif
      elseif(contextinfo.eq.2)then

C If there is an AIM-2 descritption scan the file and then report.
        if(iAIM2.eq.1)then
          call AIM2_READIN
          call AIM2_report(itu,ier)
        endif

        if(ipconv.eq.1)then
          call edisp(itu,
     &'Primary energy conversions (demand kWhr to primary kWhr) are:')
          WRITE(outs,'(A,F6.3)') ' heating   :',pcnvht
          call edisp(itu,outs)
          WRITE(outs,'(A,F6.3)') ' cooling   :',pcnvcl
          call edisp(itu,outs)
          WRITE(outs,'(A,F6.3)') ' lighting  :',pcnvlt
          call edisp(itu,outs)
          WRITE(outs,'(A,F6.3)') ' fans&pumps:',pcnvfn
          call edisp(itu,outs)
          WRITE(outs,'(A,F6.3)') ' small powr:',pcnvsp
          call edisp(itu,outs)
          WRITE(outs,'(A,F6.3)') ' hot water :',pcnvhw
          call edisp(itu,outs)
          call edisp(itu,' ')
          call edisp(itu,' Emissions (g/kWh) co2  nox  sox  ')
          WRITE(outs,'(A,3F7.2)')' heating : ',phtco2,phtnox,phtsox
          call edisp(itu,outs)
          WRITE(outs,'(A,3F7.2)')' cooling : ',pclco2,pclnox,pclsox
          call edisp(itu,outs)
          WRITE(outs,'(A,3F7.2)')' lighting: ',pltco2,pltnox,pltsox
          call edisp(itu,outs)
          WRITE(outs,'(A,3F7.2)')' fan_pump: ',pfnco2,pfnnox,pfnsox
          call edisp(itu,outs)
          WRITE(outs,'(A,3F7.2)')' small pr: ',pspco2,pspnox,pspsox
          call edisp(itu,outs)
          WRITE(outs,'(A,3F7.2)')' hot water:',phwco2,phwnox,phwsox
          call edisp(itu,outs)
        endif
      endif

C Check if IPV file and dispersed demands files are known.
      if(contextinfo.gt.0)then
        if(lnblnk(bdmds).eq.0)then
          continue
        elseif(bdmds(1:7).eq.'UNKNOWN')then
          continue
        else
          IUO=IFIL+1
          XST=.FALSE.
          call FINDFIL(bdmds,XST)
          if(XST)then
            CALL ERPFREE(IUO,ISTAT)
            CALL ERBDMD(ITRC,ITRU,IUO,IER)
            call edisp(itu,' ')
            call edisp(itu,
     &'The model includes dispersed casual gains (e.g. pumps & fans).')
            if(contextinfo.eq.1)then
              call edisp248(itu,dmdsdesc,80)
            elseif(contextinfo.eq.2)then
              call BDMDINF(itu,IER)
            endif
          endif
        endif

C Integrated Performance View. If compact only print description,
C if verbose print periods, seasons and ratios as well.
        ieripv=1  ! there is no IPV
        if(icfgv.lt.4)then
          if(lnblnk(lipvdatf).eq.0)then
            ieripv=1  ! there is no IPV
          elseif(lipvdatf(1:7).eq.'UNKNOWN')then
            ieripv=1  ! there is no IPV
          else
	    ipvaction='ipv'
            call ripvdat(IFCFG,lipvdatf,ipvaction,ieripv)  ! this returns a value for ieripv
            call edisp(itu,' ')
          endif
        else

C Check if embedded within configuration file.
          if(nipvassmt.eq.0)then ! there was nothing in the cfg file.
            ieripv=1  ! there is no IPV
          else
            ieripv=0  ! there is an IPV assessment
          endif
        endif
        if(ieripv.eq.0)then
          call edisp(itu,
     &      'An Integrated Performance View has been defined:')
          if(contextinfo.eq.1)then
            WRITE(outs,'(2A)')'Title   : ',ipvtitl(1:lnblnk(ipvtitl))
            call edisp(itu,outs)
            WRITE(outs,'(2A)')'Version : ',ipvvers(1:lnblnk(ipvvers))
            call edisp(itu,outs)
            call edisp(itu,'Synopsis:')
            call edisp248(itu,ipvsynop,80)
          elseif(contextinfo.eq.2)then

C Rescan the climatelist file to get seasons. If this was successful
C then include a description of the IPV. Otherwise warn user about
C the climate file and attempt to print out IPV contents.
            INQUIRE (FILE=cdblfil,EXIST=XST)
            if(XST)then
              IUF=IFIL+2
              call scancdblist(IUF,LCLIM,ok,iercl)
              if(iercl.eq.0)then
                call listipvdat(itu,'a',ier)
              else
                call edisp(itu,
     &            'The climatelist was incomplete or missing so the')
                call edisp(itu,'IPV details might be incorrect.')
                if(ieripv.eq.0)then
                  call listipvdat(itu,'a',ier)
                else
                  call edisp(itu,
     &              'The IPV description was incomplete or missing.')
                endif
              endif
            endif
          endif
        else
          call edisp(itu,
     &     'An Integrated Performance View is incomplete or missing.')
        endif

C If temporal data associated with the model report it.
        if(iabs(itdflg).eq.0)then
          continue
        elseif(iabs(itdflg).eq.1)then
          call edisp(itu,' ')
          call edisp(itu,'Temporal data is available for this model.')
          call edisp(itu,'It is an older file so update it via the')
          call edisp(itu,'edit/list option.')
        elseif(iabs(itdflg).eq.2)then
          call edisp(itu,' ')
          call edisp(itu,'Temporal data is available for this model.')
          call edisp(itu,'It is a slightly out of date file. Update')
          call edisp(itu,'it via the edit/list option.')
        elseif(iabs(itdflg).eq.3)then
          call edisp(itu,' ')
          call edisp(itu,'Temporal data is available for this model.')
          if(ITEMSTD.gt.0)then
            call edisp(itu,' ')
            call edisp(itu,' Temporal entities currently used...')
            call edisp(itu,'  ________________________________')
            call edisp(itu,'  |temporal    |generic |associated')
            call edisp(iuout,
     &           '  |entity name |type    |with   zone &     surface ')
            do 28, l=1,ITEMSTD
              WRITE(outs,29)l,DBTAG(L),DBTASK(L),DBZN(L),DBSN(L)
   29         FORMAT(i2,1X,A12,1X,A8,5X,A16,A16)
              call edisp(itu,outs)
  28        continue
            call edisp(itu,' ')
          endif
        endif
      endif

C Databases associated with the model.
      if(databaseinfo.gt.0)then
        call edisp(itu,' ')
        call edisp(itu,'Databases associated with the model: ')
        WRITE(outs248,'(2A)')' pressure distributions : ',
     &    LAPRES(1:lnblnk(LAPRES))
        call edisp248(itu,outs248,100)
        WRITE(outs,'(2A)')' materials              : ',
     &    LFMAT(1:lnblnk(LFMAT))
        call edisp(itu,outs)
        WRITE(outs,'(2A)')' constructions          : ',
     &    LFMUL(1:lnblnk(LFMUL))
        call edisp(itu,outs)
        WRITE(outs,'(2A)')' plant components       : ',
     &    LPCDB(1:lnblnk(LPCDB))
        call edisp(itu,outs)
        WRITE(outs,'(2A)')' event profiles         : ',
     &    LPRFDB(1:lnblnk(LPRFDB))
        call edisp(itu,outs)
        WRITE(outs248,'(2A)')' optical properties     : ',
     &    LOPTDB(1:lnblnk(LOPTDB))
        call edisp248(itu,outs248,100)   
      endif

C Control in the model
      if(controlinfo.gt.0)then

C List description of control and then the various zone descriptions.
C List control data.
        call edisp(itu,' ')
        WRITE(outs,240)
        call edisp(itu,outs)
        ICTLF=IUF
        CALL ERPFREE(ICTLF,ISTAT)
        call FINDFIL(LCTLF,XST)
        CALL EZCTLR(ICTLF,0,IUOUT,IER)
        if(fileinfo.eq.1)then
          call edisp(itu,' ')
          write(outs,'(3A)') 'Ideal control is defined in: ',
     &      LCTLF(1:lnblnk(LCTLF)),' as follows:'
          call edisp(itu,outs)
        else
         call edisp(itu,' ')
         call edisp(itu,'The model includes ideal controls as follows:')
        endif
        WRITE(outs,'(a)')'Control description: '
        call edisp(itu,outs)
        call edisp248(itu,ctldoc,72)

        if(NCF.gt.0)then
          call edisp(itu,' ')
          WRITE(outs,'(a,I2,a)')'Zones control includes ',
     &      NCF,' functions.'
          call edisp(itu,outs)
          call edisp248(itu,znctldoc,80)
          if(controlinfo.eq.2)then
            do 244 ii=1,NCF
              call LSTCNTL(itu,0,ii,IER)
 244        continue
            call LSTCNTL(itu,0,0,IER)
          endif
        endif
        if(NCL.gt.0)then
          call edisp(itu,' ')
          WRITE(outs,'(a,I2,a)')' Plant control includes ',
     &      NCL,' loops.'
          call edisp(itu,outs)
          call edisp248(itu,plctldoc,80)
          if(controlinfo.eq.2)then
            do 245 ii=1,NCL
              call LSTCNTL(itu,1,ii,IER)
 245        continue
          endif
        endif
        if(NCC.gt.0)then
          call edisp(itu,' ')
          WRITE(outs,'(a,I2,a)')' Flow control includes ',
     &      NCC,' loops.'
          call edisp(itu,outs)
          call edisp248(itu,flctldoc,80)
          if(controlinfo.eq.2)then
            do 246 ii=1,NCC
              call LSTCNTL(itu,2,ii,IER)
 246        continue
          endif
        endif
        if(NGF.gt.0)then
          call edisp(itu,' ')
          WRITE(outs,'(a,I2,a)')' Global control includes ',
     &      NGF,' loops.'
          call edisp(itu,outs)
          call edisp248(itu,glctldoc,80)
          if(controlinfo.eq.2)then
            do 147 ii=1,NGF
              call LSTCNTL(itu,3,ii,IER)
 147        continue
          endif
        endif
      endif

C If there is a flow network, rescan and list its contents.
      if(netinfo.gt.0)then
        if(IAIRN.ge.1)then
          WRITE(outs,240)
          call edisp(itu,outs)
          call edisp(itu,' ')
          call edisp(itu,'The model includes an air flow network.')
          if(netinfo.eq.2)then
            call mflist(itu)
          endif
          call edisp(itu,' ')
        endif
      endif

C If there is a plant network, scan it and report.
      if(plantinfo.gt.0)then
        if(LPNF(1:4).eq.'UNKN'.or.LPNF(1:2).eq.'  ')then
          continue
        else
          XST=.false.
          call FINDFIL(LPNF,XST)
          if(xst) then
            IFPNF=IFIL+3
            iunit1=IUF
            write(currentfile,'(a)') LPNF(1:lnblnk(LPNF))
            CALL EFOPSEQ(ifpnf,LPNF,1,IER)
            IF(IER.LT.0)THEN
              IER=1
              CALL edisp(iuout,' problem opening plant network file...')
            ENDIF
            call edisp(itu,' ')
            call edisp(itu,'The model includes a plant network.')
            call pltcfg(ifpnf,iunit1,itu,1)
C copy number of nodes in each plant component from node_count to
C nnodes: This is necessary because the nnodes common blocak (pcdat)
C is defined differently in bps, and the pltcfg.F file cannot be
C built as part of bps if it includes the prj pcdat common.
            call CopyPltNodeCounts()
          endif
        endif
      endif

C Jump if no zones.
      if(inpic.eq.0) goto 57

      call edisp(itu,' ')
      WRITE(outs,240)
      call edisp(itu,outs)
      call edisp(itu,' ')
      outs='ID Zone         Volume|          Surface          '
      call edisp(itu,outs)
      outs='   Name         m^3   | No. Opaque  Transp  ~Floor'
      call edisp(itu,outs)
      TVOL=0.
      TFLA=0.
      TOPQ=0.
      TTRN=0.
      do 97 IZ=1,INPIC
        if(IVALS(IZ).GT.0)then
          LTMP=LGEOM(IVALS(IZ))
          call eclose(gversion(IVALS(IZ)),1.1,0.01,newgeo)
          if(newgeo)then
            call georead(IUF,LTMP,IVALS(IZ),1,0,iuout,IER)
          else
            call egomin(IUF,LTMP,IVALS(IZ),1,0,iuout,IER)
          endif
          OPQ=0.
          TRN=0.
          attribok(IVALS(IZ))=.true.
          DO 41 I=1,NZSUR(ivals(iz))
            icc=izstocn(ivals(iz),i)
            if(SSOTHER(icc,1)(1:4).eq.'UNKN')attribok(IVALS(IZ))=.false.
            if(SSMLCN(icc)(1:4).eq.'UNKN')attribok(IVALS(IZ))=.false.
            if(SSOTF(icc)(1:4).NE.'OPAQ'.AND.
     &            SSOTF(icc)(1:4).NE.'CFC ')then
              TRN=TRN+SSNA(icc)
            else
              OPQ=OPQ+SSNA(icc)
            endif
   41     CONTINUE
          if(attribok(IVALS(IZ)))then
            aok=' attributed  '
            write(outs,'(i2,1x,a,f7.1,i4,3f8.1,2x,a)') IVALS(IZ),
     &      zname(IVALS(IZ)),VOL(IVALS(IZ)),NZSUR(ivals(iz)),OPQ,TRN,
     &      ZBASEA(IVALS(IZ)),
     &      zdesc(IVALS(IZ))(1:lnzdesc(IVALS(IZ)))
          else
            aok=' part-attributed '
            write(outs,'(i2,1x,a,f7.1,i4,3f8.1,a)') IVALS(IZ),
     &      zname(IVALS(IZ)),VOL(ivals(iz)),NZSUR(ivals(iz)),OPQ,TRN,
     &      ZBASEA(IVALS(IZ)),aok
          endif
          call edisp(itu,outs)
          TVOL=TVOL+VOL(IVALS(IZ))
          TFLA=TFLA+ZBASEA(IVALS(IZ))
          TOPQ=TOPQ+OPQ
          TTRN=TTRN+TRN
        endif
  97  continue
      write(outs,'(a,f9.0,i5,3f8.0)') '   all      ',TVOL,NCON,
     &   TOPQ,TTRN,TFLA
      call edisp(itu,outs)

C For each selected zone, scan geometry and produce reports.
C Zero project code complence data. xfloor is project floor area,
C xexposed is m2 of outside surface area, xvexposed is m2 of
C outside vertical surface, xuavgtran is average U value of windows,
C xuavwall is average U value of walls,
C xuavslproof is average U value of sloped roofs, xuavfltroof is
C average U value of flat roof, xuavgsky is average U value of skylights,
C xareatran m2 of windows, xareawall m2 of walls, xareaslproof m2 of
C sloped roofs, xareafltroof m2 of flat roofs, xareaskylt m2 of skylights,
C and xtranper xskyper xwallper xslproofper xflatroofper are % of
C project floor area for each.
      if(zoneextrainfo.gt.0)then
        xfloor = 0.0
        xexposed = 0.0
        xvexposed = 0.0
        xuavgtran = 0.0
        xuavwall = 0.0
        xuavslproof = 0.0
        xuavfltroof = 0.0
        xuavgsky = 0.0
        xareatran = 0.0
        xareawall = 0.0
        xareaslproof = 0.0
        xareafltroof = 0.0
        xareaskylt = 0.0
        xtranper = 0.0
        xskyper = 0.0
        xwallper = 0.0
        xslproofper = 0.0
        xflatroofper = 0.0
      endif

C Jump if no zones or no zone info requested.
      if(inpic.eq.0) goto 57
      if(geominfo.eq.0) goto 57

      do 98 IZ=1,INPIC
        IZZ=IVALS(IZ)
        if(IZZ.eq.0)goto 98
        LTMP=LGEOM(IZZ)
        call eclose(gversion(izz),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(IUF,LTMP,IZZ,1,0,itu,IER)
        else
          call egomin(IUF,LTMP,IZZ,1,0,itu,IER)
        endif

        call edisp(itu,'  ')
        WRITE(outs,240)
 240    format(1x,60('_'))
        if(iz.gt.1)call edisp(itu,outs)

        call ZINFOREP(itu,IZZ)  ! not dependant on georead.

C If there are external opaque and transparent surfaces report.
C Zero zone code complence data. xfloor is project floor area,
C exposed() is m2 of outside surface area, vexposed() is m2 of
C outside vertical surface, uavgtran() is average U value of windows,
C uavwall() is average U value of walls,
C uavslproof() is average U value of sloped roofs, uavfltroof() is
C average U value of flat roof, uavgsky() is average U value of skylights,
C areatran() m2 of windows, areawall() m2 of walls, areaslproof() m2 of
C sloped roofs, areafltroof() m2 of flat roofs, areaskylt() m2 of skylights,
C and tranper() skyper() wallper() slproofper() flatroofper() are % of
C floor area for each.
        xfloor = xfloor + ZBASEA(IZZ)
        uavgtran(IZZ) = 0.0
        uavwall(IZZ) = 0.0
        uavslproof(IZZ) = 0.0
        uavfltroof(IZZ) = 0.0
        uavgsky(IZZ) = 0.0
        areatran(IZZ) = 0.0
        areawall(IZZ) = 0.0
        areaslproof(IZZ) = 0.0
        areafltroof(IZZ) = 0.0
        areaskylt(IZZ) = 0.0
        tranper(IZZ) = 0.0
        skyper(IZZ) = 0.0
        wallper(IZZ) = 0.0
        slproofper(IZZ) = 0.0
        flatroofper(IZZ) = 0.0
        exposed(IZZ) = 0.0
        vexposed(IZZ) = 0.0
        do 111 ij=1,nzsur(izz)
          icc=izstocn(izz,ij)
          CALL SURADJ(IZZ,IJ,IE,TMP,IZC,ISC,IC,DESCRC) ! not dependant on georead
          if(IE.eq.0)then 

C Check the U value for each external surface.
C exposed walls and floors, pitched roofs, flat roofs
            UVH = 0.0
            UVU = 0.0
            UVD = 0.0
            do 511 ii=1,nmlc
              if(SSMLCN(icc)(1:12).eq.DESC(ii)(1:12)) then

C Recover the ISO 6946 U values for horizontal, upwards and downwards
C flow as well as internal partition (horizontal 0.13 both sides).
C Note: ISO 6946 advises use of horizontal hc value for surfaces where
C heat flow direction is unknown. This assumption is used in
C the U and UA reporting below.
                call etmldbu(0,itu,ii,UVH,UVU,UVD,UVI)

C Debug.
C                write(6,*) DESC(ii)(1:12),'ii UVH UVU UVD UVI',
C     &            ii,UVH,UVU,UVD,UVI
              else
                continue
              endif
  511       continue
            exposed(IZZ) = exposed(IZZ) + SSNA(icc)
            xexposed = xexposed + SSNA(icc)
            if(SSOTF(icc)(1:4).eq.'OPAQ')then
              if(SSVFC(icc)(1:4).eq.'VERT')then

C For vertical walls assume horizontal hc coef.
                areawall(IZZ) = areawall(IZZ) + SSNA(icc)
                uavwall(IZZ) = uavwall(IZZ) + (SSNA(icc) * UVH)
                vexposed(IZZ) = vexposed(IZZ) + SSNA(icc)
                xvexposed = xvexposed + SSNA(icc)
                xareawall = xareawall + SSNA(icc)
                xuavwall = xuavwall + (SSNA(icc) * UVH)
              elseif(SSVFC(icc)(1:4).eq.'SLOP')then

C For sloped surfaces.
                areaslproof(IZZ) = areaslproof(IZZ) + SSNA(icc)
                uavslproof(IZZ) = uavslproof(IZZ) + (SSNA(icc) * UVH)
                xareaslproof = xareaslproof + SSNA(icc)
                xuavslproof = xuavslproof + (SSNA(icc) * UVH)
              elseif(SSVFC(icc)(1:4).eq.'CEIL')then

C For ceilings.
                areafltroof(IZZ) = areafltroof(IZZ) + SSNA(icc)
                uavfltroof(IZZ) = uavfltroof(IZZ) + (SSNA(icc) * UVH)
                xareafltroof = xareafltroof + SSNA(icc)
                xuavfltroof = xuavfltroof + (SSNA(icc) * UVH)
              else

C For floors.
                areawall(IZZ) = areawall(IZZ) + SSNA(icc)
                uavwall(IZZ) = uavwall(IZZ) + (SSNA(icc) * UVH)
                xareawall = xareawall + SSNA(icc)
                xuavwall = xuavwall + (SSNA(icc) * UVH)
              endif
            elseif(SSOTF(icc)(1:4).ne.'OPAQ'.and.
     &            SSOTF(icc)(1:4).ne.'CFC ')then
              if(SSVFC(icc)(1:4).eq.'CEIL'.or.
     &           SSVFC(icc)(1:4).eq.'SLOP')then

C Consider glazing on ceiling or sloped to be a skylight.
                areaskylt(IZZ) = areaskylt(IZZ) + SSNA(icc)
                uavgsky(IZZ) = uavgsky(IZZ) + (SSNA(icc) * UVH) 
                xareaskylt = xareaskylt + SSNA(icc)
                xuavgsky = xuavgsky + (SSNA(icc) * UVH)
              elseif(SSVFC(icc)(1:4).eq.'VERT')then

C Consider glazing on walls to be a part of facade.
                areatran(IZZ) = areatran(IZZ) + SSNA(icc)
                uavgtran(IZZ) = uavgtran(IZZ) + (SSNA(icc) * UVH)
                vexposed(IZZ) = vexposed(IZZ) + SSNA(icc)
                xvexposed = xvexposed + SSNA(icc)
                xareatran = xareatran + SSNA(icc)
                xuavgtran = xuavgtran + (SSNA(icc) * UVH)
              else
                areatran(IZZ) = areatran(IZZ) + SSNA(icc)
                uavgtran(IZZ) = uavgtran(IZZ) + (SSNA(icc) * UVH)
                vexposed(IZZ) = vexposed(IZZ) + SSNA(icc)
                xvexposed = xvexposed + SSNA(icc)
                xareatran = xareatran + SSNA(icc)
                xuavgtran = xuavgtran + (SSNA(icc) * UVH)
              endif
            else
              continue
            endif
          else
            continue
          endif

C Check for duplicate surface names in this zone. Report is
C duplicates are found.
          write(SN,'(a12)')SSNAME(icc)
          call snamdup(SN,izz,ij,nameok)
          if(nameok)then
            continue
          else
            write(outs,'(3a)') 'Surface ',SN(1:lnblnk(SN)),
     &        ' has a duplicate name. Names must be unique!'
            call edisp(itu,outs)
          endif
 111    continue

C Write out UA information if extra information requested.
        if(zoneextrainfo.gt.0)then

C Debug...
C         write(itu,*) 'in zone ',izz,' area wall = ',areawall(IZZ)
C         write(itu,*) 'area flat roof = ',areafltroof(IZZ)
C         write(itu,*) 'area sloped roof = ',areaslproof(IZZ)
C         write(itu,*) 'area skylights = ',areaskylt(IZZ)
C         write(itu,*) 'area windows = ',areatran(IZZ)
C         write(itu,*) 'floor area = ',ZBASEA(IZZ)

          if(exposed(IZZ).gt.0.1)then

C If there is external glazing or skylights then report.
            call rel16str(exposed(IZZ),t16a,lna,ier)
            call rel16str(vexposed(IZZ),t16b,lnb,ier)
            if(vexposed(IZZ).gt.0.1)then
              write(outs,'(5a)')' There is ',
     &          t16a(1:lna),'m2 of exposed surface area, ',
     &          t16b(1:lnb),'m2 of which is vertical.'
            else
              write(outs,'(3a)')' There is ',
     &          t16a(1:lna),'m2 of exposed surface area.'
            endif
            if(attribok(IVALS(IZ)))call edisp(itu,outs)
          endif
          if(areawall(IZZ).gt.0.1)then
            if(ZBASEA(IZZ).gt.0.01)then
              wallper(IZZ) = (areawall(IZZ)/ZBASEA(IZZ)) * 100.
            else
              wallper(IZZ) = 1.0
            endif
            call rel16str(wallper(IZZ),t16a,lna,ier)
            call rel16str(uavwall(IZZ),t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')' Outside walls are ',
     &        t16a(1:lna),' % of floor area & avg U of ',
     &        uavwall(IZZ)/areawall(IZZ),' & UA of ',t16b(1:lnb)
            if(attribok(IVALS(IZ)))call edisp(itu,outs)
          endif
          if(areaslproof(IZZ).gt.0.1)then
            if(ZBASEA(IZZ).gt.0.01)then
              slproofper(IZZ) = (areaslproof(IZZ)/ZBASEA(IZZ))*100.
            else
              slproofper(IZZ) = 1.0
            endif
            call rel16str(slproofper(IZZ),t16a,lna,ier)
            call rel16str(uavslproof(IZZ),t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')' Sloped roof is ',
     &        t16a(1:lna),' % of floor area & avg U of ',
     &        uavslproof(IZZ)/areaslproof(IZZ),' & UA of ',
     &        t16b(1:lnb)
            if(attribok(IVALS(IZ)))call edisp(itu,outs)
          endif
          if(areafltroof(IZZ).gt.0.1)then
            if(ZBASEA(IZZ).gt.0.01)then
              flatroofper(IZZ) = (areafltroof(IZZ)/ZBASEA(IZZ))*100.
            else
              flatroofper(IZZ) = 1.0
            endif
            call rel16str(flatroofper(IZZ),t16a,lna,ier)
            call rel16str(uavfltroof(IZZ),t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')' Flat roof is ',
     &        t16a(1:lna),' % of floor area & avg U of ',
     &        uavfltroof(IZZ)/areafltroof(IZZ),' & UA of ',
     &        t16b(1:lnb)
            if(attribok(IVALS(IZ)))call edisp(itu,outs)
          endif
          if(areatran(IZZ).gt.0.1)then
            if(ZBASEA(IZZ).gt.0.01)then
              tranper(IZZ) = (areatran(IZZ)/ZBASEA(IZZ)) * 100.
            else
              tranper(IZZ) = 1.0
            endif
            vtranper = (areatran(IZZ)/vexposed(IZZ)) * 100.
            call rel16str(tranper(IZZ),t16a,lna,ier)
            call rel16str(vtranper,t16b,lnb,ier)
            call rel16str(uavgtran(IZZ),t16c,lnc,ier)
            write(outs,'(5a,F5.3,2a)')
     &        ' Glazing is ',t16a(1:lna),' % of floor & ',t16b(1:lnb),
     &        ' % facade with avg U of ',
     &        uavgtran(IZZ)/areatran(IZZ),' & UA of ',t16c(1:lnc)
            if(attribok(IVALS(IZ)))call edisp(itu,outs)
          endif
          if(areaskylt(IZZ).gt.0.1)then
            if(ZBASEA(IZZ).gt.0.01)then
              skyper(IZZ) = (areaskylt(IZZ)/ZBASEA(IZZ)) * 100.
            else
              skyper(IZZ) = 1.0
            endif
            call rel16str(skyper(IZZ),t16a,lna,ier)
            call rel16str(uavgsky(IZZ),t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')' Skylights are ',
     &        t16a(1:lna),' % of floor area & avg U of ',
     &        uavgsky(IZZ)/areaskylt(IZZ),' & UA of ',t16b(1:lnb)
            if(attribok(IVALS(IZ)))call edisp(itu,outs)
          endif
        endif

C Note: surinfo not dependant on recent geometry scan.
        if(geominfo.eq.2)then
          context=.true.
          CALL SURINFO(IVALS(IZ),itu,context)
        endif

C Check if the edges are proprly bounded. Uses G1 common.
        call ckvert(0,IVALS(IZ),bound,iub,inv,ier)
        if(.NOT.bound)then
          iprb=MAX0(iub,inv)
          WRITE(outs,'(A,i3,a)')  ' There are ',iub,
     &      ' edges which are not bounded (zone might be incomplete)'
          if(iub.gt.0)call edisp(itu,outs)
          WRITE(outs,'(A,i3,a)')  ' There are ',inv,
     &      ' surface with reversed edges (one might be reversed)'
          if(inv.gt.0)call edisp(itu,outs)
          if(iprb.gt.4)then
            call edisp(itu,' Please check your zone geometry.')
          endif
        endif

        if(zoneextrainfo.ge.1)then
          CALL INSINFO(IVALS(IZ),itu)

C Note any optical controls. First read zone constructions and
C then parse TMC file and if there are optical controls then list.
          XST=.false.
          call FINDFIL(LTHRM(IVALS(IZ)),XST)
          if(XST)then
            CALL ECONST(LTHRM(IVALS(IZ)),IUF,IVALS(IZ),0,IUOUT,IER)
          endif
          if(ITW(IVALS(IZ)).eq.1)then
            CALL ERTWIN(0,IUOUT,IUF,LTWIN(IVALS(IZ)),IVALS(IZ),IER)
            call edisp(itu,' ')
            DO 2020 I=1,NTMC
              if(IBCMT(IVALS(IZ),I).EQ.0)then
                continue
              else
                lto=lnblnk(TOPTIC(IVALS(IZ),I))
                if(IBCSUR(IVALS(IZ),I).eq.0)then
                  write(outs,'(4a,i2,a)')' Surfaces with optic ',
     &              TOPTIC(IVALS(IZ),I)(1:lto),' individually sensed ',
     &              'over ',NBCTMC(IVALS(IZ),I),' control periods.'
                  call edisp(itu,outs)
                else
                  write(outs,'(3a,i2,a,i2,a)')' Surfaces with optic ',
     &              TOPTIC(IVALS(IZ),I)(1:lto),' sense surface ',
     &              IBCSUR(IVALS(IZ),I),' over ',NBCTMC(IVALS(IZ),I),
     &              ' control periods.'
                  call edisp(itu,outs)
                endif

C Remind user of standard optics as well as the alternatives.
                write(outs,'(a,5f6.3)') ' Standard optic solar trans:',
     &            (TMCT(IVALS(IZ),I,M),M=1,5)
                call edisp(itu,outs)
                DO 2021 KK=1,NBCTMC(IVALS(IZ),I)
                  if(NBCTT(IVALS(IZ),KK,I).eq.0)then
                    msg=' sensing total radiation'
                  elseif(NBCTT(IVALS(IZ),KK,I).eq.1)then
                    msg=' sensing ambient temperature'
                  elseif(NBCTT(IVALS(IZ),KK,I).eq.2)then
                    msg=' sensing zone temperature'
                  elseif(NBCTT(IVALS(IZ),KK,I).eq.3)then
                    msg=' sensing daylight coeff.'
                  elseif(NBCTT(IVALS(IZ),KK,I).eq.4)then
                    msg=' Lightswitch2002.'
                  elseif(NBCTT(IVALS(IZ),KK,I).eq.-99)then
                    msg=' sensing time only'
                  endif
                  WRITE(OUTS,'(A,I2,A,I2,A,I2,2A,F7.2,a,5f6.3)') 
     &              ' Period ',KK,': from ',IBCST(IVALS(IZ),KK,I),
     &              ' to ',IBCFT(IVALS(IZ),KK,I),msg(1:lnblnk(msg)),
     &              ' setpoint @ ',BACTPT(IVALS(IZ),KK,I),' alt solar',
     &              (TMCT2(IVALS(IZ),I,M,KK),M=1,5)
                  CALL EDISP(itu,OUTS)
 2021           continue
              endif
 2020       continue
            call edisp(itu,' ')
          endif
        endif

C Note optional zone extensions.
        if(zoneextrainfo.ge.1)then
          if(IVF(IVALS(IZ)).EQ.1)call edisp(itu,
     &      ' Explicit viewfactors have been derived for this zone.')
          if(ICGC(IVALS(IZ)).EQ.1)call edisp(itu,
     &      ' Casual gains are controlled in this zone.')
          if(ISI(IVALS(IZ)).EQ.1)then
            call FINDFIL(LSHAD(IVALS(IZ)),XST)
            if(XST)then
              call edisp(itu,
     &          ' Shading patterns have been calculated for this zone.')
            else
              call edisp(itu,
     &          ' Shading patterns have not yet been calculated.')
            endif
          endif
          if(IHC(IVALS(IZ)).EQ.1)then
            if(zoneextrainfo.eq.1)then
              call edisp(itu,
     &         ' Non-default convection regimes are used in this zone.')
            elseif(zoneextrainfo.eq.2)then
              call FINDFIL(LHCCO(IVALS(IZ)),XST)
              IF(XST)THEN
                call ehtcff(LHCCO(IVALS(IZ)),IUF,itrc,IER)
                call listhtc(itu,IVALS(IZ),IER)
              endif
            endif
          endif
        endif

C Check for mismatch in surface attributes and boundary conditions.
        call edisp(itu,' ')
        bndrysxc=.true.
        DO 1243 IS=1,NZSUR(IVALS(IZ))
          icc=IZSTOCN(IVALS(IZ),is)
          if(ICT(icc).eq.0.and.SSOTHER(icc,1)(1:5).ne.'EXTER')
     &       bndrysxc=.false.
          if(ICT(icc).eq.1.and.SSOTHER(icc,1)(1:5).ne.'SIMIL')
     &       bndrysxc=.false.
          if(ICT(icc).eq.2.and.SSOTHER(icc,1)(1:5).ne.'CONST')
     &       bndrysxc=.false.
          if(ICT(icc).eq.3.and.SSOTHER(icc,1)(1:5).eq.'CONST')
     &       bndrysxc=.false.
          if(ICT(icc).eq.3.and.SSOTHER(icc,1)(1:5).eq.'SIMIL')
     &       bndrysxc=.false.
          if(ICT(icc).eq.3.and.SSOTHER(icc,1)(1:5).eq.'EXTER')
     &       bndrysxc=.false.
          if(ICT(icc).eq.3.and.SSOTHER(icc,1)(1:5).eq.'GROUN')
     &       bndrysxc=.false.
          if(ICT(icc).eq.4.and.SSOTHER(icc,1)(1:5).ne.'GROUN')
     &       bndrysxc=.false.
          if(ICT(icc).eq.5.and.SSOTHER(icc,1)(1:5).ne.'ADIAB')
     &       bndrysxc=.false.

C While looping check that constructions of partitions are matching.
          if(ICT(icc).eq.3)then
            showother=.true.
            icoth=IZSTOCN(IC2(icc),IE2(icc))
          else
            showother=.false.
          endif
          write(SN,'(a12)')SSNAME(icc)
          if(showother.and.icoth.ne.0)then
            do 17 ii=1,nmlc
              if(SSMLCN(icc)(1:12).eq.DESC(ii)(1:12)) then
                call parsemlcdesc(DESC(ii),MLCN,OTF,OPT,MLCSYM)
                if(MLCSYM(1:9).EQ.'SYMMETRIC')then
                  continue
                elseif(MLCSYM(1:12).EQ.'NONSYMMETRIC')then

C If the current construction is nonsymmetric then it should not be
C used for a partition (and db does not have a linked MLC - inform the user.
                  write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &              ' has a nonsymmetric construction ',
     &              SSMLCN(icc)(1:lnblnk(SSMLCN(icc))),'.'
                  call edisp(itu,outs)
                  write(outs,'(5a)') 'It faces ',
     &              SSNAME(icoth),' which is composed of ',
     &              SSMLCN(icoth)(1:12),' (which may not match).'
                  call edisp(itu,outs)
                else
                  continue
                endif
              endif
  17        continue
          endif
 1243   continue
        if(.NOT.bndrysxc)then
          call edisp(itu,
     &   ' NOTE: Some surface boundary attributions did not match the')
          call edisp(itu,
     &   ' master connections list. A topology update may be required.')
        endif

        if(schedinfo.ge.1)then

C If extended reporting, check to see if operations file is the same
C as an earlier one.
          lco=LPROJ(IVALS(IZ))
          call FINDFIL(lco,XST)
          if(XST)then
            ilcot=0
            duplicate=.false.
            if(IZ.gt.1)then
              do 99 IZO=1,IZ
                lcot=LPROJ(IVALS(IZO))
                if(lco(1:lnblnk(lco)).eq.lcot(1:lnblnk(lcot)))then
                  if(IZO.ne.IZ)then
                    duplicate=.true.
                    ilcot=IVALS(IZO)
                    goto 101
                  endif
                endif
  99          continue
            endif
 101        if(duplicate.and.ilcot.ne.0)then
              write(outs,'(2a)') ' Uses same operations as zone ',
     &          zname(ilcot)(1:lnzname(ilcot))
              call edisp(itu,outs)
            else
              CALL ERPFREE(IUF,ISTAT)
              CALL EROPER(0,iuout,IUF,IVALS(IZ),IER)
              if(IAIRN.ge.1.and.ICAAS(IVALS(IZ)).ne.0)then
                call edisp(itu,
     & ' Ventilation & infiltration is assessed via network analysis')
            write(outs,'(2a)') ' and the associated network node is: ',
     &             NDNAM(ICAAS(IVALS(IZ)))
                call edisp(itu,outs)
              else
                if(schedinfo.eq.1)then
                  call edisp(itu,' ')
                  call edisp248(itu,oprdesc(IVALS(IZ)),80)
                elseif(schedinfo.eq.2)then
                  call edisp(itu,' ')
                  CALL VENTINF(IVALS(IZ),ITU)
                endif
              endif
              if(schedinfo.eq.2)then
                call CASINF(IVALS(IZ),ITU)
              endif
              CALL ERPFREE(IUF,ISTAT)
            endif
          else
            write(outs,'(2a)') ' No operations defined for zone ',
     &        zname(IVALS(IZ))(1:lnzname(IVALS(IZ)))
            call edisp(itu,'  ')
            call edisp(itu,outs)
          endif
        endif
 98   continue

C Report code complience information for the project. Do this only
C if the zones are fully attributed.
      if(zoneextrainfo.ge.1)then
        allattribok=.true.
        do 699 ijk=1,INPIC
          if(.NOT.attribok(IVALS(IJK))) allattribok=.false.
 699    continue
        if(.NOT.allattribok)then
          call edisp(itu,'  ')
          call edisp(itu,'Project AU reporting not included because')
          call edisp(itu,'some zones are not fully attributed.')
        else
          call edisp(itu,'  ')
          WRITE(outs,240)
          call edisp(itu,outs)
          call edisp(itu,'  ')
          call rel16str(xfloor,t16a,lna,ier)
          call rel16str(xareawall,t16b,lnb,ier)
          call rel16str(xareatran,t16c,lnc,ier)
          write(outs,'(7a)') ' Project floor area is ',t16a(1:lna),
     &      'm2, wall area is ',t16b(1:lnb),'m2, window area is ',
     &      t16c(1:lnc),'m2.'
          call edisp(itu,outs)
          call rel16str(xareaslproof,t16a,lna,ier)
          call rel16str(xareafltroof,t16b,lnb,ier)
          call rel16str(xareaskylt,t16c,lnc,ier)
          write(outs,'(7a)') ' Sloped roof area is ',t16a(1:lna),
     &      'm2, flat roof area is ',t16b(1:lnb),
     &      'm2, skylight area is ',t16c(1:lnc),'m2.'
          call edisp(itu,outs)
          if(xexposed.gt.0.1)then
            call rel16str(xexposed,t16a,lna,ier)
            call rel16str(xvexposed,t16b,lnb,ier)
            if(xvexposed.gt.0.1)then
              write(outs,'(5a)')' There is ',
     &          t16a(1:lna),'m2 of outside surface area, ',
     &          t16b(1:lnb),'m2 of which is vertical.'
            else
              write(outs,'(3a)')' There is ',
     &          t16a(1:lna),'m2 of outside surface area.'
            endif
            call edisp(itu,outs)
          endif
          call edisp(itu,'  ')
          if(xareawall.gt.0.1)then
            xwallper = (xareawall/xfloor) * 100.
            call rel16str(xwallper,t16a,lna,ier)
            call rel16str(xuavwall,t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')' Outside walls are ',
     &        t16a(1:lna),' % of floor area & avg U of ',
     &        xuavwall/xareawall,' & UA of ',t16b(1:lnb)
            call edisp(itu,outs)
          endif
          if(xareaslproof.gt.0.1)then
            xslproofper = (xareaslproof/xfloor) * 100.
            call rel16str(xslproofper,t16a,lna,ier)
            call rel16str(xuavslproof,t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')' Sloped roof is ',
     &        t16a(1:lna),' % of floor area & avg U of ',
     &        xuavslproof/xareaslproof,' & UA of ',t16b(1:lnb)
            call edisp(itu,outs)
          endif
          if(xareafltroof.gt.0.1)then
            xflatroofper = (xareafltroof/xfloor) * 100.
            call rel16str(xflatroofper,t16a,lna,ier)
            call rel16str(xuavfltroof,t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')' Flat roof is ',
     &        t16a(1:lna),' % of floor area & avg U of ',
     &        xuavfltroof/xareafltroof,' & UA of ',t16b(1:lnb)
            call edisp(itu,outs)
          endif
          if(xareatran.gt.0.1)then
            xtranper = (xareatran/xfloor) * 100.
            vtranper = (xareatran/xvexposed) * 100.
            call rel16str(xtranper,t16a,lna,ier)
            call rel16str(vtranper,t16b,lnb,ier)
            call rel16str(xuavgtran,t16c,lnc,ier)
            write(outs,'(5a,F5.3,2a)')
     &        ' Glazing is ',t16a(1:lna),' % of floor & ',t16b(1:lnb),
     &        ' % facade with avg U of ',
     &        xuavgtran/xareatran,' & UA of ',t16c(1:lnc)
            call edisp(itu,outs)
          endif
          if(xareaskylt.gt.0.1)then
            xskyper = (xareaskylt/xfloor) * 100.
            call rel16str(xskyper,t16a,lna,ier)
            call rel16str(xuavgsky,t16b,lnb,ier)
            write(outs,'(3a,F5.3,2a)')' Skylights are ',
     &        t16a(1:lna),' % of floor area & avg U of ',
     &        xuavgsky/xareaskylt,' & UA of ',t16b(1:lnb)
            call edisp(itu,outs)
          endif
        endif
      endif

C MLC database reporting.
   57 if(databaseinfo.ge.1)then
        call edisp(itu,' ')
        WRITE(outs,240)
        call edisp(itu,outs)
        call edisp(itu,' Multi-layer constructions used:')
        do 342 imlc=1,nmlc
          found=.false.
          mlcname=DESC(imlc)(1:12)
          call mlcrefs(mlcname,areamlc,tareamlc,found)
          if(found)then
            if(databaseinfo.eq.2)then
              call etmldb(2,itu,imlc,imerr)
            endif
            write(outs,'(3a,F9.2)') ' Total area of ',
     &        mlcname(1:lnblnk(mlcname)),' is ',tareamlc
            call edisp(itu,outs)
          endif
  342   continue
      endif

C If writing to file call ctlexp to close it and switch back to text feedback.
      if(destination.eq.1)then
        call ctlexp(xfile,ixopen,ixloc,ixunit,'T','geom text',IER)
        destination=0
        call edisp(iuout,
     &    'The new QA file can be edited via the `edit file` option.')
        call edisp(iuout,' ')
      endif
      goto 3

C Error trap.
 1000 call usrmsg('Problem scanning climate file',lclim,'W')
      goto 1001

      end

C ************ mupdate
C Mupdate checks whether current model should be updated and guides
C the user through several of the tasks. Assumes that the user is
C not browsing.

      subroutine mupdate(ier)
#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/cctlnm/ctldoc,lctlf
      common/CFGV/icfgv
      common/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      common/ishdirec/iaplic(MCOM,2),nsurfcalc(MCOM),lstsfcalc(MCOM,MS),
     &     nsurfinso(MCOM),isurfinso(MCOM,MS)
      common/p3ver/ip3ver(MCOM)
      
C IPV data.
      common/IPVF/lipvdatf
      character lipvdatf*72

C Images.
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton

      character cfgroot*24,LCFGF*72,h*72,outs*124
      character LVIEW*72,LHCCO*72,LTWIN*72,LCGCIN*72,ZOBS*72
      CHARACTER CTLDOC*248,LCTLF*72
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      character ZN*12
      character ipvaction*3  ! to instruct it to read from external IPV file
      logical ok,dok,ddok,found,XST,anyish,unixok
      logical newgeo  ! to use for testing if new/old geometry file.
      logical allnew  ! if true then all zones are v1.1 or newer.
      logical forceupgrade  ! if any other zone is version 1.1
      logical MODOPR ! to flag if zone operations file needs changing.

C Flags noting whether casual gain periods are currently sorted.
      logical sorted,problem
      integer iuo  ! file unit for operations file.
      integer imgloop ! loop counter for images

C Check if Unix-based or DOS based.
      call isunix(unixok)
      newgeo=.false.  ! assume older format geometry.
      sorted=.false.  ! assume casual gains not sorted.

C Loop through and find out if all zone geometry files are
C v1.1 or newer (so set igupgrade=2). If so then there is
C no need for an upgrade. And if there are v1.1 geometry
C files but still some zone obstruction files then there
C is still a need for upgrading.
      allnew=.true.
      do 775 iz=1,ncomp
        if(gversion(iz).lt.1.1) then
          allnew=.false.
        elseif(gversion(iz).ge.1.1) then
          if(iobs(iz).eq.0)then
            continue
          elseif(iobs(iz).eq.1)then
            allnew=.false.
          elseif(iobs(iz).eq.2)then
            continue
          endif
        endif
  775 continue
      if(allnew)then
        igupgrade=2
        forceupgrade=.false.
      else

C Loop through and find out if any zone geometry files are
C version 1.0 and also if the upgrade policy is non-zero.
C So if the model is mixed then force an upgrade. If the
C igupgrade was set to 2 in preferences also force an upgrade.
        forceupgrade=.false.
        do 777 iz=1,ncomp
          if(gversion(iz).gt.1.0) forceupgrade=.true.
  777   continue
        if(forceupgrade.and.igupgrade.eq.0) igupgrade=2
        if(igupgrade.eq.2) forceupgrade=.true.
      endif

      if(forceupgrade)then
        dok=.true.
        h(1)='Older models should be updated to take advantage of'
        h(2)='extended geometric data structures and file formats'
        h(3)=' '
        h(4)='An update will also reduce the number of model '
        h(5)='files which have to be managed. '
        call askok('The current model uses an out-of-date format for',
     &    'some geometry or obstruction files. Update this?',ok,dok,5)
        if(ok)then

          do 776 iz=1,ncomp
            call eclose(gversion(iz),1.1,0.01,newgeo)
            if(newgeo)then

C If all of the shading directives are zero then double check to
C see if there are shading and obstruction files.
              call georead(IFIL+2,LGEOM(iz),iz,0,0,iuout,IER)
              if(nsurfcalc(iz).eq.0.and.nsurfinso(iz).eq.0)then
                if(ISI(iz).eq.0.and.IOBS(iz).eq.0)then
                  continue
                elseif(ISI(iz).gt.0.and.IOBS(iz).eq.0)then
                  iaplic(iz,1)=0   ! reset for insolation only
                  iaplic(iz,2)=1
                  nsurfcalc(iz)=0
                  nsurfinso(iz)=0
                  DO 678 I=1,NZSUR(iz)
                    ioc=IZSTOCN(iz,i)
                    if(SSOTHER(ioc,1)(1:5).eq.'EXTER'.and.
     &                 SSOTF(ioc)(1:4).ne.'OPAQ'.and.
     &                 SSOTF(ioc)(1:4).ne.'CFC ')then
                      nsurfinso(iz)=nsurfinso(iz)+1
                      isurfinso(iz,nsurfinso(iz))=I
                    endif
  678             continue
                  call geowrite(IFIL+2,LGEOM(iz),iz,iuout,3,IER)
                elseif(ISI(iz).gt.0.and.IOBS(iz).gt.0)then
                  iaplic(iz,1)=1   ! reset for insolation only
                  iaplic(iz,2)=1
                  nsurfcalc(iz)=0
                  nsurfinso(iz)=0
                  DO 679 I=1,NZSUR(iz)
                    ioc=IZSTOCN(iz,i)
                    if(SSOTHER(ioc,1)(1:5).eq.'EXTER'.and.
     &                 SSOTF(ioc)(1:4).ne.'OPAQ'.and.
     &                 SSOTF(ioc)(1:4).ne.'CFC ')then
                      nsurfinso(iz)=nsurfinso(iz)+1
                      isurfinso(iz,nsurfinso(iz))=I
                    endif
                    if(SSOTHER(ioc,1)(1:5).eq.'EXTER')then
                      nsurfcalc(iz)=nsurfcalc(iz)+1
                      lstsfcalc(iz,nsurfcalc(iz))=I
                    endif
  679             continue

                  call geowrite(IFIL+2,LGEOM(iz),iz,iuout,3,IER)
                endif
              endif

C If there is a v1.1 geometry file and iobs() is 1 then the
C obstructions need to be moved into the geometry file and
C zobs() cleared for this zone..
              if(iobs(iz).eq.0)then
                continue
              elseif(iobs(iz).eq.1)then
                CALL EGOMST(IFIL+2,ZOBS(iz),0,ITRC,ITRU,IER)
                iobs(iz)=2
                zobs(iz)=' '
                call geowrite(IFIL+2,LGEOM(iz),iz,iuout,3,IER)
              elseif(iobs(iz).eq.2)then
                continue
              endif
            else

C On first entry for version 1.0 several common blocks need to be
C filled in case the model is a mix of 1.0 and 1.1 versions. First
C figure out implied shading and insolation directives. If there
C is no shading file or obstruction file assume nothing. If there
C is a shading file an no obstruction set for insolation only.
C Otherwise assume all applicable for shading and insolation.
              call egomin(IFIL+2,LGEOM(iz),iz,0,0,iuout,IER)
              if(ISI(iz).eq.0.and.IOBS(iz).eq.0)then
                iaplic(iz,1)=0   ! reset for no implied shading or insolation
                iaplic(iz,2)=0
                nsurfcalc(iz)=0
                nsurfinso(iz)=0
              elseif(ISI(iz).gt.0.and.IOBS(iz).eq.0)then
                iaplic(iz,1)=0   ! reset for insolation only
                iaplic(iz,2)=1
                nsurfcalc(iz)=0
                nsurfinso(iz)=0
                DO 778 I=1,NZSUR(iz)
                  ioc=IZSTOCN(iz,i)
                  if(SSOTHER(ioc,1)(1:5).eq.'EXTER'.and.
     &               SSOTF(ioc)(1:4).ne.'OPAQ'.and.
     &               SSOTF(ioc)(1:4).ne.'CFC ')then
                    nsurfinso(iz)=nsurfinso(iz)+1
                    isurfinso(iz,nsurfinso(iz))=I
                  endif
  778           continue
              elseif(ISI(iz).gt.0.and.IOBS(iz).gt.0)then
                iaplic(iz,1)=1   ! reset for insolation only
                iaplic(iz,2)=1
                nsurfcalc(iz)=0
                nsurfinso(iz)=0
                DO 779 I=1,NZSUR(iz)
                  ioc=IZSTOCN(iz,i)
                  if(SSOTHER(ioc,1)(1:5).eq.'EXTER'.and.
     &               SSOTF(ioc)(1:4).ne.'OPAQ'.and.
     &               SSOTF(ioc)(1:4).ne.'CFC ')then
                    nsurfinso(iz)=nsurfinso(iz)+1
                    isurfinso(iz,nsurfinso(iz))=I
                  endif
                  if(SSOTHER(ioc,1)(1:5).eq.'EXTER')then
                    nsurfcalc(iz)=nsurfcalc(iz)+1
                    lstsfcalc(iz,nsurfcalc(iz))=I
                  endif
  779           continue
              endif
              gversion(iz) =1.1
              newgeo = .true.
              call geowrite(IFIL+2,LGEOM(iz),iz,iuout,3,IER)

C If iobs() is 1 then the obstructions need to be moved into the
C geometry file and zobs() cleared for this zone..
              if(iobs(iz).eq.0)then
                continue
              elseif(iobs(iz).eq.1)then
                CALL EGOMST(IFIL+2,ZOBS(iz),0,ITRC,ITRU,IER)
                iobs(iz)=2
                zobs(iz)=' '
                call geowrite(IFIL+2,LGEOM(iz),iz,iuout,3,IER)
              elseif(iobs(iz).eq.2)then
                continue
              endif
            endif

C The documentation in the zone construction and tmc files might
C have changed to scan and write out these files.
C << to be done >>

C Check for the version of the zone operations file and if it is
C an older format write to new format.
            IUO=IFIL+1
            call FINDFIL(LPROJ(iz),XST)
            IF(XST)THEN
              CALL ERPFREE(IUO,ISTAT)
              CALL EROPER(ITRC,ITRU,IUO,iz,IER)

C Do cursory check to see if the file is sorted.
              sorted=.true.
              problem=.false.
              call checksort(iz,1,problem,ier)
              if(problem)then
                sorted=.false.
                MODOPR=.true.
                call edisp(iuout,
     &           'Weekday casual gains might be unsorted.')
              endif
              problem=.false.
              call checksort(iz,2,problem,ier)
              if(problem)then
                sorted=.false.
                MODOPR=.true.
                call edisp(iuout,
     &            'Saturday casual gains might be unsorted.')
              endif
              problem=.false.
              call checksort(iz,3,problem,ier)
              if(problem)then
                sorted=.false.
                MODOPR=.true.
                call edisp(iuout,
     &            'Sunday casual gains might be unsorted.')
              endif

C If older format ip3ver=0 then force it to write to the
C current format.
              if(ip3ver(iz).eq.0)then
                MODOPR=.true.
                ip3ver(iz)=2
              endif

C Save the file, sorting the periods if necessary.
              if(MODOPR)then
                if(sorted)then
                  CALL EMKOPER(IUO,LPROJ(IZ),IZ,ITRU,IER)
                else
                  call PROCESSOLDCAS(iz,0,iuout,IER)
                  MODOPR=.true.
                  CALL EMKOPER(IUO,LPROJ(IZ),IZ,ITRU,IER)
                endif
              endif
            endif
  776     continue

C Save the model configuration file before continuing.
          CALL EMKCFG('-',IER)

C While we are at it, control files often include out of date descriptions
C and if there is a control file scan it and write it back out again.
          if(LCTLF(1:2).eq.'  '.or.LCTLF(1:4).eq.'UNKN')then
            continue
          else
            ICTLF=IFIL+1
            CALL ERPFREE(ICTLF,ISTAT)
            XST=.false.
            call FINDFIL(LCTLF,XST)
            if(XST)then
              CALL EZCTLR(ICTLF,0,IUOUT,IER)
              CALL CTLWRT(ICTLF,IER)
              call usrmsg(' ',' ','-')
            endif
          endif
        endif
      endif

C Check to see if version of configuration file requires updating.
      if(icfgv.lt.4)then
        dok=.true.
        h(1)='Older models should be updated to take advantage of'
        h(2)='extended documentation and quality assurance facilities'
        h(3)='as well as enhanced databases.'
        h(4)='An update will also reduce the number of descriptive '
        h(5)='files which have to be managed. '
        h(6)=' '
        h(7)='Older models with seaparate IPV (integrated performance'
        h(8)='view files will have this data moved into the cfg file.'
        h(9)=' '
        call askok('The current model uses an out-of-date format for',
     &    'the configuration file. Update this?',ok,dok,9)
        if(ok)then

C Check if IPV file known and fill common blocks before writing out to
C the cfg file. If it was previously merged in the file name will now
C be a black string.
          if(lnblnk(lipvdatf).eq.0)then
            continue
          elseif(lipvdatf(1:7).eq.'UNKNOWN')then
            continue
          else
            IUO=IFIL+1
	    ipvaction='ipv'
            call ripvdat(IUO,lipvdatf,ipvaction,ier)
          endif
          icfgv=4   ! set to current version.

C If there are any images associated with the model create a filler
C string for the image documentation.
          if(noimg.gt.0)then
            do 22 imgloop=1,MIMG
              imgdoc(imgloop)='no documentation (yet) for this image'
  22        continue
          endif

C Update the model configuration file.
          CALL EMKCFG('-',IER)

C Check if there were zone utility files and if so check and see if
C they can be deleted.
          found=.false.
          if(ncomp.gt.0)then
            do 2 iz=1,ncomp
              if(INDUTL(iz).eq.1)then
                XST=.false.
                call FINDFIL(LUTIL(iz),XST)
                if(XST)found=.true.
              endif
  2         continue
            if(found)then
              dok=.true.
              h(1)='This must be an ancient ESP-r model. It has some'
              h(2)='data held in an obsolete file and you should'
              h(3)='allow this to be put in the current format.'
              call askok('Remove legacy zone utility files?',' ',
     &          ddok,dok,3)
              do 4 iz=1,ncomp
                if(INDUTL(iz).eq.1)then
                  XST=.false.
                  call FINDFIL(LUTIL(iz),XST)
                  if(XST)then
                    if(ddok)then 
                      INDUTL(iz)=0
                      call usrmsg('Removing file: ',LUTIL(iz),'-')
                      CALL ERPFREE(IUF,ISTAT)
                      CALL EFOPSEQ(IUF,LUTIL(iz),1,IER)
                      CALL EFDELET(IUF,ISTAT)
                    else
                      INDUTL(iz)=0
                      call usrmsg('Dereferencing file: ',LUTIL(iz),'-')
                    endif
                  endif
                  CALL EMKCFG('-',IER)
                  call usrmsg(' ',' ','-')
                endif
  4           continue
            endif
          endif
        else
          return
        endif
      endif

C Read in and write out the control file if it exists. Why test
C for the version number?  Because older versions of ESP-r did
C not have atributed control files.
      if(icfgv.lt.3)then
        if(LCTLF(1:2).eq.'  '.or.LCTLF(1:4).eq.'UNKN')then
          continue
        else
          ICTLF=IFIL+1
          CALL ERPFREE(ICTLF,ISTAT)
          XST=.false.
          call FINDFIL(LCTLF,XST)
          if(XST)then
            CALL EZCTLR(ICTLF,0,IUOUT,IER)
            CALL CTLWRT(ICTLF,IER)
            call usrmsg(' ',' ','-')
          endif
        endif
      endif

C Offer option to update shading files.
      if(icfgv.lt.4)then
        anyish=.false.
        do 42 iz=1,ncomp
          if(ISI(iz).eq.1)then
            call FINDFIL(LSHAD(iz),XST)
            if(XST)then
              write(ZN,'(A)') zname(iz)
              write(outs,'(3a)') 'Shading for ',ZN(1:lnblnk(ZN)),
     &          ' could be updated.'
              call edisp(iuout,outs)
              anyish=.true.
            endif
          endif
  42    continue
      else
        anyish=.false.
      endif

C If there are no shading files, don't bother trying to update.
      if(anyish)then
        h(1)='Older shading files should be recalculated to reflect'
        h(2)='changes in the allowable number of surfaces in each'
        h(3)='zone. Each affected zone will be done in turn.'
        h(4)=' '
        h(5)='Recent versions of the ish module are able to make a'
        h(6)='backup copy of shading databases in ASCII format. '
        CALL EASKABC(' Update of shading options:',' ',
     &    'recalculate (silent)','recalculate (interactive)','continue',
     &     IRT,6)
      else
        IRT=3
      endif
      if(IRT.eq.1.or.IRT.eq.2)then
        call edisp(iuout,'Each zone will be done in sequence.')
        if(IRT.eq.1)then
          call edisp(iuout,
     & 'When finished control will be returned to the project manager.')
        endif
        do 43 iz=1,ncomp
          if(ISI(iz).eq.0) goto 43
          call FINDFIL(LSHAD(iz),XST)
          if(XST)then

C If user asked for silent recalculation then do each
C one in forground in an xterm with `-act update_silent`
C as the command line.
            if(IRT.eq.1)then
              call comissionish(iz,'sr ',ier)
            else
              call comissionish(iz,'ir ',ier)
            endif
          endif
  43    continue
        call edisp(iuout,
     &    'Update of zones with shading files complete.')
      endif

      return

      end
      
C ******************* ADMIT *******************
C Calculate U and Y values and surface factor.
C If a negative conductivity is supplied the abs value represents the 
C resistance of an air gap.
C << NOTE: this routine gets close for some constructions, but
C << some variables are off - further debugging required.
      subroutine ADMIT(ITRC,ITRU,IFOC,Rsi,Rso)
#include "building.h"
#include "esprdbfile.h"

      character DESC*48
      integer NMLC,IPR,LAYERS
      real DTHK,DRAIR
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)

C Arrays of material properties.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      real matdbcon,matdbden,matdbsht,matdboute,matdbine
      real matdbouta,matdbina,matdbdrv,matdbthick
      common/matdatarray/matlegindex(MGIT),matcatindex(MGIT),
     &  matdbcon(MGIT),matdbden(MGIT),matdbsht(MGIT),matdboute(MGIT),
     &  matdbine(MGIT),matdbouta(MGIT),matdbina(MGIT),matdbdrv(MGIT),
     &  matdbthick(MGIT)

      integer mathash
      common/mathasharray/mathash(0:600)

      character matname*32,matdoc*248,matopaq*1
      common/matnamearray/matname(MGIT),matdoc(MGIT),matopaq(MGIT)

      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the indes within matdatarray
      logical closemat
      
      dimension thick(8),rho(8),Cp(8),cond(8)
      CHARACTER NAM*72,TITL*72,MLCN*12,OTF*4,OPT*12
      character MLCSYM*12
      
      COMPLEX P,L(2,2),M(2,2),N(2,2)

C Pick up general description of the composite.
      ier=0
      call parsemlcdesc(DESC(IFOC),MLCN,OTF,OPT,MLCSYM)
      
C ESP-r defines layers from outside to inside. The code in
C subroutine admit works from inside to outside.
C Debug layer info.
C      write (ITRU,*) 'Layer  thick  k  rho  Cp'
      layer=LAYERS(IFOC)+1
      call eclose(matver,1.1,0.01,closemat)
      do 5 IL=1,LAYERS(IFOC)
        if(closemat)then
          legmat=IPR(IFOC,IL)   ! which legacy index
          if(legmat.ge.0)then
            matarrayindex=mathash(legmat)  ! find matching array index
            DBCON=matdbcon(matarrayindex)
            DBDEN=matdbden(matarrayindex)
            DBSHT=matdbsht(matarrayindex)
            E=matdbine(matarrayindex)
            A=matdbina(matarrayindex)
            DRV=matdbdrv(matarrayindex)
            write(TITL,'(a)') 'placeholder'
            write(NAM,'(a)') matname(matarrayindex)(1:32)
          endif
        else
          CALL ERPCDB(IFMAT,IPR(IFOC,IL),ITRU,IER,DBCON,DBDEN,
     &      DBSHT,E,A,DRV,TITL,NAM)
        endif
        layer=layer-1
        thick(layer)=DTHK(IFOC,IL)

C If an air gap (material zero) then set cond(IL) to negative of air gap res.
        IF(IPR(IFOC,IL).EQ.0)THEN
          cond(layer) = (-1.0) * DRAIR(IFOC,IL,1)
          rho(layer)=1.0
          Cp(layer)=1.0
        else
          cond(layer)=DBCON
          rho(layer)=DBDEN
          Cp(layer)=DBSHT
        endif

C Debug
        write (ITRU,'(2i3,2f6.3,2f6.0)') layer,
     &    thick(layer),cond(layer),rho(layer),Cp(layer)
 5    continue
      
C Start calculation of U-value.
      Uvalue=Rsi
      
C Define inside surface resistance matrix.
      L(1,1)=CMPLX(1.,0.)
      L(1,2)=CMPLX(Rsi,0.)
      L(2,1)=CMPLX(0.,0.)
      L(2,2)=CMPLX(1.,0.)
      
C Loop through layers (inside to outside).
      do 10 I=1,LAYERS(IFOC)
        if (cond(I).gt.0) then
          Uvalue=Uvalue+(thick(I)/cond(I))

C Define terms for arrays.
          xP=((4.*ATAN(1.))*thick(I)*thick(I)*rho(I)*Cp(I))/
     &     (86400.*cond(I))
          xP=SQRT(xP)
        
C Calculate cosh(P).
          xcreal=((exp(xP)+exp(-xP))*cos(xP))/2.
          xcimage=((exp(xP)-exp(-xP))*sin(xP))/2.

C and sinh(P).
          xsreal=((exp(xP)-exp(-xP))*cos(xP))/2.
          xsimage=((exp(xP)+exp(-xP))*sin(xP))/2.
        
C Enter matrix coefficients
          M(1,1)=CMPLX(xcreal,xcimage)
          M(1,2)=(thick(I))*CMPLX(xsreal,xsimage)/
     &         (cond(I)*CMPLX(xP,xP))
          M(2,1)=(cond(I)*CMPLX(xP,xP))*CMPLX(xsreal,xsimage)/
     &         (thick(I))
          M(2,2)=CMPLX(xcreal,xcimage)
        else
        
C Air gap
          Uvalue=Uvalue+abs(cond(I))
          M(1,1)=CMPLX(1.,0.)
          M(1,2)=CMPLX(abs(cond(I)),0.)
          M(2,1)=CMPLX(0.,0.)
          M(2,2)=CMPLX(1.,0.)
        endif

C Calculate L*M, result in N.
        call CMATMUL(L,M,N)
        
C Copy N to L.
        L(1,1)=N(1,1)
        L(1,2)=N(1,2)
        L(2,1)=N(2,1)
        L(2,2)=N(2,2)
 10   continue
      Uvalue=Uvalue+Rso
      Uvalue=1./Uvalue
 
      write (ITRU,'(a)') ' '
      write (ITRU,'(3(a,f6.2))') 'Admittance calculations using Rsi ',
     &  rsi,' Rso ',rso,' & Uvalue=',Uvalue
      
C Outside face resistance.
      M(1,1)=CMPLX(1.,0.)
      M(1,2)=CMPLX(Rso,0.)
      M(2,1)=CMPLX(0.,0.)
      M(2,2)=CMPLX(1.,0.)
      call CMATMUL(L,M,N)
            
C Calculate admittance.
      P=N(2,2)/N(1,2)
      Y=abs(P)
      omega=(12./(4.*ATAN(1.)))*atan(aimag(P)/real(P))

      write (ITRU,*) 'External surface'
      write (ITRU,'(2(a,f6.2))') ' Admittance Y=',Y,'  w=',omega
      
C Calculate decrement factor.
      P=1./(Uvalue*N(1,2))
      f=abs(P)
      phi=(12./(4.*ATAN(1.)))*atan(aimag(P)/real(P))
      if (phi.gt.0) then
        phi=12.-phi
      else
        phi=-phi
      endif
      
      write (ITRU,'(2(a,f6.2))') ' Decrement factor f=',f,'  phi=',phi

C Calculate surface factor.
      P=1.-(Rsi*N(2,2)/N(1,2))
      f=abs(P)
      phi=(12./(4.*ATAN(1.)))*atan(aimag(P)/real(P))
      if (phi.gt.0) then
        phi=12.-phi
      else
        phi=-phi
      endif
      
      write (ITRU,'(2(a,f6.2))') ' Surface factor f=',f,'  phi=',phi

C Internal surfaces.
      P=(N(2,2)-1.)/N(1,2)
      Y=abs(P)
      omega=(12./(4.*ATAN(1.)))*atan(aimag(P)/real(P))

      write (ITRU,*) '  '
      write (ITRU,*) 'Partition'
      write (ITRU,'(2(a,f6.2))') ' Admittance Y=',Y,'  w=',omega
      
C Calculate surface factor.
      P=1.-(Rsi*P)
      f=abs(P)
      phi=(12./(4.*ATAN(1.)))*atan(aimag(P)/real(P))
      if (phi.gt.0) then
        phi=12.-phi
      else
        phi=-phi
      endif
      
      write (ITRU,'(2(a,f6.2))') ' Surface factor f=',f,'  phi=',phi
      return
      end

C ******************** CMATMUL 
C CMATMUL multiplies the complex (2x2) matrices A by B returning C.
      SUBROUTINE CMATMUL(A,B,C)

      COMPLEX A(2,2),B(2,2),C(2,2)

C Calculate A*B.
      DO 10 I=1,2
        C(I,1)=A(I,1)*B(1,1)+A(I,2)*B(2,1)
        C(I,2)=A(I,1)*B(1,2)+A(I,2)*B(2,2)
   10 CONTINUE

      return
      end
      
