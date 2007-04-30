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

C Subroutines used by all modules for setting up environemt and
C parsing command lines.
C  SCESPRC: scans the ESP-r dot file (environment)
C  ESCDEF : scans the ESP-r defaults definition file
C  parsepar variants: Parse command lines.

C ******************** scesprc 
C scesprc scans the ESP-r dot file which should be located in the
C users home folder.  If not found then some defaults are set.
      SUBROUTINE scesprc(LFDEF,IFDEF,ITRC,IER)
      COMMON/OUTIN/IUOUT,IUIN
      common/uhome/upath

C External text editor.
      common/texted/tedlbl,teditor

C External document previewer.
      common/textpv/tprevlbl,tpreview

C External figure generator.
      common/figtool/figlbl,figexe

C External graphing tool.
      common/graphtool/grflbl,grfexe

C External folder browser.
      common/folderbr/foldlbl,foldexe

C A set of possible image formats which can be accepted within
C a model configuration file. These definitions are held in the
C so-called esprc file. To add additional image types and/or 3rd
C party display applications edit the ESP-r Install script which
C creates the esprc file.
C   imgtyp is the number of different image formats supported
C   fmttag (4 char) is a tag for each image formt (e.g. GIF XBM)
C   fmtexe (20 char) is the application name used to display
C          images of type fmttag.
      common/showimg/imgtyp,fmttag(5),fmtexe(5)

C A set of possible cad packages: labels and executables.
      common/cad3rd/cadlbl,cadexe,cadfmt

C Report generator: label and executable.
      common/rep3rd/replbl,repexe

C External tutorial.
      common/tutopts/tutlbl,tutexe

C Save journal. journoo = ON or OFF
      common/journopt/journio,iuj,journcmd,jfile

C Graphic capture, window dump & text print: label, command.
      common/gprint/gprlbl,gprcmd
      common/gxwd/gxwdlbl,gxwdcmd
      common/tprint/tprlbl,tprcmd

C Where exemplars list and default db list are kept.
      common/exemlst/exemlbl,exemfl
      common/defdb/dfdblbl,defdbfl

C Where validation tests are kept.
      common/vldlst/vldlbl,vldfl

C Where list of climate files is held.
      common/clmlst/cdblbl,cdblfil
      logical localdot,unixok,dll

      CHARACTER OUTSTR*124,WORD*20,LFDEF*72,outs*124
      character fmttag*4,fmtexe*20,cadlbl*20,cadexe*20,cadfmt*4
      character replbl*20,repexe*20
      character gprlbl*20,gprcmd*48
      character gxwdlbl*20,gxwdcmd*60
      character tprlbl*20,tprcmd*48
      character tedlbl*20,teditor*20
      character tprevlbl*20,tpreview*48
      character figlbl*20,figexe*48
      character grflbl*20,grfexe*48
      character foldlbl*20,foldexe*48
      character tutlbl*20,tutexe*96
      character exemlbl*20,exemfl*72
      character vldlbl*20,vldfl*72 
      character dfdblbl*20,defdbfl*72
      character cdblbl*20,cdblfil*72
      character journcmd*20,jfile*72
      character altdot*72
      character upath*72
#include "espinstalldir.h"

C Check if running in dll mode, if so clear most of the
C initial values because they will not be used.
      call isadll(dll)
      if(dll)then
        teditor=' '
        tedlbl='no word processor'
        tpreview=' '
        tprevlbl='no document preview'
        figlbl='diagram editor'
        figexe=' '
        grflbl='no graphing tool'
        grfexe=' '
        foldlbl='no folder browser'
        foldexe=' '
        cadlbl='no CAD package'
        cadexe=' '
        cadfmt='ZIP'
        replbl='No report generator'
        repexe='UNKNOWN'
        gprcmd='UNKNOWN'
        tutlbl='Tutorial'
        tutexe=' '
        journcmd=' '
        jfile=' '
        imgtyp=1
        fmttag(1)='GIF'
        fmtexe(1)=' '
        gxwdlbl='no graphic dump'
      else

C Assume initial values for interactive use. Default to putting 
C temporary files in the users home folder.
        teditor='vi'
        tedlbl='word processor'
        tpreview='GROFF -p '
        tprevlbl='document previewer'
        figlbl='diagram editor'
        figexe='xfig'
        grflbl='graphing tool'
        grfexe='xvgr'
        foldlbl='folder browser'
        foldexe='xfs '
        cadlbl='CAD package'
        cadexe='xzip'
        cadfmt='ZIP'
        replbl='No report generator'
        repexe='UNKNOWN'
        gprcmd='UNKNOWN'
        tutlbl='Tutorial'
        tutexe='netscape http://www.esru.strath.ac.uk &'
        journcmd=' '
        jfile=' '

C Assume two basic image types and that the application window dump
C will use xwud.
        imgtyp=2
        fmttag(1)='GIF'
        fmtexe(1)='xv'
        fmttag(2)='XWD'
        fmtexe(2)='xwud -in'
        gxwdlbl='Appl window dump'
      endif

C Set defaults and exemplars depending on whether Unix or NT.
      call isunix(unixok)
      ln=lnblnk(instpath)
      if(unixok)then
        write(gxwdcmd,'(3a)')'xwud -out ',upath(1:lnblnk(upath)),
     &  '/gxwd.xwd'
        tprlbl='Text dump'
        write(tprcmd,'(a,a)') upath(1:lnblnk(upath)),'/tx_dump'
        exemlbl='Standard exemplars'
        write(exemfl,'(2a)') instpath(1:ln),'/training/exemplars'
        dfdblbl='Standard defaults'
        write(defdbfl,'(2a)') instpath(1:ln),'/default'
C        defdbfl='/usr/esru/esp-r/default'
        vldlbl='Validation standards'
        write(vldfl,'(2a)') instpath(1:ln),'/validation/stds_list'
        cdblbl='Standard climates'
        write(cdblfil,'(2a)') instpath(1:ln),'/climate/climatelist'
C        cdblfil='/usr/esru/esp-r/climate/climatelist'
        write(altdot,'(2a)') instpath(1:ln),'/esprc'
C        altdot='/usr/esru/esp-r/esprc'
      else
        write(gxwdcmd,'(a)')'xwud -out c:/temp/gxwd.xwd'
        tprlbl='Text dump'
        write(tprcmd,'(a)') 'c:/temp/tx_dump'
        exemlbl='Standard exemplars'
        write(exemfl,'(2a)') instpath(1:ln),'/training/exemplars'
        dfdblbl='Standard defaults'
        write(defdbfl,'(2a)') instpath(1:ln),'/default'
        vldlbl='Validation tests'
        write(vldfl,'(2a)') instpath(1:ln),'/validation/stds_list'
        cdblbl='Standard climates'
        write(cdblfil,'(2a)') instpath(1:ln),'/climate/climatelist'
        write(altdot,'(2a)') instpath(1:ln),'/esprc'
      endif

C If running in dll mode skip reading .esprc file.
      if(dll)then
        return
      endif

C Open the esprc file, return if not found.
      IER=0
      CALL ERPFREE(IFDEF,ISTAT)

C Debug.
C      write(6,*) 'lfdef is ',lfdef
      call FPOPEN(IFDEF,ISTAT,1,0,LFDEF)
      IF(ISTAT.LT.0)THEN
        CALL ERPFREE(IFDEF,ISTAT)
        call FPOPEN(IFDEF,ISTAT,1,0,altdot)
        IF(ISTAT.LT.0)THEN
          call edisp(iuout,' No esprc file found so setting default')
          call edisp(iuout,' exemplars, CAD, report & print links...')
          IER=1
          CALL ERPFREE(IFDEF,ISTAT)
          RETURN
        else
          localdot=.false.
        endif
      else
        localdot=.true.
      ENDIF

C Read '*ESPRC' tag.
      CALL STRIPC(IFDEF,OUTSTR,0,ND,1,'ESPRC tag',IER)
      if(OUTSTR(1:6).ne.'*ESPRC')then
        if(localdot)then
        write(outs,'(a,a)')LFDEF(:lnblnk(LFDEF)),' not an .esprc file!'
        else
        write(outs,'(a,a)')altdot(:lnblnk(altdot)),' not a .esprc file!'
        endif
        call edisp(iuout,OUTSTR)
        call edisp(iuout,outs)
        IER=1
        CALL ERPFREE(IFDEF,ISTAT)
        RETURN
      endif

C Take image types from the file.
      imgtyp=0


C If '*ESPRC' found then read db until '*end' found.
  44  continue
      CALL STRIPC(IFDEF,OUTSTR,0,ND,1,'dot sequences',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','dot sequences',IFLAG)
      if(WORD(1:5).eq.'*gprn')then

C Capture rectangular area of screen via capexgf is in wwlib.c 
        CALL EGETP(OUTSTR,K,gprlbl,'W','rect area lbl',IER)
        CALL EGETP(OUTSTR,K,gprcmd,'W','rect area exe',IER)
        write(outs,'(a,a,2x,a)')' Rectangular area grab: ',gprlbl,gprcmd
        if(itrc.gt.0)call edisp(iuout,outs)
        call capexgf(gprcmd)
      elseif(WORD(1:5).eq.'*gxwd')then

C Screen capture commands - dump whole screem via capexall in wwlib.c
        CALL EGETP(OUTSTR,K,gxwdlbl,'W','screen dump lbl',IER)
        CALL EGETP(OUTSTR,K,gxwdcmd,'W','screen dump exe',IER)
        write(outs,'(a,a,2x,a)')' Screen dump: ',gxwdlbl,gxwdcmd
        if(itrc.gt.0)call edisp(iuout,outs)
        call capexall(gxwdcmd)
      elseif(WORD(1:5).eq.'*tprn')then

C Caputre text buffer to a file, capextf is in wwlib.c
        CALL EGETP(OUTSTR,K,tprlbl,'W','text print lbl',IER)
        CALL EGETP(OUTSTR,K,tprcmd,'W','text print file',IER)
        write(outs,'(a,a,2x,a)')' Local text print: ',tprlbl,tprcmd
        if(itrc.gt.0)call edisp(iuout,outs)
        call capextf(tprcmd)
      elseif(WORD(1:4).eq.'*cad')then
        CALL EGETP(OUTSTR,K,cadlbl,'W','cad lbl',IER)
        CALL EGETP(OUTSTR,K,cadexe,'W','cad exe',IER)
        CALL EGETW(OUTSTR,K,cadfmt,'W','cad fnt',IER)
        write(outs,'(a,a,2x,a,2x,a)')' Local CAD: ',cadlbl,cadexe,cadfmt
        if(itrc.gt.0)call edisp(iuout,outs)
      elseif(WORD(1:13).eq.'*tutorial_app')then

C Tutorial application.
        CALL EGETP(OUTSTR,K,tutlbl,'W','tutorial lbl',IER)
        CALL EGETP(OUTSTR,K,tutexe,'W','tutorial exe',IER)
        write(outs,'(a,a,2x,a)')' Tutorial : ',tutlbl,
     &    tutexe(1:lnblnk(tutexe))
        if(itrc.gt.0)call edisp(iuout,outs)
        call captut(tutexe)
      elseif(WORD(1:8).eq.'*journal')then

C Turn journaling on/off.
        CALL EGETW(OUTSTR,K,journcmd,'W','on/off',IFLAG)
        if(journcmd(1:2).eq.'ON'.or.journcmd(1:2).eq.'on')then
          journio=1
        else
          journio=0
        endif
      elseif(WORD(1:14).eq.'*image_display')then
        imgtyp=imgtyp+1
        CALL EGETP(OUTSTR,K,fmttag(imgtyp),'W','image fmt',IER)
        CALL EGETP(OUTSTR,K,fmtexe(imgtyp),'W','img exe',IER)
        write(outs,'(4a)')' Images : ',fmttag(imgtyp),' via ',
     &    fmtexe(imgtyp)
        if(itrc.gt.0)call edisp(iuout,outs)
      elseif(WORD(1:7).eq.'*editor')then
        CALL EGETP(OUTSTR,K,tedlbl,'W','manual editor lbl',IER)
        CALL EGETP(OUTSTR,K,teditor,'W','editor exe',IER)
        write(outs,'(4a)')' Editor: ',tedlbl,' via ',teditor
        if(itrc.gt.0)call edisp(iuout,outs)
      elseif(WORD(1:11).eq.'*docpreview')then
        CALL EGETP(OUTSTR,K,tprevlbl,'W','document previewer lbl',IER)
        CALL EGETP(OUTSTR,K,tpreview,'W','doc previewer exe',IER)
        write(outs,'(4a)')' Preview: ',tprevlbl,' via ',tpreview
        if(itrc.gt.0)call edisp(iuout,outs)
      elseif(WORD(1:7).eq.'*figure')then
        CALL EGETP(OUTSTR,K,figlbl,'W','figure editor lbl',IER)
        CALL EGETP(OUTSTR,K,figexe,'W','figure editor exe',IER)
        write(outs,'(4a)')' Preview: ',figlbl,' via ',figexe
        if(itrc.gt.0)call edisp(iuout,outs)
      elseif(WORD(1:9).eq.'*graphing')then
        CALL EGETP(OUTSTR,K,grflbl,'W','graphing tool lbl',IER)
        CALL EGETP(OUTSTR,K,grfexe,'W','graphing tool exe',IER)
        write(outs,'(4a)')' Graphing: ',grflbl,' via ',grfexe
        if(itrc.gt.0)call edisp(iuout,outs)
      elseif(WORD(1:9).eq.'*folderbr')then
        CALL EGETP(OUTSTR,K,foldlbl,'W','folder br tool lbl',IER)
        CALL EGETP(OUTSTR,K,foldexe,'W','folder tool exe',IER)
        write(outs,'(4a)')' Folder browsing: ',foldlbl,' via ',foldexe
        if(itrc.gt.0)call edisp(iuout,outs)
      elseif(WORD(1:10).eq.'*exemplars')then
        CALL EGETP(OUTSTR,K,exemlbl,'W','exemplars list lbl',IER)
        CALL EGETP(OUTSTR,K,exemfl,'W','exemplars file',IER)
        write(outs,'(4a)')' Exemplars: ',exemlbl,' in ',exemfl
        if(itrc.gt.0)call edisp(iuout,outs)
      elseif(WORD(1:12).eq.'*db_defaults')then
        CALL EGETP(OUTSTR,K,dfdblbl,'W','def db list lbl',IER)
        CALL EGETP(OUTSTR,K,defdbfl,'W','def db file',IER)
        write(outs,'(4a)')' Default: ',dfdblbl,' in ',defdbfl
        if(itrc.gt.0)call edisp(iuout,outs)
      elseif(WORD(1:16).eq.'*validation_stds')then
        CALL EGETP(OUTSTR,K,vldlbl,'W','validation list lbl',IER)
        CALL EGETP(OUTSTR,K,vldfl,'W','validation stds file',IER)
        write(outs,'(4a)')' Validation: ',vldlbl,' in ',vldfl
        if(itrc.gt.0)call edisp(iuout,outs)
      elseif(WORD(1:12).eq.'*db_climates')then
        CALL EGETP(OUTSTR,K,cdblbl,'W','clm db list lbl',IER)
        CALL EGETP(OUTSTR,K,cdblfil,'W','clm db list file',IER)
        write(outs,'(4a)')' Climate list: ',cdblbl,' in ',cdblfil
        if(itrc.gt.0)call edisp(iuout,outs)
      elseif(WORD(1:11).eq.'*report_gen')then
        CALL EGETP(OUTSTR,K,replbl,'W','report gen lbl',IER)
        CALL EGETP(OUTSTR,K,repexe,'W','report gen exe',IER)
        write(outs,'(4a)')' Reports: ',replbl,' via ',repexe
        if(itrc.gt.0)call edisp(iuout,outs)
      elseif(WORD(1:5).eq.'*year')then
      elseif(WORD(1:4).eq.'*end')then
        goto 45
      else
        call usrmsg(' Unknown dot command in... ',outstr,'W')
        goto 44
      endif

C Loop back for next command.
      goto 44

C Close file before exiting.
   45 CONTINUE
      if(localdot)then
        write(outs,'(3a)') 'Scanning ',
     &         LFDEF(1:lnblnk(LFDEF)),' definitions.'
        if(itrc.gt.1)CALL USRMSG(' ',OUTS,'-')
      else
        write(outs,'(3a)') 'Standard settings in: ',
     &         altdot(1:lnblnk(altdot)),' successfully scanned...'
        if(itrc.gt.1)CALL USRMSG(' ',OUTS,'-')
      endif
      CALL ERPFREE(IFDEF,ISTAT)
      RETURN
      END

C ******************** ESCDEF 
C ESCDEF scans the ESP-r defaults definition file and returns
C the default file and database names via common block.
C It uses espinstalldir.h which was created by the Install
C script with the current esp-r install directory held
C in a string instpath.
      SUBROUTINE ESCDEF(dfdblbl,defdbfl,IFDEF,IER)
#include "esprdbfile.h"
      COMMON/OUTIN/IUOUT,IUIN

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DMCMPDBFL,DICONDBFL,dmdbnam
      common/deflt4/dinstpath
      CHARACTER OUTSTR*124,WORD*20,outs*124
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF,DFCFD
      character*72 DECMPDBFL,DMCMPDBFL,DICONDBFL,dmdbnam
      character dfdblbl*20,defdbfl*72,dinstpath*48,di*48
      logical unixok,dll
#include "espinstalldir.h"

C Assume default db names (in case of failure to find defaults
C file or failure reading it).
      call isunix(unixok)
      if(unixok)then
        write(dinstpath,'(a)') instpath(1:lnblnk(instpath))
        write(di,'(a)') instpath(1:lnblnk(instpath))
        ln=lnblnk(di)
        write(DFCFG,'(2a)') di(1:ln),
     &    '/training/3z_bld/cfg/building_basic'
        write(DFCTL,'(2a)') di(1:ln),
     &    '/training/3z_bld/ctl/building.ctl'
        write(DAPROB,'(2a)') di(1:ln),
     &    'training/basic/networks/bld_basic_af1.afn'
        write(DPNF,'(2a)') di(1:ln),
     &    '/training/plant/vent_simple/cfg/vent.cfg'
        write(DFCFD,'(2a)') di(1:ln),
     &    '/training/cfd/template.dfd'
        write(DEFRLB,'(2a)') di(1:ln),'/databases/test.res'
        write(DAFRES,'(2a)') di(1:ln),'/databases/test.mfr'
        write(DCLIM,'(2a)') di(1:ln),'/climate/clm67'
        write(DAPRES,'(2a)') di(1:ln),'/databases/pressc.db1'
        write(DFCON,'(2a)') di(1:ln),'/databases/constr.db2'
        write(DFMUL,'(2a)') di(1:ln),'/databases/multicon.db2'
        write(DOPTDB,'(2a)') di(1:ln),'/databases/optics.db2'
        write(DPRFDB,'(2a)') di(1:ln),'/databases/profiles.db1'
        write(DPCDB,'(2a)') di(1:ln),'/databases/plantc.db1'
        write(DECMPDBFL,'(2a)') di(1:ln),'/databases/elcomp.db1'
        write(DMCMPDBFL,'(2a)') di(1:ln),'/databases/mscomp.db1'
        write(DICONDBFL,'(2a)') di(1:ln),'/databases/icons.db1'
        write(dmdbnam,'(2a)') di(1:ln),'/databases/mould.db1'
      else
C        dinstpath = 'c:/esru/esp-r'
        write(dinstpath,'(a)') instpath(1:lnblnk(instpath))
        write(di,'(a)') instpath(1:lnblnk(instpath))
        ln=lnblnk(di)
        write(DFCFG,'(2a)') di(1:ln),
     &    '/training/3z_bld/cfg/building_basic'
        write(DFCTL,'(2a)') di(1:ln),
     &    '/training/3z_bld/ctl/building.ctl'
        write(DAPROB,'(2a)') di(1:ln),
     &    'training/basic/networks/bld_basic_af1.afn'
        write(DPNF,'(2a)') di(1:ln),
     &    '/training/plant/vent_simple/cfg/vent.cfg'
        write(DFCFD,'(2a)') di(1:ln),
     &    '/training/cfd/template.dfd'
        write(DEFRLB,'(2a)') di(1:ln),'/databases/test.res'
        write(DAFRES,'(2a)') di(1:ln),'/databases/test.mfr'
        write(DCLIM,'(2a)') di(1:ln),'/climate/clm67'
        write(DAPRES,'(2a)') di(1:ln),'/databases/pressc.db1'
        write(DFCON,'(2a)') di(1:ln),'/databases/constr.db2'
        write(DFMUL,'(2a)') di(1:ln),'/databases/multicon.db2'
        write(DOPTDB,'(2a)') di(1:ln),'/databases/optics.db2'
        write(DPRFDB,'(2a)') di(1:ln),'/databases/profiles.db1'
        write(DPCDB,'(2a)') di(1:ln),'/databases/plantc.db1'
        write(DECMPDBFL,'(2a)') di(1:ln),'/databases/elcomp.db1'
        write(DMCMPDBFL,'(2a)') di(1:ln),'/databases/mscomp.db1'
        write(DICONDBFL,'(2a)') di(1:ln),'/databases/icons.db1'
        write(dmdbnam,'(2a)') di(1:ln),'/databases/mould.db1'
      endif

C Check if running in dll mode, if so skip reading defaults file.
      call isadll(dll)
      if(dll)then
        return
      endif

C Open the defaults file, return if not found.
      IER=0
      CALL EFOPSEQ(IFDEF,defdbfl,1,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'The file ',defdbfl(1:lnblnk(defdbfl)),
     &   ' was not found (assuming standard databases).'
        call edisp(IUOUT,outs)
        IER=1
        RETURN
      ENDIF

C Read '*ESP-r Defaults' tag.
      CALL STRIPC(IFDEF,OUTSTR,0,ND,1,'default tag',IER)
      if(OUTSTR(1:15).ne.'*ESP-r Defaults')then
        write(outs,'(3a)') 'The file ',defdbfl(1:lnblnk(defdbfl)),
     &   ' is not a defaults file.'
        call edisp(IUOUT,outs)
        IER=1
        RETURN
      endif

C If '*ESP-r Defaults' found then read db until '*end' found.
  44  continue
      CALL STRIPC(IFDEF,OUTSTR,0,ND,1,'def names',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','def names',IFLAG)
      if(WORD(1:5).eq.'*ipth')then
        CALL EGETRM(OUTSTR,K,dinstpath,'W','instal folder',IER)
      elseif(WORD(1:4).eq.'*cfg')then
        CALL EGETRM(OUTSTR,K,DFCFG,'W','def config',IER)
      elseif(WORD(1:4).eq.'*ctl')then
        CALL EGETRM(OUTSTR,K,DFCTL,'W','def control',IER)
      elseif(WORD(1:4).eq.'*mfn')then
        CALL EGETRM(OUTSTR,K,DAPROB,'W','def mass flow net',IER)
      elseif(WORD(1:4).eq.'*pnf')then
        CALL EGETRM(OUTSTR,K,DPNF,'W','def mass flow net',IER)
      elseif(WORD(1:4).eq.'*dfd')then
        CALL EGETRM(OUTSTR,K,DFCFD,'W','def domain flow definit.',
     &       IER)
      elseif(WORD(1:4).eq.'*res')then
        CALL EGETRM(OUTSTR,K,DEFRLB,'W','def results lib',IER)
      elseif(WORD(1:4).eq.'*mfr')then
        CALL EGETRM(OUTSTR,K,DAFRES,'W','def results lib',IER)
      elseif(WORD(1:4).eq.'*prm')then
        CALL EGETRM(OUTSTR,K,DFCON,'W','def material db',IER)
      elseif(WORD(1:4).eq.'*mlc')then
        CALL EGETRM(OUTSTR,K,DFMUL,'W','def multilayer db',IER)
      elseif(WORD(1:4).eq.'*opt')then
        CALL EGETRM(OUTSTR,K,DOPTDB,'W','def optical db',IER)
      elseif(WORD(1:4).eq.'*prs')then
        CALL EGETRM(OUTSTR,K,DAPRES,'W','def pressure db',IER)
      elseif(WORD(1:4).eq.'*evn')then
        CALL EGETRM(OUTSTR,K,DPRFDB,'W','def profiles db',IER)
      elseif(WORD(1:4).eq.'*clm')then
        CALL EGETRM(OUTSTR,K,DCLIM,'W','def climate db',IER)
      elseif(WORD(1:4).eq.'*pdb')then
        CALL EGETRM(OUTSTR,K,DPCDB,'W','def plant comp db',IER)
      elseif(WORD(1:5).eq.'*ecdb')then
        CALL EGETRM(OUTSTR,K,DECMPDBFL,'W','def elec comp db',IER)
      elseif(WORD(1:5).eq.'*mcdb')then
        CALL EGETRM(OUTSTR,K,DMCMPDBFL,'W','def misc comp db',IER)
      elseif(WORD(1:5).eq.'*icdb')then
        CALL EGETRM(OUTSTR,K,DICONDBFL,'W','default icons',IER)
      elseif(WORD(1:5).eq.'*mldb')then
        CALL EGETRM(OUTSTR,K,dmdbnam,'W','default mycotoxin db',IER)

      elseif(WORD(1:4).eq.'*end')then
        goto 45
      else
        call usrmsg(' Unknown default in... ',outstr,'W')
        goto 44
      endif

      if(ier.ne.0)then
        write(outs,'(3a)') 'Problem detected in ',
     &    defdbfl(:lnblnk(defdbfl)),' while reading...'
        CALL EDISP(IUOUT,outs)
        CALL EDISP(IUOUT,outstr)
        goto 45
      endif

C Loop back for next default file.
      goto 44

C Close defaults file before exiting.
   45 CONTINUE
      CALL ERPFREE(IFDEF,ISTAT)
      RETURN
      END

C Parse command lines.

C ********* parpsf (equivalent to startpsf.c)
C  Allowing terminal, size and config file arguments
C  to be passed to the program from the invocation line.  

      subroutine parpsf(termtype,iappw,iappx,iappy,inf)
      COMMON/OUTIN/IUOUT,IUIN

      integer termtype,iappw,iappx,iappy
      character*48 prog
      integer iargc,m
      character argument*72,mode*8,inf*144,appn*24,outs*248
      integer iXavail

C Determine if an X graphics library (GTK or X11) was linked
C into binary, and set default terminal type as necessary

      if ( iXavail() == 1 ) then 
        termtype = 8
        mode = 'graphic'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      else
        termtype = -1
        mode = 'text'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      endif

      iappw = 0
      iappx = 0
      iappy = 0
      inf  = 'UNKNOWN'
      argument = ' '

C Get arguments from command line: if one parameter.
      if(m.eq.0)then
        write(outs,'(6a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf))
C        call edisp248(iuout,outs,90)
        return
      elseif(m.ge.1)then
  41    i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)

        if(argument(1:5).eq.'-help')then
          call parpsfh(appn)
          stop
        elseif(argument(1:5).eq.'-mode')then
          i=i+1
          call getarg(i,argument)
          if(argument(1:4).eq.'text')then
            termtype = -1
            mode = 'text'
          elseif(argument(1:4).eq.'page')then
            termtype = -2
            mode = 'page'
          elseif(argument(1:5).eq.'graph')then
            termtype = 8
            mode = 'graphic'
          elseif(argument(1:6).eq.'script')then
            termtype = -6
            mode = 'script'
          endif
        elseif(argument(1:2).eq.'-s')then
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappw
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappx
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappy
        elseif(argument(1:5).eq.'-file')then
          i=i+1
          call getarg(i,inf)
        endif
        goto 41

  42    continue
        write(outs,'(6a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf))
C        call edisp248(iuout,outs,90)
        return
      endif

C Errors.
   2  write(6,'(a)') 'parpsf: error extracting parameters.'
      return
      end

C ********* parpsff
C  parpsff supports terminal, size and two file arguments
C  to be passed to the program from the invocation line.  

      subroutine parpsff(termtype,iappw,iappx,iappy,infa,infb)
      COMMON/OUTIN/IUOUT,IUIN

      integer termtype,iappw,iappx,iappy
      character*48 prog
      integer iargc,m
      character argument*72,mode*8,infa*72,infb*72,appn*24,outs*124

C NT or Unix version.
C Determine if an X graphics library (GTK or X11) was linked
C into binary, and set default terminal type as necessary

      if ( iXavail() == 1 ) then 
        termtype = 8
        mode = 'graphic'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      else
        termtype = -1
        mode = 'text'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      endif
      
      iappw = 0
      iappx = 0
      iappy = 0
      infa  = 'UNKNOWN'
      infb  = 'UNKNOWN'
      argument = ' '

C Get arguments from command line: if one parameter.
      if(m.eq.0)then
        write(outs,'(8a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with files ',
     &    infa(1:lnblnk(infa)),' & ',infb(1:lnblnk(infb))
C        call edisp(iuout,outs)
        return
      elseif(m.ge.1)then
  41    i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)

        if(argument(1:5).eq.'-help')then
          call parpsfh(appn)
          stop
        elseif(argument(1:5).eq.'-mode')then
          i=i+1
          call getarg(i,argument)
          if(argument(1:4).eq.'text')then
            termtype = -1
            mode = 'text'
          elseif(argument(1:4).eq.'page')then
            termtype = -2
            mode = 'page'
          elseif(argument(1:5).eq.'graph')then
            termtype = 8
            mode = 'graphic'
          elseif(argument(1:6).eq.'script')then
            termtype = -6
            mode = 'script'
          endif
        elseif(argument(1:2).eq.'-s')then
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappw
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappx
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappy
        elseif(argument(1:5).eq.'-file')then
          i=i+1
          call getarg(i,infa)
          i=i+1
          call getarg(i,infb)
        endif
        goto 41

  42    continue
        write(outs,'(8a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with files ',
     &    infa(1:lnblnk(infa)),' & ',infb(1:lnblnk(infb))
C        call edisp(iuout,outs)
        return
      endif

C Errors.
   2  write(6,'(a)') 'parpsff: error extracting parameters.'
      return
      end

C ****** parpsfh
C Help message for parpsf.
      subroutine parpsfh(appn)
      COMMON/OUTIN/IUOUT,IUIN
      character appn*24

      if(appn(1:3).eq.'bld'.or.appn(1:3).eq.'bps')then
        call edisp(iuout,'This is the ESP-r Simulator. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <configuration file>]')
        call edisp(iuout,'   [-p <param_set_name> <how>]')
        call edisp(iuout,'    (where how {interactive|silent} defines')
        call edisp(iuout,'     whether the user interacts')
        call edisp(iuout,'   [-actf file providing bems commands')
        call edisp(iuout,'   [-s < width, offset left & top>]')
        call edisp(iuout,'   [-b {no|yes}]')
        call edisp(iuout,' ')
        call edisp(iuout,'  where -mode options are:')
        call edisp(iuout,'    text    = text mode')
        call edisp(iuout,'    graphic = graphic mode')
        call edisp(iuout,'    script = script mode')
        call edisp(iuout,'  where -b (browse options) are used to')
        call edisp(iuout,'    indicate that the user has requested')
        call edisp(iuout,'    a simulation for a model they are')
        call edisp(iuout,'    browsing (in which case the results')
        call edisp(iuout,'    will be placed in the users home')
        call edisp(iuout,'    folder. Default is no.')
        call edisp(iuout,' ')
        call edisp(iuout,' -help :this help message. ')
        return
      elseif(appn(1:3).eq.'clm')then
        call edisp(iuout,'The climate analysis module. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <climate db>]')
        call edisp(iuout,'   [-act <action> <how> <file>]')
        call edisp(iuout,'  (where action {asci2bin|bin2asci|epw2bin})')
        call edisp(iuout,'  (where how {interactive|silent} defines ')
        call edisp(iuout,'   whether the user interacts')
        call edisp(iuout,'  (where file is asci input or output file)')
        call edisp(iuout,'   [-s < width, offset left & top>]')
        call edisp(iuout,' ')
        call edisp(iuout,'  where -mode options are:')
        call edisp(iuout,'    text    = text mode')
        call edisp(iuout,'    graphic = graphic mode')
        call edisp(iuout,'    script = script mode')
        call edisp(iuout,' ')
        call edisp(iuout,'Example converting binary file to asci: ')
        call edisp(iuout,
     &    ' clm -file current.clm -act bin2asci silent current.asci')
        call edisp(iuout,' ')
        call edisp(iuout,'Example converting asci file to binary: ')
        call edisp(iuout,
     &    ' clm -file new.clm -act asci2bin silent current.asci')
        call edisp(iuout,' ')
        call edisp(iuout,
     &    'Example converting (an edited) EPW file to binary:')
        call edisp(iuout,
     &    ' clm -file new.clm -act epw2bin silent current.epw')
        call edisp(iuout,' ')
        call edisp(iuout,' -help :this help message. ')
        return
      elseif(appn(1:3).eq.'cfg')then
        call edisp(iuout,'The topology checking module. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <configuration file>]')
      elseif(appn(1:3).eq.'dfs')then
        call edisp(iuout,'The stand-alone domain flow module. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <configuration file>]')
      elseif(appn(1:3).eq.'e2r')then
        call edisp(iuout,'The visualisation module. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <configuration file>]')
        call edisp(iuout,'       [-zone <focus zone|index>]')
        call edisp(iuout,'       [-purpose <scene purpose>]')
      elseif(appn(1:3).eq.'grd')then
        call edisp(iuout,'The 2-D and 3D gridding module.')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <configuration file>]')
        call edisp(iuout,'       [-zone <name|index>]')
      elseif(appn(1:3).eq.'ish')then
        call edisp(iuout,'The shading & insolation database module.')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <configuration file>]')
        call edisp(iuout,'       [-zone <name|index>]')
        call edisp(iuout,'       [-s < width, offset left & top>]')
        call edisp(iuout,'       [-act {recalculate|update_silent}]')
        call edisp(iuout,' ')
        call edisp(iuout,'  where -mode options are:')
        call edisp(iuout,'    text    = text mode ')
        call edisp(iuout,'    graphic = graphic mode')
        call edisp(iuout,'    script = script mode')
        call edisp(iuout,' ')
        call edisp(iuout,'  where -act recalculate')
        call edisp(iuout,'    interactively recalculates shading.')
        call edisp(iuout,'  where -act update_silent')
        call edisp(iuout,'    no usr interaction during recalculation.')
        call edisp(iuout,' ')
        call edisp(iuout,' -help :this help message. ')
        return
      elseif(appn(1:3).eq.'mfs')then
        call edisp(iuout,'The stand-alone network flow solver. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
      elseif(appn(1:3).eq.'mrt')then
        call edisp(iuout,'The viewfactor calculation module.')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <configuration file>]')
        call edisp(iuout,'       [-zone <name|index>]')
      elseif(appn(1:3).eq.'pdb')then
        call edisp(iuout,'The plant components database manager. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <plant db>]')
      elseif(appn(1:3).eq.'pdf')then
        call edisp(iuout,'The plant network definitions. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
      elseif(appn(1:3).eq.'pfs')then
        call edisp(iuout,'This is the power flow simulator. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <configuration file>]')
      elseif(appn(1:3).eq.'plt')then
        call edisp(iuout,'The stand-alone plant module. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <plant network file>]')
      elseif(appn(1:3).eq.'prj')then
        call edisp(iuout,'The project manager.')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <configuration file>]')
        call edisp(iuout,'       [-zone <name|index>]')
      elseif(appn(1:3).eq.'pro')then
        call edisp(iuout,'The event profiles database manager. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <profiles db>]')
      elseif(appn(1:3).eq.'res')then
        call edisp(iuout,'This is the ESP-r analysis module. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <results file>]')
        call edisp(iuout,'   [-act <action, {interactive|silent}>]')
        call edisp(iuout,'  (where action {ipv_win|ipv_trn|ipv_sum|)')
        call edisp(iuout,'                 ipv_ann|BEMS|recover})')
        call edisp(iuout,'  (where default is interactive mode and')
        call edisp(iuout,'   silent is automatic execution)')
        call edisp(iuout,'   [-actf file providing bems commands or')
        call edisp(iuout,'   recovery commands]')
        call edisp(iuout,'   [-s < width, offset left & top>]')
        call edisp(iuout,' ')
        call edisp(iuout,'  where -mode options are:')
        call edisp(iuout,'    text    = text mode')
        call edisp(iuout,'    graphic = graphic mode')
        call edisp(iuout,'    script = script mode')
        call edisp(iuout,' ')
        call edisp(iuout,' -help :this help message. ')
        return
      elseif(appn(1:3).eq.'tdf')then
        call edisp(iuout,'The temporal database manager. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <database file>]')
        call edisp(iuout,'       [-act {update|extract|export|}')
        call edisp(iuout,'              import}}')
        call edisp(iuout,'       [-actf file as follows:')
        call edisp(iuout,' if act = update, actf gives commands')
        call edisp(iuout,' if act = extract, actf gives commands')
        call edisp(iuout,' if act = export, actf holds exported data')
        call edisp(iuout,' if act = import, actf is data to import')
        call edisp(iuout,'       [-s < width, offset left & top>]')
      elseif(appn(1:3).eq.'vew')then
        call edisp(iuout,'The hidden line viewer. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <viewer_file>]')
      endif
      call edisp(iuout,'       [-s < width, offset left & top>]')
      call edisp(iuout,' ')
      call edisp(iuout,'  where -mode options are:')
      call edisp(iuout,'    text    = text mode ')
      call edisp(iuout,'    graphic = graphic mode')
      call edisp(iuout,'    script = script mode')
      call edisp(iuout,' ')
      call edisp(iuout,' -help :this help message. ')
      return
      end

C ********* parsim
C  Allowing terminal, size, config file, simulation parameter file arguments
C  to be passed to the program from the invocation line.  

      subroutine parsim(termtype,iappw,iappx,iappy,inf,browse,sps,
     &  aut,actf)
      COMMON/OUTIN/IUOUT,IUIN

C Timeout information
      common/TIMEOUT/sTimeout,iTimeoutFlag

      character sTimeout*72  
      integer iTimeoutFlag
      integer termtype,iappw,iappx,iappy
      character*48 prog
      integer iargc,m
      character argument*72,mode*8,inf*144,appn*24
      character outs*248,sps*8,aut*12,actf*72,browse*4


C Determine if an X graphics library (GTK or X11) was linked
C into binary, and set default terminal type as necessary

      if ( iXavail() == 1 ) then 
        termtype = 8
        mode = 'graphic'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      else
        termtype = -1
        mode = 'text'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      endif

      iappw = 0
      iappx = 0
      iappy = 0
      browse = 'no'
      inf  = 'UNKNOWN'
      sps  = 'UNKNOWN'
      aut  = 'default'
      actf  = 'UNKNOWN'
      argument = ' '

C Get arguments from command line: if one parameter.
      if(m.eq.0)then
        write(outs,'(6a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf))
C        call edisp(iuout,outs)
        write(outs,'(a,a)') 'and parameters ',sps(1:lnblnk(sps))
C        call edisp(iuout,outs)
        return
      elseif(m.ge.1)then
  41    i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)

        if(argument(1:5).eq.'-help')then
          call parpsfh(appn)
          stop
        elseif(argument(1:5).eq.'-mode')then
          i=i+1
          call getarg(i,argument)
          if(argument(1:4).eq.'text')then
            termtype = -1
            mode = 'text'
          elseif(argument(1:4).eq.'page')then
            termtype = -2
            mode = 'page'
          elseif(argument(1:5).eq.'graph')then
            termtype = 8
            mode = 'graphic'
          elseif(argument(1:6).eq.'script')then
            termtype = -6
            mode = 'script'
          endif
        elseif(argument(1:2).eq.'-b')then
          i=i+1
          call getarg(i,browse)
        elseif(argument(1:2).eq.'-s')then
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappw
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappx
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappy
        elseif(argument(1:5).eq.'-file')then
          i=i+1
          call getarg(i,inf)
        elseif(argument(1:2).eq.'-p')then
          i=i+1
          call getarg(i,sps)
          i=i+1
          call getarg(i,aut)
        elseif(argument(1:5).eq.'-actf')then
          i=i+1
          call getarg(i,actf)
        elseif(argument(1:8).eq.'-timeout')then
          i=i+1
          call getarg(i,sTimeout)
          iTimeoutFlag = 1          
        endif
        goto 41

  42    continue
        outs=' '
        write(outs,'(6a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf))
C        call edisp(iuout,outs)
        outs=' '
        write(outs,'(7a)') 'and parameters ',sps(1:lnblnk(sps)),
     &   ' in ',aut,' mode and ',browse,' browsing.'
C        call edisp(iuout,outs)
        return
      endif

C Errors.
   2  write(6,'(a)') 'parpsf: error extracting parameters.'
      return
      end

C ********* parres
C  Allowing terminal, size, results file, action and silent arguments
C  to be passed to res from the invocation line.  

      subroutine parres(termtype,iappw,iappx,iappy,inf,act,aut,actf)
      COMMON/OUTIN/IUOUT,IUIN

      integer termtype,iappw,iappx,iappy
      character*48 prog
      integer iargc,m
      character argument*72,mode*8,inf*144,appn*24
      character outs*248,actf*72,act*16,aut*12


C Determine if an X graphics library (GTK or X11) was linked
C into binary, and set default terminal type as necessary

      if ( iXavail() == 1 ) then 
        termtype = 8
        mode = 'graphic'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      else
        termtype = -1
        mode = 'text'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      endif

      iappw = 0
      iappx = 0
      iappy = 0
      inf  = 'UNKNOWN'
      act  = 'UNKNOWN'
      aut  = 'interactive'
      actf  = 'UNKNOWN'
      argument = ' '

C Get arguments from command line: if one parameter.
      if(m.eq.0)then
        write(outs,'(6a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf))
C        call edisp248(iuout,outs,90)
        return
      elseif(m.ge.1)then
 41     i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)

        if(argument(1:5).eq.'-help')then
          call parpsfh(appn)
          stop
        elseif(argument(1:5).eq.'-mode')then
          i=i+1
          call getarg(i,argument)
          if(argument(1:4).eq.'text')then
            termtype = -1
            mode = 'text'
          elseif(argument(1:4).eq.'page')then
            termtype = -2
            mode = 'page'
          elseif(argument(1:5).eq.'graph')then
            termtype = 8
            mode = 'graphic'
          elseif(argument(1:6).eq.'script')then
            termtype = -6
            mode = 'script'
          endif
        elseif(argument(1:2).eq.'-s')then
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappw
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappx
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappy
        elseif(argument(1:5).eq.'-file')then
          i=i+1
          call getarg(i,inf)
        elseif(argument(1:5).eq.'-actf')then
          i=i+1
          call getarg(i,actf)
        elseif(argument(1:4).eq.'-act')then
          i=i+1
          call getarg(i,act)
          i=i+1
          call getarg(i,aut)
        endif
        goto 41

  42    continue
        outs=' '
        write(outs,'(11a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf)),' and action ',act(1:lnblnk(act)),
     &   ' in ',aut,' mode.'
C        call edisp248(iuout,outs,90)
        return
      endif

C Errors.
   2  write(6,'(a)') 'parres: error extracting parameters.'
      return
      end

C ********* parclm
C  Allowing terminal, size, input file, action and silent arguments
C  and output (converted) file to be passed to clm from the invocation line.  

      subroutine parclm(termtype,iappw,iappx,iappy,inf,act,aut,ascif)
      COMMON/OUTIN/IUOUT,IUIN

      integer termtype,iappw,iappx,iappy
      character*48 prog
      integer iargc,m
      character argument*72,mode*8,inf*144,appn*24
      character outs*248,ascif*72,act*16,aut*12


C Determine if an X graphics library (GTK or X11) was linked
C into binary, and set default terminal type as necessary

      if ( iXavail() == 1 ) then 
        termtype = 8
        mode = 'graphic'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      else
        termtype = -1
        mode = 'text'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      endif
      
      iappw = 0
      iappx = 0
      iappy = 0
      inf  = 'UNKNOWN'
      act = 'NONE'
      aut  = 'interactive'
      ascif  = 'UNKNOWN'
      argument = ' '

C Get arguments from command line: if one parameter.
      if(m.eq.0)then
        write(outs,'(6a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf))
C        call edisp248(iuout,outs,90)
        return
      elseif(m.ge.1)then
 41     i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)

        if(argument(1:5).eq.'-help')then
          call parpsfh(appn)
          stop
        elseif(argument(1:5).eq.'-mode')then
          i=i+1
          call getarg(i,argument)
          if(argument(1:4).eq.'text')then
            termtype = -1
            mode = 'text'
          elseif(argument(1:4).eq.'page')then
            termtype = -2
            mode = 'page'
          elseif(argument(1:5).eq.'graph')then
            termtype = 8
            mode = 'graphic'
          elseif(argument(1:6).eq.'script')then
            termtype = -6
            mode = 'script'
          endif
        elseif(argument(1:2).eq.'-s')then
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappw
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappx
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappy
        elseif(argument(1:5).eq.'-file')then
          i=i+1
          call getarg(i,inf)
        elseif(argument(1:4).eq.'-act')then
          i=i+1
          call getarg(i,act)
          i=i+1
          call getarg(i,aut)
          i=i+1
          call getarg(i,ascif)
        endif
        goto 41

  42    continue
        outs=' '
        write(outs,'(6a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf))
C        call edisp(iuout,outs)
        outs=' '
        write(outs,'(5a)') 'and action ',act(1:lnblnk(act)),
     &   ' in ',aut,' mode.'
C        call edisp(iuout,outs)
        write(outs,'(2a)') 'using/making file ',ascif(1:lnblnk(ascif))
C        call edisp(iuout,outs)
        return
      endif

C Errors.
   2  write(6,'(a)') 'parclm: error extracting parameters.'
      return
      end


C ********* parsfza (equivalent to startsfza.c)
C  Allowing terminal, size, config file, zone focus and actions arguments
C  to be passed to the program from the invocation line.  

      subroutine parsfza(termtype,iappw,iappx,iappy,inf,zone,act)
      COMMON/OUTIN/IUOUT,IUIN

      integer termtype,iappw,iappx,iappy
      character*48 prog
      integer iargc,m
      character argument*72,mode*8,inf*144,appn*24
      character outs*248,zone*16,act*16


C Determine if an X graphics library (GTK or X11) was linked
C into binary, and set default terminal type as necessary

      if ( iXavail() == 1 ) then 
        termtype = 8
        mode = 'graphic'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      else
        termtype = -1
        mode = 'text'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      endif
      
      iappw = 0
      iappx = 0
      iappy = 0
      inf  = 'UNKNOWN'
      zone = 'ALL'
      act = 'NONE'
      argument = ' '

C Get arguments from command line: if one parameter.
      if(m.eq.0)then
        write(outs,'(6a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf))
C        call edisp(iuout,outs)
        write(outs,'(a,a)') 'focused on zone ',zone(1:lnblnk(zone))
C        call edisp(iuout,outs)
        write(outs,'(a,a)') 'with action ',act(1:lnblnk(act))
C        call edisp(iuout,outs)
        return
      elseif(m.ge.1)then
  41    i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)

        if(argument(1:5).eq.'-help')then
          call parpsfh(appn)
          stop
        elseif(argument(1:5).eq.'-mode')then
          i=i+1
          call getarg(i,argument)
          if(argument(1:4).eq.'text')then
            termtype = -1
            mode = 'text'
          elseif(argument(1:4).eq.'page')then
            termtype = -2
            mode = 'page'
          elseif(argument(1:5).eq.'graph')then
            termtype = 8
            mode = 'graphic'
          elseif(argument(1:6).eq.'script')then
            termtype = -6
            mode = 'script'
          endif
        elseif(argument(1:2).eq.'-s')then
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappw
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappx
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappy
        elseif(argument(1:5).eq.'-file')then
          i=i+1
          call getarg(i,inf)
        elseif(argument(1:5).eq.'-zone')then
          i=i+1
          call getarg(i,zone)
        elseif(argument(1:4).eq.'-act')then
          i=i+1
          call getarg(i,act)
        endif
        goto 41

  42    continue
        write(outs,'(10a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf)),' focused on zone ',zone(1:lnblnk(zone)),
     &    ' with action ',act(1:lnblnk(act))
C        call edisp248(iuout,outs,90)
        return
      endif

C Errors.
   2  write(6,'(a)') 'parsfza: error extracting parameters.'
      return
      end

C ********* parsfz (equivalent to startsfz.c)
C  Allowing terminal, size, config file, zone focus arguments
C  to be passed to the program from the invocation line.  

      subroutine parsfz(termtype,iappw,iappx,iappy,inf,zone)
      COMMON/OUTIN/IUOUT,IUIN

      integer termtype,iappw,iappx,iappy
      character*48 prog
      integer iargc,m
      character argument*72,mode*8,inf*144,appn*24,outs*248,zone*16


C Determine if an X graphics library (GTK or X11) was linked
C into binary, and set default terminal type as necessary

      if ( iXavail() == 1 ) then 
        termtype = 8
        mode = 'graphic'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      else
        termtype = -1
        mode = 'text'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      endif
      
      iappw = 0
      iappx = 0
      iappy = 0
      inf  = 'UNKNOWN'
      zone = 'ALL'
      argument = ' '

C Get arguments from command line: if one parameter.
      if(m.eq.0)then
        write(outs,'(8a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf)),' focused on zone ',zone(1:lnblnk(zone))
C        call edisp248(iuout,outs,90)
        return
      elseif(m.ge.1)then
  41    i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)

        if(argument(1:5).eq.'-help')then
          call parpsfh(appn)
          stop
        elseif(argument(1:5).eq.'-mode')then
          i=i+1
          call getarg(i,argument)
          if(argument(1:4).eq.'text')then
            termtype = -1
            mode = 'text'
          elseif(argument(1:4).eq.'page')then
            termtype = -2
            mode = 'page'
          elseif(argument(1:5).eq.'graph')then
            termtype = 8
            mode = 'graphic'
          elseif(argument(1:6).eq.'script')then
            termtype = -6
            mode = 'script'
          endif
        elseif(argument(1:2).eq.'-s')then
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappw
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappx
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappy
        elseif(argument(1:5).eq.'-file')then
          i=i+1
          call getarg(i,inf)
        elseif(argument(1:5).eq.'-zone')then
          i=i+1
          call getarg(i,zone)
        endif
        goto 41

  42    continue
        write(outs,'(8a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf)),' focused on zone ',zone(1:lnblnk(zone))
C        call edisp248(iuout,outs,90)
        return
      endif

C Errors.
   2  write(6,'(a)') 'parsfz: error extracting parameters.'
      return
      end


C ********* partf (equivalent to startup.c)
C  Allowing terminal and config file arguments
C  to be passed to the program from the invocation line.  

      subroutine partf(termtype,inf)
      COMMON/OUTIN/IUOUT,IUIN

      integer termtype
      character*48 prog
      integer iargc,m
      character argument*72,mode*8,inf*144,appn*24,outs*248


C Determine if an X graphics library (GTK or X11) was linked
C into binary, and set default terminal type as necessary

      if ( iXavail() == 1 ) then 
        termtype = 8
        mode = 'graphic'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      else
        termtype = -1
        mode = 'text'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      endif
      
      inf  = 'UNKNOWN'
      argument = ' '

C Get arguments from command line: if one parameter.
      if(m.eq.0)then
        write(outs,'(6a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf))
C        call edisp(iuout,outs)
        return
      elseif(m.ge.1)then
  41    i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)

        if(argument(1:5).eq.'-help')then
          call parpsfh(appn)
          stop
        elseif(argument(1:5).eq.'-mode')then
          i=i+1
          call getarg(i,argument)
          if(argument(1:4).eq.'text')then
            termtype = -1
            mode = 'text'
          elseif(argument(1:4).eq.'page')then
            termtype = -2
            mode = 'page'
          elseif(argument(1:5).eq.'graph')then
            termtype = 8
            mode = 'graphic'
          elseif(argument(1:6).eq.'script')then
            termtype = -6
            mode = 'script'
          endif
        elseif(argument(1:5).eq.'-file')then
          i=i+1
          call getarg(i,inf)
        endif
        goto 41

  42    continue
        write(outs,'(6a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf))
C        call edisp248(iuout,outs,90)
        return
      endif
      end

C ********* parspza 
C  Allowing terminal, size, config file, scene purpose and actions arguments
C  to be passed to the program from the invocation line.  

      subroutine parspza(termtype,iappw,iappx,iappy,inf,zone,aim,act)
      COMMON/OUTIN/IUOUT,IUIN

      integer termtype,iappw,iappx,iappy
      character*48 prog
      integer iargc,m
      character argument*72,mode*8,inf*144,appn*24
      character outs*248,aim*12,act*72,zone*16


C Determine if an X graphics library (GTK or X11) was linked
C into binary, and set default terminal type as necessary

      if ( iXavail() == 1 ) then 
        termtype = 8
        mode = 'graphic'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      else
        termtype = -1
        mode = 'text'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      endif
       
      iappw = 0
      iappx = 0
      iappy = 0
      inf  = 'UNKNOWN'
      zone = 'NONE'
      aim = 'NONE'
      act = 'NONE'
      argument = ' '

C Get arguments from command line: if one parameter.
      if(m.eq.0)then
        write(outs,'(6a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf))
C        call edisp(iuout,outs)
        write(outs,'(a,a)') 'for focus zone ',zone(1:lnblnk(zone))
C        call edisp(iuout,outs)
        write(outs,'(a,a)') 'with scene purpose',aim(1:lnblnk(aim))
C        call edisp(iuout,outs)
        write(outs,'(a,a)') 'with action ',act(1:lnblnk(act))
C        call edisp(iuout,outs)
        return
      elseif(m.ge.1)then
  41    i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)

        if(argument(1:5).eq.'-help')then
          call parpsfh(appn)
          stop
        elseif(argument(1:5).eq.'-mode')then
          i=i+1
          call getarg(i,argument)
          if(argument(1:4).eq.'text')then
            termtype = -1
            mode = 'text'
          elseif(argument(1:4).eq.'page')then
            termtype = -2
            mode = 'page'
          elseif(argument(1:5).eq.'graph')then
            termtype = 8
            mode = 'graphic'
          elseif(argument(1:6).eq.'script')then
            termtype = -6
            mode = 'script'
          endif
        elseif(argument(1:2).eq.'-s')then
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappw
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappx
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappy
        elseif(argument(1:5).eq.'-file')then
          i=i+1
          call getarg(i,inf)
        elseif(argument(1:5).eq.'-zone')then
          i=i+1
          call getarg(i,zone)
        elseif(argument(1:8).eq.'-purpose')then
          i=i+1
          call getarg(i,aim)
        elseif(argument(1:4).eq.'-act')then
          i=i+1
          call getarg(i,act)
        endif
        goto 41

  42    continue
        write(outs,'(12a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf)),' for focus zone ',zone(1:lnblnk(zone)),
     &    ' with scene purpose ',aim(1:lnblnk(aim)),
     &    ' with action ',act(1:lnblnk(act))
C        call edisp248(iuout,outs,90)
        return
      endif

C Errors.
   2  write(6,'(a)') 'parspza: error extracting parameters.'
      return
      end

C ********* parsfact
C  Allowing terminal, size, file andactions arguments
C  to be passed to the program from the invocation line.  

      subroutine parsfact(termtype,iappw,iappx,iappy,inf,act,actf)
      COMMON/OUTIN/IUOUT,IUIN

      integer termtype,iappw,iappx,iappy
      character*48 prog
      integer iargc,m
      character argument*72,mode*8,inf*144,actf*72,appn*24
      character outs*248,act*16


C Determine if an X graphics library (GTK or X11) was linked
C into binary, and set default terminal type as necessary

      if ( iXavail() == 1 ) then 
        termtype = 8
        mode = 'graphic'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      else
        termtype = -1
        mode = 'text'
        m = iargc()
        i = 0
        call getarg(i,prog)
        call findapp(prog,appn)
      endif
      
      iappw = 0
      iappx = 0
      iappy = 0
      inf  = 'UNKNOWN'
      actf = 'UNKNOWN'
      act = 'NONE'
      argument = ' '

C Get arguments from command line: if one parameter.
      if(m.eq.0)then
        write(outs,'(10a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf)),' with action ',act(1:lnblnk(act)),
     &    ' applied to ',actf(1:lnblnk(actf))
C        call edisp248(iuout,outs,90)
        return
      elseif(m.ge.1)then
  41    i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)

        if(argument(1:5).eq.'-help')then
          call parpsfh(appn)
          stop
        elseif(argument(1:5).eq.'-mode')then
          i=i+1
          call getarg(i,argument)
          if(argument(1:4).eq.'text')then
            termtype = -1
            mode = 'text'
          elseif(argument(1:4).eq.'page')then
            termtype = -2
            mode = 'page'
          elseif(argument(1:5).eq.'graph')then
            termtype = 8
            mode = 'graphic'
          elseif(argument(1:6).eq.'script')then
            termtype = -6
            mode = 'script'
          endif
        elseif(argument(1:2).eq.'-s')then
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappw
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappx
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=ISTAT,ERR=2)iappy
        elseif(argument(1:5).eq.'-file')then
          i=i+1
          call getarg(i,inf)
        elseif(argument(1:5).eq.'-act ')then
          i=i+1
          call getarg(i,act)
        elseif(argument(1:5).eq.'-actf')then
          i=i+1
          call getarg(i,actf)
        endif
        goto 41
  42    continue
        write(outs,'(10a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf)),' with action ',act(1:lnblnk(act)),
     &    ' applied to ',actf(1:lnblnk(actf))
C        call edisp248(iuout,outs,90)
        return
      endif

C Errors.
   2  write(6,'(a)') 'parsfact: error extracting parameters.'
      return
      end

C Parse command line paramters for ecnv.
      subroutine parcnv(itrc,conv,iobs,ichop,itmprm,iform,oform,
     &                   inf,ouf,ucfg)
      COMMON/OUTIN/IUOUT,IUIN

      integer iargc,m
      integer termtype
      integer i,itrc,itmprm,iobs,ichop
      real conv
      character argument*72,prog*48,inf*144,outs*248,appn*24
      character iform*24,oform*24,ouf*144,ucfg*72,mode*8

      inf  = 'UNKNOWN'
      iform = 'UNKNOWN'
      oform = 'UNKNOWN'
      ouf  = 'UNKNOWN'
      ucfg  = 'UNKNOWN'
      conv = 0.001
      itrc = 0
      itmprm = 1
      iobs  = 0
      ichop = 0

C Get number of arguments and command name.
      termtype = -1
      mode = 'text'
      m = iargc()
      i = 0
      call getarg(i,prog)
      call findapp(prog,appn)

C If one parameter.
      if(m.eq.0)then
        write(6,'(a)') 'No arguments - ecnv aborting...'
        stop
      elseif(m.ge.1)then
  41    i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)
        if(argument(1:5).eq.'-help')then
      call edisp(iuout,'The product model filter. ')
      call edisp(iuout,' Use: [-v] [{-mm|-metre}] ')
      call edisp(iuout,'  -if {esp|dxf|zip|ww} -in <file> ')
      call edisp(iuout,
     &  '  -of {esp|dxf|viewer|zip|xfig|vrml} -out <file>')
      call edisp(iuout,' ')
      call edisp(iuout,'Where options are:')
      call edisp(iuout,' -if [esp|dxf|zip|ww] = input format')
      call edisp(iuout,'      (dxf = AutoCAD V11-12)')
      call edisp(iuout,'      (ww = ww graphic commands)')
      call edisp(iuout,' -in [file]  = input file')
      call edisp(iuout,
     &  ' -of [esp|dxf|viewer|tsbi3|xfig|vrml] = dest format')
      call edisp(iuout,' -out [file] = output file')
      call edisp(iuout,' -v          = verbose mode')
      call edisp(iuout,' -mm -metre  = dxf coords in mm (default)')
      call edisp(iuout,' -keep       = retain scratch files')
      call edisp(iuout,' -obs        = include esp-r obstructions')
      call edisp(iuout,' ')
      call edisp(iuout,'The following combinations are supported:')
      call edisp(iuout,'  input format  output format')
      call edisp(iuout,'  esp-r         dxf, viewer, zip, tsbi3, vrml')
      call edisp(iuout,'  dxf           esp-r, viewer')
      call edisp(iuout,'  zip           esp-r, viewer')
      call edisp(iuout,' ')
      call edisp(iuout,'If converting to esp-r then an additional') 
      call edisp(iuout,'parameter is allowed:')
      call edisp(iuout,' -u [upgrade] when updating a model from ')
      call edisp(iuout,'    registration stage.')
      call edisp(iuout,' ')
      call edisp(iuout,' -help :this help message.')
      call edisp(iuout,' ')
          stop
        elseif(argument(1:2).eq.'-v')then
          itrc = 1
        elseif(argument(1:3).eq.'-mm')then
          conv = 0.001
        elseif(argument(1:6).eq.'-metre')then
          conv = 1.00
        elseif(argument(1:4).eq.'-obs')then
          iobs = 1
        elseif(argument(1:5).eq.'-chop')then
          ichop = 1
        elseif(argument(1:5).eq.'-keep')then
          itmprm = 0
        elseif(argument(1:3).eq.'-in')then
          i=i+1
          call getarg(i,inf)
        elseif(argument(1:4).eq.'-out')then
          i=i+1
          call getarg(i,ouf)
        elseif(argument(1:2).eq.'-u')then
          i=i+1
          call getarg(i,ucfg)
          call edisp(iuout,'Will attempt to update configuration')
        elseif(argument(1:3).eq.'-if')then
          i=i+1
          call getarg(i,iform)
        elseif(argument(1:3).eq.'-of')then
          i=i+1
          call getarg(i,oform)
        endif
        goto 41

  42    continue
        write(outs,'(4a)') 'Starting ecnv in ',
     &    oform(1:lnblnk(oform)),' mode with in file ',
     &    inf(1:lnblnk(inf))
        call edisp248(iuout,outs,100)
        write(outs,'(3a)') 'and output file ',ouf(1:lnblnk(ouf)),'.'
        call edisp248(iuout,outs,100)
        return
      endif
      end

C ******* FINDAPP
C Given a file name (string) see if it contains one of the esp-r module names.
      SUBROUTINE findapp(string,appn)
      CHARACTER*(*) string,appn
      logical unixok

C Clear variables and get string lengths.
      ilfilen = LEN(appn)
      ilstr = LEN(string)
      call isunix(unixok)

C Scan for last position of a `/' (UNIX) or '\' (NT).
      islash = 0
      DO 99 I=1,LEN(string)

C Comment out UNIX or NT version as required.
        if(unixok)then
          if(string(I:I).eq.'/')islash=I
        else
         if(string(I:I).eq.'\\')islash=I
C         if(string(I:I).eq.'\')islash=I
        endif
        if(ichar(string(I:I)).lt.32)goto 100
 99   CONTINUE

C If islash = 0 then no root.
100   if(islash.eq.0)then
        if(ilstr.ge.ilfilen)then
          write(appn,'(a)')string(1:ilfilen)
        elseif(ilstr.lt.ilfilen)then
          write(appn,'(a)')string(1:lnblnk(string))
        endif
      else

C If islash > 0 then root and file name
        is=islash+1
        write(appn,'(a)') string(is:lnblnk(string))
      endif

      RETURN
      END
