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

C ******* Setup of image quality and calculation options.
      SUBROUTINE RIFFORM(IER)
#include "building.h"
#include "model.h"
#include "e2r_common.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      integer itc,icout  ! trace verbosity and output channel
      common/trace/itc,icout

C Radiance processor cores.
      integer radcores
      common/radcor/radcores

      CHARACTER doit*124,VALT*32
      character item*30,HOLD*24
      character fil*80,fs*1
      character ltmp*72,ltmp60*60
      logical unixok
      integer lnrif   ! length of rif file

      integer NITEMS,INO  ! max items and current menu item

      DIMENSION ITEM(24),IVALV(20),VALT(20)

      logical OK,dok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='RIFFORM'  ! set for subroutine
      
C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      ITA2 = IFIL+7
      ITA3 = IFIL+10
      ier=0

10    INO = -4
      write(ITEM(1),'(A,A)')    '  RIF     : ',RIFNAME(ISCENE)(1:18)
      write(ITEM(2),'(A,A12)')  '  root    : ',SCENERT(ISCENE)(1:18)
      write(ITEM(3),'(A,A)')    '  folder  : ',radpth(1:18)
      write(ITEM(4),'(A,A)')    'a sky file: ',rskyfil(1:18)
      ITEM(5) =                 'b scene descriptions          '
      if(intext.eq.0)then
        ITEM(6) =               '   view is `Outside`          '
      elseif(intext.eq.1)then
        ITEM(6) =               '   view is `Inside a zone`    '
      endif
      ITEM(7) =                 '  ___________________________ '
      write(ITEM(8),'(A,A6)')   'c image quality    : ',imgqua
      write(ITEM(9),'(A,A6)')   'd detail level     : ',detlvl
      write(ITEM(10),'(A,A6)')  'e light level varib: ',llvar
      write(ITEM(11),'(A,I2)')  'f diffuse reflect  : ',indrcb
      write(ITEM(12),'(A,I4)')  'g resolution (pixels): ',ipicx
      write(ITEM(13),'(A,A5)')  'h include penumbras: ',penumb
      write(ITEM(14),'(A,I2,a)')'i track progress @ ',itrack,' min'
      write(ITEM(15),'(A,a,a)') 'j ZONE: ',rzoncmd(1:19),'...'
      write(ITEM(16),'(A,A)')   'k ambient : ',rambfil(1:18)
      write(ITEM(17),'(A,A)')   'l options : ',optnfil(1:18)
      write(ITEM(18),'(A,A)')   'm picture : ',picfil(1:18)
      ITEM(19) =                '  ___________________________ '
      ITEM(20) =                '> save RIF file               '
      ITEM(21) =                '! browse/manually edit/refresh'
      ITEM(22) =                '* test RIF creation (dry run) '
      ITEM(23) =                '? Help                        '
      ITEM(24) =                '- exit                        '
      NITEMS = 24

C Help text for this menu.
  20  helptopic='e2r_scene_files_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU(' Image & calculation options',ITEM,NITEMS,INO)
      if(INO.EQ.NITEMS)then
        return
      elseif(INO.EQ.NITEMS-1)then

c Explains octree menu.
        helptopic='e2r_scene_files_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('octree menu',nbhelp,'-',0,0,IER)
      elseif(INO.EQ.4)THEN
        ltmp = rskyfil
        CALL EASKS(ltmp,'Sky file name ?',' ',
     &    72,rskyfil,'sky file name',IER,nbhelp)
        if(ltmp.ne.' ')rskyfil = ltmp
      elseif(INO.EQ.5)then
        ltmp = glzfil
        CALL EASKS(ltmp,'Glazing compositon file name ?',' ',
     &    72,glzfil,'glaz file name',IER,nbhelp)
        if(ltmp.ne.' ')glzfil = ltmp

        ltmp = matfil
        CALL EASKS(ltmp,'Opaque materials file name ?',' ',
     &    72,matfil,'outside mat file name',IER,nbhelp)
        if(ltmp.ne.' ')matfil = ltmp

C Additional material file.
        ltmp = rmmfil
        CALL EASKS(ltmp,'Additional materials file name ?',
     &    '(If not used give NONE)? ',
     &    72,rmmfil,'additional mat file name',IER,nbhelp)
        if(ltmp(1:2).ne.'  '.or.ltmp(1:4).eq.'NONE')rmmfil = ltmp

        ltmp = rofil
        CALL EASKS(ltmp,'Outside opaque compositon file name ?',' ',
     &    72,rofil,'outside file name',IER,nbhep)
        if(ltmp.ne.' ')rofil = ltmp

        ltmp = rzfil
        CALL EASKS(ltmp,'Inside opaque composition file name ? ','  ',
     &    72,rzfil,'zone opaq def file name',IER,nbhelp)
        if(ltmp.ne.' ')rzfil = ltmp

C Additional rif file for furniture or fixtures).
        ltmp = rmfil
        CALL EASKS(ltmp,
     &    'Miscel items (furniture, fixtures etc.) ',
     &    '(If not used give NONE)?',72,rmfil,'misc file',IER,nbhelp)
        if(ltmp(1:2).ne.'  '.or.ltmp(1:4).eq.'NONE')rmfil = ltmp

      elseif(INO.EQ.8)then

C Image quality: >> Low|Medium|High  [Low]
        helptopic='image_quality_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC('Image quality: ',' ','Low (default)',
     &    'Medium (report quality)','High (takes ages) ?',IW,nbhelp)
        if(IW.eq.1)imgqua='Low'
        if(IW.eq.2)imgqua='Medium'
        if(IW.eq.3)imgqua='High'
      elseif(INO.EQ.9)then

C Detail level: >> Low|Medium|High  [Low]
        helptopic='e2r_detail_level_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC('Detail level: ',' ','Low (sparse geometry)',
     &    'Medium (busy geometry)','High (with textures) ?',IW,nbhelp)
        if(IW.eq.1)detlvl='Low'
        if(IW.eq.2)detlvl='Medium'
        if(IW.eq.3)detlvl='High'
      elseif(INO.EQ.10)then

C Light level variability: >> Low|Medium|High  [High if daylit]
        helptopic='e2r_light_var_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC('Variability of light level: ',' ',
     &    'Low (uniform artificial)',
     &    'Medium (diffuse sun)','High (direct sunlight) ?',IW,nbhelp)
        if(IW.eq.1)llvar='Low'
        if(IW.eq.2)llvar='Medium'
        if(IW.eq.3)llvar='High'
      elseif(INO.EQ.11)then
        helptopic='e2r_light_system_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC('Lighting system: ',' ','Direct (artificial)',
     &    'Direct daylight','Diffuse daylight',IW,nbhelp)
        indrcb=IW-1
      elseif(INO.EQ.12)then

C Image X pixel size.
        helptopic='e2r_pixel_size_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
  45    WRITE(HOLD,'(1x,i5)')ipicx
        CALL EASKS(HOLD,' Image width in pixels: ',
     &    '  ',24,' 512','image width',IER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,ipicx,100,1024,'W','width',IER)
        if(ier.ne.0)goto 45
      elseif(INO.EQ.13)then
        dok=.false.
        helptopic='e2r_penumbras_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK(' Include penumbras in image (soft shadows) ?',
     &    ' (Yes requires longer calculations)?',OK,dok,nbhelp)
        if(OK)then
          penumb='True'
        else
          penumb='False'
        endif
      elseif(INO.EQ.14)then
        helptopic='track_progress_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKI(itrack,' ',' Track progress every [x] minutes ? ',
     &    0,'F',10,'W',1,'reporting increment',IER,nbhelp)
      elseif(INO.EQ.15)then

C << for each axis if inside then use zone bounds, if outside use
C    the diameter of the disk...>>
        helptopic='e2r_viewpoint_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(intext.eq.0)then
          ltmp60 = rzoncmd
          CALL EASKS(ltmp60,' Initial assumptions about general view',
     &      ' and a bounding box: (see help) ',
     &      60,' Ext -100. 100. -100. 100. 0. 100.','zone cmd',
     &      IER,nbhelp)
          rzoncmd = ltmp60
        elseif(intext.eq.1)then
          ltmp60 = rzoncmd
          CALL EASKS(ltmp60,' Initial assumptions about general view',
     &      ' and a bounding box: (see help) ',
     &      60,' Int -10. 10. -10. 10. 0. 10.','zone cmd',
     &      IER,nbhelp)
          rzoncmd = ltmp60
        endif
      elseif(INO.EQ.16)then

C Specify Ambient file name.  
        helptopic='e2r_ambient_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ltmp = rambfil
        CALL EASKS(ltmp,' Abient light temporary file name ?',' ',
     &    72,'NONE','ambient file name',IER,nbhelp)
        if(ltmp.ne.' ') rambfil = ltmp

      elseif(INO.EQ.17)then

C Specify Options optnfil file name.
        helptopic='e2r_options_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ltmp = optnfil
        CALL EASKS(ltmp,' Calculation options file name ?',' ',
     &    72,'NONE','options file name',IER,nbhelp)
        if(ltmp.ne.' ') optnfil = ltmp

      elseif(INO.EQ.18)then

C Specify PIC root file name.
        helptopic='e2r_pic_root_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ltmp = picfil
        CALL EASKS(ltmp,' Picture root file name ?',' ',
     &    72,'xx','picture root file name',IER,nbhelp)
        if(ltmp.ne.' ') picfil = ltmp
      elseif(INO.EQ.20)then
 642    helptopic='e2r_RIF_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ltmp = RIFNAME(ISCENE)
        CALL EASKS(ltmp,' Radiance RIF file ?',
     &    '  ',72,'xxx.rif','rif file name',IER,nbhelp)
        if(ltmp.eq.' ')goto 642
        RIFNAME(ISCENE) = ltmp
        call mkrif(ita2,RIFNAME(ISCENE),4,0,ier)
C        call edisp(iuout,' ') 
C        call edisp(iuout,'The current contents of the rif file: ') 
C        call LISTAS(ITA2,RIFNAME(ISCENE),IER)
      elseif(INO.EQ.21)then
        helptopic='e2r_manual_edit_rif'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABCD(' Options: ',' ','Browse current `rif` file',
     &    'Manually edit file','Re-read file','ignore ?',IW,nbhelp)
        if(IW.eq.1)then

C Prepend local path.
          lnrif=lnblnk(RIFNAME(ISCENE))
          write(ltmp,'(2a)') runpath(1:lnrp),RIFNAME(ISCENE)(1:lnrif)
          call vifile(ITA2,ltmp,'b',ier)
C          call vifile(ITA2,RIFNAME(ISCENE),'b',ier)
        elseif(IW.eq.2)then

C Prepend local path.
          lnrif=lnblnk(RIFNAME(ISCENE))
          write(ltmp,'(2a)') runpath(1:lnrp),RIFNAME(ISCENE)(1:lnrif)
          call vifile(ITA2,ltmp,'e',ier)
          dok=.true.
          CALL ASKOK(' ',' Re-read after editing?',OK,dok,nbhelp)
          if(OK)call RRIF(ita2,ita3,RIFNAME(ISCENE),'s',ier)
        elseif(IW.eq.3)then
          call RRIF(ita2,ita3,RIFNAME(ISCENE),'s',ier)
        endif
      elseif(INO.EQ.22)then

C Pull up a list of views and ask user for which one to associate with
C the test run.
        helptopic='e2r_view_name_for_test'
        call gethelptext(helpinsub,helptopic,nbhelp)
        do 333 iv=1,indxvew
          write(valt(iv),'(a,1x,a)') rvewsh(iv),vewcmds(iv)(1:20)
  333   continue
        IX=1
        CALL EPICKS(IX,IVALV,' ',' View for test: ',
     &    32,indxvew,VALT,'View for test',IER,nbhelp)
        if(IX.ne.0)then
          ipckvew=IVALV(1)
        else
          ipckvew=0
        endif
        call edisp(iuout,' Checking rad with the commands:')

C Set file name including path and open.
        write(fil,'(2a)') runpath(1:lnblnk(runpath)),
     &    RIFNAME(ISCENE)(1:lnblnk(RIFNAME(ISCENE)))
        if(ipckvew.ne.0)then
          if(radcores.eq.1)then
            write(doit,'(6a)') 'cd ',runpath(1:lnrp),
     &        '; rad -n -e -v ',
     &        rvewsh(ipckvew)(1:lnblnk(rvewsh(ipckvew))),
     &        ' ',fil(1:lnblnk(fil))
          else
            write(doit,'(3a,i2,4a)') 'cd ',runpath(1:lnrp),
     &        '; rad -e -N',radcores,' -v ',
     &        rvewsh(ipckvew)(1:lnblnk(rvewsh(ipckvew))),
     &        ' ',fil(1:lnblnk(fil))
          endif
        else
          call edisp(iuout,' No view selected using default.')
          if(radcores.eq.1)then
            write(doit,'(4a)') 'cd ',runpath(1:lnrp),
     &        '; rad -n -e -v xyZv ',fil(1:lnblnk(fil))
          else
            write(doit,'(3a,i2,2a)') 'cd ',runpath(1:lnrp),
     &        '; rad -e -N',radcores,' -v xyZv ',
     &        fil(1:lnblnk(fil))
          endif
        endif

C Debug/trace.
        if(itc.eq.0)then
          continue
        else     
          write(icout,*) doit(1:lnblnk(doit))
        endif
        call edisp(iuout,'Check text window for the trial results.')
        call runit(doit,'-')
      else
        goto 20
      endif
      goto 10

      end

C ******* RIFSET
C Setup initial common blocks for RIF file given a root name
C and basic assumptions about type of use.
C If use = 'i' then initial assumptions.
C If use = 'f' then force files to account for change in root/dir name.
      subroutine rifset(ic,use)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
#include "e2r_common.h"
      
      integer lnblnk  ! function definition

      character pre*28

      character use*1
      logical founddiffuse,foundembedded,foundxform

C Make the initial part of new file names to follow scene root name.
      write(pre,'(a)') SCENERT(ISCENE)(1:lnblnk(SCENERT(ISCENE)))

C Have information on ground disk ready. << ? case of external view with ifocz -1 >>
      if(ifocz.gt.0)then
        if (iZBFLG(ifocz).eq.0) call BNDOBJ(0,IER)
      endif
      XD=(XMX-XMN)**2 + (YMX-YMN)**2
      DD=SQRT(XD)*1.5
      cx=XMN+((XMX-XMN)/2.)
      cy=YMN+((YMX-YMN)/2.)
       
C If Initial pass then reset `done' logic as well as names
C of the descriptive files.
      skydone=.false.
      outdone=.false.
      indone=.false.
      misdone=.false.
      vewdone=.false.
      iesdone=.false.
      rambfil='NONE'
      optnfil='NONE'
      itrack= 1

C If we don't yet know a zone assume outside.
      if(ic.eq.-1)then
        intext = 0
        ifocz = -1
      endif

C For both initial and update use, do the following.
      if(optnfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(optnfil,'(2a)')pre(1:lnblnk(pre)),'.opt'
      endif
      if(octfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(octfil,'(2a)')pre(1:lnblnk(pre)),'.oct'
      endif
      if(picfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(picfil,'(a)')pre(1:lnblnk(pre))
      endif
      if(rskyfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(rskyfil,'(2a)') pre(1:lnblnk(pre)),'.sky'
      endif

C If no inside file has been defined, setup typical names.
      if(rzfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(rzfil,'(2a)') pre(1:lnblnk(pre)),'-in.rad'
      endif
      if(rofil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(rofil,'(2a)') pre(1:lnblnk(pre)),'-out.rad'
      endif
      if(matfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(matfil,'(2a)') pre(1:lnblnk(pre)),'.mat'
      endif

C Additional rif file (furniture or fixtures).
      if(rmfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(rmfil,'(2a)') pre(1:lnblnk(pre)),'-misc.rad'
      endif


C Check if only embedded mentioned in model cfg file.
      if(nbofies.gt.0)then
        do loop=1,nbofies
          if(iesfile(loop)(1:8).eq.'embedded')then
            foundembedded=.true.
          else
            foundxform=.true.
          endif
        enddo
      endif

C Need to deal with a model with mixed embedded and xformed stuff.
      if(foundxform)then
        write(iesfil,'(2a)') pre(1:lnblnk(pre)),'-xform.rad'
      else
        write(iesfil,'(a)') 'embedded'
      endif


C Additional material file.
      if(rmmfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(rmmfil,'(2a)') pre(1:lnblnk(pre)),'-misc.mat'
      endif
      if(glzfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(glzfil,'(2a)') pre(1:lnblnk(pre)),'-glz.rad'
      endif
      if(aglzfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(aglzfil,'(2a)') pre(1:lnblnk(pre)),'-glz_a.rad'
      endif

C Reset to no views.
      indxvew = 0
      ipckvew = 0

C Initial assumptions.
      if (use.eq.'i') then 
        imgqua = 'Medium'
        detlvl = 'Medium'
        llvar  = 'High'
        indrcb = 0
        ipicx  = 258
        penumb = 'False'
        crenrp = '  '
        coconv = '  '
      endif

C Generate bounding box.
      if(intext.eq.1)then

C Pick up the centre of the focus zone.
        rvpx=ZCOG(ic,1)
        rvpy=ZCOG(ic,2)
        rvpz=ZCOG(ic,3)
        write(rzoncmd,'(a,6f8.2)') ' Interior ',ZXMN(ic),ZXMX(ic),
     &     ZYMN(ic),ZYMX(ic),ZZMN(ic),ZZMX(ic)
      elseif(intext.eq.0)then

C Assume an esp-r like upper Southwest corner.
        rvpx= -50.0; rvpy= -50.0; rvpz=  50.0

C Outside view. Scale bounding box to match extremes of the ground disk.
        write(rzoncmd,'(a,6f8.2)') ' Exterior ',cx-DD,cx+DD,cy-DD,
     &      cy+DD,ZMN,DD
      endif

      return
      end

