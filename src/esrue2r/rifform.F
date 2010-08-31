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
#include "e2r_common.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      common/expath/runpath,pathtype
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth,aimpth,bsmpth

      CHARACTER doit*124,VALT*32
      character runpath*72,pathtype*24
      character item*30,h*72,HOLD*24
      character fil*80
      character ltmp*72,ltmp60*60
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,aimpth*24,bsmpth*24

      DIMENSION ITEM(24),IVALV(20),VALT(20)

      logical OK,dok

      ITA2 = IFIL+7
      ITA3 = IFIL+10
      ier=0

10    INO = -4
      write(ITEM(1),'(A,A)')    '  RIF     : ',
     &                                 rifname(ifocz,iscene,1)(1:18)
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
  20  H(1)='This menu selects the scene description files to'
      H(2)='be used in the octree.'
      H(3)=' '
      H(4)='The octree will be created by using the displayed'
      H(5)='files. If another octree using other files is to'
      H(6)='be created then change the relevant file name.'
      H(7)='  '
      H(8)='  '
      H(9)='Remember to update the `RIF` file after you have'
      H(10)='made changes to any settings.'
      H(11)=' '
      H(12)='NOTE: If any of the file names are UNKNOWN then they'
      H(13)='      will not be used in creating the octree. '

      CALL EMENU(' Image & calculation options',ITEM,NITEMS,INO)
      if(INO.EQ.NITEMS)then
        return
      elseif(INO.EQ.NITEMS-1)then

c Explains octree menu.
        CALL PHELPD('octree menu',12,'-',0,0,IER)
      elseif(INO.EQ.4)THEN
        H(1)='This holds the Sky definition.'
        ltmp = rskyfil
        CALL EASKS(ltmp,' Sky file name ?',' ',
     &     72,rskyfil,'sky file name',IER,1)
        if(ltmp.ne.' ')rskyfil = ltmp
      elseif(INO.EQ.5)then
        H(1)='This holds the glazed surfaces definitions.'
        H(2)='Its format depends on how glazing is treated.'
        ltmp = glzfil
        CALL EASKS(ltmp,' Glazing compositon file name ?',' ',
     &     72,glzfil,'glaz file name',IER,2)
        if(ltmp.ne.' ')glzfil = ltmp

        H(1)='This holds surface properties for the opaque '
        H(2)='surfaces, obstructions and ground definition.'
        ltmp = matfil
        CALL EASKS(ltmp,' Opaque materials file name ?',' ',
     &     72,matfil,'outside mat file name',IER,2)
        if(ltmp.ne.' ')matfil = ltmp

        H(1)='This holds the outside surfaces, obstructions and'
        H(2)='ground definition.'
        ltmp = rofil
        CALL EASKS(ltmp,' Outside opaque compositon file name ?',' ',
     &     72,rofil,'outside file name',IER,2)
        if(ltmp.ne.' ')rofil = ltmp

        H(1)='This holds the inside (surfaces, including adjacent'
        H(2)='zones) definition.'
        ltmp = rzfil
        CALL EASKS(ltmp,' Inside opaque composition file name ? ','  ',
     &    72,rzfil,'zone opaq def file name',IER,2)
        if(ltmp.ne.' ')rzfil = ltmp

        H(1)='This holds miscel items (furniture, fittings, objects)'
        H(2)='not included in a thermal simulation.'
        ltmp = rmfil
        CALL EASKS(ltmp,
     &   ' Miscel items (furniture etc.) (If not used give NONE) ',
     &   ' ? ',72,rmfil,'misc file',IER,2)
        if(ltmp.ne.' ')rmfil = ltmp
      elseif(INO.EQ.8)then

C Image quality: >> Low|Medium|High  [Low]
        H(1)='Image quality sets a number of command parameters'
        H(2)='and influences the processing time.  Most of the time'
        H(3)='a low quality image will suffice (and there are opts '
        H(4)='which can get close to medium quality). '
        CALL EASKABC(' Image quality: ',' ','Low (default)',
     &          'Medium (report quality)','High (takes ages) ?',IW,4)
        if(IW.eq.1)imgqua='Low'
        if(IW.eq.2)imgqua='Medium'
        if(IW.eq.3)imgqua='High'
      elseif(INO.EQ.9)then

C Detail level: >> Low|Medium|High  [Low]
        H(1)='Detail level: Use low for zones with few surfaces (ie'
        H(2)='as in a thermal problem), use Medium if furnature and'
        H(3)='objects, use High if lots of detail and textures. '
        CALL EASKABC(' Detail level: ',' ','Low (sparse geometry)',
     &          'Medium (busy geometry)','High (with textures) ?',IW,3)
        if(IW.eq.1)detlvl='Low'
        if(IW.eq.2)detlvl='Medium'
        if(IW.eq.3)detlvl='High'
      elseif(INO.EQ.10)then

C Light level variability: >> Low|Medium|High  [High if daylit]
        H(1)='Light level variability: Use Low for zones which are '
        H(2)='uniformally lit by lighting fixtures, Medium if there'
        H(3)='is diffuse skylight and High if direct sunlight. '
        CALL EASKABC(' Variability of light level: ',' ',
     &          'Low (uniform artificial)',
     &          'Medium (diffuse sun)','High (direct sunlight) ?',IW,3)
        if(IW.eq.1)llvar='Low'
        if(IW.eq.2)llvar='Medium'
        if(IW.eq.3)llvar='High'
      elseif(INO.EQ.11)then
        H(1)='A direct (artificial) lighting system is one where '
        H(2)='reflections from surfaces are unimportant.'
        H(3)='If the space is naturally lit then reflections are '
        H(4)='important.  The geometry of the space defines whether'
        H(5)='the daylight is direct or diffuse.  As a basic guide'
        H(6)='assume direct daylight unless your model contains'
        H(7)='lightshelfs, an atrium or similar devices.'
        CALL EASKABC(' Lighting system: ',' ','Direct (artificial)',
     &               'Direct daylight','Diffuse daylight',IW,7)
        indrcb=IW-1
      elseif(INO.EQ.12)then

C Image X pixel size.
        H(1)='Set overall image width in pixels (smaller numbers '
        H(2)='process more quickly). The height and thus image'
        H(3)='pixel aspect are automatically set from the view'
        H(4)='data (-vh and -vv options).'
  45    WRITE(HOLD,'(1x,i5)')ipicx
        CALL EASKS(HOLD,' Image width in pixels: ',
     &                            '  ',24,' 512','image width',IER,4)
        K=0
        CALL EGETWI(HOLD,K,ipicx,100,1024,'W','width',IER)
        if(ier.ne.0)goto 45
      elseif(INO.EQ.13)then
        dok=.false.
        h(1)='Penumbras (soft shadows) in image is processor intensive'
        h(2)='and some users prefer this. '
        CALL ASKOK(' Include penumbras in image (soft shadows) ?',
     &    ' (Yes requires longer calculations)?',OK,dok,2)
        if(OK)then
          penumb='True'
        else
          penumb='False'
        endif
      elseif(INO.EQ.14)then
        H(1)='Computations can take a moment to several hours - so '
        H(2)='a % completion report is available. '
        CALL EASKI(itrack,' ',' Track progress every [x] minutes ? ',
     &          0,'F',10,'W',1,'reporting increment',IER,2)
      elseif(INO.EQ.15)then

C << for each axis if inside then use zone bounds, if outside use
C    the diameter of the disk...>>
        H(1)='Initial/simple view points are built up based on a  '
        H(2)='bounding box defined by minimimum & maximum X Y Z  '
        H(3)='coordinates.  If view has been set to inside or  '
        H(4)='outside this is also indicated in the string.  '
        if(intext.eq.0)then
          ltmp60 = rzoncmd
          CALL EASKS(ltmp60,' Initial assumptions about general view',
     &      ' and a bounding box: (see help) ',
     &       60,' Ext -100. 100. -100. 100. 0. 100.','zone cmd',IER,4)
          rzoncmd = ltmp60
        elseif(intext.eq.1)then
          ltmp60 = rzoncmd
          CALL EASKS(ltmp60,' Initial assumptions about general view',
     &      ' and a bounding box: (see help) ',
     &       60,' Int -10. 10. -10. 10. 0. 10.','zone cmd',IER,4)
          rzoncmd = ltmp60
        endif
      elseif(INO.EQ.16)then

C Specify Ambient file name.  
        H(1)='This holds temporary information on ambient light'
        H(2)='when a number of viewpoints are to be generated of '
        H(3)='the same scene.  NOT desirable othewise. '
        ltmp = rambfil
        CALL EASKS(ltmp,' Abient light temporary file name ?',' ',
     &     72,'NONE','ambient file name',IER,3)
        if(ltmp.ne.' ') rambfil = ltmp

      elseif(INO.EQ.17)then

C Specify Options optnfil file name.
        H(1)='This holds additional calculation option commands'
        H(2)='in a file. '
        ltmp = optnfil
        CALL EASKS(ltmp,' Calculation options file name ?',' ',
     &     72,'NONE','options file name',IER,2)
        if(ltmp.ne.' ') optnfil = ltmp

      elseif(INO.EQ.18)then

C Specify PIC root file name.
        H(1)='This is the root name for pictures created with `rad`'
        ltmp = picfil
        CALL EASKS(ltmp,' Picture root file name ?',' ',
     &     72,'xx','picture root file name',IER,1)
        if(ltmp.ne.' ') picfil = ltmp
      elseif(INO.EQ.20)then
 642    H(1)='A RIF (Raidance Input File) specifies the composition'
        H(2)='of the problem for Radiance as well as parameters for'
        H(3)='images quality and calculation parameters. '
        H(4)=' '
        H(5)='If the file given exists it will be read in, if not '
        H(6)='a new one will be created with typical defaults. '
        ltmp = rifname(ifocz,iscene,1)
        CALL EASKS(ltmp,' Radiance RIF file ?',
     &    '  ',72,'xxx.rif','rif file name',IER,6)
        if(ltmp.eq.' ')goto 642
        rifname(ifocz,iscene,1) = ltmp
        call mkrif(ita2,rifname(ifocz,iscene,1),4,0,ier)
C        call edisp(iuout,' ') 
C        call edisp(iuout,'The current contents of the rif file: ') 
C        call LISTAS(ITA2,rifname(izone,iscene,ibpos),IER)
      elseif(INO.EQ.21)then
        H(1)='The manual edit function allows for `experts` to work'
        H(2)='their magic on the current contents. '
        CALL EASKABCD(' Options: ',' ','Browse current `rif` file',
     &          'Manually edit file','Re-read file','ignore ?',IW,2)
        if(IW.eq.1)then
          call vifile(ITA2,rifname(ifocz,iscene,1),'b',ier)
        elseif(IW.eq.2)then
          call vifile(ITA2,rifname(ifocz,iscene,1),'e',ier)
          H(1)='The manual edit function allows for `experts` to work'
          H(2)='their magic on the current contents. After editing '
          H(3)='the re-read file function should be used. '
          dok=.true.
          CALL ASKOK(' ',' Re-read after editing?',OK,dok,3)
          if(OK)call RRIF(ita2,ita3,rifname(ifocz,iscene,1),'s',ier)
        elseif(IW.eq.3)then
          call RRIF(ita2,ita3,rifname(ifocz,iscene,1),'s',ier)
        endif
      elseif(INO.EQ.22)then

C Pull up a list of views and ask user for which one to associate with
C the test run.
        H(1)='Please select one view to associate with the `rad`'
        H(2)='test. If no view selected the default used.'
        do 333 iv=1,indxvew
          write(valt(iv),'(a,1x,a)') rvewsh(iv),vewcmds(iv)(1:20)
  333   continue
        IX=1
        CALL EPICKS(IX,IVALV,' ',' View for test: ',
     &         32,indxvew,VALT,'View for test',IER,2)
        if(IX.ne.0)then
          ipckvew=IVALV(1)
        else
          ipckvew=0
        endif
        call edisp(iuout,' Checking rad with the commands:')

C Set file name including path and open.
        write(fil,'(2a)') runpath(1:lnblnk(runpath)),
     &    rifname(ifocz,iscene,1)(1:lnblnk(rifname(ifocz,iscene,1)))
        if(ipckvew.ne.0)then
          write(doit,'(4a)') 'rad -n -e -v ',
     &      rvewsh(ipckvew)(1:lnblnk(rvewsh(ipckvew))),
     &      ' ',fil(1:lnblnk(fil))
        else
          call edisp(iuout,' No view selected using default.')
          write(doit,'(2a)') 'rad -n -e -v xyZv ',fil(1:lnblnk(fil))
        endif

C Debug.
C        write(6,*) doit

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
#include "prj3dv.h"
#include "e2r_common.h"
      
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX

      integer lnblnk  ! function definition

      character pre*28
      character use*1

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
        write(optnfil,'(a,a4)')pre(1:lnblnk(pre)),'.opt'
      endif
      if(octfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(octfil,'(a,a4)')pre(1:lnblnk(pre)),'.oct'
      endif
      if(picfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(picfil,'(a)')pre(1:lnblnk(pre))
      endif
      if(rskyfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(rskyfil,'(a,a4)') pre(1:lnblnk(pre)),'.sky'
      endif

C If no inside file has been defined, setup typical names.
      if(rzfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(rzfil,'(a,a7)') pre(1:lnblnk(pre)),'-in.rad'
      endif
      if(rofil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(rofil,'(a,a8)') pre(1:lnblnk(pre)),'-out.rad'
      endif
      if(matfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(matfil,'(a,a4)') pre(1:lnblnk(pre)),'.mat'
      endif
      if(rmfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(rmfil,'(a,a,a,a9)') pre(1:lnblnk(pre)),'-misc.rad'
      endif
      if(rmmfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(rmmfil,'(a,a9)') pre(1:lnblnk(pre)),'-misc.mat'
      endif
      if(glzfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(glzfil,'(a,a8)') pre(1:lnblnk(pre)),'-glz.rad'
      endif
      if(aglzfil(1:7).eq.'UNKNOWN'.or.use.eq.'f')then
        write(aglzfil,'(a,a10)') pre(1:lnblnk(pre)),'-glz_a.rad'
      endif

C Reset to no views.
      indxvew = 0
      ipckvew = 0

C Initial assumptions.
      if (use.eq.'i') then 
        imgqua = 'Low'
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
        rvpx= -50.
        rvpy= -50.
        rvpz=  50.

C Outside view. Scale bounding box to match extremes of the ground disk.
        write(rzoncmd,'(a,6f8.2)') ' Exterior ',cx-DD,cx+DD,cy-DD,
     &      cy+DD,ZMN,DD
      endif

      return
      end

