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

C file setup.F includes facilities for display of dates, time
C and viewpoints. MODSIT is set to true if default wireframe
C viewpoint is changed.

      SUBROUTINE SETUP(MODSIT,ITRC,IER)
#include "building.h"
#include "espriou.h"
      
      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition

      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/FILEP/IFIL
      common/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/texted/tedlbl,teditor
      common/user/browse

C Initial view.
      common/initv/initvt,EYEMI(3),VIEWMI(3),ANGI
      common/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE

C CTYPE (4 char) - zone shape type (REC, REG or GEN)
C gversion (real) version of the geometry file (1.0 legacy, 1.1 current).
C igupgrade (integer) 0 = do nothing, 1 = leave at 1.0, 2 = upgrade to 1.1
      character CTYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade

      logical OK,DOK,MODSIT,browse,defview,unixok
      logical closea,closeb,closec,closed,concat
      dimension ITEMS1(14)
      character LFIL*96,H*72,longtfile*144,llitem*144,longtfiledos*144
      character ITEMS1*47,doit*248,tedlbl*20,teditor*20
      character tmode*8,hold32*32
      character menu*32,doc*248,ureturn*8

C Set initial states to false and clear text buffers.
      MODSIT=.false.
      LFIL=' '
      longtfile=' '
      doit=' '
      hold32=' '

C Check if Unix-based or DOS based.
      call isunix(unixok)

C Check default view point.
  33  defview=.false.
      CALL ECLOSE(EYEM(1),-100.0,1.0,closea)
      CALL ECLOSE(EYEM(2),-100.0,1.0,closeb)
      CALL ECLOSE(EYEM(3),100.0,1.0,closec)
      CALL ECLOSE(ANG,40.0,1.0,closed)
      if(closea.and.closeb.and.closec.and.closed)then
        defview=.true.
      endif

      INO1=-4
      if(defview)then
        ITEMS1(1)='a wireframe view: default                     '
      else
        ITEMS1(1)='a wireframe view: user defined                '
      endif
      IF(IFDAY.EQ.0)THEN
        ITEMS1(2)='b display format for date>> DOY 10            '
      ELSEIF(IFDAY.EQ.1)THEN
        ITEMS1(2)='b display format for date>> 10 Jan            '
      ELSEIF(IFDAY.EQ.2)THEN
        ITEMS1(2)='b display format for date>> Fri 10 Jan        '
      ENDIF

C Display of time of day.
      IF(IFTIME.EQ.0)THEN
        ITEMS1(3)='c display format for time>> 10h30             '
      ELSEIF(IFTIME.EQ.1)THEN
        ITEMS1(3)='c display format for time>> 10.50             '
      ELSEIF(IFTIME.EQ.2)THEN
        ITEMS1(3)='c display format for time>> 0.4166 (day frac.)'
      ENDIF

      ITEMS1(4)  ='e look at ASCII file                          '
      ITEMS1(5)  ='f edit an ASCII file                          '
      ITEMS1(6)  ='g edit ESP-r defaults for files               '
      ITEMS1(7)  ='h edit ESP-r 3rd party applications           '
      ITEMS1(8)  ='i edit ESP-r climatelist entries              '
      ITEMS1(9)  ='j edit ESP-r exemplars entries                '
      IF(ITRC.EQ.0)THEN
        ITEMS1(10)='k feedback >> silent                          '
      ELSEIF(ITRC.EQ.1)THEN
        ITEMS1(10)='k feedback >> summary                         '
      ELSEIF(ITRC.EQ.2)THEN
        ITEMS1(10)='k feedback >> detailed                        '
      ENDIF
      if(igupgrade.eq.0)then
        ITEMS1(11)='l upgrade files >> no change                  '
      elseif(igupgrade.eq.1)then
        ITEMS1(11)='l upgrade files >> save legacy format         '
      elseif(igupgrade.eq.2)then
        ITEMS1(11)='l upgrade files >> save current format        '
      endif
      ITEMS1(12)  ='* test button (silent zone creation)          '
      ITEMS1(13) ='? help                                        '
      ITEMS1(14) ='- exit this menu                              '

C Help text for menu.
      H(1)='This menu allows the display format for dates and'
      H(2)='times to be changed.  The active year can also be'
      H(3)='defined as required by controllers that require day'
      H(4)='of the week information.'
      H(5)=' '
      H(6)='One option lists a specified ASCII file - sometimes'
      H(7)='useful if you have need to directly view the contents'
      H(8)='of a problem describing entity such as the system'
      H(9)='configuration file or a zone geometry file.'
      H(10)=' '
      H(11)='The wireframe view option allows you to set a different'
      H(12)='initial wireframe view.'
      H(13)=' '
      H(14)='The external editor is currently defined to be:'
      write(H(15),'(3a)')'      ',teditor(:lnblnk(teditor)),'.'
      H(16)='This can be re-defined in your .esprc file.'
      H(17)=' '
      H(18)='The feedback level allows you to see more or less'
      H(19)='information as model files are being scanned. '
      H(20)=' '
      H(21)='As ESP-r evolves file formats change. Use the toggle'
      H(22)='to maintain the current state, save to an eariler'
      H(23)='format or save files to a newer format (if available).'
      H(24)=' '
      H(25)='The edit `defaults` file allows you to change where'
      H(26)='ESP-r looks for certain files. TAKE CARE when editing'
      H(27)='this file. '
      H(28)=' '
      H(29)='The edit `exemplars` file allows you to change where'
      H(30)='ESP-r looks for example models. TAKE CARE when editing'
      H(31)='this file. '
      H(32)=' '
      H(33)='The edit `3rd party application` allows you to specify'
      H(34)='a text editor, image display tool, CAD tool and the '
      H(35)='location of example models and where the `defaults` '
      H(36)='file (above) is located. '

      CALL EMENU('Tool preferences',ITEMS1,14,INO1)
      IF(INO1.EQ.1)THEN

C Wire frame view point.
        if(browse)goto 33
        h(1)='Unless other wise specified the initial view of the'
        h(2)='model is a standard default. If a user defined view '
        h(3)='is more appropriate then specify this now. '
        CALL EASKABCD(' Initial view position options',' :',
     &      'default','accept current','edit','continue',IW,3)
        if(IW.EQ.1)then

C Set eye point at standard place and viewed point at cog.
          EYEMI(1)=-100.
          EYEMI(2)=-100.
          EYEMI(3)=100.
          call bndobj(0,ier)
          VIEWMI(1)=VIEWM(1)
          VIEWMI(2)=VIEWM(2)
          VIEWMI(3)=VIEWM(3)
          ANGI=40.
          initvt=0
          MODSIT=.true.
        elseif(IW.EQ.2)then
          EYEMI(1)=EYEM(1)
          EYEMI(2)=EYEM(2)
          EYEMI(3)=EYEM(3)
          VIEWMI(1)=VIEWM(1)
          VIEWMI(2)=VIEWM(2)
          VIEWMI(3)=VIEWM(3)
          ANGI=ANG
          initvt=1
          MODSIT=.true.
        elseif(IW.EQ.3)then
          H(1)='The eyepoint is in metres in the coordinate system '
          H(2)='of the site. '
 143      WRITE(HOLD32,'(1x,3f8.2)')EYEM(1),EYEM(2),EYEM(3)
          CALL EASKS(HOLD32,' Eye point  X  Y  Z (in metres): ',
     &     '  ',32,' -100.0 -100.0 100.0 ','eye point coord',IER,2)
          K=0
          CALL EGETWR(HOLD32,K,EYEMI(1),-999.,999.,'W','Xcord',IER)
          CALL EGETWR(HOLD32,K,EYEMI(2),-999.,999.,'W','Ycord',IER)
          CALL EGETWR(HOLD32,K,EYEMI(3),0.,999.,'W','eye Zcord',IER)
          if(ier.ne.0)goto 143
          H(1)='The viewpoint is in metres in the coordinate system'
          H(2)='of the site and defaults to the centre of gravity '
          H(3)='of the body. '
 141      WRITE(HOLD32,'(1x,3f8.2)')VIEWM(1),VIEWM(2),VIEWM(3)
          CALL EASKS(HOLD32,' Viewpoint  X  Y  Z (in metres): ',
     &     '  ',32,' 2.0 2.0 1.0 ','viewpoint coord',IER,3)
          K=0
          CALL EGETWR(HOLD32,K,VIEWMI(1),-999.,999.,'W','Xcord',IER)
          CALL EGETWR(HOLD32,K,VIEWMI(2),-999.,999.,'W','Ycord',IER)
          CALL EGETWR(HOLD32,K,VIEWMI(3),0.,999.,'W','v Zcord',IER)
          if(ier.ne.0)goto 141
          H(1)='The angle of view is in degrees, allowable values'
          H(2)='are from 1 to 89 degrees. '
          ANGI=ANG
          CALL EASKR(ANGI,' ',' New angle of view ? ',
     &             1.0,'W',89.0,'W',40.,'angle ofview',IER,2)
          initvt=1
          MODSIT=.true.
        endif
      ELSEIF(INO1.EQ.2)THEN

C Allow user to toggle between 'Fri 10 Jan'/'10 Jan'/'DOY 124' format.
        IFDAY=IFDAY+1
        IF(IFDAY.GT.2)IFDAY=0
      ELSEIF(INO1.EQ.3)THEN

C Allow user to toggle between '10h00'/'10.00'/'0.41666' format. Only
C allow decimal day representation if day format is DOY
        IFTIME=IFTIME+1
        IF(IFDAY.EQ.0.AND.IFTIME.GT.2)IFTIME=0
        IF(IFDAY.GE.1.AND.IFTIME.GT.1)IFTIME=0
      ELSEIF(INO1.EQ.4)THEN

C List out an ASCII file. Use file browser if in GTK mode.
        H(1)='The specified ASCII file will be read and displayed'
        H(2)='in the text window page by page.'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
  6     if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(LFIL,'ASCII file name?',
     &      ' ',96,'xxx.rep','ASCII file',IER,2)
          write(longtfile,'(a)') LFIL(1:lnblnk(LFIL))
        elseif(iglib.eq.2)then
          CALL EASKF(longtfile,'ASCII file name?',
     &      ' ',144,'xxx.rep','ASCII file',IER,2)
        else
          CALL EASKF(LFIL,'ASCII file name?',
     &      ' ',96,'xxx.rep','ASCII file',IER,2)
          write(longtfile,'(a)') LFIL(1:lnblnk(LFIL))
        endif

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 33  ! redisplay menu.
        endif
        IUNIT=IFIL+1
        CALL LISTAS(IUNIT,longtfile,IER)
        IF(IER.LT.0)THEN
          dok=.false.
          h(1)='Either the file did not exist or included characters'
          h(2)='which would not print. '
          CALL ASKOK(' Problem detected while trying to open',
     &               ' ASCII file. Try again?',OK,dok,2)
          IF(OK)GOTO 6
        ENDIF
      ELSEIF(INO1.EQ.5)THEN

C Allow user to edit a file.  Use file browser if in GTK mode.
C If vi then spawn a new window to do the editing in.
        H(1)='An ASCII format file can be edited using the current'
        H(2)='defined text editor (as defined in the default file).'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        llitem=' '
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
          goto 33  ! redisplay menu.
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
      ELSEIF(INO1.EQ.6)THEN

C Edit the installed file `defaults'.
        call isunix(unixok)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        llitem=defdbfl
        llt=lnblnk(llitem)
        H(1) ='The edit `defaults` file allows you to change where'
        H(2) ='ESP-r looks for certain files. TAKE CARE when editing'
        H(3) ='this file. The key words are:'
        H(4) =' '
        H(5) =' *ipth - where ESP-r is installed. This was set during'
        H(6) ='         in intial Installation and compile sequence.'
        H(7) ='         Leave this alone!'
        H(8) =' '
        H(8) ='Names of default files (typically used ford demos)'
        H(9) =' *cfg - a default model to open'
        H(10)=' *ctl - a default zone control definition'
        H(11)=' *mfn - a default mass flow network'
        H(12)=' *dfd - a default CFD domain'
        H(13)=' *pnf - a default plant-only model'
        H(14)=' *res - a default simulation results file'
        H(15)=' *mfr - a default mass flow simulation results file'
        H(16)=' '
        H(17)='Names of databases initially associated with models'
        H(18)='edit these so as to be appropriate for your region'
        H(19)=' *clm - initial climate file'
        H(20)=' *prs - initial wind-pressure coefficients database'
        H(21)=' *prm - initial materials database'
        H(22)=' *mlc - initial constructions database'
        H(23)=' *opt - initial optical properties database'
        H(24)=' *env - initial event profiles database'
        H(25)=' *pdb - initial plant component template database'
        H(26)=' *ecdb - initial electrical component database'
        H(27)=' *mcdb - initial active materials database'
        H(28)=' *icdb - initial icons for the net module'
        H(29)=' *mldb - initial micotoxin database'
        H(30)=' *sbem - initial UK National Calculation Method'
        H(31)='         database (or code complience rules)'
        H(32)=' '
        H(33)='After edting and saving the information is available'
        H(34)='the next time applications are invoked. '
        H(35)=' '
        H(36)='NOTE: do not change the name of the default file! '
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(llitem,'(see help)','ESP-r default files?',
     &        96,'default','defaults file',IER,36)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(llitem,'(see help)','ESP-r default files?',
     &        124,'default','defaults file',IER,36)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(llitem,'(see help)','ESP-r default files?',
     &        144,'default','defaults file',IER,36)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(llitem,'(see help)','ESP-r default files?',
     &      144,'default','defaults file',IER,36)
        else
          CALL EASKF(llitem,'(see help)','ESP-r default files?',
     &      96,'default','defaults file',IER,36)
        endif

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 33  ! redisplay menu.
        endif

C Depending on whether Unix or DOS based setup paths. If DOS
C then check for spaces in name and change / to \.
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

      ELSEIF(INO1.EQ.7)THEN

C Edit the installed file `esprc'.
        call isunix(unixok)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        llitem=esprc
        llt=lnblnk(llitem)
        H(1) ='The edit `esprc` file allows you to change 3rd party'
        H(2) ='tools used by ESP-r. A standard esprc file is included'
        H(3) ='in the installation folder. ESP-r also checks for a '
        H(4) ='.esprc (note the dot) file in the users home folder'
        H(5) ='for user specific settings.  '
        H(6) =' '
        H(7) ='        TAKE CARE when editing this file. '
        H(8) =' '
        H(9) ='Data in the file are comma separated. The first token'
        H(10)='in each line is a key, the second is (typically) a '
        H(11)='label followed by a command or exedutable to invoke.'
        H(12)=' '
        H(13)='Key words in thefile are: '
        H(14)=' *ESPRC  - first line tag (leave alone)'
        H(15)=' '
        H(16)=' *gprn line specifies an application to invoke for'
        H(17)='    capturing a portion of the graphic display. For'
        H(18)='    Linux and OSX and Solaris the `import` tool in'
        H(19)='    Imagemagik is suggested.'
        H(20)=' '
        H(21)=' *cad  line specifies a CAD application to start. There'
        H(22)='   are 3 tokens after the tag, a user description, a'
        H(23)='   file format and the name of the executable.'
        H(24)=' '
        H(25)=' *image_display line(s) define how specific image types'
        H(26)='    can be displayed. The 2nd token is TIF XMBP GIF XWD'
        H(27)='    and the 3rd token is the name of the executable'
        H(28)=' '
        H(29)=' *journal is a line specifying whether a detailed log'
        H(30)='    should be kept of the users actions. The 2nd'
        H(31)='    token is OFF or ON'
        H(32)=' '
        H(33)=' *editor line specifies what text editor to invoke'
        H(34)='    the 2nd token is a label and the 3rd token is'
        H(35)='    the application to start. Typically on Windows'
        H(36)='    the WordPad works well and notepad does not work'
        H(37)='    well. On Linux/Solaris nedit is a good choice.'
        H(38)=' '
        H(39)=' *exemplars line provides the path to the list of '
        H(40)='    exemplar models.'
        H(41)=' '
        H(42)=' *validation_stds line provides the path to the list'
        H(43)='    of standard validation models.'
        H(44)=' '
        H(45)=' *db_defaults line provides the path to the file that'
        H(46)='    holds default database and model files as well as'
        H(47)='    default constructions to use.'
        H(48)=' '
        H(49)=' *db_climates line provides the path to the file that'
        H(50)='    holds list of climate files.'
        H(51)=' '
        H(52)=' *end  the last line of the file (leave alone)'
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(llitem,'(see help)','ESP-r esprc file?',
     &        96,'esprc','esprc file',IER,52)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(llitem,'(see help)','ESP-r esprc file?',
     &        124,'esprc','esprc file',IER,52)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(llitem,'(see help)','ESP-r esprc file?',
     &        144,'esprc','esprc file',IER,52)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(llitem,'(see help)','ESP-r esprc file?',
     &      144,'esprc','esprc file',IER,52)
        else
          CALL EASKF(llitem,'(see help)','ESP-r esprc file?',
     &      96,'esprc','esprc file',IER,52)
        endif

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 33  ! redisplay menu.
        endif

C Depending on whether Unix or DOS based setup paths. If DOS
C then check for spaces in name and change / to \.
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

      ELSEIF(INO1.EQ.8)THEN

C Edit the installed file `climatelist'.
        call isunix(unixok)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        llitem=cdblfil
        llt=lnblnk(llitem)
        H(1) ='The edit `climatelist` file allows you to change the'
        H(2) ='list of climate files available for browsing. TAKE CARE'
        H(3) ='when editing this file. '
        H(4) =' '
        H(5) ='The blocks of text that form an entry within this file'
        H(6) ='are typically created within the clm module and both'
        H(7) ='the text block and the climatelist file can be edited'
        H(8) ='clm. '
        H(9) =' '
        H(10)='Key words in the file are: '
        H(11)=' '
        H(12)=' *CLIMATE_LIST  - 1st line identifier (leave alone)'
        H(13)=' '
        H(14)=' *group <tab> text for menu'
        H(15)=' '
        H(16)=' *item'
        H(17)='    marks the start of one climate'
        H(18)=' '
        H(19)=' *name <tab> text for menu'
        H(20)=' '
        H(21)=' *aide <tab> description of climate'
        H(22)=' '
        H(23)=' *dbfl <tab> full path to the climate file'
        H(24)=' '
        H(25)=' *winter_s  winer season tab has 4 numbers for:'
        H(26)='    winter begining January 1 and 4 numbers for'
        H(27)='    winter ending 31 Dec. Each set of 4 is:'
        H(28)='   start day, start month, finish day, finish month'
        H(29)=' '
        H(30)=' *spring_s has 4 numbers representing start and end'
        H(31)='     of spring and 4 numbers representing start and'
        H(32)='     end of autumn season'
        H(33)=' '
        H(34)=' *summer_s has 4 numbers representing start and end'
        H(35)='     of summer season'
        H(36)=' '
        H(37)=' *winter_t is like *winter_s but for a typical period'
        H(38)=' *spring_t is like *spring_s but for typical periods'
        H(39)=' *summer_t is like *summer_s but for typical period'
        H(40)=' '
        H(41)=' *avail <tab>  ONLINE'
        H(42)=' '
        H(43)=' *help_start'
        H(44)='    signals start of a block of contextual help'
        H(45)=' '
        H(46)=' *help_end'
        H(47)='    signals end of a block of contextual help'
        H(48)=' '
        H(49)='After edting and saving the information is available'
        H(50)='the next time applications are invoked. '
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(llitem,'(see help)','ESP-r climatelist file?',
     &        96,'default','climatelist file',IER,50)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(llitem,'(see help)','ESP-r climatelist file?',
     &        124,'default','climatelist file',IER,50)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(llitem,'(see help)','ESP-r climatelist file?',
     &        144,'default','climatelist file',IER,50)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(llitem,'(see help)','ESP-r climatelist file?',
     &      144,'default','climatelist file',IER,50)
        else
          CALL EASKF(llitem,'(see help)','ESP-r climatelist file?',
     &      96,'default','climatelist file',IER,50)
        endif

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 33  ! redisplay menu.
        endif

C Depending on whether Unix or DOS based setup paths. If DOS
C then check for spaces in name and change / to \.
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
      ELSEIF(INO1.EQ.9)THEN

C Edit the installed file `exemplars'.
        call isunix(unixok)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        llitem=exemfl
        llt=lnblnk(llitem)
        H(1) ='The edit `exemplars` file allows you to change the list'
        H(2) ='of example models. TAKE CARE when editing'
        H(3) ='this file. The key words are:'
        H(4) =' '
        H(5) =' *EXEMPLARS - fist line identifier (leave alone)'
        H(6) =' *help <tab> some text'
        H(7) =' '
        H(8) =' *label <tab> label text'
        H(9) =' '
        H(10)=' *group <tab> name of group'
        H(11)=' '
        H(12)=' *item'
        H(13)='    marks the start of the model information'
        H(14)=' '
        H(15)=' *name <tab> some text for the menu'
        H(16)=' *cfg <tab> full path to model configuration file'
        H(17)=' *root <tab> full path to model folder'
        H(18)=' '
        H(19)='To add an additional item find the relevant group and'
        H(20)='add in the *item line, *cfg line and *root line '
        H(21)=' '
        H(22)='After edting and saving the information is available'
        H(23)='the next time applications are invoked. '
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(llitem,'(see help)','ESP-r exemplars?',
     &        96,'default','exemplars file',IER,23)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(llitem,'(see help)','ESP-r exemplars?',
     &        124,'default','exemplars file',IER,23)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(llitem,'(see help)','ESP-r exemplars?',
     &        144,'default','exemplars file',IER,23)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(llitem,'(see help)','ESP-r exemplars?',
     &      144,'default','exemplars file',IER,23)
        else
          CALL EASKF(llitem,'(see help)','ESP-r exemplars?',
     &      96,'default','exemplars file',IER,23)
        endif

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          goto 33  ! redisplay menu.
        endif

C Depending on whether Unix or DOS based setup paths. If DOS
C then check for spaces in name and change / to \.
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

      ELSEIF(INO1.EQ.10)THEN

C Toggle trace/feedback level.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0
      ELSEIF(INO1.EQ.11)THEN

C Toggle file format upgrade opinion.
        igupgrade=igupgrade+1
        IF(igupgrade.GT.2)ITRC=0
        if(igupgrade.eq.2)then
          h(1)='If you want to scan in all of the model geometry files'
          h(2)='and save them out to the new format without having to'
          h(3)='edit each one this is the place.'
          CALL EASKABC('Geometric upgrade options',' :',
     &      'scan & update all zones','tell me what is out of date',
     &      'cancel',IUW,3)
          if(iuw.eq.1)then
            call mupdate(ier)
          elseif(iuw.eq.2)then
            call usrmsg('this function to be written','...','W')
          endif
        endif
      ELSEIF(INO1.EQ.12)THEN

C Test button for silent zone read testing.

C Add a choice point here - one to carry on with the current model,
C one to start a fresh model, one to just list the details of the
C silent file.
        h(1)='ESp-r can process a file with so-called silent'
        h(2)='instructions to create one or more zones with'
        h(3)='minimal user intervention. '
        h(4)=' '
        h(5)='You can ask for a synopsis of the file or process'
        h(6)='the information in the file. '
        CALL EASKABCD(' Silent file options',' :',
     &      'retrieve description','process data','XX','continue',IW,6)

C Allow user to edit a file.  Use file browser if in GTK mode.
C If vi then spawn a new window to do the editing in.
        LFIL=' '
        H(1)='An ASCII format file can be edited using the current'
        H(2)='defined text editor.'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(LFIL,'Silent zone file name?',
     &      ' ',96,'xxx.txt','ASCII file',IER,2)
          write(longtfile,'(a)') LFIL(1:lnblnk(LFIL))
        elseif(iglib.eq.2)then
          CALL EASKF(longtfile,'Silent zone file name?',
     &      ' ',144,'xxx.txt','ASCII file',IER,2)
        else
          CALL EASKF(LFIL,'Silent zone file name?',
     &      ' ',96,'xxx.txt','ASCII file',IER,2)
          write(longtfile,'(a)') LFIL(1:lnblnk(LFIL))
        endif
        IUNIT=IFIL+1
        if(IW.EQ.1)then
          call silentread(iunit,longtfile,'?',menu,doc,ureturn,ier)
          call edisp(iuout,ureturn)
          call edisp(iuout,menu)
          call edisp248(iuout,doc,80)
        elseif(IW.eq.2)then
          call silentread(iunit,longtfile,'-',menu,doc,ureturn,ier)
          call edisp(iuout,ureturn)
          call edisp(iuout,menu)
          call edisp248(iuout,doc,80)
          return
        elseif(IW.eq.3)then
          continue
        elseif(IW.eq.4)then
          continue
        endif
      ELSEIF(INO1.EQ.13)THEN

C User asked for help.
        CALL PHELPD('general preferences',36,'-',0,0,IER)
      ELSEIF(INO1.EQ.14)THEN
        RETURN
      else
        goto 33
      endif
      goto 33
     
      END
