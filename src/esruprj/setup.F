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

C << This would be a great place to add in functions to edit and
C << maintain the .esprc file and the defaults file!

      SUBROUTINE SETUP(MODSIT,ITRC,IER)

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

      logical OK,DOK,MODSIT,browse,defview,unixok
      logical closea,closeb,closec,closed
      dimension ITEMS1(10)
      character LFIL*96,H*72,longtfile*144
      character ITEMS1*47,doit*248,tedlbl*20,teditor*20
      character tmode*8,hold32*32

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
      ITEMS1(6)  ='g                                             '
      IF(ITRC.EQ.0)THEN
        ITEMS1(7)='h feedback >> silent                          '
      ELSEIF(ITRC.EQ.1)THEN
        ITEMS1(7)='h feedback >> summary                         '
      ELSEIF(ITRC.EQ.2)THEN
        ITEMS1(7)='h feedback >> detailed                        '
      ENDIF
      ITEMS1(8)  ='? help                                        '
      ITEMS1(9)  ='- exit this menu                              '
      CALL EMENU('Tool preferences',ITEMS1,9,INO1)
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
        H(2)='defined text editor.'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
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
        tmode='graph'
        if(teditor(1:2).eq.'vi')tmode='text'
        write(doit,'(a,2x,a,a)') teditor(1:lnblnk(teditor)),
     &    longtfile(1:lnblnk(longtfile)),' &'
        call runit(doit,tmode)
      ELSEIF(INO1.EQ.6)THEN
        continue
      ELSEIF(INO1.EQ.7)THEN

C Toggle trace/feedback level.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0
      ELSEIF(INO1.EQ.8)THEN
        H(1)='This menu allows the display format for dates and'
        H(2)='times to be changed.  The active year can also be'
        H(3)='defined as required by controllers that require day'
        H(4)='of the week information.'
        H(5)=' '
        H(6)='One option lists a specified ASCII file - sometimes'
        H(7)='useful if you have need to directly view the contents'
        H(8)='of a problem describing entity such as the system'
        H(9)='configuration file or a zone geometry file.  '
        H(10)=' '
        H(11)='The wireframe view option allows you to set a different'
        H(12)='initial wireframe view.'
        H(13)=' '
        H(14)='The external editor is currently defined to be:'
        write(H(15),'(3a)')'      ',teditor(:lnblnk(teditor)),'.'
        H(16)='This can be re-defined in your .esprc file.'
        H(17)=' '
        H(18)='The feedback level allows you to see more or less '
        H(19)='information as model files are being scanned. '
        CALL PHELPD('geneeral preferences',19,'-',0,0,IER)
      ELSEIF(INO1.EQ.9)THEN
        RETURN
      else
        goto 33
      endif
      goto 33
     
      END
