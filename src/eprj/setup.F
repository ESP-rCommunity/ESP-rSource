C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C file setup.F includes facilities for display of dates, time
C and viewpoints. MODSIT is set to true if default wireframe
C viewpoint is changed.

      SUBROUTINE SETUP(MODSIT,ITRC,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
#include "espriou.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN,IEOUT
      common/FILEP/IFIL
      common/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/texted/tedlbl,teditor
      common/user/browse

C Initial view.
      common/initv/initvt,EYEMI(3),VIEWMI(3),ANGI

C Radiance processor cores.
      integer radcores
      common/radcor/radcores

C Tollerance for surface matching.
      real ANGCC   ! angle between surfaces tolerance
      real CACC    ! tolerance between vertices
      real DACC    ! angle tolerance along line
      real COGCC   ! tolerance between surface COG
      real SNACC   ! tolerance between surface areas
      integer IACC ! number of matching corners outside dist tolerance
      common/matching/ANGCC,CACC,DACC,COGCC,SNACC,IACC

      logical OK,MODSIT,browse,defview,unixok
      logical closea,closeb,closec,closed,concat
      dimension ITEMS1(16)
      character LFIL*96,longtfile*144,llitem*144,longtfiledos*144
      character ITEMS1*47,doit*248,tedlbl*20,teditor*20
      character tmode*8,hold32*32
      character ureturn*8  ! info back from silentread
      integer NITMS,INO1 ! max items and current menu item
      integer iradcores

      helpinsub='setup'      ! set for subroutine
      
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
        ITEMS1(1)='a view parameters: default'
      else
        ITEMS1(1)='a view parameters: user defined'
      endif
      IF(IFDAY.EQ.0)THEN
        ITEMS1(2)='b date format: DOY 10'
      ELSEIF(IFDAY.EQ.1)THEN
        ITEMS1(2)='b date format: 10 Jan'
      ELSEIF(IFDAY.EQ.2)THEN
        ITEMS1(2)='b date format: Fri 10 Jan'
      ENDIF

C Display of time of day.
      IF(IFTIME.EQ.0)THEN
        ITEMS1(3)='c time format: 10h30'
      ELSEIF(IFTIME.EQ.1)THEN
        ITEMS1(3)='c time format: 10.50'
      ELSEIF(IFTIME.EQ.2)THEN
        ITEMS1(3)='c time format: 0.416 (day frac.)'
      ENDIF

      ITEMS1(4)  ='e list a file'
      ITEMS1(5)  ='f edit a file'
      ITEMS1(6)  ='g edit defaults files'
      ITEMS1(7)  ='h edit 3rd party applications'
      ITEMS1(8)  ='i edit climatelist entries'
      ITEMS1(9)  ='j edit exemplars entries'
      IF(ITRC.EQ.0)THEN
        ITEMS1(10)='k feedback: silent'
      ELSEIF(ITRC.EQ.1)THEN
        ITEMS1(10)='k feedback: summary'
      ELSEIF(ITRC.EQ.2)THEN
        ITEMS1(10)='k feedback: detailed'
      ENDIF
      if(igupgrade.eq.0)then
        ITEMS1(11)='l update file format: no'
      elseif(igupgrade.eq.1)then
        ITEMS1(11)='l upgrade file format: to latest'
      endif
      ITEMS1(12)  ='r cores available to Radiance'
      ITEMS1(13)  ='s surface match tolerance'
      ITEMS1(14)  ='? help'
      ITEMS1(15)  ='- exit menu'

      NITMS=15
      CALL EMENU('Preferences',ITEMS1,NITMS,INO1)

      IF(INO1.EQ.1)THEN

C Wire-frame view point.
        if(browse)goto 33
        helptopic='view_point'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABCD(' ','View parameters:',
     &      'default','current','edit','cancel',IW,nbhelp)
        if(IW.EQ.1)then

C Set eye point at standard place and viewed point at COG.
          EYEMI(1)=-100.0; EYEMI(2)=-100.0; EYEMI(3)=100.
          call bndobj(0,ier)
          VIEWMI(1)=VIEWM(1); VIEWMI(2)=VIEWM(2); VIEWMI(3)=VIEWM(3)
          ANGI=40.
          initvt=0
          MODSIT=.true.
        elseif(IW.EQ.2)then

C Set initial view to match current viewing positions.
          EYEMI(1)=EYEM(1); EYEMI(2)=EYEM(2); EYEMI(3)=EYEM(3)
          VIEWMI(1)=VIEWM(1); VIEWMI(2)=VIEWM(2)
          VIEWMI(3)=VIEWM(3)
          ANGI=ANG
          initvt=1
          MODSIT=.true.
        elseif(IW.EQ.3)then
        
C Edit initial view (and set initvt = 1).
 143      WRITE(HOLD32,'(1x,3f8.2)')EYEM(1),EYEM(2),EYEM(3)
          CALL EASKS(HOLD32,' ','Eye point X, Y, Z (m)?',
     &     32,' -100.0 -100.0 100.0 ','eye point coord',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD32,K,EYEMI(1),-999.,999.,'W','Xcord',IER)
          CALL EGETWR(HOLD32,K,EYEMI(2),-999.,999.,'W','Ycord',IER)
          CALL EGETWR(HOLD32,K,EYEMI(3),0.,999.,'W','eye Zcord',IER)
          if(ier.ne.0)goto 143
 141      WRITE(HOLD32,'(1x,3f8.2)')VIEWM(1),VIEWM(2),VIEWM(3)
          CALL EASKS(HOLD32,' ','Viewpoint X, Y, Z (m)?',
     &     32,' 2.0 2.0 1.0 ','viewpoint coord',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD32,K,VIEWMI(1),-999.,999.,'W','Xcord',IER)
          CALL EGETWR(HOLD32,K,VIEWMI(2),-999.,999.,'W','Ycord',IER)
          CALL EGETWR(HOLD32,K,VIEWMI(3),0.,999.,'W','v Zcord',IER)
          if(ier.ne.0)goto 141
          ANGI=ANG
          CALL EASKR(ANGI,' ','Angle of view (1-120)?',
     &             1.0,'W',120.0,'W',40.,'angle ofview',IER,2)
          initvt=1
          MODSIT=.true.
        endif

C Allow user to toggle between 'Fri 10 Jan'/'10 Jan'/'DOY 124' format.
      ELSEIF(INO1.EQ.2)THEN
        IFDAY=IFDAY+1
        IF(IFDAY.GT.2)IFDAY=0

C Allow user to toggle between '10h00'/'10.00'/'0.41666' format. Only
C allow decimal day representation if day format is DOY.
      ELSEIF(INO1.EQ.3)THEN
        IFTIME=IFTIME+1
        IF(IFDAY.EQ.0.AND.IFTIME.GT.2)IFTIME=0
        IF(IFDAY.GE.1.AND.IFTIME.GT.1)IFTIME=0

C List ASCII file. Use file browser if in GTK mode.
      ELSEIF(INO1.EQ.4)THEN
        helptopic='list_file'
        call gethelptext(helpinsub,helptopic,nbhelp)
        longtfile=' '    ! reset return string
  6     CALL EASKXORGTKF(LFIL,' ','ASCII file name?',
     &    'xxx.rep',longtfile,'ASCII file',IER,nbhelp)
        if(ier.eq.-3)then
          goto 33  ! redisplay menu.
        endif
        IUNIT=IFIL+1
        CALL LISTAS(IUNIT,longtfile,IER)
        IF(IER.LT.0)THEN
          helptopic='list_ascii_error'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK('Problem opening file!',
     &               'Retry?',OK,nbhelp)
          IF(OK)GOTO 6
        ENDIF

C Edit an ASCII file. Use file browser if in GTK mode.
C If vi used then spawn a new window.
      ELSEIF(INO1.EQ.5)THEN
        helptopic='edit_ascii'
        call gethelptext(helpinsub,helptopic,nbhelp)
        llitem=' '    ! reset return string
        CALL EASKXORGTKF(LFIL,' ','ASCII file name?',
     &    'xxx.rep',llitem,'ASCII file',IER,nbhelp)
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
     &    longtfile(1:lnblnk(longtfile)),' & '
        call runit(doit,tmode)
        
C Edit the `defaults` file.
      ELSEIF(INO1.EQ.6)THEN
        call isunix(unixok)
        helptopic='edit_default'
        call gethelptext(helpinsub,helptopic,nbhelp)
        llitem=' '    ! reset return string
        CALL EASKXORGTKF(defdbfl,' ','ESP-r defaults file?',
     &    'default',llitem,'defaults file',IER,nbhelp)
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
     &    longtfile(1:lnblnk(longtfile)),' & '
        call runit(doit,tmode)

C Edit `.esprc`.
      ELSEIF(INO1.EQ.7)THEN
        call isunix(unixok)
        helptopic='edit_esprc'
        call gethelptext(helpinsub,helptopic,nbhelp)
        llitem=' '    ! reset return string
        CALL EASKXORGTKF(esprc,' ','ESP-r .esprc file?',
     &    'esprc',llitem,'esprc file',IER,nbhelp)
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
     &    longtfile(1:lnblnk(longtfile)),' & '
        call runit(doit,tmode)

C Edit the `climatelist` file.
      ELSEIF(INO1.EQ.8)THEN
        call isunix(unixok)
        helptopic='edit_climatelist'
        call gethelptext(helpinsub,helptopic,nbhelp)
        llitem=' '    ! reset return string
        CALL EASKXORGTKF(cdblfil,' ','Climatelist file?',
     &    'climatelist',llitem,'climatelist file',IER,
     &    nbhelp)
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
     &    longtfile(1:lnblnk(longtfile)),' & '
        call runit(doit,tmode)
        
C Edit the installed file `exemplars'.
      ELSEIF(INO1.EQ.9)THEN
        call isunix(unixok)
        helptopic='edit_exemplars'
        call gethelptext(helpinsub,helptopic,nbhelp)
        llitem=' '    ! reset return string
        CALL EASKXORGTKF(exemfl,' ','ESP-r exemplars file?',
     &    'exemplars',llitem,'exemplars file',IER,nbhelp)
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
     &    longtfile(1:lnblnk(longtfile)),' & '
        call runit(doit,tmode)

C Toggle trace/feedback level.
      ELSEIF(INO1.EQ.10)THEN
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0

C Toggle file format upgrade options.
      ELSEIF(INO1.EQ.11)THEN
        igupgrade=igupgrade+1
        IF(igupgrade.GT.1)ITRC=0
        if(igupgrade.eq.1)then
          CALL EASKAB(' ','Upgrade options:',
     &      'scan/update all zones','cancel',IUW,0)
          if(iuw.eq.1)then
            call mupdate(ier)
            igupgrade=0
          endif
        endif

C Set number of processor cores available to Radiance.
      ELSEIF(INO1.EQ.12)THEN
        iradcores=radcores
        CALL EASKI(iradcores,' ',
     &    'Processor cores available to Radiance?',
     &       1,'F',4,'W',ITCNST,'rad cores',IERI,0)
        if(ieri.eq.-3)then
          continue
        else
          radcores=iradcores
        endif 
        
C Set tolerances for surface matching.
      ELSEIF(INO1.EQ.13)THEN
        helptopic='surface_corner_detect'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(CACC,' ','Tolerance (m) between vertices?',
     &    0.001,'W',0.2,'W',0.01,'dist tolerance',IER,nbhelp)
        CALL EASKR(ANGCC,' ',
     &    'Tolerance (degrees) between surface normals?',
     &    0.1,'W',20.0,'W',10.0,'surf normal tolerance',IER,nbhelp)
        CALL EASKR(COGCC,' ',
     &    'Tolerance (m) between surface COG?',
     &    0.01,'W',1.5,'W',0.5,'COG dist tolerance',IER,nbhelp)
        CALL EASKR(SNACC,' ',
     &    'Tolerance (m^2) between surface areas?',
     &    0.01,'W',1.0,'W',0.5,'surf area tolerance',IER,nbhelp)
        CALL EASKR(DACC,' ','Corner tolerance (degrees)?',
     &    0.1,'W',5.0,'W',1.0,'degree tolerance',IER,nbhelp)
        CALL EASKI(IACC,' ',
     &    'Number of corners allowed outside distance tolerance?',
     &     0,'F',2,'W',1,'corner tolerance',IER,nbhelp)

      ELSEIF(INO1.EQ.14)THEN

C Help text for menu.
        helptopic='setup_date_format'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('general preferences',nbhelp,'-',0,0,IER)
        
      ELSEIF(INO1.EQ.15)THEN
        RETURN
      else
        goto 33
      endif
      goto 33
     
      END
