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


C ********************* SETUP *********************
C Provides the menus and control logic for the res preference menu.

      SUBROUTINE SETUP(ITRU,IER)
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/recov01/pifltog,lpifl
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      DIMENSION SETUPM(10)
      CHARACTER PERST1*14,PERST2*44,PERST3*44
      CHARACTER SETUPM*50,pifltog*4,lpifl*96,ltmp*96
      integer NITMS,ISETUP ! max items and current menu item

      helpinsub='setres'  ! set for subroutine

C Present the preferences menu, ISETUP is the menu index returned. Generate
C the menu text from current common block into via UPSET.
    2 IER=0
      ISETUP=-2

C Get date strings.
      IVBTIM=1
      NTSPH=NTS
      IVETIM=NTSPH*24
      CALL EPERSTR(IYEAR,ISDS,IVBTIM,ISDF,IVETIM,NTSPH,
     &           IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)

      WRITE(SETUPM(1),'(A,A44)')  '  ',PERST3

      IF(IFDAY.EQ.0)THEN
        SETUPM(2)='a Display format for date>> DOY  10             '
      ELSEIF(IFDAY.EQ.1)THEN
        SETUPM(2)='a Display format for date>> 10 Jan              '
      ELSEIF(IFDAY.EQ.2)THEN
        SETUPM(2)='a Display format for date>> Fri 10 Jan          '
      ELSEIF(IFDAY.EQ.3)THEN
        SETUPM(2)='a Display format for date>> 10/01/21            '
      ENDIF

C Display of time of day.
      IF(IFTIME.EQ.0)THEN
        SETUPM(3)='b Display format for time>> 10h30               '
      ELSEIF(IFTIME.EQ.1)THEN
        SETUPM(3)='b Display format for time>> 10.50               '
      ELSEIF(IFTIME.EQ.2)THEN
        SETUPM(3)='b Display format for time>> 0.4166 (day frac)   '
      ELSEIF(IFTIME.EQ.3)THEN
        SETUPM(3)='b Display format for time>> 10:30               '
      ENDIF
      SETUPM(4)=' ______________________________________           '
      SETUPM(5)='              '

C Menu-pick (performance information requested) data.
      if(pifltog(1:3).eq.'OFF')then
        WRITE(SETUPM(6),'(A)')  'd Recording-of-picks OFF '
      else
        WRITE(SETUPM(6),'(A)')  'd Recording-of-picks ON '
      endif
      SETUPM(7)=  ' ______________________________________         '

      SETUPM(8)=  '? help'
      SETUPM(9)=  '- exit menu'

C Instantiate h() array for the menu.
      helptopic='res_preferences_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)
      NITMS=9
      CALL EMENU('res preferences',SETUPM,NITMS,ISETUP)

      IF(ISETUP.EQ.2)THEN

C Allow user to toggle between 'Fri 10 Jan'/'10 Jan'/'DOY 124'/'10/01/21' format.
        IFDAY=IFDAY+1
        IF(IFDAY.GT.3)IFDAY=0
      ELSEIF(ISETUP.EQ.3)THEN

C Allow user to toggle between '10h00'/'10.00'/'0.41666'/'10:00' format. Only
C allow decimal day representation if day format is DOY.
        IFTIME=IFTIME+1
        IF(IFDAY.EQ.0.AND.IFTIME.GT.3)IFTIME=0
        IF(IFDAY.EQ.1.AND.IFTIME.GT.1)IFTIME=0
        IF(IFDAY.EQ.2.AND.IFTIME.GT.1)IFTIME=2
        IF(IFDAY.EQ.3.AND.IFTIME.GT.3)IFTIME=0
      ELSEIF(ISETUP.EQ.5)THEN
        continue
      ELSEIF(ISETUP.EQ.6)THEN
        CALL EASKABC(' Choices for recording menu selections:',' ',
     &    'OFF (default)','ON','continue',IRT,nbhelp)
        if(IRT.eq.1)then
          pifltog='OFF '
          lpifl='UNKNOWN'
        elseif(IRT.eq.2)then
          ltmp=' '
          ltmp=lpifl
           call easks(ltmp,'File to hold sequence of selections:',
     &      ' ',96,'/tmp/res.rdf','file for selections',IER,nbhelp)
          lpifl=ltmp
          pifltog='ON  '
        endif
      ELSEIF(ISETUP.EQ.8)THEN

C Produce help text for the menu.
        helptopic='res_preferences_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('res preferences',nbhelp,'-',0,0,IER)
      ELSEIF(ISETUP.EQ.9)THEN

C Return to main menu.
        RETURN
      ELSE

C Not one of the legal menu choices.
        ISETUP=-1
        goto 2
      ENDIF
      ISETUP=-4
      goto 2

      END
