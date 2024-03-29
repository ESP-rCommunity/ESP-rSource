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


C file setup.F includes facilities for display of dates, time and files.
      SUBROUTINE SETUP(IER)
#include "help.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN,IEOUT
      common/FILEP/IFIL
      common/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/texted/tedlbl,teditor
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical OK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,concat,CFCDBOK
      dimension ITEMS1(18)
      character LFIL*72,SFILE*72,tfile*72,longtfile*144
      character OUTSTR*124,ITEMS1*47,doit*248,tedlbl*20,teditor*20
      character PERST1*14,PERST2*44,PERST3*44,tmode*8
      integer nitms,INO1 ! max items and current menu item

   33 INO1=-4
      helpinsub='setup'  ! set for subroutine
      helptopic='various_setup_inputs'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Get date strings.
      IVBTIM=1
      NTSPH=1
      IVETIM=NTSPH*24
      CALL EPERSTR(IYEAR,IBDOY,IVBTIM,IEDOY,IVETIM,NTSPH,
     &           IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)

      WRITE(ITEMS1(1),'(A,A44)')  'a ',PERST3
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

      ITEMS1(4)  ='            '

      ITEMS1(5)  ='  _________________________________           '
      ITEMS1(6)  ='e look at ASCII file    '
      ITEMS1(7)  ='f edit an ASCII file    '
      ITEMS1(8)  ='g strip file annotations'
      ITEMS1(9)  ='h miscellaneous tests   '
      ITEMS1(10) ='? help                  '
      ITEMS1(11) ='- exit menu             '
      nitms=11
      CALL EMENU('Tool preferences',ITEMS1,nitms,INO1)
      IF(INO1.EQ.1)THEN

C Define the beginning and ending dates for simulation.
        CALL EASKI(IYEAR,' ','Please enter year?',
     &    1900,'W',2051,'W',2007,'year',IER,nbhelp)
        CALL EASKPER('Assessment period:',IBDOY,IEDOY,IFDAY,IER)
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
        continue
      ELSEIF(INO1.EQ.6)THEN

C List out an ASCII file.
    6   CALL EASKS(LFIL,' ASCII file name?',
     &    ' ',72,' ','ASCII file',IER,nbhelp)

        IUNIT=IFIL+1
        CALL LISTAS(IUNIT,LFIL,IER)
        IF(IER.LT.0)THEN
          CALL EASKOK('Problem opening ASCII file!',
     &               'Retry?',OK,nbhelp)
          IF(OK)GOTO 6
        ENDIF
      ELSEIF(INO1.EQ.7)THEN

C Allow user to edit a file. Append to path if necessary.
C If vi then spawn a new window to do the editing in.
        CALL EASKS(LFIL,' ASCII file to edit ? ',
     &    ' ',72,' ','ASCII file',IER,nbhelp)
        call addpath(LFIL,longtfile,concat)
        tmode='-'
        if(teditor(1:2).eq.'vi'.or.teditor(1:4).eq.'nano')tmode='text'
        write(doit,'(a,2x,a,a)') teditor(1:lnblnk(teditor)),
     &    longtfile(1:lnblnk(longtfile)),' & '
        call runit(doit,tmode)
      ELSEIF(INO1.EQ.8)THEN

C Ask for commented file name and open it, IER=-301 if not found.
    8   CALL EASKS(LFIL,' Commented file name?',
     &      ' ',72,' ','commented file name',IER,nbhelp)

        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,LFIL,1,IER)
        IF(IER.LT.0)GOTO 8

C Ask for name of file to put uncommented text into.
    9   CALL EASKS(SFILE,' Uncommented file name?',
     &    ' ',72,' ','Uncommented file name',IER,nbhelp)

c Place output into IFIL+2, first close that unit, open any exiting
C file with this name, delete it and create a new file.
        ISUNIT=IFIL+2
        CALL EFOPSEQ(ISUNIT,SFILE,4,IER)
        IF(IER.LT.0)GOTO 9

C Loop until end of file encountered and display stripped text.
   73   CALL STRIPC(IUNIT,OUTSTR,0,ND,0,'stripping',IER)
        IF(IER.EQ.2)goto 72
        IF(IER.EQ.1)GOTO 33
        WRITE(ISUNIT,'(a)')OUTSTR(1:LNBLNK(OUTSTR))
        CALL edisp(iuout,OUTSTR)
        goto 73
   72   CALL USRMSG(' ',' reached end of source file ','-')
        CALL ERPFREE(ISUNIT,ISTAT)
        GOTO 33
      ELSEIF(INO1.EQ.9)THEN

C Place miscellaneous testing applications here.
        CALL USRMSG(' ','No options offered at this time.','-')

      ELSEIF(INO1.EQ.10)THEN
        CALL PHELPD('geneeral preferences',nbhelp,'-',0,0,IER)
      ELSEIF(INO1.EQ.11)THEN
        RETURN
      else
        goto 33
      endif
      goto 33
     
      END
