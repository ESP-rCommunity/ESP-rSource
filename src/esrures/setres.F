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

C This file (setres.f) contains the following routine:

C SETUP: provides the menus and control logic for the res preferences menu.

C ********************* SETUP *****************************************

C SETUP provides the menus and control logic for the res preference menu.
      SUBROUTINE SETUP(ITRU,IER)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/pophelp/h(60)
      common/recov01/pifltog,lpifl
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      DIMENSION SETUPM(10)
      CHARACTER PERST1*14,PERST2*44,PERST3*44
      CHARACTER SETUPM*50,H*72,pifltog*4,lpifl*72,ltmp*72

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
      ENDIF

C Display of time of day.
      IF(IFTIME.EQ.0)THEN
        SETUPM(3)='b Display format for time>> 10h30               '
      ELSEIF(IFTIME.EQ.1)THEN
        SETUPM(3)='b Display format for time>> 10.50               '
      ELSEIF(IFTIME.EQ.2)THEN
        SETUPM(3)='b Display format for time>> 0.4166 (day frac)   '
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

      SETUPM(8)=  '? Help                                          '
      SETUPM(9)=  '- Exit to main menu                             '

C Instanciate h() array for the menu.
      H(1)='The preference menu allows the format of the `res` '
      H(2)='environment to be modified. '
      H(3)=' '
      H(4)='For ease of viewing, several font sizes are available.'
      H(5)='Remember that the size of the window may be resized '
      H(6)='via the Environment button so that complex images '
      H(7)='may be viewed in greater detail.'

      CALL EMENU('res preferences',SETUPM,9,ISETUP)

      IF(ISETUP.EQ.2)THEN

C Allow user to toggle between 'Fri 10 Jan'/'10 Jan'/'DOY 124' format.
        IFDAY=IFDAY+1
        IF(IFDAY.GT.2)IFDAY=0
      ELSEIF(ISETUP.EQ.3)THEN

C Allow user to toggle between '10h00'/'10.00'/'0.41666' format. Only
C allow decimal day representation if day format is DOY.
        IFTIME=IFTIME+1
        IF(IFDAY.EQ.0.AND.IFTIME.GT.2)IFTIME=0
        IF(IFDAY.GE.1.AND.IFTIME.GT.1)IFTIME=0
      ELSEIF(ISETUP.EQ.5)THEN
        continue
      ELSEIF(ISETUP.EQ.6)THEN
        h(1)='You can record a sequence of menu selections for use in'
        h(2)='driving res to automatically recover the same performance'
        h(3)='information. '
        CALL EASKABC(' Choices for recording menu selections:',' ',
     &     'OFF (default)','ON','continue',IRT,3)
        if(IRT.eq.1)then
          pifltog='OFF '
          lpifl='UNKNOWN'
        elseif(IRT.eq.2)then
          ltmp=' '
          ltmp=lpifl
           call easks(ltmp,'File to hold sequence of selections:',
     &    ' ',72,'/tmp/res.rdf','file for selections',IER,3)
          lpifl=ltmp
          pifltog='ON  '
        endif
      ELSEIF(ISETUP.EQ.8)THEN

C Produce help text for the menu.
        CALL PHELPD('res preferences',7,'-',0,0,IER)
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
