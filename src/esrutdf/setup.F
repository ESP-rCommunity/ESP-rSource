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

C setup.f of TDF
C The following routines are included:
C SETUP: provides the menu's and control logic for the preference menu.

C ********************* SETUP 
C SETUP provides the menu's and control logic for the preference menu.
      SUBROUTINE SETUP(IER)

      common/pophelp/h(60)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/TDFIO/LIMP,IUIMP,ITIMP,LEXP,IUEXP,ITEXP
      LOGICAL XST

      DIMENSION SETUPM(7)
      CHARACTER*72 LIMP,LEXP
      CHARACTER SETUPM*50,H*72,DFILE*72,tfile*72

C Present the preference menu, ISETUP is the menu index returned. Generate
C the menu text from current common block into via UPSET.
    2 IER=0
      ISETUP=-2

C Import file.
      IF(ITIMP.EQ.0)THEN
        WRITE(SETUPM(1),'(A,A20)')'h Import file: UNKNWN Name: ',
     &        LIMP(1:20)
      ELSEIF(ITIMP.EQ.1)THEN
        WRITE(SETUPM(1),'(A,A20)')'h Import column text  Name: ',
     &        LIMP(1:20)
      ENDIF

C Export file.
      IF(ITEXP.EQ.0)THEN
        WRITE(SETUPM(2),'(A,A20)')'i Export type: UNKNWN Name: ',
     &        LEXP(1:20)
      ELSEIF(ITEXP.EQ.1)THEN
        WRITE(SETUPM(2),'(A,A20)')'i Export ASCII temporal Name: ',
     &        LEXP(1:19)
      ENDIF
      SETUPM(3)  ='  _____________________________ '

C Date display format.
      IF(IFDAY.EQ.0)THEN
        SETUPM(4)='j Display format for date>> DOY  10             '
      ELSEIF(IFDAY.EQ.1)THEN
        SETUPM(4)='j Display format for date>> 10 Jan              '
      ELSEIF(IFDAY.EQ.2)THEN
        SETUPM(4)='j Display format for date>> Fri 10 Jan          '
      ENDIF

C Display of time of day.
      IF(IFTIME.EQ.0)THEN
        SETUPM(5)='k Display format for time>> 10h30               '
      ELSEIF(IFTIME.EQ.1)THEN
        SETUPM(5)='k Display format for time>> 10.50               '
      ELSEIF(IFTIME.EQ.2)THEN
        SETUPM(5)='k Display format for time>> 0.4166 (day frac)   '
      ENDIF

      SETUPM(6)=  '? Help                                          '
      SETUPM(7)=  '- Exit ^Main Menu^                              '

C Help text for this menu.
      H(1)='The import facility is available to allow users with '
      H(2)='tabular data in a text file to select one or more '
      H(3)='columns of such information and associate it with a '
      H(4)='DB item which is of type TABULAR.  First define the '
      H(5)='import file and its format in this menu, then select '
      H(6)='the item which is to be filled and issue an import  '
      H(7)='command. '
      H(8)=' '
      H(9)='The export command... '
      H(10)=' '
      H(11)='The date and time formats may be used to alter the '
      H(12)='display as well as that of any exported data. '

C Menu control.
      CALL EMENU('Preferences',SETUPM,7,ISETUP)

      IF(ISETUP.EQ.1)THEN

C Define a standard import file name & format for use by other parts of TDF.
        H(1)='It is possible to import temporal information via an'
        H(2)='ASCII file with columns of data.'
        CALL EASKABC('Import options:',' ',
     &    'UNKNOWN','ASCII column text','continue',IWM,2)
        if(IWM.ge.3)goto 2
        ITIMP=IWM-1

        H(1)='One import source file will be assumed throughout this'
        H(2)='session. If this needs to be altered then the user'
        H(3)='should return to this menu to give the new name.'
        DFILE='data.imp'
        tfile=LIMP
        CALL EASKS(tfile,' Import file name? ',
     &   ' ',72,DFILE,'import file name',IER,3)
        if(tfile(1:2).ne.'  ')LIMP=tfile
        IF(ITIMP.EQ.1)THEN
          CALL ERPFREE(IUIMP,ISTAT)
          call findfil(LIMP,XST)
          IF(XST)THEN
            CALL USRMSG(' this file exists',' ','-')
          ELSE
            CALL USRMSG(' this file will be created',' ','-')
          ENDIF
        ENDIF
      ELSEIF(ISETUP.EQ.2)THEN

C Define a standard export file name and format for use by other parts
C of TDF.  Ask for the type then see if it exists.
        H(1)='It is possible to export information from the db via'
        H(2)='a number of file formats (ASCII currently available).'
        CALL EASKI(ITEXP,' Several export formats are available: ',
     &    '  0)UNKNOWN, 1)ASCII, 2)BINARY, 3)GRTOOL ? ',
     &       0,'F',3,'F',1,'export format',IER,2)

        H(1)='One export file will be used as the default name which'
        H(2)='the user may override.  Return to this menu to change'
        H(3)='the default name.'
        DFILE='data.exp'
        tfile=LEXP
        CALL EASKS(tfile,' Export file name? ',
     &   ' ',72,DFILE,'export file name',IER,3)
        if(tfile(1:2).ne.'  ')LEXP=tfile

        IF(ITEXP.EQ.1)THEN
          CALL ERPFREE(IUEXP,ISTAT)
          call findfil(LEXP,XST)
          IF(XST)THEN
            CALL USRMSG(' export file exists ',' ','-')
          ELSE
            CALL USRMSG(' new export file will be opened',' ','-')
          ENDIF
        ENDIF
      ELSEIF(ISETUP.EQ.4)THEN

C Allow user to toggle between 'Fri 10 Jan'/'10 Jan'/'DOY 124' format.
        IFDAY=IFDAY+1
        IF(IFDAY.GT.2)IFDAY=0
      ELSEIF(ISETUP.EQ.5)THEN

C Allow user to toggle between '10h00'/'10.00'/'0.41666' format. Only
C allow decimal day representation if day format is DOY
        IFTIME=IFTIME+1
        IF(IFDAY.EQ.0.AND.IFTIME.GT.2)IFTIME=0
        IF(IFDAY.GE.1.AND.IFTIME.GT.1)IFTIME=0
      ELSEIF(ISETUP.EQ.6)THEN

C Respond to help requests.
        CALL PHELPD('tdf preferences',12,'-',0,0,IER)
      ELSEIF(ISETUP.EQ.7)THEN
        RETURN
      ELSE

C Not one of the legal menu choices.
        ISETUP=-1
        goto 2
      ENDIF
      ISETUP=-4
      goto  2

      END



