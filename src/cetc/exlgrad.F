C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004. Please Contact Ian Beausoliel-Morrison for details 
C concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.
       SUBROUTINE exlgrad(modsit)
#include "help.h"

C *******************
C
C created by Maria Mottillo
C June 20, 2002
C
C exlgrad reads in the monthly ground temperature profile
C that is used to calculate the contribution of the ground to the 
C mean black body equivalent temperature (which is a function of sky, ground,
C and surrounding obstruction surfaces temperatures). This equivalent
C temperature is used to calculate the net longwave radiation exchange
C between external building surfaces.
C
C INPUTS:
C   modsit          flag indicating whether a modification has been made
C                   and the cfg file needs to be updated.
C  
C OUTPUTS:
C  iExtLongRadFlag  flag indicating whether a ground profile has been entered
C                   or edited. .iExtLgRadFlag=1 indicates yes; 0 indicates no.
C  eGrdTp           user input ground temperature profile (array).
C
C This subroutine uses the ESP-r subroutines EDISP, EASKAB, EASKS, iCountWords
C and EGETWR. A description of what each of these subroutines does is 
C provided below.
C
C  The subroutine EDISP is in the src/lib/ directory. EDISP sends text
C  to scrolling display
C
C  The subroutine EASKAB is in the src/lib/ directory. EASKAB is a 
C  generic choice facilty returning 1 or 2 according to which of the
C  options has been chosen.
C  EASKAB(MSG1, MSG2, AOPT,BOPT,IWHICH,NHELP)
C  MSG1 and MSG2 are prompts, AOPT and BOPT are strings describing the
C  options and IRT is the value returned (1 or 2). NHELP is the number
C  of help lines.
C
C  EASKS is a facility for asking a text string.
C  EASKS(STRVAL,PROMP1,PROMP2,ISTRW,DSTR,ERMSG,IER,NHELP)
C  STRVAL is the string returned. PROMP1 and PROMP2 are the prompts.
C  ISTRW is the length of the string returned. DSTR is a string to
C  use as default. ERMSG is a string placed at the top of the selection
C  menu. IER is error state (0 okay). NHELP is number of help lines.
C  H() is the help array.
C  EASKS subroutine is in the src/lib/ directory.
C
C  iCountWords checks a character string and returns the number of items
C  separated by ' ', tab or ','.
C  iCountWords is in the src/lib/ directory
C
C  EGETWR is found in the src/lib/ directory.
C  EGETWR(STRING,K,RV,RMN,RMX,RACT,MSG,IER)
C  EGETWR gets first word after position K from the STRING of
C  characters and converts it into a real number RV. 
C  Position K is moved up within subroutine.
C  RV is tested against minimum RMN and RMX.
C  If RACT = 'W' a warning message is provided. If RACT = 'F' a failure
C  message is provided. If RACT = '-', no range checking is done.
C  IER is error state (0 is okay).
C  Words may be separated by blanks, tabs or commas. 

        integer iCountWords
           
        CHARACTER*124  OUTS
        CHARACTER*72   hold
        LOGICAL        modsit
        REAL           getVal
        REAL           eGrdTp(12)
        INTEGER        IRT
        INTEGER        IUOUT
        INTEGER        IUIN
        INTEGER        IEOUT
        INTEGER        IER
        INTEGER        jMon
        INTEGER        k
        INTEGER        iExtLgRadFlag
        INTEGER        numVal  


C Common block created to store flag and user-input values.
        COMMON/LongRad/iExtLgRadFlag,eGrdTp

C These common blocks are copied over from EDSITE to 
C define global variables needed.
        COMMON/OUTIN/IUOUT,IUIN,IEOUT

        CALL EDISP(IUOUT,' ')     

        h(1)='This monthly ground temperature profile is used'
        h(2)='to calculate the contribution of the ground' 
        h(3)='to the mean black body equivalent temperature.' 
        h(4)='The mean black body equivalent temperature'
        h(5)='is used to calculate the net longwave radiation'
        h(6)='exchange between external building surfaces.' 
        h(7)='One profile with 12 monthly ground'
        h(8)='temperatures may be defined.'

C Two options available: user can list profile or edit/add profile.

        CALL EASKAB(' Options:',' ','list profile','edit/add profile', 
     &              IRT,8)


        IF(IRT.EQ.1)THEN
C User has selected to list profile.
C If profile exists, then display, otherwise display message.

           IF(iExtLgRadFlag.EQ.1)THEN
             CALL EDISP(IUOUT,' Ground temperature profile Jan-Dec:')
             WRITE(OUTS,'(12F6.1)')(eGrdTp(jMon),jMon=1,12)
             CALL EDISP(IUOUT,OUTS)
           ELSE 
             CALL EDISP(IUOUT, 'There are no profiles entered.')
           ENDIF

        ELSEIF(IRT.EQ.2)THEN
C User has selected to edit/add profile.
C Default values obtained from MZELWE. If hard-coded profile in MZELWE
C is modified, change should be reflected here as well.
  
 10        WRITE(hold,'(1X,6F7.2)')(eGrdTp(jMon),jMon=1,6)

           h(1)='Temperatures are typically at 1.5m depth'    

C Ask for first 6 temperatures
           CALL EASKS(hold,' Temperatures January-June:',' ',72,
     &            '7.28   6.50   6.56   7.33   9.06   11.39 ',
     &            'gr prof jan-jun',ier,1)
           numVal = iCountWords(hold)
           IF(numVal.NE.6) goto 10
           k=0

C If there is no error then store values in array.
           DO jMon=1,6
             CALL EGETWR(hold,k,getVal,0.,0.,'-','profile',IER)
             IF(IER.NE.0)goto 10
             eGrdTp(jMon)=getVal
           END DO

 20        hold = ' '

C Ask for remaining 6 temperatures.

           WRITE(hold,'(1X,6F7.2)')(eGrdTp(jMon),jMon=7,12)

           h(1)='Temperatures are typically at 1.5m depth.'

           CALL EASKS(hold,' Temperatures July-December:',' ',72,
     &             '13.50   14.33   14.11   11.72   10.72   8.67  ',
     &             'gr prof jul-dec',ier,1)
           numVal = iCountWords(hold)

           IF(numVal.NE.6)goto 20
           k=0

C If there is no error then store values in array.
           DO jMon=7,12
             CALL EGETWR(hold,k,getVal,0.,0.,'-','profile',IER)
             IF(IER.NE.0)goto 20
             eGrdTp(jMon)=getVal
           END DO

C  Set MODSIT to true if modifications made. This will flag that
C  cfg file needs to be updated.
           modsit=.true.

C Set flag that profile has been entered.
           iExtLgRadFlag = 1

        END IF

        END
