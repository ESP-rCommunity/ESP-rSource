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

C WINSHD of ESRUshd
C 'WINSHD' allows shading prediction for one window only, making use
C of the ASHRAE method.  The user is prompted for dimensional details
C relating to window shading devices.  After computation the predicted
C 24 hour shading profile can be associated with any user-specified
C object body window.
      SUBROUTINE WINSHD
#include "building.h"

      common/pophelp/h(60)
      COMMON/OUTPCH/ICOUT
      COMMON/SHAD1/ISUNUP
      COMMON/SANGL1/SAZI1,SALT1
      COMMON/WINS/ISUN(MT),SHD(MT)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/PREC8/SLAT,SLON
      DIMENSION SHDX(20),MONTH(12)
      character outs*124,H*72,BLKNAME*8,BLKMAT*12
      logical OK,dok

      DATA MONTH/31,28,31,30,31,30,31,31,30,31,30,31/


      SHDX(1)=1.
      h(1)='Width is in metres. '
      CALL EASKR(SHDX(1),' ',' Window: width ? ',
     &   0.0,'F',99.0,'W',0.0,'window width',IER,1)
 
      SHDX(2)=1.
      h(1)='Height is in metres. '
      CALL EASKR(SHDX(2),' ',' Window: height ? ',
     &   0.0,'F',99.0,'W',0.0,'window height',IER,1)

C Overhang data.
      SHDX(3)=0.0
      SHDX(4)=0.0
      SHDX(5)=0.0
      SHDX(6)=0.0
      SHDX(7)=0.0

      dok=.false.
      h(1)='Option to define an overhang. '
      CALL ASKOK(' ',' Is there an overhang ?',OK,dok,1)
      IF(.NOT.OK)goto 108

      SHDX(3)=0.1
      h(1)='Overhang depth is in metres. '
      CALL EASKR(SHDX(3),' ',' Overhang: depth ? ',
     &   0.0,'F',99.0,'W',0.0,'Overhang: depth',IER,1)

      SHDX(4)=0.1
      h(1)='Overhang distance is in metres. '
      CALL EASKR(SHDX(4),' ',
     &   ' Overhang: distance from top of window ? ',
     &   0.0,'F',99.0,'W',0.0,'Overhang: dist from top',IER,1)

      SHDX(5)=0.1
      h(1)='Overhang extension is in metres. '
      CALL EASKR(SHDX(5),' ',
     &   ' Overhang: extension past left window edge ? ',
     &   0.0,'F',99.0,'W',0.0,'Overhang: exten left',IER,1)

      SHDX(6)=0.1
      h(1)='Overhang extension is in metres. '
      CALL EASKR(SHDX(6),' ',
     &   ' Overhang: extension past right window edge ? ',
     &   0.0,'F',99.0,'W',0.0,'Overhang: exten right',IER,1)

      SHDX(7)=0.1
      h(1)='Overhang dimension is in metres. '
      CALL EASKR(SHDX(7),' ',
     &   ' Overhang: dimension of end vertical feature ? ',
     &   0.0,'F',99.0,'W',0.0,'Overhang: vert feature',IER,1)
  108 SHDX(8)=0.0
      SHDX(9)=0.0
      SHDX(10)=0.0
      SHDX(11)=0.0

      dok=.false.
      h(1)='Option to define left-side fin. '
      CALL ASKOK(' ',' Is there a left-side fin ?',OK,dok,1)
      IF(.NOT.OK)goto 103

      SHDX(8)=0.1
      h(1)='Left side fin depth is in metres. '
      CALL EASKR(SHDX(8),' ',' Left side fin: depth ? ',
     &   0.0,'F',99.0,'W',0.0,'Left fin depth',IER,1)

      SHDX(9)=0.1
      h(1)='Left side fin extension is in metres. '
      CALL EASKR(SHDX(9),' Left side fin:',
     &  ' extension above top of window ? ',
     &   0.0,'F',99.0,'W',0.0,'Left fin ext top',IER,1)

      SHDX(10)=0.1
      h(1)='Left side fin distance is in metres. '
      CALL EASKR(SHDX(10),' Left side fin:',
     &  ' distance from window edge ? ',
     &   0.0,'F',99.0,'W',0.0,'Left fin dis win edge',IER,1)

      SHDX(11)=0.1
      h(1)='Left side fin stop-short distance is in metres. '
      CALL EASKR(SHDX(11),' Left side fin:',
     &  ' stop-short distance from window bottom ? ',
     &   0.0,'F',99.0,'W',0.0,'Left fin stop short',IER,1)
  103 SHDX(12)=0.0
      SHDX(13)=0.0
      SHDX(14)=0.0
      SHDX(15)=0.0

      dok=.false.
      h(1)='Option to define right-side fin. '
      CALL ASKOK(' ',' Is there a right-side fin ?',OK,dok,1)
      IF(.NOT.OK)goto 71

      SHDX(12)=0.1
      h(1)='Right side fin distance is in metres. '
      CALL EASKR(SHDX(12),' ',' Right side fin: depth ? ',
     &   0.0,'F',99.0,'W',0.0,'Right fin depth',IER,1)

      SHDX(13)=0.1
      h(1)='Right side fin extension is in metres. '
      CALL EASKR(SHDX(13),' Right side fin:',
     &  ' extension above top of window ? ',
     &   0.0,'F',99.0,'W',0.0,'R fin ext top',IER,1)

      SHDX(14)=0.1
      h(1)='Right side fin distance is in metres. '
      CALL EASKR(SHDX(14),' Right side fin:',
     &  ' distance from window edge ? ',
     &   0.0,'F',99.0,'W',0.0,'R fin dis win edge',IER,1)

      SHDX(15)=0.1
      h(1)='Right side fin stop-short distance is in metres. '
      CALL EASKR(SHDX(15),' Right side fin:',
     &  ' stop-short distance from window bottom ? ',
     &   0.0,'F',99.0,'W',0.0,'R fin stop short',IER,1)

  71  WAZI=0.
      h(1)='Window: azimuth angle is in degrees. '
      CALL EASKR(WAZI,' ',
     &  ' Window: azimuth angle (degrees, clockwise from north) ? ',
     &   0.0,'F',360.0,'F',0.0,'azimuth',IER,1)

C Convert to radians relative to south.
      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      WAZI=(WAZI-180.)*R
      SHDX(16)=WAZI

      h(1)='month index 1 to 12. '
      IM=1
      CALL EASKI(IM,' ',' Computation month ? ',
     &  1,'F',12,'F',1,'comp month',IER,1)
      ID=1
      h(1)='day of the month 1 - 31 (with exceptions). '
      CALL EASKI(ID,' ',' Computation day ? ',
     &  1,'F',MONTH(IM),'F',1,'comp day',IER,1)

C Commence hour-by-hour computation.
      CALL EDAY(ID,IM,IYD)
      DO 10 I=1,24
        II=I
        STIME=FLOAT(II)
        CALL EAZALTS(STIME,IYD,SLAT,SLON,ISUNUP,SAZI,SALT)
        SAZI1=SAZI
        SALT1=SALT
        ISUN(I)=ISUNUP
        IF(ISUNUP.EQ.0)goto 10
        PHI=(SAZI1-180.)*R
        COSZ=COS((90.-SALT1)*R)
        CALL WSHAD(SHDX,PHI,COSZ,SHRAT)

C 'SHRAT' is insolated portion.
        SHD(I)=1.-SHRAT
   10 CONTINUE

      WRITE(outs,85)ID,IM
   85 FORMAT(' Day',I3,' of month',I3)
      call edisp(icout,outs)
      call edisp(icout,' Hour Shading ')

      DO 20 I=1,24
        IF(ISUN(I).EQ.0)then
          WRITE(outs,'(I3,A)')I,'   No sun'
          call edisp(icout,outs)
        else
          WRITE(outs,'(I4,F7.2)')I,SHD(I)
          call edisp(icout,outs)
        endif
   20 CONTINUE
      RETURN
      END
