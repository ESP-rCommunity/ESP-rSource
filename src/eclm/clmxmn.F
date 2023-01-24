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

C 'CLMXMN' COMPUTES THE MAXIMUM AND MINIMUM VALUES OF ALL CLIMATIC
C PARAMETERS.
C For a list of climatic parameters see climate.h.

      SUBROUTINE CLMXMN
#include "climate.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW

      COMMON/MXMNC/YMAX(MCM),YMIN(MCM),TMAX,TMIN,DT
      COMMON/CGSC/XSC,YSC(MCM),Xadd,Yadd(MCM) 

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
#else
      integer*8 igwid,igheight  ! for use with axiscale
#endif

      real rtmp

C IF PERIOD NOT CHANGED NO NEED TO RECALCULATE
      IF(INEW.EQ.0)GO TO 2
      DO 5 I=1,MCM
        YMAX(I)=-100000.
        YMIN(I)=100000.
   5  CONTINUE

C Re-establish julian days.
      CALL EDAY(ID1,IM1,IIDS)
      CALL EDAY(ID2,IM2,IIDF)
      DO 10 I=IIDS,IIDF

C READ VALUES OF THE CLIMATIC PARAMETERS FOR THIS DAY.
      ID=I
      CALL CLMGET(ID,IER)
      JS=1
      JF=24
      IF(I.EQ.IIDS)JS=IT1
      if(JS.eq.0)JS=1
      IF(I.EQ.IIDF)JF=IT2
      DO 20 J=JS,JF

      DO 30 K=1,MCM
        if (CMXST(K)) then

C   GET RELEVANT VALUES
          Y=CMRVAL(K,J)

C SAVE MAXIMUM AND MINIMUM VALUES.
          YMAX(K)=AMAX1(YMAX(K),Y)
          YMIN(K)=AMIN1(YMIN(K),Y)
        endif
   30 CONTINUE
   20 CONTINUE
   10 CONTINUE

C   MAX & MIN TIMES
      DT=(IIDF-IIDS)*24+IT2-IT1
      TMIN=IT1-0.5
      TMAX=IT1+DT+0.5

C   SCALE FOR EACH Y AXIS
C   2, 3 and 4 SHARE SAME AXIS
C   8 and 9 SHARE SAME AXIS
      rtmp=AMAX1(YMAX(2),YMAX(3),YMAX(4))
      YMAX(2)=rtmp; YMAX(3)=rtmp; YMAX(4)=rtmp
      rtmp=AMIN1(YMIN(2),YMIN(3),YMIN(4))
      YMIN(2)=rtmp; YMIN(3)=rtmp; YMIN(4)=rtmp

      rtmp=AMAX1(YMAX(8),YMAX(9))
      YMAX(8)=rtmp; YMAX(9)=rtmp
      rtmp=AMIN1(YMIN(8),YMIN(9))
      YMIN(8)=rtmp; YMIN(9)=rtmp

      DO 70 I=1,MCM
        if (CMXST(I)) then
          A=YMAX(I)-YMIN(I)
          IF(A.GT.0.1)GO TO 75
          YMAX(I)=YMAX(I)+1.0
          YMIN(I)=YMIN(I)-1.0
          A=2.0
   75     igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,TMIN,TMAX,YMIN(I),YMAX(I),XSC,
     &                  bysc,sca,Xadd,addY)
          YSC(I)=bysc
          Yadd(I)=addY
        endif
  70  CONTINUE
      INEW=0
   2  CONTINUE
      RETURN

      END
