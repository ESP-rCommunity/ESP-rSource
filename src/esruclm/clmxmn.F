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

C 'CLMXMN' COMPUTES THE MAXIMUM AND MINIMUM VALUES OF THE CLIMATIC
C PARAMETERS IDENTIFIED BY 'IP' PASSED OVER FROM SUBROUTINE 'CLMGRF'.
C IP = 1  - DIFFUSE HORIZONTAL SOLAR RADIATION HELD IN IX(1,MT).
C IP = 2  - EXTERNAL AIR TEMPERATURE HELD IN IX(2,MT).
C IP = 3  - DIRECT NORMAL SOLAR RADIATION HELD IN IX(3,MT).
C IP = 4  - PREVAILING WIND VELOCITY HELD IN IX(4,MT).
C IP = 5  - WIND DIRECTION
C IP = 6  - EXTERNAL RELATIVE HUMIDITY HELD IN IX(5,MT).

      SUBROUTINE CLMXMN
      PARAMETER (MT=24)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/MXMNC/YMAX(6),YMIN(6),TMAX,TMIN,DT
      COMMON/CGSC/XSC,YSC(6),Xadd,Yadd(6)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      COMMON/CLMDAT/IDY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &           CDIR(MT),CHUM(MT)
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
#else
      integer*8 igwid,igheight  ! for use with axiscale
#endif

C IF PERIOD NOT CHANGED NO NEED TO RECALCULATE
      IF(INEW.EQ.0)GO TO 2
      DO 5 I=1,6
        YMAX(I)=-10000.
        YMIN(I)=10000.
   5  CONTINUE

C Re-establish julian days.
      CALL EDAY(ID1,IM1,IIDS)
      CALL EDAY(ID2,IM2,IIDF)
      DO 10 I=IIDS,IIDF

C READ VALUES OF THE CLIMATIC PARAMETERS FOR THIS DAY.
      ID=I
      CALL CLMGET(ID)
      JS=1
      JF=24
      IF(I.EQ.IIDS)JS=IT1
      if(JS.eq.0)JS=1
      IF(I.EQ.IIDF)JF=IT2
      DO 20 J=JS,JF

      DO 30 K=1,6
      IP=K

C   GET RELEVANT VALUES
      if(IP.eq.1)then
        Y=CDIF(J)
      elseif(IP.eq.2)then
        Y=CTMP(J)
      elseif(IP.eq.3)then
        Y=CDNR(J)
      elseif(IP.eq.4)then
        Y=CVEL(J)
      elseif(IP.eq.5)then
        Y=CDIR(J)
      elseif(IP.eq.6)then
        Y=CHUM(J)
      endif

C SAVE MAXIMUM AND MINIMUM VALUES.
      YMAX(IP)=AMAX1(YMAX(IP),Y)
      YMIN(IP)=AMIN1(YMIN(IP),Y)
  30  CONTINUE
   20 CONTINUE
   10 CONTINUE

C   MAX & MIN TIMES
      DT=(IIDF-IIDS)*24+IT2-IT1
      TMIN=IT1-0.5
      TMAX=IT1+DT+0.5

C   SCALE FOR EACH Y AXIS
C   IN FACT 1 & 3 SHARE SAME AXIS
      YMAX(3)=AMAX1(YMAX(1),YMAX(3))
      YMIN(3)=AMIN1(YMIN(1),YMIN(3))
      YMAX(1)=YMAX(3)
      YMIN(1)=YMIN(3)
      DO 70 I=1,6
        A=YMAX(I)-YMIN(I)
        IF(A.GT.0.1)GO TO 75
        YMAX(I)=YMAX(I)+1.0
        YMIN(I)=YMIN(I)-1.0
        A=2.0
  75    igwid=igw
        igheight=igwh
        call axiscale(igwid,igheight,TMIN,TMAX,YMIN(I),YMAX(I),XSC,
     &              bysc,sca,Xadd,addY)
        YSC(I)=bysc
        Yadd(I)=addY
  70  CONTINUE
      INEW=0
   2  CONTINUE
      RETURN

      END
