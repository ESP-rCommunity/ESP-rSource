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

C *********** PROGRF
C 'PROGRF' outputs a graphical display of an event profile (IPRO).
      SUBROUTINE PROGRF(IPRO)

      PARAMETER  (MC=35)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PDBITM/NITEMS,lastrec,NCG(100),NO(100),DESC(100)
      COMMON/PDBELM/ICGS1(MC),ICGF1(MC),CGS1(MC),CGL1(MC)
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GFONT/IFS,ITFS,IMFS

      CHARACTER DESC*40,ETEXT*72

      DIMENSION VAL1(25),VAL2(25)

      IF(IPRO.LT.1.OR.IPRO.GT.NITEMS)then
        call usrmsg(' ',' Profile requested not within db','W')
        return
      endif

      YMAX=100.
      YMIN=0.

C Set up data ready for graph drawing.
      call startbuffer
      call axiscale(igw,igwh,0.0,24.0,YMIN,YMAX,xsc,ysc,sca,Xadd,Yadd)
      call linescale(igl,Xadd,xsc,igb,Yadd,ysc)
      call horaxis(0.,24.,igl,igr,igb,Xadd,xsc,1,'Time (hours)')
      call vrtaxis(YMIN,YMAX,igl,igb,igt,Yadd,ysc,0,0,'Percent')

C Pick up the data increment for the horizontal axis.
      call dinterval(0.,24.,DT,NDEC,1)

C Print header
      WRITE(ETEXT,16)IPRO,DESC(IPRO)
   16 FORMAT('Profile ',I3,': ',A40)
      call viewtext(ETEXT,1,0,IFS)

      DO 60 I=1,25
        VAL1(I)=0.
        VAL2(I)=0.
   60 CONTINUE

c Assign hourly values of each event.
      DO 20 I=1,NCG(IPRO)

c Period of each event.
        DO 30 J=ICGS1(I),ICGF1(I)
          J1=J+1
          VAL1(J1)=CGS1(I)
          VAL2(J1)=CGL1(I)
   30   CONTINUE
   20 CONTINUE

c Plot VAL1 array
      IPEN=1
      DO 40 I=0,24
        MARK=0
        TI=FLOAT(I)
        IR=AMOD(TI,DT)+.001
        IF(IR.EQ.0)MARK=4
        IF(I.GT.0)goto 9
        CALL ETPLOT(TI,VAL1(I+1),0,0)
        goto 40
    9   CALL ETPLOT(TI,VAL1(I+1),IPEN,MARK)
   40 CONTINUE
      call forceflush()

c Plot VAL2 array
      IPEN=-6
      DO 50 I=0,24
        MARK=0
        TI=FLOAT(I)
        IR=AMOD(TI,DT)+.001
        IF(IR.EQ.0)MARK=9
        IF(I.GT.0)goto 11
        CALL ETPLOT(TI,VAL2(I+1),0,0)
        goto 50
   11   CALL ETPLOT(TI,VAL2(I+1),IPEN,MARK)
   50 CONTINUE
      call forceflush()
      return

      END
