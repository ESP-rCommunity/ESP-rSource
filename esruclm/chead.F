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

C *********** CHEAD
C Print climate heading for text based routines. Lines is the number
C of lines to include in the text portion of the display.
      SUBROUTINE CHEAD(LINES)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/appw/iappw,iappx,iappy
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMDT0/CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GFONT/IFS,ITFS,IMFS

      CHARACTER*72 ETEXT,GTEXT
      CHARACTER*30 CLMLOC
      character PERST1*14,PERST2*44,PERST3*44

C If in type 8 terminal make the text display area larger before
C displaying the following data ( but only clear it if LINES != LIMTTY)
      if(LIMTTY.NE.LINES.AND.MMOD.EQ.8)then
#ifdef X11
        call userfonts(IFS,ITFS,IMFS)
        if(iappw.gt.0.and.iappw.lt.100)then
          menuchw = MAX0(int(27*iappw*0.01),20)
          LIMTTY=LINES
        else
          menuchw = 27
          LIMTTY=LINES
        endif
        call winclr
        CALL feedbox(menuchw,2,igfw,igfh)
        CALL opengdisp(menuchw,LIMTTY,2,igdw,igdh)
        CALL win3d(menuchw,10,22,5,3,igl,igr,igt,igb,igw,igwh)
#else
        continue
#endif
      endif
      ACLAT=ABS(CLAT)
      ACLONG=ABS(CLONG)
      IF(CLONG.LT.0.0.AND.CLAT.GE.0.0)then
        WRITE(ETEXT,102)CLMLOC,ACLAT,ACLONG,IYEAR
 102    FORMAT(' Climate analysis:',1X,A30,':',F6.2,'N',F6.2,'W :',I5)
      elseif(CLONG.GE.0.0.AND.CLAT.LT.0.0)then
        WRITE(ETEXT,103)CLMLOC,ACLAT,ACLONG,IYEAR
 103    FORMAT(' Climate analysis:',1X,A30,':',F6.2,'S',F6.2,'E :',I5)
      elseif(CLONG.LT.0.0.AND.CLAT.LT.0.0)then
        WRITE(ETEXT,104)CLMLOC,ACLAT,ACLONG,IYEAR
 104    FORMAT(' Climate analysis:',1X,A30,':',F6.2,'S',F6.2,'W :',I5)
      else
        WRITE(ETEXT,101)CLMLOC,ACLAT,ACLONG,IYEAR
 101    FORMAT(' Climate analysis:',1X,A30,':',F6.2,'N',F6.2,'E :',I5)
      endif
      GTEXT=ETEXT
      if(MMOD.gt.0)then
        CALL viewtext(GTEXT,1,0,1)
      else
        call edisp(iuout,GTEXT)
      endif

      NTSPH=1
      CALL EPERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
      write(ETEXT,'(1X,A44)')PERST2
      GTEXT=ETEXT
      if(MMOD.gt.0)then
        CALL viewtext(GTEXT,2,0,1)
      else
        call edisp(iuout,GTEXT)
      endif

      RETURN
      END

C Display heading for climate graph.
      SUBROUTINE GHEAD(imenuch)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/appw/iappw,iappx,iappy
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMDT0/CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GFONT/IFS,ITFS,IMFS

      CHARACTER*72 ETEXT,GTEXT
      CHARACTER*30 CLMLOC
      character PERST1*14,PERST2*44,PERST3*44

      IF(MMOD.EQ.8)THEN
#ifdef X11
        if(iappw.gt.0.and.iappw.lt.100)then
          menuchw = MAX0(int(imenuch*iappw*0.01),18)
          LIMTTY= MAX0(int(8*iappw*0.01),4)
          LIMIT = MAX0(int(8*iappw*0.01),4)
        else
          menuchw = imenuch
          LIMTTY=8
          LIMIT =8
        endif
        call winclr
        CALL feedbox(menuchw,2,igfw,igfh)
        CALL opengdisp(menuchw,LIMTTY,2,igdw,igdh)
        CALL win3d(menuchw,10,22,7,3,igl,igr,igt,igb,igw,igwh)
#else
        continue
#endif
      ENDIF
      ACLAT=ABS(CLAT)
      ACLONG=ABS(CLONG)
      IF(CLONG.LT.0.0.AND.CLAT.GE.0.0)then
        WRITE(ETEXT,102)CLMLOC,ACLAT,ACLONG,IYEAR
 102    FORMAT(' Climate analysis:',1X,A30,':',F6.2,'N',F6.2,'W :',I5)
      elseif(CLONG.GE.0.0.AND.CLAT.LT.0.0)then
        WRITE(ETEXT,103)CLMLOC,ACLAT,ACLONG,IYEAR
 103    FORMAT(' Climate analysis:',1X,A30,':',F6.2,'S',F6.2,'E :',I5)
      elseif(CLONG.LT.0.0.AND.CLAT.LT.0.0)then
        WRITE(ETEXT,104)CLMLOC,ACLAT,ACLONG,IYEAR
 104    FORMAT(' Climate analysis:',1X,A30,':',F6.2,'S',F6.2,'W :',I5)
      else
        WRITE(ETEXT,101)CLMLOC,ACLAT,ACLONG,IYEAR
 101    FORMAT(' Climate analysis:',1X,A30,':',F6.2,'N',F6.2,'E :',I5)
      endif
      GTEXT=ETEXT
      if(MMOD.gt.0)then
        CALL viewtext(GTEXT,1,0,1)
      else
        call edisp(iuout,GTEXT)
      endif

      NTSPH=1
      CALL EPERSTR(IYEAR,IDS,IT1,IDF,IT2,NTSPH,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
      write(ETEXT,'(1X,A44)')PERST2
      GTEXT=ETEXT
      if(MMOD.gt.0)then
        CALL viewtext(GTEXT,2,0,1)
      else
        call edisp(iuout,GTEXT)
      endif

      RETURN
      END
