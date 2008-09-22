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

      SUBROUTINE TMPLOT(ITRC,IFOC,IER)
#include "tdf2.h"

C Parameters for eline calls.
      integer LNMVAB, LNDRAB
      parameter (LNMVAB=1, LNDRAB=0)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/TDFFT/LTDF,LTDFA
      COMMON/TDFFH/NWPR,NITDF,NTSPH,NEXTCL,NEXTRC,LASTHD,
     &             LSTREC,NDBSTP,NUWPR
      COMMON/TDFIH1/TAG(MIT),TTYPE(MIT),TMENU(MIT),TAIDE(MIT)
      COMMON/TDFIH2/NTSTAT(MIT),NTBITS(MIT),
     &              NTMAR(MIT),NTMAT(MIT),NTMAI(MIT)
      COMMON/TDFLD1/TDFDES(MIT,MBITS)
      COMMON/TDFLD2/ITDFTR(MIT,MBITS),IATDF(MIT,MBITS),ITCOL(MIT,MBITS)
      COMMON/TDFTEX/TMAT(MIT,MGAT)
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/tdset/itdyear,itdbdoy,itdedoy
      COMMON/GDPER/igbdoy,igedoy

      CHARACTER TAG*12,TTYPE*8,TMENU*16,TAIDE*36
      CHARACTER TDFDES*32,TMAT*16,LABEL1*16,LABEL2*72
      CHARACTER temp*32,temp2*14,FOCUS*8
      CHARACTER*72 LTDF,LTDFA,ETEXT
      CHARACTER PERST1*14,PERST2*44,PERST3*44      
      DIMENSION IFSYM(MBITS)
      logical colok
#ifdef OSX
      integer igwid,igheight  ! for use with axiscale
#else
      integer*8 igwid,igheight  ! for use with axiscale
#endif

C Start drawing; first header
      IF(MMOD.EQ.8)THEN
        CALL startbuffer()
        CALL USRMSG(' ',' ','-')
      ELSE
        RETURN
      ENDIF

C Check if can draw in colour.
      colok=.false.
      if(nzonec.ge.6)colok=.true.

C Title strings.
      WRITE(ETEXT,'(A,A)')  ' TDF file: ',LTDF(1:50)
      CALL viewtext(ETEXT,1,0,1)
      ETEXT=' '
      IVBTIM=1
      IVETIM=NTSPH*24
      CALL EPERSTR(itdyear,itdbdoy,IVBTIM,itdedoy,IVETIM,
     &           NTSPH,IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
      WRITE(ETEXT,'(A,I2,A,A38)') ' Timesteps per hour: ', NTSPH,
     &  ' overall ',PERST3(1:38)
      CALL viewtext(ETEXT,2,0,1)

C Draw a sets of labels and line types at top of the graph.
C Must generalise this for various numbers of period fields.
      IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
      ISFLDS=IEFLDS-NTBITS(IFOC)+1
      M=0
      DO 60 J=ISFLDS,IEFLDS
        M=M+1
        WRITE(temp,'(A)')TDFDES(IFOC,J)
        temp2=temp(1:14)

C If plotting colours for different lines set this.
        if(colok)call winscl('z',M)
        IF(M.EQ.1)THEN
          CALL wstxpt(50,igt-15,temp2)
          call eline (20,igt-20, LNMVAB)
          call eline (45,igt-20, LNDRAB)
          call esymbol(30,igt-20,0,0)
          IFSYM(1)=0
        ELSEIF(M.EQ.2)THEN
          CALL wstxpt(50,igt-35,temp2)
          call eline (20,igt-40, LNMVAB)
          call eline (45,igt-40, LNDRAB)
          call esymbol(30,igt-40,1,0)
          IFSYM(2)=1
        ELSEIF(M.EQ.3)THEN
          CALL wstxpt(170,igt-15,temp2)
          call eline (140,igt-20, LNMVAB)
          call eline (165,igt-20, LNDRAB)
          call esymbol(150,igt-20,2,0)
          IFSYM(3)=2
        ELSEIF(M.EQ.4)THEN
          CALL wstxpt(170,igt-35,temp2)
          call eline (140,igt-40, LNMVAB)
          call eline (165,igt-40, LNDRAB)
          call esymbol(150,igt-40,3,0)
          IFSYM(4)=3
        ELSEIF(M.EQ.5)THEN
          CALL wstxpt(290,igt-15,temp2)
          call eline (260,igt-20, LNMVAB)
          call eline (285,igt-20, LNDRAB)
          call esymbol(270,igt-20,4,0)
          IFSYM(5)=4
        ELSEIF(M.EQ.6)THEN
          CALL wstxpt(290,igt-35,temp2)
          call eline (260,igt-40, LNMVAB)
          call eline (285,igt-40, LNDRAB)
          call esymbol(270,igt-40,5,0)
          IFSYM(6)=5
        ELSEIF(M.EQ.7)THEN
          CALL wstxpt(410,igt-15,temp2)
          call eline (380,igt-20, LNMVAB)
          call eline (405,igt-20, LNDRAB)
          call esymbol(390,igt-20,6,0)
          IFSYM(7)=6
        ELSEIF(M.EQ.8)THEN
          CALL wstxpt(410,igt-35,temp2)
          call eline (380,igt-40, LNMVAB)
          call eline (405,igt-40, LNDRAB)
          call esymbol(390,igt-40,7,0)
          IFSYM(8)=7
        ENDIF

C Reset colour to black.
        if(colok)call winscl('-',0)
        call forceflush()
   60 CONTINUE

C Scale and draw axes.
C To scale the vertical axis it will be necessary to scan through
C the whole period of the database if the instance is TABULAR.
      call usrmsg(' ','Checking bounds... ','-')
      if(igbdoy.eq.igedoy)then
        ixmin = (igbdoy-itdbdoy)*NTSPH*24
        ixmax = ixmin + (NTSPH*24)
      else
        ixmin=(igbdoy-itdbdoy)*NTSPH*24
        ixmax= ixmin + ((igedoy-igbdoy)*NTSPH*24)
      endif
      iperiod=ixmax-ixmin
      XMAX=FLOAT(ixmax)
      XMIN=FLOAT(ixmin)
C Debug.
C      write(6,*) 'steps =', iperiod,ixmin,ixmax,itdbdoy,igbdoy,igedoy,
C     &  XMIN,XMAX

      YMIN=+1.E+7
      YMAX=-1.E+7
      IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
      ISFLDS=IEFLDS-NTBITS(IFOC)+1
      DO 6 J=ISFLDS,IEFLDS

C Scan TABUMX and TABUMN for current data columns to find out extents
C of the vertical axis. 
        if(TABUMX(ITCOL(IFOC,J)).gt.YMAX) YMAX=TABUMX(ITCOL(IFOC,J))
        if(TABUMN(ITCOL(IFOC,J)).lt.YMIN) YMIN=TABUMN(ITCOL(IFOC,J))
C Debug
C      write(6,*)'ifoc j isflds ieflds max min ',ifoc,j,isflds,ieflds,
C     &  ymax,ymin
   6  CONTINUE


C Scale max and min values outwards.
      if (YMAX.lt.0.0) then
        YMAX=YMAX*0.9
      else
        YMAX=YMAX*1.1
      endif
      if (YMIN.lt.0.0) then
        YMIN=YMIN*1.1
      else
        YMIN=YMIN*0.9
      endif

C Round YMIN to a sensible value.
      if (YMIN.lt.0.0) then
        if (YMIN.gt.-1.0) then
          YMIN=float(int(YMIN*10.)-1)/10.
        elseif (YMIN.gt.-10.0) then
          YMIN=float(INT(YMIN)-1)
        elseif (YMIN.gt.-100.0) then
          YMIN=10.*float((INT(YMIN)/10)-1)
        elseif (YMIN.gt.-1000.0) then
          YMIN=100.*float((INT(YMIN)/100)-1)
        else
          YMIN=1000.*float((INT(YMIN)/1000)-1)
        endif
      else
        if (YMIN.lt.100.0) then
          YMIN=10.*float(INT(YMIN)/10)
        elseif (YMIN.lt.1000.0) then
          YMIN=100.*float(INT(YMIN)/100)
        else
          YMIN=1000.*float(INT(YMIN)/1000)
        endif
      endif
      if (ABS(YMAX-YMIN).lt.0.00001) YMAX=YMAX+0.5
      write(6,*)'ifoc isflds ieflds max min ',ifoc,isflds,ieflds,ymax,
     &  ymin
      call usrmsg(' ',' ','-')

C Set the viewing period.
      IVBTIM=1
      IVETIM=NTSPH*24
      CALL EPERSTR(itdyear,igbdoy,IVBTIM,igedoy,IVETIM,
     &           NTSPH,IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
      WRITE(LABEL2,'(4a)') 'Display ',PERST3,' (hours)'

C Give a bit of room in vertical direction.
      YMAX=YMAX+0.2

C Vertical axis label depending on generic type.
      FOCUS=TTYPE(IFOC)
      IF(FOCUS(1:7).EQ.'RADSENT')THEN
        LABEL1=TMAT(IFOC,1)
      ELSEIF(FOCUS(1:7).EQ.'ALLCLMT')THEN
        LABEL1='various units   '
      ELSEIF(FOCUS(1:7).EQ.'ZIVENTT')THEN
        LABEL1='m^3/sec         '
      ELSEIF(FOCUS(1:7).EQ.'RAIRVLT')THEN
        LABEL1='m  /sec         '
      ELSEIF(FOCUS(1:7).EQ.'SETPTTT')THEN
        LABEL1='temperatue deg C'
      ELSEIF(FOCUS(1:7).EQ.'CTLSTAT')THEN
        LABEL1='status flag     '
      ELSEIF(FOCUS(1:7).EQ.'SKYLUXT')THEN
        LABEL1='lux             '
      ELSEIF(FOCUS(1:7).EQ.'SKYTMPT')THEN
        LABEL1='temperatue deg C'
      ELSEIF(FOCUS(1:7).EQ.'DBTEXTT')THEN
        LABEL1='temperatue deg C'
      ELSEIF(FOCUS(1:7).EQ.'WINDVLT')THEN
        LABEL1='metres per sec  '
      ELSEIF(FOCUS(1:7).EQ.'WINDDRT')THEN
        LABEL1='   degrees      '
      ELSEIF(FOCUS(1:7).EQ.'RELHUMT')THEN
        LABEL1='   percent      '
      ELSEIF(FOCUS(1:7).EQ.'DIFHSLT')THEN
        LABEL1='Watts / sq metre'
      ELSEIF(FOCUS(1:7).EQ.'GLOHSLT')THEN
        LABEL1='Watts / sq metre'
      ELSEIF(FOCUS(1:7).EQ.'DIRSOLT')THEN
        LABEL1='Watts / sq metre'
      ELSEIF(FOCUS(1:7).EQ.'CASUALT')THEN
        LABEL1='Watts           '
      ELSEIF(FOCUS(1:8).EQ.'CASUAL3T')THEN
        LABEL1='Watts           '
      ELSEIF(FOCUS(1:7).EQ.'OBJVELT')THEN
        LABEL1='metres per sec  '
      ELSEIF(FOCUS(1:7).EQ.'OBJDIRT')THEN
        LABEL1='  degrees       '
      ELSEIF(FOCUS(1:7).EQ.'TSCASGT')THEN
        LABEL1='    Watts       '
      ELSEIF(FOCUS(1:7).EQ.'GLOVRTT')THEN
        LABEL1='Watts / sq metre'
      ELSEIF(FOCUS(1:8).EQ.'ACTIVITY')THEN
        LABEL1='fractions & degC'
      ELSE

C Add in additional generic data definitions here.
C    ......
      ENDIF

      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,XMIN,XMAX,YMIN,YMAX,xsc,ysc,sca,
     &  Xadd,Yadd)
      CALL dinterval(XMIN,XMAX,ddy1,ny,1)

C Try axis of hours.
      iind=1
      intsph=ntsph
      iigbdoy=igbdoy
      CALL horaxishdw(XMIN,XMAX,igl,igr,igb,Xadd,xsc,1,iind,intsph,
     &  iigbdoy,LABEL2)
C      CALL horaxis(XMIN,XMAX,igl,igr,igb,Xadd,xsc,1,LABEL2)
      CALL vrtaxis(YMIN,YMAX,igl,igb,igt,Yadd,ysc,0,0,LABEL1)

      IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
      ISFLDS=IEFLDS-NTBITS(IFOC)+1
      M=0
      DO 66 J=ISFLDS,IEFLDS
        M=M+1
C If plotting colours for different lines set this.
        if(colok)call winscl('z',M)

C Scan TAB columns. Depending on which field use different symbol.
        IF(ITDFTR(IFOC,J).NE.2)THEN
          ITREC=ixmin+49
          CALL ERTABU(ITRC,ITREC,IER)
          VAL=TABU(ITCOL(IFOC,J))
          ix=igl+INT((1.+Xadd)*xsc)
          iy=igb-INT((VAL+Yadd)*ysc)
          call eline (ix,iy, LNMVAB)
          lastx=ix
          lasty=iy

          DO 55 IP=ixmin+1,ixmax
            ITREC=IP+49
            CALL ERTABU(ITRC,ITREC,IER)
            VAL=TABU(ITCOL(IFOC,J))
            ix=igl+INT((REAL(IP)+Xadd)*xsc)
            iy=igb-INT((VAL+Yadd)*ysc)
            call eline (lastx,lasty, LNMVAB)
            call eline (ix,iy, LNDRAB)
            lastx=ix
            lasty=iy
            if(iperiod.gt.200.and.iperiod.le.500)then
              IR=INT(AMOD(REAL(IP),REAL(NTSPH*4)))
              IF(IR.EQ.0)CALL esymbol(ix,iy,IFSYM(J-ISFLDS+1),0)
            elseif(iperiod.gt.500.and.iperiod.le.1000)then
              IR=INT(AMOD(REAL(IP),REAL(NTSPH*12)))
              IF(IR.EQ.0)CALL esymbol(ix,iy,IFSYM(J-ISFLDS+1),0)
            elseif(iperiod.gt.1000)then
              IR=INT(AMOD(REAL(IP),REAL(NTSPH*24)))
              IF(IR.EQ.0)CALL esymbol(ix,iy,IFSYM(J-ISFLDS+1),0)
            else
              IR=INT(AMOD(REAL(IP),REAL(NTSPH)))
              IF(IR.EQ.0)CALL esymbol(ix,iy,IFSYM(J-ISFLDS+1),0)
            endif
   55     CONTINUE
        ENDIF
        if(colok)call winscl('-',0)
        call forceflush()
   66 CONTINUE

      RETURN

      END
