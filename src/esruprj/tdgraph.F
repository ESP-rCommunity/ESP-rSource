C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

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

      SUBROUTINE TMPLOT(ITRC,IFOC,IER)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "tdf2.h"

C Parameters for eline calls.
      integer LNMVAB, LNDRAB
      parameter (LNMVAB=1, LNDRAB=0)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/GDPER/igbdoy,igedoy

      CHARACTER LABEL1*16,LABEL2*72
      CHARACTER temp*32,temp2*14,temp3*9,FOCUS*8
      CHARACTER*72 ETEXT
      CHARACTER PERST1*14,PERST2*44,PERST3*44      
      DIMENSION IFSYM(MBITS)
      logical colok
      logical use14
      integer icontinue,icontinue2  ! to control while loops
      integer iloop,iloop2          ! for loops

#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer iid1,iid2,iilastx,iilasty,iix,iiy
      integer iicol
      integer iigl,iigr,iigt,iigb
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iid1,iid2,iilastx,iilasty,iix,iiy
      integer*8 iicol
      integer*8 iigl,iigr,iigt,iigb
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

C Cast values for horaxis.
      iigl=igl; iigr=igr; iigt=igt; iigb=igb

C Title strings.
      WRITE(etext,'(A,A)')  ' TDF file: ',LTDF(1:50)
      iside=1
      isize=0
      ifont=1
      call viewtext(etext,iside,isize,ifont)
      ETEXT=' '
      IVBTIM=1
      IVETIM=NTSPH*24
      CALL EPERSTR(itdyear,itdbdoy,IVBTIM,itdedoy,IVETIM,
     &           NTSPH,IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
      WRITE(etext,'(A,I2,A,A38)') ' Timesteps per hour: ', NTSPH,
     &  ' overall ',PERST3(1:38)
      iside=2
      call viewtext(etext,iside,isize,ifont)

C Draw a sets of labels and line types at top of the graph.
C Must generalise this for various numbers of period fields.
      IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
      ISFLDS=IEFLDS-NTBITS(IFOC)+1
      if(ieflds-isflds.lt.8)then
        use14=.true.
      else
        use14=.false.
      endif

C User a while loop to traverse the item columns.
      icontinue=1
      M=0
      iloop=ISFLDS-1
      do while (icontinue.ne.0)
        iloop=iloop+1  ! increment iloop until = IEFLDS then jump out
        if(iloop.eq.IEFLDS) icontinue=0
        M=M+1
        WRITE(temp,'(A)')TDFDES(IFOC,iloop)
        temp2=temp(1:14)
        temp3=temp(1:9)

C If plotting colours for different lines set this.
        if(colok)then
          iicol=M
          call winscl('z',iicol)
        endif
        IF(M.EQ.1)THEN
          iid1=50; iid2=igt-15;
          if(use14)then
            CALL wstxpt(iid1,iid2,temp2)
          else
            CALL wstxpt(iid1,iid2,temp3)
          endif
          iid1=20; iid2=igt-20;
          call eline (iid1,iid2, LNMVAB)
          iid1=45; iid2=igt-20;
          call eline (iid1,iid2, LNDRAB)
          iid1=30; iid2=igt-20;
          call esymbol(iid1,iid2,0,0)
          IFSYM(1)=0
        ELSEIF(M.EQ.2)THEN
          iid1=50; iid2=igt-35;
          if(use14)then
            CALL wstxpt(iid1,iid2,temp2)
          else
            CALL wstxpt(iid1,iid2,temp3)
          endif
          iid1=20; iid2=igt-40;
          call eline (iid1,iid2, LNMVAB)
          iid1=45; iid2=igt-40;
          call eline (iid1,iid2, LNDRAB)
          iid1=30; iid2=igt-40;
          call esymbol(iid1,iid2,1,0)
          IFSYM(2)=1
        ELSEIF(M.EQ.3)THEN
          iid1=170; iid2=igt-15;
          if(use14)then
            CALL wstxpt(iid1,iid2,temp2)
          else
            CALL wstxpt(iid1,iid2,temp3)
          endif
          iid1=140; iid2=igt-20;
          call eline (iid1,iid2, LNMVAB)
          iid1=165; iid2=igt-20;
          call eline (iid1,iid2, LNDRAB)
          iid1=150; iid2=igt-20;
          call esymbol(iid1,iid2,2,0)
          IFSYM(3)=2
        ELSEIF(M.EQ.4)THEN
          iid1=170; iid2=igt-35;
          if(use14)then
            CALL wstxpt(iid1,iid2,temp2)
          else
            CALL wstxpt(iid1,iid2,temp3)
          endif
          iid1=140; iid2=igt-40;
          call eline (iid1,iid2, LNMVAB)
          iid1=165; iid2=igt-40;
          call eline (iid1,iid2, LNDRAB)
          iid1=150; iid2=igt-40;
          call esymbol(iid1,iid2,3,0)
          IFSYM(4)=3
        ELSEIF(M.EQ.5)THEN
          iid1=290; iid2=igt-15;
          if(use14)then
            CALL wstxpt(iid1,iid2,temp2)
          else
            CALL wstxpt(iid1,iid2,temp3)
          endif
          iid1=260; iid2=igt-20;
          call eline (iid1,iid2, LNMVAB)
          iid1=285; iid2=igt-20;
          call eline (iid1,iid2, LNDRAB)
          iid1=270; iid2=igt-20;
          call esymbol(iid1,iid2,4,0)
          IFSYM(5)=4
        ELSEIF(M.EQ.6)THEN
          iid1=290; iid2=igt-35;
          if(use14)then
            CALL wstxpt(iid1,iid2,temp2)
          else
            CALL wstxpt(iid1,iid2,temp3)
          endif
          iid1=260; iid2=igt-40;
          call eline (iid1,iid2, LNMVAB)
          iid1=285; iid2=igt-40;
          call eline (iid1,iid2, LNDRAB)
          iid1=270; iid2=igt-40;
          call esymbol(iid1,iid2,5,0)
          IFSYM(6)=5
        ELSEIF(M.EQ.7)THEN
          iid1=410; iid2=igt-15;
          if(use14)then
            CALL wstxpt(iid1,iid2,temp2)
          else
            CALL wstxpt(iid1,iid2,temp3)
          endif
          iid1=380; iid2=igt-20;
          call eline (iid1,iid2, LNMVAB)
          iid1=405; iid2=igt-20;
          call eline (iid1,iid2, LNDRAB)
          iid1=390; iid2=igt-20;
          call esymbol(iid1,iid2,6,0)
          IFSYM(7)=6
        ELSEIF(M.EQ.8)THEN
          iid1=410; iid2=igt-35;
          if(use14)then
            CALL wstxpt(iid1,iid2,temp2)
          else
            CALL wstxpt(iid1,iid2,temp3)
          endif
          iid1=380; iid2=igt-40;
          call eline (iid1,iid2, LNMVAB)
          iid1=405; iid2=igt-40;
          call eline (iid1,iid2, LNDRAB)
          iid1=390; iid2=igt-40;
          call esymbol(iid1,iid2,7,0)
          IFSYM(8)=7
        ELSEIF(M.EQ.9)THEN
          iid1=530; iid2=igt-15;
          if(use14)then
            CALL wstxpt(iid1,iid2,temp2)
          else
            CALL wstxpt(iid1,iid2,temp3)
          endif
          iid1=500; iid2=igt-20;
          call eline (iid1,iid2, LNMVAB)
          iid1=525; iid2=igt-20;
          call eline (iid1,iid2, LNDRAB)
          iid1=510; iid2=igt-20;
          call esymbol(iid1,iid2,8,0)
          IFSYM(9)=8
        ENDIF

C Reset colour to black.
        if(colok)then
          iicol=0
          call winscl('-',iicol)
        endif
        call forceflush()
      enddo  ! of while loop

C Scale and draw axes.
C To scale the vertical axis it will be necessary to scan through
C the whole period of the database if the instance is TABULAR.
      call usrmsg(' ','Checking bounds... ','-')
      if(igbdoy.eq.igedoy)then
        ixmin = (igbdoy-itdbdoy)*NTSPH*24
        ixmax = ixmin + (NTSPH*24)
      else
        ixmin=(igbdoy-itdbdoy)*NTSPH*24
        ixmax= ixmin + (((igedoy-igbdoy)+1)*NTSPH*24)
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

C Debug.
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

C Debug.
C      write(6,*)'ifoc isflds ieflds max min ',ifoc,isflds,ieflds,ymax,
C     &  ymin

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
        LABEL1='m/sec           '
      ELSEIF(FOCUS(1:7).EQ.'SETPTTT')THEN
        LABEL1='temperatue deg C'
      ELSEIF(FOCUS(1:8).EQ.'HTCLSETP')THEN
        LABEL1='temperatue deg C'
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
      ELSEIF(FOCUS(1:7).EQ.'OBJDIRT')THEN
        LABEL1='  degrees       '
      ELSEIF(FOCUS(1:7).EQ.'TSCASGT')THEN
        LABEL1='    Watts       '
      ELSEIF(FOCUS(1:7).EQ.'VERTSOL')THEN
        LABEL1='Watts / sq metre'
      ELSEIF(FOCUS(1:8).EQ.'ACTIVITY')THEN
        LABEL1='fractions & degC'
      ELSEIF(FOCUS(1:8).EQ.'BIDIRSET')THEN
        LABEL1='set numb. (1-40)'
      ELSEIF(FOCUS(1:8).EQ.'BAMSNSET')THEN
        LABEL1='various units   '
      ELSE

C Add in additional generic data definitions here.
C    ......
      ENDIF

      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,XMIN,XMAX,YMIN,YMAX,xsc,ysc,sca,
     &   Xadd,Yadd)
      CALL dinterval(XMIN,XMAX,ddy1,ny,1)

C Try axis of hours.
      iind=1
      intsph=ntsph
      iigbdoy=igbdoy
      CALL horaxishdw(XMIN,XMAX,iigl,iigr,iigb,Xadd,xsc,1,iind,intsph,
     &  iigbdoy,LABEL2)
C      CALL horaxis(XMIN,XMAX,iigl,iigr,iigb,Xadd,xsc,1,LABEL2)
      CALL vrtaxis(YMIN,YMAX,iigl,iigb,iigt,Yadd,ysc,0,0,LABEL1)

      IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
      ISFLDS=IEFLDS-NTBITS(IFOC)+1
      icontinue=1
      M=0
      iloop=ISFLDS-1
      do while (icontinue.ne.0)
        iloop=iloop+1  ! increment iloop until = IEFLDS then jump out
        if(iloop.eq.IEFLDS) icontinue=0
        M=M+1
C If plotting colours for different lines set this.
        if(colok)then
          iicol=M
          call winscl('z',iicol)
        endif

C Scan TAB columns. Depending on which field use different symbol.
        IF(ITDFTR(IFOC,iloop).NE.2)THEN
          ITREC=ixmin+49
          CALL ERTABU(ITRC,ITREC,IER)
          VAL=TABU(ITCOL(IFOC,iloop))
          iix=igl+INT((1.+Xadd)*xsc)
          iiy=igb-INT((VAL+Yadd)*ysc)
          call eline (iix,iiy, LNMVAB)
          iilastx=iix
          iilasty=iiy

C Use while loop to traverse the records in temporal.
          icontinue2=1
          iloop2=ixmin
          do while (icontinue2.ne.0)
            iloop2=iloop2+1  ! increment iloop2 until = ixmax then jump out
            if(iloop2.eq.ixmax) icontinue2=0
            ITREC=iloop2+49
            CALL ERTABU(ITRC,ITREC,IER)
            VAL=TABU(ITCOL(IFOC,iloop))
            iix=igl+INT((REAL(iloop2)+Xadd)*xsc)
            iiy=igb-INT((VAL+Yadd)*ysc)
            call eline (iilastx,iilasty, LNMVAB)
            call eline (iix,iiy, LNDRAB)
            iilastx=iix
            iilasty=iiy
            if(iperiod.gt.200.and.iperiod.le.500)then
              IR=INT(AMOD(REAL(iloop2),REAL(NTSPH*4)))
              IF(IR.EQ.0)CALL esymbol(iix,iiy,IFSYM(iloop-ISFLDS+1),0)
            elseif(iperiod.gt.500.and.iperiod.le.1000)then
              IR=INT(AMOD(REAL(iloop2),REAL(NTSPH*12)))
              IF(IR.EQ.0)CALL esymbol(iix,iiy,IFSYM(iloop-ISFLDS+1),0)
            elseif(iperiod.gt.1000)then
              IR=INT(AMOD(REAL(iloop2),REAL(NTSPH*24)))
              IF(IR.EQ.0)CALL esymbol(iix,iiy,IFSYM(iloop-ISFLDS+1),0)
            else
              IR=INT(AMOD(REAL(iloop2),REAL(NTSPH)))
              IF(IR.EQ.0)CALL esymbol(iix,iiy,IFSYM(iloop-ISFLDS+1),0)
            endif
          enddo  ! of while loop2
        ENDIF
        if(colok)then
          iicol=0
          call winscl('-',iicol)
        endif
        call forceflush()
      enddo  ! of while loop

      RETURN

      END
