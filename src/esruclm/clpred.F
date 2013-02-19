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

C clpred.F includes the following subroutines:
C   CLPRED allows the user to predict/transform climatic data.
C   POLYTR transforms weather data for a given period according to some
C          polynomial function.
C   PDRAW will draw the predicted climatic data for any period.
C   PSOLRD uses method J.K. Page (Sheffield), to predict the direct
C          normal and the horizontal diffuse solar irradiance.
C   SINUSD is used to predict values throughout a day assuming a
C          sinusoidal variation.
C   POLYNOM is used to transform the number x to y following the 
C          formula y=a_n*x^n + a_(n-1)*x^(n-1) + .... + a_0


C *********** CLPRED
C  Subroutine CLPRED allows the user to predict/transform climatic data.
C  'CLPRED' calls other subroutines each of which will predict particular
C  climatic variables.
C IST = 1 is not currently used.
C IST = 2 the user is interested in transforming the data
C IST = 3 the user is interested in curve fitting.
      SUBROUTINE CLPRED(IST)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      PARAMETER (MT=24,MD=365,MAAS=9)
      COMMON/OUTIN/IUOUT,IUIN

C PRADB is direct normal radiation, PRADD is diffuse radiation
C PTEMP is ambient dry bulb, PWV is wind velocity
C PRH is relative hunidity, PLAST is previous value of current focus.
      COMMON/PCLIM/PRADB(MD,MT),PRADD(MD,MT),PTEMP(MD,MT),
     &             PWV(MD,MT),PWD(MD,MT),PRH(MD,MT),PLAST(MD,MT)
      COMMON/FILEP/IFIL
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     A              CDIR(MT),CHUM(MT)
      COMMON/RADTYP/IDNGH
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/PERPER/PERST2

      CHARACTER*27 ITEM(14)
      character PRML*48,PRM*24,CLMLOC*30,PERST2*44,str*60
      character outs*124
      DIMENSION ICLM(24,6),ICK(6),WVEL(24),NANG(MAAS),WRDF(MAAS)
      logical OK,dok,FOUND
      integer NITEMS,INO  ! max items and current menu item

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CLPRED'  ! set for this subroutine
   1  ISAVE=1
      if(ist.eq.1)then
        CALL EDISP(IUOUT,'Select period')
      elseif(ist.eq.2)then
        CALL EDISP(IUOUT,'Select period to be transformed')
      elseif(ist.eq.3)then
        CALL EDISP(IUOUT,'Select period that is to be predicted')
      endif
      CALL SELPER(IER)
      IF(IER.NE.0)GOTO 999
      IYDS=IDS
      IYDF=IDF
      ID=0
      DO 55 I=IYDS,IYDF
        ID=ID+1
        II=I
        IREC=II
        READ(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=999)((ICLM(J,K),K=1,6),
     &         J=1,24)
        DO 656 J=1,24
          PRADD(ID,J)=REAL(ICLM(J,1))
          PTEMP(ID,J)=REAL(ICLM(J,2))/10.
          PRADB(ID,J)=REAL(ICLM(J,3))
          PWV(ID,J)=REAL(ICLM(J,4))/10.
          PWD(ID,J)=REAL(ICLM(J,5))
          PRH(ID,J)=REAL(ICLM(J,6))
  656   CONTINUE
   55 CONTINUE

      WRITE(STR,'(2A)')'Modified ',CLMLOC(1:21)
      CALL EASKS(STR,'On changing weather data it is usual to change'
     &   ,'synopsis ',30,'synopsis. ','new synopsis',IER,0)
      ICV=0
      DO 10 I=1,4
        ICK(I)=0
   10 CONTINUE
   11 helptopic='curve_fitting_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

      if(ist.eq.1)then
        ITEM(1)='p Change period           '
      elseif(ist.eq.2)then
        ITEM(1)='p Change transform period '
      elseif(ist.eq.3)then
        ITEM(1)='p Change curve-fit period '
      endif
      ITEM(2) ='  ------------------------ '
      ITEM(3) ='1 Direct radiation         '
      ITEM(4) ='2 Diffuse radiation        '
      ITEM(5) ='3 Dry bulb temperature     '
      ITEM(6) ='4 Relative humidity %      '
      ITEM(7) ='5 Wind speed               '
      ITEM(8) ='6 Wind direction           '
      IF(IST.EQ.1)ITEM(9) =' N/A                     '
      IF(IST.EQ.2)ITEM(9)='7 Wind direction & speed  '
      IF(IST.EQ.3)ITEM(9)='7 Direct and diffuse model'
      if(ist.eq.1)then
        ITEM(10)='! Draw data              '
      elseif(ist.eq.2)then
        ITEM(10)='! Draw transformed data  '
      elseif(ist.eq.3)then
        ITEM(10)='! Draw curve-fit data    '
      endif
      ITEM(11)='s Save weather data        '
      ITEM(12)='  ------------------------ '
      ITEM(13)='? Help                     '
      ITEM(14)='- Exit                     '
      NITEMS=14

      INO=-2
      if(ist.eq.1)then
        CALL EMENU(' Weather prediction',ITEM,NITEMS,INO)
      elseif(ist.eq.2)then
        CALL EMENU(' Weather transforms',ITEM,NITEMS,INO)
      elseif(ist.eq.3)then
        CALL EMENU(' Weather curve-fits',ITEM,NITEMS,INO)
      endif
      if(ino.eq.nitems)then

C When exiting, return to the whole year period.
        IF(ISAVE.EQ.1)THEN
          ID1=1
          IM1=1
          IT1=1
          ID2=31
          IM2=12
          IT2=24
          IDS=1
          IDF=365
          RETURN
        ELSE
          helptopic='reminder_on_saving'
          call gethelptext(helpinsub,helptopic,nbhelp)
          dok=.false.
          CALL ASKOK('Changes have not yet been saved to file. Save',
     &      'data (to the current file) before exiting ?',
     &      OK,dok,nbhelp)
          if(.NOT.OK)then
            ID1=1
            IM1=1
            IT1=1
            ID2=31
            IM2=12
            IT2=24
            IDS=1
            IDF=365
            RETURN
          endif

C Update weather binary file for the period the user had selected
C as well as writing out the tail of the file.
          DO 256 IDAY=IYDS,IYDF
            IDI=IDAY-IYDS+1
            DO 257 IT=1,MT 
              CDNR(IT)=PRADB(IDI,IT)
              CDIF(IT)=PRADD(IDI,IT)
              CTMP(IT)=PTEMP(IDI,IT)
              CHUM(IT)=PRH(IDI,IT)
              CVEL(IT)=PWV(IDI,IT)
              CDIR(IT)=PWD(IDI,IT)
 257        CONTINUE
            CALL CLMPUT(IDAY)
 256      CONTINUE
          IREC=366
          IADN=IDNGH
          WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)IYEAR,IADN
          IREC=367
          WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLMLOC
          IREC=368
          WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLAT,CLONG
          ISAVE=1
          call edisp(iuout,'Weather data saved for...')
          call edisp(iuout,PERST2)
          ID1=1
          IM1=1
          IT1=1
          ID2=31
          IM2=12
          IT2=24
          IDS=1
          IDF=365
          return
        ENDIF
      ELSEIF(INO.EQ.NITEMS-1)THEN
        helptopic='curve_fitting_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('weather prediction',nbhelp,'clm_predict',0,0,IER)
      elseif(ino.eq.1)then
        goto 1
      ELSEIF(INO.GE.3.AND.INO.LE.9)THEN

C Save the current values of the item of interest into PLAST.
        DO 356 IDAY=IYDS,IYDF
          IDI=IDAY-IYDS+1
          DO 357 IT=1,MT 
            if(ino.eq.3)then
              PLAST(IDI,IT)=PRADB(IDI,IT)
            elseif(ino.eq.4)then
              PLAST(IDI,IT)=PRADD(IDI,IT)
            elseif(ino.eq.5)then
              PLAST(IDI,IT)=PTEMP(IDI,IT)
            elseif(ino.eq.6)then
              PLAST(IDI,IT)=PRH(IDI,IT)
            elseif(ino.eq.7)then
              PLAST(IDI,IT)=PWV(IDI,IT)
            elseif(ino.eq.9)then
              PLAST(IDI,IT)=PWV(IDI,IT)
            endif
 357      CONTINUE
 356    CONTINUE
        if(ino.eq.3)then
          ICV=1
          ICK(1)=1
          PRM='direct radiation '
          V_MAX=1400.
          V_MIN=0.
        elseif(ino.eq.4)then
          ICV=1
          ICK(2)=1
          PRM='diffuse radiation '
          V_MAX=1400.
          V_MIN=0.
        elseif(ino.eq.5)then
          ICV=2
          ICK(3)=1
          PRM='dry bulb temperature '
          V_MAX=100.
          V_MIN=-100.
        elseif(ino.eq.6)then
          ICV=3
          ICK(4)=1
          PRM='relative humidity % '
          V_MAX=100.
          V_MIN=0.
        elseif(ino.eq.7)then
          ICV=4
          ICK(5)=1
          PRM='wind speed '
          V_MAX=80.
          V_MIN=0.
        elseif(ino.eq.9)then
          ICV=4
          ICK(6)=1
          PRM='wind speed '
          V_MAX=80.
          V_MIN=0.
        endif
        call startbuffer()
        CALL PDRAW(ICV,-1,IYDS,IYDF)

        IF(INO.EQ.6)THEN
          helptopic='interdependant_rh_temp'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('RH prediction',nbhelp,'RH changing',0,0,IER)
        ELSEIF(INO.EQ.8)THEN

C To activate wind direction prediction change ino.eq.88 to ino.eq.8 in
C the following code blocks 
C<< Normal distribution of wind direction can be supported in future
          write(str,'(2a)')'wind direction prediction is currently',
     &    'not supported'

          call edisp(iuout,' ')
          call edisp(iuout,str)
          call edisp(iuout,' ')
          GOTO 11
        ENDIF
        ISAVE=0
        if(IST.EQ.2)then
          IF(INO.EQ.3)THEN
            CALL POLYTR(1,IYDS,IYDF)
          ELSEIF(INO.EQ.4)THEN
            CALL POLYTR(2,IYDS,IYDF)
          ELSEIF(INO.EQ.5)THEN
            CALL POLYTR(3,IYDS,IYDF)
          ELSEIF(INO.EQ.6)THEN
            CALL POLYTR(4,IYDS,IYDF)
          ELSEIF(INO.EQ.7)THEN
            CALL POLYTR(5,IYDS,IYDF)
          ELSEIF(INO.EQ.88)THEN
            CALL POLYTR(6,IYDS,IYDF)
          ENDIF
        ENDIF

C Prediction of solar radiation as function of turbidity and
C precipitable water content
        IF(IST.EQ.3)THEN
          IF(INO.EQ.9)THEN
            helptopic='jk_page_direct_normal'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL PHELPD('rad prediction',nbhelp,'rad prediction',
     &         0,0,IER)
            ICV=1
            ICK(1)=1
            ICK(2)=1
            CALL PSOLRD(IYDS,IYDF)
            CALL EDISP(IUOUT,'Calculating direct and diffuse radiation')
            GOTO 11
          ENDIF
          WRITE(PRML,'(2A)')PRM(1:LNBLNK(PRM)),' occur ?'
          helptopic='hour_max_min'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKR(HOUR,'At which hour does the maximum ',PRML
     &    ,0.,'F',24.,'F',1.,'max hour',IER,nbhelp)
          IF(IER.NE.0)GOTO 999
          CALL EASKR(VALMAX,'Maximum ',PRM,
     &    V_MIN,'F',V_MAX,'-',1.,'max value',IER,2)
          IF(IER.NE.0)GOTO 999
          CALL EASKR(VALMIN,'Minimum ',PRM,
     &    V_MIN,'F',V_MAX,'-',1.,'min value',IER,2)
          IF(IER.NE.0)GOTO 999
          CALL SINUSD(HOUR,VALMAX,VALMIN,WVEL)
          DO 50 ID=IYDS,IYDF
            IDI=ID-IYDS+1
            DO 60 IHR=1,24
              IF(INO.EQ.3)THEN
                PRADB(IDI,IHR)=WVEL(IHR)
              ELSEIF(INO.EQ.4)THEN
                PRADD(IDI,IHR)=WVEL(IHR)
              ELSEIF(INO.EQ.5)THEN
                PTEMP(IDI,IHR)=WVEL(IHR)
              ELSEIF(INO.EQ.6)THEN
                PRH(IDI,IHR)=WVEL(IHR)
              ELSEIF(INO.EQ.7)THEN
                PWV(IDI,IHR)=WVEL(IHR)
              ELSEIF(INO.EQ.88)THEN
                PWD(IDI,IHR)=WVEL(IHR)
              ENDIF
   60       CONTINUE
   50     CONTINUE
        ENDIF

C Wind speed reduction based on wind direction (if allowable maximum
C segments is changed then remember to change maximum allowable angular 
C segments MAAS)
        NSEG=3   ! initial value
        IF(IST.EQ.2)THEN
          IF(INO.EQ.9)THEN
            helptopic='interdependant_rh_temp'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKI(NSEG,' How many angular segments to divide into?'
     &      ,'(see help)',1,'F',MAAS,'F',3,'constant dir',IER,nbhelp)
            IANGS=0
            DO 7485 ISEG=1,NSEG
              IF(ISEG.EQ.NSEG)GOTO 5155
              WRITE(STR,'(A,I2)')'Finishing angle for segment no. ',ISEG
              CALL EASKI(IANGF,STR,'(see help)',
     &          1,'F',360,'F',360,'constant dir',IER,nbhelp)
              GOTO 5156
 5155         IANGS=IANGF
              IANGF=360
 5156         NANG(ISEG)=IANGF
              WRITE(STR,'(A,I3,A,I3)')
     &        'What is the wind speed reduction factor ',IANGS,' to '
     &        ,IANGF
              CALL EASKR(REDF,STR,'i.e. fraction of original speed?',
     &          0.,'-',0.,'-',1.,'wind break speed reduction',
     &          IER,nbhelp)
              IANGS=IANGF
              WRDF(ISEG)=REDF
 7485       CONTINUE
            DO 5045 ID=IYDS,IYDF
              IDI=ID-IYDS+1
              DO 6045 IHR=1,24
                FOUND=.FALSE.
                DO 7045 ISEG=1,NSEG
                  IF(.NOT.FOUND)THEN
                    IF(PWD(IDI,IHR).LT.NANG(ISEG))THEN
                      PWV(IDI,IHR)=NINT(WRDF(ISEG)*REAL(PWV(IDI,IHR)))
                     FOUND=.TRUE.
                    ENDIF
                  ENDIF
 7045           CONTINUE
 6045         CONTINUE
 5045       CONTINUE
          ENDIF
        ENDIF
      elseif(ino.eq.10)then

C Draw variables.
        IF(ICV.GT.0)THEN
          call startbuffer()
          CALL PDRAW(ICV,1,IYDS,IYDF)
        ELSE
          call usrmsg(' Draw facility not set-up correctly',
     &                ' please select again...','W')
        ENDIF
        goto 11
      elseif(ino.eq.11)then
        DO 20 I=1,4
          IF(ICK(I).NE.1)THEN
            dok=.true.
            helptopic='specify_more_variables'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL ASKOK(' Do you want to make any more changes before',
     &        ' saving the weather data?',OK,dok,nbhelp)
            IF(OK)THEN
              goto 11
            else

C Update weather binary file for each altered day as well as the tail.
              DO 156 IDAY=IYDS,IYDF
                IDI=IDAY-IYDS+1
                DO 157 IT=1,MT 
                  CDNR(IT)=PRADB(IDI,IT)
                  CDIF(IT)=PRADD(IDI,IT)
                  CTMP(IT)=PTEMP(IDI,IT)
                  CHUM(IT)=PRH(IDI,IT)
                  CVEL(IT)=PWV(IDI,IT)
                  CDIR(IT)=PWD(IDI,IT)
 157            CONTINUE
                CALL CLMPUT(IDAY)
 156          CONTINUE
              IREC=366
              IADN=IDNGH
              WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)IYEAR,IADN
              IREC=367
              WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLMLOC
              IREC=368
              WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLAT,CLONG
              ISAVE=1
              call edisp(iuout,'Weather data saved for...')
              call edisp(iuout,PERST2)
              GOTO 11
            ENDIF
          ENDIF
   20   CONTINUE
      ELSE
        GOTO 11
      ENDIF
      GOTO 11

C Error handling
  999 helptopic='io_error_message'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL PHELPD('data i/o error',nbhelp,'i/o exception handler',
     &  0,0,IER)
      RETURN

1000  write(outs,'(A,I4)')' Problem writing weather data on rec ',IREC
      call usrmsg(outs,' returning....','W')
      goto 999

      END

C POLYTR transforms weather data for a given period according to some
C polynomial function
      SUBROUTINE POLYTR(ICLMT,IYDS,IYDF)
      PARAMETER (MT=24,MD=365)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PCLIM/PRADB(MD,MT),PRADD(MD,MT),PTEMP(MD,MT),
     &             PWV(MD,MT),PWD(MD,MT),PRH(MD,MT),PLAST(MD,MT)
      DIMENSION A(10)
      character COEFFS*72,STR*72
      integer ILC  ! for radio button

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

C Setup limits for weather parameters
      helpinsub='POLYTR'  ! set for this subroutine
      IF(ICLMT.EQ.1)THEN
        CLMIN=0.
        CLMAX=1400.
      ELSEIF(ICLMT.EQ.2)THEN
        CLMIN=0.
        CLMAX=1400.
      ELSEIF(ICLMT.EQ.3)THEN
        CLMIN=-1000.
        CLMAX=1000.
      ELSEIF(ICLMT.EQ.4)THEN
        CLMIN=0.
        CLMAX=100.
      ELSEIF(ICLMT.EQ.5)THEN
        CLMIN=0.
        CLMAX=10000.
      ELSEIF(ICLMT.EQ.6)THEN
        CLMIN=0.
        CLMAX=360.
      ENDIF

      IDS=IYDS
      helptopic='polynomial_fit_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
      COEFFS=' '
      ILC=1
      CALL EASKATOG('Change original weather value','(see help)'
     & ,'set to constant','change by constant','fraction of original'
     &    ,'polynomial transform ','continue',' ',' ',ILC,nbhelp)
      IF(ILC.EQ.5)RETURN
      K=0
      IF(ILC.EQ.1)THEN
        NORD=0
        CALL EASKS(COEFFS,'What is the constant value','(see help)',
     &   5,' ','constant weather parameter',IER,nbhelp)
        CALL EGETWR(COEFFS,K,A(1),CLMIN,CLMAX,'-','coefficients',IER)
        IF(IER.NE.0)GOTO 999
      ELSEIF(ILC.EQ.2)THEN
        NORD=1
        CALL EASKS(COEFFS,'Change by what constant value','(see help)',
     &   5,' ','constant weather parameter',IER,nbhelp)
        CALL EGETWR(COEFFS,K,A(2),CLMIN,CLMAX,'-','coefficients',IER)
        IF(IER.NE.0)GOTO 999
        A(1)=1.
      ELSEIF(ILC.EQ.3)THEN
        NORD=1
        CALL EASKS(COEFFS,'Change by what fraction','(see help)',
     &   5,' ','constant weather parameter',IER,nbhelp)
        CALL EGETWR(COEFFS,K,A(1),CLMIN,CLMAX,'-','coefficients',IER)
        IF(IER.NE.0)GOTO 999
        A(2)=0.
      ELSEIF(ILC.EQ.4)THEN
        if(NORD.eq.0) NORD=1
        CALL EASKI(NORD,' What is the order of polynomial?','(see help)'
     &    ,0,'W',3,'W',0,'constant dir',IER,nbhelp)
        IF(IER.NE.0)GOTO 999
        helptopic='polynomial_coef'
        call gethelptext(helpinsub,helptopic,nbhelp)
        write(STR,'(A,I1,A)')'Enter ',NORD+1,
     &  ' coefficients (maximum powers first)'
        CALL EASKS(COEFFS,STR,' ',72,' ','polynom coeff',IER,nbhelp)
        IF(IER.NE.0)GOTO 999
        K=0
        DO 101 II=1,NORD+1
          CALL EGETWR(COEFFS,K,A(II),-100.,100.,'-','coefficients',IER)
          IF(IER.NE.0)GOTO 999
  101   CONTINUE
      ENDIF
      iydi=iydf
      CALL EDAYR(IYDF,IDAY,IMNTH)
      DO 505 ID=IDS,IYDI
        IDI=ID-IYDS+1
        DO 605 IHR=1,24
          IF(ICLMT.EQ.1)THEN
            X=PRADB(IDI,IHR)
            CALL POLYNOM(X,A,NORD,Y)
            PRADB(IDI,IHR)=Y
          ELSEIF(ICLMT.EQ.2)THEN
            X=PRADD(IDI,IHR)
            CALL POLYNOM(X,A,NORD,Y)
            PRADD(IDI,IHR)=Y
          ELSEIF(ICLMT.EQ.3)THEN
            X=PTEMP(IDI,IHR)
            CALL POLYNOM(X,A,NORD,Y)
            PTEMP(IDI,IHR)=Y
          ELSEIF(ICLMT.EQ.4)THEN
            X=PRH(IDI,IHR)
            CALL POLYNOM(X,A,NORD,Y)
            PRH(IDI,IHR)=Y
          ELSEIF(ICLMT.EQ.5)THEN
            X=PWV(IDI,IHR)
            CALL POLYNOM(X,A,NORD,Y)
            PWV(IDI,IHR)=Y
          ELSEIF(ICLMT.EQ.6)THEN
            X=PWD(IDI,IHR)
            CALL POLYNOM(X,A,NORD,Y)
            PWD(IDI,IHR)=Y
          ENDIF

C Range checking of parameters should be moved to above if-else ladder
          IF((PRADB(IDI,IHR)+PRADD(IDI,IHR)).GT.1400.)THEN
            PRADB(IDI,IHR)=1400.-PRADD(IDI,IHR)
            CALL EDISP(IUOUT,'resetting radiation to within 1400W/m^2')
          ENDIF
          IF(PTEMP(IDI,IHR).GT.1000..OR.PTEMP(IDI,IHR).LT.-1000.)THEN
            PTEMP(IDI,IHR)=0.0
            CALL EDISP(IUOUT,'out of range temperature reset to 0')
          ENDIF
          IF(PRH(IDI,IHR).GT.100..OR.PRH(IDI,IHR).LT.0.)THEN
            PRH(IDI,IHR)=50.
            CALL EDISP(IUOUT,'out of range RH reset to 50%')
          ENDIF
          IF(PWV(IDI,IHR).GT.1000..OR.PWV(IDI,IHR).LT.0.)THEN
            PWV(IDI,IHR)=0
            CALL EDISP(IUOUT,'out of range wind speed reset to 0')
          ENDIF
          IF(PWD(IDI,IHR).GT.360..OR.PWD(IDI,IHR).LT.0.)THEN
            PWD(IDI,IHR)=0
            CALL EDISP(IUOUT,'out of range wind direction reset to 0')
          ENDIF
  605   CONTINUE
  505 CONTINUE
      RETURN

C Error trap
  999 helptopic='polyr_io_error_message'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL PHELPD('data i/o error',nbhelp,'i/o exception handler',
     &  0,0,IER)
      RETURN
      END

C Subroutine 'PDRAW' will draw the predicted climatic data for any
C period within the climatic period predicted by the user.
C For the variable under analysis it is identified thus;
C   ICV=1 graphs the normal direct and diffuse solar radiation,
C   ICV=2 graphs the ambient dry bulb temperature,
C   ICV=3 graphs the relative humidity,
C   ICV=4 graphs the wind velocity.
C ICL is zero instructs the graphing to omit the previous
C   value of the selected data. If ICL is positive then draw
C   the current and previous values. If ICL is negative draw
C   only the past value.

C IYDS is the start of the analysis period, IYDF the finish.
C The user may choose a more suitable period within IYDS to IYDF by
C seleting the PERIOD facility from menu PDRAW.

      SUBROUTINE PDRAW(ICV,ICL,IYDS,IYDF)
      PARAMETER (MD=365,MT=24)

C PRADB is direct normal radiation, PRADD is diffuse radiation
C PTEMP is ambient dry bulb, PWV is wind velocity
C PRH is relative hunidity, PLAST is previous value of current focus.
      COMMON/PCLIM/PRADB(MD,MT),PRADD(MD,MT),PTEMP(MD,MT),
     &             PWV(MD,MT),PWD(MD,MT),PRH(MD,MT),PLAST(MD,MT)
      COMMON/CLIM1/CLMV(7,MD,MT),ICMAX,ICMAX2,ICMIN

C<< Redraw should be done after clearing graphical feedback area,
      DO 100 I=1,7
        DO 110 J=1,31
          DO 120 K=1,24
            CLMV(I,J,K)=0.0
  120     CONTINUE
  110   CONTINUE
  100 CONTINUE
      IDS=1
      IDF=IYDF-IYDS+1

C Set up the climatic data for determination of the max/min values.
      ICMAX=-10000
      ICMAX2=-10000
      ICMIN=10000
      DO 10 I=IDS,IDF
        DO 20 J=1,24
          if(icv.eq.1)then
            IF(NINT(PRADB(I,J)).GT.ICMAX)ICMAX=NINT(PRADB(I,J))
            IF(NINT(PRADD(I,J)).GT.ICMAX2)ICMAX2=NINT(PRADD(I,J))
            ICMIN=0
            CLMV(1,I,J)=PRADB(I,J)
            CLMV(5,I,J)=PRADD(I,J)
            if(icl.ne.0) CLMV(7,I,J)=PLAST(I,J)
          elseif(icv.eq.2)then
            IF(NINT(PTEMP(I,J)).GT.ICMAX)ICMAX=NINT(PTEMP(I,J))
            IF(NINT(PTEMP(I,J)).LT.ICMIN)ICMIN=NINT(PTEMP(I,J))
            CLMV(2,I,J)=PTEMP(I,J)
            if(icl.ne.0) CLMV(7,I,J)=PLAST(I,J)
          elseif(icv.eq.3)then
            IF(NINT(PRH(I,J)).GT.ICMAX)ICMAX=NINT(PRH(I,J))
            IF(NINT(PRH(I,J)).LT.ICMIN)ICMIN=NINT(PRH(I,J))
            CLMV(3,I,J)=PRH(I,J)
            if(icl.ne.0) CLMV(7,I,J)=PLAST(I,J)
          elseif(icv.eq.4)then
            IF(NINT(PWV(I,J)).GT.ICMAX)ICMAX=NINT(PWV(I,J))
            IF(NINT(PWV(I,J)).LT.ICMIN)ICMIN=NINT(PWV(I,J))
            CLMV(4,I,J)=PWV(I,J)
            if(icl.ne.0) CLMV(7,I,J)=PLAST(I,J)
          endif
   20   CONTINUE
   10 CONTINUE
      CALL PCGRF(ICV,ICL,IDS,IDF)
      RETURN
      END

C Subroutine 'PSOLRD' is based upon the work of J.K. Page (Sheffield),
C it is used to predict the direct normal and the horizontal diffuse
C solar irradiance for any site at any time of day for particular
C values of atmospheric turbidity and precipitable water content.
      SUBROUTINE PSOLRD(IYDS,IYDF)
      PARAMETER (MT=24,MD=365)
      COMMON/PCLIM/PRADB(MD,MT),PRADD(MD,MT),PTEMP(MD,MT),
     &             PWV(MD,MT),PWD(MD,MT),PRH(MD,MT),PLAST(MD,MT)
      DIMENSION SOLALT(24),SOLAZI(24)
      DIMENSION B(4,3)
      character outs*124
      logical ok,dok

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      DATA PI/3.14159265/,ISC/1353/
      DATA B/-0.129641,-6.42111E-2,-0.46883E-2,0.0844097E-2,
     &        0.412828E-2,-0.801046E-2,0.220414E-2,-0.0191442E-2,
     &       -1.12096E-4,1.53069E-4,-0.429818E-4,0.0374176E-4 /

      helpinsub='PSOLRD'  ! set for this subroutine
      RLATIT=0.
      helptopic='north_is_positive'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKR(RLATIT,'Latitude (North +ve: South -ve)',
     &  'for the site under analysis ? ',
     &    -90.,'F',90.,'-',32.,'latitude',IER,nbhelp)

      RLONGT=0.
      CALL EASKR(RLONGT,'Longitude (East +ve: West -ve)',
     &  'for the site under analysis ? ',
     &    -180.,'F',180.,'-',0.,'longitude',IER,nbhelp)

      HTSITE=0.
      CALL EASKR(HTSITE,
     &  'What is the elevation of the site above',' sea level (M)?',
     &    0.,'-',0.,'-',10.,'elevation',IER,nbhelp)

      helptopic='recommended_turbidity'
      call gethelptext(helpinsub,helptopic,nbhelp)
      ATTURB=0.0
      CALL EASKR(ATTURB,'Value of atmospheric turbidity?','(see help)',
     &    0.,'F',1.,'W',0.19,'turbidity',IER,nbhelp)

      IDX=0
      IF(IYDS.GT.IYDF)IDX=365
      IDX=(IYDS+IYDF+IDX)/2
      IF(IDX.GT.365)IDX=IDX-365
      DR=(2.0*PI*IDX)/365.0
      CDR=COS(DR)
      SDR=SIN(DR)
      C2DR=COS(2*DR)
      S2DR=SIN(2*DR)
      C3DR=COS(3*DR)
      S3DR=SIN(3*DR)

C Estimate the precipitable water content.
      PRECWC=10.44-6.468*CDR-3.492*SDR+1.056*C2DR+2.049*S2DR
     &       -0.128*C3DR+0.579*S3DR

      WRITE(outs,8)PRECWC
    8 FORMAT(' The mean precipitable daily water content is',F6.2,
     &' (mm)')
      dok=.true.
      helptopic='review_derived_value'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ASKOK(outs,'Is this value satisfactory?',OK,dok,nbhelp)
      if(ok)goto 32
      helptopic='precip_water_content'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKR(PRECWC,' Average precipitable water content',
     &  ' for the period under analysis ? ',
     &    0.,'-',0.,'-',0.,'prec water',IER,nbhelp)
      GOTO 19
   32 PRECWC=-1.0
   19 ITS=1

C A corrective value, depending upon the height of the
C site, is established.
      HTS=HTSITE/1000
      CORALT=EXP(HTS*(-0.1174-0.0017*HTS))
      DO 10 ID=IYDS,IYDF
      IDI=ID-IYDS+1
      IDA=ID
      DR=(2.0*PI*ID)/365.0
      CDR=COS(DR)
      SDR=SIN(DR)
      C2DR=COS(2*DR)
      S2DR=SIN(2*DR)
      C3DR=COS(3*DR)
      S3DR=SIN(3*DR)

C A correction to the solar constant is found
C  depending upon the day of the year.
      CORISC=(ISC+45.326*CDR+1.8037*SDR+0.88018*C2DR
     &        +0.09746*S2DR-0.00461*C3DR+0.18412*S3DR)/ISC

C If the precipitable water content value is unknown, it is estimate.
      IF(PRECWC.GE.0.0)GOTO 11
      PRECWC=10.44-6.468*CDR-3.492*SDR+1.056*C2DR+2.049*S2DR
     &       -0.128*C3DR+0.579*S3DR

C Estimate the daily hourly values of the solar altitude
C and azimuth.
   11 CALL AZALT(IDA,ITS,RLONGT,RLATIT,SOLALT,SOLAZI)
      DO 20 IHR=1,24

C ITSHFT is not defined!!!
      ITSHFT=0
      IX=IHR-ITSHFT
      IF(SOLALT(IHR).GT.0.0)GOTO 12
      PRADB(IDI,IHR)=0.
      PRADD(IDI,IHR)=0.
      GOTO 20

C Estimate the air mass corrected for height of site.
   12 IF(SOLALT(IX).LT.0.0)GOTO 61
      SSA=ESIND(SOLALT(IX))
      IF(SOLALT(IX).LT.10.0)GOTO 13
      AMASS=1.0/SSA
      GOTO 14
   13 AMASS=EXP(3.67985-24.4465*SSA+154.017*(SSA**2)
     &      -742.181*(SSA**3)+2263.36*(SSA**4)-3804.89*(SSA**5)+
     &      2661.05*(SSA**6))
   14 CORAM=AMASS*CORALT

C Calculation of the direct normal component PRADB
      D2=0
      DO 30 I=1,4
      DO 40 J=1,3
      IF(J.EQ.1)D1=0
      D1=D1+(B(I,J)*(PRECWC**(J-1)))
   40 CONTINUE
      D2=D2+D1*(CORAM**(I-1))
   30 CONTINUE
      BM=ISC*EXP(D2)
      PRADB(IDI,IHR)=BM*CORISC*EXP(-(ATTURB*CORAM))
      IF(PRADB(IDI,IHR).GT.0.0)GOTO 58
   61 PRADB(IDI,IHR)=0.
      PRADD(IDI,IHR)=0.
      GOTO 20

C Calculation of the diffuse radiation component PRADD.
   58 HT=SOLALT(IX)/10.0
      A0=2.+47.382*HT+29.671*(HT**2)-15.861*(HT**3)+4.3463*(HT**4)
     &   -0.57764*(HT**5)+0.03472*(HT**6)-0.0007362*(HT**7)
      A1=(297.0+1.8313*HT-3.7082*(HT**2)+4.1233*(HT**3)
     &   -0.6409*(HT**4)+0.02855*(HT**5))/1000
      PRADD(IDI,IHR)=(CORISC*A0)-(A1*PRADB(IDI,IHR)*
     &                    (ESIND(SOLALT(IX))))
   20 CONTINUE
   10 CONTINUE

      RETURN
      END

C **************
C  Subroutine SINUSD is used to predict values of any
C  variable throughout a day assuming a sinusoidal variation.
C  The tie at which the maximum value occurs combined with
C  the maximum and minimum values of the required variable
C  are passed across to this subroutine which will
C  estimate the 24 hourly values.

      SUBROUTINE SINUSD(HOUR,XMAX,XMIN,SINU)
      DIMENSION SINU(24)
      XDIFF=(XMAX-XMIN)/2.0
      XMEAN=(XMAX+XMIN)/2.0
      xhour=hour-6.0
      if(xhour.lt.0.0)xhour=xhour+24.0
      xtime=15.0*xhour
      DO 10 I=1,24
      sinu(i)=xmean+xdiff*(esind((15*i)-xtime))
   10 CONTINUE
      RETURN
      END

C **************
C Subroutine POLYNOM is used to transform the number x to y following the 
C formula y=a_n*x^n + a_(n-1)*x^(n-1) + .... + a_0
C This subroutine is passed A as a vector with n+1 rows

      SUBROUTINE POLYNOM(X,A,N,Y)

      real A(N+1)

      y=A(1)
      DO 2 I=1,N
        Y=y*X+A(I+1)
 2    CONTINUE
      RETURN
      END

C **************
C Subroutine APPRDIFFUSE is used to approximate diffuse
C solar radiation data from Global horizontal data based on Muneer's model
C See "Solar radiation and daylight models" book from Muneer 
C (1997) - ISBN: 0-7506-2495-7  pages 101-104.
C and Erbs, Klein and Duffie model: see Solar energy Journal Vol. 28,
C pp.293-302
C Note: Erbs Klein & Duffie often results in lower diffuse which is
C closer to values seen in weather files with direct and diffuse
C pre-computed. Maxwell (as described in Muneer) can result in high
C diffuse values.
      SUBROUTINE APPRDIFFUSE(IYEAR,IMT,IDAY,IHR,IGLOBH,LONGITUDE,
     &           DIFFGRNWCH,DRAD)

C Parameters:
      integer iyear   ! the year to scan for within multi-year data file
      integer imt     ! the current month
      integer iday    ! the current day of the month
      integer ihr     ! the current hour of the day
      integer IGLOBH  ! global horizontal value (in w/m2)
      real LONGITUDE  ! the weather site longitude (west: positive)
      real DIFFGRNWCH ! local time difference from Greenwich. Assumes
                      ! that west is positive (e.g. use -2 for Greece)
      real DRAD       ! diffuse value to be returned Muneer or Maxwell (in w/m2)

C RADTYP Common might be needed in order to specify that weather
C is based on global horizontal and not direct normal. At the moment
C this is specified in MKOREANCLM when this is subroutine is used
C      COMMON/RADTYP/IDNGH

      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/APPRDIFF/iapprox_diff

C IME specifies the minute
      real IME

C Local variables
      real DTOR  ! pi/180.0
      real CAPG,CAPL,CAPC,G360,ALPHA,DEC,SHA,DIFRAT
      real XAM,XKNC,CAPA,CAPB,XMAXI,XMAXHB
      real differ  ! difference between global and beam
      integer IYR1,IMT1
      logical close

      DTOR=3.14159/180.0 

C Minute input is needed. IME could be assumed 30.0
C to fit with half-hour centred data or 1.0 for on the
C hour. Orgill and Hollands and Maxwell are very sensitive to
C this value. Erbs is not so sensitive.
      IME=1.0

C Calculate UT based on time difference from Greenwich.
      XLCT=(1.0*IHR)+(1.0*IME/60.0)
      UT=XLCT+DIFFGRNWCH
      IF (IMT.GT.2) THEN
        IYR1=IYEAR
        IMT1=IMT-3
      ELSE
        IYR1=IYEAR-1
        IMT1=IMT+9
      ENDIF 

      INTT1=INT(30.6*IMT1+0.5)
      INTT2=INT(365.25*(IYR1-1976))
      SMLT=((UT/24.0)+IDAY+INTT1+INTT2-8707.5)/36525.0
      EPSILN=23.4393-0.013*SMLT
      CAPG=357.528+35999.050*SMLT
      IF(CAPG.GT.360.0) THEN
        G360=CAPG-INT(CAPG/360.0)*360.0
      ELSE
        G360=CAPG
      ENDIF
      CAPC=1.915*SIN(G360*DTOR)+0.020*SIN(2*G360*DTOR)
      CAPL=280.460+36000.770*SMLT+CAPC
      IF(CAPL.GT.360.0) THEN
        XL360=CAPL-INT(CAPL/360.0)*360.0
      ELSE
        XL360=CAPL
      ENDIF
      ALPHA=
     &  XL360-2.466*SIN(2*XL360*DTOR)+0.053*SIN(4*XL360*DTOR)
      GHA=15.0*UT-180.0-CAPC+XL360-ALPHA
      IF(GHA.GT.360.0) THEN
        GHA360=GHA-INT(GHA/360.0)*360.0
      ELSE
        GHA360=GHA
      ENDIF
      DEC=ATAN(TAN(EPSILN*DTOR)*SIN(ALPHA*DTOR))/DTOR
      SHA=GHA360-(LONGITUDE)

C Muneer has the Calculation of the Apparent Solar Time
C but espr method does not use it - just commented here
C      AST=12.0+(SHA/15.0)

C Calculate Solar Altitude 
      TRM111=SIN(CLAT*DTOR)*SIN(DEC*DTOR)
      TRM112=
     &  COS(CLAT*DTOR)*COS(DEC*DTOR)*COS((SHA+180.0)*DTOR)
      TRM11=TRM111-TRM112
      SOLALT=ASIN(TRM11)/DTOR  

C Calculate Day Number
      DN1=(IDAY+INTT1+INTT2)
      IMT9=1
      IYR1=IYEAR-1
      IMT1=IMT9+9
      INTT1=INT(30.6*IMT1+0.5)
      INTT2=INT(365.25*(IYR1-1976))
      DN2=(INTT1+INTT2)
      DN=DN1-DN2

C Calculate horizontal Extraterrestrial irRADiance
      ERAD=
     &  1353.0*(1.+0.033*COS(0.0172024*DN))*SIN(SOLALT*DTOR) 

C Muneer has the next line for Maxwell method.
      XMAXI0=1353.0*(1.+0.033*COS(0.0172024*DN))

C Calculate horizontal Diffuse irRADiance Orgill and Hollands
      GRAD=real(IGLOBH)
      XKT=GRAD/ERAD 
      IF(iapprox_diff.EQ.1)THEN

C Muneer model using Orgill and Hollands (eq 3.4.1). Note this
C sometimes results in high diffuse values when the global
C horizontal is high.
        DIFRAT=0.98
        IF(XKT.GT.0.2) THEN
          DIFRAT=1.006-(0.317*XKT)+(3.1241*XKT**2)
     &    -(12.7616*XKT**3)+(9.7166*XKT**4)
        ENDIF

C Occassionally DIFRAT evaluates to greater than one. If so use 0.98
        if(DIFRAT.gt.1.0)then
          DRAD=GRAD*0.98
        else
          DRAD=GRAD*DIFRAT
        endif

C Old logic for Orgill and Hollands.
C        IF(XKT.GT.0.2.AND.XKT.LE.1.0) THEN
C          DIFRAT=1.006-(0.317*XKT)+(3.1241*XKT**2)
C     &    -(12.7616*XKT**3)+9.7166*XKT**4
C        ELSEIF(XKT.GT.1.0) THEN
C          XKT=1.0
C          DIFRAT=1.006-(0.317*XKT)+(3.1241*XKT**2)
C     &    -(12.7616*XKT**3)+(9.7166*XKT**4)
C        ENDIF
      ELSEIF(iapprox_diff.EQ.2)THEN

C Calculate horizontal beam irradiance using Maxwell model on
C page 102 of Muneer book (1987). If SOLALT is negative then
C set to be .98 of GRAD or if GRAD is less than 1W/2m ignore.
C Brackets included to clarify equations.
        if(SOLALT.lt.0.2.or.GRAD.lt.1.0)then
          DRAD=GRAD*0.98
        else
          XAM=1.0/SIN(SOLALT*DTOR)
          XKNC=0.866-0.122*XAM+0.0121*XAM**2
     &       -(0.000653*XAM**3)+0.000014*XAM**4 
     
          IF(XKT.LE.0.6) THEN
            CAPA=0.512-(1.56*XKT)+(2.286*XKT**2)-(2.222*XKT**3)
            CAPB=0.37+(0.962*XKT)
            CAPC=-0.28+(0.932*XKT)-(2.048*XKT**2)
          ELSE
            CAPA=-5.743+(21.77*XKT)-(27.49*XKT**2)+(11.56*XKT**3)
            CAPB=41.4-(118.5*XKT)+(66.05*XKT**2)+(31.9*XKT**3)
            CAPC=-47.01+(184.2*XKT)-(222.0*XKT**2)+(73.81*XKT**3)
          ENDIF

C Calculate the beam (XMAXHB) and if a posive number
C and less than GRAD then subtract it from GRAD to get
C the diffuse. If (XMAXHB) would cause diffuse to be
C greater than global then set it to 0.98 of global.
C Limit the size of the exponent to 60 to reduce the chance
C of a numerical overflow.
          if((XAM*CAPC).gt.60.0)then
            XMAXI=XMAXI0*(XKNC-(CAPA+CAPB*EXP(60.0)))      
          elseif((XAM*CAPC).lt.(-60.0))then
            XMAXI=XMAXI0*(XKNC-(CAPA+CAPB*EXP(-60.0)))      
          else
            XMAXI=XMAXI0*(XKNC-(CAPA+CAPB*EXP(XAM*CAPC)))
          endif     
          XMAXHB=XMAXI*SIN(SOLALT*DTOR)
          if(XMAXHB.gt.0.0.and.GRAD.gt.XMAXHB)then
            DRAD=GRAD-XMAXHB
          else
            DRAD=GRAD*0.98
          endif
        endif

C And if GRAD is zero then set DRAD to zero. 
        call eclose(GRAD,0.0,1.0,close)
        if(close)then
          DRAD=0.0
        endif
      ELSEIF(iapprox_diff.EQ.3)THEN

C Erbs, Klein and Duffie model. This tends to limit the diffuse
C when global horizontal is large.
        IF(XKT.LE.0.22)THEN
          DIFRAT=1.0-0.09*XKT
        ELSEIF(XKT.GT.0.22.AND.XKT.LE.0.8)THEN
          DIFRAT=0.9511-(0.1604*XKT)+(4.388*XKT**2)
     &    -(16.638*XKT**3)+(12.336*XKT**4)
        ELSEIF(XKT.GT.0.8)THEN
          DIFRAT=0.165
        ENDIF

C Occassionally DIFRAT evaluates to greater than one. If so use 0.98
        if(DIFRAT.gt.1.0)then
          DRAD=GRAD*0.98
        else
          DRAD=GRAD*DIFRAT
        endif

      ELSE
        iapprox_diff=1
      ENDIF

      RETURN
      END
