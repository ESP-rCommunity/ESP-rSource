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
C imode = 1 is not currently used.
C imode = 2 the user is interested in transforming the data
C imode = 3 the user is interested in curve fitting.
      SUBROUTINE CLPRED(imode)
#include "esprdbfile.h"
#include "climate.h"
#include "epara.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      PARAMETER (MT=24,MD=365,MAAS=9)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C PCLMVAL holds weather data, similarly to CMRVAL, but for multiple days.
C PLAST holds previous values of current focus.
      COMMON/PCLIM/PCLMVAL(MCM,MD,MT),PLAST(MD,MT)
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

      CHARACTER*27 ITEM(30)
      character PRML*48,PRM*24,CLMLOC*42,PERST2*44,str*60,KEY*1
      character outs*124
      DIMENSION ICK(MCM),WVAL(24),NANG(MAAS),WRDF(MAAS)
      logical OK,FOUND
      integer NITEMS,INO  ! max items and current menu item
      integer menumetric(MCM)

      helpinsub='clpred'  ! set for this subroutine
   1  ISAVE=1
      if(imode.eq.1)then
        CALL EDISP(IUOUT,'Select period')
      elseif(imode.eq.2)then
        CALL EDISP(IUOUT,'Select period to be transformed')
      elseif(imode.eq.3)then
        CALL EDISP(IUOUT,'Select period that is to be predicted')
      endif
      CALL SELPER(IER)
      IF(IER.NE.0)GOTO 999
      IYDS=IDS
      IYDF=IDF
      ID=0
      DO 55 I=IYDS,IYDF
        ID=ID+1
        CALL CLMGET(I,IER)
        DO 656 J=1,24
          do K=1,MCM
            if (CMXST(K)) then
              PCLMVAL(K,ID,J)=CMRVAL(K,J)
            endif
          enddo
  656   CONTINUE
   55 CONTINUE

      WRITE(STR,'(2A)')'Modified ',CLMLOC(1:21)
      CALL EASKS(STR,'On changing weather data it is usual to change'
     &   ,'synopsis ',30,'synopsis. ','new synopsis',IER,0)
      ICV=0
      DO 10 I=1,MCM
        ICK(I)=0
   10 CONTINUE
   11 helptopic='curve_fitting_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

      MHEAD=2
      MCTL=7
      ILEN=NCM
      IPACT=CREATE
      CALL EKPAGE(IPACT)

      if(imode.eq.1)then
        ITEM(1)='1 Change period           '
      elseif(imode.eq.2)then
        ITEM(1)='1 Change transform period '
      elseif(imode.eq.3)then
        ITEM(1)='1 Change curve-fit period '
      endif
      ITEM(2) ='  ------------------------ '

      M=MHEAD
      I=0
      do imet=1,MCM
        if (CMXST(imet)) then
          I=I+1
          menumetric(I)=imet
          IF(I.GE.IST.AND.(I.LE.(IST+MIFULL)))THEN
            M=M+1
            call EMKEY(I,KEY,IER)
            write (ITEM(M),'(a,1x,a)') KEY,CMNAMF(imet)
          ENDIF
        endif
      enddo

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        ITEM(M+1)='  ------------------------ '
      ELSE
        WRITE(ITEM(M+1),'(a8,i2,a4,i2,a)')'0 Page: ',IPM,' of ',MPM,
     &    ' --------- '
      ENDIF

      IF(imode.EQ.1)ITEM(M+2) =' N/A                     '
      IF(imode.EQ.2)ITEM(M+2)='* Wind direction & speed  '
      IF(imode.EQ.3)ITEM(M+2)='* Direct and diffuse model'
      if(imode.eq.1)then
        ITEM(M+3)='! Draw data              '
      elseif(imode.eq.2)then
        ITEM(M+3)='! Draw transformed data  '
      elseif(imode.eq.3)then
        ITEM(M+3)='! Draw curve-fit data    '
      endif
      ITEM(M+4)='> Save weather data        '
      ITEM(M+5)='  ------------------------ '
      ITEM(M+6)='? Help                     '
      ITEM(M+7)='- Exit                     '

C Number of actual items displayed.
      NITMS=M+MCTL

      INO=-2
      if(imode.eq.1)then
        CALL EMENU(' Weather prediction',ITEM,NITEMS,INO)
      elseif(imode.eq.2)then
        CALL EMENU(' Weather transforms',ITEM,NITEMS,INO)
      elseif(imode.eq.3)then
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
          CALL EASKOK(' ','Save data before exiting?',OK,nbhelp)
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
            DO 257 IT=1,24 
              do imet=1,MCM
                if (CMXST(imet)) then
                  CMRVAL(imet,IT)=PCLMVAL(imet,IDI,IT)
                endif
              enddo
 257        CONTINUE
            CALL CLMPUT(IDAY)
 256      CONTINUE

          if (CFVER.eq.1) then
            IREC=366
            IADN=IDNGH
            WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFYEAR,IADN
            IREC=367
            WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFLOC
            IREC=368
            WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFLAT,CFLONG
          else
            IREC=366
            WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFYEAR
            IREC=IREC+1
            WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &        CFLOC,CFLAT,CFLONG
            IREC=IREC+1
            WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &        (CMCOL(i),i=1,CFMCM)
            IREC=IREC+1
            WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFVER
          endif

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
      ELSEIF(INO.gt.MHEAD.and.INO.le.(NITMS-MCTL))THEN

C Select a weather metric.
        CALL KEYIND(NITMS,INO,imenu,IO)
        IP=menumetric(imenu)

C Save the current values of the item of interest into PLAST.
        DO 356 IDAY=IYDS,IYDF
          IDI=IDAY-IYDS+1
          DO 357 IT=1,MT 
            PLAST(IDI,IT)=PCLMVAL(IP,IDI,IT)
 357      CONTINUE
 356    CONTINUE

        ICK(IP)=1
        PRM=CMNAMF(IP)
        if (IP.eq.1 .or. IP.eq.5) then
          V_MAX=real(CFMAX(IP))/10.
          V_MIN=real(CFMIN(IP))/10.
        else
          V_MAX=real(CFMAX(IP))
          V_MIN=real(CFMIN(IP))
        endif

C   ICV=1 graphs the normal direct and diffuse solar radiation,
C   ICV=2 graphs the ambient dry bulb temperature,
C   ICV=3 graphs the relative humidity,
C   ICV=4 graphs the wind velocity.
C   ICV=5 graphs the total and opaque cloud cover.
C   ICV=6 graphs the atmospheric pressure.
        if(IP.eq.1)then
          ICV=2
        elseif(IP.eq.2 .or. IP.eq.3 .or. IP.eq.4)then
          ICV=1
        elseif(IP.eq.7)then
          ICV=3
        elseif(IP.eq.5)then
          ICV=4
        elseif(IP.eq.6)then
          ICV=4
        elseif(IP.eq.8 .or. IP.eq.9)then
          ICV=5
        elseif(IP.eq.10)then
          ICV=6
        endif

        call startbuffer()
        CALL PDRAW(ICV,-1,IYDS,IYDF)

        IF(IP.EQ.7)THEN
          helptopic='interdependant_rh_temp'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('RH prediction',nbhelp,'RH changing',0,0,IER)
        endif

C Sinusoidal variable prediction.
        if (imode.eq.3) then
          IF(IP.EQ.6)THEN

C<< Normal distribution of wind direction can be supported in future
            write(str,'(2a)')'wind direction prediction is currently',
     &      'not supported'
            call edisp(iuout,' ')
            call edisp(iuout,str)
            call edisp(iuout,' ')
            GOTO 11
          ELSEIF(IP.EQ.8 .or. IP.eq.9)THEN
            write(str,'(2a)')'cloud cover prediction is currently',
     &      'not supported'
            call edisp(iuout,' ')
            call edisp(iuout,str)
            call edisp(iuout,' ')
            GOTO 11          
          ELSEIF(IP.EQ.10)THEN
            write(str,'(2a)')'pressure prediction is currently',
     &      'not supported'
            call edisp(iuout,' ')
            call edisp(iuout,str)
            call edisp(iuout,' ')
            GOTO 11
          ENDIF

          ISAVE=0
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
          CALL SINUSD(HOUR,VALMAX,VALMIN,WVAL)
          DO 50 ID=IYDS,IYDF
            IDI=ID-IYDS+1
            DO 60 IHR=1,24
              PCLMVAL(IP,IDI,IHR)=WVAL(IHR)
   60       CONTINUE
   50     CONTINUE

C Transform data.
        elseif(imode.EQ.2)then
          ISAVE=0
          CALL POLYTR(IP,IYDS,IYDF)
        ENDIF
      elseif (INO.eq.(NITMS-6)) then
 
C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF

      elseif (INO.eq.NITMS-5) then

C Prediction of solar radiation as function of turbidity and
C precipitable water content
        IF(imode.EQ.3)THEN
          helptopic='jk_page_direct_normal'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('rad prediction',nbhelp,'rad prediction',
     &       0,0,IER)
          ICV=4
          ICK(2)=1
          ICK(3)=1
          CALL PSOLRD(IYDS,IYDF)
          CALL EDISP(IUOUT,'Calculating direct and diffuse radiation')
          GOTO 11
        ENDIF

C Wind speed reduction based on wind direction (if allowable maximum
C segments is changed then remember to change maximum allowable angular 
C segments MAAS)
        NSEG=3   ! initial value
        IF(imode.EQ.2)THEN
          IF(INO.EQ.9)THEN
            helptopic='interdependant_rh_temp'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKI(NSEG,' ',
     &           'Number of angular segments required?',
     &           1,'F',MAAS,'F',3,'constant dir',IER,nbhelp)
            IANGS=0
            DO 7485 ISEG=1,NSEG
              IF(ISEG.EQ.NSEG)GOTO 5155
              WRITE(STR,'(A,I2)')'Finishing angle for segment ',ISEG
              CALL EASKI(IANGF,' ',STR,
     &          1,'F',360,'F',360,'constant dir',IER,nbhelp)
              GOTO 5156
 5155         IANGS=IANGF
              IANGF=360
 5156         NANG(ISEG)=IANGF
              WRITE(STR,'(A,I3,A,I3,A)')
     &        'Wind speed reduction factor ',IANGS,' to ',IANGF,'.'
              CALL EASKR(REDF,' ',STR,
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
                    IF(PCLMVAL(6,IDI,IHR).LT.NANG(ISEG))THEN
                      PCLMVAL(5,IDI,IHR)=
     &                  NINT(WRDF(ISEG)*REAL(PCLMVAL(5,IDI,IHR)))
                      FOUND=.TRUE.
                    ENDIF
                  ENDIF
 7045           CONTINUE
 6045         CONTINUE
 5045       CONTINUE
          ENDIF
        ENDIF
      elseif(ino.eq.NITMS-4)then

C Draw variables.
        IF(ICV.GT.0)THEN
          call startbuffer()
          CALL PDRAW(ICV,1,IYDS,IYDF)
        ELSE
          call usrmsg(' Draw facility not set-up correctly',
     &                ' please select again...','W')
        ENDIF
        goto 11
      elseif(ino.eq.NITMS-3)then
        DO 20 I=1,4
          IF(ICK(I).NE.1)THEN
            helptopic='specify_more_variables'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKOK(' ','More changes?',OK,nbhelp)
            IF(OK)THEN
              goto 11
            else

C Update weather binary file for each altered day as well as the tail.
              DO 156 IDAY=IYDS,IYDF
                IDI=IDAY-IYDS+1
                DO 157 IT=1,MT 
                  do imet=1,MCM
                    if (CMXST(imet)) then
                      CMRVAL(imet,IT)=PCLMVAL(imet,IDI,IT)
                    endif
                  enddo
 157            CONTINUE
                CALL CLMPUT(IDAY)
 156          CONTINUE

              if (CFVER.eq.1) then
                IREC=366
                IADN=IDNGH
                WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFYEAR,IADN
                IREC=367
                WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFLOC
                IREC=368
                WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFLAT,CFLONG
              else
                IREC=366
                WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &            CFYEAR
                IREC=IREC+1
                WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &            CFLOC,CFLAT,CFLONG
                IREC=IREC+1
                WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &            (CMCOL(j),j=1,CFMCM)
                IREC=IREC+1
                WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &            CFVER
              endif

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
#include "climate.h"
#include "help.h"

      PARAMETER (MT=24,MD=365)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/PCLIM/PCLMVAL(MCM,MD,MT),PLAST(MD,MT)
      DIMENSION A(10)
      character COEFFS*72,STR*72,outs*124
      integer ILC  ! for radio button

      helpinsub='clpred'  ! set for this subroutine

C Setup limits for weather parameters
      if (ICLMT.eq.1 .or. ICLMT.eq.5) then
        CLMIN=real(CFMIN(ICLMT))/10.
        CLMAX=real(CFMAX(ICLMT))/10.
      else
        CLMIN=real(CFMIN(ICLMT))
        CLMAX=real(CFMAX(ICLMT))
      endif

      IDS=IYDS
      helptopic='polynomial_fit_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
      COEFFS=' '
      ILC=1
      CALL EASKATOG(' ','Weather parameter:',
     &    'set to constant','change by constant','fraction of original',
     &    'polynomial transform','cancel',' ',' ',ILC,nbhelp)
      IF(ILC.EQ.5)RETURN
      K=0
      IF(ILC.EQ.1)THEN
        NORD=0
        CALL EASKS(COEFFS,' ','What is the constant value',
     &    5,' ','constant weather parameter',IER,nbhelp)
        CALL EGETWR(COEFFS,K,A(1),CLMIN,CLMAX,'-','coefficients',IER)
        IF(IER.NE.0)GOTO 999
      ELSEIF(ILC.EQ.2)THEN
        NORD=1
        CALL EASKS(COEFFS,' ','Change by what constant value?',
     &   5,' ','constant weather parameter',IER,nbhelp)
        CALL EGETWR(COEFFS,K,A(2),CLMIN,CLMAX,'-','coefficients',IER)
        IF(IER.NE.0)GOTO 999
        A(1)=1.
      ELSEIF(ILC.EQ.3)THEN
        NORD=1
        CALL EASKS(COEFFS,' ','Change by what fraction',
     &   5,' ','constant weather parameter',IER,nbhelp)
        CALL EGETWR(COEFFS,K,A(1),CLMIN,CLMAX,'-','coefficients',IER)
        IF(IER.NE.0)GOTO 999
        A(2)=0.
      ELSEIF(ILC.EQ.4)THEN
        if(NORD.eq.0) NORD=1
        CALL EASKI(NORD,' ','Polynomial order?',
     &    0,'W',3,'W',0,'constant dir',IER,nbhelp)
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

C Transform value.
          X=PCLMVAL(ICLMT,IDI,IHR)
          CALL POLYNOM(X,A,NORD,Y)
          PCLMVAL(ICLMT,IDI,IHR)=Y

c Range checking.
          if (PCLMVAL(ICLMT,IDI,IHR).gt.CLMAX) then
            write(outs,'(3a,f6.1)')' out of range ',CMNAMF,' set to ',
     &        CLMAX
          elseif (PCLMVAL(ICLMT,IDI,IHR).lt.CLMIN) then
            write(outs,'(3a,f6.1)')' out of range ',CMNAMF,' set to ',
     &        CLMIN
          endif
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
C   ICV=5 graphs the total and opaque cloud cover.
C   ICV=6 graphs the atmospheric pressure.
C ICL is zero instructs the graphing to omit the previous
C   value of the selected data. If ICL is positive then draw
C   the current and previous values. If ICL is negative draw
C   only the past value.

C IYDS is the start of the analysis period, IYDF the finish.
C The user may choose a more suitable period within IYDS to IYDF by
C seleting the PERIOD facility from menu PDRAW.

      SUBROUTINE PDRAW(ICV,ICL,IYDS,IYDF)
#include "climate.h"

      PARAMETER (MD=365,MT=24)

C PCLMVAL is current weather values, PLAST is previous value of current focus.
      COMMON/PCLIM/PCLMVAL(MCM,MD,MT),PLAST(MD,MT)
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
            if (CMXST(4)) then
              iind=4
            else
              iind=3
            endif
            itmp=NINT(PCLMVAL(iind,I,J))
            IF(itmp.GT.ICMAX)ICMAX=itmp
            CLMV(1,I,J)=itmp
            itmp=NINT(PCLMVAL(2,I,J))
            IF(itmp.GT.ICMAX2)ICMAX2=itmp
            CLMV(5,I,J)=itmp
            ICMIN=0
          elseif(icv.eq.2)then
            itmp=NINT(PCLMVAL(1,I,J))
            IF(itmp.GT.ICMAX)ICMAX=itmp
            IF(itmp.LT.ICMIN)ICMIN=itmp
            CLMV(2,I,J)=itmp
          elseif(icv.eq.3)then
            itmp=NINT(PCLMVAL(7,I,J))
            IF(itmp.GT.ICMAX)ICMAX=itmp
            IF(itmp.LT.ICMIN)ICMIN=itmp
            CLMV(3,I,J)=itmp
          elseif(icv.eq.4)then
            itmp=NINT(PCLMVAL(5,I,J))
            IF(itmp.GT.ICMAX)ICMAX=itmp
            IF(itmp.LT.ICMIN)ICMIN=itmp
            CLMV(4,I,J)=itmp
          elseif(icv.eq.5)then
            itmp=NINT(PCLMVAL(8,I,J))
            IF(itmp.GT.ICMAX)ICMAX=itmp
            CLMV(1,I,J)=itmp
            itmp=NINT(PCLMVAL(9,I,J))
            IF(itmp.GT.ICMAX2)ICMAX2=itmp
            CLMV(5,I,J)=itmp
            ICMIN=0
          elseif(icv.eq.6)then
            itmp=NINT(PCLMVAL(10,I,J))
            IF(itmp.GT.ICMAX)ICMAX=itmp
            IF(itmp.LT.ICMIN)ICMIN=itmp
            CLMV(2,I,J)=itmp
          endif
          if(icl.ne.0) CLMV(7,I,J)=PLAST(I,J)
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
#include "climate.h"
#include "help.h"

      PARAMETER (MT=24,MD=365)
      COMMON/PCLIM/PCLMVAL(MCM,MD,MT),PLAST(MD,MT)
      DIMENSION SOLALT(24),SOLAZI(24)
      DIMENSION B(4,3)
      character outs*124
      logical ok

      DATA PI/3.14159265/,ISC/1353/
      DATA B/-0.129641,-6.42111E-2,-0.46883E-2,0.0844097E-2,
     &        0.412828E-2,-0.801046E-2,0.220414E-2,-0.0191442E-2,
     &       -1.12096E-4,1.53069E-4,-0.429818E-4,0.0374176E-4 /

      helpinsub='clpred'  ! set for this subroutine
      RLATIT=0.
      helptopic='north_is_positive'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKR(RLATIT,' ','Latitude (North +ve, South -ve)?',
     &      -90.,'F',90.,'-',32.,'latitude',IER,nbhelp)

      RLONGT=0.
      CALL EASKR(RLONGT,'Longitude difference from reference meridian',
     &  '(East +ve, West -ve)?',
     &    -180.,'F',180.,'-',0.,'longitude diff',IER,nbhelp)

      HTSITE=0.
      CALL EASKR(HTSITE,' ',
     &  'Elevation of  site above sea level (m)?',
     &    0.,'-',0.,'-',10.,'elevation',IER,nbhelp)

      helptopic='recommended_turbidity'
      call gethelptext(helpinsub,helptopic,nbhelp)
      ATTURB=0.0
      CALL EASKR(ATTURB,' ','Atmospheric turbidity?',
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
    8 FORMAT(' Average precipitable daily water content is',F6.2,
     &' (mm)')
      helptopic='review_derived_value'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKOK(outs,'Value OK?',OK,nbhelp)
      if(ok)goto 32
      helptopic='precip_water_content'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKR(PRECWC,' ',
     &   'Average precipitable water content?',
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
      PCLMVAL(3,IDI,IHR)=0.
      PCLMVAL(2,IDI,IHR)=0.
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

C Calculation of the direct normal component PCLMVAL(3,,)
      D2=0
      DO 30 I=1,4
      DO 40 J=1,3
      IF(J.EQ.1)D1=0
      D1=D1+(B(I,J)*(PRECWC**(J-1)))
   40 CONTINUE
      D2=D2+D1*(CORAM**(I-1))
   30 CONTINUE
      BM=ISC*EXP(D2)
      PCLMVAL(3,IDI,IHR)=BM*CORISC*EXP(-(ATTURB*CORAM))
      IF(PCLMVAL(3,IDI,IHR).GT.0.0)GOTO 58
   61 PCLMVAL(3,IDI,IHR)=0.
      PCLMVAL(2,IDI,IHR)=0.
      GOTO 20

C Calculation of the diffuse radiation component PCLMVAL(2,,).
   58 HT=SOLALT(IX)/10.0
      A0=2.+47.382*HT+29.671*(HT**2)-15.861*(HT**3)+4.3463*(HT**4)
     &   -0.57764*(HT**5)+0.03472*(HT**6)-0.0007362*(HT**7)
      A1=(297.0+1.8313*HT-3.7082*(HT**2)+4.1233*(HT**3)
     &   -0.6409*(HT**4)+0.02855*(HT**5))/1000
      PCLMVAL(2,IDI,IHR)=(CORISC*A0)-(A1*PCLMVAL(3,IDI,IHR)*
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
      real LONGITUDE  ! the weather site longitude difference (west: positive)
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
