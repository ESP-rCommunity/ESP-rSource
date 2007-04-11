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

C This file contains the following routines:
C SUM1V:  Generic sums and gains/losses for timestep arrays (AIN(MTS)).
C CMXMN:  Returns maximum and minimum and their timesteps from AIN(MTS).
C SUMSTA: Generic sums and gains/losses for timestep array D based
C         on status of timestep array STAT.
C CHKTIME returns the first and last report iterations for a given day.
C INTRPL: Linear interpolation to return a point T3,V3 on line T1,V1 & T2,V2.
C HALFST: Finds halfstep intervals for interpolation of timestep data,
C         manages beginning and ending times.
C FLTIAV: Filter for timestep data according to IAV.
C HDDATE: Creates a descriptive string for display period.
C HDSTEP: Creates a descriptive string for timestep information.
C STIME:  Takes an integer timestep and returns a string DESCR '12h28'
C SJTIME:  Takes an integer timestep and returns a string DESCR '12:28:30'
C DATIME: Takes an integer timestep and returns a decimal hour of day e.g. 9.75
C DASYSTIME takes an integer plant timestep and returns a decimal hour of day.
C XTVAL1: Copies timestep array ARRAY(MTS) into Common VAL1(IPICK,MTS).
C XTVAL2: Copies timestep array ARRAY(MTS) into Common VAL2(IPICK,MTS).
C XTVAL3: Copies timestep array ARRAY(MTS) into Common VAL3(IPICK,MTS).
C GVTXDUM Takes data in GVAL and moves it to XDUM.
C GVTXDM1 Takes data in GVAL and moves it to XDUM1.
C ASKZON: Standard call to enquire which zone the user wishes to deal with.
C ASKSUR: Standard call to enquire which surface in zone IZONE to deal with.
C PICKSUR: Pick one or many surfaces from given zone.
C PICKANC: Pick a single anchor point from list of given type.
C PIKMRTS: Pick one or many MRT sensors from zone/ sensor list.
C ASKICN: Standard call to enquire which node in a surface.
C ASKRTIM: Enquire month and day and time for snapshot analysis.
C VALIAV: Standard call to compute averages in VAL1().
C CTLEXP: Control feedback to export file or text feedback area.

C ******************** SUM1V 
C SUM1V is a generic routine to take an array AIN(MTS) and returns
C a sum of instances GT 0. and LE 0. in SGT & SLT, as well as
C counters of the items which were GT 0., EQ 0. and LT 0..
C This is done in the range of AIN(ISTART) to AIN(IEND).
C CALLS:     CALLED BY:

      SUBROUTINE SUM1V(AIN,ISTART,IEND,SGT,SLT,IGT,IEQ,ILT)
#include "building.h"
      COMMON/OUTPCH/ICOUT

      DIMENSION AIN(MTS)
      logical close

      SGT=0.
      SLT=0.
      IGT=0
      IEQ=0
      ILT=0

C Check range.
      IF(ISTART.GE.1.AND.IEND.LE.MTS)THEN
        DO 10 I=ISTART,IEND
          call eclose(AIN(I),0.0,0.0001,close)
          if(close)then
            IEQ=IEQ + 1
          else
            IF(AIN(I).LT.0.)THEN
              SLT=SLT + AIN(I)
              ILT=ILT + 1
            ELSEIF(AIN(I).GT.0.)THEN
              SGT=SGT + AIN(I)
              IGT=IGT + 1
            ENDIF
          endif
   10   CONTINUE
      ELSE
        call edisp(icout,' Range error in SUM1V.')
      ENDIF

      RETURN
      END

C ******************** CMXMN ********************

C CMXMN is a generic routine to take an array AIN(MTS) and returns
C the maximum (DMAX) and minimum (DMIN) values and the times of
C occurrence(ITMAX) and (ITMIN) over the timestep range ISTART to IEND.
C If init=0 initialise DMAX & DMIN, otherwise test agains value
C passed into array.

      SUBROUTINE CMXMN(AIN,ISTART,IEND,DMAX,ITMAX,DMIN,ITMIN,INIT)
#include "building.h"
      COMMON/OUTPCH/ICOUT
      DIMENSION AIN(MTS)

      if(init.eq.0)then
        DMAX=0.0
        DMIN=0.0
      endif
      ITMIN=0
      ITMAX=0

C Check range.
      IF(ISTART.GE.1.AND.IEND.LE.MTS)THEN
        DO 10 I=ISTART,IEND
          if(INIT.NE.0)then
            IF(AIN(I).LT.DMIN)THEN
              ITMIN=I
              DMIN=AIN(I)
            ELSEIF(AIN(I).GT.DMAX)THEN
              ITMAX=I
              DMAX=AIN(I)
            ENDIF
          else
            DMAX=AIN(I)
            DMIN=DMAX
            INIT = 1
          endif
   10   CONTINUE
      ELSE
        call edisp(icout,' Range error in CMXMN.')
      ENDIF
      RETURN
      END

C ******************** SUMSTA ********************

C SUMSTA uses the value of items in array STAT(I) ie. GT.EQ.LT 0. to
c sum items in data array D(I) into SGT or SLT.
C This is done in the range of STAT(ISTART) to STAT(IEND).
C The returned values are as follows:
C DGTSGT = D(I) is GT 0 when STAT(I) is GT 0,
C DLTSGT = D(I) is LT 0 when STAT(I) is GT 0,
C DGTSEQ = D(I) is GT 0 when STAT(I) is = 0,
C DLTSEQ = D(I) is LT 0 when STAT(I) is = 0,
C DGTSLT = D(I) is GT 0 when STAT(I) is LT 0,
C DLTSLT = D(I) is LT 0 when STAT(I) is LT 0,

      SUBROUTINE SUMSTA(D,STAT,ISTART,IEND,DGTSGT,DLTSGT,DGTSEQ,DLTSEQ,
     &                  DGTSLT,DLTSLT)
#include "building.h"
      COMMON/OUTPCH/ICOUT

      DIMENSION D(MTS), STAT(MTS)
      logical close

      DGTSGT=0.
      DLTSGT=0.
      DGTSEQ=0.
      DLTSEQ=0.
      DGTSLT=0.
      DLTSLT=0.

C Check range.
      IF(ISTART.GE.1.AND.IEND.LE.MTS)THEN
        DO 10 I=ISTART,IEND
          call eclose(STAT(I),0.0,0.0001,close)
          if(close)then
            IF(D(I).LT.0)DLTSEQ=DLTSEQ+D(I)
            IF(D(I).GT.0)DGTSEQ=DGTSEQ+D(I)
          else
            IF(STAT(I).LT.0.)THEN
              IF(D(I).LT.0)DLTSLT=DLTSLT+D(I)
              IF(D(I).GT.0)DGTSLT=DGTSLT+D(I)
            ELSEIF(STAT(I).GT.0.)THEN
              IF(D(I).LT.0)DLTSGT=DLTSGT+D(I)
              IF(D(I).GT.0)DGTSGT=DGTSGT+D(I)
            ENDIF
          endif
   10   CONTINUE
      ELSE
        call edisp(icout,' Range error in SUMSTA.')
      ENDIF

      RETURN
      END

C ******************** SUMUSR ********************

C SUMUSR uses the user defined mapping of column registers vis
C the row displays for air nodes and surfaces.
C This is done in the range of STAt(ISTART) to STAt(IEND).
C The returned values are as follows:
C Returns REGS(6)

      SUBROUTINE SUMUSR(UD,DATA,ISTART,IEND,IROW,REGS)
#include "building.h"
      COMMON/OUTPCH/ICOUT

      DIMENSION DATA(MTS), UD(MCOM,2),REGS(6)

      DO 8 I=1,6
        REGS(I)=0.
    8 CONTINUE

C Check range.
      IF(ISTART.GE.1.AND.IEND.LE.MTS)THEN
        DO 10 I=ISTART,IEND

C User-defined summation: held in UD(IROW,J) where
C IROW= row number and J=1 to 6 relates to
C registers 1 through 6 respectively.
          IRL=INT(UD(IROW,1))
          IRG=INT(UD(IROW,2))
          IF(DATA(I).LE.0..AND.IRL.NE.0)THEN
            REGS(IRL)=REGS(IRL)+DATA(I)
          ELSEIF (DATA(I).GT.0..AND.IRG.NE.0)THEN
            REGS(IRG)=REGS(IRG)+DATA(I)
          ENDIF
   10   CONTINUE
      ELSE
        call edisp(icout,' Range error in SUMUSR.')
      ENDIF

      RETURN
      END

C ******************** CHKTIME ********************
C CHKTIME returns the beginning iteration (ISTART)
C and the ending iteration (IEND) given a simulation day (IDAY).
C If a full day has been simulated then ISTART=1 and
C IEND=24*NTS.

      SUBROUTINE CHKTIME(IDAY,ISTART,IEND)

      COMMON/OUTPCH/ICOUT

      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/SNAP/SNAPSH
      logical SNAPSH
      character outs*124

C Check range. If in shapshot (load) mode the IOH1 & IOH2 are
C timesteps rather than integer hours so return directly.
      IF(IDAY.GE.IODS.AND.IDAY.LE.IODF)THEN
        N=24*NTS
        ISTART=1
        IEND=N
        ISFLG=0
        IEFLG=0
        if(SNAPSH)then
          ISTART=IOH1
          IEND=IOH2
          goto 99
        else
          TIOH1=float(IOH1)
          TIOH2=float(IOH2)
        endif

C Stepping at defined output interval.
        DO 40 J=1,N,NOUT
          JT=J

C Compute current time.
          call DATIME(JT,TIME)

C Within requested output period ?
          if(IDAY.GT.IODS.AND.IDAY.LT.IODF)then
            ISTART=1
            IEND=N
          endif
          if(IDAY.eq.IODS)then
            IF(TIME.GT.TIOH1.AND.ISFLG.EQ.0)THEN
              if(IOH1.eq.1)then
                ISTART=J-NTS
              else
                ISTART=J-1
              endif
              ISFLG=1
            ENDIF
          endif
          if(IDAY.eq.IODF)then
            IF(TIME.LE.TIOH2)THEN
              IEND=J
            ENDIF
          endif
   40   CONTINUE
      ELSE
        write(outs,'(a,i3,a,i3,a,i3)') ' CHKTIME day ',iday,
     &     ' is outwith ',iods,' and ',iodf
        call edisp(icout,outs)
      ENDIF
   99 continue

      RETURN
      END

C ******************** INTRPL ********************

C INTRPL is a generic routine to return a value V3 at a point
C on a line T3 when the line is defined by T1 V1 and T2 V2.
C I.e. return a real value at time T3 where T3 may any time.
c Used to interpolate data between timesteps.

      SUBROUTINE INTRPL(T1,V1,T2,V2,T3,V3)
      COMMON/OUTPCH/ICOUT
      logical close

      V3=0.

C Check range.
      call eclose(T1,T2,0.001,close)
      IF(.NOT.close)THEN

C Calculate the slope of the line.
        SLOPE=(V1-V2)/(T1-T2)

C Calculate the y intercept.
        C=V2-(SLOPE*T2)

C Calculate V3 via equation of the line (T1,V1) and (T2,V2).
        V3=SLOPE*T3+C
      ELSE
        call edisp(icout,' Range error in INTRPL.')
      ENDIF

      RETURN
      END

C ******************** HALFST ********************

C HALFST is a generic routine to take the current time-step ISTEP and
C NSTEP (last step in time period) and returns P (Present), F (Future),
C FLH (Future-Halfstep), FRH (Future+Halfstep), FN (Future Next ie N+1).
C To keep from running past the array limits IAP, IAF, IAFN are returned
C and are consistent with other timesteps returned.
C Used to replicate the timesteps found in bps, especially for
C interpolation of the results library.  Temporary assumption about first
C time-step until first time row can be added to results library.
C Example call:
C        CALL HALFST(ISTEP,NSTEP,P,IAP,FLH,F,IAF,FRH,FN,IAFN)
C        CALL INTRPL(P,VAL4(19,IAP),F,VAL4(19,IAF),FLH,TNODHF)
C        CALL INTRPL(F,VAL4(19,IAF),FN,VAL4(19,IAFN),FRH,TNODHN)
C where TNODHF is interpolated data at prior half step and TNODHN
C is interpolated data at future half step.

      SUBROUTINE HALFST(ISTEP,NSTEP,P,IAP,FLH,F,IAF,FRH,FN,IAFN)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

C Check range.
      IF(ISTEP.GE.1.AND.ISTEP.LE.NSTEP)THEN

C Calculate half time step.
        HALFT=1.0/(FLOAT(NTS)*2.0)
        IF(ISTEP.EQ.1)THEN
          F=2.
          FLH=0.5
          FRH=1.5
          IAP= ISTEP
          IAF= ISTEP+1
          IAFN= ISTEP+2
        ELSEIF(ISTEP.EQ.NSTEP)THEN
          F=FLOAT(NSTEP-1)
          FLH=F+HALFT
          FRH=F+1.0+HALFT
          IAP= ISTEP-2
          IAF= ISTEP-1
          IAFN= ISTEP
        ELSE
          F=FLOAT(ISTEP)
          FLH=F-HALFT
          FRH=F+HALFT
          IAP= ISTEP-1
          IAF= ISTEP
          IAFN= ISTEP+1
        ENDIF
        P=F-1.
        FN=F+1.
      ELSE
        call edisp(iuout,' Range error in HALFST.')
      ENDIF

      RETURN
      END

C ******************** FLTIAV ********************

C FLTIAV is a generic routine to take one day's timestep data (DIN(MTS))
C and filter it according to whether IAV is 0 or 1, returning DOUT(MTS).
C If IAV is 0 then DOUT is the same as DIN.  If IAV is 1 then averaged
C data is returned, each item in DOUT is for one NOUT time-step.
C NEL is the number of NOUT timesteps in DOUT (not used).

      SUBROUTINE FLTIAV(IDAY,DIN,DOUT,NEL)
#include "building.h"
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      DIMENSION DIN(MTS),DOUT(MTS)

C Stepping at defined output interval.
      CALL CHKTIME(IDAY,ISTART,IEND)
      TFACT=FLOAT(NOUT)/FLOAT(NTS)
      DO 30 J=ISTART,IEND,NOUT
        JT=J
        DOUT(JT)=0.0
        IF(IAV.EQ.0)THEN
          DOUT(JT)=DIN(JT)*TFACT
        ELSE
          K1=J
          K2=J+NOUT
          DO 50 L=K1,K2
            XMULT=1.
            IF(L.EQ.K1.OR.L.EQ.K2)XMULT=.5
            DOUT(JT)=DOUT(JT)+DIN(L)*XMULT
   50     CONTINUE
          DIV=FLOAT(K2-K1)
          DOUT(JT)=(DOUT(JT)/DIV)*TFACT
        ENDIF
   30 CONTINUE
      RETURN
      END

C ******************** HDDATE ********************
C HDDATE takes the output period as defined in and return
C a character string describing the simulation period.
C Given: IOM1, IOD1, IOH1 and IOM2, IOD2, IOH2

      SUBROUTINE HDDATE(DESCR)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/AVRAGE/IDAVER
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      CHARACTER*60 DESCR
      CHARACTER DS*7,DS1*10,DE*7,DE1*10
      CHARACTER T1H*5,T1D*5,T2H*5,T2D*5

C Compute realistic times by taking into account the
C half time-step shift if IDAVER=0.
C Generate view period string based on IBDOY,IEDOY,BTIM,ETIM
      CALL STDATE(IYEAR,IODS,DS,DS1)
      CALL STDATE(IYEAR,IODF,DE,DE1)
      CALL ESTIME(NTS,IDAVER,IOH1,T1H,T1D,TIMER1)
      CALL ESTIME(NTS,IDAVER,IOH2*NTS,T2H,T2D,TIMER2)

      IF(IFDAY.EQ.1.OR.IFDAY.EQ.0)THEN
        IF(IFTIME.EQ.0)THEN
          WRITE(descr,7)DS,T1H,IYEAR,DE,T2H,IYEAR
    7     FORMAT('Period: ',A,'@',A,'(',I4,') to ',A,'@',A,'(',I4,')')
        ELSE
          WRITE(descr,7)DS,T1D,IYEAR,DE,T2D,IYEAR
        ENDIF
      ELSEIF(IFDAY.EQ.2)THEN
        IF(IFTIME.EQ.0)THEN
          WRITE(descr,7)DS1,T1H,IYEAR,DE1,T2H,IYEAR
        ELSE
          WRITE(descr,7)DS1,T1D,IYEAR,DE1,T2D,IYEAR
        ENDIF
      ENDIF
      RETURN
      END

C ******************** HDSTEP ********************
C HDSTEP takes the timestep information and returns
C a character string describing the simulation period.

      SUBROUTINE HDSTEP(SDESCR)

      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      CHARACTER*44 SDESCR
      CHARACTER*10 AVRCH

C Text showing averages or not.
      AVRCH=' '
      IF(IAV.EQ.1)AVRCH='(averaged)'
      ILN=max(1,lnblnk(AVRCH))

C Generate a descriptive string.
      NTM=INT(60.0/FLOAT(NTS))
      NTOM=INT(60.0/(FLOAT(NTS)/FLOAT(NOUT)))
      WRITE(SDESCR,1)NTM,NTOM,AVRCH(1:ILN)
    1 FORMAT('Time steps: sim@',I2.2,'m, output@',I2.2,'m ',A)

      RETURN
      END

C ******************** STIME ********************
C STIME takes an integer timestep and returns a string DESCR
C which takes the form '12h28'.  STIME takes into account IDAVER
C and NTS in its calculations.

      SUBROUTINE STIME(ITIME,DESCR)
      COMMON/AVRAGE/IDAVER
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      CHARACTER*5 DESCR

C Calculate decimal time of day.
      call DATIME(ITIME,TIME)

C Split time into hours and minutes.
      MINH=INT(TIME)
      MIN=INT((TIME-AINT(TIME))*60.+0.5)
      WRITE(DESCR,'(i2.2,a1,i2.2)')INT(TIME),'h',MIN
      RETURN
      END

C ******************** SJTIME ********************
C SJTIME takes an integer timestep and returns a string DESCR
C which takes the form '12:28:30'. SJTIME takes into account IDAVER
C and NTS in its calculations.

      SUBROUTINE SJTIME(ITIME,DESCR)
      COMMON/AVRAGE/IDAVER
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      CHARACTER*8 DESCR

C Calculate decimal time of day.
      call DATIME(ITIME,TIME)

C Split time into hours and minutes.
      MINH=INT(TIME)
      MIN=INT((TIME-AINT(TIME))*60.+0.5)
      WRITE(DESCR,'(i2.2,a1,i2.2,a)')INT(TIME),':',MIN,':30'
      RETURN
      END

C ******************** DATIME ********************
C DATIME takes an integer timestep and returns a decimal hour of day.
C DATIME takes into account IDAVER and NTS in its calculations.

      SUBROUTINE DATIME(ITIME,TIME)
      COMMON/AVRAGE/IDAVER
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

C Calculate decimal time of day.
      if (IDAVER.EQ.0.and.IAV.eq.1) then
        TIME=(float(ITIME)+(float(NOUT-2)/2.0))/float(NTS)
      elseif (IDAVER.EQ.1.and.IAV.eq.1) then
        TIME=(float(ITIME)+(float(NOUT-1)/2.0))/float(NTS)
      elseif (IDAVER.EQ.0.and.IAV.eq.0) then
        TIME=(float(ITIME)-0.5)/float(NTS)
      elseif (IDAVER.EQ.1.and.IAV.eq.0) then
        TIME=float(ITIME)/float(NTS)
      endif

      RETURN
      END

C ******************** DASYSTIME ********************
C DASYSTIME takes an integer plant timestep and returns a decimal hour of day.
C It takes into account IDAVER and NPTS & NTS in its calculations.
C First, this is being called from within a timestep loop rather than
C from a day/hour/timestep loop. 
      SUBROUTINE DASYSTIME(ITIME,TIME)
      COMMON/AVRAGE/IDAVER
      COMMON/PPERS/IPSD1,IPSM1,IPSD2,IPSM2,IPSDS,IPSDF,NPTS,ipsav
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/C6/INDCFG

C Calculate decimal time of day for the case of plant only model.
      if(indcfg.eq.2.or.indcfg.eq.3)then

C So if itime is > the number of periods in a day reset.
        ioneday=NPTS*24
        if(itime.le.ioneday)then
          it=itime
        else
          it=MOD(itime,ioneday)
        endif
        if (IDAVER.EQ.0.and.IAV.eq.1) then
          TIME=(float(IT)+(float(NOUT-2)/2.0))/float(NPTS)
        elseif (IDAVER.EQ.1.and.IAV.eq.1) then
          TIME=(float(IT)+(float(NOUT-1)/2.0))/float(NPTS)
        elseif (IDAVER.EQ.0.and.IAV.eq.0) then
          TIME=(float(IT)-0.5)/float(NPTS)
        elseif (IDAVER.EQ.1.and.IAV.eq.0) then
          TIME=float(IT)/float(NPTS)
        endif
      else

C Not a domain combination that we can work with.
        TIME=0.0
      endif

      RETURN
      END

C ******************** ZNLIST ********************
C ZNLIST takes the current selected zones and builds a descriptive
C string to be used in headers. 

      SUBROUTINE ZNLIST(zdescr,length,ierr)
#include "building.h"
      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      CHARACTER zname*12,zdesc*64,zdescr*80,outs*244,outsd*80

      length=0
      do 42 i=1,NZ
        lna=lnblnk(zname(NZNO(i)))
        length=length+lna+1
  42  continue
      if(length.lt.72)then
        WRITE(outs,5,iostat=ios,err=1)(zname(NZNO(I)),I=1,NZ)
    5   FORMAT(' Zones: ',20(a12,' '))
        call sdelim(outs,outsd,'S',IW)
        zdescr=outsd
        return
      else
        if(NZ.eq.1)then
         WRITE(ZDESCR,'(A,I2,A,A)')' Zone (',NZNO(1),') ',zname(NZNO(1))
        elseif(NZ.gt.1.and.NZ.LE.20)THEN
         WRITE(ZDESCR,6)(NZNO(I),I=1,NZ)
    6    FORMAT(' Zones: ',20(I2,','))
        ELSEIF(NZ.GT.20.AND.NZ.LE.24)THEN
         WRITE(ZDESCR,7)(NZNO(I),I=1,9),(NZNO(J),J=10,NZ)
    7    FORMAT(' Zones: ',9(I1,','),15(I2,','))
        ELSEIF(NZ.GT.24.AND.NZ.LE.28)THEN
         WRITE(ZDESCR,8)(NZNO(I),I=1,9),(NZNO(J),J=10,NZ)
    8    FORMAT(' Zn: ',9(I1,','),20(I2,','))
        ELSEIF(NZ.GT.28)THEN
         WRITE(ZDESCR,8)(NZNO(I),I=1,9),(NZNO(J),J=10,28)
        ENDIF
        return
      endif

   1  write(6,*) 'ZLIST: error writing zone names: ',outs
      ierr=1
      return
      END

C ******************** COLZNLIST ********************
C COLZNLIST takes the current selected zones and draws text with
C a different colour for each name. 

      SUBROUTINE COLZNLIST(line,isize,ierr)
#include "building.h"
      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      CHARACTER zname*12,zdesc*64,word*24

C Figure out how much text there is to write out. isecond is the last
C zone index that fit within 72 characters.
      length=0
      isecond=0
      do 42 i=1,NZ
        lna=lnblnk(zname(NZNO(i)))
        length=length+lna+1
        if(length.lt.72)isecond=i
  42  continue

      if(length.lt.72)then

C Full name of zone can be written. Find position of "Zones"
C Set ix to last character position before looping through zones.
        word='Zones:'
        call findviewtext(2,line,isize,ix,iy)
        call winscl('-',0)
        call wstxpt(ix,iy,word)
        icx=9
        do 43 i=1,NZ

C Find x and y pixels for the current line and character postion.
C Then get index of colour for this zone, then plot text in that colour.
          call findviewtext(icx,line,isize,ix,iy)
          lna=lnblnk(zname(NZNO(i)))
          write(word,'(a)',iostat=ios,err=1) zname(NZNO(I))(1:lna)
          call textatxy(ix,iy,word,'z',NZNO(i))
          icx=icx+lna+1
  43    continue
        call winscl('-',0)
        call forceflush()
        return
      elseif(length.lt.140)then

C Try writing full name of zone on two lines. Find position of "Zones"
C Set ix to last character position before looping through zones.
        word='Zones:'
        call findviewtext(2,line,isize,ix,iy)
        call winscl('-',0)
        call wstxpt(ix,iy,word)
        icx=9
        iloop=MIN0(isecond,NZ)
        do 143 i=1,iloop

C Find x and y pixels for the current line and character postion.
C Then get index of colour for this zone, then plot text in that colour.
          call findviewtext(icx,line,isize,ix,iy)
          lna=lnblnk(zname(NZNO(i)))
          write(word,'(a)',iostat=ios,err=1) zname(NZNO(I))(1:lna)
          call textatxy(ix,iy,word,'z',NZNO(i))
          icx=icx+lna+1
 143    continue
        call forceflush()

C If there is a second line that can be created do it.
        inextloop=isecond+1
        if(inextloop.le.NZ)then
          icx=9
          do 144 i=inextloop,NZ

C Find x and y pixels for the current line and character postion.
C Then get index of colour for this zone, then plot text in that colour.
            call findviewtext(icx,line+1,isize,ix,iy)
            lna=lnblnk(zname(NZNO(i)))
            write(word,'(a)',iostat=ios,err=1) zname(NZNO(I))(1:lna)
            call textatxy(ix,iy,word,'z',NZNO(i))
            icx=icx+lna+1
 144      continue
        endif
        call winscl('-',0)
        call forceflush()
        return
      else
        if(NZ.eq.1)then
          write(word,'(A,I2,A)',iostat=ios,err=1)' Zone (',NZNO(1),') '
          call findviewtext(2,line,isize,ix,iy)
          call winscl('-',0)
          call wstxpt(ix,iy,word)
          icx=11
          call findviewtext(icx,line,isize,ix,iy)
          lna=lnblnk(zname(NZNO(1)))
          write(word,'(a)',iostat=ios,err=1) zname(NZNO(1))(1:lna)
          call textatxy(ix,iy,word,'z',NZNO(1))
          call winscl('-',0)
          call forceflush()
        elseif(NZ.gt.1.and.NZ.LE.20)THEN
          word='Zones:'
          call findviewtext(2,line,isize,ix,iy)
          call winscl('-',0)
          call wstxpt(ix,iy,word)
          icx=9
          do 44 i=1,NZ
            call findviewtext(icx,line,isize,ix,iy)
            lna=2
            write(word,'(i2)',iostat=ios,err=1) NZNO(I)
            call textatxy(ix,iy,word,'z',NZNO(i))
            icx=icx+lna+1
  44      continue
          call winscl('-',0)
          call forceflush()
          return
        ELSEIF(NZ.GT.20.AND.NZ.LE.24)THEN
          word='Zones:'
          call findviewtext(2,line,isize,ix,iy)
          call winscl('-',0)
          call wstxpt(ix,iy,word)
          icx=9
          do 45 i=1,NZ
            call findviewtext(icx,line,isize,ix,iy)
            if(i.ge.1.and.i.lt.10)then
              lna=1
              write(word,'(i1)',iostat=ios,err=1) NZNO(I)
            elseif(i.ge.10)then
              lna=2
              write(word,'(i2)',iostat=ios,err=1) NZNO(I)
            endif
            call textatxy(ix,iy,word,'z',NZNO(i))
            icx=icx+lna+1
  45      continue
          call winscl('-',0)
          call forceflush()
          return
        ELSEIF(NZ.GT.24.AND.NZ.LE.28)THEN
          word='Zn:'
          call findviewtext(2,line,isize,ix,iy)
          call winscl('-',0)
          call wstxpt(ix,iy,word)
          icx=5
          do 46 i=1,NZ
            call findviewtext(icx,line,isize,ix,iy)
            if(i.ge.1.and.i.lt.10)then
              lna=1
              write(word,'(i1)',iostat=ios,err=1) NZNO(I)
            elseif(i.ge.10)then
              lna=2
              write(word,'(i2)',iostat=ios,err=1) NZNO(I)
            endif
            call textatxy(ix,iy,word,'z',NZNO(i))
            icx=icx+lna+1
  46      continue
          call winscl('-',0)
          call forceflush()
          return
        ELSEIF(NZ.GT.28)THEN
          icx=2
          do 47 i=1,NZ
            call findviewtext(icx,line,isize,ix,iy)
            if(i.ge.1.and.i.lt.10)then
              lna=1
              write(word,'(i1)',iostat=ios,err=1) NZNO(I)
            elseif(i.ge.10)then
              lna=2
              write(word,'(i2)',iostat=ios,err=1) NZNO(I)
            endif
            call textatxy(ix,iy,word,'z',NZNO(i))
            icx=icx+lna+1
  47      continue
          call winscl('z',0)
          call forceflush()
          return
        ENDIF
        return
      endif

   1  write(6,*) 'ZLIST: error writing zone names: ',word
      ierr=1
      return
      END

C ******************** XTVAL1 ********************

C XTVAL1 is a generic routine to take an 1-D timestep array ARRAY(MTS) and
C copy into IPICK of the common block VAL1(IPICK,MTS). In addition
C the user must specify a range to duplicate i.e. ISTART to IEND.

      SUBROUTINE XTVAL1(ARRAY,ISTART,IEND,IPICK)
#include "building.h"
      COMMON/OUTPCH/ICOUT

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      DIMENSION ARRAY(MTS)

C Check range.
      IF(ISTART.GE.1.AND.IEND.LE.MTS)THEN
        DO 10 I=ISTART,IEND
          VAL1(IPICK,I)=ARRAY(I)
   10   CONTINUE
      ELSE
        call edisp(icout,' Range error in XTVAL1.')
      ENDIF

      RETURN
      END

C ******************** XTVAL2 ********************

C XTVAL2 is a generic routine to take an 1-D timestep array ARRAY(MTS) and
C copy into IPICK of the common block VAL2(IPICK,MTS). In addition
C the user must specify a range to duplicate i.e. ISTART to IEND.

      SUBROUTINE XTVAL2(ARRAY,ISTART,IEND,IPICK)
#include "building.h"
      COMMON/OUTPCH/ICOUT

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      DIMENSION ARRAY(MTS)

C Check range.
      IF(ISTART.GE.1.AND.IEND.LE.MTS)THEN
        DO 10 I=ISTART,IEND
          VAL2(IPICK,I)=ARRAY(I)
   10   CONTINUE
      ELSE
        call edisp(icout,' Range error in XTVAL2.')
      ENDIF

      RETURN
      END

C ******************** XTVAL3 ********************

C XTVAL3 is a generic routine to take an 1-D timestep array ARRAY(MTS) and
C copy into IPICK of the common block VAL3(IPICK,MTS). In addition
C the user must specify a range to duplicate i.e. ISTART to IEND.

      SUBROUTINE XTVAL3(ARRAY,ISTART,IEND,IPICK)
#include "building.h"
      COMMON/OUTPCH/ICOUT

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      DIMENSION ARRAY(MTS)

C Check range.
      IF(ISTART.GE.1.AND.IEND.LE.MTS)THEN
        DO 10 I=ISTART,IEND
          VAL3(IPICK,I)=ARRAY(I)
   10   CONTINUE
      ELSE
        call edisp(icout,' Range error in XTVAL3.')
      ENDIF

      RETURN
      END

C ******************** GVTXDUM ********************

C GVTXDUM takes data in GVAL and moves it to XDUM. In addition
C the user must specify a range to duplicate i.e. ISTART to IEND.

      SUBROUTINE GVTXDUM(ISTART,IEND)
#include "building.h"
      COMMON/OUTPCH/ICOUT

      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

C Check range.
      IF(ISTART.GE.1.AND.IEND.LE.MTS)THEN
        DO 10 I=ISTART,IEND
          XDUM(I)=GVAL(I)
   10   CONTINUE
      ELSE
        call edisp(icout,' Range error in GVTXDUM.')
      ENDIF

      RETURN
      END

C ******************** GVTXDM1 ********************

C GVTXDM1 takes data in GVAL and moves it to XDUM1. In addition
C the user must specify a range to duplicate i.e. ISTART to IEND.

      SUBROUTINE GVTXDM1(ISTART,IEND)
#include "building.h"
      COMMON/OUTPCH/ICOUT

      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

C Check range.
      IF(ISTART.GE.1.AND.IEND.LE.MTS)THEN
        DO 10 I=ISTART,IEND
          XDUM1(I)=GVAL(I)
   10   CONTINUE
      ELSE
        call edisp(icout,' Range error in GVTXDM1.')
      ENDIF

      RETURN
      END

C ******************** ASKZON ********************
C ASKZON: call to enquire which zone the user is interested in.

      SUBROUTINE ASKZON(IZONE)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/C1/NCOMP,NCON
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      DIMENSION IVALS(MCOM),zn(MCON)
      character H*72,zname*12,zdesc*64,zn*12

C If only one zone in problem then supply it.
      if(NZ.eq.1)then
        IZONE=NZNO(1)
        return
      else

C Otherwise copy common zname to local string array zn (epicks works better
C with local string). 
        do 42 i=1,NCOMP
          zn(i)=zname(i)
  42    continue
      endif

    7 H(1)='Pick one zone for inclusion in a graph or tabular'
      H(2)='listing from the list provided.'
      INPIC=1
      CALL EPICKS(INPIC,IVALS,' ',' Which zone : ',
     &  12,NCOMP,zn,' zone list',IER,2)
      if(INPIC.eq.0)then
        call edisp(iuout,' You must choose one of the zones! ')
        goto 7
      endif
      IZONE=IVALS(1)

      RETURN
      END

C ******************** ASKSUR ********************
C ASKSUR: call to enquire which single surface in zone IZONE the user is 
C interested in for surface balances etc.  

      SUBROUTINE ASKSUR(IZONE,ISFN)

      COMMON/OUTIN/IUOUT,IUIN

      CALL EASKSUR(IZONE,ISFN,'-','Select surface to be reported on.',
     &        ' ',IER)
      if(IER.ne.0)then
        call usrmsg(' Unable to select a surface... ',
     &              ' Assuming zone 1 surface 1!','W')
        IZONE=1
        ISFN=1
      endif

      RETURN
      END

C ******************** PICKSUR ********************
C PICKSUR: call to enquire which surfaces (one or many) in zone IZ
C the user is interested in for stats/ graphs/ listings etc.

      SUBROUTINE PICKSUR(IZ,NP,IVA,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/C1/NCOMP,NCON
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      DIMENSION IVA(MS),IVALS(MS)
      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15
      character H*72,ST(MS)*12
      CHARACTER zname*12,zdesc*64,outs*124

C Check for illegal zone number.
      IER=0
      IF(IZ.GT.NCOMP.OR.IZ.EQ.0)THEN
        CALL USRMSG(' ','Zone number out of range!','W')
        IER=1
        RETURN
      ENDIF

C Create list of surfaces in current zone (IZ) and reset call parameters.
      NP=0
      INPIC=NZSUR(IZ)
      DO 11 I=1,INPIC
        ST(I)=SNAME(IZ,I)
        IVA(I)=0
   11 CONTINUE

C Display menu of surfaces allow multiple selections.
      H(1)='Pick one, several or all surfaces for inclusion.'
      write(outs,'(3a)')'Which surfaces in ',zname(IZ),' to include:'
      CALL EPICKS(INPIC,IVALS,outs,' ',12,NZSUR(IZ),ST,zname(IZ),IER,1)

C Check for selections and copy to call parameters for passing back.
      if (INPIC.gt.0) then
        NP=INPIC
        do 12 I=1,NP
          IVA(I)=IVALS(I)
 12     continue
      endif

      RETURN
      END

C ******************** PICKANC ********************
C PICKANC: call to enquire which anchor point the user wishes data to 
C be recovered for.  Returns lists of zone and surface numbers for 
C anchor type 'SURF'.
C Anchor commons:
C NALOC = nb of anchors,
C ALOC = substitution attribute used with the the anchor (usually
C   passed from a command to a function which manipulates the anchor.),
C ALOCLBL = user's name for the anchor,
C ALOCTYP = type (SURF|CTRL|LOAD) for the anchor,
C IALOC = nb of links (surfaces, etc.) associated with the anchor,
C lstanchr = links for each anchor

      SUBROUTINE PICKANC(ATYPE,IZLST,ISLST,NSANC,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      common/anchorb/IALOC(20),lstanchr(20,60)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)

      DIMENSION IV(20),IZLST(MCON),ISLST(MCON)

      character ALOC*12,ALOCLBL*12,ALOCTYP*4,ATYPE*4
      character H*72,ST(20)*12,outs*124

      IER=0

C Create list of anchors matching requested type and reset call parameters.
      NAC=0
      NSANC=0
      INPIC=1
      do 11 I=1,NALOC
        if (ALOCTYP(I)(1:4).eq.ATYPE(1:4)) then
          ST(I)=ALOCLBL(I)
          NAC=NAC+1
          IZLST(I)=0
          ISLST(I)=0
          IV(I)=0
        endif
 11   continue

C Check that there are anchor points of the desired type.
      if (NAC.eq.0) then
        write(outs,'(a)')'No anchor points of the specified type exist.'
        call usrmsg(outs,' ','W')
        IER=1
        return
      endif

C Display menu of anchor points allow only one selection.
      H(1)='Pick one anchor point.'
      write(outs,'(a)')'Which anchor point to use?'
      CALL EPICKS(INPIC,IV,outs,' ',13,NAC,ST,'Anchor points',IER,1)

C Check for selections.
      if (INPIC.gt.0) then

C Copy anchor links to call parameters for passing back.
        do 801 I=1,IALOC(IV(1))
            IAC=lstanchr(IV(1),I)
            IZLST(I)=IC1(iac)
            ISLST(I)=IE1(iac)
 801    continue
        NSANC=IALOC(IV(1))
      endif

      RETURN
      END

C ******************** PIKMRTS ********************
C PIKMRTS: call to enquire which MRT sensors to display sensed MRT for.
C Returns zone and sensor number via IZNSEL and ISFSEL respectively.

      SUBROUTINE PIKMRTS(IZLST,ISLST,NSEL,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

      COMMON/C1/NCOMP,NCON
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      common/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)

      DIMENSION IV(60),IZLST(MCON),ISLST(MCON),ILZNO(MCON),ILSNO(MCON)

      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character H*72,ST(60)*22,outs*124,LMRT*72,CUBN*6,zname*12,zdesc*64

      IER=0

C Create list of MRT sensors: loop through zones and read any view 
C factor files remembering sensor names only.
C Remember the total number os sensors.
      NTSEN=0
      do 10 I=1,NCOMP
        IF(IVF(I).EQ.1)THEN
          IUF=IFIL+1
          LMRT=LVIEW(I)
          call ERMRT(0,IUOUT,IUF,LMRT,I,IER)
          if (NCUB.gt.0) then
            do 20 J=1,NCUB
              NTSEN=NTSEN+1
              write (ST(NTSEN),'(a,a,a)') zname(I),':',CUBN(J)
              ILZNO(NTSEN)=I
              ILSNO(NTSEN)=J
 20         continue
          endif
        endif
 10   continue

C Check that there are MRT sensors.
      if (NTSEN.eq.0) then
        call usrmsg('No MRT sensors exist.','  ','W')
        IER=1
        return
      endif

C Display menu of anchor points allow only one selection.
      H(1)='Pick one or many MRT sensors.'
      write(outs,'(a)')'Which MRT sensors to use?'
      IVLS=NTSEN
      CALL EPICKS(NTSEN,IV,outs,'  ',22,IVLS,ST,'MRT sensors',IER,1)

C Check for selections.
      if (NTSEN.gt.0) then

C Copy anchor links to call parameters for passing back.
        do 801 I=1,NTSEN
          IZLST(I)=ILZNO(IV(I))
          ISLST(I)=ILSNO(IV(I))
 801    continue
        NSEL=NTSEN
      endif

      RETURN
      END

C ******************** ASKICN ********************
C ASKICN: call to enquire which node in surface ISFN in zone IZONE
C the user is interested in.

      SUBROUTINE ASKICN(IZONE,ISFN,INN)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/C1/NCOMP,NCON
      COMMON/RECV3/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)

      character outs*124,H*72

      IF(IZONE.GE.1.OR.IZONE.LE.NCOMP)THEN
        IF(ISFN.GE.1.OR.ISFN.LE.NCONST(IZONE))THEN
          NN=NELTS(IZONE,ISFN)*2
          write(outs,51)NN,ISFN
   51     FORMAT(' There are ',I3,' nodes in surface ',I3,',')
          H(1)='Node numbering begins from the `outside` of the'
          H(2)='surface.'
          CALL EASKI(INN,outs,' Which node is to be considered ?',
     &      1,'F',NN,'F',1,'node number',IER,2)
        ELSE
          call usrmsg(' ',' Surface out of range in ASKICN','W')
        ENDIF
      ELSE
        call usrmsg(' ',' Zone out of range in ASKICN','W')
      ENDIF

      RETURN
      END

C ******************** ASKRTIM ********************
C ASKRTIM: call to enquire which month and day and time 
C (point in time for snapshot analysis). Returns IMO (month), IDO (day 
C of month), IJDAY (day of year for output), TIME (real representation),
C IT (timestep). IFDAY is a toggle provided in setres.f to control
C the display and input of periods - 0 = julian day, 1 or 2 = day of
C month.
      SUBROUTINE ASKRTIM(IFDAY,IMO,IDO,IJDAY,TIME,IT,IER)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      character H*72,HOLD*24
      DIMENSION ID(12)
      DATA ID/31,28,31,30,31,30,31,31,30,31,30,31/

      IER=0
      H(1)='The output time defines the day, month and decimal '
      H(2)='hour at which the output is requested. '
      H(3)='This must be no earlier than 0.00 hours or later '
      H(4)='than 24.00 hours. '
      H(5)=' '
      IF(IFDAY.EQ.0)THEN
        CALL EDAY(IDO,IMO,IJDAY)
        write(HOLD,'(I6,a)') IJDAY,'  7.0  '
        H(6)='Example:  6th March at 9h30 am is given as 64  9.5 '
      else
        write(HOLD,'(I6,I4,a)') IDO,IMO,'  7.0  '
        H(6)='Example:  6th March at 9h30 am is given as 6 3 9.5 '
      endif

  281 IF(IFDAY.EQ.0)THEN
        CALL EASKS(HOLD,' ',' Day-of-year & time: ',
     &     24,' 1  7.0 ','doy and time',IER,6)
        K=0
        CALL EGETWI(HOLD,K,IJDAY,1,365,'F','day of year',IER)
        CALL EGETWR(HOLD,K,TIME,0.0,24.0,'F','time',IER)
        if(IER.ne.0)goto 281
        CALL EDAYR(IJDAY,IDO,IMO)
      ELSE
 283    CALL EASKS(HOLD,' ',' Day-of-month, month & time: ',
     &     24,' 1  1  7.0 ','doy, month time',IER,6)
        K=0
        CALL EGETWI(HOLD,K,IDO,1,31,'F','day of month',IER)
        CALL EGETWI(HOLD,K,IMO,1,12,'F','month',IER)
        CALL EGETWR(HOLD,K,TIME,0.0,24.0,'F','time',IER)
        if(IER.ne.0)goto 283
        CALL EDAY(IDO,IMO,IJDAY)
      ENDIF


C Check range.
      IF(IDO.GT.ID(IMO))THEN
        call edisp(iuout,' Day past end of month... try again. ')
        goto 281
      ENDIF
      CALL EDAY(IDO,IMO,IJDAY)
      IF(IJDAY.LT.ISDS)then
        call edisp(iuout,' Day is before simulation begins... ')
        goto 281
      ENDIF
      IF(IJDAY.GT.ISDF)then
        call edisp(iuout,' Day is after simulation ends... ')
        goto 281
      ENDIF

C Convert time to time-step number.
      XX=TIME+(1.0/(FLOAT(NTS)*2.0))
      IT=INT(XX*float(NTS))
      IF(IT.EQ.0)IT=1

      RETURN
      END

C ******************** VALIAV ********************
C VALIAV: call to compute averages in VAL1() based on status of IAV.  
C Data from VAL1(J,ISTEP) returned as XVAL1(J).
C Given the range of VAL1 to process ie. VAL1(1,ISTEP) to VAL1(IMAX,ISTEP).

      SUBROUTINE VALIAV(IMAX,ISTEP,XVAL1)
#include "building.h"

      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      DIMENSION XVAL1(MZS)

      IF(IAV.EQ.0)THEN
        DO 90 M=1,IMAX
          XVAL1(M)=VAL1(M,ISTEP)
   90   CONTINUE
      ELSE

C Compute average over NOUT timesteps (use L vs N in test
C so that in multiple timesteps/hr the last steps of the
C last hour get counted).
        K1=ISTEP
        K2=ISTEP+NOUT
        DO 50 K=1,IMAX
          XVAL1(K)=0.
   50   CONTINUE

        N=24*NTS
        DO 60 L=K1,K2
          XMULT=1.
          IF(L.EQ.K1.OR.L.EQ.K2)XMULT=.5
          DO 70 M=1,IMAX
            IF(L.LE.N)THEN
              XVAL1(M)=XVAL1(M)+VAL1(M,L)*XMULT
            ELSE

C Should really get information here from next day.
              XVAL1(M)=XVAL1(M)+VAL1(M,N)*XMULT
            ENDIF
   70     CONTINUE
   60   CONTINUE

        DIV=FLOAT(K2-K1)
        DO 80 M=1,IMAX
          XVAL1(M)=XVAL1(M)/DIV
   80   CONTINUE
      ENDIF

      RETURN
      END

C ************* ASKPER 
C ASKPER: Provides interface to specification of a period of days. 
      SUBROUTINE ASKPER(IFDAY,IER)

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      common/pophelp/h(60)

      DIMENSION MTHNAM(12)

      CHARACTER H*72,HOLD*20,PDESCR*60,outs*124,MTHNAM*3

      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

C Write out simulation period.
      write (PDESCR,'(2(i3,a4,a))') ID1,MTHNAM(IM1),' to',
     &                                        ID2,MTHNAM(IM2),'.'
      write (outs,'(2a)')
     &       ' Current result set contains data for the period:',PDESCR
      call edisp(iuout,outs)

      IER=0
  281 IF(IFDAY.EQ.0)THEN
        H(1)='The output start time defines the day, month and '
        H(2)='hour at which the results recovery will commence.'
        H(3)='This hour must be within the simulation period. '
        H(4)='Hour 1 is defined as the period 0h00 to 1h00, and'
        H(5)='hour 2 is defined as the period 1h00 to 2h00 etc.'
        H(6)='The period must be within a calendar year'
        write(HOLD,'(I6,I4)') IODS,IOH1
        CALL EASKS(HOLD,' ',' Start day-of-year & time: ',
     &                            20,' 9  1 ','startdoy and time',IER,6)
        K=0
        CALL EGETWI(HOLD,K,IODS,1,365,'F','start day of year',IER)
        CALL EGETWI(HOLD,K,IOH1,1,24,'F','start hour',IER)
        if(IER.ne.0)goto 281
        CALL EDAYR(IODS,IOD1,IOM1)
        if (IOD1.eq.ID2.AND.IOH1.eq.24) then
          call usrmsg('The output start hour cannot be 24 on the',
     &            'last simulated day.','W')
          goto 281
        endif

        H(1)='The output finish time defines the day, month and '
        H(2)='hour at which the results recovery will terminate.'
        H(3)='This hour must be within the simulation period. '
        H(4)='Hour 1 is defined as the period 0h00 to 1h00, and'
        H(5)='hour 2 is defined as the period 1h00 to 2h00 etc.'
        H(6)='The period must be within a calendar year'
        write(HOLD,'(I6,I4)') IODF,IOH2
        CALL EASKS(HOLD,' ',' End day-of-year & time: ',
     &     20,' 15  24 ','end doy and time',IER,6)
        K=0
        CALL EGETWI(HOLD,K,IODF,1,365,'F','end day of year',IER)
        CALL EGETWI(HOLD,K,IOH2,1,24,'F','end hour',IER)
        if(IER.ne.0)goto 281
        CALL EDAYR(IODF,IOD2,IOM2)
        if (IOD2.eq.ID1.AND.IOH2.eq.1) then
          call usrmsg('The output finish hour cannot be 1 on the',
     &            'first simulated day.','W')
          goto 281
        endif
      ELSE
        CALL EDAYR(IODS,IOD1,IOM1)
        H(1)='The output start time defines the day, month and '
        H(2)='hour at which the results recovery will commence.'
        H(3)='This hour must be within the simulation period. '
        H(4)='Hour 1 is defined as the period 0h00 to 1h00, and'
        H(5)='hour 2 is defined as the period 1h00 to 2h00 etc.'
        H(6)='The period must be within a calendar year'
        write(HOLD,'(I6,I4,I4)') IOD1,IOM1,IOH1
        CALL EASKS(HOLD,' ',' Start day-of-month, month & time: ',
     &                   20,' 9  1 ','start dom, month and time',IER,6)
        K=0
        CALL EGETWI(HOLD,K,IOD1,1,31,'F','start day of month',IER)
        CALL EGETWI(HOLD,K,IOM1,1,12,'F','start month',IER)
        CALL EGETWI(HOLD,K,IOH1,1,24,'F','start hour',IER)
        if (IOD1.eq.ID2.AND.IOH1.eq.24) then
          call usrmsg('The output start hour cannot be 24 on the',
     &            'last simulated day.','W')
          goto 281
        endif
        if(IER.ne.0)goto 281

C Check range, then convert to IODS.
        CALL EDAYCH(IOD1,IOM1,IERR)
        IF(IERR.EQ.1)then
          call usrmsg(' ',' Day is outside of the month','W')
          goto 281
        ENDIF
        CALL EDAY(IOD1,IOM1,IODS)

        H(1)='The output finish time defines the day, month and '
        H(2)='hour at which the results recovery will terminate.'
        H(3)='This hour must be within the simulation period. '
        H(4)='Hour 1 is defined as the period 0h00 to 1h00, and'
        H(5)='hour 2 is defined as the period 1h00 to 2h00 etc.'
        H(6)='The period must be within a calendar year'
        CALL EDAYR(IODF,IOD2,IOM2)
  282   write(HOLD,'(I6,I4,I4)') IOD2,IOM2,IOH2
        CALL EASKS(HOLD,' ',' End day-of-month, month & time: ',
     &     20,' 9  1 ','end dom, month and time',IER,6)
        K=0
        CALL EGETWI(HOLD,K,IOD2,1,31,'F','end day of month',IER)
        CALL EGETWI(HOLD,K,IOM2,1,12,'F','end month',IER)
        CALL EGETWI(HOLD,K,IOH2,1,24,'F','end hour',IER)
        if (IOD2.eq.ID1.AND.IOH2.eq.1) then
          call usrmsg('The output finish hour cannot be 1 on the',
     &            'first simulated day.','W')
          goto 281
        endif
        if(IER.ne.0)goto 282

C Check range, then convert to IODF.
        CALL EDAYCH(IOD2,IOM2,IERR)
        IF(IERR.EQ.1)then
          call usrmsg(' ',' Day is outside of the month','W')
          goto 282
        ENDIF
        CALL EDAY(IOD2,IOM2,IODF)
      ENDIF

      RETURN
      END

C CTLEXP ***********
C CTLEXP: Control feedback to export file or text feedback area.
C Assign user-specified export file - checking if local or remote.
C tg is a character T, X, or G specifying text, tabular or graphic 
C   info being saved.
C xfile (width variable) is the initial name of the file to be exported
C msg (width variable) is prepended to dialog as export happens
C ixopen is a toggle reset each time ctlexp is called to signal open or close file.
C ixunit is the file unit number for the export file.
      subroutine ctlexp(xfile,ixopen,ixunit,ixpunit,tg,msg,IER)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/rcmd/LCMDFL
      common/rpath/path

C Simulator parameters.
C autook if .true. then assume that the user does not need to confirm
C   the file name.
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok

      character*(*) msg,xfile
      character H*72,path*72,outs*124,tg*1,tfile*72
      character ixpfile*72,ttext*42,LCMDFL*144
      character longtfile*144,longtfiledos*144
      LOGICAL     spfileok,perok,tstepok,saveok,autook,exitok,startupok
      logical concat,remote,unixok

C Initial name for export parameter file is xfile+par.
      if(xfile(1:1).ne.' ')then
        write(ixpfile,'(a,a)') xfile(1:lnblnk(xfile)),'.par'
      else
        ixpfile = 'data.grt.par'
      endif

C Each call is a toggle.
      ixopen=ixopen+1
      if(ixopen.GT.1)ixopen=0
      if(ixopen.eq.0)then
        write(outs,'(a,a)') 'closing export file: ',xfile
        call usrmsg(outs,' ','-')
        if(tg.eq.'X')then
          CALL ERPFREE(ixunit,ISTAT)
          CALL ERPFREE(ixpunit,ISTAT)
          write(outs,'(a,a)') 'closing export parameter file: ',ixpfile
          call usrmsg(outs,'reporting >> to display ','-')
        elseif(tg.eq.'T')then
          CALL ERPFREE(ixunit,ISTAT)
          CALL ERPFREE(ixpunit,ISTAT)
        elseif(tg.eq.'G')then
          call wwcsetend
          call wwcclose(xfile)
        endif
      elseif(ixopen.eq.1)then
        iw=0
        remote=.false.
        call isunix(unixok)
        if(unixok)then
          if(path(1:2).ne.'./'.and.path(1:2).ne.'  ')remote=.true.
        else
          if(path(1:2).ne.'  ')remote=.true.
          if(ichar(path(1:1)).eq.46.and.
     &       ichar(path(2:2)).eq.92)remote=.false.
        endif
        if(remote)then
          write(outs,'(A,A)') ' The current path is: ',path
          call edisp(iuout,outs)
          h(1)='If using path then the export file will be placed in'
          h(2)='the model folder. Otherwise nominate a folder and file'
          h(3)='name for the export file (i.e. /tmp/junk.exp or'
          h(4)='/home/ralph/reports/junk.exp) '
          if (autook) then
            IW=2
          else
            write(outs,'(A,A)') ' The model is in folder ',path
            CALL EASKAB(outs,' place export file:','in model folder',
     &        'user defined folder',IW,4)
          endif
        endif
        h(1)='This file is a text file which can be used in'
        h(2)='reports or for third party tools. '
        if (.NOT.autook) then
          call easks(xfile,' export file name: ',' ',72,
     &               ' ','export file name',IER,2)
        endif
        write(outs,'(3A)')' opened ',xfile(1:lnblnk(xfile)),
     &                    ' for export.'
        call usrmsg(outs,' ','-')
        if(iw.eq.1)then
          if(tg.eq.'X')then
            call efopseq(ixunit,xfile,4,IER)
            if(ier.ne.0)return
            write(ixpfile,'(a,a)') xfile(1:lnblnk(xfile)),'.par'
            call efopseq(ixpunit,ixpfile,4,IER)
            write(outs,'(3A)')' opened ',ixpfile(1:lnblnk(ixpfile)),
     &                        ' for export parameters.'
            call usrmsg(outs,' ','-')
          elseif(tg.eq.'T')then
            call efopseq(ixunit,xfile,4,IER)
            if(ier.ne.0)return
          elseif(tg.eq.'G')then

C Writing remotely, add the path to the given file name before
C passing request to c graphic calls. Check if Unix-based or DOS based.
            call isunix(unixok)
            tfile=xfile   ! << this needs to change if xfile not 72 char >>
            longtfile=' '
            if(unixok)then
              call addpath(tfile,longtfile,concat)
            else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
              call addpath(tfile,longtfile,concat)
              call cmdfiledos(longtfile,longtfiledos,ier)
              longtfile=' '
              longtfile=longtfiledos
            endif
            call wwcopen(longtfile)
            call wwcsetstart
          endif
        else
          if(tg.eq.'X')then
            CALL ERPFREE(ixunit,ISTAT)
            call FPOPEN(ixunit,ISTAT,1,3,xfile)
            if(ISTAT.lt.0)return
            write(ixpfile,'(a,a)') xfile(1:lnblnk(xfile)),'.par'
            CALL ERPFREE(ixpunit,ISTAT)
            call FPOPEN(ixpunit,ISTAT,1,3,ixpfile)
            write(outs,'(3A)')' opened ',ixpfile(1:lnblnk(ixpfile)),
     &                        ' for export parameters.'
            call usrmsg(outs,' ','-')
          elseif(tg.eq.'T')then
            CALL ERPFREE(ixunit,ISTAT)
            call FPOPEN(ixunit,ISTAT,1,3,xfile)
            if(ISTAT.lt.0)return
          elseif(tg.eq.'G')then
            call wwcopen(xfile)
            call wwcsetstart
          endif
        endif
        write(outs,'(1x,3A)') msg(:lnblnk(msg)),
     &    ' reporting >> to ',xfile(1:lnblnk(xfile))
        call usrmsg(outs,' ','-')

C If tabular info then create the headder to the xvgr parameter file.
        if(tg.eq.'X')then
          write(ixpunit,'(a)') '# ACE/gr parameter file'
          write(ixpunit,'(a)') '# '
          write(ixpunit,'(a)') 'page 5'
          write(ixpunit,'(a)') 'page inout 5'
          write(ixpunit,'(a)') 'link page off'
          write(ixpunit,'(a)') 'with g0'
          write(ixpunit,'(a)') 'g0 on'
          write(ixpunit,'(a)') 'g0 type xy'
          write(ixpunit,'(a)') 'g0 autoscale type AUTO'
          write(ixpunit,'(a)') '  default linestyle 1'
          write(ixpunit,'(a)') '  default linewidth 1'
          write(ixpunit,'(a)') '  default color 1'
          write(ixpunit,'(a)') '  default char size 1.0000'
          write(ixpunit,'(a)') '  default font 2'
          write(ixpunit,'(a)') '  default font source 0'
          write(ixpunit,'(a)') '  default symbol size 0.5000'
          write(ixpunit,'(a)') '  view xmin 0.15'
          write(ixpunit,'(a)') '  view xmax 0.85'
          write(ixpunit,'(a)') '  view ymin 0.15'
          write(ixpunit,'(a)') '  view ymax 0.85'

          h(1)='3rd party graphing usually has a main title...'
          ttext = 'Simulation Results'
          call easks(ttext,'Title for 3rd party graph: ',' ',42,
     &      'Simulation Results ','3rd party title',IER,1)
          write(ixpunit,'(a,a,a)') 'title "',
     &      ttext(1:lnblnk(ttext)),'"'
          write(ixpunit,'(a)') 'title font 4'
          write(ixpunit,'(a)') 'title size 1.25'

C Include the name of the results library as a subtitle.
          write(ixpunit,'(a,a,a)') 'subtitle "',
     &       LCMDFL(1:lnblnk(LCMDFL)),'"'
          write(ixpunit,'(a)') 'subtitle font 4'
          write(ixpunit,'(a)') 'subtitle size 1.00'
          
C Typical xaxis stuff.
          write(ixpunit,'(a)') '  xaxis  label "Day of year"'
          write(ixpunit,'(a)') '  xaxis  label place auto'
          write(ixpunit,'(a)') '  xaxis  label char size 1.0000'
          write(ixpunit,'(a)') '  xaxis  label font 4'
          write(ixpunit,'(a)') '  xaxis  label color 1'
          write(ixpunit,'(a)') '  xaxis  label linewidth 1'
          write(ixpunit,'(a)') '  xaxis  ticklabel on'
          write(ixpunit,'(a)') '  xaxis  ticklabel type auto'
          write(ixpunit,'(a)') '  xaxis  ticklabel layout horizontal'
          write(ixpunit,'(a)') '  xaxis  ticklabel  op bottom'
          write(ixpunit,'(a)') '  xaxis  ticklabel char size 0.75000'
          
C Typical yaxis stuff.
          write(ixpunit,'(a)') '  yaxis  tick on'
          write(ixpunit,'(a)') '  yaxis  label "Data"'
          write(ixpunit,'(a)') '  yaxis  label place auto'
          write(ixpunit,'(a)') '  yaxis  label char size 1.0000'
          write(ixpunit,'(a)') '  yaxis  label font 4'
          write(ixpunit,'(a)') '  yaxis  label color 1'
          write(ixpunit,'(a)') '  yaxis  label linewidth 1'
          write(ixpunit,'(a)') '  yaxis  ticklabel on'
          write(ixpunit,'(a)') '  yaxis  ticklabel type auto'
          write(ixpunit,'(a)') '  yaxis  ticklabel layout horizontal'
          write(ixpunit,'(a)') '  yaxis  ticklabel  op left'
          write(ixpunit,'(a)') '  yaxis  ticklabel char size 0.75000'
          write(ixpunit,'(a)') '  legend on'
          write(ixpunit,'(a)') '  legend vgap 2'
          write(ixpunit,'(a)') '  legend hgap 1'
          write(ixpunit,'(a)') '  legend x1 0.2'
          write(ixpunit,'(a)') '  legend y1 0.8'
          write(ixpunit,'(a)') '  legend length 4'
          write(ixpunit,'(a)') '  legend color 1 '
          write(ixpunit,'(a)') '  legend linestyle 1'
          write(ixpunit,'(a)') '  legend linewidth 1'
          write(ixpunit,'(a)') '  legend font 4'
          write(ixpunit,'(a)') '  legend char size 0.75000'
        endif
      endif
      RETURN
      END

C ******** axisnorm  **********
C axisnorm takes in a bin width, current data minimum and mxaimum
C and returns revised max and min rounded to slighting extended plotting
C extremes.  Basic version, improvements could be made to intervals.
      subroutine axisnorm(bin,xmin,xmax,bxmin,bxmax)
      logical close1,close2,close5,close10,close100,closeh
      logical close

      call eclose(BIN,0.5,0.01,closeh)
      call eclose(BIN,1.0,0.01,close1)
      call eclose(BIN,2.0,0.01,close2)
      call eclose(BIN,5.0,0.01,close5)
      call eclose(BIN,10.0,0.01,close10)
      call eclose(BIN,100.0,0.1,close100)
      if(close1.or.close2.or.close10.or.close100)then
        T=XMAX
        call eclose(aint(T),T,0.001,close)
        if(close)then
          BXMAX=T
        else
          BXMAX=aint(T+1.)
        endif
        T=XMIN
        if(T.ge.0.)BXMIN=aint(T)
        if(T.lt.0.)then
          call eclose(aint(T),T,0.001,close)
          if(close)then
            BXMIN=aint(T)
          else
            BXMIN=aint(T-1.)
          endif
        endif
      elseif(closeh)then
        T=XMAX
        call eclose(aint(T),T,0.001,close)
        if(close)then
          BXMAX=T
        else
          if(aint(T+0.5).lt.BXMAX)then
            BXMAX=aint(T+1.)
          else
            BXMAX=aint(T+0.5)
          endif
        endif
        T=XMIN
        if(T.ge.0.)then
          if(aint(T+0.5).lt.XMIN)then
            BXMIN=aint(T+0.5)
          else
            BXMIN=aint(T)
          endif
        elseif(T.lt.0.)then
          call eclose(aint(T),T,0.001,close)
          if(close)then
            BXMIN=T
          else
            if(aint(T-0.5).gt.XMIN)then
              BXMIN=aint(T-1.0)
            else
              BXMIN=aint(T-0.5)
            endif
          endif
        endif
      else
        write(6,*) 'xdiff exception'
        XDIFF=(XMAX-XMIN)/100.0
        BXMIN = XMIN-XDIFF
        BXMAX = XMAX+XDIFF
      endif
      return
      end
