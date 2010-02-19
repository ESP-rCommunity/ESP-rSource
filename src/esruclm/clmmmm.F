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

C 'CLMMMM' COMPUTES THE MAXIMUM, MINIMUM AND MEAN VALUES OF THE CHOSEN
C CLIMATIC PARAMETER:
C   IP=1 DIFFUSE HORIZ RADIATION
C      2 EXTERNAL AIR TEMEPRATURE
C      3 DIRECT NORMAL RADIATION
C      4 WIND SPEED
C      5 WIND DIRECTION
C      6 RELATIVE HUMIDITY
C  FOR THE SPECIFIED PERIOD.   THE TIME OF OCCURRENCE OF
C THE MAXIMUM AND MINIMUM VALUES IS ALSO GIVEN - IF THESE VALUES
C OCCUR ON MORE THAN ONE OCCASION THEN THE TIME OF THE FIRST OCCURRENCE
C IS GIVEN.   OTHER TIMES CAN THEN BE FOUND BY MODIFYING THE ANALYSIS
C PERIOD ACCORDINGLY.

      SUBROUTINE CLMMMM(IP,act)
      PARAMETER (MT=24)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTPCH/ICOUT
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/DAYSF/KDS,KDF
      common/exporttgi/ixopen,ixloc,ixunit

      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)
      COMMON/CLMVAR/VNAME(6)
 
      character DESCR*7,DESCR1*10,DESCRX*7,DESCR1X*10
      character outs*124,act*1,VNAME*32
      character NMTHNM(12)*3
      
      DATA NMTHNM/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     &'Aug','Sep','Oct','Nov','Dec'/

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file...',' ','-')
      endif
      CALL CHEAD(30)
      WRITE(outs,12)VNAME(IP)
   12 FORMAT(' ',A)
      call edisp(itru,outs)
      call edisp(itru,' ')
      if(act.eq.'-'.or.act.eq.'D'.or.act.eq.'d')then
       call edisp(itru,
     &' D  M   T    Minimum  Time           Maximum  Time         Mean')
      elseif(act.eq.'M'.or.act.eq.'m')then
       call edisp(itru,
     &' Month       Minimum  Time           Maximum  Time         Mean')
      endif

C INITIALISE PARAMETERS FOR WHOLE PERIOD.
      XMAXI=-1E+10
      XMINI=1E+10
      XMEANI=0
      HRTOT=0.

C INITIALISE PARAMETERS FOR MONTHLY TOTALS.
      MTH1=IM1
      XMAXM=-1E10
      XMINM=1E10
      XMEANM=0.
      HRM=0.

C Find day of week for start of the period.
      CALL EDAYR(IDS,IDAYN,IMTHN)
      
C DAY BY DAY.
      DO 10 I=IDS,IDF

C READ VALUES OF THE CLIMATIC PARAMETERS FOR THIS DAY.
        IDD=I
        CALL CLMGET(IDD)

C SET PARAMETERS FOR THIS DAY.
        XMAXJ=-1E+10
        XMINJ=1E+10
        XMEANJ=0
        JS=KDS
        JF=KDF
        IF(I.EQ.IDS)JS=MAX0(KDS,IT1)
        IF(I.EQ.IDF)JF=MIN0(KDF,IT2)
        DAYL=JF-JS+1

C AND ANALYSE IT.
        DO 20 J=JS,JF

C GET THE REQUIRED VALUE.
          if(IP.eq.1)then
            X=CDIF(J)
          elseif(IP.eq.2)then
            X=CTMP(J)
          elseif(IP.eq.3)then
            X=CDNR(J)
          elseif(IP.eq.4)then
            X=CVEL(J)
          elseif(IP.eq.5)then
            X=CDIR(J)
          elseif(IP.eq.6)then
            X=CHUM(J)
          endif

C MAX.
          IF(X.LE.XMAXJ)GO TO 2
          XMAXJ=X
          JTMX=J

C MIN.
   2      IF(X.GE.XMINJ)GO TO 4
          XMINJ=X
          JTMN=J

C MEAN.
   4      XMEANJ=XMEANJ+X
   20   CONTINUE

C CONVERT DAY TO D,M.
        II=I
        CALL EDAYR(II,IDD,IMM)
        CALL EWEEKD(IDD,IMM,IYEAR,IDWK)
        if(IMM.EQ.MTH1)GO TO 40


C Compute parameters for this week or month.
        XMEAN=XMEANM/HRM
        XMAX=XMAXM
        XMIN=XMINM

C CONVERT YEAR DAY NOS OF MAX & MIN TIMES TO
C DAY AND MONTH NUMBER FORMAT.
        CALL EDAYR(KDMN,KDN,KMN)
        call stdate(iyear,KDMN,DESCR,DESCR1)
        CALL EDAYR(KDMX,KDX,KMX)
        call stdate(iyear,KDMX,DESCRX,DESCR1X)

C If reporting days then include month. If reporting months
C print out same data. If reporting weeks give different format.
        if(act.eq.'-'.or.act.eq.'D'.or.act.eq.'d')then
          write(outs,'(1x,a3,8x,f8.1,a,i2,2a,F8.1,a,i2,2a,f8.1)')
     &    NMTHNM(KMN),XMIN,' @',KTMN,'h00 ',DESCR1(1:6),
     &    XMAX,' @',KTMX,'h00 ',DESCR1X(1:6),XMEAN
        elseif(act.eq.'M'.or.act.eq.'m')then
          write(outs,'(1x,a3,8x,f8.1,a,i2,2a,F8.1,a,i2,2a,f8.1)')
     &    NMTHNM(KMN),XMIN,' @',KTMN,'h00 ',DESCR1(1:6),
     &    XMAX,' @',KTMX,'h00 ',DESCR1X(1:6),XMEAN
        endif
        call edisp(itru,outs)

C UPDATE PARAMETERS FOR WHOLE PERIOD.
        IF(XMAXM.LE.XMAXI)GO TO 22
        XMAXI=XMAXM
        IDMX=KDMX
        ITMX=KTMX
  22    IF(XMINM.GE.XMINI)GO TO 24
        XMINI=XMINM
        IDMN=KDMN
        ITMN=KTMN
  24    XMEANI=XMEANI+XMEANM
        HRTOT=HRTOT+HRM

C REINITIALISE.
        XMAXM=-1E10
        XMINM=1E10
        XMEANM=0.
        HRM=0.
        if(act.eq.'-'.or.act.eq.'D'.or.act.eq.'d')then
          CALL CHEAD(30)
          WRITE(outs,12)VNAME(IP)
          call edisp(itru,outs)
          call edisp(itru,
     &' D  M   T    Minimum  Time        Maximum  Time            Mean')
        endif
        MTH1=IMM
  40    CONTINUE

C WRITE OUT EACH DAY.
        XMEAN=XMEANJ/DAYL
        WRITE(outs,100)IDD,IMM,JS,JF,XMINJ,JTMN,XMAXJ,JTMX,XMEAN
 100  FORMAT(1H ,I2,',',I2,1X,I2,'-',I2,F8.1,' @',I2,'h00   ',F12.1,' @'
     &,I2,'h00     ',F10.1)
        if(act.eq.'-'.or.act.eq.'D'.or.act.eq.'d')then
          call edisp(itru,outs)
        endif

C Update parameters for this month.
        IF(XMAXJ.LE.XMAXM)GO TO 42
        XMAXM=XMAXJ
        KDMX=I
        KTMX=JTMX
  42    IF(XMINJ.GE.XMINM)GO TO 44
        XMINM=XMINJ
        KDMN=I
        KTMN=JTMN
  44    XMEANM=XMEANM+XMEANJ
        HRM=HRM+DAYL

C AND GO TO NEXT DAY.
   10 CONTINUE

C NOW LAST MONTH
C COMPUTE PARAMETERS FOR THIS MONTH
      XMEAN=XMEANM/HRM
      XMAX=XMAXM
      XMIN=XMINM

C CONVERT YEAR DAY NOS OF MAX & MIN TIMES TO
C DAY AND MONTH NUMBER FORMAT.
      CALL EDAYR(KDMN,KDN,KMN)
      call stdate(iyear,KDMN,DESCR,DESCR1)
      CALL EDAYR(KDMX,KDX,KMX)
      call stdate(iyear,KDMX,DESCRX,DESCR1X)
      write(outs,'(1x,a3,8x,f8.1,a,i2,2a,F8.1,a,i2,2a,f8.1)')
     & NMTHNM(KMN),XMIN,' @',KTMN,'h00 ',DESCR1(1:6),
     & XMAX,' @',KTMX,'h00 ',DESCR1X(1:6),XMEAN
      call edisp(itru,outs)

C UPDATE PARAMETERS FOR WHOLE PERIOD.
      IF(XMAXM.LE.XMAXI)GO TO 62
      XMAXI=XMAXM
      IDMX=KDMX
      ITMX=KTMX
  62  IF(XMINM.GE.XMINI)GO TO 64
      XMINI=XMINM
      IDMN=KDMN
      ITMN=KTMN
  64  XMEANI=XMEANI+XMEANM
      HRTOT=HRTOT+HRM

C COMPUTE PARAMETERS FOR WHOLE PERIOD.
      XMEAN=XMEANI/HRTOT
      XMAX=XMAXI
      XMIN=XMINI

C CONVERT YEAR DAY NOS OF MAX & MIN TIMES TO
C DAY AND MONTH NUMBER FORMAT.
      CALL EDAYR(IDMN,KDMN,KMMN)
      call stdate(iyear,IDMN,DESCR,DESCR1)
      CALL EDAYR(IDMX,KDMX,KMMX)
      call stdate(iyear,IDMX,DESCRX,DESCR1X)
      write(outs,'(a,f8.1,a,i2,2a,F8.1,a,i2,2a,f8.1)')
     & ' All period ',XMIN,' @',ITMN,'h00 ',DESCR1,
     & XMAX,' @',ITMX,'h00 ',DESCR1X,XMEAN
      call edisp(itru,' ')
      call eddisp(itru,outs)
   
      RETURN
      END


C 'CLMMMSYNOP' COMPUTES the weekly MAXIMUM, MINIMUM and MEAN values
C for the chosen climatic parameter:
C   IP=1 DIFFUSE HORIZ RADIATION
C      2 EXTERNAL AIR TEMEPRATURE
C      3 DIRECT NORMAL RADIATION
C      4 WIND SPEED
C      5 WIND DIRECTION
C      6 RELATIVE HUMIDITY
C  FOR THE SPECIFIED PERIOD.   THE TIME OF OCCURRENCE OF
C THE MAXIMUM AND MINIMUM VALUES IS ALSO GIVEN - IF THESE VALUES
C OCCUR ON MORE THAN ONE OCCASION THEN THE TIME OF THE FIRST OCCURRENCE
C IS GIVEN.   OTHER TIMES CAN THEN BE FOUND BY MODIFYING THE ANALYSIS
C PERIOD ACCORDINGLY.

      SUBROUTINE CLMMMSYNOP(IP)
      PARAMETER (MT=24)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTPCH/ICOUT
      common/pophelp/h(60)
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/DAYSF/KDS,KDF
      common/exporttgi/ixopen,ixloc,ixunit

      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)
      COMMON/CLMVAR/VNAME(6)
 
      character DESCR*7,DESCR1*10,DESCRX*7,DESCR1X*10,h*72
      character DESCRW*7,DESCRW1*10,outs*124,VNAME*32
      logical ok,dok
      
C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file...',' ','-')
      endif
      CALL CHEAD(30)
      WRITE(outs,'(1x,a)')VNAME(IP)
      call edisp(itru,outs)
      call edisp(itru,' ')
      call edisp(itru,
     &' Week                Minimum  Time       Maximum  Time     Mean')


C INITIALISE PARAMETERS FOR WHOLE PERIOD.
      XMAXI=-1E+10
      XMINI=1E+10
      XMEANI=0
      HRTOT=0.

C INITIALISE PARAMETERS FOR MONTHLY TOTALS.
      XMAXM=-1E10
      XMINM=1E10
      XMEANM=0.
      HRM=0.

C Find day of week for start of the period. IWK is week number, IDSOW
C is the julian day at the start of the week.
      CALL EDAYR(IDS,IDAYN,IMTHN)
      CALL EWEEKD(IDAYN,IMTHN,IYEAR,IXDWK)
      IDSOW=IDS
      IWK=0
      ix=0

C DAY BY DAY.
      DO 10 I=IDS,IDF

C READ VALUES OF THE CLIMATIC PARAMETERS FOR THIS DAY.
        ix=ix+1
        IDD=I
        CALL CLMGET(IDD)

C SET PARAMETERS FOR THIS DAY.
        XMAXJ=-1E+10
        XMINJ=1E+10
        XMEANJ=0
        JS=KDS
        JF=KDF
        IF(I.EQ.IDS)JS=MAX0(KDS,IT1)
        IF(I.EQ.IDF)JF=MIN0(KDF,IT2)
        DAYL=JF-JS+1

C AND ANALYSE IT.
        DO 20 J=JS,JF

C GET THE REQUIRED VALUE.
          if(IP.eq.1)then
            X=CDIF(J)
          elseif(IP.eq.2)then
            X=CTMP(J)
          elseif(IP.eq.3)then
            X=CDNR(J)
          elseif(IP.eq.4)then
            X=CVEL(J)
          elseif(IP.eq.5)then
            X=CDIR(J)
          elseif(IP.eq.6)then
            X=CHUM(J)
          endif

C MAX.
          IF(X.LE.XMAXJ)GO TO 2
          XMAXJ=X
          JTMX=J

C MIN.
   2      IF(X.GE.XMINJ)GO TO 4
          XMINJ=X
          JTMN=J

C MEAN.
   4      XMEANJ=XMEANJ+X
   20   CONTINUE

C CONVERT DAY TO D,M.
        II=I
        CALL EDAYR(II,IDD,IMM)
        CALL EWEEKD(IDD,IMM,IYEAR,IDWK)

C PREPARE FOR OUTPUT, check if week complete.
        IF(II.eq.IDS)GO TO 40
        IF(IDWK.ne.IXDWK)GO TO 40
        iwk=iwk+1

C COMPUTE PARAMETERS FOR THIS MONTH.
        XMEAN=XMEANM/HRM
        XMAX=XMAXM
        XMIN=XMINM

C CONVERT YEAR DAY NOS OF MAX & MIN TIMES TO
C DAY AND MONTH NUMBER FORMAT.
        call stdate(iyear,IDSOW,DESCRW,DESCRW1)
        CALL EDAYR(KDMN,KDN,KMN)
        call stdate(iyear,KDMN,DESCR,DESCR1)
        CALL EDAYR(KDMX,KDX,KMX)
        call stdate(iyear,KDMX,DESCRX,DESCR1X)
        call stdate(iyear,IDSOW,DESCRW,DESCRW1)
        write(outs,'(2a,f8.1,a,i2,2a,F8.1,a,i2,2a,f8.1)')
     &    ' Wk of ',DESCRW1,XMIN,' @',KTMN,'h00 ',DESCR1(1:6),
     &    XMAX,' @',KTMX,'h00 ',DESCR1X(1:6),XMEAN
        call edisp(itru,outs)

C Pause if a long list.
        if((idf-IDS).gt.100.and.ix.gt.150)then
          dok=.true.
          h(1)='Pause point so you can study the listing. '
          call askok(' continue listing?',' ',OK,dok,1)
          ix=1
        endif

C Update whole period.
        IDSOW=II
        IF(XMAXM.LE.XMAXI)GO TO 22
        XMAXI=XMAXM
        IDMX=KDMX
        ITMX=KTMX
  22    IF(XMINM.GE.XMINI)GO TO 24
        XMINI=XMINM
        IDMN=KDMN
        ITMN=KTMN
  24    XMEANI=XMEANI+XMEANM
        HRTOT=HRTOT+HRM

C REINITIALISE.
        XMAXM=-1E10
        XMINM=1E10
        XMEANM=0.
        HRM=0.
  40    CONTINUE


C UPDATE PARAMETERS FOR THIS MONTH.
        IF(XMAXJ.LE.XMAXM)GO TO 42
        XMAXM=XMAXJ
        KDMX=I
        KTMX=JTMX
  42    IF(XMINJ.GE.XMINM)GO TO 44
        XMINM=XMINJ
        KDMN=I
        KTMN=JTMN
  44    XMEANM=XMEANM+XMEANJ
        HRM=HRM+DAYL

C AND GO TO NEXT DAY.
   10 CONTINUE

C Now last week.
C COMPUTE PARAMETERS FOR THIS MONTH
      XMEAN=XMEANM/HRM
      XMAX=XMAXM
      XMIN=XMINM
      iwk=iwk+1

C UPDATE PARAMETERS FOR WHOLE PERIOD.
      IF(XMAXM.LE.XMAXI)GO TO 62
      XMAXI=XMAXM
      IDMX=KDMX
      ITMX=KTMX
  62  IF(XMINM.GE.XMINI)GO TO 64
      XMINI=XMINM
      IDMN=KDMN
      ITMN=KTMN
  64  XMEANI=XMEANI+XMEANM
      HRTOT=HRTOT+HRM

C COMPUTE PARAMETERS FOR WHOLE PERIOD.
      XMEAN=XMEANI/HRTOT
      XMAX=XMAXI
      XMIN=XMINI

C CONVERT YEAR DAY NOS OF MAX & MIN TIMES TO
C DAY AND MONTH NUMBER FORMAT.
      CALL EDAYR(IDMX,KDMX,KMMX)
      call stdate(iyear,IDMN,DESCR,DESCR1)
      CALL EDAYR(IDMN,KDMN,KMMN)
      call stdate(iyear,IDMX,DESCRX,DESCR1X)
      write(outs,'(a,f8.1,a,i2,2a,F8.1,a,i2,2a,f8.1)')
     & ' All period ',XMIN,' @',ITMN,'h00 ',DESCR1,
     & XMAX,' @',ITMX,'h00 ',DESCR1X,XMEAN
      call edisp(itru,' ')
      call eddisp(itru,outs)
   
      RETURN
      END
