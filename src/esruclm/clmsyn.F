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

C 'CLMSYN' PERMITS A SYNOPTIC ANALYSIS OF THE CLIMATIC DATA HELD
C WITHIN THE CLIMATE FILE.
C AT PRESENT THE MAXIMUM, MINIMUM AND MEAN EXTERNAL AIR TEMPERATURE,
C DIRECT NORMAL SOLAR RADIATION AND DIFFUSE HORIZONTAL SOLAR RADIATION
C (OCCURRING WITHIN THE USER-SPECIFIED PERIOD) CAN BE IDENTIFIED.   IN
C ADDITION, THOSE DAYS (WITHIN THE PERIOD) THAT HAVE VALUES OF THE
C ABOVE CLIMATIC PARAMETERS WHOSE MAGNITUDE IS WITHIN SOME USER-SPECIFIED
C RANGE CAN BE LOCATED.   THIS ALLOWS SEQUENCES OF 'HOT' OR 'COLD' DAYS
C TO BE LOCATED.
C THE SYNOPTIC FACILITIES ARE INTENDED TO ALLOW THE SUBSEQUENT
C SIMULATION PERIOD TO BE APPROPRIATELY SET IN TERMS OF THE INHERENT
C CHARACTERISTICS OF THE CHOSEN CLIMATIC COLLECTION.

      SUBROUTINE CLMSYN
      COMMON/CLMVAR/VNAME(6)
      COMMON/RADTYP/IDNGH
      COMMON/OUTIN/IUOUT,IUIN

      CHARACTER*24 ITEM(23)
      character outs*124,VNAME*32
      logical ok,dok
      integer NITMS,INO  ! max items and current menu item

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CLMSYN'  ! set for cfiles

      call usrmsg('  ','  ','-')
      ITEM(1) ='p set period            '
      ITEM(2) ='  ____________________  '
      ITEM(3) ='0 set time of day       '
      ITEM(4) ='  ____________________  '
      ITEM(5) ='1 dry bulb temperature  '
      if(IDNGH.eq.0)then
       ITEM(6)='2 direct normal solar   '
      else
       ITEM(6)='2 global horizon. solar '
      endif
      ITEM(7) ='3 diffuse horiz. solar  '
      ITEM(8) ='4 wind speed (m/s)      '
      ITEM(9) ='5 wind direction (deg)  '
      ITEM(10)='6 relative humidity (%) '
      ITEM(11)='  ____________________  '
      ITEM(12)='m maximum & minimum     '
      ITEM(13)='r days within a range   '
      ITEM(14)='d degree days           '
      ITEM(15)='a average values        '
      ITEM(16)='i integrate radiation   '
      ITEM(17)='h frequency histogram   '
      ITEM(18)='  ____________________  '
      ITEM(19)='c climatic severity indx'
      ITEM(20)='s find typical weeks    '
      ITEM(21)='  ____________________  '
      ITEM(22)='? help                  '
      ITEM(23)='- exit                  '

      NITMS=23
      IP=0
    7 INO=-2
    6 CALL EMENU('  Synoptic analysis',ITEM,NITMS,INO)
      
      if(ino.eq.nitms)then
        return
      elseif(ino.eq.nitms-1)then
        helptopic='typical_week_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('climate synopsis',nbhelp,'-',0,0,IER)
      elseif(ino.eq.1)then
        CALL selper(ier)
      elseif(ino.eq.3)then
        CALL CLMDAY(IER)
      elseif(ino.eq.5)then
        IP=2
      elseif(ino.eq.6)then
        IP=3
      elseif(ino.eq.7)then
        IP=1
      elseif(ino.eq.8)then
        IP=4
      elseif(ino.eq.9)then
        IP=5
      elseif(ino.eq.10)then
        IP=6
      elseif(ino.eq.12)then

C MAXIMUM, MINIMUM AND MEAN VALUES.
        if(IP.EQ.0)then
          ino= -1
          goto 6
        endif
        helptopic='dd_day_week_month'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC(' Degree day reporting: ',' ','daily','weekly',
     &    'monthly',iddc,nbhelp)
        if(iddc.eq.1)then
          CALL CLMMMM(IP,'d')
        elseif(iddc.eq.2)then
          CALL CLMMMSYNOP(IP)
        elseif(iddc.eq.3)then
          CALL CLMMMM(IP,'m')
        endif
        IP=0
      elseif(ino.eq.13)then

C Call sequence routine.
        if(IP.EQ.0)then
          ino= -1
          goto 6
        endif

C Make sure that upper is above lower.
        WRITE(outs,'(1X,A)')VNAME(IP)
        helptopic='days_in_range_limits'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(XL,outs,' Lower value for range:  ',
     &    0.,'-',0.,'-',1.0,'lower range',IER,nbhelp)
        CALL EASKR(XU,outs,' Upper value for range:  ',
     &    XL,'F',0.,'-',1.0,'upper range',IER,nbhelp)
        CALL CLMSEQ(IP,XL,XU)
        IP=0
      elseif(ino.eq.14)then

C DEGREE DAYS.
        helptopic='dd_day_week_month'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABCD(' Degree day reporting: ',' ','daily','weekly',
     &    'monthly','seasonal',iddc,nbhelp)
        if(iddc.eq.1)then
          CALL CLMDEG
        elseif(iddc.eq.2)then
          CALL SYNOPWDEG
        elseif(iddc.eq.3)then
          call SYNOPDEG
        elseif(iddc.eq.4)then
          call SYNOPDEGS
        endif
      elseif(ino.eq.15)then

C AVERAGE DAILY WEATHER.
        CALL CLMAV(IP)
      elseif(ino.eq.16)then

C Integration of radiation data.
        CALL CLMINT
      elseif(ino.eq.17)then

C Frequency distribution histogram.
      IF(IP.GT.0)CALL HISCLM(IP)
      IF(IP.LE.0)call edisp(iuout,
     &  ' Climate variable has not been selected')
      elseif(ino.eq.19)then

C CSI equation calculation.
        CALL CSIEQN
      elseif(ino.eq.20)then

C Look for DD and Radiation typical week.
        dok=.true.
        helptopic='best_fit_pause'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call askok('Do you want to record the best-fit weeks (for',
     &    'use in climatelist typical assessment periods)?',
     &    ok,dok,nbhelp)
        if(ok)then
          call DDRADSUM('?')

C << But where are the best-fit weeks reported? >>

        else
          call DDRADSUM('-')
        endif
      else
        INO=-1
        GOTO 6
      endif
      goto 7

      END


C ******** `CSIEQN' calculates the Severity of the climate in
C  terms of a dimensionless number - the CSI.
      SUBROUTINE CSIEQN
      PARAMETER (MT=24)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/appw/iappw,iappx,iappy
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      common/exporttgi/ixopen,ixloc,ixunit
      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLMDT1/CLMLOC
      CHARACTER CLMLOC*30,outs*124

      DIMENSION CSIT(12),CSIRD(12),CSIRF(12),CSIW(12),CSI(12)
      DIMENSION AA(30),BB(30),CC(30),DD(30),EA(30),EB(30),EC(30),ED(30)
      DIMENSION CSIM(30),IVC(30)
      character CSIM*45

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero

      DATA AA/-0.356,-0.372,-0.409,-0.402,-0.359,-0.409,-0.436,-0.405,
     &-0.350,-0.417,-0.421,-0.375,-0.401,-0.403,-0.333,-0.400,-0.375,
     &-0.400,-0.408,-0.386,-0.435,-0.392,-0.422,-0.370,-0.426,-0.346,
     &-0.157,-0.385,-0.383,-0.355/

      DATA BB/-0.007,-0.003,-0.005,-0.003,-0.004,-0.002,-0.001,-0.001,
     &-0.011,-0.005,-0.005,-0.003,-0.003,-0.003,-0.008,-0.004,-0.005,
     &-0.001,-0.001,-0.006,-0.0003,-0.002,-0.005,-0.003,-0.002,-0.005,
     &-0.038,-0.006,-0.007,-0.009/

      DATA CC/-0.008,-0.008,-0.002,-0.007,-0.007,-0.002,-0.002,-0.004,
     &-0.002,-0.001,-0.0003,-0.004,-0.001,-0.008,-0.008,-0.0003,-0.004,
     &-0.006,-0.002,-0.001,-0.0003,-0.001,-0.0003,-0.002,-0.005,-0.015,
     &-0.001,-0.003,-0.005,-0.004/

      DATA DD/0.127,0.185,0.136,0.129,0.210,0.201,0.143,0.198,0.137,
     &0.125,0.116,0.225,0.202,0.200,0.166,0.183,0.172,0.173,0.206,0.171
     &,0.189,0.233,0.117,0.257,0.129,0.127,0.043,0.154,0.121,0.141/

      DATA EA/6.118,6.391,7.022,6.908,6.167,7.026,7.500,6.966,6.018,
     &7.159,7.238,6.441,6.894,6.928,5.720,6.870,6.445,6.873,7.108,
     &6.631,7.469,6.744,7.254,6.359,7.322,5.954,2.703,6.609,6.576,
     &6.100/

      DATA EB/1.281,0.521,0.799,0.513,0.605,0.269,0.258,0.127,1.952,
     &0.910,0.939,0.518,0.529,0.557,1.359,0.775,0.938,0.249,0.243,1.054
     &,0.031,0.423,0.912,0.545,0.272,0.804,6.511,1.063,1.131,1.607/

      DATA EC/0.860,0.808,0.243,0.733,0.750,0.199,0.192,0.441,0.196,
     &0.070,0.031,0.392,0.072,0.018,0.888,0.016,0.436,0.644,0.190,
     &0.114,0.044,0.072,0.037,0.172,0.506,1.541,0.145,0.281,0.550,
     &0.424/

      DATA ED/-0.234,-0.339,-0.250,-0.236,-0.386,-0.368,-0.262,
     &-0.363,-0.251,-0.230,-0.213,-0.413,-0.370,-0.367,-0.304,
     &-0.366,-0.315,-0.318,-0.377,-0.313,-0.347,-0.427,-0.214,
     &-0.471,-0.236,-0.233,-0.079,-0.282,-0.221,-0.258/

      helpinsub='CSIEQN'  ! set for cfiles

C If in graphic mode make the text display area larger before
C displaying the following data.
      IF(MMOD.EQ.8)THEN
        if(iappw.gt.0.and.iappw.lt.100)then
          menuchw = MAX0(int(28*iappw*0.01),12)
          LIMTTY= MAX0(int(28*iappw*0.01),12)
          LIMIT = MAX0(int(28*iappw*0.01),12)
        else
          menuchw = 28
          LIMTTY=28
          LIMIT =28
        endif
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          CALL feedbox(menuchw,2,igfw,igfh)
          CALL opengdisp(menuchw,LIMTTY,2,igdw,igdh)
        endif
        CALL win3d(menuchw,10,20,5,3,igl,igr,igt,igb,igw,igwh)
        igw=igr-igl
        igwh=igb-igt
        call startbuffer()
        call usrmsg(' ',' ','-')
      ENDIF

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file...',' ','-')
      endif

C Initial values.
      ADDT=0.
      ADDDR=0.
      ADDFR=0.
      ADDW=0.
      NHRS=0

   75 helptopic='climate_severity_types'
      call gethelptext(helpinsub,helptopic,nbhelp)
      INPIC=1
      CSIM(1)= 'House|Capacity|Capacity|Window|Infil- |Insul.'
      CSIM(2)= 'index|        |position|size  |tration|level '
      CSIM(3)= '  1   low      middle   std    tight   std   '
      CSIM(4)= '  2   high     outside  std    tight   high  '
      CSIM(5)= '  3   medium   inside   std    tight   std   '
      CSIM(6)= '  4   high     middle   large  tight   std   '
      CSIM(7)= '  5   low      outside  std    std     high  '
      CSIM(8)= '  6   medium   outside  large  std     high  '
      CSIM(9)= '  7   high     inside   std    tight   std   '
      CSIM(10)='  8   medium   middle   large  std     high  '
      CSIM(11)='  9   medium   inside   large  std     std   '
      CSIM(12)=' 10   low      outside  large  tight   high  '
      CSIM(13)=' 11   medium   outside  large  tight   std   '
      CSIM(14)=' 12   high     outside  large  std     high  '
      CSIM(15)=' 13   high     outside  std    std     std   '
      CSIM(16)=' 14   medium   outside  std    tight   high  '
      CSIM(17)=' 15   low      inside   std    std     std   '
      CSIM(18)=' 16   medium   middle   std    std     std   '
      CSIM(19)=' 17   high     middle   std    tight   high  '
      CSIM(20)=' 18   low      inside   std    tight   high  '
      CSIM(21)=' 19   medium   outside  std    std     std   '
      CSIM(22)=' 20   high     inside   large  tight   std   '
      CSIM(23)=' 21   medium   middle   std    std     high  '
      CSIM(24)=' 22   medium   inside   std    std     high  '
      CSIM(25)=' 23   medium   middle   large  tight   std   '
      CSIM(26)=' 24   low      middle   std    std     high  '
      CSIM(27)=' 25   high     outside  large  std     std   '
      CSIM(28)=' 26   high     inside   large  tight   high  '
      CSIM(29)=' 27   low      inside   large  tight   std   '
      CALL EPICKS(INPIC,IVC,' ',' Presentation format: ',
     &  45,29,CSIM,' CSI house types',IER,nbhelp)
      IF(INPIC.eq.0)return
      IF(IVC(1).lt.3)goto 75
      ihouse=IVC(1)-2

      J=0
      YADDT=0.
      YADDDR=0.
      YADDFR=0.
      YADDW=0.
      I=0
   10 I=I+1
      II=I
      IF(I.LE.31)JJ=1
      IF(I.GT.31.AND.I.LE.59)JJ=2
      IF(I.GT.59.AND.I.LE.90)JJ=3
      IF(I.GT.90.AND.I.LE.120)JJ=4
      IF(I.GT.120.AND.I.LE.151)JJ=5
      IF(I.GT.151.AND.I.LE.181)JJ=6
      IF(I.GT.181.AND.I.LE.212)JJ=7
      IF(I.GT.212.AND.I.LE.243)JJ=8
      IF(I.GT.243.AND.I.LE.273)JJ=9
      IF(I.GT.273.AND.I.LE.304)JJ=10
      IF(I.GT.304.AND.I.LE.334)JJ=11
      IF(I.GT.334)JJ=12
      IF(I.EQ.366)JJ=13
      IF(JJ.NE.J)goto 1
      goto 2
    1 IF(I.EQ.1)goto 3

C     IF(J.GT.4.AND.J.LT.9)goto 3
      YADDT=YADDT+ADDT
      YADDDR=YADDDR+ADDDR
      YADDFR=YADDFR+ADDFR
      YADDW=YADDW+ADDW
      T=ADDT/NHRS
      RD=ADDDR/NHRS
      RF=ADDFR/NHRS
      W=ADDW/NHRS
      CSIT(J)=AA(IHOUSE)*T+EA(IHOUSE)
      CSIRD(J)=BB(IHOUSE)*RD+EB(IHOUSE)
      CSIRF(J)=CC(IHOUSE)*RF+EC(IHOUSE)
      CSIW(J)=DD(IHOUSE)*W+ED(IHOUSE)
      CSI(J)=CSIT(J)+CSIRD(J)+CSIRF(J)+CSIW(J)
      IF(JJ.EQ.13)goto 11
    3 ADDT=0.
      ADDDR=0.
      ADDFR=0.
      ADDW=0.
      NHRS=0
    2 J=JJ
      CALL CLMGET(II)
      DO 20 K=1,24
      ADDT=ADDT+CTMP(K)
      ADDDR=ADDDR+CDNR(K)
      ADDFR=ADDFR+CDIF(K)
      ADDW=ADDW+CVEL(K)
      NHRS=NHRS+1
   20 CONTINUE
      goto 10

   11 call edisp(itru,' Climatic Severity Index for :')
      WRITE(outs,4)CLMLOC,CLAT,CLONG,IYEAR
    4 FORMAT(1X,A30,F7.2,'N',F7.2,'W',' :',I5)
      call eddisp(itru,outs)
      WRITE(outs,'(A,A)')' House Type  ',CSIM(IHOUSE+2)
      call edisp(itru,' ')
      call edisp(itru,
     &  'House|Capacity|Capacity |Window|Air      |Insul ')
      call edisp(itru,
     &  'index|        |position |size  |tightness|      ')
      call edisp(itru,outs)
      call edisp(itru,' ')
      WRITE(outs,42)
   42 FORMAT(' Month',5X,'CSIT',5X,'CSIRD',5X,'CSIRF',6X,
     &       'CSIW',6X,'CSI')
      call eddisp(itru,outs)
     
      T=0.0
      RD=0.0
      RF=0.0
      W=0.0
      C=0.0
      DO 30 I=1,12

C     IF(I.GT.4.AND.I.LT.9)goto 30
      WRITE(outs,5)I,CSIT(I),CSIRD(I),CSIRF(I),CSIW(I),CSI(I)
    5 FORMAT(I5,5(5X,F5.1))
      call eddisp(itru,outs)
      T=T+CSIT(I)
      RD=RD+CSIRD(I)
      RF=RF+CSIRF(I)
      W=W+CSIW(I)
      C=C+CSI(I)
   30 CONTINUE
      TM=T/12.0
      RDM=RD/12.0
      RFM=RF/12.0
      WM=W/12.0
      CM=C/12.0
      call edisp(itru,' ')
      call edisp(itru,' Annual')
      call edisp(itru,' ')
      WRITE(outs,6)T,RD,RF,W,C
    6 FORMAT(' Total',3X,F6.1,4(4X,F6.1))
      call eddisp(itru,outs)
      call edisp(itru,' ')
      WRITE(outs,61)TM,RDM,RFM,WM,CM
   61 FORMAT('  Mean',5(5X,F5.1))
      call eddisp(itru,outs)

      RETURN
      END


C  CLMAV COMPUTES THE DAILY PROFILES(HOUR BY HOUR)OVER THE
C  SPECIFIED PERIOD OF THE MIN,MAX MEAN,ST.DEV. ETC. FOR THE CHOSEN
C CLIMATIC PARAMETER:
C   IP=1 DIFFUSE HORIZ RADIATION
C      2 EXTERNAL AIR TEMEPRATURE
C      3 DIRECT NORMAL RADIATION
C      4 WIND SPEED
C      5 WIND DIRECTION
C      6 RELATIVE HUMIDITY

      SUBROUTINE CLMAV(IP)
      PARAMETER (MT=24)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTPCH/ICOUT
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/DAYSF/KDS,KDF
      common/exporttgi/ixopen,ixloc,ixunit
      COMMON/CLMVAR/VNAME(6)

      DIMENSION XJ(MT),XLO(MT),XHI(MT)
      DIMENSION XMAXI(MT),XMINI(MT),XMEANI(MT),X2I(MT)
      DIMENSION XMAXM(MT),XMINM(MT),XMEANM(MT),X2M(MT)
      character outs*124,VNAME*32

C INITIALISE PARAMETERS FOR WHOLE PERIOD
      BIG=1E10
      TMNMN=BIG
      TMXMN=-BIG
      TAVMN=0.
      TMNMX=BIG
      TMXMX=-BIG
      TAVMX=0.
      TMNAV=BIG
      TMXAV=-BIG
      TAVAV=0.
      THR=0.
      THIM=-BIG
      TLOM=BIG
      DO 14 J=1,24
        XMAXI(J)=-1E+10
        XMINI(J)=1E+10
        XMEANI(J)=0
        X2I(J)=0.

C INITIALISE PARAMETERS FOR MONTHLY TOTALS
        XMAXM(J)=-BIG
        XMINM(J)=BIG
        XMEANM(J)=0.
        X2M(J)=0.
  14  CONTINUE
      DMNMN=BIG
      DMXMN=-BIG
      DAVMN=0.
      DMNMX=BIG
      DMXMX=-BIG
      DAVMX=0.
      DMNAV=BIG
      DMXAV=-BIG
      DAVAV=0.
      DHIM=-BIG
      DLOM=BIG
      HRTOT=0.
      HRM=0.
      MTH1=IM1

C DAY BY DAY.
      DO 10 I=IDS,IDF

C READ VALUES OF THE CLIMATIC PARAMETERS FOR THIS DAY.
      IDD=I
      CALL CLMGT1(IDD,IP,XJ)

C SET PARAMETERS FOR THIS DAY.
      JS=KDS
      JF=KDF
      IF(I.EQ.IDS)JS=MAX0(KDS,IT1)
      IF(I.EQ.IDF)JF=MIN0(KDF,IT2)
      DHR=JF-JS+1

C GET MONTH OF THIS DAY.
      II=I
      CALL EDAYR(II,IDD,IMM)
      IF(IMM.NE.MTH1)GO TO 31

  40  CONTINUE

C UPDATE PARAMETERS FOR THIS MONTH.
      DMEAN=0.
      DO 45 J=JS,JF
      X=XJ(J)
      IF(X.LE.XMAXM(J))GO TO 42
      XMAXM(J)=X
  42  IF(X.GE.XMINM(J))GO TO 44
      XMINM(J)=X
  44  XMEANM(J)=XMEANM(J)+X
      X2M(J)=X2M(J)+X*X
      DMEAN=DMEAN+X
  45  CONTINUE
      HRM=HRM+1.0

C DAILY MEAN.
      DMEAN=DMEAN/DHR

C Remember `low' and `high' days.
      IF(DMEAN.LT.DHIM)GO TO 51
      NHIM=II
      DHIM=DMEAN
  51  IF(DMEAN.GT.DLOM)GO TO 52
      NLOM=II
      DLOM=DMEAN
  52  CONTINUE

C AND GO TO NEXT DAY.
      IF(I.NE.IDF)GO TO 10

  31  CONTINUE

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file...',' ','-')
      endif
      CALL CHEAD(30)
      WRITE(outs,100)MTH1,VNAME(IP)
 100  FORMAT(' Month',I3,8X,': ',A)
      call edisp(itru,outs)
      WRITE(outs,16)
  16  FORMAT('   Hr',5X,'Min',5X,'Max',7X,'Mean',3X,'Dev.n',
     &       6X,'Loday',3X,'Hiday')
      call edisp(itru,outs)

C  GET REFERENCED LO & HI DAYS.
      CALL CLMGT1(NHIM,IP,XHI)
      CALL CLMGT1(NLOM,IP,XLO)

      DO 30 J=JS,JF
      XMEAN=XMEANM(J)/HRM
      XDEV=SQRT(X2M(J)/HRM-XMEAN*XMEAN)
      XMAX=XMAXM(J)
      XMIN=XMINM(J)
      WRITE(outs,101)J,XMIN,XMAX,XMEAN,XDEV,XLO(J),XHI(J)
 101  FORMAT(1X,I4,2F8.1,3X,2F8.1,3X,2F8.1)
      call eddisp(itru,outs)
      DMNMN=AMIN1(DMNMN,XMIN)
      DMXMN=AMAX1(DMXMN,XMIN)
      DAVMN=DAVMN+XMIN
      DMNMX=AMIN1(DMNMX,XMAX)
      DMXMX=AMAX1(DMXMX,XMAX)
      DAVMX=DAVMX+XMAX
      DMNAV=AMIN1(DMNAV,XMEAN)
      DMXAV=AMAX1(DMXAV,XMEAN)
      DAVAV=DAVAV+XMEAN

C UPDATE PARAMETERS FOR WHOLE PERIOD
      IF(XMAXM(J).LE.XMAXI(J))GO TO 22
      XMAXI(J)=XMAXM(J)
  22  IF(XMINM(J).GE.XMINI(J))GO TO 24
      XMINI(J)=XMINM(J)
  24  XMEANI(J)=XMEANI(J)+XMEANM(J)
      X2I(J)=X2I(J)+X2M(J)

C REINITIALISE
      XMAXM(J)=-BIG
      XMINM(J)=BIG
      XMEANM(J)=0.
      X2M(J)=0.
   30 CONTINUE

C SAVE PERIOD LO & HI DAYS.
      IF(DHIM.LT.THIM)GO TO 61
      NHIT=NHIM
      THIM=DHIM
  61  IF(DLOM.GT.TLOM)GO TO 62
      NLOT=NLOM
      TLOM=DLOM
  62  CONTINUE

C WHOLE PERIOD.
      TMNMN=AMIN1(TMNMN,DMNMN)
      TMXMN=AMAX1(TMXMN,DMXMN)
      TAVMN=TAVMN+DAVMN
      TMNMX=AMIN1(TMNMX,DMNMX)
      TMXMX=AMAX1(TMXMX,DMXMX)
      TAVMX=TAVMX+DAVMX
      TMNAV=AMIN1(TMNAV,DMNAV)
      TMXAV=AMAX1(TMXAV,DMXAV)
      TAVAV=TAVAV+DAVAV
      THR=THR+DHR

C PRINT TOTALS FOR THIS MONTH.
      XSF=1.0/DHR
      DAVMN=XSF*DAVMN
      DAVMX=XSF*DAVMX
      DAVAV=XSF*DAVAV
      WRITE(outs,104)DMNMN,DMNMX,DMNAV
 104  FORMAT(' Min ',2F8.1,3X,F8.1)
      call eddisp(itru,outs)
      WRITE(outs,105)DMXMN,DMXMX,DMXAV
 105  FORMAT(' Max ',2F8.1,3X,F8.1)
      call eddisp(itru,outs)
      WRITE(outs,106)DAVMN,DAVMX,DAVAV,DLOM,DHIM
 106  FORMAT(' Mean',2F8.1,3X,F8.1,11X,2F8.1)
      call eddisp(itru,outs)

C INITIALISE MONTHLY
      DMNMN=1E10
      DMXMN=-1E10
      DAVMN=0.
      DMNMX=1E10
      DMXMX=-1E10
      DAVMX=0.
      DMNAV=1E10
      DMXAV=-1E10
      DAVAV=0.
      DLOM=BIG
      DHIM=-BIG

      HRTOT=HRTOT+HRM
      HRM=0.
      MTH1=IMM
      IF(I.NE.IDF)GO TO 40
   10 CONTINUE

C NOW COMPUTE PARAMETERS FOR WHOLE PERIOD
      IF(IMM.EQ.IM1)GO TO 19
      CALL CHEAD(30)
      WRITE(outs,'(2A)')' All period      :',VNAME(IP)
      call edisp(itru,outs)
      WRITE(itru,16)
      call edisp(itru,outs)
C GET REFERENCED LO & HI DAYS FOR PERIOD
      CALL CLMGT1(NLOT,IP,XLO)
      CALL CLMGT1(NHIT,IP,XHI)

      DO 65 J=JS,JF
      XMEAN=XMEANI(J)/HRTOT
      XDEV=SQRT(X2I(J)/HRTOT-XMEAN*XMEAN)
      XMAX=XMAXI(J)
      XMIN=XMINI(J)
      WRITE(outs,102)J,XMIN,XMAX,XMEAN,XDEV,XLO(J),XHI(J)
 102  FORMAT(1X,I4,2F8.1,3X,2F8.1,3X,2F8.1)
      call eddisp(itru,outs)
  65  CONTINUE
      XSF=1.0/THR
      TAVMN=XSF*TAVMN
      TAVMX=XSF*TAVMX
      TAVAV=XSF*TAVAV
      WRITE(outs,104)TMNMN,TMNMX,TMNAV
      call eddisp(itru,outs)
      WRITE(outs,105)TMXMN,TMXMX,TMXAV
      call eddisp(itru,outs)
      WRITE(outs,106)TAVMN,TAVMX,TAVAV,TLOM,THIM
      call eddisp(itru,outs)

  19  RETURN
      END

C ********* CLMDEG *******
C CLMDEG computes degree days or equivalent for the given period.
C Display is per day.
      SUBROUTINE CLMDEG
      PARAMETER (MT=24)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTPCH/ICOUT
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/DAYSF/KDS,KDF
      common/exporttgi/ixopen,ixloc,ixunit

      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)

      character outs*124

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CLMDEG'  ! set for cfiles

      helptopic='dd_base_temperature'
      call gethelptext(helpinsub,helptopic,nbhelp)
      BT=17.
      CALL EASKR(BT,' ',' Base temperature ? ',
     &  -10.,'W',40.,'W',17.0,'DD base temp',IER,nbhelp)

C NOW OUTPUT RESULTS.

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file...',' ','-')
      endif
      CALL CHEAD(30)
      MTH1=IM1
      UPM=0.
      DNM=0.
      HRM=0.

      WRITE(outs,12)BT
  12  FORMAT(' Degree day analysis at',F6.1,' Deg C')
      call edisp(itru,outs)
      WRITE(outs,16)
  16  FORMAT(' D  M  T   ',5X,'<',8X,'>')
      call edisp(itru,outs)

C INITIALISE PARAMETERS FOR WHOLE PERIOD
      UPI=0
      DNI=0
      HRTOT=0.

C DAY BY DAY
      DO 10 I=IDS,IDF

C READ VALUES OF THE CLIMATIC PARAMETERS FOR THIS DAY.
      IDD=I
      CALL CLMGET(IDD)

C SET PARAMETERS FOR THIS DAY
      UPJ=0
      DNJ=0
      JS=KDS
      JF=KDF
      IF(I.EQ.IDS)JS=MAX0(KDS,IT1)
      IF(I.EQ.IDF)JF=MIN0(KDF,IT2)
      DAYL=JF-JS+1

C AND ANALYSE IT
      DO 20 J=JS,JF
      TT=CTMP(J)
      IF(TT.GT.BT)GO TO 2

C T BELOW BASE,SAVE DEG-HRS IN DNJ
      DNJ=DNJ+(BT-TT)
      GO TO 20

C T ABOVE BASE ,SAVE DEG-HRS IN UPJ
   2  UPJ=UPJ+(TT-BT)
   20 CONTINUE

C CONVERT DAY TO D,M
      II=I
      CALL EDAYR(II,IDD,IMM)

C PREPARE FOR OUTPUT
C CHECK IF PAGE FULL(1 MONTH)
      IF(IMM.EQ.MTH1)GO TO 40

C COMPUTE PARAMETERS FOR THIS MONTH
      XHR=24.0/HRM

C TOTAL DEGDAYS FOR MONTH
      XDN=DNM/24
      XUP=UPM/24

C AV DEGDAYS PER DAY OVER MONTH
      YDN=XHR*XDN
      YUP=XHR*XUP
      WRITE(outs,102)YDN,YUP
 102  FORMAT(' Month:av/day',2F8.2)
      call edisp(itru,outs)
      WRITE(outs,101)XDN,XUP
 101  FORMAT(' Month:total ',2F8.2)
      call edisp(itru,outs)

      CALL CHEAD(30)
      WRITE(outs,12)BT
      call edisp(itru,outs)
      WRITE(outs,16)
      call edisp(itru,outs)
      MTH1=IMM

C UPDATE WHOLE PERIOD
      HRTOT=HRTOT+HRM
      UPI=UPI+UPM
      DNI=DNI+DNM
      UPM=0.
      DNM=0.
      HRM=0.

C OUTPUT ONE DAY
  40  CONTINUE

C CONVERT TO DEG-DAYS PER DAY
      XDN=DNJ/24
      XUP=UPJ/24

C WRITE THEM OUT
      WRITE(outs,100)IDD,IMM,JS,JF,XDN,XUP
 100  FORMAT(I2,',',I2,1X,I2,'-',I2,2F8.2)
      call eddisp(itru,outs)

C   UPDATE PARAMETERS FOR THIS MONTH
      UPM=UPM+UPJ
      DNM=DNM+DNJ
      HRM=HRM+DAYL

C AND GO TO NEXT DAY
   10 CONTINUE

C NOW LAST MONTH
C COMPUTE PARAMETERS FOR THIS MONTH
      XHR=24.0/HRM

C TOTAL DEGDAYS FOR MONTH
      XDN=DNM/24
      XUP=UPM/24

C AV DEGDAYS PER DAY OVER MONTH
      YDN=XHR*XDN
      YUP=XHR*XUP
      WRITE(outs,102)YDN,YUP
      call eddisp(itru,outs)
      WRITE(outs,101)XDN,XUP
      call eddisp(itru,outs)

C COMPUTE PARAMETERS FOR WHOLE PERIOD
      HRTOT=HRTOT+HRM
      UPI=UPI+UPM
      DNI=DNI+DNM
      XHR=24.0/HRTOT

C TOTAL DEGDAYS FOR PERIOD
      XDN=DNI/24
      XUP=UPI/24

C AV DEGDAYS PER DAY OVER PERIOD
      YDN=XHR*XDN
      YUP=XHR*XUP
      WRITE(outs,104)YDN,YUP
 104  FORMAT(' Total:av/day',2F8.2)
      call eddisp(itru,outs)
      WRITE(outs,103)XDN,XUP
 103  FORMAT(' Period total',2F8.2)
      call eddisp(itru,outs)

      RETURN
      END

C ********* SYNOPDEG *******
C SYNOPDEG computes monthly heating and cooling DEGREE DAYS 
C for the given period.
      SUBROUTINE SYNOPDEG
      PARAMETER (MT=24)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTPCH/ICOUT
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/DAYSF/KDS,KDF
      common/exporttgi/ixopen,ixloc,ixunit

      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)

      character outs*124

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SYNOPDEG'  ! set for cfiles

      helptopic='dd_base_temperatures'
      call gethelptext(helpinsub,helptopic,nbhelp)
      HBT=17.
      CALL EASKR(HBT,' ',' Heating base temperature ? ',
     &  -10.,'W',40.,'W',17.0,'DD heating base temp',IER,nbhelp)
      CBT=21.
      CALL EASKR(CBT,' ',' Cooling base temperature ? ',
     &  -10.,'W',40.,'W',21.0,'DD cooling base temp',IER,nbhelp)

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file...',' ','-')
      endif
      CALL CHEAD(30)
      MTH1=IM1
      DNM=0.
      HRM=0.
      UPMC=0.
      HRMC=0.

      WRITE(outs,12)HBT,CBT
  12  FORMAT(' Degree day analysis: heating base at',F6.1,' & cooling',
     &       F6.1,' Deg C')
      call edisp(itru,outs)
      WRITE(outs,16)
  16  FORMAT(' Month              Heat dd   Cool dd')
      call edisp(itru,outs)

C INITIALISE PARAMETERS FOR WHOLE PERIOD
      DNI=0
      HRTOT=0.
      UPIC=0
      HRTOTC=0.

C DAY BY DAY
      DO 10 I=IDS,IDF

C READ VALUES OF THE CLIMATIC PARAMETERS FOR THIS DAY.
        IDD=I
        CALL CLMGET(IDD)

C SET PARAMETERS FOR THIS DAY
        DNJ=0
        UPJC=0
        JS=KDS
        JF=KDF
        IF(I.EQ.IDS)JS=MAX0(KDS,IT1)
        IF(I.EQ.IDF)JF=MIN0(KDF,IT2)
        DAYL=JF-JS+1

C AND ANALYSE IT
        DO 20 J=JS,JF
          TT=CTMP(J)
          IF(TT.GT.HBT)then
          else

C T below heating BASE,SAVE DEG-HRS IN DNJ
            DNJ=DNJ+(HBT-TT)
          endif
          IF(TT.GT.CBT)then

C T above cooling BASE ,SAVE DEG-HRS IN UPJC
            UPJC=UPJC+(TT-CBT)
          endif
   20   CONTINUE

C CONVERT DAY TO D,M
        II=I
        CALL EDAYR(II,IDD,IMM)

C PREPARE FOR OUTPUT, check if month complete.
        IF(IMM.EQ.MTH1)GO TO 40

C COMPUTE PARAMETERS FOR THIS MONTH
        XHR=24.0/HRM
        XHRC=24.0/HRMC

C TOTAL DEGDAYS FOR MONTH
        XDN=DNM/24
        XUPC=UPMC/24

C AV DEGDAYS PER DAY OVER MONTH
        YDN=XHR*XDN
        YUPC=XHRC*XUPC
        WRITE(outs,102)IMM-1,YDN,YUPC
 102    FORMAT(' Month:',I2,' avg/day',2F8.2)
        call eddisp(itru,outs)
        WRITE(outs,101)IMM-1,XDN,XUPC
 101    FORMAT(' Month:',I2,' total  ',2F8.2)
        call eddisp(itru,outs)

        MTH1=IMM

C UPDATE WHOLE PERIOD
        HRTOT=HRTOT+HRM
        DNI=DNI+DNM
        DNM=0.
        HRM=0.
        HRTOTC=HRTOTC+HRMC
        UPIC=UPIC+UPMC
        UPMC=0.
        HRMC=0.

C Output one day, converting to deg-days per day.
  40    CONTINUE
        XDN=DNJ/24
        XUPC=UPJC/24

C   UPDATE PARAMETERS FOR THIS MONTH
        DNM=DNM+DNJ
        HRM=HRM+DAYL
        UPMC=UPMC+UPJC
        HRMC=HRMC+DAYL

C AND GO TO NEXT DAY
   10 CONTINUE

C NOW LAST MONTH
C COMPUTE PARAMETERS FOR THIS MONTH
      XHR=24.0/HRM

C TOTAL DEGDAYS FOR MONTH
      XDN=DNM/24
      XUPC=UPMC/24

C AV DEGDAYS PER DAY OVER MONTH
      YDN=XHR*XDN
      YUPC=XHRC*XUPC
      WRITE(outs,102)IMM,YDN,YUPC
      call eddisp(itru,outs)
      WRITE(outs,101)IMM,XDN,XUPC
      call eddisp(itru,outs)

C COMPUTE PARAMETERS FOR WHOLE PERIOD
      HRTOT=HRTOT+HRM
      DNI=DNI+DNM
      XHR=24.0/HRTOT
      HRTOTC=HRTOTC+HRMC
      UPIC=UPIC+UPMC
      XHRC=24.0/HRTOTC

C TOTAL DEGDAYS FOR PERIOD
      XDN=DNI/24
      XUPC=UPIC/24

C AV DEGDAYS PER DAY OVER PERIOD
      YDN=XHR*XDN
      YUPC=XHRC*XUPC
      WRITE(outs,104)YDN,YUPC
 104  FORMAT(' Total:av/day    ',2F8.2)
      call eddisp(itru,outs)
      WRITE(outs,103)XDN,XUPC
 103  FORMAT(' Period total   ',2F8.1)
      call eddisp(itru,outs)

      RETURN
      END

C ********* SYNOPDEGS *******
C SYNOPDEGS computes seasonal heating and cooling DEGREE DAYS 
C for the given period.
      SUBROUTINE SYNOPDEGS

C Manual inclusion of a parameter from building.h
      parameter (MSPS=10)
      parameter (MIPVA=MSPS)

#include "seasons.h"
C seasons.h provides typper and typsea

      PARAMETER (MT=24)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTPCH/ICOUT
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/exporttgi/ixopen,ixloc,ixunit
      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)

      character outs*124,PERST1*14,PERST2*44,PERST3*44

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SYNOPDEGS'  ! set for cfiles

C Jump out if no seasonal data is available.
      if(is1wins.eq.0.or.is2wins.eq.0.or.is1sprs.eq.0)then
        call usrmsg('No winter|transition|summer season definitions',
     &    'found in the climate database. Skipping.','W')
        return
      endif

      helptopic='dd_base_temperature'
      call gethelptext(helpinsub,helptopic,nbhelp)
      HBT=17.
      CALL EASKR(HBT,' ',' Heating base temperature ? ',
     &   -10.,'W',40.,'W',17.0,'DD heating base temp',IER,nbhelp)
      CBT=21.
      CALL EASKR(CBT,' ',' Cooling base temperature ? ',
     &   -10.,'W',40.,'W',21.0,'DD cooling base temp',IER,nbhelp)

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file...',' ','-')
      endif
      CALL CHEAD(30)
      DNM=0.
      HRM=0.
      UPMC=0.
      HRMC=0.

      WRITE(outs,12)HBT,CBT
  12  FORMAT(' Degree day analysis: heating base at',F6.1,' & cooling',
     &       F6.1,' Deg C')
      call edisp(itru,outs)
      call edisp(itru,' Season                     Heat dd   Cool dd')

C INITIALISE PARAMETERS FOR WHOLE PERIOD
      DNI=0
      HRTOT=0.
      UPIC=0
      HRTOTC=0.

C Loop through each seasonal period.
      do 42 k=1,10
        if(k.eq.1)then
          call edisp(itru,
     &      'Typical winter (early year) assessment period.')
          IDSS=ia1wins
          IDSF=ia1winf
        elseif(k.eq.2)then
          call edisp(itru,'Typical spring assessment period.')
          IDSS=ia1sprs
          IDSF=ia1sprf
        elseif(k.eq.3)then
          call edisp(itru,'Typical summer assessment period.')
          IDSS=iasums
          IDSF=iasumf
        elseif(k.eq.4)then
          call edisp(itru,'Typical sautumn assessment period.')
          IDSS=ia2sprs
          IDSF=ia2sprf
        elseif(k.eq.5)then
          call edisp(itru,
     &      'Typical winter (late year) assessment period.')
          IDSS=ia1wins
          IDSF=ia1winf
        elseif(k.eq.6)then
          call edisp(itru,'Winter (early year) season.')
          DNI=0
          HRTOT=0.
          UPIC=0
          HRTOTC=0.
          IDSS=is1wins
          IDSF=is1winf
        elseif(k.eq.7)then
          call edisp(itru,'Spring season.')
          IDSS=is1sprs
          IDSF=is1sprf
        elseif(k.eq.8)then
          call edisp(itru,'Summer season.')
          IDSS=is1sums
          IDSF=is1sumf
        elseif(k.eq.9)then
          call edisp(itru,'Autumn season.')
          IDSS=is2sprs
          IDSF=is2sprf
        elseif(k.eq.10)then
          call edisp(itru,'Winter (late year) season.')
          IDSS=is2wins
          IDSF=is2winf
        endif
        itrc=0
        CALL EPERSTR(IYEAR,IDSS,0,IDSF,24,1,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)

C For each day read climate data, set parameters and analyse.
        DO 10 I=IDSS,IDSF
          IDD=I
          CALL CLMGET(IDD)
          DNJ=0
          UPJC=0.0
          DAYL=24.0
          DO 20 J=1,24
            TT=CTMP(J)
            IF(TT.GT.HBT)then
            else

C T below heating BASE,SAVE DEG-HRS IN DNJ
              DNJ=DNJ+(HBT-TT)
            endif
            IF(TT.GT.CBT)then

C T above cooling BASE ,SAVE DEG-HRS IN UPJC
              UPJC=UPJC+(TT-CBT)
            endif
   20     CONTINUE
          XDN=DNJ/24
          XUPC=UPJC/24

C Update seasonal parameters and go to the next day.
          DNM=DNM+DNJ
          HRM=HRM+DAYL
          UPMC=UPMC+UPJC
          HRMC=HRMC+DAYL
   10   CONTINUE

C Compute parameters (total & average degree days) for this season.
        XHR=24.0/HRM
        XHRC=24.0/HRMC
        XDN=DNM/24
        XUPC=UPMC/24
        YDN=XHR*XDN
        YUPC=XHRC*XUPC
        WRITE(outs,'(a,a,2F8.2)')PERST2,' avg/day',YDN,YUPC
        call eddisp(itru,outs)
        WRITE(outs,'(44x,a,2F8.2)')' total  ',XDN,XUPC
        call eddisp(itru,outs)

C UPDATE WHOLE PERIOD
        HRTOT=HRTOT+HRM
        DNI=DNI+DNM
        DNM=0.
        HRM=0.
        HRTOTC=HRTOTC+HRMC
        UPIC=UPIC+UPMC
        UPMC=0.
        HRMC=0.
  42  continue

C At end of year. Compute parameters.
      HRTOT=HRTOT+HRM
      DNI=DNI+DNM
      XHR=24.0/HRTOT
      HRTOTC=HRTOTC+HRMC
      UPIC=UPIC+UPMC
      XHRC=24.0/HRTOTC
      XDN=DNI/24
      XUPC=UPIC/24
      YDN=XHR*XDN
      YUPC=XHRC*XUPC
      WRITE(outs,104)YDN,YUPC
 104  FORMAT(' Total:av/day    ',35x,2F8.2)
      call eddisp(itru,outs)
      WRITE(outs,103)XDN,XUPC
 103  FORMAT(' Period total   ',35x,2F8.1)
      call eddisp(itru,outs)

      RETURN
      END


C ******* SYNOPWDEG *******
C SYNOPWDEG computes weekly heating and cooling DEGREE DAYS 
C for the given period.
      SUBROUTINE SYNOPWDEG
      PARAMETER (MT=24)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTPCH/ICOUT
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/DAYSF/KDS,KDF
      common/exporttg/xfile,tg,delim
      common/exporttgi/ixopen,ixloc,ixunit

      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)

      character outs*124,DESCR*7,DESCR1*10,xfile*144,tg*1,delim*1
      logical ok,dok

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SYNOPWDEG'  ! set for cfiles

      helptopic='dd_base_temperature'
      call gethelptext(helpinsub,helptopic,nbhelp)
      HBT=17.
      CALL EASKR(HBT,' ',' Heating base temperature ? ',
     &  -10.,'W',40.,'W',17.0,'DD heating base temp',IER,nbhelp)
      CBT=21.
      CALL EASKR(CBT,' ',' Cooling base temperature ? ',
     &  -10.,'W',40.,'W',21.0,'DD cooling base temp',IER,nbhelp)

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file...',' ','-')
      endif
      CALL CHEAD(32)
      DNM=0.
      HRM=0.
      UPMC=0.
      HRMC=0.

      WRITE(outs,12)HBT,CBT
  12  FORMAT(' Degree day analysis: heating base at',F6.1,' & cooling',
     &       F6.1,' Deg C')
      call edisp(itru,outs)
      WRITE(outs,'(a)')
     &  ' Week    (starting)       Heat dd           Cool dd'
      call edisp(itru,outs)
      WRITE(outs,'(a)')
     &  '                          avg/day  total    avg/day  total'
      call edisp(itru,outs)

C INITIALISE PARAMETERS FOR WHOLE PERIOD
      DNI=0
      HRTOT=0.
      UPIC=0
      HRTOTC=0.

C Find day of week for start of the period. IWK is week number, IDSOW
C is the julian day at the start of the week.
      CALL EDAYR(IDS,IDAYN,IMTHN)
      CALL EWEEKD(IDAYN,IMTHN,IYEAR,IXDWK)
      IDSOW=IDS
      IWK=0
      ix=0

C DAY BY DAY
      DO 10 I=IDS,IDF

C READ VALUES OF THE CLIMATIC PARAMETERS FOR THIS DAY.
        ix=ix+1
        IDD=I
        CALL CLMGET(IDD)

C For each day read climate data, set parameters and analyse.
        DNJ=0
        UPJC=0
        JS=KDS
        JF=KDF
        IF(I.EQ.IDS)JS=MAX0(KDS,IT1)
        IF(I.EQ.IDF)JF=MIN0(KDF,IT2)
        DAYL=JF-JS+1

C AND ANALYSE IT
        DO 20 J=JS,JF
          TT=CTMP(J)
          IF(TT.GT.HBT)then
          else

C T below heating BASE,SAVE DEG-HRS IN DNJ
            DNJ=DNJ+(HBT-TT)
          endif
          IF(TT.GT.CBT)then

C T above cooling BASE ,SAVE DEG-HRS IN UPJC
            UPJC=UPJC+(TT-CBT)
          endif
   20   CONTINUE

C CONVERT DAY TO D,M
        II=I
        CALL EDAYR(II,IDD,IMM)
        CALL EWEEKD(IDD,IMM,IYEAR,IDWK)

C PREPARE FOR OUTPUT, check if week complete.
        IF(II.eq.IDS)GO TO 40
        IF(IDWK.ne.IXDWK)GO TO 40
        iwk=iwk+1

C COMPUTE PARAMETERS FOR THIS week
        XHR=24.0/HRM
        XHRC=24.0/HRMC

C TOTAL DEGDAYS FOR week
        XDN=DNM/24
        XUPC=UPMC/24

C AV DEGDAYS per day over week
        call stdate(iyear,IDSOW,DESCR,DESCR1)
        YDN=XHR*XDN
        YUPC=XHRC*XUPC
        WRITE(outs,'(a,i2,3a,4F9.2)')' Week:',iwk,' (',DESCR1,') ',
     &    YDN,XDN,YUPC,XUPC
        call eddisp(itru,outs)

C Pause if a long list.
        if((idf-IDS).gt.120.and.ix.gt.120)then
          dok=.true.
          helptopic='dd_reporting_pause'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call askok(' continue listing?',' ',OK,dok,nbhelp)
          ix=1
          WRITE(outs,'(a)')
     &  ' Week    (starting)       Heat dd           Cool dd'
          call edisp(itru,outs)
          WRITE(outs,'(a)')
     &  '                          avg/day  total    avg/day  total'
          call edisp(itru,outs)
        endif

C Update whole period.
        IDSOW=II
        HRTOT=HRTOT+HRM
        DNI=DNI+DNM
        DNM=0.
        HRM=0.
        HRTOTC=HRTOTC+HRMC
        UPIC=UPIC+UPMC
        UPMC=0.
        HRMC=0.

C CONVERT TO DEG-DAYS PER DAY
  40    CONTINUE
        XDN=DNJ/24
        XUPC=UPJC/24

C UPDATE PARAMETERS FOR THIS week
        DNM=DNM+DNJ
        HRM=HRM+DAYL
        UPMC=UPMC+UPJC
        HRMC=HRMC+DAYL

C AND GO TO NEXT DAY
   10 CONTINUE

C NOW LAST week
C COMPUTE PARAMETERS FOR THIS week
      XHR=24.0/HRM

C TOTAL DEGDAYS FOR week
      XDN=DNM/24
      XUPC=UPMC/24

C AV DEGDAYS PER DAY OVER week
      iwk=iwk+1
      call stdate(iyear,IDSOW,DESCR,DESCR1)
      YDN=XHR*XDN
      YUPC=XHRC*XUPC
      WRITE(outs,'(a,i2,3a,4F9.2)')' Week:',iwk,' (',DESCR1,') ',
     &  YDN,XDN,YUPC,XUPC
C      WRITE(outs,102)iwk,YDN,YUPC,DESCR1
      call eddisp(itru,outs)
C      WRITE(outs,101)iwk,XDN,XUPC
C      call eddisp(itru,outs)

C COMPUTE PARAMETERS FOR WHOLE PERIOD
      HRTOT=HRTOT+HRM
      DNI=DNI+DNM
      XHR=24.0/HRTOT
      HRTOTC=HRTOTC+HRMC
      UPIC=UPIC+UPMC
      XHRC=24.0/HRTOTC

C TOTAL DEGDAYS FOR PERIOD
      XDN=DNI/24
      XUPC=UPIC/24

C AV DEGDAYS PER DAY OVER PERIOD
      YDN=XHR*XDN
      YUPC=XHRC*XUPC
      WRITE(outs,104)YDN,YUPC
 104  FORMAT(' Total:av/day   ',2F8.2)
      call eddisp(itru,outs)
      WRITE(outs,103)XDN,XUPC
 103  FORMAT(' Period total  ',2F8.1)
      call eddisp(itru,outs)

      RETURN
      END

C TO GET ONE DAY OF ONE CLIMATE PARAMETER
      SUBROUTINE CLMGT1(IDD,IP,XJ)
      PARAMETER (MT=24)

      DIMENSION XJ(24)
      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)

      CALL CLMGET(IDD)

C Recover required value.
      DO 20 J=1,24
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
        XJ(J)=X
  20  CONTINUE

      RETURN
      END

