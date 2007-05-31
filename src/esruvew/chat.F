          SUBROUTINE CHAT

C If the plan needs (re-)drawing subroutine plan is called to do so
C plan only drawn when picking e.g. eyepoint graphically.

C Subroutine rpmnu is then called to interact with the user.

C When he wants a plot subroutine begtek or begcal is called.
C It initialises the output device and sets up a frame size.
C Chat returns and is re-entered when the plot has been done.

C When the user is finished subroutine finplt is called
C It closes down the plotter if it has been used
C and says 'viewer bye-bye', and does a stop.

      INCLUDE 'viewer.h'
      INCLUDE 'viewer.h2'

      COMMON/SPAD/MMOD,LIMIT,LIMTTY 
      COMMON/NNAAMM/NAMDS,NAMPIC
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/pophelp/h(60)

C Redirected text/graphics parameters.
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit
      CHARACTER NAMDS*30,NAMPIC*40
      character*12 M(23)
      character*5 XEQ,YEQ,ZEQ
      character wins*20,outs*124,H*72,hold*36
      character xfile*144,tg*1,delim*1
      logical close,OK

C MENU OF COMMANDS
      DATA XEQ,YEQ,ZEQ/'X=   ','Y=   ','Z=   '/
      INIT=0
      M(1)= 'a input    '
      M(2)= 'b eye point'
      M(3)= 'c foc &Midp'
      M(4)= 'd midpoint '
      M(5)= 'e hidden   '
      M(6)= 'f dashed   '
      M(7)= 'g visible  '
      M(8)= 'h angle vew'
      M(9)= 'i lens mm  '
      M(10)='j enlarge %'
      M(11)='k input all'
      M(12)='l deft vews'
      M(13)='m layers   '
      M(14)='n new file '
      M(15)='o type all '
      M(16)='p || vert  '
      M(17)='q || horz  '
      M(18)='r perspect.'
      M(19)='s orthogra.'
      M(20)='t draw   '
      M(21)='> xfig file'
      M(22)='? help     '
      M(23)='- exit     '

C----------------------------------------------------
C     SET UP INITIAL PARAMETERS
  13  IF(INIT.GT.0)GOTO  20

      INIT=1
C NUMERIC INPUT FOR EYEP ETC. NOT SET
      INPT=0

C PROJECTION IS PERSPECTIVE
14    IPROJ=2

C VIEWCONE FILLS SCREEN
      SYNVKL=-90.

C STEREO NOT ON (and not used).
      ISTERE=0

C LENS IS 35MM.
      FL=35.

C ENLARGEMENT FACTOR IS 1.
      EF=1.

C HIDDEN LINES ARE REMOVED
      NTRAT=0

C CENTRE POINT AND FOCUS POINT ARE IN THE MIDDLE OF THE SCENE
      BBBB(1)=.5*(XMX+XMN)
      BBBB(2)=.5*(YMX+YMN)
      BBBB(3)=.5*(ZMX+ZMN)
      CCCC(1)=BBBB(1)
      CCCC(2)=BBBB(2)
      CCCC(3)=BBBB(3)
      X=10.*AMAX1(XMX-XMN,YMX-YMN,ZMX-ZMN)

C EYE POINT IS 10 SCENE DIAMETERS AWAY IN -X,-Y,+Z
      DDDD(1)=CCCC(1)-X
      DDDD(2)=CCCC(2)-X
      DDDD(3)=CCCC(3)+X

C SET PICTURE NAME
      CALL SETNAM

C----------------------------------------------------
  20  INO=-2
      IPLN=0

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('vew ',1)

C Main MENU.
21    CALL EMENU('viewer',M,23,INO)
      IF(INO.LE.0 .OR. INO.GT.23)then
        INO=-1
        GOTO 21
      elseif(ino.eq.23)then
        CALL usrmsg('Closing viewing facility...',' ','P')
        CALL EPAGEND
        STOP
      elseif(ino.eq.22)then

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('vew ',0)
        GOTO 20
      endif

C VALID COMMAND,GOTO  IT
      GOTO  (600,501,502,503,36,361,362,43,41,42,47,70,
     &       80,993,46,444,443,442,441,39,399),INO


C----------------------------------------------------
  32  CONTINUE
      DDDD(1)=X
      DDDD(2)=Y
      DDDD(3)=Z
      CALL PUT(X,Y,69)
      CALL SETNAM
      GOTO 21

C----------------------------------------------------
33    CCCC(1)=X
      CCCC(2)=Y
      CCCC(3)=Z
      CALL PUT(X,Y,70)
      BBBB(1)=X
      BBBB(2)=Y
      BBBB(3)=Z
      CALL PUT(X,Y,77)
      GOTO 21

C----------------------------------------------------
C MIDP, AFTER INPUTTING VALUE
34    BBBB(1)=X
      BBBB(2)=Y
      BBBB(3)=Z
      CALL PUT(X,Y,77)
      GOTO 21

C----------------------------------------------------
C HIDDEN, DASHED, VISIBL
  36  NTRAT=0
      GOTO  21
  361 NTRAT=1
      GOTO  21
  362 NTRAT=2
      GOTO  21

C----------------------------------------------------
C SCREEN, PLOT ON TEKTRONIX
C IF NOT TEK ,NO PLOTTING ON SCREEN
  39  IF (MMOD .LT. 0 .AND. MMOD .NE. -6)then
        call edisp(IUOUT,' SCREEN PLOT NOT ALLOWED!')
        INO=-1
        GOTO 21
      endif
      CALL PLTTEK
      GOTO  20

C----------------------------------------------------
C SET PLOT FILE  MODE,WHICH DOESNT PLOT
C 399  CALL PLTFIL
 399  call ctlexp(xfile,ixopen,ixloc,ixunit,'G','hidden ln',IER)
      IF (MMOD .LT. 0 .AND. MMOD .NE. -6)then
        call edisp(IUOUT,' SCREEN PLOT NOT ALLOWED!')
        INO=-1
        GOTO 21
      endif
      CALL PLTTEK
      call ctlexp(xfile,ixopen,ixloc,ixunit,'G','hidden ln',IER)
      GOTO  20

C----------------------------------------------------
C BYEBYE, CLOSE DOWN PLOTTER IF USED, AND EXIT

C----------------------------------------------------
C LENSFL
41    H(1)=' Focus length in mm.'
      CALL EASKR(FL,' ','F(MM) ? ',
     &            0.,'F',0.,'-',50.0,'F (mm)',IER,1)
      SYNVKL=0.
C SET PICTURE NAME
      CALL SETNAM
      GOTO  21

C----------------------------------------------------
C *ENLRG
42    H(1)=' Enlargement factor (100 is normal size)'
      CALL EASKR(EF,' ',' Enlargement factor (%) ? ',
     &            0.,'F',0.,'-',50.0,'enlargement',IER,1)

C HAVE SUCCESSFULLY GOT IN  ENLARGEMENT FACTOR
C SET PHOTOMONTAGE MODE
      SYNVKL=0.

C SET PICTURE NAME
      CALL SETNAM
      GOTO 21

C----------------------------------------------------
C ANGLEV, READ VIEWCONE ANGLE AND SET VIEWCONE MODE
43    H(1)=' Viewcone angle is in degrees (1-175)'
      H(2)=' 0=Photomontage -1=full picture.'
      CALL EASKR(X,' ',' Viewcone angle ? ',
     &           -1.,'F',175.,'F',40.0,'viewcone',IER,2)

      call eclose(X,0.0,0.001,close)
      IF(close)GOTO 43
      IF(X.LT.0.)X=-90.
      SYNVKL=X
C SET PICTURE NAME
      CALL SETNAM
      GOTO 21

C----------------------------------------------------
C SET PROJECTION
C ORTHO
 441  IPROJ=1
      GOTO  445

C PERSPECTIVE
 442  IPROJ=2
      GOTO  445

C PARALLEL HORIZ.
 443  IPROJ=3
      GOTO  445

C PARALLEL VERT.
 444  IPROJ=4
C SET PICTURE NAME
 445   CALL SETNAM
      GOTO  21

C----------------------------------------------------
C TYPALL
  46  CONTINUE

      CALL SPS('Eye-point',9)
      CALL SPD(XEQ,2,DDDD(1))
      CALL SPD(YEQ,2,DDDD(2))
      CALL SPD(ZEQ,2,DDDD(3))

      CALL SPS('Focus-point',11)
      CALL SPD(XEQ,2,CCCC(1))
      CALL SPD(YEQ,2,CCCC(2))
      CALL SPD(ZEQ,2,CCCC(3))

      CALL SPS('Mid-point',9)
      CALL SPD(XEQ,2,BBBB(1))
      CALL SPD(YEQ,2,BBBB(2))
      CALL SPD(ZEQ,2,BBBB(3))

      IF(NTRAT.EQ.0)CALL SPS('Hidden',6)
      IF(NTRAT.EQ.1)CALL SPS('Dashed',6)
      IF(NTRAT.EQ.2)CALL SPS('Visible',7)

      call eclose(SYNVKL,0.0,0.001,close)
      IF(.NOT.close)CALL SPD('AV=',3,SYNVKL)
      IF(close)CALL SPD('F(MM)=',6,FL)
      IF(close)CALL SPD('TIMES ',6,EF)

      IF(IPROJ.EQ.1)CALL SPS('Orthogonal',10)
      IF(IPROJ.EQ.2)CALL SPS('Perspective',11)
      IF(IPROJ.EQ.3)CALL SPS('Para horiz',10)
      IF(IPROJ.EQ.4)CALL SPS('Para vert',9)
      GOTO 21

C----------------------------------------------------
C INPALL
  47  CONTINUE

 471  CONTINUE
      WRITE(HOLD,'(3f9.3)')DDDD(1),DDDD(2),DDDD(3)
      H(1)=' Eyepoint is in metres.'
      CALL EASKS(HOLD,' Eyepoint X, Y, Z (metres): ',
     &    ' ',36,' -100. -100. 100. ','eyepoint',IER,1)
      K=0
      CALL EGETWR(HOLD,K,DDDD(1),0.,0.,'-','eye x',IER)
      CALL EGETWR(HOLD,K,DDDD(2),0.,0.,'-','eye y',IER)
      CALL EGETWR(HOLD,K,DDDD(3),0.,0.,'-','eye z',IER)
      if(ier.ne.0)goto 471

 472  CONTINUE
      H(1)=' Focuspoint is in metres.'
      WRITE(HOLD,'(3f9.3)')CCCC(1),CCCC(2),CCCC(3)
      H(1)=' Eyepoint is in metres.'
      CALL EASKS(HOLD,' Focus point X, Y, Z (metres): ',
     &    ' ',36,' 1. 1. 1. ','focus point',IER,1)
      K=0
      CALL EGETWR(HOLD,K,CCCC(1),0.,0.,'-','foc x',IER)
      CALL EGETWR(HOLD,K,CCCC(2),0.,0.,'-','foc y',IER)
      CALL EGETWR(HOLD,K,CCCC(3),0.,0.,'-','foc z',IER)
      if(ier.ne.0)goto 472

 473  CONTINUE
      H(1)=' Midpoint is in metres.'
      WRITE(HOLD,'(3f9.3)')BBBB(1),BBBB(2),BBBB(3)
      CALL EASKS(HOLD,' Midpoint X, Y, Z (metres): ',
     &    ' ',36,' 1. 1. 1. ','mid point',IER,1)
      K=0
      CALL EGETWR(HOLD,K,BBBB(1),0.,0.,'-','mid x',IER)
      CALL EGETWR(HOLD,K,BBBB(2),0.,0.,'-','mid y',IER)
      CALL EGETWR(HOLD,K,BBBB(3),0.,0.,'-','mid z',IER)
      if(ier.ne.0)goto 473

C << variable ?? >>
      H(1)=' You must choose one of the options.'
      CALL EASKABC('Hidden lines:',' ','omit','dash','draw',IW,1)
      ntrat=IW-1

      H(1)=' You must choose one of the options.'
      CALL EASKABCD('Projection:',' ','orthogonal','perspective',
     &              'pa.hor','pa.vert',NTRAT,1)

 476  CONTINUE
      H(1)=' Viewcone angle is in degrees (1-175)'
      H(2)=' 0=Photomontage -1=full picture.'
      CALL EASKR(SYNVKL,' ',' Viewcone angle ? ',
     &           -1.,'F',175.,'F',40.0,'viewcone',IER,2)
      WRITE(IUOUT,*) 'Viewcone angle (deg)(0=Photomontage) ?>'
      if(ier.ne.0)goto 476
      IF(SYNVKL.LT.0.)SYNVKL=-90.
      call eclose(SYNVKL,0.0,0.001,close)
      IF(.NOT.close)GOTO  478
C IF=0. THEN PHOTOMONTAGE MODE
      GOTO  477

C PHOTOMONTAGE MODE:ASK FOR FL & ELNARGE
 477  H(1)=' Focus length in mm.'
      CALL EASKR(FL,' ','F(MM) ? ',
     &            0.,'F',0.,'-',50.0,'F (mm)',IER,1)
      H(1)=' Enlargement factor....'
      CALL EASKR(EF,' ',' Enlargement factor ? ',
     &            0.,'F',0.,'-',50.0,'enlargement',IER,1)
      if(ier.ne.0)goto 477
      GOTO  478

C FINISH INPUT
C RETURN TO MENU
C SET PICTURE NAME
 478  CALL SETNAM
      GOTO  21

C----------------------------------------------------
C CALCMP, READ SIZE AND PLOT << PLTCAL not enabled >>.

C----------------------------------------------------
C /INPUT PICKED
C   SET FOR NUMERIC INPUT FOR THIS PICTURE
 600  INPT=1
      GOTO  21

C----------------------------------------------------
C EYEP, FOCUSP, MIDP
C GET POINT ROUTINE
C EYEP
 501  IPT=1
      GOTO  51

C FOCUSP
 502  IPT=2
      GOTO  51

C MIDP
 503  IPT=3

  51  CONTINUE
C IF /INPUT SET GOTO  ASK FOR VALUE
C IF NOT TEK ALWAYS INPUT NUMERICALLY
      IF(MMOD.LT.0)GOTO  54

      IF(INPT.EQ.1)GOTO  54

      IF(IPLN.EQ.0)CALL PLAN
   52 INO=1

C Get scaling factors and reduce the size of the plan and add to
C the normal offsets to show the image near the centre.
      call usrmsg(' ',' Select point via cursor...','-')
      call axiscale(igw,igwh,XMN,XMX,YMN,YMX,xsc,ysc,sca,Xadd,Yadd)
      scap=sca*0.2
      addx=(FLOAT(igw)*0.4)/scap
      addy=(FLOAT(igwh)*0.4)/scap
      call linescale(igl,addx,scap,igb,addy,scap)
      CALL trackview(ii,ix,iy)

C Transform back from mouse position to actual coordinate.
      x=((FLOAT(ix-igl)/scap)-addx)
      y=((FLOAT(igb-iy)/scap)-addy)
      WRITE(outs,53)x,y
  53  FORMAT(' selected point is at x=',F7.2,' & y=',F7.2)
      CALL EDISP(iuout,outs)
      INPT=0
      GOTO 54

C INPUT NUMERICAL VALUES
 54   CONTINUE
      IF(IPT.EQ.1)WRITE(HOLD,'(3f9.3)')DDDD(1),DDDD(2),DDDD(3)
      IF(IPT.EQ.2)WRITE(HOLD,'(3f9.3)')CCCC(1),CCCC(2),CCCC(3)
      IF(IPT.EQ.3)WRITE(HOLD,'(3f9.3)')BBBB(1),BBBB(2),BBBB(3)
      H(1)=' Coordinates are in metres.'
      CALL EASKS(HOLD,' Point X, Y, Z (metres): ',
     &    ' ',36,' -100. -100. 100. ','eyepoint',IER,1)
      K=0
      CALL EGETWR(HOLD,K,X,0.,0.,'-','point x',IER)
      CALL EGETWR(HOLD,K,Y,0.,0.,'-','point y',IER)
      CALL EGETWR(HOLD,K,Z,0.,0.,'-','point z',IER)
      if(ier.ne.0)goto 54

      IF(IPT.EQ.1)GOTO  32
      IF(IPT.EQ.2)GOTO  33
      IF(IPT.EQ.3)GOTO  34
C OTHERWISE AN ERROR
      GOTO  20

C----------------------------------------------------
C NEW INPUT FILE
C CALL INPUT AND THEN REINITIALISE
C ALLOW ADD OF MORE GEOMETRY DATA WITHOUT INITIALISE
 993  H(1)=' You must choose one of the options.'
      CALL EASKABC('options:',' ','Initialise/replace',
     &      'Add new file to','Subtract from existing data',IW,1)
      init=IW-1
      IF(INIT.EQ.0 .OR. INIT.EQ.1)THEN
           CALL INPUT(INIT)
      ELSE IF (INIT.EQ.2)THEN
        CALL EASKI(NBNEW,' ',' Delete data after body no.?',
     &     0,'F',2,'F',0,'delete after',IER,1)
        NB=NBNEW
        NP=IBP2(NBNEW)
        NF=IBF2(NBNEW)
        NR=IBE2(NBNEW)
      ELSE
        GOTO  993
      ENDIF
      GOTO  13

C----------------------------------------------------
C DEFAULT VIEW SETTING
  70  CONTINUE
      h(1)='Available views: '
      h(2)='  perspective (=initial view) '
      h(3)='  plan view'
      h(4)='  isometric (axonometric 45/45)'
      h(5)='  elevation (N,S,E,W) '
      h(6)='  cavalier (plan oblique 45/45)'
      h(7)='  sun view (at a specific date & time'
      call easkatog('Default views:',' ','perspec','plan','isometric',
     &              'elev.','cavalier','sun',' ',IW,7)
      call usrmsg(' ',' ','-')
      IDV=IW

C GET MAX XYZ FOR 45 DEG
      XYZMX=AMAX1((XMX-XMN),(YMX-YMN),(ZMX-ZMN))
C FOCUS,MIDPOINTS ALWAYS CENTRE OF SCENE
C FOCUS P
      BBBB(1)=.5*(XMX+XMN)
      BBBB(2)=.5*(YMX+YMN)
      BBBB(3)=.5*(ZMX+ZMN)
C =MIDPT
      CCCC(1)=BBBB(1)
      CCCC(2)=BBBB(2)
      CCCC(3)=BBBB(3)
      GOTO  (14,72,73,74,75,76),IDV

C----------------------------
C PLAN
C ORTHOG
  72  IPROJ=1
C NO ANGLE
      SYNVKL=-90.
C VISIBLE
      NTRAT=2
C EYEPT ABOVE
      DDDD(1)=BBBB(1)
      DDDD(2)=BBBB(2)-0.005*(YMX-YMN)
      DDDD(3)=BBBB(3)+10.*(ZMX-ZMN)
C SET PICTURE NAME
      NAMPIC='  Plan view'
      GOTO  21

C----------------------------
C AXONOMETRIC
C ORTHOG
  73  IPROJ=1
C NO ANGLE
      SYNVKL=-90.
C VISIBLE
      NTRAT=0
C EYEPT ABOVE
      DDDD(1)=BBBB(1)+10.*XYZMX
      DDDD(2)=BBBB(2)+10.*XYZMX
      DDDD(3)=BBBB(3)+10.*XYZMX
 
C WHICH QUADRANT
      H(1)='Standard quadrants are in terms of axis values.'
      CALL EASKABCD('quadrant:',' ','+X+Y+Z',
     &      '-X+Y+Z','-X-Y+Z','+X-Y+Z',IQUAD,1)
      GOTO  (734,735,736,737),IQUAD
 734  GOTO  738
 
 735  DDDD(1)=BBBB(1)-10.*XYZMX
      GOTO  738
 
 736  DDDD(1)=BBBB(1)-10.*XYZMX
      DDDD(2)=BBBB(2)-10.*XYZMX
      GOTO  738
 
 737  DDDD(2)=BBBB(2)-10.*(YMX-YMN)
 738  CONTINUE
      nampic='  Axonometric'
      GOTO  21

C----------------------------
C ELEVATION
C ORTHOG
  74  IPROJ=1
C SET SAME ANGLE OF VISION FOR ALL ELEVATIONS
      WW=1.1*AMAX1((XMX-XMN)/(YMX-YMN),(YMX-YMN)/(XMX-XMN))
      SYNVKL=2.*180./3.14159*ATAN(0.05*WW)
C VISIBLE
      NTRAT=0
C EYEPT ABOVE
C ADJUST EYEP SLIGHTLY
      DDDD(1)=1.001*BBBB(1)
      DDDD(2)=1.001*BBBB(2)
      DDDD(3)=1.001*BBBB(3)
 
C FIND WHICH ELEVATION
      H(1)='Standard elevations are from cardinal directions.'
      ielev=3
      CALL EASKABCD('elevation:',' ','north',
     &      'east','south','west',IELEV,1)

      GOTO  (744,745,746,747),IELEV

 744  DDDD(2)=DDDD(2)+10.*(YMX-YMN)
      GOTO  748

 745  DDDD(1)=DDDD(1)+10.*(XMX-XMN)
      GOTO  748

 746  DDDD(2)=DDDD(2)-10.*(YMX-YMN)
      GOTO  748

 747  DDDD(1)=DDDD(1)-10.*(XMX-XMN)
 748  CONTINUE
      nampic='  Elevation'
      GOTO  21

C----------------------------
C CAVALIER  IS plan OBLIQUE 45/45
C AND PRESERVES PLAN DIMENSIONS
C PARALLEL-HORIZ
  75  IPROJ=3
C NO ANGLE
      SYNVKL=-90.
C VISIBLE
      NTRAT=0
C EYEPT 45 DEG TO EACH AXIS
      DDDD(1)=BBBB(1)+10.*XYZMX
      DDDD(2)=BBBB(2)+10.*XYZMX
      DDDD(3)=BBBB(3)+10.*XYZMX

C WHICH QUADRANT
      H(1)='Standard quadrants are in terms of axis values.'
      iquad=3
      CALL EASKABCD('quadrant:',' ','+X+Y+Z',
     &      '-X+Y+Z','-X-Y+Z','+X-Y+Z',IQUAD,1)

      GOTO  (754,755,756,757),IQUAD

 754  GOTO  758

 755  DDDD(1)=BBBB(1)-10.*XYZMX
      GOTO  758

 756  DDDD(1)=BBBB(1)-10.*XYZMX
      DDDD(2)=BBBB(2)-10.*XYZMX
      GOTO  758

 757  DDDD(2)=BBBB(2)-10.*XYZMX
 758  CONTINUE
      nampic='  Cavalier'
      GOTO  21

C----------------------------------------------------
C SUN POSITION VIEW
C   GET NORTH ANGLE, DAY AND MONTH AND HOUR
C   CAALCULATE SUN ALTITUDE AND AZIMUTH
C   CALCULATE EQUIV EYE POSITION REL TO FOCUS
C   FOR PARALLEL PROJECTION (ORTHOG)
 76   IPROJ=1
C NO ANGLE
      SYNVKL=-90.

      H(1)='nothing '
      CALL EASKR(ANORTH,' ',
     & ' North angle (deg. clockwise from Y-axis) ? ',-360.,
     &             'F',360.,'F',0.0,'north angle',IER,1)
      CALL EASKR(SLAT,' ',' Site latitude ? ',-89.,
     &             'F',89.,'F',0.0,'latitude',IER,1)
      CALL EASKR(SLONG,' ',' longitude difference ? ',-15.,
     &             'F',15.,'F',0.0,'ongitude diff',IER,1)
      WRITE(IUOUT,*) 'North angle (deg. clockwise from Y-axis?>'
      H(1)='nothing '
      WINS=' 12  1   1 '
      CALL EASKS(WINS,' ',' Hour Day Month:',
     &      20,'12  1  1','hr day mth',IER,1)
      K=0
      CALL EGETWI(WINS,K,IHR,0,0,'-','hour',IER)
      CALL EGETWI(WINS,K,IDAY,0,0,'-','day',IER)
      CALL EGETWI(WINS,K,IMTH,0,0,'-','month',IER)

C GET ALTITUDE AND AZIMUTH YEARDAY NUMBER
      CALL EDAY(IDAY,IMTH,IYDN)
      CALL AZALTS ( IYDN,IHR,1,SLONG,SLAT,VALT,VAZI)
      WRITE(outs,155)VAZI,VALT
 155  FORMAT(' AZI',F8.1,' ALT',F8.1)
      call edisp(iuout,outs)
      call usrmsg(' ',' ','-')

      VAZI0=VAZI
C RELATIVE TO Y-AXIS
      VAZI=VAZI+ANORTH
C DISTANCE OF SUN!
      DSUN=10.*AMAX1(XMX-XMN,YMX-YMN,ZMX-ZMN)
C EYEPOINT
      RAD=3.14159/180.
      DDDD(1)=BBBB(1)+DSUN*COS(RAD*VALT)*SIN(RAD*VAZI)
      DDDD(2)=BBBB(2)+DSUN*COS(RAD*VALT)*COS(RAD*VAZI)
      DDDD(3)=BBBB(3)+DSUN*SIN(RAD*VALT)
      write(nampic,157)IDAY,IMTH,IHR,VALT,VAZI0
 157  FORMAT('Sunview:',I2,',',I2,1X,I2,':00Alt',F5.1,'Azi',F6.1,'    ')
      GOTO  21

C----------------------------------------------------
C SET LAYERS
  80  CONTINUE
C SET ALL ALYERS OFF
      DO 85 II=0,MLAY
      LAYER(II)=0
  85  CONTINUE
C GET NEW LIST OF LAYERS ON
      call edisp(iuout,' Select ALL layers to be ON> ')
      CALL SELECT(IUIN,0,MLAY,LAYER,1)
      go to 21
C----------------------------------------------------
  999 END

C ****** SET PICTURE NAME
      SUBROUTINE SETNAM

      COMMON/NNAAMM/NAMDS,NAMPIC
      CHARACTER NAMDS*30,NAMPIC*40
      INCLUDE 'viewer.h'

      IF (IPROJ.EQ.1)THEN
           NAMPIC(1:4)='Orth'
      ELSE IF(IPROJ.EQ.2)THEN
           NAMPIC(1:4)='Pers'
      ELSE IF(IPROJ.EQ.3)THEN
           NAMPIC(1:4)='PaHo'
      ELSE IF(IPROJ.EQ.4)THEN
           NAMPIC(1:4)='PaVe'
      ENDIF
      WRITE(NAMPIC(5:30),110) (DDDD(I),I=1,3)
 110  FORMAT(':EX',F7.1,'Y',F7.1,'Z',F7.1)
      IF(SYNVKL.LT.0.0)THEN
           WRITE(NAMPIC(31:40),120)
 120       FORMAT(':AVunset  ')
      ELSEIF (SYNVKL.EQ.0)THEN
           INTFL=FL+0.01
           WRITE(NAMPIC(31:40),130) INTFL,EF
 130       FORMAT('L',I2,'*',F4.1,'  ')
      ELSEIF(SYNVKL.GT.0.0)THEN
           WRITE(NAMPIC(31:40),140) SYNVKL
 140       FORMAT(':AV',F4.0,'   ')
      ENDIF
      RETURN
      END
