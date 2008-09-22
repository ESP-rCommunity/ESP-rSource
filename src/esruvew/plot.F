C Plot functions.

      SUBROUTINE PLOTTC(ICOL)
#include "viewer.h"
 
C PLOTTC SETS CURRENT DISPLAY COLOUR
      IF(IOUTD.EQ.1)GO TO 10
      RETURN
  
C--------------------------------
C SCREEN DISPLAY COLOUR
  10  continue
C <<      CALL DCLSET(ICOL)
      RETURN
  
      END

C------------------------------------------------------
      SUBROUTINE PLOTTR(CL,AL,BL,CO,AO,BO,IO)

C   PLOT FROM (MOVE TO) AL,BL   TO AO,BO
C   A,B ARE IN PICTURE COORDS (+-1000)
C   PLOTTR SCALES THESE TO REAL (VIEWPORT) COORDS
C   BEFORE CLIPPING AND PLOTTING TO +-BILDX,+-BILDY
C   SCALE FACTOR IS SAVED IN
C      COMMON/BPIC2/SCFXY
C   PEN INDEX NOW AS TEKTRONIX I.E.
C    1   DRAW(PEN DOWN)  0   MOVE(PEN UP)   <0  DASHED LINE

#include "viewer.h"

      COMMON /FIL/ IFUSE, IFO
      DATA SX,SY,LP/0.,0.,3/

C SCALE PEN POSITION TO ABSOLUTE COORDS
      C=SCFXY*CO
      A=SCFXY*AO
      B=SCFXY*BO
      SX=SCFXY*AL
      SY=SCFXY*BL

C PLOT DEPENDS ON OUTPUT DEVICE:
C TEKTRONIX
      IF(IOUTD.EQ.1)GO TO 10

C CALCOMP
      IF(IOUTD.EQ.2)GO TO 20

C LOCAL PLOT=TEK
      IF(IOUTD.EQ.3)GO TO 10

C PLOT FILE ONLY
      IF (IOUTD .EQ. 4)  THEN
         WRITE(IFO)  CL,SX,SY,CO,A,B,IO
      ENDIF

      RETURN

C CALCOMP REMOTE PLOTTER
C  SET BOUNDS (PHYSICAL) FOR CLIPPING
  20  BX=BILDX
      BY=BILDY
      I=IO

C  PEN 1   MOVE AND PLOT
C  PEN 0   DO NOTHING  (MOVE)
C  PEN <0   CALL DASHED LINES

      IF(IO)2,998,3

C  DASHED
   2  S=0.2
      CALL TRAT(SX,SY,A,B,S)
      GO TO 998

C   I=1 PEN DOWN PLOT IT
C   IF OLD AND NEW OUTISIDE,DO NOTHING
   3  IF(A  .LE.  -BX.AND.SX  .LE.  -BX)GO TO 998
       IF(A  .GE.  BX.AND.SX  .GE.  BX)GO TO 998
       IF(B  .LE.  -BY.AND.SY  .LE.  -BY)GO TO 998
       IF(B  .GE.  BY.AND.SY  .GE.  BY)GO TO 998
C***D      WRITE(20,*)' PLOTCLIP',SX,SY,A,B
C   DO CLIPPING,IF NECESSARY
      IF(ABS(A).LE.BX)GO TO 5
      TN=1.
      IF(A.LT.-BX)TN=-1.
      B=(SY-B)/(SX-A)*(TN*BX-A)+B
      A=TN*BX
C***D       WRITE(20,*)' A,B-',A,B
   5  IF(ABS(SX).LE.BX)GO TO 6
      TN=1.
      IF(SX.LT.-BX)TN=-1.
      SY=(SY-B)/(SX-A)*(TN*BX-SX)+SY
      SX=TN*BX
C***D      WRITE(20,*)' SX,SY-',SX,SY
   6  IF(ABS(B).LE.BY)GO TO 7
      TN=1.
      IF(B.LT.-BY)TN=-1.
      A=(TN*BY-B)*(SX-A)/(SY-B)+A
      B=TN*BY
C***D      WRITE(20,*)' A,B-',A,B
   7  IF(ABS(SY).LE.BY)GO TO 8
      TN=1.
      IF(SY.LT.-BY)TN=-1.
      SX=(TN*BY-SY)*(SX-A)/(SY-B)+SX
      SY=TN*BY
C***D      WRITE(20,*)' SX,SY-',SX,SY
8       CONTINUE
C***4.3  CLIPPING CAN LEAD TO AN ISOLATED POINT!
C   SO CHECK AGAIN FOR A POINT OUT OF BOUNDS
      IF(A  .LE.  -BX.AND.SX  .LE.  -BX)GO TO 998
       IF(A  .GE.  BX.AND.SX  .GE.  BX)GO TO 998
       IF(B  .LE.  -BY.AND.SY  .LE.  -BY)GO TO 998
       IF(B  .GE.  BY.AND.SY  .GE.  BY)GO TO 998
C DO PLOTS
      CALL PLOT(SX,SY,3)
      CALL PLOT(A,B,2)

C SAVE POSITION.
 998  SX=A
      SY=B
      LP=2
 999  RETURN

C--------------------------------
C   PEN MARKER IS 0=UP,1=DOWN,-20=DASHED
  10  CALL etplot(SX,SY,0,0)
      CALL etplot(A,B,IO,0)
      call forceflush()
      RETURN

      END

C ********** TRAT
      SUBROUTINE TRAT(X1,Y1,X2,Y2,S)

C SUBROUTINE TRAT DRAWS A DOTTED LINE WITH STEP S ON THE PLOTTER.
C THE INITIAL VERTEX IS THE POINT (X1,Y1)
C THE FINAL VERTEX IS POINT (X2,Y2).

C   DERIVE THE PLOTTER INCREMENTS
  10  FIN=0
      CALL PLOT(X1,Y1,3)
      DX=X2-X1
      DY=Y2-Y1
      DL=SQRT(DX*DX+DY*DY)
        IF(DL.EQ.0.)RETURN

      DEPX=DX*S/DL
      DEPY=DY*S/DL
      TEST=ABS(DX)
      ADEP=ABS(DEPX)
      IF(TEST)1,1,2

    1 TEST=ABS(DY)
      ADEP=ABS(DEPY)
    2 X=X1
      Y=Y1
5     X=X+DEPX
      Y=Y+DEPY
      FIN=FIN+ADEP
      IF(FIN-TEST)11,20,20

   11 CALL PLOT(X,Y,2)
      X=X+DEPX
      Y=Y+DEPY
      FIN=FIN+ADEP
      IF(FIN-TEST)14,20,20

   14 CALL PLOT(X,Y,3)
      GO TO 5

   20 CALL PLOT(X2,Y2,2)
      call forceflush()
      RETURN

      END

C--------------------------------------------------------
      SUBROUTINE PLTTEK

C ARRANGES PLOT ON TEKTRONIX SCREEN AND DOES IT

#include "viewer.h"
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
#ifdef OSX
      integer igwid,igheight  ! for use with axiscale
#else
      integer*8 igwid,igheight  ! for use with axiscale
#endif

C SET OUTPUT DEVICE
      IOUTD=1

C SET VIEWPORT (HALF)SIZE
      BILDX=380.
      BILDY=380.

C SET SCREEN WINDOW
      call startbuffer
      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,-380.,380.,-380.,380.,xsc,ysc,sca,
     &              Xadd,Yadd)
      call linescale(igl,Xadd,sca,igb,Yadd,sca)

C DO PROJECTION
  20  CALL TRANSF

C DO PLOTTING
      CALL PERS1
      NPTEK=NPTEK+1
      call forceflush()

      RETURN
      END

C--------------------------------------------------------
C PLTFIL generates a pic file for use in forms package.
      SUBROUTINE PLTFIL
#include "viewer.h"
      
      integer lnblnk  ! function definition

      COMMON /FIL/ IFUSE, IFO
      COMMON/NNAAMM/ NAMDS,NAMPIC
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      character FNAME*72,H*72,NAMDS*30,NAMPIC*40
      logical XST

C   SET OUTPUT DEVICE, debug unit and pic file unit.
      IOUTD=4

C ONLY GET FILE ONCE I.E. DONT GET A FILE FOR EACH NEW PICTURE
 10   H(1)=' This is a binary file holding plot info to export.'
      CALL EASKS(FNAME,' Picture-file name ?',' ',
     &      72,'/tmp/viewer.pic','picture file',IER,1)
      if(ier.ne.0.or.FNAME.eq.' ')goto 10

      INQUIRE (FILE=FNAME,EXIST=XST)
      IF (XST) THEN
        OPEN (IFO,FILE=FNAME(1:lnblnk(FNAME)),FORM='UNFORMATTED',
     &          ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=ISTAT)
        call efdelet(ifo,istat)
        OPEN (IFO,FILE=FNAME(1:lnblnk(FNAME)),FORM='UNFORMATTED',
     &          ACCESS='SEQUENTIAL',STATUS='NEW',IOSTAT=ISTAT)
      ELSE
        OPEN (IFO,FILE=FNAME(1:lnblnk(FNAME)),FORM='UNFORMATTED',
     &          ACCESS='SEQUENTIAL',STATUS='NEW',IOSTAT=ISTAT)
      ENDIF

      IF(ISTAT.LT.0)GO TO 10

C   SET VIEWPORT AS TEKTRONIX    FILE BOUNDARIES SET AS +-1000
      BILDX=1000.
      BILDY=1000.

      CALL TRANSF

C----------------------
C   AND WRITE TO FILE
C   HEADER RECORD:ALL PARAMETERS

C Removed write of SCFXY to match read_viewer_pic.c
C Added idum & rdum to match namset & xy in read_viewer_pic.c
      idum=1
      rdum=1.0
      WRITE(IFO)BILDX,BILDY,BBBB,CCCC,DDDD,
     &           IPROJ,SYNVKL,NTRAT,FL,EF,NAMDS,IDUM,RDUM

C Generate image writing each line to a record in
c the form CL,SX,SY,CO,A,B,IO.
      CALL PERS1

C Write closing line of the set.
      A = 0.0
      B = 0.0
      C = 0.0
      IO = -1
      WRITE(IFO)  A,B,C,A,B,C,IO

C Inform user that pic file written (but not close as this
C is only done when the program stops.
      call edisp(iuout,' Picture done.')

      RETURN
      END

C *******
C DUMMY PLOT ROUTINES FOR CACOMP PLOTTER : CALCOMP.FRT
C NEEDED BY VIEWER WHICH CALLS FOR PLOTS
C  THIS SHOULD BE REPLACED BY ACTUAL PLOT LIBRARY, OR MODIFIED
C  TO SUIT YOUR PLOTTER

      SUBROUTINE PLOTS
C   INITIALISTAION CALL, DIFFERENT FOR DIFFERENT PLOTTING SYSTEMS
C   I.E. DEPENDS ON COMPUTER,PLOTTER TYPE & ONLINE/SPOOLED ETC.
C      CHARACTER*(*) FN
      RETURN
      END

      SUBROUTINE RCO(I,FN)
      CHARACTER*(*) FN
      RETURN
      END

      SUBROUTINE FACTOR(X)
      RETURN
      END

      SUBROUTINE PLOT(X,Y,IPEN)
      RETURN
      END

      SUBROUTINE SYMBOL(X,Y,SIZE,NAM,T,N)
      CHARACTER*(*) NAM
      RETURN
      END

      SUBROUTINE NUMBER(X,Y,SIZE,R,T,N)
      RETURN
      END

      SUBROUTINE NEWPEN(I)
      RETURN
      END
