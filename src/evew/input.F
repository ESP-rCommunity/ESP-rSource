C----------------------------------------------------
      SUBROUTINE INPUT(INIT)
#include "viewer.h"
#include "viewer.h2"
      
      integer lnblnk  ! function definition

      PARAMETER (MNF = 10)
      PARAMETER (MTR = 100)
      PARAMETER (MELT = 8)

      COMMON /FIL/ IFUSE, IFO
      COMMON/NNAAMM/ NAMDS,NAMPIC

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/MAPMAP/MAPEM,MAPR(255),MAPG(255),MAPB(255)
      COMMON/TRATRA/TRX(MNF),TRY(MNF),TRZ(MNF),
     &               TRU(MNF),TRV(MNF),TRW(MNF),TRA(MNF),
     &               SAVVIS(MNF),SAVCOL(MNF),SAVLAY(MNF)
      COMMON/TRANSC/NTR(MNF),IPTR(MNF),IPNXT,
     &       ITR(MTR),XTR(MTR),YTR(MTR),ZTR(MTR)
      common/rpath/path
      common/rcmd/LCMDFL

      DIMENSION MNP(MELT),MNS(MELT),MNPP(MELT),MNPPP(MELT),XP(4),YP(4)
      CHARACTER Filnam(MNF)*72, LINE*72,path*72,LCMDFL*144
      CHARACTER NAMDS0*30,NAMDS*30,NAMPIC*40,L144*144,lltmp*144
      CHARACTER TEXT*25
      CHARACTER TTT*3, MC*3, BLAB*3
      CHARACTER*10 SN(MELT,6)
      character FNAME*72,outs*124

      LOGICAL LOOK,NOIGN

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

      DATA MNP/8,6,6,5,4,5,4,4/
C NO OF SURFACES.
      DATA MNS/6,5,5,5,4,5,4,4/
C NO OF BASE POINTS.
      DATA MNPP/4,4,3,3,3,3,3,3/
C NO OF TOP POINTS (VERTICALS).
      DATA MNPPP/4,2,3,2,1,2,1,1/
C SURFACE DESCRIPTIONS.
      DATA (SN(1,J),J = 1,6)/' 4 2 1 5 6',' 4 3 2 6 7',
     &' 4 4 3 7 8',' 4 1 4 8 5',' 4 8 7 6 5',' 4 1 2 3 4'/
      DATA (SN(3,J),J = 1,5)/' 4 2 1 4 5',' 4 3 2 5 6',
     &' 4 1 3 6 4',' 3 6 5 4',' 3 1 2 3'/
      DATA (SN(4,J),J = 1,5)/' 4 2 1 4 5',' 3 3 2 5',' 3 1 3 4',
     &' 3 5 4 3',' 3 1 2 3'/
      DATA (SN(5,J),J = 1,4)/' 3 2 1 4',' 3 3 2 4',
     &' 3 1 3 4',' 3 1 2 3'/
      DATA (SN(6,J),J = 1,5)/' 3 2 1 5',' 3 3 2 4',' 4 1 3 4 5',
     &' 3 5 4 2',' 3 1 2 3'/
      DATA (SN(7,J),J = 1,4)/' 3 2 1 4',' 3 3 2 4',
     &' 3 1 3 4',' 3 1 2 3'/
      DATA (SN(8,J),J = 1,4)/' 3 2 1 4',' 3 3 2 4',
     &' 3 1 3 4',' 3 1 2 3'/
      DATA (SN(2,J),J = 1,5)/' 4 2 1 5 6',' 3 3 2 6',
     &' 4 4 3 6 5',' 3 1 4 5',' 4 1 2 3 4'/

      DATA NAMDS0/' VIEWER  '/

C----------------------------------------------------
      helpinsub='INPUT'  ! set for suboutine

      IF (INIT.EQ.0)THEN
        LIB = 1
        BIG = 1.0E38
        XMX = -BIG
        XMN = BIG
        YMX = -BIG
        YMN = BIG
        ZMX = -BIG
        ZMN = BIG
        ZMXP = -BIG
        ZMNP = BIG

        NF = 0
        NP = 0
        NB = 0
        NR = 0
        DO 68 I = 1,MPT
          ISIPU(I) = 0
 68     CONTINUE
      ENDIF

      NAMDS = NAMDS0
      NAMSET = 0

      ICOL = 1
      LIST = 1
      ISCLOC = 1
      ILAY = 0
      DO 25 I = 0,MLAY
        LAYER(I) = 1
 25   CONTINUE

      NTR(1) = 1
      IPNXT = 1
      IGLASS = 2
      ICOL = 7
      ISTIL = 0
      ISON = 1
      IDRWT = 1
      IDRWP = 1
      NOIGN = .TRUE.

C----------------------------------------------------
C GET FILENAME IN USE
      INUSE = IFUSE
      NFILE = 1

C Confirm command line file.
 11   helptopic='the_viewer_file'
      call gethelptext(helpinsub,helptopic,nbhelp)
      L144=LCMDFL

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
      lltmp='  '
      CALL EASKXORGTKF(L144,
     &  'Viewer input file name?',' ',
     &  'model.vew',lltmp,'viewer file',IER,nbhelp)

C If user request jump back and re-display the menu.
      if(ier.eq.-3)then
        return  ! cancel detected, return.
      endif
      if(ier.ne.0.or.lltmp(1:2).eq.'  ')then
        goto 11
      endif
      LCMDFL=lltmp

      call usrmsg(' ',' ','-')
      call fdroot(LCMDFL,path,FILNAM(NFILE))
      goto 8

C If asking for clarification after an error.
 102  FNAME=FILNAM(NFILE)
      helptopic='the_viewer_file'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKS(FNAME,' Viewer input file name ?',' ',
     &      72,'test.vew','input file',IER,nbhelp)
      if(ier.ne.0.or.FNAME(1:2).eq.'  ')then
        goto 102
      endif
      FILNAM(NFILE)=FNAME

  8   CALL EFOPSEQ(INUSE,FILNAM(NFILE),1,IER)
      if(ier.ne.0)then
        call edisp(iuout,' Problem opening input file...')
        goto 102
      endif

      WRITE(outs,'(A,A)') ' Loading file: ', FILNAM(NFILE)
      call edisp(iuout,' ')
      call edisp(iuout,outs)

      GOTO 14

C----------------------------------------------------
C         HERE TO IGNORE REST OF BAD BODY OR COMMENT LINE(S)
 13   LOOK = .FALSE.

C----------------------------------------------------

 14   READ(INUSE,141,ERR=15,END=1000)MC,TEXT
 141  FORMAT(A3,A25)

      LIB2 = 1
      IF (MC.EQ.'ON ')  GOTO  370
      IF (ISON.EQ.0)  GOTO  14
      IF (MC.EQ.'OFF')  GOTO  380

      IF (MC .EQ. 'VIS')  GOTO  320

      IF (MC.EQ. 'LAY')  GOTO  330
      IF (MC.EQ. 'LA+')  GOTO  333
      IF (MC.EQ. 'LA-')  GOTO  336

      IF (MC.EQ.'GLA')  GOTO  300
      IF (MC.EQ.'NOG')  GOTO  350

      IF (MC.EQ. 'COL')  GOTO  390
      IF (MC.EQ. 'MAP')  GOTO  392
      IF (MC.EQ.'NOL')  GOTO  340
      IF (MC.EQ.'LIS')  GOTO  342

      IF (MC.EQ.'END')  GOTO  1000

C BODY TYPES
C REGULAR SHAPE
      IF (MC.EQ.'REG')  GOTO  400
C RAGGED SHAPE =  REG WITH SLOPING BASE
      IF (MC.EQ. 'RAG')  GOTO  1400

      IF (MC.EQ.'EXT')  GOTO  4000

C ALLOW POL = REG AND ADD BASIC GOAL PRIMITIVE SHAPES
      IF (MC.EQ.'POL')  GOTO  400
      IF (MC.EQ.'REC')  GOTO  600
      IF (MC.EQ.'PRM')  GOTO  650

C GENERAL SHAPE
      IF (MC.EQ.'GEN')  GOTO  500

      IF (MC.EQ.'TIL' .AND. IDRWT.EQ.1)  GOTO  700

      IF (MC .EQ. 'PAT' .AND. IDRWP.EQ.1)  GOTO  700

      IF (MC.EQ. 'PLA')  GOTO  702

      IF (MC .EQ. 'NOT')IDRWT = 0
      IF (MC .EQ. 'NOP')IDRWP = 0

      IF (MC.EQ.'LIN')  GOTO  800

      IF (MC.EQ.'DTM')  GOTO  850
      IF (MC.EQ.'NET')  GOTO  850

      IF (MC.EQ.'COM')  GOTO 13
      IF (MC.EQ.'NAM')  GOTO  360

      IF (MC.EQ.'   ')  GOTO  14

      IF (MC.EQ.'FIL')  GOTO  250

      IF ( MC .EQ. 'TRA')go to 270

      IF (MC .EQ. 'CLO')THEN
        ISCLOC = 1
        GOTO  14
      ELSE IF (MC .EQ. 'ANT')THEN
        ISCLOC = 0
        GOTO  14
      ENDIF

 15   IF (LOOK)CALL BERROR(10,NB)
      GOTO  13

C----------------------------------------------------
C FILE COMMAND

 250  CONTINUE
      SAVVIS(NFILE) = IGLASS
      SAVCOL(NFILE) = ICOL
      SAVLAY(NFILE) = ILAY

      NFILE = NFILE+1
      READ(INUSE,260,ERR=15,END=1000)FILNAM(NFILE)
 260  FORMAT(A)

      READ(INUSE,*,ERR=15,END=1000)FRX,FRY,FRZ,FRU,FRV,FRW,FRA
      TRX(NFILE) = FRX
      TRY(NFILE) = FRY
      TRZ(NFILE) = FRZ
      TRU(NFILE) = FRU
      TRV(NFILE) = FRV
      TRW(NFILE) = FRW
      TRA(NFILE) = FRA

C SAVE IN TRANSFORM STACK
      NTR(NFILE) = 4
      IPTR(NFILE) = IPNXT

C Set origin
      IP = IPNXT
      ITR(IP) = 0
      XTR(IP) = 0.
      YTR(IP) = 0.
      ZTR(IP) = 0.
C Scale
      IP = IP+1
      ITR(IP) = 1
      XTR(IP) = FRU
      YTR(IP) = FRV
      ZTR(IP) = FRW

C Rotate
      IP = IP+1
      ITR(IP) = 3
      XTR(IP) = 0.
      YTR(IP) = 0.
      ZTR(IP) = FRA

C Move by
      IP = IP+1
      ITR(IP) = 2
      XTR(IP) = FRX
      YTR(IP) = FRY
      ZTR(IP) = FRZ
      IPNXT = IP+1

      INUSE = INUSE+1
      GOTO  8

C----------------------------------------------------
C TRANSFORM COMMAND
C SUSPENDS CURRENT FILE READS AND READS THE NEW FILE
C UNTIL END , AND THEN REVERTS IE FILES CAN BE NESTED
C ALSO READ TRANSFORMS (XYZ,UVW, R) TO ALLOW TRANSFORM
C OF THE COORDS IN NEW FILE. FORMAT IS:
C          TRA
C          filename
C          number of transforms
C          list of transforms <TTT x y z>
C          -each transform is one of
C           ORI x y z    transform origin is moved to x y z
C           SCA x y z    scale by x y z factors about ORIGIN
C           MOV  x y z   translate by x y z
C           ROT  x y z   rotate about xyz  ORIGin by x,y,z deg
 270  continue

C SAVE CURRENT VIS AND COL SETTINGS AND LAYER
      SAVVIS(NFILE) = IGLASS
      SAVCOL(NFILE) = ICOL
      SAVLAY(NFILE) = ILAY

      NFILE = NFILE+1

C READ NEW FILENAME
      READ(INUSE,260,ERR=15,END=1000)FILNAM(NFILE)

C READ TRANSFORM DATA
      READ(INUSE,*,ERR=15,END=1000)NTR(NFILE)
      IPTR(NFILE) = IPNXT
      DO 280 KK = 1,NTR(NFILE)
         IP = IPNXT+KK-1

C READ DATA TO CHARACTER ARRAY
         READ(INUSE,285,ERR=15,END=1000)line
 285     FORMAT(A)
         TTT = LINE(1:3)
         READ(line(4:),*)XTR(IP),YTR(IP),ZTR(IP)
         IF (TTT.EQ.'ORI')THEN
            ITR(IP) = 0
         ELSEIF (TTT.EQ.'SCA')THEN
            ITR(IP) = 1
         ELSEIF (TTT.EQ.'MOV')THEN
            ITR(IP) = 2
         ELSEIF (TTT.EQ.'ROT')THEN
            ITR(IP) = 3
         ENDIF
 280  CONTINUE
      IPNXT = IPNXT+NTR(NFILE)
      INUSE = INUSE+1
      GOTO  8

C----------------------------------------------------
C SET GLASS ON:ALL BODIES GLASS FROM NOW ON
 300  IGLASS = -3
      IF (LIST.EQ.1)call edisp(iuout,'GLASS')
      GOTO  14

C----------------------------------------------------
C SET GLASS OFF:ALL BODIES NOT GLASS FROM NOW ON
 350  IGLASS = 2
      IF (LIST.EQ.1)call edisp(iuout,'NO-GLASS')
      GOTO  14

C----------------------------------------------------
C  SET NEW VISIBILITY INDEX
C  ABSOLUTE VLAUE OF
C   VIS INDEX  = 0  DRAW NONE OF EDGES
C              = 1  DRAW ONLY SILHOUETTE EDGES
C              = 2  DRAW FRONT EDGES (NORMAL CASE)
C              = 3  DRAW ALL EDGES (GLASS)
C  AND    >0  THSI BODY HIDES OTHERS
C         <0  THIS BODY CANNOT HIDE OTHERS
 320  READ(INUSE,*,ERR=14,END=600)IGLASS
      IF (LIST.EQ.1)then
        WRITE(outs,'(A,I4)')  'VISIBILITY',IGLASS
        call edisp(iuout,outs)
      endif
      GOTO  14

C----------------------------------------------------
C SET CURRENT LAYER : DEFAULT SETTING IS ZERO
 330  READ(INUSE,*,ERR=14,END=600)ILAY
      IF (LIST.EQ.1)then
        WRITE(outs,'(A,I4)')  'LAYER',ILAY
        call edisp(iuout,outs)
      endif
      GOTO  14
C SET LAYERS ON OR OFF
C   LA+ SETS THEM ON
C   LA- SETS THEM OFF
 333  CALL SELECT(INUSE,0,MLAY,LAYER,1)
      GOTO  14
 336  CALL SELECT(INUSE,0,MLAY,LAYER,0)
      GOTO  14

C----------------------------------------------------
C SET LISTING ON OR OFF
 340  LIST = 0
      GOTO  14
 342  LIST = 1
      GOTO  14

C----------------------------------------------------
C READ IN DATASET NAME FROM FILE AND SAVE
  360 READ(INUSE,110,ERR=13,END=600)NAMDS
 110  FORMAT(A)
      IF (LIST.EQ.1)then
        WRITE(outs,'(A,A)')  'NAME :',NAMDS
        call edisp(iuout,outs)
      endif
      NAMSET = 1
      GOTO  14

C----------------------------------------------------
C SET ON/OFF SWITCH TO ON
 370  ISON = 1
      GOTO  14

C----------------------------------------------------
C SET ON/OFF SWITCH TO OFF
 380  ISON = 0
      GOTO  14

C----------------------------------------------------
C SET CURRENT BODY COLOUR
 390  READ(INUSE,*,ERR=401,END=999)ICOL
      IF (LIST.EQ.1)then
        WRITE(outs,'(A,I4)')  'COLOUR',ICOL
        call edisp(iuout,outs)
      endif
      GOTO  14

C SET CURRENT MAP COLOURS
C READ IN MAPSIZE(NUMBER OF COLOURS)  AND INDEX,R,G,B
 392  READ (INUSE,*,ERR=401,END=999)NCMAP
      DO 395 I = 1,NCMAP
           READ(INUSE,*,ERR=401,END=999)KI,KR,KG,KB

C AND SAVE THEM
           MAPR(KI) = KR
           MAPG(KI) = KG
           MAPB(KI) = KB
C AND SET THEM AS THE DEFAULT LINE INDICES
C <<           CALL DCLDEF(KI,KR,KG,KB)
C   WHICH DOES THE SETTING DEPENDING ON THE SCREEN-DEVICE
 395   CONTINUE
       GOTO 14

C----------------------------------------------------
C ADD EXTRUDED BODY (EXT) AS REG
C BUT ALLOW EXTR ABOUT AXIS 1 = Z( = REG)  2 = X  3 = Y
C BUT ALLOW EXTR ABOUT AXIS 1 = X  2 = Y  3 = Z( = REG)

 4000 CONTINUE

      BLAB = 'EXT'

      CALL  EXT(NFILE,INUSE,ISCLOC,TEXT,LOOK)

      IF (LIST.EQ.1)WRITE(IUOUT,904)NB,BLAB,TEXT

      IBODG(NB) = IGLASS
      IBCOL(NB) = ICOL
      IBLAY(NB) = ILAY

      GOTO  14

C----------------------------------------------------
C          REG   REGULAR BODY
C   ALSO = POL   GENERAL 2D POLYGON,EXTRUDED VERTICALLY

C   REGUALR BODY IS DEFINED BY
C    NO OF SIDES,Z-HT OF FLOOR,Z-HT OF ROOF
C   X,Y COORDS OF VERTICES,CLOCKWISE FORM ABOVE

C  REG IS IEXT  = 1
 400  IEXT = 1
      J1 = 1
      J2 = 2
      J3 = 3
      J4 = 1
      BLAB = 'REG'
      NB = NB+1
      IF (NB.GT.MBB)  GOTO  415
      IF (LIST.EQ.1)then
        WRITE(outs,904)NB,BLAB,TEXT
 904    FORMAT(' Body',I4,1X,A3,A25)
        call edisp(iuout,outs)
      endif
      LOOK = .TRUE.
      READ(INUSE,*,ERR=401,END=999)NS,Z0,Z1
      NP1 = NP+1
      NP2 = NP+NS

C DO ERROR CHECKS FOR MPT,MSF,MED IN BODY
      NF1 = NF+1

C CLOCKWISE OR ANTICLOCKWISE?
C POINTS ALWAYS STORED CLOCKWISE
      IF (ISCLOC .EQ.1)THEN
      READ(INUSE,*,ERR=404,END=999)((PUNT(I,J),J=J1,J2,J4),I=NP1,NP2)
      ELSE
      READ(INUSE,*,ERR=404,END=999)((PUNT(I,J),J=J1,J2,J4),I=NP2,NP1,-1)
      ENDIF

C--------------------
C MAKE BOTTOM PLANE,3D COORDS AND PLANE DEFINITION

      DO 410 I = NP1,NP2
 410  PUNT(I,J3) = Z0
      NF2 = NF+1
      NRB = NP2+1
      LIB1 = LIB+NS-1
C--------------------
      DO 420 I = LIB,LIB1
        NRB = NRB-1
 420  ISIDOR(I) = NRB
      CALL VERT(NF2,*13)

C--------------------
C IF BODY IS THIN,OMIT SIDE PLANES
      IF (ABS(Z1-Z0).LE.0.001*ABS(Z1+Z0))  GOTO  450

C   MAKE TOP PLANE COORDS 3D
      NP3 = NP2+1
      NP2 = NP2+NS
      DO 425 I = NP3,NP2
        J = I-NS
        PUNT(I,J1) = PUNT(J,J1)
        PUNT(I,J2) = PUNT(J,J2)
  425 PUNT(I,J3) = Z1
      CALL PCONVT(NFILE,NP1,NP2)

C--------------------
C MAKE SIDE PLANE DEFINITIONS

      NRB = NP1
      NRT = NP3
      DO 430 I = 1,NS
        NF2 = NF2+1
        LIB1 = LIB+3
        ISIDOR(LIB) = NRB
        ISIDOR(LIB+1) = NRB+1
        ISIDOR(LIB+2) = NRT+1
        ISIDOR(LIB+3) = NRT
        IF (I.EQ.NS)ISIDOR(LIB+1) = NP1
        IF (I.EQ.NS)ISIDOR(LIB+2) = NP3
        CALL VERT(NF2,*13)

C--------------------
C MAKE TOP PLANE DEFINITION
            NRB = NRB+1
 430  NRT = NRT+1
      GOTO  451

C << 450 no path to this >>
 450  NP3 = NP1
 451  NF2 = NF2+1
      NRT = NP3-1
      LIB1 = LIB+NS-1
      DO 460 I = LIB,LIB1
        NRT = NRT+1
 460  ISIDOR(I) = NRT
      CALL VERT(NF2,*13)
 
      GOTO  2000

C----------------------------------------------------
C RAG   RAGGED BODY  = REGULAR BODY WITH SLOPING BASE

1400  IEXT = 1
      J1 = 1
      J2 = 2
      J3 = 3
      J4 = 1
      BLAB = 'RAG'
      NB = NB+1
      IF (NB.GT.MBB)  GOTO  415
      IF (LIST.EQ.1)then
        WRITE(outs,904)NB,BLAB,TEXT
        call edisp(iuout,outs)
      endif
      LOOK = .TRUE.
      READ(INUSE,*,ERR=401,END=999)NS,Z1
      NP1 = NP+1
      NP2 = NP+NS

C DO ERROR CHECKS FOR MPT,MSF,MED IN BODY
      NF1 = NF+1

C CLOCKWISE OR ANTICLOCKWISE?
C POINTS ALWAYS STORED CLOCKWISE
      IF (ISCLOC .EQ.1)THEN
      READ(INUSE,*,ERR=404,END=999)((PUNT(I,J),J=1,3),I=NP1,NP2)
      ELSE
      READ(INUSE,*,ERR=404,END=999)((PUNT(I,J),J=1,3),I=NP2,NP1,-1)
      ENDIF

C--------------------
C MAKE BOTTOM PLANE,3D COORDS AND PLANE DEFINITION

      NF2 = NF+1
      NRB = NP2+1
      LIB1 = LIB+NS-1
C--------------------
      DO 1420 I = LIB,LIB1
        NRB = NRB-1
1420  ISIDOR(I) = NRB
      CALL VERT(NF2,*13)

C--------------------
C MAKE TOP PLANE COORDS 3D
      NP3 = NP2+1
      NP2 = NP2+NS
      DO 1425 I = NP3,NP2
        J = I-NS
        PUNT(I,J1) = PUNT(J,J1)
        PUNT(I,J2) = PUNT(J,J2)
 1425 PUNT(I,J3) = Z1
      CALL PCONVT(NFILE,NP1,NP2)

C--------------------
C MAKE SIDE PLANE DEFINITIONS

      NRB = NP1
      NRT = NP3
      DO 1430 I = 1,NS
        NF2 = NF2+1
        LIB1 = LIB+3
        ISIDOR(LIB) = NRB
        ISIDOR(LIB+1) = NRB+1
        ISIDOR(LIB+2) = NRT+1
        ISIDOR(LIB+3) = NRT
        IF (I.EQ.NS)ISIDOR(LIB+1) = NP1
        IF (I.EQ.NS)ISIDOR(LIB+2) = NP3
        CALL VERT(NF2,*13)
        NRB = NRB+1
1430  NRT = NRT+1
      GOTO  1451

C--------------------
C MAKE TOP PLANE DEFINITION (no path statments commented out and 1451 moved up)
C      NP3 = NP1
C1451  NF2 = NF2+1
1451  NP3 = NP1
      NF2 = NF2+1
      NRT = NP3-1
      LIB1 = LIB+NS-1
      DO 1460 I = LIB,LIB1
        NRT = NRT+1
1460  ISIDOR(I) = NRT
      CALL VERT(NF2,*13)

      GOTO  2000

C----------------------------------------------------
C GEN  GENERAL BODY

C DEFINED BY:NO OF VERTICES,NO OF SURFACES
C X,Y,Z COORDS OF VERTICES (IN ANY ORDER)
C FACE DEFINITIONS IN TERMS OF VERTEX LISTS
C DEFINED CLOCKWISE FROM OUTSIDE BODY

 500  NB = NB+1
      IF (NB.GT.MBB)  GOTO  415
      LOOK = NOIGN
      IF (LIST.EQ.1)then
        WRITE(outs,905)NB,TEXT
 905    FORMAT(' Body',I4,' GEN',A25)
        call edisp(iuout,outs)
      endif
      READ(INUSE,*,ERR=401,END=999)IP,IF

C*** 4.5  allow number of surfaces = 0
C if if = 0 this is taken as a LIN body
      IF (IF.eq.0)  GOTO  820

      IF (IP.GT.0.AND.IF.GT.0)  GOTO  510

      GOTO 401

 510  NP2 = NP+IP
      NP1 = NP+1
      READ(INUSE,*,ERR=404,END=999)((PUNT(I,J),J = 1,3),I = NP1,NP2)
      NF2 = NF+IF
      NF1 = NF+1
      CALL PCONVT(NFILE,NP1,NP2)
C--------------------
      DO 540 I = NF1,NF2
C***5.4 CLOCKWISE OR ANTICLOCKWISE?
C POINTS ALWAYS STORED CLOCKWISE
        READ(INUSE,*,ERR=401,END=999)LL,(ISIDOR(K),K=LIB,LIB+LL-1)
        IF (LL .LE. 0)  GOTO  401
        LIB1 = LIB+LL-1
        IF (ISCLOC .NE. 1)  CALL  INVERT
        DO 535 K = LIB,LIB1
                ISIDOR(K) = ISIDOR(K)+NP
 535    CONTINUE
        II = I
        CALL VERT(II,*13)
 540  CONTINUE

 2000 CONTINUE

      CALL BODY(NP1,NP2,NF1,NF2)

      IBODG(NB) = IGLASS
      IBCOL(NB) = ICOL
      IBLAY(NB) = ILAY

      GOTO  14

C----------------------------------------------------
C***5.0  ALLOW GOAL PRIMITIVE SHAPES INCL REC(tangle)
C    REC = RECTANGULAR BLOCK
C          X,Y,Z,DX,DY,DZ,A
C    PRM = PRIMITIVE SHAPE FOR GOAL, BASED ON RECT AND TRIANGLE
C        = SHAPE TYPE 0-10
C          X,Y,Z,DX,DY,DZ,A

 600  NB = NB+1
      IF (NB.GT.MBB)  GOTO  415
      IF (LIST.EQ.1)then
        WRITE(outs,909)NB,MC,TEXT
 909    FORMAT(' Body',I4,1X,A3,A25)
        call edisp(iuout,outs)
      endif
      LOOK = .TRUE.
      READ(INUSE,*,ERR=401,END=999)X,Y,Z,DX,DY,DZ,ANG

C MAKE COORDS AND PLANES
      IPRM = 0
      GOTO  660

C PRM
  650  NB = NB+1
      IF (NB.GT.MBB)  GOTO  415
      LOOK = .TRUE.
      READ(INUSE,*,ERR=401,END=999)IPRM
      WRITE(outs,910)NB,MC,IPRM,TEXT
 910  FORMAT(' Body',I4,1X,A3,I1,A25)
      call edisp(iuout,outs)
      READ(INUSE,*,ERR=404,END=999)X,Y,Z,DX,DY,DZ,ANG

C MAKE COORDS AND SURFACES

 660  II = IPRM+1
      IF (II.LT.1)  GOTO  14

      KNS = MNS(II)
      KPP = MNPP(II)
      KPPP = MNPPP(II)
      ZF = Z
      ZR = Z+DZ

C 4 BASIC COORDINATES ANTICLOCKWISE FROM ABOVE
      XA = ANG
      CA = ECOSD(XA)
      SA = ESIND(XA)
      XP(1) = X
      YP(1) = Y
      XP(2) = XP(1)+DX*CA
      YP(2) = YP(1)+DX*SA
      XP(3) = XP(2)-DY*SA
      YP(3) = YP(2)+DY*CA
      XP(4) = XP(1)-DY*SA
      YP(4) = YP(1)+DY*CA

C IF TYPE GT 1:TRIANGULAR
      IF (II.LE.2)  GOTO  640

      XP(3) = XP(4)
      YP(3) = YP(4)

C WRITE POINTS
C BOTTOM POINTS
 640  NP1 = NP+1
      NP2 = NP+KPP
      DO 630 I = 1,KPP
        PUNT(I+NP,1) = XP(I)
        PUNT(I+NP,2) = YP(I)
        PUNT(I+NP,3) = ZF
 630  CONTINUE

C WRITE TOP POINTS
C NUMBERING IS ALTERED FOR TYPES 5,6,7
      IF (II.LT.6)  GOTO  644
      GOTO  (644,644,644,644,644,641,642,643),II

 641  XP(2) = XP(1)
      YP(2) = YP(1)
      XP(1) = XP(3)
      YP(1) = YP(3)
      GOTO  644

 642  XP(1) = XP(2)
      YP(1) = YP(2)
      GOTO  644

 643  XP(1) = XP(3)
      YP(1) = YP(3)
 644  CONTINUE
      NP2 = NP2+KPPP
      DO 680 I = 1,KPPP
        PUNT(I+KPP+NP,1) = XP(I)
        PUNT(I+KPP+NP,2) = YP(I)
        PUNT(I+KPP+NP,3) = ZR
 680  CONTINUE
      CALL PCONVT(NFILE,NP1,NP2)

C      WRITE(IUOUT,*)(I,(PUNT(I,K),K = 1,3),I = NP1,NP2)
C WRITE SURFACE DESCRIPTIONS
      NF1 = NF+1
      NF2 = NF+KNS
      DO 645 J = 1,KNS

C READ SURFACE DATA FROM SN
C internal file read is now formatted
C CLOCKWISE OR ANTICLOCKWISE?
C POINTS ALWAYS STORED CLOCKWISE
C      IF (ISCLOC .EQ.1)THEN
           READ(SN(II,J),646)LL,(ISIDOR(K),K = LIB,LIB+LL-1)
C      ELSE
C           READ(SN(II,J),646)LL,(ISIDOR(K),K = LIB+LL-1,LIB,-1)
C      ENDIF
 646       FORMAT(5I2)
           LIB1 = LIB+LL-1
           DO 647 K = LIB,LIB1
                ISIDOR(K) = ISIDOR(K)+NP
 647       CONTINUE
           KK = J+NF
C      WRITE(IUOUT,*)KK,(ISIDOR(K),K = LIB,LIB1)
           CALL VERT(KK,*13)
 645  CONTINUE

      GOTO  2000

C----------------------------------------------------

C   TIL  : TILE BODY DEFINED BY
C   NO OF VERTICES
C   XYZ COORDINATE LIST OF THE PLANE
C   AS VIEWD FROM 'OUTSIDE' ADJACENT BODY
C   TILE IS TAKEN TO BE GLASS
C   PLA-NE BODY AS TILE BUT NOT GLASS

C   VIEWER EXPANDS THE DEFINTION TO THE NORMAL GEN SHAPE
C   WITH 2 SIDES,A ND SETS THE BODY TO BE GLASS IF TILE

C TILE IS GLASS
 700  ISGLA = -3
      GOTO  710
C PLANE IS NOT GLASS
 702  ISGLA = IGLASS
 710  CONTINUE
      NB = NB+1
      IF (NB.GT.MBB)  GOTO  415
      LOOK = .TRUE.
      IF (LIST.EQ.1)then
        WRITE(outs,907)NB,MC,TEXT
 907    FORMAT(' Body',I4,1X,A3,A25)
        call edisp(iuout,outs)
      endif

C--------------------
      READ(INUSE,*,ERR=401,END=999)IP
      IF (IP.LE.0)  GOTO  401
      NP1 = NP+1
      NP2 = NP+IP
      READ(INUSE,*,ERR=404,END=999)((PUNT(I,J),J = 1,3),I = NP1,NP2)

      CALL PCONVT(NFILE,NP1,NP2)
      NF2 = NF+1
      NF1 = NF+1

C--------------------
C SET SURFACE DEFINITION
      NRB = NP2+1
      LIB1 = LIB+IP-1
      DO 720 I = LIB,LIB1
           NRB = NRB-1
 720       ISIDOR(I) = NRB
      CALL VERT(NF2,*13)

C--------------------
C SET FRONT SURFACE DEF.
      NF2 = NF2+1
      NRT = NP1-1
      LIB1 = LIB+IP-1
      DO 770 I = LIB,LIB1
        NRT = NRT+1
 770  ISIDOR(I) = NRT
      CALL VERT(NF2,*13)

C SET LAST BODY WAS TILE
      ISTIL = 1

      GOTO  2000

C----------------------------------------------------
C LIN

 800  NB = NB+1
      IF (NB.GT.MBB)  GOTO  415
      LOOK = .TRUE.
      IF (LIST.EQ.1)then
        WRITE(outs,'(A,I3,A,A)') 'Body',NB,' LIN',TEXT
        call edisp(iuout,outs)
      endif
      READ(INUSE,*,ERR=401,END=999)IP
      IF (IP.LE.1)  GOTO  401

C*** here from GEN if ip = 0
 820  NP1 = NP+1
      NP2 = NP+IP
      IF (NP2.GT.MPT)CALL BERROR(1,NB)

      READ(INUSE,*,ERR=404,END=999)((PUNT(I,J),J = 1,3),I = NP1,NP2)

      CALL PCONVT(NFILE,NP1,NP2)
      NR1 = IP-1
      N1 = NR+1
      NR = NR+NR1
      IBE1(NB) = N1
      IBE2(NB) = NR
      IF (NR.GT.MED)CALL BERROR(3,NB)

      IES(NB) = 1

      DO 830 I = 1,NR1
        II = N1+I-1
        JJ = I+NP1-1

        IREPU1(II) = JJ
        IREPU2(II) = JJ+1
        IREFA1(II) = 0
        IREFA2(II) = 0
        IREFAC(II) = 0

        ISIPU(JJ) = 0
 830  CONTINUE

      ISIPU(NP2) = 0

      IBF1(NB) = 0
      IBF2(NB) = NF

      IBP1(NB) = NP1
      IBP2(NB) = NP2

      NP = NP2

      CALL SHRINK(NP1,NP2)
      IBODG(NB) = -3
      IBCOL(NB) = ICOL
      IBLAY(NB) = ILAY

      GOTO  14

C----------------------------------------------------
C   DTM/NET COMMAND
C    FOR INPUT OF A GRIDDED SET OF  Z-VALUES
C     FORMAT IS
C      DTM  (OR NET IS THE SAME)
C      N M DX DY X Y Z
C      WHERE   N = NO OF ROWS
C              M = NO OF COLUMNS (Y)
C              DX = GRID SIZE IN X
C              DY = GRID SIZE IN Y
C              X,Y,Z IS ORIGIN
C      z-VALUES IN N BY M MATRIX
C        STARTING AT LOWER LEFT (THE ORIGIN)
C         UNLESS DX OR DY ARE NEGATIVE

 850  IF (LIST.EQ.1)then
         WRITE(outs,'(A,I3,A,A)') 'Body',NB+1,' NET',TEXT
         call edisp(iuout,outs)
      endif
      READ(INUSE,*,ERR=401,END=999) NPX,NPY,DNX,XOR,YOR,ZOR
      DNY = DNX

C SAVE PREVIOUS POINTS
      NP0 = NP

C Do all the rows.
      do 870 IY = 1,NPY

        NB = NB+1
        IF (NB.GT.MBB)  GOTO  415
        LOOK = .TRUE.

        NP1 = NP+1
        NP2 = NP+NPX
        IF (NP2.GT.MPT)CALL BERROR(1,NB)

C READ row of z-values.
        READ(INUSE,*,ERR=404,END=999)(PUNT(I,3),I = NP1,NP2)

C CALCULATE CORRESPONDING X AND Y
        DO 875 IX = 1,NPX
          PUNT(NP+IX,2) = YOR+(iy-1)*DNY
          PUNT(NP+IX,1) = XOR+(IX-1)*DNX
          PUNT(NP+IX,3) = ZOR+PUNT(NP+IX,3)
 875    continue

        CALL PCONVT(NFILE,NP1,NP2)
        NR1 = npx-1
        N1 = NR+1
        NR = NR+NR1
        IBE1(NB) = N1
        IBE2(NB) = NR
        IF (NR.GT.MED)CALL BERROR(3,NB)

        IES(NB) = 1

        DO 877 I = 1,NR1

          II = N1+I-1
          JJ = I+NP1-1

          IREPU1(II) = JJ
          IREPU2(II) = JJ+1
          IREFA1(II) = 0
          IREFA2(II) = 0
          IREFAC(II) = 0

          ISIPU(JJ) = 0
 877    CONTINUE

        ISIPU(NP2) = 0

        IBF1(NB) = 0
        IBF2(NB) = NF

        IBP1(NB) = NP1
        IBP2(NB) = NP2

        NP = NP2

        CALL SHRINK(NP1,NP2)
        IBODG(NB) = -3
        IBCOL(NB) = ICOL
        IBLAY(NB) = ILAY
 870  CONTINUE

      READ(INUSE,*,ERR=401,END=999) IX

C Do the columns.
      do 880 IX = 1,NpX

        NB = NB+1
        IF (NB.GT.MBB)  GOTO  415
        LOOK = .TRUE.

        NP1 = NP+1
        NP2 = NP+NPY
        IF (NP2.GT.MPT)CALL BERROR(1,NB)

C Use previuosly READ row of z-values.

C CALCULATE CORRESPONDING X AND Y
        DO 885 IY = 1,NPY
          IPP = NP0+(IY-1)*NPX+IX
          PUNT(NP+IY,1) = punt(ipp,1)
          PUNT(NP+IY,2) = punt(ipp,2)
          PUNT(NP+IY,3) = PUNT(ipp,3)
 885    CONTINUE

        NR1 = NPY-1
        N1 = NR+1
        NR = NR+NR1
        IBE1(NB) = N1
        IBE2(NB) = NR
        IF (NR.GT.MED)CALL BERROR(3,NB)

        IES(NB) = 1

        DO 887 I = 1,NR1
          II = N1+I-1
          JJ = I+NP1-1

          IREPU1(II) = JJ
          IREPU2(II) = JJ+1
          IREFA1(II) = 0
          IREFA2(II) = 0
          IREFAC(II) = 0

          ISIPU(JJ) = 0
 887    CONTINUE

        ISIPU(NP2) = 0

        IBF1(NB) = 0
        IBF2(NB) = NF

        IBP1(NB) = NP1
        IBP2(NB) = NP2
        NP = NP2
        CALL SHRINK(NP1,NP2)
        IBODG(NB) = -3
        IBCOL(NB) = ICOL
        IBLAY(NB) = ILAY
 880  CONTINUE

      GOTO  14

C------------------------------------------------------------------
C   END

1000  CONTINUE
      CALL ERPFREE(INUSE,ISTAT)

      NFILE = NFILE-1
      IF (NFILE.GT.0) THEN
        IPNXT = IPTR(NFILE)+NTR(NFILE)
        IF (NFILE.EQ.1)  IPNXT = 1

        INUSE = INUSE-1
        IGLASS = int(SAVVIS(NFILE))
        ICOL = int(SAVCOL(NFILE))
        ILAY = int(SAVLAY(NFILE))
        ISON = 1
        GOTO  14
      ENDIF

C----------------------------------------------------
C  IF NP,NF,NR,DO NOT SATISFY EULER RELATION, 
C  THE EDGE GRAPH IS NOT CONNECTED,TH
C  AT IS, IF NO CONCAVE EDGES WERE FOUND, WE HAVE MORE THAN ONE OBJECT.
C  DONT CHECK EULER,WE DO IT DIFFERENTLY IN PERS1 NOW
C600   IF ((NP+NF).NE.(NR+2))IES = 0
      if (NB.EQ.0)then
        call edisp(iuout,' NO BODIES in DATA FILE ')
      endif

      write(outs,'(A,A)')' File name     : ',NAMDS
      call edisp(iuout,outs)
      write(outs,'(a,I5,a,I5,a,I5,a,I5,a,I5)')' with ',NB,' bodies ',
     &      NP,' points ',NF,' surfaces and ',NR,' edges.'
      call edisp(iuout,outs)
      write(outs,'(A,2F8.1)')' Bounding coords : X ',XMN,XMX
      call edisp(iuout,outs)
      write(outs,'(18X,A,2F8.1)')'Y ',YMN,YMX
      call edisp(iuout,outs)
      write(outs,'(18X,A,2F8.1)')'Z ',ZMN,ZMX
      call edisp(iuout,outs)
      return

C----------------------------------------------------
C READ BERRORS
 401  CALL BERROR(9,NB)
      GOTO 13
 404  CALL BERROR(8,NB)
      GOTO 13

C MAX BODIES EXCEEDED PROCEED WITH WHAT WE HAVE,IGNORE REST OF FILE
 415  CALL BERROR(15,NB)
      return

  999 CALL BERROR(13,NB)
      STOP

      END
