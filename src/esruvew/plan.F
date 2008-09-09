C------------------------
          SUBROUTINE PLAN
C------------------------
#include "viewer.h"
#include "viewer.h2"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GFONT/IFS,ITFS,IMFS
      character outs*80

      LOGICAL J1J2
      IF(MMOD.LT.0)RETURN
      call startbuffer()
      call axiscale(igw,igwh,XMN,XMX,YMN,YMX,xsc,ysc,sca,Xadd,Yadd)
      scap=sca*0.2
      addx=(FLOAT(igw)*0.4)/scap
      addy=(FLOAT(igwh)*0.4)/scap
      call linescale(igl,addx,scap,igb,addy,scap)

      X1=XMN-1.
      Y1=YMN-1.
      DO 40 I=1,NR

        J2=IREPU1(I)
        J3=IREPU2(I)

        X2=PUNT(J2,1)
        Y2=PUNT(J2,2)
        X3=PUNT(J3,1)
        Y3=PUNT(J3,2)
        J1J2=X2.EQ.X1 .AND. Y2.EQ.Y1
        IF(X3.EQ.X1 .AND. Y3.EQ.Y1)GOTO 30

        IF(J1J2)GOTO 10

        IF(X3.EQ.X2 .AND. Y3.EQ.Y2)GOTO 40

        CALL etplot(X2,Y2,0,0)
10      CALL etplot(X3,Y3,1,0)
20      X1=X3
        Y1=Y3
        GOTO 40

30      IF(J1J2)GOTO 40

        CALL etplot(X2,Y2,1,0)
        X1=X2
        Y1=Y2
40      CONTINUE

C   PRINT Z LIMITS
      write(outs,'(A,F6.2,A,F6.2)')'Zmin=',ZMNP,' Zmax=',ZMXP
      call edisp(iuout,outs)

C   LABEL PLAN WITH E,F,M
      IPLN=1
      call etlabel('Eye',DDDD(1),DDDD(2),0,ifs)
      call etlabel('F',CCCC(1),CCCC(2),0,ifs)
      call etlabel('M',BBBB(1),BBBB(2),0,ifs)
      call forceflush()

      RETURN
      END
