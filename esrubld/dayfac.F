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

C      DAYFAC
C      ANGS
C      SCOMP
C      ICOMP

C ***************  DAYFAC ***************
C Compute daylight factor (sky component + externally reflected component
C + internally reflected component at specified point(s) on working plane.
C Initiate calc. of daylight factor(s) for standard CIE overcast sky.

      SUBROUTINE  DAYFAC(ICOMP)
#include "building.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/CGCIN2/NCGTC(MCOM,3),NLITZ(MCOM),IDFST(MCOM,MLCOM),
     & CGX(MCOM,MLCOM,MDF),CGY(MCOM,MLCOM,MDF),CGH(MCOM,MLCOM,MDF),
     & UX(MCOM,MLCOM,MDF),UY(MCOM,MLCOM,MDF),UH(MCOM,MLCOM,MDF),
     & SETPT(MCOM,MLCOM),SYSPER(MCOM,MLCOM),SOFFLL(MCOM,MLCOM),
     & IOFFDT(MCOM,MLCOM),SMLOUT(MCOM,MLCOM),SMEOUT(MCOM,MLCOM)
      COMMON/CGCIN3/ICGCS(MCOM),ICGCF(MCOM),
     & ICGCFL(MCOM,MLCOM),SPELEC(MCOM,MLCOM),SLOPEM(MCOM,MLCOM)
     
      COMMON/DAYF/NDF(MCOM,MLCOM),DFDAT(MCOM,MLCOM,MGT,MDF),
     & AZIJL(MCOM,MGT),SUMDF(MCOM,MLCOM,MDF),NUDWIN(MCOM)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/D7/T1,T2,P1,P2

      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)

      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)

      DIMENSION XPL(MV),YPL(MV),ZPL(MV),DF(MGT)
      CHARACTER*10 WINFO
      CHARACTER*25 CSTR
      logical close,closea,closeb,closec,closed,closee,closef
      logical closeg,closeh,closei

C For conversion from radians to degrees.
      PI = 4.0 * ATAN(1.0)
      TODEG=180/PI

c Trace output ? Use same slot as zone casual gains.
      ITFLG=0
      IF(ITC.EQ.1.OR.(ITRACE(12).EQ.1.AND.IZNTRC(ICOMP).EQ.1))ITFLG=1
      IF(ITFLG.EQ.1)THEN
        WRITE(ITU,9996)ICNT,ICOMP
 9996   FORMAT(' Subroutine DAYFAC   Trace output',I4,5X,'Zone',I3 /)
        ICNT=ICNT+1
        WRITE(ITU,9989)(NCGTC(ICOMP,I),I=1,3)
 9989   FORMAT(' Casual gain type to control during Weekdays  = ',I3,/,
     &   29X,'during Saturdays = ',I3,/,29X,'during Sundays   = ',I3,/)
        DO 9992 N=1,NLITZ(ICOMP)
        WRITE(ITU,9995)
 9995   FORMAT(/,' Light| Start| Finish| Control|' /
     &           ' Zone | Hour | Hour  | Type   |' /)
          WRITE(ITU,9993)N,ICGCS(ICOMP),ICGCF(ICOMP),
     &    ICGCFL(ICOMP,N)
 9993     FORMAT(I6,2I7,I9)
 9992   CONTINUE
      ENDIF

C Loop through all lighting zones
      DO 9 N=1,NLITZ(ICOMP)

C Exterior sensor, don't bother to calculate daylight factors, set to 1.0.
      IF(IDFST(ICOMP,N).EQ.3)THEN
        nt=1
        DF(nt)=1.0
        DFDAT(ICOMP,N,nt,1)=1.0
        SUMDF(ICOMP,N,1)=1.0
        ISS=INT(CGX(ICOMP,N,1))
        IF(ISS.EQ.0)ISS=1
        call eclose(CGY(ICOMP,N,1),0.00,0.5,close)
        IF(close)THEN
          IF(ITFLG.EQ.1)WRITE(ITU,896)ICOMP,N,ISS
  896     FORMAT(' Zone ',I2,' Light zone',I1,/,
     &  ' Exterior horiz sensor @ surface ',I2)
        ELSE
          IF(ITFLG.EQ.1)WRITE(ITU,897)ICOMP,N,ISS
  897     FORMAT(' Zone ',I2,' Light zone',I1,/,
     &  ' Exterior vert sensor @ surface ',I2)
        ENDIF

C User specified DF, no preprocessing required.
      ELSEIF(IDFST(ICOMP,N).EQ.2)THEN
        IF(ITFLG.EQ.1)THEN
            WRITE(ITU,881)ICOMP,NLITZ(ICOMP),NUDWIN(ICOMP)
  881       FORMAT(/,'Zone ',I2,' User defined daylighting for ',I1,
     &       ' lighting zones and ',/,I2,' user defined glazing',
     &       ' data sets [df and window no.]')
          ENDIF
      ELSE
C Trace heading.
        IF(ITFLG.EQ.1)WRITE(ITU,899)ICOMP,N
  899   FORMAT('Zone ',I2,' light zone',I1,/,
     &   '   Sensor          |  Window  |SkyAdj|IntRfl|Daylt |Azim. |',
     &   'Azim. |Elev  |Elev ',/,
     &   'No|@X   |@Y   |@Z  |Descr @Sur|Compon|Compon|Fact% |Left  |',
     &   'Right |Lower |Upper')

        DO 10 I=1,NDF(ICOMP,N)
          SUMDF(ICOMP,N,I)=0.
          XP=CGX(ICOMP,N,I)
          YP=CGY(ICOMP,N,I)
          ZP=CGH(ICOMP,N,I)

          DO 20 J=1,NSUR
             DF(J)=0.
C Continue only if this surface is vertical, is TMC, has 4 vertexes
C  and is connectedto either the exterior or another real zone.  Keep track
C of total windows in zone with nt. Exclude skylights.
            call eclose(PELV(ICOMP,J),0.00,0.5,close)
            IF(close.AND.(ITMCFL(ICOMP,J).GT.0))THEN
              IF (NVER(J).NE.4)THEN
              WRITE(ITU,811)ICOMP,J
  811         format('Zone',I2,' TMC',I2,' not 4 vertexes!')
              ENDIF

C Determine the 'other' zone and connection information.
            CALL SURADJ(ICOMP,J,IES,T,IZC,ISC,ICN,CSTR)
            NA=NVER(J)
            DO 201 K=1,NA
              XPL(K)=X(JVN(J,K))
              YPL(K)=Y(JVN(J,K))
              ZPL(K)=Z(JVN(J,K))
  201       CONTINUE

C Test if the TMC is rectangular and calculate window width
C and high WIDTH, HIGH:
              call eclose(XPL(1),XPL(2),0.01,closea)
              call eclose(XPL(1),XPL(3),0.01,closeb)
              call eclose(XPL(1),XPL(4),0.01,closec)
              call eclose(YPL(1),YPL(2),0.01,closed)
              call eclose(YPL(1),YPL(3),0.01,closee)
              call eclose(YPL(1),YPL(4),0.01,closef)
              call eclose(ZPL(1),ZPL(2),0.01,closeg)
              call eclose(ZPL(1),ZPL(3),0.01,closeh)
              call eclose(ZPL(1),ZPL(4),0.01,closei)
              IF(closea.OR.closeb.OR.closec)THEN
                IF(closed.OR.closee.OR.closef)THEN
                  IF(closeg.OR.closeh.OR.closei)THEN
                    CONTINUE
                  ELSE
                    WRITE(ITU,812)ICOMP,J
                  ENDIF                    
                ELSE
                  WRITE(ITU,812)ICOMP,J
                ENDIF
              ELSE
                WRITE(ITU,812)ICOMP,J
  812           format('Zone ',I2,'TMC',I2,' may not be rectangular!')
              ENDIF 

              XX1=XPL(1)
              DO 555 M=1,4
                XPLTEM=XPL(M)
                call eclose(XPL(1),XPLTEM,0.01,close)
                IF(.NOT.close)XX2=XPL(M)
  555         CONTINUE
              YY1=YPL(1)
              DO 556 M=1,4
                YPLTEM=YPL(M)
                call eclose(YPL(1),YPLTEM,0.01,close)
                IF(.NOT.close)YY2=YPL(M)
  556         CONTINUE
              ZZ1=ZPL(1)              
              DO 557 M=1,4
                ZPLTEM=ZPL(M)
                call eclose(ZPL(1),ZPLTEM,0.01,close)
                IF(.NOT.close)ZZ2=ZPL(M)
  557         CONTINUE

              WIDTH=SQRT((XX1-XX2)**2+(YY1-YY2)**2)
              HIGH=ABS(ZZ1-ZZ2)
              ZZMIN=ZZ1
              IF(ZZ2.LT.ZZ1)ZZMIN=ZZ2
 
              IF(IES.EQ.0)THEN
                WRITE(WINFO,'(a,I2)')' Ext Def',J
              ELSEIF(IES.EQ.3)THEN
                WRITE(WINFO,'(a,I2)')' Int Def',J
              ENDIF
              IF(IES.EQ.0.OR.IES.EQ.3)THEN
                CALL ANGS(NA,XPL,YPL,ZPL,WIDTH,HIGH,XP,YP,ZP)

C If interior the sky component represents the view into the adjacent zone.
                CALL SCOMP(S)
                CALL INTCMP(ICOMP,J,WIDTH,HIGH,ZZMIN,XIC)

C External portion.
                XC=S

                IF(IES.EQ.0)THEN
                  DF(J)=XC+XIC
                ELSEIF(IES.EQ.3)THEN
                  DF(J)=XC+XIC
                ENDIF
                DFDAT(ICOMP,N,J,I)=DF(J)
                SUMDF(ICOMP,N,I)=SUMDF(ICOMP,N,I)+DF(J)

C P1 & P2 as returned from ANGS are the radians off of the normal to
C the surface with the window (from the inside). Convert azimuth to
C degrees (clockwise from north) equivalent to SAZI by subtracting
C from the azimuth angle of the surface in question and keeping in the
C range of 0 to 360 degrees.
                  PP1=PAZI(ICOMP,J)-(P1*TODEG)
                  IF(PP1.LT.0.)PP1=360.+PP1
                  IF(PP1.GE.360.)PP1=PP1-360.
                  PP2=PAZI(ICOMP,J)-(P2*TODEG)
                  IF(PP2.LT.0.)PP2=360.+PP2
                  IF(PP2.GE.360.)PP2=PP2-360.
                  TT1=T1*TODEG
                  TT2=T2*TODEG

C Print out components and daylight factor in %
                  IF(ITFLG.EQ.1)WRITE(ITU,898)I,CGX(ICOMP,N,I),
     &              CGY(ICOMP,N,I),CGH(ICOMP,N,I),WINFO,S*100,XIC*100,
     &              DF(J)*100,PP1,PP2,TT1,TT2
  898             FORMAT(I2,3F6.2,A10,3F7.2,4F7.1)
              ENDIF
            ENDIF
   20     CONTINUE
   10   CONTINUE
      ENDIF
    9   CONTINUE
C Trace.
        IF(J.EQ.0.AND.ITFLG.EQ.1)WRITE(ITU,895)ICOMP
  895   FORMAT(' No external TMC windows in zone',I3)
      RETURN
      END

c *************** ANGS ***************

C Determine window edge angles.

C NA:      number of vertices comprising surface.
C X,Y&Z    the vertices of the surface, anticlockwise from outside.
C WDX&WDZ  window width and height.
C XP,YP,ZP reference point coordinates.
C P1&P2    azimuth angles of window `verticals' (left to right from inside).
C T1&T2    altitude angles of window `horizontals' (bottom to top).

      SUBROUTINE  ANGS(NA,X,Y,Z,WDX,WDZ,XP,YP,ZP)
      COMMON/D7/T1,T2,P1,P2

      DIMENSION  X(*), Y(*), Z(*)
      DIMENSION  TMAT(4,4),RMAT(4,4),CG(3),EP(3),PEQN(4)

C Find transformation matrices that normalise face.
      call PLEQN(X,Y,Z,NA,CG,PEQN,IERR)

C (EP is origin, CG centre of gravity of plane).
      EP(1) = CG(1) + PEQN(1)
      EP(2) = CG(2) + PEQN(2)
      EP(3) = CG(3) + PEQN(3)

      CALL  EYEMAT(EP,CG,1.0, TMAT, RMAT)

C Transform sensor point.
      CALL  ORTTRN(XP,YP,ZP, TMAT, X1,Y1,Z1, IERR)

      DZ = ABS(1. - Z1)
      P1 = ATAN((WDX-X1)/DZ)
      P2 = ATAN(-X1/DZ)
      T1 = ATAN(-Y1/DZ)
      T2 = ATAN((WDZ-Y1)/DZ)
      IF(T1.LT.0.)T1=0.
      IF(T2.LT.0.)T2=0.
      RETURN
      END


c *************** SCOMP ***************
C Calculate the sky complnent using formula 5.59 on page 211 of Energy
C Siumlation in Building Design.  Where T1=window sill, T2=window head,
C P1=angle(radians) left, P2 angle right.  This method is applicable
C only to vertical rectangular glazing systems.

      SUBROUTINE SCOMP(SC)

      COMMON/D7/T1,T2,P1,P2

      DIMENSION SCMP(2)

      PI = 4.0 * ATAN(1.0)
      DO 10 I=1,2
        PHI1=0.
        IF(I.EQ.2)THEN
          PHI2=P2
        ELSE
          PHI2=P1
        ENDIF

C Swap so that Phi2>Phi1.
        IF(PHI1.GT.PHI2)THEN
          X=PHI1
          PHI1=PHI2
          PHI2=X
        ENDIF

        X1=(1./COS(T1))**2.
        X2=(1./COS(T2))**2.
        Y1=(SIN(PHI1)/COS(PHI1))**2.
        Y2=(SIN(PHI2)/COS(PHI2))**2.
        C1=ATAN(((Y2/X1)**.5))-ATAN(((Y1/X1)**.5))
        C2=ATAN(((Y1/X2)**.5))-ATAN(((Y2/X2)**.5))
        A1=(Y2/(X1+Y2))**.5
        A2=(Y1/(X1+Y1))**.5
        A3=(Y1/(X2+Y1))**.5
        A4=(Y2/(X2+Y2))**.5
        C3=((X1-1.)**.5)*(A1-A2)
        C4=((X2-1.)**.5)*(A3-A4)
        A1=((X2-1.)*Y2/(X2+Y2))**.5
        A2=((X2-1.)*Y1/(X2+Y1))**.5
        A3=((X1-1.)*Y1/(X1+Y1))**.5
        A4=((X1-1.)*Y2/(X1+Y2))**.5
        C5=ATAN(A1)
        C6=ATAN(A2)
        C7=ATAN(A3)
        C8=ATAN(A4)
        A1=6./(28.*PI)
        A2=1./(X1**.5)
        A3=1./(X2**.5)
        A6=4./3.
        A4=A6/X1
        A5=A6/X2
        SCMP(I)=A1*(A2*C1+A3*C2+A4*C3+A5*C4+A6*(C5-C6+C7-C8))
   10 CONTINUE
      SC=ABS(SCMP(1)-SCMP(2))
      RETURN
      END

C *************** INTCMP ***************
C Calculation of internal reflected component based on eqn 5.60 as
C discussed in section 5.71 of Clarke's book.  Works best for REC type
C of geometry where windows are mapped onto an enclosing cube in the
C proper sequence. Other geometries are mapped to a bounding cube and
C the first four surfaces are taken to be typical of walls.  In some cases
C windows may not be associated properly. It is possible that a
C method using view factors will yield a more generalised solution.
C J is the surface in question and K is the default galzing.
C 1.0-AI() is assumed to represent the visible reflectance of surfaces.

      SUBROUTINE INTCMP(ICOMP,J,WIDTH,HIGH,ZWMIN,XIC)
#include "building.h"

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV  
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
C TMC data
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)

      DIMENSION SNA(MS),PAZI(MS),PELV(MS)
      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15
      logical close

C Loop through zone verticies and find out ZMIN and ZMAX.

      ZMAX=-1.E-10
      ZMIN=1.E10
      DO 99 I=1,NTV
        ZMAX=AMAX1(ZMAX,Z(I))
        ZMIN=AMIN1(ZMIN,Z(I))
   99 CONTINUE
      ZZMIN=ZWMIN-ZMIN
      RDZ=ZMAX-ZMIN

C Generate the dimensions and areas of the upper and lower half of the
C zone in relation to the midpoint of the window.  Note the use of the
C COG of a polygon would be more general, areas of opaqe surfaces are
C available and method should check for transp MLC.
      ZAREA=0.
      AVREF=0.
      AU=0.
      AL=0.
      HL=ZZMIN+.5*HIGH
      HU=RDZ-HL
      W=WIDTH*HIGH

C Get information about areas of all surfaces opaque and TMC:
      call ZINFO(IOUT,SNA,ZOA,PAZI,PELV,VOL)

      DO 10 I=1,NSUR
      call eclose(PAZI(I),PAZI(J),0.5,close)
      IF(.NOT.close)THEN
        ZAREA=ZAREA+SNA(I)
        ITVTR=ITMCFL(ICOMP,I)
        IF(SOTF(I).EQ.'OPAQ')THEN
          AVREF=AVREF+SNA(I)*(1.-AI(ICOMP,I))
        ELSE
          AVREF=AVREF+SNA(I)*(1.-(TVTR(ICOMP,ITVTR)+AI(ICOMP,I)))
        ENDIF

        IF(SVFC(I).EQ.'CEIL')THEN
          IF(SOTF(I).EQ.'OPAQ')THEN
            RCW=RCW+(1.-AI(ICOMP,I))*SNA(I)
            AU=AU+SNA(I)
          ELSE
            RCW=RCW+(1.-(TVTR(ICOMP,ITVTR)+AI(ICOMP,I)))*SNA(I)                 
            AU=AU+SNA(I)
          ENDIF

        ELSEIF(SVFC(I).EQ.'FLOR')THEN
          IF(SOTF(I).EQ.'OPAQ')THEN
            RFW=RFW+(1.-AI(ICOMP,I))*SNA(I)
            AL=AL+SNA(I)
          ELSE
            RFW=RFW+(1.-(TVTR(ICOMP,ITVTR)+AI(ICOMP,I)))*SNA(I)                 
            AL=AL+SNA(I)
          ENDIF

        ELSE
          IF(SOTF(I).EQ.'OPAQ')THEN
            RCW=RCW+(1.-AI(ICOMP,I))*SNA(I)*HU/RDZ
            RFW=RFW+(1.-AI(ICOMP,I))*SNA(I)*HL/RDZ
            AU=AU+SNA(I)*HU/RDZ
            AL=AL+SNA(I)*HL/RDZ
          ELSE
            RCW=RCW+(1.-(TVTR(ICOMP,ITVTR)+AI(ICOMP,I)))*SNA(I)*HU/RDZ
            RFW=RFW+(1.-(TVTR(ICOMP,ITVTR)+AI(ICOMP,I)))*SNA(I)*HL/RDZ
            AU=AU+SNA(I)*HU/RDZ
            AL=AL+SNA(I)*HL/RDZ
          ENDIF
        ENDIF
      ENDIF      
   10 CONTINUE

      AVREF=AVREF/ZAREA
      RFW=RFW/AL
      RCW=RCW/AU

C Note 40. is a correction factor from Table 5.11 of book assuming
C that there are no obstructions.
      XIC=0.01*((W*(40.*RFW+5.*RCW))/(ZAREA*(1.-AVREF)))
      RETURN
      END
