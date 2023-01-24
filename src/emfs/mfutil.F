C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file contains the following subroutines.
C MFLOAD - read a fluid flow network file.
C DENSIT - calculate fluid density.
C DYVISC - calculate fluid dynamic viscosity.
C MFCRIM - checks if flow results file corresponds to current model.
C MFDAYS - determine day numbers of first and last day in results file.
C MFSKIP - skip to first record of simulation day IDAYF in results file.
C MFTSPH - determine the simulation time step from results file.
C MFSNFL - sum fluid flows entering and leaving a node (NODE) from
C          one or all coupled nodes. 
C MFSTFL - sums flows through each connection to coupled nodes to get each
C          node's total fluid flow entering leaving.
C Psat01m - calculates saturation vapour pressure ss a function of
C           temperature 

C ******************** MFLOAD ********************
C Uses MFREAD (in MFPROB) to read a fluid flow network file (IAIRN=1),
C NETREAD if a graphic definition or MF3DREAD if a 3D network file.
C Performs some validity checks.
C ICONS(I,J) - number of connections between nodes I and J.

      SUBROUTINE MFLOAD(IFPRB,IER)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

      DIMENSION ICONS(MCNN,MCNN)
      character outs*124
      CHARACTER LAPROB*72

      IV=1
      IER=0
      REWIND(IFPRB,ERR=998)

C Flow network file is text-based.
      if(iairn.eq.1)then
        CALL EMFREAD(IFPRB,IER)

C Flow network file is graphical.
      elseif(iairn.eq.2)then
        CALL NETREAD(IFPRB,'S',IER)
        CALL NETTOFLW(ier)

C Flow network file is 3D.
      elseif(iairn.eq.3)then
        CALL MFCDAT
        CALL EMF3DREAD(IFPRB,'R',IER)
        CALL ERPFREE(IFPRB,ISTAT)

      endif
      IF(IER.NE.0)then
        call usrmsg(' ','Problem with flow network file!','W')
        return
      endif

      IER=0
      DO 5 I=1,MCNN
      DO 5 J=1,MCNN
        ICONS(I,J)=0
    5 CONTINUE

C Check node data.
      IV=3
      INOD=0
   10 INOD=INOD+1

C Check boundary node supplementary data.
      if(NDTYP(INOD).eq.3)then
        IF(SUPNOD(INOD,1).LT.1..OR.SUPNOD(INOD,1).GT.NPRE)then
          write(outs,'(3a,i2,a)') 'Pressure coefficient ',
     &       SUPNOD(INOD,1),' not found for node ',NDNAM(INOD),'!'
          call edisp(iuout,outs)
          GOTO 999
        endif
        IF(SUPNOD(INOD,2).LT.0..OR.SUPNOD(INOD,2).GT.360.)then
          write(outs,'(3a,i2,a)') 'Orientation ',SUPNOD(INOD,2),
     &      ' out of range for node ',NDNAM(INOD),'!'
          call edisp(iuout,outs)
          GOTO 999
        endif
      endif
      IF(INOD.LT.NNOD) GOTO 10

C Check connections data ensuring that thre are only single connections
C between nodes in case of junctions (component types 220, 230, 240 or 250).
C Set up the connectivity matrix ICONS, whose element (I,J) are the
C number of connections between nodes I and J.
      IV=6
      DO 60 ICNN=1,NCNN
      I=NODPS(ICNN)
      J=NODNE(ICNN)
      ICONS(I,J)=ICONS(I,J)+1
   60 CONTINUE

C Look for component types 220, 230, 240 or 250.
      ICNN=0
   62 ICNN=ICNN+1
      ICMP=ITPCON(ICNN)
      ICMPTP=ITPCMP(ICMP)

C Variables not (yet) used.
C      IS12=INT(SUPCMP(ICMP,12))
C      IS11=INT(SUPCMP(ICMP,11))
C      IS6 =INT(SUPCMP(ICMP,6))
      IF(ICMPTP.EQ.220.OR.ICMPTP.EQ.230.OR.
     &   ICMPTP.EQ.240.OR.ICMPTP.EQ.250) THEN
         GOTO 63
      ELSE
         GOTO 64
      ENDIF

C Junction component.
   63 I=NODPS(ICNN)
      J=NODNE(ICNN)
      K=NDSCNN(ICNN,1)
      L=NDSCNN(ICNN,2)

C Check the number of connections
      IF((ICMPTP.GE.220.AND.ICMPTP.LE.250.AND.ICONS(I,J).NE.1).OR.
     &   ((ICMPTP.EQ.220.OR.ICMPTP.EQ.240).AND.ICONS(K,J).NE.1).OR.
     &   ((ICMPTP.EQ.230.OR.ICMPTP.EQ.250).AND.ICONS(I,K).NE.1).OR.
     &   (ICMPTP.EQ.240.AND.ICONS(L,J).NE.1).OR.
     &   (ICMPTP.EQ.250.AND.ICONS(I,L).NE.1))then
         call edisp(iuout,'MFLOAD: problem with connections!')
         GOTO 999
      endif
   64 IF (ICNN.LT.NCNN) GOTO 62

  100 RETURN

C Read error or corrupt data.
  999 IER=1
      GOTO (9991,9991,9991,9992,9993,9993) IV
 9991 NNOD=0
 9992 NCMP=0
 9993 NCNN=0

      call edisp(iuout,' ')
      call edisp(iuout,'MFLOAD: flow network file error!')
      WRITE(outs,9995) INOD,ICMP,ICNN
 9995 FORMAT('Data OK up to node',I3,' component',I3,
     &       ' connection:',I3)
      call edisp(iuout,outs)
      GOTO 100

  998 write(iuout,*)'MFLOAD: problem rewinding the flow network file!'
      goto 999

      END

C ******************** DENSIT ********************
C Returns density (kg/m^3) as function of
C IFLD - fluid type (1=air, 2=water);
C TEMP - fluid temperature (C).

      FUNCTION DENSIT(IFLD,TEMP)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      character outs*124

C Temperatures limits.
      real fMaxWaterTemp
      real fMinWaterTemp
      data fMaxWaterTemp,fMinWaterTemp /100.0,0.0/

C Inappropriate temperature flag.      
      logical bWarn

      VTEMP=TEMP

C Air.
      IF(IFLD.EQ.1) THEN
        IF(TEMP.LT.-273.14) THEN
          VTEMP=-273.0
          WRITE(outs,*)'DENSIT: temperature [',TEMP,'] outside valid',
     &       ' range (T > -273.15)!'
          call edisp(iuout,outs)
          WRITE(outs,*)'DENSIT: air density evaluated for ',VTEMP,'C.'
          call edisp(iuout,outs)
        ENDIF
        DENSIT=1.1881*293.15/(273.15+VTEMP)

C Water.
      ELSEIF(IFLD.EQ.2) THEN

C Following expression valid for 0 < TEMP < 100 (from
C VDI Warmeatlas, 1984, pp. Db1).
        IF(TEMP.LT.0..OR.TEMP.GT.100.) THEN
          IF(TEMP.LT.0.0) THEN
             VTEMP=0.0
          ELSE
             VTEMP=100.
          ENDIF

C Warn user, but suppress redundant messages
          bWarn = .false.

          if (TEMP.LT.fMinWaterTemp - 0.01) then
            bWarn = .true.
            fMinWaterTemp = TEMP
          elseif (TEMP.GT.fMaxWaterTemp + 0.01) then
            bWarn = .true.
            fMaxWaterTemp = TEMP
          endif

          if (bWarn) then
            WRITE(outs,*) 'DENSIT: temperature [',TEMP,'] outside',
     &                    ' valid range (0 < T < 100)!'
            call edisp(iuout,outs)
            WRITE(outs,*) 'DENSIT: water density evaluated for ',
     &                    VTEMP,'C.'
            call edisp(iuout,outs)
          endif            
        ENDIF
        DENSIT=1001.1-0.082*VTEMP-0.0036*VTEMP*VTEMP

      ELSE
         STOP 'DENSIT: fluid type is not 1 or 2!'
      ENDIF

      RETURN
      END

C ******************** DYVISC ********************
C Returns dynamic viscosity (kg/ms) as function of
C IFLD - fluid type (1=air, 2=water);
C TEMP - fluid temperature (C).

      FUNCTION DYVISC(IFLD,TEMP)

      character outs*124

      VTEMP=TEMP

C Air.
      IF(IFLD.EQ.1) THEN
        DYVISC=1.71432E-5 +4.828E-8*VTEMP

C Water.
      ELSEIF(IFLD.EQ.2) THEN
        IF(TEMP.LT.0..OR.TEMP.GT.100.) THEN
          IF(TEMP.LT.0.0) THEN
            VTEMP=0.0
          ELSE
            VTEMP=100.
          ENDIF
          WRITE(outs,*) 'DYVISC: temperature [',TEMP,'] outside valid',
     &                 ' range: 0 < T < 100!'
          call edisp(iuout,outs)
          WRITE(outs,*) 'DYVISC: water dynamic viscosity',
     &                 ' evaluated for ',VTEMP,'C.'
          call edisp(iuout,outs)
        ENDIF

C Following expression is approximate for 0 < TEMP < 100 (from
C VDI Warmeatlas, 1984, pp. Db1).
        DYVISC=0.17444E-02 -.15885E-03*VTEMP**0.50

      ELSE
         STOP 'DYVISC: unsupported fluid type!'
      ENDIF

      RETURN
      END

C ******************** MFCRIM ********************
C Checks if network flow results file corresponds to current model
C description.
C IER - 0 match; 1 no match.

      SUBROUTINE MFCRIM(IFRES,IER)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER*20 WORD
      CHARACTER*12  CHRIN(3)
      character OUTSTR*124

      IER=0

C Check nodes.
      REWIND(IFRES,ERR=999)
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'date header',IER)

C Skip header.
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'header',IER)

C Read results node-by-node into a character string.
      DO 10 INOD=1,NNOD
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'data line',IER)
      K=0
      IERL=0

C Split string and recapture separate data items.
      CALL EGETW(OUTSTR,K,WORD,'W','node name',IER)
      CHRIN(1)=WORD(1:12)
      CALL EGETWI(OUTSTR,K,IFLDT,0,0,'-','fluid type',IER)
      CALL EGETWI(OUTSTR,K,INODT,0,0,'-','node type',IER)

C Compare these data with current model description.
      IF(IERL.NE.0.OR.
     &  NDNAM(INOD)(1:12).NE.CHRIN(1)(1:12).OR.NDFLD(INOD).NE.IFLDT.OR.
     &  NDTYP(INOD).NE.INODT)then
        call edisp(iuout,'MFCRIM: mismatched files!')
        call edisp(iuout,OUTSTR)
        GOTO 999
      endif
   10 CONTINUE

C Check connections.
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'connection header',IER)

C Read results connection-by-connection into a character string.
      DO 20 ICNN=1,NCNN
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'connection data',IER)
      K=0
      IERL=0

C Split this connections string and recapture separate data items.
      CALL EGETW(OUTSTR,K,WORD,'W','conn start',IER)
      CHRIN(1)=WORD(1:12)
      CALL EGETW(OUTSTR,K,WORD,'W','conn end',IER)
      CHRIN(2)=WORD(1:12)
      CALL EGETW(OUTSTR,K,WORD,'W','conn component',IER)
      CHRIN(3)=WORD(1:12)
      CALL EGETWI(OUTSTR,K,ICMPT,0,0,'-','conn type',IER)

C Compare these data items with current model description.
      IF(IERL.NE.0.OR.
     &   NDNAM(NODPS(ICNN))(1:12).NE.CHRIN(1)(1:12).OR.
     &   NDNAM(NODNE(ICNN))(1:12).NE.CHRIN(2)(1:12).OR.
     &   CMNAM(ITPCON(ICNN))(1:12).NE.CHRIN(3)(1:12).OR.
     &   ITPCMP(ITPCON(ICNN)).NE.ICMPT)then
         call edisp(iuout,'MFCRI: mismatched connections!.')
         call edisp(iuout,OUTSTR)
        GOTO 999
      endif
   20 CONTINUE

  100 RETURN

  999 IER=1
      call edisp(iuout,'MFCRI: differences detected between the')
      call edisp(iuout,'flow network description and results files!')
      GOTO 100

      END

C ******************** MFDAYS ********************
C Determine day numbers of first and last simulation day in results file.

      SUBROUTINE MFDAYS(IFRES,NNOD,NCNN,IFYD,ILYD,IER)

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER*22 FORM

      IER=0

C Calculate number of output lines per simulation day (LINES)
C given the number of time steps per hour (NTSPH).
      CALL MFTSPH(IFRES,NNOD,NCNN,NTSPH,IE)
      IF(IE.NE.0) GOTO 999
      LINES=NTSPH*24*(3+NNOD+NCNN)
      FORM='(7X,I2,1X,I2,     (/))'
      WRITE(FORM(14:18),1000) LINES-1
 1000 FORMAT(I5)

C Read first month and day.
      REWIND(IFRES,ERR=999)
      READ(IFRES,FORM,END=999,ERR=999) IDAY,IMON

C Calculate day number for first day.
      CALL EDAYCH(IDAY,IMON,IE)
      IF(IE.NE.0) GOTO 999
      CALL EDAY(IDAY,IMON,IFYD)

C Continue reading hour 1 of each day until a read error occurs.
   10 READ(IFRES,FORM,END=12,ERR=999) IDUM1,IDUM2
      IDAY=IDUM1
      IMON=IDUM2
      GOTO 10

C Calculate day number for last day.
   12 CALL EDAYCH(IDAY,IMON,IE)
      IF(IE.NE.0) GOTO 999
      CALL EDAY(IDAY,IMON,ILYD)

  100 RETURN

  999 IER=1
      call edisp(iuout,
     &          'MFDAYS: error reading fluid flow results file!')
      GOTO 100

      END

C ******************** MFSKIP ********************
C Skip to first record of simulation day IDAYF in file IFRES.

      SUBROUTINE MFSKIP(IFRES,NNOD,NCNN,IFYD,IDAYF,IER)

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER*10 FORM

      IER=0

C Calculate number of output lines per simulation day (LINES)
C given the number of time steps per hour (NTSPH),
      CALL MFTSPH(IFRES,NNOD,NCNN,NTSPH,IE)
      IF(IE.NE.0) GOTO 999
      LINES=NTSPH*24*(3+NNOD+NCNN)
      FORM='(     (/))'
      WRITE(FORM(2:6),1000) LINES-1
 1000 FORMAT(I5)

C Calculate number of days to skip (ISKIP).
      ISKIP=IDAYF-IFYD

C Position READ pointer at 1st hour of next day by reading ISKIP days.
      REWIND(IFRES,ERR=999)
      IF(ISKIP.EQ.0) GOTO 100
      IDAY=0
   10 IDAY=IDAY+1
      READ(IFRES,FORM,END=999,ERR=999)
      IF(IDAY.LT.ISKIP) GOTO 10

  100 RETURN

  999 IER=1
      call edisp(iuout,'MFSKIP: error reading fluid flow results file!')
      GOTO 100

      END

C *********************** MFTSPH ***********************
C Deducet from results file the number of time steps per hour NTSPH
C as used during simulation.
C STIME - time for first simulation result set.
C TIME  - time of last read simulation result set.

      SUBROUTINE MFTSPH(IFRES,NNOD,NCNN,NTSPH,IER)

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER*19 FORM

      IER=0

C Calculate number of output lines per simulation time step (LINES).
      LINES=(3+NNOD+NCNN)
      FORM='(18X,F6.3,     (/))'
      WRITE(FORM(11:15),1000) LINES-1
 1000 FORMAT(I5)

C Position READ pointer at 1st time step.
C Note that TIME is read in Fn.3 format.
      REWIND(IFRES,ERR=999)
      NTSPH=-1
   10 NTSPH=NTSPH+1
      READ(IFRES,FORM,END=999,ERR=999) TIME
      IF(NTSPH.EQ.0) STIME=TIME
      IF((TIME-STIME).GE.0.9999) GOTO 100
      GOTO 10

  100 RETURN

  999 IER=1
      call edisp(iuout,'MFTSPH: error reading fluid flow results file!')
      GOTO 100

      END

C ******************** MFSNFL ********************
C Sum fluid flows entering (FLIN) and leaving (FLUT) node (NODE) from
C coupled node (NODC) or from all coupled nodes in case NODC=0. Entering
C flow is +ve, leaving flow is -ve. For IDIM=1 the results are in kg/s
C and when IDIM=2 or 3 the results are in m^3/hr.

      SUBROUTINE MFSNFL(IDIM,NODE,NODC,FLIN,FLUT)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"

      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION FLIN,FLUT

      FLIN=0.
      FLUT=0.
      IF(IDIM.EQ.1) DIMFAC=1.0

      DO 10 ICNN=1,NCNN
      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      IF(IDPS.NE.NODE.AND.IDNE.NE.NODE) GOTO 10
      IF(IDPS.EQ.NODE.AND.(IDNE.EQ.NODC.OR.NODC.EQ.0)) THEN
        IF(FLW1(ICNN).GE.0) THEN
          IF(IDIM.EQ.2.OR.IDIM.EQ.3)
     &       DIMFAC=3600.0/DENSIT(NDFLD(IDPS),TNOD(IDPS))
          FLUT=FLUT-FLW1(ICNN)*DIMFAC
        ELSE
          IF(IDIM.EQ.2.OR.IDIM.EQ.3)
     &       DIMFAC=3600.0/DENSIT(NDFLD(IDNE),TNOD(IDNE))
          FLIN=FLIN-FLW1(ICNN)*DIMFAC
        END IF
        IF(FLW2(ICNN).GE.0) THEN
          IF(IDIM.EQ.2.OR.IDIM.EQ.3)
     &       DIMFAC=3600.0/DENSIT(NDFLD(IDPS),TNOD(IDPS))
          FLUT=FLUT-FLW2(ICNN)*DIMFAC
        ELSE
          IF(IDIM.EQ.2.OR.IDIM.EQ.3)
     &       DIMFAC=3600.0/DENSIT(NDFLD(IDNE),TNOD(IDNE))
          FLIN=FLIN-FLW2(ICNN)*DIMFAC
        END IF
      END IF
      IF(IDNE.EQ.NODE.AND.(IDPS.EQ.NODC.OR.NODC.EQ.0)) THEN
        IF(FLW1(ICNN).GE.0) THEN
          IF(IDIM.EQ.2.OR.IDIM.EQ.3)
     &       DIMFAC=3600.0/DENSIT(NDFLD(IDPS),TNOD(IDPS))
          FLIN=FLIN+FLW1(ICNN)*DIMFAC
        ELSE
          IF(IDIM.EQ.2.OR.IDIM.EQ.3)
     &       DIMFAC=3600.0/DENSIT(NDFLD(IDNE),TNOD(IDNE))
          FLUT=FLUT+FLW1(ICNN)*DIMFAC
        END IF
        IF(FLW2(ICNN).GE.0) THEN
          IF(IDIM.EQ.2.OR.IDIM.EQ.3)
     &       DIMFAC=3600.0/DENSIT(NDFLD(IDPS),TNOD(IDPS))
          FLIN=FLIN+FLW2(ICNN)*DIMFAC
        ELSE
          IF(IDIM.EQ.2.OR.IDIM.EQ.3)
     &       DIMFAC=3600.0/DENSIT(NDFLD(IDNE),TNOD(IDNE))
          FLUT=FLUT+FLW2(ICNN)*DIMFAC
        END IF
      END IF
   10 CONTINUE

      RETURN
      END

C ************************ MFSTFL ************************
C Sums flows through each connection to coupled nodes to get each
C node's total fluid flow entering (FLWIN; +ve) or leaving
C (FLWUT; -ve) respectively. In case of IDIM=1 the results will
C be in kg/s and when IDIM=2 or 3 the results will be in m^3/hr.

      SUBROUTINE MFSTFL(IDIM,FLWIN,FLWUT)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"

      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION FLWIN(MNOD),FLWUT(MNOD)

      DO 10 INOD=1,NNOD
      FLWIN(INOD)=0.0
      FLWUT(INOD)=0.0
   10 CONTINUE

C Assign the fluid flows through each connection to its
C coupled nodes.
      IF(IDIM.EQ.1) DIMFAC=1.0
      DO 20 ICNN=1,NCNN
      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      IF(FLW1(ICNN).GE.0) THEN
        IF(IDIM.EQ.2.OR.IDIM.EQ.3)
     &     DIMFAC=3600.0/DENSIT(NDFLD(IDPS),TNOD(IDPS))
        FLWUT(IDPS)=FLWUT(IDPS)-FLW1(ICNN)*DIMFAC
        FLWIN(IDNE)=FLWIN(IDNE)+FLW1(ICNN)*DIMFAC
      ELSE
        IF(IDIM.EQ.2.OR.IDIM.EQ.3)
     &     DIMFAC=3600.0/DENSIT(NDFLD(IDNE),TNOD(IDNE))
        FLWIN(IDPS)=FLWIN(IDPS)-FLW1(ICNN)*DIMFAC
        FLWUT(IDNE)=FLWUT(IDNE)+FLW1(ICNN)*DIMFAC
      END IF
      IF(FLW2(ICNN).GE.0) THEN
        IF(IDIM.EQ.2.OR.IDIM.EQ.3)
     &     DIMFAC=3600.0/DENSIT(NDFLD(IDPS),TNOD(IDPS))
        FLWUT(IDPS)=FLWUT(IDPS)-FLW2(ICNN)*DIMFAC
        FLWIN(IDNE)=FLWIN(IDNE)+FLW2(ICNN)*DIMFAC
      ELSE
        IF(IDIM.EQ.2.OR.IDIM.EQ.3)
     &     DIMFAC=3600.0/DENSIT(NDFLD(IDNE),TNOD(IDNE))
        FLWIN(IDPS)=FLWIN(IDPS)-FLW2(ICNN)*DIMFAC
        FLWUT(IDNE)=FLWUT(IDNE)+FLW2(ICNN)*DIMFAC
      END IF
   20 CONTINUE

      RETURN
      END

C ******************** Psat01m ********************
C Calculates the saturation vapour pressure (Pa) as a function of
C temperature (C) (from Marieke 1994, IEA 1991, V1, page 4.8). Copied
C from ebld/mstmsc.F, lines 541 ff.

      FUNCTION Psat01m(TC)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      character outs*124
      SMALL=1.0E-15

      IF(TC.GT.-30.0.AND.TC.LT.0.0)THEN
        A = 82.9E-3
        B = 288.1E-6
        C = 4.403E-6
        Psat01m=611.0*EXP(A*TC-B*TC*TC+C*TC**3)
      ELSEIF(TC.GT.-0.1.AND.TC.LT.40.0)THEN
        A = 72.5E-3
        B = 288.1E-6
        C = 0.79E-6
        Psat01m=611.0*EXP(A*TC-B*TC*TC+C*TC**3)
      ELSE
        Psat01m=100.0*SATVP(TC)
      ENDIF
      IF(Psat01m.LT.SMALL)then
        write(outs,'(a,F12.6,a)')
     &   'Psat01m: small saturation vapour pressure at ',TC,' C!'
        call edisp(iuout,outs)
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      endif
      RETURN
      END
