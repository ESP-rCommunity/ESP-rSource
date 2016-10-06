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


C **************** MFUTIL 
C This module contains the following utility functions or subroutines
C     MFLOAD
C     DENSIT
C     DYVISC
C     MFCRIM
C     MFDAYS
C     MFSKIP
C     MFTSPH
C     MFSNFL
C     MFSTFL

C ************* MFLOAD
C This subroutine uses MFREAD of MFPROB of ESRUmfs to read data from
C a mass flow network description file (IAIRN=1) or via NETREAD if
C a graphic definition; all data transfer unformatted
C Performs some validity checks similar to what is done by the
C subroutines MFNDES, MFCDES and MFLDES of MFPROB of ESRUmfs

C     ICONS = connectivity matrix; ICONS(I,J) holds number of
C             connections between nodes I and J

      SUBROUTINE MFLOAD(IFPRB,IER)

#include "building.h"
#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas

      DIMENSION    ICONS(MCNN,MCNN)
      character outs*124
      CHARACTER NDNAM*12
      CHARACTER LAPROB*72

C Read the fluid flow description file depending on the value
C of iairn.
C      write(6,*) iairn,laprob
      IV=1
      IER=0
      REWIND(IFPRB,ERR=998)
      if(iairn.eq.1)then
        CALL EMFREAD(IFPRB,IER)
      elseif(iairn.eq.2)then

C Scan the graphics file and then convert to flow common blocks.
        CALL NETREAD(IFPRB,'S',IER)
        CALL NETTOFLW(ier)
      endif
      IF(IER.NE.0)then
        call usrmsg(' Problem finding flow network description',
     &    'or there were problems found while scanning it.','W')
        return
      endif

C Reset MFLOAD's error indicator and start checking
      IER=0

C Zeroise ICONS
      DO 5 I=1,MCNN
      DO 5 J=1,MCNN
      ICONS(I,J)=0
    5 CONTINUE

C Perform additional checks on node data.
      IV=3
      INOD=0
   10 INOD=INOD+1

C Check boundary node supplementary data items.
      if(NDTYP(INOD).eq.3)then
        IF(SUPNOD(INOD,1).LT.1..OR.SUPNOD(INOD,1).GT.NPRE)then
          write(outs,'(3a,i2,a)') 'For node ',NDNAM(INOD),
     &      ' the referenced pressure coef ',SUPNOD(INOD,1),
     &      ' was not found!'
          call edisp(iuout,outs)
          GOTO 999
        endif
        IF(SUPNOD(INOD,2).LT.0..OR.SUPNOD(INOD,2).GT.360.)then
          write(outs,'(3a,i2,a)') 'For node ',NDNAM(INOD),
     &      ' the orientation ',SUPNOD(INOD,2),
     &      ' is not in the range of 0 to 360!'
          call edisp(iuout,outs)
          GOTO 999
        endif
      endif
      IF(INOD.LT.NNOD) GOTO 10

C Check connections data:
C Perform additional checks not made in the original file reading.

C Check that there are only single connections between the referenced
C nodes in case of the component types:
C - junctions 220, 230, 240 or 250
C For this we set up the connectivity matrix ICONS, whose element (I,J)
C holds the number of connections between nodes I and J
      IV=6
      DO 60 ICNN=1,NCNN
      I=NODPS(ICNN)
      J=NODNE(ICNN)
      ICONS(I,J)=ICONS(I,J)+1
   60 CONTINUE

C Look for a component type 220, 230, 240, 250
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
      END IF

C Right, this connection is one of those components
   63 I=NODPS(ICNN)
      J=NODNE(ICNN)
      K=NDSCNN(ICNN,1)
      L=NDSCNN(ICNN,2)

C Now check the number of connections
      IF((ICMPTP.GE.220.AND.ICMPTP.LE.250.AND.ICONS(I,J).NE.1).OR.
     &   ((ICMPTP.EQ.220.OR.ICMPTP.EQ.240).AND.ICONS(K,J).NE.1).OR.
     &   ((ICMPTP.EQ.230.OR.ICMPTP.EQ.250).AND.ICONS(I,K).NE.1).OR.
     &   (ICMPTP.EQ.240.AND.ICONS(L,J).NE.1).OR.
     &   (ICMPTP.EQ.250.AND.ICONS(I,L).NE.1))then
         call edisp(iuout,' Something rotten with connections!')
         GOTO 999
      endif
   64 IF (ICNN.LT.NCNN) GOTO 62

  100 RETURN

C Error trap on read error or silly data
C Reset number of nodes, number of flow components and/or number
C of connections and return to calling module
  999 IER=1
      GOTO (9991,9991,9991,9992,9993,9993) IV
 9991 NNOD=0
 9992 NCMP=0
 9993 NCNN=0

      call edisp(iuout,' ')
      call edisp(iuout,' MFLOAD: fluid flow file is in error!')
      WRITE(outs,9995) INOD,ICMP,ICNN
 9995 FORMAT(' data OK up to node:',I3,' component :',I3,
     &       ' connection:',I3)
      call edisp(iuout,outs)
      GOTO 100

  998 write(iuout,*)' problem rewinding the network file'
      goto 999

      END

C ****************** DENSIT 
C DENSIT returns density (kg/m^3) as function of
C     IFLD   - fluid type (1=air, 2=water)
C     TEMP   - fluid temperature (C)

      FUNCTION DENSIT(IFLD,TEMP)
      COMMON/OUTIN/IUOUT,IUIN

      character outs*124

C Max and minimum temperatures for suppression of redundent errors
      real fMaxWaterTemp
      real fMinWaterTemp
      data fMaxWaterTemp,fMinWaterTemp /100.0,0.0/

C Flag for inappropriate temperatures       
      logical bWarn

      VTEMP=TEMP

      IF(IFLD.EQ.1) THEN

         IF(TEMP.LT.-273.14) THEN
           VTEMP=-273.0
           WRITE(outs,*)' DENSIT: temperature [',TEMP,'] outside valid',
     &       ' range: T > -273.15'
           call edisp(iuout,outs)
           WRITE(outs,*) ' DENSIT: air density evaluated for T = ',
     &       VTEMP
           call edisp(iuout,outs)
         ENDIF

         DENSIT=1.1881*293.15/(273.15+VTEMP)

      ELSE IF(IFLD.EQ.2) THEN

C Following expression is only valid for 0 < TEMP < 100
C Based on: VDI Warmeatlas, 1984, pp. Db1
         IF(TEMP.LT.0..OR.TEMP.GT.100.) THEN
            IF(TEMP.LT.0.0) THEN
               VTEMP=0.0
            ELSE
               VTEMP=100.
            ENDIF


C Warn user, but suppress redundant messages
            bWarn = .false.
            
            if ( TEMP .LT. fMinWaterTemp - 0.01 ) then
              bWarn = .true.
              fMinWaterTemp = TEMP
            elseif ( TEMP .GT. fMaxWaterTemp + 0.01 ) then
              bWarn = .true.
              fMaxWaterTemp = TEMP
            endif

            if ( bWarn ) then
            
              WRITE(outs,*) ' DENSIT: temperature [',TEMP,'] outside',
     &                   ' valid range: 0 < T < 100'
              call edisp(iuout,outs)
              WRITE(outs,*) ' DENSIT: water density evaluated for T =',
     &                   VTEMP
              call edisp(iuout,outs)
            endif            
         ENDIF

         DENSIT=1001.1-0.082*VTEMP-0.0036*VTEMP*VTEMP

      ELSE
         STOP ' DENSIT: unsupported fluid type. Unresolvable error !'
      END IF

      RETURN
      END

C ****************** DYVISC 
C DYVISC returns dynamic viscosity (kg/ms) as function of
C     IFLD   - fluid type (1=air, 2=water)
C     TEMP   - fluid temperature (C)

      FUNCTION DYVISC(IFLD,TEMP)

      VTEMP=TEMP

      IF(IFLD.EQ.1) THEN
         DYVISC=1.71432E-5 +4.828E-8*VTEMP
      ELSE IF(IFLD.EQ.2) THEN

C Following expression is only valid for 0 < TEMP < 100 and
C is not very accurate, but probably sufficient for testing purposes
C Based on: VDI Warmeatlas, 1984, pp. Db1
         IF(TEMP.LT.0..OR.TEMP.GT.100.) THEN

            IF(TEMP.LT.0.0) THEN
               VTEMP=0.0
            ELSE
               VTEMP=100.
            ENDIF

            WRITE(*,*) ' DYVISC: temperature [',TEMP,'] outside valid',
     &                 ' range: 0 < T < 100'
            WRITE(*,*) ' DYVISC: water dynamic viscosity',
     &                 ' evaluated for T = ',VTEMP

         ENDIF

         DYVISC=0.17444E-02 -.15885E-03*VTEMP**0.50

      ELSE
         STOP ' DYVISC: unsupported fluid type. Unresolvable error !'
      END IF

      RETURN
      END

C ************************* MFCRIM 
C Results to input matcher; ie. check if results file corresponds to
C current model description
C     IER    - 0 they do match; 1 they don't match

      SUBROUTINE MFCRIM(IFRES,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)

      CHARACTER*20 WORD
      CHARACTER*12  CHRIN(3)
      character OUTSTR*124
      CHARACTER LTPCMP*60,CMNAM*12,NDNAM*12

      IER=0

C Read node info from results file
C First get to the start and skip header line
      REWIND(IFRES,ERR=999)
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'date header',IER)

C Start reading/checking node: name, fluid type, node type, and height
C first skip header
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'header',IER)

C then read results node by node into a string
      DO 10 INOD=1,NNOD
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'data line',IER)
      K=0
      IERL=0

C split this string and recapture separate data items
      CALL EGETW(OUTSTR,K,WORD,'W','node name',IER)
      CHRIN(1)=WORD(1:12)
      CALL EGETWI(OUTSTR,K,IFLDT,0,0,'-','fluid type',IER)
      CALL EGETWI(OUTSTR,K,INODT,0,0,'-','node type',IER)

C compare these data items with current model description
      IF(IERL.NE.0.OR.
     &   NDNAM(INOD)(1:12).NE.CHRIN(1)(1:12).OR.NDFLD(INOD).NE.IFLDT.OR.
     &   NDTYP(INOD).NE.INODT)then
         call edisp(iuout,' mismatched network nodes & results in...')
         call edisp(iuout,OUTSTR)
         GOTO 999
      endif
   10 CONTINUE

C Start reading/checking connection: nodes, component name, type and fluid
C first skip header
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'connection header',IER)

C then read results connection by connection into a string
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

C compare these data items with current model description
      IF(IERL.NE.0.OR.
     &   NDNAM(NODPS(ICNN))(1:12).NE.CHRIN(1)(1:12).OR.
     &   NDNAM(NODNE(ICNN))(1:12).NE.CHRIN(2)(1:12).OR.
     &   CMNAM(ITPCON(ICNN))(1:12).NE.CHRIN(3)(1:12).OR.
     &   ITPCMP(ITPCON(ICNN)).NE.ICMPT)then
         call edisp(iuout,' mismatched network conns & results in...')
         call edisp(iuout,OUTSTR)
        GOTO 999
      endif
   20 CONTINUE

C If so far no errors, it *must* be oke I hope :-)

  100 RETURN

C Error trap on read error or silly data
  999 IER=1
      call edisp(iuout,' In comparing the data in the results file and')
      call edisp(iuout,' the flow network description there were ')
      call edisp(iuout,' differences encountered which mean that it is')
      call edisp(iuout,' from a different simulation.')
      GOTO 100

      END

C ******************* MFDAYS 
C Find day numbers of first and last simulation day in results file
C This enables output of results from previous runs

      SUBROUTINE MFDAYS(IFRES,NNOD,NCNN,IFYD,ILYD,IER)

      COMMON/OUTIN/IUOUT,IUIN

      CHARACTER*22 FORM

      IER=0

C Calculate number of output lines per simulation day (LINES)
C For this we need to know the number of time steps per hour (NTSPH)
      CALL MFTSPH(IFRES,NNOD,NCNN,NTSPH,IE)
      IF(IE.NE.0) GOTO 999
      LINES=NTSPH*24*(3+NNOD+NCNN)
      FORM='(7X,I2,1X,I2,     (/))'
      WRITE(FORM(14:18),1000) LINES-1
 1000 FORMAT(I5)

C Read first month and day from record 1
      REWIND(IFRES,ERR=999)
      READ(IFRES,FORM,END=999,ERR=999) IDAY,IMON

C If reasonable values calculate day number for first day
      CALL EDAYCH(IDAY,IMON,IE)
      IF(IE.NE.0) GOTO 999
      CALL EDAY(IDAY,IMON,IFYD)

C Keep on reading hour 1 of next day until read error occurs
   10 READ(IFRES,FORM,END=12,ERR=999) IDUM1,IDUM2
      IDAY=IDUM1
      IMON=IDUM2
      GOTO 10
C If reasonable values calculate day number for last day
   12 CALL EDAYCH(IDAY,IMON,IE)
      IF(IE.NE.0) GOTO 999
      CALL EDAY(IDAY,IMON,ILYD)

  100 RETURN

C Error trap on read error or silly data
  999 IER=1
      call edisp(iuout,' MFDAYS: error reading fluid flow results!')
      call edisp(iuout,' Results file is either empty or does not')
      call edisp(iuout,' correspond to current model descripton !')
      GOTO 100

      END

C ********************* MFSKIP 
C Fast skip to first record of simulation day IDAYF in file IFRES

      SUBROUTINE MFSKIP(IFRES,NNOD,NCNN,IFYD,IDAYF,IER)

      COMMON/OUTIN/IUOUT,IUIN

      CHARACTER*10 FORM

      IER=0

C Calculate number of output lines per simulation day (LINES)
C For this we need to know the number of time steps per hour (NTSPH)
      CALL MFTSPH(IFRES,NNOD,NCNN,NTSPH,IE)
      IF(IE.NE.0) GOTO 999
      LINES=NTSPH*24*(3+NNOD+NCNN)
      FORM='(     (/))'
      WRITE(FORM(2:6),1000) LINES-1
 1000 FORMAT(I5)

C Calculate number of days to skip (ISKIP)
      ISKIP=IDAYF-IFYD

C Position READ pointer at 1th hour of next day by reading ISKIP days
      REWIND(IFRES,ERR=999)
      IF(ISKIP.EQ.0) GOTO 100
      IDAY=0
   10 IDAY=IDAY+1
      READ(IFRES,FORM,END=999,ERR=999)
      IF(IDAY.LT.ISKIP) GOTO 10

  100 RETURN

C Error trap on read error or silly data
  999 IER=1
      call edisp(iuout,'MFSKIP: error reading fluid flow results file!')
      GOTO 100

      END

C *********************** MFTSPH 
C Try to deduct from results file, number of time steps per hour NTSPH
C which was used during simulation (could have been by ESRUbps)
C     STIME        - time for first simulation result set
C     TIME         - time of last read simulation result set

      SUBROUTINE MFTSPH(IFRES,NNOD,NCNN,NTSPH,IER)

      COMMON/OUTIN/IUOUT,IUIN

      CHARACTER*19 FORM

      IER=0

C Calculate number of output lines per simulation time step (LINES)
      LINES=(3+NNOD+NCNN)
      FORM='(18X,F6.3,     (/))'
      WRITE(FORM(11:15),1000) LINES-1
 1000 FORMAT(I5)

C Position READ pointer at 1th time step
C Note that TIME is read in Fn.3 format
      REWIND(IFRES,ERR=999)
      NTSPH=-1
   10 NTSPH=NTSPH+1
      READ(IFRES,FORM,END=999,ERR=999) TIME
      IF(NTSPH.EQ.0) STIME=TIME
      IF((TIME-STIME).GE.0.9999) GOTO 100
      GOTO 10

  100 RETURN

C Error trap on read error or silly data
  999 IER=1
      call edisp(iuout,'MFTSPH: error reading fluid flow results file!')
      GOTO 100

      END

C ******************* MFSNFL 
C Sum fluid flows entering (FLIN) and leaving (FLUT) node NODE from
C coupled node NODC or from all coupled nodes in case NODC=0. Entering
C flow is +ve, leaving flow is -ve. In case of IDIM=1 the results will
C be in 10^-3.kg/s and when IDIM=2 or 3 the results will be in m^3/hr

C     DIMFAC - factor for calculating the appropriate flow dimension

      SUBROUTINE MFSNFL(IDIM,NODE,NODC,FLIN,FLUT)

#include "net_flow.h"

      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION FLIN,FLUT

      FLIN=0.
      FLUT=0.
      IF(IDIM.EQ.1) DIMFAC=1000.0

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

C ************************ MFSTFL 
C Sums flows through each connection coupled nodes to get each
C node's total fluid flow entering (FLWIN; +ve) or leaving
C (FLWUT; -ve), respectively. In case of IDIM=1 the results will
C be in kg/s and when IDIM=2 or 3 the results will be in m^3/hr

C     DIMFAC - factor for calculating the appropriate flow dimension

      SUBROUTINE MFSTFL(IDIM,FLWIN,FLWUT)

#include "net_flow.h"

      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION FLWIN(MNOD),FLWUT(MNOD)

C Zeroise FLWIN and FLWUT first
      DO 10 INOD=1,NNOD
      FLWIN(INOD)=0
      FLWUT(INOD)=0
   10 CONTINUE

C Assign the fluid mass flows through each connection to its
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
